/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#include <extensions.h>
#include "lbm_image.h"
#include <heap.h>
#include <env.h>

// Assumptions about the image memory:
// * It is part of the address space.
// * Image is always available at the same address (across reboots)
// * It is a write-once memory.
// * Can be cleared in its entirety.
// * Can check on a byte-level is "is-writable" (has a known initial state when cleared)

// Details
// * @const_start @const_end is tricky.
// * Constant heap is needed because of small amount of RAM.
// * Arbitrary pointers will be tricky.

// Want to be able to build an image incrementally.
// Can @const_start @const_end be used in conjuction with a
// more exlicit image manipulation subsystem.

//TBD:
// * does an image contain running threads ? (I would prefer if no)
//   instead there is a startup-entry that represents some code that will
//   be executed after setting up an image.
//   This startup-entry can spawn threads and initialize resources.
// * Will we have a heap-image or will all bindings move into the const heap.

// FEB 26:
// -- There will be no "heap-image" or "memory-image"
//    Just flattened and stored bindings from the environment (which is as good but likely smaller).
// -- There will be an image of the const-heap. in fact the const heap lives in the image always.
//    A bit problematic with building on image incrementally as it is in flash and the contents cannot be changed.
//    Contents can be added though!  (keep track of const-heap write-ptr is an issue)
// -- Maybe we should implement image for a read-write memory and then make the flash situation a special case?
// -- Maybe size fields should always be in bytes.
// -- TODO: a flatten function that flattens a value directly to flash and also does not flatten things that are
//          already in flash, but rather then just refers to them.

// FEB 27:
// -- Symbol numbering will be an issue.
//    * Store the symboltable in the image and restore it on boot.
//    * Names already in flash can be refered to.
//    * Names in ram can be copied.
//    * Entire subtable may already be in flash - leave in place and refer to it.
// -- loading an image and then adding to it can be tricky to make possible.
//    * const-heap write pointer needs to be stored. (but if it is stored then it cannot be changed)
//      Could allow multiple "const-heap-write-pointer" fields in the image and use the one that appears last...
//    * Symboltable could be created incrementally in a similar way. Append later symbol_table data fields
//      to the previously loaded.

// FEB 28:
// -- Symbol numbering problem. The structure of the symboltable may
//    need to change. It is currently impossible to append a symbol list stored
//    in flash to the global symbol table. A richer structure is needed.
// -- The symbols in the SYMTAB can all be in flash (all in the image) and
//    all name strings can be written to flash as well.
//    * May be easiest if names go to the flash heap (as it is now)
//      and table entries are a tagged field in the image (1 extra byte per symbol...)
//    * Means the image must be initialized (to a degree) before symbols are created.

// MARCH 1:
// -- Symbols are added to and restored from the image.
// -- lbm_add_symbol_const, still creates a lbm_memory list structure.
//    Const symbols should also be stored into the image and add_symbol_const
//    should check and reuse stored symbol id.
//    Check order of initialization to see how easy this is to fix.

// Offline image tools
// - Image compaction: remove overwrite fields and compact the image.
// - Change of base address: relabel all memory accesses.
// - ...


// Endianess woes...
// - little endian     least-significant byte at least address
// - big endian        most-significant byte at least address
// - all platforms we target currently are little-endian
//
//  0x11223344
//     | | | '--- [44] addr
//     | | '----- [33] addr + 1
//     | '------- [22] addr + 2
//     '--------- [11] addr + 3
//
// Images are going to be mainly little endian.  (what endianess does flatvalues use? I think BE)

// constant heap should be 4byte aligned so that the are 2 unused low end bits
// in all cell-pointers into constant heap. Constant heap should be the first thing
// to occur in all images to easily ensure this.
//                             uint8|   uint32   |  ..... 
#define CONSTANT_HEAP 0x01 // [0x01 | size bytes | pad | data]
#define BINDING_CONST 0x02 // TBD
#define BINDING_FLAT  0x03 // TBD
#define STARTUP_ENTRY 0x04 // [0x04 | size bytes | flatval])
#define SYMBOL_ENTRY  0x05 // [0x5 | ID | NAME PTR | NEXT_PTR] // symbol_entry with highest address is root.
// tagged data  that can vary in size has a size bytes field.
// Fixed size data does not.

#ifdef LBM64
#define CONSTANT_HEAP_ALIGN_PAD 3
#else
#define CONSTANT_HEAP_ALIGN_PAD 3
#endif

static lbm_image_write_fun image_write = NULL;
static lbm_image_clear_fun image_clear = NULL;

static bool image_is_empty = true;
static uint8_t *image_address = NULL;
static uint32_t write_index = 0;
static uint32_t image_size = 0;
static bool image_startup = false;
static uint32_t image_startup_position;
static uint32_t image_startup_size;

uint8_t *lbm_image_get_image(void) {
  return image_address;
}

uint32_t lbm_image_get_size(void) {
  return image_size;
}

bool lbm_image_has_startup(void) {
  return image_startup;
}

uint8_t *lbm_image_startup_address(void) {
  return image_address + image_startup_position;
}

uint32_t lbm_image_startup_size(void) {
  return image_startup_size;
}

// TODO: Add bounds checks here and there and everywhere.
// TODO: Needs a new flatten that does not flatten constant values.
// constant values are already in the flash heap and part of the image.
// TODO: 32bit / 64bit will be highly incompatible.

uint8_t read_u8(uint32_t index) {
  return *(image_address+index);
}

// Endianess sensitive read functions.
uint16_t read_u16(uint32_t index) {
  return *((uint16_t*)(image_address+index));
}

uint32_t read_u32(uint32_t index) {
  return *((uint32_t*)(image_address + index));
}

uint64_t read_u64(uint32_t index) {
  return *((uint64_t*)(image_address + index));
}

// Byte order dependent write functions.
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
bool write_u16(uint16_t w, uint32_t i) {
  uint8_t * bytes = (uint8_t*)&w;
  bool b = image_write(bytes[0], i);
  b = b && image_write(bytes[1], i+1);
  return b;
}

bool write_u32(uint32_t w, uint32_t i) {
  uint8_t * bytes = (uint8_t*)&w;
  bool b = image_write(bytes[0], i);
  b = b && image_write(bytes[1], i+1);
  b = b && image_write(bytes[2], i+2);
  b = b && image_write(bytes[3], i+3);
  return b;
}

bool write_u64(uint64_t dw, uint32_t i) {
  uint8_t * bytes = (uint8_t*)&dw;
  bool b = image_write(bytes[0], i);
  b = b && image_write(bytes[1], i+1);
  b = b && image_write(bytes[2], i+2);
  b = b && image_write(bytes[3], i+3);
  b = b && image_write(bytes[4], i+4);
  b = b && image_write(bytes[5], i+5);
  b = b && image_write(bytes[6], i+6);
  b = b && image_write(bytes[7], i+7);
  return b;
}


#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
bool write_u16(uint16_t w, uint32_t i) {
  uint8_t * bytes = (uint8_t*)&w;
  bool b = image_write(bytes[1], i);
  b = b && image_write(bytes[0], i+1);
  return b;
}

bool write_u32(uint32_t w, uint32_t i) {
  uint8_t * bytes = (uint8_t*)&w;
  bool b = image_write(bytes[3], i);
  b = b && image_write(bytes[2], i+1);
  b = b && image_write(bytes[1], i+2);
  b = b && image_write(bytes[0], i+3);
  return b;
}

bool write_u64(uint64_t dw, uint32_t i) {
  uint8_t * bytes = (uint8_t*)&dw;
  bool b = image_write(bytes[7], i);
  b = b && image_write(bytes[6], i+1);
  b = b && image_write(bytes[5], i+2);
  b = b && image_write(bytes[4], i+3);
  b = b && image_write(bytes[3], i+4);
  b = b && image_write(bytes[2], i+5);
  b = b && image_write(bytes[1], i+6);
  b = b && image_write(bytes[0], i+7);
  return b;
}
#endif

bool write_lbm_uint(lbm_uint ptr_val, uint32_t i) {
#ifdef LBM64
  return write_u64(ptr_val, i);
#else
  return write_u32(ptr_val, i);
#endif
}

bool write_lbm_value(lbm_value v, uint32_t i) {
#ifdef LBM64
  return write_u64(v, i);
#else
  return write_u32(v, i);
#endif
}

lbm_uint *lbm_image_add_symbol(char *name, lbm_uint id, lbm_uint symlist) {
  printf("adding symbol %s at address: %x\n", name, (lbm_uint)name);
  bool r = image_write(SYMBOL_ENTRY, write_index++);
  lbm_uint entry_ptr = (lbm_uint)image_address + write_index;
  r = r && write_lbm_uint((lbm_uint)name, write_index); write_index += sizeof(lbm_uint);
  r = r && write_lbm_uint(id, write_index); write_index += sizeof(lbm_uint);
  r = r && write_lbm_uint(symlist, write_index); write_index += sizeof(lbm_uint);
  if (r)
    return entry_ptr;
  return NULL;
}

bool lbm_image_save_global_env(void) {
  lbm_value *env = lbm_get_global_env();

  if (env) {
    for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
      lbm_value curr = env[i];
      while(lbm_is_cons(curr)) {
        lbm_value name_field = lbm_caar(curr);
        lbm_value val_field  = lbm_cdr(lbm_car(curr));
        char *name = (char*)lbm_get_name_by_symbol(lbm_dec_sym(name_field));
        if (!name) return false;
        if (lbm_is_constant(val_field)) {
          image_write(BINDING_CONST, write_index); write_index++;
          size_t n = strlen(name) + 1; // write the 0
          for (size_t str_i = 0; str_i < n; str_i ++) {
            image_write(name[str_i], write_index); write_index++;
          }
          write_lbm_value(val_field, write_index); write_index+=sizeof(lbm_uint);
        } else {
          printf("%s is not constant, not storing env binding\n", name);
        }
        curr = lbm_cdr(curr);
      }
    }
    return true;
  }
  return false;
}

bool lbm_image_save_startup_fv(uint8_t *data, uint32_t size) {
  uint8_t *b = (uint8_t*)&size;

  bool r = image_write(STARTUP_ENTRY, write_index++);
  r = r && image_write(b[0], write_index++);
  r = r && image_write(b[1], write_index++);
  r = r && image_write(b[2], write_index++);
  r = r && image_write(b[3], write_index++);
  for (int i = 0; i < size; i ++) {
    r = r && image_write(data[i], write_index++);
  }
  return r;
}

// Constant heaps as part of an image.

lbm_const_heap_t image_const_heap;
lbm_uint image_const_heap_start_ix = 0;

bool image_const_heap_write(lbm_uint ix, lbm_uint w) { // ix is in lbm_uint sized steps
  lbm_uint i = image_const_heap_start_ix + (ix * 4);
  return write_u32(w, i);
}

bool lbm_image_is_empty(void) {
  return image_is_empty;
}

void lbm_image_clear(void) {
  image_clear();
  image_is_empty = true;
  write_index = 0;
}

// you probably want to specify const heap size in number of words ?
bool lbm_image_create_const_heap(uint32_t size_words) {

  uint32_t size_bytes = size_words * sizeof(lbm_uint);

  if (size_bytes < image_size) {
    uint8_t *b = (uint8_t*)&size_bytes;

    image_write(CONSTANT_HEAP, write_index);
    image_write(b[0], write_index+1); // what byte order does this end up being?
    image_write(b[1], write_index+2);
    image_write(b[2], write_index+3);
    image_write(b[3], write_index+4);
    write_index += 5;
    write_index += CONSTANT_HEAP_ALIGN_PAD;
    write_index += size_bytes;
    return true;
  }
  return false;
}

void lbm_image_set_callbacks(lbm_image_clear_fun   image_clear_fun,
                             lbm_image_write_fun   image_write_fun) {
  image_clear = image_clear_fun;
  image_write = image_write_fun;
}


void lbm_image_init(uint8_t* image_mem_address,
                    uint32_t image_size_bytes) {

  image_address = image_mem_address;
  image_size = image_size_bytes;
}

void lbm_image_boot(void) {
  //process image
  uint32_t pos = 0;

  while (pos < image_size) {

    // TODO: loop until done reading image
    switch(read_u8(pos)) {
    case CONSTANT_HEAP: {
      pos ++; // Skip over tag
      image_is_empty = false;
      uint32_t size = read_u32(pos); // BYTES!
      pos += 4; //sizeof
      pos += CONSTANT_HEAP_ALIGN_PAD; // skip over pad
      // TODO: const_heap_init argument order is not ideal.
      image_const_heap_start_ix = pos;
      lbm_const_heap_init(image_const_heap_write,
                          &image_const_heap,
                          (lbm_uint*)(image_address + pos),
                          (lbm_uint)(size / (sizeof(lbm_uint)))); // size in words
      pos += size; // PAD already skipped
    } break;
    case BINDING_CONST: {
      lbm_uint sym_id;
      pos ++; //Jump past the BINDING_CONST Tag
      lbm_uint name_size = strlen((char*)(image_address + pos)) + 1;
      if (!lbm_add_symbol((char *)(image_address + pos), &sym_id)) {
        printf("error reading image\n");
        goto done_loading_image;
      }
#ifdef LBM64
      lbm_uint val = read_u64(pos+name_size);
#else
      lbm_uint val = read_u32(pos+name_size);
#endif
      lbm_uint ix_key  = sym_id & GLOBAL_ENV_MASK;
      lbm_value *global_env = lbm_get_global_env();
      lbm_uint orig_env = global_env[ix_key];
      lbm_value new_env = lbm_env_set(orig_env,lbm_enc_sym(sym_id),val);

      if (lbm_is_symbol(new_env)) {
        printf("error restoring binding from image\n");
      }
      global_env[ix_key] = new_env;
      pos += sizeof(lbm_uint) + name_size;
    } break;
    case STARTUP_ENTRY: {
      pos ++; // skip tag
      printf("startup entry found\n");
      image_startup = true;
      uint32_t size = read_u32(pos); // in bytes
      pos += 4; // jump over size
      image_startup_size = size;
      image_startup_position = pos;
      pos += size; // jump past entire block
    } break;
    case SYMBOL_ENTRY: {
      pos ++;
      printf("symbol entry found\n");
      lbm_symrepr_set_symlist((lbm_uint*)(image_address + pos));
      pos += 3 * sizeof(lbm_uint);
    } break;
    default:
      goto done_loading_image;
      break;
    }
  }
 done_loading_image:
  return;

}
