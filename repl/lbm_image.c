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

// Assumptions of about the image memory:
// * It is part of the address space.
// * It is a write-once memory.
// * Can be cleared in its entirety.

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

// Offline image tools
// - Image compaction: remove overwrite fields and compact the image.
// - Change of base address: relabel all memory accesses.
// - ...



// constant heap should be 4byte aligned so that the are 2 unused low end bits
// in all cell-pointers into constant heap. Constant heap should be the first thing
// to occur in all images to easily ensure this.
//                             BYTE
#define CONSTANT_HEAP 0x01 // [0x01 | size bytes | pad | data]
#define SYMBOL_TABLE  0x02 // [0x02 | size bytes | data]
#define BINDING_CONST 0x03 // TBD
#define BINDING_FLAT  0x04 // TBD
#define STARTUP_ENTRY 0x05 // [0x04 | size bytes | flatval])

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

bool write_lbm_value(lbm_value v, uint32_t *i) {

#ifdef LBM64
  return false;
#else
  if (*i + 4 < image_size) {
    uint8_t *bytes = (uint8_t*)&v;
    bool b = true;
    b =      image_write(*i, bytes[0]); (*i) ++;
    b = b && image_write(*i, bytes[1]); (*i) ++;
    b = b && image_write(*i, bytes[2]); (*i) ++;
    b = b && image_write(*i, bytes[3]); (*i) ++;
    return b;
  }
  return false;
#endif

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
          image_write(write_index, BINDING_CONST); write_index++;
          size_t n = strlen(name) + 1; // write the 0
          for (size_t str_i = 0; str_i < n; str_i ++) {
            image_write(write_index, name[str_i]); write_index++;
          }
          write_lbm_value(val_field, &write_index);
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

  bool r = image_write(write_index++, STARTUP_ENTRY);
  r = r && image_write(write_index++, b[0]);
  r = r && image_write(write_index++, b[1]);
  r = r && image_write(write_index++, b[2]);
  r = r && image_write(write_index++, b[3]);
  for (int i = 0; i < size; i ++) {
    r = r && image_write(write_index++, data[i]);
  }
  return r;
}

uint8_t read_u8(uint32_t index) {
  return *(image_address+index);
}

uint16_t read_u16(uint32_t index) {
  uint16_t b0 = (uint16_t)read_u8(index);
  uint16_t b1 = (uint16_t)read_u8(index+1);
  return b0 << 8 | b1;
}

uint32_t read_u32(uint32_t index) {
  return *((uint32_t*)(image_address + index));
}

uint64_t read_u64(uint32_t index) {
  return *((uint64_t*)(image_address + index));
}



// Constant heaps as part of an image.

lbm_const_heap_t image_const_heap;
lbm_uint image_const_heap_start_ix = 0;

bool image_const_heap_write(lbm_uint ix, lbm_uint w) { // ix is in lbm_uint sized steps
  uint8_t * bytes = (uint8_t*)&w;
  lbm_uint i = image_const_heap_start_ix + (ix * 4);
  bool b = true;
  b =      image_write(i,   bytes[0]);
  b = b && image_write(i+1, bytes[1]);
  b = b && image_write(i+2, bytes[2]);
  b = b && image_write(i+3, bytes[3]);
  return b;
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
void lbm_image_create_const_heap(uint32_t size_words) {

  uint32_t size_bytes = size_words * sizeof(lbm_uint);

  uint8_t *b = (uint8_t*)&size_bytes;

  image_write(write_index, CONSTANT_HEAP);
  image_write(write_index+1, b[0]); // what byte order does this end up being?
  image_write(write_index+2, b[1]);
  image_write(write_index+3, b[2]);
  image_write(write_index+4, b[3]);
  write_index += 5;
  write_index += CONSTANT_HEAP_ALIGN_PAD;
  write_index += size_bytes;
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
    default:
      goto done_loading_image;
      break;
    }
  }
 done_loading_image:
  return;

}
