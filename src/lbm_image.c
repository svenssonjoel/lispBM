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
#include <lbm_image.h>
#include <heap.h>
#include <env.h>
#include <lbm_flat_value.h>
#include <eval_cps.h>

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
//                                      uint8|   uint32   |  .....
#define CONSTANT_HEAP    0x01       // [0x01 | size bytes | pad | data]
#define CONSTANT_HEAP_IX 0x02       // [0x02 | uint32]
#define BINDING_CONST    0x03       // [0x03 | key | lbm_uint ]
#define BINDING_FLAT     0x04       // [0x04 | size bytes | key | flatval ]
#define STARTUP_ENTRY    0x05       // [0x05 | size bytes | flatval])
#define SYMBOL_ENTRY     0x06       // [0x06 | ID | NAME PTR | NEXT_PTR] // symbol_entry with highest address is root.
// tagged data  that can vary in size has a size bytes field.
// Fixed size data does not.

// To be able to work on an image incrementally (even though it is not recommended)
// many fields are allowed to be duplicated and the later ones have priority
// over earlier ones.

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


bool write_u8(uint8_t b, uint32_t *i) {
  bool r = false;
  if (*i <= image_size) {
    r = image_write(b, *i); (*i)++;
  }
  return r;
}

bool write_u16_be(uint16_t w, uint32_t *i) {
  uint8_t * bytes = (uint8_t*)&w;
  return
    write_u8(bytes[1], i) &&
    write_u8(bytes[0], i);
}

bool write_u32_be(uint32_t w, uint32_t *i) {
  uint8_t * bytes = (uint8_t*)&w;
  return
    write_u8(bytes[3], i) &&
    write_u8(bytes[2], i) &&
    write_u8(bytes[1], i) &&
    write_u8(bytes[0], i);
}

bool write_u64_be(uint64_t dw, uint32_t *i) {
  uint8_t * bytes = (uint8_t*)&dw;
   return
     write_u8(bytes[7], i) &&
     write_u8(bytes[6], i) &&
     write_u8(bytes[5], i) &&
     write_u8(bytes[4], i) &&
     write_u8(bytes[3], i) &&
     write_u8(bytes[2], i) &&
     write_u8(bytes[1], i) &&
     write_u8(bytes[0], i);
}

// Byte order dependent write functions.
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
bool write_u16(uint16_t w, uint32_t *i) {
  uint8_t * bytes = (uint8_t*)&w;
  return
    write_u8(bytes[0], i) &&
    write_u8(bytes[1], i);
}

bool write_u32(uint32_t w, uint32_t *i) {
  uint8_t * bytes = (uint8_t*)&w;
  return
    write_u8(bytes[0], i) &&
    write_u8(bytes[1], i) &&
    write_u8(bytes[2], i) &&
    write_u8(bytes[3], i);
}

bool write_u64(uint64_t dw, uint32_t *i) {
  uint8_t * bytes = (uint8_t*)&dw;
  return
    write_u8(bytes[0], i) &&
    write_u8(bytes[1], i) &&
    write_u8(bytes[2], i) &&
    write_u8(bytes[3], i) &&
    write_u8(bytes[4], i) &&
    write_u8(bytes[5], i) &&
    write_u8(bytes[6], i) &&
    write_u8(bytes[7], i);
}


#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
bool write_u16(uint16_t w, uint32_t *i) {
  return write_u16_be(w,i);
}

bool write_u32(uint32_t w, uint32_t *i) {
  write_u32_be(w,i);
}

bool write_u64(uint64_t dw, uint32_t *i) {
  write_u64_be(dw, i);
}
#endif

bool write_lbm_uint(lbm_uint ptr_val, uint32_t *i) {
#ifdef LBM64
  return write_u64(ptr_val, i);
#else
  return write_u32(ptr_val, i);
#endif
}

bool write_lbm_value(lbm_value v, uint32_t *i) {
#ifdef LBM64
  return write_u64(v, i);
#else
  return write_u32(v, i);
#endif
}

// ////////////////////////////////////////////////////////////
// Flatten a value into image

// TODO: Consants things that are stored in the image
//       does not need to be flattened. Could refer to these by
//       reference. Some new kinds of flat values needs to be added
//       for this referencing to work.

// TODO: Symbols in a flat_value in an image can be stored as
//       its numerical representation rather than its string rep.

static bool i_f_cons(void ) {
  return write_u8(S_CONS, &write_index);
}

static bool i_f_lisp_array(uint32_t size) {
  // arrays are smaller than 2^32 elements long
  bool r = write_u8(S_LBM_LISP_ARRAY, &write_index);
  r = r && write_u32_be(size, &write_index);
  return r;
}

static bool i_f_sym(lbm_value sym) {
  lbm_uint sym_id = lbm_dec_sym(sym);
  bool r = write_u8(S_SYM_VALUE, &write_index);
  #ifndef LBM64
  r = r && write_u32_be(sym_id, &write_index);
  #else
  r = r && write_u64_be(sym_id, &write_index);
  #endif
  return r;
}

static bool i_f_i(lbm_int i) {
  bool res = true;
#ifndef LBM64
  res = res && write_u8(S_I28_VALUE, &write_index);
  res = res && write_u32_be((uint32_t)i, &write_index);
#else
  res = res && write_u8(S_I56_VALUE, &write_index);
  res = res && write_u64_be((uint64_t)i,&write_index);
#endif
  return res;
}

static bool i_f_u(lbm_uint u) {
  bool res = true;
#ifndef LBM64
  res = res && write_u8(S_U28_VALUE, &write_index);
  res = res && write_u32_be((uint32_t)u,&write_index);
#else
  res = res && write_u8(S_U56_VALUE, &write_index);
  res = res && write_64_be((uint64_t)u, &write_index);
#endif
  return res;
}

static bool i_f_b(uint8_t b) {
  bool res = true;
  res = res && write_u8(S_BYTE_VALUE, &write_index);
  res = res && write_u8(b, &write_index);
  return res;
}

static bool i_f_i32(int32_t w) {
  bool res = true;
  res = res && write_u8(S_I32_VALUE,&write_index);
  res = res && write_u32_be((uint32_t)w,&write_index);
  return res;
}

static bool i_f_u32(uint32_t w) {
  bool res = true;
  res = res && write_u8(S_U32_VALUE, &write_index);
  res = res && write_u32_be(w, &write_index);
  return res;
}

static bool i_f_float(float f) {
  bool res = true;
  res = res && write_u8(S_FLOAT_VALUE, &write_index);
  uint32_t u;
  memcpy(&u, &f, sizeof(uint32_t));
  res = res && write_u32_be((uint32_t)u, &write_index);
  return res;
}

static bool i_f_double(double d) {
  bool res = true;
  res = res && write_u8(S_DOUBLE_VALUE, &write_index);
  uint64_t u;
  memcpy(&u, &d, sizeof(uint64_t));
  res = res && write_u64_be(u, &write_index);
  return res;
}

static bool i_f_i64(int64_t w) {
  bool res = true;
  res = res && write_u8(S_I64_VALUE, &write_index);
  res = res && write_u64_be((uint64_t)w, &write_index);
  return res;
}

static bool i_f_u64(uint64_t w) {
  bool res = true;
  res = res && write_u8(S_U64_VALUE, &write_index);
  res = res && write_u64_be(w, &write_index);
  return res;
}

// num_bytes is specifically an uint32_t
static bool i_f_lbm_array(uint32_t num_bytes, uint8_t *data) {
  bool res = write_u8(S_LBM_ARRAY, &write_index);
  res = res && write_u32_be(num_bytes, &write_index);
  for (uint32_t i = 0; i < num_bytes; i ++ ) {
    if (!write_u8(data[i], &write_index)) return false;
  }
  return true;
}

static bool image_flatten_value(lbm_value v) {
  lbm_uint t = lbm_type_of(v);

  // for now, ignore constant
  if (t >= LBM_POINTER_TYPE_FIRST && t < LBM_POINTER_TYPE_LAST) {
    t = t & ~(LBM_PTR_TO_CONSTANT_BIT);
  }

  switch (t) {
  case LBM_TYPE_CONS: {
    bool res = true;
    res = res && i_f_cons();
    if (res) {
      int fv_r = image_flatten_value(lbm_car(v));
      if (fv_r) {
        fv_r = image_flatten_value(lbm_cdr(v));
      }
      return fv_r;
    }
  }break;
  case LBM_TYPE_LISPARRAY: {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(v);
    if (header) {
      lbm_value *arrdata = (lbm_value*)header->data;
      // always exact multiple of sizeof(lbm_value)
      lbm_uint size = header->size / sizeof(lbm_value);
      if (!i_f_lisp_array(size)) return FLATTEN_VALUE_ERROR_NOT_ENOUGH_MEMORY;
      int fv_r = true;
      for (lbm_uint i = 0; i < size; i ++ ) {
        fv_r =  image_flatten_value(arrdata[i]);
        if (!fv_r) {
          break;
        }
      }
      return fv_r;
    } else {
      return false;
    }
  } break;
  case LBM_TYPE_BYTE:
    if (i_f_b((uint8_t)lbm_dec_as_char(v))) {
      return true;
    }
    break;
  case LBM_TYPE_U:
    if (i_f_u(lbm_dec_u(v))) {
      return true;
    }
    break;
  case LBM_TYPE_I:
    if (i_f_i(lbm_dec_i(v))) {
      return true;
    }
    break;
  case LBM_TYPE_U32:
    if (i_f_u32(lbm_dec_as_u32(v))) {
      return true;
    }
    break;
  case LBM_TYPE_I32:
    if (i_f_i32(lbm_dec_as_i32(v))) {
      return true;
    }
    break;
  case LBM_TYPE_U64:
    if (i_f_u64(lbm_dec_as_u64(v))) {
      return true;
    }
    break;
  case LBM_TYPE_I64:
    if (i_f_i64(lbm_dec_as_i64(v))) {
      return true;
    }
    break;
  case LBM_TYPE_FLOAT:
    if (i_f_float(lbm_dec_as_float(v))) {
      return true;
    }
    break;
  case LBM_TYPE_DOUBLE:
    if (i_f_double(lbm_dec_as_double(v))) {
      return true;
    }
    break;
  case LBM_TYPE_SYMBOL: {
    //char *sym_str = (char*)lbm_get_name_by_symbol(lbm_dec_sym(v));
    if (i_f_sym(v)) {
      return true;
    }
  } break;
  case LBM_TYPE_ARRAY: {
    lbm_int s = lbm_heap_array_get_size(v);
    const uint8_t *d = lbm_heap_array_get_data_ro(v);
    if (s > 0 && d != NULL) {
      if (i_f_lbm_array((uint32_t)s, (uint8_t*)d)) {
        return true;
      }
    } else {
      return false;
    }
  }break;
  }
  return false;
}


// ////////////////////////////////////////////////////////////
// Constant heaps as part of an image.

lbm_const_heap_t image_const_heap;
lbm_uint image_const_heap_start_ix = 0;

bool image_const_heap_write(lbm_uint ix, lbm_uint w) {
  lbm_uint i = image_const_heap_start_ix + (ix * 4);
  return write_u32(w, &i);
}

// ////////////////////////////////////////////////////////////
// Image manipulation

lbm_uint *lbm_image_add_symbol(char *name, lbm_uint id, lbm_uint symlist) {
  printf("adding symbol %s at address: %x\n", name, (lbm_uint)name);
  bool r = write_u8(SYMBOL_ENTRY, &write_index);
  lbm_uint entry_ptr = (lbm_uint)image_address + write_index;
  r = r && write_lbm_uint((lbm_uint)name, &write_index);
  r = r && write_lbm_uint(id, &write_index);
  r = r && write_lbm_uint(symlist, &write_index);
  if (r)
    return (lbm_uint*)entry_ptr;
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
        if (lbm_is_constant(val_field)) {
          printf("storing constant binding\n");
          write_u8(BINDING_CONST, &write_index);
          write_lbm_value(name_field, &write_index);
          write_lbm_value(val_field, &write_index);
        } else {
          int fv_size = flatten_value_size(val_field, lbm_get_max_flatten_depth());
          int tot_size = sizeof(lbm_uint) + fv_size + 1;
          if (write_index + tot_size >= image_size) {
            printf("out of room in image\n");
            return false;
          }
          write_u8(BINDING_FLAT, &write_index);
          write_u32(fv_size + sizeof(lbm_uint), &write_index);
          write_lbm_value(name_field, &write_index);
          image_flatten_value(val_field);
          printf("flattened env field into image\n");
        }
        curr = lbm_cdr(curr);
      }
    }
    return true;
  }
  return false;
}

bool lbm_image_save_startup_fv(uint8_t *data, uint32_t size) {

  bool r = write_u8(STARTUP_ENTRY, &write_index);
  r = r && write_u32(size, &write_index);
  for (uint32_t i = 0; i < size; i ++) {
    r = r && write_u8(data[i], &write_index);
  }
  return r;
}

bool lbm_image_save_constant_heap_ix(void) {
  bool r = write_u8(CONSTANT_HEAP_IX, &write_index);
  r = r && write_u32(image_const_heap.next, &write_index);
  return r;
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
    write_u8(CONSTANT_HEAP, &write_index);
    write_u32(size_bytes, &write_index);
    write_index += CONSTANT_HEAP_ALIGN_PAD;
    write_index += size_bytes;
    printf("const heap created %d\n", write_index);

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
    uint8_t val = read_u8(pos);
    switch(val) {
    case CONSTANT_HEAP: {
      printf("constant heap found\n");
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
    case CONSTANT_HEAP_IX: {
      pos ++;
      uint32_t next = read_u32(pos);
      pos += 4;
      image_const_heap.next = next;
    } break;
    case BINDING_CONST: {
      printf("binding const\n");
      pos ++; //Jump past the BINDING_CONST Tag
#ifdef LBM64
      lbm_uint bind_key = read_u64(pos);
      lbm_uint bind_val = read_u64(pos+8);
#else
      lbm_uint bind_key = read_u32(pos);
      lbm_uint bind_val = read_u32(pos+4);
#endif
      lbm_uint ix_key  = lbm_dec_sym(bind_key) & GLOBAL_ENV_MASK;
      lbm_value *global_env = lbm_get_global_env();
      lbm_uint orig_env = global_env[ix_key];
      lbm_value new_env = lbm_env_set(orig_env,bind_key,bind_val);

      if (lbm_is_symbol(new_env)) {
        printf("error restoring binding from image\n");
      }
      global_env[ix_key] = new_env;
      pos += sizeof(lbm_uint) * 2;
    } break;
    case BINDING_FLAT: {
      printf("binding flat\n");
      pos ++; // skip tag
      uint32_t s = read_u32(pos); pos += 4;
#ifdef LBM64
      lbm_uint bind_key = read_u64(pos);
#else
      lbm_uint bind_key = read_u32(pos);
#endif
      pos += sizeof(lbm_uint);
      lbm_flat_value_t fv;
      fv.buf = (uint8_t*)(image_address + pos);
      fv.buf_size = s - 4;
      fv.buf_pos = 0;
      lbm_value unflattened;
      lbm_unflatten_value(&fv, &unflattened);
      if (lbm_is_symbol_merror(unflattened)) {
        lbm_perform_gc();
        lbm_unflatten_value(&fv, &unflattened);
      }

      lbm_uint ix_key  = lbm_dec_sym(bind_key) & GLOBAL_ENV_MASK;
      lbm_value *global_env = lbm_get_global_env();
      lbm_uint orig_env = global_env[ix_key];
      lbm_value new_env = lbm_env_set(orig_env,bind_key,unflattened);

      if (lbm_is_symbol(new_env)) {
        printf("error restoring binding from image\n");
      }
      global_env[ix_key] = new_env;
      pos += s - 4;
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
      // Remember that this case is evaluated on a fresh image
      // as well at startup.
      write_index = pos;
      goto done_loading_image;
      break;
    }
  }
 done_loading_image:
  return;
}
