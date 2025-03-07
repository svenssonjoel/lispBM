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


// MARCH 5
// -- Can constants (anything on const heap) contain references into non-constant heap?
//    - I think not, but should verify this.
//    - eval_cps move_to_flash performs a deep copy into flash.

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
// in all cell-pointers into constant heap.



#define CONSTANT_HEAP    (uint32_t)0x01       // [0x01   | size bytes | data]
#define CONSTANT_HEAP_IX (uint32_t)0x02       // [0x02   | uint32]
#define BINDING_CONST    (uint32_t)0x03       // [0x03   | key | lbm_uint ]
#define BINDING_FLAT     (uint32_t)0x04       // [0x04   | size bytes | key | flatval ]
#define STARTUP_ENTRY    (uint32_t)0x05       // [0x05   | symbol ])
#define SYMBOL_ENTRY     (uint32_t)0x06       // [0x06   | ID | NAME PTR | NEXT_PTR] // symbol_entry with highest address is root.
// tagged data  that can vary in size has a size bytes field.
// Fixed size data does not.

// To be able to work on an image incrementally (even though it is not recommended)
// many fields are allowed to be duplicated and the later ones have priority
// over earlier ones.

static lbm_image_write_fun image_write = NULL;
static lbm_image_clear_fun image_clear = NULL;

static bool image_is_empty = true;
static uint32_t *image_address = NULL;
static uint32_t write_index = 0;
static uint32_t image_size = 0;
static bool image_startup = false;
static uint32_t image_startup_size;
static lbm_value image_startup_symbol = ENC_SYM_NIL;

uint32_t *lbm_image_get_image(void) {
  return image_address;
}

uint32_t lbm_image_get_size(void) {
  return image_size;
}

bool lbm_image_has_startup(void) {
  return image_startup;
}

lbm_value lbm_image_get_startup(void) {
  return image_startup_symbol;
}

uint32_t lbm_image_startup_size(void) {
  return image_startup_size;
}

uint32_t read_u32(uint32_t index) {
  return *((uint32_t*)(image_address + index));
}

uint64_t read_u64(uint32_t index) {
  return *((uint64_t*)(image_address + index));
}

bool write_u32(uint32_t w, uint32_t *i) {
  bool r = image_write(w, *i);
  (*i)++;
  return r;
}

bool write_u64(uint64_t dw, uint32_t *i) {
  uint32_t * words = (uint32_t*)&dw;
  return
    write_u32(words[0], i) &&
    write_u32(words[1], i);
}

// fv_write function write values as big endian.

uint32_t fv_buf_ix = 0;
uint8_t  fv_buf[4] = {0};
bool fv_write_u8(uint8_t b) {
  bool r = true;
  if (fv_buf_ix >= 4) {
    r = write_u32(((uint32_t*)fv_buf)[0], &write_index);
    memset(fv_buf,0,4);
    fv_buf_ix = 0;
  }
  fv_buf[fv_buf_ix] = b;
  fv_buf_ix++;
  return r;
}

bool fv_write_flush(void) {
  if (fv_buf_ix == 0) return true;
  else {
    bool r = write_u32(((uint32_t*)fv_buf)[0], &write_index);;
    fv_buf_ix = 0;
    memset(fv_buf,0,4);
    return r;
  }
}

bool fv_write_u32(uint32_t w) {
  uint8_t * bytes = (uint8_t*)&w;
  return
    fv_write_u8(bytes[3]) &&
    fv_write_u8(bytes[2]) &&
    fv_write_u8(bytes[1]) &&
    fv_write_u8(bytes[0]);
}

bool fv_write_u64(uint64_t dw) {
  uint8_t * bytes = (uint8_t*)&dw;
   return
     fv_write_u8(bytes[7]) &&
     fv_write_u8(bytes[6]) &&
     fv_write_u8(bytes[5]) &&
     fv_write_u8(bytes[4]) &&
     fv_write_u8(bytes[3]) &&
     fv_write_u8(bytes[2]) &&
     fv_write_u8(bytes[1]) &&
     fv_write_u8(bytes[0]);
}


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
  return fv_write_u8(S_CONS);
}

static bool i_f_lisp_array(uint32_t size) {
  // arrays are smaller than 2^32 elements long
  bool r = fv_write_u8(S_LBM_LISP_ARRAY);
  r = r && fv_write_u32(size);
  return r;
}

static bool i_f_sym(lbm_value sym) {
  lbm_uint sym_id = lbm_dec_sym(sym);
  bool r = fv_write_u8(S_SYM_VALUE);
  #ifndef LBM64
  r = r && fv_write_u32(sym_id);
  #else
  r = r && fv_write_u64(sym_id);
  #endif
  return r;
}

static bool i_f_i(lbm_int i) {
  bool res = true;
#ifndef LBM64
  res = res && fv_write_u8(S_I28_VALUE);
  res = res && fv_write_u32((uint32_t)i);
#else
  res = res && fv_write_u8(S_I56_VALUE);
  res = res && fv_write_u64((uint64_t)i);
#endif
  return res;
}

static bool i_f_u(lbm_uint u) {
  bool res = true;
#ifndef LBM64
  res = res && fv_write_u8(S_U28_VALUE);
  res = res && fv_write_u32((uint32_t)u);
#else
  res = res && fv_write_u8(S_U56_VALUE);
  res = res && fv_write_64((uint64_t)u);
#endif
  return res;
}

static bool i_f_b(uint8_t b) {
  bool res = true;
  res = res && fv_write_u8(S_BYTE_VALUE);
  res = res && fv_write_u8(b);
  return res;
}

static bool i_f_i32(int32_t w) {
  bool res = true;
  res = res && fv_write_u8(S_I32_VALUE);
  res = res && fv_write_u32((uint32_t)w);
  return res;
}

static bool i_f_u32(uint32_t w) {
  bool res = true;
  res = res && fv_write_u8(S_U32_VALUE);
  res = res && fv_write_u32(w);
  return res;
}

static bool i_f_float(float f) {
  bool res = true;
  res = res && fv_write_u8(S_FLOAT_VALUE);
  uint32_t u;
  memcpy(&u, &f, sizeof(uint32_t));
  res = res && fv_write_u32((uint32_t)u);
  return res;
}

static bool i_f_double(double d) {
  bool res = true;
  res = res && fv_write_u8(S_DOUBLE_VALUE);
  uint64_t u;
  memcpy(&u, &d, sizeof(uint64_t));
  res = res && fv_write_u64(u);
  return res;
}

static bool i_f_i64(int64_t w) {
  bool res = true;
  res = res && fv_write_u8(S_I64_VALUE);
  res = res && fv_write_u64((uint64_t)w);
  return res;
}

static bool i_f_u64(uint64_t w) {
  bool res = true;
  res = res && fv_write_u8(S_U64_VALUE);
  res = res && fv_write_u64(w);
  return res;
}

// num_bytes is specifically an uint32_t
static bool i_f_lbm_array(uint32_t num_bytes, uint8_t *data) {
  bool res = fv_write_u8(S_LBM_ARRAY);
  res = res && fv_write_u32(num_bytes);
  for (uint32_t i = 0; i < num_bytes; i ++ ) {
    if (!fv_write_u8(data[i])) return false;
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

bool image_const_heap_write(uint32_t w, lbm_uint ix) {
  lbm_uint i = image_const_heap_start_ix + ix;
  return write_u32(w, &i);
}

// ////////////////////////////////////////////////////////////
// Image manipulation

lbm_uint *lbm_image_add_symbol(char *name, lbm_uint id, lbm_uint symlist) {
  bool r = write_u32(SYMBOL_ENTRY, &write_index);
  lbm_uint entry_ptr = (lbm_uint)(image_address + write_index);
  r = r && write_lbm_uint((lbm_uint)name, &write_index);
  r = r && write_lbm_uint(id, &write_index);
  r = r && write_lbm_uint(symlist, &write_index);
  if (r)
    return (lbm_uint*)entry_ptr;
  return NULL;
}

// Set the name of the startup function from the env.
bool lbm_image_save_startup(lbm_value sym) {
  bool r = write_u32(STARTUP_ENTRY, &write_index);
  r = r && write_lbm_value(sym, &write_index);
  return r;
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
          write_u32(BINDING_CONST, &write_index);
          write_lbm_value(name_field, &write_index);
          write_lbm_value(val_field, &write_index);
        } else {
          int fv_size = flatten_value_size(val_field, false);
          if (fv_size > 0) {
            fv_size = (fv_size % 4 == 0) ? (fv_size / 4) : (fv_size / 4) + 1;
            int tot_size =  fv_size + 1 + (int)(sizeof(lbm_uint) / 4);

            if (write_index + (uint32_t)tot_size >=  image_size) {
              return false;
            }
            write_u32(BINDING_FLAT, &write_index);
            write_u32((uint32_t)fv_size + (sizeof(lbm_uint) / 4), &write_index);
            write_lbm_value(name_field, &write_index);
            image_flatten_value(val_field);
            fv_write_flush();
          } else {
            return false;
          }
        }
        curr = lbm_cdr(curr);
      }
    }
    return true;
  }
  return false;
}

bool lbm_image_save_constant_heap_ix(void) {
  bool r = write_u32(CONSTANT_HEAP_IX, &write_index);
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
  if (size_words+2 < image_size) {
    write_u32(CONSTANT_HEAP, &write_index);
    write_u32(size_words, &write_index);
    write_index += size_words;
    return true;
  }
  return false;
}

void lbm_image_init(uint32_t* image_mem_address,
                    uint32_t image_size_words,
                    lbm_image_write_fun image_write_fun) {
  image_write = image_write_fun;
  image_address = image_mem_address;
  image_size = image_size_words;
  write_index = 0;
  
}

bool lbm_image_exists(void) {
  uint32_t val = read_u32(0);
  return val == CONSTANT_HEAP; // constant heap always first thing in image
}

bool lbm_image_boot(void) {
  //process image
  uint32_t pos = 0;

  while (pos < image_size) {
    uint32_t val = read_u32(pos);
    pos ++;
    switch(val) {
    case CONSTANT_HEAP: {
      image_is_empty = false;
      uint32_t size = read_u32(pos); // Words
      pos++;
      image_const_heap_start_ix = pos;
      lbm_const_heap_init(image_const_heap_write,
                          &image_const_heap,
                          (lbm_uint*)(image_address + pos),
                          (lbm_uint)size); // size in words
      pos += size;
    } break;
    case CONSTANT_HEAP_IX: {
      uint32_t next = read_u32(pos);
      pos ++;
      image_const_heap.next = next;
    } break;
    case BINDING_CONST: {
#ifdef LBM64
      lbm_uint bind_key = read_u64(pos);
      lbm_uint bind_val = read_u64(pos+2);
#else
      lbm_uint bind_key = read_u32(pos);
      lbm_uint bind_val = read_u32(pos+1);
#endif
      lbm_uint ix_key  = lbm_dec_sym(bind_key) & GLOBAL_ENV_MASK;
      lbm_value *global_env = lbm_get_global_env();
      lbm_uint orig_env = global_env[ix_key];
      lbm_value new_env = lbm_env_set(orig_env,bind_key,bind_val);

      if (lbm_is_symbol(new_env)) {
        return false;
      }
      global_env[ix_key] = new_env;
      pos += (sizeof(lbm_uint)/4) * 2;
    } break;
    case BINDING_FLAT: {
      uint32_t s = read_u32(pos); pos +=1;
      uint32_t sp = pos;
#ifdef LBM64
      lbm_uint bind_key = read_u64(pos);
#else
      lbm_uint bind_key = read_u32(pos);
#endif
      pos += (sizeof(lbm_uint) / 4);
      lbm_flat_value_t fv;
      fv.buf = (uint8_t*)(image_address + pos);
      fv.buf_size = (s * 4); // larger than actual buf
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
        return false;
      }
      global_env[ix_key] = new_env;
      pos = sp + s;
    } break;
    case STARTUP_ENTRY: {
      image_startup = true;
      lbm_value sym;
#ifdef LBM64
      sym = read_u64(pos);
      pos  += 2;
#else
      sym = read_u32(pos);
      pos  ++;
#endif
      image_startup_symbol = sym;
    } break;
    case SYMBOL_ENTRY: {
      lbm_symrepr_set_symlist((lbm_uint*)(image_address + pos));
      pos += 3 * (sizeof(lbm_uint) / 4);
    } break;
    default:
      write_index = pos-1;
      goto done_loading_image;
      break;
    }
  }
 done_loading_image:
  return true;
}
