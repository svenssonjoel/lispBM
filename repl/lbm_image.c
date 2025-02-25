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


#define CONSTANT_HEAP 0x01 // [0x01 | size words | pad | data]
#define HEAP_IMAGE    0x02 // [0x02 | size TBD   | pad | data]
#define MEMORY_IMAGE  0x03 // [0x03 | size TBD   | pad | data] 
#define BINDING_CONST 0x04 // TBD
#define BINDING_FLAT  0x05 // TBD
#define STARTUP_ENTRY 0x06 // [0x06 | size bytes| flatval])

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
        }
        curr = lbm_cdr(curr);
      }
    }
    return true;
  } 
  return false;
}

bool lbm_image_save_startup_fv(uint8_t *data, uint32_t size) {
  uint8_t b0 = (uint8_t)(size >> 8);
  uint8_t b1 = (uint8_t)size;

  bool r = image_write(write_index, STARTUP_ENTRY);
  r = r && image_write(write_index+1, b0);
  r = r && image_write(write_index+2, b1);

  for (int i = 0; i < size; i ++) {
    r = r && image_write(write_index+3+i, data[i]);
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

void lbm_image_create_const_heap(uint32_t size) {

  // size in the const heap is multiples of lbm_uint size

  uint8_t b0 = (uint8_t)(size >> 8);
  uint8_t b1 = (uint8_t)size;

  image_write(write_index, CONSTANT_HEAP);
  image_write(write_index+1, b0);
  image_write(write_index+2, b1);
  write_index+=4;
  write_index+= size * sizeof(lbm_uint);
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
      image_is_empty = false;
      uint16_t size = read_u16(pos+1); // in number of words
      pos += 4; // skip over pad
      // TODO: const_heap_init argument order is not ideal.
      image_const_heap_start_ix = pos;
      lbm_const_heap_init(image_const_heap_write,
                          &image_const_heap,
                          (lbm_uint*)(image_address + pos),
                          (lbm_uint)size);
      pos += sizeof(lbm_uint)*size; 
    } break;
    case BINDING_CONST: {
      lbm_uint sym_id;
      lbm_uint name_size = strlen((char*)(image_address + pos + 1)) + 1;
      if (!lbm_add_symbol((char *)(image_address + pos + 1), &sym_id)) {
        printf("error reading image\n");
        goto done_loading_image;
      }
      lbm_uint val = read_u32(pos+name_size+1); // TODO: Incorrect on 64bit
      lbm_uint ix_key  = sym_id & GLOBAL_ENV_MASK;
      lbm_value *global_env = lbm_get_global_env();
      lbm_uint orig_env = global_env[ix_key];
      lbm_value new_env = lbm_env_set(orig_env,lbm_enc_sym(sym_id),val);
      
      if (lbm_is_symbol(new_env)) {
        printf("error restoring binding from image\n");
      }
      global_env[ix_key] = new_env;
      pos += 5 + name_size;
    } break;
    case STARTUP_ENTRY: {
      image_startup = true;
      uint16_t size = read_u16(pos+1); // in bytes
      image_startup_size = size;
      image_startup_position = pos+3;
      pos += 3 + size;
    } break;
    default:
      goto done_loading_image;
      break;
    }
  }
 done_loading_image:
  return;

}

 



