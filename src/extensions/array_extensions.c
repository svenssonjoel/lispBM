/*
    Copyright 2022 Joel Svensson        svenssonjoel@yahoo.se

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

#include "extensions/array_extensions.h"

#include "extensions.h"
#include "symrepr.h"

static lbm_uint little_endian = 0;
static lbm_uint big_endian = 0;

static lbm_value array_extension_buffer_append_16(lbm_value *args, lbm_uint argn);

bool array_extensions_init(void) {

  if (!lbm_get_symbol_by_name("little-endian", &little_endian)) {
    if (!lbm_add_symbol_const("little-endian", &little_endian)) {
      return false;
    }
  }
  if (!lbm_get_symbol_by_name("big-endian", &big_endian)) {
    if (!lbm_add_symbol_const("big-endian", &big_endian)) {
      return false;
    }
  }
  bool res = true;
  res = res && lbm_add_extension("buffer-append-16", array_extension_buffer_append_16);
  return res;
}


/* Going to use the naming convention that a buffer is an array of bytes */ 
lbm_value array_extension_buffer_append_16(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_EERROR);
  bool be = false;

  switch(argn) {

  case 4:
    if (lbm_type_of(args[3]) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(args[3]) == big_endian) {
      be = true;
    }
    /* fall through */
  case 3:
    if(lbm_type_of(args[0]) != LBM_PTR_TYPE_ARRAY ||
       !lbm_is_number(args[1]) ||
       !lbm_is_number(args[2])) {
      return res;
    }
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
    if (array->elt_type != LBM_VAL_TYPE_BYTE) {
      return res;
    }
    lbm_uint value = lbm_dec_as_u(args[1]);
    lbm_uint index = lbm_dec_as_u(args[2]);

    if (index+1 >= array->size) {
      return res;
    }

    uint8_t *data = (uint8_t*)array->data;    

    if (be) {
      data[index+1]  = (uint8_t)value;
      data[index]    = (uint8_t)(value >> 8);  
    } else {
      data[index]    = (uint8_t)value;
      data[index +1] = (uint8_t)(value >> 8);  
    }
    break;
  default:
    break;
  }
  return res;
}

lbm_value array_extension_buffer_append_32(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_EERROR);
  bool be = false;

  switch(argn) {

  case 4:
    if (lbm_type_of(args[3]) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(args[3]) == big_endian) {
      be = true;
    }
    /* fall through */
  case 3:
    if(lbm_type_of(args[0]) != LBM_PTR_TYPE_ARRAY ||
       !lbm_is_number(args[1]) ||
       !lbm_is_number(args[2])) {
      return res;
    }
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
    if (array->elt_type != LBM_VAL_TYPE_BYTE) {
      return res;
    }
    lbm_uint value = lbm_dec_as_u(args[1]);
    lbm_uint index = lbm_dec_as_u(args[2]);

    if (index+1 >= array->size) {
      return res;
    }

    uint8_t *data = (uint8_t*)array->data;    

    if (be) {
      data[index+3]  = (uint8_t)value;
      data[index+2]  = (uint8_t)(value >> 8);
      data[index+1]  = (uint8_t)(value >> 16);
      data[index]    = (uint8_t)(value >> 24);  
    } else {
      data[index]    = (uint8_t)value;
      data[index+1]  = (uint8_t)(value >> 8);
      data[index+2]  = (uint8_t)(value >> 16);
      data[index+3]  = (uint8_t)(value >> 24); 
    }
    break;
  default:
    break;
  }
  return res;
}



/*
  void buffer_append_int16(uint8_t* buffer, int16_t number, int32_t *index);
  void buffer_append_uint16(uint8_t* buffer, uint16_t number, int32_t *index);
  void buffer_append_int32(uint8_t* buffer, int32_t number, int32_t *index);
  void buffer_append_uint32(uint8_t* buffer, uint32_t number, int32_t *index);
  void buffer_append_float16(uint8_t* buffer, float number, float scale, int32_t *index);
  void buffer_append_float32(uint8_t* buffer, float number, float scale, int32_t *index);
  void buffer_append_float32_auto(uint8_t* buffer, float number, int32_t *index);
  int16_t buffer_get_int16(const uint8_t *buffer, int32_t *index);
  uint16_t buffer_get_uint16(const uint8_t *buffer, int32_t *index);
  int32_t buffer_get_int32(const uint8_t *buffer, int32_t *index);
  uint32_t buffer_get_uint32(const uint8_t *buffer, int32_t *index);
  float buffer_get_float16(const uint8_t *buffer, float scale, int32_t *index);
  float buffer_get_float32(const uint8_t *buffer, float scale, int32_t *index);
  float buffer_get_float32_auto(const uint8_t *buffer, int32_t *index);
 */
