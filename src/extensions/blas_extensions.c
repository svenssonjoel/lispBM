/*
    Copyright 2023 Joel Svensson        svenssonjoel@yahoo.se

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

#include "extensions.h"
#include "lbm_utils.h"
#include "lbm_custom_type.h"

static const char *vector_float_desc = "Vector-Float";
//static const char *matrix_float_desc = "Matrix-Float";

typedef struct {
  unsigned int size;
  float data[1];
} vector_float_t;

static bool common_destructor(lbm_uint value) {
  lbm_free((void*)value);
  return true;
}

static lbm_value vector_float_allocate(lbm_uint size) {
  vector_float_t *mem = lbm_malloc( 1 * sizeof(lbm_uint) +
                                    size * sizeof(float));
  if (!mem) return ENC_SYM_MERROR;
  mem->size = size;
  lbm_value res;
  lbm_custom_type_create((lbm_uint)mem,
                         common_destructor,
                         vector_float_desc,
                         &res);
  return res;
}

static bool is_vector_float(lbm_value v) {
  return ((lbm_uint)lbm_get_custom_descriptor(v) == (lbm_uint)vector_float_desc);
}

/* typedef struct { */
/*   unsigned int size1; */
/*   unsigned int size2; */
/*   float data[1]; */
/* } matrix_float_t; */

/* static lbm_value matrix_float_allocate(unsigned int size1, unsigned int size2) { */
/*   matrix_float_t *mem = lbm_malloc(1 * sizeof(lbm_uint) + */
/*                                    1 * sizeof(lbm_uint) + */
/*                                    size1 * size2 * sizeof(float)); */
/*   if (!mem) return ENC_SYM_MERROR; */
/*   mem->size1 = size1; */
/*   mem->size2 = size2; */
/*   lbm_value res; */
/*   lbm_custom_type_create((lbm_uint)mem, */
/*                          common_destructor, */
/*                          matrix_float_desc, */
/*                          &res); */
/*   return res; */
/* } */

/* static bool is_matrix_float(lbm_value m) { */
/*   return ((lbm_uint)lbm_get_custom_descriptor(m) == (lbm_uint)matrix_float_desc); */
/* } */

/* **************************************************
 * Extension implementations
 */

static lbm_value ext_vector(lbm_value *args, lbm_uint argn) {

  LBM_CHECK_NUMBER_ALL();

  if (argn < 1) return ENC_SYM_EERROR;

  lbm_value vec = vector_float_allocate(argn);
  if (lbm_is_error(vec)) return vec;

  vector_float_t *lvec = (vector_float_t*)lbm_get_custom_value(vec);

  for (lbm_uint i = 0; i < argn; i ++) {
    lvec->data[i] = lbm_dec_as_float(args[i]);
  }
  return vec;
}

static lbm_value ext_vector_to_list(lbm_value *args, lbm_uint argn) {

  if (argn != 1 || !is_vector_float(args[0])) {
    return ENC_SYM_EERROR;
  }

  vector_float_t *lvec = (vector_float_t*)lbm_get_custom_value(args[0]);

  lbm_value result = lbm_heap_allocate_list(lvec->size);
  if (lbm_is_cons(result)) {
    lbm_value curr = result;
    for (lbm_uint i = 0; i < lvec->size; i ++) {
      lbm_value f_val = lbm_enc_float(lvec->data[i]);
      if (lbm_is_error(f_val)) {
        result = f_val;
        break;
      }
      lbm_set_car(curr, f_val);
      curr = lbm_cdr(curr);
    }
  }

  return result;
}

static lbm_value ext_saxpy(lbm_value *args, lbm_uint argn ) {

  lbm_value res = ENC_SYM_TERROR;

  if (argn != 3) return res;
  lbm_value a = args[0];
  lbm_value x = args[1];
  lbm_value y = args[2];

  if (is_vector_float(x) && is_vector_float(y) && lbm_is_number(a)) {

    float alpha = lbm_dec_as_float(a);
    vector_float_t *X = (vector_float_t*)lbm_get_custom_value(x);
    vector_float_t *Y = (vector_float_t*)lbm_get_custom_value(y);

    unsigned int res_size = MIN(X->size, Y->size);

    res = vector_float_allocate(res_size);
    if (!lbm_is_symbol_merror(res)) {

      vector_float_t *R = (vector_float_t*)lbm_get_custom_value(res);

      for (unsigned i = 0; i < res_size; i ++) {
        R->data[i] = alpha * X->data[i] + Y->data[i];
      }
    }
  }
  return res;
}

static lbm_value ext_sdot(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;

  if (argn != 2) return res;
  lbm_value x = args[0];
  lbm_value y = args[1];

  if (is_vector_float(x) && is_vector_float(y)) {

    vector_float_t *X = (vector_float_t*)lbm_get_custom_value(x);
    vector_float_t *Y = (vector_float_t*)lbm_get_custom_value(y);

    unsigned int res_size = MIN(X->size, Y->size);

    float f_res = 0;
    for (unsigned i = 0; i < res_size; i ++) {
      f_res +=  X->data[i] * Y->data[i];
    }
    res = lbm_enc_float(f_res);
  }
  return res;
}

bool lbm_blas_extensions_init(void) {
  bool res = true;

  // Vectors
  res = res && lbm_add_extension("vector", ext_vector);
  res = res && lbm_add_extension("vector-to-list", ext_vector_to_list);
  res = res && lbm_add_extension("saxpy", ext_saxpy);
  res = res && lbm_add_extension("sdot", ext_sdot);

  // Matrices

  return res;
}
