/*
    Copyright 2025 Joel Svensson        svenssonjoel@yahoo.se

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

#include <math.h>
#include <string.h>
#include <stdio.h>

#ifdef LBM_OPT_DSP_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_DSP_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif

#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define LBM_SYSTEM_LITTLE_ENDIAN true
#else
#define LBM_SYSTEM_LITTLE_ENDIAN false
#endif


static lbm_uint sym_inverse = 0;
static lbm_uint sym_big_endian = 0;
static lbm_uint sym_little_endian = 0;

static inline uint32_t byte_order_swap(uint32_t w) {
  uint32_t r = w;
  uint8_t *bytes = (uint8_t*)&r;
  uint8_t tmp = bytes[0];
  bytes[0] = bytes[3];
  bytes[3] = tmp;
  tmp = bytes[1];
  bytes[1] = bytes[2];
  bytes[2] = tmp;

  return r;
}


void lbm_fft(float *real, float *imag, int n, int inverse) {
  int k = 0;
  for (int i = 1; i < n; i++) {
    int bit = n >> 1;
    while (k & bit) {
      k ^= bit;
      bit >>= 1;
    }
    k ^= bit;

    if (i < k) {
      float temp = real[i];
      real[i] = real[k];
      real[k] = temp;

      temp = imag[i];
      imag[i] = imag[k];
      imag[k] = temp;
    }
  }

  for (int len = 2; len <= n; len <<= 1) {
    float angle = (inverse ? 2.0f : -2.0f) * (float)M_PI / (float)len;
    float wlen_r = cosf(angle);
    float wlen_i = sinf(angle);

    for (int i = 0; i < n; i += len) {
      float w_r = 1.0f;
      float w_i = 0.0f;

      for (int j = 0; j < len / 2; j++) {
        float u_r = real[i + j];
        float u_i = imag[i + j];
        float v_r = real[i + j + len / 2];
        float v_i = imag[i + j + len / 2];

        float t_r = w_r * v_r - w_i * v_i;
        float t_i = w_r * v_i + w_i * v_r;

        real[i + j] = u_r + t_r;
        imag[i + j] = u_i + t_i;
        real[i + j + len / 2] = u_r - t_r;
        imag[i + j + len / 2] = u_i - t_i;

        float next_w_r = w_r * wlen_r - w_i * wlen_i;
        float next_w_i = w_r * wlen_i + w_i * wlen_r;
        w_r = next_w_r;
        w_i = next_w_i;
      }
    }
  }

  if (inverse) {
    for (int i = 0; i < n; i++) {
      real[i] /= (float)n;
      imag[i] /= (float)n;
    }
  }
}

static lbm_value ext_fft_f32(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn >= 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    int inverse = 0;
    bool be = true;

    if (argn > 2 &&
        lbm_is_symbol(args[2])) {
      lbm_uint sym = lbm_dec_sym(args[2]);
      if (sym == sym_inverse) {
        inverse = 1;
      } else if (sym == sym_little_endian) {
        be = false;
      }
    }

    if (argn > 3 &&
        lbm_is_symbol(args[3])) {
      if (lbm_dec_sym(args[3]) == sym_little_endian) {
        be = false;
      }
    }

    bool swap_byte_order =
      (be && LBM_SYSTEM_LITTLE_ENDIAN) ||
      (!be && !LBM_SYSTEM_LITTLE_ENDIAN);

    lbm_array_header_t *real_arr = lbm_dec_array_r(args[0]);
    lbm_array_header_t *imag_arr = lbm_dec_array_r(args[1]);

    if (!real_arr || !imag_arr) {
      return ENC_SYM_TERROR;
    }

    if (real_arr->size == imag_arr->size &&
        real_arr->size > sizeof(float) && // need more than 4 bytes for a float
        (real_arr->size % sizeof(float) == 0)) {
      lbm_uint n = real_arr->size / sizeof(float);
      lbm_uint padded_size = n;
      if ((n & (n - 1)) != 0) { // not a power of two. needs padding.
        padded_size = 1;
        while (padded_size < n) padded_size <<= 1;
      }

      lbm_value real_copy;
      lbm_value imag_copy;
      if (lbm_heap_allocate_array(&real_copy, padded_size * sizeof(float)) &&
          lbm_heap_allocate_array(&imag_copy, padded_size * sizeof(float))) {
        float *real_data = (float*)real_arr->data;
        float *imag_data = (float*)imag_arr->data;

        lbm_array_header_t *real_copy_arr = lbm_dec_array_r(real_copy);
        lbm_array_header_t *imag_copy_arr = lbm_dec_array_r(imag_copy);
        float *real_copy_data = (float*)real_copy_arr->data;
        float *imag_copy_data = (float*)imag_copy_arr->data;
        memset(real_copy_data, 0, padded_size * sizeof(float));
        memset(imag_copy_data, 0, padded_size * sizeof(float));

        if (swap_byte_order) {
          for (lbm_uint w = 0; w < n; w ++) {
            uint32_t swapped = byte_order_swap(((uint32_t*)real_data)[w]);
            ((uint32_t*)real_copy_data)[w] = swapped;
            swapped = byte_order_swap(((uint32_t*)imag_data)[w]);
            ((uint32_t*)imag_copy_data)[w] = swapped;
          }
        } else {
          memcpy(real_copy_data, real_data, real_arr->size);
          memcpy(imag_copy_data, imag_data, real_arr->size);
        }

        lbm_fft(real_copy_data, imag_copy_data, (int)padded_size, inverse);

        if (swap_byte_order) {
          for (lbm_uint w = 0; w < padded_size; w ++) {
            uint32_t swapped = byte_order_swap(((uint32_t*)real_copy_data)[w]);
            ((uint32_t*)real_copy_data)[w] = swapped;
            swapped = byte_order_swap(((uint32_t*)imag_copy_data)[w]);
            ((uint32_t*)imag_copy_data)[w] = swapped;
          }
        }
        r = lbm_cons(real_copy, imag_copy);
      } else {
        r = ENC_SYM_MERROR;
      }
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

void lbm_dsp_extensions_init(void) {
  lbm_add_symbol("inverse", &sym_inverse);
  lbm_add_symbol("little-endian", &sym_little_endian);
  lbm_add_symbol("big-endian", &sym_big_endian);

  lbm_add_extension("fft", ext_fft_f32);
}
