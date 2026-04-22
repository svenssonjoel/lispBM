/*
    Copyright 2026 Joel Svensson        svenssonjoel@yahoo.se

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

#include <string.h>

#include "extensions.h"
#include "lbm_c_interop.h"
#include "bignum_extensions.h"


#ifdef LBM_OPT_BIGNUM_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_BIGNUM_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif


// Add up 2 bignum of lengths alen, blen.
// Assumes both inputs are normalized and alen >= blen.
// res must be preallocated to alen + 1.
static void bn_add(uint32_t *a, uint32_t alen,
                   uint32_t *b, uint32_t blen,
                   uint32_t *res) {
  uint64_t carry = 0;
  for (uint32_t i = 0; i < alen; i++) {
    uint64_t sum = (uint64_t)a[i] + (i < blen ? b[i] : 0) + carry;
    res[i] = (uint32_t)sum;
    carry  = sum >> 32;
  }
  res[alen] = (uint32_t)carry;
}

// Subtract bignum b from a.
// Assumes both inputs are normalized and a >= b.
// res must be preallocated to alen.
static void bn_sub(uint32_t *a, uint32_t alen,
                   uint32_t *b, uint32_t blen,
                   uint32_t *res) {
  uint32_t borrow = 0;
  for (uint32_t i = 0; i < alen; i++) {
    uint32_t ai = a[i];
    uint32_t bi = i < blen ? b[i] : 0;
    uint32_t diff = ai - bi - borrow;
    borrow = (diff > ai) ? 1 : 0;
    res[i] = diff;
  }
}

// Multiply bignum a by bignum b.
// Assumes both inputs are normalized.
// res must be preallocated to alen + blen limbs.
static void bn_mul(uint32_t *a, uint32_t alen,
                   uint32_t *b, uint32_t blen,
                   uint32_t *res) {
  memset(res, 0, (alen + blen) * sizeof(uint32_t));
  for (uint32_t i = 0; i < alen; i++) {
    uint64_t carry = 0;
    for (uint32_t j = 0; j < blen; j++) {
      uint64_t prod = (uint64_t)a[i] * b[j] + (uint64_t)res[i+j] + carry;
      res[i+j] = (uint32_t)prod;
      carry    = prod >> 32;
    }
    res[i + blen] = (uint32_t)carry;
  }
}

// Normalize the length of a bignum.
// The smallest valid length of bignum is 1 limb.
static uint32_t bn_normalize_len(uint32_t *a, uint32_t alen) {
  uint32_t n = alen-1;
  while (a[n] == 0 && n > 0) {
    n--;
  }
  return n+1;
}


// Compare 2 bignums a and b.
// returns 0  if a = b
// returns 1  if a > b
// returns -1 is b > a
static int bn_cmp(uint32_t *a, uint32_t alen,
                  uint32_t *b, uint32_t blen) {
  uint32_t max_len = alen > blen ? alen : blen;
  for (uint32_t i = max_len; i > 0; i --) {
    uint32_t ix = i - 1;
    uint32_t ai = ix < alen ? a[ix] : 0;
    uint32_t bi = ix < blen ? b[ix] : 0;
    if (ai > bi) {
      return 1;
    } else if (bi > ai) {
      return -1;
    }
  }
  // Only if eq we loop all the way.  
  return 0;
}

static lbm_value ext_bn_add(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = bn_normalize_len(ap, a->size / sizeof(uint32_t));
    uint32_t blen = bn_normalize_len(bp, b->size / sizeof(uint32_t));
    if (alen < blen) { // ensure alen >= blen
      uint32_t *tmp = ap; ap = bp; bp = tmp;
      uint32_t  tl  = alen; alen = blen; blen = tl;
    }
    lbm_value res;
    if (lbm_heap_allocate_array(&res, (alen + 1) * sizeof(uint32_t))) {
      lbm_array_header_t *arr = lbm_dec_array_r(res);
      bn_add(ap, alen, bp, blen, (uint32_t*)arr->data);
      r = res;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

static lbm_value ext_bn_sub(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = bn_normalize_len(ap, a->size / sizeof(uint32_t));
    uint32_t blen = bn_normalize_len(bp, b->size / sizeof(uint32_t));
    r = ENC_SYM_EERROR;
    if (bn_cmp(ap, alen, bp, blen) >= 0) { // a >= b required
      lbm_value res;
      if (lbm_heap_allocate_array(&res, alen * sizeof(uint32_t))) {
        lbm_array_header_t *arr = lbm_dec_array_r(res);
        bn_sub(ap, alen, bp, blen, (uint32_t*)arr->data);
        r = res;
      } else {
        r = ENC_SYM_MERROR;
      }
    }
  }
  return r;
}

static lbm_value ext_bn_mul(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = bn_normalize_len(ap, a->size / sizeof(uint32_t));
    uint32_t blen = bn_normalize_len(bp, b->size / sizeof(uint32_t));
    lbm_value res;
    if (lbm_heap_allocate_array(&res, (alen + blen) * sizeof(uint32_t))) {
      lbm_array_header_t *arr = lbm_dec_array_r(res);
      bn_mul(ap, alen, bp, blen, (uint32_t*)arr->data);
      r = res;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

// Convert bignum to u32. Returns EERROR if bignum is too large to fit.
static lbm_value ext_bn_to_u32(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *arr = lbm_dec_array_r(args[0]);
    uint32_t *ap = (uint32_t*)arr->data;
    uint32_t alen = bn_normalize_len(ap, arr->size / sizeof(uint32_t));
    if (alen == 1) {
      r = lbm_enc_u32(ap[0]);
    } else {
      r = ENC_SYM_EERROR;
    }
  }
  return r;
}

//convert u32 to bignum
static lbm_value ext_bn_from_u32(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_value bn;
    if (lbm_heap_allocate_array(&bn, sizeof(uint32_t))) {
      // dec_array_r is a bit misleading as we are writing.
      // But it is ok and the _r variant should be slightly cheaper.
      // We know it is writable because we just allocated it!
      lbm_array_header_t *arr = lbm_dec_array_r(bn);
      uint32_t *vp = (uint32_t*)&arr->data[0];
      *vp = lbm_dec_as_u32(args[0]);
      r = bn;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

static lbm_value ext_bn_from_bytes(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *byte_array = lbm_dec_array_r(args[0]);
    r = ENC_SYM_EERROR;
    if (byte_array->size % sizeof(uint32_t) == 0) {
      uint32_t num_bytes = byte_array->size;
      lbm_value bn ;
      if (lbm_heap_allocate_array(&bn, num_bytes)) {
        lbm_array_header_t *arr = lbm_dec_array_r(bn);
        for (uint32_t i = 0; i < num_bytes ; i ++) {
          ((uint8_t*)arr->data)[i] = ((uint8_t*)byte_array->data)[num_bytes-i-1];
        }
        r = bn;
      } else {
        r = ENC_SYM_MERROR;
      }
    }
  }
  return r;
}


static lbm_value ext_bn_to_bytes(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *bn = lbm_dec_array_r(args[0]);
    uint32_t num_bytes = bn->size;
    lbm_value byte_array;
    if (lbm_heap_allocate_array(&byte_array, num_bytes)) {
      lbm_array_header_t *arr = lbm_dec_array_r(byte_array);
      for (uint32_t i = 0; i < num_bytes; i++) {
        ((uint8_t*)arr->data)[i] = ((uint8_t*)bn->data)[num_bytes - i - 1];
      }
      r = byte_array;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

static lbm_value ext_bn_cmp(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    lbm_array_header_t *a = lbm_dec_array_r(args[0]);
    lbm_array_header_t *b = lbm_dec_array_r(args[1]);
    uint32_t *ap = (uint32_t*)a->data;
    uint32_t *bp = (uint32_t*)b->data;
    uint32_t alen = bn_normalize_len(ap, a->size / sizeof(uint32_t));
    uint32_t blen = bn_normalize_len(bp, b->size / sizeof(uint32_t));
    r = lbm_enc_i(bn_cmp(ap, alen, bp, blen));
  }
  return r;
}

void lbm_bignum_extensions_init(void) {

  lbm_add_extension("bn-cmp",        ext_bn_cmp);
  lbm_add_extension("bn-add",        ext_bn_add);
  lbm_add_extension("bn-sub",        ext_bn_sub);
  lbm_add_extension("bn-mul",        ext_bn_mul);
  lbm_add_extension("bn-to-u32",     ext_bn_to_u32);
  lbm_add_extension("bn-from-u32",   ext_bn_from_u32);
  lbm_add_extension("bn-from-bytes", ext_bn_from_bytes);
  lbm_add_extension("bn-to-bytes",   ext_bn_to_bytes);
}
