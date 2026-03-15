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

#include "extensions.h"
#include "lbm_c_interop.h"
#include "ecc_extensions.h"

/*
   Reed Solomon error correction codes over GF(2^8).

   From the top!
   GF(2) is the set { 0,1 } and two operations addition (xor) and multiplication (and).
   The set, and operations form a field. For a set and its add and mul operations to be 
   a field there is a requirement of there being additive and multiplicative inverses as well.

   Additive inverse: For each value a in the set  there is a unique b in the set  so that
   a + b = 0.

     examples in GF(2): 0 xor 0 = 0
                        1 xor 1 = 0

   Multiplicative inverse: For each non-zero value a in the set there is a unique b in the set so that 
   a * b = 1.

     examples in GF(2): 1 and 1 = 1


   Arbitrary length arrays of GF(2) values are represented by the GF(2)[x] syntax and is an infinite ring
   (A ring is the field's weird cousin where there is no guarantee that every element has multiplicative 
   inverse)

   GF(2)[x] / p  (quotient ring) where p is an irreducible polynomial is written as GF(2^N) where 
   N is the degree of p. What this means is that the ADD and MULT operations on the values 
   of the GF(2)[x] are performed MODULO p (as ADD is xor it will naturally not increase degree and
   needs no mod fixup). And this is a finite field.

   The p chosen should be irreducible, and for Reed Solomon algorithms the
   p should also be primitive.

   Irreducible: The polynomial cannot be factored into 2 smaller polynomials with coefficients in GF(2). 
     (Like a prime number cannot be factored into anything other than itself and 1). 

   Primitive: The root of the polynomial generates all non-zero elements of the field. 
 
   0x11d or 100011101 or x^8 + x^4 + x^3 + x^2 + 1 is a primitive irreducible polynomial in GF(2^8)

   In Sklar (Reed-Solomon codes), the construction of GF(2^N) is done
   more bottom up in a way that makes more sense to me. That alpha is a primitive root
   is by construction in how Sklar presents it.

   The decoding is incredibly complicated and I will not even try to understand it at this 
   moment. The implementation here is directly derived from the Wikiversity: 
   https://en.wikiversity.org/wiki/Reed%E2%80%93Solomon_codes_for_coders


   Sources for further study: 
    - Ross Williams: http://www.ross.net/crc/crcpaper.html 
      polynomial division as used in CRCs 
    - Sklar: https://ptgmedia.pearsoncmg.com/images/art_sklar7_reed-solomon/elementlinks/art_sklar7_reed-solomon.pdf
    - Wikiversity: https://en.wikiversity.org/wiki/Reed%E2%80%93Solomon_codes_for_coders

*/

#define RS_MAX_ROOTS 32

/* The maximum roots sets the maximum error correction-ability.  

   roots    |  byte errors  | payload size
   2        |  1            | 253 
   4        |  2            | 251
   ..       |               | 
   32       |  16           | 223

*/


/* GF(2^8) exponent table: gf_exp[i] = 2^i mod 0x11d, doubled for wrap-around */
static const uint8_t gf_exp[512] = {
  0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1d, 0x3a, 0x74, 0xe8, 0xcd, 0x87, 0x13, 0x26,
  0x4c, 0x98, 0x2d, 0x5a, 0xb4, 0x75, 0xea, 0xc9, 0x8f, 0x03, 0x06, 0x0c, 0x18, 0x30, 0x60, 0xc0,
  0x9d, 0x27, 0x4e, 0x9c, 0x25, 0x4a, 0x94, 0x35, 0x6a, 0xd4, 0xb5, 0x77, 0xee, 0xc1, 0x9f, 0x23,
  0x46, 0x8c, 0x05, 0x0a, 0x14, 0x28, 0x50, 0xa0, 0x5d, 0xba, 0x69, 0xd2, 0xb9, 0x6f, 0xde, 0xa1,
  0x5f, 0xbe, 0x61, 0xc2, 0x99, 0x2f, 0x5e, 0xbc, 0x65, 0xca, 0x89, 0x0f, 0x1e, 0x3c, 0x78, 0xf0,
  0xfd, 0xe7, 0xd3, 0xbb, 0x6b, 0xd6, 0xb1, 0x7f, 0xfe, 0xe1, 0xdf, 0xa3, 0x5b, 0xb6, 0x71, 0xe2,
  0xd9, 0xaf, 0x43, 0x86, 0x11, 0x22, 0x44, 0x88, 0x0d, 0x1a, 0x34, 0x68, 0xd0, 0xbd, 0x67, 0xce,
  0x81, 0x1f, 0x3e, 0x7c, 0xf8, 0xed, 0xc7, 0x93, 0x3b, 0x76, 0xec, 0xc5, 0x97, 0x33, 0x66, 0xcc,
  0x85, 0x17, 0x2e, 0x5c, 0xb8, 0x6d, 0xda, 0xa9, 0x4f, 0x9e, 0x21, 0x42, 0x84, 0x15, 0x2a, 0x54,
  0xa8, 0x4d, 0x9a, 0x29, 0x52, 0xa4, 0x55, 0xaa, 0x49, 0x92, 0x39, 0x72, 0xe4, 0xd5, 0xb7, 0x73,
  0xe6, 0xd1, 0xbf, 0x63, 0xc6, 0x91, 0x3f, 0x7e, 0xfc, 0xe5, 0xd7, 0xb3, 0x7b, 0xf6, 0xf1, 0xff,
  0xe3, 0xdb, 0xab, 0x4b, 0x96, 0x31, 0x62, 0xc4, 0x95, 0x37, 0x6e, 0xdc, 0xa5, 0x57, 0xae, 0x41,
  0x82, 0x19, 0x32, 0x64, 0xc8, 0x8d, 0x07, 0x0e, 0x1c, 0x38, 0x70, 0xe0, 0xdd, 0xa7, 0x53, 0xa6,
  0x51, 0xa2, 0x59, 0xb2, 0x79, 0xf2, 0xf9, 0xef, 0xc3, 0x9b, 0x2b, 0x56, 0xac, 0x45, 0x8a, 0x09,
  0x12, 0x24, 0x48, 0x90, 0x3d, 0x7a, 0xf4, 0xf5, 0xf7, 0xf3, 0xfb, 0xeb, 0xcb, 0x8b, 0x0b, 0x16,
  0x2c, 0x58, 0xb0, 0x7d, 0xfa, 0xe9, 0xcf, 0x83, 0x1b, 0x36, 0x6c, 0xd8, 0xad, 0x47, 0x8e, 0x01,
  0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1d, 0x3a, 0x74, 0xe8, 0xcd, 0x87, 0x13, 0x26, 0x4c,
  0x98, 0x2d, 0x5a, 0xb4, 0x75, 0xea, 0xc9, 0x8f, 0x03, 0x06, 0x0c, 0x18, 0x30, 0x60, 0xc0, 0x9d,
  0x27, 0x4e, 0x9c, 0x25, 0x4a, 0x94, 0x35, 0x6a, 0xd4, 0xb5, 0x77, 0xee, 0xc1, 0x9f, 0x23, 0x46,
  0x8c, 0x05, 0x0a, 0x14, 0x28, 0x50, 0xa0, 0x5d, 0xba, 0x69, 0xd2, 0xb9, 0x6f, 0xde, 0xa1, 0x5f,
  0xbe, 0x61, 0xc2, 0x99, 0x2f, 0x5e, 0xbc, 0x65, 0xca, 0x89, 0x0f, 0x1e, 0x3c, 0x78, 0xf0, 0xfd,
  0xe7, 0xd3, 0xbb, 0x6b, 0xd6, 0xb1, 0x7f, 0xfe, 0xe1, 0xdf, 0xa3, 0x5b, 0xb6, 0x71, 0xe2, 0xd9,
  0xaf, 0x43, 0x86, 0x11, 0x22, 0x44, 0x88, 0x0d, 0x1a, 0x34, 0x68, 0xd0, 0xbd, 0x67, 0xce, 0x81,
  0x1f, 0x3e, 0x7c, 0xf8, 0xed, 0xc7, 0x93, 0x3b, 0x76, 0xec, 0xc5, 0x97, 0x33, 0x66, 0xcc, 0x85,
  0x17, 0x2e, 0x5c, 0xb8, 0x6d, 0xda, 0xa9, 0x4f, 0x9e, 0x21, 0x42, 0x84, 0x15, 0x2a, 0x54, 0xa8,
  0x4d, 0x9a, 0x29, 0x52, 0xa4, 0x55, 0xaa, 0x49, 0x92, 0x39, 0x72, 0xe4, 0xd5, 0xb7, 0x73, 0xe6,
  0xd1, 0xbf, 0x63, 0xc6, 0x91, 0x3f, 0x7e, 0xfc, 0xe5, 0xd7, 0xb3, 0x7b, 0xf6, 0xf1, 0xff, 0xe3,
  0xdb, 0xab, 0x4b, 0x96, 0x31, 0x62, 0xc4, 0x95, 0x37, 0x6e, 0xdc, 0xa5, 0x57, 0xae, 0x41, 0x82,
  0x19, 0x32, 0x64, 0xc8, 0x8d, 0x07, 0x0e, 0x1c, 0x38, 0x70, 0xe0, 0xdd, 0xa7, 0x53, 0xa6, 0x51,
  0xa2, 0x59, 0xb2, 0x79, 0xf2, 0xf9, 0xef, 0xc3, 0x9b, 0x2b, 0x56, 0xac, 0x45, 0x8a, 0x09, 0x12,
  0x24, 0x48, 0x90, 0x3d, 0x7a, 0xf4, 0xf5, 0xf7, 0xf3, 0xfb, 0xeb, 0xcb, 0x8b, 0x0b, 0x16, 0x2c,
  0x58, 0xb0, 0x7d, 0xfa, 0xe9, 0xcf, 0x83, 0x1b, 0x36, 0x6c, 0xd8, 0xad, 0x47, 0x8e, 0x01, 0x02,
};

/* GF(2^8) logarithm table: gf_log[x] = i such that 2^i = x  (gf_log[0] undefined, set 0) */
static const uint8_t gf_log[256] = {
  0x00, 0x00, 0x01, 0x19, 0x02, 0x32, 0x1a, 0xc6, 0x03, 0xdf, 0x33, 0xee, 0x1b, 0x68, 0xc7, 0x4b,
  0x04, 0x64, 0xe0, 0x0e, 0x34, 0x8d, 0xef, 0x81, 0x1c, 0xc1, 0x69, 0xf8, 0xc8, 0x08, 0x4c, 0x71,
  0x05, 0x8a, 0x65, 0x2f, 0xe1, 0x24, 0x0f, 0x21, 0x35, 0x93, 0x8e, 0xda, 0xf0, 0x12, 0x82, 0x45,
  0x1d, 0xb5, 0xc2, 0x7d, 0x6a, 0x27, 0xf9, 0xb9, 0xc9, 0x9a, 0x09, 0x78, 0x4d, 0xe4, 0x72, 0xa6,
  0x06, 0xbf, 0x8b, 0x62, 0x66, 0xdd, 0x30, 0xfd, 0xe2, 0x98, 0x25, 0xb3, 0x10, 0x91, 0x22, 0x88,
  0x36, 0xd0, 0x94, 0xce, 0x8f, 0x96, 0xdb, 0xbd, 0xf1, 0xd2, 0x13, 0x5c, 0x83, 0x38, 0x46, 0x40,
  0x1e, 0x42, 0xb6, 0xa3, 0xc3, 0x48, 0x7e, 0x6e, 0x6b, 0x3a, 0x28, 0x54, 0xfa, 0x85, 0xba, 0x3d,
  0xca, 0x5e, 0x9b, 0x9f, 0x0a, 0x15, 0x79, 0x2b, 0x4e, 0xd4, 0xe5, 0xac, 0x73, 0xf3, 0xa7, 0x57,
  0x07, 0x70, 0xc0, 0xf7, 0x8c, 0x80, 0x63, 0x0d, 0x67, 0x4a, 0xde, 0xed, 0x31, 0xc5, 0xfe, 0x18,
  0xe3, 0xa5, 0x99, 0x77, 0x26, 0xb8, 0xb4, 0x7c, 0x11, 0x44, 0x92, 0xd9, 0x23, 0x20, 0x89, 0x2e,
  0x37, 0x3f, 0xd1, 0x5b, 0x95, 0xbc, 0xcf, 0xcd, 0x90, 0x87, 0x97, 0xb2, 0xdc, 0xfc, 0xbe, 0x61,
  0xf2, 0x56, 0xd3, 0xab, 0x14, 0x2a, 0x5d, 0x9e, 0x84, 0x3c, 0x39, 0x53, 0x47, 0x6d, 0x41, 0xa2,
  0x1f, 0x2d, 0x43, 0xd8, 0xb7, 0x7b, 0xa4, 0x76, 0xc4, 0x17, 0x49, 0xec, 0x7f, 0x0c, 0x6f, 0xf6,
  0x6c, 0xa1, 0x3b, 0x52, 0x29, 0x9d, 0x55, 0xaa, 0xfb, 0x60, 0x86, 0xb1, 0xbb, 0xcc, 0x3e, 0x5a,
  0xcb, 0x59, 0x5f, 0xb0, 0x9c, 0xa9, 0xa0, 0x51, 0x0b, 0xf5, 0x16, 0xeb, 0x7a, 0x75, 0x2c, 0xd7,
  0x4f, 0xae, 0xd5, 0xe9, 0xe6, 0xe7, 0xad, 0xe8, 0x74, 0xd6, 0xf4, 0xea, 0xa8, 0x50, 0x58, 0xaf,
};

// Addition in GF(2^8) is XOR
static inline uint8_t gf_add(uint8_t a, uint8_t b) {
  return a ^ b;
}

// Multiplication in GF(2^8):
//   a * b = alpha^(log(a) + log(b))
//   alpha is the primitive root and it has been "baked" into the lookup tables.
//   The table is also 2x as big
//   to avoid the mod operation needed on indices (in some cases).
//   0 <= log(a) <= 254 
//   0 <= log(b) <= 254
//   0 <= log(a) + log(b) <= 508 (this is where the increased table size comes from).
static inline uint8_t gf_mul(uint8_t a, uint8_t b) {
  if (a == 0 || b == 0) return 0;
  return gf_exp[gf_log[a] + gf_log[b]];
}

// Multiplicative inverse in GF(2^8):
// a^(-1) = alpha^(255 - log(a)),  since a * a^(-1) = alpha^255 = 1
//
// Think of this as the exponents are subjected to arithmetic mod 255
// but it is all baked into the table. 
// Note that 0 has no inverse.
static inline uint8_t gf_inv(uint8_t a) {
  return gf_exp[255 - gf_log[a]];
}

// Total buffer size needed for an RS encoded message.
// payload_size + nroots must be at most 255 total (GF(2^8) codeword size limit).
// nroots must be even. 1 byte can be fixed for every 2 added to nroots.
static inline int rs_encoded_size(int payload_size, int nroots) {
  return payload_size + nroots;
}

// The lisp programmer can use this function to compute how much buffer
// to allocate for a given payload size:
// example:  (define mybuf (bufcreate (rs-encoded-size n 4)))
static lbm_value ext_rs_encoded_size(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1])) {
    int payload_size = (int)lbm_dec_as_i32(args[0]);
    int nroots       = (int)lbm_dec_as_i32(args[1]);
    if (payload_size <= 0 || nroots < 2 || nroots % 2 != 0 ||
        payload_size + nroots > 255) {
      res = ENC_SYM_EERROR;
    } else {
      res = lbm_enc_i(rs_encoded_size(payload_size, nroots));
    }
  }
  return res;
}


// Creates the polynomial g(x) with roots alpha, alpha^2 .. alpha^nroots.
// Could be preallocated as these are deterministic per nroots value
static void rs_gen_poly(int nroots, uint8_t *gen) {
  memset(gen, 0, (size_t)(nroots + 1));
  gen[0] = 1;
  for (int i = 0; i < nroots; i++) {
    uint8_t root = gf_exp[i + 1];
    for (int j = i + 1; j >= 1; j--) {
      gen[j] ^= gf_mul(root, gen[j - 1]);
    }
  }
}

// Encode using a linear feedback shift register (LFSR).
//
// The parity register is nroots bytes long.  For each payload byte:
//   feedback = payload_byte XOR parity[0]   (the "output" tap)
//   shift the register left by one (parity[0] <- parity[1], ..., parity[nroots-1] <- 0)
//   XOR feedback * gen[j+1] into parity[j] for j=0..nroots-1
//
// gen[1..nroots] are the non-leading coefficients of the generator polynomial
// (gen[0] = 1 is the leading term and is implicit in the division).
//
// This approach only ever writes to the parity region, so data[0..payload_len-1]
// (the payload) is left completely untouched.
//
// See: Wikiversity Reed-Solomon codes for coders, optimised encoder.
//      http://www.ross.net/crc/crcpaper.html
static void rs_encode(uint8_t *data, int payload_len, int nroots) {
  uint8_t gen[RS_MAX_ROOTS + 1];
  rs_gen_poly(nroots, gen);
  uint8_t *parity = data + payload_len;
  memset(parity, 0, (size_t)nroots);
  for (int i = 0; i < payload_len; i++) {
    uint8_t feedback = data[i] ^ parity[0];
    memmove(parity, parity + 1, (size_t)(nroots - 1));
    parity[nroots - 1] = 0;
    if (feedback != 0) {
      for (int j = 0; j < nroots; j++) {
        parity[j] ^= gf_mul(gen[j + 1], feedback);
      }
    }
  }
}

// rs-encode takes a buffer containing payload data as well as enough
// free space for the error correcting residue to be stored after the payload.
// rs-encode updates the buffer in place but leaves the payload unchanged.
//
// (rs-encode data 16)
//
// here data needs to be (payload-size + 16) <= 255
static lbm_value ext_rs_encode(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_number(args[1])) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(args[0]);
    int nroots      = (int)lbm_dec_as_i32(args[1]);
    int total       = (int)arr->size;
    int payload_len = total - nroots;
    if (payload_len <= 0 || nroots < 2 || nroots > RS_MAX_ROOTS || nroots % 2 != 0 || total > 255) {
      res = ENC_SYM_EERROR;
    } else {
      rs_encode((uint8_t*)arr->data, payload_len, nroots);
      res = args[0];
    }
  }
  return res;
}



// Evaluate the received codeword at each root of the generator polynomial.
// Horners Method of polynomial evaluation is used to avoid the need
// to explicitly evaluate any powers.
//
// The usage of the word syndrome, comes from this function computing
// an array of error (sickness) indicators. If the syndromes array are all zero
// there are no errors.
static int rs_syndromes(uint8_t *data, int total, int nroots, uint8_t *s) {
  int any_errs = 0;
  for (int i = 0; i < nroots; i++) {
    uint8_t val  = data[0];
    uint8_t root = gf_exp[i + 1];
    for (int j = 1; j < total; j++) {
      val = gf_mul(val, root) ^ data[j];
    }
    s[i] = val;
    if (val != 0) any_errs = 1;
  }
  return any_errs;
}

// Find the error locator polynomial sigma(x) using the Berlekamp-Massey
// algorithm.  The degree L of sigma equals the number of errors.
//
// sigma  = current error locator polynomial, starts as 1.
// B      = auxiliary update polynomial (previous sigma, shifted by x each step).
// d_prev = previous non-zero discrepancy.
//
// At each step n the discrepancy d measures how well sigma predicts s[n]
// from prior syndromes.  When d != 0 we correct sigma using B.  When the
// LFSR length must grow (2*L <= n) we also swap B <- old sigma and update L.
//
// Returns L (degree of sigma, = number of errors), or -1 if uncorrectable.
static int rs_berlekamp_massey(uint8_t *s, int nroots, uint8_t *sigma) {
  uint8_t B[RS_MAX_ROOTS + 1];
  memset(sigma, 0, (size_t)(nroots + 1));
  memset(B,     0, (size_t)(nroots + 1));
  sigma[0] = 1;
  B[0]     = 1;
  int     L      = 0;
  uint8_t d_prev = 1;

  for (int n = 0; n < nroots; n++) {
    // Discrepancy: d = s[n] + sum_{i=1}^{L} sigma[i]*s[n-i]
    uint8_t d = s[n];
    for (int i = 1; i <= L; i++) {
      d ^= gf_mul(sigma[i], s[n - i]);
    }
    // Shift B by one position (multiply polynomial by x)
    memmove(B + 1, B, (size_t)nroots);
    B[0] = 0;
    if (d == 0) continue;
    // sigma_new = sigma - (d/d_prev)*B
    uint8_t T[RS_MAX_ROOTS + 1];
    memcpy(T, sigma, (size_t)(nroots + 1));
    uint8_t coef = gf_mul(d, gf_inv(d_prev));
    for (int i = 0; i <= nroots; i++) {
      sigma[i] ^= gf_mul(coef, B[i]);
    }
    if (2 * L <= n) {
      // LFSR length must grow: swap B <- old sigma, update L and d_prev
      L = n + 1 - L;
      memcpy(B, T, (size_t)(nroots + 1));
      d_prev = d;
    }
  }

  return (L > nroots / 2) ? -1 : L;
}

// Find error positions by evaluating sigma at alpha^{-k} for k=0..total-1.
// gf_exp[255] = 1 = alpha^0, so gf_exp[255-k] = alpha^{-k} for all k in
// [0, 254] (and k=0 gives gf_exp[255] = 1 correctly).
// A root at k means an error at buffer position total-1-k.
//
// Fills err_pos[] and err_xi_inv[] (alpha^{-k} saved for Forney later).
// Returns the number of roots found, or -1 if the buffer would overflow.
static int rs_chien_search(uint8_t *sigma, int L, int total,
                           uint8_t *err_pos, uint8_t *err_xi_inv) {
  int err_count = 0;
  for (int k = 0; k < total; k++) {
    uint8_t xi_inv = gf_exp[255 - k]; // alpha^{-k}
    // Evaluate sigma(xi_inv) via Horner from high to low degree
    uint8_t val = sigma[L];
    for (int i = L - 1; i >= 0; i--) {
      val = gf_mul(val, xi_inv) ^ sigma[i];
    }
    if (val == 0) {
      if (err_count >= RS_MAX_ROOTS / 2) return -1; /* safety */
      err_pos[err_count]    = (uint8_t)(total - 1 - k);
      err_xi_inv[err_count] = xi_inv;
      err_count++;
    }
  }
  return err_count;
}

// Compute the error evaluator polynomial Omega(x) = S(x)*sigma(x) mod x^nroots.
// S(x) = s[0] + s[1]*x + ... + s[nroots-1]*x^{nroots-1}.
// Omega[k] = sum_{j=0}^{min(k,L)} sigma[j] * s[k-j]  for k=0..nroots-1.
static void rs_error_evaluator(uint8_t *sigma, int L,
                               uint8_t *s, int nroots, uint8_t *omega) {
  memset(omega, 0, (size_t)nroots);
  for (int k = 0; k < nroots; k++) {
    int jmax = (k < L) ? k : L;
    for (int j = 0; j <= jmax; j++) {
      omega[k] ^= gf_mul(sigma[j], s[k - j]);
    }
  }
}

// Apply the Forney algorithm to compute and correct each error magnitude.
// With FCR=1 the formula simplifies to:
//   error_magnitude = Omega(xi_inv) / sigma'(xi_inv)
//
// sigma'(x) is the formal derivative of sigma.  In characteristic 2,
// even-degree terms vanish (2 = 0), leaving only odd-indexed coefficients:
//   sigma'(x) = sigma[1] + sigma[3]*x^2 + sigma[5]*x^4 + ...
//
// XOR the magnitude into data[pos] to correct the error byte.
// Returns 0 on success, -1 if a zero denominator is encountered.
static int rs_forney(uint8_t *data, int err_count,
                     uint8_t *err_pos, uint8_t *err_xi_inv,
                     uint8_t *sigma, int L, uint8_t *omega, int nroots) {
  for (int i = 0; i < err_count; i++) {
    int     pos    = err_pos[i];
    uint8_t xi_inv = err_xi_inv[i];

    // Numerator: Omega(xi_inv) via Horner from high to low
    uint8_t num = omega[nroots - 1];
    for (int j = nroots - 2; j >= 0; j--) {
      num = gf_mul(num, xi_inv) ^ omega[j];
    }

    // Denominator: sigma'(xi_inv) — accumulate odd-indexed terms
    uint8_t den     = 0;
    uint8_t xi_inv2 = gf_mul(xi_inv, xi_inv); // xi_inv^2 
    uint8_t xi_pow  = 1;                      // xi_inv^{j-1} for j=1,3,5,...
    for (int j = 1; j <= L; j += 2) {
      den    ^= gf_mul(sigma[j], xi_pow);
      xi_pow  = gf_mul(xi_pow, xi_inv2);
    }

    if (den == 0) return -1; // degenerate, shouldn't occur for valid RS
    data[pos] ^= gf_mul(num, gf_inv(den));
  }
  return 0;
}

// Decode a Reed-Solomon codeword in-place.
// Returns the number of errors corrected (>= 0), or -1 if uncorrectable.
static int rs_decode(uint8_t *data, int payload_len, int nroots) {
  int total = payload_len + nroots;

  uint8_t s[RS_MAX_ROOTS];
  if (!rs_syndromes(data, total, nroots, s)) return 0;

  uint8_t sigma[RS_MAX_ROOTS + 1];
  int L = rs_berlekamp_massey(s, nroots, sigma);
  if (L < 0) return -1;

  uint8_t err_pos[RS_MAX_ROOTS / 2];
  uint8_t err_xi_inv[RS_MAX_ROOTS / 2];
  int err_count = rs_chien_search(sigma, L, total, err_pos, err_xi_inv);
  if (err_count != L) return -1;

  uint8_t omega[RS_MAX_ROOTS];
  rs_error_evaluator(sigma, L, s, nroots, omega);

  if (rs_forney(data, err_count, err_pos, err_xi_inv, sigma, L, omega, nroots) < 0) return -1;

  return err_count;
}

// (rs-decode data nroots) -> number of corrected errors, or -1
//
// data must be a buffer of size (payload + nroots) containing a received
// RS-encoded message (same layout produced by rs-encode).
// The buffer is corrected in-place.
static lbm_value ext_rs_decode(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_number(args[1])) {
    lbm_array_header_t *arr     = (lbm_array_header_t*)lbm_car(args[0]);
    int                 nroots  = (int)lbm_dec_as_i32(args[1]);
    int                 total   = (int)arr->size;
    int                 payload = total - nroots;
    if (payload <= 0 || nroots < 2 || nroots > RS_MAX_ROOTS || nroots % 2 != 0 || total > 255) {
      res = ENC_SYM_EERROR;
    } else {
      int corrected = rs_decode((uint8_t*)arr->data, payload, nroots);
      // Will be a negative number if the data cannot be corrected.
      res = lbm_enc_i(corrected);
    }
  }
  return res;
}

void lbm_ecc_extensions_init(void) {
  lbm_add_extension("rs-encode",       ext_rs_encode);
  lbm_add_extension("rs-decode",       ext_rs_decode);
  lbm_add_extension("rs-encoded-size", ext_rs_encoded_size);
}
