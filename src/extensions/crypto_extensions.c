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
#include "crypto_extensions.h"


/* ************************************************************
 * SHA256 hash
 * As explained in FIPS PUB 180-4 but with an incremental
 * padder instead of a padding preprocessing step.
 * ************************************************************/
static inline uint32_t rotr(uint32_t x, uint32_t n) {
  return (x >> n) | (x << (32 - n));
}

static inline uint32_t ch(uint32_t x, uint32_t y, uint32_t z) {
  return (x & y) ^ (~x & z);
}

static inline uint32_t maj(uint32_t x, uint32_t y, uint32_t z) {
  return (x & y) ^ (x & z) ^ (y & z);
}

static inline uint32_t sum0_256(uint32_t x) {
  return rotr(x, 2) ^ rotr(x, 13) ^ rotr(x, 22);
}

static inline uint32_t sum1_256(uint32_t x) {
  return rotr(x, 6) ^ rotr(x, 11) ^ rotr(x, 25);
}

static inline uint32_t sigma0_256(uint32_t x) {
  return rotr(x, 7) ^ rotr(x, 18) ^ (x >> 3);
}

static inline uint32_t sigma1_256(uint32_t x) {
  return rotr(x, 17) ^ rotr(x, 19) ^ (x >> 10);
}

static const uint32_t k256[64] = {
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
  0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
  0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
  0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
  0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
  0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

static const uint32_t h256[8] = {
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
  0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
};


static uint32_t read_m(uint8_t *bytes, uint32_t n, uint32_t block, uint32_t word) {
  // This does the M^(i)_t accesses on the fly by assembling a
  // big endian uint32_t from elements of the array.
  //
  // If the requested block and word contains the byte after the last byte of data,
  // the 0b10000000 is inserted into the result word.
  //
  // If the requested word is between the end of the data and the nearest multiple
  // of 512-64 bits, the output contains zeroes.
  //
  // If the block and word accessed is the last or second to last
  // the appropriate part of the size is appended.

  uint64_t size_in_bits = n * 8; // size of payload in bits.

  //                                size 
  //                           1-bit
  //                       size
  uint32_t padded_len  = (((n + 1) + 8) + 63) & ~63u;
  uint32_t size_start  = padded_len - 8;
  
  uint32_t start_byte = block * 64 + word * 4;
  uint32_t end_byte   = start_byte + 4;

  uint32_t res = 0; // prepadded with zeroes. 

  for (uint32_t i = start_byte; i < end_byte; i ++) {
    res = res << 8;
    // assemble a big endian result 
    
    if (i < n) {
      res |= bytes[i];
    } else if (i == n) {
      res |= (1 << 7); // 0b10000000;
    } else if (i >= size_start && i < padded_len) {
      uint32_t byte = i - size_start;
      res |= (uint8_t)(size_in_bits >> ((7 - byte) * 8));
    }
    // else leave as zeroes.
  }
  return res;           
}

// res is a 8*4 byte array preallocated before passed to this function. 
static void incremental_sha256(uint8_t *res, uint8_t *bytes, uint32_t n) {

  uint32_t padded_len  = (((n + 1) + 8) + 63) & ~63u;
  uint32_t blocks = padded_len / 64;  // at least 1.

  uint32_t w[64];
  uint32_t hash[8];
  memcpy(hash, h256, 8 * sizeof(uint32_t));
  
  for (uint32_t i = 0; i < blocks; i ++) {

    for (uint32_t t = 0; t < 64; t ++) {
      if (t <= 15) {
        w[t] = read_m(bytes, n, i, t);
      } else {
        w[t] = sigma1_256(w[t-2]) + w[t-7] + sigma0_256(w[t-15]) + w[t-16];
      }
    }

    uint32_t a = hash[0];
    uint32_t b = hash[1];
    uint32_t c = hash[2];
    uint32_t d = hash[3];
    uint32_t e = hash[4];
    uint32_t f = hash[5];
    uint32_t g = hash[6];
    uint32_t h = hash[7];

    for (uint32_t t  = 0; t < 64; t++) {
      uint32_t t1 = h + sum1_256(e) + ch(e,f,g)+k256[t]+w[t];
      uint32_t t2 = sum0_256(a) + maj(a,b,c);
      h = g;
      g = f;
      f = e;
      e = d + t1;
      d = c;
      c = b;
      b = a;
      a = t1 + t2;
    }
    hash[0] = a + hash[0];
    hash[1] = b + hash[1];
    hash[2] = c + hash[2];
    hash[3] = d + hash[3];
    hash[4] = e + hash[4];
    hash[5] = f + hash[5];
    hash[6] = g + hash[6];
    hash[7] = h + hash[7];
  }

  for (uint32_t t = 0; t < 32; t ++) {

    uint32_t word = t / 4;
    uint32_t byte = t % 4;
    
    res[t] = (uint8_t)(hash[word] >> ((3 - byte) * 8));
  }
}


static lbm_value ext_sha256_str(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) { 
    if(lbm_create_array(&res, 32)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
      char *str = lbm_dec_str(args[0]);
      uint32_t n = strlen_max(str, arr->size);
      incremental_sha256((uint8_t*)arr->data, (uint8_t*)str, n);
    }
  }
  return res;
}

static lbm_value ext_sha256(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) { 
    if(lbm_create_array(&res, 32)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
      lbm_array_header_t *in_arr = (lbm_array_header_t*)lbm_car(args[0]);
      incremental_sha256((uint8_t*)arr->data, (uint8_t*)in_arr->data, in_arr->size);
    }
  }
  return res;
}



static lbm_value ext_bytes_to_hex(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    lbm_array_header_t *in_arr = (lbm_array_header_t*)lbm_car(args[0]);
    lbm_uint hex_len = in_arr->size * 2 + 1;
    if (lbm_create_array(&res, hex_len)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
      uint8_t *in = (uint8_t*)in_arr->data;
      char *out = (char*)arr->data;
      for (lbm_uint i = 0; i < in_arr->size; i++) {
        snprintf(out + i * 2, 3, "%02x", in[i]);
      }
      out[hex_len - 1] = '\0';
    }
  }
  return res;
}

void lbm_crypto_extensions_init(void) {
  lbm_add_extension("sha256-str", ext_sha256_str);
  lbm_add_extension("sha256", ext_sha256);
  lbm_add_extension("bytes-to-hex", ext_bytes_to_hex);
}
