/*
    Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

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

#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#include <stdio.h>

#define  KEY  0
#define  CODE 1

/* The codes are generated using python script in utils directory */
// Total number of bits 392
#define NUM_CODES 66
char *codes[NUM_CODES][2] =
  {{ "lambda", "000000" },
   { "z", "001110" },
   { "x", "011111" },
   { "y", "001111" },
   { "v", "100000" },
   { "w", "010100" },
   { "t", "011100" },
   { "u", "100001" },
   { "r", "011001" },
   { "s", "100100" },
   { "p", "100110" },
   { "q", "100010" },
   { "n", "010110" },
   { "o", "001101" },
   { "l", "011110" },
   { "m", "010000" },
   { "j", "010011" },
   { "k", "010101" },
   { "h", "100011" },
   { "i", "011000" },
   { "f", "010001" },
   { "g", "011011" },
   { "d", "100101" },
   { "e", "010010" },
   { "b", "011101" },
   { "c", "011010" },
   { "car", "001001" },
   { "a", "010111" },
   { "8", "101000" },
   { "let", "000101" },
   { "((", "1111110" },
   { ">", "110111" },
   { "\\", "110010" },
   { "quote", "1111101" },
   { "nil", "000001" },
   { "closure", "1111100" },
   { "define", "001100" },
   { "<", "111000" },
   { "=", "111101" },
   { "5", "101100" },
   { "9", "101101" },
   { "6", "100111" },
   { "7", "101001" },
   { "4", "101010" },
   { "\"", "110011" },
   { "2", "101011" },
   { "3", "110000" },
   { "0", "101110" },
   { "))))", "001010" },
   { ".", "110110" },
   { "/", "111001" },
   { "-", "110101" },
   { "*", "110100" },
   { "+", "111010" },
   { "(", "1111111" },
   { ")", "000011" },
   { "'", "111011" },
   { "#", "111100" },
   { " ", "110001" },
   { "if", "000111" },
   { ")))", "001011" },
   { "1", "101111" },
   { "cdr", "001000" },
   { "))", "000010" },
   { "list", "000110" },
   { "cons", "000100" }};

int length_max_compressible() {
  int max = 0;
  for (int i = 0; i < NUM_CODES; i ++) {
    int n = strlen(codes[i][0]);
    if (n > max) max = n;
  }
  return max;
}

int length_max_decompressible() {
  int max = 0;
  for (int i = 0; i < NUM_CODES; i ++) {
    int n = strlen(codes[i][1]);
    if (n > max) max = n;
  }
  return max;
}

int match_longest(int which, char *string) {

  int longest_match_ix = -1;
  int longest_match_length = 0;
  int n = strlen(string);

  for (int i = 0; i < NUM_CODES; i ++) {
    int s_len = strlen(codes[i][which]);
    if (s_len <= n) {
      if (strncmp(codes[i][which], string, s_len) == 0) {
	if (s_len > longest_match_length) {
	  longest_match_ix = i;
	  longest_match_length = s_len;
	}
      }
    }
  }
  return longest_match_ix;
}

int compressed_length(char *string) {
  int i = 0;

  int n = strlen(string);
  int comp_len = 0; // in bits

  bool string_mode = false;
  bool gobbling_whitespace = false;

  while (i < n) {
    if (string_mode) {
      if (string[i] == '\"') {
	string_mode = false;
      } else {
	comp_len += 8;
	i++;
      }

    } else {

      if (!string_mode) {
	if ( string[i] == '\n' ||
	     string[i] == ' '  ||
	     string[i] == '\t' ||
	     string[i] == '\r') {
	  gobbling_whitespace = true;
	  *(string + i) = ' ';
	  i ++;
	  continue;
	} else if (gobbling_whitespace) {
	  gobbling_whitespace = false;
	  i--;
	}
      }

      if (string[i] == '\"') string_mode = true;

      int ix = match_longest(KEY,string + i);

      if (ix == -1) return -1;

      int code_len = strlen(codes[ix][1]);
      comp_len += code_len;
      i += strlen(codes[ix][0]);
    }
  }
  return comp_len;
}

void set_bit(char *c, char bit_pos, bool set) {
  char bval = 1 << bit_pos;
  if (set) {
    *c = *c | bval;
  } else {
    *c = *c & ~bval;
  }
}

void emit_string_char_code(char *compressed, char c, int *bit_pos) {

  for (int i = 0; i < 8; i ++) {
    int byte_ix = (*bit_pos) / 8;
    int bit_ix  = (*bit_pos) % 8;
    bool s = (c & (1 << i));
    set_bit(&compressed[byte_ix], bit_ix, s);
    *bit_pos = *bit_pos + 1;
  }
}

void emit_code(char *compressed, char *code, int *bit_pos) {
  int n = strlen(code);

  for (int i = 0; i < n; i ++) {
    int byte_ix = (*bit_pos) / 8;
    int bit_ix  = (*bit_pos) % 8;
    bool s = (code[i] == '1');
    set_bit(&compressed[byte_ix], bit_ix, s);
    *bit_pos = *bit_pos + 1;
  }
}

char read_character(char *src, int *bit_pos) {

  char c = 0;

  for (int i = 0; i < 8; i ++) {
    int byte_ix = (*bit_pos)/8;
    int bit_ix  = (*bit_pos)%8;
    bool s = src[byte_ix] & (1 << bit_ix);
    set_bit(&c, i, s);
    *bit_pos = *bit_pos + 1;
  }
  return c;
}

char *compress(char *string) {

  uint32_t c_size_bits = compressed_length(string);
  uint32_t c_size_bytes = 4 + (c_size_bits/8+1);

  uint32_t header_value = c_size_bits;

  if (header_value == 0) return NULL;

  printf("header_value: %u\n", header_value);

  char *compressed = malloc(c_size_bytes);
  if (!compressed) return NULL;
  memset(compressed, 0, c_size_bytes);
  int bit_pos = 0;

  compressed[0] = (unsigned char)header_value;
  compressed[1] = (unsigned char)(header_value >> 8);
  compressed[2] = (unsigned char)(header_value >> 16);
  compressed[3] = (unsigned char)(header_value >> 24);
  bit_pos = 32;

  bool string_mode = false;
  bool gobbling_whitespace = false;
  uint32_t n = strlen(string);
  uint32_t i = 0;

  while (i < n) {
    if (string_mode) {

      if (string[i] == '\"' &&
	  !(string[i-1] == '\\')) {
	emit_string_char_code(compressed, '\"', &bit_pos);
	i ++;
	string_mode = false;
	continue;
      } else {
	emit_string_char_code(compressed, string[i], &bit_pos);
	i++;
      }

    } else {
      if (!string_mode) {
	if ( string[i] == '\n' ||
	     string[i] == ' '  ||
	     string[i] == '\t' ||
	     string[i] == '\r') {
	  gobbling_whitespace = true;
	  *(string + i) = ' ';
	  i ++;
	  continue;
	} else if (gobbling_whitespace) {
	  gobbling_whitespace = false;
	  i--;
	}
      }

      /* Compress string-starting " character */
      if (string[i] == '\"') {
	string_mode = true;
      }
      int ix = match_longest(KEY,&string[i]);

      if (ix == -1) return NULL;
      emit_code(compressed, codes[ix][1], &bit_pos);

      i += strlen(codes[ix][0]);
    }
  }

  for (i = 0; i < c_size_bytes; i ++) {
    printf("%x ",(unsigned char)compressed[i]);
  }
  return compressed;
}


bool decompress(char *dest, uint32_t dest_n, char *src) {

  bool ret = true;
  bool string_mode = false;
  uint32_t compressed_bits = 0;
  uint32_t i = 0;
  uint32_t char_pos = 0;

  memcpy(&compressed_bits, src, 4);
  memset(dest, 0, dest_n);

  printf("decompress -- compressed_bits = %u\n",compressed_bits);

  while (i < compressed_bits && char_pos < dest_n-1) {

    if (string_mode) {

    }

    int ix = match_longest(CODE,src);

    if( strlen(codes[ix][KEY]) == 1 &&
	strncmp(codes[ix][KEY], "\"", 1) == 0) {
      string_mode = true;
    }


  }
  return ret;
}
