/*
    Copyright 2020 Joel Svensson	svenssonjoel@yahoo.se

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

#include <stdint.h>
#include <stdlib.h>

#include "memory.h"


uint32_t *bitmap = NULL;
uint32_t *memory = NULL; 
uint32_t memory_size;  // in 4 byte words 
uint32_t bitmap_size;  // in 4 byte words
unsigned int memory_base_address = 0;

int memory_init(unsigned char *data, uint32_t data_size,
		unsigned char *bits, uint32_t bits_size) {

  if (data == NULL || bits == NULL) return 0;
  
  if (((unsigned int)data % 4 != 0) || data_size < 1 || data_size % 4 != 0 ||
      ((unsigned int)bits % 4 != 0) || bits_size < 1 || bits_size % 4 != 0) {
    // data is not 4 byte aligned
    // size is too small
    // or size is not a multiple of 4
    return 0;
  }

  bitmap = (uint32_t *) bits;
  bitmap_size = bits_size / 4;  
  
  for (uint32_t i = 0; i < bitmap_size; i ++) {
    bitmap[i] = 0;
  }

  memory = (uint32_t *) data;
  memory_base_address = (unsigned int)data;

  return 1; 
}

static inline int bitmap_ix(uint32_t *ptr) {
  if ((unsigned int)ptr % 4 != 0) return -1;
  unsigned int tmp = (unsigned int)ptr - memory_base_address;
  unsigned int ix = tmp >> 2;
  return ix * 2;
}


uint32_t *memory_allocate(uint32_t num_words) {


}

int memory_free(uint32_t *ptr) {

}
