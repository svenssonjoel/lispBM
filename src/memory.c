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

#include "memory.h"


unsigned char *bitmap = NULL;
unsigned char *memory = NULL; 
unsigned int memory_size;  // in 4 byte words 
unsigned int bitmap_size;  // in bits

int memory_init(unsigned char *data, uint32_t size) {

  if (((unsigned int)data % 4 != 0) || size < 1 || size % 4 != 0) {
    // data is not 4 byte aligned
    // size is too small
    // or size is not a multiple of 4
    return 0;
  }  
  return 1; 
}
