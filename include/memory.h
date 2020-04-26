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

/* 
 * Memory manager for allocation of strings and arrays that will not be   
 * be located on the lisp-heap, but rather on the traditional heap ;) 
 * 
 * Later perhaps things such as the symbol table with symbol mappings
 * should also be located on this managed memory area.  Symbols,
 * however, are never freed after being created in lispBM. Currently
 * I dont know if that is a good idea? or if it is possible to free
 * unused symbols at all.
 */ 

/*
  0  1  2  3  4  5  6  7  8  9
[11 00 00 00 00 10 01 11 00 00] 
[11 10 01 00 10 00 00 00 00 01]  
*/

#define MEMORY_SIZE_64BYTES_TIMES_X(X) (4*(X) + 64*(X))
#define MEMORY_SIZE_8K MEMORY_SIZE_64BYTES_TIMES_X(128);
#define MEMORY_SIZE_16K MEMORY_SIZE_64BYTES_TIMES_X(256);
#define MEMORY_SIZE_32K MEMORY_SIZE_64BYTES_TIMES_X(512);
#define MEMORY_SIZE_1M MEMORY_SIZE_64BYTES_TIMES_X(16384);

#ifndef _MEMORY_H_
#define _MEMORY_H_

extern int memory_init(unsigned char *data, uint32_t size);


#endif 
