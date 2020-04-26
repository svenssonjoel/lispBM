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

#ifndef _MEMORY_H_
#define _MEMORY_H_

extern int memory_init(unsigned char *data, uint32_t size);


#endif 
