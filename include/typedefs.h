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

#ifndef TYPEDEFS_H_ 
#define TYPEDEFS_H_ 

#include <stdint.h>
#include <stdbool.h>


typedef uint32_t VALUE; // A Lisp value.
typedef uint32_t TYPE; // Representation of type.


// TODO: typedefs for non-lisp values (values relevant to the implementation)

typedef uint32_t UINT; // same size as a pointer on target platform
typedef int32_t  INT;


#endif 
