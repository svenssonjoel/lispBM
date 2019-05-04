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

#ifndef BYTECODE_H_
#define BYTECODE_H_

#include <stdint.h>
#include "typedefs.h"

#define MAX_CONSTANTS       256

#define OP_PUSH_CONST_V     1
#define OP_PUSH_CONST_D     2  
#define OP_FUN_APP          3
#define OP_DONE             254
#define OP_RETURN           255

#define COMPILER_OK                  0
#define ERROR_NOT_ENOUGH_SPACE      -1
#define ERROR_CANNOT_COMPILE        -2
#define ERROR_FORM_NOT_IMPLEMENTED  -3

typedef struct {
  unsigned int code_size; 
  uint8_t *code;
  int     num_constants; 
  VALUE   constants[MAX_CONSTANTS];
} bytecode_t;

int bytecode_create(bytecode_t *bc, int size);

int bytecode_ncompile(VALUE v, bytecode_t *bc, int max_size, int *err_code);
int bytecode_snprint(char *buf, int size, bytecode_t bc);
VALUE bytecode_eval(bytecode_t bc, VALUE globalenv, VALUE localenv);

#endif
