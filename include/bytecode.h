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
#include "stack.h"
#include "heap.h"

#define OP_PUSH_CONST_V     1
#define OP_PUSH_CONST_D     2  
#define OP_FUN_APP_V        3
#define OP_BUILTIN_APP      4
#define OP_JMP              32    // PC relative jmp
#define OP_JMP_ON_NIL       33
#define OP_JMP_ON_NON_NIL   34
#define OP_CALL_SUB         64    // PC relative call
#define OP_DONE             254
#define OP_RETURN           255

#define COMPILER_OK                     0
#define ERROR_NOT_ENOUGH_SPACE          1
#define ERROR_FORM_NOT_IMPLEMENTED      2
#define ERROR_FORBIDDEN_FORM            3
#define ERROR_NOT_A_CLOSURE             4
#define ERROR_CANNOT_COMPILE            5

bool bytecode_create(bytecode_t *bc, int size);
void bytecode_del(bytecode_t *bc);
bytecode_t *bytecode_compile(stack *s, VALUE v, int *err_code);
int bytecode_snprint(char *buf, int size, bytecode_t bc);
VALUE bytecode_eval(stack *s, bytecode_t *bc);
char *bytecode_get_error(int error);

#endif
