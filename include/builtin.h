/*
    Copyright 2018 Joel Svensson	svenssonjoel@yahoo.se

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

#ifndef BUILT_IN_H_
#define BUILT_IN_H_

#include "typedefs.h"

typedef VALUE (*bi_fptr)(VALUE);

bi_fptr builtin_lookup_function(VALUE sym);
int builtin_add_function(char *sym_str, bi_fptr fun_ptr);
int builtin_init(void);
void builtin_del(void);
VALUE built_in_gen_env(void);

int structural_equality(VALUE a, VALUE b);
#endif
