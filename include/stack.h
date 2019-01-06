#ifndef STACK_H_
#define STACK_H_
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
#include <stdint.h>

typedef struct {
  uint32_t* data;
  int32_t   sp;
  uint32_t  size;
} stack;

stack* init_cont_stack(int stack_size);
int clear_stack(stack *s);
int copy_stack(stack *dest, stack *src);
int push_u32(stack *s, uint32_t val);
int push_k(stack *s, uint32_t (*k)(uint32_t));
int pop_u32(stack *s, uint32_t *val);
int pop_k(stack *s, uint32_t (**k)(uint32_t));
#endif
