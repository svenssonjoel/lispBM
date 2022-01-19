/*
    Copyright 2019, 2021 Joel Svensson  svenssonjoel@yahoo.se

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

#include <string.h>

#include "stack.h"
#include "lispbm_types.h"
#include "print.h"
#include "lispbm_memory.h"

int stack_allocate(stack *s, unsigned int stack_size) {
  s->data = memory_allocate(stack_size);
  s->sp = 0;
  s->size = stack_size;
  s->max_sp = 0;

  if (s->data) return 1;
  return 0;
}

int stack_create(stack *s, UINT* data, unsigned int size) {
  s->data = data;
  s->sp = 0;
  s->size = size;
  s->max_sp = 0;
  return 1;
}

void stack_free(stack *s) {
  if (s->data) {
    memory_free(s->data);
  }
}

int stack_clear(stack *s) {
  s->sp = 0;
  return 1;
}

UINT *stack_ptr(stack *s, unsigned int n) {
  if (n > s->sp) return NULL;
  unsigned int index = s->sp - n;
  return &s->data[index];
}

int stack_drop(stack *s, unsigned int n) {

  if (n > s->sp) return 0;

  s->sp -= n;
  return 1;
}

int push_u32(stack *s, UINT val) {
  int res = 1;
  if (s->sp == s->size) {
    return 0;
  }

  if (!res) return res;

  s->data[s->sp] = val;
  s->sp++;

  if (s->sp > s->max_sp) s->max_sp = s->sp;

  return res;
}

int push_k(stack *s, VALUE (*k)(VALUE)) {
  int res = 1;
  if ( s->sp == s->size) {
    return 0;
  }

  if (!res) return res;

  s->data[s->sp] = (UINT)k;
  s->sp++;

  if (s->sp > s->max_sp) s->max_sp = s->sp;

  return res;
}

int pop_u32(stack *s, UINT *val) {

  s->sp--;
  *val = s->data[s->sp];
  s->data[s->sp] = 0;
  return 1;
}

int pop_k(stack *s, VALUE (**k)(VALUE)) {
  s->sp--;
  *k = (VALUE (*)(VALUE))s->data[s->sp];
  s->data[s->sp] = 0;
  return 1;
}
