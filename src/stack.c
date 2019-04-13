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

#include <string.h>

#include "stack.h"
#include "typedefs.h"



stack* init_cont_stack(unsigned int stack_size) {

  stack *s = malloc(sizeof(stack));

  s->data = malloc(sizeof(UINT) * stack_size);
  s->sp = 0;
  s->size = stack_size;

  return s;
}


int clear_stack(stack *s) {
  s->sp = 0;
  return 1;
}


int grow_stack(stack *s) {

  unsigned int new_size = s->size * 2;
  UINT *data    = malloc(sizeof(UINT) * new_size);

  if (data == NULL) return 0;

  memcpy(data, s->data, s->size*sizeof(UINT));
  free(s->data);
  s->data = data;
  s->size = new_size;
  return 1;
}

int copy_stack(stack *dest, stack *src) {
  while (dest->size < src->sp) {
    grow_stack(dest);
  }

  dest->sp = src->sp;
  memcpy(dest->data, src->data, src->sp * sizeof(UINT));

  return 1;
}


int push_u32(stack *s, UINT val) {
  int res = 1;
  s->data[s->sp] = val;
  s->sp++;
  if ( s->sp >= s->size) {
    res = grow_stack(s);
  }
  return res;
}


int push_k(stack *s, VALUE (*k)(VALUE)) {
  int res = 1;
  s->data[s->sp] = (UINT)k;
  s->sp++;
  if ( s->sp >= s->size) {
    res = grow_stack(s);
  }
  return res;
}


int pop_u32(stack *s, UINT *val) {

  s->sp--;
  *val = s->data[s->sp];

  return 1;
}

int pop_k(stack *s, VALUE (**k)(VALUE)) {
  s->sp--;
  *k = (VALUE (*)(VALUE))s->data[s->sp];
  return 1;
}
