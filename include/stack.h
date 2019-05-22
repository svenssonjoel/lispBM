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
#include <stdbool.h>
#include <stdio.h>

#include "typedefs.h"

typedef struct {
  UINT* data;
  unsigned int sp;
  unsigned int size;
  bool growable;
} stack;

stack* stack_init(unsigned int stack_size, bool growable);
void stack_del(stack *s);
int stack_clear(stack *s);
int stack_copy(stack *dest, stack *src);
int push_u32(stack *s, UINT val);
int push_k(stack *s, VALUE (*k)(VALUE));
int pop_u32(stack *s, UINT *val);
int pop_k(stack *s, VALUE (**k)(VALUE));

static inline int stack_arg_ix(stack *s, unsigned int ix, UINT *res) {
  if (ix > s->sp-1) return 0;
  *res = s->data[s->sp-(ix+1)];
  return 1;
}

static inline int push_u32_2(stack *s, UINT val0, UINT val1) {
  int res = 1;
  res &= push_u32(s,val0);
  res &= push_u32(s,val1);
  return res;
}

static inline int push_u32_3(stack *s, UINT val0, UINT val1, UINT val2) {
  int res = 1;
  res &= push_u32(s,val0);
  res &= push_u32(s,val1);
  res &= push_u32(s,val2);
  return res;
}

static inline int push_u32_4(stack *s, UINT val0, UINT val1, UINT val2, UINT val3) {
  int res = 1;
  res &= push_u32(s,val0);
  res &= push_u32(s,val1);
  res &= push_u32(s,val2);
  res &= push_u32(s,val3);
  return res;
}

static inline int push_u32_5(stack *s, UINT val0, UINT val1, UINT val2, UINT val3, UINT val4) {
  int res = 1;
  res &= push_u32(s,val0);
  res &= push_u32(s,val1);
  res &= push_u32(s,val2);
  res &= push_u32(s,val3);
  res &= push_u32(s,val4);
  return res;
}

static inline int pop_u32_2(stack *s, UINT *r0, UINT *r1) {
  int res = 1;
  res &= pop_u32(s, r0);
  res &= pop_u32(s, r1);
  return res;
}

static inline int pop_u32_3(stack *s, UINT *r0, UINT *r1, UINT *r2) {
  int res = 1;
  res &= pop_u32(s, r0);
  res &= pop_u32(s, r1);
  res &= pop_u32(s, r2);
  return res;
}

static inline int pop_u32_4(stack *s, UINT *r0, UINT *r1, UINT *r2, UINT *r3) {
  int res = 1;
  res &= pop_u32(s, r0);
  res &= pop_u32(s, r1);
  res &= pop_u32(s, r2);
  res &= pop_u32(s, r3);
  return res;
}

static inline int pop_u32_5(stack *s, UINT *r0, UINT *r1, UINT *r2, UINT *r3, UINT *r4) {
  int res = 1;
  res &= pop_u32(s, r0);
  res &= pop_u32(s, r1);
  res &= pop_u32(s, r2);
  res &= pop_u32(s, r3);
  res &= pop_u32(s, r4);
  return res;
}


#endif
