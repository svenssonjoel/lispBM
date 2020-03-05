/*
    Copyright 2018, 2020 Joel Svensson	svenssonjoel@yahoo.se

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

#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "print.h"
#include "heap.h"
#include "symrepr.h"
#include "typedefs.h"
#include "stack.h"

#define PRINT_STACK_SIZE 256 /* 1 KB */

#define PRINT          1
#define PRINT_SPACE    2
#define START_LIST     3
#define CONTINUE_LIST  4
#define END_LIST       5


int print_value(char *buf,int len, char *error, int len_error, VALUE t) {

  VALUE stack_storage[PRINT_STACK_SIZE];
  
  stack s;
  stack_create(&s, stack_storage, PRINT_STACK_SIZE);

  int n = 0;
  int offset = 0;
  char *str_ptr;
  int res;
  
  push_u32_2(&s, t, PRINT);

  while (!stack_is_empty(&s) && offset <= len - 5) {
    
    VALUE curr;
    UINT  instr;
    pop_u32(&s, &instr);

    switch(instr) {

    case START_LIST: {
      res = 1;
      pop_u32(&s, &curr);
      
      n = snprintf(buf + offset, len - offset, "(");
      offset += n;
      VALUE car_val = car(curr);
      VALUE cdr_val = cdr(curr);

      if (type_of(cdr_val) == PTR_TYPE_CONS) {
	res &= push_u32(&s, cdr_val);
	res &= push_u32(&s, CONTINUE_LIST);
      } else if (type_of(cdr_val) == VAL_TYPE_SYMBOL &&
	         dec_sym(cdr_val) == symrepr_nil()) {
	res &= push_u32(&s, END_LIST);
      } else {
	res &= push_u32(&s, END_LIST);
	res &= push_u32(&s, cdr_val);
	res &= push_u32(&s, PRINT);
	res &= push_u32(&s, PRINT_SPACE);
      }
      res &= push_u32(&s, car_val);
      res &= push_u32(&s, PRINT);

      if (!res) {
	n = snprintf(error, len_error, "Error: Out of print stack\n");
	return -1;
      }
      
      break;
    }
    case CONTINUE_LIST: {

      res = 1;
      pop_u32(&s, &curr);

      if (type_of(curr) == VAL_TYPE_SYMBOL &&
	  dec_sym(curr) == symrepr_nil()) {
	break;
      }
	   
      VALUE car_val = car(curr);
      VALUE cdr_val = cdr(curr);

      n = snprintf(buf + offset, len - offset, " ");
      offset += n;

      if (type_of(cdr_val) == PTR_TYPE_CONS) {
	res &= push_u32(&s, cdr_val);
	res &= push_u32(&s, CONTINUE_LIST);
      } else if (type_of(cdr_val) == VAL_TYPE_SYMBOL &&
		  dec_sym(cdr_val) == symrepr_nil()) {
	res &= push_u32(&s, END_LIST);
      } else {
	res &= push_u32(&s, END_LIST);
	res &= push_u32(&s, cdr_val);
	res &= push_u32(&s, PRINT);
	res &= push_u32(&s, PRINT_SPACE);
      }
      res &= push_u32(&s, car_val);
      res &= push_u32(&s, PRINT);
      if (!res) {
	n = snprintf(error, len_error, "Error: Out of print stack\n");
	return -1;
      }
      break;
    }
    case END_LIST: 
      n = snprintf(buf + offset, len - offset, ")");
      offset += n;
      break;

    case PRINT_SPACE:
      n = snprintf(buf + offset, len - offset, " ");
      offset += n;
      break;
      
    case PRINT:

      pop_u32(&s, &curr);
      
      switch(type_of(curr)) {

      case PTR_TYPE_CONS:{
	res = 1;
	res &= push_u32(&s, curr);
	res &= push_u32(&s, START_LIST);
	if (!res) {
	  n = snprintf(error, len_error, "Error: Out of print stack\n");
	  return -1;
	}
	break;
      }

      case PTR_TYPE_REF:
	n = snprintf(buf + offset, len - offset, "_ref_");
	offset += n;
	break;

      case PTR_TYPE_BOXED_F: {
	VALUE uv = car(curr);
	float v;
	memcpy(&v, &uv, sizeof(float)); // = *(float*)(&uv);
	n = snprintf(buf + offset, len - offset, "{%"PRI_FLOAT"}", v);
	offset += n;
	break;
      }
	
      case PTR_TYPE_BOXED_U: {
	VALUE v = car(curr);
	n = snprintf(buf + offset, len - offset, "{%"PRI_UINT"}", v);
	offset += n;
	break;
      }
	
      case PTR_TYPE_BOXED_I: {
	int32_t v = (int32_t)car(curr);
	return snprintf(buf + offset, len - offset, "{%"PRI_INT"}", v);
	offset += n;
	break;
      }
	
      case PTR_TYPE_ARRAY: {
	array_t *array = (array_t *)car(curr);
	switch (array->elt_type){
	case VAL_TYPE_CHAR:
	  n = snprintf(buf + offset, len - offset, "\"%s\"", array->data.c);
	  offset += n;
	  break;
	  break;
	default:
	  n = snprintf(error, len_error, "Error: Array type not supported\n");
	  return -1;
	}
	break;
      }
	
      case VAL_TYPE_SYMBOL:
	str_ptr = symrepr_lookup_name(dec_sym(curr));
	if (str_ptr == NULL) {
	  
	  snprintf(error, len_error, "Error: Symbol not in table %"PRI_UINT"", dec_sym(t));
	  return -1;
	} 
	n = snprintf(buf + offset, len - offset, "%s", str_ptr);
	offset += n;
	break; //Break VAL_TYPE_SYMBOL
	
      case VAL_TYPE_I:
	n = snprintf(buf + offset, len - offset, "%"PRI_INT"", dec_i(curr));
	offset += n;
	break;
	
      case VAL_TYPE_U:
	n = snprintf(buf + offset, len - offset, "%"PRI_UINT"", dec_u(curr));
	offset += n;
	break;
	
      case VAL_TYPE_CHAR:
	n = snprintf(buf + offset, len - offset, "\\#%c", dec_char(curr));
	offset += n;
	break;
	
      default:
	snprintf(error, len_error, "Error: print does not recognize type of value: %"PRIx32"", curr);
	return -1;
	break;
      } // Switch type of curr
      break; // case PRINT
      
    default:
      snprintf(error, len_error, "Error: Corrupt print stack!");
      return -1;
    }// Switch instruction
  }//While not empty stack


  if (!stack_is_empty(&s)) {
    snprintf(buf + (len - 5), 4, "...");
    buf[len-1] = 0;
    return len;
  }

  
  return n;
}

