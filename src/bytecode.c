/*
    Copyright 2020      Joel Svensson	svenssonjoel@yahoo.se

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

#include "symrepr.h"
#include "exp_kind.h"

const uint8_t nop  = 0x0;
/* ... */ 

typedef enum {
  COMPILE_DISPATCH,
  COMPILE_CONTINUATION
} dispatch_mode;

typedef enum {
  CONT_DONE
} continuation;

typedef enum {
  REG_CONT,
  REG_EXP,
  REG_UNEV,
  REG_PRG,
  REG_ARGL,
  REG_VAL,
  REG_FUN
} target;

typedef enum {
  RETURN,
  NEXT,
  JUMP
} link_kind;

typedef struct {
  link_kind kind;
  VALUE     flag;
} linkage;

typedef struct {
  VALUE exp;
  VALUE prg;
  VALUE cont;
  
  VALUE code_list;
  VALUE fun_list; 
} compiler_state;

compiler_state cs;

VALUE compile_instr_list(VALUE exp) {

  cs.exp = car(exp);
  cs.prg = cdr(exp);
  cs.cont = CONT_DONE;
  cs.code_list = enc_sym(symrepr_nil());
  cs.fun_list  = enc_sym(symrepr_nil());
  
  bool done = false;
  

  dispatch_mode dm = COMPILE_DISPATCH;
  
  while (!done){
    switch(dm) {
    case COMPILE_DISPATCH:
      switch(exp_kind_of(cs.exp)) {
	
      }
      break;
    case COMPILE_CONTINUATION:
      break;
    }
  }

  return cs.code_list;
}
