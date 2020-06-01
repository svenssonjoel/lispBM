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

#include "exp_kind.h"

typedef enum {
  COMPILE_DISPATCH,
  COMPILE_CONTINUATION
} dispatch_mode;

typedef enum {
  CONT_DONE
} continuation;

typedef struct {
  VALUE exp;
  VALUE prg;
  VALUE cont;

  VALUE code_list;
} compiler_state;

compiler_state cs;

VALUE compile_instr_list(VALUE exp) {

  cs.exp = car(exp);
  cs.prg = cdr(exp);
  cs.cont = CONT_DONE;
  
  bool done = false;
  VALUE code;

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

  return code;
}
