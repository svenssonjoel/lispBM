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
#include "symrepr.h"

exp_kind exp_kind_of(VALUE exp) {

  switch (type_of(exp)) {
  case VAL_TYPE_SYMBOL:
    if (!is_special(exp))
      return EXP_VARIABLE;
    // fall through
  case PTR_TYPE_BOXED_F:
  case PTR_TYPE_BOXED_U:
  case PTR_TYPE_BOXED_I:
  case VAL_TYPE_I:
  case VAL_TYPE_U:
  case VAL_TYPE_CHAR:
  case PTR_TYPE_ARRAY:
    return EXP_SELF_EVALUATING;
  case PTR_TYPE_CONS: {
    VALUE head = car(exp);
    if (type_of(head) == VAL_TYPE_SYMBOL) {
      UINT sym_id = dec_sym(head);

      if (sym_id == symrepr_and())
	return EXP_AND;
      if (sym_id == symrepr_or())
	return EXP_OR;
      if (sym_id == symrepr_quote())
	return EXP_QUOTED;
      if (sym_id == symrepr_define())
	return EXP_DEFINE;
      if (sym_id == symrepr_progn())
	return EXP_PROGN;
      if (sym_id == symrepr_lambda())
	return EXP_LAMBDA;
      if (sym_id == symrepr_if())
	return EXP_IF;
      if (sym_id == symrepr_let())
	return EXP_LET;
      if (type_of(cdr(exp)) == VAL_TYPE_SYMBOL &&
	  dec_sym(cdr(exp)) == symrepr_nil()) {
	return EXP_NO_ARGS;
      }
    } // end if symbol
    return EXP_APPLICATION;
  } // end case PTR_TYPE_CONS:
  }
  return EXP_KIND_ERROR;
}
