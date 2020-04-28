/*
    Copyright 2020 Joel Svensson	svenssonjoel@yahoo.se

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


/*
   Quasiquotation expansion attempt

   Trying to adapt the algorithm from Bawden - Quasiquotation in Lisp
   to be applied during parsing.
*/


#include "heap.h"
#include "typedefs.h"
#include "symrepr.h"
#include "stack.h"
#include "qq_expand.h"


VALUE gen_cons(VALUE a, VALUE b) {
  return cons(enc_sym(symrepr_cons()),
		   cons(a,
			cons(b, enc_sym(symrepr_nil()))));
}


VALUE append(VALUE front, VALUE back) {
  return cons (enc_sym(symrepr_append()),
	       cons(front,
		    cons(back, enc_sym(symrepr_nil()))));
}

/* Bawden's qq-expand-list implementation
(define (qq-expand-list x)
  (cond ((tag-comma? x)
	 `(list ,(tag-data x)))
	((tag-comma-atsign? x)
	 (tag-data x))
	((tag-backquote? x)
	 (qq-expand-list
	  (qq-expand (tag-data x))))
	((pair? x)
	 `(list
	   (append
	    ,(qq-expand-list (car x))
	    ,(qq-expand (cdr x)))))
	(else `'(,x))))
*/

VALUE qq_expand_list(VALUE l) {
  VALUE res = enc_sym(symrepr_nil());
  VALUE car_val;
  VALUE cdr_val;

  switch (type_of(l)) {
  case PTR_TYPE_CONS:
    car_val = car(l);
    cdr_val = cdr(l);
    if (type_of(car_val) == VAL_TYPE_SYMBOL &&
	dec_sym(car_val) == symrepr_comma()) {
      res = cons(enc_sym(symrepr_list()),
		 cons(car(cdr_val), res));
    } else if (type_of(car_val) == VAL_TYPE_SYMBOL &&
	       dec_sym(car_val) == symrepr_commaat()) {
      res = car(cdr_val);
    } else {
      VALUE expand_car = qq_expand_list(car_val);
      VALUE expand_cdr = qq_expand(cdr_val);
      res = cons(enc_sym(symrepr_list()),
		 cons(append(expand_car, expand_cdr), enc_sym(symrepr_nil())));
    }
    break;
  default:
    res = cons(enc_sym(symrepr_list()),
	       cons(l, enc_sym(symrepr_nil())));
  }
  return res;
}

/* Bawden's qq-expand implementation
(define (qq-expand x)
  (cond ((tag-comma? x)
	 (tag-data x))
	((tag-comma-atsign? x)
	 (error "Illegal"))
	((tag-backquote? x)
	 (qq-expand
	  (qq-expand (tag-data x))))
	((pair? x)
	 `(append
	   ,(qq-expand-list (car x))
	   ,(qq-expand (cdr x))))
	(else `',x)))
 */

VALUE qq_expand(VALUE qquoted) {

  VALUE res = enc_sym(symrepr_nil());
  VALUE car_val;
  VALUE cdr_val;

  switch (type_of(qquoted)) {
  case PTR_TYPE_CONS:
    car_val = car(qquoted);
    cdr_val = cdr(qquoted);
    if (type_of(car_val) == VAL_TYPE_SYMBOL &&
	dec_sym(car_val) == symrepr_comma()) {
      res = car(cdr_val);
    } else if (type_of(car_val) == VAL_TYPE_SYMBOL &&
	       dec_sym(car_val) == symrepr_commaat()) {
      res = enc_sym(symrepr_rerror()); // should have a more specific error here. 
    } else {
      VALUE expand_car = qq_expand_list(car_val);
      VALUE expand_cdr = qq_expand(cdr_val);
      res = append(expand_car, expand_cdr);
    }
    break;
  default:
    res = cons(enc_sym(symrepr_quote()), cons(qquoted, enc_sym(symrepr_nil())));
    break;
  }
  return res;
}
