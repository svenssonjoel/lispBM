/*
    Copyright 2023, 2025 Joel Svensson        svenssonjoel@yahoo.se
              2022       Benjamin Vedder      benjamin@vedder.se

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

#include <extensions.h>

const char *loop_extensions_dyn_load[4] =
  {
  "(define loopfor "
  "(macro (it start cnd update body) "
  "      `(call-cc-unsafe (lambda (break) "
  "                         (let ((a02 (lambda (,it a01) "
  "                                       (if ,cnd (a02 ,update ,body) a01)))) "
  "                           (a02 ,start nil)))) "
  "       ))",

  "(define loopwhile "
  "(macro (cnd body) "
  "       `(call-cc-unsafe (lambda (break) (let ((a02 (lambda (a01) "
  "                    (if ,cnd (a02 ,body) a01)))) "
  "         (a02 nil)))) "
  "      )) ",

  "(define looprange "
  "(macro (it start end body) "
  "       `(call-cc-unsafe "
  "        (lambda (break) "
  "          (let ((a02 (lambda (,it a01) "
  "                       (if (< ,it ,end) (a02 (+ ,it 1) ,body) a01)))) "
  "            (a02 ,start nil)))) "
  "      )) ",

  "(define loopforeach "
  "(macro (it lst body) "
  "      `(call-cc-unsafe (lambda (break) (let ((a02 (lambda (,it a04 a01) "
  "                     (if (eq ,it nil) a01 (a02 (car a04) (cdr a04) ,body))))) "
  "         (a02 (car ,lst) (cdr ,lst) nil)))) "
  "      )) "
  };

void lbm_loop_extensions_init(void) {
}
