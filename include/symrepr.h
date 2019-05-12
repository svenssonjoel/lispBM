/*
    Copyright 2018 Joel Svensson	svenssonjoel@yahoo.se

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

#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <stdint.h>
#include <stdbool.h>

#include "typedefs.h"

// Special symbol ids
#define SPECIAL_SYM_ARRAY       0x0001FFFF
#define SPECIAL_SYM_BOXED_I     0x0002FFFF
#define SPECIAL_SYM_BOXED_U     0x0003FFFF
#define SPECIAL_SYM_BOXED_F     0x0004FFFF
#define SPECIAL_SYM_REF         0x0005FFFF
#define SPECIAL_SYM_RECOVERED   0x0006FFFF
#define SPECIAL_SYM_BYTECODE    0x0007FFFF

extern int symrepr_addsym(char *, UINT*);
extern int symrepr_init();
extern void symrepr_print(void);
extern int symrepr_lookup(char *, UINT*);
extern char* symrepr_lookup_name(UINT);
extern void symrepr_del(void);
extern int gensym(UINT *res);

extern UINT symrepr_nil(void);
extern UINT symrepr_eval(void);
extern UINT symrepr_quote(void);
extern UINT symrepr_true(void);
extern UINT symrepr_if(void);
extern UINT symrepr_cond(void);
extern UINT symrepr_lambda(void);
extern UINT symrepr_closure(void);
extern UINT symrepr_let(void);
extern UINT symrepr_define(void);
extern UINT symrepr_progn(void);

extern UINT symrepr_rerror(void);
extern UINT symrepr_terror(void);
extern UINT symrepr_eerror(void);
extern UINT symrepr_merror(void);

extern bool symrepr_is_error(UINT symrep);
#endif
