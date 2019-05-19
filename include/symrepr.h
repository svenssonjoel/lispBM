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

// Default and fixed symbol ids
#define DEF_REPR_NIL        0x0FFFF
#define DEF_REPR_QUOTE      0x1FFFF
#define DEF_REPR_TRUE       0x2FFFF
#define DEF_REPR_IF         0x3FFFF
#define DEF_REPR_LAMBDA     0x4FFFF
#define DEF_REPR_CLOSURE    0x5FFFF
#define DEF_REPR_LET        0x6FFFF
#define DEF_REPR_RERROR     0x7FFFF   /* READ ERROR */
#define DEF_REPR_TERROR     0x8FFFF   /* TYPE ERROR */
#define DEF_REPR_EERROR     0x9FFFF   /* EVAL ERROR */
#define DEF_REPR_MERROR     0xAFFFF
#define DEF_REPR_DEFINE     0xBFFFF
#define DEF_REPR_PROGN      0xCFFFF

// Special symbol ids
#define DEF_REPR_ARRAY_TYPE     0x1FFFFFF
#define DEF_REPR_BOXED_I_TYPE   0x2FFFFFF
#define DEF_REPR_BOXED_U_TYPE   0x3FFFFFF
#define DEF_REPR_BOXED_F_TYPE   0x4FFFFFF
#define DEF_REPR_REF_TYPE       0x5FFFFFF
#define DEF_REPR_RECOVERED      0x6FFFFFF
#define DEF_REPR_BYTECODE_TYPE  0x7FFFFFF

#define SYMBOL_MAX  0xFFFFFFF

extern int symrepr_addsym(char *, UINT*);
extern bool symrepr_init();
extern void symrepr_print(void);
extern int symrepr_lookup(char *, UINT*);
extern char* symrepr_lookup_name(UINT);
extern void symrepr_del(void);

static inline UINT symrepr_nil(void)     { return DEF_REPR_NIL; }
static inline UINT symrepr_quote(void)   { return DEF_REPR_QUOTE; }
static inline UINT symrepr_true(void)    { return DEF_REPR_TRUE; }
static inline UINT symrepr_if(void)      { return DEF_REPR_IF; }
static inline UINT symrepr_lambda(void)  { return DEF_REPR_LAMBDA; }
static inline UINT symrepr_closure(void) { return DEF_REPR_CLOSURE; }
static inline UINT symrepr_let(void)     { return DEF_REPR_LET; }
static inline UINT symrepr_define(void)  { return DEF_REPR_DEFINE; }
static inline UINT symrepr_progn(void)   { return DEF_REPR_PROGN; }

static inline UINT symrepr_rerror(void)  { return DEF_REPR_RERROR; }
static inline UINT symrepr_terror(void)  { return DEF_REPR_TERROR; }
static inline UINT symrepr_eerror(void)  { return DEF_REPR_EERROR; }
static inline UINT symrepr_merror(void)  { return DEF_REPR_MERROR; }

static inline bool symrepr_is_error(UINT symrep){
  return (symrep == DEF_REPR_RERROR ||
	  symrep == DEF_REPR_TERROR ||
	  symrep == DEF_REPR_RERROR ||
	  symrep == DEF_REPR_MERROR);
}
#endif
