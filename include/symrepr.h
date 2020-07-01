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
#define DEF_REPR_NIL           0x0
#define DEF_REPR_QUOTE         0x1
#define DEF_REPR_TRUE          0x2
#define DEF_REPR_IF            0x3
#define DEF_REPR_LAMBDA        0x4 
#define DEF_REPR_CLOSURE       0x5
#define DEF_REPR_LET           0x6
#define DEF_REPR_RERROR        0x7   /* READ ERROR */
#define DEF_REPR_TERROR        0x8   /* TYPE ERROR */
#define DEF_REPR_EERROR        0x9   /* EVAL ERROR */
#define DEF_REPR_MERROR        0xA
#define DEF_REPR_DIVZERO       0xB
#define DEF_REPR_FATAL_ERROR   0xC  /* Runtime system is corrupt */
#define DEF_REPR_DEFINE        0xD
#define DEF_REPR_PROGN         0xE
//#define DEF_REPR_BACKQUOTE     0xF
#define DEF_REPR_COMMA         0x10
#define DEF_REPR_COMMAAT       0x11

// Special symbol ids
#define DEF_REPR_ARRAY_TYPE     0x20
#define DEF_REPR_BOXED_I_TYPE   0x21
#define DEF_REPR_BOXED_U_TYPE   0x22
#define DEF_REPR_BOXED_F_TYPE   0x23
#define DEF_REPR_REF_TYPE       0x24
#define DEF_REPR_RECOVERED      0x25
#define DEF_REPR_BYTECODE_TYPE  0x26
#define DEF_REPR_NONSENSE       0x27
#define DEF_REPR_NOT_FOUND      0x28

// Type identifying symbols
#define DEF_REPR_TYPE_LIST      0x29
#define DEF_REPR_TYPE_I28       0x2A
#define DEF_REPR_TYPE_U28       0x2B
#define DEF_REPR_TYPE_FLOAT     0x2C
#define DEF_REPR_TYPE_I32       0x2D
#define DEF_REPR_TYPE_U32       0x2E
#define DEF_REPR_TYPE_ARRAY     0x2F
#define DEF_REPR_TYPE_SYMBOL    0x30
#define DEF_REPR_TYPE_CHAR      0x31
#define DEF_REPR_TYPE_REF       0x32

// Fundamental Operations
#define FUNDAMENTALS_START      0x100
#define SYM_ADD                 0x100
#define SYM_SUB                 0x101
#define SYM_MUL                 0x102
#define SYM_DIV                 0x103
#define SYM_MOD                 0x104
#define SYM_EQ                  0x105
#define SYM_NUMEQ               0x106
#define SYM_LT                  0x107
#define SYM_GT                  0x108
#define SYM_EVAL                0x109

#define SYM_AND                 0x110
#define SYM_OR                  0x111
#define SYM_NOT                 0x112

#define SYM_YIELD               0x113
#define SYM_WAIT                0x114
#define SYM_SPAWN               0x115

#define SYM_CONS                0x120
#define SYM_CAR                 0x121
#define SYM_CDR                 0x122
#define SYM_LIST                0x123
#define SYM_APPEND              0x124

#define SYM_ARRAY_READ          0x130
#define SYM_ARRAY_WRITE         0x131
#define SYM_ARRAY_CREATE        0x132

#define SYM_SYMBOL_TO_STRING    0x140
#define SYM_STRING_TO_SYMBOL    0x141
#define SYM_SYMBOL_TO_UINT      0x142
#define SYM_UINT_TO_SYMBOL      0x143
#define SYM_MK_SYMBOL_INDIRECT  0x144

#define SYM_IS_FUNDAMENTAL      0x150

#define SYM_TYPE_OF             0x200
#define FUNDAMENTALS_END        0x200

#define MAX_SPECIAL_SYMBOLS 4096 // 12bits (highest id allowed is 0xFFFF) 

extern int symrepr_addsym(char *, UINT*);
extern bool symrepr_init(void);
extern int symrepr_lookup(char *, UINT*);
extern const char* symrepr_lookup_name(UINT);
extern void symrepr_del(void);

extern unsigned int symrepr_size(void);

static inline UINT symrepr_nil(void)         { return DEF_REPR_NIL; }
static inline UINT symrepr_quote(void)       { return DEF_REPR_QUOTE; }
static inline UINT symrepr_true(void)        { return DEF_REPR_TRUE; }
static inline UINT symrepr_if(void)          { return DEF_REPR_IF; }
static inline UINT symrepr_lambda(void)      { return DEF_REPR_LAMBDA; }
static inline UINT symrepr_closure(void)     { return DEF_REPR_CLOSURE; }
static inline UINT symrepr_let(void)         { return DEF_REPR_LET; }
static inline UINT symrepr_define(void)      { return DEF_REPR_DEFINE; }
static inline UINT symrepr_progn(void)       { return DEF_REPR_PROGN; }
static inline UINT symrepr_comma(void)       { return DEF_REPR_COMMA; }
static inline UINT symrepr_commaat(void)     { return DEF_REPR_COMMAAT; }

static inline UINT symrepr_cons(void)        { return SYM_CONS; }
static inline UINT symrepr_list(void)        { return SYM_LIST; }
static inline UINT symrepr_append(void)      { return SYM_APPEND; }
static inline UINT symrepr_and(void)         { return SYM_AND; }
static inline UINT symrepr_or(void)          { return SYM_OR; }
static inline UINT symrepr_not(void)         { return SYM_NOT; }

static inline UINT symrepr_eval(void)        { return SYM_EVAL; }
static inline UINT symrepr_yield(void)       { return SYM_YIELD; }
static inline UINT symrepr_wait(void)        { return SYM_WAIT; }
static inline UINT symrepr_spawn(void)       { return SYM_SPAWN; }

static inline UINT symrepr_rerror(void)      { return DEF_REPR_RERROR; }
static inline UINT symrepr_terror(void)      { return DEF_REPR_TERROR; }
static inline UINT symrepr_eerror(void)      { return DEF_REPR_EERROR; }
static inline UINT symrepr_merror(void)      { return DEF_REPR_MERROR; }
static inline UINT symrepr_divzero(void)     { return DEF_REPR_DIVZERO; }
static inline UINT symrepr_fatal_error(void) { return DEF_REPR_FATAL_ERROR; }

static inline UINT symrepr_nonsense(void)    { return DEF_REPR_NONSENSE; }
static inline UINT symrepr_not_found(void)   { return DEF_REPR_NOT_FOUND; }

static inline UINT symrepr_type_list(void)   {return DEF_REPR_TYPE_LIST; }
static inline UINT symrepr_type_i28(void)    {return DEF_REPR_TYPE_I28; }       
static inline UINT symrepr_type_u28(void)    {return DEF_REPR_TYPE_U28; }       
static inline UINT symrepr_type_float(void)  {return DEF_REPR_TYPE_FLOAT; }     
static inline UINT symrepr_type_i32(void)    {return DEF_REPR_TYPE_I32; }       
static inline UINT symrepr_type_u32(void)    {return DEF_REPR_TYPE_U32; }       
static inline UINT symrepr_type_array(void)  {return DEF_REPR_TYPE_ARRAY; }     
static inline UINT symrepr_type_symbol(void) {return DEF_REPR_TYPE_SYMBOL; }
static inline UINT symrepr_type_char(void)   {return DEF_REPR_TYPE_CHAR; }
static inline UINT symrepr_type_ref(void)    {return DEF_REPR_TYPE_REF; }


static inline bool symrepr_is_error(UINT symrep){
  return (symrep == DEF_REPR_RERROR ||
	  symrep == DEF_REPR_TERROR ||
	  symrep == DEF_REPR_RERROR ||
	  symrep == DEF_REPR_MERROR ||
	  symrep == DEF_REPR_EERROR || 
	  symrep == DEF_REPR_FATAL_ERROR);
}


#endif
