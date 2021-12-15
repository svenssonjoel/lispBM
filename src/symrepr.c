/*
    Copyright 2018, 2021 Joel Svensson	svenssonjoel@yahoo.se

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

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

#include "symrepr.h"
#include "memory.h"

/*************************************/
/* Constants for be built in symbols */

const UINT symrepr_nil         =  DEF_REPR_NIL; 
const UINT symrepr_quote       =  DEF_REPR_QUOTE; 
const UINT symrepr_true        =  DEF_REPR_TRUE; 
const UINT symrepr_if          =  DEF_REPR_IF; 
const UINT symrepr_lambda      =  DEF_REPR_LAMBDA; 
const UINT symrepr_closure     =  DEF_REPR_CLOSURE; 
const UINT symrepr_let         =  DEF_REPR_LET; 
const UINT symrepr_define      =  DEF_REPR_DEFINE; 
const UINT symrepr_progn       =  DEF_REPR_PROGN; 
const UINT symrepr_comma       =  DEF_REPR_COMMA; 
const UINT symrepr_commaat     =  DEF_REPR_COMMAAT;
const UINT symrepr_dontcare    =  DEF_REPR_DONTCARE;
const UINT symrepr_send        =  DEF_REPR_SEND;
const UINT symrepr_receive     =  DEF_REPR_RECEIVE;
const UINT symrepr_match       =  DEF_REPR_MATCH;
const UINT symrepr_nomatch     =  DEF_REPR_NO_MATCH;
const UINT symrepr_match_any   =  DEF_REPR_MATCH_ANY;
const UINT symrepr_match_i28   =  DEF_REPR_MATCH_I28;
const UINT symrepr_match_u28   =  DEF_REPR_MATCH_U28;
const UINT symrepr_match_float =  DEF_REPR_MATCH_FLOAT;
const UINT symrepr_match_cons  =  DEF_REPR_MATCH_CONS;

const UINT symrepr_cons        =  SYM_CONS; 
const UINT symrepr_list        =  SYM_LIST; 
const UINT symrepr_append      =  SYM_APPEND; 
const UINT symrepr_and         =  SYM_AND; 
const UINT symrepr_or          =  SYM_OR; 
const UINT symrepr_not         =  SYM_NOT; 

const UINT symrepr_eval        =  SYM_EVAL; 
const UINT symrepr_yield       =  SYM_YIELD; 
const UINT symrepr_wait        =  SYM_WAIT; 
const UINT symrepr_spawn       =  SYM_SPAWN; 

const UINT symrepr_rerror      =  DEF_REPR_RERROR; 
const UINT symrepr_terror      =  DEF_REPR_TERROR; 
const UINT symrepr_eerror      =  DEF_REPR_EERROR; 
const UINT symrepr_merror      =  DEF_REPR_MERROR; 
const UINT symrepr_divzero     =  DEF_REPR_DIVZERO; 
const UINT symrepr_fatal_error =  DEF_REPR_FATAL_ERROR; 

const UINT symrepr_nonsense    =  DEF_REPR_NONSENSE; 
const UINT symrepr_not_found   =  DEF_REPR_NOT_FOUND; 

const UINT symrepr_type_list   =  DEF_REPR_TYPE_LIST; 
const UINT symrepr_type_i28    =  DEF_REPR_TYPE_I28;        
const UINT symrepr_type_u28    =  DEF_REPR_TYPE_U28;        
const UINT symrepr_type_float  =  DEF_REPR_TYPE_FLOAT;      
const UINT symrepr_type_i32    =  DEF_REPR_TYPE_I32;        
const UINT symrepr_type_u32    =  DEF_REPR_TYPE_U32;        
const UINT symrepr_type_array  =  DEF_REPR_TYPE_ARRAY;      
const UINT symrepr_type_symbol =  DEF_REPR_TYPE_SYMBOL; 
const UINT symrepr_type_char   =  DEF_REPR_TYPE_CHAR; 
const UINT symrepr_type_ref    =  DEF_REPR_TYPE_REF; 

#define NUM_SPECIAL_SYMBOLS 79

#define NAME   0
#define ID     1
#define NEXT   2

typedef struct {
  const char *name;
  const UINT id;
} special_sym;

special_sym const special_symbols[NUM_SPECIAL_SYMBOLS] =  {
  {"nil"        , DEF_REPR_NIL},
  {"quote"      , DEF_REPR_QUOTE},
  {"t"          , DEF_REPR_TRUE},
  {"if"         , DEF_REPR_IF},
  {"lambda"     , DEF_REPR_LAMBDA},
  {"closure"    , DEF_REPR_CLOSURE},
  {"let"        , DEF_REPR_LET},
  {"define"     , DEF_REPR_DEFINE},
  {"progn"      , DEF_REPR_PROGN},
  //{"bquote"     , DEF_REPR_BACKQUOTE},
  {"comma"      , DEF_REPR_COMMA},
  {"splice"     , DEF_REPR_COMMAAT},
  {"match"      , DEF_REPR_MATCH},
  {"_"          , DEF_REPR_DONTCARE},
  {"send"       , DEF_REPR_SEND},
  {"recv"       , DEF_REPR_RECEIVE},
  {"?"          , DEF_REPR_MATCH_ANY},
  {"?i28"       , DEF_REPR_MATCH_I28},
  {"?u28"       , DEF_REPR_MATCH_U28},
  {"?float"     , DEF_REPR_MATCH_FLOAT},
  {"?cons"      , DEF_REPR_MATCH_CONS},
  
  // Special symbols with unparseable names
  {"no_match"           , DEF_REPR_NO_MATCH},
  {"read_error"         , DEF_REPR_RERROR},
  {"type_error"         , DEF_REPR_TERROR},
  {"eval_error"         , DEF_REPR_EERROR},
  {"out_of_memory"      , DEF_REPR_MERROR},
  {"fatal_error"        , DEF_REPR_FATAL_ERROR},
  {"division_by_zero"   , DEF_REPR_DIVZERO},
  {"sym_array"          , DEF_REPR_ARRAY_TYPE},
  {"sym_boxed_i"        , DEF_REPR_BOXED_I_TYPE},
  {"sym_boxed_u"        , DEF_REPR_BOXED_U_TYPE},
  {"sym_boxed_f"        , DEF_REPR_BOXED_F_TYPE},
  {"sym_ref"            , DEF_REPR_REF_TYPE},
  {"sym_recovered"      , DEF_REPR_RECOVERED},
  {"sym_bytecode"       , DEF_REPR_BYTECODE_TYPE},
  {"sym_nonsense"       , DEF_REPR_NONSENSE},
  {"variable_not_bound" , DEF_REPR_NOT_FOUND},
  
  // special symbols with parseable names
  {"type-list"        , DEF_REPR_TYPE_LIST},
  {"type-i28"         , DEF_REPR_TYPE_I28},
  {"type-u28"         , DEF_REPR_TYPE_U28},
  {"type-float"       , DEF_REPR_TYPE_FLOAT},
  {"type-i32"         , DEF_REPR_TYPE_I32},
  {"type-u32"         , DEF_REPR_TYPE_U32},
  {"type-array"       , DEF_REPR_TYPE_ARRAY},
  {"type-symbol"      , DEF_REPR_TYPE_SYMBOL},
  {"type-char"        , DEF_REPR_TYPE_CHAR},
  {"type-ref"         , DEF_REPR_TYPE_REF},

  // Fundamental operations
  {"+"              , SYM_ADD},
  {"-"              , SYM_SUB},
  {"*"              , SYM_MUL},
  {"/"              , SYM_DIV},
  {"mod"            , SYM_MOD},
  {"="              , SYM_EQ},
  {"<"              , SYM_LT},
  {">"              , SYM_GT},
  {"eval"           , SYM_EVAL},
  {"and"            , SYM_AND},
  {"or"             , SYM_OR},
  {"not"            , SYM_NOT},
  {"yield"          , SYM_YIELD},
  {"wait"           , SYM_WAIT},
  {"spawn"          , SYM_SPAWN},
  {"num-eq"         , SYM_NUMEQ},
  {"car"            , SYM_CAR},
  {"cdr"            , SYM_CDR},
  {"cons"           , SYM_CONS},
  {"list"           , SYM_LIST},
  {"append"         , SYM_APPEND},
  {"array-read"     , SYM_ARRAY_READ},
  {"array-write"    , SYM_ARRAY_WRITE},
  {"array-create"   , SYM_ARRAY_CREATE},
  {"type-of"        , SYM_TYPE_OF},
  {"sym-to-str"     , SYM_SYMBOL_TO_STRING},
  {"str-to-sym"     , SYM_STRING_TO_SYMBOL},
  {"sym-to-u"       , SYM_SYMBOL_TO_UINT},
  {"u-to-sym"       , SYM_UINT_TO_SYMBOL},
  {"mk-sym-indirect", SYM_MK_SYMBOL_INDIRECT},
  {"set-car"        , SYM_SET_CAR},
  {"set-cdr"        , SYM_SET_CDR},
  {"is-fundamental" , SYM_IS_FUNDAMENTAL} 
};


uint32_t *symlist = NULL;
UINT next_symbol_id = 0;

bool symrepr_init(void) {
  return true;
}

void symrepr_del(void) {

  uint32_t *curr = symlist;
  while (curr) {
    uint32_t *tmp = curr; 
    curr = (uint32_t*)curr[NEXT];
    memory_free((uint32_t*)tmp[NAME]);
    memory_free(tmp);
  }
}

const char *lookup_symrepr_name_memory(UINT id) {

  uint32_t *curr = symlist;
  while (curr) {
    if (id == curr[ID]) {
      return (const char *)curr[NAME];
    }
    curr = (uint32_t*)curr[NEXT];
  }
  return NULL;
}

// Lookup symbol name given a symbol id
const char *symrepr_lookup_name(UINT id) {
  if (id < MAX_SPECIAL_SYMBOLS) {
    for (int i = 0; i < NUM_SPECIAL_SYMBOLS; i ++) {
      if (id == special_symbols[i].id) {
	return (special_symbols[i].name);
      }
    } 
  }
  return lookup_symrepr_name_memory(id);
}

// Lookup symbol id given symbol name
int symrepr_lookup(char *name, UINT* id) {

  // loop through special symbols
  for (int i = 0; i < NUM_SPECIAL_SYMBOLS; i ++) {
    if (strcmp(name, special_symbols[i].name) == 0) {
      *id = special_symbols[i].id;
      return 1;
    }
  }

  uint32_t *curr = symlist;
  while (curr) {
    char *str = (char*)curr[NAME];
    if (strcmp(name, str) == 0) {
      *id = curr[ID];
      return 1;
    }
    curr = (uint32_t*)curr[NEXT];
  }
  return 0;
}

int symrepr_addsym(char *name, UINT* id) {
  size_t  n = 0;

  n = strlen(name) + 1;
  if (n == 1) return 0; // failure if empty symbol

  uint32_t *m = memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  char *symbol_name_storage = NULL;;
  if (n % 4 == 0) {
    symbol_name_storage = (char *)memory_allocate(n);
  } else {
    symbol_name_storage = (char *)memory_allocate(n);
  }

  if (symbol_name_storage == NULL) {
    memory_free(m);
    return 0;
  }

  strcpy(symbol_name_storage, name);

  m[NAME] = (uint32_t)symbol_name_storage;
  
  if (symlist == NULL) {
    m[NEXT] = (uint32_t) NULL;
    symlist = m;
  } else {
    m[NEXT] = (uint32_t) symlist;
    symlist = m;
  }
  m[ID] = MAX_SPECIAL_SYMBOLS + next_symbol_id++; 
  *id = m[ID];
  return 1;
}

unsigned int symrepr_size(void) {

  unsigned int n = 0;
  uint32_t *curr = symlist;

  while (curr) {
    // up to 3 extra bytes are used for string storage if length is not multiple of 4
    size_t s = strlen((char *)curr[NAME]);
    s ++;
    n += s % 4;
    n += 12; // sizeof the node in the linked list
    curr = (uint32_t *)curr[NEXT];
  }
  return n;
}
