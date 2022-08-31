/*
    Copyright 2018, 2021 2022 Joel Svensson  svenssonjoel@yahoo.se

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
#include <lbm_memory.h>

#include "symrepr.h"

#define NUM_SPECIAL_SYMBOLS (sizeof(special_symbols) / sizeof(special_sym))
#define NAME   0
#define ID     1
#define NEXT   2

typedef struct {
  const char *name;
  const lbm_uint id;
} special_sym;

special_sym const special_symbols[] =  {
  {"nil"        , SYM_NIL},
  {"quote"      , SYM_QUOTE},
  {"t"          , SYM_TRUE},
  {"if"         , SYM_IF},
  {"lambda"     , SYM_LAMBDA},
  {"closure"    , SYM_CLOSURE},
  {"let"        , SYM_LET},
  {"define"     , SYM_DEFINE},
  {"progn"      , SYM_PROGN},
  {"read"       , SYM_READ},
  {"read-program" , SYM_READ_PROGRAM},
  //{"comma"      , SYM_COMMA},   // should not be accessible to programmer
  //{"splice"     , SYM_COMMAAT},
  {"match"        , SYM_MATCH},
  {"_"            , SYM_DONTCARE},
  {"send"         , SYM_SEND},
  {"recv"         , SYM_RECEIVE},
  {"macro"        , SYM_MACRO},
  {"macro-expand" , SYM_MACRO_EXPAND},
  {"call-cc"      , SYM_CALLCC},
  {"continuation" , SYM_CONT},

  {"setvar"         , SYM_SETVAR},
  {"gc"           , SYM_PERFORM_GC},
  {"namespace"    , SYM_NAMESPACE},

  // pattern matching
  {"?"          , SYM_MATCH_ANY},
  {"?i"         , SYM_MATCH_I},
  {"?u"         , SYM_MATCH_U},
  {"?u32"       , SYM_MATCH_U32},
  {"?i32"       , SYM_MATCH_I32},
  {"?float"     , SYM_MATCH_FLOAT},
  {"?cons"      , SYM_MATCH_CONS},
  {"?u64"       , SYM_MATCH_U64},
  {"?i64"       , SYM_MATCH_I64},
  {"?double"    , SYM_MATCH_DOUBLE},

  // Special symbols with unparsable names
  {"no_match"           , SYM_NO_MATCH},
  {"read_error"         , SYM_RERROR},
  {"type_error"         , SYM_TERROR},
  {"eval_error"         , SYM_EERROR},
  {"out_of_memory"      , SYM_MERROR},
  {"fatal_error"        , SYM_FATAL_ERROR},
  {"out_of_stack"       , SYM_STACK_ERROR},
  {"division_by_zero"   , SYM_DIVZERO},
  {"sym_array"          , SYM_ARRAY_TYPE},
  {"sym_raw_i"          , SYM_RAW_I_TYPE},
  {"sym_raw_u"          , SYM_RAW_U_TYPE},
  {"sym_raw_f"          , SYM_RAW_F_TYPE},
  {"sym_ind_i"          , SYM_IND_I_TYPE},
  {"sym_ind_u"          , SYM_IND_U_TYPE},
  {"sym_ind_f"          , SYM_IND_F_TYPE},
  {"sym_stream"         , SYM_STREAM_TYPE},
  {"sym_recovered"      , SYM_RECOVERED},
  {"sym_bytecode"       , SYM_BYTECODE_TYPE},
  {"sym_custom"         , SYM_CUSTOM_TYPE},
  {"sym_nonsense"       , SYM_NONSENSE},
  {"variable_not_bound" , SYM_NOT_FOUND},

  // tokenizer symbols with unparsable names
  {"sym_openpar"        , SYM_OPENPAR},
  {"sym_closepar"       , SYM_CLOSEPAR},
  {"sym_backquote"      , SYM_BACKQUOTE},
  {"sym_comma"          , SYM_COMMA},
  {"sym_commaat"        , SYM_COMMAAT},
  {"sym_dot"            , SYM_DOT},
  {"sym_tok_done"       , SYM_TOKENIZER_DONE},
  {"sym_quote_it"       , SYM_QUOTE_IT},
  {"sym_colon"          , SYM_COLON},

  // special symbols with parseable names
  {"type-list"        , SYM_TYPE_LIST},
  {"type-i"           , SYM_TYPE_I},
  {"type-u"           , SYM_TYPE_U},
  {"type-float"       , SYM_TYPE_FLOAT},
  {"type-i32"         , SYM_TYPE_I32},
  {"type-u32"         , SYM_TYPE_U32},
  {"type-double"      , SYM_TYPE_DOUBLE},
  {"type-i64"         , SYM_TYPE_I64},
  {"type-u64"         , SYM_TYPE_U64},
  {"type-array"       , SYM_TYPE_ARRAY},
  {"type-symbol"      , SYM_TYPE_SYMBOL},
  {"type-char"        , SYM_TYPE_CHAR},
  {"type-byte"        , SYM_TYPE_BYTE},
  {"type-ref"         , SYM_TYPE_REF},
  {"type-stream"      , SYM_TYPE_STREAM},
  // Fundamental operations
  {"+"              , SYM_ADD},
  {"-"              , SYM_SUB},
  {"*"              , SYM_MUL},
  {"/"              , SYM_DIV},
  {"mod"            , SYM_MOD},
  {"="              , SYM_NUMEQ},
  {"<"              , SYM_LT},
  {">"              , SYM_GT},
  {"<="             , SYM_LEQ},
  {">="             , SYM_GEQ},
  {"eval"           , SYM_EVAL},
  {"eval-program"   , SYM_EVAL_PROGRAM},
  {"and"            , SYM_AND},
  {"or"             , SYM_OR},
  {"not"            , SYM_NOT},
  {"yield"          , SYM_YIELD},
  {"wait"           , SYM_WAIT},
  {"spawn"          , SYM_SPAWN},
  {"eq"             , SYM_EQ},
  {"car"            , SYM_CAR},
  {"cdr"            , SYM_CDR},
  {"cons"           , SYM_CONS},
  {"list"           , SYM_LIST},
  {"append"         , SYM_APPEND},
  {"array-read"     , SYM_ARRAY_READ},
  {"array-write"    , SYM_ARRAY_WRITE},
  {"array-create"   , SYM_ARRAY_CREATE},
  {"array-size"     , SYM_ARRAY_SIZE},
  {"array-clear"    , SYM_ARRAY_CLEAR},
  {"type-of"        , SYM_TYPE_OF},
  {"sym2str"        , SYM_SYMBOL_TO_STRING},
  {"str2sym"        , SYM_STRING_TO_SYMBOL},
  {"sym2u"          , SYM_SYMBOL_TO_UINT},
  {"u2sym"          , SYM_UINT_TO_SYMBOL},
  {"setcar"         , SYM_SET_CAR},
  {"setcdr"         , SYM_SET_CDR},
  {"setix"          , SYM_SET_IX},

  {"assoc"          , SYM_ASSOC}, // lookup an association
  {"cossa"          , SYM_COSSA}, // lookup an association "backwards"
  {"acons"          , SYM_ACONS}, // Add to alist
  {"setassoc"       , SYM_SET_ASSOC}, // Change association

  {"shl"            , SYM_SHL},
  {"shr"            , SYM_SHR},
  {"bitwise-and"    , SYM_BITWISE_AND},
  {"bitwise-or"     , SYM_BITWISE_OR},
  {"bitwise-xor"    , SYM_BITWISE_XOR},
  {"bitwise-not"    , SYM_BITWISE_NOT},

  {"custom-destruct", SYM_CUSTOM_DESTRUCT},

  {"to-i"           , SYM_TO_I},
  {"to-i32"         , SYM_TO_I32},
  {"to-u"           , SYM_TO_U},
  {"to-u32"         , SYM_TO_U32},
  {"to-float"       , SYM_TO_FLOAT},
  {"to-i64"         , SYM_TO_I64},
  {"to-u64"         , SYM_TO_U64},
  {"to-double"      , SYM_TO_DOUBLE},
  {"to-byte"        , SYM_TO_BYTE},

  // Streams
  {"stream-get"     , SYM_STREAM_GET},
  {"stream-more"    , SYM_STREAM_MORE},
  {"stream-peek"    , SYM_STREAM_PEEK},
  {"stream-drop"    , SYM_STREAM_DROP},
  {"stream-put"     , SYM_STREAM_PUT},

  // fast access in list
  {"ix"             , SYM_IX},

  // Low-level
  {"encode-i32"     , SYM_ENCODE_I32},
  {"encode-u32"     , SYM_ENCODE_U32},
  {"encode-float"   , SYM_ENCODE_FLOAT},
  {"decode"         , SYM_DECODE},

  {"is-fundamental" , SYM_IS_FUNDAMENTAL},

  // aliases
  {"first"          , SYM_CAR},
  {"rest"           , SYM_CDR},
  {"fn"             , SYM_LAMBDA},
  {"def"            , SYM_DEFINE}

};

static lbm_uint *symlist = NULL;
static lbm_uint next_symbol_id = RUNTIME_SYMBOLS_START;
static lbm_uint next_extension_symbol_id = EXTENSION_SYMBOLS_START;
static lbm_uint next_variable_symbol_id = VARIABLE_SYMBOLS_START;

static lbm_uint symbol_table_size_list = 0;
static lbm_uint symbol_table_size_strings = 0;

int lbm_symrepr_init(void) {
  symlist = NULL;
  next_symbol_id = RUNTIME_SYMBOLS_START;
  next_extension_symbol_id = EXTENSION_SYMBOLS_START;
  next_variable_symbol_id = VARIABLE_SYMBOLS_START;
  symbol_table_size_list = 0;
  symbol_table_size_strings = 0;
  return 1;
}

void lbm_symrepr_name_iterator(symrepr_name_iterator_fun f) {

  lbm_uint *curr = symlist;
  while (curr) {
    f((const char *)curr[NAME]);
    curr = (lbm_uint *)curr[NEXT];
  }
}

const char *lookup_symrepr_name_memory(lbm_uint id) {

  lbm_uint *curr = symlist;
  while (curr) {
    if (id == curr[ID]) {
      return (const char *)curr[NAME];
    }
    curr = (lbm_uint*)curr[NEXT];
  }
  return NULL;
}

// Lookup symbol name given a symbol id
const char *lbm_get_name_by_symbol(lbm_uint id) {
  if (id < SPECIAL_SYMBOLS_END) {
    for (unsigned int i = 0; i < NUM_SPECIAL_SYMBOLS; i ++) {
      if (id == special_symbols[i].id) {
        return (special_symbols[i].name);
      }
    }
  }
  return lookup_symrepr_name_memory(id);
}

// Lookup symbol id given symbol name
int lbm_get_symbol_by_name(char *name, lbm_uint* id) {

  // loop through special symbols
  for (unsigned int i = 0; i < NUM_SPECIAL_SYMBOLS; i ++) {
    if (strcmp(name, special_symbols[i].name) == 0) {
      *id = special_symbols[i].id;
      return 1;
    }
  }

  lbm_uint *curr = symlist;
  while (curr) {
    char *str = (char*)curr[NAME];
    if (strcmp(name, str) == 0) {
      *id = curr[ID];
      return 1;
    }
    curr = (lbm_uint*)curr[NEXT];
  }
  return 0;
}

int lbm_add_symbol(char *name, lbm_uint* id) {
  size_t  n = 0;

  n = strlen(name) + 1;
  if (n == 1) return 0; // failure if empty symbol

  lbm_uint *m = lbm_memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  char *symbol_name_storage = NULL;
  lbm_uint alloc_size;
  if (n % sizeof(lbm_uint) == 0) {
    alloc_size = n/(sizeof(lbm_uint));
  } else {
    alloc_size = (n/(sizeof(lbm_uint))) + 1;
  }

  symbol_name_storage = (char *)lbm_memory_allocate(alloc_size);

  if (symbol_name_storage == NULL) {
    lbm_memory_free(m);
    return 0;
  }

  symbol_table_size_list += 3;
  symbol_table_size_strings += alloc_size;

  strcpy(symbol_name_storage, name);

  m[NAME] = (lbm_uint)symbol_name_storage;

  if (symlist == NULL) {
    m[NEXT] = (lbm_uint) NULL;
    symlist = m;
  } else {
    m[NEXT] = (lbm_uint) symlist;
    symlist = m;
  }
  m[ID] = next_symbol_id++;
  *id = m[ID];
  return 1;
}

int lbm_str_to_symbol(char *name, lbm_uint *sym_id) {
  if (lbm_get_symbol_by_name(name, &sym_id))
    return 1;
  else if (lbm_add_symbol(name, &sym_id))
    return 1;
  return 0;
}

int lbm_add_variable_symbol(char *name, lbm_uint* id) {
  if (strlen(name) == 0) return 0; // failure if empty symbol
  if (next_variable_symbol_id >= VARIABLE_SYMBOLS_END) return 0;
  size_t  n = 0;

  n = strlen(name) + 1;
  if (n == 1) return 0; // failure if empty symbol

  lbm_uint *m = lbm_memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  char *symbol_name_storage = NULL;
  lbm_uint alloc_size;
  if (n % sizeof(lbm_uint) == 0) {
    alloc_size = n/(sizeof(lbm_uint));
  } else {
    alloc_size = (n/(sizeof(lbm_uint))) + 1;
  }

  symbol_name_storage = (char *)lbm_memory_allocate(alloc_size);

  if (symbol_name_storage == NULL) {
    lbm_memory_free(m);
    return 0;
  }

  symbol_table_size_list += 3;
  symbol_table_size_strings += alloc_size;

  strcpy(symbol_name_storage, name);

  m[NAME] = (lbm_uint)symbol_name_storage;

  if (symlist == NULL) {
    m[NEXT] = (lbm_uint) NULL;
    symlist = m;
  } else {
    m[NEXT] = (lbm_uint) symlist;
    symlist = m;
  }
  m[ID] = next_variable_symbol_id++;
  *id = m[ID];
  return 1;
}

int lbm_add_variable_symbol_const(char *name, lbm_uint* id) {
  if (strlen(name) == 0) return 0; // failure if empty symbol
  if (next_variable_symbol_id >= VARIABLE_SYMBOLS_END) return 0;
  size_t  n = 0;

  n = strlen(name) + 1;
  if (n == 1) return 0; // failure if empty symbol

  lbm_uint *m = lbm_memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  symbol_table_size_list += 3;

  m[NAME] = (lbm_uint)name;

  if (symlist == NULL) {
    m[NEXT] = (lbm_uint) NULL;
    symlist = m;
  } else {
    m[NEXT] = (lbm_uint) symlist;
    symlist = m;
  }
  m[ID] = next_variable_symbol_id++;
  *id = m[ID];
  return 1;
}


int lbm_add_symbol_const(char *name, lbm_uint* id) {
  if (strlen(name) == 0) return 0; // failure if empty symbol

  lbm_uint *m = lbm_memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  symbol_table_size_list += 3;

  m[NAME] = (lbm_uint)name;

  if (symlist == NULL) {
    m[NEXT] = (lbm_uint) NULL;
    symlist = m;
  } else {
    m[NEXT] = (lbm_uint) symlist;
    symlist = m;
  }
  m[ID] = next_symbol_id++;
  *id = m[ID];
  return 1;
}

int lbm_add_extension_symbol(char *name, lbm_uint* id) {
  size_t  n = 0;
  n = strlen(name) + 1;

  if (n == 1) return 0; // failure if empty symbol
  if (next_extension_symbol_id >= EXTENSION_SYMBOLS_END) return 0;

  lbm_uint *m = lbm_memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  char *symbol_name_storage = NULL;
  lbm_uint alloc_size;
  if (n % sizeof(lbm_uint) == 0) {
    alloc_size = n/(sizeof(lbm_uint));
  } else {
    alloc_size = (n/(sizeof(lbm_uint))) + 1;
  }

  symbol_name_storage = (char *)lbm_memory_allocate(alloc_size);

  if (symbol_name_storage == NULL) {
    lbm_memory_free(m);
    return 0;
  }

  symbol_table_size_list += 3;
  symbol_table_size_strings += alloc_size;

  strcpy(symbol_name_storage, name);

  m[NAME] = (lbm_uint)symbol_name_storage;

  if (symlist == NULL) {
    m[NEXT] = (lbm_uint) NULL;
    symlist = m;
  } else {
    m[NEXT] = (lbm_uint) symlist;
    symlist = m;
  }
  m[ID] = next_extension_symbol_id++;
  *id = m[ID];
  return 1;
}

int lbm_add_extension_symbol_const(char *name, lbm_uint* id) {
  if (strlen(name) == 0) return 0; // failure if empty symbol
  if (next_extension_symbol_id >= EXTENSION_SYMBOLS_END) return 0;

  lbm_uint *m = lbm_memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  symbol_table_size_list += 3;

  m[NAME] = (lbm_uint)name;

  if (symlist == NULL) {
    m[NEXT] = (lbm_uint) NULL;
    symlist = m;
  } else {
    m[NEXT] = (lbm_uint) symlist;
    symlist = m;
  }
  m[ID] = next_extension_symbol_id++;
  *id = m[ID];
  return 1;
}

lbm_uint lbm_get_symbol_table_size(void) {
  return (symbol_table_size_list +
          symbol_table_size_strings) * sizeof(lbm_uint);
}

lbm_uint lbm_get_symbol_table_size_names(void) {
  return symbol_table_size_strings * sizeof(lbm_uint);
}

int lbm_get_num_variables(void) {
  return (int)next_variable_symbol_id - VARIABLE_SYMBOLS_START;
}
