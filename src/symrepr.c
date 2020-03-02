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

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

#include "symrepr.h"

/*
   Name -> 28bit integer mapping that is (I hope) somewhat
   efficient in both directions. (it's a shot from the hip... )

   In the best case, looking up the 28bit id has cost relative to
   length of name. In the worst, it has that cost + cost of a walk
   over linked list (that can at most be 4095 links long).

   In the best case, looking up a name given a 28bit id has constant cost.
   In the worst case it has the same linked list traversal.

   ## overhauling Symbol representation

     - Remove the gensym functionality
     - 65533 buckets of depth 4096
     - bucket 65534 contains up to 4096 hardcoded symbols.
       These will be used for the following symbols and such:
       (nil, quote, true, if, lambda, closure, let, progn, +,-,*, /, mod, sin, cos, etc etc).

       
   ## 
     4096 - 12Bit
            16Bit
            28
 */
#define HASHTAB_MALLOC_SIZE 0x10000
#define HASHTAB_SIZE        0xFFFF
#define BUCKET_DEPTH        4096

//static UINT def_repr[14];

#define SMALL_PRIMES        33
UINT small_primes[SMALL_PRIMES] =
  {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31,
   37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79,
   359, 419, 463, 523, 593, 643, 701, 761, 827, 883, 953};

static UINT hash_string(char *str, UINT modulo);
static bool symrepr_addspecial(char *name, UINT spec_id);

typedef struct s_name_mapping {
  UINT key; /* hash including collision id */
  char* name;
  struct s_name_mapping* next;
} name_mapping_t;

#ifdef TINY_SYMTAB
typedef struct s_name_list {
  UINT key; /* 16 bit hash part */
  name_mapping_t* map;
  struct s_name_list* next;
}name_list_t;

int name_list_is_empty(name_list_t *l) {
  return (l == NULL);
}

name_mapping_t* name_list_get_mappings(name_list_t *l, UINT key) {

  /* do not care about anything but the low 16 bits */
  UINT key_ = key & 0xFFFF;

  name_list_t *curr = l;

  while (curr != NULL) {
    if (curr->key == key_) {
      return curr->map;
    }
    curr = curr->next;
  }
  return NULL;
}

int name_mapping_contains(name_mapping_t *map, UINT key) {

  while (map != NULL) {
    if (map->key == key) return 1;
    map = map->next;
  }
  return 0;
}

name_list_t *name_list = NULL;
#else
name_mapping_t **name_table = NULL;
#endif

bool add_default_symbols() {
  bool res = true;
  res = res && symrepr_addspecial("nil"        , DEF_REPR_NIL);
  res = res && symrepr_addspecial("quote"      , DEF_REPR_QUOTE);
  res = res && symrepr_addspecial("t"          , DEF_REPR_TRUE);
  res = res && symrepr_addspecial("if"         , DEF_REPR_IF);
  res = res && symrepr_addspecial("lambda"     , DEF_REPR_LAMBDA);
  res = res && symrepr_addspecial("closure"    , DEF_REPR_CLOSURE);
  res = res && symrepr_addspecial("let"        , DEF_REPR_LET);
  res = res && symrepr_addspecial("define"     , DEF_REPR_DEFINE);
  res = res && symrepr_addspecial("progn"      , DEF_REPR_PROGN);
  
  // Special symbols with unparseable names
  res = res && symrepr_addspecial("read_error"       , DEF_REPR_RERROR);
  res = res && symrepr_addspecial("type_error"       , DEF_REPR_TERROR);
  res = res && symrepr_addspecial("eval_error"       , DEF_REPR_EERROR);
  res = res && symrepr_addspecial("out_of_memory"    , DEF_REPR_MERROR);
  res = res && symrepr_addspecial("fatal_error"      , DEF_REPR_FATAL_ERROR);
  res = res && symrepr_addspecial("division_by_zero" , DEF_REPR_DIVZERO);
  res = res && symrepr_addspecial("sym_array"        , DEF_REPR_ARRAY_TYPE);
  res = res && symrepr_addspecial("sym_boxed_i"      , DEF_REPR_BOXED_I_TYPE);
  res = res && symrepr_addspecial("sym_boxed_u"      , DEF_REPR_BOXED_U_TYPE);
  res = res && symrepr_addspecial("sym_boxed_f"      , DEF_REPR_BOXED_F_TYPE);
  res = res && symrepr_addspecial("sym_ref"          , DEF_REPR_REF_TYPE);
  res = res && symrepr_addspecial("sym_recovered"    , DEF_REPR_RECOVERED);
  res = res && symrepr_addspecial("sym_bytecode"     , DEF_REPR_BYTECODE_TYPE);
  res = res && symrepr_addspecial("sym_nonsense"     , DEF_REPR_NONSENSE);

  // special symbols with parseable names
  res = res && symrepr_addspecial("type-list"        , DEF_REPR_TYPE_LIST);
  res = res && symrepr_addspecial("type-i28"         , DEF_REPR_TYPE_I28);
  res = res && symrepr_addspecial("type-u28"         , DEF_REPR_TYPE_U28);
  res = res && symrepr_addspecial("type-float"       , DEF_REPR_TYPE_FLOAT);
  res = res && symrepr_addspecial("type-i32"         , DEF_REPR_TYPE_I32);
  res = res && symrepr_addspecial("type-u32"         , DEF_REPR_TYPE_U32);
  res = res && symrepr_addspecial("type-array"       , DEF_REPR_TYPE_ARRAY);
  res = res && symrepr_addspecial("type-symbol"      , DEF_REPR_TYPE_SYMBOL);
  res = res && symrepr_addspecial("type-char"        , DEF_REPR_TYPE_CHAR);

  
  res = res && symrepr_addspecial("+", SYM_ADD);
  res = res && symrepr_addspecial("-", SYM_SUB);
  res = res && symrepr_addspecial("*", SYM_MUL);
  res = res && symrepr_addspecial("/", SYM_DIV);
  res = res && symrepr_addspecial("mod", SYM_MOD);
  res = res && symrepr_addspecial("=", SYM_EQ);
  res = res && symrepr_addspecial("<", SYM_LT);
  res = res && symrepr_addspecial(">", SYM_GT);
  res = res && symrepr_addspecial("eval", SYM_EVAL);
  res = res && symrepr_addspecial("num-eq", SYM_NUMEQ);
  res = res && symrepr_addspecial("car", SYM_CAR);
  res = res && symrepr_addspecial("cdr", SYM_CDR);
  res = res && symrepr_addspecial("cons", SYM_CONS);
  res = res && symrepr_addspecial("list", SYM_LIST);

  res = res && symrepr_addspecial("array-read", SYM_ARRAY_READ);
  res = res && symrepr_addspecial("array-write", SYM_ARRAY_WRITE);
  res = res && symrepr_addspecial("array-create", SYM_ARRAY_CREATE);

  res = res && symrepr_addspecial("type-of", SYM_TYPE_OF);
  return res;
}

bool symrepr_init(void) {
#ifdef TINY_SYMTAB
  name_list = NULL; /* empty list of symbol names */
#else
  name_table = (name_mapping_t**)malloc(HASHTAB_MALLOC_SIZE * sizeof(name_mapping_t*));
  if (!name_table) return false;
  memset(name_table, 0, HASHTAB_MALLOC_SIZE * sizeof(name_mapping_t*));
#endif
  return add_default_symbols();
}


bool symrepr_addspecial(char *name, UINT spec_id) {

  size_t   n = 0;
  UINT hash = 0xFFFF;
  UINT key  = spec_id;
  n = strlen(name) + 1;

  if (n == 1) return false; /* failure if empty symbol */

#ifdef TINY_SYMTAB
  if (name_list == NULL) {
    name_list = (name_list_t*)malloc(sizeof(name_list_t));
    if (name_list == NULL) return false;
    name_list->next = NULL;
    name_list->key = hash;
    name_list->map = (name_mapping_t*)malloc(sizeof(name_mapping_t));
    if (name_list->map == NULL) return false;
    name_list->map->key = key;
    name_list->map->next = NULL;
    name_list->map->name = (char*)malloc(n);
    if (name_list->map->name == NULL) return false;
    strncpy(name_list->map->name, name, n);
  } else {
    UINT t_id;
    if (symrepr_lookup(name, &t_id)) {
      return false;
    }
    
    name_mapping_t *head = name_list_get_mappings(name_list,hash);
    if (head == NULL) {
      name_list_t *new_entry = (name_list_t*)malloc(sizeof(name_list_t));
      if (new_entry == NULL) return 0;
      new_entry->next = NULL;
      new_entry->key = hash;
      new_entry->map = (name_mapping_t*)malloc(sizeof(name_mapping_t));
      if (new_entry->map == NULL) return 0;
      new_entry->map->key = key;
      new_entry->map->next = NULL;
      new_entry->map->name = (char*)malloc(n);
      if (new_entry->map->name == NULL) return 0;
      strncpy(new_entry->map->name, name, n);

      /* Update global list */
      new_entry->next = name_list;
      name_list = new_entry;
    } else {
      name_mapping_t *new_mapping = (name_mapping_t*)malloc(sizeof(name_mapping_t));
      new_mapping->next = NULL;
      new_mapping->key  = key;
      new_mapping->name = (char*)malloc(n);
      if (new_mapping->name == NULL) return false;
      strncpy(new_mapping->name, name, n);
      while (head->next != NULL) head = head->next;
      head->next = new_mapping;
    }
  }
      
      
#else
  if (name_table[hash] == NULL){
    name_table[hash] = (name_mapping_t*)malloc(sizeof(name_mapping_t));
    name_table[hash]->key = key;
    name_table[hash]->name = (char*)malloc(n);
    strncpy(name_table[hash]->name, name, n);
    name_table[hash]->next = NULL;
  } else {
    UINT t_id;
    if (symrepr_lookup(name, &t_id))
      return false;

    /* collision */
    name_mapping_t *head = name_table[hash];

    name_table[hash] = (name_mapping_t*)malloc(sizeof(name_mapping_t));
    name_table[hash]->key = key;
    name_table[hash]->name = (char*)malloc(n);
    strncpy(name_table[hash]->name, name, n);
    name_table[hash]->next = head;
  }
#endif
  return true;
}

int symrepr_addsym(char *name, UINT* id) {
  size_t   n = 0;

  n = strlen(name) + 1;
  if (n == 1) return 0; /* failure if empty symbol */

  UINT hash = hash_string(name, HASHTAB_SIZE);

  if (hash >= HASHTAB_SIZE) /* impossible */ return 0;

#ifdef TINY_SYMTAB
  /* If the symbol name_list is empty */
  if (name_list == NULL) {
    name_list = (name_list_t*)malloc(sizeof(name_list_t));
    if (name_list == NULL) return 0;
    name_list->next = NULL;
    name_list->key = hash;
    name_list->map = (name_mapping_t*)malloc(sizeof(name_mapping_t));
    if (name_list->map == NULL) return 0;
    name_list->map->key = hash;
    name_list->map->next = NULL;
    name_list->map->name = (char*)malloc(n);
    if (name_list->map->name == NULL) return 0;
    strcpy(name_list->map->name, name);

    if (id != NULL) *id = hash;

  } else { /* there is at least one entry in the name list */
    name_mapping_t* tmp = name_list_get_mappings(name_list, hash);

    if (tmp == NULL) {
      /* There is no entry for this hash, just append it to name_list */
      name_list_t *new_entry = (name_list_t*)malloc(sizeof(name_list_t));
      if (new_entry == NULL) return 0;
      new_entry->next = NULL;
      new_entry->key = hash;
      new_entry->map = (name_mapping_t*)malloc(sizeof(name_mapping_t));
      if (new_entry->map == NULL) return 0;
      new_entry->map->key = hash;
      new_entry->map->next = NULL;
      new_entry->map->name = (char*)malloc(n);
      if (new_entry->map->name == NULL) return 0;
      strcpy(new_entry->map->name, name);

      /* Update global list */
      new_entry->next = name_list;
      name_list = new_entry;

      if (id != NULL) *id = hash;
    } else {
      /* There are entries for this hash */
      /* TODO add new entry */
      UINT max_12bit = 0;

      while (tmp->next) {
	if ((tmp->key >> 16) > max_12bit) max_12bit = tmp->key >> 16;
	tmp = tmp->next;
      }

      /* ready to add a new entry if there is room in the 12 bits */
      if (++max_12bit > 4095) return 0;

      tmp->next = (name_mapping_t*)malloc(sizeof(name_mapping_t));
      if (tmp->next == NULL) return 0;

      UINT new_key = hash | (max_12bit << 16);

      tmp->next->next = NULL;
      tmp->next->key  = new_key;
      tmp->next->name = (char*)malloc(n);
      if (tmp->next->name == NULL) return 0;
      strncpy(tmp->next->name, name, n);

      if (id != NULL) *id = new_key;
    }

  }

  return 1;
#else

  if (name_table[hash] == NULL){
    name_table[hash] = (name_mapping_t*)malloc(sizeof(name_mapping_t));
    name_table[hash]->key = hash;
    if (id != NULL) *id = hash;
    n = strlen(name) + 1;
    name_table[hash]->name = (char*)malloc(n);
    strncpy(name_table[hash]->name, name, n);
    name_table[hash]->next = NULL;
  } else {
    UINT t_id;
    if (symrepr_lookup(name, &t_id)) {
      /* name already in table */

      if (id != NULL) *id = t_id;

      return 0; /* set id, but return failure */
    }

    /* collision */
    name_mapping_t *head = name_table[hash];
    UINT hkey_id = head->key & 0xFFFF0000 ;

    /* If new ID would be too big return failure */
    if ((hkey_id >> 16) >= (BUCKET_DEPTH - 1)) {
      return 0;
    }

    /* problem if hkey_id = 0xFFFF0000 */
    name_table[hash] = (name_mapping_t*)malloc(sizeof(name_mapping_t));
    name_table[hash]->key = hash + (hkey_id + (1 << 16));
    if (id != NULL) *id = hash + (hkey_id + (1 << 16));
    n = strlen(name) + 1;
    name_table[hash]->name = (char*)malloc(n);
    strncpy(name_table[hash]->name, name, n);
    name_table[hash]->next = head;
  }
  return 1;
#endif
}



int symrepr_lookup(char *name, UINT* id) {
  name_mapping_t *head;
  UINT hash = 0xFFFF;
  /* check if fixed_id symbol */
#ifdef TINY_SYMTAB
  head = name_list_get_mappings(name_list, hash);
#else
  head = name_table[hash];
#endif
  if (head != NULL) {
    while (head) {
      if (strcmp(head->name, name) == 0) {
	*id = head->key;
	return 1;
      }
      head = head->next;
    }
  }

  hash = hash_string(name, HASHTAB_SIZE);
#ifdef TINY_SYMTAB
  head = name_list_get_mappings(name_list, hash);
#else
  head = name_table[hash];
#endif
  if (head == NULL) return 0;

  while (head) {
    if (strcmp(head->name, name) == 0) {
      *id = head->key;
      return 1;
    }
    head = head->next;
  }
  return 0;
}


char *symrepr_lookup_name(UINT id) {

  name_mapping_t *head = NULL;
  UINT hash = id & (UINT)0x0000FFFF; /*extract index*/
#ifdef TINY_SYMTAB
  head = name_list_get_mappings(name_list, hash);
#else
  head = name_table[hash];
#endif

  if (head == NULL) return NULL;

  while (head) {
    if (head->key == id) {
      return head->name;
    }
    head = head->next;
  }
  return NULL;
}

void symrepr_del(void) {
#ifdef TINY_SYMTAB
  name_list_t *curr = name_list;

  while (curr) {
    name_mapping_t* head = curr->map;
    name_list_t* t0 = curr->next;
    while (head) {
      name_mapping_t* t1 = head->next;
      free(head->name);
      free(head);
      head = t1;
    }
    free(curr);
    curr = t0;
  }
#else
  int i;

  if(!name_table) return;

  for (i = 0; i < HASHTAB_MALLOC_SIZE; i ++) {
    if (name_table[i]) {
      name_mapping_t *head = name_table[i];
      name_mapping_t *next;
      while (head) {
	next = head->next;
	free(head->name);
	free(head);
	head = next;
      }
    }
  }
#endif
}

UINT hash_string(char *str, UINT modulo) {

  UINT r = 1;
  size_t n = strlen(str);

  for (UINT i = 0; i < n; i ++) {
    UINT sp = small_primes[i % SMALL_PRIMES];
    UINT v = (UINT)str[i];
    r = (r + (sp * v)) % modulo;
  }

  return r;
}
