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

   Replace with something more thought through (or looked up) later.

     - There is a 16 bit hash table (only 49999 buckets targeted by hash function) 
     - and 12 additional bits (bucket depth 4096)
     - The buckets numbered 49999 - 65534 are used by gensyming new symbols.
     - The last bucket 65535 is used for internal implementation needs.
       There are 4096 symbols that cannot collide with any user specified
       or gensymed symbol.
 */

#define HASHTAB_MALLOC_SIZE 65535 /* 65535 */
#define HASHTAB_SIZE 49999 /* 65521 */
#define BUCKET_DEPTH 4096
#define SMALL_PRIMES 11

#define DEF_REPR_NIL       0
#define DEF_REPR_QUOTE     1
#define DEF_REPR_TRUE      2
#define DEF_REPR_COND      3
#define DEF_REPR_IF        4
#define DEF_REPR_LAMBDA    5
#define DEF_REPR_CLOSURE   6
#define DEF_REPR_LET       7
#define DEF_REPR_RERROR    8   /* READ ERROR */
#define DEF_REPR_TERROR    9   /* TYPE ERROR */
#define DEF_REPR_EERROR    10  /* EVAL ERROR */
#define DEF_REPR_MERROR    11
#define DEF_REPR_DEFINE    12

static uint32_t gensym_next = HASHTAB_SIZE;

static uint32_t hash_string(char *str, uint32_t modulo);

static uint32_t def_repr[13];

typedef struct s_name_mapping {
  uint32_t key; /* hash including collision id */
  char* name;
  struct s_name_mapping* next;
} name_mapping_t;

#ifdef TINY_SYMTAB
typedef struct s_name_list {
  uint32_t key; /* 16 bit hash part */
  name_mapping_t* map;
  struct s_name_list* next;
}name_list_t;

int name_list_is_empty(name_list_t *l) {
  return (l == NULL);
}

name_mapping_t* name_list_get_mappings(name_list_t *l, uint32_t key) {

  /* do not care about anothing but the low 16 bits */
  uint32_t key_ = key & 0xFFFF;

  name_list_t *curr = l;
  
  while (curr != NULL) {
    if (curr->key == key_) {
      return curr->map; 
    }
    curr = curr->next;
  }
  return NULL; 
}

int name_mapping_contains(name_mapping_t *map, uint32_t key) {

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

int add_default_symbols(void) {
  int res = 1;

  res &= symrepr_addsym("nil"    , &def_repr[DEF_REPR_NIL]);
  res &= symrepr_addsym("quote"  , &def_repr[DEF_REPR_QUOTE]);
  res &= symrepr_addsym("t"      , &def_repr[DEF_REPR_TRUE]);
  res &= symrepr_addsym("cond"   , &def_repr[DEF_REPR_COND]);
  res &= symrepr_addsym("if"     , &def_repr[DEF_REPR_IF]);
  res &= symrepr_addsym("lambda" , &def_repr[DEF_REPR_LAMBDA]);
  res &= symrepr_addsym("closure", &def_repr[DEF_REPR_CLOSURE]);
  res &= symrepr_addsym("let"    , &def_repr[DEF_REPR_LET]);
  res &= symrepr_addsym("read_error" , &def_repr[DEF_REPR_RERROR]);
  res &= symrepr_addsym("type_error" , &def_repr[DEF_REPR_TERROR]);
  res &= symrepr_addsym("eval_error" , &def_repr[DEF_REPR_EERROR]);
  res &= symrepr_addsym("out_of_memory_error" , &def_repr[DEF_REPR_MERROR]);
  res &= symrepr_addsym("define" , &def_repr[DEF_REPR_DEFINE]);

  return res;
}

uint32_t symrepr_nil(void)     { return def_repr[DEF_REPR_NIL]; }
uint32_t symrepr_quote(void)   { return def_repr[DEF_REPR_QUOTE]; }
uint32_t symrepr_true(void)    { return def_repr[DEF_REPR_TRUE]; }
uint32_t symrepr_cond(void)    { return def_repr[DEF_REPR_COND]; }
uint32_t symrepr_if(void)      { return def_repr[DEF_REPR_IF]; }
uint32_t symrepr_lambda(void)  { return def_repr[DEF_REPR_LAMBDA]; }
uint32_t symrepr_closure(void) { return def_repr[DEF_REPR_CLOSURE]; }
uint32_t symrepr_let(void)     { return def_repr[DEF_REPR_LET]; }
uint32_t symrepr_rerror(void)  { return def_repr[DEF_REPR_RERROR]; }
uint32_t symrepr_terror(void)  { return def_repr[DEF_REPR_TERROR]; }
uint32_t symrepr_eerror(void)  { return def_repr[DEF_REPR_EERROR]; }
uint32_t symrepr_merror(void)  { return def_repr[DEF_REPR_MERROR]; }
uint32_t symrepr_define(void)  { return def_repr[DEF_REPR_DEFINE]; }

int symrepr_init(void) {
#ifdef TINY_SYMTAB
  name_list = NULL; /* empty list of symbol names */
#else
  name_table = (name_mapping_t**)malloc(HASHTAB_MALLOC_SIZE * sizeof(name_mapping_t*));
  if (!name_table) return 0;
  memset(name_table, 0, HASHTAB_MALLOC_SIZE * sizeof(name_mapping_t*));
#endif
  return add_default_symbols();
}

int gensym(uint32_t *res) {
#ifdef TINY_SYMTAB
  return 0;
#else
  uint32_t v = gensym_next;
  int n;
  char gensym_name[1024];
  memset(gensym_name,0,1024);

  if(name_table[v] == NULL && v < HASHTAB_MALLOC_SIZE) {
    name_table[v] = (name_mapping_t*)malloc(sizeof(name_mapping_t));
    name_table[v]->key = v;
    n = snprintf(gensym_name,1024,"gensym_%"PRIu32"", v);
    name_table[v]->name = (char*)malloc(n+1);
    memset(name_table[v]->name, 0, n+1);
    strncpy(name_table[v]->name, gensym_name, n);
    name_table[v]->next = NULL;
    *res = v;
  } else {
    /* Gensym already added to this bucket */
    name_mapping_t *head = name_table[v];
    uint32_t hkey_id = head->key & 0xFFFF0000 ;

    /* If new ID would be too big return failure */
    if ((hkey_id >> 16) >= (BUCKET_DEPTH - 1)) {
      return 0;
    }

    /* problem if hkey_id = 0xFFFF0000 */
    name_table[v] = (name_mapping_t*)malloc(sizeof(name_mapping_t)); /* replace */
    name_table[v]->key = v + (hkey_id + (1 << 16));
    uint32_t v_prim = v + (hkey_id + (1 << 16));
    n = snprintf(gensym_name,1024,"gensym_%"PRIu32"", v_prim);
    name_table[v]->name = (char*)malloc(n);
    memset(name_table[v]->name, 0, n+1);
    strncpy(name_table[v]->name, gensym_name, n);
    name_table[v]->next = head;
    *res = v_prim;
  }

  gensym_next++;
  if (gensym_next >= (HASHTAB_MALLOC_SIZE-1)) gensym_next = HASHTAB_SIZE;
  return 1;
#endif
}

int symrepr_addsym(char *name, uint32_t* id) {
  size_t   n = 0;

  n = strlen(name) + 1;
  if (n == 1) return 0; /* failure if empty symbol */
  
  uint32_t hash = hash_string(name, HASHTAB_SIZE);

  if (hash >= HASHTAB_SIZE) /* impossible */ return 0;
  
#ifdef TINY_SYMTAB

  /* If the symbol name_list is empty */ 
  if (name_list == NULL) {
    name_list = (name_list_t*)malloc(sizeof(name_list_t));
    if (name_list == NULL) return 0;
    name_list->key = hash;
    name_list->map = (name_mapping_t*)malloc(sizeof(name_mapping_t));
    if (name_list->map == NULL) return 0;
    name_list->map->key = hash;
    name_list->map->next = NULL;
    name_list->map->name = (char*)malloc(n);
    if (name_list->map->name == NULL) return 0;
    strncpy(name_list->map->name, name, n);

    if (id != NULL) *id = hash;
    
  } else { /* there is at least one entry in the name list */ 

    name_mapping_t* tmp = name_list_get_mappings(name_list, hash);

    if (tmp == NULL) {
      /* There is no entry for this hash, just append it to name_list */
      name_list_t *new_entry = (name_list_t*)malloc(sizeof(name_list_t));
      if (new_entry == NULL) return 0;
      new_entry->key = hash;
      new_entry->map = (name_mapping_t*)malloc(sizeof(name_mapping_t));
      if (new_entry->map == NULL) return 0;
      new_entry->map->key = hash;
      new_entry->map->next = NULL;
      new_entry->map->name = (char*)malloc(n);
      if (new_entry->map->name == NULL) return 0;
      strncpy(new_entry->map->name, name, n);

      /* Update global list */ 
      new_entry->next = name_list;
      name_list = new_entry;

      if (id != NULL) *id = hash;
    } else {
      /* There are entries for this hash */
      /* TODO add new entry */
      uint32_t max_12bit = 0;

      while (tmp->next) {
	if ((tmp->key >> 16) > max_12bit) max_12bit = tmp->key >> 16;
	tmp = tmp->next;
      }

      /* ready to add a new entry if there is room in the 12 bits */
      if (++max_12bit > 4095) return 0;

      tmp->next = (name_mapping_t*)malloc(sizeof(name_mapping_t));
      if (tmp->next == NULL) return 0;

      uint32_t new_key = hash | (max_12bit << 16); 
      
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
    uint32_t t_id;
    if (symrepr_lookup(name, &t_id)) {
      /* name already in table */

      if (id != NULL) *id = t_id;

      return 0; /* set id, but return failure */
    }

    /* collision */
    name_mapping_t *head = name_table[hash];
    uint32_t hkey_id = head->key & 0xFFFF0000 ;

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


int symrepr_lookup(char *name, uint32_t* id) {
  
  name_mapping_t *head;
  uint32_t hash = hash_string(name, HASHTAB_SIZE);
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


char *symrepr_lookup_name(uint32_t id) {

  name_mapping_t *head = NULL;
  uint32_t hash = id & (uint32_t)0x0000FFFF; /*extract index*/
  if(hash == 65535) return "special_symbol";
  
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

void symrepr_print(void) {
#ifdef TINY_SYMTAB
  name_list_t *curr = name_list;
  
  while (curr) {
    name_mapping_t *head = curr->map;
    printf("--# Bucket: %"PRIu32"\n", head->key);
    while (head) {
      printf("%"PRIu32" : %"PRIx32" : %s\n", head->key, head->key, head->name);
      head = head->next;
    }
    curr = curr->next;
  }
#else
  int i;
  for (i = 0; i < HASHTAB_MALLOC_SIZE; i ++) {
    if (name_table[i] != NULL) {
      name_mapping_t *head = name_table[i];
      while (head) {
	printf("%"PRIu32" : %"PRIx32" : %s\n", head->key, head->key, head->name);
	head = head->next;
      }
    }
  }
#endif
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

uint32_t small_primes[SMALL_PRIMES] =
  /* {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31}; */
  /* {37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79}; */
  {359, 419, 463, 523, 593, 643, 701, 761, 827, 883, 953};
uint32_t hash_string(char *str, uint32_t modulo) {

  uint32_t r = 1;
  size_t n = strlen(str);

  for (int i = 0; i < n; i ++) {
    uint32_t sp = small_primes[i % SMALL_PRIMES];
    r = (r + (sp * str[i])) % modulo; 
  }

  return r;
}

