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

     - There is a 16 bit hash table (65521 buckets (65536 - 15)) 
     - and 12 additional bits (bucket depth 4096)

 */

#define HASHTAB_SIZE 65521    
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
#define DEF_REPR_RERROR    8  // READ ERROR
#define DEF_REPR_TERROR    9  // TYPE ERROR
#define DEF_REPR_EERROR    10  // EVAL ERROR


static uint32_t hash_string(char *str); 

static uint32_t def_repr[11]; 

typedef struct s_name_mapping {
  uint32_t key; // hash including collision id //
  char* name;
  struct s_name_mapping* next;
} name_mapping_t; 
  
name_mapping_t **name_table = NULL;

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
  res &= symrepr_addsym("rerror" , &def_repr[DEF_REPR_RERROR]);
  res &= symrepr_addsym("terror" , &def_repr[DEF_REPR_TERROR]);
  res &= symrepr_addsym("eerror" , &def_repr[DEF_REPR_EERROR]);

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

int symrepr_init(void) {
  name_table = (name_mapping_t**)malloc(HASHTAB_SIZE * sizeof(name_mapping_t*));
  if (!name_table) return 0; 
  memset(name_table, 0, HASHTAB_SIZE * sizeof(name_mapping_t*));
  return add_default_symbols();
}

int symrepr_addsym(char *name, uint32_t* id) {
  size_t   n = 0;

  if(strlen(name) == 0) return 0; //return failure if empty symbol
  
  uint32_t hash = hash_string(name);
  
  if (hash >= HASHTAB_SIZE) /* impossible */ return 0;

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

    // If new ID would be too big return failure 
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
}

int symrepr_lookup(char *name, uint32_t* id) {

  int r = 0; 
  uint32_t hash = hash_string(name);
  
  if (name_table[hash] == NULL) return 0;

  name_mapping_t *head = name_table[hash];
 
  while (head) {
    if (strcmp(head->name, name) == 0) {
      r = 1;
      *id = head->key; 
    }
    head = head->next; 
  }
  return r; 
}

char *symrepr_lookup_name(uint32_t id) {
  uint32_t hash = id & (uint32_t)0x0000FFFF; /*extract index*/
  if (name_table[hash]) {
    name_mapping_t *head = name_table[hash];

    while (head) {
      if (head->key == id) {
	return head->name;
      }
      head = head->next; 
    }
  }
  return NULL; 
}

void symrepr_print(void) {
  int i;

  for (i = 0; i < HASHTAB_SIZE; i ++) {
    if (name_table[i] != NULL) {
      name_mapping_t *head = name_table[i]; 
      while (head) {
	printf("%d : %s\n", head->key,head->name); 
	head = head->next; 
      }
    }
  }
}

void symrepr_del(void) {
  int i;

  if(!name_table) return; 
  
  for (i = 0; i < HASHTAB_SIZE; i ++) {
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
}

uint32_t small_primes[SMALL_PRIMES] =
  /* {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31}; */ 
  /* {37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79}; */
  {359, 419, 463, 523, 593, 643, 701, 761, 827, 883, 953}; 
uint32_t hash_string(char *str) {

  uint32_t r = 1;
  size_t n = strlen(str); 
  
  for (int i = 0; i < n; i ++) {
    uint32_t sp = small_primes[i % SMALL_PRIMES]; 
    r = (r + (sp * str[i])) % HASHTAB_SIZE; 
  }

  return r; 
}



