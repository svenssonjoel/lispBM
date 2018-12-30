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

#include <stdio.h>
#include <stdint.h>

#include "heap.h"
#include "symrepr.h"
#ifdef VISUALIZE_HEAP
#include "heap_vis.h"
#endif


uint32_t global_env;

static cons_t*      heap = NULL;
static uint32_t     heap_base;
static heap_state_t heap_state;

static uint32_t     SYMBOL_NIL;

// ref_cell: returns a reference to the cell addressed by bits 3 - 26
//           Assumes user has checked that IS_PTR was set
cons_t* ref_cell(uint32_t addr) {
  return (cons_t*)(heap_base + (addr & PTR_VAL_MASK));
}

static uint32_t read_car(cons_t *cell) {
  return cell->car;
}

static uint32_t read_cdr(cons_t *cell) {
  return cell->cdr;
}

static void set_car_(cons_t *cell, uint32_t v) {
  cell->car = v;
}

static void set_cdr_(cons_t *cell, uint32_t v) {
  cell->cdr = v;
}

static void set_gc_mark(cons_t *cell) {
  uint32_t cdr = read_cdr(cell);
  set_cdr_(cell, cdr | GC_MARKED);
}

static void clr_gc_mark(cons_t *cell) {
  uint32_t cdr = read_cdr(cell);
  set_cdr_(cell, cdr & ~GC_MASK);
}

static uint32_t get_gc_mark(cons_t* cell) {
  uint32_t cdr = read_cdr(cell);
  return cdr & GC_MASK;
}

int generate_freelist(size_t num_cells) {
  size_t i = 0;

  if (!heap) return 0;

  heap_state.freelist = ENC_CONS_PTR(0);

  // Add all cells to free list
  for (i = 1; i < num_cells; i ++) {
    cons_t *t = ref_cell( ENC_CONS_PTR(i-1));
    set_car_(t, ENC_SYM(SYMBOL_NIL));    // all cars in free list are nil
    set_cdr_(t, ENC_CONS_PTR(i));
  }

  heap_state.freelist_last = ENC_CONS_PTR(num_cells-1);
  set_cdr_(ref_cell(heap_state.freelist_last), ENC_SYM(SYMBOL_NIL));

  if (read_cdr(ref_cell(heap_state.freelist_last)) == ENC_SYM(SYMBOL_NIL)) {
    return 1;
  }
  return 0;
}

int heap_init(size_t num_cells) {

  // retrieve nil symbol value f
  SYMBOL_NIL = symrepr_nil();

  // Allocate heap
  heap = (cons_t *)malloc(num_cells * sizeof(cons_t));

  if (!heap) return 0;

  heap_base = (uint32_t)heap;

  // Initialize heap statistics
  heap_state.heap_base    = heap_base;
  heap_state.heap_bytes   = (uint32_t)(num_cells * sizeof(cons_t));
  heap_state.heap_size    = num_cells;

  heap_state.num_alloc           = 0;
  heap_state.num_alloc_arrays    = 0;
  heap_state.gc_num              = 0;
  heap_state.gc_marked           = 0;
  heap_state.gc_recovered        = 0;
  heap_state.gc_recovered_arrays = 0;

  return (generate_freelist(num_cells));
}

void heap_del(void) {
  if (heap)
    free(heap);
}

uint32_t heap_num_free(void) {

  uint32_t count = 0;
  uint32_t curr = heap_state.freelist;

  while (TYPE_OF(curr) == PTR_TYPE_CONS) {
    curr = read_cdr(ref_cell(curr));
    count++;
  }
  // Prudence.
  if (!(TYPE_OF(curr) == VAL_TYPE_SYMBOL) &&
      (DEC_SYM(curr) == SYMBOL_NIL)){
    return 0;
  }
  return count;
}


uint32_t heap_allocate_cell(uint32_t ptr_type) {

  uint32_t res;

  if (! IS_PTR(heap_state.freelist)) {
    // Free list not a ptr (should be Symbol NIL)
    if ((VAL_TYPE(heap_state.freelist) == VAL_TYPE_SYMBOL) &&
	(DEC_SYM(heap_state.freelist) == SYMBOL_NIL)) {
      // all is as it should be (but no free cells)
      return heap_state.freelist;
    } else {
      // something is most likely very wrong
      //printf("heap_allocate_cell Error\n");
      return ENC_SYM(SYMBOL_NIL);
    }
  } else { // it is a ptr replace freelist with cdr of freelist;
    res = heap_state.freelist;
    res = res | ptr_type;
    heap_state.freelist =
      read_cdr(ref_cell(heap_state.freelist));
  }

  heap_state.num_alloc++;

  // set some ok initial values (nil . nil)
  set_car_(ref_cell(res), ENC_SYM(SYMBOL_NIL));
  set_cdr_(ref_cell(res), ENC_SYM(SYMBOL_NIL));

  // clear GC bit on allocated cell
  clr_gc_mark(ref_cell(res));
  return res;
}

uint32_t heap_num_allocated(void) {
  return heap_state.num_alloc;
}
uint32_t heap_size() {
  return heap_state.heap_size;
}

uint32_t heap_size_bytes(void) {
  return heap_state.heap_bytes;
}

void heap_get_state(heap_state_t *res) {
  res->heap_base           = heap_state.heap_base;
  res->freelist            = heap_state.freelist;
  res->freelist_last       = heap_state.freelist_last;
  res->heap_size           = heap_state.heap_size;
  res->heap_bytes          = heap_state.heap_bytes;
  res->num_alloc           = heap_state.num_alloc;
  res->num_alloc_arrays    = heap_state.num_alloc_arrays;
  res->gc_num              = heap_state.gc_num;
  res->gc_marked           = heap_state.gc_marked;
  res->gc_recovered        = heap_state.gc_recovered;
  res->gc_recovered_arrays = heap_state.gc_recovered_arrays;
}

// Recursive implementation can exhaust stack!
int gc_mark_phase(uint32_t env) {

  if (!IS_PTR(env)) {
      return 1; // Nothing to mark here
  }

  if (get_gc_mark(ref_cell(env))) {
    return 1; // Circular object on heap, or visited (aux)..
  }

  // There is at least a pointer to one cell here. Mark it and recurse over  car and cdr 
  heap_state.gc_marked ++;

  set_gc_mark(ref_cell(env));

  int res = 1;
  if (IS_PTR(car(env)))
    res &= gc_mark_phase(car(env));
  if (IS_PTR(cdr(env)))
    res &= gc_mark_phase(cdr(env));

  return res;
}

// The free list should be a "proper list"
// Using a while loop to traverse over the cdrs
int gc_mark_freelist() {

  uint32_t curr;
  cons_t *t;
  uint32_t fl = heap_state.freelist;

  if (!IS_PTR(fl)) {
    if ((VAL_TYPE(fl) == VAL_TYPE_SYMBOL) &&
	(DEC_SYM(fl) == SYMBOL_NIL)){
      return 1; // Nothing to mark here
    } else {
      printf(" ERROR CASE! %x \n", fl);
      return 0;
    }
  }

  curr = fl;
  while (IS_PTR(curr)){
     t = ref_cell(curr);
     set_gc_mark(t);
     curr = read_cdr(t);

     heap_state.gc_marked ++;
  }

  if ( t != ref_cell(heap_state.freelist_last)) {
    printf( "CORRUPT FREELIST!\n");
  }
  return 1;
}

int gc_mark_aux(uint32_t *aux_data, uint32_t aux_size) {

  cons_t *t;

  for (int i = 0; i < aux_size; i ++) {
    if (IS_PTR(aux_data[i])) {

      uint32_t pt_t = PTR_TYPE(aux_data[i]);
      uint32_t pt_v = DEC_PTR(aux_data[i]);

      if ( pt_t == PTR_TYPE_CONS ||
	   pt_t == PTR_TYPE_I32 ||
	   pt_t == PTR_TYPE_U32 ||
	   pt_t == PTR_TYPE_F32 ||
	   pt_t == PTR_TYPE_ARRAY ||
	   pt_t == PTR_TYPE_REF ||
	   pt_t == PTR_TYPE_STREAM &&
	   pt_v < heap_state.heap_size) {

	gc_mark_phase(aux_data[i]);
      }
    }
  }

  return 1;
}


// Sweep moves non-marked heap objects to the free list.
int gc_sweep_phase(void) {

  uint32_t i = 0;
  cons_t *heap = (cons_t *)heap_base;
  cons_t *fl_last;

  uint32_t cdr;

  for (i = 0; i < heap_state.heap_size; i ++) {
    if ( !get_gc_mark(&heap[i])){
      fl_last = ref_cell(heap_state.freelist_last);

      if ( DEC_SYM(read_cdr(fl_last)) != SYMBOL_NIL) {
	printf( "ERROR: cdr of fl_last not nil\n");
      }

      // check for array
      if (TYPE_OF(heap[i].cdr) == VAL_TYPE_SYMBOL &&
	  DEC_SYM(heap[i].cdr) == SPECIAL_SYM_ARRAY) {
	array_t *arr = (array_t*)heap[i].car; 
	switch(arr->elt_type) {
	case VAL_TYPE_CHAR: //TODO: test and complete
	  if (arr->data.c) free(arr->data.c);
	  free(arr);
	  heap_state.gc_recovered_arrays++; 
	  break;
	}
      }


      // Clear the "freed" cell.
      set_cdr_(&heap[i], 0);
      set_cdr_(&heap[i], ENC_SYM(SYMBOL_NIL));
      set_car_(&heap[i], ENC_SYM(SYMBOL_NIL));

      // create pointer to free cell to put at end of freelist
      uint32_t addr = ENC_CONS_PTR(i);

      set_cdr_(fl_last, addr);
      set_gc_mark(fl_last); // the above set_cdr_ clears the gc mark on fl_last.
      heap_state.freelist_last = addr;

      heap_state.num_alloc --;
      heap_state.gc_recovered ++;
    }
    clr_gc_mark(&heap[i]);
  }
  return 1;
}

int heap_perform_gc(uint32_t env) {
  heap_state.gc_num ++;
  heap_state.gc_recovered = 0;
  heap_state.gc_marked = 0;

  gc_mark_freelist();
  gc_mark_phase(env);
  return gc_sweep_phase();
}

int heap_perform_gc_aux(uint32_t env, uint32_t env2, uint32_t exp, uint32_t exp2, uint32_t *aux_data, uint32_t aux_size) {
  heap_state.gc_num ++;
  heap_state.gc_recovered = 0;
  heap_state.gc_marked = 0;

  gc_mark_freelist();
  gc_mark_phase(exp);
  gc_mark_phase(exp2);
  gc_mark_phase(env);
  gc_mark_phase(env2);
  gc_mark_aux(aux_data, aux_size);

#ifdef VISUALIZE_HEAP
  heap_vis_gen_image();
#endif

  return gc_sweep_phase();
}


// construct, alter and break apart
uint32_t cons(uint32_t car, uint32_t cdr) {
  uint32_t addr = heap_allocate_cell(PTR_TYPE_CONS);
  if ( IS_PTR(addr)) {
    set_car_(ref_cell(addr), car);
    set_cdr_(ref_cell(addr), cdr);
    return addr;
  }
  else return ENC_SYM(symrepr_nil());
}

uint32_t car(uint32_t c){

  if (TYPE_OF(c) == VAL_TYPE_SYMBOL &&
      DEC_SYM(c) == SYMBOL_NIL) {
    return c; // if nil, return nil.
  }

  //if (TYPE_OF(c) == PTR_TYPE_CONS ||
  //    TYPE_OF(c) == PTR_TYPE_F32) {   //TODO: one test should be enough
  if (IS_PTR(c) ){
    cons_t *cell = ref_cell(c);
    return read_car(cell);
  }
  return ENC_SYM(symrepr_terror());
}

uint32_t cdr(uint32_t c){

  if (TYPE_OF(c) == VAL_TYPE_SYMBOL &&
      DEC_SYM(c) == SYMBOL_NIL) {
    return c; // if nil, return nil.
  }

  if (TYPE_OF(c) == PTR_TYPE_CONS) {
    cons_t *cell = ref_cell(c);
    return read_cdr(cell);
  }
  return ENC_SYM(symrepr_terror());
}

void set_car(uint32_t c, uint32_t v) {
  if (IS_PTR(c) && PTR_TYPE(c) == PTR_TYPE_CONS) {
    cons_t *cell = ref_cell(c);
    set_car_(cell,v);
  }
}

void set_cdr(uint32_t c, uint32_t v) {
  if (TYPE_OF(c) == PTR_TYPE_CONS){
    cons_t *cell = ref_cell(c);
    set_cdr_(cell,v);
  }
}

/* calculate length of a proper list */
uint32_t length(uint32_t c) {
  uint32_t len = 0;

  while (TYPE_OF(c) == PTR_TYPE_CONS){
    len ++;
    c = cdr(c);
  }
  return len;
}

/* reverse a proper list */
uint32_t reverse(uint32_t list) {

  uint32_t curr = list;

  uint32_t new_list = ENC_SYM(symrepr_nil());
  while (TYPE_OF(curr) == PTR_TYPE_CONS) {

    new_list = cons(car(curr), new_list);
    curr = cdr(curr);
  }
  return new_list;
}




////////////////////////////////////////////////////////////
// ARRAY, REF and Stream functionality
////////////////////////////////////////////////////////////

// Arrays are part of the heap module because their lifespan is managed
// by the garbage collector. The data in the array is not stored
// in the "heap of cons cells".
int heap_allocate_array(uint32_t *res, uint32_t size, uint32_t type){

  array_t *array = malloc(sizeof(array_t));
  // allocating a cell that will, to start with, be a cons cell.
  uint32_t cell  = heap_allocate_cell(PTR_TYPE_CONS);

  switch(type) {
  case PTR_TYPE_I32: // array of I32
    array->data.i32 = (int32_t*)malloc(size * (sizeof(int32_t)));
    if (array->data.i32 == NULL) return 0;
    break;
  case PTR_TYPE_U32: // array of U32
    array->data.u32 = (uint32_t*)malloc(size * (sizeof(uint32_t)));
    if (array->data.u32 == NULL) return 0;
    break;
  case PTR_TYPE_F32: // array of Float
    array->data.f = (float*)malloc(size * (sizeof(float)));
    if (array->data.f == NULL) return 0;
    break;
  case VAL_TYPE_CHAR: // Array of Char
    array->data.c = (char*)malloc(size * (sizeof(char)));
    if (array->data.c == NULL) return 0;
    break;
  case VAL_TYPE_I28:
    array->data.i32 = (int32_t*)malloc(size * (sizeof(int32_t)));
    break;
  case VAL_TYPE_U28:
    array->data.u32 = (uint32_t*)malloc(size * (sizeof(uint32_t)));
    break;
  case VAL_TYPE_SYMBOL:
    array->data.u32 = (uint32_t*)malloc(size * (sizeof(uint32_t)));
    break;
  default:
    *res = ENC_SYM(symrepr_nil());
    return 0;
  }

  array->aux = 0;
  array->elt_type = type;
  array->size = size;

  set_car(cell, (uint32_t)array);
  set_cdr(cell, ENC_SYM(SPECIAL_SYM_ARRAY));

  cell = cell | PTR_TYPE_ARRAY;

  *res = cell;

  heap_state.num_alloc_arrays ++;

  return 1;
}
