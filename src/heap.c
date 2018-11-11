
#include <stdio.h>
#include <stdint.h>

#include "heap.h"
#include "symrepr.h"

static cons_t *heap = NULL; 

static uint32_t heap_base; 
static uint32_t free_list = 0; 
static uint32_t free_list_last = 0;


static heap_stats_t heap_stats;

static uint32_t SYMBOL_NIL; // pre shifted

// ref_cell: returns a reference to the cell addressed by bits 3 - 26
//           Assumes user has checked that IS_PTR was set 
cons_t* ref_cell(uint32_t addr) {
  return (cons_t*)(heap_base + (addr & PTR_VAL_MASK));
}

uint32_t read_car(cons_t *cell) {
  return cell->car;
}

uint32_t read_cdr(cons_t *cell) {
  return cell->cdr;
}

void set_car(cons_t *cell, uint32_t v) {
  cell->car = v;
}

void set_cdr(cons_t *cell, uint32_t v) {
  cell->cdr = v;
}

void set_gc_mark(cons_t *cell) {
  uint32_t cdr = read_cdr(cell);
  set_cdr(cell, cdr | GC_MARKED); 
}

void clr_gc_mark(cons_t *cell) {
  uint32_t cdr = read_cdr(cell);
  set_cdr(cell, cdr & ~GC_MASK);
}

int generate_free_list(size_t num_cells) {
  size_t i = 0; 
  
  if (!heap) return 0;
  
  free_list = 0 | IS_PTR;  

  // Add all cells to free list
  for (i = 1; i < num_cells; i ++) {
    cons_t *t = ref_cell( (i-1) << ADDRESS_SHIFT);
    set_car(t, SYMBOL_NIL | VAL_TYPE_SYMBOL);    // all cars in free list are nil 
    set_cdr(t, (i << ADDRESS_SHIFT) | IS_PTR);
    set_gc_mark(t); 
  }
  
  free_list_last = (num_cells-1)<<ADDRESS_SHIFT;
  set_cdr(ref_cell(free_list_last), SYMBOL_NIL | VAL_TYPE_SYMBOL);
  
  if (read_cdr(ref_cell(free_list_last)) == (VAL_TYPE_SYMBOL | SYMBOL_NIL)) {
    return 1;
  }
  return 0; 
}

void print_bit(uint32_t v) {
  for  (int i = 31; i >= 0; i --) {
    if (v & (1 << i)) {
      printf("1"); 
    } else {
      printf("0"); 
    }
  }
  printf("\n"); 
}

int heap_init(size_t num_cells) {

  symrepr_lookup("nil", &SYMBOL_NIL); 
  SYMBOL_NIL = SYMBOL_NIL << VAL_SHIFT;

  // Allocate heap 
  heap = (cons_t *)malloc(num_cells * sizeof(cons_t));

  if (!heap) return 0;

  heap_base = (uint32_t)heap;
  
  // Initialize heap statistics
  heap_stats.heap_bytes = (uint32_t)(num_cells * sizeof(cons_t));
  heap_stats.heap_size = num_cells;
  
  heap_stats.num_alloc = 0;
  heap_stats.gc_num = 0;
  heap_stats.gc_marked = 0;
  heap_stats.gc_recovered = 0; 
  
  return (generate_free_list(num_cells)); 
}

void heap_del(void) {
  if (heap)
    free(heap); 
}

uint32_t heap_num_free(void) {

  uint32_t count = 0;
  uint32_t curr = free_list; 
  
  while ((curr & PTR_MASK) == IS_PTR) {
    curr = read_cdr(ref_cell(curr));
    count++; 
  }

  if (((curr & VAL_TYPE_MASK) != VAL_TYPE_SYMBOL) ||
      ((curr & VAL_MASK) != SYMBOL_NIL)) {
    return 0; 
  } 
  return count; 
}


uint32_t heap_allocate_cell(void) {

  uint32_t res;
  
  if (! (free_list & PTR_MASK) == IS_PTR) {
    // Free list not a ptr (should be Symbol NIL)
    if (((free_list & VAL_TYPE_MASK) == VAL_TYPE_SYMBOL) &&
	((free_list & VAL_MASK) == SYMBOL_NIL)) {
      // all is as it should be (but no free cells)
      return free_list; 
    } else {
      // something is most likely very wrong
      //printf("heap_allocate_cell Error\n"); 
      return SYMBOL_NIL | VAL_TYPE_SYMBOL;
    }   
  } else { // it is a ptr replace free_list with cdr of free_list; 
    res = free_list; 
    free_list = (read_cdr(ref_cell(free_list & PTR_VAL_MASK)));
  }

  heap_stats.num_alloc++;

  // set some ok initial values (nil . nil)
  set_car(ref_cell(res), SYMBOL_NIL | VAL_TYPE_SYMBOL);
  set_cdr(ref_cell(res), SYMBOL_NIL | VAL_TYPE_SYMBOL); 
	  
  
  // clear GC bit on allocated cell
  clr_gc_mark(ref_cell(res));
  return res;
}

uint32_t heap_size_bytes(void) {
  return heap_stats.heap_bytes;
}
  
void heap_get_stats(heap_stats_t *res) {
  res->heap_size  = heap_stats.heap_size;
  res->heap_bytes = heap_stats.heap_bytes;
  res->num_alloc  = heap_stats.num_alloc;
  res->gc_num     = heap_stats.gc_num;
  res->gc_marked  = heap_stats.gc_marked;
  res->gc_recovered = heap_stats.gc_recovered;
}

uint32_t heap_get_freelist(void) {
  return free_list;
}


// Recursive implementation can exhaust stack!
int gc_mark_phase(uint32_t env) {

  cons_t *t;
  uint32_t car; 
  uint32_t cdr;

  if (!((env & PTR_MASK) == IS_PTR)) {
    if ((env & VAL_MASK) == SYMBOL_NIL) {
      return 1; // Nothing to mark here 
    } else {
      printf(" ERROR CASE! %x \n", env);
      return 0;
    }
  }
  
  // There is at least a pointer to one cell here. Mark it and recurse
  // car and cdr 
  // TODO: Special cases here for differnt kinds of pointers.

  heap_stats.gc_marked ++;

  t = ref_cell(env);

  set_gc_mark(t); 
    
  car = read_car(t);
  cdr = read_cdr(t); 
  
  gc_mark_phase(car);
  gc_mark_phase(cdr); 
  
  return 1; 
}

// The free list should be a "proper list"
// Using a while loop to traverse over the cdrs 
int gc_mark_free_list(uint32_t fl) {

  uint32_t curr;
  cons_t *t;

  if (!((fl & PTR_MASK) == IS_PTR)) {
    if ((fl & VAL_MASK) == SYMBOL_NIL) {
      return 1; // Nothing to mark here 
    } else {
      printf(" ERROR CASE! %x \n", fl);
      return 0;
    }
  }

  curr = fl;
  while ((curr & PTR_MASK) == IS_PTR) {
    
     t = ref_cell(curr);
     set_gc_mark(t);
     heap_stats.gc_marked ++;
     curr = read_cdr(t); 
  }
  return 1;
}

// Sweep moves non-marked heap objects to the free list.
int gc_sweep_phase(void) {

  uint32_t i = 0; 
  cons_t *heap = (cons_t *)heap_base;
  cons_t *fl_last; 
  
  uint32_t cdr;
  
  for (i = 0; i < heap_stats.heap_size; i ++) {   
    cdr = read_cdr(&heap[i]);
    if ( (cdr & GC_MASK) != GC_MARKED ) {
      fl_last = ref_cell(free_list_last);

      set_cdr(&heap[i], 0); 
      set_cdr(&heap[i], SYMBOL_NIL | VAL_TYPE_SYMBOL | GC_MARKED);
      set_car(&heap[i], SYMBOL_NIL | VAL_TYPE_SYMBOL);

      uint32_t addr = (i << ADDRESS_SHIFT) | IS_PTR | GC_MARKED;

      set_cdr(fl_last, addr);
      free_list_last = addr;

      heap_stats.gc_recovered ++;
    }
    clr_gc_mark(&heap[i]);
  }
  return 1; 
}


int heap_perform_gc(uint32_t env) {
  heap_stats.gc_num ++;
  heap_stats.gc_recovered = 0; 
  heap_stats.gc_marked = 0; 

  gc_mark_free_list(free_list);
  gc_mark_phase(env); 
  return gc_sweep_phase();
}
