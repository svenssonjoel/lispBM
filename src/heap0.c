#include "heap0.h"

#include <stdio.h>
#include <stdint.h>

cons_t *heap = NULL; 

uint32_t heap_bytes; 

uint32_t heap_base; 
uint32_t free_list = 0; 
uint32_t free_list_last = 0;

#define REF_CELL(ADDR) ((cons_t*)(heap_base + (ADDR)))


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
	     
int generate_free_list(size_t num_cells) {
  size_t i = 0; 
  
  if (!heap) return 0;
  
  free_list = 0 | IS_PTR;  

  // Add all cells to free list
  for (i = 1; i < num_cells; i ++) {
    cons_t *t = ref_cell((i-1)<< ADDRESS_SHIFT); 
    set_car(t, 0);
    set_cdr(t, (i * CONS_CELL_SIZE) | IS_PTR); 
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

  heap = (cons_t *)malloc(num_cells * sizeof(cons_t));

  if (!heap) return 0;

  heap_base = (uint32_t)heap;
  heap_bytes = (uint32_t)(num_cells * sizeof(cons_t)); 

  printf("heap addr: %x \n",(uint32_t) heap);
  print_bit((uint32_t)heap);

  for (int i = 0; i < 10; i ++) {
    print_bit((uint32_t)&heap[i]); 
  }
  
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
    curr = read_cdr(ref_cell(curr & PTR_VAL_MASK));
    count++; 
  }

  if (((curr & VAL_TYPE_MASK) != VAL_TYPE_SYMBOL) ||
      ((curr & VAL_MASK) != SYMBOL_NIL)) {
    printf("FREE LIST ERROR\n");
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
  } else { // it is a ptr
    res = free_list; 
    free_list = (read_cdr(ref_cell(free_list & PTR_VAL_MASK)));
  }
  return res;
}

uint32_t heap_size_bytes(void) {
  return heap_bytes;
}
  
