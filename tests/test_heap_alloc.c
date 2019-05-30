
#include <stdlib.h>
#include <stdio.h>

#include "heap.h"
#include "symrepr.h"


int main(int argc, char **argv) {

  int res = 1;

  unsigned int heap_size = 1024 * 1024; 
  uint32_t cell;

  res = symrepr_init();
  if (!res) {
    printf("Error initializing symrepr\n");
    return 0;
  }
  printf("Initialized symrepr: OK\n"); 
  
  res = heap_init(heap_size);
  if (!res) {
    printf("Error initializing heap\n"); 
    return 0;
  }

  printf("Initialized heap: OK\n"); 
  
  for (int i = 0; i < heap_size; i ++) {
    cell = heap_allocate_cell(PTR_TYPE_CONS);
    if (!is_ptr(cell)) {
      printf("Error allocating cell %d\n", i); 
      return 0;
    }
  }
  printf("Allocated %d heap cells: OK\n", heap_size);

  for (int i = 0; i < 34; i ++) {
    cell = heap_allocate_cell(PTR_TYPE_CONS);
    if (is_ptr(cell)) {
      printf("Error allocation succeeded on empty heap\n"); 
      return 0;
    } else if (val_type(cell) != VAL_TYPE_SYMBOL ||
	       dec_sym(cell) != symrepr_merror()) {
      printf("Error Incorrect return value at cell allocation on full heap\n");
      return 0; 
    }
  }

  printf("HEAP allocation when full test: OK\n");
  return 1; 
  
}
