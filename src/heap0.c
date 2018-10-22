#include "heap0.h"

#include <stdio.h>

cons_t *heap = NULL; 

cons_t *free_list = NULL;
cons_t *free_list_last = NULL; 

int generate_free_list(size_t num_cells) {
  size_t i = 0; 
  
  if (!heap) return 0;

  free_list = &heap[0]; /* the entire heap is free as a starting point 
			   So point freelist to first cons cell in heap. 
			*/ 

  cons_t *curr = &heap[0]; 
  
  /* link the entire heap into one list */ 
  for (i = 1; i < num_cells; i ++) {

    curr->cdr.cons = &heap[i];
    curr = &heap[i]; 
  }
  /* terminate the free list with NULL */ 
  curr->cdr.cons = NULL;
  /*and point the free-list "last" pointer to curr */
  free_list_last = curr; 
} 


int heap_init(size_t num_cells) {

  heap = (cons_t *)malloc(num_cells * sizeof(cons_t));

  if (!heap) return 0;

  return (generate_free_list(num_cells)); 
}

void heap_del(void) {
  if (heap)
    free(heap); 
}

size_t heap_num_free(void) {

  size_t count = 0;

  cons_t *curr = free_list;
  
  if (curr == NULL) return 0;
  
  while (curr != NULL) {
    count++;
    /* todo also check that cdr is of POINTER type */ 
    curr = curr->cdr.cons; 
  }

  return count; 
}

cons_t *heap_allocate_cell() {

  cons_t *res;

  res = free_list; /* the first element in the free-list */

  if (free_list) {
    if (free_list->cdr.cons) {
      free_list = free_list->cdr.cons;
    } else {
      free_list = NULL; /* empty */
    }
  }
  
  if (res == NULL) return NULL;

  res->type = (NIL << 8 | NIL); 
  res->cdr.i = 0; /* hack */
  res->car.i = 0; /* hack */ 
  
  return res;
}
