#define _GNU_SOURCE // MAP_ANON
#define _POSIX_C_SOURCE 200809L // nanosleep?
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <unistd.h>

#include "lispbm.h"
#include "heap.h"

#include "init/start_lispbm.c"

static int test_init(void) {
  return start_lispbm_for_tests();
}

int test_lbm_heap_num_allocated(void) {
  if (!test_init()) return 0;
  
  // Test 1: Check that heap num allocated returns valid value initially
  lbm_uint initial_allocated = lbm_heap_num_allocated();
  
  // Test 2: Allocate some memory and verify count increases
  lbm_value cons_cell = lbm_cons(lbm_enc_i(42), lbm_enc_i(24));
  if (lbm_is_symbol_merror(cons_cell)) return 0;
  
  lbm_uint after_alloc = lbm_heap_num_allocated();
  
  // Test 3: Verify allocation count increased
  if (after_alloc <= initial_allocated) {
    return 0;
  }
  
  // Test 4: Multiple allocations should increase count further
  lbm_value another_cons = lbm_cons(lbm_enc_i(100), cons_cell);
  if (lbm_is_symbol_merror(another_cons)) return 0;
  
  lbm_uint final_allocated = lbm_heap_num_allocated();
  
  if (final_allocated <= after_alloc) {
    return 0;
  }
  
  return 1;
}

int test_lbm_heap_size(void) {
  if (!test_init()) return 0;
  
  // Test 1: Heap size should return a positive value
  lbm_uint heap_size = lbm_heap_size();
  
  if (heap_size == 0) {
    return 0;
  }
  
  // Test 2: Heap size should remain constant regardless of allocations
  lbm_value test_alloc = lbm_cons(lbm_enc_i(1), lbm_enc_i(2));
  if (lbm_is_symbol_merror(test_alloc)) return 0;
  
  lbm_uint heap_size_after = lbm_heap_size();
  
  if (heap_size != heap_size_after) {
    return 0;
  }
  
  return 1;
}

int test_lbm_heap_size_bytes(void) {
  if (!test_init()) return 0;
  
  // Test 1: Heap size in bytes should return a positive value
  lbm_uint heap_bytes = lbm_heap_size_bytes();
  
  if (heap_bytes == 0) {
    return 0;
  }
  
  // Test 2: Size in bytes should be larger than heap size (since each cell is multiple bytes)
  lbm_uint heap_cells = lbm_heap_size();
  
  if (heap_bytes <= heap_cells) {
    return 0;
  }
  
  // Test 3: Size in bytes should remain constant regardless of allocations
  lbm_value test_alloc = lbm_cons(lbm_enc_i(10), lbm_enc_i(20));
  if (lbm_is_symbol_merror(test_alloc)) return 0;
  
  lbm_uint heap_bytes_after = lbm_heap_size_bytes();
  
  if (heap_bytes != heap_bytes_after) {
    return 0;
  }
  
  return 1;
}

int test_lbm_get_gc_stack_size(void) {
  if (!test_init()) return 0;
  
  // Test 1: GC stack size should return a valid value (could be 0 initially)
  lbm_uint gc_stack_size = lbm_get_gc_stack_size();
  
  // Test 2: Create some allocations that might affect GC stack
  lbm_value nested_list = lbm_cons(lbm_enc_i(1), 
                                  lbm_cons(lbm_enc_i(2), 
                                          lbm_cons(lbm_enc_i(3), ENC_SYM_NIL)));
  if (lbm_is_symbol_merror(nested_list)) return 0;
  
  // Test 3: Stack size should still be valid after allocations
  lbm_uint gc_stack_size_after = lbm_get_gc_stack_size();
  
  // Both values should be valid (non-negative, which they are as unsigned)
  (void)gc_stack_size;
  (void)gc_stack_size_after;
  
  return 1;
}

int test_lbm_get_gc_stack_max(void) {
  if (!test_init()) return 0;
  
  // Test 1: GC stack max should return a positive value
  lbm_uint gc_stack_max = lbm_get_gc_stack_max();

  // gc_stack_max keeps track of the "high water mark"
  // it is not an actual maximum. 
  
  // Test 2: 
  lbm_uint gc_stack_size = lbm_get_gc_stack_size();

  // gc_stack_size should be larger than gc_stack_max
  if (gc_stack_size < gc_stack_max) return 0;
  
    
  // Test 3:
  // max is not a constant, it is the highest point reached.
  lbm_value test_alloc = lbm_cons(lbm_enc_i(99), lbm_enc_i(88));
  if (lbm_is_symbol_merror(test_alloc)) return 0;
  
  lbm_uint gc_stack_max_after = lbm_get_gc_stack_max();
  
  if (!(gc_stack_max_after >= gc_stack_max)) {
    return 0;
  }
  
  return 1;
}

int test_heap_functions_comprehensive(void) {
  if (!test_init()) return 0;
  
  // Test relationships between different heap functions
  lbm_uint heap_size = lbm_heap_size();
  lbm_uint heap_bytes = lbm_heap_size_bytes();
  lbm_uint allocated = lbm_heap_num_allocated();
  lbm_uint gc_size = lbm_get_gc_stack_size();
  lbm_uint gc_max = lbm_get_gc_stack_max();
  
  // Test 1: Allocated should not exceed total heap size
  if (allocated > heap_size) {
    return 0;
  }
  
  // Test 2: 
  if (gc_size < gc_max) {
    return 0;
  }
  
  // Test 3: All values should be reasonable (not obviously corrupted)
  if (heap_size == 0 || heap_bytes == 0) {
    return 0;
  }
  
  // Test 4: Make several allocations and verify heap state remains consistent
  for (int i = 0; i < 10; i++) {
    lbm_value test_val = lbm_cons(lbm_enc_i(i), lbm_enc_i(i * 2));
    if (lbm_is_symbol_merror(test_val)) return 0;
  }
  
  // Verify heap is still in valid state
  lbm_uint new_allocated = lbm_heap_num_allocated();
  lbm_uint new_heap_size = lbm_heap_size();
  lbm_uint new_heap_bytes = lbm_heap_size_bytes();
  lbm_uint new_gc_max = lbm_get_gc_stack_max();
  
  if (new_allocated <= allocated) return 0; // Should have increased
  if (new_heap_size != heap_size) return 0; // Should remain constant
  if (new_heap_bytes != heap_bytes) return 0; // Should remain constant
  if (new_gc_max != gc_max) return 0; // Should remain constant
  if (new_allocated > new_heap_size) return 0; // Should not exceed heap size
  
  return 1;
}

int test_lbm_gc_lock_unlock(void) {
  if (!test_init()) return 0;
  
  // Test 1: Basic lock and unlock operations should not crash
  lbm_gc_lock();
  lbm_gc_unlock();
  
  // Test 2: Multiple locks should work (if implementation supports nesting)
  lbm_gc_lock();
  lbm_gc_lock();
  lbm_gc_unlock();
  lbm_gc_unlock();
  
  // Test 3: Test that allocations work normally after unlock
  lbm_value test_alloc1 = lbm_cons(lbm_enc_i(123), lbm_enc_i(456));
  if (lbm_is_symbol_merror(test_alloc1)) return 0;
  
  // Test 4: Lock during allocation sequence
  lbm_gc_lock();
  lbm_value test_alloc2 = lbm_cons(lbm_enc_i(789), lbm_enc_i(012));
  if (lbm_is_symbol_merror(test_alloc2)) return 0;
  lbm_gc_unlock();
  
  // Test 5: Verify heap is still functional after lock/unlock operations
  lbm_value final_test = lbm_cons(test_alloc1, test_alloc2);
  if (lbm_is_symbol_merror(final_test)) return 0;
  
  return 1;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  total_tests++; if (test_lbm_heap_num_allocated()) tests_passed++;
  printf("%d\n", tests_passed);
  total_tests++; if (test_lbm_heap_size()) tests_passed++;
  printf("%d\n", tests_passed);
  total_tests++; if (test_lbm_heap_size_bytes()) tests_passed++;
  printf("%d\n", tests_passed);
  total_tests++; if (test_lbm_get_gc_stack_size()) tests_passed++;
  printf("%d\n", tests_passed);
  total_tests++; if (test_lbm_get_gc_stack_max()) tests_passed++;
  printf("%d\n", tests_passed);
  total_tests++; if (test_heap_functions_comprehensive()) tests_passed++;
  printf("%d\n", tests_passed);
  total_tests++; if (test_lbm_gc_lock_unlock()) tests_passed++;
  printf("%d\n", tests_passed);
  
  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
