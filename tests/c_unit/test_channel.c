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
#include "lbm_image.h"
#include "lbm_channel.h"

#include "init/start_lispbm.c"

int test_string_char_channel_peek(void) {
  
  char *expr1 = "abcd";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  int r = 0;
  char peek_r;
  r = lbm_channel_peek(&chan1, 0, &peek_r);
  if (r != CHANNEL_SUCCESS || peek_r != 'a') return 0;

  r = lbm_channel_peek(&chan1, 1, &peek_r);
  if (r != CHANNEL_SUCCESS || peek_r != 'b') return 0;
  
  r = lbm_channel_peek(&chan1, 2, &peek_r);
  if (r != CHANNEL_SUCCESS || peek_r != 'c') return 0;

  r = lbm_channel_peek(&chan1, 3, &peek_r);
  if (r != CHANNEL_SUCCESS || peek_r != 'd') return 0;

  r = lbm_channel_peek(&chan1, 4, &peek_r);
  if (r != CHANNEL_END) return 0;

  return 1;
}

int test_string_char_channel_read(void) {
  
  char *expr1 = "abcd";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  bool r;
  char read_r;
  r = lbm_channel_read(&chan1,  &read_r);
  if (!r || read_r != 'a') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);
  if (!r || read_r != 'b') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);
  if (!r || read_r != 'c') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);
  if (!r || read_r != 'd') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);

  // TODO: string_read always returns true.
  //        This is not correct, look into fixing it.
  
  //if (r) return 0;
  
  return 1;
}

// The meaning of more is that there is more comming than what is currently
// available.
// So it is very possible that more = false
// and at the same time is_empty = false
//
// For a string channel more is always false.
int test_string_char_channel_more(void) {
  
  char *expr1 = "abcd";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  bool r;
  char read_r;

  if (lbm_channel_more(&chan1)) return 0;

  r = lbm_channel_read(&chan1,  &read_r);
  if (!r || read_r != 'a') return 0;

  r = lbm_channel_read(&chan1, &read_r);
  if (!r || read_r != 'b') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);
  if (!r || read_r != 'c') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);

  // NOTE that if the channel says more is available here, it is a bug.
  if (!r || read_r != 'd') return 0;

  r = lbm_channel_read(&chan1, &read_r);
  // TODO: Incorrect return value from string_read 

  if (lbm_channel_more(&chan1)) return 0;
  
  return 1;
}

int test_string_char_channel_is_empty(void) {
  
  char *expr1 = "abcd";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  bool r;
  char read_r;
  bool is_empty;

  r = lbm_channel_read(&chan1,  &read_r);
  is_empty = lbm_channel_is_empty(&chan1);
  if (is_empty || !r || read_r != 'a') return 0;

  r = lbm_channel_read(&chan1, &read_r);
  is_empty = lbm_channel_is_empty(&chan1);
  if (is_empty || !r || read_r != 'b') return 0;

  r = lbm_channel_read(&chan1, &read_r);
  is_empty = lbm_channel_is_empty(&chan1);
  if (is_empty || !r || read_r != 'c') return 0;

  r = lbm_channel_read(&chan1, &read_r);
  is_empty = lbm_channel_is_empty(&chan1);
  //Note that channel has become empty here
  if (!is_empty || !r || read_r != 'd') return 0;
  
  return 1;
}


// string_channel_is_full 
//  There is a write_pos even in string channels
//  and you can write to string channels. I think
//  this makes very little sense. 
int test_string_char_channel_is_full(void) {
  
  char *expr1 = "abcd";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  bool r;
  char read_r;

  if (lbm_channel_is_full(&chan1)) return 0;
  
  r = lbm_channel_read(&chan1,  &read_r); 
  if ( !r || read_r != 'a') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);
  if ( !r || read_r != 'b') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);
  if ( !r || read_r != 'c') return 0;
  
  r = lbm_channel_read(&chan1, &read_r);
  if ( !r || read_r != 'd') return 0;

  if (lbm_channel_is_full(&chan1)) return 0;
                       
  return 1;
}

int test_string_char_channel_write(void) {


  char my_str[5]; 

  memcpy(my_str,"abcd",5);
  
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, my_str);

  bool r;
  char read_r;
  

  if (lbm_channel_is_full(&chan1)) return 0;
  
  if (lbm_channel_write(&chan1, 'i') != CHANNEL_SUCCESS) return 0;

  if (lbm_channel_write(&chan1, 'j') != CHANNEL_SUCCESS) return 0;

  if (lbm_channel_write(&chan1, 'k') != CHANNEL_SUCCESS) return 0;

  if (lbm_channel_write(&chan1, 'l') != CHANNEL_SUCCESS) return 0;

    //if (!lbm_channel_is_full(&chan1)) return 0;

  r = lbm_channel_read(&chan1,  &read_r); 
  if ( !r || read_r != 'i') return 0;

  r = lbm_channel_read(&chan1, &read_r);
  if ( !r || read_r != 'j') return 0;

  r = lbm_channel_read(&chan1, &read_r);
  if ( !r || read_r != 'k') return 0;

    //r = lbm_channel_read(&chan1, &read_r);
    //if ( !r || read_r != 'l') return 0;
  return 1;
}


int test_string_char_channel_row_column(void) {
  
  char *expr1 = "\n\n\n\naaaa";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  char read_r;

  for (int i = 0; i < 8; i ++) {
    lbm_channel_read(&chan1,  &read_r);
  }

  unsigned int row = lbm_channel_row(&chan1);
  unsigned int col = lbm_channel_column(&chan1);
  
  if (row != 5 || col != 5) return 0;
  
  return 1;
}

int test_string_char_channel_drop(void) {
  
  char *expr1 = "abcdefghijkl";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  char read_r;

  lbm_channel_drop(&chan1, 6);
  lbm_channel_read(&chan1,  &read_r);

  if (read_r != 'g') return 0;
  
  return 1;
}

int test_string_char_channel_reader_close_closed(void) {
  
  char *expr1 = "abcdefghijkl";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  char read_r;

  lbm_channel_reader_close(&chan1);
  bool closed = lbm_channel_reader_is_closed(&chan1);
  
  // string reader allows reading from closed channel.
  lbm_channel_read(&chan1,  &read_r);

  if (!closed) return 0;
  
  return 1;
}

int test_string_char_channel_writer_close_closed(void) {
  
  char *expr1 = "abcdefghijkl";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  char read_r;

  lbm_channel_writer_close(&chan1);
  bool more = lbm_channel_more(&chan1);
  
  // string reader allows reading from closed channel.
  lbm_channel_read(&chan1,  &read_r);

  if (more) return 0;
  
  return 1;
}

int test_string_char_channel_may_block(void) {
  
  char *expr1 = "abcdefghijkl";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  bool may_block = lbm_channel_may_block(&chan1);
  
  if (may_block) return 0;
  
  return 1;
}

int test_string_char_channel_set_comment_comment(void) {
  
  char *expr1 = "abcdefghijkl";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, expr1);

  lbm_channel_set_comment(&chan1,true);

  if (!lbm_channel_comment(&chan1)) return 0;

  lbm_channel_set_comment(&chan1,false);

  if (lbm_channel_comment(&chan1)) return 0;
  
  return 1;
}



int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  total_tests++; if (test_string_char_channel_peek()) tests_passed++;
  total_tests++; if (test_string_char_channel_read()) tests_passed++;
  total_tests++; if (test_string_char_channel_more()) tests_passed++;
  total_tests++; if (test_string_char_channel_is_empty()) tests_passed++;
  total_tests++; if (test_string_char_channel_is_full()) tests_passed++;
  total_tests++; if (test_string_char_channel_write()) tests_passed++;
  total_tests++; if (test_string_char_channel_row_column()) tests_passed++;
  total_tests++; if (test_string_char_channel_drop()) tests_passed++;
  total_tests++; if (test_string_char_channel_reader_close_closed()) tests_passed++;
  total_tests++; if (test_string_char_channel_writer_close_closed()) tests_passed++;
  total_tests++; if (test_string_char_channel_set_comment_comment()) tests_passed++;  
  
  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
