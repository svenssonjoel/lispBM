/*
    Copyright 2019,2021 Joel Svensson	svenssonjoel@yahoo.se

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

#include <device.h>
#include <drivers/uart.h>
#include <zephyr.h>
#include <sys/ring_buffer.h>
#include <stdint.h>

#include "memory.h"
#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "env.h"

/* ******************************** */
/* LLBridge includes                */

#include "usb_cdc.h"

#define LISPBM_HEAP_SIZE 2048
#define LISPBM_OUTPUT_BUFFER_SIZE 4096
#define LISPBM_ERROR_BUFFER_SIZE  1024
#define LISPBM_INPUT_BUFFER_SIZE  1024
#define EVAL_CPS_STACK_SIZE 256

void main(void)
{

  start_usb_cdc_thread();
  
  k_sleep(K_SECONDS(5));

  usb_printf("Allocating input/output buffers\r\n");
  char *str = malloc(LISPBM_INPUT_BUFFER_SIZE);
  char *outbuf = malloc(LISPBM_OUTPUT_BUFFER_SIZE);
  char *error = malloc(LISPBM_ERROR_BUFFER_SIZE);
  int res = 0;

  heap_state_t heap_state;

  unsigned char *memory = malloc(MEMORY_SIZE_16K);
  unsigned char *bitmap = malloc(MEMORY_BITMAP_SIZE_16K);
  if (memory == NULL || bitmap == NULL) return;
  
  res = memory_init(memory, MEMORY_SIZE_16K,
		    bitmap, MEMORY_BITMAP_SIZE_16K);
  if (res)
    usb_printf("Memory initialized. Memory size: %u Words. Free: %u Words.\r\n", memory_num_words(), memory_num_free());
  else {
    usb_printf("Error initializing memory!\r\n");
    return;
  } 
  
  res = symrepr_init();
  if (res)
    usb_printf("Symrepr initialized.\r\n");
  else {
    usb_printf("Error initializing symrepr!\r\n");
    return;
  }

  res = heap_init(LISPBM_HEAP_SIZE);
  if (res)
    usb_printf("Heap initialized. Free cons cells: %u\r\n", heap_num_free());
  else {
    usb_printf("Error initializing heap!\r\n");
    return;
  }

  res = eval_cps_init_nc(EVAL_CPS_STACK_SIZE, false);
  if (res)
    usb_printf("Evaluator initialized.\r\n");
  else {
    usb_printf("Error initializing evaluator.\r\n");
  }
	
  VALUE prelude = prelude_load();
  eval_cps_program_nc(prelude);


  usb_printf("Lisp REPL started (ZephyrOS)!\r\n");
	
  while (1) {
    k_sleep(K_MSEC(100));
    usb_printf("# ");
    memset(str,0,LISPBM_INPUT_BUFFER_SIZE);
    memset(outbuf,0, LISPBM_OUTPUT_BUFFER_SIZE);

    /* While loop handles empty lines */
    while ( usb_readl(str, LISPBM_INPUT_BUFFER_SIZE) == 0) {
      k_sleep(K_MSEC(100));
    }
    
    usb_printf("\r\n");

    if (strncmp(str, ":info", 5) == 0) {
      usb_printf("##(REPL - ZephyrOS)#########################################\r\n");
      usb_printf("Used cons cells: %lu \r\n", LISPBM_HEAP_SIZE - heap_num_free());
      res = print_value(outbuf, LISPBM_OUTPUT_BUFFER_SIZE, error, LISPBM_ERROR_BUFFER_SIZE, *env_get_global_ptr());
      if (res >= 0) {
	usb_printf("ENV: %s \r\n", outbuf);
      } else {
	usb_printf("%s\n", error);
      }
      heap_get_state(&heap_state);
      usb_printf("GC counter: %lu\r\n", heap_state.gc_num);
      usb_printf("Recovered: %lu\r\n", heap_state.gc_recovered);
      usb_printf("Marked: %lu\r\n", heap_state.gc_marked);
      usb_printf("Free cons cells: %lu\r\n", heap_num_free());
      usb_printf("############################################################\r\n");
      memset(outbuf,0, LISPBM_OUTPUT_BUFFER_SIZE);
    } else if (strncmp(str, ":quit", 5) == 0) {
      break;
    } else {

      VALUE t;
      t = tokpar_parse(str);

      t = eval_cps_program_nc(t);

      res = print_value(outbuf, LISPBM_OUTPUT_BUFFER_SIZE, error, LISPBM_ERROR_BUFFER_SIZE, t);
      if (res >= 0) {
	usb_printf("> %s\r\n", outbuf);
      } else {
	usb_printf("%s\r\n", error);
      }
    }
  }

  symrepr_del();
  heap_del();

}
