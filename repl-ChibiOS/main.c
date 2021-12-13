/*
    Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

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

// File also contains code distributed as part of Chibios under license below.

/*
    ChibiOS - Copyright (C) 2006..2018 Giovanni Di Sirio

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
 */

#include "ctype.h"

#include "ch.h"
#include "hal.h"
#include "chvt.h"
#include "chtime.h"

#include "usbcfg.h"
#include "chprintf.h"

#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "extensions.h"
#include "memory.h"
#include "env.h"

#define EVAL_WA_SIZE THD_WORKING_AREA_SIZE(8192)
#define REPL_WA_SIZE THD_WORKING_AREA_SIZE(4096)
#define EVAL_CPS_STACK_SIZE 256

BaseSequentialStream *chp = NULL;

int inputline(BaseSequentialStream *chp, char *buffer, int size) {
  int n = 0;
  unsigned char c;
  for (n = 0; n < size - 1; n++) {

    c = streamGet(chp);
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      streamPut(chp,0x8); /* output backspace character */
      streamPut(chp,' ');
      streamPut(chp,0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        streamPut(chp,c);
        buffer[n] = c;
      } else {
        n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}

void done_callback(eval_context_t *ctx) {

  char output[1024];
  char error[1024];

  CID cid = ctx->id;
  VALUE t = ctx->r;
  
  int print_ret = print_value(output, 1024, error, 1024, t);

  if (print_ret >= 0) {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, output);
  } else {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, error);
  }
}

uint32_t timestamp_callback() {
  systime_t t = chVTGetSystemTime();
  return (uint32_t) (100 * t);
}

void sleep_callback(uint32_t us) {
  chThdSleepMicroseconds(us);
}

volatile static eval_initiated=false;

static THD_FUNCTION(eval, arg) {
  (void) arg;

  while (!eval_initiated) {
    chThdSleepMilliseconds(10);
  }
  
  chprintf(chp,"Starting evaluator\n");
  eval_cps_run_eval();
}


VALUE ext_print(VALUE *args, int argn) {

  for (int i = 0; i < argn; i ++) {
    VALUE t = args[i];

    if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
      array_header_t *array = (array_header_t *)car(t);
      switch (array->elt_type){
      case VAL_TYPE_CHAR:
	chprintf(chp,"%s", (char*)array + 8);
	break;
      default:
	return enc_sym(symrepr_nil);
	break;
      }
    } else if (val_type(t) == VAL_TYPE_CHAR) {
      chprintf(chp,"%c", dec_char(t));
    } else {
      return enc_sym(symrepr_nil);
    }
 
  }
  return enc_sym(symrepr_true);
}


static THD_FUNCTION(repl, arg) {

  (void) arg;
  
  size_t len = 1024;
  char *str = malloc(1024);
  char *outbuf = malloc(2048);
  char *error = malloc(1024);
  int res = 0;
  
  heap_state_t heap_state;

  int heap_size = 2048;

  chThdSleepMilliseconds(2000);
  
  unsigned char *memory = malloc(MEMORY_SIZE_16K);
  unsigned char *bitmap = malloc(MEMORY_BITMAP_SIZE_16K);
  if (memory == NULL || bitmap == NULL) {
    chprintf(chp,"Unable to allocate memory!\r\n");
    return 0;
  }

  res = memory_init(memory, MEMORY_SIZE_16K,
		    bitmap, MEMORY_BITMAP_SIZE_16K);
  if (res)
    chprintf(chp,"Memory initialized. Memory size: %u Words. Free: %u Words.\n", memory_num_words(), memory_num_free());
  else {
    chprintf(chp,"Error initializing memory!\n");
    return 0;
  }
  
   res = symrepr_init();
  if (res)
    chprintf(chp,"Symrepr initialized.\r\n");
  else {
    chprintf(chp,"Error initializing symrepr!\r\n");
    return res;
  }
  
  res = heap_init(heap_size);
  if (res)
    chprintf(chp,"Heap initialized. Free cons cells: %u\r\n", heap_num_free());
  else {
    chprintf(chp,"Error initializing heap!\r\n");
    return res;
  }

  eval_cps_set_ctx_done_callback(done_callback);
  eval_cps_set_timestamp_us_callback(timestamp_callback);
  eval_cps_set_usleep_callback(sleep_callback);
  
  res = eval_cps_init();
  if (res)
    chprintf(chp,"Evaluator initialized.\r\n");
  else {
    chprintf(chp,"Error initializing evaluator.\r\n");
    return res;
  }
  
  res = extensions_add("print", ext_print);
  if (res)
    chprintf(chp,"Extension added.\r\n");
  else
    chprintf(chp,"Error adding extension.\r\n");

  eval_initiated = true;
  
  VALUE prelude = prelude_load();
  eval_cps_program(prelude);

  chprintf(chp,"Lisp REPL started (ChibiOS)!\r\n");

  while (1) {
    chprintf(chp,"# ");
    memset(str,0,len);
    memset(outbuf,0, 2048);
    inputline(chp,str, len);
    chprintf(chp,"\r\n");

    if (strncmp(str, ":info", 5) == 0) {
      chprintf(chp,"##(ChibiOS)#################################################\r\n");
      chprintf(chp,"Used cons cells: %lu \r\n", heap_size - heap_num_free());
      res = print_value(outbuf,2048, error, 1024, *env_get_global_ptr());
      if (res >= 0) {
	chprintf(chp,"ENV: %s \r\n", outbuf);
      } else {
	chprintf(chp,"%s\r\n",error);
      }
      heap_get_state(&heap_state);
      chprintf(chp,"GC counter: %lu\r\n", heap_state.gc_num);
      chprintf(chp,"Recovered: %lu\r\n", heap_state.gc_recovered);
      chprintf(chp,"Marked: %lu\r\n", heap_state.gc_marked);
      chprintf(chp,"Free cons cells: %lu\r\n", heap_num_free());
      chprintf(chp,"############################################################\r\n");
      memset(outbuf,0, 2048);
    } else if (strncmp(str, ":quit", 5) == 0) {
      break;
    } else {

      if (strlen(str) == 0) {
	continue;
      }
      
      VALUE t;
      t = tokpar_parse(str);

      CID cid = eval_cps_program(t);
      if (cid == 0) {
	chprintf(chp,"Error creating ctx\r\n");
      } else {
	chprintf(chp,"started ctx: %u\r\n", cid);
      }
    }
  }

  symrepr_del();
  heap_del();
}

int main(void) {
  halInit();
  chSysInit();

  sduObjectInit(&SDU1);
  sduStart(&SDU1, &serusbcfg);

  /*
   * Activates the USB driver and then the USB bus pull-up on D+.
   * Note, a delay is inserted in order to not have to disconnect the cable
   * after a reset.
   */
  usbDisconnectBus(serusbcfg.usbp);
  chThdSleepMilliseconds(1500);
  usbStart(serusbcfg.usbp, &usbcfg);
  usbConnectBus(serusbcfg.usbp);	

  chp = (BaseSequentialStream*)&SDU1;

  chThdCreateFromHeap(NULL, EVAL_WA_SIZE,
		      "eval", NORMALPRIO + 1,
		      eval, (void *)NULL);

  
  chThdCreateFromHeap(NULL, REPL_WA_SIZE,
		      "repl", NORMALPRIO + 1,
		      repl, (void *)NULL);
  
  while(1) { 
    chThdSleepMilliseconds(500);
  }

}
