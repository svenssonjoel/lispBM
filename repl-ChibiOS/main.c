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

#include "usbcfg.h"
#include "chprintf.h"

#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "extensions.h"

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

#define REPL_WA_SIZE THD_WORKING_AREA_SIZE(10*4096)

VALUE ext_print(VALUE *args, int argn) {

  for (int i = 0; i < argn; i ++) {
    VALUE t = args[i];

    if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
      array_t *array = (array_t *)car(t);
      switch (array->elt_type){
      case VAL_TYPE_CHAR:
	chprintf(chp,"%s", array->data.c);
	break;
      default:
	return enc_sym(symrepr_nil());
	break;
      }
    } else if (val_type(t) == VAL_TYPE_CHAR) {
      chprintf(chp,"%c", dec_char(t));
    } else {
      return enc_sym(symrepr_nil());
    }
 
  }
  return enc_sym(symrepr_true());
}



int reset_repl(int heap_size) {
  symrepr_del();
  heap_del();
  extensions_del();

  int res = 0;

  res = symrepr_init();
  if (res)
    chprintf(chp,"Symrepr initialized.\n\r");
  else {
    chprintf(chp,"Error initializing symrepr!\n\r");
    return res;
  }
  
  res = heap_init(heap_size);
  if (res)
    chprintf(chp,"Heap initialized. Free cons cells: %u\n\r", heap_num_free());
  else {
    chprintf(chp,"Error initializing heap!\n\r");
    return res;
  }

  res = eval_cps_init(false);
  if (res)
    chprintf(chp,"Evaluator initialized.\n\r");
  else {
    chprintf(chp,"Error initializing evaluator.\n\r");
    return res;
  }
  
  res = extensions_add("print", ext_print);
  if (res)
    chprintf(chp,"Extension added.\n\r");
  else
    chprintf(chp,"Error adding extension.\n\r");

  VALUE prelude = prelude_load();
  eval_cps_program(prelude);

  chprintf(chp,"Lisp REPL started (ChibiOS)!\n\r");
  
  return res;
}


static THD_FUNCTION(repl, arg) {

  (void) arg;
  
  size_t len = 1024;
  char *str = malloc(1024);
  char *outbuf = malloc(2048);

  heap_state_t heap_state;

  int heap_size = 2048;

  reset_repl(heap_size);

  while (1) {
    chprintf(chp,"# ");
    memset(str,0,len);
    memset(outbuf,0, 2048);
    inputline(chp,str, len);
    chprintf(chp,"\n\r");

    if (strncmp(str, ":reset", 6) == 0) {
      reset_repl(heap_size);
      continue;
    } else if (strncmp(str, ":info", 5) == 0) {
      chprintf(chp,"##(ChibiOS)#################################################\n\r");
      chprintf(chp,"Used cons cells: %lu \n\r", heap_size - heap_num_free());
      chprintf(chp,"ENV: "); simple_snprint(outbuf,2048, eval_cps_get_env()); chprintf(chp, "%s \n\r", outbuf);
      heap_get_state(&heap_state);
      chprintf(chp,"GC counter: %lu\n\r", heap_state.gc_num);
      chprintf(chp,"Recovered: %lu\n\r", heap_state.gc_recovered);
      chprintf(chp,"Marked: %lu\n\r", heap_state.gc_marked);
      chprintf(chp,"Free cons cells: %lu\n\r", heap_num_free());
      chprintf(chp,"############################################################\n\r");
      memset(outbuf,0, 2048);
    } else if (strncmp(str, ":quit", 5) == 0) {
      break;
    } else {

      VALUE t;
      t = tokpar_parse(str);

      t = eval_cps_program(t);

      if (dec_sym(t) == symrepr_eerror()) {
	chprintf(chp,"Error\n");
      } else {
	chprintf(chp,"> "); simple_snprint(outbuf, 2048, t); chprintf(chp,"%s \n\r", outbuf);
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
	
	chThdCreateFromHeap(NULL, REPL_WA_SIZE,
			    "repl", NORMALPRIO + 1,
			    repl, (void *)NULL);
	

	
	while(1) { 
	  chThdSleepMilliseconds(500);
	}

}
