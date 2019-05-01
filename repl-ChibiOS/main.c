
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

#include <stdio.h>
#include <string.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "ch.h"
#include "hal.h"

#include "shell.h"
#include "chprintf.h"

#include "usbcfg.h"

#include "heap.h"
#include "symrepr.h"
#include "builtin.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"


/*===========================================================================*/
/* Command line related.                                                     */
/*===========================================================================*/

#define SHELL_WA_SIZE   THD_WORKING_AREA_SIZE(2048)

unsigned char inbyte(BaseSequentialStream *chp) {
  unsigned char c;
  while (streamRead(chp, &c, 1) == 0);
  return c;
}

void outbyte(BaseSequentialStream *chp, char c) {
  streamPut(chp, c);
}

int inputline(BaseSequentialStream *chp, char *buffer, int size) {
  int n = 0;
  unsigned char c;
  for (n = 0; n < size - 1; n++) {

    c = inbyte(chp);
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      outbyte(chp,'\b'); /* output backspace character */
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        outbyte(chp,c);
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

/* Can be measured using dd if=/dev/xxxx of=/dev/null bs=512 count=10000.*/
static void cmd_repl(BaseSequentialStream *chp, int argc, char *argv[]) {

  (void) argc;
  (void) argv;

  size_t len = 1024;
  char *str = malloc(1024);
  char *outbuf = malloc(1024);
  int res = 0;

  heap_state_t heap_state;

  res = symrepr_init();
  if (res)
    chprintf(chp,"Symrepr initialized.\n\r");
  else {
    chprintf(chp,"Error initializing symrepr!\n\r");
    return;
  }
  int heap_size = 2048;
  res = heap_init(heap_size);
  if (res)
    chprintf(chp,"Heap initialized. Free cons cells: %u\n\r", heap_num_free());
  else {
    chprintf(chp,"Error initializing heap!\n\r");
    return;
  }
  res = builtin_init();
  if (res)
    chprintf(chp, "Built in functions initialized.\n\r");
  else {
    chprintf(chp, "Error initializing built in functions.\n\r");
    return;
  }
  res = eval_cps_init();
  if (res)
    chprintf(chp,"Evaluator initialized.\n\r");
  else {
    chprintf(chp,"Error initializing evaluator.\n\r");
  }
  chprintf(chp,"Lisp REPL started (ChibiOS)!\n\r");

  while (1) {
    chprintf(chp,"# ");
    memset(str,0,len);
    memset(outbuf,0, 1024);
    inputline(chp,str, len);
    chprintf(chp,"\n\r");

    if (strncmp(str, ":info", 5) == 0) {
      chprintf(chp,"##(ChibiOS)#################################################\n\r");
      chprintf(chp,"Used cons cells: %lu \n\r", heap_size - heap_num_free());
      chprintf(chp,"ENV: "); simple_snprint(outbuf,1023, eval_cps_get_env()); chprintf(chp, "%s \n\r", outbuf);
      //symrepr_print();
      //heap_perform_gc(eval_cps_get_env());
      heap_get_state(&heap_state);
      chprintf(chp,"GC counter: %lu\n\r", heap_state.gc_num);
      chprintf(chp,"Recovered: %lu\n\r", heap_state.gc_recovered);
      chprintf(chp,"Marked: %lu\n\r", heap_state.gc_marked);
      chprintf(chp,"Free cons cells: %lu\n\r", heap_num_free());
      chprintf(chp,"############################################################\n\r");
      memset(outbuf,0, 1024);
    } else if (strncmp(str, ":quit", 5) == 0) {
      break;
    } else {

      VALUE t;
      t = tokpar_parse(str);

      t = eval_cps_program(t);

      if (dec_sym(t) == symrepr_eerror()) {
	chprintf(chp,"Error\n");
      } else {
	chprintf(chp,"> "); simple_snprint(outbuf, 1023, t); chprintf(chp,"%s \n\r", outbuf);
      }
    }
  }

  symrepr_del();
  heap_del();
  builtin_del();
}

static const ShellCommand commands[] = {
		{"repl", cmd_repl},
		{NULL, NULL}
};

static const ShellConfig shell_cfg1 = {
		(BaseSequentialStream *)&SDU1,
		commands
};

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

	shellInit();

	/*
	 * Normal main() thread activity, spawning shells.
	 */
	while (true) {
		if (SDU1.config->usbp->state == USB_ACTIVE) {
			thread_t *shelltp = chThdCreateFromHeap(NULL, SHELL_WA_SIZE,
					"shell", NORMALPRIO + 1,
					shellThread, (void *)&shell_cfg1);
			chThdWait(shelltp);
		}
		chThdSleepMilliseconds(1000);
	}
}
