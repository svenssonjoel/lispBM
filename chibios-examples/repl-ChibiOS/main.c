/*
    Copyright 2019, 2022  Joel Svensson        svenssonjoel@yahoo.se
                          Benjamin Vedder

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

#define EVAL_WA_SIZE THD_WORKING_AREA_SIZE(1024)
#define EVAL_CPS_STACK_SIZE 256

#define HEAP_SIZE 2048

cons_t heap[HEAP_SIZE];

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

char print_output[1024];
char error_output[1024]; 

void done_callback(eval_context_t *ctx) {

  char *output = print_output;
  char *error  = error_output;

  CID cid = ctx->id;
  VALUE t = ctx->r;

  int print_ret = print_value(output, 1024, error, 1024, t);

  if (print_ret >= 0) {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, output);
  } else {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, error);
  }
}

uint32_t timestamp_callback(void) {
  systime_t t = chVTGetSystemTime();
  return (uint32_t) (100 * t);
}

void sleep_callback(uint32_t us) {
  chThdSleepMicroseconds(us);
}

static THD_FUNCTION(eval, arg) {
  (void) arg;
  eval_cps_run_eval();
}

/* ext_print is atomic from the point of view of the lisp RTS */
VALUE ext_print(VALUE *args, UINT argn) {

  char *output = print_output;
  char *error = error_output;

  for (UINT i = 0; i < argn; i ++) {
    VALUE t = args[i];

    if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
      array_header_t *array = (array_header_t *)car(t);
      switch (array->elt_type){
      case VAL_TYPE_CHAR:
        chprintf(chp,"%s", (char*)array + 8);
        break;
      default:
        return enc_sym(SYM_NIL);
        break;
      }
    } else if (val_type(t) == VAL_TYPE_CHAR) {
      if (dec_char(t) =='\n') {
        chprintf(chp, "\r\n");
      } else {
        chprintf(chp,"%c", dec_char(t));
      }
    }  else {
      int print_ret = print_value(output, 1024, error, 1024, t);

      if (print_ret >= 0) {
        chprintf(chp,"%s", output);
      } else {
        chprintf(chp,"%s", error);
      }
    }
  }
  return enc_sym(SYM_TRUE);
}

char str[1024];
char outbuf[2048];
char error[1024];
char file_buffer[2048];


void print_ctx_info(eval_context_t *ctx, void *aux) {
  int print_ret = print_value(outbuf, 2048, error, 1024, ctx->r);
  chprintf(chp, "%s %x %u %u %s\r\n", (char*)aux, (uint32_t)ctx, ctx->id, ctx->K.sp, print_ret ? outbuf : error );   
}


unsigned char memory_array[MEMORY_SIZE_8K];
unsigned char bitmap_array[MEMORY_BITMAP_SIZE_8K];

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

  size_t len = 1024;

  int res = 0;

  heap_state_t heap_state;

  chThdSleepMilliseconds(2000);

  res = memory_init(memory_array, MEMORY_SIZE_8K,
                    bitmap_array, MEMORY_BITMAP_SIZE_8K);
  if (res)
    chprintf(chp,"Memory initialized. Memory size: %u Words. Free: %u Words.\r\n", memory_num_words(), memory_num_free());
  else {
    chprintf(chp,"Error initializing memory!\r\n");
    return 0;
  }

   res = symrepr_init();
  if (res)
    chprintf(chp,"Symrepr initialized.\r\n");
  else {
    chprintf(chp,"Error initializing symrepr!\r\n");
    return 0;
  }

  res = heap_init(heap, HEAP_SIZE);
  if (res)
    chprintf(chp,"Heap initialized. Free cons cells: %u\r\n", heap_num_free());
  else {
    chprintf(chp,"Error initializing heap!\r\n");
    return 0;
  }

  res = eval_cps_init();
  if (res)
    chprintf(chp,"Evaluator initialized.\r\n");
  else {
    chprintf(chp,"Error initializing evaluator.\r\n");
    return 0;
  }

  eval_cps_set_ctx_done_callback(done_callback);
  eval_cps_set_timestamp_us_callback(timestamp_callback);
  eval_cps_set_usleep_callback(sleep_callback);

  res = extensions_add("print", ext_print);
  if (res)
    chprintf(chp,"Extension added.\r\n");
  else
    chprintf(chp,"Error adding extension.\r\n");

  thread_t *t = chThdCreateFromHeap(NULL, EVAL_WA_SIZE,
                                    "eval", NORMALPRIO+1,
                                    eval, (void *)NULL);

  if (!t) {
    chprintf(chp,"Error starting evaluator thread.\r\n");
    return 0;
  }

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
      chprintf(chp,"------------------------------------------------------------\r\n");
      chprintf(chp,"Used cons cells: %lu \r\n", heap_size - heap_num_free());
      chprintf(chp,"Free cons cells: %lu\r\n", heap_num_free());
      heap_get_state(&heap_state);
      chprintf(chp,"GC counter: %lu\r\n", heap_state.gc_num);
      chprintf(chp,"Recovered: %lu\r\n", heap_state.gc_recovered);
      chprintf(chp,"Marked: %lu\r\n", heap_state.gc_marked);
      
      chprintf(chp,"Array and symbol string memory:\r\n");
      chprintf(chp,"  Size: %u 32Bit words\r\n", memory_num_words());
      chprintf(chp,"  Free: %u 32Bit words\r\n", memory_num_free());
      chprintf(chp,"------------------------------------------------------------\r\n");
      memset(outbuf,0, 2048);
    } else if (strncmp(str, ":env", 4) == 0) {
      VALUE curr = *env_get_global_ptr();
      chprintf(chp,"Environment:\r\n");
      while (type_of(curr) == PTR_TYPE_CONS) {
        res = print_value(outbuf,2048, error, 1024, car(curr));
        curr = cdr(curr);

        if (res >= 0) {
          chprintf(chp,"  %s \r\n", outbuf);
        } else {
          chprintf(chp,"  %s\r\n",error);
        }
      }
    } else if (strncmp(str, ":threads", 8) == 0) {
      thread_t *tp;
      static const char *states[] = {CH_STATE_NAMES};
      chprintf(chp, "    addr prio refs     state           name time    \r\n");
      chprintf(chp, "-------------------------------------------------------------------\r\n");
      tp = chRegFirstThread();
      do {
        chprintf(chp, "%.8lx %4lu %4lu %9s %14s %lu (%.1f %%)\r\n",
                 (uint32_t)tp,
                 (uint32_t)tp->prio, (uint32_t)(tp->refs - 1),
                 states[tp->state], tp->name, (uint32_t)(tp->time - tp->time_last),
                 (double)(100.0 * (float)(tp->time - tp->time_last) / (float)(chVTGetSystemTimeX() - tp->time_last)));
        tp->time_last = tp->time;
        tp = chRegNextThread(tp);
      } while (tp != NULL);
    } else if (strncmp(str, ":mem", 4) == 0) {
      size_t n, size, sizel;
      n = chHeapStatus(NULL, &size, &sizel);
      chprintf(chp, "core free memory  : %u bytes\r\n", chCoreGetStatusX());
      chprintf(chp, "heap fragments    : %u\r\n", n);
      chprintf(chp, "heap free largest : %u bytes\r\n", sizel);
      chprintf(chp, "heap free total   : %u bytes\n\r\n", size);
    } else if (strncmp(str, ":ctxs", 5) == 0) {
      eval_cps_running_iterator(print_ctx_info, "RUNNING");
      eval_cps_blocked_iterator(print_ctx_info, "BLOCKED");
      eval_cps_done_iterator   (print_ctx_info, "DONE");
    }else if (strncmp(str, ":quit", 5) == 0) {
      break;
    } else if (strncmp(str, ":read", 5) == 0) {
      memset(file_buffer, 0, 2048);
      bool done = false;
      int c;

      for (int i = 0; i < 2048; i ++) {
        c = streamGet(chp);

        if (c == 4 || c == 26 || c == STM_RESET) {
          done = true;
          break;
        }
        file_buffer[i] = (char)c;
      }

      chprintf(chp, "%s\r\n", file_buffer);
      chprintf(chp, "received %d bytes\r\n", strlen(file_buffer));

      if (done) {
        VALUE t;
        t = tokpar_parse(file_buffer);
        CID cid = eval_cps_program(t);
        if (cid == 0) {
          chprintf(chp,"Error creating ctx\r\n");
        } else {
          chprintf(chp,"started ctx: %u\r\n", cid);
        }
      }
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
}

