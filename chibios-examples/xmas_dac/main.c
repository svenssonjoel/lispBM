/*
    Copyright 2021, 2022 Joel Svensson	svenssonjoel@yahoo.se

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
#include "hal_pal.h"
#include "chvt.h"
#include "chtime.h"

#include "usbcfg.h"
#include "chprintf.h"

#include "lispbm.h"

#define EVAL_WA_SIZE THD_WORKING_AREA_SIZE(1024)
#define EVAL_CPS_STACK_SIZE 256

#define HEAP_SIZE 8192

cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));


BaseSequentialStream *chp = NULL;

#define DAC0_CH   1
#define DAC0_GPIO GPIOA
#define DAC0_PIN  4
#define DAC0_MODE PAL_MODE_INPUT_ANALOG

DACConfig dacc;
DACDriver *dacd;

#define DACTHREAD_WA_SIZE THD_WORKING_AREA_SIZE(512)
static THD_WORKING_AREA(dac_wa, DACTHREAD_WA_SIZE);

#define MAX_MESSAGES 10
static mailbox_t mb;
static msg_t b[MAX_MESSAGES];

typedef struct {
  uint32_t type;
  uint32_t data[4];
} dac_msg_t;


static dac_msg_t msgs[MAX_MESSAGES] __attribute__((aligned((4))));

static MEMORYPOOL_DECL(msg_pool, sizeof (dac_msg_t), PORT_NATURAL_ALIGN, NULL);

static int send_to_dac(dac_msg_t msg) {
  int r = 1;
  dac_msg_t* m = (dac_msg_t*)chPoolAllocI(&msg_pool);

  if (m) {
    *m = msg;
    msg_t msg_val = chMBPostI(&mb, (uint32_t) m);
    if (msg_val != MSG_OK) {
      chPoolFree(&msg_pool, (void*) m);
      r = 0;
    }
  }
  return r;
}

static int read_msg(dac_msg_t *msg) {

  msg_t msg_value;

  int r = chMBFetchTimeout(&mb, &msg_value, 0);

  if (r == MSG_OK ) {

    *msg = *(dac_msg_t*)msg_value;

    chPoolFree(&msg_pool, (void*)msg_value);
    r = 1;
  } else {
    r = 0;
  }

  return r;
}

// Message types
#define NOTE 100

static THD_FUNCTION(dac_thread, arg) {
  (void) arg;

  dac_msg_t msg;

  uint32_t half_period = 1136;
  uint32_t vol_mask = 0;
  uint32_t volume = 4095;
  uint     s = 1;

  while (1) {


    if (read_msg(&msg)) {
      switch (msg.type) {
      case NOTE:
        half_period = msg.data[0];
        vol_mask    = msg.data[1];
        break;
      default:
        break;
      }
    }

    dacPutChannelX(dacd, 0, s * (volume & vol_mask));

    chThdSleepMicroseconds(half_period); // 440Hz
    s = 1 - s;
  }
}

void init_dac(void) {

  dacd = &DACD1;

  dacc.init = 0u;
  dacc.datamode = DAC_DHRM_12BIT_RIGHT;
  dacc.cr = 0;

  palSetPadMode(DAC0_GPIO, DAC0_PIN, DAC0_MODE);

  dacStart(dacd, &dacc);
}


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

  CID cid = ctx->id;
  VALUE t = ctx->r;

  int print_ret = print_value(output, 1024, t);

  if (print_ret >= 0) {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, output);
  } else {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, output);
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


VALUE ext_print(VALUE *args, UINT argn) {

  char output[1024];

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
      int print_ret = print_value(output, 1024, t);
      chprintf(chp,"%s", output);
    }
  }
  return enc_sym(SYM_TRUE);
}

VALUE ext_note(VALUE *args, UINT argn) {

  if (argn != 2) {
    return enc_sym(SYM_NIL);
  }

  dac_msg_t msg;
  msg.type = NOTE;
  msg.data[0] = dec_u(args[0]);
  msg.data[1] = dec_u(args[1]);

  chSysLock();
  send_to_dac(msg);
  chSysUnlock();
  return enc_sym(SYM_TRUE);
}



static uint32_t memory_array[MEMORY_SIZE_8K];
static uint32_t bitmap_array[MEMORY_BITMAP_SIZE_8K];

char str[1024];
char outbuf[2048];
char error[1024];
char file_buffer[4096];

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

  chprintf(chp,"\r\n\r\nStarting up\r\n\r\n");

  init_dac();
  chPoolLoadArray(&msg_pool,msgs, MAX_MESSAGES);
  chMBObjectInit(&mb, b, MAX_MESSAGES);


  chThdCreateStatic(dac_wa,
                    sizeof dac_wa,
                    NORMALPRIO,
                    dac_thread,
                    NULL);

  chThdSleepMilliseconds(1); // start up the dac thread now


  if (!lispbm_init(heap, HEAP_SIZE,
                   memory_array, MEMORY_SIZE_8K,
                   bitmap_array, MEMORY_BITMAP_SIZE_8K)) {
    chprintf(chp,"Initializing LispBM failed\r\n");
    return 0;
  }

  eval_cps_set_ctx_done_callback(done_callback);
  eval_cps_set_timestamp_us_callback(timestamp_callback);
  eval_cps_set_usleep_callback(sleep_callback);

  res = extensions_add("print", ext_print);
  if (res)
    chprintf(chp,"Extension added: print.\r\n");
  else
    chprintf(chp,"Error adding extension.\r\n");

  res = extensions_add("note", ext_note);
  if (res)
    chprintf(chp,"Extension added: note.\r\n");
  else
    chprintf(chp,"Error adding extension.\r\n");


  thread_t *t = chThdCreateFromHeap(NULL, EVAL_WA_SIZE,
                                    "eval", NORMALPRIO+1,
                                    eval, (void *)NULL);

  if (!t) {
    chprintf(chp,"Error starting evaluator thread.\r\n");
    return 1;
  }

  VALUE prelude = prelude_load();
  eval_cps_program(prelude);

  chprintf(chp,"Lisp REPL started (ChibiOS)!\r\n");

  while (1) {
    chprintf(chp,"# ");
    memset(str,0,len);
    memset(outbuf,0, 1024);
    inputline(chp,str, len);
    chprintf(chp,"\r\n");

    if (strncmp(str, ":info", 5) == 0) {
      chprintf(chp,"##(ChibiOS)#################################################\r\n");
      chprintf(chp,"Used cons cells: %lu \r\n", HEAP_SIZE - heap_num_free());
      res = print_value(outbuf,1024, *env_get_global_ptr());
      chprintf(chp,"%s\r\n",outbuf);

      heap_get_state(&heap_state);
      chprintf(chp,"GC counter: %lu\r\n", heap_state.gc_num);
      chprintf(chp,"Recovered: %lu\r\n", heap_state.gc_recovered);
      chprintf(chp,"Marked: %lu\r\n", heap_state.gc_marked);
      chprintf(chp,"Free cons cells: %lu\r\n", heap_num_free());
      chprintf(chp,"############################################################\r\n");
      memset(outbuf,0, 1024);
    } else if (strncmp(str, ":quit", 5) == 0) {
      break;
    } else if (strncmp(str, ":wait", 5) == 0) {
      int cid = atoi(str+5);
      chprintf(chp,"waiting for cid: %d\r\n", cid);
      eval_cps_wait_ctx(cid);
    } else if (strncmp(str, ":read", 5) == 0) {
      memset(file_buffer, 0, 4096);
      bool done = false;
      int c;

      for (int i = 0; i < 4096; i ++) {
        c = streamGet(chp);

        if (c == 4 || c == 26 || c == STM_RESET) {
          done = true;
          break;
        }
        file_buffer[i] = (char)c;
      }


      //chThdSleepMilliseconds(100);
      chprintf(chp, "%s\r\n", file_buffer);
      chprintf(chp, "received %d bytes\r\n", strlen(file_buffer));

      if (done) {
        VALUE t;

        chprintf(chp, "Parsing file\r\n");
        t = tokpar_parse(file_buffer);
        chprintf(chp, "Evaluating contents\r\n");
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
}
