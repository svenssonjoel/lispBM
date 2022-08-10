/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se
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

#include "platform_uart.h"
#include "lbm_types.h"
#include "symrepr.h"
#include "heap.h"
#include "extensions.h"
#include "streams.h"
#include <string.h>

#include "platform_chibios_conf.h"

#if !defined(LBM_UART_0) && !defined(LBM_UART_1) && !defined(LBM_UART_2) && !defined(LBM_UART_3)
#warning No UART configured
#endif

// 4 uarts made available
#ifdef LBM_UART_0
static SerialConfig uart_cfg0 = {
  115200, 0, 0, 0
};
#endif
#ifdef LBM_UART_1
static SerialConfig uart_cfg1 = {
  115200, 0, 0, 0
};
#endif
#ifdef LBM_UART_2
static SerialConfig uart_cfg2 = {
  115200, 0, 0, 0
};
#endif
#ifdef LBM_UART_3
static SerialConfig uart_cfg3 = {
  115200, 0, 0, 0
};
#endif

static SerialConfig *get_uart_config(int uart) {
  switch(uart) {
  #ifdef LBM_UART_0
  case 0: return &uart_cfg0;
  #endif
  #ifdef LBM_UART_1
  case 1: return &uart_cfg1;
  #endif
  #ifdef LBM_UART_2
  case 2: return &uart_cfg2;
  #endif
  #ifdef LBM_UART_3
  case 3: return &uart_cfg3;
  #endif
  }
  return NULL;
}

static SerialDriver *get_uart_driver(int uart) {
  switch(uart) {
  #ifdef LBM_UART_0
  case 0: return &LBM_UART_0;
  #endif
  #ifdef LBM_UART_1
  case 1: return &LBM_UART_1;
  #endif
  #ifdef LBM_UART_2
  case 2: return &LBM_UART_2;
  #endif
  #ifdef LBM_UART_3
  case 3: return &LBM_UART_3;
  #endif
  }
  return NULL;
}

static ioportid_t get_uart_tx_gpio(int uart) {
  switch(uart) {
  #ifdef LBM_UART_0
  case 0: return LBM_UART_0_TX_GPIO;
  #endif
  #ifdef LBM_UART_1
  case 1: return LBM_UART_1_TX_GPIO;
  #endif
  #ifdef LBM_UART_2
  case 2: return LBM_UART_2_TX_GPIO;
  #endif
  #ifdef LBM_UART_3
  case 3: return LBM_UART_3_TX_GPIO;
  #endif
  }
  return NULL;
}

static ioportid_t get_uart_rx_gpio(int uart) {
  switch(uart) {
  #ifdef LBM_UART_0
  case 0: return LBM_UART_0_RX_GPIO;
  #endif
  #ifdef LBM_UART_1
  case 1: return LBM_UART_1_RX_GPIO;
  #endif
  #ifdef LBM_UART_2
  case 2: return LBM_UART_2_RX_GPIO;
  #endif
  #ifdef LBM_UART_3
  case 3: return LBM_UART_3_RX_GPIO;
  #endif
  }
  return NULL;
}

static int get_uart_tx_pin(int uart) {
  switch(uart) {
  #ifdef LBM_UART_0
  case 0: return LBM_UART_0_TX_PIN;
  #endif
  #ifdef LBM_UART_1
  case 1: return LBM_UART_1_TX_PIN;
  #endif
  #ifdef LBM_UART_2
  case 2: return LBM_UART_2_TX_PIN;
  #endif
  #ifdef LBM_UART_3
  case 3: return LBM_UART_3_TX_PIN;
  #endif
  }
  return -1;
}

static int get_uart_rx_pin(int uart) {
  switch(uart) {
  #ifdef LBM_UART_0
  case 0: return LBM_UART_0_RX_PIN;
  #endif
  #ifdef LBM_UART_1
  case 1: return LBM_UART_1_RX_PIN;
  #endif
  #ifdef LBM_UART_2
  case 2: return LBM_UART_2_RX_PIN;
  #endif
  #ifdef LBM_UART_3
  case 3: return LBM_UART_3_RX_PIN;
  #endif
  }
  return -1;
}

static int get_uart_tx_pin_mode(int uart) {
  switch(uart) {
  #ifdef LBM_UART_0
  case 0: return LBM_UART_0_TX_PIN_MODE;
  #endif
  #ifdef LBM_UART_1
  case 1: return LBM_UART_1_TX_PIN_MODE;
  #endif
  #ifdef LBM_UART_2
  case 2: return LBM_UART_2_TX_PIN_MODE;
  #endif
  #ifdef LBM_UART_3
  case 3: return LBM_UART_3_TX_PIN_MODE;
  #endif
  }
  return -1;
}

static int get_uart_rx_pin_mode(int uart) {
  switch(uart) {
  #ifdef LBM_UART_0
  case 0: return LBM_UART_0_RX_PIN_MODE;
  #endif
  #ifdef LBM_UART_1
  case 1: return LBM_UART_1_RX_PIN_MODE;
  #endif
  #ifdef LBM_UART_2
  case 2: return LBM_UART_2_RX_PIN_MODE;
  #endif
  #ifdef LBM_UART_3
  case 3: return LBM_UART_3_RX_PIN_MODE;
  #endif
  }
  return -1;
}


lbm_value ext_uart_init(lbm_value *args, lbm_uint argn){

  if (argn != 2)
    return ENC_SYM_NIL;

  if (!lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;

  lbm_uint baud = lbm_dec_as_u32(args[1]);
  lbm_uint uart = lbm_dec_as_u32(args[0]);

  SerialConfig *cfg = get_uart_config(uart);
  SerialDriver *drv = get_uart_driver(uart);
  if (cfg && drv) {

    memset(cfg, 0 , sizeof(SerialConfig));
    cfg->speed = baud;
    palSetPadMode(get_uart_tx_gpio(uart),
                  get_uart_tx_pin(uart),
                  get_uart_tx_pin_mode(uart));
    palSetPadMode(get_uart_rx_gpio(uart),
                  get_uart_rx_pin(uart),
                  get_uart_rx_pin_mode(uart));
    sdStart(get_uart_driver(uart), cfg);
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_NIL;
}


static lbm_value ext_uart_write(lbm_value *args, lbm_uint argn) {

  if (argn != 2 || (!lbm_is_number(args[0]) ||
                    (lbm_type_of(args[1]) != LBM_TYPE_CONS &&
                     lbm_type_of(args[1]) != LBM_TYPE_ARRAY))) {
    return ENC_SYM_TERROR;
  }

  const int max_len = 20;
  uint8_t to_send[max_len];
  uint8_t *to_send_ptr = to_send;
  int ind = 0;

  int uart = lbm_dec_as_i32(args[0]);

  if (lbm_type_of(args[1]) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
    if (array->elt_type != LBM_TYPE_BYTE) {
      return lbm_enc_sym(SYM_EERROR);
    }

    to_send_ptr = (uint8_t*)array->data;
    ind = array->size;
  } else {
    lbm_value curr = args[1];
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      lbm_value  arg = lbm_car(curr);

      if (lbm_is_number(arg)) {
        to_send[ind++] = lbm_dec_as_u32(arg);
      } else {
        return lbm_enc_sym(SYM_EERROR);
      }

      if (ind == max_len) {
        break;
      }

      curr = lbm_cdr(curr);
    }
  }

  SerialDriver *drv = get_uart_driver(uart);
  if (drv) {
    sdWrite(drv, to_send_ptr, ind);
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_NIL;
}

static lbm_value ext_uart_read(lbm_value *args, lbm_uint argn) {
  if ((argn != 3 && argn != 4 && argn != 5) ||
      !lbm_is_number(args[0]) ||
      lbm_type_of(args[1]) != LBM_TYPE_ARRAY || !lbm_is_number(args[2])) {
    return lbm_enc_sym(SYM_TERROR);
  }

  int uart = lbm_dec_as_i32(args[0]);

  SerialDriver *drv = get_uart_driver(uart);
  if (!drv) {
    return lbm_enc_sym(SYM_EERROR);
  }

  unsigned int num = lbm_dec_as_u32(args[2]);
  if (num > 512) {
    return lbm_enc_sym(SYM_TERROR);
  }

  if (num == 0) {
    return lbm_enc_i(0);
  }

  unsigned int offset = 0;
  if (argn >= 4) {
    if (!lbm_is_number(args[3])) {
      return lbm_enc_sym(SYM_TERROR);
    }
    offset = lbm_dec_as_u32(args[2]);
  }

  int stop_at = -1;
  if (argn >= 5) {
    if (!lbm_is_number(args[4])) {
      return lbm_enc_sym(SYM_TERROR);
    }
    stop_at = lbm_dec_as_u32(args[4]);
  }

  lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
  if (array->elt_type != LBM_TYPE_BYTE || array->size < (num + offset)) {
    return lbm_enc_sym(SYM_EERROR);
  }

  unsigned int count = 0;
  msg_t res = sdGetTimeout(drv, TIME_IMMEDIATE);
  while (res != MSG_TIMEOUT) {
    ((uint8_t*)array->data)[offset + count] = (uint8_t)res;
    count++;
    if (res == stop_at || count >= num) {
      break;
    }
    res = sdGetTimeout(drv, TIME_IMMEDIATE);
  }

  return lbm_enc_i(count);
}


// Uart low-level character stream interface
typedef struct {
  char *buffer;
  int  read;
  int  write;
  int  size;
  int  uart;
  bool more;
  int  row;
  int  column;
} uart_char_stream_state;

static bool buffer_empty(uart_char_stream_state *s) {
  return (s->write == s->read);
}

static bool buffer_full(uart_char_stream_state *s) {
  return (s->write == s->read - 1 ||
	  (s->read == 0 && s->write == s->size - 1));
}

static bool buffer_put(uart_char_stream_state *s, char c) {

  if (buffer_full(s)) return false;

  s->buffer[s->write] = c;
  s->write = (s->write + 1) % s->size;

  return true;
}

static bool buffer_get(uart_char_stream_state *s, char *c) {

  if (s->read == s->write) return false;

  *c = s->buffer[s->read];
  s->read = (s->read + 1) % s->size;
}

static bool buffer_peek(uart_char_stream_state *s, int n, char *c) {

  if (s->read + n < s->write) {
    c = s->buffer[s->read + n];
    return true;
  }
  return false;
}


static bool uart_stream_more(lbm_tokenizer_char_stream_t *t) {
  uart_char_stream_state *state;
  state = (uart_char_stream_state*)t->state;
  return state->more;
}

static char uart_stream_get(lbm_tokenizer_char_stream_t *t) {

  char c;
  uart_char_stream_state *s = (uart_char_stream_state *)t->state;
  if (!buffer_empty(s)) {
    buffer_get(s, &c);
    return c;
  } else {

    SerialDriver *drv = get_uart_driver(s->uart);
    if (!drv) {
      return lbm_enc_sym(SYM_EERROR);
    }

    int num_read = 0;

    msg_t res;
    do {
      res = sdGetTimeout(drv, TIME_IMMEDIATE);
      if (res != MSG_TIMEOUT && res != 0x1A) {
	buffer_put(t, res);
	num_read ++;
      }
    } while (!buffer_full(t) && res != MSG_TIMEOUT && res != 0x1A);

    if (res == 0x1A) s->more = false;

    // This blocking behaviour is unfortunate.
    // Doing it the right way may be a big pain though.
    if (num_read == 0) {
      c = sdGet(drv);
      if (c == 0x1A ) s->more = false;
      return c;
    } else {
      buffer_get(s, &c);
      return c;
    }
  }
}

static bool uart_stream_put(lbm_tokenizer_char_stream_t *t, char c) {
  uart_char_stream_state *state;
  state = (uart_char_stream_state*)t->state;

  SerialDriver *drv = get_uart_driver(state->uart);
  if (drv) {
    sdWrite(drv, &c, 1);
    return true;
  }
  return false;
}

static char uart_stream_peek(lbm_tokenizer_char_stream_t *t, unsigned int n) {
  return 'p';
}

static void uart_stream_drop(lbm_tokenizer_char_stream_t *t, unsigned int n) {
  for (int i = 0; i < n; i ++) {
    uart_stream_get(t);
  }
}

static unsigned int uart_stream_row(lbm_tokenizer_char_stream_t *t) {
  (void)t;
  return 0;
}

static unsigned int uart_stream_column(lbm_tokenizer_char_stream_t *t) {
  (void) t;
  return 0;
}


static lbm_tokenizer_char_stream_t *create_uart_char_stream(int uart, int buffer_size) {

  lbm_tokenizer_char_stream_t *str = NULL;
  uart_char_stream_state *state = NULL;
  char *buffer = NULL;

  buffer = (char *)lbm_memory_allocate(1 + buffer_size / sizeof(lbm_uint));
  if (!buffer) {
    return NULL;
  }
  str = (lbm_tokenizer_char_stream_t *)lbm_memory_allocate(1 + sizeof(lbm_tokenizer_char_stream_t) / sizeof(lbm_uint));
  if (!str) {
    lbm_memory_free((lbm_uint*)buffer);
    return NULL;
  }
  state = (uart_char_stream_state *)lbm_memory_allocate(1 + sizeof(uart_char_stream_state) / sizeof(lbm_uint));
  if (!state) {
    lbm_memory_free((lbm_uint*)str);
    lbm_memory_free((lbm_uint*)buffer);
    return NULL;
  }

  state->buffer = buffer;
  state->size   = buffer_size;
  state->write  = 0;
  state->read   = 0;
  state->uart   = uart;
  state->more   = true; // will remain true until an EOF arrives on stream

  str->state = (void*)state;
  str->more  = uart_stream_more;
  str->get   = uart_stream_get;
  str->put   = uart_stream_put;
  str->peek  = uart_stream_peek;
  str->drop  = uart_stream_drop;
  str->row   = uart_stream_row;
  str->column = uart_stream_column;

  return str;
}

// Stream extension

static lbm_value ext_uart_stream(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1])) {
    return ENC_SYM_TERROR;
  }

  int uart = lbm_dec_as_i32(args[0]);
  int buffer_size = lbm_dec_as_i32(args[1]);

  lbm_tokenizer_char_stream_t *tstr = create_uart_char_stream(uart, buffer_size);
  if (tstr == NULL)
    return ENC_SYM_MERROR;

  return lbm_stream_lift(tstr);
}


// Interface

bool platform_uart_init(void) {
  int res = 1;

  res = res && lbm_add_extension("uart-init", ext_uart_init);
  res = res && lbm_add_extension("uart-write", ext_uart_write);
  res = res && lbm_add_extension("uart-read", ext_uart_read);
  res = res && lbm_add_extension("uart-stream", ext_uart_stream);

  return res;
}
