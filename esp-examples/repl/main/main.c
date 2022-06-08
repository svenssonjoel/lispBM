/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se

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

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include "sdkconfig.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_chip_info.h"
#include "esp_spi_flash.h"
#include "driver/uart.h"

#include "lispbm.h"
#include "lbm_llama_ascii.h"
#include "lbm_version.h"


#include "extensions/array_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"

#include "lbm_custom_type.h"

#define UART_NUM      0
#define UART_BAUDRATE 115200
#define UART_TX       21
#define UART_RX       20


void uart_init(void) {
	uart_config_t uart_config = {
			.baud_rate = UART_BAUDRATE,
			.data_bits = UART_DATA_8_BITS,
			.parity    = UART_PARITY_DISABLE,
			.stop_bits = UART_STOP_BITS_1,
			.flow_ctrl = UART_HW_FLOWCTRL_DISABLE,
			.source_clk = UART_SCLK_DEFAULT,
	};

	uart_driver_install(UART_NUM, 512, 512, 0, 0, 0);
	uart_param_config(UART_NUM, &uart_config);
	uart_set_pin(UART_NUM, UART_TX, UART_RX, -1, -1);
}

int get_char(void) {
	uint8_t c;
	int r = 0;
	do {
		r = uart_read_bytes(UART_NUM, &c, 1, portMAX_DELAY);
	} while (r == 0);
	return (int)c;
}

void uart_printf(const char* fmt, ...) {
	char buffer[256];
	va_list args;
	va_start (args, fmt);
	int n = vsnprintf (buffer,256,fmt, args);
	va_end (args);
	if (n > 0) {
		uart_write_bytes(UART_NUM, buffer, n);
	}
}

void put_char(char c) {
   uart_write_bytes(UART_NUM, &c, 1);
}

int inputline(char *buffer, int size) {
  int n = 0;
  unsigned char c;

  for (n = 0; n < size - 1; n++) {

    c = get_char();
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      put_char(0x8); /* output backspace character */
      put_char(' ');
      put_char(0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        put_char(c);
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

#define EVAL_CPS_STACK_SIZE 256
#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define EXTENSION_STORAGE_SIZE 256
#define VARIABLE_STORAGE_SIZE 256
#define WAIT_TIMEOUT 2500
#define STR_SIZE 1024
#define HEAP_SIZE 2048
#define PRINT_SIZE 1024

lbm_uint gc_stack_storage[GC_STACK_SIZE];
lbm_uint print_stack_storage[PRINT_STACK_SIZE];
extension_fptr extension_storage[EXTENSION_STORAGE_SIZE];
lbm_value variable_storage[VARIABLE_STORAGE_SIZE];

static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));

static lbm_uint memory[LBM_MEMORY_SIZE_8K];
static lbm_uint bitmap[LBM_MEMORY_BITMAP_SIZE_8K];

static lbm_tokenizer_string_state_t string_tok_state;
static lbm_tokenizer_char_stream_t string_tok;

static char print_output[PRINT_SIZE];

void eval_thd_wrapper(void *v) {
  lbm_run_eval();
}

void done_callback(eval_context_t *ctx) {

	char *output = print_output;

	lbm_cid cid = ctx->id;
	lbm_value t = ctx->r;

	int print_ret = lbm_print_value(output, PRINT_SIZE, t);

	if (print_ret >= 0) {
		uart_printf("<< Context %d finished with value %s >>\r\n# ", cid, output);
	} else {
		uart_printf("<< Context %d finished with value %s >>\r\n# ", cid, output);
	}
}

uint32_t timestamp_callback(void) {
	TickType_t t = xTaskGetTickCount();
	return (uint32_t) (100 * t);
}

void sleep_callback(uint32_t us) {
	vTaskDelay(us / 100);
}

lbm_value ext_print(lbm_value *args, lbm_uint argn) {

  char *output = print_output;

  for (lbm_uint i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      switch (array->elt_type){
      case LBM_TYPE_CHAR:
        uart_printf("%s", (char*)array->data);
        break;
      default:
        return lbm_enc_sym(SYM_NIL);
        break;
      }
    } else if (lbm_type_of(t) == LBM_TYPE_CHAR) {
      if (lbm_dec_char(t) =='\n') {
        uart_printf("\r\n");
      } else {
        uart_printf("%c", lbm_dec_char(t));
      }
    }  else {
      lbm_print_value(output, 1024, t);
      uart_printf("%s", output);
    }
  }
  return lbm_enc_sym(SYM_TRUE);
}

static char str[1024];
static char outbuf[1024];

void app_main(void)
{

	int res = 0;
	lbm_heap_state_t heap_state;


	vTaskDelay(1000);
	uart_init();


	if (!lbm_init(heap, HEAP_SIZE,
			gc_stack_storage, GC_STACK_SIZE,
			memory, LBM_MEMORY_SIZE_8K,
			bitmap, LBM_MEMORY_BITMAP_SIZE_8K,
			print_stack_storage, PRINT_STACK_SIZE,
			extension_storage, EXTENSION_STORAGE_SIZE)) {
		uart_printf("LispBM Init failed.\r\n");
		return;
	}
	uart_printf("LispBM Initialized\n");

	lbm_set_ctx_done_callback(done_callback);
	lbm_set_timestamp_us_callback(timestamp_callback);
	lbm_set_usleep_callback(sleep_callback);

	res = lbm_add_extension("print", ext_print);
	if (res)
		uart_printf("Extension added.\r\n");
	else
		uart_printf("Error adding extension.\r\n");


	TaskHandle_t eval_thd = NULL;
	BaseType_t status = xTaskCreate(eval_thd_wrapper,
			"eval",
			4096,
			NULL,
			2,
			&eval_thd
	);

	if( status == pdPASS ) {
		uart_printf("Evaluator thread started\n");
		//vTaskDelete( xHandle );
	}

	uart_printf("LispBM Version %d.%d.%d\r\n", LBM_MAJOR_VERSION, LBM_MINOR_VERSION, LBM_PATCH_VERSION);
	uart_printf("Lisp REPL started (ESP32C3)\r\n");

	while (1) {
		uart_printf("# ");
		memset(str,0,1024);
		memset(outbuf,0, 1024);
		inputline(str, 1024);
		uart_printf("\r\n");
		if (strncmp(str, ":info", 5) == 0) {
			uart_printf("------------------------------------------------------------\r\n");
			uart_printf("Used cons cells: %lu \r\n", HEAP_SIZE - lbm_heap_num_free());
			uart_printf("Free cons cells: %lu\r\n", lbm_heap_num_free());
			lbm_get_heap_state(&heap_state);
			uart_printf("GC counter: %lu\r\n", heap_state.gc_num);
			uart_printf("Recovered: %lu\r\n", heap_state.gc_recovered);
			uart_printf("Marked: %lu\r\n", heap_state.gc_marked);

			uart_printf("Array and symbol string memory:\r\n");
			uart_printf("  Size: %u 32Bit words\r\n", lbm_memory_num_words());
			uart_printf("  Free: %u 32Bit words\r\n", lbm_memory_num_free());
			uart_printf("------------------------------------------------------------\r\n");
			memset(outbuf,0, 1024);
		}
	}
}
