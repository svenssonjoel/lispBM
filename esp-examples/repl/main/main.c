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
#include <ctype.h>
#include "sdkconfig.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_chip_info.h"
#include "esp_spi_flash.h"
#include "driver/uart.h"

#include "lispbm.h"

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

int getChar(void) {
	uint8_t buf[1];
	int r = 0;
	do {
		r = uart_read_bytes(UART_NUM, buf, 1, portMAX_DELAY);
		if (r < 0) return -1;
	} while (r < 1);
	return (int)buf[0];
}

int inputline(char *buffer, unsigned int size) {
	unsigned int n = 0;
	int c;
	for (n = 0; n < size - 1; n++) {

		c = getchar(); // busy waiting.

		if (c < 0) {
			n--;
			struct timespec s;
			struct timespec r;
			s.tv_sec = 0;
			s.tv_nsec = (long)1000 * 1000;
			nanosleep(&s, &r);
			continue;
		}
		switch (c) {
		case 27:
			break;
		case 127: /* fall through to below */
		case '\b': /* backspace character received */
			if (n > 0)
				n--;
			buffer[n] = 0;
			//putchar(0x8); /* output backspace character */
			//putchar(' ');
			//putchar(0x8);
			n--; /* set up next iteration to deal with preceding char location */
			break;
		case '\n': /* fall through to \r */
		case '\r':
			buffer[n] = 0;
			return n;
		default:
			if (isprint(c)) { /* ignore non-printable characters */
				//putchar(c);
				buffer[n] = (char)c;
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

lbm_uint gc_stack_storage[GC_STACK_SIZE];
lbm_uint print_stack_storage[PRINT_STACK_SIZE];
extension_fptr extension_storage[EXTENSION_STORAGE_SIZE];
lbm_value variable_storage[VARIABLE_STORAGE_SIZE];

static lbm_uint memory[LBM_MEMORY_SIZE_8K];
static lbm_uint bitmap[LBM_MEMORY_BITMAP_SIZE_8K];

static lbm_tokenizer_string_state_t string_tok_state;
static lbm_tokenizer_char_stream_t string_tok;

void app_main(void)
{
    /* Print chip information */
    esp_chip_info_t chip_info;
    esp_chip_info(&chip_info);
    printf("This is %s chip with %d CPU core(s), WiFi%s%s, ",
           CONFIG_IDF_TARGET,
           chip_info.cores,
           (chip_info.features & CHIP_FEATURE_BT) ? "/BT" : "",
           (chip_info.features & CHIP_FEATURE_BLE) ? "/BLE" : "");

    printf("silicon revision %d, ", chip_info.revision);

    printf("%uMB %s flash\n", spi_flash_get_chip_size() / (1024 * 1024),
           (chip_info.features & CHIP_FEATURE_EMB_FLASH) ? "embedded" : "external");

    printf("Minimum free heap size: %d bytes\n", esp_get_minimum_free_heap_size());

    for (int i = 10; i >= 0; i--) {
        printf("Restarting in %d seconds...\n", i);
        vTaskDelay(1000 / portTICK_PERIOD_MS);
    }
    printf("Restarting now.\n");
    fflush(stdout);
    esp_restart();
}
