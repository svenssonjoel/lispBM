
#include <stdio.h>
#include <stdlib.h>
#include "platform.h"
#include "xil_printf.h"

#include "heap.h"
#include "symrepr.h"
#include "builtin.h"
#include "eval.h"
#include "print.h"
#include "tokpar.h"

int inputline(char *buffer, int size) {
  int n = 0;
  char c;

  for (n = 0; n < size - 1; n++) {

    c = inbyte();
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      outbyte('\b'); /* output backspace character */
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        outbyte(c);
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



int main()
{
	init_platform();

	char *str = malloc(1024);;
	size_t len = 1024;
	int res = 0;

	heap_state_t heap_state;

	res = symrepr_init();
	if (res)
		printf("Symrepr initialized.\n");
	else {
		printf("Error initializing symrepr!\n");
		return 0;
	}
	int heap_size = 8 * 1024 * 1024;
	res = heap_init(heap_size);
	if (res)
		printf("Heap initialized. Heap size: %f MiB. Free cons cells: %u\n", heap_size_bytes() / 1024.0 / 1024.0, heap_num_free());
	else {
		printf("Error initializing heap!\n");
		return 0;
	}

	res = builtin_init();
	if (res)
		printf("Built in functions initialized.\n");
	else {
		printf("Error initializing built in functions.\n");
		return 0;
	}

	res = eval_init();
	if (res)
		printf("Evaluator initialized.\n");
	else {
		printf("Error initializing evaluator.\n");
	}

	printf("Lisp REPL started!\n");

	while (1) {
		printf("# "); fflush(stdout);
		memset(str,0,len);
		inputline(str, len);
		printf("\n");

		if (strncmp(str, "info", 4) == 0) {
			printf("############################################################\n");
			printf("Used cons cells: %u \n", heap_size - heap_num_free());
			printf("ENV: "); simple_print(eval_get_env()); printf("\n");
			//symrepr_print();
			heap_perform_gc(eval_get_env());
			heap_get_state(&heap_state);
			printf("GC counter: %u\n", heap_state.gc_num);
			printf("Recovered: %u\n", heap_state.gc_recovered);
			printf("Marked: %u\n", heap_state.gc_marked);
			printf("Free cons cells: %u\n", heap_num_free());
			printf("############################################################\n");
		} else {

			VALUE t;
			t = tokpar_parse(str);

			t = eval_program(t);

			if (dec_sym(t) == symrepr_eerror()) {
				printf("%s\n", eval_get_error());
			} else {
				printf("> "); simple_print(t); printf("\n");
			}

		}
	}

	symrepr_del();
	heap_del();

	cleanup_platform();
	return 0;
}
