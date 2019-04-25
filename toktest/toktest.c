
#include <stdio.h>
#include "tokpar.h"
#include "heap.h"
#include "symrepr.h"
#include "print.h"


int main(int argc, char **argv) {

  tokpar_parse_string("()");

  tokpar_parse_string("a");
  tokpar_parse_string("ap");
  tokpar_parse_string("apa");
  tokpar_parse_string("(apa)");
  tokpar_parse_string("((apa))");
  tokpar_parse_string("(((apa)))");
  tokpar_parse_string("apa\"bepa\"");

  tokpar_parse_string("+");
  tokpar_parse_string("(+)");
  tokpar_parse_string("((+))");

  tokpar_parse_string("123");
  tokpar_parse_string("(123)");
  tokpar_parse_string("123I");
  tokpar_parse_string("(123I)");

  tokpar_parse_string("   (+ 1 2)");

  tokpar_parse_string("  (+ 1024 202) (+ apa (- 1))\n ()");

  tokpar_parse_string("  '() 'apa '1 \n");

  tokpar_parse_string("1\"fhirnfirninfrinfrifi\"");

  tokpar_parse_string("\\#c");
  tokpar_parse_string(" \\#c");
  tokpar_parse_string("() \\#c");
  tokpar_parse_string("\\#c123");
  tokpar_parse_string("(= (array-read a 3u) \\#l)");

  tokpar_parse_string("3.14apa");

  tokpar_parse_string("0.1e19"); // will break things

  tokpar_parse_string("1 ;fhiefheihfeifehfi\n;ijfiejfiejfiejeif\n  ;    fijfiejfiefiejifj\n 2");

  tokpar_parse_string("\"hello\"\"world\"");

  tokpar_parse_string("(= (+ 4I 7I) 11I)");

  tokpar_parse_string("0xf");
  tokpar_parse_string("0x1");
  tokpar_parse_string("0Xf");
  tokpar_parse_string("0X1");
  tokpar_parse_string("0X11");

  tokpar_parse_string(":");
  tokpar_parse_string("(:)");
  tokpar_parse_string("( : )");

  tokpar_parse_string(",");
  tokpar_parse_string("(,)");
  tokpar_parse_string("( , )");

  int res = 0;

  res = symrepr_init();
  if (res)
    printf("Symrepr initialized.\n");
  else {
    printf("Error initializing symrepr!\n");
    return 0;
  }

  int heap_size = 8 * 1024;
  res = heap_init(heap_size);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", heap_size_bytes() / 1024.0 / 1024.0, heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  VALUE v;

  v = tokpar_parse("1");

  simple_print(v); printf("\n");

  v = tokpar_parse("(+ 1 2 3 4 5)");

  simple_print(v); printf("\n");

  v = tokpar_parse("'(+ 1 2 3 4 5)");

  simple_print(v); printf("\n");

  v = tokpar_parse("\"hello\"");

  simple_print(v); printf("\n");

  v = tokpar_parse("\"hello\"\"world\"");

  simple_print(v); printf("\n");

  v = tokpar_parse("\"hello\";apa!\n\"world\"");

  simple_print(v); printf("\n");

  v = tokpar_parse("(= (+ 5u 60u) 65u)");

  simple_print(v); printf("\n");

  v = tokpar_parse("  (= (+ 5u 60u) 65u)");

  simple_print(v); printf("\n");

  v = tokpar_parse("(= (array-read a 3u) \\#l)");

  simple_print(v); printf("\n");

  v = tokpar_parse("(= (+ 4I 7I) 11I)");

  simple_print(v); printf("\n");

  v = tokpar_parse("  (= (+ 1 0xf) 16u)");

  simple_print(v); printf("\n");

  v = tokpar_parse(":");

  simple_print(v); printf("\n");

  v = tokpar_parse("(:)");

  simple_print(v); printf("\n");

  v = tokpar_parse("( : )");

  simple_print(v); printf("\n");

  v = tokpar_parse(",");

  simple_print(v); printf("\n");

  v = tokpar_parse("(,)");

  simple_print(v); printf("\n");

  v = tokpar_parse("( , )");

  simple_print(v); printf("\n");

  return 0;
}
