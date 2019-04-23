

#include "tokpar.h"


int main(int argc, char **argv) {



  tokpar_parse_string("()");

  tokpar_parse_string("(apa)");

  tokpar_parse_string("+");
  tokpar_parse_string("(+)");
  tokpar_parse_string("((+))");
  
  tokpar_parse_string("(123)");

  tokpar_parse_string("   (+ 1 2)");

  return 0;
}
