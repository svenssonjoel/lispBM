
#include <stdlib.h>
#include <stdio.h>

#include "heap.h"

int main(int argc, char **argv) {

  int res = 1; 
  
  int n = 0;
  res &= (DEC_I28(ENC_I28(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_I28(ENC_I28(-1)) == -1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_I28(ENC_I28(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_I28(ENC_I28(134217727)) == 134217727);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_I28(ENC_I28(-134217728)) == -134217728);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (DEC_U28(ENC_U28(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_U28(ENC_U28(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_U28(ENC_U28(268435455)) == 268435455);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (DEC_CHAR(ENC_CHAR(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_CHAR(ENC_CHAR(-1)) == -1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_CHAR(ENC_CHAR(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_CHAR(ENC_CHAR(127)) == 127);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_CHAR(ENC_CHAR(-128)) == -128);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (DEC_SYM(ENC_SYM(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_SYM(ENC_SYM(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (DEC_SYM(ENC_SYM(268435455)) == 268435455);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  return res;

}
