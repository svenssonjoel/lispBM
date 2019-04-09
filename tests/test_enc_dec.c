
#include <stdlib.h>
#include <stdio.h>

#include "heap.h"

int main(int argc, char **argv) {

  int res = 1; 
  
  int n = 0;
  res &= (dec_i28(enc_i28(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_i28(enc_i28(-1)) == -1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_i28(enc_i28(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_i28(enc_i28(134217727)) == 134217727);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_i28(enc_i28(-134217728)) == -134217728);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (dec_u28(enc_u28(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_u28(enc_u28(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_u28(enc_u28(268435455)) == 268435455);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (dec_char(enc_char(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_char(enc_char(-1)) == -1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_char(enc_char(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_char(enc_char(127)) == 127);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_char(enc_char(-128)) == -128);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (dec_sym(enc_sym(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_sym(enc_sym(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (dec_sym(enc_sym(268435455)) == 268435455);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  return res;

}
