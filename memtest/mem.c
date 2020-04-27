/*
    Copyright 2018 Joel Svensson	svenssonjoel@yahoo.se

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

 #define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>
#include <termios.h>
#include <ctype.h>

#include "heap.h"
#include "symrepr.h"
#include "extensions.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "typedefs.h"
#include "memory.h"

int main(int argc, char **argv) {


  unsigned char *data = malloc(MEMORY_SIZE_4K);
  unsigned char *bits = malloc(MEMORY_BITMAP_SIZE_4K);
  
  printf("Number of bytes data: %d\n", MEMORY_SIZE_4K);
  printf("Number of bytes bitmap: %d\n", MEMORY_BITMAP_SIZE_4K);

  printf("Data is %s\n", (unsigned int)data % 4 == 0 ? "aligned" : "NOT aligned");
  printf("Bitmap is %s\n", (unsigned int)bits % 4 == 0 ? "aligned" : "NOT aligned");
  
  printf("Data addr %x\n", (unsigned int) data);
  printf("Bitmap addr %x\n", (unsigned int) bits);
  
  int res = memory_init(data, MEMORY_SIZE_4K,
			bits, MEMORY_BITMAP_SIZE_4K);

  if (!res) {
    printf("Error configuring memory\n"); 
  } else {
    printf("Memory configured OK!\n");
  }

  uint32_t *a = memory_allocate(5);
  uint32_t *b = memory_allocate(1);
  uint32_t *c = memory_allocate(1);
  uint32_t *d = memory_allocate(1);
  uint32_t *e = memory_allocate(5);

  printf("%x\n", (unsigned int)a);
  printf("%x\n", (unsigned int)b);
  printf("%x\n", (unsigned int)c);
  printf("%x\n", (unsigned int)d);
  printf("%x\n", (unsigned int)e);

  
  a[0] = 1;
  a[1] = 2;
  a[2] = 3;
  a[3] = 4; 

  b[0] = 1000;
  c[0] = 109999;
  d[0] = 0xFFFFFFFF;
  
  e[0] = 10;
  e[1] = 20;
  e[2] = 30;
  e[3] = 40;
    
  printf("b[0] = %u\n", b[0]);
  printf("c[0] = %u\n", c[0]);
  printf("d[0] = %x\n", d[0]);

  printf("a[0] = %u\n", a[0]);
  printf("a[1] = %u\n", a[1]);
  printf("a[2] = %u\n", a[2]);
  printf("a[3] = %u\n", a[3]);

  printf("e[0] = %u\n", e[0]);
  printf("e[1] = %u\n", e[1]);
  printf("e[2] = %u\n", e[2]);
  printf("e[3] = %u\n", e[3]);

  if (!memory_free(b)) printf("error freeing b\n");
  if (!memory_free(c)) printf("error freeing c\n");
  if (!memory_free(d)) printf("error freeing d\n");

  uint32_t *apa = memory_allocate(3);
  
  b = memory_allocate(1);
  c = memory_allocate(1);
  d = memory_allocate(1);
  
  printf("a: %x\n", (unsigned int)a);
  printf("b: %x\n", (unsigned int)b);
  printf("c: %x\n", (unsigned int)c);
  printf("d: %x\n", (unsigned int)d);
  printf("e: %x\n", (unsigned int)e);
  printf("apa: %x\n", (unsigned int)apa);

  
  
  return 0; 
}

