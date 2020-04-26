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
   

  return 0; 
}

