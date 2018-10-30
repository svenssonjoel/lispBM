
#ifndef HEAP_H_
#define HEAP_H_

#include <stdlib.h>
#include <stdint.h> 

/* 
Rethinking heap structure. 

0000 0000  Size    Free bits
003F FFFF  4MB      10 
007F FFFF  8MB       9
00FF FFFF  16MB      8
01FF FFFF  32MB      7 
03FF FFFF  64MB      6         * Kind of heap size I am looking for 
07FF FFFF  128MB     5
0FFF FFFF  256MB     4 
1FFF FFFF  512MB     3    

it is also the case that not all addresses will be used if all "cells" are 
of the same size, 4 bytes... 

value 0: 0000 0000 
value 1: 0000 0004
value 3: 0000 0008 
value 4: 0000 000C

Means bit 0 and bit one will always be empty in a valid address.

Cons cells also need to be have room for 2 pointers. So each allocated cell from
memory should be 8bytes. 
 
Things that needs to be represented within these bits:
 
 - GC MARK one per cell  
 - TYPE: type of CAR and type of cons

Types I would want: 
 - Full 32bit integer. Does not leave room for identification of type  
 - Float values.  Same problem
 

Free bits in pointers 64MB heap: 
31 30 29 28 27 26                               1 0
0  0  0  0  0  0  XX XXXX XXXX XXXX XXXX XXXX XX0 0 

 64 
-48
=16




 */ 


#define INTEGER          1 
#define FLOAT            2
#define SYMBOL           3 
#define POINTER          4
#define OFF_HEAP_POINTER 5 
#define NIL              6
#define NOTHING          7 /* a Nothing is not a Nil (does Nothing need to exist?) */


/* bitpositions in the TYPE field */
#define CAR_TYPE_POS  0
#define CDR_TYPE_POS  8
#define CELL_TYPE_POS 16
#define AUX_BITS_POS  17
#define MARK_POS      31  /* last bit reserved for garbage collector */  


// I guess, equivalent would be if CDR is NOTHING... 
// Used to differentiate between a cons cell and a primitive value cell.
#define GET_CONS_TYPE(X)   (((X) >> CELL_TYPE_POS) & 0x1)
#define SET_CONS_TYPE(X,T) (((X) & ~(1 << CELL_TYPE_POS)) | (((T) & 0x1) << CELL_TYPE_POS))

// Get/Set CAR and CDR type.
#define GET_CAR_TYPE(X)    ((X) & 0xFF)
#define GET_CDR_TYPE(X)    (((X) >> CDR_TYPE_POS) & 0xFF)

#define SET_CAR_TYPE(X,T)  (((X) & ~0xFF) | ((T) & 0xFF))
#define SET_CDR_TYPE(X,T)  (((X) & ~(0xFF << CDR_TYPE_POS)) | (((T) & 0xFF) << CDR_TYPE_POS))

// toggle Auxiliary bits in type field. 
#define GET_AUX_BIT(X,N)   ((X) >> (AUX_BITS_POS + (N)) & 1) 
#define SET_AUX_BIT(X,B,N) (((X) & ~(1 << AUX_BITS_POS + (N))) | ((B) << (AUX_BITS_POS + (N))))

struct s_cons;
union  s_car;

/* The (main) target platform in mind has 32bit pointers. 
   So in that particular case, all members of this union 
   are of the same size. 
   
   TODO: Think of using some flags here to "up" the int32_t and float
         to int64_t and double, in case of building for regular X86-64.
*/ 
typedef union s_car {
  int32_t i;
  float   f;
  int32_t s;
  struct s_cons *cons; 
} car_t, cdr_t; 

typedef struct s_cons {
  uint32_t type;
  
  car_t car; 
  cdr_t cdr;
} cons_t;

extern int heap_init(size_t num_cells);
extern void heap_del(void);
extern size_t heap_num_free(void);
extern cons_t* heap_allocate_cell(void); 
extern size_t heap_size_bytes(void); 
#endif
