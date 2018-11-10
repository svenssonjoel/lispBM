
#ifndef HEAP_H_
#define HEAP_H_

#include <stdlib.h>
#include <stdint.h> 

/* 
Planning for a more space efficient heap representation. 
TODO: Need to find a good reference to read up on this. 
      - List based heap 
      - Easy to implement and somewhat efficient 

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
of the same size, 8 bytes... 

value 0: 0000 0000 
value 1: 0000 0008
value 3: 0000 0010 
value 4: 0000 0018

Means bits 0,1,2 will always be empty in a valid address.

Cons cells also need to be have room for 2 pointers. So each allocated cell from
memory should be 8bytes. 
 
Things that needs to be represented within these bits:
 
 - GC MARK one per cell  
 - TYPE: type of CAR and type of cons

Types I would want: 
 - Full 32bit integer. Does not leave room for identification of type  
 - Float values.  Same problem
 

Free bits in pointers 64MB heap: 
31 30 29 28 27 26                               2 1 0
0  0  0  0  0  0  XX XXXX XXXX XXXX XXXX XXXX X 0 0 0 


Information needed for each cell: 
 Meaning  |   bits total | bits per car | bits per cdr 
 GC mark  |    2         |   1          |   1          - only one of them will be used (the other is wasted)   
 Type     |    2x        |   x          |   x 
 Ptr/!ptr |    2         |   1          |   1 


Types (unboxed): 
 - Symbols 
 - 28bit integer   ( will need signed shift right functionality )
 - 28bit unsigned integer 
 - Character

If four types is all that should be possible (unboxed). then 2 bits are needed to differentiate.  
2 + 1 + 1 = 4 => 28bits for data. 

bit 0: ptr/!ptr
bit 1: gc
bit 2-3: type (if not ptr)  
bit 3 - 24 ptr (if ptr)
bit 4 - 31 value (if value) 

Maybe the GC bit in the car field can be used to indicate that this is a 
"car only cons cell". 


An unboxed value can occupy a car or cdr field in a cons cell. 

types (boxed) extra information in pointer to cell can contain information
 - 32 bit integer 
 - 32 bit unsigned integer 
 - 32 bit float 

boxed representation: 
  [ptr| cdr] 
    |
  [Value | Aux + GC_MARK] 

Kinds of pointers:
 - Pointer to cons cell. 
 - Pointer to unboxed value  (fixnums not in a list, I hope this is so rare that it can be removed )  
   - integer
   - unsigned integer 
   - symbol
   - float
 - Pointer to boxed value.  
   - 32 bit integer
   - 32 bit unsigned integer 
   - 32 bit float 
 - (Maybe something else ? Vectors/strings allocated in memory not occupied by heap?)
   - vector of int 
   - vector of uint 
   - vector of float
   - vector of double 
   - String

13 pointer"types" -> needs 5 bits
for 64MB heap there are 6 free bits. So with this scheme going to 128MB heap 
is also possible 

 a pointer to some off heap vector/string could be represented by 
 
 [ptr | cdr] 
   | 
 [full pointer | Aux + GC_MARK] 
   | 
 [VECTOR] 

 */

#define CONS_CELL_SIZE   8
#define ADDRESS_SHIFT    3

#define PTR_MASK         0x1
#define IS_PTR           0x1
#define PTR_VAL_MASK     0x03FFFFF8

#define GC_MASK          0x2
#define GC_MARKED        0x2 

#define VAL_MASK         0xFFFFFFF0
#define VAL_TYPE_MASK    0xC
#define VAL_SHIFT        4 

#define VAL_TYPE_SYMBOL  0x0
#define VAL_TYPE_I28     0x4
#define VAL_TYPE_U28     0x8
#define VAL_TYPE_CHAR    0xC 

#define ENC_I28(X)  ((((uint32_t)(X)) << VAL_SHIFT) | VAL_TYPE_I28)
#define ENC_U28(X)  ((((uint32_t)(X)) << VAL_SHIFT) | VAL_TYPE_U28)
#define ENC_CHAR(X) ((((uint32_t)(X)) << VAL_SHIFT) | VAL_TYPE_CHAR)
#define ENC_SYM(X)  ((((uint32_t)(X)) << VAL_SHIFT) | VAL_TYPE_SYMBOL)

#define DEC_I28(X)  ((int32_t)((int32_t)(X) >> VAL_SHIFT))
#define DEC_U28(X)  ((uint32_t)((uint32_t)(X) >> VAL_SHIFT))
#define DEC_CHAR(X) ((char)((int32_t)(X) >> VAL_SHIFT))
#define DEC_SYM(X)  ((uint32_t)((uint32_t)(X) >> VAL_SHIFT))

typedef struct s_cons {
  uint32_t car;
  uint32_t cdr; 
} cons_t; 

extern int heap_init(size_t num_cells);
extern void heap_del(void);
extern uint32_t heap_num_free(void);
extern uint32_t heap_allocate_cell(void); 
extern uint32_t heap_size_bytes(void);

// accessor helpers
extern cons_t* ref_cell(uint32_t addr);
extern uint32_t read_car(cons_t*);
extern uint32_t read_cdr(cons_t*);
extern void set_car(cons_t*, uint32_t);
extern void set_cdr(cons_t*, uint32_t); 
#endif
