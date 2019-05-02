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

#ifndef HEAP_H_
#define HEAP_H_

#include <string.h>
#include "typedefs.h"

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

13 pointer"types" -> needs 4 bits
for 64MB heap there are 6 free bits. So with this scheme going to 128MB or 256MB heap 
is also possible

 a pointer to some off heap vector/string could be represented by

 [ptr | cdr]
   |
 [full pointer | Aux + GC_MARK]
   |
 [VECTOR]

Aux bits could be used for storing vector size. Up to 30bits should be available there
>> This is problematic. Now the information that something is a vector is split up 
>> between 2 cons cells. This means GC needs both of these intact to be able to make 
>> proper decision.
>> Will try to resolve this by adding some special symbols. But these must be symbols 
>> that cannot occur normally in programs. Then an array could be:

 [Full pointer | ARRAY_SYM + GC_MARK]
     |
 [VECTOR]

>> Boxed values same treatment as above.
>> TODO: Could this be simpler?

[ VALUE | TYPE_SYM + GC_MARK]


0000 00XX XXXX XXXX XXXX XXXX XXXX X000   : 0x03FF FFF8
1111 AA00 0000 0000 0000 0000 0000 0000   : 0xFC00 0000 (AA bits left unused for now, future heap growth?)
 */

#define CONS_CELL_SIZE       8
#define ADDRESS_SHIFT        3
#define VAL_SHIFT            4

#define PTR_MASK             0x00000001u
#define PTR                  0x00000001u
#define PTR_VAL_MASK         0x03FFFFF8u
#define PTR_TYPE_MASK        0xFC000000u

#define PTR_TYPE_CONS        0x10000000u
#define PTR_TYPE_BOXED_I     0x20000000u
#define PTR_TYPE_BOXED_U     0x30000000u
#define PTR_TYPE_BOXED_F     0x40000000u
/*...*/
#define PTR_TYPE_BYTECODE    0xC0000000u 
#define PTR_TYPE_ARRAY       0xD0000000u
#define PTR_TYPE_REF         0xE0000000u //untyped reference to memory location
#define PTR_TYPE_STREAM      0xF0000000u

#define GC_MASK              0x00000002u
#define GC_MARKED            0x00000002u

#define VAL_MASK             0xFFFFFFF0u
#define VAL_TYPE_MASK        0x0000000Cu

#define VAL_TYPE_SYMBOL      0x00000000u
#define VAL_TYPE_CHAR        0x00000004u
#define VAL_TYPE_I           0x00000008u
#define VAL_TYPE_U           0x0000000Cu

typedef struct {
  VALUE car;
  VALUE cdr;
} cons_t;

typedef struct {
  cons_t  *heap;            
  bool  malloced;           // allocated by heap_init
  VALUE freelist;           // list of free cons cells.

  unsigned int heap_size;          // In number of cells.
  unsigned int heap_bytes;         // In bytes.

  unsigned int num_alloc;          // Number of cells allocated.
  unsigned int num_alloc_arrays;   // Number of arrays allocated.

  unsigned int gc_num;             // Number of times gc has been performed.
  unsigned int gc_marked;          // Number of cells marked by mark phase.
  unsigned int gc_recovered;       // Number of cells recovered by sweep phase.
  unsigned int gc_recovered_arrays;// Number of arrays recovered by sweep.
} heap_state_t;


typedef struct {
  TYPE elt_type;            // Type of elements: VAL_TYPE_FLOAT, U, I or CHAR
  unsigned int size;        // Number of elements
  union {
    float    *f;
    UINT     *u; 
    INT      *i;
    char     *c;
  } data;                   // Array data storage
} array_t;

extern int heap_init_addr(cons_t *addr, unsigned int num_cells);
extern int heap_init(unsigned int num_cells);
extern void heap_del(void);
extern unsigned int heap_num_free(void);
extern unsigned int heap_num_allocated(void);
extern unsigned int heap_size(void);
extern VALUE heap_allocate_cell(TYPE type);
extern unsigned int heap_size_bytes(void);

extern VALUE cons(VALUE car, VALUE cdr);
extern VALUE car(VALUE cons);
extern VALUE cdr(VALUE cons);
extern void set_car(VALUE c, VALUE v);
extern void set_cdr(VALUE c, VALUE v);
extern unsigned int length(VALUE c);
extern VALUE reverse(VALUE list);
extern VALUE copy(VALUE list);

// State and statistics
extern void heap_get_state(heap_state_t *);

// Garbage collection
extern int heap_perform_gc(VALUE env);
extern int heap_perform_gc_extra(VALUE env, VALUE env2, VALUE exp, VALUE exp2, VALUE list);
extern int heap_perform_gc_aux(VALUE env, VALUE env2, VALUE exp, VALUE exp2, UINT *aux_data, unsigned int aux_size);


// Array functionality
extern int heap_allocate_array(VALUE *res, unsigned int size, TYPE type);

static inline TYPE val_type(VALUE x) {
  return (x & VAL_TYPE_MASK);
}

static inline TYPE ptr_type(VALUE p) {
  return (p & PTR_TYPE_MASK);
}

static inline TYPE type_of(VALUE x) {
  return (x & PTR_MASK) ? (x & PTR_TYPE_MASK) : (x & VAL_TYPE_MASK);
}

static inline bool is_ptr(VALUE x) {
  return (x & PTR_MASK);
}

static inline VALUE enc_cons_ptr(UINT x) {
  return ((x << ADDRESS_SHIFT) | PTR_TYPE_CONS | PTR);
}

static inline uint32_t dec_ptr(VALUE p) {
  return ((PTR_VAL_MASK & p) >> ADDRESS_SHIFT);
}

static inline VALUE set_ptr_type(VALUE p, TYPE t) {
  return (PTR_VAL_MASK & p) | t | PTR;
}

static inline VALUE enc_i(INT x) {
  return ((UINT)x << VAL_SHIFT) | VAL_TYPE_I;
}

static inline VALUE enc_u(UINT x) {
  return (x << VAL_SHIFT) | VAL_TYPE_U;
}

static inline VALUE enc_char(char x) {
  return ((UINT)x << VAL_SHIFT) | VAL_TYPE_CHAR;
}

static inline VALUE enc_sym(uint32_t s) {
  return (s << VAL_SHIFT) | VAL_TYPE_SYMBOL;
}

static inline INT dec_i(VALUE x) {
  return (INT)x >> VAL_SHIFT;
}

static inline UINT dec_u(VALUE x) {
  return x >> VAL_SHIFT;
}

static inline char dec_char(VALUE x) {
  return (char)(x >> VAL_SHIFT);
}

static inline UINT dec_sym(VALUE x) {
  return x >> VAL_SHIFT;
}

static inline FLOAT dec_f(VALUE x) { // Use only when knowing that x is a VAL_TYPE_F
  FLOAT f_tmp;
  UINT tmp = car(x);
  memcpy(&f_tmp, &tmp, sizeof(FLOAT));
  return f_tmp;
}

static inline UINT dec_U(VALUE x) {
  return car(x);
}

static inline INT dec_I(VALUE x) {
  return (INT)car(x);
}

static inline VALUE val_set_gc_mark(VALUE x) {
  return x | GC_MARKED;
}

static inline VALUE val_clr_gc_mark(VALUE x) {
  return x & ~GC_MASK;
}

static inline bool val_get_gc_mark(VALUE x) {
  return x & GC_MASK;
}


#endif
