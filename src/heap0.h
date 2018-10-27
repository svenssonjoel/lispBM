
#ifndef HEAP_H_
#define HEAP_H_

#include <stdlib.h>
#include <stdint.h> 

#define INTEGER          1 
#define FLOAT            2
#define SYMBOL           3 
#define POINTER          4
#define OFF_HEAP_POINTER 5 
#define NIL              6
#define NOTHING          7 /* a Nothing is not a Nil (does Nothing need to exist?) */

/* bitpositions in the TYPE field */
#define CAR_TYPE_POS 0
#define CDR_TYPE_POS 8
#define AUX_BITS_POS 9
#define MARK_POS     31    /* last bit reserved for garbage collector */  

#define GET_CAR_TYPE(X)    ((X) & 0xFF)
#define GET_CDR_TYPE(X)    (((X) >> CDR_TYPE_POS) & 0xFF)

#define SET_CAR_TYPE(X,T)  (((X) & ~0xFF) | ((T) & 0xFF))
#define SET_CDR_TYPE(X,T)  (((X) & ~(0xFF << CDR_TYPE_POS)) | (((T) & 0xFF) << CDR_TYPE_POS))

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
