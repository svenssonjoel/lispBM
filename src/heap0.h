
#ifndef HEAP0_H_
#define HEAP0_H_

#include <stdlib.h>
#include <stdint.h> 

#define INTEGER 1 
#define FLOAT   2
#define SYMBOL  3 
#define POINTER 4
#define NIL     5 

#define CAR_TYPE(X) ((X) & 0xFF)
#define CDR_TYPE(X) (((X) >> 8) & 0xFF)



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
  int32_t type;
  
  car_t car; 
  cdr_t cdr;
} cons_t;

extern int heap_init(size_t num_cells);
extern void heap_del(void);

extern size_t heap_num_free(void);

extern cons_t* heap_allocate_cell(); 

#endif
