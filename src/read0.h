
#ifndef READ_H_
#define READ_H_

#include <stdint.h> 

#define INTEGER 1 
#define FLOAT   2
#define SYMBOL  3 
#define CONS    4 

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



#endif 
