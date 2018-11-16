
#ifndef EVAL_H_
#define EVAL_H_

#include "heap.h"


int eval_init();
void eval_set_env(uint32_t env);
uint32_t eval_get_env(void);
uint32_t eval_program(uint32_t lisp);


#endif

