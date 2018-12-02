
#include "symrepr.h"
#include "heap.h"
#include "builtin.h"
#include "print.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

extern int lookup_env(uint32_t sym, uint32_t env, uint32_t *res);

/* 
   Attempt to implement an evaluator in continuation passing style: 
   (In spite of C not being very suitable for the task.) 
   
   - No idea if this will work..

   - I hope this is possible without C being able to generate functions
     on the fly (and without going all the way to some jit setup). 

   - peeking a lot at: http://lisperator.net/pltut/cps-evaluator/stack-guard
     during the implementation of this.
     
   - Want to rewind the stack using setjmp longjmp.

   - Memory management will be real tricky since cannot rely on data on the stack.

 */

jmp_buf rewind_buf;

typedef struct {
  uint8_t *(*fptr)(uint8_t *, uint8_t *);
  uint32_t args_bytes;
  uint8_t *args;
} cont;
    


static uint32_t curr_exp; 
static uint32_t curr_env; 
static cont     curr_cont;


// The size of this stack should be limited in size by syntactic nesting
// of the source program, not by for example recursion. (I hope) 
typedef struct {
  uint32_t* data;
  int32_t   sp;
  uint32_t  size; 
} stack;

stack *K; 

stack* init_cont_stack(int stack_size) {

  stack *s = malloc(sizeof(stack));
 
  s->data = malloc(sizeof(uint32_t) * stack_size);
  s->sp = 0;
  s->size = stack_size;

  return s;
}

int grow_stack(stack *s) {
 
  uint32_t new_size = s->size * 2;
  uint32_t *data    = malloc(sizeof(uint32_t) * new_size);
  
  if (data == NULL) return 0; 

  memcpy(data, s->data, s->size*sizeof(uint32_t));
  free(s->data); 
  s->data = data;
  s->size = new_size;
  return 1;
  
}

int push_u32(stack *s, uint32_t val) {
  int res = 1;
  s->data[s->sp] = val;
  s->sp++; 
  if ( s->sp >= s->size) {
    res = grow_stack(s);
  }
  return res; 
}

int push_k(stack *s, uint32_t (*k)(uint32_t)) {
  int res = 1;
  s->data[s->sp] = (uint32_t)k;
  s->sp++;
  if ( s->sp >= s->size) {
    res = grow_stack(s);
  }
  return res; 
}

int pop_u32(stack *s, uint32_t *val) {
  
  s->sp--;
  *val = s->data[s->sp];
  
  return 1; 
}

int pop_k(stack *s, uint32_t (**k)(uint32_t)) {
  s->sp--;
  *k = (uint32_t (*)(uint32_t))s->data[s->sp]; 
}
  
uint32_t eval_cps_get_env(void) {
  return global_env;
}


uint32_t done(uint32_t arg) {
  return arg;
}

uint32_t apply_continuation(stack *K, uint32_t args){

  printf("apply_continuation\n");

  uint32_t (*k)(uint32_t);
  pop_k(K,&k);
  
  return  (*k)(args);
}

uint32_t set_global_env(uint32_t val){
  
  uint32_t curr = global_env;
  
  uint32_t key;
  pop_u32(K,&key);


  while(TYPE_OF(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == key) {
      set_cdr(car(curr),val);
      
      return  ENC_SYM(symrepr_true());
    }
    curr = cdr(curr);
  }
  uint32_t keyval = cons(key,val);
  if (TYPE_OF(keyval) == VAL_TYPE_SYMBOL) {
    return ENC_SYM(symrepr_nil());
  }
  global_env = cons(keyval,global_env);

  return ENC_SYM(symrepr_true());
}


uint32_t function_app(uint32_t args) {

  uint32_t fun;
  pop_u32(K, &fun);

  args = reverse(args); 
  
  printf("fun:"); simple_print(fun); printf("\n");
  printf("args:"); simple_print(args); printf("\n"); 

  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(fun));

  if (f == NULL) {
    printf("Built in function does not exist"); 
    return ENC_SYM(symrepr_eerror());
  }

  
  return apply_continuation(K,f(args)); 
}

uint32_t eval_rest(uint32_t head) {

  uint32_t rest;
  uint32_t acc;
  
  pop_u32(K, &rest);
  pop_u32(K, &acc); 
  
  if (TYPE_OF(rest) == VAL_TYPE_SYMBOL &&
      DEC_SYM(rest) == symrepr_nil()) {

    uint32_t args = cons(head, acc); 
    return apply_continuation(K, args);
  }

  push_u32(K, cons(head, acc));
  push_u32(K, cdr(rest));
  push_k(K, eval_rest);
  
  curr_exp = car(rest);
  //env unchanged
  longjmp(rewind_buf, 1); 
 
}

// Closure or built-in function 
uint32_t function_cont(uint32_t fun) {
 
  uint32_t fun_args;
  pop_u32(K,&fun_args); 

  printf("function:"); simple_print(fun); printf("\n");
  printf("args:"); simple_print(fun_args); printf("\n");  

  uint32_t head = car(fun_args); 

  push_u32(K,fun); 
  push_k(K,function_app);
  push_u32(K,ENC_SYM(symrepr_nil()));
  push_u32(K,cdr(fun_args)); 
  push_k(K, eval_rest); 
 
  curr_exp = head;
  //env unchanged
  longjmp(rewind_buf, 1); 
} 



uint32_t eval_cps(uint32_t *lisp_in, uint32_t *env_in) {
  
  uint32_t env = *env_in;
  uint32_t lisp = *lisp_in;
  
  uint32_t tmp = ENC_SYM(symrepr_eerror()); 
  int ret = 0;
  uint32_t head;
  
  printf("eval_cps\n"); 
  switch (TYPE_OF(lisp)) {
    
  case VAL_TYPE_SYMBOL:
    ret = lookup_env(lisp, env, &tmp);
    if (!ret) {
      ret = lookup_env(lisp, global_env, &tmp);
    }
    if (ret) {
      return apply_continuation(K, tmp);
    }
    return ENC_SYM(symrepr_eerror());
    
  case VAL_TYPE_I28: 
  case VAL_TYPE_CHAR:
  case VAL_TYPE_U28: {
    return apply_continuation(K, lisp);
  }

  case PTR_TYPE_CONS:
    head = car(lisp);
    
    if (TYPE_OF(head) == VAL_TYPE_SYMBOL) {
      
      // Special form: QUOTE
      if (DEC_SYM(head) == symrepr_quote()) {
	uint32_t val =  car(cdr(lisp));
	return apply_continuation(K,val);
      }
      
      // Special form: DEFINE 
      if (DEC_SYM(head) == symrepr_define()) {
	uint32_t key = car(cdr(lisp));
	uint32_t val_exp = car(cdr(cdr(lisp)));
	
	if (TYPE_OF(key) != VAL_TYPE_SYMBOL ||
	    DEC_SYM(key) == symrepr_nil()) {
	  uint32_t val = ENC_SYM(symrepr_eerror()); 
	  return val;
	}

	push_u32(K, key);
	push_k(K,set_global_env); 
		
	*lisp_in = val_exp;
	*env_in = env;
	longjmp(rewind_buf,1); 
      }

      // Special form: LAMBDA 
      if (DEC_SYM(head) == symrepr_lambda()) {
	printf("NOT IMPLEMENTED\n");
	return ENC_SYM(symrepr_eerror()); 
      }

      // Special form: IF 
      if (DEC_SYM(head) == symrepr_if()) {
	printf("NOT IMPLEMENTED\n");
	return ENC_SYM(symrepr_eerror()); 
      }

      // Special form: LET
      if (DEC_SYM(head) == symrepr_let()) {
	printf("NOT IMPLEMENTED\n");
	return ENC_SYM(symrepr_eerror()); 
      }
    } // If head is symbol

    // Possibly an application form:


    push_u32(K, cdr(lisp)); // list of arguments that needs to be evaluated. 
    push_k(K, function_cont); 
    
    *lisp_in = head;
    *env_in  = *env_in;
    longjmp(rewind_buf, 1); 
    
  }
}
      



int run_eval(uint32_t lisp, uint32_t env) {


  push_k(K,done);
  
  curr_exp = lisp;
  curr_env = env;

  uint32_t r;
  
  printf("run_eval\n"); 
  
  if (setjmp(rewind_buf)) {
    printf("Rewind!\n");
 
    // GC also needs info about things alive in the "continuation"
    heap_perform_gc_aux(global_env, curr_env, K->data, K->sp);
    
    r = eval_cps(&curr_exp, &curr_env);
   	
  } else {

    // kickstarts evaluation with the done_cont;
    r = eval_cps(&curr_exp, &curr_env);
  
  }

  // DONE!
  uint32_t r_val = r;  
  simple_print(r_val); printf("\n");

  return 1; 
}

int eval_cps_init() {

  K = init_cont_stack(100);
  
  global_env = ENC_SYM(symrepr_nil());

  global_env = built_in_gen_env();
  
  uint32_t nil_entry = cons(ENC_SYM(symrepr_nil()), ENC_SYM(symrepr_nil()));
  global_env = cons(nil_entry, global_env);

  
  return 1;
}
