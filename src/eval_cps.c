
#include "symrepr.h"
#include "heap.h"
#include "builtin.h"
#include "print.h"
#include "env.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

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

static uint32_t curr_exp; 
static uint32_t curr_env; 

// The size of this stack should be limited in size by syntactic nesting
// of the source program, not by for example recursion. (I hope) 
typedef struct {
  uint32_t* data;
  int32_t   sp;
  uint32_t  size; 
} stack;

stack *K; // Stack describes the current continuation.

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
  return 1; 
}
  
uint32_t eval_cps_get_env(void) {
  return global_env;
}


uint32_t done(uint32_t arg) {
  return arg;
}

uint32_t apply_continuation(stack *K, uint32_t args){

  //printf("apply_continuation\n");

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
 
  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(fun));

  if (f == NULL) {
    //printf("Built in function does not exist"); 
    return ENC_SYM(symrepr_eerror());
  }
  
  return apply_continuation(K,f(args)); 
}


uint32_t restore_env(uint32_t pass_through) {

  //printf("**************** restoring env ****************\n"); 
  uint32_t env;
  pop_u32(K, &env);

  curr_env = env;

  return apply_continuation(K,pass_through); 
}

uint32_t closure_app(uint32_t args) {

  uint32_t closure;
  pop_u32(K, &closure);

  args = reverse(args);

  uint32_t params  = car(cdr(closure));
  uint32_t exp     = car(cdr(cdr(closure)));
  uint32_t clo_env = car(cdr(cdr(cdr(closure))));

  uint32_t local_env;
  env_build_params_args(params, args, clo_env, &local_env); 
  //simple_print(local_env); printf("\n"); 
  //push_u32(K, curr_env);
  //push_k(K, restore_env);
  
  curr_exp = exp;
  curr_env = local_env; 
  longjmp(rewind_buf, 1); 
}

uint32_t eval_rest(uint32_t head) {

  uint32_t rest;
  uint32_t acc;
  uint32_t env;

  pop_u32(K, &rest);
  pop_u32(K, &acc); 
  pop_u32(K, &env);
  
  if (TYPE_OF(rest) == VAL_TYPE_SYMBOL &&
      DEC_SYM(rest) == symrepr_nil()) {

    uint32_t args = cons(head, acc); 
    return apply_continuation(K, args);
  }

  push_u32(K, env); 
  push_u32(K, cons(head, acc));
  push_u32(K, cdr(rest));
  push_k(K, eval_rest);
  
  curr_exp = car(rest);
  curr_env = env;
  longjmp(rewind_buf, 1); 
}

// Closure or built-in function 
uint32_t function_cont(uint32_t fun) {
 
  uint32_t fun_args;
  uint32_t env;
  pop_u32(K,&fun_args);
  pop_u32(K,&env); 
  
  uint32_t head = car(fun_args); 

  push_u32(K,fun);
  if ( TYPE_OF(fun) == PTR_TYPE_CONS &&
       DEC_SYM(car(fun)) == symrepr_closure()) {
    push_k(K,closure_app); 
  } else {
    push_k(K,function_app);
  }
  push_u32(K,env); 
  push_u32(K,ENC_SYM(symrepr_nil()));
  push_u32(K,cdr(fun_args)); 
  push_k(K, eval_rest); 
 
  curr_exp = head;
  curr_env = env;
  longjmp(rewind_buf, 1); 
} 


uint32_t bind_to_key_rest(uint32_t val) {
  uint32_t key;
  uint32_t env;
  uint32_t rest;

  pop_u32(K, &key);
  pop_u32(K, &env);
  pop_u32(K, &rest); 
  
  env_modify_binding(env, key, val); 

  if ( TYPE_OF(rest) == PTR_TYPE_CONS ){
    uint32_t keyn = car(car(rest));
    uint32_t valn_exp = car(cdr(car(rest)));
			    
			    
    push_u32(K,cdr(rest));
    push_u32(K,env);
    push_u32(K,keyn);  
    push_k(K, bind_to_key_rest); 

    curr_exp = valn_exp;
    curr_env = env;
    longjmp(rewind_buf, 1);
  }

  // Otherwise evaluate the expression in the populated env
  uint32_t exp;
  pop_u32(K, &exp);
  curr_exp = exp;
  curr_env = env;
  longjmp(rewind_buf, 1); 
  
}

uint32_t process_let(uint32_t binds, uint32_t orig_env, uint32_t exp) {
  uint32_t curr = binds;
  uint32_t new_env = orig_env;

  if (TYPE_OF(binds) != PTR_TYPE_CONS) return ENC_SYM(symrepr_eerror()); 
  
  while (TYPE_OF(curr) == PTR_TYPE_CONS) { 
    uint32_t key = car(car(curr));
    uint32_t val = ENC_SYM(symrepr_nil()); // a temporary
    uint32_t binding = cons(key,val);
    new_env = cons(binding, new_env); 
    curr = cdr(curr); 
  }

  uint32_t key0 = car(car(binds)); 
  uint32_t val0_exp = car(cdr(car(binds)));

  push_u32(K,exp); // exp to evaluate in new environment
  push_u32(K,cdr(binds));
  push_u32(K,new_env);
  push_u32(K,key0);
  push_k(K, bind_to_key_rest);

  curr_exp = val0_exp;
  curr_env = new_env;  // env annotated with temporaries
  longjmp(rewind_buf, 1); 
}


uint32_t if_cont(uint32_t cond) {

  uint32_t then_branch;
  uint32_t else_branch;

  pop_u32(K, &then_branch);
  pop_u32(K, &else_branch);
  
  if (TYPE_OF(cond) == VAL_TYPE_SYMBOL &&
      DEC_SYM(cond) == symrepr_true()) {
    curr_exp = then_branch;
    //curr_env = curr_env;
    longjmp(rewind_buf,1); 
  } else {
    curr_exp = else_branch;
    //curr_env = curr_env;
    longjmp(rewind_buf,1); 
  }
}
		  

uint32_t eval_cps(uint32_t *lisp_in, uint32_t *env_in) {
  
  uint32_t env = *env_in;
  uint32_t lisp = *lisp_in;
  
  uint32_t tmp = ENC_SYM(symrepr_eerror()); 
  int ret = 0;
  uint32_t head;
  
  //printf("eval_cps\n"); 
  switch (TYPE_OF(lisp)) {
    
  case VAL_TYPE_SYMBOL:
    ret = env_lookup(lisp, env, &tmp);
    if (!ret) {
      ret = env_lookup(lisp, global_env, &tmp);
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
	uint32_t env_cpy;
	if (! env_copy_shallow(env,&env_cpy))
	  return ENC_SYM(symrepr_eerror());

	uint32_t closure = cons(ENC_SYM(symrepr_closure()),
				cons(car(cdr(lisp)),
				     cons(car(cdr(cdr(lisp))),
					  cons(env_cpy, ENC_SYM(symrepr_nil())))));

	return apply_continuation(K, closure); 
      }

      // Special form: IF 
      if (DEC_SYM(head) == symrepr_if()) {

	push_u32(K,car(cdr(cdr(cdr(lisp))))); // else branch
	push_u32(K,car(cdr(cdr(lisp)))); // Then branch
	push_k(K,if_cont);

	*lisp_in = car(cdr(lisp)); // condition
	*env_in  = *env_in;
	longjmp(rewind_buf, 1); 
	
      }

      // Special form: LET
      if (DEC_SYM(head) == symrepr_let()) {
	uint32_t orig_env = *env_in;
	uint32_t binds   = car(cdr(lisp)); // key value pairs.
	uint32_t exp     = car(cdr(cdr(lisp))); // exp to evaluate in the new env.
	// Setup the bindings

	//
	return process_let(binds, orig_env, exp);
      }
    } // If head is symbol
    
    // Possibly an application form:
    push_u32(K, *env_in); // The environment each element should be evaluated in 
    push_u32(K, cdr(lisp)); // list of arguments that needs to be evaluated. 
    push_k(K, function_cont); 
    
    *lisp_in = head;
    *env_in  = *env_in;
    longjmp(rewind_buf, 1); 
    
  }
}
      

int run_eval(uint32_t lisp, uint32_t env) {

  uint32_t half_heap = heap_size() / 2; 
  
  push_k(K,done);
  
  curr_exp = lisp;
  curr_env = env;

  uint32_t r;
  
  //printf("run_eval\n"); 
  
  if (setjmp(rewind_buf)) {
    //printf("Rewind!\n");

    //printf("Continuation size: %d\n", K->size);
    //printf("Continuation sp:   %d\n", K->sp); 
    //printf("USED HEAP:         %d\n", heap_size() - heap_num_allocated()); 
    
    if (heap_size() - heap_num_allocated() < half_heap ){
      // GC also needs info about things alive in the "continuation"
      heap_perform_gc_aux(global_env, curr_env, curr_exp, K->data, K->sp);
    }
    
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

  K = init_cont_stack(1000);
  
  global_env = ENC_SYM(symrepr_nil());

  global_env = built_in_gen_env();
  
  uint32_t nil_entry = cons(ENC_SYM(symrepr_nil()), ENC_SYM(symrepr_nil()));
  global_env = cons(nil_entry, global_env);

  
  return 1;
}
