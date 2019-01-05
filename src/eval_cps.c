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

#include "symrepr.h"
#include "heap.h"
#include "builtin.h"
#include "print.h"
#include "env.h"

#ifdef VISUALIZE_HEAP
#include "heap_vis.h" 
#endif 


#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

/* 
   Attempt to implement an evaluator in continuation passing style: 
   (In spite of C not being very suitable for the task.) 
   
   - No idea if this will work..
     # Seems to be. In the right direction at least. 

   - I hope this is possible without C being able to generate functions
     on the fly (and without going all the way to some jit setup).
     # Looks like I was very incorrect in thinking anything like that 
       would be needed. 

   - peeking a lot at: http://lisperator.net/pltut/cps-evaluator/stack-guard
     during the implementation of this.
     # I still dont understand what goes on there. 
     
   - Want to rewind the stack using setjmp longjmp.
     #Probably not essential. Or maybe it is..  

   - Memory management will be real tricky since cannot rely on data on the stack.
     # Instead created another stack to depend upon. 


   - Bah, this is too complicated and messy. Will try something new. 
*/

#define EVAL_CONTINUE 1
#define PERFORM_GC    2

static uint32_t run_eval(uint32_t orig_prg, uint32_t lisp, uint32_t env);

jmp_buf rewind_buf;

static uint32_t curr_exp; 
static uint32_t curr_env; 

// The hope is that for tail-recursive function this stack will not "grow".
typedef struct {
  uint32_t* data;
  int32_t   sp;
  uint32_t  size; 
} stack;

stack *K; // Stack describes the current continuation.
stack *K_save; // Stack save area for resume after gc. 

stack* init_cont_stack(int stack_size) {

  stack *s = malloc(sizeof(stack));
 
  s->data = malloc(sizeof(uint32_t) * stack_size);
  s->sp = 0;
  s->size = stack_size;

  return s;
}

int clear_stack(stack *s) {
  s->sp = 0;
  return 1; 
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

int copy_stack(stack *dest, stack *src) {
  while (dest->size < src->sp) {
    grow_stack(dest);
  }
  
  dest->sp = src->sp;
  memcpy(dest->data, src->data, src->sp * sizeof(uint32_t)); 
  
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


uint32_t eval_cps_bi(uint32_t lisp) {
 
  curr_exp = car(lisp);
  curr_env = curr_env;
  longjmp(rewind_buf,EVAL_CONTINUE); 
}


uint32_t done(uint32_t arg) {
  return arg;
}

uint32_t apply_continuation(stack *K){

  //printf("apply_continuation\n");

  uint32_t (*k)(uint32_t);
  uint32_t args; 
  pop_u32(K, &args);
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

  // check if cons was unsuccessful
  if (TYPE_OF(keyval) == VAL_TYPE_SYMBOL &&
      DEC_SYM(keyval) == symrepr_merror()) {
    // Abort computation and perform GC.
    push_u32(K, key);
    push_k(K, set_global_env);
    longjmp(rewind_buf, PERFORM_GC);
  }
  global_env = cons(keyval,global_env);
  if (TYPE_OF(global_env) == VAL_TYPE_SYMBOL &&
      DEC_SYM(global_env) == symrepr_merror()) {
    push_u32(K, key);
    push_k(K, set_global_env);
    longjmp(rewind_buf, PERFORM_GC);
  }

  return ENC_SYM(symrepr_true());
}


uint32_t function_app(uint32_t args) {

  uint32_t args_rev;
  uint32_t fun;
  pop_u32(K, &fun);
  
  args_rev = reverse(args);

  if (TYPE_OF(args_rev) == VAL_TYPE_SYMBOL &&
      DEC_SYM(args_rev) == symrepr_merror()) {
    push_u32(K, fun);
    push_k(K, function_app);
    longjmp(rewind_buf, PERFORM_GC); 
  }
  
  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(fun));

  if (f == NULL) {
    //printf("Built in function does not exist");
    return ENC_SYM(symrepr_eerror());
  }
  uint32_t f_res = f(args_rev);
  push_u32(K, f_res);
  return apply_continuation(K); 
}

uint32_t closure_app(uint32_t args) {

  uint32_t args_rev;
  uint32_t closure;
  pop_u32(K, &closure);

  args_rev = reverse(args);
  if (TYPE_OF(args) == VAL_TYPE_SYMBOL &&
      DEC_SYM(args) == symrepr_merror()) {
    push_u32(K, closure);
    push_k(K, closure_app);
    longjmp(rewind_buf, PERFORM_GC); 
  }

  uint32_t params  = car(cdr(closure));
  uint32_t exp     = car(cdr(cdr(closure)));
  uint32_t clo_env = car(cdr(cdr(cdr(closure))));

  uint32_t local_env;
  if (!env_build_params_args(params, args_rev, clo_env, &local_env)) {
    push_u32(K, closure);
    push_k(K, closure_app);
    longjmp(rewind_buf, PERFORM_GC);
  }
  
  curr_exp = exp;
  curr_env = local_env; 
  longjmp(rewind_buf, EVAL_CONTINUE); 
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
    if (TYPE_OF(args) == VAL_TYPE_SYMBOL &&
	DEC_SYM(args) == symrepr_merror()) {
      push_u32(K, env);
      push_u32(K, acc);
      push_u32(K, rest);
      push_k(K, eval_rest);
      
      longjmp(rewind_buf, PERFORM_GC); 
    }
    push_u32(K, args);
    return apply_continuation(K);
  }

  acc = cons(head, acc);
  if (TYPE_OF(acc) == VAL_TYPE_SYMBOL &&
      DEC_SYM(acc) == symrepr_merror()) {
    push_u32(K, env); 
    push_u32(K, acc);
    push_u32(K, rest);
    push_k(K, eval_rest);

    longjmp(rewind_buf, PERFORM_GC);
  }
  
  push_u32(K, env); 
  push_u32(K, acc);
  push_u32(K, cdr(rest));
  push_k(K, eval_rest);
  
  curr_exp = car(rest);
  curr_env = env;
  longjmp(rewind_buf, EVAL_CONTINUE); 
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
  longjmp(rewind_buf, EVAL_CONTINUE); 
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
    longjmp(rewind_buf, EVAL_CONTINUE);
  }

  // Otherwise evaluate the expression in the populated env
  uint32_t exp;
  pop_u32(K, &exp);
  curr_exp = exp;
  curr_env = env;
  longjmp(rewind_buf, EVAL_CONTINUE); 
  
}

uint32_t process_let(uint32_t binds, uint32_t orig_env, uint32_t exp) {
  uint32_t curr = binds;
  uint32_t new_env = orig_env;

  if (TYPE_OF(binds) != PTR_TYPE_CONS) return ENC_SYM(symrepr_eerror()); 
  
  while (TYPE_OF(curr) == PTR_TYPE_CONS) { 
    uint32_t key = car(car(curr));
    uint32_t val = ENC_SYM(symrepr_nil()); // a temporary
    uint32_t binding = cons(key,val);
    if (TYPE_OF(binding) == VAL_TYPE_SYMBOL &&
	DEC_SYM(binding) == symrepr_merror()) {
      longjmp(rewind_buf, PERFORM_GC);
    }
    new_env = cons(binding, new_env);
    if (TYPE_OF(new_env) == VAL_TYPE_SYMBOL &&
	DEC_SYM(new_env) == symrepr_merror()) {
      longjmp(rewind_buf, PERFORM_GC);
    }
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
  longjmp(rewind_buf, EVAL_CONTINUE); 
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
    longjmp(rewind_buf,EVAL_CONTINUE); 
  } else {
    curr_exp = else_branch;
    //curr_env = curr_env;
    longjmp(rewind_buf,EVAL_CONTINUE); 
  }
}
		  
uint32_t eval_cps(uint32_t lisp, uint32_t env) {
    
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
      push_u32(K, tmp);
      return apply_continuation(K);
    }
    return ENC_SYM(symrepr_eerror());

  case PTR_TYPE_F32:
  case PTR_TYPE_U32:
  case VAL_TYPE_I28:
  case PTR_TYPE_I32:
  case VAL_TYPE_CHAR:
  case VAL_TYPE_U28:
  case PTR_TYPE_ARRAY:{
    push_u32(K, lisp);
    return apply_continuation(K);
  }

  case PTR_TYPE_REF:
  case PTR_TYPE_STREAM:
    return ENC_SYM(symrepr_eerror());
    break;
 
  case PTR_TYPE_CONS:
    head = car(lisp);
    
    if (TYPE_OF(head) == VAL_TYPE_SYMBOL) {
      
      // Special form: QUOTE
      if (DEC_SYM(head) == symrepr_quote()) {
	uint32_t val =  car(cdr(lisp));
	push_u32(K, val);
	return apply_continuation(K);
      }
      
      // Special form: DEFINE 
      if (DEC_SYM(head) == symrepr_define()) {
	uint32_t key = car(cdr(lisp));
	uint32_t val_exp = car(cdr(cdr(lisp)));
	
	if (TYPE_OF(key) != VAL_TYPE_SYMBOL ||
	    DEC_SYM(key) == symrepr_nil()) {
	  return ENC_SYM(symrepr_eerror()); 
	}

	push_u32(K, key);
	push_k(K,set_global_env); 
		
	curr_exp = val_exp;
	curr_env = env;
	longjmp(rewind_buf,EVAL_CONTINUE); 
      }

      // Special form: LAMBDA 
      if (DEC_SYM(head) == symrepr_lambda()) {
	uint32_t env_cpy;
	if (!env_copy_shallow(env,&env_cpy))
	  longjmp(rewind_buf, PERFORM_GC); 

	//printf("Making closure\n");

	uint32_t env_end = cons(env_cpy,ENC_SYM(symrepr_nil()));
	uint32_t body    = cons(car(cdr(cdr(lisp))), env_end);
	uint32_t params  = cons(car(cdr(lisp)), body);
	uint32_t clos    = cons(ENC_SYM(symrepr_closure()), params);

	if (TYPE_OF(env_end) == VAL_TYPE_SYMBOL ||
	    TYPE_OF(body)    == VAL_TYPE_SYMBOL ||
	    TYPE_OF(params)  == VAL_TYPE_SYMBOL ||
	    TYPE_OF(clos)    == VAL_TYPE_SYMBOL) {
	  longjmp(rewind_buf, PERFORM_GC); 
	}
	uint32_t closure = clos; 
	/* uint32_t closure = cons(ENC_SYM(symrepr_closure()), */
	/* 			cons(car(cdr(lisp)), */
	/* 			     cons(car(cdr(cdr(lisp))), */
	/* 				  cons(env_cpy, ENC_SYM(symrepr_nil()))))); */
	//if (TYPE_OF(closure) == VAL_TYPE_SYMBOL &&
	//    DEC_SYM(closure) == symrepr_merror()) {
	//  longjmp(rewind_buf, PERFORM_GC);
	//}

	push_u32(K,closure);
	return apply_continuation(K); 
      }

      // Special form: IF 
      if (DEC_SYM(head) == symrepr_if()) {

	push_u32(K,car(cdr(cdr(cdr(lisp))))); // else branch
	push_u32(K,car(cdr(cdr(lisp)))); // Then branch
	push_k(K,if_cont);

	curr_exp = car(cdr(lisp)); // condition
	curr_env = curr_env;
	longjmp(rewind_buf, EVAL_CONTINUE); 
	
      }

      // Special form: LET
      if (DEC_SYM(head) == symrepr_let()) {
	uint32_t orig_env = env;
	uint32_t binds   = car(cdr(lisp)); // key value pairs.
	uint32_t exp     = car(cdr(cdr(lisp))); // exp to evaluate in the new env.
	// Setup the bindings

	//
	return process_let(binds, orig_env, exp);
      }
    } // If head is symbol
    
    // Possibly an application form:
    push_u32(K, curr_env);  // The environment each element should be evaluated in 
    push_u32(K, cdr(lisp)); // list of arguments that needs to be evaluated. 
    push_k(K, function_cont); 
    
    curr_exp = head;
    curr_env = curr_env;
    longjmp(rewind_buf, EVAL_CONTINUE); 
    
  default:
    // BUG No applicable case!
    return ENC_SYM(symrepr_eerror());
    break;
  }
}
      

uint32_t run_eval(uint32_t orig_prg, uint32_t lisp, uint32_t env){

  uint32_t half_heap = heap_size() / 2; 
  
  push_k(K,done);
  push_k(K_save,done);
  
  curr_exp = lisp;
  curr_env = env;

  uint32_t r;

  int res = setjmp(rewind_buf);
  
  if (res) {
  
#ifdef VISUALIZE_HEAP
    heap_vis_gen_image();
#endif 
    if (res == PERFORM_GC) {
      //printf("***  PERFORMING GC ***\n");
      // restore stack
      clear_stack(K);
      copy_stack(K, K_save); 
      
      heap_perform_gc_aux(global_env, curr_env, curr_exp, orig_prg, K->data, K->sp);
      
      //printf("RESUMING EVAL: %x\n", curr_exp);
      //printf("STACK SP: %d\n", K->sp);


    }
    //if (heap_size() - heap_num_allocated() < half_heap ){ 
      // GC also needs info about things alive in the "continuation" 
    //  heap_perform_gc_aux(global_env, curr_env, curr_exp, orig_prg, K->data, K->sp); 
    //}

    if(res == EVAL_CONTINUE) {
      //printf("CONTINUING EVAL: %x\n", curr_exp);
      //printf("STACK SP: %d\n", K->sp);
      clear_stack(K_save);
      copy_stack(K_save, K); 
    }
    
      
    r = eval_cps(curr_exp, curr_env);

  } else {

    // kickstarts evaluation with the done_cont;
    r = eval_cps(curr_exp, curr_env);
  }

  return r;
}

uint32_t eval_cps_program(uint32_t lisp) {

  uint32_t curr = lisp;
  uint32_t res = ENC_SYM(symrepr_nil());
  
  while (TYPE_OF(curr) == PTR_TYPE_CONS) {
    uint32_t val = run_eval(lisp, car(curr),ENC_SYM(symrepr_nil()));
    res = cons(val,res);
    if (TYPE_OF(res) == VAL_TYPE_SYMBOL) {
      //simple_print(val); printf("\n");
      //simple_print(res); printf("\n");
      printf("ERROR: Memory full\n"); 
    }
    curr = cdr(curr); 
  }

  return reverse(res);
}



int eval_cps_init() {

  K = init_cont_stack(1000);
  K_save = init_cont_stack(1000);

  int res = builtin_add_function("eval",eval_cps_bi); 
  
  global_env = ENC_SYM(symrepr_nil());

  global_env = built_in_gen_env();
  
  uint32_t nil_entry = cons(ENC_SYM(symrepr_nil()), ENC_SYM(symrepr_nil()));
  global_env = cons(nil_entry, global_env);

  if (TYPE_OF(nil_entry) == VAL_TYPE_SYMBOL ||
      TYPE_OF(global_env) == VAL_TYPE_SYMBOL) res = 0; 
  
  return res;
}
