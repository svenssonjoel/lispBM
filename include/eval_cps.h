/*
    Copyright 2018, 2020 Joel Svensson	svenssonjoel@yahoo.se

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
#ifndef EVAL_CPS_H_
#define EVAL_CPS_H_

#include "stack.h"

typedef struct eval_context_s{
  VALUE program;
  VALUE curr_exp;
  VALUE curr_env;
  VALUE r;
  bool  done;
  bool  app_cont;
  stack K;
  uint32_t timestamp;
  uint32_t sleep_us;
  struct eval_context_s *prev;
  struct eval_context_s *next;
} eval_context_t;

extern void eval_cps_set_usleep_callback(void (*fptr)(uint32_t));
extern void eval_cps_set_timestamp_us_callback(uint32_t (*fptr)(void));
extern void eval_cps_set_ctx_done_callback(void (*fptr)(eval_context_t *));


extern eval_context_t *eval_cps_get_current_context(void);
extern eval_context_t *eval_cps_new_context_inherit_env(VALUE program, VALUE curr_exp);
extern void eval_cps_drop_top_context(void);

extern VALUE eval_cps_get_env(void);
extern int eval_cps_init(unsigned int initial_stack_size,
			 bool grow_continuation_stack);
extern void eval_cps_del(void);
extern VALUE eval_cps_program(VALUE lisp);
extern VALUE eval_cps_bi_eval(VALUE exp);
#endif
