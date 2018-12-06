#ifndef EVAL_CPS_H_
#define EVAL_CPS_H_

extern uint32_t eval_cps_get_env(void);

extern uint32_t eval_cps(uint32_t, uint32_t); 
extern int eval_cps_init(void);

extern int run_eval(uint32_t lisp, uint32_t env);
#endif
