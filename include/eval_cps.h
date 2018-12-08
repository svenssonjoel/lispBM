#ifndef EVAL_CPS_H_
#define EVAL_CPS_H_

extern uint32_t eval_cps_get_env(void);

extern int eval_cps_init(void);
extern uint32_t eval_cps_program(uint32_t lisp);
#endif
