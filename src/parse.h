
#ifndef PARSE_H_INCLUDED_
#define PARSE_H_INCLUDED_

#include "mpc.h"

extern void init_parser(void);
extern void destroy_parser(void);
extern mpc_ast_t* parse_string(char *input); 


#endif
