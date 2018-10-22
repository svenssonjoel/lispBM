
#ifndef PARSE_H_INCLUDED_
#define PARSE_H_INCLUDED_

#include "mpc.h"

extern int parser_init(void);
extern void parser_del(void);
extern mpc_ast_t* parser_parse_string(char *input); 


#endif
