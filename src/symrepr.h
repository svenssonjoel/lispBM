#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <stdint.h> 

extern int symrepr_addsym(char *, uint32_t*); 
extern int symrepr_init(void);
extern void symrepr_print(void);
extern int symrepr_lookup(char *, uint32_t*);
extern char* symrepr_lookup_name(uint32_t); 
extern void symrepr_del(void);


extern uint32_t symrepr_nil();
extern uint32_t symrepr_quote();
extern uint32_t symrepr_car();
extern uint32_t symrepr_cdr();
extern uint32_t symrepr_cons();
extern uint32_t symrepr_lambda();
extern uint32_t symrepr_closure(); 

extern uint32_t symrepr_rerror();
#endif 
