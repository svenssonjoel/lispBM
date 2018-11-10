#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <stdint.h> 

int symrepr_addsym(char *, uint32_t*); 
int symrepr_init(void);
void symrepr_print(void);
int symrepr_lookup(char *, uint32_t*);
char* symrepr_lookup_name(uint32_t); 
void symrepr_del(void);

#endif 
