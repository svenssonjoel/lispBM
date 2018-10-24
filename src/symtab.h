#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <stdint.h> 

int symtab_addname(char *, uint32_t*); 
int symtab_init(void);
void symtab_print(void);
int symtab_lookup(char *, uint32_t*);
char* symtab_lookup_name(uint32_t); 
void symtab_del(void);

#endif 
