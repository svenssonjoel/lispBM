#ifndef HTAB_H_
#define HTAB_H_

#include <stdint.h> 

#define HTAB_SIZE 65521  
#define SMALL_PRIMES 11

int htab_addname(char *, uint32_t*); 
int htab_init(void);
void htab_print(void);
int htab_lookup(char *, uint32_t*);
char* htab_lookup_name(uint32_t); 
void htab_del(void);

#endif 
