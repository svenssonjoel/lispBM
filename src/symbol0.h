
#ifndef SYMBOL_H_
#define SYMBOL_H_

#include <stdint.h> 

/* 2048 symbols can be defined before a additional allocation is needed */ 
#define INITIAL_SYMBOL_TABLE_SIZE 2048


typedef struct s_symtab {
  char **symbols; 
  struct s_symtab* next;  /* additonal symbols go here, 
			     increases lookup time   */
} symtab_t;


extern int symtab_init();
extern void symtab_del(); 
extern int symtab_addsym(char *, uint32_t *);
extern void symtab_print();  


#endif 
