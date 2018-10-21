
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


extern int init_symtab();
extern void del_symtab(); 
extern uint32_t add_symbol(char *);
extern void print_symtab(); 


#endif 
