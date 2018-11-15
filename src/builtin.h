#ifndef BUILT_IN_H_
#define BUILT_IN_H_

typedef uint32_t (*bi_fptr)(uint32_t);

bi_fptr builtin_lookup_function(uint32_t sym);
int builtin_add_function(char *sym_str, bi_fptr fun_ptr);
int builtin_init();

#endif 
