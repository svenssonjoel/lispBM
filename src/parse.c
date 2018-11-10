
#include "mpc.h"


/* Grammar */

mpc_parser_t* Atom;
mpc_parser_t* SExp;
mpc_parser_t* QExp;
mpc_parser_t* Program;
mpc_parser_t* Integer;
mpc_parser_t* Float;
mpc_parser_t* Name;

int parser_init(void) {
  
  Atom = mpc_new("atom");
  SExp = mpc_new("sexp");
  QExp = mpc_new("qexp");
  Program = mpc_new("program");
  Integer = mpc_new("integer");
  Float   = mpc_new("float");
  Name = mpc_new("name");
  
  mpca_lang(MPCA_LANG_DEFAULT,
	    "program   : /^/ <sexp>+ /$/ ;"
	    "atom      : <float> | <integer> | <name> ;"
	    "sexp      : <atom> | '(' <sexp>* ')' | '(' <sexp> '.' <sexp> ')' ;"
	    "qexp      : '<sexp> ;"
	    "integer   : /[0-9]+/ ;"
	    "float     : /-?[0-9]+\\.?[0-9]+/;"
	    "name      : /[a-zA-Z+\\*\\-\\/?><=]+[a-zA-Z0-9+\\-\\*\\/?><=]*/;",
	    Program, Atom, SExp, QExp, Integer, Float , Name, NULL) ;
  
  return 1; 
}

void parser_del(void) {
  
  mpc_cleanup(6, Atom, SExp, QExp, Program, Integer, Float, Name);
} 


mpc_ast_t* parser_parse_string(char *input) {

  mpc_result_t r;
  
  if( mpc_parse("<string>", input, Program, &r)) {
    return (mpc_ast_t*)r.output;
  }
  return NULL; 
} 
