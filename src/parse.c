
#include "mpc.h"


/* Grammar */

mpc_parser_t* Atom;
mpc_parser_t* SExp;
mpc_parser_t* Program;
mpc_parser_t* Value;
mpc_parser_t* Name;

void init_parser(void) {
  
  Atom = mpc_new("atom");
  SExp = mpc_new("sexp");
  Program = mpc_new("program");
  Value = mpc_new("value");
  Name = mpc_new("name");

  mpca_lang(MPCA_LANG_DEFAULT,
	    "atom      : <value> | <name> ;"
	    "sexp      : <atom>  | '(' <sexp>+ ')' ;"
	    "program   : <sexp>+ ;"
	    "value     : /[0-9]+/;"
	    "name      : /[a-zA-Z+\\*\\-\\/?><=]+[a-zA-Z0-9+\\-\\*\\/?><=]*/;",
	    Atom, SExp, Program, Value, Name);  
}

void destroy_parser(void) {
  
  mpc_cleanup(5, Atom, SExp, Program, Value, Name);
} 


mpc_ast_t* parse_string(char *input) {

  mpc_result_t r;
  
  if( mpc_parse("<string>", input, Program, &r)) {
    return (mpc_ast_t*)r.output;
  }
  return NULL; 
} 
