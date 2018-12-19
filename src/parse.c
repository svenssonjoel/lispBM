/*
    Copyright 2018 Joel Svensson	svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "mpc.h"


/* Grammar */
mpc_parser_t* Comment;
mpc_parser_t* Exp;
mpc_parser_t* SExp;
mpc_parser_t* QExp;
mpc_parser_t* Program;
mpc_parser_t* Integer;
mpc_parser_t* Float;
mpc_parser_t* Character;
mpc_parser_t* String;
mpc_parser_t* Name;

int parser_init(void) {

  Comment = mpc_new("comment");
  Exp  = mpc_new("exp");
  SExp = mpc_new("sexp");
  QExp = mpc_new("qexp");
  Program = mpc_new("program");
  Integer = mpc_new("integer");
  Float   = mpc_new("float");
  Character = mpc_new("character");
  String = mpc_new("stringlit"); 
  Name = mpc_new("name");

  mpca_lang(MPCA_LANG_DEFAULT,
	    "program   : /^/ (<exp>|<comment>)+ /$/ ;"
	    "comment   : /[;]+.*/ ;"
	    "exp       : <float> | <integer> | <name> | <character> "
	    "          |  <sexp> | <qexp> ;"
	    "sexp      : '(' <exp>* ')' | '(' <exp> '.' <exp> ')' ;"
	    "qexp      : '\'' <exp> ;"
	    "integer   : /0x([0-9a-fA-F]+)|([0-9]+(U|I|u)?)/ ;"
	    "float     : /[0-9]+\\.?[0-9]+/;"
	    "character : /\\\\#newline|\\\\#./ ;"
	    "stringlit :  /\"(\\\\.|[^\"])*\"/ ;"
	    "name      : /[a-zA-Z+\\*\\-\\/?><=]+[a-zA-Z0-9+\\-\\*\\/?><=]*/;",
	    Program, Comment, Exp, SExp, QExp, Integer, Float , Character, String, Name, NULL) ;

  return 1;
}

void parser_del(void) {

  mpc_cleanup(9, Comment, Exp, SExp, QExp, Program, Integer, Float, Character, String, Name);
}


mpc_ast_t* parser_parse_string(char *input) {

  mpc_result_t r;

  if( mpc_parse("<string>", input, Program, &r)) {
    return (mpc_ast_t*)r.output;
  }
  return NULL;
}
