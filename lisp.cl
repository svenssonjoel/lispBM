

comment ";" ;

token Identifier ( (letter | '+' | '-' | '*' | '/' | '?' | '!' | '<' | '>' | '=' | '\\')
                   (letter | digit | '+' | '-' | '*' | '/' | '?' | '!' | '.' | '<' | '>' | '=' )* ) ;

Program.  Prg  ::= [SExp] ;

SNil.     SExp ::= "("  ")" ;
SList.    SExp ::= "(" [SExp] ")" ;
SAtom.    SExp ::= Atom ;

Id.       Atom ::= Identifier ; 
Integer.  Atom ::= Integer ;
Float.    Atom ::= Double ;
String.   Atom ::= String ; 

terminator SExp "" ;

