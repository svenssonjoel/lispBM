/*
    Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

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


// WORK IN PROGRESS

#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "tokpar.h"


#define TOKOPENPAR      0
#define TOKCLOSEPAR     1
#define TOKQUOTE        2
#define TOKSYMBOL       3
#define TOKINT          4
#define TOKUINT         5
#define TOKBOXEDINT     6
#define TOKBOXEDUINT    7
#define TOKBOXEDFLOAT   8
#define TOKSTRING       9
#define TOKENIZER_ERROR 1024
#define TOKENIZER_END   2048

typedef struct {
  
  unsigned int type;
  union {
    char  *text;
    INT   i;
    UINT  u;
    FLOAT f;
  }data;
} token;


typedef struct {
  char *str;
  unsigned int pos;
} tokenizer_state; 
    

int tok_openpar(char *str) {
  if (*str == '(')
    return 1;
  return 0;
}

int tok_closepar(char *str) {
  if (*str == ')')
    return 1;
  return 0;
}

bool symchar0(char c) {
  char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+*-/";
  
  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

bool symchar(char c) {
  char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/";
  
  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

int tok_symbol(char *str, char* res) {

  if (symchar0(*str)) res[0] = *str;
  else return 0;
  
  int n = 1;
  while (symchar(*(str+n))) { // TODO: overflow check
    res[n] = *(str+n);         
    n++;
  }
  return n;
}


int tok_i(char *str, INT *res) {

  INT acc = 0; 
  int n = 0;

  while ( *(str+n) >= '0' && *(str+n) <= '9' ){
    acc = (acc*10) + (*(str+n) - '0');
    n++;
  }

  // Not needed if strict adherence to ordering of calls to tokenizers.
  if (*(str+n) == 'U' ||
      *(str+n) == 'u' ||
      *(str+n) == '.' ||
      *(str+n) == 'I') return 0;

  *res = acc;
  return n;

}

token next_token(tokenizer_state *ts) {

  token t;
  char *curr = ts->str + ts->pos;
  char buff[2048];
  INT i_val;
  
  // eat whitespace
  int n = 0; 
  while ( *curr && isspace(*curr) ) {
    curr = curr+1;
    n++;
  }
  ts->pos += n;
  if ( *curr == 0 ) {
    t.type = TOKENIZER_END;
    return t;
  }

  n = 0;
  if ((n = tok_openpar(curr))) {
    ts->pos += n;
    t.type = TOKOPENPAR;
    return t;
  }

  if ((n = tok_closepar(curr))) {
    ts->pos += n;
    t.type = TOKCLOSEPAR;
    return t;
  }

  if ((n = tok_symbol(curr, buff))) {
    ts->pos += n;
    t.data.text = malloc(n+1);
    memset(t.data.text, 0, n+1);
    memcpy(t.data.text,buff, n);
    t.type = TOKSYMBOL;
    return t;
  }

  // Shortest form of integer match. Move to last in chain of numerical tokens. 
  if ((n = tok_i(curr, &i_val))) {
    ts->pos += n;
    t.data.i = i_val;
    t.type = TOKINT;
    return t;
  }
      
     
  t.type = TOKENIZER_ERROR;
  return t;
}


VALUE tokpar_parse_string(char *str) { 

  printf("Parsing string: %s\n", str);
  
  tokenizer_state ts;
  ts.str = str;
  ts.pos = 0;

  token tok = next_token(&ts);
  while (tok.type != TOKENIZER_END &&
	 tok.type != TOKENIZER_ERROR) {

    switch(tok.type) {

    case TOKOPENPAR:
      printf("TOKEN: %d\t(\n", tok.type);
      break;
    case TOKCLOSEPAR:
      printf("TOKEN: %d\t)\n", tok.type);
      break;
    case TOKQUOTE:
      printf("TOKEN: %d\t'\n", tok.type);
      break;
    case TOKSYMBOL:
      printf("TOKEN: %d\t%s\n", tok.type, tok.data.text);
      free(tok.data.text);
      break;
    case TOKINT:
      printf("TOKEN: %d\t%d\n", tok.type, tok.data.i);
      break;
    case TOKUINT:
      printf("TOKEN: %d\t%u\n", tok.type, tok.data.u);
      break;
    case TOKBOXEDINT:
      printf("TOKEN: %d\t%d\n", tok.type, tok.data.i);
      break;
    case TOKBOXEDUINT:
      printf("TOKEN: %d\t%u\n", tok.type, tok.data.u);
      break;
    case TOKBOXEDFLOAT:
      printf("TOKEN: %d\t%f\n", tok.type, tok.data.f);
      break;
    case TOKSTRING:
      printf("TOKEN: %d\t%s\n", tok.type, tok.data.text);
      free(tok.data.text);
      break;
    default:
      printf("ERROR\n");
      break;
    }
    tok = next_token(&ts); 
  }
  
  
  return 0;
}
