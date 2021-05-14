/*
    Copyright 2019, 2021 Joel Svensson	svenssonjoel@yahoo.se

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

#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "tokpar.h"
#include "symrepr.h"
#include "heap.h"
#include "typedefs.h"
#include "compression.h"
#include "qq_expand.h"
#include "memory.h"
#include "env.h"

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
#define TOKCHAR         10
#define TOKBACKQUOTE    11
#define TOKCOMMA        12
#define TOKCOMMAAT      13
#define TOKENIZER_ERROR 1024
#define TOKENIZER_END   2048

typedef struct {

  unsigned int type;

  unsigned int text_len;
  union {
    char  c;
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


typedef struct tcs{
  void *state;
  bool (*more)(struct tcs);
  char (*get)(struct tcs);
  char (*peek)(struct tcs, unsigned int);
  void (*drop)(struct tcs, unsigned int);
} tokenizer_char_stream;

// Todo: Try to figure out how to do GC while reading
/* static int gc() { */
/*   gc_state_inc(); */
/*   gc_mark_freelist(); */
/*   gc_mark_phase(*env_get_global_ptr()); */
/*   return gc_sweep_phase(); */
/* } */

bool more(tokenizer_char_stream str) {
  return str.more(str);
}

char get(tokenizer_char_stream str) {
  return str.get(str);
}

char peek(tokenizer_char_stream str, unsigned int n) {
  return str.peek(str,n);
}

void drop(tokenizer_char_stream str, unsigned int n) {
  str.drop(str,n);
}

int tok_openpar(tokenizer_char_stream str) {
  if (peek(str,0) == '(') {
    drop(str,1);
    return 1;
  }
  return 0;
}

int tok_closepar(tokenizer_char_stream str) {
  if (peek(str,0) == ')') {
    drop(str,1);
    return 1;
  }
  return 0;
}

int tok_quote(tokenizer_char_stream str) {
  if (peek(str,0) == '\'') {
    drop(str,1);
    return 1;
  }
  return 0;
}

int tok_backquote(tokenizer_char_stream str) {
  if (peek(str,0) == '`') {
    drop(str, 1);
    return 1;
  }
  return 0;
}

int tok_commaat(tokenizer_char_stream str) {
  if (peek(str,0) == ',' &&
      peek(str,1) == '@') {
    drop(str,2);
    return 2;
  }
  return 0;
} 

int tok_comma(tokenizer_char_stream str) {
  if (peek(str,0) == ',') {
    drop(str, 1);
    return 1;
  }
  return 0;
}

bool symchar0(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/=<>";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

bool symchar(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/=<>";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

int tok_symbol(tokenizer_char_stream str, char** res) {

  if (!symchar0(peek(str,0)))  return 0;

  unsigned int i = 0;
  unsigned int len = 1;
  int n = 0;

  while (symchar((peek(str,len)))) {
    len++;
  }

  *res = malloc(len+1);
  if (*res == NULL) return -1;
  memset(*res,0,len+1);

  int c = 0;

  for (i = 0; i < len; i ++) {
    c = tolower(get(str));
    if (c >= 0 && c <= 255) {
      (*res)[i] = (char)c; 
      n++;
    } else {
      return -1;
    }
  }
  /* means the longest len of a symbol allowed is 2^31-1 */
  /* Should be a check that the length makes sense */
  return (int)n;
}

int tok_string(tokenizer_char_stream str, char **res) {

  unsigned int i = 0;
  int n = 0;
  unsigned int len = 0;
  if (!(peek(str,0) == '\"')) return 0;

  get(str); // remove the " char
  n++;

  // compute length of string
  while (peek(str,len) != 0 &&
	 peek(str,len) != '\"') {
    len++;
  }

  // str ends before tokenized string is closed.
  if ((peek(str,len)) != '\"') {
    return 0;
  }

  // allocate memory for result string
  *res = malloc(len+1);
  if (*res == NULL) return -1;
  memset(*res, 0, len+1);

  for (i = 0; i < len; i ++) {
    (*res)[i] = get(str);
    n++;
  }

  get(str);  // throw away the "
  /* The longest readable string is 2^31-1 characters. enforce this with a check */
  return (int)(n+1);
}

int tok_char(tokenizer_char_stream str, char *res) {

  int count = 0;
  if (peek(str,0) == '\\' &&
      peek(str,1) == '#' &&
      peek(str,2) == 'n' &&
      peek(str,3) == 'e' &&
      peek(str,4) == 'w' &&
      peek(str,5) == 'l' &&
      peek(str,6) == 'i' &&
      peek(str,7) == 'n' &&
      peek(str,8) == 'e') {
    *res = '\n';
    drop(str,9);
    count = 9;
  } else if (peek(str,0) == '\\' &&
	     peek(str,1) == '#' &&
	     isgraph(peek(str,2))) {
    *res = peek(str,2);
    drop(str,3);
    count = 3;
  }
  return count;
}

int tok_i(tokenizer_char_stream str, INT *res) {

  INT acc = 0;
  unsigned int n = 0;

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (peek(str,n) - '0');
    n++;
  }

  // Not needed if strict adherence to ordering of calls to tokenizers.
  if (peek(str,n) == 'U' ||
      peek(str,n) == 'u' ||
      peek(str,n) == '.' ||
      peek(str,n) == 'I') return 0;

  drop(str,n);
  *res = acc;
  return (int)n; /*check that isnt so high that it becomes a negative number when casted */
}

int tok_I(tokenizer_char_stream str, INT *res) {
  INT acc = 0;
  unsigned int n = 0;

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (peek(str,n) - '0');
    n++;
  }

  if (peek(str,n) == 'i' &&
      peek(str,n+1) == '3' &&
      peek(str,n+2) == '2') {
    *res = acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_u(tokenizer_char_stream str, UINT *res) {
  UINT acc = 0;
  unsigned int n = 0;

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (UINT)(peek(str,n) - '0');
    n++;
  }

  if (peek(str,n) == 'u' &&
      peek(str,n+1) == '2' &&
      peek(str,n+2) == '8' ) {
    *res = acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_U(tokenizer_char_stream str, UINT *res) {
  UINT acc = 0;
  unsigned int n = 0;

  // Check if hex notation is used
  if (peek(str,0) == '0' &&
      (peek(str,1) == 'x' || peek(str,1) == 'X')) {
    n+= 2;
    while ( (peek(str,n) >= '0' && peek(str,n) <= '9') ||
	    (peek(str,n) >= 'a' && peek(str,n) <= 'f') ||
	    (peek(str,n) >= 'A' && peek(str,n) <= 'F')){
      UINT val;
      if (peek(str,n) >= 'a' && peek(str,n) <= 'f') {
	val = 10 + (UINT)(peek(str,n) - 'a');
      } else if (peek(str,n) >= 'A' && peek(str,n) <= 'F') {
	val = 10 + (UINT)(peek(str,n) - 'A');
      } else {
	val = (UINT)peek(str,n) - '0';
      }
      acc = (acc * 0x10) + val;
      n++;
    }
    *res = acc;
    drop(str,n);
    return (int)n;
  }

  // check if nonhex
  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (UINT)(peek(str,n) - '0');
    n++;
  }

  if (peek(str,n) == 'u' &&
      peek(str,n+1) == '3' &&
      peek(str,n+2) == '2') {
    *res = acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_F(tokenizer_char_stream str, FLOAT *res) {

  unsigned int n = 0;
  unsigned int m = 0;
  char fbuf[256];

  while ( peek(str,n) >= '0' && peek(str,n) <= '9') n++;

  if ( peek(str,n) == '.') n++;
  else return 0;

  if ( !(peek(str,n) >= '0' && peek(str,n) <= '9')) return 0;
  while ( peek(str,n) >= '0' && peek(str,n) <= '9') n++;

  if (n > 255) m = 255;
  else m = n;

  unsigned int i;
  for (i = 0; i < m; i ++) {
    fbuf[i] = get(str);
  }

  fbuf[i] = 0;
  *res = (float)strtod(fbuf, NULL);
  return (int)n;
}


token next_token(tokenizer_char_stream str) {
  token t;

  INT i_val;
  UINT u_val;
  char c_val;
  FLOAT f_val;
  int n = 0;

  if (!more(str)) {
    t.type = TOKENIZER_END;
    return t;
  }

  // Eat whitespace and comments.
  bool clean_whitespace = true;
  while ( clean_whitespace ){
    if ( peek(str,0) == ';' ) {
      while ( more(str) && peek(str, 0) != '\n') {
	drop(str,1);
      }
    } else if ( isspace(peek(str,0))) {
      drop(str,1);
    } else {
      clean_whitespace = false;
    }
  }

  // Check for end of string again
  if (!more(str)) {
    t.type = TOKENIZER_END;
    return t;
  }

  if (tok_quote(str)) {
    t.type = TOKQUOTE;
    return t;
  }

  if (tok_backquote(str)) {
    t.type = TOKBACKQUOTE;
    return t;
  }

  if (tok_commaat(str)) {
    t.type= TOKCOMMAAT;
    return t;
  }
  
  if (tok_comma(str)) {
    t.type = TOKCOMMA;
    return t;
  }

  if (tok_openpar(str)) {
    t.type = TOKOPENPAR;
    return t;
  }

  if (tok_closepar(str)) {
    t.type = TOKCLOSEPAR;
    return t;
  }

  n = tok_symbol(str, &t.data.text);
  if (n > 0) {
    t.text_len = (unsigned int)n;
    t.type = TOKSYMBOL;
    return t;
  } else if (n < 0) {
    t.type = TOKENIZER_ERROR;
    return t;
  }
   
  if (tok_char(str, &c_val)) {
    t.data.c = c_val;
    t.type = TOKCHAR;
    return t;
  }

  n = tok_string(str, &t.data.text);
  if (n >= 2) {
    t.text_len = (unsigned int)n - 2;
    t.type = TOKSTRING;
    return t;
  } else if (n < 0) {
    t.type = TOKENIZER_ERROR;
    return t;
  }

  if (tok_F(str, &f_val)) {
    t.data.f = f_val;
    t.type = TOKBOXEDFLOAT;
    return t;
  }

  if (tok_U(str, &u_val)) {
    t.data.u = u_val;
    t.type = TOKBOXEDUINT;
    return t;
  }


  if (tok_u(str, &u_val)) {
    t.data.u = u_val;
    t.type = TOKUINT;
    return t;
  }

  if (tok_I(str, &i_val)) {
    t.data.i = i_val;
    t.type = TOKBOXEDINT;
    return t;
  }

  // Shortest form of integer match. Move to last in chain of numerical tokens.
  if (tok_i(str, &i_val)) {
    t.data.i = i_val;
    t.type = TOKINT;
    return t;
  }

  t.type = TOKENIZER_ERROR;
  return t;
}

VALUE parse_sexp(token tok, tokenizer_char_stream str);
VALUE parse_sexp_list(token tok, tokenizer_char_stream str);

VALUE parse_program(tokenizer_char_stream str) {
  token tok = next_token(str);
  VALUE head;
  VALUE tail;

  if (tok.type == TOKENIZER_ERROR) {
    return enc_sym(symrepr_rerror());
  }

  if (tok.type == TOKENIZER_END) {
    return enc_sym(symrepr_nil());
  }

  head = parse_sexp(tok, str);
  tail = parse_program(str);

  return cons(head, tail);
}

VALUE parse_sexp(token tok, tokenizer_char_stream str) {

  VALUE v;
  token t;

  switch (tok.type) {
  case TOKENIZER_END:
    return enc_sym(symrepr_rerror());
  case TOKENIZER_ERROR:
    return enc_sym(symrepr_rerror());
  case TOKOPENPAR:
    t = next_token(str);
    return parse_sexp_list(t,str);
  case TOKSYMBOL: {
    UINT symbol_id;

    if (symrepr_lookup(tok.data.text, &symbol_id)) {
      v = enc_sym(symbol_id);
    }
    else if (symrepr_addsym(tok.data.text, &symbol_id)) {
      v = enc_sym(symbol_id);
    } else {
      v = enc_sym(symrepr_rerror());
    }
    free(tok.data.text);
    return v;
  }
  case TOKSTRING: {
    heap_allocate_array(&v, tok.text_len+1, VAL_TYPE_CHAR);
    array_header_t *arr = (array_header_t*)car(v);
    char *data = (char *)arr + 8;
    memset(data, 0, (tok.text_len+1) * sizeof(char));
    memcpy(data, tok.data.text, tok.text_len * sizeof(char));
    free(tok.data.text);
    return v;
  }
  case TOKINT:
    return enc_i(tok.data.i);
  case TOKUINT:
    return enc_u(tok.data.u);
  case TOKCHAR:
    return enc_char(tok.data.c);
  case TOKBOXEDINT:
    return set_ptr_type(cons((VALUE)tok.data.i, enc_sym(DEF_REPR_BOXED_I_TYPE)), PTR_TYPE_BOXED_I);
  case TOKBOXEDUINT:
    return set_ptr_type(cons(tok.data.u, enc_sym(DEF_REPR_BOXED_U_TYPE)), PTR_TYPE_BOXED_U);
  case TOKBOXEDFLOAT:
    return set_ptr_type(cons(tok.data.u, enc_sym(DEF_REPR_BOXED_F_TYPE)), PTR_TYPE_BOXED_F);
  case TOKQUOTE: {
    t = next_token(str);
    VALUE quoted = parse_sexp(t, str);
    if (type_of(quoted) == VAL_TYPE_SYMBOL &&
	dec_sym(quoted) == symrepr_rerror()) return quoted;
    return cons(enc_sym(symrepr_quote()), cons (quoted, enc_sym(symrepr_nil()))); 
  }
  case TOKBACKQUOTE: {
    t = next_token(str);
    VALUE quoted = parse_sexp(t, str);
    if (type_of(quoted) == VAL_TYPE_SYMBOL &&
	dec_sym(quoted) == symrepr_rerror()) return quoted;
    VALUE expanded = qq_expand(quoted);
    if (type_of(expanded) == VAL_TYPE_SYMBOL &&
	symrepr_is_error(dec_sym(expanded))) return expanded;
    return expanded;
  }
  case TOKCOMMAAT: {
    t = next_token(str);
    VALUE splice = parse_sexp(t, str);
    if (type_of(splice) == VAL_TYPE_SYMBOL &&
	dec_sym(splice) == symrepr_rerror()) return splice;
    return cons(enc_sym(symrepr_commaat()), cons (splice, enc_sym(symrepr_nil())));
  }
  case TOKCOMMA: {
    t = next_token(str);
    VALUE unquoted = parse_sexp(t, str);
    if (type_of(unquoted) == VAL_TYPE_SYMBOL &&
	dec_sym(unquoted) == symrepr_rerror()) return unquoted;
    return cons(enc_sym(symrepr_comma()), cons (unquoted, enc_sym(symrepr_nil()))); 
  }
  }
  return enc_sym(symrepr_rerror());
}

VALUE parse_sexp_list(token tok, tokenizer_char_stream str) {

  token t;
  VALUE head;
  VALUE tail;

  switch (tok.type) {
  case TOKENIZER_END:
    return enc_sym(symrepr_rerror());
  case TOKENIZER_ERROR:
    return enc_sym(symrepr_rerror());
  case TOKCLOSEPAR:
    return enc_sym(symrepr_nil());
  default:
    head = parse_sexp(tok, str);
    t = next_token(str);
    tail = parse_sexp_list(t, str);
    if ((type_of(head) == VAL_TYPE_SYMBOL &&
	 dec_sym(head) == symrepr_rerror() ) ||
	(type_of(tail) == VAL_TYPE_SYMBOL &&
	 dec_sym(tail) == symrepr_rerror() )) return enc_sym(symrepr_rerror());
    return cons(head, tail);
  }

  return enc_sym(symrepr_rerror());
}

bool more_string(tokenizer_char_stream str) {
  tokenizer_state *s = (tokenizer_state*)str.state;
  return s->str[s->pos] != 0;
}

char get_string(tokenizer_char_stream str) {
  tokenizer_state *s = (tokenizer_state*)str.state;
  char c = s->str[s->pos];
  s->pos = s->pos + 1;
  return c;
}

char peek_string(tokenizer_char_stream str, unsigned int n) {
  tokenizer_state *s = (tokenizer_state*)str.state;
  // TODO error checking ?? how ?
  char c = s->str[s->pos + n];
  return c;
}

void drop_string(tokenizer_char_stream str, unsigned int n) {
  tokenizer_state *s = (tokenizer_state*)str.state;
  s->pos = s->pos + n;
}

VALUE tokpar_parse(char *string) {

  tokenizer_state ts;
  ts.str = string;
  ts.pos = 0;

  tokenizer_char_stream str;
  str.state = &ts;
  str.more = more_string;
  str.peek = peek_string;
  str.drop = drop_string;
  str.get  = get_string;

  return parse_program(str);
}

#define DECOMP_BUFF_SIZE 32
typedef struct {
  decomp_state ds;
  char decomp_buff[DECOMP_BUFF_SIZE];
  int  decomp_bytes;
  int  buff_pos;
} tokenizer_compressed_state;

bool more_compressed(tokenizer_char_stream str) {
  tokenizer_compressed_state *s = (tokenizer_compressed_state*)str.state;
  bool more =
    (s->ds.i < s->ds.compressed_bits + 32) ||
    (s->buff_pos < s->decomp_bytes);
  return more;
}

char get_compressed(tokenizer_char_stream str) {
  tokenizer_compressed_state *s = (tokenizer_compressed_state*)str.state;

  if (s->ds.i >= s->ds.compressed_bits + 32 &&
      (s->buff_pos >= s->decomp_bytes)) {
    return 0;
  }

  if (s->buff_pos >= s->decomp_bytes) {
    int n = compression_decompress_incremental(&s->ds, s->decomp_buff,DECOMP_BUFF_SIZE);
    if (n == 0) {
      return 0;
    }
    s->decomp_bytes = n;
    s->buff_pos = 0;
  }
  char c = s->decomp_buff[s->buff_pos];
  s->buff_pos += 1;
  return c;
}

char peek_compressed(tokenizer_char_stream str, unsigned int n) {
  tokenizer_compressed_state *s = (tokenizer_compressed_state*)str.state;

  tokenizer_compressed_state old;

  memcpy(&old, s, sizeof(tokenizer_compressed_state));

  char c = get_compressed(str);;
  for (unsigned int i = 1; i <= n; i ++) {
    c = get_compressed(str);
  }

  memcpy(str.state, &old, sizeof(tokenizer_compressed_state));
  return c;
}


void drop_compressed(tokenizer_char_stream str, unsigned int n) {
  for (unsigned int i = 0; i < n; i ++) {
    get_compressed(str);
  }
}


VALUE tokpar_parse_compressed(char *bytes) {

  tokenizer_compressed_state ts;

  ts.decomp_bytes = 0;
  memset(ts.decomp_buff, 0, 32);
  ts.buff_pos = 0;

  compression_init_state(&ts.ds, bytes);

  tokenizer_char_stream str;
  str.state = &ts;
  str.more = more_compressed;
  str.get = get_compressed;
  str.peek = peek_compressed;
  str.drop = drop_compressed;

  return parse_program(str);
}
