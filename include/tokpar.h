/*
    Copyright 2019 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef TOKPAR_H_
#define TOKPAR_H_

#include "lispbm_types.h"

typedef struct {
  char *str;
  unsigned int pos;
} tokenizer_string_state_t;

extern void tokpar_create_char_stream_from_string(tokenizer_string_state_t *,
                                                  tokenizer_char_stream *,
                                                  char *);
extern VALUE tokpar_parse(tokenizer_char_stream *str);
extern VALUE tokpar_parse_program(tokenizer_char_stream *str);

#endif
