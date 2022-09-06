/*
    Copyright 2022 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef LBM_CHANNEL_H_
#define LBM_CHANNEL_H_

#include <stdint.h>
#include <stdbool.h>

#define TOKENIZER_BUFFER_SIZE 257

struct lbm_char_channel_s {
  void *state;

  /* Read side */
  bool (*more)(struct lbm_char_channel_s*);
  bool (*peek)(struct lbm_char_channel_s*, unsigned int, char *);
  bool (*read)(struct lbm_char_channel_s*, char *);
  bool (*drop)(struct lbm_char_channel_s*, unsigned int);
  bool (*comment)(struct lbm_char_channel_s *);
  void (*set_comment)(struct lbm_char_channel_s *);

  /* Write side */
  bool (*write)(struct lbm_char_channel_s*, char c);
  bool (*close)(struct lbm_char_channel_s*);

  /* Statistics */
  unsigned int (*row)(struct lbm_char_channel_s*);
  unsigned int (*column)(struct lbm_char_channel_s*);
};

typedef struct lbm_char_channel_s lbm_char_channel_t;

typedef struct {
  char buffer[TOKENIZER_BUFFER_SIZE];
  unsigned int write_pos;
  unsigned int read_pos;
  bool more;
  bool comment;

  // statistics
  unsigned int row;
  unsigned int column;
} lbm_buffered_channel_state_t;

bool more(lbm_char_channel_t *ch);
bool peek(lbm_char_channel_t *ch, unsigned int n, char *res);
bool read(lbm_char_channel_t *ch, char *res);
bool drop(lbm_char_channel_t *ch, unsigned int n);
bool comment(lbm_char_channel_t *ch);
void set_comment(lbm_char_channel_t *ch, bool comment);

/* Write side */
bool write(lbm_char_channel_t *ch, char c);
bool close(lbm_char_channel_t *ch);

/* Statistics */
unsigned int row(lbm_char_channel_t *ch);
unsigned int column(lbm_char_channel_t *ch);



void lbm_create_buffered_char_channel(lbm_buffered_channel_state_t *state,
				      lbm_char_channel_t *ch);

#endif
