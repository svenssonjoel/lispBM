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

#include <lbm_channel.h>


bool more(lbm_char_channel_t *ch) {
  return ch->more(ch);
}

bool peek(lbm_char_channel_t *ch, unsigned int n, char *res) {
  return ch->peek(ch, n, res);
}

bool read(lbm_char_channel_t *ch, char *res) {
  return ch->read(ch, res);
}

bool drop(lbm_char_channel_t *ch, unsigned int n) {
  return ch->drop(ch, n);
}

bool comment(lbm_char_channel_t *ch) {
  return ch->comment(ch);
}

void set_comment(lbm_char_channel_t *ch) {
  return ch->set_comment(ch);
}

bool write(lbm_char_channel_t *ch, char c) {
  return ch->write(ch, c);
}

bool close(lbm_char_channel_t *ch) {
  return ch->close(ch);
}

unsigned int row(lbm_char_channel_t *ch) {
  return ch->row(ch);
}

unsigned int column(lbm_char_channel_t *ch) {
  return ch->column(ch);
}


static bool buffered_more(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = ch->state;
  return st->more;
}

static bool buffered_close(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = ch->state;
  st->more = false;
  return true;
}

static bool buffered_peek(lbm_char_channel_t *ch, unsigned int n, char *res) {
  lbm_buffered_channel_state_t *st = ch->state;
  char *buffer = st->buffer;

  unsigned int peek_pos = (st->read_pos + n) % TOKENIZER_BUFFER_SIZE;
  bool in_data;
  if (st->write_pos >= st->read_pos) {
    in_data = peek_pos < st->write_pos;
  } else {
    in_data = !(peek_pos >= st->write_pos && peek_pos < st->read_pos);
  }

  if (in_data) {
    *res = buffer[peek_pos];
    return true;
  }
  return false;
}

static bool buffer_is_empty(lbm_buffered_channel_state_t *st) {
  if (st->read_pos == st->write_pos) {
    return true;
  }
  return false;
}

static bool buffer_is_full(lbm_buffered_channel_state_t *st) {
  if (st->write_pos == st->read_pos - 1 ||
      (st->read_pos == 0 &&
       st->write_pos == TOKENIZER_BUFFER_SIZE-1)) {
    return true;
  }
  return false;
}

static bool buffered_read(lbm_char_channel_t *ch, char *res) {
  lbm_buffered_channel_state_t *st = ch->state;
  char *buffer = st->buffer;

  if (buffer_is_empty(st)) {
    return false;
  }

  *res = buffer[st->read_pos];
  st->read_pos = (st->read_pos + 1) % TOKENIZER_BUFFER_SIZE;
  return true;
}

static bool buffered_drop(lbm_char_channel_t *ch, unsigned int n) {
  lbm_buffered_channel_state_t *st = ch->state;
  bool r = true;
  char c;
  if (n > 0) {
    do {
      r = buffered_read(ch, &c);
    } while (n > 0 && r);
  }
  return r;
}

static bool buffered_write(lbm_char_channel_t *ch, char c) {
  lbm_buffered_channel_state_t *st = ch->state;
  char *buffer = st->buffer;
  if (buffer_is_full(st)) {
    return false;
  }

  buffer[st->write_pos] = c;
  st->write_pos = (st->write_pos + 1) % TOKENIZER_BUFFER_SIZE;
  return true;
}

static unsigned int buffered_row(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = ch->state;
  return st->row;
}

static unsigned int buffered_column(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = ch->state;
  return st->column;
}

static bool buffered_comment(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = ch->state;
  return st->comment;
}

static void buffered_set_comment(lbm_char_channel_t *ch, bool comment) {
  lbm_buffered_channel_state_t *st = ch->state;
  st->comment = comment;
}

void lbm_create_buffered_char_channel(lbm_buffered_channel_state_t *state,
				      lbm_char_channel_t *ch) {

  memset(state->buffer, 0 , TOKENIZER_BUFFER_SIZE);
  state->write_pos = 0;
  state->read_pos = 0;
  state->row = 1;
  state->column = 1;
  state->more = true;
  state->comment = false;

  ch->state = (void*)state;
  ch->more = buffered_more;
  ch->peek = buffered_peek;
  ch->read = buffered_read;
  ch->drop = buffered_drop;
  ch->write = buffered_write;
  ch->close = buffered_close;
  ch->row = buffered_row;
  ch->column = buffered_column;
  ch->comment = buffered_comment;
  ch->set_comment = buffered_set_comment;
}

