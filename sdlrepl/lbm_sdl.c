/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se

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

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_timer.h>

#include <stdio.h>
#include <unistd.h>
#include <stdbool.h>

#include <lispbm.h>
#include <lbm_custom_type.h>

typedef struct {
  uint32_t sdl_id;
  char *name;
  lbm_uint sym_id;
} sdl_symbol_t;

sdl_symbol_t lbm_sdl_events[] = {
  {0       , "sdl-no-event", 0},
  {SDL_QUIT, "sdl-quit-event", 0}
};


static int register_sdl_event_symbols(void) {
  for (int i = 0; i < (sizeof(lbm_sdl_events) / sizeof(sdl_symbol_t)); i ++) {
    if (!lbm_add_symbol(lbm_sdl_events[i].name, &lbm_sdl_events[i].sym_id))
      return 0;
  }
  return 1;
}

static lbm_uint lookup_sdl_event_symbol(uint32_t sdl_event) {
  for (int i = 0; i < (sizeof(lbm_sdl_events) / sizeof(sdl_symbol_t)); i ++) {
    if (sdl_event == lbm_sdl_events[i].sdl_id) {
      return lbm_sdl_events[i].sym_id;
    }
  }
  return lbm_sdl_events[0].sym_id;
}

static lbm_value ext_sdl_init(lbm_value *args, lbm_uint argn) {
  // TODO init differently depending on args
  lbm_value res = lbm_enc_sym(SYM_NIL);

  if (SDL_Init(SDL_INIT_EVERYTHING) == 0) {
    res = lbm_enc_sym(SYM_TRUE);
  }
  return res;
}

static bool sdl_window_destructor(lbm_uint lbm_mem_ptr) {

  lbm_uint *m = (lbm_uint *)lbm_mem_ptr;
  lbm_uint win = m[CUSTOM_TYPE_VALUE];
  SDL_DestroyWindow((SDL_Window*)win);
  return true;
}

static lbm_value ext_sdl_create_window(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_NIL);

  if (argn == 2) {

    int32_t w = lbm_dec_as_i32(args[0]);
    int32_t h = lbm_dec_as_i32(args[1]);

    SDL_Window* win = SDL_CreateWindow("SDL Window",
                                       SDL_WINDOWPOS_CENTERED,
                                       SDL_WINDOWPOS_CENTERED,
                                       w, h, 0);

    if (win && !lbm_custom_type_create((lbm_uint)win, sdl_window_destructor, "SDLWindow", &res)) {
      SDL_DestroyWindow(win);
    }
  }
  return res;
}

static bool sdl_renderer_destructor(lbm_uint lbm_mem_ptr) {

  lbm_uint *m = (lbm_uint *)lbm_mem_ptr;
  lbm_uint rend = m[CUSTOM_TYPE_VALUE];
  SDL_DestroyRenderer((SDL_Renderer*)rend);
  return true;
}

static lbm_value ext_sdl_create_soft_renderer(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_NIL);

  if (argn == 1 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {

    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);

    SDL_Renderer* rend = SDL_CreateRenderer((SDL_Window *)m[CUSTOM_TYPE_VALUE],
                                            -1,SDL_RENDERER_SOFTWARE);
    if (rend && !lbm_custom_type_create((lbm_uint)rend, sdl_renderer_destructor, "SDLRenderer", &res)) {
      SDL_DestroyRenderer(rend);
    }
  }
  return res;
}

static lbm_value ext_sdl_renderer_set_color(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_TRUE);

  if (argn == 4 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer *)m[CUSTOM_TYPE_VALUE];

    uint8_t r = lbm_dec_as_char(args[1]);
    uint8_t g = lbm_dec_as_char(args[2]);
    uint8_t b = lbm_dec_as_char(args[3]);

    /* SDL functions are 0 on success */
    if (SDL_SetRenderDrawColor(rend, r, g, b, SDL_ALPHA_OPAQUE)) {
      res = lbm_enc_sym(SYM_NIL);
    }
  }
  return res;
}


static lbm_value ext_sdl_draw_line(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_TRUE);

  if (argn == 5 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];

    int32_t x1 = lbm_dec_as_i32(args[1]);
    int32_t y1 = lbm_dec_as_i32(args[2]);
    int32_t x2 = lbm_dec_as_i32(args[3]);
    int32_t y2 = lbm_dec_as_i32(args[4]);

    if (SDL_RenderDrawLine(rend, x1, y1, x2, y2)) {
      res = lbm_enc_sym(SYM_NIL);
    }
  }
  return res;
}

static lbm_value ext_sdl_clear(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_TRUE);

  if (argn == 1 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];

    if (SDL_RenderClear(rend)) {
      res = lbm_enc_sym(SYM_NIL);
    }
  }
  return res;
}

static lbm_value ext_sdl_present(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_TRUE);

  if (argn == 1 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];

    SDL_RenderPresent(rend);
  }
  return res;
}


static lbm_value ext_sdl_poll_event(lbm_value *args, lbm_uint argn) {

  SDL_Event event;

  if (SDL_PollEvent(&event) == 0)
    return lbm_enc_sym(lookup_sdl_event_symbol(0));
  return lbm_enc_sym(lookup_sdl_event_symbol(event.type));
}


bool lbm_sdl_init(void) {

  bool res = true;

  register_sdl_event_symbols();

  res = res && lbm_add_extension("sdl-init", ext_sdl_init);
  res = res && lbm_add_extension("sdl-create-window",ext_sdl_create_window);
  res = res && lbm_add_extension("sdl-create-soft-renderer", ext_sdl_create_soft_renderer);
  res = res && lbm_add_extension("sdl-renderer-set-color", ext_sdl_renderer_set_color);
  res = res && lbm_add_extension("sdl-draw-line", ext_sdl_draw_line);
  res = res && lbm_add_extension("sdl-clear", ext_sdl_clear);
  res = res && lbm_add_extension("sdl-present", ext_sdl_present);
  res = res && lbm_add_extension("sdl-poll-event", ext_sdl_poll_event);
  return res;
}
