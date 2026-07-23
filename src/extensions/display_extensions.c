/*
  Copyright 2023 - 2025 Benjamin Vedder            benjamin@vedder.se
  Copyright 2023 - 2026 Joel Svensson              svenssonjoel@yahoo.se
  Copyright 2023        Rasmus Söderhielm          rasmus.soderhielm@gmail.com
  Copyright 2025        Joakim Lundborg            joakim.lundborg@gmail.com

  This file is part of LispBM. (Originally a part of the vesc_express FW)

  LispBM is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LispBM is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "tjpgd.h"

#include <math.h>
#include <string.h>

#include <extensions/display_extensions.h>
#include <lbm_utils.h>
#include <lbm_defrag_mem.h>

#ifdef LBM_OPT_DISPLAY_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_DISPLAY_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif

#define MAX_WIDTH 32000
#define MAX_HEIGHT 32000

static lbm_uint symbol_indexed2 = 0;
static lbm_uint symbol_indexed4 = 0;
static lbm_uint symbol_indexed16 = 0;
static lbm_uint symbol_rgb332 = 0;
static lbm_uint symbol_rgb565 = 0;
static lbm_uint symbol_rgb888 = 0;

static lbm_uint symbol_thickness = 0;
static lbm_uint symbol_filled = 0;
static lbm_uint symbol_rounded = 0;
static lbm_uint symbol_dotted = 0;
static lbm_uint symbol_scale = 0;
static lbm_uint symbol_rotate = 0;
static lbm_uint symbol_resolution = 0;
static lbm_uint symbol_tile = 0;
static lbm_uint symbol_clip = 0;


static lbm_uint symbol_regular = 0;
static lbm_uint symbol_gradient_x = 0;
static lbm_uint symbol_gradient_y = 0;
static lbm_uint symbol_gradient_x_pre = 0;
static lbm_uint symbol_gradient_y_pre = 0;
static lbm_uint symbol_repeat = 0;
static lbm_uint symbol_mirrored = 0;

static lbm_uint symbol_color_0 = 0;
static lbm_uint symbol_color_1 = 0;
static lbm_uint symbol_width = 0;
static lbm_uint symbol_offset = 0;
static lbm_uint symbol_repeat_type = 0;
static lbm_uint symbol_alpha = 0;

static lbm_uint symbol_down = 0;
static lbm_uint symbol_up = 0;
static lbm_uint symbol_magnify = 0;
static lbm_uint symbol_spacing = 0;
static lbm_uint symbol_align = 0;
static lbm_uint symbol_left = 0;
static lbm_uint symbol_center = 0;
static lbm_uint symbol_right = 0;

bool display_is_symbol_up(lbm_value v) {
  if (lbm_is_symbol(v)) {
    lbm_uint s = lbm_dec_sym(v);
    return (s == symbol_up);
  }
  return false;
}

bool display_is_symbol_down(lbm_value v) {
  if (lbm_is_symbol(v)) {
    lbm_uint s = lbm_dec_sym(v);
    return (s == symbol_down);
  }
  return false;
}

color_format_t sym_to_color_format(lbm_value v) {
  lbm_uint s = lbm_dec_sym(v);
  if (s == symbol_indexed2) return indexed2;
  if (s == symbol_indexed4) return indexed4;
  if (s == symbol_indexed16) return indexed16;
  if (s == symbol_rgb332) return rgb332;
  if (s == symbol_rgb565) return rgb565;
  if (s == symbol_rgb888) return rgb888;
  return format_not_supported;
}

#define COLOR_MAGIC (uint32_t)0x4C4F4300

#define COLOR_PRECALC_LEN	512

static lbm_value image_buffer_lift(uint8_t *buf, color_format_t fmt, uint16_t width, uint16_t height) {
  lbm_value res = ENC_SYM_MERROR;
  lbm_uint size = image_dims_to_size_bytes(fmt, width, height);
  if ( lbm_lift_array(&res, (char*)buf, IMAGE_BUFFER_HEADER_SIZE + size)) {
    buf[0] = (uint8_t)(width >> 8);
    buf[1] = (uint8_t)width;
    buf[2] = (uint8_t)(height >> 8);
    buf[3] = (uint8_t)height;
    buf[4] = color_format_to_byte(fmt);
  }
  return res;
}

static inline bool is_color_sized(lbm_uint size) {
  size_t color_size = sizeof(color_t);
  return (size == color_size);
}

static inline bool is_color(uint8_t *data, lbm_uint size) {
  bool res = false;
  if (is_color_sized(size)) {
    color_t *color = (color_t*)data;
    res = (color->magic == COLOR_MAGIC);
  }
  return res;
}


static lbm_value color_allocate(COLOR_TYPE type, int32_t color1, int32_t color2, uint16_t param1, uint16_t param2, bool mirrored, uint8_t alpha) {
  color_t *color = lbm_malloc(sizeof(color_t));
  if (!color) {
    return ENC_SYM_MERROR;
  }

  uint32_t *pre = 0;
  if (type == COLOR_PRE_X || type == COLOR_PRE_Y) {
    pre = lbm_malloc(COLOR_PRECALC_LEN * sizeof(uint32_t));
    if (!pre) {
      lbm_free(color);
      return ENC_SYM_MERROR;
    }
  }

  lbm_value res = ENC_SYM_MERROR;

  if (lbm_lift_array(&res, (char*)color, sizeof(color_t))) {
    color->magic = COLOR_MAGIC;
    color->type = type;
    color->color1 = color1;
    color->color2 = color2;
    color->param1 = param1;
    color->param2 = param2;
    color->mirrored = mirrored;
    color->alpha = alpha;
    color->precalc = pre;

    if (pre) {
      COLOR_TYPE type_old = color->type;
      if (type == COLOR_PRE_X) {
        color->type = COLOR_GRADIENT_X;
      } else if (type == COLOR_PRE_Y) {
        color->type = COLOR_GRADIENT_Y;
      }

      if (color->param1 > COLOR_PRECALC_LEN) {
        color->param1 = COLOR_PRECALC_LEN;
      }

      for (int i = 0;i < color->param1;i++) {
        pre[i] = lbm_display_rgb888_from_color(*color, i + color->param2, i + color->param2);
      }

      color->type = type_old;
    }
  } else {
    lbm_free(pre);
    lbm_free(color);
  }

  return res;
}

static lbm_value image_buffer_allocate(color_format_t fmt, uint16_t width, uint16_t height) {
  uint32_t size_bytes = image_dims_to_size_bytes(fmt, width, height);

  uint8_t *buf = lbm_malloc(IMAGE_BUFFER_HEADER_SIZE + size_bytes);
  if (!buf) {
    return ENC_SYM_MERROR;
  }
  memset(buf, 0, size_bytes + IMAGE_BUFFER_HEADER_SIZE);
  lbm_value res = image_buffer_lift(buf, fmt, width, height);
  if (lbm_is_symbol(res)) { /* something is wrong, free */
    lbm_free(buf);
  }
  return res;
}

static lbm_value image_buffer_allocate_dm(lbm_uint *dm, color_format_t fmt, uint16_t width, uint16_t height) {
  uint32_t size_bytes = image_dims_to_size_bytes(fmt, width, height);

  lbm_value res = lbm_defrag_mem_alloc(dm, IMAGE_BUFFER_HEADER_SIZE + size_bytes);
  lbm_array_header_t *arr = lbm_dec_array_r(res);
  if (arr) {
    uint8_t *buf = (uint8_t*)arr->data;
    buf[0] = (uint8_t)(width >> 8);
    buf[1] = (uint8_t)width;
    buf[2] = (uint8_t)(height >> 8);
    buf[3] = (uint8_t)height;
    buf[4] = color_format_to_byte(fmt);
  }
  return res;
}

// Exported interface
bool display_is_color(lbm_value v) {
  lbm_array_header_t *array = lbm_dec_array_r(v);
  bool res = false;
  if (array && is_color_sized(array->size)) {
    res = (is_color((uint8_t*)array->data, array->size));
  }
  return res;
}

static color_t *get_color(lbm_value v) {
  color_t *res = NULL;
  lbm_array_header_t *array = lbm_dec_array_r(v);
  if (array && is_color_sized(array->size)
      && (is_color((uint8_t*)array->data, array->size))) {
    res = (color_t*)array->data;
  }
  return res;
}


// Register symbols

static bool register_symbols(void) {
  bool res = true;
  res = res && lbm_add_symbol_const("indexed2", &symbol_indexed2);
  res = res && lbm_add_symbol_const("indexed4", &symbol_indexed4);
  res = res && lbm_add_symbol_const("indexed16", &symbol_indexed16);
  res = res && lbm_add_symbol_const("rgb332", &symbol_rgb332);
  res = res && lbm_add_symbol_const("rgb565", &symbol_rgb565);
  res = res && lbm_add_symbol_const("rgb888", &symbol_rgb888);

  res = res && lbm_add_symbol_const("thickness", &symbol_thickness);
  res = res && lbm_add_symbol_const("filled", &symbol_filled);
  res = res && lbm_add_symbol_const("rounded", &symbol_rounded);
  res = res && lbm_add_symbol_const("dotted", &symbol_dotted);
  res = res && lbm_add_symbol_const("scale", &symbol_scale);
  res = res && lbm_add_symbol_const("rotate", &symbol_rotate);
  res = res && lbm_add_symbol_const("resolution", &symbol_resolution);
  res = res && lbm_add_symbol_const("tile", &symbol_tile);
  res = res && lbm_add_symbol_const("clip", &symbol_clip);

  res = res && lbm_add_symbol_const("regular", &symbol_regular);
  res = res && lbm_add_symbol_const("gradient_x", &symbol_gradient_x);
  res = res && lbm_add_symbol_const("gradient_y", &symbol_gradient_y);
  res = res && lbm_add_symbol_const("gradient_x_pre", &symbol_gradient_x_pre);
  res = res && lbm_add_symbol_const("gradient_y_pre", &symbol_gradient_y_pre);
  res = res && lbm_add_symbol_const("mirrored", &symbol_mirrored);
  res = res && lbm_add_symbol_const("repeat", &symbol_repeat);

  res = res && lbm_add_symbol_const("color-0", &symbol_color_0);
  res = res && lbm_add_symbol_const("color-1", &symbol_color_1);
  res = res && lbm_add_symbol_const("width", &symbol_width);
  res = res && lbm_add_symbol_const("offset", &symbol_offset);
  res = res && lbm_add_symbol_const("repeat-type", &symbol_repeat_type);
  res = res && lbm_add_symbol_const("alpha", &symbol_alpha);

  res = res && lbm_add_symbol_const("down", &symbol_down);
  res = res && lbm_add_symbol_const("up", &symbol_up);
  res = res && lbm_add_symbol_const("magnify", &symbol_magnify);
  res = res && lbm_add_symbol_const("spacing", &symbol_spacing);
  res = res && lbm_add_symbol_const("align", &symbol_align);
  res = res && lbm_add_symbol_const("left", &symbol_left);
  res = res && lbm_add_symbol_const("center", &symbol_center);
  res = res && lbm_add_symbol_const("right", &symbol_right);

  return res;
}

static const uint8_t retro5x7[] = {
    5, 7, 91, 1, 0, 0, 0, 0, 0, 132, 16, 2, 8, 0, 74, 1, 0, 0, 0, 74,
    125, 245, 149, 2, 196, 23, 71, 31, 1, 115, 33, 34, 116, 6, 38, 21, 83, 147, 5, 132,
    0, 0, 0, 0, 136, 8, 33, 8, 2, 130, 32, 132, 136, 0, 64, 145, 79, 20, 0, 128,
    144, 79, 8, 0, 0, 0, 192, 136, 0, 0, 128, 15, 0, 0, 0, 0, 0, 140, 1, 16,
    33, 34, 68, 0, 46, 230, 58, 163, 3, 196, 16, 66, 136, 3, 46, 66, 38, 194, 7, 46,
    66, 7, 163, 3, 76, 165, 244, 17, 2, 63, 60, 8, 163, 3, 46, 132, 23, 163, 3, 31,
    34, 34, 132, 0, 46, 70, 23, 163, 3, 46, 70, 15, 161, 3, 192, 24, 96, 12, 0, 192,
    24, 96, 68, 0, 136, 136, 32, 8, 2, 0, 124, 240, 1, 0, 130, 32, 136, 136, 0, 46,
    66, 68, 0, 1, 46, 246, 218, 130, 3, 68, 197, 248, 99, 4, 47, 198, 23, 227, 3, 46,
    134, 16, 162, 3, 47, 198, 24, 227, 3, 63, 132, 23, 194, 7, 63, 132, 23, 66, 0, 46,
    134, 30, 163, 7, 49, 198, 31, 99, 4, 142, 16, 66, 136, 3, 28, 33, 132, 146, 1, 49,
    149, 81, 82, 4, 33, 132, 16, 194, 7, 113, 215, 24, 99, 4, 113, 214, 28, 99, 4, 46,
    198, 24, 163, 3, 47, 198, 23, 66, 0, 46, 198, 88, 179, 3, 47, 198, 87, 82, 4, 46,
    6, 7, 163, 3, 159, 16, 66, 8, 1, 49, 198, 24, 163, 3, 49, 198, 168, 20, 1, 49,
    198, 90, 119, 4, 49, 42, 162, 98, 4, 49, 42, 66, 8, 1, 31, 34, 34, 194, 7, 78,
    8, 33, 132, 3, 33, 8, 130, 32, 4, 14, 33, 132, 144, 3, 68, 69, 0, 0, 0, 0,
    0, 0, 192, 7, 134, 32, 0, 0, 0, 0, 56, 232, 163, 7, 33, 188, 24, 227, 3, 0,
    184, 16, 162, 3, 16, 250, 24, 163, 7, 0, 184, 248, 131, 3, 76, 136, 39, 132, 0, 192,
    199, 232, 161, 3, 33, 188, 24, 99, 4, 4, 24, 66, 136, 3, 16, 64, 8, 163, 3, 33,
    149, 81, 82, 4, 134, 16, 66, 136, 3, 0, 172, 90, 99, 4, 0, 188, 24, 99, 4, 0,
    184, 24, 163, 3, 224, 197, 248, 66, 0, 192, 199, 232, 33, 4, 0, 188, 24, 66, 0, 0,
    248, 224, 224, 3, 66, 60, 33, 36, 3, 0, 196, 24, 163, 7, 0, 196, 24, 21, 1, 0,
    196, 88, 171, 2, 0, 68, 69, 84, 4, 0, 196, 232, 161, 3, 0, 124, 68, 196, 7
  };

// returns: 1 parsed, 0 not an attr list, -1 malformed/invalid
static int parse_text_attr(lbm_value v, float *mag, int *spacing, int *align, int *rotation_deg) {
  if (!lbm_is_cons(v)) {
    return 0;
  }

  lbm_value key = lbm_car(v);
  lbm_value rest = lbm_cdr(v);
  if (!lbm_is_symbol(key) || !lbm_is_cons(rest)) {
    return 0;
  }

  lbm_value val = lbm_car(rest);
  if (lbm_cdr(rest) != ENC_SYM_NIL) {
    return -1;
  }

  lbm_uint sym = lbm_dec_sym(key);
  if (sym == symbol_magnify || sym == symbol_scale) {
    if (!lbm_is_number(val)) return -1;
    float m = lbm_dec_as_float(val);
    if (m < 0.1f) m = 0.1f;
    *mag = m;
    return 1;
  }

  if (sym == symbol_spacing) {
    if (!lbm_is_number(val)) return -1;
    *spacing = lbm_dec_as_i32(val);
    return 1;
  }

  if (sym == symbol_align) {
    if (lbm_is_number(val)) {
      int a = lbm_dec_as_i32(val);
      if (a < 0 || a > 2) return -1;
      *align = a;
      return 1;
    }

    if (lbm_is_symbol(val)) {
      lbm_uint av = lbm_dec_sym(val);
      if (av == symbol_left) { *align = 0; return 1; }
      if (av == symbol_center) { *align = 1; return 1; }
      if (av == symbol_right) { *align = 2; return 1; }
      return -1;
    }

    char *astr = lbm_dec_str(val);
    if (!astr) return -1;
    if (strcmp(astr, "left") == 0) { *align = 0; return 1; }
    if (strcmp(astr, "center") == 0) { *align = 1; return 1; }
    if (strcmp(astr, "right") == 0) { *align = 2; return 1; }
    return -1;
  }

  if (sym == symbol_rotate) {
    if (!lbm_is_number(val)) return -1;
    int r = lbm_dec_as_i32(val);
    *rotation_deg = r % 360;
    return 1;
  }

  return 0;
}

// Extensions

#define ATTR_MAX_ARGS	4
#define ARG_MAX_NUM	8

typedef struct {
  bool is_valid;
  uint16_t arg_num;
  lbm_value args[ATTR_MAX_ARGS];
} attr_t;

typedef struct {
  bool is_valid;
  image_buffer_t img;
  lbm_value args[ARG_MAX_NUM];
  uint8_t alpha;
  attr_t attr_thickness;
  attr_t attr_filled;
  attr_t attr_rounded;
  attr_t attr_dotted;
  attr_t attr_scale;
  attr_t attr_rotate;
  attr_t attr_resolution;
  attr_t attr_tile;
  attr_t attr_clip;
} img_args_t;

static img_args_t decode_args(lbm_value *args, lbm_uint argn, int num_expected) {
  img_args_t res;
  memset(&res, 0, sizeof(res));
  res.is_valid = false;
  res.alpha = 255;

  lbm_array_header_t *arr;
  if (argn >= 1 && (arr = get_image_buffer(args[0]))) {
    // at least one argument which is an image buffer.
    res.img.width = image_buffer_width((uint8_t*)arr->data);
    res.img.height = image_buffer_height((uint8_t*)arr->data);
    res.img.fmt = image_buffer_format((uint8_t*)arr->data);
    res.img.mem_base = (uint8_t*)arr->data;
    res.img.data = image_buffer_data((uint8_t*)arr->data);


    int num_dec = 0;
    for (unsigned int i = 1;i < argn;i++) {
      if (!lbm_is_number(args[i]) && !lbm_is_cons(args[i])) {
        color_t *clr = get_color(args[i]);
        if (clr && clr->type == COLOR_REGULAR &&
            num_dec == num_expected - 1) {
          res.args[num_dec] = lbm_enc_u32((uint32_t)clr->color1);
          res.alpha = clr->alpha;
          num_dec++;
          if (num_dec >= ARG_MAX_NUM) return res;
          continue;
        }
        return res;
      }

      if (lbm_is_number(args[i])) {
        res.args[num_dec] = args[i];
        num_dec++;

        if (num_dec >= ARG_MAX_NUM) {
          return res;
        }
      } else {
        lbm_value curr = args[i];
        int attr_ind = 0;
        attr_t *attr_now = 0;
        while (lbm_is_cons(curr)) {
          lbm_value  arg = lbm_car(curr);

          if (attr_ind == 0) {
            if (!lbm_is_symbol(arg)) {
              return res;
            }

            if (lbm_dec_sym(arg) == symbol_thickness) {
              attr_now = &res.attr_thickness;
              attr_now->arg_num = 1;
            } else if (lbm_dec_sym(arg) == symbol_filled) {
              attr_now = &res.attr_filled;
              attr_now->arg_num = 0;
            } else if (lbm_dec_sym(arg) == symbol_rounded) {
              attr_now = &res.attr_rounded;
              attr_now->arg_num = 1;
            } else if (lbm_dec_sym(arg) == symbol_dotted) {
              attr_now = &res.attr_dotted;
              attr_now->arg_num = 2;
            } else if (lbm_dec_sym(arg) == symbol_scale) {
              attr_now = &res.attr_scale;
              attr_now->arg_num = 1;
            } else if (lbm_dec_sym(arg) == symbol_rotate) {
              attr_now = &res.attr_rotate;
              attr_now->arg_num = 3;
            } else if (lbm_dec_sym(arg) == symbol_resolution) {
              attr_now = &res.attr_resolution;
              attr_now->arg_num = 1;
            } else if (lbm_dec_sym(arg) == symbol_tile) {
              attr_now = &res.attr_tile;
              attr_now->arg_num = 0;
            } else if (lbm_dec_sym(arg) == symbol_clip) {
              attr_now = &res.attr_clip;
              attr_now->arg_num = 4;
            }
            else {
              return res;
            }
          } else {
            if (!lbm_is_number(arg)) {
              return res;
            }

            attr_now->args[attr_ind - 1] = arg;
          }

          attr_ind++;
          if (attr_ind > (ATTR_MAX_ARGS + 1)) {
            return res;
          }

          curr = lbm_cdr(curr);
        }

        // does this really compare the pointer addresses?
        if (attr_now == &res.attr_rounded && attr_ind == 1) {
          attr_now->arg_num = 0; // the `rounded` attribute may be empty
        }


        if ((attr_ind - 1) == attr_now->arg_num) {
          attr_now->is_valid = true;
        } else {
          return res;
        }
      }
    }
    if (num_dec != num_expected) {
      return res;
    }
    // I think this should go here ???
    res.is_valid = true;
  }
  return res;
}

static lbm_value ext_image_dims(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 0);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  lbm_value dims = lbm_heap_allocate_list(2);
  if (lbm_is_symbol(dims)) {
    return dims;
  }
  lbm_value curr = dims;
  lbm_set_car(curr, lbm_enc_i(arg_dec.img.width));
  curr = lbm_cdr(curr);
  lbm_set_car(curr, lbm_enc_i(arg_dec.img.height));
  return dims;
}

static lbm_value ext_image_buffer(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  bool args_ok = false;
  color_format_t fmt = indexed2;
  lbm_uint w = 0;
  lbm_uint h = 0;

  if (argn == 4 &&
      lbm_is_defrag_mem(args[0]) &&
      lbm_is_symbol(args[1]) &&
      lbm_is_number(args[2]) &&
      lbm_is_number(args[3])) {
    fmt = sym_to_color_format(args[1]);
    w = lbm_dec_as_u32(args[2]);
    h = lbm_dec_as_u32(args[3]);
    args_ok = true;
  } else if (argn == 3 &&
             lbm_is_symbol(args[0]) &&
             lbm_is_number(args[1]) &&
             lbm_is_number(args[2])) {
    fmt = sym_to_color_format(args[0]);
    w = lbm_dec_as_u32(args[1]);
    h = lbm_dec_as_u32(args[2]);
    args_ok = true;
  }

  if (args_ok && fmt != format_not_supported && w > 0 && h > 0 && w < MAX_WIDTH && h < MAX_HEIGHT) {
    if (argn == 3) {
      res = image_buffer_allocate(fmt, (uint16_t)w, (uint16_t)h);
    } else {
      res = image_buffer_allocate_dm((lbm_uint*)lbm_car(args[0]), fmt, (uint16_t)w, (uint16_t)h);
    }
  }
  return res;
}


static lbm_value ext_is_image_buffer(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;

  if (argn == 1) {
    res = ENC_SYM_NIL;
    lbm_array_header_t *array = lbm_dec_array_r(args[0]);
    if (array) {
      uint8_t *data = (uint8_t*)array->data;
      if (image_buffer_is_valid(data, array->size)) {
        res = ENC_SYM_TRUE;;
      }
    }
  }
  return res;
}

static lbm_value ext_color(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;

  if (argn >= 2 && argn <= 7 &&
      lbm_is_symbol(args[0]) &&
      lbm_is_number(args[1])) {

    // Color1 and color2 are int in the struct and decoded as i32, why
    // where they stored in uint32_t?
    int32_t color1 = lbm_dec_as_i32(args[1]);

    int32_t color2 = 0;
    if (argn >= 3) {
      if (lbm_is_number(args[2])) {
        color2 = lbm_dec_as_i32(args[2]);
      } else {
        return ENC_SYM_TERROR;
      }
    }

    int32_t param1 = 0;
    if (argn >= 4) {
      if (lbm_is_number(args[3])) {
        param1 = lbm_dec_as_i32(args[3]);
      } else {
        return ENC_SYM_TERROR;
      }
    }

    int32_t param2 = 0;
    if (argn >= 5) {
      if (lbm_is_number(args[4])) {
        param2 = lbm_dec_as_i32(args[4]);
      } else {
        return ENC_SYM_TERROR;
      }
    }

    bool mirrored = false;
    lbm_uint alpha_arg = argn;

    if (argn >= 6) {
      if (lbm_is_symbol(args[5])) {
        lbm_uint sym = lbm_dec_sym(args[5]);
        if (sym == symbol_repeat) {
          mirrored = false;
        } else if (sym == symbol_mirrored) {
          mirrored = true;
        } else {
          return ENC_SYM_TERROR;
        }
        alpha_arg = 6;
      } else if (argn == 6 && lbm_is_number(args[5])) {
        alpha_arg = 5;
      } else {
        return ENC_SYM_TERROR;
      }
    }

    COLOR_TYPE t;
    if (lbm_dec_sym(args[0]) == symbol_regular) {
      t = COLOR_REGULAR;
    } else if (lbm_dec_sym(args[0]) == symbol_gradient_x) {
      t = COLOR_GRADIENT_X;
    } else if (lbm_dec_sym(args[0]) == symbol_gradient_y) {
      t = COLOR_GRADIENT_Y;
    } else if (lbm_dec_sym(args[0]) == symbol_gradient_x_pre) {
      t = COLOR_PRE_X;
    } else if (lbm_dec_sym(args[0]) == symbol_gradient_y_pre) {
      t = COLOR_PRE_Y;
    } else {
      return ENC_SYM_TERROR;
    }

    if (t == COLOR_REGULAR && argn == 3) {
      alpha_arg = 2;
    }

    uint8_t alpha = 255;
    if (alpha_arg < argn) {
      if (lbm_is_number(args[alpha_arg])) {
        int32_t a = lbm_dec_as_i32(args[alpha_arg]);
        if (a < 0) a = 0;
        if (a > 255) a = 255;
        alpha = (uint8_t)a;
      } else {
        return ENC_SYM_TERROR;
      }
    }

    // Maybe check if param is in ranges first ?
    res = color_allocate(t, color1, color2, (uint16_t)param1, (uint16_t)param2, mirrored, alpha);
  }

  return res;
}

static lbm_value ext_color_set(lbm_value *args, lbm_uint argn) {
  color_t *color;
  if (argn != 3 || !(color = get_color(args[0])) || // color assignment
      !lbm_is_symbol(args[1])) {
    return ENC_SYM_TERROR;
  }

  bool is_regular = color->type == COLOR_REGULAR;
  bool is_gradient = color->type == COLOR_GRADIENT_X || color->type == COLOR_GRADIENT_Y;
  bool is_pre = color->type == COLOR_PRE_X || color->type == COLOR_PRE_Y;

  lbm_uint prop = lbm_dec_sym(args[1]);
  if (prop == symbol_color_0) {
    if (!lbm_is_number(args[2]) || !(is_regular || is_gradient)) {
      return ENC_SYM_TERROR;
    }
    color->color1 = lbm_dec_as_i32(args[2]);
  } else if (prop == symbol_color_1) {
    if (!lbm_is_number(args[2]) || !is_gradient) {
      return ENC_SYM_TERROR;
    }
    color->color2 = lbm_dec_as_i32(args[2]);
  } else if (prop == symbol_width) {
    if (!lbm_is_number(args[2]) || !is_gradient) {
      return ENC_SYM_TERROR;
    }
    color->param1 = (uint16_t)lbm_dec_as_u32(args[2]);
  } else if (prop == symbol_offset) {
    if (!lbm_is_number(args[2]) || !(is_gradient || is_pre)) {
      return ENC_SYM_TERROR;
    }
    color->param2 = (uint16_t)lbm_dec_as_u32(args[2]);
  } else if (prop == symbol_repeat_type) {
    if (!lbm_is_symbol(args[2]) || !(is_gradient || is_pre)) {
      return ENC_SYM_TERROR;
    }
    lbm_uint sym = lbm_dec_sym(args[2]);
    if (sym == symbol_repeat) {
      color->mirrored = false;
    } else if (sym == symbol_mirrored) {
      color->mirrored = true;
    } else {
      return ENC_SYM_TERROR;
    }
  } else if (prop == symbol_alpha) {
    if (!lbm_is_number(args[2])) {
      return ENC_SYM_TERROR;
    }
    int32_t a = lbm_dec_as_i32(args[2]);
    if (a < 0) a = 0;
    if (a > 255) a = 255;
    color->alpha = (uint8_t)a;
  } else {
    return ENC_SYM_TERROR;
  }

  return ENC_SYM_TRUE;
}

static lbm_value ext_color_get(lbm_value *args, lbm_uint argn) {
  color_t *color;
  if (argn != 2 || !(color = get_color(args[0])) || // color assignment
      !lbm_is_symbol(args[1])) {
    return ENC_SYM_TERROR;
  }

  bool is_gradient = color->type == COLOR_GRADIENT_X || color->type == COLOR_GRADIENT_Y;
  bool is_pre = color->type == COLOR_PRE_X || color->type == COLOR_PRE_Y;

  lbm_uint prop = lbm_dec_sym(args[1]);
  if (prop == symbol_color_0) {
    // always allowed
    return lbm_enc_u32((uint32_t)color->color1);
  } else if (prop == symbol_color_1) {
    if (!is_gradient && !is_pre) {
      return ENC_SYM_TERROR;
    }
    return lbm_enc_u32((uint32_t)color->color2);
  } else if (prop == symbol_width) {
    if (!is_gradient && !is_pre) {
      return ENC_SYM_TERROR;
    }
    return lbm_enc_i32((int32_t)color->param1);
  } else if (prop == symbol_offset) {
    if (!is_gradient && !is_pre) {
      return ENC_SYM_TERROR;
    }
    return lbm_enc_i32((int32_t)color->param2);
  } else if (prop == symbol_repeat_type) {
    if (!is_gradient && !is_pre) {
      return ENC_SYM_TERROR;
    }
    return lbm_enc_sym(color->mirrored ? symbol_mirrored : symbol_repeat);
  } else if (prop == symbol_alpha) {
    return lbm_enc_u32((uint32_t)color->alpha);
  } else {
    return ENC_SYM_TERROR;
  }

  return ENC_SYM_TRUE;
}

static lbm_value ext_color_setpre(lbm_value *args, lbm_uint argn) {
  color_t *color;
  if (argn != 3 || !(color = get_color(args[0])) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2])) {
    return ENC_SYM_TERROR;
  }

  uint32_t pos = lbm_dec_as_u32(args[1]);
  int new_color = lbm_dec_as_i32(args[2]);

  if (color->precalc == 0 || pos >= COLOR_PRECALC_LEN) {
    return ENC_SYM_EERROR;
  }

  color->precalc[pos] = (uint32_t)new_color;

  return ENC_SYM_TRUE;
}

static lbm_value ext_color_getpre(lbm_value *args, lbm_uint argn) {
  color_t *color;
  if (argn != 2 || !(color = get_color(args[0])) ||
      !lbm_is_number(args[1])) {
    return ENC_SYM_TERROR;
  }
  uint32_t pos = lbm_dec_as_u32(args[1]);

  if (color->precalc == 0 || pos >= COLOR_PRECALC_LEN) {
    return ENC_SYM_EERROR;
  }

  return lbm_enc_u32(color->precalc[pos]);
}

static lbm_value ext_clear(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  lbm_array_header_t *arr;
  if ((argn == 1 || argn == 2) &&
      (arr = get_image_buffer(args[0])) &&   // assignment
      (argn != 2 || lbm_is_number(args[1]))) { // ( argn == 2 -> lbm_is_number(args[1]))
    image_buffer_t img_buf;
    img_buf.width = image_buffer_width((uint8_t*)arr->data);
    img_buf.height = image_buffer_height((uint8_t*)arr->data);
    img_buf.fmt = image_buffer_format((uint8_t*)arr->data);
    img_buf.mem_base = (uint8_t*)arr->data;
    img_buf.data = image_buffer_data((uint8_t*)arr->data);

    uint32_t color = 0;
    if (argn == 2) {
      color = lbm_dec_as_u32(args[1]);
    }

    image_buffer_clear(&img_buf, color);
    res = ENC_SYM_TRUE;
  }
  return res;
}

static lbm_value ext_putpixel(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 3);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  putpixel(&arg_dec.img,
           lbm_dec_as_i32(arg_dec.args[0]),
           lbm_dec_as_i32(arg_dec.args[1]),
           lbm_dec_as_u32(arg_dec.args[2]),
           arg_dec.alpha);
  return ENC_SYM_TRUE;
}

static lbm_value ext_getpixel(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 2);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  uint32_t c = getpixel(&arg_dec.img,
                        lbm_dec_as_i32(arg_dec.args[0]),
                        lbm_dec_as_i32(arg_dec.args[1]));
  return lbm_enc_u32(c);
}

// lisp args: img x1 y1 x2 y2 color opt-attr1 ... opt-attrN
static lbm_value ext_line(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 5);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  tinygfx_line(&arg_dec.img,
       lbm_dec_as_i32(arg_dec.args[0]),
       lbm_dec_as_i32(arg_dec.args[1]),
       lbm_dec_as_i32(arg_dec.args[2]),
       lbm_dec_as_i32(arg_dec.args[3]),
       lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
       lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
       lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
       lbm_dec_as_u32(arg_dec.args[4]),
       arg_dec.alpha);

  return ENC_SYM_TRUE;
}

// Shared by ext_arc/ext_circle_sector/ext_circle_segment, which decode
// identical attribute sets and only differ in sector/segment and which
// args[] slot holds the color.
static arc_params_t make_arc_params(const img_args_t *a, bool rounded, bool sector, bool segment, lbm_value color_arg) {
  arc_params_t p = {
    .thickness  = lbm_dec_as_i32(a->attr_thickness.args[0]),
    .rounded    = rounded,
    .filled     = a->attr_filled.is_valid,
    .sector     = sector,
    .segment    = segment,
    .dot1       = lbm_dec_as_i32(a->attr_dotted.args[0]),
    .dot2       = lbm_dec_as_i32(a->attr_dotted.args[1]),
    .resolution = lbm_dec_as_i32(a->attr_resolution.args[0]),
    .color      = lbm_dec_as_u32(color_arg),
    .alpha      = a->alpha,
  };
  return p;
}

// lisp args: img cx cy r color opt-attr1 ... opt-attrN
static lbm_value ext_circle(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 4);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  if (arg_dec.attr_filled.is_valid) {
    tinygfx_fill_circle(&arg_dec.img,
                lbm_dec_as_i32(arg_dec.args[0]),
                lbm_dec_as_i32(arg_dec.args[1]),
                lbm_dec_as_i32(arg_dec.args[2]),
                lbm_dec_as_u32(arg_dec.args[3]),
                arg_dec.alpha);
  } else if (arg_dec.attr_dotted.is_valid) {
    // rounded here currently does nothing as the line function doesn't
    // support square ends.
    arc_params_t p = make_arc_params(&arg_dec, arg_dec.attr_rounded.is_valid, false, false, arg_dec.args[3]);
    tinygfx_arc(&arg_dec.img,
        lbm_dec_as_i32(arg_dec.args[0]),
        lbm_dec_as_i32(arg_dec.args[1]),
        lbm_dec_as_i32(arg_dec.args[2]),
        0, 359.9f,
        &p);
  } else {
    tinygfx_circle(&arg_dec.img,
           lbm_dec_as_i32(arg_dec.args[0]),
           lbm_dec_as_i32(arg_dec.args[1]),
           lbm_dec_as_i32(arg_dec.args[2]),
           lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
           lbm_dec_as_u32(arg_dec.args[3]),
           arg_dec.alpha);
  }

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r ang-s ang-e color opt-attr1 ... opt-attrN
static lbm_value ext_arc(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 6);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  arc_params_t p = make_arc_params(&arg_dec, arg_dec.attr_rounded.is_valid, false, false, arg_dec.args[5]);
  tinygfx_arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      &p);

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r ang-s ang-e color opt-attr1 ... opt-attrN
static lbm_value ext_circle_sector(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 6);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  arc_params_t p = make_arc_params(&arg_dec, true, true, false, arg_dec.args[5]);
  tinygfx_arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      &p);

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r ang-s ang-e color opt-attr1 ... opt-attrN
static lbm_value ext_circle_segment(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 6);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  arc_params_t p = make_arc_params(&arg_dec, true, false, true, arg_dec.args[5]);
  tinygfx_arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      &p);


  return ENC_SYM_TRUE;
}

// lisp args: img x y width height color opt-attr1 ... opt-attrN
static lbm_value ext_rectangle(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 5);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  image_buffer_t *img = &arg_dec.img;
  int x = lbm_dec_as_i32(arg_dec.args[0]);
  int y = lbm_dec_as_i32(arg_dec.args[1]);
  int width = lbm_dec_as_i32(arg_dec.args[2]);
  int height = lbm_dec_as_i32(arg_dec.args[3]);
  int rad = lbm_dec_as_i32(arg_dec.attr_rounded.args[0]);
  int thickness = lbm_dec_as_i32(arg_dec.attr_thickness.args[0]);
  uint32_t color = lbm_dec_as_u32(arg_dec.args[4]);
  int dot1 = lbm_dec_as_i32(arg_dec.attr_dotted.args[0]);
  int dot2 = lbm_dec_as_i32(arg_dec.attr_dotted.args[1]);
  int resolution = lbm_dec_as_i32(arg_dec.attr_resolution.args[0]);

  if (arg_dec.attr_rounded.is_valid) {
    if (arg_dec.attr_filled.is_valid) {
      tinygfx_fill_rounded_rectangle(img, x, y, width, height, rad, color, arg_dec.alpha);
    } else {
      // Remember to change these to use the rounded attribute,
      // when/if line supports it!

      int line_thickness = thickness / 2;
      thickness = line_thickness * 2; // round it to even for consistency.

      // top
      tinygfx_line(img, x + rad, y + line_thickness, x + width - rad, y + line_thickness, line_thickness, dot1, dot2, color, arg_dec.alpha);
      // bottom
      tinygfx_line(img, x + rad, y + height - line_thickness, x + width - rad, y + height - line_thickness, line_thickness, dot1, dot2, color, arg_dec.alpha);
      // left
      tinygfx_line(img, x + line_thickness, y + rad, x + line_thickness, y + height - rad, line_thickness, dot1, dot2, color, arg_dec.alpha);
      // right
      tinygfx_line(img, x + width - line_thickness, y + rad, x + width - line_thickness, y + height - rad, line_thickness, dot1, dot2, color, arg_dec.alpha);

      arc_params_t p = {
        .thickness = thickness, .rounded = false, .filled = false, .sector = false, .segment = false,
        .dot1 = dot1, .dot2 = dot2, .resolution = resolution, .color = color, .alpha = arg_dec.alpha,
      };
      // upper left
      tinygfx_arc(img, x + rad, y + rad, rad, 180, 270, &p);
      // upper right
      tinygfx_arc(img, x + width - rad, y + rad, rad, 270, 0, &p);
      // bottom left
      tinygfx_arc(img, x + rad, y + height - rad, rad, 90, 180, &p);
      // bottom right
      tinygfx_arc(img, x + width - rad, y + height - rad, rad, 0, 90, &p);
    }
  } else {
    tinygfx_rectangle(img,
              x, y,
              width, height,
              arg_dec.attr_filled.is_valid,
              thickness,
              dot1, dot2,
              color, arg_dec.alpha);
  }

  return ENC_SYM_TRUE;
}

// lisp args: img x1 y1 x2 y2 x3 y3 color opt-attr1 ... opt-attrN
static lbm_value ext_triangle(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 7);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  image_buffer_t *img = &arg_dec.img;
  int x0 = lbm_dec_as_i32(arg_dec.args[0]);
  int y0 = lbm_dec_as_i32(arg_dec.args[1]);
  int x1 = lbm_dec_as_i32(arg_dec.args[2]);
  int y1 = lbm_dec_as_i32(arg_dec.args[3]);
  int x2 = lbm_dec_as_i32(arg_dec.args[4]);
  int y2 = lbm_dec_as_i32(arg_dec.args[5]);
  int thickness = lbm_dec_as_i32(arg_dec.attr_thickness.args[0]);
  int dot1 = lbm_dec_as_i32(arg_dec.attr_dotted.args[0]);
  int dot2 = lbm_dec_as_i32(arg_dec.attr_dotted.args[1]);
  uint32_t color = lbm_dec_as_u32(arg_dec.args[6]);

  if (arg_dec.attr_filled.is_valid) {
    tinygfx_fill_triangle(img, x0, y0, x1, y1, x2, y2, color, arg_dec.alpha);
  } else {
    tinygfx_line(img, x0, y0, x1, y1, thickness, dot1, dot2, color, arg_dec.alpha);
    tinygfx_line(img, x1, y1, x2, y2, thickness, dot1, dot2, color, arg_dec.alpha);
    tinygfx_line(img, x2, y2, x0, y0, thickness, dot1, dot2, color, arg_dec.alpha);
  }

  return ENC_SYM_TRUE;
}

// lisp args:
//   img x y fg bg font str  [attrs] ['up|'down]
//   img x y fg bg str       [attrs] ['up|'down]   uses built-in retro5x7 font
//   img x y '(c0..c3) font str [attrs] ['up|'down]  4-color form for 2bpp fonts
// attrs: '(magnify N) '(scale N) '(spacing N) '(align 'left|'center|'right) '(rotate deg)
// orient: 0=normal 1=up/90CCW 2=180 3=down/90CW
static lbm_value ext_text(lbm_value *args, lbm_uint argn) {
  int orient = 0;

  if (argn >= 6 && lbm_is_symbol(args[argn - 1])) {
    lbm_uint sym = lbm_dec_sym(args[argn - 1]);
    if (sym == symbol_up)   { orient = 1; argn--; }
    if (sym == symbol_down) { orient = 3; argn--; }
  }

  if (argn < 6) return ENC_SYM_TERROR;

  float txt_mag = 1.0f;
  int spacing = 0;
  int align   = 0;
  int rot_deg = 0;

  lbm_uint core_argn = argn;
  while (core_argn > 0) {
    int r = parse_text_attr(args[core_argn - 1], &txt_mag, &spacing, &align, &rot_deg);
    if (r == 1) { core_argn--; continue; }
    if (r < 0)  { return ENC_SYM_TERROR; }
    break;
  }

  if (core_argn < 6 || core_argn > 7) return ENC_SYM_TERROR;

  if (rot_deg != 0) {
    orient = (((rot_deg % 360) + 360) % 360) / 90;
  }

  int x = lbm_dec_as_i32(args[1]);
  int y = lbm_dec_as_i32(args[2]);

  int32_t colors[4] = {-1, -1, -1, -1};
  uint8_t *font_data = (uint8_t*)retro5x7;
  char *txt = NULL;

  if (lbm_is_cons(args[3])) {
    // color list form: img x y '(c0..cN) font str
    if (core_argn != 6) return ENC_SYM_TERROR;
    lbm_value curr = args[3];
    int ci = 0;
    while (lbm_is_cons(curr) && ci < 4) {
      lbm_value a = lbm_car(curr);
      if (!lbm_is_number(a)) return ENC_SYM_TERROR;
      colors[ci++] = lbm_dec_as_i32(a);
      curr = lbm_cdr(curr);
    }
    if (lbm_type_of_functional(args[4]) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *fh = (lbm_array_header_t*)lbm_car(args[4]);
      if (fh->size < 4) return ENC_SYM_TERROR;
      uint8_t *fd = (uint8_t*)fh->data;
      uint32_t need = ((uint32_t)fd[0] * fd[1] * fd[2] * fd[3] + 7) / 8;
      if (fh->size - 4 < need) return ENC_SYM_TERROR;
      font_data = fd;
    }
    txt = lbm_dec_str(args[5]);
  } else if (lbm_is_number(args[3]) && lbm_is_number(args[4])) {
    colors[0] = lbm_dec_as_i32(args[3]);
    colors[1] = lbm_dec_as_i32(args[4]);
    if (core_argn == 7) {
      if (lbm_type_of_functional(args[5]) == LBM_TYPE_ARRAY) {
        lbm_array_header_t *fh = (lbm_array_header_t*)lbm_car(args[5]);
        if (fh->size < 4) return ENC_SYM_TERROR;
        uint8_t *fd = (uint8_t*)fh->data;
        uint32_t need = ((uint32_t)fd[0] * fd[1] * fd[2] * fd[3] + 7) / 8;
        if (fh->size - 4 < need) return ENC_SYM_TERROR;
        font_data = fd;
      } else {
        return ENC_SYM_TERROR;
      }
      txt = lbm_dec_str(args[6]);
    } else {
      txt = lbm_dec_str(args[5]);
    }
  } else {
    return ENC_SYM_TERROR;
  }

  if (!txt) return ENC_SYM_TERROR;

  if (!array_is_image_buffer(args[0])) return ENC_SYM_TERROR;
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
  image_buffer_t img_buf;
  img_buf.width    = image_buffer_width((uint8_t*)arr->data);
  img_buf.height   = image_buffer_height((uint8_t*)arr->data);
  img_buf.fmt      = image_buffer_format((uint8_t*)arr->data);
  img_buf.mem_base = (uint8_t*)arr->data;
  img_buf.data     = image_buffer_data((uint8_t*)arr->data);

  uint8_t w = font_data[0];

  static const int8_t incx_tbl[4] = { 1,  0, -1,  0};
  static const int8_t incy_tbl[4] = { 0, -1,  0,  1};
  int incx = incx_tbl[orient];
  int incy = incy_tbl[orient];

  float char_step_f = (float)w * txt_mag + (float)spacing;
  int char_step = (int)lroundf(char_step_f);
  int txt_len      = (int)strlen(txt);
  int total        = (int)lroundf((float)txt_len * char_step_f - (float)spacing);
  int align_offset = (align == 1) ? total / 2 : (align == 2) ? total : 0;

  int x0 = x - align_offset * incx;
  int y0 = y - align_offset * incy;

  for (int ind = 0; txt[ind] != 0; ind++) {
    tinygfx_img_putc(&img_buf,
      x0 + ind * char_step * incx,
      y0 + ind * char_step * incy,
      (uint32_t *)colors,
      4,
      font_data,
      (uint8_t)txt[ind],
      orient,
      txt_mag);
  }

  return ENC_SYM_TRUE;
}

static lbm_value ext_blit(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args + 1, argn - 1, 3);

  lbm_value res = ENC_SYM_TERROR;
  lbm_array_header_t *arr;
  if (arg_dec.is_valid && (arr = get_image_buffer(args[0]))) { //assignment
    image_buffer_t dest_buf;
    dest_buf.width = image_buffer_width((uint8_t*)arr->data);
    dest_buf.height = image_buffer_height((uint8_t*)arr->data);
    dest_buf.fmt = image_buffer_format((uint8_t*)arr->data);
    dest_buf.mem_base = (uint8_t*)arr->data;
    dest_buf.data = image_buffer_data((uint8_t*)arr->data);

    float scale = 1.0;
    if (arg_dec.attr_scale.is_valid) {
      scale = lbm_dec_as_float(arg_dec.attr_scale.args[0]);
    }

    blit(
        &dest_buf,
        &arg_dec.img,
        lbm_dec_as_i32(arg_dec.args[0]),
        lbm_dec_as_i32(arg_dec.args[1]),
        lbm_dec_as_float(arg_dec.attr_rotate.args[0]),
        lbm_dec_as_float(arg_dec.attr_rotate.args[1]),
        lbm_dec_as_float(arg_dec.attr_rotate.args[2]),
        scale,
        lbm_dec_as_i32(arg_dec.args[2]),
        arg_dec.attr_tile.is_valid,
        arg_dec.attr_clip.is_valid ? lbm_dec_as_i32(arg_dec.attr_clip.args[0]) : 0,
        arg_dec.attr_clip.is_valid ? lbm_dec_as_i32(arg_dec.attr_clip.args[1]) : 0,
        arg_dec.attr_clip.is_valid ? lbm_dec_as_i32(arg_dec.attr_clip.args[2]) : dest_buf.width,
        arg_dec.attr_clip.is_valid ? lbm_dec_as_i32(arg_dec.attr_clip.args[3]) : dest_buf.height
    );
    res = ENC_SYM_TRUE;
  }
  return res;
}

void display_dummy_reset(void) {
  return;
}

void display_dummy_clear(uint32_t color) {
  (void) color;
  return;
}

bool display_dummy_render_image(image_buffer_t *img, uint16_t x, uint16_t y,  color_t *colors) {
  (void) img;
  (void) x;
  (void) y;
  (void) colors;
  return false;
}

static bool(* volatile disp_render_image)(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors) = display_dummy_render_image;
static void(* volatile disp_clear)(uint32_t color) = display_dummy_clear;
static void(* volatile disp_reset)(void) = display_dummy_reset;

static char *msg_not_supported = "Command not supported or display driver not initialized";

static lbm_value ext_disp_reset(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;

  if (disp_reset == NULL) {
    lbm_set_error_reason(msg_not_supported);
    return ENC_SYM_EERROR;
  }

  disp_reset();

  return ENC_SYM_TRUE;
}

static lbm_value ext_disp_clear(lbm_value *args, lbm_uint argn) {
  if (disp_clear == NULL) {
    lbm_set_error_reason(msg_not_supported);
    return ENC_SYM_EERROR;
  }

  if (argn > 1) {
    return ENC_SYM_TERROR;
  }

  uint32_t clear_color = 0;

  if (argn == 1) {
    if (!lbm_is_number(args[0])) {
      return ENC_SYM_TERROR;
    }

    clear_color = lbm_dec_as_u32(args[0]);
  }

  disp_clear(clear_color);

  return ENC_SYM_TRUE;
}

static lbm_value ext_disp_render(lbm_value *args, lbm_uint argn) {
  if (disp_render_image == NULL) {
    lbm_set_error_reason(msg_not_supported);
    return ENC_SYM_EERROR;
  }

  lbm_value res = ENC_SYM_TERROR;
  lbm_array_header_t *arr;
  if ((argn == 3 || argn == 4) &&
      (arr = get_image_buffer(args[0])) &&
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2])) {
    image_buffer_t img_buf;
    img_buf.fmt = image_buffer_format((uint8_t*)arr->data);
    img_buf.width = image_buffer_width((uint8_t*)arr->data);
    img_buf.height = image_buffer_height((uint8_t*)arr->data);
    img_buf.mem_base = (uint8_t*)arr->data;
    img_buf.data = image_buffer_data((uint8_t*)arr->data);

    color_t colors[16];
    memset(colors, 0, sizeof(color_t) * 16);

    if (argn == 4 && lbm_is_list(args[3])) {
      int i = 0;
      lbm_value curr = args[3];
      while (lbm_is_cons(curr) && i < 16) {
        lbm_value arg = lbm_car(curr);
        color_t *color;
        if (lbm_is_number(arg)) {
          colors[i].color1 = (int)lbm_dec_as_u32(arg);
        } else if ((color = get_color(arg))) { // color assignment
          colors[i] = *color;
        } else {
          return ENC_SYM_TERROR;
        }

        curr = lbm_cdr(curr);
        i++;
      }
    }

    // img_buf is a stack allocated image_buffer_t.
    bool render_res = disp_render_image(&img_buf, (uint16_t)lbm_dec_as_u32(args[1]), (uint16_t)lbm_dec_as_u32(args[2]), colors);
    if (!render_res) {
      lbm_set_error_reason("Could not render image. Check if the format and location is compatible with the display.");
      return ENC_SYM_EERROR;
    }
    res = ENC_SYM_TRUE;
  }
  return res;
}

// Jpg decoder

typedef struct {
  uint8_t *data;
  int pos;
  int size;
  int ofs_x;
  int ofs_y;
} jpg_bufdef;

size_t jpg_input_func (JDEC* jd, uint8_t* buff, size_t ndata) {
  jpg_bufdef *dev = (jpg_bufdef*)jd->device;

  if ((int)ndata > (dev->size - dev->pos)) {
    ndata = (size_t)(dev->size - dev->pos);
  }

  if (buff) {
    memcpy(buff, dev->data + dev->pos, ndata);
  }
  dev->pos += (int)ndata;
  return ndata;
}

int jpg_output_func (	/* 1:Ok, 0:Aborted */
                     JDEC* jd,		/* Decompression object */
                     void* bitmap,	/* Bitmap data to be output */
                     JRECT* rect		/* Rectangular region to output */
                        ) {
  jpg_bufdef *dev = (jpg_bufdef*)jd->device;

  image_buffer_t img;
  img.mem_base = (uint8_t*)bitmap;
  img.data = (uint8_t*)bitmap;
  img.width = (uint16_t)(rect->right - rect->left + 1);
  img.height = (uint16_t)(rect->bottom - rect->top + 1);
  img.fmt = rgb888;

  disp_render_image(&img, (uint16_t)(rect->left + dev->ofs_x), (uint16_t)(rect->top + dev->ofs_y), 0);

  return 1;
}

static lbm_value ext_disp_render_jpg(lbm_value *args, lbm_uint argn) {

  lbm_array_header_t *array;
  lbm_value res = ENC_SYM_TERROR;

  if (argn == 3 &&
      (array = lbm_dec_array_r(args[0])) && //asignment
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2])) {

    JDEC jd;
    void *jdwork;
    // make a bit of room before the buffer.
    const size_t sz_work = 4096 + IMAGE_BUFFER_HEADER_SIZE;

    jdwork = lbm_malloc(sz_work);
    if (!jdwork) {
      return ENC_SYM_MERROR;
    }



    jpg_bufdef iodev;
    iodev.data = (uint8_t*)(array->data);
    iodev.size = (int)array->size;
    iodev.pos = 0;
    iodev.ofs_x = lbm_dec_as_i32(args[1]);
    iodev.ofs_y = lbm_dec_as_i32(args[2]);
    jd_prepare(&jd, jpg_input_func, jdwork, sz_work, &iodev);
    jd_decomp(&jd, jpg_output_func, 0);
    lbm_free(jdwork);
    res = ENC_SYM_TRUE;
  }
  return res;
}

void lbm_display_extensions_init(void) {
  register_symbols();

  disp_render_image = NULL;
  disp_clear = NULL;
  disp_reset = NULL;

  lbm_add_extension("img-buffer", ext_image_buffer);
  lbm_add_extension("img-buffer?", ext_is_image_buffer);
  lbm_add_extension("img-color", ext_color);
  lbm_add_extension("img-color-set", ext_color_set);
  lbm_add_extension("img-color-get", ext_color_get);
  lbm_add_extension("img-color-setpre", ext_color_setpre);
  lbm_add_extension("img-color-getpre", ext_color_getpre);
  lbm_add_extension("img-dims", ext_image_dims);
  lbm_add_extension("img-setpix", ext_putpixel);
  lbm_add_extension("img-getpix", ext_getpixel);
  lbm_add_extension("img-line", ext_line);
  lbm_add_extension("img-text", ext_text);
  lbm_add_extension("img-clear", ext_clear);
  lbm_add_extension("img-circle", ext_circle);
  lbm_add_extension("img-arc", ext_arc);
  lbm_add_extension("img-circle-sector", ext_circle_sector);
  lbm_add_extension("img-circle-segment", ext_circle_segment);
  lbm_add_extension("img-rectangle", ext_rectangle);
  lbm_add_extension("img-triangle", ext_triangle);
  lbm_add_extension("img-blit", ext_blit);

  lbm_add_extension("disp-reset", ext_disp_reset);
  lbm_add_extension("disp-clear", ext_disp_clear);
  lbm_add_extension("disp-render", ext_disp_render);
  lbm_add_extension("disp-render-jpg", ext_disp_render_jpg);
}

void lbm_display_extensions_set_callbacks(
                                          bool(* volatile render_image)(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors),
                                          void(* volatile clear)(uint32_t color),
                                          void(* volatile reset)(void)
                                          ) {
  disp_render_image = render_image ? render_image : display_dummy_render_image;
  disp_clear = clear ? clear : display_dummy_clear;
  disp_reset = reset ? reset : display_dummy_reset;
}
