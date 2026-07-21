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
#include <lbm_cos_table.h>

#ifdef LBM_OPT_DISPLAY_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_DISPLAY_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif

#define MAX_WIDTH 32000
#define MAX_HEIGHT 32000

uint32_t lbm_display_rgb888_from_color(color_t color, int x, int y) {
  switch (color.type) {
  case COLOR_REGULAR:
    return (uint32_t)color.color1;

  case COLOR_GRADIENT_X:
  case COLOR_GRADIENT_Y: {
    uint32_t res;
    uint32_t r1 = (uint32_t)color.color1 >> 16;
    uint32_t g1 = (uint32_t)color.color1 >> 8 & 0xFF;
    uint32_t b1 = (uint32_t)color.color1 & 0xff;

    uint32_t r2 = (uint32_t)color.color2 >> 16;
    uint32_t g2 = (uint32_t)color.color2 >> 8 & 0xFF;
    uint32_t b2 = (uint32_t)color.color2 & 0xff;

    int used_len = color.mirrored ? 256 : 128;

    int pos = color.type == COLOR_GRADIENT_X ? x : y;
    // int tab_pos = ((pos * 256) / color.param1 + color.param2) % 256;
    int tab_pos = (((pos - color.param2) * 256) / color.param1 / 2) % used_len;
    if (tab_pos < 0) {
      tab_pos += used_len;
    }

    uint32_t tab_val = (uint32_t)lbm_cos_tab_128[tab_pos <= 127 ? tab_pos : 128 - (tab_pos - 127)];

    uint32_t r = (r1 * tab_val + r2 * (255 - tab_val)) / 255;
    uint32_t g = (g1 * tab_val + g2 * (255 - tab_val)) / 255;
    uint32_t b = (b1 * tab_val + b2 * (255 - tab_val)) / 255;

    res = r << 16 | g << 8 | b;
    return res;
  }

  default:
    return 0;
  }
}

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

uint32_t image_dims_to_size_bytes(color_format_t fmt, uint16_t width, uint16_t height) {
  uint32_t num_pix = (uint32_t)width * (uint32_t)height;
  switch(fmt) {
  case indexed2:
    if (num_pix % 8 != 0) return (num_pix / 8) + 1;
    else return (num_pix / 8);
    break;
  case indexed4:
    if (num_pix % 4 != 0) return (num_pix / 4) + 1;
    else return (num_pix / 4);
    break;
  case indexed16: // Two pixels per byte
    if (num_pix % 2 != 0) return (num_pix / 2) + 1;
    else return (num_pix / 2);
  case rgb332:
    return num_pix;
    break;
  case rgb565:
    return num_pix * 2;
    break;
  case rgb888:
    return num_pix * 3;
  default:
    return 0;
  }
}

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

// Internal functions
static inline void swap_points(int *x0, int *y0, int *x1, int *y1) {
  int tx = *x0, ty = *y0;
  *x0 = *x1; *y0 = *y1;
  *x1 = tx;  *y1 = ty;
}

// Geometry utility functions

static inline void norm_angle(float *angle) {
  while (*angle < -M_PI) { *angle += 2.0f * (float)M_PI; }
  while (*angle >=  M_PI) { *angle -= 2.0f * (float)M_PI; }
}

static inline void norm_angle_0_2pi(float *angle) {
  while (*angle < 0) { *angle += 2.0f * (float)M_PI; }
  while (*angle >= 2.0 * M_PI) { *angle -= 2.0f * (float)M_PI; }
}

static uint8_t rgb888to332(uint32_t rgb) {
  uint8_t r = (uint8_t)(rgb >> (16 + 5));
  uint8_t g = (uint8_t)(rgb >> (8 + 5));
  uint8_t b = (uint8_t)(rgb >> 6);
  r = (uint8_t)((r & 0x7) << 5);
  g = (uint8_t)((g & 0x7) << 2);
  b = (b & 0x3);
  uint8_t res_rgb332 = r | g | b;
  return res_rgb332;
}

static uint16_t rgb888to565(uint32_t rgb) {
  uint16_t r = (uint16_t)(rgb >> (16 + 3));
  uint16_t g = (uint16_t)(rgb >> (8 + 2));
  uint16_t b = (uint16_t)(rgb >> 3);
  r = (uint16_t)(r << 11);
  g = (uint16_t)((g & 0x3F) << 5);
  b = (b & 0x1F);
  uint16_t res_rgb565 = r | g | b;
  return res_rgb565;
}

// One problem with rgb332 is that
// if you take 3 most significant bits of 255 you get 7.
// There is no whole number that you can multiply 7 with to get 255.
// This is fundamental for any conversion from RGB888 that just uses the
// N < 8 most significant bits. And it means that conversion to this format
// and then back to rgb888 will not (without tricks) map highest intensity
// back to highest intensity.
//
// Another issue is that 2 bits (the blue channel) yields steps of 85 (255 / 3)
// while 3 bits yields steps of 36.4 (255 / 7)
//
// 36.4 72.8 109.3 145.7 182.1 218.6 254.99
//         85          170               255
//
// The multiples of 85 never coincide with the multiples of 36.4 except
// for at 0 and 255
static uint32_t rgb332to888(uint8_t rgb) {
  uint32_t r = (uint32_t)((rgb>>5) & 0x7);
  uint32_t g = (uint32_t)((rgb>>2) & 0x7);

  // turn 2 bits into 3 having value 0 3 5 or 7
  // so that 4 points match up when doing greyscale.
  uint32_t b = (uint32_t)(rgb & 0x3);

  b = (b > 0) ? (2 * b) + 1 : 0;
  r = (r == 7) ? 255 : 36 * r; // 36 is an approximation (36.4)
  g = (g == 7) ? 255 : 36 * g;
  b = (b == 7) ? 255 : 36 * b;
  uint32_t res_rgb888 = r << 16 | g << 8 | b;
  return res_rgb888;
}

// RGB 565
// 2^5 = 32
// 2^6 = 64
// 255 / 31 = 8.226
// 255 / 63 = 4.18
//         0   1     2     3     4     5     6     7     8       ...  31   63
// 5 bits  0   8.226 16.45 24.67 32.9  41.13 49.35 57.58 65.81   ...  254.9
// 6 bits  0   4.047 8.09  12.14 16.19 20.24 24.29 28.33 32.38      ...    254.9
//
// For RGB 565 the 6 and 5 bit channels match up very nicely such
// index i in the 5 bit channel is equal to index (2 * i) in the 6 bit channel.
// RGB 565 will have nice grayscales.

static uint32_t  rgb565to888(uint16_t rgb) {
  uint32_t r = (uint32_t)(rgb >> 11);
  uint32_t g = (uint32_t)((rgb >> 5) & 0x3F);
  uint32_t b = (uint32_t)(rgb & 0x1F);
  uint32_t res_rgb888 = r << (16 + 3) | g << (8 + 2) | b << 3;
  return res_rgb888;
}

// Integer Div by 255 which is correct for values 0 <= x <= 65025
// (* 255 255) => 65025
static inline uint32_t div255(uint32_t x) {
  return ((x + 128 + ((x + 128) >> 8)) >> 8);
}

static uint32_t alpha_blend_rgb888(uint32_t src, uint32_t dst, uint8_t alpha) {
  if (alpha == 255) {
    return src;
  }
  if (alpha == 0) {
    return dst;
  }

  uint32_t sr = (src >> 16) & 0xFF;
  uint32_t sg = (src >> 8) & 0xFF;
  uint32_t sb = src & 0xFF;

  uint32_t dr = (dst >> 16) & 0xFF;
  uint32_t dg = (dst >> 8) & 0xFF;
  uint32_t db = dst & 0xFF;

  uint32_t inverse_alpha = 255u - alpha;
  uint32_t r = div255(sr * alpha + dr * inverse_alpha);
  uint32_t g = div255(sg * alpha + dg * inverse_alpha);
  uint32_t b = div255(sb * alpha + db * inverse_alpha);

  return (r << 16) | (g << 8) | b;
}

void image_buffer_clear(image_buffer_t *img, uint32_t cc) {
  color_format_t fmt = img->fmt;
  uint32_t w = img->width;
  uint32_t h = img->height;
  uint32_t img_size = w * h;
  uint8_t *data = img->data;
  switch (fmt) {
  case indexed2: {
    uint32_t bytes = (img_size / 8) + (img_size % 8 ? 1 : 0);
    uint8_t c8 = (uint8_t)((cc & 1) ? 0xFF : 0x0);
    memset(data, c8, bytes);
  }
    break;
  case indexed4: {
    static const uint8_t index4_table[4] = {0x00, 0x55, 0xAA, 0xFF};
    uint32_t bytes = (img_size / 4) + (img_size % 4 ? 1 : 0);
    uint8_t ix = (uint8_t)(cc & 0x3);
    memset(data, index4_table[ix], bytes);
  }
    break;
  case indexed16: {
    uint32_t bytes = (img_size / 2) + (img_size % 2 ? 1 : 0);
    uint8_t ix = (uint8_t)(cc & 0xF);
    uint8_t color = (uint8_t)(ix | ix << 4);  // create a color based on duplication of index
    memset(data, color, bytes);
  }
    break;
  case rgb332: {
    memset(data, rgb888to332(cc), img_size);
  }
    break;
  case rgb565: {
    uint16_t c = rgb888to565(cc);
    uint8_t *dp = (uint8_t*)data;
    for (unsigned int i = 0; i < img_size * 2; i +=2) {
      dp[i] = (uint8_t)(c >> 8);
      dp[i+1] = (uint8_t)c;
    }
  }
    break;
  case rgb888: {
    uint8_t *dp = (uint8_t*)data;
    for (unsigned int i = 0; i < img_size * 3; i+= 3) {
      dp[i]   = (uint8_t)(cc >> 16);
      dp[i+1] = (uint8_t)(cc >> 8);
      dp[i+2] = (uint8_t)cc;
    }
  }
    break;
  default:
    break;
  }
}

static const uint8_t indexed4_mask[4] = {0x03, 0x0C, 0x30, 0xC0};
static const uint8_t indexed4_shift[4] = {0, 2, 4, 6};
static const uint8_t indexed16_mask[4] = {0x0F, 0xF0};
static const uint8_t indexed16_shift[4] = {0, 4};


void putpixel(image_buffer_t* img, int x_i, int y_i, uint32_t c, uint8_t alpha) {
  if (alpha == 0) {
    return;
  }
  uint16_t w = img->width;
  uint16_t h = img->height;
  uint16_t x = (uint16_t)x_i; // negative numbers become really large.
  uint16_t y = (uint16_t)y_i;

  if (x < w && y < h) {
    color_format_t fmt = img->fmt;
    uint8_t *data = img->data;
    bool opaque = alpha == 255;

    switch(fmt) {
    case indexed2: {
      uint32_t pos = (uint32_t)y * (uint32_t)w + (uint32_t)x;
      uint32_t byte = pos >> 3;
      uint32_t bit  = 7 - (pos & 0x7);
      if (c) {
        data[byte] |= (uint8_t)(1 << bit);
      } else {
        data[byte] &= (uint8_t)~(1 << bit);
      }
      break;
    }
    case indexed4: {
      uint32_t pos = (uint32_t)y*w + x;
      uint32_t byte = pos >> 2;
      uint32_t ix  = 3 - (pos & 0x3);
      data[byte] = (uint8_t)((uint8_t)(data[byte] & ~indexed4_mask[ix]) | (uint8_t)(c << indexed4_shift[ix]));
      break;
    }
    case indexed16: {
      uint32_t pos = (uint32_t)y*w + x;
      uint32_t byte = pos >> 1;
      uint32_t ix  = 1 - (pos & 0x1);
      data[byte] = (uint8_t)((uint8_t)(data[byte] & ~indexed16_mask[ix]) | (uint8_t)(c << indexed16_shift[ix]));
      break;
    }
    case rgb332: {
      int pos = y*w + x;
      if (opaque) {
        data[pos] = rgb888to332(c);
      } else {
        uint32_t dst = rgb332to888(data[pos]);
        data[pos] = rgb888to332(alpha_blend_rgb888(c, dst, alpha));
      }
      break;
    }
    case rgb565: {
      int pos = y*(w<<1) + (x<<1) ;
      if (opaque) {
        uint16_t color = rgb888to565(c);
        data[pos] = (uint8_t)(color >> 8);
        data[pos+1] = (uint8_t)color;
      } else {
        uint16_t dst = (uint16_t)((data[pos] << 8) | data[pos+1]);
        uint16_t color = rgb888to565(alpha_blend_rgb888(c, rgb565to888(dst), alpha));
        data[pos] = (uint8_t)(color >> 8);
        data[pos+1] = (uint8_t)color;
      }
      break;
    }
    case rgb888: {
      int pos = y*(w*3) + (x*3);
      if (opaque) {
        data[pos] = (uint8_t)(c>>16);
        data[pos+1] = (uint8_t)(c>>8);
        data[pos+2] = (uint8_t)c;
      } else {
        uint32_t dst = ((uint32_t)data[pos] << 16) | ((uint32_t)data[pos+1] << 8) | data[pos+2];
        uint32_t color = alpha_blend_rgb888(c, dst, alpha);
        data[pos] = (uint8_t)(color>>16);
        data[pos+1] = (uint8_t)(color>>8);
        data[pos+2] = (uint8_t)color;
      }
      break;
    }
    default:
      break;
    }
  }
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

uint32_t getpixel(image_buffer_t* img, int x_i, int y_i) {
  uint16_t w = img->width;
  uint16_t h = img->height;
  uint16_t x = (uint16_t)x_i;
  uint16_t y = (uint16_t)y_i;

  if (x < w && y < h) {
    color_format_t fmt = img->fmt;
    uint8_t *data = img->data;
    switch(fmt) {
    case indexed2: {
      uint32_t pos = (uint32_t)y * w + x;
      uint32_t byte = pos >> 3;
      uint32_t bit  = 7 - (pos & 0x7);
      return (uint32_t)(data[byte] >> bit) & 0x1;
    }
    case indexed4: {
      uint32_t pos = (uint32_t)y*w + x;
      uint32_t byte = pos >> 2;
      uint32_t ix  = 3 - (pos & 0x3);
      return (uint32_t)((data[byte] & indexed4_mask[ix]) >> indexed4_shift[ix]);
    }
    case indexed16: {
      uint32_t pos = (uint32_t)y*w + x;
      uint32_t byte = pos >> 1;
      uint32_t ix  = 1 - (pos & 0x1);
      return (uint32_t)((data[byte] & indexed16_mask[ix]) >> indexed16_shift[ix]);
    }
    case rgb332: {
      int pos = y*w + x;
      return rgb332to888(data[pos]);
    }
    case rgb565: {
      int pos = y*(w<<1) + (x<<1);
      uint16_t c = (uint16_t)(((uint16_t)data[pos] << 8) | (uint16_t)data[pos+1]);
      return rgb565to888(c);
    }
    case rgb888: {
      int pos = y*(w*3) + (x*3);
      uint32_t r = data[pos];
      uint32_t g = data[pos+1];
      uint32_t b = data[pos+2];
      return (r << 16 | g << 8 | b);
    }
    default:
      break;
    }
  }
  return 0;
}

#ifdef USE_EFFICIENT_HLINE_VLINE

// Putpixel inlined into h/v_line and code that does not change along the line
// is hoisted out of the loop. Partial alpha still uses putpixel because
// every destination pixel participates in blending.
static void h_line(image_buffer_t* img, int x, int y, int len, uint32_t c, uint8_t alpha) {
  if (alpha == 0 || len <= 0) return;
  if (alpha != 255) {
    for (int i = 0; i < len; i++) {
      putpixel(img, x + i, y, c, alpha);
    }
    return;
  }

  if (y < 0 || y >= img->height) return;
  int x0 = x;
  int x1 = x + len - 1;
  if (x0 < 0) x0 = 0;
  if (x1 >= img->width) x1 = img->width - 1;
  if (x0 > x1) return;
  uint32_t n = (uint32_t)(x1 - x0 + 1);
  uint32_t w = img->width;
  uint8_t *data = img->data;

  switch (img->fmt) {
  case indexed2: {
    uint32_t pos0 = (uint32_t)y * w + (uint32_t)x0;
    for (uint32_t i = 0; i < n; i++) {
      uint32_t pos = pos0 + i;
      uint32_t byte = pos >> 3, bit = 7 - (pos & 7);
      if (c) data[byte] |= (uint8_t)(1 << bit);
      else data[byte] &= (uint8_t)~(1 << bit);
    }
    break;
  }
  case indexed4: {
    uint32_t pos0 = (uint32_t)y * w + (uint32_t)x0;
    for (uint32_t i = 0; i < n; i++) {
      uint32_t pos = pos0 + i;
      uint32_t byte = pos >> 2, ix = 3 - (pos & 0x3);
      data[byte] = (uint8_t)((uint8_t)(data[byte] & ~indexed4_mask[ix]) | (uint8_t)(c << indexed4_shift[ix]));
    }
    break;
  }
  case indexed16: {
    uint32_t pos0 = (uint32_t)y * w + (uint32_t)x0;
    for (uint32_t i = 0; i < n; i++) {
      uint32_t pos = pos0 + i;
      uint32_t byte = pos >> 1, ix = 1 - (pos & 0x1);
      data[byte] = (uint8_t)((uint8_t)(data[byte] & ~indexed16_mask[ix]) | (uint8_t)(c << indexed16_shift[ix]));
    }
    break;
  }
  case rgb332: {
    uint32_t pos = (uint32_t)y * w + (uint32_t)x0;
    memset(data + pos, rgb888to332(c), n);
    break;
  }
  case rgb565: {
    uint16_t color = rgb888to565(c);
    uint8_t hi = (uint8_t)(color >> 8), lo = (uint8_t)color;
    uint32_t pos = (uint32_t)y * (w << 1) + ((uint32_t)x0 << 1);
    uint8_t *dp = data + pos;
    for (uint32_t i = 0; i < n; i++) {
      dp[0] = hi;
      dp[1] = lo;
      dp += 2;
    }
    break;
  }
  case rgb888: {
    uint8_t b0 = (uint8_t)(c >> 16), b1 = (uint8_t)(c >> 8), b2 = (uint8_t)c;
    uint32_t pos = (uint32_t)y * (w * 3) + ((uint32_t)x0 * 3);
    uint8_t *dp = data + pos;
    for (uint32_t i = 0; i < n; i++) {
      dp[0] = b0;
      dp[1] = b1;
      dp[2] = b2;
      dp += 3;
    }
    break;
  }
  default:
    break;
  }
}

static void v_line(image_buffer_t* img, int x, int y, int len, uint32_t c, uint8_t alpha) {
  if (alpha == 0 || len <= 0) return;
  if (alpha != 255) {
    for (int i = 0; i < len; i++) {
      putpixel(img, x, y + i, c, alpha);
    }
    return;
  }

  if (x < 0 || x >= img->width) return;
  int y0 = y;
  int y1 = y + len - 1;
  if (y0 < 0) y0 = 0;
  if (y1 >= img->height) y1 = img->height - 1;
  if (y0 > y1) return;
  uint32_t n = (uint32_t)(y1 - y0 + 1);
  uint32_t w = img->width;
  uint8_t *data = img->data;

  switch (img->fmt) {
  case indexed2: {
    uint32_t pos = (uint32_t)y0 * w + (uint32_t)x;
    for (uint32_t i = 0; i < n; i++) {
      uint32_t byte = pos >> 3, bit = 7 - (pos & 7);
      if (c) data[byte] |= (uint8_t)(1 << bit);
      else data[byte] &= (uint8_t)~(1 << bit);
      pos += w;
    }
    break;
  }
  case indexed4: {
    uint32_t pos = (uint32_t)y0 * w + (uint32_t)x;
    for (uint32_t i = 0; i < n; i++) {
      uint32_t byte = pos >> 2, ix = 3 - (pos & 0x3);
      data[byte] = (uint8_t)((uint8_t)(data[byte] & ~indexed4_mask[ix]) | (uint8_t)(c << indexed4_shift[ix]));
      pos += w;
    }
    break;
  }
  case indexed16: {
    uint32_t pos = (uint32_t)y0 * w + (uint32_t)x;
    for (uint32_t i = 0; i < n; i++) {
      uint32_t byte = pos >> 1, ix = 1 - (pos & 0x1);
      data[byte] = (uint8_t)((uint8_t)(data[byte] & ~indexed16_mask[ix]) | (uint8_t)(c << indexed16_shift[ix]));
      pos += w;
    }
    break;
  }
  case rgb332: {
    uint8_t col = rgb888to332(c);
    uint32_t pos = (uint32_t)y0 * w + (uint32_t)x;
    for (uint32_t i = 0; i < n; i++) {
      data[pos] = col;
      pos += w;
    }
    break;
  }
  case rgb565: {
    uint16_t color = rgb888to565(c);
    uint8_t hi = (uint8_t)(color >> 8), lo = (uint8_t)color;
    uint32_t stride = w << 1;
    uint32_t pos = (uint32_t)y0 * stride + ((uint32_t)x << 1);
    for (uint32_t i = 0; i < n; i++) {
      data[pos] = hi;
      data[pos + 1] = lo;
      pos += stride;
    }
    break;
  }
  case rgb888: {
    uint8_t b0 = (uint8_t)(c >> 16), b1 = (uint8_t)(c >> 8), b2 = (uint8_t)c;
    uint32_t stride = w * 3;
    uint32_t pos = (uint32_t)y0 * stride + ((uint32_t)x * 3);
    for (uint32_t i = 0; i < n; i++) {
      data[pos] = b0;
      data[pos + 1] = b1;
      data[pos + 2] = b2;
      pos += stride;
    }
    break;
  }
  default:
    break;
  }
}
#else
static void h_line(image_buffer_t* img, int x, int y, int len, uint32_t c, uint8_t alpha) {
  for (int i = 0; i < len; i ++) {
    putpixel(img, x+i, y, c, alpha);
  }
}

static void v_line(image_buffer_t* img, int x, int y, int len, uint32_t c, uint8_t alpha) {
  for (int i = 0; i < len; i ++) {
    putpixel(img, x, y+i, c, alpha);
  }
}
#endif // USE_EFFICIENT_HLINE_VLINE

////////////////////////////////////////////////////////////
//  ARCS AND CIRCLES
//
// The Boundary tracking scanline fill circle and arc functions
// in this file have been implemented assisted by Claude code.
// I(Joel) personally did not fully understand the old Arc/Circle
// code and I still dont.

// The boundary test for arcs and circles is ((2x+1)^2+(2y+1)^2<=4r^2
// At least we are moving towards using that boundary test uniformly,
// there may still be cases where other definitions of the "boundary
// of a arc-shape"

#define ARC_BIG 1000000

// Advances *cursor monotonically (only ever forward) until it lands on the
// first x such that pixel-center (x+0.5, row) falls within radius_dbl_sq (a
// squared radius already doubled: (2r)^2). row_dbl_sq is (2*y+1)^2 for row y.
static inline void circle_boundary_advance(int *cursor, int row_dbl_sq, int radius_dbl_sq) {
  while (true) {
    int x_dbl_offs = 2 * (*cursor) + 1;
    if (x_dbl_offs * x_dbl_offs + row_dbl_sq <= radius_dbl_sq) break;
    (*cursor)++;
  }
}

static void arc_ray_clip(double dir_x, double dir_y, int Y, int *lo, int *hi) {
  if (dir_y == 0) {
    if (-(double)Y * dir_x <= 0) { *lo = -ARC_BIG; *hi = ARC_BIG; }
    else { *lo = 1; *hi = 0; }
    return;
  }
  double xc = (double)Y * dir_x / dir_y;
  if (dir_y > 0) { *lo = -ARC_BIG; *hi = (int)floor(xc); }
  else { *lo = (int)ceil(xc); *hi = ARC_BIG; }
}

static void arc_chord_clip(double sx, double sy, double ex, double ey, int Y, int *lo, int *hi) {
  double dx = ex - sx, dy = ey - sy;
  if (dy == 0) {
    if (0.0 <= ((double)Y - sy) * dx) { *lo = -ARC_BIG; *hi = ARC_BIG; }
    else { *lo = 1; *hi = 0; }
    return;
  }
  double rhs = sx * dy + ((double)Y - sy) * dx;
  if (dy > 0) { *lo = -ARC_BIG; *hi = (int)floor(rhs / dy); }
  else { *lo = (int)ceil(rhs / dy); *hi = ARC_BIG; }
}

// Clip a row by the two rays (or, for a filled segment, the caller skips
// this and uses arc_chord_clip directly). Returns 1 or 2 spans in lo[]/hi[].
static int arc_wedge_clip(double dir0_x, double dir0_y, double dir1_x, double dir1_y,
                          bool angle_is_closed, int Y, int *lo, int *hi) {
  int r0_lo, r0_hi, r1_lo, r1_hi;
  arc_ray_clip(dir0_x, dir0_y, Y, &r0_lo, &r0_hi);
  arc_ray_clip(-dir1_x, -dir1_y, Y, &r1_lo, &r1_hi);
  if (!angle_is_closed) {
    // Convex wedge: intersection of the two half-planes.
    lo[0] = MAX(r0_lo, r1_lo);
    hi[0] = MIN(r0_hi, r1_hi);
    return 1;
  }
  if (r0_lo > r0_hi) { lo[0] = r1_lo; hi[0] = r1_hi; return 1; }
  if (r1_lo > r1_hi) { lo[0] = r0_lo; hi[0] = r0_hi; return 1; }
  if (r0_lo <= -ARC_BIG && r0_hi >= ARC_BIG) { lo[0] = r0_lo; hi[0] = r0_hi; return 1; }
  if (r1_lo <= -ARC_BIG && r1_hi >= ARC_BIG) { lo[0] = r1_lo; hi[0] = r1_hi; return 1; }
  if (r0_hi >= r1_lo - 1 && r1_hi >= r0_lo - 1) {
    // Reflex wedge (union of two half-planes), touching/overlapping here.
    lo[0] = MIN(r0_lo, r1_lo);
    hi[0] = MAX(r0_hi, r1_hi);
    return 1;
  }
  // Reflex wedge, disjoint on this row: two separate spans.
  if (r0_lo <= r1_lo) {
    lo[0] = r0_lo; hi[0] = r0_hi; lo[1] = r1_lo; hi[1] = r1_hi;
  } else {
    lo[0] = r1_lo; hi[0] = r1_hi; lo[1] = r0_lo; hi[1] = r0_hi;
  }
  return 2;
}

// Renders each row's annulus-piece x clip-piece intersection the
// moment it's known . full_circle=true (fill_circle, circle's thick ring) skips
// angle clipping entirely and draws the raw annulus/disk interval.
static void circle_row_draw(image_buffer_t *img, int c_x, int c_y, uint32_t color, uint8_t alpha, int Y,
                            int x_out, int x_out_r, bool has_gap, int x_in, int x_in_r,
                            double chord0_x, double chord0_y, double chord1_x, double chord1_y,
                            bool angle_is_closed, bool filled_segment, bool full_circle) {
  int a_lo[2], a_hi[2], a_n;
  if (!has_gap) {
    a_n = 1;
    a_lo[0] = x_out; a_hi[0] = x_out_r;
  } else {
    a_n = 2;
    a_lo[0] = x_out;      a_hi[0] = x_in - 1;
    a_lo[1] = x_in_r + 1; a_hi[1] = x_out_r;
  }

  if (full_circle) {
    for (int i = 0; i < a_n; i++) {
      if (a_lo[i] <= a_hi[i]) {
        h_line(img, c_x + a_lo[i], c_y + Y, a_hi[i] - a_lo[i] + 1, color, alpha);
      }
    }
    return;
  }

  int c_lo[2], c_hi[2], c_n;
  if (filled_segment) {
    c_n = 1;
    arc_chord_clip(chord1_x, chord1_y, chord0_x, chord0_y, Y, &c_lo[0], &c_hi[0]);
  } else {
    c_n = arc_wedge_clip(chord0_x, chord0_y, chord1_x, chord1_y, angle_is_closed, Y, c_lo, c_hi);
  }

  for (int i = 0; i < a_n; i++) {
    for (int j = 0; j < c_n; j++) {
      int lo = MAX(a_lo[i], c_lo[j]);
      int hi = MIN(a_hi[i], c_hi[j]);
      if (lo <= hi) {
        h_line(img, c_x + lo, c_y + Y, hi - lo + 1, color, alpha);
      }
    }
  }
}

// Filled disk, boundary-cursor version of fill_circle's radius>4 case: a
// single boundary (no inner ring), always full_circle=true (no angle clip).
static void fill_circle_scan(image_buffer_t *img, int c_x, int c_y, int radius, uint32_t color, uint8_t alpha) {
  int radius_dbl_sq = radius * radius * 4;
  int xo = -radius;
  for (int y0 = 0; y0 < radius; y0++) {
    int row_dbl_sq = (2 * y0 + 1) * (2 * y0 + 1);
    circle_boundary_advance(&xo, row_dbl_sq, radius_dbl_sq);
    int x_out = xo;
    int x_out_r = -xo - 1;
    circle_row_draw(img, c_x, c_y, color, alpha, y0, x_out, x_out_r, false, 0, -1, 0, 0, 0, 0, false, false, true);
    circle_row_draw(img, c_x, c_y, color, alpha, -y0 - 1, x_out, x_out_r, false, 0, -1, 0, 0, 0, 0, false, false, true);
  }
}

// Thick ring, boundary-cursor version of circle()'s thickness>0 case.
static void circle_ring(image_buffer_t *img, int c_x, int c_y, int radius, int thickness, uint32_t color, uint8_t alpha) {
  int radius_outer = radius;
  int radius_inner = radius - thickness;
  if (radius_inner < 0) radius_inner = 0;

  int radius_outer_dbl_sq = radius_outer * radius_outer * 4;
  int radius_inner_dbl_sq = radius_inner * radius_inner * 4;

  int xo = -radius_outer;
  int xi = -radius_inner;

  for (int y0 = 0; y0 < radius_outer; y0++) {
    int row_dbl_sq = (2 * y0 + 1) * (2 * y0 + 1);
    circle_boundary_advance(&xo, row_dbl_sq, radius_outer_dbl_sq);
    int x_out = xo;
    int x_out_r = -xo - 1;

    bool has_gap = (radius_inner > 0) && (y0 < radius_inner);
    int x_in = 0, x_in_r = -1;
    if (has_gap) {
      circle_boundary_advance(&xi, row_dbl_sq, radius_inner_dbl_sq);
      x_in = xi;
      x_in_r = -xi - 1;
    }

    circle_row_draw(img, c_x, c_y, color, alpha, y0, x_out, x_out_r, has_gap, x_in, x_in_r, 0, 0, 0, 0, false, false, true);
    circle_row_draw(img, c_x, c_y, color, alpha, -y0 - 1, x_out, x_out_r, has_gap, x_in, x_in_r, 0, 0, 0, 0, false, false, true);
  }
}

// Filled rounded rectangle, boundary-cursor version.
static void fill_rounded_rectangle(image_buffer_t *img, int x, int y, int width, int height, int radius, uint32_t color, uint8_t alpha) {
  if (radius > width / 2) radius = width / 2;
  if (radius > height / 2) radius = height / 2;

  if (radius <= 0) {
    for (int row = 0; row < height; row++) {
      h_line(img, x, y + row, width, color, alpha);
    }
    return;
  }

  int radius_dbl_sq = radius * radius * 4;
  int xo = -radius;

  for (int y0 = 0; y0 < radius; y0++) {
    int row_dbl_sq = (2 * y0 + 1) * (2 * y0 + 1);
    circle_boundary_advance(&xo, row_dbl_sq, radius_dbl_sq);
    int inset = radius + xo;

    int row_top = radius - 1 - y0;
    int row_bottom = height - radius + y0;
    h_line(img, x + inset, y + row_top, width - 2 * inset, color, alpha);
    h_line(img, x + inset, y + row_bottom, width - 2 * inset, color, alpha);
  }

  for (int row = radius; row < height - radius; row++) {
    h_line(img, x, y + row, width, color, alpha);
  }
}

static void fill_circle(image_buffer_t *img, int x, int y, int radius, uint32_t color, uint8_t alpha) {
  switch (radius) {
  case 0:
    break;

  case 1:
    putpixel(img, x - 1, y - 1, color, alpha);
    putpixel(img, x, y - 1, color, alpha);
    putpixel(img, x - 1, y, color, alpha);
    putpixel(img, x, y, color, alpha);
    break;

  case 2:
    h_line(img, x - 1, y - 2, 2, color, alpha);
    h_line(img, x - 2, y - 1, 4, color, alpha);
    h_line(img, x - 2, y, 4, color, alpha);
    h_line(img, x - 1, y + 1, 2, color, alpha);
    break;

  case 3:
    h_line(img, x - 2, y - 3, 4, color, alpha);
    h_line(img, x - 3, y - 2, 6, color, alpha);
    h_line(img, x - 3, y - 1, 6, color, alpha);
    h_line(img, x - 3, y, 6, color, alpha);
    h_line(img, x - 3, y + 1, 6, color, alpha);
    h_line(img, x - 2, y + 2, 4, color, alpha);
    break;

  case 4:
    h_line(img, x - 2, y - 4, 4, color, alpha);
    h_line(img, x - 3, y - 3, 6, color, alpha);
    h_line(img, x - 4, y - 2, 8, color, alpha);
    h_line(img, x - 4, y - 1, 8, color, alpha);
    h_line(img, x - 4, y, 8, color, alpha);
    h_line(img, x - 4, y + 1, 8, color, alpha);
    h_line(img, x - 3, y + 2, 6, color, alpha);
    h_line(img, x - 2, y + 3, 4, color, alpha);
    break;

  default:
    fill_circle_scan(img, x, y, radius, color, alpha);
    break;
  }
}

// thickness extends inwards from the given radius circle
static void circle(image_buffer_t *img, int x, int y, int radius, int thickness, uint32_t color, uint8_t alpha) {
  if (thickness <= 0) {
    if (radius == 0) {
      // x0==y0==0 right away, so the loop below never runs and the
      // post-loop diagonal patch (for the x0==y0 hole at 45 degrees)
      // would collapse all 4 of its offsets onto this single point,
      // writing it 4 times instead of once.
      putpixel(img, x, y, color, alpha);
      return;
    }
    int x0 = 0;
    int y0 = radius;
    int d = 5 - 4*radius;
    int da = 12;
    int db = 20 - 8*radius;

    while (x0 < y0) {
      putpixel(img, x + x0, y + y0, color, alpha);
      putpixel(img, x + x0, y - y0, color, alpha);
      putpixel(img, x + y0, y + x0, color, alpha);
      putpixel(img, x - y0, y + x0, color, alpha);
      if (x0 != 0) {
        // At x0==0 (the first step), the mirrored quartet below is the
        // same 4 pixels as above.
        putpixel(img, x - x0, y + y0, color, alpha);
        putpixel(img, x - x0, y - y0, color, alpha);
        putpixel(img, x + y0, y - x0, color, alpha);
        putpixel(img, x - y0, y - x0, color, alpha);
      }
      if (d < 0) { d = d + da; db = db+8; }
      else  { y0 = y0 - 1; d = d+db; db = db + 16; }
      x0 = x0+1;
      da = da + 8;
    }
    // For roughly half of all radii the loop above exits with x0 == y0:
    // x0 always advances by exactly 1 per iteration while y0 advances by
    // at most 1, so the loop guard (x0 < y0) can become false exactly when
    // x0 lands on the diagonal, meaning that point was never plotted (a
    // hole at the 45-degree points, not a double-write). Since x0 == y0
    // here, the usual 8-way mirror also collapses onto only 4 points.
    if (x0 == y0) {
      putpixel(img, x + x0, y + y0, color, alpha);
      putpixel(img, x + x0, y - y0, color, alpha);
      putpixel(img, x - x0, y + y0, color, alpha);
      putpixel(img, x - x0, y - y0, color, alpha);
    }
    return;
  }

  circle_ring(img, x, y, radius, thickness, color, alpha);
}

// Thickness extends outwards and inwards from the given line equally, resulting
// in double the total thickness.
// TODO: This should be more efficient
// http://homepages.enterprise.net/murphy/thickline/index.html
// https://github.com/ArminJo/STMF3-Discovery-Demos/blob/master/lib/BlueDisplay/LocalGUI/ThickLine.hpp
static void line(image_buffer_t *img, int x0, int y0, int x1, int y1, int thickness, int dot1, int dot2, uint32_t c, uint8_t alpha) {
  int dx = abs(x1 - x0);
  int sx = x0 < x1 ? 1 : -1;
  int dy = -abs(y1 - y0);
  int sy = y0 < y1 ? 1 : -1;
  int error = dx + dy;

  if (dot1 > 0) {
    // These are used to deal with consecutive calls with
    // possibly overlapping pixels.
    static int dotcnt = 0;
    static int x_last = 0;
    static int y_last = 0;

    while (true) {
      if (dotcnt <= dot1) {
        if (thickness > 1) {
          fill_circle(img, x0, y0, thickness, c, alpha);
        } else {
          putpixel(img, x0, y0, c, alpha);
        }
      }

      if (x0 != x_last || y0 != y_last) {
        dotcnt++;
      }

      x_last = x0;
      y_last = y0;

      if (dotcnt >= (dot1 + dot2)) {
        dotcnt = 0;
      }

      if (x0 == x1 && y0 == y1) {
        break;
      }
      if ((error * 2) >= dy) {
        if (x0 == x1) {
          break;
        }
        error += dy;
        x0 += sx;
      }
      if ((error * 2) <= dx) {
        if (y0 == y1) {
          break;
        }
        error += dx;
        y0 += sy;
      }
    }
  } else {
    while (true) {
      if (thickness > 1) {
        fill_circle(img, x0, y0, thickness, c, alpha);
      } else {
        putpixel(img, x0, y0, c, alpha);
      }

      if (x0 == x1 && y0 == y1) {
        break;
      }
      if ((error * 2) >= dy) {
        if (x0 == x1) {
          break;
        }
        error += dy;
        x0 += sx;
      }
      if ((error * 2) <= dx) {
        if (y0 == y1) {
          break;
        }
        error += dx;
        y0 += sy;
      }
    }
  }
}

// thickness extends inwards from the given rectangle edge.
static void rectangle(image_buffer_t *img, int x, int y, int width, int height,
                      bool fill, int thickness, int dot1, int dot2, uint32_t color, uint8_t alpha) {
  thickness /= 2;

  if (fill) {
    for (int i = y; i < (y + height);i++) {
      h_line(img, x, i, width, color, alpha);
    }
  } else {
    if (thickness <= 0 && dot1 == 0) {
      h_line(img, x, y, width, color, alpha);
      h_line(img, x, y + height, width, color, alpha);
      v_line(img, x, y, height, color, alpha);
      v_line(img, x + width, y, height, color, alpha);
    } else {
      x += thickness;
      y += thickness;
      width -= thickness * 2;
      height -= thickness * 2;
      // top
      line(img, x, y, x + width, y, thickness, dot1, dot2, color, alpha);
      // bottom
      line(img, x, y + height, x + width, y + height, thickness, dot1, dot2, color, alpha);
      // left
      line(img, x, y, x, y + height, thickness, dot1, dot2, color, alpha);
      // right
      line(img, x + width, y, x + width, y + height, thickness, dot1, dot2, color, alpha);
    }
  }
}

#define NMIN(a, b) ((a) < (b) ? (a) : (b))
#define NMAX(a, b) ((a) > (b) ? (a) : (b))

// Scanline fill:
// See Black art of 3d game programming - André Lamothe for a good
// introduction to scanline fill approaches.
// The triangle filler in "Black art" explicitly splits triangles into subtriangles
// and invents a "cut-vertex" along the long edge. Here the long edge state
// is shared across the "implicitly" split up subtriangles saving some work.
// Most important! Never draw a pixel twice.
static void fill_triangle(image_buffer_t *img, int x0, int y0,
                          int x1, int y1, int x2, int y2, uint32_t color, uint8_t alpha) {
  if (y0 > y1) swap_points(&x0, &y0, &x1, &y1);
  if (y1 > y2) swap_points(&x1, &y1, &x2, &y2);
  if (y0 > y1) swap_points(&x0, &y0, &x1, &y1);

  if (y0 == y2) return;

  float dx_long = (float)(x2 - x0) / (float)(y2 - y0);
  float x_long = (float)x0;

  // Top part of general triangle case
  // If the triangle has a flat top, then y1 == y0 and this loop is skipped
  if (y1 > y0) {
    float dx_short = (float)(x1 - x0) / (float)(y1 - y0);
    float x_short = (float)x0;
    for (int y = y0; y < y1; y++) {
      int xa = (int)x_long, xb = (int)x_short;
      int lo = NMIN(xa, xb), hi = NMAX(xa, xb);
      h_line(img, lo, y, hi - lo + 1, color, alpha);
      x_long += dx_long;
      x_short += dx_short;
    }
  }

  // bottom half of general triangle case
  // y1 is drawn here. The top loop stops at y1-1.
  // This ensures no line is drawn more than once.
  if (y2 > y1) {
    float dx_short = (float)(x2 - x1) / (float)(y2 - y1);
    float x_short = (float)x1;
    for (int y = y1; y <= y2; y++) {
      int xa = (int)x_long, xb = (int)x_short;
      int lo = NMIN(xa, xb), hi = NMAX(xa, xb);
      h_line(img, lo, y, hi - lo + 1, color, alpha);
      x_long += dx_long;
      x_short += dx_short;
    }
  } else {
    // When y2 == y1 the above code draws nothing,
    // So here we add in a final h_line for the flat bottom triangles.
    int xa = (int)x_long, xb = x1;
    int lo = NMIN(xa, xb), hi = NMAX(xa, xb);
    h_line(img, lo, y1, hi - lo + 1, color, alpha);
  }
}

static void generic_arc(image_buffer_t *img, int x, int y, int rad, float ang_start, float ang_end,
                        int thickness, int dot1, int dot2, int res, bool sector, bool segment, uint32_t color, uint8_t alpha) {

  bool full_circle = fabsf(ang_end - ang_start) > 1e-4f
    && fabsf(fmodf(ang_end - ang_start, 360.0f)) < 1e-3f;

  ang_start *= (float)M_PI / 180.0f;
  ang_end *= (float)M_PI / 180.0f;

  norm_angle(&ang_start);
  norm_angle(&ang_end);

  float ang_range = ang_end - ang_start;

  if (full_circle) {
    ang_range = 2.0f * (float)M_PI;
  } else if (ang_range < 0.0) {
    ang_range += 2.0f * (float)M_PI;
  }

  if (res <= 0) {
    res = 80;
  }

  float steps = ceilf((float)res * ang_range * (0.5f / (float)M_PI));

  float ang_step = ang_range / steps;
  float sa = sinf(ang_step);
  float ca = cosf(ang_step);

  float px_start = cosf(ang_start) * (float)rad;
  float py_start = sinf(ang_start) * (float)rad;


  float px = px_start;
  float py = py_start;

  for (int i = 0;i < (int)steps;i++) {
    float px_before = px;
    float py_before = py;

    px = px * ca - py * sa;
    py = py * ca + px_before * sa;

    line(img, x + (int)px_before, y + (int)py_before,
         x + (int)px, y + (int)py, thickness, dot1, dot2, color, alpha);
  }

  if (sector) {
    line(img, x + (int)px, y + (int)py,
         x, y,
         thickness, dot1, dot2, color, alpha);
    line(img, x, y,
         x + (int)px_start, y + (int)py_start,
         thickness, dot1, dot2, color, alpha);
  }

  if (segment) {
    line(img, x + (int)px, y + (int)py,
         x + (int)px_start, y + (int)py_start,
         thickness, dot1, dot2, color, alpha);
  }
}

// Thin/ring arc rasterizer.

static void arc_thin_plot(image_buffer_t *img, int c_x, int c_y, int px, int py,
                          double cap0_x, double cap0_y, double cap1_x, double cap1_y,
                          bool angle_is_closed, bool full_circle, uint32_t color, uint8_t alpha) {
  bool inside;
  if (full_circle) {
    inside = true;
  } else {
    double cross0 = px * cap0_y - py * cap0_x;
    double cross1 = px * cap1_y - py * cap1_x;
    bool inside0 = cross0 <= 0;
    bool inside1 = cross1 >= 0;
    inside = angle_is_closed ? (inside0 || inside1) : (inside0 && inside1);
  }
  if (inside) putpixel(img, c_x + px, c_y + py, color, alpha);
}

// Thin (<=1px) outline: classic 8-way symmetric midpoint circle (same
// recurrence as the plain circle() thin path). Octant symmetry means
// consecutive plotted points are always <=1 pixel apart, so -- unlike the
// row-scan approach -- there is no gap to patch up near the poles.
static void arc_thin(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1, bool sector, bool segment, uint32_t color, uint8_t alpha) {
  if (radius == 0) return;

  bool full_circle = fabsf(angle1 - angle0) > 1e-4f
    && fabsf(fmodf(angle1 - angle0, 360.0f)) < 1e-3f;

  angle0 *= (float)M_PI / 180.0f;
  angle1 *= (float)M_PI / 180.0f;
  norm_angle_0_2pi(&angle0);
  norm_angle_0_2pi(&angle1);
  if (!full_circle && angle0 == angle1) return;

  bool angle_is_closed;
  if (angle1 - angle0 > 0.0) angle_is_closed = fabsf(angle1 - angle0) > M_PI;
  else angle_is_closed = fabsf(angle1 - angle0) < M_PI;

  int cap0_x = (int)(cosf(angle0) * (float)radius);
  int cap0_y = (int)(sinf(angle0) * (float)radius);
  int cap1_x = (int)(cosf(angle1) * (float)radius);
  int cap1_y = (int)(sinf(angle1) * (float)radius);

  double ray0_x = cos(angle0), ray0_y = sin(angle0);
  double ray1_x = cos(angle1), ray1_y = sin(angle1);

  long r_dbl_sq = 4L * radius * radius;
  int px = 0, py = radius;
  while (px <= py) {
    while (py >= px && (long)(2 * px + 1) * (2 * px + 1) + (long)(2 * py + 1) * (2 * py + 1) > r_dbl_sq) py--;
    if (px > py) break;
    arc_thin_plot(img, c_x, c_y,  px,      py,     ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, color, alpha);
    arc_thin_plot(img, c_x, c_y,  px,     -py - 1, ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, color, alpha);
    arc_thin_plot(img, c_x, c_y, -px - 1,  py,     ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, color, alpha);
    arc_thin_plot(img, c_x, c_y, -px - 1, -py - 1, ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, color, alpha);
    if (px != py) {
      // At the px==py octant boundary, (px,py)/(py,px) etc. are the same
      // four pixels as above
      arc_thin_plot(img, c_x, c_y,  py,      px,     ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, color, alpha);
      arc_thin_plot(img, c_x, c_y,  py,     -px - 1, ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, color, alpha);
      arc_thin_plot(img, c_x, c_y, -py - 1,  px,     ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, color, alpha);
      arc_thin_plot(img, c_x, c_y, -py - 1, -px - 1, ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, color, alpha);
    }
    px += 1;
  }

  if (sector) {
    line(img, c_x, c_y, c_x + cap0_x, c_y + cap0_y, 1, 0, 0, color, alpha);
    line(img, c_x, c_y, c_x + cap1_x, c_y + cap1_y, 1, 0, 0, color, alpha);
  }
  if (segment) {
    line(img, c_x + cap0_x, c_y + cap0_y, c_x + cap1_x, c_y + cap1_y, 1, 0, 0, color, alpha);
  }
}

static void arc_ring(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1,
                             int thickness, bool rounded, bool filled, bool sector, bool segment, uint32_t color, uint8_t alpha) {
  if (thickness <= 1 && !filled) {
    arc_thin(img, c_x, c_y, radius, angle0, angle1, sector, segment, color, alpha);
    return;
  }
  if (radius == 0) return;

  bool full_circle = fabsf(angle1 - angle0) > 1e-4f
    && fabsf(fmodf(angle1 - angle0, 360.0f)) < 1e-3f;

  angle0 *= (float)M_PI / 180.0f;
  angle1 *= (float)M_PI / 180.0f;
  norm_angle_0_2pi(&angle0);
  norm_angle_0_2pi(&angle1);
  if (!full_circle && angle0 == angle1) return;

  bool angle_is_closed;
  if (angle1 - angle0 > 0.0) angle_is_closed = fabsf(angle1 - angle0) > M_PI;
  else angle_is_closed = fabsf(angle1 - angle0) < M_PI;

  if (!full_circle && !angle_is_closed && fabsf(angle1 - angle0) < 0.0174532925) {
    if (rounded) {
      float rad_f = (float)radius - ((float)thickness / 2.0f);
      float angle = (angle0 + angle1) / 2.0f;
      int cap_center_x = (int)floorf(cosf(angle) * rad_f);
      int cap_center_y = (int)floorf(sinf(angle) * rad_f);
      fill_circle(img, c_x + cap_center_x, c_y + cap_center_y, thickness / 2, color, alpha);
    }
    return;
  }

  if (thickness >= radius) filled = true;

  int radius_outer = radius;
  int radius_inner = filled ? 0 : (radius - thickness);
  if (radius_inner < 0) radius_inner = 0;

  float angle0_cos = cosf(angle0), angle0_sin = sinf(angle0);
  float angle1_cos = cosf(angle1), angle1_sin = sinf(angle1);

  bool filled_segment = filled && segment;

  int radius_outer_dbl_sq = radius_outer * radius_outer * 4;
  int radius_inner_dbl_sq = radius_inner * radius_inner * 4;

  int xo = -radius_outer;
  int xi = -radius_inner;

  for (int y0 = 0; y0 < radius_outer; y0++) {
    int row_dbl_sq = (2 * y0 + 1) * (2 * y0 + 1);
    circle_boundary_advance(&xo, row_dbl_sq, radius_outer_dbl_sq);
    int x_out = xo;
    int x_out_r = -xo - 1;

    bool has_gap = (radius_inner > 0) && (y0 < radius_inner);
    int x_in = 0, x_in_r = -1;
    if (has_gap) {
      circle_boundary_advance(&xi, row_dbl_sq, radius_inner_dbl_sq);
      x_in = xi;
      x_in_r = -xi - 1;
    }

    circle_row_draw(img, c_x, c_y, color, alpha, y0, x_out, x_out_r, has_gap, x_in, x_in_r,
                    (double)angle0_cos * radius_outer, (double)angle0_sin * radius_outer,
                    (double)angle1_cos * radius_outer, (double)angle1_sin * radius_outer,
                    angle_is_closed, filled_segment, full_circle);
    circle_row_draw(img, c_x, c_y, color, alpha, -y0 - 1, x_out, x_out_r, has_gap, x_in, x_in_r,
                    (double)angle0_cos * radius_outer, (double)angle0_sin * radius_outer,
                    (double)angle1_cos * radius_outer, (double)angle1_sin * radius_outer,
                    angle_is_closed, filled_segment, full_circle);
  }

  if (rounded && !filled && !sector && !segment) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);
    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);
    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);
    int th = thickness / 2;
    fill_circle(img, c_x + cap0_center_x, c_y + cap0_center_y, th, color, alpha);
    fill_circle(img, c_x + cap1_center_x, c_y + cap1_center_y, th, color, alpha);
  }
  if (sector && !filled) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);
    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);
    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);
    int th = thickness / 2;
    line(img, c_x + cap0_center_x, c_y + cap0_center_y, c_x, c_y, th, 0, 0, color, alpha);
    line(img, c_x + cap1_center_x, c_y + cap1_center_y, c_x, c_y, th, 0, 0, color, alpha);
  }
  if (segment && !filled) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);
    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);
    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);
    int th = thickness / 2;
    line(img, c_x + cap0_center_x, c_y + cap0_center_y, c_x + cap1_center_x, c_y + cap1_center_y, th, 0, 0, color, alpha);
  }
}

static void arc(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1,
                int thickness, bool rounded, bool filled, bool sector, bool segment, int dot1, int dot2, int resolution, uint32_t color, uint8_t alpha) {
  if (dot1 > 0 && !filled) {
    thickness /= 2;

    radius -= thickness;

    if (thickness == 0) {
      thickness = 1;
    }

    generic_arc(img, c_x, c_y, radius, angle0, angle1, thickness, dot1, dot2, resolution, sector, segment, color, alpha);

    return;
  }

  arc_ring(img, c_x, c_y, radius, angle0, angle1, thickness, rounded, filled, sector, segment, color, alpha);
}

// orient: 0=normal, 1=up(90°CCW), 2=180°, 3=down(90°CW)
static void img_putc(image_buffer_t *img, int x, int y, uint32_t *colors, int num_colors,
                     uint8_t *font_data, uint8_t ch, int orient, float mag) {
  uint8_t w = font_data[0];
  uint8_t h = font_data[1];
  uint8_t char_num = font_data[2];
  uint8_t bits_per_pixel = font_data[3];

  int pixels_per_byte = (int)(8 / bits_per_pixel);
  int bytes_per_char = (int)((w * h) / pixels_per_byte);
  if ((w * h) % pixels_per_byte != 0) {
    bytes_per_char += 1;
  }

  if (char_num == 10) {
    ch = (uint8_t)(ch - '0');
  } else {
    ch = (uint8_t)(ch - ' ');
  }

  if (ch >= char_num) {
    return;
  }

  for (int i = 0; i < w * h; i++) {
    int x0 = i % w;
    int y0 = i / w;

    int sx0 = (int)floorf((float)x0 * mag);
    int sx1 = (int)floorf((float)(x0 + 1) * mag) - 1;
    int sy0 = (int)floorf((float)y0 * mag);
    int sy1 = (int)floorf((float)(y0 + 1) * mag) - 1;

    if (sx1 < sx0) sx1 = sx0;
    if (sy1 < sy0) sy1 = sy0;

    uint32_t color;
    if (bits_per_pixel == 2) {
      if (num_colors < 4) return;
      uint8_t byte = font_data[4 + bytes_per_char * ch + (i / 4)];
      uint8_t bit_pos = (uint8_t)(i % pixels_per_byte);
      uint8_t pixel_value = (byte >> (bit_pos * 2)) & 0x03;
      color = colors[pixel_value];
    } else {
      if (num_colors < 1) return;
      int32_t fg = (int32_t)colors[0];
      int32_t bg = (num_colors > 1) ? (int32_t)colors[1] : -1;
      uint8_t byte = font_data[4 + bytes_per_char * ch + (i / 8)];
      uint8_t bit_pos = (uint8_t)(i % 8);
      uint8_t bit = (uint8_t)(byte & (1 << bit_pos));
      if (!bit && bg < 0) {
        continue;
      }
      color = bit ? (uint32_t)fg : (uint32_t)bg;
    }

    for (int py = sy0; py <= sy1; py++) {
      for (int px = sx0; px <= sx1; px++) {
        switch (orient) {
          case 1:  putpixel(img, x + py, y - px, color, 255); break;
          case 2:  putpixel(img, x - px, y - py, color, 255); break;
          case 3:  putpixel(img, x - py, y + px, color, 255); break;
          default: putpixel(img, x + px, y + py, color, 255); break;
        }
      }
    }
  }
}

static inline void copy_pixel(
        image_buffer_t *img_dest,
    image_buffer_t *img_src,
    int dest_x, int dest_y,
    int src_x, int src_y,
    int src_w, int src_h,
    int transparent_color,
    bool tile
) {
    if (tile) {
        src_x = src_x % src_w;
        if (src_x < 0) src_x = src_x + src_w;
        src_y = src_y % src_h;
        if (src_y < 0) src_y = src_y + src_h;
    }

    if (src_x >= 0 && src_x < src_w && src_y >= 0 && src_y < src_h) {
        uint32_t p = getpixel(img_src, src_x, src_y);
        if (transparent_color == -1 || p != (uint32_t)transparent_color) {
            putpixel(img_dest, dest_x, dest_y, p, 255);
        }
    }
}

// Copy pixels from source to destination with transformations
void blit(
    image_buffer_t *img_dest,  // Destination image buffer
    image_buffer_t *img_src,   // Source image buffer
    int dest_offset_x, int dest_offset_y,              // Where on dest to start writing pixels
    float rot_x, float rot_y,  // Coordinate in src to rotate around
    float rot_angle,           // Rotation angle in degrees
    float scale,               // Scale factor
    int32_t transparent_color, // Color that will not be drawn -1 to disable
    bool tile,                 // Tile src to fill dest
    int clip_x, int clip_y,    // Clip start in dest
    int clip_w, int clip_h     // Clip width and height
) {
  if (scale == 0.0) return;
  int src_w = img_src->width;
  int src_h = img_src->height;

  int dest_x_start = clip_x;
  int dest_y_start = clip_y;
  int dest_x_end = clip_w;
  int dest_y_end = clip_h;

  if (rot_angle == 0.0 && scale == 1.0) {
    if (dest_offset_x > 0) dest_x_start += dest_offset_x;
    if (dest_offset_y > 0) dest_y_start += dest_offset_y;
    if (!tile) {
        if ((dest_x_end - dest_offset_x) > src_w) dest_x_end = src_w + dest_offset_x;
        if ((dest_y_end - dest_offset_y) > src_h) dest_y_end = src_h + dest_offset_y;
    }

    for (int dest_y = dest_y_start; dest_y < dest_y_end; dest_y++) {
      for (int dest_x = dest_x_start; dest_x < dest_x_end; dest_x++) {
        int src_x = dest_x - dest_offset_x;
        int src_y = dest_y - dest_offset_y;
        copy_pixel(img_dest, img_src, dest_x, dest_y, src_x, src_y, src_w, src_h, transparent_color, tile);
      }
    }
  } else if (rot_angle == 0.0) {
    rot_x *= scale;
    rot_y *= scale;

    const int fp_scale = 1000;

    int rot_x_x = (int)rot_x;
    int rot_y_i = (int)rot_y;
    int scale_i = (int)(scale * (float) fp_scale);

    for (int dest_y = dest_y_start; dest_y < dest_y_end; dest_y++) {
      for (int dest_x = dest_x_start; dest_x < dest_x_end; dest_x++) {
        int src_x = (dest_x - dest_offset_x - rot_x_x) * fp_scale;
        int src_y = (dest_y - dest_offset_y - rot_y_i) * fp_scale;

        src_x += rot_x_x * fp_scale;
        src_y += rot_y_i * fp_scale;

        src_x /= scale_i;
        src_y /= scale_i;
        copy_pixel(img_dest, img_src, dest_x, dest_y, src_x, src_y, src_w, src_h, transparent_color, tile);
      }
    }
  } else {
    float sin_rot_angle = sinf(-rot_angle * (float)M_PI / 180.0f);
    float cos_rot_angle = cosf(-rot_angle * (float)M_PI / 180.0f);

    rot_x *= scale;
    rot_y *= scale;

    const int fp_scale = 1000;

    int sin_rot_angle_i = (int)(sin_rot_angle * (float)fp_scale);
    int cos_rot_angle_i = (int)(cos_rot_angle * (float)fp_scale);
    int rot_x_i = (int)rot_x;
    int rot_y_i = (int)rot_y;
    int scale_i = (int)(scale * (float) fp_scale);

    for (int dest_y = dest_y_start; dest_y < dest_y_end; dest_y++) {
      for (int dest_x = dest_x_start; dest_x < dest_x_end; dest_x++) {
        int src_x =  (dest_x - dest_offset_x - rot_x_i) * cos_rot_angle_i + (dest_y - dest_offset_y - rot_y_i) * sin_rot_angle_i;
        int src_y = -(dest_x - dest_offset_x - rot_x_i) * sin_rot_angle_i + (dest_y - dest_offset_y - rot_y_i) * cos_rot_angle_i;

        src_x += rot_x_i * fp_scale;
        src_y += rot_y_i * fp_scale;

        src_x /= scale_i;
        src_y /= scale_i;
        copy_pixel(img_dest, img_src, dest_x, dest_y, src_x,  src_y, src_w, src_h, transparent_color, tile);
      }
    }
  }
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

  line(&arg_dec.img,
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

// lisp args: img cx cy r color opt-attr1 ... opt-attrN
static lbm_value ext_circle(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 4);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  if (arg_dec.attr_filled.is_valid) {
    fill_circle(&arg_dec.img,
                lbm_dec_as_i32(arg_dec.args[0]),
                lbm_dec_as_i32(arg_dec.args[1]),
                lbm_dec_as_i32(arg_dec.args[2]),
                lbm_dec_as_u32(arg_dec.args[3]),
                arg_dec.alpha);
  } else if (arg_dec.attr_dotted.is_valid) {
    arc(&arg_dec.img,
        lbm_dec_as_i32(arg_dec.args[0]),
        lbm_dec_as_i32(arg_dec.args[1]),
        lbm_dec_as_i32(arg_dec.args[2]),
        0, 359.9f,
        lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
        arg_dec.attr_rounded.is_valid, // currently does nothing as the line function doesn't support square ends.
        false,
        false, false,
        lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
        lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
        lbm_dec_as_i32(arg_dec.attr_resolution.args[0]),
        lbm_dec_as_u32(arg_dec.args[3]),
        arg_dec.alpha);
  } else {
    circle(&arg_dec.img,
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

  arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
      arg_dec.attr_rounded.is_valid,
      arg_dec.attr_filled.is_valid,
      false, false,
      lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
      lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
      lbm_dec_as_i32(arg_dec.attr_resolution.args[0]),
      lbm_dec_as_u32(arg_dec.args[5]),
      arg_dec.alpha);

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r ang-s ang-e color opt-attr1 ... opt-attrN
static lbm_value ext_circle_sector(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 6);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
      true,
      arg_dec.attr_filled.is_valid,
      true, false,
      lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
      lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
      lbm_dec_as_i32(arg_dec.attr_resolution.args[0]),
      lbm_dec_as_u32(arg_dec.args[5]),
      arg_dec.alpha);

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r ang-s ang-e color opt-attr1 ... opt-attrN
static lbm_value ext_circle_segment(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 6);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
      true,
      arg_dec.attr_filled.is_valid,
      false, true,
      lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
      lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
      lbm_dec_as_i32(arg_dec.attr_resolution.args[0]),
      lbm_dec_as_u32(arg_dec.args[5]),
      arg_dec.alpha);


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
      fill_rounded_rectangle(img, x, y, width, height, rad, color, arg_dec.alpha);
    } else {
      // Remember to change these to use the rounded attribute,
      // when/if line supports it!

      int line_thickness = thickness / 2;
      thickness = line_thickness * 2; // round it to even for consistency.

      // top
      line(img, x + rad, y + line_thickness, x + width - rad, y + line_thickness, line_thickness, dot1, dot2, color, arg_dec.alpha);
      // bottom
      line(img, x + rad, y + height - line_thickness, x + width - rad, y + height - line_thickness, line_thickness, dot1, dot2, color, arg_dec.alpha);
      // left
      line(img, x + line_thickness, y + rad, x + line_thickness, y + height - rad, line_thickness, dot1, dot2, color, arg_dec.alpha);
      // right
      line(img, x + width - line_thickness, y + rad, x + width - line_thickness, y + height - rad, line_thickness, dot1, dot2, color, arg_dec.alpha);

      // upper left
      arc(img, x + rad, y + rad, rad, 180, 270, thickness, false, false, false, false, dot1, dot2, resolution, color, arg_dec.alpha);
      // upper right
      arc(img, x + width - rad, y + rad, rad, 270, 0, thickness, false, false, false, false, dot1, dot2, resolution, color, arg_dec.alpha);
      // bottom left
      arc(img, x + rad, y + height - rad, rad, 90, 180, thickness, false, false, false, false, dot1, dot2, resolution, color, arg_dec.alpha);
      // bottom right
      arc(img, x + width - rad, y + height - rad, rad, 0, 90, thickness, false, false, false, false, dot1, dot2, resolution, color, arg_dec.alpha);
    }
  } else {
    rectangle(img,
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
    fill_triangle(img, x0, y0, x1, y1, x2, y2, color, arg_dec.alpha);
  } else {
    line(img, x0, y0, x1, y1, thickness, dot1, dot2, color, arg_dec.alpha);
    line(img, x1, y1, x2, y2, thickness, dot1, dot2, color, arg_dec.alpha);
    line(img, x2, y2, x0, y0, thickness, dot1, dot2, color, arg_dec.alpha);
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
    img_putc(&img_buf,
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
