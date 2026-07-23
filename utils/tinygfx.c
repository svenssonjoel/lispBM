/*
  Copyright 2023 - 2025 Benjamin Vedder            benjamin@vedder.se
  Copyright 2023 - 2026 Joel Svensson              svenssonjoel@yahoo.se
  Copyright 2023        Rasmus Söderhielm          rasmus.soderhielm@gmail.com
  Copyright 2025        Joakim Lundborg            joakim.lundborg@gmail.com

  This file is part of LispBM.

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

#include "tinygfx.h"
#include "tjpgd.h"
#include "lbm_cos_table.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#ifdef LBM_OPT_DISPLAY_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_DISPLAY_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

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

static inline void swap_points(int *x0, int *y0, int *x1, int *y1) {
  int tx = *x0, ty = *y0;
  *x0 = *x1; *y0 = *y1;
  *x1 = tx;  *y1 = ty;
}

static inline void norm_angle(float *angle) {
  while (*angle < -M_PI) { *angle += 2.0f * (float)M_PI; }
  while (*angle >=  M_PI) { *angle -= 2.0f * (float)M_PI; }
}

static inline void norm_angle_0_2pi(float *angle) {
  while (*angle < 0) { *angle += 2.0f * (float)M_PI; }
  while (*angle >= 2.0 * M_PI) { *angle -= 2.0f * (float)M_PI; }
}

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

void tinygfx_fill_rounded_rectangle(image_buffer_t *img, int x, int y, int width, int height, int radius, uint32_t color, uint8_t alpha) {
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

void tinygfx_fill_circle(image_buffer_t *img, int x, int y, int radius, uint32_t color, uint8_t alpha) {
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
void tinygfx_circle(image_buffer_t *img, int x, int y, int radius, int thickness, uint32_t color, uint8_t alpha) {
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
void tinygfx_line(image_buffer_t *img, int x0, int y0, int x1, int y1, int thickness, int dot1, int dot2, uint32_t c, uint8_t alpha) {
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
          tinygfx_fill_circle(img, x0, y0, thickness, c, alpha);
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
        tinygfx_fill_circle(img, x0, y0, thickness, c, alpha);
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
void tinygfx_rectangle(image_buffer_t *img, int x, int y, int width, int height,
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
      tinygfx_line(img, x, y, x + width, y, thickness, dot1, dot2, color, alpha);
      // bottom
      tinygfx_line(img, x, y + height, x + width, y + height, thickness, dot1, dot2, color, alpha);
      // left
      tinygfx_line(img, x, y, x, y + height, thickness, dot1, dot2, color, alpha);
      // right
      tinygfx_line(img, x + width, y, x + width, y + height, thickness, dot1, dot2, color, alpha);
    }
  }
}

// Scanline fill:
// See Black art of 3d game programming - André Lamothe for a good
// introduction to scanline fill approaches.
// The triangle filler in "Black art" explicitly splits triangles into subtriangles
// and invents a "cut-vertex" along the long edge. Here the long edge state
// is shared across the "implicitly" split up subtriangles saving some work.
// Most important! Never draw a pixel twice.
void tinygfx_fill_triangle(image_buffer_t *img, int x0, int y0,
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
      int lo = MIN(xa, xb), hi = MAX(xa, xb);
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
      int lo = MIN(xa, xb), hi = MAX(xa, xb);
      h_line(img, lo, y, hi - lo + 1, color, alpha);
      x_long += dx_long;
      x_short += dx_short;
    }
  } else {
    // When y2 == y1 the above code draws nothing,
    // So here we add in a final h_line for the flat bottom triangles.
    int xa = (int)x_long, xb = x1;
    int lo = MIN(xa, xb), hi = MAX(xa, xb);
    h_line(img, lo, y1, hi - lo + 1, color, alpha);
  }
}

static void generic_arc(image_buffer_t *img, int x, int y, int rad, float ang_start, float ang_end,
                        const arc_params_t *p) {

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

  int res = p->resolution;
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

    tinygfx_line(img, x + (int)px_before, y + (int)py_before,
         x + (int)px, y + (int)py, p->thickness, p->dot1, p->dot2, p->color, p->alpha);
  }

  if (p->sector) {
    tinygfx_line(img, x + (int)px, y + (int)py,
         x, y,
         p->thickness, p->dot1, p->dot2, p->color, p->alpha);
    tinygfx_line(img, x, y,
         x + (int)px_start, y + (int)py_start,
         p->thickness, p->dot1, p->dot2, p->color, p->alpha);
  }

  if (p->segment) {
    tinygfx_line(img, x + (int)px, y + (int)py,
         x + (int)px_start, y + (int)py_start,
         p->thickness, p->dot1, p->dot2, p->color, p->alpha);
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
static void arc_thin(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1, const arc_params_t *p) {
  if (radius <= 0) return;

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

  double ray0_x = cosf(angle0), ray0_y = sinf(angle0);
  double ray1_x = cosf(angle1), ray1_y = sinf(angle1);

  long r_dbl_sq = 4L * radius * radius;
  int px = 0, py = radius;
  while (px <= py) {
    while (py >= px && (long)(2 * px + 1) * (2 * px + 1) + (long)(2 * py + 1) * (2 * py + 1) > r_dbl_sq) py--;
    if (px > py) break;
    arc_thin_plot(img, c_x, c_y,  px,      py,     ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, p->color, p->alpha);
    arc_thin_plot(img, c_x, c_y,  px,     -py - 1, ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, p->color, p->alpha);
    arc_thin_plot(img, c_x, c_y, -px - 1,  py,     ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, p->color, p->alpha);
    arc_thin_plot(img, c_x, c_y, -px - 1, -py - 1, ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, p->color, p->alpha);
    if (px != py) {
      // At the px==py octant boundary, (px,py)/(py,px) etc. are the same
      // four pixels as above
      arc_thin_plot(img, c_x, c_y,  py,      px,     ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, p->color, p->alpha);
      arc_thin_plot(img, c_x, c_y,  py,     -px - 1, ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, p->color, p->alpha);
      arc_thin_plot(img, c_x, c_y, -py - 1,  px,     ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, p->color, p->alpha);
      arc_thin_plot(img, c_x, c_y, -py - 1, -px - 1, ray0_x, ray0_y, ray1_x, ray1_y, angle_is_closed, full_circle, p->color, p->alpha);
    }
    px += 1;
  }

  if (p->sector) {
    tinygfx_line(img, c_x, c_y, c_x + cap0_x, c_y + cap0_y, 1, 0, 0, p->color, p->alpha);
    tinygfx_line(img, c_x, c_y, c_x + cap1_x, c_y + cap1_y, 1, 0, 0, p->color, p->alpha);
  }
  if (p->segment) {
    tinygfx_line(img, c_x + cap0_x, c_y + cap0_y, c_x + cap1_x, c_y + cap1_y, 1, 0, 0, p->color, p->alpha);
  }
}

static void arc_ring(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1,
                             const arc_params_t *p) {
  if (p->thickness <= 1 && !p->filled) {
    arc_thin(img, c_x, c_y, radius, angle0, angle1, p);
    return;
  }
  if (radius <= 0) return;

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

  int thickness = p->thickness;

  if (!full_circle && !angle_is_closed && fabsf(angle1 - angle0) < 0.0174532925) {
    if (p->rounded) {
      float rad_f = (float)radius - ((float)thickness / 2.0f);
      float angle = (angle0 + angle1) / 2.0f;
      int cap_center_x = (int)floorf(cosf(angle) * rad_f);
      int cap_center_y = (int)floorf(sinf(angle) * rad_f);
      tinygfx_fill_circle(img, c_x + cap_center_x, c_y + cap_center_y, thickness / 2, p->color, p->alpha);
    }
    return;
  }

  bool filled = p->filled;
  if (thickness >= radius) filled = true;

  int radius_outer = radius;
  int radius_inner = filled ? 0 : (radius - thickness);
  if (radius_inner < 0) radius_inner = 0;

  float angle0_cos = cosf(angle0), angle0_sin = sinf(angle0);
  float angle1_cos = cosf(angle1), angle1_sin = sinf(angle1);

  bool filled_segment = filled && p->segment;

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

    circle_row_draw(img, c_x, c_y, p->color, p->alpha, y0, x_out, x_out_r, has_gap, x_in, x_in_r,
                    (double)angle0_cos * radius_outer, (double)angle0_sin * radius_outer,
                    (double)angle1_cos * radius_outer, (double)angle1_sin * radius_outer,
                    angle_is_closed, filled_segment, full_circle);
    circle_row_draw(img, c_x, c_y, p->color, p->alpha, -y0 - 1, x_out, x_out_r, has_gap, x_in, x_in_r,
                    (double)angle0_cos * radius_outer, (double)angle0_sin * radius_outer,
                    (double)angle1_cos * radius_outer, (double)angle1_sin * radius_outer,
                    angle_is_closed, filled_segment, full_circle);
  }

  if (p->rounded && !filled && !p->sector && !p->segment) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);
    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);
    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);
    int th = thickness / 2;
    tinygfx_fill_circle(img, c_x + cap0_center_x, c_y + cap0_center_y, th, p->color, p->alpha);
    tinygfx_fill_circle(img, c_x + cap1_center_x, c_y + cap1_center_y, th, p->color, p->alpha);
  }
  if (p->sector && !filled) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);
    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);
    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);
    int th = thickness / 2;
    tinygfx_line(img, c_x + cap0_center_x, c_y + cap0_center_y, c_x, c_y, th, 0, 0, p->color, p->alpha);
    tinygfx_line(img, c_x + cap1_center_x, c_y + cap1_center_y, c_x, c_y, th, 0, 0, p->color, p->alpha);
  }
  if (p->segment && !filled) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);
    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);
    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);
    int th = thickness / 2;
    tinygfx_line(img, c_x + cap0_center_x, c_y + cap0_center_y, c_x + cap1_center_x, c_y + cap1_center_y, th, 0, 0, p->color, p->alpha);
  }
}

void tinygfx_arc(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1,
                const arc_params_t *p) {
  if (p->dot1 > 0 && !p->filled) {
    arc_params_t p2 = *p;
    p2.thickness /= 2;

    radius -= p2.thickness;

    if (p2.thickness == 0) {
      p2.thickness = 1;
    }

    generic_arc(img, c_x, c_y, radius, angle0, angle1, &p2);

    return;
  }

  arc_ring(img, c_x, c_y, radius, angle0, angle1, p);
}

// orient: 0=normal, 1=up(90°CCW), 2=180°, 3=down(90°CW)
void tinygfx_img_putc(image_buffer_t *img, int x, int y, uint32_t *colors, int num_colors,
                     const uint8_t *font_data, uint8_t ch, int orient, float mag) {
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
  int dest_x_end = clip_x + clip_w;
  int dest_y_end = clip_y + clip_h;

  if (rot_angle == 0.0 && scale == 1.0) {
    if (dest_offset_x > dest_x_start) dest_x_start = dest_offset_x;
    if (dest_offset_y > dest_y_start) dest_y_start = dest_offset_y;
    if (!tile) {
        if (dest_offset_x + src_w < dest_x_end) dest_x_end = dest_offset_x + src_w;
        if (dest_offset_y + src_h < dest_y_end) dest_y_end = dest_offset_y + src_h;
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

typedef struct {
  const uint8_t *data;
  size_t pos;
  size_t size;
  image_buffer_t *dest;
  int ofs_x;
  int ofs_y;
} tinygfx_jpg_io_t;

static size_t tinygfx_jpg_input(JDEC* jd, uint8_t* buff, size_t ndata) {
  tinygfx_jpg_io_t *dev = (tinygfx_jpg_io_t*)jd->device;

  if (ndata > (dev->size - dev->pos)) {
    ndata = dev->size - dev->pos;
  }

  if (buff) {
    memcpy(buff, dev->data + dev->pos, ndata);
  }
  dev->pos += ndata;
  return ndata;
}

static int tinygfx_jpg_output(JDEC* jd, void* bitmap, JRECT* rect) {
  tinygfx_jpg_io_t *dev = (tinygfx_jpg_io_t*)jd->device;

  image_buffer_t src;
  src.mem_base = (uint8_t*)bitmap;
  src.data = (uint8_t*)bitmap;
  src.width = (uint16_t)(rect->right - rect->left + 1);
  src.height = (uint16_t)(rect->bottom - rect->top + 1);
  src.fmt = rgb888;

  int dx = dev->ofs_x + rect->left;
  int dy = dev->ofs_y + rect->top;

  for (int y = 0; y < src.height; y++) {
    for (int x = 0; x < src.width; x++) {
      putpixel(dev->dest, dx + x, dy + y, getpixel(&src, x, y), 255);
    }
  }

  return 1;
}

bool tinygfx_decode_jpg(image_buffer_t *dest, const uint8_t *jpg_data, size_t jpg_size,
                         int ofs_x, int ofs_y, void *work_buf, size_t work_buf_size) {
  if (!work_buf || work_buf_size < 4096) {
    return false;
  }

  tinygfx_jpg_io_t io;
  io.data = jpg_data;
  io.pos = 0;
  io.size = jpg_size;
  io.dest = dest;
  io.ofs_x = ofs_x;
  io.ofs_y = ofs_y;

  JDEC jd;
  if (jd_prepare(&jd, tinygfx_jpg_input, work_buf, work_buf_size, &io) != JDR_OK) {
    return false;
  }
  return jd_decomp(&jd, tinygfx_jpg_output, 0) == JDR_OK;
}
