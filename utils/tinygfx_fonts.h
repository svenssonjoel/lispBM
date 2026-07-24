/*
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

#ifndef TINYGFX_FONTS_H_
#define TINYGFX_FONTS_H_

#include <stdint.h>

#define LBM_DISPLAY_FONT_RETRO_5X7 1

#ifndef USE_TINYGFX_FONT_1
#define USE_TINYGFX_FONT_1 1
#endif

#ifndef LBM_DISPLAY_DEFAULT_FONT
#define LBM_DISPLAY_DEFAULT_FONT LBM_DISPLAY_FONT_RETRO_5X7
#endif

#if (LBM_DISPLAY_DEFAULT_FONT == LBM_DISPLAY_FONT_RETRO_5X7) && !USE_TINYGFX_FONT_1
#error "LBM_DISPLAY_DEFAULT_FONT 1 is disabled"
#endif

#if USE_TINYGFX_FONT_1

static const uint8_t tinygfx_font_retro_5x7[] = {
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

#endif

static inline const uint8_t *tinygfx_get_builtin_font(int font_id) {
#if USE_TINYGFX_FONT_1
  if (font_id == LBM_DISPLAY_FONT_RETRO_5X7) {
    return tinygfx_font_retro_5x7;
  }
#else
  (void)font_id;
#endif
  return 0;
}

#endif

