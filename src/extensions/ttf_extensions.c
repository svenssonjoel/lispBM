/*
  Copyright 2025 Joel Svensson              svenssonjoel@yahoo.se

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

#include <extensions/ttf_extensions.h>
#include <extensions.h>
#include <buffer.h>

#include "schrift.h"

bool is_font_value(lbm_value font) {
  return (lbm_is_cons(font) &&
          lbm_is_number(lbm_car(font)) &&
          lbm_is_number(lbm_car(lbm_cdr(font))) &&
          lbm_is_array_r(lbm_car(lbm_cdr(lbm_cdr(font)))));
}

bool is_prepared_font_value(lbm_value font) {
  return (lbm_is_cons(font) &&
          lbm_is_number(lbm_car(font)) &&
          lbm_is_number(lbm_cadr(font)) &&
          lbm_is_array_r(lbm_cadr(lbm_cdr(font))) &&
          lbm_is_cons(lbm_cadr(lbm_cdr(lbm_cdr(font)))));
}

static uint32_t font_get_x_scale(lbm_value font_val) {
  return lbm_dec_u(lbm_car(font_val));
}

static uint32_t font_get_y_scale(lbm_value font_val) {
  return lbm_dec_u(lbm_car(lbm_cdr(font_val)));
}

static bool mk_font_raw(SFT_Font *ft, lbm_value font_val) {
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(font_val);
  ft->memory = (uint8_t*)arr->data;
  ft->size = (uint_fast32_t)arr->size;
  ft->unitsPerEm = 0;
  ft->locaFormat = 0;
  ft->numLongHmtx = 0;
  if (init_font(ft) < 0) {
    return false;
  }
  return true;
}


static bool mk_font(SFT_Font *ft, lbm_value font_val) {
  lbm_value font_file = lbm_car(lbm_cdr(lbm_cdr(font_val)));

  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(font_file);
  ft->memory = (uint8_t*)arr->data;
  ft->size = (uint_fast32_t)arr->size;
  ft->unitsPerEm = 0;
  ft->locaFormat = 0;
  ft->numLongHmtx = 0;
  if (init_font(ft) < 0) {
    return false;
  }
  return true;
}


static SFT mk_sft(SFT_Font *ft, float x_scale, float y_scale) {
  SFT sft;
  sft.font = ft;
  sft.xScale = x_scale;
  sft.yScale = y_scale;
  sft.xOffset = 0;
  sft.yOffset = 0;
  sft.flags = SFT_DOWNWARD_Y;

  return sft;
}

lbm_value ext_ttf_glyph_dims(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      is_font_value(args[0]) &&
      lbm_is_number(args[1])) { // glyph id
    SFT_Font ft;
    if (!mk_font(&ft,args[0])) {
      return ENC_SYM_EERROR;
    }
    SFT sft = mk_sft(&ft, font_get_x_scale(args[0]), font_get_y_scale(args[0]));
    SFT_GMetrics gmtx;
    SFT_Glyph gid = lbm_dec_as_u32(args[1]);

    if (sft_gmetrics(&sft, gid, &gmtx) < 0) {
      res = ENC_SYM_EERROR;
      goto glyph_dims_done;
    }
    res = lbm_heap_allocate_list_init(2,
                                      lbm_enc_u((uint32_t)(gmtx.minWidth)), // + 3) & ~3)),
                                      lbm_enc_u((uint32_t)gmtx.minHeight));
  }
 glyph_dims_done:
  return res;
}

lbm_value ext_ttf_glyph_render(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  lbm_array_header_t *img_arr;
  if (argn == 3 &&
      (img_arr = get_image_buffer(args[0])) &&
      is_font_value(args[1]) &&
      lbm_is_number(args[2])) { // glyph id

    SFT_Font ft;
    if (!mk_font(&ft,args[1])) {
      return ENC_SYM_EERROR;
    }
    SFT sft = mk_sft(&ft, font_get_x_scale(args[1]), font_get_y_scale(args[1]));
    SFT_GMetrics gmtx;
    SFT_Glyph gid = lbm_dec_as_u32(args[2]);

    if (sft_gmetrics(&sft, gid, &gmtx) < 0) {
      res = ENC_SYM_EERROR;
      goto glyph_render_done;
    }

    image_buffer_t img;
    img.width = image_buffer_width((uint8_t*)img_arr->data);
    img.height = image_buffer_height((uint8_t*)img_arr->data);
    img.fmt = image_buffer_format((uint8_t*)img_arr->data);
    img.mem_base = (uint8_t*)img_arr->data;
    img.data = image_buffer_data((uint8_t*)img_arr->data);

    if ((img.width < gmtx.minWidth) || // ((gmtx.minWidth + 3) & ~3)) ||
        (img.height < gmtx.minHeight)) {
      res = ENC_SYM_EERROR;
      goto glyph_render_done;
    }
    int r = sft_render(&sft, gid, &img);
    if (r == SFT_MEM_ERROR) {
      res = ENC_SYM_MERROR;
    } else if (r < 0) {
      res = ENC_SYM_EERROR;
    }
    res = ENC_SYM_TRUE;
  }
 glyph_render_done:
  return res;
}

static lbm_value lookup_glyph_image(uint32_t gid, lbm_value ls) {
  lbm_value curr = ls;
  lbm_value res = ENC_SYM_NO_MATCH;
  while (lbm_is_cons(curr)) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if (lbm_is_cons(c)) {
      if (lbm_dec_as_u32(lbm_ref_cell(c)->car) == gid) { // regular equality should work fine!
        res = lbm_car(lbm_ref_cell(c)->cdr);
        break;
      }
    } else {
      res = ENC_SYM_EERROR;
      break;
    }
    curr = lbm_ref_cell(curr)->cdr;
  }
  return res;
}

lbm_value ext_ttf_wh(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  lbm_value font;
  uint32_t next_arg = 0;
  lbm_value utf8_str;
  if (argn >= 2 &&
             is_prepared_font_value(args[0]) &&
             lbm_is_array_r(args[1])) { // sequence of utf8 characters
    font = args[0];
    utf8_str = args[1];
    next_arg = 2;
  } else {
    return res;
  }

  float line_spacing = 1.0f;
  bool up = false;
  bool down = false;
  for (int i = next_arg; i < argn; i ++) {
    if (lbm_is_symbol(args[i])) {
      up = display_is_symbol_up(args[i]);
      down = display_is_symbol_down(args[i]);
    } else if (lbm_is_number(args[i])) {
      line_spacing = lbm_dec_as_float(args[i]);
    }
  }

  lbm_value r_list = lbm_heap_allocate_list(2);
  if (lbm_is_symbol(r_list)) return r_list;

  SFT_Font ft;
  if (!mk_font(&ft,font)) {
    return ENC_SYM_EERROR;
  }
  SFT sft = mk_sft(&ft, font_get_x_scale(font), font_get_y_scale(font));

  int x_pos = lbm_dec_as_i32(args[1]);
  int y_pos = lbm_dec_as_i32(args[2]);
  uint8_t *utf8 = (uint8_t*)lbm_dec_str(utf8_str);
  uint32_t i = 0;
  uint32_t next_i = 0;
  SFT_Glyph prev = 0;
  bool has_prev = false;
  uint32_t utf32;

  SFT_LMetrics lmtx;
  if (sft_lmetrics(&sft, &lmtx) < 0) {
    res = ENC_SYM_EERROR;
    goto ttf_wh_done;
  }
  lbm_value glyph_tab = lbm_index_list(font, 3);
  float x = 0.0;
  float y = 0.0;

  float max_x = 0.0;

  while (get_utf32(utf8, &utf32, i, &next_i)) {

    if (utf32 == '\n') {
      if (x > max_x) max_x = x;
      x = 0.0;
      y += line_spacing * (lmtx.ascender - lmtx.descender + lmtx.lineGap);
      i++;
      continue; // next iteration
    }

    SFT_Glyph gid;
    if (sft_lookup(&sft, utf32, &gid) < 0) {
      res = ENC_SYM_EERROR;
      goto ttf_wh_done;
    }
    SFT_GMetrics gmtx;
    if (sft_gmetrics(&sft, gid, &gmtx) < 0) {
      res = ENC_SYM_EERROR;
      goto ttf_wh_done;
    }

    lbm_value glyph = lookup_glyph_image(gid, glyph_tab);
    if (!(lbm_is_array_r(glyph) || lbm_is_symbol_nil(glyph))) {
      res = ENC_SYM_EERROR;
      goto ttf_wh_done;
    }

    float x_shift = 0;
    float y_shift = 0;
    if (has_prev) {
      SFT_Kerning kern;
      kern.xShift = 0.0;
      kern.yShift = 0.0;
      // silly kerning lookup by first trying to get
      // the kern from GPOS and if we are unable, we try kern table.
      if (sft.font->pairAdjustOffset) {
        sft_gpos_kerning(&sft, prev, gid, &kern);
      }
      if (kern.xShift == 0.0 && kern.yShift == 0.0) {
        sft_kerning(&sft, prev, gid, &kern);
      }

      x_shift = kern.xShift;
      y_shift = kern.yShift;
    }

    float x_n = x;
    float y_n = y;

    x_n += x_shift;
    y_n += y_shift;
    y_n += gmtx.yOffset;

    x = x_n + gmtx.advanceWidth;
    i = next_i;
    prev = gid;
    has_prev = true;
  }

  if (max_x < x) max_x = x;
  lbm_value rest = lbm_cdr(r_list);
  if (up || down) {
    lbm_set_car(r_list, lbm_enc_u((uint32_t)(y + line_spacing * (lmtx.ascender - lmtx.descender + lmtx.lineGap))));
    lbm_set_car(rest, lbm_enc_u((uint32_t)max_x));
  } else {
    lbm_set_car(r_list, lbm_enc_u((uint32_t)max_x));
    lbm_set_car(rest, lbm_enc_u((uint32_t)(y + line_spacing * (lmtx.ascender - lmtx.descender + lmtx.lineGap))));
  }
  return r_list;
 ttf_wh_done:
  return ENC_SYM_EERROR;
}

lbm_value ext_ttf_print(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  lbm_array_header_t *img_arr;
  lbm_value font;
  lbm_value utf8_str;
  uint32_t colors[16];
  uint32_t next_arg = 0;
  if (argn >= 6 &&
             (img_arr = get_image_buffer(args[0])) &&
             lbm_is_number(args[1]) && // x position
             lbm_is_number(args[2]) && // y position
             lbm_is_cons(args[3]) &&   // list of colors
             is_prepared_font_value(args[4]) &&
             lbm_is_array_r(args[5])) { // sequence of utf8 characters
    lbm_value curr = args[3];
    int i = 0;
    while(lbm_is_cons(curr) && i < 16) {
      colors[i] = lbm_dec_as_u32(lbm_car(curr));
      curr = lbm_cdr(curr);
      i ++;
    }
    font = args[4];
    utf8_str = args[5];
    next_arg = 6;
  } else {
    return res;
  }

  float line_spacing = 1.0f;
  bool up = false;
  bool down = false;
  for (int i = next_arg; i < argn; i ++) {
    if (lbm_is_symbol(args[i])) {
      up = display_is_symbol_up(args[i]);
      down = display_is_symbol_down(args[i]);
    } else if (lbm_is_number(args[i])) {
      line_spacing = lbm_dec_as_float(args[i]);
    }

  }

  SFT_Font ft;
  if (!mk_font(&ft,font)) {
    return ENC_SYM_EERROR;
  }
  SFT sft = mk_sft(&ft, font_get_x_scale(font), font_get_y_scale(font));

  image_buffer_t tgt;
  tgt.width = image_buffer_width((uint8_t*)img_arr->data);
  tgt.height = image_buffer_height((uint8_t*)img_arr->data);
  tgt.fmt = image_buffer_format((uint8_t*)img_arr->data);
  tgt.mem_base = (uint8_t*)img_arr->data;
  tgt.data = image_buffer_data((uint8_t*)img_arr->data);

  int x_pos = lbm_dec_as_i32(args[1]);
  int y_pos = lbm_dec_as_i32(args[2]);
  uint8_t *utf8 = (uint8_t*)lbm_dec_str(utf8_str);
  uint32_t i = 0;
  uint32_t next_i = 0;
  SFT_Glyph prev = 0;
  bool has_prev = false;
  uint32_t utf32;

  SFT_LMetrics lmtx;
  if (sft_lmetrics(&sft, &lmtx) < 0) {
    res = ENC_SYM_EERROR;
    goto ttf_print_done;
  }
  lbm_value glyph_tab = lbm_index_list(font, 3);
  float x = 0.0;
  float y = 0.0;

  while (get_utf32(utf8, &utf32, i, &next_i)) {

    if (utf32 == '\n') {
      x = 0.0;
      y += line_spacing * (lmtx.ascender - lmtx.descender + lmtx.lineGap);
      i++;
      continue; // next iteration
    }

    SFT_Glyph gid;
    if (sft_lookup(&sft, utf32, &gid) < 0) {
      res = ENC_SYM_EERROR;
      goto ttf_print_done;
    }
    SFT_GMetrics gmtx;
    if (sft_gmetrics(&sft, gid, &gmtx) < 0) {
      res = ENC_SYM_EERROR;
      goto ttf_print_done;
    }

    lbm_value glyph = lookup_glyph_image(gid, glyph_tab);
    if (!(lbm_is_array_r(glyph) || lbm_is_symbol_nil(glyph))) {
      res = ENC_SYM_EERROR;
      goto ttf_print_done;
    }

    float x_shift = 0;
    float y_shift = 0;
    if (has_prev) {
      SFT_Kerning kern;
      kern.xShift = 0.0;
      kern.yShift = 0.0;
      // silly kerning lookup by first trying to get
      // the kern from GPOS and if we are unable, we try kern table.
      if (sft.font->pairAdjustOffset) {
        sft_gpos_kerning(&sft, prev, gid, &kern);
      }
      if (kern.xShift == 0.0 && kern.yShift == 0.0) {
        sft_kerning(&sft, prev, gid, &kern);
      }

      x_shift = kern.xShift;
      y_shift = kern.yShift;
    }

    float x_n = x;
    float y_n = y;

    x_n += x_shift;
    y_n += y_shift;
    y_n += gmtx.yOffset;

    if (!lbm_is_symbol_nil(glyph)) { // whitespaces have no pre-rendered glyph
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(glyph);
      image_buffer_t src;
      src.width = image_buffer_width((uint8_t*)arr->data);
      src.height = image_buffer_height((uint8_t*)arr->data);
      src.fmt = image_buffer_format((uint8_t*)arr->data);
      src.mem_base = (uint8_t*)arr->data;
      src.data = image_buffer_data((uint8_t*)arr->data);

      uint32_t num_colors = 1 << src.fmt;

      for (int j = 0; j < src.height; j++) {
        for (int i = 0; i < src.width; i ++) {
          // the bearing should not be accumulated into the advances

          uint32_t p = getpixel(&src, i, j);
          if (p) { // only draw colored
            uint32_t c = colors[p & (num_colors-1)]; // ceiled

            if (up) {
              putpixel(&tgt, x_pos + (j + (int)y_n), y_pos - (i + (int)(x_n + gmtx.leftSideBearing)), c);
            } else if (down) {
              putpixel(&tgt, x_pos - (j + (int)y_n), y_pos + (i + (int)(x_n + gmtx.leftSideBearing)), c);
            } else {
              putpixel(&tgt, x_pos + (i + (int)(x_n + gmtx.leftSideBearing)), y_pos + (j + (int)y_n), c);
            }
          }
        }
      }
    }
    x = x_n + gmtx.advanceWidth;
    i = next_i;
    prev = gid;
    has_prev = true;
  }
  res = ENC_SYM_TRUE;
 ttf_print_done:
  return res;
}

// Extract a glyph starting at an index in a byte array (string)
lbm_value ext_ttf_glyph_id(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 3 &&
      is_font_value(args[0]) &&
      lbm_is_array_r(args[1]) &&
      lbm_is_number(args[2])) {
    res = ENC_SYM_EERROR;

    uint32_t ix = lbm_dec_as_u32(args[2]);

    SFT_Font ft;
    if (!mk_font(&ft, args[0])) {
      res = ENC_SYM_EERROR;
      goto glyph_id_done;
    }
    SFT sft = mk_sft(&ft, font_get_x_scale(args[0]),font_get_y_scale(args[0]));

    uint8_t *utf8 = (uint8_t*)lbm_dec_str(args[1]);
    uint32_t utf32 = 0;
    uint32_t next_ix = 0;
    if (get_utf32(utf8, &utf32, ix, &next_ix)) {
      SFT_Glyph gid;
      if (sft_lookup(&sft, utf32, &gid) < 0) {
        res = ENC_SYM_EERROR;
        goto glyph_id_done;
      }
      // If encoding gid fails, allocate list should too.
      lbm_value ls = lbm_heap_allocate_list_init(2,
                                                 lbm_enc_u32(gid),
                                                 lbm_enc_u(next_ix));
      res = ls;
    } else {
      res = ENC_SYM_NIL;
    }
  }
 glyph_id_done:
  return res;
}

lbm_value ext_ttf_line_height(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_prepared_font_value(args[0])) {
    SFT_Font ft;
    if (!mk_font(&ft, args[0])) {
      return ENC_SYM_EERROR;
    }
    SFT sft = mk_sft(&ft, font_get_x_scale(args[0]), font_get_y_scale(args[0]));
    SFT_LMetrics lmtx;
    if (sft_lmetrics(&sft, &lmtx) < 0) {
      res = ENC_SYM_EERROR;
    } else {
      res = lbm_enc_float(lmtx.ascender - lmtx.descender + lmtx.lineGap);
    }
  }
  return res;
}

lbm_value ext_ttf_ascender(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_prepared_font_value(args[0])) {
    SFT_Font ft;
    if (!mk_font(&ft, args[0])) {
      return ENC_SYM_EERROR;
    }
    SFT sft = mk_sft(&ft, font_get_x_scale(args[0]), font_get_y_scale(args[0]));
    SFT_LMetrics lmtx;
    if (sft_lmetrics(&sft, &lmtx) < 0) {
      res = ENC_SYM_EERROR;
    } else {
      res = lbm_enc_float(lmtx.ascender);
    }
  }
  return res;
}

lbm_value ext_ttf_descender(lbm_value *args, lbm_uint argn) {
    lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_prepared_font_value(args[0])) {
    SFT_Font ft;
    if (!mk_font(&ft, args[0])) {
      return ENC_SYM_EERROR;
    }
    SFT sft = mk_sft(&ft,font_get_x_scale(args[0]),font_get_y_scale(args[0]));
    SFT_LMetrics lmtx;
    if (sft_lmetrics(&sft, &lmtx) < 0) {
      res = ENC_SYM_EERROR;
    } else {
      res = lbm_enc_float(lmtx.descender);
    }
  }
  return res;
}

lbm_value ext_ttf_line_gap(lbm_value *args, lbm_uint argn) {
    lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_prepared_font_value(args[0])) {
    SFT_Font ft;
    if (!mk_font(&ft,args[0])) {
      return ENC_SYM_EERROR;
    }
    SFT sft = mk_sft(&ft,font_get_x_scale(args[0]),font_get_y_scale(args[0]));
    SFT_LMetrics lmtx;
    if (sft_lmetrics(&sft, &lmtx) < 0) {
      res = ENC_SYM_EERROR;
    } else {
      res = lbm_enc_float(lmtx.lineGap);
    }
  }
  return res;
}

// Binary prerendered font format
// contents:
// Line-metrics
// Kerning table
// Glyphs
//   Glyph-metrics

// Format font type identification
// uint16_t : 0x0             (rule out string)
// uint16_t : version number  (expect to not have to maintain different versions of this)
// uint32_t : 0x666F6E74      (magic number that makes us fairly sure the data is a font)

// format Line-metrics
// - "lmtx"
// - uint32 : size
// - float : ascender
// - float : descender
// - float : lineGap

// Format kerning table
// - "kern"
// - uint32 : kern-table-total-size
// - uint32 : numRows
// - kernTableRow[]

// 13 + (8 * 4) + (10 * 6)
// 13 + 16 + 60
// 29 + 60
// 89
// format kerning table row
// - UTF32 : leftGlyph
// - uint32 : numKernPairs
// - KernPair[]

// format KernPair
// - UTF32 : rightGlyph
// - float : xShift
// - float : yShift

// format glyph table
// "glyphs"
// uint32 : total size bytes
// - numberOfGlyphs
// - image format of glyphs
// - glyph[]

// format glyph
// - UTF32 : glyph
// - float : advanceWidth
// - float : leftSideBearing
// - int32 : yOffset          - can prob be int16
// - int32 : width            - can be int16
// - int32 : height           - can be int16
// - uint8[] : width * height number data

// If we are not bin searching then sorting the UTF32 codes is not needed.

#define FONT_MAX_ID_STRING_LENGTH   10
#define FONT_VERSION                0
#define FONT_MAGIC_STRING           "font"
#define FONT_LINE_METRICS_STRING    "lmtx"
#define FONT_KERNING_STRING         "kern"
#define FONT_GLYPHS_STRING          "glyphs"

// sizeof when used on string literals include the the terminating 0
#define FONT_PREAMBLE_SIZE          (sizeof(uint16_t) * 2 + sizeof(FONT_MAGIC_STRING))
#define FONT_LINE_METRICS_SIZE      (sizeof(uint32_t) + (sizeof(float) * 3) + sizeof(FONT_LINE_METRICS_STRING))

// "header sizes" excluding data payload
#define FONT_KERN_PAIR_SIZE         (4 + 4 + 4)
#define FONT_KERN_ROW_SIZE          (4 + 4)
#define FONT_KERN_TABLE_SIZE        (sizeof(FONT_KERNING_STRING) + 4 + 4)
#define FONT_GLYPH_TABLE_SIZE       (sizeof(FONT_GLYPHS_STRING) + 4 + 4)
#define FONT_GLYPH_SIZE             (4 + 4 + 4 + 4 + 4 + 4)

static int num_kern_pairs_row(SFT *sft, uint32_t utf32, uint32_t *codes, uint32_t num_codes) {

  int num = 0;

  SFT_Glyph lgid;
  if (sft_lookup(sft, utf32, &lgid) < 0) {
    return -1;
  }

  for (uint32_t i = 0; i < num_codes; i ++) {
    uint32_t right_utf32 = codes[i];
    SFT_Kerning kern;
    kern.xShift = 0.0;
    kern.yShift = 0.0;

    SFT_Glyph rgid;
    if (sft_lookup(sft, right_utf32, &rgid) < 0) {
      return -1;
    }

    if (sft->font->pairAdjustOffset) {
      sft_gpos_kerning(sft, lgid, rgid, &kern); //TODO: can it fail?
    }
    if (kern.xShift == 0.0 && kern.yShift == 0.0) {
      sft_kerning(sft, lgid, rgid, &kern); //TODO: can it fail?
    }
    if (kern.xShift != 0.0 || kern.yShift != 0.0) {
      num++;
    }
  }
  return num;
}

static bool kern_table_dims(SFT *sft, uint32_t *codes, uint32_t num_codes, int *rows, int *tot_pairs) {

  int num_rows = 0;
  int tot_kern_pairs = 0;
  for (uint32_t i = 0; i < num_codes; i ++) {
    int r = num_kern_pairs_row(sft, codes[i], codes, num_codes);
    if (r > 0) {
      num_rows ++;
      tot_kern_pairs += r;
    } else if (r < 0) {
      return false;
    }
  }
  *rows = num_rows;
  *tot_pairs = tot_kern_pairs;
  return true;
}

static int kern_table_size_bytes(SFT *sft, uint32_t *codes, uint32_t num_codes) {
  int rows = 0;
  int tot_pairs = 0;

  int size_bytes;
  if (kern_table_dims(sft, codes, num_codes, &rows, &tot_pairs)) {
    size_bytes =
      FONT_KERN_PAIR_SIZE * tot_pairs +
      FONT_KERN_ROW_SIZE * rows +
      FONT_KERN_TABLE_SIZE;
  } else {
    return -1;
  }
  return size_bytes;
}


static void buffer_append_string(uint8_t *buffer, char *str, int32_t *index) {
  size_t n = strlen(str);
  memcpy(&buffer[*index], str, n + 1); // include the 0
  *index = *index + n + 1;
}

static void buffer_append_font_preamble(uint8_t *buffer, int32_t *index) {
  buffer_append_uint16(buffer, 0, index); // 2 leading zero bytes
  buffer_append_uint16(buffer, 0, index); // version 0
  buffer_append_string(buffer, FONT_MAGIC_STRING, index);
}

static void buffer_append_line_metrics(uint8_t *buffer, float ascender, float descender, float line_gap, int32_t *index) {
  buffer_append_string(buffer, FONT_LINE_METRICS_STRING, index);
  buffer_append_uint32(buffer, sizeof(float) * 3, index);
  buffer_append_float32_auto(buffer, ascender, index);
  buffer_append_float32_auto(buffer, descender, index);
  buffer_append_float32_auto(buffer, line_gap, index);
}


static bool buffer_append_kerning_table(uint8_t *buffer, SFT *sft, uint32_t *codes, uint32_t num_codes, int32_t *index) {

  int num_rows = 0;
  int tot_pairs = 0;

  if (kern_table_dims(sft, codes, num_codes, &num_rows, &tot_pairs)) {

    printf("total pairs: %d = %d\n", tot_pairs, FONT_KERN_PAIR_SIZE * tot_pairs);
    printf("num rows: %d = %d\n", num_rows, FONT_KERN_ROW_SIZE * num_rows);
    printf("kern tab: %d\n", FONT_KERN_TABLE_SIZE);

    // TODO: compute size of "payload" only
    uint32_t size_bytes =
      FONT_KERN_PAIR_SIZE * tot_pairs +
      FONT_KERN_ROW_SIZE * num_rows +
      + 4; // number of rows field
    printf("size bytes: %d\n", size_bytes);

    buffer_append_string(buffer, FONT_KERNING_STRING, index);
    buffer_append_uint32(buffer, size_bytes, index); // distance to jump ahead from index if not interested in kerning.
    buffer_append_uint32(buffer, num_rows, index);

    for (uint32_t left_ix = 0; left_ix < num_codes; left_ix ++) { // loop over all codes
      int32_t row_len = num_kern_pairs_row(sft, codes[left_ix], codes, num_codes);
      if ( row_len > 0) {
        printf("row_len %d\n", row_len);
        SFT_Glyph lgid;
        if (sft_lookup(sft, codes[left_ix], &lgid) < 0) {
          return false;
        }

        // format kerning table row
        // - UTF32 : leftGlyph
        // - uint32 : numKernPairs
        // - KernPair[]

        buffer_append_uint32(buffer, codes[left_ix],index);
        buffer_append_uint32(buffer, row_len, index);

        for (uint32_t right_ix = 0; right_ix < num_codes; right_ix ++) { // and all codes
          uint32_t right_utf32 = codes[right_ix];
          SFT_Kerning kern;
          kern.xShift = 0.0;
          kern.yShift = 0.0;

          // format KernPair
          // - UTF32 : rightGlyph
          // - float : xShift
          // - float : yShift

          SFT_Glyph rgid;
          if (sft_lookup(sft, right_utf32, &rgid) < 0) {
            return false;
          }

          if (sft->font->pairAdjustOffset) {
            sft_gpos_kerning(sft, lgid, rgid, &kern); //TODO: can it fail?
          }
          if (kern.xShift == 0.0 && kern.yShift == 0.0) {
            sft_kerning(sft, lgid, rgid, &kern); //TODO: can it fail?
          }
          if (kern.xShift != 0.0 || kern.yShift != 0.0) {
            buffer_append_uint32(buffer, right_utf32, index);
            buffer_append_float32_auto(buffer, kern.xShift, index);
            buffer_append_float32_auto(buffer, kern.yShift, index);
          }
        }
      }
    }
  }
  return true;
}

int glyphs_img_data_size(SFT *sft, color_format_t fmt, uint32_t *codes, uint32_t num_codes) {
  int total_size = 0;
  for (uint32_t i = 0; i < num_codes; i ++) {
    SFT_Glyph gid;
    if (sft_lookup(sft, codes[i], &gid) < 0)  return -1;
    SFT_GMetrics gmtx;
    if (sft_gmetrics(sft, gid, &gmtx) < 0) return -1;
    total_size += image_dims_to_size_bytes(fmt, gmtx.minWidth, gmtx.minHeight);
  }
  return total_size;
}

static int buffer_append_glyph(uint8_t *buffer, SFT *sft, color_format_t fmt, uint32_t utf32, int32_t *index){
  SFT_Glyph gid;
  if (sft_lookup(sft, utf32, &gid) < 0)  return -1;
  SFT_GMetrics gmtx;
  if (sft_gmetrics(sft, gid, &gmtx) < 0) return -1;

  buffer_append_uint32(buffer, utf32, index);
  buffer_append_float32_auto(buffer, gmtx.advanceWidth, index);
  buffer_append_float32_auto(buffer, gmtx.leftSideBearing, index);
  buffer_append_int32(buffer,gmtx.yOffset,index);
  buffer_append_int32(buffer,gmtx.minWidth, index);
  buffer_append_int32(buffer,gmtx.minHeight, index);

  image_buffer_t img;
  img.width = gmtx.minWidth;
  img.height = gmtx.minHeight;
  img.fmt = fmt;
  img.mem_base = &buffer[*index];
  img.data = &buffer[*index];

  int r = sft_render(sft, gid, &img);
  *index += image_dims_to_size_bytes(fmt, gmtx.minWidth, gmtx.minHeight);
  return r;
}

static int buffer_append_glyph_table(uint8_t *buffer, SFT *sft, color_format_t fmt, uint32_t *codes, uint32_t num_codes, int32_t *index) {

  int size_bytes =
    4 + // number of glyphs
    4 + // image format
    num_codes * 24 + // glyph metrics
    glyphs_img_data_size(sft,fmt,codes,num_codes);

  buffer_append_string(buffer, FONT_GLYPHS_STRING, index);
  buffer_append_uint32(buffer, size_bytes, index); // distance to jump ahead from index if not interested in kerning.
  buffer_append_uint32(buffer, num_codes, index);
  buffer_append_uint32(buffer, (uint32_t)fmt, index);

  int r = 0;
  for (uint32_t i = 0; i < num_codes; i ++) {
    r = buffer_append_glyph(buffer,sft,fmt,codes[i], index);
    if (r < 0) return r;
  }
  return r;
}

//returns the increment for n
static int insert_nub(uint32_t *arr, uint32_t n, uint32_t new_elt) {
  uint32_t i;
  for (i = 0; i < n; i ++) {
    if (arr[i] == new_elt) return 0;
    if (arr[i] > new_elt) {
      memmove(&arr[i+1], &arr[i], (n - i) * 4);
      arr[i] = new_elt;
      return 1;
    }
  }
  arr[i] = new_elt;
  return 1;
}

// (ttf-prepare-bin font font-scale img-fmt chars-string)
lbm_value ext_ttf_prepare_bin(lbm_value *args, lbm_uint argn) {
  if (argn == 4 &&
      lbm_is_array_r(args[0]) && // font file data
      lbm_is_number(args[1])  &&
      lbm_is_symbol(args[2])  &&
      lbm_is_array_r(args[3])) {

    float x_scale = lbm_dec_as_float(args[1]);
    float y_scale = x_scale;

    color_format_t fmt = sym_to_color_format(args[2]);

    lbm_value result_array_cell = lbm_heap_allocate_cell(LBM_TYPE_CONS, ENC_SYM_NIL, ENC_SYM_ARRAY_TYPE);

    if (result_array_cell == ENC_SYM_MERROR) return result_array_cell;
    lbm_array_header_t *result_array_header = (lbm_array_header_t *)lbm_malloc(sizeof(lbm_array_header_t));
    if (!result_array_header) return ENC_SYM_MERROR;

    lbm_array_header_t *utf8_array_header = (lbm_array_header_t*)(lbm_car(args[3]));

    // Try to keep the utf8 array as nubbed as possible or there will be waste of mem.
    // Unfortunate dynamic tmp storage...
    uint32_t* unique_utf32 = lbm_malloc(utf8_array_header->size * sizeof(uint32_t));

    if (unique_utf32) {

      SFT_Font ft;
      if (!mk_font_raw(&ft,args[0])) {
        lbm_free(unique_utf32);
        return ENC_SYM_EERROR;
      }
      SFT sft = mk_sft(&ft, x_scale, y_scale);

      // We know which glyphs to prerender...
      // So time to start collecting information to put into the binary prerender format
      // and to figure out how much prerender space to allocate!

      uint32_t i = 0;
      uint32_t next_i = 0;
      uint32_t utf32;
      int n = 0;

      while (get_utf32(utf8_array_header->data, &utf32, i, &next_i)) {
        n += insert_nub(unique_utf32, n, utf32);
        i = next_i;
      }

      int kern_tab_bytes = kern_table_size_bytes(&sft, unique_utf32, n);
      if (kern_tab_bytes > 0) {
        printf("kern table size bytes: %d\n", kern_tab_bytes);
      } else {
        lbm_free(unique_utf32);
        return ENC_SYM_MERROR;
      }

      int glyph_gfx_size = glyphs_img_data_size(&sft, fmt, unique_utf32, n);
      if (glyph_gfx_size > 0) {
        printf("glyph gfx size: %d\n", glyph_gfx_size);
      } else {
        lbm_free(unique_utf32);
        return ENC_SYM_EERROR;
      }

      uint32_t bytes_required =
        FONT_PREAMBLE_SIZE +
        FONT_LINE_METRICS_SIZE +
        kern_tab_bytes +
        FONT_GLYPH_TABLE_SIZE +
        n * 24 + // glyph metrics
        glyph_gfx_size + 50; // TODO: CALCULATE THIS CORRECTLY

      uint8_t *buffer = (uint8_t*)lbm_malloc(bytes_required);
      if (!buffer) {
        lbm_free(unique_utf32);
        return ENC_SYM_MERROR;
      }
      memset(buffer,0, bytes_required);

      SFT_LMetrics lmtx;
      if (sft_lmetrics(&sft, &lmtx) < 0) {
        lbm_free(unique_utf32);
        return ENC_SYM_EERROR;
      }
      int32_t index = 0;

      buffer_append_font_preamble(buffer, &index);
      buffer_append_line_metrics(buffer,
                                 lmtx.ascender,
                                 lmtx.descender,
                                 lmtx.lineGap,
                                 &index);
      buffer_append_kerning_table(buffer, &sft, unique_utf32, n, &index);

      int r = buffer_append_glyph_table(buffer, &sft, fmt, unique_utf32, n, &index);
      if ( r == SFT_MEM_ERROR) {
        lbm_free(unique_utf32);
        lbm_free(buffer);
        lbm_set_car_and_cdr(result_array_cell, ENC_SYM_NIL, ENC_SYM_NIL);
        return ENC_SYM_MERROR;
      } else if (r < 0) {
        printf("error in buffer_append_glyph_table\n");
        lbm_free(unique_utf32);
        lbm_free(buffer);
        lbm_set_car_and_cdr(result_array_cell, ENC_SYM_NIL, ENC_SYM_NIL);
        return ENC_SYM_EERROR;
      }

      lbm_free(unique_utf32); // tmp data nolonger needed
      result_array_header->size = index;
      result_array_header->data = buffer;
      lbm_set_car(result_array_cell, (lbm_uint)result_array_header);
      result_array_cell = lbm_set_ptr_type(result_array_cell, LBM_TYPE_ARRAY);
      return result_array_cell;
    } else {
      return ENC_SYM_MERROR;
    }
  }
  return ENC_SYM_TERROR;
}

bool buffer_get_font_preamble(uint8_t* buffer, uint16_t *version, int32_t *index) {

  int16_t zero = buffer_get_uint16(buffer, index);
  if (zero == 0) {
    *version = buffer_get_uint16(buffer, index);
    if (strncmp(&buffer[*index], "font", 4) == 0) {
      *index += sizeof("font"); // includes 0 for constant string
      return true;
    }
  }
  return false;
}

static bool font_get_line_metrics(uint8_t *buffer, int32_t buffer_size, float *ascender, float *descender, float *line_gap ,int32_t index) {

  while(index < buffer_size) {
    char *str = &buffer[index];
    if (strncmp(str, "lmtx", 4) == 0) {
      int32_t i = index + 5 + 4; // skip over string and size field;
      *ascender = buffer_get_float32_auto(buffer, &i);
      *descender = buffer_get_float32_auto(buffer, &i);
      *line_gap = buffer_get_float32_auto(buffer, &i);
      return true;
    }
    index += strlen(str) + 1;
    index += buffer_get_uint32(buffer,&index); // just to next position
  }
  return false;
}

static bool font_get_kerning_table_index(uint8_t *buffer, int32_t buffer_size, int32_t *res_index, int32_t index) {

  while (index < buffer_size) {
    char *str = &buffer[index];
    if (strncmp(str, "kern", 4) == 0) {
      *res_index = index + 5 + 4;
      return true;
    }
    index += strlen(str) + 1;
    index += buffer_get_uint32(buffer,&index); // jump to next position
  }
  return false;
}

static bool font_get_glyphs_table_index(uint8_t *buffer, int32_t buffer_size, int32_t *res_index, uint32_t *num_codes, uint32_t *fmt, int32_t index) {
  while (index < buffer_size) {
    char *str = &buffer[index];
    if (strncmp(str, "glyphs", 6) == 0) {
      int32_t i = index + 7 + 4;
      *num_codes = buffer_get_uint32(buffer,&i);
      *fmt = buffer_get_uint32(buffer,&i);
      *res_index = i;
      return true;
    }
    index += strlen(str) + 1;
    index += buffer_get_uint32(buffer,&index);
  }
  return false;
}

static bool font_get_glyph(uint8_t *buffer,
                           int32_t buffer_size,
                           float *advance_width,
                           float *left_side_bearing,
                           int32_t *y_offset,
                           int32_t *width,
                           int32_t *height,
                           uint8_t **gfx,
                           uint32_t utf32,
                           uint32_t num_codes,
                           color_format_t fmt,
                           int32_t index) {

  uint32_t i = 0;
  printf("num_codes = %d\n",num_codes);
  while (i < num_codes) {
    uint32_t c = buffer_get_uint32(buffer, &index);
    printf("looking for %d, at %d\n", utf32, c);
    if (c == utf32) {
      *advance_width = buffer_get_float32_auto(buffer, &index);
      *left_side_bearing = buffer_get_float32_auto(buffer, &index);
      *y_offset = buffer_get_int32(buffer, &index);
      *width = buffer_get_int32(buffer, &index);
      *height = buffer_get_int32(buffer,&index);
      *gfx = &buffer[index];
      return true;
    } else {
      index += 12;
      int32_t w = buffer_get_int32(buffer, &index);
      int32_t h = buffer_get_int32(buffer, &index);
      index += image_dims_to_size_bytes(fmt, w, h);
    }
    i++;
  }
  return false;
}

lbm_value ttf_text_bin(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  lbm_array_header_t *img_arr;
  lbm_value font;
  char *utf8_str;
  uint32_t colors[16];
  uint32_t next_arg = 0;
  if (argn >= 6 &&
      (img_arr = get_image_buffer(args[0])) &&
      lbm_is_number(args[1]) &&  // x position
      lbm_is_number(args[2]) &&  // y position
      lbm_is_cons(args[3]) &&    // list of colors
      lbm_is_array_r(args[4]) && // Binary font
      lbm_is_array_r(args[5])) { // sequence of utf8 characters
    lbm_value curr = args[3];
    int i = 0;
    while(lbm_is_cons(curr) && i < 16) {
      colors[i] = lbm_dec_as_u32(lbm_car(curr));
      curr = lbm_cdr(curr);
      i ++;
    }
    font = args[4];
    utf8_str = lbm_dec_str(args[5]);
    next_arg = 6;
  } else {
    return res;
  }

  int x_pos = lbm_dec_as_i32(args[1]);
  int y_pos = lbm_dec_as_i32(args[2]);

  float line_spacing = 1.0f;
  bool up = false;
  bool down = false;
  for (int i = next_arg; i < argn; i ++) {
    if (lbm_is_symbol(args[i])) {
      up = display_is_symbol_up(args[i]);
      down = display_is_symbol_down(args[i]);
    } else if (lbm_is_number(args[i])) {
      line_spacing = lbm_dec_as_float(args[i]);
    }
  }

  lbm_array_header_t *font_arr = lbm_dec_array_r(font);
  if (font_arr->size < 10) return ENC_SYM_EERROR;

  int32_t index = 0;
  uint16_t version;

  if (buffer_get_font_preamble(font_arr->data, &version, &index)) {
    printf("font preamble ok: %d\n", version);
  } else {
    printf("font preamble is NOT ok!\n");
  }

  float ascender;
  float descender;
  float line_gap;

  if(font_get_line_metrics(font_arr->data, font_arr->size, &ascender, &descender, &line_gap , index)) {
    printf("Line metrix ok: %f, %f, %f\n", ascender, descender, line_gap);
  } else {
    printf("error getting line metrix\n");
  }


  int32_t kern_index;

  if (font_get_kerning_table_index(font_arr->data, font_arr->size, &kern_index, index)) {
    printf("Kerning table index found!\n");
  } else {
    printf("error getting kerning table index\n");
  }

  int32_t glyphs_index;
  uint32_t num_codes;
  uint32_t color_fmt;

  if (font_get_glyphs_table_index(font_arr->data, font_arr->size, &glyphs_index, &num_codes, &color_fmt, index)) {
    printf("Glyph table index found! %u : %u\n", num_codes, color_fmt);
  } else {
    printf("error getting glyph table index\n");
  }

  color_format_t fmt = (color_format_t)color_fmt;
  float x = 0.0;
  float y = 0.0;

  image_buffer_t tgt;
  tgt.width = image_buffer_width((uint8_t*)img_arr->data);
  tgt.height = image_buffer_height((uint8_t*)img_arr->data);
  tgt.fmt = image_buffer_format((uint8_t*)img_arr->data);
  tgt.mem_base = (uint8_t*)img_arr->data;
  tgt.data = image_buffer_data((uint8_t*)img_arr->data);

  uint32_t utf32;
  uint32_t i = 0;
  uint32_t next_i = 0;
  while (get_utf32(utf8_str, &utf32, i, &next_i)) {
    if (utf32 == '\n') {
      i++;
      continue; // next iteration
    }

    float x_n = x;
    float y_n = y;

    float advance_width;
    float left_side_bearing;
    int32_t y_offset;
    int32_t width;
    int32_t height;
    uint8_t *gfx;

    if (font_get_glyph(font_arr->data,
                       font_arr->size,
                       &advance_width,
                       &left_side_bearing,
                       &y_offset,
                       &width,
                       &height,
                       &gfx,
                       utf32,
                       num_codes,
                       fmt,
                       glyphs_index)) {
      //x_n += x_shift;
      //y_n += y_shift;
      y_n += y_offset;

      image_buffer_t src;
      src.width = width;
      src.height = height;
      src.fmt = fmt;
      //src.mem_base = gfx;
      src.data = gfx;

      uint32_t num_colors = 1 << src.fmt;
      for (int j = 0; j < src.height; j++) {
        for (int i = 0; i < src.width; i ++) {
          // the bearing should not be accumulated into the advances

          uint32_t p = getpixel(&src, i, j);
          if (p) { // only draw colored
            uint32_t c = colors[p & (num_colors-1)]; // ceiled
            if (up) {
              putpixel(&tgt, x_pos + (j + (int)y_n), y_pos - (i + (int)(x_n + left_side_bearing)), c);
            } else if (down) {
              putpixel(&tgt, x_pos - (j + (int)y_n), y_pos + (i + (int)(x_n + left_side_bearing)), c);
            } else {
              putpixel(&tgt, x_pos + (i + (int)(x_n + left_side_bearing)), y_pos + (j + (int)y_n), c);
            }
          }
        }
      }
    } else {
      printf("GLYPH NOT FOUND\n");
    }
    x = x_n + advance_width;
    i = next_i;
  }
  return ENC_SYM_TRUE;
}

void lbm_ttf_extensions_init(void) {

  // metrics
  lbm_add_extension("ttf-line-height", ext_ttf_line_height);
  lbm_add_extension("ttf-ascender", ext_ttf_ascender);
  lbm_add_extension("ttf-descender", ext_ttf_descender);
  lbm_add_extension("ttf-line-gap", ext_ttf_line_gap);
  lbm_add_extension("ttf-text-dims",ext_ttf_wh);

  // Low level utilities
  lbm_add_extension("ttf-prepare-bin", ext_ttf_prepare_bin);
  lbm_add_extension("ttf-glyph-dims",ext_ttf_glyph_dims);
  lbm_add_extension("ttf-glyph-render", ext_ttf_glyph_render);
  lbm_add_extension("ttf-glyph-id", ext_ttf_glyph_id);

  // Draw text.
  lbm_add_extension("ttf-text-bin", ttf_text_bin);
  lbm_add_extension("ttf-text",ext_ttf_print);
}
