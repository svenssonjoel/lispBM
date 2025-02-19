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

void lbm_ttf_extensions_init(void) {

  // metrics
  lbm_add_extension("ttf-line-height", ext_ttf_line_height);
  lbm_add_extension("ttf-ascender", ext_ttf_ascender);
  lbm_add_extension("ttf-descender", ext_ttf_descender);
  lbm_add_extension("ttf-line-gap", ext_ttf_line_gap);

  // Low level utilities
  lbm_add_extension("ttf-glyph-dims",ext_ttf_glyph_dims);
  lbm_add_extension("ttf-glyph-render", ext_ttf_glyph_render);
  lbm_add_extension("ttf-glyph-id", ext_ttf_glyph_id);

  // Create font and draw text.
  lbm_add_extension("ttf-text",ext_ttf_print);
}
