/*
    Copyright 2026 Joel Svensson    svenssonjoel@yahoo.se

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

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <emscripten.h>

#include "lispbm.h"
#include "lbm_image.h"
#include "extensions/array_extensions.h"
#include "extensions/display_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/runtime_extensions.h"
#include "extensions/random_extensions.h"
#include "extensions/set_extensions.h"
#include "extensions/lbm_dyn_lib.h"
#include "extensions/crypto_extensions.h"
#include "extensions/dsp_extensions.h"
#include "extensions/ecc_extensions.h"

#include "lbm_custom_type.h"

#define HEAP_SIZE              (1 << 14)
#define GC_STACK_SIZE          256
#define PRINT_STACK_SIZE       256
#define EXTENSION_STORAGE_SIZE 512
#define LBM_MEMORY_BLOCKS      131072  // ~10K  (* (* 160 16) 4) 10240
                                       //  64K  (* (* 1024 16) 4) 65536
                                       // 256K  (* (* 4096 16) 4) 262144
                                       // 512K  (* (* 8192 16) 4) 655360
                                       //   1M  (* (* 16384 16) 4) 1048576
                                       //   8M  (* (* 131072 16) 4) 8388608
#define LBM_MEMORY_SIZE        LBM_MEMORY_SIZE_BLOCKS_TO_WORDS(LBM_MEMORY_BLOCKS)
#define LBM_BITMAP_SIZE        LBM_MEMORY_BITMAP_SIZE(LBM_MEMORY_BLOCKS)
#define OUTPUT_BUFFER_SIZE     65536
#define CTX_LIST_BUFFER_SIZE   4096
#define STATS_BUFFER_SIZE      512
#define IMAGE_STORAGE_SIZE     (128 * 1024)


// Feature wishlist:
// - Tab that renders manual in markdown
// - An example browser tab where clicking an example opens it in an editor tab.
// - A version of the dsp_lang that works with the wasm plotting. Add to libs dir.
// - Making tabs (and perhaps other browser entities) first class citizens in
//   the users lisp program. So that we could create tabs in code
//   - Ploting can take an additional tab argument so we can choose into what tab to plot.
// - Data export in CSV format (or matlad, octave suitable formats)
// - Png export (to quickly get a pic of the graph).
// - "Image" canvas for drawing using the display library
// - Encode entire state in base64 and enable sharable links replicating full editor state.
//      Save code editor contents and maybe active plot tabs ? not entire runtime system state. 
// - Import that does not use the "deprecated" XHR approach. Needs investigation to find best approach.
// - Lisp image saving and restoring to/from binary file (requires image to be at fixed
//      address accross instances of the wasm rts.


static lbm_cons_t      heap[HEAP_SIZE];
static lbm_uint        lbm_memory[LBM_MEMORY_SIZE];
static lbm_uint        lbm_bitmap[LBM_BITMAP_SIZE];
static lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];

static uint32_t image_storage[IMAGE_STORAGE_SIZE / sizeof(uint32_t)];

static bool wasm_image_write(uint32_t w, int32_t ix, bool is_const_heap) {
  (void)is_const_heap;
  image_storage[ix] = w;
  return true;
}

static char output_buffer[OUTPUT_BUFFER_SIZE];
static int  output_pos = 0;

static char ctx_list_buffer[CTX_LIST_BUFFER_SIZE];
static char stats_buffer[STATS_BUFFER_SIZE];

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t         string_tok;

typedef struct reader_s {
  char            *str;
  lbm_cid          cid;
  struct reader_s *next;
} reader_t;

static reader_t *readers = NULL;

static void add_reader(char *str, lbm_cid cid) {
  reader_t *r = (reader_t*)malloc(sizeof(reader_t));
  if (!r) return;
  r->str  = str;
  r->cid  = cid;
  r->next = readers;
  readers = r;
}

static void drop_reader(lbm_cid cid) {
  reader_t *prev = NULL;
  reader_t *curr = readers;
  while (curr) {
    if (curr->cid == cid) {
      if (prev) prev->next = curr->next;
      else      readers    = curr->next;
      free(curr->str);
      free(curr);
      return;
    }
    prev = curr;
    curr = curr->next;
  }
}

// ////////////////////////////////////////////////////////////
//   Callbacks
//

static int print_callback(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int written = vsnprintf(output_buffer + output_pos,
                          OUTPUT_BUFFER_SIZE - output_pos - 1,
                          fmt, args);
  va_end(args);
  if (written > 0) {
    output_pos += written;
    if (output_pos >= OUTPUT_BUFFER_SIZE - 1)
      output_pos = OUTPUT_BUFFER_SIZE - 1;
    output_buffer[output_pos] = '\0';
  }
  return written;
}

static void critical_callback(void) {
  print_callback("CRITICAL ERROR\n");
}

// ////////////////////////////////////////////////////////////
// Extensions
//

static bool dynamic_loader(const char *sym, const char **code) {
  return lbm_dyn_lib_find(sym, code);
}

// Systime in whole number of ms.
// The type of emscripted_get_now() is a double but
// browsers may provide only 1ms resolution (a security measure).
static lbm_value ext_systime(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_u32((uint32_t)emscripten_get_now());
}

static lbm_value ext_secs_since(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  uint32_t t0   = lbm_dec_as_u32(args[0]);
  uint32_t now  = (uint32_t)emscripten_get_now();
  uint32_t diff = now - t0;
  return lbm_enc_float((float)diff / 1000.0f);
}

static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  char buf[256];
  for (lbm_uint i = 0; i < argn; i++) {
    lbm_print_value(buf, sizeof(buf), args[i]);
    print_callback("%s", buf);
  }
  print_callback("\n");
  return ENC_SYM_TRUE;
}

/* EM_JS(void, js_plot_slot, (int slot, int nbytes, const char *title), { */
/*   if (typeof window.createPlotTab === 'function') { */
/*     window.createPlotTab(slot, nbytes, UTF8ToString(title)); */
/*   } */
/* }); */

EM_JS(void, js_plot_bufs, (const char *bufs_json, const char *title), {
  if (typeof window.createMultiPlotTab === 'function') {
    window.createMultiPlotTab(UTF8ToString(bufs_json), UTF8ToString(title));
  }
});

EM_JS(void, js_plot_buf, (uint8_t *buffer, int nbytes, const char *title), {
    if (typeof window.createPlotTab === 'function') {
      window.createPlotTab(buffer, nbytes, UTF8ToString(title));
    }
  });

EM_JS(void, js_plot_xy, (uint8_t *xbuf, int xbytes, uint8_t *ybuf, int ybytes, const char *title), {
  if (typeof window.createXYPlotTab === 'function') {
    window.createXYPlotTab(xbuf, xbytes, ybuf, ybytes, UTF8ToString(title));
  }
});


// Count how many open editor tabs have the given filename.
EM_JS(int, js_count_tab_matches, (const char *filename), {
  if (typeof window.countEditorTabMatches !== 'function') return 0;
  return window.countEditorTabMatches(UTF8ToString(filename));
});

// Check if an editor tab with the given filename is open; return its content.
EM_JS(char*, js_get_tab_content, (const char *filename), {
  if (typeof window.getEditorTabContent !== 'function') return 0;
  const content = window.getEditorTabContent(UTF8ToString(filename));
  if (content === null) return 0;
  const len = lengthBytesUTF8(content) + 1;
  const buf = _malloc(len);
  if (!buf) return 0;
  stringToUTF8(content, buf, len);
  return buf;
});

// Javascript function callable from C.
// defines:
//  char *js_import_lib(const char *filename);
//  which is the C "binding" for the javascript body below.
EM_JS(char*, js_import_lib, (const char *filename), {
  const url = 'libs/' + UTF8ToString(filename);
  const xhr = new XMLHttpRequest();
  xhr.open('GET', url, false);
  try {
    xhr.send(null);
  } catch(e) {
    return 0;
  }
  if (xhr.status !== 200) return 0;
  const str = xhr.responseText;
  const len = lengthBytesUTF8(str) + 1;
  const buf = _malloc(len);
  if (!buf) return 0;
  stringToUTF8(str, buf, len);
  return buf;
});


// (wasm-plot buf "Title")
// 
static lbm_value ext_wasm_plot(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;

  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    const char *title = lbm_dec_str(args[1]);
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(args[0]);
    js_plot_buf((uint8_t*)array->data,array->size,  title);
    res = ENC_SYM_TRUE;

  }  
  return res;
}

// (wasm-plot-multi '(buf1 buf2 ...) "Title")
// signal JS to create a multi-series plot tab from LispBM byte arrays
static lbm_value ext_wasm_plot_multi(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_cons(args[0])) return ENC_SYM_TERROR;
  char json[512];
  int pos = 0;
  pos += snprintf(json + pos, (int)sizeof(json) - pos, "[");
  lbm_value lst = args[0];
  int first = 1;
  while (lbm_is_cons(lst)) {
    lbm_value head = lbm_car(lst);
    if (!lbm_is_array_r(head)) return ENC_SYM_TERROR;
    lbm_array_header_t *hdr = (lbm_array_header_t*)lbm_car(head);
    if (!first) pos += snprintf(json + pos, (int)sizeof(json) - pos, ",");
    pos += snprintf(json + pos, (int)sizeof(json) - pos,
                   "{\"ptr\":%u,\"nbytes\":%u}", (unsigned int)(uintptr_t)hdr->data, (unsigned int)hdr->size);
    first = 0;
    lst = lbm_cdr(lst);
  }
  snprintf(json + pos, (int)sizeof(json) - pos, "]");
  const char *title = "";
  if (argn >= 2 && lbm_is_array_r(args[1])) {
    title = lbm_dec_str(args[1]);
  }
  js_plot_bufs(json, title);
  return ENC_SYM_TRUE;
}

// (wasm-plot-xy x-buf y-buf "Title")
static lbm_value ext_wasm_plot_xy(lbm_value *args, lbm_uint argn) {
  if (argn == 3 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1]) &&
      lbm_is_array_r(args[2])) {
    const char *title = lbm_dec_str(args[2]);
    lbm_array_header_t *xarr = (lbm_array_header_t*)lbm_car(args[0]);
    lbm_array_header_t *yarr = (lbm_array_header_t*)lbm_car(args[1]);
    js_plot_xy((uint8_t*)xarr->data, xarr->size,
               (uint8_t*)yarr->data, yarr->size, title);
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_TERROR;
}

// (import library-file-name sym)
static lbm_value ext_import(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_array_r(args[0]) || !lbm_is_symbol(args[1])) return ENC_SYM_TERROR;
  const char *filename = lbm_dec_str(args[0]);
  if (!filename) return ENC_SYM_TERROR;
  const char *symname = lbm_get_name_by_symbol(lbm_dec_sym(args[1]));
  if (!symname) return ENC_SYM_TERROR;
  int matches = js_count_tab_matches(filename);
  if (matches > 1) {
    print_callback("import: %d open tabs named \"%s\", using first match\n", matches, filename);
  }
  char *code = js_get_tab_content(filename);
  if (!code) code = js_import_lib(filename);
  if (!code) return ENC_SYM_NIL;
  lbm_uint len = (lbm_uint)strlen(code);
  lbm_value result;
  if (!lbm_create_array(&result, len + 1)) {
    free(code);
    return ENC_SYM_MERROR;
  }
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(result);
  memcpy(arr->data, code, len + 1);
  free(code);
  lbm_define((char*)symname, result);
  return result;
}

EM_JS(int, js_create_canvas_tab, (int w, int h, const char *title), {
  if (typeof window.createCanvasTab !== 'function') return -1;
  return window.createCanvasTab(w, h, UTF8ToString(title));
});

EM_JS(void, js_canvas_put_image, (int canvas_id, uint8_t *rgba, int w, int h, int x, int y), {
  if (typeof window.canvasPutImage === 'function') {
    window.canvasPutImage(canvas_id, rgba, w, h, x, y);
  }
});

EM_JS(void, js_canvas_clear_js, (int canvas_id, uint32_t color), {
  if (typeof window.canvasClear === 'function') {
    window.canvasClear(canvas_id, color);
  }
});

static int active_canvas_id = -1;

// (wasm-create-canvas w h) or (wasm-create-canvas w h "title")
static lbm_value ext_wasm_create_canvas(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  int w = lbm_dec_as_i32(args[0]);
  int h = lbm_dec_as_i32(args[1]);
  const char *title = "";
  if (argn >= 3 && lbm_is_array_r(args[2])) {
    title = lbm_dec_str(args[2]);
    if (!title) title = "";
  }
  active_canvas_id = js_create_canvas_tab(w, h, title);
  return ENC_SYM_TRUE;
}

static bool wasm_render_image(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors) {
  if (active_canvas_id < 0) return false;

  uint16_t w    = img->width;
  uint16_t h    = img->height;
  uint32_t npix = (uint32_t)w * (uint32_t)h;
  uint8_t *data = img->data;

  uint8_t *rgba = (uint8_t*)malloc(npix * 4);
  if (!rgba) return false;

  for (uint32_t i = 0; i < npix; i++) {
    uint32_t c  = 0;
    int      px = (int)(i % w);
    int      py = (int)(i / w);

    switch (img->fmt) {
    case indexed2: {
      int byte = (int)i >> 3;
      int bit  = 7 - ((int)i & 0x7);
      int ci   = (data[byte] & (1 << bit)) >> bit;
      c = COLOR_TO_RGB888(colors[ci], px, py);
      break;
    }
    case indexed4: {
      int byte = (int)i >> 2;
      int bit  = (3 - ((int)i & 0x03)) * 2;
      int ci   = (data[byte] & (0x03 << bit)) >> bit;
      c = COLOR_TO_RGB888(colors[ci], px, py);
      break;
    }
    case indexed16: {
      int byte = (int)i >> 1;
      int bit  = (1 - ((int)i & 0x01)) * 4;
      int ci   = (data[byte] & (0x0F << bit)) >> bit;
      c = COLOR_TO_RGB888(colors[ci], px, py);
      break;
    }
    case rgb332: {
      uint8_t  p = data[i];
      uint32_t r = (p >> 5) & 0x7;
      uint32_t g = (p >> 2) & 0x7;
      uint32_t b = p & 0x3;
      b = (b > 0) ? 2*b+1 : 0;
      r = (r == 7) ? 255 : r * 36;
      g = (g == 7) ? 255 : g * 36;
      b = (b == 7) ? 255 : b * 36;
      c = (r << 16) | (g << 8) | b;
      break;
    }
    case rgb565: {
      uint16_t p = (uint16_t)((data[2*i] << 8) | data[2*i+1]);
      uint32_t r = p >> 11;
      uint32_t g = (p >> 5) & 0x3F;
      uint32_t b = p & 0x1F;
      c = (r << 19) | (g << 10) | (b << 3);
      break;
    }
    case rgb888: {
      c = ((uint32_t)data[3*i] << 16) | ((uint32_t)data[3*i+1] << 8) | data[3*i+2];
      break;
    }
    default: break;
    }

    rgba[4*i + 0] = (c >> 16) & 0xFF;
    rgba[4*i + 1] = (c >>  8) & 0xFF;
    rgba[4*i + 2] =  c        & 0xFF;
    rgba[4*i + 3] = 255;
  }

  js_canvas_put_image(active_canvas_id, rgba, (int)w, (int)h, (int)x, (int)y);
  free(rgba);
  return true;
}

static void wasm_clear(uint32_t color) {
  if (active_canvas_id >= 0) js_canvas_clear_js(active_canvas_id, color);
}

static void wasm_reset(void) {
  if (active_canvas_id >= 0) js_canvas_clear_js(active_canvas_id, 0);
}

static const char *ctx_state_str(uint32_t state) {
  uint32_t s = state & ~LBM_THREAD_STATE_GC_BIT;
  if (s == LBM_THREAD_STATE_READY)   return "ready";
  if (s & LBM_THREAD_STATE_SLEEPING) return "sleeping";
  if (s & LBM_THREAD_STATE_RECV_BL)  return "recv-blocked";
  if (s & LBM_THREAD_STATE_RECV_TO)  return "recv-timeout";
  if (s & LBM_THREAD_STATE_TIMEOUT)  return "timeout";
  if (s & LBM_THREAD_STATE_BLOCKED)  return "blocked";
  return "unknown";
}

static void ctx_to_json(eval_context_t *ctx, void *arg1, void *arg2) {
  char *buf = (char*)arg1;
  int  *pos = (int*)arg2;
  const char *sep   = (*pos > 1) ? "," : "";
  const char *name  = ctx->name ? ctx->name : "";
  const char *state = ctx_state_str(ctx->state);
  int n = snprintf(buf + *pos, (size_t)(CTX_LIST_BUFFER_SIZE - *pos),
                   "%s{\"cid\":%d,\"name\":\"%s\",\"state\":\"%s\"}",
                   sep, (int)ctx->id, name, state);
  if (n > 0) *pos += n;
}

static void done_callback(eval_context_t *ctx) {
  char result[1024];
  lbm_print_value(result, sizeof(result), ctx->r);
  print_callback("> %s\n", result);
  drop_reader(ctx->id);
}

static void sleep_callback(uint32_t us) {
  (void)us;
}

// ////////////////////////////////////////////////////////////
//   Exported WASM API
//

EMSCRIPTEN_KEEPALIVE
int lbm_wasm_init(void) {
  if (!lbm_init(heap, HEAP_SIZE,
                lbm_memory, LBM_MEMORY_SIZE,
                lbm_bitmap, LBM_BITMAP_SIZE,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    return 0;
  }
  if (!lbm_eval_init_events(20)) {
    return 0;
  }
  lbm_set_critical_error_callback(critical_callback);
  lbm_set_ctx_done_callback(done_callback);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_dynamic_load_callback(dynamic_loader);
  lbm_set_printf_callback(print_callback);

  lbm_image_init(image_storage,
                 IMAGE_STORAGE_SIZE / sizeof(uint32_t),
                 wasm_image_write);
  memset(image_storage, 0, IMAGE_STORAGE_SIZE);
  lbm_image_create("wasm");
  if (!lbm_image_boot()) {
    return 0;
  }

  lbm_array_extensions_init();
  lbm_string_extensions_init();
  lbm_math_extensions_init();
  lbm_runtime_extensions_init();
  lbm_random_extensions_init();
  lbm_set_extensions_init();
  lbm_dyn_lib_init();
  lbm_crypto_extensions_init();
  lbm_dsp_extensions_init();
  lbm_ecc_extensions_init();
  lbm_display_extensions_init();
  lbm_display_extensions_set_callbacks(wasm_render_image, wasm_clear, wasm_reset);

  lbm_add_eval_symbols();

  lbm_add_extension("systime",             ext_systime);
  lbm_add_extension("secs-since",         ext_secs_since);
  lbm_add_extension("print",               ext_print);
  lbm_add_extension("wasm-create-canvas", ext_wasm_create_canvas);
  lbm_add_extension("wasm-plot",           ext_wasm_plot);
  lbm_add_extension("wasm-plot-multi", ext_wasm_plot_multi);
  lbm_add_extension("wasm-plot-xy",    ext_wasm_plot_xy);
  lbm_add_extension("import",          ext_import);

  output_buffer[0] = '\0';

  print_callback("\nLispBM REPL on WASM\n");

  return 1;
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_eval(const char *str) {
  size_t len = strlen(str);
  char *buf = (char*)malloc(len + 1);
  if (!buf) return;
  memcpy(buf, str, len + 1);
  lbm_create_string_char_channel(&string_tok_state, &string_tok, buf);
  lbm_cid cid = lbm_load_and_eval_expression(&string_tok);
  if (cid >= 0) {
    add_reader(buf, cid);
  } else {
    free(buf);
    print_callback("Error: failed to spawn eval context\n");
  }
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_eval_program(const char *str) {
  size_t len = strlen(str);
  char *buf = (char*)malloc(len + 1);
  if (!buf) return;
  memcpy(buf, str, len + 1);
  lbm_create_string_char_channel(&string_tok_state, &string_tok, buf);
  lbm_cid cid = lbm_load_and_eval_program(&string_tok, NULL);
  if (cid >= 0) {
    add_reader(buf, cid);
  } else {
    free(buf);
    print_callback("Error: failed to spawn eval program context\n");
  }
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_step(void) {
  lbm_eval_step(1);
}

EMSCRIPTEN_KEEPALIVE
bool lbm_wasm_run(int steps) {
  return lbm_eval_step(steps);
}

EMSCRIPTEN_KEEPALIVE
const char *lbm_wasm_get_output(void) {
  return output_buffer;
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_clear_output(void) {
  output_pos = 0;
  output_buffer[0] = '\0';
}

EMSCRIPTEN_KEEPALIVE
const char *lbm_wasm_get_stats(void) {
  lbm_heap_state_t hs;
  lbm_get_heap_state(&hs);

  lbm_uint heap_free    = lbm_heap_num_free();
  lbm_uint mem_words    = lbm_memory_num_words();
  lbm_uint mem_free     = lbm_memory_num_free();
  lbm_uint mem_longest  = lbm_memory_longest_free();
  lbm_uint mem_max_used = lbm_memory_maximum_used();
  float    mem_max_pct  = mem_words > 0
                          ? 100.0f * ((float)mem_max_used / (float)mem_words)
                          : 0.0f;

  snprintf(stats_buffer, STATS_BUFFER_SIZE,
           "{"
           "\"heap_size\":%u,\"heap_free\":%u,"
           "\"gc_num\":%u,\"gc_recovered\":%u,"
           "\"gc_recovered_arrays\":%u,\"gc_marked\":%u,"
           "\"gc_stack_max\":%u,\"gc_stack_size\":%u,"
           "\"mem_size\":%u,\"mem_free\":%u,"
           "\"mem_longest_free\":%u,\"mem_max_used_pct\":%.1f,"
           "\"num_alloc_arrays\":%u"
           "}",
           (unsigned)HEAP_SIZE,        (unsigned)heap_free,
           (unsigned)hs.gc_num,        (unsigned)hs.gc_recovered,
           (unsigned)hs.gc_recovered_arrays, (unsigned)hs.gc_marked,
           (unsigned)lbm_get_gc_stack_max(), (unsigned)lbm_get_gc_stack_size(),
           (unsigned)(mem_words * 4),  (unsigned)(mem_free * 4),
           (unsigned)(mem_longest * 4), mem_max_pct,
           (unsigned)hs.num_alloc_arrays);

  return stats_buffer;
}

EMSCRIPTEN_KEEPALIVE
int lbm_wasm_is_running(void) {
  return (int)(lbm_get_eval_state() != EVAL_CPS_STATE_DEAD);
}

EMSCRIPTEN_KEEPALIVE
const char *lbm_wasm_get_ctxs(void) {
  ctx_list_buffer[0] = '[';
  int pos = 1;
  lbm_all_ctxs_iterator(ctx_to_json, ctx_list_buffer, &pos);
  if (pos < CTX_LIST_BUFFER_SIZE - 2) {
    ctx_list_buffer[pos++] = ']';
    ctx_list_buffer[pos]   = '\0';
  }
  return ctx_list_buffer;
}

int main(void) {
  return 0;
}
