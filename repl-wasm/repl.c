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
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/runtime_extensions.h"
#include "extensions/random_extensions.h"
#include "extensions/set_extensions.h"
#include "extensions/lbm_dyn_lib.h"
#include "extensions/crypto_extensions.h"
#include "extensions/dsp_extensions.h"
#include "extensions/ecc_extensions.h"

#define HEAP_SIZE              (1 << 14)
#define GC_STACK_SIZE          256
#define PRINT_STACK_SIZE       256
#define EXTENSION_STORAGE_SIZE 512
#define LBM_MEMORY_BLOCKS      16384   // ~10K  (* (* 160 16) 4) 10240
                                       //  64K  (* (* 1024 16) 4) 65536
                                       // 256K  (* (* 4096 16) 4) 262144
                                       // 512K  (* (* 8192 16) 4) 655360
                                       //   1M  (* (* 16384 16) 4) 1048576
#define LBM_MEMORY_SIZE        LBM_MEMORY_SIZE_BLOCKS_TO_WORDS(LBM_MEMORY_BLOCKS)
#define LBM_BITMAP_SIZE        LBM_MEMORY_BITMAP_SIZE(LBM_MEMORY_BLOCKS)
#define OUTPUT_BUFFER_SIZE     65536
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

/* ----------------------------------------------------------
   Callbacks
   ---------------------------------------------------------- */

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

// ------------------------------------------------------------
//   Extensions
// ------------------------------------------------------------

static bool dynamic_loader(const char *sym, const char **code) {
  return lbm_dyn_lib_find(sym, code);
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

// (import library-file-name sym)
static lbm_value ext_import(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_array_r(args[0]) || !lbm_is_symbol(args[1])) return ENC_SYM_TERROR;
  const char *filename = lbm_dec_str(args[0]);
  if (!filename) return ENC_SYM_TERROR;
  const char *symname = lbm_get_name_by_symbol(lbm_dec_sym(args[1]));
  if (!symname) return ENC_SYM_TERROR;
  char *code = js_import_lib(filename);
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

static void done_callback(eval_context_t *ctx) {
  char result[1024];
  lbm_print_value(result, sizeof(result), ctx->r);
  print_callback("> %s\n", result);
  drop_reader(ctx->id);
}

static void sleep_callback(uint32_t us) {
  (void)us;
}

/* ----------------------------------------------------------
   Exported WASM API
   ---------------------------------------------------------- */

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

  lbm_add_eval_symbols();

  lbm_add_extension("print",          ext_print);
  lbm_add_extension("wasm-plot",       ext_wasm_plot);
  lbm_add_extension("wasm-plot-multi", ext_wasm_plot_multi);
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
  lbm_eval_step();
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_run(int steps) {
  for (int i = 0; i < steps; i++) {
    lbm_eval_step();
  }
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
int lbm_wasm_is_running(void) {
  return (int)(lbm_get_eval_state() != EVAL_CPS_STATE_DEAD);
}

int main(void) {
  return 0;
}
