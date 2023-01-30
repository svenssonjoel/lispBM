/*
    Copyright 2023 Joel Svensson        svenssonjoel@yahoo.se

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

#include <lbm_memory.h>
#include <heap.h>
#include <eval_cps.h>
#include <extensions.h>
#include <lbm_utils.h>
#include <lbm_version.h>

lbm_value ext_eval_set_quota(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  uint32_t q = lbm_dec_as_u32(args[0]);
  lbm_set_eval_step_quota(q);
  return ENC_SYM_TRUE;
}

lbm_value ext_memory_num_free(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  lbm_uint n = lbm_memory_num_free();
  return lbm_enc_i((lbm_int)n);
}

lbm_value ext_memory_longest_free(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  lbm_uint n = lbm_memory_longest_free();
  return lbm_enc_i((lbm_int)n);
}

lbm_value ext_memory_size(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  lbm_uint n = lbm_memory_num_words();
  return lbm_enc_i((lbm_int)n);
}

lbm_value ext_memory_word_size(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  return lbm_enc_i((lbm_int)sizeof(lbm_uint));
}

lbm_value ext_lbm_version(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  lbm_value version;
  lbm_heap_allocate_list_init(&version, 3,
                              lbm_enc_i(LBM_MAJOR_VERSION),
                              lbm_enc_i(LBM_MINOR_VERSION),
                              lbm_enc_i(LBM_PATCH_VERSION));
  return version;
}

lbm_value ext_lbm_num_gc(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  lbm_heap_state_t state;
  lbm_get_heap_state(&state);
  return lbm_enc_i(state.gc_num);
}


bool lbm_runtime_extensions_init(void) {

  bool res = true;
  res = res && lbm_add_extension("set-eval-quota", ext_eval_set_quota);
  res = res && lbm_add_extension("mem-num-free", ext_memory_num_free);
  res = res && lbm_add_extension("mem-longest-free", ext_memory_longest_free);
  res = res && lbm_add_extension("mem-size", ext_memory_size);
  res = res && lbm_add_extension("word-size", ext_memory_word_size);
  res = res && lbm_add_extension("lbm-version", ext_lbm_version);
  res = res && lbm_add_extension("lbm-num-gc", ext_lbm_num_gc);
  return res;
}
