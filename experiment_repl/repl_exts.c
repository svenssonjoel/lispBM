/*
    Copyright 2024 Joel Svensson  svenssonjoel@yahoo.se

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

#include "repl_exts.h"

#include <sys/time.h>


uint32_t timestamp(void) {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (uint32_t)(tv.tv_sec * 1000000 + tv.tv_usec);
}

static lbm_value ext_time(lbm_value *args, lbm_uint argn) {

  uint32_t time = timestamp();

  return lbm_enc_u32(time);
}

int init_exts(void) {

  if (!lbm_array_extensions_init()) {
    return 0;
  }
  if (!lbm_string_extensions_init()) {
    return 0;
  }
  if (!lbm_math_extensions_init()) {
    return 0;
  }
  if (!lbm_runtime_extensions_init(false)) {
    return 0;
  } 

  lbm_add_extension("time", ext_time);
  
  return 1;
}
