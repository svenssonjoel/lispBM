/*
    Copyright 2026 Joel Svensson        svenssonjoel@yahoo.se

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

#include "rtlsdr_extensions.h"
#include "extensions.h"
#include "lbm_c_interop.h"

#include <rtl-sdr.h>

#define RTLSDR_MAX_DEVICES 5

typedef struct {
  rtlsdr_dev_t *dev;
  uint32_t index;
  bool     open;
} rtlsdr_dev_wrapper;

rtlsdr_dev_wrapper devs[RTLSDR_MAX_DEVICES];

lbm_value ext_rtlsdr_open(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t ix = lbm_dec_as_u32(args[0]);
    int32_t free_dev = -1;
    for (int i = 0; i < RTLSDR_MAX_DEVICES; i ++) {
      if (!devs[i].open) free_dev = i;
      if (devs[i].open && devs[i].index == ix) { // device enumeration index
        r = lbm_enc_i(i); // device ID
        goto rtlsdr_open_done;
      }
    }
    r = ENC_SYM_NIL;
    if (free_dev > 0) {
      devs[free_dev].open = true;
      devs[free_dev].index = ix;
      if (rtlsdr_open(&devs[free_dev].dev, ix) == 0) {
        r = lbm_enc_i(free_dev);
      }
    }
  }
 rtlsdr_open_done:
  return r;
}

lbm_value ext_rtlsdr_close(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev_id >= 0 && dev_id < RTLSDR_MAX_DEVICES) {

      if (devs[dev_id].open) {
        devs[dev_id].open = false;
        devs[dev_id].index = 0;
        // can closing fail? in that case we should have an error field in the
        // devs array and a way to clean out broken devices.
        rtlsdr_close(devs[dev_id].dev);
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}



lbm_value ext_rtlsdr_get_device_count(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;

  if (argn == 0) {
    uint32_t num_devices = rtlsdr_get_device_count();
    r = lbm_enc_i((int32_t)num_devices);
  }
  return r;
}
const char* rtlsdr_get_device_name(uint32_t index);

lbm_value ext_rtlsdr_get_device_name(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;

  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t ix = lbm_dec_as_u32(args[0]);
    const char * device_name = rtlsdr_get_device_name(ix);
    if (device_name) {
      lbm_uint n = strlen(device_name);
      r = ENC_SYM_NIL;
      if (n > 0) {
        lbm_value res_arr;
        r = ENC_SYM_MERROR;
        if (lbm_create_array(&res_arr, n+1)) {
          lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res_arr);
          memcpy(arr->data, device_name, n);
          ((char*)(arr->data))[n] = '\0';
          r = res_arr;
        }
      }
    }
  }
  return r;
}

void lbm_rtlsdr_extensions_init(void) {

  for (int i = 0; i < RTLSDR_MAX_DEVICES; i ++) {
    devs[i].dev = NULL;
    devs[i].index = 0;
    devs[i].open = false;
  }

  lbm_add_extension("rtlsdr-open", ext_rtlsdr_open);
  lbm_add_extension("rtlsdr-close", ext_rtlsdr_close);
  lbm_add_extension("rtlsdr-get-device-count", ext_rtlsdr_get_device_count);
  lbm_add_extension("rtlsdr-get-device-name", ext_rtlsdr_get_device_name);
}
