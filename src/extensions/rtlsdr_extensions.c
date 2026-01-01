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

static lbm_uint sym_gain_auto;
static lbm_uint sym_gain_manual;
static lbm_uint sym_agc_on;
static lbm_uint sym_agc_off;

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
    if (free_dev >= 0) {
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
    if (dev_id < RTLSDR_MAX_DEVICES) {

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

lbm_value ext_rtlsdr_set_center_freq(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_number(args[0]) && lbm_is_number(args[1])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    uint32_t freq = lbm_dec_as_u32(args[1]);
    r = ENC_SYM_EERROR;
    if (dev_id < RTLSDR_MAX_DEVICES && devs[dev_id].open) {
      if (rtlsdr_set_center_freq(devs[dev_id].dev, freq) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_get_center_freq(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev_id < RTLSDR_MAX_DEVICES && devs[dev_id].open) {
      uint32_t freq = rtlsdr_get_center_freq(devs[dev_id].dev);
      r = lbm_enc_u32(freq);
    }
  }
  return r;
}

lbm_value ext_rtlsdr_set_sample_rate(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_number(args[0]) && lbm_is_number(args[1])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    uint32_t rate = lbm_dec_as_u32(args[1]);
    r = ENC_SYM_EERROR;
    if (dev_id < RTLSDR_MAX_DEVICES && devs[dev_id].open) {
      if (rtlsdr_set_sample_rate(devs[dev_id].dev, rate) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_get_sample_rate(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev_id < RTLSDR_MAX_DEVICES && devs[dev_id].open) {
      uint32_t rate = rtlsdr_get_sample_rate(devs[dev_id].dev);
      r = lbm_enc_u32(rate);
    }
  }
  return r;
}

lbm_value ext_rtlsdr_set_tuner_gain_mode(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_number(args[0]) && lbm_is_symbol(args[1])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    int32_t mode = lbm_dec_sym(args[1]);
    r = ENC_SYM_EERROR;
    if (dev_id < RTLSDR_MAX_DEVICES && devs[dev_id].open) {
      r = ENC_SYM_NIL;
      if (mode == sym_gain_manual) {
        if (rtlsdr_set_tuner_gain_mode(devs[dev_id].dev, 1) == 0) {
          r = ENC_SYM_TRUE;
        }
      } else if (mode == sym_gain_auto) {
        if (rtlsdr_set_tuner_gain_mode(devs[dev_id].dev, 0) == 0) {
          r = ENC_SYM_TRUE;
        }
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_set_tuner_gain(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_number(args[0]) && lbm_is_number(args[1])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    int32_t gain = lbm_dec_as_i32(args[1]);
    r = ENC_SYM_EERROR;
    if (dev_id < RTLSDR_MAX_DEVICES && devs[dev_id].open) {
      if (rtlsdr_set_tuner_gain(devs[dev_id].dev, gain) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_get_tuner_gain(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev_id < RTLSDR_MAX_DEVICES && devs[dev_id].open) {
      int32_t gain = rtlsdr_get_tuner_gain(devs[dev_id].dev);
      r = lbm_enc_i32(gain);
    }
  }
  return r;
}

lbm_value ext_rtlsdr_set_agc_mode(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_number(args[0]) && lbm_is_symbol(args[1])) {
    uint32_t dev_id = lbm_dec_as_u32(args[0]);
    int32_t mode = lbm_dec_sym(args[1]);
    r = ENC_SYM_EERROR;
    if (dev_id < RTLSDR_MAX_DEVICES && devs[dev_id].open) {
      r = ENC_SYM_NIL;
      if (mode == sym_agc_on) {
        if (rtlsdr_set_agc_mode(devs[dev_id].dev, 1) == 0) {
          r = ENC_SYM_TRUE;
        }
      } else if (mode == sym_agc_off) {
        if (rtlsdr_set_agc_mode(devs[dev_id].dev, 0) == 0) {
          r = ENC_SYM_TRUE;
        }
      }
    }
  }
  return r;
}

// Towards FM demodulation


// audio_out is the same size as the i_data/q_data input arrays.
static void fm_discriminator(float *i_data,
                             float *q_data,
                             unsigned int length,
                             float *audio_out) {
  float prev_i = i_data[0];
  float prev_q = q_data[0];
  for (unsigned int n = 1; n < length; n++) {
    audio_out[n] = q_data[n] * prev_i - i_data[n] * prev_q;
    prev_i = i_data[n];
    prev_q = q_data[n];
  }
}


void lbm_rtlsdr_extensions_init(void) {

  for (int i = 0; i < RTLSDR_MAX_DEVICES; i ++) {
    devs[i].dev = NULL;
    devs[i].index = 0;
    devs[i].open = false;
  }

  lbm_add_symbol_const("gain-mode-auto", &sym_gain_auto);
  lbm_add_symbol_const("gain-mode-manual", &sym_gain_manual);
  lbm_add_symbol_const("agc-on", &sym_agc_on);
  lbm_add_symbol_const("agc-off", &sym_agc_off);

  lbm_add_extension("rtlsdr-open", ext_rtlsdr_open);
  lbm_add_extension("rtlsdr-close", ext_rtlsdr_close);
  lbm_add_extension("rtlsdr-get-device-count", ext_rtlsdr_get_device_count);
  lbm_add_extension("rtlsdr-get-device-name", ext_rtlsdr_get_device_name);
  lbm_add_extension("rtlsdr-set-center-freq", ext_rtlsdr_set_center_freq);
  lbm_add_extension("rtlsdr-get-center-freq", ext_rtlsdr_get_center_freq);
  lbm_add_extension("rtlsdr-set-sample-rate", ext_rtlsdr_set_sample_rate);
  lbm_add_extension("rtlsdr-get-sample-rate", ext_rtlsdr_get_sample_rate);
  lbm_add_extension("rtlsdr-set-tuner-gain-mode", ext_rtlsdr_set_tuner_gain_mode);
  lbm_add_extension("rtlsdr-set-tuner-gain", ext_rtlsdr_set_tuner_gain);
  lbm_add_extension("rtlsdr-get-tuner-gain", ext_rtlsdr_get_tuner_gain);
  lbm_add_extension("rtlsdr-set-agc-mode", ext_rtlsdr_set_agc_mode);
}
