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
#include "platform_thread.h"
#include "platform_timestamp.h"

#include <rtl-sdr.h>

// supports having a single device open.

rtlsdr_dev_t *dev = NULL;

static lbm_uint sym_gain_auto;
static lbm_uint sym_gain_manual;
static lbm_uint sym_agc_on;
static lbm_uint sym_agc_off;

lbm_value ext_rtlsdr_open(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t ix = lbm_dec_as_u32(args[0]);
    if (rtlsdr_open(&dev, ix) == 0) {
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

lbm_value ext_rtlsdr_close(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (dev) {
      rtlsdr_close(dev);
      r = ENC_SYM_TRUE;
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
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t freq = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (rtlsdr_set_center_freq(dev, freq) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_get_center_freq(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_EERROR;
    if (dev) {
      uint32_t freq = rtlsdr_get_center_freq(dev);
      r = lbm_enc_u32(freq);
    }
  }
  return r;
}

lbm_value ext_rtlsdr_set_sample_rate(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t rate = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (rtlsdr_set_sample_rate(dev, rate) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_get_sample_rate(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_EERROR;
    if (dev) {
      uint32_t rate = rtlsdr_get_sample_rate(dev);
      r = lbm_enc_u32(rate);
    }
  }
  return r;
}

lbm_value ext_rtlsdr_set_tuner_gain_mode(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_symbol(args[0])) {
    int32_t mode = lbm_dec_sym(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (mode == sym_gain_manual) {
        if (rtlsdr_set_tuner_gain_mode(dev, 1) == 0) {
          r = ENC_SYM_TRUE;
        }
      } else if (mode == sym_gain_auto) {
        if (rtlsdr_set_tuner_gain_mode(dev, 0) == 0) {
          r = ENC_SYM_TRUE;
        }
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_set_tuner_gain(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    int32_t gain = lbm_dec_as_i32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (rtlsdr_set_tuner_gain(dev, gain) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_get_tuner_gain(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_EERROR;
    if (dev) {
      int32_t gain = rtlsdr_get_tuner_gain(dev);
      r = lbm_enc_i32(gain);
    }
  }
  return r;
}

lbm_value ext_rtlsdr_set_agc_mode(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_symbol(args[0])) {
    int32_t mode = lbm_dec_sym(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (mode == sym_agc_on) {
        if (rtlsdr_set_agc_mode(dev, 1) == 0) {
          r = ENC_SYM_TRUE;
        }
      } else if (mode == sym_agc_off) {
        if (rtlsdr_set_agc_mode(dev, 0) == 0) {
          r = ENC_SYM_TRUE;
        }
      }
    }
  }
  return r;
}

// Towards FM demodulation

static lbm_thread_t radio_thread;

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


static volatile bool radio_thread_running = false;

static void radio_thd(void *arg) {
  (void) arg;

  while (radio_thread_running) {

    lbm_thread_sleep_us(100);
  }

}

lbm_value ext_rtlsdr_start_radio_thd(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_TRUE;
    if (!radio_thread_running) {
      radio_thread_running = true;
      if (!lbm_thread_create(&radio_thread, "radio_thd", radio_thd,
                             NULL, LBM_THREAD_PRIO_NORMAL, 8192)) {
        r = ENC_SYM_NIL;
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_stop_radio_thd(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (radio_thread_running) {
      radio_thread_running = false;
      lbm_thread_destroy(&radio_thread);
      r = ENC_SYM_TRUE;
    }
  }
  return r;
}


void lbm_rtlsdr_extensions_init(void) {

  dev = NULL;

  radio_thread_running = false;

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

  lbm_add_extension("rtlsdr-start-radio-thread", ext_rtlsdr_start_radio_thd);
  lbm_add_extension("rtlsdr-stop-radio-thread", ext_rtlsdr_stop_radio_thd);
}
