/*
    Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

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

#include "lispbm.h"

static lbm_value ext_stub(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_NIL;
}

void load_vesc_extension_stubs(void) {
  lbm_add_extension("aes-ctr-crypt", ext_stub);            // (str str str int int) -> bool
  lbm_add_extension("app-adc-detach", ext_stub);           // (int [int]) -> bool
  lbm_add_extension("app-adc-override", ext_stub);         // (int float) -> bool
  lbm_add_extension("app-adc-range-ok", ext_stub);         // () -> bool
  lbm_add_extension("app-disable-output", ext_stub);       // (int) -> bool
  lbm_add_extension("app-is-output-disabled", ext_stub);   // () -> bool
  lbm_add_extension("app-pas-get-rpm", ext_stub);          // () -> float
  lbm_add_extension("app-ppm-detach", ext_stub);           // (int) -> bool
  lbm_add_extension("app-ppm-override", ext_stub);         // (float) -> bool
  lbm_add_extension("as504x-angle", ext_stub);             // () -> float
  lbm_add_extension("as504x-deinit", ext_stub);            // () -> bool
  lbm_add_extension("as504x-init", ext_stub);              // (int int int [int]) -> bool
  lbm_add_extension("bits-dec-int", ext_stub);             // (int int int) -> int
  lbm_add_extension("bits-enc-int", ext_stub);             // (int int int int) -> int
  lbm_add_extension("bme280-hum", ext_stub);               // () -> float
  lbm_add_extension("bme280-pres", ext_stub);              // () -> float
  lbm_add_extension("bme280-start", ext_stub);             // (int int) -> bool
  lbm_add_extension("bme280-stop", ext_stub);              // () -> bool
  lbm_add_extension("bme280-temp", ext_stub);              // () -> float
  lbm_add_extension("bms-force-balance", ext_stub);        // (int) -> bool
  lbm_add_extension("bms-st", ext_stub);                   // ([int]) -> list
  lbm_add_extension("bms-zero-offset", ext_stub);          // () -> bool
  lbm_add_extension("buf-resize", ext_stub);               // (str int) -> str
  lbm_add_extension("cmds-proc", ext_stub);                // (str) -> bool
  lbm_add_extension("cmds-start-stop", ext_stub);          // ([bool]) -> bool
  lbm_add_extension("conf-dc-cal", ext_stub);              // ([bool]) -> list
  lbm_add_extension("conf-dc-cal-set", ext_stub);          // (float...) -> bool
  lbm_add_extension("conf-detect-foc", ext_stub);          // (bool float float float float float) -> int
  lbm_add_extension("conf-detect-hall", ext_stub);         // (float) -> list
  lbm_add_extension("conf-detect-lambda-enc", ext_stub);   // (float float float float float) -> list
  lbm_add_extension("conf-enc-sincos", ext_stub);          // (float...) -> list
  lbm_add_extension("conf-get", ext_stub);                 // (sym) -> any
  lbm_add_extension("conf-get-limits", ext_stub);          // () -> list
  lbm_add_extension("conf-measure-ind", ext_stub);         // (float [int]) -> list
  lbm_add_extension("conf-measure-res", ext_stub);         // (float [int]) -> float
  lbm_add_extension("conf-remap-as504x", ext_stub);        // (sym...) -> bool
  lbm_add_extension("conf-remap-hall", ext_stub);          // (sym...) -> bool
  lbm_add_extension("conf-restore-app", ext_stub);         // () -> bool
  lbm_add_extension("conf-restore-mc", ext_stub);          // () -> bool
  lbm_add_extension("conf-set", ext_stub);                 // (sym num) -> bool
  lbm_add_extension("conf-set-pid-offset", ext_stub);      // (float [bool]) -> bool
  lbm_add_extension("conf-store", ext_stub);               // () -> bool
  lbm_add_extension("connected-ble", ext_stub);            // () -> bool
  lbm_add_extension("connected-hub", ext_stub);            // () -> bool
  lbm_add_extension("connected-usb", ext_stub);            // () -> bool
  lbm_add_extension("connected-wifi", ext_stub);           // () -> bool
  lbm_add_extension("crc16", ext_stub);                    // (str [int]) -> int
  lbm_add_extension("crc32", ext_stub);                    // (str [int]) -> int
  lbm_add_extension("eeprom-erase", ext_stub);             // () -> bool
  lbm_add_extension("eeprom-read-f", ext_stub);            // (int) -> float
  lbm_add_extension("eeprom-read-i", ext_stub);            // (int) -> int
  lbm_add_extension("eeprom-store-f", ext_stub);           // (int float) -> bool
  lbm_add_extension("eeprom-store-i", ext_stub);           // (int int) -> bool
  lbm_add_extension("empty", ext_stub);                    // () -> bool
  lbm_add_extension("enc-corr", ext_stub);                 // (int [int]) -> int
  lbm_add_extension("enc-corr-en", ext_stub);              // ([int]) -> int
  lbm_add_extension("enc-sample", ext_stub);               // (str int) -> bool
  lbm_add_extension("encoder-abi-state", ext_stub);        // () -> list
  lbm_add_extension("encoder-index-found", ext_stub);      // () -> bool
  lbm_add_extension("esp-now-add-peer", ext_stub);         // (str) -> bool
  lbm_add_extension("esp-now-del-peer", ext_stub);         // (str) -> bool
  lbm_add_extension("esp-now-recv", ext_stub);             // ([float]) -> list
  lbm_add_extension("esp-now-send", ext_stub);             // (str str) -> bool
  lbm_add_extension("esp-now-start", ext_stub);            // () -> bool
  lbm_add_extension("event-enable", ext_stub);             // (sym [bool]) -> bool
  lbm_add_extension("foc-beep", ext_stub);                 // (float float float) -> bool
  lbm_add_extension("foc-est-ind", ext_stub);              // () -> float
  lbm_add_extension("foc-est-lambda", ext_stub);           // () -> float
  lbm_add_extension("foc-est-res", ext_stub);              // () -> float
  lbm_add_extension("foc-hfi-res", ext_stub);              // () -> float
  lbm_add_extension("foc-openloop", ext_stub);             // (float float) -> bool
  lbm_add_extension("foc-openloop-phase", ext_stub);       // (float float float) -> bool
  lbm_add_extension("foc-play-samples", ext_stub);         // (str float float) -> bool
  lbm_add_extension("foc-play-stop", ext_stub);            // () -> bool
  lbm_add_extension("foc-play-tone", ext_stub);            // (float float float) -> bool
  lbm_add_extension("foc-set-fw-override", ext_stub);      // (float) -> bool
  lbm_add_extension("fw-data", ext_stub);                  // ([int [int]]) -> str
  lbm_add_extension("fw-erase", ext_stub);                 // (int [int]) -> bool
  lbm_add_extension("fw-info", ext_stub);                  // ([int]) -> list
  lbm_add_extension("fw-reboot", ext_stub);                // ([int]) -> bool
  lbm_add_extension("fw-write", ext_stub);                 // (int str [int]) -> bool
  lbm_add_extension("fw-write-raw", ext_stub);             // (int str) -> bool
  lbm_add_extension("get-adc", ext_stub);                  // ([int]) -> float
  lbm_add_extension("get-adc-decoded", ext_stub);          // ([int]) -> float
  lbm_add_extension("get-ah", ext_stub);                   // () -> float
  lbm_add_extension("get-ah-chg", ext_stub);               // () -> float
  lbm_add_extension("get-batt", ext_stub);                 // () -> float
  lbm_add_extension("get-bms-val", ext_stub);              // (sym [int]) -> any
  lbm_add_extension("get-current", ext_stub);              // ([int]) -> float
  lbm_add_extension("get-current-dir", ext_stub);          // () -> float
  lbm_add_extension("get-current-in", ext_stub);           // ([int]) -> float
  lbm_add_extension("get-dist", ext_stub);                 // () -> float
  lbm_add_extension("get-dist-abs", ext_stub);             // () -> float
  lbm_add_extension("get-duty", ext_stub);                 // () -> float
  lbm_add_extension("get-duty-abs", ext_stub);             // () -> float
  lbm_add_extension("get-encoder", ext_stub);              // () -> float
  lbm_add_extension("get-encoder-error-rate", ext_stub);   // () -> float
  lbm_add_extension("get-est-ind", ext_stub);              // () -> float
  lbm_add_extension("get-est-lambda", ext_stub);           // () -> float
  lbm_add_extension("get-est-res", ext_stub);              // () -> float
  lbm_add_extension("get-fault", ext_stub);                // () -> sym
  lbm_add_extension("get-hfi-res", ext_stub);              // () -> float
  lbm_add_extension("get-id", ext_stub);                   // ([int]) -> float
  lbm_add_extension("get-id-set", ext_stub);               // () -> float
  lbm_add_extension("get-id-target", ext_stub);            // () -> float
  lbm_add_extension("get-imu-acc", ext_stub);              // () -> (float float float)
  lbm_add_extension("get-imu-acc-derot", ext_stub);        // () -> (float float float)
  lbm_add_extension("get-imu-gyro", ext_stub);             // () -> (float float float)
  lbm_add_extension("get-imu-gyro-derot", ext_stub);       // () -> (float float float)
  lbm_add_extension("get-imu-mag", ext_stub);              // () -> (float float float)
  lbm_add_extension("get-imu-quat", ext_stub);             // () -> (float float float float)
  lbm_add_extension("get-imu-rpy", ext_stub);              // () -> (float float float)
  lbm_add_extension("get-iq", ext_stub);                   // ([int]) -> float
  lbm_add_extension("get-iq-set", ext_stub);               // () -> float
  lbm_add_extension("get-iq-target", ext_stub);            // () -> float
  lbm_add_extension("get-mac-addr", ext_stub);             // () -> str
  lbm_add_extension("get-pos", ext_stub);                  // () -> float
  lbm_add_extension("get-ppm", ext_stub);                  // () -> float
  lbm_add_extension("get-ppm-age", ext_stub);              // () -> float
  lbm_add_extension("get-remote-state", ext_stub);         // () -> list
  lbm_add_extension("get-rpm", ext_stub);                  // () -> float
  lbm_add_extension("get-rpm-fast", ext_stub);             // () -> float
  lbm_add_extension("get-rpm-faster", ext_stub);           // () -> float
  lbm_add_extension("get-rpm-set", ext_stub);              // () -> float
  lbm_add_extension("get-selected-motor", ext_stub);       // () -> int
  lbm_add_extension("get-speed", ext_stub);                // () -> float
  lbm_add_extension("get-speed-set", ext_stub);            // () -> float
  lbm_add_extension("get-temp-fet", ext_stub);             // ([int]) -> float
  lbm_add_extension("get-temp-mot", ext_stub);             // () -> float
  lbm_add_extension("get-temp-mot-res", ext_stub);         // () -> float
  lbm_add_extension("get-vd", ext_stub);                   // ([int]) -> float
  lbm_add_extension("get-vin", ext_stub);                  // () -> float
  lbm_add_extension("get-vq", ext_stub);                   // () -> float
  lbm_add_extension("get-wh", ext_stub);                   // () -> float
  lbm_add_extension("get-wh-chg", ext_stub);               // () -> float
  lbm_add_extension("gnss-age", ext_stub);                 // () -> float
  lbm_add_extension("gnss-date-time", ext_stub);           // () -> list
  lbm_add_extension("gnss-hdop", ext_stub);                // () -> float
  lbm_add_extension("gnss-height", ext_stub);              // () -> float
  lbm_add_extension("gnss-lat-lon", ext_stub);             // () -> (float float)
  lbm_add_extension("gnss-speed", ext_stub);               // () -> float
  lbm_add_extension("gpio-configure", ext_stub);           // (sym sym) -> bool
  lbm_add_extension("gpio-hold", ext_stub);                // (int bool) -> bool
  lbm_add_extension("gpio-hold-deepsleep", ext_stub);      // (bool) -> bool
  lbm_add_extension("gpio-read", ext_stub);                // (sym) -> bool
  lbm_add_extension("gpio-write", ext_stub);               // (sym bool) -> bool
  lbm_add_extension("i2c-detect-addr", ext_stub);          // ([int]) -> list
  lbm_add_extension("i2c-restore", ext_stub);              // () -> bool
  lbm_add_extension("i2c-start", ext_stub);                // ([sym [int int]]) -> bool
  lbm_add_extension("i2c-tx-rx", ext_stub);                // (int str [str]) -> bool
  lbm_add_extension("icu-period", ext_stub);               // (sym) -> float
  lbm_add_extension("icu-start", ext_stub);                // (sym float) -> bool
  lbm_add_extension("icu-width", ext_stub);                // (sym) -> float
  lbm_add_extension("image-save", ext_stub);               // () -> bool
  lbm_add_extension("imu-start-lsm6", ext_stub);           // ([sym [int int]]) -> bool
  lbm_add_extension("imu-stop", ext_stub);                 // () -> bool
  lbm_add_extension("ioboard-get-adc", ext_stub);          // (int int) -> float
  lbm_add_extension("ioboard-get-digital", ext_stub);      // (int int) -> bool
  lbm_add_extension("ioboard-set-digital", ext_stub);      // (int int bool) -> bool
  lbm_add_extension("ioboard-set-pwm", ext_stub);          // (int int float) -> bool
  lbm_add_extension("lbm-erase", ext_stub);                // ([int]) -> bool
  lbm_add_extension("lbm-run", ext_stub);                  // (bool [int]) -> bool
  lbm_add_extension("lbm-set-gc-stack-size", ext_stub);    // (int) -> bool
  lbm_add_extension("lbm-set-quota", ext_stub);            // (int) -> bool
  lbm_add_extension("lbm-write", ext_stub);                // (int str [int]) -> bool
  lbm_add_extension("load-native-lib", ext_stub);          // (str) -> any
  lbm_add_extension("log-config-field", ext_stub);         // (int int str str str int bool bool) -> bool
  lbm_add_extension("log-send-f32", ext_stub);             // (int int num...) -> bool
  lbm_add_extension("log-send-f64", ext_stub);             // (int int num...) -> bool
  lbm_add_extension("log-start", ext_stub);                // (int int str bool bool) -> bool
  lbm_add_extension("log-stop", ext_stub);                 // (int) -> bool
  lbm_add_extension("main-init-done", ext_stub);           // () -> bool
  lbm_add_extension("nmea-parse", ext_stub);               // (str) -> bool
  lbm_add_extension("nvs-erase", ext_stub);                // (str) -> bool
  lbm_add_extension("nvs-list", ext_stub);                 // () -> list
  lbm_add_extension("nvs-qml-erase-key", ext_stub);        // (str) -> bool
  lbm_add_extension("nvs-qml-erase-partition", ext_stub);  // () -> bool
  lbm_add_extension("nvs-qml-init", ext_stub);             // () -> bool
  lbm_add_extension("nvs-qml-list", ext_stub);             // () -> list
  lbm_add_extension("nvs-qml-read", ext_stub);             // (str) -> any
  lbm_add_extension("nvs-qml-write", ext_stub);            // (str any) -> bool
  lbm_add_extension("nvs-read", ext_stub);                 // (str) -> any
  lbm_add_extension("nvs-write", ext_stub);                // (str any) -> bool
  lbm_add_extension("observer-error", ext_stub);           // () -> float
  lbm_add_extension("override-speed", ext_stub);           // (float) -> bool
  lbm_add_extension("override-temp-motor", ext_stub);      // (float) -> bool
  lbm_add_extension("phase-all", ext_stub);                // () -> list
  lbm_add_extension("phase-encoder", ext_stub);            // () -> float
  lbm_add_extension("phase-hall", ext_stub);               // () -> float
  lbm_add_extension("phase-motor", ext_stub);              // () -> float
  lbm_add_extension("phase-observer", ext_stub);           // () -> float
  lbm_add_extension("plot-add-graph", ext_stub);           // (str) -> bool
  lbm_add_extension("plot-init", ext_stub);                // (str str) -> bool
  lbm_add_extension("plot-send-points", ext_stub);         // (float float) -> bool
  lbm_add_extension("plot-set-graph", ext_stub);           // (int) -> bool
  lbm_add_extension("pos-pid-error", ext_stub);            // () -> float
  lbm_add_extension("pos-pid-now", ext_stub);              // () -> float
  lbm_add_extension("pos-pid-set", ext_stub);              // () -> float
  lbm_add_extension("prof-result", ext_stub);              // () -> list
  lbm_add_extension("prof-trig", ext_stub);                // () -> bool
  lbm_add_extension("puts", ext_stub);                     // (str) -> bool
  lbm_add_extension("pwm-set-duty", ext_stub);             // (float [int]) -> any
  lbm_add_extension("pwm-start", ext_stub);                // (int float [int int]) -> int
  lbm_add_extension("pwm-stop", ext_stub);                 // ([int]) -> bool
  lbm_add_extension("qml-erase", ext_stub);                // ([int]) -> bool
  lbm_add_extension("qml-write", ext_stub);                // (int str [int]) -> bool
  lbm_add_extension("rand", ext_stub);                     // ([int]) -> int
  lbm_add_extension("rand-max", ext_stub);                 // () -> int
  lbm_add_extension("raw-adc-current", ext_stub);          // (int int [int]) -> float
  lbm_add_extension("raw-adc-voltage", ext_stub);          // (int int [int]) -> float
  lbm_add_extension("raw-hall", ext_stub);                 // (int [int]) -> list
  lbm_add_extension("raw-mod-alpha", ext_stub);            // () -> float
  lbm_add_extension("raw-mod-alpha-measured", ext_stub);   // () -> float
  lbm_add_extension("raw-mod-beta", ext_stub);             // () -> float
  lbm_add_extension("raw-mod-beta-measured", ext_stub);    // () -> float
  lbm_add_extension("reboot", ext_stub);                   // () -> nil
  lbm_add_extension("recv-data", ext_stub);                // ([float]) -> list
  lbm_add_extension("reset-timeout", ext_stub);            // () -> bool
  lbm_add_extension("rtc-data", ext_stub);                 // () -> str
  lbm_add_extension("secs-since", ext_stub);               // (int) -> float
  lbm_add_extension("select-motor", ext_stub);             // (int) -> bool
  lbm_add_extension("send-data", ext_stub);                // (str [int [int]]) -> bool
  lbm_add_extension("set-aux", ext_stub);                  // (int bool) -> bool
  lbm_add_extension("set-bms-chg-allowed", ext_stub);      // (int) -> bool
  lbm_add_extension("set-bms-val", ext_stub);              // (sym any [int]) -> bool
  lbm_add_extension("set-brake", ext_stub);                // (float) -> bool
  lbm_add_extension("set-brake-rel", ext_stub);            // (float) -> bool
  lbm_add_extension("set-current", ext_stub);              // (float [float]) -> bool
  lbm_add_extension("set-current-rel", ext_stub);          // (float [float]) -> bool
  lbm_add_extension("set-duty", ext_stub);                 // (float) -> bool
  lbm_add_extension("set-encoder", ext_stub);              // (float) -> bool
  lbm_add_extension("set-fw-name", ext_stub);              // (str) -> bool
  lbm_add_extension("set-handbrake", ext_stub);            // (float) -> bool
  lbm_add_extension("set-handbrake-rel", ext_stub);        // (float) -> bool
  lbm_add_extension("set-kill-sw", ext_stub);              // (bool) -> bool
  lbm_add_extension("set-odometer", ext_stub);             // (int) -> bool
  lbm_add_extension("set-pos", ext_stub);                  // (float) -> bool
  lbm_add_extension("set-pos-time", ext_stub);             // (float...) -> bool
  lbm_add_extension("set-print-prefix", ext_stub);         // (str) -> bool
  lbm_add_extension("set-remote-state", ext_stub);         // (float float int int int) -> bool
  lbm_add_extension("set-rpm", ext_stub);                  // (float) -> bool
  lbm_add_extension("set-servo", ext_stub);                // (float) -> bool
  lbm_add_extension("setup-ah", ext_stub);                 // () -> float
  lbm_add_extension("setup-ah-chg", ext_stub);             // () -> float
  lbm_add_extension("setup-current", ext_stub);            // () -> float
  lbm_add_extension("setup-current-in", ext_stub);         // () -> float
  lbm_add_extension("setup-num-vescs", ext_stub);          // () -> int
  lbm_add_extension("setup-wh", ext_stub);                 // () -> float
  lbm_add_extension("setup-wh-chg", ext_stub);             // () -> float
  lbm_add_extension("shutdown-hold", ext_stub);            // (bool) -> bool
  lbm_add_extension("sleep-config-wakeup-pin", ext_stub);  // (int int) -> bool
  lbm_add_extension("sleep-deep", ext_stub);               // (float) -> bool
  lbm_add_extension("sleep-light", ext_stub);              // (float) -> bool
  lbm_add_extension("stats", ext_stub);                    // (sym) -> float
  lbm_add_extension("stats-reset", ext_stub);              // () -> bool
  lbm_add_extension("store-backup", ext_stub);             // () -> bool
  lbm_add_extension("sysinfo", ext_stub);                  // (sym) -> any
  //  lbm_add_extension("systime", ext_stub); Implemented in repl_exts.c
  lbm_add_extension("throttle-curve", ext_stub);           // (float float float int) -> float
  lbm_add_extension("timeout-reset", ext_stub);            // () -> bool
  lbm_add_extension("uartcomm-start", ext_stub);           // (int int int int) -> bool
  lbm_add_extension("uartcomm-stop", ext_stub);            // (int) -> bool
  lbm_add_extension("uart-read", ext_stub);                // (str int [int [int [float]]]) -> int
  lbm_add_extension("uart-start", ext_stub);               // (int [sym]) -> bool
  lbm_add_extension("uart-stop", ext_stub);                // () -> bool
  lbm_add_extension("uart-write", ext_stub);               // (str) -> bool
  lbm_add_extension("uavcan-last-rawcmd", ext_stub);       // (int) -> float
  lbm_add_extension("uavcan-last-rpmcmd", ext_stub);       // (int) -> float
  lbm_add_extension("ublox-init", ext_stub);               // ([int [int [int [int]]]]) -> bool
  lbm_add_extension("unload-native-lib", ext_stub);        // (any) -> bool
  lbm_add_extension("wifi-get-bw", ext_stub);              // () -> int
  lbm_add_extension("wifi-get-chan", ext_stub);             // () -> int
  lbm_add_extension("wifi-set-bw", ext_stub);              // (int) -> bool
  lbm_add_extension("wifi-set-chan", ext_stub);             // (int) -> bool
  lbm_add_extension("wifi-start", ext_stub);               // (str str [int]) -> bool
  lbm_add_extension("wifi-stop", ext_stub);                // () -> bool
  lbm_add_extension("zip-ls", ext_stub);                   // (str) -> list
}
