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

#define _DEFAULT_SOURCE  // CRTSCTS

#include "lbm_vesc_can.h"

#include <termios.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "datatypes.h" // from vesc_express ea7b85b243e965e981b44e28a274c0e6e49fbbe9
                       // Tue Jun 23 10:02:33 2026
#include "packet.h"
#include "buffer.h"
#include "lispbm.h"
#include "lbm_flat_value.h"
#include "platform_thread.h"
#include "platform_mutex.h"

// ////////////////////////////////////////////////////////////
// Status message ring buffers (mirrors comm_can.c on device)

#define CAN_STATUS_MSGS_TO_STORE 10

static can_status_msg   stat1[CAN_STATUS_MSGS_TO_STORE];
static can_status_msg_2 stat2[CAN_STATUS_MSGS_TO_STORE];
static can_status_msg_3 stat3[CAN_STATUS_MSGS_TO_STORE];
static can_status_msg_4 stat4[CAN_STATUS_MSGS_TO_STORE];
static can_status_msg_5 stat5[CAN_STATUS_MSGS_TO_STORE];
static can_status_msg_6 stat6[CAN_STATUS_MSGS_TO_STORE];

static lbm_mutex_t stat_mutex;

// ////////////////////////////////////////////////////////////
// Pending response slot (one outstanding request at a time).
#define PENDING_DATA_MAX 256

static volatile lbm_cid pending_cid    = -1;
static volatile bool    pending_active = false;
static volatile uint8_t pending_cmd    = 0;
static volatile int     pending_id     = -1;
static lbm_mutex_t      pending_mutex;

// ////////////////////////////////////////////////////////////
// Pre-registered symbol IDs

static lbm_uint sym_rpm;
static lbm_uint sym_current;
static lbm_uint sym_duty;
static lbm_uint sym_temp_fet;
static lbm_uint sym_temp_motor;
static lbm_uint sym_current_in;
static lbm_uint sym_pid_pos;
static lbm_uint sym_v_in;
static lbm_uint sym_tacho;
static lbm_uint sym_fw_major;
static lbm_uint sym_fw_minor;
static lbm_uint sym_hw_type;
static lbm_uint sym_event_can_eid;

// ////////////////////////////////////////////////////////////
// Serial / packet state

static int           serial_fd = -1;
static volatile bool rx_running = false;
static PACKET_STATE_t pkt_state;
static lbm_mutex_t    send_mutex;
static lbm_thread_t   rx_thread;

static volatile vesc_can_relay_func_t relay_func = NULL;

// ////////////////////////////////////////////////////////////
// Serial and packet layer

static void serial_send(unsigned char *data, unsigned int len) {
  if (serial_fd < 0) return;
  ssize_t r = write(serial_fd, data, (size_t)len);
  (void)r;
}

static void send_packet(uint8_t *data, unsigned int len) {
  lbm_mutex_lock(&send_mutex);
  packet_send_packet(data, len, &pkt_state);
  lbm_mutex_unlock(&send_mutex);
}

// ////////////////////////////////////////////////////////////
// CAN frame decoding (status messages arriving from nanolog)

static void decode_can_frame(uint32_t eid, uint8_t *data, int len) {
  uint8_t id  = (uint8_t)(eid & 0xFF);
  uint8_t cmd = (uint8_t)(eid >> 8);
  int32_t ind = 0;

  lbm_mutex_lock(&stat_mutex);
  switch (cmd) {
  case CAN_PACKET_STATUS:
    for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
      if (stat1[i].id == (int)id || stat1[i].id == -1) {
        ind = 0;
        stat1[i].id      = (int)id;
        stat1[i].rpm     = (float)buffer_get_int32(data, &ind);
        stat1[i].current = (float)buffer_get_int16(data, &ind) / 10.0f;
        stat1[i].duty    = (float)buffer_get_int16(data, &ind) / 1000.0f;
        break;
      }
    }
    break;
  case CAN_PACKET_STATUS_2:
    for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
      if (stat2[i].id == (int)id || stat2[i].id == -1) {
        ind = 0;
        stat2[i].id                = (int)id;
        stat2[i].amp_hours         = (float)buffer_get_int32(data, &ind) / 1e4f;
        stat2[i].amp_hours_charged = (float)buffer_get_int32(data, &ind) / 1e4f;
        break;
      }
    }
    break;
  case CAN_PACKET_STATUS_3:
    for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
      if (stat3[i].id == (int)id || stat3[i].id == -1) {
        ind = 0;
        stat3[i].id                 = (int)id;
        stat3[i].watt_hours         = (float)buffer_get_int32(data, &ind) / 1e4f;
        stat3[i].watt_hours_charged = (float)buffer_get_int32(data, &ind) / 1e4f;
        break;
      }
    }
    break;
  case CAN_PACKET_STATUS_4:
    for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
      if (stat4[i].id == (int)id || stat4[i].id == -1) {
        ind = 0;
        stat4[i].id          = (int)id;
        stat4[i].temp_fet    = (float)buffer_get_int16(data, &ind) / 10.0f;
        stat4[i].temp_motor  = (float)buffer_get_int16(data, &ind) / 10.0f;
        stat4[i].current_in  = (float)buffer_get_int16(data, &ind) / 10.0f;
        stat4[i].pid_pos_now = (float)buffer_get_int16(data, &ind) / 50.0f;
        break;
      }
    }
    break;
  case CAN_PACKET_STATUS_5:
    for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
      if (stat5[i].id == (int)id || stat5[i].id == -1) {
        ind = 0;
        stat5[i].id          = (int)id;
        stat5[i].tacho_value = buffer_get_int32(data, &ind);
        stat5[i].v_in        = (float)buffer_get_int16(data, &ind) / 10.0f;
        break;
      }
    }
    break;
  case CAN_PACKET_STATUS_6:
    for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
      if (stat6[i].id == (int)id || stat6[i].id == -1) {
        ind = 0;
        stat6[i].id    = (int)id;
        stat6[i].adc_1 = buffer_get_float16(data, 1e3f, &ind);
        stat6[i].adc_2 = buffer_get_float16(data, 1e3f, &ind);
        stat6[i].adc_3 = buffer_get_float16(data, 1e3f, &ind);
        stat6[i].ppm   = buffer_get_float16(data, 1e3f, &ind);
        break;
      }
    }
    break;
  default:
    break;
  }
  (void)len;
  lbm_mutex_unlock(&stat_mutex);

  lbm_flat_value_t fv;
  if (lbm_start_flatten(&fv, 40 + (size_t)len)) {
    bool ok =
      f_cons(&fv) && f_sym(&fv, sym_event_can_eid) &&
      f_cons(&fv) && f_u32(&fv, eid) &&
      f_cons(&fv) && f_lbm_array(&fv, (uint32_t)len, data) &&
      f_sym(&fv, SYM_NIL);
    if (!ok || !lbm_event(&fv)) lbm_free(fv.buf);
  }
}

// ////////////////////////////////////////////////////////////
// Response parsers
static void parse_fw_version(lbm_cid cid, uint8_t *d, int dlen) {
  if (dlen < 2) { lbm_unblock_ctx_unboxed(cid, ENC_SYM_NIL); return; }

  uint8_t major   = d[0];
  uint8_t minor   = d[1];

  int ind = 2;
  while (ind < dlen && d[ind] != 0) ind++;
  ind++;        // null terminator
  ind += 12;    // uuid
  ind += 1;     // pairing_done
  ind += 1;     // fw_test_version
  uint8_t hw_type = (ind < dlen) ? d[ind] : 0;
  // hw_type: 0 = VESC, 1 = VESC_BMS, 2 = VESC_EXPRESS

  lbm_flat_value_t fv;
  if (!lbm_start_flatten(&fv, 100)) { lbm_unblock_ctx_unboxed(cid, ENC_SYM_NIL); return; }

  bool ok = true;
  ok = ok && f_cons(&fv) && f_cons(&fv) && f_sym(&fv, sym_fw_major) && f_i(&fv, (lbm_int)major);
  ok = ok && f_cons(&fv) && f_cons(&fv) && f_sym(&fv, sym_fw_minor) && f_i(&fv, (lbm_int)minor);
  ok = ok && f_cons(&fv) && f_cons(&fv) && f_sym(&fv, sym_hw_type)  && f_i(&fv, (lbm_int)hw_type);
  ok = ok && f_sym(&fv, SYM_NIL);

  if (!ok || !lbm_finish_flatten(&fv)) { lbm_free(fv.buf); lbm_unblock_ctx_unboxed(cid, ENC_SYM_NIL); return; }
  if (!lbm_unblock_ctx(cid, &fv)) lbm_free(fv.buf);
}

static void parse_get_values(lbm_cid cid, int dev_id, uint8_t *d, int dlen) {
  if (dlen < 28) { lbm_unblock_ctx_unboxed(cid, ENC_SYM_NIL); return; }

  int32_t ind      = 0;
  float temp_fet   = buffer_get_float16(d, 1e1f, &ind);
  float temp_motor = buffer_get_float16(d, 1e1f, &ind);
  float current    = buffer_get_float32(d, 1e2f, &ind);
  float current_in = buffer_get_float32(d, 1e2f, &ind);
  ind += 4;  // skip id
  ind += 4;  // skip iq
  float duty = buffer_get_float16(d, 1e3f, &ind);
  float rpm  = buffer_get_float32(d, 1e0f, &ind);
  float v_in = buffer_get_float16(d, 1e1f, &ind);

  lbm_mutex_lock(&stat_mutex);
  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    if (stat1[i].id == dev_id || stat1[i].id == -1) {
      stat1[i].id      = dev_id;
      stat1[i].rpm     = rpm;
      stat1[i].current = current;
      stat1[i].duty    = duty;
      break;
    }
  }
  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    if (stat4[i].id == dev_id || stat4[i].id == -1) {
      stat4[i].id         = dev_id;
      stat4[i].temp_fet   = temp_fet;
      stat4[i].temp_motor = temp_motor;
      stat4[i].current_in = current_in;
      break;
    }
  }
  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    if (stat5[i].id == dev_id || stat5[i].id == -1) {
      stat5[i].id    = dev_id;
      stat5[i].v_in  = v_in;
      break;
    }
  }
  lbm_mutex_unlock(&stat_mutex);

  lbm_flat_value_t fv;
  if (!lbm_start_flatten(&fv, 200)) { lbm_unblock_ctx_unboxed(cid, ENC_SYM_NIL); return; }

#define FENTRY(sym, val) \
  ok = ok && f_cons(&fv) && f_cons(&fv) && f_sym(&fv, (sym)) && f_float(&fv, (val))

  bool ok = true;
  FENTRY(sym_rpm,        rpm);
  FENTRY(sym_current,    current);
  FENTRY(sym_duty,       duty);
  FENTRY(sym_temp_fet,   temp_fet);
  FENTRY(sym_temp_motor, temp_motor);
  FENTRY(sym_current_in, current_in);
  FENTRY(sym_v_in,       v_in);
  ok = ok && f_sym(&fv, SYM_NIL);

#undef FENTRY

  if (!ok || !lbm_finish_flatten(&fv)) { lbm_free(fv.buf); lbm_unblock_ctx_unboxed(cid, ENC_SYM_NIL); return; }
  if (!lbm_unblock_ctx(cid, &fv)) lbm_free(fv.buf);
}

// ////////////////////////////////////////////////////////////
// Single dispatch point

static void parse_response(uint8_t cmd, uint8_t *d, int dlen, lbm_cid cid, int dev_id) {
  switch (cmd) {
  case COMM_FW_VERSION: parse_fw_version(cid, d, dlen); break;
  case COMM_GET_VALUES: parse_get_values(cid, dev_id, d, dlen); break;
  default:
    lbm_unblock_ctx_unboxed(cid, ENC_SYM_NIL);
    break;
  }
}

// ////////////////////////////////////////////////////////////
// Incoming packet dispatch

static void process_packet(unsigned char *data, unsigned int len) {
  if (len < 1) return;
  uint8_t cmd = data[0];

  if (relay_func) {
    relay_func(data, len);
  }

  if (cmd == COMM_CAN_FWD_FRAME && len >= 6) {
    int32_t ind = 1;
    uint32_t eid = buffer_get_uint32(data, &ind);
    bool is_ext  = data[(size_t)ind++] != 0;
    if (is_ext) decode_can_frame(eid, data + ind, (int)(len - (unsigned int)ind));
    return;
  }

  if (cmd == COMM_LISP_RMSG && len >= 5) {
    int32_t ind = 1;
    uint32_t eid = buffer_get_uint32(data, &ind);
    decode_can_frame(eid, data + ind, (int)(len - (unsigned int)ind));
    return;
  }

  lbm_mutex_lock(&pending_mutex);
  if (!pending_active || cmd != pending_cmd) {
    lbm_mutex_unlock(&pending_mutex);
    return;
  }
  uint8_t local_data[PENDING_DATA_MAX];
  int dlen = (int)len - 1;
  if (dlen < 0) dlen = 0;
  if (dlen > PENDING_DATA_MAX) dlen = PENDING_DATA_MAX;
  memcpy(local_data, data + 1, (size_t)dlen);
  pending_active = false;
  lbm_cid cid    = pending_cid;
  int     dev_id = pending_id;
  pending_cid    = -1;
  pending_id     = -1;
  lbm_mutex_unlock(&pending_mutex);

  if (cid < 0) return;
  parse_response(cmd, local_data, dlen, cid, dev_id);
}

// ////////////////////////////////////////////////////////////
// RX thread

static void rx_thd(void *arg) {
  (void)arg;
  uint8_t byte;
  while (rx_running) {
    ssize_t n = read(serial_fd, &byte, 1);
    if (n == 1) {
      packet_process_byte(byte, &pkt_state);
    } else if (n < 0) {
      lbm_thread_sleep_us(1000);
    }
  }
}

// ////////////////////////////////////////////////////////////
// Serial port setup

static int open_serial(const char *port) {
  int fd = open(port, O_RDWR | O_NOCTTY | O_SYNC);
  if (fd < 0) return -1;

  struct termios tty;
  memset(&tty, 0, sizeof(tty));
  if (tcgetattr(fd, &tty) != 0) { close(fd); return -1; }

  cfsetospeed(&tty, B115200);
  cfsetispeed(&tty, B115200);

  tty.c_cflag  = (tty.c_cflag & ~(tcflag_t)CSIZE) | CS8;
  tty.c_cflag |= (CLOCAL | CREAD);
  tty.c_cflag &= ~(tcflag_t)(PARENB | PARODD | CSTOPB | CRTSCTS);
  tty.c_iflag &= ~(tcflag_t)(IGNBRK | IXON | IXOFF | IXANY);
  tty.c_lflag  = 0;
  tty.c_oflag  = 0;
  tty.c_cc[VMIN]  = 0;
  tty.c_cc[VTIME] = 1;  // 100 ms read timeout

  if (tcsetattr(fd, TCSANOW, &tty) != 0) { close(fd); return -1; }
  return fd;
}

// ////////////////////////////////////////////////////////////
// Internal send helpers

static void send_can_eid(uint32_t eid, uint8_t *payload, int plen) {
  uint8_t buf[16];
  int32_t ind = 0;
  buf[ind++] = COMM_CAN_FWD_FRAME;
  buffer_append_uint32(buf, eid, &ind);
  buf[ind++] = 1; // is_ext
  memcpy(buf + ind, payload, (size_t)plen);
  ind += plen;
  send_packet(buf, (unsigned int)ind);
}

// ////////////////////////////////////////////////////////////
// Status lookup helpers

static bool stat1_for(int id, can_status_msg *out) {
  lbm_mutex_lock(&stat_mutex);
  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    if (stat1[i].id == id) { *out = stat1[i]; lbm_mutex_unlock(&stat_mutex); return true; }
  }
  lbm_mutex_unlock(&stat_mutex);
  return false;
}

static bool stat4_for(int id, can_status_msg_4 *out) {
  lbm_mutex_lock(&stat_mutex);
  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    if (stat4[i].id == id) { *out = stat4[i]; lbm_mutex_unlock(&stat_mutex); return true; }
  }
  lbm_mutex_unlock(&stat_mutex);
  return false;
}

static bool stat5_for(int id, can_status_msg_5 *out) {
  lbm_mutex_lock(&stat_mutex);
  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    if (stat5[i].id == id) { *out = stat5[i]; lbm_mutex_unlock(&stat_mutex); return true; }
  }
  lbm_mutex_unlock(&stat_mutex);
  return false;
}

static bool stat6_for(int id, can_status_msg_6 *out) {
  lbm_mutex_lock(&stat_mutex);
  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    if (stat6[i].id == id) { *out = stat6[i]; lbm_mutex_unlock(&stat_mutex); return true; }
  }
  lbm_mutex_unlock(&stat_mutex);
  return false;
}

// ////////////////////////////////////////////////////////////
// Public relay / send API

bool lbm_vesc_can_is_connected(void) {
  return serial_fd >= 0 && rx_running;
}

void lbm_vesc_can_send_raw(unsigned char *data, unsigned int len) {
  send_packet(data, len);
}

void lbm_vesc_can_set_relay(vesc_can_relay_func_t f) {
  relay_func = f;
}

// ////////////////////////////////////////////////////////////
// Connect to can bridge (from repl --can= startup)

bool lbm_vesc_can_connect(const char *port) {
  if (serial_fd >= 0) {
    rx_running = false;
    lbm_thread_destroy(&rx_thread);
    close(serial_fd);
    serial_fd = -1;
  }

  int fd = open_serial(port);
  if (fd < 0) return false;

  serial_fd  = fd;
  packet_init(serial_send, process_packet, &pkt_state);
  rx_running = true;

  if (!lbm_thread_create(&rx_thread, "vesc_can_rx", rx_thd, NULL,
                          LBM_THREAD_PRIO_NORMAL, 4096)) {
    rx_running = false;
    close(serial_fd);
    serial_fd = -1;
    return false;
  }
  return true;
}

// ////////////////////////////////////////////////////////////
// Extensions

static lbm_value ext_can_duty(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0) return ENC_SYM_TERROR;
  uint8_t buf[4]; int32_t ind = 0;
  buffer_append_int32(buf, (int32_t)(lbm_dec_as_float(args[1]) * 100000.0f), &ind);
  send_can_eid(lbm_dec_as_u32(args[0]) | ((uint32_t)CAN_PACKET_SET_DUTY << 8), buf, ind);
  return ENC_SYM_TRUE;
}

static lbm_value ext_can_current(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0) return ENC_SYM_TERROR;
  uint8_t buf[4]; int32_t ind = 0;
  buffer_append_int32(buf, (int32_t)(lbm_dec_as_float(args[1]) * 1000.0f), &ind);
  send_can_eid(lbm_dec_as_u32(args[0]) | ((uint32_t)CAN_PACKET_SET_CURRENT << 8), buf, ind);
  return ENC_SYM_TRUE;
}

static lbm_value ext_can_current_rel(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0) return ENC_SYM_TERROR;
  uint8_t buf[4]; int32_t ind = 0;
  buffer_append_float32(buf, lbm_dec_as_float(args[1]), 1e5f, &ind);
  send_can_eid(lbm_dec_as_u32(args[0]) | ((uint32_t)CAN_PACKET_SET_CURRENT_REL << 8), buf, ind);
  return ENC_SYM_TRUE;
}

static lbm_value ext_can_brake(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0) return ENC_SYM_TERROR;
  uint8_t buf[4]; int32_t ind = 0;
  buffer_append_int32(buf, (int32_t)(lbm_dec_as_float(args[1]) * 1000.0f), &ind);
  send_can_eid(lbm_dec_as_u32(args[0]) | ((uint32_t)CAN_PACKET_SET_CURRENT_BRAKE << 8), buf, ind);
  return ENC_SYM_TRUE;
}

static lbm_value ext_can_brake_rel(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0) return ENC_SYM_TERROR;
  uint8_t buf[4]; int32_t ind = 0;
  buffer_append_float32(buf, lbm_dec_as_float(args[1]), 1e5f, &ind);
  send_can_eid(lbm_dec_as_u32(args[0]) | ((uint32_t)CAN_PACKET_SET_CURRENT_BRAKE_REL << 8), buf, ind);
  return ENC_SYM_TRUE;
}

static lbm_value ext_can_rpm(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0) return ENC_SYM_TERROR;
  uint8_t buf[4]; int32_t ind = 0;
  buffer_append_int32(buf, (int32_t)lbm_dec_as_float(args[1]), &ind);
  send_can_eid(lbm_dec_as_u32(args[0]) | ((uint32_t)CAN_PACKET_SET_RPM << 8), buf, ind);
  return ENC_SYM_TRUE;
}

static lbm_value ext_can_pos(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0) return ENC_SYM_TERROR;
  uint8_t buf[4]; int32_t ind = 0;
  buffer_append_int32(buf, (int32_t)(lbm_dec_as_float(args[1]) * 1000000.0f), &ind);
  send_can_eid(lbm_dec_as_u32(args[0]) | ((uint32_t)CAN_PACKET_SET_POS << 8), buf, ind);
  return ENC_SYM_TRUE;
}

// ////////////////////////////////////////////////////////////
// Extensions

static lbm_value ext_can_send_eid(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0 || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[1]);
  int dlen = (int)arr->size; if (dlen > 8) dlen = 8;
  uint8_t buf[16]; int32_t ind = 0;
  buf[ind++] = COMM_CAN_FWD_FRAME;
  buffer_append_uint32(buf, lbm_dec_as_u32(args[0]), &ind);
  buf[ind++] = 1;
  memcpy(buf + ind, arr->data, (size_t)dlen); ind += dlen;
  send_packet(buf, (unsigned int)ind);
  return ENC_SYM_TRUE;
}

static lbm_value ext_can_send_sid(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0 || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[1]);
  int dlen = (int)arr->size; if (dlen > 8) dlen = 8;
  uint8_t buf[16]; int32_t ind = 0;
  buf[ind++] = COMM_CAN_FWD_FRAME;
  buffer_append_uint32(buf, lbm_dec_as_u32(args[0]), &ind);
  buf[ind++] = 0;
  memcpy(buf + ind, arr->data, (size_t)dlen); ind += dlen;
  send_packet(buf, (unsigned int)ind);
  return ENC_SYM_TRUE;
}

// Forward a VESC protocol buffer to a CAN device.
static lbm_value ext_can_cmd(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || serial_fd < 0 || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[1]);
  unsigned int total = (unsigned int)arr->size + 2u;
  uint8_t *buf = malloc(total);
  if (!buf) return ENC_SYM_NIL;
  buf[0] = COMM_FORWARD_CAN;
  buf[1] = (uint8_t)lbm_dec_as_u32(args[0]);
  memcpy(buf + 2, arr->data, arr->size);
  send_packet(buf, total);
  free(buf);
  return ENC_SYM_TRUE;
}

// ////////////////////////////////////////////////////////////
// Extensions

static lbm_value ext_can_get_rpm(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg s; if (!stat1_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.rpm);
}

static lbm_value ext_can_get_current(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg s; if (!stat1_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.current);
}

static lbm_value ext_can_get_current_dir(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg s; if (!stat1_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.current >= 0.0f ? 1.0f : -1.0f);
}

static lbm_value ext_can_get_duty(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg s; if (!stat1_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.duty);
}

static lbm_value ext_can_get_temp_fet(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg_4 s; if (!stat4_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.temp_fet);
}

static lbm_value ext_can_get_temp_motor(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg_4 s; if (!stat4_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.temp_motor);
}

static lbm_value ext_can_get_current_in(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg_4 s; if (!stat4_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.current_in);
}

static lbm_value ext_can_get_vin(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg_5 s; if (!stat5_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.v_in);
}

static lbm_value ext_can_get_ppm(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg_6 s; if (!stat6_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  return lbm_enc_float(s.ppm);
}

// (can-get-adc id channel) — channel 0/1/2
static lbm_value ext_can_get_adc(lbm_value *args, lbm_uint argn) {
  if (argn != 2) return ENC_SYM_TERROR;
  can_status_msg_6 s; if (!stat6_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  int ch = (int)lbm_dec_as_i32(args[1]);
  float v = (ch == 0) ? s.adc_1 : (ch == 1) ? s.adc_2 : s.adc_3;
  return lbm_enc_float(v);
}

static lbm_value ext_can_get_status(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg s; if (!stat1_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  lbm_value rpm  = lbm_cons(lbm_enc_sym(sym_rpm),     lbm_enc_float(s.rpm));
  lbm_value cur  = lbm_cons(lbm_enc_sym(sym_current),  lbm_enc_float(s.current));
  lbm_value duty = lbm_cons(lbm_enc_sym(sym_duty),     lbm_enc_float(s.duty));
  return lbm_cons(rpm, lbm_cons(cur, lbm_cons(duty, ENC_SYM_NIL)));
}

static lbm_value ext_can_get_status_4(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg_4 s; if (!stat4_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  lbm_value tf  = lbm_cons(lbm_enc_sym(sym_temp_fet),   lbm_enc_float(s.temp_fet));
  lbm_value tm  = lbm_cons(lbm_enc_sym(sym_temp_motor),  lbm_enc_float(s.temp_motor));
  lbm_value ci  = lbm_cons(lbm_enc_sym(sym_current_in),  lbm_enc_float(s.current_in));
  lbm_value pos = lbm_cons(lbm_enc_sym(sym_pid_pos),     lbm_enc_float(s.pid_pos_now));
  return lbm_cons(tf, lbm_cons(tm, lbm_cons(ci, lbm_cons(pos, ENC_SYM_NIL))));
}

static lbm_value ext_can_get_status_5(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  can_status_msg_5 s; if (!stat5_for((int)lbm_dec_as_i32(args[0]), &s)) return ENC_SYM_NIL;
  lbm_value vin   = lbm_cons(lbm_enc_sym(sym_v_in),  lbm_enc_float(s.v_in));
  lbm_value tacho = lbm_cons(lbm_enc_sym(sym_tacho), lbm_enc_i(s.tacho_value));
  return lbm_cons(vin, lbm_cons(tacho, ENC_SYM_NIL));
}

// ////////////////////////////////////////////////////////////
// Ping
static bool do_ping(int id, int timeout_ms) {
  lbm_mutex_lock(&pending_mutex);
  pending_active = false;  // reset any stale timeout from a previous blocking call
  pending_cid    = -1;     // signal: usleep path, no blocking ctx
  pending_cmd    = COMM_FW_VERSION;
  pending_active = true;
  lbm_mutex_unlock(&pending_mutex);

  uint8_t pkt[3] = { COMM_FORWARD_CAN, (uint8_t)id, COMM_FW_VERSION };
  send_packet(pkt, 3);

  while (timeout_ms-- > 0 && pending_active) usleep(1000);

  lbm_mutex_lock(&pending_mutex);
  bool got = !pending_active;
  pending_active = false;  // clean up in case of timeout
  lbm_mutex_unlock(&pending_mutex);
  return got;
}

static lbm_value ext_can_ping(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || serial_fd < 0) return ENC_SYM_TERROR;
  int id = (int)lbm_dec_as_i32(args[0]);

  lbm_mutex_lock(&pending_mutex);
  pending_active = false; 
  pending_cid    = lbm_get_current_cid();
  pending_cmd    = COMM_FW_VERSION;
  pending_active = true;
  lbm_mutex_unlock(&pending_mutex);

  uint8_t pkt[3] = { COMM_FORWARD_CAN, (uint8_t)id, COMM_FW_VERSION };
  send_packet(pkt, 3);

  lbm_block_ctx_from_extension_timeout(0.2f);
  return ENC_SYM_NIL; 
}

static lbm_value ext_can_scan(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  if (serial_fd < 0) return ENC_SYM_NIL;
  lbm_value found = ENC_SYM_NIL;
  for (int id = 127; id >= 0; id--) {
    if (do_ping(id, 50)) {
      found = lbm_cons(lbm_enc_i(id), found);
    }
  }
  return found;
}

static lbm_value ext_can_fw_version(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || serial_fd < 0) return ENC_SYM_TERROR;
  int id = (int)lbm_dec_as_i32(args[0]);

  lbm_mutex_lock(&pending_mutex);
  pending_active = false;
  pending_cid    = lbm_get_current_cid();
  pending_cmd    = COMM_FW_VERSION;
  pending_active = true;
  lbm_mutex_unlock(&pending_mutex);

  uint8_t pkt[3] = { COMM_FORWARD_CAN, (uint8_t)id, COMM_FW_VERSION };
  send_packet(pkt, 3);

  lbm_block_ctx_from_extension_timeout(0.5f);
  return ENC_SYM_NIL;
}

static lbm_value ext_can_get_values(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || serial_fd < 0) return ENC_SYM_TERROR;
  int id = (int)lbm_dec_as_i32(args[0]);

  lbm_mutex_lock(&pending_mutex);
  pending_active = false;
  pending_cid    = lbm_get_current_cid();
  pending_cmd    = COMM_GET_VALUES;
  pending_id     = id;
  pending_active = true;
  lbm_mutex_unlock(&pending_mutex);

  uint8_t pkt[3] = { COMM_FORWARD_CAN, (uint8_t)id, COMM_GET_VALUES };
  send_packet(pkt, 3);

  lbm_block_ctx_from_extension_timeout(0.5f);
  return ENC_SYM_NIL;
}

// ////////////////////////////////////////////////////////////
// STUBING of yet not implemented stuff.

// ////////////////////////////////////////////////////////////
// CAN relay script — uploaded to bridge VESC to forward
// directed CAN frames back over USB as COMM_LISP_RMSG packets.
// Payload format: eid(4 bytes big-endian) + CAN data bytes.

static const char relay_script[] =
  "(defun fwd (e)\n"
  "  (match e\n"
  "    ((event-can-eid id data)\n"
  "     (let ((b (bufcreate (+ 4 (buflen data)))))\n"
  "       { (bufset-u32 b 0 id)\n"
  "         (bufcpy b 4 data 0 (buflen data))\n"
  "         (send-data b) }))\n"
  "    (_ nil)))\n"
  "(event-register-handler (spawn fwd))\n"
  "(event-enable 'event-can-eid)\n";

static lbm_value ext_can_upload_relay(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  if (serial_fd < 0) return ENC_SYM_NIL;

  uint8_t erase_buf[1] = { (uint8_t)COMM_LISP_ERASE_CODE };
  send_packet(erase_buf, 1);

  uint32_t slen = (uint32_t)(sizeof(relay_script) - 1);
  uint8_t *wbuf = malloc(5u + slen);
  if (!wbuf) return ENC_SYM_NIL;
  int32_t ind = 0;
  wbuf[ind++] = (uint8_t)COMM_LISP_WRITE_CODE;
  buffer_append_uint32(wbuf, 0, &ind);
  memcpy(wbuf + ind, relay_script, slen);
  send_packet(wbuf, 5u + slen);
  free(wbuf);

  uint8_t run_buf[2] = { (uint8_t)COMM_LISP_SET_RUNNING, 1 };
  send_packet(run_buf, 2);

  return ENC_SYM_TRUE;
}

static lbm_value ext_stub(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_NIL;
}

static lbm_value ext_can_local_id(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_i(0);
}

// ////////////////////////////////////////////////////////////
// Public API

void lbm_vesc_can_register_extensions(void) {
  lbm_add_extension("canset-duty",            ext_can_duty);
  lbm_add_extension("canset-current",         ext_can_current);
  lbm_add_extension("canset-current-rel",     ext_can_current_rel);
  lbm_add_extension("canset-brake",           ext_can_brake);
  lbm_add_extension("canset-brake-rel",       ext_can_brake_rel);
  lbm_add_extension("canset-rpm",             ext_can_rpm);
  lbm_add_extension("canset-pos",             ext_can_pos);

  lbm_add_extension("can-send-eid",           ext_can_send_eid);
  lbm_add_extension("can-send-sid",           ext_can_send_sid);
  lbm_add_extension("can-cmd",                ext_can_cmd);

  lbm_add_extension("canget-rpm",             ext_can_get_rpm);
  lbm_add_extension("canget-current",         ext_can_get_current);
  lbm_add_extension("canget-current-dir",     ext_can_get_current_dir);
  lbm_add_extension("canget-current-in",      ext_can_get_current_in);
  lbm_add_extension("canget-duty",            ext_can_get_duty);
  lbm_add_extension("canget-temp-fet",        ext_can_get_temp_fet);
  lbm_add_extension("canget-temp-motor",      ext_can_get_temp_motor);
  lbm_add_extension("canget-vin",             ext_can_get_vin);
  lbm_add_extension("canget-ppm",             ext_can_get_ppm);
  lbm_add_extension("canget-adc",             ext_can_get_adc);

  lbm_add_extension("canget-status",           ext_can_get_status);
  lbm_add_extension("canget-status-4",         ext_can_get_status_4);
  lbm_add_extension("canget-status-5",         ext_can_get_status_5);

  lbm_add_extension("send-bms-can",           ext_stub);
  lbm_add_extension("can-msg-age",            ext_stub);
  lbm_add_extension("canget-speed",           ext_stub);
  lbm_add_extension("canget-dist",            ext_stub);
  lbm_add_extension("can-list-devs",          ext_stub);
  lbm_add_extension("can-local-id",          ext_can_local_id);
  lbm_add_extension("can-update-baud",       ext_stub);
  lbm_add_extension("can-start",             ext_stub);
  lbm_add_extension("can-stop",              ext_stub);
  lbm_add_extension("can-use-vesc",          ext_stub);
  lbm_add_extension("can-scan",              ext_can_scan);
  lbm_add_extension("can-ping",              ext_can_ping);
  lbm_add_extension("can-fw-version",        ext_can_fw_version);
  lbm_add_extension("canget-values",          ext_can_get_values);
  lbm_add_extension("can-recv-sid",          ext_stub);
  lbm_add_extension("can-recv-eid",          ext_stub);
  lbm_add_extension("canmsg-recv",           ext_stub);
  lbm_add_extension("canmsg-send",           ext_stub);
  lbm_add_extension("uavcan-last-rawcmd",    ext_stub);
  lbm_add_extension("uavcan-last-rpmcmd",    ext_stub);
  lbm_add_extension("can-upload-relay",      ext_can_upload_relay);
}

void lbm_vesc_can_init(void) {
  lbm_mutex_init(&send_mutex);
  lbm_mutex_init(&stat_mutex);
  lbm_mutex_init(&pending_mutex);
  pending_active = false;
  pending_cid    = -1;
  pending_id     = -1;

  lbm_add_symbol("fw-major",        &sym_fw_major);
  lbm_add_symbol("fw-minor",        &sym_fw_minor);
  lbm_add_symbol("hw-type",         &sym_hw_type);
  lbm_add_symbol("event-can-eid",   &sym_event_can_eid);
  lbm_add_symbol("rpm",        &sym_rpm);
  lbm_add_symbol("current",    &sym_current);
  lbm_add_symbol("duty",       &sym_duty);
  lbm_add_symbol("temp-fet",   &sym_temp_fet);
  lbm_add_symbol("temp-motor", &sym_temp_motor);
  lbm_add_symbol("current-in", &sym_current_in);
  lbm_add_symbol("pid-pos",    &sym_pid_pos);
  lbm_add_symbol("v-in",       &sym_v_in);
  lbm_add_symbol("tacho",      &sym_tacho);

  for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
    stat1[i].id = -1; stat2[i].id = -1; stat3[i].id = -1;
    stat4[i].id = -1; stat5[i].id = -1; stat6[i].id = -1;
  }
}
