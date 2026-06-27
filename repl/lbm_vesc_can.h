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

#ifndef LBM_VESC_CAN_H_
#define LBM_VESC_CAN_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

void lbm_vesc_can_init(void);
void lbm_vesc_can_register_extensions(void);
bool lbm_vesc_can_connect(const char *port);
bool lbm_vesc_can_is_connected(void);
void lbm_vesc_can_send_raw(unsigned char *data, unsigned int len);

typedef void (*vesc_can_relay_func_t)(unsigned char *data, unsigned int len);
void lbm_vesc_can_set_relay(vesc_can_relay_func_t f);

#ifdef __cplusplus
}
#endif
#endif
