/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

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

#ifndef LBM_IMAGE_H_
#define LBM_IMAGE_H_

typedef bool (*lbm_image_write_fun)(uint32_t index, uint8_t data);
typedef bool (*lbm_image_clear_fun)(void);

// C interface to image manipulation
uint8_t *lbm_image_get_image(void);
uint32_t lbm_image_get_size(void);
bool lbm_image_has_startup(void);
uint8_t *lbm_image_startup_address(void);
uint32_t lbm_image_startup_size(void);
bool lbm_image_save_startup_fv(uint8_t *data, uint32_t size);
bool lbm_image_save_global_env(void);

bool lbm_image_is_empty(void);
void lbm_image_clear(void);
bool lbm_image_create_const_heap(uint32_t size_words);

// startup initialization
void lbm_image_set_callbacks(lbm_image_clear_fun   image_clear_fun,
                             lbm_image_write_fun   image_write_fun);

void lbm_image_init(uint8_t *             image_mem_addr,
                    uint32_t              max_index);

void lbm_image_boot(void);

#endif
