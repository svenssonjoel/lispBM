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
/** \file lbm_image.h */

#ifndef LBM_IMAGE_H_
#define LBM_IMAGE_H_

/**
 * lbm_image_write_fun function ptr.
 * \param data 32bit word to write.
 * \param index Position to write to.
 * \param const_heap True if writing to const heap (bottom of image) and false otherwise.
 */
typedef bool (*lbm_image_write_fun)(uint32_t data, int32_t index, bool const_heap);

// C interface to image manipulation
uint32_t *lbm_image_get_image(void);
lbm_value lbm_image_get_startup(void);
int32_t lbm_image_get_write_index(void);

uint32_t lbm_image_get_size(void);
bool lbm_image_has_startup(void);
bool lbm_image_has_extensions(void);
bool lbm_image_save_startup(lbm_value sym);
bool lbm_image_save_global_env(void);
bool lbm_image_save_extensions(void);
bool lbm_image_save_constant_heap_ix(void);
/**
 * Add a symbol to the image.
 * Symbols added to the image are restored upon image-boot.
 * \param name Symbol name.
 * \param id Symbol id.
 * \param symlist Ptr to rest of symbol list.
 * \return pointer to head of symbol list.
 */
lbm_uint *lbm_image_add_symbol(char *name, lbm_uint id, lbm_uint symlist);
/**
 * Add a symbol to the image and "link" it to a C address (variable).
 * \param name Symbol name.
 * \param id Symbol id.
 * \param symlist Ptr to rest of symbol list.
 * \param link ptr where to store the symbol ID on image-boot.
 * \return pointer to head of symbol list.
 */
lbm_uint *lbm_image_add_and_link_symbol(char *name, lbm_uint id, lbm_uint symlist, lbm_uint *link);

bool lbm_image_is_empty(void);

// startup initialization
void lbm_image_init(uint32_t *image_mem_addr,
                    uint32_t  image_size,
                    lbm_image_write_fun  image_write_fun);

/**
 * Create an image at the address and of the size given to lbm_image_init.
 * \param version_str a zero terminated version string or NULL.
 */
void lbm_image_create(char *version_str);
bool lbm_image_exists(void);
bool lbm_image_boot(void);

/**
 * Get the version string that was stored in the image.
 * If no version string was stored in the image, the result is NULL.
 * \return pointer to version string or NULL.
 */
char *lbm_image_get_version(void);

#endif
