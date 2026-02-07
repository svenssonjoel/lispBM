/*
    Copyright 2024, 2025, 2026 Joel Svensson        svenssonjoel@yahoo.se

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


/*
  lbm_defrag_mem.h/c provides a memory area that is compacted when
  needed.  This means that allocations in the lbm_defrag_mem must be
  able to find all places that reference it and update those
  locations.

  Here an earlier design choice came in useful! LispBM arrays are
  allocated in a way such that the actual data in the array is only
  referenced through a single pointer.  This pointer is in a
  heap-cell. The heap-cell itself can then be referenced from many
  locations, but we never need to update those at all! So, because of
  this design choice we just need to keep track of a single
  cell-reference in the compactible memory allocation to be able to
  update the reference upon a compaction.

  **INVARIANT**
  This comes with one invariant that must be upheld in all operations,
  that an array-reference-cell is never duplicated.

  A ByteArray header consists of a size and a data-pointer. So the
  general layout of a byteArray in memory is:

     [size, data-pointer]->[data ..., padding]

  The padding is either there or not depending on the data being a
  multiple of the word size or not.

  In defrag memory an allocation of a ByteArray is structured as
  follows.

     [size, data-pointer, cell-back-ptr, data ..., padding]
               |                           |
               -----------------------------

  Note that the allocation in the defrag memory is large enough to
  hold all the bytes of data, the entire header as well as the
  cell-back-ptr consecutively.

  It is important that the [size, data-pointer]..[data] areas are
  compatible with the normal ByteArray header struct so that all the
  functions that operate on regular ByteArrays also can operate
  unchanged on ByteArrays stored in defrag memory.
 */

#ifndef LBM_DEFRAG_MEM_H_
#define LBM_DEFRAG_MEM_H_

#include <heap.h>
#include <stdint.h>

extern lbm_value lbm_defrag_mem_create(lbm_uint nbytes);
extern void lbm_defrag_mem_destroy(lbm_uint *ptr);
extern lbm_value lbm_defrag_mem_alloc(lbm_uint *defrag_mem, lbm_uint nbytes);
//extern lbm_value lbm_defrag_mem_alloc_lisparray(lbm_uint *defrag_mem, lbm_uint elts);
extern void lbm_defrag_mem_free(lbm_uint* data);

static inline bool lbm_defrag_mem_valid(lbm_value arr) {
  return !(lbm_is_symbol_nil(lbm_car(arr)));
}

static inline bool lbm_is_defrag_mem(lbm_value x) {
  lbm_type t = lbm_type_of(x);
  return ((t == LBM_TYPE_DEFRAG_MEM) && lbm_defrag_mem_valid(x)) ;
}


#endif
