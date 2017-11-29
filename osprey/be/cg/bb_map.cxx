/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* =======================================================================
 * =======================================================================
 *
 *  Module: bb_map.c
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:20-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.bb_map.cxx $
 *
 *  Revision comments:
 *
 *  3-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Implementation of BB_MAPs.  See bb_map.h for interface description.
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef _KEEP_RCS_ID
static char *source_file = __FILE__;
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.bb_map.cxx $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "bb.h"
#include "bb_map.h"

#define BB_EXPANSION_FACTOR 2

/* List of deleted maps */
static BB_MAP free_maps;

/* Link free map list internally.  Assumes all maps have length > 0. */
#define BB_MAP_next(map) ((map)->values.ptr[0])


void BB_MAP_Init(void)
{
  free_maps = NULL;
}


void BB_MAP_Delete(BB_MAP map)
{
  DevAssert(!map->deleted, ("BB_MAP 0x%x already deleted", map));
  /* Just put <map> on front of free list. */
  BB_MAP_next(map) = free_maps;
  map->deleted = TRUE;
  free_maps = map;
}


inline UINT16 sizeof_kind(_BB_MAP_KIND kind)
{
  switch (kind) {
  case _BB_MAP_PTR:
    return sizeof(void *);
  case _BB_MAP_I32:
    return sizeof(mINT32);
  case _BB_MAP_I64:
    return sizeof(mINT64);
  default:
    FmtAssert(FALSE, ("sizeof_kind passed bad <kind>"));
  }

  /* Unreachable since FmtAssert will terminate, but
   * most compilers won't figure that out, so this prevents
   * "missing return" warnings.
   */
  return 0;
}


BB_MAP BB_MAP_create_kind(_BB_MAP_KIND kind)
{
  BB_MAP result, prev;
  UINT32 length = (BB_MAP_idx_max + 1) * BB_EXPANSION_FACTOR;

  /* See whether we can find one in free list */
  prev = NULL;
  for (result = free_maps; result; result = (BB_MAP) BB_MAP_next(result)) {
    if (sizeof_kind(kind) == sizeof_kind(result->kind)) {
      if (prev)
	BB_MAP_next(prev) = BB_MAP_next(result);
      else
	free_maps = (BB_MAP) BB_MAP_next(result);
      result->deleted = FALSE;
      result->kind = kind;
      result->gen += 1;
      if (result->gen == 0) {
	/* Generation count overflow.  Should be extremely rare. */
	DevWarn("(Performance) BB_MAP gen overflow - zeroing.");
	BZERO(result->value_gens, sizeof(mUINT32) * result->length);
	BZERO(result->values.ptr, sizeof_kind(kind) * result->length);
      }
      return result;
    }
    prev = result;
  }

  /* Allocate a new one */
  result = TYPE_P_ALLOC(struct bb_map);
  result->deleted = FALSE;
  result->kind = kind;
  result->length = length;
  result->gen = 0;
  result->value_gens = TYPE_P_ALLOC_N(mUINT32, length);
  switch (kind) {
  case _BB_MAP_PTR:
    result->values.ptr = TYPE_P_ALLOC_N(void *, length);
    break;
  case _BB_MAP_I32:
    result->values.i32 = TYPE_P_ALLOC_N(mINT32, length);
    break;
  case _BB_MAP_I64:
    result->values.i64 = TYPE_P_ALLOC_N(mINT64, length);
    break;
  default:
    FmtAssert(FALSE, ("BB_MAP_create_kind passed bad <kind>"));
  }

  return result;
}


void BB_MAP_grow(BB_MAP map, BB *bb)
{
  INT32 new_length = (BB_MAP_idx_max + 1) * BB_EXPANSION_FACTOR;

  if (BB_map_idx(bb) > new_length) {
    DevWarn("BB_MAP_idx_max not up to date");
    new_length = BB_map_idx(bb) + 1;
  }

  /* TODO: Realloc'ing in a shared MEM_POOL is slow.  Experiment with
   * using a separate MEM_POOL for each BB_MAP.
   */
   
  map->value_gens = TYPE_MEM_POOL_REALLOC_N(mUINT32, &MEM_phase_pool,
					    map->value_gens, map->length,
					    new_length);

  switch (map->kind) {
  case _BB_MAP_PTR:
    map->values.ptr =
      TYPE_MEM_POOL_REALLOC_N(void *, &MEM_phase_pool, map->values.ptr,
			      map->length, new_length);
    break;
  case _BB_MAP_I32:
    map->values.i32 =
      TYPE_MEM_POOL_REALLOC_N(INT32, &MEM_phase_pool, map->values.i32,
			      map->length, new_length);
    break;
  case _BB_MAP_I64:
    map->values.i64 =
      TYPE_MEM_POOL_REALLOC_N(INT64, &MEM_phase_pool, map->values.i64,
			      map->length, new_length);
    break;
  default:
    FmtAssert(FALSE, ("BB_MAP_grow passed map with bad <kind>"));
  }

  map->length = new_length;
}
