/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
 
/*-*-c++-*-*/ 
/* =======================================================================
 * =======================================================================
 *
 *  Module: region_map.c
 *  $Revision: 1.1 $
 *  $Date: 2005/12/30 01:47:13 $
 *  $Author: weitang $
 *  $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/region_map.cxx,v $
 *
 *  Revision comments:
 *
 *  3-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Implementation of REGION_MAPs.  See region_map.h for interface description.
 *
 * =======================================================================
 * =======================================================================
 */

#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "region.h"
#include "region_map.h"

#define REGION_EXPANSION_FACTOR 2

/* List of deleted maps */
static REGION_MAP free_maps;

/* Link free map list internally.  Assumes all maps have length > 0. */
#define REGION_MAP_next(map) ((map)->values.ptr[0])


void REGION_MAP_Init(void)
{
  free_maps = NULL;
}


void REGION_MAP_Delete(REGION_MAP map)
{
  DevAssert(!map->deleted, ("REGION_MAP 0x%x already deleted", map));
  /* Just put <map> on front of free list. */
  REGION_MAP_next(map) = free_maps;
  map->deleted = TRUE;
  free_maps = map;
}


inline UINT16 sizeof_kind(_REGION_MAP_KIND kind)
{
  switch (kind) {
  case _REGION_MAP_PTR:
    return sizeof(void *);
  case _REGION_MAP_I32:
    return sizeof(mINT32);
  case _REGION_MAP_I64:
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


REGION_MAP REGION_MAP_create_kind(INT32 REGION_MAP_idx_max, _REGION_MAP_KIND kind)
{
  REGION_MAP result, prev;
  UINT32 length = (REGION_MAP_idx_max + 1) * REGION_EXPANSION_FACTOR;

  /* See whether we can find one in free list */
  prev = NULL;
  for (result = free_maps; result; result = (REGION_MAP) REGION_MAP_next(result)) {
    if (sizeof_kind(kind) == sizeof_kind(result->kind)) {
      if (prev)
	REGION_MAP_next(prev) = REGION_MAP_next(result);
      else
	free_maps = (REGION_MAP) REGION_MAP_next(result);
      result->deleted = FALSE;
      result->kind = kind;
      result->gen += 1;
      if (result->gen == 0) {
	/* Generation count overflow.  Should be extremely rare. */
	DevWarn("(Performance) REGION_MAP gen overflow - zeroing.");
	bzero(result->value_gens, sizeof(mUINT32) * result->length);
	bzero(result->values.ptr, sizeof_kind(kind) * result->length);
      }
      return result;
    }
    prev = result;
  }

  /* Allocate a new one */
  result = TYPE_P_ALLOC(struct region_map);
  result->deleted = FALSE;
  result->kind = kind;
  result->length = length;
  result->gen = 0;
  result->value_gens = TYPE_P_ALLOC_N(mUINT32, length);
  switch (kind) {
  case _REGION_MAP_PTR:
    result->values.ptr = TYPE_P_ALLOC_N(void *, length);
    break;
  case _REGION_MAP_I32:
    result->values.i32 = TYPE_P_ALLOC_N(mINT32, length);
    break;
  case _REGION_MAP_I64:
    result->values.i64 = TYPE_P_ALLOC_N(mINT64, length);
    break;
  default:
    FmtAssert(FALSE, ("REGION_MAP_create_kind passed bad <kind>"));
  }

  return result;
}


void REGION_MAP_grow(REGION_MAP map, REGION *region)
{
  INT32 new_length = (sizeof(map) + 1) * REGION_EXPANSION_FACTOR;

  if (REGION_map_idx(region) > new_length) {
    DevWarn("REGION_MAP_idx_max not up to date");
    new_length = REGION_map_idx(region) + 1;
  }

  /* TODO: Realloc'ing in a shared MEM_POOL is slow.  Experiment with
   * using a separate MEM_POOL for each REGION_MAP.
   */
   
  map->value_gens = TYPE_MEM_POOL_REALLOC_N(mUINT32, &MEM_phase_pool,
					    map->value_gens, map->length,
					    new_length);

  switch (map->kind) {
  case _REGION_MAP_PTR:
    map->values.ptr =
      TYPE_MEM_POOL_REALLOC_N(void *, &MEM_phase_pool, map->values.ptr,
			      map->length, new_length);
    break;
  case _REGION_MAP_I32:
    map->values.i32 =
      TYPE_MEM_POOL_REALLOC_N(INT32, &MEM_phase_pool, map->values.i32,
			      map->length, new_length);
    break;
  case _REGION_MAP_I64:
    map->values.i64 =
      TYPE_MEM_POOL_REALLOC_N(INT64, &MEM_phase_pool, map->values.i64,
			      map->length, new_length);
    break;
  default:
    FmtAssert(FALSE, ("REGION_MAP_grow passed map with bad <kind>"));
  }

  map->length = new_length;
}
