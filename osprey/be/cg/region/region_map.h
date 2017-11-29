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
 
/* ======================================================================= 
 * =======================================================================
 *
 *  Module: region_map.h
 *  $Revision: 1.1 $
 *  $Date: 2005/12/30 01:47:13 $
 *  $Author: weitang $
 *  $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/region_map.h,v $
 *
 *  Revision comments:
 *
 *  3-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  General REGION_MAP facility for dynamically attaching information to
 *  REGIONs.  Much more limited in scope than WN_MAP.  Can map to three
 *  types of objects: void *, INT32, and INT64.  REGION_MAPs live only
 *  within CG, and are allocated in the phase memory pool.  The startup
 *  function REGION_MAP_Init should be called at the start of each
 *  invocation of CG.
 * 
 *  void REGION_MAP_Init()
 *	Initialize the REGION_MAP mechanism for a given invocation of CG.
 * 
 *  REGION_MAP REGION_MAP_Create()
 *  REGION_MAP REGION_MAP32_Create()
 *  REGION_MAP REGION_MAP64_Create()
 *	Create and return a new REGION_MAP.
 *
 *  void REGION_MAP_Delete(REGION_MAP map)
 *	Free the storage for <map> so it can be reused within the same
 *	invocation of CG.
 *
 *  void REGION_MAP_Set(REGION_MAP map, REGION *region, void *value)
 *  void REGION_MAP32_Set(REGION_MAP map, REGION *region, INT32 value)
 *  void REGION_MAP64_Set(REGION_MAP map, REGION *region, INT64 value)
 *	Set the <map> value for <region> to <value>.
 *
 *  void *REGION_MAP_Get(REGION_MAP map, REGION *region)
 *  INT32 REGION_MAP32_Get(REGION_MAP map, REGION *region)
 *  INT64 REGION_MAP64_Get(REGION_MAP map, REGION *region)
 *	Lookup the <map> value for <region>.  If no value has been previously
 *	set, returns the appropriate zero value.
 *
 * =======================================================================
 * ======================================================================= */

#ifndef REGION_MAP_INCLUDED
#define REGION_MAP_INCLUDED

#include "region.h"

/* Use REGION_id as REGION_map_idx. */
#define REGION_map_idx(region) region->Id()


typedef enum {
  _REGION_MAP_PTR,
  _REGION_MAP_I32,
  _REGION_MAP_I64
} _REGION_MAP_KIND;


typedef struct region_map {
  mUINT32 length;
  mUINT32 gen;
  mUINT32 *value_gens;
  union {
    void **ptr;
    mINT32 *i32;
    mINT64 *i64;
  } values;
  _REGION_MAP_KIND kind;
  mBOOL deleted;
} *REGION_MAP;


void REGION_MAP_Init(void);

REGION_MAP REGION_MAP_create_kind(INT32 REGION_MAP_idx_max, _REGION_MAP_KIND kind);

void REGION_MAP_Delete(REGION_MAP map);

void REGION_MAP_grow(REGION_MAP map, REGION *region);


inline void REGION_MAP_Set(REGION_MAP map, REGION *region, void *value)
{
  UINT32 idx = REGION_map_idx(region);
  DevAssert(map->kind == _REGION_MAP_PTR, ("REGION_MAP has wrong kind"));
  if (value && idx >= map->length)
    REGION_MAP_grow(map, region);
  if (idx < map->length) {
    map->values.ptr[idx] = value;
    map->value_gens[idx] = map->gen;
  }
}


inline void REGION_MAP32_Set(REGION_MAP map, REGION *region, INT32 value)
{
  UINT32 idx = REGION_map_idx(region);
  DevAssert(map->kind == _REGION_MAP_I32, ("REGION_MAP has wrong kind"));
  if (value && idx >= map->length)
    REGION_MAP_grow(map, region);
  if (idx < map->length) {
    map->values.i32[idx] = value;
    map->value_gens[idx] = map->gen;
  }
}


inline void REGION_MAP64_Set(REGION_MAP map, REGION *region, INT64 value)
{
  UINT32 idx = REGION_map_idx(region);
  DevAssert(map->kind == _REGION_MAP_I64, ("REGION_MAP has wrong kind"));
  if (value && idx >= map->length)
    REGION_MAP_grow(map, region);
  if (idx < map->length) {
    map->values.i64[idx] = value;
    map->value_gens[idx] = map->gen;
  }
}


inline void *REGION_MAP_Get(REGION_MAP map, REGION *region)
{
  UINT32 idx = REGION_map_idx(region);
  DevAssert(map->kind == _REGION_MAP_PTR, ("REGION_MAP has wrong kind"));
  return idx < map->length && map->value_gens[idx] == map->gen ?
    map->values.ptr[idx] : NULL;
}


inline INT32 REGION_MAP32_Get(REGION_MAP map, REGION *region)
{
  UINT32 idx = REGION_map_idx(region);
  DevAssert(map->kind == _REGION_MAP_I32, ("REGION_MAP has wrong kind"));
  return idx < map->length && map->value_gens[idx] == map->gen ?
    map->values.i32[idx] : 0;
}


inline INT64 REGION_MAP64_Get(REGION_MAP map, REGION *region)
{
  UINT64 idx = REGION_map_idx(region);
  DevAssert(map->kind == _REGION_MAP_I64, ("REGION_MAP has wrong kind"));
  return idx < map->length && map->value_gens[idx] == map->gen ?
    map->values.i64[idx] : 0;
}


#define REGION_MAP_Create(size) REGION_MAP_create_kind(size, _REGION_MAP_PTR)

#define REGION_MAP32_Create() REGION_MAP_create_kind(_REGION_MAP_I32)

#define REGION_MAP64_Create() REGION_MAP_create_kind(_REGION_MAP_I64)

#endif /* REGION_MAP_INCLUDED */
