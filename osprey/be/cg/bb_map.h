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
 *  Module: bb_map.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:20-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.bb_map.h $
 *
 *  Revision comments:
 *
 *  3-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  General BB_MAP facility for dynamically attaching information to
 *  BBs.  Much more limited in scope than WN_MAP.  Can map to three
 *  types of objects: void *, INT32, and INT64.  BB_MAPs live only
 *  within CG, and are allocated in the phase memory pool.  The startup
 *  function BB_MAP_Init should be called at the start of each
 *  invocation of CG.
 * 
 *  void BB_MAP_Init()
 *	Initialize the BB_MAP mechanism for a given invocation of CG.
 * 
 *  BB_MAP BB_MAP_Create()
 *  BB_MAP BB_MAP32_Create()
 *  BB_MAP BB_MAP64_Create()
 *	Create and return a new BB_MAP.
 *
 *  void BB_MAP_Delete(BB_MAP map)
 *	Free the storage for <map> so it can be reused within the same
 *	invocation of CG.
 *
 *  void BB_MAP_Set(BB_MAP map, BB *bb, void *value)
 *  void BB_MAP32_Set(BB_MAP map, BB *bb, INT32 value)
 *  void BB_MAP64_Set(BB_MAP map, BB *bb, INT64 value)
 *	Set the <map> value for <bb> to <value>.
 *
 *  void *BB_MAP_Get(BB_MAP map, BB *BB)
 *  INT32 BB_MAP32_Get(BB_MAP map, BB *BB)
 *  INT64 BB_MAP64_Get(BB_MAP map, BB *BB)
 *	Lookup the <map> value for <bb>.  If no value has been previously
 *	set, returns the appropriate zero value.
 *
 * =======================================================================
 * ======================================================================= */

#ifndef BB_MAP_INCLUDED
#define BB_MAP_INCLUDED

#include "bb.h"


/* Use BB_id as BB_map_idx. */
#define BB_MAP_idx_max PU_BB_Count
#define BB_map_idx(bb) BB_id(bb)


typedef enum {
  _BB_MAP_PTR,
  _BB_MAP_I32,
  _BB_MAP_I64
} _BB_MAP_KIND;


typedef struct bb_map {
  mUINT32 length;
  mUINT32 gen;
  mUINT32 *value_gens;
  union {
    void **ptr;
    mINT32 *i32;
    mINT64 *i64;
  } values;
  _BB_MAP_KIND kind;
  mBOOL deleted;
} *BB_MAP;


void BB_MAP_Init(void);

BB_MAP BB_MAP_create_kind(_BB_MAP_KIND kind);

void BB_MAP_Delete(BB_MAP map);

void BB_MAP_grow(BB_MAP map, BB *bb);


inline void BB_MAP_Set(BB_MAP map, BB *bb, void *value)
{
  UINT32 idx = BB_map_idx(bb);
  DevAssert(map->kind == _BB_MAP_PTR, ("BB_MAP has wrong kind"));
  if (value && idx >= map->length)
    BB_MAP_grow(map, bb);
  if (idx < map->length) {
    map->values.ptr[idx] = value;
    map->value_gens[idx] = map->gen;
  }
}


inline void BB_MAP32_Set(BB_MAP map, BB *bb, INT32 value)
{
  UINT32 idx = BB_map_idx(bb);
  DevAssert(map->kind == _BB_MAP_I32, ("BB_MAP has wrong kind"));
  if (value && idx >= map->length)
    BB_MAP_grow(map, bb);
  if (idx < map->length) {
    map->values.i32[idx] = value;
    map->value_gens[idx] = map->gen;
  }
}


inline void BB_MAP64_Set(BB_MAP map, BB *bb, INT64 value)
{
  UINT32 idx = BB_map_idx(bb);
  DevAssert(map->kind == _BB_MAP_I64, ("BB_MAP has wrong kind"));
  if (value && idx >= map->length)
    BB_MAP_grow(map, bb);
  if (idx < map->length) {
    map->values.i64[idx] = value;
    map->value_gens[idx] = map->gen;
  }
}


inline void *BB_MAP_Get(BB_MAP map, BB *bb)
{
  UINT32 idx = BB_map_idx(bb);
  DevAssert(map->kind == _BB_MAP_PTR, ("BB_MAP has wrong kind"));
  return idx < map->length && map->value_gens[idx] == map->gen ?
    map->values.ptr[idx] : NULL;
}


inline INT32 BB_MAP32_Get(BB_MAP map, BB *bb)
{
  UINT32 idx = BB_map_idx(bb);
  DevAssert(map->kind == _BB_MAP_I32, ("BB_MAP has wrong kind"));
  return idx < map->length && map->value_gens[idx] == map->gen ?
    map->values.i32[idx] : 0;
}


inline INT64 BB_MAP64_Get(BB_MAP map, BB *bb)
{
  UINT64 idx = BB_map_idx(bb);
  DevAssert(map->kind == _BB_MAP_I64, ("BB_MAP has wrong kind"));
  return idx < map->length && map->value_gens[idx] == map->gen ?
    map->values.i64[idx] : 0;
}


#define BB_MAP_Create() BB_MAP_create_kind(_BB_MAP_PTR)

#define BB_MAP32_Create() BB_MAP_create_kind(_BB_MAP_I32)

#define BB_MAP64_Create() BB_MAP_create_kind(_BB_MAP_I64)

#endif /* BB_MAP_INCLUDED */
