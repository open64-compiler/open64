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
 *  Module: op_map.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:27-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.op_map.h $
 *
 *  Revision comments:
 *
 *  5-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  General OP-map facility for dynamically attaching information to OPs.  
 *  OP-maps can be instantiated either locally (BB_OP_MAP: limited to OPs
 *  in a single BB) or globally (OP_MAP: applying to OPs in any BB). 
 *  OP-maps are much more limited in scope than WN_MAPs.  They map to
 *  three types of objects: void *, INT32, and INT64.  OP-maps live only 
 *  within CG.
 *
 *  void OP_MAP_Init()
 *	Initialize the OP_MAP mechanism for a given invocation of CG.
 *
 *  void OP_MAP_Finish()
 *	Cleanup the OP_MAP mechanism after a given invocation of CG.
 *
 *
 *  Global maps (OP_MAP) can contain OPs from any BB as well as OPs that are
 *  currently not contained in any BB:
 *
 *  OP_MAP OP_MAP_Create(void)
 *  OP_MAP OP_MAP32_Create(void)
 *  OP_MAP OP_MAP64_Create(void)
 *	Create and return a new global OP_MAP.
 *
 *  void OP_MAP_Delete(OP_MAP map)
 *	Free the storage for <map> so it can be reused within the same
 *	invocation of CG.
 *
 *  void OP_MAP_Set(OP_MAP map, OP *op, void *value)
 *  void OP_MAP32_Set(OP_MAP map, OP *op, INT32 value)
 *  void OP_MAP64_Set(OP_MAP map, OP *op, INT64 value)
 *	Set the <map> value for <op> to <value>.
 *
 *  void *OP_MAP_Get(OP_MAP map, OP *OP)
 *  INT32 OP_MAP32_Get(OP_MAP map, OP *OP)
 *  INT64 OP_MAP64_Get(OP_MAP map, OP *OP)
 *	Lookup the <map> value for <op>.  If no value has been previously
 *	set, returns the appropriate zero value.
 *
 *
 *  Local maps (BB_OP_MAP) are restricted to OPs contained within one
 *  specified BB (OPs may be added to a BB after instantiating a local
 *  op map). As a result, they are more space and time efficient than
 *  global maps:
 *
 *  BB_OP_MAP BB_OP_MAP_Create(BB *bb, MEM_POOL *pool)
 *  BB_OP_MAP BB_OP_MAP32_Create(BB *bb, MEM_POOL *pool)
 *  BB_OP_MAP BB_OP_MAP64_Create(BB *bb, MEM_POOL *pool)
 *	Create and return a new BB_OP_MAP for <bb>. <pool> specifies
 *	the MEM_POOL used for storage allocation of the BB_OP_MAP
 *	data structures. Note that there is no corresponding delete
 *	function -- deallocating the memory from <pool> is sufficient.
 *
 *  void BB_OP_MAP_Set(BB_OP_MAP map, OP *op, void *value)
 *  void BB_OP_MAP32_Set(BB_OP_MAP map, OP *op, INT32 value)
 *  void BB_OP_MAP64_Set(BB_OP_MAP map, OP *op, INT64 value)
 *	Set the <map> value for <op> to <value>.
 *
 *  void *BB_OP_MAP_Get(BB_OP_MAP map, OP *op)
 *  INT32 BB_OP_MAP32_Get(BB_OP_MAP map, OP *op)
 *  INT64 BB_OP_MAP32_Get(BB_OP_MAP map, OP *op)
 *	Lookup the <map> value for <op>.  If no value has been previously
 *	set, or <op> is not contained in the BB for which the map was
 *	created, returns the appropriate zero value.
 *
 *
 *  In addition to OP_MAPs, this module also provides a way of mapping
 *  16-bit OP_map_idx's to local OPs.  This takes advantage of the
 *  size limitations on BBs to provide a more space-efficient way of
 *  referring to local OPs.  This is currently tuned to provide
 *  constant-time access when called with the same BB more than once
 *  in a row.  If necessary, the implementation can be changed to
 *  easily deal with a working set of more than one BB.
 *
 *  OP *OP_MAP_Idx_Op(BB *bb, UINT16 idx)
 *	Return the OP in <bb> such that OP_map_idx(op) == <idx>, or NULL
 *	if there is no OP in <bb> with the given <idx>.
 *
 * =======================================================================
 * ======================================================================= */

#ifndef OP_MAP_INCLUDED
#define OP_MAP_INCLUDED

#include "op.h"
#include "bb.h"


/* Initialization and finalization:
 */
extern void OP_MAP_Init(void);
extern void OP_MAP_Finish(void);


/* Global maps:
 */
typedef struct op_map *OP_MAP;

typedef enum { OP_MAP_PTR, OP_MAP_I32, OP_MAP_I64, OP_MAP_DELETED } OP_MAP_KIND;

extern OP_MAP _OP_MAP_Create(OP_MAP_KIND kind);
#define OP_MAP_Create() _OP_MAP_Create(OP_MAP_PTR)
#define OP_MAP32_Create() _OP_MAP_Create(OP_MAP_I32)
#define OP_MAP64_Create() _OP_MAP_Create(OP_MAP_I64)

void OP_MAP_Delete(OP_MAP map);
#ifdef TARG_IA64
BOOL OP_MAP_Is_Delete(OP_MAP map);
#endif

void OP_MAP_Set(OP_MAP map, OP *op, void *value);
void OP_MAP32_Set(OP_MAP map, OP *op, INT32 value);
void OP_MAP64_Set(OP_MAP map, OP *op, INT64 value);

void *OP_MAP_Get(OP_MAP map, const OP *OP);
INT32 OP_MAP32_Get(OP_MAP map, const OP *OP);
INT64 OP_MAP64_Get(OP_MAP map, const OP *OP);

/* Local maps:
 */
typedef struct bb_op_map {
/* -------------------------------------------------- */
/* All fields are private.                            */
/* -------------------------------------------------- */
  BB		*bb;
  union {
    void	**ptr;
    INT32	*i32;
    INT64	*i64;
  } themap;
  MEM_POOL	*pool;
  UINT32	nelem;
  OP_MAP_KIND	kind;
} *BB_OP_MAP;

extern BB_OP_MAP BB_OP_MAP_Create_Kind(BB *bb, 
				       MEM_POOL *pool, 
				       OP_MAP_KIND kind);

#define BB_OP_MAP_Create(bb, pool)   BB_OP_MAP_Create_Kind((bb), (pool), OP_MAP_PTR)
#define BB_OP_MAP32_Create(bb, pool) BB_OP_MAP_Create_Kind((bb), (pool), OP_MAP_I32)
#define BB_OP_MAP64_Create(bb, pool) BB_OP_MAP_Create_Kind((bb), (pool), OP_MAP_I64)

/* declare Extend_Map here so inline functions find it with correct name */
extern void BB_OP_MAP_Extend_Map(BB_OP_MAP map, OP *op);

inline void BB_OP_MAP_Set(BB_OP_MAP map, OP *op, void *value)
{
  INT idx = OP_map_idx(op);

  Is_True(map->kind == OP_MAP_PTR, ("OP_MAP is of wrong kind"));
  if (OP_bb(op) != map->bb || idx >= map->nelem) {
    BB_OP_MAP_Extend_Map(map, op);
  }
  map->themap.ptr[idx] = value;
}

inline void BB_OP_MAP32_Set(BB_OP_MAP map, OP *op, INT32 value)
{
  INT idx = OP_map_idx(op);

  Is_True(map->kind == OP_MAP_I32, ("OP_MAP is of wrong kind"));
  if (OP_bb(op) != map->bb || idx >= map->nelem) {
    BB_OP_MAP_Extend_Map(map, op);
  }
  map->themap.i32[idx] = value;
}

inline void BB_OP_MAP64_Set(BB_OP_MAP map, OP *op, INT64 value)
{
  INT idx = OP_map_idx(op);

  Is_True(map->kind == OP_MAP_I64, ("OP_MAP is of wrong kind"));
  if (OP_bb(op) != map->bb || idx >= map->nelem) {
    BB_OP_MAP_Extend_Map(map, op);
  }
  map->themap.i64[idx] = value;
}

inline void *BB_OP_MAP_Get(BB_OP_MAP map, const OP *op)
{
  INT idx = OP_map_idx(op);
  Is_True(map->kind == OP_MAP_PTR, ("OP_MAP is of wrong kind"));
  return    OP_bb(op) == map->bb 
	 && idx < map->nelem
	 ? map->themap.ptr[idx] : (void *)NULL;
}

inline INT32 BB_OP_MAP32_Get(BB_OP_MAP map, const OP *op)
{
  INT idx = OP_map_idx(op);
  Is_True(map->kind == OP_MAP_I32, ("OP_MAP is of wrong kind"));
  return    OP_bb(op) == map->bb 
	 && idx < map->nelem
	 ? map->themap.i32[idx] : (INT32)0;
}

inline INT64 BB_OP_MAP64_Get(BB_OP_MAP map, const OP *op)
{
  INT idx = OP_map_idx(op);
  Is_True(map->kind == OP_MAP_I64, ("OP_MAP is of wrong kind"));
  return    OP_bb(op) == map->bb 
	 && idx < map->nelem
	 ? map->themap.i64[idx] : (INT64)0;
}


/* Misc:
 */
extern OP *OP_MAP_Idx_Op(BB *bb, UINT16 idx);

#endif /* OP_MAP_INCLUDED */
