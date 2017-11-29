/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: wn_map.c
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/wn_map.cxx,v $
 *
 * Revision history:
 *  09-Dec-94 - Original Version (derived from old wn_map.cxx file)
 *
 * Description: WHIRL mapping mechanism
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "wn.h"

/**                     Mapping mechanism for tree nodes
***                     --------------------------------
***
*** Description:
***
***     This module implements the mapping mechanism.  Its interface
***	is described in wn_map.h
***
*** Implementation Description
***	The interface/external behavior of the mapping routines is
***	described in wn_map.h.  Here we describe the implementation.
***
***	The set of possible opcodes is partitioned into categories.
***	We provide a separate mapping mechanism for each category,
***     although this is transparent to the user.  When the user creates
***	a mapping, no memory is allocated.  We instead use a lazy model that
***	allocates space when the user first sets the value for a node.
***     Thus, while the same mapping can be used by opcodes in different 
***	categories, space is only reserved for those categores that have 
***	opcodes which are set.  As an example, the user may create a
***	data dependence mapping.  If he only sets the mappings for loads
***	and stores, memory will only be allocated for all the load/store
***	nodes.
***
***	Each wn_map has a WN_MAP_KIND flag to indicate what kind of values
***	it holds.  They may be void*, INT32, or INT64 values.
***
***	Every WN contains a map_id.  This map_id is unique among all nodes
***	of the same category in one PU.  Two nodes of different categories
***     or in different PUs may share the same map_id.  Every node has its
***	map_id initialized to -1.  A node only gets a valid map_id when it
***	is assigned a value for one of the maps.  This allows us to avoid
***	wasting map_id space on nodes that are never mapped.
***
***	To save space, we retain a free list of map_ids.  When a WN is
***	deleted, the WN_MAP_Add_Free_List method is called automatically
***	to add the map_id to the free list.  If we later need a new map_id,
***     we can just grab a value off the free list.
***
*/

/* global variable to keep track of the current map table */
WN_MAP_TAB *Current_Map_Tab = NULL;

#define INIT_MAP_SIZE 20

static WN_MAP_ID WN_MAP_get_map_id(
    WN_MAP_TAB *maptab,
    OPERATOR_MAPCAT category,
    WN *wn);

static void WN_MAP_realloc_array(
    WN_MAP_TAB *maptab,
    OPERATOR_MAPCAT category,
    WN_MAP wn_map,
    WN_MAP_ID id,
    INT32 elemsz);


#define WN_MAP_check_kind(maptab, wn_map, kind) \
    ((maptab)->_kind[(wn_map)] == (kind))


/*
 *  Create a new map table and initialize it.  The slots for the
 *  predefined maps must be marked as "in use" and all the other
 *  slots must be cleared out.
 */

WN_MAP_TAB *
WN_MAP_TAB_Create(MEM_POOL *pool)
{
  INT32 i, category;
  WN_MAP_TAB *maptab = TYPE_MEM_POOL_ALLOC(WN_MAP_TAB, pool);

  maptab->_free_list_pool = pool;

  /*  initialize the predefined mappings */
  for (i = 0; i < WN_MAP_RESERVED; i++) {
    maptab->_is_used[i] = TRUE;
    maptab->_dont_copy[i] = FALSE;
    /* zero the _map_size counter */
    for (category = 0; category < WN_MAP_CATEGORIES; category++) { 
      maptab->_map_size[category][i] = 0;
      maptab->_mapping[category][i] = NULL;
    }
    maptab->_pool[i] = pool;
  }

  /*  set the kinds for the predefined mappings */
  maptab->_kind[WN_MAP_DEPGRAPH] = WN_MAP_KIND_VOIDP;
  maptab->_kind[WN_MAP_PREFETCH] = WN_MAP_KIND_VOIDP;
  maptab->_kind[WN_MAP_FEEDBACK] = WN_MAP_KIND_INT32;
  maptab->_kind[WN_MAP_AC_INTERNAL] = WN_MAP_KIND_VOIDP;
  maptab->_kind[WN_MAP_ALIAS_CLASS] = WN_MAP_KIND_INT32;
  maptab->_kind[WN_MAP_ALIAS_CGNODE] = WN_MAP_KIND_INT32;

  /*  clear the slots that are not reserved */
  for (i = WN_MAP_RESERVED; i < WN_MAP_MAX; i++) { 
    maptab->_is_used[i] = FALSE;
    maptab->_dont_copy[i] = FALSE;
  }
  /* clear all category info, even for reserved ones */
  /* Because we don't init mem to 0, have to set values here. */
  for (i = 0; i < WN_MAP_CATEGORIES; i++) { 
    maptab->_last_map_id[i] = -1;
    maptab->_free_list_count[i] = 0;
    maptab->_free_list_size[i] = 0;
  }

  /*  record the current map table */
  Current_Map_Tab = maptab;
  return maptab;
}


/*
 *  Deallocate a map table.
 */

void
WN_MAP_TAB_Delete(WN_MAP_TAB *maptab)
{
  INT32 i, category;

  /* delete the maps that are in use */
  for (i = 0; i < WN_MAP_MAX; i++) { 
    if (maptab->_is_used[i]) {
      IPA_WN_MAP_Delete(maptab, i);
    }
  }

  /* deallocate the free list storage */
  for (category = 0; category < WN_MAP_CATEGORIES; category++) { 
    if (maptab->_free_list_size[category] > 0) {
      MEM_POOL_FREE(maptab->_free_list_pool, maptab->_free_list[category]);
    }
  }

  /* finally deallocate the table itself */
  MEM_POOL_FREE(maptab->_free_list_pool, maptab);
}


/*
 *  Search a map table for an open slot and initialize that slots to
 *  contain a new mapping.  If no slots are available, return -1.
 */

WN_MAP
WN_MAP_Do_Create(WN_MAP_TAB *maptab, MEM_POOL *pool, WN_MAP_KIND kind)
{
  WN_MAP_ID wn_map;
  INT32 category;

  /* find an unused map, return -1 on error */
  for (wn_map = WN_MAP_RESERVED; wn_map < WN_MAP_MAX; wn_map++) {
    if (!maptab->_is_used[wn_map]) break;
  }
  FmtAssert(wn_map != WN_MAP_MAX,("WN_MAP_Do_Create, ran out of maps"));

  maptab->_is_used[wn_map] = TRUE;
  maptab->_dont_copy[wn_map] = FALSE;

  /* zero the _map_size counter */
  for (category = 0; category < WN_MAP_CATEGORIES; category++) { 
    maptab->_map_size[category][wn_map] = 0;
      maptab->_mapping[category][wn_map] = NULL;
  }

  maptab->_pool[wn_map] = pool;
  maptab->_kind[wn_map] = kind;
  return wn_map;
}


/*
 *  Delete a map from a map table so that its slot can be reused.  The
 *  space used by the map value array is only deallocated if the MEM_POOL
 *  is the Malloc_Mem_Pool.
 */

void
IPA_WN_MAP_Delete(WN_MAP_TAB *maptab, WN_MAP wn_map)
{
  INT32 category;

  if (maptab == 0)
      return;
  
  Is_True(0 <= wn_map && wn_map < WN_MAP_MAX,
          ("IPA_WN_MAP_Delete: invalid map index %d", wn_map));

  for (category = 0; category < WN_MAP_CATEGORIES; category++) {
    if (maptab->_map_size[category][wn_map] != 0) {
      if (maptab->_pool[wn_map] == Malloc_Mem_Pool) {
	MEM_POOL_FREE(Malloc_Mem_Pool, maptab->_mapping[category][wn_map]);
      }
      maptab->_map_size[category][wn_map] = 0;
      maptab->_mapping[category][wn_map] = NULL;
    }
  }
  maptab->_is_used[wn_map] = FALSE;
}

WN_MAP_ID
IPA_WN_MAP_Status(WN_MAP_TAB *maptab)
{
  WN_MAP_ID wn_map;

  /* find an first unused map, return -1 on error */
  for (wn_map = WN_MAP_RESERVED; wn_map < WN_MAP_MAX; wn_map++)
    if (!maptab->_is_used[wn_map])
      return wn_map;
  Is_True(0,("WN_MAP_Status, all maps used"));
  return (WN_MAP_ID)(-1);
}

/*
 *  The following functions are used to set the value for a WN in a
 *  particular mapping.  The WN_MAP_get_map_id function checks if
 *  the WN has been assigned a map_id.  If not it gets a new map_id,
 *  checking the free list to see if it can reuse an old map_id value.
 *  WN_MAP_realloc_array reallocates the map value array if it is not
 *  big enough.
 */

WN_MAP_ID
WN_MAP_get_map_id (WN_MAP_TAB *maptab, OPERATOR_MAPCAT category, WN *wn)
{
  /* Check if this node has been given an id */
  if (WN_map_id(wn) != -1) {
    return WN_map_id(wn);
  }

  /* is there anything on the free list */
  if (maptab->_free_list_count[category] > 0) {
    INT32 i;
    WN_MAP_ID id =
      maptab->_free_list[category][--maptab->_free_list_count[category]];
    WN_set_map_id(wn, id);
    /* grabbed it from the free list, now must zero old mappings */
    for (i = 0; i < WN_MAP_MAX; i++) {
      if (maptab->_is_used[i] && (id < maptab->_map_size[category][i])) {
	switch (maptab->_kind[i]) {
	case WN_MAP_KIND_VOIDP:
	  maptab->_mapping[category][i][id] = NULL;
	  break;
	case WN_MAP_KIND_INT32:
	  ((INT32*)maptab->_mapping[category][i])[id] = 0;
	  break;
	case WN_MAP_KIND_INT64:
	  ((INT64*)maptab->_mapping[category][i])[id] = 0;
	  break;
	default:
	  Is_True(FALSE, ("WN_MAP_do_set: unknown map kind"));
	}
      }
    }
    return id;
  }

  /* nothing on the free list, give it a new id */
  WN_set_map_id(wn, ++maptab->_last_map_id[category]);
  return WN_map_id(wn);
}


void
WN_MAP_realloc_array (WN_MAP_TAB *maptab, OPERATOR_MAPCAT category,
		      WN_MAP wn_map, WN_MAP_ID id, INT32 elemsz)
{
  INT32 old_size = maptab->_map_size[category][wn_map];
  INT32 new_size;
  if (old_size == 0) {
    new_size = INIT_MAP_SIZE;
  } else {
    new_size = old_size * 2;
  }
  while (id >= new_size) {
    new_size *= 2;
  }

  maptab->_map_size[category][wn_map] = new_size;
  maptab->_mapping[category][wn_map] = (void **)
      MEM_POOL_Realloc(maptab->_pool[wn_map],
		     maptab->_mapping[category][wn_map],
		     old_size * elemsz,
		     new_size * elemsz);

  /* make sure the new storage is zeroed */
  if (!(maptab->_pool[wn_map]->bz)) {
      INTPS address = ((INTPS) maptab->_mapping[category][wn_map]) + (old_size * elemsz);
      BZERO((void *) address, (new_size - old_size) * elemsz);
  }
}


void
IPA_WN_MAP_Set(WN_MAP_TAB *maptab, WN_MAP wn_map, WN *wn, void *thing)
{
  OPERATOR_MAPCAT category;
  WN_MAP_ID id;

  Is_True(wn != NULL,("WN_MAP_Set: wn is NULL"));
  category = OPCODE_mapcat(WN_opcode(wn));
  Is_True(maptab->_is_used[wn_map], ("WN_MAP_Set: wn_map is not valid"));
  Is_True(WN_MAP_check_kind(maptab, wn_map, WN_MAP_KIND_VOIDP),
	  ("WN_MAP_Set: not a VOID* map"));

  id = WN_MAP_get_map_id(maptab, category, wn);
  if (id >= maptab->_map_size[category][wn_map]) {
    WN_MAP_realloc_array(maptab, category, wn_map, id, sizeof(void*));
  }
  maptab->_mapping[category][wn_map][id] = thing;
}


void
IPA_WN_MAP32_Set(WN_MAP_TAB *maptab, WN_MAP wn_map, WN *wn, INT32 thing)
{
  OPERATOR_MAPCAT category = OPCODE_mapcat(WN_opcode(wn));
  WN_MAP_ID id;

  Is_True(maptab->_is_used[wn_map], ("WN_MAP32_Set: wn_map is not valid"));
  Is_True(WN_MAP_check_kind(maptab, wn_map, WN_MAP_KIND_INT32),
	  ("WN_MAP32_Set: not an INT32 map"));

  id = WN_MAP_get_map_id(maptab, category, wn);
  if (id >= maptab->_map_size[category][wn_map]) {
    WN_MAP_realloc_array(maptab, category, wn_map, id, sizeof(INT32));
  }
  ((INT32*)maptab->_mapping[category][wn_map])[id] = thing;
}


void
IPA_WN_MAP64_Set(WN_MAP_TAB *maptab, WN_MAP wn_map, WN *wn, INT64 thing)
{
  OPERATOR_MAPCAT category = OPCODE_mapcat(WN_opcode(wn));
  WN_MAP_ID id;

  Is_True(maptab->_is_used[wn_map], ("WN_MAP64_Set: wn_map is not valid"));
  Is_True(WN_MAP_check_kind(maptab, wn_map, WN_MAP_KIND_INT64),
	  ("WN_MAP64_Set: not an INT64 map"));

  id = WN_MAP_get_map_id(maptab, category, wn);
  if (id >= maptab->_map_size[category][wn_map]) {
    WN_MAP_realloc_array(maptab, category, wn_map, id, sizeof(INT64));
  }
  ((INT64*)maptab->_mapping[category][wn_map])[id] = thing;
}


/*
 *  The following functions retrieve the value for a WN in a particular
 *  map.  If no value has been assigned for that WN, they return 0.
 */

void *
IPA_WN_MAP_Get(WN_MAP_TAB *maptab, WN_MAP wn_map, const WN *wn)
{
  WN_MAP_ID id = WN_map_id(wn);
  OPERATOR_MAPCAT category;

  if (id == -1) return NULL;
  Is_True(maptab->_is_used[wn_map], ("WN_MAP_Get: wn_map is not valid"));
  Is_True(WN_MAP_check_kind(maptab, wn_map, WN_MAP_KIND_VOIDP),
	  ("WN_MAP_Get: not a VOID* map"));

  category = OPCODE_mapcat(WN_opcode(wn));
  if (id >= maptab->_map_size[category][wn_map]) return NULL;
  return maptab->_mapping[category][wn_map][id];
}


INT32
IPA_WN_MAP32_Get(WN_MAP_TAB *maptab, WN_MAP wn_map, const WN *wn)
{
  WN_MAP_ID id = WN_map_id(wn);
  OPERATOR_MAPCAT category;

  Is_True(maptab->_is_used[wn_map], ("WN_MAP32_Get: wn_map is not valid"));
  Is_True(WN_MAP_check_kind(maptab, wn_map, WN_MAP_KIND_INT32),
	  ("WN_MAP32_Get: not an INT32 map"));

  if (id == -1) return 0;
  category = OPCODE_mapcat(WN_opcode(wn));
  if (id >= maptab->_map_size[category][wn_map]) return 0;
  return ((INT32*)maptab->_mapping[category][wn_map])[id];
}


INT64
IPA_WN_MAP64_Get(WN_MAP_TAB *maptab, WN_MAP wn_map, const WN *wn)
{
  WN_MAP_ID id = WN_map_id(wn);
  OPERATOR_MAPCAT category;

  Is_True(maptab->_is_used[wn_map], ("WN_MAP64_Get: wn_map is not valid"));
  Is_True(WN_MAP_check_kind(maptab, wn_map, WN_MAP_KIND_INT64),
	  ("WN_MAP64_Get: not an INT64 map"));

  if (id == -1) return 0;
  category = OPCODE_mapcat(WN_opcode(wn));
  if (id >= maptab->_map_size[category][wn_map]) return 0;
  return ((INT64*)maptab->_mapping[category][wn_map])[id];
}


/*
 *  Add the map_id for a WN to the free list in a map table.  This is
 *  called automatically by WN_Delete.  The free list is just an array;
 *  if it runs out of space, we just realloc it.
 */

void
WN_MAP_Add_Free_List(WN_MAP_TAB *maptab, WN *wn)
{
  OPERATOR_MAPCAT category = OPCODE_mapcat(WN_opcode(wn));
  INT32 count, size;
  
  if (WN_map_id(wn) == -1) return; /* no map_id to free */
  
  count = maptab->_free_list_count[category];
  size = maptab->_free_list_size[category];

  /* is there space, if not, make some */
  if (count >= size) {
    if (size == 0) {
      INT32 elements = 50;
      maptab->_free_list[category] =
        TYPE_MEM_POOL_ALLOC_N(WN_MAP_ID, maptab->_free_list_pool, elements);
      maptab->_free_list_size[category] = elements;
    } else {
      INT32 elements = MAX(2 * size, size + 50);
      maptab->_free_list[category] =
        TYPE_MEM_POOL_REALLOC_N(WN_MAP_ID, maptab->_free_list_pool,
			      maptab->_free_list[category], size, elements);
      maptab->_free_list_size[category] = elements;
    }
  }
  
  maptab->_free_list[category][count] = WN_map_id(wn);
  maptab->_free_list_count[category] = count + 1;
}


/*
 *  Set the WN map_id field if it doesn't already have an ID.
 */

void
WN_MAP_Set_ID (WN_MAP_TAB *maptab, WN *wn)
{
  OPERATOR_MAPCAT category = OPCODE_mapcat(WN_opcode(wn));
  (void)WN_MAP_get_map_id (maptab, category, wn);
}


