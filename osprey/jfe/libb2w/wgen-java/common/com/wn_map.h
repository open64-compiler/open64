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


#ifndef wn_map_INCLUDED
#define wn_map_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

#ifdef _KEEP_RCS_ID
static char *wn_map_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */


/**                     Mapping mechanism for tree nodes
***                     --------------------------------
***
***	Used to maintain mappings from WNs to arbitrary data.  There
***	are three different kinds of map values: void*, INT32, and INT64.
***	A map can be created to handle one of these three kinds of values.
***     The map interface functions for different kinds of values are
***	distinguished by their prefixes.  Functions for INT32 and INT64
***	value maps begin with WN_MAP32 and WN_MAP64, respectively.
***	Functions for void* value maps and functions that apply to all
***	three kinds begin with the WN_MAP prefix.
***
***
*** Reserved prefixes:
***
***	WN_MAP		for tree node mappings
***	WN_MAP_TAB	for mapping tables
***
***
*** Exported types:
***
*** WN_MAP
***
***	WN_MAP values are just indices into the slots in a mapping table.
***	They identify particular mappings.  Some mappings are permanently
***	associated with the intermediate representation of a PU.  These
***	predefined maps may be stored in the output files and read in again
***	along with the WNs.  Each predefined map has a specific wn_map slot
***	reserved for it in all the map tables.  The WN_MAP values for these
***	maps are defined here.
***
***	Predefined Maps:
***	WN_MAP_DEPGRAPH
***	WN_MAP_PREFETCH
***
***
*** WN_MAP_TAB
***
***	The mappings are recorded in mapping tables using the WN_MAP_TAB
***	data structure.  Each PU has its own mapping table.  For programs
***	that operate on one PU at a time, the details of this should
***	all be handled automatically.  The mapping table for the current
***	PU is recorded in a global variable, so it need not be specified
***	every time a map is accessed.  IPA and any other programs that
***	operate on multiple PUs simultaneously must keep track of the
***	map table corresponding to each PU and must specify the table
***	when accessing a map.  Thus, there are separate versions of many
***	map interface functions (beginning with the prefix IPA) that take
***	the map table as an additional argument.
***
***
***
*** Exported functions:
***
*** WN_MAP_TAB *WN_MAP_TAB_Create(
***	MEM_POOL    *pool)
***
***	Create a new map table.  You must specify the MEM_POOL that
***	will be used to hold the predefined maps and the free list of
***	map IDs.  The new map table is recorded in a global variable
***	so that programs that operate on one PU at a time do not need
***	to keep track of the current map table.  This function is
***	called automatically whenever a new PU is read in (e.g. by
***	Read_Local_Info).
***
***
*** void WN_MAP_TAB_Delete(
***	WN_MAP_TAB  *maptab)
***
***	Deallocate a map table.  This is called automatically by
***	Free_Local_Info.
***
***
*** WN_MAP_ID WN_MAP_TAB_Last_ID(
***	WN_MAP_TAB  *maptab
***	OPCODE_MAPCAT category)
***
***	Return the largest map ID used in a particular category.
***
***
*** WN_MAP WN_MAP_Create(
***	MEM_POOL    *pool)
*** WN_MAP WN_MAP32_Create(
***	MEM_POOL    *pool)
*** WN_MAP WN_MAP64_Create(
***	MEM_POOL    *pool)
***
*** WN_MAP IPA_WN_MAP_Create(
***	WN_MAP_TAB  *maptab,
***	MEM_POOL    *pool)
*** WN_MAP IPA_WN_MAP32_Create(
***	WN_MAP_TAB  *maptab,
***	MEM_POOL    *pool)
*** WN_MAP IPA_WN_MAP64_Create(
***	WN_MAP_TAB  *maptab,
***	MEM_POOL    *pool)
***
***	Create and return a new WN_MAP.  Create does not allocate any
***	space; when values are assigned to the map, we allocate space
***	as needed from the given MEM_POOL.  These functions return -1
***	on error (no space left in mapping table).  
***
***
*** void WN_MAP_Delete(
***	WN_MAP      wn_map)
*** void IPA_WN_MAP_Delete(
***	WN_MAP_TAB  *maptab,
***	WN_MAP      wn_map)
***
***	Delete the given map so that its slot in the map table can be
***	reused.  If the MEM_POOL for the map is the Malloc_Mem_Pool,
***	then the map space is also deallocated.  In no case, however,
***	are the user objects (things pointed to by the map) deleted.
***	This function MUST be called before popping the MEM_POOL for
***	the map.
***
***
*** void WN_MAP_Set(
***	WN_MAP      wn_map,
***	WN          *wn,
***	void        *thing)
*** void WN_MAP32_Set(
***	WN_MAP      wn_map,
***	WN          *wn,
***	INT32       thing)
*** void WN_MAP64_Set(
***	WN_MAP      wn_map,
***	WN          *wn,
***	INT64       thing)
***
*** void IPA_WN_MAP_Set(
***	WN_MAP_TAB  *maptab,
***	WN_MAP      wn_map,
***	WN          *wn,
***	void        *thing)
*** void IPA_WN_MAP32_Set(
***	WN_MAP_TAB  *maptab,
***	WN_MAP      wn_map,
***	WN          *wn,
***	INT32       thing)
*** void IPA_WN_MAP64_Set(
***	WN_MAP_TAB  *maptab,
***	WN_MAP      wn_map,
***	WN          *wn,
***	INT64       thing)
***
***	Set the map value for 'wn' to be 'thing'.  You must use a
***	function corresponding to the kind of values in the specified
***	map (e.g. WN_MAP_Set for void* values and WN_MAP64_Set for INT64s).
***
***
*** void *WN_MAP_Get(
***	WN_MAP      wn_map,
***	WN          *wn)
*** INT32 WN_MAP32_Get(
***	WN_MAP      wn_map,
***	WN          *wn)
*** INT64 WN_MAP64_Get(
***	WN_MAP      wn_map,
***	WN          *wn)
***
*** void *IPA_WN_MAP_Get(
***	WN_MAP_TAB  *maptab,
***	WN_MAP      wn_map,
***	WN          *wn)
*** INT32 IPA_WN_MAP32_Get(
***	WN_MAP_TAB  *maptab,
***	WN_MAP      wn_map,
***	WN          *wn)
*** INT64 IPA_WN_MAP64_Get(
***	WN_MAP_TAB  *maptab,
***	WN_MAP      wn_map,
***	WN          *wn)
***
***	Get the map value for 'wn'.  These return 0 for WNs for which
***	the mapping has not yet been set.  You must use the function
***	corresponding to the kind of values in the map (e.g. WN_MAP_Get
***	for void* values and WN_MAP64_Get for INT64s).
***
***
*** Related functions (declared in wn.h):
***
*** void IPA_WN_Move_Maps(
***	WN_MAP_TAB	*maptab,
***	WN		*dst,
***	WN		*src)
*** void WN_Move_Maps(
***	WN		*dst,
***	WN		*src)
***
***	Move the information stored in the map table for one WN to
***	another WN.
***
*** void IPA_WN_Move_Maps_PU(
***	WN_MAP_TAB	*src,
***	WN_MAP_TAB	*dst,
***	WN		*wn)
***
***     Move wn's mapping information from the source to the destination
***     table.  This is used when a WN is moved from one PU to another
***
***
*** The following functions should not be needed by most users.  They
*** may be called automatically by other functions.
***
***
*** INT32 WN_MAP_Size(
***	WN_MAP      wn_map,
***	OPCODE_MAPCAT category)
*** INT32 IPA_WN_MAP_Size(
***	WN_MAP_TAB  *maptab,
***	WN_MAP      wn_map,
***	OPCODE_MAPCAT category)
***
***	Return the number of elements of a mapping in a particular
***	category.
***
***
*** void WN_MAP_Add_Free_List(
***	WN_MAP_TAB  *maptab,
***	WN          *wn)
***
***	When a WN is deleted, it's WN_MAP_ID should be added to the free
***	list in the mapping table so that it can be reused.  This function
***	should be called automatically by the WN_Delete function(s).
***
***
*** void WN_MAP_Set_ID(
***	WN_MAP_TAB *maptab,
***	WN *wn)
***
***	Set the map ID of a WN without assigning anything to a map
***	(if it's not already set).
**/

struct mem_pool;


typedef INT32 		WN_MAP;

/* predefined (reserved) mappings */
#define WN_MAP_UNDEFINED (-1)
#define WN_MAP_DEPGRAPH 0
#define WN_MAP_PREFETCH 1
#define WN_MAP_FEEDBACK 2
#define WN_MAP_AC_INTERNAL 3
#define WN_MAP_ALIAS_CLASS 4

/* number of reserved mappings and total number */
#define WN_MAP_RESERVED 5
#define WN_MAP_MAX 28

typedef enum {
  WN_MAP_KIND_VOIDP = 1,
  WN_MAP_KIND_INT32 = 2,
  WN_MAP_KIND_INT64 = 3
} WN_MAP_KIND;

typedef struct wn_map_tab {
  struct mem_pool *_free_list_pool;
  struct mem_pool *_pool[WN_MAP_MAX];
  WN_MAP_KIND _kind[WN_MAP_MAX];
  void **_mapping[WN_MAP_CATEGORIES][WN_MAP_MAX];
  BOOL _is_used[WN_MAP_MAX];  /* is this WN_MAP being used */
  INT32 _last_map_id[WN_MAP_CATEGORIES]; /* last map_id for each category */
  INT32 _map_size[WN_MAP_CATEGORIES][WN_MAP_MAX]; /* size of each mapping */

  /* free list of map_ids */
  WN_MAP_ID *_free_list[WN_MAP_CATEGORIES];
  INT32 _free_list_count[WN_MAP_CATEGORIES];
  INT32 _free_list_size[WN_MAP_CATEGORIES];
} WN_MAP_TAB; 

/* map table for the current PU (set automatically) */
extern WN_MAP_TAB *Current_Map_Tab;


extern WN_MAP_TAB *WN_MAP_TAB_Create(
    struct mem_pool *pool
);

extern void WN_MAP_TAB_Delete(
    WN_MAP_TAB	*maptab
);

#define WN_MAP_TAB_Last_ID(maptab,category) \
  (maptab)->_last_map_id[(category)]


extern WN_MAP WN_MAP_Do_Create(
    WN_MAP_TAB  *maptab,
    struct mem_pool *pool,
    WN_MAP_KIND kind
);

#define WN_MAP_Create(pool) \
  WN_MAP_Do_Create(Current_Map_Tab, (pool), WN_MAP_KIND_VOIDP)
#define WN_MAP32_Create(pool) \
  WN_MAP_Do_Create(Current_Map_Tab, (pool), WN_MAP_KIND_INT32)
#define WN_MAP64_Create(pool) \
  WN_MAP_Do_Create(Current_Map_Tab, (pool), WN_MAP_KIND_INT64)

#define IPA_WN_MAP_Create(maptab, pool) \
  WN_MAP_Do_Create((maptab), (pool), WN_MAP_KIND_VOIDP)
#define IPA_WN_MAP32_Create(maptab, pool) \
  WN_MAP_Do_Create((maptab), (pool), WN_MAP_KIND_INT32)
#define IPA_WN_MAP64_Create(maptab, pool) \
  WN_MAP_Do_Create((maptab), (pool), WN_MAP_KIND_INT64)

extern WN_MAP_ID IPA_WN_MAP_Status(
    WN_MAP_TAB *maptab
);

#define WN_MAP_Status() \
  IPA_WN_MAP_Status(Current_Map_Tab)

extern void IPA_WN_MAP_Delete(
    WN_MAP_TAB  *maptab,
    WN_MAP      wn_map
);

#define WN_MAP_Delete(wn_map) \
  IPA_WN_MAP_Delete(Current_Map_Tab, (wn_map))


extern void IPA_WN_MAP_Set(
    WN_MAP_TAB  *maptab,
    WN_MAP      wn_map,
    WN          *wn,
    void        *thing
);
extern void IPA_WN_MAP32_Set(
    WN_MAP_TAB  *maptab,
    WN_MAP      wn_map,
    WN          *wn,
    INT32       thing
);
extern void IPA_WN_MAP64_Set(
    WN_MAP_TAB  *maptab,
    WN_MAP      wn_map,
    WN          *wn,
    INT64       thing
);

#define WN_MAP_Set(wn_map,wn,thing) \
  IPA_WN_MAP_Set(Current_Map_Tab, (wn_map), (wn), (thing))
#define WN_MAP32_Set(wn_map,wn,thing) \
  IPA_WN_MAP32_Set(Current_Map_Tab, (wn_map), (wn), (thing))
#define WN_MAP64_Set(wn_map,wn,thing) \
  IPA_WN_MAP64_Set(Current_Map_Tab, (wn_map), (wn), (thing))


extern void *IPA_WN_MAP_Get(
    WN_MAP_TAB  *maptab,
    WN_MAP      wn_map,
    const WN    *wn
);
extern INT32 IPA_WN_MAP32_Get(
    WN_MAP_TAB  *maptab,
    WN_MAP      wn_map,
    const WN    *wn
);
extern INT64 IPA_WN_MAP64_Get(
    WN_MAP_TAB  *maptab,
    WN_MAP      wn_map,
    const WN    *wn
);

#define WN_MAP_Get(wn_map,wn) \
  IPA_WN_MAP_Get(Current_Map_Tab, (wn_map), (wn))
#define WN_MAP32_Get(wn_map,wn) \
  IPA_WN_MAP32_Get(Current_Map_Tab, (wn_map), (wn))
#define WN_MAP64_Get(wn_map,wn) \
  IPA_WN_MAP64_Get(Current_Map_Tab, (wn_map), (wn))


#define WN_MAP_Size(wn_map,category) \
  IPA_WN_MAP_Size(Current_Map_Tab, (wn_map), (category))
#define IPA_WN_MAP_Size(maptab,wn_map,category) \
  (maptab)->_map_size[(category)][(wn_map)]


extern void WN_MAP_Add_Free_List(
    WN_MAP_TAB  *maptab,
    WN          *wn
);

extern void WN_MAP_Set_ID(
    WN_MAP_TAB	*maptab,
    WN		*wn
);

#define WN_MAP_Get_Kind(wn_map) (Current_Map_Tab->_kind[(wn_map)])

#ifdef __cplusplus
}
#endif
#endif /* wn_map_INCLUDED */
