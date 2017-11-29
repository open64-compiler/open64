/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


/*-*-c++-*-*/
/* ====================================================================
 * Module: opt_region_util.c
 * $Revision: 1.6 $
 * $Date: 05/12/05 08:59:31-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.region_util.cxx $
 *
 * Revision history:
 *  31-MAY-95 wdl - Original Version
 *
 * Description:
 * 	Utilities for creating, deleting, modifying REGIONs
 * ====================================================================*/

#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "opcode.h"
#include "wn_util.h"		// wn accessors
#include "wn_map.h"
#include "region_util.h"
#include "region_main.h"
#include "opt_alias_interface.h"
#include "optimizer.h"		// PREOPT_IPA0_PHASE
#include "ir_reader.h"
#include "wn_pragmas.h"		// WN_PRAGMA_OPTIONS
#include "cxx_memory.h"		// CXX_DELETE_ARRAY
#include "region_whirl_templates.h"	// REGION_search_block

/* this next header should be after the external declarations in the others */
#include "pragma_weak.h"	/* weak pragmas for Valid_alias...	*/

#if defined(TARG_SL)
#include "fb_whirl.h"
#endif

#if defined(__linux__) || defined(BUILD_OS_DARWIN)
extern void (*Print_points_to_p) (FILE *fp, POINTS_TO *ptmp);
#define Print_points_to (*Print_points_to_p)
#else
#pragma weak Print_points_to
#endif

// Emit 'T' or 'F' in the print routines for BOOLs
#define tf(x) ((x) ? 'T' : 'F')

/* =======================================================================
   REGION_new_wn
   transfers rid from old_wn to new_wn
   =======================================================================*/
void REGION_new_wn(WN *new_wn, WN *old_wn)
{
  RID *rid;
  if (new_wn == old_wn) /* they are the same already */
    return;
  rid = REGION_get_rid(old_wn);
  if (rid == NULL) {
    if (PU_has_region (Get_Current_PU ())) {
      FmtAssert(rid != NULL, ("REGION_new_wn, could not find matching RID"));
    }
    return;
  }
  WN_MAP_Set(RID_map, new_wn, rid);
  RID_rwn(rid) = new_wn;

  /* make sure rid_id and whirl rid_id match */
  Is_True(RID_id(rid) == WN_region_id(new_wn),
	  ("REGION_new_wn, region ids do not match"));
}

/* =======================================================================
   REGION_consistency_check
   This is called when Is_True_On but make it still be there in case
   someone calls it anyway.
   =======================================================================*/
BOOL REGION_consistency_check(WN *wn)
{
#ifdef Is_True_On
    if (PU_has_region (Get_Current_PU ())) {
    RID *rid = REGION_get_rid(wn);
    FmtAssert(rid != NULL,("REGION_consistency_check, could not find RID"));
    FmtAssert(RID_id(rid) == WN_region_id(wn),
	      ("REGION_consistency_check, region ids mismatch, RID %d, WN %d",
	       RID_id(rid), WN_region_id(wn)));
  }
#endif
  return TRUE;
}

/* =======================================================================
   REGION_update_alias_info
   go through every load and store in region and check for alias id
   this is done after cg generates glue code - the glue code needs alias info
   =======================================================================*/
void REGION_update_alias_info(WN *wn, struct ALIAS_MANAGER *alias_mgr)
{
  WN *wtmp;
  mINT16 i;
  OPCODE opc = WN_opcode(wn);
  BOOL trace = Get_Trace(TP_REGION, TT_REGION_ALL);

  if (PU_has_region (Get_Current_PU ())) {
    if (!alias_mgr) /* no alias manager (wopt.so not loaded) */
      return;

    /* OPC_PARM also?? */
    if (OPCODE_is_load(opc) || OPCODE_is_store(opc)) {
      if (Valid_alias(alias_mgr,wn) == FALSE) {
	Is_Trace(trace,
		 (TFile, "REGION_update_alias_info, alias id is zero\n"));
	Is_Trace_cmd(trace, fdump_wn(TFile, wn));
	Create_alias(alias_mgr, wn); /* fix the problem */
      }
    }

    /* recurse down the tree */
    switch (opc) {
      case OPR_BLOCK:
        for (wtmp=WN_first(wn); wtmp; wtmp=WN_next(wtmp))
	  REGION_update_alias_info(wtmp,alias_mgr);
	break;
      default:
        for (i=0; i<WN_kid_count(wn); i++)
	  REGION_update_alias_info(WN_kid(wn,i),alias_mgr);
	break;
    }
  }
}

/* =======================================================================
   REGION_get_rid
   ======================================================================= */
RID *REGION_get_rid(const WN *wn)
{
  if (RID_map == WN_MAP_UNDEFINED)	/* IPA doesn't have this defined */
    return NULL;
  return (wn) ? (RID *) WN_MAP_Get(RID_map, wn) : NULL;
}
/* second version used only by RID_Delete to delete RIDs with maps
   in a different context (the child PU has a different set of maps) */
static RID *REGION_get_rid2(WN_MAP_TAB *maptab, const WN *wn)
{
  if (RID_map == WN_MAP_UNDEFINED)	/* IPA doesn't have this defined */
    return NULL;
  return (wn) ? (RID *) IPA_WN_MAP_Get(maptab, RID_map, wn) : NULL;
}

/* =======================================================================
   REGION_find_pu
   given a WN * to a region, go up the RID tree to the PU WN *
   ======================================================================= */
WN *REGION_find_pu(WN *wn)
{
  RID *rtmp;
  WN *parent;

  Is_True(REGION_consistency_check(wn), ("REGION_find_pu"));

  for (rtmp=REGION_get_rid(wn); RID_parent(rtmp); rtmp=RID_parent(rtmp))
    ; /* NULL BODY */

  parent = RID_rwn(rtmp);

  Is_True(REGION_consistency_check(parent), ("REGION_find_pu"));
  return parent;
}

/* =======================================================================
   REGION_has_black_regions (non-recursive)
   Given a RID, determine if this region could be black later on
   based on the rid type.
   ======================================================================= */
BOOL REGION_has_black_regions(RID *rid)
{
  Is_True(rid != NULL, ("REGION_has_black_regions, RID is NULL"));
  Is_True(RID_TYPE_func_entry(rid) || !RID_TYPE_transparent(rid),
	  ("REGION_has_black_regions, called on a transparent region"));

  /* 7.3 specific assertion */
#ifdef TARG_SL //region_type_for_major
  Is_True(RID_TYPE_func_entry(rid) || RID_TYPE_olimit(rid) || RID_TYPE_major(rid) || 
#else 
  Is_True(RID_TYPE_func_entry(rid) || RID_TYPE_olimit(rid) ||
#endif  
	  RID_TYPE_pragma(rid),
	  ("REGION_has_no_black_regions, unsupported region type"));

  if (RID_contains_black(rid)) /* previously calculated, no traversal req'd */
    return TRUE;

  if (!RID_TYPE_func_entry(rid)) {
    /* search to top of RID tree and set bits on the way to the top */
    RID *rtmp;
    for (rtmp=rid; rtmp; rtmp=RID_parent(rtmp))
      RID_contains_black(rtmp) = TRUE;
  }
  return RID_contains_black(rid);
}

/* =======================================================================
   REGION_set_level (recursive)
   set the region_level for this RID and all its children
   ======================================================================= */
void REGION_set_level(RID *rid, REGION_LEVEL level)
{
  RID *rtmp;

  RID_level(rid) = level;

  for (rtmp=RID_first_kid(rid); rtmp; rtmp=RID_next(rtmp))
    REGION_set_level(rtmp, level);
}

/* =======================================================================
   REGION_count_exits
   given the exit block for a region, count the exits
   ======================================================================= */
INT32 REGION_count_exits(WN *exit_blk)
{
  INT32 count = 0;
  WN *wtmp;

  Is_True(WN_opcode(exit_blk) == OPC_BLOCK,
	  ("REGION_count_exits, can't find exit block"));

  if (WN_first(exit_blk) != NULL) {
    for (wtmp=WN_first(exit_blk); wtmp; wtmp=WN_next(wtmp))
	count++;
  }
  return count;
}

/* =======================================================================
   REGION_fix_up_exits
   recount exits for a region and expand or contract pregs sets if required
   ======================================================================= */
void REGION_fix_up_exits(RID *rid, WN *rwn)
{
  INT32 new_exits = REGION_count_exits(WN_region_exits(rwn));
  if (new_exits != RID_num_exits(rid)) { /* need to reallocate pregs_out */
    INT32 i, limit;
    PREG_LIST **new_pregs_out = TYPE_MEM_POOL_ALLOC_N(PREG_LIST *,
					      &REGION_mem_pool, new_exits);
    limit = MAX(new_exits, RID_num_exits(rid));
    for (i=0; i<limit; i++) {
      if (i < RID_num_exits(rid))	/* copy old exit list pointers */
	new_pregs_out[i] = RID_pregs_out_i(rid, i);
      else				/* initialize new exit lists */
	new_pregs_out[i] = NULL;
    }
    RID_pregs_out(rid) = new_pregs_out;
    RID_num_exits(rid) = new_exits;
  }
}

/* =======================================================================
   REGION_scan_exits
   Look for a given label number in the exit block for a RID, TRUE if found
   This is needed for wopt to build a CFG correctly.
   ======================================================================= */
BOOL REGION_scan_exits(WN *wn, INT32 label_no)
{
  WN *wtmp;

  Is_True(wn && label_no, ("REGION_scan_exits, parameters out of range"));
  Is_True(WN_opcode(wn) == OPC_BLOCK,
	  ("REGION_scan_exits, can't find exit block"));
  for (wtmp=WN_first(wn); wtmp; wtmp=WN_next(wtmp))
    if (WN_label_number(wtmp) == label_no)
      return TRUE;
  return FALSE;
}

/* =======================================================================
   RID_Create
   ======================================================================= */
RID *RID_Create( INT id, INT depth, WN *wn )
{
  RID *rid, *wrid;

  rid = TYPE_MEM_POOL_ALLOC(RID, &REGION_mem_pool);
  RID_id(rid) = (id == RID_CREATE_NEW_ID) ? New_Region_Id() : id;
  RID_depth(rid) = depth;
  RID_flags(rid) = 0;

  if (wn) {
    if (wrid = (RID *)WN_MAP_Get(RID_map, wn))
      RID_srcpos(rid) = RID_srcpos(wrid);
    else
      RID_srcpos(rid) = WN_Get_Linenum(wn);
    RID_rwn(rid) = wn;
  } else {
    RID_srcpos(rid) = (SRCPOS)0;
    RID_rwn(rid) = (WN *)NULL;
  }

  RID_num_exits(rid) = 0;

  /* live-in, live-out sets */
  RID_pregs_in (rid) = (PREG_LIST *)NULL;
  RID_pregs_out(rid) = (PREG_LIST **)NULL;
  RID_used_in(rid) = (POINTS_TO_SET *)NULL;
  RID_def_in_live_out(rid) = (POINTS_TO_SET *)NULL;

  RID_options(rid)   = (char *)NULL;
  RID_parent(rid)    = (RID *)NULL;
  RID_first_kid(rid) = (RID *)NULL;
  RID_next(rid)      = (RID *)NULL;
  RID_lowered(rid)   = (LOWER_ACTIONS)NULL;
  RID_parent_block(rid) = (WN *)NULL;

  return rid;
}

/* given two RIDs make the first the child of the second */
void RID_Add_kid(RID *new_kid, RID *parent)
{
  RID *rtmp;

  if (RID_first_kid(parent) == NULL) {
    RID_first_kid(parent) = new_kid;
  } else {
    for (rtmp=RID_first_kid(parent); rtmp; rtmp=RID_next(rtmp)) {
      if (RID_next(rtmp) == NULL) {
	RID_next(rtmp) = new_kid;
	break;
      }
    }
  }
  RID_parent(new_kid) = parent;
}

/* remove a RID from the RID tree */
void RID_unlink(RID *rid)
{
  RID *rtmp, *rprev, *parent = RID_parent(rid);

  for (rprev=rtmp=RID_first_kid(parent); rtmp; rtmp=RID_next(rtmp)) {
    if (rtmp == rid) {
      if (rtmp == RID_first_kid(parent))
	RID_first_kid(parent) = RID_next(rtmp);
      else 
	RID_next(rprev) = RID_next(rtmp);
      RID_next(rid) = NULL;
      return;
    }
    rprev = rtmp;
  }
  FmtAssert(FALSE, ("RID_unlink, did not find rid"));
}

/* replace old_rid with new_rid, leaving old_rid unlinked */
void RID_replace (RID *old_rid, RID *new_rid)
{
  RID *rtmp, *rprev, *parent = RID_parent(old_rid);

  for (rprev=rtmp=RID_first_kid(parent); rtmp; rtmp=RID_next(rtmp)) {
    if (rtmp == old_rid) {
      /* found it */
      RID_next(new_rid) = RID_next(rtmp);
      if (rtmp == RID_first_kid(parent))
	RID_first_kid(parent) = new_rid;
      else 
	RID_next(rprev) = new_rid;
      RID_next(old_rid) = NULL;
      RID_parent(new_rid) = parent;
      return;
    }
    rprev = rtmp;
  }
  FmtAssert(FALSE, ("RID_replace, did not find rid"));
}

/* =======================================================================
   finds last kid, used by RID_Delete2()
   =======================================================================*/
static RID *RID_last_kid(RID *parent)
{
  RID *rtmp;
  Is_True(parent != NULL && RID_first_kid(parent) != NULL,
	  ("RID_last_kid, parent or first kid is NULL"));
  for (rtmp=RID_first_kid(parent); RID_next(rtmp); rtmp=RID_next(rtmp))
    ; /* NULL BODY */
  Is_True(rtmp != NULL, ("RID_last_kid, null last kid"));
  return rtmp;
}

/* =======================================================================
   for every RID in list, set it's parent pointer to parent
   used by RID_Delete2()
   =======================================================================*/
static void RID_set_parents(RID *list, RID *parent)
{
  RID *rtmp;
  Is_True(parent != NULL && list != NULL,
	  ("RID_set_parents, parent or RID list is NULL"));
  for (rtmp=list; rtmp; rtmp=RID_next(rtmp))
    RID_parent(rtmp) = parent;
}

/* =======================================================================
   RID_Delete
   Unlink the map from the WN tree to the RID tree. Used by MP lowering
   when it deletes a region and replaces it with a nested PU.
   =======================================================================*/
void RID_Delete(WN_MAP_TAB *maptab, WN *wn)
{
  RID *rid = REGION_get_rid2(maptab, wn);

  Is_True(rid != NULL, ("RID_Delete, could not find rid"));
  IPA_WN_MAP_Set(maptab, RID_map, wn, NULL); /* pointer from wn to rid */

  RID_Delete2(rid);
}

/* =======================================================================
   RID_Delete2
   Actually does the deletion of the rid from the RID tree.
   If the rid has kids, link them to the parent.
   This allows regions to be deleted in any order.
   =======================================================================*/
void RID_Delete2(RID *rid)
{
  RID *rtmp, *rprev, *parent, *last;

  Is_True(rid != NULL && RID_id(rid) != -1,
	  ("RID_Delete2, passed in NULL rid"));
  parent = RID_parent(rid);
  Is_True(parent != NULL && RID_id(parent) != -1,
	  ("RID_Delete2, tried to delete root RID"));
  rprev = RID_first_kid(parent);
  Is_True(rprev != NULL && RID_id(rprev) != -1,
	  ("RID_Delete2, RID tree inconsistency"));

  if (rprev == rid) {
    if (RID_first_kid(rid) != NULL) {	// need to move kids up a level
      RID_set_parents(RID_first_kid(rid),parent); // set rid's kids to parent
      last = RID_last_kid(rid);
      RID_next(last) = RID_next(rid);
      RID_first_kid(parent) = RID_first_kid(rid);
    } else				// can delete rid, no kids
      RID_first_kid(parent) = RID_next(rid);
  } else {
    for (rtmp=RID_next(rprev); rtmp; rtmp=RID_next(rtmp)) {
      if (rtmp == rid) { // found it, delete it
	if (RID_first_kid(rid) != NULL) { // need to move kids up a level
	  RID_set_parents(RID_first_kid(rid),parent);
	  last = RID_last_kid(rid);
	  RID_next(last) = RID_next(rid);
	  RID_next(rprev) = RID_first_kid(rid);
	} else				// can delete rid, no kids
	  RID_next(rprev) = RID_next(rid);
	break;
      }
      rprev = rtmp;
    }
  }
  // clear out some fields, just in case
  RID_id(rid) = -1;
  RID_rwn(rid) = NULL;		// pointer from rid to wn
  RID_parent_block(rid) = NULL;
  RID_parent(rid) = NULL;
  RID_first_kid(rid) = NULL;
  RID_next(rid) = NULL;

  if (RID_options(rid) != NULL)
    CXX_DELETE_ARRAY(RID_options(rid), &REGION_mem_pool);
  RID_options(rid) = NULL;	// options string
}

/* =======================================================================
   REGION_clone
   LNO sometimes duplicates code, if that code has a region in it we
   need to clone the RID and link it into the RID tree.
   Assumptions:
     1) old_wn must have a parent region or func_entry
     2) old_wn must have correct exits and links to RID tree
     3) both new and old WNs must be regions
     4) the 3rd parameter is the parent - needed for cloning nested
     	regions. if it is null, use parent of old_wn.
     5) if the parent RID cannot be found let it go.
   Limitations:
     1) boundary info cannot be transferred
     2) region exits are not re-analyzed
   =======================================================================*/
void REGION_clone(WN *old_wn, WN *new_wn, WN *parent_wn)
{
  RID *o_rid, *n_rid, *p_rid;

  Is_True(WN_opcode(old_wn) == OPC_REGION,
	  ("REGION_clone, old_wn not a region"));
  Is_True(WN_opcode(new_wn) == OPC_REGION,
	  ("REGION_clone, new_wn not a region"));
  Is_True(parent_wn == NULL || WN_opcode(parent_wn) == OPC_REGION ||
	   WN_opcode(parent_wn) == OPC_FUNC_ENTRY,
	  ("REGION_clone, expecting a REGION or PU for parent_wn"));

  o_rid = REGION_get_rid(old_wn);
  Is_True(o_rid != NULL, ("REGION_clone, no original region"));
  if (parent_wn)	/* parent is given */
    p_rid = REGION_get_rid(parent_wn);
  else			/* use parent of old_wn */
    p_rid = RID_parent(o_rid);

  Is_True(WN_region_id(new_wn) != 0, ("REGION_clone, can't clone PU"));
  n_rid = RID_Create(WN_region_id(new_wn), RID_level(o_rid), new_wn);

  /* copy some flags over, set others */
  RID_level(n_rid) = RID_level(o_rid);
  RID_type(n_rid) = RID_type(o_rid);
  RID_depth(n_rid) = RID_depth(o_rid);
  RID_srcpos(n_rid) = WN_Get_Linenum(new_wn);
  RID_bounds_exist(n_rid) = REGION_BOUND_UNKNOWN;
  Is_True(RID_bounds_exist(o_rid) == REGION_BOUND_UNKNOWN,
	  ("REGION_clone, not enough information to clone boundary sets"));
  RID_has_return(n_rid) = REGION_NO_RETURN;
  RID_num_exits(n_rid) = RID_num_exits(o_rid);
  RID_eh_range_ptr(n_rid) = RID_eh_range_ptr(o_rid);
  Is_True(RID_num_exits(o_rid) == 1 || RID_num_exits(o_rid) == 0,
	  ("REGION_clone, not enough information to clone region exits"));

  /* connect to RID to WHIRL */
  WN_MAP_Set(RID_map, new_wn, (void *)n_rid);
  RID_rwn(n_rid) = new_wn;
  if (p_rid)
    RID_Add_kid(n_rid, p_rid);
}

/* =======================================================================
   REGION_emit
   This code is common to the preopt/mainopt/rvi emitters and is used for
	the func_entries and regions for each (6 times total).
   When called on a func_entry, the rid is gotten from Cfg()->Rid().
   For regions it is in the bb node.
   This code gets called for func_entries also, can occur without regions
   being "on", e.g. -pfa.
   =======================================================================*/
void REGION_emit(RID *rid, WN *rwn, INT32 level, INT32 nexits, INT64 linenum)
{
  FmtAssert(rid != NULL,("REGION_emit, can't find RID"));
  RID_rwn(rid) = rwn;
  RID_level(rid) = level;
  WN_MAP_Set(RID_map, rwn, (void *)rid);
  RID_num_exits(rid) = nexits;
  if (linenum != 0)
    WN_Set_Linenum(rwn, linenum);
  Is_True(rid == REGION_get_rid(rwn), ("REGION_emit, map error"));
}

/* =======================================================================
   RID_copy_sets
   Copy the boundary sets from one rid to another.
   =======================================================================*/
void RID_copy_sets(RID *new_rid, RID *rid_orig)
{
  RID_used_in(new_rid) = RID_used_in(rid_orig);
  RID_def_in_live_out(new_rid) = RID_def_in_live_out(rid_orig);
  RID_pregs_in(new_rid) = RID_pregs_in(rid_orig);
  RID_pregs_quad(new_rid) = RID_pregs_quad(rid_orig);
  RID_pregs_complex_quad(new_rid) = RID_pregs_complex_quad(rid_orig);
  if (RID_num_exits(new_rid) > 0 && RID_pregs_out(rid_orig) != NULL) {
    INT32 i;
    RID_pregs_out(new_rid) = TYPE_MEM_POOL_ALLOC_N(PREG_LIST *,
	   &REGION_mem_pool, RID_num_exits(new_rid));
    for (i=0; i<RID_num_exits(new_rid); i++)
      RID_pregs_set_out_i(new_rid,i) = RID_pregs_out_i(rid_orig,i);
  }
}

/* =======================================================================
   RID_preopt_level
   sets the RID level for preopt based on which phase called it
   =======================================================================*/
REGION_LEVEL RID_preopt_level(INT phase)
{
  /* these are the possible places that can call preopt, see optimizer.h */
  switch (phase) {
    case PREOPT_IPA0_PHASE:	return RL_IPA_PREOPT;
    case PREOPT_IPA1_PHASE:	return RL_IPA_PREOPT;
    case PREOPT_LNO_PHASE:	return RL_LNO_PREOPT;
    case PREOPT_LNO1_PHASE:     return RL_LNO1_PREOPT;
    case PREOPT_DUONLY_PHASE:	return RL_DU_PREOPT;
    case PREOPT_PHASE:		return RL_PREOPT;
    case MAINOPT_PHASE:		return RL_MAINOPT;
  }
  Is_True(FALSE,("RID_preopt_level, Preopt called by unknown phase"));
  return RL_UNKNOWN; /* to satisfy compiler */
}

/* =======================================================================
   REGION_add_wn_points_to
   Create a POINTS_TO from a WHIRL node and add to list unconditionally.
   Called by cg_region.c/REGION_Entry_PREG_Whirl to add POINTS_TOs for glue
   code. Don't call this for pregs.
   =======================================================================*/
void REGION_add_wn_points_to(POINTS_TO_SET **pset, WN *wn,
			  struct ALIAS_MANAGER *am)
{
  POINTS_TO_SET *ptr;

  if (am) { /* only do if alias manager is loaded (wopt.so) */
    ST *st = WN_st(wn);    
    Is_True(st != NULL && ST_class(st) != CLASS_PREG,
	    ("REGION_add_wn_points_to, found a PREG"));
    Is_Trace(Get_Trace(TP_REGION,TT_REGION_BOUND_DEBUG),
	      (TFile,"REGION_add_wn_points_to, adding wn to POINTS_TO_SET\n"));
    Is_Trace_cmd(Get_Trace(TP_REGION,TT_REGION_BOUND_DEBUG),
		 fdump_tree(TFile,wn));
    ptr = TYPE_MEM_POOL_ALLOC(POINTS_TO_SET, &REGION_mem_pool);
    ptr->Next = *pset; /* add to set */
    *pset = ptr;
    Create_alias(am, wn); /* WHIRL to POINTS_TO mapping */
    ptr->Pt = Points_to_copy(Points_to(am, wn), &REGION_mem_pool); /* alloc */
  }
}

/* =======================================================================
   REGION_add_points_to (similar to REGION_add_wn_points_to)
   Given a POINTS_TO from a WHIRL, add to list unconditionally.
   Do not search for duplicates.
   Call by opt_emit.cxx/REGION_used_in_from_chi to add used_in for entry chi.
   Don't call this for pregs.
   =======================================================================*/
void REGION_add_points_to(POINTS_TO_SET **pset, POINTS_TO *pt,
			  struct ALIAS_MANAGER *am)
{
  POINTS_TO_SET *ptr;

  if (am) { /* only do if alias manager is loaded (wopt.so) */
    Is_Trace(Get_Trace(TP_REGION,TT_REGION_ALL),
	     (TFile,"REGION_add_points_to, adding pt to POINTS_TO_SET\n"));
    Is_Trace_cmd(Get_Trace(TP_REGION,TT_REGION_ALL),
		 Print_points_to(TFile,pt));

    ptr = TYPE_MEM_POOL_ALLOC(POINTS_TO_SET, &REGION_mem_pool);
    ptr->Next = *pset; /* add to set */
    *pset = ptr;
    ptr->Pt = Points_to_copy(pt, &REGION_mem_pool);
  }
}

/* =======================================================================
   REGION_add_preg_in
   quad: MTYPE_V for normal, MTYPE_FQ for quad (FQ),
   	MTYPE_CQ for complex quad (CQ), also takes MTYPE_C4, MTYPE_C8
   (puts the quad ones on in reverse order so it in order when done)
   returns TRUE if it did anything
   =======================================================================*/
BOOL REGION_add_preg_in(RID *rid, PREG_NUM pr, TYPE_ID quad)
{
  BOOL ret = FALSE;
  Is_True(rid != NULL, ("REGION_add_preg_in, NULL RID"));
#ifdef Is_True_On
  BOOL trace = Get_Trace(TP_REGION,TT_REGION_ALL) ||
    Get_Trace(TP_REGION,TT_REGION_BOUND_DEBUG);
#endif

  INT32 npregs = Preg_Increment(quad);
  if (npregs == 2) { // quad
    // for quads, put on quad list and pr+1
    // for C4, it actually is two F4's so not a quad
    if ((quad == MTYPE_FQ || quad == MTYPE_C8 || quad == MTYPE_C10) &&
	!REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
      RID_pregs_quad(rid) = PREG_LIST_Push(pr, RID_pregs_quad(rid), 
					   &REGION_mem_pool);
      ret = TRUE;
    }
    // check for duplicates of the second preg
    if (!REGION_search_preg_set(RID_pregs_in(rid), pr+1)) {
      RID_pregs_in(rid) = PREG_LIST_Push(pr+1,RID_pregs_in(rid),
					 &REGION_mem_pool);
      ret = TRUE;
      Is_Trace(trace, (TFile,"REGION_add_preg_in, adding quad part 2 "
		       "PREG %d to in-set, RGN %d\n", pr+1, RID_id(rid)));
    }
  } else if (npregs == 4) { // complex quad
    Is_True(quad == MTYPE_CQ || quad == MTYPE_C16, ("REGION_add_preg_in, not a complex quad"));
    // for complex quads, put on complex list, and pr, pr+1, pr+2, pr+3
    if (!REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
      RID_pregs_complex_quad(rid) = PREG_LIST_Push(pr,
					   RID_pregs_complex_quad(rid), 
					   &REGION_mem_pool);
      ret = TRUE;
    }
    // check for duplicates of the remaining pregs
    for (PREG_NUM ptmp=pr+3; ptmp>=pr+1; ptmp--) {
      if (REGION_search_preg_set(RID_pregs_in(rid), ptmp))
	continue;
      RID_pregs_in(rid) = PREG_LIST_Push(ptmp, RID_pregs_in(rid),
					 &REGION_mem_pool);
      ret = TRUE;
      Is_Trace(trace, (TFile,"REGION_add_preg_in, adding complex quad "
		       "PREG %d to in-set, RGN %d\n", ptmp, RID_id(rid)));
    }
  }

  // check for duplicates - linear search - slow
  if (!REGION_search_preg_set(RID_pregs_in(rid), pr)) {
    RID_pregs_in(rid) = PREG_LIST_Push(pr, RID_pregs_in(rid),
				       &REGION_mem_pool);
    ret = TRUE;
  }
  Is_Trace(trace && ret, (TFile,"REGION_add_preg_in, adding PREG %d (%s) "
			  "to in-set, RGN %d\n", pr, MTYPE_name(quad),
			  RID_id(rid)));
  return ret;
}

/* =======================================================================
   REGION_add_preg_out
   quad: MTYPE_V for normal, MTYPE_FQ for quad (FQ),
   	MTYPE_CQ for complex quad (CQ), also takes MTYPE_C4, MTYPE_C8
   (puts the quad ones on in reverse order so it in order when done)
   returns TRUE if it did anything
   =======================================================================*/
BOOL REGION_add_preg_out(RID *rid, INT32 which_set, PREG_NUM pr, TYPE_ID quad)
{
  INT32 i;
  BOOL ret = FALSE;
#ifdef Is_True_On
  BOOL trace = Get_Trace(TP_REGION,TT_REGION_ALL) ||
    Get_Trace(TP_REGION,TT_REGION_BOUND_DEBUG);
#endif
  
  Is_True(rid != NULL, ("REGION_add_preg_out, NULL RID"));
  Is_True(which_set >=0 && which_set < RID_num_exits(rid),
	  ("REGION_add_preg_out, trying to add to preg set %d", which_set));
  // passive initialization
  if (RID_pregs_out(rid) == NULL) {
    RID_pregs_out(rid) = TYPE_MEM_POOL_ALLOC_N(PREG_LIST *,
			       &REGION_mem_pool, RID_num_exits(rid));
    for (i=0; i<RID_num_exits(rid); i++)
      RID_pregs_set_out_i(rid, i) = (PREG_LIST *)NULL;
  }

  INT32 npregs = Preg_Increment(quad);
  if (npregs == 2) { // quad
    // for quads, put on quad list and pr+1
    // for C4, it actually is two F4's so not a quad
    if ((quad == MTYPE_FQ || quad == MTYPE_C8) &&
	!REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
      RID_pregs_quad(rid) = PREG_LIST_Push(pr, RID_pregs_quad(rid), 
					   &REGION_mem_pool);
      ret = TRUE;
    }
    // check for duplicates of the second preg
    if (!REGION_search_preg_set(RID_pregs_out_i(rid, which_set), pr+1)) {
      RID_pregs_set_out_i(rid, which_set) = PREG_LIST_Push(pr+1,
					       RID_pregs_out_i(rid,which_set),
					       &REGION_mem_pool);
      ret = TRUE;
      Is_Trace(trace, (TFile,"REGION_add_preg_out, adding quad part 2 "
		       "PREG %d to out-set %d, RGN %d\n",
		       pr+1, which_set, RID_id(rid)));
    }
  } else if (npregs == 4) { // complex quad
    Is_True(quad == MTYPE_CQ || quad == MTYPE_C16, ("REGION_add_preg_in, not a complex quad"));
    // for complex quads, put on complex list, and pr, pr+1, pr+2, pr+3
    if (!REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
      RID_pregs_complex_quad(rid) = PREG_LIST_Push(pr,
					   RID_pregs_complex_quad(rid), 
					   &REGION_mem_pool);
      ret = TRUE;
    }
    // check for duplicates of the remaining pregs
    for (PREG_NUM ptmp=pr+3; ptmp>=pr+1; ptmp--) {
      if (REGION_search_preg_set(RID_pregs_out_i(rid, which_set), ptmp))
	continue;
      RID_pregs_set_out_i(rid, which_set) = PREG_LIST_Push(ptmp,
						RID_pregs_out_i(rid,which_set),
						&REGION_mem_pool);
      ret = TRUE;
      Is_Trace(trace, (TFile,"REGION_add_preg_out, adding complex quad "
		       "PREG %d to out-set %d, RGN %d\n",
		       ptmp, which_set, RID_id(rid)));
    }
  }

  // check for duplicates - linear search - slow
  if (!REGION_search_preg_set(RID_pregs_out_i(rid, which_set), pr)) {
    RID_pregs_set_out_i(rid, which_set) = PREG_LIST_Push(pr,
					        RID_pregs_out_i(rid,which_set),
						&REGION_mem_pool);
    ret = TRUE;
  }
  Is_Trace(trace && ret, (TFile,"REGION_add_preg_out, adding PREG %d (%s) "
			  "to out-set %d, RGN %d\n",
			  pr, MTYPE_name(quad), which_set, RID_id(rid)));
  return ret;
}

/* =======================================================================
   REGION_remove_preg, removes a PREG from either the pregs_in or all
   the pregs_out sets. Handles quads as well. Returns TRUE if it did anything.
   Called at the end of WOPT and CG to update the boundary sets.
   =======================================================================*/
BOOL
REGION_remove_preg(RID *rid, PREG_NUM pr, BOOL outset)
{
  INT i;
  Is_True(rid != NULL, ("REGION_remove_preg, NULL RID"));
  Is_True(!RID_TYPE_transparent(rid), ("REGION_remove_preg, transparent RID"));
  Is_True(RID_bounds_exist(rid) == REGION_BOUND_EXISTS,
	  ("REGION_remove_preg, region bounds do not exist"));

  if (outset) { /* pregs_out sets */

    if (RID_pregs_out(rid) == NULL)
      return FALSE;

    /* go through all the exits sets */
    for (i=0; i<RID_num_exits(rid); i++) {
      /* find the preg in the list and delete */
      RID_pregs_set_out_i(rid,i) = PREG_LIST_Delete(pr,
						  RID_pregs_set_out_i(rid,i));
      Is_Trace(Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
	       (TFile,"REGION_remove_preg(RGN %d), removing PREG %d "
		"from out-set %d\n", RID_id(rid), pr, i));

      // look for second part of a quad
      if (REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
	RID_pregs_set_out_i(rid,i) = PREG_LIST_Delete(pr+1,
				      RID_pregs_set_out_i(rid,i));
	Is_Trace(Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
		 (TFile,"REGION_remove_preg(RGN %d), removing quad part 2 "
		  "PREG %d from out-set %d\n", RID_id(rid), pr+1, i));
	// do not remove from quad set if it is there
	// (there may be other sets that use it)
      }

      // look for remaining 3 pregs if complex quad 
      if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
	for (PREG_NUM ptmp=pr+1; ptmp<pr+4; ptmp++) {
	  RID_pregs_set_out_i(rid,i) = PREG_LIST_Delete(ptmp,
						  RID_pregs_set_out_i(rid,i));
	  Is_Trace(Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
		   (TFile,"REGION_remove_preg(RGN %d), removing complex quad "
		  "PREG %d from out-set %d\n", RID_id(rid), ptmp, i));
	}
	// do not remove from complex quad set if it is there
	// (there may be other sets that use it)
      }

    }  // for (INT i=0; i<RID_num_exits(rid); i++)

  } else { // pregs_in set

    if (RID_pregs_in(rid) == NULL)
      return FALSE;

    RID_pregs_in(rid) = PREG_LIST_Delete(pr, RID_pregs_in(rid));
    Is_Trace(Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
	     (TFile,"REGION_remove_preg(RGN %d), removing PREG %d "
	      "from in-set\n", RID_id(rid), pr));

    // look for second part of a quad
    if (REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
      RID_pregs_in(rid) = PREG_LIST_Delete(pr+1, RID_pregs_in(rid));
      Is_Trace(Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
	       (TFile,"REGION_remove_preg(RGN %d), removing quad part 2 "
		"PREG %d from in-set\n", RID_id(rid), pr+1));
      // do not remove from quad set if it is there
      // (there may be other sets that use it)
    }

    // look for remaining 3 pregs if complex quad 
    if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
      for (PREG_NUM ptmp=pr+1; ptmp<pr+4; ptmp++) {
	RID_pregs_in(rid) = PREG_LIST_Delete(pr+1, RID_pregs_in(rid));
	Is_Trace(Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
		 (TFile,"REGION_remove_preg(RGN %d), removing complex quad "
		  "PREG %d from in-set\n", RID_id(rid), ptmp));
      }
      // do not remove from quad set if it is there
      // (there may be other sets that use it)
    }

  } /* else */
  return TRUE;
}

/* =======================================================================
   REGION_add_exit: returns new wn for after region (it added
   	the external label node)
   You're on your own for updating RID_num_exits because no RID is passed
   =======================================================================*/
WN *
REGION_add_exit(WN *block, WN *after, WN *region)
{
  WN *wn_label, *wn_exit, *wn_last;

  LABEL_IDX label;
  New_LABEL(CURRENT_SYMTAB, label);
  wn_label = WN_CreateLabel(label, 0, NULL);
  WN_Set_Linenum(wn_label, (after) ? WN_Get_Linenum(after) :
		 		     WN_Get_Linenum(block));

  WN_INSERT_BlockBefore(block, after, wn_label); /* after can be NULL */

  /* REGION_EXIT for end of region */
  wn_exit = WN_CreateRegionExit (label);
  wn_last = WN_last(WN_region_body(region));
  WN_Set_Linenum(wn_exit, WN_Get_Linenum(wn_last));
  WN_INSERT_BlockAfter(WN_region_body(region), wn_last, wn_exit);

  return wn_label;
}

/* =======================================================================
   REGION_delete_exit: delete a region exit from the exit block and
   decrement RID_num_exits(). If there are PREGs out sets, compact
   them so they are correct. Then go up to root rid and get rid of this
   exit all the way up. We can do this because each is unique.
   =======================================================================*/
void
REGION_delete_exit(RID *rid, INT32 label, WN *exit_block, BOOL recursive_call)
{
  WN *wtmp;
  INT32 which_set = 0;
  BOOL found = FALSE;

  Is_True(RID_num_exits(rid) > 0,
	  ("REGION_delete_exit, no exit to remove, RGN %d",RID_id(rid)));
  Is_Trace(Get_Trace(TP_REGION, TT_REGION_ALL), (TFile,
	    "REGION_delete_exit, REGION_EXIT L%d from RGN %d, #exits %d\n",
	    label, RID_id(rid), RID_num_exits(rid)));
  
  /* first find the REGION_EXIT and delete */
  Is_True (WN_first(exit_block) != NULL,("REGION_delete_exit, no exit block"));
  for (wtmp=WN_first(exit_block); wtmp; wtmp=WN_next(wtmp)) {
    Is_True(WN_opcode(wtmp) == OPC_REGION_EXIT,
	    ("REGION_delete_exit, illegal opcode in exit block"));
    if (WN_label_number(wtmp) == label) { /* found it */
      /* delete from exit list */
      WN_DELETE_FromBlock(exit_block, wtmp);
      /* delete from pregs out sets */
      if (RID_pregs_out(rid) != NULL) {
	INT32 i;
	for (i=which_set; i<RID_num_exits(rid)-1; i++)
	  RID_pregs_set_out_i(rid, i) = RID_pregs_out_i(rid, i+1);
      }
      /* decrement num_exits */
      RID_num_exits(rid)--;
      found = TRUE;
    }
    which_set++;
  }

  /* if this is the first call, we better have found the exit */
  if (recursive_call == FALSE)
    Is_True(found == TRUE,
	    ("REGION_delete_exit, region exit not found, RGN %d, label %d",
	     RID_id(rid), label));

  /* If we found the exit, go up a level in the RID tree and check again */
  if (found) {
    RID *parent_rid = RID_parent(rid);
    WN *parent_wn;

    Is_True(parent_rid != NULL, ("REGION_delete_exit, could not find parent"));
    parent_wn = RID_rwn(parent_rid);
    /* No exits to remove in PU */
    /* Since LNO does not handle regions, they are removed by preopt
       and so the parent_wn may be NULL (PV 457243, 475929) */
    if (RID_TYPE_func_entry(parent_rid) || parent_wn == NULL)
      return;
    Is_True(parent_wn != NULL && 
	    (WN_opcode(parent_wn) == OPC_FUNC_ENTRY ||
	     WN_opcode(parent_wn) == OPC_REGION),
	    ("REGION_delete_exit, parent RID has invalid WHIRL"));
    exit_block = WN_region_exits(parent_wn);
    Is_True(exit_block != NULL && WN_opcode(exit_block) == OPC_BLOCK,
	    ("REGION_delete_exit, parent exit block is malformed"));
    REGION_delete_exit(parent_rid, label, exit_block, TRUE);
  }
}

/* =======================================================================
   REGION_propagate_return(RID *)
   =======================================================================*/
void
REGION_propagate_return(RID *rid)
{
  RID *rtmp;

  for (rtmp=rid; rtmp; rtmp=RID_parent(rtmp)) {
    if (RID_has_return(rtmp) == REGION_NO_RETURN) {
      RID_has_return(rtmp) = REGION_RETURN;
      if (Get_Trace(TP_REGION,TT_REGION_ALL))
	fprintf(TFile,"REGION_propagate_return(RGN %d)\n",RID_id(rtmp));
    }
  }
}

/* =======================================================================
   RID_level_str
   =======================================================================*/
char *RID_level_str(RID *rid)
{
  static char buff[20];

  switch (RID_level(rid)) {
    case RL_UNKNOWN:	strcpy(buff,"RL_UNKNOWN");	break;
    case RL_SRC:	strcpy(buff,"RL_SRC");		break;
    case RL_MP:		strcpy(buff,"RL_MP");		break;
    case RL_RGN_INIT:	strcpy(buff,"RL_RGN_INIT");	break;
    case RL_LNO_PREOPT:	strcpy(buff,"RL_LNO_PREOPT");	break;
    case RL_LNO1_PREOPT: strcpy(buff,"RL_LNO1_PREOPT");	break;
    case RL_LNO:	strcpy(buff,"RL_LNO");		break;
    case RL_RAIL:	strcpy(buff,"RL_RAIL");		break;
    case RL_RBI:	strcpy(buff,"RL_RBI");		break;
    case RL_PREOPT:	strcpy(buff,"RL_PREOPT");	break;
    case RL_MAINOPT:	strcpy(buff,"RL_MAINOPT");	break;
    case RL_RVI1:	strcpy(buff,"RL_RVI1");		break;
    case RL_RVI2:	strcpy(buff,"RL_RVI2");		break;
    case RL_CG:		strcpy(buff,"RL_CG");		break;
    case RL_CGSCHED:	strcpy(buff,"RL_CGSCHED");	break;
    default:
      strcpy(buff,"ILLEGAL");
      Is_True(FALSE,("RID_level_str, internal inconsistency"));
      break;
  }
  return buff;
}

/* =======================================================================
   RID_type_str
   =======================================================================*/
char *RID_type_str(RID_TYPE type)
{
  static char buff[100];

  buff[0] = '\0';

  if (type & RID_TYPE_func_entry)
    strcat(buff," FUNC_ENTRY");
  if (type & RID_TYPE_loop)
    strcat(buff," LOOP");
  if (type & RID_TYPE_pragma)
    strcat(buff," PRAGMA");
  if (type & RID_TYPE_olimit)
    strcat(buff," OLIMIT");
  if (type & RID_TYPE_mp)
    strcat(buff," MP");
  if (type & RID_TYPE_rpi)
    strcat(buff," RPI");
  if (type & RID_TYPE_cold)
    strcat(buff," COLD");
  if (type & RID_TYPE_swp)
    strcat(buff," SWP");
#ifdef TARG_SL //fork_joint
  if (type & RID_TYPE_major)
    strcat(buff," SL2_MAJOR");
  if(type & RID_TYPE_minor)
    strcat(buff, "SL2_MINOR");
#endif
  if (type & RID_TYPE_try)
    strcat(buff," TRY");
  if (type & RID_TYPE_cleanup)
    strcat(buff," CLEANUP");
  if (type & RID_TYPE_exc_spec)
    strcat(buff," EXC_SPEC");
  if (type & RID_TYPE_mask)
    strcat(buff," MASK");
  if (type & RID_TYPE_guard)
    strcat(buff," GUARD");
  if (type & RID_TYPE_null_cleanup)
    strcat(buff," NULL_CLEANUP");

  if (type & RID_TYPE_eh) /* mask */
    strcat(buff," EH");

  if (type == RID_TYPE_undefined)
    strcpy(buff," UNDEFINED");

  return buff;
}

/* =======================================================================
   RID_lower_Fprint
   Print the actions associated with the flag actions
   =======================================================================*/
static void RID_lower_Fprint(FILE *FD, LOWER_ACTIONS actions)
{
  UINT64 i = 1;

  while(actions) {
    if (actions & i) {
      fprintf(FD, "%s ", LOWER_ACTIONS_name( i ));
      actions = actions ^ i;
    }
    i <<= 1;
  }
  fprintf(FD, "\n");
}

/* =======================================================================
   support functions for RID_set_print
   =======================================================================*/
static void
Dump_preg_list(FILE *FD, PREG_LIST *plist)
{
  PREG_LIST *ptmp;
  PREG_NUM pr;
  INT count = 0;

  if (plist)
    fprintf(FD,"    ");
  for (ptmp = plist; ptmp; ptmp = PREG_LIST_rest(ptmp)) {
    pr = PREG_LIST_first(ptmp);
    fprintf(FD,"%d ",pr);
    if (++count >= 10 && PREG_LIST_rest(ptmp) != NULL) {
      count = 0;
      fprintf(FD,"\n    ");
    }
  }
  if (plist)
    fprintf(FD,"\n");
}

static void
Dump_points_to_list(FILE *FD, POINTS_TO_SET *ptset, const char *str)
{
  fprintf(FD,"  %s\n",str);
  if (ptset == NULL)
    fprintf(FD,"    NULL\n");
  else {
    POINTS_TO_SET *ptmp;
    for (ptmp=ptset; ptmp; ptmp=ptmp->Next) {
      fprintf(FD,"    ");
      Print_points_to(FD,ptmp->Pt);
    }
  }
}

/* returns number of region exits it finds */
static void
Dump_region_exits(FILE *FD, WN *exit_blk)
{
    WN *wtmp;
    Is_True(WN_opcode(exit_blk) == OPC_BLOCK,
	    ("Dump_region_exits, illegal input"));
    if (WN_first(exit_blk) != NULL) {
      fprintf(FD,"  region exits: ");
      for (wtmp=WN_first(exit_blk); wtmp; wtmp=WN_next(wtmp))
	fprintf(FD,"L%d ",WN_label_number(wtmp));
      fprintf(FD,"\n");
    }
}

/* =======================================================================
   RID_set_print
   print information about a region's boundary sets
   recursive
   =======================================================================*/
void RID_set_print(FILE *FD, RID *rid)
{
  Is_True(rid != NULL,("RID_set_print, inconsistency in RID"));

  fprintf(FD,
	  "===== RID_set_print(%s %d), num_exits %d, has_return=%c, "
	  "bounds_defined=%c, parent_block=0x%p\n",
	  RID_TYPE_func_entry(rid) ? "PU" : "RGN", RID_id(rid),
	  RID_num_exits(rid), tf(RID_has_return(rid)),
	  tf(RID_bounds_exist(rid)), RID_parent_block(rid));
  fprintf(FD, "  aliased_to_globals=%c, aliased_to_indirects=%c, "
	  "contains_uplevel=%c\n  contains_bounds=%c, contains_barrier=%c\n",
	  tf(RID_aliased_to_globals(rid)), tf(RID_aliased_to_indirects(rid)),
	  tf(RID_contains_uplevel(rid)), tf(RID_contains_bounds(rid)),
	  tf(RID_contains_barrier(rid)));

  /* pregs_in */
  fprintf(FD,"  pregs_in:\n");
  Dump_preg_list(FD, RID_pregs_in(rid));

  /* pregs_out */
  if (RID_pregs_out(rid) != NULL) {
    INT32 nexits;
    for (nexits = 0; nexits < RID_num_exits(rid); nexits++) {
      fprintf(FD,"  pregs_out(exit %d):\n",nexits);
      Dump_preg_list(FD, RID_pregs_out(rid)[nexits]);
    }
  }

  /* pregs_quad */
  if (RID_pregs_quad(rid) != NULL) {
    fprintf(FD,"  pregs_quad:\n");
    Dump_preg_list(FD, RID_pregs_quad(rid));
  }

  /* pregs_complex_quad */
  if (RID_pregs_complex_quad(rid) != NULL) {
    fprintf(FD,"  pregs_complex_quad:\n");
    Dump_preg_list(FD, RID_pregs_complex_quad(rid));
  }

  /* used_in */
  Dump_points_to_list(FD, RID_used_in(rid), "used_in:");

  /* def_in_live_out */
  Dump_points_to_list(FD, RID_def_in_live_out(rid), "def_in_live_out:");

  /* dump kids also */
  if (RID_first_kid(rid) != NULL) {
    RID *rtmp;
    for (rtmp=RID_first_kid(rid); rtmp; rtmp=RID_next(rtmp))
      RID_set_print(FD,rtmp);
  }
}

// =======================================================================
// RID_Fprint
// do not call REGION_consistency check here, this dump routine is called
// by WOPT to print out RIDs after their WHIRL has been destroyed.
// =======================================================================
void RID_Fprint(FILE *FD, RID *rid)
{
  INT id, depth;
  char *level, *type;
  BOOL bound, ret;
  SRCPOS s;
  USRCPOS us;
  UINT32 flags;
  RID *kid;
  WN *rwn;

  id	= RID_id(rid);
  depth	= RID_depth(rid);
  level	= RID_level_str(rid);
  type	= RID_type_str(RID_type(rid));
  bound	= RID_bounds_exist(rid);
  ret	= RID_has_return(rid);
  flags	= RID_flags(rid);
  s	= RID_srcpos(rid);
  USRCPOS_srcpos(us) = s;

  // first check if rwn has same rid id
  rwn = RID_rwn(rid);
  if (rwn != NULL && WN_region_id(rwn) != id)
    fprintf(TFile,"**** RID_Fprint: RID_id = %d, WN_region_id = %d\n",
	    id, WN_region_id(rwn));

  // print general RID information
  fprintf(FD, "RID %d: loop depth %d, processed to %s, linenum %d,\n",
	  id, depth, level, USRCPOS_linenum(us));
  fprintf(FD, "  exits %d, flags 0x%x, type%s, parent RID %d\n",
	  RID_num_exits(rid), flags, type, 
	  RID_parent(rid) ? RID_id(RID_parent(rid)) : -1);
  fprintf(FD, "  parent_block=0x%p, cginfo=0x%p, has_return=%c, "
	  "bounds_defined=%c\n", RID_parent_block(rid),
	  RID_cginfo(rid), tf(ret), tf(bound == REGION_BOUND_EXISTS));
  fprintf(FD, "  aliased_to_globals=%c, aliased_to_indirects=%c, "
	  "contains_uplevel=%c\n  contains_bounds=%c, contains_barrier=%c\n",
	  tf(RID_aliased_to_globals(rid)), tf(RID_aliased_to_indirects(rid)),
	  tf(RID_contains_uplevel(rid)), tf(RID_contains_bounds(rid)),
	  tf(RID_contains_barrier(rid)));

  // per region options
  if (RID_options(rid) != NULL)
    fprintf(FD, "  options=%s\n", RID_options(rid));

  // print region exits
  if (!RID_TYPE_func_entry(rid) && RID_rwn(rid)) {
    INT32 count = REGION_count_exits(WN_region_exits(RID_rwn(rid)));
    Dump_region_exits(FD, WN_region_exits(RID_rwn(rid)));
    Is_True(count == RID_num_exits(rid),
	    ("# exits mismatch in RGN%d: num_exits=%d, actual=%d",
	     RID_id(rid), RID_num_exits(rid), count));
  }

  // print kid pointers
  for (kid=RID_first_kid(rid); kid; kid=RID_next(kid)) {
    if (kid == RID_first_kid(rid))
      fprintf(FD,"  kids: ");
    fprintf(FD,"RID%d ",RID_id(kid));
    if (RID_next(kid) == NULL)
      fprintf(FD,"\n");
    if (RID_lowered(rid))
      RID_lower_Fprint(FD, RID_lowered(rid));
  }
  if (RID_next(rid) != NULL) {
    fprintf(FD,"  next: RID%d\n",RID_id(RID_next(rid)));
  }
}

/* =======================================================================
   RID_Tree_Print
   =======================================================================*/
void RID_Tree_Print(FILE *FD, RID *rid)
{
  if (rid) {
    RID *kid;
    RID_Fprint(FD,rid);
    for (kid=RID_first_kid(rid); kid; kid=RID_next(kid))
      RID_Tree_Print(FD,kid);
  }
}

/* =======================================================================
   RID_is_valid: to check whether rid is valid in the rid tree
   ====================================================================*/
bool RID_is_valid(RID *parent, RID *rid)
{
    if (parent) {
        if ( parent == rid) return true;
        for (RID *kid = RID_first_kid(parent); kid; kid=RID_next(kid))
            if (RID_is_valid(kid, rid))
                return true;
    }        
    return false;
}

/* ======================================================================
   RID_WN_Tree_Print
   ====================================================================== */
void RID_WN_Tree_Print(FILE *FD, WN *tree)
{
  RID *rid = REGION_get_rid(tree);
  RID_Tree_Print(FD, rid);
}

// ======================================================================
// routines below this line are used in be driver only
// (region_initialize is in region_init.cxx)
// ======================================================================

// =======================================================================
// update_rid_parent_block
// recursive WHIRL traversal to update RID_parent_block
// used only before the main region iterator
// NOTE: depends on `block' parameter only being non-NULL when
//       traversing the statements immediately below a block.
// =======================================================================
static void update_parent_block(WN *block, WN *wn)
{
  RID *rid;
  WN *wtmp;
  INT i;

  switch (WN_opcode(wn)) {

    case OPC_BLOCK:
      for (wtmp=WN_first(wn); wtmp; wtmp=WN_next(wtmp))
	update_parent_block(wn, wtmp);
      break;

    case OPC_REGION:
      Is_True(block != NULL, ("update_parent_block, block is NULL"));
      rid = REGION_get_rid(wn);
      Is_True(rid != NULL, ("update_parent_block, NULL rid"));
      RID_parent_block(rid) = block; // update parent_block ptr
      // go down only if there are regions down there
      if (RID_first_kid(rid) != NULL) {
	for (wtmp=WN_first(WN_region_body(wn)); wtmp; wtmp=WN_next(wtmp))
	  update_parent_block(WN_region_body(wn), wtmp);
      }
      break;

    case OPC_FUNC_ENTRY:
      Is_True (PU_has_region (Get_Current_PU ()),
	      ("update_parent_block, no regions"));
      Is_True(block == NULL, ("update_parent_block, block is not NULL"));
      rid = REGION_get_rid(wn);
      Is_True(rid != NULL, ("update_parent_block, NULL rid"));
      RID_parent_block(rid) = NULL; // there is no parent for the PU
      // go down only if there are regions down there
      if (RID_first_kid(rid) != NULL) {
	for (wtmp=WN_first(WN_func_body(wn)); wtmp; wtmp=WN_next(wtmp))
	  update_parent_block(WN_func_body(wn), wtmp);
      }
      break;

    default:
      // pass in NULL for block because a region can only be under a block
      // this code is to traverse all the structured control flow
      for (i=0; i<WN_kid_count(wn); i++)
	update_parent_block(NULL, WN_kid(wn,i));
      break;
  }
}

// =======================================================================
// REGION_CS_ITER_init
// =======================================================================
void REGION_CS_ITER_init(REGION_CS_ITER *iter, WN *pu)
{
  INT32 i;

  REGION_CS_ITER_me(iter) = NULL;
  REGION_CS_ITER_kid(iter) = NULL;
  REGION_CS_ITER_type(iter) = RID_TYPE_undefined;
  REGION_CS_ITER_is_pu(iter) = FALSE;
  REGION_CS_ITER_is_not_stacked(iter) = FALSE;
  REGION_CS_ITER_sp(iter) = 0;
  for (i=0; i<REGION_STACK_SIZE; i++)
    REGION_CS_ITER_marker(iter,i) = NULL;

  // Traverse tree and make sure RID_parent_block is up to date.
  // This is so we can put what comes back from CG into the WHIRL tree.
  if (PU_has_region (Get_Current_PU ()))
    update_parent_block(NULL, pu);
}

// =======================================================================
// REGION_CS_NoEarlierSub_While
// At the end of the tree traversal, we are at the root,
// so `me' is the parent of root, which is NULL.
// =======================================================================
BOOL REGION_CS_NoEarlierSub_While(REGION_CS_ITER *iter)
{
  return (!REGION_CS_ITER_is_not_stacked(iter) &&
    	  REGION_CS_ITER_kid(iter) != NULL);
}

// =======================================================================
// REGION_CS_Next
// Local routine to recursively find the next RID to process.
// This is tricky.
// =======================================================================
static void
REGION_CS_Next(REGION_CS_ITER *iter)
{
  RID *me  = REGION_CS_ITER_me(iter);
  RID *kid = REGION_CS_ITER_kid(iter);
  RID_TYPE type = REGION_CS_ITER_type(iter);
  RID *parent = (me != NULL) ? RID_parent(me) : NULL;
  Is_True(type & RID_TYPE_loop || type & RID_TYPE_pragma ||
#ifdef TARG_SL //region_type_for_major
	  type & RID_TYPE_olimit || type & RID_TYPE_func_entry || type & RID_TYPE_major || 
#else 
	  type & RID_TYPE_olimit || type & RID_TYPE_func_entry ||
#endif 	  
	  type & RID_TYPE_eh,
	  ("REGION_CS_Next, unknown region type, 0x%x",type));

  if (me == NULL) { // completely done
    REGION_CS_ITER_kid(iter) = NULL;
    return;
  }

  if (kid == NULL) {
    if (RID_first_kid(me) == NULL) {	// leaf node
      REGION_CS_ITER_kid(iter) = me;
      REGION_CS_ITER_me(iter) = parent;
    } else {				// not a leaf, initialization
      REGION_CS_ITER_kid(iter) = NULL;
      REGION_CS_ITER_me(iter) = RID_first_kid(me);
      REGION_CS_Next(iter);
    }
  } else {
    if (RID_next(kid)) {		// next kid
      if (RID_first_kid(RID_next(kid))) {
	REGION_CS_ITER_kid(iter) = NULL;
	REGION_CS_ITER_me(iter) = RID_next(kid);
	REGION_CS_Next(iter);
      } else
	REGION_CS_ITER_kid(iter) = RID_next(kid);
    } else if (RID_is_glue_code(RID_parent(kid))
	&& RID_next(RID_parent(kid)) != NULL) 
    {
	// glue code causes next to be sibling of glue-code
	REGION_CS_ITER_kid(iter) = RID_parent(kid);
	REGION_CS_Next(iter);
    } else {				// done with kids, go up
      REGION_CS_ITER_kid(iter) = me;
      REGION_CS_ITER_me(iter) = parent;
    }
  }
}

// =======================================================================
// REGION_CS_NoEarlierSub_Next
// Iterator routine to find next RID of a given type to process.
// =======================================================================
void REGION_CS_NoEarlierSub_Next(REGION_CS_ITER *iter)
{
  BOOL done;

  // if type of the RID we found isn't what we are looking for, move to next
  do {
    done = TRUE;
    REGION_CS_Next(iter); // go to next RID no matter the type
    if (REGION_CS_ITER_kid(iter) != NULL) { // if is is NULL we are done
      RID *rid;
      rid = REGION_CS_ITER_kid(iter);

      Is_True(RID_type(rid) != RID_TYPE_undefined,
	      ("REGION_CS_NoEarlierSub_Next, undefined RID type"));
#ifdef Is_True_On
      { // do all sorts of consistency checks
	WN *rwn;
	RID *rid2;
	rwn = REGION_CS_ITER_wn(iter);
	Is_True(rwn != NULL, ("REGION_CS_NoEarlierSub_Next, NULL rwn"));
	rid2 = REGION_get_rid(rwn);
	Is_True(rid2 != NULL, ("REGION_CS_NoEarlierSub_Next, NULL rid"));
	Is_True(REGION_consistency_check(rwn), (""));
	Is_True(rid2 == rid,
		("REGION_CS_NoEarlierSub_Next, RIDs do not match"));
	Is_True(RID_type(rid2) != RID_TYPE_undefined,
		("REGION_CS_NoEarlierSub_Next, undefined RID type"));
      }
#endif      
      if (!(RID_type(rid) & REGION_CS_ITER_type(iter) ||
	    RID_type(rid) & RID_TYPE_func_entry))
	done = FALSE;
    }
  } while (!done);
}

// =======================================================================
// REGION_CS_NoEarlierSub_First
// This routine works for a func_entry with no regions inside the function,
// a func_entry with regions inside the function, or a region.
// =======================================================================
void REGION_CS_NoEarlierSub_First(REGION_CS_ITER *iter, WN *tree,
				  RID_TYPE type)
{
  RID *my_rid;

  my_rid = (RID*)WN_MAP_Get(RID_map, tree); // always there even if no regions
  Is_True(my_rid != NULL, ("REGION_CS_NoEarlierSub_First, can't find region"));

  REGION_CS_ITER_me(iter) = my_rid;
  REGION_CS_ITER_kid(iter) = (RID *)NULL;
  REGION_CS_ITER_type(iter) = type; // more than one type can be set
  // the type tells what driver loop we are doing:
  // loop/func_entry: regions introduced by RAIL, loop around MainOpt/CG
  // pragma/olimit/func_entry: regions intorduced by user pragmas or olimit
  // 			heuristic, loop around LNO/MainOpt/CG

  REGION_CS_NoEarlierSub_Next(iter);
}

// =======================================================================
// REGION_CS_print() - dump an iterator's state
// =======================================================================
void REGION_CS_print(REGION_CS_ITER *iter)
{
#ifdef Is_True_On
  fprintf(TFile, "me = RGN %d\n", iter->me ? RID_id(iter->me) : -1);
  fprintf(TFile, "kid = RGN %d\n", iter->kid ? RID_id(iter->kid) : -1);
  fprintf(TFile, "type = %s\n", RID_type_str(iter->type));
  fprintf(TFile, "parent_block = 0x%p\n", iter->parent_block);
  fprintf(TFile, "is_pu = %c\n", tf(iter->is_pu));
  fprintf(TFile, "is_not_stacked = %c\n", tf(iter->is_not_stacked));
#endif
}

// =======================================================================
// REGION_remove_and_mark
// Remove the given region from the PU and return a pointer to it.
// What really happens is a new region node is created and the children
// of the old region node copied over to it. The old region node is the
// marker for REGION_replace_from_mark(). A stack of these is maintained
// so that regions can be removed and replaced from regions. For the PU
// case, nothing is done except set the is_pu and is_not_stacked bits so
// we know what to do in REGION_replace_from_mark.
// =======================================================================
WN *REGION_remove_and_mark(WN *pu, REGION_CS_ITER *iter)
{
  WN *new_rwn, *rwn;

  rwn = REGION_CS_ITER_wn(iter);
  // do nothing if rwn is the whole pu
  if (pu == rwn || rwn == NULL || WN_opcode(rwn) == OPC_FUNC_ENTRY) {
    if (WN_opcode(pu) == OPC_FUNC_ENTRY)
      REGION_CS_ITER_is_pu(iter) = TRUE;
    REGION_CS_ITER_is_not_stacked(iter) = TRUE;
    return pu;
  }

  Is_True(REGION_consistency_check(pu), (""));
  Is_True(REGION_consistency_check(rwn), (""));
  Is_True(WN_opcode(rwn) == OPC_REGION,
	  ("REGION_remove_and_mark, can't find region"));

  // save the parent_block pointer
  Is_True(RID_parent_block(REGION_get_rid(rwn)),
	  ("REGION_remove_and_mark, parent_block not set"));
  REGION_CS_ITER_parent_block(iter) = RID_parent_block(REGION_get_rid(rwn));
  // clear out parent_block pointer while it is being processed
  RID_parent_block(REGION_get_rid(rwn)) = NULL;

  // create a new region node and move kids over
  new_rwn = WN_CopyNode(rwn);
#if defined(TARG_SL)
  WN_CopyMap(new_rwn,WN_MAP_FEEDBACK,rwn);
#endif
  WN_region_exits(new_rwn) = WN_region_exits(rwn);
  WN_region_pragmas(new_rwn) = WN_region_pragmas(rwn);
  WN_region_body(new_rwn) = WN_region_body(rwn);
  // transfer RID to marker
  REGION_new_wn(new_rwn, rwn);

  // mark old region node, push onto marker stack
  REGION_CS_ITER_marker(iter,REGION_CS_ITER_sp(iter)++) = rwn;
  FmtAssert(REGION_CS_ITER_sp(iter) < REGION_STACK_SIZE,
	    ("REGION_remove_and_mark, region stack overflow"));

  // clean up dangling pointers and put in a comment in case
  // anyone prints the tree and wants to know where the region went
  {
    WN *new_cmt, *new_blk = WN_CreateBlock();
    char str[100];
    sprintf(str,"REGION %d currently being processed",
	    RID_id(REGION_get_rid(rwn)));
    new_cmt = WN_CreateComment(str);
    WN_INSERT_BlockBefore(new_blk,NULL,new_cmt);
    WN_region_body(rwn) = new_blk;	    
    WN_region_exits(rwn) = WN_CreateBlock();
    WN_region_pragmas(rwn) = WN_CreateBlock();
  }

  Is_True(REGION_consistency_check(pu),(""));
  Is_True(REGION_consistency_check(new_rwn),(""));
  if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
    fprintf(TFile,"===== REGION_remove_and_mark RGN %d\n",
	    RID_id(REGION_get_rid(new_rwn)));
  }

  WN_verifier(rwn);
  WN_verifier(new_rwn);
  return new_rwn;
}

// =======================================================================
// REGION_replace_from_mark
// rwn may be a region or a block
// =======================================================================
void REGION_replace_from_mark(WN *rwn, REGION_CS_ITER *iter)
{
  WN *r_marker;

  // if it is not stacked then no need to replace, if the phase that
  // processed this didn't return anything, then don't replace either
  if (REGION_CS_ITER_is_not_stacked(iter) || rwn == NULL)
    return;

  --REGION_CS_ITER_sp(iter);
  FmtAssert(REGION_CS_ITER_sp(iter) >= 0,
	    ("REGION_replace_from_mark, region stack underflow"));
  r_marker = REGION_CS_ITER_marker(iter, REGION_CS_ITER_sp(iter));
  REGION_CS_ITER_marker(iter,REGION_CS_ITER_sp(iter)) = NULL;

  // CG can return a block, others return a region so handle either case
  if (WN_operator(rwn) == OPR_BLOCK) {
    WN *wtmp;
    BOOL found = FALSE;
    WN *parent_block = REGION_CS_ITER_parent_block(iter);
    Is_True(parent_block != NULL,
	    ("REGION_replace_from_mark, parent_block not set"));
    Is_True(WN_opcode(r_marker) == OPC_REGION,
	    ("REGION_replace_from_mark, marker is wrong"));

#ifdef Is_True_On
    // verify the region is still within the parent block
    for (wtmp=WN_first(parent_block); wtmp!=NULL && !found; wtmp=WN_next(wtmp))
      found = (wtmp == r_marker);
    Is_True(found, ("REGION_replace_from_mark, marker not found"));
#endif

    // insert contents of new block after r_marker
    WN_INSERT_BlockAfter(parent_block, r_marker, rwn);

    // undo the DAG part of the marker so we can delete it
    WN_DELETE_Tree(WN_region_body(r_marker));
    WN_region_exits(r_marker)   = NULL;
    WN_region_pragmas(r_marker) = NULL;
    WN_region_body(r_marker)    = NULL;

    // delete r_marker from the PU
    WN_DELETE_FromBlock(parent_block, r_marker);
  } else { // OPC_REGION

    Is_True(WN_operator(rwn) == OPR_REGION,
	  ("REGION_replace_from_mark, not a region"));
    Is_True(REGION_consistency_check(rwn), (""));

    // transfer kids to mark
    WN_region_exits(r_marker)   = WN_region_exits(rwn);
    WN_region_pragmas(r_marker) = WN_region_pragmas(rwn);
    WN_region_body(r_marker)    = WN_region_body(rwn);

    // erase kids so there is no mistake
    WN_region_exits(rwn) = NULL;
    WN_region_pragmas(rwn) = NULL;
    WN_region_body(rwn) = NULL;

    // transfer kind and region_id
    WN_set_region_kind(r_marker, WN_region_kind(rwn));
    WN_set_region_id(r_marker, WN_region_id(rwn));

    // transfer RID
    REGION_new_wn(r_marker, rwn);

    Is_True(REGION_consistency_check(r_marker), (""));
    if (Get_Trace(TP_REGION, TT_REGION_ALL)) {
      fprintf(TFile,"===== REGION_replace_from_mark RGN %d, stacked=1\n",
	      RID_id(REGION_get_rid(r_marker)));
    }
  } // if (WN_operator(rwn) == OPR_BLOCK) else clause
}

// =======================================================================
// REGION_get_options_string
// search for the options pragma in a PU or region, return the options
// =======================================================================
char *REGION_get_options_string(WN *wn)
{
  WN *wtmp;
  Is_True(WN_opcode(wn) == OPC_REGION || WN_opcode(wn) == OPC_FUNC_ENTRY,
	  ("REGION_get_options_string, unexpected WN"));

  // first find the options pragma
  wtmp = REGION_search_block(
   (WN_opcode(wn) == OPC_REGION) ? WN_region_pragmas(wn) : WN_func_pragmas(wn),
			     comp_same_pragma(WN_PRAGMA_OPTIONS));
  // no options pragma found
  if (wtmp == NULL)
    return NULL;

  // return the options string
  ST *st = WN_st(wtmp);
  Is_True(st != NULL, ("REGION_get_options_string, NULL st"));
  Is_True (ST_tcon(st) != 0, ("REGION_get_options_string, NULL tcon"));
  TCON *tc = &Tcon_Table[ST_tcon (st)];
  char *s = Targ_String_Address(*tc);
  Is_True(s != NULL, ("REGION_get_options_string, NULL string"));
  Is_True(strlen(s) == Targ_String_Length(*tc),
	  ("REGION_get_options_string, wrong string length"));
  if (*s == '\0') // if string is empty, set to NULL
    s = NULL;
  return s;
}

// =======================================================================
// =======================================================================

// REGION_KIND_EH is a mask
BOOL REGION_is_EH(WN * wn)
{
#ifdef Is_True_On
  if (WN_region_kind(wn) & REGION_KIND_EH) {
    RID *rid = REGION_get_rid(wn);
    if (rid) /* rid may not be set yet */
      Is_True(RID_TYPE_eh(rid) &&
	      (RID_TYPE_try(rid) || RID_TYPE_cleanup(rid) ||
	       RID_TYPE_exc_spec(rid) || RID_TYPE_mask(rid) ||
	       RID_TYPE_guard(rid) || RID_TYPE_null_cleanup(rid)),
	      ("REGION_is_EH, region type/kind inconsistency"));
  }
#endif
  return WN_region_kind(wn) & REGION_KIND_EH;
}

/* REGION_KIND_MP is not a mask */
BOOL REGION_is_mp(WN * wn)
{
#ifdef Is_True_On
  if (WN_region_kind(wn) == REGION_KIND_MP) {
    RID *rid = REGION_get_rid(wn);
    if (rid) /* rid may not be set yet */
      Is_True(RID_TYPE_mp(rid),
	      ("REGION_is_mp, region type/kind inconsistency"));
  }
#endif
  return WN_region_kind(wn) == REGION_KIND_MP;
}

#if defined(TARG_SL)
/* REGION_KIND_MP is not a mask */
BOOL REGION_is_sl2_para(WN * wn)
{
  if (WN_region_kind(wn) == REGION_KIND_MAJOR || 
       WN_region_kind(wn) == REGION_KIND_MINOR) {

    RID *rid = REGION_get_rid(wn);
    if (rid) /* rid may not be set yet */
      Is_True(RID_TYPE_sl2_para(rid),
	      ("REGION_is_sl2_para, region type/kind inconsistency"));
  }
  return (WN_region_kind(wn) == REGION_KIND_MAJOR || 
               WN_region_kind(wn) == REGION_KIND_MINOR);
}
#endif 

REGION_KIND REGION_type_to_kind(RID *rid)
{
  Is_True(rid != NULL, ("REGION_type_to_kind, NULL RID"));
  
  if (RID_type(rid) & RID_TYPE_func_entry)
    return REGION_KIND_FUNC_ENTRY;
  if (RID_type(rid) & RID_TYPE_loop)
    return REGION_KIND_LOOP;
  if (RID_type(rid) & RID_TYPE_pragma)
    return REGION_KIND_PRAGMA;
  if (RID_type(rid) & RID_TYPE_olimit)
    return REGION_KIND_OLIMIT;
  if (RID_type(rid) & RID_TYPE_mp)
    return REGION_KIND_MP;
  if (RID_type(rid) & RID_TYPE_rpi)
    return REGION_KIND_RPI;
  if (RID_type(rid) & RID_TYPE_cold)
    return REGION_KIND_COLD;
  if (RID_type(rid) & RID_TYPE_swp)
    return REGION_KIND_SWP;
#ifdef TARG_SL //fork_joint
  if (RID_type(rid) & RID_TYPE_major)
    return REGION_KIND_MAJOR;
  if(RID_type(rid)  & RID_TYPE_minor)
    return REGION_KIND_MINOR;
#endif 
  if (RID_type(rid) & RID_TYPE_try)
    return REGION_KIND_TRY;
  if (RID_type(rid) & RID_TYPE_cleanup)
    return REGION_KIND_CLEANUP;
  if (RID_type(rid) & RID_TYPE_exc_spec)
    return REGION_KIND_EXC_SPEC;
  if (RID_type(rid) & RID_TYPE_mask)
    return REGION_KIND_MASK;
  if (RID_type(rid) & RID_TYPE_guard)
    return REGION_KIND_GUARD;
  if (RID_type(rid) & RID_TYPE_null_cleanup)
    return REGION_KIND_NULL_CLEANUP;
  Is_True(FALSE,("REGION_type_to_kind, unknown RID type"));
  return REGION_KIND_PRAGMA; /* to satisfy compiler */
}

/* looks at the REGION_KIND in wn and sets the RID_TYPE in rid */
void REGION_kind_to_type(WN *wn, RID *rid)
{
  if (REGION_is_EH(wn)) {
    switch (WN_region_kind(wn)) {
      case REGION_KIND_TRY:
        RID_TYPE_try_Set(rid);
	break;
      case REGION_KIND_CLEANUP:
	RID_TYPE_cleanup_Set(rid);
	break;
      case REGION_KIND_EXC_SPEC:
	RID_TYPE_exc_spec_Set(rid);
	break;
      case REGION_KIND_MASK:
	RID_TYPE_mask_Set(rid);
	break;
      case REGION_KIND_GUARD:
	RID_TYPE_guard_Set(rid);
	break;
      case REGION_KIND_NULL_CLEANUP:
	RID_TYPE_null_cleanup_Set(rid);
	break;
      default:
	Is_True(FALSE,("REGION_kind_to_type, unknown kind"));
	break;
      }
  } else { /* not EH */
    switch (WN_region_kind(wn)) {
      case REGION_KIND_PRAGMA:
	RID_TYPE_pragma_Set(rid);
	break;
      case REGION_KIND_FUNC_ENTRY:
	RID_TYPE_pragma_Set(rid);
	break;
      case REGION_KIND_LOOP:
	RID_TYPE_loop_Set(rid);
	break;
      case REGION_KIND_OLIMIT:
	RID_TYPE_olimit_Set(rid);
	break;
      case REGION_KIND_MP:
        Is_True(REGION_is_mp(wn), ("REGION_kind_to_type, internal error"));
	RID_TYPE_mp_Set(rid);
	break;
      case REGION_KIND_RPI:
	RID_TYPE_rpi_Set(rid);
	break;
      case REGION_KIND_COLD:
	RID_TYPE_cold_Set(rid);
	break;
      case REGION_KIND_SWP:
	RID_TYPE_swp_Set(rid);
	break;
#ifdef TARG_SL //fork_joint
      case REGION_KIND_MINOR:
	RID_TYPE_minor_Set(rid);
	break;
	case REGION_KIND_MAJOR:
	RID_TYPE_major_Set(rid);
	break;
#endif 
	
      default:
	Is_True(FALSE,("REGION_kind_to_type, unknown kind"));
	break;
    }
  }
}

/* =======================================================================
 Is it a fake call from exception handling block? Need to ignore it
 The parentization must have been done for this code to work 
    =======================================================================*/
#define LWN_Get_Parent_From_Map(wn,Parent_Map) \
		((WN*)WN_MAP_Get(Parent_Map, (WN*)wn))

BOOL WN_Fake_Call_EH_Region(WN * wn, WN_MAP Parent_Map) { 
/* ignore fake call from exception handling block */
  WN * parent;
  WN * grandparent;

  FmtAssert(WN_opcode(wn) == OPC_VCALL,("In EH pragma, not a call"));
  parent = LWN_Get_Parent_From_Map(wn,Parent_Map);
  if (parent && (WN_operator(parent) == OPR_BLOCK)) {
    grandparent = LWN_Get_Parent_From_Map(parent,Parent_Map);
    if (grandparent && 
	(WN_operator(grandparent) == OPR_REGION) &&
        (REGION_is_EH(grandparent)) &&
        (WN_region_pragmas(grandparent) == parent))
      return TRUE;
  }
  return FALSE;
}
