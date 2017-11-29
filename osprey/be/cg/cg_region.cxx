/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* Utilities for creating, deleting, modifying CG info for REGIONs */

#include <ext/slist>
#include <algorithm>
#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "cgir.h"
#include "opcode.h"
#include "wn_util.h"
#include "wn_map.h"
#include "tn_map.h"
#include "whirl2ops.h"
#include "region_util.h"
#include "cg_region.h"
#include "annotations.h"
#include "cg_flags.h"
#include "cg_internal.h"
#include "ir_reader.h"		// for fdump_tree()
#include "label_util.h"
#include "region_whirl_templates.h"	// REGION_search_block
#include "strtab.h"		// Save_Str
#include "targ_sim.h"
#if defined(TARG_SL) && defined(TARG_SL2)
#include "fb_whirl.h"
#endif

/* =======================================================================
 *
 *  CGRIN_Create
 *
 *  See interface description.
 *
 * =======================================================================
 */
CGRIN *CGRIN_Create( INT num_exits )
{
  CGRIN *cgrin;
  BB **exits;
  LABEL_IDX *exit_labels;
  WN *entry_glue, **exit_glue;
  INT i;

  cgrin = TYPE_MEM_POOL_ALLOC( CGRIN, &REGION_mem_pool );

  if (num_exits > 0) {
    exits = TYPE_MEM_POOL_ALLOC_N( BB *, &REGION_mem_pool, num_exits );
    CGRIN_exits( cgrin ) = exits;
  } else
    CGRIN_exits( cgrin ) = NULL; // PU does not have any region exits

  exit_labels = TYPE_MEM_POOL_ALLOC_N( LABEL_IDX, &REGION_mem_pool, num_exits );
  CGRIN_exit_labels( cgrin ) = exit_labels;

  entry_glue = WN_CreateBlock();
  CGRIN_entry_glue( cgrin ) = entry_glue;
  exit_glue = TYPE_MEM_POOL_ALLOC_N( WN *, &REGION_mem_pool, num_exits );
  CGRIN_exit_glue( cgrin ) = exit_glue;

  for ( i = 0; i < num_exits; i++ )
    CGRIN_exit_glue_i( cgrin, i ) = WN_CreateBlock();

  CGRIN_nested_exit(cgrin) = WN_CreateBlock();
  
  return cgrin;
}

// use begin_pregtn ops to get in-list of tns
TN_LIST *
REGION_Get_TN_In_List (RID *rid)
{
	BB *bb = CGRIN_entry( RID_cginfo( rid ));
	OP *op;
  	TN_LIST *tnl = NULL;
	TN *tn;
    	if (CG_localize_tns) DevWarn("REGION_Get_TN_In_List NYI when localized");
	FOR_ALL_BB_OPs (bb, op) {
      		if (OP_code(op) != TOP_begin_pregtn) continue;
	  	tn = OP_opnd(op,0);
	  	// add tn to tns_in list so outer region can find it
	  	// when glue code is lowered and we want to reuse tn
	  	tnl = TN_LIST_Push( tn, tnl, &REGION_mem_pool);
	}
	return tnl;
}

// use end_pregtn ops to get out-list of tns
TN_LIST *
REGION_Get_TN_Out_List (RID *rid, INT exit_num)
{
	BB *bb = CGRIN_exit_i( RID_cginfo( rid ), exit_num);
	OP *op;
  	TN_LIST *tnl = NULL;
	TN *tn;
    	if (CG_localize_tns) DevWarn("REGION_Get_TN_Out_List NYI when localized");
	FOR_ALL_BB_OPs (bb, op) {
      		if (OP_code(op) != TOP_end_pregtn) continue;
	  	tn = OP_opnd(op,0);
	  	// add tn to tns_in list so outer region can find it
	  	// when glue code is lowered and we want to reuse tn
	  	tnl = TN_LIST_Push( tn, tnl, &REGION_mem_pool);
	}
	return tnl;
}

//============================================================================
// verify_pregs_list (Is_True_On only)
// Looks through a preg list and verifies that the pregs are in the right
// order (for quads and complex quads).
// Gen_quad_preg relies on the pregs_tns to be in order so check it
// "in-order" means the remaining pregs after the first one come after.
//============================================================================
#ifdef Is_True_On
static
void verify_pregs_list(RID *rid, PREG_LIST *prl)
{
  PREG_NUM pr;

  // Gen_quad_preg relies on the pregs_in list to be in order so check it
  // "in-order" means the remaining pregs after the first one come after
  for ( ; prl; prl = PREG_LIST_rest(prl)) {
    pr = PREG_LIST_first(prl);
    if (pr <= Last_Dedicated_Preg_Offset)
      continue;
    if (REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
      Is_True(pr + 1 == PREG_LIST_first(PREG_LIST_rest(prl)),
	      ("verify_pregs_list(RGN %d), pregs_in list out "
	       "of order", RID_id(rid)));
    } else if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
      PREG_LIST *tmp = PREG_LIST_rest(prl);
      Is_True(pr + 1 == PREG_LIST_first(tmp),
	      ("verify_pregs_list(RGN %d), pregs_in list out "
	       "of order", RID_id(rid)));
      tmp = PREG_LIST_rest(tmp);
      Is_True(pr + 2 == PREG_LIST_first(tmp),
	      ("verify_pregs_list(RGN %d), pregs_in list out "
	       "of order", RID_id(rid)));
      tmp = PREG_LIST_rest(tmp);
      Is_True(pr + 3 == PREG_LIST_first(tmp),
	      ("verify_pregs_list(RGN %d), pregs_in list out "
	       "of order", RID_id(rid)));
    }
  }
}
#endif

//============================================================================
// verify_pregtn_order (Is_True_On only)
// Looks through a BB at the begin_pregtns and end_pregtns and verifies
// they are in the right order (for quads and complex quads).
// Gen_quad_preg relies on the pregs_tns to be in order so check it
// "in-order" means the remaining pregs after the first one come after.
//============================================================================
#ifdef Is_True_On
#define verify_macro(preg_n)						\
	tmp = OP_next(op);						\
	while (tmp != NULL && OP_code(tmp) != opc)			\
	  tmp = OP_next(tmp);						\
	Is_True(tmp != NULL && OP_code(tmp) == opc,			\
		("verify_pregtn_order, preg_tns are mixed"));		\
	Is_True((preg_n) == TN_value(OP_opnd(tmp, 1)),			\
		("verify_pregtn_order(BB %d), preg_tns out of order",	\
		 BB_id(bb)));

static
void verify_pregtn_order(BB *bb, RID *rid, TOP opc)
{
  OP *op = BB_first_op(bb), *tmp;
  while (op != NULL) {
    if (OP_code(op) == opc) {
      PREG_NUM pr = TN_value(OP_opnd(op, 1));
      if (REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
	verify_macro(pr+1);
      } else if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
	verify_macro(pr+1);
	verify_macro(pr+2);
	verify_macro(pr+3);
      }
    }
    op = OP_next(op);
  }
}
#endif

//============================================================================
// Update_preg_to_tn_array
// At -O2, the PREG_To_TN_Array[pr] is not guaranteed to be current. The
// PREG <--> TN mapping is kept in the code in the form of begin_pregtns
// and end_pregtns. This routine opdates the array to make glue code
// generation easier.
//============================================================================
static inline
void Update_preg_to_tn_array(OP *op, TOP opc)
{
  Is_True(!CG_localize_tns,
	  ("Update_preg_to_tn_array, don't call this routine at -O0"));
  // search down for pregtns that match opc
  while (op != NULL) {
    if (OP_code(op) == opc) {
      TN *tn = OP_opnd(op, 0);
      PREG_NUM pr = TN_value(OP_opnd(op, 1));
      PREG_NUM ppr = TN_To_Assigned_PREG(tn);
      PREG_To_TN_Array[pr] = tn;
      PREG_To_TN_Array[ppr] = tn;
      Is_Trace(Get_Trace(TP_REGION, TT_REGION_CG_DEBUG) ||
	       Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
	       (TFile,"Update_preg_to_tn_array, updating PREG %d <-> TN %d\n",
		ppr, TN_number(tn)));
      Is_Trace(Get_Trace(TP_REGION, TT_REGION_CG_DEBUG) ||
	       Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
	       (TFile,"Update_preg_to_tn_array, updating PREG %d <-> TN %d\n",
		pr, TN_number(tn)));
      Is_Trace_cmd(Get_Trace(TP_REGION, TT_REGION_CG_DEBUG) ||
		   Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG),
		   Print_OP(op));
    }
    op = OP_next(op);
  }
}

//============================================================================
// Gen_quad_preg - code to generate temporary and correct sequence for
// quad and complex quad pregs live across a region boundary
//============================================================================
static ST *gen_quad;		// flags that say we have generated a
static ST *gen_complex_quad;	// temp name in the symbol table

// these statics need to be reset for every PU, called in CG_PU_Initialize
// this is extern "C" because of cg.h
extern "C" void Init_gen_quad_preg(void)
{
  gen_quad = NULL;
  gen_complex_quad = NULL;
}

//============================================================================
// Gen_quad_preg:
//============================================================================
// Parameters:
//   pr is the quad preg
//   tnl is the tn list in cgrin that we need to add the tns to
//   block is where to insert the glue, after is used for exit code
//   mtype is original type (FQ, CQ), type is what it was lowered to (F8)
//   in_glue tells whether we are generating glue code coming in or out
//   the rid and alias_manager are required to update the variable boundary
//	sets in the case of localized spilled locations
// 
// Note:
//   This does the tn push to the tnl list and the region boundary because
//   there are multiple objects that go on these lists for quads and complex.
//   It does not assume the caller of this routine will do any of the
//   additions to the lists.
//============================================================================
static
void Gen_quad_preg(PREG_NUM pr, TN_LIST **tnl, WN *block,
		   WN **after, TYPE_ID mtype, BOOL in_glue, RID *rid,
		   INT exit_num, ALIAS_MANAGER *am)
{
  Is_True(mtype == MTYPE_FQ || mtype == MTYPE_CQ,
	  ("Gen_quad_preg, unknown mtype"));
  if (CG_localize_tns)
    Is_True(rid != NULL && am != NULL, ("Gen_quad_preg, RID or AM is NULL"));
  else
    Is_True(rid != NULL, ("Gen_quad_preg, RID is NULL"));
#ifdef Is_True_On
  BOOL trace = Get_Trace(TP_REGION, TT_REGION_CG_DEBUG) || 
    Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG);
#endif
  ST *newst;
  TYPE_ID type = PREG_To_TN_Mtype[pr]; // type of the lowered parts
  Is_True(type == MTYPE_F8, ("Gen_quad_preg, type error, expecting F8, %s",
			     MTYPE_name(type)));

  // see if temporary name has been created, if not create it now.
  if ((gen_quad == NULL && mtype == MTYPE_FQ) ||
      (gen_complex_quad == NULL && mtype == MTYPE_CQ)) {
    // add newst to symbol table (name)
    newst = New_ST();
    ST_Init(newst, 
            (mtype == MTYPE_FQ) ? Save_Str("region_bound_quad")
                                : Save_Str("region_bound_complex_quad"),
            CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, MTYPE_To_TY(mtype));
    Set_ST_is_temp_var(newst);
    if (mtype == MTYPE_FQ)
      gen_quad = newst;
    else
      gen_complex_quad = newst;
    Is_Trace(trace, (TFile,"Gen_quad_preg, generated temp for quad\n"));
  } else {
    // use previously created name
    newst = (mtype == MTYPE_FQ) ? gen_quad : gen_complex_quad;
  }

  WN *load, *store;
  // load from quad temporary in 2 or 4 pieces, put in ded. pregs or memory
  INT N = (mtype == MTYPE_FQ) ? 2 : 4; // two or four parts
  if (in_glue) {
    // load from quad (or complex quad) preg, put in new quad temporary
    load = WN_LdidPreg(mtype, pr);
    store = WN_Stid(mtype, (WN_OFFSET)0, newst, ST_type(newst), load);
    WN_INSERT_BlockLast(block, store);

    for (INT i=0; i<N; i++) {
      TN *tn2 = PREG_To_TN_Array[pr+i]; // thanks to Update_preg_to_tn_array
      Is_True(tn2 != NULL, ("Gen_quad_preg, tn2 is NULL, PREG %d", pr+i));
      load = WN_Ldid(type, (WN_OFFSET)(i*8), newst, ST_type(newst));
      if (CG_localize_tns) {
	ST *spill_loc2 = TN_spill(tn2);
	Is_True(strncmp(ST_name(spill_loc2),"lcl_spill_temp",14) == 0,
		("Gen_quad_preg, not a spill loc"));
	store = WN_Stid(TY_mtype(ST_type(spill_loc2)), 0, spill_loc2,
			ST_type(spill_loc2), load);
	REGION_add_wn_points_to(&RID_used_in(rid), store, am);
	Is_Trace(trace, (TFile, "Gen_quad_preg(in glue), TN %d <-> PREG %d\n",
			 TN_number(tn2), pr+i));
      } else {
	ST *spill_loc2 = TN_spill(tn2);
	if (TN_is_rematerializable(tn2)) {
		DevWarn("region bounds has rematerializable tn; can ignore?");
		spill_loc2 = NULL;	// have whirl home, not ST
	}
	if (TN_is_gra_homeable(tn2)) {
		DevWarn("region bounds has home-able tn; should store to home, but just use reg for now");
		spill_loc2 = NULL;	// have whirl home, not ST
	}
	if (spill_loc2) {
		Is_True(strncmp(ST_name(spill_loc2),"gra_spill_temp",14) == 0,
			("Gen_quad_preg, not a spill loc"));
		store = WN_Stid(TY_mtype(ST_type(spill_loc2)), 0, spill_loc2,
			ST_type(spill_loc2), load);
		REGION_add_wn_points_to(&RID_used_in(rid), store, am);
		Is_Trace(trace, (TFile, "Gen_quad_preg(in glue), "
			"TN %d <-> PREG %d\n",
			TN_number(tn2), pr+i));
	}
	else {
		PREG_NUM ppr2 = TN_To_Assigned_PREG(tn2);
		store = WN_StidIntoPreg(type, ppr2, MTYPE_To_PREG(type), load);
		REGION_add_preg_in(rid, ppr2, MTYPE_V); // don't want to add ppr2+1
		Is_Trace(trace, (TFile, "Gen_quad_preg(in glue), "
			 "TN %d (assigned to PREG %d) <-> PREG %d\n",
			 TN_number(tn2), ppr2, pr+i));
	}
      }
      *tnl = TN_LIST_Push(tn2, *tnl, &REGION_mem_pool);
      WN_INSERT_BlockLast(block, store);
    }

  } else { // out glue
    // load from new quad temp, put in quad (or complex quad) preg
    for (INT i=0; i<N; i++) {
      TN *tn2 = PREG_To_TN_Array[pr+i]; // thanks to Update_preg_to_tn_array
      Is_True(tn2 != NULL, ("Gen_quad_preg, tn2 is NULL PREG %d", pr+i));
      if (CG_localize_tns) {
	ST *spill_loc2 = TN_spill(tn2);
	Is_True(strncmp(ST_name(spill_loc2),"lcl_spill_temp",14) == 0,
		("Gen_quad_preg, not a spill loc"));
	load = WN_Ldid(TY_mtype(ST_type(spill_loc2)), 0, spill_loc2,
		       ST_type(spill_loc2));
	store = WN_Stid(type, (WN_OFFSET)(i*8), newst, ST_type(newst), load);
	REGION_add_wn_points_to(&RID_def_in_live_out(rid), store, am);
	Is_Trace(trace, (TFile, "Gen_quad_preg(out glue), TN %d <-> PREG %d\n",
			 TN_number(tn2), pr+i));
      } else {
	ST *spill_loc2 = TN_spill(tn2);
	if (TN_is_rematerializable(tn2)) {
		DevWarn("region bounds has rematerializable tn; can ignore?");
		spill_loc2 = NULL;	// have whirl home, not ST
	}
	if (TN_is_gra_homeable(tn2)) {
		DevWarn("region bounds has home-able tn; should store to home, but just use reg for now");
		spill_loc2 = NULL;	// have whirl home, not ST
	}
	if (spill_loc2) {
		Is_True(strncmp(ST_name(spill_loc2),"gra_spill_temp",14) == 0,
			("Gen_quad_preg, not a spill loc"));
		load = WN_Ldid(TY_mtype(ST_type(spill_loc2)), 0, spill_loc2,
		       ST_type(spill_loc2));
		REGION_add_wn_points_to(&RID_def_in_live_out(rid), store, am);
		Is_Trace(trace, (TFile, "Gen_quad_preg(out glue), "
			"TN %d <-> PREG %d\n",
			TN_number(tn2), pr+i));
	}
	else {
		PREG_NUM ppr2 = TN_To_Assigned_PREG(tn2);
		load = WN_LdidPreg(type, ppr2);
		REGION_add_preg_out(rid, exit_num, ppr2, MTYPE_V); // don't add ppr2+1
		Is_Trace(trace, (TFile, "Gen_quad_preg(out glue), "
			 "TN %d (assigned to PREG %d) <-> PREG %d\n",
			 TN_number(tn2), ppr2, pr+i));
	}
	store = WN_Stid(type, (WN_OFFSET)(i*8), newst, ST_type(newst), load);
      }
      *tnl = TN_LIST_Push(tn2, *tnl, &REGION_mem_pool);
      WN_INSERT_BlockAfter(block, *after, store);
      *after = store;
    }

    // load from new quad temporary, put in quad (or complex quad) preg
    load = WN_Ldid(mtype, (WN_OFFSET)0, newst, ST_type(newst));
    store = WN_StidIntoPreg(mtype, pr, MTYPE_To_PREG(mtype), load);
    WN_INSERT_BlockAfter(block, *after, store);
    *after = store;
  }
}

// =======================================================================
//  REGION_Entry_PREG_Whirl
// =======================================================================
void
REGION_Entry_PREG_Whirl( RID *rid, WN *entry_whirl, TN_LIST *inlist,
			struct ALIAS_MANAGER *am)
{
  TN_LIST *tnl;
#ifdef Is_True_On
  BOOL trace = Get_Trace(TP_REGION, TT_REGION_CG_DEBUG) ||
    Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG);
#endif

  if (RID_pregs_in(rid) != NULL) {
    CGRIN *cgrin = RID_cginfo( rid );
    Is_True(cgrin != NULL, ("REGION_Entry_PREG_Whirl, NULL cgrin"));
    tnl = NULL;
    // if localized, then the RID_pregs list is up-to-date;
    // otherwise (GRA), the pregtn ops is up-to-date.
    // (for localize, it is easier to update the pregs list
    // and not have to find the pregtn op when changing tns).
    if (CG_localize_tns) {
      PREG_LIST *prl;
      // used to skip the remaining pregs for quad, complex quad
      slist<PREG_NUM> skip_list;
#ifdef Is_True_On
      verify_pregs_list(rid, RID_pregs_in(rid));
#endif
      for (prl = RID_pregs_in(rid); prl; prl = PREG_LIST_rest(prl)) {
	PREG_NUM pr = PREG_LIST_first( prl );
	if (pr <= Last_Dedicated_Preg_Offset)
	  continue;

	// for quad/complex quad, skip the remaining pregs
	// once we have seen the first one
	if (skip_list.size()) {
	  slist<PREG_NUM>::iterator skip_search =
	    find(skip_list.begin(), skip_list.end(), pr);
	  if (skip_search != skip_list.end()) { // found it, skip it
	    Is_Trace(trace, (TFile, "REGION_Entry_PREG_Whirl(CG_localize_tns)"
			", skipping PREG %d\n\tbecause of (complex) quad\n",
			     pr));
	    continue;
	  }
	}

	TN *tn = PREG_To_TN_Array[pr];
	Is_True(tn != NULL,
		("REGION_Entry_PREG_Whirl(CG_localize_tns), NULL tn"));
	ST *spill_loc = TN_spill(tn);
	// will have created spill location
	if (spill_loc == NULL && TN_register(tn) != REGISTER_UNDEFINED) {
		// localize might have used dedicated stacked register 
	}
	else if (spill_loc == NULL || TN_is_rematerializable(tn)) {
	  // -O2 -CG:opt=0 could cause tn to have home rather than spill loc;
	  // is that okay?
	  DevWarn("REGION_Entry_PREG_Whirl: no spill location "
		  "for tn in entry list?");
	  continue;
	}

	// if the preg was a quad or quad complex, generate a temp location
	// and use that to get the aliasing right
	if (REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
	  Gen_quad_preg(pr, &tnl, entry_whirl, NULL, MTYPE_FQ,
			TRUE, rid, 0, am);
	  skip_list.push_front(pr + 1);
	} else if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
	  Gen_quad_preg(pr, &tnl, entry_whirl, NULL, MTYPE_CQ,
			TRUE, rid, 0, am);
	  skip_list.push_front(pr + 1); // no need to check for duplicates,
	  skip_list.push_front(pr + 2); // the preg_in set doesn't have any
	  skip_list.push_front(pr + 3); // and so we can't generate any here.
	} else { // no quad or complex quad
	  // 'type' is the type that the original preg had
	  TYPE_ID type = PREG_To_TN_Mtype[pr];
	  // generate load and store for glue code (not quad or complex quad)
	  WN *ld = WN_LdidPreg(type, pr); // need to use correct type
	  WN *st;
	  if (spill_loc) {
	  	st = WN_Stid(TY_mtype(ST_type(spill_loc)),
		       0, spill_loc, ST_type(spill_loc), ld);
	  	// the st is a lcl_spill_temp_xxx, add it to the
		// variable boundary set (550977)
	  	Is_True(strncmp(ST_name(spill_loc),"lcl_spill_temp",14) == 0,
		  ("REGION_Entry_PREG_Whirl, not a spill loc"));
	  	REGION_add_wn_points_to(&RID_used_in(rid), st, am);
	  	Is_Trace(trace,(TFile,"REGION_Entry_PREG_Whirl(CG_localize_tns), "
			  "TN %d <-> PREG %d, %s\n",
			  TN_number(tn), pr, ST_name(spill_loc)));
	  }
	  else {
	    	PREG_NUM ppr = TN_To_Assigned_PREG(tn);
		st = WN_StidIntoPreg(type, ppr, MTYPE_To_PREG(type), ld);
	    	REGION_add_preg_in(rid, ppr, MTYPE_V);
	    	Is_Trace(trace,(TFile,"REGION_Entry_PREG_Whirl, "
			    "TN %d (assigned to PREG %d) <--> PREG %d\n",
			    TN_number(tn), ppr, pr));
	  }
	  // add tn to tns_in list so outer region can find it
	  // when glue code is lowered and we want to reuse tn
	  tnl = TN_LIST_Push(tn, tnl, &REGION_mem_pool);
	  WN_INSERT_BlockLast(entry_whirl, st);
	}
      } // for (prl = RID_pregs_in
    } // if (CG_localize_tns)
    else {
      BB *bb = CGRIN_entry(cgrin);
      OP *op = BB_first_op(bb);
      OP *next;
#ifdef Is_True_On
//      verify_pregtn_order(bb, rid, TOP_begin_pregtn);
#endif
      // the PREG_To_TN_Array is out of date and Gen_quad_preg uses it, update
      Update_preg_to_tn_array(op, TOP_begin_pregtn);
      // used to skip the remaining pregs for quad, complex quad
      slist<PREG_NUM> skip_list;
      // find TOP_begin_pregtn, and build glue from that
      // have to iterate by hand since removing ops while iterating
      while (op != NULL) {
      	if (OP_code(op) == TOP_begin_pregtn) {
	  PREG_NUM pr = TN_value(OP_opnd(op, 1));

	  // for quad/complex quad, skip the remaining pregs
	  // once we have seen the first one
	  if (skip_list.size()) {
	    slist<PREG_NUM>::iterator skip_search =
	      find(skip_list.begin(), skip_list.end(), pr);
	    if (skip_search != skip_list.end()) { // found it, skip it
	      Is_Trace(trace, (TFile, "REGION_Entry_PREG_Whirl, skipping "
			       "PREG %d\n\tbecause of (complex) quad\n", pr));
	      next = OP_next(op);
	      op = next;
	      continue;
	    }
	  }

	  // if the preg was a quad or quad complex, generate a temp location
	  // and use that to get the aliasing right
	  if (REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
	    Gen_quad_preg(pr, &tnl, entry_whirl, NULL, MTYPE_FQ,
			  TRUE, rid, 0, am);
	    skip_list.push_front(pr + 1);
	  } else if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
	    Gen_quad_preg(pr, &tnl, entry_whirl, NULL, MTYPE_CQ,
			  TRUE, rid, 0, am);
	    skip_list.push_front(pr + 1); // no need to check for duplicates,
	    skip_list.push_front(pr + 2); // the pregtns do not have any
	    skip_list.push_front(pr + 3); // and so we can't generate any here.
	  } else { // no quad or complex quad
	    TN *tn = OP_opnd(op, 0);
	    Is_True(tn != NULL, ("REGION_Entry_PREG_Whirl, NULL tn"));
	    // 'type' is the type that the original preg had
	    TYPE_ID type = PREG_To_TN_Mtype[pr];
	    WN *ld = WN_LdidPreg(type, pr); // need to use correct type
	    WN *st;
	    ST *spill_loc = TN_spill(tn);
	    if (TN_is_rematerializable(tn)) {
		DevWarn("region bounds has rematerializable tn; can ignore?");
		spill_loc = NULL;	// have whirl home, not ST
	    }
	    if (TN_is_gra_homeable(tn)) {
		DevWarn("region bounds has home-able tn; should store to home, but just use reg for now");
		spill_loc = NULL;	// have whirl home, not ST
	    }
	    if (spill_loc) {
	  	Is_True(strncmp(ST_name(spill_loc),"gra_spill_temp",14) == 0,
			("REGION_Entry_PREG_Whirl, not a gra spill loc"));
	  	st = WN_Stid(TY_mtype(ST_type(spill_loc)),
		       0, spill_loc, ST_type(spill_loc), ld);
	  	REGION_add_wn_points_to(&RID_used_in(rid), st, am);
	  	Is_Trace(trace,(TFile,"REGION_Entry_PREG_Whirl, "
			  "TN %d <-> PREG %d, %s\n",
			  TN_number(tn), pr, ST_name(spill_loc)));
	    } 
	    else {
	    	PREG_NUM ppr = TN_To_Assigned_PREG(tn);
		st = WN_StidIntoPreg(type, ppr, MTYPE_To_PREG(type), ld);
	    	REGION_add_preg_in(rid, ppr, MTYPE_V);
	    	Is_Trace(trace,(TFile,"REGION_Entry_PREG_Whirl, "
			    "TN %d (assigned to PREG %d) <--> PREG %d\n",
			    TN_number(tn), ppr, pr));
	    }
	    // add tn to tns_in list so outer region can find it
	    // when glue code is lowered and we want to reuse tn
	    tnl = TN_LIST_Push(tn, tnl, &REGION_mem_pool);
	    WN_INSERT_BlockLast( entry_whirl, st );
	  }

	  next = OP_next(op);
	  op = next;
      	} else // not pregtn op
	  op = OP_next(op);
      } // while (op != NULL)
    } // else part of if (CG_localize_tns)

    CGRIN_tns_in(cgrin) = tnl;
    Is_Trace(trace, (TFile,"REGION_Entry_PREG_Whirl, tnl =\n"));
    Is_Trace_cmd(trace, Print_TN_List(TFile, tnl));
    Is_Trace(trace, (TFile,"REGION_Entry_PREG_Whirl, RGN %d bounds =\n",
		     RID_id(rid)));
    Is_Trace_cmd(trace, RID_set_print(TFile, rid));
  } // if (RID_pregs_in(rid) != NULL)
}

/* =======================================================================
 *
 *  REGION_Exit_Whirl_Labels
 *
 *  See interface description.
 *
 * =======================================================================
 */
LABEL_IDX
REGION_Exit_Whirl_Labels( WN *exit_whirl, BB *exit_bb, LABEL_IDX external_label,
			 RID *rid )
{
  WN *label_wn, *goto_wn, *last, *parent_exit;
  LABEL_IDX new_label;
  TN *new_target, *opndi;
  OP *br_op;
  INT i;

  Is_True(rid != NULL, ("REGION_Exit_Whirl_Labels, NULL rid"));
  Is_True(external_label != (LABEL_IDX) 0,
	    ("null external label in REGION_Exit_Whirl_Labels"));
  Is_True(WN_opcode(exit_whirl) == OPC_BLOCK,
	    ("unexpected opcode for exit_whirl"));

  last = WN_last( exit_whirl );
  if ( last != NULL && WN_opcode( last ) == OPC_RETURN ) {
    /* special case for glue block containing a RETURN
       The goto should already exist in the exit_bb */
    label_wn = WN_CreateLabel( external_label, 0, NULL );
    WN_INSERT_BlockFirst( exit_whirl, label_wn );
    return LABEL_IDX_ZERO; /* no new label */
  }

  /* new exit for region */
  new_label = Gen_Temp_Label();
  label_wn = WN_CreateLabel( new_label, 0, NULL );
  
#if defined(TARG_SL) && defined(TARG_SL2) 
  if(Cur_PU_Feedback) {
    FB_FREQ out_rgn=Cur_PU_Feedback->Query(rid->rwn,  FB_EDGE_CALL_OUTGOING);
    FB_Info_Invoke fb_inv(out_rgn);
    Cur_PU_Feedback->Annot_invoke(label_wn, fb_inv);
  }
#endif  
  
  WN_INSERT_BlockFirst( exit_whirl, label_wn );

  /* Reuse old exit label to keep external interface the same.
     Create a goto if the parent region does not have this label in its
     exit list. If the parent does have this exit, create a region_exit.
     The test case for this is cmetric, Olimit=10, -O2 */
  parent_exit = NULL;
  Is_True(RID_parent(rid) != NULL,
	  ("REGION_Exit_Whirl_Labels, NULL parent rid"));
  /* if parent is func_entry, it has no exit block */
  if (!RID_TYPE_func_entry(RID_parent(rid))) {
    Is_True(RID_rwn(RID_parent(rid)) != NULL,
	    ("REGION_Exit_Whirl_Labels, NULL rwn"));
    Is_True(WN_region_exits(RID_rwn(RID_parent(rid))) != NULL,
	    ("REGION_Exit_Whirl_Labels, NULL exit block"));
    parent_exit = WN_region_exits(RID_rwn(RID_parent(rid)));
    parent_exit = REGION_search_block(parent_exit,
		      comp_same_label_no(external_label));
  }
  if (parent_exit != NULL)
    goto_wn = WN_CreateRegionExit(external_label);
  else
    goto_wn = WN_CreateGoto(external_label);
  
#if defined(TARG_SL) && defined(TARG_SL2) 
  if(Cur_PU_Feedback) {
    FB_FREQ out_rgn=Cur_PU_Feedback->Query(rid->rwn,  FB_EDGE_CALL_OUTGOING);
    FB_Info_Invoke fb_inv(out_rgn);
    Cur_PU_Feedback->Annot_invoke(goto_wn, fb_inv);
  }
#endif

  WN_INSERT_BlockLast(exit_whirl, goto_wn);

  new_target = Gen_Label_TN( new_label, 0 );
  br_op = BB_branch_op( exit_bb );
  Is_True( br_op != NULL, ("REGION_Exit_Whirl_Labels, no branch op "
			   "in block exiting to an external label") );
  for ( i = 0; i < OP_opnds(br_op); i++ ) {
    opndi = OP_opnd( br_op, i );
    if ( ! TN_is_label( opndi ) )
      continue;
    if ( TN_label( opndi ) != external_label )
      continue;
    Set_OP_opnd( br_op, i, new_target );
  }

  return new_label; /* return new label so we can make new exit table */
}

/* =======================================================================
 *
 *  REGION_Exit_PREG_Whirl
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
REGION_Exit_PREG_Whirl( RID *rid, INT exit_num, WN *exit_whirl, 
			TN_LIST *outlist, struct ALIAS_MANAGER *am)
{
  WN *after;
  TN_LIST *tnl;
#ifdef Is_True_On
  BOOL trace = Get_Trace(TP_REGION, TT_REGION_CG_DEBUG) ||
    Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG);
#endif

  FmtAssert( WN_opcode( exit_whirl ) == OPC_BLOCK,
	    ("REGION_Exit_PREG_Whirl, unexpected opcode for exit_whirl") );
  Is_True(rid != NULL && exit_num < RID_num_exits(rid),
	("REGION_Exit_PREG_Whirl, inconsistency in num_exits"));

  // set 'after' to point to the last WHIRL statement inserted after the label
  // but before the goto in the region exit glue code
  {
    WN *last_label, *next;
    WN *first = WN_first( exit_whirl );
    if ( first == (WN *)NULL || WN_opcode(first) != OPC_LABEL ) {
      last_label = NULL;
    } else {
      last_label = first;
      while ( (next = WN_next(last_label)) && WN_opcode(next) == OPC_LABEL ) {
	last_label = next;
      }
    }
    after = last_label;
  }

  if (RID_pregs_out(rid) != NULL && RID_pregs_out_i(rid,exit_num) != NULL) {
    CGRIN *cgrin = RID_cginfo( rid );
    tnl = NULL;
    if (CG_localize_tns) {
      PREG_LIST *prl;
      // used to skip the remaining pregs for quad, complex quad
      slist<PREG_NUM> skip_list;
#ifdef Is_True_On
      verify_pregs_list(rid, RID_pregs_out_i(rid, exit_num));
#endif
      for (prl=RID_pregs_out_i(rid, exit_num); prl; prl=PREG_LIST_rest(prl)) {
	PREG_NUM pr = PREG_LIST_first( prl );
	if (pr <= Last_Dedicated_Preg_Offset)
	  continue;

	// for quad/complex quad, skip the remaining pregs
	// once we have seen the first one
	if (skip_list.size()) {
	  slist<PREG_NUM>::iterator skip_search =
	    find(skip_list.begin(), skip_list.end(), pr);
	  if (skip_search != skip_list.end()) { // found it, skip it
	    Is_Trace(trace, (TFile, "REGION_Exit_PREG_Whirl(CG_localize_tns)"
			", skipping PREG %d\n\tbecause of (complex) quad\n",
			     pr));
	    continue;
	  }
	}

	// 'type' is the type that the original preg had
	TYPE_ID type = PREG_To_TN_Mtype[pr];
	TN *tn = PREG_To_TN_Array[pr];
	Is_True(tn != NULL,
		("REGION_Exit_PREG_Whirl(CG_localize_tns), NULL tn"));
	ST *spill_loc = TN_spill( tn );
	if (spill_loc == NULL && TN_register(tn) != REGISTER_UNDEFINED) {
		// localize might have used dedicated stacked register 
	}
	else if (spill_loc == NULL || TN_is_rematerializable(tn)) {
	  DevWarn ("REGION_Exit_PREG_Whirl: no spill location "
		   "for tn in exit list?");
	  continue;
	}

	// if the preg was a quad or quad complex, generate a temp location
	// and use that to get the aliasing right
	if (REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
	  Gen_quad_preg(pr, &tnl, exit_whirl, &after, MTYPE_FQ,
			FALSE, rid, exit_num, am);
	  skip_list.push_front(pr + 1);
	} else if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
	  Gen_quad_preg(pr, &tnl, exit_whirl, &after, MTYPE_CQ,
			FALSE, rid, exit_num, am);
	  skip_list.push_front(pr + 1); // no need to check for duplicates,
	  skip_list.push_front(pr + 2); // the preg_in set doesn't have any
	  skip_list.push_front(pr + 3); // and so we can't generate any here.
	} else { // no quad or complex quad
	  WN *ld;
	  if (spill_loc) {
	  	// use type of spill rather than preg type
	  	ld = WN_Ldid( TY_mtype(ST_type(spill_loc)),
		       0, spill_loc, ST_type( spill_loc ) );
	  	// the ld is a lcl_spill_temp_xxx, add it to the
	  	// variable boundary set (550977)
	  	Is_True(strncmp(ST_name(spill_loc),"lcl_spill_temp",14) == 0,
		  ("REGION_Exit_PREG_Whirl, not a spill loc"));
	  	REGION_add_wn_points_to(&RID_def_in_live_out(rid), ld, am);
	  	Is_Trace(trace,(TFile,"REGION_Exit_PREG_Whirl(CG_localize_tns), "
			  "TN %d <-> PREG %d, %s\n",
			  TN_number(tn), pr, ST_name(spill_loc)));
	  } 
	  else {
	  	PREG_NUM ppr = TN_To_Assigned_PREG(tn);
	    	ld = WN_LdidPreg(type, ppr);
	    	REGION_add_preg_out(rid, exit_num, ppr, MTYPE_V);
	    	Is_Trace(trace,(TFile,"REGION_Exit_PREG_Whirl, "
			    "TN %d (assigned to PREG %d) <-> PREG %d\n",
			    TN_number(tn), ppr, pr));
	  }
	  WN *st = WN_StidIntoPreg( type, pr, MTYPE_To_PREG( type ), ld );
	  // add tn to tns_out list so outer region can find it
	  // when glue code is lowered and we want to reuse tn
	  tnl = TN_LIST_Push(tn, tnl, &REGION_mem_pool);
	  WN_INSERT_BlockAfter(exit_whirl, after, st);
	  after = st;
	}
      }
    }
    else {
      BB *bb = CGRIN_exit_i(cgrin,exit_num);
      OP *op = BB_first_op(bb);
      OP *next;
#ifdef Is_True_On
//      verify_pregtn_order(bb, rid, TOP_end_pregtn);
#endif
      // the PREG_To_TN_Array is out of date and Gen_quad_preg uses it, update
      Update_preg_to_tn_array(op, TOP_end_pregtn);
      // used to skip the remaining pregs for quad, complex quad
      slist<PREG_NUM> skip_list;
      // find TOP_end_pregtn, and build glue from that
      // have to iterate by hand since removing ops while iterating
      while (op != NULL) {
        if (OP_code(op) == TOP_end_pregtn) {
	  TN *tn = OP_opnd(op, 0);
	  Is_True(tn != NULL, ("REGION_Exit_PREG_Whirl, NULL tn"));
	  PREG_NUM pr = TN_value(OP_opnd(op, 1));

	  // for quad/complex quad, skip the remaining pregs
	  // once we have seen the first one
	  if (skip_list.size()) {
	    slist<PREG_NUM>::iterator skip_search =
	      find(skip_list.begin(), skip_list.end(), pr);
	    if (skip_search != skip_list.end()) { // found it, skip it
	      Is_Trace(trace, (TFile, "REGION_Exit_PREG_Whirl, skipping "
			       "PREG %d\n\tbecause of (complex) quad\n", pr));
	      next = OP_next(op);
	      op = next;
	      continue;
	    }
	  }

	  // 'type' is the type that the original preg had
	  TYPE_ID type = PREG_To_TN_Mtype[pr];

	  // if the preg was a quad or quad complex, generate a temp location
	  // and use that to get the aliasing right
	  if (REGION_search_preg_set(RID_pregs_quad(rid), pr)) {
	    Gen_quad_preg(pr, &tnl, exit_whirl, &after, MTYPE_FQ,
			  FALSE, rid, exit_num, am);
	    skip_list.push_front(pr + 1);
	  } else if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr)) {
	    Gen_quad_preg(pr, &tnl, exit_whirl, &after, MTYPE_CQ,
			  FALSE, rid, exit_num, am);
	    skip_list.push_front(pr + 1); // no need to check for duplicates,
	    skip_list.push_front(pr + 2); // the pregtns do not have any
	    skip_list.push_front(pr + 3); // and so we can't generate any here
	  } else { // no quad or complex quad
	    WN *ld;
	    ST *spill_loc = TN_spill(tn);
	    if (TN_is_rematerializable(tn)) {
		DevWarn("region bounds has rematerializable tn; can ignore?");
		spill_loc = NULL;	// have whirl home, not ST
	    }
	    if (TN_is_gra_homeable(tn)) {
		DevWarn("region bounds has home-able tn; should store to home, but just use reg for now");
		spill_loc = NULL;	// have whirl home, not ST
	    }
	    if (spill_loc) {
	  	Is_True(strncmp(ST_name(spill_loc),"gra_spill_temp",14) == 0,
			("REGION_Exit_PREG_Whirl, not a gra spill loc"));
	  	ld = WN_Ldid( TY_mtype(ST_type(spill_loc)),
		       0, spill_loc, ST_type( spill_loc ) );
	  	REGION_add_wn_points_to(&RID_def_in_live_out(rid), ld, am);
	  	Is_Trace(trace,(TFile,"REGION_Exit_PREG_Whirl, "
			  "TN %d <-> PREG %d, %s\n",
			  TN_number(tn), pr, ST_name(spill_loc)));
	    } 
	    else {
	  	PREG_NUM ppr = TN_To_Assigned_PREG(tn);
	    	ld = WN_LdidPreg(type, ppr);
	    	REGION_add_preg_out(rid, exit_num, ppr, MTYPE_V);
	    	Is_Trace(trace,(TFile,"REGION_Exit_PREG_Whirl, "
			    "TN %d (assigned to PREG %d) <-> PREG %d\n",
			    TN_number(tn), ppr, pr));
	    }
	    WN *st = WN_StidIntoPreg(type, pr, MTYPE_To_PREG(type), ld);
	    // add tn to tns_out list so outer region can find it
	    // when glue code is lowered and we want to reuse tn
	    tnl = TN_LIST_Push(tn, tnl, &REGION_mem_pool);
	    WN_INSERT_BlockAfter(exit_whirl, after, st);
	    after = st;
	  }

	  next = OP_next(op);
	  op = next;
      	} else // not pregtn
	  op = OP_next(op);
      } // while (op != NULL)
    }
    CGRIN_tns_out_i(cgrin,exit_num) = tnl;
    Is_Trace(trace, (TFile,"REGION_Exit_PREG_Whirl, tnl =\n"));
    Is_Trace_cmd(trace, Print_TN_List(TFile, tnl));
    Is_Trace(trace, (TFile,"REGION_Exit_PREG_Whirl, RGN %d bounds =\n",
		     RID_id(rid)));
    Is_Trace_cmd(trace, RID_set_print(TFile, rid));
  } // if (RID_pregs_out(rid) != NULL ...
}

/* =======================================================================
 *
 *  BB_REGION_Exit
 *
 *  See interface description.
 *
 * =======================================================================
 */
INT
BB_REGION_Exit( BB *bb, RID *rid )
{
  CGRIN *cgrin;
  INT n, i;
  
  if ( rid == NULL )
    return NO_REGION_EXIT;
  cgrin = RID_cginfo( rid );
  if ( cgrin == NULL )
    return NO_REGION_EXIT;
  n = RID_num_exits( rid );
  for ( i = 0; i < n; i++ ) {
    if ( bb == CGRIN_exit_i( cgrin, i ) ) {
      return i;
    }
  }
  return NO_REGION_EXIT;
}

/* =======================================================================
 *
 *  TN_LIST_Print
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
TN_LIST_Print( TN_LIST *tnl0 )
{
  TN_LIST *tnl;
  TN *tn;

  for ( tnl = tnl0; tnl; tnl = TN_LIST_rest( tnl ) ) {
    tn = TN_LIST_first( tnl );
    Print_TN( tn, TRUE );
    fprintf( TFile, "\n" );
  }
}

/* =======================================================================
 *
 *  RID_Find_Cginfo
 *
 *  See interface description.
 *
 * =======================================================================
 */
CGRIN* 
RID_Find_Cginfo( BB *bb)
{
  RID *tmp_rgn;
  tmp_rgn = BB_rid(bb);
  FmtAssert( tmp_rgn != NULL, ("null region found for bb"));
  tmp_rgn = RID_is_glue_code(tmp_rgn) ? RID_parent(tmp_rgn) : tmp_rgn;
  return RID_cginfo(tmp_rgn);
}

/* 
 * Return non-transparent (according to cg) rid.
 * Wopt and other phases have a different view of transparency,
 * but for CG it means not a olimit or user region.
 */
RID*
Non_Transparent_RID (RID *rid)
{
	switch (RID_type(rid)) {
	case RID_TYPE_func_entry:
	case RID_TYPE_pragma:
	case RID_TYPE_olimit:
		if (RID_is_glue_code(rid))
			return Non_Transparent_RID (RID_parent(rid));
		return rid;
	default:
		return Non_Transparent_RID (RID_parent(rid));
	}
}

