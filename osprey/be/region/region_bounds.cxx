/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


//-*-c++-*-
//============================================================================
//
// Module: region_bounds.cxx
// $Revision: 1.5 $
// $Date: 05/12/05 08:59:31-08:00 $
// $Author: bos@eng-24.pathscale.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.region_bounds.cxx $
//
// Revision history:
//  1-MAY-97 dahl - Original Version
//
// Description:
//   1) Generate a conservative boundary set for a region when MainOpt is not
//   run. This allows CG to be debugged separately. Called when command 
//   line options are: -O2 -PHASE:w=0 or -O0 -CG:opt=2
//
//   2) Generate a conservative boundary set when Preopt is run over the
//   entire PU (LNO's Preopt). This is for: -PHASE:p or -O3 -PHASE:w=0
//   It is basically the same code as for 1) except we do it on the CODEREP
//   form.
//
// Algorithm:
//   Recursively descend the WHIRL/CODEREP tree looking for STs within the
//   region. Enter these variables and pregs on the boundary lists.
//
// REGION_BOUND is just a wrapper for the routines, the trace flag, and
// the alias manager pointer.
//
// REGION_search_preg_set() searches the preg-in and preg-out sets.
//
//============================================================================

#define region_bounds_CXX	"region_bounds.cxx"
#ifdef _KEEP_RCS_ID
static char *rcs_id = region_bounds_CXX"$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include "wn.h"			// WN type
#include "wn_util.h"		// wn accessors
#include "ir_reader.h"		// for fdump_tree
#include "region_util.h"	// RID structure
#include "tracing.h"		// TFile
#include "opt_cfg.h"		// CFG
#include "opt_main.h"		// COMP_UNIT
#include "opt_alias_rule.h"	// ALIAS_RULE
#include "pragma_weak.h"	// pragma weak
#include "opt_points_to.h"	// Is_nested_call
#include "region_alias_templates.h"	// REGION_search_set
#include "data_layout.h"	// Get_ST_formal_preg_num

//============================================================================

// class to hold a couple variables global to traversal
class REGION_BOUND {
  private:
    BOOL _trace;		// trace flag is set by constructor
    ALIAS_MANAGER *_alias_mgr;  // am for POINTS_TO
    POINTS_TO *_all_alias_pt;   // "once size fits all" POINTS_TO
    STACK<RID *> _rstack;  	// stack of RIDs from root to current RID
    				// RGNs with boundary sets only

    void Push(RID *rid)		{ _rstack.Push(rid); }
    RID *Pop(void)		{ return _rstack.Pop(); }
    BOOL Is_Empty(void)		{ return _rstack.Is_Empty(); }
    RID *Top(void)		{ return _rstack.Top(); }
    ALIAS_MANAGER *Am(void)	{ return _alias_mgr; }
    POINTS_TO *All_alias_pt(void) { return _all_alias_pt; }
    void Set_all_alias_pt(POINTS_TO *pt) { _all_alias_pt = pt; }

    BOOL is_global(ST *);
    void GRB_merge_var(RID *, WN *);
    void GRB_merge_preg(RID *, WN *);
    void GRB_formal_ref(RID *, ST *, TY_IDX);	// formal refs only
    void GRB_merge_preg(RID *, PREG_NUM, ST *);	// in-set only (formal ref)
    void GRB_merge_var(RID *, ST *, TY_IDX);	// in-set only (formal ref)
    void Propagate_boundary_up(RID *);
    void Convert_ST_list(RID *, WN *);		// for barrier calls

  public:
    REGION_BOUND(ALIAS_MANAGER *alias_mgr, MEM_POOL *pool) :
      _alias_mgr(alias_mgr), _rstack(pool) {
      // -O0 -CG:opt=2 or -O2 -PHASE:p ==> it is a CG or boundary problem
      _trace = Get_Trace(TP_REGION, TT_REGION_CG_DEBUG) ||
	Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG);
      _all_alias_pt = NULL;
      _rstack.Clear();
      Is_True(_alias_mgr != NULL,
	      ("REGION_BOUND::REGION_BOUND alias manager is NULL"));
    }
    ~REGION_BOUND(void) { }

    BOOL Trace(void)		{ return _trace; }
    void grb(WN *);		// recursive traversal of WHIRL
    void prune(RID *);		// traverse RID tree and cleanup
};

// decide if a symbol is global or not
inline BOOL
REGION_BOUND::is_global(ST *st)
{
  switch (ST_sclass(st)) {
    case SCLASS_PSTATIC:
    case SCLASS_FSTATIC:
    case SCLASS_COMMON:
    case SCLASS_EXTERN:
    case SCLASS_UGLOBAL:
    case SCLASS_DGLOBAL:
      return TRUE;
    default:
      return FALSE;
  }
}

// Add a preg to the boundary set.
// Optimization: if all occurrences of a preg are LDIDs, it can
// be kept of out the live_out set.
void
REGION_BOUND::GRB_merge_preg(RID *rid, WN *wn)
{
  ST *st = WN_st(wn);

  // load_offset and store_offset are unioned
  WN_OFFSET off = WN_load_offset(wn);

  // REGION_add_preg_* checks for duplicates, creates one from
  // the region pool if needed, returns TRUE if it did something
  BOOL added = REGION_add_preg_in(rid, off, ST_btype(st));

  Is_True(WN_operator(wn) == OPR_STID || WN_operator(wn) == OPR_LDID,
	  ("REGION_BOUND::GRB_merge_preg, unknown operator"));
  BOOL is_stid = (WN_operator(wn) == OPR_STID);

  // LDID: only add to used_in set
  if (is_stid) {
    // added: time-saver, we have to try to add to live-out set regardless
    // of if it was added to live-in set (e.g. LDIDs followed by STID, it
    // is already in the live-in set but we need to try to add to live out
    // once we hit the STID. Save time though if STID is not added, don't
    // try rest of exits.
    added = TRUE;
    for (INT i=0; i<RID_num_exits(rid) && added; i++)
      added = REGION_add_preg_out(rid, i, off, ST_btype(st));
  }
  Is_Trace(Trace() && added, (TFile,"REGION_BOUND::GRB_merge_preg, RGN %d, "
			      "adding %s %d\n",RID_id(rid),ST_name(st),off));
}

//---------------------------------------------------------------------------
// the next three subroutines are for formal-refs

// This is used for f77 formal refs, LDID, STID, or LDA
// add either a preg or a temporary stack location to the live-in sets
inline void
REGION_BOUND::GRB_formal_ref(RID *rid, ST *st, TY_IDX ty)
{
  Is_True(st != NULL, ("REGION_BOUND::GRB_formal_ref, NULL ST"));
  if (ST_sclass(st) == SCLASS_FORMAL_REF) {
    ST *base = Get_ST_formal_ref_base(st);
    Is_True(base != NULL,
	    ("REGION_BOUND::GRB_formal_ref, no base for formal ref"));
    PREG_NUM preg = Get_ST_formal_preg_num(base);
    if (preg != 0) // add a preg
      GRB_merge_preg(rid, preg, base);
    else // add a temp stack location
      GRB_merge_var(rid, st, ty);
  }
}

// This is used for f77 formal refs only which are read-only so no need
// to put in live-out set.
void
REGION_BOUND::GRB_merge_preg(RID *rid, PREG_NUM pr, ST *st)
{
  // REGION_add_preg_* checks for duplicates, creates one from
  // the region pool if needed, returns TRUE if it did something
  BOOL added = REGION_add_preg_in(rid, pr, MTYPE_V);

  Is_Trace(Trace() && added, (TFile,"REGION_BOUND::GRB_merge_preg2, RGN %d, "
			      "adding %s %d\n", RID_id(rid), ST_name(st), pr));
}

// Add a variable to the live_in boundary set, don't have WHIRL
// used for formal_refs, searches for duplicates
void
REGION_BOUND::GRB_merge_var(RID *rid, ST *st, TY_IDX ty)
{
  Is_True(st != NULL, ("REGION_BOUND::GRB_merge_var2, NULL ST"));
  Is_True(ty != (TY_IDX) 0, ("REGION_BOUND::GRB_merge_var2, NULL TY"));
  // create points_to and add to live-in set
  POINTS_TO pt;
  INT64 size = TY_size(ty);
  Is_True(size != 0,
	  ("REGION_BOUND::GRB_merge_var2, ST size is wrong"));
  pt.Analyze_ST(st, 0/*offset*/, size, 0, 0,
		ty, FALSE/*assume has no equiv*/);
  // first search for duplicates
  if (!REGION_search_set(RID_used_in(rid),
			 comp_same_pt(&pt, Am()->Rule()))) {
    REGION_add_points_to(&RID_used_in(rid), &pt, Am());
    Is_Trace(Trace(), (TFile,"REGION_BOUND::GRB_merge_var2, RGN %d, "
			      "adding %s\n", RID_id(rid), ST_name(st)));
  }
}

//---------------------------------------------------------------------------

// POINTS_TO::Meet()
#pragma weak Meet__9POINTS_TOGPC9POINTS_TOP2ST

// Add a variable to the boundary set.
// Optimization: if all occurrences of a variable are LDIDs, it can
// be kept of out the live_out set.
// This routine handles LDID/STID only. The tricky part is adding to
// a set but avoiding duplicates.
void
REGION_BOUND::GRB_merge_var(RID *rid, WN *wn)
{
  Is_True(Am() != NULL, ("REGION_BOUNDS:GRB_merge_var(), NULL alias manager"));

  // first scan live-in set for duplicates, if none, add to it
  POINTS_TO pt;
  pt.Analyze_WN_expr(wn);

  // for LDID, search live in set, it may not be in live out set (no STID yet)
  // for STID, it may only have been put in live in set last time (only have
  //	seen LDIDs so far), so check live out set
  Is_True(WN_operator(wn) == OPR_STID || WN_operator(wn) == OPR_LDID,
	  ("REGION_BOUND::GRB_merge_var, unknown operator"));
  BOOL is_stid = (WN_operator(wn) == OPR_STID);
  BOOL added = FALSE;

  // load or store
  if (!REGION_search_set(RID_used_in(rid),
			 comp_same_pt(&pt, Am()->Rule()))) {
    REGION_add_wn_points_to(&RID_used_in(rid), wn, Am());
    added = TRUE;
  }

  // store only
  if (is_stid && !REGION_search_set(RID_def_in_live_out(rid),
				    comp_same_pt(&pt, Am()->Rule()))) {
    REGION_add_wn_points_to(&RID_def_in_live_out(rid), wn, Am());
    added = TRUE;
  }

  Is_Trace(Trace() && added, (TFile,"REGION_BOUND::GRB_merge_var, RGN %d, "
	    "added variable %s to %s\n", RID_id(rid), ST_name(WN_st(wn)),
		    is_stid ? "in/out sets" : "in set"));
}

// Once a region's boundary set is found, we need to propagate this
// information up the RID tree. The tricky part is that the enclosing
// region may be transparent and so we need to go to the enclosing
// region with a boundary, that is what the stack is for.
// We only need to propagate up one level because when the enclosing
// region is finished it will call this routine also. The parent
// region may have boundaries from sibling regions or its own code
// so need to merge and check for duplicates.
void
REGION_BOUND::Propagate_boundary_up(RID *rid)
{
  if ( !Is_Empty() ) {
    Is_True(rid != Top(),
	    ("REGION_BOUND::Propagate_boundary_up, TOS is same as rid"));
    Is_True(RID_bounds_exist(rid) == REGION_BOUND_EXISTS,
	    ("REGION_BOUND::Propagate_boundary_up, bounds do not exist"));
    Is_Trace(Trace(), (TFile,"Propagate up: RGN %d to RGN %d\n",
		       RID_id(rid), RID_id(Top())));

    // variables in/out
    for (POINTS_TO_SET *ptmp=RID_used_in(rid); ptmp; ptmp=ptmp->Next) {
      if (!REGION_search_set(RID_used_in(Top()),
			     comp_same_pt(ptmp->Pt, Am()->Rule()))) {
	// does not check for duplicates
	REGION_add_points_to(&RID_used_in(Top()), ptmp->Pt, Am());
	REGION_add_points_to(&RID_def_in_live_out(Top()), ptmp->Pt, Am());
      }
    }

    // pregs in
    PREG_LIST *ptmp2;
    for (ptmp2=RID_pregs_in(rid); ptmp2;
	 ptmp2=PREG_LIST_rest(ptmp2)) {
      PREG_NUM pr = PREG_LIST_first(ptmp2);
      // check for quad and complex quad, avoid duplicates
      TYPE_ID type = MTYPE_V;
      if (REGION_search_preg_set(RID_pregs_quad(rid), pr))
	type = MTYPE_FQ;
      else if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr))
	type = MTYPE_CQ;
      // add to in-set of enclosing non-transparent region
      REGION_add_preg_in(Top(), pr, type);
    }

    // pregs out, slow but accurate
    for (INT32 j=0; j<RID_num_exits(rid); j++) {
      for (ptmp2=RID_pregs_out_i(rid, j); ptmp2;
	   ptmp2=PREG_LIST_rest(ptmp2)) {
	PREG_NUM pr = PREG_LIST_first(ptmp2);
	// check for quad and complex quad, avoid duplicates
	TYPE_ID type = MTYPE_V;
	if (REGION_search_preg_set(RID_pregs_quad(rid), pr))
	  type = MTYPE_FQ;
	else if (REGION_search_preg_set(RID_pregs_complex_quad(rid), pr))
	  type = MTYPE_CQ;
	// add to all exits in enclosing non-transparent region
	for (INT32 i=0; i<RID_num_exits(Top()); i++)
	  REGION_add_preg_out(Top(), i, pr, type);
      }
    }
  }
}

// Convert ST list of barrier call to POINTS_TOs
// two cases:
// 1) nested call has list of STs. Convert these to POINTS_TOs and
//	add to boundary sets
// 2) nested call has no list, set contains_barrier bit
void
REGION_BOUND::Convert_ST_list(RID *rid, WN *wn)
{
  Is_True(WN_opcode(wn) == OPC_FORWARD_BARRIER ||
	  WN_opcode(wn) == OPC_BACKWARD_BARRIER,
	  ("REGION_BOUND::Convert_ST_list, not a barrier"));

  // based on OPT_STAB::Create_barrier_defs
  if (WN_kid_count(wn) == 0) 
    RID_contains_barrier(rid) = TRUE;
  else {
    for (INT i=0; i<WN_kid_count(wn); i++) {
      // create pt for each st in the barrier
      ST *st = WN_st(WN_kid(wn, i));
          Is_True(ST_class (st) == CLASS_VAR,
            ("OPT_STAB::Create_barrier_defs: barrier contains non-var ST"));
      POINTS_TO pt;
      Is_True(ST_sclass(st) != SCLASS_UNKNOWN,(""));
      INT64 ofst = 0;
      TY& ty = Ty_Table[ST_type (st)];
      INT64 size = TY_size (ty);
      pt.Analyze_ST(st, ofst, size, 0, 0, ST_type(st), TRUE/*assume has equiv*/);

      // see if that pt is in the set already
      if (!REGION_search_set(RID_used_in(rid),
			     comp_same_pt(&pt, Am()->Rule()))) {
	REGION_add_points_to(&RID_used_in(rid), &pt, Am());
	Is_True(REGION_search_set(RID_def_in_live_out(rid),
				  comp_same_pt(&pt, Am()->Rule())),
		("REGION_BOUND::Convert_ST_list, pt not found in used_in "
		 "but in live_out"));
	REGION_add_points_to(&RID_def_in_live_out(rid), &pt, Am());
      }
    }
  }
}

// Recursive traversal of WHIRL/CODEREPs inside a region, looks for STs to
// put in boundary lists. Note: `node' is either a WN or CFG
void
REGION_BOUND::grb(WN *wn)
{
  WN *wtmp;
  RID *rid;
  INT i;
  BOOL push_rid = FALSE;

  Is_True(wn != NULL, ("REGION_BOUND::grb NULL WN"));

  // figure out what rid to use, cases:
  //   region with bounds - use rid associated with wn
  //   region with no bounds - use enclosing rid or NULL if no enclosing region
  //   not a region - use enclosing rid or NULL if no enclosing region
  // The enclosing rid is really the enclosing rid that is not transparent.
  if (WN_opcode(wn) == OPC_REGION) {
    rid = REGION_get_rid(wn);
    Is_True(rid != NULL, ("REGION_BOUND:grb, NULL RID"));
    // if the region has no bounds (transparent) then use the enclosing rid
    if (RID_TYPE_transparent(rid))
      rid = (Is_Empty()) ? NULL : Top();
    else
      push_rid = TRUE; // region with bounds, push on stack
  } else
    rid = (Is_Empty()) ? NULL : Top();

  switch (WN_operator(wn)) {

    // grab the ST and add it to the appropriate boundary sets
    // for all other load variants use default case and recurse down
    // to LDID, STID, or LDA
    case OPR_LDID:
    case OPR_STID:
      { // we traverse loads and stores for non-transparent regions only
	Is_True(rid != NULL && !RID_TYPE_transparent(rid),
		("REGION_BOUND::grb, transparent rgn"));
	ST *st = WN_st(wn);
	Is_True(st != NULL, ("REGION_BOUND::grb, NULL ST"));
	TY_IDX ty = WN_ty(wn);
	Is_True(ty != (TY_IDX) 0, ("REGION_BOUND::grb, NULL TY"));

	// This IF condition is tricky so compute the opposite which
	// is easier to think about:
	//   1) Don't have to worry about volatiles, they are always saved
	BOOL cond = TY_is_volatile(ty);
	//   2) No need to add to boundary set if global and
	//      already aliased to all globals
	cond |= is_global(st) && RID_aliased_to_globals(rid);
	if (!cond) {
	  // adds to both live-in and live-out sets, checks for duplicates
	  if (ST_class(st) == CLASS_PREG) { // add a preg
	    // don't add dedicated pregs at this point
	    if (WN_offset(wn) != 32 && WN_offset(wn) != 2) //PPP
	      GRB_merge_preg(rid, wn);
	  } else // add a variable
	    GRB_merge_var(rid, wn);
	}

	// handle scalar F77 params that will be split into a preg later
	// during lower_to_cg
	GRB_formal_ref(rid, st, ty);

	// continue traversing down
	for (i=0; i<WN_kid_count(wn); i++)
	  grb(WN_kid(wn,i));
      }
      break;

    case OPR_LDA: // do nothing for RHS LDA, only look at LDA indirectly
      // we traverse loads and stores for non-transparent regions only
      Is_True(rid != NULL && !RID_TYPE_transparent(rid),
	      ("REGION_BOUND::grb, transparent rgn"));

      // handle scalar F77 params that will be split into a preg later
      // during lower_to_cg
      GRB_formal_ref(rid, WN_st(wn), WN_ty(wn));
      break;	  // when processing ILOAD

    case OPR_PARM:
    case OPR_ILOAD:
    case OPR_MLOAD:
    case OPR_ILOADX:
    case OPR_ISTORE:
    case OPR_MSTORE:
    case OPR_ISTOREX:
      // we traverse loads and stores for non-transparent regions only
      Is_True(rid != NULL && !RID_TYPE_transparent(rid),
	      ("REGION_BOUND::grb, transparent rgn"));
      {
	POINTS_TO pt;
	pt.Analyze_WN_expr(wn);
	if (pt.Base_is_fixed() || pt.Restricted() || pt.Unique_pt() ||
	    pt.F_param()) {
	  // avoid duplicates
	  if (!REGION_search_set(RID_used_in(rid),
				 comp_same_pt(&pt, Am()->Rule()))) {
	    // conservative, should only add to in-set when load
	    REGION_add_points_to(&RID_used_in(rid), &pt, Am());
	    REGION_add_points_to(&RID_def_in_live_out(rid), &pt, Am());
	  }
	} else
	   RID_aliased_to_indirects(rid) = TRUE;
      }

      // continue traversing down
      for (i=0; i<WN_kid_count(wn); i++)
	grb(WN_kid(wn,i));
      break;

    // ----- the cases below handle the recursive tree walk -----

    case OPR_REGION: // skip pragma and exits blocks, no STs there
    { RID *rtmp;
      // see if there is a reason to push a new rid on the stack
      // and process its contents
      if (push_rid) {
	Is_True(rid != NULL && REGION_get_rid(wn) == rid &&
		!RID_TYPE_transparent(rid),
		("REGION_BOUND::grb, rid is wrong for region with bounds"));
	Is_True(RID_rwn(REGION_get_rid(wn)) == wn,
		("REGION_BOUND::grb, RID <-> WN inconsistency"));

	// push this RID on the stack because it has a valid boundary set
	Push(rid);

	// go through each statement in the region body
	for (wtmp=WN_first(WN_region_body(wn)); wtmp; wtmp=WN_next(wtmp))
	  grb(wtmp);

	RID *rtmp2 = Pop();
	Is_True(rtmp2 == rid, ("REGION_BOUND::grb, stack inconsistency"));

	// whether we actually added to the boundary or not, it has been
	// processed
	RID_bounds_exist(rid) = REGION_BOUND_EXISTS;
	RID_level(rid) = RL_RBI;
	
	// transfer this region boundary information to parent regions
	// that have boundaries
	Propagate_boundary_up(rid);

      } else {
	// This region is transparent but if there is another region on
	// the stack, we need to scan this region and account for the
	// loads/stores to the region on the stack
	if (rid != NULL) {
	  Is_True(rid == Top(),("REGION_BOUND::grb, rid != Top()"));
	  // go through each statement in the region body and use the stacked
	  // rid to find the appropriate boundary
	  for (wtmp=WN_first(WN_region_body(wn)); wtmp; wtmp=WN_next(wtmp))
	    grb(wtmp);
	} else {
	  // Region is transparent and there is no region on the stack.
	  // If the region has no child regions, return.
	  // Otherwise skip to the next region and see if it has work to do.
	  rid = REGION_get_rid(wn); // rematerialize rid, we had it NULL
	  			    // so no one would add a bound to this
	  			    // transparent region.
	  Is_True(rid != NULL, ("REGION_BOUND:grb, NULL RID"));
	  Is_True(RID_TYPE_transparent(rid), 
		  ("REGION_BOUND::grb, region is not transparent"));
	  for (rtmp = RID_first_kid(rid); rtmp != NULL; rtmp=RID_next(rtmp))
	    grb(RID_rwn(rtmp));
	}
      }
    }
      break;

    case OPR_FUNC_ENTRY:
    { RID *rtmp;
      Is_True(rid == NULL, ("REGION_BOUND::grb, rid should be NULL here"));
      rid = REGION_get_rid(wn);
      Is_True(rid != NULL && RID_TYPE_func_entry(rid),
	      ("REGION_BOUND::grb, rid messed up for func_entry"));
      Is_Trace(Trace(),(TFile,"===== Generate_region_boundaries (GRB) %s\n",
			ST_name(WN_st(wn))));
      // Func_entries are transparent (there is no work to do at this level).
      // If the PU has no regions, skip everything in this PU and return.
      // Otherwise skip to the next region and see if it has work to do.
      // We do have to scan all regions because there may be a region with
      // boundaries hidden down in the tree.
      for (rtmp = RID_first_kid(rid); rtmp != NULL; rtmp = RID_next(rtmp))
	grb(RID_rwn(rtmp));
    }
      break;

    case OPR_CALL: // mark all aliased bits on rid
      RID_aliased_to_globals(rid) = TRUE;
      RID_aliased_to_indirects(rid) = TRUE;

      // Is_nested_call is a template that works with Mongoose stab
      // or aux_stab. STAB_ADAPTER is from opt_points_to.h
      if (Is_nested_call(wn, STAB_ADAPTER()))  // opt_alias_analysis.cxx
	RID_contains_uplevel(rid) = TRUE; // local vars that are really uplevel

      // need to look at parameters because some may be locals
      for (i=0; i<WN_kid_count(wn); i++) {
	WN *wtmp2 = WN_kid(wn, i);
	Is_True(WN_operator(wtmp2) == OPR_PARM,
		("REGION_BOUND::grb, CALL sequence error"));
	grb(wtmp2);
      }
      break;

    case OPR_FORWARD_BARRIER:
    case OPR_BACKWARD_BARRIER:
      Convert_ST_list(rid, wn);
      break;

    case OPR_BLOCK:
      for (wtmp=WN_first(wn); wtmp; wtmp=WN_next(wtmp))
	grb(wtmp);
      break;

    default:
      for (i=0; i<WN_kid_count(wn); i++)
	grb(WN_kid(wn, i));
      break;
  } // switch
}

void
REGION_BOUND::prune(RID *rid)
{
  Is_True(rid != NULL, ("REGION_BOUND::prune, NULL RID"));
  Is_Trace(Trace(), (TFile, "REGION_BOUND::prune boundary sets called\n"));

  // TODO:
  // go through all sets that have boundaries and if the global bit is
  // set take out the globals from the variable sets.
}

// Entry point for Generate_region_boundaries, called by be/be/driver.c.
//
// Algorithm (recursive: Generate_region_boundaries): 
// 	scan the WHIRL tree and find all STs. 
//	for each ST:
//		if it is a PREG, add to PREG lists
//		if it is a variable, add to variable lists
//
// Requirements:
//	requires that region_init has assigned RIDs to all the regions
//	first because it uses the fact that a PU RID has a child or not
//	to control the traversal. It also uses the RID tree to jump to
//	the next WHIRL node that has work to do.
//
// This has the effect of producing a conservative boundary set.
// Note it is not too conservative because only the variables used/def'd
// in the region are added to the boundary set. This is important because
// we do not want variables that are used elsewhere in the PU but not in
// this region to be in the boundary set.
extern "C"
void Generate_region_boundaries(WN *wn, ALIAS_MANAGER *am)
{
  Is_True(wn != NULL && WN_opcode(wn) == OPC_FUNC_ENTRY,
	  ("Generate_region_boundaries, NULL WN or not a PU"));
  RID *root_rid = REGION_get_rid(wn);
  Is_True(root_rid != NULL && RID_TYPE_func_entry(root_rid),
	  ("Generate_region_boundaries, invalid RID type"));

  // recursive part, traverses WN tree looking for STs
  REGION_BOUND rb(am, &REGION_mem_pool);
  rb.grb(wn);
  rb.prune(REGION_get_rid(wn));

  Is_Trace(rb.Trace(),
	   (TFile,"Generate_region_boundaries (GRB) final sets:\n"));
  Is_Trace_cmd(rb.Trace(), RID_set_print(TFile, root_rid));
}

// =======================================================================
// REGION_search_preg_set: search preg sets
// returns TRUE if a PREG is found in the set
// used to avoid duplicates in boundary sets
// NOTE: this involves linear search and should be improved
//       (it is isolated here for your rewriting convenience)
extern "C"
BOOL REGION_search_preg_set(PREG_LIST *pset, PREG_NUM pr)
{
  PREG_LIST *ptmp;
  for (ptmp = pset; ptmp; ptmp = PREG_LIST_rest(ptmp)) {
    if (pr == PREG_LIST_first(ptmp))
      return TRUE;
  }
  return FALSE;
}
