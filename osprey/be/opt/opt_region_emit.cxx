//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_region_emit.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_region_emit.cxx,v $
//
// Revision history:
//  03-OCT-96 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// Defines utilities for Handling Region related work in Optimizer
// Emitter
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_region_emit_CXX	"opt_region_emit.cxx"
static char *rcs_id = 	opt_region_emit_CXX"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "tracing.h"

#include "region_util.h"

#include "opt_base.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_htable.h"
#include "opt_region_emit.h"
#include "opt_mu_chi.h"
#include "config.h"

#include "bitset.h"

// ====================================================================
// Check if there are any pragmas that make us keep the region
// ====================================================================

static BOOL
Required_pragmas_for_region( WN * region_pragma_list )
{
  Is_True( WN_operator(region_pragma_list) == OPR_BLOCK,
    ("Required_pragmas_for_region: not a block") );

  for ( WN *stmt = WN_first(region_pragma_list);
	stmt != NULL;
	stmt = WN_next(stmt) )
  {
    const OPERATOR opr = WN_operator(stmt);

    if ( opr == OPR_PRAGMA || opr == OPR_XPRAGMA ) {
      WN_PRAGMA_ID pragma = (WN_PRAGMA_ID)WN_pragma(stmt);

      switch ( pragma ) {
	case WN_PRAGMA_COPYIN:
	  return TRUE;

	default:
	  break;
      }
    }
  }

  // if no required ones, must not be required
  return FALSE;
}

// ====================================================================
// Handle the creation of regions
// ====================================================================

void
Push_region(STACK<E_REGION *> *stk, BB_NODE *start_region, MEM_POOL *pool)
{
  Is_True(start_region->Kind() == BB_REGIONSTART,
	  ("Push_region, region start not found"));
  BB_REGION *bb_region = start_region->Regioninfo();
  BB_NODE *end_region = bb_region->Region_end();

  E_REGION *e_region = CXX_NEW(E_REGION(start_region, end_region), pool);

  stk->Push(e_region);
}

WN*
Pop_region(STACK<E_REGION *> *stk, WN *first_wn, WN *last_wn,
	   REGION_LEVEL rgn_level, OPT_STAB *opt_stab )
{
  E_REGION *e_region = stk->Pop();
  BB_REGION *bb_region = e_region->Region_start()->Regioninfo();
  WN *region_wn = NULL;

  // for EH Guard region, emit even if empty
  if (RID_TYPE_guard(bb_region->Rid()) && first_wn == NULL && last_wn == NULL){
    first_wn = last_wn = WN_CreateComment("EH GUARD REGION");
  }

  // for empty regions, see if there are required pragmas
  if ( first_wn == NULL && last_wn == NULL &&
       Required_pragmas_for_region(bb_region->Region_pragma_list()) )
  {
    first_wn = last_wn = WN_CreateComment("REQUIRED PRAGMAS FOR REGION");
  }

  // emit if:
  //   there is code in the region
  if ( first_wn != NULL ) { // PPP 549705
    // create the region and the body
    WN *region_body = WN_CreateBlock();
    Is_True(first_wn != NULL,("EMITTER::Pop_region, null first statement"));
    WN_first(region_body) = first_wn;
    Is_True(last_wn != NULL,("EMITTER::Pop_region, null last statement"));
    WN_last(region_body) = last_wn;
    region_wn = WN_CreateRegion(REGION_type_to_kind(bb_region->Rid()),
				region_body,
				bb_region->Region_pragma_list(),
				bb_region->Region_exit_list(),
				RID_id(bb_region->Rid()),
				bb_region->Ereg_supp());
    WN_COPY_All_Maps(region_wn, bb_region->Orig_wn());

    // go through region pragmas and convert aux_ids back to STs and offsets
    if (REGION_is_EH(region_wn))
      opt_stab->Convert_EH_pragmas(region_wn);

    // update the RID and level
    REGION_emit(bb_region->Rid(), region_wn, rgn_level, 
		bb_region->Region_num_exits(), bb_region->Region_line_num());
  }
  return region_wn;  
}

// generate a used_in boundary set for a region given an entry chi list
// PPP NOTE: This routine is too conservative. It assumes that all sym
// entries are live in. Only the ones that have upward exposed uses are.
// Need to expand this routine to look at DU chains and exclude aux_ids
// that are defined before used inside the region.
void
REGION_live_in_from_chi(RID *rid,
                        CHI_LIST *clist,
                        OPT_STAB *opt_stab,
                        ALIAS_MANAGER *alias_mgr)
{
  Is_True(PU_has_region(Get_Current_PU()),
	  ("REGION_live_in_from_chi, regions are off"));

  if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
    fprintf(TFile,"===== REGION_live_in_from_chi, RGN#%d\n",
	    RID_id(rid));
    clist->Print(TFile);
  }

  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;
  FOR_ALL_NODE(cnode, chi_iter, Init(clist)) {
    if (cnode->Live()) { // include only upward exposed uses
      AUX_STAB_ENTRY *psym = opt_stab->Aux_stab_entry(cnode->Aux_id());
      if ((psym->Is_real_var() && !psym->Is_volatile()) ||
	  psym->Is_virtual()) {
	POINTS_TO *aux_pt = psym->Points_to();
	ST *st = opt_stab->St(cnode->Aux_id());
	if (st && ST_class(st) == CLASS_PREG) { // preg live-in
	  PREG_NUM pnum = opt_stab->St_ofst(cnode->Aux_id());
	  REGION_add_preg_in(rid, pnum, ST_btype(st));
	} else {				// var live-in
	  POINTS_TO *pt = opt_stab->Points_to(cnode->Aux_id());
	  REGION_add_points_to(&RID_used_in(rid), pt, alias_mgr);
	}
      }
    }
  }
  RID_bounds_exist(rid) = REGION_BOUND_EXISTS;
}

// generate a live_out boundary set for a region given a region exit mu list
void
REGION_live_out_from_mu(RID *rid,
                        MU_LIST *mlist,
                        OPT_STAB *opt_stab,
                        ALIAS_MANAGER *alias_mgr)
{
  Is_True(PU_has_region(Get_Current_PU()),
	  ("REGION_live_out_from_mu, regions are off"));

  if (Get_Trace(TP_REGION,TT_REGION_ALL)) {
    fprintf(TFile,"===== REGION_live_out_from_mu, RGN#%d\n",
	    RID_id(rid));
    mlist->Print(TFile);
  }

  MU_LIST_ITER mu_iter;
  MU_NODE *mnode;
  FOR_ALL_NODE(mnode, mu_iter, Init(mlist)) {
    AUX_STAB_ENTRY *psym = opt_stab->Aux_stab_entry(mnode->Aux_id());
    if ((psym->Is_real_var() && !psym->Is_volatile()) ||
	psym->Is_virtual()) {
      POINTS_TO *aux_pt = psym->Points_to();
      ST *st = opt_stab->St(mnode->Aux_id());
      if (st && ST_class(st) == CLASS_PREG) { // preg live-out
	// NOTE: if PREG 2 is ever mentioned in the region (due to a call)
	// 	 then it will be live out.
	PREG_NUM pnum = opt_stab->St_ofst(mnode->Aux_id());
	REGION_add_preg_out(rid, 0 /* this is a hack! PPP */,
			pnum, ST_btype(st));
      } else {				      // var live-out
	POINTS_TO *pt = opt_stab->Points_to(mnode->Aux_id());
	REGION_add_points_to(&RID_def_in_live_out(rid), pt, alias_mgr);
      }
    }
  }
  RID_bounds_exist(rid) = REGION_BOUND_EXISTS;
}

//============================================================================
// Prune the region's boundary sets (called from the main emitter)
// Look through all loads and stores of PREGs in the region and
// update the pregs in and out sets accordingly.
//============================================================================

// Useset_expr, set useset bits for each LDID of a preg
// recursive, searches cr tree
void
PRUNE_BOUND::Useset_expr(CODEREP *cr)
{
  switch (cr->Kind()) {
    case CK_LDA: 
    case CK_CONST: 
    case CK_RCONST:
      break;

    case CK_VAR:
      {
	// if PREG then add to use set
	AUX_ID aux_id = cr->Aux_id();
	ST *st = _opt_stab->St(aux_id);
	if (st && ST_class(st) == CLASS_PREG) {
	  Is_Trace(Trace(), (TFile, "Useset_expr, adding Aux_id %d, "
			     "PREG %d\n", aux_id,
			     (PREG_NUM) _opt_stab->St_ofst(aux_id)));
	  FBS_Union1D(_useset, aux_id);
	}
      }
      break;

    case CK_IVAR: 
      Useset_expr(cr->Ilod_base());
      if (cr->Opr() == OPR_MLOAD)
	Useset_expr(cr->Mload_size());
      else if (cr->Opr() == OPR_ILOADX)
	Useset_expr(cr->Index());
      break;

    case CK_OP:
      {
	for  (INT32 i=0; i<cr->Kid_count(); i++)
	  Useset_expr(cr->Opnd(i));
      }
      break;

    default:
      Is_True(FALSE, ("PRUNE_BOUND::Useset_expr: unexpected CK_KIND"));
      break;
  }
}

// Useset_stmt, set useset bits for each STID of a preg
// calls Usest_expr to search each tree
void
PRUNE_BOUND::Useset_stmt(STMTREP *stmt)
{
  if (stmt->Rhs())
    Useset_expr(stmt->Rhs());
  switch (stmt->Opr()) { 
    case OPR_ISTORE:
    case OPR_ISTBITS:
      Useset_expr(stmt->Lhs()->Istr_base());
      break;
    case OPR_MSTORE:
      {
	Useset_expr(stmt->Lhs()->Istr_base());
	CODEREP *num_bytes = (CODEREP *)stmt->Lhs()->Mstore_size();
	Useset_expr(num_bytes);
      }
      break;
    default:
      ;
  }
}

// traverse entire region and set bit in mod/use sets
void
PRUNE_BOUND::Collect_mod_use_sets(BB_NODE *bb)
{
  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    // set the _modset for all the defs
    if (OPERATOR_is_scalar_store (stmt->Opr()))
      FBS_Union1D(_modset, stmt->Lhs()->Aux_id());
    // now look at every LDID for the useset
    Useset_stmt(stmt);
  }

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Collect_mod_use_sets(dom_bb);
}



// remove a POINTS_TO for the aux_id from the boundary set
// based on OPT_STAB::REGION_add_to_bound()
// BOOL outset is TRUE if we are to remove from the out-set
// FALSE means remove from in-set.
void
PRUNE_BOUND::REGION_remove_from_bound(AUX_ID aux_id, BOOL outset)
{
  RID *rid = _cfg->Rid();
  Is_True(rid != NULL, ("PRUNE_BOUND::REGION_remove_from_bound, NULL RID"));

  // skip return vsym, note: do not skip default vsym
  if (aux_id == _opt_stab->Return_vsym()) {
    Is_Trace(Trace(),(TFile,"Return_vsym(), aux_id %d\n",aux_id));
    return;
  }

  POINTS_TO *pt = _opt_stab->Points_to(aux_id);
  ST *st = pt->Base();
  // we only look at Is_real_var so the ST is guaranteed to exist
  Is_True(st != NULL, ("PRUNE_BOUND::REGION_remove_from_bound, NULL st"));

//  else {
//    REGION_remove_aux_id_points_to(&RID_def_in_live_out(rid),
//				   aux_id, opt_stab);
//    Is_Trace(Trace(),(TFile,"===== REGION_remove_from_bound (RGN %d), "
//	      "removing vir var aux_id %d, %s\n",RID_id(rid), aux_id,
//	      outset ? "out-set" : "in-set"));
//  }
}

//  Prune the boundary set
//    -- See pv470095.  If DCE deletes a path and removes
//       all assignments the variable, then need to delete the 
//       variable from the boundary set.
//
void
PRUNE_BOUND::Prune_boundary_sets(void)
{
  // set up the _modset and _useset bit vectors
  Collect_mod_use_sets(_cfg->Entry_bb());

  if (Trace()) {
    fprintf(TFile,"PRUNE_BOUND::Prune_boundary_sets, modset: ");
    BS_Print(_modset,TFile);
    fprintf(TFile,"\nPRUNE_BOUND::Prune_boundary_sets, useset: ");
    BS_Print(_useset,TFile);
    fprintf(TFile,"\n");
    RID_set_print(TFile, _cfg->Rid());
  }

  AUX_ID idx;
  AUX_STAB_ITER aux_stab_iter(_opt_stab);
  FOR_ALL_NODE(idx, aux_stab_iter, Init()) {
    if (_opt_stab->Aux_stab_entry(idx)->Is_real_var()) {
      if (!FBS_MemberP(_modset, idx)) // not modified
	REGION_remove_from_bound(idx, TRUE);
      if (!FBS_MemberP(_useset, idx)) // not used
	REGION_remove_from_bound(idx, FALSE);
    }
  }
}
