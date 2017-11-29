/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_emit.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_emit.cxx,v $
//
// Revision history:
//  21-DEC-94 shin - Original Version
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
//    The documentation of the emitter design is in Preopt-emitter.doc
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_emit_CXX	"opt_emit.cxx"
static char *rcs_id = 	opt_emit_CXX"$Revision: 1.13 $";
#endif /* _KEEP_RCS_ID */

// The following for pu_info.h, which includes elftypes.h, which uses
// standard types.
#define USE_STANDARD_TYPES

#include <stdint.h>
#include "defs.h"
#include "tracing.h"
#include "erglob.h"
#include "config_targ.h"
#include "wn.h"
#include "ir_reader.h"          // fdump_tree_with_freq
#include "wn_simp.h"
#include "wn_util.h"
#include "region_util.h"
#include "pu_info.h"		// Set_PU_Info_state

#include "config.h"
#include "opt_config.h"
#include "opt_base.h"
#include "opt_config.h"
#include "opt_util.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "opt_emit.h"
#include "opt_cfg.h"
#include "opt_region_emit.h"
#include "opt_rvi.h"
#include "opt_sym.h"
#include "opt_ssa.h"
#include "opt_du.h"
#include "opt_alias_mgr.h"
#include "opt_loop.h"
#include "opt_main.h"

#include "opt_emit_template.h"
#include "wssa_emitter.h"  // WSSA emitter
#include "pu_info.h"

#if defined(TARG_NVISA)
// To get better loop depth comments in the ptx file
// Require better implement because opt is not supposed
// to use global statics
static INT cur_loop_depth = 0;
#endif


inline WN *
STMTREP::Gen_wn(STMT_CONTAINER *stmt_container, EMITTER *emitter)
{
  return Gen_stmt_wn(this, stmt_container, emitter);
}


inline void
STMT_LIST::Gen_wn(STMT_CONTAINER *stmt_container, EMITTER *emitter)
{
  Gen_stmt_list_wn(this, stmt_container, emitter);
}

inline WN*
CODEREP::Gen_wn(EMITTER *emitter)
{
  WN *wn = Gen_exp_wn(this, emitter);
  if (Kind() == CK_VAR || 
      (Kind() == CK_IVAR && Opr() == OPR_PARM))
    emitter->Connect_cr_wn(this, wn);
  return wn;
}


inline void
BB_NODE::Gen_wn(EMITTER *emitter, BOOL copy_phi)
{
  Gen_bb_wn(this, emitter);
  if (OPT_Enable_WHIRL_SSA && copy_phi &&
      Firststmt() != NULL &&
      WN_operator_is(Firststmt(), OPR_LABEL)) {
    emitter->WSSA_Emitter()->WSSA_Copy_PHI(this, Firststmt());
  }
}


//============================================================================
// functions that raise certain statements in the preopt emitter
//============================================================================
#define SWAP(a, b, tmp) ( tmp = a, a = b, b = tmp )

inline BOOL
Verify_Block_Stmt_Sequence(WN *block)
{
  // Check WN_first can reach WN_last through WN_next
  //
  WN *tmp = WN_first(block);
  while (tmp && WN_next(tmp) != NULL) 
     tmp = WN_next(tmp);
  if (tmp != WN_last(block))
     return FALSE;
  else
     return TRUE;
}

static WN*
Create_block_stmt(BB_NODE *ff, BB_NODE *ll)
{
  WN *wn = WN_CreateBlock();

  // need to find the first block with a statement
  WN_first(wn) = NULL;
  WN_last(wn) = NULL;

  // Fix 641790 and 645068:
  //  emitter has trouble dealing with an empty "ff" BB.
  //  A BB has the STMTREP * and WN *.  They might not
  //  match if the basic block is empty.  The workaround here
  //  is to rely on STMTREP * to screen out empty BBs.
  // 
  while (ff->First_stmtrep() == NULL &&
#ifdef KEY
         ff->Firststmt() == NULL &&
#endif
	 ff->Kind() == BB_GOTO &&
	 ff != ll)
    ff = ff->Next();
  if (ff == ll && ll->First_stmtrep() == NULL)
    return wn;

  for ( BB_NODE *tmpf = ff; tmpf != NULL; tmpf = tmpf->Next() ) {
     // The following assert is commented out, since we frequently see
     // cases where Firststmt()==NULL and Laststmt()!=NULL (bug# .
     //
     // Is_True((tmpf->Firststmt() == NULL && tmpf->Laststmt() == NULL) ||
     //      (tmpf->Firststmt() != NULL && tmpf->Laststmt() != NULL),
     //      ("Error in stmt sequencing for BB %d in Create_block_stmt()",
     //       (int)tmpf->Id()));
     //
     // TODO: fix this, and also limit the BB sequence this is called on
     // (looks like O(n*n) algorithm now, where n is no of basic blocks,
     // when this is done for nested loops)!
     //
     // rdahl:TEST
     // if (tmpf->Firststmt() != NULL && tmpf->Laststmt() == NULL)
     // fprintf(stderr,
     // 	"bb->id()=%d, first=Ox%x, last=Ox%x, next(last)=Ox%x\n",
     // 	(int)tmpf->Id(), tmpf->Firststmt(), tmpf->Laststmt(),
     // 	WN_next(tmpf->Laststmt()));
     // if (tmpf == ll)
     // fprintf(stderr, "-----------------------");
    
    if ( WN_first(wn) == NULL && tmpf->Firststmt() != NULL ) {
      WN_first(wn) = tmpf->Firststmt();
    }
    if ( tmpf->Laststmt() != NULL) {
      WN_last(wn) = tmpf->Laststmt();
    }
    // we may have reached the last BB in this group
    if (tmpf == ll)
      break;
  }

  Is_True( WN_first(wn) == NULL || WN_prev(WN_first(wn)) == NULL,
	   ("First stmt has prev stmt."));
  Is_True(Verify_Block_Stmt_Sequence(wn),
	  ("Illegal stmt sequence in block created in Create_block_stmt"));
  return wn;
}

static WN*
Create_block(EMITTER *emitter, BB_NODE *ff, BB_NODE *ll)
{
  emitter->Gen_wn(ff, ll);
  return Create_block_stmt( ff, ll);
}

BOOL
EMITTER::Raise_altentry( BB_NODE *bb )
{
  bb->Init_stmt( bb->Entrywn() );
  if (bb->First_stmtrep()) {
    if (bb->First_stmtrep()->Op() == OPC_OPT_CHI) {
      Connect_sr_wn( bb->First_stmtrep(), bb->Entrywn() );
      if (bb->First_stmtrep() == bb->Last_stmtrep())
	return TRUE;
    }
  }
  return FALSE;
}


void
EMITTER::Raise_func_entry(BB_NODE *bb, BB_NODE *last_bb)
{
  FmtAssert(bb != NULL,
            ("EMITTER::Raise_func_entry: no OPR_FUNC_ENTRY statement"));
  WN *stmt;
  BB_REGION *bb_region = NULL;

  if (bb->Kind() == BB_ENTRY) {
    _opt_func = bb->Entrywn();
    stmt = Create_block(this, bb, last_bb);
  } else {
    Is_True(bb->Kind() == BB_REGIONSTART,
	    ("EMITTER::Raise_func_entry, inappropriate entry"));
    bb_region = bb->Regioninfo();
    RID *rid = bb_region->Rid();

    stmt = Create_block(this, bb, last_bb);
    _opt_func = WN_CreateRegion(REGION_type_to_kind(bb_region->Rid()),
				stmt,
				bb_region->Region_pragma_list(),
				bb_region->Region_exit_list(),
				RID_id(bb_region->Rid()), /* recreate region */
				bb_region->Ereg_supp());
    WN_COPY_All_Maps(_opt_func, bb_region->Orig_wn());

    // go through region pragmas and convert aux_ids back to STs and offsets
    if (REGION_is_EH(_opt_func))
      Opt_stab()->Convert_EH_pragmas(_opt_func);
    // connect between entry chi and stmtrep was delayed until region created
    Connect_sr_wn(Region_entry_stmt(),_opt_func);
    // line number for region
    WN_Set_Linenum(_opt_func, bb->Linenum());
    // update region used-in sets from entry CHI list
    if (bb->First_stmtrep() && bb->First_stmtrep()->Op() == OPC_OPT_CHI)
// RFE: 454057
// TODO: these routines need to be rewritten to use SSA to be more exact.
// Only call for performance regions.
//      REGION_live_in_from_chi(Cfg()->Rid(), bb->First_stmtrep()->Chi_list(),
//                              Opt_stab(), Alias_Mgr());
;
    else
      FmtAssert(0,("EMITTER::Raise_func_entry, could not find entry CHI"));
  }

  bb->Init_stmt(_opt_func);
  WN_Set_Linenum(stmt, WN_Get_Linenum(_opt_func));
  if (Cfg()->Feedback()) {
    Cfg()->Feedback()->Emit_feedback(_opt_func, bb);
  }

  WN *pragmas;
  if (WN_opcode(_opt_func) == OPC_FUNC_ENTRY) {
    WN_func_body(_opt_func) = stmt;
    pragmas = WN_func_pragmas(_opt_func);
  } else {
    Is_True(WN_opcode(_opt_func) == OPC_REGION,
	    ("EMITTER::Raise_func_entry, unknown entry WN"));
    pragmas = WN_region_pragmas(_opt_func);
  }
  
  WN *wopt_pragma;
  for (wopt_pragma = WN_first(pragmas);
       wopt_pragma != NULL;
       wopt_pragma = WN_next(wopt_pragma)) {
    if ( WN_pragma(wopt_pragma) == WN_PRAGMA_WOPT_FINISHED_OPT )
      break;
  }
  
  if (wopt_pragma == NULL) {
    wopt_pragma = WN_CreatePragma(WN_PRAGMA_WOPT_FINISHED_OPT,
				  (ST *) NULL, 0, 0);
    WN_INSERT_BlockBefore(pragmas, NULL, wopt_pragma);
  }

  INT32 pragma_flag = WN_pragma_arg2(wopt_pragma);
  if (WOPT_Enable_Tail_Recur)
    pragma_flag |= WOPT_TAIL_RECUR_FINISHED;
  WN_pragma_arg2(wopt_pragma) = pragma_flag;
  
  if (WN_opcode(_opt_func) == OPC_FUNC_ENTRY) {
    WN_func_pragmas(_opt_func) = pragmas; 
    // attach rid and update level
    REGION_emit(Cfg()->Rid(), _opt_func, Cfg()->Rgn_level(), 1, 0);
  } else {
    WN_region_pragmas(_opt_func) = pragmas; 
    // attach rid and update level
    REGION_emit(Cfg()->Rid(), _opt_func, Cfg()->Rgn_level(),
		bb_region->Region_num_exits(), bb_region->Region_line_num());
  }
}


static WN*
Raise_if_stmt(EMITTER *emitter, BB_NODE **bb)
{
  // The result WN node would be a statement with branch condition
  // as the WN_if_test, then block in WN_then, and else block in
  // WN_else.

  // Identify the four major blocks.
  // These are assumptions made according to CFG construction:
  // cond_bb has two successor: then block and else block
  // then-block has one successor: merge block
  // else-block has one successor: merge-block
  // the prev/next sequence is: cond-then-else-merge

  BB_LIST_ITER bb_iter;
  BB_NODE *bb_cond, *bb_then, *bb_else, *bb_merge;

  bb_cond = *bb;

  bb_then = bb_cond->If_then();
  bb_else = bb_cond->If_else();
  bb_merge = bb_cond->If_merge();

  bb_cond->Gen_wn(emitter);

  // Now IPA relies this relation between CFG and if-then-else stmts
  Is_True(bb_cond->Next() == bb_then, 
	  ("EMITTER::Raise_if_stmt:  then statement (BB%d) does not follow if-statement (BB%d).",
	   bb_then->Id(), bb_cond->Id()));

  // create OPC_BLOCK statement for then-block and else-block
  WN *block_then = Create_block(emitter, bb_then, bb_else->Prev());
  WN_Set_Linenum(block_then, bb_cond->Then_loc());
  WN *block_else = Create_block(emitter, bb_else, bb_merge->Prev());
  WN_Set_Linenum(block_else, bb_cond->Else_loc());

  // set *bb to merge_bb
  *bb = bb_merge;

  // pull out the test from the falsebr statement, which is the only
  // one statement in bb_cond
  WN *rwn = WN_CreateIf(WN_kid0(bb_cond->Laststmt()), block_then, block_else);
  WN_Set_Linenum(rwn, bb_cond->Linenum());
  if (emitter->Cfg()->Feedback()) {
    emitter->Cfg()->Feedback()->Emit_feedback( rwn, bb_cond );
  }
  emitter->Connect_sr_wn( bb_cond->Branch_stmtrep(), rwn );

  if (OPT_Enable_WHIRL_SSA && 
      (bb_merge->First_stmtrep() == NULL ||
       bb_merge->First_stmtrep()->Opr() != OPR_LABEL)) {
    // only place phi on IF when merge bb is empty 
    //   or first stmt in merge bb is not LABEL
    emitter->WSSA_Emitter()->WSSA_Copy_PHI(bb_merge, rwn);
  }

  // if bb_cond has more than one statement, create a BLOCK ...
  if (bb_cond->Firststmt() != bb_cond->Laststmt()) {
    WN *block = WN_CreateBlock();
    STMT_CONTAINER stmtcon(bb_cond->Firststmt(), bb_cond->Laststmt());
    stmtcon.Remove(bb_cond->Laststmt());
    stmtcon.Append(rwn);
    WN_first(block) = stmtcon.Head();
    WN_last(block) = stmtcon.Tail();
    return block;
  } else
    return rwn;
}

// ====================================================================
// Make sure that the loop-ending condition involves the loop index,
// and the other part of the condition is loop invariant
// ====================================================================
static BOOL
Valid_doloop_condition( BB_NODE *bb_end, const OPT_STAB *opt_stab )
{
  STMTREP *cond_br = bb_end->Branch_stmtrep();
  BB_LOOP *loop_info = bb_end->Loop();
  WN *index = loop_info->Index();

  // make sure we had a conditional branch
  if ( cond_br->Op() != OPC_TRUEBR && cond_br->Op() != OPC_FALSEBR ) {
    DevWarn( "Valid_doloop_condition(BB:%d) not true/falsebr",
	    bb_end->Id() );
    return FALSE;
  }

  // make sure we have a comparison with two operands
  CODEREP *rhs = cond_br->Rhs();
  if ( rhs->Kind() != CK_OP || ! OPERATOR_is_compare(rhs->Opr()) ) {
    DevWarn( "Valid_doloop_condition(BB:%d) cond_br not compare",
	    bb_end->Id() );
    return FALSE;
  }

  // DO loop does not allow OPR_NE and OPR_EQ.
  const OPERATOR cond_opr = rhs->Opr();
  if (!(cond_opr == OPR_LE || cond_opr == OPR_GE ||
	cond_opr == OPR_LT || cond_opr == OPR_GT))
    return FALSE;
	

  // find a coderep to match our loop index
  CODEREP *index_cr = NULL;
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  FOR_ALL_ELEM ( phi, phi_iter, Init(bb_end->Phi_list()) ) {
    if (!phi->Res_is_cr()) continue;
    CODEREP *res = phi->RESULT();
    // skip the dead phi
    if (res == NULL) continue;
    if (WN_st(index) == opt_stab->St(res->Aux_id()) &&
	WN_idname_offset(index) == opt_stab->St_ofst(res->Aux_id()) &&
	opt_stab->Is_real_var(res->Aux_id())) {
      index_cr = res;
      break;
    }
  }
  if ( index_cr == NULL ) {
    DevWarn( "Valid_doloop_condition(BB:%d) no index in phis",
	    bb_end->Id() );
    return FALSE;
  }

  // Fix 481888, as long as LHS or RHS based strictly on index var,
  // return TRUE.
  BOOL found_one_side_use_index = FALSE;
  for (INT i = 0; i < rhs->Kid_count(); i++)
    if (rhs->Opnd(i)->Contains(index_cr) &&
	loop_info->Index_relative_expr(rhs->Opnd(i), index_cr))
      found_one_side_use_index = TRUE;
  if (found_one_side_use_index == FALSE) {
    DevWarn( "Valid_doloop_condition(BB:%d) can't find index var on comparison",
	    bb_end->Id() );
    return FALSE;
  }

  STMTREP_ITER stmt_iter(bb_end->Stmtlist());
  STMTREP *sr;
  FOR_ALL_NODE( sr, stmt_iter, Init() ) {
    if (sr->Op() != OPC_LABEL && sr != bb_end->Last_stmtrep()) {
      DevWarn( "Valid_doloop_condition(BB:%d) more than one stmt",
       	      bb_end->Id() );
      return FALSE;
    }
  }

  return TRUE;
}

void
Detect_invalid_doloops(COMP_UNIT *cu)
{
  if (cu->Phase() != MAINOPT_PHASE) {
    CFG_ITER cfg_iter(cu->Cfg());
    BB_NODE *bb;
    FOR_ALL_NODE( bb, cfg_iter, Init() ) {
      if (bb->Kind() == BB_DOEND) {
	if (!Valid_doloop_condition(bb, cu->Opt_stab())) {
	  bb->Loop()->Reset_valid_doloop();
	  DevWarn("Invalid doloop detected in WHIRL input to PREOPT.");
	}
      }
    }
  }
}


// ====================================================================
// Build a new LOOP_INFO wn attached to the DO_LOOP
// ====================================================================

static WN *
Build_new_loop_info( WN *do_loop, WN *old_info )
{
  WN *induction = NULL;
  WN *trip_count= NULL;
  UINT16 est_trips;
  UINT16 depth;
  INT32 lflags;
  INT new_kid_count = 0;

  // do we have anything to build upon?
  if ( old_info != NULL ) {
    est_trips = WN_loop_trip_est(old_info);
    depth = WN_loop_depth(old_info);
    lflags = WN_loop_flag(old_info);
  }
  else {
    est_trips = 0;
#if defined(TARG_NVISA)
    depth = cur_loop_depth;
#else
    depth = 0;
#endif
    lflags = 0;
  }

  // the iv function returns the tree itself, and not a copy
  induction = WN_LOOP_InductionVariable( do_loop );
  if ( induction ) {
    Is_True( WN_opcode(induction) == OPC_IDNAME,
      ("Build_new_loop_info: bad induction var: %s",
	OPCODE_name(WN_opcode(induction))) );
    induction = WN_COPY_Tree_With_Map(induction);
  }
  trip_count= WN_LOOP_TripCount( do_loop );

  // update the estimated trip count
  if ( trip_count != NULL ) {
    if ( WN_operator(trip_count) == OPR_INTCONST ) {
      INT64 trip_count_val = WN_const_val(trip_count);
      if ( trip_count_val <= 0 )
	est_trips = 0;
      else
	est_trips = MIN( trip_count_val, UINT16_MAX );
    }
    else {
      // because we don't have coderep<->wn mappings for the trip-
      // count expression, we do not put out non-constant trip counts.
      trip_count = NULL;
    }
  }

  if ( induction != NULL ) new_kid_count++;
  if ( trip_count != NULL ) new_kid_count++;

  WN *new_info;
  if ( old_info == NULL || new_kid_count != WN_kid_count(old_info) ) {
    new_info = WN_CreateLoopInfo( induction, trip_count,
				  est_trips, depth, lflags );
  }
  else {
    // update the old info rather than create a new node
    if ( induction )
      WN_set_loop_induction(old_info, induction);
    if ( trip_count )
      WN_set_loop_trip(old_info, trip_count);
    WN_loop_trip_est(old_info) = est_trips;
    WN_loop_depth(old_info) = depth;
    WN_loop_flag(old_info) = lflags;

    new_info = old_info;
  }

  return new_info;
}


// ====================================================================
// Find the WN that defines the index variable for the given loop.
// If it's found, and can be moved to the end of the block, return TRUE
// ====================================================================

static BOOL
Find_doloop_init_stmt(EMITTER *emitter, BB_NODE *bb_start, 
		      BB_LOOP *loop_info, WN **init_stmtwn)
{
  BOOL found_init = FALSE;

  OPT_STAB *opt_stab = emitter->Opt_stab();
  WN *index_wn = loop_info ? loop_info->Index() : NULL;
  ST *index_st = index_wn ? WN_st(index_wn) : NULL;
  WN_OFFSET index_ofs = index_wn ? WN_idname_offset(index_wn) : 0;

  // see if we can find a store to the index variable
  STMTREP_ITER stmt_iter(bb_start->Stmtlist());
  STMTREP *init_sr;
  WN *init_wn = bb_start->Firststmt();
  STMTREP *found_init_sr;
  WN *found_init_wn;
  FOR_ALL_NODE( init_sr, stmt_iter, Init() ) {
    if ( init_sr->Opr() == OPR_STID ) {
      CODEREP *init_lhs = init_sr->Lhs();
      if ( opt_stab->St(init_lhs->Aux_id()) == index_st &&
	   init_lhs->Offset() == index_ofs )
      {
	// make sure we have a matching WN as well
	if ( init_wn &&
	     WN_operator(init_wn) == OPR_STID &&
	     WN_st(init_wn) == index_st &&
	     WN_store_offset(init_wn) == index_ofs )
	{
	  found_init = TRUE;
	  found_init_sr = init_sr;
	  found_init_wn = init_wn;
	}
	else {
	  // mis-match between WN and STMTREP, so give up
	  found_init = FALSE;
	}
      }
    }
    // proceed to next WN as well as next stmtrep
    init_wn = init_wn ? WN_next(init_wn) : NULL;
  }

  if ( found_init ) {
    // if it's not the last statement, see if we can make it the
    // last statement by checking for dependences among statements
    // that follow it
    if ( found_init_sr != bb_start->Last_stmtrep() ) {
      CODEREP *index_cr = found_init_sr->Lhs();	// index variable

      // process all of the statements that follow the init statement
      STMTREP_ITER rev_stmt_iter(bb_start->Stmtlist());
      STMTREP *tmp_sr;
      FOR_ALL_NODE_REVERSE( tmp_sr, rev_stmt_iter, Init() ) {
	// stop at the init statement
	if ( tmp_sr == found_init_sr )
	  break;

	// does the statement redefine the index var?
	if ( tmp_sr->Redefines_var( index_cr->Aux_id() ) ) {
	  found_init = FALSE;
	  break;
	}

	// does the statement reference the index var?
	if ( tmp_sr->References_var( index_cr->Aux_id() ) ) {
	  found_init = FALSE;
	  break;
	}

	// does the left-hand side redefine something used in the
	// rhs of the init statement?
	if ( tmp_sr->Opr() == OPR_STID ) {
	  if ( found_init_sr->Rhs()->References_var( tmp_sr->Lhs()->Aux_id() ) ) {
	    found_init = FALSE;
	    break;
	  }
	}

	// does this statement have a chi that redefines something
	// used in the rhs of the init statement?
	if ( tmp_sr->Has_chi() ) {
	  CHI_LIST_ITER chi_iter;
	  CHI_NODE *cnode;
	  CHI_LIST *chi_list = tmp_sr->Chi_list();
	  FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	    if ( found_init_sr->Rhs()->References_var( cnode->Aux_id() ) ) {
	      found_init = FALSE;
	      break;
	    }
	  }
	  if ( !found_init )
	    break;
	}

      } // end reverse statement loop
    }

    // if we still have a valid init, return it
    if ( found_init ) {
      *init_stmtwn  = found_init_wn;
    }
  }

  return found_init;
}




// ====================================================================
// Raise the block(s) to either a do_loop or while_do depending on
// validity of the loop.
// ====================================================================

static WN*
Raise_doloop_stmt(EMITTER *emitter, BB_NODE **bb)
{
  // Identify the major blocks.
  // These are assumptions made according to CFG construction:
  // start-block has one successor: end-block
  // end-block has two successor: body-block and merge-block
  // body-block has one successor: step-block
  // step-block has one successor: end-block
  // the prev/next sequence is: start-end-body-step-merge

  BB_LIST_ITER bb_iter;
  BB_NODE *bb_start, *bb_end, *bb_body, *bb_step, *bb_merge;

  bb_start = *bb;
  bb_end = bb_start->Next();
  bb_body = bb_end->Next();

  bb_iter.Init(bb_end->Succ());
  BB_NODE *s1 = bb_iter.First_elem(); BB_NODE *s2 = bb_iter.Next_elem();
  bb_merge = (s1 == bb_body)? s2 : s1;

  bb_iter.Init(bb_end->Pred());
  s1 = bb_iter.First_elem(); s2 = bb_iter.Next_elem();
  bb_step = (s1 == bb_start)? s2 : s1;

  FmtAssert(bb_start == bb_start->Loopstart(), ("Wrong start"));
  FmtAssert(bb_end == bb_start->Loopend(), ("Wrong end"));
  FmtAssert(bb_body == bb_start->Loopbody(), ("Wrong body"));
  FmtAssert(bb_step == bb_start->Loopstep(), ("Wrong step"));
  FmtAssert(bb_merge == bb_start->Loopmerge(), ("Wrong merge"));

#if defined(TARG_NVISA)
  ++cur_loop_depth;
#endif
  
  // generate code for the initialization block
  bb_start->Gen_wn(emitter);

  // and then find the initialization statement
  WN *init_stmtwn = NULL;
  if ( ! Find_doloop_init_stmt( emitter, bb_start, bb_end->Loop(),
				&init_stmtwn ) )
  {
    FmtAssert(FALSE, ( "Find_doloop_init_stmt(BB:%d) returns FALSE",
		       bb_end->Id() ));
  }

  // do we need to get rid of the "goto" at the end of the step block?
  STMTREP *goto_end = bb_step->Branch_stmtrep();
  if ( goto_end != NULL && goto_end->Op() == OPC_GOTO ) {
    if ( goto_end->Label_number() == bb_end->Labnam() ) {
      bb_step->Remove_stmtrep( goto_end );
    }
  }
  // generate code for the increment block
  bb_step->Gen_wn(emitter);

  // generate code for the condition
  bb_end->Gen_wn(emitter, FALSE);
  FmtAssert(WN_opcode(bb_end->Laststmt()) == OPC_TRUEBR ||
	    WN_opcode(bb_end->Laststmt()) == OPC_FALSEBR,
	    ("DO condition is not TRUEBR/FALSEBR. (BB:%d)", bb_end->Id()));
  // verify that the index variable is part of the condition
  if ( ! Valid_doloop_condition( bb_end, emitter->Opt_stab()) ) {

    // Fix 663341:  do not verify invalid do-loops.
    if (bb_end->Loop()->Valid_doloop())
      FmtAssert(FALSE, ( "Valid_doloop_condition(BB:%d) returns FALSE",
			 bb_end->Id() ));
  }

  // generate code for the body of the loop
  WN *block_body = Create_block(emitter, bb_body, bb_step->Prev());
  WN_Set_Linenum(block_body, bb_start->Linenum());

  // get the induction variable for the loop
  WN *index = bb_start->Loopindex();

  // The result WN node would be a statement 
  WN *rwn = NULL;

  // create a do loop
  WN *step = bb_step->Laststmt();

  // Move extra statements in BB_STEP into the body.
  if (bb_step->Firststmt() != bb_step->Laststmt()) {
    STMT_CONTAINER stmtcon(WN_first(block_body), WN_last(block_body));
    stmtcon.Append_list(bb_step->Firststmt(), bb_step->Laststmt());
    STMT_ITER stmt_iter;
    FOR_ALL_ELEM(step,stmt_iter,Init(bb_step->Firststmt(),
				     bb_step->Laststmt())) {
      if (WN_operator(step) == OPR_STID && WN_st(step) == WN_st(index)) {
	stmtcon.Remove(step);
	break;
      }
    }
    Is_True(step, ("Raise_doloop_stmt: cannot find step statement"));
    WN_first(block_body) = stmtcon.Head();
    WN_last(block_body) = stmtcon.Tail();
  }

  Is_True(WN_opcode(bb_end->Laststmt()) == OPC_FALSEBR,
	  ("Raise_doloop_stmt, unknown opcode"));
  WN *loop_cond = WN_kid0(bb_end->Laststmt());

  // reattach (update?) the opc_loop_info if we started with one
  WN *loop_info_wn = bb_body->Label_loop_info();
  char fake_info_wn_area [sizeof (WN)];
  WN* fake_info_wn = (WN *) &fake_info_wn_area;
  if ( loop_info_wn == NULL && WOPT_Enable_Add_Do_Loop_Info ) {
    // sigh, just do this to provide a dummy parm to createdo
    loop_info_wn = fake_info_wn;
    WN_set_opcode(loop_info_wn, OPC_LOOP_INFO);
  }

  rwn = WN_CreateDO(index,
		    init_stmtwn,
		    loop_cond,
		    step,
		    block_body, 
		    loop_info_wn);
  WN * orig_wn = bb_end->Loop()->Orig_wn();
  if (orig_wn)
    WN_COPY_All_Maps(rwn, orig_wn);
  else
    bb_end->Loop()->Set_orig_wn(rwn);

  WN_Set_Linenum(rwn, bb_start->Linenum());
  if (emitter->Cfg()->Feedback()) {  // NOTE: Use Orig_wn()?
    emitter->Cfg()->Feedback()->Emit_feedback( rwn, bb_end );
  }
  bb_end->Set_loopstmt(rwn);
  emitter->Connect_sr_wn( bb_end->Branch_stmtrep(), rwn );
  if(OPT_Enable_WHIRL_SSA) {
    emitter->WSSA_Emitter()->WSSA_Copy_PHI(bb_end, rwn);
  }
  emitter->Set_has_do_loop();

  // update the loop_info because we just used an old one to act
  // as a place-holder.
  if ( loop_info_wn != NULL ) {
    // no longer need fake place holder
    if ( loop_info_wn == fake_info_wn )
      loop_info_wn = NULL;
    WN_set_do_loop_info(rwn, Build_new_loop_info(rwn, loop_info_wn));
  }

  // create a new BLOCK if BB_INIT has more than one statements
  if (bb_start->Firststmt() != bb_start->Laststmt()) {
    WN *newblock = WN_CreateBlock();
    STMT_CONTAINER stmtcon(bb_start->Firststmt(), bb_start->Laststmt());
    stmtcon.Remove(init_stmtwn);
    stmtcon.Append(rwn);
    WN_first(newblock) = stmtcon.Head();
    WN_last(newblock) = stmtcon.Tail();
    rwn = newblock;
  }

  // fix bb_merge:  if bb_merge has a label and exactly one pred,
  // the bb_merge is only reachable from the DO-loop, so it is safe to
  // delete the label stmt.  Fix 315023.
  if (bb_merge->Label_stmtrep() != NULL && bb_merge->Pred()->Len() == 1)
    bb_merge->Remove_stmtrep( bb_merge->Label_stmtrep() );

  // set *bb to merge_bb, so we advance to it
  *bb = bb_merge;

#if defined(TARG_NVISA)
  --cur_loop_depth;
#endif
  return rwn;
}



static WN*
Raise_whiledo_stmt_to_whileloop(EMITTER *emitter, BB_NODE *bb, BB_NODE **next_bb)
{
  // The result WN node would be a statement 

  // Identify the major blocks.
  // These are assumptions made according to CFG construction:
  // end-block has two successor: body-block and merge-block
  // body-block has one successor: end-block
  // the prev/next sequence is: end-body-merge

  BB_LIST_ITER bb_iter;

  BB_NODE *bb_end = bb;
  FmtAssert(bb_end == bb_end->Loopend(), ("Wrong end"));

  BB_NODE *bb_body = bb_end->Loopbody();
  BB_NODE *loop_back = bb_end->Loopstep();
  BB_NODE *merge_bb = bb_end->Loopmerge();

  // do we need to get rid of the "goto" at the end of the loopback block?
  STMTREP *goto_end = loop_back->Branch_stmtrep();
  if ( goto_end != NULL ) {
    FmtAssert( goto_end->Op() == OPC_GOTO,
      ("Raise_whiledo_stmt: loop-back with non-goto branch") );
    FmtAssert( goto_end->Label_number() == bb_end->Labnam(),
      ("Raise_whiledo_stmt: loop-back to wrong location") );

    loop_back->Remove_stmtrep( goto_end );
  }

  // generate the code for the ending condition
  bb_end->Gen_wn(emitter, FALSE);

  // create OPC_BLOCK statement for while body
  WN *block_body = Create_block(emitter, bb_body, loop_back);
  WN_Set_Linenum(block_body, bb_end->Linenum());

  Is_True(WN_opcode(bb_end->Laststmt()) == OPC_FALSEBR,
	  ("Raise_whiledo_stmt: condition not a falsebr") );
  WN *cond = WN_kid0(bb_end->Laststmt());

  WN *rwn = WN_CreateWhileDo(cond, block_body);
  WN_Set_Linenum(rwn, bb_end->Linenum());
  if (emitter->Cfg()->Feedback()) {
    emitter->Cfg()->Feedback()->Emit_feedback( rwn, bb_end );
  }

  // This cannot be set for a non-do loop.  Otherwise, it
  // confuses the emitter!
  // bb_end->Loop()->Set_loopstmt(rwn);

  emitter->Connect_sr_wn( bb_end->Branch_stmtrep(), rwn );
  if(OPT_Enable_WHIRL_SSA) {
    emitter->WSSA_Emitter()->WSSA_Copy_PHI(bb_end, rwn);
  }


  // If bb_end contains more than one statement, create a new block to
  // contain the extra statements in the whileend block and the whiledo
  // statement.
  if (bb_end->Firststmt() != bb_end->Laststmt()) {
    WN *newblock = WN_CreateBlock();
    STMT_CONTAINER stmtcon(bb_end->Firststmt(), bb_end->Laststmt());
    stmtcon.Remove(bb_end->Laststmt());

    // duplicate the statements (non-label) into the end of the 
    // loop's body so they are executed at the end of every iteration 
    // as well.
    STMT_ITER stmt_iter( stmtcon.Head(), stmtcon.Tail() );
    WN *wnstmt;
    FOR_ALL_ELEM(wnstmt,stmt_iter,Init(stmtcon.Head(),stmtcon.Tail())) {
      if ( WN_opcode(wnstmt) == OPC_LABEL ) 
	continue;
      
      WN *newwn = WN_COPY_Tree(wnstmt);
      emitter->Duplicate_sr_cr_connections( wnstmt, newwn );
      emitter->Alias_Mgr()->Dup_tree_alias_id( wnstmt, newwn );
      WN_INSERT_BlockLast( block_body, newwn );
    }

    // add the loop statement to the list of statements
    stmtcon.Append(rwn);
    WN_first(newblock) = stmtcon.Head();
    WN_last(newblock) = stmtcon.Tail();
    rwn = newblock;
  }

  // fix bb_merge:  if bb_merge has a label and exactly one pred,
  // the bb_merge is only reachable from the WHILE-loop, so it is safe to
  // delete the label stmt.  Fix 315023.
  if (merge_bb->Label_stmtrep() != NULL && merge_bb->Pred()->Len() == 1)
    merge_bb->Remove_stmtrep( merge_bb->Label_stmtrep() );
    
  // set *bb to merge_bb
  *next_bb = merge_bb;

  return rwn;
}


static WN*
Raise_whiledo_stmt_to_doloop(EMITTER *emitter, BB_NODE *bb, BB_NODE *prev_bb, BB_NODE **next_bb)
{
  BB_NODE *header = bb;
  BB_NODE *preheader;
  BB_NODE *loopback;
  BB_NODE *loopbody;
  BB_NODE *loopmerge;

  BB_LOOP *loop = header->Loop();

  BB_LIST_ITER bb_pred_iter;
  BB_NODE *pred;
  INT count = 0;
  FOR_ALL_ELEM( pred, bb_pred_iter, Init(header->Pred()) ) {
    if (Is_backedge(pred, header)) {
      loopback = pred;
    } else {
      preheader = pred;
    }
    count++;
  }
  Is_True(count == 2, ("Raise_whiledo_stmt_to_doloop:"
		       " too many or too litter predecessor"));

  loopbody = header->Next();
  loopmerge = (header->Nth_succ(0) != header->Next())
              ? header->Nth_succ(0) : header->Nth_succ(1);
  Is_True(loopmerge, ("Loop merge is NULL"));

  // head BB must have phi nodes, if it has label, its must unused label
  // other wise it will not promote to do loop.
  // skip copy phi nodes to label, we need record phi nodes on do loop
  header->Gen_wn(emitter, FALSE);

  // Remove the init WN* and STMTREP* from the preheader block
  WN *init_stmt = preheader->Laststmt();
  if (WN_prev(init_stmt) != NULL)
    WN_next(WN_prev(init_stmt)) = NULL;

  Is_True(prev_bb->Laststmt() == init_stmt, 
	  ("Raise_whiledo_stmt_doloop: prev_bb and preheader didn't agree."));

  if (prev_bb->Firststmt() == prev_bb->Laststmt()) {
    // HACK HACK HACK
    //
    // Historical reason: prev_bb must be non-empty unless every BB before it
    // is also empty
    //   -Raymond  12/24/97.
    //
    // prev_bb->Set_firststmt(NULL);
    // prev_bb->Set_laststmt(NULL);
    
    prev_bb->Set_firststmt(WN_prev(init_stmt));
    prev_bb->Set_laststmt(WN_prev(init_stmt));
    
  } else 
    prev_bb->Set_laststmt(WN_prev(init_stmt));
  WN_prev(init_stmt) = WN_next(init_stmt) = NULL;

#ifdef KEY
  if (preheader->Last_stmtrep())
#endif
  preheader->Remove_stmtrep( preheader->Last_stmtrep());

  STMTREP *goto_end = loopback->Branch_stmtrep();
  if ( goto_end != NULL)
    loopback->Remove_stmtrep( goto_end );

#if defined(TARG_NVISA)
  ++cur_loop_depth;
#endif

  // generate code for the body of the loop
  WN *block_body = Create_block(emitter, loopbody, loopback);
  WN_Set_Linenum(block_body, header->Linenum());

  // Remove the step statement from the loopback block
  WN *incr_stmt = loopback->Laststmt();
  if (WN_first(block_body) != WN_last(block_body)) {
    WN_next(WN_prev(incr_stmt)) = NULL;
    WN_last(block_body) = WN_prev(incr_stmt);
  } else {
    DevWarn("EMITTER:  an empty loop is emitted.");
    WN_first(block_body) = WN_last(block_body) = NULL;
  }

  if (loopback->Firststmt() == loopback->Laststmt()) {
    loopback->Set_firststmt(NULL);
    loopback->Set_laststmt(NULL);
  } else 
    loopback->Set_laststmt(WN_prev(incr_stmt));

  loopback->Remove_stmtrep( loopback->Last_stmtrep());
  WN_next(incr_stmt) = WN_prev(incr_stmt) = NULL;

  ST *index_st = emitter->Opt_stab()->St(loop->Iv()->Aux_id());
  INT64 ofst = emitter->Opt_stab()->St_ofst(loop->Iv()->Aux_id());
  WN *index = WN_CreateIdname(ofst, index_st);

  WN *loop_info_wn = NULL;
  char fake_info_wn_area [sizeof (WN)];
  WN* fake_info_wn = (WN *) &fake_info_wn_area;
  if ( WOPT_Enable_Add_Do_Loop_Info ) {
    // sigh, just do this to provide a dummy parm to createdo
    loop_info_wn = fake_info_wn;
    WN_set_opcode(loop_info_wn, OPC_LOOP_INFO);
  }

  WN *loop_cond = WN_kid0(header->Laststmt());

  WN *rwn = WN_CreateDO(index,
			init_stmt,
			loop_cond,
			incr_stmt,
			block_body, 
			loop_info_wn);

  header->Set_loopstmt(rwn);
  header->Loop()->Set_loopstmt(rwn);

  emitter->Connect_sr_wn( header->Branch_stmtrep(), rwn );
  if(OPT_Enable_WHIRL_SSA) {
    emitter->WSSA_Emitter()->WSSA_Copy_PHI(header, rwn);
  }
  // fix bb_merge:  if bb_merge has a label and exactly one pred,
  // the bb_merge is only reachable from the DO-loop, so it is safe to
  // delete the label stmt.  Fix 315023.
  if (loopmerge->Label_stmtrep() != NULL && loopmerge->Pred()->Len() == 1)
    loopmerge->Remove_stmtrep( loopmerge->Label_stmtrep() );
  
  *next_bb = loopmerge;

  //  Why not use the preheader->Linenum?
  //  It has been like that in 7.2.
  WN_Set_Linenum(rwn, header->Linenum());
  if (emitter->Cfg()->Feedback()) {
    emitter->Cfg()->Feedback()->Emit_feedback( rwn, header );
  }
  header->Set_loopstmt(rwn);
  emitter->Set_has_do_loop();

  // A loop info may have been attached to the BB_LOOP by loop multiversion
  WN *old_loop_info =loop ? loopbody->Label_loop_info() : NULL;
  if (WOPT_Enable_Add_Do_Loop_Info)
    WN_set_do_loop_info(rwn, Build_new_loop_info(rwn,old_loop_info));

#if defined(TARG_NVISA)
  --cur_loop_depth;
#endif

  return rwn;
}


static WN*
Raise_whiledo_stmt(EMITTER *emitter, BB_NODE *bb, BB_NODE *prev_bb, BB_NODE **next_bb)
{
  WN *rwn;
   
  if (WOPT_Enable_While_Loop && 
      Can_raise_to_doloop(bb->Loop(), FALSE, emitter->Htable()))
    rwn = Raise_whiledo_stmt_to_doloop(emitter, bb, prev_bb, next_bb);
  else 
    rwn = Raise_whiledo_stmt_to_whileloop(emitter, bb, next_bb);

#ifdef KEY
  if (bb->Loop()->Orig_wn() != NULL &&
      WN_MAP32_Get(WN_MAP_FEEDBACK, bb->Loop()->Orig_wn()) != 0)
  {
    WN_CopyMap(rwn, WN_MAP_FEEDBACK, bb->Loop()->Orig_wn());
  }
#endif
  return rwn;
}


static WN*
Raise_dowhile_stmt(EMITTER *emitter, BB_NODE **bb)
{
  // The result WN node would be a statement 

  // Identify the major blocks.
  // These are assumptions made according to CFG construction:
  // body-block has loopinfo

  BB_NODE *bb_body = *bb;
  BB_NODE *bb_end = bb_body->Loopend();
  BB_NODE *bb_merge = bb_body->Loopmerge();

  FmtAssert( bb_end->Kind() == BB_REPEATEND,
    ("Raise_dowhile_stmt: repeatbody BB:%d has invalid end BB:%d",
     bb_body->Id(), bb_end->Id()) );

  bb_end->Gen_wn(emitter);

  // create OPC_BLOCK statement for dowhile body
  BB_KIND  old_kind = bb_body->Kind();
  bb_body->Set_kind(BB_GOTO);
  BB_NODE *bb_tmp = bb_body;
  // Workaround for 583092:  the first BB of the Create_block
  // cannot be empty.
  while (bb_tmp->First_stmtrep() == NULL && bb_tmp->Kind() == BB_GOTO && 
	 bb_tmp != bb_end)
    bb_tmp = bb_tmp->Next();
  WN *block_body = (bb_tmp != bb_end) ? 
    Create_block(emitter, bb_tmp, bb_end->Prev()) :
    Create_block(emitter, bb_end->Prev(), bb_end->Prev());
  
  WN_Set_Linenum(block_body, bb_end->Linenum());
  bb_body->Set_kind(old_kind);
  
  // set *bb to merge_bb
  *bb = bb_merge;

  // pull out the test from the TRUEBR statement, which is the only
  // one statement in bb_cond
  WN *rwn = WN_CreateDoWhile(WN_kid0(bb_end->Laststmt()),
			     block_body);
  WN_Set_Linenum(rwn, bb_end->Linenum());
  if (emitter->Cfg()->Feedback()) {
    emitter->Cfg()->Feedback()->Emit_feedback( rwn, bb_end );
  }
  emitter->Connect_sr_wn( bb_end->Branch_stmtrep(), rwn );
  if(OPT_Enable_WHIRL_SSA) {
    emitter->WSSA_Emitter()->WSSA_Copy_PHI(bb_body, rwn);
  }
  // Move the extra statements in BB_END to the body.
  if (bb_end->Firststmt() != bb_end->Laststmt()) {
    STMT_CONTAINER stmtcon(WN_first(block_body), WN_last(block_body));
    stmtcon.Append_list(bb_end->Firststmt(), bb_end->Laststmt());
    stmtcon.Remove(bb_end->Laststmt());
    WN_first(block_body) = stmtcon.Head();
    WN_last(block_body) = stmtcon.Tail();
  }

  return rwn;
}

static WN*
Raise_region_stmt(EMITTER *emitter, BB_NODE **bb)
{
  BB_NODE *region_start = *bb;
  BB_REGION *bb_region = region_start->Regioninfo();
  BB_NODE *region_end = bb_region->Region_end();

  // create OPC_BLOCK statement for region body
  WN *region_body;
  region_body = Create_block( emitter, region_start, region_end );

  WN *region_wn = WN_CreateRegion(REGION_type_to_kind(bb_region->Rid()),
				  region_body,
				  bb_region->Region_pragma_list(),
				  bb_region->Region_exit_list(),
				  RID_id(bb_region->Rid()),
				  bb_region->Ereg_supp());
  WN_COPY_All_Maps(region_wn, bb_region->Orig_wn());

  // go through region pragmas and convert aux_ids back to STs and offsets
  if (REGION_is_EH(region_wn))
    emitter->Opt_stab()->Convert_EH_pragmas(region_wn);

  WN_Set_Linenum(region_wn, region_start->Linenum());
  // Update Feedback?

  // update rid and level
  REGION_emit(bb_region->Rid(), region_wn, emitter->Cfg()->Rgn_level(),
	      bb_region->Region_num_exits(), bb_region->Region_line_num());

  // connect WN and STMTREP for DU
  // PPP this is a kludge !!! need a node to mark region_entry/func_entry
  emitter->Connect_sr_wn( region_start->Succ()->Node()->First_stmtrep(),
			 region_wn );
  if(OPT_Enable_WHIRL_SSA) {
    emitter->WSSA_Emitter()->WSSA_Copy_PHI( region_start, region_wn );
  }

  // progress onto the next block
  *bb = region_end->Next();

  return region_wn;
}


static WN*
Raise_unknown_stmt(EMITTER *emitter, BB_NODE **bb)
{
  FmtAssert(FALSE, ("opt_emit: Raise unknown statement"));
  return NULL;
}

typedef WN* (*RAISE_FUNC)(EMITTER *, BB_NODE **);
static RAISE_FUNC raise_func[] = {
  Raise_unknown_stmt /* BB_UNKNOWN */,
  Raise_unknown_stmt /* BB_GOTO */,
  Raise_if_stmt      /* BB_LOGIF */,
  Raise_unknown_stmt /* BB_VARGOTO */,
  Raise_unknown_stmt /* BB_ENTRY */,
  Raise_unknown_stmt /* BB_EXIT */,
  Raise_doloop_stmt  /* BB_DOSTART */,
  Raise_unknown_stmt /* BB_DOEND */,
  Raise_unknown_stmt /* BB_DOSTEP */,
  Raise_unknown_stmt /* BB_DOHEAD */,
  Raise_unknown_stmt /* BB_DOTAIL */,
  Raise_unknown_stmt /* BB_IO */,
  Raise_unknown_stmt /* BB_WHILEEND */,
  Raise_region_stmt  /* BB_REGIONSTART */,
  Raise_unknown_stmt /* BB_REGIONEXIT */,
  Raise_dowhile_stmt /* BB_REPEATBODY */,
  Raise_unknown_stmt /* BB_REPEATEND */,
  Raise_unknown_stmt /* BB_SUMMARY */
};


// ====================================================================
// preopt emitter
// ====================================================================
//
// EMITTER::Gen_wn
//
// It reconstruct the high level statements from the partially lowered
// form in the preopt phase.  It looks ahead one basic block.  If it
// is a candidate for raising, ie entry, if, do, while, repeat
// statements, it pulls out all related BBs to form the new statement
// and append to the end of the current WN list.
//
// ====================================================================

#include <malloc.h>
void
EMITTER::Gen_wn(BB_NODE *first_bb, BB_NODE *last_bb)
{
  BB_NODE *prev_bb = NULL;
  BB_NODE *bb = first_bb;
  BB_NODE *next_bb;
  WN      *stmt;

  while (bb != NULL) {
    switch (bb->Kind()) {
    case BB_LOGIF:
    case BB_DOSTART:
    case BB_REPEATBODY:
    case BB_WHILEEND:
      if (!Can_raise_to_scf(bb))
	goto ordinary;

      next_bb = bb;
      if (bb->Kind() == BB_WHILEEND) 
	stmt = Raise_whiledo_stmt(this, bb, prev_bb, &next_bb);
      else
	stmt = (*raise_func[bb->Kind()])(this, & next_bb);

      if ( WN_opcode(stmt) != OPC_BLOCK ) {
	bb->Init_stmt( stmt );
      } else {
	bb->Set_firststmt(WN_first(stmt));
	bb->Set_laststmt(WN_last(stmt));
	WN_Delete(stmt);// get rid of the block statement
      }
      stmt = NULL;	// don't let anyone else use this

      // connect the statement to the end of 'bb'
      if (prev_bb) {
	if (prev_bb->Laststmt() != NULL)
	  prev_bb->Connect_wn_list(bb->Firststmt(), bb->Laststmt());
	else {
	  prev_bb->Set_firststmt(bb->Firststmt());
	  prev_bb->Set_laststmt(bb->Laststmt());
	}
      }

      // Did the raising handle all of the blocks?
      if ( next_bb != NULL && next_bb->Prev() == last_bb ) {
	return;
      }

      // This forces the entry block to work properly
      prev_bb = bb;
      // Virtually, all BBs from 'bb' upto merged bb are deleted in WN
      // tree point of view
      bb = next_bb;

      break;

    case BB_ENTRY:
      // skip fake entries which have no wn for them
      if (bb->Entrywn()) {
	const OPCODE entry_opc = WN_opcode(bb->Entrywn());
	if ( entry_opc == OPC_FUNC_ENTRY ) {
          goto ordinary;
	}
	else if ( entry_opc == OPC_ALTENTRY ||
		  (entry_opc == OPC_LABEL && 
		   (WN_Label_Is_Handler_Begin(bb->Entrywn())
#ifdef KEY
		   || LABEL_target_of_goto_outer_block(WN_label_number(bb->Entrywn()))
#endif
		   )) )
	{
	  BOOL skip_curbb = Raise_altentry( bb );
	  // Connect all WN nodes in the merged bb to the end of 'bb'
          if (!skip_curbb) {
            bb->Gen_wn(this);
          }
	  if (prev_bb) {
	    if (prev_bb->Laststmt() != NULL) {
	      Is_True( WN_next(prev_bb->Laststmt()) == NULL, 
		("Next stmt of prev_bb last stmt is not NULL.") );
	      prev_bb->Connect_wn_list(bb->Firststmt(), bb->Laststmt());
	    } else {
	      prev_bb->Set_firststmt(bb->Firststmt());
	      prev_bb->Set_laststmt(bb->Laststmt());
	    }
	  }
	}
	else {
	  FmtAssert( FALSE, 
	    ("EMITTER::Gen_wn: unknown entry opcode: %s",
	     OPCODE_name(entry_opc)) );
	}
      }

      prev_bb = bb;
      bb = bb->Next();
      break;

    case BB_REGIONSTART:
    {
      BB_REGION *bb_region = bb->Regioninfo();
      // we want to emit MP and EH regions
      // also emit any transparent region when Preopt is called from IPA or LNO
      if (RID_TYPE_mp(bb_region->Rid()) || RID_TYPE_eh(bb_region->Rid()) ||
	  // kludge for 7.2, see pv 457243
	  RID_TYPE_olimit(bb_region->Rid()) || 
#if defined(TARG_SL) //region_type_for_major
	  RID_TYPE_sl2_para(bb_region->Rid()) ||
#endif 	  
	  RID_TYPE_pragma(bb_region->Rid())) {

	Is_True(bb_region->Region_start() == bb,
		("EMITTER::Gen_wn, regioninfo is incorrect"));

	Push_region(Region_stack(), bb, Loc_pool());
	BB_NODE *rstart = bb_region->Region_start();
	if (RID_TYPE_mp(bb_region->Rid())) {
	  rstart->Gen_wn(this); // generate pragmas
	  // for mp, generate a new pragma block, for others use the one saved
	  bb_region->Set_region_pragmas(Create_block_stmt(rstart,rstart));
	}
	Gen_wn(bb_region->Region_start()->Next(),
	       bb_region->Region_end()); // generate region body
	WN *first, *last;
	bb_region->Find_first_last_stmt(bb_region->Region_start()->Next(),
			       bb_region->Region_end(), &first, &last);
	stmt = Pop_region(Region_stack(), first, last,
			  Cfg()->Rgn_level(), Opt_stab());

	// normal case, region not empty or EH Guard region
	if (stmt) {
	  Is_True(WN_opcode(stmt) == OPC_REGION,
		  ("EMITTER::Gen_wn, pop_region did not return a region"));
	  bb->Init_stmt(stmt);
	  stmt = NULL; // don't let anyone else use this

	  // set the last statement of the region so Create_block_stmt
	  // gets it right.
	  bb_region->Region_end()->Set_laststmt(bb->Laststmt());

	  if (prev_bb) {
	    if (prev_bb->Laststmt() != NULL) {
	      Is_True( WN_next(prev_bb->Laststmt()) == NULL, 
		      ("Next stmt of prev_bb last stmt is not NULL.") );
	      prev_bb->Connect_wn_list(bb->Firststmt(), bb->Laststmt());
	    } else {
	      prev_bb->Set_firststmt(bb->Firststmt());
	      prev_bb->Set_laststmt(bb->Laststmt());
	    }
	  }
	} else {
	  // kill any pragmas emitted before we knew the region was empty
	  rstart->Set_firststmt(NULL);
	  rstart->Set_laststmt(NULL);
	  // region is empty, don't emit it.
	  RID_Delete2(bb_region->Rid());
	  bb = prev_bb;
	}

	if (bb_region->Region_end() == last_bb)
	  return;
	prev_bb = bb;
	bb = bb_region->Region_end()->Next();
     } else
	goto ordinary;
    }
      break;

    case BB_DOSTEP:
    case BB_EXIT:
    case BB_REGIONEXIT:
    case BB_GOTO:
    case BB_IO:
    case BB_VARGOTO:
    case BB_REPEATEND:
    case BB_DOEND:
    case BB_DOHEAD:
    case BB_DOTAIL:
    ordinary:
      if (bb->First_stmtrep()) {
	bb->Gen_wn(this);
      
	// Connect all WN nodes in the merged bb to the end of 'bb'
	if (prev_bb) {
	  if (prev_bb->Laststmt() != NULL) {
	    Is_True( WN_next(prev_bb->Laststmt()) == NULL, 
	      ("Next stmt of prev_bb last stmt is not NULL.") );
	    prev_bb->Connect_wn_list(bb->Firststmt(), bb->Laststmt());
	  } else {
	    prev_bb->Set_firststmt(bb->Firststmt());
	    prev_bb->Set_laststmt(bb->Laststmt());
#ifdef KEY // bug 1294
	    if (bb != last_bb && bb->Succ() && !bb->Succ()->Multiple_bbs() &&
		bb->Succ()->Node() == bb->Next()) { 
	      // only 1 successor: delete this BB
	      // fix up predecessor list of successor
	      BB_LIST *pred = bb->Next()->Pred();
	      while (pred->Node() != bb)
		pred = pred->Next();
	      pred->Set_node(prev_bb);

	      // fix up successor list of predecessor
	      prev_bb->Succ()->Set_node(bb->Next());

	      bb = bb->Next(); // delete this BB
	      break;
	    }
#endif
	  }
	}
      }
      else {
	bb->Set_wngend();
	if (prev_bb) { // This is an empty block, set the laststmt
	  bb->Set_laststmt(prev_bb->Laststmt());
	  bb->Set_firststmt(prev_bb->Firststmt());
	}
      }
      if (bb == last_bb)
	return;
      prev_bb = bb;
      bb = bb->Next();
      break;
    
    default:
      FmtAssert(FALSE, ("EMITTER::Gen_wn: Illegal BB_KIND "));
      break;
    }
  }
}

WN *
EMITTER::Emit(COMP_UNIT *cu, DU_MANAGER *du_mgr, 
	      ALIAS_MANAGER *alias_mgr )
{
  _htable = cu->Htable();
  _cfg = cu->Cfg();
  _opt_stab = cu->Opt_stab();
  _trace = Get_Trace(TP_GLOBOPT, EMIT_DUMP_FLAG);
  _alias_mgr = alias_mgr;
  _du_mgr = du_mgr;

  // initial wssa emitter
  if (OPT_Enable_WHIRL_SSA) {
    WSSA::WHIRL_SSA_MANAGER *wssa_mgr = PU_Info_ssa_ptr(Current_PU_Info);
    _wssa_emitter = CXX_NEW(WHIRL_SSA_EMITTER(wssa_mgr, _opt_stab, _wn_to_cr_map),
                            _mem_pool);
    if (Get_Trace(TP_WSSA, TT_WSSA_EMT_INOUT)) {
      _htable->Print(TFile);
      _cfg->Print(TFile);
    }
    if (Get_Trace(TP_WSSA, TT_WSSA_EMT)) {
      _wssa_emitter->Set_trace(TRUE);
      fprintf(TFile, "Entering WSSA Emitter\n");
    }
    if (Get_Trace(TKIND_INFO, TINFO_TIME)) {
      _wssa_emitter->Set_trace_time(TRUE);
    }
  }

  // do we have valid loop-body information, which is necessary for
  // some of the checks
  Cfg()->Analyze_loops();
  
  {
    CFG_ITER cfg_iter;
    BB_NODE *bb;
    FOR_ALL_ELEM (bb, cfg_iter, Init(Cfg())) {
      if (bb->Kind() == BB_WHILEEND && WOPT_Enable_While_Loop) {
	if (Can_raise_to_doloop(bb->Innermost(), TRUE/*repair*/, Htable())) {
	  if (_trace)
	    fprintf(TFile, "EMIT: BB%d can raise to do-loop\n", bb->Id());
	} else {
	  if (_trace)
	    fprintf(TFile, "EMIT: BB%d cannot raise to do-loop\n", bb->Id());
	}
      } else if (bb->Kind() == BB_DOEND) {
	Fix_do_loop(bb->Loop(), Htable());
      }
    }
  }

  // prepare for the def-use construction
  du_mgr->Set_alias_mgr(alias_mgr);

  // convert opt_stab's vsym into symtab
  if(OPT_Enable_WHIRL_SSA) {
    _wssa_emitter->WSSA_Convert_OPT_Symbol();
  }
  
  // generate all of the Whirl
  Raise_func_entry(Cfg()->Func_entry_bb(), Cfg()->Last_bb());

  if (OPT_Enable_WHIRL_SSA) {
    _wssa_emitter->WSSA_Build_MU_CHI_Version(_opt_func);
    if (_wssa_emitter->Get_trace()) {
      fprintf(TFile, "Exiting WSSA Emitter\n");
    }
    CXX_DELETE(_wssa_emitter, _mem_pool);
    if (Get_Trace(TP_WSSA, TT_WSSA_EMT_INOUT)) {
      fprintf(TFile, "WSSA sym table and WN tree after WSSA Emitter\n");
      WSSA::WHIRL_SSA_MANAGER *wssa_mgr = PU_Info_ssa_ptr(Current_PU_Info);
      wssa_mgr->Print_wst_table(TFile);
      fdump_tree(TFile, _opt_func);
    }
  }

  // construct the def-use information after all Whirl generated
  Compute_use_def(du_mgr);

  // Verify that all PHI visted bit are reset
#ifdef Is_True_On
  {
    CFG_ITER cfg_iter;
    BB_NODE *bb;
    FOR_ALL_ELEM (bb, cfg_iter, Init(Cfg())) {
      PHI_NODE *phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
	Is_True(!phi->Visited(), ("EMITTER::PHI visted bit is not reset."));
      }
    }
  }
#endif

  // tell everyone that the generated WHIRL has feedback info if it
  // does
  if (Cur_PU_Feedback) {
    Set_PU_Info_state(Current_PU_Info, WT_FEEDBACK, Subsect_InMem);
  }

  Is_True(Get_Preg_Num(PREG_Table_Size(CURRENT_SYMTAB)) ==
	  Opt_stab()->Last_preg(),
	  ("EMITTER:Emit, incorrect last preg number"));

#if !defined(TARG_NVISA)
  {
    BOOL tr = _trace || Get_Trace (TP_GLOBOPT, ALIAS_DUMP_FLAG);
    if (Opt_stab()->Phase() == PREOPT_LNO_PHASE
        || Opt_stab()->Phase() == PREOPT_LNO1_PHASE) {
      Opt_stab()->Cr_sr_annot_mgr()->Export_annot 
                     (_opt_func, alias_mgr, FALSE, tr);
    } else {
      Opt_stab()->Cr_sr_annot_mgr()->Discard_offline_annot 
                     (_opt_func, alias_mgr, tr);
    }
    WN_MEMOP_ANNOT_MGR::WN_mem_annot_mgr()->Set_active_mgr();  
  }
#endif

  Verify(_opt_func);
  
  if (_trace)  {
    fprintf( TFile, "%sAfter EMITTER\n%s", DBar, DBar );
    if (! Cur_PU_Feedback) {
      IR_dump_map_info = TRUE;
      fdump_tree( TFile, _opt_func);
    }
    else fdump_tree_with_freq( TFile, _opt_func, WN_MAP_FEEDBACK);
  }

  return _opt_func;
}


// Verify the assumptions used in the raising function.
// Insert the required labels/gotos.
//
//   ***** THIS SHOULD BE THROW AWAY AFTER CFG IS CLEANUP UP.
//   ***** JUST HACK IT TO MAKE WOPT WORKS.
//
BOOL
EMITTER::Can_raise_to_scf(BB_NODE *bb)
{
  Warn_todo("Cleanup gotos and labels in CFG.");
  BB_LIST_ITER bb_pred_iter;
  switch (bb->Kind()) {
    BB_NODE *bb_start;
    BB_NODE *bb_body;
    BB_NODE *bb_end;
    BB_NODE *bb_step;
    BB_NODE *bb_merge;
    STMTREP *new_goto;
    BB_NODE *pred;
  case BB_LOGIF:
    if (bb->Ifinfo() != NULL &&
	bb->Succ()->Contains(bb->If_then()) &&
	bb->Succ()->Contains(bb->If_else())) 
      return TRUE;
    if (bb->Ifinfo() != NULL && bb->If_merge() != NULL) {
      if (bb->If_merge()->Label_stmtrep() == NULL) {
	if (bb->If_merge()->Labnam() == 0)
	  bb->If_merge()->Set_labnam( _cfg->Alloc_label());
	bb->If_merge()->Add_label_stmtrep(Mem_pool());
      }
      FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->If_merge()->Pred())) {
	if (pred->Branch_stmtrep() == NULL) {
	  new_goto = CXX_NEW( STMTREP(OPC_GOTO), Mem_pool() );
	  new_goto->Init_Goto( NULL, bb->If_merge()->Labnam(), 0 );
	  pred->Append_stmtrep( new_goto );
	}
      }
    }
    break;
  case BB_REGIONSTART:
    return TRUE;
  case BB_DOSTART:
    bb_start = bb;
    bb_end = bb_start->Loopend();
    bb_step = bb_start->Loopstep();
    bb_merge = bb_start->Loopmerge();
#ifdef KEY // bug 8327: the incr stmt has been optimized to something else
    // bugs 13605, 13624: A DOSTEP originally contains the step WN followed
    // by a goto to the DOEND. DCE sometimes introduces a label at the start,
    // and sometimes is not able to delete the goto. So the actual STEP wn
    // may be neither the head nor the tail of the stmtlist. If DCE is later
    // found to do more changes to the DOSTEP bb, then we should just scan
    // the entire bb for the STEP wn.
    if (bb_step->Stmtlist()->Tail() == NULL ||
	((bb_step->Stmtlist()->Tail()->Opr() != OPR_STID ||
	  bb_step->Stmtlist()->Tail()->Rhs()->Kind() != CK_OP) &&
	 // Now check the previous statement, if any.
	 (bb_step->Stmtlist()->Tail()->Prev() == NULL ||
	  bb_step->Stmtlist()->Tail()->Prev()->Opr() != OPR_STID ||
	  bb_step->Stmtlist()->Tail()->Prev()->Rhs()->Kind() != CK_OP)))
      ;
    else
#endif
    if (bb_step->Succ()->Contains(bb_end) &&
	bb_end->Succ()->Contains(bb_merge))
      return TRUE;
    
    new_goto = CXX_NEW( STMTREP(OPC_GOTO), Mem_pool() );
    new_goto->Init_Goto( NULL, bb_end->Labnam(), 0 );
    bb_step->Append_stmtrep( new_goto );
    if (bb_end->Label_stmtrep() == NULL) {
      if (bb_end->Labnam() == 0)
	bb_end->Set_labnam( _cfg->Alloc_label());
      bb_end->Add_label_stmtrep(Mem_pool());
    }
    if (bb_merge->Label_stmtrep() == NULL) {
      if (bb_merge->Labnam() == 0)
	bb_merge->Set_labnam( _cfg->Alloc_label());
      bb_merge->Add_label_stmtrep(Mem_pool());
    }
    break;
  case BB_WHILEEND:
    bb_end = bb;
    bb_body = bb_end->Loopbody();
    bb_merge = bb_end->Loopmerge();
    if (bb_end->Succ()->Contains(bb_body) && 
	bb_end->Succ()->Contains(bb_merge) &&
	bb_end->Next() == bb_body)
      return TRUE;
    if (bb_end->Label_stmtrep() == NULL) {
      if (bb_end->Labnam() == 0)
	bb_end->Set_labnam( _cfg->Alloc_label());
      bb_end->Add_label_stmtrep(Mem_pool());
    }
    if (bb_merge->Label_stmtrep() == NULL)  {
      if (bb_merge->Labnam() == 0)
	bb_merge->Set_labnam( _cfg->Alloc_label());
      bb_merge->Add_label_stmtrep(Mem_pool());
    }
    FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb_merge->Pred())) {
      if (pred->Branch_stmtrep() == NULL) {
	new_goto = CXX_NEW( STMTREP(OPC_GOTO), Mem_pool() );
	new_goto->Init_Goto( NULL, bb_merge->Labnam(), 0 );
	pred->Append_stmtrep( new_goto );
      }
    }
    break;
  case BB_REPEATBODY:
    bb_body = bb;
    bb_end = bb_body->Loopend();
    bb_merge = bb_body->Loopmerge();
    if (bb_end->Succ()->Contains(bb_body) && 
	bb_end->Succ()->Contains(bb_merge) &&
	bb_end->Next() == bb_merge)
      return TRUE;
    if (bb_end->Label_stmtrep() == NULL) {
      if (bb_end->Labnam() == 0)
	bb_end->Set_labnam( _cfg->Alloc_label());
      bb_end->Add_label_stmtrep(Mem_pool());
    }
    break;
  }
  return FALSE;
}

// ====================================================================
// Find the first and last statements in a region
// ====================================================================
void
BB_REGION::Find_first_last_stmt(BB_NODE *first_bb, BB_NODE *last_bb, 
				WN **first_stmt, WN **last_stmt)
{
  BB_NODE *bbtmp;

  // Hack to find the last_bb when a REGION has return or STOP intrinsic
  // Also set the this->Region_end() to fool the rest of the emitter!!!
  //  -Raymond 12/14/98.
  if (last_bb == NULL) {
   BB_NODE *bb = first_bb;
   while (bb && bb->Rid_id() == first_bb->Rid_id()) {
     last_bb = bb;
     bb = bb->Next();
   }
   DevWarn("fixing region %d with first_bb BB%d to have region_end BB%d\n",
	   first_bb->Rid_id(),first_bb->Id(), last_bb->Id());
   Set_region_end(last_bb);
  }

  Is_True(first_bb != NULL && last_bb != NULL,
	  ("BB_REGION::Find_first_last_stmt, NULL BBs"));

  if (first_bb->Firststmt()) // common case
    *first_stmt = first_bb->Firststmt();
  else {	// look down a few BBs for the first statement
    *first_stmt = NULL;
    bbtmp = first_bb;
    do {
      if (bbtmp->Firststmt()) {
	*first_stmt = bbtmp->Firststmt();
	break;
      }
      // control flow splits aren't allowed when going down - there
      // should be code before every split
      Is_True(bbtmp->Succ()->Next() == NULL,
	   ("BB_REGION::Find_first_last_stmt, unexpected control flow split"));
      if (bbtmp == last_bb)
	break;
      bbtmp = bbtmp->Next(); // go down in code emit order
    } while (1);
  }

  if (last_bb->Last_stmtrep()) // common case
    *last_stmt = last_bb->Laststmt();
  else {	// look up a few BBs for the last statement
    *last_stmt = NULL;
    bbtmp = last_bb;
    do {
      if (bbtmp->Laststmt()) {
	*last_stmt = bbtmp->Laststmt();
	break;
      }
      // control flow splits are allowed when going up - there
      // may be a merge block with nothing in it.
      if (bbtmp == first_bb)
	break;
      bbtmp = bbtmp->Prev(); // go up in reverse code emit order
    } while (1);
  }
}


// Verify emitter output
//
BOOL
EMITTER::Verify(WN *wn)
{
  INT32 i;
  WN *stmt;

  if (wn == NULL)
    return TRUE;

  if (WN_opcode(wn) == OPC_BLOCK)  {
    if (WN_first(wn) && WN_prev(WN_first(wn))) {
      fprintf(stderr, "### EMIT::Verify, WN_first has prev stmt.\n");
      fprintf(TFile, "### EMIT::Verify, WN_first has prev stmt.\n");
      fdump_tree(TFile,wn);
      fprintf(TFile, "###  prev stmt is\n");
      fdump_tree(TFile,WN_prev(WN_first(wn)));
    }
    if (WN_last(wn) && WN_next(WN_last(wn))) {
      fprintf(stderr, "### EMIT::Verify, WN_last has next stmt.\n");
      fprintf(TFile, "### EMIT::Verify, WN_last has next stmt.\n");
      fdump_tree(TFile,wn);
      fprintf(TFile, "###  next stmt is\n");
      fdump_tree(TFile,WN_next(WN_last(wn)));
    }
    for (stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))  
      Verify(stmt);
  } else if (WN_opcode(wn) == OPC_FUNC_ENTRY) {
    // skip the pragmas and varrefs
    Verify(WN_func_body(wn));
  } else
    for (i = 0; i < WN_kid_count(wn); i++) 
      Verify(WN_kid(wn,i));
  return TRUE;
}

EMITTER::EMITTER(MEM_POOL *lpool, MEM_POOL *gpool, OPT_PHASE opt_phase):
	_loc_pool(lpool),
	_mem_pool(gpool),
	_opt_phase(opt_phase),
	_region_stack(lpool),
	_preg_renumbering_map(128, 0, lpool, FALSE)
{
  _cfg = NULL;
  _htable = NULL;
  _opt_stab = NULL;
  _opt_func = NULL;
  _rgn_entry_stmt = NULL;
  _wssa_emitter = NULL;
  _has_do_loop = FALSE;

  // create a mapping from WN to CODEREP/STMTREP
  _wn_to_cr_map  = WN_MAP_Create(Mem_pool());
}

EMITTER::~EMITTER(void)
{
  // no longer need the map
  WN_MAP_Delete( _wn_to_cr_map );
}
