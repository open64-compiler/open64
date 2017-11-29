/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005-2008 Simplight NanoElectronics Ltd.  All rights reserved.
 */

/*
 * Copyright 2005-2007 NVIDIA Corporation.  All rights reserved.
 */

//-*-c++-*-

/*
 *  Copyright (C) 2007 QLogic Corporation.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_cfg.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_cfg.cxx,v $
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
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_cfg_CXX	"opt_cfg.cxx"
static char *rcs_id = 	opt_cfg_CXX"$Revision: 1.30 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "wintrinsic.h"
#include "opcode.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "config_targ.h"
#include "config_opt.h"		// For Instrumentation_Enabled
#include "region_util.h"
#include "fb_whirl.h"		// For TP_FEEDBACK_CFG
#include "opt_cfg.h"
#include "opt_exc.h"
#include "opt_htable.h"
#include "opt_util.h"
#include "opt_wn.h"
#include "opt_config.h"
#include "opt_alias_class.h"
#include "bb_node_set.h"
#include "opt_ssa.h"
#include "opt_tail.h"
#include "w2op.h"
#include "wn_tree_util.h"
#include "cxx_hash.h"
CFG::CFG(MEM_POOL *pool, MEM_POOL *lpool)
     : _bb_vec(pool),
       _entry_vec(pool),
       _exit_vec(pool),
       _notreach_vec(pool),
       _agoto_pred_vec(pool),
       _agoto_succ_vec(pool),
       _mp_rid(pool),
       _mp_type(pool),
#if defined(TARG_SL) //PARA_EXTENSION
       _sl2_para_rid(pool),
       _sl2_para_type(pool),
#endif
       _bb_region(pool),
       _eh_rid(pool)
{
  _mem_pool = pool;
  _loc_pool = lpool;
  _sc_pool = NULL;
  _bb_vec.Alloc_array(CFG_BB_TAB_SIZE);
  _entry_vec.Alloc_array(CFG_ALTENTRY_TAB_SIZE);
  _exit_vec.Alloc_array(CFG_EARLYEXIT_TAB_SIZE);
  _notreach_vec.Alloc_array(CFG_EARLYEXIT_TAB_SIZE);
  _agoto_pred_vec.Alloc_array(CFG_BB_TAB_SIZE);  // TODO: NEW CONSTANT HERE?
  _agoto_succ_vec.Alloc_array(CFG_BB_TAB_SIZE);  // TODO: NEW CONSTANT HERE?
  _dfs_vec = NULL;
  _dfs_vec_sz = 0;
  _po_vec = NULL;
  _po_vec_sz = 0;
  _dpo_vec = NULL;
  _dpo_vec_sz = 0;
  _pdo_vec = NULL;
  _pdo_vec_sz = 0;
  _entry_bb = NULL;
  _exit_bb = NULL;
  _first_bb = NULL;
  _last_bb = NULL;
  _fake_entry_bb = NULL;
  _fake_exit_bb = NULL;
  _loops = NULL;
  _exc = NULL;
  _bb_set = NULL;
  _non_true_body_set = NULL;
  _current_bb = NULL;
  _first_bb_id = _last_bb_id = _bb_vec.Newidx();
  _label_map = CXX_NEW(MAP(CFG_LAB_HASH_SIZE, pool), pool);
  _orig_last_label = _last_label_num = 0;
  _cur_loop_depth = 0;
  _lower_fully = FALSE;
  _calls_break = TRUE;
  _rvi_break_stmt = FALSE;	// default until we get rid of it
  _feedback = NULL;
  _rid = NULL;
  _rgn_level = RL_UNKNOWN;
  _has_regions = FALSE;
  _dohead_cnt = 0;
  WN_Simplifier_Enable(FALSE);
  _trace = Get_Trace(TP_GLOBOPT, CFG_DUMP_FLAG);
  _loops_valid = FALSE;
  _clone_map = NULL;
}

CFG::~CFG()
{
  // free up the memory in _bb_vec, _entry_vec, _exit_vec, _notreach_vec,
  //                       _agoto_pred_vec and _agoto_succ_vec
  _bb_vec.Free_array();
  _entry_vec.Free_array();
  _exit_vec.Free_array();
  _notreach_vec.Free_array();
  _agoto_pred_vec.Free_array();
  _agoto_succ_vec.Free_array();

}

// ====================================================================
// Creates a new bb to sequentially follow the previous current_bb
// and connects them by control-flow if "connect" is TRUE.
// ====================================================================

BB_NODE *
CFG::New_bb( BOOL connect, BB_KIND kind /*=BB_GOTO*/ )
{
  BB_NODE *newbb = Create_bb(kind);
  if ( connect )
    Connect_predsucc( _current_bb, newbb );
  Append_bb( newbb );

  if (Inside_mp_do()) newbb->Set_MP_region();
  return newbb;
}

// ====================================================================
// Is this block not one that we always want to keep around
// ====================================================================

BOOL
CFG::Removable_bb( const BB_NODE *bb ) const
{
  if ( bb == Fake_entry_bb() || bb == Fake_exit_bb())
    return FALSE;

  return TRUE;
}

// ====================================================================
// Get rid of the block completely (see Remove_block)
// ====================================================================

void
CFG::Remove_bb(BB_NODE *bb)
{
  Is_True( Removable_bb( bb ),
    ("CFG::Remove_bb: trying to remove unremovable bb:%d", bb->Id()) );

  // disconnect from our preds and succs if not done earlier
  BB_LIST_ITER bb_ps_iter;
  BB_NODE *succ,*pred;
  FOR_ALL_ELEM( succ, bb_ps_iter, Init(bb->Succ()) ) {
    succ->Remove_pred( bb, Mem_pool() );
  }
  FOR_ALL_ELEM( pred, bb_ps_iter, Init(bb->Pred()) ) {
    pred->Remove_succ( bb, Mem_pool() );
  }

  if (bb->EH_region() && bb->Rid()) {
    // If bb is in a EH region, walk up Prev() link to find the region start.
    // If bb is the region end, make its Prev() node to be the new
    // region end.
    INT id = RID_id(bb->Rid());
    STACK<INT> * stk = CXX_NEW(STACK<INT> (Mem_pool()), Mem_pool());
    stk->Push(id);
    BB_NODE * bb_iter = bb;
    while (bb_iter && bb_iter->Rid() && !stk->Is_Empty()) {
      INT iter_id = RID_id(bb_iter->Rid());
      if (stk->Top() != iter_id) {
	stk->Push(iter_id);
      }
      else if (bb_iter->Kind() == BB_REGIONSTART) {
	BB_REGION * region = bb_iter->Regioninfo();
	if (region && (region->Region_end() == bb)) {
	  BB_NODE * bb_prev = bb->Prev();
	  region->Set_region_end(bb_prev);
	}
	stk->Pop();
      }
      bb_iter = bb_iter->Prev();
    }
    CXX_DELETE(stk, Mem_pool());
  }

  if (bb->Is_first())
    _first_bb = bb->Next();
  if (bb->Is_last())
    _last_bb = bb->Prev();

  // is it in our list of exit blocks?
  for (INT i=0; i <= _exit_vec.Lastidx(); i++) {
    if ( _exit_vec[i] == bb ) {
      // rather than compress the vector, just get rid of the entry
      _exit_vec[i] = NULL;
    }
  }

  // rather than compress the vector, just get rid of the entry
  _bb_vec[bb->Id()] = NULL;

  // To help detect errors
  bb->Set_dom_dfs_id(0);
  bb->Set_pdom_dfs_id(0);

  bb->Remove();

  // Update feedback
  if ( Feedback() ) {
    Feedback()->Delete_node( bb->Id() );
  }

}


// ====================================================================
// Get rid of the block completely (see Remove_block)
// ====================================================================

void
CFG::Remove_path(BB_NODE *pred, BB_NODE *succ)
{
  succ->Remove_phi_reference( succ->Pred()->Pos(pred) );
  pred->Remove_succ( succ, Mem_pool() );
  succ->Remove_pred( pred, Mem_pool() );

  if ( Trace() ) {
    fprintf( TFile, "CFG::Remove_path: Removed bb:%d->bb:%d\n",
	     pred->Id(), succ->Id() );
  }

  // Note: Feedback is updated in context, not here
}

void
CFG::Delete_bbs(BB_LIST *bbs, MOD_PHI_BB_CONTAINER *mod_phis)
{
  BB_NODE *last_bb, *succ, *pred;
  BB_NODE *bb;
  BB_LIST_ITER bb_iter, bb_succ_iter, bb_pred_iter;
 
  FOR_ALL_ELEM( bb, bb_iter, Init(bbs) )
    last_bb = bb;
//  last_bb = bbs->Node();

  INT num_succs = last_bb->Succ()->Len();
  FmtAssert( num_succs <= 1,
    ("CFG::Delete_bb: trying to delete BB%d with %d succs",
     last_bb->Id(), num_succs) );

  mINT32 *pos_array = NULL;
  if ( num_succs > 0 ) {
    pos_array = TYPE_OPT_POOL_ALLOC_N( mINT32, Loc_pool(), num_succs, -1 );
    INT isucc = 0;
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(last_bb->Succ()) ) {
      Is_True( isucc < num_succs,
	("CFG::Delete_bb: wrong number of successors") );
      pos_array[isucc] = succ->Pred()->Pos(last_bb);
      isucc++;
    }
  }
  INT32 pred_num = 0;

  FOR_ALL_ELEM( bb, bb_iter, Init(bbs) ){
    FOR_ALL_ELEM( pred, bb_pred_iter, Init( bb->Pred() ) ) {
      if (bbs->Contains(pred))
        continue;
      pred_num ++;
    }
  }

  if (pred_num == 0) {
    // no pred. This can happen such as break inside loops
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(last_bb->Succ()) ) {
      succ->Remove_phi_reference(succ->Pred()->Pos(last_bb));
      succ->Remove_pred(last_bb, _mem_pool);
    }
  }

  PHI_LIST *preserved_philist = NULL;
  BB_NODE  *unique_succ = NULL;
  if (last_bb->Succ()->Len() == 1 &&
      last_bb->Succ()->Node()->Pred()->Len() == 1 &&
      last_bb->Succ()->Node()->Phi_list()->Is_Empty() &&
      last_bb->Phi_list() != NULL) {
    unique_succ = last_bb->Succ()->Node();

    preserved_philist = last_bb->Phi_list()->Dup_phi_node(_mem_pool, last_bb);
    PHI_NODE *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM ( phi, phi_iter, Init(preserved_philist)) {
      phi->Set_bb(unique_succ);
    }
    mod_phis->Add_entry(unique_succ, unique_succ->Phi_list(),
			preserved_philist, _mem_pool);
    unique_succ->Set_phi_list(preserved_philist);
  }

  FOR_ALL_ELEM( bb, bb_iter, Init(bbs) ){
    FOR_ALL_ELEM( pred, bb_pred_iter, Init( bb->Pred() ) ) {
      if (bbs->Contains(pred))
        continue;
      INT succnum = 0;
      FOR_ALL_ELEM( succ, bb_succ_iter, Init(last_bb->Succ()) ) {
        if (succ->Pred()->Contains(pred)) {
	  succ->Remove_phi_reference(succ->Pred()->Pos(last_bb));
	  succ->Remove_pred(last_bb, _mem_pool);
        }
        else {
          BOOL added = FALSE;
	  BB_LIST *tmp;
	  for (tmp = succ->Pred(); tmp != NULL; tmp = tmp->Next()) {
	    if (tmp->Node() == last_bb) {
	      tmp->Set_node(pred);
	      added = TRUE;
	      break;
	    }
	  }
	  if ( !added ) {
	    PHI_LIST *new_philist;
	    succ->Append_pred(pred, _mem_pool); 
	    new_philist = succ->Phi_list()->Dup_phi_node(_mem_pool, succ,
						       pos_array[succnum]);
            mod_phis->Add_entry(succ, succ->Phi_list(), new_philist, _mem_pool);
	    succ->Set_phi_list(new_philist);
	  }
        }
        succnum++;
      }
    }
  }

//  if (unique_succ) {
//    mod_phis->Add_entry(unique_succ, unique_succ->Phi_list(),
//			preserved_philist, _mem_pool);
//  }

  FOR_ALL_ELEM( succ, bb_succ_iter, Init(last_bb->Succ()) ) {
    FOR_ALL_ELEM( bb, bb_iter, Init(bbs) ){
      FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
        if (bbs->Contains(pred))
          continue;
        BB_LIST *tmp;
        BOOL added = FALSE;
        if (pred->Succ()->Contains(succ)) {
	  pred->Remove_succ(bb, _mem_pool);
        }
        else {
	  for (tmp = pred->Succ(); tmp != NULL; tmp = tmp->Next()) {
	    if (tmp->Node() == bb) {
	      tmp->Set_node(succ);
	      added = TRUE;
	      break;
	    }
	  }
	  if (!added) 
	    pred->Append_succ(succ, _mem_pool); 
        }	
      }
    }
  }
  FOR_ALL_ELEM( bb, bb_iter, Init(bbs) ){
    if ( Feedback() && bb->Succ()->Len() == 1 )
      Feedback()->Move_incoming_edges_dest( bb->Id(), bb->Succ()->Node()->Id() );
    Remove_bb(bb);
  }
/*
  while (bbs->Len() > 1){
    FOR_ALL_ELEM( bb, bb_iter, Init(bbs) ) 
      last_bb = bb;
    if ( Feedback() && last_bb->Succ()->Len() == 1 )
      Feedback()->Move_incoming_edges_dest( last_bb->Id(), last_bb->Succ()->Node()->Id() );
    Remove_bb(last_bb);
    bbs->Remove(last_bb, _mem_pool);
  }
  // The bbs is allocated on _loc_pool and it's already deleted at 
  //     removable_bb_chain(), opt_dce.cxx:4632
  // Refer removable_bb_chain(), opt_dce.cxx:4659
  CXX_DELETE(bbs, _mem_pool);
*/

}

// ====================================================================
// Remove_block - undo the internal linkage to remove this block
//                when phi_list is replaced, journal in mod_phis list
// ====================================================================

void
CFG::Delete_bb(BB_NODE *bb, MOD_PHI_BB_CONTAINER *mod_phis)
{
  BB_NODE *succ, *pred;
  BB_LIST_ITER bb_succ_iter, bb_pred_iter;
  INT num_succs = bb->Succ()->Len();

  // Fix 629536:  remove edges of self-loop
  if (num_succs > 1) {
    FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
      if (pred == bb) {
	Remove_path(pred, bb);
	num_succs = bb->Succ()->Len(); 
      }
    }
    if ( Feedback() )
      Feedback()->Delete_edge( bb->Id(), bb->Id() );
  }

  FmtAssert( num_succs <= 1,
    ("CFG::Delete_bb: trying to delete BB%d with %d succs",
     bb->Id(), num_succs) );

  // for all of the successors, we need to save this bb's position
  // in their predecessor list.
  mINT32 *pos_array = NULL;
  if ( num_succs > 0 ) {
    pos_array = TYPE_OPT_POOL_ALLOC_N( mINT32, Loc_pool(), num_succs, -1 );
    INT isucc = 0;
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
      Is_True( isucc < num_succs,
	("CFG::Delete_bb: wrong number of successors") );
      pos_array[isucc] = succ->Pred()->Pos(bb);
      isucc++;
    }
  }

  if (bb->Pred() == NULL) {
    // no pred. This can happen such as break inside loops
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
      succ->Remove_phi_reference(succ->Pred()->Pos(bb));
      succ->Remove_pred(bb, _mem_pool);
    }
  }

  PHI_LIST *preserved_philist = NULL;
  BB_NODE  *unique_succ = NULL;
  // Save the dead phi phis in bb and transfer them to succ.
  if (bb->Succ()->Len() == 1 &&
      bb->Succ()->Node()->Pred()->Len() == 1 &&
      bb->Succ()->Node()->Phi_list()->Is_Empty() &&
      bb->Phi_list() != NULL) {
    unique_succ = bb->Succ()->Node();
    preserved_philist = bb->Phi_list()->Dup_phi_node(_mem_pool, bb, 0);
    PHI_NODE *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM ( phi, phi_iter, Init(preserved_philist)) {
      phi->Set_bb(unique_succ);
    }
  }

  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
    if (pred == bb)
      continue;

    INT succnum = 0;
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {

      // link up the succ of bb to be the succ of bb's pred 
      if (succ->Pred()->Contains(pred)) {
	// the link is already there, cleanup phi-node and succ/pred
	succ->Remove_phi_reference(succ->Pred()->Pos(bb));
	succ->Remove_pred(bb, _mem_pool);
      }
      else { // link them up, be careful to preserve the
	// right order so the phi-pos is the same.
        BOOL added = FALSE;
	BB_LIST *tmp;
	for (tmp = succ->Pred(); tmp != NULL; tmp = tmp->Next()) {
	  if (tmp->Node() == bb) {
	    tmp->Set_node(pred);
	    added = TRUE;
	    break;
	  }
	}
	// link to bb has been removed due to prev round, append pred
	// and the version of var. to each phi in the phi-list
	if ( !added ) {
	  PHI_LIST *new_philist;
	  succ->Append_pred(pred, _mem_pool); 
	  new_philist = succ->Phi_list()->Dup_phi_node(_mem_pool, succ,
						       pos_array[succnum]);
          mod_phis->Add_entry(succ, succ->Phi_list(), new_philist, _mem_pool);
	  succ->Set_phi_list(new_philist);
	}
      }
      succnum++;
    }
  }

  if (unique_succ) {
    Is_Trace(Trace(), (TFile,"CFG::Delete_bb: preserve dead phi from "
		       "BB%d to BB%d\n", bb->Id(), unique_succ->Id()));
    mod_phis->Add_entry(unique_succ, unique_succ->Phi_list(),
			preserved_philist, _mem_pool);
    unique_succ->Set_phi_list(preserved_philist);
  }

  FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {

    FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {

      // link up the succ of bb to be the succ of bb's pred 
      BB_LIST *tmp;
      BOOL added = FALSE;
      
      if (pred->Succ()->Contains(succ)) {
	  pred->Remove_succ(bb, _mem_pool);
      }
      else {
	for (tmp = pred->Succ(); tmp != NULL; tmp = tmp->Next()) {
	  if (tmp->Node() == bb) {
	    tmp->Set_node(succ);
	    added = TRUE;
	    break;
	  }
	}
	// link to bb has been removed due to prev round, append
	if (!added) 
	  pred->Append_succ(succ, _mem_pool); 
      }	
    }
  }

  // Update feedback by moving incoming edges
  if ( Feedback() && bb->Succ()->Len() == 1 )
    Feedback()->Move_incoming_edges_dest( bb->Id(), bb->Succ()->Node()->Id() );

  Remove_bb(bb);
}

// ====================================================================
// Prepend wn at the beginning of the block
// ====================================================================

void 
CFG::Prepend_wn_in(BB_NODE *bb, WN* wn)
{
  if (bb->Firststmt() == NULL) {
    Init_stmtlist(bb, wn);
  }
  else {
    WN_prev(bb->Firststmt()) = wn;
    WN_next(wn) = bb->Firststmt();
    bb->Set_firststmt(wn);
  }
}

// ====================================================================
// Append wn at the end of the block
// ====================================================================
void
CFG::Append_wn_in(BB_NODE *bb, WN* wn)
{
  // don't allow anything but pragmas to be inserted into a BB_REGIONSTART
  // for C++ EH regions we allow vcall and goto in the pragma block
  Is_True(bb->Kind() != BB_REGIONSTART || (bb->Kind() == BB_REGIONSTART &&
	 (WN_opcode(wn) == OPC_PRAGMA || WN_opcode(wn) == OPC_XPRAGMA ||
	  WN_opcode(wn) == OPC_VCALL || WN_operator(wn) == OPR_GOTO)),
	  ("CFG::Append_wn_in(), inserting into a %s",bb->Kind_name()));

  if (bb->Firststmt() == NULL) {
    Init_stmtlist(bb, wn);
  } else {
    WN_next(bb->Laststmt()) = wn;
    WN_prev(wn) = bb->Laststmt();
    bb->Set_laststmt(wn);
  }
}

// ====================================================================
// Create a label statement for the block
// ====================================================================

void 
CFG::Create_label_stmt( BB_NODE *bb )
{
  Is_True( bb->Firststmt() == NULL,
    ("CFG::Create_label_stmt: non-empty block to get label") );

  LABEL_IDX label = bb->Labnam();

  if ( label == 0 )
    label = Alloc_label();

  WN *new_wn = WN_CreateLabel(0, label, 0, NULL);
  Init_stmtlist(bb, new_wn);
  Append_label_map(label, bb);
}

// ====================================================================
// Create new block that has a label statement, and is not connected to
// any other blocks
// ====================================================================

BB_NODE *
CFG::Create_labelled_bb( BB_KIND k /*=BB_GOTO*/ )
{
  BB_NODE *retbb = Create_bb( k );
  Create_label_stmt( retbb );
  return retbb;
}

// ====================================================================
// Create the conditional branch for this test
// ====================================================================

BB_NODE *
CFG::Create_conditional(WN      *cond,
			BB_NODE *true_bb,
			BB_NODE *false_bb,
			BOOL	 true_branch,
			WN     **created_branch)
{
  Is_True(cond != NULL, ("CFG::Create_conditional: NULL cond"));
  Is_True(true_bb != NULL || false_bb != NULL,
    ("CFG::Create_conditional: no target block"));

  if ( WN_operator(cond) == OPR_CAND ) {
    // get block where right-side kid will end up being evaluated
    BB_NODE *short_circuit = Create_labelled_bb();
    BOOL append_false_bb = FALSE;

    // will we need a false-target block
    if ( false_bb == NULL && true_branch ) {
      false_bb = Create_labelled_bb();
      append_false_bb = TRUE;
    }

    // evaluate left-side kid, branching around right-side expr if
    // we evaluate to false
    WN *wn_left_branch;
    if ( WN_operator(WN_kid0(cond)) == OPR_CAND ||
	 WN_operator(WN_kid0(cond)) == OPR_CIOR ) 
    {
      Create_conditional( WN_kid0(cond), short_circuit, false_bb, FALSE,
			  &wn_left_branch );
    }
    else {
      Create_conditional( WN_kid0(cond), true_bb, false_bb, FALSE,
			  &wn_left_branch );
    }

    // and connect in short-circuit
    Connect_predsucc( _current_bb, short_circuit );
    Append_bb( short_circuit );

    // evaluate the right-side kid normally
    WN *wn_right_branch;
    Create_conditional( WN_kid1(cond), true_bb, false_bb, true_branch,
			&wn_right_branch);

    // and connect in our false target if necessary
    if ( append_false_bb ) {
      Connect_predsucc( _current_bb, false_bb );
      Append_bb( false_bb );
    }

    // Lower feedback data for CAND
    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_circuit( cond,
					 wn_left_branch, wn_right_branch );

    *created_branch = NULL;
  }
  else if ( WN_operator(cond) == OPR_CIOR ) {
    // get block where right-side kid will end up being evaluated
    BB_NODE *short_circuit = Create_labelled_bb();
    BOOL append_true_bb = FALSE;

    if ( true_bb == NULL && !true_branch ) {
      true_bb = Create_labelled_bb();
      append_true_bb = TRUE;
    }

    // evaluate left-side kid, branching around right-side expr if
    // we evaluate to true
    WN *wn_left_branch;
    if ( WN_operator(WN_kid0(cond)) == OPR_CAND ||
	 WN_operator(WN_kid0(cond)) == OPR_CIOR )
    {
      Create_conditional( WN_kid0(cond), true_bb, short_circuit, TRUE,
			  &wn_left_branch );
    }
    else {
      Create_conditional( WN_kid0(cond), true_bb, false_bb, TRUE,
			  &wn_left_branch );
    }


    // connect in short-circuit
    Connect_predsucc( _current_bb, short_circuit );
    Append_bb( short_circuit );

    // evaluate the right-side kid normally
    WN *wn_right_branch;
    Create_conditional( WN_kid1(cond), true_bb, false_bb, true_branch,
			&wn_right_branch);

    // and connect in our true block
    if ( append_true_bb ) {
      Connect_predsucc( _current_bb, true_bb );
      Append_bb( true_bb );
    }

    // Lower feedback data for CIOR
    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_circuit( cond,
					 wn_left_branch, wn_right_branch );

    *created_branch = NULL;
  }
  else {
    // normal condition, so decide what kind of branch to generate
    WN *end_cond;
    if ( true_branch ) {
      // make sure we have a label
      if ( true_bb->Labnam() == 0 )
	Create_label_stmt( true_bb );

      end_cond = WN_CreateTruebr( true_bb->Labnam(), cond );
    }
    else {
      // ! true_branch
      // make sure we have a label
      if ( false_bb->Labnam() == 0 )
	Create_label_stmt( false_bb );

      end_cond = WN_CreateFalsebr( false_bb->Labnam(), cond );
    }
    WN_Set_Linenum( end_cond, WN_Get_Linenum(cond) );
    *created_branch = end_cond;

    // add the statement to this block
    Add_one_stmt(end_cond, NULL);
  }

  return _current_bb;
}
  
// ====================================================================
// fully lower an entry condition for a loop or if statement.
// "target" is the block we should conditionally branch to if the
// condition is false.  It must have a valid label assigned to it.
// ====================================================================

BB_NODE *
CFG::Create_entrytest(WN *cond, BB_NODE *target)
{
  FmtAssert(cond != NULL, ("CFG::Create_entrytest: NULL cond"));
  FmtAssert(target->Labnam() != 0, 
    ("CFG::Create_entrytest: BB:%d has no label", target->Id()) );

  WN *end_cond;
  end_cond = WN_CreateFalsebr(target->Labnam(), cond);
  WN_Set_Linenum( end_cond, WN_Get_Linenum(cond) );

  BB_NODE *retbb = _current_bb;
  Add_one_stmt(end_cond, NULL);
  return retbb;
}

// ====================================================================
// fully lower the body of a loop by starting it out with a labelled
// block and then handling the body.
// ====================================================================

BB_NODE *
CFG::Create_loopbody( WN *body )
{
  BB_NODE *body_bb = Create_labelled_bb();
  body_bb->Set_linenum(WN_Get_Linenum(body));
  Append_bb(body_bb);

  END_BLOCK body_bb_end;
  Add_one_stmt(body, &body_bb_end);

  // did the body block end with a branch to someplace else?
  if ( body_bb_end == END_FALLTHRU || body_bb_end == END_BREAK ) {
    // connect the next block only if the previous didn't jump elsewhere
    (void) New_bb( body_bb_end == END_FALLTHRU );
  }

  return body_bb;
}

// ====================================================================
// Create the back-edge conditional branch for a loop
// ====================================================================

BB_NODE *
CFG::Create_exittest(WN      *cond,
		     BB_NODE *body_bb,
		     BB_KIND  k)
{
  Is_True(cond != NULL, ("CFG::Create_exittest: NULL cond"));
  FmtAssert( body_bb->Labnam() != 0,
    ("CFG::Create_exittest: body_bb:%d has no label", body_bb->Id()) );

  WN *end_cond = WN_CreateTruebr(body_bb->Labnam(), cond);
  WN_Set_Linenum( end_cond, WN_Get_Linenum(cond) );

  // Set the loop end test target to be the body_bb
  Add_one_stmt(end_cond, NULL);

  _current_bb->Set_kind(k);
  return _current_bb;
}
  
// ====================================================================
// generate the LOOP_INFO and attach to the body_bb
// ====================================================================

void
CFG::Create_loop_info( BB_NODE *body_bb, WN *loop_wn )
{
  // see if we had a pre-existing loop info hanging off of this tree
  WN *loop_info = WN_do_loop_info( loop_wn );

  if ( loop_info == NULL ) {
    WN *index_var = WN_index( loop_wn );
    // save calculating the trip count until emit time.  Partly because
    // we will have changed some of the values from variable references
    // to constant references, and partly because WN_LOOP_TripCount()
    // relies on ST*, and we've already changed the STs (sigh)
    WN *loop_trip = NULL;

    loop_info = WN_CreateLoopInfo ( index_var, loop_trip, 
		  0/*estcount*/, Cur_loop_depth(), 0/*flags*/ );
  }

  body_bb->Set_label_loop_info(loop_info);
}

// ====================================================================
// generate the LOOP_INFO and attach to the body_bb
// ====================================================================

void
CFG::Create_blank_loop_info( BB_NODE *body_bb )
{
  WN *index_var = NULL;
  WN *loop_trip = NULL;
  WN *loop_info = WN_CreateLoopInfo ( index_var, loop_trip, 0 /*estcount*/,
				      Cur_loop_depth(), 0 /*flags*/ );

  body_bb->Set_label_loop_info(loop_info);
}

// There will be lot of troubles if the preheader is not "dedicated",
// meaning the preheader is intermingled with the statements of the 
// loop and other statements.
//
// This is especially true for loop multiversioning which duplicate 
// not only loop body but also the preheader. The purpose of dup 
// preheader is simply to make emiter happy: it will give up converting 
// a while-loop into do-loop if the the init statement of the IV is not 
// the last one of the preheader.
// 
void
CFG::Create_empty_preheader (WN* loop) {
  if (_current_bb->Firststmt() != NULL) {
    BB_NODE* blk = New_bb (TRUE/*connect*/);
    blk->Set_linenum (WN_Get_Linenum(loop));
  }
}

// ====================================================================
// fully lower DO_LOOP statements
// ====================================================================

void
CFG::Lower_do_loop( WN *wn, END_BLOCK *ends_bb )
{
  Is_True(Lower_fully(), ("CFG::Lower_do_loop: Lower_fully not true"));
  Set_cur_loop_depth( Cur_loop_depth() + 1 );

  // The do loop is lowered into the form:
  //    iv = iv_init; (C)
  //    if (iv cond loop_bound) iv_init will be propagated into iv later
  //       goto Exit;
  // Head:
  //    dohead block
  // Body:
  //    statements in original loop body
  //    statement in loop_step
  //    if (iv cond loop_bound)
  //      goto Body;
  // Tail:
  //    dotail block
  // Exit:

  WN *start_wn = WN_start(wn);
  FmtAssert(start_wn != NULL, ("CFG::Lower_do_loop: NULL start"));

  // create the doinit block, which contains the iv initialization
  Add_one_stmt( start_wn, NULL );
  
  // create, but do not connect, the exit bb
  BB_NODE *exit_bb = Create_labelled_bb(/* BB_GOTO */);
  BB_NODE *dohead_bb = Create_labelled_bb( BB_DOHEAD );

  // do we need an entry test?  Do so only if we have no info about
  // the loop, or it says it's not a non-zero trip count loop.
  BB_NODE *entry_test_bb = _current_bb;
  WN *loop_info = WN_do_loop_info(wn);
  WN *wn_top_branch = NULL;
  if ( loop_info == NULL || !WN_Loop_Nz_Trip(loop_info) ) {
    // Create loop entry test bb, and make a copy of original stmt.
    WN *endcopy = WN_copy(WN_end(wn));
    WN_copy_stmap(WN_end(wn), endcopy);
    if (Cur_PU_Feedback)
      Cur_PU_Feedback->FB_clone_loop_test( WN_end(wn), endcopy, wn );

    entry_test_bb = Create_conditional( endcopy, dohead_bb, exit_bb, FALSE,
					&wn_top_branch );
  }

  // Create the loop head, and connect it
  Connect_predsucc( entry_test_bb, dohead_bb );
  Append_bb( dohead_bb );

  // Create the loop body, and connect it to the dohead
  BB_NODE *body_bb = Create_loopbody(WN_do_body(wn));
  Connect_predsucc(dohead_bb, body_bb);

  // Create a LOOP_INFO node and attach to the body label
  Create_loop_info( body_bb, wn );

  // Append the loop increment statement to the end of the loop_body
  FmtAssert(WN_step(wn), ("CFG::Lower_do_loop: NULL do step"));
  Add_one_stmt(WN_step(wn), NULL);

  // Create the loop tail
  BB_NODE *dotail_bb = Create_labelled_bb( BB_DOTAIL );

  // Create the loop exit test
  WN *wn_back_branch;
  BB_NODE *cond_bb = Create_conditional( WN_end(wn), body_bb, dotail_bb, TRUE,
					 &wn_back_branch);
  cond_bb->Set_kind(BB_DOEND);

  // Create and connect the loop tail
  Connect_predsucc( cond_bb, dotail_bb );
  Append_bb( dotail_bb );

  // add the exit bb
  Connect_predsucc(dotail_bb, exit_bb);
  Append_bb(exit_bb);

  // Lower feedback data
  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_lower_loop(wn, wn_top_branch, wn_back_branch);
  }

  BB_LOOP *loopinfo = CXX_NEW( BB_LOOP( WN_index(wn), // index
					dohead_bb,    // start (init)
					cond_bb,      // condition
					body_bb,      // first body bb
					cond_bb,      // increment block
					dotail_bb ),  // merge block
			       _mem_pool );
  loopinfo->Set_has_entry_guard();
  loopinfo->Set_flag(LOOP_DO);
  dohead_bb->Set_loop(loopinfo);
  cond_bb->Set_loop(loopinfo);
  body_bb->Set_loop(loopinfo);

  // this statement does not break the block because we've created
  // a merge point; subsequent statements will be appended in the exit
  // block.
  if ( ends_bb )
    *ends_bb = END_NOT;

  Set_cur_loop_depth( Cur_loop_depth() - 1 );
}

// ====================================================================
// fully lower WHILE_DO statements
// ====================================================================

void
CFG::Lower_while_do( WN *wn, END_BLOCK *ends_bb )
{
  Is_True(Lower_fully(), ("CFG::Lower_while_do: Lower_fully not true"));
  Set_cur_loop_depth( Cur_loop_depth() + 1 );

  // The while loop is lowered into the form:
  //    if (! cond)  the entry_test
  //       goto Exit;
  // Head:
  //    dohead block
  // Body:
  //    statements in original loop body
  //    if (cond)
  //      goto Body;
  // Tail:
  //    dotail block
  // Exit:

  // create, but do not connect, the exit bb
  BB_NODE *exit_bb = Create_labelled_bb();
  BB_NODE *dohead_bb = Create_labelled_bb( BB_DOHEAD );

  // Create loop entry test bb, and make a copy of original expn.
  WN *testcopy = WN_copy(WN_while_test(wn));
  WN_copy_stmap(WN_while_test(wn), testcopy);
  if (Cur_PU_Feedback)
    Cur_PU_Feedback->FB_clone_loop_test( WN_while_test(wn), testcopy, wn );

  WN *wn_top_branch;
  BB_NODE *entry_test_bb = 
    Create_conditional( testcopy, dohead_bb, exit_bb, FALSE , &wn_top_branch);

  // Create and connect the loop head
  Connect_predsucc( entry_test_bb, dohead_bb );
  Append_bb( dohead_bb );

  // Create the loop body
  BB_NODE *body_bb = Create_loopbody(WN_while_body(wn));
  Connect_predsucc(dohead_bb, body_bb);

  // Create a LOOP_INFO node and attach to the body label
  Create_blank_loop_info(body_bb);

  // Create the loop tail
  BB_NODE *dotail_bb = Create_labelled_bb( BB_DOTAIL );

  // Create the loop exit test
  WN *wn_back_branch;
  BB_NODE *cond_bb = Create_conditional( WN_while_test(wn), body_bb,
					 dotail_bb, TRUE , &wn_back_branch);
  cond_bb->Set_kind(BB_WHILEEND);

  // connect the loop tail
  Connect_predsucc( cond_bb, dotail_bb );
  Append_bb( dotail_bb );

  // add the exit block
  Connect_predsucc(dotail_bb, exit_bb);
  Append_bb( exit_bb );

  
  // Lower feedback data
  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_lower_loop(wn, wn_top_branch, wn_back_branch);
  }

  BB_LOOP *loopinfo = CXX_NEW( BB_LOOP( NULL,      // index
					dohead_bb, // start (init)
					cond_bb,   // condition
					body_bb,   // first body bb
					cond_bb,   // increment and branch back
					dotail_bb ), // merge block
			       _mem_pool );
  // See PV 354340.
  //  WHILE-loop do not have entry guard because the loop-exit condition
  //  might be modified inside a loop.
  //
  // loopinfo->Set_has_entry_guard();
  loopinfo->Set_flag(LOOP_WHILE);
  dohead_bb->Set_loop(loopinfo);
  cond_bb->Set_loop(loopinfo);
  body_bb->Set_loop(loopinfo);

  // this statement does not break the block because we've created
  // a merge point
  if ( ends_bb )
    *ends_bb = END_NOT;

  Set_cur_loop_depth( Cur_loop_depth() - 1 );
}

// ====================================================================
// fully lower DO_WHILE statements
// ====================================================================

void
CFG::Lower_do_while( WN *wn, END_BLOCK *ends_bb )
{
  Is_True(Lower_fully(), ("CFG::Lower_do_while: Lower_fully not true"));
  Set_cur_loop_depth( Cur_loop_depth()+1 );

  // The repeat loop is lowered into the form:
  // Head:
  //    dohead block
  // Body:
  //    statements in original loop body
  //    if (cond)
  //      goto Body;
  // Tail:
  //    dotail block

  // Create the loop head as an empty block
  BB_NODE *dohead_bb;

  // The head block should be empty at this point.
  if (_current_bb->Firststmt() == NULL) {
    dohead_bb = _current_bb;
    dohead_bb->Set_kind( BB_DOHEAD );
    Create_label_stmt( dohead_bb );
  }
  else {
    dohead_bb = Create_labelled_bb( BB_DOHEAD );
    Connect_predsucc( _current_bb, dohead_bb );
    Append_bb( dohead_bb );
  }
  dohead_bb->Set_linenum(WN_Get_Linenum(wn));

  // Create the loop body
  BB_NODE *body_bb = Create_loopbody(WN_while_body(wn));
  Connect_predsucc(dohead_bb, body_bb);

  // Create a LOOP_INFO node and attach to the body label
  Create_blank_loop_info(body_bb);

  // Create the loop tail
  BB_NODE *dotail_bb = Create_labelled_bb( BB_DOTAIL );

  // Create the loop exit test
   WN *wn_back_branch;
   BB_NODE *cond_bb = Create_conditional( WN_while_test(wn), body_bb,
					  dotail_bb, TRUE , &wn_back_branch);
  cond_bb->Set_kind(BB_REPEATEND);

  // connect up the loop tail
  Connect_predsucc( cond_bb, dotail_bb );
  Append_bb( dotail_bb );

  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_lower_loop(wn, NULL, wn_back_branch);
  }

  BB_LOOP *loopinfo = CXX_NEW( BB_LOOP( NULL,	     // index
					dohead_bb,   // start (init)
					cond_bb,     // condition
					body_bb,     // first body bb
					cond_bb,     // incr and branch back
					dotail_bb ), // merge block
			       _mem_pool );
  loopinfo->Set_flag(LOOP_REPEAT);
  dohead_bb->Set_loop(loopinfo);
  cond_bb->Set_loop(loopinfo);
  body_bb->Set_loop(loopinfo);

  // this statement does not break the block because we've created
  // a merge point
  if ( ends_bb )
    *ends_bb = END_NOT;

  Set_cur_loop_depth( Cur_loop_depth()-1 );
}

// ====================================================================
// check if the expr in wn is a simple expression for purpose of if-conversion;
// if true, return number of leaf nodes; otherwise, return 0 for false.
// ====================================================================
INT 
CFG::Is_simple_expr(WN *wn) {
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_LDID && 
     _opt_stab->Safe_to_speculate(WN_aux(wn)) &&
     !_opt_stab->Is_volatile(WN_aux(wn))) 
    return 1;
  if (opr == OPR_INTCONST || opr == OPR_CONST)
    return 1;
  // bug 11542
  if (opr == OPR_LDA)
    return 1;
#if defined(TARG_IA32) || defined(TARG_X8664)
  if (! MTYPE_is_integral(WN_rtype(wn)))
    return 0;
#elif defined(TARG_SL)
  if (! MTYPE_is_integral(WN_rtype(wn)))
    return 0;
  if (opr == OPR_SELECT)
    return 1;
  if (WOPT_Enable_If_Conv_For_Iload && (opr == OPR_ILOAD))
    return Is_simple_expr(WN_kid0(wn));
#endif
  if (opr == OPR_NEG || opr == OPR_ABS || opr == OPR_CVTL)
    return Is_simple_expr(WN_kid0(wn));
  if (opr == OPR_CVT &&			// bug 11885
      MTYPE_is_integral(WN_desc(wn)) &&
      MTYPE_is_integral(WN_rtype(wn))) {
    return Is_simple_expr(WN_kid0(wn));
  }

  // ADD is special because it translates into LEA, which can do one
  // multiplication by 2, 4, or 8 for free.  Bug 11885.
  if (opr == OPR_ADD) {
    WN *mult = NULL;
    WN *non_mult = NULL;
    OPERATOR opr0 = WN_operator(WN_kid0(wn));
    OPERATOR opr1 = WN_operator(WN_kid1(wn));
    if (opr0 == OPR_MPY && opr1 != OPR_MPY) {
      mult = WN_kid0(wn);
      non_mult = WN_kid1(wn);
    } else if (opr0 != OPR_MPY && opr1 == OPR_MPY) {
      mult = WN_kid1(wn);
      non_mult = WN_kid0(wn);
    }
    if (mult != NULL) {
      WN *mult_opnd;
      INT mult_kid_ans, non_mult_kid_ans;
      INT val = -1;
      if (WN_operator(WN_kid0(mult)) == OPR_INTCONST) {
	val = WN_const_val(WN_kid0(mult));
	mult_opnd = WN_kid1(mult);
      } else if (WN_operator(WN_kid1(mult)) == OPR_INTCONST) {
	val = WN_const_val(WN_kid1(mult));
	mult_opnd = WN_kid0(mult);
      }
      if (val != 2 && val != 4 && val != 8)
	return 0;
      mult_kid_ans = Is_simple_expr(mult_opnd);
      if (mult_kid_ans == 0)
	return 0;
      non_mult_kid_ans = Is_simple_expr(non_mult);
      if (non_mult_kid_ans == 0)
	return 0;
      return mult_kid_ans + non_mult_kid_ans;
    }
  }	// Fall through if not exactly one operand is mult.

  if (opr == OPR_ADD || opr == OPR_SUB || opr == OPR_NEG ||
      opr == OPR_SHL || opr == OPR_ASHR || opr == OPR_LSHR ||
#ifdef TARG_NVISA
      // mpy is usually fast on nvisa
      opr == OPR_MPY ||
#endif
#ifdef TARG_SL
      opr == OPR_MIN || opr == OPR_MAX ||
#endif
      opr == OPR_BAND || opr == OPR_BIOR || opr == OPR_BNOR || opr == OPR_BXOR)
  { INT kid0ans, kid1ans;
    kid0ans = Is_simple_expr(WN_kid0(wn));
    if (kid0ans == 0)
      return 0;
    kid1ans = Is_simple_expr(WN_kid1(wn));
    if (kid1ans == 0)
      return 0;
    return kid0ans + kid1ans;
  }
#ifdef TARG_NVISA
  if (opr == OPR_CVT || opr == OPR_RECIP) {
      INT kidans = Is_simple_expr(WN_kid0(wn));
      return kidans;
  }
  if (opr == OPR_MADD || opr == OPR_NMADD) {
      INT kid0ans = Is_simple_expr(WN_kid0(wn));
      INT kid1ans = Is_simple_expr(WN_kid1(wn));
      INT kid2ans = Is_simple_expr(WN_kid2(wn));
      if (kid0ans == 0 || kid1ans == 0 || kid2ans == 0)
	return 0;
      return kid0ans + kid1ans + kid2ans;
  }
#endif
  return 0;
}

// ====================================================================
// copy the address expression tree recursively
// ====================================================================
static WN *Copy_addr_expr(WN *wn, ALIAS_CLASSIFICATION *ac)
{
  if (wn == NULL)
    return NULL;
  WN *new_wn = WN_CopyNode(wn);
  if (OPERATOR_has_aux(WN_operator(wn)))
    WN_set_aux(new_wn, WN_aux(wn)); // setting mapping to indicate ST_is_aux
  else if (OPERATOR_is_load(WN_operator(wn)) || WN_operator(wn) == OPR_LDA) {
    ac->Copy_alias_class(wn, new_wn);
    IDTYPE ip_alias_class = WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn);
    if (ip_alias_class != OPTIMISTIC_AC_ID)
      WN_MAP32_Set(WN_MAP_ALIAS_CLASS, new_wn, ip_alias_class);
    AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
    if (aa)
      aa->transferAliasTag(new_wn,wn);
  }
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    WN *kid = WN_kid(wn, i);
    if (kid)
      WN_kid(new_wn, i) = Copy_addr_expr(kid, ac);
    else WN_kid(new_wn, i) = NULL;
  }
  return new_wn;
}

// ====================================================================
// check if the two address expressions are the same; only handle common 
// operators in address expressions; use WN_aux (not WN_st) field for LDID nodes
// ====================================================================
static BOOL Same_addr_expr(WN *wn1, WN *wn2)
{
  if (WN_opcode(wn1) != WN_opcode(wn2))
    return FALSE;
  switch (WN_operator(wn1)) {
    case OPR_INTCONST: 
      return WN_const_val(wn1) == WN_const_val(wn2);
    case OPR_LDA:
      if (WN_lda_offset(wn1) != WN_lda_offset(wn2))
	return FALSE;
      return WN_aux(wn1) == WN_aux(wn2);
    case OPR_LDBITS:
      if (WN_bit_offset(wn1) != WN_bit_offset(wn2))
	return FALSE;
      if (WN_bit_size(wn1) != WN_bit_size(wn2))
	return FALSE;
      // fall thru
    case OPR_LDID:
      return WN_aux(wn1) == WN_aux(wn2);
    case OPR_ILDBITS:
      if (WN_bit_offset(wn1) != WN_bit_offset(wn2))
	return FALSE;
      if (WN_bit_size(wn1) != WN_bit_size(wn2))
	return FALSE;
      // fall thru
    case OPR_ILOAD:
      if (WN_load_offset(wn1) != WN_load_offset(wn2))
	return FALSE;
      return Same_addr_expr(WN_kid0(wn1), WN_kid0(wn2));
    case OPR_CVTL:
      if (WN_cvtl_bits(wn1) != WN_cvtl_bits(wn2))
	return FALSE;
      // fall thru
    case OPR_CVT: case OPR_NEG: case OPR_ABS:
      return Same_addr_expr(WN_kid0(wn1), WN_kid0(wn2));
    case OPR_ADD: case OPR_SUB: case OPR_MPY: case OPR_DIV: case OPR_MOD:
    case OPR_REM: case OPR_MAX: case OPR_MIN: case OPR_EQ: case OPR_NE:
    case OPR_GE: case OPR_GT: case OPR_LE: case OPR_LT: case OPR_BAND:
    case OPR_BIOR: case OPR_BNOR: case OPR_BXOR: case OPR_SHL: 
    case OPR_ASHR: case OPR_LSHR:
      return Same_addr_expr(WN_kid0(wn1), WN_kid0(wn2)) &&
             Same_addr_expr(WN_kid1(wn1), WN_kid1(wn2));
    case OPR_SELECT:
      return Same_addr_expr(WN_kid0(wn1), WN_kid0(wn2)) &&
             Same_addr_expr(WN_kid1(wn1), WN_kid1(wn2)) &&
             Same_addr_expr(WN_kid2(wn1), WN_kid2(wn2));
    case OPR_ARRAY:
      if (WN_num_dim(wn1) != WN_num_dim(wn2))
	return FALSE;
      if (WN_element_size(wn1) != WN_element_size(wn2))
	return FALSE;
      if (!Same_addr_expr(WN_array_base(wn1), WN_array_base(wn2)))
	return FALSE;
      for (INT i=0; i < WN_num_dim(wn1); i++) {
	if (!Same_addr_expr(WN_array_index(wn1, i), WN_array_index(wn2, i)))
	  return FALSE;
	if (!Same_addr_expr(WN_array_dim(wn1, i), WN_array_dim(wn2, i)))
	  return FALSE;
      }
      return TRUE;
    default:  // operators not common in address expressions
      return FALSE;
  }
  return FALSE;
}

// ====================================================================
// find if there is an ILOAD in the expression wn with the same address expr
// ====================================================================
static BOOL Has_iload_with_same_addr_expr(WN *addr_expr, WN *wn)
{
  if (WN_operator(wn) == OPR_ILOAD) {
    if (Same_addr_expr(WN_kid0(wn), addr_expr))
      return TRUE;
  }
  else if (WN_operator(wn) == OPR_ISTORE) {
    if (Same_addr_expr(WN_kid1(wn), addr_expr))
      return TRUE;
  }
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    WN *kid = WN_kid(wn, i);
    if (Has_iload_with_same_addr_expr(addr_expr, kid))
      return TRUE;
  }
  return FALSE;
}

// ====================================================================
// wn1 and wn2 are store nodes; check if they are storing to identical targets
// ====================================================================
static BOOL Same_store_target(WN *wn1, WN *wn2)
{
  OPERATOR opr1 = WN_operator(wn1);
  OPERATOR opr2 = WN_operator(wn2);
  if (opr1 != opr2)
    return FALSE;
  if (WN_desc(wn1) != WN_desc(wn2))
    return FALSE;
  if (opr1 == OPR_STID) 
    return WN_aux(wn1) == WN_aux(wn2);
  if (opr1 == OPR_STBITS) 
    return WN_aux(wn1) == WN_aux(wn2) && 
      WN_bit_offset(wn1) == WN_bit_offset(wn2) &&
      WN_bit_size(wn1) == WN_bit_size(wn2);
  // ISTORE/ISTBITS
  if (WN_store_offset(wn1) != WN_store_offset(wn2))
    return FALSE;
  if (WN_ty(wn1) != WN_ty(wn2))
    return FALSE;
  if (opr1 == OPR_ISTBITS &&
      (WN_bit_offset(wn1) != WN_bit_offset(wn2) ||
       WN_bit_size(wn1) != WN_bit_size(wn2)))
    return FALSE;
  return Same_addr_expr(WN_kid1(wn1), WN_kid1(wn2));
}

BOOL CFG::Screen_cand(WN* wn)
{
  FmtAssert(WN_operator(wn) == OPR_IF, ("Screen_cand: Not an if stmt"));
  WN *then_wn = WN_then(wn);
  WN *else_wn = WN_else(wn);
  BOOL empty_then = !then_wn || !WN_first(then_wn);
  BOOL empty_else = !else_wn || !WN_first(else_wn);

  WN *if_test = WN_if_test(wn);
  WN *stmt = WN_first(empty_then ? else_wn : then_wn);

  if (WN_operator(stmt) == OPR_MSTORE)
    return TRUE;

  // Get the desc type
  MTYPE dsctyp = WN_desc(stmt);

  if (dsctyp == MTYPE_M) {
    // don't generate select for MTYPE_M because there is no register for
    // MTYPE_M
    return TRUE;
  }

  if (! WOPT_Enable_If_Conv_For_Istore &&
      (WN_operator(stmt) == OPR_ISTORE || WN_operator(stmt) == OPR_ISTBITS))
    return TRUE;

  if (WN_operator(stmt) == OPR_STID || WN_operator(stmt) == OPR_STBITS) {
    if (_opt_stab->Is_volatile(WN_aux(stmt)))
      return TRUE;	
#ifdef TARG_NVISA
    // shared memory is semi-volatile, in that we can't write multiple
    // different values at the same time.
    if (ST_in_shared_mem(_opt_stab->St(WN_aux(stmt)))) {
      DevWarn("skip if-conversion of shared store");
      return TRUE;
    }
#endif
  }
  else {
    if (TY_is_volatile(WN_ty(stmt)))
      return TRUE;
  }

  if ((WN_operator(stmt) == OPR_STBITS || WN_operator(stmt) == OPR_ISTBITS) &&
      (empty_then || empty_else)) 
    return TRUE;

  if (!OPCODE_Can_Be_Speculative(OPC_I4I4ILOAD)) {
    if (WN_operator(stmt) == OPR_STID || WN_operator(stmt) == OPR_STBITS) {
      if (!empty_then) {
        ST *st = _opt_stab->St(WN_aux(WN_first(then_wn)));
        if (ST_sclass(st) == SCLASS_FORMAL_REF)
          return TRUE; // may be storing into read-only data (bug 12
      }
      if (!empty_else) {
        ST *st = _opt_stab->St(WN_aux(WN_first(else_wn)));
        if (ST_sclass(st) == SCLASS_FORMAL_REF)
          return TRUE; // may be storing into read-only data (bug 12
      }
    }
    else if (empty_then || empty_else) {
      WN *addr_expr = WN_kid1(stmt);
      // because need to generate an extra ILOAD, if the if_test involves
      // the addr_expr, it is probably means speculation is unsafe, so give up
      // bug 7845
      if (OPERATOR_is_compare(WN_operator(if_test)) &&
          (Same_addr_expr(WN_kid0(if_test), addr_expr) ||
           Same_addr_expr(WN_kid1(if_test), addr_expr)))
        return TRUE;

      // because need to generate an extra ILOAD, see that a similar ILOAD has
      // occurred unconditionally; check currently limited to conditional expr
      // plus previous 2 statements
      if (! Has_iload_with_same_addr_expr(addr_expr, if_test)) {
        // check previous statement
        if (_current_bb->Laststmt() == NULL) 
          return TRUE;
        if (! Has_iload_with_same_addr_expr(addr_expr, _current_bb->Laststmt()))
        {
          if (WN_prev(_current_bb->Laststmt()) == NULL) 
            return TRUE;
          if (!Has_iload_with_same_addr_expr(addr_expr, WN_prev(_current_bb->Laststmt())))
            return TRUE;
        }
      }
    }
  }
#if defined(TARG_X8664) || defined(TARG_SL)
  if (MTYPE_is_float(dsctyp))
    return TRUE;
#endif /* TARG_X8664 */
  return FALSE;
}


BOOL CFG::If_convertible_cond(WN *wn)
{
  // given an if stmt, is this if-convertible
  FmtAssert((OPCODE_operator(WN_opcode(wn)) == OPR_IF), ("Not an if stmt"));
  WN    *then_wn    = WN_then(wn);
  BOOL   empty_then = FALSE;
  WN *if_test = WN_if_test(wn);

  if ( then_wn == NULL )
    empty_then = TRUE;
  else if ( WN_opcode(then_wn) == OPC_BLOCK && 
      WN_first(then_wn) == NULL )
    empty_then = TRUE;

  WN   *else_wn    = WN_else(wn);
  BOOL  empty_else = FALSE;

  if ( else_wn == NULL )
    empty_else = TRUE;
  else if ( WN_opcode(else_wn) == OPC_BLOCK && 
      WN_first(else_wn) == NULL )
    empty_else = TRUE;

  if (Cand_is_select(wn)) {
    if (Screen_cand(wn)) {
      return FALSE;
    }
    return TRUE;
  }
  return FALSE;
}

BOOL CFG::Cand_is_return_inside_select(WN *wn)
{
  FmtAssert(WN_operator(wn) == OPR_IF, ("Extract_Return: Unexpected WN"));
  
  WN *then_wn = WN_then(wn);
  WN *else_wn = WN_else(wn);

  WN *then_wn_last = WN_last(then_wn);
  WN *else_wn_last = WN_last(else_wn);

  if(!(then_wn_last && else_wn_last 
    && (WN_operator(then_wn_last) == OPR_RETURN)
    && (WN_operator(else_wn_last) == OPR_RETURN))) 
    return FALSE;

  WN *then_wn_prev = WN_prev(then_wn_last);
  WN *else_wn_prev = WN_prev(else_wn_last);

  // During the creation of cfg in  RVI phase2, the  IF-THEN-ELSE block,
  // which was converted from SELECT during wn_lower between RVI phase1
  // and phase2, may be converted again which will expect the opt_stab is
  // still valid.Return FALSE to stop the convertion.
  if(_opt_stab == NULL)
    return FALSE;
  
  /* both is empty, return FALSE */
  if (then_wn_prev == NULL && else_wn_prev == NULL)
    return FALSE;

  /* then is not empty and not assign-return, return FALSE */
  if (then_wn_prev && !WN_is_assign_return(then_wn)) {
    return FALSE; 
  }

  /* then is not empty and not assign-return, return FALSE */  
  if (else_wn_prev && !WN_is_assign_return(else_wn)) {
    return FALSE;
  }

  /* then and else have different target, return FALSE */
  if (then_wn_prev && else_wn_prev && !Same_store_target(then_wn_prev, else_wn_prev))
    return FALSE;

  return TRUE;
}

BOOL CFG::Cand_is_select(WN* wn)
{
  // Perform if-conversion for <simple if-then-else> statements.
  //
  //   <simple if-then-else> :: if (cond) then <block> else <block> ;;
  //   <block> :: <empty_block> | block stid <var> <simple_expr>  end_block ;;
  //   <empty_block> :: block end_block ;;
  //   <simple_expr> :: <var> | <const> | <f8const> ;;
  //  
  //   where <var> can be speculated and is non-volatile.  At
  //   least one of then or else block is non-empty.  If both the 
  //   then and else block are non-empty, they stores to
  //   the same variable.
  //
  WN *then_wn = WN_then(wn);
  WN *else_wn = WN_else(wn);
  BOOL empty_then = !then_wn || !WN_first(then_wn);
  BOOL empty_else = !else_wn || !WN_first(else_wn);

#if defined(TARG_X8664)  // do not if-convert if it has either empty then or else part and it
  if (
      // is the only statement in the BB since CG's cflow can be quite effective
      ((empty_else || empty_then) &&
#if defined(TARG_IA64)
      // here select larger than 6 from osprey, I think it may be more aggressive
       WOPT_Enable_If_Conv_Limit <= 6 &&
#else
       (WOPT_Enable_Simple_If_Conv <= 1) &&
#endif
       (WN_next(wn) == NULL &&
       !(_current_bb->Firststmt() != NULL && // no previous statement in BB
        (_current_bb->Firststmt() != _current_bb->Laststmt() || // prev is LABEL
        (WN_operator(_current_bb->Firststmt()) != OPR_LABEL))))
       )
      ) 
    return FALSE;
#endif

  // During the creation of cfg in  RVI phase2, the  IF-THEN-ELSE block,
  // which was converted from SELECT during wn_lower between RVI phase1
  // and phase2, may be converted again which will expect the opt_stab is
  // still valid.Return FALSE to stop the convertion.
  if(_opt_stab == NULL)
    return FALSE;

  // at least one of the then or else statement is non-empty
  if (empty_else && empty_then)
    return FALSE;

  // either the else-stmt is empty or has one  assignment
  if (!empty_else && !(WN_is_assign(else_wn)))
      return FALSE;
  
  // either the then-stmt is empty or has one assignment
  if (!empty_then && !WN_is_assign(then_wn))
    return FALSE;
  
  // both the then and else are empty or has one assignment with same lhs
  if ((!empty_else && !empty_then &&	!Same_store_target(WN_first(else_wn), WN_first(then_wn))))
    return FALSE;
  return TRUE;
}

#if defined(TARG_SL)
// ====================================================================
// check whether mode_wn is sub-tree of wn, and the sub-tree of parent_wn is ILOAD.
// check the such case for if_conversion:
//    if(p)
//      res = p->a
//    else
//      res = 0;
//  
//    p is mode_wn, p->a is parent_wn
// ====================================================================
BOOL CFG::Is_Sub_ILOAD_Tree(WN *wn, WN *parent_wn, WN * mode_wn)
{
  if (parent_wn 
   && ((WN_operator(parent_wn) == OPR_ILOAD) || (WN_operator(parent_wn) == OPR_ILOADX)
    || (WN_operator(parent_wn) == OPR_MLOAD))
   && (WN_Simp_Compare_Trees(wn, mode_wn) == 0)) 
    return TRUE;

  INT kids_cnt = WN_kid_count(wn);
  if (kids_cnt == 0) {
    return FALSE;
  } else { 
    int i;
    for(i = 0; i< kids_cnt; i++) {
      if (Is_Sub_ILOAD_Tree(WN_kid(wn, i), wn, mode_wn)) {
        return TRUE;
      }      
    }
    return FALSE;
  }  
}
#endif

WN *
CFG::if_convert(WN *wn)
{
  Is_True(WN_operator(wn) == OPR_IF, ("CFG::if_convert: Unexpected wn"));

  if (!(WOPT_Enable_Simple_If_Conv
  // bug 5684: deleting branches interferes with branch profiling
     && !Instrumentation_Enabled
#ifdef TARG_MIPS
     && Is_Target_ISA_M4Plus()
#endif
    ))
    return wn;
  
  WN *then_wn = WN_then(wn);
  WN *else_wn = WN_else(wn);

  // Handle embedded if-stmt
  if (then_wn && WN_first(then_wn) && WN_first(then_wn) == WN_last(then_wn) 
      && WN_operator(WN_first(then_wn)) == OPR_IF) {
    then_wn = if_convert(WN_first(then_wn));
    WN_then(wn) = then_wn;
  }
  if (else_wn && WN_first(else_wn) && WN_first(else_wn) == WN_last(else_wn) 
      && WN_operator(WN_first(else_wn)) == OPR_IF) {
    else_wn = if_convert(WN_first(else_wn));
    WN_else(wn) = else_wn;
  }

  BOOL if_select = Cand_is_select(wn);
  BOOL if_select_return = Cand_is_return_inside_select(wn);

  if (!if_select && !if_select_return)
    return wn;

  WN *return_wn = NULL;
  WN *wn_bk = WN_COPY_Tree_With_Map(wn); /* back up wn */

  if (if_select_return) {
    /* store return_wn */
    return_wn = WN_last(WN_else(wn));

    /* reset then_block */
    WN *then_block = WN_CreateBlock();
    if (WN_prev(WN_last(then_wn)))
      WN_INSERT_BlockFirst(then_block, WN_prev(WN_last(then_wn)));
    then_wn     = then_block;
    WN_then(wn) = then_wn;

    /* reset else_block */
    WN *else_block = WN_CreateBlock();
    if (WN_prev(WN_last(else_wn)))
      WN_INSERT_BlockFirst(else_block, WN_prev(WN_last(else_wn)));
    else_wn     = else_block;
    WN_else(wn) = else_wn;
  }

  BOOL empty_then = !then_wn || !WN_first(then_wn);    
  BOOL empty_else = !else_wn || !WN_first(else_wn);
  
  FmtAssert(!(empty_then && empty_else), 
       ("Screen_cand: Both then_stmt and else_stmt are NULL"));

  if (Screen_cand(wn))
    return wn_bk;
  
  // Get the store from either the first statement of the non-empty block
  WN *stmt = WN_first(empty_then ? else_wn : then_wn);

  WN *load = NULL;
  WN *store = WN_COPY_Tree_With_Map(stmt);
  MTYPE dsctyp = WN_desc(stmt);

  if (WN_operator(stmt) == OPR_STID || WN_operator(stmt) == OPR_STBITS)
    WN_set_aux(store, WN_aux(stmt)); // setting mapping to indicate ST_is_aux

  // Generate a load for the empty block
  if (empty_then || empty_else) {
    if (WN_operator(stmt) == OPR_STID) {
      load = WN_Ldid(dsctyp, WN_offset(stmt), (ST_IDX) WN_aux(stmt),
                      WN_ty(stmt), WN_field_id(stmt));
      WN_set_aux(load, WN_aux(stmt)); // setting mapping to indicate ST_is_aux
    } else {
      MTYPE rtype = WN_rtype(WN_kid0(stmt));
      if (MTYPE_byte_size(rtype) < MTYPE_byte_size(dsctyp))
        rtype = dsctyp;// rtype should never be smaller than dsctyp (bug 6910)
      else 
        rtype = Mtype_TransferSign(dsctyp, rtype);
      
      load = WN_CreateIload(OPR_ILOAD, rtype, dsctyp, WN_offset(stmt), 
                TY_pointed(WN_ty(stmt)), WN_ty(stmt), 
                Copy_addr_expr(WN_kid1(stmt), _opt_stab->Alias_classification()), 
                WN_field_id(stmt));

      // copy alias class info from the ISTORE node
      _opt_stab->Alias_classification()->Copy_alias_class(stmt, load);
      IDTYPE ip_alias_class = WN_MAP32_Get(WN_MAP_ALIAS_CLASS, stmt);
      if (ip_alias_class != OPTIMISTIC_AC_ID)
        WN_MAP32_Set(WN_MAP_ALIAS_CLASS, load, ip_alias_class);
    }
  }
 
  WN *then_expr = empty_then ? load : WN_kid0(WN_first(then_wn));
  WN *else_expr = empty_else ? load : WN_kid0(WN_first(else_wn));
  INT lanswer, ranswer;

  // profitability check
  if (
#if !defined(TARG_IA32) && !defined(TARG_X8664) && !defined(TARG_SL)
    // The expr in the then-block can be speculated and non-volatile,
    // is a const or LDA.
    Is_simple_expr(then_expr) &&

    // The expr in the else-block can be speculated and non-volatile,
    // is a const or LDA.
    Is_simple_expr(else_expr) 
#else // allow simple expressions of up to 4 leaf nodes
    (lanswer = (empty_then ? 1 : Is_simple_expr(then_expr))) && 

    (ranswer = (empty_else ? 1 : Is_simple_expr(else_expr))) && 

    (lanswer + ranswer) <= WOPT_Enable_If_Conv_Limit
#endif
     ) {
    // Generate a SELECT expression
    WN *sel;
#ifdef TARG_SL 
    //
    // For such case,
    //    if(p)
    //      res = p->a
    //    else
    //      res = 0;
    // if_conversion is skipped as accessing ZERO-address is illegal for SL. 
    //
    WN *if_test   = WN_if_test(wn);
    WN *cond_kid0 = WN_kid0(if_test);
    WN *cond_kid1 = WN_kid1(if_test);
    OPERATOR opr = WN_operator(if_test);

    if ((opr == OPR_EQ || opr == OPR_NE || opr == OPR_GE || opr == OPR_GT || opr == OPR_LE || opr == OPR_LT)  
     && ((WN_operator(cond_kid1) == OPR_INTCONST) && (WN_const_val(cond_kid1) == 0))
     && (Is_Sub_ILOAD_Tree(then_expr, NULL, cond_kid0) || Is_Sub_ILOAD_Tree(else_expr, NULL, cond_kid0))) 
      return wn_bk;

    if (opr == OPR_GE || opr == OPR_GT || opr == OPR_LE) {
      // SL only have one compare instruction, that is "<".
      // So do following convert when generating "SELECT" for better
      // optimization of CG phase.
      // a >= b ? 5 : 3   ==>   a < b ? 3 : 5
      // a >  b ? 5 : 3   ==>   b < a ? 5 : 3
      // a <= b ? 5 : 3   ==>   b < a ? 3 : 5
      WN *left_cond, *right_cond, *true_expr, *false_expr;
      switch(opr) {
        case OPR_GE:
        {
          left_cond  = cond_kid0;
          right_cond = cond_kid1;
          true_expr  = else_expr;
          false_expr = then_expr;
          break;
        }
        case OPR_GT:
        {
          left_cond  = cond_kid1;
          right_cond = cond_kid0;
          true_expr  = then_expr;
          false_expr = else_expr;
          break;          
        }

        case OPR_LE:
        {
          left_cond  = cond_kid1;
          right_cond = cond_kid0;
          true_expr  = else_expr;
          false_expr = then_expr;
          break;
        }
        default:
          Fail_FmtAssertion("Lower_if_stmt: Unexpected compare operator");
      }
      
      WN *test_wn = WN_CreateExp2(OPR_LT, WN_rtype(if_test), WN_desc(if_test), 
                                    left_cond, right_cond);
      sel = WN_Select(Mtype_comparison(dsctyp),
                  test_wn, true_expr, false_expr);
   } else
#endif
    sel = WN_Select(Mtype_comparison(dsctyp),
          WN_if_test(wn), then_expr, else_expr);
    WN_kid0(store) = sel;
    WN_Set_Linenum(store, WN_Get_Linenum(wn));

    WN *block = WN_CreateBlock();
    WN_INSERT_BlockFirst(block, store);
    if (if_select_return) {
      WN_INSERT_BlockAfter(block, store, return_wn);
    }
    return block;    
  } else {
    return wn_bk;
  }
}

void
CFG::Lower_if_stmt(WN *wn, END_BLOCK *ends_bb)
{
  Is_True(Lower_fully(), ("CFG::Lower_if_stmt: Lower_fully not true"));
  WN *conv_wn = if_convert(wn);
  if (WN_operator(conv_wn) == OPR_IF)
    Add_one_if_stmt(conv_wn, ends_bb);
  else
    Add_one_stmt(conv_wn, ends_bb);

  // Lower feedback data
  if (Cur_PU_Feedback)
    Cur_PU_Feedback->FB_lower_branch(wn, NULL);
}

// ====================================================================
// Add a DO_LOOP statement
// ====================================================================

void
CFG::Add_one_do_loop_stmt( WN *wn, END_BLOCK *ends_bb )
{
  BOOL process_mp_do = Inside_mp_do();

  Set_cur_loop_depth( Cur_loop_depth() + 1 );

  // this section of code should occur for preopt only!
  // Please read opt_cfg.h to see the shape of outcome.
  // It does some form of lowering without changing the original IR.

  // The init statement goes in its own block

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = Add_sc(NULL, SC_LOOP);
    _sc_parent_stack->Push(sc);
    sc = Add_sc(NULL, SC_LP_START);
    _sc_parent_stack->Push(sc);
  }
  
  BB_NODE *start_bb;
  if (_current_bb->Firststmt() != NULL) {
    start_bb = New_bb( TRUE/*connect*/, BB_DOSTART );
  }
  else {
    start_bb = _current_bb;
    if (Do_pro_loop_trans()) {
      SC_NODE * sc = Unlink_sc(start_bb);
      if (sc == NULL)
	Add_sc(start_bb, SC_BLOCK);
      else {
	sc->Convert(SC_BLOCK);
	SC_NODE * parent = _sc_parent_stack->Top();
	sc->Set_parent(parent);
	parent->Append_kid(sc);
      }
    }

    start_bb->Set_kind( BB_DOSTART );
  }
  
  start_bb->Set_linenum(WN_Get_Linenum(wn));

  // Put the init statement in it
  WN *start_wn = WN_start(wn);
  Is_True(start_wn != NULL, ("CFG::Add_one_do_loop_stmt: NULL start"));
  Add_one_stmt( start_wn, NULL );

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_START), ("Expect a SC_LP_START"));
  }

  // Create, but do not connect the merge block
  BB_NODE *merge_bb = Create_bb();

  Append_label_map(Alloc_label(), merge_bb);

  // Second, create loop cond bb
  Is_True(WN_end(wn) != NULL, ("CFG::Add_one_do_loop_stmt: NULL end"));
  WN *end_cond = WN_CreateFalsebr(merge_bb->Labnam(), WN_end(wn));
  WN_Set_Linenum( end_cond, WN_Get_Linenum(WN_end(wn)) );

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = Add_sc(NULL, SC_LP_COND);
    _sc_parent_stack->Push(sc);
  }

  BB_NODE *cond_bb = New_bb( TRUE/*connect*/ );
  Add_one_stmt(end_cond, NULL);
  cond_bb->Set_kind(BB_DOEND);
  if ( cond_bb->Labnam() == 0 )
    Append_label_map(Alloc_label(), cond_bb);

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_COND), ("Expect a SC_LP_COND"));
    sc = Add_sc(NULL, SC_LP_BODY);
    _sc_parent_stack->Push(sc);
  }

  // Third, create loop body bb
  Is_True(WN_do_body(wn) != NULL, 
	  ("CFG::Add_one_do_loop_stmt: NULL do_body pointer"));
  BB_NODE *body_bb = New_bb( TRUE/*connect*/ );
  body_bb->Set_linenum(WN_Get_Linenum(WN_do_body(wn)));
  END_BLOCK body_end;
  Add_one_stmt(WN_do_body(wn), &body_end);

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_BODY), ("Expect a SC_LP_BODY"));
    sc = Add_sc(NULL, SC_LP_STEP);
    _sc_parent_stack->Push(sc);
  }

  // Fourth, add loop increment bb
  FmtAssert(WN_step(wn) != NULL, 
	    ("CFG::Add_one_do_loop_stmt: NULL step pointer"));
  // connect from body if it doesn't break
  BB_NODE *step_bb = New_bb( body_end!=END_BREAK, BB_DOSTEP );
  Add_one_stmt( WN_step(wn), NULL );
  FmtAssert( _current_bb == step_bb,
	     ("CFG::Add_one_do_loop_stmt: step block not current block") );

  // create back edge from step to cond
  WN *gotocond = WN_CreateGoto(cond_bb->Labnam());
  Add_one_stmt( gotocond, NULL );

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_STEP), ("Expect a SC_LP_STEP"));
    sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LOOP), ("Expect a SC_LOOP"));
  }

  // here is where the merge block goes
  Append_bb( merge_bb );

  if (Cur_PU_Feedback)
    Cur_PU_Feedback->FB_lower_loop_alt( wn, end_cond );

  BB_LOOP *loopinfo = CXX_NEW(BB_LOOP(WN_index(wn),
			     start_bb,
			     cond_bb,
			     body_bb,
			     step_bb,
			     merge_bb), _mem_pool);
  LOOP_FLAGS flags = LOOP_PRE_DO;
#ifdef Is_True_On  
  // for tracking if an original DO-loop is demoted.
  if (Wn_flags(wn) & WN_FLAG_DO_LOOP)
    flags = (LOOP_FLAGS) (flags | LOOP_ORIGINAL_DO);
#endif
  if (Wn_flags(wn) & WN_FLAG_PROMOTED_DO_LOOP)
    loopinfo->Set_promoted_do();
  loopinfo->Set_flag(flags);
  loopinfo->Set_orig_wn(wn);
  start_bb->Set_loop(loopinfo);
  cond_bb->Set_loop(loopinfo);
  step_bb->Set_loop(loopinfo);

  // this statement does not break the block because we've created
  // a merge point
  if ( ends_bb )
    *ends_bb = END_NOT;

  Set_cur_loop_depth( Cur_loop_depth() - 1 );
  if (process_mp_do) {
    // top_mp_type contains loop_flag
    if (Top_mp_type() == MP_DOACROSS) flags = (LOOP_FLAGS)(flags|MP_DOACROSS);
    else if (Top_mp_type() == MP_PDO) flags = (LOOP_FLAGS)(flags|MP_PDO);
    loopinfo->Set_flag(flags);
    merge_bb->Reset_MP_region();
  }
}

// ====================================================================
// Add a WHILE_DO statement
// ====================================================================

void
CFG::Add_one_while_do_stmt( WN *wn, END_BLOCK *ends_bb )
{
  Set_cur_loop_depth( Cur_loop_depth() + 1 );

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = Add_sc(NULL, SC_LOOP);
    _sc_parent_stack->Push(sc);
    sc = Add_sc(NULL, SC_LP_COND);
    _sc_parent_stack->Push(sc);
  }

  // create loop cond bb 
  BB_NODE *cond_bb = NULL;
  // do not reuse the empty bb, because it can be a merge bb of do loop
  cond_bb = New_bb( TRUE/*connect*/ );
  cond_bb->Set_linenum(WN_Get_Linenum(wn));
  // assign a label to the condition block
  if ( cond_bb->Labnam() == 0 )
    Append_label_map( Alloc_label(), cond_bb );

  // create, but do not connect merge bb
  BB_NODE *merge_bb = Create_bb();
  Append_label_map( Alloc_label(), merge_bb );

  WN *end_cond = WN_CreateFalsebr( merge_bb->Labnam(), WN_while_test(wn) );
  WN_Set_Linenum( end_cond, WN_Get_Linenum(wn) );
  Add_one_stmt( end_cond, NULL );
  cond_bb->Set_kind( BB_WHILEEND );

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_COND), ("Expect a SC_LP_COND"));
    sc = Add_sc(NULL, SC_LP_BODY);
    _sc_parent_stack->Push(sc);
  }

  // add the loop body bb now
  BB_NODE *body_bb = New_bb( TRUE/*connect*/ );
  END_BLOCK body_end;
  Add_one_stmt( WN_while_body(wn), &body_end );

  
  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_BODY), ("Expect a SC_LP_BODY"));
    sc = Add_sc(NULL, SC_LP_BACKEDGE);
    _sc_parent_stack->Push(sc);
  }

  // create back edge to loop cond_bb
  BB_NODE *loop_back = New_bb( body_end != END_BREAK );
  // create back edge from step to cond
  WN *gotocond = WN_CreateGoto(cond_bb->Labnam());
  Add_one_stmt( gotocond, NULL );

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_BACKEDGE), ("Expect a SC_LP_BACKEDGE"));
    sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LOOP), ("Expect a SC_LOOP"));
  }

  // here is where the merge block goes
  Append_bb( merge_bb );

  if (Cur_PU_Feedback)
    Cur_PU_Feedback->FB_lower_loop_alt( wn, end_cond );

  BB_LOOP *loopinfo = CXX_NEW(BB_LOOP(NULL,	// index
			     NULL,	// start (init)
			     cond_bb,	// condition
			     body_bb,	// first body bb
			     loop_back,	// increment/loop-back
			     merge_bb),	// merge block
		      _mem_pool);
  loopinfo->Set_flag(LOOP_PRE_WHILE);
  loopinfo->Set_orig_wn(wn);
  cond_bb->Set_loop(loopinfo);

  // this statement does not break the block because we've created
  // a merge point
  if ( ends_bb )
    *ends_bb = END_NOT;

  Set_cur_loop_depth( Cur_loop_depth() - 1 );
}

// ====================================================================
// Add a DO_WHILE statement
// ====================================================================

void
CFG::Add_one_do_while_stmt( WN *wn, END_BLOCK *ends_bb )
{
  Set_cur_loop_depth( Cur_loop_depth()+1 );

  BB_NODE *body_bb = NULL;

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = Add_sc(NULL, SC_LOOP);
    _sc_parent_stack->Push(sc);
    sc = Add_sc(NULL, SC_LP_BODY);
    _sc_parent_stack->Push(sc);
  }
  
  body_bb = New_bb( TRUE/*connect*/, BB_REPEATBODY );
  body_bb->Set_linenum(WN_Get_Linenum(wn));
  if ( body_bb->Labnam() == 0 )
    Append_label_map(Alloc_label(), body_bb);

  // create blocks for the stuff within the loop
  BB_NODE *first_block = New_bb( TRUE/*connect*/ );
  END_BLOCK body_end;
  Add_one_stmt( WN_while_body(wn), &body_end );

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_BODY), ("Expect a SC_LP_BODY"));
    sc = Add_sc(NULL, SC_LP_COND);
    _sc_parent_stack->Push(sc);
  }

  // create loop cond

  // create loop cond bb, and connect from body if it doesn't goto
  BB_NODE *cond_bb = New_bb( body_end != END_BREAK );
  cond_bb->Set_linenum(WN_Get_Linenum(WN_while_test(wn)));
  WN *end_cond = WN_CreateTruebr( body_bb->Labnam(), WN_while_test(wn));
  WN_Set_Linenum( end_cond, WN_Get_Linenum(WN_while_test(wn)) );
  Add_one_stmt( end_cond, NULL );
  cond_bb->Set_kind(BB_REPEATEND);

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LP_COND), ("Expect a SC_LP_COND"));
    sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_LOOP), ("Expect a SC_LOOP"));
  }

  // Create new bb after the cond bb to be the merge block
  BB_NODE *merge_bb = New_bb( TRUE/*connect*/ );

  if (Cur_PU_Feedback)
    Cur_PU_Feedback->FB_lower_loop( wn, NULL, end_cond );

  BB_LOOP *loopinfo = CXX_NEW(BB_LOOP(NULL,	// index
				     NULL,	// start (init)
				     cond_bb,	// condition
				     body_bb,	// first body bb
				     NULL,	// increment
				     merge_bb),	//merge block
			      _mem_pool);
  loopinfo->Set_flag(LOOP_PRE_REPEAT);
  loopinfo->Set_orig_wn(wn);
  body_bb->Set_loop(loopinfo);
  cond_bb->Set_loop(loopinfo);

  // this statement does not break the block because we've created
  // a merge point
  if ( ends_bb )
    *ends_bb = END_NOT;

  Set_cur_loop_depth( Cur_loop_depth() - 1 );
}

// ====================================================================
// Add an IF statement
// ====================================================================

void
CFG::Add_one_if_stmt( WN *wn, END_BLOCK *ends_bb )
{
  // create, but do not connect, the "else" block
  BB_NODE *else_bb = Create_bb();
  Append_label_map(Alloc_label(), else_bb);

  // create if bb
  WN *end_cond = WN_CreateFalsebr(else_bb->Labnam(), WN_if_test(wn));
  WN_Set_Linenum( end_cond, WN_Get_Linenum(wn));
  BB_NODE *cond_bb = _current_bb;
  Add_one_stmt( end_cond, NULL );

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = Add_sc(cond_bb, SC_IF);
    _sc_parent_stack->Push(sc);
  }

  // create, but do not connect, the merge point
  BB_NODE *merge_bb = Create_bb();
  merge_bb->Set_ifmerge();

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = Add_sc(NULL, SC_THEN);
    _sc_parent_stack->Push(sc);
  }

  // process then block first so as maintain source order for the BBs
  BB_NODE *then_bb = New_bb( TRUE/*connect*/ );
  END_BLOCK block_end;
  Add_one_stmt( WN_then(wn), &block_end );
  if ( block_end != END_BREAK ) {
    if ( block_end == END_FALLTHRU ) {
      // always make the fall-thru block the Next one which matches
      // most people's expectations
      BB_NODE *then_fallthru = New_bb( TRUE/*connect*/, BB_GOTO );
    }

    Connect_predsucc(_current_bb, merge_bb);
  }

  if (Do_pro_loop_trans()) {
    SC_NODE * sc =_sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_THEN), ("Expect a SC_THEN"));
    sc = Add_sc(NULL, SC_ELSE);
    _sc_parent_stack->Push(sc);
  }

  // now process the "else" part using the block created earlier
  Append_bb( else_bb );

  Add_one_stmt( WN_else(wn), &block_end );
  if ( block_end != END_BREAK )
    Connect_predsucc(_current_bb, merge_bb);

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_ELSE), ("Expect a SC_ELSE"));
    sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_IF), ("Expect a SC_IF"));
  }
  
  // here is where the merge block goes
  Append_bb(merge_bb);

  if ( Cur_PU_Feedback )
    Cur_PU_Feedback->FB_lower_branch( wn, end_cond );

  BB_IFINFO *ifinfo = CXX_NEW(BB_IFINFO(WN_Get_Linenum(WN_then(wn)),
					WN_Get_Linenum(WN_else(wn)),
					cond_bb, 
					then_bb, 
					else_bb, 
					merge_bb),
			       _mem_pool);

  cond_bb->Set_ifinfo(ifinfo);

  // this statement does not break the block because we've created
  // a merge point
  if ( ends_bb )
    *ends_bb = END_NOT;
}

// ====================================================================
// Add a computed goto (switch) statement
// ====================================================================

void
CFG::Add_one_compgoto_stmt( WN *wn, END_BLOCK *ends_bb )
{
  INT32 new_num_entries = WN_num_entries(wn);

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = Add_sc(_current_bb, SC_COMPGOTO);
    _sc_parent_stack->Push(sc);
  }

  _current_bb->Set_kind(BB_VARGOTO);
  _current_bb->Set_hasujp();
  Append_wn_in(_current_bb, wn);
  _current_bb->Set_switchinfo(CXX_NEW(BB_SWITCH(new_num_entries, _mem_pool),
				      _mem_pool ) );

  // need to process the "default" block if one is provided
  if ( WN_kid_count(wn) > 2 ) {
    // a default block is provided
    WN *def_goto = WN_kid(wn,2);
    Is_True( WN_opcode(def_goto) == OPC_GOTO,
	     ("Invalid default goto in OPC_COMPGOTO") );

    BB_NODE *def_blk = Get_bb_from_label( WN_label_number(def_goto) );
    if (def_blk == NULL) {
      def_blk = Create_bb();
      Append_label_map(WN_label_number(def_goto), def_blk);
    }
    else {
      FmtAssert(!Do_pro_loop_trans(), ("Unexpected def_blk"));
    }

    _current_bb->Set_switchdefault(def_blk);
    Connect_predsucc(_current_bb, def_blk);
  }

  // need to process the "case" block 
  WN *jumptable = WN_kid1(wn);
  Is_True( WN_opcode(jumptable) == OPC_BLOCK, ("Invalid kid1 in OPC_GOTO"));
  
  WN *case_wn = WN_first(jumptable);
  INT32 case_cnt = 0;
  for ( ; case_wn != NULL; case_wn = WN_next(case_wn), case_cnt++ )
  {
    // the gotos inside a compgoto might have been converted to region exits
    // if they exit the region - this is not allowed
    Is_True( WN_opcode(case_wn) != OPC_REGION_EXIT,
	     ("CFG::Add_one_compgoto_stmt, found a region exit"));
    Is_True( WN_opcode(case_wn) == OPC_GOTO,
	     ("CFG::Add_one_compgoto__stmt, invalid statement"
	      " in OPC_COMPGOTO jump table") );
    Is_True( case_cnt < new_num_entries,
	     ("CFG::Add_one_compgoto_stmt, too many entries"
	      " in jump table block") );
    
    BB_NODE *case_blk = Get_bb_from_label( WN_label_number(case_wn) );
    if (case_blk == NULL) {
      case_blk = Create_bb();
      Append_label_map(WN_label_number(case_wn), case_blk);
    }

    _current_bb->Set_switchcase(case_blk, case_cnt);
    Connect_predsucc(_current_bb, case_blk);
  }

  // this statement breaks the block because we've created a jump at 
  // the end of the current block
  if ( ends_bb )
    *ends_bb = END_BREAK;

  if (Do_pro_loop_trans()) {
    SC_NODE * sc = _sc_parent_stack->Pop();
    FmtAssert((sc->Type() == SC_COMPGOTO), ("Expect a SC_COMPGOTO"));
  }
}

// ====================================================================
// Handle opr_io statement for Add_one_stmt().
// ====================================================================

void
CFG::Add_one_io_stmt( WN *wn, END_BLOCK *ends_bb )
{
  BOOL has_cntl_flow = FALSE;

  _current_bb->Set_hascall();
  Append_wn_in(_current_bb, wn);

  for (INT32 i = 0; i < WN_kid_count(wn); i++) {
    WN *kid = WN_kid(wn, i);

    OPCODE opcode = WN_opcode(kid);
    Is_True(OPCODE_has_inumber(opcode), 
      ("illegal OPC_IO_ITEM within iostmt"));
    if (opcode == OPC_IO_ITEM) {
      if (WN_io_item(kid) == IOC_END || WN_io_item(kid) == IOC_ERR ||
	  WN_io_item(kid) == IOC_EOR) {
	WN *kid0 = WN_kid0(kid);

	Is_True(WN_operator(kid0) == OPR_GOTO,
		("IOC_END/ERR with non-GOTO kid"));
	LABEL_IDX lab = WN_label_number(kid0);

	BB_NODE *label_bb = Get_bb_from_label(lab);
	if (label_bb == NULL) {
	  label_bb = Create_bb();
	  // the _current_bb needs to be in the label map
	  Append_label_map(lab, label_bb);
	}
	Connect_predsucc(_current_bb, label_bb);
	has_cntl_flow = TRUE;

	_current_bb->Set_kind( BB_IO );
	if ( _current_bb->IOinfo() == NULL ) {
	  _current_bb->Set_ioinfo( CXX_NEW(
	    BB_SWITCH(IO_TAB_SIZE, _mem_pool), _mem_pool ) );
	  _current_bb->Set_io_entries( 0 );
	}

	_current_bb->Set_io_bb( label_bb, _current_bb->IO_entries() );
	_current_bb->Set_io_entries( _current_bb->IO_entries()+1 );
      }
    }
  }

  if ( ends_bb ) {
    // IO statements break blocks either if there is control flow,
    // or if we've decided to break blocks at calls.
    if ( has_cntl_flow || Calls_break() )
      *ends_bb = END_FALLTHRU;
    else
      *ends_bb = END_NOT;
  }
}

// copy the xpragmas into bb
// for C++ EH regions there can be a vcall or goto statement also
// these are ignored here because they are tucked away in the bb_region node.
void
CFG::Copy_xpragmas_into(BB_NODE *bb, WN *pragmas)
{
  STMT_ITER stmt_iter;
  WN *stmt;

  //  Iterate through each statement
  FOR_ALL_ELEM (stmt, stmt_iter, Init(WN_first(pragmas), WN_last(pragmas))) {
    Is_True(WN_operator(stmt) == OPR_PRAGMA ||
            WN_operator(stmt) == OPR_XPRAGMA ||
	    WN_opcode(stmt) == OPC_VCALL ||
	    WN_operator(stmt) == OPR_GOTO,
            ("CFG::Copy_pragmas_into: statement is not "
	     "a pragma, vcall, or goto"));
    // only xpragmas and pragmas exist as code in the optimizer
    // the vcall and gotos are tucked away in the bb_region node (as whirl)
    // which gets translated to aux_ids and then back again in emitter
    if (WN_operator(stmt) == OPR_XPRAGMA || WN_operator(stmt) == OPR_PRAGMA) {
      Append_wn_in(bb, stmt);
      bb->Set_haspragma();
    }
  }
}

// ====================================================================
// Handle a region statement for Add_one_stmt().
// ====================================================================

void
CFG::Add_one_region( WN *wn, END_BLOCK *ends_bb )
{
  RID *rid = REGION_get_rid(wn);

  if (Do_pro_loop_trans()) {
    Free_sc();
    // FmtAssert(FALSE, ("TODO. region"));
  }

  Is_True(rid != NULL,("CFG::Add_one_region, cannot find RID"));
  Is_True(REGION_consistency_check(wn), (""));
  
  // Is this a region we should be handling?  Or is it a black
  // box?  If it's a black-box, we just leave it as a "region" op
  // and don't lower it further.
  if ( RID_level(rid) >= Rgn_level() ) { // black box region

    WN *wtmp;
    BB_NODE *bsave = _current_bb;

    Append_wn_in( _current_bb, wn );

    // NOTE: we don't add buffer blocks before or after black box
    // regions. For exits from a black box, we can't change the label
    // inside to break a critical edge so to prevent SSAPRE from inserting
    // into the region we link it up with the fake exit. (PV475285)

    // black box region assumed to have return for SSAPRE, connect to exit
    BB_NODE *btmp;
    btmp = New_bb(TRUE/*connect*/,BB_EXIT);
    btmp->Set_hasujp();
    _current_bb = bsave;

    // go through black box region exits, connect up to CFG
    for (wtmp=WN_first(WN_region_exits(wn)); wtmp!=NULL; wtmp=WN_next(wtmp)) {
      INT32 label = WN_label_number(wtmp);
      BB_NODE *label_bb = Get_bb_from_label(label);
      if (label_bb == NULL) {
	label_bb = Create_bb(/*BB_GOTO*/);
	// it needs to be in the label map
	Append_label_map(WN_label_number(wtmp), label_bb);
      }
      Connect_predsucc(bsave, label_bb);
    }

    if ( ends_bb )
      *ends_bb = END_FALLTHRU;

    return;
  }

  // this program has lowered regions in it
  Set_has_regions();

  // add an extra empty block before the region for code that is
  // to be inserted before a region (e.g. a region is the first
  // statement in a loop and we want to insert secondary IV code
  // at the beginning of the loop body) This block is often empty.
  // Lastidx will be -1 for region, 0 or more for pu.
  BB_NODE *first_region_bb;
  if (RID_id(rid) != RID_id(Rid())) {
    if (_current_bb->Firststmt() != NULL)
      _current_bb = New_bb(TRUE/*connect*/, BB_GOTO);
    else
      _current_bb->Set_kind(BB_GOTO); // reuse empty block
    first_region_bb = New_bb(TRUE/*connect*/, BB_REGIONSTART);
  } else { // start off region with first block if just a region
    // if the current_bb has something in it, start another block
    if (_current_bb->Firststmt() != NULL)
      (void) New_bb(TRUE/*connect*/, BB_REGIONSTART);
    else 
      _current_bb->Set_kind(BB_REGIONSTART);
    first_region_bb = _current_bb;
  }
  first_region_bb->Set_linenum(WN_Get_Linenum(wn));

  // create bb_region node so BB_REGIONEXITs have something to connect to
  BB_REGION *bb_region = CXX_NEW(BB_REGION(first_region_bb, NULL, rid,
	   NULL, WN_region_pragmas(wn), WN_region_exits(wn),
	   WN_ereg_supp(wn), WN_Get_Linenum(wn), wn), _mem_pool);
  first_region_bb->Set_regioninfo(bb_region);
  Push_bb_region(bb_region);

  if (REGION_is_mp(wn)) {
    if ( Is_region_with_pragma(wn,WN_PRAGMA_DOACROSS) ||
	 Is_region_with_pragma(wn,WN_PRAGMA_PARALLEL_DO) )
      Push_mp_type(MP_DOACROSS);
    else if ( Is_region_with_pragma(wn,WN_PRAGMA_PDO_BEGIN) )
      Push_mp_type(MP_PDO);
    else
      Push_mp_type(MP_REGION);
    Push_mp_rid(rid);
  }

#if defined(TARG_SL)
  if (REGION_is_sl2_para(wn)) {
    Push_sl2_para_type(SL2_PARA_REGION);
    Push_sl2_para_rid(rid);
  }
#endif

  // create a block to hold the first statement
  BB_NODE *first_body_bb = New_bb( TRUE/*connect*/ );

  END_BLOCK block_end;
  Add_one_stmt( WN_region_body(wn), &block_end );

  if (REGION_is_mp(wn)) {
    // add an empty region exit block for MP regions
    if (_current_bb->Kind() != BB_REGIONEXIT)
      (void) New_bb(TRUE, BB_REGIONEXIT);
    Pop_mp_type();
    Pop_mp_rid();
  }
#if defined(TARG_SL)
  if (REGION_is_sl2_para(wn)) {
    // add an empty region exit block for MP regions
    if (_current_bb->Kind() != BB_REGIONEXIT)
      (void) New_bb(TRUE, BB_REGIONEXIT);
    Pop_sl2_para_type();
    Pop_sl2_para_rid();
  }
#endif

  if (OPT_Enable_EH_CFG_OPT && REGION_is_EH(wn)) 
  {
    if (_current_bb->Succ() ||
        _current_bb->Kind() == BB_EXIT)
        (void) New_bb(FALSE, BB_REGIONEXIT);
    else
        (void) New_bb(TRUE, BB_REGIONEXIT);
  }
    
  Pop_bb_region();

  // remember the last block in the region
  BB_NODE *last_region_bb = _current_bb;
    
  // bug 8690
  if (REGION_is_mp(wn) && _rgn_level != RL_MAINOPT &&
      Is_region_with_pragma(wn,WN_PRAGMA_SINGLE_PROCESS_BEGIN)) {
    // add extra edge in the cfg to reflect jump around the single region
    Connect_predsucc( first_region_bb, last_region_bb );
  }

  // last pieces of missing information
  bb_region->Set_region_end(last_region_bb);
  bb_region->Set_parent(Null_bb_region() ? NULL : Top_bb_region());

  // regioninfo for bb, first and last blocks
  if (last_region_bb->Kind() == BB_REGIONEXIT ||
      last_region_bb->Kind() == BB_EXIT)
    last_region_bb->Set_regioninfo(bb_region); // last block points to it also

  // XPRAGMAS and PRAGMAS go into BB_REGION_START, everything else
  // (VCALL and GOTO for EH regions) stored as WHIRL in bb_region.
  Copy_xpragmas_into(first_region_bb, WN_region_pragmas(wn));

  // unmap this "lowered" region from the original wn
  WN_MAP_Set( RID_map, wn, NULL );
  RID_rwn(rid) = NULL;

  // this ends the block
  if ( ends_bb ) {
    if ( block_end == END_BREAK )
      *ends_bb = END_BREAK;
    else
      *ends_bb = END_FALLTHRU;
  }
}

BB_NODE*
CFG::Process_entry( WN *wn, END_BLOCK *ends_bb )
{
  BB_NODE *retv;
  BB_NODE *last_bb = _current_bb;
  // entry must be the only thing in the block, and cannot have
  // predecessors
  if ( _current_bb->Firststmt() != NULL || 
      (_current_bb->Pred() && _current_bb->Pred()->Len() > 0) )
    {
      (void)New_bb( FALSE/*!connect*/, BB_ENTRY );
    }
  else
    _current_bb->Set_kind(BB_ENTRY);
      
  // this is an entrypoint
  Add_altentry(_current_bb);

  // make a copy of the func_entry, none of the original WHIRL can be saved
  WN *copy_wn = WN_CopyNode(wn);

  // This wn is a OPR_FUNC_ENTRY node. If we copy the map-id directly, we don't
  // remove the id from the free-list. OPR_REGION and OPR_FUNC_ENTRY are the 
  // only 2 operators in the same annotation category. So an OPR_REGION picks
  // up the first free-list entry which turns out to be the same as the one
  // in OPR_FUNC_ENTRY.
  WN_COPY_All_Maps(copy_wn, wn);

  if ( Cur_PU_Feedback )
    Cur_PU_Feedback->FB_duplicate_node( wn, copy_wn );

  const OPERATOR opr = WN_operator(wn);

  // get a copy of the tree
  if (opr != OPR_LABEL) {
    INT kids_to_copy;
    Is_True(opr == OPR_FUNC_ENTRY || opr == OPR_ALTENTRY,
	    ("CFG::Process_entry, Illegal Operator"));
    if ( opr == OPR_FUNC_ENTRY )
      kids_to_copy = WN_num_formals(wn);
    else // OPR_ALTENTRY
      kids_to_copy = WN_kid_count(wn);

    for (INT i = 0; i < kids_to_copy; i++)
      WN_formal(copy_wn, i) = WN_CopyNode(WN_formal(wn, i));

    if (opr == OPR_FUNC_ENTRY) { // save pragmas and varrefs also
      WN_func_pragmas(copy_wn) = WN_COPY_Tree_With_Map(WN_func_pragmas(wn));
      WN_func_varrefs(copy_wn) = WN_COPY_Tree_With_Map(WN_func_varrefs(wn));
      WN_func_body(copy_wn) = WN_CreateBlock();
    }
  }
  else {
    WN_label_flag(copy_wn) = WN_label_flag(wn);
    Append_label_map(WN_label_number(copy_wn), _current_bb);
  }
  _current_bb->Set_entrywn(copy_wn);
  retv = _current_bb;

  // start a new block for subseqent statements or the body
  (void)New_bb( TRUE/* connect*/ );

  if ( opr == OPR_FUNC_ENTRY ) {
    // and process the function body
    FmtAssert(WN_func_body(wn) != NULL, 
              ("CFG::Process_entry: NULL body pointer"));

    Add_one_stmt( WN_func_body(wn), NULL );

    // keep us from following down this kid again
    WN_func_body(wn) = NULL;

    // and even though we should never see this statement in a block,
    // we'll go ahead and say that after we're done, we should start
    // a new block.
    if ( ends_bb ) *ends_bb = END_BREAK;
  }
  else/* if (opr == OPR_ALTENTRY || opr == OPR_LABEL)*/ {
    if ( ends_bb ) *ends_bb = END_NOT;
#ifdef KEY
    if (opr == OPR_LABEL && 
        LABEL_target_of_goto_outer_block(WN_label_number(wn)))
      Connect_predsucc(last_bb, _current_bb);
#endif
  }
  return retv;
}

// ====================================================================
// Break the whirl tree into optimizer's basic-blocks.  
// Out parameters: 
//   ends_bb indicates whether this statement should break the block 
//   it's added to.
// ====================================================================



BOOL CFG::bottom_test_loop(WN* scf_loop){
  
  Is_True(scf_loop != NULL,("NULL passed to bottom_test_loop\n"));

  const OPCODE   opc = WN_opcode(scf_loop);
  const OPERATOR opr = OPCODE_operator(opc);
  
  Is_True(opr == OPR_WHILE_DO || opr == OPR_DO_LOOP,
          ("%s not supported, only handle OPR_WHILE_DO or OPR_DO_LOOP\n",OPCODE_name(opc)));

  WN *do_loopinfo = NULL;
  WN *do_tripcount = NULL;

  // for const tripcount(>0) do_loop , we always do bottom_test_loop
  if ( (opr == OPR_DO_LOOP) && 
       (do_loopinfo = WN_do_loop_info(scf_loop)) &&
       (do_tripcount = WN_loop_trip(do_loopinfo)) &&
       (WN_operator(do_tripcount) == OPR_INTCONST) &&
       (WN_const_val(do_tripcount) > 0)) {
    if (Trace()) {
      fprintf(TFile,"bottom_test_loop:\n");
      fdump_tree_no_st(TFile,scf_loop);
    }
    return TRUE;
  }


  WN* loop_condition = (opr == OPR_WHILE_DO) ? WN_while_test(scf_loop) : WN_end(scf_loop);
  WN* loop_body = (opr == OPR_WHILE_DO) ? WN_while_body(scf_loop) : WN_do_body(scf_loop) ;

  const OPCODE cond_op = WN_opcode(loop_condition);
  const OPCODE body_op = WN_opcode(loop_body);
  const OPERATOR opr_body = OPCODE_operator(body_op);

  Is_True(OPCODE_is_expression(cond_op), 
	  ("Bad condition opcode %s\n", 
	   OPCODE_name(cond_op)));
  Is_True(opr_body == OPR_BLOCK, 
	  ("Bad Body block opcode %s\n",
	   OPCODE_name(body_op)));

  WN_TREE_CONTAINER<PRE_ORDER>  wcpre(loop_condition);
  WN_TREE_CONTAINER<PRE_ORDER> ::iterator wipre;
  WN_count wc;
  INT32 stmt_count=0;

  for (wipre = wcpre.begin(); wipre != wcpre.end(); ++wipre)
    wc(wipre.Wn());
  if (wc.num_nodes >= WOPT_Bottom_Test_Loop_Cond_Limit) {
    for ( WN *stmt = WN_first(loop_body); stmt != NULL; stmt = WN_next(stmt) ) {
      const OPCODE op_in_body = WN_opcode(stmt);
      if( ! OPCODE_is_stmt(op_in_body) ) {
	if (Trace()) {
	  fprintf(TFile,"bottom_test_loop:\n");
	  fdump_tree_no_st(TFile,scf_loop);
	}
	return TRUE;
      }
      if ( ! OPCODE_has_label(op_in_body)) {
	stmt_count++;
	if (stmt_count >= WOPT_Bottom_Test_Loop_Body_Limit) {
	  if (Trace()) {
	    fprintf(TFile,"bottom_test_loop:\n");
	    fdump_tree_no_st(TFile,scf_loop);
	  }
	  return TRUE;
	}
      }
    }
    if (Trace()) {
	  fprintf(TFile,"non bottom_test_loop:\n");
	  fdump_tree_no_st(TFile,scf_loop);
    }
    return FALSE;
  }
  else {
    if (Trace()) {
      fprintf(TFile,"bottom_test_loop:\n");
      fdump_tree_no_st(TFile,scf_loop);
    }
    return TRUE;
  }
}


void
CFG::Add_one_stmt( WN *wn, END_BLOCK *ends_bb )
{
  BB_NODE *label_bb;

  if ( ends_bb )
    *ends_bb = END_UNKNOWN;

  if (wn == NULL) return;

  const OPCODE   opc = WN_opcode(wn);
  const OPERATOR opr = OPCODE_operator(opc);

  switch ( opr ) {

  case OPR_BLOCK:
    if ( WN_first(wn) != NULL ) {
      END_BLOCK endsbb = END_NOT;
      WN *nextstmt = NULL;
      for ( WN *stmt = WN_first(wn); stmt != NULL; stmt = nextstmt ) {
	nextstmt = WN_next(stmt);
	WN_next(stmt) = NULL;
	WN_prev(stmt) = NULL;
	Add_one_stmt( stmt, &endsbb );

        if (WN_operator(stmt) == OPR_CALL && WOPT_Enable_Noreturn_Attr_Opt) {
          if (PU_has_attr_noreturn(Pu_Table[ST_pu(WN_st(stmt))])) {
            // Insert OPC_RETURN after the call, so the rest statements
            // will be ignored
            if (!nextstmt || 
                (WN_operator(nextstmt) != OPR_RETURN && 
                 WN_operator(nextstmt) != OPR_RETURN_VAL)) {
                WN *nextstmt_save = nextstmt;
                nextstmt = WN_Create (OPC_RETURN, 0);
                WN_next(nextstmt) = nextstmt_save;
		if (nextstmt_save)
                  WN_prev(nextstmt_save) = nextstmt;
            }
          }
        }

	// For RVI's temporary use, break blocks at EVERY statement
	if ( endsbb == END_NOT && Rvi_break_stmt() )
	  endsbb = END_FALLTHRU;

	if ( endsbb == END_FALLTHRU || endsbb == END_BREAK ) {
	  // only need to break the block if there is another statement
	  if ( nextstmt != NULL ) {
	    // if the current block ends with a call, then the next
	    // block may have call-related code at the top
	    BOOL need_to_set_callrel = FALSE;
	    if ( Calls_break() && _current_bb->Hascall() ) {
	      need_to_set_callrel = TRUE;
	    }

	    // do something about forward labels
	    BB_NODE *label_bb;
	    if ( WN_opcode(nextstmt) == OPC_LABEL &&
		 (label_bb = Get_bb_from_label(WN_label_number(nextstmt))) )
	    {
	      // should we connect this fwd-declared bb to cur one?
	      if ( endsbb == END_FALLTHRU ) {
		Connect_predsucc( _current_bb, label_bb );

	        // this block may have call-related code at the top
		if ( need_to_set_callrel )
		  label_bb->Set_callrel();
	      }
	    }
	    else {
	      (void)New_bb( endsbb == END_FALLTHRU );

	      // this block may have call-related code at the top
	      if ( need_to_set_callrel )
		_current_bb->Set_callrel();
	    }
	  }
	  else {
	    // no more statements
	    if ( endsbb == END_FALLTHRU ) {
	      // provide a fall-thru block so conditional branches
	      // at end of blocks have a place to fall to
	      if ( _current_bb->Kind() ==  BB_LOGIF ) {
		BB_NODE *fallthru = New_bb( TRUE/*connect*/ );
		endsbb = END_NOT;
	      }
	    }
	  }
	}
	else {
	  Is_True( endsbb != END_UNKNOWN,
	    ("CFG::Add_one_stmt: unknown kind of block ending") );
	}
      }
      // this block ends however the last statement said it does
      if ( ends_bb )
	*ends_bb = endsbb;
    }
    else {
      FmtAssert(WN_last(wn) == NULL,
		("CFG::Add_one_stmt: mismatching block statements"));
      if ( ends_bb )
	*ends_bb = END_NOT;
    }
    // OPR_BLOCK doesn't go in a specific BB, so we don't do anything
    // here about feedback info. Recursive invocations will handle it.
    break;

  case OPR_FUNC_ENTRY:
  case OPR_ALTENTRY:
    Process_entry( wn, ends_bb );
    break;

  case OPR_DO_LOOP:
    Create_empty_preheader (wn);
    if (Lower_fully()) {
      if ( OPT_Space && WOPT_Bottom_Test_Loop_Check ) {
        if ( ! bottom_test_loop(wn) ) {
          Add_one_do_loop_stmt( wn, ends_bb );
        }
        else {
          Lower_do_loop( wn, ends_bb );
        }
      }
      else {
        Lower_do_loop( wn, ends_bb );
      }
    }
    else
      Add_one_do_loop_stmt( wn, ends_bb );
    break;

  case OPR_WHILE_DO:
    Create_empty_preheader (wn);
    if (Lower_fully()) {
      if (OPT_Space && WOPT_Bottom_Test_Loop_Check) {
        if ( ! bottom_test_loop(wn)) {
	   Add_one_while_do_stmt(wn, ends_bb);
	}
	else {
	  Lower_while_do( wn, ends_bb );
	}
      }
      else {
	Lower_while_do( wn, ends_bb );
      }
    }
    else {
      Add_one_while_do_stmt( wn, ends_bb );
    }
    break;

  case OPR_DO_WHILE:
    Create_empty_preheader (wn);
    if (Lower_fully())
      Lower_do_while( wn, ends_bb );
    else
      Add_one_do_while_stmt( wn, ends_bb );
    break;

  case OPR_IF:
    if (Lower_fully()) {
      Lower_if_stmt( wn, ends_bb );
    }
    else {
      Add_one_if_stmt( wn, ends_bb );
    }
    break;

  case OPR_GOTO:
    label_bb = Get_bb_from_label( WN_label_number(wn) );
    if (label_bb == NULL) {
      label_bb = Create_bb();
      // it needs to be in the label map
      Append_label_map(WN_label_number(wn), label_bb);
    }
    Append_wn_in(_current_bb, wn);
    _current_bb->Set_hasujp();
    Connect_predsucc(_current_bb, label_bb);
    if ( ends_bb )
      *ends_bb = END_BREAK;
    break;

  case OPR_AGOTO:
    _agoto_pred_vec.AddElement( _current_bb );
    Append_wn_in(_current_bb, wn);
    _current_bb->Set_hasujp();
    _current_bb->Set_kind(BB_VARGOTO);
    if ( ends_bb )
      *ends_bb = END_BREAK;
    break;

  case OPR_ZDLBR:
    label_bb = Get_bb_from_label(WN_label_number(wn));
    FmtAssert(label_bb != NULL, ("CFG::Add_one_stmt: ZDLBR does not have a label_bb"));

    Connect_predsucc(_current_bb, label_bb);
    Append_wn_in(_current_bb, wn); 
    _current_bb->Set_kind(BB_LOGIF);

    if (ends_bb)
      *ends_bb = END_FALLTHRU;
    break;

  case OPR_FALSEBR:
  case OPR_TRUEBR:
    label_bb = Get_bb_from_label( WN_label_number(wn) );
    if (label_bb == NULL) {
      label_bb = Create_bb();
      // the _current_bb needs to be in the label map
      Append_label_map(WN_label_number(wn), label_bb);
    }

    if ( Lower_fully() &&
	 (WN_operator(WN_kid0(wn)) == OPR_CAND ||
	  WN_operator(WN_kid0(wn)) == OPR_CIOR) )
    {
      WN *wn_branch;
      if ( opr == OPR_TRUEBR )
	Create_conditional( WN_kid0(wn), label_bb, NULL, TRUE,  &wn_branch);
      else
	Create_conditional( WN_kid0(wn), NULL, label_bb, FALSE, &wn_branch);

      // Lower feedback data for TRUEBR or FALSEBR
      if ( Cur_PU_Feedback ) {
	Cur_PU_Feedback->FB_lower_branch( wn, wn_branch );
      }
    }
    else {
      Connect_predsucc(_current_bb, label_bb);
      Append_wn_in(_current_bb, wn);
      _current_bb->Set_kind(BB_LOGIF);
    }

    if ( ends_bb )
      *ends_bb = END_FALLTHRU;
    break;

  case OPR_RETURN:
  case OPR_RETURN_VAL:
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
    {
    Append_wn_in(_current_bb, wn);
    _current_bb->Set_hasujp();
    // returns are always BB_EXIT, even if return is in a region
    _current_bb->Set_kind( BB_EXIT );

    // fix up bb_region if it exists
    BB_REGION *bb_region = Null_bb_region() ? NULL : Top_bb_region();
    _current_bb->Set_regioninfo(bb_region);

    if ( ends_bb )
      *ends_bb = END_BREAK;
    }
    break;

  case OPR_LABEL:
    {
      // Look up the _label_map to see if it's forward declared by goto
      if (WN_Label_Is_Handler_Begin(wn)
#ifdef KEY
	  || LABEL_target_of_goto_outer_block(WN_label_number(wn))
#endif
	 ) {
	// make sure we haven't already seen a goto to this label
	// because we are not inserting the real label.
	FmtAssert( Get_bb_from_label( WN_label_number(wn) ) == NULL,
	  ("Exception Handler Label is target of goto statement") );

        Process_entry( wn, ends_bb );
	break;
      }

      label_bb = Get_bb_from_label( WN_label_number(wn) );
      if ( label_bb != NULL ) {
        Is_True( label_bb->Firststmt() == NULL,
                ("Non-empty block assigned to a label") );
        // if we're trying to add this label to a non-empty block, we
        // need to end the current block, and connect the new one to it

        // fix bug869(open64.net), check to see whether last statement of _current_bb 
        // is OPR_FALSEBR/OPR_TRUEBR, and the statment's label bb is label_bb.
        // if so, add the null fall-through bb and set it be the current bb
        WN* last_wn=_current_bb->Laststmt();
        if (last_wn &&
            (OPCODE_operator(WN_opcode(last_wn)) == OPR_FALSEBR ||
             OPCODE_operator(WN_opcode(last_wn)) == OPR_TRUEBR) &&
            Get_bb_from_label( WN_label_number(last_wn)) == label_bb) {
          BB_NODE* fall_through_bb= Create_bb();
          Connect_predsucc(_current_bb,fall_through_bb);
          Append_bb(fall_through_bb);
            }
        if ( ! _current_bb->Hasujp() )
          Connect_predsucc( _current_bb, label_bb );
        Append_bb( label_bb );
      }
      else if ( _current_bb->Firststmt() != NULL ||
               _current_bb->Labnam() != 0 ||
               First_bb()->Next() == _current_bb )
        {
          // non-empty block, or it's already been assigned a label num, 
          // so need a new one
          label_bb = New_bb( TRUE/*connect*/ );
          Append_label_map(WN_label_number(wn), label_bb);
        }
      else {
        // empty block, so let's use this one
        Is_True( _current_bb->Labnam() == 0,
                ("CFG::Add_one_stmt: empty block has label assigned") );
        label_bb = _current_bb;
        Append_label_map(WN_label_number(wn), label_bb);
      }
      Is_True( _current_bb == label_bb,
              ("Label block not same as current block") );
      label_bb->Set_linenum(WN_Get_Linenum(wn));
      if ( WN_label_loop_info(wn) != NULL ) {
        WN *loop_info = WN_COPY_Tree_With_Map(WN_label_loop_info(wn));
        label_bb->Set_label_loop_info(loop_info);
      }

      // Check if label could be the target of an AGOTO
      if ( LABEL_addr_saved( label_bb->Labnam() ) ) {
	_agoto_succ_vec.AddElement( label_bb );
      }

      Append_wn_in( label_bb, wn );
      if ( ends_bb ) *ends_bb = END_NOT;
    }
    break;
      
  case OPR_COMPGOTO:
    Add_one_compgoto_stmt( wn, ends_bb );
    break;

  case OPR_CALL:
  case OPR_ICALL:
  case OPR_PICCALL:
    if (_exc) // register each call with the exception region
      _exc->Link_top_es(wn);
    _current_bb->Set_hascall();
    Append_wn_in(_current_bb, wn);
    if (ends_bb) {
      *ends_bb = Calls_break() ? END_FALLTHRU : END_NOT;
    }
    break;

  case OPR_INTRINSIC_CALL:
    _current_bb->Set_hascall();
    Append_wn_in(_current_bb, wn);
    if (WN_Call_Never_Return(wn)) {
      _current_bb->Set_kind( BB_EXIT );
      _current_bb->Set_hasujp();
      if ( ends_bb )
	*ends_bb = END_BREAK;
    } else {
      if ( ends_bb )
	*ends_bb = Calls_break() ? END_FALLTHRU : END_NOT;
    }
    break;

  case OPR_IO:
    // input/output statements are odd things involving calls
    // and calls may break the block.  Also useful because there can
    // be unstructured gotos in this statement.
    Add_one_io_stmt( wn, ends_bb );
    break;

  case OPR_REGION:
    { EXC_SCOPE *exc_scope;

      // _exc is only defined for mainopt, not rvi
      if (REGION_is_EH(wn) && _exc) {
	// push the exc scope and set pointer from exc_scope to wn
	exc_scope = _exc->Push_exc_scope(wn);
	exc_scope->Set_begin_wn(wn);
	// begin_wn is set in the Push_exc_scope
	// set the pointer from wn to exc_scope
	_exc->Link_wn_es(wn, exc_scope);
      }

      // all regions do this
      Add_one_region( wn, ends_bb );

      if (REGION_is_EH(wn) && _exc) {
        exc_scope = _exc->Pop_exc_scope();
      }
    }
    break;

  case OPR_PRAGMA:
  case OPR_XPRAGMA:
    _current_bb->Set_haspragma();
    Append_wn_in(_current_bb, wn);
    if ( ends_bb )
      *ends_bb = END_NOT;
    break;

  case OPR_REGION_EXIT:
  {
    // Decide if the label that the region exit goes to is inside the
    // region, if so we create a label_bb for it.
    // Look at the exits for the region being processed (this->Rid())
    // and see if any of the labels are there, if so we don't create a bb here.
    // REGION_scan_exits returns TRUE if it found the label.
    BB_REGION *bb_region = Null_bb_region() ? NULL : Top_bb_region();
    BOOL create_label_bb = TRUE; // label is assumed internal
    if (!RID_TYPE_func_entry(Rid()) && bb_region) {
      Is_True(Rid() && RID_rwn(Rid()) &&
	      WN_opcode(RID_rwn(Rid())) == OPC_REGION &&
	      WN_region_exits(RID_rwn(Rid())) &&
	      WN_opcode(WN_region_exits(RID_rwn(Rid()))) == OPC_BLOCK,
	      ("CFG::Add_one_stmt, invalid assumption about region exit"));
      WN *exit_blk;
      exit_blk = WN_region_exits(RID_rwn(Rid()));
      // find out if label is really internal or goes outside
      create_label_bb = !REGION_scan_exits(exit_blk, WN_label_number(wn));
    }

    label_bb = Get_bb_from_label( WN_label_number(wn) );
    if (label_bb == NULL && create_label_bb) {
      label_bb = Create_bb(); // this label_bb is filled in later
      Append_label_map(WN_label_number(wn), label_bb); // add to label map
    }
    if (label_bb) // either we processed the label earlier or just created one
      Connect_predsucc(_current_bb, label_bb);

    Append_wn_in(_current_bb, wn);
    _current_bb->Set_kind(BB_REGIONEXIT);
    _current_bb->Set_regioninfo(bb_region);
    _current_bb->Set_hasujp();

    if ( ends_bb )
      *ends_bb = END_BREAK;
  }
    break;

  default:
    // should have no control stmt get here
    FmtAssert(!OPCODE_is_scf(opc),
	      ("CFG::Add_one_stmt:CTRL opcode %s is not handled yet",
	       OPCODE_name(opc)) );
    // advancing my last statement of the current bb
    Append_wn_in(_current_bb, wn);

    if ( ends_bb )
      *ends_bb = END_NOT;
    break;
  }

}

// ====================================================================
// connect all agoto blocks to all labels that could be targets
// ====================================================================

void
CFG::Connect_agotos()
{
  INT i, j;

  // Print to trace file
  if ( Trace() ) {
    fprintf(TFile, "_agoto_pred_vec:");
    for (i = 0; i <= _agoto_pred_vec.Lastidx(); i++) {
      fprintf(TFile, " %d", _agoto_pred_vec[i]->Id());
    }
    fprintf(TFile, "\n_agoto_succ_vec:");
    for (j = 0; j <= _agoto_succ_vec.Lastidx(); j++) {
      fprintf(TFile, " %d", _agoto_succ_vec[j]->Id());
    }
    fprintf(TFile, "\n");
  }

  // Connect agotos
  for (i = 0; i <= _agoto_pred_vec.Lastidx(); i++) {
    for (j = 0; j <= _agoto_succ_vec.Lastidx(); j++) {
      Connect_predsucc(_agoto_pred_vec[i], _agoto_succ_vec[j]);
    }
  }
}

// Query whether a SC_NODE of given type has representing BB_NODE.
BOOL
SC_type_has_rep(SC_TYPE type)
{
  return ((type == SC_IF) || (type == SC_COMPGOTO));
}

// Query whether a SC_NODE of given type has a list of BB_NODEs.
BOOL
SC_type_has_bbs(SC_TYPE type)
{
  return (type == SC_BLOCK);
}

// Query whether a SC_NODE of given type is a marker, i.e., it does not
// contain any BB_NODE and only serves as a delimitator.

BOOL
SC_type_is_marker(SC_TYPE type)
{
  return ((type == SC_THEN) || (type == SC_ELSE) || (type == SC_LOOP) || (type == SC_LP_BODY)
	  || (type == SC_LP_START) || (type == SC_LP_COND) || (type == SC_LP_STEP)
	  || (type == SC_LP_BACKEDGE));
}

// Allocate a SC_NODE of the given type.
SC_NODE *
CFG::Create_sc(SC_TYPE type)
{
  SC_NODE * sc = CXX_NEW(SC_NODE(), _sc_pool);
  sc->Set_type(type);
  sc->Set_id(++_last_sc_id);
  sc->Set_pool(_sc_pool);

  return sc;
}

// Initialize scratch fields to build a SC tree.
void
CFG::SC_init()
{
  _sc_root = NULL;
  _sc_parent_stack = NULL;
  _sc_map = NULL;
  _last_sc_id = 0;
}

// Remove BB_NODE from SC tree.  Used during SC_TREE construction.

SC_NODE *
CFG::Unlink_sc(BB_NODE *bb)
{
  SC_NODE * sc = this->Get_sc_from_bb(bb);

  if (sc == NULL)
    return NULL;

  SC_TYPE type = sc->Type();

  if (SC_type_has_bbs(type)) {
    BB_LIST * bb_list = sc->Get_bbs();
    if (bb_list->Multiple_bbs()) {
      bb_list->Remove(bb, sc->Get_pool());
      this->Remove_sc_map(bb,sc);
      return NULL;
    }
  }

  SC_NODE * parent = sc->Parent();

  FmtAssert((sc->Kids() == NULL), ("Expect a leaf SC_NODE"));

  parent->Remove_kid(sc);
  sc->Set_parent(NULL);
  
  return sc;
}

// Add BB_NODE to SC tree. Used during SC_TREE construction.

SC_NODE *
CFG::Add_sc(BB_NODE *bb, SC_TYPE type)
{
  FmtAssert((_sc_parent_stack != NULL), ("Expect non NULL sc parent"));

  SC_NODE * parent = _sc_parent_stack->Top();
  SC_NODE * sc = (bb != NULL) ? this->Get_sc_from_bb(bb) : NULL;
  BOOL connect_parent_kid = TRUE;

  if (sc != NULL) {
    // Handle the case that a condition-testing expression was appended to an
    // existing bb.  If the bb belongs to a SC_BLOCK containing a single bb,
    // convert the bb into a SC_IF.  If the sc contains more than one bbs, 
    // remove the bb from the sc.

    if (SC_type_has_rep(type)) {
      SC_TYPE old_type = sc->Type();
      BB_LIST * bb_list = sc->Get_bbs();
      FmtAssert(SC_type_has_bbs(old_type), ("Unexpected SC type"));
      FmtAssert(bb_list && (bb_list->Contains(bb)), ("Unexpected bbs"));
      
      if (bb_list->Multiple_bbs()) {
	bb_list->Remove(bb, sc->Get_pool());
	this->Remove_sc_map(bb, sc);
	sc = NULL;
      }
      else 
	sc->Convert(type);
    }
  }

  if (sc != NULL)
    return sc;


  if (SC_type_has_rep(type)) {
    sc = Create_sc(type);
    sc->Set_bb_rep(bb);
  }
  else if (SC_type_has_bbs(type)) {
    sc = parent->Last_kid();

    if ((sc == NULL) || (sc->Type() != SC_BLOCK)
	|| (sc->Type() != type)) {
      sc = Create_sc(type);
    }
    else {
      connect_parent_kid = FALSE;
    }
    sc->Append_bbs(bb);
  }
  else if (SC_type_is_marker(type)) {
    sc = Create_sc(type);
  }
  else {
    FmtAssert(FALSE, ("Unexpected SC type"));
  }

  if (bb != NULL)
    this->Add_sc_map(bb, sc);

  if (connect_parent_kid) {
    sc->Set_parent(parent);
    parent->Append_kid(sc);
  }

  // For COMPGOTO, BBs of case statements and default statement are appended to the CFG
  // when the label WNs are processed, though the pred/succ are connected at the time
  // the BBs are created.
  //
  if (parent && (parent->Type() == SC_COMPGOTO)) {
    BB_NODE * parent_bb = parent->Get_bb_rep();
    BB_LIST * succ = parent_bb->Succ();
    BB_LIST_ITER bb_list_iter(succ);
    BOOL all_processed = TRUE;
    BB_NODE * tmp;

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (this->Get_sc_from_bb(tmp) == NULL) {
	all_processed = FALSE;
	break;
      }
    }

    if (all_processed)
      _sc_parent_stack->Pop();
  }

  return sc;
}

// Free SC storages.
void
CFG::Free_sc()
{
  if (_sc_map) {
    CXX_DELETE(_sc_map, _sc_pool);
    _sc_map = NULL;
  }

  if (_sc_parent_stack) {
    CXX_DELETE(_sc_parent_stack, _sc_pool);
    _sc_parent_stack = NULL;
  }
    
  _sc_root->Delete();
  _sc_root = NULL;
}

// ====================================================================
// Remove bb from the list of blocks containing AGOTOs (used during DCE)
// ====================================================================

void
CFG::Remove_agoto_pred( BB_NODE *bb )
{
  mUINT32 lastidx = _agoto_pred_vec.Lastidx();
  for ( mUINT32 idx = 0; idx <= lastidx; ++idx ) {
    if ( _agoto_pred_vec[idx] == bb ) {
      // Replace bb by last element, then free last idx
      _agoto_pred_vec[idx] = _agoto_pred_vec[lastidx];
      _agoto_pred_vec.Decidx();
      return;
    }
  }
  Is_True( FALSE, ( "CFG::Remove_agoto_pred: bb not found" ) );
}

// ====================================================================
// Return the number of blocks containing potential agoto target labels
// ====================================================================

INT32
CFG::Agoto_succ_entries()
{
  return _agoto_succ_vec.Elements();
}

// ====================================================================
// Return the idx'th block containing a potential agoto target labels
// ====================================================================

BB_NODE *
CFG::Agoto_succ_bb(INT32 idx)
{
  Is_True( idx >= 0 && idx <= _agoto_succ_vec.Lastidx(),
	   ( "CFG::Agoto_succ_bb found out of range idx %d (range 0...%d)",
	     idx, _agoto_succ_vec.Lastidx() ) );
  return _agoto_succ_vec[idx];
}

// ====================================================================
// process multiple entry or exit blocks.  If redo_exits is true, we
// recalculate the _exit_vec[].
// ====================================================================
void
CFG::Process_multi_entryexit( BOOL is_whirl )
{
  Is_Trace(Trace(), (TFile,"CFG::Process_multi_entryexit\n"));

  // For our iurposes "is_whirl" also means we can disconnect
  // unreachable blocks because we have not yet inserted phi nodes.
  Process_not_reached( is_whirl );

  Is_True( _entry_vec.Lastidx() >= 0,
    ("CFG::Process_multi_entryexit: No entrypoints") );

  // how many entrypoints and notreached blocks are there?
  if ( Fake_entry_bb() == NULL &&
       _entry_vec.Lastidx() == 0 && _notreach_vec.Lastidx() < 0 )
  {
    // single entry, and no not-reached blocks
    _entry_bb = _entry_vec[0];
  } else {
    // need (or already have) a fake-entry block because there are 
    // either multiple entrypoints to the region, or there are blocks 
    // that are not reached from an entrypoint
    if ( Fake_entry_bb() == NULL ) {
      _fake_entry_bb = New_bb( FALSE/*!connect*/, BB_ENTRY );
      _entry_bb = _fake_entry_bb;
    } else {
      _entry_bb = Fake_entry_bb();
    }

    // connect all entrypoints and all not-reached blocks to the
    // fake entry block
    INT i;
    for ( i=0; i <= _entry_vec.Lastidx(); i++ )
      Connect_predsucc(_entry_bb, _entry_vec[i]);
    for ( i=0; i <= _notreach_vec.Lastidx(); i++ )
      Connect_predsucc(_entry_bb, _notreach_vec[i]);
  }
  // for exits, also does region exits
  Find_exit_blocks();

  // find blocks that do not reach an exit block
  Process_no_exit();

  Is_True( _exit_vec.Lastidx() >= 0,
    ("CFG::Process_multi_entryexit: no exit blocks") );

  // if only one exit BB that indeed exits, no fake BB needed
  if ( _exit_vec.Lastidx() == 0 && Fake_exit_bb() == NULL ) {
    _exit_bb = _exit_vec[0];

    if ( _exit_bb->Willexit() ) {
      return;
    }
  }

  // multiple exit BBs or blocks that do not reach an exit, need fake BB
  if (Fake_exit_bb() == NULL) {
    _fake_exit_bb = New_bb( FALSE/*!connect*/, BB_EXIT );
    _fake_exit_bb->Set_willexit();
    _exit_bb = _fake_exit_bb;
  } else {
    // if a fake exit BB already exists,  no need to create a new one.
    _exit_bb = Fake_exit_bb();
  }

  for (INT i=0; i <= _exit_vec.Lastidx(); i++) {
    Is_True( _exit_vec[i] != _exit_bb,
      ("CFG::Process_multi_entryexit: _exit_bb in _exit_vec") );
    Connect_predsucc(_exit_vec[i], _exit_bb);
  }
}

// ====================================================================
// Go through all BBs, and identify the bb is inside an MP region
// Only do this for MP regions. This routine will fail on a normal
// region because multiple exits don't fit a stack model.
// ====================================================================
void
CFG::Ident_mp_regions(void)
{
  CFG_ITER cfg_iter(this);
  BB_NODE *bb;
  BB_REGION *bb_region;

  Clear_mp_type();
  Clear_mp_rid();
  Clear_bb_region();

  // assuming all MP region has single entry and exit bb
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    if (bb->Kind() == BB_REGIONSTART) {
      bb_region = bb->Regioninfo();
      Is_True(bb_region != NULL, ("CFG::Ident_mp_regions, no regioninfo"));
      if (RID_TYPE_mp(bb_region->Rid())) {
	Push_mp_type(MP_REGION);
	Push_mp_rid(bb_region->Rid());
	Push_bb_region(bb_region);
      }
    }
      
    if (! NULL_mp_type()) { // stack not empty
      bb->Set_MP_region();
      bb->Set_rid_id(RID_id(Top_mp_rid()));
    }

    // an MP region has a BB_REGIONEXIT even though it has no OPC_REGION_EXIT
    if (bb->Kind() == BB_REGIONEXIT) {
      bb_region = bb->Regioninfo();
      Is_True(bb_region != NULL, ("CFG::Ident_mp_regions, no regioninfo"));
      if (RID_TYPE_mp(bb_region->Rid())) {
	Pop_mp_type();
	Pop_mp_rid();
	Pop_bb_region();
      }
    }
  }  
}

/* ============================================================================
*  This routine is to identify eh regions based on regionstart and 
*  regionexit bbs. It will mark all the bbs between regionstart
*  and regionexit to be eh_region_bb and set its rid_id to be the current 
*  eh_region rid. regionexit BB's rid_id is the parent eh_region's rid.
============================================================================ */
void
CFG::Ident_eh_regions(void)
{
  CFG_ITER cfg_iter(this); 
  BB_NODE *bb;
  BB_REGION *bb_region;
  INT32 eh_level = 0;

  Clear_eh_rid();
  
  // EH region can be single entry and multiple exits
  // In the mainopt, LOWER_REGION_EXITS is on, region_exits is changed to goto.
  // however, the goto exit has stmt, while the newly added region_exit is empty bb.
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
  
    if (bb->Kind() == BB_REGIONSTART) {
      bb_region = bb->Regioninfo();
      Is_True(bb_region != NULL, ("CFG::Ident_eh_regions, no regioninfo"));
      if (RID_TYPE_eh(bb_region->Rid())) {
        eh_level ++;
        Push_eh_rid(bb_region->Rid());
      }
    }
      
    if (eh_level > 0 ) {
      bb->Set_EH_region();
      bb->Set_rid(Top_eh_rid());
    }

    if (bb->Kind() == BB_REGIONEXIT ) {
      bb_region = bb->Regioninfo();
      Is_True(bb_region != NULL, ("CFG::Ident_eh_regions, no regioninfo"));
      // empty regionexit bb is the one that was added as the region end marker
      if (RID_TYPE_eh(bb_region->Rid()) && !bb->Firststmt()) {
        eh_level --;
        Is_True(eh_level >= 0 , ("CFG::Ident_eh_regions, not match regionstart and regionexit"));
        Pop_eh_rid();
        if (!Null_eh_rid())
            bb->Set_rid(Top_eh_rid());
        else
            bb->Set_rid(NULL);
      }
    }
  }

  Is_True(eh_level == 0 , ("CFG::Ident_eh_regions, not match regionstart and regionexit"));

}

#if defined(TARG_SL) //PARA_EXTENSION
// ====================================================================
// Go through all BBs, and identify the bb is inside an SL2 parallel region
// Only do this for SL2 parallel regions. This routine will fail on a normal
// region because multiple exits don't fit a stack model.
// ====================================================================
void
CFG::Ident_sl2_para_regions(void)
{
  CFG_ITER cfg_iter(this);
  BB_NODE *bb;
  BB_REGION *bb_region;

  Clear_sl2_para_type();
  Clear_sl2_para_rid();
  Clear_bb_region();

  // assuming all sl2 parallel region has single entry and exit bb
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    if (bb->Kind() == BB_REGIONSTART) {
      bb_region = bb->Regioninfo();
      Is_True(bb_region != NULL, ("CFG::Ident_sl2_parallel_regions, no regioninfo"));
      if (RID_TYPE_sl2_para(bb_region->Rid())) {
       Push_sl2_para_type(SL2_PARA_REGION);
	Push_sl2_para_rid(bb_region->Rid());
	Push_bb_region(bb_region);
      }
    }
      
    if (! NULL_sl2_para_type()) { // stack not empty
      bb->Set_SL2_para_region();
      bb->Set_rid_id(RID_id(Top_sl2_para_rid()));
    }

    // an SL2 parallel region has a BB_REGIONEXIT even though it has no OPC_REGION_EXIT
    if (bb->Kind() == BB_REGIONEXIT) {
      bb_region = bb->Regioninfo();
      Is_True(bb_region != NULL, ("CFG::Ident_sl2_parallel_regions, no regioninfo"));
      if (RID_TYPE_sl2_para(bb_region->Rid())) 
      {
	  Pop_sl2_para_type();
	  Pop_sl2_para_rid();
	  Pop_bb_region();
      }
    }
  }  
}

#endif

// ====================================================================
// Creates the CFG for a function node or other high-level node
// ====================================================================

void 
CFG::Create(WN *func_wn, BOOL lower_fully, BOOL calls_break, 
	    REGION_LEVEL rgn_level, OPT_STAB *opt_stab, BOOL do_tail, MEM_POOL * sc_pool )
{
  _opt_stab = opt_stab;
  _lower_fully = lower_fully;
  _calls_break = calls_break;
  _rgn_level   = rgn_level;	// context: preopt/mainopt/rvi
  _sc_pool = sc_pool;

  Set_cur_loop_depth( 0 );

  OPERATOR opr = WN_operator(func_wn);
  FmtAssert(opr == OPR_FUNC_ENTRY || opr == OPR_REGION, 
	    ("CFG::Create: root wn not FUNC_ENTRY or REGION"));

  SC_init();

  if (WOPT_Enable_Pro_Loop_Fusion_Trans || WOPT_Enable_Pro_Loop_Interchange_Trans) {
    if (opr == OPR_FUNC_ENTRY) {
      PU &pu = Pu_Table[ST_pu(WN_st(func_wn))];

      // Skip abnormal control flows in the first implementation.

      if (!PU_has_altentry(pu)
	  && !PU_has_exc_scopes(pu)
	  && !PU_has_unknown_control_flow(pu)
	  && !PU_has_nonlocal_goto_label(pu)
	  && !PU_has_goto_outer_block(pu)
	  && !PU_calls_setjmp(pu)
	  && !PU_calls_longjmp(pu)) {
	FmtAssert(_sc_pool, ("Null SC pool"));
	_sc_map = CXX_NEW(MAP(CFG_BB_TAB_SIZE, _sc_pool), _sc_pool);
	_sc_root = Create_sc(SC_FUNC);
	_sc_parent_stack = CXX_NEW(STACK<SC_NODE *>(_sc_pool), _sc_pool);
	_sc_parent_stack->Push(_sc_root);
      }
    }
  }

  // create the first entry block
  _last_bb = NULL;
  _first_bb = Create_bb();
  Append_bb( _first_bb );

  // assign the RID for this section of code, can be func_entry (RID tree)
  // or individual region with no subregions
  Is_True(REGION_consistency_check(func_wn),("CFG::Create"));
  _rid = REGION_get_rid(func_wn); 

  END_BLOCK endsbb;
  Add_one_stmt(func_wn, &endsbb);
  if (opr == OPR_REGION)
    Add_altentry(_first_bb); // make an entrypoint for the region

  // Contect all blocks containing AGOTO to all labels that could be targets
  Connect_agotos();

  // perform tail-recursion before dealing with multiple exits
  // because we may remove some exits, and we may create infinite
  // loops in the graph (think of a function that all it does is
  // call itself).
  if ( do_tail ) {
    OPT_TAIL opt_tail( this, opt_stab );
    opt_tail.Mutate();
  }

  if (OPT_Enable_EH_CFG_OPT)
    Ident_eh_regions();
 
  Process_multi_entryexit( TRUE/*is_whirl*/ );

  Ident_mp_regions();

#if defined(TARG_SL) //PARA_EXTENSION
  Ident_sl2_para_regions();
#endif

  if ( Get_Trace(TP_GLOBOPT, CFG_VERF_FLAG)) {
    fprintf(TFile, "%sDump after CFG construction %s%d\n%s", DBar,
	    (opr == OPR_REGION) ? "RGN " : "PU ", RID_id(_rid), DBar);
    RID_Tree_Print(TFile,Rid());
    Print(TFile);
    Validate(TFile);
  }
}

// ====================================================================
// Creates a new bb to join two BBs
// ====================================================================
static const BB_LOOP *
Common_loop(const BB_LOOP *a, const BB_LOOP *b)
{
  const BB_LOOP *p_a = a;
  const BB_LOOP *p_b = b;

  while (p_a && p_b && p_a != p_b) {
    if (p_a->Header()->Loopdepth() > p_b->Header()->Loopdepth()) {
      p_a = p_a->Parent();
    } else if (p_a->Header()->Loopdepth() < p_b->Header()->Loopdepth()) {
      p_b = p_b->Parent();
    } else {
      p_a = p_a->Parent();
      p_b = p_b->Parent();
    }
  }

  if (p_a == NULL || p_b == NULL) return NULL;

  return p_a;
}

BB_NODE *
CFG::Add_bb_to_edge( BB_NODE *v, BB_NODE *w)
{
  BB_NODE *new_bb;

  if (Trace()) 
    fprintf(TFile, "Add_bb_to_edge:  BB%d -> BB%d\n", v->Id(), w->Id());

  if (v->Next() == w) {
    new_bb = Create_and_allocate_bb(BB_GOTO);
    v->Insert_After(new_bb);

    // need to keep preds/succs in order, so replace directly
    v->Replace_succ( w, new_bb );
    new_bb->Append_pred( v, Mem_pool() );
    w->Replace_pred( v, new_bb );
    new_bb->Append_succ( w, Mem_pool() );
    // no label and goto are needed because it is a fall-through path.

    if ( Feedback() )
      Feedback()->Split_edge( v->Id(), new_bb->Id(), w->Id() );

  } else {

    new_bb = Create_and_allocate_bb(BB_GOTO);
    
    STMTREP *bb_branch = v->Branch_stmtrep();
    Is_True(bb_branch != NULL, ("BB has no branch statement."));

    OPERATOR opr = bb_branch->Opr();
    
    // assocate the new_bb with a label and update the TRUEBR label 
    Is_True(w->Labnam() != 0, ("BB has no label."));
    LABEL_IDX new_label = Alloc_label();

    Append_label_map(new_label, new_bb);

    STMTREP *new_labstmt = CXX_NEW (STMTREP(OPC_LABEL), Mem_pool());
    new_labstmt->Init_Label(NULL, new_label, 0);
    new_bb->Append_stmtrep( new_labstmt );

    if (opr == OPR_TRUEBR || opr == OPR_FALSEBR)
      bb_branch->Set_label_number( new_label );
    else {
      Is_True( opr == OPR_COMPGOTO,
	       ("don't know how to handle this branch kind.") );
      for (INT32 num_entry = 0; num_entry < v->Switchentries(); num_entry++) {
	if (v->Switchcase(num_entry) == w)
	  v->Set_switchcase(new_bb, num_entry);
      }
      if (v->Switchdefault() == w) {
        v->Set_switchdefault(new_bb);
      }
    }

    // need to keep preds/succs in order, so replace directly
    v->Replace_succ( w, new_bb );
    new_bb->Append_pred( v, Mem_pool() );
    w->Replace_pred( v, new_bb );
    new_bb->Append_succ( w, Mem_pool() );

    // new_bb is inserted before 'w'
    w->Insert_Before(new_bb);

    if ( Feedback() )
      Feedback()->Split_edge( v->Id(), new_bb->Id(), w->Id() );
   
    // 'u', the original prev of 'w', no longer has fall through
    BB_NODE *u = new_bb->Prev(); 
    if (w->Pred()->Contains(u)) {
      STMTREP *u_branch = u->Branch_stmtrep();
      if (u_branch == NULL && u->Kind() != BB_REGIONSTART &&
	  u->Kind() != BB_ENTRY) {

	// add a goto to 'w' at the end of 'u'
	STMTREP *new_goto = CXX_NEW( STMTREP(OPC_GOTO), Mem_pool() );
	new_goto->Init_Goto( NULL, w->Labnam(), 0 );
	u->Append_stmtrep( new_goto );
	if (w->Label_stmtrep() == NULL)
	  w->Add_label_stmtrep( Mem_pool() );

      } 
      else {

	if (u->Kind() == BB_REGIONSTART || u->Kind() == BB_ENTRY ||
	    OPCODE_operator(u_branch->Op()) == OPR_TRUEBR ||
	    OPCODE_operator(u_branch->Op()) == OPR_FALSEBR) {

	  // Insert a new bb, 'new_u' after 'u' as fall though and goto 'w'
	  BB_NODE *new_u = Create_and_allocate_bb(BB_GOTO);
	  u->Insert_After(new_u);

	  // need to keep preds/succs in order, so replace directly
	  u->Replace_succ( w, new_u );
	  new_u->Append_pred( u, Mem_pool() );
	  w->Replace_pred( u, new_u );
	  new_u->Append_succ( w, Mem_pool() );

	  if ( Feedback() )
	    Feedback()->Split_edge( u->Id(), new_u->Id(), w->Id() );  

	  STMTREP *new_goto = CXX_NEW( STMTREP(OPC_GOTO), Mem_pool() );
	  new_goto->Init_Goto( NULL, w->Labnam(), 0 );
	  new_u->Append_stmtrep( new_goto );

	  if (w->Label_stmtrep() == NULL)
	    w->Add_label_stmtrep( Mem_pool() );
	}

      }
    }
  }
  return new_bb;
}

// ====================================================================
// Find out those blocks not reached from an entrypoint to the region
// ====================================================================

void
CFG::Prop_entry(BB_NODE *bb) const
{
  bb->Set_flag(BB_REACHED|BB_DFORDER);
  Is_Trace(Trace(), (TFile,"%d, ",bb->Id()));

  BB_LIST_ITER bb_succ_iter;
  BB_NODE *succ;
  FOR_ALL_ELEM(succ, bb_succ_iter, Init(bb->Succ())) {
    if (!succ->Dforder())
      Prop_entry(succ);
  }
}

// public version that just sets Reached() flag, and does not fill
// in the notreached vector
void
CFG::Find_not_reached(void)
{
  CFG_ITER cfg_iter(this);
  BB_NODE *bb;

  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    bb->Reset_dforder();
    bb->Reset_reached();
  }

  // find those blocks that reach from an entry block
  Is_Trace(Trace(),(TFile,"CFG::Find_not_reached, prop_entry blocks:\n"));
  for (INT i=0; i <= _entry_vec.Lastidx(); i++) {
    Prop_entry(_entry_vec[i]);
  }
  Is_Trace(Trace(),(TFile,"\n"));
}

void
CFG::Process_not_reached( BOOL /* can_disconnect */ )
{
  CFG_ITER cfg_iter(this);
  BB_NODE *bb;

  // empty out the array of not-reached blocks
  _notreach_vec.Bzero_array();
  _notreach_vec.Resetidx();

  // if there's a fake entry block, get rid of all of its successors
  // and re-find them.  Some should have been removed
  if ( Fake_entry_bb() != NULL )
    Fake_entry_bb()->Set_succ( NULL );

  Find_not_reached();

  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    if ( ! bb->Reached() && Removable_bb(bb) )
    {
      // just disconnect this block from its preds and succs
      while ( bb->Pred() != NULL ) {
	BB_NODE *pred = bb->Pred()->Node();
	Remove_path( pred, bb );
	if ( Feedback() )
	  Feedback()->Delete_edge( pred->Id(), bb->Id() );
      }
      while ( bb->Succ() != NULL ) {
	BB_NODE *succ = bb->Succ()->Node();
	Remove_path( bb, succ );
	if ( Feedback() )
	  Feedback()->Delete_edge( bb->Id(), succ->Id() );
      }

      // go ahead and add it our not reached list, and it will be
      // dealt with in time.
      Is_Trace(Trace(),
	       (TFile,"CFG::Process_not_reached() 1, deleting BB%d %s\n",
		bb->Id(),bb->Kind_name()));
      Change_block_kind(bb, BB_GOTO);
      Add_notreach(bb);
    }
  }
}

// ====================================================================
// Handle any necessary paperwork to change the block's kind
// ====================================================================
void
CFG::Change_block_kind( BB_NODE *bb, BB_KIND newkind ) const
{
  switch ( bb->Kind() ) {

  case BB_DOSTART:	// init block
    // if we're changing this, the loop must no longer be a loop, so
    // change the kinds of the other blocks as well
    {
      BB_LOOP *loopinfo = bb->Loop();

      // set the new kind because we're going to recurse
      bb->Set_kind( newkind );

      if ( loopinfo->End() != NULL && 
	   loopinfo->End()->Kind() == BB_DOEND )
	Change_block_kind( loopinfo->End(), BB_LOGIF );
      if ( loopinfo->Step() != NULL && 
	   loopinfo->Step()->Kind() == BB_DOSTEP )
	Change_block_kind( loopinfo->Step(), BB_GOTO );

      bb->Set_loop( NULL );
    }
    break;

  case BB_DOEND:	// ending condition for do loop
    // if we're changing this, the loop must no longer be a loop, so
    // change the kinds of the other blocks as well
    {
      BB_LOOP *loopinfo = bb->Loop();
      if ( loopinfo->Start() != NULL &&
	   (loopinfo->Start()->Kind() == BB_DOSTART ||
	    loopinfo->Start()->Kind() == BB_DOHEAD) )
	Change_block_kind( loopinfo->Start(), BB_GOTO );
      if ( loopinfo->Step() != NULL && 
	   loopinfo->Step()->Kind() == BB_DOSTEP )
	Change_block_kind( loopinfo->Step(), BB_GOTO );

      // when we lowered fully, we have a tail block
      if ( loopinfo->Merge() != NULL &&
	   loopinfo->Merge()->Kind() == BB_DOTAIL )
	Change_block_kind( loopinfo->Merge(), BB_GOTO );

      bb->Set_loop( NULL );
    }
    break;

  case BB_DOHEAD:	// head block for lowered loops
    // if we're changing this, we must be getting rid of the loop,
    // but let doend case handle munging things.  This is almost
    // identical to a goto already because it just falls through.
    break;

  case BB_DOSTEP:	// increment
    {
      BB_LOOP *loopinfo = bb->Loop();

      // set the new kind because we're going to recurse
      bb->Set_kind( newkind );

      if ( loopinfo->End() != NULL && 
	   loopinfo->End()->Kind() == BB_DOEND )
	Change_block_kind( loopinfo->End(), BB_LOGIF );
      if ( loopinfo->Start() != NULL &&
	   loopinfo->Start()->Kind() == BB_DOSTART )
	Change_block_kind( loopinfo->Start(), BB_GOTO );

      bb->Set_loop( NULL );
    }
    break;

  case BB_DOTAIL:	// tail block for lowered loops
    break;

  case BB_ENTRY:	// the entry bb
    // it's OK, to *try* to change this block, but we don't really
    // do it.
    if ( bb == Fake_entry_bb() )
      return;
    FmtAssert( FALSE,
      ("CFG::Change_block_kind: trying to change kind of entry bb:%d",
       bb->Id()) );
    break;

  case BB_EXIT:		// the exit bb
    // It's OK, to *try* to change this block, but we don't really do it.
    // Leave exit_vec alone because we really didn't change block kind.
    return;
    // no break because of return

  case BB_REGIONEXIT:		// region exit bb
  { // Leave bb_region the same because we really didn't change the kind.
    BOOL everything_gone = Remove_region_exit(bb, TRUE);
    if (everything_gone)
      bb->Set_kind( newkind );
  }
    return;
    // no break because of return

  case BB_REGIONSTART:  // start of region
    // if changing this, we must no longer have a valid region
    Remove_region_entry(bb);
    // now set the kind for the BB at the bottom
    break;

  case BB_IO:		// IO statement
    // if changing this, we must no longer have valid IO info
    bb->Set_ioinfo( NULL );
    break;
    
  case BB_LOGIF:	// logical if
    // if changing this, we must no longer have valid if info
    if ( bb->Ifinfo() != NULL ) {
      BB_NODE *mergebb = bb->Ifinfo()->Merge();
      if ( mergebb != NULL )
	mergebb->Reset_ifmerge();
      bb->Set_ifinfo( NULL );
    }
    break;

  case BB_REPEATBODY:	// first BB in repeat body
    {
      BB_LOOP *loopinfo = bb->Loop();
      if ( loopinfo != NULL && loopinfo->End() != NULL &&
	   loopinfo->End()->Kind() == BB_REPEATEND )
      {
	// go ahead and set our kind
	bb->Set_kind( newkind );
	// and change the loop ending condition to just be a conditional
	// branch
	Change_block_kind( loopinfo->End(), BB_LOGIF );
      }
      bb->Set_loop( NULL );
    }
    // if changing this, we must no longer have valid loop info
    bb->Set_loop( NULL );
    break;

  case BB_REPEATEND:	// ending condition for repeat statement
    {
      BB_LOOP *loopinfo = bb->Loop();
      if ( loopinfo != NULL ) {
	if ( Lower_fully() && loopinfo->Start() != NULL &&
	     loopinfo->Start()->Kind() == BB_DOHEAD )
	{
	  // go ahead and set our kind
	  bb->Set_kind( newkind );
	  // and the first body becomes a simple fall-through block
	  Change_block_kind( loopinfo->Start(), BB_GOTO );
	}
	else if ( ! Lower_fully() && loopinfo->Body() != NULL &&
		  loopinfo->Body()->Kind() == BB_REPEATBODY )
	{
	  // go ahead and set our kind
	  bb->Set_kind( newkind );
	  // and the first body becomes a simple fall-through block
	  Change_block_kind( loopinfo->Body(), BB_GOTO );
	}
      }
      bb->Set_loop( NULL );
    }
    break;

  case BB_WHILEEND:  // ending condition for while statement
    {
      BB_LOOP *loopinfo = bb->Loop();
      if ( loopinfo != NULL ) {
	if ( Lower_fully() ) {
	  if ( loopinfo->Start() != NULL &&
	       loopinfo->Start()->Kind() == BB_DOHEAD )
	  {
	    // go ahead and set our kind
	    bb->Set_kind( newkind );
	    // and the first body becomes a simple fall-through block
	    Change_block_kind( loopinfo->Start(), BB_GOTO );
	  }
	  if ( loopinfo->Merge() != NULL &&
	       loopinfo->Merge()->Kind() == BB_DOTAIL )
	  {
	    // go ahead and set our kind
	    bb->Set_kind( newkind );
	    // and the merge block becomes a simple fall-through block
	    Change_block_kind( loopinfo->Merge(), BB_GOTO );
	  }
	}
	// NOTE that if we're not lowering fully (preopt), we don't
	// have other blocks we need to change.

	// if changing this, we must no longer have valid loop info
	bb->Set_loop( NULL );
      }
    }
    break;

  case BB_VARGOTO:   // variable goto
    // if changing this, we must no longer have valid switch info
    bb->Set_switchinfo( NULL );
    break;

  case BB_GOTO:      // single target BB
  case BB_SUMMARY:   // summary BB
    break;

  case BB_UNKNOWN:
  default:
    ErrMsg( EC_Unimplemented, "CFG::Change_block_kind: bad kind" );
    break;
  }

  bb->Set_kind( newkind );
}

// ====================================================================
// Recursive function to find blocks that do not exit, by searching
// down successor chains until one is found that neither exits and
// has no successors that aren't one of our predecessors.
// NOTE:  Need to reset Dforder for all blocks before calling this
// routine.
// ====================================================================
void 
CFG::Find_no_exit_blocks( BB_NODE *bb, BB_NODE_SET *instack )
{
  BB_NODE *succ;
  BB_LIST_ITER bb_succ_iter;

  // otherwise, we go down our successor chain until one is found that
  // has no successors, or has successors that we've already visited.
  // Those succs we mark as early exits.  This whole effort is to keep
  // as few blocks as possible as early exits
  if ( ! bb->Dforder() ) {
    INT succs_visited = 0;
    bb->Set_dforder();
    instack->Union1D( bb );
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
      if ( ! instack->MemberP(succ) ) {
	Find_no_exit_blocks( succ, instack );
	succs_visited++;
      }
    }
    instack->Difference1D( bb );

    // if we weren't able to visit any of our successors, (and we
    // earlier found we don't exit), then say we exit early
    if ( succs_visited == 0 && !bb->Willexit() &&
	 bb != Fake_exit_bb() ) 
    {
      Add_earlyexit(bb);
    }
  }
}

void
CFG::Bkwd_prop_exit(BB_NODE *bb) const
{
  bb->Set_flag(BB_WILLEXIT|BB_DFORDER);

  BB_LIST_ITER bb_pred_iter;
  BB_NODE *pred;

  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) 
    if (!pred->Dforder()) 
      Bkwd_prop_exit(pred);
}

void
CFG::Process_no_exit(void)
{
  CFG_ITER cfg_iter(this);
  BB_NODE *bb;

  // reset both reach and willexit flag
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    bb->Reset_visit();
    bb->Reset_willexit();
  }

  // find those blocks that reach an exit block
  INT i;
  for (i = 0; i <= _exit_vec.Lastidx(); ++i)
    Bkwd_prop_exit(_exit_vec[i]);

  // starting at entries, find blocks that don't exit
  FOR_ALL_NODE( bb, cfg_iter, Init() )
    bb->Reset_dforder();
  for (i=0; i <= _entry_vec.Lastidx(); i++) {
    BB_NODE_SET instack(Total_bb_count(), this, Loc_pool(), BBNS_EMPTY);
    Find_no_exit_blocks( _entry_vec[i], &instack );
  }

  // it is also possible there are blocks not reachable from an entry
  // which does not reach an exit.  Assume that we've already set up
  // a fake entry block if there are any such cases, and find blocks
  // from there.
  if ( Fake_entry_bb() != NULL ) {
    BB_NODE_SET instack(Total_bb_count(), this, Loc_pool(), BBNS_EMPTY);
    Find_no_exit_blocks( Fake_entry_bb(), &instack );
  }
}

// ====================================================================
// Find all of the blocks that exit from the region, and fill in the
// _exit_vec[] array.
// ====================================================================

void
CFG::Find_exit_blocks( void )
{
  CFG_ITER cfg_iter;
  BB_NODE *bb;

  // assume that the _exit_vec is large enough
  _exit_vec.Bzero_array();
  _exit_vec.Resetidx();

  // if there's a fake exit block, get rid of all of its predecessors
  // and re-find them.  Some will have been removed
  if ( Fake_exit_bb() != NULL )
    Fake_exit_bb()->Set_pred( NULL );

  // Search for exits to connect to fake_exit_bb.
  // This is tricky. Not all REGION_EXITs should be connected to the
  // fake exit. Some REGION_EXITs go from one region to another inside
  // the region being processed. These are not connected to the fake exit.
  FOR_ALL_ELEM ( bb, cfg_iter, Init(this) ) {

    if (bb == Fake_exit_bb() || bb == Fake_entry_bb())
      continue;

    if (bb->Kind() == BB_REGIONEXIT) {
      // Assume internal region exit --> no connect to fake exit by default
      Is_True(bb->Regioninfo() != NULL && bb->Regioninfo()->Rid() != NULL,
	      ("CFG::Find_exit_blocks, Region info missing from region exit"));
      // MP regions do not have exit blocks, they do not connect to fake exit
      if (!RID_TYPE_mp(bb->Regioninfo()->Rid())) {
	// first check if this region_exit exits the region we are
	// processing or is internal (between transparent regions)
	if (_entry_bb->Kind() == BB_REGIONSTART) {
	  Is_True(_entry_bb->Regioninfo(),
		  ("CFG::Find_exit_block, entry is not region start"));

	  // Find the label number for the REGION_EXIT.
	  // NOTE: since this code is called by CFG::Create() and
	  // CFG::Invalidate_and_update_aux_info(), we need to find
	  // either the WN *or* the STMTREP for the last statement.
	  INT32 label_number;
	  if (bb->Laststmt()) {
	    Is_True(WN_opcode(bb->Laststmt()) == OPC_REGION_EXIT,
		    ("CFG::Find_exit_block, region exit not found (1)"));
	    label_number = WN_label_number(bb->Laststmt());
	  } else if (bb->Last_stmtrep()) {
	    Is_True(bb->Last_stmtrep()->Op() == OPC_REGION_EXIT,
		    ("CFG::Find_exit_block, region exit not found (2)"));
	    label_number = bb->Last_stmtrep()->Label_number();
	  } else
	    Is_True(FALSE,
		    ("CFG::Find_exit_block, region exit not found (3)"));
	  
	  if (REGION_scan_exits(_entry_bb->Regioninfo()->Region_exit_list(),
				label_number)) {
	    Add_earlyexit( bb ); // it goes outside
	  }
	} else { // _entry->Kind() != BB_REGIONSTART
	  // empty, for _entry_bb->Kind() == BB_ENTRY all region exits
	  // are internal
	}
      } // if (!RID_TYPE_mp(bb->Regioninfo()->Rid()))
    } else if (bb->Kind() == BB_EXIT) {
      Add_earlyexit( bb );
    } else { // this is for all !BB_EXIT and !BB_REGIONEXIT blocks (inf loop)
      // save the last real block in source order to later consider it
      // as a block that leaves the region even if it has no return.
      if (bb->Succ() == NULL ||
	  (bb->Succ() != NULL && bb->Succ()->Len() == 0)) {
	Add_earlyexit( bb ); // don't reset the kind here, just connect up
      }
    }

  } // FOR_ALL_ELEM ( bb, cfg_iter, Init(this) )
}

// ====================================================================
// update pred/succ arcs of fake entry/exit blocks so they are not
// reachable from normal blocks - Basically, make the arcs one-way so
// fake blocks can reach succs/preds but their succs/preds cannot reach
// them.
// ====================================================================

void
CFG::Remove_fake_entryexit_arcs( void )
{
  BOOL fakeentry_to_fakeexit = false;
  
  if ( Fake_entry_bb() != NULL ) {
    BB_NODE     *succ;
    BB_LIST_ITER bb_succ_iter;
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(Fake_entry_bb()->Succ()) ) {
      if (succ == Fake_exit_bb()) fakeentry_to_fakeexit = true;
      succ->Remove_pred( Fake_entry_bb(), Mem_pool() );
    }
  }
  if ( Fake_exit_bb() != NULL ) {
    BB_NODE     *pred;
    BB_LIST_ITER bb_pred_iter;
    FOR_ALL_ELEM( pred, bb_pred_iter, Init(Fake_exit_bb()->Pred()) ) {
      pred->Remove_succ( Fake_exit_bb(), Mem_pool() );
    }
  }

  // If the fake exit is the fake entry's succ
  // then fake entry will be removed from fake exit's pred list
  // thus the fake exit could not be removed from the fake 
  // entry's succ list by the above remove_succ. We need to
  // do it here.
  if (fakeentry_to_fakeexit)
    Fake_entry_bb()->Remove_succ(Fake_exit_bb(), Mem_pool() );
    
}

// ====================================================================
// CFG::Func_entry_bb
//   The BB that contains the OPR_FUNC_ENTRY node.  Note: This
//   algorithm is based on the attribute of CFG_ITER, BBs are visited
//   in source order.
// ====================================================================
BB_NODE*
CFG::Func_entry_bb(void) const
{
  CFG_ITER cfg_iter( this );
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    if (bb->Kind() == BB_ENTRY && bb->Entrywn() &&
	WN_opcode(bb->Entrywn()) == OPC_FUNC_ENTRY )
      return bb;
    if (bb->Kind() == BB_REGIONSTART) {
      Is_True(bb->Regioninfo() != NULL,
	      ("CFG::Func_entry_bb, NULL regioninfo"));
      return bb;
    }
  }
  return NULL;
}

// ====================================================================
// Split one BB into two consecutive ones.  The "after_wn"
// becomes the last statement in the oldbb.
// The new block is the sole successor of the old block.  The old block
// becomes simply a goto block that falls through to the new block.
// This routine does not update the hascall flag or any other data
// structures, except for feedback info.
// ====================================================================

BB_NODE*
CFG::Split_bb_with_wns( BB_NODE *oldbb, WN *after_wn )
{
  BB_NODE *newbb = CXX_NEW(BB_NODE(*oldbb),Mem_pool());
  // newbb gets new id, and place in the array of blocks
  newbb->Set_id(Alloc_bb_id());
  _bb_vec[newbb->Id()] = newbb;
  newbb->Set_labnam(0);
  // UPDATE FREQUENCY -- OLD CODE: newbb->Set_freq(oldbb->Freq());

  // insert the block into the list appropriately
  oldbb->Insert_After(newbb);
  if ( oldbb == _last_bb )
    _last_bb = newbb;

  // right now newbb and oldbb share many pointers, so need to clear
  // some of them
  
  // the old block keeps the predecessor list, and the new one doesn't
  newbb->Set_pred(NULL);
  newbb->Set_succ(NULL);
  // the new block keeps the successor list, and the old one doesn't
  // so connect up the newbb with all of the successors
  BB_NODE     *succ;
  BB_LIST_ITER bb_iter;
  FOR_ALL_ELEM( succ, bb_iter, Init(oldbb->Succ())) {
    Connect_predsucc(newbb, succ);
  }
  // and then disconnect the oldbb from those same successors
  FOR_ALL_ELEM( succ, bb_iter, Init(newbb->Succ())) {
    DisConnect_predsucc(oldbb, succ);
  }

  // and the new block becomes successor of old block
  Connect_predsucc(oldbb, newbb);

  Is_True(oldbb->Kind() != BB_REGIONEXIT,
       ("CFG::Split_bb_with_wns, Splitting a REGION exit requires more code"));
  oldbb->Set_kind( BB_GOTO );

  // now fix up the list of statements
  newbb->Set_firststmt( WN_next(after_wn) );
  if ( WN_next(after_wn) )
    WN_prev(WN_next(after_wn)) = NULL;
  oldbb->Set_laststmt( after_wn );
  WN_next(after_wn) = NULL;

  return newbb;
}

// ====================================================================
// fill in provided array of BB_NODE *'s so they are ordered in
// pred-first order.  (i.e., all of a node's predecessors come before
// it in the array)
// Also return the number of entries in the array.
// get_pred_first_vec is private to CFG::Get_pred_first_vec
// ====================================================================

static void
get_pred_first_vec( BB_NODE *bb, BB_NODE *bbvec[], INT32 *numbbs )
{
  BB_NODE *pred;
  BB_LIST_ITER bb_pred_iter;

  bb->Set_dforder();

  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
    if ( ! pred->Dforder() ) {
      get_pred_first_vec( pred, bbvec, numbbs );
    }
  }

  bbvec[*numbbs] = bb;
  (*numbbs)++;
}

void
CFG::Get_pred_first_vec( BB_NODE *bbvec[], INT32 *numbbs )
{
  BB_NODE *bb;
  CFG_ITER cfg_iter(this);

  // starting at entries, find blocks that don't exit
  FOR_ALL_NODE( bb, cfg_iter, Init() )
    bb->Reset_dforder();

  *numbbs = 0;
  get_pred_first_vec( Exit_bb(), bbvec, numbbs );
}

// ====================================================================
// fill in _dfs_vec and po_vec of BB_NODE *'s so they are ordered in
// depth-first order and postorder respectively.
// Also return the number of entries in each of the arrays.
// ====================================================================

void
CFG::Fill_DFS_vec( BB_NODE *bb )
{
  bb->Set_dforder();

  if (bb != _fake_entry_bb && bb != _fake_exit_bb) {
    _dfs_vec[_dfs_vec_sz] = bb;
    (_dfs_vec_sz)++;
  }

  BB_NODE     *succ;
  BB_LIST_ITER bb_succ_iter;
  FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
    if ( ! succ->Dforder() ) {
      Fill_DFS_vec( succ );
    }
  }

  if (bb != _fake_entry_bb && bb != _fake_exit_bb) {
    _po_vec[_po_vec_sz] = bb;
    (_po_vec_sz)++;
  }
}

BB_NODE **
CFG::Dfs_vec(void)
{
  if (_dfs_vec == NULL) {

    if (_po_vec != NULL) {
      CXX_DELETE_ARRAY(_po_vec, Mem_pool());
      _po_vec = NULL;
    }

    _dfs_vec = (BB_NODE **)
       CXX_NEW_ARRAY(BB_NODE*,Total_bb_count(),Mem_pool());
    _po_vec = (BB_NODE **)
       CXX_NEW_ARRAY(BB_NODE*,Total_bb_count(),Mem_pool());
    
    BB_NODE *bb;
    CFG_ITER cfg_iter(this);
    FOR_ALL_NODE( bb, cfg_iter, Init() )
      bb->Reset_dforder();

    _dfs_vec_sz = 0;
    _po_vec_sz = 0;
    Fill_DFS_vec( Entry_bb() );

    RPOBB_ITER rpo_iter(this);
    INT32      rpo_id = 1;
    FOR_ALL_ELEM( bb, rpo_iter, Init() )
       bb->Set_rpo_id(rpo_id++);
  }
  return _dfs_vec;
}

BB_NODE **
CFG::Po_vec(void)
{
   if (_po_vec == NULL)
   {
      if (_dfs_vec != NULL)
      {
	 CXX_DELETE_ARRAY(_dfs_vec, Mem_pool());
	 _dfs_vec = NULL;
      }
      (void)Dfs_vec();
   }
   return _po_vec;
}

// ====================================================================
// Initialize _dpo_vec and _pdo_vec, the dominator and postdominator
// tree preorder vectors.  Skips fake entry and exit blocks.
// ====================================================================

void
CFG::Init_dpo_vec(BB_NODE *bb, INT *id)
{
  Is_True(bb->Dom_dfs_id() == *id,
	  ("CFG::Init_dpo_vec: bb->Dom_dfs_id() = %u, *id = %u",
	   bb->Dom_dfs_id(), *id));

  _dpo_vec[*id] = bb;
  ++(*id);

  BB_NODE     *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM ( dom_bb, dom_bb_iter, Init(bb->Dom_bbs()) )
    Init_dpo_vec( dom_bb, id );  /* child */
}

void
CFG::Init_pdo_vec(BB_NODE *bb, INT *id)
{
  Is_True(bb->Pdom_dfs_id() == *id,
	  ("CFG::Init_pdo_vec: bb->Pdom_dfs_id() = %u, *id = %u",
	   bb->Pdom_dfs_id(), *id));

  _pdo_vec[*id] = bb;
  ++(*id);

  BB_NODE     *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM ( dom_bb, dom_bb_iter, Init(bb->Pdom_bbs()) )
    Init_pdo_vec( dom_bb, id );  /* child */
}

// ====================================================================
// Retrieve, and initialize if necessary, the dominator and
// postdominator tree preorder vectors.
// ====================================================================

BB_NODE **
CFG::Dpo_vec(void)
{
  if (_dpo_vec == NULL) {
    _dpo_vec = (BB_NODE **) CXX_NEW_ARRAY(BB_NODE*, Total_bb_count(), Mem_pool());
    INT id = 0;
    Init_dpo_vec(Entry_bb(), &id);
    _dpo_vec_sz = id;
  }
  return _dpo_vec;
}

BB_NODE **
CFG::Pdo_vec(void)
{
  if (_pdo_vec == NULL) {
    _pdo_vec = (BB_NODE **) CXX_NEW_ARRAY(BB_NODE*, Total_bb_count(), Mem_pool());
    INT id = 0;
    Init_pdo_vec(Exit_bb(), &id);
    _pdo_vec_sz = id;
  }
  return _pdo_vec;
}

// ====================================================================
// Invalidate the entire loop structure.  Two parts, one recursive,
// and the other just is the CFG interface
// ====================================================================

static void
invalidate_loops( BB_LOOP *loop )
{
  BB_LOOP_CONTAINER loop_con(loop);
  BB_LOOP *head;
  while ( (head = loop_con.Remove_Headnode()) != NULL ) {
    // visit the child
    if ( head->Child() != NULL ) {
      invalidate_loops( head->Child() );
      head->Set_child(NULL);
    }
    head->Set_parent(NULL);
    if ( head->Body_set() != NULL ) 
      head->Body_set()->ClearD();
    if ( head->True_body_set() != NULL ) 
      head->True_body_set()->ClearD();
    head->Set_size_estimate(0);
  }
}

void
CFG::Invalidate_loops(void)
{
  if ( Loops_valid() && Loops() ) {
    invalidate_loops(Loops());
    _loops = NULL;
  }
  Set_loops_valid(FALSE);
}

// ====================================================================
// identify the loops in the range of blocks.  Create the sets of BBs
// in each block in the CFG's local pool.  Return the BB_LOOP that
// corresponds to the identified loop, or the first loop found in the
// blocks.
// ====================================================================

BB_LOOP *
CFG::Ident_loop( BB_NODE *first_bb, BB_NODE *last_bb,
		 INT32 loopnest,    BB_LOOP *cur_loop)
{
  // Identify the loop to work with
  BB_NODE *bb = first_bb;
  BB_NODE_SET *body_set = (cur_loop) ? cur_loop->Body_set() : NULL;
  BB_LOOP *sibling = NULL;
  BB_LOOP *nest_loop;

  while (bb != NULL) {

    bb->Set_loopdepth(loopnest);
    if (body_set) body_set->Union1D(bb);
    bb->Set_innermost(cur_loop);
    switch (bb->Kind()) {
    case BB_DOEND:
    case BB_WHILEEND:
    case BB_REPEATBODY:
      {
      bb->Loop()->Set_header(bb);
      BB_NODE_SET *new_body_set = 
         CXX_NEW(BB_NODE_SET(Total_bb_count(), this, Mem_pool(), BBNS_EMPTY),
		Mem_pool());
      // this doend block is considered to be one of the body bbs as
      // well
      new_body_set->Union1D(bb);
      bb->Set_loopbodyset(new_body_set);
      nest_loop = bb->Loop();
      nest_loop->Set_parent(cur_loop);

      if (sibling) sibling->Append(nest_loop);
      else {
	sibling = nest_loop;
	if (cur_loop)
          cur_loop->Set_child(sibling);
      }

      Is_Trace(Trace(),(TFile, "new loop formed at BB%d (inside loop at %d)\n",
		bb->Id(), cur_loop ? cur_loop->End()->Id() : 0));

      // first set all the information as if this doend block was in
      // the loop, which it is in a way.  If we don't do this, we don't
      // consider the end condition to be inside the loop like all
      // refs inside the body/increment.
      bb->Set_loopdepth(loopnest+1);
      bb->Set_innermost(nest_loop);
      
      if (bb->Kind() == BB_DOEND) {
        // do_end's next is the beginning of do_body
        Ident_loop(bb->Next(), bb->Loopstep(), loopnest+1, nest_loop);
        if (body_set) body_set->UnionD(new_body_set);
        bb = bb->Loopmerge();
      }
      else if (bb->Kind() == BB_WHILEEND) {
        Ident_loop(bb->Next(), bb->Loopmerge()->Prev(), loopnest+1, nest_loop);
        if (body_set) body_set->UnionD(new_body_set);
        if (bb->Loopmerge()->Prev() == last_bb) return sibling;
        bb = bb->Loopmerge();
      }
      else { // BB_REPEATBODY
        Ident_loop(bb->Next(), bb->Loopend(), loopnest+1, nest_loop);
        if (body_set) body_set->UnionD(new_body_set);
        if (bb->Loopend() == last_bb) return sibling;
        bb = bb->Loopmerge();
      }
      } // end scope
      break;
    case BB_REGIONEXIT:
    case BB_REGIONSTART:
      Is_True(bb->Regioninfo() != NULL,
	      ("CFG:Ident_mp_regions, NULL regioninfo"));
      // fall-thru
    case BB_GOTO:
    case BB_IO:
    case BB_LOGIF:
    case BB_VARGOTO:
    case BB_ENTRY:
    case BB_EXIT:
    case BB_DOSTART:
    case BB_DOSTEP:
    case BB_REPEATEND:
      if (bb == last_bb) return sibling;
      bb = bb->Next();
      break;
    default:
      FmtAssert(FALSE, ("CFG::Ident_loop: Illegal BB_KIND "));
      break;
    }
  }
  return sibling;
}

void
CFG::Set_loop_bb_set(BB_LOOP *loop)
{
  if (_bb_set == NULL)
     _bb_set = CXX_NEW(BB_NODE_SET(Total_bb_count(), this, Mem_pool(), BBNS_EMPTY),
                      Mem_pool());

  _bb_set->CopyD(loop->Body_set());

  BB_LOOP_CONST_ITER loop_iter(loop->Child());
  const BB_LOOP *nest_loop;
  FOR_ALL_NODE(nest_loop, loop_iter, Init()) {
    _bb_set->DifferenceD(nest_loop->Body_set());
  }
}

BOOL
CFG::Loop_itself_is_empty(BB_LOOP *loop)
{
  if (loop->Child() == NULL) return FALSE;

  if (loop->End()->Kind() == BB_LOGIF) return TRUE;
  Set_loop_bb_set(loop);
  return _bb_set->EmptyP();
}

BB_LOOP*
CFG::Get_last_loop(BB_LOOP *loop)
{
  BB_LOOP_ITER loop_iter(loop);
  BB_LOOP *nest_loop;
  BB_LOOP *last_loop = NULL;
  FOR_ALL_NODE(nest_loop, loop_iter, Init())
    last_loop = nest_loop;
  return last_loop;
}

// Remove given BB_LOOP from loop hierarchy. It is removed because
// the loop construct no longer exist (because, for instance, 
// fully-unrolling).
//
void
CFG::Remove_loop_construct (BB_LOOP* loop) {
        
  // step 1: Bypass this loop "vertically" through step 1.1 and 1.2.
  //
  //  step 1.1 Set all immediate nested loops' nesting loop to be this 
  //       <loop>'s immediate parent.
  //
  if (BB_LOOP* nested = (BB_LOOP*)loop->Child ()) {

    BB_LOOP_ITER iter (nested);
    BB_LOOP* nested_loop;
    BB_LOOP* last_nested_loop = NULL;

    FOR_ALL_NODE (nested_loop, iter, Init()) {
      nested_loop->Set_parent (loop->Parent ()); 
      last_nested_loop = nested_loop;
    }
  
    // step 1.2: "promote" immediate nested loops to be this <loop>'s siblling.
    //
    loop->Set_child (NULL);
    last_nested_loop->Set_Next (loop->Next ());
    loop->Set_Next (nested);
  }

  // step 2: Bypass this <loop> "horizontally" by hooking this <loop>'s previous
  //   sibling and next sibling.
  //
  BB_LOOP* first_sibling;
  if (loop->Parent ())
    first_sibling = loop->Parent ()->Child ();
  else
    first_sibling = Loops ();

  {
    BB_LOOP_ITER iter (first_sibling);
    BB_LOOP* sibling;
    FOR_ALL_NODE (sibling, iter, Init()) {
      if (sibling->Next () == loop) {
        sibling->Set_Next (loop->Next ());
        break;
      }
    }

    if (Loops () == loop) {
      // change loop hierarchy root
      //
      Set_loops (loop->Next ());
    }
  }

  // In case this <loop> is its parent's first kid, change parent's first kid
  // to be <loop>'s next sibling.
  //
  if (loop->Parent() && loop->Parent()->Child () == loop) {
    loop->Parent ()->Set_child (loop->Next ());  
  }

  CXX_DELETE (loop, Mem_pool ());
}

// bb is a member of loop->Body_set(), but has not yet been determined if it
// is member of loop->True_body_set() or member of _non_true_body_set.  It 
// assumes that loop->Body() (body first BB)  has already been made a member of
// loop->True_body_set().  By induction, if it can reach a BB that is member of
// loop->True_body_set(), then it can reach the loop's body first bb.  It also
// updates loop->True_body_set() and _non_true_body_set before returning
BOOL
CFG::Check_if_it_can_reach_body_first_bb(BB_NODE *bb, BB_LOOP *loop)
{
  if (bb->TLBS_processing())
    return FALSE;
  bb->Set_TLBS_processing();

  BB_LIST_ITER bb_succ_iter;
  BB_NODE *succ;
  BOOL can_reach_body_first_bb = FALSE;
  FOR_ALL_ELEM(succ, bb_succ_iter, Init(bb->Succ())) {
    if (loop->True_body_set()->MemberP(succ)) {
      can_reach_body_first_bb = TRUE;
      break;
    }
    if (! _non_true_body_set->MemberP(succ)) {
      if (Check_if_it_can_reach_body_first_bb(succ, loop)) {
        can_reach_body_first_bb = TRUE;
        break;
      }
    }
  }
  bb->Reset_TLBS_processing();
  if (can_reach_body_first_bb) {
    loop->True_body_set()->Union1D(bb);
    loop->Incr_size_estimate(bb->Code_size_est());
    if (Trace()) 
      fprintf(TFile, "adding bb%d\n", bb->Id());
    return TRUE;
  }
  else {
    _non_true_body_set->Union1D(bb);
    if (Trace()) 
      fprintf(TFile, "disqualifying bb%d\n", bb->Id());
    return FALSE;
  }
}

// for each loop in the PU, determine true_body_set, which must be a subset of
// body_set; the algorithm uses the fact that to be a true member of the loop
// body, it must be able to reach loop->Body() without passing through a
// BB that does not belong to true_body_set.  It processes the tree of nested
// loops bottom up, and speeds things up by initializing true_body_set by
// unioning the true_body_sets of the loops nested inside it.  Note that this
// will not correctly process the case where on the way out of the loop it
// passes another loop before it gets out of the static loop region.  But it
// has less processing (i.e. compile time) overhead this way; and even if it
// can determine that a loop in its scope is on the way out, it will not update
// the loop info data structure any way. 
void
CFG::Compute_true_loop_body_set(BB_LOOP *loops)
{
  if (loops == NULL) return;

  BB_NODE *bb;
  BB_NODE_SET_ITER bb_iter;
  BB_LOOP *loop;
  BB_LOOP_ITER loop_iter(loops);
  FOR_ALL_NODE(loop, loop_iter, Init()) {
    // recursive call
    Compute_true_loop_body_set(loop->Child());
    // allocate true_body_set
    if (loop->True_body_set() == NULL)
      loop->Set_true_body_set(
         CXX_NEW(BB_NODE_SET(Total_bb_count(), this, Mem_pool(), BBNS_EMPTY),
		    Mem_pool()));
    else loop->True_body_set()->ClearD();
    loop->True_body_set()->Union1D(loop->Body()); // body first BB is member
    loop->Set_size_estimate(loop->Body()->Code_size_est());
    { // union true_body_set's from its immediate children
      BB_LOOP *nested_loop;
      BB_LOOP_ITER nested_loop_iter(loop->Child());
      FOR_ALL_NODE(nested_loop, nested_loop_iter, Init()) {
        loop->True_body_set()->UnionD(nested_loop->True_body_set());
        loop->Incr_size_estimate(nested_loop->Size_estimate());
      }
    }
    // initialize non_true_body_set
    _non_true_body_set->UniverseD(Total_bb_count());
    _non_true_body_set->DifferenceD(loop->Body_set());
    if (Trace()) {
      fprintf(TFile, "Determining true loop body set from body set: ");
      loop->Body_set()->Print(TFile);
      fprintf(TFile, "\nInitial true loop body set: ");
      loop->True_body_set()->Print(TFile);
      fprintf(TFile, "\nInitial non-true loop body set: ");
      _non_true_body_set->Print(TFile);
      fprintf(TFile, "\n");
    }
    // determine if each member of body_set is member of true_body_set
    FOR_ALL_ELEM(bb, bb_iter, Init(loop->Body_set())) {
      if (loop->True_body_set()->MemberP(bb))
	continue;
      if (_non_true_body_set->MemberP(bb))
	continue;
      Check_if_it_can_reach_body_first_bb(bb, loop);
    }
  }
}


// Definitions:
//
//  Back-edge: an edge whose head dominates its tail
//     (it is more restricted than the other possible defintion:
//      a back-edge is one that goes from a node to one of its ancestors in
//      the dfs tree)
//     
//  The back-edge defines a loop with a single entry point.
//
//  Given a back edge m->n, the natural loop of m->n is the subgraph
//  consisting of the set of nodes containing n and all the nodes from which m
//  can be reached in the flowgraph without passing through n.
//
//  Node n is the loop header.
// 
//  Two natural loops can have the same header.  We treat such loops
//  as a single BB_LOOP.  i.e.,  a header uniquely identifies a BB_LOOP.
//
//  Well-formed loop: a BB_LOOP whose header has exactly two predecessors and
//  has one back edge.  Also, the preheader must not end with a branch
//  statement.
//
//  IVR can only optimize well formed loops.
//
//  TODO: ESTR should be aware of non-well-formed loops.
//  Loops with multiple entries are not considered
//  loop, and therefore ESTR can potentially introduce injury update
//  inside such loops to repair expressions outside of the loop.
//


//  Compute the preheader, loopback for each natural loop containing
//  one backedge.  A loop containing more than one preheader or loopback
//  is not "well-formed" and its preheader and loopback are set to NULL.
//  If the loop is well-formed, then we guess if the loop is tripcount-able,
//  the trip count exit is at the first block of the loop or the last block
//  of the loop.  Also compute the loop may exit early conservatively.
//
void Update_loops_for_mainopt(BB_LOOP *loop)
{
  //  loop->Header() must be defined

  loop->Set_loopback(NULL);
  loop->Set_preheader(NULL);
  loop->Set_tail(NULL);
  loop->Reset_well_formed();

  loop->Set_test_at_entry(false);
  loop->Set_test_at_exit(false);
  loop->Set_exit_early(true);

  BB_LIST_ITER bb_pred_iter;
  BB_NODE *pred;
  INT count = 0;
  FOR_ALL_ELEM( pred, bb_pred_iter, Init(loop->Header()->Pred()) ) {
    if (Is_backedge(pred, loop->Header())) {
      loop->Set_loopback(pred);
      loop->Set_loopback_pred_num(count);
    } else {
      loop->Set_preheader(pred);
      loop->Set_preheader_pred_num(count);
    }
    count++;
  }
  loop->Set_header_pred_count(count);
  if (count != 2 ||
      loop->Loopback() == NULL ||
      loop->Preheader() == NULL ||
      loop->Preheader()->Succ()->Len() != 1) {
    loop->Set_loopback(NULL);
    loop->Set_preheader(NULL);
    return;
  }

  // The loop  has single entry and single loop back edge.
  // It is said to be well-formed.
  //
  loop->Set_well_formed();

  if (loop->Loopback()->Succ()->Len() == 2) {
    loop->Set_test_at_entry(false);
    loop->Set_test_at_exit(true);
    BB_LIST_ITER bb_succ_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM(succ, bb_succ_iter, Init(loop->Loopback()->Succ())) {
      if (succ != loop->Header()) {
	loop->Set_tail(succ);
	break;
      }
    }
  } else if (loop->Header()->Succ()->Len() == 2) {
    loop->Set_test_at_entry(true);
    loop->Set_test_at_exit(false);
    BB_LIST_ITER bb_succ_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM(succ, bb_succ_iter, Init(loop->Header()->Succ())) {
      if (succ != loop->Header()) {
	loop->Set_tail(succ);
	break;
      }
    }
  }
  
  loop->Set_exit_early(loop->Tail() == NULL ||
		       loop->Tail()->Pred()->Len() != 1 || 
		       ! loop->Tail()->Postdominates_strictly(loop->Header()));
}

// Similar to Update_loops_for_preopt, except when the loops
// are not well-formed, just compute test_at_entry, test_at_exit,
// and exit_early based on SCF.
//
void Update_loops_for_preopt(BB_LOOP *loop)
{
  Update_loops_for_mainopt(loop);
  
  // Fix up the loop attributes if it wasn't a natural loop.
  //
  if (loop->Body() == loop->End()) {
    loop->Set_test_at_entry(false);
    loop->Set_test_at_exit(true);
  } else {
    loop->Set_test_at_entry(loop->Header() == loop->End());
    loop->Set_test_at_exit(loop->Header() != loop->End());
  }
  loop->Set_exit_early(loop->Dotail()->Pred()->Len() != 1 ||
		       ! loop->Dotail()->Postdominates_strictly(loop->Body()));
}

// A BB is a loop header block if it dominates the head of one of
// its incoming edge.
static bool
BB_is_header(BB_NODE *bb)
{
  BB_NODE *pred;
  BB_LIST_ITER bb_pred_iter;
  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
    if (Is_backedge(pred, bb)) {
      return true;
    }
  }
  return false;
}

// Determine loop body BBs.
//  -- the inner loop must be processed before the outer loop
//     is processed.  Loops should be processed according to
//     postorder dominator tree traversal.
//
static void
Collect_loop_body(BB_LOOP *loop, BB_NODE *bb)
{
  Is_True( loop->Header()->Dominates(bb),
	   ("Loop header %d does not dominate body BB%d.",
	    loop->Header()->Id(), bb->Id()) );

  Is_True(!loop->True_body_set()->MemberP(bb),
	  ("BB %d already visited.", bb->Id()));

  loop->True_body_set()->Union1D(bb);
  loop->Incr_size_estimate(bb->Code_size_est());

  // A loopback block can belong to two loops.
  if (bb->Innermost() == NULL)
    bb->Set_innermost(loop);

  if (bb == loop->Header()) return;

  BB_LIST_ITER bb_pred_iter;
  BB_NODE *pred;
  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
    if (loop->True_body_set()->MemberP(pred))
      continue;
    // not processed
    BB_LOOP *inner = pred->Innermost();
    Is_True(inner != loop, ("wrong pred->Innermost()."));
    if (inner) {
      Is_True(!inner->True_body_set()->EmptyP(), 
	      ("inner loop true body set not computed."));
      loop->True_body_set()->UnionD(inner->True_body_set());
      loop->Incr_size_estimate(inner->Size_estimate());
      if (inner->Well_formed()) {
	if (!loop->True_body_set()->MemberP(inner->Preheader()))
	  Collect_loop_body(loop, inner->Preheader());
      } else {
	BB_LIST_ITER bb_pred_iter;
	BB_NODE *pred;
	FOR_ALL_ELEM( pred, bb_pred_iter, Init(inner->Header()->Pred()) ) {
	  if ( ! Is_backedge(pred, inner->Header())
	       && ! loop->True_body_set()->MemberP(pred) )
	    Collect_loop_body(loop, pred);
	}
      }
    } else
      Collect_loop_body(loop, pred);
  }
}


static BB_LOOP *
Allocate_loop(BB_NODE *bb, BB_LOOP *parent, CFG *cfg)
{
  BB_LOOP *loop = bb->Loop();
  if (loop == NULL)
    loop = CXX_NEW( BB_LOOP(NULL, NULL, NULL, NULL, NULL, NULL),
		    cfg->Mem_pool() );
  loop->Set_true_body_set(CXX_NEW(BB_NODE_SET(cfg->Total_bb_count(), cfg,
					      cfg->Mem_pool(), BBNS_EMPTY),
				  cfg->Mem_pool()));
  loop->Set_body_set(NULL);  // body_set not supported in mainopt
  loop->Set_header(bb);
  loop->Set_child(NULL);
  loop->Set_parent(NULL);
  loop->Set_depth(0);
  loop->Set_max_depth(0);
  Update_loops_for_mainopt(loop);
  return loop;
}


// Traverse the dominator tree in pre and post order.
//   1. Allocate_loop in preorder
//   2. Collect_loop_body in postorder.
//
void Find_real_loops(BB_NODE *bb, BB_LOOP *parent, CFG *cfg)
{
  bb->Set_innermost(NULL);

  BB_LOOP *cur = parent;
  bool is_header = BB_is_header(bb);
  if (is_header) {
    cur = Allocate_loop(bb, parent, cfg);
    bb->Set_loop(cur);
  } else
    bb->Set_loop(NULL);

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  if (is_header) {
    FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
      Find_real_loops(dom_bb, cur, cfg);
    }
    if (cur->Well_formed())
      Collect_loop_body(cur, cur->Loopback());
    else {
      BB_LIST_ITER bb_pred_iter;
      BB_NODE *pred;
      FOR_ALL_ELEM( pred, bb_pred_iter, Init(cur->Header()->Pred()) ) {
	if ( Is_backedge(pred, cur->Header())
	     && ! cur->True_body_set()->MemberP(pred) )
	  Collect_loop_body(cur, pred);
      }
    }
  } else {
    FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
      Find_real_loops(dom_bb, parent, cfg);
    }
  }
}


static void
Fix_SCF_for_mainopt(CFG *cfg)
{
  // Part 1:  Fix bb->Loop() for SCF 
  //
  BB_NODE *bb;
  for (bb = cfg->First_bb(); bb != NULL; bb = bb->Next()) {
    BB_LOOP *loop = bb->Loop();
    if (loop != NULL && loop->Header() == bb) {
      if (loop->End() != NULL) {
	if(loop->Start())
	  loop->Start()->Set_loop(loop);
	else {
	  Is_True(loop->Is_flag_set(LOOP_PRE_WHILE),("wrong non bottom test loop lowering\n"));
	}
	loop->End()->Set_loop(loop);
	loop->Body()->Set_loop(loop);
	if (loop->Step())
	  loop->Step()->Set_loop(loop);
	// loop->Merge()->Loop() is invalid because
	// loop->Merge() can be the WHILE_END of the outer loop,
	// if the inner loop is a DO_WHILE loop.
	//   loop->Merge()->Set_loop(loop);
      } 
    }
  }
  // Part 2:  change SCF blocks of invalid loops into GOTO blocks
  //
  for (bb = cfg->First_bb(); bb != NULL; bb = bb->Next()) {
    BB_LOOP *loop = bb->Loop();
    if (loop == NULL || !loop->Well_formed()) {
      switch (bb->Kind()) {
      case BB_DOSTART:
      case BB_DOHEAD:
      case BB_DOSTEP:
      case BB_DOTAIL:
      case BB_REPEATBODY: 
	bb->Set_kind(BB_GOTO);
	break;
      case BB_DOEND:
      case BB_REPEATEND: 
      case BB_WHILEEND:
	bb->Set_kind(BB_LOGIF);
      }
    }
  }
}

static INT
Compute_loop_depth_rec(BB_LOOP *loop, INT depth)
{
  INT max_depth = depth;
  if (loop->Child()) {
    BB_LOOP_ITER loop_iter(loop->Child());
    BB_LOOP *child;
    FOR_ALL_NODE(child, loop_iter, Init()) {
      INT child_depth = Compute_loop_depth_rec(child, depth+1);
      max_depth = MAX(max_depth, child_depth);
    }
  }
  loop->Set_depth(depth);
  loop->Set_max_depth(max_depth);
  return max_depth;
}

static void
Compute_loop_depth(CFG *cfg)
{
  //  1. Build the parent/child loop relation
  //
  BB_NODE *bb;
  for (bb = cfg->First_bb(); bb != NULL; bb = bb->Next()) {
    BB_LOOP *loop = bb->Loop();
    if (loop != NULL && loop->Header() == bb) {
      BB_NODE *dom = bb->Idom();
      while (dom != NULL) {
	if (dom->Loop() &&
	    dom->Loop()->Header() == dom &&
	    dom->Loop()->True_body_set()->MemberP(bb))
	  break;  // found loop
	dom = dom->Idom();
      }
      BB_LOOP *parent = dom ? dom->Loop() : NULL;
      loop->Set_parent(parent);
      if (parent) {
	if (parent->Child()) 
	  parent->Child()->Append(loop);
	else 
	  parent->Set_child(loop);
      } else {
	if (cfg->Loops())
	  cfg->Loops()->Append(loop);
	else
	  cfg->Set_loops(loop);
      }
    } 
  }
  //  2. Compute loop depth
  //
  if (cfg->Loops()) {
    BB_LOOP_ITER loop_iter(cfg->Loops());
    BB_LOOP *child;
    FOR_ALL_NODE(child, loop_iter, Init()) {
      Compute_loop_depth_rec(child, 1);
    }
  }
  //  3.  Transfer loop depth from BB_LOOP to BB_NODE
  //
  for (bb = cfg->First_bb(); bb != NULL; bb = bb->Next()) {
    bb->Set_loopdepth( bb->Innermost() ? bb->Innermost()->Depth() : 0);
  }
}
  

#ifdef Is_True_On
static void
verify_loops(CFG *cfg)
{
  for (BB_NODE *bb = cfg->First_bb(); bb != NULL; bb = bb->Next()) {
    BB_LOOP *loop = bb->Loop();
    if (loop != NULL && loop->Header() == bb) {
      if (loop->End() == NULL) {
	Is_True(loop->Start() == NULL &&
		loop->End() == NULL &&
		loop->Body() == NULL &&
		loop->Step() == NULL &&
		loop->Merge() == NULL,
		("found inconsistent BB_LOOP."));
	Is_True(loop->Header() == NULL ||
		loop->Header()->Kind() == BB_GOTO ||
		loop->Header()->Kind() == BB_LOGIF ||
		loop->Header()->Kind() == BB_VARGOTO,
		("found inconsistent BB_LOOP -- header is not BB_GOTO"));
	Is_True(loop->Loopback() == NULL ||
		loop->Loopback()->Kind() == BB_GOTO ||
		loop->Loopback()->Kind() == BB_LOGIF ||
		loop->Loopback()->Kind() == BB_VARGOTO||
		loop->Loopback()->Kind() == BB_REGIONSTART ||
        loop->Loopback()->Kind() == BB_REGIONEXIT,
		("found inconsistent BB_LOOP -- loopback is not BB_GOTO"));
      } else {
	Is_True((loop->Start() == NULL || loop->Start()->Loop() == loop) &&
		loop->End()->Loop() == loop &&
		loop->Body()->Loop() == loop &&
		(loop->Step() == NULL || loop->Step()->Loop() == loop),
		// loop->Merge()->Loop() == loop,
		("found inconsistent BB_LOOP and CFG."));
	if (loop->Start() == NULL) {
          Is_True(loop->Is_flag_set(LOOP_PRE_WHILE),("wrong non bottom test loop lowering!\n"));
        }
      }
    }
    // verify innermost loop
    BB_NODE *dom = bb;
    while (dom != NULL) {
      if (dom->Loop() &&
	  dom->Loop()->Header() == dom &&
	  dom->Loop()->True_body_set()->MemberP(bb))
	break;  // found loop
      dom = dom->Idom();
    }
    Is_True(bb->Innermost() == (dom ? dom->Loop() : NULL),
	    ("BB%d innermost loop should be %d instead of %d\n",
	     bb->Id(), 
	     dom ? dom->Id(): 0,
	     bb->Innermost() ? bb->Innermost()->Header()->Id() : 0));
  }
}
#else
#define verify_loops(cfg)
#endif

static void
print_nested_loops_rec(const BB_LOOP *loop, FILE *fp)
{
  if (loop->Child()) {
    BB_LOOP_CONST_ITER loop_iter(loop->Child());
    const BB_LOOP *nest_loop;
    FOR_ALL_NODE(nest_loop, loop_iter, Init()) {
      nest_loop->Print(fp);
      print_nested_loops_rec(nest_loop, fp);
    }
  }
}

static void
print_nested_loops(CFG *cfg, FILE *fp)
{
  if (cfg->Loops()) {
    BB_LOOP_CONST_ITER loop_iter(cfg->Loops());
    const BB_LOOP *nest_loop;
    FOR_ALL_NODE(nest_loop, loop_iter, Init()) {
      nest_loop->Print(fp);
      print_nested_loops_rec(nest_loop, fp);
    }
  }
}

static void
print_bb_loopinfo(CFG *cfg, FILE *fp)
{
  for (BB_NODE *bb = cfg->First_bb(); bb != NULL; bb = bb->Next()) {
    BB_LOOP *loop = bb->Loop();
    BB_LOOP *inner = bb->Innermost();
    fprintf(fp, "BB%d depth=%d, loop=%d, innermost=%d\n",
	    bb->Id(),
	    bb->Loopdepth(),
	    loop ? 
	    (loop->Header() ? loop->Header()->Id() : loop->Dohead()->Id()) : 0,
	    inner ? inner->Header()->Id() : 0);
  }
}
  
// This function analyzes the loop structure for this comp_unit, and
// set the _loops field with the hierarchical loop structure.
BB_LOOP*
CFG::Analyze_loops(void)
{
  if (Loops_valid()) return Loops();
  Is_True(Loops() == NULL,
	  ("Loops are already updated."));

  if (Lower_fully()) {
    // MAINOPT
    Find_real_loops(Entry_bb(), NULL, this);
    Fix_SCF_for_mainopt(this);
    Compute_loop_depth(this);
    if (Trace() && Loops()) {
      print_nested_loops(this, TFile);
      print_bb_loopinfo(this, TFile);
    }

    verify_loops(this);
  } else {
    // PREOPT
    _loops = Ident_loop(First_bb(), Last_bb(), 0, NULL);

    if (_non_true_body_set == NULL ||
	(bs_PBPB(_non_true_body_set->Alloc_size() - sizeof(BS_WORD)) < 
	 Total_bb_count()))
      _non_true_body_set = CXX_NEW(BB_NODE_SET(Total_bb_count(), this, Mem_pool(),
					       BBNS_EMPTY), Mem_pool());

    Compute_true_loop_body_set(_loops);

    for (BB_NODE *bb = First_bb(); bb != NULL; bb = bb->Next()) {
      if (bb->Loop() != NULL && bb->Loop()->Header() == bb)
	Update_loops_for_preopt(bb->Loop());
    }
  }
  Set_loops_valid(TRUE);
  return Loops();
}

const BB_LOOP*
CFG::Find_innermost_loop_contains(BB_NODE *bb)
{
  Analyze_loops();
  return bb->Innermost();
}

// ====================================================================
// Find a region with the given pragma, that encloses the given BB
// Note that the region encloses "bb" and does not start with it
// ====================================================================

BB_NODE*
CFG::Find_enclosing_region_bb( BB_NODE *bb, WN_PRAGMA_ID region_pragma )
{
  for ( BB_NODE *dom = bb->Idom(); dom != NULL; dom = dom->Idom() ) {
    if ( dom->Kind() == BB_REGIONSTART ) {
      // first try whirl
      {
	WN     *wn;
	STMT_ITER stmt_iter;
	FOR_ALL_ELEM (wn, stmt_iter, Init(dom->Firststmt(), dom->Laststmt())) {
	  if ( WN_opcode(wn) == OPC_PRAGMA ) {
	    if ( (WN_PRAGMA_ID)WN_pragma(wn) == region_pragma ) {
	      // OK, we've found a matching region, but does it include
	      // the given block?
	      BB_REGION *dom_region = dom->Regioninfo();
	      for ( BB_NODE *region_bb = dom_region->Region_start();
		    region_bb != NULL;
		    region_bb = region_bb->Next() )
	      {
		if ( region_bb == bb ) {
		  return dom;
		}
		else if ( region_bb == dom_region->Region_end() ) {
		  // not in this region
		  break;
		}
	      }
	    }
	  }
	}
      } // end of whirl

      // second, try stmtrep
      {
	STMTREP     *stmt;
	STMTREP_ITER stmt_iter(dom->Stmtlist());
	FOR_ALL_NODE(stmt, stmt_iter, Init()) {
	  if ( stmt->Op() == OPC_PRAGMA ) {
	    WN *pragma_wn = stmt->Orig_wn();
	    if ( (WN_PRAGMA_ID)WN_pragma(pragma_wn) == region_pragma ) {
	      // OK, we've found a matching region, but does it include
	      // the given block?
	      BB_REGION *dom_region = dom->Regioninfo();
	      for ( BB_NODE *region_bb = dom_region->Region_start();
		    region_bb != NULL;
		    region_bb = region_bb->Next() )
	      {
		if ( region_bb == bb ) {
		  return dom;
		}
		else if ( region_bb == dom_region->Region_end() ) {
		  // not in this region
		  break;
		}
      }
	    }
	  }
	}
      } // end of stmtrep
    }
  }

  // didn't find it
  return NULL;
}

// ====================================================================
// Find a parallel region that dominates the given BB.
// Note that the region encloses "bb" and does not start with it
// ====================================================================
BB_NODE*
CFG::Find_enclosing_parallel_region_bb( BB_NODE *bb)
{
  for ( BB_NODE *dom = bb->Idom(); dom != NULL; dom = dom->Idom() ) {
    // bug 13579: Make sure the rid-type of the BB is MP. MP_region
    // flag only ensures that the BB is inside an MP region. For C++,
    // it could be an EH region. Here we are looking for the enclosing
    // MP parallel region.
    if ( dom->Kind() == BB_REGIONSTART && dom->MP_region() &&
	 RID_TYPE_mp(dom->Regioninfo()->Rid()) ) {
      // OK, we've found a matching region, but does it include
      // the given block?
      BB_REGION *dom_region = dom->Regioninfo();
      for ( BB_NODE *region_bb = dom_region->Region_start();
	    region_bb != NULL;
	    region_bb = region_bb->Next() )
      {
	if ( region_bb == bb ) {
	  return dom;
	}
	else if ( region_bb == dom_region->Region_end() ) {
	  // not in this region
	  break;
	}
      }
    }
  }
  // didn't find it
  return NULL;
}


// ====================================================================
// Determine if this loop is the outermost one in a parallel region
// (any sort of parallel loop or region for which we probably do not
//  wish to perform IVR on...)
// ====================================================================

BOOL
CFG::Is_outermost_loop_in_parallel_region( BB_LOOP *loop,
					   WN_PRAGMA_ID pragma_id )
{
  Analyze_loops();

  BB_NODE *par_region_bb = 
    Find_enclosing_region_bb( loop->End(), pragma_id );

  if ( par_region_bb != NULL ) {
    // is there any parent loop within the region that keeps us from
    // being the outermost?
    if ( loop->Parent() != NULL ) {
      BB_NODE *parent_loop_bb = loop->Parent()->End();

      if ( ! par_region_bb->Dominates_strictly( parent_loop_bb ) ) {
	// our parent is not dominated by the region, so we're the
	// outermost in this region
	return TRUE;
      }
    }
    else {
      // no parent, so we are the outermost
      return TRUE;
    }
  }

  return FALSE;
}


// Add a BB_NODE to each CFG edge from a bb with multiple successors to
// a bb with multiple predecessors.  Rebuild the data structure if
// rebuild_ds is TRUE.  do_df indicates whether to compute dom frontiers.

void    
CFG::Invalidate_and_update_aux_info(BOOL do_df)
{
  // handle blocks that don't exit
  Process_multi_entryexit( FALSE/*!whirl*/ );

  if (_dfs_vec != NULL) {
    // Force recomputation of dfs vector now that new BB's are in
    // place.
    CXX_DELETE_ARRAY(_dfs_vec, Mem_pool());
    _dfs_vec = NULL;
  }

  Compute_dom_tree(TRUE); // create dom tree
  Compute_dom_tree(FALSE);// create post-dom tree
  Remove_fake_entryexit_arcs();

  if (do_df) {
    Compute_dom_frontier(); // create dom frontier
    Compute_control_dependence(); // create reverse cfg dom
  }

  // fake blocks should not be considered as reached
  if ( Fake_entry_bb() != NULL )
    Fake_entry_bb()->Reset_reached();
  if ( Fake_exit_bb() != NULL )
    Fake_exit_bb()->Reset_reached();
    
  if (_po_vec != NULL) {
    CXX_DELETE_ARRAY(_po_vec, Mem_pool());
    _po_vec = NULL;
    _po_vec_sz = 0;
  }
  if (_dfs_vec != NULL) {
    CXX_DELETE_ARRAY(_dfs_vec, Mem_pool());
    _dfs_vec = NULL;
    _dfs_vec_sz = 0;
  }
  if (_dpo_vec != NULL) {
    CXX_DELETE_ARRAY(_dpo_vec, Mem_pool());
    _dpo_vec = NULL;
    _dpo_vec_sz = 0;
  }
  if (_pdo_vec != NULL) {
    CXX_DELETE_ARRAY(_pdo_vec, Mem_pool());
    _pdo_vec = NULL;
    _pdo_vec_sz = 0;
  }
}

INT
CFG::Remove_critical_edge()
{
  INT inserted = 0;
  OPT_POOL_Push( Loc_pool(), CFG_DUMP_FLAG );
  CFG_ITER cfg_iter(this);
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {

    if ( ! bb->Reached() )
      continue;

    // if there is a critical edge out of a black box region,
    // we can't fix it
    if (bb->Kind() == BB_GOTO &&
	((bb->Last_stmtrep() && bb->Last_stmtrep()->Op() == OPC_REGION) ||
	 (bb->Laststmt() && WN_opcode(bb->Laststmt()) == OPC_REGION)))
      continue;

    if ( (bb->Succ()->Len() > 1 && 
	  bb->Branch_stmtrep() != NULL &&
	  bb->Branch_stmtrep()->Opr() != OPR_AGOTO) ||
	 bb->Kind() == BB_REGIONSTART/*pv465582*/ ) {

      BB_LIST_ITER bb_succ_iter;
      BB_NODE *succ;
      FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
	if ( succ->Pred()->Len() > 1) {
	  if (!Is_backedge(bb, succ) ||
	      WOPT_Enable_Backedge_Placement) {
	    // found a critical edge
	    BB_NODE *new_bb = Add_bb_to_edge(bb, succ);
	    new_bb->Set_phi_list(CXX_NEW(PHI_LIST(new_bb), Mem_pool()));
	    inserted++;
	  }
	}
      }
    }
  }
  // Rebuild the data structures
  if (inserted > 0) {
    Invalidate_and_update_aux_info(TRUE);
    Invalidate_loops();
    Analyze_loops();
  }
  OPT_POOL_Pop( Loc_pool(), CFG_DUMP_FLAG );
  return inserted;
}

// find the BB that is the entry point to the PU/region we are processing
// used by the emitters
BB_NODE *
CFG::Find_entry_bb(void)
{
  BB_NODE *entry_bb = Entry_bb();
  if (entry_bb == Fake_entry_bb()) {
    // need to find the real entry
    BB_NODE *succ;
    BB_LIST_ITER bb_succ_iter;
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(entry_bb->Succ()) ) {
      if ( succ->Kind() == BB_ENTRY && succ->Entrywn() &&
#ifndef KEY
	  !WN_Label_Is_Handler_Begin(succ->Entrywn()) 
#else
	  WN_operator(succ->Entrywn()) != OPR_LABEL
#endif
	   ) {
        // look for the real entry, and not an alternate one
        if ( WN_opcode(succ->Entrywn()) == OPC_FUNC_ENTRY)
          entry_bb = succ;
          break;
      }
      if ( succ->Kind() == BB_REGIONSTART ) {
	Is_True(succ->Regioninfo() != NULL,
		("CFG::Find_entry_bb, NULL regioninfo"));
        entry_bb = succ;
        break;
      }
    }
    Is_True(entry_bb != Fake_entry_bb(),
	    ("CFG::Find_entry_bb, did not find entry_bb"));
  }
  return entry_bb;
}
      
// add entry to this container
void
MOD_PHI_BB_CONTAINER::Add_entry(BB_NODE  *bb,
                                PHI_LIST *old_lst,
                                PHI_LIST *new_lst,
                                MEM_POOL *pool)
{
  if (this == NULL) return;
  // first check if there is an entry in the list already
  MOD_PHI_BB     *tmp;
  MOD_PHI_BB_ITER mod_iter(this);
  FOR_ALL_NODE(tmp, mod_iter, Init()) {
    if (tmp->Bb() == bb) {
      // has to be a replacement
      Is_True (old_lst == tmp->New_lst(),
               ("MOD_PHI_BB_CONTAINER::Add_entry: Not replacement?"));
      if (tmp->Old_lst() != old_lst){
        PHI_NODE *phi;
        PHI_LIST_ITER phi_iter;
        FOR_ALL_ELEM ( phi, phi_iter, Init(old_lst) ){
          CXX_DELETE_ARRAY(phi->Vec(), pool);
          CXX_DELETE(phi, pool);
        }
        CXX_DELETE(old_lst, pool);
      }
      tmp->Set_new_lst(new_lst);
      return;
    }
  }
  // if it does not exist, create a new entry and insert to the list
  Is_True(tmp == NULL, ("Logic error"));
  tmp = CXX_NEW(MOD_PHI_BB(bb, old_lst, new_lst), _mem_pool);
  Append(tmp);
}


// Verify the input IR tree is a DAG
//
BOOL
CFG::Verify_tree(WN *wn)
{
  if (!WOPT_Enable_Verify)
    return TRUE;

  OPT_POOL_Push( Loc_pool(), CFG_DUMP_FLAG );
  WN_MAP wn_map = WN_MAP_Create( Loc_pool());
  
  BOOL result = Verify_wn(wn, NULL, wn_map);

  WN_MAP_Delete(wn_map);
  OPT_POOL_Pop( Loc_pool(), CFG_DUMP_FLAG);
  return result;
}

// Verify the wn node is not visited yet.
//
BOOL
CFG::Verify_wn(WN *wn, WN *parent, WN_MAP wn_map)
{
  if (!WOPT_Enable_Verify)
    return TRUE;

  INT32 i;
  WN *stmt;

  if (wn == NULL) return TRUE;

  WN *old_parent = (WN *) WN_MAP_Get(wn_map, wn);
  if (old_parent && old_parent != parent) {
    FmtAssert(FALSE, 
      ("WHIRL tree 0x%08x is pointed to from 0x%08x and 0x%08x\n",
	wn, parent, old_parent));
    return FALSE;
  }
  WN_MAP_Set(wn_map, wn, (void *)parent);
  
  if (WN_opcode(wn) == OPC_BLOCK) 
    for (stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
      if ( ! Verify_wn(stmt, wn, wn_map) )
	return FALSE;
    }
  else
    for (i = 0; i < WN_kid_count(wn); i++) {
      if ( ! Verify_wn(WN_kid(wn,i), wn, wn_map) )
	return FALSE;
    }

  return TRUE;
}

// Verify that the WN node in the CFG are not shared among BBs.
// TODO: Also verify that predecessor and successor sets of each BB have
// total frequency at least as great as the frequency of the BB.
//
BOOL
CFG::Verify_cfg(void)
{
  if (! WOPT_Enable_Verify)
    return TRUE;

  WN        *wn;
  BB_NODE   *bb;
  CFG_ITER   cfg_iter;
  STMT_ITER  stmt_iter;

  OPT_POOL_Push( &MEM_local_pool, CFG_DUMP_FLAG );
  WN_MAP wn_map = WN_MAP32_Create(&MEM_local_pool);
  
  FOR_ALL_ELEM (bb, cfg_iter, Init(this)) {
    FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
      IDTYPE id = WN_MAP32_Get(wn_map, wn);
      FmtAssert( id == 0,
		 ("Shared WN node at BB %d and BB %d\n", id, bb->Id()) );
      WN_MAP32_Set(wn_map, wn, bb->Id());
    }
  }

  WN_MAP_Delete(wn_map);
  OPT_POOL_Pop(&MEM_local_pool, CFG_DUMP_FLAG);
  return TRUE;
}

void 
CFG::Print(FILE *fp, BOOL rpo, IDTYPE bb_id)
{
  // Should we instead always print in source order (i.e. in order of 
  // basic block numbers)? 
  //
  if (!WOPT_Enable_Source_Order && rpo && Entry_bb() != NULL)
  {
     RPOBB_ITER rpo_iter(this);
     BB_NODE   *tmp;
     FOR_ALL_ELEM(tmp, rpo_iter, Init()) {
       // print if bb_id is not set or just print a particular BB
       if (bb_id == -1 || bb_id == tmp->Id())
	 tmp->Print(fp);
     }
  }
  else
  {
    CFG_ITER cfg_iter(this);
    BB_NODE *tmp;
    FOR_ALL_NODE(tmp, cfg_iter, Init()) {
      // print if bb_id is not set or just print a particular BB
      if (bb_id == -1 || bb_id == tmp->Id())
	tmp->Print(fp);
    }
  }
}

void
dump_cfg (CFG *cfg)
{
  cfg->Print(stdout);
}

void CFG::PrintLoopVis(BB_LOOP * loop, int& id)
{
  BB_NODE *tmp;
  BB_NODE_SET_ITER bb_iter;

  fprintf(stdout, "subgraph cluster%d {\n", id);
  FOR_ALL_ELEM(tmp, bb_iter, Init(loop->Body_set())) {
    fprintf(stdout, "BB%d;\n", tmp->Id());
  }

  BB_LOOP_ITER loop_iter(loop->Child());
  BB_LOOP *child;

  id = id + 1;

  FOR_ALL_NODE(child, loop_iter, Init()) {
    PrintLoopVis(child, id);
  }
  fprintf(stdout, "};\n");

}

void
CFG::PrintVis(BOOL draw_loops)
{
  CFG_ITER cfg_iter(this);
  BB_NODE *tmp;


  fprintf(stdout, "digraph Cfg {\n");

  if (draw_loops) {
    BB_LOOP_ITER loop_iter(Loops());
    BB_LOOP * loop;
    BB_LOOP * tmp_loop;

    int loop_num = 0;
    FOR_ALL_NODE(loop, loop_iter, Init()) {
      PrintLoopVis(loop, loop_num);
    }
  }

  FOR_ALL_NODE(tmp, cfg_iter, Init()) {
    tmp->PrintVis();
  }
  fprintf(stdout, "}\n");

}

void CFG::PrintCDVis(void)
{
  CFG_ITER cfg_iter(this);
  BB_NODE * bb;
  fprintf(stdout, "digraph CD { \n");
  FOR_ALL_NODE(bb, cfg_iter, Init()) {
    BB_NODE * dep_node;
    BB_NODE_SET_ITER rcfg_iter;
    fprintf(stdout, "BB%d;\n", bb->Id());
    FOR_ALL_ELEM(dep_node, rcfg_iter, Init(bb->Rcfg_dom_frontier())) {
      fprintf(stdout, "BB%d -> BB%d;\n", dep_node->Id(), bb->Id());
    }
  }
  fprintf(stdout, "}\n");
}

void
CFG::Validate(FILE *fp)
{
  CFG_ITER cfg_iter(this);
  BB_NODE *tmp;

  FOR_ALL_NODE(tmp, cfg_iter, Init())
    tmp->Validate(fp);

  // _first_bb_id is reserved for error condition
  for (IDTYPE id = _first_bb_id+1; id <= _last_bb_id; id++) {
    if (_bb_vec[id] == NULL)
      fprintf(fp, "BB_id:%d assigned but not used\n", id);
    else if (_bb_vec[id]->Id() != id)
      fprintf(fp, "_bb_vec[%d] points to BB%d", id, _bb_vec[id]->Id());
  }
}


// insert the new_temp into the ENTRY CHI LIST  
void Add_new_auxid_to_entry_chis(AUX_ID new_temp, CFG *cfg, CODEMAP *htable,
				 OPT_STAB *opt_stab)
{ 
  CODEREP *zcr = htable->Ssa()->Get_zero_version_CR(new_temp, opt_stab, 0);
  if (cfg->Fake_entry_bb() != NULL) {
    BB_NODE *bb; 
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (bb, bb_iter, Init(cfg->Fake_entry_bb()->Succ())) {
      if (bb->Kind() == BB_ENTRY) {
	STMTREP *entry_chi = bb->First_stmtrep();
	Is_True( OPCODE_operator(entry_chi->Op()) == OPR_OPT_CHI,
		 ("cannot find entry chi.") );
	CHI_LIST *chi_list = entry_chi->Chi_list();
	CHI_NODE *cnode = chi_list->New_chi_node(new_temp, htable->Mem_pool());
	cnode->Set_OPND(zcr);
	cnode->Set_RESULT(zcr);
	cnode->Set_live(TRUE);
      }
    }
  } else {
    BB_NODE *bb = cfg->Entry_bb();
    STMTREP *entry_chi = bb->First_stmtrep();
    Is_True( OPCODE_operator(entry_chi->Op()) == OPR_OPT_CHI,
	     ("cannot find entry chi.") );
    CHI_LIST *chi_list = entry_chi->Chi_list();
    CHI_NODE *cnode = chi_list->New_chi_node(new_temp, htable->Mem_pool());
    cnode->Set_OPND(zcr);
    cnode->Set_RESULT(zcr);
    cnode->Set_live(TRUE);
  }
}


BOOL
CFG::Fall_through(BB_NODE *bb1, BB_NODE *bb2)
{
  if (bb1->Next() != bb2) return FALSE;
  Is_True(bb2->Prev() == bb1, ("Invalid bb->Pred()"));
  STMTREP *bb_branch = bb1->Branch_stmtrep();
  if (bb_branch == NULL) return TRUE;
  if (OPC_GOTO != bb_branch->Op()) return FALSE;
  // bug 11304
  if (bb1->Succ() == NULL) return FALSE;

  if (bb1->Succ()->Node() == bb2) return TRUE;
  return FALSE;
}


static BOOL is_delete_bb_candidates(BB_NODE *bb)
{
  return (!bb->Regionend() &&
	  (bb->Kind() == BB_GOTO  ||
	   bb->Kind() == BB_LOGIF ||
	   bb->Kind() == BB_EXIT));
}

// Delete empty BB from CFG.
// WARNING::  do not call this function in PREOPT because
// it didn't maintain correct HL control structure.
void CFG::Delete_empty_BB()
{
  // Delete basic block that is empty or only contains
  // useless labels and branches

  BB_NODE *bb, *bb_next;
  for (bb = First_bb(); bb != NULL; bb = bb_next) {
    bb_next = bb->Next();
    if (bb_next != NULL && 
	is_delete_bb_candidates(bb) &&
	bb->Is_empty() &&
	Fall_through(bb, bb_next) &&
	bb_next->Labnam() != 0 &&
	bb->Pred()->Len() == 1 &&
	is_delete_bb_candidates(bb->Pred()->Node()) &&
	bb->Pred()->Node()->Branch_stmtrep() != NULL &&
	(bb->Pred()->Node()->Branch_stmtrep()->Op() == OPC_TRUEBR ||
	 bb->Pred()->Node()->Branch_stmtrep()->Op() == OPC_FALSEBR) &&
	bb->Pred()->Node()->Branch_stmtrep()->Label_number() == bb->Labnam()) {

      BB_NODE *pred = bb->Pred()->Node();
      if (Trace())
	fprintf(TFile, "CFG::Remove_empty_bb %d between %d and %d\n", bb->Id(),
		pred->Id(), bb_next->Id());
      pred->Remove_succ(bb, _mem_pool);
      bb->Remove_pred(pred, _mem_pool);
      bb->Remove_succ(bb_next, _mem_pool);
      bb_next->Remove_pred(bb, _mem_pool);
      if (bb_next->Labnam() == 0) 
	bb_next->Add_label(this);
      if (bb_next->Label_stmtrep() == NULL)
	bb_next->Add_label_stmtrep(_mem_pool);
      pred->Branch_stmtrep()->Set_label_number(bb_next->Labnam());
      pred->Append_succ(bb_next, _mem_pool);
      bb_next->Append_pred(pred, _mem_pool);

      BB_NODE *bb_prev = bb->Prev();
      bb->Set_next(NULL);
      bb->Set_prev(NULL);
      bb_prev->Set_next(bb_next);
      bb_next->Set_prev(bb_prev);
    }
  }

  // Delete GOTO into fall-through blocks
  // Delete useless labels

  for (bb = First_bb();
       bb != NULL ;
       bb = bb->Next()) {
    if (bb->Branch_stmtrep() != NULL &&
	bb->Branch_stmtrep()->Op() == OPC_GOTO && // because of OPC_IO ...
	bb->Succ()->Len() == 1 &&
	bb->Succ()->Node() == bb->Next()) {
      if (Trace())
	fprintf(TFile, "CFG::Remove branch stmtrep at BB %d\n", bb->Id());
      bb->Remove_stmtrep(bb->Branch_stmtrep());
    }

    if (bb->Label_stmtrep() != NULL &&
	! LABEL_addr_saved( bb->Labnam() ) &&
	bb->Pred()->Len() == 1 &&
	(bb->Pred()->Node()->Branch_stmtrep() == NULL ||
	 bb->Pred()->Node()->Branch_stmtrep()->Op() == OPC_GOTO ||
	 bb->Pred()->Node()->Branch_stmtrep()->Op() == OPC_TRUEBR ||
	 bb->Pred()->Node()->Branch_stmtrep()->Op() == OPC_FALSEBR) &&
	bb->Pred()->Node() == bb->Prev()) {
      if (Trace())
	fprintf(TFile, "CFG::Remove label stmtrep at BB %d\n", bb->Id());
      bb->Remove_label_stmtrep();
    }
  }
}

// Fix if info and loop info for the given sc.
void
CFG::Fix_info(SC_NODE * sc)
{
  SC_NODE * sc_tmp;
  BB_NODE * bb_tmp;
  BB_LIST_ITER bb_list_iter;
  SC_TYPE type = sc->Type();
  
  if (type == SC_IF) {
    BB_NODE * bb_head = sc->Head();
    BB_IFINFO * ifinfo = bb_head->Ifinfo();
    
    sc_tmp = sc->Find_kid_of_type(SC_THEN);
    ifinfo->Set_then(sc_tmp->First_bb());
    sc_tmp = sc->Find_kid_of_type(SC_ELSE);
    ifinfo->Set_else(sc_tmp->First_bb());
    sc_tmp = sc->Next_sibling();
    ifinfo->Set_merge(sc_tmp->First_bb());
  }    
  else if (type == SC_LOOP) {
    BB_LOOP * loop = sc->Loopinfo();
    sc_tmp = sc->Find_kid_of_type(SC_LP_BODY);
    if (sc_tmp)
    loop->Set_body(sc_tmp->First_bb());

    sc_tmp = sc->Find_kid_of_type(SC_LP_STEP);
    if (sc_tmp)
      loop->Set_step(sc_tmp->First_bb());

    sc_tmp = sc->Find_kid_of_type(SC_LP_START);
    if (sc_tmp)
      loop->Set_start(sc_tmp->First_bb());

    sc_tmp = sc->Find_kid_of_type(SC_LP_COND);
    if (sc_tmp)
      loop->Set_end(sc_tmp->First_bb());

    sc_tmp = sc->Next_sibling();
    if (sc_tmp)
      loop->Set_merge(sc_tmp->First_bb());
    else {
      sc_tmp = sc->Find_kid_of_type(SC_LP_COND);
      if (sc_tmp) {
	FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(sc_tmp->Last_bb()->Succ())) {
	  if (!sc->Contains(bb_tmp)) {
	    loop->Set_merge(bb_tmp);
	    break;
	  }
	}
      }
    }
  }
}

// Split given SC_BLOCK into multiple ones so that each of them only contains one BB_NODE.
SC_NODE *
CFG::Split(SC_NODE * sc)
{
  FmtAssert((sc->Type() == SC_BLOCK), ("Expect a SC_BLOCK"));
  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list->Len() > 1) {
    BB_NODE * bb = sc->Last_bb();
    bb_list = bb_list->Remove(bb, sc->Get_pool());
    sc->Set_bbs(bb_list);
    SC_NODE * sc_new = Create_sc(SC_BLOCK);
    sc_new->Append_bbs(bb);
    sc->Insert_after(sc_new);
  }

  return sc;
}

// Insert an empty SC_BLOCK after a SC_BLOCK.  Return the inserted SC_BLOCK.
SC_NODE *
CFG::Insert_block_after(SC_NODE * sc)
{
  SC_TYPE type = sc->Type();
  FmtAssert((type == SC_IF) || (type == SC_BLOCK), ("Expect a SC_BLOCK"));
  BB_NODE * bb_new = Create_and_allocate_bb(BB_GOTO);
  SC_NODE * sc_new = Create_sc(SC_BLOCK);
  sc_new->Append_bbs(bb_new);

  MEM_POOL * pool = Mem_pool();

  if (type == SC_BLOCK) {
    BB_NODE * bb_last = sc->Last_bb();
    BB_NODE * bb_next = bb_last->Succ()->Node();
    bb_last->Replace_succ(bb_next, bb_new);
    bb_next->Replace_pred(bb_last, bb_new);

    BB_LIST * bb_list = CXX_NEW(BB_LIST(bb_next), pool);
    bb_new->Set_succ(bb_list);
    bb_list = CXX_NEW(BB_LIST(bb_last), pool);
    bb_new->Set_pred(bb_list);

    BB_NODE * bb_tmp = bb_last->Next();
    bb_last->Set_next(bb_new);
    bb_new->Set_prev(bb_last);
    bb_new->Set_next(bb_tmp);
    bb_tmp->Set_prev(bb_new);

    if (Feedback()) {
      Feedback()->Add_node(bb_new->Id());
      Feedback()->Move_edge_dest(bb_last->Id(), bb_next->Id(), bb_new->Id());
      FB_FREQ freq = Feedback()->Get_edge_freq(bb_last->Id(), bb_new->Id());
      Feedback()->Add_edge(bb_new->Id(), bb_next->Id(), FB_EDGE_OUTGOING, freq);
    }
  }
  else if (type == SC_IF) {
    BB_NODE * bb_merge = sc->Merge();
    BB_NODE * bb_tmp;
    BB_LIST_ITER bb_list_iter;
    FB_FREQ freq = 0;

    if (Feedback())
      Feedback()->Add_node(bb_new->Id());

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_merge->Pred())) {
      bb_tmp->Replace_succ(bb_merge, bb_new);
      if (Feedback()) {
	Feedback()->Move_edge_dest(bb_tmp->Id(), bb_merge->Id(), bb_new->Id());
	freq += Feedback()->Get_edge_freq(bb_tmp->Id(), bb_new->Id());
      }

      if (bb_tmp->Is_branch_to(bb_merge)) {
	WN * branch_wn = bb_tmp->Branch_wn();
	Add_label_with_wn(bb_new);
	WN_label_number(branch_wn) = bb_new->Labnam();
      }
    }

    if (Feedback())
      Feedback()->Add_edge(bb_new->Id(), bb_merge->Id(), FB_EDGE_OUTGOING, freq);

    bb_new->Set_pred(bb_merge->Pred());
    bb_new->Append_succ(bb_merge, _mem_pool);
    bb_merge->Set_pred(NULL);
    bb_merge->Append_pred(bb_new, _mem_pool);
    
    bb_tmp = bb_merge->Prev();
    bb_new->Set_prev(bb_tmp);
    bb_tmp->Set_next(bb_new);
    bb_new->Set_next(bb_merge);
    bb_merge->Set_prev(bb_new);
  }

  sc->Insert_after(sc_new);
  Fix_info(sc);
  Fix_info(sc->Parent());
  Invalidate_and_update_aux_info(FALSE);
  Invalidate_loops();
  return sc_new;
}

// Insert an empty SC_BLOCK before 'sc'.  Return the inserted SC_BLOCK.
SC_NODE *
CFG::Insert_block_before(SC_NODE * sc)
{
  FmtAssert((sc->Type() == SC_IF) || (sc->Type() == SC_BLOCK)
	    || ((sc->Type() == SC_LOOP) && sc->Loopinfo()->Is_flag_set(LOOP_PRE_DO)),
	    ("Unexpected SC type"));
  SC_NODE  * sc_prev = sc->Prev_sibling();
  BB_NODE * bb_new = Create_and_allocate_bb(BB_GOTO);
  SC_NODE * sc_new = Create_sc(SC_BLOCK);
  sc_new->Append_bbs(bb_new);
  
  BB_NODE * bb_head = sc->First_bb();

  BB_NODE * bb_prev = bb_head->Prev();
  BB_LIST_ITER bb_list_iter;
  BB_NODE * bb_tmp;
  FB_FREQ freq = 0;

  if (Feedback()) 
    Feedback()->Add_node(bb_new->Id());
  
  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_head->Pred())) {
    if (bb_tmp->Is_branch_to(bb_head)) {
      WN * branch_wn = bb_tmp->Branch_wn();
      if (bb_new->Labnam() == 0)
	Add_label_with_wn(bb_new);
      WN_label_number(branch_wn) = bb_new->Labnam();
    }

    bb_tmp->Replace_succ(bb_head, bb_new);
    if (Feedback()) {
      Feedback()->Move_edge_dest(bb_tmp->Id(), bb_head->Id(), bb_new->Id());
      freq += Feedback()->Get_edge_freq(bb_tmp->Id(), bb_new->Id());
    }
  }
  
  if (Feedback())
    Feedback()->Add_edge(bb_new->Id(), bb_head->Id(), FB_EDGE_OUTGOING, freq);

  bb_new->Set_pred(bb_head->Pred());
  bb_new->Append_succ(bb_head, _mem_pool);
  bb_head->Set_pred(NULL);
  bb_head->Append_pred(bb_new, _mem_pool);
  
  bb_new->Set_next(bb_head);
  bb_head->Set_prev(bb_new);
  bb_new->Set_prev(bb_prev);
  bb_prev->Set_next(bb_new);

  sc->Insert_before(sc_new);
  if (sc_prev)
    Fix_info(sc_prev);
  Fix_info(sc->Parent());
  Invalidate_and_update_aux_info(FALSE);
  Invalidate_loops();
  return sc_new;
}

// Insert a SC_IF before 'sc_insert', 'bb_head' is the new SC_IF's head.
SC_NODE *
CFG::Insert_if_before(SC_NODE * sc_insert, BB_NODE * bb_head)
{
  SC_NODE * sc_prev = sc_insert->Prev_sibling();
  BB_NODE * bb_then = Create_and_allocate_bb(BB_GOTO);
  BB_NODE * bb_else = Create_and_allocate_bb(BB_GOTO);
  BB_NODE * bb_merge = sc_insert->First_bb();

  bb_head->Append_succ(bb_else, _mem_pool);
  bb_head->Append_succ(bb_then, _mem_pool);
  bb_then->Append_pred(bb_head, _mem_pool);
  bb_else->Append_pred(bb_head, _mem_pool);
  bb_then->Append_succ(bb_merge, _mem_pool);
  bb_else->Append_succ(bb_merge, _mem_pool);
  
  BB_NODE * bb_tmp;
  BB_LIST_ITER bb_list_iter;
  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_merge->Pred())) {
    bb_tmp->Replace_succ(bb_merge, bb_head);
    if (bb_tmp->Is_branch_to(bb_merge)) {
      WN * branch_wn = bb_tmp->Branch_wn();
      Add_label_with_wn(bb_head);
      WN_label_number(branch_wn) = bb_head->Labnam();
    }
    if (Feedback())
      Feedback()->Move_edge_dest(bb_tmp->Id(), bb_merge->Id(), bb_head->Id());
  }

  bb_head->Set_pred(bb_merge->Pred());
  bb_merge->Set_pred(NULL);
  bb_merge->Append_pred(bb_then, _mem_pool);
  bb_merge->Append_pred(bb_else, _mem_pool);
  
  bb_tmp = bb_merge->Prev();
  if (bb_tmp) {
    bb_tmp->Set_next(bb_head);
    bb_head->Set_prev(bb_tmp);
  }
  
  bb_head->Set_next(bb_then);
  bb_then->Set_prev(bb_head);
  bb_then->Set_next(bb_else);
  bb_else->Set_prev(bb_then);
  bb_else->Set_next(bb_merge);
  bb_merge->Set_prev(bb_else);

  Add_label_with_wn(bb_else);
  WN * wn_tmp = bb_head->Branch_wn();
  WN_label_number(wn_tmp) = bb_else->Labnam();

  SC_NODE * sc_if = Create_sc(SC_IF);
  SC_NODE * sc_then = Create_sc(SC_THEN);
  SC_NODE * sc_else = Create_sc(SC_ELSE);
  SC_NODE * sc_b1 = Create_sc(SC_BLOCK);
  SC_NODE * sc_b2 = Create_sc(SC_BLOCK);
  
  sc_if->Set_bb_rep(bb_head);
  sc_b1->Append_bbs(bb_then);
  sc_b2->Append_bbs(bb_else);
  
  BB_IFINFO * ifinfo = bb_head->Ifinfo();
  ifinfo->Set_cond(bb_head);
  ifinfo->Set_merge(bb_merge);
  ifinfo->Set_then(bb_then);
  ifinfo->Set_else(bb_else);

  sc_if->Append_kid(sc_then);
  sc_if->Append_kid(sc_else);
  sc_then->Set_parent(sc_if);
  sc_else->Set_parent(sc_if);
  sc_then->Append_kid(sc_b1);
  sc_else->Append_kid(sc_b2);
  sc_b1->Set_parent(sc_then);
  sc_b2->Set_parent(sc_else);

  sc_insert->Insert_before(sc_if);
  
  if (sc_prev) 
    Fix_info(sc_prev);

  Fix_info(sc_insert->Get_real_parent());
  Invalidate_and_update_aux_info(FALSE);
  Invalidate_loops();
  return sc_if;
}

// Obtained cloned equivalent of given bb
BB_NODE * 
CFG::Get_cloned_bb(BB_NODE * bb)
{
  FmtAssert(_clone_map, ("NULL clone map"));
  IDTYPE new_id = (IDTYPE) (unsigned long) (_clone_map->Get_val((POINTER) bb->Id()));
  if (new_id)
    return Get_bb(new_id);

  return NULL;
}

// Fix label number for WHIRL tree rooted at wn.  Must be invoked on a valid _clone_map.
void CFG::Fix_WN_label(WN * wn)
{
  OPCODE op;
  WN * kid;
  op = WN_opcode(wn);
  INT32 label = WN_label_number(wn);

  if (label) {
    BB_NODE * old_bb = Get_bb_from_label(label);
    if (old_bb) {
      BB_NODE * new_bb = Get_cloned_bb(old_bb);
      if (new_bb) 
	WN_label_number(wn) = new_bb->Labnam();
    }
  }

  if (op == OPC_BLOCK) {
    kid = WN_first(wn);
    while (kid) {
      Fix_WN_label(kid);
      kid = WN_next(kid);
    }
  }
  else {
    INT kidno;
    for (kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      kid = WN_kid(wn, kidno);
      if (kid)
	Fix_WN_label(kid);
    }
  }
}

// For every BB_NODE bb, Verify that:
// - If bb ends with a branch WN, then WN's label is the same as bb's non-fall-through successor.
// - If bb has a ifinfo, then ifinfo's Then() and Else() are bb's successor.

BOOL CFG::Verify_label()
{
  BOOL ret_val = TRUE;

  for (BB_NODE *bb = First_bb(); bb != NULL; bb = bb->Next()) {
    WN * branch_wn = bb->Branch_wn();
    BB_NODE * tmp;
    BB_LIST_ITER bb_list_iter;

    BB_IFINFO * ifinfo = NULL;

    if ((bb->Kind() == BB_VARGOTO)
	|| (bb->Kind() == BB_IO))
      continue;

    if (bb->Kind() == BB_LOGIF)
      ifinfo = bb->Ifinfo();

    if (!branch_wn && !ifinfo)
      continue;

    OPCODE opcode = WN_opcode(branch_wn);

    FOR_ALL_ELEM(tmp, bb_list_iter, Init(bb->Succ())) {
      if (branch_wn && (bb->Next() != tmp) && !OPCODE_is_call(opcode)) {
	if (WN_label_number(branch_wn) != tmp->Labnam()) {
	  ret_val = FALSE;
	  FmtAssert(FALSE, ("Verification error: branch label"));
	}
      }

      if (ifinfo) {
	if ((ifinfo->Then() != tmp) && (ifinfo->Else() != tmp)) {
	  ret_val = FALSE;
	  FmtAssert(FALSE, ("Verification error: ifinfo"));
	}
      }
    }
  }

  return ret_val;
}

// If the given bb has no label, add a new label and create a LABEL WN.
LABEL_IDX CFG::Add_label_with_wn(BB_NODE * bb)
{
  LABEL_IDX label = 0;

  if (bb->Labnam() == 0) {
    bb->Add_label(this);
    label = bb->Labnam();
    FmtAssert(label, ("NULL label"));
    WN * new_wn = WN_CreateLabel(0, label, 0, NULL);
    Prepend_wn_in(bb, new_wn);
  }
  return label;
}

void CFG::Clone_bb(IDTYPE src, IDTYPE dst, BOOL clone_wn)
{
  BB_NODE *srcbb = Get_bb(src);
  BB_NODE *destbb = Get_bb(dst);

  Is_True( srcbb->Clonable(TRUE, NULL, _allow_clone_calls),
	   ("CFG::Clone_bb:  BB%d is not clonable.", src) );
    
  destbb->Clear();
  destbb->Set_id(dst);
  destbb->Set_kind(srcbb->Kind());
  destbb->Set_labnam(0);
  destbb->Set_phi_list(CXX_NEW(PHI_LIST(destbb), Mem_pool()));
  destbb->Set_linenum(srcbb->Linenum());
  destbb->Set_rid_id(srcbb->Rid_id());
  destbb->Set_rid(srcbb->Rid());
  // UPDATE FREQUENCY -- OLD CODE: destbb->Set_freq(srcbb->Freq());

  // Fix zero version for phi.
  // After the basic block is cloned, the number of predecessor
  // can be different and the phi functions might be deleted,
  // e.g.,
  //       x0 = chi()   // define zero version
  //       x1 = phi(x0, ...) // x1 is a real version
  //          = x1      // real use
  // After the phi is deleted, the real use will be renamed 
  // zero version.
  //   -Raymond  7/30/98.

  PHI_NODE *phi; 
  PHI_LIST_ITER phi_iter;
  FOR_ALL_ELEM (phi, phi_iter, Init(srcbb->Phi_list())) {
    if (phi->Live()) {
      for ( INT pkid = 0; pkid < phi->Size(); pkid++ ) {
	if (phi->OPND(pkid)->Is_flag_set(CF_IS_ZERO_VERSION)) {
	  Htable()->Fix_zero_version(phi, pkid, true /*allow_real_def*/);
	}
	phi->OPND(pkid)->Reset_flag(CF_DONT_PROP);
      }
    }
  }

  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(srcbb->Stmtlist());
  STMTREP *stmt;       
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (stmt->Op() == OPC_LABEL) continue;
    // if (stmt->Op() == OPC_GOTO) continue;
    STMTREP *tmp = CXX_NEW(STMTREP, Htable()->Mem_pool());
    tmp->Clone(stmt, Htable(), Htable()->Mem_pool());
    destbb->Append_stmtrep(tmp);
  }

  // Clone WN statements.
  if (clone_wn) {
    STMT_ITER stmt_iter;
    WN * wn_iter;
    WN * prev_wn = NULL;

    FOR_ALL_ELEM (wn_iter, stmt_iter, Init(srcbb->Firststmt(), srcbb->Laststmt())) {
      WN * wn_new = WN_COPY_Tree_With_Map(wn_iter);
      Append_wn_in(destbb, wn_new);
    }
  }
}

// Clone all BB_NODEs that are linked by Next fields starting from bb_first and ending 
// in bb_last. Pred/Succ are set up for internal nodes.  bb_first and bb_last's cloned
// equivalents are returned via new_first and new_last.

void CFG::Clone_bbs
(
 BB_NODE * bb_first, 
 BB_NODE * bb_last,
 BB_NODE ** new_first,
 BB_NODE ** new_last,
 BOOL  clone_wn,
 float scale
)
{
  *new_first = NULL;
  *new_last = NULL;
  BB_NODE * bb_cur;
  MEM_POOL * pool = _loc_pool;
  if (_clone_map)
    CXX_DELETE(_clone_map, pool);

  _clone_map = CXX_NEW(MAP(CFG_BB_TAB_SIZE, pool), pool);

  for (bb_cur = bb_first; bb_cur; bb_cur = bb_cur->Next()) {
    BB_NODE * bb_new = Create_and_allocate_bb(bb_cur->Kind());
    IDTYPE src_id = bb_cur->Id();
    IDTYPE dst_id = bb_new->Id();

    Clone_bb(src_id, dst_id, clone_wn);

    if (bb_cur->Labnam()) {
      INT32 label = Alloc_label();
      Append_label_map(label, bb_new);
    }

    if (Feedback())
      Feedback()->Add_node(dst_id);

    if (bb_cur == bb_first)
      *new_first = bb_new;

    if (bb_cur == bb_last)
      *new_last = bb_new;
    
    _clone_map->Add_map((POINTER) src_id, (POINTER) dst_id);
    if (bb_cur == bb_last)
      break;
  }

  bb_cur = bb_first;
  BB_NODE * bb_prev = NULL;

  while (bb_cur) {
    BB_NODE * bb_new = Get_cloned_bb(bb_cur);
    FmtAssert(bb_new, ("Cloned BB_NODE not found"));
    BB_LIST_ITER bb_list_iter;
    BB_NODE * old_pred;

    // Update label
    STMT_ITER stmt_iter;
    WN * wn_iter;
    FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb_new->Firststmt(), bb_new->Laststmt())) {
      Fix_WN_label(wn_iter);
    }

    // Set up prev/next
    bb_new->Set_prev(bb_prev);
    if (bb_prev)
      bb_prev->Set_next(bb_new);

    if (bb_cur != bb_first) {
      // Connect pred/succ
      FOR_ALL_ELEM(old_pred, bb_list_iter, Init(bb_cur->Pred())) {
	BB_NODE * new_pred = Get_cloned_bb(old_pred);
	FmtAssert(new_pred, ("Cloned BB_NODE not found"));

	// Preserve the order of last(fall-through) successor.
	if (bb_cur != old_pred->Last_succ()) 
	  Prepend_predsucc(new_pred, bb_new);
	else
	  Connect_predsucc(new_pred, bb_new);	  
	
	if (Feedback()) {
	  Feedback()->Clone_edge(old_pred->Id(), bb_cur->Id(),
				 new_pred->Id(), bb_new->Id(),
				 scale);
	}
      }
    }
    
    // Clone ifinfo
    BB_IFINFO * ifinfo = bb_cur->Ifinfo();

    if (ifinfo && !bb_new->Ifinfo()) {
      BB_IFINFO * new_ifinfo = Clone_ifinfo(ifinfo);
      bb_new->Set_ifinfo(new_ifinfo);
    }

    // Clone loopinfo
    BB_LOOP * loopinfo = bb_cur->Loop();
    
    if (loopinfo && !bb_new->Loop()) {
      BB_LOOP * new_loopinfo = Clone_loop(loopinfo);
      bb_new->Set_loop(new_loopinfo);
    }

    if (bb_cur == bb_last)
      break;

    bb_prev = bb_new;
    bb_cur = bb_cur->Next();
  }
}

// Clone given BB_IFINFO. Must be invoked on a valid _clone_map.
BB_IFINFO *
CFG::Clone_ifinfo(BB_IFINFO * ifinfo)
{
  BB_NODE * old_cond = ifinfo->Cond();
  BB_NODE * old_then = ifinfo->Then();
  BB_NODE * old_else = ifinfo->Else();
  BB_NODE * old_merge = ifinfo->Merge();
  BB_NODE * new_cond = old_cond ? Get_cloned_bb(old_cond) : NULL;
  BB_NODE * new_then = old_then ? Get_cloned_bb(old_then) : NULL;
  BB_NODE * new_else = old_else ? Get_cloned_bb(old_else) : NULL;
  BB_NODE * new_merge = old_merge ? Get_cloned_bb(old_merge) : NULL;

  if (new_cond && new_cond->Ifinfo())
    return new_cond->Ifinfo();

  BB_IFINFO * new_ifinfo = CXX_NEW(BB_IFINFO(ifinfo->Thenloc(),
					     ifinfo->Elseloc(),
					     new_cond,
					     new_then,
					     new_else,
					     new_merge),
				   Mem_pool());

  return new_ifinfo;
}

// Clone given BB_LOOP. Must be invoked on a valid _clone_map.
BB_LOOP *
CFG::Clone_loop(BB_LOOP * bb_loop)
{
  WN * old_index = bb_loop->Index();
  BB_NODE * old_start = bb_loop->Start();
  BB_NODE * old_end = bb_loop->End();
  BB_NODE * old_body = bb_loop->Body();
  BB_NODE * old_step = bb_loop->Step();
  BB_NODE * old_merge = bb_loop->Merge();
  BB_NODE * old_preheader = bb_loop->Preheader ();
  BB_NODE * old_tail = bb_loop->Tail ();
  WN *      new_index = NULL;
  BB_NODE * new_start = old_start ? Get_cloned_bb(old_start) : NULL;
  BB_NODE * new_end = old_end ? Get_cloned_bb(old_end) : NULL;
  BB_NODE * new_body = old_body ? Get_cloned_bb(old_body) : NULL;
  BB_NODE * new_step = old_step ? Get_cloned_bb(old_step) : NULL;
  BB_NODE * new_merge = old_merge ? Get_cloned_bb(old_merge) : NULL;
  BB_NODE * new_preheader = old_preheader ? Get_cloned_bb(old_preheader) : NULL;
  BB_NODE * new_tail = old_tail ? Get_cloned_bb(old_tail) : NULL;

  if (new_start && new_start->Loop())
    return new_start->Loop();

  if (new_end && new_end->Loop())
    return new_end->Loop();

  if (new_body && new_body->Loop())
    return new_body->Loop();

  if (new_step && new_step->Loop())
    return new_step->Loop();

  if (new_merge && new_merge->Loop())
    return new_merge->Loop();

  if (old_index)
    new_index = WN_COPY_Tree_With_Map(old_index);

  BB_LOOP * new_loopinfo = CXX_NEW(BB_LOOP(new_index,
					   new_start,
					   new_end,
					   new_body,
					   new_step,
					   new_merge),
				   Mem_pool());

  new_loopinfo->Set_flag(bb_loop->Flags());

  if (bb_loop->Has_entry_guard())
    new_loopinfo->Set_has_entry_guard();

  if (bb_loop->Promoted_do())
    new_loopinfo->Set_promoted_do();

  // Nullify Orig_wn to avoid copying of stale feedback info at downstream phases.
  // Feedback info should have been updated when the loop is copied.
  new_loopinfo->Set_orig_wn(NULL);
  
  new_loopinfo->Set_preheader (new_preheader);

  return new_loopinfo;
}

// Clone given sc, BB_NODEs are cloned only at root level for SESEs.
// if 'sc' is a SC_IF at root level, we also clone its merge block and
// return cloned merge node via 'p_next'.
SC_NODE *
CFG::Clone_sc(SC_NODE * sc, BOOL is_root, float scale, SC_NODE ** p_merge)
{
  SC_NODE * sc_new = NULL;
  SC_TYPE type = sc->Type();

  if (is_root) {
    FmtAssert(sc->Is_sese(), ("Expect a single entry single exit node"));
    BB_NODE * bb_first = sc->First_bb();
    BB_NODE * bb_last;

    if (type == SC_IF) {
      SC_NODE * sc_next = sc->Next_sibling();
      FmtAssert(sc_next->Is_empty_block(), ("Expect an empty merge"));
      bb_last = sc->Merge();
    }
    else
      bb_last = sc->Last_bb();
    BB_NODE * bb_new_first = NULL;
    BB_NODE * bb_new_last = NULL;
    Clone_bbs(bb_first, bb_last, &bb_new_first, &bb_new_last, TRUE, scale);
  }
  
  sc_new = Create_sc(sc->Type());
  BB_NODE * bb = sc->Get_bb_rep();
  BB_NODE * bb_new;

  if (bb) {
    bb_new = Get_cloned_bb(bb);
    FmtAssert(bb_new, ("BB_NODE not cloned yet"));
    sc_new->Set_bb_rep(bb_new);
  }

  BB_LIST_ITER bb_list_iter(sc->Get_bbs());

  FOR_ALL_ELEM(bb, bb_list_iter, Init()) {
    bb_new = Get_cloned_bb(bb);
    FmtAssert(bb_new, ("BB_NODE not cloned yet"));
    sc_new->Append_bbs(bb_new);
  }

  SC_LIST_ITER sc_list_iter(sc->Kids());
  SC_NODE * tmp;
  SC_NODE * new_kid;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    new_kid = Clone_sc(tmp, FALSE, scale, NULL);
    sc_new->Append_kid(new_kid);
    new_kid->Set_parent(sc_new);
  }

  if (is_root
      && (type == SC_IF)) {
    SC_NODE * new_merge = Create_sc(SC_BLOCK);
    bb = sc->Merge();
    bb_new = Get_cloned_bb(bb);
    FmtAssert(bb_new, ("BB_NODE not cloned yet"));
    new_merge->Append_bbs(bb_new);
    FmtAssert(p_merge, ("Expect a return pointer."));
    *p_merge = new_merge;
  }

  return sc_new;
}

// Propagate edge frequency for all nodes in the given sc.
void 
CFG::Freq_propagate(SC_NODE * sc)
{
  BB_NODE * bb = sc->Get_bb_rep();

  if (bb != NULL)
    Feedback()->Freq_propagate(bb->Id());
  
  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      Feedback()->Freq_propagate(tmp->Id());
    }
  }

  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      Freq_propagate(tmp);
    }
  }
}

// Scale given BB_NODE's successor edges excluding those end at the given sc.
void
CFG::Freq_scale(BB_NODE * bb, SC_NODE * sc, float scale)
{
  BB_NODE * bb_tmp;
  BB_LIST_ITER bb_list_iter;
  FB_FREQ freq;
  IDTYPE edge;

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb->Succ())) {
    if ((sc != NULL) && sc->Contains(bb_tmp))
      continue;
    
    edge = Feedback()->Get_edge(bb->Id(), bb_tmp->Id());
    freq = Feedback()->Get_edge_freq(bb->Id(), bb_tmp->Id());
    Feedback()->Change_edge_freq(edge, freq * scale);
  }
}

// Scale successor edges of the nodes in the given sc excluding those
// end at loops.
void
CFG::Freq_scale(SC_NODE * sc, float scale)
{
  SC_TYPE sc_type = sc->Type();
  SC_NODE * sc_tmp = NULL;
    
  if (sc_type == SC_LP_COND) {
    sc_tmp = sc->Parent()->Find_kid_of_type(SC_LP_BODY);
    sc = sc->Find_kid_of_type(SC_BLOCK);
    if (!sc)
      return;
  }
  
  BB_NODE * bb = sc->Get_bb_rep();
  if (bb != NULL)
    Freq_scale(bb, sc_tmp, scale);

  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter;
    BB_NODE * bb_tmp;
    
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_list)) {
      Freq_scale(bb_tmp, sc_tmp, scale);
    }
  }
  
  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter;

    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc->Kids())) {
      sc_type = sc_tmp->Type();

      if ((sc_type != SC_LP_BODY) && (sc_type != SC_LP_STEP))
	Freq_scale(sc_tmp, scale);
    }
  }
}
  
