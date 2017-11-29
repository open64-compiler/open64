//-*-c++-*-

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_rviwn.cxx
// $Revision: 1.11 $
// $Date: 04/12/21 14:57:19-08:00 $
// $Author: bos@eng-25.internal.keyresearch.com $
// $Source: /home/bos/bk/kpro64-pending/be/opt/SCCS/s.opt_rviwn.cxx $
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
// Dealing with WN for Register-Variable Identification.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_rviwn_CXX	"opt_rviwn.cxx"
static char *rcs_id = 	opt_rviwn_CXX"$Revision: 1.11 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "stab.h"
#include "wn.h"
#include "wn_util.h"
#include "config.h"
#include "cxx_memory.h"

#include "w2op.h"
#include "data_layout.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_base.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_alias_mgr.h"
#include "opt_wn.h"
#include "opt_rvilr.h"
#include "opt_rvitab.h"
#include "opt_rviwn.h"
#include "opt_rvi.h"
#include "opt_util.h"
#include "bb_node_set.h"
#include "idx_32_set.h"
#include "opt_cvtl_rule.h"


// ====================================================================
// Create a new LDID of this annotation
// ====================================================================

WN *
RVI_ANN::New_ldid( ALIAS_MANAGER *alias_mgr ) const
{
  MTYPE mtype = TY_mtype(ST_type(Preg_st()));
  OPCODE load_opc = OPCODE_make_op( OPR_LDID, mtype, mtype );
  WN *newldid = WN_CreateLdid( load_opc, Preg(), Preg_st(),
			       MTYPE_To_TY(mtype) );
  alias_mgr->Gen_alias_id(newldid, NULL);
  return newldid;
}

// ====================================================================
// Print an annotation
// ====================================================================

void
RVI_ANN::Print( FILE *fp ) const
{
  fprintf( fp, "ST:%s Preg:%d Bitpos:%d\n",
	   ST_name(Preg_st()), Preg(), Bitpos() );
}

// ====================================================================
// Find a bitpos in the annotation list
// ====================================================================

RVI_ANN *
RVI_ANN_LIST::Find( const IDX_32 bitpos )
{
  RVI_ANN_ITER ann_iter;
  RVI_ANN *ann;
  FOR_ALL_NODE( ann, ann_iter, Init(this) ) {
    if ( bitpos == ann->Bitpos() ) {
      return ann;
    }
  }

  return NULL;
}

// ====================================================================
// Print an annotation list
// ====================================================================

void
RVI_ANN_LIST::Print( FILE *fp )
{
  RVI_ANN_ITER ann_iter;
  RVI_ANN *ann;
  FOR_ALL_NODE( ann, ann_iter, Init(this) ) {
    ann->Print( fp );
  }
}

// ====================================================================
// Insert the WN at the top (at_top=TRUE) or bottom of the block
// ====================================================================

void
RVI::Insert_statement( BB_NODE *bb, WN *wn, RVI_INSERT insert ) const
{
  if ( insert == RVI_INS_TOP ) {
    // assign line-number
    if ( bb->Firststmt() != NULL ) {
      WN_Set_Linenum( wn, WN_Get_Linenum(bb->Firststmt()) );
    }

    bb->Prepend_wn_after_labels( wn );
  }
  else if ( insert == RVI_INS_BEFORE_IREF ) {
    // assign line-number
    Is_True ( bb->Loc_mu_wn() != NULL,
      ("RVI::Insert_statement: ins before null iref") );

    WN_Set_Linenum( wn, WN_Get_Linenum(bb->Loc_mu_wn()) );
    bb->Insert_wn_before( wn, bb->Loc_mu_wn() );
  }
  else {
    WN *last_stmt;

    if ( insert == RVI_INS_PRECALL ) {
      last_stmt = bb->Hascall() ? bb->Loc_mu_wn() : bb->Laststmt();
    }
    else if ( insert == RVI_INS_POSTCHI ) {
      last_stmt = bb->Loc_mu_wn();
      if ( last_stmt == NULL )
	last_stmt = bb->Laststmt();
    }
    else {
      Is_True( FALSE,
	("RVI::Insert_statement: bad insert value") );
    }

    if ( last_stmt != NULL ) {
      // assign line-number
      WN_Set_Linenum( wn, WN_Get_Linenum(last_stmt) );

      // do we have to insert before a branch?
      if ( bb->Hascall() ) {
	if ( insert == RVI_INS_PRECALL ) {
	  bb->Insert_wn_before( wn, last_stmt );
	}
	else {
	  bb->Insert_wn_after( wn, last_stmt );
	}
      }
      else if ( OPCODE_is_endsbb(WN_opcode(last_stmt)) ) {
	// have to always insert code before other branches, even
	// when adding to bottom of block
	bb->Insert_wn_before( wn, last_stmt );
      }
      else {
	// if no call or branch, just append to the end
	bb->Insert_wn_after( wn, last_stmt );
      }
    }
    else {
      // only statement in block
      bb->Insert_wn_before( wn, NULL );
    }
  }

  if ( Tracing() ) {
    const char *msg;
    switch ( insert ) {
      case RVI_INS_TOP:         msg = "at top"; break;
      case RVI_INS_BEFORE_IREF: msg = "before iref"; break;
      case RVI_INS_PRECALL:     msg = "before call"; break;
      case RVI_INS_POSTCHI:     msg = "after chi"; break;
    }

    fprintf( TFile, "RVI::Insert_statement: added %s of BB:%d\n",
	     msg, bb->Id() );
    fdump_tree( TFile, wn );
  }
}

// ====================================================================
// Load the value into the preg
// ====================================================================

WN *
RVI_NODE::Create_load( INT32 preg, ALIAS_MANAGER *alias_mgr ) const
{
  WN *newload = NULL;
  if (Loadwn() != NULL) 
    newload = WN_COPY_Tree(Loadwn());
  else {
    OPCODE newldid_opc = OPCODE_make_op(OPR_LDID, 
					Mtype(), 
					WN_desc(Storewn()));
    newload = WN_CreateLdid( newldid_opc,
			     WN_store_offset(Storewn()),
			     WN_st(Storewn()),
			     WN_ty(Storewn()));
  }
  if (OPCODE_is_load(WN_opcode(newload))) {
    if (Loadwn() != NULL) {
      alias_mgr->Dup_tree_alias_id( Loadwn(), newload );
      WN_dup_dep_vertex(Loadwn(), newload);
    } else
      Copy_alias_info(alias_mgr, Storewn(), newload);
    alias_mgr->Set_homing_load( newload, TRUE );
  }
  MTYPE mtype = Mtype();
  OPCODE newstore_opc = OPCODE_make_op(OPR_STID,MTYPE_V,mtype);
  ST *preg_st = MTYPE_To_PREG( mtype );
  WN *pregstore = WN_CreateStid( newstore_opc,
				preg, preg_st, 
				MTYPE_To_TY(mtype),
				newload );
  alias_mgr->Gen_alias_id(pregstore, NULL);
  return pregstore;
}

// ====================================================================
// Store the preg into the variable
// ====================================================================

WN *
RVI_NODE::Create_store( INT32 preg, ALIAS_MANAGER *alias_mgr ) const
{
  MTYPE mtype = Mtype();
  OPCODE load_opc = OPCODE_make_op( OPR_LDID, mtype, mtype );
  ST *preg_st = MTYPE_To_PREG( mtype );
  WN *pregload = WN_CreateLdid( load_opc, preg, preg_st,
				MTYPE_To_TY(mtype) );
  alias_mgr->Gen_alias_id(pregload, NULL);

  OPCODE newstore_opc = WN_opcode(Storewn());
  WN *varstore = WN_CreateStid( newstore_opc,
				WN_store_offset(Storewn()), 
				WN_st(Storewn()), 
				WN_ty(Storewn()),
				pregload );
  Copy_alias_info(alias_mgr, Storewn(), varstore);
  WN_dup_dep_vertex(Storewn(), varstore);
  alias_mgr->Set_homing_store( varstore, TRUE );

  return varstore;
}

// ====================================================================
// Insert loads at appropriate places
// ====================================================================

void
RVI::Insert_load( RVI_LRBB *lrbb, WN *loadwn, RVI_LR *live_range ) const
{
  // Do we want to insert this load at the top of this block, or in
  // the predecessors that are not in the live-range?
  if ( lrbb->Need_load_here() ) {
    if ( ! Redundant_load_top()->MemberP(lrbb->Bb()) ) {
      Insert_statement( lrbb->Bb(), loadwn, RVI_INS_TOP );
      Redundant_load_top()->Union1D(lrbb->Bb());
    }
  }
  else if ( lrbb->Need_load_pred() || lrbb->Need_load_chi() ) {
    // #368284: Check to see if any predecessor is an ENTRY.  If so, then we 
    // can't insert the load in the pred.  Instead, insert it at the top of
    // this BB.  This is a very minor performance lose.
    BOOL pred_is_entry = FALSE;
    BB_LIST_ITER bb_iter;
    BB_NODE *pred;
    FOR_ALL_ELEM( pred, bb_iter, Init(lrbb->Bb()->Pred()) ) {
      if (pred->Kind() == BB_ENTRY ||
	  pred->Kind() == BB_REGIONSTART) {
	pred_is_entry = TRUE;
	break;
      }
    }
    if (pred_is_entry) {
      if ( ! Redundant_load_top()->MemberP(lrbb->Bb()) ) {
	Insert_statement( lrbb->Bb(), loadwn, RVI_INS_TOP );
	Redundant_load_top()->Union1D(lrbb->Bb());
      }
    } else {
      BOOL need_copy = FALSE;	// track whether we use loadwn > 1 time
      FOR_ALL_ELEM( pred, bb_iter, Init(lrbb->Bb()->Pred()) ) {
	// insert the load only in preds not in the live-range
	BOOL pred_in_lr = live_range->Block_set()->MemberP( pred );
	
	// may have to consider preds that are in the lr, but end
	// with chi's that kill this value as being out of the lr.
	if ( pred_in_lr && lrbb->Need_load_chi() ) {
	  // does predecessor end with chi that modifies this value
	  if ( Has_end_chi_list(pred) ) {
	    const IDX_32_SET *pred_chi = Bb_end_chi_list(pred);
	    if ( pred_chi != NULL && 
		pred_chi->MemberP(live_range->Bitpos()) )
		{
		  // do not consider this pred to be in the live-range if it 
		  // ends with something that indirectly modifies the value.
		  pred_in_lr = FALSE;
		}
	  }
	}
	
	if ( ! pred_in_lr ) {
	  if ( ! Redundant_load_bot()->MemberP(pred) ) {
	    if ( ! need_copy ) {
	      Insert_statement( pred, loadwn, RVI_INS_POSTCHI );
	      Redundant_load_bot()->Union1D(pred);
	    }
	    else {
	      WN *cp_wn = WN_COPY_Tree(loadwn);
	      Alias_Mgr()->Dup_tree_alias_id( loadwn, cp_wn );
	      WN_dup_dep_vertex(loadwn, cp_wn);
	      Insert_statement( pred, cp_wn, RVI_INS_POSTCHI );
	      Redundant_load_bot()->Union1D(pred);
	    }
	    
	    need_copy = TRUE;		// if we use it again, we need to copy
	  }
	}
      }
    }
  }
  else {
    FmtAssert( FALSE, ("RVI::Insert_load: no loads needed") );
  }
}

// ====================================================================
// Insert stores at appropriate places
// ====================================================================

void
RVI::Insert_store( RVI_LRBB *lrbb, WN *storewn, RVI_LR *live_range ) const
{
  BOOL need_copy = FALSE;	// track whether we use storewn > 1

  // may need to store before the indirect reference at the end of the
  // block.  Note that this may be in addition to storing at the bottom
  // if the value we're storing before the iref is also defined by the
  // the iref wn.
  if ( lrbb->Need_store_iref() ) {
    if ( ! Redundant_store_iref()->MemberP(lrbb->Bb()) ) {
      Insert_statement( lrbb->Bb(), storewn, RVI_INS_BEFORE_IREF );
      Redundant_store_iref()->Union1D(lrbb->Bb());
      need_copy = TRUE;
    }
  }

  // Do we want to insert this store at the bottom of this block, or in
  // the successors that are not in the live-range?
  if ( lrbb->Need_store_bot() ) {
    BOOL need_store_bot = ! Redundant_store_iref()->MemberP(lrbb->Bb());
    if ( ! need_store_bot ) {
      // see if the last stid in the block is for this variable, and
      // check if it's going to be forced to memory at emit time
      // because it has a chi. 
      if ( lrbb->Bb()->Last_stid_bitpos() == live_range->Bitpos() &&
	   ! lrbb->Bb()->Last_stid_has_chi() )
      {
	need_store_bot = TRUE;
      }
    }

    if ( need_store_bot ) {
      WN *cp_wn = storewn;
      if ( need_copy ) {
	cp_wn = WN_COPY_Tree(storewn);
	Alias_Mgr()->Dup_tree_alias_id( storewn, cp_wn );
	WN_dup_dep_vertex(storewn, cp_wn);
      }

      Insert_statement( lrbb->Bb(), cp_wn, RVI_INS_PRECALL );
      Redundant_store_iref()->Union1D(lrbb->Bb());
    }
  }
  else if ( lrbb->Need_store_succ() ) {
    BB_LIST_ITER bb_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM( succ, bb_iter, Init(lrbb->Bb()->Succ()) ) {
      // insert the load only in succs not in the live-range
      if ( ! live_range->Block_set()->MemberP( succ ) ) {
	if ( ! Redundant_store_top()->MemberP(succ) ) {
	  WN *cp_wn = storewn;
	  if ( need_copy ) {
	    cp_wn = WN_COPY_Tree(storewn);
	    Alias_Mgr()->Dup_tree_alias_id( storewn, cp_wn );
	    WN_dup_dep_vertex(storewn, cp_wn);
	  }

	  Insert_statement( succ, cp_wn, RVI_INS_TOP );
	  Redundant_store_top()->Union1D(succ);
	  need_copy = TRUE;	// if we use it again, we need to copy
	}
      }
    }
  }
  else {
    FmtAssert( lrbb->Need_store_iref(), 
      ("RVI::Insert_store: no stores needed") );
  }
}

// ====================================================================
// Annotate this block so we later know to replace the loads/stores
// to the given vnode with the preg
// ====================================================================

void
RVI::Annotate_load_store( BB_NODE *bb, RVI_NODE *node, INT32 preg )
{
  // need to have a list of annotations
  if ( bb->Rvi_anns() == NULL ) {
    bb->Set_rvi_anns( CXX_NEW(RVI_ANN_LIST(),Rvi_ppool()) );
  }
  
  ST *preg_st = MTYPE_To_PREG( node->Mtype() );
  RVI_ANN *rvi_ann = CXX_NEW( RVI_ANN( preg_st, preg, node ),
			      Rvi_ppool() );
  bb->Rvi_anns()->Prepend(rvi_ann);
}

// ====================================================================
// Decide whether or not to change any loads or stores for this
// variable, and then do it if so
// ====================================================================

void
RVI::Insert_loads_stores( RVI_LR *live_range, RVI_NODE *node )
{
  if ( ! live_range->Replace_anything() ) {
    return;
  }

  FmtAssert( live_range->Preg() != 0,
    ("RVI::Insert_loads_stores: no preg for node %d",
     node->Bitpos()) );

  // track redundant code
  Clear_redundant();

  RVI_LRBB_ITER lrbb_iter;
  RVI_LRBB *lrbb;
  FOR_ALL_NODE( lrbb, lrbb_iter, Init(live_range->Blocks()) ) {
    if ( lrbb->Load_cnt() > 0 || lrbb->Store_cnt() > 0 ) {
      Annotate_load_store( lrbb->Bb(), node, live_range->Preg() );
    }
    if ( lrbb->Need_load() ) {
      WN *loadwn = node->Create_load(live_range->Preg(), Alias_Mgr());
      Insert_load( lrbb, loadwn, live_range );
    }
    if ( lrbb->Need_store() ) {
      WN *storewn = node->Create_store(live_range->Preg(), Alias_Mgr());
      Insert_store( lrbb, storewn, live_range );
    }
  }
}

// ====================================================================
// Is the constant handled by CG with shifts for division operations?
// ====================================================================

inline BOOL
Is_div_handled_constant( INT64 con_val )
{
  // is it a power of 2?
  if ( con_val == ( con_val & ~(con_val-1) ) )
    return TRUE;

  return FALSE;
}

// ====================================================================
// Determine if this constant should be a candidate for RVI purposes
// (i.e., it does not fit in the immediate field of the parent)
// ====================================================================

BOOL
RVI::Is_const_candidate( const WN *parent, const WN *constant, INT whichkid ) const
{
  const OPCODE   con_opc  = WN_opcode(constant);
  const OPERATOR con_opr  = OPCODE_operator(con_opc);
  const OPCODE   par_opc  = WN_opcode(parent);
  const OPERATOR par_opr  = OPCODE_operator(par_opc);
  const MTYPE    par_dtyp = OPCODE_rtype(par_opc);
  ST * const     stid_st  = (par_opr == OPR_STID) ? WN_st(parent) : NULL;

  Is_True( con_opr == OPR_INTCONST,
    ("RVI::Is_const_candidate: Bad const: %s", OPCODE_name(con_opc)) );
  const INT64 con_val = WN_const_val(constant);

#if defined(TARG_MIPS) || defined(TARG_LOONGSON)
  if (con_val == 0)
    return FALSE;
#endif

  return !Can_Be_Immediate(par_opr, con_val, par_dtyp, whichkid, stid_st);
}

// ====================================================================
// Determine if this lda should be a candidate for RVI purposes
// (i.e., we'd prefer to leave it as an immediate kid of its parent)
// ====================================================================

BOOL
RVI::Is_lda_candidate( const WN *parent, const WN *lda, INT whichkid ) const
{
  const OPCODE   lda_opc = WN_opcode(lda);
  const OPERATOR lda_opr = OPCODE_operator(lda_opc);
  const OPCODE   par_opc = WN_opcode(parent);
  const OPERATOR par_opr = OPCODE_operator(par_opc);

  Is_True( lda_opr == OPR_LDA,
    ("RVI::Is_lda_candidate: Bad lda: %s", OPCODE_name(lda_opc)) );
  const WN_OFFSET lda_offset = WN_lda_offset(lda);
  ST *lda_st = WN_st(lda);

  switch ( par_opr ) {
    case OPR_ILOAD:
    case OPR_MLOAD:
      // if kid0 is a small lda, do not RVI it
      if ( whichkid == 0 ) {
	return !Uses_Small_Offset( lda_st, lda_offset );
      }
      return TRUE;

    case OPR_ISTORE:
    case OPR_MSTORE:
      // if kid1 is a small lda or it is the lhs of a store of a dedicated
      // register (WN_is_call_related), do not RVI it
      if ( whichkid == 1 ) {
	WN *rhs = WN_kid0(parent);
	return (!Uses_Small_Offset( lda_st, lda_offset ) &&
		!(WN_operator(rhs) == OPR_LDID && // (fix for bug 670407)
		  ST_class(WN_st(rhs)) == CLASS_PREG &&
		  Preg_Is_Dedicated(WN_offset(rhs))));
      }
      return TRUE;

    case OPR_ILOADX:
    case OPR_ISTOREX:
      return FALSE;

    case OPR_CALL:
    case OPR_ICALL:
    case OPR_INTRINSIC_CALL:
    case OPR_PARM:
      // calls end up storing their constant parameters to dedicated
      // registers, which can be quicker if they're left in place as
      // lda's and they're small
      return !Uses_Small_Offset( lda_st, lda_offset );

    case OPR_PICCALL:
      // these calls have the additional argument that is the function
      // address.  Decide whether or not it's safe to handle it.
      if ( whichkid == WN_kid_count(parent)-1 ) {
	if ( Enable_GOT_Call_Conversion ) {
	  // handle even unsafe conversions
	  return TRUE;
	}
	else {
	  // handle only safe conversions, which means we'll handle
	  // address of functions that aren't preemptible
	  if ((Gen_PIC_Call_Shared || Gen_PIC_Shared) && 
	      !ST_visible_outside_dso(lda_st) )
	  {
	    return TRUE;
	  }
	}

	return FALSE;
      }
      else {
	// treat all other kids like other calls'
	//
	// calls end up storing their constant parameters to dedicated
	// registers, which can be quicker if they're left in place as
	// lda's and they're small
	return !Uses_Small_Offset( lda_st, lda_offset );
      }

    case OPR_STID:
      // is this in a store to a register, which usually
      // means in preparation for a call, or return value, so just 
      // let us generate the stid/load-immediate in place if it fits
      if ( ST_class(WN_st(parent)) == CLASS_PREG &&
	   Uses_Small_Offset( lda_st, lda_offset ) )
      {
	// it can be an inlined lda
	return FALSE;
      }
      return TRUE;

    default:
      return TRUE;
  }
}

/* CVTL-RELATED start (correctness) */
// ====================================================================
// Decide if we need a truncation CVTL
// ====================================================================

static BOOL
Need_cvtl_for_store_to_preg( MTYPE store_type, MTYPE rhs_type, 
			     MTYPE preg_type, WN *rhs )
{
  // are we essentially truncating the value?
  if ( MTYPE_size_min(store_type) < MTYPE_size_min(preg_type) &&
       MTYPE_size_min(rhs_type) > MTYPE_size_min(store_type) )
  {
    // check if the value has already been truncated appropriately
    OPCODE new_cvtl_opc = OPCODE_make_op(OPR_CVTL, preg_type, MTYPE_V);
    if ( WN_opcode(rhs) == new_cvtl_opc &&
	 WN_cvtl_bits(rhs) == MTYPE_size_min(store_type) )
    {
      // already has a CVTL that we would have given it, so no need to
      // generate another one
      return FALSE;
    }
    return TRUE;
  }

  return FALSE;
}

// ====================================================================
// Revise a WN that converts the value to the given mtype.  If no
// conversion is necessary, the original "value" tree is returned.
// ====================================================================

void
RVI::Store_to_preg_cvtl(WN     *store_wn, ST*      preg_st,
			TY_IDX  preg_ty,  PREG_NUM preg_num) const
{
  OPCODE store_opc = WN_opcode(store_wn);
  MTYPE store_type = OPCODE_desc(store_opc);
  MTYPE preg_type  = TY_mtype(preg_ty);
  WN   *rhs = WN_kid0(store_wn);
  MTYPE rhs_type = Actual_result_type(rhs); 

  if (WN_operator(rhs) == OPR_INTCONST) {
    MTYPE new_type = Adjust_signed_type(preg_type,
                                        MTYPE_size_min(store_type), rhs);
    if (new_type) rhs_type = new_type;
  }

  // are we essentially truncating the value?
  if ( Need_cvtl_for_store_to_preg(store_type,rhs_type,preg_type,rhs) ){
    OPCODE cvtl_opc = OPCODE_make_op( OPR_CVTL, preg_type, MTYPE_V );
    WN *new_val = WN_CreateCvtl( cvtl_opc, MTYPE_size_min(store_type),
				 rhs );
    WN_kid0(store_wn) = new_val;
  }
  else if (MTYPE_size_min(store_type) <= MTYPE_size_min(rhs_type)){
    INT           cvt_kind; // simulate memory store implied truncation
    OPCODE        opc;
    cvt_kind = Need_type_conversion(rhs_type, store_type, &opc);
    if (cvt_kind == NEED_CVT) {
      WN *new_val = WN_CreateExp1( opc, rhs );
      WN_kid0(store_wn) = new_val;
    }
  }

  WN_store_offset(store_wn) = preg_num;
  WN_st_idx(store_wn) = ST_st_idx(preg_st);
  WN_set_ty( store_wn, preg_ty );

  Alias_Mgr()->Gen_alias_id(store_wn, NULL);
}

// ====================================================================
// Revise a WN that converts the value to the given mtype.  If no
// conversion is necessary, the original "value" tree is returned.
// Also stores the value to memory, so we insert a store to a preg
// before the given store_wn.
// ====================================================================

void
RVI::Store_to_preg_and_mem( BB_NODE *bb, WN *store_wn, 
		 ST* preg_st, TY_IDX preg_ty, PREG_NUM preg_num ) const
{
  OPCODE store_opc = WN_opcode(store_wn);
  MTYPE store_type = OPCODE_desc(store_opc);
  MTYPE preg_type  = TY_mtype(preg_ty);
  WN   *rhs = WN_kid0(store_wn);
  MTYPE rhs_type = Actual_result_type(rhs); 

  if (WN_operator(rhs) == OPR_INTCONST) {
    MTYPE new_type = Adjust_signed_type(preg_type,
                                        MTYPE_size_min(store_type), rhs);
    if (new_type) rhs_type = new_type;
  }

  // are we essentially truncating the value?
  if ( Need_cvtl_for_store_to_preg(store_type,rhs_type,preg_type,rhs) ){
    OPCODE cvtl_opc = OPCODE_make_op( OPR_CVTL, preg_type, MTYPE_V );
    WN *new_val = WN_CreateCvtl( cvtl_opc, MTYPE_size_min(store_type),
				 rhs );
    WN_kid0(store_wn) = new_val;
  }
  else if (MTYPE_size_min(store_type) <= MTYPE_size_min(rhs_type)){
    INT           cvt_kind; // simulate memory store implied truncation
    OPCODE        opc;
    cvt_kind = Need_type_conversion(rhs_type, store_type, &opc);
    if (cvt_kind == NEED_CVT) {
      WN *new_val = WN_CreateExp1( opc, rhs );
      WN_kid0(store_wn) = new_val;
    }
  }

  // generate a new store to a preg
  WN *new_store = WN_CreateStid( WN_opcode(store_wn),
		    preg_num, preg_st, preg_ty, WN_kid0(store_wn) );
  Alias_Mgr()->Gen_alias_id( new_store, NULL );
  WN_Set_Linenum( new_store, WN_Get_Linenum(store_wn) );

  // and then store the preg to the original store location
  const OPCODE ldid_opc = OPCODE_make_op(OPR_LDID,preg_type,preg_type);
  WN *new_ldid = WN_CreateLdid( ldid_opc, preg_num, preg_st, preg_ty );
  Alias_Mgr()->Gen_alias_id( new_ldid, NULL );
  WN_kid0(store_wn) = new_ldid;
  Alias_Mgr()->Set_homing_store( store_wn, TRUE );

  // and insert the preg store before the original store
  bb->Insert_wn_before( new_store, store_wn );
}

// ====================================================================
// Return a WN that performs the necessary conversion so that a load
// using load_opc, can return the value expected from the given WN's
// load from a preg.
// May return the original WN if no conversion is necessary.
// ====================================================================

WN *
RVI::Load_from_preg_cvtl( WN *wn, OPCODE load_opc ) const
{
  const OPCODE opc = WN_opcode(wn);
  if ( opc == load_opc ) 
    return wn;

  if ( Tracing() ) {
    fprintf( TFile, "RVI::Load_from_preg_cvtl: converting\n" );
    fdump_tree( TFile, wn );
  }

  // first need to trick the converter into treating this load like
  // the original load from memory
  WN_set_opcode(wn, load_opc);

  // convert to the desc type
  WN *convert1_wn = WN_Type_Conversion( wn, OPCODE_desc(opc) );

  // convert that to the result type
  WN *convert2_wn = WN_Type_Conversion( convert1_wn, OPCODE_rtype(opc));

  if ( Tracing() ) {
    fprintf( TFile, "  converted value\n" );
    fdump_tree( TFile, convert2_wn );
  }

  return convert2_wn;
}
/* CVTL-RELATED finish */

// ====================================================================
// Temporary (?) hack to make sure dedicated pregs defined by function 
// calls are stored to pregs rather than to things RVI may work on.
// This may become unnecessary if this is a WHIRL requirement.
// ====================================================================

static BOOL
WN_is_store_of_ded_reg( const WN *wn )
{
  const OPERATOR opr = WN_operator(wn);

  if ( opr == OPR_STID || opr == OPR_ISTORE || opr == OPR_ISTOREX )
  {
    WN *val = WN_kid0(wn);
    const OPERATOR val_opr = WN_operator(val);
    if ( val_opr == OPR_LDID ) {
      ST *val_st = WN_st(val);
      if ( ST_class(val_st) == CLASS_PREG && 
	   Preg_Is_Dedicated(WN_offset(val)) )
      {
	return TRUE;
      }
    }
  }

  // must not be store of ded reg.
  return FALSE;
}

inline BOOL
WN_is_store_to_preg( const WN *wn )
{
  const OPERATOR opr = WN_operator(wn);

  if ( opr == OPR_STID ) {
    if ( ST_class(WN_st(wn)) == CLASS_PREG ) {
      return TRUE;
    }
  }

  // must not be store of ded reg.
  return FALSE;
}

static void
Replace_store_of_ded_reg( BB_NODE *bb, 
			  WN *wn, WN *insert_ded_reg_ref_before,
			  ALIAS_MANAGER *alias_mgr,
			  BOOL trace )
{
  WN *ded_ldid = WN_kid0(wn);
  ST *ded_st   = WN_st(ded_ldid);
  MTYPE reg_type = TY_mtype(ST_type(ded_st));
  PREG_NUM pregnum = Create_Preg( reg_type, NULL/*name*/);

  if ( trace ) {
    fprintf( TFile, "Replace_store_of_ded_reg: BB:%d replacing:\n",
	     bb->Id() );
    fdump_tree( TFile, wn );
  }

  // change original store by storing new preg
  WN *new_ldid = WN_LdidPreg( reg_type, pregnum );
  alias_mgr->Gen_alias_id( new_ldid, NULL );
  WN_kid0(wn) = new_ldid;

  // create new store of dedicated register
  WN *new_store = WN_StidIntoPreg( reg_type, pregnum, ded_st, ded_ldid);
  alias_mgr->Gen_alias_id( new_store, NULL );

  // and insert the new store right before original store
  bb->Insert_wn_before( new_store, insert_ded_reg_ref_before );

  if ( trace ) {
    fprintf( TFile, "  replaced with:\n" );
    fdump_tree( TFile, new_store );
    fdump_tree( TFile, wn );
  }
}


void
RVI::Set_callrel() const
{
  // For cases when we do not need to copy dedicated registers using
  // RVI::Copy_dedicated_regs_to_pregs(), we will still need to set 
  // the Callrel() attribute of BBs correctly for subsequent calls to
  // BB_NODE::Prepend_wn_after_labels().
  //
  CFG_ITER cfg_iter(Cfg());
  BB_NODE *bb;
  WN      *insert_ded_preg_ref_before = NULL;
  
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {

    if (!bb->Callrel() && bb->Kind() == BB_ENTRY) {
      // the successor (successors?) of an entry block may in fact
      // have some instructions at their top that look like
      // call-related code. (copy regs to homes, up-level links)
      // So, determine if this is the case, and mark them specially
      BB_NODE *succ;
      BB_LIST_ITER bb_succ_iter;
      FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
	succ->Set_callrel();
      }
    }
  }
} // RVI::Set_callrel()


void
RVI::Copy_dedicated_regs_to_pregs( void ) const
{
  // Note: as a side-effect this also does what RVI::Set_callrel() does.
  //
  CFG_ITER cfg_iter(Cfg());
  BB_NODE *bb;
  WN      *insert_ded_preg_ref_before = NULL;
  
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {

    // does it have call-related code?
    if ( bb->Callrel() ) {
      // find the first store in the block
      WN *wn;
      for ( wn = bb->Firststmt(); wn != NULL; wn = WN_next(wn) ) {
	if ( OPCODE_is_store(WN_opcode(wn)) )
	  break;
      }

      // did we find a store of a dedicated register?
      if ( wn != NULL && WN_is_store_of_ded_reg(wn) ) {
	if ( ! WN_is_store_to_preg( wn ) ) {
	  Replace_store_of_ded_reg( bb, wn, wn, Alias_Mgr(), Tracing() );
	  insert_ded_preg_ref_before = wn;
	}
      }
      if (insert_ded_preg_ref_before != NULL)
      {
	 // Look for a second return register, and make sure the load of the
	 // two return registers remain back-to-back.
	 //
	 wn = WN_next(wn);
	 if ( wn != NULL && WN_is_store_of_ded_reg(wn) ) {
	    if ( ! WN_is_store_to_preg( wn ) ) {
	       Replace_store_of_ded_reg( bb, wn, insert_ded_preg_ref_before,
					 Alias_Mgr(), Tracing() );
	       insert_ded_preg_ref_before = NULL;
	    }
	 }
      }
    }
    else if ( bb->Kind() == BB_ENTRY ) {
      // the successor (successors?) of an entry block may in fact
      // have some instructions at their top that look like
      // call-related code. (copy regs to homes, up-level links)
      // So, determine if this is the case, and mark them specially
      BB_NODE *succ;
      BB_LIST_ITER bb_succ_iter;
      FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
	succ->Set_callrel();
      }
    }
  }
}

