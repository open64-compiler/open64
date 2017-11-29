//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_rvilr.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rvilr.cxx,v $
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
// Live-Ranges used for Register-Variable Identification.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_rvilr_CXX	"opt_rvilr.cxx"
static char *rcs_id = 	opt_rvilr_CXX"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "wn.h"
#include "cxx_memory.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_base.h"
#include "opt_bb.h"
#include "opt_rvilr.h"
#include "opt_rvitab.h"
#include "opt_rvi.h"
#include "opt_util.h"
#include "bb_node_set.h"
#include "idx_32_set.h"


// ====================================================================
// local class to help in analysis
// ====================================================================

class RVI_LR_INFO {
private:
  BOOL		_bb_mu_ref;	// this bb ends with wn that irefs val
  BOOL		_bb_chi_def;	// this bb ends with wn that idefs val
  INT		_pred_in;	// # preds in live-range
  INT		_pred_out;	// # preds not in live_range
  BOOL		_pred_out_chi;	// a pred-out has chi that mods value
  BOOL		_pred_out_succ_out;// is any pred-out's succ out too
  BOOL		_pred_out_deep;	// is any pred-out in deeper loop?
  BOOL		_succ_has_load;	// successor has load from memory
  INT		_succ_in;	// # succs in live_range
  INT		_succ_out;	// # succs not in live_range
  BOOL		_succ_out_pred_out;// is any succ-out's pred out too
  BOOL		_succ_out_deep;	// is any succ-out in deeper loop?
  BOOL		_succ_out_live_out;// is any succ-out live-out too?

  RVI_LR_INFO(const RVI_LR_INFO&);
  RVI_LR_INFO& operator = (const RVI_LR_INFO&);

public:

  RVI_LR_INFO( void )
		{
		  _bb_mu_ref = FALSE;
		  _bb_chi_def = FALSE;
		  _pred_in = 0;
		  _pred_out = 0;
		  _pred_out_chi = FALSE;
		  _pred_out_succ_out = FALSE;
		  _pred_out_deep = FALSE;
		  _succ_has_load = FALSE;
		  _succ_in = 0;
		  _succ_out = 0;
		  _succ_out_pred_out = FALSE;
		  _succ_out_deep = FALSE;
		  _succ_out_live_out = FALSE;
		}
  ~RVI_LR_INFO( void ) {}

  BOOL Bb_mu_ref( void ) const
		{ return _bb_mu_ref; }
  void Set_bb_mu_ref( void )
		{ _bb_mu_ref = TRUE; }
  BOOL Bb_chi_def( void ) const
		{ return _bb_chi_def; }
  void Set_bb_chi_def( void )
		{ _bb_chi_def = TRUE; }
  INT Pred_in( void ) const
		{ return _pred_in; }
  void Set_pred_in( INT num )
		{ _pred_in = num; }
  INT Pred_out( void ) const
		{ return _pred_out; }
  void Set_pred_out( INT num )
		{ _pred_out = num; }
  BOOL Pred_out_chi( void ) const
		{ return _pred_out_chi; }
  void Set_pred_out_chi( void )
		{ _pred_out_chi = TRUE; }
  BOOL Pred_out_succ_out( void ) const
		{ return _pred_out_succ_out; }
  void Set_pred_out_succ_out( void )
		{ _pred_out_succ_out = TRUE; }
  BOOL Pred_out_deep( void ) const
		{ return _pred_out_deep; }
  void Set_pred_out_deep( void )
		{ _pred_out_deep = TRUE; }
  BOOL Succ_has_load( void ) const
		{ return _succ_has_load; }
  void Set_succ_has_load( void )
		{ _succ_has_load = TRUE; }
  INT Succ_in( void ) const
		{ return _succ_in; }
  void Set_succ_in( INT num )
		{ _succ_in = num; }
  INT Succ_out( void ) const
		{ return _succ_out; }
  void Set_succ_out( INT num )
		{ _succ_out = num; }
  BOOL Succ_out_pred_out( void ) const
		{ return _succ_out_pred_out; }
  void Set_succ_out_pred_out( void )
		{ _succ_out_pred_out = TRUE; }
  BOOL Succ_out_deep( void ) const
		{ return _succ_out_deep; }
  void Set_succ_out_deep( void )
		{ _succ_out_deep = TRUE; }
  BOOL Succ_out_live_out( void ) const
		{ return _succ_out_live_out; }
  void Set_succ_out_live_out( void )
		{ _succ_out_live_out = TRUE; }

  // Some analysis support
  BOOL Load_should_be_in_pred( void ) const;
  BOOL Store_should_be_in_succ( void ) const;

  void Print( void ) const;
};

// ====================================================================
// Print out the LR block
// ====================================================================

void
RVI_LRBB::Print( FILE *fp ) const
{
  fprintf( fp, "BB:%d, %d loads, %d stores", 
	   _bb->Id(), _load_cnt, _store_cnt );
  if ( First_is_store() )
    fprintf( fp, " STR" );
  if ( Need_store_bot() )
    fprintf( fp, " ND_STR_BOT" );
  if ( Need_store_succ() )
    fprintf( fp, " ND_STR_SUCC" );
  if ( Need_store_iref() )
    fprintf( fp, " ND_STR_IREF" );
  if ( Need_load_here() )
    fprintf( fp, " ND_LOD_HERE" );
  if ( Need_load_pred() )
    fprintf( fp, " ND_LOD_PRED" );
  if ( Need_load_chi() )
    fprintf( fp, " ND_LOD_CHI" );

  fprintf( fp, "\n" );
}

// ====================================================================
// Locate the block in the list
// ====================================================================

RVI_LRBB *
RVI_LRBB_LIST::Find( const BB_NODE *bb )
{
  RVI_LRBB_ITER lrbb_iter;
  RVI_LRBB *lrbb;

  FOR_ALL_NODE( lrbb, lrbb_iter, Init(this) ) {
    if ( bb == lrbb->Bb() )
      return lrbb;
  }

  return NULL;
}

// ====================================================================
// Constructor for live-ranges
// ====================================================================

RVI_LR::RVI_LR( IDX_32 bitpos, const CFG *cfg, MEM_POOL *pool ) :
  _bitpos(bitpos)
{
  _block_set = CXX_NEW(BB_NODE_SET(cfg->Total_bb_count(), cfg, pool,
				   BBNS_EMPTY), pool);
  _preg = 0;
  _load_cnt  = 0; 
  _store_cnt = 0; 
  _flags = LRF_NONE;
}

// ====================================================================
// Print out the live-range
// ====================================================================

void
RVI_LR::Print( FILE *fp )
{
  RVI_LRBB_ITER lrbb_iter;
  RVI_LRBB *lrbb;

  fprintf( fp, " Bitpos:%d", Bitpos() );

  if ( Preg() != 0 ) {
    fprintf( fp, " Preg:%d", Preg() );
  }
  if ( Load_cnt() != 0 ) {
    fprintf( fp, " Load_cnt:%d", Load_cnt() );
  }
  if ( Store_cnt() != 0 ) {
    fprintf( fp, " Store_cnt:%d", Store_cnt() );
  }
  if ( Replace_anything() ) {
    fprintf( fp, " Replace" );
  }
  if ( Need_home() ) {
    fprintf( fp, " Need_home" );
  }
  if ( Predout_nostore() ) {
    fprintf( fp, " Predout_nostore" );
  }

  fprintf( fp, "\n" );

  FOR_ALL_NODE( lrbb, lrbb_iter, Init(Blocks()) ) {
    lrbb->Print( fp );
  }
}

// ====================================================================
// Recursive function that starts with a block in a live-range and then
// visits preds and succs trying to extend the live-range in both
// directions.
// ====================================================================

void
RVI::Build_up_live_range( RVI_LR *live_range, BB_NODE *bb, 
		     RVI_LRBB_LIST *appearances,
		     BB_NODE_SET *visited_bbs, MEM_POOL *pool ) const
{
  // have we already visited this block?
  if ( visited_bbs->MemberP( bb ) )
    return;

  IDX_32 bitpos = live_range->Bitpos();

  // say we've visited this block
  visited_bbs->Union1D( bb );

  // should we find the lrbb in the appearance list?
  RVI_LRBB *lrbb = NULL;
  if ( bb->Loc_appear()->MemberP( bitpos ) ) {
    // find it in the appearances list and remove it
    RVI_LRBB_ITER lrbb_iter;
    RVI_LRBB *prev = NULL;

    FOR_ALL_NODE( lrbb, lrbb_iter, Init(appearances) ) {
      if ( lrbb->Bb() == bb ) {
	lrbb = appearances->Remove( prev, lrbb );
	break;
      }
      else {
	prev = lrbb;
      }
    }
    Is_True( lrbb != NULL,
      ("Build_up_live_range: bitpos %d, bb:%d does not appear",
       bitpos, bb->Id()) );
  }

  // is this block in one of bitpos' live-ranges?
  if ( bb->Defreach()->MemberP( bitpos ) ) {

    // shouldn't already be a member of the set
    Is_True( !live_range->Block_set()->MemberP( bb ),
      ("RVI::Build_up_live_range: BB:%d already a member for bit %d",
       bb->Id(), bitpos) );

    // did we find the lrbb before?
    if ( lrbb == NULL ) {
      // need to create a new one for this block where the live-range 
      // passes through
      lrbb = CXX_NEW(RVI_LRBB(bb),pool);
    }

    // add this block to the live-range
    live_range->Blocks()->Prepend( lrbb );
    live_range->Block_set()->Union1D( bb );

    // check out this block's preds and succs to extend the live-range
    // in both directions
    BB_NODE *pred, *succ;
    BB_LIST_ITER bb_list_iter;
    FOR_ALL_ELEM( pred, bb_list_iter, Init(bb->Pred()) ) {
      if ( ! visited_bbs->MemberP( pred ) ) {

	// Determine if this pred ends the live-range because it ends 
	// with something that indirectly references the value.  If 
	// the live-range continues with this pred, we'll end up 
	// putting it in a different live-range later when dealing 
	// with its references
	BOOL visit_pred = TRUE;
	if ( visit_pred && Has_end_chi_list(pred) ) {
	  const IDX_32_SET *pred_chi = Bb_end_chi_list(pred);
	  if ( pred_chi != NULL && pred_chi->MemberP(bitpos) ) {
	    if ( pred->Last_stid_bitpos() == bitpos ) {
	      // we ignore chi's on stores to this variable for now,
	      // but we will later make sure to update memory at this
	      // store
	    }
	    else {
	      visit_pred = FALSE;
	    }
	  }
	}

	if ( visit_pred ) {
	  Build_up_live_range( live_range, pred, appearances,
			       visited_bbs, pool );
	}
      }

      if ( ! live_range->Block_set()->MemberP( pred ) ) {
	// keep track of LRs that have preds out of the live-range,
	// thus possibly requiring a load if this block doesn't start
	// with a store. (see pv #311244)
	if ( ! lrbb->First_is_store() ) {
	  live_range->Set_predout_nostore();
	}
      }
    }

    // Determine if this block ends the live-range because it ends with
    // something that indirectly references the value.  If the live-
    // range continues with any of these succs, we'll end up putting
    // them in a different live-range later when dealing with their
    // references
    BOOL visit_succs = TRUE;
    if ( visit_succs && Has_end_chi_list(bb) ) {
      const IDX_32_SET *pred_chi = Bb_end_chi_list(bb);
      if ( pred_chi != NULL && pred_chi->MemberP(bitpos) ) {
	if ( bb->Last_stid_bitpos() == bitpos ) {
	  // we can ignore chi's on stores to this variable for now,
	  // but we will later make sure to update memory at this store
	}
	else {
	  visit_succs = FALSE;
	}
      }
    }

    if ( visit_succs ) {
      FOR_ALL_ELEM( succ, bb_list_iter, Init(bb->Succ()) ) {
	if ( ! visited_bbs->MemberP( succ ) ) {
	  Build_up_live_range( live_range, succ, appearances,
			       visited_bbs, pool );
	}
      }
    }

  } // end if block is in a bitpos live-range
}

// ====================================================================
// Build list of live-range structures for the given variable
// ====================================================================

void
RVI::Build_live_ranges( RVI_NODE *rvi_node, MEM_POOL *pool ) const
{
  RVI_LRBB *lrbb, *prev_lrbb;
  BB_NODE_SET visited( Cfg()->Last_bb_id()+1, Cfg(), pool, BBNS_EMPTY );

  // starting with each "appear" block, add it to a live-range and
  // check if any of its preds/succs are also in the live-range.
  // Note that Build_up_live_range() removes items from our Appearances
  // list.  This then guarantees that we should terminate at some point

  prev_lrbb = NULL;
  while ( (lrbb = rvi_node->Appearances()->Head()) != NULL ) {
    // make sure we're progressing
    Is_True( lrbb != prev_lrbb,
      ("RVI::Build_live_range (rvi_node:%d): no progress made (bb:%d)",
       rvi_node->Bitpos(), lrbb->Bb()->Id()) );
    prev_lrbb = lrbb;

    if ( rvi_node->Live_ranges() == NULL ) {
      rvi_node->Set_live_ranges(CXX_NEW(RVI_LR_LIST(), pool));
    }

    // create a new empty live-range
    RVI_LR *lr = CXX_NEW(RVI_LR(rvi_node->Bitpos(),Cfg(),pool), pool);
    rvi_node->Live_ranges()->Prepend(lr);

    // visit the preds and succs of this block to create the full lr
    Build_up_live_range( lr, lrbb->Bb(),
			 rvi_node->Appearances(), &visited, pool );
  }
}

// ====================================================================
// Count up the number of predecessors in/not-in the live-range
// as well as other information about predecessors
// Fills in the lr_info structure.
// ====================================================================

void
RVI_LR::Analyze_preds( const BB_NODE *bb, const RVI *rvi, 
			RVI_LR_INFO *lr_info )
{
  BB_LIST_ITER bb_pred_iter;
  BB_NODE *pred;
  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
    BOOL pred_in_lr = Block_set()->MemberP( pred );

    // does predecessor end with chi that modifies this value
    if ( rvi->Has_end_chi_list(pred) ) {
      const IDX_32_SET *pred_chi = rvi->Bb_end_chi_list(pred);
      if ( pred_chi != NULL && pred_chi->MemberP(Bitpos()) ) {
	// do not consider this pred to be in the live-range if it ends
	// with something that indirectly modifies the value.
	pred_in_lr = FALSE;
	lr_info->Set_pred_out_chi();
      }
    }

    if ( pred_in_lr ) {
      lr_info->Set_pred_in( lr_info->Pred_in() + 1 );
    }
    else {
      // this predecessor is not in this live-range
      lr_info->Set_pred_out( lr_info->Pred_out() + 1 );

      // is predecessor in a more deeply-nested loop
      if ( pred->Loopdepth() > bb->Loopdepth() )
	lr_info->Set_pred_out_deep();

      // are there any successors of this pred that are also not in
      // this live-range?
      if ( ! lr_info->Pred_out_succ_out() ) {
	BB_LIST_ITER bb_predsucc_iter;
	BB_NODE *predsucc;
	FOR_ALL_ELEM( predsucc, bb_predsucc_iter, Init(pred->Succ()) ) {
	  if ( ! Block_set()->MemberP( predsucc ) ) {
	    lr_info->Set_pred_out_succ_out();
	  }
	  else if ( predsucc != bb ) {
	    // this succ (not me) is also in the live-range.  If it
	    // either starts with a load or store, we'd be adding
	    // inefficiencies (and possibly incorrect code) if we put 
	    // a load in this pred.  (see pv #291710)
	    RVI_LRBB *predsucc_lrbb = Blocks()->Find( predsucc );
	    if ( predsucc_lrbb->First_is_store() ||
		 predsucc_lrbb->Need_load_here() )
	    {
	      // slight hack, but say this block has a pred with a
	      // successor out of the live-range to force us to put
	      // the load in the block itself.
	      lr_info->Set_pred_out_succ_out();
	    }
	  }
	}
      }
    }
  }
}

// ====================================================================
// Analyze this block and its successors
// NOTE:  Assumed that all processing for Loads has been previously
// performed.
// ====================================================================

void
RVI_LR::Analyze_succs( const BB_NODE *bb, const RVI *rvi,
			RVI_LR_INFO *lr_info )
{
  BOOL maybe_live_out = FALSE;

  // check for indirect references at the end of this block
  if ( rvi->Has_end_mu_list(bb) ) {
    const IDX_32_SET *bb_mu = rvi->Bb_end_mu_list(bb);
    if ( bb_mu != NULL && bb_mu->MemberP(Bitpos()) ) {
      lr_info->Set_bb_mu_ref();
    }
  }

  // check for indirect definitions at the end of this block
  if ( rvi->Has_end_chi_list(bb) ) {
    const IDX_32_SET *bb_chi = rvi->Bb_end_chi_list(bb);
    if ( bb_chi != NULL && bb_chi->MemberP(Bitpos()) ) {
      lr_info->Set_bb_chi_def();
      Is_True( bb->Last_stid_bitpos() != Bitpos(),
	("RVI_LR::Analyze_succs: Bb_end_chi_list kills last_stid") );
    }
  }

  // is the value possibly live-out?
  if ( bb->Live_out()->MemberP( Bitpos() ) ) {
    maybe_live_out = TRUE;

    // if this is an exit block then the value is indeed live-out
    if ( rvi->Is_exit_block(bb) ) {
      lr_info->Set_succ_out_live_out();
    }
  }

  // is there any need to analyze the successors because we already
  // know that we need to store in this block before a mu-ref?
  if ( ! lr_info->Bb_mu_ref() || bb->Last_stid_bitpos() == Bitpos() ) {
    BB_LIST_ITER bb_succ_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
      BOOL succ_in_lr = Block_set()->MemberP( succ );

      if ( succ_in_lr ) {
	lr_info->Set_succ_in( lr_info->Succ_in() + 1 );

	RVI_LRBB *succlrbb = Blocks()->Find( succ );
	Is_True( succlrbb != NULL,
	  ("RVI_LR::Analyze_succs: successor BB:%d has no lrbb",
	   succ->Id()) );

	if ( succlrbb->Need_load_here() ) {
	  lr_info->Set_succ_has_load();
	}
	else if ( succlrbb->Need_load_chi() &&
		  lr_info->Bb_chi_def() )
	{
	  // this successor is going to insert loads in all preds that
	  // are either out of the live-range, or are in the live-range
	  // and have chi's that define the value.  This latter one is
	  // now the case, so say the successor has a load, which will
	  // force us to insert a store before the chi-def.
	  lr_info->Set_succ_has_load();
	}
      }
      else {
	// this succecessor is not in this live-range
	lr_info->Set_succ_out( lr_info->Succ_out() + 1 );

	// is successor in a more deeply-nested loop
	if ( succ->Loopdepth() > bb->Loopdepth() )
	  lr_info->Set_succ_out_deep();

	if ( maybe_live_out ) {
	  // is value live-out because of this succ that's not in the lr
	  if ( succ->Loc_upwd()->MemberP( Bitpos() ) ) {
	    // if the value is locally upward-exposed, then it's 
	    // definitely live-out of this block.
	    lr_info->Set_succ_out_live_out();
	  }
	  else if ( succ->Live_out()->MemberP( Bitpos() ) ) {
	    // make sure it's not locally-defined there which makes the
	    // live-outness not get to this block
	    if ( ! succ->Loc_def()->MemberP( Bitpos() ) ) {
	      lr_info->Set_succ_out_live_out();
	    }
	  }
	}

	// are there any predecessors of this succ that are also not in
	// this live-range?  (don't repeat check if we've already found
	// it out before)
	if ( ! lr_info->Succ_out_pred_out() ) {
	  BB_LIST_ITER bb_succpred_iter;
	  BB_NODE *succpred;
	  FOR_ALL_ELEM(succpred, bb_succpred_iter, Init(succ->Pred())) {
	    if ( ! Block_set()->MemberP( succpred ) ) {
	      lr_info->Set_succ_out_pred_out();
	    }
	    else {
	      // we think this succ's pred is in the live-range, but
	      // see if it ends with something that essentially makes
	      // it the end of its live-range (like a chi that mods
	      // the value)
	      if ( rvi->Has_end_chi_list(succpred) ) {
		const IDX_32_SET *succpred_chi = 
					rvi->Bb_end_chi_list(succpred);
		if ( succpred_chi != NULL && 
		     succpred_chi->MemberP(Bitpos()) )
		{
		  // do not consider this pred to be in the live-range
		  // if it ends with something that indirectly modifies
		  // the value.
		  lr_info->Set_succ_out_pred_out();
		}
	      }
	    }
	  } // end for all preds of this succ

	}
      }
    } // end for all succs of this bb

  }
}

// ====================================================================
// Decide if the load should be at the top of this block or in the
// predecessors, given the info
// ====================================================================

inline BOOL
RVI_LR_INFO::Load_should_be_in_pred( void ) const
{
  // are there any predecessors that are not in the live-range and
  // have successors that are also not in the live-range
  if ( Pred_out_succ_out() )
    return FALSE;

  // are there any predecessors not in the live-range? (entry??)
  // are there more predecessors not in the live-range than in?
  // (avoids bottom of switch statements getting duplicated in all
  //  of the cases)
  if ( Pred_out() == 0 || Pred_out() > Pred_in() )
    return FALSE;

  // is one of the predecessors in a more deeply-nested loop?
  if ( Pred_out_deep() )
    return FALSE;

  return TRUE;
}

// ====================================================================
// Decide if the store should be at the bottom of this block or in the
// successors, given the info
// ====================================================================

inline BOOL
RVI_LR_INFO::Store_should_be_in_succ( void ) const
{
  // are there any successors that are not in the live-range and
  // have predecessors that are also not in the live-range
  if ( Succ_out_pred_out() )
    return FALSE;

  // are there any successors not in the live-range? (exit??)
  // are there more successors not in the live-range than in?
  // (avoids bottom of switch statements getting duplicated in all
  //  of the cases)
  if ( Succ_out() == 0 || Succ_out() > Succ_in() )
    return FALSE;

  // is one of the succecessors in a more deeply-nested loop?
  if ( Succ_out_deep() )
    return FALSE;

  return TRUE;
}

// ====================================================================
// ====================================================================

void
RVI_LR_INFO::Print( void ) const
{
  fprintf( TFile, " p-in:%d p-out:%d s-in:%d s-out:%d\n",
    Pred_in(), Pred_out(), Succ_in(), Succ_out() );

  if ( Bb_mu_ref() )
    fprintf( TFile, " Bb_mu_ref" );

  if ( Bb_chi_def() )
    fprintf( TFile, " Bb_chi_def" );

  if ( Pred_out_chi() )
    fprintf( TFile, " Pred_out_chi" );

  if ( Pred_out_succ_out() )
    fprintf( TFile, " Pred_out_succ_out" );

  if ( Pred_out_deep() )
    fprintf( TFile, " Pred_out_deep" );

  if ( Succ_has_load() )
    fprintf( TFile, " Succ_has_load" );

  if ( Succ_out_pred_out() )
    fprintf( TFile, " Succ_out_pred_out" );

  if ( Succ_out_deep() )
    fprintf( TFile, " Succ_out_deep" );

  if ( Succ_out_live_out() )
    fprintf( TFile, " Succ_out_live_out" );

  fprintf( TFile, "\n" );
}

// ====================================================================
// Determine if we should bother doing anything with this live-range
// ====================================================================

BOOL
RVI_LR::Do_anything( void )
{
  // if 2 loads or more, or 2 stores or more, we'll do something
  if ( Load_cnt() > 1 || Store_cnt() > 1 )
    return TRUE;

  // have either a single reference, or 1 store and 1 load.

  // see if the live-range is inside a loop
  // This means that either the live-range includes more than a single
  // block, or it's a self-loop block
  // NOTE that this loop has a max of 2 iterations, so it's not costly,
  // and it's quicker than counting all of the blocks to see if it's
  // greater than 1.
  RVI_LRBB_ITER lrbb_iter;
  RVI_LRBB *lrbb, *first_lrbb = NULL;
  INT count = 0;
  FOR_ALL_NODE( lrbb, lrbb_iter, Init( Blocks() ) ) {
    count++;
    // on second trip through this loop, we know we've got more than
    // a single block live-range, so we know our answer
    if ( count > 1 )
      return TRUE;

    // on first (only?) trip through this loop, see if this block is
    // in a loop
    BB_NODE *bb = lrbb->Bb();
    if ( bb->Loopdepth() > 0 )
      return TRUE;

    // track the first/only block in the live-range
    // NOTE: can do this because if there is a second trip through
    // this loop, we return above.
    first_lrbb = lrbb;
  }

  // have a single block live-range, with either a single ref or
  // 1 store and 1 load.
  if ( Load_cnt() == 1 && Store_cnt() == 1 ) {
    // see if we're going to need both a load and a store, in which 
    // case, we're not making things better
    if ( ! (first_lrbb->Need_store() && first_lrbb->Need_load()) ) {
      return TRUE;
    }
  }

  return FALSE;
}

// ====================================================================
// Analyze all of the live-ranges for the bitpos
// ====================================================================

void
RVI::Analyze_live_range( RVI_LR *live_range ) const
{
  RVI_LRBB_ITER lrbb_iter;
  RVI_LRBB *lrbb;
  BOOL need_home = FALSE;
  IDX_32 bitpos = live_range->Bitpos();

  // count up references first
  FOR_ALL_NODE( lrbb, lrbb_iter, Init( live_range->Blocks() ) ) {
    BB_NODE *bb = lrbb->Bb();
    UINT loads = lrbb->Load_cnt();
    if ( loads > 0 ) {
      live_range->Set_load_cnt(live_range->Load_cnt() + loads );
    }
    UINT stores = lrbb->Store_cnt();
    if ( stores > 0 ) {
      live_range->Set_store_cnt( live_range->Store_cnt() + stores );
    }
  }

  // Evaluate all blocks for loads
  //
  // Do this before handling stores because we may be required to place
  // a store in a block simply because one of its successors, which is
  // also in the live-range needs a load.
  if ( live_range->Load_cnt() > 0 || live_range->Predout_nostore() ) {
    FOR_ALL_NODE( lrbb, lrbb_iter, Init( live_range->Blocks() ) ) {
      BB_NODE *bb = lrbb->Bb();

      RVI_LR_INFO lr_info;
      live_range->Analyze_preds( bb, this, &lr_info );

#ifdef DEBUGGING
if(Tracing()){
  fprintf( TFile, "Analyze_preds for bitpos:%d in bb:%d\n", bitpos,
	   bb->Id());
  lr_info.Print();
}
#endif // DEBUGGING

      if ( lr_info.Pred_out() > 0 ) {
	// does this block store to the value for its first reference?
	if ( ! lrbb->First_is_store() ) {
	  // this block needs to have the value loaded at the top, but
	  // should it be done in this block, or at the bottom of the
	  // predecessors?
	  if ( lr_info.Load_should_be_in_pred() ) {
	    if ( lr_info.Pred_out_chi() ) {
	      // for preds that have chi's that kill this value, we
	      // need to insert after those statements.
	      lrbb->Set_need_load_chi();
	    }
	    else {
	      lrbb->Set_need_load_pred();
	    }
	  }
	  else {
	    lrbb->Set_need_load_here();
	  }

	  // we're loading this value from its home
	  need_home = TRUE;
	}
      }
    }
  }

  // Evaluate all blocks for stores
  // 
  if ( live_range->Store_cnt() > 0 ) {
    FOR_ALL_NODE( lrbb, lrbb_iter, Init( live_range->Blocks() ) ) {
      BB_NODE *bb = lrbb->Bb();

      // only need to store if a definition has reached this block
      if ( bb->Unstored_defs()->MemberP(bitpos) ||
	   bb->Last_stid_bitpos() == bitpos )
      {

	// do not need to store if we're inserting a load at the top
	// of this block, and there are no definitions in this block
	if ( lrbb->Need_load_here() && lrbb->Store_cnt() == 0 )
	  continue;

	RVI_LR_INFO lr_info;
	live_range->Analyze_succs( bb, this, &lr_info );

#ifdef DEBUGGING
if(Tracing()){
  fprintf( TFile, "Analyze_succs for bitpos:%d in bb:%d\n", bitpos,
	   bb->Id());
  lr_info.Print();
}
#endif // DEBUGGING

	// Note that we may need to store before the iref, and after
	// it as well if the iref wn also defines this variable.
	BOOL may_need_to_store_after = TRUE;

	// do we need a store in this block because it ends with wn
	// that indirectly references the value?   
	if ( lr_info.Bb_mu_ref() &&
	     bb->Unstored_defs()->MemberP(bitpos) )
	{
	  lrbb->Set_need_store_iref();
	  // we're storing this value to its home
	  need_home = TRUE;


	  // Fix 366941:  if we need to insert an istore at the top of the block,
	  // and there is predecessor outside of the live-range.  Need to load
	  // the variable into the preg.
	  // Is_True(lrbb->Need_store_iref()) 
	  //
	  {
	    BB_NODE *bb = lrbb->Bb();
	    RVI_LR_INFO lr_info;
	    live_range->Analyze_preds(bb, this, &lr_info);
	    if (lr_info.Pred_out() > 0) {
	      // Find out if the preg is exposed.
	      BOOL need_load_pred = TRUE;
	      if (bb->Loc_mu_wn() != NULL) {
		for (WN *stmt = WN_prev(bb->Loc_mu_wn()); stmt != NULL; stmt = WN_prev(stmt)) {
		  if (WN_operator(stmt) == OPR_STID && Get_bitpos(stmt) == bitpos) {
		    need_load_pred = FALSE;
		    break;
		  }
		}
	      }
	      if (need_load_pred)
		lrbb->Set_need_load_pred();
	    }
	  }

	  // do not need to store again, unless this block ends with
	  // another store to this variable
	  if ( bb->Last_stid_bitpos() != bitpos ) {
	    may_need_to_store_after = FALSE;
	  }
	}

	if ( may_need_to_store_after ) {
	  // do not need to store after if there's a stid with a chi
	  // at the end of this block that defines this variable 
	  // because the emitter will store to both a preg and memory
	  // in that case.
	  if ( bb->Last_stid_bitpos() == bitpos &&
	       bb->Last_stid_has_chi() )
	  {
	    may_need_to_store_after = FALSE;
	    // even though we're not storing now, we will do so at
	    // emit time
	    need_home = TRUE;
	  }
	}

#ifdef DEBUGGING
if(Tracing()){
  fprintf( TFile, "  may_need_to_store_after=%d\n",
		     may_need_to_store_after );
}
#endif // DEBUGGING

	if ( may_need_to_store_after ) {
	  if ( lr_info.Succ_has_load() ) {
	    if ( lr_info.Bb_chi_def() ) {
	      lrbb->Set_need_store_iref();
	    }
	    else {
	      lrbb->Set_need_store_bot();
	    }

	    // we're storing this value to its home
	    need_home = TRUE;
	  }
	  else if ( lr_info.Succ_out() > 0 ) {
	    // is the value live-out of the block?
	    if ( lr_info.Succ_out_live_out() ) {
	      // this block needs to store the value, but should it be 
	      // before a chi-def, or at the bottom of this block, or 
	      // at the top of the successors?
	      if ( lr_info.Bb_chi_def() ) {
		lrbb->Set_need_store_iref();
	      }
	      else if ( lr_info.Store_should_be_in_succ() ) {
		lrbb->Set_need_store_succ();
	      }
	      else {
		lrbb->Set_need_store_bot();
	      }

	      // we're storing this value to its home
	      need_home = TRUE;
	    }
	  }
	}
      }
    }
  }


  // is there anything for us to do?
  if ( live_range->Do_anything() ) {
    live_range->Set_replace_anything();

    // does this live-range refer to the home location/value?
    // NOTE that for variables, this means it refers to a memory
    // location, and for constants, it generates that value into a preg
    if ( need_home ) {
      live_range->Set_need_home();
    }
  }

}

