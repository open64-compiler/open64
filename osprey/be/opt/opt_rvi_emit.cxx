//-*-c++-*-

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_rvi_emit.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rvi_emit.cxx,v $
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
#define opt_rvi_emit_CXX	"opt_rvi_emit.cxx"
static char *rcs_id = 	opt_rvi_emit_CXX"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "wn.h"
#include "region_util.h"
#include "cxx_memory.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_base.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_rvi.h"
#include "opt_rviwn.h"
#include "opt_rvi_emit.h"
#include "opt_alias_mgr.h"

// ====================================================================
// Process this block for the emitter
// ====================================================================

void
RVI_EMIT::Emit_bb( BB_NODE *bb )
{
  if ( Rvi()->Tracing() )
    fprintf( TFile, "RVI_EMIT::Emit_bb(BB:%d)\n", bb->Id() );

  if (bb->Kind() == BB_ENTRY && bb->Entrywn()) {
    const OPCODE fopc = WN_opcode(bb->Entrywn());
    if ( fopc == OPC_FUNC_ENTRY ) {
      Set_entry_wn(bb->Entrywn());
    }
    else if ( fopc == OPC_ALTENTRY ) {
      // this becomes a real statement in the block, so just add
      // it before any others
      bb->Prepend_wn_after_labels( bb->Entrywn() );
    }
    else if ( fopc == OPC_LABEL &&
	      WN_Label_Is_Handler_Begin(bb->Entrywn()) )
    {
      // this is an exception-handler label, and it becomes a real 
      // statement in the block, so just add it before any others
      bb->Prepend_wn_after_labels( bb->Entrywn() );
    }
#ifdef KEY
    else if ( fopc == OPC_LABEL &&
	      LABEL_target_of_goto_outer_block(WN_label_number(bb->Entrywn())))
    {
      // this is label being jumped to from a nested function, and it becomes a
      // real statement in the block, so just add it before any others
      bb->Prepend_wn_after_labels( bb->Entrywn() );
    }
#endif
  }

  WN *first = bb->Firststmt();
  WN *last  = bb->Laststmt();

  if ( first == NULL ) {
    Is_True( last == NULL,
      ("RVI_EMIT::Emit_bb: non-NULL last for BB:%d", bb->Id()) );
    Is_Trace( Rvi()->Tracing(),
	     (TFile,"RVI_EMIT::Emit_bb, empty BB%d\n",bb->Id()));
    return;
  }

  if ( Last_wn() != NULL ) {
    WN_prev(first) = Last_wn();
    WN_next(Last_wn()) = first;
    WN_next(last) = NULL;
    Set_last_wn( last );
  }
  else {
    // we've just begun, so these are the only statements
    Set_first_wn( first );
    Set_last_wn( last );
  }
}

// ====================================================================
// Process this block for the emitter, updating it with the annotations
// ====================================================================

void
RVI_EMIT::Emit_bb_annotations( BB_NODE *bb )
{
  if ( Rvi()->Tracing() ) {
    fprintf( TFile, "RVI_EMIT::Emit_bb_annotations(BB:%d)\n", bb->Id());
  }

  WN *first = bb->Firststmt();
  if ( first == NULL ) {
    // empty block, so why are there annotations?
    Is_True( bb->Rvi_anns() == NULL || bb->Rvi_anns()->Is_Empty(),
      ("RVI_EMIT::Emit_bb_annotations: annotations in empty bb:%d",
	bb->Id()) );

    return;
  }

  STMT_ITER stmt_iter;
  WN *wn;
  if ( ! Lda_only() ) {
    FOR_ALL_ELEM( wn, stmt_iter, Init(bb->Firststmt(),bb->Laststmt()) ){
      WN *new_kid;
      Emit_wn_annotations( bb, wn, &new_kid );
      Is_True( new_kid == NULL,
	("RVI_EMIT::Get_bb_annotations: statement can't be replaced") );
    }
  }
  else {
    // replace only LDAs
    FOR_ALL_ELEM( wn, stmt_iter, Init(bb->Firststmt(),bb->Laststmt()) ){
      WN *new_kid;
      Emit_lda_wn_annotations( bb, wn, &new_kid );
      Is_True( new_kid == NULL,
	("RVI_EMIT::Get_bb_annotations: statement can't be replaced") );
    }
  }


}

// ====================================================================
// Handle annotations for this block and this WN.  If the WN needs to
// be replaced, return a new WN to replace it with.
// ====================================================================

void
RVI_EMIT::Emit_wn_annotations( BB_NODE *bb, WN *wn, WN **new_wn ) const
{
  const OPCODE   opc = WN_opcode(wn);
  const OPERATOR opr = OPCODE_operator(opc);
  IDX_32 bitpos;
  RVI_ANN *annotation;

  // start off assuming we don't want to replace it
  *new_wn = NULL;

  // don't delve any more than necessary into "black boxes"
  if ( ! Rvi()->Black_box(opc) ) {
    // handle kids if necessary, deal with case of constant child
    for ( INT ikid = 0; ikid < WN_kid_count(wn); ikid++ ) {
      WN *new_kid;
#ifdef KEY // bug 12471: __builtin_expect's first kid must be constant
      if (WN_operator(wn) == OPR_INTRINSIC_OP &&
	  ((INTRINSIC) WN_intrinsic(wn)) == INTRN_EXPECT &&
	  ikid == 1)
	continue;
#endif
      Emit_wn_annotations( bb, WN_kid(wn,ikid), &new_kid );

      // see if this kid needs to be replaced
      if ( new_kid != NULL ) {
	WN_kid(wn,ikid) = new_kid;
      }
    }
  }

  switch ( opr ) {
    case OPR_CONST:
    case OPR_INTCONST:
    case OPR_LDA:
      // these constants are always candidates for our expected targets
      bitpos = Rvi()->Get_bitpos(wn);
      if ( bitpos != ILLEGAL_BP ) {
	if ( (annotation = bb->Rvi_anns()->Find( bitpos )) != NULL ) {
	  *new_wn = annotation->New_ldid( Alias_Mgr() );
	}
      }
      break;

    case OPR_LDID:
      if ( ST_class(WN_st(wn)) == CLASS_PREG ) {
	// we just don't care about pseudo-registers
	break;
      }
      bitpos = Rvi()->Get_bitpos( wn );
      if ( bitpos != ILLEGAL_BP ) {
	if ( (annotation = bb->Rvi_anns()->Find( bitpos )) != NULL ) {
	  WN_load_offset(wn) = annotation->Preg();
	  WN_st_idx(wn) = ST_st_idx(annotation->Preg_st());
	  WN_set_ty(wn, annotation->Preg_ty());
	  Alias_Mgr()->Gen_alias_id(wn, NULL);

	  /* CVTL-RELATED start (correctness) */
	  // is conversion necessary?
	  // Should be only if loads from memory are different than
	  // this load we're replacing.
	  OPCODE load_opc = WN_opcode(annotation->Rvi_node()->Loadwn());
	  if ( load_opc != opc ) {
	    *new_wn = Rvi()->Load_from_preg_cvtl( wn, load_opc );
	  /* CVTL-RELATED finish */
	  }
	}
      }
      break;

    case OPR_STID:
      if ( ST_class(WN_st(wn)) == CLASS_PREG ) {
	// we just don't care about pseudo-registers
	break;
      }
      bitpos = Rvi()->Get_bitpos( wn );
      if ( bitpos != ILLEGAL_BP ) {
	if ( (annotation = bb->Rvi_anns()->Find( bitpos )) != NULL ) {
	  // if this store has a chi, we need to save both to a preg
	  // and to memory.  So, we store the original saved value
	  // to a preg (with possible conversion), and then we use
	  // the original stid to store the preg.
	  if ( Rvi()->Get_chi_list(wn) != NULL ) {
	    Rvi()->Store_to_preg_and_mem(bb, wn,
					 annotation->Preg_st(),
	  				 annotation->Preg_ty(),
	  				 annotation->Preg() );
	  }
	  else {
	    /* CVTL-RELATED start (correctness) */
	    // storing to a preg may require a conversion
	    Rvi()->Store_to_preg_cvtl(wn, annotation->Preg_st(),
				      annotation->Preg_ty(),
				      annotation->Preg() );
	    /* CVTL-RELATED finish */
	  }
	}
      }
      break;

    default:
      break;
  }

}

// ====================================================================
// Handle annotations for this block and this WN.  If the WN needs to
// be replaced, return a new WN to replace it with.
// ====================================================================

void
RVI_EMIT::Emit_lda_wn_annotations( BB_NODE *bb, WN *wn, WN **new_wn ) const
{
  const OPCODE   opc = WN_opcode(wn);
  const OPERATOR opr = OPCODE_operator(opc);
  IDX_32 bitpos;
  RVI_ANN *annotation;

  // start off assuming we don't want to replace it
  *new_wn = NULL;

  // don't delve any more than necessary into "black boxes"
  if ( ! Rvi()->Black_box(opc) ) {
    // handle kids if necessary, deal with case of constant child
    for ( INT ikid = 0; ikid < WN_kid_count(wn); ikid++ ) {

      // Do not traverse LDAs that are not candidates for ivr2.
      //
      if (WN_operator(WN_kid(wn,ikid)) != OPR_LDA ||
	  Rvi()->Is_lda_candidate(wn, WN_kid(wn,ikid), ikid))
      {
        WN *new_kid;

	Emit_lda_wn_annotations( bb, WN_kid(wn,ikid), &new_kid );

	// see if this kid needs to be replaced
	if ( new_kid != NULL ) {
	  WN_kid(wn,ikid) = new_kid;
	}
      }
    }
  }

  if ( opr == OPR_LDA ) {
    // these constants are always candidates for our expected targets
    bitpos = Rvi()->Get_bitpos(wn);
    if ( bitpos != ILLEGAL_BP ) {
      if ( (annotation = bb->Rvi_anns()->Find( bitpos )) != NULL ) {
	*new_wn = annotation->New_ldid( Alias_Mgr() );
      }
    }
  }

}


// ====================================================================
// Handle the creation of regions
// ====================================================================

void
RVI_EMIT::Push_region( BB_NODE *start_region )
{
  BB_REGION *bb_region = start_region->Regioninfo();
  BB_NODE *end_region = bb_region->Region_end();

  RVIE_REGION *rvie_region = 
    CXX_NEW(RVIE_REGION(start_region, end_region, Last_wn()), 
	    Rvi()->Rvi_ppool());

  _region_stack.Push(rvie_region);
}

void
RVI_EMIT::Pop_region( void )
{
  RVIE_REGION *rvie_region = _region_stack.Pop();
  WN *prev_wn = rvie_region->Prev_wn();
  WN *last_region_wn = Last_wn();
  WN *first_region_wn= prev_wn ? WN_next(prev_wn) : First_wn();
  BB_REGION *bb_region = rvie_region->Region_start()->Regioninfo();

  // if it is an empty EH Guard region, put a comment in there
  if (RID_TYPE_guard(bb_region->Rid()) && first_region_wn == NULL &&
      (last_region_wn == NULL || last_region_wn == prev_wn)){
    first_region_wn = last_region_wn = WN_CreateComment("EH GUARD REGION");
  }

  if (first_region_wn == NULL &&
      (last_region_wn == NULL || last_region_wn == prev_wn))
  {
    // Nothing (empty stmt sequence) was emitted for this region.  The code
    // here is loosely based on EMITTER::Gen_wn() in opt_emit.cxx.
    //
    BB_NODE *rstart = bb_region->Region_start();

    // Leave the RVIE_REGION in a consistent state, and kill any pragmas
    // emitted before we knew the region was empty.
    //
    last_region_wn = NULL;
    rstart->Set_firststmt(NULL);
    rstart->Set_laststmt(NULL);
    
    // region is empty, don't emit it.
    //
    RID_Delete2(bb_region->Rid());
  }
  else
  {
    // create the region and the body
    WN *region_body = WN_CreateBlock();
    WN_first(region_body) = first_region_wn;
    WN_last(region_body)  = last_region_wn;
    WN *region_wn = WN_CreateRegion(REGION_type_to_kind(bb_region->Rid()),
				    region_body,
				    bb_region->Region_pragma_list(),
				    bb_region->Region_exit_list(),
				    RID_id(bb_region->Rid()),
				    bb_region->Ereg_supp());

     // no need to go through pragma block because RVI doesn't deal with
     // EH regions
  
     // update the wn list so this region node replaces the statements
     // it put into its body.
    if ( first_region_wn != NULL )
      WN_prev(first_region_wn) = NULL;
    Is_True( last_region_wn == NULL || WN_next(last_region_wn) == NULL,
	     ("RVI_EMIT::Pop_region: last_region_wn has non-null next") );

    if ( prev_wn != NULL )
      WN_next(prev_wn) = region_wn;
    WN_prev(region_wn) = prev_wn;
    Set_last_wn(region_wn);
    // do we become the first statement?
    if ( first_region_wn == First_wn() )
      Set_first_wn(region_wn);

    // update the RID
    REGION_emit(bb_region->Rid(), region_wn, _region_level,
		bb_region->Region_num_exits(), bb_region->Region_line_num());
  }
}

// ====================================================================
// Constructor for RVI_EMIT actually does all of the work
// ====================================================================

RVI_EMIT::RVI_EMIT(const RVI *rvi, BOOL lda_only, ALIAS_MANAGER *alias_mgr,
		   REGION_LEVEL region_level) : 
  _rvi(rvi), _lda_only(lda_only), _region_stack(rvi->Rvi_ppool())
{
  // init fields first
  _entry_wn = NULL;
  _first_wn = NULL;
  _last_wn  = NULL;
  _alias_mgr = alias_mgr;
  _region_level = region_level;

  // process all of the blocks, linking them up as necessary
  CFG_ITER cfg_iter(Rvi()->Cfg());
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {

    Is_Trace(Rvi()->Tracing(),(TFile,"----- BB%d -----\n",bb->Id()));

    // do we have a region to build up?
    if ( bb->Kind() == BB_REGIONSTART ) {
      Is_Trace(Rvi()->Tracing(),
	       (TFile,"RVI_EMIT, Push_region BB%d\n",bb->Id()));
      Push_region(bb);
    }

    if ( bb->Rvi_anns() != NULL ) {
      Emit_bb_annotations( bb );
    }

    Emit_bb( bb );

    // did we just finish a region?
    while ( _region_stack.Elements() > 0 && 
	    _region_stack.Top()->Region_end() == bb )
    {
      Is_Trace(Rvi()->Tracing(),(TFile,"RVI_EMIT, Pop_region\n"));
      Pop_region();
    }
  }

  // we should have cleared off the stack of regions
  Is_True( _region_stack.Elements() == 0,
    ("RVI_EMIT::RVI_EMIT: region stack not empty") );

  // this routine is used by the main emitter and rvi emitter to find
  // the first bb with code in it.
  BB_NODE *entry_bb = Rvi()->Cfg()->Find_entry_bb();
  if (entry_bb->Kind() == BB_ENTRY) {
    if ( Entry_wn() != NULL ) {
      WN *block = WN_CreateBlock();
      WN_first(block) = First_wn();
      WN_last(block) = Last_wn();
      WN_Set_Linenum( block, WN_Get_Linenum(Entry_wn()) );

      const OPCODE eopc = WN_opcode(Entry_wn());
      if ( eopc == OPC_FUNC_ENTRY ) {
	WN_func_body(Entry_wn()) = block;
	REGION_emit(Rvi()->Cfg()->Rid(), Entry_wn(), _region_level, 1, 0);
      } else {
	FmtAssert( FALSE, ("RVI_EMIT::RVI_EMIT: Can't handle entry %s",
			   OPCODE_name(eopc)) );
      }
    }
  } else {
    Is_True(entry_bb->Kind() == BB_REGIONSTART,
	    ("RVI_EMIT::RVI_EMIT, unknown entry kind %s",
	     entry_bb->Kind_name()));
    Set_entry_wn(First_wn());
    Is_True(REGION_consistency_check(Entry_wn()),
	    ("RVI_EMIT::RVI_EMIT, inconsistent region"));
  }

}
