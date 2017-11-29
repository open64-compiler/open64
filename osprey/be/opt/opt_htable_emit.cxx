/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_htable_emit.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_htable_emit.cxx,v $
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
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_htable_emit_CXX	"opt_htable_emit.cxx"
static char *rcs_id = 	opt_htable_emit_CXX"$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include "limits.h"
#include "defs.h"
#include "tracing.h"
#include "config_targ.h"
#include "wn.h"
#include "wn_simp.h"
#include "wn_util.h"

#include "opt_alias_mgr.h"
#include "opt_base.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_htable.h"
#include "opt_main.h"
#include "opt_mu_chi.h"
#include "opt_region_emit.h"	// PRUNE_BOUND
#include "opt_rvi.h"
#include "opt_sym.h"

#include "opt_emit_template.h" // this comes the last
#include "wssa_emitter.h"      // WHIRL SSA emitter
#include "pu_info.h"

class RVI;

class ML_WHIRL_EMITTER {
private:
  CFG             *_cfg;
  CODEMAP         *_htable;
  OPT_STAB        *_opt_stab;
  WN              *_opt_func;
  STMT_CONTAINER   _wn_list;
  MEM_POOL        *_loc_pool;
  MEM_POOL        *_mem_pool;
  ALIAS_MANAGER   *_alias_mgr;
  BOOL	           _do_rvi;       // do the bit vector RVI afterward
  RVI             *_rvi;
  WHIRL_SSA_EMITTER *_wssa_emitter ;    // WSSA emitter
  BOOL             _trace;
  WN_MAP           _wn_to_cr_map;	// map from wn -> cr/sr for WSSA emitter

  STACK<E_REGION*> _region_stack; // for MP regions

  ID_MAP<IDTYPE, INT32> _preg_renumbering_map;	// never initialized
						// in current
						// implementation.

  ML_WHIRL_EMITTER(const ML_WHIRL_EMITTER&);
  ML_WHIRL_EMITTER& operator = (const ML_WHIRL_EMITTER&);

  WN              *Emit(void);

  void             Gen_stmt(STMTREP*);

  void             Create_entry(BB_NODE *bb);
  STACK<E_REGION *> *Region_stack(void)       { return &_region_stack; }

  void             Pop_region(void);

public:
  ML_WHIRL_EMITTER(CFG           *cfg,
                   OPT_STAB      *opt_stab,
                   CODEMAP       *htable,
                   ALIAS_MANAGER *alias_mgr,
                   RVI           *rvi,
                   MEM_POOL      *lpool,
                   MEM_POOL      *gpool);
  ~ML_WHIRL_EMITTER(void)                            {}

  OPT_STAB        *Opt_stab(void) const       { return _opt_stab; }
  MEM_POOL        *Loc_pool(void) const       { return _loc_pool; }
  MEM_POOL        *Mem_pool(void) const       { return _mem_pool; }
  CFG             *Cfg(void) const            { return _cfg; }
  CODEMAP         *Htable(void) const         { return _htable; }
  WN              *Opt_func(void) const       { return _opt_func; }
  STMT_CONTAINER  *Wn_list(void)              { return &_wn_list; }
  ALIAS_MANAGER   *Alias_Mgr(void) const      { return _alias_mgr; }
  BOOL             Trace(void) const          { return _trace; }
  BOOL             For_preopt(void) const     { return FALSE; }
  WHIRL_SSA_EMITTER *WSSA_Emitter(void) const { return _wssa_emitter;}

  void             Gen_wn(BB_NODE *f,
                          BB_NODE *l);
  void             Insert_wn(WN *wn)          { _wn_list.Append(wn); }
                                              // insert a wn into the list
  BOOL             Verify(WN *wn);
  WN              *Build_loop_info(BB_NODE *);// build loop_info wn

  // a few routines used by the template in opt_emit_template.h
  ID_MAP<IDTYPE, INT32> &Preg_renumbering_map(void)
    { return _preg_renumbering_map; }	// not used, but must be defined.

  BOOL             Gen_lno_info(void)         { return TRUE; }
  BOOL	           Do_rvi(void) const 
			{ return WOPT_Enable_RVI1 && _do_rvi; }
  RVI             *Rvi(void) const            { return _rvi; };

  void             Connect_sr_wn(STMTREP* sr, WN *wn)
    { if (OPT_Enable_WHIRL_SSA) { WN_MAP_Set(_wn_to_cr_map, wn, sr); } }
  void             Set_region_entry_stmt(STMTREP*) {}
  STMTREP         *Region_entry_stmt(void)    { return NULL; }

  WN_MAP          *Wn_to_cr_map(void)        
    { return OPT_Enable_WHIRL_SSA ? &_wn_to_cr_map : NULL; }
};


// fix up the function entry, handles region entry also
void
ML_WHIRL_EMITTER::Create_entry(BB_NODE *bb)
{
  // this is similar to Create_block in preopt emitter
  WN *stmt = WN_CreateBlock();
  WN_first(stmt) = Wn_list()->Head();
  WN_last(stmt) = Wn_list()->Tail();

  WN *wn = bb->Entrywn();
  Is_True(wn != NULL,("ML_WHIRL_EMITTER::Create_entry, wn is NULL"));

  WN_Set_Linenum(stmt, WN_Get_Linenum(wn));
  _opt_func = wn;
  bb->Init_stmt(_opt_func);

  WN_func_body(_opt_func) = stmt;
  REGION_emit(Cfg()->Rid(), _opt_func, RL_MAINOPT, 1, 0);
}

void
ML_WHIRL_EMITTER::Gen_stmt(STMTREP *stmt)
{
  WN *wn = Gen_stmt_wn(stmt, Wn_list(), this);

  // OPR_EVAL, need loop info
}

BOOL
ML_WHIRL_EMITTER::Verify(WN *wn)
{
  return TRUE;
}

void
ML_WHIRL_EMITTER::Pop_region( void )
{
  E_REGION *e_region = _region_stack.Pop();
  WN *prev_wn = e_region->Prev_wn();
  WN *last_region_wn = _wn_list.Tail();
  WN *first_region_wn = prev_wn ? WN_next(prev_wn) : _wn_list.Head();
  BB_REGION *bb_region = e_region->Region_start()->Regioninfo();

  // Sometimes the region emitted is empty (EH Guard regions have
  // to be present even if they are empty). Fix up the pointers.
  if (!first_region_wn) {
    Is_True(prev_wn == _wn_list.Tail(),
	    ("ML_WHIRL_EMITTER::Pop_region, prev_wn mistake"));
    last_region_wn = NULL;
  }

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

  // go through region pragmas and convert aux_ids back to STs and offsets
  if (REGION_is_EH(region_wn))
    Opt_stab()->Convert_EH_pragmas(region_wn);

  // update the wn list so this region node replaces the statements
  // it put into its body.
  if ( first_region_wn != NULL )
    WN_prev(first_region_wn) = NULL;
  Is_True( last_region_wn == NULL || WN_next(last_region_wn) == NULL,
    ("MAIN_EMITTER::Pop_region: last_region_wn has non-null next") );

  if ( prev_wn != NULL )
    WN_next(prev_wn) = region_wn;
  WN_prev(region_wn) = prev_wn;
  _wn_list.Set_tail(region_wn);
  // do we become the first statement?
  if ( first_region_wn == _wn_list.Head() )
    _wn_list.Set_head(region_wn);

  // update the RID and level
  REGION_emit(bb_region->Rid(), region_wn, RL_MAINOPT, 
	      bb_region->Region_num_exits(), bb_region->Region_line_num());
}

//=====================================================================
// Try to build a LOOP_INFO wn for emitting.  If unsuccessful, returns
// a NULL wn.
//=====================================================================

WN *
ML_WHIRL_EMITTER::Build_loop_info( BB_NODE *label_bb )
{
  if ( label_bb->Label_loop_info() == NULL )
    return NULL;

  UINT16 est_trips = WN_loop_trip_est(label_bb->Label_loop_info());
  UINT16 depth = WN_loop_depth(label_bb->Label_loop_info());
  INT32 lflags = WN_loop_flag(label_bb->Label_loop_info());

  // this label_bb should be the first body_bb in the loop.  So, it's
  // prev block should be the dohead.
  BB_NODE *dohead_bb = label_bb->Prev();
  if ( dohead_bb == NULL || dohead_bb->Kind() != BB_DOHEAD ) {
    // this must not be a valid do-loop any more
    return NULL;
  }

  // make sure the cfg's loop information is still valid, and this
  // block is indeed the first body block.
  BB_LOOP *bb_loop = dohead_bb->Loop();
  if ( bb_loop == NULL || bb_loop->Body() != label_bb ) {
    return NULL;
  }

  BB_NODE *doend_bb = bb_loop->End();
  if ( doend_bb == NULL ||
      (doend_bb->Kind() != BB_DOEND &&
       doend_bb->Kind() != BB_WHILEEND && doend_bb->Kind() != BB_REPEATEND) ) {
    // this must not be a valid do-loop any more
    return NULL;
  }

  // make sure that the dohead dominates the ending condition
  if ( ! dohead_bb->Dominates_strictly( doend_bb ) ) {
    return NULL;
  }

  // if feedback info is avaiable, update the estimated trip count for CG.
  if ( Cfg()->Feedback() ) {
    FB_FREQ
      freq_trips = Cfg()->Feedback()->Get_node_freq_out( doend_bb->Id() );
    freq_trips /= Cfg()->Feedback()->Get_node_freq_out( dohead_bb->Id() );
    if ( freq_trips.Known() ) {
      INT32 trips = INT32( freq_trips.Value() + 0.5 );
      // est_trip is UINT16: check for overflow.
      est_trips = (trips <= USHRT_MAX) ? trips : USHRT_MAX;
    }
  }

  // Set the innermost flag
  if (bb_loop->Child() == NULL)
    lflags |= WN_LOOP_INNERMOST;
  else
    lflags &= ~WN_LOOP_INNERMOST;

  // this is the common point at which we can build the node, given
  // all available information
  WN *trip_count = NULL;
  WN *induction = NULL;

  // did LFTR replace the induction variable?
  if ( bb_loop->Iv_replacement() != NULL ) {
    induction = Gen_exp_wn(bb_loop->Iv_replacement(), this);
  }
  else {
    CODEREP *iv = bb_loop->Iv();
    if (iv != NULL && iv->Kind() == CK_VAR && 
	(Do_rvi() && iv->Bitpos() != ILLEGAL_BP ||
         !Do_rvi()))
    {
      MTYPE ivtype = TY_mtype(iv->Lod_ty());
#ifdef KEY // bug 5645
      if (ivtype == MTYPE_M)
	ivtype = iv->Dtyp();
#endif

      induction = WN_CreateLdid(Ldid_from_mtype(ivtype),
				iv->Offset(),
				Opt_stab()->St(iv->Aux_id()),
				iv->Lod_ty(),
				iv->Field_id());
      // WHIRL SSA
      if (OPT_Enable_WHIRL_SSA) {
        Connect_cr_wn(&_wn_to_cr_map, iv, induction);
      }
#ifdef TARG_LOONGSON
      // Need to change operator of induction to OPT_LDBITS when induction is BITs variable
      if (iv->Points_to(Opt_stab())->Bit_Size() != 0)
        WN_change_operator(induction, OPR_LDBITS);
#endif
      if (Do_rvi() && ST_class(WN_st(induction)) != CLASS_PREG) {
	Warn_todo("ML_WHIRL_EMITTER::Build_loop_info: do not adjust bitpos by 1" );
	Rvi()->Map_bitpos(induction, iv->Bitpos() + 1);
      }
      Alias_Mgr()->Gen_alias_id(induction, iv->Points_to(Opt_stab()));
    }
  }

  // must have an induction variable
  if ( induction == NULL ) {
    return NULL;
  }

  if (bb_loop->Trip_count_stmt() != NULL) {
    if (bb_loop->Wn_trip_count() != NULL) 
      trip_count = bb_loop->Wn_trip_count();
  } else {
    if (bb_loop->Trip_count_expr()) 
      trip_count = Gen_exp_wn(bb_loop->Trip_count_expr(), this);
  }

  WN *loop_info = WN_CreateLoopInfo( induction, trip_count, 
				     est_trips, depth, lflags );
  return loop_info;
}

// ====================================================================
//  Emit the whole program in WHIRL form
// ====================================================================
WN *
ML_WHIRL_EMITTER::Emit(void)
{
  if (Trace())
    fprintf(TFile,"%sML_WHIRL_EMITTER\n%s",DBar,DBar);

  // Reduce the region boundary sets to a minimum
  Is_True(Cfg()->Rid() != NULL, ("ML_WHIRL_EMITTER::Emit, NULL RID"));
  if (!RID_TYPE_func_entry(Cfg()->Rid())) {
    // the constructor does everything
    PRUNE_BOUND prune(Cfg(), Opt_stab());
    // the destructor is called here
  }

  // do we have valid loop-body information, which is necessary for
  // some of the checks
  Cfg()->Analyze_loops();

  BOOL saved_wn_simp_enable = WN_Simplifier_Enable(FALSE);

  // Fix 592011:  simplify CG's job.
  Cfg()->Delete_empty_BB();

  // WHIRL SSA
  if (OPT_Enable_WHIRL_SSA) {
    WSSA::WHIRL_SSA_MANAGER *wssa_mgr = NULL;
    if (PU_Info_state(Current_PU_Info, WT_SSA) == Subsect_InMem) {
      wssa_mgr = PU_Info_ssa_ptr(Current_PU_Info);
    }
    else {
      wssa_mgr = new WSSA::WHIRL_SSA_MANAGER(MEM_pu_nz_pool_ptr);
      Set_PU_Info_ssa_ptr(Current_PU_Info, wssa_mgr);
      Set_PU_Info_state(Current_PU_Info, WT_SSA, Subsect_InMem);
    }

    _wssa_emitter = CXX_NEW(WHIRL_SSA_EMITTER(wssa_mgr, _opt_stab, 
                                              _wn_to_cr_map), _mem_pool);
    // dump code rep and SSA tree before emitter.
    if (Get_Trace(TP_WSSA, TT_WSSA_EMT_INOUT)) {
      fprintf( TFile, "Coderep before WSSA Emitter\n");
      _htable->Print(TFile);
      _cfg->Print(TFile);
    }
    // internal emitter trace
    if (Get_Trace(TP_WSSA, TT_WSSA_EMT)) {
      _wssa_emitter->Set_trace(TRUE);
      fprintf( TFile, "Entering WSSA Emitter\n");
    }
    // time trace
    if (Get_Trace(TKIND_INFO, TINFO_TIME)) {
      _wssa_emitter->Set_trace_time(TRUE);
    }
    _wssa_emitter->WSSA_Convert_OPT_Symbol();
  }

  // Visit BBs in program order and preprocess then emit to WN
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  WN* last_stmt = NULL; // last genreated wn in wn_list
  FOR_ALL_ELEM(bb, cfg_iter, Init(Cfg())) {
    if (bb->Reached()) { // skip fake entry and exit BBs

      if (Trace())
	fprintf(TFile,"----- BB%d -----\n",bb->Id());

      // does this block mark the beginning of a region?
      if ( bb->Kind() == BB_REGIONSTART ) {
        WN *prev_wn = _wn_list.Tail();  // need to save the current tail
	Push_region(Region_stack(), bb, Loc_pool());
        E_REGION *e_region = _region_stack.Top();
        e_region->Set_prev_wn(prev_wn); // in new region's prev_wn
	Is_Trace(Trace(),(TFile,"Push_region(RGN %d), prev_wn = 0x%p\n",
		  RID_id(bb->Regioninfo()->Rid()),prev_wn));
      }

    // Add an assertion to make sure that the BB layout is right with respect
    // to eh_region. That is, if BB is inside a valid eh region (dce may 
    // remove some eh region and make rid invalid), its BB rid should 
    // match with the top eh_region's rid
    if (bb->Kind() != BB_REGIONEXIT && bb->EH_region() && bb->Rid() && 
        RID_is_valid(Cfg()->Rid(), bb->Rid()))
        Is_True(bb->Rid() == _region_stack.Top()->Region_start()->Rid(),
        ("ML_WHIRL_EMITTER::Emit: BB region id not match"));

      // generate alternate entry statement if necessary
      if ( bb->Kind() == BB_ENTRY && bb->Entrywn() &&
	   (WN_opcode(bb->Entrywn()) == OPC_ALTENTRY ||
	    (WN_opcode(bb->Entrywn()) == OPC_LABEL &&
	     (WN_Label_Is_Handler_Begin(bb->Entrywn())
#ifdef KEY
	      || LABEL_target_of_goto_outer_block(WN_label_number(bb->Entrywn()))
#endif
	     ))) )
      {
        Insert_wn( bb->Entrywn() );
	// Update Feedback?
      }

      // mark the beginning of this BB with a comment
      BOOL generate_comments = Get_Trace(TP_GLOBOPT, 0xffffffff);
      WN *comment_wn = NULL;
      if ( generate_comments ) {
	char str[120];
	sprintf(str,"BB%03d (%s) %40.40s", bb->Id(), bb->Kind_name(), SBar);
	comment_wn = WN_CreateComment(str);
	WN_Set_Linenum(comment_wn, bb->Linenum());

	// should we put the comment at the top of the block, or
	// immediately after the label?
	if ( bb->Label_stmtrep() == NULL ) {
	  Insert_wn( comment_wn );
	  comment_wn = NULL;
	}
      }

      STMTREP_ITER stmt_iter(bb->Stmtlist());
      STMTREP *tmp;
      BOOL bb_fisrt_stmt = TRUE;
      BOOL copy_phi = FALSE;
      WN *bb_prev_stmt = _wn_list.Tail();
      FOR_ALL_NODE(tmp, stmt_iter, Init()) {

        OPERATOR stmt_opr = OPCODE_operator(tmp->Op());

        // first check if we need add a label wn to record phi node.
        // 1. first stmt is a label
        // 2. not a label, has fall through phi node 
        //    the phi has only opnd, and the effect is change version.
        // 3. not a label, has valid phi(not fall through), error case.
        if (OPT_Enable_WHIRL_SSA && bb_fisrt_stmt) {
          copy_phi = FALSE;
          if (stmt_opr == OPR_LABEL) {
            copy_phi = TRUE;
          }
          else if (stmt_opr != OPR_OPT_CHI &&
                   bb->Has_valid_phi() &&
                   bb->Only_fall_through_phi()) {
            _wssa_emitter->WSSA_Copy_Fallthrough_PHI(bb, &_wn_list);
            copy_phi = FALSE;
          }
          else if (stmt_opr != OPR_OPT_CHI) {
            // assert no valid phi node on this bb, because this bb has no label.
            Is_True(!bb->Has_valid_phi(), ("unexpected case \n"));
          }
        }
        
        if ( tmp->Live_stmt() )
          Gen_stmt(tmp);

        // map WSSA phi info to stmt next to bb_prev_stmt.
        if (OPT_Enable_WHIRL_SSA && bb_fisrt_stmt && copy_phi) {
          WN* copy_wn;
          if (bb_prev_stmt == NULL) {
            copy_wn = _wn_list.Head();
          }
          else {
            copy_wn = WN_next(bb_prev_stmt);
          }
          _wssa_emitter->WSSA_Copy_PHI(bb, copy_wn);
          copy_phi = FALSE;
        }
        bb_fisrt_stmt = FALSE;

	// insert the comment if necessary after the label
	if ( stmt_opr == OPR_LABEL && comment_wn != NULL ) {
	  Insert_wn( comment_wn );
	  comment_wn = NULL;
	}
      }

      // emit phi for bb with single pred without stmt
      if (OPT_Enable_WHIRL_SSA &&
          bb->Stmtlist()->Is_Empty() &&
          bb->Has_valid_phi() &&
          bb->Only_fall_through_phi()) {
        _wssa_emitter->WSSA_Copy_Fallthrough_PHI(bb, &_wn_list);
      }

      bb->Set_wngend();

      // was this block the end of a region?
      while (_region_stack.Elements() > 0 &&
	     _region_stack.Top()->Region_end() == bb) {
	Is_Trace(Trace(),(TFile,"Pop_region(RGN %d), prev_wn = 0x%p\n",
	    RID_id(_region_stack.Top()->Region_start()->Regioninfo()->Rid()),
		  _region_stack.Top()->Prev_wn()));
        Pop_region();
      }
    }
  }

  // we should have cleared off the stack of regions
  Is_True( _region_stack.Elements() == 0,
    ("ML_WHIRL_EMITTER::Emit: region stack not empty") );

  // find the bb that is the entry point for this PU/region
  BB_NODE *entry_bb = Cfg()->Find_entry_bb();
  if (entry_bb->Kind() == BB_ENTRY) {
    // generate a function entry
    Create_entry(entry_bb);
  } else { // region
    Is_True(entry_bb->Kind() == BB_REGIONSTART,
	    ("ML_WHIRL_EMITTER::Emit, unknown entry kind %s",
	     entry_bb->Kind_name()));
    _opt_func = _wn_list.Head();
    Is_True(REGION_consistency_check(_opt_func),
	    ("ML_WHIRL_EMITTER::Emit, inconsistent region"));
  }

  // copy wssa mu/chi/version info
  if (OPT_Enable_WHIRL_SSA) {
    _wssa_emitter->WSSA_Build_MU_CHI_Version(_opt_func);
    if (_wssa_emitter->Get_trace()) {
      fprintf( TFile, "Exiting WSSA Emitter\n");
    }
    CXX_DELETE(_wssa_emitter, _mem_pool);
    if (Get_Trace(TP_WSSA, TT_WSSA_EMT_INOUT)) {
      fprintf(TFile, "WSSA sym table and WN tree after WSSA Emitter\n");
      WSSA::WHIRL_SSA_MANAGER *wssa_mgr = PU_Info_ssa_ptr(Current_PU_Info);
      wssa_mgr->Print_wst_table(TFile);
      fdump_tree(TFile, _opt_func);
    }
  }

  // update SYMTAB with number of labels
  Is_True(Get_Preg_Num(PREG_Table_Size(CURRENT_SYMTAB)) ==
	  Opt_stab()->Last_preg(),
	  ("ML_WHIRL_EMITTER::Emit, incorrect last preg number"));
  // For now, we assume that labels have been created during wopt
  // using the regular symtab mechanism, so we don't have to do
  // anything to tell the symtab how many labels we used.

  if (Trace())  {
    fprintf(TFile,"%sAfter ML_WHIRL_EMITTER\n%s",DBar,DBar);
    _alias_mgr->Print(_opt_func, TFile);
    Print_dep_graph(TFile);
  }

  /*PPP this shouldn't be necessary - the main emitter is destroying alias
    info for previously processed regions */
  REGION_update_alias_info(_opt_func,_alias_mgr);

  if (Opt_stab()->Phase() == MAINOPT_PHASE) {
    BOOL tr = Trace() || Get_Trace (TP_GLOBOPT, ALIAS_DUMP_FLAG);
    Opt_stab()->Cr_sr_annot_mgr()->
      Export_annot (_opt_func, _alias_mgr, TRUE, tr);
    WN_MEMOP_ANNOT_MGR::WN_mem_annot_mgr()->Set_active_mgr();  
  }

  Verify(_opt_func);

  Is_True(REGION_consistency_check(_opt_func),(""));

  WN_Simplifier_Enable(saved_wn_simp_enable);

  return _opt_func;
}

ML_WHIRL_EMITTER::ML_WHIRL_EMITTER(CFG           *cfg,
                                   OPT_STAB      *opt_stab,
                                   CODEMAP       *htable,
                                   ALIAS_MANAGER *alias_mgr,
                                   RVI           *rvi,
                                   MEM_POOL      *lpool,
                                   MEM_POOL      *gpool)
     :_cfg(cfg), _opt_stab(opt_stab), _htable(htable), _alias_mgr(alias_mgr),
     _rvi(rvi), _do_rvi(rvi->Do_rvi()), _mem_pool(gpool), _loc_pool(lpool),
     _region_stack(lpool), _preg_renumbering_map(128, 0, lpool, FALSE),
     _wssa_emitter(NULL)
{
  _trace = Get_Trace(TP_GLOBOPT, MAIN_EMIT_DUMP_FLAG);
  if (OPT_Enable_WHIRL_SSA)
    _wn_to_cr_map = WN_MAP_Create(Mem_pool());
  Emit();
  if (OPT_Enable_WHIRL_SSA)
    WN_MAP_Delete(_wn_to_cr_map);
}

WN*
COMP_UNIT::Emit_ML_WHIRL(RVI *rvi)
{
  // TODO: there are some question about Pop_region, not complete
  // TODO: LOOP_INFO is not done
  ML_WHIRL_EMITTER emitter(Cfg(),
                           Opt_stab(),
                           Htable(),
                           Alias_mgr(),
                           rvi,
                           Loc_pool(),
                           Mem_pool());
  return emitter.Opt_func();
}
