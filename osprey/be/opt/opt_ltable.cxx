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
// Module: opt_lpre.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_ltable.cxx,v $
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
// Memory Load PRE (LPRE), first phase of SSA based RVI
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#define opt_lpre_CXX  "opt_lpre.cxx"

#include "defs.h"
#include "cxx_memory.h"
#include "data_layout.h"
#include "w2op.h"
#include "wn_core.h"
#include "config.h"

#include "opt_base.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_estr.h"
#include "opt_etable.h"
#include "opt_efinalize.h"
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_main.h"
#include "opt_ssa.h"
#include "opt_sys.h"
#include "opt_util.h"
#include "tracing.h"
#include "opt_lftr2.h"
#include "config_targ.h"      // needed for Pointer_type


// ===================================================================
// Bottom-up traversal of CODEREP nodes in a statement to create
// occurrence list for LPRE
// ===================================================================
void
ETABLE::LPRE_bottom_up_stmt(STMTREP *stmt)
{
  const OPERATOR stmt_opr = OPCODE_operator(stmt->Op());

  Is_Trace(Tracing(),
	   (TFile, "----- stmt: %s -----\n", OPCODE_name(stmt->Op())));
  Is_Trace_cmd(Tracing(),stmt->Print(TFile));
  
  stmt->Set_stmt_id(Cfg()->Get_stmt_id());

  // for each statement see if they have a rhs and lhs and traverse
  // any expressions there
  CODEREP *rhs = stmt->Rhs();
  CODEREP *lhs = stmt->Lhs();

  if (OPCODE_is_fake(stmt->Op())) {
    for (INT32 i = 0; i < rhs->Kid_count(); i++) {
      New_temp_id();
      LPRE_bottom_up_cr(stmt, i, rhs->Opnd(i), FALSE, 0, rhs, i);
    }
  } else if (rhs != NULL) {
    New_temp_id();
    LPRE_bottom_up_cr(stmt, 0, rhs, FALSE, 0, lhs, 0);
  }
  if (stmt->Lhs()) {
    Is_Trace(Tracing(),(TFile,"Lhs\n"));
    New_temp_id();
    LPRE_bottom_up_cr(stmt, 1, stmt->Lhs(), OPCODE_is_store(stmt->Op()), 0, NULL, 0);
  }
}

// ===================================================================
// Bottom-up traversal of CODEREP nodes, assumes CODEREP is not NULL
// ===================================================================
void    
ETABLE::LPRE_bottom_up_cr(STMTREP *stmt, INT stmt_kid_num, CODEREP *cr,
			  BOOL is_store, UINT depth, CODEREP *parent, INT whichkid)
{
  Is_True(cr != NULL,("ETABLE::LPRE_bottom_up_cr, null CODEREP"));

  Is_Trace(Tracing(),(TFile, "----- cr -----\n"));
  Is_Trace_cmd(Tracing(),cr->Print(2,TFile));

  switch (cr->Kind()) {

    // three constant RVI cases
  case CK_CONST:  
    if ( LPRE_do_consts() && 
	 cr->Is_rvi_const_candidate(parent, whichkid, Opt_stab()) )
      Append_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE);
    break;
  case CK_RCONST: 
#if defined(TARG_X8664) || defined(TARG_LOONGSON) // bug 11268: need to keep the real and imag halves together 
    if (cr->Dtyp() == MTYPE_C4 && parent && parent->Kind() == CK_IVAR && 
	parent->Opr() == OPR_PARM)
      break;
#endif
    if ( LPRE_do_consts() ) {
#ifdef TARG_NVISA
      // on this targ, even floats can be immediates so we need that check
      if (!cr->Is_rvi_const_candidate(parent, whichkid, Opt_stab()))
        break;
#endif
      Append_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE);
    }
    break;
  case CK_LDA:    
    if ( LPRE_do_consts() && ! cr->Is_flag_set(CF_LDA_LABEL) &&
	 cr->Is_rvi_lda_candidate(parent, whichkid, Opt_stab()) )
      Append_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE);
    break;

  case CK_VAR:	    // variable terminal rvi candidates

#if defined(TARG_X8664) || defined(TARG_LOONGSON) // bug 11268: need to keep the real and imag halves together 
    if (cr->Dtyp() == MTYPE_C4 && parent && parent->Kind() == CK_IVAR && 
	parent->Opr() == OPR_PARM)
      break;
#endif
    if ( LPRE_do_loads() && 
	 !cr->Is_var_volatile() && 
         // screen out MLDID
         !Opt_stab()->Aux_stab_entry(cr->Aux_id())->No_register() &&
	 ST_class( Opt_stab()->St(cr->Aux_id()) ) != CLASS_PREG )
    {
#ifdef TARG_NVISA
      if (Tracing())
              Opt_stab()->St(cr->Aux_id())->Print(TFile);
      if (!WOPT_Enable_Const_Var_PRE 
	&& ST_is_const_var( Opt_stab()->St(cr->Aux_id())))
      {
  	    Is_Trace(Tracing(),(TFile, "don't do lpre on const_var symbol\n"));
      } else
#endif
      Insert_real_occurrence(cr, stmt, stmt_kid_num, depth, is_store, TRUE);
    }
    break;

  case CK_IVAR:	// non-terminal

#ifdef Is_True_On
    if (cr->Ilod_base() != NULL && cr->Istr_base() != NULL && 
	cr->Ilod_base() != cr->Istr_base())
      Warn_todo("CODEREP ilod_base != istr_base.");
    if (cr->Opr() == OPR_ILOADX)
      Warn_todo("ETABLE::Bottom_up_cr: Indexed load.");
#endif

    // Skip the DUMMY OPR_PARM
    if (cr->Opr() == OPR_PARM && (cr->Offset() & WN_PARM_DUMMY)) 
      break;

    LPRE_bottom_up_cr(stmt, stmt_kid_num, 
		      is_store ? cr->Istr_base() : cr->Ilod_base(),
		      FALSE, ( depth + 1 ), cr, 0);
    if ( cr->Opr() == OPR_MLOAD ) {
      LPRE_bottom_up_cr(stmt, stmt_kid_num, 
			cr->Mload_size() ? 
			cr->Mload_size() : cr->Mstore_size(),
			FALSE, ( depth + 1 ), cr, 1);
    }
    else if (cr->Opr() == OPR_ILOADX) {
      LPRE_bottom_up_cr(stmt, stmt_kid_num, cr->Index(),
			FALSE, ( depth + 1 ), cr, 1);
    }
    break;

  case CK_OP:		// non-terminal
    {
      Is_True(cr->Opr() != OPR_ARRAY, 
	      ("ETABLE::LPRE_bottom_up_cr: reach an OPR_ARRAY node,"
	       "this is a bug in lowering process"));
      
      //  A CR has its temp_id field initialized to -1, and might be
      //  set to the temp_id of statement processed. If
      //  ETABLE::New_temp_id() guarantees not to repeatedly use an
      //  id, CRs reached here the first time will have an id
      //  different from the Cur_temp_id.
      //  
      //   cr->Temp_id() != Cur_temp_id()               -->  first visit 
      //   cr->Temp_id() == Cur_temp_id() && !Is_lcse   -->  second visit
      //   cr->Temp_id() == Cur_temp_id() && Is_lcse    -->  third+ visit
      //  
      if (cr->Temp_id() == Cur_temp_id())
	if (cr->Is_lcse() && (cr->Max_depth() >= depth ||
			      cr->Max_depth() == 255))
	  break;   // return because this CR has been visited twice
	else
	  cr->Set_is_lcse();  // this is the second time
      else {
	cr->Set_temp_id(Cur_temp_id());  // this is the first time
	cr->Reset_is_lcse();
	cr->Set_max_depth(depth);
      }

      // remember maximum depth 
      if (cr->Max_depth() < depth)	
	cr->Set_max_depth( ( depth <= 255 ) ? depth : 255 );
	
      for (INT32 i=0; i<cr->Kid_count(); i++)	{ 
#ifdef KEY // bug 12471: __builtin_expect's first kid must be constant
	if (cr->Opr() == OPR_INTRINSIC_OP && cr->Intrinsic() == INTRN_EXPECT &&
	    i == 1)
	  continue;
#endif
	LPRE_bottom_up_cr(stmt, stmt_kid_num, cr->Opnd(i), FALSE, (depth+1), cr, i);
      }
      break;
    }
  default:		// illegal kind
    Is_True(0,("ETABLE::Bottom_up_cr(), unexpected kind 0x%x",cr->Kind()));
    break;
  }
}

void
EXP_WORKLST::Remove_if_saved_to_preg(ETABLE *etable)
{
  Is_True(etable->Pre_kind() == PK_LPRE,
	  ("EXP_WORKLST::Remove_if_saved_to_preg: not called from LPRE"));

  EXP_OCCURS *exp_occ, *prev_occ = NULL, *next_occ;
  EXP_OCCURS_ITER exp_occ_iter;
  exp_occ_iter.Init(Real_occurs().Head());
  for (exp_occ = exp_occ_iter.First(); !exp_occ_iter.Is_Empty();
       exp_occ = next_occ) {
    next_occ = exp_occ_iter.Next();
    STMTREP *stmt = exp_occ->Stmt();
    if (stmt->Is_saved_RHS() && Exp() == stmt->Rhs()) {
      Is_True(stmt->Lhs()->Kind() == CK_VAR && 
	      ST_class(etable->Opt_stab()->St(stmt->Lhs()->Aux_id())) == CLASS_PREG, 
	      ("EXP_WORKLST::Remove_if_saved_to_preg: Lhs of saved RHS is not a preg"));
      etable->Add_to_occ_freelist(exp_occ);
      Real_occurs().Remove(prev_occ, exp_occ);
    } else
      prev_occ = exp_occ;
  }
}

// perform the LPRE optimization
void
ETABLE::Perform_LPRE_optimization(void)
{

  // Even they are useless, the structure should be around to
  // share the code
  _str_red = CXX_NEW(STR_RED(Cfg(), Htable(), _etable_pool, _tracing),
		     _etable_pool);
  _lftr = CXX_NEW(LFTR(this, Htable(), Cfg(), LFTR_HASH_SIZE), _etable_pool);


  if (WOPT_Enable_Hoisting)
    _exp_hoisting = New_EXP_HOISTING(this, _etable_pool);

  // Phase numbers for various steps:
  INT32 Exc_sr_cands_phase  = 0;
  INT32 Finalize_phase      = 0;
  INT32 Phi_placement_phase = 0;
  INT32 Iterator_phase      = 0;
  INT32 Exp_renaming_phase  = 0;
  INT32 Downsafe_prop_phase = 0;
  INT32 Phi_liveness_phase  = 0;
  INT32 Avail_insert_phase  = 0;
  INT32 Ssa_min_phase       = 0;
  INT32 Hoist_phase         = 0;
  INT32 Save_reload_phase   = 0;

  // Initialize Dpo_vec before Dpo_Bb(dpo_id) in EXP_WORKLST::Insert_exp_phi
  Cfg()->Dpo_vec();

  Cfg()->Reset_stmt_id();

  SET_OPT_PHASE("LPRE: Build initial occurrence lists");

  // To get memory allocation tracing, use -ta25
  if (Get_Trace(TKIND_ALLOC, TP_WOPT1)) {
    MEM_Tracing_Enable();
  }

  Init_worklst(); // Step 1, all real occurances
  INT first_rank_e_num = _cur_e_num;
  INT bb_cnt = 0, edge_cnt = 0;

#ifdef Is_True_On
  CFG_ITER cfg_iter;
  BB_NODE *bb;
  FOR_ALL_ELEM(bb, cfg_iter, Init(Cfg())) {
    bb_cnt++;
    edge_cnt += bb->Succ()->Len();
  }
#endif

  EXP_WORKLST *cur_worklst;
# ifdef KEY
  INT32 cur_worklst_idx = 0;
# else
  INT32 cur_worklst_idx = -1;
# endif
  INT total_phi_count = 0;
  INT total_opt_ssa_count = 0;
  INT total_dense_ssa_count = 0;
  INT orig_coderep_id_cnt = Htable()->Coderep_id_cnt();
  EXP_WORKLST_ITER2 worklst_iter(Exp_worklst(), Urgent_worklst());
  BOOL use_feedback = WOPT_Enable_Feedback_LPRE;

  FOR_ALL_NODE(cur_worklst, worklst_iter, Init()) {

    cur_worklst_idx++;
    if (WOPT_Enable_Load_PRE_Limit != -1 &&
	cur_worklst_idx/*cur_worklst->E_num()*/ > WOPT_Enable_Load_PRE_Limit) {
      DevWarn("LPRE: skip LPRE for variable with v_num > %d\n",
	      WOPT_Enable_Load_PRE_Limit);
      break;
    }
    
    OPT_POOL_Push(Per_expr_pool(), -1);

    Is_Trace(Tracing(),
	     (TFile, "processing %dth expression\n", cur_worklst_idx));
    Is_Trace_cmd(Tracing(),cur_worklst->Exp()->Print(0,TFile));
    Is_Trace_cmd(Tracing(),cur_worklst->Print(TFile));


    // do stuff for each expression
    Per_worklst_cleanup(cur_worklst);
    _str_red->Perform_per_expr_cleanup();

    // exclude all expressions from being handled by Strength
    cur_worklst->Set_exclude_sr_cand();

#if defined(TARG_NVISA)
    if (cur_worklst->Has_unequal_sizes()) {
      Is_Trace(Tracing(), (TFile, "Skipping, different sizes of loads"));
      cur_worklst->Remove_occurs(this);
      continue;
    }
#endif

    // remove small constant occ if their lhs has been promoted to PREG
    if (cur_worklst->Exp()->Kind() == CK_CONST) {
      const INT64  scon_val = cur_worklst->Exp()->Const_val();
      const UINT64 ucon_val = scon_val;
      if ( (scon_val >= -32768 && scon_val <= 32767) ||
	  (ucon_val <= 0xffff) ) {
	cur_worklst->Remove_if_saved_to_preg(this);
      }
    }
    
    // Step 2 may cause bypass of later phases
    SET_OPT_REPEAT_PHASE(Phi_placement_phase, "LPRE: Var phi placement");
    if (cur_worklst->Real_occurs().Head() != NULL && 
	cur_worklst->Insert_exp_phi(this)) {

      // set up the iterator array for fast iteration (used by Steps 3, 5, 6)
      SET_OPT_REPEAT_PHASE(Iterator_phase, "LPRE: Iterator");
      EXP_ALL_OCCURS_ITER *exp_occ_iter = (EXP_ALL_OCCURS_ITER *)
	CXX_NEW(EXP_ALL_OCCURS_ITER(cur_worklst,this,Lftr()), Per_expr_pool());
      cur_worklst->Set_iterator(exp_occ_iter); // pointer to this iterator
      
      SET_OPT_REPEAT_PHASE(Exp_renaming_phase, "LPRE: Var version renaming");
      cur_worklst->Rename_expression(this); // Step 3

      if (use_feedback) {
	
	cur_worklst->Save_flags();

	SET_OPT_REPEAT_PHASE(Downsafe_prop_phase, "LPRE: Var anticipation");
	cur_worklst->Propagate_downsafe(this);

	OPT_POOL_Push(Etable_local_pool(), -1);

	SET_OPT_REPEAT_PHASE(Avail_insert_phase,
			     "LPRE: Var availability/insertion");
	cur_worklst->Compute_forward_attributes(this, TRUE /* compute partial avail */);
	
	SET_OPT_REPEAT_PHASE(Save_reload_phase, "LPRE: Compute var save/reload");
	cur_worklst->Compute_save_delete(Htable(), this, NULL);

	cur_worklst->Estimate_cost(this, PK_LPRE);

	// Free the def-use space.
	OPT_POOL_Pop(Etable_local_pool(), -1);
      }

      // Path with and without feedback
	
      // Step 4 may cause the bypass of later phases
      SET_OPT_REPEAT_PHASE(Downsafe_prop_phase, "LPRE: Var anticipation");
      if (cur_worklst->Propagate_downsafe(this)) {

	OPT_POOL_Push(Etable_local_pool(), -1);

	SET_OPT_REPEAT_PHASE(Avail_insert_phase,
			     "LPRE: Var availability/insertion");
	cur_worklst->Compute_forward_attributes(this);

	if (!WOPT_Enable_SSA_Minimization) {
	  // Free the def-use space.
	  OPT_POOL_Pop(Etable_local_pool(), -1);
	}
	
	if (WOPT_Enable_Hoisting) {
	  SET_OPT_REPEAT_PHASE(Hoist_phase, "LPRE: Var hoisting.");
	  cur_worklst->Hoist_expression(Exp_hoisting());
	}

	SET_OPT_REPEAT_PHASE(Save_reload_phase, "LPRE: Compute var save/reload");
	cur_worklst->Compute_save_delete(Htable(), this, NULL);

	if (WOPT_Enable_SSA_Minimization) {
	  SET_OPT_REPEAT_PHASE(Ssa_min_phase, "LPRE: SSA minimization");
	  cur_worklst->Minimize_temp_ssa(this, Tracing());
	  // Free the def-use space.
	  OPT_POOL_Pop(Etable_local_pool(), -1);
	}

	SET_OPT_REPEAT_PHASE(Finalize_phase, "LPRE: CO Var save/reload");
	cur_worklst->Generate_save_reload(this); // Step 6

#ifdef Is_True_On      
	cur_worklst->Verify();
#endif
      } // bypass by step 4

      Opt_tlog( "LPRE", 0,
	       "%d-th expression: Inserts=%d, Saves=%d, Reloads=%d, Temp phis=%d, Hoisted=%d",
	       cur_worklst_idx,
	       cur_worklst->Insert_count(),
	       cur_worklst->Save_count(),
	       cur_worklst->Reload_count(),
	       cur_worklst->Temp_phi_count(),
	       cur_worklst->Hoisted_count());

#ifdef Is_True_On
      // This tlog is under Is_True_On because it could be expensive to collect them,
      // so we don't want it in the production compiler.
      //
      Opt_tlog( "LPRE", 0,
	       "%d-th variable: Phis=%d(%d%%), Optimistic_SSA=%d(%d%%), Dense_SSA=%d(%d%%)",
	       cur_worklst_idx,
	       cur_worklst->Phi_count(),
	       cur_worklst->Phi_count() * 100 / bb_cnt,
	       cur_worklst->Optimistic_ssa_count(), 
	       cur_worklst->Optimistic_ssa_count() * 100 / (bb_cnt+edge_cnt),
	       cur_worklst->Dense_ssa_count(),
	       cur_worklst->Dense_ssa_count() * 100 / (bb_cnt+edge_cnt));

      total_phi_count += cur_worklst->Phi_count();
      total_opt_ssa_count += cur_worklst->Optimistic_ssa_count();
      total_dense_ssa_count += cur_worklst->Dense_ssa_count();
#endif

      exp_occ_iter->Remove_iter();
      cur_worklst->Set_iterator(NULL);

    } // Step 2 bypass

    cur_worklst->Remove_occurs(this);
    OPT_POOL_Pop(Per_expr_pool(), -1);

    if (WOPT_Enable_Verify >= 4) {
      Is_True(_comp_unit->Verify_CODEMAP(), ("CODEMAP corrupted."));
      _comp_unit->Verify_version();
    }
  }

#ifdef Is_True_On
  Opt_tlog("LPRE_PU_info", 0,
	   "CFG nodes=%d, edges=%d, nodes+edges=%d, init_enum=%d, final_enum=%d",
	   bb_cnt,
	   edge_cnt,
	   bb_cnt + edge_cnt,
	   first_rank_e_num,
	   _cur_e_num);

  if (_cur_e_num > 0) {
    total_phi_count /= _cur_e_num;
    total_opt_ssa_count /= _cur_e_num;
    total_dense_ssa_count /= _cur_e_num;
  }

  Opt_tlog("LPRE_PU_info", 0,
	   "PU Average:  Phis=%d(%d%%), Optimistic_SSA=%d(%d%%), Dense_SSA=%d(%d%%)",
	   total_phi_count,
	   total_phi_count * 100 / bb_cnt,
	   total_opt_ssa_count,
	   total_opt_ssa_count * 100 / (bb_cnt+edge_cnt),
	   total_dense_ssa_count,
	   total_dense_ssa_count * 100 / (bb_cnt+edge_cnt));
#endif

  if (Tracing()) {
    fprintf(TFile, "%sAfter LPRE\n%s", DBar, DBar);
    fprintf(TFile, "Statistics (all expressions): Insert Count %d, "
	    "Save Count %d, Reload Count %d, Temp Phi Count %d, Hoisted Count %d\n",
	    _num_inserted_saves, _num_cse_saves, _num_cse_reloads, 
	    _num_temp_phis, _num_hoisted);
    fprintf(TFile, "Coderep Statistics (entire PU): previous count: %d new count: %d\n", 
	    orig_coderep_id_cnt, Htable()->Coderep_id_cnt());
    fprintf(TFile, "     Expr nodes changed to temps without rehashing: %d\n",
	    _num_temp_owners);
    Cfg()->Print(TFile);
    if (Get_Trace(TKIND_ALLOC, TP_WOPT1)) {
      MEM_Trace();
    }
  }

  CXX_DELETE(_str_red,_etable_pool);
  CXX_DELETE(_lftr,_etable_pool);
  if (WOPT_Enable_Hoisting)
    Delete_EXP_HOISTING(_exp_hoisting);

}  

void
COMP_UNIT::Do_load_pre(BOOL do_consts, BOOL do_loads)
{
  MEM_POOL etable_pool, phi_pool, etable_local_pool;

  OPT_POOL_Initialize(&etable_pool, "etable pool", FALSE, LPRE_DUMP_FLAG);
  OPT_POOL_Initialize(&phi_pool, "phi pool", FALSE, LPRE_DUMP_FLAG);
  OPT_POOL_Initialize(&etable_local_pool, "etable local pool", FALSE, LPRE_DUMP_FLAG);
  OPT_POOL_Push(&etable_pool, LPRE_DUMP_FLAG);
  OPT_POOL_Push(&phi_pool, LPRE_DUMP_FLAG);
  OPT_POOL_Push(&etable_local_pool, LPRE_DUMP_FLAG);

  {
    ETABLE etable(Cfg(), Opt_stab(), Htable(), Arule(), 10,
                  &etable_pool, &phi_pool, &etable_local_pool, this, PK_LPRE);
    etable.LPRE_set_do_consts(do_consts);
    etable.LPRE_set_do_loads(do_loads);
    etable.Perform_LPRE_optimization();
  } // the etable destructor is called here

  OPT_POOL_Pop(&etable_local_pool, LPRE_DUMP_FLAG);
  OPT_POOL_Pop(&phi_pool, LPRE_DUMP_FLAG);
  OPT_POOL_Pop(&etable_pool, LPRE_DUMP_FLAG);
  OPT_POOL_Delete(&etable_local_pool, LPRE_DUMP_FLAG);
  OPT_POOL_Delete(&phi_pool, LPRE_DUMP_FLAG);
  OPT_POOL_Delete(&etable_pool, LPRE_DUMP_FLAG);
}

// =====================================================================
// Same as the RVI::Is_const_candidate but work with CODEREP
// =====================================================================
BOOL 
CODEREP::Is_rvi_const_candidate(const CODEREP *parent, INT whichkid, const OPT_STAB *opt_stab) const
{
  if ( parent == NULL ) return FALSE;

#ifdef TARG_NVISA
  // On this TARG, float constants(CK_RCONST) can be immediates
  // Using separate function to avoid any confusion since
  // Can_Be_Immediate has implicit assumption of being integer const
  if (Kind() == CK_RCONST) {
    if (MTYPE_is_float(Dtyp())) {
      return !Can_Be_Float_Immediate(Dtyp());
    }
    return false;
  }
#endif

  Is_True( (Kind() == CK_CONST) && MTYPE_is_integral(Dtyp()),
	  ("CODEREP::Is_rvi_const_candidate: not an integer const") );

  const MTYPE    par_dtyp = parent->Dtyp();
  const INT64    con_val  = Const_val();
  const CODEKIND par_ck   = parent->Kind();
  const OPERATOR par_opr  = (par_ck == CK_VAR) ? OPR_STID : parent->Opr();

#if defined(TARG_MIPS) || defined(TARG_LOONGSON)
  if (con_val == 0)
    return FALSE;
#endif

  // following check is applicable only to coderep due to the fact that,
  // in coderep, a constant cannot have MTYPE_B; we don't want to put MTYPE_B
  // constants in preg
  switch (par_opr) {
    case OPR_EQ: case OPR_NE:
    case OPR_LNOT:
    case OPR_CVT: 
      if (parent->Dsctyp() == MTYPE_B)
        return FALSE;
      break;
    case OPR_SELECT:
      if (parent->Dsctyp() == MTYPE_B && whichkid == 0)
        return FALSE;
      break;
    default: ;
  }

  ST * const     stid_st  =   (par_ck == CK_VAR) 
			    ? opt_stab->St(parent->Aux_id()) : NULL;

  return !Can_Be_Immediate(par_opr, con_val, par_dtyp, whichkid, stid_st);
}

// =====================================================================
// Same as the RVI::Is_lda_candidate but work with CODEREP
// Note it returns FALSE if the Offset is 0, because second phase RVI
// can do it better after lowering.
// =====================================================================
BOOL
CODEREP::Is_rvi_lda_candidate( const CODEREP *parent, INT whichkid, const OPT_STAB *opt_stab ) const
{
#if defined(TARG_X8664) || defined(TARG_IA32)
  return FALSE;
#else
  if (parent == NULL) return FALSE;

  Is_True( Kind() == CK_LDA,
	  ("CODEREP::Is_rvi_lda_candidate: not a LDA") );

  const OPERATOR par_opr 
    = (parent->Kind() == CK_VAR) ? OPR_STID : parent->Opr();
  const WN_OFFSET lda_offset = Offset();

  if (lda_offset == 0) return FALSE; // leave it to second phase RVI

  ST *lda_st = opt_stab->St(Lda_aux_id());

  switch ( par_opr ) {
  case OPR_ILOAD:
  case OPR_ILOADX:
  case OPR_MLOAD:
  case OPR_ILDBITS:
    // include OPR_ISTORE, OPR_MSTORE
    // if this is a small lda, do not RVI it
    if ( this == parent->Ilod_base() || this == parent->Istr_base() ) {
      return !Uses_Small_Offset( lda_st, lda_offset );
    }
    return TRUE;

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
    if ( whichkid == (parent->Kid_count() - 1) ) {
      if ( Enable_GOT_Call_Conversion ) {
	// handle even unsafe conversions
	return TRUE;
      }
      else {
	// handle only safe conversions, which means we'll handle
	// address of functions that aren't preemptible
	if ( (Gen_PIC_Call_Shared || Gen_PIC_Shared) && 
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
    // is this in a store to a dedicated register, which usually
    // means in preparation for a call, or return value, so just 
    // let us generate the stid/load-immediate in place if it fits
    //
    // Removed Preg_Is_Dedicated(WN_offset(parent)) obsolete PV#461064
    //
    if ( ST_class(opt_stab->St(parent->Aux_id())) == CLASS_PREG &&
	Uses_Small_Offset( lda_st, lda_offset ) )
      {
	// it can be an inlined lda
	return FALSE;
      }
    return TRUE;

  default:
    return TRUE;
  }
#endif
}
