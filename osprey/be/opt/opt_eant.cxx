/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_eant.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_eant.cxx,v $
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
// Several anticipatibility related steps.
//
//
// SSA/PRE Step 4 (Not down safe) and 
//                AGGCM (speculate safe ops out of while-do loop)
//
// (AGGCM) If an phi result is not fully anticipated, but the phi node
// has an edge that is a back edge, and the operation is proper for
// speculation, mark the phi result to be fully anticipated.
//
// Propagate not down safe (not fully anticipated) phi-assignments
// upward.  I.e., if x5 = phi(x3,x4) is not down-safe, if x3 is also a
// phi-assignment and there is no real occurrence of x3, then the x3
// phi-assignment is also not down-safe. Note this happens only when
// x5 and x3 are not marked as down-safe by AGGCM.
//
//
// SSA/PRE Step 4.5 (dead phi elimination):
//
// Determine live phi (phi result is partially anticipated).  If a phi
// result is not partially anticipated, it is dead.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "opt_cfg.h"
#include "opt_etable.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "tracing.h"
#include "config_targ.h"
#include "config_wopt.h"
#include "opt_mu_chi.h"

// ===================================================================
// A BB_NODE has an incoming backedge if it has a predecessor that it
// dominates. The sense is reversed if 'loop_tail' is TRUE.
// The CODEREP is invariant related to the backedge if:
// (1) no phi function exists for its operand at the BB, or
// (2) the operand from each backedge is the same as the LHS of the phi
// ===================================================================

static BOOL
BB_has_backedge(BB_NODE *bb, BOOL loop_tail = FALSE)
{
  Is_True(bb, ("BB_has_backedge: Null BB pointer"));

  BB_NODE            *bb_pred;
  BB_NODE            *bb_succ;
  BB_LIST_ITER        bb_iter;
  
  if (loop_tail) {
    FOR_ALL_ELEM(bb_succ, bb_iter, Init(bb->Succ())) 
      if (bb_succ->Dominates(bb)) return TRUE;
  } else {
    FOR_ALL_ELEM(bb_pred, bb_iter, Init(bb->Pred())) 
      if (bb->Dominates(bb_pred)) return TRUE;
  }
  
  return FALSE;
}

//=======================================================================
// BB_NODE is inside a loop which has a large compgoto or deep branch
// chain. Here, "large" compgoto means the number of entries is larger
// than  WOPT_Enable_Aggressive_CM_Switch_Threshold (default is 20).
// "deep" branch chains means the number of branchs is larger than
// WOPT_Enable_Aggressive_CM_Branch_Threshold (default is 10). It is not 
// beneficial to do speculative code motion in this case because
// register pressure can be high.
//=======================================================================
static BOOL 
BB_in_complex_loop(BB_NODE *bb, std::map<IDTYPE, BOOL> * complex_loop_map, CFG * cfg)
{

// curtail aggressive code motion if target arch has small 
// register file (e.g., x86)
#if defined(TARG_X8664)

    if (WOPT_Enable_Aggressive_Code_Motion >= 2) return FALSE;

    std::map<IDTYPE, BOOL>::iterator bb_it = complex_loop_map->find(bb->Id());
    if (bb_it != complex_loop_map->end())
        return (*bb_it).second;

    BB_NODE *bb_loop;
    BB_NODE_SET_ITER bb_iter;
    int branch = 0;
    
    const BB_LOOP *loop = cfg->Find_innermost_loop_contains(bb);
    if (loop && loop->True_body_set()->MemberP(bb)) {
        FOR_ALL_ELEM(bb_loop, bb_iter, Init(loop->True_body_set())) {
            std::map<IDTYPE, BOOL>::iterator bb_loop_it;
            bb_loop_it = complex_loop_map->find(bb_loop->Id());
            if (bb_loop_it != complex_loop_map->end() && (*bb_loop_it).second ||
                bb_loop->Branch_stmtrep() != NULL &&
                bb_loop->Branch_stmtrep()->Opr() == OPR_COMPGOTO &&
                bb_loop->Switchentries() > WOPT_Enable_Aggressive_CM_Switch_Threshold) {
                (*complex_loop_map)[bb->Id()] = TRUE;
                return TRUE;
            }
            if (bb_loop->Branch_stmtrep() != NULL &&
                (bb_loop->Branch_stmtrep()->Opr() == OPR_TRUEBR || 
                bb_loop->Branch_stmtrep()->Opr() == OPR_FALSEBR))
                branch ++;
       }
    }

    if (branch > WOPT_Enable_Aggressive_CM_Branch_Threshold) {
        (*complex_loop_map)[bb->Id()] = TRUE;
        return TRUE;
    }
        
    (*complex_loop_map)[bb->Id()] = FALSE;

#endif

    return FALSE;   

}

static BOOL
Exp_phi_is_invariant(ETABLE *etable, EXP_PHI *exp_phi, BOOL forward_pre)
{
  
  Is_True(exp_phi, ("Exp_phi_is_invariant: exp_phi is NULL"));
  BOOL is_used_in_loop = FALSE;
  BOOL is_invariant;
  OPT_POOL_Push(etable->Etable_local_pool(), -1);
  {
    STACK<EXP_PHI*> phi_stack(etable->Etable_local_pool());
    is_invariant = exp_phi->Identity_assignment(is_used_in_loop, exp_phi, forward_pre, phi_stack);
    EXP_PHI *t;
    while (!phi_stack.Is_Empty()) {
      t = phi_stack.Pop();
      t->Reset_I_A_Seen();
    }
  }
  OPT_POOL_Pop(etable->Etable_local_pool(), -1);
  return ( is_used_in_loop && is_invariant );
}

static BOOL
BB_has_backedge_and_is_invariant(BB_NODE *bb, CODEREP *cr, ETABLE *etable)
{
  Is_True(bb,("Has_backedge: Null BB pointer"));

  BB_NODE            *bb_pred;
  BB_LIST_ITER        bb_iter;
  INT                 which_pred = 0;
  BOOL                has_back_edge = FALSE;

  FOR_ALL_ELEM(bb_pred, bb_iter, Init(bb->Pred())) {
    if (bb->Dominates(bb_pred)) {
      has_back_edge = TRUE;
      if (cr->Kind() != CK_IVAR) {
	for (INT i = 0; i < cr->Kid_count(); ++i) {
	  CODEREP *kid = cr->Opnd(i);
	  if ((kid->Kind() == CK_IVAR) && (kid->Opr() == OPR_PARM)) 
	    kid = kid->Ilod_base();
	  if (kid->Kind() == CK_VAR) {
	    const PHI_NODE *var_phi = etable->Lookup_var_phi(bb, kid->Aux_id());
	    if (var_phi && (var_phi->RESULT() != var_phi->OPND(which_pred)))
	      return FALSE;
	  } 
	}
      } 
      else {
	const CODEREP *vsym = cr->Get_ivar_vsym();
	if (vsym) {
	  const PHI_NODE *var_phi = etable->Lookup_var_phi(bb,vsym->Aux_id());
	  if (var_phi && 
	      (vsym->Is_flag_set(CF_IS_ZERO_VERSION) ||
	       (var_phi->RESULT() != var_phi->OPND(which_pred))))
	    return FALSE;
	}
      }
    }
    which_pred++;
  }

  return has_back_edge;
}

static 
BOOL Is_loop_entry_bb(CFG *cfg, BB_NODE *bb)
{
  Is_True(bb != NULL, ("Is_loop_entry_bb, NULL bb"));

  const BB_LOOP *loop = cfg->Find_innermost_loop_contains(bb);
  return ( loop != NULL && loop->Body() == bb );
}

// ===================================================================
// SSA/PRE Step 4:
// If a phi-assignment is not an AGGCM candidate and not down-safe.
// Propagate the not down-safe (not anticipated) phi-assignments upward
// to the non AGGCM candidates.
// I.e., if x5 = phi(x3,x4) is not down-safe, if x3 is also a phi-assignment
// and there is no real occurrence of x3, then the x3 phi-assignment is
// also not down-safe.
// ===================================================================
BOOL 
EXP_WORKLST::Propagate_downsafe(ETABLE *etable)
{ 
  BOOL ret_value = TRUE;
  INT  length = 0;
  BOOL is_spre = (etable->Pre_kind() == PK_SPRE);
  BOOL is_lpre = (etable->Pre_kind() == PK_LPRE);

  BOOL aggcm_candidate = (WOPT_Enable_Aggressive_Code_Motion &&
			  Exp()->Can_be_speculated(etable->Opt_stab()));

  FmtAssert(etable,("EXP_WORKLST::Propagate_downsafe: Etable is NULL"));

  Is_Trace(etable->Tracing(),
	   (TFile, "====== Before EXP_WORKLST::Propagate_downsafe ======\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));
	       
  OPT_POOL_Push(etable->Etable_local_pool(), -1);
  {
    EXP_OCCURS         *phi_occ;
    EXP_OCCURS_ITER     phi_iter;
    STACK<EXP_OCCURS*>  worklist(etable->Etable_local_pool());
  
    FOR_ALL_NODE (phi_occ, phi_iter, Init(Phi_occurs().Head())) {
      length = length + 1;
      Is_True(phi_occ->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR,
	      ("EXP_WORKLST::Propagate_downsafe: Not a phi occurrence"));
      EXP_PHI *phi = phi_occ->Exp_phi();

      if (phi->Not_down_safe()) {
	if (!aggcm_candidate ||
	    phi->Dead_phi_region() ||
	    (is_spre && phi_occ->Occurrence() == NULL) ||
	    !BB_has_backedge(phi->Bb(),is_spre) ||
	    !Exp_phi_is_invariant(etable, phi, !is_spre) ||
        BB_in_complex_loop(phi->Bb(), etable->Complex_loop_map(), etable->Cfg())) {
	  worklist.Push(phi_occ);
	} 
	else {
	  Is_True(phi_occ->Occurrence(), ("AGGCM: occurrence of phi is NULL"));
	  phi->Reset_not_down_safe();
	  Is_Trace(etable->Tracing(),(TFile, "------ Phi node has backedge ------\n"));
	  Is_Trace_cmd(etable->Tracing(), phi_occ->Print(TFile));
	  Is_Trace(etable->Tracing(), (TFile, "------ AGGCM phi ------\n"));
	  Is_Trace_cmd(etable->Tracing(), phi->Print(TFile));

	  // A very yucky hack of an idea to avoid bug 665964.
	  if (WOPT_Enable_Speculation_Defeats_LFTR) {
	    Set_no_lftr();
	  }
	}
      }
    }
    
    for (INT i = 0; i < worklist.Elements(); ++i) {
      phi_occ = worklist.Bottom_nth(i);
      EXP_PHI    *cur_phi = phi_occ->Exp_phi();
      Is_True(cur_phi->Not_down_safe(),
	      ("EXP_WORKLST::Propagate_downsafe: down safe phi occurrence"));
      for (INT j = 0; j < cur_phi->Opnd_count(); ++j) {
	if (!cur_phi->Has_real_occ(j)) {            
	  EXP_OCCURS *def_occ = cur_phi->Opnd(j);
	  EXP_PHI    *def_phi = (def_occ ? def_occ->Exp_phi() : NULL);

	  if (def_occ && def_occ->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR && 
	      !def_phi->Not_down_safe()) {
	    if (!aggcm_candidate || 
		def_phi->Dead_phi_region() ||
		cur_phi->Dead_phi_region() ||
		(is_spre && def_occ->Occurrence() == NULL) ||
		!BB_has_backedge(def_phi->Bb(),is_spre) ||
		!Exp_phi_is_invariant(etable, def_phi, !is_spre) ||
         BB_in_complex_loop(def_phi->Bb(),  etable->Complex_loop_map(), etable->Cfg())) {
	      def_phi->Set_not_down_safe();
	      if (cur_phi->Dead_phi_region())
	        def_phi->Set_dead_phi_region();
	      worklist.Push(def_occ);
	    } 
	    else { 
	      Is_True(def_occ->Occurrence(), ("AGGCM: occurrence of phi is NULL"));
	      Is_Trace(etable->Tracing(),(TFile, "------ Phi node has backedge ------\n"));
	      Is_Trace_cmd(etable->Tracing(), def_occ->Print(TFile));
	      Is_Trace(etable->Tracing(),(TFile, "------ AGGCM phi ------\n"));
	      Is_Trace_cmd(etable->Tracing(), def_phi->Print(TFile));

	      // A very yucky hack of an idea to avoid bug 665964.
	      if (WOPT_Enable_Speculation_Defeats_LFTR) {
		Set_no_lftr();
	      }
	    }
	  }
	}
      } // for (j)
    } // for (i)

    // Every phi is not down safe and there is only one real occurrence
    if (length == worklist.Elements() &&
	Real_occurs().Head() == Real_occurs().Tail() &&
	!Real_occurs().Head()->Mult_real()) ret_value = FALSE;
  }
  OPT_POOL_Pop(etable->Etable_local_pool(), -1);

  Is_Trace(etable->Tracing(),
	   (TFile, "====== After EXP_WORKLST::Propagate_downsafe ======\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));

  return ret_value;
}

// ===================================================================
// SSA/PRE Step 4.5:
// Determine live phi (phi result is partially anticipated).
// ===================================================================
void
EXP_WORKLST::Determine_live_phi(ETABLE *etable)
{
  FmtAssert(etable,("EXP_WORKLST::Determine_live_phi: Etable is NULL"));
  Is_Trace(etable->Tracing(),
	   (TFile, "====== Before EXP_WORKLST::Determine_live_phi ======\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));

  OPT_POOL_Push(etable->Etable_local_pool(), -1);
  {
    EXP_OCCURS         *phi_occ;
    EXP_OCCURS_ITER     phi_iter;
    STACK<EXP_OCCURS*>  worklist(etable->Etable_local_pool());
  
    FOR_ALL_NODE (phi_occ, phi_iter, Init(Phi_occurs().Head())) {
      Is_True(phi_occ->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR,
	      ("EXP_WORKLST::Determine_live_phi: Not a phi occurrence"));
      if (phi_occ->Occurrence() != NULL) {
	phi_occ->Exp_phi()->Set_live();
	worklist.Push(phi_occ);
      }
    }
  
    for (INT i = 0; i < worklist.Elements(); ++i) {
      phi_occ = worklist.Bottom_nth(i);
      EXP_PHI    *cur_phi = phi_occ->Exp_phi();
      Is_True(cur_phi->Is_live(),
	      ("EXP_WORKLST::Determine_live_phi: dead phi occurrence"));

      for (INT j = 0; j < cur_phi->Opnd_count(); ++j) {
	EXP_OCCURS * def_occ = cur_phi->Opnd(j);
	if (def_occ &&
	    def_occ->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR &&
	    !def_occ->Exp_phi()->Is_live()) {
	  def_occ->Exp_phi()->Set_live();
	  worklist.Push(def_occ);
	}
      }
    }
  }
  OPT_POOL_Pop(etable->Etable_local_pool(), -1);

  Is_Trace(etable->Tracing(),
	   (TFile, "====== After EXP_WORKLST::Determine_live_phi ======\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));

}

