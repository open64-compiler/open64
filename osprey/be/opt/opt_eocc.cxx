//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_eocc.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_eocc.cxx,v $
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
// PRE Step 2:
// Scan CFG and build real occurrence and phi occurrence lists for 
// expressions that are the lowerest in the expression trees (i.e.,
// (one operator and two terminals operands).
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "erglob.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "opt_etable.h"
#include "opt_estr.h"
#include "opt_lftr2.h"
#include "opt_config.h"
#include "tracing.h"
#include "config_targ.h"
#include "opt_mu_chi.h"
#include "opt_cvtl_rule.h"

// ====================================================================
//
// EOCC class - This unexported class holds the (minimal) data and 
// all of the methods for placing phi's for expression occurrences and
// building the occurrence lists in Etable. 
//
// ====================================================================

class EOCC {
private:
  CFG     *_cfg;	       	// handle on the control-flow graph
  ETABLE  *_etable;		// pointer to the itable
  BOOL     _tracing;		// is general tracing enabled?
  
  CFG     *Cfg(void) const	{ return _cfg; }
  ETABLE  *Etable(void) const	{ return _etable; }
  
public:
  EOCC(ETABLE *etab, CFG *cfg):
  		_cfg(cfg), _etable(etab)
    { 
      _tracing = etab->Tracing();
    }
  
  ~EOCC(void) {}
  
  // Collect real occurrences to etable
  void Collect_real_occurrences( void );

  BOOL Tracing(void) const	              { return _tracing; }
}; // end of class EOCC

// ====================================================================
// Determine if 'this' OP is a SSAPRE candidate (must be CK_OP kind)
// ====================================================================
BOOL
CODEREP::Exp_has_e_num(void) const
{ 
  Is_True(Kind() == CK_OP, ("CODEREP::Oper_has_e_num: illegal kind."));

  if (OPERATOR_is_volatile(Opr())) return FALSE;
  
  switch (Opr()) {
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
  case OPR_ASM_INPUT:
  case OPR_ASM_STMT:
    return FALSE;
  case OPR_INTRINSIC_OP:
    return WOPT_Enable_Move_Intrinsicop;
#ifdef KEY
  case OPR_PURE_CALL_OP:
#endif
  case OPR_CVT:
//    return !Cvt_is_nop();
  case OPR_CVTL:
//    return !Cvtl_is_nop();
  default:
    return TRUE;
  }
}


// ====================================================================
// Determine if 'this' is a non_volatile_terminal
// ====================================================================
BOOL 
CODEREP::Is_non_volatile_terminal(OPT_STAB *opt_stab) const
{
  switch (Kind()) {
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    return TRUE;
  case CK_VAR: 
    {
      if (Is_var_volatile()) return FALSE;

      ST *s = opt_stab->St(Aux_id());
      if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(Offset())))
	return FALSE;
      return TRUE;
    }
  case CK_IVAR:
    if (Is_ivar_volatile()) return FALSE;
    if (Opr() == OPR_PARM && Ivar_mu_node() == NULL && WOPT_Enable_Move_Intrinsicop)
      return Ilod_base()->Is_non_volatile_terminal(opt_stab);
  default:
    return FALSE;
  }
}

void
EOCC::Collect_real_occurrences( void )
{
  BB_NODE *bb;
  DPOBB_ITER dpo_iter(Cfg());
  FOR_ALL_ELEM (bb, dpo_iter, Init()) {

    Is_Trace(Tracing(),(TFile,
	      "====== EOCC::Collect_real_occurrences, BB%d (dpo %d) ======\n",
	      bb->Id(),dpo_iter.Cur()));

    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {

      /* CVTL-RELATED start (performance) */
      // Simplification of CVT/CVTL
      if ( Etable()->Pre_kind() == PK_EPRE ) {
	OPERATOR stmt_opr = stmt->Opr();
	if (OPERATOR_is_scalar_store (stmt_opr) &&
	    ST_class(Etable()->Opt_stab()->St(stmt->Lhs()->Aux_id())) 
	    != CLASS_PREG || OPERATOR_is_scalar_istore (stmt_opr) ||
	    stmt_opr == OPR_ISTOREX) {
	  CODEREP *rhs_cr = stmt->Rhs();
	  CODEREP *lhs = stmt->Lhs();
	  if (WOPT_Enable_Cvt_Folding && rhs_cr->Kind() == CK_OP && 
#ifdef KEY // bug 11797
	      ! MTYPE_is_vector(rhs_cr->Dsctyp()) &&
	      ! MTYPE_is_vector(rhs_cr->Dtyp()) &&
#endif
	      (rhs_cr->Opr() == OPR_CVT && MTYPE_is_integral(rhs_cr->Dsctyp()) 
	       || rhs_cr->Opr() == OPR_CVTL) &&
#ifdef TARG_X8664
	      !MTYPE_is_vector(lhs->Dsctyp()) &&
#endif
	      MTYPE_is_integral(rhs_cr->Dtyp()) && 
	      MTYPE_is_integral(lhs->Dsctyp()) 
	      ) {
	    MTYPE actual_type;
	    if (rhs_cr->Opr() == OPR_CVTL)
	      actual_type = Actual_cvtl_type(rhs_cr->Op(),rhs_cr->Offset());
	    else if (MTYPE_size_min(rhs_cr->Dsctyp()) <= MTYPE_size_min(rhs_cr->Dtyp()))
	      actual_type = rhs_cr->Dsctyp();
	    else
	      actual_type = rhs_cr->Dtyp();
	    if (MTYPE_size_min(lhs->Dsctyp()) <= MTYPE_size_min(actual_type)) {
	      stmt->Set_rhs(rhs_cr->Get_opnd(0));
	    }
	  }
	}
      }
      /* CVTL-RELATED finish */

      stmt->Reset_RHS_saved();
      stmt->Reset_saved_RHS();
      if ( Etable()->Pre_kind() == PK_EPRE )
	Etable()->Bottom_up_stmt(stmt);
      else {
	Is_True(Etable()->Pre_kind() == PK_LPRE, 
		("EOCC::Collect_real_occurrences: illegal kind"));
	Etable()->LPRE_bottom_up_stmt(stmt);
	if (OPERATOR_is_scalar_store (stmt->Opr())) // set flag for SPRE to use
	  Etable()->Opt_stab()->Aux_stab_entry(stmt->Lhs()->Aux_id())->Set_has_store_in_PU();
      }
    }

    if (bb->Kind() == BB_EXIT && bb != Cfg()->Fake_exit_bb())
      Etable()->Append_exit_occurrence(bb);

    PHI_LIST *phi_list = bb->Phi_list();
    if (phi_list) {
      PHI_NODE *phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
        Is_Trace(Tracing(),(TFile,
	    "====== EOCC::Collect_real_occurrences, Enter Phi ======\n"));
        Is_Trace_cmd(Tracing(),phi->Print(phi_list->In_degree(),TFile));

	// determine if this phi has any operands defined by IV updates
	if (Etable()->Pre_kind() == PK_EPRE && 
	    WOPT_Enable_New_SR &&
	    phi->Live() &&
	    // TODO: NULL in the following line should eventually change
	    Etable()->Str_red()->Determine_iv_update_phi(phi, NULL))
	{
	  Is_Trace(Tracing(),(TFile,"-phi has iv update opnd-\n"));
	}
      }
    }
  }

  if (Etable()->Lftr()->Lftr_on()) {
    // some comparison occurrence nodes may have been created when their
    // lftr_var is not in Lftr_non_candidates(), but later the lftr_var is
    // set in Lftr_non_candidates(); these comp occurrences nodes now need
    // to be deleted
    Etable()->Lftr()->Remove_lftr_non_candidates();
  }
}


void
ETABLE::Init_worklst(void)
{
  EOCC eocc( this, Cfg() );

  // statements and phi results that are CRs
  // collect list of comparisons for LFTR
  eocc.Collect_real_occurrences();

  Is_Trace(Tracing(),(TFile, "====== ETABLE::Init_worklst ======\n"));
}

static BOOL
Is_same_location(const CODEREP *cr1, const CODEREP *cr2)
{
  CODEREP *base1 = cr1->Ilod_base() ? cr1->Ilod_base() : cr1->Istr_base();
  CODEREP *base2 = cr2->Ilod_base() ? cr2->Ilod_base() : cr2->Istr_base();
  return ( ( base1 == base2 ) && ( cr1->Offset() == cr2->Offset() ) );
}
  
// =====================================================================
// Set the 'stmt''s BB_NODE to be a member of the 'defnodes' set.
// If there is a PHI_NODE for the variable in the BB_NODE, also set
// the Defstmt's of the phi-operands to the defnodes.
// Recursive procedure to make Fred happy :-)
// =====================================================================
static void
Set_defphi_recursive(PHI_NODE    *phi_node,
		     BB_NODE_SET &phi_list,
		     BOOL         tracing,
		     ETABLE      *etable,
		     BOOL         handle_sr)
{
  Is_True(phi_node, ("Set_defphi_recursive: Null PHI_NODE parameter"));

  if (phi_node->Live()) {
    BB_NODE *bb = phi_node->Bb();
    if (!phi_list.MemberP(bb->Dom_dfs_id())) {
      Is_Trace(tracing,
	       (TFile, "------ Enter Phi-node (by variable): %d ------\n",
		bb->Id()));
      phi_list.Union1D(bb->Dom_dfs_id());
      CODEREP *cr;
      PHI_NODE *defphi;
      for (INT i = 0; i < phi_node->Size(); ++i) {
	cr = phi_node->OPND(i);
	if (cr) {
	  if ( handle_sr && cr->Kind() == CK_VAR ) {
	    while (!cr->Is_flag_set((CR_FLAG) (CF_DEF_BY_PHI|
					       CF_DEF_BY_CHI|
					       CF_IS_ZERO_VERSION))) {
	      // check if this variable is defined by an IV-update
	      STMTREP *cr_def = cr->Defstmt();
	      if (etable->Str_red()->Determine_iv_update(cr_def, NULL)) {
		CODEREP *new_cr, *dummy_incr;
		BOOL dummy_is_add;
		if (etable->Str_red()->
		    Find_iv_and_incr(cr_def, &new_cr, &dummy_incr,
				     &dummy_is_add)) {
		  cr = new_cr;
		  continue;
		}
	      }
	      // if make it here, we're done
	      break;
	    }
	  }
	  // end of inserted code for SR
	      
	  if (cr->Is_flag_set(CF_DEF_BY_PHI) && 
	      ((defphi = cr->Defphi() ) != NULL)) {
	    Set_defphi_recursive(defphi, phi_list, tracing, etable,
				 handle_sr);
	  }
	}
      }
    }
  }
}

// =====================================================================
// Same as above but for indirect variable EXCEPT that
// backtracking can not stop at a CHI node unless the defstmt defines
// exactly the same variable
// =====================================================================
static void
Set_indirect_defphi_recursive(CODEREP *vsym, BB_NODE_SET &phi_list, BOOL tracing, CODEREP *ivar)
{
  Is_True(vsym, ("Set_indirect defphi_recursive: new kid in town, missing road sign (Null vsym pointer)"));

  PHI_NODE *phi_node;
  BB_NODE *bb;
  CODEREP *vsym_cur;

  if ( vsym->Is_flag_set(CF_DEF_BY_PHI) ) {
    if ( ( ( phi_node = vsym->Defphi() ) != NULL ) && phi_node->Live() ) {
      bb = phi_node->Bb();
      if ( ! phi_list.MemberP( bb->Dom_dfs_id() ) ) {
	Is_Trace(tracing, (TFile, "------Enter Phi-node (by ivar): %d ------\n", bb->Id()));
	phi_list.Union1D( bb->Dom_dfs_id() );
	for (INT i = 0; i < phi_node->Size(); ++i) {
	  if ( ( vsym_cur = phi_node->OPND(i) ) != NULL ) 
	    Set_indirect_defphi_recursive(vsym_cur, phi_list, tracing, ivar);
	}
      }
    }
  }
  else if ( vsym->Is_flag_set(CF_DEF_BY_CHI) ) {
    CHI_NODE *chi_node = vsym->Defchi();
    if ( chi_node && chi_node->Live() && 
	( ( vsym_cur = chi_node->OPND() ) != NULL ) ) {
      if ( vsym_cur->Is_flag_set(CF_IS_ZERO_VERSION) ) {
	Set_indirect_defphi_recursive(vsym_cur, phi_list, tracing, ivar);
      } else {
	STMTREP *defstmt = vsym->Defstmt();
	if ( ( defstmt == NULL ) || ! OPERATOR_is_store(defstmt->Opr()) )
	  Set_indirect_defphi_recursive(vsym_cur, phi_list, tracing, ivar);
	else if ( (defstmt->Lhs()->Kind() != CK_IVAR) ||
		 ! Is_same_location(defstmt->Lhs(), ivar) )
	  Set_indirect_defphi_recursive(vsym_cur, phi_list, tracing, ivar);
      }
    }
  }
}

// ======================================================================
// SSA/PRE Step 2 private helper function
// Find the phi list due to phi of the Opnd(kid_number) variable.
// ======================================================================
void
EXP_WORKLST::Generate_variable_phi_list(INT kid_number,
					BB_NODE_SET &var_phi_list, 
					BOOL tracing, 
					ETABLE *etable)
{
  EXP_OCCURS        *exp_occ;
  EXP_OCCURS_ITER    exp_occ_iter;
  CODEREP           *cr;
  PHI_NODE          *phi_node;

  // Collect all the def-nodes of kid 'kid_number'
  FOR_ALL_NODE (exp_occ, exp_occ_iter, Init(Real_occurs().Head())) {
    cr = exp_occ->Occurrence();
    Is_True(cr,("EXP_WORKLST::Generate_variable_phi_list: real occurrence with null CODEREP"));
    if ( Pre_kind() == PK_EPRE ) cr = cr->Opnd(kid_number);
    Is_True(cr,("EXP_WORKLST::Generate_variable_phi_list: real occurrence with null CODEREP as kid"));
    if ((cr->Kind()==CK_IVAR) && (cr->Opr()==OPR_PARM)) cr = cr->Ilod_base();
    Is_True(cr,("EXP_WORKLST::Generate_variable_phi_list: real occurrence with null CODEREP as kid"));    

    // beginning of inserted code for SR
    if (!Exclude_sr_cand() && cr->Kind() == CK_VAR) {
      while (!cr->Is_flag_set(CF_DEF_BY_PHI) &&
	     !cr->Is_flag_set(CF_DEF_BY_CHI)) {
	// check if this variable is defined by an IV-update
	STMTREP *cr_def = cr->Defstmt();
	if ( etable->Str_red()->Determine_iv_update(cr_def, NULL) ) {
	  CODEREP *new_cr, *dummy_incr;
	  BOOL dummy_is_add;
	  if (etable->Str_red()->
	      Find_iv_and_incr(cr_def, &new_cr, &dummy_incr, &dummy_is_add)) {
	    cr = new_cr;
	    continue;
	  }
	}

	// if make it here, we're done
	break;
      }
    }
    // end of inserted code for SR

    if (cr->Is_flag_set(CF_DEF_BY_PHI) && 
	((phi_node = cr->Defphi()) != NULL)) 
      Set_defphi_recursive(phi_node, var_phi_list, tracing, etable,
			   !Exclude_sr_cand() );
  }
}

// ======================================================================
// Almost the same as the above, except looking for the indirect variable
// and handle the address expression.
// ======================================================================
void
EXP_WORKLST::Generate_ivariable_phi_list_addr(BB_NODE_SET &var_phi_list, 
					      BOOL tracing,
					      ETABLE *etable)
{
  Is_True(Exp()->Kind()==CK_IVAR, ("EXP_WORKLST::Generate_indirect_variable_phi_list: Exp is not a CK_IVAR"));

  EXP_OCCURS        *exp_occ;
  EXP_OCCURS_ITER    exp_occ_iter;
  CODEREP           *cr;
  PHI_NODE          *phi_node;

  // Collect all the def-nodes of the base variable
  FOR_ALL_NODE (exp_occ, exp_occ_iter, Init(Real_occurs().Head())) {
    cr = exp_occ->Occurrence();
    Is_True(cr,("EXP_WORKLST::Generate_variable_phi_list: indirect occurrence with null CODEREP"));
    cr = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    Is_True(cr,("EXP_WORKLST::Generate_variable_phi_list: indirect occurrence with null CODEREP as Ilod_base() and Istr_base()"));
    if (cr->Is_flag_set(CF_DEF_BY_PHI) && ( (phi_node=cr->Defphi()) != NULL )) 
      Set_defphi_recursive(phi_node, var_phi_list, tracing, etable, FALSE);
  }
}

// ======================================================================    
// Same as above but handle the definition of indirect variables through
// chi's of the vsym.
// WARNING: this and the above cannot be combined into one because the
// 'var_phi_list' is also used to remember whether it has visited a
// phi node before.
// ======================================================================
void
EXP_WORKLST::Generate_ivariable_phi_list_vsym(BB_NODE_SET &var_phi_list, 
					      BOOL tracing)
{
  Is_True(Exp()->Kind()==CK_IVAR, ("EXP_WORKLST::Generate_indirect_variable_phi_list: Exp is not a CK_IVAR"));
  
  EXP_OCCURS        *exp_occ;
  EXP_OCCURS_ITER    exp_occ_iter;
  CODEREP           *cr;
  CODEREP           *vsym;

  // Collect all the def-nodes of the CODEREP through its 'vsym'
  FOR_ALL_NODE (exp_occ, exp_occ_iter, Init(Real_occurs().Head())) {
    if (!exp_occ->Occurs_as_lvalue()) {
      cr = exp_occ->Occurrence();
      if (cr->Ivar_mu_node()) {
	vsym = cr->Ivar_mu_node()->OPND();
	if (vsym) Set_indirect_defphi_recursive(vsym, var_phi_list, tracing, cr);
      }
    }
  }
}
  
// ======================================================================
// SSA/PRE Step 2
// Insert exp's phi.
// Build expression phi list and phi's predecessor list, both in DPO
// ======================================================================
BOOL
EXP_WORKLST::Insert_exp_phi(ETABLE *etable)
{
  FmtAssert(etable,("EXP_WORKLST::Insert_exp_phi: Etable is NULL"));

  Is_Trace(etable->Tracing(),
	   (TFile, "====== ETABLE::Insert_phi_occurrences ======\n"));
  Is_Trace(etable->Tracing(),
	   (TFile, "The real occurrence list is:\n"));
  Is_Trace_cmd(etable->Tracing(),Real_occurs().Print(TFile));

  // short circuit the process if there is only one real occurrence with
  // no phi function at all.
  if (Real_occurs().Head() == Real_occurs().Tail() &&
      !Real_occurs().Head()->Mult_real() &&
      Real_occurs().Head()->Bb()->Dom_frontier()->EmptyP()) return FALSE;

  BB_NODE_SET        &phi_list = etable->Phi_work_set();
  BB_NODE_SET        &var_phi_list = etable->Var_phi_set();
  BB_NODE_SET_ITER    df_iter;
  EXP_OCCURS         *exp_occ;
  EXP_OCCURS_ITER     exp_occ_iter;
  BB_NODE            *bb_orig;
  BB_NODE            *bb_phi;
  MEM_POOL           *local_pool = etable->Etable_local_pool();
  MEM_POOL           *per_exp_pool = etable->Per_expr_pool();

  OPT_POOL_Push(local_pool, -1);
  {

    var_phi_list.ClearD();
    if ( Pre_kind() == PK_EPRE ) {
      // Collect phi lists from all the kids
      if (Exp()->Kind() != CK_IVAR) {
	for (INT i = 0; i < Exp()->Kid_count(); ++i) {
	  phi_list.ClearD();
	  Generate_variable_phi_list(i, phi_list, etable->Tracing(), etable);
	  var_phi_list.UnionD(&phi_list);
	}
      } else if (Exp()->Ilod_base() || Exp()->Istr_base()) {
	phi_list.ClearD();
	Generate_ivariable_phi_list_addr(phi_list, etable->Tracing(), etable);
	var_phi_list.UnionD(&phi_list);
	phi_list.ClearD();
	Generate_ivariable_phi_list_vsym(phi_list, etable->Tracing());
	var_phi_list.UnionD(&phi_list);
      } else {
	FmtAssert(FALSE,("EXP_WORKLST::Insert_exp_phi: unhandled PRE candidate"));
	Exp()->Print(0,TFile);
      }
    } else if (Pre_kind() == PK_LPRE && Exp()->Kind() == CK_VAR) {
      phi_list.ClearD();
      Generate_variable_phi_list(-1, phi_list, etable->Tracing(), etable);
      var_phi_list.UnionD(&phi_list);
    }
  
    BB_LIST_CONTAINER   worklist;

    phi_list.ClearD();

    FOR_ALL_NODE (exp_occ, exp_occ_iter, Init(Real_occurs().Head())) {
      bb_orig = exp_occ->Bb();
      FOR_ALL_ELEM (bb_phi, df_iter, Init(bb_orig->Dom_frontier())) {
	if (!phi_list.MemberP(bb_phi->Dom_dfs_id())) {
	  Is_Trace(etable->Tracing(),
		   (TFile, "------ Enter Phi-node: %d ------\n", 
		    bb_phi->Id()));
	  phi_list.Union1D(bb_phi->Dom_dfs_id());
	  worklist.Append(bb_phi, local_pool);
	}
      }
    }
  
    IDTYPE dpo_id;
    FOR_ALL_NODE (dpo_id, df_iter, Init(&var_phi_list)) {
      bb_phi = etable->Cfg()->Dpo_Bb(dpo_id);
      if (!phi_list.MemberP(dpo_id)) {
	worklist.Append(bb_phi, local_pool);
      }
    }

    phi_list.UnionD(&var_phi_list);

    while (bb_orig = worklist.Remove_head(local_pool)) {
      FOR_ALL_ELEM (bb_phi, df_iter, Init(bb_orig->Dom_frontier())) {
	if (!phi_list.MemberP(bb_phi->Dom_dfs_id())) {
	  Is_Trace(etable->Tracing(),
		   (TFile, "------ Enter Phi-node: %d ------\n",
		    bb_phi->Id()));
	  phi_list.Union1D(bb_phi->Dom_dfs_id());
	  worklist.Append(bb_phi, local_pool);
	}
      }
    }

    // At this point, phi_list contains all the BBs that needs Phi-insertion.
    // Hence, let's insert the Phi nodes
    FOR_ALL_NODE (dpo_id, df_iter, Init(&phi_list)) {
      bb_phi = etable->Cfg()->Dpo_Bb(dpo_id);
      Is_Trace(etable->Tracing(),
	       (TFile, "------ Generate EXP_PHI: %d ------\n", bb_phi->Id()));
      EXP_PHI *new_phi = CXX_NEW(EXP_PHI(E_num(), 
					 bb_phi->Phi_list()->In_degree(), 
					 bb_phi, per_exp_pool), per_exp_pool);
      EXP_OCCURS *new_occ = etable->Append_phi_occurrence(Exp(), new_phi, this);
      etable->Set_exp_phi_bb(bb_phi, new_occ);
      bb_phi->Set_exp_phi(new_phi);
#if defined(TARG_SL)
    // set such phi not down-safe for that has predecessor from different parallel region 
      if(bb_phi->SL2_para_region() && bb_phi->Preds_or_succs_from_different_region()) 
        new_phi->Set_not_down_safe();
#endif
    }

    // Build the phi-predecessor list
    BB_LIST_ITER bb_iter;
    phi_list.ClearD();
    FOR_ALL_NODE (exp_occ, exp_occ_iter, Init(Phi_occurs().Head())) {
      bb_orig = exp_occ->Bb();
      FOR_ALL_ELEM (bb_phi, bb_iter, Init(bb_orig->Pred())) {
	Is_Trace(etable->Tracing(),
	 (TFile, "------ Enter Phi-pred %d ------\n", bb_phi->Id()));
	phi_list.Union1D(bb_phi->Dom_dfs_id());
      }
    }
  
    FOR_ALL_NODE (dpo_id, df_iter, Init(&phi_list)) {
      bb_orig = etable->Cfg()->Dpo_Bb(dpo_id);
      Is_Trace(etable->Tracing(),
	(TFile,"------ Generate Phi-pred occurrence %d ------\n", 
	 bb_orig->Id()));
      EXP_OCCURS *phi_pred = etable->Append_phi_pred_occurrence(Exp(), bb_orig, this);
      FOR_ALL_ELEM (bb_phi, bb_iter, Init(bb_orig->Succ())) {
	EXP_PHI *exp_phi = etable->Lookup_exp_phi(bb_phi, Exp());
	if (exp_phi != NULL) {
	  INT32 opnd_num = bb_phi->Pred()->Pos(bb_orig);
	  exp_phi->Set_pred(opnd_num, phi_pred);
	}
      }
    }
    
  }
  OPT_POOL_Pop(local_pool, -1);

  return TRUE;
}
