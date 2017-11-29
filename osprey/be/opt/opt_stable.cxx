//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
//
// Module: opt_stable.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_stable.cxx,v $
//
// Revision history:
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


#define opt_stable_CXX	"opt_stable.cxx"


#include "defs.h"
#include "cxx_memory.h"
#include "opt_bb.h"
#include "opt_main.h"
#include "opt_etable.h"
#include "idx_32_set.h"
#include "opt_ssu.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_cvtl_rule.h"

// =====================================================================
// Create a real occurrence node for a store and append to the real occurrence
// list
// =====================================================================
EXP_OCCURS *
ETABLE::Append_spre_real_occurrence(STMTREP *stmt,
				    EXP_WORKLST *worklist)
{
  Is_True(worklist != NULL, ("ETABLE::Append_spre_real_occurrence: null work list node"));
  // create the EXP_OCCURS node
  EXP_OCCURS *occur = Alloc_occurs_node();
  occur->Set_occurrence(stmt->Lhs());
  occur->Set_kind(EXP_OCCURS::OCC_REAL_OCCUR);
  occur->Set_enclose_stmt(stmt);

  // call the WORKLST append
  worklist->SPRE_append_occurrence(occur);

  return occur;
}

// =====================================================================
// Create a iphi_succ occurrence node for a store and append to the iphi_succ 
// occurrence list
// =====================================================================
EXP_OCCURS *
ETABLE::Append_iphi_succ_occurrence(BB_NODE *bb,
				    EXP_WORKLST *worklist)
{
  Is_True(worklist != NULL, ("ETABLE::Append_iphi_succ_occurrence: null work list node"));
  // create the EXP_OCCURS node
  EXP_OCCURS *occur = Alloc_occurs_node();
  occur->Set_occurrence(NULL);
  occur->Set_kind(EXP_OCCURS::OCC_PHI_PRED_OCCUR);
  occur->Set_enclose_bb(bb);

  // call the WORKLST append
  worklist->SPRE_append_occurrence(occur);
  return occur;
}

void
EXP_WORKLST::SPRE_create_iphi_succ(ETABLE *etable)
{
  BB_NODE_SET        &iphi_succs = etable->Phi_work_set();
  BB_NODE_SET_ITER    bb_iter;
  IDTYPE              pdo_id;
  EXP_OCCURS         *iphi_occ;
  EXP_OCCURS_ITER     iphi_occ_iter;
  BB_NODE            *bb_iphi;
  BB_NODE            *bb_orig;
  BB_LIST_ITER        bb_list_iter;
  
  Is_Trace(etable->Tracing(),
	   (TFile, "====== EXP_WORKLST::SPRE_create_iphi_succ ======\n"));

  Is_Trace(etable->Tracing(),
	   (TFile, "The real occurrence list is:\n"));
  Is_Trace_cmd(etable->Tracing(), Real_occurs().Print(TFile));
  Is_Trace(etable->Tracing(),
	   (TFile, "The iphi occurrence list is:\n"));
  Is_Trace_cmd(etable->Tracing(), Phi_occurs().Print(TFile));
  
  iphi_succs.ClearD();
  
  FOR_ALL_NODE ( iphi_occ, iphi_occ_iter, Init( Phi_occurs().Head()) ) {
    FOR_ALL_ELEM ( bb_iphi, bb_list_iter, Init( iphi_occ->Bb()->Succ() ) ) {
      iphi_succs.Union1D( bb_iphi->Pdom_dfs_id() );
    }
    iphi_occ->Bb()->Set_exp_phi(iphi_occ->Exp_phi());
  }
  
  FOR_ALL_NODE ( pdo_id, bb_iter, Init(&iphi_succs) ) {
    bb_orig = etable->Cfg()->Pdo_Bb( pdo_id );
    Is_Trace(etable->Tracing(),
	     (TFile,"------ Generate Iphi-succ occurrence %d ------\n", 
	      bb_orig->Id()));
    EXP_OCCURS *iphi_succ = etable->Append_iphi_succ_occurrence(bb_orig, this);
    FOR_ALL_ELEM ( bb_iphi, bb_list_iter, Init( bb_orig->Pred() ) ) {
      EXP_PHI *iphi = etable->Lookup_exp_phi( bb_iphi, Exp() );
      if ( iphi != NULL ) {
	INT32 opnd_num = bb_iphi->Succ()->Pos( bb_orig );
	iphi->Set_pred(opnd_num, iphi_succ);
      }
    }
  }
}

inline void
EXP_WORKLST::SPRE_compute_backward_attributes(// etable carries
                                              // Etable_local_pool()
                                              ETABLE     *etable)
{
  Compute_forward_attributes(etable);
}



// =====================================================================
// If the rhs of the statement is not a CK_VAR node of the preg, force
// it to be by inserting a statement before it that stores the rhs into the
// preg and change the rhs of the existing statement to the preg. 
// Note: the original rhs' usecount is unchanged.
// =====================================================================
static void
SPRE_force_rhs_to_preg(STMTREP *stmt, IDTYPE preg, ETABLE *etable)
{
  CODEREP *rhs = stmt->Rhs();
  if (rhs->Kind() == CK_VAR && rhs->Aux_id() == preg) {
    etable->Htable()->Insert_var_phi(rhs, rhs->Defbb());
    return;
  }
  // create a new coderep node for the preg
  CODEREP *pregcr =
      etable->Htable()->Add_def(preg, -1, NULL /* defstmt */, 
				stmt->Lhs()->Dtyp(), stmt->Lhs()->Dtyp(),
				etable->Htable()->Sym()->St_ofst(preg),
				stmt->Lhs()->Lod_ty(), 0, TRUE);
  STMTREP *newstmt = etable->Save_replace_rhs_by_preg(stmt, pregcr, NULL);  
  etable->Add_stmt(newstmt, stmt->Bb());
}

// =====================================================================
// v is defined by chi; generate a load of v and assign to a new version 
// of the preg; insert this new statement at the end of bb
// =====================================================================
static void
SPRE_insert_load_to_preg(CODEREP *v, IDTYPE preg, BOOL sign_extd, BB_NODE *bb, ETABLE *etable)
{
  CODEREP *rhs;
  Is_True(! v->Is_flag_set(CF_IS_ZERO_VERSION),
	  ("SPRE_insert_load_to_preg: variable cannot be zero version"));
  v->IncUsecnt();
  // create a new coderep node for the preg
  CODEREP *pregcr =
      etable->Htable()->Add_def(preg, -1, NULL /* defstmt */, 
				v->Dtyp(), v->Dtyp(),
				etable->Htable()->Sym()->St_ofst(preg),
				v->Lod_ty(), v->Field_id(), TRUE);
  rhs = v;
  /* CVTL-RELATED start (correctness) */
  if (v->Is_integral_load_store() && 
      MTYPE_size_min(v->Dsctyp()) <= MTYPE_size_min(MTYPE_I4)) {
    OPCODE opc;
    CODEREP *new_cr = Alloc_stack_cr(0);
    INT load_need_cvt = Need_load_type_conversion(v->Is_sign_extd(), sign_extd,
						v->Dtyp(), v->Dsctyp(), &opc);
    switch (load_need_cvt) {
      case NOT_AT_ALL:
	break;
      case NEED_CVT:
	if (v->Is_flag_set(CF_MADEUP_TYPE)) {
	  v->Reset_flag(CF_MADEUP_TYPE);
	  if (opc == OPC_U8U4CVT) {
	    v->Set_dtyp(MTYPE_U8);
	    v->Set_dsctyp(MTYPE_U4);
	    v->Set_sign_extension_flag();
#ifndef KEY
          } else if (opc == OPC_U4U8CVT) {
            v->Set_dtyp(MTYPE_U4);
            v->Set_sign_extension_flag();
#else
          } else if (opc == OPC_I8I4CVT) {
            v->Set_dtyp(MTYPE_I8);
	    v->Set_dsctyp(MTYPE_I4);
            v->Set_sign_extension_flag();
#endif
	  } else Is_True(FALSE, ("SPRE_isnert_load_to_preg: wrong type conversion"));
	}
	else {
	  new_cr->Init_expr(opc, v);
	  rhs = etable->Htable()->Rehash(new_cr);
	}
	break;
      case NEED_CVTL:
	if (v->Is_flag_set(CF_MADEUP_TYPE)) {
	  v->Reset_flag(CF_MADEUP_TYPE);
	  if (opc == OPC_U4CVTL || opc == OPC_U8CVTL) {
	    v->Set_dtyp(Mtype_TransferSign(MTYPE_U4,v->Dtyp()));
	    v->Set_dsctyp(Mtype_TransferSign(MTYPE_U4,v->Dsctyp()));
	    v->Set_sign_extension_flag();
	  } else if (opc == OPC_I4CVTL || opc == OPC_I8CVTL) {
	    v->Set_dtyp(Mtype_TransferSign(MTYPE_I4,v->Dtyp()));
	    v->Set_dsctyp(Mtype_TransferSign(MTYPE_I4,v->Dsctyp()));
	    v->Set_sign_extension_flag();
	  } else Is_True(FALSE, ("SPRE_insert_load_to_preg: wrong type conversion"));
	}
	else {
	  new_cr->Init_expr(opc, v);
	  new_cr->Set_offset(MTYPE_size_min(v->Dsctyp()));
	  rhs = etable->Htable()->Rehash(new_cr);
	}
	break;
    }
  }
  /* CVTL-RELATED finish */

  STMTREP *newstmt = etable->Generate_stid_to_preg(pregcr, rhs, rhs->Dtyp(), bb,
						   0 /* linenum */);
  bb->Append_stmt_before_branch(newstmt);
  newstmt->Set_stmt_id(etable->Cfg()->Get_stmt_id());
  etable->Add_stmt(newstmt, bb);
}

static CHI_LIST *
Pick_less_live_chi_list(CHI_LIST *chi_list1, CHI_LIST *chi_list2)
{
  CHI_LIST_ITER chi_iter;
  CHI_NODE *chi;
  INT32 live_nodes1 = 0;
  INT32 live_nodes2 = 0;
  FOR_ALL_NODE(chi, chi_iter, Init(chi_list1)) {
    if (chi->Live()) 
      live_nodes1++;
  }
  FOR_ALL_NODE(chi, chi_iter, Init(chi_list2)) {
    if (chi->Live()) 
      live_nodes2++;
  }
  if (live_nodes1 <= live_nodes2)
    return chi_list1;
  return chi_list2;
}

// =====================================================================
// From the phi operands, get to all real stores of the variable 
// and force their rhs to become the preg of the variable.  Also return
// any chi list from there.  When it get to a phi that has been processed 
// before, it can return, to prevent infinite loop; it defines and uses the 
// PNF_FIND_DEF_PROCESSED flag to detect this.
// =====================================================================
static CHI_LIST *
SPRE_find_def_from_phi(PHI_NODE *phi, IDTYPE preg, BOOL sign_extd, ETABLE *etable)
{
  CODEREP *opnd;
  CHI_LIST *chi_list = NULL;
  BB_LIST_ITER bb_pred_iter;
  BB_NODE *bbpred;
  INT32 pos = 0;
  FOR_ALL_ELEM(bbpred, bb_pred_iter, Init(phi->Bb()->Pred())) {
    opnd = phi->OPND(pos);
    if (opnd->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI|CF_IS_ZERO_VERSION))) {
      // this happens only under aggcm
      Is_True(WOPT_Enable_Aggressive_Code_Motion,
	      ("SPRE_find_def_from_phi: must not see a def by chi or zero ver"));
      Is_Trace(etable->Tracing(),
	       (TFile, "SPRE aggcm: load to preg inserted at BB%d\n", 
		bbpred->Id()));
      if (opnd->Is_flag_set(CF_IS_ZERO_VERSION)) { // rename to new version
	etable->Htable()->Fix_zero_version(phi, pos);
	opnd = phi->OPND(pos);
      }
      SPRE_insert_load_to_preg(opnd, preg, sign_extd, bbpred, etable);
    }
    else if (! opnd->Is_flag_set(CF_DEF_BY_PHI)) {
      STMTREP *stmt = opnd->Defstmt();
      Is_True(stmt != NULL, 
	      ("SPRE_find_def_from_phi: defstmt is NULL for cr%d", opnd->Coderep_id()));
      SPRE_force_rhs_to_preg(stmt, preg, etable);
      if (chi_list == NULL)
	chi_list = stmt->Chi_list();
      else chi_list = Pick_less_live_chi_list(chi_list, stmt->Chi_list());
    }
    else { // recursive call
      PHI_NODE *defphi = opnd->Defphi();
      if (defphi->Find_def_processed()) {
	pos++;
	continue;
      }
      defphi->Set_find_def_processed();
      CHI_LIST *tmp_chi_list = SPRE_find_def_from_phi(defphi, preg, sign_extd, etable);
      if (tmp_chi_list != NULL) {
	if (chi_list == NULL)
          chi_list = tmp_chi_list;
        else chi_list = Pick_less_live_chi_list(chi_list, tmp_chi_list);
      }
    }
    pos++;
  }
  return chi_list;
}

// =====================================================================
// x is a variable defined by phi.  preg is x's corresponding preg. Find
// the corresponding coderep node of preg based on the fact that at the
// phi of x, there must be a phi for preg. It defines and uses the 
// PNF_FIND_CORR_PROCESSED flag to prevent infinite loop.
// =====================================================================
static CODEREP *
SPRE_find_corresponding_preg_cr(CODEREP *x, IDTYPE preg, ETABLE *etable)
{
  if (x->Defphi()->Find_corr_processed())
    return NULL;
  x->Defphi()->Set_find_corr_processed();

  BB_NODE *bb = x->Defphi()->Bb();
  PHI_NODE *pregphi = etable->Lookup_var_phi(bb, preg);
  if (pregphi != NULL)
    return pregphi->RESULT();
  CODEREP *opnd = x->Defphi()->OPND(0); // 0th operand is arbitrary, should
                                        // get same result
  if (opnd->Is_flag_set(CF_DEF_BY_PHI)) 
    return SPRE_find_corresponding_preg_cr(opnd, preg, etable);
  if (opnd->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI|CF_IS_ZERO_VERSION))) {
    Is_True(WOPT_Enable_Aggressive_Code_Motion,
            ("SPRE_find_corresponding_preg_cr: must not see a def by chi"));
    return NULL;
  }
  // defined by STID
  SPRE_force_rhs_to_preg(opnd->Defstmt(), preg, etable);
  CODEREP *rhs = opnd->Defstmt()->Rhs();
  Is_True(rhs->Kind() == CK_VAR && rhs->Aux_id() == preg,
          ("SPRE_find_corresponding_preg_cr: store of current version has bad rhs"));

  return rhs;
}

// =====================================================================
// Given an old chi list, clone a new chi list, creating new versions for
// the chi results.  If chi_opnd_not_current, use 0 versions for the chi
// operands; otherwise, use the same chi operand.
// =====================================================================
static CHI_LIST *
SPRE_clone_chi_list(CHI_LIST *old_chi_list, 
		    BOOL chi_opnd_not_current,
		    STMTREP *stmt,
		    ETABLE *etable)
{
  CHI_LIST_ITER chi_iter;
  CHI_NODE *chi, *newchi;
  CHI_LIST *new_chi_list = CXX_NEW(CHI_LIST, etable->Htable()->Mem_pool());
  FOR_ALL_NODE(chi, chi_iter, Init(old_chi_list)) {
    newchi = new_chi_list->New_chi_node(chi->Aux_id(), etable->Htable()->Mem_pool());
    newchi->Clone(chi);

    if (!chi->Dse_dead()) {
      // create a new coderep node for the lhs of the chi
      CODEREP *new_lhs = etable->Htable()->Add_def(chi->Aux_id(), -1,
						   NULL /* defstmt */, 
						   chi->RESULT()->Dtyp(), 
						   chi->RESULT()->Dsctyp(),
						   chi->RESULT()->Offset(),
						   chi->RESULT()->Lod_ty(),
						   chi->RESULT()->Field_id(),
						   TRUE);
      new_lhs->Set_flag(CF_DEF_BY_CHI);
      new_lhs->Set_defstmt(stmt);
      new_lhs->Set_defchi(newchi);
      newchi->Set_RESULT(new_lhs);
      if (chi_opnd_not_current) 
	newchi->Set_OPND(etable->Htable()->Ssa()->Get_zero_version_CR(
	  chi->Aux_id(), etable->Opt_stab(), 0));
      else newchi->Set_OPND(chi->OPND());
    }
  }
  return new_chi_list;
}

// =====================================================================
// Perform store code motion (SPRE step 6).  It first processes insertions
// by looping through the phi_pred occurrence list.  Then it processes
// deletions by loop through the real occurrence list.
// =====================================================================
void
EXP_WORKLST::SPRE_perform_insert_delete(ETABLE *etable)
{
  EXP_OCCURS *occur;
  STMTREP *stmt;
  BB_NODE *bb;
  EXP_PHI *iphi;
  BB_NODE *iphi_bb;
  BB_LIST_ITER bb_iter;
  CHI_LIST_ITER chi_iter;
  CHI_NODE *chi;
  EXP_OCCURS_ITER occ_iter;


  // process insertions
  FOR_ALL_NODE(occur, occ_iter, Init(Phi_pred_occurs().Head())) {
    CODEREP *old_lhs;

    if (! occur->Inserted_computation()) 
      continue;
    bb = occur->Bb();

    // if there is a phi for the variable in bb, must be insertion at
    // critical edge, and vice versa
    PHI_NODE *varphi = NULL;
    if (bb->Pred()->Multiple_bbs()) // > 1 predecessor
      varphi = etable->Lookup_var_phi(bb, Exp()->Aux_id());
    if (varphi != NULL)
      old_lhs = varphi->RESULT();
    else {
      // get the iphi in order to get the lhs of the old store (old_lhs)
      FOR_ALL_ELEM(iphi_bb, bb_iter, Init(bb->Pred())) {
        iphi = etable->Lookup_exp_phi(iphi_bb, Exp());
        if (iphi != NULL) break;
      }
      Is_True(iphi != NULL,
	      ("EXP_WORKLST::SPRE_perform_insert_delete: at iphi succ, cannot find iphi"));
  
      old_lhs = iphi->Result()->Occurrence();
      Is_True(old_lhs != NULL,
	      ("EXP_WORKLST::SPRE_perform_insert_delete: occurrence of iphi not set at BB%d", 
	       iphi_bb->Id()));
    }

    Is_True(! old_lhs->Is_flag_set(CF_IS_ZERO_VERSION),
	    ("EXP_WORKLST::SPRE_perform_insert_delete: at insertion, current version is 0 version"));

    // determine the rhs of the store being inserted
    Is_True(! old_lhs->Is_flag_set(CF_DEF_BY_CHI),
	    ("EXP_WORKLST::SPRE_perform_insert_delete: at insertion, current version cannot be defined by chi"));
    BOOL chi_opnd_not_current;  // if TRUE, use 0 versions for chi opnds
    CHI_LIST *old_chi_list;
    CODEREP *rhs;

    if (old_lhs->Is_flag_set(CF_DEF_BY_PHI)) {
      Is_True(old_lhs->Defphi()->Live(),
	      ("EXP_WORKLST::SPRE_perform_insert_delete: at insertion, current version cannot be defined by dead phi"));
      // use the result of the phi of the preg in the def bb
      BB_NODE *defbb = old_lhs->Defphi()->Bb();
      old_lhs->Defphi()->Set_find_def_processed();
      old_chi_list = SPRE_find_def_from_phi(old_lhs->Defphi(), Preg(), Sign_extd(), etable);
      chi_opnd_not_current = TRUE;
      rhs = SPRE_find_corresponding_preg_cr(old_lhs, Preg(), etable);
      if (rhs == NULL) {
        rhs = etable->Htable()->Ssa()->Get_zero_version_CR(
					      Preg(), etable->Opt_stab(), 0);
        // NVISA hits this cause aggcm off by default,
	// fred doesn't remember why assertion is there, and code seems
	// fine without it, so ifdef it out.
        // Is_True(WOPT_Enable_Aggressive_Code_Motion,
	//    ("EXP_WORKLST::SPRE_perform_insert_delete: cannot find phi for preg %d at BB%d", Preg(), defbb->Id()));
      }
    }
    else { // defined by STID
      SPRE_force_rhs_to_preg(old_lhs->Defstmt(), Preg(), etable);
      rhs = old_lhs->Defstmt()->Rhs();
      Is_True(rhs->Kind() == CK_VAR && rhs->Aux_id() == Preg(),
	      ("EXP_WORKLST::SPRE_perform_insert_delete: at insertion, store of current version has bad rhs"));
      old_chi_list = old_lhs->Defstmt()->Chi_list();
      chi_opnd_not_current = FALSE;
    }
    rhs->IncUsecnt();	// increment use count of preg

    // create a new coderep node for the lhs of the store being inserted
#ifdef KEY // bug 5798
    if (old_lhs->Dtyp() == MTYPE_UNKNOWN) {
      old_lhs->Set_dsctyp(Exp()->Dsctyp());
      old_lhs->Set_dtyp(Exp()->Dtyp());
    }
#endif
    CODEREP *new_lhs = etable->Htable()->Add_def(old_lhs->Aux_id(), -1,
	  NULL /* defstmt */, old_lhs->Dtyp(), old_lhs->Dsctyp(),
	  old_lhs->Offset(), old_lhs->Lod_ty(), old_lhs->Field_id(), TRUE);

    // create the inserted store statement
    stmt = CXX_NEW(STMTREP, etable->Htable()->Mem_pool());
    stmt->Init(new_lhs, rhs, OPCODE_make_op(OPR_STID, MTYPE_V, new_lhs->Dsctyp()));
    stmt->Set_chi_list(SPRE_clone_chi_list(old_chi_list, 
				chi_opnd_not_current, stmt, etable));
    // stmt->Set_rhs_type(rhs->Dtyp());
    new_lhs->Set_defstmt(stmt);
    stmt->Set_bb(bb);
    if (bb->First_stmtrep() == NULL)
      stmt->Set_linenum(bb->Linenum());
    else stmt->Set_linenum(bb->First_stmtrep()->Linenum());
    stmt->Set_live_stmt();
    stmt->Set_stmt_id(etable->Cfg()->Get_stmt_id());

    // insert the statement at beginning of BB
    bb->Prepend_stmtrep(stmt);
    etable->Add_stmt(stmt, bb);
    Inc_insert_count();

    // update SPRE_temp flag in opt_stab.
    etable->Opt_stab()->Aux_stab_entry(Preg())->Set_SPRE_temp();
  }

  // process deletions
  FOR_ALL_NODE(occur, occ_iter, Init(Real_occurs().Head())) {
    if (occur->Delete_comp()) {
      stmt = occur->Enclosed_in_stmt();
      if (stmt->Volatile_stmt()) 
	continue;
      bb = stmt->Bb();

      // go thru chi list to mark chi results with CF_SPRE_REMOVED flag
      FOR_ALL_NODE(chi, chi_iter, Init(stmt->Chi_list())) {
	if (chi->Dse_dead() ||
	    !chi->Live() || 
	    chi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION))
	  continue;
	chi->RESULT()->Set_flag(CF_SPRE_REMOVED);
      }

      // insert phi for the RHS preg
      if (stmt->Rhs()->Kind() == CK_VAR)
	etable->Htable()->Insert_var_phi(stmt->Rhs(), stmt->Rhs()->Defbb());

      stmt->Lhs()->Set_flag(CF_SPRE_REMOVED);
      bb->Remove_stmtrep(stmt); // TODO: free up chi list?
      Inc_reload_count();

    } 
  }

  Is_Trace(etable->Tracing(),
	   (TFile, "==== After EXP_WORKLST::SPRE_perform_insert_delete\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile, NULL));

  // statistics
  etable->Inc_cse_reloads(Reload_count());
  etable->Inc_inserted_saves(Insert_count());
}


void
ETABLE::Add_stmt(STMTREP *stmt, BB_NODE *bb)
{
  // verify that all inserted definitions have their corr phi already in place.
  CODEREP *new_lhs = stmt->Lhs();
  Htable()->Insert_var_phi(new_lhs, bb);
  CHI_LIST_ITER chi_iter;
  CHI_NODE *chi;
  FOR_ALL_NODE(chi, chi_iter, Init(stmt->Chi_list())) {
    if (chi->Live())
      Htable()->Insert_var_phi(chi->RESULT(), bb);
  }
}


void
ETABLE::Perform_SPRE_optimization(void)
{
  if (Tracing()) {
    if (Get_Trace(TKIND_ALLOC, TP_WOPT1)) {
      MEM_Tracing_Enable();
    }
    fprintf( TFile, "%sProgram before SPRE:\n%s",
	     DBar, DBar );
    Cfg()->Print(TFile);
  }

  Cfg()->Pdo_vec();  // To initialize Pdo_vec before Cfg()->Pdo_Bb(pdo_id)

  OPT_POOL_Push(Etable_local_pool(), SPRE_DUMP_FLAG);
  {
    _ssu = CXX_NEW(SSU(Htable(), Cfg(), Opt_stab(), this, 
		       Etable_pool(), Etable_local_pool(),
                       Get_Trace(TP_GLOBOPT, SPRE_DUMP_FLAG)),
		   Etable_local_pool());

    // perform SSU related functions:
    // (1) phi insertion
    // (2) renaming
    // (3) build worklist for ETABLE
    Ssu()->Construct();
  }
  OPT_POOL_Pop(Etable_local_pool(), SPRE_DUMP_FLAG);
  
  // Phase numbers for various steps
  INT32 Downsafe_prop_phase = 0;
  INT32 Avail_insert_phase  = 0;
  INT32 Save_reload_phase   = 0;
  INT32 Finalize_phase 	= 0;
      
  INT32            cur_worklst_idx = 0;
  EXP_WORKLST     *cur_worklst;
  EXP_WORKLST_ITER worklst_iter(Exp_worklst());
  FOR_ALL_NODE(cur_worklst, worklst_iter, Init()) {

    cur_worklst_idx++;
    if (WOPT_Enable_Store_PRE_Limit != -1 &&
	/*cur_worklst->E_num()*/cur_worklst_idx > WOPT_Enable_Store_PRE_Limit) {
      DevWarn("SPRE: skip SPRE for variable with v_num > %d\n",
	      WOPT_Enable_Store_PRE_Limit);
      break;
    }

    OPT_POOL_Push(Per_expr_pool(), SPRE_DUMP_FLAG);
    Is_Trace(Tracing(),
	     (TFile, "\n||||||||||||| processing %dth store\n", cur_worklst_idx));
    Is_Trace_cmd(Tracing(),cur_worklst->Print(TFile, NULL));

    Is_Trace_cmd(Tracing(),cur_worklst->Exp()->Print(0,TFile));

#ifdef Is_True_On    
    {
      EXP_OCCURS *iphi_occ;
      EXP_OCCURS_ITER     iphi_occ_iter;
	FOR_ALL_NODE ( iphi_occ, iphi_occ_iter, Init( cur_worklst->Phi_occurs().Head()) ) {
	  Is_True(iphi_occ->Exp_phi()->Result() == iphi_occ,
		  ("ETABLE::Perform_SPRE_optimization: Inconsistent Phi-occur."));
	  Is_True(iphi_occ->For_spre(),
		  ("ETABLE::Perform_SPRE_optimization:  for_spre not set in EXP_OCCUR."));
	}
    }
#endif


    {
      // Create iphi_succ and setup Bb()->Exp_phi() field.  
      cur_worklst->SPRE_create_iphi_succ(this);

      // Propgate downsafe must not remove anything from Phi_occurs.
      SET_OPT_REPEAT_PHASE(Downsafe_prop_phase, "SPRE: Up Safety");
      cur_worklst->Propagate_downsafe(this);

      SET_OPT_REPEAT_PHASE(Avail_insert_phase, "SPRE: Backward attributes");
      cur_worklst->SPRE_compute_backward_attributes(this);
      
      SET_OPT_REPEAT_PHASE(Save_reload_phase, "SPRE: Compute insert/delete");
      cur_worklst->SPRE_compute_insert_delete(Htable(), this);
      
      SET_OPT_REPEAT_PHASE(Finalize_phase, "SPRE: Perform insert/delete");
      cur_worklst->SPRE_perform_insert_delete(this);

      Opt_tlog( "SPRE", 0,
	        "%d-th variable: Inserts=%d, Deletes=%d",
               cur_worklst_idx,
               cur_worklst->Insert_count(),
               cur_worklst->Reload_count());

      // Clear Bb()->Exp_phi() field.  
      EXP_OCCURS *iphi_occ;
      EXP_OCCURS_ITER     iphi_occ_iter;
      FOR_ALL_NODE ( iphi_occ, iphi_occ_iter, Init( cur_worklst->Phi_occurs().Head()) ) {
	iphi_occ->Bb()->Set_exp_phi(NULL);
      }
    }  

    OPT_POOL_Pop(Per_expr_pool(), SPRE_DUMP_FLAG);

  }

  if (Tracing()) {
    fprintf(TFile, "%sBefore SPRE rename\n%s", DBar, DBar);
    Cfg()->Print(TFile);
  }

  SET_OPT_PHASE("SPRE: Update SSA");
  SPRE_update_ssa();

  if (Tracing()) {
    fprintf(TFile, "%sAfter SPRE\n%s", DBar, DBar);
    fprintf(TFile, "Statistics (all expressions): Insert Count %d, "
	    "Delete Count %d\n", _num_inserted_saves, _num_cse_reloads);
    Cfg()->Print(TFile);
    if (Get_Trace(TKIND_ALLOC, TP_WOPT1)) {
      MEM_Trace();
    }
  }
}

void
COMP_UNIT::Do_store_pre(void)
{
  MEM_POOL stable_pool, per_str_pool, stable_local_pool;

  OPT_POOL_Initialize(&stable_pool, "stable pool", FALSE, SPRE_DUMP_FLAG);
  OPT_POOL_Initialize(&per_str_pool, "per str pool", FALSE, SPRE_DUMP_FLAG);
  OPT_POOL_Initialize(&stable_local_pool, "stable local pool", FALSE, SPRE_DUMP_FLAG);
  OPT_POOL_Push(&stable_pool, SPRE_DUMP_FLAG);
  OPT_POOL_Push(&per_str_pool, SPRE_DUMP_FLAG);
  OPT_POOL_Push(&stable_local_pool, SPRE_DUMP_FLAG);

  {
    ETABLE etable(Cfg(), Opt_stab(), Htable(), Arule(), 10, &stable_pool, 
		  &per_str_pool, &stable_local_pool, this, PK_SPRE);
    etable.Perform_SPRE_optimization();
  } // the etable destructor is called here

  OPT_POOL_Pop(&stable_local_pool, SPRE_DUMP_FLAG);
  OPT_POOL_Pop(&per_str_pool, SPRE_DUMP_FLAG);
  OPT_POOL_Pop(&stable_pool, SPRE_DUMP_FLAG);
  OPT_POOL_Delete(&stable_local_pool, SPRE_DUMP_FLAG);
  OPT_POOL_Delete(&per_str_pool, SPRE_DUMP_FLAG);
  OPT_POOL_Delete(&stable_pool, SPRE_DUMP_FLAG);
}
