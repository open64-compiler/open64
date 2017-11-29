//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_spre_finalize.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_spre_finalize.cxx,v $
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


#include "id_map.h"

#include "opt_etable.h"
#include "opt_vertab.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_fold.h"
#include "tracing.h"

#include "opt_config.h"		// WOPT_Enable_Output_Copy


void
EXP_WORKLST::SPRE_Determine_inserts_saves_deletions(CODEMAP         *htable,
						    ETABLE          *etable,
						    E_VER_TAB       *e_ver_tab)
{
  // For each spot where an expression needs to be inserted, determine
  // the current versions of both expression operands (there can be
  // two at most), build a coderep referring to them, and insert the
  // coderep in the appropriate BB.
  EXP_ALL_OCCURS_ITER  occ_iter(Real_occurs().Head(),
				NULL,
				Phi_occurs().Head(),
				Phi_pred_occurs().Head(),
				NULL);   // disregard exit occurrences
  EXP_OCCURS          *occ;

  // For each phi node for this expression, find its operands and its
  // predecessor BB's, and perform insertion if required (i.e., if the
  // operand should be available but isn't).
  //
  // For each real occurrence node for this expression, determine
  // whether the node should be saved, and whether it should be
  // deleted. We make heavy use of the property that the
  // EXP_ALL_OCCURS_ITER (via dominator preorder) gives us a
  // definition for each version before we see any uses.
  FOR_ALL_NODE(occ, occ_iter, Init()) {
    switch (occ->Occ_kind()) {
    case EXP_OCCURS::OCC_PHI_OCCUR:
      if (occ->Exp_phi()->Will_b_avail()) {
	// Because the result of this phi will be available, this phi
	// is the available definition for its result's
	// e-version. Save the occurrence in the e_ver_tab.
	Is_True(e_ver_tab->Avail_def(occ->E_version()) == NULL,
		("SPRE: e-version %d already has an available definition",
		 occ->E_version()));
	e_ver_tab->Set_avail_def(occ->E_version(), occ);
      }
      else {
	// This phi occurrence will not be available. I don't think we
	// need to do anything.
      }
      break;
    case EXP_OCCURS::OCC_REAL_OCCUR:
      {
	// In the beginning, all real occurrences are assumed not to be
	// deleted so if we turn off Finalize, CodeMotion will still
	// generate correct output. We set the "delete" bit for this
	// occurrence if some other occurrence will serve as the
	// available definition for this e-version (i.e. the avail_def
	// entry for this e-version is non-NULL).
	//
	// If this occurrence is not the available definition for its
	// e-version, we will certainly not save it and we will
	// certainly delete it. Therefore it requires no further
	// processing. If is is the available definition, though, we
	// cannot determine its "save" bit, since we don't know whether
	// it has more than one use or not. So we save it in the
	// e_ver_tab, which will get traversed after we've seen
	// all the occurrences for this expression.

	EXP_OCCURS *def = e_ver_tab->Avail_def(occ->E_version());

	if (def == NULL || !def->Bb()->Postdominates(occ->Bb())) {
	  // This is the first possible available definition we've seen
	  // of this version along the present control flow path, so
	  // this is the available definition. We know we won't delete
	  // this occurrence, but whether we save it or not will depend
	  // on whether the same version gets used anywhere else (as a
	  // real occurrence or as an operand of a phi whose result will
	  // be available).
	  e_ver_tab->Set_real_avail_def(occ->E_version(), occ);
	  occ->Set_def_occur(NULL);
	}
	else {
	  // The available definition for this version is by a
	  // dominating real or phi occurrence with the same
	  // e-version. If the definition is a real occurrence, it must
	  // be saved to PREG and this real occurrence will be deleted
	  // (reloaded).
	  Is_True(def->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR ||
		  (def->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR &&
		   e_ver_tab->Occ_count(occ->E_version()) >= 1),
		  ("SPRE: Inconsistent occurrence count for e-version %d",
		   occ->E_version()));

	  // This occurrence will be a reload. Set the bit to say its
	  // computation will be deleted, and hook up the defining
	  // occurrence field.
	  Is_Trace(etable->Tracing(), 
		   (TFile, "SPRE: delete one operand at BB%d\n", occ->Bb()->Id()));
	  occ->Set_delete_comp();
	  occ->Set_def_occur(def);
	  e_ver_tab->Note_version_use(occ->E_version(), 1);
	}
      }
      break;
    case EXP_OCCURS::OCC_PHI_PRED_OCCUR:  // OCC_IPHI_SUCC_OCCUR
      {
	// If any operand of a will-be-available phi corresponding to
	// this phi pred occurrence uses the current version, we need
	// to save the version's available definition. If any operand
	// of a will-be-available phi corresponding to this phi pred
	// occurrence is NULL or is not will-be-available, we need to
	// perform insertion for that operand.

	BB_LIST_ITER  pred_bb_iter;
	BB_NODE      *pred_bb;
	BB_NODE      *occ_bb = occ->Bb();
#if Is_True_On
	BOOL          found_phi = FALSE;
#endif
	FOR_ALL_ELEM(pred_bb, pred_bb_iter, Init(occ_bb->Pred())) {
	  EXP_PHI *phi = etable->Lookup_exp_phi(pred_bb, Exp());

	  if (phi != NULL) {
	    Is_True(phi->Bb() == pred_bb,
		    ("EXP_WORKLST::SPRE_Determine_inserts_saves_deletions: "
		     "ETABLE::Lookup_exp_phi failed"));

	    // There is an expression phi in this successor block.
#if Is_True_On
	    found_phi = TRUE;
#endif
	    if (phi->Will_b_avail()) {
	      // POSSIBLE BUG: If occ_bb appears in succ_bb->Pred()
	      // more than once, we have a problem. A bug I fixed once
	      // in IRM suggests that this can happen. -- RK
	      const INT32             opnd_num = pred_bb->Succ()->Pos(occ_bb);
	      const EXP_OCCURS *const opnd     = phi->Opnd(opnd_num);

	      Is_True(phi->Succ(opnd_num) == occ,
		      ("EXP_WORKLST::SPRE_Determine_inserts_saves_deletions: "
		       "phi->Succ() / phi-pred inconsistency"));

	      if (phi->Need_insertion(opnd_num)) {

		occ->Set_inserted();

		// Perform insertion for this operand.
		// Insert_one_operand(etable, htable, phi, opnd_num);
		Is_Trace(etable->Tracing(), 
			 (TFile, "SPRE: insert one operand at BB%d\n", occ_bb->Id()));
	      }
	      else {
		// No insertion required for this operand; note its
		// use of its available definition.

		// Must use opnd->E_version() in the following because
		// phi-pred occurrences aren't allowed to use the
		// E_version() member function.

		e_ver_tab->Note_version_use(opnd->E_version());
		phi->Set_opnd(opnd_num,
			      e_ver_tab->Avail_def(opnd->E_version()));
	      }
	    }
	  }
	}
	Is_True(found_phi,
		("EXP_WORKLST::SPR_Determine_inserts_saves_deletions: "
		 "phi-pred must have successor phi"));
      }
      break;
    default:
      FmtAssert(FALSE, ("EXP_WORKLST::SPRE_Determine_inserts_saves_deletions: "
			"Bad occurrence kind"));
    }
  }
}



void
EXP_WORKLST::SPRE_compute_insert_delete(// inserted exprs go into htable
                                        CODEMAP         *htable,
                                        // etable carries
                                        // Etable_local_pool()
                                        ETABLE          *etable)
{
  // Prepare for e_ver_tab allocation.
  OPT_POOL_Push(etable->Etable_local_pool(), SPRE_DUMP_FLAG+1);

  E_VER_TAB *e_ver_tab =
    CXX_NEW(E_VER_TAB(etable->Etable_local_pool(),
		      Cur_e_version(),
		      etable->Tracing()),
	    etable->Etable_local_pool());

  SPRE_Determine_inserts_saves_deletions(htable, etable, e_ver_tab);

  Is_Trace(etable->Tracing(),
	   (TFile, "==== After EXP_WORKLST::SPRE_Determine_inserts_saves_deletions\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));

  CXX_DELETE(e_ver_tab, etable->Etable_local_pool());

  OPT_POOL_Pop(etable->Etable_local_pool(), SPRE_DUMP_FLAG+1);
}


//  Returns the current version if the current version has been changed.
//
CODEREP *
ETABLE::SPRE_rename_var(CODEREP *cr, BOOL allow_zero_ver)
{
  if (cr->Is_var_volatile())
    return NULL;

  AUX_ID auxid = cr->Aux_id();
  if (Opt_stab()->NULL_coderep(auxid)) {

    Is_Trace(Tracing(),
	     (TFile, "SPRE_rename_ver:  pushing zero version cr onto stack for auxid %d.\n", auxid));
    CODEREP *zcr = Htable()->Ssa()->Get_zero_version_CR(auxid, Opt_stab(), 0);
    Opt_stab()->Push_coderep(auxid, zcr);
  }

  CODEREP *top = Opt_stab()->Top_coderep(auxid);

  Is_True(top != NULL, ("ETABLE::SPRE_rename_var:  stack top is null."));

#ifdef Is_True_On
  if (!allow_zero_ver) {
    Is_True(!top->Is_flag_set(CF_IS_ZERO_VERSION),
	    ("ETABLE::SPRE_rename_var: sym%dv%d(cr%d) is zero ver.", 
	     top->Aux_id(), top->Version(), top->Coderep_id()));
  }
#endif

  return (top != cr) ? top : NULL;
}


CODEREP *
ETABLE::SPRE_rename_expr(CODEREP *cr, BB_NODE *bb)
{
  switch (cr->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
    return NULL;

  case CK_VAR:
    return SPRE_rename_var(cr, FALSE);

  case CK_IVAR: 
    {
      CODEREP *newcr = Alloc_stack_cr(cr->Extra_ptrs_used());
      MU_NODE *mnode = cr->Ivar_mu_node();
      CODEREP *m_cr = NULL;
      CODEREP *ilod_base = NULL;
      CODEREP *mload_size = NULL;
      CODEREP *iloadx_index = NULL;

      if (mnode) {
	if (cr->Ivar_defstmt() && 
	    OPERATOR_is_scalar_istore (cr->Ivar_defstmt()->Opr()) &&
	    cr->Ivar_defstmt()->Lhs() == cr) {
	  // Part of problem exposed by 621101:
	  // need to preserve the mu-node if this CK_IVAR is also
	  // an ISTORE node.
	} else {
	  m_cr = SPRE_rename_var(mnode->OPND(), TRUE);
#ifdef KEY // bug 11739
          if (m_cr && m_cr->Is_flag_set(CF_IS_ZERO_VERSION))
            m_cr = NULL;
#endif
        }
      }

      ilod_base = SPRE_rename_expr(cr->Ilod_base(), bb);

      if (cr->Opr() == OPR_MLOAD)
	mload_size = SPRE_rename_expr(cr->Mload_size(), bb);
      else if (cr->Opr() == OPR_ILOADX)
	iloadx_index = SPRE_rename_expr(cr->Index(), bb);

      if (m_cr || ilod_base || mload_size || iloadx_index) { // need rehash
	newcr->Copy(*cr);   // not creating new cr, just copy
	if (m_cr) {
	  // Fix 622235, 621101:
	  //   Clone the mu-node.  Update the version of mu-operand.
	  //   Rehash the CK_IVAR node if mu-operand has been changed.
	  MU_NODE *mnode = (MU_NODE*) CXX_NEW(MU_NODE, Htable()->Mem_pool());
	  mnode->Clone(cr->Ivar_mu_node());
	  newcr->Set_ivar_mu_node(mnode);
	  mnode->Set_OPND(m_cr);
	}
	if (ilod_base) 
	  newcr->Set_ilod_base(ilod_base);
	newcr->Set_istr_base(NULL);
	newcr->Set_usecnt(0);
	if (mload_size)
	  newcr->Set_mload_size(mload_size);
	if (iloadx_index)
	  newcr->Set_index(iloadx_index);
	newcr->Set_ivar_occ(cr->Ivar_occ());
	cr->DecUsecnt();
	return Htable()->Rehash(newcr);
      } 
      return NULL;
    }

  case CK_OP:
    {
      CODEREP *newcr = Alloc_stack_cr(cr->Extra_ptrs_used());
      newcr->Copy(*cr);
      BOOL need_rehash = FALSE;
      newcr->Set_usecnt(0);

      for  (INT32 i = 0; i < cr->Kid_count(); i++) {
	CODEREP *expr = SPRE_rename_expr(cr->Opnd(i), bb);
	if (expr) {
	  need_rehash = TRUE;
	  newcr->Set_opnd(i, expr);
	}
      }

      if (need_rehash) 
	return Htable()->Rehash(newcr);
      return NULL;
    }

  default:
    Is_True(FALSE, ("ETABLE::SPRE_rename_expr:  unexpected CK_KIND."));
    return NULL;
  }
}


void
ETABLE::SPRE_rename_stmt(STMTREP *stmt, BB_NODE *bb)
{
  CODEREP *newcr;

  if (stmt->Rhs()) {
    newcr = SPRE_rename_expr(stmt->Rhs(), bb);
    if (newcr) 
      stmt->Set_rhs(newcr);
  }
  switch (stmt->Opr()) { 
  case OPR_ISTORE:
  case OPR_ISTBITS:
    newcr = SPRE_rename_expr(stmt->Lhs()->Istr_base(), bb);
    if (newcr) 
      stmt->Lhs()->Set_istr_base(newcr);
    break;
  case OPR_MSTORE:
    {
      newcr = SPRE_rename_expr(stmt->Lhs()->Istr_base(), bb);
      if (newcr) 
	stmt->Lhs()->Set_istr_base(newcr);

      CODEREP *num_bytes = (CODEREP *)stmt->Lhs()->Mstore_size();
      newcr = SPRE_rename_expr(num_bytes, bb);
      if (newcr)
	stmt->Lhs()->Set_mstore_size(newcr);
      break;
    }
  default: ;
  }
}



//  Returns TRUE if
//   for every def-use chain (might be empty) connects a non-phi definition
//   of lhs to 'lhs',
//    1)  the non-phi definition is a STID, and
//    2)  the RHS of such definitions are lexical identical to 'rhs', and
//    3)  all variables in rhs are not modified in the path.
//      
BOOL
ETABLE::RHS_is_fully_avail(CODEREP *lhs, CODEREP *rhs)
{
  if (lhs->Is_flag_set(CF_IS_ZERO_VERSION) ||
      rhs->Is_flag_set(CF_IS_ZERO_VERSION))
    return FALSE;

  if (lhs->Is_flag_set(CF_DEF_BY_CHI)) {
    return FALSE;

  } else if (lhs->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = lhs->Defphi();
    if (phi->Visited())
      return FALSE;

    PHI_NODE *rhs_phi = Lookup_var_phi(phi->Bb(), rhs->Aux_id());
    PHI_OPND_ITER phi_opnd_iter(phi);

    phi->Set_visited();
    if (rhs_phi == NULL) {
      CODEREP *opnd;
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
	if (!RHS_is_fully_avail(opnd, rhs))
	  return FALSE;
      }
    } else {
      if (rhs_phi->RESULT() != rhs)
	return FALSE;
      INT i = 0;
      CODEREP *opnd;
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
	if (!RHS_is_fully_avail(opnd, rhs_phi->OPND(i++)))
	  return FALSE;
      }
    }
    phi->Reset_visited();
    return TRUE;
  } else {
    return lhs->Defstmt()->Rhs() == rhs;
  }
}


// Check if the stmt of the from "reg = var" is fully redundant.
//
//   This form is sufficient because of var = expr
//   becomes  var = t  after EPRE
//   then becomes t2 = t; var = t2 after LPRE.
//   
//   The stmt var = t2 will be deleted by SPRE.
//
BOOL
ETABLE::Stmt_is_redundant(STMTREP *stmt)
{
  Is_True(stmt->Live_stmt(), ("ETABLE::Delete_redundant_stmt: stmt already dead."));

  // must be STID
  if (! OPERATOR_is_scalar_store (stmt->Opr()))
    return FALSE;

  AUX_ID aux = stmt->Lhs()->Aux_id();

  // must find a definition of LHS at top of stack
  if (Opt_stab()->NULL_coderep(aux))
    return FALSE;

  // PV 473694.   LPRE does not introduce dead phi function,
  // so the top of stack might not be available!
  // Limit this optimization if the top of stack and the stmt are in
  // the same BB.
  CODEREP *top = Opt_stab()->Top_coderep(aux);
  if (top->Is_flag_set(CF_IS_ZERO_VERSION))
    return FALSE;
  if (top->Defbb() != stmt->Bb()) {
    Warn_todo("ETABLE::Stmt_is_redundant:  need dead phi for SPRE vars.");
    return FALSE;
  }

  // limit to PREG for the moment
  if (ST_sclass(Opt_stab()->Aux_stab_entry(aux)->St()) != SCLASS_REG)
    return FALSE;

  // Limit to CK_VAR RHS for the moment because all expr should have 
  // been converted to preg by EPRE.
  //
  // NOTE:  if this constraint is relaxed, then test whether the expr
  //  contain any dedicated preg.
  //
  if (stmt->Rhs()->Kind() != CK_VAR)
    return FALSE;

  // cannot delete assignments from/to dedicated registers
  if (Opt_stab()->Aux_stab_entry(aux)->Is_dedicated_preg() ||
      Opt_stab()->Aux_stab_entry(stmt->Rhs()->Aux_id())->Is_dedicated_preg())
    return FALSE;

  return RHS_is_fully_avail(Opt_stab()->Top_coderep(aux), stmt->Rhs());
}


//  After new versions are inserted, replace reference to old def
//  with new ones.
//
void
ETABLE::SPRE_rename(BB_NODE *bb)
{
  CODEREP *cr;
  PHI_NODE *phi; 
  PHI_LIST_ITER phi_iter;

  if (bb->Kind() == BB_ENTRY && bb != Cfg()->Fake_entry_bb()) {
    STMTREP *entry_chi = bb->Stmtlist()->Head();
    Is_True(entry_chi->Opr() == OPR_OPT_CHI, ("cannot find entry chi."));
    Set_entry_chi(entry_chi);
  }

  //  Iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (phi->Live()) 
      Opt_stab()->Push_coderep(phi->Aux_id(), phi->RESULT());
    else {
      CODEREP *zcr = Htable()->Ssa()->Get_zero_version_CR(phi->Aux_id(), Opt_stab(), 0);
      Opt_stab()->Push_coderep(phi->Aux_id(), zcr);
    }
  }

  BOOL some_stmt_deleted = FALSE;
  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    SPRE_rename_stmt(stmt, bb);

    // New step7 feature:  delete fully redundant STIDs
    //
    if (Stmt_is_redundant(stmt)) {
      some_stmt_deleted = TRUE;
      stmt->Reset_live_stmt();
      if (Tracing()) {
	fprintf(TFile, "SPRE_rename: found a redundant statement.");
	Htable()->Print_SR(stmt, TFile);
      }
      continue;  // skip the renaming of this stmt
    }

    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      CODEREP *lhs = stmt->Lhs();
      Opt_stab()->Push_coderep(lhs->Aux_id(), lhs);
    }

    if (stmt->Has_mu()) {
      MU_LIST_ITER mu_iter;
      MU_NODE *mnode;
      FOR_ALL_NODE( mnode, mu_iter, Init(stmt->Mu_list())) {
	cr = SPRE_rename_var(mnode->OPND(), TRUE);
	if (cr)
	  mnode->Set_OPND(cr);
      }
    }
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      if (stmt->Opr() == OPR_OPT_CHI) {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (!cnode->Dse_dead())
	    Opt_stab()->Push_coderep(cnode->Aux_id(), cnode->RESULT());
	  else {
	    CODEREP *zcr = Htable()->Ssa()->Get_zero_version_CR(cnode->Aux_id(), Opt_stab(), 0);
	    Opt_stab()->Push_coderep(cnode->Aux_id(), zcr);
	  }
	}
      } else {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (cnode->Live()) {
	    cr = SPRE_rename_var(cnode->OPND(), TRUE);
	    if (cr)
	      cnode->Set_OPND(cr);
	    Opt_stab()->Push_coderep(cnode->Aux_id(), cnode->RESULT());
	  }
	}
      }
    }
  }

  if (some_stmt_deleted) {
    STMTREP *nextstmt;
    for ( stmt = bb->First_stmtrep(), nextstmt = NULL;
	 stmt != NULL;
	 stmt = nextstmt ) {
      nextstmt = stmt->Next();
      if ( ! stmt->Live_stmt() ) {
	if (Tracing()) {
	  fprintf(TFile, "SPRE_rename: remove a redundant statement.");
	}
	bb->Remove_stmtrep(stmt);
      }
    }
  }

  BB_NODE *succ; BB_LIST_ITER bb_iter;
  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    INT32 pos = succ->Pred()->Pos(bb);
    FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
      if (phi->Live()) {
        cr = SPRE_rename_var(phi->OPND(pos), TRUE);
	if (cr) {
	  phi->Set_opnd(pos, cr);
	}
      }
    }
  }

  // do copy propagation for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    SPRE_rename(dom_bb);

  // iterate through each statement in this bb
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      if (stmt->Opr() == OPR_OPT_CHI) {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  Opt_stab()->Pop_coderep(cnode->Aux_id());
	}
      } else {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (cnode->Live())
	    Opt_stab()->Pop_coderep(cnode->Aux_id());
	}
      }
    } 
    if (OPERATOR_is_scalar_store (stmt->Opr()))
      Opt_stab()->Pop_coderep(stmt->Lhs()->Aux_id());
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    Opt_stab()->Pop_coderep(phi->Aux_id());
  }
}


void
ETABLE::SPRE_update_ssa(void)
{
  OPT_POOL_Push(Etable_local_pool(), SPRE_DUMP_FLAG+2);
  Opt_stab()->New_coderep(Etable_local_pool());
  Opt_stab()->Clear_coderep();
  SPRE_rename(Cfg()->Entry_bb());
  OPT_POOL_Pop(Etable_local_pool(), SPRE_DUMP_FLAG+2);
}
