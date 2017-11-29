//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
//
// Module: opt_ssu.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_ssu.cxx,v $
//
// Revision history:
//  10-DEC-96 ptu - Initial Version
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
//  Construction of SSU form.
//
//  Basically, the same old idea, but for use and RCFG.
//
//  See the paper Efficiently Computing Static Single Assignment Form
//  and the Control Dependence graph, Ron Cytron, Jeanne Ferrante,
//  Barry K. Rose, and Mark N. Wegman,  TOPLAS v13 n4 Oct., 1991.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#define opt_ssu_CXX    "opt_ssu.cxx"

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "erglob.h"
#include "glob.h"	// for Cur_PU_Name
#include "bb_node_set.h"
#include "idx_32_set.h"
#include "tracing.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_htable.h"
#include "opt_util.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_wn.h"
#include "opt_prop.h"
#include "wn.h"
#include "opt_alias_rule.h"
#include "opt_etable.h"
#include "opt_ssu.h"

#ifdef KEY
static void
// =====================================================================
// x's dtyp and dsctyp fields are not initialized; initialize them with
// values gotten by searching for other nodes of the same symbol
// =====================================================================
Fix_var_mtypes(CODEREP *x, AUX_STAB_ENTRY *aux) {
  CODEREP_ITER cr_iter;
  CODEREP *cr;
  FOR_ALL_NODE(cr, cr_iter, Init(aux->Cr_list()))
    if (cr->Dtyp() != MTYPE_V && cr->Dtyp() != MTYPE_UNKNOWN) {
      x->Set_dtyp(cr->Dtyp());
      x->Set_dsctyp(cr->Dsctyp());
    }
}
#endif

// =====================================================================
// cr must be a CK_VAR node; determine if it is a candidate for SPRE, and
// create the SPRE worklist node if it is, and return it
// =====================================================================
EXP_WORKLST * SSU::SPRE_candidate(CODEREP *cr)
{
  Is_True(cr->Kind() == CK_VAR,
	  ("SSU::SPRE_candidate: parameter must be CK_VAR node"));
  AUX_STAB_ENTRY *aux= Opt_stab()->Aux_stab_entry( cr->Aux_id() );
  EXP_WORKLST *wk = aux->Spre_node();
  if (wk)
    return wk;
  if (aux->No_spre()) return NULL; // this flag added to speed up checking
  if (!aux->Is_real_var() || 
      aux->No_register() || // Screen out MSTID
      !aux->Has_store_in_PU() || 
      aux->Is_volatile() ||
      ST_class(aux->St()) == CLASS_PREG ||
      cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
      Opt_stab()->Is_varargs_func() && ST_sclass(aux->St()) == SCLASS_FORMAL) {
    aux->Set_no_spre();
    return NULL;
  }

  wk = Etable()->Get_worklst(cr);
  aux->Set_spre_node(wk);
  // if LPRE has not been run or there is no load, home_sym will be 0;
  // create a new preg and set home_sym
  if (aux->Home_sym() == 0) {
#ifndef KEY
    Is_True(cr->Dtyp() != MTYPE_V && cr->Dtyp() != MTYPE_UNKNOWN,
	    ("SSU::SPRE_candidate: dtyp is void or unknown"));
#else
    // fix bug no OSP_52
    if (cr->Dtyp() == MTYPE_V || cr->Dtyp() == MTYPE_UNKNOWN ||
	cr->Dsctyp() == MTYPE_V || cr->Dsctyp() == MTYPE_UNKNOWN)
      Fix_var_mtypes(cr, aux);
#endif
    WN *home_wn = cr->Rvi_home_wn(Opt_stab());
    AUX_ID home_auxid = Opt_stab()->Create_preg(cr->Dtyp(),aux->St_name(),home_wn);
    // call to Create_preg may cause aux_stab to be re-allocated
    aux = Opt_stab()->Aux_stab_entry( cr->Aux_id() );
    aux->Set_home_sym(home_auxid);
    Opt_stab()->Aux_stab_entry(home_auxid)->Set_home_sym(cr->Aux_id());
    // get sign_extd flag from variable's coderep node
    if (cr->Is_sign_extd())
      wk->Set_sign_extd();
  }
  else { // get sign_extd flag from preg's coderep node
    if (Opt_stab()->Aux_stab_entry(aux->Home_sym())->LPRE_sign_extd())
      wk->Set_sign_extd();
  }
  wk->Set_preg(aux->Home_sym());
  if (wk->Iphi_bbs() == NULL) {
    wk->Set_iphi_bbs(CXX_NEW(BB_NODE_SET(Cfg()->Last_bb_id()+1, Cfg(),
			     Loc_pool(), BBNS_EMPTY), Loc_pool()));
    wk->Set_temp_id(0);		// initialize it
  }
  return wk;
}

// =====================================================================
// Insert an iphi at the post-dominance frontiers of bb if it is not yet 
// inserted, by checking the bit vector Iphi_bbs().  After insertion, 
// recursively insert at succesive post-dominance frontiers.
// =====================================================================
void SSU::Insert_iphis_recursive(EXP_WORKLST *wk, BB_NODE *bb)
{
  EXP_OCCURS *iphi;
  BB_NODE *bby;
  BB_NODE_SET_ITER bns_iter;

  if (wk->Temp_id() == bb->Id()) 
    return; // already processed

  //  Go through the post dominance frontiers of bb
  FOR_ALL_ELEM (bby, bns_iter, Init(bb->Rcfg_dom_frontier())) {
    if (wk->Iphi_bbs()->MemberP(bby)) 
      continue;
    wk->Iphi_bbs()->Union1D(bby);
    iphi = Etable()->New_phi_occurrence(wk, Mem_pool(), bby);
    iphi->Exp_phi()->Set_reverse_phi();
    bby->Iphi_list()->Append(iphi->Exp_phi());
    // recursive call
    Insert_iphis_recursive(wk, bby);
  }
}

// =====================================================================
// For the succ of iphibb that iphibb does not postdominate,
// set NULL_SSU_VERSION for the corresponding iphi operand.
// =====================================================================
inline void SSU::Make_non_postdominated_iphi_opnd_null(BB_NODE *iphibb,
						       EXP_PHI *iphi)
{
  BB_NODE *bbsucc;
  BB_LIST_ITER bb_list_iter;
  INT32 pos = 0;
  FOR_ALL_ELEM(bbsucc, bb_list_iter, Init(iphibb->Succ())) {
    if (! iphibb->Postdominates(bbsucc)) {
      iphi->Set_null_ssu_version(pos);
    }
    pos++;
  }
}

// =====================================================================
// v's use in usebb postdominates its def.  Check if there is any iphi 
// between the use and def.  If there is, set the NULL_SSU_VERSION flag for
// the appropriate iphi operands.  Return true if such an iphi exists.
// wk must not be NULL.
// =====================================================================
BOOL SSU::Find_intervening_iphi(EXP_WORKLST *wk, 
				CODEREP *v, 
				BB_NODE *usebb)
{
  if (wk == NULL)
    return FALSE;
  if (v->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *stmt = v->Defstmt();
    if (! OPERATOR_is_scalar_store (stmt->Opr()))
      return FALSE;
  }
  EXP_PHI *iphi;
  EXP_PHI_LIST_ITER iphi_iter;
  BB_NODE *bby;
  BB_NODE_SET_ITER bns_iter;
  BB_NODE *defbb = v->Defbb();
  BOOL found = FALSE;

  //  Go through the post dominance frontiers of defbb
  FOR_ALL_ELEM (bby, bns_iter, Init(defbb->Rcfg_dom_frontier())) {
    if (bby->Postdominates(defbb) && usebb->Postdominates(bby) && bby != usebb){
      if (wk->Iphi_bbs()->MemberP(bby)) {
        found = TRUE;
        // set iphi to the right iphi node
        FOR_ALL_NODE(iphi, iphi_iter, Init(bby->Iphi_list())) {
	  if (iphi->Result()->Spre_wk() == wk)
	    break;
        }
        Make_non_postdominated_iphi_opnd_null(bby, iphi);
      }
    }
  }
  return found;
}

// =====================================================================
// The phi is post-dominated by the use. Look at each of the predecessors
// of the phi. If there is an iphi, set NULL_SSU_VERSION for the appropriate
// iphi operand; otherwise, call Make_diff_ssu_version for the operand.
// wk must not be NULL. It does not process only itself.
// =====================================================================
void SSU::Make_diff_ssu_version_at_phi(EXP_WORKLST *wk, 
				       BB_NODE *defbb, 
				       PHI_NODE *phi)
{
  BB_NODE *bbpred;
  BB_LIST_ITER bb_list_iter;
  EXP_PHI *iphi;
  EXP_PHI_LIST_ITER iphi_iter;
  CODEREP *opnd;
  POINTS_TO *points_to = Opt_stab()->Points_to(phi->Aux_id());
  INT32 pos = 0;
  phi->Set_null_ssu_processed();
  FOR_ALL_ELEM(bbpred, bb_list_iter, Init(defbb->Pred())) {
    if (wk->Iphi_bbs()->MemberP(bbpred)) {
      // if bbpred has an iphi, set the null_ssu_version flag;
      // do the same for iphi's aliased with it
      INT32 pos2 = bbpred->Succ()->Pos(defbb);

      FOR_ALL_NODE(iphi, iphi_iter, Init(bbpred->Iphi_list())) {
	EXP_WORKLST *wk2 = iphi->Result()->Spre_wk();
	if (wk2 == wk) 
	  iphi->Set_null_ssu_version(pos2);
	else {
	  POINTS_TO *iphi_points_to = Opt_stab()->Points_to(wk2->Exp()->Aux_id());
	  if (Opt_stab()->Rule()->Aliased_Memop_By_Analysis(points_to,
							    iphi_points_to))
	    iphi->Set_null_ssu_version(pos2);
	}
      }
    }
    else { // call recursively
      opnd = phi->OPND(pos);
      Make_diff_ssu_version(wk, opnd, bbpred, FALSE);
    }
    pos++;
  }
}

// =====================================================================
// Make sure wk has an iphi in the BB given by iphibb
// =====================================================================
void SSU::Check_iphi_presence(EXP_WORKLST *wk, BB_NODE *iphibb)
{
  if (! wk->Iphi_bbs()->MemberP(iphibb)) {
    // if iphi not yet inserted, insert it
    wk->Iphi_bbs()->Union1D(iphibb);
    EXP_OCCURS *iphiocc = Etable()->New_phi_occurrence(wk, Mem_pool(), iphibb);
    EXP_PHI *iphi = iphiocc->Exp_phi();
    iphi->Set_reverse_phi();
    iphibb->Iphi_list()->Append(iphi);
    // this new iphi causes more iphis to be inserted
    Insert_iphis_recursive(wk, iphibb);
  }
}

// =====================================================================
// iphibb is in the dominance frontier of usebb; set the appropriate operand
// of the iphi of the set of worklist given by e_num_set to NULL; if the iphi 
// is not there, insert it.
// =====================================================================
void SSU::Make_null_ssu_version_in_iphi_for_e_num_set(BB_NODE *iphibb,
						      BB_NODE *usebb)
{
  EXP_PHI *iphi;
  EXP_PHI_LIST_ITER  iphi_iter;
  EXP_WORKLST *wk;

  BB_NODE *bbsucc;
  BB_LIST_ITER bb_list_iter;
  INT32 pos = 0;
  // find the succ bb post-dominated by usebb (may be more than 1)
  FOR_ALL_ELEM(bbsucc, bb_list_iter, Init(iphibb->Succ())) {
    if (usebb->Postdominates(bbsucc)) {
      // set the null_ssu_version flag in the corresponding iphi result for
      // the members of _e_num_set
      FOR_ALL_NODE(iphi, iphi_iter, Init(iphibb->Iphi_list())) {
	wk = iphi->Result()->Spre_wk();
	if (_e_num_set->MemberP(wk->E_num())) {
	  iphi->Set_null_ssu_version(pos);
#ifdef Is_True_On
	  if (Get_Trace(TP_GLOBOPT, SPRE_DUMP_FLAG))
	    fprintf(TFile,
		    "SSU: E_num %d iphi at BB%d pos %d made null ssu version\n",
		    wk->E_num(), iphibb->Id(), pos);
#endif
	}
      }
    }
    pos++;
  }
}

// =====================================================================
// set the DIFF_SSU_VERSION flag at either a real store or the iphi result
// that is post-dominated by usebb.  If usebb does not post-dominates v's 
// Defbb(), then there must be iphi in between, so we can just set the flag
// for the appropriate iphi result and quit. If usebb post-dominates v's
// Defbb(), and v is defined by phi or chi, call recursively for each operand 
// of the phi or chi.  If wk is NULL, the variable itself is not an SPRE
// candidate, so only need to apply to aliased variables and continue up its
// u-d chain.  If only_itself is TRUE, do not apply to aliased variables. 
// =====================================================================
void SSU::Make_diff_ssu_version(EXP_WORKLST *wk, 
				CODEREP *v, 
				BB_NODE *usebb,
				BOOL only_itself)
{
  STMTREP *stmt;
  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter;
  EXP_WORKLST *wk2;

  if (v->Is_flag_set(CF_IS_ZERO_VERSION))
    return;
  BB_NODE *defbb = v->Defbb();
  if (defbb == NULL) {
    Is_True(v->Is_var_volatile(),
	    ("SSU::Make_diff_ssu_version: variable has no def"));
    return;
  }
  if (usebb->Postdominates(defbb) || wk == NULL) {
    if (v->Is_flag_set(CF_DEF_BY_PHI)) {
      PHI_NODE *phi = v->Defphi();

      if (wk == NULL) { // just continue up the u-d chain
	if (! phi->Null_ssu_processed()) {
	  phi->Set_null_ssu_processed();

	  BB_LIST_ITER bb_list_iter;
	  BB_NODE *bbpred;
	  CODEREP *opnd;
	  INT32 pos = 0;
	  FOR_ALL_ELEM(bbpred, bb_list_iter, Init(defbb->Pred())) {
	    opnd = phi->OPND(pos);
	    Make_diff_ssu_version(NULL, opnd, bbpred, FALSE);
	    pos++;
	  }
	}
	return;
      }

      if (! phi->Null_ssu_processed() && 
	  ! Find_intervening_iphi(wk, v, usebb))
        Make_diff_ssu_version_at_phi(wk, defbb, phi);
    }
    else if (v->Is_flag_set(CF_DEF_BY_CHI)) {
      stmt = v->Defstmt();
      if (OPERATOR_is_scalar_store (stmt->Opr()) && 
	  ! stmt->Is_diff_ssu_version()) {
        CODEREP *lhs = stmt->Lhs();
        wk2 = Opt_stab()->Aux_stab_entry(lhs->Aux_id())->Spre_node();
        if (! Find_intervening_iphi(wk2, lhs, usebb))
	  stmt->Set_diff_ssu_version();
      }
      BOOL first_one = TRUE;
      FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
	if (! only_itself || only_itself && cnode->RESULT() == v) {
	  if (cnode->Ssu_processed() || ! cnode->Live() ||
	      cnode->Aux_id() == Opt_stab()->Return_vsym() || // ignore return vsym
	      cnode->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION) ||
	      cnode->RESULT()->Is_flag_set(CF_INCOMPLETE_USES))
	    continue;
	  wk2 = Opt_stab()->Aux_stab_entry(cnode->Aux_id())->Spre_node();
	  if (wk2 == NULL) {
	    // continue up the u-d chain 
	    cnode->Set_ssu_processed(TRUE);
	    Make_diff_ssu_version(NULL, cnode->OPND(), defbb, ! first_one);
	    first_one = FALSE;
	  }
	  else if (! Find_intervening_iphi(wk2, cnode->RESULT(), usebb)) {
	    cnode->Set_ssu_processed(TRUE);
	    // call recursively for the chi operand
	    Make_diff_ssu_version(wk2, cnode->OPND(), defbb, ! first_one);
	    first_one = FALSE;
	  }
	  if (only_itself)
	    break;
	}
      }
    }
    else { // defined by a real store
      stmt = v->Defstmt();
      if (wk != NULL && ! stmt->Is_iphi_inserted()) {
        Insert_iphis_recursive(wk, defbb);
        stmt->Set_iphi_inserted();
      }
      if (! stmt->Is_diff_ssu_version() &&
	  ! Find_intervening_iphi(wk, v, usebb)) 
        stmt->Set_diff_ssu_version();
      if (! only_itself) {
	BOOL first_one = TRUE;
        FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
	  if (cnode->Ssu_processed() || ! cnode->Live() ||
	      cnode->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION) ||
	      cnode->RESULT()->Is_flag_set(CF_INCOMPLETE_USES) ||
	      Opt_stab()->Is_virtual(cnode->Aux_id()) &&
	      ! Opt_stab()->Is_real_var(cnode->Aux_id()))
	    continue;
	  wk2 = Opt_stab()->Aux_stab_entry(cnode->Aux_id())->Spre_node();
	  if (! Find_intervening_iphi(wk2, cnode->RESULT(), usebb)) {
	    cnode->Set_ssu_processed(TRUE);
	    // call recursively for the chi operand
	    Make_diff_ssu_version(wk2, cnode->OPND(), defbb, ! first_one);
	    first_one = FALSE;
	  }
	}
      }
    }
  }
  else { // there must be intervening iphi(s)
    if  (_make_diff_ssu_version_called_in_bb[usebb->Id()]->MemberP(v->Aux_id()))
      return; // already processed previously

    _e_num_set->ClearD();
    // _e_num_set will give the worklist candidates whose iphi operands
    // postdominated by usebb will be marked null_ssu_version
    BB_NODE *iphibb;
    BB_NODE_SET_ITER bns_iter;
    if (wk != NULL) {
      _e_num_set->Union1D(wk->E_num());
      // go through the post dominance frontiers of usebb
      FOR_ALL_ELEM(iphibb, bns_iter, Init(usebb->Rcfg_dom_frontier()))
	Check_iphi_presence(wk, iphibb);
    }

    if (only_itself || wk == NULL || v->Points_to(Opt_stab())->No_alias())
      ;
    else {
      AUX_ID st_idx = v->Aux_id();
      AUX_STAB_ENTRY *aux= Opt_stab()->Aux_stab_entry(st_idx);

      if (aux->Is_real_var()) {
	AUX_ID cur_idx = aux->St_group();
	// loop through the variables aliased with it
	while (cur_idx && cur_idx != st_idx) {
	  aux = Opt_stab()->Aux_stab_entry(cur_idx);
	  wk2 = aux->Spre_node();
	  if (wk2 != NULL) {
            _e_num_set->Union1D(wk2->E_num());
	    _make_diff_ssu_version_called_in_bb[usebb->Id()]->Union1D(cur_idx);
	    FOR_ALL_ELEM(iphibb, bns_iter, Init(usebb->Rcfg_dom_frontier()))
	      Check_iphi_presence(wk2, iphibb);
	  }
	  cur_idx = aux->St_group();
	}
      }

      if (aux->Is_virtual() && aux->Aux_id_list() != NULL) {
	AUX_ID_LIST_ITER id_list_iter;
	AUX_ID_NODE *id_node;
	FOR_ALL_ELEM(id_node, id_list_iter, Init(aux->Aux_id_list())) {
	  if ( (IDX_32)(id_node->Aux_id()) != ILLEGAL_BP ) {
	    aux = Opt_stab()->Aux_stab_entry(id_node->Aux_id());
	    wk2 = aux->Spre_node();
	    if (wk2 != NULL) {
              _e_num_set->Union1D(wk2->E_num());
	      _make_diff_ssu_version_called_in_bb[usebb->Id()]->Union1D(id_node->Aux_id());
	      FOR_ALL_ELEM(iphibb, bns_iter, Init(usebb->Rcfg_dom_frontier()))
		Check_iphi_presence(wk2, iphibb);
	    }
	  }
	}
      }
      _make_diff_ssu_version_called_in_bb[usebb->Id()]->Union1D(st_idx);
    }

    // go through the post dominance frontiers of usebb and for each member
    // of _e_num_set, mark the iphi operands postdominated by usebb 
    // null_ssu_version
    FOR_ALL_ELEM(iphibb, bns_iter, Init(usebb->Rcfg_dom_frontier())) 
      Make_null_ssu_version_in_iphi_for_e_num_set(iphibb, usebb);
  }
}

// =====================================================================
// Traverse MU_LIST set the read bb
// =====================================================================
void SSU::Traverse_mu_read(MU_LIST *mu_list, BB_NODE *bb)
{
  Is_True(mu_list != NULL, ("SSU::Traverse_mu_read: Null mu_list ptr"));

  MU_LIST_ITER  mu_iter;
  MU_NODE      *mnode;
  
  FOR_ALL_NODE( mnode, mu_iter, Init(mu_list) ) {
    if (mnode->Aux_id() == Opt_stab()->Return_vsym()) // ignore return vsym
      continue;
    EXP_WORKLST *wk = SPRE_candidate(mnode->OPND());
    if (wk != NULL) {
      Make_diff_ssu_version(wk, mnode->OPND(), bb, FALSE);
      if (wk->Temp_id() != bb->Id() && 
	  ! mnode->OPND()->Is_flag_set(CF_IS_ZERO_VERSION)) 
	// mark it so won't process iphi insertion the 2nd time
	wk->Set_temp_id(bb->Id());
    }
    else Make_diff_ssu_version(NULL, mnode->OPND(), bb, FALSE);
  }
}

// =====================================================================
// Traverse CODEREP and collect the following info:
// Def: stores to a location (excluding CHI)
// Use: read from a location (including MU)
// not_preg_safe flag is used only if is_store is false; it is according to
// whether the use is of the form: preg = x;
// =====================================================================
void SSU::Traverse_cr_rw(CODEREP *cr, 
			 BB_NODE *bb, 
			 BOOL is_store)
{
  switch (cr->Kind()) {

  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    break;

  case CK_VAR:
    { EXP_WORKLST *wk = SPRE_candidate(cr);
      if (wk != NULL) {
        if (is_store && ! cr->Defstmt()->Is_iphi_inserted()) {
	  Insert_iphis_recursive(wk, bb);
	  cr->Defstmt()->Set_iphi_inserted();
	}
        if (! is_store)
	  Make_diff_ssu_version(wk, cr, bb, FALSE);
  
        if (wk->Temp_id() != bb->Id()) 
	  // mark it so won't process iphi insertion the 2nd time
	  wk->Set_temp_id(bb->Id());	
      }
      else if (! is_store && ! cr->Points_to(Opt_stab())->No_alias())
        Make_diff_ssu_version(NULL, cr, bb, FALSE);
      break;
    }

  case CK_IVAR:
    {
#ifdef KEY
      Traverse_cr_rw( ! is_store ? 
#else
      Traverse_cr_rw( cr->Ilod_base() ? 
#endif
		     cr->Ilod_base() : cr->Istr_base(), bb, FALSE);
      if ( cr->Opr() == OPR_MLOAD ) {
	Traverse_cr_rw( cr->Mload_size() ? 
		       cr->Mload_size() : cr->Mstore_size(), bb, FALSE);
      }
      if ( cr->Opr() == OPR_ILOADX ) {
	Traverse_cr_rw( cr->Index(), bb, FALSE);
      }
      if ( cr->Ivar_mu_node() != NULL ) {
	MU_LIST mu_list( cr->Ivar_mu_node() );
	Traverse_mu_read( &mu_list, bb );
      }
    }
    break;

  case CK_OP:
    {
      for ( INT32 i = 0; i < cr->Kid_count(); ++i ) {
	Traverse_cr_rw( cr->Opnd(i), bb, FALSE);
      }
    }
    break;
  default:
    Is_True(0, ("SSU::Traverse_cr_rw: unexpected kind 0x%x", cr->Kind()));
  }

}

// ===================================================================
// Traverse the program in pre-order dominator tree order. In this order,
// defs must be seen before uses, so that it is necessary to do processing
// if defs have not been seen, because there is nothing to move down.
// It inserts iphis at post-dominance frontiers of 
// defs (excluding CHIs and PHIs) and uses (including MUs).  For each use
// (including MUs), also set the diff_ssu_version flag for the store 
// or closest iphi-succ that it post-dominates.  If it is defined by a phi
// or chi, and there is no iphi in between, apply the setting of the
// diff_ssu_version flag recursively to each phi/chi operand.
// It traverses the program in pre-order traversal of dominator tree so that
// when it is at a use, it must have seen the def earlier; if at a use there
// is no worklist node created yet, it does not need to insert any iphi.
// ===================================================================
void SSU::Iphi_insertion(void)
{
  DPOBB_ITER     cfg_iter(Cfg(), TRUE); // pre-order traversal of dominator tree
  BB_NODE       *bb;
  EXP_WORKLST *wk;
  
  FOR_ALL_ELEM ( bb, cfg_iter, Init() ) {
    STMTREP_ITER   stmt_iter(bb->Stmtlist());
    STMTREP       *stmt;
    FOR_ALL_NODE ( stmt, stmt_iter, Init() ) {
      // Process MU_LIST
      if (stmt->Has_mu()) {
	MU_LIST *mu_list = stmt->Mu_list();
	if ( mu_list != NULL ) {
	  Traverse_mu_read( mu_list, bb );
	}
      }

      CODEREP *rhs = stmt->Rhs();
      CODEREP *lhs = stmt->Lhs();

      // RHS 
      if ( rhs ) 
	Traverse_cr_rw( stmt->Rhs(), bb, FALSE);

      // LHS 
      if ( lhs ) {
	Traverse_cr_rw( lhs, bb, TRUE);

	if (OPERATOR_is_scalar_store (stmt->Opr()))
	  stmt->Reset_RHS_saved_saved_RHS();
      }

      // Process CHI's whose results are zero version
      if (stmt->Has_chi()) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	CHI_LIST *chi_list = stmt->Chi_list();
        FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
          if (! cnode->Live() || 
	      ! (cnode->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION) ||
		 cnode->RESULT()->Is_flag_set(CF_INCOMPLETE_USES)))
	    continue;
	  stmt->Set_diff_ssu_version();
	  wk = SPRE_candidate(cnode->OPND());
	  Make_diff_ssu_version(wk, cnode->OPND(), bb, FALSE);
	  // cannot use TRUE (for only itself) because there may be a dead
	  // chi aliased with it not handled in this loop
	}
      }
    }

    // process phi operands in successor BBs whose results are zero versions
    BB_LIST_ITER bb_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM(succ, bb_iter, Init(bb->Succ())) {
      PHI_NODE *phi;
      PHI_LIST_ITER phi_iter;
      INT32 pos = succ->Pred()->Pos(bb);
      FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
        if (phi->Live() && 
	    (phi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION) ||
	     phi->RESULT()->Is_flag_set(CF_INCOMPLETE_USES))) {
          wk = SPRE_candidate(phi->OPND(pos));
	  Make_diff_ssu_version(wk, phi->OPND(pos), bb, FALSE);
        }
      }
    }
  }
}

// =====================================================================
// If a TOS is a phi-occur, reset its downsafe bit.
// =====================================================================
void
inline SSU::Reset_tos_downsafe(void)
{
  // Reset down-safe if the phi result does have any real use.
  // (if there is any real use, the stack top should be a real occur.)

  EXP_WORKLST *wk;
  EXP_WORKLST_ITER worklst_iter(Etable()->Exp_worklst());
  FOR_ALL_NODE(wk, worklst_iter, Init()) {
    if (! wk->Spre_stack()->Is_Empty() &&
	wk->Spre_stack()->Top()->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR ) 
      wk->Spre_stack()->Top()->Exp_phi()->Set_not_down_safe();
  }
}

// =====================================================================
// By following the iphi du chain, propagate the occurrence info to
// adjacent iphi's
// =====================================================================
void
SSU::Propagate_occurrences(EXP_OCCURS *iphi_occ, CODEREP *cr)
{
  EXP_PHI *iphi = iphi_occ->Exp_phi();
  for (INT opnd_num = 0; opnd_num < iphi->Opnd_count(); opnd_num++) {
    EXP_OCCURS *opnd = iphi->Opnd(opnd_num);
    if (opnd != NULL && opnd->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR &&
	opnd->Occurrence() == NULL) {
      opnd->Set_occurrence(cr);
      Propagate_occurrences(opnd, cr);
    }
  }
}

// =====================================================================
//  Rename the SSU form and create the occurrence nodes pointed to from
//  the worklist nodes
// =====================================================================
void SSU::Rename(BB_NODE *bb)
{
  BB_LIST_ITER bb_iter;
  BB_NODE *pred, *pdom_bb;
  BB_LIST_ITER pdom_bb_iter;
  INT32 pos;
  STMTREP           *stmt;
  STMTREP_ITER       stmt_iter(bb->Stmtlist());
  EXP_PHI           *iphi;
  EXP_PHI_LIST_ITER  iphi_iter;
  EXP_OCCURS        *occur;
  EXP_OCCURS 	    *tos;
  AUX_STAB_ENTRY    *psym;
  EXP_WORKLST       *wk;

  // iterate through each iphi node
  FOR_ALL_NODE(iphi, iphi_iter, Init(bb->Iphi_list())) {
    occur = iphi->Result();
    wk = occur->Spre_wk();
    if (wk->Spre_stack() != NULL) {
      // assign a new s version to the r.h.s. (single use) of the iphi
      occur->Set_e_version(wk->Cur_e_version());
      wk->New_e_version();
      // push the occurrence node onto the stack
      wk->Spre_stack()->Push(occur);
      // add to the appropriate occurrence list in the work list node
      wk->SPRE_append_occurrence(occur);
    }
  }


  // iterate through each statement (backward order)
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    // process the store
    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      psym = Opt_stab()->Aux_stab_entry(stmt->Lhs()->Aux_id());
      wk = psym->Spre_node();
      if (wk != NULL) {
        Is_True(stmt->Lhs()->Aux_id() == wk->Exp()->Aux_id(),
	        ("SSU::Rename: inconsistent aux_id found"));
	if (! wk->Spre_stack()->Is_Empty())
	  tos = wk->Spre_stack()->Top();
	else tos = NULL;
        // the iphi's occurrence field is updated
	if (tos != NULL && tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR &&
	    tos->Occurrence() == NULL)
	  tos->Set_occurrence(stmt->Lhs());
	if (stmt->Is_diff_ssu_version() || tos == NULL) {
	  // if TOS is iphi, reset its downsafe bit
	  if (tos != NULL && tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR)
	    tos->Exp_phi()->Set_not_down_safe();
	  // create occurrence node and push a new SSU version on the stack
	  occur = _etable->Append_spre_real_occurrence(stmt, wk);
	  wk->Spre_stack()->Push(occur);
	  // assign a new version
          occur->Set_e_version(wk->Cur_e_version());
          wk->New_e_version();
	}
	else {
	  // create occurrence node using the version on top of the stack
	  occur = _etable->Append_spre_real_occurrence(stmt, wk);
	  occur->Set_e_version(tos->E_version());
          occur->Set_def_occur(tos);

	  if (tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
	    wk->Spre_stack()->Push(occur); // push real occurrence 
          }
          else {
            // it is real occurrence, the tos node may have its
            // def_occur point to a phi occurrence node already
            if (tos->Def_occur() != NULL)
              occur->Set_def_occur(tos->Def_occur());
          }
	}
      }
    }

    if (stmt->Has_chi()) { // only purpose is update the current version at iphi
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
        psym = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
        wk = psym->Spre_node();
        if (wk == NULL) 
	  continue;
        // the iphi's occurrence field is updated
	if (! wk->Spre_stack()->Is_Empty()) {
	  // the iphi's occurrence field is updated
	  EXP_OCCURS *item = wk->Spre_stack()->Top();
	  if (item->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
	    if (cnode->Live() && item->Occurrence() == NULL)
	      item->Set_occurrence(cnode->RESULT());
	    // disable insertion for this iphi due to reaching chi def
	    item->Exp_phi()->Set_not_down_safe();
	    item->Exp_phi()->Set_dead_phi_region(); // to prevent aggcm from
						    // reversing it
	  }
	}
      }
    }
  }

  // go thru the SSA phis to update current version
  PHI_NODE *phi;
  PHI_LIST_ITER phi_iter;
  FOR_ALL_ELEM(phi, phi_iter, Init(bb->Phi_list())) {
    psym = Opt_stab()->Aux_stab_entry(phi->Aux_id());
    wk = psym->Spre_node();
    if (wk == NULL) 
      continue;
    if (! wk->Spre_stack()->Is_Empty()) {
      // the iphi's occurrence field is updated
      EXP_OCCURS *item = wk->Spre_stack()->Top();
      if (item->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
        if (! phi->Live() || 
	    phi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION) ||
	    phi->RESULT()->Is_flag_set(CF_INCOMPLETE_USES)) {
	  // disable insertion for this iphi due to possible incomplete u-d 
	  // chain
	  item->Exp_phi()->Set_not_down_safe();
	  item->Exp_phi()->Set_dead_phi_region();
        }
        else if (item->Occurrence() == NULL) {
	  item->Set_occurrence(phi->RESULT());
	  // if any phi operand is zero version, also set not down_safe
	  PHI_OPND_ITER phi_opnd_iter(phi);
	  CODEREP *opnd;
	  FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
	    if (opnd->Is_flag_set(CF_IS_ZERO_VERSION)) {
	      item->Exp_phi()->Set_not_down_safe();
	      break;
	    }
	  }
	}
      }
    }
  }

  if (bb == Cfg()->Fake_exit_bb()) {
  }
  else if (bb->Pred() == NULL) {
    // exit occurrence node (entry for SPRE) is useless in SPRE because
    // Rename traverses the CFG, not the occurrence nodes; in expression PRE,
    // only Rename needs it

    // Reset the not_down_safe bit for all the TOS iphi occurrences
    Reset_tos_downsafe();
  }
  else {
    // create iphi-succ occurrence nodes and rename iphi operands in predecessors
    FOR_ALL_ELEM(pred, bb_iter, Init(bb->Pred())) {
      pos = pred->Succ()->Pos(bb);
      FOR_ALL_NODE(iphi, iphi_iter, Init(pred->Iphi_list())) {
        occur = iphi->Result();
	wk = occur->Spre_wk();
	STACK<EXP_OCCURS*> *spre_stack = wk->Spre_stack();
	if (spre_stack != NULL) {
	  if (iphi->Null_ssu_version(pos)) {
	    if (! spre_stack->Is_Empty() &&
		spre_stack->Top()->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) 
	      spre_stack->Top()->Exp_phi()->Set_not_down_safe();
	  }
	  else {
	    // set the iphi operand
	    Is_True(occur->Exp_phi()->Opnd(pos) == NULL,
		    ("SSU::Rename: at iphi succ, iphi operand not null"));
	    if (! spre_stack->Is_Empty()) { 
	      occur->Exp_phi()->Set_opnd(pos, spre_stack->Top());
  	
	      // set the iphi 'pos'th iphi operand 'has_real_occ'
	      if (spre_stack->Top()->Occ_kind()==EXP_OCCURS::OCC_REAL_OCCUR) {
	        occur->Exp_phi()->Set_has_real_occ(pos);
	        if (spre_stack->Top()->Def_occur() != NULL)
		  occur->Exp_phi()->Set_opnd(pos, spre_stack->Top()->Def_occur());
	      }
	    } // otherwise, leave as NULL iphi operand
	  }
	}
      }
    }
  }
  
  // recursive calls
  FOR_ALL_ELEM(pdom_bb, pdom_bb_iter, Init(bb->Pdom_bbs())) {
    Rename(pdom_bb);
  }
  
  // pop off all nodes pushed in rename stacks due to this bb
  EXP_WORKLST_ITER worklst_iter(Etable()->Exp_worklst());
  FOR_ALL_NODE(wk, worklst_iter, Init()) {
    BOOL done = FALSE;
    while (! (done || wk->Spre_stack()->Is_Empty()) ) {
      occur = wk->Spre_stack()->Top();
      switch (occur->Occ_kind()) {
      case EXP_OCCURS::OCC_REAL_OCCUR:
	if (occur->Spre_store()->Bb() == bb)
          wk->Spre_stack()->Pop();
	else done = TRUE;
	break;
      case EXP_OCCURS::OCC_PHI_OCCUR:
	if (occur->Exp_phi()->Bb() == bb)
          wk->Spre_stack()->Pop();
	done = TRUE;	// there can only be one phi for each variable in BB
	break;
      default: Is_True(FALSE, ("SSU::Rename: bad item on spre rename stack"));
      }
    }
  }
}

// This is a predicate for Remove_if().
// It is overloaded with the processing of the deleted node, too! 
//
struct REMOVE_EMPTY_WORKLIST {
  OPT_STAB *_opt_stab;
  BOOL _trace;

  REMOVE_EMPTY_WORKLIST(OPT_STAB *opt_stab, BOOL trace) 
    { _opt_stab = opt_stab; _trace = trace; }

  BOOL operator()(EXP_WORKLST *wk)
  { 
    if (wk->Real_occurs().Head() == NULL) {
      Is_True(wk->Exp()->Kind() == CK_VAR, ("REMOVE_EMPTY_WORKLIST(): not CK_VAR."));
      AUX_STAB_ENTRY *psym = _opt_stab->Aux_stab_entry(wk->Exp()->Aux_id());

      Is_True(psym->Spre_node() == NULL || psym->Spre_node() == wk,
	      ("REMOVE_EMPTY_WORKLIST():  psym->Spre_node() is inconsistent."));

      Is_Trace(_trace, 
	       (TFile, "REMOVE_EMPTY_WORKLIST:  remove worklist for auxid %d\n", wk->Exp()->Aux_id()));
      psym->Set_spre_node(NULL);
      wk->Set_spre_stack(NULL);
      return TRUE;
    }
    return FALSE;
  }
};

// =====================================================================
//   Construct the SSU form.
// =====================================================================
void SSU::Construct(void)
{

  BB_NODE *bb;
  CFG_ITER cfg_iter;
  AUX_ID var;
  VER_ID du;
  AUX_STAB_ITER opt_stab_iter(Opt_stab());

  STMTREP *dummy_exit_stmt = CXX_NEW(STMTREP, Mem_pool());
  dummy_exit_stmt->Set_bb(Cfg()->Exit_bb());

  OPT_POOL_Push(Loc_pool(), SSA_DUMP_FLAG);
  {
    FOR_ALL_NODE(var, opt_stab_iter, Init()) {
      Opt_stab()->Aux_stab_entry(var)->Set_spre_node(NULL);
    }

    // For every BBs, setup the Iphi_list.
    FOR_ALL_ELEM ( bb, cfg_iter, Init(Cfg()) )
      bb->Set_iphi_list(CXX_NEW(EXP_PHI_LIST(bb->Succ()->Len()), 
				Mem_pool()));
    
    // Placement of iphi functions.
    SET_OPT_PHASE("SPRE: Iphi Insertion");
    Iphi_insertion();
  }
  OPT_POOL_Pop(Loc_pool(), SSA_DUMP_FLAG);

  Opt_stab()->Reset_def_bbs();

  OPT_POOL_Push(Loc_pool(), SSA_DUMP_FLAG);
  {
    SET_OPT_PHASE("SPRE: SSU Renaming");
    // Generate the default version for local variables; allocate rename-stack

    EXP_WORKLST *wk;
    EXP_WORKLST_ITER worklst_iter(_etable->Exp_worklst());
    // Iterate through all the SPRE candidates
    FOR_ALL_NODE(wk, worklst_iter, Init()) {
      wk->Set_spre_stack(CXX_NEW(STACK<EXP_OCCURS*>(Loc_pool()), Loc_pool()));
      AUX_STAB_ENTRY *psym = Opt_stab()->Aux_stab_entry(wk->Exp()->Aux_id());
      if (psym->Points_to()->Local()) {// for locals, assign a version at exit
        EXP_OCCURS *occur =
	  _etable->Append_spre_real_occurrence(dummy_exit_stmt, wk);
        wk->Spre_stack()->Push(occur);
        occur->Set_fake_store();
        // assign a new version
        occur->Set_e_version(wk->Cur_e_version());
        wk->New_e_version();
      }
      // else, leave stack empty at exit
    } 

    //  Rename into SSU names.
    Rename(_cfg->Exit_bb());

    // propagate occurrences through the iphi's
    EXP_OCCURS *occur;
    EXP_OCCURS_ITER phi_occur_iter;
    FOR_ALL_NODE(wk, worklst_iter, Init(_etable->Exp_worklst())) {
      FOR_ALL_NODE(occur, phi_occur_iter, Init(wk->Phi_occurs().Head())) {
	if (occur->Occurrence() != NULL)
	  Propagate_occurrences(occur, occur->Occurrence());
      }
    }

    //  the rename stack must be either empty or has 1 element if local var
  }

  // Prune the worklist
  {
    REMOVE_EMPTY_WORKLIST predicate(Opt_stab(), Get_Trace(TP_GLOBOPT, SPRE_DUMP_FLAG));
    Remove_if(*_etable->Exp_worklst(), predicate);
  }

  OPT_POOL_Pop(Loc_pool(), SSA_DUMP_FLAG);
  
}
