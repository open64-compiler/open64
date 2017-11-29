/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_rename.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rename.cxx,v $
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


#include "opt_transform.h"
#include "bb_node_set.h"

#ifdef Is_True_On
static int transform_stop_bb;
static int transform_stop_stmt;
void set_transform_stop_at(int bb_id, int stmt_id) 
{
  transform_stop_bb = bb_id;
  transform_stop_stmt = stmt_id;
}

void transform_stopped()
{
  printf("transform stopped at bb%d stmt%d\n", transform_stop_bb, transform_stop_stmt);
}

void check_transform_stop_at(int bb_id, int stmt_id)
{
  if (bb_id == transform_stop_bb &&
      stmt_id == transform_stop_stmt) {
    transform_stopped();
  }
}
#endif

//  Insert_var_phi
//   -- mark required phi function visited and introduce new phi functions
//  
static void
Insert_var_phi(CODEREP *lhs, BB_NODE *bb, COMP_UNIT *cu, BOOL trace)
{
  AUX_ID aux = lhs->Aux_id();
  BB_NODE_SET_ITER    df_iter;
  BB_NODE *bb_phi;
  CODEMAP *htable = cu->Htable();

  FOR_ALL_ELEM (bb_phi, df_iter, Init(bb->Dom_frontier())) {
    PHI_NODE *phi = htable->Lookup_var_phi(bb_phi, aux);

    if (phi == NULL) {
      phi = bb_phi->Phi_list()->New_phi_node(aux, cu->Ssa()->Mem_pool(), bb_phi);
      
      // Enter into the var phi hash table.
      htable->Enter_var_phi_hash(phi);

      CODEREP *phi_res = htable->Add_def(aux, -1, 
					 NULL, lhs->Dtyp(), lhs->Dsctyp(),
					 lhs->Offset(), lhs->Lod_ty(),
					 lhs->Field_id(), TRUE);
      phi_res->Set_flag(CF_DEF_BY_PHI);
      phi_res->Set_defphi(phi);
      phi->Set_live();
      phi->Set_result(phi_res);
      phi->Set_visited();
      phi->Set_res_is_cr();

      Is_Trace(trace,
	       (TFile, "SSA RENAME: insert new phi at BB%d for sym%d\n", bb->Id(),
		aux));
      
      // Put zero versions at the phi opnds.
      CODEREP *zcr = cu->Ssa()->Get_zero_version_CR(aux, cu->Opt_stab(), 0);
      BB_NODE *pred;
      BB_LIST_ITER bb_iter;
      INT opnd = 0;
      FOR_ALL_ELEM (pred, bb_iter, Init(bb_phi->Pred())) {
	phi->Set_opnd(opnd, zcr);
	opnd++;
      }
      
      // Recursively introduce phi nodes
      Insert_var_phi(lhs, bb_phi, cu, trace);

    } else if (!phi->Visited()) {

      phi->Set_visited(); 

      // Recursively introduce phi nodes
      Insert_var_phi(lhs, bb_phi, cu, trace);
    }
  }
}


static void
delete_var_phi(PHI_NODE *phi, CODEMAP *htable, BOOL trace)
{
  Is_Trace(trace,
	   (TFile, "SSA RENAME: remove phi at BB%d for sym%d\n", phi->Bb()->Id(),
	    phi->Aux_id()));
  
  // This phi function will be removed, the use of the phi result
  // will becomes one of its operands.   And the operand must not
  // be a zero-cr if the phi result isn't a zero version.
  if (phi->Live() &&
      !phi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION)) {
    for ( INT pkid = 0; pkid < phi->Size(); pkid++ ) {
      if (phi->OPND(pkid)->Is_flag_set(CF_IS_ZERO_VERSION)) {
	htable->Fix_zero_version(phi, pkid, true /*allow_real_def*/);
      }
      phi->OPND(pkid)->Reset_flag(CF_DONT_PROP);
    }
  }
  htable->Remove_var_phi_hash(phi);
}


//  Insert_delete_phi
//   1. delete phi functions from BB that has a different number of predecessors
//   2. mark all phi functions not visited
//   3. insert new phi functions and mark required old phi visited
//   4. delete phi that are not visited
//      if the deleted phi has zero version, change the opnd to be
//      non-zero versions.
//
static void
Insert_delete_phi(COMP_UNIT *cu, BOOL trace)
{
  BB_NODE *bb;
  CFG *cfg = cu->Cfg();
  CODEMAP *htable = cu->Htable();
  CFG_ITER cfg_iter;

  // Delete phi from BB that has a different number of pred.  For those
  // not deleted, mark them not visited
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {
    PHI_NODE *phi; 
    PHI_LIST_ITER phi_iter;
    if (bb->Phi_list() != NULL) {
      if (bb->Phi_list()->In_degree() != bb->Pred()->Len()) {
	if (trace) {
	  fprintf(TFile, "SSA rename: detect all phi nodes from BB%d "
		  "because of the number of pred has changed.\n", bb->Id());
	}

	PHI_NODE *next_phi;
	for (PHI_NODE *phi = bb->Phi_list()->Head();
	     phi != NULL;
	     phi = next_phi) {
	  next_phi = phi->Next();
	  delete_var_phi(phi, htable, trace);
	}
	bb->Phi_list()->Set_Head(NULL);
	bb->Phi_list()->Set_Tail(NULL);
	bb->Phi_list()->Set_in_degree(bb->Pred()->Len());
      } else {
	FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
	  phi->Reset_visited();
	}
      }
    }
  }

  // Run classical phi insertion algorithm to mark required phis.
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (OPERATOR_is_scalar_store (stmt->Opr())) {
	CODEREP *lhs = stmt->Lhs();
	if (!lhs->Is_var_volatile())
	  Insert_var_phi(lhs, bb, cu, trace);
      }
      if (stmt->Has_chi()) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	CHI_LIST *chi_list = stmt->Chi_list();
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  CODEREP *lhs = cnode->Live() ? cnode->RESULT() : 
	    cu->Ssa()->Get_zero_version_CR(cnode->Aux_id(), cu->Opt_stab(), 0);
	  if (!lhs->Is_var_volatile())
	    Insert_var_phi(lhs, bb, cu, trace);
	}
      }
    }
  }

  // Delete phi that are not 'visited' by 'insert_var_phi'
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {
    PHI_NODE *first = NULL;
    PHI_NODE *last = NULL;
    PHI_NODE *next_phi;
    if (bb->Phi_list() != NULL) {
      for (PHI_NODE *phi = bb->Phi_list()->Head();
	   phi != NULL;
	   phi = next_phi) {
	if (phi->Visited()) {
	  if (first == NULL) first = phi;
	  last = phi;
	  next_phi = phi->Next();
	  phi->Reset_visited();
	} else {
	  next_phi = phi->Next();
	  delete_var_phi(phi, htable, trace);
	  if (last != NULL)
	    last->Set_Next(next_phi);
	  phi->Set_Next(NULL);
	}
      }
      bb->Phi_list()->Set_Head(first);
      bb->Phi_list()->Set_Tail(last);
    }
  }
}

static TRACK_CUR_VERSION *debug_rename_stack;

extern "C" {
void Print_version_stack(AUX_ID id, FILE *fp);
}

void Print_version_stack(AUX_ID id, FILE *fp) {
  STACK<CODEREP*> *s = debug_rename_stack->_vec_of_stack[id];
  for (int i = 0; i < s->Elements(); ++i) {
    CODEREP *top = s->Top_nth(i);
    if (top)
      fprintf(fp, "cr%d ", top->Coderep_id());
    else
      fprintf(fp, "null ");
  }
  fprintf(fp,"\n");
}


//  Apply SSA Rename on CODEREPs 

struct SSA_RENAME : public NULL_TRANSFORM {
  COMP_UNIT         *cu;
  TRACK_CUR_VERSION *rename_stack;

  SSA_RENAME(COMP_UNIT *comp_unit): 
    NULL_TRANSFORM(), cu(comp_unit), rename_stack(NULL)
  {}
  
  const char *Name() const { return "SSA RENAME"; }
  CODEREP *cur_version(AUX_ID id, CODEREP *cr) const {
    if (!cr->Is_var_volatile())
      if (!rename_stack->Is_volatile(id))   // See opt_verify.cxx for comments
	if (rename_stack->Size(id) == 0)
	  // Use without def (e.g. zero version phi opnd introduced during
	  // Insert_var_phi()
	  //
	  return cu->Ssa()->Get_zero_version_CR(id, cu->Opt_stab(), 0);
	else
	  return rename_stack->Top(id);
    return cr;
  }
  CODEREP *non_zero_cur_version(AUX_ID id, CODEREP *cr) const {
    if (!cr->Is_var_volatile())
      if (!rename_stack->Is_volatile(id))   // See opt_verify.cxx for comments
	return rename_stack->Non_zero_top(id);
    return cr;
  }
  void Print_stack(AUX_ID id, FILE *fp) {
    rename_stack->Print_stack(id, fp);
  }
  CODEREP *Apply_cr(CODEREP *cr, bool is_mu, STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) const 
  {
    if (cr->Kind() == CK_VAR) {
      CODEREP *tmp = cur_version(cr->Aux_id(), cr);
      if (is_mu && tmp->Is_flag_set(CF_IS_ZERO_VERSION)) {
	tmp = non_zero_cur_version(cr->Aux_id(), cr);
	Is_True(tmp != NULL, ("Second rename: cannot find non-zero version."));
      }
      if (tmp->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI|CF_DEF_BY_PHI))) {
        tmp->Set_field_id(cr->Field_id());
        tmp->Set_lod_ty(cr->Lod_ty());
#ifdef KEY
	if (cr->Dtyp() != MTYPE_UNKNOWN)
#endif
          tmp->Set_dtyp(cr->Dtyp());
#ifdef KEY
	if (cr->Dsctyp() != MTYPE_UNKNOWN)
#endif
          tmp->Set_dsctyp(cr->Dsctyp());
#ifdef KEY
	tmp->Set_sign_extension_flag(); // cr could be a non-real occurence node
	                   // being converted to real the first time, so its 
			   // sign_extd flag has never been set before
#endif
      }
      if (cr->Dsctyp() == MTYPE_BS) 
        tmp->Set_offset(cr->Offset());  // cannot use offset out of opt_stab
      return ((tmp != cr) ? 
	      (is_mu || cr->Dsctyp() == MTYPE_BS ? 
				tmp : 
				cr->Convert_type(htable, tmp, FALSE)) :
	      NULL);
    }
    if (inCODEKIND(cr->Kind(), CK_IVAR|CK_OP)) {
      // do not reset CF_C_P_PROCESSED, i.e. same as CF_LDA_LABEL, for CK_LDA  
      cr->Reset_flag(CF_C_P_PROCESSED);
    }
    //  CF_C_P_REHASHED is same as CF_DONT_PROP
    cr->Reset_flag(CF_DONT_PROP);
    if (is_mu)
      cr->Set_flag(CF_DONT_PROP);
    return NULL;
  }
  void Apply_sr(STMTREP *sr, BB_NODE *bb, CODEMAP *htable) const
  {
    if (sr->Has_mu()) {
      MU_LIST_ITER mu_iter;
      MU_NODE *mnode;
      FOR_ALL_NODE( mnode, mu_iter, Init(sr->Mu_list())) {
	CODEREP *cur = cur_version(mnode->Aux_id(), mnode->OPND());
	mnode->Set_OPND(cur);
	cur->Set_flag(CF_DONT_PROP);
      }
    }
    if (sr->Has_chi() && sr->Opr() != OPR_OPT_CHI) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = sr->Chi_list();
      if (OPERATOR_is_scalar_istore (sr->Opr())) {
	AUX_ID aux = sr->Lhs()->Ivar_occ()->Aux_id();
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (!cnode->Dse_dead()) {
	    CODEREP *cur = cur_version(cnode->Aux_id(), cnode->OPND());	
	    cnode->Set_OPND(cur);
	    if (cnode->Live())
	      cur->Set_flag(CF_DONT_PROP);
	    if (cnode->Aux_id() == aux && sr->Lhs()->Ivar_mu_node())
	      sr->Lhs()->Ivar_mu_node()->Set_OPND(cnode->RESULT());
	  }
	}
      } else {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (!cnode->Dse_dead()) {
	    CODEREP *cur = cur_version(cnode->Aux_id(), cnode->OPND());	
	    cnode->Set_OPND(cur);
	    if (cnode->Live())
	      cur->Set_flag(CF_DONT_PROP);
	  }
	}
      }
    }
  }
  void Apply_bb_post(BB_NODE *bb, CODEMAP *htable) const 
  {
    PHI_NODE *phi;
    PHI_LIST_ITER phi_iter;
    BB_NODE *succ; BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
      INT32 pos = succ->Pred()->Pos(bb);
      FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
	if (phi->Live()) {
	  CODEREP *cur = cur_version(phi->Aux_id(), phi->OPND(pos));
	  phi->Set_opnd(pos, cur);
	  cur->Set_flag(CF_DONT_PROP);
	}
      }
    }
  }
  // Setup control which combinations are legal
  void Setup(PER_SR_CACHE *, TRACK_CUR_VERSION *ver) {
    rename_stack = ver;
    debug_rename_stack = rename_stack;
  }
};


//  Apply SSA rename to the CODEMAP.
//    Insert and delete phi function as needed
//
void
Rename_CODEMAP(COMP_UNIT *cu)
{
  BOOL trace = Get_Trace(TP_WOPT2, SECOND_RENAME_FLAG);
  Insert_delete_phi(cu, trace);

  SSA_RENAME ssa_rename(cu);
  UPDATE<SSA_RENAME, PER_SR_CACHE, TRACK_CUR_VERSION>
    UPDATE_ssa(cu, &ssa_rename, trace);
  UPDATE_ssa.Process_PU();

  if (trace) {
    fprintf( TFile, "%sAfter COMP_UNIT::Rename_CODEMAP\n%s",
	     DBar, DBar );
    cu->Cfg()->Print(TFile);
  }
}
