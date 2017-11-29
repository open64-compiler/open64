/*
 *  Copyright (C) 2006. 2007 QLogic Corporation. All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
//
// Module: opt_lftr2.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_lftr2.cxx,v $
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


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_lftr2_CXX   "opt_lftr2.cxx"
static char *rcs_id =   opt_lftr2_CXX"$Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#include "opt_lftr2.h"		// LFTR class
#include "config_wopt.h"	// WOPT_Enable_LFTR2
#include "opt_fold.h"		// FOLD class
#include "idx_32_set.h"

// ======================================================================
// LFTR Constructor
// ======================================================================
LFTR::LFTR(ETABLE *etable, CODEMAP *htable, CFG *cfg, mUINT32 hash_size)
{
  _lftr_on = WOPT_Enable_LFTR2 && etable->Pre_kind() == PK_EPRE;
  _exp_iter = NULL;		// set later once the iterator is created
  if (Lftr_on()) {
    OPT_POOL_Initialize(&_mem_pool, "PRE LFTR mem pool", FALSE, LFTR_FLAG);
    OPT_POOL_Push(&_mem_pool, LFTR_FLAG);

    _etable = etable;
    _htable = htable;
    _cfg = cfg;
    _trace = Get_Trace(TP_GLOBOPT, LFTR_FLAG);
    _num_substitutions = 0;
    _last_stmt = 0;
    _len = 0;

    // create hash table
    Alloc_hash_vec(hash_size);

    // init dynamic arrays
    _stmt_no.Set_Mem_Pool(&_mem_pool);	// stmt_id to stmt_no mapping

  } else {
    // clear out the stuff that matters
    _etable   = NULL;
    _htable   = NULL;
    _cfg      = NULL;
    _hash_vec = NULL;
    _trace    = FALSE;
  }
}

// ======================================================================
// LFTR Destructor
// ======================================================================
LFTR::~LFTR(void)
{
  if (Lftr_on()) {
    { // PPP - temporary code hack
      BB_NODE *bb;
      CFG_ITER cfg_iter(Cfg());
      FOR_ALL_NODE(bb, cfg_iter, Init()) {
	STMTREP *stmt;
	STMTREP_ITER stmt_iter(bb->Stmtlist());
	FOR_ALL_NODE(stmt, stmt_iter, Init())
	  stmt->Set_Bitpos(ILLEGAL_BP);
      }
    } // end of temporary hack

    // PPP - need to delete all comparison occurrence nodes

    Free_hash_vec(); // LFTR hash table
    _stmt_no.Free_array(); // _stmt_id to _stmt_no array
    OPT_POOL_Pop(&_mem_pool, LFTR_FLAG);
    OPT_POOL_Delete(&_mem_pool, LFTR_FLAG);
    Opt_tlog("LFTR2", 0, "comparisons substituted %d", Num_substitutions());
  }
}

// ======================================================================
// Allocate hash table
// ======================================================================
void
LFTR::Alloc_hash_vec(INT32 hash_size)
{
  _hash_size = hash_size;

  // hash table
  _hash_vec = CXX_NEW_ARRAY(LFTR_VAR *, _hash_size+1, &_mem_pool);
  if (_hash_vec == NULL)
    ErrMsg (EC_No_Mem, "LFTR::Alloc_hash_vec, hash_vec");
  BZERO(_hash_vec, sizeof(LFTR_VAR *) * (_hash_size+1));

  Is_Trace(Trace(),(TFile,"LFTR::Alloc_hash_vec(%d)\n",hash_size));
}

// ======================================================================
// Free hash table
// ======================================================================
void
LFTR::Free_hash_vec(void)
{
  Is_True(_hash_vec != NULL,("LFTR::Free_hash_vec, it's already been freed"));
  CXX_DELETE_ARRAY(_hash_vec, &_mem_pool);
  _hash_vec = NULL;
  Is_Trace(Trace(),(TFile,"LFTR::Free_hash_vec\n"));
}

// ======================================================================
// Find the appropriate LFTR node in the list (that matches lftr_var)
// calls LFTR_VAR::Find_comp_list
// ======================================================================
LFTR_VAR *
LFTR::Find_comp_list(AUX_ID lftr_var) // main entry point for hash fn.
{
  LFTR_VAR *ltmp = _hash_vec[Hash_lftr_var(lftr_var)];
  if (ltmp != NULL) {
    LFTR_VAR *tmp = ltmp->Find_comp_list(lftr_var);
    if (tmp != NULL) {
      Set_len(tmp->Size());
      return tmp;
    }
  }
  Set_len(0);
  return NULL;
}

// ======================================================================
// find the appropriate LFTR_VAR node in the list (that matches lftr_var)
// ======================================================================
LFTR_VAR *
LFTR_VAR::Find_comp_list(AUX_ID lftr_var)
{
  LFTR_VAR *ltmp;
  LFTR_VAR_ITER lftr_var_iter(this);
  FOR_ALL_NODE(ltmp, lftr_var_iter, Init()) {
    if (ltmp->Lftr_var() == lftr_var)
      return ltmp;
  }
  return NULL;
}

// ======================================================================
// clear all Def_occurs in this list
// ======================================================================
void
LFTR::Clear_def_occurs(EXP_WORKLST *exp_worklst)
{
  if (!Lftr_on())
    return;

  EXP_OCCURS_PAIR *comp_list = Exp_hash(exp_worklst);
  if (comp_list != NULL) {
    EXP_OCCURS *exp_occ;
    EXP_ALL_REAL_ITER exp_iter(NULL, comp_list);
    FOR_ALL_NODE(exp_occ, exp_iter, Init())
      exp_occ->Set_def_occur(NULL);
  }
}

// ======================================================================
// lftr_comparison, look for CRs <,>,<=,>=,==,!=,!
// Screen comparisons:
//	1) comparison is binary operator
//	2) LHS is LFTR var by itself
//	3) RHS is loop invariant if inside a loop
//	4) LHS is not invariant
//      6) LHS is not member of innermost loop's Lftr_non_candidates()
// [ 5) RHS is a constant if outside a loop (no longer perform if outside loop)]
// Not a factor:
//	1) LHS is an induction var if inside a loop - too strict
// ======================================================================
void
LFTR::Lftr_comparison(CODEREP *cr, STMTREP *stmt, INT32 stmt_kid_num)
{
  Is_True(Lftr_on(), ("LFTR::Lftr_comparison, LFTR not on"));
  Is_True(Is_comparison(cr), ("LFTR::Lftr_comparison, not a comparison"));
  if (cr->Kid_count() != 2)
    return;
  if (cr->Opnd(0)->Kind() == CK_OP && cr->Opnd(1)->Kind() == CK_OP)
    return; 

  // find the LFTR var
  AUX_ID lftr_var = Find_lftr_var_id(cr); // 1) and 2)
  if (lftr_var == ILLEGAL_AUX_ID) {
    Is_Trace(Trace(),
	 (TFile,"LFTR::Lftr_comparison, return 1, could not find lftr_var\n"));
    return;
  }

  CODEREP *lhs, *rhs;
  BB_NODE *bb = stmt->Bb();
  lhs = Set_lhs_and_rhs(cr,&rhs);
  const BB_LOOP *loop = Cfg()->Find_innermost_loop_contains( bb );
  if (loop && 
      loop->True_body_set()->MemberP(bb)) { // the comparison is inside a loop
    if (rhs && !loop->Invariant_cr(rhs)) {  // 3) RHS is not invariant
      Is_Trace(Trace(),
	   (TFile,"LFTR::Lftr_comparison, return 3 - RHS is not invariant\n"));
      return;
    }
    if (loop->Invariant_cr(lhs)) {          // 4) LHS is invariant
      Is_Trace(Trace(),
	       (TFile,"LFTR::Lftr_comparison, return 4 - LHS is invariant\n"));
      return;
    }
    if (loop->Lftr_non_candidates() &&
	loop->Lftr_non_candidates()->MemberP(lftr_var)) { 
      Is_Trace(Trace(),
	       (TFile,"LFTR::Lftr_comparison, return 6 - LHS is in Lftr_non_candidates\n"));
      return;
    }
    Is_Trace(Trace(),
	   (TFile,"LFTR::Lftr_comparison, found comparison inside a loop\n"));
  } else {		// comparison is outside a loop
    Is_Trace(Trace(),
	   (TFile,"LFTR::Lftr_comparison, found comparison outside a loop\n"));
    return;
  }

  // ok, we'll let this one through
  Is_Trace(Trace(),(TFile,"  lftr_var = "));
  Is_Trace_cmd(Trace(),lhs->Print(0,TFile));
  Is_Trace(Trace(),(TFile,"  rhs = "));
  Is_Trace_cmd(Trace(),rhs->Print(0,TFile));
    
  // hash and add to appropriate list
  Add_comparison(cr, lftr_var, stmt, stmt_kid_num);
}

// ======================================================================
// Add_comparison, add a comparison to the appropriate comparison list
//	1) hash comparison based on LFTR var
//	2) put comparison on comparison list for that LFTR var
//
// hash table:
//	each slot if for a different LFTR var
//	expressions like i < 10 and i < n will hash to same slot
// ======================================================================
void
LFTR::Add_comparison(CODEREP *cr, AUX_ID lftr_var, STMTREP *stmt,
		     INT32 stmt_kid_num)
{
  Is_True(lftr_var != ILLEGAL_AUX_ID,
	  ("LFTR::Add_comparison, lftr_var not valid"));
  Is_True(cr != NULL,("LFTR::Add_comparison, NULL CR"));
  IDX_32 hash_index = Hash_lftr_var(lftr_var);

  // now add comparison to slot
  LFTR_VAR *comp_list;
  if (_hash_vec[hash_index] == NULL) { // no lists in this slot yet
    Is_Trace(Trace(),
	     (TFile,"LFTR::Add_comparison, adding to NULL list %d\n",
	      hash_index));
    Is_Trace_cmd(Trace(),stmt->Print(TFile));
    // create a LFTR_VAR head node
    _hash_vec[hash_index] = comp_list = CXX_NEW(LFTR_VAR(lftr_var),&_mem_pool);
  } else { // need to find right list
    LFTR_VAR_ITER lftr_var_iter(_hash_vec[hash_index]);
    FOR_ALL_NODE(comp_list, lftr_var_iter, Init()) { // find the right list
      if (comp_list->Lftr_var() == lftr_var)
	break;
    }
    if (comp_list == NULL) { // need to create another list
      comp_list = CXX_NEW(LFTR_VAR(lftr_var),&_mem_pool);
      Is_Trace(Trace(),
	       (TFile,"LFTR::Add_comparison, creating comparison list %d\n",
		hash_index));
      Is_Trace_cmd(Trace(),stmt->Print(TFile));
      _hash_vec[hash_index]->Insert_Before(comp_list);
      _hash_vec[hash_index] = comp_list;
    } else { // found an existing list of the same LFTR var
      Is_Trace(Trace(),
	       (TFile,"LFTR::Add_comparison, adding to comparison list %d\n",
		hash_index));
      Is_Trace_cmd(Trace(),stmt->Print(TFile));
    }
  }
  Is_True(comp_list != NULL, ("LFTR::Add_comparison, NULL LFTR_VAR"));

  comp_list->Add_comparison(cr, stmt, stmt_kid_num, &_mem_pool, Trace());
}

// there is an Add_comparison in the LFTR class also
// Add_comparison to this LFTR_VAR list, maintain DPO order
// TODO: this could be made faster by always appending to the end in Step 1
//	 and only do the DPO order search in Step 6.
void
LFTR_VAR::Add_comparison(CODEREP  *cr,
			 STMTREP  *stmt,
			 INT32     stmt_kid_num,
			 MEM_POOL *mem_pool,
			 BOOL      trace)
{
  BB_NODE *bb = stmt->Bb();
  EXP_OCCURS *stmt_first_comp = NULL;	// First COMP occur in this
					// kid of *stmt
  Is_Trace(trace, (TFile, "LFTR_VAR::Add_comparison: checking for\n"));
  Is_Trace_cmd(trace, cr->Print(3, TFile));
  Is_Trace(trace, (TFile, "   in kid %d of\n", stmt_kid_num));
  Is_Trace_cmd(trace, stmt->Print(TFile));

  EXP_OCCURS *tmp_occ, *prev = NULL;
  EXP_OCCURS_ITER tmp_occ_iter;
  FOR_ALL_NODE(tmp_occ, tmp_occ_iter, Init(_comp_list)) {
    Is_Trace(trace, (TFile, " --- examining (stmt kid %d)\n",
		     tmp_occ->Stmt_kid_num()));
    Is_Trace_cmd(trace, tmp_occ->Print(TFile));
    if (bb->Dom_dfs_id() < tmp_occ->Bb()->Dom_dfs_id()) {
      Is_Trace(trace, (TFile, " ---   breaking (BB)\n"));
      break;
    }
    if (bb == tmp_occ->Bb() &&
	stmt->Stmt_order_less_or_equal(tmp_occ->Stmt())) {
      if (stmt == tmp_occ->Stmt()) {
	stmt_first_comp = tmp_occ;
      }
      Is_Trace(trace, (TFile, " ---   breaking (stmt) (%sNULL)\n",
		       (stmt_first_comp != NULL ? "non-" : "")));
      break;
    }
    prev = tmp_occ;
  }

  BOOL duplicate = FALSE;
  if (stmt_first_comp != NULL) {
    Is_Trace(trace, (TFile, "Checking for duplicates...\n"));
    // Go through the comparisons in this kid of this statement
    // and see whether any is a duplicate of the new comparison
    // we're about to enter.
    FOR_ALL_NODE(tmp_occ, tmp_occ_iter, Init(stmt_first_comp)) {
      if (duplicate || (tmp_occ->Stmt() != stmt)) {
	break;
      }
      if (tmp_occ->Stmt_kid_num() == stmt_kid_num &&
	  tmp_occ->Occurrence() == cr) {
	duplicate = TRUE;
      }
      if (duplicate || tmp_occ->Stmt() != stmt) {
	break;
      }
    }
  }
  if (!duplicate) {
    // This comparison is not a duplicate of one already in the
    // list.
    Is_Trace(trace, (TFile, "No duplicate found\n"));

    // new comparison occurrence (real occurrence)
    // never freed, popped when LFTR memory is popped
    EXP_OCCURS *new_occur = CXX_NEW(EXP_OCCURS(cr, stmt, NULL, TRUE),
				    mem_pool);
    new_occur->Set_kind(EXP_OCCURS::OCC_COMP_OCCUR);	// comparison occurrence
    new_occur->Set_stmt_kid_num(stmt_kid_num);
    if (prev != NULL) {		// insert in middle or end
      new_occur->Set_Next(prev->Next());
      prev->Set_Next(new_occur);
    }
    else {			// front of list
      new_occur->Set_Next(_comp_list);
      _comp_list = new_occur;
    }
    Inc_list_size(); // size of lftr_var list for fast iterator
  }
  else {
    Is_Trace(trace, (TFile, " --- duplicate; no insertion\n"));
  }
}

// there is an Add_comparison in the LFTR class also
// Add_comparison to this LFTR_VAR list, maintain DPO order
// TODO: this could be made faster by always appending to the end in Step 1
//	 and only do the DPO order search in Step 6.
// We require that the given new occurrence not be a duplicate (same
// Occurrence(), Stmt(), and Stmt_kid_num() as another in the list).
void
LFTR_VAR::Add_comparison(EXP_OCCURS *new_occur)
{
  Is_True(new_occur->Occ_kind() == EXP_OCCURS::OCC_COMP_OCCUR,
	  ("LFTR_VAR::Add_comparison, wrong occur kind"));

  if (_comp_list == NULL)	// new list of comp occurrences
    _comp_list = new_occur;
  else {			// need to insert in DPO order
    EXP_OCCURS *tmp_occ, *prev = NULL;
    EXP_OCCURS_ITER tmp_occ_iter;
    FOR_ALL_NODE(tmp_occ, tmp_occ_iter, Init(_comp_list)) {
      if (new_occur->Bb()->Dom_dfs_id() < tmp_occ->Bb()->Dom_dfs_id())
	break;
      if (new_occur->Bb() == tmp_occ->Bb() &&
          new_occur->Stmt_order_less_or_equal(tmp_occ))
        break;
      prev = tmp_occ;
    }
    if (prev != NULL) {		// insert in middle or end
      new_occur->Set_Next(prev->Next());
      prev->Set_Next(new_occur);
    } else {			// front of list
      new_occur->Set_Next(_comp_list);
      _comp_list = new_occur;
    }
  }
  Inc_list_size(); // size of lftr_var list for fast iterator
}

// ======================================================================
// remove a comparison from a specific list
// ======================================================================
void
LFTR::Remove_comparison(EXP_OCCURS *comp, AUX_ID lftr_var)
{
  Is_True(comp != NULL, ("LFTR::Remove_comparison, NULL occur"));
  Is_True(lftr_var != ILLEGAL_AUX_ID,
	  ("LFTR::Remove_comparison, bad lftr_var"));

  LFTR_VAR *list = Find_comp_list(lftr_var);
  Is_True(list != NULL, ("LFTR::Remove_comparison, NULL lftr_var list"));
  EXP_OCCURS *exp_occ, *prev = NULL;
  EXP_OCCURS_ITER exp_iter;
  FOR_ALL_NODE(exp_occ, exp_iter, Init(list->Occ_list())) {
    if (exp_occ == comp) { // found it
      if (prev == NULL)
	list->Set_occ_list(exp_occ->Next());
      else
	prev->Set_Next(exp_occ->Next());
      list->Dec_list_size();
      return;
    }
    prev = exp_occ;
  }
  Is_True(FALSE, ("LFTR::Remove_comparison, cound not find occur"));
}

// ======================================================================
// This routine is called at the end of step 1 in EPRE.  This is needed 
// because the BB_LOOP bitset Lftr_non_candidates() is both set and used in
// step 1, so some comp occurrence nodes could be created when their
// lftr_var is not in Lftr_non_candidates(), but later the lftr_var is
// set in Lftr_non_candidates(); this routine finds and removes these comp 
// occurrences nodes.
// ======================================================================
void
LFTR::Remove_lftr_non_candidates(void)
{
  LFTR_VAR *comp_list;
  EXP_OCCURS *occur, *prev;
  EXP_OCCURS_ITER occur_iter;
  INT32 i;
  for (i=0; i != _hash_size; i++) {
    if (_hash_vec[i] == NULL)
      continue;
    LFTR_VAR_ITER lftr_var_iter(_hash_vec[i]);
    FOR_ALL_NODE(comp_list, lftr_var_iter, Init()) {
      prev = NULL;
      FOR_ALL_NODE(occur, occur_iter, Init(comp_list->Occ_list())) {
	BB_NODE *bb = occur->Bb();
	BB_LOOP *loop = bb->Innermost();
	if (loop == NULL || loop->Lftr_non_candidates() == NULL) {
	  prev = occur;
	  continue;
	}
	if (loop->Lftr_non_candidates()->MemberP(comp_list->Lftr_var())) {
	  // delete this comp occurrence node
	  if (prev == NULL)
	    comp_list->Set_occ_list(occur->Next());
	  else prev->Set_Next(occur->Next());
	  comp_list->Dec_list_size();
	  Is_Trace(Trace(),(TFile,"LFTR::Remove_lftr_non_candidate removes in BB%d:\n", bb->Id()));
	  Is_Trace_cmd(Trace(),occur->Print(TFile));
	}
	else prev = occur;
      }
    }
  }
}

// ======================================================================
// Assign a statement number to each statement in the PU
// ======================================================================
void
LFTR::Assign_stmt_no(STMTREP *stmt)
{
  Is_True(Lftr_on(),("LFTR::Assign_stmt_no, LFTR not on"));
  _stmt_no.AddElement(_last_stmt);
  stmt->Set_stmt_id(_stmt_no.Lastidx()); // PPP do this during CFG build
  _last_stmt++;
}

// ======================================================================
// Check if worklist expression is not a comparison but contains a LFTR
// var, if so they return the correct comparison occurrence list for it
// If the expression doesn't look appropriate for LFTR, return NULL.
// ======================================================================
EXP_OCCURS_PAIR *
LFTR::Exp_hash(EXP_WORKLST *worklst)
{
  EXP_OCCURS_PAIR *comp_occurs = &worklst->Comp_occurs();
  comp_occurs->Clear();

  CODEREP *exp = worklst->Exp();
  AUX_ID   prev_aux_id = ILLEGAL_AUX_ID;
  if (Is_lftr_exp(exp)) { // e.g. i * 4
    // find the LFTR var
    for (INT i = 0; i < exp->Kid_count(); i++) {
      CODEREP *kid = exp->Opnd(i);
      if (kid->Kind() == CK_VAR && kid->Aux_id() != prev_aux_id) {
        AUX_ID lftr_var_id = kid->Aux_id();
        prev_aux_id = lftr_var_id;
        if (lftr_var_id != ILLEGAL_AUX_ID) { // found a valid lftr_var
          LFTR_VAR *lftr_var = Find_comp_list(lftr_var_id); // sets list length
          if (lftr_var != NULL)
            comp_occurs->Set_occ(i, lftr_var->Occ_list());
        } // legal aux_id
      } // is a variable
    } // for each kid
  }
  return comp_occurs;
}

// ======================================================================
// Return the CR of the lftr var if found, NULL otherwise
// Doesn't check if the rhs is invariant because we can't tell if we
// are in a loop (stmtrep is not always available).
// ======================================================================
CODEREP *
LFTR::Find_lftr_var(CODEREP *cr)
{
  // find the LFTR var, can't tell if expr is inside a loop because
  // EXP_WORKLST doesn't have stmt
  CODEREP *lhs, *rhs;
  lhs = Set_lhs_and_rhs(cr,&rhs);
  if (lhs->Kind() == CK_VAR) {	// LFTR var could be lhs
    // A) no rhs
    // B) constant on right, lftr var is lhs
    // C) LFTR var could be either (PPP - arbitrary decision)
    if (rhs == NULL || inCODEKIND(rhs->Kind(),CK_CONST|CK_VAR|CK_OP|CK_LDA))
      return lhs;
  }
  return NULL;
}

// ======================================================================
// a different version of it, look for the CODEREP in tree that has
// the same Aux_id as the second parameter 'var'
// ======================================================================
CODEREP *
LFTR::Find_lftr_var(CODEREP *exp, CODEREP *var)
{
  if (exp->Kind() != CK_OP) return NULL;

  for (INT i = 0; i < exp->Kid_count(); i++) {
    CODEREP *kid = exp->Opnd(i);
    if (kid->Kind() == CK_VAR && kid->Aux_id() == var->Aux_id())
      return kid;
  }
  return NULL;
}

// ======================================================================
// Find an LDID of the given aux id in the given expression. If not
// found, return NULL.
// ======================================================================

static CODEREP *
Find_aux_id_use_in_expr(AUX_ID id, CODEREP *cr)
{
  CODEREP *retval;
  INT      i;

  switch (cr->Kind()) {
  case CK_VAR:
    if (cr->Aux_id() == id)
      return cr;
    else
      return NULL;
  case CK_IVAR:
    Is_True(cr->Ilod_base(),
            ("Find_aux_id_use_in_expr: Ilod_base is NULL"));
    return Find_aux_id_use_in_expr(id, cr->Ilod_base());

  case CK_OP:
    for (i = 0; i < cr->Kid_count(); i++) {
      retval = Find_aux_id_use_in_expr(id, cr->Opnd(i));
      if (retval != NULL)
	return retval;
    }
    /* FALLTHRU */
  default:
    return NULL;
  }
}

// ======================================================================
// verify that the statement still contains that comparison (not needed
// if we check for removal of comp occurs when doing bottom-up rehash).
// If so, set the OCC_OBSOLETE_COMPARISON flag in the occurrence node.
// ======================================================================
void
LFTR::Check_for_obsolete_comparison(EXP_OCCURS *comp)
{
  if (comp->Obsolete_comparison())
    return;
  STMTREP *stmt = comp->Stmt();
  INT32 kid_num = comp->Stmt_kid_num();
  BOOL obsolete;

  if (OPERATOR_is_call(stmt->Opr())) 
    obsolete =  ! stmt->Rhs()->Opnd(kid_num)->Contains(comp->Occurrence());
  else if (OPERATOR_is_store(stmt->Opr())) {
    switch (kid_num) {
    case 0:
      obsolete = ! stmt->Rhs()->Contains(comp->Occurrence());
      break;
    case 1:
      if (stmt->Lhs()->Kind() == CK_IVAR) {
	// This is an ISTORE statement. CODEREP::Contains() works only
	// on rvalues.
	obsolete = ! stmt->Lhs()->Istr_base()->Contains(comp->Occurrence());
      }
      else {
	obsolete = ! stmt->Lhs()->Contains(comp->Occurrence());
      }
      break;
    case 2:
      obsolete = ! stmt->Lhs()->Mstore_size()->Contains(comp->Occurrence());
      break;
    default: 
      Is_True(FALSE, ("LFTR::Check_for_obsolete_comparison: bad stmt_kid_num"));
    }
  }
  else if (stmt->Opr() == OPR_PREFETCH) 
    obsolete = ! stmt->Rhs()->Ilod_base()->Contains(comp->Occurrence());
  else obsolete = ! stmt->Rhs()->Contains(comp->Occurrence());

  if (obsolete) {
    comp->Set_obsolete_comparison();
    Is_Trace(Trace(), (TFile, "LFTR::Check_for_obsolete_comparison finds:\n"));
    Is_Trace_cmd(Trace(), comp->Print(TFile));
    Is_Trace(Trace(), (TFile, "                                    no "
		       "longer in\n"));
    Is_Trace_cmd(Trace(), stmt->Print(TFile));
  }
}

// ======================================================================
// x is the induction expression containing the IV with aux_id being used in 
// test replacement; check that the result of x can only be a larger value
// than the IV; if it sees any other variable in the expression, has to 
// return false because the variable can be -ve.
// ======================================================================
BOOL
LFTR::Can_only_increase(CODEREP *x, AUX_ID aux_id)
{
  switch (x->Kind()) {
  case CK_LDA: 
    return TRUE;
  case CK_CONST:
    return (x->Const_val() >> 15) == 0; // if constant is large, assume -ve
  case CK_RCONST:
    return FALSE;
  case CK_VAR:
    return x->Aux_id() == aux_id;
  case CK_IVAR:
    return FALSE;
  case CK_OP:
    switch (x->Opr()) {
    case OPR_NEG: case OPR_SUB:
      return FALSE;
    case OPR_ADD: case OPR_MPY: 
      {
	for (INT i = 0; i < x->Kid_count(); i++) {
	  if (! Can_only_increase(x->Opnd(i), aux_id))
	    return FALSE;
	}
      }
      return TRUE;
    default: ;
    }
  default: ;
  }
  return FALSE;
}

// Given a CODEREP 'cr_iter', walk its operands, replace the use of 'cr_old' with 'cr_new'.
// The replacement is done without rehashing.
void
LFTR::Replace_use(CODEREP * cr_iter, CODEREP * cr_old, CODEREP * cr_new)
{
  CODEKIND kind = cr_iter->Kind();
  CODEREP * cr_copy = NULL;
  CODEREP * cr_orig = cr_iter;

  if (kind == CK_IVAR) {
    CODEREP * cr_base = NULL;
    BOOL is_ilod = FALSE;

    cr_base = cr_iter->Ilod_base();
    if (cr_base) {
      if (cr_base == cr_old) {
	cr_iter->Set_ilod_base(cr_new);
      }
      else {
	Replace_use(cr_base, cr_old, cr_new);
      }
    }

    cr_base = cr_iter->Istr_base();
    if (cr_base) {
      if (cr_base == cr_old) {
	cr_iter->Set_istr_base(cr_new);
      }
      else {
	Replace_use(cr_base, cr_old, cr_new);
      }
    }
  }
  else if (kind == CK_OP) {
    cr_copy = NULL;

    for (int i = 0; i < cr_iter->Kid_count(); i++) {
      CODEREP * cr_tmp = cr_iter->Opnd(i);
      if (cr_tmp->Kind() == CK_VAR) {
	if (cr_tmp == cr_old) {
	  cr_iter->Set_opnd(i, cr_new);
	}
      }
      else {
	Replace_use(cr_tmp, cr_old, cr_new);
      }
    }
  }
}

// Given a loop, iterate all statements, replace the use of 'cr_old' with 'cr_new'.
// Skip the definition statement of 'cr_new'.
void
LFTR::Replace_use(const BB_LOOP * loop, CODEREP * cr_old, CODEREP * cr_new)
{
  BB_NODE * bb;
  BB_NODE_SET_ITER bb_iter;
  FOR_ALL_ELEM(bb, bb_iter, Init(loop->True_body_set())) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt == cr_new->Defstmt())
	continue;

      CODEREP * rhs = stmt->Rhs();
      if (rhs) {
	if (rhs == cr_old) {
	  stmt->Set_rhs(cr_new);
	  cr_new->IncUsecnt();
	}
	else {
	  Replace_use(rhs, cr_old, cr_new);
	}
      }
      CODEREP * lhs = stmt->Lhs();
      if (lhs) {
	Replace_use(lhs, cr_old, cr_new);
      }
    }
  }
}

// ======================================================================
// Perform the replacement of the comparison
//   replace lhs with tempcr
//   replace rhs with original expression with rhs substituted
//   remove original comparison for list and put substituted comparison on
//   save rhs expression for adding to front of worklst
//
// We replace only comparisons in loops that are the innermost loop
// containing some real occurrence. Here is a discussion of the
// reasons behind this replacement criterion:
//
// Consider the following example (derived from the test case for
// 535546):
//
// 1:
//  a = b;
// 2:
//  if (b < a + 1) ...
//  b = b + 1;  // No real occurrence of b + 1 here because this
//              // statement is an IV update.
//  if (...) goto 2;
//  if (...) goto 1;
//
// Now perform PRE, replacing a + 1 with the temporary c:
//
// 1:
//  a = b;
//  c = a + 1;
// 2:
//  if (b < c) ...
//  b = b + 1;
//  if (...) goto 2;
//  if (...) goto 1;
//
// Now perform output copy-propagation:
//
// 1:
//  a = b;
//  c = b + 1;
// 2:
//  if (b < c) ...
//  b = b + 1;  // No real occurrence of b + 1 here because this
//              // statement is an IV update.
//  if (...) goto 2;
//  if (...) goto 1;
//
// Now perform PRE, replacing b + 1 with the temporary d, and
// performing LFTR on the comparison b < c:
//
//  d = b + 1;
// 1:
//  a = b;
//  c = d;
// 2:
//  if (d < c + 1) ...
//  b = b + 1;  // No real occurrence of b + 1 here because this
//              // statement is an IV update.
//  d = d + 1;  // Strenth reduction injury repair
//  if (...) goto 2;
//  if (...) goto 1;
//
// Now c and d stand in the same relationship as was originally held
// by a and b, so we get an endless loop in the compiler, with PRE
// in a cycle of length two.
//
// To address this problem, we establish the criterion that to be
// eligible for LFTR, any comparison must be accompanied by a real
// occurrence of the current expression at the same loop nest
// level. This criterion avoids the looping behavior in the above
// small example by disabling LFTR for the b < c comparison. This
// criterion is sufficient to guarantee termination in general by the
// following argument. We assume that SSAPRE terminates with all its
// bells and whistles except LFTR turned on, so we only need to
// consider possible non-termination due to LFTR.
//
// Proposition 1: Any infinite sequence of LFTR's realized by the
// algorithm contains no infinite sequence of appearances of the same
// expression as SSAPRE's current expression. In other words, any
// infinite sequence of LFTR's eventually has the property that we
// don't have the same expression showing up over and over again.
//
// Proof: For the moment, I don't see how to prove proposition 1. I
// thought for some time that I did, but I don't. I submit it without
// proof because I hope it's true, and in any case the kind of endless
// loop it promises is the kind we had and the kind the "real
// occurrence in the loop" fix is designed to address. So we'll use
// proposition 1 without proof.
//
// Proposition 2: Any infinite sequence of PRE operations realized by
// SSAPRE must contain an infinite sequence of LFTR's (by the
// assumption that we terminate without LFTR), and such an infinite
// sequence of LFTR's eventually reaches a state where every real
// occurrence considered by the algorithm during the remainder of the
// sequence is contained in some comparison.
//
// Proof: This should follow from proposition 1, but at the moment I
// haven't made the details work out.
//
// The upshot of proposition 2 is that we don't need to worry about
// real occurrences other than those that show up on the right-hand
// sides of comparisons that are eligible for LFTR. Since only
// comparisons with loop-invariant right-hand sides and loop-variant
// left-hand sides are eligible for LFTR, we're done because within a
// particular loop, no LFTR variable can appear in a real occurrence
// of an expression that shows up in the right-hand side of a
// comparision that's eligible for LFTR. And if the current expression
// has no real occurrence loop-nested at the same level as the
// comparison, we don't do LFTR.
// ======================================================================
void
LFTR::Replace_comparison(EXP_OCCURS *comp, BOOL cur_expr_is_sr_candidate)
{
  Is_Trace(Trace(),(TFile,"LFTR::Replace_comparison called on:\n"));
  Is_Trace_cmd(Trace(),comp->Print(TFile));
  Is_Trace_cmd(Trace(),comp->Occurrence()->Print(0,TFile));
  Is_True(comp->Occ_kind() == EXP_OCCURS::OCC_COMP_OCCUR,
	  ("LFTR::Replace_comparison, comp is not a comp occur"));

  if (WOPT_Enable_LFTR2_Limit != -1 && WOPT_Enable_LFTR2_Limit <= Num_substitutions()) {
    Is_Trace(Trace(), (TFile,"LFTR return : exceeded limit\n"));
    return;
  }

  // verify that the statement still contains that comparison (not needed
  // if we check for removal of comp occurs when doing bottom-up rehash)
  Check_for_obsolete_comparison(comp);
  if (comp->Obsolete_comparison())
    return;

  // The LOOP_HAS_REAL_OCC flag is set for loops containing real
  // occurrences in ESSA::Rename() during the initial rename pass over
  // all the occurrences.
  //
  // We replace comparisons only in loops that contain real
  // occurrences because this criterion helps us guarantee termination
  // of the combination of PRE with LFTR, Strength Reduction,
  // second-order effects, and output copy-propagation.
  //
  // Related bugs: 469388 and 535546. The fix for 469388 fixed all the
  // examples I knew about at the time, but I was too lazy to put
  // together a proof that my fix had addressed the problem in
  // general. So of course it hadn't addressed the problem in general,
  // and it only took a few obscure changes to the optimizer to expose
  // the same problem again in the form of 535546. This time, I've
  // sketched a proof roughly above, but I'm still too lazy to get all
  // the details at the moment. -- RK 971028
  //
  // See above for more detailed discussion.
  if (cur_expr_is_sr_candidate &&
      !comp->Bb()->Innermost()->Is_flag_set(LOOP_HAS_REAL_OCC)) {
    Is_Trace(Trace(), (TFile, "LFTR return 0 - no real occ in same "
		       "loop nest level\n"));
    return;
  }
  else {
    Is_Trace(Trace(), (TFile, "Loop headed at BB%d contains real occ\n",
		       comp->Bb()->Innermost()->Header()->Id()));
  }

  EXP_OCCURS *tos = comp->Def_occur();
  if (tos == NULL) { // no dominating expr for this comparison
    Is_Trace(Trace(), (TFile,"LFTR return 1 - no dominating tos\n"));
    return;
  }
  // verify that tos dominates comp
  Is_True(tos->Bb()->Dominates(comp->Bb()) &&
	  (tos->Bb() != comp->Bb() ||
	   tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR ||
	   tos->Enclosed_in_stmt()->Stmt_id() <=
	   comp->Enclosed_in_stmt()->Stmt_id()),
	  ("LFTR::Replace_comparison, tos does not dominate comp"));

  Is_True(tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR ||
	  tos->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
	  ("LFTR::Replace_comparison, unexpected tos kind"));
  // Step 3 missed this comparison because no expr dominates it
  if (comp->E_version() == EXP_OCCURS::ILLEGAL_E_VERSION) {
    Is_Trace(Trace(),(TFile,"LFTR return 2 - no e-version for comp occur\n"));
    return;
  }
  // the two versions of the expressions have to be the same
  // Note: the versions of the variables involved in the expressions do
  // not have to be the same, just the e-version of the expressions.
  Is_True(comp->E_version() == tos->E_version(),
	  ("LFTR::Replace_comparison, e-version of tos and comp differ"));

  // the occurrences cannot be compound (SSA PRE rule)
  Is_True(tos->Occurrence()->Kind() == CK_OP &&
	  tos->Occurrence()->Opnd(0)->Kind() != CK_OP &&
	  (tos->Occurrence()->Kid_count() == 1 ||
	   tos->Occurrence()->Opnd(1)->Kind() != CK_OP),
	  ("LFTR::Replace_comparison, tos->Occurrence is compound"));

  // no floating point or quad comparisons
  if (MTYPE_is_float(comp->Occurrence()->Dsctyp()))
    return;

  // find the lftr_vars for the comparison and dominating expression
  CODEREP *comp_lftr_var = Find_lftr_var(comp->Occurrence());

  // find the kid in tos that has the same lftr_var as the one in comp occurs
  CODEREP *tos_lftr_var = Find_lftr_var(tos->Occurrence(), comp_lftr_var);

  if (comp_lftr_var == NULL || tos_lftr_var == NULL) {
    Is_Trace(Trace(), (TFile, "LFTR return 3 - could not find Xa lftr_var, "
		       "comp=0x%p, tos=0x%p\n", comp_lftr_var,
		       tos_lftr_var));
    return;
  }
  else {
    Is_Trace(Trace(), (TFile, "comp_lftr_var: "));
    Is_Trace_cmd(Trace(), comp_lftr_var->Print(0, TFile));
    Is_Trace(Trace(), (TFile, " tos_lftr_var: "));
    Is_Trace_cmd(Trace(), tos_lftr_var->Print(0, TFile));
  }

  AUX_ID comp_lftr_var_id = comp_lftr_var->Aux_id();
  AUX_ID tos_lftr_var_id = tos_lftr_var->Aux_id();

  // could not find one of the lftr_vars
  if (comp_lftr_var_id==ILLEGAL_AUX_ID || tos_lftr_var_id==ILLEGAL_AUX_ID) {
    Is_Trace(Trace(),
	     (TFile,"LFTR return 4 - illegal aux_id comp_id=%d, tos_id=%d\n",
		      comp_lftr_var_id,tos_lftr_var_id));
    return;
  }

  // tempcr is the preg load that we want to use for the comparison lhs
  CODEREP *tempcr = tos->Temp_cr();
  STMTREP * iv_init = NULL;
  STMTREP * temp_init = NULL;
  CODEREP * iv_incr = NULL;
  // comp is dominated by a phi, find dominating temp (due to SR injury fix)
  if (tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
    STMTREP *iv_defstmt = comp_lftr_var->Defstmt();
    if (iv_defstmt == NULL) {
      // no increment before the comparison, so just use the tempcr of the PHI
      // for substitution
    } else if (iv_defstmt->Bb() != tos->Bb() &&
	iv_defstmt->Bb()->Dominates(tos->Bb())) { // cases 4, 5
      // the DEF for i is before tos, we want to use the tempcr of the PHI
      // for substitution
    } else { // cases 1, 2, 3
      CODEREP *cr = Find_SR_tempcr(iv_defstmt,tempcr);
      if (cr == NULL) {
	Is_Trace(Trace(),(TFile,"LFTR return 6 - temp not found\n"));
	return; // 1, 2
      }
//Bug# 1164
#ifdef KEY
      if (iv_defstmt->Iv_update() && iv_defstmt->Repaired() && tempcr->Aux_id() == cr->Aux_id()) {
#else
      if (iv_defstmt->Iv_update() && tempcr->Aux_id() == cr->Aux_id()) {
#endif
	// 3
	Is_True(tempcr != cr, ("LFTR::Replace_comparison, tempcr is same"));
#if defined(TARG_IA32) || defined(TARG_X8664)
	// Bug 778.  overflow problem for 32-bit architectures.
	if (Is_Target_32bit()) {
	  CODEREP * sym4 = tempcr;
	  CODEREP * sym5 = cr;
	  tempcr = cr;
	  // Attempt pre-increment transformation for the following pattern to avoid overflow.
	  // sym7v3 = const1                 // sym7 corresponds to IV.
	  // sym11v3 = sym7v3 + sym3         // sym11 corresponds to tempcr.
	  // sym11v4 = phi(sym11v3, sym11v5)
	  // LABEL:
	  //    *sym11v4 = ...
	  //    sym11v5 = sym11v4 + const2
	  //    if (sym11v5 <= end)
	  //       goto LABEL
	  //
	  // After the transformation:
	  // sym7v3 = const1
	  // sym11v3 = const1 - const2 + sym3 // The result of 'const1 - const2' should use signed type.
	  // sym11v4 = phi(sym11v3, sym11v5)
	  // LABEL:
	  // sym11v5 = sym11v4 + const2           
	  //   *sym11v5  = ...
	  //   if (sym11v5 <= end - const2)
	  //      goto LABEL
	  PHI_NODE * phi = (sym4->Is_flag_set(CF_DEF_BY_PHI)) ? sym4->Defphi() : NULL;
	  OPT_STAB * ops = Etable()->Opt_stab();
	  if ((phi != NULL) && (phi->Size() == 2)
	      && (ST_class(ops->St(tempcr->Aux_id())) == CLASS_PREG)
	      && (iv_defstmt->Bb() == comp->Bb())
	      && (tempcr->Usecnt() == 0)
	      && (Find_comp_list(comp_lftr_var_id)->Size() == 1)) {
	    STMTREP * stmt_tmp = tempcr->Defstmt();
	    CODEREP * cr_tmp = stmt_tmp->Rhs();
	    EXP_WORKLST * worklst = Etable()->Get_worklst(cr_tmp, FALSE, TRUE);
	    
	    // Avoid the case that the RHS of 'stmt_tmp' is a CSE since we are going to 
	    // replace the use operands without rehashing.
	    if (worklst 
		&& (worklst->Real_occurs().Head() != worklst->Real_occurs().Tail()))
	      cr_tmp = NULL;
	    else if (cr_tmp->Usecnt() > 1) {
	      // Additional check to preclude the cases that the RHS of 'stmt_tmp' is a CSE.
	      cr_tmp = NULL;
	    }

	    if (cr_tmp
		&& (cr_tmp->Kind() == CK_OP) 
		&& (cr_tmp->Opr() == OPR_ADD)
		&& (cr_tmp->Opnd(1)->Kind() == CK_CONST)
		&& (cr_tmp->Opnd(1)->Const_val() > 0)) {
	      iv_incr = cr_tmp->Opnd(1);
	      CODEREP * opnd1 = phi->OPND(0);
	      CODEREP * opnd2 = phi->OPND(1);
	      if (opnd1->Defstmt() == stmt_tmp)
		stmt_tmp = opnd2->Defstmt();
	      else if (opnd2->Defstmt() == stmt_tmp)
		stmt_tmp = opnd1->Defstmt();	
	      else
		stmt_tmp = NULL;
	      if (stmt_tmp){
		temp_init = stmt_tmp;
		cr_tmp = stmt_tmp->Rhs();
		if ((cr_tmp->Kind() == CK_OP)
		    && (cr_tmp->Opr() == OPR_ADD)) {
		  // Find initial IV definition.
		  opnd1 = cr_tmp->Opnd(0);
		  opnd2 = cr_tmp->Opnd(1);
		  stmt_tmp = NULL;
		  if (opnd1->Kind() == CK_VAR) 
		    stmt_tmp = opnd1->Defstmt();
		  else if (opnd2->Kind() == CK_VAR)
		    stmt_tmp = opnd2->Defstmt();
		  if (stmt_tmp) {
		    cr_tmp = stmt_tmp->Lhs();
		    if (cr_tmp
			&& (cr_tmp->Kind() == CK_VAR)
			&& (cr_tmp->Aux_id() == iv_defstmt->Lhs()->Aux_id())) {
		      cr_tmp = stmt_tmp->Rhs();
		      if ((cr_tmp->Kind() == CK_CONST)) {
			CODEREP * cr_cmp = comp->Occurrence();
			CODEREP * cr_cons = cr_cmp->Opnd(1);
			OPERATOR opr = cr_cmp->Opr();
			// Limit to the case that loop trip count is at least 2
			// to avoid underflow of "end - const2".
			if ((cr_cons->Kind() == CK_CONST)
			    && ((opr == OPR_LE) || (opr == OPR_LT))) {
			  int trip_cnt = (cr_cons->Const_val() - cr_tmp->Const_val())
			    / iv_incr->Const_val();
			  if (opr == OPR_LE)
			    trip_cnt++;
			  if (trip_cnt >= 2) {
			    iv_init = stmt_tmp;
			  }
			}
		      }
		    }
		  }
		}
	      }
	    }
	  }
	}
#endif
      }
    }
  } else { // dominated by real occurrence, easy case
    // if tempcr is non-NULL, it better be a tempcr and not unioned def_occur
    Is_True((!tos->Save_to_temp() && tempcr == NULL) || tempcr,
	    ("LFTR::Replace_comparison, tempcr is not really a tempcr"));
    if (tempcr == NULL) { // no tempcr saved so can't do anything
      Is_Trace(Trace(),(TFile,"LFTR return 7 - no tempcr for real tos\n"));
      return;
    }
  }

  // Now we have to check that the version of the lftr var at the
  // comparison matches the version where the tempcr was defined. If
  // it doesn't, there was some redefinition of the lftr var that SR
  // did not repair, and we must give up.
  if (tempcr->Is_flag_set(CF_DEF_BY_PHI)) {
    // The temp is defined by phi. The defining phi must be before (in
    // the case of simplification of identity assignment) or at the
    // point of tos. It isn't completely trivial to see, but in either
    // case, the version of the lftr var in the tos occurrence must be
    // current at the point of definition of tempcr.
    Is_Trace(Trace(),
	     (TFile, "LFTR::Replace_comparison: temp is def by phi\n"));
    if (tos_lftr_var != comp_lftr_var) {
      Is_Trace(Trace(), (TFile, "LFTR return 8 - lftr var in comp "
			 "not current at temp def\n"));
      return;
    }
    iv_init = NULL;
  }
  else {
    // The temp is defined by a statement -- either a save to
    // temporary or an injury repair.
    FmtAssert(!tempcr->Is_flag_set(CF_DEF_BY_CHI),
	      ("LFTR::Replace_comparison: tempcr must not be def by chi"));

    // If the temp is defined by a save to temp, the lftr var will
    // appear in the RHS of the save statement. If the temp is defined
    // by an injury repair, we do linear search upward in the block to
    // find the injuring statement; its LHS must be the lftr var.
    //
    // First, try to find the lftr var as if tempcr->Defstmt() were a
    // save to temp:
    CODEREP *temp_lftr_var = Find_aux_id_use_in_expr(tos_lftr_var_id,
						     tempcr->Defstmt()->Rhs());

    if (temp_lftr_var == NULL) {
      // tempcr->Defstmt() must not have been a save to temp. Instead,
      // it must be an injury repair.
      STMTREP *lftr_var_def;
      for (lftr_var_def = tempcr->Defstmt();
	   lftr_var_def;
	   lftr_var_def = lftr_var_def->Prev()) {
	if (OPERATOR_is_scalar_store (lftr_var_def->Opr())) {
	  if (lftr_var_def->Lhs()->Aux_id() == tos_lftr_var_id) {
	    // Found the current version. Stop.
	    temp_lftr_var = lftr_var_def->Lhs();
	    break;
	  }
	}
	else {
	  Is_True(OPERATOR_is_scalar_istore (lftr_var_def->Opr()) &&
		  lftr_var_def->Is_RHS_saved(),
		  ("LFTR::Replace_comparison: "
		   "String of injury repairs must not be polluted"));
	}
      }
      FmtAssert(lftr_var_def != NULL,
		("LFTR:Replace_comparison: cannot find lftr var version"));
      Is_Trace(Trace(), (TFile, "LFTR::Replace_comparison: "
			 "lftr var in lhs of repaired injury\n"));
    }
    else {
      Is_Trace(Trace(), (TFile, "LFTR::Replace_comparison: "
			 "lftr var in rhs of save to temp\n"));
      iv_init = NULL;
    }
    // If the current version of the lftr var at the temp's definition
    // is different from that in the comparison, we can't replace the
    // comparison.
    if (temp_lftr_var != comp_lftr_var) {
      Is_Trace(Trace(), (TFile, "LFTR return 9 - lftr var in comp "
			 "not current at temp def\n"));
      return;
    }
  }

  // check the size of tempcr >= size of original iv
  if (MTYPE_size_min(tempcr->Dtyp()) < MTYPE_size_min(comp_lftr_var->Dtyp()))
    return;

  NUMBER factor;
  if (Find_one_variant(comp->Bb(), comp_lftr_var, tos->Occurrence(), &factor,
                       Htable()) != ONE_VARIANT) {
    return;
  }
  else {
    Is_Trace(Trace(), (TFile, "LFTR var variant at BB%d "
		       "(loop head at BB%d)\n", comp->Bb()->Id(),
		       comp->Bb()->Innermost()->Header()->Id()));
  }

  BOOL eq_neq = comp->Occurrence()->Opr() == OPR_EQ || 
		comp->Occurrence()->Opr() == OPR_NE;

  if (factor.Value() < 0 &&
      MTYPE_is_unsigned(comp->Occurrence()->Dsctyp()) && ! eq_neq)
    return;


  // if the multiply factor is unknown, we cannot perform fenceposting
  // because the invariant might be 0.
  if (factor.Desc() == NUMBER_UNKNOWN)
    return;

  ADDRESSABILITY addressable = tos->Occurrence()->Check_if_result_is_address(Htable()->Sym());

  if (MTYPE_is_unsigned(comp->Occurrence()->Dsctyp()) && ! eq_neq &&
      addressable != ADDRESSABILITY_IS_ADDRESS &&
      ! Can_only_increase(tos->Occurrence(), tos_lftr_var_id))
    return;

  // Assert that the loop header's block contains a phi for the
  // current expression's temporary. I believe this condition is
  // guaranteed by the conditions above, particularly the
  // Find_one_variant() call [ this belief is false; see below ]. We
  // require that the head block of the innermost loop containing the
  // comparison have a phi for the temporary for the following
  // reasons:
  //
  // 1. A correctness reason: This condition is required to keep
  // strength reduction, LFTR, SSAPRE's ocopy-prop, and SSAPRE
  // second-order effects from interacting to bring about
  // nonterminating compilation (see bug 490533).
  //
  // 2. A performance reason: It doesn't really help to LFTR
  // comparisons whose results are loop-invariant.
  const BB_LOOP *loop = Cfg()->Find_innermost_loop_contains(comp->Bb());
  Is_True(loop != NULL,
	  ("LFTR::Replace_comparison: comparison must be in a loop"));

  BB_NODE *head = loop->Header();
  Is_True(head != NULL,
	  ("LFTR::Replace_comparison: loop head must not be NULL"));

  // This allows hoisting the definition statement of 'tempcr' to loop header.
  if (iv_init && !tempcr->Defstmt()->Bb()->Postdominates(head)) {
    iv_init = NULL;
  }

  // In spite of the Find_one_variant() call above, there may not be a
  // phi for the temp at the top of the loop, since the loop-variant
  // operand of the comparison may be defined by a killing
  // (non-iv-update) definition inside the loop, meaning that the
  // current expression gets saved and reloaded inside the loop. This
  // is essentially analogous to the straight-line code case. TODO:
  // Perhaps we should delete special handling earlier for
  // straight-line code and just let this check take care of it.
  if (Etable()->Lookup_var_phi(head, tempcr->Aux_id()) == NULL) {
    Is_Trace(Trace(), (TFile, "LFTR::Replace_comparison: return 10 - "
		       "lftr var not recognized as loop induction var"));
    return;
  }
  else {
    Is_Trace(Trace(), (TFile, "LFTR:Replace_comparison: Loop head "
		       "is BB%d\n", head->Id()));
  }

  // create new rhs using tos expression as a template
  {
    CODEREP *rhs_template =
      Alloc_stack_cr(tos->Occurrence()->Extra_ptrs_used());
    rhs_template->Copy(*tos->Occurrence());
    CODEREP *new_cr, *fold_cr;
    FOLD ftmp;
    new_cr = Replace_lftr_var(rhs_template,comp_lftr_var_id,
			      comp->Occurrence()->Opnd(1));
    OPERATOR opr = new_cr->Opr();

#ifdef TARG_X8664
    if (opr != OPR_CVT)
#endif
    {
    // check possible overflow situations
    CODEREP  *kid0 = new_cr->Opnd(0);
    CODEREP  *kid1 = new_cr->Opnd(1);
    if (kid0 == NULL || kid1 == NULL) return;
    NUMBER n1,n2,n3;
    if (kid0->Kind() == CK_CONST && kid1->Kind() == CK_CONST) {
      n1.Set_const(kid0->Const_val());
      n2.Set_const(kid1->Const_val());
      n3.Eval2(opr, &n1, &n2);
      if (n3.Desc() != NUMBER_KNOWN ||
          !n3.Representable_in_nbits(MTYPE_size_min(new_cr->Dtyp())))
        return;
    } else if (kid0->Kind() == CK_LDA && kid1->Kind() == CK_CONST) {
      n1.Set_const(kid0->Offset());
      n2.Set_const(kid1->Const_val());
      n3.Eval2(opr, &n1, &n2);
      if (n3.Desc() != NUMBER_KNOWN ||
          !n3.Representable_in_nbits(new_cr->Offset_nbits()))
        return;
    } else if (kid0->Kind() == CK_CONST && kid1->Kind() == CK_LDA && opr == OPR_ADD) {
      n1.Set_const(kid0->Const_val());
      n2.Set_const(kid1->Offset());
      n3.Eval2(opr, &n1, &n2);
      if (n3.Desc() != NUMBER_KNOWN ||
          !n3.Representable_in_nbits(new_cr->Offset_nbits()))
        return;
    }
    }

    if (comp->Occurrence()->Opnd(1)->Kind() != CK_OP)
      fold_cr = ftmp.Fold_Expr(new_cr);
    else fold_cr = NULL; // cannot fold because it may remove some 1st order 
			 // exprs but we do not delete them from the work list
    if (fold_cr == NULL) {
      new_cr->Set_coderep_id(0);
      fold_cr = Htable()->Rehash(new_cr); 
   }

    // ---- from this point on we are committed to doing the substitution ----

    // create new comparison, this is done because the CR might be shared.
    CODEREP *comparison_cr = CXX_NEW(CODEREP, &_mem_pool);
    comparison_cr->Init_op(comp->Occurrence()->Op(), 2);

    Is_Trace(Trace(),(TFile,"LFTR::Replace_comparison, %dth committed\n",
                      _num_substitutions+1));
    Is_Trace_cmd(Trace(),Print_occ(TFile,"tos:",tos));
    Is_Trace(Trace(),(TFile,"tempcr:\n"));
    Is_Trace_cmd(Trace(),tempcr->Print(0,TFile));

    // adjust operator of new comparison
    OPERATOR new_compare_opr = comparison_cr->Opr();
#ifdef KEY 
    MTYPE new_compare_type = tempcr->Dtyp();
    // do not change signedness of comparison since that could change semantics
    new_compare_type = Mtype_TransferSign(comparison_cr->Dsctyp(), 
					  new_compare_type);
    if (Is_Target_32bit()
	&& (addressable == ADDRESSABILITY_IS_ADDRESS)
	&& MTYPE_is_signed(new_compare_type)) {
      // For 32-bit targets, force comparison of address expressions to be unsigned.
      new_compare_type = Mtype_from_mtype_class_and_size(MTYPE_CLASS_UNSIGNED,
							 MTYPE_size_min(new_compare_type)/8);
    }
    else {
      iv_init = NULL;
    }
#else
    MTYPE new_compare_type = comparison_cr->Dsctyp();
    if (addressable == ADDRESSABILITY_IS_ADDRESS &&
	MTYPE_is_signed(new_compare_type)) {
      // force new comparison to be unsigned comparison
      new_compare_type = Mtype_from_mtype_class_and_size(MTYPE_CLASS_UNSIGNED,
				MTYPE_size_min(new_compare_type)/8);
    }
#endif
    if (factor.Value() < 0 && ! eq_neq) {
      switch (new_compare_opr) {
      case OPR_LT:
	new_compare_opr = OPR_GT;
        break;
      case OPR_LE:
	new_compare_opr = OPR_GE;
        break;
      case OPR_GT:
	new_compare_opr = OPR_LT;
        break;
      case OPR_GE:
	new_compare_opr = OPR_LE;
        break;
      }
    }

    if ((new_compare_opr != OPR_LT) && (new_compare_opr != OPR_LE))
      iv_init = NULL;

    if (fold_cr->Kind() != CK_OP)
      iv_init = NULL;

    if (iv_init) {
      opr = fold_cr->Opr();
      EXP_WORKLST * worklst = Etable()->Get_worklst(fold_cr, FALSE, TRUE); 

      if (opr != OPR_ADD)
	iv_init = NULL;
      else if (worklst && (worklst->Real_occurs().Head())) {
	// avoid the situation that 'fold_cr' has PRE/CSE occurrences so that
	// the CODEREP * at other occurrences won't be changed unintentionally.
	iv_init = NULL;
      }
      else {
	CODEREP * opnd1 = fold_cr->Opnd(0);
	CODEREP * opnd2 = fold_cr->Opnd(1);
	CODEREP * opnd = NULL;

	// reduce loop upper bound by IV increment.
	if (opnd1->Kind() == CK_CONST) 
	  opnd = opnd1;
	else if (opnd2->Kind() == CK_CONST) 
	  opnd = opnd2;
	
	if (opnd) {
	  CODEMAP * codemap = Etable()->Htable();	  
	  INT64 const_val = opnd->Const_val() - iv_incr->Const_val();
	  MTYPE type = opnd->Dtyp();
	  if (const_val < 0)
	    type = (MTYPE_byte_size(type) == 4) ? MTYPE_I4 : MTYPE_I8;

	  CODEREP * cr_const = codemap->Add_const(type, const_val);
	  CODEREP * cr_new = Alloc_stack_cr(fold_cr->Extra_ptrs_used());
	  cr_new->Copy(*fold_cr);
	
	  if (opnd == opnd1)
	    cr_new->Set_opnd(0,cr_const);
	  else
	    cr_new->Set_opnd(1,cr_const);

	  fold_cr = cr_new;
	  fold_cr->Set_coderep_id(0);
	  fold_cr = Htable()->Rehash(fold_cr);
	}
	else
	  iv_init = NULL;
      }
    }

    comparison_cr->Set_opr(new_compare_opr);
    comparison_cr->Set_dsctyp(new_compare_type);

    comparison_cr->Set_opnd(1, fold_cr); 

    // replace lhs with tempcr
    Is_True(comp->Occurrence()->Opnd(0)->Kind() == CK_VAR,
            ("LFTR::Replace_comparison, lhs must be var"));
    comparison_cr->Set_opnd(0, tempcr);

    if (comp->Stmt()->Op() == OPC_TRUEBR||comp->Stmt()->Op() == OPC_FALSEBR){
      BB_LOOP *loop = comp->Bb()->Loop();
      if ( loop != NULL && loop->Loopback() == comp->Bb() )
        loop->Set_iv_replacement(tempcr); // prepare for emitter
    }

    Is_Trace(Trace(),(TFile,"LFTR::Replace_comparison, new cr:\n"));
    Is_Trace_cmd(Trace(),comparison_cr->Print(0,TFile));

    // make sure we can still find the lftr_var of the new expression
    Is_True(Find_lftr_var_id(comparison_cr) != ILLEGAL_AUX_ID,
            ("LFTR::Replace_comparison, can't find new lftr_var"));

    // PPP: use Replace_iv_with_invariant instead, it has all the details

    // remove for original i list
    Remove_comparison(comp, comp_lftr_var_id);
    if (comp->Occurrence()->Opnd(1)->Kind() != CK_OP) { 
      // comparison op is 1st order: search real occurrences for original 
      // comparison, and remove from occur list
      Etable()->Remove_real_occurrence(comp->Occurrence(),
                                       comp->Enclosed_in_stmt());
    }
  
    // rehash new expression
    comparison_cr = Htable()->Rehash(comparison_cr, FALSE); // no canonicalize
    // replace old expr w/new one in stmt
    Etable()->Replace_by_temp(comp, comparison_cr);

    if (comparison_cr->Opnd(1)->Kind() == CK_OP) { // e.g. t < <expr> * 4
      if (comp->Occurrence()->Opnd(1)->Kind() != CK_OP) { // <expr> is terminal
        // prepend n * 4 to worklist
        Etable()->Bottom_up_cr(comp->Enclosed_in_stmt(), comp->Stmt_kid_num(),
                               comparison_cr->Opnd(1), FALSE, 
			       ETABLE::URGENT_INSERT, 0, OPCODE_UNKNOWN, FALSE);
      }
      // create comparison occurrence for new form of comparison
      Insert_comp_occurrence(comparison_cr, comp->Enclosed_in_stmt(),
			     comp->Stmt_kid_num());
    } else { // e.g. t < 40
      // append t < 40 to worklist (also create comparison occurrence)
      Etable()->Bottom_up_cr(comp->Enclosed_in_stmt(), comp->Stmt_kid_num(), 
                             comparison_cr, FALSE, ETABLE::NOT_URGENT_INS, 0,
			     OPCODE_UNKNOWN, FALSE);
    }

    if (iv_init != NULL) {
      // reduce initial value of 'temp_init'.
      INT64 const_val = iv_init->Rhs()->Const_val() - iv_incr->Const_val();
      CODEMAP * codemap = Etable()->Htable();
      CODEREP * cr_old = temp_init->Rhs();
      MTYPE type = cr_old->Dtyp();

      if (const_val < 0) 
	type = (MTYPE_byte_size(type) == 4) ? MTYPE_I4 : MTYPE_I8;

      CODEREP * cr_const = codemap->Add_const(type, const_val);
      CODEREP * cr_new = Alloc_stack_cr(cr_old->Extra_ptrs_used());
      cr_new->Copy(*cr_old);

      AUX_ID id  = iv_init->Lhs()->Aux_id();
      CODEREP * opnd1 = cr_new->Opnd(0);
      CODEREP * opnd2 = cr_new->Opnd(1);

      if ((opnd1->Kind() == CK_VAR)
	  && (opnd1->Aux_id() == id))
	cr_new->Set_opnd(0, cr_const);
      else if ((opnd2->Kind() == CK_VAR)
	       && (opnd2->Aux_id() == id))
	cr_new->Set_opnd(1, cr_const);

      cr_new = Htable()->Rehash(cr_new);
      temp_init->Set_rhs(cr_new);

      Etable()->Remove_real_occurrence(cr_old, temp_init);
      EXP_WORKLST * worklst = Etable()->Get_worklst(cr_old, FALSE, TRUE);
      if (worklst) {
	worklst->Remove_phi_pred_occurrence(temp_init);
      }
      
      STACK<EXP_OCCURS *> * stk = Etable()->Deferred_ocopy_occurs();
      for (int i = 0; i < stk->Elements(); i ++) {
	EXP_OCCURS * occ = stk->Top_nth(i);
	if (occ->Stmt() == temp_init) {
	  stk->DeleteTop(i);
	  break;
	}
      }

      STMTREP * def_stmt = tempcr->Defstmt();
      def_stmt->Bb()->Stmtlist()->Remove(def_stmt);
      BB_NODE * bb_head = loop->Header();
      STMTREP * stmt_ins = NULL;
      STMTREP_ITER stmt_iter(bb_head->Stmtlist());
      STMTREP * stmt;
      FOR_ALL_NODE(stmt, stmt_iter, Init()) {
	if (stmt->Opr() != OPR_LABEL) {
	  stmt_ins = stmt;
	  break;
	}
      }
      if (stmt_ins) {
	bb_head->Insert_stmtrep_before(def_stmt, stmt_ins);
      }
      else {
	bb_head->Append_stmtrep(def_stmt);
      }

      Replace_use(loop, def_stmt->Rhs()->Opnd(0), tempcr);
      FmtAssert(def_stmt->Rhs()->Opnd(0)->Version() != def_stmt->Lhs()->Version(),
		("Wrong version for iv."));
    }


    Is_Trace(Trace(),(TFile,"LFTR::Replace_comparison, new comparison:\n"));
    Is_Trace_cmd(Trace(),comparison_cr->Print(0,TFile));
  }
  _num_substitutions++; // for the tlog
}

// ======================================================================
// based on CSE::Find_injury_update
// find the closest dominating expr
// ======================================================================
CODEREP *
LFTR::Find_SR_tempcr(STMTREP *iv_def, CODEREP *temp)
{
  Is_True(iv_def != NULL && temp != NULL,
	  ("LFTR::Find_SR_tempcr2, null iv_def or temp"));

  // continue searching forward until we find one that defines the temp
  for (STMTREP *tmp_def=iv_def; tmp_def; tmp_def=tmp_def->Next()) {
    if (OPERATOR_is_scalar_store (tmp_def->Opr()) &&
	tmp_def->Lhs()->Aux_id() == temp->Aux_id())
      return tmp_def->Lhs();
  }
  return NULL;
}

// ======================================================================
// replace an LFTR var in an expression with a substitute expression
// return resulting expression
// RECURSIVE
// ======================================================================
CODEREP *
LFTR::Replace_lftr_var(CODEREP *templt, AUX_ID lftr_var, CODEREP *new_expr)
{
  INT32 i;
  switch (templt->Kind()) {
    case CK_OP:
      for (i = 0; i < templt->Kid_count(); i++) {
	CODEREP *kid = templt->Opnd(i);
	CODEREP *tmp = Replace_lftr_var(kid, lftr_var, new_expr);
	if (tmp != NULL && tmp != kid) {
#ifdef KEY // bug 12770
	  CODEREP *cr = Alloc_stack_cr(0);
	  if (MTYPE_byte_size(templt->Dtyp()) > MTYPE_byte_size(tmp->Dtyp())) {
	    cr->Init_expr(OPC_I8I4CVT, tmp);
	    tmp = Htable()->Rehash(cr);
	  }
#endif
	  templt->Set_opnd(i,tmp);
	}
      }
      break;

    case CK_VAR:
      if (templt->Aux_id() == lftr_var)
	return new_expr;
      break;

    default:
      break;
  }
  return templt;
}

// find the corresponding comp occur for the real occur and delete
// Note: there may be no corresponding comp occur for the real occur.
// For example, a comp occur is not created for every comparison in
// the program because there are criteria about finding a lftr_var,
// the lftr_var is the loop IV, the rhs has to be invariant, etc.
void
LFTR::Remove_comp_occur(EXP_OCCURS *occur)
{
  Is_True(Lftr_on(), ("LFTR::Remove_comp_occur called with LFTR off"));
  Is_True(Is_comparison(occur->Occurrence()),
	  ("LFTR::Remove_comp_occur, need a comparison"));
  Is_True(occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
	  ("LFTR::Remove_comp_occur_stmt, occur must be a real occur"));
  Is_Trace(Trace(), (TFile,"LFTR::Remove_comp_occur\n"));

  // find the right comp occur list
  AUX_ID lftr_var = Find_lftr_var_id(occur->Occurrence());
  if (lftr_var == ILLEGAL_AUX_ID) {
    Is_Trace(Trace(),
	     (TFile,"LFTR::Remove_comp_occur, could not find lftr_var\n"));
    return;
  }
  LFTR_VAR *lftr_var_head = Find_comp_list(lftr_var);
  // the comp list may not exist - we screen out several comparisons in
  // Lftr_comparison().
  if (lftr_var_head == NULL) {
    Is_Trace(Trace(),
	   (TFile,"LFTR::Remove_comp_occur, could not find lftr_var_head\n"));
    return;
  }

  // now go through all comp occurs in comp_list
  EXP_OCCURS      *comp, *comp_prev = NULL;
  EXP_OCCURS_ITER  comp_iter;
  BOOL             found_comp = FALSE, found_stmt = FALSE;
  Is_Trace(Trace(), (TFile, "  --- checking 0x%p kid %d\n",
		     occur->Stmt(), occur->Stmt_kid_num()));
  Is_Trace_cmd(Trace(), occur->Print(TFile));
  Is_Trace(Trace(), (TFile, "      in\n"));
  Is_Trace_cmd(Trace(), occur->Stmt()->Print(TFile));
  FOR_ALL_NODE(comp, comp_iter, Init(lftr_var_head->Occ_list())) {
    // there are comp occurs for other expressions in this list -
    // ignore them
    Is_Trace(Trace(), (TFile, "  --- against  0x%p kid %d\n",
		       comp->Stmt(), comp->Stmt_kid_num()));
    Is_Trace_cmd(Trace(), comp->Print(TFile));
    Is_Trace(Trace(), (TFile, "      in\n"));
    Is_Trace_cmd(Trace(), comp->Stmt()->Print(TFile));
    if (occur->Occurrence() == comp->Occurrence() &&
	occur->Enclosed_in_stmt() == comp->Enclosed_in_stmt()) {
      found_stmt = TRUE;
      if (occur->Stmt_kid_num() == comp->Stmt_kid_num()) {
	Is_Trace(Trace(), (TFile,"LFTR::Remove_comp_occur, removed comp:\n"));
	Is_Trace_cmd(Trace(),comp->Occurrence()->Print(0,TFile));
	if (comp_prev == NULL)
	  lftr_var_head->Set_occ_list(comp->Next());
	else
	  comp_prev->Set_Next(comp->Next());
	lftr_var_head->Dec_list_size();

	Is_True(occur->Mult_real() || !found_comp,
		("LFTR::Remove_comp_occur: Multiple comps in same stmt "
		 "kid must correspond to Mult_real()"));
	found_comp = TRUE;
      }
    }
    else if (found_stmt &&
	     (occur->Enclosed_in_stmt() != comp->Enclosed_in_stmt() ||
	      occur->Stmt_kid_num() != comp->Stmt_kid_num())) {
      // We have found all the comp occurrences corresponding to the
      // given real occurrence.
      break;
    }
    comp_prev = comp;
  }
}

// ======================================================================
// find the corresponding comp occur and update the stmt pointer
// ======================================================================
void
LFTR::Update_comp_occur_stmt(EXP_OCCURS *occur, STMTREP *new_stmt)
{
  Is_True(Lftr_on(), ("LFTR::Update_comp_occur_stmt, LFTR must be on"));
  Is_True(Is_comparison(occur->Occurrence()),
	  ("LFTR::Update_comp_occur_stmt, need a comparison"));
  Is_True(occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
	  ("LFTR::Update_comp_occur_stmt, occur must be a real occur"));
  Is_Trace(Trace(),(TFile,"LFTR::Update_comp_occur_stmt\n"));
	  
  // find the right comp occur list
  AUX_ID lftr_var = Find_lftr_var_id(occur->Occurrence());
  if (lftr_var == ILLEGAL_AUX_ID) {
    Is_Trace(Trace(),
       (TFile,"LFTR::Update_comp_occur_stmt, could not find lftr_var\n"));
    return;
  }
  LFTR_VAR *lftr_var_head = Find_comp_list(lftr_var);
  // the comp list may not exist - we screen out several comparisons in
  // Lftr_comparison().
  if (lftr_var_head == NULL) {
    Is_Trace(Trace(),
       (TFile,"LFTR::Update_comp_occur_stmt, could not find lftr_var_head\n"));
    return;
  }

  // now go through all comp occurs in comp_list
  EXP_OCCURS *comp, *comp_next, *comp_prev = NULL, *update = NULL;
  EXP_OCCURS_ITER comp_iter(lftr_var_head->Occ_list());
  for( comp = comp_iter.First(); ! comp_iter.Is_Empty(); comp = comp_next) {
    comp_next = comp_iter.Next();
    // there are comp occurs for other expressions in this list - ignore them
    if (update) {
      if (comp_next == NULL || !update->Is_DPO_less_than(comp_next)) {
        comp->Set_Next(update);
        update->Set_Next(comp_next);
        break;
      }
      comp_prev = comp;
    }
    else if (occur->Occurrence() == comp->Occurrence() &&
	occur->Enclosed_in_stmt() == comp->Enclosed_in_stmt()) {
      Is_Trace(Trace(),(TFile,"LFTR::Update_comp_occur_stmt, old:\n"));
      Is_Trace_cmd(Trace(),comp->Enclosed_in_stmt()->Print(TFile));
      comp->Set_enclose_stmt(new_stmt);
      comp->Set_stmt_kid_num(1);
      Is_Trace(Trace(),(TFile,"new:\n"));
      Is_Trace_cmd(Trace(),comp->Enclosed_in_stmt()->Print(TFile));

      if (comp_next && !comp->Is_DPO_less_than(comp_next)) {
        // need to move comp forward
        if (comp_prev == NULL)
          lftr_var_head->Set_occ_list(comp_next);
        else 
          comp_prev->Set_Next(comp_next);
        update = comp;
        update->Set_Next(NULL);
      }
      else if (comp_prev && comp->Is_DPO_less_than(comp_prev)) {
        // need to move comp backward
        comp_prev->Set_Next(comp_next);
        lftr_var_head->Add_comparison(comp);
      }
      else
        break;
    }
    else
      comp_prev = comp;
  }
}

// ======================================================================
// Dump everything LFTR knows
// ======================================================================
void
LFTR::Print(FILE *fp)
{
  fprintf(fp,"%sLFTR::Print\n",DBar);
  fprintf(fp,"_trace=%c, _lftr_on=%c, _mem_pool=0x%p, _etable=0x%p\n",
	  _trace?'T':'F', _lftr_on?'T':'F', &_mem_pool, _etable);
  fprintf(fp,"_num_substitutions=%d\n%s",_num_substitutions, DBar);

  if (_hash_vec != NULL) {
    for (INT32 index=0; index<=_hash_size; index++) {
      LFTR_VAR *ltmp = _hash_vec[index];
      if (ltmp != NULL) {
	fprintf(fp,"_hash_vec[%d]:\n",index);
	LFTR_VAR *tmp;
	LFTR_VAR_ITER lftr_var_iter(ltmp);
	FOR_ALL_NODE(tmp, lftr_var_iter, Init())
	  tmp->Print(fp);
      }
    }
    fprintf(fp,"%s",DBar);
  }
}

// ======================================================================
// print a LFTR_VAR structure
// ======================================================================
void
LFTR_VAR::Print(FILE *fp)
{
  fprintf(fp,"  lftr_var: %d\n",Lftr_var());
  EXP_OCCURS *ctmp;
  EXP_OCCURS_ITER comp_iter(Occ_list());
  FOR_ALL_NODE(ctmp, comp_iter, Init()) {
    ctmp->Print(fp);
    ctmp->Occurrence()->Print(0,fp);
  }
}
