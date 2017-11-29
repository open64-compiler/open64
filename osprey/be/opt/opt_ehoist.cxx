//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_ehoist.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_ehoist.cxx,v $
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
//   Implements Code Hoisting.   
//   See comments with EXP_HOISTING::Generate_hoisted_occur() for the
//   hoisting algorithm.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "defs.h"
#include "tracing.h"
#include "opt_defs.h"
#include "config_wopt.h"
#include "opt_cfg.h"
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_etable.h"
#include "opt_estr.h"


class HOIST_SUMMARY
{
private:
  // These fields are not modified after Init() and Init_cfg_info().
  BB_NODE  *_bb;                 // this BB_NODE *
  UINT      _succ_mask;          // bit mask.  # of 1s is the number of succs

  HOIST_SUMMARY *_cd_hs;         // the closest dominating BB that the current BB control dependent on
  UINT      _which_cd_succ;      // which succ of the CD is postdominated by this BB

  // These fields will change on per-expr basis.
  UINT           _injured:1;
  EXP_ID         _exp_id:31;     // the exp_id that the following information represents
  HOIST_SUMMARY *_next;          // linked list for HOIST_SUMMARY worklist
  UINT           _succ_count;    // initialized to _succ_mask
  EXP_OCCURS    *_exp_occur;     // may point to the real-occ, phi-pred-occ, or hoisted-occ
  EXP_OCCURS    *_hoisted_occur; // cache the hoisted occurrence

public:
  void Init(void) {
    _exp_id = ETABLE::ILLEGAL_E_NUMBER;
    _cd_hs = NULL;
    _bb = NULL;
    _succ_mask = 0;
  }
  
  void Init_per_expr(EXP_ID id) {
    _exp_id = id;
    _injured = 0;
    _succ_count = _succ_mask;
    _exp_occur = NULL;
    _hoisted_occur = NULL;
    _next = NULL;
  }

  void Init_cfg_info(BB_NODE *bb, HOIST_SUMMARY *, ETABLE *);

  EXP_ID    Exp_id(void) const     { return _exp_id; }
  BOOL      Injured(void) const    { return (_injured != 0); }
  BB_NODE  *Bb(void) const         { return _bb; }
  BOOL      Anticipated(void)      { return _succ_count == 0; }
  HOIST_SUMMARY *Next(void) const  { return _next; }
  HOIST_SUMMARY *Cd_hs(void) const { return _cd_hs; }
  EXP_OCCURS    *Exp_occur(void) const      { return _exp_occur; }
  EXP_OCCURS    *Hoisted_occur(void) const      { return _hoisted_occur; }
  UINT           Which_cd_succ(void) const  { return _which_cd_succ; }

  void  Set_bb(BB_NODE *bb)                 { _bb = bb; }
  void  Set_injured(void)                   { _injured = 1; }
  void  Set_next(HOIST_SUMMARY *n)          { _next = n; }
  void  Set_exp_occur(EXP_OCCURS *occ)      { _exp_occur = occ; }
  void  Set_hoisted_occur(EXP_OCCURS *occ)  { _hoisted_occur = occ; }
  void  Update_succ_count(INT i)            { _succ_count &= ~(1 << i); }

  // return TRUE if succ is not processed
  BOOL  Succ_processed(INT i)          { return ((_succ_count & (1 << i)) == 0); }
  
  // return TRUE if the expr is anticipated at its CD node.
  BOOL  Anticipated_at_CD(CODEREP *) const;

  // return TRUE if the expr's def_occur can be ignored
  inline BOOL  Def_occur_allows_hoisting(EXP_OCCURS *) const;

  void  Print(FILE *fp) const;
};


class EXP_HOISTING {
private:
  // The following fields do not change after EXP_HOISTING construction.
  ETABLE         *_etable;
  EXP_WORKLST    *_exp_worklst;
  CFG            *_cfg;
  MEM_POOL       *_pool;
  HOIST_SUMMARY  *_summary;    // array of summary node indexed by BB-id

  // The following fields are per-expr.
  EXP_ID          _cur_exp_id;
  HOIST_SUMMARY  *_head; 
  HOIST_SUMMARY  *_tail;
  
  CFG            *Cfg(void) const          { return _cfg; }
  ETABLE         *Etable(void) const       { return _etable; }
  EXP_WORKLST    *Exp_worklst(void) const  { return _exp_worklst; }
  EXP_ID          Cur_exp_id(void) const   { return _cur_exp_id; }
  HOIST_SUMMARY  *Summary(IDTYPE bb_id) const { return &_summary[bb_id]; }
  
  void            Set_cur_exp_id(EXP_ID id) { _cur_exp_id = id; }

  void Clear_hoist_worklist(void) { _head = _tail = NULL; }

  void Append_hoist_worklist(HOIST_SUMMARY *hs) {
    hs->Set_next(NULL);
    if (_head == NULL) {
      _head = _tail = hs;
    } else {
      _tail->Set_next(hs);
      _tail = hs;
    }
  }
    
public:

  EXP_HOISTING(ETABLE *etable, MEM_POOL *pool)
  {
    _etable = etable;
    _cfg = etable->Cfg();
    _pool = pool;
    _summary = (HOIST_SUMMARY*)
      CXX_NEW_ARRAY(HOIST_SUMMARY, Cfg()->Total_bb_count(), _pool);

    CFG_ITER cfg_iter;
    BB_NODE *bb;
  
    FOR_ALL_ELEM (bb, cfg_iter, Init(Cfg())) {
      HOIST_SUMMARY *hs = Summary(bb->Id());
      hs->Init();
      hs->Init_cfg_info(bb, _summary, etable);
    }

    if (etable->Tracing()) {
      FOR_ALL_ELEM (bb, cfg_iter, Init(Cfg())) {
	HOIST_SUMMARY *hs = Summary(bb->Id());
	Is_Trace(hs->Cd_hs() != NULL, 
		 (TFile, "HOISTING: BB%d is control dependent on BB%d succ-edge %d.\n",
		  hs->Bb()->Id(), hs->Cd_hs()->Bb()->Id(), hs->Which_cd_succ()));
      }
    }
  }


  ~EXP_HOISTING(void)
  {
    // no destruction.  _summary should be deallocated when the Etable()->Mem_pool()
    // is gone.
  }

  MEM_POOL       *Pool(void) const         { return _pool; }

  inline void   Update_succ_count_rec(HOIST_SUMMARY *, UINT, CODEREP *, EXP_OCCURS *);
  EXP_OCCURS   *Get_hoisted_exp_occur(HOIST_SUMMARY *, CODEREP *, EXP_OCCURS *, BOOL);
  void          Generate_hoisted_occur(EXP_WORKLST *);
  
};

void
HOIST_SUMMARY::Print(FILE *fp) const
{
  (void) fprintf(fp, "HS> BB%d; CD on BB%d; Exp %d;\n",
		 Bb()->Id(), (Cd_hs() ? (INT) Cd_hs()->Bb()->Id() : -1),
		 Exp_id());
  (void) fprintf(fp, " ");
  if (Exp_occur() == NULL) {
    (void) fprintf(fp, "   <null>\n");
  }
  else {
    Exp_occur()->Print(TFile);
  }
  (void) fprintf(fp, " ");
  if (Hoisted_occur() == NULL) {
    (void) fprintf(fp, "   <null>\n");
  }
  else {
    Hoisted_occur()->Print(TFile);
  }
}

// Precompute the per-PU information into the HOIST_SUMMARY data structure once.
// Those information will be reused for each expression presented for code hoisting.
// For example, the control dependent node and number of successors.
//
void
HOIST_SUMMARY::Init_cfg_info(BB_NODE *bb, HOIST_SUMMARY *summary, ETABLE *etable)
{
  _bb = bb;
  _succ_mask = (1 << bb->Succ()->Len()) - 1;
  _cd_hs = NULL;

  BB_NODE *temp_bb;
  BB_NODE_SET_ITER rcfg_iter;

  // Identify the control dependence BB. 
  // Notice that we disable code hoisting if a BB has more than 
  // 1 control dependence node.
  //
  BB_NODE *cd_bb = NULL;
  FOR_ALL_ELEM( temp_bb, rcfg_iter, Init( bb->Rcfg_dom_frontier() ) ) {
    if (cd_bb != NULL)  // already assigned some value
      return;
    cd_bb = temp_bb;
  }
 
  // Skip ENTRY_BB because it is not allowed to hoist
  // any expression into the entry node.
  //
  // Skip hoisting to a CD that is not dominating the BB, otherwise
  // the expr need to be duplicated.
  //
  if (cd_bb == NULL || 
      cd_bb->Kind() == BB_ENTRY || 
      cd_bb == bb ||
      !cd_bb->Dominates(bb))
    return;

  // Fix 626386: 
  //
  // Consider this example:  
  // BB8 and BB9 forms a loop. BB8 is a loop header.
  // BB8 exits to BB10 (BB10 at lower nesting level).  
  // i=i+1 is in BB8. 
  // i*4 is in BB9. Therefore the expr i*4 in BB9 is injured and defined by phi at BB8.
  // i*4 is inserted at BB10.
  // Expr hoisting can insert at the common dominator of BB10 and BB9, i.e. BB8.
  // Inserted expr i*4 is marked injury because it should have the version of the
  // temp after injury repair.
  // The reload in BB9 and BB10 are then processed.
  // If the reload in BB10 is processed first, CSE found out i*4 needs a repair
  // from i=i+1, but i=i+1 has deeper nesting level than i*4 and CSE asserts.
  //
  // Solution: do not allow hoisting from a less frequent point to a higher frequent
  // point (actually call Str_red()->Update_happens_rarely_happens() to decide).
  // 
  if (!etable->Str_red()->Update_happens_rarely_enough(cd_bb, bb, NULL))
    return;

  BB_LIST_ITER bb_iter;
  BB_NODE *succ;
  INT i = 0;
  _which_cd_succ = (UINT) -1;
  FOR_ALL_ELEM( succ, bb_iter, Init(cd_bb->Succ()) ) {
    if (bb->Postdominates(succ)) {
      _which_cd_succ = i;
      break;
    }
    i++;
  }
  Is_True(_which_cd_succ != (UINT) -1, 
	  ("HOIST_SUMMARY::Init_cfg_info: cannot find succ that bb postdom."));

  // The HOIST_SUMMARY data structure only handles up to 32 successors.
  // Disable code hoisting if the CD has more than 32 successors.
  //
  if (cd_bb->Succ()->Len() >= sizeof(UINT)*8) {
    _which_cd_succ = 0;
    return;  // cd_hs is null when returning at this point
  }

  _cd_hs = &summary[cd_bb->Id()];
}


//  Update the succ counter of a BB.  The counter keeps tracks of the
//  successors edges that are anticipated.  If the BB becomes fully
//  anticipated, then locate and update its control dependent parent if
//  the expr is also anticipated at the CD parent.
//
inline void
EXP_HOISTING::Update_succ_count_rec(HOIST_SUMMARY *hs, UINT which_succ, CODEREP *expr, EXP_OCCURS *exp_occur)
{
  // Init HS if it is not already initialized
  if (hs->Exp_id() != Cur_exp_id())
    hs->Init_per_expr(Cur_exp_id());

  Is_True(0 <= (INT) which_succ && which_succ < hs->Bb()->Succ()->Len(),
	  ("EXP_HOISTING::Update_succ_count_rec: invalid which_succ."));

  if (hs->Exp_occur() == NULL && exp_occur != NULL) 
    hs->Set_exp_occur(exp_occur);

  // Succ_processed keeps track of which succ edge is set anticipated,
  // so there is no need to work on that edge again.
  if (!hs->Succ_processed(which_succ)) {
    // Update succ count.  If the expr is fully anticipated at this point,
    // recursively updates its CD parent.
    hs->Update_succ_count(which_succ);

    if (hs->Anticipated()) {
      Is_True(hs->Exp_occur() != NULL,
	      ("EXP_HOISTING::Update_succ_count_rec: "
	       "hs is fully anticipated but has null exp_occur."));
    }

    Is_Trace(Etable()->Tracing(), 
	     (TFile, "EXP_HOISTING: update succ count of BB%d due to e-ver %d%s\n",
	      hs->Bb()->Id(),
	      exp_occur != NULL ? (INT) exp_occur->E_version() : -1,
	      hs->Anticipated() ? ", becomes fully anticipated." : "."));

    if (hs->Anticipated() &&              // this node becomes fully anticipated
	hs->Cd_hs() != NULL &&            // this node has a CD parent
	
	// Fix 475644: see comments in Generate_hoisted_occur
	(exp_occur != NULL || hs->Cd_hs()->Exp_occur() != NULL) &&

	hs->Anticipated_at_CD(expr) &&    // the expr is anticipated at the CD parent
	hs->Def_occur_allows_hoisting(exp_occur)) 
      Update_succ_count_rec(hs->Cd_hs(), hs->Which_cd_succ(), expr, exp_occur);
  }
}


// See if two codereps (that might not be in htable) matches.
//  -- assume their kids have been hashed.
//
static 
BOOL CR_match_kids(CODEREP *cr1, CODEREP *cr2)
{
  if (cr1 == cr2) return TRUE;
  if (cr1->Kind() != cr2->Kind()) return FALSE;

  switch (cr1->Kind()) {
  case CK_IVAR:
    if (cr1->Ilod_base() != cr2->Ilod_base())
	return FALSE;
	if (cr1->Ivar_mu_node()->OPND() != cr2->Ivar_mu_node()->OPND()) 
	  return FALSE;
    if (cr1->Opr() != OPR_ILOADX) {
      if (cr1->Offset() != cr2->Offset())
	return FALSE;	// offset not the same
    }
    else {
      if (cr1->Index() != cr2->Index())
	return FALSE;	// index not the same
    }
    if (Get_mtype_class(cr1->Dtyp()) != Get_mtype_class(cr2->Dtyp()))
      return FALSE;	// type class not the same
    if (MTYPE_size_min(cr1->Dsctyp()) != MTYPE_size_min(cr2->Dsctyp()))
      return FALSE;	// size not the same

    Is_True(OPCODE_operator(cr1->Op()) != OPR_MLOAD,
	    ("CR_match_kids: OPR_MLOAD not supported in EPRE."));

    return TRUE;

  case CK_OP:
    {
      if (cr1->Op() != cr2->Op()) return FALSE;
      for (INT i = 0; i < cr1->Kid_count(); i++)
	if (cr1->Opnd(i) != cr2->Opnd(i))
	  return FALSE;
      if (OPCODE_operator(cr1->Op()) == OPR_INTRINSIC_OP &&
	  cr1->Intrinsic() != cr2->Intrinsic())
	return FALSE;
#ifdef KEY
      if (OPCODE_operator(cr1->Op()) == OPR_PURE_CALL_OP &&
          cr1->Call_op_aux_id() != cr2->Call_op_aux_id())
        return FALSE;
#endif
      if (OPCODE_operator(cr1->Op()) == OPR_CVTL && 
	  cr1->Offset() != cr2->Offset())
	return FALSE;
      return TRUE;
    }
  default:
    return FALSE;
  }
}

//  EXP_HOISTING::Get_hoisted_exp_occur() performs two functions.
//  It inserts OCC_HOISTED OCC_REAL_OCCURS into the worklist if necessary.
//  Otherwise it looks up the corr EXP_OCCUR node.
//
//  A hoisted occurrence is allocated and inserted into the real occurrence list if
//    1) the expr is fully anticipated at that node;
//    2) the expr is not fully anticipated at its CD parent;
//    3) the def_occur of the expr is not fully available
//
//  The lookup can happen when cond(2) or cond(3) is FALSE.
//  When cond(2) is FALSE, use the Def_occur at its CD parent.
//  When cond(3) is FALSE, use the Def_occur from the real occurrence.
//
//  Def_occur are also cached at the HOIST_SUMMARY.
//
EXP_OCCURS *
EXP_HOISTING::Get_hoisted_exp_occur(HOIST_SUMMARY *hs, CODEREP *cr, EXP_OCCURS *def_occur, BOOL real_occ_is_injured)
{
  Is_True(hs->Anticipated(),
	  ("EXP_HOISTING::Alloc_and_insert_exp_occur: expr not fully anticipated."));
  Is_True(hs->Exp_id() == Cur_exp_id(), 
	  ("EXP_HOISTING::wrong exp id %d in worklist of %d.", hs->Exp_id(), Cur_exp_id()));

  // already generated
  if (hs->Exp_id() == Cur_exp_id() &&
      hs->Hoisted_occur() != NULL) {
    // One of these must be true:
    // 1) the hoisted occur occurrence matches with the coderep;
    // 2) if the def-occur and the real-occ have different codreps (when performing
    //    extra alias analysis with IVAR),  the hoisted occur occurrence matches
    //    that of the def-occur.
    // 3) the cr is an inserted coderep.
    // 4) the real_occ is injured.
    //
    Is_True(hs->Hoisted_occur()->Occurrence() == cr ||
	    (cr->Kind() == CK_IVAR &&
	     (def_occur == NULL || 
	      CR_match_kids(hs->Hoisted_occur()->Occurrence(),def_occur->Occurrence()))) ||
	    cr->Coderep_id() == 0 || // inserted expr not rehashed 
	    real_occ_is_injured,
	    ("EXP_HOISTING::Get_hoisted_exp_occur: CRs (cr%d) and (cr%d) are different!",
	     hs->Hoisted_occur()->Occurrence()->Coderep_id(), cr->Coderep_id()));

    if (real_occ_is_injured)
      hs->Set_injured();

    return hs->Hoisted_occur();
  }

  Is_Trace(Etable()->Tracing(),
	   (TFile, "EXP_HOISTING: hs->Cd_hs():\n"));
  if (hs->Cd_hs() != NULL && hs->Cd_hs()->Exp_id() == Cur_exp_id()) {
    Is_Trace_cmd(Etable()->Tracing(), hs->Cd_hs()->Print(TFile));
  }
  else if (hs->Cd_hs() == NULL) {
    Is_Trace(Etable()->Tracing(), (TFile, "     is NULL\n"));
  } else {
    Is_Trace(Etable()->Tracing(), (TFile, "     has diff exp-id\n"));
  }

  // To continue looking up the chain of CDs, the cr* must match.
  // The cr* match protects against using the temp of a different injured expr.
  // Also check for Def_occur_allows_hoisting, so we don't hoist anything beyond
  // a redundant phi.
  //
  EXP_OCCURS *occ;
  if (hs->Cd_hs() != NULL &&
      hs->Cd_hs()->Exp_id() == Cur_exp_id() &&
      (hs->Cd_hs()->Exp_occur() == NULL ||
       CR_match_kids(hs->Cd_hs()->Exp_occur()->Occurrence(),cr)) &&
      hs->Def_occur_allows_hoisting(def_occur) &&
      hs->Cd_hs()->Anticipated()) {

    // One of its fully anticipated CD parent should hold the hoisted occurrence
    occ = Get_hoisted_exp_occur(hs->Cd_hs(), cr, def_occur, real_occ_is_injured);

    if (hs->Cd_hs()->Injured()) // need this for to propagate injured bit for inserted occ
      hs->Set_injured();
  } else {

    // This is the root of the hoisted expr chain.
    // If the def_occur does not dominates this node, create a new EXP_OCCUR node.
    // Otherwise, use the def_occur parameter.

    if (def_occur != NULL && 
	(def_occur->Bb() == hs->Bb() || 
	 def_occur->Bb()->Dominates(hs->Bb())) &&
	(def_occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR ||
	 (def_occur->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR && 
	 def_occur->Exp_phi()->Will_b_avail()))) {
      
      occ = def_occur;
      if (def_occur->Occurrence()->Coderep_id() == 0) 
	def_occur->Set_occurrence(cr);
      
      Is_True(def_occur->Occurrence() == cr || real_occ_is_injured ||
	      (cr->Kind() == CK_IVAR &&
	       (def_occur->Occurrence()->Ilod_base() == cr->Ilod_base() ||
		def_occur->Occurrence()->Istr_base() == cr->Ilod_base())),
	      ("EXP_HOISTING::Get_hoisted_exp_occur: def_occur and cr not match."));

    } else {

      // Enter cr into htable if the cr is an inserted coderep.
      BOOL tree_changed = FALSE;
      if (cr->Coderep_id() == 0) {
	if (cr->Kind() == CK_IVAR) {
	  cr->Set_ivar_mu_node(CXX_NEW(MU_NODE(*cr->Ivar_mu_node()),
				       Etable()->Opt_stab()->Occ_pool()));
	  cr->Set_ivar_occ(CXX_NEW(OCC_TAB_ENTRY(*cr->Ivar_occ()),
				   Etable()->Opt_stab()->Occ_pool()));
	}
	
	/* CVTL-RELATED start (correctness) */
	// Insert CVT/CVTL if necessary
	if (cr->Is_integral_load_store()) {
	  cr = Exp_worklst()->Save_use_cr(Etable(), cr);
	}
	/* CVTL-RELATED finish */

	cr = Etable()->Htable()->Rehash_tree(cr, FALSE,
					     &tree_changed, hs->Bb());
	cr->Set_e_num(Cur_exp_id());
      }

      // Generate new EXP_OCCUR
      EXP_OCCURS *new_occ = Etable()->Alloc_occurs_node(cr);  // also set Occurrence()

      new_occ->Set_hoisted_occ();  // Set EXP_OCCURS::OCC_REAL_OCCUR and EXP_OCCURS::OCC_HOISTED
      new_occ->Set_enclose_bb(hs->Bb());
      if (def_occur != NULL && 
	  (def_occur->Bb() == hs->Bb() || 
	   def_occur->Bb()->Dominates(hs->Bb())) &&
	  def_occur->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
	new_occ->Set_e_version(def_occur->E_version());
	new_occ->Set_def_occur(def_occur);
	occ = new_occ;
      } else {
	new_occ->Set_e_version(Exp_worklst()->Cur_e_version());
	Exp_worklst()->New_e_version();
	new_occ->Set_def_occur(NULL);
	occ = new_occ;
      }

      if (tree_changed) {
	new_occ->Set_rehash_changed_expr();
      }

      // TODO: implement faster insertion
      // The insertions are produced in a DPO order.
      // Insert_real_occ(occ);      // Insert into real_occ list
      // Notice that it is important that Insert_real_occ inserts this
      // occurrence after other occurrence with same bb_id, but still
      // maintains DPO order.
      Warn_todo("EXP_HOISTING::Generate_hoisted_exp_occur: "
		"inefficient insertion.");

      Exp_worklst()->Insert_occurrence(new_occ, hs->Bb());
      Is_Trace(Etable()->Tracing(),
	       (TFile, "HOISTING: Insert hoisted expr version %d (cr%d) at BB%d\n",
		occ->E_version(), cr->Coderep_id(), hs->Bb()->Id()));
    }
  }
  if (real_occ_is_injured)
    hs->Set_injured();
  hs->Set_hoisted_occur(occ);
  return occ;
}


//  Test if a CK_VAR is not fully anticipated at the *exit* of a BB.
//
inline BOOL
Lod_modified(const BB_NODE *bb, const CODEREP *cr) 
{
  Is_True(cr->Kind() == CK_VAR, ("Lod_modified: expecting CK_VAR."));
  Is_True(!cr->Is_flag_set(CF_IS_ZERO_VERSION), ("Lod_modified: zero ver not allowed."));
  if (cr->Defbb() != bb) {
    // different BB, check dominance
    if (cr->Defbb()->Dominates(bb)) 
      return FALSE;
  } else 
    // if same BB, always return FALSE because the hoisted expr will be
    // inserted at the exit of the BB.
    return FALSE;
  return TRUE;
}


//  Test if a CODEREP expr is fully anticipated at the *exit* of a BB.
//
BOOL
HOIST_SUMMARY::Anticipated_at_CD(CODEREP *cr) const
{
  BB_NODE *cd_bb = Cd_hs()->Bb();

  switch (cr->Kind()) {
  case CK_RCONST:
  case CK_CONST:
  case CK_LDA:
    return TRUE;
  case CK_VAR: 
    return !Lod_modified(cd_bb, cr);
  case CK_IVAR: {
    CODEREP *addr = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    if (addr->Kind() == CK_VAR) {
      if (Lod_modified(cd_bb, addr))
	return FALSE;
    }
    // simpler version of ilod_modified.
    const CODEREP *cr_vsym = cr->Ivar_mu_node()->OPND();
    if (Lod_modified(cd_bb, cr_vsym))
      return FALSE;
    return TRUE;
  }

  case CK_OP: {
    for (INT i = 0; i < cr->Kid_count(); i++) {
      CODEREP *kid = cr->Opnd(i);
      switch (kid->Kind()) {
      case CK_VAR:
	if (Lod_modified(cd_bb, kid))
	  return FALSE;
	break;
      case CK_IVAR:  // must be OPR_PARM
	return FALSE;
      case CK_CONST:
      case CK_RCONST:
      case CK_LDA:
	break;
      default:
	Is_True(FALSE, ("unexpected CK_KIND."));
      }
    }
    return TRUE;
  }
  default:
    Is_True(FALSE, ("unexpected CK_KIND."));
  }

  return TRUE;
}


//  This is to fix a problem that the expression is hoisted 
//  pass the phi function that defines its version.
//
//  Returns TRUE if def_occur is 
//    a) non-NULL and
//    b) definied by PHI and
//    c) not dominates the CD node
inline BOOL 
HOIST_SUMMARY::Def_occur_allows_hoisting(EXP_OCCURS *def_occur) const
{
  if (def_occur == NULL ||
      def_occur->Occ_kind() != EXP_OCCURS::OCC_PHI_OCCUR ||
      def_occur->Bb()->Dominates( Cd_hs()->Bb() ))
    return TRUE;
  return FALSE;
}


//  EXP_HOISTING::Generate_hoisted_occur() has two passes.
//
//  The first pass examines all real occurrences and phi-pred occurrences
//  needed insertion. If the occurrence is anticipated at its CD parent,
//  its CD parent is updated and it is entered into a worklist to be used 
//  in the second pass.  
//
//  The second pass examines all occurrence in the worklist, and call
//  Get_hoisted_exp_occur() to determine the dominating exp_occur after
//  code hoisting.  It also inserts new OCC_HOISTED real occurrence into
//  the real occurrence worklist.  Update the def_occur and e_version for
//  the original real occurrence and phi-pred, so that CSE will eliminate
//  the redundancy introduced by the OCC_HOISTED real occurrence.
//
void
EXP_HOISTING::Generate_hoisted_occur(EXP_WORKLST *exp_worklst)
{
  EXP_OCCURS         *real_occ;
  EXP_OCCURS         *phi_pred;
  EXP_OCCURS_ITER     real_occ_iter;
  EXP_OCCURS_ITER     phi_pred_iter;

  _exp_worklst = exp_worklst;
  Set_cur_exp_id(Exp_worklst()->E_num());
  Clear_hoist_worklist();

  FOR_ALL_NODE (real_occ, real_occ_iter, Init(Exp_worklst()->Real_occurs().Head())) {
    HOIST_SUMMARY *hs = Summary(real_occ->Bb()->Id());
    if (hs->Exp_id() != Cur_exp_id())
      hs->Init_per_expr(Cur_exp_id());

    if (!real_occ->Occurs_as_lvalue() &&    // not on the LHS
	hs->Cd_hs() != NULL &&              // it has a CD node
	hs->Exp_occur() == NULL &&          // hs not inserted 
	hs->Anticipated_at_CD(real_occ->Occurrence()) && // and anticipated at the CD node
	hs->Def_occur_allows_hoisting(real_occ->Def_occur())) {
      hs->Set_exp_occur(real_occ);
      Append_hoist_worklist(hs);
      Update_succ_count_rec(hs->Cd_hs(), hs->Which_cd_succ(), real_occ->Occurrence(), real_occ);
    }
  }
    
  FOR_ALL_NODE (phi_pred, phi_pred_iter, Init(Exp_worklst()->Phi_pred_occurs().Head())) {
      
    HOIST_SUMMARY *hs = Summary(phi_pred->Bb()->Id());
    if (hs->Exp_id() != Cur_exp_id())
      hs->Init_per_expr(Cur_exp_id());

    BB_LIST_ITER  succ_bb_iter;
    BB_NODE      *succ_bb;
    CODEREP      *insert_cr;
    INT           opnd_num = -1;
    EXP_PHI      *exp_phi = NULL;
    FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(phi_pred->Bb()->Succ())) {
      EXP_PHI *exp_phi = Etable()->Lookup_exp_phi(succ_bb, Exp_worklst()->Exp());
      if (exp_phi != NULL && exp_phi->Will_b_avail()) {
	opnd_num = succ_bb->Pred()->Pos(phi_pred->Bb());
	if (exp_phi->Need_insertion(opnd_num)) {
	  insert_cr = Etable()->
	    Alloc_and_generate_cur_expr(exp_phi->Result()->Occurrence(),
					succ_bb,
					opnd_num,
					Etable()->Per_expr_pool(),
					TRUE /*convert zero ver */);
	  if (hs->Cd_hs() != NULL &&           // it has a CD
	      hs->Exp_occur() == NULL &&       // hs not inserted

	      // Fix 475644:
	      //   Suppress hoisting if none of the hoisted expr are real occur.
	      //   We need at least one real occur beecause the inserted occur cannot 
	      //   supply a "hashed" coderep.
	      //
	      hs->Cd_hs()->Exp_occur() != NULL && // there is at least one real occur there

	      hs->Anticipated_at_CD(insert_cr) &&   // and anticipated at the CD node.
	      hs->Def_occur_allows_hoisting(exp_phi->Opnd(opnd_num))) {
	    Append_hoist_worklist(hs);
	    hs->Set_exp_occur(phi_pred);
	    Update_succ_count_rec(hs->Cd_hs(), hs->Which_cd_succ(), insert_cr, NULL);
	    break;
	  }
	}
      }
    }
  }


  // Go thru all REAL_OCC appended to the worklist
  //
  HOIST_SUMMARY *hs;
  for (hs = _head; hs != NULL; hs = hs->Next()) {

    Is_True(hs->Exp_id() == Cur_exp_id(), 
	    ("EXP_HOISTING::Generate_hoisted_occur: uninitialized var."));

    Is_Trace(Etable()->Tracing(),
	     (TFile, "-------\nEXP_HOISTING: Considering hs:\n"));
    Is_Trace_cmd(Etable()->Tracing(), hs->Print(TFile));

    // If there should be a dominating hoisted expr in the immed CD node.
    if (hs->Cd_hs() != NULL &&
	hs->Cd_hs()->Exp_id() == Cur_exp_id() &&
	hs->Cd_hs()->Anticipated()) {

      Is_Trace(Etable()->Tracing(), (TFile, "EXP_HOISTING: May hoist...\n"));

      if (hs->Exp_occur()->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR) {
	EXP_OCCURS *real_occ = hs->Exp_occur();

	if (hs->Cd_hs()->Hoisted_occur() != NULL) {
	  if (!CR_match_kids(hs->Cd_hs()->Hoisted_occur()->Occurrence(), real_occ->Occurrence()))
	    continue;
	}

	EXP_OCCURS *hoisted_occ = 
	  Get_hoisted_exp_occur(hs->Cd_hs(), real_occ->Occurrence(),
				real_occ->Def_occur(), real_occ->Injured_occ());
	Is_Trace(Etable()->Tracing(),
		 (TFile, " Get hoisted exp occur for real occ ver %d\n",
		  real_occ->E_version()));

	EXP_OCCURS *hoisted_def_occ = 
	  (hoisted_occ->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR && 
	   hoisted_occ->Def_occur() != NULL) 
	  ? hoisted_occ->Def_occur() : hoisted_occ;
	
	// Search the real-occur list for all real occurrence belongs to the same BB and has
	// same E-version.
	if (hoisted_def_occ->E_version() != real_occ->E_version()) {
	  IDTYPE      e_ver = real_occ->E_version();
	  BB_NODE    *bb    = real_occ->Bb();
	  EXP_OCCURS *occ   = real_occ;
	  do {
	    Is_Trace(Etable()->Tracing(),
		     (TFile, "HOISTING: updating real occ in BB%d from ver %d to %d\n",
		      occ->Bb()->Id(), occ->E_version(), hoisted_def_occ->E_version()));
	    occ->Set_e_version(hoisted_def_occ->E_version());
	    occ->Set_def_occur(hoisted_def_occ);
	    Exp_worklst()->Inc_hoisted_count();
	    occ = occ->Next();
	  } while (occ != NULL && occ->Bb() == bb && occ->E_version() == e_ver);
	}
      } 
    }
  }

  // Go thru all  PHI_PRED appended to the worklist
  //
  for (hs = _head; hs != NULL; hs = hs->Next()) {

    Is_True(hs->Exp_id() == Cur_exp_id(), 
	    ("EXP_HOISTING::Generate_hoisted_occur: uninitialized var."));

    Is_Trace(Etable()->Tracing(),
	     (TFile, "-------\nEXP_HOISTING: Considering hs:\n"));
    Is_Trace_cmd(Etable()->Tracing(), hs->Print(TFile));

    // If there should be a dominating hoisted expr in the immed CD node.
    if (hs->Cd_hs() != NULL &&
	hs->Cd_hs()->Exp_id() == Cur_exp_id() &&
	hs->Cd_hs()->Anticipated()) {

      Is_Trace(Etable()->Tracing(), (TFile, "EXP_HOISTING: May hoist...\n"));

      if (hs->Exp_occur()->Occ_kind() == EXP_OCCURS::OCC_PHI_PRED_OCCUR) {
	EXP_OCCURS   *insert_occ = hs->Exp_occur();
	BB_LIST_ITER  succ_bb_iter;
	BB_NODE      *succ_bb;
	CODEREP      *insert_cr = NULL;
	INT           opnd_num = -1;
	EXP_PHI      *exp_phi = NULL;
	FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(insert_occ->Bb()->Succ())) {
	  EXP_PHI *temp_phi = Etable()->Lookup_exp_phi(succ_bb, Exp_worklst()->Exp());
	  if (temp_phi != NULL) {
	    exp_phi = temp_phi;
	    opnd_num = succ_bb->Pred()->Pos(insert_occ->Bb());
	    if (exp_phi->Need_insertion(opnd_num)) {
	      insert_cr = Etable()->Get_cached_cur_expr(succ_bb, opnd_num);
	      Is_True(insert_cr, ("EXP_HOISTING::Generate_hoisted_occur: unable to get cached expr.")); 
	      Is_True(inCODEKIND(insert_cr->Kind(),CK_VAR|CK_RCONST|CK_CONST|CK_LDA)
		      || insert_cr->Coderep_id() == 0, // expr need rehashing
		      ("EXP_HOISTING::Generate_hoisted_occ: valno not cleared."));  
	      break;
	    }
	  }
	}
	
	Is_True(insert_cr != NULL,
		("EXP_HOISTING::Generate_hoisted_occur: can't generate coderep."));

	if (hs->Cd_hs()->Hoisted_occur() != NULL) {
	  if (!CR_match_kids(hs->Cd_hs()->Hoisted_occur()->Occurrence(), insert_cr))
	    continue;
	}

	EXP_OCCURS *hoisted_occ = Get_hoisted_exp_occur(hs->Cd_hs(), insert_cr, NULL, FALSE);

	// side effect of Get_hoisted_exp_occur: it updates the injured bit along the
	// the path.
	BOOL contains_injured_real_occ = hs->Cd_hs()->Injured();  

	EXP_OCCURS *hoisted_def_occ = 
	  (hoisted_occ->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR &&
	   hoisted_occ->Def_occur() != NULL) 
	  ? hoisted_occ->Def_occur() : hoisted_occ;

	Is_Trace(Etable()->Tracing(),
		 (TFile, "HOISTING: updating insert occ in BB%d to ver %d\n",
		  insert_occ->Bb()->Id(), hoisted_occ->E_version()));
	exp_phi->Set_opnd(opnd_num, hoisted_def_occ);
	
	// Set phi opnd has real occ so that later steps will not generate
	// an insertion.
	exp_phi->Set_has_real_occ(opnd_num);

	// Also reset the OCC_INSERTED_OCCUR bit in the phi-pred node
	insert_occ->Reset_inserted();

	// Fix 459760.  STR_RED::Find_iv_and_mult complains about the failure
	// to find an iv-update for an injured phi-pred node.  The phi-pred
	// node is first marked for insertion, and then later hoisted.  The injury bit
	// of the phi-pred node was set because there is an iv-update between
	// the phi-pred node and its def-occur.  The injury bit is irrelevant
	// because EPRE decided to insert the expr "LATE".  When hoisting
	// decides to hoist the insertion, it should reset the injury bit at
	// the same time. 
	//
	// Also see PV 628050:  if any reference of the hoisted expr is injured,
	// then all other references must be injured (they need to use the version
	// of the temp after ivupdate.)
	if (contains_injured_real_occ) {
	  exp_phi->Set_injured(opnd_num);
	  Etable()->Update_cached_cur_expr(succ_bb, opnd_num,
					   hoisted_occ->Occurrence());
	  exp_phi->Pred(opnd_num)->Set_occurrence(hoisted_occ->Occurrence());
	} 

	Exp_worklst()->Inc_hoisted_count();
      }
    }
  }

  Etable()->Inc_hoisted(Exp_worklst()->Hoisted_count());

  //  This is to fix an overlapped live range problem.
  //  It was because an expr is not hoisted, but one of its dominating expr is.
  //  See the example in sanity/wopt/COMPILE_ONLY/tlog/expr_hoist.c.
  //  The first expr has two CD and therefore we decided not to spend time optimizing it.
  //  The second expr has just one CD (the CD dominates the first expr) and hoisted
  //  above the first expr.

  FOR_ALL_NODE (real_occ, real_occ_iter, Init(Exp_worklst()->Real_occurs().Head())) {
    if (!real_occ->Occurs_as_lvalue() &&    // not on the LHS
	real_occ->Def_occur() != NULL &&
	real_occ->Def_occur()->E_version() != real_occ->E_version()) {
      Is_Trace(Etable()->Tracing(),
	       (TFile, "HOISTING: This is not a bug. "
		"e-num %d : e-version %d is renamed to e-version %d.", 
		Exp_worklst()->E_num(),
		real_occ->E_version(),
		real_occ->Def_occur()->E_version()));
      real_occ->Set_e_version(real_occ->Def_occur()->E_version());
    }
  }
}


// The followings are interface functions called from ETABLE.
// They are written this way to avoid polluting opt_etable.h by
// putting EXP_HOISTING there.
//
EXP_HOISTING *
New_EXP_HOISTING(ETABLE *etable, MEM_POOL *pool)
{
  return CXX_NEW(EXP_HOISTING(etable, pool), pool);
}


void
Delete_EXP_HOISTING(EXP_HOISTING *eh)
{
  CXX_DELETE(eh, eh->Pool());
}


void
EXP_WORKLST::Hoist_expression(EXP_HOISTING *exp_hoist)
{
  if (!WOPT_Enable_Ivar_Hoisting && Exp()->Kind() == CK_IVAR)
    return;

  exp_hoist->Generate_hoisted_occur(this);
  Is_True(Verify_dpo_order(Real_occurs()), 
	  ("EXP_WORKLST::Hoist_expression: Not in DPO order after "
	   "hoisted real_occurrence insertion"));
}
