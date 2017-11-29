/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_swp_target.cxx
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/cg_swp_target.cxx,v $
 *
 * =======================================================================
 * ======================================================================= */

#include <stdint.h>

#define USE_STANDARD_TYPES
#include <map>
#include "defs.h"
#include "mempool.h"
#include "tn.h"
#include "tn_set.h"
#include "bb.h"
#include "op.h"
#include "op_list.h"
#include "op_map.h"
#include "cgexp.h"
#include "cgtarget.h"
#include "register.h"
#include "cg_loop.h"
#include "cg_swp_options.h"
#include "cg_swp.h"
#include "cg_swp_target.h"
#include "tracing.h"
#include "pf_cg.h"
#include "cg_loop.h"
#include "calls.h"
#include "tag.h"
#include "targ_issue_port.h" // To get PROCESSOR_Version

/* ====================================================================
 *
 *  Gen_SWP_Branch
 *
 * ====================================================================
 */
void Gen_SWP_Branch(CG_LOOP &cl, bool is_doloop)
{
  BB *body = cl.Loop_header();
  OP *br_op = BB_branch_op(body);
  TN *label_tn = OP_opnd(br_op, Branch_Target_Operand(br_op));
  OPS ops = OPS_EMPTY;
  OP *op;
  TN *ar_ec = Build_Dedicated_TN ( ISA_REGISTER_CLASS_application,
				   (REGISTER)(REGISTER_MIN + 66), 
				   8);
  
  if (is_doloop) {

    // Generate br.ctop for doloop

    if(PROCESSOR_Version == 2)
      Build_OP (TOP_br_ctop, 
	      ar_ec, LC_TN,
	      Gen_Enum_TN(ECV_bwh_sptk),
	      Gen_Enum_TN(ECV_ph_few),
	      Gen_Enum_TN(ECV_dh),
	      label_tn, 
	      ar_ec, LC_TN, &ops);
    else
      Build_OP (TOP_br_ctop, 
	      ar_ec, LC_TN,
	      Gen_Enum_TN(ECV_bwh_dptk),
	      Gen_Enum_TN(ECV_ph_few),
	      Gen_Enum_TN(ECV_dh),
	      label_tn, 
	      ar_ec, LC_TN, &ops);

    Is_True(OP_code(br_op) == TOP_br_cloop,
	    ("Gen_SWP_Branch: SWP doloop must use TOP_br_cloop."));
    LC_Used_In_PU = TRUE;

  } else {

    TN *predicate = OP_opnd(br_op, OP_PREDICATE_OPND);

    Build_OP (TOP_br_wtop, 
	      ar_ec,
	      predicate,
	      Gen_Enum_TN(ECV_bwh_dptk),
	      Gen_Enum_TN(ECV_ph_few),
	      Gen_Enum_TN(ECV_dh),
	      label_tn, 
	      ar_ec, &ops);

    Is_True(OP_code(br_op) == TOP_br_cond,
	    ("Gen_SWP_Branch: SWP whileloop must use TOP_br."));

  }
  
  op = OPS_last(&ops);
  Set_OP_Tag (op, Gen_Tag());

  BB_Remove_Op(body, br_op);
  BB_Append_Ops(body, &ops);
}

/* ====================================================================
 *
 *  Gen_SWP_Branch_Predict
 *
 * ====================================================================
 */
void Gen_SWP_Branch_Predict(BB *body, BB *prolog, BB *epilog)
{
  // Generate brp.loop.imp instruction to reduce the branch penalty of
  // the backward branch. This needs to be generated very early to be
  // effective, takes about 5 fetch cycles.

  OPS prolog_ops = OPS_EMPTY;
  OP *br_op = BB_branch_op(body);
  
  TN *dest_label = OP_opnd(br_op, Branch_Target_Operand(br_op));

  TN *src_label = Gen_Tag_TN (Get_OP_Tag (br_op));

  Build_OP(TOP_brp,
	   Gen_Enum_TN(ECV_ipwh_loop),
	   Gen_Enum_TN(ECV_ih_imp),
	   dest_label,
	   src_label,
	   &prolog_ops);

  BB_Prepend_Ops(prolog, &prolog_ops);
}

/* ====================================================================
 *
 *  Undo_SWP_Branch
 *
 * ====================================================================
 */
void Undo_SWP_Branch(CG_LOOP &cl, bool is_doloop)
{
  BB *body = cl.Loop_header();
  OP *br_op = BB_branch_op(body);
  TN *label_tn = OP_opnd(br_op, Branch_Target_Operand(br_op));
  OPS ops = OPS_EMPTY;
  
  if (is_doloop) {

    // Generate br.ctop for doloop

    if(PROCESSOR_Version == 2)
      Build_OP (TOP_br_cloop, 
	      LC_TN,
	      Gen_Enum_TN(ECV_bwh_sptk),
	      Gen_Enum_TN(ECV_ph_few),
	      Gen_Enum_TN(ECV_dh),
	      label_tn, 
	      LC_TN, &ops);
    else
      Build_OP (TOP_br_cloop, 
	      LC_TN,
	      Gen_Enum_TN(ECV_bwh_dptk),
	      Gen_Enum_TN(ECV_ph_few),
	      Gen_Enum_TN(ECV_dh),
	      label_tn, 
	      LC_TN, &ops);
      

    Is_True(OP_code(br_op) == TOP_br_ctop,
	    ("Undo_SWP_Branch: SWP doloop must use TOP_br_ctop."));
    LC_Used_In_PU = TRUE;

  } else {

    TN *predicate = OP_opnd(br_op, OP_PREDICATE_OPND);

    Build_OP (TOP_br_cond, 
	      predicate,
	      Gen_Enum_TN(ECV_bwh_dptk),
	      Gen_Enum_TN(ECV_ph_few),
	      Gen_Enum_TN(ECV_dh),
	      label_tn, &ops);

    Is_True(OP_code(br_op) == TOP_br_wtop,
	    ("Undo_SWP_Branch: SWP whileloop must use TOP_br_wtop."));

  }
  
  BB_Remove_Op(body, br_op);
  BB_Append_Ops(body, &ops);

  CG_LOOP_Init_Op(BB_branch_op(body));
}



/* ====================================================================
 *
 * SWP_Loop_Init_Fini
 *
 * ====================================================================
 */
void
SWP_Loop_Init_Fini(bool is_doloop,
		   INT stage_count,
		   OPS *prolog_ops,
		   OPS *body_ops,
		   OPS *epilog_ops)
{ 
  // br.wtop has already been generate at the CG_LOOP level.

  INT tn_size = 4;

  TN *ar_ec = Build_Dedicated_TN ( ISA_REGISTER_CLASS_application,
				   (REGISTER)(REGISTER_MIN + 66), 
				   8);
  
  TN *epilog_count = Gen_Literal_TN(stage_count, tn_size);
  
  const ISA_OPERAND_INFO *oinfo;
  oinfo = ISA_OPERAND_Info(TOP_mov_t_ar_i_i);
  const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Operand(oinfo, 1);
  ISA_LIT_CLASS lc = ISA_OPERAND_VALTYP_Literal_Class(vtype);
  if (ISA_LC_Value_In_Class(stage_count, lc)){
	  Build_OP (TOP_mov_t_ar_i_i, ar_ec, True_TN, epilog_count, prolog_ops);
  }
  else{
	  TN *tmp_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer, tn_size);
	  Exp_COPY(tmp_tn, epilog_count, prolog_ops);
	  Exp_COPY(ar_ec, tmp_tn, prolog_ops);
  }
  
  // Reset CFM.rrb pr
  Build_OP(TOP_clrrrb_pr, prolog_ops);

  // TODO: set the clrrrb in epilog depends on the number 
  //       of the alloc in current PU.
  // In current, we don't need clrrrb in epilog because of
  // single alloc in each PU!

  // Reset CFM.rrb 
  //Build_OP(TOP_clrrrb, epilog_ops);

  // Initialize the rotating predicate registers
  //   mov pr.rot = 1 << 16 for doloop
  //   mov pr.rot = 0 for whileloop
  TN *pr_mask = Gen_Literal_TN(is_doloop ? 1 << 16: 0, 4);
  Build_OP(TOP_mov_t_pr_i,
	   True_TN, /* controlling predicate */
	   pr_mask,   /* immediate operand */
	   prolog_ops);
}


/* ====================================================================
 *
 * SWP_Exp_Copy
 *
 * ====================================================================
 */
void SWP_Exp_COPY(TN *result, TN *opnd, OPS *ops)
{
  if (TN_register_class(result) == ISA_REGISTER_CLASS_predicate) {
    if (opnd == True_TN) {
      Build_OP (TOP_cmp_i_eq, result, True_TN, 
		True_TN, Gen_Literal_TN(0, 4), Zero_TN, ops);
      return;
    }
  }
  Exp_COPY(result, opnd, ops); 
}


/* ====================================================================
 *
 * SWP_Slots_Per_Cycle
 *    Returns a maximum number of slots (i.e. instructions) per cycle
 *    (i.e. group).
 *
 * ====================================================================
 */
INT32 SWP_Max_Slots_Per_Cycle() 
{
  // For Itanium(TM) : at most 2 bundles, each with 3 instructions, per group
  return 6;
}


/* ====================================================================
 *
 *  Convert all invariant predicate into a computation 
 *
 * ====================================================================
 */
void Remove_Invariant_Predicates(CG_LOOP& cl, bool trace)
{
  BB *prolog = cl.Prolog_end();
  BB *body = cl.Loop_header();
  CXX_MEM_POOL pool("Temp TN_SET", FALSE);

  TN_SET *predicate_tn_defs = TN_SET_Create_Empty(Last_TN + 1, pool());

  // collect all predicate definitions
  //
  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    for (INT i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      if (TN_is_register(tn) &&
	  !TN_is_dedicated(tn) &&
	  TN_register_class(tn) == ISA_REGISTER_CLASS_predicate)
	predicate_tn_defs = TN_SET_Union1D(predicate_tn_defs, tn, pool());
    }
  }

  // transform each invariant predicate into a new variant predicate
  //  initialized in the prolog.
  //
  std::map<TN*, TN*> tn_map;
  OPS prolog_ops = OPS_EMPTY;
  OPS body_ops = OPS_EMPTY;
  FOR_ALL_BB_OPs(body, op) {
    if (OP_has_predicate(op)) {
      TN *predicate_tn = OP_opnd(op, OP_PREDICATE_OPND);
      if (!TN_is_dedicated(predicate_tn) &&
	  !TN_SET_MemberP(predicate_tn_defs, predicate_tn)) {
	TN *new_tn;
	if (tn_map.find(predicate_tn) == tn_map.end()) {
	  // generate a new tn
	  new_tn = Dup_TN(predicate_tn);
	  tn_map[predicate_tn] = new_tn;
	  TN *int_tn = Build_TN_Of_Mtype(MTYPE_I4);

	  // generate  tn = 0; (p) tn = 1; in the prolog
	  // and generate  cmp.ne p_tn, p = 0, tn in the body
	  
	  Build_OP(TOP_adds,
		   int_tn,
		   True_TN,
		   Gen_Literal_TN(0, 4),
		   Zero_TN,
		   &prolog_ops);
	  Build_OP(TOP_adds,
		   int_tn,
		   predicate_tn,
		   Gen_Literal_TN(1, 4),
		   Zero_TN,
		   &prolog_ops);
	  Build_OP(TOP_cmp_i_ne,
		   new_tn,
		   True_TN, 
		   True_TN,
		   Gen_Literal_TN(0, 4), int_tn,
		   &body_ops);

	  if (trace)
	    fprintf(TFile, "Remove_Invariant_Predicates: TN%d\n",
		    TN_number(predicate_tn));
	} else
	  new_tn = tn_map[predicate_tn];
	Set_OP_opnd(op, OP_PREDICATE_OPND, new_tn);
      }
    }
  }
  BB_Append_Ops(prolog, &prolog_ops);
  BB_Prepend_Ops(body, &body_ops);
}


/* ====================================================================
 *
 *  Convert all p0 conditional cmp into unconditional form
 *
 * ====================================================================
 */
void Unc_Promotion(CG_LOOP& cl, bool trace)
{
  BB *prolog = cl.Prolog_end();
  BB *body = cl.Loop_header();
  CXX_MEM_POOL pool("Temp TN_SET", FALSE);

  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    if (OP_has_predicate(op) &&
	!OP_cond_def(op)) {

      // Convert into unc form 
      TOP unc_top = CGTARG_Get_unc_Variant(OP_code(op));
      if (unc_top != TOP_UNDEFINED && unc_top != OP_code(op)) {
	OP_Change_Opcode(op, unc_top);
	CGTARG_Init_OP_cond_def_kind(op);
      }
    }
  }
}



/* ====================================================================
 *
 *  Remove MOVL to avoid special template requirement and register 
 *  requirement.
 *
 * ====================================================================
 */
void Hoist_MOVL(CG_LOOP& cl, bool trace)
{
  BB *prolog = cl.Prolog_end();
  BB *body = cl.Loop_header();
  // collect all predicate definitions
  //
  std::vector<OP*> movl_ops;

  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    if (OP_code(op) == TOP_movl)
      movl_ops.push_back(op);
  }

  for (INT i = 0; i < movl_ops.size(); i++) {
    OP *op = movl_ops[i];
    TN *tn = OP_result(op, 0);
    TN *new_tn = Dup_TN(tn);
    OPS ops = OPS_EMPTY;
    Exp_COPY(tn, new_tn, &ops);
    // bug fix for OSP_190
    // should set the predicate control operand for the 
    // new added op with predicate oprand tn of previous op
    if (OP_has_predicate(op)) {
      TN *pre_opnd_tn = OP_opnd(op, OP_PREDICATE_OPND);
      Set_OP_opnd(ops.first, OP_PREDICATE_OPND, pre_opnd_tn);  	 
      Set_OP_opnd(ops.last, OP_PREDICATE_OPND, pre_opnd_tn);  	 
    }
    BB_Insert_Ops(body, op, &ops, TRUE /*before*/);
    BB_Remove_Op(body, op);
    BB_Append_Op(prolog, op);
    Set_OP_result(op, 0, new_tn);
    Set_OP_opnd(op, OP_PREDICATE_OPND, True_TN); /* Be sure that it is always loaded in the prolog. */
    if (trace)
      fprintf(TFile, "Hoist_MOVL:  TN%d\n", TN_number(tn));
  }
}


// Construct a data structure to locate all defs and uses of a TN,
// using the OP_VECTOR::index.   The properties of the OP_VECTOR::index
// is that it's ordered.
// 
struct TN_DU {
  typedef OP_VECTOR::index_type index_type;
  std::vector<index_type> defs;
  std::vector<index_type> uses;

  bool TN_is_invariant() const {
    return defs.size() == 0;
  }

  // Returns true if the TN is not modified in the range [first,last)
  bool TN_unchanged(index_type first, index_type last) {
    for (int i = 0; i < defs.size(); i++) {
      index_type t = defs[i];
      if (first <= t && t < last)
	return false;
    }
    return true;
  }

  // Returns true if the TN can be assigned a non-rotating register
  bool TN_can_use_non_rotating_reg(TN *tn, OP_VECTOR& op_vec) {
    
    // d is set to the earliest definition, set to MAX_INT if there is no definitions
    index_type d = defs.size() > 0 ? defs[0] : INT32_MAX;

    // an omega 0 use is always OK
    // an omega 1 use is OK if the use is before the earliest definition
    // an omega >1 use is never OK

    for (int j = 0; j < uses.size(); j++) {
      index_type u = uses[j];
      OP *op = op_vec[u];

      for (int i = 0; i < OP_opnds(op); i++) {
	if (tn == OP_opnd(op, i)) {
	  int omega = OP_omega(op, i);
	  if (omega >= 1) {
	    if (omega > 1)
	      return false;
	    if (d < u)  // omega == 1
	      return false;
	  }
	}
      }
    }
    return true;
  }
};


// Construct a TN to TN_DU mapping.
//
struct TN_DU_MAP {

  typedef std::map<TN *, TN_DU>::iterator iterator;
  std::map<TN *, TN_DU> TN_DU_map;

  iterator begin() {
    return TN_DU_map.begin();
  }

  iterator end() {
    return TN_DU_map.end();
  }

  TN_DU& operator[](TN *tn) {
    return TN_DU_map[tn];
  }

  // Build a TN_DU data structure for each TN
  // referenced in the BB.   And also assign an OP-number
  // to each OP *.   The TN_DU represents all occurrences
  // of defs and uses of the TN using the OP-number.
  //
  TN_DU_MAP(OP_VECTOR& op_vec, bool trace) {

    for (INT op_num = 0; op_num < op_vec.size(); op_num++) {
      OP *op = op_vec[op_num];
      INT i;
      for (i = 0; i < OP_results(op); i++) {
	TN *tn = OP_result(op,i);
	if (TN_is_register(tn) && 
	    !TN_is_dedicated(tn) &&
	    !TN_is_const_reg(tn)) {
	  if (TN_DU_map.find(tn) == TN_DU_map.end()) 
	    TN_DU_map[tn] = TN_DU();
	  TN_DU_map[tn].defs.push_back(op_num);
	}
      }
      for (i = 0; i < OP_opnds(op); i++) {
	TN *tn = OP_opnd(op,i);
	if (TN_is_register(tn) && 
	    !TN_is_dedicated(tn) &&
	    !TN_is_const_reg(tn)) {
	  if (TN_DU_map.find(tn) == TN_DU_map.end()) 
	    TN_DU_map[tn] = TN_DU();
	  TN_DU_map[tn].uses.push_back(op_num);
	}
      }
    }

    // Trace the TN_DU data structure
    if (trace) {
      for (iterator it = TN_DU_map.begin(); it != TN_DU_map.end(); it++) {
	TN *tn = (*it).first;
	TN_DU &lrs = (*it).second;
	fprintf(TFile, "Remove_Non_Definite_Dependence: TN_DU of TN%d: defs={", TN_number(tn));
	{
	  for (int i = 0; i < lrs.defs.size(); i++) {
	    fprintf(TFile, "%d", lrs.defs[i]);
	    if (i != lrs.defs.size()-1) fputc(',', TFile);
	  }
	}
	fprintf(TFile, "}, uses={");
	{
	  for (int i = 0; i < lrs.uses.size(); i++) {
	    fprintf(TFile, "%d", lrs.uses[i]);
	    if (i != lrs.uses.size()-1) fputc(',', TFile);
	  }
	}
	fprintf(TFile, "}\n");
      }
    }
  }
};



/* ====================================================================
 *
 * tn_is_needed_in_epilog
 *
 *   Using the increment feature on memory ops will cause the new
 *   value of the index to destroy the previous value.  When we need
 *   the previous value to complete the epilog sequence, we need to
 *   use rotating registers to save the previous value, or decrement
 *   the value in the epilog before it is used (not yet implemented).
 *
 * ====================================================================
 */
BOOL static tn_is_needed_in_epilog (TN *tn)
{ BB *epilog = CG_LOOP_epilog;
  CG_LOOP_BACKPATCH *bp;
  for (bp = CG_LOOP_Backpatch_First(epilog, NULL); bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    if (body_tn == tn) {
      return TRUE;
    }
  }
  return FALSE;
}



/* ====================================================================
 *
 * OP_owns_the_base_TN
 *
 *   If there is no other dependence memop using the base TN,
 *   then this OP owns it.
 *
 * ====================================================================
 */
bool OP_owns_the_base_TN(OP *op, TN *base, TN_DU& tn_du, OP_VECTOR& op_vec) 
{
  for (int j = 0; j < tn_du.uses.size(); j++) {
    int u = tn_du.uses[j];
    OP *op2 = op_vec[u];

    if (op != op2 && OP_memory(op2)) {
      if (OP_store(op) || OP_store(op2))
	return false;
    }
  }
  return true;
}



/* ====================================================================
 *
 * Remove cross-iteration non-defnite TN dependence by 
 * introducing copies.
 *
 * ====================================================================
 */
BOOL Remove_Non_Definite_Dependence(CG_LOOP &cl, bool cg_loop_init, bool trace)
{
  BB *body = cl.Loop_header();

  OP_VECTOR op_vec(body);
  TN_DU_MAP TN_DU_map(op_vec, trace);

  // Based on the TN_DU data structure, determine if
  // a TN has non-definite dependence.  If yes, generate a copy
  //
  int copy_inserted = 0;
  OPS ops = OPS_EMPTY;

  for (TN_DU_MAP::iterator it = TN_DU_map.begin(); it != TN_DU_map.end(); it++) {

    TN *tn = (*it).first;
    TN_DU &lrs = (*it).second;

    if (TN_is_dedicated(tn)) continue;

    bool need_copy = true;

    // there is no defs, then all uses are invariants
    if (lrs.TN_is_invariant())
      need_copy = false;

    // if the first def is uncond def, all uses are definite
    if (need_copy) 
      if (lrs.defs.size() > 0) {
	OP *op = op_vec[lrs.defs[0]];
	if (!OP_cond_def(op))
	  need_copy = false;
      }

    // the case where we have two complementary conditional defs
    // both coming before the first use
    if (need_copy) {
      if (lrs.defs.size() >= 2  && lrs.uses.size() > 0) {
        if (lrs.defs[0] < lrs.uses[0] && lrs.defs[1] < lrs.uses[0]) {
          OP *def1 = op_vec[lrs.defs[0]];
          OP *def2 = op_vec[lrs.defs[1]];
          if (OP_cond_def(def1) && OP_cond_def(def2)) {
            TN *pred1 = OP_opnd(def1, OP_PREDICATE_OPND);
            TN *pred2 = OP_opnd(def2, OP_PREDICATE_OPND);
            if (pred1 != pred2) {
              DEF_KIND kind;
              OP *pred1_def = TN_Reaching_Value_At_Op(pred1,def1,&kind,TRUE);
              OP *pred2_def = TN_Reaching_Value_At_Op(pred2,def2,&kind,TRUE);
              if (kind == VAL_KNOWN && pred1_def && pred1_def == pred2_def) {
                need_copy = false;
              }
            }
          }
        }
      }
    }

    // if the predicate of every use is covered by some defs,
    // the uses are definite
    if (need_copy) {
      bool all_uses_covered = true;
      for (int i = 0; i < lrs.uses.size(); i++) {
	bool found_covering_def = false;
	OP *op = op_vec[lrs.uses[i]];
	if (OP_cond_def(op)) {
	  TN *pred_tn = OP_opnd(op, OP_PREDICATE_OPND);
	  for (int j = lrs.defs.size() - 1; j >= 0; j--) {
	    if (lrs.defs[j] < lrs.uses[i]) {
	      OP *defop = op_vec[lrs.defs[j]];
	      TN *def_pred_tn = OP_opnd(defop, OP_PREDICATE_OPND);
	      if (def_pred_tn == pred_tn &&
		  TN_DU_map[pred_tn].TN_unchanged(lrs.defs[j], lrs.defs[i])) {
		found_covering_def = true;
		break;
	      }
	    }
	  }
	}
	if (!found_covering_def) {
	  all_uses_covered = false;
	  break;
	}
      }
      if (all_uses_covered) {
	BB *epilog = CG_LOOP_epilog;
	CG_LOOP_BACKPATCH *bp;
	for (bp = CG_LOOP_Backpatch_First(epilog, NULL); bp; bp = CG_LOOP_Backpatch_Next(bp)) {
	  TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
	  if (body_tn == tn) {
	    all_uses_covered = false;
	    break;
	  }
	}
      }
      if (all_uses_covered) 
	need_copy = false;
    }

    if (need_copy) {
      // create a definite copy
      Exp_COPY(tn, tn, &ops); 
      copy_inserted++;
      if (trace)
	fprintf(TFile, "Remove_Non_Definite_Dependence: TN%d\n", TN_number(tn));
    }
  }

  OP *old_first_op = BB_first_op(body);
  BB_Prepend_Ops(body, &ops);

  if (cg_loop_init) {
    for (OP *op = BB_first_op(body); op != old_first_op; op = OP_next(op)) {
      CG_LOOP_Init_Op(op);
      for (INT i = 0; i < OP_opnds(op); i++) {
	if (i != OP_PREDICATE_OPND && TN_is_register(OP_opnd(op,i))
	    && ! TN_is_dedicated(OP_opnd(op,i))) 
	  Set_OP_omega(op, i, 1);
      }
    }
  }

  if (trace) 
    fprintf(TFile, "Remove_Non_Definite_Dependence: %d copy inserted\n", 
	    copy_inserted);

  return TRUE;
}


// Return the base update form of the OP
//
BASE_UPDATE
OP_convertible_to_base_update(OP *op)
{
  if (OP_load(op) || OP_store(op)) {
    switch (OP_code(op)) {
    case TOP_ld1:  
    case TOP_ld2:
    case TOP_ld4:
    case TOP_ld8:
    case TOP_ldf8:
    case TOP_ldfd:
    case TOP_ldfe:
    case TOP_ldfs:
      return (BASE_UPDATE) (REG_BASE_UPDATE | IMM_BASE_UPDATE);
    case TOP_st1:  
    case TOP_st2:  
    case TOP_st4:  
    case TOP_st8:  
    case TOP_stf8: 
    case TOP_stfd: 
    case TOP_stfe: 
    case TOP_stfs: 
      return IMM_BASE_UPDATE;
    }
  }
  return NO_BASE_UPDATE;
}


BASE_UPDATE OP_base_update_kind(OP *op)
{
  if (OP_load(op) || OP_store(op)) {
    switch (OP_code(op)) {
    case TOP_ld1_i:  
    case TOP_ld2_i:
    case TOP_ld4_i:
    case TOP_ld8_i:
    case TOP_ldf8_i:
    case TOP_ldfd_i:
    case TOP_ldfe_i:
    case TOP_ldfs_i:
    case TOP_st1_i:  
    case TOP_st2_i:  
    case TOP_st4_i:  
    case TOP_st8_i:  
    case TOP_stf8_i: 
    case TOP_stfd_i: 
    case TOP_stfe_i: 
    case TOP_stfs_i: 
      return IMM_BASE_UPDATE;
    case TOP_ld1_r:  
    case TOP_ld2_r:
    case TOP_ld4_r:
    case TOP_ld8_r:
    case TOP_ldf8_r:
    case TOP_ldfd_r:
    case TOP_ldfe_r:
    case TOP_ldfs_r:
      return REG_BASE_UPDATE;
    }
  }
  return NO_BASE_UPDATE;
}


// Returns the address base opnd
INT OP_base_opnd_num(OP *op)
{
  if (OP_load(op) || OP_store(op)) {
    switch (OP_code(op)) {
    case TOP_ld1:  
    case TOP_ld2:
    case TOP_ld4:
    case TOP_ld8:
    case TOP_ldf8:
    case TOP_ldfd:
    case TOP_ldfe:
    case TOP_ldfs:
    case TOP_st1:  
    case TOP_st2:  
    case TOP_st4:  
    case TOP_st8:  
    case TOP_ld1_i:  
    case TOP_ld2_i:
    case TOP_ld4_i:
    case TOP_ld8_i:
    case TOP_ldf8_i:
    case TOP_ldfd_i:
    case TOP_ldfe_i:
    case TOP_ldfs_i:
    case TOP_st1_i:  
    case TOP_st2_i:  
    case TOP_st4_i:  
    case TOP_st8_i:  
    case TOP_ld1_r:  
    case TOP_ld2_r:
    case TOP_ld4_r:
    case TOP_ld8_r:
    case TOP_ldf8_r:
    case TOP_ldfd_r:
    case TOP_ldfe_r:
    case TOP_ldfs_r:
      return 3;
    case TOP_stf8: 
    case TOP_stfd: 
    case TOP_stfe: 
    case TOP_stfs: 
    case TOP_stf8_i: 
    case TOP_stfd_i: 
    case TOP_stfe_i: 
    case TOP_stfs_i: 
      return 2;
    }
  }
  return -1;
}


// Returns the address base opnd
INT OP_base_res_num(OP *op)
{
  if (OP_load(op) || OP_store(op)) {
    switch (OP_code(op)) {
    case TOP_ld1:  
    case TOP_ld2:
    case TOP_ld4:
    case TOP_ld8:
    case TOP_ldf8:
    case TOP_ldfd:
    case TOP_ldfe:
    case TOP_ldfs:
    case TOP_ld1_i:  
    case TOP_ld2_i:
    case TOP_ld4_i:
    case TOP_ld8_i:
    case TOP_ldf8_i:
    case TOP_ldfd_i:
    case TOP_ldfe_i:
    case TOP_ldfs_i:
    case TOP_ld1_r:  
    case TOP_ld2_r:
    case TOP_ld4_r:
    case TOP_ld8_r:
    case TOP_ldf8_r:
    case TOP_ldfd_r:
    case TOP_ldfe_r:
    case TOP_ldfs_r:
      return 1;
    case TOP_st1:  
    case TOP_st2:  
    case TOP_st4:  
    case TOP_st8:  
    case TOP_st1_i:  
    case TOP_st2_i:  
    case TOP_st4_i:  
    case TOP_st8_i:  
    case TOP_stf8: 
    case TOP_stfd: 
    case TOP_stfe: 
    case TOP_stfs: 
    case TOP_stf8_i: 
    case TOP_stfd_i: 
    case TOP_stfe_i: 
    case TOP_stfs_i: 
      return 0;
    }
  }
  return -1;
}


// Returns the address base opnd
INT OP_imm_opnd_num(OP *op)
{
  if (OP_load(op) || OP_store(op)) {
    switch (OP_code(op)) {
    case TOP_ld1_i:  
    case TOP_ld2_i:
    case TOP_ld4_i:
    case TOP_ld8_i:
    case TOP_ldf8_i:
    case TOP_ldfd_i:
    case TOP_ldfe_i:
    case TOP_ldfs_i:
      return 4;
    case TOP_st1_i:  
    case TOP_st2_i:  
    case TOP_st4_i:  
    case TOP_st8_i:
      return 5;
    case TOP_stf8_i: 
    case TOP_stfd_i: 
    case TOP_stfe_i: 
    case TOP_stfs_i: 
      return 4;
    }
  }
  return -1;
}


INT32 OP_incr_opnd_num(TOP top)
{
  // the last operand
  return (TOP_fixed_opnds(top) - 1);
}

INT32 OP_incr_opnd_num(OP *op)
{
  // the last operand
  return (OP_opnds(op) - 1);
}


BOOL Imm_Value_In_Range(OP *op, INT64 imm)
{
  TOP new_opc;
  switch (OP_code(op)) {
  case TOP_ld1:  new_opc = TOP_ld1_i; break;
  case TOP_ld2:  new_opc = TOP_ld2_i; break;
  case TOP_ld4:  new_opc = TOP_ld4_i; break;
  case TOP_ld8:  new_opc = TOP_ld8_i; break;
  case TOP_ldf8: new_opc = TOP_ldf8_i; break;
  case TOP_ldfd: new_opc = TOP_ldfd_i; break;
  case TOP_ldfe: new_opc = TOP_ldfe_i; break;
  case TOP_ldfs: new_opc = TOP_ldfs_i; break;
  case TOP_st1:  new_opc = TOP_st1_i; break;
  case TOP_st2:  new_opc = TOP_st2_i; break;
  case TOP_st4:  new_opc = TOP_st4_i; break;
  case TOP_st8:  new_opc = TOP_st8_i; break;
  case TOP_stf8: new_opc = TOP_stf8_i; break;
  case TOP_stfd: new_opc = TOP_stfd_i; break;
  case TOP_stfe: new_opc = TOP_stfe_i; break;
  case TOP_stfs: new_opc = TOP_stfs_i; break;

  case TOP_ld1_i:  
  case TOP_ld2_i:
  case TOP_ld4_i:
  case TOP_ld8_i:
  case TOP_ldf8_i:
  case TOP_ldfd_i:
  case TOP_ldfe_i:
  case TOP_ldfs_i:
  case TOP_st1_i:  
  case TOP_st2_i:  
  case TOP_st4_i:  
  case TOP_st8_i:  
  case TOP_stf8_i: 
  case TOP_stfd_i: 
  case TOP_stfe_i: 
  case TOP_stfs_i: 
    new_opc = OP_code(op);
    break;

  default:
    return FALSE;
  }

  INT opnd = OP_incr_opnd_num(new_opc);
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(new_opc);
  const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  ISA_LIT_CLASS lc = ISA_OPERAND_VALTYP_Literal_Class(vtype);
  BOOL retv = ISA_LC_Value_In_Class(imm, lc);
  return retv;
}

static INT32 Num_defs(BB *body, TN *def_tn)
{
  INT32 count = 0;
  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    for (INT i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      if (tn == def_tn)
	count++;
    }
  }
  return count;
}


BOOL Mem_stride_ge_access(OP *op, INT64 stride)
{
  stride = stride > 0 ? stride : -stride;
  switch (OP_code(op)) {
  case TOP_ld1:  
  case TOP_ld1_i:  
  case TOP_st1:  
  case TOP_st1_i:  
    return (stride >= 1);

  case TOP_ld2:
  case TOP_ld2_i:
  case TOP_st2:  
  case TOP_st2_i:  
    return (stride >= 2);

  case TOP_ld4:
  case TOP_ld4_i:
  case TOP_st4:  
  case TOP_st4_i:  
  case TOP_ldfs:
  case TOP_ldfs_i:
  case TOP_stfs: 
  case TOP_stfs_i: 
    return (stride >= 4);

  case TOP_ld8:
  case TOP_ld8_i:
  case TOP_st8:  
  case TOP_st8_i:  
  case TOP_ldf8:
  case TOP_ldf8_i:
  case TOP_stf8: 
  case TOP_stf8_i: 
  case TOP_ldfd:
  case TOP_ldfd_i:
  case TOP_stfd: 
  case TOP_stfd_i: 
    return (stride >= 8);

  case TOP_ldfe:
  case TOP_ldfe_i:
  case TOP_stfe: 
  case TOP_stfe_i: 
    return (stride >= 16);
  }
  return FALSE;
}


/* ====================================================================
 *
 *   Marks OP that has no cross-iteration aliasing.
 *
 *     need to run after unrolling and before postincr form 
 *
 * ====================================================================
 */
void Init_OP_no_ci_alias(CG_LOOP& cl, BOOL trace)
{
  BB *body = cl.Loop_header();
  CG_LOOP_DEF tn_def(body);

  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    if ((OP_load(op) || OP_store(op)) &&
	OP_convertible_to_base_update(op) != NO_BASE_UPDATE) {
      INT addr_opnd = OP_base_opnd_num(op);
      TN *addr_tn = OP_opnd(op, addr_opnd);
      OP *incr_op = tn_def.Get(addr_tn);
      if (incr_op) {
	switch (OP_code(incr_op)) {
	case TOP_adds:
	case TOP_addl:
	  if (OP_opnd(incr_op, 2) == addr_tn && 
	      TN_is_constant(OP_opnd(incr_op, 1)) &&
	      Num_defs(body, addr_tn) == 1 &&
	      Mem_stride_ge_access(op, TN_value(OP_opnd(incr_op, 1))) &&
	      OP_opnd(incr_op, OP_PREDICATE_OPND) == True_TN) {
	    Set_OP_no_ci_alias(op);
	    if (trace)
	      fprintf(TFile, "Set OP%d to no ci alias\n", 
		      OP_map_idx(op));
	  }
	  break;
	}
      }
    }
  }
}


// Identify and delete the increment operation.
// The increment operation will be combined into the op's
// base-update form.
//
static TN *
Identify_and_delete_incr(BB *bb, OP *memop, INT base_opnd_num, BASE_UPDATE up) 
{
  Is_True(OP_load(memop) || OP_store(memop), ("invalid op."));

  TN *base = OP_opnd(memop, base_opnd_num);

  bool allow_imm = (up & IMM_BASE_UPDATE) != 0;
  bool allow_reg = (up & REG_BASE_UPDATE) != 0;
  bool incr_kill = false;

  TN *pred_tn = OP_opnd(memop, OP_PREDICATE_OPND);

  for (OP *incr_op = OP_next(memop); incr_op != NULL; incr_op = OP_next(incr_op)) {

    if (OP_opnd(incr_op, OP_PREDICATE_OPND) == pred_tn) {
      // case 1: TNx := adds|addl predicateTN imm TNx where imm is at most 9 bits

      if (allow_imm &&
	  (OP_code(incr_op) == TOP_adds  || OP_code(incr_op) == TOP_addl) &&
	  OP_result(incr_op, 0) == base &&
	  OP_opnd(incr_op, 2) == base &&
	  OP_omega(incr_op, 2) == OP_omega(memop, base_opnd_num) &&
	  Imm_Value_In_Range(memop, TN_value(OP_opnd(incr_op, 1)))) {
	BB_Remove_Op(bb, incr_op);
	return OP_opnd(incr_op, 1);
      }

      // case 2: TNx := add predicateTN TNx TNy | add predicateTN TNy TNx

      if (allow_reg &&
	  OP_code(incr_op) == TOP_add && 
	  OP_result(incr_op, 0) == base) {
	if ((OP_opnd(incr_op, 1) == base) &&
	    (OP_omega(incr_op, 1) == OP_omega(memop, base_opnd_num)) &&
	    TN_is_global_reg(OP_opnd(incr_op, 2))) {
	  //OSP_383&377
	  //for the following op:
	  //op1,  GTN100 = ld  GTN1001
	  //op2,  GTN1002 = ld GTN1003
	  //op3   GTN1001 = add GTN1001 + GTN1002
          //GTN1002 is killed between op1 and op3, so the transform is invalid.
	  //we should find such kill and prevent such transformation.
	  for(OP *def_op = OP_next(memop);!incr_kill && def_op!=incr_op;def_op=OP_next(def_op))
	    {
	      if (OP_Defs_TN(def_op, OP_opnd(incr_op, 2))) incr_kill = true;
	    }
	  if(!incr_kill){
	    BB_Remove_Op(bb, incr_op);
	    return OP_opnd(incr_op, 2);
	  }
	  else 
	    return NULL;
	}
	else if ((OP_opnd(incr_op, 2) == base) &&
		 (OP_omega(incr_op, 2) == OP_omega(memop, base_opnd_num)) &&
		 TN_is_global_reg(OP_opnd(incr_op, 1))) {
	  for(OP *def_op = OP_next(memop);!incr_kill && def_op!=incr_op;def_op=OP_next(def_op))
	    {
	      if (OP_Defs_TN(def_op, OP_opnd(incr_op, 1))) incr_kill = true;
	    }
	  if(!incr_kill){
	    BB_Remove_Op(bb, incr_op);
	    return OP_opnd(incr_op, 1);
	  }
	  else
	    return NULL;
	}
      }
    }

    // cannot convert to postincr form if the base-address is either
    // defined or used between the memop and the incr op.

    if (OP_Defs_TN(incr_op, base)) break;
    if (OP_Refs_TN(incr_op, base)) break;
    if (OP_Defs_TN(incr_op, pred_tn)) break;
  }
  return NULL;
}


void Convert_OP_to_base_update_form(BB *body, OP *op, TN *incr, BASE_UPDATE up,
				    INT base_opnd_num, bool trace)
{
  TOP new_opc;
  if (TN_is_register(incr)) {
    Is_True(up & REG_BASE_UPDATE, ("reg base-update form not allowed"));
    switch (OP_code(op)) {
    case TOP_ld1:  new_opc = TOP_ld1_r; break;
    case TOP_ld2:  new_opc = TOP_ld2_r; break;
    case TOP_ld4:  new_opc = TOP_ld4_r; break;
    case TOP_ld8:  new_opc = TOP_ld8_r; break;
    case TOP_ldf8: new_opc = TOP_ldf8_r; break;
    case TOP_ldfd: new_opc = TOP_ldfd_r; break;
    case TOP_ldfe: new_opc = TOP_ldfe_r; break;
    case TOP_ldfs: new_opc = TOP_ldfs_r; break;
    default:
      Is_True(FALSE, ("unimplemented opcode."));
    }
  } else {
    Is_True(up & IMM_BASE_UPDATE, ("reg base-update form not allowed"));
    Is_True(Imm_Value_In_Range(op, TN_value(incr)), ("incr literal too big."));
    switch (OP_code(op)) {
    case TOP_ld1:  new_opc = TOP_ld1_i; break;
    case TOP_ld2:  new_opc = TOP_ld2_i; break;
    case TOP_ld4:  new_opc = TOP_ld4_i; break;
    case TOP_ld8:  new_opc = TOP_ld8_i; break;
    case TOP_ldf8: new_opc = TOP_ldf8_i; break;
    case TOP_ldfd: new_opc = TOP_ldfd_i; break;
    case TOP_ldfe: new_opc = TOP_ldfe_i; break;
    case TOP_ldfs: new_opc = TOP_ldfs_i; break;
    case TOP_st1:  new_opc = TOP_st1_i; break;
    case TOP_st2:  new_opc = TOP_st2_i; break;
    case TOP_st4:  new_opc = TOP_st4_i; break;
    case TOP_st8:  new_opc = TOP_st8_i; break;
    case TOP_stf8: new_opc = TOP_stf8_i; break;
    case TOP_stfd: new_opc = TOP_stfd_i; break;
    case TOP_stfe: new_opc = TOP_stfe_i; break;
    case TOP_stfs: new_opc = TOP_stfs_i; break;
    default:
      Is_True(FALSE, ("unimplemented opcode."));
    }
  }
  OP *new_op;
  if (OP_load(op)) {
    new_op = Mk_OP (new_opc, 
		    OP_result(op, 0),
		    OP_opnd(op, base_opnd_num),
		    OP_opnd(op, 0), 
		    OP_opnd(op, 1),
		    OP_opnd(op, 2),
		    OP_opnd(op, 3),
		    incr);
    Is_True(OP_opnds(new_op) == 5, ("postincr load has 5 opnds."));
  } else {
    if (OP_opnds(op) == 5) {
      new_op = Mk_OP (new_opc, 
		      OP_opnd(op, base_opnd_num),
		      OP_opnd(op, 0), 
		      OP_opnd(op, 1),
		      OP_opnd(op, 2),
		      OP_opnd(op, 3),
		      OP_opnd(op, 4),
		      incr);
      Is_True(OP_opnds(new_op) == 6, ("postincr store has 6 opnds."));
    } else {
      new_op = Mk_OP (new_opc, 
		      OP_opnd(op, base_opnd_num),
		      OP_opnd(op, 0), 
		      OP_opnd(op, 1),
		      OP_opnd(op, 2),
		      OP_opnd(op, 3),
		      incr);
      Is_True(OP_opnds(new_op) == 5, ("postincr store has 5 opnds."));
    }
  }
  
  // Transfer OP_omegas
  CG_LOOP_Init_Op(new_op);
  for (INT i = 0; i < OP_opnds(op); i++) {
    if (TN_is_register(OP_opnd(op,i))) {
      Is_True(OP_opnd(op, i) == OP_opnd(new_op, i),
	      ("Convert_Post_Incr:  can't transfer omega."));
      Set_OP_omega(new_op, i, OP_omega(op, i));
    }
  }
  
  // Transfer OP_flags
  OP_flags(new_op) = OP_flags(op);
  Set_OP_unrolling(new_op, OP_unrolling(op));
  Set_OP_orig_idx(new_op, OP_orig_idx(op));
  
  if (trace) {
    fprintf(TFile, "<postincr>: replacing ");
    Print_OP_No_SrcLine(op);
    fprintf(TFile, "with ");
    Print_OP_No_SrcLine(new_op);
  }
  BB_Insert_Op(body, op, new_op, TRUE/*before*/);
  BB_Remove_Op(body, op);
  Copy_WN_For_Memory_OP (new_op, op);
  op = new_op;
}


/* ====================================================================
 *
 *  Convert_Post_Incr
 *   try to convert load/store into their base update form
 *   it must also update the OP_omega information
 *
 * ====================================================================
 */
void Gen_Post_Incr_Memop(CG_LOOP& cl, bool trace)
{
  BB *body = cl.Loop_header();
  OP_VECTOR op_vec(body);
  TN_DU_MAP TN_DU_map(op_vec, trace);

  OP *next_op;
  for (OP *op = BB_first_op(body); op != NULL; op = next_op) {

    // note that Convert_OP_to_base_update_form can delete the current op
    next_op = OP_next(op); 

    BASE_UPDATE up = OP_convertible_to_base_update(op);
    if (up != NO_BASE_UPDATE) {
      // NOTE: Try_to_delete_incr already removed the increment OPs.
      INT base_opnd_num = OP_base_opnd_num(op);
      TN *base = OP_opnd(op, base_opnd_num);

      // 1. Live-range the base address TN must not overlap with itself.
      // 2. The base TN is not shared with a dependent memop.
      //
      bool can_use_non_rotating_reg = TN_DU_map[base].TN_can_use_non_rotating_reg(base, op_vec);
      bool owns_the_base = can_use_non_rotating_reg &&
	OP_owns_the_base_TN(op, base, TN_DU_map[base], op_vec);
      bool base_needed_in_epilog = tn_is_needed_in_epilog(base);
      TN *incr = NULL;
      incr = (owns_the_base  && !base_needed_in_epilog)
                        ? Identify_and_delete_incr(body, op, base_opnd_num, up) : NULL;
      if (incr != NULL)
	Convert_OP_to_base_update_form(body, op, incr, up, base_opnd_num, trace);

      if (trace) {
	if (!can_use_non_rotating_reg) 
	  fprintf(TFile, "<postincr> TN%d cannot use non-rotating reg\n", TN_number(base));
	else if (!owns_the_base)
	  fprintf(TFile, "<postincr> OP does not own the base TN%d\n", TN_number(base));
	else if (base_needed_in_epilog)
	  fprintf(TFile, "<postincr> TN%d[1] needed in epilog\n", TN_number(base));
	else if (incr == NULL)
	  fprintf(TFile, "<postincr> cannot delete incr TN%d\n", TN_number(base));
      }
    }
  }
}


/* ====================================================================
 *
 *  Expand_Simulated_Ops
 *
 * ====================================================================
 */
static BOOL Expand_Simulated_Ops(CG_LOOP& cl, bool trace)
{
  BB *body = cl.Loop_header();
  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    TOP top = OP_code(op);
    if (OP_simulated(op) && top != TOP_asm) {
      if (trace) 
	fprintf(TFile, "<swp> expand simulated op %s\n", TOP_Name(top));
      switch (top) {
      case TOP_mov_t_ar_r: 
	{
	  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(top);
	  const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Result(oinfo, 0);
	  const ISA_REGISTER_SUBCLASS sc = ISA_OPERAND_VALTYP_Register_Subclass(vtype);
	  if (sc == ISA_REGISTER_SUBCLASS_ar_i)
	    OP_Change_Opcode(op, TOP_mov_t_ar_r_i);
	  else
	    OP_Change_Opcode(op, TOP_mov_t_ar_r_m);
	  break;
	}
      default:
	DevWarn("SWP: support of simulated OP %s not implemented.", TOP_Name(top));
	return FALSE;
      }
    }
  }
  return TRUE;
}


/* ====================================================================
 *
 *  Prepare_Loop_For_SWP
 *    See interface in cg_swp_target.h
 *    This function is called before building CG_LOOP_INFO
 *
 * ====================================================================
 */
BOOL Prepare_Loop_For_SWP_1(CG_LOOP& cl, bool trace)
{
  if (!Expand_Simulated_Ops(cl, trace))
    return FALSE;

  if (!Remove_Non_Definite_Dependence(cl, false/*no CG_LOOP_Init_OP*/, trace))
    return FALSE;

  Remove_Invariant_Predicates(cl, trace);

  Unc_Promotion(cl, trace);

  Hoist_MOVL(cl, trace);

  Init_OP_no_ci_alias(cl, trace);

  return TRUE;
}


/* ====================================================================
 *
 *  Prepare_Loop_For_SWP
 *    See interface in cg_swp_target.h
 *    This function is called after building CG_LOOP_INFO
 *
 * ====================================================================
 */
BOOL Prepare_Loop_For_SWP_2(CG_LOOP& cl, bool trace)
{
  if (SWP_Options.Enable_Post_Incr)
    Gen_Post_Incr_Memop(cl, trace);

  if (!Remove_Non_Definite_Dependence(cl, true/*need CG_LOOP_Init_OP*/, trace))
    return FALSE;

  return TRUE;
}



TOP Get_Predicated_Form(TOP top)
{
  switch (top) {
  case TOP_fcmp_eq_unc:      return TOP_fcmp_eq;
  case TOP_fcmp_ge_unc:      return TOP_fcmp_ge;
  case TOP_fcmp_gt_unc:      return TOP_fcmp_gt;
  case TOP_fcmp_le_unc:      return TOP_fcmp_le;
  case TOP_fcmp_lt_unc:      return TOP_fcmp_lt;
  case TOP_fcmp_neq_unc:     return TOP_fcmp_neq;
  case TOP_fcmp_nge_unc:     return TOP_fcmp_nge;
  case TOP_fcmp_ngt_unc:     return TOP_fcmp_ngt;
  case TOP_fcmp_nle_unc:     return TOP_fcmp_nle;
  case TOP_fcmp_nlt_unc:     return TOP_fcmp_nlt;
  case TOP_fcmp_ord_unc:     return TOP_fcmp_ord;
  case TOP_fcmp_unord_unc:   return TOP_fcmp_unord;
  case TOP_cmp_eq_unc:       return TOP_cmp_eq;
  case TOP_cmp_lt_unc:       return TOP_cmp_lt;
  case TOP_cmp_ltu_unc:      return TOP_cmp_ltu;
  case TOP_cmp_i_eq_unc:     return TOP_cmp_i_eq;
  case TOP_cmp_i_lt_unc:     return TOP_cmp_i_lt;
  case TOP_cmp_i_ltu_unc:    return TOP_cmp_i_ltu;
  case TOP_cmp4_eq_unc:      return TOP_cmp4_eq;
  case TOP_cmp4_lt_unc:      return TOP_cmp4_lt;
  case TOP_cmp4_ltu_unc:     return TOP_cmp4_ltu;
  case TOP_cmp4_i_eq_unc:    return TOP_cmp4_i_eq;
  case TOP_cmp4_i_lt_unc:    return TOP_cmp4_i_lt;
  case TOP_cmp4_i_ltu_unc:   return TOP_cmp4_i_ltu;
  case TOP_cmp_ne_unc:       return TOP_cmp_ne;
  case TOP_cmp_le_unc:       return TOP_cmp_le;
  case TOP_cmp_gt_unc:       return TOP_cmp_gt;
  case TOP_cmp_ge_unc:       return TOP_cmp_ge;
  case TOP_cmp_leu_unc:      return TOP_cmp_leu;
  case TOP_cmp_gtu_unc:      return TOP_cmp_gtu;
  case TOP_cmp_geu_unc:      return TOP_cmp_geu;
  case TOP_cmp_i_ne_unc:     return TOP_cmp_i_ne;
  case TOP_cmp_i_le_unc:     return TOP_cmp_i_le;
  case TOP_cmp_i_gt_unc:     return TOP_cmp_i_gt;
  case TOP_cmp_i_ge_unc:     return TOP_cmp_i_ge;
  case TOP_cmp_i_leu_unc:    return TOP_cmp_i_leu;
  case TOP_cmp_i_gtu_unc:    return TOP_cmp_i_gtu; 
  case TOP_cmp_i_geu_unc:    return TOP_cmp_i_geu;
  case TOP_cmp4_ne_unc:      return TOP_cmp4_ne; 
  case TOP_cmp4_le_unc:      return TOP_cmp4_le;
  case TOP_cmp4_gt_unc:      return TOP_cmp4_gt;
  case TOP_cmp4_ge_unc:      return TOP_cmp4_ge;
  case TOP_cmp4_leu_unc:     return TOP_cmp4_leu;
  case TOP_cmp4_gtu_unc:     return TOP_cmp4_gtu;
  case TOP_cmp4_geu_unc:     return TOP_cmp4_geu;
  case TOP_cmp4_i_ne_unc:    return TOP_cmp4_i_ne;
  case TOP_cmp4_i_le_unc:    return TOP_cmp4_i_le;
  case TOP_cmp4_i_gt_unc:    return TOP_cmp4_i_gt;
  case TOP_cmp4_i_ge_unc:    return TOP_cmp4_i_ge;
  case TOP_cmp4_i_leu_unc:   return TOP_cmp4_i_leu;
  case TOP_cmp4_i_gtu_unc:   return TOP_cmp4_i_gtu;
  case TOP_cmp4_i_geu_unc:   return TOP_cmp4_i_geu;
  case TOP_tbit_nz_unc:      return TOP_tbit_nz;
  case TOP_tbit_z_unc:       return TOP_tbit_z;
  case TOP_tnat_nz_unc:      return TOP_tnat_nz;
  case TOP_tnat_z_unc:       return TOP_tnat_z;
  case TOP_fcmp_eq:
  case TOP_fcmp_ge:
  case TOP_fcmp_gt:
  case TOP_fcmp_le:
  case TOP_fcmp_lt:
  case TOP_fcmp_neq:
  case TOP_fcmp_nge:
  case TOP_fcmp_ngt:
  case TOP_fcmp_nle:
  case TOP_fcmp_nlt:
  case TOP_fcmp_ord:
  case TOP_fcmp_unord:
  case TOP_cmp_eq:
  case TOP_cmp_lt:
  case TOP_cmp_ltu:
  case TOP_cmp_i_eq:
  case TOP_cmp_i_lt:
  case TOP_cmp_i_ltu:
  case TOP_cmp4_eq:
  case TOP_cmp4_lt:
  case TOP_cmp4_ltu:
  case TOP_cmp4_i_eq:
  case TOP_cmp4_i_lt:
  case TOP_cmp4_i_ltu:
  case TOP_cmp_ne:
  case TOP_cmp_le:
  case TOP_cmp_gt:
  case TOP_cmp_ge:
  case TOP_cmp_leu:
  case TOP_cmp_gtu:
  case TOP_cmp_geu:
  case TOP_cmp_i_ne:
  case TOP_cmp_i_le:
  case TOP_cmp_i_gt:
  case TOP_cmp_i_ge:
  case TOP_cmp_i_leu:
  case TOP_cmp_i_gtu:
  case TOP_cmp_i_geu:
  case TOP_cmp4_ne:
  case TOP_cmp4_le:
  case TOP_cmp4_gt:
  case TOP_cmp4_ge:
  case TOP_cmp4_leu:
  case TOP_cmp4_gtu:
  case TOP_cmp4_geu:
  case TOP_cmp4_i_ne:
  case TOP_cmp4_i_le:
  case TOP_cmp4_i_gt:
  case TOP_cmp4_i_ge:
  case TOP_cmp4_i_leu:
  case TOP_cmp4_i_gtu:
  case TOP_cmp4_i_geu:
  case TOP_tbit_nz:
  case TOP_tbit_z:
  case TOP_tnat_nz:
  case TOP_tnat_z:
    return top;
  }
  return TOP_UNDEFINED;
}


/* ====================================================================
 *
 *  Convert a while-loop to the fully predicated form
 *   - predicate all OPs in the loop body by the
 *     control predicate of the loopback branch.
 *
 * ====================================================================
 */
void Convert_While_Loop_to_Fully_Predicated_Form(CG_LOOP& cl)
{
  BB *body = cl.Loop_header();
  OP *br_op = BB_branch_op(body);
  TN *predicate_tn = OP_opnd(br_op, OP_PREDICATE_OPND);
  if (TN_is_dedicated(predicate_tn))
    return;

  // The predicate has some initial value entering the loop.
  // 
  if (CG_LOOP_Backpatch_First(cl.Prolog_end(), predicate_tn) != NULL)
    return;

  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    if (OP_has_predicate(op) &&
	TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND))) {

      if (OP_results(op) == 2 &&
	  (OP_result(op, 0) == predicate_tn ||
	   OP_result(op, 1) == predicate_tn)) {
	// Convert control predicate update to regular form
	TOP pred_top = Get_Predicated_Form(OP_code(op));
	Is_True(pred_top != TOP_UNDEFINED, 
		("Convert_While_Loop_to_Fully_Predicated_Form: unable to generate unc form of the OP."));
	if (pred_top != OP_code(op))
	  OP_Change_Opcode(op, pred_top);
      } else {
	// Convert branch into unc form if it does not compute
	// control predicate.
	TOP unc_top = CGTARG_Get_unc_Variant(OP_code(op));
	if (unc_top != TOP_UNDEFINED) {
	  OP_Change_Opcode(op, unc_top);
	  CGTARG_Init_OP_cond_def_kind(op);
	}
      }

      Set_OP_opnd(op, OP_PREDICATE_OPND, predicate_tn);
      Set_OP_omega(op, OP_PREDICATE_OPND, 1);
    }
  }
  CG_LOOP_Backpatch_Add(cl.Prolog_end(), True_TN, predicate_tn, 1);
}


// Generate implicit Prefetch
//
void Gen_Implicit_Prefetches(CG_LOOP& cl, bool trace)
{
  if (!SWP_Options.Implicit_Prefetch)
    return;

  BB *body = cl.Loop_header();
  OP *next_op;

  std::vector<OP*> prune_pref;

  // Now regenerate the prefetch by updating the prefetch hints
  for (OP *op = BB_first_op(body); op != NULL; op = next_op) {
    next_op = OP_next(op);
    if (OP_memory(op)) {
      WN *wn = Get_WN_From_Memory_OP( op);
      if (wn == NULL)
	continue;
      PF_POINTER *pf_ptr = (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,wn);
      if (pf_ptr == NULL)
	continue;

      INT stride = 0;
      INT offset = 0;
      WN *pref = PF_PTR_wn_pref_2L(pf_ptr);
      if (pref && Prefetch_Kind_Enabled(pref)) {
	stride = WN_pf_stride_2L(pref);
	offset = PF_PTR_distance_2L(pf_ptr);
      }
      else {
	pref = PF_PTR_wn_pref_1L(pf_ptr);
	if (pref && Prefetch_Kind_Enabled(pref)) {
	  stride = WN_pf_stride_1L(pref);
	  offset = PF_PTR_distance_1L(pf_ptr);
	}
      }
      if (stride != 0 && (OP_unrolling(op) % stride) == 0) {
	if (trace)
	  fprintf(TFile, "<swp_pref> offset=%d stride=%d\n", offset, stride);

	switch (OP_base_update_kind(op)) {
	case IMM_BASE_UPDATE:
	  {
	    INT base_opnd_num = OP_base_opnd_num(op);
	    INT imm_opnd_num = OP_imm_opnd_num(op);
	    INT adjusted_offset = TN_value(OP_opnd(op, imm_opnd_num)) - offset;
	    if (Imm_Value_In_Range(op, offset)) {
	      OPS ops = OPS_EMPTY;
	      Build_OP(TOP_adds,
		       OP_opnd(op, base_opnd_num),
		       True_TN,
		       Gen_Literal_TN(adjusted_offset, 4),
		       OP_opnd(op, base_opnd_num),
		       &ops);
	      CG_LOOP_Init_OPS(&ops);
	      BB_Insert_Ops_Before(body, next_op, &ops);
	      TN *imm = Gen_Literal_TN(offset, 4);
	      Set_OP_opnd(op, imm_opnd_num, imm);
	      SET_PF_PRUNED(pf_ptr);
              // Also prune PF_POINTER for the corresponding PREFETCH node
              // because these could be different after WOPT
              if (PF_PTR_wn_pref_2L(pf_ptr)) {
                PF_POINTER* pf2_pf_ptr = (PF_POINTER*) 
                  WN_MAP_Get(WN_MAP_PREFETCH, PF_PTR_wn_pref_2L(pf_ptr));
                if (pf2_pf_ptr) {
                  SET_PF_PRUNED(pf2_pf_ptr);
                }
              }
	    }
	  }
	}
      }
    }
  }

  // Remove the prefetch whose PF_POINTER has been marked!
  //
  {
    for (OP *op = BB_first_op(body); op != NULL; op = next_op) {
      next_op = OP_next(op);
      if (OP_prefetch(op)) {
	WN *wn = Get_WN_From_Memory_OP( op);
	PF_POINTER *pf_ptr = (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,wn);
	if (pf_ptr && PF_PRUNED(pf_ptr)) {
	  if (trace) 
	    fprintf(TFile, "<swp_pref> removing prefetch OP%d\n", OP_map_idx(op));
	  BB_Remove_Op(body, op);
	}
      }
    }
  }
}


