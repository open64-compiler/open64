/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*

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

*/


/* CGEXP routines for expanding branches */

#include "defs.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "tn.h"
#include "cg_flags.h"
#include "op.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "whirl2ops.h"
#include "config_opt.h"      // For Force_IEEE_Comparisons

void
Initialize_Branch_Variants(void)
{
	// nothing to do
}

// Check that compare is of proper form,
// and return TOP to use for the compare.
// May modify the variant and src tns.
TOP
Pick_Compare_TOP (VARIANT *variant, TN **src1, TN **src2, OPS *ops)
{
  TOP cmp = TOP_UNDEFINED;

  if (*src1 != NULL && TN_has_value(*src1)) {
    // swap operands and change variant
    TN *tmp = *src1;
    *src1 = *src2;
    *src2 = tmp;
    *variant = Invert_BR_Variant(*variant);
  }
  
  if (*src2 != NULL && TN_is_zero(*src2)) {
    switch (*variant) {
    case V_BR_U8LT:	
    case V_BR_U4LT:	
      *variant = V_BR_NEVER; break;
    case V_BR_U8GE:
    case V_BR_U4GE:
      *variant = V_BR_ALWAYS; break;
      // because src2 is zero, and comparison is unsigned
    case V_BR_U4LE: 
      *variant = V_BR_U4EQ; break;
      // because src2 is zero, and comparison is unsigned
    case V_BR_U8LE:
      *variant = V_BR_U8EQ; break;
    }
  }

  // If branch variant condition is V_BR_ALWAYS the caller expects 
  // a TOP_UNDEFINED, so it can create an unconditional jump.
  // Also, this guards against dereferencing of *src2 when NULL.
  if (*src2 == NULL)
    return TOP_UNDEFINED;

  // pick tops
  switch (*variant) {
  case V_BR_I4EQ: cmp = TOP_setp_eq_s32; break;
  case V_BR_I4NE: cmp = TOP_setp_ne_s32; break;
  case V_BR_I4GT: cmp = TOP_setp_gt_s32; break;
  case V_BR_I4GE: cmp = TOP_setp_ge_s32; break;
  case V_BR_I4LT: cmp = TOP_setp_lt_s32; break;
  case V_BR_I4LE: cmp = TOP_setp_le_s32; break;
  case V_BR_U4EQ: cmp = TOP_setp_eq_u32; break;
  case V_BR_U4NE: cmp = TOP_setp_ne_u32; break;
  case V_BR_U4GT: cmp = TOP_setp_gt_u32; break;
  case V_BR_U4GE: cmp = TOP_setp_ge_u32; break;
  case V_BR_U4LT: cmp = TOP_setp_lt_u32; break;
  case V_BR_U4LE: cmp = TOP_setp_le_u32; break;
  case V_BR_I8EQ: cmp = TOP_setp_eq_s64; break;
  case V_BR_I8NE: cmp = TOP_setp_ne_s64; break;
  case V_BR_I8GT: cmp = TOP_setp_gt_s64; break;
  case V_BR_I8GE: cmp = TOP_setp_ge_s64; break;
  case V_BR_I8LT: cmp = TOP_setp_lt_s64; break;
  case V_BR_I8LE: cmp = TOP_setp_le_s64; break;
  case V_BR_U8EQ: cmp = TOP_setp_eq_u64; break;
  case V_BR_U8NE: cmp = TOP_setp_ne_u64; break;
  case V_BR_U8GT: cmp = TOP_setp_gt_u64; break;
  case V_BR_U8GE: cmp = TOP_setp_ge_u64; break;
  case V_BR_U8LT: cmp = TOP_setp_lt_u64; break;
  case V_BR_U8LE: cmp = TOP_setp_le_u64; break;
  case V_BR_FEQ: cmp = TOP_setp_eq_f32; break;
  // floats use neu not ne so nan's work properly
  case V_BR_FNE: cmp = TOP_setp_neu_f32; break;
  case V_BR_FGT: cmp = TOP_setp_gt_f32; break;
  case V_BR_FGE: cmp = TOP_setp_ge_f32; break;
  case V_BR_FLT: cmp = TOP_setp_lt_f32; break;
  case V_BR_FLE: cmp = TOP_setp_le_f32; break;
  case V_BR_DEQ: cmp = TOP_setp_eq_f64; break;
  case V_BR_DNE: cmp = TOP_setp_neu_f64; break;
  case V_BR_DGT: cmp = TOP_setp_gt_f64; break;
  case V_BR_DGE: cmp = TOP_setp_ge_f64; break;
  case V_BR_DLT: cmp = TOP_setp_lt_f64; break;
  case V_BR_DLE: cmp = TOP_setp_le_f64; break;
  }
  return cmp;
}


/* Handle unordered compare&branch */
static void Expand_Ordered_Branch( TOP cmp_opcode, TN* src1, TN* src2,
				   TOP jmp_opcode, TN* targ, OPS* ops )
{
    FmtAssert( false, ("NYI") );

  return;
}

void Expand_Branch ( TN *targ, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  BOOL false_br = V_false_br(variant);
  VARIANT cond = V_br_condition(variant);
  TN *pred;

  /* Trace if required: */
  if ( Trace_Exp2 ) {
    fprintf ( TFile, "<cgexp> Translating %s branch:\n",
        (false_br ? "false" : "true") );
  }

  FmtAssert( cond <= V_BR_LAST, ("unexpected variant in Expand_Branch"));
  FmtAssert( cond != V_BR_NONE, ("BR_NONE variant in Expand_Branch"));

  const TOP cmp = Pick_Compare_TOP (&cond, &src1, &src2, ops);
  if ( Trace_Exp2 && cond != variant) {
    fprintf ( TFile, "<cgexp> transformed branch cond = %llx\n", cond);
  }

  switch (cond) {
  case V_BR_ALWAYS:
  case V_BR_NEVER:
    Is_True(cmp == TOP_UNDEFINED, 
	    ("unexpected compare op for %s", BR_Variant_Name(cond)));
    if ((cond == V_BR_ALWAYS) ^ false_br) {
      // Unconditional branch for ALWAYS/!false_br and NEVER/false_br
      Build_OP (TOP_bra_uni, targ, ops);
    }
    break;
  case V_BR_PEQ:
  case V_BR_PNE:
    pred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    Build_OP (TOP_xor_pred, pred, src1, src2, ops);
    // xor returns true if predicates not equal
    if ((cond == V_BR_PEQ && false_br) || (cond == V_BR_PNE && !false_br)) 
      	Build_OP (TOP_bra_p, pred, targ, ops);
    else
      	Build_OP (TOP_bra_np, pred, targ, ops);
    break;
  case V_BR_P_TRUE:
    if (false_br)
      	Build_OP (TOP_bra_np, src1, targ, ops);
    else
      	Build_OP (TOP_bra_p, src1, targ, ops);
    break;
  default:
    {
      Is_True(cmp != TOP_UNDEFINED, ("no topcode for compare"));

      if( TN_has_value(src1) ){
	src1 = Expand_Mtype_Immediate_Into_Register (src1, Mtype_Of_TN(src2), ops);
      }
      if( TN_has_value(src2) ){
        src2 = Expand_Mtype_Immediate_Into_Register (src2, Mtype_Of_TN(src1), ops);
      }
      // predicate version:
      // Generate sequence of set.cmp p; @p bra
      pred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      Build_OP (cmp, pred, src1, src2, ops);
      if (false_br)
      	Build_OP (TOP_bra_np, pred, targ, ops);
      else
      	Build_OP (TOP_bra_p, pred, targ, ops);
	
    }

    break;
  }
}

void Exp_Indirect_Branch (TN *targ_reg, OPS *ops)
{
  FmtAssert(FALSE, ("NYI"));
}

void Exp_Local_Jump(BB *bb, INT64 offset, OPS *ops)
{
  FmtAssert(FALSE, ("NYI: Exp_Local_Jump"));
}

void Exp_Return (TN *return_address, OPS *ops)
{
  if (ST_in_global_mem(Get_Current_PU_ST())) {
    Build_OP (TOP_exit, ops); // exit main entry
  } else {
    Build_OP (TOP_ret, ops);  // return from call
  }
}

void Exp_Call( OPERATOR opr, TN *return_address, TN *target, OPS *ops )
{
  Build_OP (TOP_call, target, ops);
}
