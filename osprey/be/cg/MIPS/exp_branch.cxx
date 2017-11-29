/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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


/* CGEXP routines for expanding branches */

#include "defs.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "config_targ_opt.h"
#include "tn.h"
#include "cg_flags.h"
#include "op.h"
#include "cgexp.h"
#include "cgexp_internals.h"

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
  TOP cmp_i = TOP_UNDEFINED;
  INT64 val;

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

  // pick tops
  switch (*variant) {
  case V_BR_I8EQ:
  case V_BR_U8EQ:
  case V_BR_I8NE:
  case V_BR_U8NE:
  case V_BR_I4EQ:
  case V_BR_U4EQ:
  case V_BR_I4NE:
  case V_BR_U4NE:	
    break; 

  case V_BR_I8LT:	
  case V_BR_I4LT:
    cmp_i = TOP_slti; cmp = TOP_slt; 
    break;
  case V_BR_U8LT:
  case V_BR_U4LT:	
    cmp_i = TOP_sltiu; cmp = TOP_sltu; 
    break;

  case V_BR_I8GE:	
  case V_BR_I4GE:
    cmp_i = TOP_slti; cmp = TOP_slt; 
    break;
  case V_BR_U8GE:
  case V_BR_U4GE:	
    cmp_i = TOP_sltiu; cmp = TOP_sltu; 
    break;

  case V_BR_I8GT:	
  case V_BR_I4GT:
    if (TN_has_value(*src2)) { // add 1 to value and handle as GE
      val = TN_value(*src2);
      *src2 = Gen_Literal_TN(val+1, TN_size(*src2));
      cmp_i = TOP_slti; cmp = TOP_slt; 
      *variant = (*variant == V_BR_I8GT) ? V_BR_I8GE : V_BR_I4GE;
    }
    else { // swap operands and change variant
      TN *tmp = *src1;
      *src1 = *src2;
      *src2 = tmp;
      *variant = Invert_BR_Variant(*variant);
      cmp_i = TOP_slti; cmp = TOP_slt; 
    }
    break;
  case V_BR_U8GT:
  case V_BR_U4GT:
    if (TN_has_value(*src2)) { // add 1 to value and handle as GE
      val = TN_value(*src2);
      if (val == -1) {
	*variant = V_BR_NEVER;
	break;
      }
      *src2 = Gen_Literal_TN(val+1, TN_size(*src2));
      cmp_i = TOP_sltiu; cmp = TOP_sltu; 
      *variant = (*variant == V_BR_U8GT) ? V_BR_U8GE : V_BR_U4GE;
    }
    else { // swap operands and change variant
      TN *tmp = *src1;
      *src1 = *src2;
      *src2 = tmp;
      *variant = Invert_BR_Variant(*variant);
      cmp_i = TOP_sltiu; cmp = TOP_sltu; 
    }
    break;

  case V_BR_I8LE:
  case V_BR_I4LE:
    if (TN_has_value(*src2)) { // subtract 1 from value and handle as LT
      val = TN_value(*src2);
      *src2 = Gen_Literal_TN(val+1, TN_size(*src2));
      cmp_i = TOP_slti; cmp = TOP_slt; 
      *variant = (*variant == V_BR_I8LE) ? V_BR_I8LT : V_BR_I4LT;
    }
    else { // swap operands and change variant
      TN *tmp = *src1;
      *src1 = *src2;
      *src2 = tmp;
      *variant = Invert_BR_Variant(*variant);
      cmp_i = TOP_slti; cmp = TOP_slt; 
    }
    break;
  case V_BR_U8LE:
  case V_BR_U4LE:
    if (TN_has_value(*src2)) { // subtract 1 from value and handle as LT
      val = TN_value(*src2);
      if (val == -1) {
	*variant = V_BR_ALWAYS;
	break;
      }
      *src2 = Gen_Literal_TN(val+1, TN_size(*src2));
      cmp_i = TOP_sltiu; cmp = TOP_sltu; 
      *variant = (*variant == V_BR_U8LE) ? V_BR_U8LT : V_BR_U4LT;
    }
    else { // swap operands and change variant
      TN *tmp = *src1;
      *src1 = *src2;
      *src2 = tmp;
      *variant = Invert_BR_Variant(*variant);
      cmp_i = TOP_sltiu; cmp = TOP_sltu; 
    }
    break;

  case V_BR_FEQ:	cmp = TOP_c_eq_s; break;
  case V_BR_DEQ:	cmp = TOP_c_eq_d; break;
#if defined(TARG_SL)
  case V_BR_FLT:	cmp = TOP_c_olt_s; break;
#else
  case V_BR_FLT:	cmp = TOP_c_lt_s; break;
#endif
  case V_BR_DLT:	cmp = TOP_c_lt_d; break;
#if defined(TARG_SL)
  case V_BR_FLE:	cmp = TOP_c_ole_s; break;
#else
  case V_BR_FLE:	cmp = TOP_c_le_s; break;
#endif
  case V_BR_DLE:	cmp = TOP_c_le_d; break;
  case V_BR_FNE:	cmp = TOP_c_neq_s; break;
  case V_BR_DNE:	cmp = TOP_c_neq_d; break;
  case V_BR_FGT: 
    {
      TN *tmp = *src1;
      *src1 = *src2;
      *src2 = tmp;
      *variant = Invert_BR_Variant(*variant);
#if defined(TARG_SL)
      cmp = TOP_c_olt_s; 
#else
      cmp = TOP_c_lt_s; 
#endif
      break;
    }
  case V_BR_DGT:
    {
      TN *tmp = *src1;
      *src1 = *src2;
      *src2 = tmp;
      *variant = Invert_BR_Variant(*variant);
      cmp = TOP_c_lt_d; 
      break;
    }
  case V_BR_FGE:	
    {
      TN *tmp = *src1;
      *src1 = *src2;
      *src2 = tmp;
      *variant = Invert_BR_Variant(*variant);
#if defined(TARG_SL)
      cmp = TOP_c_ole_s; break;
#else
      cmp = TOP_c_le_s; break;
#endif
    }
  case V_BR_DGE:	
    {
      TN *tmp = *src1;
      *src1 = *src2;
      *src2 = tmp;
      *variant = Invert_BR_Variant(*variant);
      cmp = TOP_c_le_d; break;
    }
  default:	;
  }

  // If branch variant condition is V_BR_ALWAYS the caller expects 
  // a TOP_UNDEFINED, so it can create an unconditional jump.
  // Also, this guards against dereferencing of *src2 when NULL.
  if (*src2 == NULL)
    return TOP_UNDEFINED;

  // if src2 is immed and fits, use immed form of top
  if (TN_has_value(*src2)) {
    if (cmp_i != TOP_UNDEFINED &&
	ISA_LC_Value_In_Class(TN_value(*src2), LC_simm16))
      cmp = cmp_i;
    else
      *src2 = Expand_Immediate_Into_Register(*src2, TN_size(*src1) == 8, ops);
  }

  return cmp;
}

void
Expand_Branch ( TN *targ, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  TOP cmp;
  BOOL false_br = V_false_br(variant);
  VARIANT cond = V_br_condition(variant);

  /* Trace if required: */
  if ( Trace_Exp2 ) {
    fprintf ( TFile, "<cgexp> Translating %s branch:\n",
        (false_br ? "false" : "true") );
  }

  FmtAssert( cond <= V_BR_LAST, ("unexpected variant in Expand_Branch"));
  FmtAssert( cond != V_BR_NONE, ("BR_NONE variant in Expand_Branch"));

  cmp = Pick_Compare_TOP (&cond, &src1, &src2, ops);

  if ( Trace_Exp2 && cond != variant) {
    fprintf ( TFile, "<cgexp> transformed branch cond = %lld\n", cond);
  }

  switch (cond) {
  case V_BR_ALWAYS:
  case V_BR_NEVER:
    Is_True(cmp == TOP_UNDEFINED, 
	    ("unexpected compare op for %s", BR_Variant_Name(cond)));
    if ((cond == V_BR_ALWAYS) ^ false_br) {
      // Unconditional branch for ALWAYS/!false_br and NEVER/false_br
      Build_OP (TOP_j, targ, ops);
    }
    break;
  case V_BR_PEQ:
  case V_BR_PNE:
  case V_BR_P_TRUE:
    FmtAssert(FALSE, ("unimplemented branch variant in Expand_Branch"));
    break;
  default:
    {
      TOP top;
      // integer or floating point conditional branch
      TN *tmp;
      if (cmp != TOP_UNDEFINED) {
	if (TOP_is_flop(cmp)) { 
	  TN *fcc_tmp = Gen_Register_TN(ISA_REGISTER_CLASS_fcc, 1);
	  // c_neq_s and c_neq_d are not recognized by the Octane assembler;
	  if (cmp == TOP_c_neq_s || cmp == TOP_c_neq_d) {
	    if(Target_ISA<TARGET_ISA_M4){
   	      Build_OP ((cmp == TOP_c_neq_d)?TOP_c_eq_d:TOP_c_eq_s, 
		      src1, src2, ops);
	      Build_OP (false_br?TOP_bc1t:TOP_bc1f, targ, ops);
	    }
	    else{
   	      Build_OP ((cmp == TOP_c_neq_d)?TOP_c_eq_d:TOP_c_eq_s, 
		      fcc_tmp, src1, src2, ops);
	      Build_OP (false_br?TOP_bc1t:TOP_bc1f, fcc_tmp, targ, ops);
	    }
	  } else {
	    Build_OP (cmp, fcc_tmp, src1, src2, ops);
	    Build_OP (false_br?TOP_bc1f:TOP_bc1t, fcc_tmp, targ, ops);
	  }
        }
        else {
	  if (TN_is_constant(src2) &&
	      TN_has_value(src2) &&
	      (TN_value(src2) == 0)) {
	    if (cond == V_BR_I8LT || cond == V_BR_U8LT ||
		cond == V_BR_I4LT || cond == V_BR_U4LT)
	      top = TOP_bltz;
	    else top = TOP_bgez;
	    Build_OP (top, src1, targ, ops);
	  } else {
	    tmp = Build_TN_Of_Mtype (MTYPE_I4);
	    Build_OP (cmp, tmp, src1, src2, ops);
	    if (cond == V_BR_I8LT || cond == V_BR_U8LT ||
		cond == V_BR_I4LT || cond == V_BR_U4LT)
	      top = TOP_bne;
	    else top = TOP_beq;
	    Build_OP (top, tmp, Zero_TN, targ, ops);
	  }
	}
      }
      else {
	switch (cond) {
	case V_BR_I8GE:	top = TOP_bgez; break;
	case V_BR_I8GT:	top = TOP_bgtz; break;
	case V_BR_I8LE:	top = TOP_blez; break;
	case V_BR_I8LT:	top = TOP_bltz; break;
	case V_BR_I8EQ:	top = TOP_beq; break;
	case V_BR_U8EQ:	top = TOP_beq; break;
	case V_BR_I8NE:	top = TOP_bne; break;
	case V_BR_U8NE:	top = TOP_bne; break;
	case V_BR_I4GE:	top = TOP_bgez; break;
	case V_BR_I4GT:	top = TOP_bgtz; break;
	case V_BR_I4LE:	top = TOP_blez; break;
	case V_BR_I4LT:	top = TOP_bltz; break;
	case V_BR_I4EQ:	top = TOP_beq; break;
	case V_BR_U4EQ:	top = TOP_beq; break;
	case V_BR_I4NE:	top = TOP_bne; break;
	case V_BR_U4NE:	top = TOP_bne; break;
	default:
	  FmtAssert(FALSE, ("unimplemented branch variant in Expand_Branch"));
	}
	if (top == TOP_bne || top == TOP_beq)
	  Build_OP(top, src1, TN_is_zero(src2) ? Zero_TN : src2, targ, ops);
	else Build_OP(top, src1, targ, ops);
      }
    }
    break;
  }
}

void Exp_Indirect_Branch (TN *targ_reg, OPS *ops)
{
#if defined(TARG_SL)
  Build_OP(TOP_mvtc, JA_TN, targ_reg, ops);
  Build_OP(TOP_jr, JA_TN, ops);
#else
  Build_OP(TOP_jr, targ_reg, ops);
#endif
}

#if defined(TARG_SL) && defined(TARG_SL2)
void Exp_Local_Jump(BB * bb, INT64 offset, OPS * ops)
{
  if(Gen_PIC_Shared) {
    ST *st = Gen_ST_For_BB(bb);
    TN *tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);  
    Exp_Lda(MTYPE_U4, tn, st, offset, OPERATOR_UNKNOWN, ops);
    Build_OP(TOP_jr, tn, ops);
  } else {

    /* non-shared and call-shared */
    LABEL_IDX lab = Gen_Label_For_BB(bb);
    TN *lab_tn = Gen_Label_TN(lab, offset);
    Build_OP(TOP_j, lab_tn, ops);
  }

}
#else
void Exp_Local_Jump(BB *bb, INT64 offset, OPS *ops)
{
  FmtAssert(FALSE, ("NYI: Exp_Local_Jump"));
}
#endif

void Exp_Return (TN *return_address, OPS *ops)
{
#if defined(TARG_SL)
  Build_OP(TOP_ret,return_address, ops);
#else
  Build_OP(TOP_jr, return_address, ops);
#endif
}

void Exp_Call (OPERATOR opr, TN *return_address, TN *target, OPS *ops)
{
  TOP top;
  switch (opr) {
  case OPR_CALL:
    top = TOP_jal;
    break;
  case OPR_ICALL:
    /*FALLTHROUGH*/
  case OPR_PICCALL:
    top = TOP_jalr;
#if defined(TARG_SL)
    Build_OP(TOP_mvtc, JA_TN, target, ops);
    Build_OP (top, return_address, JA_TN, ops);
    return;
#endif
    break;
  default:
    FmtAssert(FALSE, ("unexpected opr in Exp_Call"));
    /*NOTREACHED*/
  }
  Build_OP (top, return_address, target, ops);
}
