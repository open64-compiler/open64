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
#include "topcode.h"
#include "tn.h"
#include "cg_flags.h"
#include "targ_isa_lits.h"
#include "op.h"
#include "cg_spill.h"
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
	TOP cmp;
	TOP cmp_i;

	if (*src2 != NULL && TN_has_value(*src2)) {
		// swap operands and change variant
		TN *tmp = *src1;
		*src1 = *src2;
		*src2 = tmp;
		*variant = Invert_BR_Variant(*variant);
	}

	// check for special cases of first or second arg being zero.
	if (*src1 != NULL && TN_is_zero(*src1)) {
		switch (*variant) {
		case V_BR_U8LE:	
		case V_BR_U4LE:	
			*variant = V_BR_ALWAYS; break;
		case V_BR_U8GT:
		case V_BR_U4GT:
			*variant = V_BR_NEVER; break;
		}
	}
	if (*src2 != NULL && TN_is_zero_reg(*src2)) {
		switch (*variant) {
		case V_BR_U8LT:	
		case V_BR_U4LT:	
			*variant = V_BR_NEVER; break;
		case V_BR_U8GE:
		case V_BR_U4GE:
			*variant = V_BR_ALWAYS; break;
		}
	}

	// pick tops
	switch (*variant) {
	case V_BR_I8GE:	cmp_i = TOP_cmp_i_ge; cmp = TOP_cmp_ge; break;
	case V_BR_I8GT:	cmp_i = TOP_cmp_i_gt; cmp = TOP_cmp_gt; break; 
	case V_BR_I8LE:	cmp_i = TOP_cmp_i_le; cmp = TOP_cmp_le; break; 
	case V_BR_I8LT:	cmp_i = TOP_cmp_i_lt; cmp = TOP_cmp_lt; break; 
	case V_BR_U8GE:	cmp_i = TOP_cmp_i_geu; cmp = TOP_cmp_geu; break; 
	case V_BR_U8GT:	cmp_i = TOP_cmp_i_gtu; cmp = TOP_cmp_gtu; break; 
	case V_BR_U8LE:	cmp_i = TOP_cmp_i_leu; cmp = TOP_cmp_leu; break; 
	case V_BR_U8LT:	cmp_i = TOP_cmp_i_ltu; cmp = TOP_cmp_ltu; break; 
	case V_BR_I8EQ:	cmp_i = TOP_cmp_i_eq; cmp = TOP_cmp_eq; break; 
	case V_BR_U8EQ:	cmp_i = TOP_cmp_i_eq; cmp = TOP_cmp_eq; break;
	case V_BR_I8NE:	cmp_i = TOP_cmp_i_ne; cmp = TOP_cmp_ne; break;
	case V_BR_U8NE:	cmp_i = TOP_cmp_i_ne; cmp = TOP_cmp_ne; break;
	case V_BR_I4GE:	cmp_i = TOP_cmp4_i_ge; cmp = TOP_cmp4_ge; break;
	case V_BR_I4GT:	cmp_i = TOP_cmp4_i_gt; cmp = TOP_cmp4_gt; break;
	case V_BR_I4LE:	cmp_i = TOP_cmp4_i_le; cmp = TOP_cmp4_le; break;
	case V_BR_I4LT:	cmp_i = TOP_cmp4_i_lt; cmp = TOP_cmp4_lt; break;
	case V_BR_U4GE:	cmp_i = TOP_cmp4_i_geu; cmp = TOP_cmp4_geu; break;
	case V_BR_U4GT:	cmp_i = TOP_cmp4_i_gtu; cmp = TOP_cmp4_gtu; break;
	case V_BR_U4LE:	cmp_i = TOP_cmp4_i_leu; cmp = TOP_cmp4_leu; break;
	case V_BR_U4LT:	cmp_i = TOP_cmp4_i_ltu; cmp = TOP_cmp4_ltu; break;
	case V_BR_I4EQ:	cmp_i = TOP_cmp4_i_eq; cmp = TOP_cmp4_eq; break;
	case V_BR_U4EQ:	cmp_i = TOP_cmp4_i_eq; cmp = TOP_cmp4_eq; break;
	case V_BR_I4NE:	cmp_i = TOP_cmp4_i_ne; cmp = TOP_cmp4_ne; break; 
	case V_BR_U4NE:	cmp_i = TOP_cmp4_i_ne; cmp = TOP_cmp4_ne; break; 
	case V_BR_FEQ:	/*FALLTHROUGH*/
	case V_BR_DEQ:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_eq; break;
	case V_BR_XEQ:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_eq; break;
	case V_BR_FLT:	/*FALLTHROUGH*/
	case V_BR_DLT:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_lt; break;
	case V_BR_XLT:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_lt; break;
	case V_BR_FLE:	/*FALLTHROUGH*/
	case V_BR_DLE:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_le; break;
	case V_BR_XLE:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_le; break;
	case V_BR_FNE:	/*FALLTHROUGH*/
	case V_BR_DNE:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_neq; break;
	case V_BR_XNE:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_neq; break;
	case V_BR_FGT:	/*FALLTHROUGH*/
	case V_BR_DGT:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_gt; break;
	case V_BR_XGT:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_gt; break;
	case V_BR_FGE:	/*FALLTHROUGH*/
	case V_BR_DGE:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_ge; break;
	case V_BR_XGE:	cmp_i = TOP_UNDEFINED; cmp = TOP_fcmp_ge; break;
	default:	cmp_i = TOP_UNDEFINED; cmp = TOP_UNDEFINED; break;
	}

	// if src1 is immed and fits, use immed form of top
	if (cmp_i != TOP_UNDEFINED && *src1 != NULL && TN_has_value(*src1)) {
		const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(cmp_i);
		const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, 1);
		ISA_LIT_CLASS lc = ISA_OPERAND_VALTYP_Literal_Class(otype);
		if (ISA_LC_Value_In_Class(TN_value(*src1), lc))
		    cmp = cmp_i;
		else
		    *src1 = Expand_Immediate_Into_Register(*src1, ops);
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
  if ( Trace_Exp2 ) {
    fprintf ( TFile, "<cgexp> transformed branch cond = %lld\n", cond);
  }

  switch (cond) {
  case V_BR_ALWAYS:
  case V_BR_NEVER:
    Is_True(cmp == TOP_UNDEFINED, 
	    ("unexpected compare op for %s", BR_Variant_Name(cond)));
    if ((cond == V_BR_ALWAYS) ^ false_br) {
      // Unconditional branch for ALWAYS/!false_br and NEVER/false_br
      Build_OP (TOP_br, Gen_Enum_TN(ECV_ph_few), Gen_Enum_TN(ECV_dh), targ, ops);
    }
    break;
  case V_BR_PEQ:
  case V_BR_PNE:
    {
      Is_True(cmp == TOP_UNDEFINED, 
	      ("unexpected compare op for V_BR_PEQ/V_BR_PNE"));
      TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      TN *tn = Build_TN_Of_Mtype (MTYPE_I8);
      TOP action = (cond == V_BR_PEQ) ? TOP_cmp_ne : TOP_cmp_eq;
      //handle when the kids of BBNE/BBEQ are inconst
      if(TN_is_constant(src1))
	{
	  if(TN_value(src1) == 1)
	    src1 = True_TN;
	  else  if(TN_value(src1) == 0)
	    src1 = Zero_TN;
	  else
	    FmtAssert(TN_value(src1) == 1 || TN_value(src1) == 0, ("unexpect operands for V_BR_PEQ/V_BR_PNE"));
	}
      if(TN_is_constant(src2))
        {
          if(TN_value(src2) == 1)
            src1 = True_TN;
          else  if(TN_value(src2) == 0)
            src2 = Zero_TN;
          else
            FmtAssert(TN_value(src2) == 1 || TN_value(src2) == 0, ("unexpect operands for V_BR_PEQ/V_BR_PNE"));
        }

      // tn = (src1 == src2)
      Build_OP (TOP_mov_i, tn, True_TN, Gen_Literal_TN(1, 8), ops);
      Build_OP (TOP_xor_i, tn, src1, Gen_Literal_TN(1, 8), tn, ops);
      Build_OP (TOP_xor_i, tn, src2, Gen_Literal_TN(1, 8), tn, ops);

      // p1,p2 = (src1 <cond> src2)
      Build_OP (action, p1, p2, True_TN, tn, Zero_TN, ops);

      Build_OP (TOP_br_cond, 
	        false_br ? p2 : p1,
	        Gen_Enum_TN(ECV_bwh_dptk),
	        Gen_Enum_TN(ECV_ph_few),
	        Gen_Enum_TN(ECV_dh),
	        targ, ops);
    }
    break;
  case V_BR_P_TRUE:
    Is_True(cmp == TOP_UNDEFINED, ("unexpected compare op for V_BR_P_TRUE"));
    if (false_br) {
      DevWarn("inverted V_BR_P_TRUE");
      TN *tmp = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
      Exp_Pred_Complement(tmp, True_TN, src1, ops);
      src1 = tmp;
    }
    Build_OP (TOP_br_cond, 
	      src1,
	      Gen_Enum_TN(ECV_bwh_dptk),
	      Gen_Enum_TN(ECV_ph_few),
	      Gen_Enum_TN(ECV_dh),
	      targ, ops);
    break;
  default:
    {
      // integer or floating point conditional branch
      FmtAssert(cmp != TOP_UNDEFINED, ("Expand_Branch: unexpected comparison"));
      TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      if (TOP_is_flop(cmp)) {
	Build_OP (cmp, p1, p2, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
      } else {
	Build_OP (cmp, p1, p2, True_TN, src1, src2, ops);
      }
      Build_OP (TOP_br_cond, 
	        false_br ? p2 : p1, 
	        Gen_Enum_TN(ECV_bwh_dptk),
	        Gen_Enum_TN(ECV_ph_few),
	        Gen_Enum_TN(ECV_dh),
	        targ, ops);
    }
    break;
  }
}

void Exp_Indirect_Branch (TN *targ_reg, OPS *ops)
{
	if (TN_register_class(targ_reg) != ISA_REGISTER_CLASS_branch) {
  		TN *tmp = Build_RCLASS_TN (ISA_REGISTER_CLASS_branch);
		Exp_COPY (tmp, targ_reg, ops);
		targ_reg = tmp;
	}
	Build_OP (TOP_br_r, 
		  Gen_Enum_TN(ECV_ph_few),
		  Gen_Enum_TN(ECV_dh),
		  targ_reg,
		  ops);
}

void Exp_Local_Jump(BB *bb, INT64 offset, OPS *ops)
{
  FmtAssert(FALSE, ("NYI: Exp_Local_Jump"));
}

void Exp_Return (TN *return_address, OPS *ops)
{
        Build_OP (TOP_br_ret, True_TN, 
		  Gen_Enum_TN(ECV_bwh_sptk),
		  Gen_Enum_TN(ECV_ph_many),
		  Gen_Enum_TN(ECV_dh),
		  return_address, ops);
}

void Exp_Call (OPERATOR opr, TN *return_address, TN *target, OPS *ops)
{
  TOP top;
  TN *br_tmp;
  TN *ar_ec = Build_Dedicated_TN ( ISA_REGISTER_CLASS_application,
				   (REGISTER)(REGISTER_MIN + 66), 
				   8);
  switch (opr) {
  case OPR_CALL:
    top = TOP_br_call; break;
  case OPR_ICALL:
    if ( ! Get_Trace (TP_CGEXP, 256)) {
      // target is at 0(target), gp at 8(target)
      OPCODE opc = OPCODE_make_op (OPR_LDID, Pointer_Mtype, Pointer_Mtype);
      TN *tmp1 = Build_TN_Of_Mtype (Pointer_Mtype);

      // bug fix for OSP_144
      // Do not change gp with -mconstant-gp
      if (!Constant_GP)
	Expand_Load (opc, GP_TN, target, Gen_Literal_TN(8, 4), V_NONE, ops);
      
      Expand_Load (opc, tmp1, target, Gen_Literal_TN(0, 4), V_NONE, ops);
      target = tmp1;
    }
    /*FALLTHROUGH*/
  case OPR_PICCALL:
    top = TOP_br_r_call;
    br_tmp = Build_RCLASS_TN(ISA_REGISTER_CLASS_branch);
    Build_OP (TOP_mov_t_br, br_tmp, True_TN, target, ops);
    target = br_tmp;
    break;
  default:
    FmtAssert(FALSE, ("unexpected opr in Exp_Call"));
    /*NOTREACHED*/
  }
  Build_OP (top, return_address, True_TN, 
	    (opr == OPR_ICALL ? Gen_Enum_TN(ECV_bwh_dptk)
			      : Gen_Enum_TN(ECV_bwh_sptk) ),
	    Gen_Enum_TN(ECV_ph_many),
	    Gen_Enum_TN(ECV_dh),
	    target, 
	    ar_ec, ops);
}
