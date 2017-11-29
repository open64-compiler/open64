/*
 * Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
 */

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
#include "olive_convert_wn.h"

void
Initialize_Branch_Variants(void)
{
	// nothing to do
}

BOOL Cond_Is_64Bit(VARIANT cond)
{
  return (V_BR_I8EQ0 <= cond) && (cond <= V_BR_U8LE);
}

// Check that compare is of proper form,
// and return TOP to use for the compare.
// May modify the variant and src tns.
TOP Pick_Compare_TOP (VARIANT *variant, TN **src1, TN **src2, OPS *ops)
{
  TOP cmp = TOP_UNDEFINED;
  TOP cmp_i = TOP_UNDEFINED;
  INT64 val;

  // pick tops
  switch (*variant) {
  case V_BR_I4GT:
  case V_BR_I4GE:
  case V_BR_I4EQ:
  case V_BR_I4LE:
  case V_BR_I4LT:
  case V_BR_I4NE:
  case V_BR_I8GT:
  case V_BR_I8GE:
  case V_BR_I8EQ:
  case V_BR_I8LE:
  case V_BR_I8LT:
  case V_BR_I8NE:
  	cmp_i = TOP_cmpwi;
	cmp = TOP_cmpw;
	break;

  case V_BR_U4GT:
  case V_BR_U4GE:
  case V_BR_U4EQ:
  case V_BR_U4LE:
  case V_BR_U4LT:
  case V_BR_U4NE:
    
  case V_BR_U8GT:
  case V_BR_U8GE:
  case V_BR_U8EQ:
  case V_BR_U8LE:
  case V_BR_U8LT:
  case V_BR_U8NE:

#ifdef KEY
  case V_BR_I8GT0:	
  case V_BR_I4GT0:	
  case V_BR_U8GT0:	
  case V_BR_U4GT0:    

  case V_BR_I8GE0:	
  case V_BR_I4GE0:	
  case V_BR_U8GE0:	
  case V_BR_U4GE0:

  case V_BR_I8EQ0:	
  case V_BR_I4EQ0:	
  case V_BR_U8EQ0:	
  case V_BR_U4EQ0:

  case V_BR_I8LE0:	
  case V_BR_I4LE0:	
  case V_BR_U8LE0:	
  case V_BR_U4LE0:

  case V_BR_I8LT0:	
  case V_BR_I4LT0:	
  case V_BR_U8LT0:	
  case V_BR_U4LT0:

  case V_BR_I8NE0:	
  case V_BR_I4NE0:	
  case V_BR_U8NE0:	
  case V_BR_U4NE0:
#endif    
    cmp_i = TOP_cmplwi; cmp = TOP_cmplw; 
    break;

  case V_BR_FEQ:	
  case V_BR_DEQ:	
  case V_BR_FLT:	
  case V_BR_DLT:	
  case V_BR_FLE:	
  case V_BR_DLE:	
  case V_BR_FNE:	
  case V_BR_DNE:	
  case V_BR_FGT: 
  case V_BR_DGT:
  case V_BR_FGE:	
  case V_BR_DGE:	
    cmp = TOP_fcmpu;
    break;
  default:	;
  }

if (*src1 != NULL && TN_has_value(*src1)) {
	  TN *tmp = *src1;
	  *src1 = *src2;
	  *src2 = tmp;
	  // variant will not effect compare inst
	  *variant = Invert_BR_Variant(*variant);
  }

  // if src2 is immed and fits, use immed form of top
  if ( *src1 != NULL && !Cond_Is_64Bit(*variant) && *src2 != NULL && TN_has_value(*src2)) {
    if ((cmp_i == TOP_cmpwi && ISA_LC_Value_In_Class(TN_value(*src2), LC_simm16))
		|| (cmp_i == TOP_cmplwi && ISA_LC_Value_In_Class(TN_value(*src2), LC_uimm16))) {
      cmp = cmp_i;
    }
  }

  return cmp;
}

TOP Cond_To_Top(VARIANT cond)
{
  TOP top = TOP_UNDEFINED;
  switch (cond)
  {
  case V_BR_I8GT:	
  case V_BR_I4GT:	
  case V_BR_U8GT:	
  case V_BR_U4GT:
  case V_BR_FGT:
  case V_BR_DGT:
#ifdef KEY
  case V_BR_I8GT0:	
  case V_BR_I4GT0:	
  case V_BR_U8GT0:	
  case V_BR_U4GT0:
#endif
    top = TOP_bgt; break;    

  case V_BR_I8GE:	
  case V_BR_I4GE:	
  case V_BR_U8GE:	
  case V_BR_U4GE:
  case V_BR_FGE:
  case V_BR_DGE:
#ifdef KEY
  case V_BR_I8GE0:	
  case V_BR_I4GE0:	
  case V_BR_U8GE0:	
  case V_BR_U4GE0:
#endif
    top = TOP_bge; break;
    
  case V_BR_I8EQ:	
  case V_BR_I4EQ:	
  case V_BR_U8EQ:	
  case V_BR_U4EQ:
  case V_BR_FEQ:
  case V_BR_DEQ:
#ifdef KEY
  case V_BR_I8EQ0:	
  case V_BR_I4EQ0:	
  case V_BR_U8EQ0:	
  case V_BR_U4EQ0:
#endif
    top = TOP_beq; break;    
    
  case V_BR_I8LE:	
  case V_BR_I4LE:	
  case V_BR_U8LE:	
  case V_BR_U4LE:
  case V_BR_FLE:
  case V_BR_DLE:
#ifdef KEY
  case V_BR_I8LE0:	
  case V_BR_I4LE0:	
  case V_BR_U8LE0:	
  case V_BR_U4LE0:
#endif
    top = TOP_ble; break;   
    
  case V_BR_I8LT:	
  case V_BR_I4LT:	
  case V_BR_U8LT:	
  case V_BR_U4LT:
  case V_BR_FLT:
  case V_BR_DLT:
#ifdef KEY
  case V_BR_I8LT0:	
  case V_BR_I4LT0:	
  case V_BR_U8LT0:	
  case V_BR_U4LT0:
#endif
    top = TOP_blt; break;        
    
  case V_BR_I8NE:	
  case V_BR_I4NE:	
  case V_BR_U8NE:	
  case V_BR_U4NE:
  case V_BR_FNE:
  case V_BR_DNE:
#ifdef KEY
  case V_BR_I8NE0:	
  case V_BR_I4NE0:	
  case V_BR_U8NE0:	
  case V_BR_U4NE0:
#endif
    top = TOP_bne; break;
  default:
  FmtAssert(FALSE, ("Not handled condition branch!"));  
 
  }

  FmtAssert(top != TOP_UNDEFINED, ("Cond to top failed"));
  return top;
}

TOP
Negative_Branch_TOP(TOP br)
{
  switch (br) {
    case TOP_bgt: return TOP_ble;
    case TOP_bge: return TOP_blt;
    case TOP_beq: return TOP_bne;
    case TOP_bne: return TOP_beq;
    case TOP_ble: return TOP_bgt;
    case TOP_blt: return TOP_bge;
  }
  FmtAssert(FALSE, ("unknown branch Top in Negative_Branch_TOP"));
}

extern OPS New_OPs;
extern OP *Last_Processed_OP;
extern INT total_bb_insts;
extern BB *Cur_BB;
extern void Process_New_OPs(void);

void
Expand_64Bit_Branch(TOP cmp, TN* targ, TN* src1_high, TN* src1_low, TN* src2_high, TN* src2_low, VARIANT variant, OPS *ops)
{
  VARIANT cond = V_br_condition(variant);
  TN* cr = Gen_CR_TN(2);
  if (cond == V_BR_I8EQ || cond == V_BR_U8EQ) {
    BB* bb_entry = Cur_BB;
    BB* bb_cmp_low = Gen_And_Append_BB(bb_entry);
    BB* bb_exit = Gen_And_Append_BB(bb_cmp_low);
    const LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);
    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = label_bb_exit;
    Build_OP(cmp, cr, src1_high, src2_high, ops);
    Build_OP(TOP_bne, cr, Gen_Label_TN(label_bb_exit, 0), ops);

    if( ops != &New_OPs )
        OPS_Append_Ops(&New_OPs, ops);
    
    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    BB_branch_wn(bb_cmp_low) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_low)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_low)) = TN_label(targ);

    OPS* bb_cmp_low_ops = &New_OPs;
    if (TN_has_value(src2_low)) {
      Build_OP(TOP_cmplwi, cr, src1_low, src2_low, bb_cmp_low_ops);
    }
    else {
      Build_OP(TOP_cmplw, cr, src1_low, src2_low, bb_cmp_low_ops);
    }
    Build_OP(TOP_beq, cr, targ, bb_cmp_low_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Cur_BB = bb_exit;
    return;
  }
  
  if (cond == V_BR_I8NE || cond == V_BR_U8NE) {
    BB* bb_entry = Cur_BB;
    BB* bb_cmp_low = Gen_And_Append_BB(bb_entry);
    BB* bb_exit = Gen_And_Append_BB(bb_cmp_low);
    const LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = TN_label(targ);
    Build_OP(cmp, cr, src1_high, src2_high, ops);
    Build_OP(TOP_bne, cr, targ, ops);

    if( ops != &New_OPs )
        OPS_Append_Ops(&New_OPs, ops);
    
    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    BB_branch_wn(bb_cmp_low) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_low)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_low)) = TN_label(targ);

    OPS* bb_cmp_low_ops = &New_OPs;
    if (TN_has_value(src2_low)) {
      Build_OP(TOP_cmplwi, cr, src1_low, src2_low, bb_cmp_low_ops);
    }
    else {
      Build_OP(TOP_cmplw, cr, src1_low, src2_low, bb_cmp_low_ops);
    }
    Build_OP(TOP_bne, cr, targ, bb_cmp_low_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Cur_BB = bb_exit;
    return;
  }

  TOP br1;
  TOP br2;
  TOP br3;
  switch (cond) {
    case V_BR_I8GT:
    case V_BR_U8GT:
		br1 = TOP_bgt;
		br2 = TOP_blt;
		br3 = TOP_bgt;
		break;
    case V_BR_I8GE:
    case V_BR_U8GE:
		br1 = TOP_bgt;
		br2 = TOP_blt;
		br3 = TOP_bge;
		break;
    case V_BR_I8LE:
    case V_BR_U8LE:
		br1 = TOP_blt;
		br2 = TOP_bgt;
		br3 = TOP_ble;
		break;
    case V_BR_I8LT:
    case V_BR_U8LT:
		br1 = TOP_blt;
		br2 = TOP_bgt;
		br3 = TOP_blt;
		break;
    default:
		FmtAssert(FALSE, ("unknown condition in Expand_64Bit_Branch"));
  }

    BB* bb_entry = Cur_BB;
    BB* bb_cmp_high = Gen_And_Append_BB(bb_entry);
    BB* bb_cmp_low = Gen_And_Append_BB(bb_cmp_high);
    BB* bb_exit = Gen_And_Append_BB(bb_cmp_low);
    const LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = TN_label(targ);

    Build_OP(cmp, cr, src1_high, src2_high, ops);
    Build_OP(br1, cr, targ, ops);

    if( ops != &New_OPs )
        OPS_Append_Ops(&New_OPs, ops);
    
    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    BB_branch_wn(bb_cmp_high) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_high)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_high)) = label_bb_exit;
    
    OPS* bb_cmp_high_ops = &New_OPs;
    Build_OP(cmp, cr, src1_high, src2_high, bb_cmp_high_ops);
    Build_OP(br2, cr, Gen_Label_TN(label_bb_exit, 0), bb_cmp_high_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_high, bb_cmp_high_ops);
    OPS_Init(bb_cmp_high_ops);

    BB_branch_wn(bb_cmp_low) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_low)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_low)) = TN_label(targ);

    OPS* bb_cmp_low_ops = &New_OPs;
    if (TN_has_value(src2_low)) {
      Build_OP(TOP_cmplwi, cr, src1_low, src2_low, bb_cmp_low_ops);
    }
    else {
      Build_OP(TOP_cmplw, cr, src1_low, src2_low, bb_cmp_low_ops);
    }
    Build_OP(br3, cr, targ, bb_cmp_low_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Cur_BB = bb_exit;
}

void
Expand_64Bit_Branch0(TOP cmp, TN *targ, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  TN * src2_high  = Get_TN_Pair(src2);
  TN * src2_low   = src2;
  TN * src1_high  = Get_TN_Pair(src1);
  TN * src1_low   = src1;

  if (TN_has_value(src2)) {
    src2_low = Expand_Immediate_Into_Register(NULL, src2, MTYPE_I8, ops);
    src2_high  = Get_TN_Pair(src2_low);
  }
  else {
    if (!src2_high) {
      src2_high = Build_TN_Like(src2);
      Build_OP(TOP_srawi, src2_high, src2, Gen_Literal_TN(31, 4), ops);
    }
  }
  
  FmtAssert(src2_high && src1_high, ("Expand_64Bit_Branch0 src2_high/src1_high is NULL"));
  
  cmp = Pick_Compare_TOP(&variant, &src1_low, &src2_low, ops);
  Expand_64Bit_Branch(cmp, targ, src1_high, src1_low, src2_high, src2_low, variant, ops);
}

void
Expand_Branch (TN *targ, TN *src1, TN *src2, VARIANT variant, OPS *ops)
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
  
  

  if ( Trace_Exp2 && cond != variant) {
    fprintf ( TFile, "<cgexp> transformed branch cond = %lld\n", cond);
  }

  switch (cond) {
  case V_BR_ALWAYS:
  case V_BR_NEVER:
    if ((cond == V_BR_ALWAYS) ^ false_br) {
      // Unconditional branch for ALWAYS/!false_br and NEVER/false_br
      Build_OP (TOP_b, targ, ops);
    }
    break;
  case V_BR_PEQ:
  case V_BR_PNE:
  case V_BR_P_TRUE:
    FmtAssert(FALSE, ("unimplemented branch variant in Expand_Branch"));
    break;
  default:
      // integer, floating point and long long conditional branch
      cmp = Pick_Compare_TOP (&cond, &src1, &src2, ops);
      if (cmp == TOP_UNDEFINED) {
        FmtAssert(FALSE, ("unkown condition in Expand_Branch"));
      }

      // if (Cond_Is_64Bit(cond) && Get_TN_Pair(src1) != NULL) { // handle long long
      if (Cond_Is_64Bit(cond) && Get_TN_Pair(src1) != NULL) {
        Expand_64Bit_Branch0(cmp, targ, src1, src2, variant, ops);
        return;
      }
      
      if ((src2 != NULL) && (TN_has_value(src2)) ) {
        if ((cmp == TOP_cmpw) && (!ISA_LC_Value_In_Class(TN_value(src2), LC_simm16)) ||
          (cmp == TOP_cmplw) && (!ISA_LC_Value_In_Class(TN_value(src2), LC_uimm16)) ) {
          src2 = Expand_Immediate_Into_Register(NULL, src2, 
            (cmp == TOP_cmplw) ? MTYPE_U4 : MTYPE_I4, ops);
        }
      }

      TN * cr = Gen_CR_TN(7);
      
      Build_OP(cmp, cr, src1, src2, ops);
      TOP top_br;
      if (!TOP_is_flop(cmp)) {  // integer
        top_br = Cond_To_Top(cond);
        Build_OP (top_br, cr, targ, ops);
      }
      else { // handle float
        TN* new_cr = Gen_CR_TN(0);
        switch (cond) {
          case V_BR_FGT:
          case V_BR_DGT:
          case V_BR_FLT:
          case V_BR_DLT:
          case V_BR_FEQ:
          case V_BR_DEQ:
          case V_BR_FNE:
          case V_BR_DNE:
    	      top_br = Cond_To_Top(cond);
    	      // in fact, PowerPC's ble means "not greater", not means "less or equal"
    	      // it also works when floatint point compare unordered
    	      // the same as bge and bne
    	      if (false_br) {
    	        top_br = Negative_Branch_TOP(top_br);
    	      }
          Build_OP (top_br, cr, targ, ops);
	      return;

          // !important: don't use bge for V_BR_FGE and V_BR_DGE
          case V_BR_FGE:
          case V_BR_DGE:
            Build_OP(TOP_cror, new_cr, Gen_Literal_TN(2, 4), Gen_Literal_TN(29, 4), 
              Gen_Literal_TN(30, 4), cr, cr, ops);
            break;
          case V_BR_FLE:
          case V_BR_DLE:
            Build_OP(TOP_cror, new_cr, Gen_Literal_TN(2, 4), Gen_Literal_TN(28, 4), 
              Gen_Literal_TN(30, 4), cr, cr, ops);
            break;
          default:
            FmtAssert(FALSE, ("Not Here"));
        }

        if (false_br) {
          Build_OP (TOP_bne, new_cr, targ, ops);
        }
        else {
          Build_OP (TOP_beq, new_cr, targ, ops);
        }
      }      
    }
}

void Exp_Indirect_Branch (TN *targ_reg, OPS *ops)
{
  Build_OP(TOP_mtctr, targ_reg, ops);
  Build_OP(TOP_bctr, targ_reg, ops);
}

void Exp_Local_Jump(BB *bb, INT64 offset, OPS *ops)
{
  FmtAssert(FALSE, ("NYI: Exp_Local_Jump"));
}

void Exp_Return (TN *return_address, OPS *ops)
{
   Build_OP(TOP_blr, return_address, ops);
}

void Exp_Call (OPERATOR opr, TN *return_address, TN *target, OPS *ops)
{
  TOP top;
  TN * ja_tn = NULL;
  TN * cnd   = NULL;
  TN* cr = NULL;
  switch (opr) {
  case OPR_CALL:
    top = TOP_bl;
    Build_OP (top,  target, ops);
    break;
  case OPR_ICALL:
    /*FALLTHROUGH*/
  case OPR_PICCALL:
    cnd = Gen_Literal_TN(6, 4);
    cr = Gen_CR_TN(0);
    Build_OP(TOP_mtctr, target, ops);
    Build_OP(TOP_creqv, cr, cnd, cnd, cnd, cr, cr, ops);
    Build_OP (TOP_bctrl,  ops);
    break;
  default:
    FmtAssert(FALSE, ("unexpected opr in Exp_Call"));
    /*NOTREACHED*/
  }
  
}
