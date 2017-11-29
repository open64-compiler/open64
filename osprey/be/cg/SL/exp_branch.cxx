/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

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

extern OPS   New_OPs;
extern OP *  Last_Processed_OP;
extern INT   total_bb_insts;
extern BB *  Cur_BB;

extern INT64 Get_TN_Value(TN * tn);
extern TN *  Get_64Bit_High_TN(TN * low, TYPE_ID type, OPS * ops);
extern void  Process_New_OPs(void);

void
Initialize_Branch_Variants(void)
{
	// nothing to do
}

BOOL 
Cond_Is_64Bit(VARIANT cond)
{
  const int COND_OP[] = {
    V_BR_I8EQ0, V_BR_I8NE0, V_BR_I8GT0, V_BR_I8GE0, V_BR_I8LT0, V_BR_I8LE0,           
    V_BR_I8EQ,  V_BR_I8NE,  V_BR_I8GT,  V_BR_I8GE,  V_BR_I8LT,  V_BR_I8LE,
    V_BR_U8EQ0, V_BR_U8NE0, V_BR_U8GT0, V_BR_U8GE0, V_BR_U8LT0, V_BR_U8LE0,
    V_BR_U8EQ,  V_BR_U8NE,  V_BR_U8GT,  V_BR_U8GE,  V_BR_U8LT,  V_BR_U8LE};

  for (int i = 0; i < sizeof(COND_OP) / sizeof(COND_OP[0]); i++) {
    if (cond == COND_OP[i]) return TRUE;		
  }

	return FALSE;
}

BOOL
Cond_Is_Unsigned(VARIANT cond)
{
  const int UNSIGNED_COND_OP[] = {
    V_BR_U4EQ0, V_BR_U4NE0, V_BR_U4GT0, V_BR_U4GE0, V_BR_U4LT0, V_BR_U4LE0,
    V_BR_U4EQ,  V_BR_U4NE,  V_BR_U4GT,  V_BR_U4GE,  V_BR_U4LT,  V_BR_U4LE,
    V_BR_U8EQ0, V_BR_U8NE0, V_BR_U8GT0, V_BR_U8GE0, V_BR_U8LT0, V_BR_U8LE0,
    V_BR_U8EQ,  V_BR_U8NE,  V_BR_U8GT,  V_BR_U8GE,  V_BR_U8LT,  V_BR_U8LE
  };

  const int SIGNED_COND_OP[] = {
    V_BR_I4EQ0, V_BR_I4NE0, V_BR_I4GT0, V_BR_I4GE0, V_BR_I4LT0, V_BR_I4LE0,           
    V_BR_I4EQ,  V_BR_I4NE,  V_BR_I4GT,  V_BR_I4GE,  V_BR_I4LT,  V_BR_I4LE,
    V_BR_I8EQ0, V_BR_I8NE0, V_BR_I8GT0, V_BR_I8GE0, V_BR_I8LT0, V_BR_I8LE0,           
    V_BR_I8EQ,  V_BR_I8NE,  V_BR_I8GT,  V_BR_I8GE,  V_BR_I8LT,  V_BR_I8LE
  };

  for (int i = 0; i < sizeof(UNSIGNED_COND_OP) / sizeof(UNSIGNED_COND_OP[0]); i++) {
    if (cond == UNSIGNED_COND_OP[i]) return TRUE;		
  }

  for (int i = 0; i < sizeof(SIGNED_COND_OP) / sizeof(SIGNED_COND_OP[0]); i++) {
    if (cond == SIGNED_COND_OP[i]) return FALSE;		
  }

  FmtAssert(FALSE, ("Cond_Is_Unsigned: Branch cond NYI"));
  return FALSE;
}

void
Expand_64Bit_Branch(TN* targ, TN* src1, TN* src2, VARIANT variant, OPS *ops)
{
  TN * src1_low   = src1;
  TN * src1_high  = Get_TN_Pair(src1);
  TN * src2_low   = src2;
  TN * src2_high  = Get_TN_Pair(src2);

  if (TN_has_value(src1)) {
    Expand_64Bit_Branch(targ, src2, src1, Invert_BR_Variant(variant), ops);
    return;
  }

  VARIANT cond = V_br_condition(variant);

  if (src1_high == NULL) {
    src1_high = Get_64Bit_High_TN(src1_low, Cond_Is_Unsigned(cond) ? 
        MTYPE_U8 : MTYPE_I8, ops);
  }

  if (TN_is_register(src2) && (src2_high == NULL)) {
    src2_high = Get_64Bit_High_TN(src2_low, Cond_Is_Unsigned(cond) ? 
        MTYPE_U8 : MTYPE_I8, ops);
  } else if (TN_has_value(src2)) {
    INT64 val      = Get_TN_Value(src2);
    INT32 val_high = (val >> 32);
    UINT32 val_low = val & 0XFFFFFFFF;
    src2_high = Gen_Literal_TN(val_high, 4);
    src2_low  = Gen_Literal_TN(val_low,  4);	
  }

  FmtAssert((src2_high != NULL) && (src1_high != NULL), ("Expand_64Bit_Branch src2_high/src1_high is NULL"));

  if (cond == V_BR_I8EQ || cond == V_BR_U8EQ) {
    BB* bb_entry   = Cur_BB;
    BB* bb_cmp_low = Gen_And_Append_BB(bb_entry);
    BB* bb_exit    = Gen_And_Append_BB(bb_cmp_low);

    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);
    BB_branch_wn(bb_entry)  = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = label_bb_exit;

    Expand_Branch(Gen_Label_TN(label_bb_exit, 0), src1_high, src2_high, 
        cond == V_BR_I8EQ ? V_BR_I4NE : V_BR_U4NE, ops);

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

    Expand_Branch(targ, src1_low, src2_low, V_BR_U4EQ, bb_cmp_low_ops);

    total_bb_insts    = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);

    total_bb_insts    = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    OPS_Init(&New_OPs);

    Cur_BB            = bb_exit;
    return;
  }
  if (cond == V_BR_I8NE || cond == V_BR_U8NE) {
    BB* bb_entry   = Cur_BB;
    BB* bb_cmp_low = Gen_And_Append_BB(bb_entry);
    BB* bb_exit    = Gen_And_Append_BB(bb_cmp_low);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    BB_branch_wn(bb_entry)  = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = TN_label(targ);

    Expand_Branch(targ, src1_high, src2_high, 
        cond == V_BR_I8NE ? V_BR_I4NE : V_BR_U4NE, ops);

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

    Expand_Branch(targ, src1_low, src2_low, V_BR_U4NE, bb_cmp_low_ops);    

    total_bb_insts    = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);

    total_bb_insts    = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    OPS_Init(&New_OPs);

    Cur_BB            = bb_exit;
    return;
  }

  VARIANT br1_cond, br2_cond, br3_cond;
  switch (cond) {
    case V_BR_I8GT: // GT, LT, GT
      br1_cond = V_BR_I4GT;
      br2_cond = V_BR_I4LT;
      br3_cond = V_BR_U4GT;
      break;
    case V_BR_U8GT: 
      br1_cond = V_BR_U4GT;
      br2_cond = V_BR_U4LT;
      br3_cond = V_BR_U4GT;
      break;
    case V_BR_I8GE: // GT, LT, GE
      br1_cond = V_BR_I4GT;
      br2_cond = V_BR_I4LT;
      br3_cond = V_BR_U4GE;
      break;
    case V_BR_U8GE:
      br1_cond = V_BR_U4GT;
      br2_cond = V_BR_U4LT;
      br3_cond = V_BR_U4GE;
      break;
    case V_BR_I8LE: // LT, GT, LE
      br1_cond = V_BR_I4LT;
      br2_cond = V_BR_I4GT;
      br3_cond = V_BR_U4LE;
      break;		
    case V_BR_U8LE:
      br1_cond = V_BR_U4LT;
      br2_cond = V_BR_U4GT;
      br3_cond = V_BR_U4LE;
      break;		
    case V_BR_I8LT: // LT, GT, LT
      br1_cond = V_BR_I4LT;
      br2_cond = V_BR_I4GT;
      br3_cond = V_BR_U4LT;
      break;		
    case V_BR_U8LT:
      br1_cond = V_BR_U4LT;
      br2_cond = V_BR_U4GT;
      br3_cond = V_BR_U4LT;
      break;
    default:
      FmtAssert(FALSE, ("unknown condition in Expand_64Bit_Branch"));
  }

  BB* bb_entry    = Cur_BB;
  BB* bb_cmp_high = Gen_And_Append_BB(bb_entry);
  BB* bb_cmp_low  = Gen_And_Append_BB(bb_cmp_high);
  BB* bb_exit     = Gen_And_Append_BB(bb_cmp_low);
  LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

  BB_branch_wn(bb_entry)  = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = TN_label(targ);

  Expand_Branch(targ, src1_high, src2_high, br1_cond, ops);

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
  Expand_Branch(Gen_Label_TN(label_bb_exit, 0), src1_high, src2_high, br2_cond, bb_cmp_high_ops);

  total_bb_insts    = 0;
  Last_Processed_OP = NULL;
  Process_New_OPs();
  BB_Append_Ops(bb_cmp_high, bb_cmp_high_ops);
  OPS_Init(bb_cmp_high_ops);

  BB_branch_wn(bb_cmp_low) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_cmp_low)) = NULL;
  WN_label_number(BB_branch_wn(bb_cmp_low)) = TN_label(targ);

  OPS* bb_cmp_low_ops = &New_OPs;

  Expand_Branch(targ, src1_low, src2_low, br3_cond, bb_cmp_low_ops);

  total_bb_insts    = 0;
  Last_Processed_OP = NULL;
  Process_New_OPs();
  BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
  OPS_Init(bb_cmp_low_ops);

  total_bb_insts    = 0;
  Last_Processed_OP = NULL;
  Process_New_OPs();
  OPS_Init(&New_OPs);

  Cur_BB = bb_exit;
}

// Check that compare is of proper form,
// and return TOP to use for the compare.
// May modify the variant and src tns.
TOP
Pick_Compare_TOP (VARIANT *variant, TN **src1, TN **src2, OPS *ops)
{
  TOP cmp = TOP_UNDEFINED;
  TOP cmp_i = TOP_UNDEFINED;
  int val;

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
	  // because src2 is zero, and comparison is unsigned
	  case V_BR_U4GT: 
	          *variant = V_BR_U4NE; break;
	  // because src2 is zero, and comparison is unsigned
	  case V_BR_U8GT:
	          *variant = V_BR_U8NE; break;
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
    if (TN_is_zero(*src2)) {
    } else if (TN_has_value(*src2)) { // add 1 to value and handle as GE
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
    if (TN_is_zero(*src2)) {
    } else if (TN_has_value(*src2)) { // add 1 to value and handle as GE
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
    if (TN_is_zero(*src2)) {
    } else if (TN_has_value(*src2)) { // subtract 1 from value and handle as LT
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
    if (TN_is_zero(*src2)) {
    } else if (TN_has_value(*src2)) { // subtract 1 from value and handle as LT
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

  if (Cond_Is_64Bit(cond) && Get_TN_Pair(src1) != NULL) {
    Expand_64Bit_Branch(targ, src1, src2, variant, ops);
    return;
  }

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
	    Build_OP ((cmp == TOP_c_neq_d)?TOP_c_eq_d:TOP_c_eq_s, 
		      fcc_tmp, src1, src2, ops);
	    Build_OP (false_br?TOP_bc1t:TOP_bc1f, fcc_tmp, targ, ops);
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
