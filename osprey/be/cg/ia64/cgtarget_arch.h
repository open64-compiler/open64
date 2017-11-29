/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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


/* ====================================================================
 * ====================================================================
 *
 *  Target Specific Miscellany Declarations which include target
 *  dependencies.
 *
 *  THIS FILE IS ONLY TO BE INCLUDE BY ../cgtarget.h!!!!
 *
 * ====================================================================
 * ====================================================================
 */

#define INST_BYTES 16
#define DEFAULT_LONG_BRANCH_LIMIT (2000000 * INST_BYTES)

/* The BRP instruction has a minimum of 13-bit offset limit, which if signed
   means 2^12 == 4096 bytes / INST_BYTES == 256 */
#define DEFAULT_BRP_BRANCH_LIMIT (256 * INST_BYTES)
#define MIN_BRANCH_DISP (2097151 * INST_BYTES)

inline INT32 CGTARG_Branch_Taken_Penalty(void)
{
  return CGTARG_branch_taken_penalty_overridden ?
    CGTARG_branch_taken_penalty : 1;
}

inline INT32 CGTARG_Issue_Width(void)
{
  return Is_Target_Itanium() ? 6 : 1;
}

inline BOOL 
CGTARG_Use_Brlikely(float branch_taken_probability)
{
  return FALSE;
}

inline BOOL
CGTARG_Can_Predicate_Calls()
{
  return TRUE;
}


inline BOOL
CGTARG_Can_Predicate_Returns()
{
  return TRUE;
}

inline BOOL
CGTARG_Can_Predicate_Branches()
{
  return TRUE;
}

inline INT
CGTARG_Text_Alignment (void)
{
  return 32;
}

inline TOP
CGTARG_Noop_Top(void)
{
  return TOP_nop;	// return simulated nop
}

inline TOP
CGTARG_Noop_Top(ISA_EXEC_UNIT_PROPERTY unit)
{
  switch (unit) {
  case ISA_EXEC_PROPERTY_I_Unit: 
  case ISA_EXEC_PROPERTY_I2_Unit:
    return TOP_nop_i;
  case ISA_EXEC_PROPERTY_M_Unit: 
    return TOP_nop_m;
  case ISA_EXEC_PROPERTY_B_Unit: 
  case ISA_EXEC_PROPERTY_B2_Unit:
    return TOP_nop_b;
  case ISA_EXEC_PROPERTY_F_Unit: 
    return TOP_nop_f;
  default: 		 	 
    return TOP_nop;
  }
}

inline TOP
CGTARG_Simulated_Top(OP *op, ISA_EXEC_UNIT_PROPERTY unit)
{

  TOP top = OP_code(op);
 
  /* Prune the obvious cases. */
  if (!TOP_is_simulated(top)) return top;

  /* Placeholder for itemizing specific simulated ops */
  switch (top) {
  case TOP_mov_t_ar_r:
    switch (unit) {
    case ISA_EXEC_PROPERTY_I_Unit: 
    case ISA_EXEC_PROPERTY_I2_Unit:
      return TOP_mov_t_ar_r_i;
    case ISA_EXEC_PROPERTY_M_Unit: 
      return TOP_mov_t_ar_r_m;
    default: 
      FmtAssert(FALSE, ("unexpected ISA_EXEC_UNIT_PROPERTY type"));
    }
  case TOP_mov_t_ar_i:
    switch (unit) {
    case ISA_EXEC_PROPERTY_I_Unit: 
    case ISA_EXEC_PROPERTY_I2_Unit:
      return TOP_mov_t_ar_i_i;
    case ISA_EXEC_PROPERTY_M_Unit: 
      return TOP_mov_t_ar_i_m;
    default: 
      FmtAssert(FALSE, ("unexpected ISA_EXEC_UNIT_PROPERTY type"));
    }
  case TOP_mov_f_ar:
    switch (unit) {
    case ISA_EXEC_PROPERTY_I_Unit: 
    case ISA_EXEC_PROPERTY_I2_Unit:
      return TOP_mov_f_ar_i;
    case ISA_EXEC_PROPERTY_M_Unit: 
      return TOP_mov_f_ar_m;
    default: 
      FmtAssert(FALSE, ("unexpected ISA_EXEC_UNIT_PROPERTY type"));
    }
  case TOP_break:
    switch (unit) {
    case ISA_EXEC_PROPERTY_I_Unit: 
    case ISA_EXEC_PROPERTY_I2_Unit:
      return TOP_break_i;
    case ISA_EXEC_PROPERTY_M_Unit: 
      return TOP_break_m;
    case ISA_EXEC_PROPERTY_B_Unit: 
    case ISA_EXEC_PROPERTY_B2_Unit:
      return TOP_break_b;
    case ISA_EXEC_PROPERTY_F_Unit: 
      return TOP_break_f;
    default: 
      FmtAssert(FALSE, ("unexpected ISA_EXEC_UNIT_PROPERTY type"));
    }
  case TOP_chk_s:
    switch (unit) {
    case ISA_EXEC_PROPERTY_I_Unit: 
    case ISA_EXEC_PROPERTY_I2_Unit:
      return TOP_chk_s_i;
    case ISA_EXEC_PROPERTY_M_Unit: 
      return TOP_chk_s_m;
    default: 
      FmtAssert(FALSE, ("unexpected ISA_EXEC_UNIT_PROPERTY type"));
    }
  case TOP_cmp_lt_and:
  case TOP_cmp_lt_or:
  case TOP_cmp_lt_or_andcm:
  case TOP_cmp_le_and:
  case TOP_cmp_le_or:
  case TOP_cmp_le_or_andcm:
  case TOP_cmp_gt_and:
  case TOP_cmp_gt_or:
  case TOP_cmp_gt_or_andcm:
  case TOP_cmp_ge_and:
  case TOP_cmp_ge_or:
  case TOP_cmp_ge_or_andcm:
  case TOP_cmp_lt_orcm:
  case TOP_cmp_lt_andcm:
  case TOP_cmp_lt_and_orcm:
  case TOP_cmp_le_orcm:
  case TOP_cmp_le_andcm:
  case TOP_cmp_le_and_orcm:
  case TOP_cmp_gt_orcm:
  case TOP_cmp_gt_andcm:
  case TOP_cmp_gt_and_orcm:
  case TOP_cmp_ge_orcm:
  case TOP_cmp_ge_andcm:
  case TOP_cmp_ge_and_orcm:
  case TOP_cmp4_lt_and:
  case TOP_cmp4_lt_or:
  case TOP_cmp4_lt_or_andcm:
  case TOP_cmp4_le_and:
  case TOP_cmp4_le_or:
  case TOP_cmp4_le_or_andcm:
  case TOP_cmp4_gt_and:
  case TOP_cmp4_gt_or:
  case TOP_cmp4_gt_or_andcm:
  case TOP_cmp4_ge_and:
  case TOP_cmp4_ge_or:
  case TOP_cmp4_ge_or_andcm:
  case TOP_cmp4_lt_orcm:
  case TOP_cmp4_lt_andcm:
  case TOP_cmp4_lt_and_orcm:
  case TOP_cmp4_le_orcm:
  case TOP_cmp4_le_andcm:
  case TOP_cmp4_le_and_orcm:
  case TOP_cmp4_gt_orcm:
  case TOP_cmp4_gt_andcm:
  case TOP_cmp4_gt_and_orcm:
  case TOP_cmp4_ge_orcm:
  case TOP_cmp4_ge_andcm:
  case TOP_cmp4_ge_and_orcm:
    return top;

  case TOP_nop:
    return CGTARG_Noop_Top(unit);

  default:
    // TOP_intrncall, TOP_spadjust, TOP_copy_br, TOP_asm,
    return top;
  }
}

/*
 *     TN *CGTARG_Return_Enum_TN(OP *op, ISA_ENUM_CLASS class, INT *pos)
 *     if 'op' has an enum operand which matches the ISA_ENUM_CLASS 
 *     <class> type, return the enum TN and its operand position.
 */
inline TN* 
CGTARG_Return_Enum_TN(OP *op, ISA_ENUM_CLASS_VALUE enum_class, INT *pos)
{
  INT i;

  for (i = 0; i < OP_opnds(op); ++i) {
    TN *opnd_tn = OP_opnd(op, i);
    if (TN_is_enum(opnd_tn) && (enum_class == TN_enum(opnd_tn))) {
      *pos = i;
      return opnd_tn;
    }
  }

  return NULL;
}

inline BOOL 
CGTARG_Is_OP_Barrier(OP *op)
{
  return FALSE;
}

inline BOOL
CGTARG_Is_OP_Intrinsic(OP *op)
{
  return OP_code(op) == TOP_intrncall;
}

inline BOOL
CGTARG_Is_Bad_Shift_Op (OP *op)
{
  return FALSE;
}

inline BOOL
CGTARG_Is_Shift_Redundant (OP *op)
{
  BOOL predicated = OP_has_predicate(op) != 0;

  switch (OP_code(op)) {
  case TOP_shr:
  case TOP_shr_u:
  case TOP_shrp:
    return TN_register(OP_opnd(op,0+predicated)) == REGISTER_zero;
  }
  return FALSE;
}
    
inline BOOL 
CGTARG_Is_OP_Addr_Incr(OP *op)
{
  TOP top = OP_code(op);
  return (top == TOP_adds || top == TOP_addl || top == TOP_addp4_i) &&
	  TN_has_value(OP_opnd(op, 1)) &&
	  OP_result(op, 0) == OP_opnd(op, 2);
}

inline BOOL
CGTARG_Can_daddu_Be_Folded(OP *op1, OP *op2)
{
  return FALSE;
}

inline TOP
CGTARG_Copy_Op(UINT8 size, BOOL is_float)
{
  if (is_float) {
    return TOP_mov_f;
  } else {
    return TOP_mov;
  }
}

inline BOOL
CGTARG_Have_Indexed_Mem_Insts(void)
{
  return FALSE;
}

inline BOOL
Has_Single_FP_Condition_Code (void)
{
  return FALSE;
}

inline BOOL
CGTARG_Is_OP_daddu(OP *op)
{
  return FALSE;
}

inline BOOL
CGTARG_Can_Predicate()
{
  return TRUE;
}

extern TOP CGTARG_Get_unc_Variant(TOP top);
extern void Make_Branch_Conditional(BB *bb);

inline BOOL
CGTARG_Has_Branch_Predict(void)
{
	return TRUE;
}
