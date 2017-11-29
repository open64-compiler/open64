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

#define INST_BYTES 4
#define DEFAULT_LONG_BRANCH_LIMIT (0x1ffffff * INST_BYTES)
#define DEFAULT_BRP_BRANCH_LIMIT DEFAULT_LONG_BRANCH_LIMIT
#define MIN_BRANCH_DISP (2097151 * INST_BYTES)


inline INT32 CGTARG_Branch_Taken_Penalty(void)
{
  return CGTARG_branch_taken_penalty_overridden ?
    CGTARG_branch_taken_penalty : 1;
}

inline INT32 CGTARG_Issue_Width(void)
{
  return 4;
}

inline BOOL 
CGTARG_Use_Brlikely(float branch_taken_probability)
{
  return FALSE;
}

inline BOOL
CGTARG_Can_Predicate_Calls()
{
  return FALSE;
}


inline BOOL
CGTARG_Can_Predicate_Returns()
{
  return FALSE;
}

inline BOOL
CGTARG_Can_Predicate_Branches()
{
  return FALSE;
}

inline INT
CGTARG_Text_Alignment (void)
{
#ifdef TARG_SL
  extern BOOL CG_check_quadword;
  if (CG_check_quadword)
    return 16;
  else
#endif  	
    return 4;	
}

inline TOP
CGTARG_Noop_Top(void)
{
  return TOP_nop;	// return simulated nop
}

inline TOP
CGTARG_Noop_Top(ISA_EXEC_UNIT_PROPERTY unit)
{
  return CGTARG_Noop_Top();
}

inline TOP
CGTARG_Simulated_Top(OP *op, ISA_EXEC_UNIT_PROPERTY unit)
{

  TOP top = OP_code(op);
 
  /* Prune the obvious cases. */
  if (!TOP_is_simulated(top)) return top;

  /* Placeholder for itemizing specific simulated ops */
  return top;
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
  if (OP_code(op) == TOP_asm) {
    /* HD - So far, we don't complete the OP_Asm_Map support,
            so I comment all the below statements.
    extern OP_MAP OP_Asm_Map;
    ASM_OP_ANNOT* asm_info = (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op);
    return (WN_Asm_Clobbers_Mem(ASM_OP_wn(asm_info)));
    */
    return TRUE;
  } 
#ifdef TARG_SL2
  else if ( OP_code(op) == TOP_c2_joint  || 
            OP_code(op) == TOP_c2_thctrl_lock ||
            OP_code(op) == TOP_c2_thctrl_unlock ||
            OP_code(op) == TOP_c2_thctrl_deact ||
            OP_code(op) == TOP_c2_thctrl_act ||
            OP_code(op) == TOP_c2_thctrl_mode4 ||
            OP_code(op) == TOP_c2_thctrl_mode5 ||
            OP_code(op) == TOP_c2_thctrl_mode6 ||
            OP_code(op) == TOP_c2_thctrl_mode7 ||
            OP_code(op) == TOP_c2_fork_m ||
            OP_code(op) == TOP_c2_fork_n ||
            OP_code(op) == TOP_peripheral_rw_begin || 
            OP_code(op) == TOP_peripheral_rw_end){ 
    return TRUE;
  } 
#endif
  else
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
  // Check unneeded since there is no zero register.
  return FALSE;
}
    
inline BOOL 
CGTARG_Is_OP_Addr_Incr(OP *op)
{
  TOP top = OP_code(op);
  return((top == TOP_addi || top == TOP_addiu || 
	  top == TOP_daddi || top == TOP_daddiu) &&
	 TN_has_value(OP_opnd(op, 1)) &&
	 (OP_result(op, 0) == OP_opnd(op, 0)));
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
    return size == 4 ? TOP_mov_s : TOP_mov_d;
  }
  return size == 4 ? TOP_add : TOP_dadd;
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
  TOP top = OP_code(op);
  return top == TOP_daddu;
}

inline BOOL
CGTARG_Can_Predicate()
{
  return FALSE;
}

extern TOP CGTARG_Get_unc_Variant(TOP top);
extern void Make_Branch_Conditional(BB *bb);

inline BOOL
CGTARG_Has_Branch_Predict(void)
{
  return TRUE;
}
