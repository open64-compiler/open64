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
 * Module: betarget.cxx
 * $Revision: 1.2 $
 * $Date: 2005/09/15 03:55:38 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/com/MIPS/betarget.cxx,v $
 *
 * Description:
 *
 * Support routines for target-specific functionality.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "errors.h"
#include "util.h"
#include "tracing.h"
#include "topcode.h"
#include "wn.h"
#include "opcode.h"
#include "config_targ.h"
#include "targ_isa_lits.h"
#include "betarget.h"
#include "w2op.h"

BOOL Targ_Lower_Float_To_Unsigned = TRUE;
BOOL Targ_Lower_Unsigned_To_Float = TRUE;

// largest signed offset possible in small-frame stack model
INT Max_Small_Frame_Offset = 0x7fff;    // 15 bits


/* only return machine_ops, TOP_UNDEFINED if not an exact correspondence */
TOP
OPCODE_To_TOP (OPCODE opcode)
{
  OPERATOR opr   = OPCODE_operator (opcode);
  TYPE_ID  rtype = OPCODE_rtype (opcode);
  TYPE_ID  desc  = OPCODE_desc  (opcode);

  switch (opr) {

  case OPR_GOTO:
    return TOP_j;

#ifdef TARG_SL2 //fork_joint_region 
  case OPR_SL2_FORK_MAJOR:
    return TOP_c2_fork_m;
   case OPR_SL2_FORK_MINOR:
    return TOP_c2_fork_n;
#endif 
	
  case OPR_FORWARD_BARRIER:
    return TOP_fwd_bar;

  case OPR_BACKWARD_BARRIER:
    return TOP_bwd_bar;

  case OPR_INTRINSIC_CALL:
    if (rtype == MTYPE_V) return TOP_intrncall;
    else                  return TOP_UNDEFINED;

  case OPR_NEG:
    return TOP_UNDEFINED;

  case OPR_ABS:
    return TOP_UNDEFINED;

  case OPR_PAREN:
    if (rtype == MTYPE_F4) return TOP_noop;
    else if (rtype == MTYPE_F8) return TOP_noop;
    else                        return TOP_UNDEFINED;

  case OPR_PARM:
    return TOP_noop;

  case OPR_TRAP:
    return TOP_break;

  default:
    return TOP_UNDEFINED;
  }
}

/* pick the opcode corresponding to the TAS, which will either
 * be a float<->int move or a no-op. */
TOP
TAS_To_TOP (WN *tas_wn)
{
  TYPE_ID kid_mtype = WN_rtype(WN_kid0(tas_wn));

  switch (WN_opcode(tas_wn)) {
    case OPC_I4TAS:
    case OPC_U4TAS:
      return MTYPE_float(kid_mtype) ? TOP_mfc1 : TOP_noop;
    case OPC_I8TAS:
    case OPC_U8TAS:
      return MTYPE_float(kid_mtype) ? TOP_dmfc1 : TOP_noop;
    case OPC_F4TAS:
    case OPC_F8TAS:
      if (MTYPE_float(kid_mtype)) 
	return TOP_noop;
      else return MTYPE_is_size_double(kid_mtype) ? TOP_dmtc1 : TOP_mtc1;
    default:
      return TOP_UNDEFINED;
  }
}

/* return TRUE if the val is a power of 2 */
/* WARNING:  these routines must be kept in sync with cg's exp_divrem */
#define IS_POWER_OF_2(val)      ((val != 0) && ((val & (val-1)) == 0))

static BOOL Is_Power_Of_2(INT64 val, TYPE_ID mtype)
{
  if (MTYPE_is_signed(mtype) && val < 0)
    val=        -val;

  if (mtype == MTYPE_U4)
    val &= 0xffffffffull;

  return IS_POWER_OF_2(val);
}

static BOOL Is_Power_Of_2_Diff1(INT64 val, TYPE_ID mtype)
{
  	return (Is_Power_Of_2(val-1, mtype)||Is_Power_Of_2(val+1, mtype));
}

static BOOL Is_Power_Of_2_Diff2(INT64 val, TYPE_ID mtype)
{
	return (Is_Power_Of_2(val-2, mtype)||Is_Power_Of_2(val+2, mtype));
}

/* return whether MPY will be translated into shifts and adds */
/* NOTE:  this routine must stay in sync with cg's Expand_Multiply */
BOOL
Can_Do_Fast_Multiply (TYPE_ID mtype, INT64 val)
{
#if defined(TARG_SL)
  if(Is_Power_Of_2(val, mtype)||Is_Power_Of_2_Diff1(val, mtype)||Is_Power_Of_2_Diff2(val, mtype)){
  	 return TRUE;
  }
  return FALSE;
#else
  return TRUE;
#endif
}

/* return whether DIV will be translated into shifts */
BOOL
Can_Do_Fast_Divide (TYPE_ID mtype, INT64 dividend)
{
  return Is_Power_Of_2(dividend, mtype);
}

/* return whether REM or MOD will be translated into shifts */
BOOL
Can_Do_Fast_Remainder (TYPE_ID mtype, INT64 dividend)
{
  return Is_Power_Of_2(dividend, mtype);
}


/* ====================================================================
 *
 * Multiply_Limit
 * Divide_Limit
 *
 * When trying to convert a multiply or divide operation into a series
 * of shifts/adds/subtracts, there is some operation limit at which
 * the conversion is not profitable.  Return that limit.
 * The number of cycles should be the mult latency + mflo cycle.
 * TODO: Revisit these limits to check for I-cache effects, timing.
 *
 * ==================================================================== */
INT
Multiply_Limit (WN * tree)
{
  Is_True( WN_operator( tree ) == OPR_MPY, ("Expecting a multiply operator") );
  if (MTYPE_byte_size(WN_rtype(tree)) == 8) 
    return 4;
  else return 3;
}

INT
Divide_Limit (WN * tree)
{
  Is_True( WN_operator( tree ) == OPR_DIV, ("Expecting a divide operator") );
  if (MTYPE_byte_size(WN_rtype(tree)) == 8)
    return 68;
  else return 36;
}


INT
Copy_Quantum_Ratio(void)
{
  INT32  ratio;

  //  Lmt_DevWarn(1, ("Copy_Quantum_Ratio needs work"));
  // I don't think it still needs work
  switch(Target) {
  case TARGET_R10K:
  case TARGET_sb1:	ratio=	4; break;
  default:		ratio=	4; break;
  }

  return ratio;    
}


/* Does <val> fit in a signed word of length <bits>?
 */
inline BOOL Is_Signed_Bits(INT64 val, INT bits)
{
  INT64 hibit = 1LL << (bits - 1);
  return val >= -hibit && val <= (hibit - 1);
}


/* Does <val> fit in a unsigned word of length <bits>?
 */
inline BOOL Is_Unsigned_Bits(UINT64 val, INT bits)
{
  return val < (1ULL << bits);
}


/* Indicate if the specified operation can have an immediate operand.
 */
BOOL Can_Be_Immediate(OPERATOR opr,
		      INT64 val,
		      TYPE_ID dtype,
		      INT whichkid,
		      ST *stid_st)
{
  TOP top;
  
  switch (opr)
  {
  case OPR_AGOTO:	// leave a constant condition here alone
  case OPR_LOOP_INFO:	// leave the constant trip-count alone
    return TRUE;

  case OPR_EQ:
  case OPR_NE:
    return FALSE;

  case OPR_GE:
    return whichkid == 0 && Is_Signed_Bits(val+1, 16); // implemented as LT

  case OPR_GT:
    return whichkid == 0 && Is_Signed_Bits(val, 16); // implemented as LT

  case OPR_LE:
    return whichkid == 1 && Is_Signed_Bits(val+1, 16); // implemented as LT

  case OPR_LT:
    return whichkid == 1 && Is_Signed_Bits(val, 16);

  case OPR_MAX:		// treat as OPR_LT without regard to which kid
  case OPR_MIN:		// treat as OPR_LT without regard to which kid
    return Is_Signed_Bits(val, 16);
    
  case OPR_ASHR:
  case OPR_LSHR:
    return (whichkid == 1);

  case OPR_SHL:
    return (whichkid == 1);

  case OPR_MLOAD:
    return whichkid == 1;

  case OPR_MSTORE:
    return whichkid == 2;

  case OPR_BAND:
    if ((val > 0) && (val < 65536))
      return TRUE;
    break;
    
  case OPR_SUB:
    /* If the constant is the second operand, then we can add -val. */
    if (whichkid != 1)
      return FALSE;
    val = -val;
    // fall-through

  case OPR_ADD:
    return ISA_LC_Value_In_Class(val, LC_simm16);

  case OPR_DIV:
    // can the second kid be handled with shifts?
    return whichkid == 1 && Can_Do_Fast_Divide(dtype, val);

  case OPR_REM:
  case OPR_MOD:
    // can the second kid be handled with shifts?
    return whichkid == 1 && Can_Do_Fast_Remainder(dtype, val);

  case OPR_DIVREM:
    // can the second kid be handled with shifts?
    return    whichkid == 1 
	   && Can_Do_Fast_Remainder(dtype, val)
	   && Can_Do_Fast_Divide(dtype, val);

  case OPR_MPY:
    // can the value be handled with shifts?
    return Can_Do_Fast_Multiply(dtype, val);
    
  case OPR_SELECT:
    // The select will be expanded as a pair of predicated moves.
    // Therefore the constant we can handle is determined by 'mov',
    // i.e. can value fit in signed 16 bits?
    return whichkid > 0 && Is_Signed_Bits(val, 16);
    
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
  case OPR_PARM:
    /* calls end up storing their constant parameters to dedicated
       registers, which can be quicker if they're left in place as
       ldimm's. */
    return TRUE;
    
  case OPR_STID:
    /* is this in a store to a register, which usually means in
       preparation for a call, or return value, so just let us
       generate the stid/load-immediate in place if it fits. */
    return ST_class(stid_st) == CLASS_PREG;
  }

  return FALSE;
}
