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


/* ====================================================================
 * ====================================================================
 *
 * Module: betarget.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/com/ia64/betarget.cxx,v $
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

BOOL Targ_Lower_Float_To_Unsigned = FALSE;
BOOL Targ_Lower_Unsigned_To_Float = FALSE;

// largest signed offset possible in small-frame stack model
INT Max_Small_Frame_Offset = 0x1fff;    // 13 bits


/* only return machine_ops, TOP_UNDEFINED if not an exact correspondence */
TOP
OPCODE_To_TOP (OPCODE opcode)
{
  OPERATOR opr   = OPCODE_operator (opcode);
  TYPE_ID  rtype = OPCODE_rtype (opcode);
  TYPE_ID  desc  = OPCODE_desc  (opcode);

  switch (opr) {

    case OPR_FORWARD_BARRIER:
      return TOP_fwd_bar;

    case OPR_BACKWARD_BARRIER:
      return TOP_bwd_bar;

    case OPR_INTRINSIC_CALL:
      if (rtype == MTYPE_V) return TOP_intrncall;
      else                  return TOP_UNDEFINED;

    case OPR_NEG:
      if (rtype == MTYPE_F4 || rtype == MTYPE_F8 || rtype == MTYPE_F10)
	return TOP_fneg;
      else
      	return TOP_UNDEFINED;

    case OPR_ABS:
      if (rtype == MTYPE_F4 || rtype == MTYPE_F8 || rtype == MTYPE_F10)
	return TOP_fabs;
      else
	return TOP_UNDEFINED;

    case OPR_PAREN:
      if (rtype == MTYPE_F4 || rtype == MTYPE_F8 || rtype == MTYPE_F10)
	return TOP_nop;
      else
	return TOP_UNDEFINED;

    case OPR_PARM:
      return TOP_nop;

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
    case OPC_I8TAS:
    case OPC_U8TAS:
      return MTYPE_float(kid_mtype) ? TOP_getf_d : TOP_nop;
    case OPC_I4TAS:
    case OPC_U4TAS:
      return MTYPE_float(kid_mtype) ? TOP_getf_s : TOP_nop;
    case OPC_F8TAS:
      return MTYPE_float(kid_mtype) ? TOP_nop : TOP_setf_d;
    case OPC_F4TAS:
      return MTYPE_float(kid_mtype) ? TOP_nop : TOP_setf_s;
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

/* return whether DIV will be translated into shifts */
extern BOOL
Can_Do_Fast_Divide (TYPE_ID mtype, INT64 dividend)
{
	if (Is_Power_Of_2(dividend, mtype)) {
		return TRUE;
	}
	return FALSE;
}

/* return whether REM or MOD will be translated into shifts */
extern BOOL
Can_Do_Fast_Remainder (TYPE_ID mtype, INT64 dividend)
{
	if (Is_Power_Of_2(dividend, mtype)) {
		return TRUE;
	}
	return FALSE;
}


/* ====================================================================
 *
 * Multiply_Limit
 * Divide_Limit
 *
 * When trying to convert a multiply or divide operation into a series
 * of shifts/adds/subtracts, there is some limit (cycles? ops?) at
 * which the conversion is not profitable.  Return that limit.
 * The number of cycles should be the mult latency + mflo cycle.
 * TODO: Revisit these limits to check for I-cache effects, timing.
 *
 * ====================================================================
 */
extern INT
Multiply_Limit( BOOL is_64bit, INT64 val)
{
  INT limit = 0;

  if (is_64bit)	{ /* 64-bit multiply */
    switch( Target ) {
    case TARGET_ITANIUM:	limit = 14; break;
    case TARGET_ITANIUM2:	limit = 14; break;
    default:		limit = 14; break;
    }
  } else { /* 32-bit multiply */
    switch( Target ) {
    case TARGET_ITANIUM:	limit = 14; break;
    case TARGET_ITANIUM2:	limit = 14; break;
    default:		limit = 14; break;
    }
  }
  return limit;
}

extern INT
Divide_Limit ( BOOL is_64bit)
{
  INT limit = 0;
  if (is_64bit)	{ /* 64-bit divide */
    switch( Target ) {
    case TARGET_ITANIUM:	limit = 50; break;
    case TARGET_ITANIUM2:	limit = 50; break;
    default:		limit = 50; break;
    }
  } else { /* 32-bit divide */
    switch( Target ) {
    case TARGET_ITANIUM:	limit = 50; break;
    case TARGET_ITANIUM2:	limit = 50; break;
    default:		limit = 50; break;
    }
  }
  return ( limit );
}

/* Count # instructions needed to do multiply with shifts and adds.
 * NOTE:  this routine must stay in sync with cg's Expand_Multiply_Into_Shifts.
 * See that routine for an explanation of the algorithm. */
static INT
Count_Multiply_Shifts (TARG_UINT constant)
{
  switch (constant) {
  case 0:
  case 1:
  case 2:
	return 1;
  default:
    if ((constant % 2) == 1) {		/* odd */
	if ((constant & 2) != 0)
		return 1 + Count_Multiply_Shifts (constant+1);
	else
		return 1 + Count_Multiply_Shifts (constant-1);
    }
    else {                  		/* even */
	while ((constant % 2) == 0) {	/* even */
		constant = (TARG_UINT)constant >> 1;
	}
	if (constant == 1)
		return 1;
	else
		return 1 + Count_Multiply_Shifts (constant);
    }
  }
}

/* return whether MPY will be translated into shifts and adds */
/* NOTE:  this routine must stay in sync with cg's Expand_Multiply */
extern BOOL
Can_Do_Fast_Multiply (TYPE_ID mtype, INT64 val)
{
  INT limit;
  INT num = 0;
  
  // Count the number of 1 bits
  INT num_ones=0;
  INT num_ones_neg=0;
  
  if (val < 0) val = -val;

  UINT64 uc=val;
  while (uc) {num_ones += (uc&1); uc >>= 1;}
  
  uc = val;
  if (uc <= 63) return (TRUE);

  if (num_ones == 1) return (TRUE); // Anything with this few ones is fair game
  while ((uc & 1) == 0) {uc >>= 1;}
  // Another special case, 2**N - 1
  if (uc == ((1<<num_ones)-1)) return (TRUE);
  //
  // The remaining piece we need to see how to multiply. 
  // 
  // Using shladd we can always multiply no worse than:
  //
  // <=6 bits - 4/3  (4 instructions, dependence height of 3)
  // 7,8      - 7/3  (do as 2 groups of 4 combine with shladd)
  // 9,10     - 8/4  (do as group of 6 and 4 combine with shladd)
  // 11-16    - 16/5 (do as two groups of 8, combine with pair of shladds)
  //
  if (OPT_Space) return FALSE;  // These may eat up a bunch of instructions
  if (uc <= 65535) return TRUE;
  return FALSE;
}


/*
 */
INT Copy_Quantum_Ratio(void)
{
  INT32  ratio;

  //  Lmt_DevWarn(1, ("Copy_Quantum_Ratio needs work"));
  // I don't think it still needs work
  switch(Target) {
  case TARGET_ITANIUM:	ratio=	4; break;
  case TARGET_ITANIUM2:	ratio=	4; break;
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
  // for IA64 targets, the value 0 is a dedicated register
  if (val == 0) return TRUE;

  switch (opr) {
  case OPR_AGOTO:	// leave a constant condition here alone
  case OPR_LOOP_INFO:	// leave the constant trip-count alone
    return TRUE;
    
  case OPR_EQ:
  case OPR_NE:
  case OPR_GE:
  case OPR_GT:
  case OPR_LE:
  case OPR_LT:
  case OPR_MAX:		// treat as OPR_LT without regard to which kid
  case OPR_MIN:		// treat as OPR_LT without regard to which kid
    // the range of immediates for ia64 is complicated, but basically
    // an 8-bit literal is sign-extended and then its value is
    // taken in the context of the data size and type.
    if (!MTYPE_is_size_double(dtype)) val = (INT32)val;

    switch (opr) {
    case OPR_LE:
    case OPR_GT:
      whichkid = !whichkid;
      /*FALLTHROUGH*/
    case OPR_LT:
    case OPR_GE:
      if (whichkid == 1) val = val - 1;
      break;
    }

    return Is_Signed_Bits(val, 8);

  case OPR_ASHR:
  case OPR_LSHR:
  case OPR_MLOAD:
  case OPR_SHL:
    // can handle any constant as long as it's the second kid
    return whichkid == 1;

  case OPR_MSTORE:
    // can handle any constant as long as it's the third kid
    return whichkid == 2;

  case OPR_BAND:
  case OPR_BIOR:
  case OPR_BXOR:
    // can the value fit in signed 8 bits?
    return Is_Signed_Bits(val, 8);

  case OPR_SUB:
    // If the constant is kid #0 (left operand) then we can use
    // the subtract instruction which takes an immediate 8-bit first/left
    // operand. If the constant is kid #1 (right operand) then
    // we can negate the constant and use an add.
    if (whichkid == 0) return Is_Signed_Bits(val, 8);
    return Is_Signed_Bits(-val, 14);

  case OPR_ADD:
    // can the value fit in signed 14 bits?
    return Is_Signed_Bits(val, 14);

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
    // i.e. can value fit in signed 22 bits?
    return whichkid > 0 && Is_Signed_Bits(val, 22);

  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
  case OPR_PARM:
    // calls end up storing their constant parameters to dedicated
    // registers, which can be quicker if they're left in place as
    // ldimm's.
    return TRUE;
    
  case OPR_STID:
    // is this in a store to a register, which usually
    // means in preparation for a call, or return value, so just 
    // let us generate the stid/load-immediate in place if it fits
    return ST_class(stid_st) == CLASS_PREG;
  }

  return FALSE;
}
