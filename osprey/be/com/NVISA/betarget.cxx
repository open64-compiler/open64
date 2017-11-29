/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: betarget.cxx
 * $Revision: 1.22 $
 * $Date: 05/05/13 15:03:55-07:00 $
 * $Author: kannann@iridot.keyresearch $
 * $Source: /scratch/mee/Patch0002-taketwo/kpro64-pending/be/com/x8664/SCCS/s.betarget.cxx $
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
#include "config_wopt.h"

BOOL Targ_Lower_Float_To_Unsigned = FALSE;
BOOL Targ_Lower_Unsigned_To_Float = FALSE;

// largest signed offset possible in small-frame stack model
INT Max_Small_Frame_Offset = 0x7fffffff;  


/* only return machine_ops, TOP_UNDEFINED if not an exact correspondence */
TOP
OPCODE_To_TOP (OPCODE opcode)
{
  OPERATOR opr   = OPCODE_operator (opcode);
  TYPE_ID  rtype = OPCODE_rtype (opcode);
  TYPE_ID  desc  = OPCODE_desc  (opcode);

  switch (opr) {
  case OPR_PARM:
    return TOP_noop;	// so ignored
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
      return MTYPE_float(kid_mtype) ? TOP_mov_b64_f2i : TOP_nop;
    case OPC_I4TAS:
    case OPC_U4TAS:
      return MTYPE_float(kid_mtype) ? TOP_mov_b32_f2i : TOP_nop;
    case OPC_F8TAS:
      return MTYPE_float(kid_mtype) ? TOP_nop : TOP_mov_b64_i2f;
    case OPC_F4TAS:
      return MTYPE_float(kid_mtype) ? TOP_nop : TOP_mov_b32_i2f;
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

  return limit;
}

extern INT
Divide_Limit ( BOOL is_64bit)
{
  INT limit = 0;
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
#define MULTIPLICATION_LATENCY_32_BIT 2
#define MULTIPLICATION_LATENCY_64_BIT 3 

  INT num_ops = 4; // Initialize more than limit
  BOOL neg = FALSE;

  if (OPT_Space) return FALSE;  // These may eat up a bunch of instructions
  
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
  case TARGET_opteron:	ratio=	4; break;
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
  // All integers considered immediates on our acrhitecture family
  return TRUE;
}

#ifdef TARG_NVISA
/* Indicate if the specified operation can have an immediate operand.
 */
BOOL Can_Be_Float_Immediate(TYPE_ID mtype)
{
  if (MTYPE_bit_size(mtype) >= WOPT_Const_PRE_Float_Size)
    return FALSE;

  return TRUE;
}
#endif
