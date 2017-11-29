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
 * Module: expand.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/expand.cxx,v $
 *
 * Description:
 *
 * This file contains the internals of code expansion. Its interface
 * is 'Exp_OP', which takes an OP, expands it into a list of OPs which
 * are appended to the oplist passed in.
 *
 * It handles all the macro expansions, special handling during 
 * expansion and all the nitty gritty stuff that goes along with it.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "config.h"
#include "erglob.h"
#include "ercg.h"
#include "glob.h"
#include "tracing.h"
#include "util.h"

#include "topcode.h"
#include "tn.h"
#include "cg_flags.h"
#include "targ_isa_lits.h"
#include "bb.h"
#include "symtab.h"
#include "opcode.h"
#include "const.h"	/* needed to manipulate target/host consts */
#include "targ_const.h"	/* needed to manipulate target/host consts */
#include "op.h"
#include "data_layout.h"
#include "stblock.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "w2op.h"
#include "label_util.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "cg_spill.h"

BOOL Reuse_Temp_TNs = FALSE;

BOOL Trace_Exp2 = FALSE;      /* extra cgexp trace*/

/* Disable conversion of constant integer multiplies into shift/adds:*/
static BOOL Disable_Const_Mult_Opt = FALSE;

/* Dup_TN won't dup a dedicated tn, but for our purposes we
 * can just re-use the dedicated tn.  Don't want to re-use a
 * symbolic tn or it will mess up live ranges. */
/* DOESN'T WORK:  causes problems in Create_lvs because it causes
 * a use of a parm reg at the call-site, so it looks like the
 * parm-reg is incoming at the call?  This probably should work,
 * but for now we can use other routine that create a real dup tn. */
#define DUP_TN(tn)	Dup_TN_Even_If_Dedicated(tn)

static TOP
Pick_Imm_Form_TOP (TOP regform)
{
	switch (regform) {
	case TOP_shl:	return TOP_shl_i;
	case TOP_shr:	return TOP_shr_i;
	case TOP_shr_u:	return TOP_shr_i_u;
	case TOP_and:	return TOP_and_i;
	case TOP_or:	return TOP_or_i;
	case TOP_xor:	return TOP_xor_i;
	default:	return regform;
	}
}




void
Expand_Copy (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  TOP mov_opc = MTYPE_is_float(mtype) ? TOP_mov_f : TOP_mov;
  Build_OP (mov_opc, result, True_TN, src, ops);
  Set_OP_copy (OPS_last(ops));
}

//
//  Helper routine to do proper sign extension
//
static void
Fixup_32_Bit_Op(TN *result,TN *src, TYPE_ID dest_type, OPS *ops)
{
  if (dest_type == MTYPE_I8 || dest_type == MTYPE_U8) {
    Expand_Copy(result,src,dest_type,ops);
  } else {
    Expand_Convert_Length (result, src, Gen_Literal_TN(MTYPE_size_reg(dest_type), 4),
			   dest_type, MTYPE_is_signed(dest_type),ops);
  }
}


/* ====================================================================
 *
 * Expand_Convert_Length
 *
 * Generate code to expand an xCVTL operator.  The code generated is a
 * left shift to put the upper bit to be kept in the high bit of the
 * word or double-word, followed by a right shift back to either sign-
 * or zero-extend it as requested.
 *
 * ====================================================================
 */

void
Expand_Convert_Length ( TN *dest, TN *src, TN *length_tn, TYPE_ID mtype, BOOL signed_extension, OPS *ops)
{
  INT16 new_length;	/* Length to convert to */
  TOP opc = TOP_UNDEFINED;

  /* Do we sign-extend, or zero-extend?
   * What length do we convert to?  It's the maximum of the target
   * register size, and the operator's result size:
   */
  new_length = TN_value (length_tn);
  if (new_length == 8) opc = (signed_extension ? TOP_sxt1 : TOP_zxt1);
  else if (new_length == 16) opc = (signed_extension ? TOP_sxt2 : TOP_zxt2);
  else if (new_length == 32) opc = (signed_extension ? TOP_sxt4 : TOP_zxt4);
  if (opc != TOP_UNDEFINED) {
      Build_OP ( opc, dest, True_TN, src, ops);
      return;
  }
  // non-byte-sized: use extr and extr.u
  opc = signed_extension ? TOP_extr : TOP_extr_u;
  Build_OP ( opc, dest, True_TN, src, Gen_Literal_TN(0, 4),
	     Gen_Literal_TN(new_length, 4), ops );
}

void
Exp_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
	INT64 val;
  	if ( TN_has_value(src) ) {
		val = TN_value(src);
	}
  	else if ( TN_is_symbol(src) ) {
		ST *base;
		Base_Symbol_And_Offset_For_Addressing (TN_var(src), TN_offset(src), &base, &val);
	}
	if (ISA_LC_Value_In_Class (val, LC_i22)) {
		Build_OP (TOP_mov_i, dest, True_TN, src, ops);
	}
	else {
		Build_OP (TOP_movl, dest, True_TN, src, ops);
	}
}

/* 
 * Expand Immediate value.
 */
void
Expand_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
  FmtAssert((TN_is_constant(src)),
	    ("unexpected non-constant in Expand_Immediate"));
  FmtAssert((TN_has_value(src) || TN_is_symbol(src)), 
	    ("expected value or const in Expand_Immediate"));
  Exp_Immediate (dest, src, TRUE, ops);
}

TN*
Expand_Immediate_Into_Register (TN *src, OPS *ops)
{
	/* load into reg and do reg case */
  	TN *tmp = Build_TN_Of_Mtype (MTYPE_I8);
	Expand_Immediate (tmp, src, TRUE, ops);
	return tmp;
}

void
Expand_Add (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TOP new_opcode;
  INT64 val;
  if (TN_is_constant(src1)) {
	if (TN_has_value(src1)) {
		val = TN_value(src1);
		if (val == 0) {
			Expand_Copy (result, src2, mtype, ops);
			return;
		}
		if (ISA_LC_Value_In_Class ( val, LC_i14)) {
  			new_opcode = TOP_adds;
		} else {
			src1 = Expand_Immediate_Into_Register (src1, ops);
  			new_opcode = TOP_add;
		}
	} else if ( TN_is_symbol(src1) ) {
		/* symbolic constant, gp-relative or sp-relative */
		if (TN_is_gp_reg(src2))
  			new_opcode = TOP_addl;
		else {
			ST *base;
			INT64 ofst;
			Base_Symbol_And_Offset_For_Addressing (TN_var(src1), TN_offset(src1), &base, &ofst);
			if (ISA_LC_Value_In_Class ( ofst, LC_i14)) {
  				new_opcode = TOP_adds;
			} else {
				src1 = Expand_Immediate_Into_Register (src1, ops);
  				new_opcode = TOP_add;
			}
		}
	} else {
      		FmtAssert(FALSE,("unexpected constant in Expand_Add"));
	}
  	Build_OP (new_opcode, result, True_TN, src1, src2, ops);
  }
  else if (TN_is_constant(src2)) {
  	// switch order of src so immediate is first
	Expand_Add (result, src2, src1, mtype, ops);
  } 
  else {
  	Build_OP (TOP_add, result, True_TN, src1, src2, ops);
  }
}

void
Expand_Sub (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  INT64 val;
  TOP new_opcode = TOP_sub;
  if (TN_is_constant(src1)) {
	if (TN_has_value(src1)) {
		val = TN_value(src1);
		if (ISA_LC_Value_In_Class ( val, LC_i8)) {
			new_opcode = TOP_sub_i;
		} else {
			src1 = Expand_Immediate_Into_Register (src1, ops);
		}
	} else if ( TN_is_symbol(src1) ) {
		src1 = Expand_Immediate_Into_Register (src1, ops);
	} else {
      		FmtAssert(FALSE,("unexpected constant in Expand_Sub"));
	}
  }
  else if (TN_is_constant(src2)) {
	if (TN_has_value(src2)) {
		val = TN_value(src2);
		if (ISA_LC_Value_In_Class ( val, LC_i14)) {
                        /* return the negative of the value */
                        val = -val;
			src2 = Gen_Literal_TN (val, 4);
			Expand_Add (result, src1, src2, mtype, ops);
			return;
		} else {
			src2 = Expand_Immediate_Into_Register (src2, ops);
		}
	} else if ( TN_is_symbol(src2) ) {
		/* symbolic constant; return negative of value */
		src2 = Gen_Symbol_TN ( TN_var(src2), TN_offset(src2), 
			TN_RELOC_NEG );
		Expand_Add (result, src1, src2, mtype, ops);
		return;
	}
  }
  Build_OP (new_opcode, result, True_TN, src1, src2, ops);
}


void
Expand_Neg (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  /* neg op0 -> subu $0, op0 */
  Build_OP (TOP_sub, result, True_TN, Zero_TN, src, ops);
}


void
Expand_Abs (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  Expand_Copy (dest, src, mtype, ops);
  Build_OP (TOP_cmp_lt, p1, p2, True_TN, src, Zero_TN, ops);
  Build_OP (TOP_sub, dest, p1, Zero_TN, src, ops);
}

void
Expand_Shift (TN *result, TN *src1, TN *src2, TYPE_ID mtype, SHIFT_DIRECTION kind, OPS *ops)
{
  TOP new_opcode;
  UINT wordsize = MTYPE_is_size_double(mtype) ? 64 : 32;

  switch (kind) {
  case shift_left:
    new_opcode = TOP_shl;
    break;
  case shift_aright:
    new_opcode = TOP_shr;
    break;
  case shift_lright:
    new_opcode = TOP_shr_u;
    break;
  }
  if (TN_has_value(src2)) {
    UINT64 val = TN_value(src2);

    // On IA-64, shifts with a shift count >= wordsize, are equivalent
    // to performing a shift by the wordsize. This is different than
    // mips where only the low log2(wordsize) bits of the shift count
    // were used. For constant shifts, the immediate field is not
    // large enough for such counts, so simulate the effect by
    // replacing the result with 0, or the sign bit of the source
    // for shift_aright.
    if (val >= wordsize) {
      if (kind == shift_aright) {
	Build_OP (TOP_extr, result, True_TN, src1,
		  Gen_Literal_TN (wordsize - 1, 4),
		  Gen_Literal_TN (1, 4), ops);
      } else {
	Build_OP (TOP_mov_i, result, True_TN, Gen_Literal_TN (0, 4), ops);
      }
      return;
    }
    
    if (kind == shift_left) {
      if (wordsize == 64) {
	if (val >= 1 && val <= 4) {
	  Build_OP (TOP_shladd,result,True_TN,src1,Gen_Literal_TN (val, 4),Zero_TN,ops);
	} else if (val > 4 && val <= 8 && !OPT_Space) {
	  Build_OP(TOP_shl_i,result,True_TN,src1,src2,ops);
	} else {
	  // We can use a dep.z for this case
	  Build_OP (TOP_shl_i, result, True_TN, src1, src2, ops);
	}
      } else {
	// Wordsize < 64, we can do this with a dep.z for the unsigned result, 
	// or a dep.z followed by a sign extension for the signed case.
	if (MTYPE_is_signed(mtype)) {
	  TN *r1 = DUP_TN(result);
	  Expand_Shift(r1, src1, src2, MTYPE_I8, kind, ops);
	  Fixup_32_Bit_Op(result,r1,mtype,ops);
	} else {
	  Build_OP (TOP_dep_z, result, True_TN, src1, src2, Gen_Literal_TN (wordsize-val, 4), ops);
	}
      }
      return;
    }

    // Right-shifts
    // The immediate shift instructions perform a 64-bit shift.
    // If we use one of them, we might need to do a sign/zero-extend
    // later. So instead we use extr/extr.u to properly extend the result.
    if (wordsize != 64) {
      new_opcode = (kind == shift_aright) ? TOP_extr : TOP_extr_u;
      // 32-bit right shift needs len to be 32-count not 64-count
      Build_OP (new_opcode, result, True_TN, src1, src2, 
		Gen_Literal_TN (32-val, 4), ops);
    } else {
      new_opcode = Pick_Imm_Form_TOP (new_opcode);
      Build_OP (new_opcode, result, True_TN, src1, src2, ops);
    }
    return;
  }

  // The shift is a variable shift

  if (wordsize != 64) {
    TN *r1 = DUP_TN(result);
    if (kind == shift_left) {
      Build_OP (new_opcode, r1, True_TN, src1, src2, ops);
      Fixup_32_Bit_Op(result,r1,mtype,ops);
    } else {
      // Right shifts, we convert the source first
      if (kind == shift_aright) {
	Fixup_32_Bit_Op(r1,src1,MTYPE_I4,ops);
      } else {
	Fixup_32_Bit_Op(r1,src1,MTYPE_U4,ops);
      }
      Build_OP (new_opcode, result, True_TN, r1, src2, ops);
    }
  } else {
    Build_OP (new_opcode, result, True_TN, src1, src2, ops);
  }
}

inline void
Expand_G_To_F (TN *ftn, TN *gtn, OPS *ops)
{
  Build_OP (TOP_setf_sig, ftn, True_TN, gtn, ops);
}

inline void
Expand_F_To_G (TN *gtn, TN *ftn, OPS *ops)
{
  Build_OP (TOP_getf_sig, gtn, True_TN, ftn, ops);
}



/*
 *
 * Helper routine for Expand_Small_Multiply
 *
 */
static void shladd(TN *r, TN *x1, INT s, TN *x2, OPS *ops)
{
  Build_OP (TOP_shladd,r,True_TN,x1,Gen_Literal_TN (s, 4),x2,ops);
}

static void shl(TN *r, TN *x1, INT s, OPS *ops)
{
  Build_OP (TOP_shl_i,r,True_TN,x1,Gen_Literal_TN (s, 4),ops);
}


/*
 * Expand_Small_Multiply produces an optimized expansion of 
 * multiplication by any constant between -1 and 63. Multiplication is done for 64
 * bit quantities only. 
 *
 */
static void
Expand_Small_Multiply(TN *r,  // result
		      TN *x,  // source
		      INT16 val, // multiplicand
		      OPS * ops) // place to put the ops
{
  TN *r1;
  TN *r2; // Temps
  TN *Z=Zero_TN; // Makes it a little easier to write

#define ONE_TEMP r1=Build_TN_Of_Mtype(MTYPE_I8)
#define TWO_TEMPS ONE_TEMP; r2=Build_TN_Of_Mtype(MTYPE_I8)

  // Although ugly, a big case statement is I think the best way to express this
  switch (val) {
   case -1:
     Expand_Neg(r,x,MTYPE_I8,ops);
     break;
   case 0:
     Expand_Copy (r, Z, MTYPE_I8, ops);
     break;
   case  1 :
     Expand_Copy (r, x, MTYPE_I8, ops);
     break;
   case  2 :
     shladd(r,x,1,Z,ops);
     break;
   case  3 :
     shladd(r,x,1,x,ops);
     break;
   case  4 :
     shladd(r,x,2,Z,ops);
     break;
   case  5 :
     shladd(r,x,2,x,ops);
     break;
   case  6:
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r,r1,2,ops);
     break;
   case  7 :
     ONE_TEMP;
     shladd(r1,x,1,x,ops);
     shladd(r,r1,1,x,ops);
     break;
   case  8 :
     shladd(r,x,3,Z,ops);
     break;
   case  9 :
     shladd(r,x,3,x,ops);
     break;
   case  10 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r,r1,2,ops);
     break;
   case  11 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,9,ops);
     shladd(r,x,1,r1,ops);
     break;
   case  12 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r,r1,4,ops);
     break;
   case  13 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,5,ops);
     shladd(r,x,3,r1,ops);
     break;
   case  14 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r2,x,9,ops);
     Expand_Add(r,r1,r2,MTYPE_I8,ops);
     break;
   case  15 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r,r1,5,ops);
     break;
   case  16 :
     shladd(r,x,4,Z,ops);
     break;
   case  17 :
     shladd(r,x,4,x,ops);
     break;
   case  18 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,9,ops);
     Expand_Small_Multiply(r,r1,2,ops);
     break;
   case  19 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,9,ops);
     shladd(r,r1,1,x,ops);
     break;
   case  20 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r,r1,4,ops);
     break;
   case  21 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,5,ops);
     shladd(r,x,4,r1,ops);
     break;
   case  22 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,17,ops);
     Expand_Small_Multiply(r2,x,5,ops);
     Expand_Add(r,r1,r2,MTYPE_I8,ops);
     break;
   case  23 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,17,ops);
     Expand_Small_Multiply(r2,x,3,ops);
     shladd(r,r2,1,r1,ops);
     break;
   case  24 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r,r1,8,ops);
     break;
   case  25 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r,r1,5,ops);
     break;
   case  26 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,17,ops);
     Expand_Small_Multiply(r2,x,9,ops);
     Expand_Add(r,r1,r2,MTYPE_I8,ops);
     break;
   case  27 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,9,ops);
     Expand_Small_Multiply(r,r1,3,ops);
     break;
   case  28 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r2,x,4,ops);
     shladd(r,r1,3,r2,ops);  // 8*3+4
     break;
   case  29 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r2,x,5,ops);
     shladd(r,r1,3,r2,ops);  // 8*3+5
     break;
   case  30 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,15,ops);
     Expand_Small_Multiply(r,r1,2,ops);
     break;
   case  31 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,16,ops);
     Expand_Small_Multiply(r2,x,-1,ops);
     shladd(r,r1,1,r2,ops);
     break;
   case  32 :
	 shl(r,x,5,ops);
     break;
   case  33 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,16,ops);
     shladd(r,r1,1,x,ops);
     break;
   case  34 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,17,ops);
     Expand_Small_Multiply(r,r1,2,ops);
     break;
   case  35 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,17,ops);
     shladd(r,r1,1,x,ops);
     break;
   case  36 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,9,ops);
     Expand_Small_Multiply(r,r1,4,ops);
     break;
   case  37 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,9,ops);
     shladd(r,r1,2,x,ops);
     break;
   case  38 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,17,ops);
     Expand_Small_Multiply(r2,x,4,ops);
     shladd(r,r1,1,r2,ops);
     break;
   case  39 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,17,ops);
     Expand_Small_Multiply(r2,x,5,ops);
     shladd(r,r1,1,r2,ops);
     break;
   case  40 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r,r1,8,ops);
     break;
   case  41 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,5,ops);
     shladd(r,r1,3,x,ops);
     break;
   case  42 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r2,x,2,ops);
     shladd(r,r1,3,r2,ops);
     break;
   case  43 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r2,x,3,ops);
     shladd(r,r1,3,r2,ops);
     break;
   case  44 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r2,x,4,ops);
     shladd(r,r1,3,r2,ops);
     break;
   case  45 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,5,ops);
     Expand_Small_Multiply(r,r1,9,ops);
     break;
   case  46 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,45,ops);
     Expand_Add(r,r1,x,MTYPE_I8,ops);
     break;
   case  47 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,-1,ops);
     Expand_Small_Multiply(r2,x,3,ops);
     shladd(r,r2,4,r1,ops);
     break;
   case  48 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r,r1,16,ops);
     break;
   case  49 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,3,ops);
     shladd(r,r1,4,x,ops);
     break;
   case  50 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r2,x,2,ops);
     shladd(r,r1,4,r2,ops);
     break;
   case  51 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r,r1,17,ops);
     break;
   case  52 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r2,x,4,ops);
     shladd(r,r1,4,r2,ops);
     break;
   case  53 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r2,x,5,ops);
     shladd(r,r1,4,r2,ops);
     break;
   case  54 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,9,ops);
     Expand_Small_Multiply(r,r1,6,ops);
     break;
   case  55 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,11,ops);
     Expand_Small_Multiply(r,r1,5,ops);
     break;
   case  56 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r2,x,8,ops);
     shladd(r,r1,4,r2,ops);
     break;
   case  57 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,3,ops);
     Expand_Small_Multiply(r2,x,9,ops);
     shladd(r,r1,4,r2,ops);
     break;
   case  58 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,29,ops);
     Expand_Small_Multiply(r,r1,2,ops);
     break;
   case  59 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,29,ops);
     shladd(r,r1,1,x,ops);
     break;
   case  60 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,12,ops);
     Expand_Small_Multiply(r,r1,5,ops);
     break;
   case  61 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,45,ops);
     shladd(r,x,4,r1,ops);
     break;
   case  62 :
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,31,ops);
     Expand_Small_Multiply(r,r1,2,ops);
     break;
   case  63 :
     TWO_TEMPS;
     Expand_Small_Multiply(r1,x,-1,ops);
     Expand_Small_Multiply(r2,x,16,ops);
     shladd(r,r2,2,r1,ops);
     break;
   default:
     #pragma mips_frequency_hint NEVER
     FmtAssert(0,("Can't small multiply by %d",val));
     /*NOTREACHED*/
  }
}



/* 
 * Expand the multiply into a series of shifts and adds,
 * unless the sequence is longer than "limit".
 */
static BOOL
Expand_Multiply_Into_Shifts (
  TN	   *result_tn,
  TN	   *var_tn,
  TARG_UINT constant,
  INT16	    limit,
  TYPE_ID   mtype,
  OPS 	*ops)
{
  TN *tmp_tn;
  switch (constant) {
  case 0:
    if ( limit < 1 ) return FALSE;
    Expand_Copy (result_tn, Zero_TN, mtype, ops);
    return TRUE;
  case 1:
    if ( limit < 1 ) return FALSE;
    Expand_Copy (result_tn, var_tn, mtype, ops);
    return TRUE;
  case 2:
    if ( limit < 1 ) return FALSE;
    Expand_Add (result_tn, var_tn, var_tn, mtype, ops);
    return TRUE;
  default:
    if ((constant % 2) == 1) {		/* odd */
        tmp_tn = DUP_TN(result_tn);
	if ((constant & 2) != 0) {
		if ( limit < 2 ) return FALSE;
		if (!Expand_Multiply_Into_Shifts (
			tmp_tn, var_tn, 
			constant+1, limit-1,
			mtype, ops)) 
			return FALSE;
		Expand_Sub (result_tn, tmp_tn, var_tn, mtype, ops);
		return TRUE;
	}
	else {
		if ( limit < 2 ) return FALSE;
		if (!Expand_Multiply_Into_Shifts (
			tmp_tn, var_tn, 
			constant-1, limit-1,
			mtype, ops)) 
			return FALSE;
		Expand_Add (result_tn, tmp_tn, var_tn, mtype, ops);
		return TRUE;
	}
    }
    else {                  		/* even */
	INT shift_cnt = 0;
	while ((constant % 2) == 0) {	/* even */
		shift_cnt++;
		constant = (TARG_UINT)constant >> 1;
	} /*while*/
	if (constant == 1) {
		if ( limit < 1 ) return FALSE;
		Expand_Shift(result_tn, var_tn, Gen_Literal_TN(shift_cnt, 4), mtype, shift_left, ops);
		return TRUE;
	}
	else {
		if ( limit < 2 ) return FALSE;
        	tmp_tn = DUP_TN(result_tn);
		if (!Expand_Multiply_Into_Shifts (
			tmp_tn, var_tn, 
			constant, limit-1,
			mtype, ops)) 
			return FALSE;
		Expand_Shift(result_tn, tmp_tn, Gen_Literal_TN(shift_cnt, 4), mtype, shift_left, ops);
		return TRUE;
	}
    }
  }
}

/*
 *  Try to expand a multiply into a sequence of less expensive operations.
 */
#define NUM_FAST_MPYS 8
static INT fast_mpys[NUM_FAST_MPYS] = {17,16,9,8,5,4,3,2};

static BOOL
Expand_Constant_Multiply (TN *result, TN *var_tn, TARG_INT constant, TYPE_ID mtype, OPS *ops)
{
  BOOL did_do_fast;
  INT16 limit;	/* maximum number of operations to replace the multiply */
  TN *x = var_tn;
  INT64 c = constant; // I don't want to depend on TARG_INT
  BOOL needs_sign_extension;

  // fast special cases
  if (c == 0) {
    Expand_Copy (result, Zero_TN, MTYPE_I8, ops);
    return TRUE;
  } else if (c == 1) {
    Expand_Copy (result, var_tn, MTYPE_I8, ops);
    return TRUE;
  } else if (c == -1) {
    Expand_Neg(result, var_tn, mtype, ops);
    return TRUE;
  }
    
  needs_sign_extension = MTYPE_size_reg(mtype) != 64;

  if (c < 0) {
    c = -c;
    x = DUP_TN(var_tn);
    Expand_Neg(x, var_tn, mtype, ops);
  }    


  // Count the number of 1's in c and -c
  INT num_ones=0;
  UINT64 uc=c;
  while (uc) {num_ones += (uc&1); uc >>= 1;}
  uc = c;
  
  //
  // Small constants always make sense to use the optimized sequences
  //
  if (uc <= 63) {
    if (needs_sign_extension) {
      Expand_Small_Multiply(result,x,uc,ops);
    } else {
      TN *r1 = Build_TN_Of_Mtype(MTYPE_I8);
      Expand_Small_Multiply(r1,x,uc,ops);
      Fixup_32_Bit_Op(result, r1, mtype,ops);
    }
    return TRUE;
  }
  
  // 
  // We have |constant| > 63, with the fewest number of 1's
  // Find where the (least significant) 1 is located.
  // If there is exactly one 1 in it, we will use a shift to do the multiply. 
  //
  INT first_1 = 0;
  while ((uc & 1) == 0) {++first_1; uc >>= 1;}
  if (first_1 != 0) {
    if (num_ones == 1) {
      // Just do the shift
      Expand_Shift(result, x, Gen_Literal_TN(first_1, 4), mtype, shift_left, ops);
      return TRUE;
    } else {
      TN *x1 = DUP_TN(x);
      Expand_Shift(x1, x, Gen_Literal_TN(first_1, 4), MTYPE_I8, shift_left, ops);
      x = x1;
    }
  }
  //
  // Another special case, 2**N - 1
  // Note that num_ones can't be 64 (or we'd have been in the -1 case above)
  // So the shift and subtract test is safe here.
  // Also, we don't want to do this case if uc is small, because we can do better
  // with the optimized sequences.
  //
  if (uc == ((1<<num_ones)-1) && uc > 63) {
    TN *r1 = DUP_TN(result);
    Expand_Shift(r1, x, Gen_Literal_TN(num_ones, 4), MTYPE_I8, shift_left, ops);
    if (!needs_sign_extension) {
      Expand_Sub(result,r1,x,mtype,ops);
    } else {
      TN *r2 = DUP_TN(result);
      Expand_Sub(r2,r1,x,mtype,ops);
      Fixup_32_Bit_Op(result,r2,mtype,ops);
    }
    return TRUE;
  }
  
  //
  // Check for some cases we can do with a single-instruction multiply on top
  // of a small multiply.
  //
  INT i;
  for (i=0; i < NUM_FAST_MPYS; i++) {
    INT mpy=fast_mpys[i];
    if (uc%mpy == 0 && uc/mpy <= 63) {
      INT64 uc1;
      TN *r1 = DUP_TN(result);
      Expand_Small_Multiply(r1,x,uc/mpy,ops);
      Expand_Constant_Multiply(result,r1,mpy,mtype,ops);
      return TRUE;
    }
  }
  
  //
  // We put things in r to make the possible sign extension a bit easier
  //
  TN *r = result;
  if (needs_sign_extension) {
    r = DUP_TN(result);
  }
  // 
  // If the remaining number is less than 16 bits, we will do it by
  // breaking it into chunks and combining them. We also handle a few special cases.
  // For numbers greater than 16 bits, we break things up and combine recursively. 
  // This is implemented for completeness but probably shouldn't be done in practice.
  //
  if (uc <= 63) {
    Expand_Small_Multiply(r,x,uc,ops);
  } else if (uc <= 1023) {
    INT64 uc1,uc2;
    TN *r1 = DUP_TN(result);
    // Do in group of 4 and at most 6
    // Note that uc2 >= 4 (or we would have been in the above case)
    uc1 = uc & 15;
    uc2 = uc >> 4;
    
    Expand_Small_Multiply(r1,x,uc2,ops);
    if (uc1 == 0) {
      shladd(r,r1,4,Zero_TN,ops);
    } else if (uc1 == 1) {
      shladd(r,r1,4,x,ops);
    } else if (uc1 == uc2) {
      shladd(r,r1,4,r1,ops);
    } else {
      TN *r2 = DUP_TN(result);
      Expand_Small_Multiply(r2,x,uc1,ops);
      shladd(r,r1,4,r2,ops);
    }
  } else if (uc <= 65535) {
    // Do in two groups of 8. Note that uc2 >= 4 again.
    // Also not that because we are combining with 2 shladds, we have 
    // additional opportunities for optimizations
    // if the low part is a multiple of 16 or 17 (smaller multiplies
    // tend to be a bit faster), or the low part is 16 or 17x the high part
    // we get it for free. 
    //
    INT64 uc1,uc2;
    TN *r1 = DUP_TN(result);
    TN *r2 = DUP_TN(result);
    uc1 = uc & 255;
    uc2 = uc >> 8;
    Expand_Constant_Multiply (r1, x, uc2, MTYPE_I8, ops);
    if (uc1 == 0) {
      shladd(r2,r1,4,Zero_TN,ops);
      shladd(r,r2,4,Zero_TN,ops);
    } else if (uc1 == 1) {
      shladd(r2,r1,4,Zero_TN,ops);
      shladd(r,r2,4,x,ops);

    } else if (uc1 == 16) {
      shladd(r2,r1,4,x,ops);
      shladd(r,r2,4,Zero_TN,ops);

    } else if (uc1 == 17) {
      shladd(r2,r1,4,x,ops);
      shladd(r,r2,4,x,ops);

    } else if (uc1 == uc2) {
      shladd(r2,r1,4,Zero_TN,ops);
      shladd(r,r2,4,r1,ops);

    } else if (uc1 == 16*uc2) {
      shladd(r2,r1,4,r1,ops);
      shladd(r,r2,4,Zero_TN,ops);

    } else if (uc1 == 17*uc2) {
      shladd(r2,r1,4,r1,ops);
      shladd(r,r2,4,r1,ops);

    } else if (uc1%16 == 0) {
      TN *r3 = DUP_TN(result);
      uc1 /= 16;
      Expand_Constant_Multiply(r3,x,uc1,MTYPE_I8,ops);
      shladd(r2,r1,4,r3,ops);
      shladd(r,r2,4,Zero_TN,ops);

    } else if (uc1%17 == 0) {
      TN *r3 = DUP_TN(result);
      uc1 /= 17;
      Expand_Constant_Multiply(r3,x,uc1,MTYPE_I8,ops);
      shladd(r2,r1,4,r3,ops);
      shladd(r,r2,4,r3,ops);

    } else {
      TN *r3 = DUP_TN(result);
      Expand_Constant_Multiply(r3,x,uc1,MTYPE_I8,ops);
      shladd(r2,r1,4,Zero_TN,ops);
      shladd(r,r2,4,r3,ops);
    }
  } else if (uc <= ((1LL << 32)-1)) {
    // For completeness, although it's probably getting to be not worth it
    // for the sheer number of instructions generated, even if the latency is good
    // (latency <= 8, instructions <= 34)
    //
    INT64 uc1,uc2;
    TN *r1 = DUP_TN(result);
    TN *r2 = DUP_TN(result);
    TN *r3 = DUP_TN(result);
    uc1 = uc & 65535;
    uc2 = uc >> 16;
    Expand_Constant_Multiply(r1,x,uc1,MTYPE_I8,ops);
    Expand_Constant_Multiply(r2,x,uc2,MTYPE_I8,ops);
    Expand_Shift(r3,r2,Gen_Literal_TN(16, 4),MTYPE_I8,shift_left,ops);
    Expand_Add(r,r1,r3,MTYPE_I8,ops);
  } else {
    // Worst case, latency <= 11, instructions <= 70
    // You really don't want to do this, but we will just let Can_Do_Fast_Multiply stop it
    //
    // For completeness, although it's probably getting to be not worth it
    // for the sheer number of instructions generated, even if the latency is good
    // (latency <= 8, instructions <= 34)
    //
    INT64 uc1,uc2;
    TN *r1 = DUP_TN(result);
    TN *r2 = DUP_TN(result);
    TN *r3 = DUP_TN(result);
    uc1 = uc & 0xffffffff;
    uc2 = uc >> 32;
    Expand_Constant_Multiply(r1,x,uc1,MTYPE_I8,ops);
    Expand_Constant_Multiply(r2,x,uc2,MTYPE_I8,ops);
    Expand_Shift(r3,r2,Gen_Literal_TN(32, 4),MTYPE_I8,shift_left,ops);
    Expand_Add(r,r1,r3,MTYPE_I8,ops);
  }

  if (needs_sign_extension) {
    Fixup_32_Bit_Op(result,r,mtype,ops);
  }

  return TRUE;
}

void
Expand_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops, OPCODE opcode)
{
  TOP new_opcode;
  INT64 constant;
  //
  // Check for two constants
  // 
  if ((TN_has_value(src1) || TN_is_rematerializable(src1)) &&
      (TN_has_value(src2) || TN_is_rematerializable(src2))) {
    // Two constants can sometimes occur because of DIVREM production in 
    TN *val_tn;
    constant = TN_has_value(src1) ? TN_value(src1) : WN_const_val(TN_home(src1));
    constant *= TN_has_value(src2) ? TN_value(src2) : WN_const_val(TN_home(src2));
    // Need to get the constant of the right length
    constant = Targ_To_Host(Host_To_Targ(mtype,constant));
    val_tn = Gen_Literal_TN(constant, 8);
    Exp_Immediate(result,val_tn,MTYPE_is_signed(mtype),ops);
    return;
  }

#ifdef KEY
  if (CGEXP_cvrt_int_mult_to_add_shift &&
      (TN_has_value(src1) || TN_has_value(src2) ||
       TN_is_rematerializable(src1) ||TN_is_rematerializable(src2))) {
#else
  if (!Disable_Const_Mult_Opt && (TN_has_value(src1) || TN_has_value(src2) ||
      TN_is_rematerializable(src1) ||TN_is_rematerializable(src2))) {
#endif
    TN *var_tn;
    if ( TN_has_value(src1) || TN_is_rematerializable(src1) ) {
      constant = TN_has_value(src1) ? TN_value(src1) : WN_const_val(TN_home(src1));
      var_tn = src2;
    }
    else {
      constant = TN_has_value(src2) ? TN_value(src2) : WN_const_val(TN_home(src2));
      var_tn = src1;
    }
    
    if (Can_Do_Fast_Multiply (mtype, constant)) {
      if (Expand_Constant_Multiply (result, var_tn, constant, mtype, ops)) {
	/* able to convert multiply into shifts/adds/subs */
	return;
      }
    }
  }
  if (TN_has_value(src2)) {
    src2 = Expand_Immediate_Into_Register (src2, ops);
  }

  /* Optimize any special cases
   */
  if (CGEXP_fast_imul) {
    switch (mtype) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_U1:
    case MTYPE_U2:
      Build_OP(TOP_pmpy2_r, result, True_TN, src1, src2, ops);
      return;
    case MTYPE_I4:
    case MTYPE_U4:
      if (!OPT_Space) {
	TN *t0 = Build_TN_Of_Mtype(MTYPE_I8);
	TN *t1 = Build_TN_Of_Mtype(MTYPE_I8);
	TN *t2 = Build_TN_Of_Mtype(MTYPE_I8);
	TN *t3 = Build_TN_Of_Mtype(MTYPE_I8);
	TN *t4 = Build_TN_Of_Mtype(MTYPE_I8);
	TN *t5 = Build_TN_Of_Mtype(MTYPE_I8);
	TN *t6 = Build_TN_Of_Mtype(MTYPE_I8);

	// see imul32.doc for the details of this algorithm
	Build_OP(TOP_mux2, t0, True_TN, src1, Gen_Literal_TN(0x50, 4), ops);
	Build_OP(TOP_mux2, t1, True_TN, src2, Gen_Literal_TN(0x44, 4), ops);
	Build_OP(TOP_pmpyshr2_u, t2, True_TN, src1, src2, Gen_Literal_TN(16, 4), ops);
	Build_OP(TOP_shl_i, t3, True_TN, t2, Gen_Literal_TN(16, 4), ops);
	Build_OP(TOP_pmpyshr2_u, t4, True_TN, t0, t1, Gen_Literal_TN(0, 4), ops);
	Build_OP(TOP_unpack2_h, t5, True_TN, t4, Zero_TN, ops);
	Build_OP(TOP_add, t6, True_TN, t4, t3, ops);
	Build_OP(TOP_add, result, True_TN, t5, t6, ops);
	return;
      }
      break;
    }
  }

  /* move to fp, xmpy, move back
   */
  TN *fdest, *fsrc1, *fsrc2;
  fdest = Build_TN_Of_Mtype (MTYPE_F8);
  fsrc1 = Build_TN_Of_Mtype (MTYPE_F8);
  fsrc2 = Build_TN_Of_Mtype (MTYPE_F8);
  Expand_G_To_F (fsrc1, src1, ops);
  Expand_G_To_F (fsrc2, src2, ops);
  new_opcode = (MTYPE_is_signed(mtype) ? TOP_xmpy_l : TOP_xmpy_lu);
  Build_OP (new_opcode, fdest, True_TN, fsrc1, fsrc2, ops);
  Expand_F_To_G (result, fdest, ops);
}

/* return high part of multiply result */
void
Expand_High_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TOP new_opcode;

  if (TN_has_value(src2)) {
	src2 = Expand_Immediate_Into_Register (src2, ops);
  }
  /* mov to fp, xmpy move back */
  TN *fdest, *fsrc1, *fsrc2;
  fdest = Build_TN_Of_Mtype (MTYPE_F8);
  fsrc1 = Build_TN_Of_Mtype (MTYPE_F8);
  fsrc2 = Build_TN_Of_Mtype (MTYPE_F8);
  Expand_G_To_F (fsrc1, src1, ops);
  Expand_G_To_F (fsrc2, src2, ops);
  new_opcode = (MTYPE_is_signed(mtype) ? TOP_xmpy_h : TOP_xmpy_hu);
  Build_OP (new_opcode, fdest, True_TN, fsrc1, fsrc2, ops);
  Expand_F_To_G (result, fdest, ops);
}


static void
Expand_Normalize_Logical (TN *src, OPS *ops)
{
  TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  Build_OP (TOP_cmp_ne, p1, p2, True_TN, src, Zero_TN, ops);
  Build_OP (TOP_mov_i, src, p1, Gen_Literal_TN(1, 4), ops);
}

void
Expand_Logical_Not (TN *dest, TN *src, VARIANT variant, OPS *ops)
{
  /* dest = (src == 0) ? 1 : 0 */
  if (TN_register_class(dest) == ISA_REGISTER_CLASS_predicate) {
    TN *p1 = dest;
    TN *p2 = Get_Complement_TN(dest);
    if (TN_register_class(src) == ISA_REGISTER_CLASS_predicate) {
      Build_OP (TOP_cmp_eq, p1, p2, True_TN, Zero_TN, Zero_TN, ops);
      Build_OP (TOP_cmp_ne, p1, p2, src, Zero_TN, Zero_TN, ops);
    } else {
      Build_OP (TOP_cmp_eq, p1, p2, True_TN, src, Zero_TN, ops);
    }
  } else {
   /*
    *  if CG_EXP_normalize is true we must normalized the operands
    *  (if not already normalized)
    */
    if (!V_normalized_op1(variant) && CGEXP_normalize_logical)
    {
          Expand_Normalize_Logical (src, ops);
    }
    Build_OP (TOP_xor_i, dest, True_TN, Gen_Literal_TN(1, 4), src, ops);
  }
}

/*
**	dest = (src1 != 0 & src2 != 0) ? 1 : 0 
**	sltu	t1, 0, s1		(if not normalized)
**	sltu	t2, 0, s2		(if not normalized)
**	and/or	d, t1, t2 
*/
static void
Expand_Logical_And_Or (TOP action, TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
 /*
  *  if CG_EXP_normalize is true we must normalized the operands 
  *  (if not already normalized)
  */
  if (!V_normalized_op1(variant) && CGEXP_normalize_logical)
  {
    Expand_Normalize_Logical (src1, ops);
  }
  if (!V_normalized_op2(variant) && CGEXP_normalize_logical)
  {
    Expand_Normalize_Logical (src2, ops);
  }

  Build_OP (action, dest, True_TN, src1, src2, ops);
}

void
Expand_Logical_And (TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
        Expand_Logical_And_Or (TOP_and, dest, src1, src2, variant, ops);
}

void
Expand_Logical_Or (TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
        Expand_Logical_And_Or (TOP_or, dest, src1, src2, variant, ops);
}


void
Expand_Binary_Complement (TN *dest, TN *src, TYPE_ID /* mtype */, OPS *ops)
{
  /* complement == nor src $0 */
  Build_OP (TOP_xor_i, dest, True_TN, Gen_Literal_TN(-1, 4), src, ops);
}


/* Expand special cases of AND with an immediate. These are cases
 * where the constant is too big for TOP_and_i, but can be handled
 * more simply than loading the constant into a register and using TOP_and.
 *
 * NOTE: that 'mix' could be used to zero fixed patterns of bytes and
 * shorts, but I couldn't find a case to trigger it so I left it out -- Ken
 */
BOOL Expand_Special_And_Immed(TN *dest, TN *src1, INT64 imm, OPS *ops)
{
  UINT len;
  UINT start;
  UINT64 v;

  /* We shouldn't get here with these, but if we do they cause the routine
   * to fail, so guard against it.
   */
  if (imm == 0 || imm == -1) {
    Is_True(FALSE, ("Expand_Special_And_Immed called with imm == %d", imm));
    return FALSE;
  }

  /* Find the bit range that is being zeroed out by the AND.
   * If there is more than one contiguous range of zeros then
   * give up since we can't handle it.
   */
  start = 0;
  for (v = ~imm; (v & 1) == 0; v >>= 1) ++start;

  len = 0;
  for (; (v & 1) != 0; v >>= 1) ++len;

  if (v != 0) return FALSE;

  /* If the zeroed bits include bit-63, then we are performing an
   * extract, so use either extr.u or zxt*. Otherwise use dep.i.
   * Note that we could always use dep.i, but the extract is more 
   * readable, and zxt* might be faster in some cases.
   */
  if (start > 0 && start + len == 64) {
    const UINT extr_len = start;
    if (extr_len == 8 || extr_len == 16 || extr_len == 32) {
      TOP zxt_opc;
      switch (extr_len / 8) {
      case 1: zxt_opc = TOP_zxt1; break;
      case 2: zxt_opc = TOP_zxt2; break;
      case 4: zxt_opc = TOP_zxt4; break;
      }
      Build_OP(zxt_opc, dest, True_TN, src1, ops);
    } else {
      Build_OP(TOP_extr_u, dest, True_TN, src1,
	       Gen_Literal_TN(0, 4), Gen_Literal_TN(extr_len, 4), ops);
    }
  } else {
    Build_OP(TOP_dep_i, dest, True_TN, Gen_Literal_TN(0, 4), src1,
	     Gen_Literal_TN(start, 4), Gen_Literal_TN(len, 4), ops);
  }

  return TRUE;
}


static void
Expand_Binary_And_Or (TOP action, TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  /* binary AND: and || andi */
  if (TN_has_value(src2)) {
	if (ISA_LC_Value_In_Class(TN_value(src2), LC_i8)) {
		action = Pick_Imm_Form_TOP (action);
  		// switch order of src so immediate is first
  		Build_OP (action, dest, True_TN, src2, src1, ops);
		return;
	} else if (action == TOP_and &&
		   Expand_Special_And_Immed(dest, src1, TN_value(src2), ops))
	{
		return;
	} else {
		src2 = Expand_Immediate_Into_Register (src2, ops);
	}
  }
  Build_OP (action, dest, True_TN, src1, src2, ops);
}

void
Expand_Binary_And (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
       	Expand_Binary_And_Or (TOP_and, dest, src1, src2, mtype, ops);
}

void
Expand_Binary_Nand (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TN *tmp = Build_TN_Like(dest);
  Expand_Binary_And_Or (TOP_and, tmp, src1, src2, mtype, ops);
  Expand_Binary_And_Or (TOP_xor, dest, tmp, Gen_Literal_TN(1, 4), mtype, ops);
}

void
Expand_Binary_Or (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
        Expand_Binary_And_Or (TOP_or, dest, src1, src2, mtype, ops);
}

void
Expand_Binary_Xor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
        Expand_Binary_And_Or (TOP_xor, dest, src1, src2, mtype, ops);
}

void
Expand_Binary_Nor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
	// nor is or s1,s2; xor -1
	//
	// NOTE: if one of the operands is constant, the expansion
	// could be 'andcm ~imm,s2' which is one inst if the complemented
	// constant fits in the immed. But testing has not found a
	// case where this would occur, so leave it out. -- Ken
	TN *tmp = Build_TN_Like(dest);
        Expand_Binary_And_Or (TOP_or, tmp, src1, src2, mtype, ops);
        Expand_Binary_And_Or (TOP_xor, dest, tmp, Gen_Literal_TN(-1, 4), mtype, ops);
}

static void
Expand_Int_Comparison (TOP action, TN *dest, TN *src1, TN *src2, OPS *ops)
{
  if (TN_register_class(dest) == ISA_REGISTER_CLASS_predicate) {
    // return result of comparison in a predicate register
    TN *p1 = dest;
    TN *p2 = Get_Complement_TN(dest);
    Build_OP (action, p1, p2, True_TN, src1, src2, ops);
  } else {
    // return result of comparison in an integer register
    TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    Build_OP (action, p1, p2, True_TN, src1, src2, ops);
    // can either do unconditional copy of 0,
    // or predicated copy of 0 followed by predicated copy of 1.
    // Expand_Copy (dest, Zero_TN, MTYPE_I8, ops);
    Build_OP (TOP_mov, dest, p2, Zero_TN, ops);
    Build_OP (TOP_mov_i, dest, p1, Gen_Literal_TN(1, 4), ops);
  }
}

void
Expand_Int_Less (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  VARIANT variant;
  TOP action;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8LT; break;
  case MTYPE_I4: variant = V_BR_I4LT; break;
  case MTYPE_U8: variant = V_BR_U8LT; break;
  case MTYPE_U4: variant = V_BR_U4LT; break;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_Int_Less: MTYPE_%s is not handled", Mtype_Name(mtype)));
  }
  action = Pick_Compare_TOP (&variant, &src1, &src2, ops);
  Expand_Int_Comparison (action, dest, src1, src2, ops);
}

void
Expand_Int_Less_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  VARIANT variant;
  TOP action;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8LE; break;
  case MTYPE_I4: variant = V_BR_I4LE; break;
  case MTYPE_U8: variant = V_BR_U8LE; break;
  case MTYPE_U4: variant = V_BR_U4LE; break;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_Int_Less_Equal: MTYPE_%s is not handled", Mtype_Name(mtype)));
  }
  action = Pick_Compare_TOP (&variant, &src1, &src2, ops);
  Expand_Int_Comparison (action, dest, src1, src2, ops);
}

void
Expand_Int_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  VARIANT variant;
  TOP action;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8EQ; break;
  case MTYPE_I4: variant = V_BR_I4EQ; break;
  case MTYPE_U8: variant = V_BR_U8EQ; break;
  case MTYPE_U4: variant = V_BR_U4EQ; break;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_Int_Equal: MTYPE_%s is not handled", Mtype_Name(mtype)));
  }
  action = Pick_Compare_TOP (&variant, &src1, &src2, ops);
  Expand_Int_Comparison (action, dest, src1, src2, ops);
}

void
Expand_Int_Not_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  VARIANT variant;
  TOP action;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8NE; break;
  case MTYPE_I4: variant = V_BR_I4NE; break;
  case MTYPE_U8: variant = V_BR_U8NE; break;
  case MTYPE_U4: variant = V_BR_U4NE; break;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_Int_Not_Equal: MTYPE_%s is not handled", Mtype_Name(mtype)));
  }
  action = Pick_Compare_TOP (&variant, &src1, &src2, ops);
  Expand_Int_Comparison (action, dest, src1, src2, ops);
}

void
Expand_Int_Greater_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  VARIANT variant;
  TOP action;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8GE; break;
  case MTYPE_I4: variant = V_BR_I4GE; break;
  case MTYPE_U8: variant = V_BR_U8GE; break;
  case MTYPE_U4: variant = V_BR_U4GE; break;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_Int_Greater_Equal: MTYPE_%s is not handled", Mtype_Name(mtype)));
  }
  action = Pick_Compare_TOP (&variant, &src1, &src2, ops);
  Expand_Int_Comparison (action, dest, src1, src2, ops);
}

void
Expand_Int_Greater (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  VARIANT variant;
  TOP action;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8GT; break;
  case MTYPE_I4: variant = V_BR_I4GT; break;
  case MTYPE_U8: variant = V_BR_U8GT; break;
  case MTYPE_U4: variant = V_BR_U4GT; break;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_Int_Greater: MTYPE_%s is not handled", Mtype_Name(mtype)));
  }
  action = Pick_Compare_TOP (&variant, &src1, &src2, ops);
  Expand_Int_Comparison (action, dest, src1, src2, ops);
}

static void
Expand_Bool_Comparison (BOOL equals, TN *dest, TN *src1, TN *src2, OPS *ops)
{
  if (TN_register_class(dest) == ISA_REGISTER_CLASS_predicate) {
    // return result of comparison in a predicate register
    TOP action = equals ? TOP_cmp_ne : TOP_cmp_eq;
    TN *p1 = dest;
    TN *p2 = Get_Complement_TN (dest);
    TN *tn = Build_TN_Of_Mtype (MTYPE_I4);

    // generate: tn = (src1 == src2)
    Build_OP (TOP_mov_i, tn, True_TN, Gen_Literal_TN (1, 4), ops);
    Build_OP (TOP_xor_i, tn, src1, Gen_Literal_TN (1, 4), tn, ops);
    Build_OP (TOP_xor_i, tn, src2, Gen_Literal_TN (1, 4), tn, ops);

    Build_OP (action, p1, p2, True_TN, tn, Zero_TN, ops);
  } 
  else if (TN_is_dedicated (dest)) {
    TN *tn = Build_TN_Of_Mtype (MTYPE_I4);
    
    Build_OP (TOP_mov_i, tn, True_TN, Gen_Literal_TN (equals, 4), ops);
    Build_OP (TOP_xor_i, tn, src1, Gen_Literal_TN (1, 4), tn, ops);
    Build_OP (TOP_xor_i, tn, src2, Gen_Literal_TN (1, 4), tn, ops);
    
    Build_OP (TOP_mov, dest, True_TN, tn, ops); 
  }
  else {
    Build_OP (TOP_mov_i, dest, True_TN, Gen_Literal_TN (equals, 4), ops);
    Build_OP (TOP_xor_i, dest, src1, Gen_Literal_TN (1, 4), dest, ops);
    Build_OP (TOP_xor_i, dest, src2, Gen_Literal_TN (1, 4), dest, ops);
  }
}

void
Expand_Bool_Equal (TN *dest, TN *src1, TN *src2, OPS *ops)
{
  Expand_Bool_Comparison (TRUE, dest, src1, src2, ops);
}

void
Expand_Bool_Not_Equal (TN *dest, TN *src1, TN *src2, OPS *ops)
{
  Expand_Bool_Comparison (FALSE, dest, src1, src2, ops);
}

void
Expand_Bool_To_Int (TN *dest, TN *src, TYPE_ID rtype, OPS *ops)
{
  Build_OP (TOP_mov, dest, True_TN, Zero_TN, ops);
  Build_OP (TOP_mov_i, dest, src, Gen_Literal_TN(1, 4), ops);
}

typedef enum {
  ROUND_USER,
  ROUND_NEAREST,
  ROUND_CHOP,
  ROUND_NEG_INF,
  ROUND_PLUS_INF
} ROUND_MODE;

// TODO how do you trap on float val too big for [u]int32?
static void
Expand_Float_To_Int (ROUND_MODE rm, TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  // IFCVT -> fsetc ; fcvt.fx ; getf
  TOP fcvt;
  TN *sf;
  const BOOL is_signed = MTYPE_is_signed(imtype);
  TN *tmp = Build_RCLASS_TN(ISA_REGISTER_CLASS_float);
  switch (rm) {
  case ROUND_USER:
    fcvt = is_signed ? TOP_fcvt_fx : TOP_fcvt_fxu;
    sf = Gen_Enum_TN(ECV_sf_s0);
    break;
  case ROUND_NEAREST:
    fcvt = is_signed ? TOP_fcvt_fx : TOP_fcvt_fxu;
    sf = Gen_Enum_TN(ECV_sf_s2);
    Build_OP(TOP_fsetc, 
	     True_TN, sf, Gen_Literal_TN(0x4f, 4), Gen_Literal_TN(0x00, 4), ops);
    break;
  case ROUND_CHOP:
    fcvt = is_signed ? TOP_fcvt_fx_trunc : TOP_fcvt_fxu_trunc;
    sf = Gen_Enum_TN(ECV_sf_s0);
    break;
  case ROUND_NEG_INF:
    fcvt = is_signed ? TOP_fcvt_fx : TOP_fcvt_fxu;
    sf = Gen_Enum_TN(ECV_sf_s2);
    Build_OP(TOP_fsetc, 
	     True_TN, sf, Gen_Literal_TN(0x4f, 4), Gen_Literal_TN(0x10, 4), ops);
    break;
  case ROUND_PLUS_INF:
    fcvt = is_signed ? TOP_fcvt_fx : TOP_fcvt_fxu;
    sf = Gen_Enum_TN(ECV_sf_s2);
    Build_OP(TOP_fsetc, 
	     True_TN, sf, Gen_Literal_TN(0x4f, 4), Gen_Literal_TN(0x20, 4), ops);
    break;
  }
  Build_OP(fcvt, tmp, True_TN, sf, src, ops);
  Build_OP(TOP_getf_sig, dest, True_TN, tmp, ops);
}

void
Expand_Float_To_Int_Cvt (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_USER, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Round (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_NEAREST, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Trunc (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_CHOP, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Floor (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    Is_True ( MTYPE_is_float(fmtype), ("fmtype is not floating-point"));
    // OSP, Expand_Float_To_Int does not support the src larger than 2^64
    if ( MTYPE_is_integral(imtype) ) {
        Expand_Float_To_Int (ROUND_NEG_INF, dest, src, imtype, fmtype, ops);
    }
    else {
        // new implementation for fp
        TN* expmask = Build_TN_Of_Mtype(MTYPE_I4);
        Build_OP(TOP_mov_i, expmask, True_TN, Gen_Literal_TN(0x1FFFF, 4), ops);

        TN* sigwidth = Build_TN_Of_Mtype(MTYPE_I4);
        switch(fmtype) {
            default:
                FmtAssert(FALSE, ("Invalid fmtype in ..."));
            case MTYPE_F4:
                // single precision, 23 bits significand
                Build_OP(TOP_mov_i, sigwidth, True_TN, Gen_Literal_TN(0x10016, 4), ops);
                break;
            case MTYPE_F8:
                // double precision, 52 bits significand
                Build_OP(TOP_mov_i, sigwidth, True_TN, Gen_Literal_TN(0x10033, 4), ops);
                break;
            case MTYPE_F10:
                // double extended, 63 bits fraction
                Build_OP(TOP_mov_i, sigwidth, True_TN, Gen_Literal_TN(0x1003E, 4), ops);
                break;
        }

        TN* exp = Build_TN_Of_Mtype(MTYPE_I4);
        Build_OP (TOP_getf_exp, exp, True_TN, src, ops);

        TN* trunc_val = Build_RCLASS_TN(ISA_REGISTER_CLASS_float);
        Build_OP (TOP_fcvt_fx_trunc, trunc_val, True_TN, 
                  Gen_Enum_TN(ECV_sf_s1), src, ops);

        Build_OP (TOP_fmerge_s, dest, True_TN, 
                  src, src, ops);

        Build_OP (TOP_fcvt_xf, trunc_val, True_TN, trunc_val, ops);
        Build_OP (TOP_and, exp, True_TN, expmask, exp, ops);

        TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
        TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
     
        //TN* dest_fp = Build_RCLASS_TN(ISA_REGISTER_CLASS_float);
        Build_OP (TOP_cmp_geu_unc, True_TN, p1, True_TN, exp, sigwidth, ops);
        Build_OP (TOP_fcmp_nlt_unc, p2, p1, p1, 
                  Gen_Enum_TN(ECV_sf_s1), src, trunc_val, ops);
        Build_OP (TOP_fsub_d, dest, p1, 
                  Gen_Enum_TN(ECV_sf_s1), trunc_val, FOne_TN, ops);
        Build_OP (TOP_fmerge_s, dest, p2, src, trunc_val, ops);
        //Build_OP (TOP_getf_sig, dest, True_TN, dest_fp, ops);
    }
}

void
Expand_Float_To_Int_Ceil (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_PLUS_INF, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Float (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  TOP top;
  if (mtype == MTYPE_F4)
	top = TOP_fnorm_s;
  else if (mtype == MTYPE_F8)
	top = TOP_fnorm_d;
  else if (mtype == MTYPE_F10)
	top = TOP_fnorm;
  else
  	FmtAssert(FALSE, ("NYI:  Expand_Float_To_Float mtype"));
  Build_OP (top, dest, True_TN, Gen_Enum_TN(ECV_sf_s0), src, ops);
}


void
Expand_Int_To_Float (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  /* FICVT -> setf ; fcvt.xf */
  TN *tmp = Build_TN_Like(dest);
  Build_OP (TOP_setf_sig, tmp, True_TN, src, ops);
  if (MTYPE_is_signed(imtype)) {
	TN *tmp2 = Build_TN_Like(dest);
  	Build_OP (TOP_fcvt_xf, tmp2, True_TN, tmp, ops);
	tmp = tmp2;
  }
  // else unsigned, then fnorm is equivalent to fcvt.xuf
  Expand_Float_To_Float (dest, tmp, fmtype, ops);
}


static BOOL
Optimize_Select (
	TOP cmp,
  	TN *cond1, 
  	TN *cond2, 
  	TN *dest, 
  	TN *dest2,
  	TN *src1, 
  	TN *src2, 
	BOOL is_float,
	OPS *ops)
{

  // The comparison opnds must be the same as the src opnds.
  BOOL reversed;
  if (cond1 == src1 && cond2 == src2) {
    reversed = FALSE;
  } else if (cond1 == src2 && cond2 == src1) {
    reversed = TRUE;
  } else {
    return FALSE;
  }

  // Now optimize according to the type of comparison.
  switch (cmp) {
  case TOP_fcmp_gt:
  case TOP_fcmp_ge:
    reversed = !reversed;
    /*FALLTHROUGH*/
  case TOP_fcmp_lt:
  case TOP_fcmp_le:
    {
      TOP opc = reversed ? TOP_fmax : TOP_fmin;
      Build_OP(opc, dest, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
      if (dest2) {
	opc = reversed ? TOP_fmin : TOP_fmax;
	Build_OP(opc, dest2, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
      }
    }
    return TRUE;
  case TOP_cmp_ne:
  case TOP_cmp4_ne:
  case TOP_cmp_i_ne:
  case TOP_cmp4_i_ne:
  case TOP_fcmp_neq:
    reversed = !reversed;
    /*FALLTHROUGH*/
  case TOP_cmp_eq:
  case TOP_cmp4_eq:
  case TOP_cmp_i_eq:
  case TOP_cmp4_i_eq:
  case TOP_fcmp_eq:
    {
      TN *src = reversed ? src1 : src2;
      Exp_COPY(dest, src, ops);
      if (dest2) {
	src = reversed ? src2 : src1;
	Exp_COPY(dest2, src, ops);
      }
    }
    return TRUE;
  }

  return FALSE;
}


static void
Expand_Compare_And_Select (
	TOP cmp,
  	TN *cond1, 
  	TN *cond2, 
  	TN *dest, 
  	TN *opposite_dest, 
  	TN *true_tn, 
  	TN *false_tn, 
	BOOL is_float,
	OPS *ops)
{
  // Look for special cases to optimize
  if (Optimize_Select(cmp, cond1, cond2, dest, opposite_dest,
		      true_tn, false_tn, is_float, ops)) return;

  TOP tmove;
  TOP fmove;
  TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  if (TOP_is_flop(cmp)) {
	Build_OP (cmp, p1, p2, True_TN, Gen_Enum_TN(ECV_sf_s0), 
		  cond1, cond2, ops);
  } else {
	Build_OP (cmp, p1, p2, True_TN, cond1, cond2, ops);
  }

  // do predicated copies of the true and false sides
  if (is_float) {
	tmove = TOP_mov_f;
	fmove = TOP_mov_f;
  } else {
	tmove = TN_has_value(true_tn) ? TOP_mov_i : TOP_mov;
	fmove = TN_has_value(false_tn) ? TOP_mov_i : TOP_mov;
  }
  Build_OP (tmove, dest, p1, true_tn, ops);
  Build_OP (fmove, dest, p2, false_tn, ops);
  if (opposite_dest) {
  	Build_OP (tmove, opposite_dest, p2, true_tn, ops);
  	Build_OP (fmove, opposite_dest, p1, false_tn, ops);
  }
}

void
Expand_Select (
  TN *dest_tn, 
  TN *cond_tn, 
  TN *true_tn, 
  TN *false_tn, 
  TYPE_ID mtype, 
  BOOL float_cond,
  OPS *ops)
{
  const BOOL is_float = MTYPE_is_float(mtype);
  if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_predicate) {
    TOP tmove, fmove;
    TN *p1 = cond_tn;

    // Get_Complement_TN()  can't be used if p1 is not dedicated register.
//    FmtAssert(ops->length!=0, ("The whole op sequence should be passed in."));
    TN *p2;
    if ((ops->length!=0) && (OP_result(OPS_last(ops), 0)==p1)) {
      p2 = OP_result(OPS_last(ops), 1); // the predicate conversion of boolean exppression gurantees this.
    }
    else 
    	{
      p2 = Get_Complement_TN(cond_tn);
    }

    if (is_float) {
      tmove = TOP_mov_f;
      fmove = TOP_mov_f;
    } else {
      tmove = TN_has_value(true_tn) ? TOP_mov_i : TOP_mov;
      fmove = TN_has_value(false_tn) ? TOP_mov_i : TOP_mov;
    }
    Build_OP (tmove, dest_tn, p1, true_tn, ops);
    Build_OP (fmove, dest_tn, p2, false_tn, ops);
  } else {
    // create compare of cond to 0
    TOP cmp;
    if (float_cond)
 	cmp = TOP_fcmp_neq;
    else if (MTYPE_is_size_double(mtype))
	cmp = TOP_cmp_ne;
    else
	cmp = TOP_cmp4_ne;
    Expand_Compare_And_Select (cmp, cond_tn, 
	(float_cond ? FZero_TN : Zero_TN), 
	dest_tn, NULL,
	true_tn, false_tn, 
	is_float, ops);
  }
}

void
Expand_Min (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  // t = s1 < s2; d = select t ? s1 : s2
  TOP cmp;
  VARIANT variant;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8LT; break;
  case MTYPE_I4: variant = V_BR_I4LT; break;
  case MTYPE_U8: variant = V_BR_U8LT; break;
  case MTYPE_U4: variant = V_BR_U4LT; break;
  case MTYPE_F4: 
  case MTYPE_F8: 
  case MTYPE_F10: 
    Build_OP(TOP_fmin, dest, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
    return;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_Min: unexpected mtype"));
  }
  cmp = Pick_Compare_TOP (&variant, &src1, &src2, ops);

  Is_True(!MTYPE_is_float(mtype), ("Expand_Min: float should have been handled"));
  Expand_Compare_And_Select (cmp, src1, src2, dest, NULL, src1, src2, FALSE, ops);
}

void
Expand_Max (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  // t = s1 > s2; d = select t ? s1 : s2
  TOP cmp;
  VARIANT variant;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8GT; break;
  case MTYPE_I4: variant = V_BR_I4GT; break;
  case MTYPE_U8: variant = V_BR_U8GT; break;
  case MTYPE_U4: variant = V_BR_U4GT; break;
  case MTYPE_F4: 
  case MTYPE_F8: 
  case MTYPE_F10: 
    Build_OP(TOP_fmax, dest, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
    return;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_Max: unexpected mtype"));
  }
  cmp = Pick_Compare_TOP (&variant, &src1, &src2, ops);

  Is_True(!MTYPE_is_float(mtype), ("Expand_Max: float should have been handled"));
  Expand_Compare_And_Select (cmp, src1, src2, dest, NULL, src1, src2, FALSE, ops);
}

void
Expand_MinMax (TN *dest, TN *dest2, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  // t = s1 < s2; d = select t ? s1 : s2; d2 = select t ? s2 : s1
  TOP cmp;
  VARIANT variant;
  switch (mtype) {
  case MTYPE_I8: variant = V_BR_I8LT; break;
  case MTYPE_I4: variant = V_BR_I4LT; break;
  case MTYPE_U8: variant = V_BR_U8LT; break;
  case MTYPE_U4: variant = V_BR_U4LT; break;
  case MTYPE_F4: 
  case MTYPE_F8: 
  case MTYPE_F10: 
    Build_OP(TOP_fmin, dest, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
    Build_OP(TOP_fmax, dest2, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
    return;
  default:
    #pragma mips_frequency_hint NEVER
    Is_True(FALSE, ("Expand_MinMax: unexpected mtype"));
  }
  cmp = Pick_Compare_TOP (&variant, &src1, &src2, ops);

  Is_True(!MTYPE_is_float(mtype), ("Expand_MinMax: float should have been handled"));
  Expand_Compare_And_Select (cmp, src1, src2, dest, dest2, src1, src2, FALSE, ops);
}

/* check whether to eval condition before select */
extern BOOL
Check_Select_Expansion (OPCODE compare)
{
  // in order to get optimal code,
  // don't evaluate the condition first,
  // but pass the condition and kids to exp_select,
  // which will do the compare and use the predicate results.
  return FALSE;
}

extern void 
Exp_Select_And_Condition (
        OPCODE select, TN *result, TN *true_tn, TN *false_tn,
        OPCODE compare, TN *cmp_kid1, TN *cmp_kid2, VARIANT variant, OPS *ops)
{
  OPS newops = OPS_EMPTY;
  TOP cmp = Pick_Compare_TOP (&variant, &cmp_kid1, &cmp_kid2, &newops);

  switch (variant) {
  case V_BR_PEQ:
  case V_BR_PNE:
    {
      Is_True(cmp == TOP_UNDEFINED, 
	      ("unexpected compare op for V_BR_PEQ/V_BR_PNE"));

      // tmp = (cmp_kid1 == cmp_kid2)
      TN *tmp = Build_TN_Of_Mtype (MTYPE_I8);
      Build_OP (TOP_mov_i, tmp, True_TN, Gen_Literal_TN(1, 8), &newops);
      Build_OP (TOP_xor_i, tmp, cmp_kid1, Gen_Literal_TN(1, 8), tmp, &newops);
      Build_OP (TOP_xor_i, tmp, cmp_kid2, Gen_Literal_TN(1, 8), tmp, &newops);

      cmp = (variant == V_BR_PEQ) ? TOP_cmp_ne : TOP_cmp_eq;
      cmp_kid1 = tmp;
      cmp_kid2 = Zero_TN;
    }
    break;
  case V_BR_NONE:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("Exp_Select_And_Condition given br_none variant"));
    /*NOTREACHED*/
  default:
    FmtAssert(cmp != TOP_UNDEFINED, ("Exp_Select_And_Condition: unexpected comparison"));
    break;
  }

  Expand_Compare_And_Select (cmp, cmp_kid1, cmp_kid2, 
			     result, NULL, true_tn, false_tn,
			     MTYPE_is_float(OPCODE_rtype(select)), &newops);

  if (Trace_Exp) {
    #pragma mips_frequency_hint NEVER
    OP *op;
    fprintf(TFile, "Exp_Select_And_Condition:\n");
    FOR_ALL_OPS_OPs(&newops, op) {
      fprintf(TFile, " into ");
      Print_OP (op);
    }
  }

  OPS_Append_Ops(ops, &newops);
}


#define RESET_COND_DEF_LAST(ops) Set_OP_cond_def_kind(OPS_last(ops),OP_ALWAYS_UNC_DEF)

static void
Expand_SGI_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  /*	(p0) frsqrta.s0 f6,p2=src	# y2 = ~1/sqrt(x)
   *
   *	(p2) ldfd	f4=half		# f4 = 0.5 (0x3fe0000000000000)
   *	(p2) ldfd	f7=ah		# f7 = 0x3fe0000000000001
   *
   *	(p2) fmpy.d.s1	f3=src,f6	# g = x*y2
   *	(p2) fmpy.d.s1	f2=f4,f6	# y = 0.5*y2
   *
   *	(p2) fnma.d.s1	f5=f3,f3,src	# d = x - g*g
   *
   *	(p2) fma.d.s1	f3=f2,f5,f3	# g = g + y*d # 16 bit approximation
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s1   f3=f2,f5,f3     # g = g + y*d # 32 bit approximation
   *	(p2) fadd.d.s1  f6=f3,f3        # y2 = y + y
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s1   f3=f2,f5,f3     # g = g + y*d # 64 bit approximation before rounding
   *	(p2) fadd.d.s1  f6=f3,f3        # y2 = y + y
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s0   f6=f2,f5,f3	# result = g + y*d
   */
  // 3-mar-00/ken: this doesn't work for MTYPE_F10!!!!

  INT i;
  TN *tn;
  TN *p2;
  TN *f2, *f3, *f4, *f5, *f6, *f6a, *f7, *f8;
  BOOL is_double = MTYPE_is_size_double(mtype) != 0;

  // Per Dick's suggestion, use the dynamic precision version
  /*
  TOP fnma = is_double ? TOP_fnma_d : TOP_fnma_s;
  TOP fma  = is_double ? TOP_fma_d  : TOP_fma_s;
  TOP fmpy = is_double ? TOP_fmpy_d : TOP_fmpy_s;
  TOP fadd = is_double ? TOP_fadd_d : TOP_fadd_s;
  TOP rounded_fma = fma;
  */

  const TOP fnma = TOP_fnma;
  const TOP fma  = TOP_fma;
  const TOP fmpy = TOP_fmpy;
  const TOP fadd = TOP_fadd;
  const TOP rounded_fma = is_double ? TOP_fma_d  : TOP_fma_s;

  INT epsilon_pow2 = is_double ? -53 : -24;
  const INT exp_bias = 0xffff;
  TN * const p0 = True_TN;

  p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  f6 = Build_TN_Of_Mtype (mtype);
  Build_OP(TOP_frsqrta, f6, p2, p0, Gen_Enum_TN(ECV_sf_s0), src, ops);

  // f4 = 0.5
  tn = Build_TN_Of_Mtype (MTYPE_I8);
  Build_OP(TOP_mov_i, tn, p0, Gen_Literal_TN(-1 + exp_bias, 4), ops);
  f4 = Build_TN_Of_Mtype (mtype);
  Build_OP(TOP_setf_exp, f4, p0, tn, ops);

  // f7 = 0.5 + epsilon
  tn = Build_TN_Of_Mtype (MTYPE_I8);
  Build_OP(TOP_mov_i, tn, p0, Gen_Literal_TN(epsilon_pow2 + exp_bias, 4), ops);
  f7 = Build_TN_Of_Mtype (mtype);
  Build_OP(TOP_setf_exp, f7, p0, tn, ops);
  tn = Build_TN_Of_Mtype (mtype);
  Build_OP(TOP_fadd, tn, p0, Gen_Enum_TN(ECV_sf_s1), f7, f4, ops);
  f7 = tn;

  f3 = Build_TN_Of_Mtype (mtype);
  Build_OP(fmpy, f3, p2, Gen_Enum_TN(ECV_sf_s1), src, f6, ops);
  RESET_COND_DEF_LAST(ops);
  tn = Build_TN_Of_Mtype (mtype);
  Build_OP(fmpy, tn, p2, Gen_Enum_TN(ECV_sf_s1), f4, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f2 = tn;

  f5 = Build_TN_Of_Mtype (mtype);
  Build_OP(fnma, f5, p2, Gen_Enum_TN(ECV_sf_s1), f3, f3, src, ops);
  RESET_COND_DEF_LAST(ops);

  tn = Build_TN_Of_Mtype (mtype);
  Build_OP(fma, tn, p2, Gen_Enum_TN(ECV_sf_s1), f2, f5, f3, ops);
  RESET_COND_DEF_LAST(ops);
  f3 = tn;

  f6a = f6;
  for (i = 0;; ++i) {
    tn = Build_TN_Of_Mtype (mtype);
    Build_OP(fnma, tn, p2, Gen_Enum_TN(ECV_sf_s1), f2, f3, f7, ops);
    RESET_COND_DEF_LAST(ops);
    f8 = tn;
    tn = Build_TN_Of_Mtype (mtype);
    Build_OP(fnma, tn, p2, Gen_Enum_TN(ECV_sf_s1), f3, f3, src, ops);
    RESET_COND_DEF_LAST(ops);
    f5 = tn;
    tn = Build_TN_Of_Mtype (mtype);
    Build_OP(fma, tn, p2, Gen_Enum_TN(ECV_sf_s1), f8, f6a, f2, ops);
    RESET_COND_DEF_LAST(ops);
    f2 = tn;

    if (i == 1 + is_double) break;

    tn = Build_TN_Of_Mtype (mtype);
    Build_OP(fma, tn, p2, Gen_Enum_TN(ECV_sf_s1), f2, f5, f3, ops);
    RESET_COND_DEF_LAST(ops);
    f3 = tn;
    tn = Build_TN_Of_Mtype (mtype);
    Build_OP(fadd, tn, p2, Gen_Enum_TN(ECV_sf_s1), f2, f2, ops);
    RESET_COND_DEF_LAST(ops);
    f6a = tn;
  }

  Build_OP(rounded_fma, f6, p2, Gen_Enum_TN(ECV_sf_s0), f2, f5, f3, ops);

  /* When p2 is clear, the result of frsqrta is the sqrt. Therefore,
   * the frsqrta and the iterations must target the same TN. 
   * To avoid conflicts with src we copy to the real result here. 
   * Other components will optimize away the copy if possible.
   */
  Exp_COPY(result, f6, ops);
}


static void
Expand_Intel_F10_Sqrt(TN *result, TN *src, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN * const f6 = src;	// load the argument a in f6
  TN *f7, *f8, *f9, *f10, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *r2;
  TN *t1;

  // BEGIN DOUBLE EXTENDED SQUARE ROOT ALGORITHM

  // exponent of +1/2 in r2
  // movl r2 = 0x0fffe;;
  t1 = Gen_Literal_TN(0x0fffe, 8);
  r2 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r2, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r2, TN_value(t1));
  // +1/2 in f10
  // setf.exp f8 = r2
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_exp, f8, p0, r2, ops);
  // Step (1)
  // y0 = 1/sqrt(a) in f7
  // frsqrta.s0 f7,p6=f6
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  f7 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_frsqrta, f7, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, ops);
  fr = f7;
  // Step (2)
  // H0 = +1/2 * y0 in f9
  // (p6) fma.s1 f9=f8,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (3)
  // S0 = a * y0 in f7
  // (p6) fma.s1 f7=f6,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (4)
  // d0 = 1/2 - S0 * H0 in f10
  // (p6) fnma.s1 f10=f7,f9,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (5)
  // H1 = H0 + d0 * H0 in f9
  // (p6) fma.s1 f9=f10,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (6)
  // S1 = S0 + d0 * S0 in f7
  // (p6) fma.s1 f7=f10,f7,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f7, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (7)
  // d1 = 1/2 - S1 * H1 in f10
  // (p6) fnma.s1 f10=f7,f9,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (8)
  // H2 = H1 + d1 * H1 in f9
  // (p6) fma.s1 f9=f10,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (9)
  // S2 = S1 + d1 * S1 in f7
  // (p6) fma.s1 f7=f10,f7,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f7, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (10)
  // d2 = 1/2 - S2 * H2 in f10
  // (p6) fnma.s1 f10=f7,f9,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (11)
  // e2 = a - S2 * S2 in f8
  // (p6) fnma.s1 f8=f7,f7,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (12)
  // S3 = S2 + e2 * H2 in f7
  // (p6) fma.s1 f7=f8,f9,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f9, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (13)
  // H3 = H2 + d2 * H2 in f9
  // (p6) fma.s1 f9=f10,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (14)
  // e3 = a - S3 * S3 in f8
  // (p6) fnma.s1 f8=f7,f7,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (15)
  // S = S3 + e3 * H3 in f7
  // (p6) fma.s0 f7=f8,f9,f7
  Build_OP(TOP_fma, fr, p6, Gen_Enum_TN(ECV_sf_s0), f8, f9, f7, ops);
  f7 = fr;

  // END DOUBLE EXTENDED SQUARE ROOT ALGORITHM

  Exp_COPY(result, f7, ops);
}


static void
Expand_Intel_Max_Thr_F8_Sqrt(TN *result, TN *src, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN * const f6 = src;	// load the argument a in f6
  TN *f7, *f8, *f9, *f10, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *r2;
  TN *t1;

  // BEGIN DOUBLE PRECISION THROUGHPUT-OPTIMIZED SQUARE ROOT ALGORITHM

  // exponent of +1/2 in r2
  // movl r2 = 0x0fffe;;
  t1 = Gen_Literal_TN(0x0fffe, 8);
  r2 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r2, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r2, TN_value(t1));
  // +1/2 in f10
  // setf.exp f10 = r2
  f10 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_exp, f10, p0, r2, ops);
  // Step (1)
  // y0 = 1/sqrt(a) in f7
  // frsqrta.s0 f7,p6=f6
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  f7 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_frsqrta, f7, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, ops);
  fr = f7;
  // Step (2)
  // H0 = +1/2 * y0 in f8
  // (p6) fma.s1 f8=f10,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (3)
  // G0 = a * y0 in f7
  // (p6) fma.s1 f7=f6,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (4)
  // r0 = 1/2 - G0 * H0 in f9
  // (p6) fnma.s1 f9=f7,f8,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (5)
  // H1 = H0 + r0 * H0 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (6)
  // G1 = G0 + r0 * G0 in f7
  // (p6) fma.s1 f7=f9,f7,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f7, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (7)
  // r1 = 1/2 - G1 * H1 in f9
  // (p6) fnma.s1 f9=f7,f8,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (8)
  // H2 = H1 + r1 * H1 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (9)
  // G2 = G1 + r1 * G1 in f7
  // (p6) fma.s1 f7=f9,f7,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f7, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (10)
  // d2 = a - G2 * G2 in f9
  // (p6) fnma.s1 f9=f7,f7,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (11)
  // G3 = G2 + d2 * H2 in f8
  // (p6) fma.s1 f7=f9,f8,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (12)
  // d3 = a - G3 * G3 in f7
  // (p6) fnma.s1 f9=f7,f7,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F8);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (13)
  // S = G3 + d3 * H2 in f7
  // (p6) fma.d.s0 f7=f9,f8,f7
  Build_OP(TOP_fma_d, fr, p6, Gen_Enum_TN(ECV_sf_s0), f9, f8, f7, ops);
  f7 = fr;

  // END DOUBLE PRECISION THROUGHPUT-OPTIMIZED SQUARE ROOT ALGORITHM

  Exp_COPY(result, f7, ops);
}


static void
Expand_Intel_Max_Thr_F4_Sqrt(TN *result, TN *src, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN * const f6 = src;	// load the argument a in f6
  TN *f7, *f8, *f9, *f10, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *r2;
  TN *t1;

  // BEGIN SINGLE PRECISION THROUGHPUT-OPTIMIZED SQUARE ROOT ALGORITHM

  // exponent of +1/2 in r2
  // movl r2 = 0x0fffe;;
  t1 = Gen_Literal_TN(0x0fffe, 8);
  r2 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r2, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r2, TN_value(t1));
  // +1/2 in f8
  // setf.exp f8 = r2
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_exp, f8, p0, r2, ops);
  // Step (1)
  // y0 = 1/sqrt(a) in f7
  // frsqrta.s0 f7,p6=f6
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  f7 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_frsqrta, f7, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, ops);
  fr = f7;
  // Step (2)
  // H0 = 1/2 * y0 in f9
  // (p6) fma.s1 f9=f8,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (3)
  // S0 = a * y0 in f7
  // (p6) fma.s1 f7=f6,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (4)
  // d = 1/2 - S0 * H0 in f10
  // (p6) fnma.s1 f10=f7,f9,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (5)
  // d' = d + 1/2 * d in f8
  // (p6) fma.s1 f8=f8,f10,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f10, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (6)
  // e = d + d * d' in f8
  // (p6) fma.s1 f8=f10,f8,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f8, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (7)
  // S1 = S0 + e * S0 in f7
  // (p6) fma.s.s1 f7=f8,f7,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma_s, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f7, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (8)
  // H1 = H0 + e * H0 in f8
  // (p6) fma.s1 f8=f8,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (9)
  // d1 = a - S1 * S1 in f9
  // (p6) fnma.s1 f9=f7,f7,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (10)
  // S = S1 + d1 * H1 in f7
  // (p6) fma.s.s0 f7=f9,f8,f7
  Build_OP(TOP_fma_s, fr, p6, Gen_Enum_TN(ECV_sf_s0), f9, f8, f7, ops);
  f7 = fr;

  // END SINGLE PRECISION THROUGHPUT-OPTIMIZED SQUARE ROOT ALGORITHM

  Exp_COPY(result, f7, ops);
}


static void
Expand_Intel_Min_Lat_F8_Sqrt(TN *result, TN *src, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN * const f6 = src;	// load the argument a in f6
  TN *f7, *f8, *f9, *f10, *f11, *f12, *f13, *f14, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *r2, *r3;
  TN *t1;

  // BEGIN DOUBLE PRECISION LATENCY-OPTIMIZED SQUARE ROOT ALGORITHM

  // exponent of +1/2 in r2
  // movl r2 = 0x0fffe;;
  t1 = Gen_Literal_TN(0x0fffe, 8);
  r2 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r2, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r2, TN_value(t1));
  // +1/2 in f9
  // setf.exp f9 = r2
  f9 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_exp, f9, p0, r2, ops);
  // 3/2 in r3
  // movl r3=0x3fc00000;;
  // setf.s f10=r3
  t1 = Gen_Literal_TN(0x3fc00000, 8);
  r3 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r3, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r3, TN_value(t1));
  f10 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_s, f10, p0, r3, ops);
  // Step (1)
  // y0 = 1/sqrt(a) in f7
  // frsqrta.s0 f7,p6=f6
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  f7 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_frsqrta, f7, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, ops);
  fr = f7;
  // 5/2 in r2
  // movl r2 = 0x40200000
  // setf.s f11=r2
  t1 = Gen_Literal_TN(0x40200000, 8);
  r2 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r2, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r2, TN_value(t1));
  f11 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_s, f11, p0, r2, ops);
  // 63/8 in r3
  // movl r3 = 0x40fc0000;;
  // setf.s f12=r3
  t1 = Gen_Literal_TN(0x40fc0000, 8);
  r3 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r3, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r3, TN_value(t1));
  f12 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_s, f12, p0, r3, ops);
  // Step (2)
  // h = +1/2 * y0 in f8
  // (p6) fma.s1 f8=f9,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (3)
  // g = a * y0 in f7
  // (p6) fma.s1 f7=f6,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // 231/16 in r2
  // movl r2 = 0x41670000;;
  // setf.s f13=r2
  t1 = Gen_Literal_TN(0x41670000, 8);
  r2 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r2, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r2, TN_value(t1));
  f13 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_s, f13, p0, r2, ops);
  // Step (4)
  // e = 1/2 - g * h in f9
  // (p6) fnma.s1 f9=f7,f8,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // 35/8 in r3
  // movl r3 = 0x408c0000;;
  // setf.s f14=r3
  t1 = Gen_Literal_TN(0x408c0000, 8);
  r3 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r3, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r3, TN_value(t1));
  f14 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_s, f14, p0, r3, ops);
  // Step (5)
  // S0 = 3/2 + 5/2 * e in f10
  // (p6) fma.s1 f10=f11,f9,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f9, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (6)
  // e2 = e * e in f11
  // (p6) fma.s1 f11=f9,f9,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f9, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f11 = t1;
  // Step (7)
  // t = 63/8 + 231/16 * e in f12
  // (p6) fma.s1 f12=f13,f9,f12
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f13, f9, f12, ops);
  RESET_COND_DEF_LAST(ops);
  f12 = t1;
  // Step (8)
  // S1 = e + e2 * S0 in f10
  // (p6) fma.s1 f10=f11,f10,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f10, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (9)
  // e4 = e2 * e2 in f11
  // (p6) fma.s1 f11=f11,f11,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f11, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f11 = t1;
  // Step (10)
  // t1 = 35/8 + e * t in f9
  // (p6) fma.s1 f9=f9,f12,f14
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f12, f14, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (11)
  // G = g + S1 * g in f12
  // (p6) fma.s1 f12=f10,f7,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f7, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f12 = t1;
  // Step (12)
  // E = g * e4 in f7
  // (p6) fma.s1 f7=f7,f11,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f11, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (13)
  // u = S1 + e4 * t1 in f10
  // (p6) fma.s1 f10=f11,f9,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f9, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (14)
  // g1 = G + t1 * E in f7
  // (p6) fma.d.s1 f7=f9,f7,f12
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma_d, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f7, f12, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (15)
  // h1 = h + u * h in f8
  // (p6) fma.s1 f8=f10,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (16)
  // d = a - g1 * g1 in f9
  // (p6) fnma.s1 f9=f7,f7,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (17)
  // S = g1 + d * h1 in f7
  // (p6) fma.d.s0 f7=f9,f8,f7
  Build_OP(TOP_fma_d, fr, p6, Gen_Enum_TN(ECV_sf_s0), f9, f8, f7, ops);
  f7 = fr;

  // END DOUBLE PRECISION LATENCY-OPTIMIZED SQUARE ROOT ALGORITHM

  Exp_COPY(result, f7, ops);
}


static void
Expand_Intel_Min_Lat_F4_Sqrt(TN *result, TN *src, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN * const f6 = src;	// load the argument a in f6
  TN *f7, *f8, *f9, *f10, *f11, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *r2;
  TN *t1;

  // BEGIN SINGLE PRECISION LATENCY-OPTIMIZED SQUARE ROOT ALGORITHM

  // exponent of +1/2 in r2
  // movl r2 = 0x0fffe;;
  t1 = Gen_Literal_TN(0x0fffe, 8);
  r2 = Build_TN_Of_Mtype(MTYPE_I8);
  Build_OP(TOP_movl, r2, p0, t1, ops);
  CGSPILL_Attach_Intconst_Remat(r2, TN_value(t1));
  // +1/2 in f8
  // setf.exp f8 = r2
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_setf_exp, f8, p0, r2, ops);
  // Step (1)
  // y0 = 1/sqrt(a) in f7
  // frsqrta.s0 f7,p6=f6
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  f7 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_frsqrta, f7, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, ops);
  fr = f7;
  // Step (2)
  // H0 = 1/2 * y0 in f9
  // (p6) fma.s1 f9=f8,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (3)
  // S0 = a * y0 in f7
  // (p6) fma.s1 f7=f6,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (4)
  // d = 1/2 - S0 * H0 in f10
  // (p6) fnma.s1 f10=f7,f9,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (0'')
  // 3/2 = 1 + 1/2 in f8
  // (p6) fma.s1 f8=f8,f1,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f1, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (5)
  // e = 1 + 3/2 * d in f8
  // (p6) fma.s1 f8=f8,f10,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f10, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (6)
  // T0 = d * S0 in f11
  // (p6) fma.s1 f11=f10,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f11 = t1;
  // Step (7)
  // G0 = d * H0 in f10
  // (p6) fma.s1 f10=f10,f9,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f9, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (8)
  // S1 = S0 + e * T0 in f7
  // (p6) fma.s.s1 f7=f8,f11,f7
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma_s, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f11, f7, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (9)
  // H1 = H0 + e * G0 in f8
  // (p6) fma.s1 f8=f8,f10,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f10, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (10)
  // d1 = a - S1 * S1 in f9
  // (p6) fnma.s1 f9=f7,f7,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (11)
  // S = S1 + d1 * H1 in f7
  // (p6) fma.s.s0 f7=f9,f8,f7
  Build_OP(TOP_fma_s, fr, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f7, ops);
  f7 = fr;

  // END SINGLE PRECISION LATENCY-OPTIMIZED SQUARE ROOT ALGORITHM

  Exp_COPY(result, f7, ops);
}


static void
Expand_Intel_Max_Thr_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  switch (mtype) {
  case MTYPE_F4:
    Expand_Intel_Max_Thr_F4_Sqrt(result, src, ops);
    break;
  case MTYPE_F8:
    Expand_Intel_Max_Thr_F8_Sqrt(result, src, ops);
    break;
  case MTYPE_F10:
    Expand_Intel_F10_Sqrt(result, src, ops);
    break;
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("Bad type in Expand_Intel_Max_Thr_Sqrt"));
    /*NOTREACHED*/
  }
}


static void
Expand_Intel_Min_Lat_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  switch (mtype) {
  case MTYPE_F4:
    Expand_Intel_Min_Lat_F4_Sqrt(result, src, ops);
    break;
  case MTYPE_F8:
    Expand_Intel_Min_Lat_F8_Sqrt(result, src, ops);
    break;
  case MTYPE_F10:
    Expand_Intel_F10_Sqrt(result, src, ops);
    break;
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("Bad type in Expand_Intel_Min_Lat_Sqrt"));
    /*NOTREACHED*/
  }
}


void
Expand_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  static BOOL initialized;
  static void (*exp_sqrt)(TN *, TN *, TYPE_ID, OPS *) = Expand_SGI_Sqrt;

  // SGI_Sqrt does not support F10
  if (mtype == MTYPE_F10) {
    Expand_Intel_F10_Sqrt(result, src, ops);
    return;
  }

  if (!initialized) {
    const char * const alg = CGEXP_sqrt_algorithm;
    if (strcasecmp(alg, "intel_max_thr") == 0) {
      exp_sqrt = Expand_Intel_Max_Thr_Sqrt;
    } else if (strcasecmp(alg, "intel_min_lat") == 0) {
      exp_sqrt = Expand_Intel_Min_Lat_Sqrt;
    } else if (strcasecmp(alg, "sgi") != 0) {
      DevWarn("invalid fdiv algorithm: %s", alg);
    }
    initialized = TRUE;
  }

  exp_sqrt(result, src, mtype, ops);
}


static void
Expand_Float_Compares(TOP cmp_opcode, TN *dest, TN *src1, TN *src2, OPS *ops, BOOL disable_exception = FALSE)
{
  TN* sf = (disable_exception == TRUE) ? Gen_Enum_TN(ECV_sf_s1) : Gen_Enum_TN(ECV_sf_s0);
  if (TN_register_class(dest) == ISA_REGISTER_CLASS_predicate) {
    // return result of comparison in a predicate register
    TN *p1 = dest;
    TN *p2 = Get_Complement_TN(dest);
    Build_OP (cmp_opcode, p1, p2, True_TN, sf, src1, src2, ops);
  } else {
    TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    Build_OP (cmp_opcode, p1, p2, True_TN, sf, src1, src2, ops);
    // can either do unconditional copy of 0,
    // or predicated copy of 0 followed by predicated copy of 1.
    // Expand_Copy (dest, Zero_TN, MTYPE_I8, ops);
    Build_OP (TOP_mov, dest, p2, Zero_TN, ops);
    Build_OP (TOP_mov_i, dest, p1, Gen_Literal_TN(1, 4), ops);
  }
}

void
Expand_Float_Less (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_fcmp_lt, dest, src1, src2, ops);
}

void
Expand_Float_Greater (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_fcmp_gt, dest, src1, src2, ops);
}

void
Expand_Float_Less_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_fcmp_le, dest, src1, src2, ops);
}

void
Expand_Float_Greater_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_fcmp_ge, dest, src1, src2, ops);
}

void
Expand_Float_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_fcmp_eq, dest, src1, src2, ops);
}

void
Expand_Float_Not_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_fcmp_neq, dest, src1, src2, ops);
}

void
Expand_Recip_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  /*	(p0) frsqrta.s0 f2,p2=src	# y = ~1/sqrt(x)
   *
   *	(p2) ldfd	f4=half		# f4 = 0.5
   *	(p2) fmpy.d.s1	f5=f4,src	# hx = 0.5*x
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s1   f2=f2,f6,f2	# y = y + y*z
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s1   f2=f2,f6,f2	# y = y + y*z
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s0   f2=f2,f6,f2	# result = y + y*z
   */

  INT i;
  TN *tn;
  TN *p2;
  TN *fr, *f2, *f3, *f4, *f5, *f6;
  BOOL is_double = MTYPE_is_size_double(mtype) != 0;
  TOP fnma = is_double ? TOP_fnma_d : TOP_fnma_s;
  TOP fma  = is_double ? TOP_fma_d  : TOP_fma_s;
  TOP fmpy = is_double ? TOP_fmpy_d : TOP_fmpy_s;
  const INT exp_bias = 0xffff;
  TN * const p0 = True_TN;

  p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  f2 = Build_TN_Of_Mtype (mtype);
  Build_OP(TOP_frsqrta, f2, p2, p0, Gen_Enum_TN(ECV_sf_s0), src, ops);
  fr = f2;

  // f4 = 0.5
  tn = Build_TN_Of_Mtype (MTYPE_I8);
  Build_OP(TOP_mov_i, tn, p0, Gen_Literal_TN(-1 + exp_bias, 4), ops);
  f4 = Build_TN_Of_Mtype (mtype);
  Build_OP(TOP_setf_exp, f4, p0, tn, ops);
  f5 = Build_TN_Of_Mtype (mtype);
  Build_OP(fmpy, f5, p2, Gen_Enum_TN(ECV_sf_s1), f4, src, ops);
  RESET_COND_DEF_LAST(ops);

  for (i = 0; ; ++i) {
    tn = Build_TN_Of_Mtype (mtype);
    Build_OP(fmpy, tn, p2, Gen_Enum_TN(ECV_sf_s1), f2, f2, ops);
    RESET_COND_DEF_LAST(ops);
    f3 = tn;
    tn = Build_TN_Of_Mtype (mtype);
    Build_OP(fnma, tn, p2, Gen_Enum_TN(ECV_sf_s1), f5, f3, f4, ops);
    RESET_COND_DEF_LAST(ops);
    f6 = tn;

    // We were originally stopping for floats after 2 iterations,
    // but that was not accurate enough in some cases (see pv 810256)
    // so now do all 3 iterations for all sizes.
    if (i == 2) break;

    tn = Build_TN_Of_Mtype (mtype);
    Build_OP(fma, tn, p2, Gen_Enum_TN(ECV_sf_s1), f2, f6, f2, ops);
    RESET_COND_DEF_LAST(ops);
    f2 = tn;
  }

  Build_OP(fma, fr, p2, Gen_Enum_TN(ECV_sf_s1), f2, f6, f2, ops);
  Exp_COPY(result, f2, ops);
}

void
Expand_Flop (OPCODE opcode, TN *result, TN *src1, TN *src2, TN *src3, OPS *ops)
{
  TOP opc;

  switch (opcode) {
  case OPC_F4ADD:
    opc = TOP_fadd_s;
    break;
  case OPC_F8ADD:
    opc = TOP_fadd_d;
    break;
  case OPC_F10ADD:
    opc = TOP_fadd;
    break;
  case OPC_F4SUB:
    opc = TOP_fsub_s;
    break;
  case OPC_F8SUB:
    opc = TOP_fsub_d;
    break;
  case OPC_F10SUB:
    opc = TOP_fsub;
    break;
  case OPC_F4MPY:
    opc = TOP_fmpy_s;
    break;
  case OPC_F8MPY:
    opc = TOP_fmpy_d;
    break;
  case OPC_F10MPY:
    opc = TOP_fmpy;
    break;
  case OPC_F4MADD:	// (src2 * src3) + src1
    opc = TOP_fma_s;
    break;
  case OPC_F4NMADD:	// -((src2 * src3) + src1)
    {
      TN *tn = Build_TN_Of_Mtype (MTYPE_F4);
      Build_OP(TOP_fneg, tn, True_TN, src1, ops);
      src1 = tn;
      opc = TOP_fnma_s;
    }
    break;
  case OPC_F4MSUB:	// (src2 * src3) - src1
    opc = TOP_fms_s;
    break;
  case OPC_F4NMSUB:	// -((src2 * src3) - src1)
    opc = TOP_fnma_s;
    break;
  case OPC_F8MADD:	// (src2 * src3) + src1
    opc = TOP_fma_d;
    break;
  case OPC_F8NMADD:	// -((src2 * src3) + src1)
    {
      TN *tn = Build_TN_Of_Mtype (MTYPE_F8);
      Build_OP(TOP_fneg, tn, True_TN, src1, ops);
      src1 = tn;
      opc = TOP_fnma_d;
    }
    break;
  case OPC_F8MSUB:	// (src2 * src3) - src1
    opc = TOP_fms_d;
    break;
  case OPC_F8NMSUB:	// -((src2 * src3) - src1)
    opc = TOP_fnma_d;
    break;
  case OPC_F10MADD:	// (src2 * src3) + src1
    opc = TOP_fma;
    break;
  case OPC_F10NMADD:	// -((src2 * src3) + src1)
    {
      TN *tn = Build_TN_Of_Mtype (MTYPE_F10);
      Build_OP(TOP_fneg, tn, True_TN, src1, ops);
      src1 = tn;
      opc = TOP_fnma;
    }
    break;
  case OPC_F10MSUB:	// (src2 * src3) - src1
    opc = TOP_fms;
    break;
  case OPC_F10NMSUB:	// -((src2 * src3) - src1)
    opc = TOP_fnma;
    break;
  case OPC_F4DIV:
  case OPC_F8DIV:
  case OPC_F10DIV:
    Expand_Float_Divide (result, src1, src2, OPCODE_rtype(opcode), ops);
    return;
  case OPC_F4RECIP:
  case OPC_F8RECIP:
  case OPC_F10RECIP:
    Expand_Float_Recip (result, src1, OPCODE_rtype(opcode), ops);
    return;
  case OPC_F4RSQRT:
  case OPC_F8RSQRT:
  case OPC_F10RSQRT:
    Expand_Recip_Sqrt (result, src1, OPCODE_rtype(opcode), ops);
    return;
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("unexpected opcode %s", OPCODE_name(opcode)));
    /*NOTREACHED*/
  }
  if (TOP_is_madd(opc)) {
    Build_OP(opc, result, True_TN, Gen_Enum_TN(ECV_sf_s0), src2, src3, src1, ops);
  } else {
    Build_OP(opc, result, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
  }
}


/* Initialize the tracing flags for the expander. */
extern void
Init_CG_Expand (void)
{
  static BOOL Initialized = FALSE;

  // per PU:
  Trace_Exp = Get_Trace (TP_CGEXP, 1);
  /* whirl2ops uses -ttexp:2 */
  Trace_Exp2 = Get_Trace (TP_CGEXP, 4);
  /* calls.c use -ttexp:64 */

  if (Initialized) return;
  Initialized = TRUE;
  // once per file:
  Initialize_Branch_Variants();
}


/* ======================================================================
 * Exp_COPY
 * 
 * Generate a register transfer copy from 'src_tn' to 'tgt_tn'. 
 * ======================================================================*/
void 
Exp_COPY (TN *tgt_tn, TN *src_tn, OPS *ops)
{
  if ( TN_is_constant(src_tn) ) {
    FmtAssert (TN_has_value(src_tn), ("Exp_COPY: illegal source tn"));
    /* expansion for INTCONST doesn't depend on size */
    Exp_OP1 (OPC_I8INTCONST, tgt_tn, src_tn, ops);
  }
  else {
    ISA_REGISTER_CLASS tgt_rc = TN_register_class(tgt_tn);
    ISA_REGISTER_CLASS src_rc = TN_register_class(src_tn);

    if (tgt_rc != src_rc) {

      /* If a float rc is involved, figure out if it's a double.
       * For other rc's, it just doesn't matter.
       */
      BOOL is_double = FALSE;
      if (   (tgt_rc == ISA_REGISTER_CLASS_float && TN_size(tgt_tn) == 8)
	  || (src_rc == ISA_REGISTER_CLASS_float && TN_size(src_tn) == 8)) {
	is_double = TRUE;
      }

      TOP opc = CGTARG_Inter_RegClass_Copy(tgt_rc, src_rc, is_double);

      FmtAssert(opc != TOP_UNDEFINED, ("NYI: Exp_COPY inter-class copy for %s to %s",
		ISA_REGISTER_CLASS_INFO_Name(ISA_REGISTER_CLASS_Info(src_rc)),
		ISA_REGISTER_CLASS_INFO_Name(ISA_REGISTER_CLASS_Info(tgt_rc))));

      Build_OP(opc, tgt_tn, True_TN, src_tn, ops);
    } else if (tgt_rc == ISA_REGISTER_CLASS_float) {
      Build_OP (TOP_mov_f, tgt_tn, True_TN, src_tn, ops);
      Set_OP_copy (OPS_last(ops));
    } else if (tgt_rc == ISA_REGISTER_CLASS_integer) {
      Build_OP (TOP_mov, tgt_tn, True_TN, src_tn, ops);
      Set_OP_copy (OPS_last(ops));
    } else if (tgt_rc == ISA_REGISTER_CLASS_branch) {
      Build_OP (TOP_copy_br, tgt_tn, True_TN, src_tn, ops);
      Set_OP_copy (OPS_last(ops));
    } else if (tgt_rc == ISA_REGISTER_CLASS_predicate) {
      Build_OP(TOP_cmp_eq_unc,tgt_tn,True_TN,src_tn,Zero_TN,Zero_TN,ops);
      // I don't think it's safe to set OP_copy, but I'm not sure
    } else {
      #pragma mips_frequency_hint NEVER
      FmtAssert(FALSE, ("NYI: Exp_COPY intra-class copy for %s",
		ISA_REGISTER_CLASS_INFO_Name(ISA_REGISTER_CLASS_Info(tgt_rc))));
      /*NOTREACHED*/
    }

    if (Trace_Exp2) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "exp_copy into: ");
      Print_OP(OPS_last(ops));
    }
  }
}

void
Exp_Intrinsic_Op (INTRINSIC id, TN *result, TN *op0, TN * op1, OPS *ops)
{
  switch (id) {
  case INTRN_GETF_EXP:
    Build_OP (TOP_getf_exp, result, True_TN, op0, ops);
    break;
  case INTRN_GETF_SIG:
    Build_OP (TOP_getf_sig, result, True_TN, op0, ops);
    break;
  case INTRN_SETF_EXP:
    Build_OP (TOP_setf_exp, result, True_TN, op0, ops);
    break;
  case INTRN_SETF_SIG:
    Build_OP (TOP_setf_sig, result, True_TN, op0, ops);
    break;
  case INTRN_I4FFS:
    {
      /* Return the position of the first bit set in op0.  The least 
       * significant bit is position 1 and the most significant position 32.
       * Return 0 if no bits are set.
       *
       * For non-zero values of op0, we use popcnt to compute the bit
       * position. To do so, we adjust op0 so that it has the number of 
       * one bits set corresponding to the least significant bit set.
       * The subtract 1 (adds) and xor accomplish the conversion.
       */
      TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      TN *t1 = Build_TN_Of_Mtype (MTYPE_I4);
      TN *t2 = Build_TN_Of_Mtype (MTYPE_I4);
      Build_OP (TOP_cmp_eq, p1, p2, True_TN, op0, Zero_TN, ops);
      Build_OP (TOP_adds, t1, True_TN, Gen_Literal_TN(-1, 4), op0, ops);
      Build_OP (TOP_xor, t2, True_TN, t1, op0, ops);
      Build_OP (TOP_mov, result, p1, Zero_TN, ops);
      Build_OP (TOP_popcnt, result, p2, t2, ops);
    }
    break;
  case INTRN_ISGREATER:
    Expand_Float_Compares(TOP_fcmp_gt, result, op0, op1, ops, TRUE /* disable exception */);
    break;
  case INTRN_ISGREATEREQUAL:
    Expand_Float_Compares(TOP_fcmp_ge, result, op0, op1, ops, TRUE /* disable exception */);
    break;
  case INTRN_ISLESS:
    Expand_Float_Compares(TOP_fcmp_lt, result, op0, op1, ops, TRUE /* disable exception */);
    break;
  case INTRN_ISLESSEQUAL:
    Expand_Float_Compares(TOP_fcmp_le, result, op0, op1, ops, TRUE /* disable exception */);
    break;
  case INTRN_ISLESSGREATER:
    Expand_Float_Compares(TOP_fcmp_neq, result, op0, op1, ops, TRUE /* disable exception */); 
    break;
  case INTRN_ISUNORDERED:
    Expand_Float_Compares(TOP_fcmp_unord, result, op0, op1, ops, TRUE /* disable exception */);
    break;
  case INTRN_CLZ32:
    // expand the intrinsic __builtin_clz, which Returns the number of leading 0-bits in X, 
    // starting at the most significant bit position.
    //
    {
      TN* t1 = Build_TN_Of_Mtype (MTYPE_F8);
      TN* t2 = Build_TN_Of_Mtype (MTYPE_F8);
      TN* t3 = Build_TN_Of_Mtype (MTYPE_I4);
      Build_OP (TOP_setf_sig, t1, True_TN, op0, ops);
      Build_OP (TOP_fcvt_xuf, t2, True_TN, Gen_Enum_TN(ECV_sf_s0), t1, ops);
      Build_OP (TOP_getf_exp, t3, True_TN, t2, ops);
      Build_OP (TOP_mov_i, result, True_TN, Gen_Literal_TN(65598, 4), ops);
      Build_OP (TOP_sub, result, True_TN, result, t3, ops);
      Build_OP (TOP_adds, result, True_TN, Gen_Literal_TN(-32, 4), result, ops);
    }
    break;
  case INTRN_CTZ:
    // Bug fix for OSP_433
    // expand the intrinsic __builtin_ctzl, which returns the bit index of the least significant bit
    //
    {
      TN* tn1 = Build_TN_Of_Mtype (MTYPE_I8);
      Build_OP (TOP_adds, tn1, True_TN, Gen_Literal_TN(-1, 8), op0, ops);
      Build_OP (TOP_andcm_i, op0, True_TN, Gen_Literal_TN(-1, 8), op0, ops);
      Build_OP (TOP_and, op0, True_TN, op0, tn1, ops);
      Build_OP (TOP_popcnt, result, True_TN, op0, ops);
    }
    break;
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert (FALSE, ("WHIRL_To_OPs: illegal intrinsic op"));
    /*NOTREACHED*/
  }
}

/* ======================================================================
 * Expand_TOP_intrncall
 * 
 * Given a TOP_intrncall <op>, expand it into the sequence of instructions 
 * that must be generated. If <get_sequence_length> is TRUE, return only
 * the number of instructions in the sequence and don't actually do the 
 * expansion.
 * ======================================================================*/
static INT
Expand_TOP_intrncall (
  const OP *op, 
  OPS *ops, 
  BOOL get_sequence_length,
  INT pc_value)
{
  FmtAssert(FALSE, ("Expand_TOP_intrncall NYI"));
  /*NOTREACHED*/
}

static TYPE_ID
Get_Intrinsic_Size_Mtype (INTRINSIC id)
{
  switch (id) {
  case INTRN_BOOL_COMPARE_AND_SWAP_I4:
  case INTRN_COMPARE_AND_SWAP_I4:
  case INTRN_LOCK_TEST_AND_SET_I4:
  case INTRN_LOCK_RELEASE_I4:
  case INTRN_FETCH_AND_ADD_I4:
  case INTRN_ADD_AND_FETCH_I4:
  case INTRN_SUB_AND_FETCH_I4:
  case INTRN_OR_AND_FETCH_I4:
  case INTRN_XOR_AND_FETCH_I4:
  case INTRN_AND_AND_FETCH_I4:
  case INTRN_NAND_AND_FETCH_I4:
  case INTRN_FETCH_AND_SUB_I4:
  case INTRN_FETCH_AND_OR_I4:
  case INTRN_FETCH_AND_XOR_I4:
  case INTRN_FETCH_AND_AND_I4:
  case INTRN_FETCH_AND_NAND_I4:
	return MTYPE_I4;
  case INTRN_BOOL_COMPARE_AND_SWAP_I8:
  case INTRN_COMPARE_AND_SWAP_I8:
  case INTRN_LOCK_TEST_AND_SET_I8:
  case INTRN_LOCK_RELEASE_I8:
  case INTRN_FETCH_AND_ADD_I8:
  case INTRN_ADD_AND_FETCH_I8:
  case INTRN_SUB_AND_FETCH_I8:
  case INTRN_OR_AND_FETCH_I8:
  case INTRN_XOR_AND_FETCH_I8:
  case INTRN_AND_AND_FETCH_I8:
  case INTRN_NAND_AND_FETCH_I8:
  case INTRN_FETCH_AND_SUB_I8:
  case INTRN_FETCH_AND_OR_I8:
  case INTRN_FETCH_AND_XOR_I8:
  case INTRN_FETCH_AND_AND_I8:
  case INTRN_FETCH_AND_NAND_I8:
  case INTRN_SYNCHRONIZE:
	return MTYPE_I8;
  default:
	#pragma mips_frequency_hint NEVER
  	FmtAssert(FALSE, ("Unexpected intrinsic %d", id));
	/*NOTREACHED*/
  }
}

static BOOL
Intrinsic_Returns_New_Value (INTRINSIC id)
{
  switch (id) {
  case INTRN_ADD_AND_FETCH_I4:
  case INTRN_SUB_AND_FETCH_I4:
  case INTRN_OR_AND_FETCH_I4:
  case INTRN_XOR_AND_FETCH_I4:
  case INTRN_AND_AND_FETCH_I4:
  case INTRN_NAND_AND_FETCH_I4:
  case INTRN_ADD_AND_FETCH_I8:
  case INTRN_SUB_AND_FETCH_I8:
  case INTRN_OR_AND_FETCH_I8:
  case INTRN_XOR_AND_FETCH_I8:
  case INTRN_AND_AND_FETCH_I8:
  case INTRN_NAND_AND_FETCH_I8:
	return TRUE;
  default:
	return FALSE;
  }
}

// initial expansion of intrinsic call (may not be complete lowering).
// return result TN (if set).
// If the intrinsic requires a label and loop (2 bb's)
// then ops is for first bb and ops2 is for bb after the label.
// Otherwise only ops is filled in.
TN *
Exp_Intrinsic_Call (INTRINSIC id, TN *op0, TN *op1, TN *op2, OPS *ops, 
	LABEL_IDX *label, OPS *loop_ops)
{
  TN *result;
  TN *ar_ccv;
  TN *p1;
  TN *p2;
  TOP top;
  TYPE_ID size_mtype = Get_Intrinsic_Size_Mtype (id);

  // expand these at cgexp time to avoid bundling issues 
  // when handling simulated ops in cgemit.

  if (id == INTRN_BOOL_COMPARE_AND_SWAP_I4 || id == INTRN_BOOL_COMPARE_AND_SWAP_I8 ||
      id == INTRN_COMPARE_AND_SWAP_I4      || id == INTRN_COMPARE_AND_SWAP_I8) {
    //
    // compare_and_swap (type* ptr, type oldvalue, type newvalue)
    //      if (*ptr != oldvalue) return 0;
    //      else {
    //              *ptr = newvalue;
    //              return 1;
    //      }
    // where op0 == ptr, op1 == oldvalue, op2 == newvalue
    result = Build_TN_Of_Mtype (MTYPE_I8);
    ar_ccv = Build_Dedicated_TN ( ISA_REGISTER_CLASS_application,
                                 (REGISTER)(REGISTER_MIN + 32),
                                 8);
    Build_OP (TOP_mov_t_ar_r, ar_ccv, True_TN, op1, ops);
    Build_OP (TOP_mf, True_TN, ops);        // memory fence
    if (size_mtype == MTYPE_I4)
      top = TOP_cmpxchg4;
    else
      top = TOP_cmpxchg8;
    Build_OP (top, result, True_TN, Gen_Enum_TN(ECV_sem_rel),
              Gen_Enum_TN(ECV_ldhint_nta), op0, op2, ops);
    p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    Build_OP (TOP_cmp_eq, p1, p2, True_TN, result, op1, ops);
    Build_OP (TOP_mov_i, result, p1, Gen_Literal_TN(1, 4), ops);
    Build_OP (TOP_mov, result, p2, Zero_TN, ops);  
    return result;
  }

  else if (id == INTRN_LOCK_TEST_AND_SET_I4 || id == INTRN_LOCK_TEST_AND_SET_I8) {
    // type __lock_test_and_set (type* ptr, type value, ...)
    // atomically store the supplied value in *ptr
    // and return the old value of *ptr
    // i.e. { tmp = *ptr; *ptr = value; return tmp; }
    // op0 == ptr, op1 == value
    if (size_mtype == MTYPE_I4)
      top = TOP_xchg4;
    else
      top = TOP_xchg8;
    result = Build_TN_Of_Mtype (MTYPE_I8);
    Build_OP (top, result, True_TN, Gen_Enum_TN(ECV_ldhint_nta),
              op0, op1, ops);
    return result;
  }

  else if (id == INTRN_LOCK_RELEASE_I4 || id == INTRN_LOCK_RELEASE_I8) {
    //  "void __lock_release (type* ptr, ...)"
    // Set *ptr to 0.  (i.e.) { *ptr = 0 }
    // op0 == ptr
    if (size_mtype == MTYPE_I4)
      top = TOP_st4;
    else
      top = TOP_st8;
    Build_OP (top, True_TN,
              Gen_Enum_TN(ECV_sttype_rel),
              Gen_Enum_TN(ECV_sthint_nta),
              op0, Zero_TN, ops);  
    return result;
  }

  else if (id == INTRN_SYNCHRONIZE) {
    Build_OP (TOP_mf, True_TN, ops);    // memory fence
    return result;
  }
  
  // fetch_and_add (type *ptr, type value, ...)
  // tmp = *ptr; *ptr += value; return tmp;
  // op0 = ptr, op1 = value
  // fetchadd inst only takes +/- 1,4,8,16
  else if((id == INTRN_FETCH_AND_ADD_I4 || id == INTRN_FETCH_AND_ADD_I8) &&
          (TN_is_constant(op1) && (TN_value(op1) == 1 || TN_value(op1) == -1 ||
           TN_value(op1) == 4 || TN_value(op1) == -4 || TN_value(op1) == 8 ||
           TN_value(op1) == -8 || TN_value(op1) == 16 || TN_value(op1) == -16 ))){
    Build_OP (TOP_mf, True_TN, ops);        // memory fence
    if (size_mtype == MTYPE_I4)
      top = TOP_fetchadd4;
    else
      top = TOP_fetchadd8;
    result = Build_TN_Of_Mtype (MTYPE_I8);
    Build_OP (top, result, True_TN,
              Gen_Enum_TN(ECV_sem_rel),
              Gen_Enum_TN(ECV_ldhint),
              op0, op1, ops);
    return result;
  }

  else {
    // tmp == new *ptr, result == old *ptr
    TN *orig_value = Build_TN_Of_Mtype (size_mtype);
    TN *old_value = Build_TN_Of_Mtype (size_mtype);
    TN *new_value = Build_TN_Of_Mtype (size_mtype); 
    // immed doesn't fit, so do loop case
    Expand_Load (
            // load is of address, not of result type
            OPCODE_make_op(OPR_LDID, Pointer_Mtype, Pointer_Mtype),
            orig_value, op0, Gen_Literal_TN (0, 4), V_NONE, ops);

    *label = Gen_Temp_Label();
    if(id == INTRN_FETCH_AND_ADD_I4 || id == INTRN_FETCH_AND_ADD_I8 ||
       id == INTRN_ADD_AND_FETCH_I4 || id == INTRN_ADD_AND_FETCH_I8){
      Expand_Add (new_value, orig_value, op1, size_mtype, loop_ops);
    }
    else if(id == INTRN_FETCH_AND_SUB_I4 || id == INTRN_FETCH_AND_SUB_I8 ||
            id == INTRN_SUB_AND_FETCH_I4 || id == INTRN_SUB_AND_FETCH_I8){
      Expand_Sub (new_value, orig_value, op1, size_mtype, loop_ops);
    }
    else if(id == INTRN_FETCH_AND_OR_I4 || id == INTRN_FETCH_AND_OR_I8 ||
            id == INTRN_OR_AND_FETCH_I4 || id == INTRN_OR_AND_FETCH_I8){
      Expand_Binary_Or (new_value, orig_value, op1, size_mtype, loop_ops);
    }
    else if(id == INTRN_FETCH_AND_AND_I4 || id == INTRN_FETCH_AND_AND_I8 ||
            id == INTRN_AND_AND_FETCH_I4 || id == INTRN_AND_AND_FETCH_I8){
      Expand_Binary_And (new_value, orig_value, op1, size_mtype, loop_ops);
    }
    else if(id == INTRN_FETCH_AND_XOR_I4 || id == INTRN_FETCH_AND_XOR_I8 ||
            id == INTRN_XOR_AND_FETCH_I4 || id == INTRN_XOR_AND_FETCH_I8){
      Expand_Binary_Xor (new_value, orig_value, op1, size_mtype, loop_ops);
    }
    else if(id == INTRN_FETCH_AND_NAND_I4 || id == INTRN_FETCH_AND_NAND_I8 ||
            id == INTRN_NAND_AND_FETCH_I4 || id == INTRN_NAND_AND_FETCH_I8){
      Expand_Binary_Nand (new_value, orig_value, op1, size_mtype, loop_ops);
    }
    else{
      #pragma mips_frequency_hint NEVER
      FmtAssert (FALSE, ("WHIRL_To_OPs: illegal intrinsic call"));
    }
    ar_ccv = Build_Dedicated_TN ( ISA_REGISTER_CLASS_application,
                                 (REGISTER)(REGISTER_MIN + 32),
                                 8);
    Build_OP (TOP_mov_t_ar_r, ar_ccv, True_TN, orig_value, loop_ops);
    Build_OP (TOP_mf, True_TN, loop_ops);   // memory fence
    if (size_mtype == MTYPE_I4)
      top = TOP_cmpxchg4;
    else
      top = TOP_cmpxchg8;
    Build_OP (top, old_value, True_TN, Gen_Enum_TN(ECV_sem_rel),
              Gen_Enum_TN(ECV_ldhint), op0, new_value, loop_ops);
    p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    Build_OP (TOP_cmp_eq, p1, p2, True_TN, old_value, orig_value, loop_ops);
    if (Intrinsic_Returns_New_Value(id)){
      Build_OP (TOP_mov, orig_value, p2, new_value, loop_ops);
      result = new_value;
    }
    else {
      Build_OP (TOP_mov, orig_value, p2, old_value, loop_ops);
      result = old_value;
    }
    Build_OP (TOP_br_cond, p2,
              Gen_Enum_TN(ECV_bwh_dptk),
              Gen_Enum_TN(ECV_ph_few),
              Gen_Enum_TN(ECV_dh),
              Gen_Label_TN(*label,0), loop_ops);
    return result;
  }
}


/* ======================================================================
 * Exp_Simulated_Op
 *
 * Given a simulated <op>, expand it into the sequence of instructions
 * supported by the target.
 * ======================================================================*/
void Exp_Simulated_Op(const OP *op, OPS *ops, INT pc_value)
{
  DevWarn("Exp_Simulated_Op should not be reached");
  switch (OP_code(op)) {
  case TOP_intrncall:
    Expand_TOP_intrncall(op, ops, FALSE, pc_value);
    break;
  case TOP_spadjust:
    // spadjust should only show up for alloca/dealloca
    if (OP_spadjust_plus(op)) {
	// dealloca does copy of kid to $sp (op1 is old sp value)
	Expand_Copy (OP_result(op,0), OP_opnd(op,2), Pointer_Mtype, ops);
    } else {
	Expand_Sub (OP_result(op,0), OP_opnd(op,1), OP_opnd(op,2), Pointer_Mtype, ops);
    }
    {
	// copy predicate to new copy/sub ops
	OP *newop;
	FOR_ALL_OPS_OPs(ops, newop) {
		if (OP_has_predicate(newop))
			Set_OP_opnd (newop, OP_PREDICATE_OPND, 
				OP_opnd(op, OP_PREDICATE_OPND) );
	}
    }
    break;
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("simulated OP %s not handled", TOP_Name(OP_code(op))));
    /*NOTREACHED*/
  }
}


/* ======================================================================
 * Simulated_Op_Real_Ops
 *
 * Return the number of instructions that will be generated by Exp_Simulated_Op
 * ======================================================================*/
INT
Simulated_Op_Real_Ops(const OP *op)
{
  switch (OP_code(op)) {
  case TOP_intrncall:
    return Expand_TOP_intrncall (op, NULL, TRUE, 0);
  case TOP_spadjust:
    return 1;
  default:

    /* Anything other than the above is presumed to be removed by
     * emit time, therefore we just say the expansion generates 0 ops.
     * (we used to assert, but that isn't a good solution -- see pv 652898).
     */
    return 0;
  }
}


/* ======================================================================
 * Simulated_Op_Real_Inst_Words
 *
 * Return the number of instruction words that will ultimately be emitted
 * for the expansion generated by Exp_Simulated_Op
 * ======================================================================*/
INT
Simulated_Op_Real_Inst_Words(const OP *op)
{
    switch (OP_code(op)) {
    case TOP_spadjust:
	return 1;
    case TOP_asm:
	// this is a hack; will be a multiple of 3, but don't know
	// exact number.
	return 3;
    default:
    	// For IA-64, we should never emit a simulated OP, so just assert.
	#pragma mips_frequency_hint NEVER
    	FmtAssert(FALSE, ("shouldn't be calling Simulated_Op_Real_Inst_Words for %s", TOP_Name(OP_code(op)) ));
    	/*NOTREACHED*/
    }
}


/* ======================================================================
 * Exp_Is_Large_Stack_Sym
 *
 * determine if a given symbol is a stack relative reference that will
 * require multiple instructions to load or store.
 * ======================================================================*/
BOOL
Exp_Is_Large_Stack_Sym(ST* sym,  INT64 ofst)
{
  ST *base_sym;
  INT64 base_ofst;
  
  if (sym == NULL) {
    return FALSE;
  }

  Allocate_Object(sym);		/* make sure sym is allocated */

  Base_Symbol_And_Offset_For_Addressing (sym, ofst, &base_sym, &base_ofst);

  if (ST_on_stack(sym) && !ISA_LC_Value_In_Class(base_ofst, LC_i14)) {
    return TRUE;
  }
  return FALSE;
}

void
Exp_Noop (OPS *ops)
{
  Build_OP (CGTARG_Noop_Top(), True_TN, Gen_Literal_TN(0, 4), ops);
}

void 
Exp_Generic_Pred_Calc(TN* result1, TN *result2, COMPARE_TYPE ctype,
		      TN *qual_pred, OPS* ops)
{
  TOP pred_top;
  
  switch (ctype) {
  case COMPARE_TYPE_or:
    pred_top = TOP_cmp_eq_or;
    break;
  case COMPARE_TYPE_and:
    pred_top = TOP_cmp_ne_and;
    break;
  }
  Build_OP(pred_top, result1, result2, qual_pred, Zero_TN, Zero_TN, ops);
}


void
Exp_Pred_Calc(TN* result, OP* cmp_op, COMPARE_TYPE ctype, BOOL false_result,
	      OPS* ops)
{
  TOP pred_top;

  switch (ctype) {
  case COMPARE_TYPE_or:
  case COMPARE_TYPE_orcm:
  case COMPARE_TYPE_and:
  case COMPARE_TYPE_andcm:

    //
    // No direct parallel compares for floats and some general purpose
    // compares.  Use predicate result from current compare to control
    // a parallel "or" that will always set the compound predicate. 
    //
    pred_top = CGTARG_Parallel_Compare(cmp_op, ctype);
    if (pred_top == TOP_UNDEFINED) {
      TN* pred;
      switch (ctype) {
      case COMPARE_TYPE_or:
	pred = OP_result(cmp_op, 0);
	pred_top = TOP_cmp_eq_or;
	break;
      case COMPARE_TYPE_orcm:
	pred = OP_result(cmp_op, 1);
	pred_top = TOP_cmp_eq_or;
	break;
      case COMPARE_TYPE_and:
	pred = OP_result(cmp_op, 0);
	pred_top = TOP_cmp_eq_and;
	break;
      case COMPARE_TYPE_andcm:
	pred = OP_result(cmp_op, 1);
	pred_top = TOP_cmp_eq_and;
	break;
      }
      Build_OP(pred_top, result,
	       True_TN, pred,
	       Zero_TN, Zero_TN, ops);
    } else {
      Build_OP(pred_top, result, True_TN,
	       OP_opnd(cmp_op, OP_PREDICATE_OPND),
	       OP_opnd(cmp_op, OP_PREDICATE_OPND+1),
	       OP_opnd(cmp_op, OP_PREDICATE_OPND+2),
	       ops);
    }
    break;

  case COMPARE_TYPE_or_andcm:
  case COMPARE_TYPE_and_orcm:
    FmtAssert(0,("Exp_Pred_Calc NYI for or_andcm, and and_orcm compares."));
    break;

  case COMPARE_TYPE_unc:
    if (false_result) {
      if (TOP_is_flop(OP_code(cmp_op))) {
	Build_OP(OP_code(cmp_op), True_TN, result,
		 OP_opnd(cmp_op, OP_PREDICATE_OPND),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+1),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+2),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+3),
		 ops);
      } else {
	Build_OP(OP_code(cmp_op), True_TN, result,
		 OP_opnd(cmp_op, OP_PREDICATE_OPND),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+1),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+2),
		 ops);
      }
    } else {
      if (TOP_is_flop(OP_code(cmp_op))) {
	Build_OP(OP_code(cmp_op), result, True_TN,
		 OP_opnd(cmp_op, OP_PREDICATE_OPND),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+1),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+2),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+3),
		 ops);
      } else {
	Build_OP(OP_code(cmp_op), result, True_TN,
		 OP_opnd(cmp_op, OP_PREDICATE_OPND),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+1),
		 OP_opnd(cmp_op, OP_PREDICATE_OPND+2),
		 ops);
      }
    }
    break;
  }
}

///////////////////////////////////////////////////////////
//
// Setup the true_tn and false_tn for a BB. The true_tn is a TN such that
// it is true if the branch at the end of a BB is taken. If it false
// through the false_tn will be set true.
// 
// This routine works by trying to find the compare which generates the
// branch predicate for the block. Assuming it finds one, and it's of the
// right form (i.e. an unc form), it attempts to simply re-use the two TN's
// it generates. 
//
// Right now, if it doesn't find it, it asserts, but I don't think this is
// going to happen, given the current way we generate things.
//
// The above can happen if we are trying to generate the false predicate
// for a block that has a branch which came from a previous pass of
// hyperblock formation. In this case, we don't have a single defining
// compare. So if we have a predicate Pb, (which is the predicate used for
// the branch, we wan't Pf such that Pf is TRUE if Pb is false and the block
// is executed.  We can accomplish this by initializing Pf to 1 under the
// block predicate, and setting it to 0 if Pb is TRUE.
// 
void
Exp_True_False_Preds_For_Block(BB *bb, TN* &true_tn, TN * &false_tn) 
{
   COMPARE_TYPE comp_type;
   TN* tn1;
   TN* tn2;
   OP* compare_op;
   OP* br_op = BB_branch_op(bb);
   BOOL reusing_tns;
   VARIANT branch_variant;
   DEF_KIND kind;
 
   true_tn = NULL;
   false_tn = NULL;
   reusing_tns = FALSE;
   
   branch_variant = CGTARG_Analyze_Branch(br_op, &tn1, &tn2);
   Is_True(branch_variant == V_BR_P_TRUE,("Can't get predicates for block %d",BB_id(bb)));

   /* Try to find the compare op */
   compare_op = TN_Reaching_Value_At_Op(tn1, br_op, &kind, TRUE);
   if (compare_op && kind == VAL_KNOWN && !OP_cond_def(compare_op)) {
       //
       // This is the 99% case (maybe the 100% case, given the current
       // generation schemes). The result predicates are 100% defined, so we
       // can safely replace the opcode with the unconditional variant, and
       // then return the two result predicates in the appropriate slots.
       //
       reusing_tns = TRUE;
       OP_Change_Opcode(compare_op,CGTARG_Get_unc_Variant(OP_code(compare_op)));
       Set_OP_cond_def_kind(compare_op,OP_ALWAYS_UNC_DEF);

       true_tn = tn1;
       // Get the other result as the false_tn
       if (OP_result(compare_op,1) != tn1) {
	 false_tn = OP_result(compare_op,1);
       } else {
	 false_tn = OP_result(compare_op,0);
       }
   }

   if (!reusing_tns) {
     OPS ops = OPS_EMPTY;
     DevWarn("inserting non-optimal inverse predicate in BB %d",BB_id(bb));
     true_tn = tn1;
     // must create tns before Exp_Pred* if want to return them.
     false_tn = Gen_Predicate_TN();	
     Exp_Pred_Complement(false_tn, True_TN, true_tn, &ops);
     BB_Insert_Ops(bb,br_op,&ops,TRUE);
   }
   
   return;
}

void Expand_Const (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert( TN_is_symbol(src), ("Expand_Const: src not a symbol TN"));

  TCON tc = ST_tcon_val(TN_var(src));

  if (Targ_Is_Zero(tc)) {
    // copy 0
    Build_OP (TOP_mov_f, dest, True_TN, FZero_TN, ops);
    return;
  }

  if (MTYPE_is_float(mtype) && mtype == TCON_ty(tc)) {
    if (mtype == MTYPE_F4 || mtype == MTYPE_F8 || mtype == MTYPE_F10) {
      double val = Targ_To_Host_Float(tc);
      if (val == 1.0) {
	// copy 1
	Build_OP (TOP_mov_f, dest, True_TN, FOne_TN, ops);
	return;
      }
      if (val == -1.0) {
	// negate 1
	Build_OP (TOP_fneg, dest, True_TN, FOne_TN, ops);
	return;
      }
      if (val == 2.0) {
	// 1 + 1
	TOP fadd = (mtype == MTYPE_F4) ? TOP_fadd_s :
		(mtype == MTYPE_F8) ? TOP_fadd_d : TOP_fadd;
	Build_OP (fadd, dest, True_TN, Gen_Enum_TN(ECV_sf_s0), FOne_TN,
		FOne_TN, ops);
	return;
      }
    }
    if (CGEXP_float_consts_from_ints &&
	(mtype == MTYPE_F4 || mtype == MTYPE_F8)) {
      TN *tn = Build_TN_Of_Mtype (MTYPE_I8);
      INT64 fimm = (mtype == MTYPE_F4) ? TCON_uval(tc) : TCON_k0(tc);
      if (Targ_Is_Power_Of_Two(tc)) {
	// +/-2^n
	INT exp;
	INT sign;
	INT sign_exp;
	if (mtype == MTYPE_F4) {
	  exp = ((fimm >> 23) & 0xff) - 127;
	  sign = (fimm >> 31) & 0x1;
	} else {
	  exp = ((fimm >> 52) & 0x7ff) - 1023;
	  sign = (fimm >> 63) & 0x1;
	}
	sign_exp = (sign << 17) | (exp + 0xffff);
	Build_OP (TOP_mov_i, tn, True_TN, Gen_Literal_TN(sign_exp, 4), ops);
	CGSPILL_Attach_Intconst_Remat (tn, sign_exp);
	Build_OP (TOP_setf_exp, dest, True_TN, tn, ops);
	return;
      }
      if (mtype == MTYPE_F4 || mtype == MTYPE_F8) {
	// arbitrary floating constant
	TOP setf = (mtype == MTYPE_F4) ? TOP_setf_s : TOP_setf_d;
	Build_OP (TOP_movl, tn, True_TN, Gen_Literal_TN(fimm, 8), ops);
	CGSPILL_Attach_Intconst_Remat (tn, fimm);
	Build_OP (setf, dest, True_TN, tn, ops);
	return;
      }
    }
  }

  // load from memory
  Exp_Load (mtype, mtype, dest, TN_var(src), 0, ops, V_NONE);
}

BOOL
Target_Has_Immediate_Operand (WN *parent, WN *expr)
{
  if (WN_operator(parent) == OPR_INTRINSIC_CALL
	&& (((INTRINSIC) WN_intrinsic (parent) == INTRN_FETCH_AND_ADD_I4)
	 || ((INTRINSIC) WN_intrinsic (parent) == INTRN_FETCH_AND_ADD_I8)) )
  {
	// can optimize for some constants
	return TRUE;
  }
  if (WN_operator(parent) == OPR_SUB) {
	// can handle immediates in either operand
	return TRUE;
  }
  // default to false, which really means "don't know"
  return FALSE;
}

void 
Exp_Spadjust (TN *dest, TN *size, VARIANT variant, OPS *ops)
{
  Build_OP (TOP_spadjust, dest, True_TN, SP_TN, size, ops);
  switch (variant) {
  case V_SPADJUST_PLUS:
    Set_OP_spadjust_plus(OPS_last(ops));
    break;
  case V_SPADJUST_MINUS:
    Set_OP_spadjust_minus(OPS_last(ops));
    break;
  case V_NONE:
    break;
  default:
    Is_True(FALSE, ("bad variant (0x%llx) for Exp_Spadjust", (INT64)variant));
    break;
  }
}
