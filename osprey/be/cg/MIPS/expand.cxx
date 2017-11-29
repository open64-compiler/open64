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


/* ====================================================================
 * ====================================================================
 *
 * Module: expand.c
 * $Revision: 1.28 $
 * $Date: 2006/05/17 06:58:34 $
 * $Author: weitang $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/MIPS/expand.cxx,v $
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

#include <stdint.h>
#include "defs.h"
#include "config.h"
#include "erglob.h"
#include "ercg.h"
#include "glob.h"
#include "tracing.h"
#include "util.h"

#include "tn.h"
#include "cg_flags.h"
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
#include "targ_sim.h"   /* To generate stores of param registers in builtin_apply_args */

/* HD - for Build_LUT_Insn, I include the following */
#include <iostream>
#include <vector>
#include <stdlib.h>
#include <strings.h>
#include <map>
#include <fstream>
#include <string>
#include <ctype.h>
#include "glob.h"
#include "lib_phase_dir.h"

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

void
Expand_Copy (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  if (MTYPE_is_float(mtype))
    Build_OP (MTYPE_is_size_double(mtype)?TOP_mov_d: TOP_mov_s, 
	      result, src, ops);
  else
    Build_OP(TOP_or, result, src, Zero_TN, ops);            
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
	  TN *lit_tn;
	  FmtAssert (! MTYPE_float(mtype), ("Expand_Convert_Length: illegal data type\n"));
	  FmtAssert (TN_has_value(length_tn), ("Expand_Convert_Length: non-constant length\n"));
	  UINT64 val = TN_value(length_tn);
	  if (val <= 16 && ! signed_extension) 
            Build_OP(TOP_andi, dest, src, Gen_Literal_TN((1 << val) - 1, 4), ops);  
	  else if (MTYPE_is_size_double(mtype) || val == 32) {
	    if (val > 32) {
	    	TN *tmp = Build_TN_Of_Mtype(mtype);
	    	lit_tn = Gen_Literal_TN(64 - val, 4);
	   		Build_OP(TOP_dsll, tmp, src, lit_tn, ops);
	    	Build_OP(signed_extension ? TOP_dsra : TOP_dsrl, dest, tmp, lit_tn, ops);
	  	}
	 	else {
	          if(Pointer_Size==8) {
		  	TN *tmp = Build_TN_Of_Mtype(mtype);
		    	lit_tn = Gen_Literal_TN(32 - val, 4);
		    	Build_OP(TOP_dsll32, tmp, src, lit_tn, ops);
		    	Build_OP(signed_extension ? TOP_dsra32 : TOP_dsrl32, dest, tmp, lit_tn, ops);	    	
	 	  }
	         else{
		  /*
		  We need to copy the src_tn to dest, or the data dependence would be broken.
		  */
		      Exp_COPY(dest,src,ops);
	         }
	  	}
	  }
	  else {
	    TN *tmp = Build_TN_Of_Mtype(mtype);
	    lit_tn = Gen_Literal_TN(32 - val, 4);
	    Build_OP(TOP_sll, tmp, src, lit_tn, ops);
	    Build_OP(signed_extension ? TOP_sra : TOP_srl, dest, tmp, lit_tn, ops);
	  }
 // #endif
}

static void
Exp_Immediate (TN *dest, TN *src, OPS *ops)
{
  INT64 val;
  TN *tmp = Build_TN_Like(dest);
  
  
  if ( TN_has_value(src) ) {
    val = TN_value(src);
  }
  else if ( TN_is_symbol(src) ) {
    ST *base;
    Base_Symbol_And_Offset_For_Addressing (TN_var(src), TN_offset(src), &base, 
		    &val);
  }
  else FmtAssert(FALSE,("unexpected constant in Exp_Immediate"));

  if (TN_is_symbol(src) && TN_relocs(src) == TN_RELOC_GPSUB) {
    Build_OP(TOP_lui, tmp, 
		    Gen_Symbol_TN(TN_var(src), 0, TN_RELOC_HI_GPSUB), ops);
    Build_OP(TOP_addiu, dest, tmp, Gen_Symbol_TN(TN_var(src), 0, 
			    TN_RELOC_LO_GPSUB), ops);
  }
  else if (ISA_LC_Value_In_Class (val, LC_simm16)) {
    Build_OP (TOP_addiu, dest, Zero_TN, src, ops);
  }
  else if (ISA_LC_Value_In_Class (val, LC_uimm16)) {
    Build_OP (TOP_ori, dest, Zero_TN, src, ops);
  }
  else if (val >= INT32_MIN && val <= INT32_MAX){
    Build_OP (TOP_lui, tmp, Gen_Literal_TN((val >> 16)&0xffff, 4), ops);
    Build_OP (TOP_ori, dest, tmp, Gen_Literal_TN(val & 0xffff, 4), ops);
  }
  else if ((UINT64)val <= UINT32_MAX) {
    if (TN_size(dest) == 4)
      Build_OP (TOP_lui, tmp, Gen_Literal_TN((val >> 16) & 0xffff, 4), ops);
    else {
      Build_OP (TOP_ori, tmp, Zero_TN, 
		Gen_Literal_TN((val >> 16) & 0xffff, 4), ops);
      Build_OP (TOP_dsll, tmp, tmp, Gen_Literal_TN(16, 4), ops);
    }
    Build_OP (TOP_ori, dest, tmp, Gen_Literal_TN(val & 0xffff, 4), ops);
  }
  else {
    TCON tcon = Host_To_Targ (MTYPE_I8, val);
    ST *sym = New_Const_Sym (Enter_tcon (tcon), Be_Type_Tbl(MTYPE_I8));
    Allocate_Object(sym);
		if(ST_gprel(sym)) {
 			 //Build_OP(TOP_ld, dest, GP_TN, Gen_Symbol_TN(sym, 0, TN_RELOC_GPREL16), ops);			
		     Build_OP(TOP_lw, dest, GP_TN, Gen_Symbol_TN(sym, 0, TN_RELOC_GPREL16), ops);
		     DevWarn("Long Long value is not supported, constant value has been truncated");
		}
		else {
			TN *tmp1= Build_TN_Like(dest);
			 Build_OP (TOP_lui, tmp1, Gen_Literal_TN((val & 0xffffffff)>> 16, 8), ops);
			 Build_OP (TOP_ori, tmp1, tmp1, Gen_Literal_TN(val  & 0xffff, 8), ops);

			 
			
			 Build_OP (TOP_lui, tmp, Gen_Literal_TN((val >> 48) & 0xffff, 8), ops);
			 Build_OP (TOP_ori, dest, tmp, Gen_Literal_TN((val >> 32) & 0xffff, 8), ops);

			 Build_OP (TOP_dsll32, dest, dest, Gen_Literal_TN(0, 8), ops);
			 Build_OP (TOP_daddu, dest, tmp1, dest, ops);
			 
			 DevWarn("Long Long value is not supported, constant value has been truncated");		 	 			 
		}
  } 
} 

void
Exp_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
  Expand_Immediate(dest, src, is_signed, ops);
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
  Exp_Immediate (dest, src, ops);
}

TN*
Expand_Immediate_Into_Register (TN *src, BOOL is_64bit, OPS *ops)
{
  if (TN_value(src) == 0)
    return Zero_TN;
  /* load into reg and do reg case */
  TN *tmp = Build_TN_Of_Mtype (is_64bit ? MTYPE_I8 : MTYPE_I4);
  Expand_Immediate (tmp, src, TRUE, ops);
  return tmp;
}

void
Expand_Add (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Add: illegal result size\n"));
  TOP new_opcode;
  INT64 val;
  BOOL is_64bit = MTYPE_bit_size(mtype) == 64;
  if (TN_is_constant(src1)) {
    if (TN_has_value(src1)) {
      val = TN_value(src1);
      if (val == 0) {
	Expand_Copy (result, src2, mtype, ops);
	return;
      }
    } else if ( TN_is_symbol(src1) ) {
      /* symbolic constant, gp-relative or sp-relative */
      ST *base;
      INT64 val;
      TN *tmp = Build_TN_Of_Mtype (mtype);
      Base_Symbol_And_Offset_For_Addressing (TN_var(src1), TN_offset(src1), 
					     &base, &val);
      new_opcode = is_64bit ? TOP_daddiu : TOP_addiu;
      if (ISA_LC_Value_In_Class (val, LC_simm16) || 
	  ISA_LC_Value_In_Class (val, LC_uimm16)) {
	Build_OP (new_opcode, result, src2, src1, ops);
      } else if (val >= INT32_MIN && val <= INT32_MAX) {
	Build_OP (TOP_lui, tmp, Gen_Literal_TN((val >> 16)&0xffff, 4), ops);
	Build_OP (TOP_ori, tmp, tmp, Gen_Literal_TN(val & 0xffff, 4), ops);
	Build_OP (is_64bit ? TOP_daddu : TOP_addu, result, tmp, src2, ops);
      } else {
	TCON tcon = Host_To_Targ (MTYPE_I8, val);
	ST *sym = New_Const_Sym (Enter_tcon (tcon), Be_Type_Tbl(MTYPE_I8));
	Allocate_Object(sym);
	if (Use_32_Bit_Pointers)
	  Build_OP(TOP_lw, tmp, GP_TN,
		   Gen_Symbol_TN(sym, 0, TN_RELOC_GOT_DISP), ops);
	else {
	  Build_OP(TOP_ld, tmp, GP_TN,
		   Gen_Symbol_TN(sym, 0, TN_RELOC_GOT_DISP), ops);
	}
	Build_OP(TOP_ld, tmp, tmp, Gen_Literal_TN(0, 4), ops);
	Build_OP (is_64bit ? TOP_daddu : TOP_addu, result, tmp, src2, ops);
      }       
      return;
    } 
    else FmtAssert(FALSE,("unexpected constant in Expand_Add"));
    
    if (ISA_LC_Value_In_Class ( val, LC_simm16)) {
      new_opcode = is_64bit ? TOP_daddiu : TOP_addiu;
      Build_OP (new_opcode, result, src2, Gen_Literal_TN(val,4), ops);
    } else {
      src1 = Expand_Immediate_Into_Register (src1, is_64bit, ops);
      new_opcode = is_64bit ? TOP_dadd : TOP_addu;
      Build_OP (new_opcode, result, src2, src1, ops);
    }
  } else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Add (result, src2, src1, mtype, ops);
  } else {
  	Build_OP (is_64bit ? TOP_daddu : TOP_addu, result, src1, src2, ops);
  }
}

void
Expand_Sub (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Sub: illegal result size\n"));
  TOP new_opcode;
  INT64 val;
  BOOL is_64bit = MTYPE_bit_size(mtype) == 64;
  if (TN_is_constant(src2)) {
	if (TN_has_value(src2)) {
		val = - TN_value(src2);
		if (val == 0) {
			Expand_Copy (result, src1, mtype, ops);
			return;
		}
	} 
	else if ( TN_is_symbol(src2) ) {
		/* symbolic constant, gp-relative or sp-relative */
		ST *base;
		INT64 val;
		Base_Symbol_And_Offset_For_Addressing (TN_var(src2), TN_offset(src2), &base, &val);
		val = - val;
	} 
	else FmtAssert(FALSE,("unexpected constant in Expand_Sub"));

	if (ISA_LC_Value_In_Class ( val, LC_simm16)) {
		new_opcode = is_64bit ? TOP_daddiu : TOP_addiu;
		Build_OP (new_opcode, result, src1, Gen_Literal_TN(val,4), ops);
	} else {
		src2 = Expand_Immediate_Into_Register (src2, is_64bit, ops);
		new_opcode = is_64bit ? TOP_dsubu : TOP_subu;
		Build_OP (new_opcode, result, src1, src2, ops);
	}
  }
  else if (TN_is_constant(src1)) {
    TN *tmp = Build_TN_Of_Mtype (mtype);
  	// switch order of src so immediate is first
	Expand_Sub (tmp, src2, src1, mtype, ops);
	// generate a negate
	Build_OP(is_64bit ? TOP_dsubu : TOP_subu, result, Zero_TN, tmp, ops);
  } 
  else {
  	Build_OP (is_64bit ? TOP_dsubu : TOP_subu, result, src1, src2, ops);
  }
}


void
Expand_Neg (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  BOOL is_64bit = MTYPE_bit_size(mtype) == 64;
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Neg: illegal result size\n"));
  Build_OP (is_64bit ? TOP_dsubu : TOP_subu, result, Zero_TN, src, ops);
}

void
Expand_Abs (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
/*For MIPS3 machine.*/
  if (mtype == MTYPE_U4 || mtype == MTYPE_U8){
         DevWarn("Try to get Abs value of an unsigned number!\n");
         Expand_Copy(dest, src, mtype, ops);
         return;
  }

  TN *tmp1, *tmp2;
  tmp1 = Build_TN_Like(dest);
  tmp2 = Build_TN_Like(dest);

  if (mtype == MTYPE_I8) {
         Build_OP(TOP_dsra32, tmp1, src, Gen_Literal_TN(31, 2), ops);
         Build_OP(TOP_xor, tmp2,  src, tmp1, ops);
         Build_OP(TOP_dsub, dest,  tmp2, tmp1, ops);
  } else if (mtype == MTYPE_I4) {
         Build_OP(TOP_sra, tmp1,  src, Gen_Literal_TN(31, 2), ops);
         Build_OP(TOP_xor, tmp2,  src, tmp1, ops);
         Build_OP(TOP_sub, dest,  tmp2, tmp1, ops);
  } else {
    Is_True(FALSE, ("Can not handle this ABS case!\n"));
  }
/*For MIPS4*/
}

void
Expand_Shift (TN *result, TN *src1, TN *src2, TYPE_ID mtype, SHIFT_DIRECTION kind, OPS *ops)
{
  WN *tree;
  TOP top;  
  BOOL is_64bit = MTYPE_is_size_double(mtype);


  if (TN_is_constant(src1))
    src1 = Expand_Immediate_Into_Register(src1, MTYPE_is_size_double(mtype), ops);
  if (TN_has_value(src2)) {
    // In mips, only the low log2(wordsize) bits of the shift count are used. 
    UINT64 val = TN_value(src2);
    switch (kind) {
    case shift_left:
      if (! is_64bit)
	top = TOP_sll;
      else if (val < 32) 
	top = TOP_dsll;
      else top = TOP_dsll32;
      break;
    case shift_aright:
      if (! is_64bit)
	top = TOP_sra;
      else if (val < 32) 
	top = TOP_dsra;
      else top = TOP_dsra32;
      break;
    case shift_lright:
      if (! is_64bit)
	top = TOP_srl;
      else if (val < 32) 
	top = TOP_dsrl;
      else top = TOP_dsrl32;
      break;
    }
    Build_OP(top, result, src1, Gen_Literal_TN(val & 31, 4), ops);
  }
  else {
    switch (kind) {
    case shift_left:
      top = is_64bit ? TOP_dsllv : TOP_sllv;
      break;
  
    case shift_aright:
      top = is_64bit ? TOP_dsrav : TOP_srav;
      break;
  
    case shift_lright:
      top = is_64bit ? TOP_dsrlv : TOP_srlv;
      break;
    }
    Build_OP(top, result, src1, src2, ops);
  }
}

inline void
Expand_G_To_F (TN *ftn, TN *gtn, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

inline void
Expand_F_To_G (TN *gtn, TN *ftn, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


/*
 *
 * Helper routine for Expand_Small_Multiply
 *
 */
static void shladd(TN *r, TN *x1, INT s, TN *x2, OPS *ops)
{
  FmtAssert(s <= 31,("shladd: shift amount too large: %d",s));
  if (x2 == Zero_TN) {
    Build_OP(TN_size(r) == 8 ? TOP_dsll : TOP_sll, r, x1, Gen_Literal_TN(s, 4),
	     ops);
  }
  else {
    TN *tmp_tn = Build_TN_Like(r);
    if (TN_size(r) == 8) {
      Build_OP(TOP_dsll, tmp_tn, x1, Gen_Literal_TN(s, 4), ops);
      Build_OP(TOP_daddu, r, tmp_tn, x2, ops);
    } else {
      Build_OP(TOP_sll, tmp_tn, x1, Gen_Literal_TN(s, 4), ops);
      Build_OP(TOP_addu, r, tmp_tn, x2, ops);
    }
  }
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

#define ONE_TEMP r1=Build_TN_Like(r)
#define TWO_TEMPS ONE_TEMP; r2=Build_TN_Like(r)

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
     ONE_TEMP;
     Expand_Small_Multiply(r1,x,16,ops);
     Expand_Small_Multiply(r,r1,2,ops);
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
{ FmtAssert(FALSE,("Unimplemented")); }

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
  TN *r1 = Build_TN_Like(result);
  INT64 c = constant; // I don't want to depend on TARG_INT
  BOOL needs_sign_extension;

  // fast special cases
  if (c == 0) {
    Expand_Copy (result, Zero_TN, mtype, ops);
    return TRUE;
  } else if (c == 1) {
    Expand_Copy (result, var_tn, mtype, ops);
    return TRUE;
  } else if (c == -1) {
    Expand_Neg(result, var_tn, mtype, ops);
    return TRUE;
  }

  if (c < 0) {
    c = -c;
    x = Build_TN_Like(var_tn);
    Expand_Neg(x, var_tn, mtype, ops);
  }


	/* SL specified expand
	 * constant = 2 to 5, handled by individual routine
	 * constant > 5, handles if and only if: 
	 * constant==2^x (x=3 to 31) 
	 * constant==2^x (x=3 to 31) +/- constant
	 * constant==2^x (x=3 to 31) +/- (2*constant)
	 */
#if defined(TARG_SL) 
	 	needs_sign_extension = MTYPE_size_reg(mtype) != 32;
		BOOL matched = FALSE;
		if(c<=5) {
			switch(c) {
				case 2:
					Expand_Shift(result, x, Gen_Literal_TN(1, 4), mtype, shift_left, ops);
					break;
				case 3:
				  Expand_Shift(r1, x, Gen_Literal_TN(1, 4), mtype, shift_left, ops);					
					Expand_Add(result, r1, x, mtype,ops);
					break;
				case 4:
					Expand_Shift(result, x, Gen_Literal_TN(2, 4), mtype, shift_left, ops);
					break;
				case 5:
					Expand_Shift(r1, x, Gen_Literal_TN(2, 4), mtype, shift_left, ops);
					Expand_Add(result, r1, x, mtype,ops);
					break;
			}
			matched = TRUE;
		}
		else {
			UINT shift_bit;
			UINT word_bit = HOST_WORD_SIZE * 8;
			for(shift_bit = 3; shift_bit<word_bit&&matched==FALSE; shift_bit++) {
				UINT sNumber = 1 << shift_bit;
				if(sNumber==c) {
				  Expand_Shift(result, x, Gen_Literal_TN(shift_bit, 4), mtype, shift_left, ops);
					matched = TRUE;					
				}
				else if(sNumber-1==c) {
				  Expand_Shift(r1, x, Gen_Literal_TN(shift_bit, 4), mtype, shift_left, ops);
					Expand_Sub(result, r1, x, mtype,ops);
					matched = TRUE;					
				}
				else if(sNumber+1==c) {
				  Expand_Shift(r1, x, Gen_Literal_TN(shift_bit, 4), mtype, shift_left, ops);					
					Expand_Add(result, r1, x, mtype,ops);
					matched = TRUE;					
				}
				else if(sNumber-2==c) {
				  Expand_Shift(r1, x, Gen_Literal_TN(shift_bit, 4), mtype, shift_left, ops);
					Expand_Sub(r1, r1, x,mtype,ops);	
					Expand_Sub(result, r1, x, mtype,ops);				
					matched = TRUE;					
				}
				else if(sNumber+2==c) {
				  Expand_Shift(r1, x, Gen_Literal_TN(shift_bit, 4), mtype, shift_left, ops);
					Expand_Add(r1, r1, x, mtype,ops);	
					Expand_Add(result, r1, x, mtype,ops);						
					matched = TRUE;					
				}												
			}
	  }
	  	if(matched==TRUE) {
		  	if (needs_sign_extension) {
		  		TN *r2 = Build_TN_Like(result);
		  		Expand_Copy (r2, result, mtype, ops);
		   		Fixup_32_Bit_Op(result,r2,mtype,ops);
		  	}
		  	return TRUE;	
	  	}
	  	else {
	  		return FALSE;
	  	}
	  	
	/*
	 * original expand  		
	 */
#else

  // Count the number of 1's in c and -c
  INT num_ones=0;
  UINT64 uc=c;
  while (uc) {num_ones += (uc&1); uc >>= 1;}
  uc = c;
  needs_sign_extension = MTYPE_size_reg(mtype) != 64;
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
      TN *x1 = Dup_TN(x);
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
    TN *r1 = Dup_TN(result);
    Expand_Shift(r1, x, Gen_Literal_TN(num_ones, 4), MTYPE_I8, shift_left, ops);
    if (!needs_sign_extension) {
      Expand_Sub(result,r1,x,mtype,ops);
    } else {
      TN *r2 = Dup_TN(result);
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
      TN *r1 = Dup_TN(result);
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
    r = Dup_TN(result);
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
    TN *r1 = Dup_TN(result);
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
      TN *r2 = Dup_TN(result);
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
    TN *r1 = Dup_TN(result);
    TN *r2 = Dup_TN(result);
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
      TN *r3 = Dup_TN(result);
      uc1 /= 16;
      Expand_Constant_Multiply(r3,x,uc1,MTYPE_I8,ops);
      shladd(r2,r1,4,r3,ops);
      shladd(r,r2,4,Zero_TN,ops);

    } else if (uc1%17 == 0) {
      TN *r3 = Dup_TN(result);
      uc1 /= 17;
      Expand_Constant_Multiply(r3,x,uc1,MTYPE_I8,ops);
      shladd(r2,r1,4,r3,ops);
      shladd(r,r2,4,r3,ops);

    } else {
      TN *r3 = Dup_TN(result);
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
    TN *r1 = Dup_TN(result);
    TN *r2 = Dup_TN(result);
    TN *r3 = Dup_TN(result);
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
    TN *r1 = Dup_TN(result);
    TN *r2 = Dup_TN(result);
    TN *r3 = Dup_TN(result);
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
#endif // TARG_SL
}

void
Expand_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TOP top;
  INT64 constant;
  //
  // Check for two constants
  // 
  // TODO: check this portion of Exapnd_Multiply once divrem is retargeted.
  if ((TN_has_value(src1) || TN_is_rematerializable(src1)) &&
      (TN_has_value(src2) || TN_is_rematerializable(src2))) {
    // Two constants can sometimes occur because of DIVREM production in 
    TN *val_tn;
    constant = TN_has_value(src1) ? TN_value(src1) : WN_const_val(TN_home(src1));
    constant *= TN_has_value(src2) ? TN_value(src2) : WN_const_val(TN_home(src2));
    // Need to get the constant of the right length
    constant = Targ_To_Host(Host_To_Targ(mtype,constant));
    #if defined(TARG_SL) 
    	val_tn = Gen_Literal_TN(constant, 4);
    #else
        val_tn = Gen_Literal_TN(constant, 8);
	  #endif
    Exp_Immediate(result,val_tn,MTYPE_is_signed(mtype),ops);
    return;
  }

  if (!Disable_Const_Mult_Opt && (TN_has_value(src1) || TN_has_value(src2) ||
				  TN_is_rematerializable(src1) ||TN_is_rematerializable(src2))) {
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
    src2 = Expand_Immediate_Into_Register (src2, MTYPE_is_size_double(mtype), ops);
  }

  FmtAssert(!TN_is_constant(src1),("Expand_Multiply: unexpected constant operand"));
#if defined(TARG_SL)
   Is_True(!(MTYPE_is_size_double(mtype)), ("Expand_Multiply: unsupport 64-bit compute"));
 
   if (CG_sl2) {
   	top = TOP_c2_muls;
   } else {
       top = MTYPE_signed(mtype) ? TOP_c3_muls : TOP_c3_mulus;
   }
#else
   if (! MTYPE_is_size_double(mtype))
     top = MTYPE_signed(mtype) ? TOP_mult : TOP_multu;
   else top = MTYPE_signed(mtype) ? TOP_dmult : TOP_dmultu;
#endif

#if defined(TARG_SL)
  TN *zero_tn = Gen_Literal_TN(0, 4);
  if (CG_sl2) {
    TN* hi_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, 8, 0);
    Build_OP(top, result, hi_tn, src1, src2, zero_tn, Zero_TN, zero_tn, ops);	
  } else {
    Build_OP(top, HI_TN, result, src1, src2,zero_tn,ops);
  }
#else
  Build_OP(top, Hilo_TN(), src1, src2, ops);
  Build_OP(TOP_mflo, result, Hilo_TN(), ops);
#endif
}

/* return high part of multiply result */
void
Expand_High_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TOP top;
  FmtAssert(!TN_is_constant(src1),("Expand_High_Multiply: unexpected constant operand"));

#if defined(TARG_SL)
   Is_True(!(MTYPE_is_size_double(mtype)), ("Expand_High_Multiply: unsupport 64-bit compute"));
   top = MTYPE_signed(mtype) ? TOP_c3_muls : TOP_c3_mulus;
#else
   if (!MTYPE_is_size_double(mtype))
     top = MTYPE_signed(mtype) ? TOP_mult : TOP_multu;
   else top = MTYPE_signed(mtype) ? TOP_dmult : TOP_dmultu;
#endif

 if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(src2, MTYPE_is_size_double(mtype), ops);
  
#if defined(TARG_SL)
   TN *lo_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
   TN *zero_tn = Gen_Literal_TN(0, 4);
   Build_OP(top, HI_TN, lo_tn, src1, src2, zero_tn, ops);
   Build_OP(TOP_c3_mvfs, result, HI_TN, zero_tn, ops);
#else
  Build_OP(top, Hilo_TN(), src1, src2, ops);
  Build_OP(TOP_mfhi, result, Hilo_TN(), ops);
#endif 
}


void
Expand_Logical_Not (TN *dest, TN *src, VARIANT variant, OPS *ops)
{
  /* dest = (src == 0) ? 1 : 0 */
  Build_OP (TOP_xori, dest, src, Gen_Literal_TN(1, 4), ops);
}

void
Expand_Logical_And (TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  Build_OP (TOP_and, dest, src1, src2, ops);
}

void
Expand_Logical_Or (TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  Build_OP (TOP_or, dest, src1, src2, ops);
}


void
Expand_Binary_Complement (TN *dest, TN *src, TYPE_ID /* mtype */, OPS *ops)
{
  Build_OP(TOP_nor, dest, src, Zero_TN, ops);
}

void
Expand_Binary_And (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Binary_And: illegal result size\n"));
  if (TN_is_constant(src1)) {
    INT64 val;
    if (TN_has_value(src1)) {
	    val = TN_value(src1);
	    if (val == -1) {
		    Expand_Copy (dest, src2, mtype, ops);
		    return;
	    }
    } 
    else FmtAssert(FALSE,("unexpected constant in Expand_Binary_And"));

    TOP new_opcode;
    if (ISA_LC_Value_In_Class ( val, LC_uimm16)) 
      new_opcode = TOP_andi;
    else {
      src1 = Expand_Immediate_Into_Register(src1, MTYPE_bit_size(mtype) == 64,
					    ops);
      new_opcode = TOP_and;
    }
    Build_OP (new_opcode, dest, src2, src1, ops);
  }
  else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_And (dest, src2, src1, mtype, ops);
  } 
  else {
    Build_OP (TOP_and, dest, src1, src2, ops);
  }
}

void
Expand_Binary_Or (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Binary_Or: illegal dest size\n"));
  if (TN_is_constant(src1)) {
    INT64 val;
    if (TN_has_value(src1)) {
	    val = TN_value(src1);
	    if (val == 0) {
		    Expand_Copy (dest, src2, mtype, ops);
		    return;
	    }
    } 
    else FmtAssert(FALSE,("unexpected constant in Expand_Binary_Or"));

    TOP new_opcode;
    if (ISA_LC_Value_In_Class ( val, LC_uimm16)) 
      new_opcode = TOP_ori;
    else {
      src1 = Expand_Immediate_Into_Register(src1, MTYPE_bit_size(mtype) == 64,
					    ops);
      new_opcode = TOP_or;
    }
    Build_OP (new_opcode, dest, src2, src1, ops);
  }
  else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_Or (dest, src2, src1, mtype, ops);
  } 
  else {
    Build_OP (TOP_or, dest, src1, src2, ops);
  } 
}

void
Expand_Binary_Xor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Binary_Xor: illegal dest size\n"));
  if (TN_is_constant(src1)) {
    INT64 val;
    if (TN_has_value(src1)) 
      val = TN_value(src1);
    else FmtAssert(FALSE,("unexpected constant in Expand_Binary_Xor"));
    if (val == 0 && src1 == dest)
      return;

    TOP new_opcode;
    if (ISA_LC_Value_In_Class ( val, LC_uimm16)) 
      new_opcode = TOP_xori;
    else {
      src1 = Expand_Immediate_Into_Register(src1, MTYPE_bit_size(mtype) == 64,
					    ops);
      new_opcode = TOP_xor;
    }
    Build_OP (new_opcode, dest, src2, src1, ops);
  }
  else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_Xor (dest, src2, src1, mtype, ops);
  } 
  else {
    Build_OP (TOP_xor, dest, src1, src2, ops);
  }
}

void
Expand_Binary_Nor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Build_OP (TOP_nor, dest, src1, src2, ops);
}

void
Expand_Int_Less (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Int_Less: illegal dest size\n"));
  if (TN_is_constant(src2)) {
    INT64 val;
    if (TN_has_value(src2)) 
      val = TN_value(src2);
    else if ( TN_is_symbol(src2) ) {
      /* symbolic constant, gp-relative or sp-relative */
      ST *base;
      INT64 val;
      Base_Symbol_And_Offset_For_Addressing (TN_var(src2), TN_offset(src2), &base, &val);
    } 
    else FmtAssert(FALSE,("unexpected constant in Expand_Int_Less"));

    TOP new_opcode;
    if (ISA_LC_Value_In_Class ( val, LC_simm16)) 
      new_opcode = MTYPE_signed(mtype) ? TOP_slti : TOP_sltiu;
    else {
      src2 = Expand_Immediate_Into_Register(src2, MTYPE_bit_size(mtype) == 64,
					    ops);
      new_opcode = MTYPE_signed(mtype) ? TOP_slt : TOP_sltu;
    }
    Build_OP (new_opcode, dest, src1, src2, ops);
  }
  else 
    Build_OP (MTYPE_signed(mtype) ? TOP_slt : TOP_sltu, dest, src1, src2, ops);
}

void
Expand_Int_Less_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Int_Less_Equal: illegal dest size\n"));
  if (TN_is_constant(src2)) {
    INT64 val;
    if (TN_has_value(src2)) 
      val = TN_value(src2);
    else FmtAssert(FALSE,("unexpected constant in Expand_Int_Less_Equal"));

    if (ISA_LC_Value_In_Class ( val+1, LC_simm16)) {
      Build_OP(MTYPE_signed(mtype) ? TOP_slti : TOP_sltiu, dest, src1,
	       Gen_Literal_TN(val+1, 4), ops);
    }
    else {
      INT size = MTYPE_byte_size(mtype);
      src2 = Expand_Immediate_Into_Register(Gen_Literal_TN(val+1, size), 
	      				    size == 8, ops);
      Build_OP(MTYPE_signed(mtype) ? TOP_slt : TOP_sltu, dest, src1, src2, ops);
    }
  }
  else { // i <= j => !(j < i)
    TN *tmp = Build_TN_Of_Mtype(mtype);   
    Build_OP (MTYPE_signed(mtype) ? TOP_slt : TOP_sltu, tmp, src2, src1, ops);
    Build_OP (TOP_xori, dest, tmp, Gen_Literal_TN(1, 4), ops);
  }
}

void
Expand_Int_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TN *tmp_tn;
  if ((TN_size(dest) == MTYPE_byte_size(mtype)) &&
      !TN_is_dedicated(dest))
    tmp_tn = dest;
  else tmp_tn  = Gen_Typed_Register_TN(mtype, MTYPE_byte_size(mtype));
  Expand_Binary_Xor(tmp_tn, src1, src2, mtype, ops);
  Build_OP (TOP_sltiu, dest, tmp_tn, Gen_Literal_TN(1, 4), ops);
}

void
Expand_Int_Not_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TN *tmp_tn;
  if ((TN_size(dest) == MTYPE_byte_size(mtype)) && 
      !TN_is_dedicated(dest))
    tmp_tn = dest;
  else tmp_tn  = Gen_Typed_Register_TN(mtype, MTYPE_byte_size(mtype));
  Expand_Binary_Xor(tmp_tn, src1, src2, mtype, ops);
  Build_OP (TOP_sltu, dest, Zero_TN, tmp_tn, ops);
}

void
Expand_Int_Greater_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TN *tmp_tn;
  if ((TN_size(dest) == MTYPE_byte_size(mtype)) && 
      !TN_is_dedicated(dest))
    tmp_tn = dest;
  else tmp_tn  = Gen_Typed_Register_TN(mtype, MTYPE_byte_size(mtype));
  Expand_Int_Less(tmp_tn, src1, src2, mtype, ops);
  Build_OP (TOP_xori, dest, tmp_tn, Gen_Literal_TN(1, 4), ops);
}

void
Expand_Int_Greater (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
	               ("Expand_Int_Greater: illegal dest size\n"));
  if (TN_is_constant(src1)) 
    src1 = Expand_Immediate_Into_Register(src1, MTYPE_bit_size(mtype)==64, ops);
  if (TN_is_constant(src2)) 
    src2 = Expand_Immediate_Into_Register(src2, MTYPE_bit_size(mtype)==64, ops);
  Build_OP (MTYPE_signed(mtype) ? TOP_slt : TOP_sltu, dest, src2, src1, ops);
}

static void
Expand_Bool_Comparison (BOOL equals, TN *dest, TN *src1, TN *src2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Bool_Equal (TN *dest, TN *src1, TN *src2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Bool_Not_Equal (TN *dest, TN *src1, TN *src2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Bool_To_Int (TN *dest, TN *src, TYPE_ID rtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

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
  TOP top;
  BOOL int_64bit = MTYPE_byte_size(imtype) == 8;
  if (fmtype == MTYPE_F4) {
    if (int_64bit || !(MTYPE_is_signed(imtype))) {
      switch (rm) {
      case ROUND_USER: top = TOP_cvt_l_s; break;
      case ROUND_NEAREST: top = TOP_round_l_s; break;
      case ROUND_CHOP: top = TOP_trunc_l_s; break;
      case ROUND_NEG_INF: top = TOP_floor_l_s; break;
      case ROUND_PLUS_INF: top = TOP_ceil_l_s; break;
      default: FmtAssert(FALSE,("Unimplemented rounding mode"));
      }
    }
    else {
      switch (rm) {
      case ROUND_USER: top = TOP_cvt_w_s; break;
      case ROUND_NEAREST: top = TOP_round_w_s; break;
      case ROUND_CHOP: top = TOP_trunc_w_s; break;
      case ROUND_NEG_INF: top = TOP_floor_w_s; break;
      case ROUND_PLUS_INF: top = TOP_ceil_w_s; break;
      default: FmtAssert(FALSE,("Unimplemented rounding mode"));
      }
    }
  }
  else if (fmtype == MTYPE_F8) {
    if (int_64bit || !(MTYPE_is_signed(imtype))) {
      switch (rm) {
      case ROUND_USER: top = TOP_cvt_l_d; break;
      case ROUND_NEAREST: top = TOP_round_l_d; break;
      case ROUND_CHOP: top = TOP_trunc_l_d; break;
      case ROUND_NEG_INF: top = TOP_floor_l_d; break;
      case ROUND_PLUS_INF: top = TOP_ceil_l_d; break;
      default: FmtAssert(FALSE,("Unimplemented rounding mode"));
      }
    }
    else {
      switch (rm) {
      case ROUND_USER: top = TOP_cvt_w_d; break;
      case ROUND_NEAREST: top = TOP_round_w_d; break;
      case ROUND_CHOP: top = TOP_trunc_w_d; break;
      case ROUND_NEG_INF: top = TOP_floor_w_d; break;
      case ROUND_PLUS_INF: top = TOP_ceil_w_d; break;
      default: FmtAssert(FALSE,("Unimplemented rounding mode"));
      }
    }
  }
  else FmtAssert(FALSE,("unsupported float size in Expand_Float_To_Int"));
  if (imtype == fmtype)  // no need to convert if result is float type
    return;
  TN *tmp = Build_TN_Of_Mtype (fmtype);
  Build_OP(top, tmp, src, ops);
  Build_OP(int_64bit ? TOP_dmfc1 : TOP_mfc1, dest, tmp, ops);
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
#if !defined(TARG_SL)
        Expand_Float_To_Int (ROUND_CHOP, dest, src, imtype, fmtype, ops);
#else
        TN* tmp = Build_TN_Of_Mtype(MTYPE_I4);
	TN* tmp2 = Build_TN_Of_Mtype(MTYPE_I4);
	Build_OP(TOP_cfc1, tmp2, ops);
	Build_OP(TOP_ori, tmp, tmp2, Gen_Literal_TN(0x3, 4), ops);
	Build_OP(TOP_xori, tmp, tmp, Gen_Literal_TN(0x2, 4), ops);
	Build_OP(TOP_ctc1, tmp,ops);
       	Build_OP(fmtype == MTYPE_F4 ? TOP_cvt_w_s : TOP_cvt_w_d, src, src, ops);
	Build_OP(TOP_ctc1, tmp2, ops);	
	Build_OP(TOP_mfc1, dest, src, ops);
#endif
}


void
Expand_Float_To_Int_Floor (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_NEG_INF, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Ceil (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_PLUS_INF, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Float (TN *dest, TN *src, TYPE_ID rtype, TYPE_ID desc, OPS *ops)
{
  TOP top = (rtype == MTYPE_F8) ? TOP_cvt_d_s : TOP_cvt_s_d;
  Build_OP(top, dest, src, ops);
}


void
Expand_Int_To_Float (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  TOP top;
  BOOL int_64bit = MTYPE_byte_size(imtype) == 8;
  TN *tmp = Build_TN_Of_Mtype(fmtype);
  Build_OP(int_64bit ? TOP_dmtc1 : TOP_mtc1, tmp, src, ops);
  if (fmtype == MTYPE_F4) {    
    // See example gcc.c-torture/execute/conversion.c
    top = (MTYPE_is_signed(imtype)? TOP_cvt_s_w: TOP_cvt_s_l);
    if (MTYPE_is_size_double(imtype))
      top = TOP_cvt_s_l;
  }
  else if (fmtype == MTYPE_F8) {
    // See example gcc.c-torture/execute/conversion.c
    top = (MTYPE_is_signed(imtype)? TOP_cvt_d_w: TOP_cvt_d_l);
    if (MTYPE_is_size_double(imtype))
      top = TOP_cvt_d_l;
  }
  else FmtAssert(FALSE,("unsupported float size in Expand_Int_To_Float"));
  Build_OP(top, dest, tmp, ops);
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
{ FmtAssert(FALSE,("Unimplemented")); }


#ifdef TARG_SL
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
  int unsignedflag = (mtype == MTYPE_U4) ? 1 : 0;
  Is_True((mtype == MTYPE_U4 || mtype==MTYPE_I4) , ("mtype must be MTYPE_I4 or MTYPE_U4" ));
  
  if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_fcc) {
    if (is_float) {
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movt_d:TOP_movt_s, 
	       dest_tn, true_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movf_d:TOP_movf_s, 
	       dest_tn, false_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    } else {
      Build_OP(TOP_movt, dest_tn, true_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(TOP_movf, dest_tn, false_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    }
  } else if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_integer) {
    if (is_float) {
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movn_d:TOP_movn_s, 
	       dest_tn, true_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movz_d:TOP_movz_s, 
	       dest_tn, false_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);      
    } else {
        if (tns_are_equals(true_tn, false_tn)) {
	  Build_OP(TOP_addu, dest_tn, true_tn, Zero_TN, ops);
	  DevWarn("Conditional move::  Expand_Select(1 instructions -copy) ");
        } else if (tns_are_equals(Zero_TN, true_tn)) {
	  Build_MC_OP(TOP_mc_zc_eq, dest_tn, cond_tn, false_tn,unsignedflag, ops, OP_ALWAYS_UNC_DEF);
        } else if (tns_are_equals(Zero_TN, false_tn)) {
          Build_MC_OP(TOP_mc_zc_ne, dest_tn, cond_tn, true_tn, unsignedflag, ops, OP_ALWAYS_UNC_DEF);
        } else if (tns_are_equals(dest_tn, false_tn)) {
          Build_MC_OP(TOP_mc_z_ne, dest_tn, cond_tn, true_tn, unsignedflag, ops, OP_ALWAYS_COND_DEF);
        } else if (tns_are_equals(dest_tn, true_tn)) {
          Build_MC_OP(TOP_mc_z_eq, dest_tn, cond_tn, false_tn, unsignedflag, ops, OP_ALWAYS_COND_DEF);
        } else {
	  Exp_2inst_MC_Zero (TOP_mc_z_ne, dest_tn, true_tn, false_tn, cond_tn, unsignedflag, ops);
          DevWarn("Conditional move::  Expand_Select(2 instructions) ");
        }
    }
  } else if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_float) {
    TN *tmp_tn = Gen_Typed_Register_TN(MTYPE_I4, 4);
    Build_OP(TOP_mfc1, tmp_tn, cond_tn, ops);
    if (is_float) {
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movn_d:TOP_movn_s, 
	       dest_tn, true_tn, tmp_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movz_d:TOP_movz_s, 
	       dest_tn, false_tn, tmp_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);      
    } else {
        DevWarn("Conditional move::  Expand_Select (float) ");
        if (tns_are_equals(true_tn, false_tn)) {
          Build_OP(TOP_addu, dest_tn, true_tn, Zero_TN, ops);
	  DevWarn("Conditional move::  Expand_Select(float: 1 instructions -copy) ");
        } else if (tns_are_equals(Zero_TN, true_tn)) {
          Build_MC_OP(TOP_mc_zc_eq, dest_tn, tmp_tn, false_tn, unsignedflag, ops, OP_ALWAYS_UNC_DEF);
        } else if (tns_are_equals(Zero_TN, false_tn)) {
          Build_MC_OP(TOP_mc_zc_ne, dest_tn, tmp_tn, true_tn, unsignedflag, ops, OP_ALWAYS_UNC_DEF);
        } else {
  	  Exp_2inst_MC_Zero (TOP_mc_z_ne, dest_tn, true_tn, false_tn, tmp_tn,unsignedflag, ops);
          DevWarn("Conditional move::  Expand_Select(float: 2 instructions) ");
        }
    }
  } else {
    FmtAssert(FALSE, ("UNIMPLEMENTED"));
  }
}
#else

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
  if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_fcc) {
    if (is_float) {
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movt_d:TOP_movt_s, 
	       dest_tn, true_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movf_d:TOP_movf_s, 
	       dest_tn, false_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    } else {
      Build_OP(TOP_movt, dest_tn, true_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(TOP_movf, dest_tn, false_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    }
  } else if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_integer) {
    if (is_float) {
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movn_d:TOP_movn_s, 
	       dest_tn, true_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movz_d:TOP_movz_s, 
	       dest_tn, false_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);      
    } else {
      Build_OP(TOP_movn, dest_tn, true_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(TOP_movz, dest_tn, false_tn, cond_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    }
  } else if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_float) {
    TN *tmp_tn = Gen_Typed_Register_TN(MTYPE_I4, 4);
    Build_OP(TOP_mfc1, tmp_tn, cond_tn, ops);
    if (is_float) {
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movn_d:TOP_movn_s, 
	       dest_tn, true_tn, tmp_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(MTYPE_is_size_double(mtype)?TOP_movz_d:TOP_movz_s, 
	       dest_tn, false_tn, tmp_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);      
    } else {
      Build_OP(TOP_movn, dest_tn, true_tn, tmp_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      Build_OP(TOP_movz, dest_tn, false_tn, tmp_tn, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    }
  } else {
    FmtAssert(FALSE, ("UNIMPLEMENTED"));
  }
}
#endif

void
Expand_Min (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  BOOL is_signed = MTYPE_is_signed(mtype);
  if (!MTYPE_is_float(mtype)) {
    TN *tmp_tn = Gen_Typed_Register_TN(MTYPE_I4, 4);
    if (src1 == dest) {
      Build_OP(is_signed?TOP_slt:TOP_sltu, tmp_tn, src1, src2, ops);
      Build_OP(TOP_movz, dest, src2, tmp_tn, ops); 
    }
    else if (src2 == dest) {
      Build_OP(is_signed?TOP_slt:TOP_sltu, tmp_tn, src2, src1, ops);
      Build_OP(TOP_movz, dest, src1, tmp_tn, ops); 
    }
    else {
      Build_OP(TOP_or, dest, src1, Zero_TN, ops);
      Build_OP(is_signed?TOP_slt:TOP_sltu, tmp_tn, src1, src2, ops);
      Build_OP(TOP_movz, dest, src2, tmp_tn, ops); 
    }
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  } else {
    TN *tmp_fcc = Gen_Register_TN(ISA_REGISTER_CLASS_fcc, 1);
    if (src1 == dest) {
      Build_OP(MTYPE_is_size_double(mtype)? TOP_c_lt_d: TOP_c_lt_s,
	       tmp_fcc, src2, src1, ops);
      Build_OP(MTYPE_is_size_double(mtype)? TOP_movt_d: TOP_movt_s,
	       dest, src2, tmp_fcc, ops);
    }
    else if (src2 == dest) {
      Build_OP(MTYPE_is_size_double(mtype)? TOP_c_lt_d: TOP_c_lt_s,
	       tmp_fcc, src1, src2, ops);
      Build_OP(MTYPE_is_size_double(mtype)? TOP_movt_d: TOP_movt_s,
	       dest, src1, tmp_fcc, ops);
    }
    else {
      Build_OP(MTYPE_is_size_double(mtype)? TOP_mov_d: TOP_mov_s,
	       dest, src2, ops);
      Build_OP(MTYPE_is_size_double(mtype)? TOP_c_lt_d: TOP_c_lt_s,
	       tmp_fcc, src1, src2, ops);
      Build_OP(MTYPE_is_size_double(mtype)? TOP_movt_d: TOP_movt_s,
	       dest, src1, tmp_fcc, ops);
    }
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  }
}

void
Expand_Max (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{     
  BOOL is_signed = MTYPE_is_signed(mtype);
  if (!MTYPE_is_float(mtype)) {
    TN *tmp_tn = Gen_Typed_Register_TN(MTYPE_I4, 4);
    if (src1 == dest) {
      Build_OP(is_signed?TOP_slt:TOP_sltu, tmp_tn, src2, src1, ops);
      Build_OP(TOP_movz, dest, src2, tmp_tn, ops); 
    }
    else if (src2 == dest) {
      Build_OP(is_signed?TOP_slt:TOP_sltu, tmp_tn, src1, src2, ops);
      Build_OP(TOP_movz, dest, src1, tmp_tn, ops); 
    }
    else {
      Build_OP(TOP_or, dest, src2, Zero_TN, ops);
      Build_OP(is_signed?TOP_slt:TOP_sltu, tmp_tn, src1, src2, ops);
      Build_OP(TOP_movz, dest, src1, tmp_tn, ops); 
    }
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  } else {
    TN *tmp_fcc = Gen_Register_TN(ISA_REGISTER_CLASS_fcc, 1);
    if (src1 == dest) {
      Build_OP(MTYPE_is_size_double(mtype)? TOP_c_lt_d: TOP_c_lt_s,
	       tmp_fcc, src1, src2, ops);
      Build_OP(MTYPE_is_size_double(mtype)? TOP_movt_d: TOP_movt_s,
	       dest, src2, tmp_fcc, ops);
    }
    else if (src2 == dest) {
      Build_OP(MTYPE_is_size_double(mtype)? TOP_c_lt_d: TOP_c_lt_s,
	       tmp_fcc, src2, src1, ops);
      Build_OP(MTYPE_is_size_double(mtype)? TOP_movt_d: TOP_movt_s,
	       dest, src1, tmp_fcc, ops);
    }
    else {
      Build_OP(MTYPE_is_size_double(mtype)? TOP_mov_d: TOP_mov_s,
	       dest, src1, ops);
      Build_OP(MTYPE_is_size_double(mtype)? TOP_c_lt_d: TOP_c_lt_s,
	       tmp_fcc, src1, src2, ops);
      Build_OP(MTYPE_is_size_double(mtype)? TOP_movt_d: TOP_movt_s,
	       dest, src2, tmp_fcc, ops);
    }
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  }
}

void
Expand_MinMax (TN *dest, TN *dest2, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ 
  BOOL is_signed = MTYPE_is_signed(mtype);
  FmtAssert(dest != src1 && dest2 != src1 && dest != src2 && dest2 != src2,
	    ("Expand_MinMax: dest TN cannot be also src TN"));
  if (!MTYPE_is_float(mtype)) {
    TN *tmp_tn = Gen_Typed_Register_TN(MTYPE_I4, 4);
    Build_OP(TOP_or, dest, src1, Zero_TN, ops);
    Build_OP(TOP_or, dest2, src2, Zero_TN, ops);
    Build_OP(is_signed?TOP_slt:TOP_sltu, tmp_tn, src1, src2, ops);
    Build_OP(TOP_movz, dest, src2, tmp_tn, ops); 
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    Build_OP(TOP_movz, dest2, src1, tmp_tn, ops); 
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  } else {
    TN *tmp_fcc = Gen_Register_TN(ISA_REGISTER_CLASS_fcc, 1);
    Build_OP(MTYPE_is_size_double(mtype)? TOP_mov_d: TOP_mov_s,
	     dest, src1, ops);
    Build_OP(MTYPE_is_size_double(mtype)? TOP_mov_d: TOP_mov_s,
	     dest2, src2, ops);
    Build_OP(MTYPE_is_size_double(mtype)? TOP_c_lt_d: TOP_c_lt_s,
	     tmp_fcc, src1, src2, ops);
    Build_OP(MTYPE_is_size_double(mtype)? TOP_movf_d: TOP_movf_s,
	     dest, src2, tmp_fcc, ops);
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    Build_OP(MTYPE_is_size_double(mtype)? TOP_movf_d: TOP_movf_s,
	     dest2, src1, tmp_fcc, ops);
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  }
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
  TOP cmp1, cmp2;

  if (Trace_Exp) {
    fprintf(TFile, "expand %s: ", OPCODE_name(select));
    if (result) Print_TN(result,FALSE);
    fprintf(TFile, " :- (");
    if (cmp_kid1) Print_TN(cmp_kid1,FALSE);
    fprintf(TFile, " ");
    fprintf(TFile, OPCODE_name(compare));
    fprintf(TFile, " ");
    if (cmp_kid2) Print_TN(cmp_kid2,FALSE);
    fprintf(TFile, ") ? ");
    if (true_tn) Print_TN(true_tn,FALSE);
    fprintf(TFile, " : ");
    if (false_tn) Print_TN(false_tn,FALSE);
    fprintf(TFile, " ");
    if (variant) fprintf(TFile, "(0x%llx)", (INT64)variant);
    fprintf(TFile, "\n");
  }

  TYPE_ID desc = OPCODE_desc(compare);
  OPERATOR compare_opr = OPCODE_operator(compare);
  TN *p;
  OPS new_ops;
  OPS_Init(&new_ops);
  if (MTYPE_is_float(desc))
    p = Gen_Register_TN(ISA_REGISTER_CLASS_fcc, 1);
  else p = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);

  if (compare_opr == OPR_NE) { // c_neq_[sd] not supported by assembler
    TN *tmp = true_tn;
    true_tn = false_tn;
    false_tn = tmp;
    compare_opr = OPR_EQ;
  }
  
  switch(compare_opr) {
  case OPR_LT:
    if (MTYPE_is_float(desc))
	Expand_Float_Less (p, cmp_kid1, cmp_kid2, variant, desc, &new_ops);
    else
	Expand_Int_Less (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  case OPR_LE:
    if (MTYPE_is_float(desc))
	Expand_Float_Less_Equal (p, cmp_kid1, cmp_kid2, variant, desc, &new_ops);
    else
	Expand_Int_Less_Equal (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  case OPR_EQ:
    if (MTYPE_is_float(desc))
	Expand_Float_Equal (p, cmp_kid1, cmp_kid2, variant, desc, &new_ops);
    else if (desc == MTYPE_B)
	Expand_Bool_Equal (p, cmp_kid1, cmp_kid2, &new_ops);
    else
	Expand_Int_Equal (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  case OPR_GE:
    if (MTYPE_is_float(desc))
	Expand_Float_Greater_Equal (p, cmp_kid1, cmp_kid2, variant, desc, &new_ops);
    else
	Expand_Int_Greater_Equal (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  case OPR_GT:
    if (MTYPE_is_float(desc))
	Expand_Float_Greater (p, cmp_kid1, cmp_kid2, variant, desc, &new_ops);
    else
	Expand_Int_Greater (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  default:
    FmtAssert(FALSE, ("Unknown opcode"));
  }

  if (result != true_tn && result != false_tn) {
    if (MTYPE_is_float(OPCODE_rtype(select))) {
      Build_OP (MTYPE_is_size_double(OPCODE_rtype(select))?TOP_mov_d:TOP_mov_s, 
	        result, true_tn, &new_ops);
    } else {
      Build_OP (TOP_or, result, Zero_TN, true_tn, &new_ops);
    }
  }

  if (MTYPE_is_float(OPCODE_rtype(select))) {
    if (result != false_tn) {
      if (MTYPE_is_float(desc))
	Build_OP (MTYPE_is_size_double(OPCODE_rtype(select))?TOP_movf_d:TOP_movf_s, 
		  result, false_tn, p, &new_ops);
      else Build_OP (MTYPE_is_size_double(OPCODE_rtype(select))?TOP_movz_d:TOP_movz_s,
		     result, false_tn, p, &new_ops);
    } else {
      if (MTYPE_is_float(desc))
	Build_OP (MTYPE_is_size_double(OPCODE_rtype(select))?TOP_movt_d:TOP_movt_s, 
		  result, true_tn, p, &new_ops);
      else Build_OP (MTYPE_is_size_double(OPCODE_rtype(select))?TOP_movn_d:TOP_movn_s,
		     result, true_tn, p, &new_ops);
    }
  } else {
    if (result != false_tn) {
      if (MTYPE_is_float(desc))
	Build_OP (TOP_movf, result, false_tn, p, &new_ops);
      else Build_OP (TOP_movz, result, false_tn, p, &new_ops);
    } else {
      if (MTYPE_is_float(desc))
	Build_OP (TOP_movt, result, true_tn, p, &new_ops);
      else Build_OP (TOP_movn, result, true_tn, p, &new_ops);
    }
  }
  Set_OP_cond_def_kind(OPS_last(&new_ops), OP_ALWAYS_COND_DEF); 
  if (Trace_Exp) {
    OP *op;
    FOR_ALL_OPS_OPs (&new_ops, op) {
	    fprintf(TFile, " into "); Print_OP (op);
    }
  }
  OPS_Append_Ops(ops, &new_ops);
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
}

static void
Expand_Intel_F10_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Max_Thr_F8_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Max_Thr_F4_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Min_Lat_F8_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Min_Lat_F4_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


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
    FmtAssert(FALSE, ("Bad type in Expand_Intel_Min_Lat_Sqrt"));
    /*NOTREACHED*/
  }
}


void
Expand_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert(TN_register_class(result) == ISA_REGISTER_CLASS_float && 
	    TN_register_class(src) == ISA_REGISTER_CLASS_float, 
	    ("Unimplemented sqrt for integer src/dest"));  
  Build_OP(MTYPE_is_size_double(mtype)?TOP_sqrt_d:TOP_sqrt_s, 
	   result, src, ops);
}


static void
Expand_Float_Compares(TOP cmp_opcode, TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  if (TN_register_class(dest) == ISA_REGISTER_CLASS_fcc) {
    FmtAssert(cmp_opcode != TOP_c_neq_d && cmp_opcode != TOP_c_neq_s,
	      ("Expand_Float_Compares: should not use c_neq_[sd] which is not supported by assembler"));  
    Build_OP(cmp_opcode, dest, src1, src2, ops);   
  } else {
    FmtAssert(TN_register_class(dest) == ISA_REGISTER_CLASS_integer,
	      ("Expand_Float_Compares: result must be fcc or integer"));  
    TN *tmp_fcc = Gen_Register_TN(ISA_REGISTER_CLASS_fcc, 1);
    Build_OP(MTYPE_is_size_double(mtype)?TOP_daddiu:TOP_addiu, 
	     dest, Zero_TN, Gen_Literal_TN(1, 4), ops);
    if (cmp_opcode == TOP_c_neq_d || cmp_opcode == TOP_c_neq_s) {
      Build_OP(cmp_opcode == TOP_c_neq_d ? TOP_c_eq_d : TOP_c_eq_s, 
	       tmp_fcc, src1, src2, ops);     
      Build_OP(TOP_movt, dest, Zero_TN, tmp_fcc, ops);
    }
    else {
      Build_OP(cmp_opcode, tmp_fcc, src1, src2, ops);     
      Build_OP(TOP_movf, dest, Zero_TN, tmp_fcc, ops);
    }
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  }
}

void
Expand_Float_Less (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
#if defined(TARG_SL)
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_lt_d:TOP_c_olt_s,
#else 
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_lt_d:TOP_c_lt_s, 
#endif
			dest, src1, src2, mtype, ops);
}

void
Expand_Float_Greater (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
#if defined(TARG_SL)
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_lt_d:TOP_c_olt_s,
#else 
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_lt_d:TOP_c_lt_s, 
#endif
			dest, src2, src1, mtype, ops);
}

void
Expand_Float_Less_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
#if defined(TARG_SL)
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_le_d:TOP_c_ole_s, 
#else
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_le_d:TOP_c_le_s, 
#endif
			dest, src1, src2, mtype, ops);
}

void
Expand_Float_Greater_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
#if defined(TARG_SL)
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_le_d:TOP_c_ole_s, 
#else
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_le_d:TOP_c_le_s, 
#endif
			dest, src2, src1, mtype, ops);
}

void
Expand_Float_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_eq_d:TOP_c_eq_s, 
			dest, src1, src2, mtype, ops);
}

void
Expand_Float_Not_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(MTYPE_is_size_double(mtype)?TOP_c_neq_d:TOP_c_neq_s, 
			dest, src1, src2, mtype, ops);
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
}

void
Expand_Flop (OPCODE opcode, TN *result, TN *src1, TN *src2, TN *src3, OPS *ops)
{
  TOP opc;

  switch (opcode) {
  case OPC_F4ADD:
    opc = TOP_add_s;
    break;
  case OPC_F8ADD:
    opc = TOP_add_d;
    break;
  case OPC_F4SUB:
    opc = TOP_sub_s;
    break;
  case OPC_F8SUB:
    opc = TOP_sub_d;
    break;
  case OPC_F4MPY:
    opc = TOP_mul_s;
    break;
  case OPC_F8MPY:
    opc = TOP_mul_d;
    break;
  case OPC_F4MADD:	// (src2 * src3) + src1
    opc = TOP_madd_s;
    break;
  case OPC_F4NMADD:	// -((src2 * src3) + src1)
    opc = TOP_nmadd_s;
    break;
  case OPC_F4MSUB:	// (src2 * src3) - src1
    opc = TOP_msub_s;
    break;
  case OPC_F4NMSUB:	// -((src2 * src3) - src1)
    opc = TOP_nmsub_s;
    break;
  case OPC_F8MADD:	// (src2 * src3) + src1
    opc = TOP_madd_d;
    break;
  case OPC_F8NMADD:	// -((src2 * src3) + src1)
    opc = TOP_nmadd_d;
    break;
  case OPC_F8MSUB:	// (src2 * src3) - src1
    opc = TOP_msub_d;
    break;
  case OPC_F8NMSUB:	// -((src2 * src3) - src1)
    opc = TOP_nmsub_d;
    break;
  case OPC_F4DIV:
    opc = TOP_div_s;
    break;
  case OPC_F8DIV:
    opc = TOP_div_d;
    break;
  case OPC_F4RECIP:
    opc = TOP_recip_s;
    break;
  case OPC_F8RECIP:
    opc = TOP_recip_d;
    break;
  case OPC_F4RSQRT:
    opc = TOP_rsqrt_s;
    break;
  case OPC_F8RSQRT:
    opc = TOP_rsqrt_d;
    break;
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("unexpected opcode %s", OPCODE_name(opcode)));
    /*NOTREACHED*/
  }
  if (TOP_is_madd(opc)) {
    Build_OP(opc, result, src1, src2, src3, ops);
  } else {
    Build_OP(opc, result, src1, src2, ops);
  }
}

extern void
Init_CG_Expand (void)
{
  static BOOL Initialized = FALSE;

  // per PU:
  Trace_Exp = Get_Trace (TP_CGEXP, 1);
  /* whirl2ops uses -ttexp:2 */
  Trace_Exp2 = Get_Trace (TP_CGEXP, 4);
  Disable_Const_Mult_Opt = Get_Trace (TP_CGEXP, 32);
  
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
  if( TN_is_constant(src_tn) )
  {
    FmtAssert (TN_has_value(src_tn), ("Exp_COPY: illegal source tn"));
    /* expansion for INTCONST doesn't depend on size */
    Exp_OP1 (OPC_I4INTCONST, tgt_tn, src_tn, ops);
  }
  else
  {
#if defined(TARG_SL)
    /* if src tn is map to a special register, the tgt_tn need to be mapped to same special register*/
    Copy_Tn_MapInfo(src_tn, tgt_tn); 
#endif

    ISA_REGISTER_CLASS tgt_rc = TN_register_class(tgt_tn);
    ISA_REGISTER_CLASS src_rc = TN_register_class(src_tn);

    if (tgt_rc == src_rc && tgt_rc == ISA_REGISTER_CLASS_integer) {
      Build_OP( TOP_or, tgt_tn, src_tn, Zero_TN, ops );
      Set_OP_copy (OPS_last(ops));
    }
    else if (src_tn == tgt_tn)
    {
      /* We don't know how to do this copy, but since the source and
         target are the same, we can just return a nop (we must return
         some op). */
      Build_OP(TOP_noop, ops);
    }
    else if (tgt_rc == src_rc && tgt_rc == ISA_REGISTER_CLASS_float) {
      /* dedicated TNs always have size 8, so need to check both TNs */
      BOOL is_double = (TN_size(tgt_tn) == 8 && TN_size(src_tn) == 8);
      Build_OP(is_double ? TOP_mov_d : TOP_mov_s, tgt_tn, src_tn, ops);
      Set_OP_copy (OPS_last(ops));
    }
#if defined(TARG_SL)
    else if (src_rc == ISA_REGISTER_CLASS_control) {
      if (src_rc == tgt_rc) {
        TN *tmp_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer, 4);
          Build_OP(TOP_mvfc, tmp_tn, src_tn, ops);
          Build_OP(TOP_mvtc, tgt_tn, tmp_tn, ops);
      } else if (tgt_rc == ISA_REGISTER_CLASS_integer) {
          Build_OP(TOP_mvfc, tgt_tn, src_tn, ops);
      }
    }
    else if (tgt_rc == ISA_REGISTER_CLASS_control) {
      Is_True(src_rc == ISA_REGISTER_CLASS_integer, ("not supported reg class save"));
      Build_OP(TOP_mvtc, tgt_tn, src_tn, ops);
    }
    else if (src_rc == ISA_REGISTER_CLASS_accum) {
	 if (src_rc == tgt_rc) {
        TN *tmp_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer, 4);
          Build_OP(TOP_c3_mvfacc, tmp_tn, src_tn, ops);
          Build_OP(TOP_c3_mvtacc, tgt_tn, tmp_tn, ops);
      } else if (tgt_rc == ISA_REGISTER_CLASS_integer) {
          Build_OP(TOP_c3_mvfacc, tgt_tn, src_tn, ops);
      }	
    } else if (tgt_rc == ISA_REGISTER_CLASS_accum) {
        Is_True(src_rc == ISA_REGISTER_CLASS_integer, ("not supported reg class save"));
        Build_OP(TOP_c3_mvtacc, tgt_tn, src_tn, ops); 
    }
    else if (src_rc == ISA_REGISTER_CLASS_addr) {
	 if (src_rc == tgt_rc) {
        TN *tmp_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer, 4);
          Build_OP(TOP_c3_mvfadd, tmp_tn, src_tn, ops);
          Build_OP(TOP_c3_mvtadd, tgt_tn, tmp_tn, ops);
      } else if (tgt_rc == ISA_REGISTER_CLASS_integer) {
          Build_OP(TOP_c3_mvfadd, tgt_tn, src_tn, ops);
      }	
    } else if (tgt_rc == ISA_REGISTER_CLASS_addr) {
        Is_True(src_rc == ISA_REGISTER_CLASS_integer, ("not supported reg class save"));
        Build_OP(TOP_c3_mvtadd, tgt_tn, src_tn, ops); 
    }	
#endif

    else
    {
      /* dedicated TNs always have size 8, so need to check both TNs */
      BOOL is_double = (TN_size(tgt_tn) == 8 && TN_size(src_tn) == 8); 
      if (src_rc == ISA_REGISTER_CLASS_integer) { // tgt_tc is float class
	Build_OP(is_double ? TOP_dmtc1 : TOP_mtc1, tgt_tn, src_tn, ops);
      } else if (src_rc == ISA_REGISTER_CLASS_float) { // tgt_tc is integer class
	Build_OP(is_double ? TOP_dmfc1 : TOP_mfc1, tgt_tn, src_tn, ops);
      } else {
	FmtAssert(FALSE, ("Unimplemented Copy.\n"));
      }
    }
  }
}

static ST *tmp_apply_arg = NULL;
void
Generate_Temp_Apply_Arg ( )
{
  TY_IDX tyi;
  TY& ty = New_TY(tyi);
  TY_Init(ty, 144, KIND_STRUCT, MTYPE_M,
          Save_Str("__apply_arg"));
  Set_TY_align(tyi, 8);
  tmp_apply_arg = New_ST(CURRENT_SYMTAB);
  ST_Init(tmp_apply_arg, TY_name_idx(ty),
          CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, tyi);
  Set_ST_is_temp_var(tmp_apply_arg);
  Allocate_Object(tmp_apply_arg);
}

void
Exp_Intrinsic_Op (INTRINSIC id, TN *result, TN *op0, TN *op1, TYPE_ID mtype, OPS *ops)
{
  FmtAssert(FALSE, ("Exp_Intrinsic_Op NYI"));
  return; // if you can
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
  return 0;
}

static TYPE_ID
Get_Intrinsic_Size_Mtype (INTRINSIC id)
{
  switch (id) {
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
  	FmtAssert(FALSE, ("Unexpected intrinsic %d", id));
	return MTYPE_UNKNOWN;
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
  switch (id) {
  case INTRN_APPLY_ARGS: 
    {
      static ST *last_PU = NULL;
      static TN *return_tn;
      ST *current_pu_st = Get_Current_PU_ST();
      if (last_PU == current_pu_st)
	return return_tn;
      else {
	Generate_Temp_Apply_Arg();
	last_PU = current_pu_st;
	INT par;
	INT ofst = 16;
	TN *ded_tn;

	// We always store the varargs immediately after FP
	// Store the address of vararg_temp_0 into new struct
	TN *vararg_ptr_tn = Build_TN_Of_Mtype(MTYPE_I8);
	Build_OP(TOP_addiu, vararg_ptr_tn, FP_TN, Gen_Literal_TN(-56, 4), ops);
	Exp_Store (MTYPE_I4, vararg_ptr_tn, tmp_apply_arg, 8, ops, 0);

	// Now, store all parameters into the new structure
	for (par = 0; par < MAX_NUMBER_OF_REGISTER_PARAMETERS; par ++) {
	  ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer,
				      par+5,
				      8 /* assume 64 bit registers */);
	  Exp_Store (MTYPE_I8, ded_tn, tmp_apply_arg, ofst, ops, 0);
	  ofst+= 8;
	}
	for (par = 0; par < MAX_NUMBER_OF_REGISTER_PARAMETERS; par ++) {
	  ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float,
				      par+13,
				      8 /* assume 64 bit registers */);
	  Exp_Store (MTYPE_F8, ded_tn, tmp_apply_arg, ofst, ops, 0);
	  ofst+= 8;
	}

	// return the pointer to the new structure
	return_tn = Build_TN_Of_Mtype(MTYPE_I8);
	TN *base_tn, *ofst_tn;
	base_tn = FP_TN;
	ofst_tn = Gen_Symbol_TN (tmp_apply_arg, 8, 0);
	Exp_OP2( Pointer_Size == 4 ? OPC_I4ADD : OPC_I8ADD,
		 return_tn, base_tn, ofst_tn, ops );

	// store the first argument (function); can not see how this is 
	// useful. builtin_apply always has the name of the function as the
	// first argument, so why store it again here?
	ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, 5,
				    8 /* assume 64 bit registers */);
	Exp_Store (MTYPE_I4, ded_tn, tmp_apply_arg, 0, ops, 0);
	return return_tn;
      }
    }
  case INTRN_APPLY:  
    {
      if (CG_opt_level > 0) {
	char asm_string[256];
	sprintf(asm_string, "__asm_builtin_apply_load");
	TN *opnd[1];
	opnd[0] = op1;
	OP *asm_op = Mk_VarOP(TOP_asm, 0, 1, NULL, opnd);
	Set_OP_volatile(asm_op);
	ASM_OP_ANNOT* asm_info = TYPE_PU_ALLOC(ASM_OP_ANNOT);
	bzero(asm_info, sizeof(ASM_OP_ANNOT));
	WN *asm_wn = WN_CreateAsm_Stmt (0, asm_string);
	ASM_OP_wn(asm_info) = asm_wn;
	OP_MAP_Set(OP_Asm_Map, asm_op, asm_info);
	OPS_Append_Op(ops, asm_op);
      } else {
	INT par;
	INT ofst = 8;
	TN *ded_tn;
	for (par = 0; par < MAX_NUMBER_OF_REGISTER_PARAMETERS; par ++) {
	  ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer,
				      par+5,
				      8 /* assume 64 bit registers */);
	  Build_OP(TOP_ld, ded_tn, op1, Gen_Literal_TN(ofst, 4), ops);
	  ofst+= 8;
	}
	for (par = 0; par < MAX_NUMBER_OF_REGISTER_PARAMETERS; par ++) {
	  ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float,
				      par+13,
				      8 /* assume 64 bit registers */);
	  Build_OP(TOP_ldc1, ded_tn, op1, Gen_Literal_TN(ofst, 4), ops);
	  ofst+= 8;
	}      
      }
      return NULL;
    }
  case INTRN_RETURN:
    {
      TN *ded_tn;
      ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer,
		      		  3, 8);
      Build_OP(TOP_ld, ded_tn, op0, Gen_Literal_TN(0, 4), ops);
      ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float,
		      		  1, 8);
      Build_OP(TOP_ldc1, ded_tn, op0, Gen_Literal_TN(8, 4), ops);
      return NULL;
    }
    return NULL;	
  default:
    FmtAssert(FALSE, ("Exp_Intrinsic_Call NYI"));
    return NULL; // if you can
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
  TOP top = OP_code(op);

  switch (top)
  {
  default:
    FmtAssert(FALSE, ("simulated OP %s not handled", TOP_Name(top)));
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
  INT num_bytes = 0;
  TOP top = OP_code(op);

  switch (top)
  {
  case TOP_spadjust:
    return 1;
  case TOP_asm:
#ifdef TARG_SL
   
    if (CG_check_quadword) {
       extern int Compute_Asm_Num (const char *asm_string, BOOL emit_phase) ;
	extern char* Generate_Asm_String(OP* asm_op, BB *bb);
	Is_True(((OP *)op)->bb, ("Simulated_Op_Real_Inst_Words:: bb is null"));
       char *asm_string = Generate_Asm_String((OP *)op, op->bb);
	num_bytes = Compute_Asm_Num((const char *)asm_string, FALSE);
	if (0) {
	  fprintf(stdout, "%s --%d\n", asm_string, num_bytes);
	}
    } else {
       num_bytes = 3;
    }
   	
#else
    /* We don't know how many instructions are "within" the asm, so we
       just assume 3 bytes. */
    num_bytes = 3;
#endif
    break;

  default:
    FmtAssert(FALSE, ("simulated OP %s not handled", TOP_Name(OP_code(op))));
  }

  return num_bytes;
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
  
  if (sym == NULL)
    return FALSE;

  Allocate_Object(sym);
  Base_Symbol_And_Offset_For_Addressing (sym, ofst, &base_sym, &base_ofst);

  /* We can assume that 'sym' is a spill location for an integer
     register, so we can check for l32i/s32i range. */
  
  if ((base_sym == SP_Sym || base_sym == FP_Sym) &&
      !ISA_LC_Value_In_Class(base_ofst, LC_simm16)) {
    return TRUE;
  }

  return FALSE;
}

void
Exp_Noop (OPS *ops)
{
  Build_OP (CGTARG_Noop_Top(), ops);
}

void Expand_Const (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert( TN_is_symbol(src), ("Expand_Const: src not a symbol TN"));

  TCON tcon = STC_val(TN_var(src));
  if (TCON_ty(tcon) == MTYPE_F4 && TCON_fval(tcon) == 0.0) {
    FmtAssert(TCON_ty(tcon) == mtype, ("Expand_Const: inconsistent mtypes"));
    Build_OP(TOP_mtc1, dest, Zero_TN, ops);
    return;
  }
  if (TCON_ty(tcon) == MTYPE_F8 && TCON_dval(tcon) == 0.0) {
    FmtAssert(TCON_ty(tcon) == mtype, ("Expand_Const: inconsistent mtypes"));
    Build_OP(TOP_dmtc1, dest, Zero_TN, ops);
    return;
  }
  Exp_Load(mtype, mtype, dest, TN_var(src), 0, ops, 0);
}

static BB* last_bb = NULL;
static TN *last_true_tn = NULL, *last_false_tn = NULL;
void
HB_Reinit_Pred ()
{
  last_true_tn = NULL;
  last_false_tn = NULL;
  last_bb = NULL;
}

void
Exp_True_False_Preds_For_Block(BB *bb, TN* &true_tn, TN * &false_tn)
{ 
  if (last_bb != bb)
    last_bb = bb;
  else {
    true_tn = last_true_tn;
    false_tn = last_false_tn;
    return;
  }
  OP* br_op = BB_branch_op(bb);
  if (!br_op)
    return;
  if (OP_code(br_op) == TOP_beq || OP_code(br_op) == TOP_bne) {
    OPS new_ops;
    OPS_Init(&new_ops);
    true_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    false_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    TN *tmp_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    TN *tmp1_tn;
    Build_OP(TOP_xor, tmp_tn, OP_opnd(br_op, 0), OP_opnd(br_op, 1), 
	     &new_ops);
    Build_OP(TOP_sltiu, false_tn, tmp_tn, Gen_Literal_TN(1, 4), &new_ops);
    Build_OP(TOP_sltu, true_tn, Zero_TN, tmp_tn, &new_ops);
    if (OP_code(br_op) == TOP_bne) {
      tmp1_tn = true_tn;
      true_tn = false_tn;
      false_tn = tmp1_tn;
    }
    BB_Insert_Ops_Before(bb, br_op, &new_ops);
    last_true_tn = true_tn;
    last_false_tn = false_tn;
    return;       
  } else if (OP_code(br_op) == TOP_bc1f || OP_code(br_op) == TOP_bc1t) {
    OPS new_ops;
    OPS_Init(&new_ops);
    true_tn = OP_opnd(br_op, 0);
    false_tn = Gen_Register_TN(ISA_REGISTER_CLASS_fcc, 4);
    // search backwards for the instruction that sets the conditon bit;
    // The false TN would be obtained by inverting that condition.
    OP *op;
    BOOL found_cond_set = FALSE;
    FOR_ALL_BB_OPs_REV(bb,op) {
      if (OP_results(op) && 
	  (OP_result(op, 0) == OP_opnd(br_op, 0))) {
	found_cond_set = TRUE;
	break;
      }
    }
    FmtAssert((found_cond_set==TRUE), 
	       ("Did not find instruction setting condition bit in BB"));    
    switch (OP_code(op)) {
    case TOP_c_le_d:
      Build_OP(TOP_c_lt_d, false_tn, OP_opnd(op, 1), OP_opnd(op, 0), &new_ops);
      break;
    case TOP_c_le_s:
      Build_OP(TOP_c_lt_s, false_tn, OP_opnd(op, 1), OP_opnd(op, 0), &new_ops);
      break;
    case TOP_c_lt_d:
      Build_OP(TOP_c_le_d, false_tn, OP_opnd(op, 1), OP_opnd(op, 0), &new_ops);
      break;
    case TOP_c_lt_s:
      Build_OP(TOP_c_le_s, false_tn, OP_opnd(op, 1), OP_opnd(op, 0), &new_ops);
      break;
    case TOP_c_eq_d:
      Build_OP(TOP_c_neq_d, false_tn, OP_opnd(op, 1), OP_opnd(op, 0), 
	       &new_ops);
      break;
    case TOP_c_eq_s:
      Build_OP(TOP_c_neq_s, false_tn, OP_opnd(op, 1), OP_opnd(op, 0), 
	       &new_ops);
      break;      
    default:
      FmtAssert(FALSE, ("Handle this case"));
      break;
    }
    TN *tmp_tn = Gen_Register_TN(ISA_REGISTER_CLASS_fcc, 4);
    if (OP_code(br_op) == TOP_bc1t) {
      tmp_tn = true_tn;
      true_tn = false_tn;
      false_tn = tmp_tn;
    }
    BB_Insert_Ops_Before(bb, br_op, &new_ops);
    last_true_tn = true_tn;
    last_false_tn = false_tn;
    return;           
  } else if (OP_code(br_op) == TOP_bgez || OP_code(br_op) == TOP_bltz) {
    OPS new_ops;
    OPS_Init(&new_ops);
    true_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    false_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    TN *tmp_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    TN *tmp1_tn;
    Build_OP(TOP_slt, tmp_tn, OP_opnd(br_op, 0), Zero_TN,
	     &new_ops);
    Build_OP(TOP_xori, true_tn, tmp_tn, Gen_Literal_TN(1, 4), &new_ops);
    Build_OP(TOP_slti, false_tn, OP_opnd(br_op, 0), Gen_Literal_TN(0, 4), 
	     &new_ops);
    if (OP_code(br_op) == TOP_bgez) {
      tmp1_tn = true_tn;
      true_tn = false_tn;
      false_tn = tmp1_tn;
    }
    BB_Insert_Ops_Before(bb, br_op, &new_ops);
    last_true_tn = true_tn;
    last_false_tn = false_tn;
    return;       
  }
  FmtAssert(FALSE, ("HANDLE THIS CASE"));
  true_tn = false_tn = Zero_TN;
}

BOOL
Target_Has_Immediate_Operand (WN *parent, WN *expr)
{
  OPERATOR opr = WN_operator(parent);
  return opr == OPR_ADD || opr == OPR_SUB || 
         opr == OPR_BAND || opr == OPR_BIOR || opr == OPR_BXOR ||
         opr == OPR_LT || opr == OPR_LE || opr == OPR_GT || opr == OPR_GE ||
         opr == OPR_LSHR || opr == OPR_ASHR || opr == OPR_SHL;
}

void 
Exp_Spadjust (TN *dest, TN *size, VARIANT variant, OPS *ops)
{
  Build_OP (TOP_spadjust, dest, SP_TN, size, ops);
  OP_variant(OPS_last(ops)) = variant;
}

/* Return a unique name for a symbol representing a literal. */
char *
Exp_Unique_Literal_Name (void)
{
  static int unique;
  static char name[32];

  sprintf(name, ".LC%d", unique);
  unique++;
  return name;
}

void
Exp_Generic_Pred_Calc(TN* result1, TN *result2, COMPARE_TYPE ctype,
                      TN *qual_pred, OPS* ops)
{ FmtAssert(FALSE,("Unimplemented")); }
  
  
void
Exp_Pred_Calc(TN* result, OP* cmp_op, COMPARE_TYPE ctype, BOOL false_result,
	      OPS* ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void Expand_Float_To_Float_Floor( TN* dest, TN* src,
				  TYPE_ID rtype, TYPE_ID desc, OPS* ops )
{
}
