/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: cgexp.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/cgexp.cxx,v $
 *
 * History:  extracted target-independent parts from expand.cxx
 *
 * Description:
 *
 * This file contains the internal switch-stmt of code expansion. Its interface
 * is 'Exp_OP', which takes an OP, expands it into a list of OPs which
 * are appended to the oplist passed in.
 *
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
#include "symtab.h"
#include "opcode.h"
#include "op.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "w2op.h"

BOOL Trace_Exp = FALSE;	/* General code expansion trace */

#ifdef TARG_NVISA
// fp ops are same as int ops
static BOOL Separate_FP_Expansion = FALSE;
#else
// fp ops go thru different path from int ops
static BOOL Separate_FP_Expansion = TRUE;
#endif

/* ====================================================================
 *
 * Expand_OP
 *
 * Given an operator, expand it.  There are two possibilities:
 *
 *  1)	A machine operator requires no further expansion.
 *
 *  2)  An OP_* operator is expanded into a sequence of TOP_* instructions.
 *
 * ====================================================================
 */

static void
Expand_OP (OPCODE opcode, TN *result, TN *op1, TN *op2, TN *op3, VARIANT variant, OPS *ops)
{
  TYPE_ID desc = OPCODE_desc(opcode);
  TYPE_ID rtype = OPCODE_rtype(opcode);

  switch (OPCODE_operator(opcode)) {
  case OPR_TRUEBR:
  case OPR_FALSEBR:
	Expand_Branch (op1, op2, op3, variant, ops);
	break;
  case OPR_GOTO:
  case OPR_GOTO_OUTER_BLOCK:
	Expand_Branch (op1, op2, op3, V_BR_ALWAYS, ops);
	break;
  case OPR_LDA:
	Expand_Lda (result, op1, ops);
	break;
  case OPR_LDA_LABEL:
	Expand_Lda_Label (result, op1, ops);
	break;
  case OPR_INTCONST:
#if defined(TARG_NVISA)
	Expand_Mtype_Immediate (result, op1, rtype, ops);
#elif defined(TARG_SL)
	Expand_Immediate (result, op1, rtype, ops);
#elif defined(TARG_PPC32) 
	void Expand_Immediate (TN *, TN *, BOOL, OPS *, TYPE_ID);
	Expand_Immediate (result, op1, TRUE /* is_signed */, ops, rtype);
#else
	Expand_Immediate (result, op1, TRUE /* is_signed */, ops);
#endif
	break;
  case OPR_CONST:
	Expand_Const (result, op1, rtype, ops);
	break;
  case OPR_ADD:
	if (MTYPE_is_float(rtype) && Separate_FP_Expansion)
		Expand_Flop (opcode, result, op1, op2, op3, ops);
	else
		Expand_Add (result, op1, op2, rtype, ops);
	break;
  case OPR_SUB:
	if (MTYPE_is_float(rtype) && Separate_FP_Expansion)
		Expand_Flop (opcode, result, op1, op2, op3, ops);
	else
		Expand_Sub (result, op1, op2, rtype, ops);
	break;
  case OPR_NEG:
	Expand_Neg (result, op1, rtype, ops);
	break;
  case OPR_SHL:
	Expand_Shift (result, op1, op2, rtype, shift_left, ops);
	break;
  case OPR_ASHR:
	Expand_Shift (result, op1, op2, rtype, shift_aright, ops);
	break;
  case OPR_LSHR:
	Expand_Shift (result, op1, op2, rtype, shift_lright, ops);
	break;
#ifdef TARG_X8664
  case OPR_RROTATE:
	Expand_Rrotate (result, op1, op2, rtype, desc, ops);
	break;
#endif
  case OPR_ILOADX:
  case OPR_ILOAD:
  case OPR_LDID:
	if ( V_align_all(variant) != 0 ) {
		Expand_Misaligned_Load ( opcode, result, op1, op2, variant, ops);
	}
	else {
#if defined(TARG_MIPS) || defined(TARG_X8664) || defined(TARG_PPC32)
		Expand_Load (opcode, result, op1, op2, ops);
#else
		Expand_Load (opcode, result, op1, op2, variant, ops);
#endif
	}
	break;
  case OPR_ISTOREX:
  case OPR_ISTORE:
  case OPR_STID:
	if ( variant & V_HIGH64);
	if ( V_align_all(variant) != 0 ) {
#if defined(TARG_SL)
          if (CG_check_packed)
  	    Is_True(0, ("SL does not handle unaligned store"));
#endif
		Expand_Misaligned_Store (desc, op1, op2, op3, variant, ops);
	}
	else {
#if defined(TARG_MIPS) || defined(TARG_PPC32)
		Expand_Store (desc, op1, op2, op3, ops);
#else
		Expand_Store (desc, op1, op2, op3, variant, ops);
#endif
	}
	break;
  case OPR_ABS:
#ifdef TARG_LOONGSON
	if (MTYPE_is_float(rtype))
		Expand_Flop (opcode, result, op1, op2, op3, ops);
	else
#endif
	Expand_Abs (result, op1, rtype, ops);
	break;
  case OPR_MPY:
#ifdef TARG_X8664
	if (MTYPE_is_float(rtype) || MTYPE_is_mmx_vector(rtype))
#else
	if (MTYPE_is_float(rtype) && Separate_FP_Expansion)
#endif
		Expand_Flop (opcode, result, op1, op2, op3, ops);
	else
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
		Expand_Multiply (result, op1, op2, rtype, ops, opcode);
#else
	Expand_Multiply (result, op1, op2, rtype, ops);
#endif
	break;
  case OPR_HIGHMPY:
	Expand_High_Multiply (result, op1, op2, rtype, ops);
	break;
  case OPR_REM:
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
	Expand_Rem (result, op1, op2, rtype, ops, opcode);
#else
	Expand_Rem (result, op1, op2, rtype, ops);
#endif
	break;
  case OPR_MOD:
	if (MTYPE_is_signed(rtype))
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
		Expand_Mod (result, op1, op2, rtype, ops, opcode);
#else
	Expand_Mod (result, op1, op2, rtype, ops );
#endif
	else
		// unsigned MOD acts like REM
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
		Expand_Rem (result, op1, op2, rtype, ops, opcode);
#else
	Expand_Rem (result, op1, op2, rtype, ops);
#endif
	break;
  case OPR_DIV:
	if (MTYPE_is_float(rtype))
		Expand_Flop (opcode, result, op1, op2, op3, ops);
	else
		Expand_Divide (result, op1, op2, rtype, ops);
	break;
  case OPR_DIVREM:
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
	Expand_DivRem(result, op1, op2, op3, rtype, ops, opcode);
#else
        Expand_DivRem(result, op1, op2, op3, rtype, ops);
#endif
	break;
  case OPR_SQRT:
	Expand_Sqrt (result, op1, rtype, ops);
	break;
  case OPR_LNOT:
	Expand_Logical_Not (result, op1, variant, ops);
	break;
  case OPR_LAND:
	Expand_Logical_And (result, op1, op2, variant, ops);
	break;
  case OPR_LIOR:
	Expand_Logical_Or (result, op1, op2, variant, ops);
	break;
  case OPR_BNOT:
	Expand_Binary_Complement (result, op1, rtype, ops);
	break;
  case OPR_BAND:
	Expand_Binary_And (result, op1, op2, rtype, ops);
	break;
  case OPR_BIOR:
	Expand_Binary_Or (result, op1, op2, rtype, ops);
	break;
  case OPR_BXOR:
	Expand_Binary_Xor (result, op1, op2, rtype, ops);
	break;
  case OPR_BNOR:
	Expand_Binary_Nor (result, op1, op2, rtype, ops);
	break;
  case OPR_LT:
	if (MTYPE_is_float(desc))
		Expand_Float_Less (result, op1, op2, variant, desc, ops);
	else
		Expand_Int_Less (result, op1, op2, desc, ops);
	break;
  case OPR_LE:
	if (MTYPE_is_float(desc))
		Expand_Float_Less_Equal (result, op1, op2, variant, desc, ops);
	else
		Expand_Int_Less_Equal (result, op1, op2, desc, ops);
	break;
  case OPR_EQ:
	if (MTYPE_is_float(desc))
		Expand_Float_Equal (result, op1, op2, variant, desc, ops);
	else if (desc == MTYPE_B)
		Expand_Bool_Equal (result, op1, op2, ops);
	else
		Expand_Int_Equal (result, op1, op2, desc, ops);
	break;
  case OPR_NE:
	if (MTYPE_is_float(desc))
		Expand_Float_Not_Equal (result, op1, op2, variant, desc, ops);
	else if (desc == MTYPE_B)
		Expand_Bool_Not_Equal (result, op1, op2, ops);
	else
		Expand_Int_Not_Equal (result, op1, op2, desc, ops);
	break;
  case OPR_GE:
	if (MTYPE_is_float(desc))
		Expand_Float_Greater_Equal (result, op1, op2, variant, desc, ops);
	else
		Expand_Int_Greater_Equal (result, op1, op2, desc, ops);
	break;
  case OPR_GT:
	if (MTYPE_is_float(desc))
		Expand_Float_Greater (result, op1, op2, variant, desc, ops);
	else
		Expand_Int_Greater (result, op1, op2, desc, ops);
	break;

  case OPR_CVTL:
#if defined(TARG_PPC32)
	Expand_Convert_Length ( result, op1, op2, rtype, rtype, ops);
#else
	Expand_Convert_Length ( result, op1, op2, rtype, MTYPE_is_signed(rtype), ops);
#endif
	break;
  case OPR_CVT:
	Is_True(rtype != MTYPE_B, ("conversion to bool unsupported"));
	if (MTYPE_is_float(rtype) && MTYPE_is_float(desc)) {
#if defined(TARG_X8664) || defined(TARG_SL) || defined(TARG_MIPS) || defined(TARG_PPC32) || defined(TARG_LOONGSON)
		Expand_Float_To_Float (result, op1, rtype, desc, ops);
#else
		Expand_Float_To_Float (result, op1, rtype, ops);
#endif
	}
	else if (MTYPE_is_float(rtype)) {
		// desc is int
		Expand_Int_To_Float (result, op1, desc, rtype, ops);
	}
	else if (MTYPE_is_float(desc)) {
		// rtype is int
		Expand_Float_To_Int_Cvt (result, op1, rtype, desc, ops);
	}
	else if (desc == MTYPE_B) {
		// desc is bool
		Expand_Bool_To_Int (result, op1, rtype, ops);
	}
#ifdef TARG_X8664
	else if (MTYPE_is_vector (rtype)) {
		// vector conversions not covered above
		Expand_Conv_To_Vector (result, op1, desc, rtype, ops);
	}
	else if (MTYPE_is_vector (desc)) {
		Expand_Conv_From_Vector (result, op1, desc, rtype, ops);
	}
#endif
	else {
		// both are int
  		// zero-extend when enlarging an unsigned value, or 
  		//   converting to smaller unsigned vlaue (e.g U4I8CVT)
		// else sign-extend.
#if defined(TARG_NVISA)
		// have to change register size, so not an in-place truncation
		Expand_Convert (result, rtype, op1, desc, ops);
#elif defined(TARG_PPC32)
		Expand_Convert_Length (result, op1, op2, rtype, desc, ops);
#else
		Expand_Convert_Length ( result, op1, op2, 
#ifdef TARG_LOONGSON
			desc,
#else
			rtype,
#endif
			(MTYPE_is_signed(desc)
#ifdef TARG_IA64
			 && (MTYPE_bit_size(desc) < MTYPE_bit_size(rtype) ) ),
			ops); // OSP_171
#else
		|| (MTYPE_bit_size(desc) > MTYPE_bit_size(rtype) ) ),
		     ops);
#endif
#endif // TARG_NVISA TARG_PPC32
	}
	break;
#ifdef TARG_X8664
  case OPR_TAS:
        if (MTYPE_is_vector(rtype)) // bugs 11797 11876
          Expand_Int_To_Vect_Tas(result, op1, rtype, ops);
        else if (MTYPE_is_integral(rtype))
          Expand_Float_To_Int_Tas(result, op1, rtype, ops);
        else Expand_Int_To_Float_Tas(result, op1, rtype, ops);
	break;
#endif
  case OPR_RND:
	Expand_Float_To_Int_Round (result, op1, rtype, desc, ops);
	break;
  case OPR_TRUNC:
	Expand_Float_To_Int_Trunc (result, op1, rtype, desc, ops);
	break;
  case OPR_CEIL:
	Expand_Float_To_Int_Ceil (result, op1, rtype, desc, ops);
	break;
  case OPR_FLOOR:
#ifdef TARG_X8664
    if( MTYPE_is_float( rtype ) ){
      if( MTYPE_is_F10( rtype ) )
	Expand_Float_To_Float_Floorl( result, op1, rtype, desc, ops );
      else if( rtype == MTYPE_F8 )
	Expand_Float_To_Float_Floor( result, op1, rtype, desc, ops );
      else
	Expand_Float_To_Float_Floorf( result, op1, rtype, desc, ops );
      break;
    }	
#elif defined (TARG_MIPS) && !defined(TARG_SL)
        if (MTYPE_is_float (rtype)) {
	  Expand_Float_To_Float_Floor (result, op1, rtype, desc, ops);
	  break;
	}
#endif
	Expand_Float_To_Int_Floor (result, op1, rtype, desc, ops);
	break;
  case OPR_MIN:
	Expand_Min (result, op1, op2, rtype, ops);
	break;
  case OPR_MAX:
	Expand_Max (result, op1, op2, rtype, ops);
	break;

  case OPR_MINMAX:
	Expand_MinMax (result, op1, op2, op3, rtype, ops);
	break;

  case OPR_SELECT:
	Expand_Select (result, op1, op2, op3, rtype, V_select_uses_fcc(variant), ops);
	break;

  case OPR_MADD:
  case OPR_NMADD:
  case OPR_MSUB:
  case OPR_NMSUB:
  case OPR_RECIP:
  case OPR_RSQRT:
#ifdef TARG_X8664
  case OPR_ATOMIC_RSQRT:	// bug 6123
#endif
	Expand_Flop (opcode, result, op1, op2, op3, ops);
	break;

#ifdef TARG_X8664
  case OPR_REPLICATE:
        Expand_Replicate (opcode, result, op1, ops);
	break;

  case OPR_REDUCE_ADD:
        Expand_Reduce_Add (opcode, result, op1, ops);
	break;

  case OPR_REDUCE_MPY:
        Expand_Reduce_Mpy (opcode, result, op1, ops);
	break;

  case OPR_REDUCE_MAX:
        Expand_Reduce_Max (opcode, result, op1, ops);
	break;

  case OPR_REDUCE_MIN:
        Expand_Reduce_Min (opcode, result, op1, ops);
	break;

#endif /* TARG_X8664 */
#ifdef TARG_X8664
  case OPR_COMPLEX:
	FmtAssert(opcode == OPC_V16C8PAIR,
		("Expand_OP:  unexpected PAIR opcode %s", OPCODE_name(opcode)));
	extern void Expand_Complex(OPCODE, TN*, TN*, TN*,OPS*);
	extern void Expand_Firstpart(OPCODE, TN*, TN*, OPS*);
	extern void Expand_Secondpart(OPCODE, TN*, TN*, OPS*);
	Expand_Complex(opcode, result, op1, op2, ops);
	break;
  case OPR_REALPART:
	FmtAssert(opcode == OPC_F8FIRSTPART,
		("Expand_OP:  unexpected FIRSTPART opcode %s", OPCODE_name(opcode)));
	Expand_Firstpart(opcode, result, op1, ops);
	break;
  case OPR_IMAGPART:
	FmtAssert(opcode == OPC_F8SECONDPART,
		("Expand_OP:  unexpected SECONDPART opcode %s", OPCODE_name(opcode)));
	Expand_Secondpart(opcode, result, op1, ops);
	break;
#endif
  default:
	FmtAssert(FALSE, 
		("Expand_OP:  unexpected opcode %s", OPCODE_name(opcode)));
	break;
  }

#ifdef TARG_X8664
  /* For comparsions, the upper 32-bit of the result is ignored.
     Thus under -m32, the value of the higher 32-bit part is undefined.
     (bug#2499)
  */
  if( OPCODE_is_compare( opcode ) &&
      OP_NEED_PAIR( rtype ) ){
    TN* result_hi = Create_TN_Pair( result, rtype );
    Exp_Immediate( result_hi, Gen_Literal_TN(0,4), FALSE, ops );
  }
#endif // TARG_X8664
}


extern void 
Exp_OP (OPCODE opcode, TN *result, TN *op1, TN *op2, TN *op3, VARIANT variant, OPS *ops)
{
  TOP top = OPCODE_To_TOP (opcode);

  if (Trace_Exp) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "expand %s: ", OPCODE_name(opcode)); 
	if (result) Print_TN(result,FALSE);
  	fprintf(TFile, " :- ");
	if (op1) Print_TN(op1,FALSE);
	fprintf(TFile, " ");
	if (op2) Print_TN(op2,FALSE);
	fprintf(TFile, " ");
	if (op3) Print_TN(op3,FALSE);
	fprintf(TFile, " ");
	if (variant) fprintf(TFile, "(0x%llx)", (INT64)variant);
	fprintf(TFile, "\n");
  }

  if (top != TOP_UNDEFINED) {
	TN *args[5];
	OP *op;
	INT i = 0;

	// Build up a list of arguments to Mk_OP. The code here
	// relies on the fact that Mk_OP will ignore unnecessary
	// trailing arguments.
	if (result) args[i++] = result;
#if !defined(TARG_X8664)
	if (TOP_is_predicated(top)) {
	  Is_True(OP_PREDICATE_OPND == 0, ("predicate operand is not 0"));
	  args[i++] = True_TN;
	}
#endif
	args[i++] = op1;
	args[i++] = op2;
	args[i] = op3;
	op = Mk_OP(top, args[0], args[1], args[2], args[3], args[4]);
	if (OP_defs_fpu_int(op)) Set_TN_is_fpu_int(result);
    	/* Add the new OPs to the end of the list passed in */
	OPS_Append_Op(ops, op);
  	if (Trace_Exp) {
		#pragma mips_frequency_hint NEVER
		fprintf(TFile, " into "); Print_OP (op);
	}
  }
  else {
	/* Sometimes the whole ops sequence is needed in Expand_OP.
	So instead of create a new OPS structure, the old one is added.
	*/
	if (OPS_length(ops)==0) {
#ifdef TARG_LOONGSON
  	/* For TARG_LOONGSON, when expanding op, the 'ops' should be New_OPs
        because 'Start_New_Basic_Block' will check New_OPs to decide if 
        a new  BB should be created. So, we cannot create a new OPs here.
        */
	Expand_OP (opcode, result, op1, op2, op3, variant, ops);
  	if (Trace_Exp) {
  		#pragma mips_frequency_hint NEVER
  		OP *op;
  		FOR_ALL_OPS_OPs (ops, op) {
  			fprintf(TFile, " into "); Print_OP (op);
  		}
  	}

#else
		OPS new_ops;
  		OPS_Init(&new_ops);
  		Expand_OP (opcode, result, op1, op2, op3, variant, &new_ops);
  		if (Trace_Exp) {
  			#pragma mips_frequency_hint NEVER
  			OP *op;
  			FOR_ALL_OPS_OPs (&new_ops, op) {
  				fprintf(TFile, " into "); Print_OP (op);
  			}
  		}
		/* check if there is at least one OP in the expansion */
		if (OPS_first(&new_ops) != NULL) {
			/* Add the new OPs to the end of the list passed in */
			OPS_Append_Ops(ops, &new_ops);
  		}
#endif
  	}
  	else {
		OP *last_OP = OPS_last(ops);
  		Expand_OP (opcode, result, op1, op2, op3, variant, ops);
  		if (Trace_Exp) {
  			if (OP_next(last_OP)!=NULL) {
  				#pragma mips_frequency_hint NEVER
	  			OP *op;
		  		for (op = OP_next(last_OP); op && (OPS_last(ops) == NULL || 
                                                                   op != OP_next(OPS_last(ops))); op = OP_next(op)){
			  		fprintf(TFile, " into "); Print_OP (op);
			  	}
		  	}
		}
	}
  }
}
