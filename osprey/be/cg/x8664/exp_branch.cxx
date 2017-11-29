/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
#include "whirl2ops.h"
#include "config_opt.h"      // For Force_IEEE_Comparisons

void
Initialize_Branch_Variants(void)
{
	// nothing to do
}

// Check that compare is of proper form,
// and return TOP to use for the compare.
// May modify the variant and src tns.
TOP
Pick_Compare_TOP (VARIANT *variant, TN **src1, TN **src2, OPS *ops)
{
  TOP cmp = TOP_UNDEFINED;

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
    }
  }

  // If branch variant condition is V_BR_ALWAYS the caller expects 
  // a TOP_UNDEFINED, so it can create an unconditional jump.
  // Also, this guards against dereferencing of *src2 when NULL.
  if (*src2 == NULL)
    return TOP_UNDEFINED;

  return cmp;
}


/* Split a 64-bit cmp into two 32-bit cmps.
 */
static void Expand_Split_Branch( TOP cmp_opcode, TN* src1_lo, TN* src2_lo,
				 TOP jmp_opcode, TN* targ, OPS* ops )
{
  switch( cmp_opcode ){
  case TOP_cmp64:   cmp_opcode = TOP_cmp32;   break;
  case TOP_cmpi64:  cmp_opcode = TOP_cmpi32;  break;
  case TOP_test64:  cmp_opcode = TOP_test32;  break;
  case TOP_testi64: cmp_opcode = TOP_testi32; break;
  default:
    FmtAssert( false, ("NYI") );
  }

  TN* src1_hi = Get_TN_Pair( src1_lo );
  TN* src2_hi = Get_TN_Pair( src2_lo );

  if( src2_hi == NULL ){
    if( TN_has_value( src2_lo ) ){
      const INT64 val = TN_value( src2_lo ) >> 32;
      src2_hi = Gen_Literal_TN( val, 4 );

    } else {
      DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	       TN_number(src2_lo) );
      src2_hi = Build_TN_Like( src2_lo );
      Build_OP( TOP_ldc32, src2_hi, Gen_Literal_TN(0,4), ops );    
    }
  }

  if( src1_hi == NULL ){
    DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	     TN_number(src1_lo) );
    src1_hi = Build_TN_Like( src1_lo );
    Build_OP( TOP_ldc32, src1_hi, Gen_Literal_TN(0,4), ops );    
  }

  TN* rflags = Rflags_TN();

  BB* bb_entry = Cur_BB;
  BB* bb_then  = Gen_And_Insert_BB_After( bb_entry );
  BB* bb_then1 = Gen_And_Insert_BB_After( bb_then );

  BB* bb_exit  = Gen_And_Insert_BB_After( bb_then1 );
  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

  FmtAssert( TN_is_label(targ), ("NYI") );
  const LABEL_IDX targ_label = TN_label( targ );

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = targ_label;

  BB_branch_wn(bb_then) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_then)) = NULL;
  WN_label_number(BB_branch_wn(bb_then)) = bb_exit_label;

  BB_branch_wn(bb_then1) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_then1)) = NULL;
  WN_label_number(BB_branch_wn(bb_then1)) = targ_label;

  // Compare the higher 32-bit here.
  {
    if( jmp_opcode != TOP_je ){
      TOP jmp = jmp_opcode;
      switch( jmp ){
      case TOP_jge: jmp = TOP_jg;  break;
      case TOP_jae: jmp = TOP_ja;  break;
      case TOP_jle: jmp = TOP_jl;  break;
      case TOP_jbe: jmp = TOP_jb;  break;
      }

      Build_OP( cmp_opcode, rflags, src1_hi, src2_hi, ops );
      Build_OP( jmp, rflags, targ, ops );
    }

    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );

    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );
  }

  // Compare the higher 32-bit here.
  if( jmp_opcode != TOP_jne ){
    OPS* bb_then_ops = &New_OPs;
    TOP rev_jmp_opcode = CGTARG_Invert( jmp_opcode );

    switch( rev_jmp_opcode ){
    case TOP_jge: rev_jmp_opcode = TOP_jg;  break;
    case TOP_jae: rev_jmp_opcode = TOP_ja;  break;
    case TOP_jle: rev_jmp_opcode = TOP_jl;  break;
    case TOP_jbe: rev_jmp_opcode = TOP_jb;  break;
    }

    Build_OP( cmp_opcode, rflags, src1_hi, src2_hi, bb_then_ops );
    Build_OP( rev_jmp_opcode, rflags, Gen_Label_TN( bb_exit_label, 0 ), bb_then_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_then, bb_then_ops );
    OPS_Init( bb_then_ops );
  }

  // Compare the lower 32-bit using unsigned comparision.
  {
    OPS* bb_then1_ops = &New_OPs;
    TOP jmp = jmp_opcode;
    switch( jmp_opcode ){
    case TOP_jg:  jmp = TOP_ja;  break;
    case TOP_jge: jmp = TOP_jae; break;
    case TOP_jl:  jmp = TOP_jb;  break;
    case TOP_jle: jmp = TOP_jbe; break;
    }

    Build_OP( cmp_opcode, rflags, src1_lo, src2_lo, bb_then1_ops );
    Build_OP( jmp, rflags, targ, bb_then1_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_then1, bb_then1_ops );
    OPS_Init( bb_then1_ops );
  }

  Cur_BB = bb_exit;
}

/* Handle unordered compare&branch */
static void Expand_Ordered_Branch( TOP cmp_opcode, TN* src1, TN* src2,
				   TOP jmp_opcode, TN* targ, OPS* ops )
{
  TN* rflags = Rflags_TN();


  if (jmp_opcode == TOP_jb || jmp_opcode == TOP_jbe ||
      jmp_opcode == TOP_jae || jmp_opcode == TOP_ja) {

    Build_OP( cmp_opcode, rflags, src1, src2, ops );
    Build_OP( jmp_opcode, rflags, targ, ops );

  } else if (jmp_opcode == TOP_jne) {

    BB* bb_entry = Cur_BB;
    BB* bb_first = Gen_And_Append_BB( bb_entry );
    BB* bb_second = Gen_And_Append_BB( bb_first );
    BB* bb_exit = Gen_And_Append_BB( bb_second );
    
    const LABEL_IDX bb_first_label = Gen_Label_For_BB( bb_first );
    const LABEL_IDX bb_second_label = Gen_Label_For_BB( bb_second );
    const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

    OPS* bb_first_ops;
    OPS* bb_second_ops;

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = bb_second_label;
    
    BB_branch_wn(bb_first) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_first)) = NULL;
    WN_label_number(BB_branch_wn(bb_first)) = bb_exit_label;

    BB_branch_wn(bb_second) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_second)) = NULL;
    WN_label_number(BB_branch_wn(bb_second)) = TN_label( targ );

    // Build BB_entry
    Build_OP( cmp_opcode, rflags, src1, src2, ops );
    Build_OP( TOP_jp, rflags, Gen_Label_TN( bb_second_label, 0 ), ops );
    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );    
    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );

    // Build BB_first
    bb_first_ops = &New_OPs;
    Build_OP( cmp_opcode, rflags, src1, src2, bb_first_ops );
    Build_OP( TOP_je, rflags, Gen_Label_TN( bb_exit_label, 0 ), bb_first_ops );
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_first, bb_first_ops );
    OPS_Init( bb_first_ops );

    // Build BB_second
    bb_second_ops = &New_OPs;
    Build_OP( TOP_jmp, targ, bb_second_ops );
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_second, bb_second_ops );
    OPS_Init( bb_second_ops );

    Cur_BB = bb_exit;

  } else if (jmp_opcode == TOP_je) {

    BB* bb_entry = Cur_BB;
    BB* bb_first = Gen_And_Append_BB( bb_entry );
    BB* bb_second = Gen_And_Append_BB( bb_first );
    BB* bb_exit = Gen_And_Append_BB( bb_second );
    
    const LABEL_IDX bb_first_label = Gen_Label_For_BB( bb_first );
    const LABEL_IDX bb_second_label = Gen_Label_For_BB( bb_second );
    const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

    OPS* bb_first_ops;
    OPS* bb_second_ops;

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;
    
    BB_branch_wn(bb_first) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_first)) = NULL;
    WN_label_number(BB_branch_wn(bb_first)) = bb_exit_label;

    BB_branch_wn(bb_second) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_second)) = NULL;
    WN_label_number(BB_branch_wn(bb_second)) = TN_label( targ );

    // Build BB_entry
    Build_OP( cmp_opcode, rflags, src1, src2, ops );
    Build_OP( TOP_jne, rflags, Gen_Label_TN( bb_exit_label, 0 ), ops );
    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );    
    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );

    // Build BB_first
    bb_first_ops = &New_OPs;
    Build_OP( cmp_opcode, rflags, src1, src2, bb_first_ops );
    Build_OP( TOP_jp, rflags, Gen_Label_TN( bb_exit_label, 0 ), bb_first_ops );
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_first, bb_first_ops );
    OPS_Init( bb_first_ops );

    // Build BB_second
    bb_second_ops = &New_OPs;
    Build_OP( TOP_jmp, targ, bb_second_ops );
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_second, bb_second_ops );
    OPS_Init( bb_second_ops );

    Cur_BB = bb_exit;

  }

  return;
}

void Expand_Branch ( TN *targ, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  const BOOL false_br = V_false_br(variant);
  VARIANT cond = V_br_condition(variant);
  TN *tmp;

  /* Trace if required: */
  if ( Trace_Exp2 ) {
    fprintf ( TFile, "<cgexp> Translating %s branch:\n",
        (false_br ? "false" : "true") );
  }

  FmtAssert( cond <= V_BR_LAST, ("unexpected variant in Expand_Branch"));
  FmtAssert( cond != V_BR_NONE, ("BR_NONE variant in Expand_Branch"));

  const TOP cmp = Pick_Compare_TOP (&cond, &src1, &src2, ops);
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
      Build_OP (TOP_jmp, targ, ops);
    }
    break;
  case V_BR_PEQ:
  case V_BR_PNE:
  case V_BR_P_TRUE:
    FmtAssert(FALSE, ("unimplemented branch variant in Expand_Branch"));
    break;
  default:
    {
      TOP jmp_opcode = TOP_UNDEFINED;
      TOP cmp_opcode;

      BOOL is_64bit = FALSE;

      switch ( cond ){
      case V_BR_I4GE:   
      case V_BR_U4GE:
      case V_BR_I4GT:
      case V_BR_U4GT:
      case V_BR_I4LE:
      case V_BR_U4LE:
      case V_BR_I4LT:
      case V_BR_U4LT:
      case V_BR_I4EQ:
      case V_BR_U4EQ:
      case V_BR_I4NE:
      case V_BR_U4NE:
      case V_BR_FEQ:
      case V_BR_FNE:
      case V_BR_FGT:
      case V_BR_FGE:
      case V_BR_FLT:
      case V_BR_FLE:
	is_64bit = FALSE;
	break;

      case V_BR_I8GE:	
      case V_BR_U8GE:	
      case V_BR_I8GT:	
      case V_BR_U8GT:	
      case V_BR_I8LE:	
      case V_BR_U8LE:	
      case V_BR_I8LT:	
      case V_BR_U8LT:	
      case V_BR_I8EQ:
      case V_BR_U8EQ:	
      case V_BR_I8NE:
      case V_BR_U8NE:	
      case V_BR_DEQ:
      case V_BR_DNE:
      case V_BR_DGT:
      case V_BR_DGE:
      case V_BR_DLT:
      case V_BR_DLE:
	is_64bit = TRUE;
	break;

      case V_BR_XEQ:
      case V_BR_XNE:
      case V_BR_XGT:
      case V_BR_XGE:
      case V_BR_XLT:
      case V_BR_XLE:

      case V_BR_QEQ:
      case V_BR_QNE:
      case V_BR_QGT:
      case V_BR_QGE:
      case V_BR_QLT:
      case V_BR_QLE:
	/* Treat all the 'long double' cmp as 64-bit cmp, since they all
	   use the rflags register.
	*/
	is_64bit = TRUE;
	break;

      default:
	FmtAssert(FALSE, ("unimplemented branch variant in Expand_Branch"));
      }

      if( TN_has_value(src1) ){
	tmp = Gen_Register_TN( ISA_REGISTER_CLASS_integer,
				   is_64bit ? 8 : 4 );
	Build_OP( is_64bit ? TOP_ldc64 : TOP_ldc32, tmp, src1, ops );
	src1 = tmp;
      }

      if( TN_has_value(src2) ){
	const INT64 imm = TN_value(src2);

	if( imm == 0 ){
	  cmp_opcode = is_64bit ? TOP_test64 : TOP_test32;
	  src2 = src1;

	} else {
	  cmp_opcode = is_64bit ? TOP_cmpi64 : TOP_cmpi32;

	  const ISA_OPERAND_INFO* oinfo = ISA_OPERAND_Info( cmp_opcode );
	  const ISA_OPERAND_VALTYP* vtype = ISA_OPERAND_INFO_Operand(oinfo, 1);
	  const ISA_LIT_CLASS lc = ISA_OPERAND_VALTYP_Literal_Class(vtype);

	  if( !ISA_LC_Value_In_Class(imm, lc) ){
	    TN* tmp_tn = Build_TN_Like( src1 );

	    if( is_64bit && Is_Target_32bit() ){
	      extern void Expand_Split_UOP( OPERATOR, TYPE_ID, TN*, TN*, OPS* );
	      Expand_Split_UOP( OPR_INTCONST, MTYPE_I8, tmp_tn, src2, ops );
	    } else {
	      Build_OP( is_64bit ? TOP_ldc64 : TOP_ldc32, tmp_tn, src2, ops );
	    }

	    cmp_opcode = is_64bit ? TOP_cmp64 : TOP_cmp32;
	    src2 = tmp_tn;	      
	  }
	}

      } else {
	cmp_opcode = is_64bit ? TOP_cmp64 : TOP_cmp32;
      }

      // integer or floating point conditional branch

      FmtAssert( cmp == TOP_UNDEFINED, ("cmp is defined") );

      BOOL flip_opnds = FALSE;

      switch( cond ){
      case V_BR_I4GE:   
      case V_BR_I8GE:	jmp_opcode = TOP_jge; break;
      case V_BR_U4GE:
      case V_BR_U8GE:	jmp_opcode = TOP_jae; break;
      case V_BR_I4GT:
      case V_BR_I8GT:	jmp_opcode = TOP_jg; break;
      case V_BR_U4GT:
      case V_BR_U8GT:	jmp_opcode = TOP_ja; break;
      case V_BR_I4LE:
      case V_BR_I8LE:	jmp_opcode = TOP_jle; break;
      case V_BR_U4LE:
      case V_BR_U8LE:	jmp_opcode = TOP_jbe; break;
      case V_BR_I4LT:
      case V_BR_I8LT:	jmp_opcode = TOP_jl; break;
      case V_BR_U4LT:
      case V_BR_U8LT:	jmp_opcode = TOP_jb; break;
      case V_BR_I4EQ:
      case V_BR_I8EQ:
      case V_BR_U4EQ:
      case V_BR_U8EQ:	jmp_opcode = TOP_je; break;
      case V_BR_I4NE:
      case V_BR_I8NE:
      case V_BR_U4NE:
      case V_BR_U8NE:	jmp_opcode = TOP_jne; break;

      case V_BR_QEQ:
      case V_BR_XEQ:
	jmp_opcode = false_br ? TOP_jne : TOP_je;
	cmp_opcode = TOP_fucomi;
	break;
      case V_BR_FEQ:
      case V_BR_DEQ:
	jmp_opcode = false_br ? TOP_jne : TOP_je;
	cmp_opcode = Is_Target_SSE2()
	  ? ( is_64bit ? TOP_comisd : TOP_comiss ) : TOP_fucomi;
	break;
      case V_BR_QNE:
      case V_BR_XNE:
	jmp_opcode = false_br ? TOP_je : TOP_jne;
	cmp_opcode = TOP_fucomi;
	break;
      case V_BR_FNE:
      case V_BR_DNE:
	jmp_opcode = false_br ? TOP_je : TOP_jne;
	cmp_opcode = Is_Target_SSE2()
	  ? ( is_64bit ? TOP_comisd : TOP_comiss ) : TOP_fucomi;
	break;
      case V_BR_QGT:
      case V_BR_XGT:
	jmp_opcode = false_br ? TOP_jbe : TOP_ja;
	cmp_opcode = TOP_fucomi;
	break;
      case V_BR_FGT:
      case V_BR_DGT:
	jmp_opcode = false_br ? TOP_jbe : TOP_ja;
	cmp_opcode = Is_Target_SSE2()
	  ? ( is_64bit ? TOP_comisd : TOP_comiss ) : TOP_fucomi;
	break;
      case V_BR_QGE:
      case V_BR_XGE:
	jmp_opcode = false_br ? TOP_jb : TOP_jae;
	cmp_opcode = TOP_fucomi;
	break;
      case V_BR_FGE:
      case V_BR_DGE:
	jmp_opcode = false_br ? TOP_jb : TOP_jae;
	cmp_opcode = Is_Target_SSE2()
	  ? ( is_64bit ? TOP_comisd : TOP_comiss ) : TOP_fucomi;
	break;
      case V_BR_QLT:
      case V_BR_XLT:
        flip_opnds = TRUE;
	jmp_opcode = false_br ? TOP_jbe : TOP_ja;
	cmp_opcode = TOP_fucomi;
	break;
      case V_BR_FLT:
      case V_BR_DLT:
        flip_opnds = TRUE;
	jmp_opcode = false_br ? TOP_jbe : TOP_ja;
	cmp_opcode = Is_Target_SSE2()
	  ? ( is_64bit ? TOP_comisd : TOP_comiss ) : TOP_fucomi;
	break;
      case V_BR_QLE:
      case V_BR_XLE:
        flip_opnds = false_br;
	jmp_opcode = false_br ? TOP_jb : TOP_jbe;
	cmp_opcode = TOP_fucomi;
	break;
      case V_BR_FLE:
      case V_BR_DLE:
        flip_opnds = false_br;
	jmp_opcode = false_br ? TOP_jb : TOP_jbe;
	cmp_opcode = Is_Target_SSE2()
	  ? ( is_64bit ? TOP_comisd : TOP_comiss ) : TOP_fucomi;
	break;
      default:
	FmtAssert(FALSE, ("unimplemented branch variant in Expand_Branch"));
      }

      if( !TOP_is_flop( cmp_opcode ) )
	FmtAssert( !false_br, ("false_br for int cmp") );

      if (flip_opnds) {
	tmp = src1;
	src1 = src2;
	src2 = tmp;
      }

      if( Is_Target_32bit() &&
	  ( cond >= V_BR_I8EQ0 &&
	    cond <= V_BR_U8LE ) ){
	Expand_Split_Branch( cmp_opcode, src1, src2, jmp_opcode, targ, ops );

      } else {
	// Bug#330
	if (Force_IEEE_Comparisons && 
	    TOP_is_flop( cmp_opcode ) ){
	  Expand_Ordered_Branch( cmp_opcode, src1, src2, jmp_opcode, targ, ops);
	} else {
	  TN* rflags = Rflags_TN();

	  Build_OP( cmp_opcode, rflags, src1, src2, ops );
	  Build_OP( jmp_opcode, rflags, targ, ops );
	}
      }
    }

    break;
  }
}

void Exp_Indirect_Branch (TN *targ_reg, OPS *ops)
{
  Build_OP(TOP_ijmp, targ_reg, ops);
}

void Exp_Local_Jump(BB *bb, INT64 offset, OPS *ops)
{
  FmtAssert(FALSE, ("NYI: Exp_Local_Jump"));
}

void Exp_Return (TN *return_address, int sp_adjust, OPS *ops)
{
  if( sp_adjust == 0 )
    Build_OP( TOP_ret, return_address, ops );
  else
    Build_OP( TOP_reti, Gen_Literal_TN(sp_adjust,4), ops );
}

void Exp_Call( OPERATOR opr, TN *return_address, TN *target, OPS *ops )
{
  TOP top = TOP_call;

  if( opr == OPR_CALL ){
    top = TOP_call;

  } else if( opr == OPR_ICALL ){
    top = TOP_icall;

  } else
    FmtAssert( false, ("NYI") );


  Build_OP( top, target, ops );
}
