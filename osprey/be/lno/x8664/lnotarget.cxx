/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


#include "lnotarget.h"
#include "w2op.h"

TOP
LNOTARGET_Whirl_To_Top (WN* wn)
{
  OPCODE opcode = WN_opcode(wn);
  switch (opcode) {
  case OPC_F4ADD:
    return TOP_addss;
  case OPC_F8ADD:
    return TOP_addsd;
  case OPC_F10ADD:
    return TOP_fadd;
  case OPC_F4SUB:
    return TOP_subss;
  case OPC_F8SUB:
    return TOP_subsd;
  case OPC_F10SUB:
    return TOP_fsub;
  case OPC_F4MPY:
    return TOP_mulss;
  case OPC_F8MPY:
    return TOP_mulsd;
  case OPC_F10MPY:
    return TOP_fmul;
  case OPC_F4MADD:	// (src2 * src3) + src1
  case OPC_F4NMADD:	// -((src2 * src3) + src1)
  case OPC_F4MSUB:	// (src2 * src3) - src1
  case OPC_F4NMSUB:	// -((src2 * src3) - src1)
  case OPC_F8MADD:	// (src2 * src3) + src1
  case OPC_F8NMADD:	// -((src2 * src3) + src1)
  case OPC_F8MSUB:	// (src2 * src3) - src1
  case OPC_F8NMSUB:	// -((src2 * src3) - src1)
    FmtAssert( false, ("NYI") );
    break;
  case OPC_F4DIV:
    return TOP_divss;
  case OPC_F8DIV:
    return TOP_divsd;
  case OPC_F10DIV:
    return TOP_fdiv;
  case OPC_F4RSQRT:
  case OPC_F4ATOMIC_RSQRT:
    return TOP_sqrtss;
  case OPC_F8RSQRT:
    return TOP_sqrtsd;
  case OPC_F4NEG:
    return TOP_xorps;
  case OPC_F8NEG:
    return TOP_xorpd;    
  case OPC_I4F10EQ:
  case OPC_I4F8EQ:
  case OPC_I4F4EQ:
  case OPC_I4F10NE:
  case OPC_I4F8NE:
  case OPC_I4F4NE:
  case OPC_I4F10LT:
  case OPC_I4F8LT:
  case OPC_I4F4LT:
  case OPC_I4F10GT:
  case OPC_I4F8GT:
  case OPC_I4F4GT:
  case OPC_I4F10LE:
  case OPC_I4F8LE:
  case OPC_I4F4LE:
  case OPC_I4F10GE:
  case OPC_I4F8GE:
  case OPC_I4F4GE:
  case OPC_U4F10EQ:
  case OPC_U4F8EQ:
  case OPC_U4F4EQ:
  case OPC_U4F10NE:
  case OPC_U4F8NE:
  case OPC_U4F4NE:
  case OPC_U4F10LT:
  case OPC_U4F8LT:
  case OPC_U4F4LT:
  case OPC_U4F10GT:
  case OPC_U4F8GT:
  case OPC_U4F4GT:
  case OPC_U4F10LE:
  case OPC_U4F8LE:
  case OPC_U4F4LE:
  case OPC_U4F10GE:
  case OPC_U4F8GE:
  case OPC_U4F4GE:
    // needs a ucomisd/ucomiss and some form of set
    return TOP_UNDEFINED;    
  case OPC_F4RECIP:
  case OPC_F8RECIP:
  case OPC_F10RECIP:
    // needs two ops - one to load constant 1; and one to divide
    return TOP_UNDEFINED;
  case OPC_F4ABS:
    // approximate because needs an op to load a special constant 
    return TOP_andps;
  case OPC_F8ABS:
    // approximate because needs an op to load a special constant 
    return TOP_andpd;
  case OPC_F10ABS:
    return TOP_fabs;
  case OPC_F4SELECT:
  case OPC_F8SELECT:
    // selects have no direct particular ops; and get expanded to multiple ops.
    return TOP_UNDEFINED;
  case OPC_F4F4FLOOR:
  case OPC_F8F8FLOOR:
    // expand into several ops
    return TOP_UNDEFINED;
  default:
    // bug 10320: handle TAS to avoid assertion failure
    if(OPCODE_operator(opcode) == OPR_TAS)
      break;
    else
      FmtAssert(FALSE, ("Handle this case"));
  }
  return WHIRL_To_TOP(wn);
}

void
LNOTARGET_Loop_Inc_Test_Res (TI_RES_COUNT* resource_count)
{
  // Can not be sure about the exact instructions; 
  // we could be using an lea inplace of add
  // and, we could be using a test instruction in place of cmp
  // Also, there are many flavors of branch.
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp32);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_je);
}

double
LNOTARGET_Cvt_Res (TI_RES_COUNT* resource_count, OPCODE opcode)
{
  TYPE_ID rtype, desc;

  switch(opcode) {
    // Is_CVT_Noop
  case OPC_I4U4CVT:
  case OPC_U4I4CVT:
  case OPC_I8U8CVT:
  case OPC_U8I8CVT:
    return 0.0;

    // TODO: need to look at parent load/store; 
    // but there is a long call chain that needs to be updated
    // to support that in model.cxx
  case OPC_I4U8CVT: rtype = MTYPE_I4; desc = MTYPE_U8; break;
  case OPC_I4I8CVT: rtype = MTYPE_I4; desc = MTYPE_I8; break;
  case OPC_U4U8CVT: rtype = MTYPE_U4; desc = MTYPE_U8; break;
  case OPC_U4I8CVT: rtype = MTYPE_U4; desc = MTYPE_I8; break;

    // Is_CVTL_Opcode
  case OPC_U8I4CVT: rtype = MTYPE_U8; desc = MTYPE_I4; break;
  case OPC_U8U4CVT: rtype = MTYPE_U8; desc = MTYPE_U4; break;
  case OPC_I8I4CVT: rtype = MTYPE_I8; desc = MTYPE_I4; break;
  case OPC_I8U4CVT: rtype = MTYPE_I8; desc = MTYPE_U4; break;

  case OPC_I8CVTL:
  case OPC_I4CVTL:
  case OPC_U8CVTL:
  case OPC_U4CVTL:
    FmtAssert(FALSE, ("NYI2.2"));
    break;
    
  case OPC_F8F4CVT:       
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtss2sd);
    return 1.0;
  case OPC_F4F8CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsd2ss);
    return 1.0;

  case OPC_F4I8CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsi2ssq);
    return 1.0;    
  case OPC_F4I4CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsi2ss);
    return 1.0;    
  case OPC_F4U8CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shri64);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_andi32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or64);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsi2ssq);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_addss);
    return 5.0;    
  case OPC_F4U4CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsi2ssq);
    return 2.0;    
  case OPC_F8I8CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsi2sdq);
    return 1.0;    
  case OPC_F8I4CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsi2sd);
    return 1.0;    
  case OPC_F8U8CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shri64);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_andi32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or64);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsi2sdq);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_addsd);
    return 5.0;    
  case OPC_F8U4CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvtsi2sdq);
    return 2.0;    

  case OPC_I8F4TRUNC: 
  case OPC_I8F4RND: 
  case OPC_I8F4CEIL: 
  case OPC_I8F4FLOOR: 
  case OPC_I8F4CVT: 
  case OPC_U8F4CVT: 
  case OPC_U8F4TRUNC:
  case OPC_U8F4RND:
  case OPC_U8F4CEIL:
  case OPC_U8F4FLOOR:
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvttss2siq);
    return 1.0;    
  case OPC_I4F4TRUNC: 
  case OPC_I4F4RND: 
  case OPC_I4F4CEIL: 
  case OPC_I4F4FLOOR: 
  case OPC_I4F4CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvttss2si);
    return 1.0;    
  case OPC_U4F4TRUNC: 
  case OPC_U4F4RND: 
  case OPC_U4F4CEIL: 
  case OPC_U4F4FLOOR: 
  case OPC_U4F4CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvttss2siq);
    return 1.0;        
  case OPC_I8F8TRUNC: 
  case OPC_U8F8TRUNC:
  case OPC_I8F8RND: 
  case OPC_U8F8RND:
  case OPC_I8F8CEIL:
  case OPC_U8F8CEIL:
  case OPC_I8F8FLOOR:
  case OPC_U8F8FLOOR:
  case OPC_I8F8CVT: 
  case OPC_U8F8CVT:
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvttsd2siq);
    return 1.0;        
  case OPC_I4F8TRUNC:     
  case OPC_I4F8RND:     
  case OPC_I4F8CEIL:     
  case OPC_I4F8FLOOR:     
  case OPC_I4F8CVT:     
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvttsd2si);
    return 1.0;        
  case OPC_U4F8TRUNC: 
  case OPC_U4F8RND: 
  case OPC_U4F8CEIL: 
  case OPC_U4F8FLOOR: 
  case OPC_U4F8CVT: 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvttsd2siq);
    return 1.0;        
 
  case OPC_F10F8CVT:
  case OPC_F10F4CVT:
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  opcode == OPC_F8F10CVT ? TOP_stsd : TOP_stss);
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  opcode == OPC_F8F10CVT ? TOP_fldl : TOP_flds);
    return 2.0;        
  case OPC_F8F10CVT:
  case OPC_F4F10CVT:
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  opcode == OPC_F8F10CVT ? TOP_fstpl:TOP_fstps);
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  opcode == OPC_F8F10CVT ? TOP_ldsd : TOP_ldss);
    return 2.0;        

  case OPC_F10I4CVT:
  case OPC_F10I8CVT:
  case OPC_F10U4CVT:
  case OPC_F10U8CVT:
    // CG forms few basic blocks. Use a simple model here.
    return 4.0;

  case OPC_I4F10CVT:
  case OPC_I8F10CVT:
  case OPC_U4F10CVT:
  case OPC_U8F10CVT:
    {
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnstcw);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ld32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or32);    
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_store16);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fldcw);
      TOP top;
      switch (opcode) {
      case OPC_I4F10CVT:    top = TOP_fistpl;   break;
      case OPC_U4F10CVT:
      case OPC_U8F10CVT:
      case OPC_I8F10CVT:    top = TOP_fistpll;  break;
      default:
	// Follow CG
	FmtAssert( false, ("NYI") );
      }
      TI_RES_COUNT_Add_Op_Resources(resource_count, top);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fldcw);
      if (opcode != OPC_U8F10CVT)
	return 7.0;
      else
	// and then few new basic blocks. Use a simple model here.
	return 11.0;
    }

  default:
    FmtAssert(FALSE, ("NYI2.1"));
    break;
  }

  switch (opcode) {
    // Is_CVTL_Opcode
  case OPC_U8I4CVT: 
  case OPC_U8U4CVT: 
  case OPC_I8I4CVT: 
  case OPC_I8U4CVT: 
  case OPC_I4I8CVT: 
  case OPC_I4U8CVT: 
  case OPC_U4U8CVT: 
  case OPC_U4I8CVT: 
    {
      const UINT64 val = 32;
      const BOOL is_64bit = MTYPE_is_size_double(rtype);
      const BOOL signed_extension = MTYPE_is_signed(desc) || 
	(MTYPE_bit_size(desc) > MTYPE_bit_size(rtype));
      TOP new_opcode;

      if ( is_64bit )
	new_opcode = signed_extension ? TOP_movslq: TOP_mov64;
      else if ( MTYPE_bit_size(rtype) == 32)
	new_opcode = TOP_mov32;
      TI_RES_COUNT_Add_Op_Resources(resource_count, new_opcode);
      return 1.0;
    }
    break;
  }
  return 0.0;
}

double
LNOTARGET_FP_Madd_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mulsd: TOP_mulss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_addsd: TOP_addss);
  return 2.0;
}

double
LNOTARGET_FP_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_minsd: TOP_minss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_maxsd: TOP_maxss);
  return 2.0;
}

double
LNOTARGET_FP_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_divsd: TOP_divss);
  return 1.0;
}

double
LNOTARGET_FP_Recip_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_ldc64: TOP_ldc32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_divsd: TOP_divss);
  return 2.0;
}

double
LNOTARGET_FP_Rsqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_sqrtsd: TOP_sqrtss);
  return 1.0;
}

double
LNOTARGET_FP_Sqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_sqrtsd: TOP_sqrtss);
  return 1.0;
}
 
double
LNOTARGET_FP_Exp_Res (TI_RES_COUNT* resource_count, 
                      INTRINSIC intr,
                      INT num_multiplies)
{
  return -1.0;
}

double
LNOTARGET_FP_Floor_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  if (mtype == MTYPE_F4) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_andps);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_orps);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_addss);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subss);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subss);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmpss);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_andps);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subss);
    return 8.0;
  } else if (mtype == MTYPE_F8) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_andpd);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_andpd);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_orpd);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_addsd);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subsd);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subsd);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subsd);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmpsd);
    return 8.0;
  }
  FmtAssert(FALSE, ("LNOTARGET_FP_Floor_Res: mtype not yet implemented"));
  return 0.0;	// dummy
}

double
LNOTARGET_Complex_Add_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_addsd: TOP_addss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_addsd: TOP_addss);
  return 2.0;  
}

double
LNOTARGET_Complex_Mult_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mulsd: TOP_mulss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mulsd: TOP_mulss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mulsd: TOP_mulss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mulsd: TOP_mulss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_addsd: TOP_addss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_addsd: TOP_addss);
  return 6.0;  
}

double
LNOTARGET_Complex_Neg_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_ldsd: TOP_ldss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_ldsd: TOP_ldss);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_xorpd: TOP_xorps);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_xorpd: TOP_xorps);
  return 4.0;
}

double
LNOTARGET_Fp_Select_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{  
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_andpd: TOP_andps);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_andnpd: TOP_andnps);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_orpd: TOP_orps);
  
  return 3.0;
}

double
LNOTARGET_Fp_Compare_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{  
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmpsd: TOP_cmpss);
  
  return 1.0;
}

double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{  
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  // copes if dest_tn  == true_tn and dest == false_tn
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mov64: TOP_mov32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mov64: TOP_mov32);
  
  // assign true_tn to dest_tn
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mov64: TOP_mov32);
  
  // test cond_tn
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_test64: TOP_test32);

  // move conditionally
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmove);

  return 5.0;
}

double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count, 
			TYPE_ID rtype, INT length)
{
  const BOOL signed_extension = MTYPE_is_signed(rtype);
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TOP new_opcode;

  if( length < 8 ){
    FmtAssert( !signed_extension, ("NYI") );
    new_opcode = TOP_andi32;
  } else if( length == 8 ){
    if( signed_extension ){
      new_opcode = is_64bit ? TOP_movsbq : TOP_movsbl;
    } else {
      new_opcode = is_64bit ? TOP_movzbq : TOP_movzbl;
    }
  } else if( length == 16 ){
    if( signed_extension ){
      new_opcode = is_64bit ? TOP_movswq : TOP_movswl;
    } else {
      new_opcode = is_64bit ? TOP_movzwq : TOP_movzwl;
    }
  } else if( length == 32 ){
    if( is_64bit )
      new_opcode = signed_extension ? TOP_movslq : TOP_mov64;
    else if( MTYPE_bit_size(rtype) == 32 )
      new_opcode = TOP_mov32;
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, new_opcode);
  return 1.0;
}

double
LNOTARGET_Int_Neg_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes ? TOP_neg64: TOP_neg32);
  return 1.0;
}

double
LNOTARGET_Int_Abs_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  // copy for src == dest
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes ? TOP_mov64: TOP_mov32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes ? TOP_neg64: TOP_neg32);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmovs);
  return 3.0;
}

double
LNOTARGET_Int_Bnot_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit? TOP_not64: TOP_not32);
  return 1.0;  
}

double
LNOTARGET_Int_Lnot_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit? TOP_xori64: TOP_xori32);
  return 1.0;  
}

double
LNOTARGET_Int_Mult_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_imul64:TOP_imul32);
  return 1.0;
}

/* return whether MPY will be translated into shifts and adds */
/* NOTE:  this routine must stay in sync with cg's Expand_Multiply */
static BOOL
CGEXP_Can_Do_Fast_Multiply (TYPE_ID mtype, INT64 val)
{
#define MULTIPLICATION_LATENCY_32_BIT 2
#define MULTIPLICATION_LATENCY_64_BIT 3 

  INT num_ops = 4; // Initialize more than limit
  BOOL neg = FALSE;

  if (OPT_Space) return FALSE;  // These may eat up a bunch of instructions
  
  // The following heuristic is based on Software Optimization guide for
  // Opteron. Section 8.2
  if (val == 0 || val == 1 || val == -1)
    return TRUE;
  else if (val < 0) {
    val = -val;
    neg = TRUE;
  } 
  switch (val) {
  case 3: num_ops = 2; break;
  case 5: num_ops = 2; break;
  case 6: num_ops = 3; break;
  case 7: num_ops = 2; break;
  case 9: num_ops = 2; break;
  case 10: num_ops = 3; break;
  case 11: num_ops = 3; break;
  case 12: num_ops = 3; break;
  case 13: num_ops = 3; break;
  case 14: num_ops = 3; break;
  case 15: num_ops = 2; break;
  case 17: num_ops = 2; break;
  case 18: num_ops = 3; break;
  case 19: num_ops = 3; break;
  case 20: num_ops = 3; break;
  case 21: num_ops = 3; break;
  case 23: num_ops = 3; break;
  case 24: num_ops = 3; break;
  case 25: num_ops = 3; break;
  case 27: num_ops = 3; break;
  case 28: num_ops = 3; break;
  case 29: num_ops = 3; break;
  case 30: num_ops = 3; break;
  case 31: num_ops = 2; break;
  }
  if (neg)
    num_ops ++;
  if ((mtype == MTYPE_I4 || mtype == MTYPE_U4) &&
      num_ops <= MULTIPLICATION_LATENCY_32_BIT)
    return TRUE;
  else if ((mtype == MTYPE_I8 || mtype == MTYPE_U8) &&
	   num_ops <= MULTIPLICATION_LATENCY_64_BIT)
    return TRUE; 
  else
    return FALSE;
}

static double 
CGEXP_Expand_Constant_Multiply (TI_RES_COUNT* resource_count, 
			  TYPE_ID mtype, INT64 val)
{
  BOOL eight_bytes = MTYPE_is_size_double(mtype);
  BOOL needs_sign_extension;
  double curr_instr = 0.0;

  // fast special cases
  if (val == 0) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  eight_bytes?TOP_ldc64:TOP_ldc32);
    return 1.0;
  } else if (val == 1) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  eight_bytes?TOP_mov64:TOP_mov32);
    return 1.0;
  } else if (val == -1) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  eight_bytes?TOP_neg64:TOP_neg32);
    return 1.0;
  }
    
  needs_sign_extension = MTYPE_size_reg(mtype) != 64;

  if (val < 0) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  eight_bytes?TOP_neg64:TOP_neg32);
    curr_instr = 1.0;
  }    

  TOP lea, add, sub, shift;
  if (eight_bytes) {
    lea = TOP_leax32;
    add = TOP_add32;
    sub = TOP_sub32;
    shift = TOP_shli32;
  } else {
    lea = TOP_leax64;
    add = TOP_add64;
    sub = TOP_sub64;
    shift = TOP_shli32;
  }
  switch (val) {
  case 3:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    curr_instr += 1.0;
    break;
  case 5:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    curr_instr += 1.0;
    break;
  case 6:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    curr_instr += 2.0;
    break;
  case 7:
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 2.0;
    break;
  case 9:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    curr_instr += 1.0;
    break;
  case 10:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    curr_instr += 2.0;
    break;
  case 11:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    curr_instr += 3.0;
    break;
  case 12:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    curr_instr += 2.0;
    break;
  case 13:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 3.0;
    break;
  case 14:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 3.0;
    break;
  case 15:
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 2.0;
    break;
  case 17:
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    curr_instr += 2.0;
    break;
  case 18:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    curr_instr += 2.0;
    break;
  case 19:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    curr_instr += 3.0;
    break;
  case 20:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    curr_instr += 2.0;
    break;
  case 21:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    curr_instr += 3.0;
    break;
  case 23:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 3.0;
    break;
  case 24:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    curr_instr += 2.0;
    break;
  case 25:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, add);
    curr_instr += 3.0;
    break;
  case 27:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 3.0;
    break;
  case 29:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 3.0;
    break;
  case 30:
    TI_RES_COUNT_Add_Op_Resources(resource_count, lea);
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 3.0;
    break;
  case 31:
    TI_RES_COUNT_Add_Op_Resources(resource_count, shift);
    TI_RES_COUNT_Add_Op_Resources(resource_count, sub);
    curr_instr += 2.0;
    break;
  default:
    curr_instr = -1.0;
    break;
  }
  return curr_instr;
}

double
LNOTARGET_Int_Mult_Str_Red_Res (TI_RES_COUNT* resource_count, 
				TYPE_ID rtype, INT64 operand)
{
  INT64 val = operand;
  const BOOL eight_bytes = MTYPE_is_size_double(rtype);
  double ret_val;

  if (val == 1) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  eight_bytes?TOP_add64:TOP_add32);
    return 1.0;
  }
  if( val > 0 &&
      ( val & ( val - 1 ) ) == 0 ){
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  eight_bytes?TOP_shli64:TOP_shli32);
    return 1.0;
  }
  else if (CGEXP_Can_Do_Fast_Multiply (rtype, val)) {
    ret_val = CGEXP_Expand_Constant_Multiply (resource_count, rtype, val);
    if (ret_val != -1.0)
      /* able to convert multiply into shifts/adds/subs */
      return ret_val;
  }      

  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_ldc64:TOP_ldc32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_imul64:TOP_imul32);
  return 2.0;
}

double
LNOTARGET_Int_Add_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_add64:TOP_add32);
  return 1.0;
}

double
LNOTARGET_Int_Sub_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{ 
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_sub64:TOP_sub32);
  return 1.0;
}

double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  TOP top;
  const BOOL is_signed = MTYPE_is_signed(rtype);
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  if (is_signed)
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_sari64 : TOP_sari32); 
  else 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ldc32);
  switch (rtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, top); 
  return (2.0);
}

/* return TRUE if the val is a power of 2 */
#define IS_POWER_OF_2(val)	((val != 0) && ((val & (val-1)) == 0))

static BOOL Is_Power_Of_2(INT64 val, TYPE_ID mtype)
{
  if (MTYPE_is_signed(mtype) && val < 0) val = -val;

  if (mtype == MTYPE_U4) val &= 0xffffffffull;

  return IS_POWER_OF_2(val);
}

/******************************************************************************
 *
 *   Function Name: determine_pseudo_inverse
 *
 *   Author: Bill Homer
 *
 *   Input Parameters: b              constant divisor
 *                     maxbits_in_a   size of dividend
 *
 *   Returns:          pseudo inverse
 *                     pn             smallest n, such that 2^n >= b
 *
 *   Description:
 *   Given an unsigned integer, calculate a "pseudo-inverse"
 *   (which is the return value) and the associated shift width
 *   (which is returned via the third parameter).
 *
 *   Usage:
 *    Let BPUL be the number of bits in an unsigned long.
 *    When b is a compile time constant, optimize an unsigned
 *    long integer division on T90,IEEE
 *                                    q = a/b
 *    by replacing it with:
 *
 *    Case 1) b == 2^n                q = b>>n
 *            Not done here.
 *
 *    Case 2) b >= 2^(BPUL-1)         q = (a >= b)
 *
 *    Case 3) a, b < 2^(BPUL/2)
 *            Not used; might be a good way to handle 32 bit ints.
 *            At compile time:        d = (~0UL)/b
 *            At run time:            q = int_mult_upper(d,a)
 *
 *    Case 4) Not used - general case; longer code than 5) & 6)
 *
 *    Case 5) a < 2^(BPUL-1)
 *            Used for 32 and 64 bit signed ints.
 *            At compile time:        d = determine_pseudo_inverse(b,BPUL-1,&n)
 *            At run time:            q = int_mult_upper(d,a) >> (n-1)
 *
 *    Case 6) default
 *            Used for unsigned 32 and 64 bit ints.
 *            At compile time:        d = determine_pseudo_inverse(b,BPUL,&n)
 *            At run time:            p = int_mult_upper(d,a)
 *                                    q = (p + ((a-p)>>1)) >> (n-1)
 *
 *****************************************************************************/

static UINT64 determine_pseudo_inverse (
  UINT64      b,
  INT64       maxbits_a,
  INT64      *pn)
{
  INT64  i, n;
  UINT64 m, q, b1;

  /*  Calculate the smallest n such that 2^n >= b,
   *  and the corresponding m = 2^n - 1
   *  (which satisfies m >= b - 1).
   */
  b1 = b - 1;
  n = 1;
  m = 1;
  while(m < b1) {
    n++;
    m = (m<<1) | 1;
  }
  *pn = n;

  /*  Calculate the "pseudo-inverse" of b, which is
   *  the ceiling of 2^(n+maxbits_a) divided by b, or
   *     d = 1 + (2^(n+maxbits_a) - 1) / b
   *  Because 2^n >=  b, d >= 2^maxbits_a, and
   *  because 2^n < 2*b, d <  2^(maxbits_a+1).
   *  Therefore d occupies (maxbits_a+1) bits,
   *  and its top bit is 1.
   *  Return value is:
   *     d         if maxbits_a  < BPUL (bits per unsigned long)
   *     d-2^BPUL  if maxbits_a == BPUL (i.e., all but top bit)
   */
  BOOL m_overflow = FALSE;
  for(q=i=0; i<=maxbits_a; i++) {
    q <<= 1;
    if(m_overflow) {
      // because ((m>>1) | 0x8000000000000000ULL) >= m
      // if m>=b in this iteration, then in last iteration m also >= b. 
      // This can't happen
      Is_True(m < b, ("m bigger than b and m is overflow in last iteration\n"));
      m -= b;
      q |= 1;
      m_overflow = FALSE;
    }
    else if(m >= b) {
      m -= b;
      q |= 1;
    }
    // After subtraction, m must be smaller than b. And m's 64 bit MSB must be zero.
    // if m's 64bit MSB is 1, then subtraction not happen in this iteration.
    // it means b>m, then b's 64 bit MSB is also 1.
    // Mark m overflow and in next iteration, actually m is bigger than b.
    // Need do subtraction in next itration.
    if (m & 0x8000000000000000ULL) {
      Is_True(b & 0x8000000000000000ULL, ("b's 64th bit must be 1\n"));
      m_overflow = TRUE;
    }
    m = (m<<1) | 1;
  }
  return 1+q;
}

static double 
CGEXP_Expand_Integer_Divide_By_Constant (TI_RES_COUNT* resource_count, 
				   TYPE_ID rtype, INT64 denom_val)
{
  UINT64 b;				// b = |denom_val|
  UINT64 d;      			// division scaling factor
  INT64  precision_required;
  INT64  n;
  BOOL is_odd;
  TOP opc;
  const BOOL is_double = MTYPE_is_size_double(rtype);
  const BOOL is_signed = MTYPE_is_signed(rtype);
  double curr_instr = 0.0;

  if (denom_val == 1) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double? TOP_mov64: TOP_mov32);     
    return 1.0;
  } else if (is_signed && denom_val == -1) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double? TOP_neg64: TOP_neg32);     
    return 1.0;
  }

  if( Is_Power_Of_2( denom_val, rtype)) {
    if (is_signed) {
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double? TOP_shri64: TOP_shri32);     
      return 1.0;
    } else {
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double? TOP_sari64: TOP_sari32);     
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double? TOP_ldc64: TOP_ldc32);          
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double? TOP_and64: TOP_and32);          
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double? TOP_add64: TOP_add32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double? TOP_sari64: TOP_sari32);
      curr_instr = 4.0;
      if (denom_val < 0) {
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double? TOP_neg64: TOP_neg32);
	curr_instr += 1.0;
      }
      return curr_instr;
    }
  }

  if (is_signed) {

    b = denom_val<0 ? -denom_val : denom_val;       // b = |denom_val|
    is_odd = (b&1);

    d = determine_pseudo_inverse (b, is_double?63:31, &n);

    if (n > (is_double ? 63 : 31)) {
      /* OOPS! The shift count can't be bigger than the word size! */
      return -1.0;
    }

    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double? TOP_ldc64: TOP_ldc32);           

    /* Generate the absolute value of the numerator:
     */
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double? TOP_neg64: TOP_neg32);           
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmovs);
 
    curr_instr += 3.0;
    
    /* Generate a multiply upper:
     */
    if (!is_double) {
      if (is_signed) {
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ldc64);	
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_imul64);	
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shri64);	
	curr_instr += 3.0;
      } else {
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ldc64);	
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul32);	
	curr_instr += 2.0;
      }
    } else {
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_imul64);	
      curr_instr += 1.0;
    }
    
    /* Generate and attach the shift:
     */
    if (n > 0) {
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double? TOP_sari64: TOP_sari32);
      curr_instr += 1.0;
    }
    
    /* Select positive or negated result:
     */
    // for copy TNs. worst case
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double? TOP_mov64: TOP_mov32);           
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double? TOP_mov64: TOP_mov32);           

    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double? TOP_test64: TOP_test32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double? TOP_neg64: TOP_neg32);           
    if (denom_val > 0)
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmovge);
    else
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmovl);

    curr_instr += 5.0;
  } /* end Signed */

  else { /* Unsigned */

    b = denom_val;
    is_odd = (b&1);

    /* Full precision calculation is required.
     */
    if (is_odd) {
      precision_required = is_double?64:32;
    } else {
      
      /* Pre-shift the numerator and denominator so that
	 one less bit is required in the calculation. Then
	 we can avoid the subtract-shift-add after the 
	 multiply operation. */
      b >>= 1;
      precision_required = is_double?63:31;
	
      /* Pre-shift to simplify later calculations.
       */
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double ? TOP_sari64: TOP_sari32);
      curr_instr += 1.0;
    }

    d = determine_pseudo_inverse (b, precision_required, &n);

    if (n > precision_required) {
      /* OOPS! The shift count can't be bigger than the word size! */
      return -1.0;
    }

    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double ? TOP_ldc64: TOP_ldc32);

    curr_instr += 1.0;

    /* Generate a multiply upper:
     */
    if (!is_double) {
      if (is_signed) {
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ldc64);	
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_imul64);	
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shri64);	
	curr_instr += 3.0;
      } else {
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ldc64);	
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul32);	
	curr_instr += 2.0;
      }
    } else {
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_imul64);	
      curr_instr += 1.0;
    }

    if (precision_required == 64 || precision_required == 32) {

      /* Odd divisors need full precision and, hence, extra instructions.
       */
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double ? TOP_sub64: TOP_sub32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double ? TOP_shri64: TOP_shri32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double ? TOP_add64: TOP_add32);
      curr_instr += 3.0;
    }

    /* Generate and attach the shift:
     */
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double ? TOP_shri64: TOP_shri32);
    curr_instr += 1.0;
  } /* end Unsigned */

  return curr_instr;
}

double
LNOTARGET_Int_Div_Str_Red_Res (TI_RES_COUNT* resource_count, 
			       TYPE_ID rtype, INT64 denom_val)
{
  double curr_instr;
  TOP top;
  const BOOL is_double = MTYPE_is_signed(rtype);

  curr_instr = CGEXP_Expand_Integer_Divide_By_Constant (resource_count, 
							rtype, denom_val);
  if (curr_instr != -1.0)
    return curr_instr;
  
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_ldc64: TOP_ldc32);
  switch (rtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, top); 
  return 2.0;
}

double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  double val = LNOTARGET_Int_Rem_Res (resource_count, rtype);
  const BOOL is_double = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_xor64: TOP_xor32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_test64: TOP_test32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_ldc64: TOP_ldc32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmovl);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_add64: TOP_add32);
  return (val + 5.0);
}

double
LNOTARGET_Int_Mod_Str_Red_Res (TI_RES_COUNT* resource_count, 
			       TYPE_ID rtype, INT64 denom_val)
{
  const BOOL is_signed = MTYPE_is_signed(rtype);
  const BOOL is_double = MTYPE_is_size_double(rtype);

  if (Is_Power_Of_2(denom_val, rtype)) {
    if (is_signed && denom_val < 0) {
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double ? TOP_neg64: TOP_neg32);    
      if (denom_val == -1) 
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_mov64: TOP_mov32);    
      else if ( denom_val & 0xffffffff00000000ULL == 0 ) 
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_andi64: TOP_andi32);
      else {
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_mov64: TOP_mov32);
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_and64: TOP_and32);
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_neg64: TOP_neg32);
	return 4.0;
      }
      TI_RES_COUNT_Add_Op_Resources(resource_count, 
				    is_double ? TOP_neg64: TOP_neg32);
      return 3.0;
    } else {
      if ( denom_val & 0xffffffff00000000ULL == 0 ) 
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_andi64: TOP_andi32);
      else {
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_mov64: TOP_mov32);
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_and64: TOP_and32);
	TI_RES_COUNT_Add_Op_Resources(resource_count, 
				      is_double ? TOP_neg64: TOP_neg32);
	return 3.0;
      }
      return 1.0;
    }
  }

  double val = LNOTARGET_Int_Rem_Res (resource_count, rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_xor64: TOP_xor32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_test64: TOP_test32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_ldc64: TOP_ldc32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmovl);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_add64: TOP_add32);
  return (val + 5.0);
}

double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  TOP top;
  const BOOL is_signed = MTYPE_is_signed(rtype);
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  if (is_signed)
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_sari64 : TOP_sari32); 
  else 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ldc32);
  switch (rtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, top); 
  return (2.0);
}

double
LNOTARGET_Int_Rem_Str_Red_Res (TI_RES_COUNT* resource_count, 
			       TYPE_ID rtype, INT64 denom_val)
{
  double curr_instr;
  TOP top;
  BOOL is_double = MTYPE_is_size_double(rtype);

  curr_instr = CGEXP_Expand_Integer_Divide_By_Constant (resource_count, 
							rtype, denom_val);
  if (curr_instr != -1.0) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double ? TOP_imul64 : TOP_imul32);     
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double ? TOP_sub64 : TOP_sub32);    
    return curr_instr + 2.0;
  }
  
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_ldc64: TOP_ldc32);
  switch (rtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, top); 
  return 2.0;
}

double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  TOP top;
  const BOOL is_signed = MTYPE_is_signed(rtype);
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  if (is_signed)
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_sari64 : TOP_sari32); 
  else 
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ldc32);
  switch (rtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, top); 
  return (2.0);
}

double
LNOTARGET_Int_DivRem_Str_Red_Res (TI_RES_COUNT* resource_count, 
			       TYPE_ID rtype, INT64 denom_val)
{
  double curr_instr;
  TOP top;
  BOOL is_double = MTYPE_is_size_double(rtype);

  curr_instr = CGEXP_Expand_Integer_Divide_By_Constant (resource_count, 
							rtype, denom_val);
  if (curr_instr != -1.0) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double ? TOP_imul64 : TOP_imul32);     
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_double ? TOP_sub64 : TOP_sub32);    
    return curr_instr + 2.0;
  }
  
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_double ? TOP_ldc64: TOP_ldc32);
  switch (rtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, top); 
  return 2.0;
}

double
LNOTARGET_Int_Min_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  const BOOL is_signed = MTYPE_is_signed(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mov64 : TOP_mov32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_signed ? TOP_cmovle: TOP_cmovbe);
  return 3.0;
}
  
double
LNOTARGET_Int_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  const BOOL is_signed = MTYPE_is_signed(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mov64 : TOP_mov32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_signed ? TOP_cmovge: TOP_cmovae);
  return 3.0;
}
  
double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  const BOOL is_signed = MTYPE_is_signed(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mov64 : TOP_mov32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_mov64 : TOP_mov32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_signed ? TOP_cmovl: TOP_cmovb);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_signed ? TOP_cmovl: TOP_cmovb);
  return 5.0;
}
  
double
LNOTARGET_Int_Band_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_and64: TOP_and32);
  return 1.0;
}

double
LNOTARGET_Int_Bior_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_or64: TOP_or32);
  return 1.0;
}

double
LNOTARGET_Int_Bnor_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  FmtAssert(FALSE, ("NYI30"));
  return 1.0;
}

double
LNOTARGET_Int_Bxor_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_xor64: TOP_xor32);
  return 1.0;
}

double
LNOTARGET_Int_Band_Str_Red_Res (TI_RES_COUNT* resource_count, 
				TYPE_ID rtype, INT64 val)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  if (val == -1) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_mov64: TOP_mov32);    
    return 1.0;
  }
  else if ( val & 0xffffffff00000000ULL == 0 ) { // LC_simm32
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_andi64: TOP_andi32);    
    return 1.0;
  } else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_ldc64: TOP_ldc32);    
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_and64: TOP_and32);    
    return 2.0;
  }  
}

double
LNOTARGET_Int_Bior_Str_Red_Res (TI_RES_COUNT* resource_count, 
				TYPE_ID rtype, INT64 val)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  if (val == 0) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_mov64: TOP_mov32);    
    return 1.0;
  }
  else if ( val & 0xffffffff00000000ULL == 0 ) { // LC_simm32
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_ori64: TOP_ori32);    
    return 1.0;
  } else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_ldc64: TOP_ldc32);    
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_or64: TOP_or32);    
    return 2.0;
  }  
}

double
LNOTARGET_Int_Bxor_Str_Red_Res (TI_RES_COUNT* resource_count, 
				TYPE_ID rtype, INT64 val)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  if (val == 0) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_mov64: TOP_mov32);    
    return 1.0;
  }
  else if ( val & 0xffffffff00000000ULL == 0 ) { // LC_simm32
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_xori64: TOP_xori32);    
    return 1.0;
  } else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_ldc64: TOP_ldc32);    
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  is_64bit ? TOP_xor64: TOP_xor32);    
    return 2.0;
  }  
}

double
LNOTARGET_Int_Land_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_and64: TOP_and32);    
  return 1.0;
}

double
LNOTARGET_Int_Cand_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_and64: TOP_and32);    
  return 1.0;
}

double
LNOTARGET_Int_Lior_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_or64: TOP_or32);    
  return 1.0;
}

double
LNOTARGET_Int_Cior_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype)
{
  const BOOL is_64bit = MTYPE_is_size_double(rtype);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_or64: TOP_or32);    
  return 1.0;
}

double
LNOTARGET_Int_Shl_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes ? TOP_shli64: TOP_shli32);
  return 1.0;
}

double
LNOTARGET_Int_Ashr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes ? TOP_sari64: TOP_sari32);
  return 1.0;
}

double
LNOTARGET_Int_Lshr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes ? TOP_shri64: TOP_shri32);
  return 1.0;
}

double
LNOTARGET_Int_Eq_Res (TI_RES_COUNT* resource_count, TYPE_ID desc)
{
  const BOOL is_64bit = MTYPE_is_size_double(desc);
  // We could end up using test32/test64/cmpi32/cmpi64 if it is a compare 
  // with imm.
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sete);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_movzbq: TOP_movzbl);
  return 3.0;
}

double
LNOTARGET_Int_Ne_Res (TI_RES_COUNT* resource_count, TYPE_ID desc)
{
  const BOOL is_64bit = MTYPE_is_size_double(desc);
  // We could end up using test32/test64/cmpi32/cmpi64 if it is a compare 
  // with imm.
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setne);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_movzbq: TOP_movzbl);
  return 3.0;
}

double
LNOTARGET_Int_Gt_Res (TI_RES_COUNT* resource_count, TYPE_ID desc)
{  
  const BOOL is_64bit = MTYPE_is_size_double(desc);
  const BOOL is_signed = MTYPE_is_signed(desc);
  // We could end up using test32/test64/cmpi32/cmpi64 if it is a compare 
  // with imm.
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_signed ? TOP_setg : TOP_seta);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_movzbq: TOP_movzbl);
  return 3.0;
}

double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count, TYPE_ID desc)
{
  const BOOL is_64bit = MTYPE_is_size_double(desc);
  const BOOL is_signed = MTYPE_is_signed(desc);
  // We could end up using test32/test64/cmpi32/cmpi64 if it is a compare 
  // with imm.
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_signed ? TOP_setge : TOP_setae);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_movzbq: TOP_movzbl);
  return 3.0;
}

double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count, TYPE_ID desc)
{
  const BOOL is_64bit = MTYPE_is_size_double(desc);
  const BOOL is_signed = MTYPE_is_signed(desc);
  // We could end up using test32/test64/cmpi32/cmpi64 if it is a compare 
  // with imm.
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_signed ? TOP_setl : TOP_setb);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_movzbq: TOP_movzbl);
  return 3.0;
}

double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count, TYPE_ID desc)
{
  const BOOL is_64bit = MTYPE_is_size_double(desc);
  const BOOL is_signed = MTYPE_is_signed(desc);
  // We could end up using test32/test64/cmpi32/cmpi64 if it is a compare 
  // with imm.
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_cmp64: TOP_cmp32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_signed ? TOP_setle : TOP_setbe);
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				is_64bit ? TOP_movzbq: TOP_movzbl);
  return 3.0;
}

double
LNOTARGET_Int_Lda_Res (TI_RES_COUNT* resource_count)
{
  FmtAssert(FALSE, ("NYI44"));
  return 2.0;
}

INT
LNOTARGET_Cvt_Lat (OPCODE opcode)
{
  TYPE_ID rtype, desc;

  switch(opcode) {
    // Is_CVT_Noop
  case OPC_I4U4CVT:
  case OPC_U4I4CVT:
  case OPC_I8U8CVT:
  case OPC_U8I8CVT:
    return 0;

    // TODO: need to look at parent load/store; 
    // but there is a long call chain that needs to be updated
    // to support that in model.cxx
  case OPC_I4U8CVT: rtype = MTYPE_I4; desc = MTYPE_U8; break;
  case OPC_I4I8CVT: rtype = MTYPE_I4; desc = MTYPE_I8; break;
  case OPC_U4U8CVT: rtype = MTYPE_U4; desc = MTYPE_U8; break;
  case OPC_U4I8CVT: rtype = MTYPE_U4; desc = MTYPE_I8; break;

    // Is_CVTL_Opcode
  case OPC_U8I4CVT: rtype = MTYPE_U8; desc = MTYPE_I4; break;
  case OPC_U8U4CVT: rtype = MTYPE_U8; desc = MTYPE_U4; break;
  case OPC_I8I4CVT: rtype = MTYPE_I8; desc = MTYPE_I4; break;
  case OPC_I8U4CVT: rtype = MTYPE_I8; desc = MTYPE_U4; break;
  case OPC_I8CVTL:
  case OPC_I4CVTL:
  case OPC_U8CVTL:
  case OPC_U4CVTL:
    FmtAssert(FALSE, ("NYI2.2"));
    break;
    
  case OPC_F8F4CVT:       
    return LNOTARGET_Top_Latency(TOP_cvtss2sd);
  case OPC_F4F8CVT: 
    return LNOTARGET_Top_Latency(TOP_cvtsd2ss);

  case OPC_F4I8CVT: 
    return LNOTARGET_Top_Latency(TOP_cvtsi2ssq);
  case OPC_F4I4CVT: 
    return LNOTARGET_Top_Latency(TOP_cvtsi2ss);
  case OPC_F4U8CVT: 
    return LNOTARGET_Top_Latency(TOP_shri64) +
      LNOTARGET_Top_Latency(TOP_andi32) +
      LNOTARGET_Top_Latency(TOP_or64) +
      LNOTARGET_Top_Latency(TOP_cvtsi2ssq) +
      LNOTARGET_Top_Latency(TOP_addss);
  case OPC_F4U4CVT: 
    return LNOTARGET_Top_Latency(TOP_mov32) +
      LNOTARGET_Top_Latency(TOP_cvtsi2ssq);
  case OPC_F8I8CVT: 
    return LNOTARGET_Top_Latency(TOP_cvtsi2sdq);
  case OPC_F8I4CVT: 
    return LNOTARGET_Top_Latency(TOP_cvtsi2sd);
  case OPC_F8U8CVT: 
    return LNOTARGET_Top_Latency(TOP_shri64) +
      LNOTARGET_Top_Latency(TOP_andi32) +
      LNOTARGET_Top_Latency(TOP_or64) +
      LNOTARGET_Top_Latency(TOP_cvtsi2sdq) +
      LNOTARGET_Top_Latency(TOP_addsd);
  case OPC_F8U4CVT: 
    return LNOTARGET_Top_Latency(TOP_mov32) +
      LNOTARGET_Top_Latency(TOP_cvtsi2sdq);

  case OPC_I8F4TRUNC: 
  case OPC_I8F4RND: 
  case OPC_I8F4CEIL: 
  case OPC_I8F4FLOOR: 
  case OPC_I8F4CVT: 
  case OPC_U8F4CVT: 
  case OPC_U8F4TRUNC:
  case OPC_U8F4RND:
  case OPC_U8F4CEIL:
  case OPC_U8F4FLOOR:
    return LNOTARGET_Top_Latency(TOP_cvttss2siq);
  case OPC_I4F4TRUNC: 
  case OPC_I4F4RND: 
  case OPC_I4F4CEIL: 
  case OPC_I4F4FLOOR: 
  case OPC_I4F4CVT: 
    return LNOTARGET_Top_Latency(TOP_cvttss2si);
  case OPC_U4F4TRUNC: 
  case OPC_U4F4RND: 
  case OPC_U4F4CEIL: 
  case OPC_U4F4FLOOR: 
  case OPC_U4F4CVT: 
    return LNOTARGET_Top_Latency(TOP_cvttss2siq);
  case OPC_I8F8TRUNC: 
  case OPC_U8F8TRUNC:
  case OPC_I8F8RND: 
  case OPC_U8F8RND:
  case OPC_I8F8CEIL:
  case OPC_U8F8CEIL:
  case OPC_I8F8FLOOR:
  case OPC_U8F8FLOOR:
  case OPC_I8F8CVT: 
  case OPC_U8F8CVT:
    return LNOTARGET_Top_Latency(TOP_cvttsd2siq);
  case OPC_I4F8TRUNC:     
  case OPC_I4F8RND:     
  case OPC_I4F8CEIL:     
  case OPC_I4F8FLOOR:     
  case OPC_I4F8CVT:     
    return LNOTARGET_Top_Latency(TOP_cvttsd2si);
  case OPC_U4F8TRUNC: 
  case OPC_U4F8RND: 
  case OPC_U4F8CEIL: 
  case OPC_U4F8FLOOR: 
  case OPC_U4F8CVT: 
    return LNOTARGET_Top_Latency(TOP_cvttsd2siq);
 
  default:
    FmtAssert(FALSE, ("NYI2.1"));
    break;
  }

  switch (opcode) {
    // Is_CVTL_Opcode
  case OPC_U8I4CVT: 
  case OPC_U8U4CVT: 
  case OPC_I8I4CVT: 
  case OPC_I8U4CVT: 
  case OPC_I4I8CVT: 
  case OPC_I4U8CVT: 
  case OPC_U4U8CVT: 
  case OPC_U4I8CVT: 
    {
      const BOOL is_64bit = MTYPE_is_size_double(rtype);
      const BOOL signed_extension = MTYPE_is_signed(desc) || 
	(MTYPE_bit_size(desc) > MTYPE_bit_size(rtype));
      TOP new_opcode;

      if ( is_64bit )
	new_opcode = signed_extension ? TOP_movslq: TOP_mov64;
      else if ( MTYPE_bit_size(rtype) == 32)
	new_opcode = TOP_mov32;
      return LNOTARGET_Top_Latency(new_opcode);
    }
    break;
  }
  return 0;
}

INT
LNOTARGET_FP_Madd_Add_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_F4 ? TOP_addss: TOP_addsd);
  return LNOTARGET_Top_Latency(add);
}

INT
LNOTARGET_FP_Madd_Mult_Lat (TYPE_ID mtype)
{
  TOP mpy = (mtype == MTYPE_F4 ? TOP_mulss: TOP_mulsd);
  return LNOTARGET_Top_Latency(mpy);
}

INT
LNOTARGET_FP_Min_Max_Lat (TYPE_ID mtype)
{
  TOP min = (mtype == MTYPE_F4 ? TOP_minss: TOP_minsd);
  TOP max = (mtype == MTYPE_F4 ? TOP_maxss: TOP_maxsd);
  return LNOTARGET_Top_Latency(min)+
    LNOTARGET_Top_Latency(max);
}

INT
LNOTARGET_FP_Div_Lat (TYPE_ID mtype)
{
  TOP div = (mtype == MTYPE_F4 ? TOP_divss: TOP_divsd);
  return LNOTARGET_Top_Latency(div);
}

INT
LNOTARGET_FP_Recip_Lat (TYPE_ID mtype)
{
  TOP div = (mtype == MTYPE_F4 ? TOP_divss: TOP_divsd);
  TOP recip = (mtype == MTYPE_F4 ? TOP_ldc32: TOP_ldc64);
  return LNOTARGET_Top_Latency(div) +
    LNOTARGET_Top_Latency(recip);
}

INT
LNOTARGET_FP_Rsqrt_Lat (TYPE_ID mtype)
{
  TOP rsqrt = (mtype == MTYPE_F4 ? TOP_sqrtss: TOP_sqrtsd);
  return LNOTARGET_Top_Latency(rsqrt);
}

INT
LNOTARGET_FP_Sqrt_Lat (TYPE_ID mtype)
{
  TOP sqrt = (mtype == MTYPE_F4 ? TOP_sqrtss: TOP_sqrtsd);
  return LNOTARGET_Top_Latency(sqrt);
}

INT
LNOTARGET_FP_Exp_Lat (INTRINSIC intr, INT num_multiplies)
{
  return -1;
}

INT
LNOTARGET_FP_Floor_Lat (TYPE_ID mtype)
{
  if (mtype == MTYPE_F4) {
    return LNOTARGET_Top_Latency(TOP_andps) * 2
	   + LNOTARGET_Top_Latency(TOP_orps)
	   + LNOTARGET_Top_Latency(TOP_addss)
	   + LNOTARGET_Top_Latency(TOP_subss) * 3
	   + LNOTARGET_Top_Latency(TOP_cmpss);
  } else if (mtype == MTYPE_F8) {
    return LNOTARGET_Top_Latency(TOP_andpd) * 2
	   + LNOTARGET_Top_Latency(TOP_orpd)
	   + LNOTARGET_Top_Latency(TOP_addsd)
	   + LNOTARGET_Top_Latency(TOP_subsd) * 3
	   + LNOTARGET_Top_Latency(TOP_cmpsd);
  }
  FmtAssert(FALSE, ("LNOTARGET_FP_Floor_Res: mtype not yet implemented"));
  return 0;	// dummy
}

INT
LNOTARGET_FP_Select_Lat (TYPE_ID mtype)
{
  BOOL is_64bit = MTYPE_is_size_double(mtype);
  TOP andp = is_64bit ? TOP_andpd : TOP_andps;
  TOP andnp = is_64bit ? TOP_andnpd : TOP_andnps;
  TOP orp = is_64bit ? TOP_orpd : TOP_orps;
  return LNOTARGET_Top_Latency(andp) +
    LNOTARGET_Top_Latency(andnp) +
    LNOTARGET_Top_Latency(orp);
}

INT
LNOTARGET_FP_Compare_Lat (TYPE_ID mtype)
{
  TOP cmp = (mtype == MTYPE_F4 ? TOP_cmpss: TOP_cmpsd);
  return LNOTARGET_Top_Latency(cmp);
}

INT
LNOTARGET_Complex_Add_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_C4 ? TOP_addss: TOP_addsd);
  return 2*LNOTARGET_Top_Latency(add);
}

INT
LNOTARGET_Complex_Mult_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_C4 ? TOP_addss: TOP_addsd);
  TOP mpy = (mtype == MTYPE_C4 ? TOP_mulss: TOP_mulsd);
  return 2*LNOTARGET_Top_Latency(add) 
    + 4*LNOTARGET_Top_Latency(mpy);
}

INT
LNOTARGET_Complex_Neg_Lat (TYPE_ID mtype)
{
  TOP load = (mtype == MTYPE_C4 ? TOP_ldss: TOP_ldsd);
  TOP xori = (mtype == MTYPE_C4 ? TOP_xorps: TOP_xorpd);
  return 2*LNOTARGET_Top_Latency(load) 
    + 2*LNOTARGET_Top_Latency(xori);
}
