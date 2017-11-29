/*
 * Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
 */

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


#include "lnotarget.h"
#include "w2op.h"

TOP
LNOTARGET_Whirl_To_Top (WN* wn)
{
  return WHIRL_To_TOP(wn);
}

void
LNOTARGET_Loop_Inc_Test_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_addi);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_beq);
}

double
LNOTARGET_Cvt_Res (TI_RES_COUNT* resource_count, OPCODE opcode)
{
  return 0.0;
}

double
LNOTARGET_FP_Madd_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP madd = (mtype == MTYPE_F4 ? TOP_fmadds : TOP_fmadd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, madd);
  return 1.0;
}

double
LNOTARGET_FP_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 3.0;
}

double
LNOTARGET_FP_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 1.0;
}

double
LNOTARGET_FP_Recip_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 1.0;
}

double
LNOTARGET_FP_Rsqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP top = TOP_frsqrte;
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  return 1.0;
}

double
LNOTARGET_FP_Sqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_F4) ? TOP_fsqrts : TOP_fsqrt;
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  return 1.0;
}
 
double
LNOTARGET_FP_Exp_Res (TI_RES_COUNT* resource_count, 
                      INTRINSIC intr,
                      INT num_multiplies)
{
  INT i;
  switch (intr) {
    case INTRN_F4I4EXPEXPR: 
    case INTRN_F4I8EXPEXPR: 
      /*for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
      }*/
      return num_multiplies;
    case INTRN_F8I4EXPEXPR: 
    case INTRN_F8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmul);
      }
      return num_multiplies;
    case INTRN_C4I4EXPEXPR: 
    case INTRN_C4I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmadds);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmadds);
        // TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
        // TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
      }
      return 4*num_multiplies;
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmadd);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmadd);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmul);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmul);
      }
      return 4*num_multiplies;
  }
  return 0.0;
}

double
LNOTARGET_Complex_Add_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 2.0;
}

double
LNOTARGET_Complex_Mult_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP mul = TOP_fmul;//(mtype == MTYPE_C4 ? TOP_mul_s : TOP_fmul);
  TOP madd = TOP_fmadd;//(mtype == MTYPE_F4 ? TOP_fmadds : TOP_fmadd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, mul);
  TI_RES_COUNT_Add_Op_Resources(resource_count, mul);
  TI_RES_COUNT_Add_Op_Resources(resource_count, madd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, madd);
  return 4.0;
}

double
LNOTARGET_Complex_Neg_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 2.0;
}

double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count)
{
  return 2.0;
}

double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_andi_);
  return 1.0;
}

double
LNOTARGET_Int_Neg_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subf);
  return 1.0;
}

double
LNOTARGET_Int_Abs_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return 3.0;
}

double
LNOTARGET_Int_Bnot_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_nor);  
  return 1.0;
}

double
LNOTARGET_Int_Lnot_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xori);  
  return 1.0;
}

double
LNOTARGET_Int_Mult_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return 2.0;
}

double
LNOTARGET_Int_Add_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return 1.0;
}

double
LNOTARGET_Int_Sub_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  //TI_RES_COUNT_Add_Op_Resources(resource_count, 
	//			eight_bytes?TOP_dsub:TOP_sub);
  return 1.0;
}

double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, 
		       BOOL eight_bytes, 
		       BOOL mtype_signed)
{
  return 3.0;  
}

double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return (8.0);
}

double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return (3.0);
}

double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return (4.0);
}

double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, BOOL minmax)
{
  return (3.0);  
}
  
double
LNOTARGET_Int_Band_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);  
  return 1.0;
}

double
LNOTARGET_Int_Bior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);  
  return 1.0;
}

double
LNOTARGET_Int_Bnor_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_nor);  
  return 1.0;
}

double
LNOTARGET_Int_Bxor_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);
  return 1.0;
}

double
LNOTARGET_Int_Land_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
  return 1.0;
}

double
LNOTARGET_Int_Cand_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
  return 1.0;
}

double
LNOTARGET_Int_Lior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  return 1.0;
}

double
LNOTARGET_Int_Cior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  return 1.0;
}

double
LNOTARGET_Int_Shl_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return 1.0;
}

double
LNOTARGET_Int_Ashr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return 1.0;
}

double
LNOTARGET_Int_Lshr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return 1.0;
}

double
LNOTARGET_Int_Eq_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_bne);
  return 1.0;
}

double
LNOTARGET_Int_Ne_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_beq);
  return 1.0;
}

double
LNOTARGET_Int_Gt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmpw);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_beq);
  return 2.0;
}

double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmpw);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_bne);
  return 2.0;
}

double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmpw);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_beq);
  return 2.0;
}

double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmpw);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_bne);
  return 2.0;
}

double
LNOTARGET_Int_Lda_Res (TI_RES_COUNT* resource_count)
{
  FmtAssert(FALSE, ("NYI")); 
  return 2.0;
}

INT
LNOTARGET_Cvt_Lat (OPCODE opcode)
{
  return TI_LATENCY_Result_Available_Cycle(TOP_frsp, 0) 
       - TI_LATENCY_Operand_Access_Cycle(TOP_frsp, 0);
}

INT
LNOTARGET_FP_Madd_Add_Lat (TYPE_ID mtype)
{
  TOP madd = (mtype == MTYPE_F4 ? TOP_fmadds : TOP_fmadd);
  return TI_LATENCY_Result_Available_Cycle(madd, 0) 
       - TI_LATENCY_Operand_Access_Cycle(madd, 0);
}

INT
LNOTARGET_FP_Madd_Mult_Lat (TYPE_ID mtype)
{
  TOP madd = (mtype == MTYPE_F4 ? TOP_fmadds : TOP_fmadd);
  return TI_LATENCY_Result_Available_Cycle(madd, 0) 
       - TI_LATENCY_Operand_Access_Cycle(madd, 0);
}

INT
LNOTARGET_FP_Min_Max_Lat (TYPE_ID mtype)
{
  return 2;
}

INT
LNOTARGET_FP_Div_Lat (TYPE_ID mtype)
{
  TOP top = TOP_fdiv; // (mtype == MTYPE_F4 ? TOP_div_s : TOP_fdiv);
  return TI_LATENCY_Result_Available_Cycle(top, 0) 
       - TI_LATENCY_Operand_Access_Cycle(top, 0);
}

INT
LNOTARGET_FP_Recip_Lat (TYPE_ID mtype)
{
  TOP top = TOP_fres; // (mtype == MTYPE_F4 ? TOP_fres : TOP_recip_d);
  return TI_LATENCY_Result_Available_Cycle(top, 0) 
       - TI_LATENCY_Operand_Access_Cycle(top, 0);
}

INT
LNOTARGET_FP_Rsqrt_Lat (TYPE_ID mtype)
{
  TOP top = TOP_frsqrte;
  return TI_LATENCY_Result_Available_Cycle(top, 0) 
       - TI_LATENCY_Operand_Access_Cycle(top, 0);
}

INT
LNOTARGET_FP_Sqrt_Lat (TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_F4 ? TOP_fsqrts : TOP_fsqrt);
  return TI_LATENCY_Result_Available_Cycle(top, 0) 
       - TI_LATENCY_Operand_Access_Cycle(top, 0);
}

INT
LNOTARGET_FP_Exp_Lat (INTRINSIC intr, INT num_multiplies)
{
  return -1;
}

INT
LNOTARGET_Complex_Add_Lat (TYPE_ID mtype)
{
  TOP top = TOP_fadd;// (mtype == MTYPE_C4 ? TOP_add_s : TOP_fadd);
  return 2 * (TI_LATENCY_Result_Available_Cycle(top, 0) -
	      TI_LATENCY_Operand_Access_Cycle(top,0));
}

INT
LNOTARGET_Complex_Mult_Lat (TYPE_ID mtype)
{
  TOP mul = TOP_fmul;//(mtype == MTYPE_C4 ? TOP_mul_s : TOP_fmul);
  TOP madd = TOP_fmadd;//(mtype == MTYPE_F4 ? TOP_fmadds : TOP_fmadd);
  return 2 * (TI_LATENCY_Result_Available_Cycle(mul, 0) -
	      TI_LATENCY_Operand_Access_Cycle(mul,0)) +
         2 * (TI_LATENCY_Result_Available_Cycle(madd, 0) -
	      TI_LATENCY_Operand_Access_Cycle(madd,0));
}

INT
LNOTARGET_Complex_Neg_Lat (TYPE_ID mtype)
{
  TOP top = TOP_fneg;//(mtype == MTYPE_C4 ? TOP_neg_s : TOP_fnet);
  return 2 * (TI_LATENCY_Result_Available_Cycle(top, 0) -
	      TI_LATENCY_Operand_Access_Cycle(top,0));
}
