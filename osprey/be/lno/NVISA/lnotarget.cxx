/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
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
      return TOP_add_f32;
    case OPC_F8ADD:
      return TOP_add_f64;
    case OPC_F4SUB:
      return TOP_sub_f32;
    case OPC_F8SUB:
      return TOP_sub_f64;
    case OPC_F4MPY:
      return TOP_mul_f32;
    case OPC_F8MPY:
      return TOP_mul_f64;
    case OPC_BF4LT:  
    case OPC_BF8LT:  
    case OPC_I4F4LT: 
    case OPC_I4F8LT: 
      return TOP_setp_lt_f32;
    case OPC_BF4GT:
    case OPC_BF8GT:
    case OPC_I4F4GT: 
    case OPC_I4F8GT: 
      return TOP_setp_gt_f32;
    case OPC_BF4LE:  
    case OPC_BF8LE:  
    case OPC_I4F4LE: 
    case OPC_I4F8LE: 
      return TOP_setp_le_f32;
    case OPC_BF4GE:
    case OPC_BF8GE:
    case OPC_I4F4GE: 
    case OPC_I4F8GE: 
      return TOP_setp_ge_f32;
    case OPC_BF4EQ:  
    case OPC_BF8EQ:  
    case OPC_I4F4EQ: 
    case OPC_I4F8EQ: 
      return TOP_setp_eq_f32;
    case OPC_BF4NE:
    case OPC_BF8NE:
    case OPC_I4F4NE: 
    case OPC_I4F8NE: 
      return TOP_setp_ne_f32;
  }
  return WHIRL_To_TOP(wn);
}

void
LNOTARGET_Loop_Inc_Test_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setp_eq_s32);
}

double
LNOTARGET_Cvt_Res (TI_RES_COUNT* resource_count, OPCODE opcode)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 0.0;
}

double
LNOTARGET_FP_Madd_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fma = (mtype == MTYPE_F4 ? TOP_mad_f32 : TOP_mad_f64);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  return 1.0;
}

double
LNOTARGET_FP_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setp_eq_f32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_f32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_f32);
  return 3.0;
}

double
LNOTARGET_FP_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_rcp_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f64);
    return 10.0;
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_rcp_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    return 8.0;
  }
}

double
LNOTARGET_FP_Recip_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_rcp_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f64);
    return 9.0;
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_rcp_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
    return 7.0;
  }
}

double
LNOTARGET_FP_Rsqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  BOOL is_double = (MTYPE_is_size_double(mtype) != 0);
  TOP fnma = is_double ? TOP_mad_f64 : TOP_mad_f32;
  TOP fma  = is_double ? TOP_mad_f64  : TOP_mad_f32;
  TOP fmpy = is_double ? TOP_mul_f64 : TOP_mul_f32;

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);

  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);

  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);

  if (is_double) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
    return 13.0;
  }
  else {
    return 10.0;
  }
}

double
LNOTARGET_FP_Sqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  BOOL is_double = (MTYPE_is_size_double(mtype) != 0);
  TOP fnma = is_double ? TOP_mad_f64 : TOP_mad_f32;
  TOP fma  = is_double ? TOP_mad_f64  : TOP_mad_f32;
  TOP fmpy = is_double ? TOP_mul_f64 : TOP_mul_f32;
  TOP fadd = is_double ? TOP_add_f64 : TOP_add_f32;

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_f32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);

  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  
  if (is_double) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
    TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  }

  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  return (is_double ? 24.0 : 19.0);
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
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
      }
      return num_multiplies;
    case INTRN_F8I4EXPEXPR: 
    case INTRN_F8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f64);
      }
      return num_multiplies;
    case INTRN_C4I4EXPEXPR: 
    case INTRN_C4I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f32);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f32);
      }
      return 4*num_multiplies;
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f64);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mad_f64);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f64);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_f64);
      }
      return 4*num_multiplies;
  }
  return 0.0;
}

double
LNOTARGET_Complex_Add_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fadd = (mtype == MTYPE_C4 ? TOP_add_f32 : TOP_add_f64);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  return 2.0;
}

double
LNOTARGET_Complex_Mult_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fmpy = (mtype == MTYPE_C4 ? TOP_mul_f32 : TOP_mul_f64);
  TOP fma  = (mtype == MTYPE_C4 ? TOP_mad_f32  : TOP_mad_f64);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  return 4.0;
}

double
LNOTARGET_Complex_Neg_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 2.0;
}

double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  return 3.0;
}

double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 1.0;
}

double
LNOTARGET_Int_Neg_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_not_b32);
  return 1.0;
}

double
LNOTARGET_Int_Abs_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_abs_s32);
  return 1.0;
}

double
LNOTARGET_Int_Bnot_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_not_b32);
  return 1.0;
}

double
LNOTARGET_Int_Lnot_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor_b32);
  return 1.0;
}

double
LNOTARGET_Int_Mult_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 4.0;
}

double
LNOTARGET_Int_Add_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_s32);  
  return 1.0;
}

double
LNOTARGET_Int_Sub_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub_s32);  
  return 1.0;
}

double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  if (eight_bytes)
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_div_s64);  
  else
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_div_s32);  
  return 1.0;
}

double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  if (eight_bytes)
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_rem_s64);  
  else
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_rem_s32);  
  return 1.0;
}

double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  if (eight_bytes)
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_rem_s64);  
  else
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_rem_s32);  
  return 1.0;
}

double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 4.0;
}

double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, BOOL minmax)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_min_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_max_s32);
  return 2.0;
}
  
double
LNOTARGET_Int_Band_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and_b32);  
  return 1.0;
}

double
LNOTARGET_Int_Bior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or_b32);  
  return 1.0;
}

double
LNOTARGET_Int_Bnor_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or_b32);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor_b32);  
  return 2.0;
}

double
LNOTARGET_Int_Bxor_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor_b32);  
  return 1.0;
}

double
LNOTARGET_Int_Land_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and_b32);  
  return 1.0;
}

double
LNOTARGET_Int_Cand_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and_b32);  
  return 1.0;
}

double
LNOTARGET_Int_Lior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or_b32);  
  return 1.0;
}

double
LNOTARGET_Int_Cior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or_b32);  
  return 1.0;
}

double
LNOTARGET_Int_Shl_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shl_b32);
    return 1.0;
}

double
LNOTARGET_Int_Ashr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shr_s32);
    return 1.0;
}

double
LNOTARGET_Int_Lshr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shr_s32);
    return 1.0;
}

double
LNOTARGET_Int_Eq_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setp_eq_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  return 3.0;
}

double
LNOTARGET_Int_Ne_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setp_eq_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  return 3.0;
}

double
LNOTARGET_Int_Gt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setp_eq_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  return 3.0;
}

double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setp_eq_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  return 3.0;
}

double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setp_eq_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  return 3.0;
}

double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setp_eq_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s32);
  return 3.0;
}

double
LNOTARGET_Int_Lda_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_s32);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_s32);
  return 2.0;
}


INT
LNOTARGET_Cvt_Lat (OPCODE opcode)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return -1;
}

INT
LNOTARGET_FP_Madd_Add_Lat (TYPE_ID mtype)
{
  TOP fma = (mtype == MTYPE_F4 ? TOP_mad_f32 : TOP_mad_f64);
  return TI_LATENCY_Result_Available_Cycle(fma, 0) 
       - TI_LATENCY_Operand_Access_Cycle(fma, 0);
}

INT
LNOTARGET_FP_Madd_Mult_Lat (TYPE_ID mtype)
{
  TOP fma = (mtype == MTYPE_F4 ? TOP_mad_f32 : TOP_mad_f64);
  return TI_LATENCY_Result_Available_Cycle(fma, 0) 
       - TI_LATENCY_Operand_Access_Cycle(fma, 1);
}

INT
LNOTARGET_FP_Min_Max_Lat (TYPE_ID mtype)
{
  return LNOTARGET_Top_Latency(TOP_min_f32)
       + LNOTARGET_Top_Latency(TOP_max_f32);
}

INT
LNOTARGET_FP_Div_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 20;
}

INT
LNOTARGET_FP_Recip_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 15;
}

INT
LNOTARGET_FP_Rsqrt_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 15;
}

INT
LNOTARGET_FP_Sqrt_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 20;
}

INT
LNOTARGET_FP_Exp_Lat (INTRINSIC intr, INT num_multiplies)
{
  switch (intr) {
    case INTRN_F4I4EXPEXPR: 
    case INTRN_F4I8EXPEXPR:
      return num_multiplies * LNOTARGET_Top_Latency(TOP_mul_f32);
    case INTRN_F8I4EXPEXPR: 
    case INTRN_F8I8EXPEXPR:
      return num_multiplies * LNOTARGET_Top_Latency(TOP_mul_f64);
    case INTRN_C4I4EXPEXPR: 
    case INTRN_C4I8EXPEXPR:
      return num_multiplies * (2*LNOTARGET_Top_Latency(TOP_mul_f32) + 
                               2*LNOTARGET_Top_Latency(TOP_add_f32));
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR:
      return num_multiplies * (2*LNOTARGET_Top_Latency(TOP_mul_f64) + 
                               2*LNOTARGET_Top_Latency(TOP_add_f64));
  }
  return -1;
}

INT
LNOTARGET_Complex_Add_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_C4 ? TOP_add_f32 : TOP_add_f64);
  return 2*LNOTARGET_Top_Latency(add);
}

INT
LNOTARGET_Complex_Mult_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_C4 ? TOP_add_f32 : TOP_add_f64);
  TOP mul = (mtype == MTYPE_C4 ? TOP_mul_f32 : TOP_mul_f64);
  return 2*LNOTARGET_Top_Latency(add) 
       + 2*LNOTARGET_Top_Latency(mul);
}

INT
LNOTARGET_Complex_Neg_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 2*LNOTARGET_Top_Latency(TOP_not_b64);
}
