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
  switch(OPCODE_operator(opcode)) {
  case OPR_CVT:
    if (MTYPE_is_float(OPCODE_rtype(opcode)) && 
	MTYPE_is_integral(OPCODE_desc(opcode))) {
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mtc1);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_w);
      return 2.0;
    }
    if (MTYPE_is_integral(OPCODE_rtype(opcode)) && 
	MTYPE_is_float(OPCODE_desc(opcode))) {
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_w_s);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfc1);
      return 2.0;
    }
    switch(opcode) {
    case OPC_F8F4CVT: 
    case OPC_F4F8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_s_d);
      return 1.0;
    case OPC_U4U8CVT:
    case OPC_I4U8CVT:
    case OPC_I4I8CVT:
    case OPC_U4I8CVT:
    case OPC_U8U4CVT:
    case OPC_I8U4CVT:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsll32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsra32);
      return 2.0;
    }
    case OPC_U8I4CVT:
    case OPC_I8I4CVT:
      return 0.0;
    break;
  case OPR_RND:
  case OPR_TRUNC:
  case OPR_CEIL:
  case OPR_FLOOR:
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_w_s);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfc1);
    return 2.0;
  }
  FmtAssert(FALSE, ("NYI")); 
  return 0.0;
}

double
LNOTARGET_FP_Madd_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP madd = (mtype == MTYPE_F4 ? TOP_madd_s : TOP_madd_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, madd);
  return 1.0;
}

double
LNOTARGET_FP_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_c_lt_s);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movt_s);
  return 3.0;
}

double
LNOTARGET_FP_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP div = (mtype == MTYPE_F4) ? TOP_div_s : TOP_div_d;
  TI_RES_COUNT_Add_Op_Resources(resource_count, div);
  return 1.0;
}

double
LNOTARGET_FP_Recip_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_F4) ? TOP_recip_s : TOP_recip_d;
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  return 1.0;
}

double
LNOTARGET_FP_Rsqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_F4) ? TOP_rsqrt_s : TOP_rsqrt_d;
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  return 1.0;
}

double
LNOTARGET_FP_Sqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_F4) ? TOP_sqrt_s : TOP_sqrt_d;
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
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
      }
      return num_multiplies;
    case INTRN_F8I4EXPEXPR: 
    case INTRN_F8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_d);
      }
      return num_multiplies;
    case INTRN_C4I4EXPEXPR: 
    case INTRN_C4I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_madd_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_madd_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
      }
      return 4*num_multiplies;
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_madd_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_madd_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_d);
      }
      return 4*num_multiplies;
  }
  return 0.0;
}

double
LNOTARGET_Complex_Add_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_C4 ? TOP_add_s : TOP_add_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  return 2.0;
}

double
LNOTARGET_Complex_Mult_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP mul = (mtype == MTYPE_C4 ? TOP_mul_s : TOP_mul_d);
  TOP madd = (mtype == MTYPE_F4 ? TOP_madd_s : TOP_madd_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, mul);
  TI_RES_COUNT_Add_Op_Resources(resource_count, mul);
  TI_RES_COUNT_Add_Op_Resources(resource_count, madd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, madd);
  return 4.0;
}

double
LNOTARGET_Complex_Neg_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_C4 ? TOP_neg_s : TOP_neg_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  return 2.0;
}

double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movz);  
  return 2.0;
}

double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_andi);
  return 1.0;
}

double
LNOTARGET_Int_Neg_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
  return 1.0;
}

double
LNOTARGET_Int_Abs_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subu);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movz);  
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
  char *op_name = NULL;
#ifdef TARG_SL
  if (Is_Target_Sl5()) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_smult);
  } 
  else 
#else
  {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mult);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mflo);
  }
#endif
  return 2.0;
}

double
LNOTARGET_Int_Add_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_dadd:TOP_add);  
  return 1.0;
}

double
LNOTARGET_Int_Sub_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_dsub:TOP_sub);
  return 1.0;
}

double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, 
		       BOOL eight_bytes, 
		       BOOL mtype_signed)
{
  if (!mtype_signed) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, 
				  eight_bytes?TOP_ddivu:TOP_divu);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mflo);
    return (3.0);
  } else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mtc1);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mtc1);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_w);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_w);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_div_d);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_w_d);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfc1);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);
    return (8.0);
  }
}

double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_ddiv:TOP_div);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfhi);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movn);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movz);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_daddu:TOP_addu);
  return (8.0);
}

double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_ddiv:TOP_div);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfhi);
  return (3.0);
}

double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_ddivu:TOP_divu);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mflo);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfhi);
  return (4.0);
}

double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, BOOL minmax)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movz);
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
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_dsll:TOP_sll);
  return 1.0;
}

double
LNOTARGET_Int_Ashr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_dsra:TOP_sra);
  return 1.0;
}

double
LNOTARGET_Int_Lshr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, 
				eight_bytes?TOP_dsrl:TOP_srl);
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
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_beq);
  return 2.0;
}

double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_bne);
  return 2.0;
}

double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_beq);
  return 2.0;
}

double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
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
  return TI_LATENCY_Result_Available_Cycle(TOP_cvt_d_s, 0) 
       - TI_LATENCY_Operand_Access_Cycle(TOP_cvt_d_s, 0);
}

INT
LNOTARGET_FP_Madd_Add_Lat (TYPE_ID mtype)
{
  TOP madd = (mtype == MTYPE_F4 ? TOP_madd_s : TOP_madd_d);
  return TI_LATENCY_Result_Available_Cycle(madd, 0) 
       - TI_LATENCY_Operand_Access_Cycle(madd, 0);
}

INT
LNOTARGET_FP_Madd_Mult_Lat (TYPE_ID mtype)
{
  TOP madd = (mtype == MTYPE_F4 ? TOP_madd_s : TOP_madd_d);
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
  TOP top = (mtype == MTYPE_F4 ? TOP_div_s : TOP_div_d);
  return TI_LATENCY_Result_Available_Cycle(top, 0) 
       - TI_LATENCY_Operand_Access_Cycle(top, 0);
}

INT
LNOTARGET_FP_Recip_Lat (TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_F4 ? TOP_recip_s : TOP_recip_d);
  return TI_LATENCY_Result_Available_Cycle(top, 0) 
       - TI_LATENCY_Operand_Access_Cycle(top, 0);
}

INT
LNOTARGET_FP_Rsqrt_Lat (TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_F4 ? TOP_rsqrt_s : TOP_rsqrt_d);
  return TI_LATENCY_Result_Available_Cycle(top, 0) 
       - TI_LATENCY_Operand_Access_Cycle(top, 0);
}

INT
LNOTARGET_FP_Sqrt_Lat (TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_F4 ? TOP_sqrt_s : TOP_sqrt_d);
  return TI_LATENCY_Result_Available_Cycle(top, 0) 
       - TI_LATENCY_Operand_Access_Cycle(top, 0);
}

INT
LNOTARGET_FP_Exp_Lat (INTRINSIC intr, INT num_multiplies)
{
  switch (intr) {
    case INTRN_F4I4EXPEXPR: 
    case INTRN_F4I8EXPEXPR:
      return num_multiplies * LNOTARGET_Top_Latency(TOP_mul_s);
    case INTRN_F8I4EXPEXPR: 
    case INTRN_F8I8EXPEXPR:
      return num_multiplies * LNOTARGET_Top_Latency(TOP_mul_d);
    case INTRN_C4I4EXPEXPR: 
    case INTRN_C4I8EXPEXPR:
      return num_multiplies * (2*LNOTARGET_Top_Latency(TOP_mul_s) + 
                               2*LNOTARGET_Top_Latency(TOP_madd_s));
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR:
      return num_multiplies * (2*LNOTARGET_Top_Latency(TOP_mul_d) + 
                               2*LNOTARGET_Top_Latency(TOP_madd_d));
  }
  return -1;
}

INT
LNOTARGET_Complex_Add_Lat (TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_C4 ? TOP_add_s : TOP_add_d);
  return 2 * (TI_LATENCY_Result_Available_Cycle(top, 0) -
	      TI_LATENCY_Operand_Access_Cycle(top,0));
}

INT
LNOTARGET_Complex_Mult_Lat (TYPE_ID mtype)
{
  TOP mul = (mtype == MTYPE_C4 ? TOP_mul_s : TOP_mul_d);
  TOP madd = (mtype == MTYPE_F4 ? TOP_madd_s : TOP_madd_d);
  return 2 * (TI_LATENCY_Result_Available_Cycle(mul, 0) -
	      TI_LATENCY_Operand_Access_Cycle(mul,0)) +
         2 * (TI_LATENCY_Result_Available_Cycle(madd, 0) -
	      TI_LATENCY_Operand_Access_Cycle(madd,0));
}

INT
LNOTARGET_Complex_Neg_Lat (TYPE_ID mtype)
{
  TOP top = (mtype == MTYPE_C4 ? TOP_neg_s : TOP_neg_d);
  return 2 * (TI_LATENCY_Result_Available_Cycle(top, 0) -
	      TI_LATENCY_Operand_Access_Cycle(top,0));
}
