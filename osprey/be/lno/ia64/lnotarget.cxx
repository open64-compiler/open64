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
      return TOP_fadd_s;
    case OPC_F8ADD:
      return TOP_fadd_d;
    case OPC_F4SUB:
      return TOP_fsub_s;
    case OPC_F8SUB:
      return TOP_fsub_d;
    case OPC_F4MPY:
      return TOP_fmpy_s;
    case OPC_F8MPY:
      return TOP_fmpy_d;
    case OPC_BF4LT:  
    case OPC_BF8LT:  
    case OPC_I4F4LT: 
    case OPC_I4F8LT: 
      return TOP_fcmp_lt;
    case OPC_BF4GT:
    case OPC_BF8GT:
    case OPC_I4F4GT: 
    case OPC_I4F8GT: 
      return TOP_fcmp_gt;
    case OPC_BF4LE:  
    case OPC_BF8LE:  
    case OPC_I4F4LE: 
    case OPC_I4F8LE: 
      return TOP_fcmp_le;
    case OPC_BF4GE:
    case OPC_BF8GE:
    case OPC_I4F4GE: 
    case OPC_I4F8GE: 
      return TOP_fcmp_ge;
    case OPC_BF4EQ:  
    case OPC_BF8EQ:  
    case OPC_I4F4EQ: 
    case OPC_I4F8EQ: 
      return TOP_fcmp_eq;
    case OPC_BF4NE:
    case OPC_BF8NE:
    case OPC_I4F4NE: 
    case OPC_I4F8NE: 
      return TOP_fcmp_neq;
  }
  return WHIRL_To_TOP(wn);
}

void
LNOTARGET_Loop_Inc_Test_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_adds);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_br_cloop);
}

double
LNOTARGET_Cvt_Res (TI_RES_COUNT* resource_count, OPCODE opcode)
{
  switch(opcode) {
    case OPC_F4I4CVT: 
    case OPC_F4I8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnorm_s);
      return 3.0;
    case OPC_F8I4CVT: 
    case OPC_F8I8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnorm_d);
      return 3.0;
    case OPC_F4U4CVT: 
    case OPC_F4U8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnorm_s);
      return 2.0;
    case OPC_F8U4CVT: 
    case OPC_F8U8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnorm_d);
      return 2.0;
    case OPC_I4F4CVT: 
    case OPC_I4F8CVT: 
    case OPC_I8F4CVT: 
    case OPC_I8F8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);
      return 2.0;
    case OPC_U4F4CVT: 
    case OPC_U4F8CVT: 
    case OPC_U8F4CVT: 
    case OPC_U8F8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fxu);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);
      return 2.0;
    case OPC_I4F4TRUNC: 
    case OPC_I8F4TRUNC: 
    case OPC_I4F8TRUNC: 
    case OPC_I8F8TRUNC: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx_trunc);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);
      return 2.0;
    case OPC_U4F4TRUNC:
    case OPC_U8F4TRUNC:
    case OPC_U4F8TRUNC:
    case OPC_U8F8TRUNC:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fxu_trunc);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);
      return 2.0;
    case OPC_I4F4RND: 
    case OPC_I8F4RND: 
    case OPC_I4F8RND: 
    case OPC_I8F8RND: 
    case OPC_I4F4CEIL: 
    case OPC_I8F4CEIL: 
    case OPC_I4F8CEIL: 
    case OPC_I8F8CEIL: 
    case OPC_I4F4FLOOR: 
    case OPC_I8F4FLOOR: 
    case OPC_I4F8FLOOR: 
    case OPC_I8F8FLOOR: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fsetc);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);
      return 3.0;
    case OPC_U4F4RND:
    case OPC_U8F4RND:
    case OPC_U4F8RND:
    case OPC_U8F8RND:
    case OPC_U4F4CEIL:
    case OPC_U8F4CEIL:
    case OPC_U4F8CEIL:
    case OPC_U8F8CEIL:
    case OPC_U4F4FLOOR:
    case OPC_U8F4FLOOR:
    case OPC_U4F8FLOOR:
    case OPC_U8F8FLOOR:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fxu);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fsetc);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);
      return 3.0;
    case OPC_F8F4CVT:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnorm_d);
      return 1.0;
    case OPC_F4F8CVT:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnorm_s);
      return 1.0;
  }
  return 0.0;
}

double
LNOTARGET_FP_Madd_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fma = (mtype == MTYPE_F4 ? TOP_fma_s : TOP_fma_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  return 1.0;
}

double
LNOTARGET_FP_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcmp_lt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_f);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_f);
  return 3.0;
}

double
LNOTARGET_FP_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frcpa);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma_d);
    return 10.0;
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frcpa);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma_s);
    return 8.0;
  }
}

double
LNOTARGET_FP_Recip_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frcpa);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma_d);
    return 9.0;
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frcpa);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fnma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma_s);
    return 7.0;
  }
}

double
LNOTARGET_FP_Rsqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  BOOL is_double = (MTYPE_is_size_double(mtype) != 0);
  TOP fnma = is_double ? TOP_fnma_d : TOP_fnma_s;
  TOP fma  = is_double ? TOP_fma_d  : TOP_fma_s;
  TOP fmpy = is_double ? TOP_fmpy_d : TOP_fmpy_s;

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frsqrta);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_exp);
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
  TOP fnma = is_double ? TOP_fnma_d : TOP_fnma_s;
  TOP fma  = is_double ? TOP_fma_d  : TOP_fma_s;
  TOP fmpy = is_double ? TOP_fmpy_d : TOP_fmpy_s;
  TOP fadd = is_double ? TOP_fadd_d : TOP_fadd_s;

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frsqrta);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_exp);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_exp);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fadd);
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
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy_s);
      }
      return num_multiplies;
    case INTRN_F8I4EXPEXPR: 
    case INTRN_F8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy_d);
      }
      return num_multiplies;
    case INTRN_C4I4EXPEXPR: 
    case INTRN_C4I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy_s);
      }
      return 4*num_multiplies;
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fma_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fmpy_d);
      }
      return 4*num_multiplies;
  }
  return 0.0;
}

double
LNOTARGET_Complex_Add_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fadd = (mtype == MTYPE_C4 ? TOP_fadd_s : TOP_fadd_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  return 2.0;
}

double
LNOTARGET_Complex_Mult_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fmpy = (mtype == MTYPE_C4 ? TOP_fmpy_s : TOP_fmpy_d);
  TOP fma  = (mtype == MTYPE_C4 ? TOP_fma_s  : TOP_fma_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  return 4.0;
}

double
LNOTARGET_Complex_Neg_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fneg);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fneg);
  return 2.0;
}

double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_ne);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  return 3.0;
}

double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_extr);
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
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_lt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
  return 1.0;
}

double
LNOTARGET_Int_Bnot_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor_i);
  return 1.0;
}

double
LNOTARGET_Int_Lnot_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor_i);
  return 1.0;
}

double
LNOTARGET_Int_Mult_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xma_l);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);  
  return 4.0;
}

double
LNOTARGET_Int_Add_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add);  
  return 1.0;
}

double
LNOTARGET_Int_Sub_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);  
  return 1.0;
}

double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TOP fnma = eight_bytes ? TOP_fnma : TOP_fnma_d;
  TOP fma  = eight_bytes ? TOP_fma  : TOP_fma_d;
  TOP fmpy = eight_bytes ? TOP_fmpy : TOP_fmpy_d;

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frcpa);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  if (eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
    TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx_trunc);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);  
  return (eight_bytes ? 18.0 : 16.0);
}

double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TOP fnma = eight_bytes ? TOP_fnma   : TOP_fnma_d;
  TOP fma  = eight_bytes ? TOP_fma    : TOP_fma_d;
  TOP fmpy = eight_bytes ? TOP_fmpy   : TOP_fmpy_d;
  TOP cmp  = eight_bytes ? TOP_cmp_lt : TOP_cmp4_lt;

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frcpa);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  if (eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
    TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx_trunc);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, cmp);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);  
  return (eight_bytes ? 25.0 : 23.0);
}

double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TOP fnma = eight_bytes ? TOP_fnma : TOP_fnma_d;
  TOP fma  = eight_bytes ? TOP_fma  : TOP_fma_d;
  TOP fmpy = eight_bytes ? TOP_fmpy : TOP_fmpy_d;

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frcpa);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  if (eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
    TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx_trunc);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);  
  return (eight_bytes ? 21.0 : 19.0);
}

double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TOP fnma = eight_bytes ? TOP_fnma : TOP_fnma_d;
  TOP fma  = eight_bytes ? TOP_fma  : TOP_fma_d;
  TOP fmpy = eight_bytes ? TOP_fmpy : TOP_fmpy_d;

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_setf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_frcpa);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  if (eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
    TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx_trunc);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_xf);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, fnma);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_fcvt_fx);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_getf_sig);  
  return (eight_bytes ? 22.0 : 20.0);
}

double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, BOOL minmax)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_lt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  if (minmax) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
    return 5.0;
  }
  else {
    return 3.0;
  }
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
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);  
  return 2.0;
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
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shl);
  if (!eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sxt4);
    return 2.0;
  }
  else {
    return 1.0;
  }
}

double
LNOTARGET_Int_Ashr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shr);
  if (!eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sxt4);
    return 2.0;
  }
  else {
    return 1.0;
  }
}

double
LNOTARGET_Int_Lshr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_shr_u);
  if (!eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sxt4);
    return 2.0;
  }
  else {
    return 1.0;
  }
}

double
LNOTARGET_Int_Eq_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_eq);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  return 3.0;
}

double
LNOTARGET_Int_Ne_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_ne);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  return 3.0;
}

double
LNOTARGET_Int_Gt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_gt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  return 3.0;
}

double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_ge);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  return 3.0;
}

double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_lt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  return 3.0;
}

double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cmp_le);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_i);
  return 3.0;
}

double
LNOTARGET_Int_Lda_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add);
  return 2.0;
}


INT
LNOTARGET_Cvt_Lat (OPCODE opcode)
{
  switch(opcode) {
    case OPC_F4I4CVT: 
    case OPC_F4I8CVT: 
      return LNOTARGET_Top_Latency(TOP_setf_sig)
           + LNOTARGET_Top_Latency(TOP_fcvt_xf)
           + LNOTARGET_Top_Latency(TOP_fnorm_s);
    case OPC_F8I4CVT: 
    case OPC_F8I8CVT: 
      return LNOTARGET_Top_Latency(TOP_setf_sig)
           + LNOTARGET_Top_Latency(TOP_fcvt_xf)
           + LNOTARGET_Top_Latency(TOP_fnorm_d);
    case OPC_F4U4CVT: 
    case OPC_F4U8CVT: 
      return LNOTARGET_Top_Latency(TOP_setf_sig)
           + LNOTARGET_Top_Latency(TOP_fnorm_s);
    case OPC_F8U4CVT: 
    case OPC_F8U8CVT: 
      return LNOTARGET_Top_Latency(TOP_setf_sig)
           + LNOTARGET_Top_Latency(TOP_fnorm_d);
    case OPC_I4F4CVT: 
    case OPC_I4F8CVT: 
    case OPC_I8F4CVT: 
    case OPC_I8F8CVT: 
      return LNOTARGET_Top_Latency(TOP_fcvt_fx)
           + LNOTARGET_Top_Latency(TOP_getf_sig);
    case OPC_U4F4CVT: 
    case OPC_U4F8CVT: 
    case OPC_U8F4CVT: 
    case OPC_U8F8CVT: 
      return LNOTARGET_Top_Latency(TOP_fcvt_fxu)
           + LNOTARGET_Top_Latency(TOP_getf_sig);
    case OPC_I4F4TRUNC: 
    case OPC_I8F4TRUNC: 
    case OPC_I4F8TRUNC: 
    case OPC_I8F8TRUNC: 
      return LNOTARGET_Top_Latency(TOP_fcvt_fx_trunc)
           + LNOTARGET_Top_Latency(TOP_getf_sig);
    case OPC_U4F4TRUNC:
    case OPC_U8F4TRUNC:
    case OPC_U4F8TRUNC:
    case OPC_U8F8TRUNC:
      return LNOTARGET_Top_Latency(TOP_fcvt_fxu_trunc)
           + LNOTARGET_Top_Latency(TOP_getf_sig);
    case OPC_I4F4RND: 
    case OPC_I8F4RND: 
    case OPC_I4F8RND: 
    case OPC_I8F8RND: 
    case OPC_I4F4CEIL: 
    case OPC_I8F4CEIL: 
    case OPC_I4F8CEIL: 
    case OPC_I8F8CEIL: 
    case OPC_I4F4FLOOR: 
    case OPC_I8F4FLOOR: 
    case OPC_I4F8FLOOR: 
    case OPC_I8F8FLOOR: 
      return LNOTARGET_Top_Latency(TOP_fcvt_fx)
           + LNOTARGET_Top_Latency(TOP_fsetc)
           + LNOTARGET_Top_Latency(TOP_getf_sig);
    case OPC_U4F4RND:
    case OPC_U8F4RND:
    case OPC_U4F8RND:
    case OPC_U8F8RND:
    case OPC_U4F4CEIL:
    case OPC_U8F4CEIL:
    case OPC_U4F8CEIL:
    case OPC_U8F8CEIL:
    case OPC_U4F4FLOOR:
    case OPC_U8F4FLOOR:
    case OPC_U4F8FLOOR:
    case OPC_U8F8FLOOR:
      return LNOTARGET_Top_Latency(TOP_fcvt_fxu)
           + LNOTARGET_Top_Latency(TOP_fsetc)
           + LNOTARGET_Top_Latency(TOP_getf_sig);
    case OPC_F8F4CVT:
      return LNOTARGET_Top_Latency(TOP_fnorm_d);
    case OPC_F4F8CVT:
      return LNOTARGET_Top_Latency(TOP_fnorm_s);
    // These cvts should not return -1, return 0 can catch more opportunities.
    // This will influence LNO model to perform different optimizations.
    case OPC_U8I4CVT:
    case OPC_U8U4CVT:
    case OPC_I8I4CVT:
    case OPC_I8U4CVT:
      return LNOTARGET_Top_Latency(TOP_nop);// just follow the convention, can be return 0  
  }
  return -1;
}

INT
LNOTARGET_FP_Madd_Add_Lat (TYPE_ID mtype)
{
  TOP fma = (mtype == MTYPE_F4 ? TOP_fma_s : TOP_fma_d);
  return TI_LATENCY_Result_Available_Cycle(fma, 0) 
       - TI_LATENCY_Operand_Access_Cycle(fma, 0);
}

INT
LNOTARGET_FP_Madd_Mult_Lat (TYPE_ID mtype)
{
  TOP fma = (mtype == MTYPE_F4 ? TOP_fma_s : TOP_fma_d);
  return TI_LATENCY_Result_Available_Cycle(fma, 0) 
       - TI_LATENCY_Operand_Access_Cycle(fma, 1);
}

INT
LNOTARGET_FP_Min_Max_Lat (TYPE_ID mtype)
{
  return LNOTARGET_Top_Latency(TOP_fcmp_lt)
       + LNOTARGET_Top_Latency(TOP_mov_f);
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
      return num_multiplies * LNOTARGET_Top_Latency(TOP_fmpy_s);
    case INTRN_F8I4EXPEXPR: 
    case INTRN_F8I8EXPEXPR:
      return num_multiplies * LNOTARGET_Top_Latency(TOP_fmpy_d);
    case INTRN_C4I4EXPEXPR: 
    case INTRN_C4I8EXPEXPR:
      return num_multiplies * (2*LNOTARGET_Top_Latency(TOP_fmpy_s) + 
                               2*LNOTARGET_Top_Latency(TOP_fadd_s));
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR:
      return num_multiplies * (2*LNOTARGET_Top_Latency(TOP_fmpy_d) + 
                               2*LNOTARGET_Top_Latency(TOP_fadd_d));
  }
  return -1;
}

INT
LNOTARGET_Complex_Add_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_C4 ? TOP_fadd_s : TOP_fadd_d);
  return 2*LNOTARGET_Top_Latency(add);
}

INT
LNOTARGET_Complex_Mult_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_C4 ? TOP_fadd_s : TOP_fadd_d);
  TOP mul = (mtype == MTYPE_C4 ? TOP_fmpy_s : TOP_fmpy_d);
  return 2*LNOTARGET_Top_Latency(add) 
       + 2*LNOTARGET_Top_Latency(mul);
}

INT
LNOTARGET_Complex_Neg_Lat (TYPE_ID mtype)
{
  return 2*LNOTARGET_Top_Latency(TOP_fneg);
}
