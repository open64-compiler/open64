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
      return TOP_add_s;
    case OPC_F8ADD:
      return TOP_add_d;
    case OPC_F4SUB:
      return TOP_sub_s;
    case OPC_F8SUB:
      return TOP_sub_d;
    case OPC_F4MPY:
      return TOP_mul_s;
    case OPC_F8MPY:
      return TOP_mul_d;
   }
  return WHIRL_To_TOP(wn);
}

void
LNOTARGET_Loop_Inc_Test_Res (TI_RES_COUNT* resource_count)
{
}

double
LNOTARGET_Cvt_Res (TI_RES_COUNT* resource_count, OPCODE opcode)
{
  switch(opcode) {
    case OPC_F4I4CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mtc1);		
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_s_w);
      return 2.0;
    case OPC_F4I8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1);			
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_s_l);
      return 2.0;
    case OPC_F8I4CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mtc1);		
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_w);
      return 2.0;	
    case OPC_F8I8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1);			
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_l);
      return 2.0;
    case OPC_F4U4CVT:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsll32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsrl32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1);			
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_s_l); 
      return 4.0;
      /* Attention please, since the speciality of MIPS ISA,the expansion of F4U4CVT,F8U4CVT used 
	 shift instruction.
	 --George Her  */
    case OPC_F4U8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_s_l);
	  return 2.0;
    case OPC_F8U4CVT:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsll32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsrl32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1);			
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_l);
      return 4.0;  	
    case OPC_F8U8CVT: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_l);
      return 2.0;
    case OPC_I4F4TRUNC:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_w_s);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfc1);
      return 2.0;
    case OPC_I8F4TRUNC:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_l_s);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmfc1);
      return 2.0;		
    case OPC_I4F8TRUNC: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_w_d);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfc1);
      return 2.0;		
    case OPC_I8F8TRUNC: 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_l_d);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmfc1);
      return 2.0;	
    case OPC_U4F4TRUNC:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_l_s);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmfc1);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sll);		
      return 3.0;	
      /* Attention please, since the speciality of MIPS ISA,the expansion of U4F4TRUNC used 
	 shift instruction after conversion to keep value consistency.
	 --George Her  */		
    case OPC_U8F4TRUNC:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_lui);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mtc1);	
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_c_le_s);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_bc1f);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub_s);	
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_l_s);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmfc1);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ori); 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsll32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_j); 	
      return 11.0;
      /* Due to  MIPS ISA, there must be branch while dealing with U8F4TRUNC, U8F8TRUNC nodes, we choose 
         the longer path of the branch to calculate required info.
         -- George Her   */	
    case OPC_U4F8TRUNC:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_l_d);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmfc1);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sll);		
      return 3.0;			
    case OPC_U8F8TRUNC:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ori);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsll32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1); 	
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_c_le_d);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_bc1f);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub_d);	
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_trunc_l_d);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmfc1);   		
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ori); 
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsll32);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_j); 	
      return 12.0;
    case OPC_F8F4CVT:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_s);
      return 1.0;
    case OPC_F4F8CVT:
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_s_d);
      return 1.0;
  }
  return 0.0;
}

double
LNOTARGET_FP_Madd_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fma = (mtype == MTYPE_F4 ? TOP_madd_s: TOP_madd_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fma);
  return 1.0;
}

double
LNOTARGET_FP_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  ( MTYPE_is_size_double(mtype) )?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_c_lt_d)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_c_lt_s);	
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_bc1t);
  ( MTYPE_is_size_double(mtype) )?
	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_d)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mov_s);		
  return 3.0;
}

double
LNOTARGET_FP_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_div_d);
    return 1.0;
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_div_s);
    return 1.0;
  }
}

double
LNOTARGET_FP_Recip_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ori); 
  
  if (mtype == MTYPE_F8) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1);			
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_l);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_div_d);
   }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mtc1);		
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_s_w);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_div_s);
  }
  return 4.0;	
}

double
LNOTARGET_FP_Rsqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  BOOL is_double = (MTYPE_is_size_double(mtype) != 0);
  TOP sqrt_op = is_double ? TOP_sqrt_d: TOP_sqrt_s;
  TOP division_op = is_double ? TOP_div_d: TOP_div_s;	

  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_ori); 
  
  if (mtype == MTYPE_F8) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmtc1);			
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_d_l);
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mtc1);		
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_cvt_s_w);
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, sqrt_op);
  TI_RES_COUNT_Add_Op_Resources(resource_count, division_op);
  return  5.0;
}


double
LNOTARGET_FP_Sqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  BOOL is_double = (MTYPE_is_size_double(mtype) != 0);
  TOP sqrt_op = is_double ? TOP_sqrt_d: TOP_sqrt_s;
  TI_RES_COUNT_Add_Op_Resources(resource_count, sqrt_op);
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
      	TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_s);
      }
      return 6*num_multiplies;
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR: 
      for (i = 0; i < num_multiplies; i++) {
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_d);
        TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mul_d);
      }
      return 6*num_multiplies;
      /* number "6" is the number of intructions per iteration -- George Her */		
  }
  return 0.0;
}

double
LNOTARGET_Complex_Add_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fadd = (mtype == MTYPE_C4 ? TOP_add_s : TOP_add_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  return 2.0;
}

double
LNOTARGET_Complex_Mult_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TOP fmpy = (mtype == MTYPE_C4 ? TOP_mul_s : TOP_mul_d);
  TOP fadd  = (mtype == MTYPE_C4 ? TOP_add_s  : TOP_add_d);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fmpy);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  TI_RES_COUNT_Add_Op_Resources(resource_count, fadd);
  return 6.0;
}

double
LNOTARGET_Complex_Neg_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_neg_s);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_neg_s);
  return 2.0;
}

double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  eight_bytes?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsub)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
	
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_nor);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
	
  eight_bytes?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_daddu)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_addu);
  return 5.0;
}

double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count, TYPE_ID from, TYPE_ID to)
{
  if (MTYPE_bit_size(from)  ==  MTYPE_bit_size(to) )  {
    if (to == MTYPE_U8 || to == MTYPE_I8) {    	 
      Is_True(FALSE, ("64-bit cvt to 64-bit?\n"));
      return 0.0;
    }
    else {
      // No need to do U4I4CVT and I4U4CVT
      TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
      return 1.0;
    }
  }
  else {
    (MTYPE_is_size_double (to) )?
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsll32)
    : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sll);
    (MTYPE_is_size_double (to) )?
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsra32)
    : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sra);	
    return 2.0;	
  }		
}
	
double
LNOTARGET_Int_Neg_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  eight_bytes?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsubu)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subu);
  return 1.0;
}

double
LNOTARGET_Int_Abs_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes, TYPE_ID mtype)
{
  if (MTYPE_is_unsigned(mtype)) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or); 
    return 1.0;		
  }
  if (eight_bytes) { 	
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsra32);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsub);
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sra);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);		
  }		
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
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sltu);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xori);
  return 2.0;
}

double
LNOTARGET_Int_Mult_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  eight_bytes?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dmult)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mult);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mflo);
  return 2.0;
}

double
LNOTARGET_Int_Add_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  eight_bytes?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dadd)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add);
  return 1.0;
}

double
LNOTARGET_Int_Sub_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  eight_bytes?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsubu)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_subu);
  return 1.0;
}

double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{

  TOP division_op = eight_bytes? TOP_ddiv: TOP_div;

  TI_RES_COUNT_Add_Op_Resources(resource_count, division_op);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mflo);  
  return 3.0;
}

double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TOP division_op = eight_bytes? TOP_ddiv: TOP_div;

  TI_RES_COUNT_Add_Op_Resources(resource_count, division_op);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfhi);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_addu);  

	return 5.0;	
}

double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TOP division_op = eight_bytes? TOP_ddiv: TOP_div;

  TI_RES_COUNT_Add_Op_Resources(resource_count, division_op);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfhi);  
  return 3.0;
}

double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TOP division_op = eight_bytes? TOP_ddiv: TOP_div;

  TI_RES_COUNT_Add_Op_Resources(resource_count, division_op);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_teq);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mflo); 
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_mfhi);  	
  return 4.0;
}

double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, BOOL minmax, BOOL eight_bytes)
{
  eight_bytes?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsub)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
	
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_nor);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);	

  eight_bytes?
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dadd)
  : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add); 		
  if (minmax) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
    eight_bytes?
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dadd)
    : TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add); 		
    return 8.0;
  }
  else {
    return 5.0;
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
  if (!eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sll);
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsll);
  }	
  return 1.0;	
}

double
LNOTARGET_Int_Ashr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  if (!eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sra);
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsra);
  }	
  return 1.0;	
 }

double
LNOTARGET_Int_Lshr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  if (!eight_bytes) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_srl);
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_dsrl);
  }	
  return 1.0;	
}

double
LNOTARGET_Int_Eq_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sltiu);
  return 2.0;
}

double
LNOTARGET_Int_Ne_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sltu);
  return 2.0;
}

double
LNOTARGET_Int_Gt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sltu);
  return 2.0;
}

double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xori);
  return 2.0;
}

double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
   return 1.0;
}

double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_slt);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xori);
  return 2.0;
}

double
LNOTARGET_Int_Lda_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_daddiu);
  return 1.0;
}


INT
LNOTARGET_Cvt_Lat (OPCODE opcode)
{
  switch(opcode) {
     case OPC_F4I4CVT: 
      return LNOTARGET_Top_Latency(TOP_mtc1)
           + LNOTARGET_Top_Latency(TOP_cvt_s_w);	
    case OPC_F4I8CVT: 
      return LNOTARGET_Top_Latency(TOP_dmtc1)
           + LNOTARGET_Top_Latency(TOP_cvt_s_l); 
    case OPC_F8I4CVT:
      return LNOTARGET_Top_Latency(TOP_mtc1)
           + LNOTARGET_Top_Latency(TOP_cvt_d_w);	
    case OPC_F8I8CVT: 
      return LNOTARGET_Top_Latency(TOP_dmtc1)
           + LNOTARGET_Top_Latency(TOP_cvt_d_l); 
   	case OPC_F4U4CVT: 
      return LNOTARGET_Top_Latency(TOP_dsll32)
           + LNOTARGET_Top_Latency(TOP_dsrl32)
           + LNOTARGET_Top_Latency(TOP_dmtc1)
           + LNOTARGET_Top_Latency(TOP_cvt_s_l);		
    case OPC_F4U8CVT: 
      return LNOTARGET_Top_Latency(TOP_dmtc1)
           + LNOTARGET_Top_Latency(TOP_cvt_s_l);
    case OPC_F8U4CVT: 
      return LNOTARGET_Top_Latency(TOP_dsll32)
           + LNOTARGET_Top_Latency(TOP_dsrl32)
           + LNOTARGET_Top_Latency(TOP_dmtc1)
           + LNOTARGET_Top_Latency(TOP_cvt_d_l);	
    case OPC_F8U8CVT: 
      return LNOTARGET_Top_Latency(TOP_dmtc1)
           + LNOTARGET_Top_Latency(TOP_cvt_d_l);
    case OPC_I4F4TRUNC: 
      return LNOTARGET_Top_Latency(TOP_trunc_w_s)
      	   + LNOTARGET_Top_Latency(TOP_mfc1);
    case OPC_I8F4TRUNC:
      return LNOTARGET_Top_Latency(TOP_trunc_l_s)
      	   + LNOTARGET_Top_Latency(TOP_dmfc1);		
    case OPC_I4F8TRUNC:
      return LNOTARGET_Top_Latency(TOP_trunc_w_d)
      	   + LNOTARGET_Top_Latency(TOP_mfc1);		
    case OPC_I8F8TRUNC: 
      return LNOTARGET_Top_Latency(TOP_trunc_l_d)
           + LNOTARGET_Top_Latency(TOP_dmfc1);
    case OPC_U4F4TRUNC:
      return LNOTARGET_Top_Latency(TOP_trunc_l_s)
      	   + LNOTARGET_Top_Latency(TOP_dmfc1)
      	   + LNOTARGET_Top_Latency(TOP_sll);      	   	
    case OPC_U8F4TRUNC:
      return LNOTARGET_Top_Latency(TOP_lui)
      	   + LNOTARGET_Top_Latency(TOP_mtc1)
      	   + LNOTARGET_Top_Latency(TOP_c_le_s)      	   
      	   + LNOTARGET_Top_Latency(TOP_bc1f)
      	   + LNOTARGET_Top_Latency(TOP_sub_s)
      	   + LNOTARGET_Top_Latency(TOP_trunc_l_s)	
      	   + LNOTARGET_Top_Latency(TOP_dmfc1)
      	   + LNOTARGET_Top_Latency(TOP_ori)
      	   + LNOTARGET_Top_Latency(TOP_dsll32)
      	   + LNOTARGET_Top_Latency(TOP_or)
    	   + LNOTARGET_Top_Latency(TOP_j);  
    case OPC_U4F8TRUNC:
      return LNOTARGET_Top_Latency(TOP_trunc_l_d)
      	   + LNOTARGET_Top_Latency(TOP_dmfc1)
      	   + LNOTARGET_Top_Latency(TOP_sll);      
    case OPC_U8F8TRUNC:
      return LNOTARGET_Top_Latency(TOP_ori)
      	   + LNOTARGET_Top_Latency(TOP_dsll32)
      	   + LNOTARGET_Top_Latency(TOP_dmtc1)      	   
      	   + LNOTARGET_Top_Latency(TOP_c_le_d)
      	   + LNOTARGET_Top_Latency(TOP_bc1f)
      	   + LNOTARGET_Top_Latency(TOP_sub_d)	
      	   + LNOTARGET_Top_Latency(TOP_trunc_l_d)
      	   + LNOTARGET_Top_Latency(TOP_dmfc1)
      	   + LNOTARGET_Top_Latency(TOP_ori)
      	   + LNOTARGET_Top_Latency(TOP_dsll32)
    	   + LNOTARGET_Top_Latency(TOP_or)
    	   + LNOTARGET_Top_Latency(TOP_j); 
    case OPC_F8F4CVT:
      return LNOTARGET_Top_Latency(TOP_cvt_d_s);
    case OPC_F4F8CVT:
      return LNOTARGET_Top_Latency(TOP_cvt_s_d);
  }
  return -1;
}

INT
LNOTARGET_FP_Madd_Add_Lat (TYPE_ID mtype)
{
  Is_True(FALSE, ("Unexpected call for LNOTARGET_FP_Madd_Add_Lat!"));
}

INT
LNOTARGET_FP_Madd_Mult_Lat (TYPE_ID mtype)
{
  Is_True(FALSE, ("Unexpected call for LNOTARGET_FP_Madd_Add_Lat!"));
}

INT
LNOTARGET_FP_Min_Max_Lat (TYPE_ID mtype)
{
  if ( MTYPE_is_size_double(mtype) ) {
    return LNOTARGET_Top_Latency(TOP_c_lt_d)
	  + LNOTARGET_Top_Latency(TOP_bc1t)
          + LNOTARGET_Top_Latency(TOP_mov_d);
  }
  else {
    return LNOTARGET_Top_Latency(TOP_c_lt_s)
	  + LNOTARGET_Top_Latency(TOP_bc1t)
          + LNOTARGET_Top_Latency(TOP_mov_s);	
  }
}

INT
LNOTARGET_FP_Div_Lat (TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    return LNOTARGET_Top_Latency(TOP_div_d);
  }
  else {
    return LNOTARGET_Top_Latency(TOP_div_s);
  }
}

INT
LNOTARGET_FP_Recip_Lat (TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    return LNOTARGET_Top_Latency(TOP_ori)
	  + LNOTARGET_Top_Latency(TOP_dmtc1)
	  + LNOTARGET_Top_Latency(TOP_cvt_d_l)
          + LNOTARGET_Top_Latency(TOP_div_d);	
  }
  else {
    return LNOTARGET_Top_Latency(TOP_ori)
	  + LNOTARGET_Top_Latency(TOP_mtc1)
	  + LNOTARGET_Top_Latency(TOP_cvt_s_w)
          + LNOTARGET_Top_Latency(TOP_div_s);		
  }
}

INT
LNOTARGET_FP_Rsqrt_Lat (TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    return LNOTARGET_Top_Latency(TOP_ori)
	  + LNOTARGET_Top_Latency(TOP_dmtc1)
	  + LNOTARGET_Top_Latency(TOP_cvt_d_l)
	  + LNOTARGET_Top_Latency(TOP_sqrt_d)
          + LNOTARGET_Top_Latency(TOP_div_d);	
  }
  else {
   return LNOTARGET_Top_Latency(TOP_ori)
	  + LNOTARGET_Top_Latency(TOP_mtc1)
	  + LNOTARGET_Top_Latency(TOP_cvt_s_w)
	  + LNOTARGET_Top_Latency(TOP_sqrt_s)
          + LNOTARGET_Top_Latency(TOP_div_s);		
  }
}

INT
LNOTARGET_FP_Sqrt_Lat (TYPE_ID mtype)
{
  if (mtype == MTYPE_F8) {
    return LNOTARGET_Top_Latency(TOP_sqrt_d);
  }
  else {
    return LNOTARGET_Top_Latency(TOP_sqrt_s);
  }
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
                               2*LNOTARGET_Top_Latency(TOP_add_s));
    case INTRN_C8I4EXPEXPR: 
    case INTRN_C8I8EXPEXPR:
      return num_multiplies * (2*LNOTARGET_Top_Latency(TOP_mul_d) + 
                               2*LNOTARGET_Top_Latency(TOP_add_d));
  }
  return -1;
}

INT
LNOTARGET_Complex_Add_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_C4 ? TOP_add_s : TOP_add_d);
  return 2*LNOTARGET_Top_Latency(add);
}

INT
LNOTARGET_Complex_Mult_Lat (TYPE_ID mtype)
{
  TOP add = (mtype == MTYPE_C4 ? TOP_add_s : TOP_add_d);
  TOP mul = (mtype == MTYPE_C4 ? TOP_mul_s : TOP_mul_d);
  return 2*LNOTARGET_Top_Latency(add) 
       + 2*LNOTARGET_Top_Latency(mul);
}

INT
LNOTARGET_Complex_Neg_Lat (TYPE_ID mtype)
{
  TOP neg = (mtype == MTYPE_C4 ? TOP_neg_s : TOP_neg_d);
  return 2*LNOTARGET_Top_Latency(neg);
}
