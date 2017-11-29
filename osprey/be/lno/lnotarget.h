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


#include "wn.h"
#include "mtypes.h"
#include "cxx_memory.h"
#include "ti_latency.h"
#include "ti_res_count.h"

extern TOP
LNOTARGET_Whirl_To_Top (WN* wn);

extern void 
LNOTARGET_Loop_Inc_Test_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Cvt_Res (TI_RES_COUNT* resource_count, OPCODE opcode);

extern double
LNOTARGET_FP_Madd_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

extern double
LNOTARGET_FP_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

extern double
LNOTARGET_FP_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

extern double
LNOTARGET_FP_Recip_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

extern double
LNOTARGET_FP_Rsqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

extern double
LNOTARGET_FP_Sqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

extern double
LNOTARGET_FP_Exp_Res (TI_RES_COUNT* resource_count, 
                      INTRINSIC intr,
                      INT num_multiplies);

#ifdef TARG_X8664
extern double
LNOTARGET_FP_Floor_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);
#endif

extern double
LNOTARGET_Complex_Add_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

extern double
LNOTARGET_Complex_Mult_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

extern double
LNOTARGET_Complex_Neg_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype);

#ifdef TARG_X8664
extern double
LNOTARGET_Fp_Compare_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Fp_Select_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype, INT length);
#elif defined(TARG_LOONGSON)
extern double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

extern double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count, TYPE_ID from, TYPE_ID to);
#else
extern double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count);
#endif /* TARG_X8664 */

extern double
LNOTARGET_Int_Neg_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

#ifdef TARG_LOONGSON
extern double
LNOTARGET_Int_Abs_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes, TYPE_ID mtype);
#else
extern double
LNOTARGET_Int_Abs_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);
#endif

#ifdef TARG_X8664
extern double
LNOTARGET_Int_Bnot_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Lnot_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);
#else
extern double
LNOTARGET_Int_Bnot_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Lnot_Res (TI_RES_COUNT* resource_count);
#endif /* TARG_X8664 */

extern double
LNOTARGET_Int_Mult_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

#ifdef TARG_X8664
extern double
LNOTARGET_Int_Mult_Str_Red_Res (TI_RES_COUNT* resource_count, 
				TYPE_ID rtype, INT64 operand);
#endif /* TARG_X8664 */

extern double
LNOTARGET_Int_Add_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

extern double
LNOTARGET_Int_Sub_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

#ifdef TARG_X8664
extern double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Div_Str_Red_Res (TI_RES_COUNT* resource_count, 
			       TYPE_ID rtype, INT64 denom_val);

extern double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Mod_Str_Red_Res (TI_RES_COUNT* resource_count, 
			       TYPE_ID rtype, INT64 denom_val);

extern double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Rem_Str_Red_Res (TI_RES_COUNT* resource_count, 
			       TYPE_ID rtype, INT64 denom_val);

extern double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_DivRem_Str_Red_Res (TI_RES_COUNT* resource_count, 
			       TYPE_ID rtype, INT64 denom_val);

extern double
LNOTARGET_Int_Min_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Band_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Bior_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Bnor_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Bxor_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Band_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Band_Str_Red_Res (TI_RES_COUNT* resource_count, 
				TYPE_ID rtype, INT64 val);

extern double
LNOTARGET_Int_Bior_Str_Red_Res (TI_RES_COUNT* resource_count, 
				TYPE_ID rtype, INT64 val);

extern double
LNOTARGET_Int_Bxor_Str_Red_Res (TI_RES_COUNT* resource_count, 
				TYPE_ID rtype, INT64 val);
extern double
LNOTARGET_Int_Land_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Cand_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Lior_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);

extern double
LNOTARGET_Int_Cior_Res (TI_RES_COUNT* resource_count, TYPE_ID rtype);
#else
#if !(defined(TARG_MIPS) || defined(TARG_PPC32))
extern double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);
#else
extern double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, 
		       BOOL eight_bytes, 
		       BOOL mtype_signed);
#endif

extern double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

extern double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

extern double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

#ifdef TARG_LOONGSON
extern double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, BOOL minmax, BOOL eight_byte);
#else
extern double 
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, BOOL minmax);
#endif //TARG_LOONGSON

extern double
LNOTARGET_Int_Band_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Bior_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Bnor_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Bxor_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Land_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Cand_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Lior_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Cior_Res (TI_RES_COUNT* resource_count);

#endif /* TARG_X8664 */
extern double
LNOTARGET_Int_Shl_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

extern double
LNOTARGET_Int_Ashr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

extern double
LNOTARGET_Int_Lshr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes);

#ifdef TARG_X8664
extern double
LNOTARGET_Int_Eq_Res (TI_RES_COUNT* resource_count, TYPE_ID desc);

extern double
LNOTARGET_Int_Ne_Res (TI_RES_COUNT* resource_count, TYPE_ID desc);

extern double
LNOTARGET_Int_Gt_Res (TI_RES_COUNT* resource_count, TYPE_ID desc);

extern double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count, TYPE_ID desc);

extern double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count, TYPE_ID desc);

extern double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count, TYPE_ID desc);
#else
extern double
LNOTARGET_Int_Eq_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Ne_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Gt_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count);

extern double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count);
#endif /* TARG_X8664 */

extern double
LNOTARGET_Int_Lda_Res (TI_RES_COUNT* resource_count);


extern INT
LNOTARGET_Cvt_Lat (OPCODE opcode);

extern INT
LNOTARGET_FP_Madd_Add_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Madd_Mult_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Min_Max_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Div_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Recip_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Rsqrt_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Sqrt_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Exp_Lat (INTRINSIC intr, INT num_multiplies);

#ifdef TARG_X8664
extern INT
LNOTARGET_FP_Floor_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Select_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_FP_Compare_Lat (TYPE_ID mtype);
#endif

extern INT
LNOTARGET_Complex_Add_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_Complex_Mult_Lat (TYPE_ID mtype);

extern INT
LNOTARGET_Complex_Neg_Lat (TYPE_ID mtype);


// what is the latency of a single TOP
inline INT 
LNOTARGET_Top_Latency(TOP top) 
{
  return (TI_LATENCY_Result_Available_Cycle(top, 0) - 
          TI_LATENCY_Operand_Access_Cycle(top, 0));
}

