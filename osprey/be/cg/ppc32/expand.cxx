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
#include "const.h"  /* needed to manipulate target/host consts */
#include "targ_const.h" /* needed to manipulate target/host consts */
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
// #include "exp_intrn_info.h"
#include "olive_convert_wn.h"

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
#define DUP_TN(tn)  Dup_TN_Even_If_Dedicated(tn)


// code skentch from IA32 to handle 64bit OPs
static TN_MAP _TN_Pair_table = NULL;

void Expand_Start()
{
  FmtAssert(_TN_Pair_table == NULL, ("TN_Pair_table is not NULL"));
  _TN_Pair_table = TN_MAP_Create();
}


void Expand_Finish()
{
  FmtAssert(_TN_Pair_table != NULL, ("TN_Pair_table is NULL"));
  TN_MAP_Delete(_TN_Pair_table);
  _TN_Pair_table = NULL;
}


// Always use the lower part as key.
TN* Get_TN_Pair(TN* key)
{
  TN* pair = NULL;

  if (Is_Target_32bit() && TN_is_register(key)){
    pair = (TN*)TN_MAP_Get(_TN_Pair_table, key);
  }

  return pair;
}

void Create_TN_Pair(TN* key, TN* pair)
{
  Is_True(Get_TN_Pair(key) == NULL, ("Add_TN_Pair: NYI"));
  TN_MAP_Set(_TN_Pair_table, key, pair);    
}

void Add_TN_Pair(TN* key, TN* pair)
{
  Create_TN_Pair(key, pair);
}

TN * Reset_TN_Pair(TN * key)
{
  TN * pair = Get_TN_Pair(key);
  TN_MAP_Set(_TN_Pair_table, key, NULL);
  return pair;
}

/* Always use the lower part as key.
   Notice that literal TNs will not have pairs.
 */
TN* Create_TN_Pair(TN* key, TYPE_ID mtype)
{
  FmtAssert(TN_is_register(key), ("NYI"));

  if (mtype == MTYPE_I8)
    mtype = MTYPE_I4;
  else if (mtype == MTYPE_U8)
    mtype = MTYPE_U4;

  TN* pair = Get_TN_Pair(key);

  if (pair == NULL){
    Set_TN_size(key, MTYPE_byte_size(mtype));
    /* We don't know what <pair> will be later. So don't use
       Dup_TN that will carry the homing info of <key>.
     */
    pair = Build_TN_Like(key);
    TN_MAP_Set(_TN_Pair_table, key, pair);
  }

  if (TN_register(key) != REGISTER_UNDEFINED){
    // Is_True(TN_register(pair) != REGISTER_UNDEFINED, ("pair TN is async"));
  }

  return pair;
}

TN * Get_64Bit_High_TN(TN * low, BOOL bsigned, OPS * ops)
{
  TN * high = Build_TN_Of_Mtype(bsigned ? MTYPE_I4 : MTYPE_U4);
  
  if (TN_is_register(low) && bsigned) {
    Build_OP(TOP_srawi, high, low, Gen_Literal_TN(31, 4), ops);
  }
  else {
    Build_OP(TOP_li, high, Gen_Literal_TN(0, 4), ops);
  }
  
  return high;
}

static TN * Setup_64Bit_High_TN(TN * low, BOOL bsigned, OPS * ops)
{
  TN * high = NULL;
  if (TN_is_register(low)) {
    high        = Get_64Bit_High_TN(low, bsigned, ops);
  }
  else {
    INT64 val   = TN_value(low);
    INT32 val_h = (bsigned && (val < 0)) ? 0xffffffff : 0;
    high        = Gen_Literal_TN(val_h, 4);
  }

  return high;
}

/*  <result> = <cmp_kid1> <compare> <cmp_kid2> ? <true_tn> : <false_tn>
 */
static void Expand_64Bit_Select(TN* result, OPERATOR compare, TOP cmp_opcode,
         TN* cmp_kid1, TN* cmp_kid2, TYPE_ID cmp_type,
         TN* true_tn, TN* false_tn, TYPE_ID select_type,
         OPS* ops)
{
  FmtAssert(FALSE, ("Expand_64Bit_Select NYI"));
}


static void  
Expand_64Bit_Int_Cmp(OPERATOR opr, TN* result, TN* src1_l, 
  TN* src2_l, TYPE_ID mtype, OPS* ops)
{
  FmtAssert((result != src1_l) && (result != src2_l), ("NYI"));

  TN* src1_h = Get_TN_Pair(src1_l);
  TN* src2_h = Get_TN_Pair(src2_l);
  TOP cmp;
  
  FmtAssert(src1_h, ("Expand_64Bit_Int_Cmp src1_h is NULL"));
    
  if (TN_has_value(src2_l)) {
    src2_l = Expand_Immediate_Into_Register(NULL, src2_l, mtype, ops);
    src2_h = Get_TN_Pair(src2_l);
  }
  else {
    if (!src2_h) {
      src2_h = Get_64Bit_High_TN(src2_l, MTYPE_is_signed(mtype), ops);
    }
  }
  
  cmp = MTYPE_is_signed(mtype) ? TOP_cmpw : TOP_cmplw;
  Handle_Cond_Longlong(opr, cmp, result, src1_h, src1_l, 
      src2_h, src2_l, ops);
}


static void Expand_64Bit_Cvtl(TN * result_l, TN * src_l, TN * len, 
  TYPE_ID mtype, TYPE_ID desc, OPS* ops)
{
  TN * result_h = Get_TN_Pair(result_l);
  TN * src_h    = Get_TN_Pair(src_l);
  BOOL signedex = MTYPE_is_signed(mtype);
  TYPE_ID mt  = (mtype == MTYPE_I8) ? MTYPE_I4 : MTYPE_U4;
  
  INT64 l = TN_value(len);
  if (l < 32) {
    if (signedex) {
      TN* tn = Build_TN_Like(result_l);
      Expand_Convert_Length(tn, src_l, len, mt, mt, ops);
      Build_OP(TOP_mr, result_l, tn, ops);
      Build_OP(TOP_srawi, result_h, tn, Gen_Literal_TN(31, 4), ops);
    } else {
      Expand_Convert_Length(result_l, src_l, len, mt, mt, ops);
      Build_OP(TOP_li, result_h, Gen_Literal_TN(0, 4), ops);
    }
  } else if (l == 32) {
    Expand_Copy(result_l, src_l, MTYPE_I4, ops);
    if (signedex) {
      Build_OP(TOP_srawi, result_h, src_l, Gen_Literal_TN(31, 4), ops);
    } else {
      Build_OP(TOP_li, result_h, Gen_Literal_TN(0, 4), ops);
    }
  } else {
      FmtAssert(src_h, ("src_h not setup"));
      Expand_Copy(result_l, src_l, MTYPE_I4, ops);
      Expand_Convert_Length(result_h, src_h, Gen_Literal_TN(l - 32, 4), mt, desc, ops);
      
  }
}

static BOOL Exp_Imm_Value(TN * dest, INT64 val, OPS * ops);

// Use two or three 32-bit operations to emulate a 64-bit
// unary operation.
void Expand_64Bit_Unary_OP(OPERATOR opr, TYPE_ID mtype,
           TN * result_l, TN * src_l,
           OPS* ops)
{
  TN * result_h = Get_TN_Pair(result_l);
  TN * src_h    = Get_TN_Pair(src_l);
  FmtAssert(result_h != NULL, ("Expand_64Bit_Unary_OP result tn pair not setup"));  
  
  switch (opr){
  case OPR_INTCONST:
  {   
    INT64 val = TN_value(src_l);  // treat as signed value
    int v_l   = (int)(val & 0xffffffff);
    int v_h   = (int)(val >> 32);    

    Exp_Imm_Value(result_l, v_l, ops);
    Exp_Imm_Value(result_h, v_h, ops);    
    break;
  }   
  case OPR_NEG:
    FmtAssert(src_h != NULL, ("Expand_64Bit_Unary_OP NEG: source tn pair not setup"));
    Build_OP(TOP_subfic, result_l, src_l, Gen_Literal_TN(0, 4), ops);
    Build_OP(TOP_subfze, result_h, src_h, ops);
    break;
  case OPR_BNOT:
    FmtAssert(src_h != NULL, ("Expand_64Bit_Unary_OP BNOT: source tn pair not setup"));
    Build_OP(TOP_nor, result_l, src_l, src_l, ops);
    Build_OP(TOP_nor, result_h, src_h, src_h, ops);
    break;
  
  default:
    FmtAssert(FALSE, ("Expand_64Bit_Unary_OP NYI"));
  }
}


/* Use two 32-bit binary operations to emulate a 64-bit
   binary operation.
*/
static void Expand_64Bit_Binary_OP(OPERATOR opr, TYPE_ID mtype,
            TN * result_l, TN * src1_l, TN * src2_l,
            OPS * ops)
{ 
  TN* result_h = Get_TN_Pair(result_l);
  TN* src1_h   = Get_TN_Pair(src1_l);
  TN* src2_h   = TN_has_value(src2_l) ? NULL : Get_TN_Pair(src2_l);

  if (TN_has_value(src1_l)) {
    src1_l = Expand_Immediate_Into_Register(NULL, src1_l, mtype, ops);
    src1_h = Get_TN_Pair(src1_l);
  }
  else if (src1_h == NULL){
    src1_h = Get_64Bit_High_TN(src1_l, MTYPE_signed(mtype), ops);
  }

  if (TN_has_value(src2_l)){
    src2_l  = Expand_Immediate_Into_Register(NULL, src2_l, mtype, ops);
    src2_h  = Get_TN_Pair(src2_l);
  }
  else if (src2_h == NULL) {
    src2_h = Get_64Bit_High_TN(src2_l, MTYPE_signed(mtype), ops);
  }  

  FmtAssert(result_h && src1_h && src2_h, ("Expand_64Bit_Binary_OP"));  

  switch (opr){
  case OPR_ADD:
    Build_OP(TOP_addc, result_l, src1_l, src2_l, ops);
    Build_OP(TOP_adde, result_h, src1_h, src2_h, ops);
    break;
  case OPR_SUB:
    Build_OP(TOP_subfc, result_l, src2_l, src1_l, ops);
    Build_OP(TOP_subfe, result_h, src2_h, src1_h, ops);
    break;
  case OPR_BAND:
    Build_OP(TOP_and, result_l, src1_l, src2_l, ops);
    Build_OP(TOP_and, result_h, src1_h, src2_h, ops);
    break;
  case OPR_BIOR:
    Build_OP(TOP_or, result_l, src1_l, src2_l, ops);
    Build_OP(TOP_or, result_h, src1_h, src2_h, ops);
    break;
  case OPR_BXOR:
    Build_OP(TOP_xor, result_l, src1_l, src2_l, ops);
    Build_OP(TOP_xor, result_h, src1_h, src2_h, ops);
    break;
  case OPR_BNOR:
    Build_OP(TOP_nor, result_l, src1_l, src2_l, ops);
    Build_OP(TOP_nor, result_h, src1_h, src2_h, ops);
    break;
  default:
    FmtAssert(FALSE, ("Expand_64Bit_Binary_OP NYI"));
  }   
}


static void Expand_64Bit_Multiply(TN* result_l, TN* src1_l, TN* src2_l, TYPE_ID mtype, OPS* ops)
{
  TN* result_h = Get_TN_Pair(result_l);
  TN* src1_h   = Get_TN_Pair(src1_l);
  TN* src2_h   = TN_has_value(src2_l) ? NULL : Get_TN_Pair(src2_l);

if (TN_has_value(src1_l)) {
    src1_l = Expand_Immediate_Into_Register(NULL, src1_l, mtype, ops);
    src1_h = Get_TN_Pair(src1_l);
  }
  else if (src1_h == NULL){
    src1_h = Get_64Bit_High_TN(src1_l, MTYPE_signed(mtype), ops);
  }

  if (TN_has_value(src2_l)){
    src2_l  = Expand_Immediate_Into_Register(NULL, src2_l, mtype, ops);
    src2_h  = Get_TN_Pair(src2_l);
  }
  else if (src2_h == NULL) {
    src2_h = Get_64Bit_High_TN(src2_l, MTYPE_signed(mtype), ops);
  }  


  TN* tn1 = Build_TN_Like(result_h);
  Build_OP(TOP_mullw,  tn1,  src1_h,    src2_l, ops);
  TN* tn2 = Build_TN_Like(result_l);
  Build_OP(TOP_mullw,  tn2,  src1_l,    src2_h, ops);
  TN* tn3 = Build_TN_Like(result_h);
  Build_OP(TOP_add,   tn3,  tn2,  tn1, ops);
  TN* tn4 = Build_TN_Like(result_l);
  Build_OP(TOP_mulhwu, tn4, src1_l, src2_l, ops);
  Build_OP(TOP_add, result_h, tn4,  tn3, ops);
  Build_OP(TOP_mullw,  result_l,  src1_l,    src2_l, ops);
}


static void Expand_64Bit_Shift(SHIFT_DIRECTION shift_dir,
        TN* result_l, TN* src_l, TN* shift, TYPE_ID mtype, OPS* ops)
{ 
  TN * result_h = Get_TN_Pair(result_l);
  TN * src_h    = Get_TN_Pair(src_l);

  if (src_h == NULL){
    src_h = Get_64Bit_High_TN(src_l, MTYPE_signed(mtype), ops);
  }

  FmtAssert(result_h && src_h && src_l, ("Expand_64Bit_Shift result or operand1 TN pair not setup"));
  FmtAssert(TN_has_value(shift),("Expand_64Bit_Shift : should be a value tn"));

    INT64 shift_amt = TN_value(shift);
    shift_amt = shift_amt % 64;
    if (shift_amt < 0) shift_amt += 64;
    
    if (shift_amt > 32){
      switch (shift_dir){
      case shift_left:
        Build_OP(TOP_slwi, result_h, src_l, Gen_Literal_TN(shift_amt-32,4), ops);
        Build_OP(TOP_li,   result_l, Gen_Literal_TN(0,4), ops);
        break;

      case shift_aright:
	 Build_OP(TOP_srawi, result_l, src_h, Gen_Literal_TN(shift_amt-32, 4), ops);
        Build_OP(TOP_srawi, result_h, src_h, Gen_Literal_TN(31, 4), ops);
        break;

      case shift_lright:
	 Build_OP(TOP_srwi, result_l, src_h, Gen_Literal_TN(shift_amt-32,4), ops);
        Build_OP(TOP_li,   result_h, Gen_Literal_TN(0,4), ops);
        break;
      }
    } 
    else if (shift_amt == 32){
      switch (shift_dir){
      case shift_left:
        Exp_COPY(result_h,src_l, ops);
        Build_OP(TOP_li, result_l, Gen_Literal_TN(0,4), ops);
        break;

      case shift_aright:
        Exp_COPY(result_l, src_h, ops);
        Build_OP(TOP_srawi, result_h, src_h, Gen_Literal_TN(31, 4), ops);        
        break;

      case shift_lright:
        Exp_COPY(result_l, src_h, ops);
        Build_OP(TOP_li, result_h, Gen_Literal_TN(0,4), ops);        
        break;
      }
    } 
    else { // for shift_amt < 32
      TN* tn = Build_TN_Like(result_l);
      TN* tn1 = Build_TN_Like(result_h);
      switch (shift_dir){
      case shift_left:
        Build_OP(TOP_srwi, tn, src_l, Gen_Literal_TN(32-shift_amt, 4), ops);
        Build_OP(TOP_slwi, tn1, src_h, Gen_Literal_TN(shift_amt, 4), ops);
        Build_OP(TOP_or,   result_h, tn1, tn, ops);
        Build_OP(TOP_slwi, result_l, src_l, Gen_Literal_TN(shift_amt, 4), ops);
        break;

      case shift_aright:
	 Build_OP(TOP_srwi, result_l, src_l, Gen_Literal_TN(shift_amt, 4), ops);
        Build_OP(TOP_rlwimi, result_l, src_h, Gen_Literal_TN(32-shift_amt, 4),
          Gen_Literal_TN(0, 4), Gen_Literal_TN(shift_amt-1, 4), result_l, ops);
        Build_OP(TOP_srawi, result_h, src_h, Gen_Literal_TN(shift_amt, 4), ops);
        break;

      case shift_lright:
        Build_OP(TOP_srwi, result_l, src_l, Gen_Literal_TN(shift_amt, 4), ops);
        Build_OP(TOP_rlwimi, result_l, src_h, Gen_Literal_TN(32-shift_amt, 4), 
          Gen_Literal_TN(0, 4), Gen_Literal_TN(shift_amt-1, 4), result_l, ops);
        Build_OP(TOP_srwi, result_h, src_h, Gen_Literal_TN(shift_amt, 4), ops);
        break;
      }
    }
}

void Expand_Vararg_Float_Flag(BOOL bfloat, OPS * ops)
{
  TOP top = bfloat ? TOP_creqv : TOP_crxor;
  TN * tn = Gen_Literal_TN(6, 4);
  TN * cr = Gen_CR_TN(0);
  Build_OP(top, cr, tn, tn, tn, cr, cr, ops);
}

static void Expand_64Bit_Abs(TN* result, TN* src, TYPE_ID mtype, OPS* ops)
{
  FmtAssert(FALSE, ("Expand_64Bit_Abs NYI"));
}

void
Expand_Copy (TN *result_l, TN *src_l, TYPE_ID mtype, OPS *ops)
{
  if (OP_NEED_PAIR(mtype)) {    
    TN * result_h  = Get_TN_Pair(result_l);
    TN * src_h     = Get_TN_Pair(src_l);
    if (result_h && src_h) {
      Build_OP(TOP_mr, result_l, src_l, ops);
      Set_OP_copy(OPS_last(ops));
      Build_OP(TOP_mr, result_h, src_h, ops);
      Set_OP_copy(OPS_last(ops));      
    }
    else if (result_h == NULL) {  // I8/U8 ==>> U4/I4
      DevWarn("Expand_Copy : result TN pair not setup");
      FmtAssert(src_h, ("Expand_Copy : src TN pair not setup"));
      Build_OP(TOP_mr, result_l, src_l, ops);
      Set_OP_copy(OPS_last(ops));
    }
    else {  // U4/I4 ==>> U8/I8
      Build_OP(TOP_mr, result_l, src_l, ops);
      Set_OP_copy(OPS_last(ops));
      if (MTYPE_is_signed(mtype)) {
        Build_OP(TOP_srawi, result_h, src_l, Gen_Literal_TN(31, 4), ops);
      } else {
        Build_OP(TOP_li, result_h, Gen_Literal_TN(0, 4), ops);
      }
    }
  }
  else if (MTYPE_is_float(mtype)) {
    Build_OP(TOP_fmr, result_l, src_l, ops);
    Set_OP_copy(OPS_last(ops));
  }
  else {
    Build_OP(TOP_mr, result_l, src_l, ops);
    Set_OP_copy(OPS_last(ops));
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
Expand_Convert_Length (TN *dest, TN *src, TN *length_tn, TYPE_ID mtype, TYPE_ID desc, OPS *ops)
{
  FmtAssert(!MTYPE_float(mtype), ("Expand_Convert_Length: illegal data type\n"));
  FmtAssert(TN_has_value(length_tn), ("Expand_Convert_Length: non-constant length\n"));

  UINT64 val = TN_value(length_tn);
  BOOL signed_extension = MTYPE_is_signed(mtype);

  if (OP_NEED_PAIR(mtype)) {    // 64bit result
    if (!Get_TN_Pair(dest)) {
      Add_TN_Pair(dest, Build_TN_Like(dest));
    }
    
    Expand_64Bit_Cvtl(dest, src, length_tn, mtype, desc, ops);
  }
  else {    // 32bit resutl
    if (val == 32) {
      Build_OP(TOP_mr, dest, src, ops);
    }
    else if (signed_extension) {
      if (val == 8) {
        Build_OP(TOP_extsb, dest, src, ops);
      } else if (val == 16) {
        Build_OP(TOP_extsh, dest, src, ops);
      } else {
        TN* tn = Build_TN_Like(dest);
        Build_OP(TOP_slwi, tn, src, Gen_Literal_TN(32 - val, 4), ops);
        Build_OP(TOP_srawi, dest, tn, Gen_Literal_TN(32 - val, 4), ops);
      }
    } else {
        if (val <= 16) {
          Build_OP(TOP_andi_, dest, src, Gen_Literal_TN((1 << val) - 1, 4), ops);  
        } else {
          TN* tn = Build_TN_Like(dest);
          Build_OP(TOP_slwi, tn, src, Gen_Literal_TN(32 - val, 4), ops);
          Build_OP(TOP_srwi, dest, tn, Gen_Literal_TN(32 - val, 4), ops);
        }
    }
  }
}

static BOOL Exp_Imm_Value(TN * dest, INT64 val, OPS * ops)
{
  TN * tmp = Build_TN_Like(dest);
  if (ISA_LC_Value_In_Class (val, LC_simm16)) {
    Build_OP(TOP_li, dest, Gen_Literal_TN(val, 4), ops);
  }
  else if (ISA_LC_Value_In_Class (val, LC_uimm16)) {
    Build_OP(TOP_li, tmp, Gen_Literal_TN(0, 4), ops);
    Build_OP(TOP_ori, dest, tmp, Gen_Literal_TN((ushort)val, 4), ops);
  }
  else if (val >= INT32_MIN && val <= INT32_MAX) {
    short sv = (short)((val >> 16)&0xffff);
    if (val & 0xffff){
      Build_OP(TOP_lis, tmp, Gen_Literal_TN(sv, 4), ops);    
      Build_OP(TOP_ori, dest, tmp, Gen_Literal_TN(val & 0xffff, 4), ops);
    }
    else {
      Build_OP(TOP_lis, dest, Gen_Literal_TN(sv, 4), ops);    
    }
  }
  else if ((UINT32)val <= UINT32_MAX) {
    short sv = (short)((val >> 16)&0xffff);
    if (val & 0xffff){
      Build_OP(TOP_lis, tmp, Gen_Literal_TN(sv, 4), ops);    
      Build_OP(TOP_ori, dest, tmp, Gen_Literal_TN(val & 0xffff, 4), ops);
    }
    else {
      Build_OP(TOP_lis, dest, Gen_Literal_TN(sv, 4), ops);    
    }
  }
  else {  // can not handle
    return FALSE;
  }
  
  return TRUE;
}
static void
Exp_Immediate (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  INT64 val;
  // TN *tmp = Build_TN_Like(dest);
  
  if (TN_has_value(src)) {
    val = TN_value(src);
  }
  else if (TN_is_symbol(src)) {
    ST *base;
    Base_Symbol_And_Offset_For_Addressing (TN_var(src), TN_offset(src), &base, 
        &val);
  }
  else FmtAssert(FALSE,("unexpected constant in Exp_Immediate"));

  if (OP_NEED_PAIR(mtype) && Get_TN_Pair(dest)){
    Expand_64Bit_Unary_OP(OPR_INTCONST, MTYPE_I8, dest, src, ops );
  }     
  else if (TN_size(src) == 8) {
    if (!Get_TN_Pair(dest))
	Add_TN_Pair(dest, Build_TN_Like(dest));
    Expand_64Bit_Unary_OP(OPR_INTCONST, MTYPE_I8, dest, src, ops );   
  }  
  else if (TN_is_symbol(src) && TN_relocs(src) == TN_RELOC_GPSUB) {
    FmtAssert(FALSE, ("Reloc_GPSub not verification"));
    Build_OP(TOP_li, dest, Gen_Symbol_TN(TN_var(src), 
      0, TN_RELOC_HI_GPSUB), ops);
    Build_OP(TOP_addi, dest, dest, Gen_Symbol_TN(TN_var(src), 
      0, TN_RELOC_LO_GPSUB), ops);
  }
  else if (Exp_Imm_Value(dest, val, ops)) {
    return;
  }
  else {
    FmtAssert(FALSE, ("why here ?"));  
    TCON tcon = Host_To_Targ (MTYPE_I8, val);
    ST *sym   = New_Const_Sym (Enter_tcon (tcon), Be_Type_Tbl(MTYPE_I8));
    Allocate_Object(sym);
    if(ST_gprel(sym)) {
      Build_OP(TOP_lwz, dest, GP_TN, Gen_Symbol_TN(sym, 0, TN_RELOC_GPREL16), ops);
      DevWarn("Long Long value is not supported, constant value has been truncated");
    }
    else {
      TN* tn = Build_TN_Like(dest);
      Build_OP(TOP_li, tn, Gen_Literal_TN((val >> 48) & 0xffff, 8), ops);
      Build_OP(TOP_ori, dest, tn, Gen_Literal_TN((val >> 32) & 0xffff, 8), ops);
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
Expand_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops, TYPE_ID mtype)
{
  FmtAssert((TN_is_constant(src)), ("unexpected non-constant in Expand_Immediate"));
  FmtAssert((TN_has_value(src) || TN_is_symbol(src)), ("expected value or const in Expand_Immediate"));
  
  Exp_Immediate(dest, src, mtype, ops);
}

void 
Expand_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
  Expand_Immediate(dest, src, is_signed, ops, 
    is_signed ? MTYPE_I4 : MTYPE_U4);
}

TN*
Expand_Immediate_Into_Register (TN * reg, TN *src, TYPE_ID mtype, OPS *ops)
{
  /* load into reg and do reg case */
  TN * tmp, *tmp1;
  if (reg) 
    tmp = reg;
  else 
    tmp = Build_TN_Of_Mtype(MTYPE_I4);
  
  if (OP_NEED_PAIR(mtype)) {
    TN * reg1 = NULL;
    if (reg) 
      reg1 = Get_TN_Pair(reg);
    
    if (!reg1) {
      tmp1 = Build_TN_Like(tmp);
      Add_TN_Pair(tmp, tmp1);
    }
  }
  
  Expand_Immediate(tmp, src, MTYPE_is_signed(mtype), ops, mtype);
  return tmp;
}

TN * 
Expand_Immediate_Into_Register (TN *src, BOOL is_64bit, OPS *ops)
{
  TYPE_ID mtype = is_64bit ? MTYPE_I8 : MTYPE_I4;
  return Expand_Immediate_Into_Register(NULL, src, mtype, ops);
}

#include <set>
#include <queue>

static WN * CURRENT_PU_WN;

void Set_Current_PU_WN(WN * wn)
{
  CURRENT_PU_WN = wn;
}

void Set_PU_Frame_Length_ST_In_BB(BB * bb, INT64 len) 
{
  OP * prev = NULL;
  OP * op = OPS_first(&bb->ops);
  while (op) {
  	// for (INT resnum = 0; resnum < OP_opnds(op); resnum++) {      
	  if (OP_code(op) == TOP_stw) {
	  	TN * tn = OP_opnd(op, 2);
	  	ST * st = TN_var(tn);
		if (strncmp(ST_name(st), "__PU_FRAME_LENGTH", 17) == 0) {
		  FmtAssert((prev != NULL), ("Set_PU_Frame_Length_ST_In_BB prev is NULL"));
		  FmtAssert((OP_code(prev) == TOP_li), ("Set_PU_Frame_Length_ST_In_BB prev is not TOP_li"));

      len = len + TN_value(OP_opnd(prev, 0)) + 8;
		  Set_OP_opnd(prev, 0, Gen_Literal_TN(len, 4));
		  break;
		}
	  }
    // }
	prev = op;
  	op = op->next;
  }
}

void Set_PU_Frame_Length_ST(BB * start, INT64 len) 
{
  FmtAssert((len < 0x7fff), ("VA can not handle Frame Length > 0x7fff"));
  
  std::queue<BB*> to_visit;
  std::set<BB*>   visited_bb;
  to_visit.push(start);
  int cnt = 0;
  
  while ((!to_visit.empty()) && (cnt++ < 1000)) {
    BB * bb_proc = to_visit.front();
    visited_bb.insert(bb_proc);
    to_visit.pop();

	Set_PU_Frame_Length_ST_In_BB(bb_proc, len);

    BBLIST* nxt;
    BBLIST* prevs;
    for (prevs = BB_preds(start); prevs; prevs = nxt) {
      BB* bb = BBLIST_item(prevs);
      nxt = BBLIST_next(prevs);

      if (visited_bb.count(bb) == 0) 
        to_visit.push(bb);
    } 
  }

}

extern STACK_MODEL Current_PU_Stack_Model;

static std::queue<OP*> frame_lengths;
static std::queue<OP*> va_arg_starts;

extern INT32 Get_Vararg_Start_Offset(void);

#define Set_OP_code(o,opc)	((o)->opr = (mTOP)(opc))

static void
Set_FrameLen_TN_Value(INT64 frameLenVal)
{
	while (!frame_lengths.empty()) {
		OP* op = frame_lengths.front();
		frame_lengths.pop();
		Set_OP_code(op, TOP_addi);
		TN* frameLen = OP_opnd(op, 1);
		if (Current_PU_Stack_Model == SMODEL_SMALL) {
			Set_TN_value(frameLen, TN_value(frameLen) + frameLenVal);
		}
	}
}

static void
Set_Va_Arg_Start_TN_Value()
{
	while (!va_arg_starts.empty()) {
		OP* op = va_arg_starts.front();
		va_arg_starts.pop();
		Set_OP_code(op, TOP_addi);
		Set_TN_value(OP_opnd(op, 1), Get_Vararg_Start_Offset());
	}
}

void Expand_SR_Adj(BOOL isEntry, TN *result, TN *imm, OPS *ops)
{ 
  if (!isEntry) {
    FmtAssert(FALSE, ("Not IMP"));
    return;
  }

  ST * pu_st = Get_Current_PU_ST();
  WN * pu = CURRENT_PU_WN;

  TY_IDX call_ty  = (WN_operator_is(pu, OPR_ICALL) ? WN_ty(pu) :
	  ST_pu_type(pu_st));

  if (TY_has_prototype(call_ty) && TY_is_varargs(call_ty)) {
  	Set_FrameLen_TN_Value(-TN_value(imm));
	Set_Va_Arg_Start_TN_Value();
  }

  if (TN_has_value(imm)) {
    FmtAssert((TN_value(imm) < 0), ("SR adjustment value wrong!"));    
      Build_OP(TOP_stwu, result, result, imm, ops);
  }else{
    FmtAssert((TN_register_class(imm) == ISA_REGISTER_CLASS_integer), ("SR reg class error"));
    Build_OP(TOP_stwux, result, result, imm, ops);
  }
}

void
Expand_Add (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
    ("Expand_Add: illegal result size\n"));

  if (OP_NEED_PAIR(mtype)) {
    if  (!Get_TN_Pair(result))
	Add_TN_Pair(result, Build_TN_Like(result));
    Expand_64Bit_Binary_OP(OPR_ADD, mtype, result, src1, src2, ops );
    return;
  }

  // va_arg overflow_arg_area base
  if (TN_is_dedicated(src1) && TN_register(src1) == 65) {
  	src1 = Current_PU_Stack_Model == SMODEL_SMALL ? SP_TN: FP_TN;
	src2 = Gen_Unique_Literal_TN(TN_value(src2), 4);
	OP* op = Mk_OP(TOP_simaddi, result,src1, src2);
	frame_lengths.push(op);
	OPS_Append_Op(ops, op);
	return;
  }

  // va_arg reg_save_area base
  if (TN_is_dedicated(src1) && TN_register(src1) == 66) {
  	src1 = Current_PU_Stack_Model == SMODEL_DYNAMIC ? FP_TN: SP_TN;
	OP* op = Mk_OP(TOP_simaddi, result, src1, Gen_Unique_Literal_TN(1, 4));
	va_arg_starts.push(op);
	OPS_Append_Op(ops, op);
	return;
  }
  
  TOP new_opcode;
  INT64 val;
  
  if (TN_is_constant(src1)) {
    if (TN_has_value(src1)) {
      val = TN_value(src1);
      if (val == 0) {
        Expand_Copy (result, src2, mtype, ops);
        return;
      }
    } 
    else if (TN_is_symbol(src1)) {
      /* symbolic constant, gp-relative or sp-relative */
      ST *base;
      INT64 val;
      TN *tmp = Build_TN_Of_Mtype (mtype);
      Base_Symbol_And_Offset_For_Addressing (TN_var(src1), TN_offset(src1), 
               &base, &val);
      if (ISA_LC_Value_In_Class (val, LC_simm16)) {
        new_opcode = TOP_addi;
        Build_OP(new_opcode, result, src2, src1, ops);
      } 
      else if ((val >= INT32_MIN && val <= INT32_MAX)) {
        src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
        Build_OP(TOP_add, result, src1, src2, ops);
      } 
      else {
        TCON tcon = Host_To_Targ (MTYPE_I8, val);
        ST *sym = New_Const_Sym (Enter_tcon (tcon), Be_Type_Tbl(MTYPE_I8));
        Allocate_Object(sym);
        if (Use_32_Bit_Pointers)
          Build_OP(TOP_lwz, tmp, GP_TN, Gen_Symbol_TN(sym, 0, TN_RELOC_GOT_DISP), ops);
        else {
          FmtAssert(FALSE, ("Not Imp"));
        }
        FmtAssert(FALSE, ("Not Imp"));
      }       
      return;
    } 
    else FmtAssert(FALSE,("unexpected constant in Expand_Add"));

    if (ISA_LC_Value_In_Class(val, LC_simm16)) {
      Build_OP(TOP_addi, result, src2, Gen_Literal_TN(val, 4), ops);
    } 
    else {
      src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
      Build_OP (TOP_add, result, src2, src1, ops);
    }    
  } 
  else if (TN_is_constant(src2)) {  
    Expand_Add(result, src2, src1, mtype, ops);
  }
  else{
    Build_OP(TOP_add, result, src1, src2, ops);
  }
}

//   U8U8LDID
//   U8U8LDID
// U4SUB
// we should handle above WHIRL
void
Expand_Sub (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32) || (MTYPE_bit_size(mtype) == 64), 
    ("Expand_Sub: illegal result size\n"));

  if (OP_NEED_PAIR(mtype)) {
    if  (!Get_TN_Pair(result))
	Add_TN_Pair(result, Build_TN_Like(result));
    Expand_64Bit_Binary_OP(OPR_SUB, mtype, result, src1, src2, ops );
    return;
  }
  
  TOP new_opcode;
  INT64 val;
  if (TN_is_constant(src2)) {
  if (TN_has_value(src2)) {
    val = - TN_value(src2);
    if (val == 0) {
      Expand_Copy (result, src1, mtype, ops);
      return;
    }
  } 
  else if (TN_is_symbol(src2)) {
    /* symbolic constant, gp-relative or sp-relative */
    ST *base;
    INT64 val;
    Base_Symbol_And_Offset_For_Addressing (TN_var(src2), TN_offset(src2), &base, &val);
    val = - val;
  } 
  else FmtAssert(FALSE,("unexpected constant in Expand_Sub"));

  if (ISA_LC_Value_In_Class (val, LC_simm16)) {
    new_opcode = TOP_addi;
    Build_OP(new_opcode, result, src1, Gen_Literal_TN(val,4), ops);
  } else {
    src2 = Expand_Immediate_Into_Register(NULL, src2, mtype, ops);
    new_opcode = TOP_subf;
    Build_OP(new_opcode, result, src2,  src1, ops);
  }
  }
  else if (TN_is_constant(src1)) {
    TN *tmp = Build_TN_Of_Mtype (mtype);
    Expand_Sub (tmp, src2, src1, mtype, ops);
    Build_OP(TOP_neg, result, tmp, ops);
  } 
  else {
    Build_OP(TOP_subf, result,  src2,  src1, ops);
  }
}


void
Expand_Neg (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64), 
    ("Expand_Neg: illegal result size\n"));

  if (OP_NEED_PAIR(mtype)) {
    if  (!Get_TN_Pair(result)) 
	Add_TN_Pair(result, Build_TN_Like(result));
    Expand_64Bit_Unary_OP(OPR_NEG, mtype, result, src, ops );   
  }
  else
    Build_OP(TOP_neg, result, src, ops);  
}

void
Expand_Abs (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  if (MTYPE_is_float(mtype)) {
    Build_OP(TOP_fabs, dest, src, ops);
  }
  else {
    if (OP_NEED_PAIR(mtype)) {
      TN * mask = Build_TN_Of_Mtype (MTYPE_I4);
      TN * dest_high = Get_TN_Pair(dest);
      TN * src_high = Get_TN_Pair(src); 
      TN * dest_low = dest;
      TN * src_low  = src;
      FmtAssert(dest_high, ("Expand_Abs result tn pair not setup"));
      FmtAssert(src_high , ("Expand_Abs src tn pair not setup"));      
      
      Build_OP(TOP_srawi, mask, src_high, Gen_Literal_TN(31, 4), ops);
      TN* tn = Build_TN_Like(dest_high);
      Build_OP(TOP_xor, tn,  src_high, mask, ops);
      TN* tn1 = Build_TN_Like(dest_low);
      Build_OP(TOP_xor, tn1, src_low, mask, ops);
      Build_OP(TOP_subfc, dest_low, mask, tn1, ops);
      Build_OP(TOP_subfe, dest_high, mask, tn, ops);      
    }
    else {
      TN  * tmp1 = Build_TN_Like(src);
      TN  * tmp2 = Build_TN_Like(src);
      Build_OP(TOP_srawi, tmp1, src, Gen_Literal_TN(31, 4), ops);
      Build_OP(TOP_xor, tmp2, tmp1, src, ops);
      Build_OP(TOP_subf, dest, tmp1, tmp2, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_UNC_DEF); 
    }
  }
}

void
Expand_Shift (TN *result, TN *src1, TN *src2, TYPE_ID mtype, SHIFT_DIRECTION kind, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32) || (MTYPE_bit_size(mtype) == 64), 
    ("Expand_Shift: illegal result size\n"));

  if (OP_NEED_PAIR(mtype)) {
    if (!Get_TN_Pair(result))
	Add_TN_Pair(result, Build_TN_Like(result));      
    Expand_64Bit_Shift(kind, result, src1, src2, mtype, ops );
    return;
  }
  
  WN *tree;
  TOP top;  

  if (TN_is_constant(src1))
    src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
  
  if (TN_has_value(src2)) {
    UINT64 val = TN_value(src2);
    switch (kind) {
    case shift_left:
    top = TOP_slwi;
      break;
    case shift_aright:
    top = TOP_srawi;
      break;
    case shift_lright:
    top = TOP_srwi;
      break;
    }
    Build_OP(top, result, src1, Gen_Literal_TN(val & 31, 4), ops);
  }
  else {
    switch (kind) {
    case shift_left:
      top = TOP_slw;
      break;  
    case shift_aright:
      top = TOP_sraw;
      break;  
    case shift_lright:
      top = TOP_srw;
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
    Build_OP(TOP_slwi, r, x1, Gen_Literal_TN(s, 4), ops);
  }
  else {
    TN *tmp_tn = Build_TN_Like(r);
      Build_OP(TOP_slwi, tmp_tn, x1, Gen_Literal_TN(s, 4), ops);
      Build_OP(TOP_add, r, tmp_tn, x2, ops);
  }
}


/*
 * Expand_Small_Multiply produces an optimized expansion of 
 * multiplication by any constant between -1 and 63. Multiplication is done for 64
 * bit quantities only. 
 *
 */
static void
Expand_Small_Multiply(
              TN *r,        // result
          TN *x,        // source
          INT16 val,    // multiplicand
          OPS * ops)    // place to put the ops
{
  TN *r1;
  TN *r2; // Temps
  TN *Z=Zero_TN; // Makes it a little easier to write

#define ONE_TEMP r1=Build_TN_Like(r)
#define TWO_TEMPS ONE_TEMP; r2=Build_TN_Like(r)

  // Although ugly, a big case statement is I think the best way to express this
  switch (val) {
   case -1:
     Expand_Neg(r,x,MTYPE_I4,ops);
     break;
   case 0:
     Expand_Immediate_Into_Register(NULL, r, MTYPE_I4, ops);
     break;
   case  1 :
     Expand_Copy (r, x, MTYPE_I4, ops);
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
  TN     *result_tn,
  TN     *var_tn,
  TARG_UINT constant,
  INT16     limit,
  TYPE_ID   mtype,
  OPS   *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

/*
 *  Try to expand a multiply into a sequence of less expensive operations.
 */
#define NUM_FAST_MPYS 8
static INT fast_mpys[NUM_FAST_MPYS] = {17,16,9,8,5,4,3,2};

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

static BOOL
Expand_Constant_Multiply (TN *result, TN *var_tn, TARG_INT constant, TYPE_ID mtype, OPS *ops)
{
  BOOL did_do_fast;
  INT16 limit;  /* maximum number of operations to replace the multiply */
  TN *x = var_tn;
  TN *r1 = Build_TN_Like(result);
  INT64 c = constant; // I don't want to depend on TARG_INT
  BOOL needs_sign_extension;

  // fast special cases
  if (c == 0) {
    Build_OP(TOP_li, result, Gen_Literal_TN(0, Pointer_Size), ops);
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
#if defined(TARG_PPC32) 
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
#endif // TARG_PPC32
}

void
Expand_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  // FmtAssert(FALSE, ("Not IMP"));  
  // DevWarn("Expand_Multiply not verified");

  if (OP_NEED_PAIR(mtype)) {
    if (!Get_TN_Pair(result)) 
	Add_TN_Pair(result, Build_TN_Like(result));
    Expand_64Bit_Multiply(result, src1, src2, mtype, ops);
    return;
  }
  
  TOP top;
  INT64 constant;
  //
  // Check for two constants
  // 
  // TODO: check this portion of Expand_Multiply once divrem is retargeted.
  if ((TN_has_value(src1) || TN_is_rematerializable(src1)) &&
      (TN_has_value(src2) || TN_is_rematerializable(src2))) {
    // Two constants can sometimes occur because of DIVREM production in 
    TN *val_tn;
    constant = TN_has_value(src1) ? TN_value(src1) : WN_const_val(TN_home(src1));
    constant *= TN_has_value(src2) ? TN_value(src2) : WN_const_val(TN_home(src2));
    // Need to get the constant of the right length
    constant = Targ_To_Host(Host_To_Targ(mtype,constant));
    val_tn = Gen_Literal_TN(constant, 4);
    Exp_Immediate(result,val_tn,MTYPE_is_signed(mtype),ops);
    return;
  }

  if (!Disable_Const_Mult_Opt && (TN_has_value(src1) || TN_has_value(src2) ||
          TN_is_rematerializable(src1) ||TN_is_rematerializable(src2))) {
    TN *var_tn;
    if (TN_has_value(src1) || TN_is_rematerializable(src1)) {
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
    if (ISA_LC_Value_In_Class (constant, LC_simm16)) {   
      Build_OP(TOP_mulli, result, var_tn, Gen_Literal_TN(constant, 4), ops);
    }
    else {
      TN * tmp = Build_TN_Like(result);
      Exp_Immediate(tmp, Gen_Literal_TN(constant, Pointer_Size), MTYPE_is_signed(mtype), ops);
      Build_OP(TOP_mullw, result, src1, tmp, ops);   
    }
    return;
  }
  
  Build_OP(TOP_mullw, result, src1, src2, ops);
}

/* return high part of multiply result */
void
Expand_High_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  // FmtAssert(FALSE, ("Not IMP"));  
  DevWarn("Expand_High_Multiply not verified");
  TOP top;
  FmtAssert(!TN_is_constant(src1),("Expand_High_Multiply: unexpected constant operand"));
  top = MTYPE_signed(mtype) ? TOP_mulhw: TOP_mulhwu;
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(NULL, src2, mtype, ops);  
  Build_OP(top, result, src1, src2, ops);
}


void
Expand_Logical_Not (TN *dest, TN *src, VARIANT variant, OPS *ops)
{
  Build_OP(TOP_xori, dest, src, Gen_Literal_TN(1, 4), ops);
}

void
Expand_Logical_And (TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  FmtAssert(!Get_TN_Pair(dest), ("No 64 bit logical op"));
  Build_OP(TOP_and, dest, src1, src2, ops);  
}

void
Expand_Logical_Or (TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  FmtAssert(!Get_TN_Pair(dest), ("No 64 bit logical op"));
  Build_OP(TOP_or, dest, src1, src2, ops);
}


void
Expand_Binary_Complement (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  if (OP_NEED_PAIR(mtype) && Get_TN_Pair(dest)) {
    Expand_64Bit_Unary_OP(OPR_BNOT, MTYPE_I8, dest, src, ops);
  }
  else {
    Build_OP(TOP_nor, dest, src, src, ops);
  }
}

void
Expand_Binary_And (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
      ("Expand_Binary_And: illegal result size\n"));
  
  if (OP_NEED_PAIR(mtype) && Get_TN_Pair(dest)) {
    Expand_64Bit_Binary_OP(OPR_BAND, mtype, dest, src1, src2, ops);
    return;
  }

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
    if (ISA_LC_Value_In_Class (val, LC_uimm16)) 
      new_opcode = TOP_andi_;
    else {
      src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
      new_opcode = TOP_and;
    }
    Build_OP(new_opcode, dest, src2, src1, ops);
  }
  else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_And (dest, src2, src1, mtype, ops);
  } 
  else {
    Build_OP(TOP_and, dest, src1, src2, ops);
  }
}

void
Expand_Binary_Or (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
      ("Expand_Binary_Or: illegal dest size\n"));

  if (OP_NEED_PAIR(mtype) && Get_TN_Pair(dest)) {
    Expand_64Bit_Binary_OP(OPR_BIOR, mtype, dest, src1, src2, ops);
    return;
  }
  
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
    if (ISA_LC_Value_In_Class (val, LC_uimm16)) 
      new_opcode = TOP_ori;
    else {
      src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
      new_opcode = TOP_or;
    }
    Build_OP(new_opcode, dest, src2, src1, ops);
  }
  else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_Or (dest, src2, src1, mtype, ops);
  } 
  else {
    Build_OP(TOP_or, dest, src1, src2, ops);
  } 
}

void
Expand_Binary_Xor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
                 ("Expand_Binary_Xor: illegal dest size\n"));

  if (OP_NEED_PAIR(mtype) && Get_TN_Pair(dest)) {
    Expand_64Bit_Binary_OP(OPR_BXOR, mtype, dest, src1, src2, ops);
    return;
  }
  
  if (TN_is_constant(src1)) {
    INT64 val;
    if (TN_has_value(src1)) 
      val = TN_value(src1);
    else FmtAssert(FALSE,("unexpected constant in Expand_Binary_Xor"));
    if (val == 0 && src1 == dest)
      return;

    TOP new_opcode;
    if (ISA_LC_Value_In_Class (val, LC_uimm16)) 
      new_opcode = TOP_xori;
    else {
      src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
      new_opcode = TOP_xor;
    }
    Build_OP(new_opcode, dest, src2, src1, ops);
  }
  else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_Xor (dest, src2, src1, mtype, ops);
  } 
  else {
    Build_OP(TOP_xor, dest, src1, src2, ops);
  }
}

void
Expand_Binary_Nor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  if (OP_NEED_PAIR(mtype) && Get_TN_Pair(dest)) {
    Expand_64Bit_Binary_OP(OPR_BNOR, mtype, dest, src1, src2, ops);
    return;
  }
  
  Build_OP(TOP_nor, dest, src1, src2, ops);
}

static void 
Expand_Cond(TOP top, TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TOP opcmp = TOP_UNDEFINED;
  opcmp = MTYPE_signed(mtype) ? TOP_cmpw : TOP_cmplw;
  if (TN_is_constant(src2)) {
    INT64 val;
    if (TN_has_value(src2)) 
      val = TN_value(src2);
    else if (TN_is_symbol(src2)) {
      ST *base;
      Base_Symbol_And_Offset_For_Addressing(TN_var(src2), TN_offset(src2), &base, &val);
    } 
    else FmtAssert(FALSE,("unexpected constant in Expand_Cond"));

    if ((opcmp == TOP_cmpw) && ISA_LC_Value_In_Class(val, LC_simm16)) { 
      opcmp = TOP_cmpwi;
    }
    else if ((opcmp == TOP_cmplw) && ISA_LC_Value_In_Class (val, LC_uimm16)) {
      opcmp = TOP_cmplwi;
    }
    else{
      src2  = Expand_Immediate_Into_Register(NULL, src2, mtype, ops);
      opcmp = MTYPE_signed(mtype) ? TOP_cmpw : TOP_cmplw;
    }
  }

  OPERATOR opr;
  switch (top) {
  case TOP_blt:opr = OPR_LT;break;
  case TOP_ble:opr = OPR_LE;break;
  case TOP_beq:opr = OPR_EQ;break;
  case TOP_bne:opr = OPR_NE;break;
  case TOP_bge:opr = OPR_GE;break;
  case TOP_bgt:opr = OPR_GT;break;
  default: 
    FmtAssert(FALSE, ("Invalid TOP"));
  }
  
  Handle_Cond_Move_Int(opr, opcmp, dest, src1, src2, ops);
}

void 
Expand_Int_Cmp(OPERATOR opr, TOP top, TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || MTYPE_bit_size(mtype) == 64),
    ("Expand_Int_Cmp: illegal dest size\n"));

  if (OP_NEED_PAIR(mtype)) {    
    if (Get_TN_Pair(src1))
      Expand_64Bit_Int_Cmp(opr, dest, src1, src2, mtype, ops);
    else
      Expand_Cond(top, dest, src1, src2, mtype, ops);    
  }
  else {  
    Expand_Cond(top, dest, src1, src2, mtype, ops);
  }
}

void
Expand_Int_Less (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp(OPR_LT, TOP_blt, dest, src1, src2, mtype, ops);
}

void
Expand_Int_Less_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp(OPR_LE, TOP_ble, dest, src1, src2, mtype, ops);
}

void
Expand_Int_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp(OPR_EQ, TOP_beq, dest, src1, src2, mtype, ops);
}

void
Expand_Int_Not_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp(OPR_NE, TOP_bne, dest, src1, src2, mtype, ops);
}

void
Expand_Int_Greater_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp(OPR_GE, TOP_bge, dest, src1, src2, mtype, ops);
}

void
Expand_Int_Greater (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp(OPR_GT, TOP_bgt, dest, src1, src2, mtype, ops);
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


static void
Expand_Float_To_Int (ROUND_MODE rm, TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  FmtAssert(MTYPE_is_integral(imtype), ("Expand_Float_To_Int : Dest type should be int"));
  if (OP_NEED_PAIR(imtype)) {
    FmtAssert(FALSE, ("Expand_Float_To_Int_Cvt 64Bit convert"));
  }
  else {
    if (MTYPE_is_signed(imtype)) {
      Handle_Float_Int_Cvt(rm, fmtype, dest, src, ops);
    }
    else {
      Handle_Float_Uint_Cvt(rm, fmtype, dest, src, ops);
    }
  }
}

void
Expand_Float_To_Int_Cvt (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  Expand_Float_To_Int(ROUND_USER, dest, src, imtype, fmtype, ops); 
}

void
Expand_Float_To_Int_Round (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  Expand_Float_To_Int(ROUND_NEAREST, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Trunc (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  Expand_Float_To_Int(ROUND_CHOP, dest, src, imtype, fmtype, ops);
}


void
Expand_Float_To_Int_Floor (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  Expand_Float_To_Int(ROUND_NEG_INF, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Ceil (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  Expand_Float_To_Int(ROUND_PLUS_INF, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Float (TN *dest, TN *src, TYPE_ID rtype, TYPE_ID desc, OPS *ops)
{
  if (rtype == MTYPE_F4)
  {
    Build_OP(TOP_frsp, dest, src, ops);
  }
  else
  {
    FmtAssert(FALSE, ("PPC F4->F8 need do nothing"));
  }
}


void
Expand_Int_To_Float (TN *dest, TN *src_l, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    if (imtype == MTYPE_U8) {
        Handle_ULonglong_Float_Cvt(dest, Get_TN_Pair(src_l), src_l, ops, fmtype == MTYPE_F8);
    }
    Handle_Int_Float_Cvt(dest, src_l, ops, MTYPE_is_unsigned(imtype), fmtype == MTYPE_F8);
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
{ 
  FmtAssert(FALSE,("Expand_Compare_And_Select Unimplemented")); 
}
extern TOP Pick_Compare_TOP (VARIANT *, TN **, TN **, OPS *);
extern TOP Cond_To_Top(VARIANT);

void
Expand_Select (TN *dest_tn, TN *cond_tn, TN *true_tn, TN *false_tn, 
  TYPE_ID mtype, BOOL float_cond, OPS *ops)
{
  TN * zo    = NULL;
  TOP cmp    = TOP_UNDEFINED;

  if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_integer) {
    zo      = Gen_Literal_TN(0, 4);
    cmp     = TOP_cmplwi;
  } 
  else if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_float) {
    if (MTYPE_is_float(mtype)) {
      TN* tn = Build_TN_Like(cond_tn);
      Build_OP(TOP_fnabs, tn, cond_tn, ops);
      Build_OP(TOP_fsel, dest_tn, cond_tn, true_tn, false_tn, ops);
      return;
    }
    unsigned char zmagic[8] = {0, 0, 0, 0, 0, 0, 0, 0};
    TCON tcon = Host_To_Targ_Float(MTYPE_F8, *(double *)zmagic);
    TY_IDX ty_idx = MTYPE_F8 << 8;
    ST * st   = New_Const_Sym(Enter_tcon(tcon), ty_idx);
    WN * wn   = WN_CreateConst(OPR_CONST, TY_mtype(ty_idx), MTYPE_V, st);  
    zo        = Gen_Register_TN(ISA_REGISTER_CLASS_float, 8);      
    Exp_Load(MTYPE_F8, MTYPE_F8, zo, st, 0, ops, 0); 
    
    cmp       = TOP_fcmpu;
  }
  else {
    FmtAssert(FALSE, ("Expand_Select : unknown condition tn"));
  }
  TN* dest_tn_new = dest_tn;
  if (TN_is_dedicated(dest_tn)) {
    dest_tn_new = Build_TN_Like(dest_tn);
    if (OP_NEED_PAIR(mtype)) {
      Add_TN_Pair(dest_tn_new, Build_TN_Like(dest_tn));
    }
  }
  
  if (dest_tn_new == true_tn) {
    BB* bb_entry        = Cur_BB;
    BB* bb_cmp_high;
    BB* bb_set_false;
    TN* cond_tn_high = Get_TN_Pair(cond_tn);
    if (cond_tn_high != NULL) {
      bb_cmp_high = Gen_And_Append_BB(bb_entry);
      bb_set_false = Gen_And_Append_BB(bb_cmp_high);
    } else {
      bb_set_false = Gen_And_Append_BB(bb_entry);
    }
    BB* bb_exit         = Gen_And_Append_BB(bb_set_false);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);
    if (cond_tn_high != NULL) {
      BB_branch_wn(bb_cmp_high) = WN_Create(OPC_TRUEBR,1);
      WN_kid0(BB_branch_wn(bb_cmp_high)) = NULL;
      WN_label_number(BB_branch_wn(bb_cmp_high)) = label_bb_exit;
    }
    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = label_bb_exit;
  
    TN* cr3 = Gen_CR_TN(3);
    Build_OP(cmp, cr3, cond_tn, zo, ops);
    if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_float) {
      Build_OP(TOP_cror, cr3, Gen_Literal_TN(14, 4), Gen_Literal_TN(14, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
    }
    Build_OP(TOP_bne, cr3, Gen_Label_TN(label_bb_exit, 0), ops);

    if (ops != &New_OPs)
      OPS_Append_Ops(&New_OPs, ops);

    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    if (cond_tn_high != NULL) {
      OPS* bb_cmp_high_ops = &New_OPs;
      TN* cr4 = Gen_CR_TN(4);
      Build_OP(cmp, cr4, cond_tn_high, zo, ops);
      Build_OP(TOP_bne, cr3, Gen_Label_TN(label_bb_exit, 0), ops);
      total_bb_insts = 0;
      Last_Processed_OP = NULL;
      Process_New_OPs();
      BB_Append_Ops(bb_cmp_high, bb_cmp_high_ops);
      OPS_Init(bb_cmp_high_ops);
    }

    OPS* bb_set_false_ops = &New_OPs;
    Expand_Copy(dest_tn_new, false_tn, mtype, bb_set_false_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_false, bb_set_false_ops);
    OPS_Init(bb_set_false_ops);

    Cur_BB = bb_exit;
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
  } 
  else if (dest_tn_new == false_tn) { 
    BB* bb_entry        = Cur_BB;
    BB* bb_cmp_high;
    BB* bb_set_true;
    LABEL_IDX label_bb_set_true;
    TN* cond_tn_high = Get_TN_Pair(cond_tn);
    if (cond_tn_high != NULL) {
      bb_cmp_high = Gen_And_Append_BB(bb_entry);
      bb_set_true = Gen_And_Append_BB(bb_cmp_high);
      label_bb_set_true  = Gen_Label_For_BB(bb_set_true);
    } else {
      bb_set_true = Gen_And_Append_BB(bb_entry);
    }
    BB* bb_exit         = Gen_And_Append_BB(bb_set_true);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    if (cond_tn_high != NULL) {
      BB_branch_wn(bb_cmp_high) = WN_Create(OPC_TRUEBR,1);
      WN_kid0(BB_branch_wn(bb_cmp_high)) = NULL;
      WN_label_number(BB_branch_wn(bb_cmp_high)) = label_bb_exit;
    }

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    if (cond_tn_high != NULL) {
      WN_label_number(BB_branch_wn(bb_entry)) = label_bb_set_true;
    } else {
      WN_label_number(BB_branch_wn(bb_entry)) = label_bb_exit;
    }
  
    TN* cr3 = Gen_CR_TN(3);
    Build_OP(cmp, cr3, cond_tn, zo, ops);
    if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_float) {
      Build_OP(TOP_cror, cr3, Gen_Literal_TN(14, 4), Gen_Literal_TN(14, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
    }
    if (cond_tn_high != NULL) {
      Build_OP(TOP_bne, cr3, Gen_Label_TN(label_bb_set_true, 0), ops);
    } else {
      Build_OP(TOP_beq, cr3, Gen_Label_TN(label_bb_exit, 0), ops);
    }

    if (ops != &New_OPs)
      OPS_Append_Ops(&New_OPs, ops);

    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    if (cond_tn_high != NULL) {
      OPS* bb_cmp_high_ops = &New_OPs;
      TN* cr4 = Gen_CR_TN(4);
      Build_OP(cmp, cr4, cond_tn_high, zo, ops);
      Build_OP(TOP_beq, cr3, Gen_Label_TN(label_bb_exit, 0), ops);
      total_bb_insts = 0;
      Last_Processed_OP = NULL;
      Process_New_OPs();
      BB_Append_Ops(bb_cmp_high, bb_cmp_high_ops);
      OPS_Init(bb_cmp_high_ops);
    }

    OPS* bb_set_true_ops = &New_OPs;
    Expand_Copy(dest_tn_new, true_tn, mtype, bb_set_true_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_true, bb_set_true_ops);
    OPS_Init(bb_set_true_ops);

    Cur_BB = bb_exit;
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
  } 
  else {
    BB* bb_entry        = Cur_BB;
    BB* bb_cmp_high;
    BB* bb_set_true;
    TN* cond_tn_high = Get_TN_Pair(cond_tn);
    if (cond_tn_high != NULL) {
      bb_cmp_high = Gen_And_Append_BB(bb_entry);
      bb_set_true = Gen_And_Append_BB(bb_cmp_high);
    } else {
      bb_set_true = Gen_And_Append_BB(bb_entry);
    }
    BB* bb_set_false = Gen_And_Append_BB(bb_set_true);
    BB* bb_exit         = Gen_And_Append_BB(bb_set_false);
    LABEL_IDX label_bb_set_true = Gen_Label_For_BB(bb_set_true);
    LABEL_IDX label_bb_set_false = Gen_Label_For_BB(bb_set_false);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    if (cond_tn_high != NULL) {
      BB_branch_wn(bb_cmp_high) = WN_Create(OPC_TRUEBR,1);
      WN_kid0(BB_branch_wn(bb_cmp_high)) = NULL;
      WN_label_number(BB_branch_wn(bb_cmp_high)) = label_bb_set_false;
    }

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    if (cond_tn_high != NULL) {
      WN_label_number(BB_branch_wn(bb_entry)) = label_bb_set_true;
    } else {
      WN_label_number(BB_branch_wn(bb_entry)) = label_bb_set_false;
    }

    BB_branch_wn(bb_set_true) = WN_Create(OPC_GOTO,0);
    WN_label_number(BB_branch_wn(bb_set_true)) = label_bb_exit;
  
    TN* cr3 = Gen_CR_TN(3);
    Build_OP(cmp, cr3, cond_tn, zo, ops);
    if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_float) {
      Build_OP(TOP_cror, cr3, Gen_Literal_TN(14, 4), Gen_Literal_TN(14, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
    }
    if (cond_tn_high != NULL) {
      Build_OP(TOP_bne, cr3, Gen_Label_TN(Gen_Label_For_BB(bb_set_true), 0), ops);
    } else {
      Build_OP(TOP_beq, cr3, Gen_Label_TN(label_bb_set_false, 0), ops);
    }

    if (ops != &New_OPs)
      OPS_Append_Ops(&New_OPs, ops);

    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    if (cond_tn_high != NULL) {
      OPS* bb_cmp_high_ops = &New_OPs;
      TN* cr4 = Gen_CR_TN(4);
      Build_OP(cmp, cr4, cond_tn_high, zo, ops);
      Build_OP(TOP_beq, cr3, Gen_Label_TN(label_bb_set_false, 0), ops);
      total_bb_insts = 0;
      Last_Processed_OP = NULL;
      Process_New_OPs();
      BB_Append_Ops(bb_cmp_high, bb_cmp_high_ops);
      OPS_Init(bb_cmp_high_ops);
    }

    OPS* bb_set_true_ops = &New_OPs;
    Expand_Copy(dest_tn_new, true_tn, mtype, bb_set_true_ops);
    Build_OP(TOP_b, Gen_Label_TN(label_bb_exit, 0), bb_set_true_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_true, bb_set_true_ops);
    OPS_Init(bb_set_true_ops);

    OPS* bb_set_false_ops = &New_OPs;
    Expand_Copy(dest_tn_new, false_tn, mtype, bb_set_false_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_false, bb_set_false_ops);
    OPS_Init(bb_set_false_ops);

    Cur_BB = bb_exit;
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
  }

  

  if (dest_tn_new != dest_tn) {
    Expand_Copy(dest_tn, dest_tn_new, mtype, &New_OPs);
  }
}

static void
Expand_MinMax_All (TN *destMin, TN *destMax, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    BOOL is_signed = MTYPE_is_signed(mtype);
    TN * op1, * op2;
    if (is_signed){
      op1 = Build_TN_Like(src1);
      op2 = Build_TN_Like(src2);
      Build_OP(TOP_xoris, op1, src1, Gen_Literal_TN(0x8000, 4), ops);
      Build_OP(TOP_xoris, op2, src2, Gen_Literal_TN(0x8000, 4), ops); 
    } else {
      op1 = src1;
      op2 = src2;
    }

    TN* tmp1 = Build_TN_Like(op1);
    TN* tmp2 = Build_TN_Like(op1);
    TN* tmp3 = Build_TN_Like(op1);
    Build_OP(TOP_subfc, tmp1, op1, op2, ops);
    Build_OP(TOP_subfe, tmp2, op1, op1, ops);
    if (destMin){
      Build_OP(TOP_and, tmp3, tmp1, tmp2, ops);
      Build_OP(TOP_add, destMin, tmp3, src1, ops); 
    }
    if (destMax){
      Build_OP(TOP_andc, tmp3, tmp1, tmp2, ops);
      Build_OP(TOP_add, destMax, tmp3, src1, ops); 
    } 
/* Algorithm from "The PowerPC Compiler Writer Guide.pdf"
  # Handle Signed
  # R3 = a
  # R4 = b
  xoris R4,R4,0x8000 # flip sign b
  xoris R3,R3,0x8000 # flip sign a
      
      subfc R5,R3,R4 # R5 = b - a with carry
      subfe R6,R4,R4 # R6 = (b >= a) ? 0 : -1
      if (dest){  // get Min
        and R7,R5,R6
        add dest,R3,R7
      }
      if (dest2){ // get Max
        andc R5,R5,R6 # R5 = (b >= a) ? (Rb - Ra) : 0
        add dest2,R3,R5 # R3 = (b >= a) ? Rb : Ra
      }      */
}

static void
Expand_64Bit_MinMax_All(TN *destMin, TN *destMax, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    BB* bb_cmp_high = Gen_And_Append_BB(Cur_BB);
    BB* bb_cmp_low = Gen_And_Append_BB(bb_cmp_high);
    BB* bb_assign1 = Gen_And_Append_BB(bb_cmp_low);
    BB* bb_assign2 = Gen_And_Append_BB(bb_assign1);
    BB* bb_exit =  Gen_And_Append_BB(bb_assign2);
    const LABEL_IDX label_bb_assign1 = Gen_Label_For_BB(bb_assign1);
    const LABEL_IDX label_bb_assign2 = Gen_Label_For_BB(bb_assign2);
    const LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    BB_branch_wn(Cur_BB) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(Cur_BB)) = NULL;
    WN_label_number(BB_branch_wn(Cur_BB)) = label_bb_assign1;

    BB_branch_wn(bb_cmp_high) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_high)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_high)) = label_bb_assign2;

    BB_branch_wn(bb_cmp_low) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_low)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_low)) = label_bb_assign2;

    BB_branch_wn(bb_assign1) = WN_Create(OPC_GOTO,0);
    WN_label_number(BB_branch_wn(bb_assign1)) = label_bb_exit;

    TN* src1_high = Get_TN_Pair(src1);
    if (src1_high == NULL) {
        src1_high = Get_64Bit_High_TN(src1, MTYPE_is_signed(mtype), ops);
        Add_TN_Pair(src1, src1_high);
    }
    TN* src2_high = Get_TN_Pair(src2);
    if (src2_high == NULL) {
        src2_high = Get_64Bit_High_TN(src2, MTYPE_is_signed(mtype), ops);
        Add_TN_Pair(src2, src2_high);
    }
    TN* destMin_new= destMin;
    if (destMin && TN_is_dedicated(destMin)) {
        destMin_new = Build_TN_Like(destMin);
        Add_TN_Pair(destMin_new, Build_TN_Like(destMin));
    }
    TN* destMax_new= destMax;
    if (destMax && TN_is_dedicated(destMax)) {
        destMax_new = Build_TN_Like(destMax);
        Add_TN_Pair(destMax_new, Build_TN_Like(destMax));
    }
    TOP top_cmp_high = MTYPE_is_signed(mtype) ? TOP_cmpw : TOP_cmplw;
    TN* cr_high = Gen_CR_TN(0);
    TN* cr_low = Gen_CR_TN(1);
    Build_OP(top_cmp_high, cr_high, src1_high, src2_high, ops);
    Build_OP(TOP_bgt, cr_high, Gen_Label_TN(label_bb_assign1, 0), ops);
    
    if( ops != &New_OPs )
        OPS_Append_Ops(&New_OPs, ops);
    
    Process_New_OPs();
    BB_Append_Ops(Cur_BB, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);
    
    OPS* bb_cmp_high_ops = &New_OPs;
    Build_OP(top_cmp_high, cr_high , src1_high, src2_high, bb_cmp_high_ops);
    Build_OP(TOP_blt, cr_high , Gen_Label_TN(label_bb_assign2, 0), bb_cmp_high_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_high, bb_cmp_high_ops);
    OPS_Init(bb_cmp_high_ops);
    
    OPS* bb_cmp_low_ops = &New_OPs;       
    Build_OP(TOP_cmplw, cr_low , src1, src2, bb_cmp_low_ops);
    Build_OP(TOP_blt, cr_low, Gen_Label_TN(label_bb_assign2, 0), bb_cmp_low_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);
    
    OPS* bb_assign1_ops = &New_OPs;
    if (destMin_new) {
        Expand_Copy(destMin_new, src2, mtype, bb_assign1_ops);
    }
    if (destMax_new) {
        Expand_Copy(destMax_new, src1, mtype, bb_assign1_ops);
    }
    Build_OP(TOP_b,  Gen_Label_TN(label_bb_exit, 0), bb_assign1_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_assign1, bb_assign1_ops);
    OPS_Init(bb_assign1_ops);

    OPS* bb_assign2_ops = &New_OPs;
    if (destMin_new) {
        Expand_Copy(destMin_new, src1, mtype, bb_assign2_ops);
    }
    if (destMax_new) {
        Expand_Copy(destMax_new, src2, mtype, bb_assign2_ops);
    }

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_assign2, bb_assign2_ops);
    OPS_Init(bb_assign2_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Cur_BB = bb_exit;

    if (destMin_new != destMin) {
        Expand_Copy(destMin, destMin_new, mtype, &New_OPs);
    }
    if (destMax_new != destMax) {
        Expand_Copy(destMax, destMax_new, mtype, &New_OPs);
    }
}

void Expand_Float_MinMax(TN * destMin, TN * destMax, TN * src1, TN * src2, 
  TYPE_ID mtype, OPS * ops)
{
  TN * cond = Gen_Register_TN(ISA_REGISTER_CLASS_float, 8);
  Build_OP(TOP_fsub, cond, src1, src2, ops);

  if (destMax) {
    Build_OP(TOP_fsel, destMax, cond, src1, src2, ops);
  }
  if (destMin) {
    Build_OP(TOP_fsel, destMin, cond, src2, src1, ops);
  }
}

void
Expand_Min (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  if (mtype == MTYPE_I8 || mtype == MTYPE_U8) {
    Expand_64Bit_MinMax_All(dest, NULL, src1, src2, mtype, ops);
  }
  else if (MTYPE_is_float(mtype)) {
    Expand_Float_MinMax(dest, NULL, src1, src2, mtype, ops);
  }
  else {
    Expand_MinMax_All(dest, NULL, src1, src2, mtype, ops);
    
    if (OPS_last(ops))
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_UNC_DEF);
  }
}


void
Expand_Max (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  if (mtype == MTYPE_I8 || mtype == MTYPE_U8) {
    Expand_64Bit_MinMax_All(NULL, dest, src1, src2, mtype, ops);
  }
  else if (MTYPE_is_float(mtype)) {
    Expand_Float_MinMax(NULL, dest, src1, src2, mtype, ops);
  }
  else {
    Expand_MinMax_All(NULL, dest, src1, src2, mtype, ops);
    if (OPS_last(ops))
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_UNC_DEF);
  }
}

void
Expand_MinMax (TN *destMin, TN *destMax, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  if (mtype == MTYPE_I8 || mtype == MTYPE_U8) {
    Expand_64Bit_MinMax_All(destMin, destMax, src1, src2, mtype, ops);
  }
  else if (MTYPE_is_float(mtype)) {
    Expand_Float_MinMax(destMin, destMax, src1, src2, mtype, ops);
  }
  else {
    Expand_MinMax_All(destMin, destMax, src1, src2, mtype, ops);
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

TOP Branch_OPR2TOP(OPERATOR opr)
{
  switch (opr)
  {
  case OPR_LT:
    return TOP_blt; 
  case OPR_LE:
    return TOP_ble;
  case OPR_EQ:
    return TOP_beq;
  case OPR_NE:
    return TOP_bne;
  case OPR_GE:
    return TOP_bge;
  case OPR_GT:
    return TOP_bgt;
  }
  FmtAssert(FALSE, ("INVALID COMPARE OPR"));
  return (TOP)-1;
}

TOP
Branch_OPR2TOP_Reverse(OPERATOR opr)
{
  switch (opr)
  {
  case OPR_LT:
    return TOP_bge; 
  case OPR_LE:
    return TOP_bgt;
  case OPR_EQ:
    return TOP_bne;
  case OPR_NE:
    return TOP_beq;
  case OPR_GE:
    return TOP_blt;
  case OPR_GT:
    return TOP_ble;
  }
  FmtAssert(FALSE, ("INVALID COMPARE OPR"));
  return (TOP)-1;
}

void
Exp_64bit_Select_And_Condition(OPCODE select, TN* result, TN* true_tn, TN* false_tn,
  OPCODE compare, TN* cmp_kid1, TN* cmp_kid2, VARIANT variant, OPS* ops) {
  OPERATOR opr = OPCODE_operator(compare);
  TYPE_ID mtype = OPCODE_rtype(select);
  if (TN_is_constant(cmp_kid2)) {
    cmp_kid2 = Expand_Immediate_Into_Register(cmp_kid2, TRUE, ops);
  }
  TN* result_new = result;
  if (result == cmp_kid1 || result == cmp_kid2 || TN_is_dedicated(result)) {
    result_new = Build_TN_Like(result);
    if (OP_NEED_PAIR(mtype)) {
        Add_TN_Pair(result_new, Build_TN_Like(result));
    }
  }

  TN* cmp_kid1_high = Get_TN_Pair(cmp_kid1);
  TN* cmp_kid2_high = Get_TN_Pair(cmp_kid2);

  TOP cmp = OPCODE_desc(compare) == MTYPE_I8 ? TOP_cmpw : TOP_cmplw;

  if (opr == OPR_EQ || opr == OPR_NE) {
    BB* bb_entry = Cur_BB;
    BB* bb_cmp_low = Gen_And_Append_BB(bb_entry);
    BB* bb_set_true = Gen_And_Append_BB(bb_cmp_low);
    BB* bb_set_false = Gen_And_Append_BB(bb_set_true);
    BB* bb_exit = Gen_And_Append_BB(bb_set_false);

    LABEL_IDX label_bb_set_true = Gen_Label_For_BB(bb_set_true);
    LABEL_IDX label_bb_set_false = Gen_Label_For_BB(bb_set_false);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = opr == OPR_EQ ? label_bb_set_false : label_bb_set_true;

    BB_branch_wn(bb_cmp_low) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_low)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_low)) = label_bb_set_false;

    BB_branch_wn(bb_set_true) = WN_Create(OPC_GOTO,0);
    WN_label_number(BB_branch_wn(bb_set_true)) = label_bb_exit;

    TN* cr = Gen_CR_TN(2);
    Build_OP(cmp, cr, cmp_kid1_high, cmp_kid2_high, ops);
    Build_OP(TOP_bne, cr, Gen_Label_TN(opr == OPR_EQ ? label_bb_set_false : label_bb_set_true, 0), ops);
    if (ops != &New_OPs)
      OPS_Append_Ops(&New_OPs, ops);

    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    OPS* bb_cmp_low_ops = &New_OPs;
    cr = Gen_CR_TN(3);
    Build_OP(TOP_cmplw, cr, cmp_kid1, cmp_kid2, bb_cmp_low_ops);
    Build_OP(opr == OPR_EQ ? TOP_bne : TOP_beq, cr, Gen_Label_TN(label_bb_set_false, 0), bb_cmp_low_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);

    OPS* bb_set_true_ops = &New_OPs;
    Expand_Copy(result_new, true_tn, mtype, bb_set_true_ops);
    Build_OP(TOP_b, Gen_Label_TN(label_bb_exit, 0), bb_set_true_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_true, bb_set_true_ops);
    OPS_Init(bb_set_true_ops);

    OPS* bb_set_false_ops = &New_OPs;
    Expand_Copy(result_new, false_tn, mtype, bb_set_false_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_false, bb_set_false_ops);
    OPS_Init(bb_set_false_ops);

    Cur_BB = bb_exit;
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
  } else {
    BB* bb_entry = Cur_BB;
    BB* bb_cmp_high = Gen_And_Append_BB(bb_entry);
    BB* bb_cmp_low = Gen_And_Append_BB(bb_cmp_high);
    BB* bb_set_true = Gen_And_Append_BB(bb_cmp_low);
    BB* bb_set_false = Gen_And_Append_BB(bb_set_true);
    BB* bb_exit = Gen_And_Append_BB(bb_set_false);

    LABEL_IDX label_bb_set_true = Gen_Label_For_BB(bb_set_true);
    LABEL_IDX label_bb_set_false = Gen_Label_For_BB(bb_set_false);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = label_bb_set_true;

    BB_branch_wn(bb_cmp_high) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_high)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_high)) = label_bb_set_false;

    BB_branch_wn(bb_cmp_low) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_low)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_low)) = label_bb_set_false;

    BB_branch_wn(bb_set_true) = WN_Create(OPC_GOTO,0);
    WN_label_number(BB_branch_wn(bb_set_true)) = label_bb_exit;

    TOP br1;
    TOP br2;
    TOP br3;
    switch (opr) {
    case OPR_LT:
      br1 = TOP_blt;
      br2 = TOP_bgt;
      br3 = TOP_bge;
      break;
    case OPR_GT:
      br1 = TOP_bgt;
      br2 = TOP_blt;
      br3 = TOP_ble;
      break;
    case OPR_LE:
      br1 = TOP_blt;
      br2 = TOP_bgt;
      br3 = TOP_bgt;
      break;
    case OPR_GE:
      br1 = TOP_bgt;
      br2 = TOP_blt;
      br3 = TOP_blt;
      break;
    }
    TN* cr = Gen_CR_TN(2);
    Build_OP(cmp, cr, cmp_kid1_high, cmp_kid2_high, ops);
    Build_OP(br1, cr, Gen_Label_TN(label_bb_set_true, 0), ops);
    if (ops != &New_OPs)
      OPS_Append_Ops(&New_OPs, ops);

    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    OPS* bb_cmp_high_ops = &New_OPs;
    cr = Gen_CR_TN(3);
    Build_OP(cmp, cr, cmp_kid1_high, cmp_kid2_high, bb_cmp_high_ops);
    Build_OP(br2, cr, Gen_Label_TN(label_bb_set_false, 0), bb_cmp_high_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_high, bb_cmp_high_ops);
    OPS_Init(bb_cmp_high_ops);

    OPS* bb_cmp_low_ops = &New_OPs;
    cr = Gen_CR_TN(4);
    Build_OP(TOP_cmplw, cr, cmp_kid1, cmp_kid2, bb_cmp_low_ops);
    Build_OP(br3, cr, Gen_Label_TN(label_bb_set_false, 0), bb_cmp_low_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);

    OPS* bb_set_true_ops = &New_OPs;
    Expand_Copy(result_new, true_tn, mtype, bb_set_true_ops);
    Build_OP(TOP_b, Gen_Label_TN(label_bb_exit, 0), bb_set_true_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_true, bb_set_true_ops);
    OPS_Init(bb_set_true_ops);

    OPS* bb_set_false_ops = &New_OPs;
    Expand_Copy(result_new, false_tn, mtype, bb_set_false_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_false, bb_set_false_ops);
    OPS_Init(bb_set_false_ops);

    Cur_BB = bb_exit;
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
  }
  
}

extern void 
Exp_Select_And_Condition (OPCODE select, TN *result, TN *true_tn, TN *false_tn,
  OPCODE compare, TN *cmp_kid1, TN *cmp_kid2, VARIANT variant, OPS *ops)
{

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

  TYPE_ID mtype = OPCODE_rtype(select);

  if ((TN_is_constant(cmp_kid1) && !(TN_is_constant(cmp_kid2)))){ // swap
    TN * tmp = cmp_kid1;
    cmp_kid1 = cmp_kid2;
    cmp_kid2 = tmp; 
    tmp      = true_tn;
    true_tn  = false_tn;
    false_tn = tmp;
  }
  
  if (Get_TN_Pair(cmp_kid1) != NULL) {
    Exp_64bit_Select_And_Condition(select, result, true_tn, false_tn,
	compare, cmp_kid1, cmp_kid2, variant, ops);
    return;
  }
  TOP cmp = Pick_Compare_TOP (&variant, &cmp_kid1, &cmp_kid2, ops);
  if (TN_is_constant(cmp_kid2) && cmp != TOP_cmplwi && cmp != TOP_cmpwi) {
    TN* new_kid2 = Expand_Immediate_Into_Register(cmp_kid2, false, ops);
    cmp_kid2 = new_kid2;
  }

  OPERATOR opr = OPCODE_operator(compare);
  
  TN* result_new = result;
  if (TN_is_dedicated(result)) {
    result_new = Build_TN_Like(result);
    if (OP_NEED_PAIR(mtype)) {
        Add_TN_Pair(result_new, Build_TN_Like(result));
    }
  }

  if (result_new == true_tn) {
    BB* bb_entry        = Cur_BB;
    BB* bb_set_false = Gen_And_Append_BB(bb_entry);
    BB* bb_exit         = Gen_And_Append_BB(bb_set_false);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);
  
    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = label_bb_exit;

    TN* cr3 = Gen_CR_TN(3);
    Build_OP(cmp, cr3, cmp_kid1, cmp_kid2, ops);
    if (TN_register_class(cmp_kid1) == ISA_REGISTER_CLASS_float) {
      switch (opr) {
      case OPR_LE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(13, 4), Gen_Literal_TN(13, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      case OPR_GE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(12, 4), Gen_Literal_TN(12, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      case OPR_NE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(14, 4), Gen_Literal_TN(14, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      }
    }
    Build_OP(Branch_OPR2TOP(opr), cr3, Gen_Label_TN(label_bb_exit, 0), ops);

    if (ops != &New_OPs)
      OPS_Append_Ops(&New_OPs, ops);

    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    OPS* bb_set_false_ops = &New_OPs;
    Expand_Copy(result_new, false_tn, mtype, bb_set_false_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_false, bb_set_false_ops);
    OPS_Init(bb_set_false_ops);

    Cur_BB = bb_exit;
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
  }
  else if (result_new == false_tn) {
    BB* bb_entry        = Cur_BB;
    BB* bb_set_true = Gen_And_Append_BB(bb_entry);
    BB* bb_exit         = Gen_And_Append_BB(bb_set_true);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);
  
    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = label_bb_exit;

    TN* cr3 = Gen_CR_TN(3);
    Build_OP(cmp, cr3, cmp_kid1, cmp_kid2, ops);
    if (TN_register_class(cmp_kid1) == ISA_REGISTER_CLASS_float) {
      switch (opr) {
      case OPR_LE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(13, 4), Gen_Literal_TN(13, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      case OPR_GE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(12, 4), Gen_Literal_TN(12, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      case OPR_NE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(14, 4), Gen_Literal_TN(14, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      }
    }
    Build_OP(Branch_OPR2TOP_Reverse(opr), cr3, Gen_Label_TN(label_bb_exit, 0), ops);

    if (ops != &New_OPs)
      OPS_Append_Ops(&New_OPs, ops);

    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    OPS* bb_set_true_ops = &New_OPs;
    Expand_Copy(result_new, true_tn, mtype, bb_set_true_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_true, bb_set_true_ops);
    OPS_Init(bb_set_true_ops);

    Cur_BB = bb_exit;
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
  } 
  else{
    BB* bb_entry        = Cur_BB;
    BB* bb_set_true = Gen_And_Append_BB(bb_entry);
    BB* bb_set_false = Gen_And_Append_BB(bb_set_true);
    BB* bb_exit         = Gen_And_Append_BB(bb_set_false);
    LABEL_IDX label_bb_set_false = Gen_Label_For_BB(bb_set_false);
    LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);
  
    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = label_bb_set_false;

    BB_branch_wn(bb_set_true) = WN_Create(OPC_GOTO,0);
    WN_label_number(BB_branch_wn(bb_set_true)) = label_bb_exit;

    TN* cr3 = Gen_CR_TN(3);
    Build_OP(cmp, cr3, cmp_kid1, cmp_kid2, ops);
    if (TN_register_class(cmp_kid1) == ISA_REGISTER_CLASS_float) {
      switch (opr) {
      case OPR_LE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(13, 4), Gen_Literal_TN(13, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      case OPR_GE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(12, 4), Gen_Literal_TN(12, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      case OPR_NE:
        Build_OP(TOP_cror, cr3, Gen_Literal_TN(14, 4), Gen_Literal_TN(14, 4), Gen_Literal_TN(15, 4), cr3, cr3, ops);
        break;
      }
    }

    Build_OP(Branch_OPR2TOP_Reverse(opr), cr3, Gen_Label_TN(label_bb_set_false, 0), ops);
    if (ops != &New_OPs)
      OPS_Append_Ops(&New_OPs, ops);

    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    OPS* bb_set_true_ops = &New_OPs;
    Expand_Copy(result_new, true_tn, mtype, bb_set_true_ops);
    Build_OP(TOP_b, Gen_Label_TN(label_bb_exit, 0), bb_set_true_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_true, bb_set_true_ops);
    OPS_Init(bb_set_true_ops);

    OPS* bb_set_false_ops = &New_OPs;
    Expand_Copy(result_new, false_tn, mtype, bb_set_false_ops);
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_set_false, bb_set_false_ops);
    OPS_Init(bb_set_false_ops);

    Cur_BB = bb_exit;
    total_bb_insts = 0;
    Last_Processed_OP = NULL;
  }
  
  if (result_new != result) {
    Expand_Copy(result, result_new, mtype, &New_OPs);
  }
  
}  //Exp_Select_And_Condition 

extern LABEL_IDX 
Exp_Select_Part1(
        OPCODE select, TN *result, TN *true_tn, TN *false_tn,
        OPCODE compare, TN *cmp_kid1, TN *cmp_kid2, OPS *ops)
{
  TOP cmp1, cmp2;
  DevWarn("Exp_Select_Part1 : HERE");
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
    fprintf(TFile, "\n");
  }

  TYPE_ID desc = OPCODE_desc(compare);
  Is_True(MTYPE_is_float(desc)==FALSE, ("cannot handle floats"));

  OPERATOR compare_opr = OPCODE_operator(compare);
  TN *p;
  OPS new_ops;
  LABEL_IDX merge_lab;

  OPS_Init(&new_ops);
  p = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);

  if (compare_opr == OPR_NE) { // c_neq_[sd] not supported by assembler
    TN *tmp = true_tn;
    true_tn = false_tn;
    false_tn = tmp;
    compare_opr = OPR_EQ;
  }
  
  switch(compare_opr) {
  case OPR_LT:
    Expand_Int_Less (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  case OPR_LE:
    Expand_Int_Less_Equal (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  case OPR_EQ:
    if (desc == MTYPE_B)
  Expand_Bool_Equal (p, cmp_kid1, cmp_kid2, &new_ops);
    else
  Expand_Int_Equal (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  case OPR_GE:
    Expand_Int_Greater_Equal (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  case OPR_GT:
    Expand_Int_Greater (p, cmp_kid1, cmp_kid2, desc, &new_ops);
    break;
  default:
    FmtAssert(FALSE, ("Unknown opcode"));
  }

  if (result != true_tn) {
    Build_OP(TOP_or, result, Zero_TN, true_tn, &new_ops);
  }

  merge_lab = Gen_Temp_Label();
  TN *merge_lab_tn = Gen_Label_TN(merge_lab, 0);
  Build_OP(TOP_bne, p, Zero_TN, merge_lab_tn, &new_ops);

  if (Trace_Exp) {
    OP *op;
    FOR_ALL_OPS_OPs (&new_ops, op) {
      fprintf(TFile, " into "); Print_OP (op);
    }
  }
  OPS_Append_Ops(ops, &new_ops);

  return merge_lab;
}

#define RESET_COND_DEF_LAST(ops) Set_OP_cond_def_kind(OPS_last(ops),OP_ALWAYS_UNC_DEF)

static void
Expand_Intel_F10_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Expand_Intel_F10_SqrtUnimplemented")); }


static void
Expand_Intel_Max_Thr_F8_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Expand_Intel_Max_Thr_F8_Sqrt Unimplemented")); }


static void
Expand_Intel_Max_Thr_F4_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Expand_Intel_Max_Thr_F4_Sqrt Unimplemented")); }


static void
Expand_Intel_Min_Lat_F8_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Expand_Intel_Min_Lat_F8_Sqrt Unimplemented")); }


static void
Expand_Intel_Min_Lat_F4_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Expand_Intel_Min_Lat_F4_Sqrt Unimplemented")); }


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
      ("Expand_Sqrt : illegal result or operand")); 
  
  TOP top = MTYPE_is_size_double(mtype) ? TOP_fsqrt:TOP_fsqrts;
  Build_OP(top, result, src, ops);
}


static void
Expand_Float_Compares(OPERATOR opr, TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Handle_Cond_Move_Float(opr, dest, src1, src2, ops);
}

void
Expand_Float_Less (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(OPR_LT, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Greater (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(OPR_GT, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Less_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(OPR_LE, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Greater_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(OPR_GE, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(OPR_EQ, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Not_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(OPR_NE, dest, src1, src2, mtype, ops);
}

void
Expand_Recip_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert(TN_register_class(result) == ISA_REGISTER_CLASS_float && 
      TN_register_class(src) == ISA_REGISTER_CLASS_float, 
      ("Expand_Recip_Sqrt : illegal result or operand"));  
  
  FmtAssert(FALSE, ("NYI"));
}

void
Expand_Flop (OPCODE opcode, TN *result, TN *src1, TN *src2, TN *src3, OPS *ops)
{
    if (opcode == OPC_F4RSQRT) {
        TN* tn = Build_TN_Like(result);
        Build_OP(TOP_frsqrte, tn, src1, ops);
        Build_OP(TOP_frsp, result, tn, ops);
        return;
    } else if (opcode == OPC_F8RSQRT) {
        Build_OP(TOP_frsqrte, result, src1, ops);
        return;
    } else if (opcode == OPC_F4RECIP) {
        TCON tcon = Host_To_Targ_Float(MTYPE_F4, 1.0); // 2^32 - 1
        TY_IDX ty_idx = MTYPE_F4 << 8;
        ST * st   = New_Const_Sym(Enter_tcon(tcon), ty_idx);
        WN * wn   = WN_CreateConst(OPR_CONST, TY_mtype(ty_idx), MTYPE_V, st);  
        TN * fr  = Gen_Register_TN(ISA_REGISTER_CLASS_float, 4);  
        Exp_Load(MTYPE_F4, MTYPE_F4, fr, st, 0, ops, 0); // load magic value
        Build_OP(TOP_fdivs, result, fr, src1, ops);
//        Build_OP(TOP_fres, result, src1, ops);
        return;
    } 
  TOP opc;

  switch (opcode) {
  case OPC_F4ADD:
    opc = TOP_fadds;
    break;
  case OPC_F8ADD:
    opc = TOP_fadd;
    break;
  case OPC_F4SUB:
    opc = TOP_fsubs;
    break;
  case OPC_F8SUB:
    opc = TOP_fsub;
    break;
  case OPC_F4MPY:
    opc = TOP_fmuls;
    break;
  case OPC_F8MPY:
    opc = TOP_fmul;
    break;
  case OPC_F4MADD:  // (src2 * src3) + src1
    opc = TOP_fmadds;
    break;
  case OPC_F4NMADD: // -((src2 * src3) + src1)
    opc = TOP_fnmadds;
    break;
  case OPC_F4MSUB:  // (src2 * src3) - src1
    opc = TOP_fmsubs;
    break;
  case OPC_F4NMSUB: // -((src2 * src3) - src1)
    opc = TOP_fnmsubs;
    break;
  case OPC_F8MADD:  // (src2 * src3) + src1
    opc = TOP_fmadd;
    break;
  case OPC_F8NMADD: // -((src2 * src3) + src1)
    opc = TOP_fnmadd;
    break;
  case OPC_F8MSUB:  // (src2 * src3) - src1
    opc = TOP_fmsub;
    break;
  case OPC_F8NMSUB: // -((src2 * src3) - src1)
    opc = TOP_fnmsub;
    break;
  case OPC_F4DIV:
    opc = TOP_fdivs;
    break;
  case OPC_F8DIV:
    opc = TOP_fdiv;
    break;
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("unexpected opcode %s", OPCODE_name(opcode)));
    /*NOTREACHED*/
  }
  if (TOP_is_madd(opc)) {
    Build_OP(opc, result, src2, src3, src1, ops);
  } else {
    Build_OP(opc, result, src1, src2, ops);
  }
}

extern void
Init_CG_Expand (void)
{
  static BOOL Initialized = FALSE;

  // per PU:
  Trace_Exp  = Get_Trace (TP_CGEXP, 1);
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
  if (TN_is_constant(src_tn))
  {
    FmtAssert (TN_has_value(src_tn), ("Exp_COPY: illegal source tn"));
    /* expansion for INTCONST doesn't depend on size */
    Exp_OP1 (OPC_I4INTCONST, tgt_tn, src_tn, ops);
  }
  else
  {
    ISA_REGISTER_CLASS tgt_rc = TN_register_class(tgt_tn);
    ISA_REGISTER_CLASS src_rc = TN_register_class(src_tn);

    if (tgt_rc == src_rc && tgt_rc == ISA_REGISTER_CLASS_integer) {
      Build_OP(TOP_mr, tgt_tn, src_tn, ops);
      Set_OP_copy (OPS_last(ops));
    }
    else if (src_tn == tgt_tn)
    {
      /* We don't know how to do this copy, but since the source and
         target are the same, we can just return a nop (we must return
         some op). */
      // Build_OP(TOP_noop, ops);
    }
    else if (tgt_rc == src_rc && tgt_rc == ISA_REGISTER_CLASS_float) {
      /* dedicated TNs always have size 8, so need to check both TNs */
     // BOOL is_double = (TN_size(tgt_tn) == 8 && TN_size(src_tn) == 8);
      Build_OP(TOP_fmr, tgt_tn, src_tn, ops);
      Set_OP_copy (OPS_last(ops));
    }
    else if (ISA_REGISTER_CLASS_integer == tgt_rc && ISA_REGISTER_CLASS_special == src_rc){
      Build_OP(TOP_mflr, tgt_tn, ops); 
    }
    else if (ISA_REGISTER_CLASS_special == tgt_rc && ISA_REGISTER_CLASS_integer == src_rc){ 
      Build_OP(TOP_mtlr, src_tn, ops); 
    }
    else if (ISA_REGISTER_CLASS_condition == tgt_rc && ISA_REGISTER_CLASS_condition == src_rc){ 
      Build_OP(TOP_mcrf, tgt_tn, src_tn, ops); 
    }
    else
    {      
      FmtAssert(FALSE, ("Unimplemented Copy.\n"));
    }
  }
}

static ST *tmp_apply_arg = NULL;
void
Generate_Temp_Apply_Arg ()
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
Exp_Float_Is_Unordered(TN *result, TN *op0, TN *op1, OPS *ops)
{
	TN* cr =  Gen_CR_TN(3);
	Build_OP(TOP_fcmpu, cr, op0, op1, ops);
	TN* tn = Build_TN_Like(result);
	Build_OP(TOP_mfcr, tn, cr, ops);
	Build_OP(TOP_rlwinm, result, tn, Gen_Literal_TN(16, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
}

void
Exp_Intrinsic_Op (INTRINSIC id, TN *result, TN *op0, TN *op1, TYPE_ID mtype, OPS *ops)
{
  TN* tn = Build_TN_Like(result);
  switch (id) {
    case INTRN_ISGREATER:
		Expand_Float_Compares(OPR_GT, result, op0, op1, MTYPE_F8, ops);
		break;
    case INTRN_ISGREATEREQUAL:
		Expand_Float_Compares(OPR_GE, result, op0, op1, MTYPE_F8, ops);
		break;
    case INTRN_ISLESS:
		Expand_Float_Compares(OPR_LT, result, op0, op1, MTYPE_F8, ops);
		break;
    case INTRN_ISLESSEQUAL:
		Expand_Float_Compares(OPR_LE, result, op0, op1, MTYPE_F8, ops);
		break;
    case INTRN_ISLESSGREATER:
		Expand_Float_Compares(OPR_NE, result, op0, op1, MTYPE_F8, ops);
		break;
    case INTRN_ISORDERED:
		Exp_Float_Is_Unordered(tn, op0, op1, ops);
		Build_OP(TOP_xoris, result, tn, Gen_Literal_TN(1, 4), ops);
		break;
    case INTRN_ISUNORDERED:
		Exp_Float_Is_Unordered(result, op0, op1, ops);
		break;
    default:
		FmtAssert(FALSE, ("Exp_Intrinsic_Op unknown op"));
  }
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
  FmtAssert(FALSE, ("Exp_Intrinsic_Call : NYI"));  
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
    /* We don't know how many instructions are "within" the asm, so we
       just assume 3 bytes. */
    num_bytes = 3;
    break;
  case TOP_simaddi:
    return 1;
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
  Build_OP(CGTARG_Noop_Top(), ops);
}

void Expand_Const (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  TN * tmp = Build_TN_Like(dest);
  Exp_Load(mtype, mtype, tmp, TN_var(src), 0, ops, 0);
  Exp_COPY(dest, tmp, ops);
}

static BB* last_bb = NULL;
static TN *last_true_tn = NULL, *last_false_tn = NULL;
void
HB_Reinit_Pred ()
{
  last_true_tn = NULL;
  last_false_tn= NULL;
  last_bb       = NULL;
}

void
Exp_True_False_Preds_For_Block(BB *bb, TN* &true_tn, TN * &false_tn)
{ 
  FmtAssert(FALSE, ("Exp_True_False_Preds_For_Block : NYI"));  
}

BOOL
Target_Has_Immediate_Operand (WN *parent, WN *expr)
{
  OPERATOR opr = WN_operator(parent);
  return opr == OPR_ADD  || opr == OPR_SUB  || 
          opr == OPR_BAND || opr == OPR_BIOR || opr == OPR_BXOR ||
          opr == OPR_LT   || opr == OPR_LE   || opr == OPR_GT   || opr == OPR_GE ||
          opr == OPR_LSHR || opr == OPR_ASHR || opr == OPR_SHL;
}

void 
Exp_Spadjust (TN *dest, TN *size, VARIANT variant, OPS *ops)
{
  Build_OP(TOP_spadjust, dest, SP_TN, size, ops);
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
{ FmtAssert(FALSE,("Exp_Generic_Pred_Calc Unimplemented")); }
  
  
void
Exp_Pred_Calc(TN* result, OP* cmp_op, COMPARE_TYPE ctype, BOOL false_result,
        OPS* ops)
{ FmtAssert(FALSE,("Exp_Pred_Calc Unimplemented")); }
