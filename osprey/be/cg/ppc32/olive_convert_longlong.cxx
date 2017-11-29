/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.

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

*/

#include "olive_convert_wn.h"
#include "olive_convert_longlong.h"
#include "olive_gen_expr.h"

TN* Map_High_TN(TN* key)
{
	TN* pair = Get_TN_Pair(key);
	if(!pair){
		pair = Build_TN_Like(key);
		Add_TN_Pair(key, pair);
	}
	return pair;
}

TN* Map_OPND_High_TN(TN* &tn_l, TYPE_ID mtype, OPS* ops)
{
	TN* tn_h;
	if(TN_has_value(tn_l)){
		tn_l = Expand_Immediate_Into_Register(NULL, tn_l, mtype, ops);
		tn_h = Map_High_TN(tn_l);
	}
	else{
		tn_h = Get_TN_Pair(tn_l);
		if(tn_h == NULL)
			tn_h = Get_64Bit_High_TN(tn_l, MTYPE_signed(mtype), ops);
	}
	return tn_h;
}

void Handle_High_TNs(TN* &res_l, TN* &res_h, TYPE_ID mtype, OPS* ops)
{
	res_h = Map_High_TN(res_l);
}

void Handle_High_TNs(TN* &res_l, TN* &res_h, TN* &src1_l, TN* &src1_h,
	TYPE_ID mtype, OPS* ops)
{
	res_h = Map_High_TN(res_l);
	src1_h = Map_OPND_High_TN(src1_l, mtype, ops);
}

void Handle_High_TNs(TN* &res_l, TN* &res_h, TN* &src1_l, TN* &src1_h, TN* &src2_l, TN* &src2_h,
	TYPE_ID mtype, OPS* ops)
{
	res_h = Map_High_TN(res_l);
	src1_h = Map_OPND_High_TN(src1_l, mtype, ops);
	src2_h = Map_OPND_High_TN(src2_l, mtype, ops);
}

	

void Handle_64Bit_Unary_OP(OPERATOR opr, TYPE_ID mtype,
           TN * result_l, TN * src_l,
           OPS* ops)
{
  TN * result_h = Map_High_TN(result_l);
  TN * src_h    = Get_TN_Pair(src_l);
  
  switch (opr){
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

void Expand_64Bit_Shift(SHIFT_DIRECTION shift_dir,
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

void Handle_64Bit_Multiply(TN* result_l, TN* src1_l, TN* src2_l, TYPE_ID mtype, OPS* ops)
{
  TN* result_h, *src1_h, *src2_h;
  Handle_High_TNs(result_l, result_h, src1_l, src1_h, src2_l, src2_h, mtype, ops);

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


