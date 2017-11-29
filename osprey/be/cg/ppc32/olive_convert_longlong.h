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

#if !defined __OLIVE_CONVERT_LONGLONG_H__
#define __OLIVE_CONVERT_LONGLONG_H__

#include "olive_convert_wn.h"
#include "cgexp_internals.h"
#include "cgexp.h"

void Add_TN_Pair (TN*, TN*);
TN * Get_TN_Pair(TN*);
TN * Get_64Bit_High_TN(TN * low, BOOL bsigned, OPS * ops);

void Handle_High_TNs(TN* &res_l, TN* &res_h, TYPE_ID mtype, OPS* ops);
void Handle_High_TNs(TN* &res_l, TN* &res_h, TN* &src1_l, TN* &src1_h, TYPE_ID mtype, OPS* ops);
void Handle_High_TNs(TN* &res_l, TN* &res_h, TN* &src1_l, TN* &src1_h, TN* &src2_l, TN* &src2_h, TYPE_ID mtype, OPS* ops);
void Handle_64Bit_Unary_OP(OPERATOR opr, TYPE_ID mtype, TN * result_l, TN * src_l, OPS* ops);
void Expand_64Bit_Shift(SHIFT_DIRECTION shift_dir, TN* result_l, TN* src_l, TN* shift, TYPE_ID mtype, OPS* ops);
void Handle_64Bit_Multiply(TN* result_l, TN* src1_l, TN* src2_l, TYPE_ID mtype, OPS* ops);


TN * Handle_LNOT(WN *expr, WN *parent, TN *result);
TN * Handle_LAND_LIOR(WN *expr, WN *parent, TN *result);

void Expand_Int_Cmp(OPERATOR opr, TOP top, TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops);
BOOL Has_Immediate_Operand (WN *parent, WN *expr);
void Expand_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops, TYPE_ID mtype);

LABEL_IDX Get_WN_Label (WN *wn, BOOL *is_non_local_label = NULL);


TN * Handle_LDID (WN *ldid, TN *result, OPCODE opcode);
void Handle_STID (WN *stid, OPCODE opcode);
TN * Handle_ILOAD (WN *iload, TN *result, OPCODE opcode);
void Handle_ISTORE (WN *istore, OPCODE opcode);
TN * Handle_LDBITS (WN *ldbits, TN *result, OPCODE opcode);
void Handle_STBITS (WN *stbits);
TN * Handle_ILDBITS (WN *ildbits, TN *result, OPCODE opcode);
void Handle_ISTBITS (WN *istbits);

TN * Handle_LDA (WN *lda, WN *parent, TN *result, OPCODE opcode);
TN * Handle_DIVREM(WN *expr, WN *parent, TN *result, OPCODE opcode);
TN * Handle_DIVPART(WN *expr, WN *parent, TN *result);
TN * Handle_REMPART(WN *expr, WN *parent, TN *result);
TN * Handle_ALLOCA (WN *tree, TN *result);
void Handle_DEALLOCA (WN *tree);


TN * Handle_EXTRACT_BITS (WN *extrbits, TN *result, OPCODE opcode);
TN * Handle_COMPOSE_BITS (WN *compbits, TN *result, OPCODE opcode);

void Handle_Call_Site (WN *call, OPERATOR call_opr);

TN * Handle_INTRINSIC_OP (WN *expr, TN *result);
WN * Handle_INTRINSIC_CALL (WN *intrncall);

TN * Handle_MAXPART(WN *expr, WN *parent, TN *result);
TN * Handle_MINPART(WN *expr, WN *parent, TN *result);
TN * Handle_MINMAX(WN *expr, WN *parent, TN *result, OPCODE opcode);

TN * Handle_SELECT(WN *select, TN *result, OPCODE opcode);

#endif

