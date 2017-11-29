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

#ifndef exp_slintrn_included
#define exp_slintrn_included  "exp_slintrn.h"
extern TN *Expand_C3_INIT_ACC (WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_SAVE_ACC (WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_MVFS(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_INIT_DACC (WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_SAVE_DACC (WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_INIT_ADDR (WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_SAVE_ADDR (WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_PTR (WN *intrncall, TN *result, OPS *ops);
extern void Copy_Tn_MapInfo(TN *src_tn, TN *tgt_tn) ;
extern TN *Expand_SET_ADDRSIZE (WN *intrncall, TN *result, OPS *ops);
extern TN* Expand_Float64_Const(WN* intrncall, TN* result,  BOOL Is_high,  OPS *ops);
extern TN* Expand_Float32_Const(WN* intrncall, TN* result, OPS *ops);
extern TN* Expand_LONGLONG_Const(WN* intrncall, TN* result,  BOOL Is_high,  OPS *ops);
extern TN *Expand_Unsigned_Extract(WN *intrncall, TN *result, OPS *ops);
//new c3 instruction version
extern TN *Expand_Set_CircBuf(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_FFE(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_aadda(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_bitr(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_cs(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_dadd(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_Mode0(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode1(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums,  OPS *ops);
extern TN *Expand_C3_Mode2(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops);
extern TN *Expand_C3_revb(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_Mode3(TOP top, WN *intrncall, TN *result, UINT32 const_parm_numbers, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode4(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode5(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode6(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode7(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode8(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode9(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode10(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_Mode11(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE);
extern TN *Expand_C3_lead(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_shlafa_i(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_saadda_a(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_subc(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_nega(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_C3_mul(TOP top, WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_Init_HI(WN *intrncall, TN *result, OPS *ops);
extern TN *Expand_Copy_HI(WN *intrncall, TN *result, OPS *ops);
// end
#endif

