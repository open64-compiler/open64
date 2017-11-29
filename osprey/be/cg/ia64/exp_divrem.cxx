/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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


/* CGEXP routines for expanding divide and rem */

#include <stdint.h>

#include <signal.h>
#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "config_targ_opt.h"
#include "config_debug.h"
#include "mtypes.h"
#include "topcode.h"
#include "float_rf.h"
#include "tn.h"
#include "cg_flags.h"
#include "targ_isa_lits.h"
#include "op.h"
#include "cg_spill.h"
#include "cgexp.h"
#include "cgexp_internals.h"

// TODO: this should come from float_rf package.
static const float_rf One_rf = { 0, 0, 0x0ffff, 0x8000000000000000ll };

#define RESET_COND_DEF_LAST(ops) Set_OP_cond_def_kind(OPS_last(ops),OP_ALWAYS_UNC_DEF)
/*****************************************************************************
 *
 * Floating-point division internal support routines
 *
 *****************************************************************************/

static void
Expand_SGI_F10_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  // Do an 82 bit floating-point divide
  // Should be 14 operations, 9 groups
  TN *qpred;
  TN *fr, *f9, *f9a, *f9b, *f9c, *f10, *f10a, *f10b, *f10c, *f8b, *f8c, *f6a, *f7a;

  // Make up the TN's
  qpred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  fr   = Build_TN_Of_Mtype (MTYPE_F10);
  f9   = Build_TN_Of_Mtype (MTYPE_F10);
  f9a  = Build_TN_Of_Mtype (MTYPE_F10);
  f9b  = Build_TN_Of_Mtype (MTYPE_F10);
  f9c  = Build_TN_Of_Mtype (MTYPE_F10);
  f10  = Build_TN_Of_Mtype (MTYPE_F10);
  f10a = Build_TN_Of_Mtype (MTYPE_F10);
  f10b = Build_TN_Of_Mtype (MTYPE_F10);
  f10c = Build_TN_Of_Mtype (MTYPE_F10);
  f8b  = Build_TN_Of_Mtype (MTYPE_F10);
  f8c  = Build_TN_Of_Mtype (MTYPE_F10);
  f6a  = Build_TN_Of_Mtype (MTYPE_F10);
  f7a  = Build_TN_Of_Mtype (MTYPE_F10);

  // Do the operations
  //
  // N.B. reset Cond_def of all the ops because the definitions are not really conditional
  //
  Build_OP(TOP_frcpa, fr,   qpred, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2,  ops);
  Build_OP(TOP_fnma,  f9,   qpred, Gen_Enum_TN(ECV_sf_s1), fr,   src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f10,  qpred, Gen_Enum_TN(ECV_sf_s1), f9,   fr  , fr      , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  f9a,  qpred, Gen_Enum_TN(ECV_sf_s1), f9,   f9            , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f9b,  qpred, Gen_Enum_TN(ECV_sf_s1), f9a,  f10,  f10     , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f10a, qpred, Gen_Enum_TN(ECV_sf_s1), f9b,  src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  f8b,  qpred, Gen_Enum_TN(ECV_sf_s1), fr,   src1          , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f9c,  qpred, Gen_Enum_TN(ECV_sf_s1), f10a, f9b,  f9b,      ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f10b, qpred, Gen_Enum_TN(ECV_sf_s1), f8b,  src2, src1 ,    ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f8c,  qpred, Gen_Enum_TN(ECV_sf_s1), f10b, f9c,  f8b ,     ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f10c, qpred, Gen_Enum_TN(ECV_sf_s1), f9c,  src2, FOne_TN,  ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f6a,  qpred, Gen_Enum_TN(ECV_sf_s1), f8c,  src2, src1 ,    ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f7a,  qpred, Gen_Enum_TN(ECV_sf_s1), f10c, f9c,  f9c ,     ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   fr,   qpred, Gen_Enum_TN(ECV_sf_s0), f6a,  f7a,  f8c ,     ops);

  // Copy in case result overlaps one of the sources
  Exp_COPY(result, fr, ops);
}


#ifdef OSP_OPT
static void
Expand_I8_I8_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  // Do an 82 bit floating-point divide
  TN *qpred;
  TN *f10, *f10a, *f10b, *f10c, *f11, *f11a, *f12, *f12a, *f12b, *f13;
  
  // Make up the TN's
  qpred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  f10  = Build_TN_Of_Mtype (MTYPE_F10);
  f10a = Build_TN_Of_Mtype (MTYPE_F10);
  f10b = Build_TN_Of_Mtype (MTYPE_F10);
  f10c = Build_TN_Of_Mtype (MTYPE_F10);
  f11  = Build_TN_Of_Mtype (MTYPE_F10);
  f11a = Build_TN_Of_Mtype (MTYPE_F10);
  f12  = Build_TN_Of_Mtype (MTYPE_F10);
  f12a = Build_TN_Of_Mtype (MTYPE_F10);
  f12b = Build_TN_Of_Mtype (MTYPE_F10);
  f13  = Build_TN_Of_Mtype (MTYPE_F10);
 
  // Do the operations
  //
  // N.B. reset Cond_def of all the ops because the definitions are not really conditional
  // The following sequence of code is learnt from gcc routine __moddi3
  //        frcpa.s1 f10, p6 = src1,src2
  //(p6)    fmpy.s1 f12 = src1, f10
  //(p6)    fnma.s1 f11 = src2, f10, f1
  //(p6)    fma.s1 f12 = f11, f12, f12
  //(p6)    fmpy.s1 f13 = f11, f11
  //(p6)    fma.s1 f10 = f11, f10, f10
  //(p6)    fma.s1 f11 = f13, f12, f12
  //(p6)    fma.s1 f10 = f13, f10, f10
  //(p6)    fnma.s1 f12 = src2, f11, src1
  //(p6)    fma.s1 f10 = f12, f10, f11
  
  Build_OP(TOP_frcpa, f10,  qpred, True_TN, Gen_Enum_TN(ECV_sf_s1), src1,  src2, ops);
  Build_OP(TOP_fmpy,  f12,  qpred, Gen_Enum_TN(ECV_sf_s1), src1, f10           , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f11,  qpred, Gen_Enum_TN(ECV_sf_s1), src2, f10 , FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f12a, qpred, Gen_Enum_TN(ECV_sf_s1), f11,  f12 , f12     , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  f13,  qpred, Gen_Enum_TN(ECV_sf_s1), f11,  f11           , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f10a, qpred, Gen_Enum_TN(ECV_sf_s1), f11,  f10 , f10     , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f11a, qpred, Gen_Enum_TN(ECV_sf_s1), f13,  f12a, f12a    , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f10b, qpred, Gen_Enum_TN(ECV_sf_s1), f13 , f10a, f10a    , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f12b,  qpred, Gen_Enum_TN(ECV_sf_s1),src2, f11a, src1    , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f10, qpred, Gen_Enum_TN(ECV_sf_s1), f12b, f10b, f11a    , ops);

  // Copy in case result overlaps one of the sources
  Exp_COPY(result, f10, ops);
}
#endif

static void
Expand_SGI_F8_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  // Do a 64 bit floating-point divide
  // Should be 10 operations, 8 groups
  TN *qpred;
  TN *r0,*r2,*r4,*fr,*q0,*q1,*q2,*q,*r;
  
  // Make up the TN's
  qpred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  fr = Build_TN_Of_Mtype (MTYPE_F10);
  r0 = Build_TN_Of_Mtype (MTYPE_F10);
  r2 = Build_TN_Of_Mtype (MTYPE_F10);
  r4 = Build_TN_Of_Mtype (MTYPE_F10);
  q0 = Build_TN_Of_Mtype (MTYPE_F10);
  q1 = Build_TN_Of_Mtype (MTYPE_F10);
  q2 = Build_TN_Of_Mtype (MTYPE_F10);
  q  = Build_TN_Of_Mtype (MTYPE_F10);
  r  = Build_TN_Of_Mtype (MTYPE_F10);

  /* pseudocode
     fr  = recipa(d);                        // [0]
     
     r0  = nmadd_82(fr,d,One_rf);            // [4]
     
     q0  = madd_82(fr,r0,fr); r2  = mul_82(r0,r0);  // 8 
     q1  = madd_82(q0,r2,q0); r4  = mul_82(r2,r2);  // 12
     q2  = madd_82(q1,r4,q1);                       // 16  
     
     q  = mul_82(q2,n);                             // 20
     r  = nmadd_82(q,d,n);                          // 24
     fr = madd_82(q2,r,q,DBL);                      // 28 
  */
  
  // Do the operations
  //
  // N.B. reset Cond_def of all the ops because the definitions are not really conditional
  // 
  Build_OP(TOP_frcpa, fr, qpred, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
  Build_OP(TOP_fnma,  r0, qpred, Gen_Enum_TN(ECV_sf_s1), fr, src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q0, qpred, Gen_Enum_TN(ECV_sf_s1), fr, r0, fr, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  r2, qpred, Gen_Enum_TN(ECV_sf_s1), r0, r0, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q1, qpred, Gen_Enum_TN(ECV_sf_s1), q0, r2, q0, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  r4, qpred, Gen_Enum_TN(ECV_sf_s1), r2, r2, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q2, qpred, Gen_Enum_TN(ECV_sf_s1), q1, r4, q1, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  q,  qpred, Gen_Enum_TN(ECV_sf_s1), src1, q2, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  r,  qpred, Gen_Enum_TN(ECV_sf_s1), q, src2, src1 , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma_d, fr, qpred, Gen_Enum_TN(ECV_sf_s0), q2, r, q, ops);

  // Copy in case result overlaps one of the sources
  Exp_COPY(result, fr, ops);
}

static void
Expand_SGI_F4_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  // Do a 32 bit floating-point divide
  // Should be 8 operations, 5 groups
  TN *qpred;
  TN *r0,*r2,*r4,*fr,*q0,*q1,*q2;
  
  // Make up the TN's
  qpred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  fr = Build_TN_Of_Mtype (MTYPE_F10);
  r0 = Build_TN_Of_Mtype (MTYPE_F10);
  r2 = Build_TN_Of_Mtype (MTYPE_F10);
  r4 = Build_TN_Of_Mtype (MTYPE_F10);
  q0 = Build_TN_Of_Mtype (MTYPE_F10);
  q1 = Build_TN_Of_Mtype (MTYPE_F10);
  q2 = Build_TN_Of_Mtype (MTYPE_F10);

  /* pseudocode
     fr = recipa(d);
     r0 = nmadd_82(fr,d,One_rf); q0  = mul_82(n,fr);
     q1 = madd_82(q0,r0,q0);     r2  = mul_82(r0,r0);
     q2  = madd_82(q1,r2,q1);    r4  = mul_82(r2,r2); 
     fr  = madd_82(f8,r4,f8,SNGL);
  */
  
  // Do the operations
  //
  // N.B. reset Cond_def of all the ops because the definitions are not really conditional
  // 
  Build_OP(TOP_frcpa, fr, qpred, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
  Build_OP(TOP_fnma,  r0, qpred, Gen_Enum_TN(ECV_sf_s1), fr, src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  q0, qpred, Gen_Enum_TN(ECV_sf_s1), fr, src1, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q1, qpred, Gen_Enum_TN(ECV_sf_s1), q0, r0, q0, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  r2, qpred, Gen_Enum_TN(ECV_sf_s1), r0, r0, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q2, qpred, Gen_Enum_TN(ECV_sf_s1), q1, r2, q1, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  r4, qpred, Gen_Enum_TN(ECV_sf_s1), r2, r2, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma_s, fr, qpred, Gen_Enum_TN(ECV_sf_s0), q2, r4, q2, ops);

  // Copy in case result overlaps one of the sources
  Exp_COPY(result, fr, ops);
}


static void
Expand_Intel_Max_Thr_F10_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN *f2, *f3, *f4, *f5;
  TN *f6 = src1;	// load a, the first argument, in f6
  TN * const f7 = src2;	// load b, the second argument, in f7
  TN *f8, *f9, *f10, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;

  // BEGIN DOUBLE EXTENDED PRECISION MAX. THROUGHPUT DIVIDE ALGORITHM

  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  fr = f8;
  // Step (2)
  // e0 = 1 - b * y0 in f9
  // (p6) fnma.s1 f9=f7,f8,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (3)
  // y1 = y0 + e0 * y0 in f10
  // (p6) fma.s1 f10=f9,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (4)
  // e1 = e0 * e0 in f9
  // (p6) fma.s1 f9=f9,f9,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f9, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (5)
  // y2 = y1 + e1 * y1 in f9
  // (p6) fma.s1 f9=f9,f10,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f10, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (6)
  // q0 = a * y0 in f10
  // (p6) fma.s1 f10=f6,f8,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (7)
  // e2 = 1 - b * y2 in f8
  // (p6) fnma.s1 f8=f7,f9,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (8)
  // y3 = y2 + e2 * y2 in f8
  // (p6) fma.s1 f8=f8,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f8, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (9)
  // r0 = a - b * q0 in f9
  // (p6) fnma.s1 f9=f7,f10,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f10, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (10)
  // q1 = q0 + r0 * y3 in f9
  // (p6) fma.s1 f9=f9,f8,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (11)
  // e3 = 1 - b * y3 in f10
  // (p6) fnma.s1 f10=f7,f8,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (12)
  // y4 = y3 + e3 * y3 in f8
  // (p6) fma.s1 f8=f10,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (13)
  // r1 = a - b * q1 in f10
  // (p6) fnma.s1 f10=f7,f9,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (14)
  // q2 = q1 + r1 * y4 in f8
  // (p6) fma.s0 f8=f10,f8,f9
  Build_OP(TOP_fma, fr, p6, Gen_Enum_TN(ECV_sf_s0), f10, f8, f9, ops);
  f8 = fr;

  // END DOUBLE-EXTENDED PRECISION MAX. THROUGHPUT DIVIDE  ALGORITHM

  Exp_COPY(result, f8, ops);
}


static void
Expand_Intel_Max_Thr_F8_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN *f2, *f3, *f4, *f5;
  TN *f6 = src1;	// load a, the first argument, in f6
  TN * const f7 = src2;	// load b, the second argument, in f7
  TN *f8, *f9, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;

  // BEGIN DOUBLE PRECISION THROUGHPUT-OPTIMIZED DIVIDE ALGORITHM

  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  fr = f8;
  // Step (2)
  // e0 = 1 - b * y0 in f9
  // (p6) fnma.s1 f9=f7,f8,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (3)
  // y1 = y0 + e0 * y0 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (4)
  // e1 = e0 * e0 in f9
  // (p6) fma.s1 f9=f9,f9,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f9, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (5)
  // y2 = y1 + e1 * y1 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (6)
  // e2 = e1 * e1 in f9
  // (p6) fma.s1 f9=f9,f9,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f9, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (7)
  // y3 = y2 + e2 * y2 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (8)
  // q0 = a * y3 in f9
  // (p6) fma.d.s1 f9=f6,f8,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F8);
  Build_OP(TOP_fma_d, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (9)
  // r0 = a - b * q0 in f6
  // (p6) fnma.d.s1 f6=f7,f9,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F8);
  Build_OP(TOP_fnma_d, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f6 = t1;
  // Step (10)
  // q1 = q0 + r0 * y3 in f8
  // (p6) fma.d.s0 f8=f6,f8,f9
  Build_OP(TOP_fma_d, fr, p6, Gen_Enum_TN(ECV_sf_s0), f6, f8, f9, ops);
  f8 = fr;

  // END DOUBLE PRECISION THROUGHPUT-OPTIMIZED DIVIDE ALGORITHM

  Exp_COPY(result, f8, ops);
}


static void
Expand_Intel_Max_Thr_F4_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN *f2, *f3, *f4, *f5;
  TN *f6 = src1;	// load a, the first argument, in f6
  TN * const f7 = src2;	// load b, the second argument, in f7
  TN *f8, *f9, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;

  // BEGIN SINGLE PRECISION THROUGHPUT-OPTIMIZED DIVIDE ALGORITHM

  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  fr = f8;
  // Step (2)
  // d = 1 - b * y0 in f9
  // (p6) fnma.s1 f9=f7,f8,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (3)
  // e = d + d * d in f9
  // (p6) fma.s1 f9=f9,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (4)
  // y1 = y0 + e * y0 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (5)
  // q1 = a * y1 in f9
  // (p6) fma.s.s1 f9=f6,f8,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F4);
  Build_OP(TOP_fma_s, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (6)
  // r = a - b * q1 in f6
  // (p6) fnma.s1 f6=f7,f9,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f6 = t1;
  // Step (7)
  // q = q1 + r * y1 in f8
  // (p6) fma.s.s0 f8=f6,f8,f9
  Build_OP(TOP_fma_s, fr, p6, Gen_Enum_TN(ECV_sf_s0), f6, f8, f9, ops);
  f8 = fr;

  // END SINGLE PRECISION THROUGHPUT-OPTIMIZED DIVIDE ALGORITHM

  Exp_COPY(result, f8, ops);
}

static void
Expand_I4_I4_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN *f6, *f7, *f10, *f11, *f8, *f9;
  TN *f13 = src1;	// load a, the first argument, in f13
  TN * const f12 = src2;	// load b, the second argument, in f12
  TN * const p0 = True_TN;
  TN *p6;
  TN *t0;

  t0 = Build_TN_Of_Mtype(MTYPE_I8);
  f6 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_addl,t0,True_TN,Gen_Literal_TN(65501,4),Zero_TN,ops);
  Build_OP(TOP_setf_exp, f6, True_TN, t0, ops);
  

  f7 = Build_TN_Of_Mtype(MTYPE_F10);
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  Build_OP(TOP_frcpa, f7, p6, p0, Gen_Enum_TN(ECV_sf_s1), f13, f12, ops);
  
  f10 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, f10, p6, Gen_Enum_TN(ECV_sf_s1), f12, f7, f1, ops);
  RESET_COND_DEF_LAST(ops);
 
 f11 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, f11, p6, Gen_Enum_TN(ECV_sf_s1), f13, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, f8, p6, Gen_Enum_TN(ECV_sf_s1), f10, f10, f6, ops);
  RESET_COND_DEF_LAST(ops);
  
  f9 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, f9, p6, Gen_Enum_TN(ECV_sf_s1), f10, f11, f11, ops);
  RESET_COND_DEF_LAST(ops);
  
  Build_OP(TOP_fma, f7, p6, Gen_Enum_TN(ECV_sf_s1), f8, f9, f9, ops);

  Exp_COPY(result, f7, ops);
}


static void
Expand_Intel_Min_Lat_F10_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN *f2, *f3, *f4, *f5;
  TN *f6 = src1;	// load a, the first argument, in f6
  TN * const f7 = src2;	// load b, the second argument, in f7
  TN *f8, *f9, *f10, *f11, *f12, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;

  // BEGIN DOUBLE-EXTENDED PRECISION LATENCY-OPTIMIZED DIVIDE ALGORITHM

  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  fr = f8;
  // Step (2)
  // d = 1 - b * y0 in f9
  // (p6) fnma.s1 f9=f7,f8,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (3)
  // q0 = a * y0 in f10
  // (p6) fma.s1 f10=f6,f8,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (4)
  // d2 = d * d in f11
  // (p6) fma.s1 f11=f9,f9,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f9, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f11 = t1;
  // Step (5)
  // d3 = d * d + d in f12
  // (p6) fma.s1 f12=f9,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f12 = t1;
  // Step (6)
  // d5 = d2 * d2 + d in f9
  // (p6) fma.s1 f9=f11,f11,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f11, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (7)
  // y1 = y0 + d3 * y0 in f11
  // (p6) fma.s1 f11=f12,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f12, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f11 = t1;
  // Step (8)
  // y2 = y0 + d5 * y1 in f8
  // (p6) fma.s1 f8=f11,f9,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f9, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (9)
  // r0 = a - b * q0 in f9
  // (p6) fnma.s1 f9=f7,f10,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f10, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (10)
  // q1 = q0 + r0 * y2 in f9
  // (p6) fma.s1 f9=f9,f8,f10
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f10, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (11)
  // e = 1 - b * y2 in f10
  // (p6) fnma.s1 f10=f7,f8,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (12)
  // y3 = y2 + e * y2 in f8
  // (p6) fma.s1 f8=f10,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (13)
  // r = a - b * q1 in f10
  // (p6) fnma.s1 f10=f7,f9,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (14)
  // q = q1 + r * y3 in f8
  // (p6) fma.s0 f8=f10,f8,f9
  Build_OP(TOP_fma, fr, p6, Gen_Enum_TN(ECV_sf_s0), f10, f8, f9, ops);
  f8 = fr;

  // END DOUBLE-EXTENDED PRECISION LATENCY-OPTIMIZED DIVIDE ALGORITHM

  Exp_COPY(result, f8, ops);
}


static void
Expand_Intel_Min_Lat_F8_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN *f2, *f3, *f4, *f5;
  TN *f6 = src1;	// load a, the first argument, in f6
  TN * const f7 = src2;	// load b, the second argument, in f7
  TN *f8, *f9, *f10, *f11, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;

  // BEGIN DOUBLE PRECISION LATENCY-OPTIMIZED DIVIDE ALGORITHM

  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  fr = f8;
  // Step (2)
  // q0 = a * y0 in f9
  // (p6) fma.s1 f9=f6,f8,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (3)
  // e0 = 1 - b * y0 in f10
  // (p6) fnma.s1 f10=f7,f8,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (4)
  // q1 = q0 + e0 * q0 in f9
  // (p6) fma.s1 f9=f10,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (5)
  // e1 = e0 * e0 in f11
  // (p6) fma.s1 f11=f10,f10,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f10, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f11 = t1;
  // Step (6)
  // y1 = y0 + e0 * y0 in f8
  // (p6) fma.s1 f8=f10,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (7)
  // q2 = q1 + e1 * q1 in f9
  // (p6) fma.s1 f9=f11,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (8)
  // e2 = e1 * e1 in f10
  // (p6) fma.s1 f10=f11,f11,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f11, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f10 = t1;
  // Step (9)
  // y2 = y1 + e1 * y1 in f8
  // (p6) fma.s1 f8=f11,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f11, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (10)
  // q3 = q2 + e2 * q2 in f9
  // (p6) fma.d.s1 f9=f10,f9,f9
  t1 = Build_TN_Of_Mtype(MTYPE_F8);
  Build_OP(TOP_fma_d, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f9, f9, ops);
  RESET_COND_DEF_LAST(ops);
  f9 = t1;
  // Step (11)
  // y3 = y2 + e2 * y2 in f8
  // (p6) fma.s1 f8=f10,f8,f8
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f10, f8, f8, ops);
  RESET_COND_DEF_LAST(ops);
  f8 = t1;
  // Step (12)
  // r0 = a - b * q3 in f6
  // (p6) fnma.d.s1 f6=f7,f9,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F8);
  Build_OP(TOP_fnma_d, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f6 = t1;
  // Step (13)
  // q4 = q3 + r0 * y3 in f8
  // (p6) fma.d.s0 f8=f6,f8,f9
  Build_OP(TOP_fma_d, fr, p6, Gen_Enum_TN(ECV_sf_s0), f6, f8, f9, ops);
  f8 = fr;

  // END DOUBLE PRECISION LATENCY-OPTIMIZED DIVIDE ALGORITHM

  Exp_COPY(result, f8, ops);
}


static void
Expand_Intel_Min_Lat_F4_Divide(TN *result, TN *src1, TN *src2, OPS *ops)
{
  TN * const f0 = FZero_TN;
  TN * const f1 = FOne_TN;
  TN *f2, *f3, *f4, *f5;
  TN *f6 = src1;	// load a, the first argument, in f6
  TN *f7 = src2;	// load b, the second argument, in f7
  TN *f8, *fr;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;

  // BEGIN SINGLE PRECISION LATENCY-OPTIMIZED DIVIDE ALGORITHM

  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  f8 = Build_TN_Of_Mtype(MTYPE_F10);
  p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  fr = f8;
  // Step (2)
  // q0 = a * y0 in f6
  // (p6) fma.s1 f6=f6,f8,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f6 = t1;
  // Step (3)
  // e0 = 1 - b * y0 in f7
  // (p6) fnma.s1 f7=f7,f8,f1
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f8, f1, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (4)
  // q1 = q0 + e0 * q0 in f6
  // (p6) fma.s1 f6=f7,f6,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f6, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f6 = t1;
  // Step (5)
  // e1 = e0 * e0 in f7
  // (p6) fma.s1 f7=f7,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (6)
  // q2 = q1 + e1 * q1 in f6
  // (p6) fma.s1 f6=f7,f6,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f6, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f6 = t1;
  // Step (7)
  // e2 = e1 * e1 in f7
  // (p6) fma.s1 f7=f7,f7,f0
  t1 = Build_TN_Of_Mtype(MTYPE_F10);
  Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f7, f0, ops);
  RESET_COND_DEF_LAST(ops);
  f7 = t1;
  // Step (8)
  // q3 = q2 + e2 * q2 in f6
  // (p6) fma.d.s1 f6=f7,f6,f6
  t1 = Build_TN_Of_Mtype(MTYPE_F8);
  Build_OP(TOP_fma_d, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f6, f6, ops);
  RESET_COND_DEF_LAST(ops);
  f6 = t1;
  // Step (9)
  // q3' = q3 in f8
  // (p6) fma.s.s0 f8=f6,f1,f0
  Build_OP(TOP_fma_s, fr, p6, Gen_Enum_TN(ECV_sf_s0), f6, f1, f0, ops);
  f8 = fr;

  // END SINGLE PRECISION LATENCY-OPTIMIZED DIVIDE ALGORITHM

  Exp_COPY(result, f8, ops);
}


static void
Expand_F10_Recip(TN *result, TN *src2, OPS *ops)
{
  // Do an 82 bit floating-point reciprocal
  // Should be 13 operations, 9 groups
  TN *qpred;
  TN *fr, *f9, *f9a, *f9b, *f9c, *f10, *f10a, *f10b, *f10c, *f8c, *f6a, *f7a;
  
  // Make up the TN's
  qpred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  fr   = Build_TN_Of_Mtype (MTYPE_F10);
  f9   = Build_TN_Of_Mtype (MTYPE_F10);
  f9a  = Build_TN_Of_Mtype (MTYPE_F10);
  f9b  = Build_TN_Of_Mtype (MTYPE_F10);
  f9c  = Build_TN_Of_Mtype (MTYPE_F10);
  f10  = Build_TN_Of_Mtype (MTYPE_F10);
  f10a = Build_TN_Of_Mtype (MTYPE_F10);
  f10b = Build_TN_Of_Mtype (MTYPE_F10);
  f10c = Build_TN_Of_Mtype (MTYPE_F10);
  f8c  = Build_TN_Of_Mtype (MTYPE_F10);
  f6a  = Build_TN_Of_Mtype (MTYPE_F10);
  f7a  = Build_TN_Of_Mtype (MTYPE_F10);
  
  // Do the operations
  //
  // N.B. reset Cond_def of all the ops because the definitions are not really conditional
  // 
  Build_OP(TOP_frcpa, fr,   qpred, True_TN, Gen_Enum_TN(ECV_sf_s0), FOne_TN, src2,  ops);
  Build_OP(TOP_fnma,  f9,   qpred, Gen_Enum_TN(ECV_sf_s1), fr,   src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f10,  qpred, Gen_Enum_TN(ECV_sf_s1), f9,   fr  , fr      , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  f9a,  qpred, Gen_Enum_TN(ECV_sf_s1), f9,   f9            , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f9b,  qpred, Gen_Enum_TN(ECV_sf_s1), f9a,  f10,  f10     , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f10a, qpred, Gen_Enum_TN(ECV_sf_s1), f9b,  src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f9c,  qpred, Gen_Enum_TN(ECV_sf_s1), f10a, f9b,  f9b,      ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f10b, qpred, Gen_Enum_TN(ECV_sf_s1), fr,   src2, FOne_TN,  ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f8c,  qpred, Gen_Enum_TN(ECV_sf_s1), f10b, f9c,  fr ,      ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f10c, qpred, Gen_Enum_TN(ECV_sf_s1), f9c,  src2, FOne_TN,  ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  f6a,  qpred, Gen_Enum_TN(ECV_sf_s1), f8c,  src2, FOne_TN,  ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   f7a,  qpred, Gen_Enum_TN(ECV_sf_s1), f10c, f9c,  f9c ,     ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   fr,   qpred, Gen_Enum_TN(ECV_sf_s0), f6a,  f7a,  f8c ,     ops);

  // Copy in case result overlaps one of the sources
  Exp_COPY(result, fr, ops);
}

static void
Expand_F8_Recip(TN *result, TN *src2, OPS *ops)
{
  // Do a 64 bit floating-point recip
  // Should be 8 operations, 7 groups
  TN *qpred;
  TN *r0,*r2,*r4,*fr,*q0,*q1,*q2,*q,*r;
  
  // Make up the TN's
  qpred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  fr = Build_TN_Of_Mtype (MTYPE_F10);
  r0 = Build_TN_Of_Mtype (MTYPE_F10);
  r2 = Build_TN_Of_Mtype (MTYPE_F10);
  r4 = Build_TN_Of_Mtype (MTYPE_F10);
  q0 = Build_TN_Of_Mtype (MTYPE_F10);
  q1 = Build_TN_Of_Mtype (MTYPE_F10);
  q2 = Build_TN_Of_Mtype (MTYPE_F10);
  q  = Build_TN_Of_Mtype (MTYPE_F10);
  r  = Build_TN_Of_Mtype (MTYPE_F10);

  /* pseudocode
     fr  = recipa(d);                        // [0]
     
     r0  = nmadd_82(fr,d,One_rf);            // [4]
     
     q0  = madd_82(fr,r0,fr); r2  = mul_82(r0,r0);  // 8 
     q1  = madd_82(q0,r2,q0); r4  = mul_82(r2,r2);  // 12
     q2  = madd_82(q1,r4,q1);                       // 16  
     
     r  = nmadd_82(q2,d,One_rf);                    // 24
     fr = madd_82(q2,r,q2,DBL);                      // 28 
  */
  
  // Do the operations
  //
  // N.B. reset Cond_def of all th ops becuase the definitions are not really conditional
  // 
  Build_OP(TOP_frcpa, fr, qpred, True_TN, Gen_Enum_TN(ECV_sf_s0), FOne_TN, src2, ops);
  Build_OP(TOP_fnma,  r0, qpred, Gen_Enum_TN(ECV_sf_s1), fr, src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q0, qpred, Gen_Enum_TN(ECV_sf_s1), fr, r0, fr, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  r2, qpred, Gen_Enum_TN(ECV_sf_s1), r0, r0, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q1, qpred, Gen_Enum_TN(ECV_sf_s1), q0, r2, q0, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  r4, qpred, Gen_Enum_TN(ECV_sf_s1), r2, r2, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q2, qpred, Gen_Enum_TN(ECV_sf_s1), q1, r4, q1, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fnma,  r,  qpred, Gen_Enum_TN(ECV_sf_s1), q2, src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma_d, fr, qpred, Gen_Enum_TN(ECV_sf_s0), q2, r, q2, ops);

  // Copy in case result overlaps one of the sources
  Exp_COPY(result, fr, ops);
}

static void
Expand_F4_Recip(TN *result, TN *src2, OPS *ops)
{
  // Do a 32 bit floating-point divide
  // Should be 7 operations, 5 groups
  TN *qpred;
  TN *r0,*r2,*r4,*fr,*q1,*q2;
  
  // Make up the TN's
  qpred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
  fr = Build_TN_Of_Mtype (MTYPE_F10);
  r0 = Build_TN_Of_Mtype (MTYPE_F10);
  r2 = Build_TN_Of_Mtype (MTYPE_F10);
  r4 = Build_TN_Of_Mtype (MTYPE_F10);
  q1 = Build_TN_Of_Mtype (MTYPE_F10);
  q2 = Build_TN_Of_Mtype (MTYPE_F10);

  /* pseudocode
     fr = recipa(d);
     r0 = nmadd_82(fr,d,One_rf);
     q1 = madd_82(fr,r0,fr);   r2  = mul_82(r0,r0);
     r4  = mul_82(r2,r2);      q2  = madd_82(q1,r2,q1);
     fr  = madd_82(f8,r4,f8,SNGL);
  */
  
  // Do the operations
  //
  // N.B. reset Cond_def of all th ops becuase the definitions are not really conditional
  // 
  Build_OP(TOP_frcpa, fr, qpred, True_TN, Gen_Enum_TN(ECV_sf_s0), FOne_TN, src2, ops);
  Build_OP(TOP_fnma,  r0, qpred, Gen_Enum_TN(ECV_sf_s1), fr, src2, FOne_TN , ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q1, qpred, Gen_Enum_TN(ECV_sf_s1), fr, r0, fr, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  r2, qpred, Gen_Enum_TN(ECV_sf_s1), r0, r0, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma,   q2, qpred, Gen_Enum_TN(ECV_sf_s1), q1, r2, q1, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fmpy,  r4, qpred, Gen_Enum_TN(ECV_sf_s1), r2, r2, ops);
  RESET_COND_DEF_LAST(ops);
  Build_OP(TOP_fma_s, fr, qpred, Gen_Enum_TN(ECV_sf_s0), q2, r4, q2, ops);

  // Copy in case result overlaps one of the sources
  Exp_COPY(result, fr, ops);
}

/* Define the exponent parameters for the various float types.
 */
enum { E32min = -126,   E32max = 127,   E32bias = 127   }; // single
enum { E64min = -1022,  E64max = 1023,  E64bias = 1023  }; // double
enum { E80min = -16382, E80max = 16383, E80bias = 16383 }; // long double
enum { E82min = -65534, E82max = 65535, E82bias = 65535 }; // register-format

/* Load a floating point constant in 82-bit register format.
 */
static void
Load_82bit_Constant(TN *result, const float_rf *c, OPS *ops)
{
  TN * const p0 = True_TN;
  TN *r1, *f8, *f9;

  /* If the constant can be exactly represented by a double, then
   * load it that way. If not it has to be constructed from
   * an 18-bit sign/exponent and a 64-bit fraction.
   */
  if (   c->exp >= (E64min + E82bias)
      && c->exp <= (E64max + E82bias)
      && (c->frac & 0x7ff) == 0)
  {
    INT exp = (c->exp - E82bias) + E64bias;
    UINT64 dw =   ((UINT64)c->sign << 63)
	        | ((UINT64)exp << 52)
		| ((c->frac >> 11) & 0xfffffffffffffULL);
    r1 = Build_TN_Of_Mtype(MTYPE_U8);
    Build_OP(TOP_movl, r1, p0, Gen_Literal_TN(dw, 8), ops);
    CGSPILL_Attach_Intconst_Remat(r1, dw);
    Build_OP(TOP_setf_d, result, p0, r1, ops);
    CGSPILL_Attach_Floatconst_Remat(result, MTYPE_F8, *(double *)&dw);
  } else {
    INT32 sign_exp = (c->sign << 17) | c->exp;
    r1 = Build_TN_Of_Mtype(MTYPE_U8);
    Build_OP(TOP_mov_i, r1, p0, Gen_Literal_TN(sign_exp, 4), ops);
    CGSPILL_Attach_Intconst_Remat(r1, sign_exp);
    f8 = Build_TN_Of_Mtype(MTYPE_F10);
    Build_OP(TOP_setf_exp, f8, p0, r1, ops);
    r1 = Build_TN_Of_Mtype(MTYPE_U8);
    Build_OP(TOP_movl, r1, p0, Gen_Literal_TN(c->frac, 8), ops);
    CGSPILL_Attach_Intconst_Remat(r1, c->frac);
    f9 = Build_TN_Of_Mtype(MTYPE_F10);
    Build_OP(TOP_setf_sig, f9, p0, r1, ops);
    Build_OP(TOP_fmerge_se, result, p0, f8, f9, ops);
  }
}

/* Expansion for single precision (32-bit) floating point division
 * by a constant.
 */
static void
Expand_F4_Div_By_Const(TN *result, TN *src1, TN *src2, 
		       const float_rf *b, OPS *ops)
{
  INT pr;
  float_rf y0, y1;
  float_rf d, e;
  TN * const f0 = FZero_TN;
  TN *f6 = src1;		// load a, the first argument, in f6
  TN * const f7 = src2;		// load b, the second argument, in f7
  TN *f8, *f9, *fr, *fy1;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;
  status_field sf[2] = {
    0x0000000000000300,	// pc=3
    0x0000000000001380,	// td, pc=3, wre
  };

  /* Compute the invariant part of the divide expansion:
   */
  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  y0 = frcpa(&pr, &One_rf, b, &sf[0]);
  // Step (2)
  // d = 1 - b * y0 in f9
  // (p6) fnma.s1 f9=f7,f8,f1
  d = fnma(b, &y0, &One_rf, &sf[1]);
  // Step (3)
  // e = d + d * d in f9
  // (p6) fma.s1 f9=f9,f9,f9
  e = fma(&d, &d, &d, &sf[1]);
  // Step (4)
  // y1 = y0 + e * y0 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  y1 = fma(&e, &y0, &y0, &sf[1]);

  /* Now generate code for the variant part of the divide expansion:
   */
  if (pr) {
    fy1 = Build_TN_Of_Mtype(MTYPE_F10);
    Load_82bit_Constant(fy1, &y1, ops);

    // Step (1)
    // y0 = 1 / b in f8
    // frcpa.s0 f8,p6=f6,f7
    f8 = Build_TN_Of_Mtype(MTYPE_F10);
    p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
    Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
    fr = f8;
    // Step (2..4)
    f8 = fy1;
    // Step (5)
    // q1 = a * y1 in f9
    // (p6) fma.s.s1 f9=f6,f8,f0
    t1 = Build_TN_Of_Mtype(MTYPE_F4);
    Build_OP(TOP_fma_s, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
    RESET_COND_DEF_LAST(ops);
    f9 = t1;
    // Step (6)
    // r = a - b * q1 in f6
    // (p6) fnma.s1 f6=f7,f9,f6
    t1 = Build_TN_Of_Mtype(MTYPE_F10);
    Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f6, ops);
    RESET_COND_DEF_LAST(ops);
    f6 = t1;
    // Step (7)
    // q = q1 + r * y1 in f8
    // (p6) fma.s.s0 f8=f6,f8,f9
    Build_OP(TOP_fma_s, fr, p6, Gen_Enum_TN(ECV_sf_s0), f6, f8, f9, ops);
    f8 = fr;

    Exp_COPY(result, f8, ops);
  } else {

    /* If the denominator is such that we know the frcpa will compute
     * the result, then we don't need to generate anything else.
     */
    p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
    Build_OP(TOP_frcpa, result, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  }
}

/* Expansion for double precision (64-bit) floating point division
 * by a constant.
 */
static void
Expand_F8_Div_By_Const(TN *result, TN *src1, TN *src2, 
		       const float_rf *b, OPS *ops)
{
  INT pr;
  float_rf y0, y1, y2, y3;
  float_rf e0, e1, e2;
  TN * const f0 = FZero_TN;
  TN *f6 = src1;		// load a, the first argument, in f6
  TN * const f7 = src2;		// load b, the second argument, in f7
  TN *f8, *f9, *fr, *fy3;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;
  status_field sf[2] = {
    0x0000000000000300,	// pc=3
    0x0000000000001380,	// td, pc=3, wre
  };

  /* Compute the invariant part of the divide expansion:
   */
  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  y0 = frcpa(&pr, &One_rf, b, &sf[0]);
  // Step (2)
  // e0 = 1 - b * y0 in f9
  // (p6) fnma.s1 f9=f7,f8,f1
  e0 = fnma(b, &y0, &One_rf, &sf[1]);
  // Step (3)
  // y1 = y0 + e0 * y0 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  y1 = fma(&e0, &y0, &y0, &sf[1]);
  // Step (4)
  // e1 = e0 * e0 in f9
  // (p6) fma.s1 f9=f9,f9,f0
  e1 = fma(&e0, &e0, &Zero_rf, &sf[1]);
  // Step (5)
  // y2 = y1 + e1 * y1 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  y2 = fma(&e1, &y1, &y1, &sf[1]);
  // Step (6)
  // e2 = e1 * e1 in f9
  // (p6) fma.s1 f9=f9,f9,f0
  e2 = fma(&e1, &e1, &Zero_rf, &sf[1]);
  // Step (7)
  // y3 = y2 + e2 * y2 in f8
  // (p6) fma.s1 f8=f9,f8,f8
  y3 = fma(&e2, &y2, &y2, &sf[1]);

  /* Now generate code for the variant part of the divide expansion:
   */
  if (pr) {
    fy3 = Build_TN_Of_Mtype(MTYPE_F10);
    Load_82bit_Constant(fy3, &y3, ops);

    // Step (1)
    // y0 = 1 / b in f8
    // frcpa.s0 f8,p6=f6,f7
    f8 = Build_TN_Of_Mtype(MTYPE_F10);
    p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
    Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
    fr = f8;
    // Step (2..7)
    f8 = fy3;
    // Step (8)
    // q0 = a * y3 in f9
    // (p6) fma.d.s1 f9=f6,f8,f0
    t1 = Build_TN_Of_Mtype(MTYPE_F8);
    Build_OP(TOP_fma_d, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
    RESET_COND_DEF_LAST(ops);
    f9 = t1;
    // Step (9)
    // r0 = a - b * q0 in f6
    // (p6) fnma.d.s1 f6=f7,f9,f6
    t1 = Build_TN_Of_Mtype(MTYPE_F8);
    Build_OP(TOP_fnma_d, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f6, ops);
    RESET_COND_DEF_LAST(ops);
    f6 = t1;
    // Step (10)
    // q1 = q0 + r0 * y3 in f8
    // (p6) fma.d.s0 f8=f6,f8,f9
    Build_OP(TOP_fma_d, fr, p6, Gen_Enum_TN(ECV_sf_s0), f6, f8, f9, ops);
    f8 = fr;

    Exp_COPY(result, f8, ops);
  } else {

    /* If the denominator is such that we know the frcpa will compute
     * the result, then we don't need to generate anything else.
     */
    p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
    Build_OP(TOP_frcpa, result, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  }
}

/* Expansion for long double precision (80-bit) floating point division
 * by a constant.
 */
static void
Expand_F10_Div_By_Const(TN *result, TN *src1, TN *src2, 
		       const float_rf *b, OPS *ops)
{
  INT pr;
  float_rf y0, y1, y2, y3, y4;
  float_rf e0, e1, e2, e3;
  TN * const f0 = FZero_TN;
  TN *f6 = src1;		// load a, the first argument, in f6
  TN * const f7 = src2;		// load b, the second argument, in f7
  TN *f8, *f9, *f10, *fr, *fy3, *fy4;
  TN * const p0 = True_TN;
  TN *p6;
  TN *t1;
  status_field sf[2] = {
    0x0000000000000300,	// pc=3
    0x0000000000001380,	// td, pc=3, wre
  };

  /* Compute the invariant part of the divide expansion:
   */
  // Step (1)
  // y0 = 1 / b in f8
  // frcpa.s0 f8,p6=f6,f7
  y0 = frcpa(&pr, &One_rf, b, &sf[0]);
  // Step (2)
  // e0 = 1 - b * y0 in f9
  // (p6) fnma.s1 f9=f7,f8,f1
  e0 = fnma(b, &y0, &One_rf, &sf[1]);
  // Step (3)
  // y1 = y0 + e0 * y0 in f10
  // (p6) fma.s1 f10=f9,f8,f8
  y1 = fma(&e0, &y0, &y0, &sf[1]);
  // Step (4)
  // e1 = e0 * e0 in f9
  // (p6) fma.s1 f9=f9,f9,f0
  e1 = fma(&e0, &e0, &Zero_rf, &sf[1]);
  // Step (5)
  // y2 = y1 + e1 * y1 in f9
  // (p6) fma.s1 f9=f9,f10,f10
  y2 = fma(&e1, &y1, &y1, &sf[1]);

  // Step (7)
  // e2 = 1 - b * y2 in f8
  // (p6) fnma.s1 f8=f7,f9,f1
  e2 = fnma(b, &y2, &One_rf, &sf[1]);
  // Step (8)
  // y3 = y2 + e2 * y2 in f8
  // (p6) fma.s1 f8=f8,f9,f9
  y3 = fma(&e2, &y2, &y2, &sf[1]);

  // Step (11)
  // e3 = 1 - b * y3 in f10
  // (p6) fnma.s1 f10=f7,f8,f1
  e3 = fnma(b, &y3, &One_rf, &sf[1]);
  // Step (12)
  // y4 = y3 + e3 * y3 in f8
  // (p6) fma.s1 f8=f10,f8,f8
  y4 = fma(&e3, &y3, &y3, &sf[1]);

  /* Now generate code for the variant part of the divide expansion:
   */
  if (pr) {
    fy3 = Build_TN_Of_Mtype(MTYPE_F10);
    Load_82bit_Constant(fy3, &y3, ops);

    fy4 = Build_TN_Of_Mtype(MTYPE_F10);
    Load_82bit_Constant(fy4, &y4, ops);

    // Step (1)
    // y0 = 1 / b in f8
    // frcpa.s0 f8,p6=f6,f7
    f8 = Build_TN_Of_Mtype(MTYPE_F10);
    p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
    Build_OP(TOP_frcpa, f8, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
    fr = f8;
    // Step (2..5)
    // e0 = 1 - b * y0 in f9
    // y1 = y0 + e0 * y0 in f10
    // e1 = e0 * e0 in f9
    // y2 = y1 + e1 * y1 in f9
    // Step (6)
    // q0 = a * y0 in f10
    // (p6) fma.s1 f10=f6,f8,f0
    t1 = Build_TN_Of_Mtype(MTYPE_F10);
    Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f6, f8, f0, ops);
    RESET_COND_DEF_LAST(ops);
    f10 = t1;
    // Step (7..8)
    // e2 = 1 - b * y2 in f8
    // y3 = y2 + e2 * y2 in f8
    f8 = fy3;
    // Step (9)
    // r0 = a - b * q0 in f9
    // (p6) fnma.s1 f9=f7,f10,f6
    t1 = Build_TN_Of_Mtype(MTYPE_F10);
    Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f10, f6, ops);
    RESET_COND_DEF_LAST(ops);
    f9 = t1;
    // Step (10)
    // q1 = q0 + r0 * y3 in f9
    // (p6) fma.s1 f9=f9,f8,f10
    t1 = Build_TN_Of_Mtype(MTYPE_F10);
    Build_OP(TOP_fma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f9, f8, f10, ops);
    RESET_COND_DEF_LAST(ops);
    f9 = t1;
    // Step (11..12)
    // e3 = 1 - b * y3 in f10
    // y4 = y3 + e3 * y3 in f8
    f8 = fy4;
    // Step (13)
    // r1 = a - b * q1 in f10
    // (p6) fnma.s1 f10=f7,f9,f6
    t1 = Build_TN_Of_Mtype(MTYPE_F10);
    Build_OP(TOP_fnma, t1, p6, Gen_Enum_TN(ECV_sf_s1), f7, f9, f6, ops);
    RESET_COND_DEF_LAST(ops);
    f10 = t1;
    // Step (14)
    // q2 = q1 + r1 * y4 in f8
    // (p6) fma.s0 f8=f10,f8,f9
    Build_OP(TOP_fma, fr, p6, Gen_Enum_TN(ECV_sf_s0), f10, f8, f9, ops);
    f8 = fr;

    Exp_COPY(result, f8, ops);
  } else {

    /* If the denominator is such that we know the frcpa will compute
     * the result, then we don't need to generate anything else.
     */
    p6 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
    Build_OP(TOP_frcpa, result, p6, p0, Gen_Enum_TN(ECV_sf_s0), f6, f7, ops);
  }
}


/* Expansion for floating point division by a power of two.
 * Return a boolean that indicates if we were able to generate the
 * expansion.
 */
static BOOL
Expand_Float_Div_By_Pow2(TN *result, TN *src1, TCON src2, TYPE_ID mtype, OPS *ops)
{
  TN *t1, *t2;
  TOP fmpy;
  INT exp;
  INT sign;
  INT32 sign_exp;

  /* Convert the divide into a multiply by the reciprocal of the divisor.
   * Only power of two divisors can be inverted exactly.
   *
   * Based on the type, get the sign, exponent and multiply operation to use.
   */
  switch (mtype) {
  case MTYPE_F4:
    {
      float val = TCON_fval(src2);
      UINT32 w = *(UINT32 *)&val;
      exp = ((w >> 23) & 0xff) - E32bias;
      sign = (w >> 31) & 0x1;
      fmpy = TOP_fmpy_s;
    }
    break;
  case MTYPE_F8:
    {
      double val = TCON_dval(src2);
      UINT64 dw = *(UINT64 *)&val;
      exp = ((dw >> 52) & 0x7ff) - E64bias;
      sign = (dw >> 63) & 0x1;
      fmpy = TOP_fmpy_d;
    }
    break;
  case MTYPE_F10:
    {
      // TODO: 80-bit float when targ_const can represent the constants
      fmpy = TOP_fmpy;
    }
    return FALSE;
  }

  /* Handle divisor of +/-1.0 specially.
   */
  if (exp == 0) {
    if (sign == 0) {
      Exp_COPY(result, src1, ops);
    } else {
      Expand_Neg(result, src1, mtype, ops);
    }
    return TRUE;
  }

  /* The exponent is a power of two. To get the reciprocal, we just
   * need to negate it.
   */
  exp = -exp;

  /* Load the reciprocal and do the multiplication.
   */
  sign_exp = (sign << 17) | (exp + E82bias);
  t1 = Build_TN_Of_Mtype(MTYPE_I4);
  Build_OP(TOP_mov_i, t1, True_TN, Gen_Literal_TN(sign_exp, 4), ops);
  CGSPILL_Attach_Intconst_Remat(t1, sign_exp);
  t2 = Build_TN_Of_Mtype(mtype);
  Build_OP(TOP_setf_exp, t2, True_TN, t1, ops);
  CGSPILL_Attach_Floatconst_Remat(t2, mtype, 1.0 / Targ_To_Host_Float(src2));

  Build_OP(fmpy, result, True_TN, Gen_Enum_TN(ECV_sf_s0), src1, t2, ops);

  return TRUE;
}


/* Expansion for floating point division by a constant.
 * Return a boolean that indicates if we were able to generate the
 * expansion.
 */
static BOOL
Expand_Float_Div_By_Const(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  INT Emin, Emax;
  INT sign;
  INT exp;
  UINT64 frac;
  float_rf b;
  WN *home;
  TCON tc;

  /* Get the constant divisor.
   */
  home = TN_home(src2);
  if (WN_operator(home) != OPR_CONST) return FALSE;
  tc = ST_tcon_val(WN_st(home));

  if (   Targ_Is_Power_Of_Two(tc)
      && Expand_Float_Div_By_Pow2(result, src1, tc, mtype, ops))
  {
    return TRUE;
  }

  /* Based on the constant's type, get its sign, unbiased exponent,
   * and fraction. The fraction is shifted so that the binary
   * point is between bit 63 and 62. The ones bit (63) is not set
   * at this point.
   */
  switch (mtype) {
  case MTYPE_F4:
    {
      float val = TCON_fval(tc);
      UINT32 w = *(UINT32 *)&val;
      sign = (w >> 31) & 1;
      exp = ((w >> 23) & 0xff) - E32bias;
      frac = (UINT64)((w >> 0) & 0x7fffff) << 40;
      Emin = E32min;
      Emax = E32max;
    }
    break;
  case MTYPE_F8:
    {
      double val = TCON_dval(tc);
      UINT64 dw = *(UINT64 *)&val;
      sign = (dw >> 63) & 1;
      exp = ((dw >> 52) & 0x7ff) - E64bias;
      frac = ((dw >> 0) & 0xfffffffffffffULL) << 11;
      Emin = E64min;
      Emax = E64max;
    }
    break;
  case MTYPE_F10:
    {
      Emin = E80min;
      Emax = E80max;
    }
    // TODO: 80-bit float when targ_const can represent the constants
    return FALSE;
  default:
    return FALSE;
  }

  /* Init the 82-bit formatted sign, exponent and fraction.
   */
  if (exp >= Emin && exp <= Emax) {
    // normal number
    b.sign = sign;
    b.exp = exp + E82bias;
    b.frac = frac | (1ULL << 63);
  } else if (exp == Emin-1 && frac == 0) {
    // +/-0.0
    b.sign = sign;
    b.exp = E82min-1 + E82bias;
    b.frac = 0;
  } else if (exp == Emin-1 && frac != 0) {
    // denorm
    b.sign = sign;
    b.exp = Emin + E82bias;
    b.frac = frac;
  } else if (exp == Emax+1 && frac == 0) {
    // Infinity
    b.sign = sign;
    b.exp = E82max+1 + E82bias;
    b.frac = 0;
  } else if (exp == Emax+1 && frac != 0) {
    // NaN
    // TODO
    return FALSE;
  } else {
    Is_True(FALSE, ("unable to classify %d %d 0x%016llx", sign, exp, frac));
    return FALSE;
  }

  /* Finally, generate the expansion.
   */
  switch (mtype) {
  case MTYPE_F4:
    Expand_F4_Div_By_Const(result, src1, src2, &b, ops);
    break;
  case MTYPE_F8:
    Expand_F8_Div_By_Const(result, src1, src2, &b, ops);
    break;
  case MTYPE_F10:
    Expand_F10_Div_By_Const(result, src1, src2, &b, ops);
    break;
  }

  return TRUE;
}

/*****************************************************************************
 *
 * Floating-point division external interface
 *
 *****************************************************************************/

void
Expand_Float_Divide(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  static BOOL initialized;
  static void (*f4div)(TN *, TN *, TN *, OPS *) = Expand_SGI_F4_Divide;
  static void (*f8div)(TN *, TN *, TN *, OPS *) = Expand_SGI_F8_Divide;
  static void (*f10div)(TN *, TN *, TN *, OPS *) = Expand_SGI_F10_Divide;

  /* Optimize division by constant if desired and possible.
   */
  if (CGEXP_opt_float_div_by_const) {
    if (src2 == FOne_TN) {
      Exp_COPY(result, src1, ops);
      return;
    } else if (   TN_is_rematerializable(src2) 
	       && Expand_Float_Div_By_Const(result, src1, src2, mtype, ops))
    {
      return;
    }
  }

  if (!initialized) {
    const char * const alg = CGEXP_fdiv_algorithm;
    if (strcasecmp(alg, "intel_max_thr") == 0) {
      f4div = Expand_Intel_Max_Thr_F4_Divide;
      f8div = Expand_Intel_Max_Thr_F8_Divide;
      f10div = Expand_Intel_Max_Thr_F10_Divide;
    } else if (strcasecmp(alg, "intel_min_lat") == 0) {
      f4div = Expand_Intel_Min_Lat_F4_Divide;
      f8div = Expand_Intel_Min_Lat_F8_Divide;
      f10div = Expand_Intel_Min_Lat_F10_Divide;
    } else if (strcasecmp(alg, "sgi") != 0) {
      DevWarn("invalid fdiv algorithm: %s", alg);
    }
    initialized = TRUE;
  }

  switch (mtype) {
  case MTYPE_F4:
    f4div(result, src1, src2, ops);
    break;
  case MTYPE_F8:
    f8div(result, src1, src2, ops);
    break;
  case MTYPE_F10:
    f10div(result, src1, src2, ops);
    break;
  default:
    FmtAssert(FALSE, ("Bad type in Expand_Float_Divide"));
    /*NOTREACHED*/
  }
}
    
void
Expand_Float_Recip(TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  /* The current recip expansion is almost identical to divide, so
   * the divide expansion special cases recip (which is a useful
   * optimization in itself).
   */
  switch (mtype) {
  case MTYPE_F4:
    Expand_F4_Recip(result, src, ops);
    break;
  case MTYPE_F8:
    Expand_F8_Recip(result, src, ops);
    break;
  case MTYPE_F10:
    Expand_F10_Recip(result, src, ops);
    break;
  default:
    FmtAssert(FALSE, ("Bad type in Expand_Float_Recip"));
    /*NOTREACHED*/
  }
}

/*****************************************************************************
 *
 * Integer division internal support routines
 *
 *****************************************************************************/

/* Return values for Check_Divide:
 */
typedef enum {
  DIVCHK_RUNTIME,	// unknown except at run-time
  DIVCHK_BYZERO,	// unconditional div-by-zero
  DIVCHK_OVERFLOW	// unconditional overflow
} DIVCHK_STATUS;

/* Check a divide for undefined operations. The operands are examined
 * to determine if it is known at compile-time that a fault will
 * occur or if it cannot be known until run-time. The return value
 * indicates the situation. In addition, if divide checking is
 * enabled, code is generated to cause an exception.
 */
static DIVCHK_STATUS
Check_Divide (TN *numerator, TN *divisor, TYPE_ID mtype, OPS *ops)
{
  // TODO: don't want to generate checks while using simulator, so reset
  // div-by-zero checking which is on by default.
  BOOL div_zero_check = FALSE; // DEBUG_Div_Zero_Check

  INT64	divisor_val;
  BOOL const_divisor = TN_Value_At_Op (divisor, NULL, &divisor_val);
  BOOL is_double = MTYPE_is_size_double(mtype);

  /* Check for divide-by-zero.
   */
  if (const_divisor) {
    if (divisor_val == 0) {
      if (div_zero_check) {
	Build_OP (TOP_break, True_TN, Gen_Literal_TN(FPE_INTDIV, 4), ops);
      }
      return DIVCHK_BYZERO;
    }
  } else if (div_zero_check) {
    TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    TOP opc = is_double ? TOP_cmp_eq : TOP_cmp4_eq;
    Build_OP (opc, p1, p2, True_TN, divisor, Zero_TN, ops);
    Build_OP (TOP_break, p1, Gen_Literal_TN(FPE_INTDIV, 4), ops);
  }

  /* Check for overflow.
   */
  if (MTYPE_is_signed(mtype)) {
    INT64 numer_val;
    BOOL const_numer = TN_Value_At_Op (numerator, NULL, &numer_val);
    const INT64 minint_val = is_double ? INT64_MIN : INT32_MIN;
    const INT min_tn_size = is_double ? 8 : 4;
    if (const_divisor && const_numer) {
      if (numer_val == minint_val && divisor_val == -1) {
	if (DEBUG_Div_Oflow_Check) {
	  Build_OP (TOP_break, True_TN, Gen_Literal_TN(FPE_INTOVF, 4), ops);
	}
	return DIVCHK_OVERFLOW;
      }
    } else if (DEBUG_Div_Oflow_Check) {

      /* Generate code to test for most-negative-integer divided by -1
       */
      TOP opc;
      TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      if (const_numer) {
	opc = is_double ? TOP_cmp_i_eq : TOP_cmp4_i_eq;
	Build_OP (opc, p1, p2, True_TN, Gen_Literal_TN(-1, 4), divisor, ops);
      } else if (const_divisor) {
	TN *minint = Build_TN_Of_Mtype (mtype);
	Expand_Immediate (minint, Gen_Literal_TN(minint_val, min_tn_size), 
			  TRUE, ops);
	opc = is_double ? TOP_cmp_eq : TOP_cmp4_eq;
	Build_OP (opc, p1, p2, True_TN, numerator, minint, ops);
      } else {
	opc = is_double ? TOP_cmp_i_eq_unc : TOP_cmp4_i_eq_unc;
	Build_OP (opc, p1, p2, True_TN, Gen_Literal_TN(-1, 4), divisor, ops);

	TN *minint = Build_TN_Of_Mtype (mtype);
	Expand_Immediate (minint, Gen_Literal_TN(minint_val, min_tn_size), 
			  TRUE, ops);
	opc = is_double ? TOP_cmp_eq_and : TOP_cmp4_eq_and;
	Build_OP (opc, p1, p2, True_TN, numerator, minint, ops);
      }
      Build_OP (TOP_break, p1, Gen_Literal_TN(FPE_INTOVF, 4), ops);
    }
  }

  return DIVCHK_RUNTIME;
}


/* return TRUE if the val is a power of 2 */
#define IS_POWER_OF_2(val)	((val != 0) && ((val & (val-1)) == 0))

static BOOL Is_Power_Of_2(INT64 val, TYPE_ID mtype)
{
  if (MTYPE_is_signed(mtype) && val < 0) val = -val;

  if (mtype == MTYPE_U4) val &= 0xffffffffull;

  return IS_POWER_OF_2(val);
}


static INT
Get_Power_Of_2 (INT64 val, TYPE_ID mtype)
{
  INT i;
  INT64 pow2mask;

  if (MTYPE_is_signed(mtype) && val < 0) val = -val;

  if (mtype == MTYPE_U4) val &= 0xffffffffull;

  pow2mask = 1;
  for ( i = 0; i < MTYPE_size_reg(mtype); ++i ) {
    if (val == pow2mask) return i;
    pow2mask <<= 1;
  }

  FmtAssert(FALSE, ("Get_Power_Of_2 unexpected value"));
  /* NOTREACHED */
}


/* Expand the sequence for division by a power of two. It is the
 * caller's job to verify that divisor is a non-zero power of two.
 */
static void
Expand_Power_Of_2_Divide (TN *result, TN *numer, INT64 dvsr, TYPE_ID mtype, OPS *ops)
{
  INT n = Get_Power_Of_2(dvsr, mtype);

  if (MTYPE_is_unsigned(mtype)) {
    Expand_Shift(result, numer, Gen_Literal_TN(n, 4), mtype, shift_lright, ops);
  } else {
    TN *t1 = Build_TN_Of_Mtype(mtype);
    TN *t2 = dvsr < 0 ? Build_TN_Of_Mtype(mtype) : result;
    INT64 absdvsr = dvsr < 0 ? -dvsr : dvsr;
    BOOL is_double = MTYPE_is_size_double(mtype);
    if (absdvsr == 2) {

      /* Optimize for abs(divisor) == 2:
       *      extr.u tmp0=numer,signbit,1  -- dvsr-1 if numer negative else 0
       *      add tmp1=tmp0,numer
       *      shr result=tmp1,1
       * if (dvsr<0) sub result=0,result
       */
      TN *t0 = Build_TN_Of_Mtype(mtype);
      Build_OP(TOP_extr_u, t0, True_TN, numer,
	       Gen_Literal_TN(is_double ? 63 : 31, 4),
	       Gen_Literal_TN(1, 4), ops);
      Build_OP(TOP_add, t1, True_TN, t0, numer, ops);
      if (is_double) {
	Build_OP(TOP_shr_i, t2, True_TN, t1, Gen_Literal_TN(1, 4), ops);
      } else {
	/* Perform a 32-bit shr using extr. By not relying on the upper
	 * bits, we may allow the removal of an sxt4 on the numerator.
	 */
	Build_OP(TOP_extr, t2, True_TN, t1,
		 Gen_Literal_TN(1, 4), Gen_Literal_TN(31, 4), ops);
      }
    } else {

      /* General case:
       *      cmp.lt p1,p2=numer,zero         -- numerator negative?
       *      add tmp1=abs(dvsr)-1,numer      -- speculatively add dvsr-1 to numer
       * (p1) shr result=tmp1,pow2(abs(dvsr))
       * (p2) shr result=numer,pow2(abs(dvsr))
       * if (dvsr<0) sub result=0,result
       */
      TN *p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      TN *p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      TOP opc = is_double ? TOP_cmp_lt : TOP_cmp4_lt;
      Build_OP(opc, p1, p2, True_TN, numer, Zero_TN, ops);
      Expand_Add (t1, Gen_Literal_TN(absdvsr-1, is_double ? 8 : 4), 
		  numer, mtype, ops);
      if (is_double) {
	Build_OP(TOP_shr_i, t2, p1, t1, Gen_Literal_TN(n, 4), ops);
	Build_OP(TOP_shr_i, t2, p2, numer, Gen_Literal_TN(n, 4), ops);
      } else {
	/* Perform a 32-bit shr using extr. By not relying on the upper
	 * bits, we may allow the removal of an sxt4 on the numerator.
	 */
	Build_OP(TOP_extr, t2, p1, t1,
	         Gen_Literal_TN(n, 4), Gen_Literal_TN(32-n, 4), ops);
	Build_OP(TOP_extr, t2, p2, numer,
		 Gen_Literal_TN(n, 4), Gen_Literal_TN(32-n, 4), ops);
      }
    }
    if (dvsr < 0) Expand_Neg(result, t2, mtype, ops);
  }
}

#ifdef OSP_OPT
/* Expand the sequence for division and/or remainder by a variable.
 */
static void
Expand_NonConst_DivRem (TN *quot, TN *rem, TN *x, TN *y, TYPE_ID mtype, OPS *ops)
{
  /*	// pc = 64-bit-divide ? <none> : d
   *	(qp) setf.sig	f12=x		# move x to f.p. unit
   *	(qp) setf.sig	f13=y		# move y to f.p. unit
   *	(qp) fcvt.xf	f12=f12		# convert x to f.p.
   *	(qp) fcvt.xf	f13=f13		# convert y to f.p.
   *
   *    if (64 bit-divide) {
   *      do an F10 divide
   *    } else {
   *      do an F8 divide
   *    }
   *	(qp) fcvt.fx.trunc.s1 f2=f2	# convert quot to integer
   *	if (want-remainder) {
   *	  (qp) minus_y = -y;
   *	  (qp) minus_y_in_fp = setf.sig minus_y
   *	  (qp) xma.l f3=f2, minus_y_in_fp, f12
   *	  (qp) getf.sig rem=f3		# move rem to integer unit
   *	}
   *	if (want-quotient) {
   *	  (qp) getf.sig	quot=f2		# move quot to integer unit
   *    }
   */

  INT i;
  TN *tn;
  TN *f2, *f2a, *f2b, *f3, *f4, *f6, *f12, *f13, *x_in_fp;
  BOOL is_recip;
  BOOL is_signed = MTYPE_is_signed(mtype);
  BOOL is_double = MTYPE_is_size_double(mtype) != 0;
  TN * const qp = True_TN;
  TOP fnma = TOP_fnma;
  TOP fma  = TOP_fma; 
  TOP fmpy = TOP_fmpy;

  /* See if the numerator is one:
   */
  is_recip = FALSE;
  if (TN_is_rematerializable(x)) {
    INT64 x_val = WN_const_val(TN_home(x));
    if (x_val == 1) is_recip = TRUE;
  }

  /* Load the demoninator if it's a constant:
   */
  if (TN_has_value(y)) {
    TN *result = quot ? quot : rem;
    TN *tmp = Build_TN_Like(result);
    Expand_Immediate(tmp, y, is_signed, ops);
    y = tmp;
  }


  x_in_fp = f12 = Build_TN_Of_Mtype (MTYPE_F10);
  Build_OP(TOP_setf_sig, f12, qp, x, ops);
  tn = Build_TN_Of_Mtype (MTYPE_F8);
  if (is_signed) {
    Build_OP(TOP_fcvt_xf, tn, qp, f12, ops);
  } else {
    Build_OP(TOP_fcvt_xuf, tn, qp, Gen_Enum_TN(ECV_sf_s0), f12, ops);
  }
  f12 = tn;

  f13 = Build_TN_Of_Mtype (MTYPE_F10);
  Build_OP(TOP_setf_sig, f13, qp, y, ops);
  tn = Build_TN_Of_Mtype (MTYPE_F10);
  if (is_signed) {
    Build_OP(TOP_fcvt_xf, tn, qp, f13, ops);
  } else {
    Build_OP(TOP_fcvt_xuf, tn, qp, Gen_Enum_TN(ECV_sf_s0), f13, ops);
  }
  f13 = tn;

  f2 = Build_TN_Of_Mtype (MTYPE_F10);
  if (is_recip) {
    // Only need to do the first approximation. Either we get the +-1, the 
    // division by 0, or something < 1. The extra add is to make sure the 
    // case of 1, -1 works.
    TN * qpred = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    tn = Build_TN_Of_Mtype (MTYPE_F10);
    Build_OP(TOP_frcpa, tn, qpred , qp,  Gen_Enum_TN(ECV_sf_s0), f12, f13,  ops);
    Build_OP(TOP_fma,   f2, qpred , Gen_Enum_TN(ECV_sf_s1), tn, f12, tn,  ops);
  } else {
    if (is_double) {
	if (mtype == MTYPE_I8)
            Expand_I8_I8_Divide(f2, f12, f13, ops); // Only call fast divide for I8
	else
            Expand_SGI_F10_Divide(f2, f12, f13, ops);
    } else {
	  if (mtype==MTYPE_I1 ||mtype==MTYPE_I2 || mtype==MTYPE_I4 || mtype==MTYPE_U1 ||mtype==MTYPE_U2||mtype==MTYPE_U4)
	  {
		Expand_I4_I4_Divide(f2,f12,f13,ops);
	  }
	  else
	  {
      		Expand_SGI_F8_Divide(f2, f12, f13, ops);
	  }
    }
  }

  f2a = Build_TN_Of_Mtype (MTYPE_F10);
  Build_OP(is_signed ? TOP_fcvt_fx_trunc : TOP_fcvt_fxu_trunc,
	   f2a, qp, Gen_Enum_TN(ECV_sf_s1), f2, ops);

  if (rem) {
      TN *minus_y = Build_TN_Like (y);
      TN *minus_y_in_fp = Build_TN_Of_Mtype (MTYPE_F10);
      f3 = Build_TN_Of_Mtype (MTYPE_F10);

      Build_OP(TOP_sub, minus_y, qp, Zero_TN, y, ops);
      Build_OP(TOP_setf_sig, minus_y_in_fp, qp, minus_y, ops);
      Build_OP(TOP_xma_l, f3, qp, f2a, minus_y_in_fp, x_in_fp, ops);
      Build_OP(TOP_getf_sig, rem, qp, f3, ops);
  }

  if (quot) {
    Build_OP(TOP_getf_sig, quot, qp, f2a, ops);
  }
}
#endif

/******************************************************************************
 *
 *   Function Name: determine_pseudo_inverse
 *
 *   Author: Bill Homer
 *
 *   Input Parameters: b              constant divisor
 *                     maxbits_in_a   size of dividend
 *
 *   Returns:          pseudo inverse
 *                     pn             smallest n, such that 2^n >= b
 *
 *   Description:
 *   Given an unsigned integer, calculate a "pseudo-inverse"
 *   (which is the return value) and the associated shift width
 *   (which is returned via the third parameter).
 *
 *   Usage:
 *    Let BPUL be the number of bits in an unsigned long.
 *    When b is a compile time constant, optimize an unsigned
 *    long integer division on T90,IEEE
 *                                    q = a/b
 *    by replacing it with:
 *
 *    Case 1) b == 2^n                q = b>>n
 *            Not done here.
 *
 *    Case 2) b >= 2^(BPUL-1)         q = (a >= b)
 *
 *    Case 3) a, b < 2^(BPUL/2)
 *            Not used; might be a good way to handle 32 bit ints.
 *            At compile time:        d = (~0UL)/b
 *            At run time:            q = int_mult_upper(d,a)
 *
 *    Case 4) Not used - general case; longer code than 5) & 6)
 *
 *    Case 5) a < 2^(BPUL-1)
 *            Used for 32 and 64 bit signed ints.
 *            At compile time:        d = determine_pseudo_inverse(b,BPUL-1,&n)
 *            At run time:            q = int_mult_upper(d,a) >> (n-1)
 *
 *    Case 6) default
 *            Used for unsigned 32 and 64 bit ints.
 *            At compile time:        d = determine_pseudo_inverse(b,BPUL,&n)
 *            At run time:            p = int_mult_upper(d,a)
 *                                    q = (p + ((a-p)>>1)) >> (n-1)
 *
 ******************************************************************************/

static UINT64 determine_pseudo_inverse (
  UINT64      b,
  INT64       maxbits_a,
  INT64      *pn)
{
  INT64  i, n;
  UINT64 m, q, b1;

  /*  Calculate the smallest n such that 2^n >= b,
   *  and the corresponding m = 2^n - 1
   *  (which satisfies m >= b - 1).
   */
  b1 = b - 1;
  n = 1;
  m = 1;
  while(m < b1) {
    n++;
    m = (m<<1) | 1;
  }
  *pn = n;

  /*  Calculate the "pseudo-inverse" of b, which is
   *  the ceiling of 2^(n+maxbits_a) divided by b, or
   *     d = 1 + (2^(n+maxbits_a) - 1) / b
   *  Because 2^n >=  b, d >= 2^maxbits_a, and
   *  because 2^n < 2*b, d <  2^(maxbits_a+1).
   *  Therefore d occupies (maxbits_a+1) bits,
   *  and its top bit is 1.
   *  Return value is:
   *     d         if maxbits_a  < BPUL (bits per unsigned long)
   *     d-2^BPUL  if maxbits_a == BPUL (i.e., all but top bit)
   */
  BOOL m_overflow = FALSE;
  for(q=i=0; i<=maxbits_a; i++) {
    q <<= 1;
    if(m_overflow) {
      // because ((m>>1) | 0x8000000000000000ULL) >= m
      // if m>=b in this iteration, then in last iteration m also >= b. 
      // This can't happen
      Is_True(m < b, ("m bigger than b and m is overflow in last iteration\n"));
      m -= b;
      q |= 1;
      m_overflow = FALSE;
    }
    else if(m >= b) {
      m -= b;
      q |= 1;
    }
    // After subtraction, m must be smaller than b. And m's 64 bit MSB must be zero.
    // if m's 64bit MSB is 1, then subtraction not happen in this iteration.
    // it means b>m, then b's 64 bit MSB is also 1.
    // Mark m overflow and in next iteration, actually m is bigger than b.
    // Need do subtraction in next itration.
    if (m & 0x8000000000000000ULL) {
      Is_True(b & 0x8000000000000000ULL, ("b's 64th bit must be 1\n"));
      m_overflow = TRUE;
    }
    m = (m<<1) | 1;
  }
  return 1+q;
}


/* Expand the sequence for division by a constant. It is the caller's
 * job to verify that the divisor is non-zero.
 */
static BOOL
Expand_Integer_Divide_By_Constant(TN *result, TN *numer_tn, INT64 denom_val,
				  TYPE_ID mtype, OPS *ops)
{
  UINT64 b;				// b = |denom_val|
  UINT64 d;      			// division scaling factor
  INT64  precision_required;
  INT64  n;
  BOOL is_odd;
  TN *d_tn;
  TN *abs_tn;
  TN *mult_tn;
  TN *shift_tn;
  TOP opc;
  TN *p1, *p2;
  BOOL is_double = MTYPE_is_size_double(mtype);
  BOOL is_signed = MTYPE_is_signed(mtype);

  /* Handle the trivial ones:
   */
  if (denom_val == 1) {
    Exp_COPY(result, numer_tn, ops);
    return TRUE;
  } else if (is_signed && denom_val == -1) {
    Expand_Neg(result, numer_tn, mtype, ops);
    return TRUE;
  }

  /* Look for simple shift optimizations:
   */
  if (Is_Power_Of_2( denom_val, mtype)) {
    Expand_Power_Of_2_Divide(result, numer_tn, denom_val, mtype, ops);
    return TRUE;
  }

  if (!CGEXP_cvrt_int_div_to_mult) return FALSE;

  if (is_signed) {

    b = denom_val<0 ? -denom_val : denom_val;       // b = |denom_val|
    is_odd = (b&1);

    d = determine_pseudo_inverse (b, is_double ? 63 : 31, &n);

    if (n > (is_double ? 63 : 31)) {
      /* OOPS! The shift count can't be bigger than the word size! */
      return FALSE;
    }

    d_tn = Build_TN_Like (result);
    Expand_Immediate (d_tn, 
		      Gen_Literal_TN (is_double ? d : d << 32, 8),
		      is_signed, ops);

    /* Generate the absolute value of the numerator:
     */
    abs_tn = Build_TN_Of_Mtype (mtype);
    p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    opc = is_double ? TOP_cmp_lt : TOP_cmp4_lt;
    Build_OP (opc, p1, p2, True_TN, numer_tn, Zero_TN, ops);
    Build_OP (TOP_sub, abs_tn, p1, Zero_TN, numer_tn, ops);
    Build_OP (TOP_mov, abs_tn, p2, numer_tn, ops);

    /* Generate a multiply upper:
     */
    mult_tn = Build_TN_Of_Mtype (mtype);
    Expand_High_Multiply (mult_tn, abs_tn, d_tn, MTYPE_U8, ops);

    /* Generate and attach the shift:
     */
    if (n > 0) {
      shift_tn = Build_TN_Of_Mtype (mtype);
      Build_OP (TOP_shr_i_u, shift_tn, True_TN, mult_tn, Gen_Literal_TN (n-1, 4), ops);
    } else {
      shift_tn = mult_tn;
    }

    /* Select positive or negated result:
     */
    if (denom_val < 0) {
      TN *tmp = p1;
      p1 = p2;
      p2 = tmp;
    }
    Build_OP (TOP_sub, result, p1, Zero_TN, shift_tn, ops);
    Build_OP (TOP_mov, result, p2, shift_tn, ops);
  } /* end Signed */

  else { /* Unsigned */

    b = denom_val;
    is_odd = (b&1);

    /* Full precision calculation is required.
     */
    if (is_double) {
      if (is_odd) {
	precision_required = 64;
      } else {
      
        /* Pre-shift the numerator and denominator so that
           one less bit is required in the calculation. Then
           we can avoid the subtract-shift-add after the 
           multiply operation. */
	b >>= 1;
	precision_required = 63;
	
	/* Pre-shift to simplify later calculations.
	 */
	TN *tmp1_tn = Build_TN_Of_Mtype (mtype);
	Build_OP (TOP_shr_i_u, tmp1_tn, True_TN, numer_tn, Gen_Literal_TN (1, 4), ops);
	numer_tn = tmp1_tn;
      }
    } else{
      precision_required = 63;
    }

    d = determine_pseudo_inverse (b, precision_required, &n);

    if (n > precision_required) {
      /* OOPS! The shift count can't be bigger than the word size! */
      return FALSE;
    }

    d_tn = Build_TN_Like (result);

    Expand_Immediate (d_tn, Gen_Literal_TN (d, 8), is_signed, ops);

    /* Generate a multiply upper:
     */
    mult_tn = Build_TN_Of_Mtype (mtype);
    
    Expand_High_Multiply (mult_tn, numer_tn, d_tn, MTYPE_U8, ops);

    if (precision_required == 64) {

      /* Odd divisors need full precision and, hence, extra instructions.
       */
      TN *tmp1_tn = Build_TN_Of_Mtype (mtype);
      TN *tmp2_tn = Build_TN_Of_Mtype (mtype);
      TN *tmp3_tn = Build_TN_Of_Mtype (mtype);
      Build_OP (TOP_sub, tmp1_tn, True_TN, numer_tn, mult_tn, ops);
      Build_OP (TOP_shr_i_u, tmp2_tn, True_TN, tmp1_tn, Gen_Literal_TN (1, 4), ops);
      Build_OP (TOP_add, tmp3_tn, True_TN, mult_tn, tmp2_tn, ops);
      mult_tn = tmp3_tn;
    }

    /* Generate and attach the shift:
     */
    Build_OP (TOP_shr_i_u, result, True_TN, mult_tn,
	      Gen_Literal_TN (n-1, 4),
	      ops);
    
  } /* end Unsigned */

  return TRUE;
}


/* Expand the sequence for remainder of a power of two. It is the
 * caller's job to verify that divisor is a non-zero power of two.
 *
 * Expand rem(x, [+-]2^n) as follows:
 *
 *	Using the identities
 *		rem(x,y) =  rem( x,-y)
 *		rem(x,y) = -rem(-x, y)
 *
 *	unsigned
 *	f=	x & MASK(n)
 *
 *	signed
 *	f=	x & MASK(n)		x>=0
 *	f=	-(-x & MASK(n))		x<0
 */
static void 
Expand_Power_Of_2_Rem (TN *result, TN *src1, INT64 src2_val, TYPE_ID mtype, OPS *ops)
{
  BOOL is_double = MTYPE_is_size_double(mtype);
  INT n = Get_Power_Of_2(src2_val, mtype);
  INT64 nMask = (1LL << n) - 1;
  TN *con = Gen_Literal_TN(nMask, is_double ? 8 : 4);

  if (MTYPE_is_signed(mtype)) {
    TN *tmp1, *tmp2;
    TN *p1, *p2;
    TOP opc;

    /* Test sign of src1
     */
    p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    p2 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
    opc = is_double ? TOP_cmp_lt : TOP_cmp4_lt;
    Build_OP(opc, p1, p2, True_TN, src1, Zero_TN, ops);

    /* Get absolute value of src1
     */
    tmp1 = Build_TN_Of_Mtype(mtype);
    Build_OP(TOP_sub, tmp1, p1, Zero_TN, src1, ops);
    Build_OP(TOP_mov, tmp1, p2, src1, ops);

    /* Perform the AND
     */
    tmp2 = Build_TN_Of_Mtype(mtype);
    Expand_Binary_And(tmp2, tmp1, con, mtype, ops);

    /* Negate the result if src1 was negative
     */
    Build_OP(TOP_sub, result, p1, Zero_TN, tmp2, ops);
    Build_OP(TOP_mov, result, p2, tmp2, ops);
  } else {
    Expand_Binary_And(result, src1, con, mtype, ops);
  }
}


/* Expand the sequence for mod of a power of two. It is the
 * caller's job to verify that divisor is a non-zero power of two.
 *
 * Expand mod(x, [+-]2^n) as follows:
 *
 *	Using the identity
 *		mod(x,y) = -mod(-x, y)
 *	
 *	f=	x & MASK(n)		(2^n > 0) || unsigned
 *
 *	t=	-(-x & MASK(n))		(2^n < 0)
 *
 *	Special case for n=31 and n=63 return zero
 */
static void 
Expand_Power_Of_2_Mod (TN *result, TN *src1, INT64 src2_val, TYPE_ID mtype, OPS *ops)
{
  BOOL is_double = MTYPE_is_size_double(mtype);
  INT64 absval = src2_val < 0 ? -src2_val : src2_val;
  INT	n      = Get_Power_Of_2(absval, mtype);
  INT64	nMask  = (1LL << n) - 1;
  TN	*con   = Gen_Literal_TN(nMask, is_double ? 8 : 4);

  if (MTYPE_is_signed(mtype) && src2_val < 0) {
    TN *tmp1, *tmp2;

    tmp1 = Build_TN_Of_Mtype(mtype);
    Expand_Neg(tmp1, src1, mtype, ops);

    tmp2 = Build_TN_Of_Mtype(mtype);
    Expand_Binary_And(tmp2, tmp1, con, mtype, ops);

    Expand_Neg(result, tmp2, mtype, ops);
  } else {
    Expand_Binary_And(result, src1, con, mtype, ops);
  }
}

/*****************************************************************************
 *
 * Integer division external interfaces
 *
 *****************************************************************************/

TN *
Expand_Divide (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  /* Check for undefined operations we can detect at compile-time
   * and when enabled, generate run-time checks.
   */
  switch (Check_Divide(src1, src2, mtype, ops)) {
  case DIVCHK_BYZERO:
  case DIVCHK_OVERFLOW:
    Build_OP(TOP_ifixup, result, ops);
    return NULL;
  }

  /* Look for simple shift optimizations and multiply_hi optimizations:
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src2) {
    if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops)) {
      return NULL; // no hilo
    }
  }

  Expand_NonConst_DivRem(result, NULL, src1, src2, mtype, ops);
  return NULL;  // no hilo
}


void
Expand_Rem (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops, OPCODE opcode)
{
  /* Check for undefined operations we can detect at compile-time
   * and when enabled, generate run-time checks.
   */
  switch (Check_Divide(src1, src2, mtype, ops)) {
  case DIVCHK_BYZERO:
  case DIVCHK_OVERFLOW:
    Build_OP(TOP_ifixup, result, ops);
    return;
  }

  /* Try to optimize when constant divisor.
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src2) {

    /* Handle powers of 2 specially.
     */
    if (Is_Power_Of_2(src2_val, mtype)) {
      Expand_Power_Of_2_Rem(result, src1, src2_val, mtype, ops);
      return;
    }

    if (CGEXP_cvrt_int_div_to_mult) {
      TN *div_tn = Build_TN_Like (result);

      if (Expand_Integer_Divide_By_Constant(div_tn, src1, src2_val, mtype, ops)) {
	TN *mult_tn;

	/* Generate a multiply:
	 */
	mult_tn = Build_TN_Like (result);
	Expand_Multiply(mult_tn, div_tn, src2, mtype, ops, opcode);

	/* Subtract the result of the multiply from the original value.
	 */
	Build_OP(TOP_sub, result, True_TN, src1, mult_tn, ops);
	return;
      }
    }
  }

  Expand_NonConst_DivRem(NULL, result, src1, src2, mtype, ops);
}


/*	Expand mod(x,y) as follows:
 *		t1=	rem(x,y)
 *		t2=	xor(t1,y)
 *		t3,t4=	cmp.lt(t2,0)
 *	  if t3 r=	t1+y
 *	  if t4 r=	t1
 */
void 
Expand_Mod (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops, OPCODE opcode)
{
  TN *tmp1;
  TN *tmp2;
  TN *p1;
  TN *p2;
  TOP opc;
  BOOL is_double = MTYPE_is_size_double(mtype);

  /* Check for undefined operations we can detect at compile-time
   * and when enabled, generate run-time checks.
   */
  switch (Check_Divide(src1, src2, mtype, ops)) {
  case DIVCHK_BYZERO:
  case DIVCHK_OVERFLOW:
    Build_OP(TOP_ifixup, result, ops);
    return;
  }

  /* Handle mod by power of 2 specially
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src2 && Is_Power_Of_2(src2_val, mtype)) {
    Expand_Power_Of_2_Mod (result, src1, src2_val, mtype, ops);
    return;
  }

  /* Calculate remainder 
   */
  tmp1 = Build_TN_Like(result);
  Expand_Rem(tmp1, src1, src2, mtype, ops, opcode);

  /* Are signs different? 
   */
  tmp2 = Build_TN_Like(result);
  Build_OP(TOP_xor, tmp2, True_TN, tmp1, src2, ops);

  p1 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  p2 = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  opc = is_double ? TOP_cmp_lt : TOP_cmp4_lt;
  Build_OP(opc, p1, p2, True_TN, tmp2, Zero_TN, ops);

  /* result = divisor + remainder if p1
   * result = remainder if p2
   */
  Build_OP(TOP_add, result, p1, src2, tmp1, ops);
  Build_OP(TOP_mov, result, p2, tmp1, ops);
}


void 
Expand_DivRem(TN *result, TN *result2, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops, OPCODE opcode)
{
  /* Check for undefined operations we can detect at compile-time
   * and when enabled, generate run-time checks.
   */
  switch (Check_Divide(src1, src2, mtype, ops)) {
  case DIVCHK_BYZERO:
  case DIVCHK_OVERFLOW:
    Build_OP(TOP_ifixup, result, ops);
    Build_OP(TOP_ifixup, result2, ops);
    return;
  }

  /* Usually we expect whirl operators to be folded where possible.
   * But divrem is an odd beast in that the result is a special
   * "handle" rather than a value. There is no way to produce constants.
   * Therefore in some odd instances we can get constant operands,
   * so fold them here, avoiding nasty trapping issues.
   */
  INT64 src1_val;
  BOOL const_src1 = TN_Value_At_Op (src1, NULL, &src1_val);
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src1 && const_src2) {
    INT64 quot_val, rem_val;
    switch (mtype) {
    case MTYPE_I8:
      quot_val = (INT64)src1_val / (INT64)src2_val;
      rem_val = (INT64)src1_val % (INT64)src2_val;
      break;
    case MTYPE_U8:
      quot_val = (UINT64)src1_val / (UINT64)src2_val;
      rem_val = (UINT64)src1_val % (UINT64)src2_val;
      break;
    case MTYPE_U4:
      quot_val = (UINT32)src1_val / (UINT32)src2_val;
      rem_val = (UINT32)src1_val % (UINT32)src2_val;
      break;
    case MTYPE_I4:
      quot_val = (INT32)src1_val / (INT32)src2_val;
      rem_val = (INT32)src1_val % (INT32)src2_val;
      break;
    }
    BOOL is_signed = MTYPE_is_signed(mtype);
    INT tn_size = MTYPE_is_size_double(mtype) ? 8 : 4;
    Exp_Immediate(result, Gen_Literal_TN(quot_val, tn_size), is_signed, ops);
    Exp_Immediate(result2, Gen_Literal_TN(rem_val, tn_size), is_signed, ops);
    return;
  }

  /* Look for simple shift optimizations and multiply_hi optimizations:
   */
  if (const_src2) {
    if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops)) {

      // Now get the rem part. Since the constant value is probably small, 
      // we are unlikely to generate a multiply here (i.e., we'll probably
      // generate shifts)
      //
      if (!MTYPE_is_signed(mtype) && Is_Power_Of_2(src2_val, mtype)) {
        Expand_Power_Of_2_Rem(result2, src1, src2_val, mtype, ops);
      } else {
	TN *t1 = Build_TN_Like(result);
	Expand_Multiply(t1, result, src2, mtype, ops, opcode);
	Build_OP(TOP_sub, result2, True_TN, src1, t1, ops);
      }
      return;
    }
  }
  Expand_NonConst_DivRem(result, result2, src1, src2, mtype, ops);
}
