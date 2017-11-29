/*
 * Copyright (C) 2008 PathScale, LLC.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: cwh_intrin
 * $Revision: 1.9 $
 * $Date: 05/11/08 23:33:31-08:00 $
 * $Author: scorrell@soapstone.internal.keyresearch.com $
 * $Source: 
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Process most of the intrinsic type nodes
 * 
 *              
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_intrin.cxx $ $Revision: 1.9 $";
#endif /* _KEEP_RCS_ID */

/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "stab.h"
#include "strtab.h"
#include "erfe90.h"
#include "errors.h"
#include "config_targ.h"
#include "wn.h"
#include "wn_util.h"
#include "const.h"
#include "wintrinsic.h"
#include "f90_utils.h"

/* Cray includes */

#include "i_cvrt.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_addr.h"
#include "cwh_block.h"
#include "cwh_stk.h"
#include "cwh_types.h"
#include "cwh_expr.h"
#include "cwh_stmt.h"
#include "cwh_preg.h"
#include "cwh_stab.h"
#include "cwh_intrin.h"
#include "cwh_intrin.i"

/*
 * Used by RANGET and RANSET and RTC and UNIT and LENGTH and GETPOS
 * and OMP_SET_LOCK, OMP_UNSET_LOCK, and OMP_TEST_LOCK
 */
static ST *ranget_st = NULL;
static ST *ranset_st = NULL;
static ST *rtc_st = NULL;
static ST *unit_st = NULL;
static ST *length_st = NULL;
static ST *getpos_st = NULL;
static ST *omp_set_lock_st=NULL;
static ST *omp_unset_lock_st=NULL;
static ST *omp_test_lock_st=NULL;
#ifdef KEY /* Bug 1324 */
static ST *erf_st = NULL;
static ST *erfc_st = NULL;
static ST *derf_st = NULL;
static ST *derfc_st = NULL;
#endif /* KEY Bug 1324 */

/*================================================================
 *
 * cwh_intrin_get_return_value
 *
 * Copy the return value of a function into a PREG, and return a 
 * load of that PREG.
 *
 *================================================================
*/
static WN * 
cwh_intrin_get_return_value(TYPE_ID rtype, const char *name )
{
   PREG_NUM rpreg;
   PREG_det rpreg_det;
   WN *wn;

   rpreg_det = cwh_preg_next_preg (rtype,name,NULL);
   rpreg = rpreg_det.preg;
   wn = cwh_expr_operand(NULL);
   wn = WN_StidPreg(rtype,rpreg,wn);
   cwh_block_append(wn);
   wn = WN_LdidPreg(rtype,rpreg);
   return (wn);
}


/* Utility to get a t_enum number from an MTYPE */
static t_enum t_from_mtype(TYPE_ID ty) 
{
   t_enum t;
   t = t_BAD;
   switch (ty) {
    case MTYPE_I1: t = t_I1; break;
    case MTYPE_I2: t = t_I2; break;
    case MTYPE_I4: t = t_I4; break;
    case MTYPE_I8: t = t_I8; break;
    case MTYPE_F4: t = t_F4; break;
    case MTYPE_F8: t = t_F8; break;
    case MTYPE_FQ: t = t_FQ; break;
    case MTYPE_C4: t = t_C4; break;
    case MTYPE_C8: t = t_C8; break;
    case MTYPE_CQ: t = t_CQ; break;
    default: Fail_FmtAssertion(("Bad MTYPE %d seen in t_from_mtype"),ty);
   }
   return (t);
}

/*================================================================
 *
 * cwh_intrin_null_parm
 *
 * Create a VPARM node with 0 children. We need to do it this way, 
 * because the Whirl node creation functions won't create a PARM
 * without children
 *
 *================================================================
*/
static WN * 
cwh_intrin_null_parm(void)
{
   WN *wn;

   wn = WN_CreateParm(MTYPE_V,WN_Zerocon(MTYPE_I4),Be_Type_Tbl(MTYPE_V),0);
   return (wn);
}

/*================================================================
 *
 * cwh_intrin_make_intrinsic_symbol(char *name, TYPE_ID ty)
 *
 * Build an ST for a symbol used in a call. The name of the symbol will
 * be name, and the type will be function returning a ty.
 *
 *================================================================
*/
extern
ST * cwh_intrin_make_intrinsic_symbol(const char *name, TYPE_ID bt) 
{
   ST *st;
   st = cwh_stab_mk_fn_0args(name, EXPORT_PREEMPTIBLE, GLOBAL_SYMTAB + 1,
			     Be_Type_Tbl(bt));
   return (st);
}

/*===============================================
 *
 * cwh_intrin_wrap_value_parm
 *
 * Wrap a by-value OPC_PARM around an argument
 *
 *===============================================
 */ 
extern WN * 
cwh_intrin_wrap_value_parm(WN *w)
{
  TYPE_ID t;
  WN *r;
  TY_IDX ty;

  if (WNOPR(w) == OPR_PARM) {
     return (w);
  }

  t  = WN_rtype(w);
  if (t == MTYPE_M) {
     /* we need to be more sophisticated so we can 
      * get the precise type that belongs on the PARM node.
      */
     ty = cwh_types_WN_TY(w,FALSE);
  } else {
     ty = Be_Type_Tbl(t);
  }

  r = WN_CreateParm(t,w,ty,WN_PARM_BY_VALUE);
  return (r);
}

/*===============================================
 *
 * cwh_intrin_wrap_ref_parm
 *
 * Wrap a by-reference OPC_PARM around an argument
 * If the TY is provided, use that to form the PARM's
 * type, otherwise use the TY of the WN. 
 *
 * The PARM's rty is always Pointer_Mtype. (Sometimes
 * we wrap a function result which gets a temp later
 * in the lowering ie: wa is  not an address..)
 *
 *===============================================
 */ 
extern WN * 
cwh_intrin_wrap_ref_parm(WN *wa, TY_IDX ty)
{
  WN * wn  ;


  if (ty == 0) 
    ty = cwh_types_WN_TY(wa,TRUE);

  wn = WN_CreateParm (Pointer_Mtype,
		      wa, 
		      ty,
		      WN_PARM_BY_REFERENCE);

  return(wn);
}

/*===============================================
 *
 * cwh_intrin_wrap_char_parm
 *
 * Wrap a by-reference OPC_PARM around an argument
 * It's the address of a character object, so the
 * type of the OPC_PARM has to be a pointer to 
 * a string of the correct length, and this is
 * created from the SZ argument. 
 *
 *===============================================
 */ 
extern WN * 
cwh_intrin_wrap_char_parm(WN *wa, WN *sz )
{
  WN * wn  ;
  TY_IDX  ty  ;


  DevAssert((sz != NULL),("Bad PARM TY"));
  ty = cwh_types_ch_parm_TY(sz);
  
  wn = cwh_intrin_wrap_ref_parm(wa,ty);

  return(wn);
}

/*===============================================
 *
 * simple_intrinsic
 *
 *  Helper routine for processing intrinsics
 *  Does a lookup in the intrinsics table, and 
 *  builds either an intrinsic op or the appropriate 
 *  WHIRL node. There is an assumption the arguments
 *  are passed by value. Temps for results would be
 * required otherwise.
 *
 *===============================================
 */
static void 
simple_intrinsic(i_enum intrin, TYPE_ID bt, INT numargs, INT numpop)
{
   OPCODE  opc ;
   INTRINSIC intr;
   WN *k[3];
#ifdef KEY /* Bug 10177 */
   WN *wn  = 0;
#else /* KEY Bug 10177 */
   WN *wn  ;
#endif /* KEY Bug 10177 */
   INT i;
   TYPE_ID t;
   WN *ae=NULL;

   DevAssert((numargs <= 3),("Can't handle that many arguments"));

   intr = GET_ITAB_IOP(intrin,bt);
   opc  = GET_ITAB_WOP(intrin,bt);
   
   DevAssert((opc || intr),("Unsupported intr/ty combo"));

   /* Remove spurious arguments */

   for (i = 0; i < numpop; i++) {
      WN_DELETE_Tree(cwh_expr_operand(NULL));
   }

   for (i = numargs-1; i >= 0; i--) 
     k[i] = cwh_expr_operand(&ae);

   /* Make sure types of arguments all match */
   t = WN_rtype(k[0]);
   for (i = 1; i < numargs; i++) {
      if (WNRTY(k[i]) != t) {
	 k[i] = cwh_convert_to_ty(k[i],t);

         if (intr)
	   k[i] = WN_CreateParm(t,k[i],Be_Type_Tbl(t),WN_PARM_BY_VALUE);
      }
   }

   if (intr) 
      wn = cwh_intrin_build(k,intr,bt,numargs);

   else {
      switch (numargs) {
       case 1:
	 wn = WN_CreateExp1(opc,k[0]);
	 break;
       case 2:
	 wn = WN_CreateExp2(opc,k[0],k[1]);
	 break;
       case 3:
	 wn = WN_CreateExp3(opc,k[0],k[1],k[2]);
	 break;
      }
   }

   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}   

/*===============================================
 *
 * simple_intrinsic_nt
 *
 * Does a lookup in the intrinsics table, and 
 * builds either an intrinsic op or 
 * the appropriate WHIRL node. The result TY 
 * is that of the first operand ie: last of
 * those read from the stack.
 *
 * Args passed by value.
 *
 *===============================================
 */
static void 
simple_intrinsic_nt(i_enum intrin, INT numargs, INT numpop)
{
   OPCODE  opc ;
   INTRINSIC intr;
   WN *k[3];
   WN *wn  ;
   INT i;
   TYPE_ID bt;
   WN *ae=NULL;

   DevAssert((numargs <=3),("Can't handle that many arguments"));
   /* Remove spurious arguments */
   for (i = 0; i < numpop; i++) {
      WN_DELETE_Tree(cwh_expr_operand(NULL));
   }

   for (i = numargs-1; i >= 0; i--) {
      k[i] = cwh_expr_operand(&ae);
   }

   bt = WN_rtype(k[0]);
   
   intr = GET_ITAB_IOP(intrin,bt);
   opc  = GET_ITAB_WOP(intrin,bt);
   
   DevAssert((opc || intr),("Unsupported intr/ty combo"));

   if (intr) 
      wn = cwh_intrin_build(k,intr,bt,numargs);

   else {
      switch (numargs) {
       case 1:
	 wn = WN_CreateExp1(opc,k[0]);
	 break;
       case 2:
	 wn = WN_CreateExp2(opc,k[0],k[1]);
	 break;
       case 3:
	 wn = WN_CreateExp3(opc,k[0],k[1],k[2]);
	 break;
      }
   }

   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}   

/*===============================================
 *
 * do_simple & do_simple_nt
 *
 * macro definitions to invoke utility routines
 * simple_intrinsic or simple_intrinsic_nt. If
 * the type of the result is inferred from the
 * intrinsic table use do_simple, if from the 
 * first operand, use do_simple_nt.
 *
 *===============================================
 */
#define do_simple(name,numargs,numpop) void fei_##name(TYPE type) \
{simple_intrinsic(i_##name,TY_mtype(cast_to_TY(t_TY(type))),numargs,numpop);}

#define do_simple_nt(name,numargs,numpop) void fei_##name(void) \
{simple_intrinsic_nt(i_##name,numargs,numpop);}

do_simple(acos,1,0)
do_simple(asin,1,0)
do_simple(atan,1,0)
do_simple(atan2,2,0)
do_simple(conjg,1,0)
do_simple(cos,1,0)
do_simple(cosh,1,0)
do_simple(exp,1,0)
do_simple_nt(fraction,1,0)
do_simple(ishftc,3,0)
do_simple(log,1,0)
do_simple(log10,1,0)
do_simple(mod,2,0)
do_simple(modulo,2,0) 
do_simple(nextafter,2,0)
do_simple(rrspace,1,1)
#ifdef KEY /* Bug 9140 */
do_simple(sign_xfer,2,0)
#endif /* KEY Bug 9140 */
do_simple(sin,1,0)
do_simple(sinh,1,0)
do_simple(space,1,1)
do_simple(sqrt,1,0)
do_simple(tan,1,0)
do_simple(tanh,1,0)

do_simple(acosd,1,0)
do_simple(asind,1,0)
do_simple(atand,1,0)
do_simple(atan2d,2,0)
do_simple(cosd,1,0)
do_simple(sind,1,0)
do_simple(tand,1,0)


/********************************************************************

Unfortunately, we need to hand-generate most of these following

*********************************************************************/

/*===============================================
 *
 * fei_complex
 *
 * CMPLX intrinsic. A special case because
 * result and operands are not same type
 * and we might need a conversion. Integer
 * constant arguments have already been converted
 * to floats.
 *
 *===============================================
 */ 
void 
fei_complex(TYPE type) 
{
  TYPE_ID br ;
#ifdef KEY /* Bug 10177 */
  TYPE_ID bt  = 0;
#else /* KEY Bug 10177 */
  TYPE_ID bt ;
#endif /* KEY Bug 10177 */
  WN *k[2]   ;
  WN * wn    ;
  INT i      ;
  WN *ae=NULL;

  OPCODE opc ;

  k[1] = cwh_expr_operand(&ae);
  k[0] = cwh_expr_operand(&ae);
  br   = TY_mtype(cast_to_TY(t_TY(type))) ;
  opc  = GET_ITAB_WOP(i_complex,br);

  for (i = 0 ; i < 2 ; i ++ ) {

    switch (br) {
    case MTYPE_C4: bt = MTYPE_F4; break;
    case MTYPE_C8: bt = MTYPE_F8; break;
    case MTYPE_CQ: bt = MTYPE_FQ; break;
    }

    k[i] = cwh_convert_to_ty(k[i],bt);
  }

  wn = WN_CreateExp2(opc,k[0],k[1]);
  wn = cwh_expr_restore_arrayexp(wn,ae);
  cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * fei_abs
 *
 * ABS intrinsic. A special case because
 * operator depends on result and argument.
 * If the argument is complex, then pick
 * up a special operator, otherwise look
 * up in the intrinsic table. Argument is
 * TOS so pop & examine.
 *
 *===============================================
 */ 
void 
fei_abs(TYPE type) 
{
  TYPE_ID ba ;
  TYPE_ID br ;
  TY_IDX ty ;
  WN     *wn ;
  WN *ae=NULL;

#ifdef KEY /* Bug 10177 */
  INTRINSIC intr = INTRN_I4EXPEXPR;
#else /* KEY Bug 10177 */
  INTRINSIC intr;
#endif /* KEY Bug 10177 */

  wn = cwh_expr_operand(&ae);
  ty = cwh_types_WN_TY(wn,FALSE);
  ty = cwh_types_scalar_TY(ty);
  ba = TY_mtype(ty);
  br = TY_mtype(cast_to_TY(t_TY(type))) ;

  if (MTYPE_is_complex(ba)) {
    switch(ba) {
    case MTYPE_C4: intr = INTRN_F4C4ABS ; break;
    case MTYPE_C8: intr = INTRN_F8C8ABS ; break;
    case MTYPE_CQ: intr = INTRN_FQCQABS ; break;
      
    }
    wn = cwh_intrin_build(&wn,intr,br,1);
    wn = cwh_expr_restore_arrayexp(wn,ae);
    cwh_stk_push(wn,WN_item);

  } else {
     wn = cwh_wrap_cvtl(wn,br);
     wn = cwh_expr_restore_arrayexp(wn,ae);
     cwh_stk_push(wn,WN_item);
     simple_intrinsic(i_abs,br,1,0);
  }
}

/*
 * Cotangent
 */

void 
fei_cot(TYPE type)
{
   WN *one, *wn;

   fei_tan(type);
   wn = cwh_expr_operand(NULL);
   one = WN_Intconst(MTYPE_I4,1);
   cwh_stk_push(one,WN_item);
   cwh_stk_push(wn,WN_item);
   fei_div(type);
}

void 
fei_exponentiate(TYPE type) 
{
   
   TYPE_ID bt, rt;
   TYPE_ID et;
#ifdef KEY /* Bug 10177 */
   INTRINSIC intr = INTRN_I4EXPEXPR;
#else /* KEY Bug 10177 */
   INTRINSIC intr;
#endif /* KEY Bug 10177 */
   WN *k[2];
   WN *wn  ;
   WN *base, *exp;
   WN *ae=NULL;

 
   bt  = TY_mtype(cast_to_TY(t_TY(type))) ;
   exp = cwh_expr_operand(&ae);
   base = cwh_get_typed_operand(bt,&ae);

   et = WN_rtype(exp);
   
   if (et == MTYPE_I4) {
      switch (bt) {
       case MTYPE_I1:
       case MTYPE_I2:
       case MTYPE_I4:
	 intr = INTRN_I4EXPEXPR; break;
       case MTYPE_I8: intr = INTRN_I8EXPEXPR; break;
       case MTYPE_F4: intr = INTRN_F4I4EXPEXPR; break;
       case MTYPE_F8: intr = INTRN_F8I4EXPEXPR; break;
       case MTYPE_FQ: intr = INTRN_FQI4EXPEXPR; break;
       case MTYPE_C4: intr = INTRN_C4I4EXPEXPR; break;
       case MTYPE_C8: intr = INTRN_C8I4EXPEXPR; break;
       case MTYPE_CQ: intr = INTRN_CQI4EXPEXPR; break;
      }
   } else if (et == MTYPE_I8) {
      switch (bt) {
       case MTYPE_I1:
       case MTYPE_I2:
       case MTYPE_I4:
       case MTYPE_I8:
	 intr = INTRN_I8EXPEXPR; break;
       case MTYPE_F4: intr = INTRN_F4I8EXPEXPR; break;
       case MTYPE_F8: intr = INTRN_F8I8EXPEXPR; break;
       case MTYPE_FQ: intr = INTRN_FQI8EXPEXPR; break;
       case MTYPE_C4: intr = INTRN_C4I8EXPEXPR; break;
       case MTYPE_C8: intr = INTRN_C8I8EXPEXPR; break;
       case MTYPE_CQ: intr = INTRN_CQI8EXPEXPR; break;
      }
   } else {
      exp = cwh_convert_to_ty(exp,bt);
      switch (bt) {
       case MTYPE_F4: intr = INTRN_F4EXPEXPR; break;
       case MTYPE_F8: intr = INTRN_F8EXPEXPR; break;
       case MTYPE_FQ: intr = INTRN_FQEXPEXPR; break;
       case MTYPE_C4: intr = INTRN_C4EXPEXPR; break;
       case MTYPE_C8: intr = INTRN_C8EXPEXPR; break;
       case MTYPE_CQ: intr = INTRN_CQEXPEXPR; break;
      }
   }
   
   rt   = WN_rtype(base);
   k[0] = base;
   k[1] = exp ;
   wn   = cwh_intrin_build(k,intr,rt,2);

   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * fei_round
 *
 * ANINT & NINT intrinsic variants. Select intrinsic
 * op based on type, and push the op on the stack.
 * Although some are called by ref, some by value we
 * pass a by-value intrinsic and the f90 lowerer patches
 * it up to the correct ref/value.
 *
 *===============================================
 */ 
void
fei_round(TYPE type) 
{
   TYPE_ID bt,rt  ;
#ifdef KEY /* Bug 10177 */
   OPCODE  opc = (OPCODE) 0;
   INTRINSIC intr = INTRN_I4EXPEXPR;
#else /* KEY Bug 10177 */
   OPCODE  opc ;
   INTRINSIC intr;
#endif /* KEY Bug 10177 */
   WN *k[2];
   WN *wn  ;
   WN *ae=NULL;
 
   rt  = TY_mtype(cast_to_TY(t_TY(type)));
   
   if(MTYPE_is_float(rt)) {
      k[0] = cwh_expr_operand(&ae);
      bt = WNRTY(k[0]);
      opc = cwh_make_typed_opcode(OPR_INTRINSIC_OP, bt, MTYPE_V);
      k[0] = cwh_intrin_wrap_value_parm(k[0]);
      
      switch (bt) {
       case MTYPE_F4: intr = INTRN_F4ANINT; break;
       case MTYPE_F8: intr = INTRN_F8ANINT; break;
       case MTYPE_FQ: intr = INTRN_FQANINT; break;
      }
   } else {
      
      k[0] = cwh_expr_operand(&ae);
      bt   = WNRTY(k[0]);
      k[0] = cwh_intrin_wrap_value_parm(k[0]);

      switch (rt) {
       case MTYPE_I1:
       case MTYPE_I2:
       case MTYPE_I4:
	  opc = cwh_make_typed_opcode(OPR_INTRINSIC_OP, MTYPE_I4, MTYPE_V);
	  switch (bt) {
	   case MTYPE_F4: intr = INTRN_I4F4NINT; break;
	   case MTYPE_F8: intr = INTRN_I4F8IDNINT; break;
	   case MTYPE_FQ: intr = INTRN_I4FQIQNINT; break;
	  }
	  break;
      
       case MTYPE_I8:
	  opc = cwh_make_typed_opcode(OPR_INTRINSIC_OP, MTYPE_I8, MTYPE_V);
	  switch (bt) {
	   case MTYPE_F4: intr = INTRN_I8F4NINT; break;
	   case MTYPE_F8: intr = INTRN_I8F8IDNINT; break;
	   case MTYPE_FQ: intr = INTRN_I8FQIQNINT; break;
	  }
	  break;
      }
   }
     
   wn = WN_Create_Intrinsic(opc,intr,1,k);
   if(MTYPE_is_float(rt)) {
      wn = cwh_convert_to_ty(wn,rt);
   } else {
      wn = cwh_wrap_cvtl(wn,rt);
   }
   
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push_typed(wn,WN_item,Be_Type_Tbl(rt));
}

/*===============================================
 *
 * fei_trunc
 *
 * AINT intrinsic variants. Needed because of KIND argument.
 *
 *===============================================
 */ 
void
fei_trunc(TYPE type) 
{
   TYPE_ID bt,rt  ;
   INTRINSIC intr;
   WN *k[1];
   WN *wn  ;
   WN *ae=NULL;
 
   rt  = TY_mtype(cast_to_TY(t_TY(type)));
   k[0] = cwh_expr_operand(&ae);
   bt = WNRTY(k[0]);
   k[0] = cwh_intrin_wrap_value_parm(k[0]);
   
   intr = GET_ITAB_IOP(i_trunc,bt);

   DevAssert((intr),("Unsupported intr/ty combo"));

   wn = cwh_intrin_build(k,intr,bt,1);
   wn = cwh_convert_to_ty(wn,rt);
   
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}

void fei_scale(TYPE type) 
{
   TYPE_ID bt  ;
   INTRINSIC intr;
   WN *k[2];
   WN *wn  ;
   WN *ae=NULL;
 
   k[1] = cwh_get_typed_operand(MTYPE_I4,&ae);
   k[0] = cwh_expr_operand(&ae);
   bt   = WN_rtype(k[0]);
   intr = GET_ITAB_IOP(i_scale,bt);

   wn = cwh_intrin_build(k,intr,bt,2);
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}

void fei_near(TYPE type) 
{
   TYPE_ID bt;
   INTRINSIC intr;
   WN *k[2];
   WN *wn;
   WN *ae=NULL;
 
   WN_DELETE_Tree(cwh_expr_operand(NULL));
   k[1] = cwh_expr_operand(&ae);
   k[0] = cwh_expr_operand(&ae);
   bt   = WN_rtype(k[0]);
#ifdef KEY /* Bug 4056 */
   /* When operand types don't match, convert second to match first, since the
    * BE emits only _NEAREST or _NEAREST_4, and the computation is sensitive
    * to the precision of the first operand. Although library contains
    * _NEAREST_4_8, that seems unnecessary, since the 2nd argument is used only
    * for its sign, and with IEEE floating point, if the 2nd argument exceeds
    * the range of the first, it gets converted into infinity with the correct
    * sign. */
   k[1] = cwh_convert_to_ty(k[1], bt);
#endif /* KEY Bug 4056 */
   intr = GET_ITAB_IOP(i_near,bt);

   wn = cwh_intrin_build(k,intr,bt,2);
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}


void fei_set_exponent(TYPE type) 
{
   TYPE_ID bt  ;
   INTRINSIC intr;
   WN *k[2];
   WN *wn  ;
   WN *ae=NULL;
 
   k[1] = cwh_get_typed_operand(MTYPE_I4,&ae);
   k[0] = cwh_expr_operand(&ae);
   bt = WN_rtype(k[0]);
   
   intr = GET_ITAB_IOP(i_set_exponent,bt);
   wn   = cwh_intrin_build(k,intr,bt,2);
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}

void fei_exponent(TYPE type) 
{
   TYPE_ID bt,rt;
   INTRINSIC intr;
   WN *k[1];
   WN *wn  ;
   WN *ae=NULL;
 
   rt  = TY_mtype(cast_to_TY(t_TY(type)));
   k[0] = cwh_expr_operand(&ae);
   bt = WN_rtype(k[0]);
   
   intr = GET_ITAB_IOP(i_exponent,bt);
   wn   = cwh_intrin_build(k,intr,rt,1);
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}


/* 
 * Inline the Fortran DIM intrinsic
 */
void fei_pos_diff(TYPE type)
{
   WN *zero;
   fei_minus(type);
   zero = WN_Intconst(MTYPE_I4,0);
   cwh_stk_push(zero,WN_item);
   fei_max(2,type);
}


#ifndef KEY /* Bug 9140 */
/* 
 * Inline the SIGN intrinsic
 */
void fei_sign_xfer(TYPE type)
{
   WN *a, *aneg, *b;
   WN *ae=NULL;

   b = cwh_expr_operand(&ae);

   fei_abs(type);
   a = cwh_expr_operand(&ae);
   cwh_stk_push(WN_COPY_Tree(a),WN_item);
   fei_uminus(type);
   aneg = cwh_expr_operand(&ae);

   /* If B > 0, return A else return -A */
   cwh_stk_push(b,WN_item);
   cwh_stk_push(WN_Zerocon(WN_rtype(b)),WN_item);
   fei_ge(type);
   b = cwh_expr_operand(&ae);
   cwh_stk_push(a,WN_item);
   cwh_stk_push(aneg,WN_item);
   b = cwh_expr_restore_arrayexp(b,ae);
   cwh_stk_push(b,WN_item);
#ifdef KEY /* Bug 10410 */
   fei_select(type, 0);
#else /* KEY Bug 10410 */
   fei_select(type);
#endif /* KEY Bug 10410 */
}
#endif /* KEY Bug 9140 */

/*
 * Inline the IEEE SIGN intrinsic
 */
void fei_ieee_sign_xfer(TYPE type)
{
   WN *a, *b;
   WN *ae=NULL;
   TYPE_ID rt,it,bt;

   rt = TY_mtype(cast_to_TY(t_TY(type)));
   if (rt == MTYPE_FQ) {
      fei_sign_xfer(type);
      return;
   } else if (rt == MTYPE_F8) {
      it = MTYPE_I8;
   } else {
      it = MTYPE_I4;
   }

   b = cwh_expr_operand(&ae);
   bt = WNRTY(b);
   if (bt == MTYPE_F4) {
      b = WN_Tas(MTYPE_I4,Be_Type_Tbl(MTYPE_I4),b);
      b = WN_Lshr(MTYPE_I4,b,WN_Intconst(MTYPE_I4,31));
   } else if (bt == MTYPE_F8) {
      b = WN_Tas(MTYPE_I8,Be_Type_Tbl(MTYPE_I8),b);
      b = WN_Lshr(MTYPE_I8,b,WN_Intconst(MTYPE_I8,63));
   } else {
      /* FQ */
      b = WN_LT(bt,b,WN_Zerocon(bt));
   }

   /* At this point B is either 0 or 1, and is either a 64 bit or a 32 bit integer */
   /* Get it in the right position for Or'ing it in */
   bt = WNRTY(b);
   if (MTYPE_bit_size(bt) == MTYPE_bit_size(rt)) {
      b = WN_Shl(bt,b,WN_Intconst(MTYPE_I4,MTYPE_bit_size(bt)-1));
   } else if (MTYPE_bit_size(bt) > MTYPE_bit_size(rt)) {
      /* bt must be I8, rt, F4 */
      b = WN_Shl(MTYPE_I4,b,WN_Intconst(MTYPE_I4,31));
   } else {
      /* bt must be I4, rt, F8 */
      b = WN_Shl(MTYPE_I8,b,WN_Intconst(MTYPE_I4,63));
   }

   /* Get ABS(A) */
   fei_abs(type);
   a = cwh_expr_operand(&ae);

   /* Convert it to an integer and or with the sign bit */
   a = WN_Tas(it,Be_Type_Tbl(it),a);
   a = cwh_expr_bincalc(OPR_BIOR,a,b);

   /* Convert it back to a real */
   a = WN_Tas(rt,Be_Type_Tbl(rt),a);

   a = cwh_expr_restore_arrayexp(a,ae);
   cwh_stk_push(a,WN_item);
}

static void cwh_ceiling_floor(TYPE type, OPERATOR opr) 
{
   
   TYPE_ID bt;
   TYPE_ID rt; 
   OPCODE  opc ;
   WN *k;
   WN *wn  ;
   WN *ae=NULL;

   k = cwh_expr_operand(&ae);
   bt = WN_rtype(k);
   rt = TY_mtype(cast_to_TY(t_TY(type)));
   
   opc = cwh_make_typed_opcode(opr, rt, bt);
   wn = WN_CreateExp1 ( opc, k) ;

   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}

void fei_ceiling (TYPE type) 
{
   cwh_ceiling_floor(type, OPR_CEIL);
}

void fei_floor (TYPE type) 
{
   cwh_ceiling_floor(type, OPR_FLOOR);
}


/*================================================================
 * 
 * cwh_do_tranformational
 * 
 * Utility to build transformationals
 *
 * intrn - intrinsic to build
 * numargs - number of arguments to pop
 * rtype - the type of the returned value.
 * is_numeric - if TRUE, use the type from rtype to make up the type of the intrinsic op,
 *              otherwise, use the type of the first argument. 
 * cvt_to_rtype - if TRUE, all arguments are first converetd to the return type
 *
 *================================================================
*/
#define MAXARGS 6

static void 
cwh_do_tranformational(INTRINSIC intrn, INT numargs, TYPE rtype, BOOL is_numeric,
		       BOOL cvt_to_rtype)
{
   WN * args[MAXARGS];
   WN *wn;
#ifdef KEY /* Bug 10177 */
   WN *charlen = 0;
#else /* KEY Bug 10177 */
   WN *charlen;
#endif /* KEY Bug 10177 */
   OPCODE op;
   INT i;
   BOOL is_char;
#ifdef KEY /* Bug 10177 */
   TY_IDX  str_ty = 0;
#else /* KEY Bug 10177 */
   TY_IDX  str_ty;
#endif /* KEY Bug 10177 */
   TY_IDX  p_ty;
   TY_IDX  rty;
   TYPE_ID type_from_first;
   TYPE_ID result_type;

   rty = cast_to_TY(t_TY(rtype));
   result_type = TY_mtype(rty);

   is_char = FALSE;
   for (i=numargs-1; i >= 0; i--) {

      if (cwh_stk_get_class() == STR_item) {
	 is_char = TRUE;

	 cwh_stk_pop_STR();
	 charlen  = cwh_expr_operand(NULL);
	 str_ty   = cwh_stk_get_TY();
	 args[i]  = cwh_expr_address(f_NONE);

	 if (TY_kind(str_ty) == KIND_POINTER) {
	   p_ty   = str_ty;
	   str_ty = TY_pointed(p_ty);

	 } else
	   p_ty = cwh_types_make_pointer_type(str_ty, FALSE);

	 if (WN_operator(args[i]) != OPR_INTRINSIC_OP) {
	   args[i] = WN_CreateMload(0,p_ty,args[i],WN_COPY_Tree(charlen));
	 } else {
	    WN_set_opcode(args[i],OPC_MINTRINSIC_OP);
	 }
	 args[i] = WN_CreateParm(MTYPE_M,args[i],str_ty,WN_PARM_BY_VALUE);

      } else {
	 args[i] = cwh_expr_operand(NULL);
	 if (!args[i]) {
	    args[i] = cwh_intrin_null_parm();
	 } else {
	   if (cvt_to_rtype) {
	     args[i] = cwh_convert_to_ty(args[i],result_type);
	   }
	   args[i] = cwh_intrin_wrap_value_parm(args[i]);
	 }
      }
   }

   if (is_char) {
      type_from_first = Pointer_Mtype;
      op = cwh_make_typed_opcode(OPR_INTRINSIC_OP, Pointer_Mtype, MTYPE_V);
   } else {
      type_from_first = result_type;
      if (is_numeric) {
	 op = OPCODE_make_op(OPR_INTRINSIC_OP, result_type, MTYPE_V);
      } else {
	 op = OPCODE_make_op(OPR_INTRINSIC_OP, WNRTY(args[0]), MTYPE_V);
      }
   }

   wn = WN_Create_Intrinsic(op,intrn,numargs,args);
   if (is_numeric) {
      wn = cwh_wrap_cvtl(wn,type_from_first);
   }
   wn = F90_Wrap_ARREXP(wn);

   if (is_char) {
      cwh_stk_push_STR(charlen,wn,str_ty,WN_item); /* assumes type_from_first */
   } else {
      cwh_stk_push_typed(wn,WN_item,rty);
   }
   return;
}
   
#define do_transformational(name,intrn,numargs,is_numeric) void name(TYPE rtype) \
   {cwh_do_tranformational(intrn,numargs,rtype,is_numeric,FALSE);}

#define do_transformational_cvt(name,intrn,numargs,is_numeric) void name(TYPE rtype) \
   {cwh_do_tranformational(intrn,numargs,rtype,is_numeric,TRUE);}

do_transformational(fei_spread,INTRN_SPREAD,3,FALSE)
do_transformational(fei_transpose,INTRN_TRANSPOSE,1,FALSE)
do_transformational(fei_all,INTRN_ALL,2,TRUE)
do_transformational(fei_any,INTRN_ANY,2,TRUE)
do_transformational(fei_product,INTRN_PRODUCT,3,TRUE)
do_transformational(fei_sum,INTRN_SUM,3,TRUE)
do_transformational(fei_maxval,INTRN_MAXVAL,3,TRUE)
do_transformational(fei_minval,INTRN_MINVAL,3,TRUE)
do_transformational(fei_maxloc,INTRN_MAXLOC,2,TRUE)
do_transformational(fei_minloc,INTRN_MINLOC,2,TRUE)
do_transformational(fei__maxloc,INTRN_MAXLOC,3,TRUE)
do_transformational(fei__minloc,INTRN_MINLOC,3,TRUE)
do_transformational(fei_pack,INTRN_PACK,3,FALSE)
do_transformational(fei_unpack,INTRN_UNPACK,3,FALSE)
do_transformational(fei_cshift,INTRN_CSHIFT,3,FALSE)
do_transformational(fei_eoshift,INTRN_EOSHIFT,4,FALSE)

/*================================================================
 * 
 * fei_matmul
 * 
 * Do matrix multiply
 *
 *================================================================
*/

void 
fei_matmul(TYPE rtype)
{
   WN * args[2];
   WN * wn;
   OPCODE op;
   INT i;
   TY_IDX  rty;
   TYPE_ID result_type;

   rty = cast_to_TY(t_TY(rtype));
   result_type = TY_mtype(rty);

   for (i=1; i >= 0; i--) {
     args[i] = cwh_expr_operand(NULL);
     args[i] = cwh_convert_to_ty(args[i],result_type);
     args[i] = cwh_intrin_wrap_value_parm(args[i]);
   }
   
   if (TY_is_logical(rty)) {
     op = OPCODE_make_op(OPR_INTRINSIC_OP, MTYPE_B, MTYPE_V);
   } else {
     op = OPCODE_make_op(OPR_INTRINSIC_OP, result_type, MTYPE_V);
   }

   wn = WN_Create_Intrinsic(op,INTRN_MATMUL,2,args);
   if (!TY_is_logical(rty)) {
     wn = cwh_wrap_cvtl(wn,result_type);
   }
   wn = F90_Wrap_ARREXP(wn);

   cwh_stk_push_typed(wn,WN_item,rty);
   return;
}



void
fei_dot_product(TYPE rtype)
{
   WN *arg0,*arg1;
   WN *intr_args[3];
   WN *wn;
   OPCODE op,mpy_op;
   INTRINSIC intr;
   WN *ae=NULL;
   TY_IDX rty;
   TYPE_ID ty;

   rty = cast_to_TY(t_TY(rtype)); 
   ty = TY_mtype(rty);

   arg1 = cwh_expr_operand(&ae);
   arg0 = cwh_expr_operand(&ae);
   arg0 = cwh_convert_to_ty(arg0,ty);
   arg1 = cwh_convert_to_ty(arg1,ty);
   
   op = cwh_make_typed_opcode(OPR_INTRINSIC_OP, ty, MTYPE_V);
   mpy_op = cwh_make_typed_opcode(OPR_MPY,ty,MTYPE_V);
   
   if (MTYPE_is_complex(ty)) {
      /* Need to conjugate arg0 */
      if (ty == MTYPE_C4) {
	 intr = INTRN_C4CONJG;
      } else if (ty == MTYPE_C8) {
	 intr = INTRN_C8CONJG;
      } else {
	 intr = INTRN_CQCONJG;
      }
      arg0 = cwh_intrin_wrap_value_parm(arg0);
      arg0 = WN_Create_Intrinsic(op,intr,1,&arg0);
   }
  
   arg0 = WN_CreateExp2(mpy_op,arg0,arg1);
   arg0 = cwh_expr_restore_arrayexp(arg0,ae);
   intr_args[0] = cwh_intrin_wrap_value_parm(arg0);
   intr_args[1] = cwh_intrin_null_parm();
   intr_args[2] = cwh_intrin_null_parm();
   wn = WN_Create_Intrinsic(op,INTRN_SUM,3,intr_args);
   wn = cwh_wrap_cvtl(wn,ty);

   cwh_stk_push_typed(wn,WN_item,rty);
   return;
}

/*===============================================
 *
 * fei_dot_product_logical
 *
 * Do DOT_PRODUCT intrinsic for logicals. TOS has
 * a pair of operands, and destination below. 
 * Setup the ANY intrinsic to be the result of the
 * logical AND of the operands. & push the result
 * for fei_store.
 * 
 *===============================================
 */ 
void
fei_dot_product_logical(TYPE rtype)
{
   WN *arg0,*arg1;
   WN *intr_args[2];
   WN *wn;
   OPCODE op ;
   WN *ae=NULL;
   
   
   TYPE_ID ty;

   arg1 = cwh_expr_operand(&ae);
   arg0 = cwh_expr_operand(&ae);
   ty   = cwh_get_highest_type(arg0,arg1);
   arg0 = cwh_convert_to_ty(arg0,ty);
   arg1 = cwh_convert_to_ty(arg1,ty);
   
   op = cwh_make_typed_opcode(OPR_INTRINSIC_OP, ty, MTYPE_V);
    
   arg0 = WN_CreateExp2(OPC_I4LAND,arg0,arg1);
   arg0 = cwh_expr_restore_arrayexp(arg0,ae);
   intr_args[0] = cwh_intrin_wrap_value_parm(arg0);
   intr_args[1] = cwh_intrin_null_parm();
   wn = WN_Create_Intrinsic(op,INTRN_ANY,2,intr_args);

   cwh_stk_push_typed(wn,WN_item,cast_to_TY(t_TY(rtype)));
   return;
}

void
fei_count(TYPE type)
{
   WN *args[3];
   WN *wn;
   OPCODE op;
   TYPE_ID ty;
   WN *ae=NULL;

   args[1] = cwh_expr_operand(NULL);
   args[0] = cwh_expr_operand(&ae);
   if (!args[1]) {
      args[1] = cwh_intrin_wrap_value_parm(WN_Zerocon(MTYPE_I4));
   } else {
      args[1] = cwh_intrin_wrap_value_parm(args[1]);
   }
   args[2] = cwh_intrin_wrap_value_parm(WN_Intconst(MTYPE_I4,1));
   
   /* Turn this into SUM(ARRAY.NE.0,...) */
   ty = WN_rtype(args[0]);
   if (ty != MTYPE_B) {
      op = cwh_make_typed_opcode(OPR_NE,MTYPE_I4,ty);
      args[0] = WN_CreateExp2(op,args[0],WN_Zerocon(ty));
   }
   args[0] = cwh_expr_restore_arrayexp(args[0],ae);
   args[0] = cwh_intrin_wrap_value_parm(args[0]);

   op = cwh_make_typed_opcode(OPR_INTRINSIC_OP, Pointer_Size==8 ? MTYPE_I8 : MTYPE_I4, MTYPE_V);
   wn = WN_Create_Intrinsic(op,INTRN_SUM,3,args);
   
   wn = F90_Wrap_ARREXP(wn);
   cwh_stk_push(wn,WN_item);
   return;
}

/*===============================================
 *
 * fei_malloc, fei_alloc
 *
 * Space allocation. fei_malloc is the malloc
 * intrinsic(ie: heap). fei_alloc is used by 
 * automatic arrays & local temps (ie: stack).
 * The amount to allocate is TOS. Create the
 * intrinsic, and push on the stack, for 
 * fei_store to put away.
 *
 *===============================================
 */ 
void
fei_malloc (void)
{
  WN * k[1];
  WN * sz  = NULL ;
  WN * call;
  BOOL v = TRUE;
  WN * wn  ;
  INTRINSIC intr;
  char preg_name[32];
  
  /* Build the intrinsic_call node */
  k[0] = cwh_expr_operand(NULL);
  intr = (Pointer_Size == 4) ? INTRN_U4I4MALLOC : INTRN_U8I8MALLOC;
     
  call = cwh_intrin_call(intr, 1, k, &sz, &v, Pointer_Mtype);
  WN_Set_Call_Does_Mem_Alloc(call);

  /* Get the return value */
  wn = cwh_stmt_return_scalar(NULL, NULL, Be_Type_Tbl(Pointer_Mtype), FALSE);
  cwh_stk_push(wn,WN_item);
  sprintf(preg_name,"malloc@line_%d",USRCPOS_linenum(current_srcpos));
  wn = cwh_intrin_get_return_value(Pointer_Mtype,preg_name);


  cwh_stk_push(wn,WN_item);
}

void
fei_alloc (void)
{
  WN * k[1];
  WN * wn  ;
  
  k[0] = cwh_expr_operand(NULL);
  if (Heap_Allocation_Threshold == -1) {
    wn = cwh_intrin_build(k,INTRN_F90_STACKTEMPALLOC,Pointer_Mtype,1);
  } else if (Heap_Allocation_Threshold == 0) {
    wn = cwh_intrin_build(k,INTRN_F90_HEAPTEMPALLOC,Pointer_Mtype,1);
  } else {
    wn = cwh_intrin_build(k,INTRN_F90_DYNAMICTEMPALLOC,Pointer_Mtype,1);
  }
    
  cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * fei_free, fei_mfree
 *
 * Return temp space,stack or heap. TOS has pointer.
 * An intrinsic call is tacked to the current block.
 *
 *===============================================
 */ 
void
fei_mfree (void)
{
  WN * k[1];
  WN * sz  = NULL ;
  WN * call;
  BOOL val = TRUE;
  INTRINSIC intr;
  
  intr = (Pointer_Size == 4) ? INTRN_U4FREE : INTRN_U8FREE;
  
  k[0] = cwh_expr_operand(NULL);
  call = cwh_intrin_call(intr,1,k,&sz,&val,MTYPE_V);
  WN_Set_Call_Does_Mem_Free(call);
}

void
fei_free (void)
{
  WN * k[1];
  WN * sz  = NULL ;
  BOOL val = TRUE;
  
  k[0] = cwh_expr_operand(NULL);
  if (Heap_Allocation_Threshold == -1) {
    cwh_intrin_call(INTRN_F90_STACKTEMPFREE,1,k,&sz,&val,Pointer_Mtype);
  } else if (Heap_Allocation_Threshold == 0) {
    cwh_intrin_call(INTRN_F90_HEAPTEMPFREE,1,k,&sz,&val,Pointer_Mtype);
  } else {
    cwh_intrin_call(INTRN_F90_DYNAMICTEMPFREE,1,k,&sz,&val,Pointer_Mtype);
  }
    
}

/*===============================================
 *
 * fei_ranf,fei_ranget,fei_ranset
 *
 * The next threee implement the random number 
 * intrinsic subroutines RANDOM_NUMBER and RANDOM_SEED
 *
 * Ranf returns the next random number.
 *
 *===============================================
 */ 
void
fei_ranf(TYPE type) {
   WN *wn;
   TYPE_ID t;

   t = TY_mtype(cast_to_TY(t_TY(type)));
   if (t == MTYPE_F4) {
      wn = WN_Create_Intrinsic(OPC_F4INTRINSIC_OP,INTRN_F4I4RAN,0,NULL);
   } else {
      wn = WN_Create_Intrinsic(OPC_F8INTRINSIC_OP,INTRN_F8I4RAN,0,NULL);
   }      
   cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * fei_ranget
 *
 * Do a GET operation for RANDOM SEED. ie:
 * return the current value of the seed. TOS has 
 * the destination address.  Create an
 * intrinsic call and push a NULL for fei_store.
 *
 *===============================================
 */ 
void
fei_ranget (TYPE type) {
   WN *addr;
   WN *call;
   INT64 flags = 0;

   if (!ranget_st) {
      ranget_st = cwh_intrin_make_intrinsic_symbol("_RANGET",MTYPE_V);
   }

   addr = cwh_expr_address(f_T_PASSED);
   cwh_stk_push(ranget_st,ST_item);
   cwh_stk_push(addr,ADDR_item);
   call = cwh_stmt_call_helper(1,Be_Type_Tbl(MTYPE_V),0,flags);
#ifdef KEY /* Bug 4434 */
   cwh_stk_push(NULL,WN_item);
#endif /* KEY Bug 4434 */
   
}

/*===============================================
 *
 * fei_ranset
 *
 * Do a PUT operation for RANDOM SEED. ie:
 * establish a seed value. TOS has the seed 
 * address. Create an intrinsic call and push 
 * a NULL for fei_store.
 *
 *===============================================
 */ 
void
fei_ranset (TYPE type) {
   WN *call;
   WN *wn;
   INT64 flags = 0;

   if (!ranset_st) {
      ranset_st = cwh_intrin_make_intrinsic_symbol("_RANSET",MTYPE_V);
   }

   wn = cwh_expr_address(f_T_PASSED);
   cwh_stk_push(ranset_st,ST_item);
   cwh_stk_push(wn,ADDR_item);
   call = cwh_stmt_call_helper(1,Be_Type_Tbl(MTYPE_V),0,flags);

   cwh_stk_push(NULL,WN_item);
}


void
fei_rtc (TYPE type) {
   WN *call;
   WN *wn;
   INT64 flags = 0;

   if (!rtc_st) {
      rtc_st = cwh_intrin_make_intrinsic_symbol("_IRTC_",MTYPE_I8);
   }

   cwh_stk_push(rtc_st,ST_item);
   call = cwh_stmt_call_helper(0,Be_Type_Tbl(MTYPE_I8),0,flags);
   wn = cwh_intrin_get_return_value(MTYPE_I8,"@f90rtc");
   wn = cwh_convert_to_ty(wn,TY_mtype(cast_to_TY(t_TY(type))));
   cwh_stk_push(wn,WN_item);
}

void
fei_unit(void)
{
   WN *call;
   WN *addr;
   WN *wn;
   INT64 flags = 0;

   if (!unit_st) {
      unit_st = cwh_intrin_make_intrinsic_symbol("_UNIT_",MTYPE_F4);
   }

   addr = cwh_expr_address(f_T_PASSED);
   cwh_stk_push(unit_st,ST_item);
   cwh_stk_push(addr,ADDR_item);
   call = cwh_stmt_call_helper(1,Be_Type_Tbl(MTYPE_F4),0,flags);
   
   wn = cwh_intrin_get_return_value (MTYPE_F4,"@f90unit");
   cwh_stk_push(wn,WN_item);

}


void
fei_length(void)
{
   WN *call;
   WN *addr;
   WN *wn;
   INT64 flags = 0;

   if (!length_st) {
      length_st = cwh_intrin_make_intrinsic_symbol("_LENGTH_",MTYPE_I4);
   }

   addr = cwh_expr_address(f_T_PASSED);
   cwh_stk_push(length_st,ST_item);
   cwh_stk_push(addr,ADDR_item);
   call = cwh_stmt_call_helper(1,Be_Type_Tbl(MTYPE_I4),0,flags);
   
   wn = cwh_intrin_get_return_value (MTYPE_I4,"@f90length");
   cwh_stk_push(wn,WN_item);

}



void
fei_present(void)
{
   WN *wn;
   WN *arg;
   TY_IDX ty;

   arg = cwh_expr_address(f_NONE);
   wn  = WN_CreateExp2(OPCODE_make_op(OPR_NE,MTYPE_I4,Pointer_Mtype),
		       arg,
		       WN_Intconst(Pointer_Mtype,0));
   cwh_stk_push_typed(wn,WN_item,logical4_ty);
}



/*================================================================
 * 
 * fei_ibits: implement the IBITS intrinsic
 *
 * 
 * IBITS(X,POS,LEN) = X >> POS && MASK
 * where mask is  (1 << LEN) - 1. For 64 bit types, we add
 * a little check to ((LEN != 64) << LEN) - 1. That way, if LEN =64, 
 * we get a mask with all 1's
 *
 *================================================================
 */

void
fei_ibits(TYPE type)
{
   WN *x, *pos, *len;
   WN *mask;
   TYPE_ID ty,rty;
   WN *ae=NULL;

   len = cwh_expr_operand(&ae);
   pos = cwh_expr_operand(&ae);
   x   = cwh_expr_operand(&ae);
   
   rty = TY_mtype(cast_to_TY(t_TY(type)));
   ty = Mtype_comparison(rty);
   
   x = WN_Lshr(ty,x,pos);
   
   mask = cwh_generate_bitmask(len,ty);
   x = cwh_expr_bincalc(OPR_BAND,x,mask);
#ifdef KEY /* Bug 8547 */
   /* If cwh_expr_bincalc() generated an unsigned integer (e.g. U4EXTRACT_BITS)
    * we need to correct the result type to be signed, but cwh_wrap_cvtl()
    * doesn't do anything unless the input type is smaller than 4 bytes. */
   ty = WN_rtype(x);
   if (ty != rty) {
     if (MTYPE_I4 == rty || MTYPE_I8 == rty) {
       OPCODE cvt_op = OPCODE_make_op(OPR_CVT,rty,ty);
       x = WN_CreateExp1(cvt_op, x);
     }
     else {
       x = cwh_wrap_cvtl(x, rty);
     }
   }
#else /* KEY Bug 8547 */
   x = cwh_wrap_cvtl(x,rty);
#endif /* KEY Bug 8547 */

   x = cwh_expr_restore_arrayexp(x,ae);
   cwh_stk_push(x,WN_item);
}

/*================================================================
 * 
 * fei_mvbits: implement the MVBITS intrinsic
 *
 *================================================================
 */

void
fei_mvbits(TYPE type)
{
   WN *from,*frompos,*len,*to,*topos;
   WN *t1,*mask,*r;
   WN *ae=NULL;

   TYPE_ID ty,rty;

   topos   = cwh_expr_operand(&ae);
   to      = cwh_expr_operand(&ae);
   len     = cwh_expr_operand(&ae);
   frompos = cwh_expr_operand(&ae);
   from    = cwh_expr_operand(&ae);
   
   rty = TY_mtype(cast_to_TY(t_TY(type)));
   ty = Mtype_comparison(rty);

   from = WN_Lshr(ty,from,frompos);
   mask = cwh_generate_bitmask(len,ty);
   t1 = cwh_expr_bincalc(OPR_BAND,from,WN_COPY_Tree(mask));
   t1 = cwh_expr_bincalc(OPR_SHL,t1,WN_COPY_Tree(topos));

   mask = WN_Shl(ty,mask,topos);
   mask = WN_CreateExp1(OPCODE_make_op(OPR_BNOT,ty,MTYPE_V),mask);

   r = cwh_expr_bincalc(OPR_BAND,to,mask);
   r = cwh_expr_bincalc(OPR_BIOR,r,t1);
   r = cwh_wrap_cvtl(r,rty);

   r = cwh_expr_restore_arrayexp(r,ae);
   cwh_stk_push(r,WN_item);
   fei_store(type);
}

/*================================================================
 *
 * cwh_char_intrin
 *
 * Utility for character intrinsic ops - used by scan/verify etc.
 * Create op and push the result.
 *
 * intr - intrinsic to call
 * numargs - number of arguments
 * 
 * automatically detects string arguments
 *================================================================
 */
static void
cwh_char_intrin(INTRINSIC intr, INT numargs) 
{
   WN * args[5];
   INT arg_count;
   INT i;
   WN *charlen;
   WN *charlen1;
   WN *wn;
   OPCODE op;

   arg_count = 5;
   for (i = 0; i < numargs; i++) {
      if (cwh_stk_get_class() == STR_item) {
	 cwh_stk_pop_STR();
	 charlen = cwh_expr_operand(NULL);
	 charlen1 = WN_COPY_Tree(charlen);
	 args[--arg_count] = cwh_intrin_wrap_value_parm(charlen);
	 args[--arg_count] = cwh_expr_address(f_NONE);
	 args[arg_count]   = cwh_intrin_wrap_char_parm(args[arg_count],charlen1);
      } else {
	 args[--arg_count] = cwh_intrin_wrap_value_parm(cwh_expr_operand(NULL));
      }
   }
   
   wn = WN_Create_Intrinsic(OPC_I4INTRINSIC_OP,intr,5-arg_count,&args[arg_count]);
   cwh_stk_push(wn,WN_item);
}


#define do_char_intrin(name,intr,args) void name(TYPE type) {cwh_char_intrin(intr,args);}
#define do_char_intrin_nt(name,intr,args) void name(void) {cwh_char_intrin(intr,args);}

do_char_intrin(fei_scan,INTRN_SCAN,3)
do_char_intrin(fei_verify,INTRN_VERIFY,3)
do_char_intrin_nt(fei_index,INTRN_F90INDEX,3)
do_char_intrin_nt(fei_len_trim,INTRN_LENTRIM,1)


/*================================================================
 *
 * fei_adjust{l,r}
 *
 * Intrinsics ADJUSTL/R - move a string. Turned into calls
 * which assign into the result (temp) below the argument
 * at TOS. Push NULLS so fei_store ignores result.
 *
 *================================================================
 */
void
fei_adjustl (TYPE type)
{
   cwh_stmt_character_icall(INTRN_ADJUSTL);
   cwh_stk_push(NULL,WN_item);
   cwh_stk_push(NULL,WN_item);
}

void
fei_adjustr (TYPE type)
{
   cwh_stmt_character_icall(INTRN_ADJUSTR);
   cwh_stk_push(NULL,WN_item);
   cwh_stk_push(NULL,WN_item);
}

void
fei_ieee_round(TYPE type)
{
   fei_cvtop(type);
}


void
fei_ieee_trunc(TYPE type)
{
   TY_IDX ty;
   WN *ae=NULL;
   WN *r;
   TYPE_ID bt;
   INTRINSIC intr;

   ty = cast_to_TY(t_TY(type));
   bt = TY_mtype(ty);

   /* Convert to REAL(16) then build the appropriate intrinsic */
   r = cwh_expr_operand(&ae);
   r = cwh_convert_to_ty(r,MTYPE_FQ);
   intr = GET_ITAB_IOP(i_ieee_int,bt);
   r = cwh_intrin_build(&r, intr, bt, 1);
   r = cwh_wrap_cvtl(r,bt);
   r = cwh_expr_restore_arrayexp(r,ae);
   cwh_stk_push_typed(r,WN_item,ty);
}

/* Build POPCNT and LEADZ (and POPPAR) */
/* type is the type of the intrinsic, arg is the type of the argument */
static void
cwh_intrin_popcnt_leadz_helper(INTRINSIC i1, INTRINSIC i2, INTRINSIC i4, INTRINSIC i8,
			       TYPE rtype, TYPE arg)
{
   WN *wn;
   WN *r;
   TYPE_ID t,ti,rt;
#ifdef KEY /* Bug 10177 */
   INTRINSIC intr = INTRN_I4EXPEXPR;
#else /* KEY Bug 10177 */
   INTRINSIC intr;
#endif /* KEY Bug 10177 */
   WN *ae=NULL;
   
   t = TY_mtype(t_TY(arg));
   rt = TY_mtype(t_TY(rtype));

   wn = cwh_expr_operand(&ae);

   /* For non integral types, we need to create a cast */
   if (!MTYPE_is_integral(t)) {
      if (MTYPE_size_reg(t) == 32) {
	 ti = MTYPE_U4;
      } else {
	 ti = MTYPE_U8;
      }
      wn = WN_Tas(ti,Be_Type_Tbl(t),wn);
   } else {
      ti = t;
   }

   // 12969: For MTYPE_U1, MTYPE_I1, MTYPE_U2, and MTYPE_U4,
   // we won't worry about zeroing out the upper bits here.
   // Instead, our library functions ignore the higher bits.
   switch (ti) {
    case MTYPE_U1: case MTYPE_I1: intr = i1; break;
    case MTYPE_U2: case MTYPE_I2: intr = i2; break;
    case MTYPE_U4: case MTYPE_I4: intr = i4; break;
    case MTYPE_U8: case MTYPE_I8: intr = i8; break;
    default: DevAssert(0,("Unknown type"));
   }

   wn = cwh_intrin_wrap_value_parm(wn);
   r = WN_Create_Intrinsic(OPC_I4INTRINSIC_OP,intr,1,&wn);
   r = cwh_convert_to_ty(r,rt);
   r = cwh_expr_restore_arrayexp(r,ae);
   cwh_stk_push(r,WN_item);
}


void
fei_popcnt (TYPE type, TYPE arg)
{
   cwh_intrin_popcnt_leadz_helper(INTRN_I1POPCNT,INTRN_I2POPCNT,
				  INTRN_I4POPCNT,INTRN_I8POPCNT,type,arg);
}


void
fei_leadz (TYPE type, TYPE arg)
{
   cwh_intrin_popcnt_leadz_helper(INTRN_I1LEADZ,INTRN_I2LEADZ,
				  INTRN_I4LEADZ,INTRN_I8LEADZ,type,arg);
}


void
fei_poppar (TYPE type, TYPE arg)
{
   cwh_intrin_popcnt_leadz_helper(INTRN_I4POPPAR,INTRN_I4POPPAR,
				  INTRN_I4POPPAR,INTRN_I8POPPAR,type,arg);
}


/*================================================================
 * Utility routine for several of the IEEE intrinsics which don't have
 * REAL(4) implementations
 *
 * intr - intrinsic to use
 * numargs - number of arguments
 * t - type of the returned intrinsic
 * cvtf4 - if TRUE, F4 intrinsics are converted to F8 intrinsics 
 * ae - ARRAYEXP node to possibly put on top
 *
 *================================================================
 */

static void cwh_funny_fp_intrinsic(INTRINSIC intr, INT numargs, WN **args, TY_IDX ty,
				   BOOL cvtf4, WN *ae)
{
   INT i;
   WN *wn;
   OPCODE opc;
   TYPE_ID t;
   
   t = TY_mtype(ty);

   /* Convert all REAL(4) arguments to type double */
   for (i=0; i < numargs; i++) {
      if (WN_rtype(args[i]) == MTYPE_F4 && cvtf4) {
	 args[i] = cwh_convert_to_ty(args[i],MTYPE_F8);
      }
      args[i] = cwh_intrin_wrap_value_parm(args[i]);
   }

   if (t == MTYPE_F4 && cvtf4) {
      opc = OPC_F8INTRINSIC_OP;
   } else {
      opc = cwh_make_typed_opcode(OPR_INTRINSIC_OP, t, MTYPE_V);
   }

   wn = WN_Create_Intrinsic(opc,intr,numargs,args);

   /* Convert back to REAL(4) if necessary */
   if (t == MTYPE_F4 && cvtf4) {
      wn = cwh_convert_to_ty(wn,MTYPE_F4);
   }
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push_typed(wn,WN_item,ty);
}


#define SELECT_INTRINSIC(t,f) ((t==MTYPE_F4) ? INTRN_F4##f : \
			       ((t==MTYPE_F8) ? INTRN_F8##f : INTRN_FQ##f))

void
fei_scalb(TYPE type)
{
   WN *args[2];
   INTRINSIC intr;
   TYPE_ID t;
   WN *ae=NULL;
   
   args[1] = cwh_get_typed_operand(MTYPE_I4,&ae);
   args[0] = cwh_expr_operand(&ae);
   t = WN_rtype(args[0]);
   intr = SELECT_INTRINSIC(t,SCALB);
   cwh_funny_fp_intrinsic(intr,2,args,Be_Type_Tbl(t),TRUE,ae);
}

void
fei_remainder(TYPE type)
{
   WN *args[2];
   INTRINSIC intr;
   TYPE_ID t;
   WN *ae=NULL;
   
   args[1] = cwh_expr_operand(&ae);
   args[0] = cwh_expr_operand(&ae);
   t = cwh_get_highest_type(args[0],args[1]);
   args[0] = cwh_convert_to_ty(args[0],t);
   args[1] = cwh_convert_to_ty(args[1],t);
   
   intr = SELECT_INTRINSIC(t,IEEE_REMAINDER);
   cwh_funny_fp_intrinsic(intr,2,args,Be_Type_Tbl(t),TRUE,ae);
}

void
fei_logb(TYPE type)
{
   WN *args[1];
   INTRINSIC intr;
   TYPE_ID t,rt,ot;
   WN  *wn;
   WN *ae=NULL;
   WN *argeq0;
   INT64 mhuge;
   
   rt = TY_mtype(cast_to_TY(t_TY(type)));
   args[0] = cwh_expr_operand(&ae);
   t = WN_rtype(args[0]);
   argeq0 = WN_EQ(t,WN_COPY_Tree(args[0]),WN_Zerocon(t));
   intr = SELECT_INTRINSIC(t,LOGB);
   cwh_funny_fp_intrinsic(intr,1,args,Be_Type_Tbl(t),TRUE,NULL);
   if (MTYPE_is_integral(rt)) {
      ot = MTYPE_I4;
      switch (rt) {
       case MTYPE_I1:
	  mhuge = 127LL;
	  break;
       case MTYPE_I2:
	  mhuge = 32767LL;
	  break;
       case MTYPE_I4:
	  mhuge = 2147483647LL; 
	  break;
       case MTYPE_I8:
       default:
	  mhuge = 9223372036854775807LL;
	  ot = MTYPE_I8;
	  break;
      }
      /* Need to limit the range to +HUGE(0), and set 0 to -HUGE(0)
       * because floating-point conversion doesn't get things quite right
       */
      wn = cwh_get_typed_operand(ot,NULL);
      if (rt == MTYPE_I1 || rt == MTYPE_I2) {
	 wn = WN_CreateExp2(OPC_I4MIN,wn,WN_Intconst(MTYPE_I4,mhuge));
      }
      wn = WN_Select(ot,argeq0,WN_Intconst(ot,-mhuge),wn);
      wn = cwh_wrap_cvtl(wn,rt);
   } else {
      wn = cwh_get_typed_operand(rt,NULL);
   }
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push_typed(wn,WN_item,Be_Type_Tbl(rt));
}

void
fei_isfinite(TYPE type)
{
   WN *args[1];
   INTRINSIC intr;
   TYPE_ID t;
   WN *ae=NULL;
   
   args[0] = cwh_expr_operand(&ae);
   t = WN_rtype(args[0]);
   intr = SELECT_INTRINSIC(t,FINITE);
   cwh_funny_fp_intrinsic(intr,1,args,logical4_ty,TRUE,ae);
}

void
fei_isnan(TYPE type)
{
   WN *args[1];
   INTRINSIC intr;
   TYPE_ID t;
   WN *ae=NULL;
   
   args[0] = cwh_expr_operand(&ae);
   t = WN_rtype(args[0]);
   intr = SELECT_INTRINSIC(t,ISNAN);
   cwh_funny_fp_intrinsic(intr,1,args,logical4_ty,FALSE,ae);
}

void
fei_isunordered(TYPE type)
{
   WN *args[2];
   INTRINSIC intr;
   TYPE_ID t;
   WN *ae=NULL;
   
   args[1] = cwh_expr_operand(&ae);
   args[0] = cwh_expr_operand(&ae);
   t = WN_rtype(args[0]);
   intr = SELECT_INTRINSIC(t,UNORDERED);
   cwh_funny_fp_intrinsic(intr,2,args,logical4_ty,TRUE,ae);
}

void
fei_fpclass(TYPE type)
{
   WN *args[1];
   INTRINSIC intr;
   TYPE_ID t;
   WN *ae=NULL;
   
   args[0] = cwh_expr_operand(&ae);
   t = WN_rtype(args[0]);
   intr = SELECT_INTRINSIC(t,FPCLASS);
   cwh_funny_fp_intrinsic(intr,1,args,Be_Type_Tbl(MTYPE_I4),FALSE,ae);
}

#define UNIMPLEMENTED(fname) void fname() {printf("%3d %s\n",__LINE__,# fname);}



/* Helper function for IEEE intrinsics */
static void
cwh_intrin_ieee_intrin_call_helper(INTRINSIC intrin, TYPE_ID type, INT nargs, 
				   BOOL issue_warning, const char * iname)
{
   BOOL v[2];
   WN *args[2];
   WN *sz[2];
   INT i;
   WN *wn;

   if (issue_warning && (Opt_Level != 0)) {
      ErrMsg(EC_IEEE_Intrinsic_Warning,iname);
   }
   
   sz[0] = NULL;
   sz[1] = NULL;
   v[0] = TRUE;
   v[1] = TRUE;
   
   for (i=nargs-1 ; i >= 0; i--) {
      args[i] = cwh_expr_operand(NULL);
   }

   cwh_intrin_call(intrin, nargs, args, sz, v, type);
   if (type != MTYPE_V ) {
      wn = cwh_stmt_return_scalar(NULL, NULL, Be_Type_Tbl(type), FALSE);
      cwh_stk_push(wn,WN_item);
   }
}

#define IEEE_INTRINCALL(name,intrin,rty,nargs,warn_msg) \
extern void name (void) {cwh_intrin_ieee_intrin_call_helper(INTRN_##intrin,rty,nargs,warn_msg,#intrin); }

IEEE_INTRINCALL(fei_set_all_estat,SET_IEEE_EXCEPTIONS,MTYPE_V,1,TRUE);
IEEE_INTRINCALL(fei_get_interupt,GET_IEEE_INTERRUPTS,MTYPE_I4,0,FALSE);
IEEE_INTRINCALL(fei_get_all_estat,GET_IEEE_EXCEPTIONS,MTYPE_I4,0,TRUE);
IEEE_INTRINCALL(fei_readsr,GET_IEEE_STATUS,MTYPE_I4,1,TRUE);
IEEE_INTRINCALL(fei_get_rmode,GET_IEEE_ROUNDING_MODE,MTYPE_I4,0,FALSE);
IEEE_INTRINCALL(fei_set_rmode,SET_IEEE_ROUNDING_MODE,MTYPE_V,1,TRUE);
IEEE_INTRINCALL(fei_set_ieee_stat,SET_IEEE_STATUS,MTYPE_V,1,TRUE);
IEEE_INTRINCALL(fei_set_interupt,SET_IEEE_INTERRUPTS,MTYPE_V,1,TRUE);
IEEE_INTRINCALL(fei_set_estat,SET_IEEE_EXCEPTION,MTYPE_V,2,TRUE);
IEEE_INTRINCALL(fei_dsbl_interupt,DISABLE_IEEE_INTERRUPT,MTYPE_V,1,TRUE);
IEEE_INTRINCALL(fei_enbl_interupt,ENABLE_IEEE_INTERRUPT,MTYPE_V,1,TRUE);



/* Helper function for IEEE intrinsics */
static void
cwh_intrin_ieee_intrin_helper(INTRINSIC intrin,BOOL issue_warning,const char *iname)
{
   WN *args;
   WN *sz;
   BOOL v;

   WN *wn;
   TY_IDX ty;
   WN *oldblock;
   
   if (issue_warning && (Opt_Level != 0)) {
      ErrMsg(EC_IEEE_Intrinsic_Warning,iname);
   }

   oldblock = cwh_block_new_and_current();
   
   args = cwh_expr_operand(NULL);
   sz = NULL;
   v = TRUE;
   cwh_intrin_call(intrin, 1, &args, &sz, &v, MTYPE_I4);
   
   /* Get the return value */
   wn = cwh_stmt_return_scalar(NULL, NULL, logical4_ty, FALSE);
   cwh_stk_push(wn,WN_item);
   wn = cwh_intrin_get_return_value(MTYPE_I4,"f90ieeelogval");
   
   /* Get the call block */
   oldblock = cwh_block_exchange_current(oldblock);

   /* Build the COMMA node */
   wn = WN_CreateComma(OPC_I4COMMA,oldblock,wn);
   cwh_stk_push_typed(wn,WN_item,logical4_ty);
}


void 
fei_test_interupt(void)
{
   cwh_intrin_ieee_intrin_helper(INTRN_TEST_IEEE_INTERRUPT,FALSE,NULL);
}

void 
fei_test_estat(void)
{
   cwh_intrin_ieee_intrin_helper(INTRN_TEST_IEEE_EXCEPTION,TRUE,"TEST_IEEE_EXCEPTION");
}


/*
 * OMP Intrinsics
 */
void fei_omp_set_lock(void)
{
   WN *args;
   WN *wn;
   INT64 flags = 0;
   
   if (!omp_set_lock_st) {
      omp_set_lock_st = cwh_intrin_make_intrinsic_symbol("omp_set_lock_",MTYPE_V);
   }
   args = cwh_expr_address(f_T_PASSED);
   cwh_stk_push(omp_set_lock_st,ST_item);
   cwh_stk_push(args,WN_item);
   cwh_stmt_call_helper(1,Be_Type_Tbl(MTYPE_V),0,flags);
   
   /* Insert the backward barrier */
   wn = WN_CreateBarrier( FALSE, 0 );
   cwh_block_append(wn);
}

void fei_omp_unset_lock(void)
{
   WN *args;
   WN *wn;
   INT64 flags = 0;
   
   if (!omp_unset_lock_st) {
      omp_unset_lock_st = cwh_intrin_make_intrinsic_symbol("omp_unset_lock_",MTYPE_V);
   }
   /* Insert the forward barrier */
   wn = WN_CreateBarrier( TRUE, 0 );
   cwh_block_append(wn);

   args = cwh_expr_address(f_T_PASSED);
   cwh_stk_push(omp_unset_lock_st,ST_item);
   cwh_stk_push(args,WN_item);
   cwh_stmt_call_helper(1,Be_Type_Tbl(MTYPE_V),0,flags);
   
}


void fei_omp_test_lock(void)
{
   WN *args;
   WN *wn;
   WN *rval;
   INT64 flags = 0;

   if (!omp_test_lock_st) {
      omp_test_lock_st = cwh_intrin_make_intrinsic_symbol("omp_test_lock_",MTYPE_I4);
   }

   args = cwh_expr_address(f_T_PASSED);
   cwh_stk_push(omp_test_lock_st,ST_item);
   cwh_stk_push(args,WN_item);
   cwh_stmt_call_helper(1,Be_Type_Tbl(MTYPE_I4),0,flags);
   rval = cwh_intrin_get_return_value(MTYPE_I4,"@f90testlock");

   /* Insert the backward barrier */
   wn = WN_CreateBarrier( FALSE, 0 );
   cwh_block_append(wn);

   /* Push the return value */
   cwh_stk_push_typed(rval,WN_item,logical4_ty);
}


/*================================================================
 *
 * Helper routine for synchronization intrinsics
 *
 *
 *================================================================
 */

static void 
cwh_intrin_sync_intrin(INTRINSIC i4intrin, INTRINSIC i8intrin, TYPE_ID rtype, INT num_args)
{
   WN *args[3]; /* Max number of arguments */
   BOOL v[3];
   WN *sz[3];
   INT i;
   ST *st;
   WN *wn;
   TYPE_ID atype;
   INTRINSIC intr;
   
   /* Insert the FORWARD_BARRIER */
   cwh_block_append(WN_CreateBarrier (TRUE, 0));
   atype = rtype;
   
   /* Get the arguments */
   for (i=num_args-1; i >= 0; i--) {
      sz[i] = NULL;
      if (i != 0) {
	 v[i] = TRUE;
	 args[i] = cwh_expr_operand(NULL);
      } else {
	 v[i] = FALSE;
	 if (cwh_stk_get_class() == ST_item) {
	    args[i] = cwh_expr_address(f_T_PASSED);
	 } else {
	    /* Need to store to a temp first */
	    wn = cwh_expr_operand(NULL);
	    atype = WNRTY(wn);
	    st = cwh_stab_temp_ST(Be_Type_Tbl(atype),"synctmp");
	    cwh_addr_store_ST(st,0,0,wn);
	    args[i] = cwh_addr_address_ST(st,0);
	    cwh_expr_set_flags(st,f_T_PASSED);
	 }
      }
   }

   /* Select the intrinsic type */
   if (rtype == MTYPE_V && atype != MTYPE_V) {
      intr = (atype == MTYPE_I8 ? i8intrin : i4intrin);
   } else if (rtype == MTYPE_V && atype == MTYPE_V) {
      intr = i4intrin;
   } else {
      intr = (rtype == MTYPE_I8 ? i8intrin : i4intrin);
   }
      
   /* Build the intrinsic_call node */
   cwh_intrin_call(intr, num_args, args, sz, v, rtype);

   if (rtype != MTYPE_V) {
      /* Get the return value */
      wn = cwh_stmt_return_scalar(NULL, NULL, Be_Type_Tbl(rtype), FALSE);
      cwh_stk_push(wn,WN_item);
      wn = cwh_intrin_get_return_value(rtype,"syncpreg");
      cwh_stk_push(wn,WN_item);
   }
   
   /* Insert the backward barrier */
   cwh_block_append(WN_CreateBarrier (FALSE, 0));
}

#define SYNC_INTRIN(name,iname,nargs) void name (TYPE type) {\
   cwh_intrin_sync_intrin(INTRN_##iname##_I4,INTRN_##iname##_I8,TY_mtype(cast_to_TY(t_TY(type))),nargs);}

/* syncronization intrinsics */
SYNC_INTRIN(fei_fetch_and_add,FETCH_AND_ADD,2)
SYNC_INTRIN(fei_fetch_and_and,FETCH_AND_AND,2)
SYNC_INTRIN(fei_fetch_and_nand,FETCH_AND_NAND,2)
SYNC_INTRIN(fei_fetch_and_or,FETCH_AND_OR,2)
SYNC_INTRIN(fei_fetch_and_sub,FETCH_AND_SUB,2)
SYNC_INTRIN(fei_fetch_and_xor,FETCH_AND_XOR,2)
SYNC_INTRIN(fei_add_and_fetch,ADD_AND_FETCH,2)
SYNC_INTRIN(fei_and_and_fetch,AND_AND_FETCH,2)
SYNC_INTRIN(fei_nand_and_fetch,NAND_AND_FETCH,2)
SYNC_INTRIN(fei_or_and_fetch,OR_AND_FETCH,2)
SYNC_INTRIN(fei_sub_and_fetch,SUB_AND_FETCH,2)
SYNC_INTRIN(fei_xor_and_fetch,XOR_AND_FETCH,2)
SYNC_INTRIN(fei_compare_and_swap,COMPARE_AND_SWAP,3)
SYNC_INTRIN(fei_lock_test_and_set,LOCK_TEST_AND_SET,2)

void
fei_synchronize (void) 
{
   cwh_intrin_sync_intrin(INTRN_SYNCHRONIZE,INTRN_SYNCHRONIZE,MTYPE_V,0);
}

void
fei_lock_release(void) 
{
   cwh_intrin_sync_intrin(INTRN_LOCK_RELEASE_I4,INTRN_LOCK_RELEASE_I8,MTYPE_V,1);
}

#ifdef KEY /* Bug 1324 */
static void
help_make_intrin_symbol(ST **symbol, const char *func_name, TYPE_ID result_type) {
  if (! *symbol) {
     *symbol = cwh_intrin_make_intrinsic_symbol(func_name,result_type);
  }
}

/*===============================================
 *
 * fei_erf
 *
 * Generate function call for erf, erfc
 *
 *===============================================
 */ 
void
fei_erf (TYPE type, int complement) {
   WN *addr;
   WN *call;
   INT64 flags = 0;

   const char *func_name;
   TYPE_ID result_type = TY_mtype(cast_to_TY(t_TY(type)));
   ST *symbol;

   if (complement) {
     if (MTYPE_F4 == result_type) {
       func_name = "erfc_";
       symbol = erfc_st;
       }
     else {
       func_name = "derfc_";
       symbol = derfc_st;
       }
     }
   else {
     if (MTYPE_F4 == result_type) {
       func_name = "erf_";
       symbol = erf_st;
       }
     else {
       func_name = "derf_";
       symbol = derf_st;
       }
     }

   help_make_intrin_symbol(&symbol, func_name, result_type);

   addr = cwh_expr_address(f_T_PASSED);
   cwh_stk_push(symbol,ST_item);
   cwh_stk_push(addr,ADDR_item);
   call = cwh_stmt_call_helper(1,Be_Type_Tbl(result_type),0,flags);
   WN *wn = cwh_intrin_get_return_value (result_type,"@f90erf{c}");
   cwh_stk_push(wn,WN_item);
} /* fei_erf */
#endif /* KEY Bug 1324 */

/*===============================================
 *
 * cwh_intrin_call
 *
 * Make an intrinsic call, given the intrinsic
 * and a list of arguments. The kids will be 
 * wrapped with PARM nodes (side effect..).
 *
 * If a character address is a passed in, then
 * a SZ entry describing the substring size has
 * to be provided. The boolean flags say if
 * value or reference parms are required.
 *
 * appends the call to the current block, returns the call node
 *
 *===============================================
 */ 
extern WN *
cwh_intrin_call(INTRINSIC intr, INT16 numargs, WN ** k, WN**sz, BOOL *v, TYPE_ID bt )
{
  INT16  i   ;
  OPCODE opc ;
  WN    * wn ;

  opc = cwh_make_typed_opcode(OPR_INTRINSIC_CALL, bt, MTYPE_V);

  for (i = 0 ; i < numargs; i++) {
    if (v[i]) 
      k[i] = cwh_intrin_wrap_value_parm(k[i]);
    else if (sz[i] != NULL)
      k[i] = cwh_intrin_wrap_char_parm(k[i],sz[i]);
    else
      k[i] = cwh_intrin_wrap_ref_parm(k[i], (TY_IDX) NULL);
  }

  wn = WN_Create_Intrinsic(opc,intr,numargs,k);

  WN_Set_Call_Default_Flags(wn);

  cwh_block_append(wn);

  return (wn);
}

/*===============================================
 *
 * cwh_intrin_op
 *
 * Make an intrinsic op, given the intrinsic
 * and a list of arguments. The kids will be 
 * wrapped with PARM nodes (side effect..).
 *
 * If a character address is a passed in, then
 * a SZ entry describing the substring size has
 * to be provided. The boolean flags say if
 * value or reference parms are required.
 *
 *===============================================
 */ 

extern WN *
cwh_intrin_op(INTRINSIC intr, INT16 numargs, WN ** k, WN**sz, BOOL *v, TYPE_ID bt )
{
  INT16  i   ;
  OPCODE opc ;
  WN    * wn ;

  opc = cwh_make_typed_opcode(OPR_INTRINSIC_OP, bt, MTYPE_V);

  for (i = 0 ; i < numargs; i++) {
    if (v[i]) 
      k[i] = cwh_intrin_wrap_value_parm(k[i]);
    else if (sz[i] != NULL)
      k[i] = cwh_intrin_wrap_char_parm(k[i],sz[i]);
    else
      k[i] = cwh_intrin_wrap_ref_parm(k[i], (TY_IDX) NULL);
  }

  wn = WN_Create_Intrinsic(opc,intr,numargs,k);

  return(wn);
}

/*===============================================
 *
 * cwh_intrin_build
 *
 * Make an intrinsic op, given the intrinsic,
 * its result type and a list of arguments. The args
 * will be wrapped as value parms (side effect).
 *
 *===============================================
 */ 
static WN *
cwh_intrin_build(WN **k, INTRINSIC intr,TYPE_ID bt, INT numargs)
{
  INT i;
  OPCODE opc;
  WN  *wn ;
  
  opc = cwh_make_typed_opcode(OPR_INTRINSIC_OP, bt, MTYPE_V);

  for (i = 0 ; i < numargs; i++) 
    k[i] = cwh_intrin_wrap_value_parm(k[i]);

  wn = WN_Create_Intrinsic(opc,intr,numargs,k);

  return wn ;
}

    
/*=============================================
 *
 *  cwh_whirl_simplfier_control(BOOL onoff);
 *
 *  We may need to turn the simplifier off at some point in the 
 *  processing of some expressions. For each call with FALSE, we increment the
 *  number of times we've been called, and turn off the simplifier. 
 *  Each TRUE turns it back on again.
 *
 *=============================================
 */
extern void
cwh_whirl_simplfier_control(BOOL onoff)
{
}
