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
 * Module: cwh_expr
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:57:32-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: 
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: contains routines for expressions and conversions.
 *              Entry points from PDGCS layer are those for
 *              unary & binary operations eg: 
 *                fei_plus,fei_bneg, fei_max..
 * 
 *              The Cray IR doesn't contain explicit conversions
 *              so each operand is checked, and OPC_CVRTLs sprinkled
 *              around. Small operands - I1,U2 etc are also converted
 *              to the minumun WH sizes I4,U4. 
 *
 *              The source of operands is the expression stack, via
 *              the routine cwh_expr_operand. Usually that's where
 *              the result goes too.
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_expr.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */


/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "stab.h"
#include "strtab.h"
#include "errors.h"
#include "config_targ.h"
#include "targ_const.h"
#include "wn.h"
#include "wn_util.h"
#include "const.h"
#include "f90_utils.h"
#include "sgi_cmd_line.h"

/* Cray includes */

#include "i_cvrt.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_addr.h"
#include "cwh_expr.h"
#include "cwh_block.h"
#include "cwh_types.h"
#include "cwh_preg.h"
#include "cwh_stab.h"
#include "cwh_auxst.h"
#include "cwh_stmt.h"
#include "cwh_stk.h"
#include "cwh_expr.h"
#include "cwh_intrin.h"
#include "cwh_preg.h"

static void cwh_expr_binop(OPERATOR op, TY_IDX  result_ty) ;
static void cwh_expr_unop(OPERATOR  op,TY_IDX  result_ty) ;
static WN * cwh_expr_compare_char(OPERATOR op, TY_IDX  ty) ;


/*================================================================
 *
 * cwh_expr_extract_arrayexp
 *
 * Pulls an ARRAYEXP off the top of wn. If *arrayexp is non-null,
 * delete the ARRAYEXP. IF *arrayexp is NULL, return the ARRAYEXP in it.
 * If arrayexp == 1, then delete the arrayexp.
 *
 *================================================================
 */
extern WN *
cwh_expr_extract_arrayexp(WN *wn, WN **arrayexp)
{
   WN * ae;
   INT i;
   if (Full_arrayexp) {
      if (WNOPR(wn) == OPR_ARRAYEXP && arrayexp != NULL) {
	 ae = wn;
	 wn = WN_kid0(wn);
	 
	 /* Check to see if we should delete it */
	 if (arrayexp != DELETE_ARRAYEXP_WN && *arrayexp == NULL) {
	    *arrayexp = ae;
	 } else {
	    for (i = 1; i < WN_kid_count(ae); i++) {
	       WN_DELETE_Tree(WN_kid(ae,i));
	    }
	    WN_Delete(ae);
	 }
      }
   }

   return (wn);
}

/*================================================================
 *
 * cwh_expr_restore_arrayexp
 *
 * Puts an ARRAYEXP back on top of wn. If arrayexp is non-null,
 * put it on top of the expression.
 *
 *================================================================
 */
extern WN *
cwh_expr_restore_arrayexp(WN *wn, WN *arrayexp)
{
   OPCODE opc;

   if (Full_arrayexp && arrayexp) {
      WN_kid0(arrayexp) = wn;
      opc = cwh_make_typed_opcode(OPR_ARRAYEXP,WN_rtype(wn),MTYPE_V);
      WN_set_opcode(arrayexp,opc);
      return (arrayexp); 
   } else {
      return (wn);
   }
}

/*================================================================
 *
 * cwh_wrap_cvtl 
 *
 * Wrap a CVTL around an expressions for small types
 *
 *==================================================================
 */
extern WN * 
cwh_wrap_cvtl(WN * wn, TYPE_ID ty)
{
  return F90_wrap_cvtl(wn,ty);
}

/*================================================================
 *
 * cwh_convert_to_ty
 *
 * Convert a node to a new type. Needed because we don't
 * have explicit converts in the trees from the Cray IR. It's also
 * a useful routine to have around.
 *
 *================================================================
 */
extern WN * 
cwh_convert_to_ty(WN * wn, TYPE_ID ty)
{
   TYPE_ID old_ty,real_ty,new_real_ty;
   OPCODE  cvt_op;
   OPCODE  realpart,imagpart;
   WN *r;
   WN *ri,*rr;
   
   old_ty = WNRTY(wn);
   if (old_ty == ty) return (wn);

   if (old_ty == MTYPE_I1 || old_ty == MTYPE_I2) {
      /* treat as it it were an I4 */
      old_ty = MTYPE_I4;
   }

   r = wn;
   cvt_op = OPCODE_UNKNOWN;
   

   /* Special case for TAS nodes */
   if (WNOPR(wn) == OPR_TAS) {
      WN_set_opcode(wn,OPCODE_make_op(OPR_TAS,ty,MTYPE_V));
      return (wn);
   }

   if (MTYPE_is_complex(old_ty)) {
      real_ty  = Mtype_complex_to_real(old_ty);
      realpart = OPCODE_make_op(OPR_REALPART,real_ty,MTYPE_V);
      imagpart = OPCODE_make_op(OPR_IMAGPART,real_ty,MTYPE_V);
      
      /* complex to non-complex */

      if (!MTYPE_is_complex(ty)) { 
	 r = WN_CreateExp1(realpart,r);
	 r = cwh_convert_to_ty(r,ty);

      } else { 	 /* complex to complex */

	 new_real_ty = Mtype_complex_to_real(ty);
	 rr = WN_CreateExp1(realpart,WN_COPY_Tree(r));
	 rr = cwh_convert_to_ty(rr,new_real_ty);
	 ri = WN_CreateExp1(imagpart,r);
	 ri = cwh_convert_to_ty(ri,new_real_ty);
	 r  = WN_CreateExp2(OPCODE_make_op(OPR_COMPLEX,ty,MTYPE_V),rr,ri);
      }
      return (r);

   } else if (MTYPE_is_complex(ty)) {
      real_ty = Mtype_complex_to_real(ty);
      cvt_op  = OPCODE_make_op(OPR_COMPLEX,ty,MTYPE_V);
      r = cwh_convert_to_ty(r,real_ty);
      r = WN_CreateExp2(cvt_op,r,Make_Zerocon(real_ty));
      return (r);
   }


   if (ty == MTYPE_I1 || ty == MTYPE_I2) {
      /* First convert to I4, then do a CVTL */
      r = cwh_convert_to_ty(wn,MTYPE_I4);
      r = cwh_wrap_cvtl(r,ty);
      return (r);
   }

   if (ty == MTYPE_U1 || ty == MTYPE_U2) {
      /* First convert to I4, then do a CVTL */
      r = cwh_convert_to_ty(wn,MTYPE_U4);
      r = cwh_wrap_cvtl(r,ty);
      return (r);
   }


   if (MTYPE_is_float(ty)) {
      /* Converts to float */
      cvt_op = OPCODE_make_op(OPR_CVT,ty,old_ty);
   } else if (MTYPE_is_float(old_ty)) {
      /* Converts from float to integer */
      cvt_op = OPCODE_make_op(OPR_TRUNC,ty,old_ty);
   } else { 
      /* Integral to integral */
      if (MTYPE_size_reg(ty) != MTYPE_size_reg(old_ty)) {
	 cvt_op = OPCODE_make_op(OPR_CVT,ty,old_ty);
      }
   }

   /* See if there is a single op to do the conversion */
   if (cvt_op != 0) {
      r = WN_CreateExp1(cvt_op,r);
   }

   return (r);
}

/*===============================================
 *
 * cwh_get_highest_type
 *
 * Utility routine for type conversions, etc.
 *
 * Takes two operands, and converts them so that both
 * are of the "greater" (in the fortran sense) type.
 *
 *===============================================
 */
extern TYPE_ID
cwh_get_highest_type(WN *lhs, WN *rhs)
{
   TYPE_ID t1,t2,r;
   t1 = WN_rtype(lhs);
   t2 = WN_rtype(rhs);
   
   /* Types are the same, nothing to do */
   if (t1 == t2) return (t1);
   
   if (MTYPE_is_complex(t1) && !MTYPE_is_complex(t2)) {
      t1 = Mtype_complex_to_real(t1);
      if (MTYPE_type_order(t2) > MTYPE_type_order(t1)) {
	 r = t2;
      } else {
	 r = t1;
      }
      /* Convert r to complex */
      switch (r) {
       case MTYPE_F4: r = MTYPE_C4; break;
       case MTYPE_F8: r = MTYPE_C8; break;
       case MTYPE_FQ: r = MTYPE_CQ; break;
      }
   } else if (MTYPE_is_complex(t2) && !MTYPE_is_complex(t1)) {
      t2 = Mtype_complex_to_real(t2);
      if (MTYPE_type_order(t2) > MTYPE_type_order(t1)) {
	 r = t2;
      } else {
	 r = t1;
      }
      /* Convert r to complex */
      switch (r) {
       case MTYPE_F4: r = MTYPE_C4; break;
       case MTYPE_F8: r = MTYPE_C8; break;
       case MTYPE_FQ: r = MTYPE_CQ; break;
      }
   } else {
      /* No complexes, return the greatest in type order */
      if (MTYPE_type_order(t2) > MTYPE_type_order(t1)) {
	 r = t2;
      } else {
	 r = t1;
      }
   }
   return (r);
}

/*===============================================
 *
 * cwh_get_typed_operand
 *
 * Utility routine for type conversions, etc.
 *
 * Pops an operand from the stack and converts 
 * it to type ty if necessary.
 *
 * arrexp is treated as it is for cwh_expr_operand
 *
 *===============================================
 */ 
extern WN *
cwh_get_typed_operand(TYPE_ID ty, WN **arrexp)
{
   WN *r;

   r = cwh_expr_operand(arrexp);
   r = cwh_convert_to_ty(r,ty);
   return (r);
}

/*===============================================
 *
 * cwh_make_typed_opcode
 *
 * Build an opcode from a type, also checking 
 * for small types and converting them to 32 bits.
 *
 *===============================================
 */ 
extern OPCODE 
cwh_make_typed_opcode(OPERATOR op, TYPE_ID ty1, TYPE_ID ty2)
{
   OPCODE opc;
   TYPE_ID ti ;

   switch (ty1) {
    case MTYPE_B:
    case MTYPE_I1:
    case MTYPE_I2:
       ti = MTYPE_I4;
       break ;
       
    case MTYPE_U1:
    case MTYPE_U2:
       ti = MTYPE_U4;
       break ;
       
    default:
       ti = ty1;
       break;
   }
   opc = OPCODE_make_op(op,ti,ty2);
   return (opc);
}

/*===============================================
 *
 * cwh_expr_binop
 *
 * Apply the binop to the two operands at the
 * top of the stack and push the result. Conversions
 * are added to lhs & rhs if required.
 *
 *===============================================
 */ 
static void
cwh_expr_binop(OPERATOR op,TY_IDX  result_ty)
{

  WN *rhs ;
  WN *lhs ;
  WN *wn  ;
  TYPE_ID bt  ;
  OPCODE  opc ;
  TYPE_ID ot;
  WN *ae=NULL;
 

  rhs = cwh_expr_operand(&ae);
  lhs = cwh_expr_operand(&ae);

  ot  = cwh_get_highest_type(rhs,lhs);
  if (result_ty) {
     bt  = TY_mtype(result_ty) ;
  } else {
     bt = ot;
  }
  opc = cwh_make_typed_opcode(op, ot, MTYPE_V);
  lhs = cwh_convert_to_ty(lhs,ot);
  rhs = cwh_convert_to_ty(rhs,ot);
  
  wn = WN_CreateExp2 ( opc, lhs, rhs) ;

  /* Need to insert a CVTL on top of the small ops */
  wn = cwh_wrap_cvtl(wn,bt);
  
  wn = cwh_expr_restore_arrayexp(wn,ae);
  cwh_stk_push_typed(wn,WN_item,result_ty) ;
}

/*===============================================
 *
 * cwh_expr_binop_shift
 *
 * Apply the binop to the two operands at the
 * top of the stack and push the result. The
 * shifts yield integer results and can be applied
 * to float operands.
 *
 *===============================================
 */ 
static void
cwh_expr_binop_shift(OPERATOR op, TY_IDX  result_ty)
{

  WN *arg   ;
  WN *shft  ;
  WN *wn    ;
  WN *temp  ;
  WN *ae=NULL;

  TYPE_ID bt  ;
  TYPE_ID ret_t;
  TYPE_ID br  ;
  TYPE_ID ba  ;
  OPCODE  opc ;
  INT     bitlen;
  INT     reslen;
 
  br   = TY_mtype(result_ty) ;
  ret_t = br;
  reslen = MTYPE_size_best(br);
  shft = cwh_expr_operand(&ae);
  arg  = cwh_expr_operand(&ae);

  bt = WNRTY(arg);
  bitlen = MTYPE_size_best(bt);
   
  if (reslen < 32 && op == OPR_LSHR) {
     /* Need to clear out the upper bits */
     arg = WN_Band(bt,arg,WN_Intconst(bt,(1<<reslen)-1));
  }

  if (bitlen <= MTYPE_size_best(MTYPE_U4))
    ba = MTYPE_I4 ;
  else
    ba = MTYPE_I8 ;

  if (reslen > 32) {
     br = MTYPE_I8;
  } else {
     br = MTYPE_I4;
  }

  if (!MTYPE_is_integral(bt)) 
    arg = WN_Tas(ba,Be_Type_Tbl(bt),arg) ;

  opc = cwh_make_typed_opcode(op, br, MTYPE_V);
  if (op == OPR_ASHR) {
    /* shift is MIN(shift,reslen-1) */
    if (ARCH_mask_shift_counts) {
      temp = WN_GT(br,WN_COPY_Tree(shft),WN_Intconst(br,reslen-1));
      temp = cwh_convert_to_ty(temp,br);
      temp = WN_Neg(br,temp);
      shft = WN_Bior(br,temp,shft);
    }
    wn = WN_CreateExp2 (opc, arg, shft);
  } else {
    if (ARCH_mask_shift_counts) {
      /* shift is (arg op shift ) & -(shift<reslen) */
      temp = WN_LT(br,WN_COPY_Tree(shft),WN_Intconst(br,reslen));
      temp = cwh_convert_to_ty(temp,br);
      temp = WN_Neg(br,temp);
      wn = WN_CreateExp2 (opc, arg, shft);
      wn = WN_Band(br,wn,temp);
    } else {
      wn = WN_CreateExp2 (opc, arg, shft);
    }
  }
  
  wn = cwh_wrap_cvtl(wn,ret_t);
  
  wn = cwh_expr_restore_arrayexp(wn,ae);
  cwh_stk_push_typed(wn,WN_item,result_ty);
}

/*===============================================
 *
 * cwh_expr_compare
 *
 * Apply the compare binop to the two operands at the
 * top of the stack and push the result. Convert 
 * both operands 'highest' type before selecting
 * operator.
 *
 * Character operations are diverted to create
 * the correct intrinsic.
 *
 *===============================================
 */ 
extern void
cwh_expr_compare(OPERATOR op,TY_IDX  ty)
{

  WN *rhs ;
  WN *lhs ;
  WN *wn  ;
  WN *ae=NULL;

  TYPE_ID bt  ;
  OPCODE  opc ;
 
  if (cwh_stk_get_class() == STR_item) 
    wn = cwh_expr_compare_char(op,ty);

  else {

    rhs = cwh_expr_operand(&ae);
    lhs = cwh_expr_operand(&ae);
    
    bt  = cwh_get_highest_type(rhs,lhs);
    opc = cwh_make_typed_opcode(op, MTYPE_I4, Mtype_comparison(bt));
    lhs = cwh_convert_to_ty(lhs,bt);
    rhs = cwh_convert_to_ty(rhs,bt);
    
    wn  = WN_CreateExp2 ( opc, lhs, rhs) ;
    wn  = cwh_expr_restore_arrayexp(wn,ae);
  }

  cwh_stk_push_typed(wn,WN_item,ty);
}

/*===============================================
 *
 * cwh_expr_compare_char
 *
 * Convert the compare binop into an intrinsic OP
 * and return the WN of the op. Assumes two
 * STR_items are TOS.
 *
 *===============================================
 */ 
static WN * 
cwh_expr_compare_char(OPERATOR op, TY_IDX  ty)
{
  WN * ar[4];
  WN * sz[4];
  BOOL va[4];
  WN * wn   ;
#ifdef KEY /* Bug 10177 */
  INTRINSIC intr = INTRN_CLTEXPR;
#else /* KEY Bug 10177 */
  INTRINSIC intr;
#endif /* KEY Bug 10177 */

  cwh_stk_pop_STR();
  ar[3] = cwh_expr_operand(NULL);
  ar[1] = cwh_expr_address(f_NONE);

  sz[3] = NULL;
  sz[1] = WN_COPY_Tree(ar[3]);
  va[3] = TRUE;
  va[1] = FALSE;

  cwh_stk_pop_STR();
  ar[2] = cwh_expr_operand(NULL);
  ar[0] = cwh_expr_address(f_NONE);

  sz[2] = NULL;
  sz[0] = WN_COPY_Tree(ar[2]);
  va[2] = TRUE;
  va[0] = FALSE;

  switch(op) {
  case OPR_LT:
    intr = INTRN_CLTEXPR;
    break ;
  case OPR_LE:
    intr = INTRN_CLEEXPR;
    break ;
  case OPR_GE:
    intr = INTRN_CGEEXPR;
    break ;
  case OPR_GT:
    intr = INTRN_CGTEXPR;
    break ;
  case OPR_EQ:
    intr = INTRN_CEQEXPR;
    break ;
  case OPR_NE:
    intr = INTRN_CNEEXPR;
    break ;

  default:
    DevAssert((0),("Missing char comp"));

  }
  wn = cwh_intrin_op(intr,4,ar,sz,va,TY_mtype(ty));
  wn = F90_Wrap_ARREXP(wn);
  return (wn);
}

/*===============================================
 *
 * cwh_expr_compare_logical
 *
 * Logical WN comparisons don't have a type
 * so just pop the items, stick the operator 
 * on top and push the result.
 *
 *===============================================
 */ 
static void
cwh_expr_compare_logical(OPCODE opc,TY_IDX ty)
{
  WN * rhs;
  WN * lhs;
  WN * wn ;
  WN *ae=NULL;

  rhs = cwh_expr_operand(&ae);
  lhs = cwh_expr_operand(&ae);
  wn  = WN_CreateExp2 ( opc, lhs, rhs) ;
  wn  = cwh_expr_restore_arrayexp(wn,ae);

  cwh_stk_push_typed(wn,WN_item,ty);
}

/*===============================================
 *
 * cwh_expr_compare_bitwise
 *
 * Bitwise WN comparisons for all types. Convert
 * bits to integer type (if they aren't already) 
 * using TAS operate & push the result.
 * The result type should be either I8 or I4.
 *
 *===============================================
 */ 
static void
cwh_expr_compare_bitwise(OPERATOR op,TY_IDX  ty)
{
  WN * rhs;
  WN * lhs;
  WN * wn ;
  TY_IDX  ta ; 

  TYPE_ID bt;
  TYPE_ID br;
  TYPE_ID ba;
  TYPE_ID rhs_t,lhs_t;
  OPCODE  opc;
  WN *ae=NULL;

  bt  = br = TY_mtype(ty);
  if (bt == MTYPE_U4) br = MTYPE_I4 ;
  if (bt == MTYPE_U8) br = MTYPE_I8 ;

  rhs = cwh_expr_operand(&ae) ;
  lhs = cwh_expr_operand(&ae) ;
  rhs_t = WN_rtype(rhs);
  lhs_t = WN_rtype(lhs);

  ta  = cwh_types_scalar_TY(cwh_types_WN_TY(rhs,FALSE));
  ba  = TY_mtype(ta);

  if (!MTYPE_is_integral(rhs_t)) {
     rhs = WN_Tas(br,ta,rhs)  ;
  } 
  if (!MTYPE_is_integral(lhs_t)) {
     lhs = WN_Tas(br,ta,lhs)  ;
  } 
  opc = cwh_make_typed_opcode(op,br,MTYPE_V);
  wn  = WN_CreateExp2 ( opc, lhs, rhs) ;

  wn = cwh_expr_restore_arrayexp(wn,ae);
  cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * fei_lneg
 *
 * .not. processing - similar to cwh_expr_unop,
 * except result doesn't have a type.
 *
 *===============================================
 */ 
extern void
fei_lneg(TYPE result)
{
  WN * lhs;
  WN * wn ;
  WN *ae=NULL;

  lhs = cwh_expr_operand(&ae);
  wn  = WN_CreateExp1(OPC_I4LNOT, lhs) ;

  wn = cwh_expr_restore_arrayexp(wn,ae);
  cwh_stk_push_typed(wn,WN_item,(INTPTR)cast_to_TY(t_TY(result)));
}

/*===============================================
 *
 * cwh_expr_unop
 *
 * Apply the unop to the operand at the
 * top of the stack and push the result.
 * The operand is converted to the type
 * of the result.
 *
 *===============================================
 */ 
static void
cwh_expr_unop(OPERATOR op,TY_IDX  result_ty)
{

  WN *lhs ;
  WN *wn  ;
  WN *ae=NULL;

  TYPE_ID bt  ;
  OPCODE  opc ;
 
  bt  = TY_mtype(result_ty) ;
  opc = cwh_make_typed_opcode(op, bt, MTYPE_V);
  lhs = cwh_get_typed_operand(bt,&ae);

  wn = WN_CreateExp1 ( opc, lhs) ;
  wn = cwh_wrap_cvtl(wn,bt);
  
  wn = cwh_expr_restore_arrayexp(wn,ae);
  cwh_stk_push(wn,WN_item);
}

/*===================================================
 *
 * cwh_expr_bincalc
 *
 * Given a binary op, and two WNs create the WN of
 * the result. Used by address calculations, only. 
 * The type of the result is the type of the first
 * argument.
 *
 ====================================================
*/
extern  WN *
cwh_expr_bincalc(OPERATOR op, WN * wn1, WN * wn2)
{
  TYPE_ID bt ;

  bt = cwh_get_highest_type(wn1,wn2);
  wn1 = cwh_convert_to_ty(wn1,bt);
  wn2 = cwh_convert_to_ty(wn2,bt);

  return WN_CreateExp2 (OPCODE_make_op(op,bt,MTYPE_V),
			wn1, 
			wn2) ;
}

/*===============================================
 *
 * cwh_expr_operand
 *
 * Pop the top of the stack and make it
 * into an operand. If it's a WN, then it may be a
 * constant or expression, and it's just returned, 
 * unless an OPC_ARRAY or OPC_ARRSECTION when
 * a load is added. An ST requires a load. A FLD
 * loads from its offset.
 *
 * if arrexp is non-null, and the returned node is an 
 * ARRAYEXP node, *arrexp is set to the ARRAYEXP node, and 
 * the first child if the ARRAYEXP is returned.
 *===============================================
 */ 
extern WN *
cwh_expr_operand(WN **arrexp)
{
  WN  * wn  ;
  ST  * st  ;
  TY_IDX ts  ;

  FLD_det det;

  ts = cwh_stk_get_TY();
 
  switch(cwh_stk_get_class()) {
  case WN_item:
  case WN_item_whole_array:
    wn = cwh_stk_pop_WN();
    if (wn == NULL)
      return(wn);

    if (cwh_addr_is_array(wn)) {
       wn = cwh_addr_load_WN(wn,0,ts);
    } else if (cwh_addr_is_section(wn)) {
       wn = cwh_addr_load_WN(wn,0,ts);
       if (Full_arrayexp) {
	  wn = F90_Wrap_ARREXP(wn);
       }
    } 
    wn = cwh_expr_extract_arrayexp(wn,arrexp);
    break  ;
    
  case ADDR_item:
    wn = cwh_stk_pop_ADDR();
    break ;

  case DEREF_item:
    wn = cwh_stk_pop_DEREF();
    wn = cwh_addr_load_WN(wn,0,0);
    break ;

  case ST_item:
  case ST_item_whole_array:
    st  = cwh_stk_pop_ST();
    wn  = cwh_addr_load_ST(st,0,0);
    break ;

  case FLD_item:
    det = cwh_addr_offset();

    if (cwh_stk_get_class() == ST_item || cwh_stk_get_class() == ST_item_whole_array) {
      st  = cwh_stk_pop_ST();
      wn  = cwh_addr_load_ST(st,det.off,det.type);

    } else {

      wn = cwh_stk_pop_WHIRL();
      wn = cwh_expr_extract_arrayexp(wn,DELETE_ARRAYEXP_WN);
      wn = cwh_addr_load_WN(wn,det.off,det.type);
      if (Full_arrayexp) {
	 wn = F90_Wrap_ARREXP(wn);
      }
      wn = cwh_expr_extract_arrayexp(wn,arrexp);
    }
    break ;
    
  case PCONST_item:
    st = (ST *) cwh_stk_pop_PCONST();
    wn = cwh_addr_address_ST(st);
    break;

  default:
    DevAssert((0),("Bad operand"));
  }
  return (wn);
}

/*===============================================
 *
 * cwh_expr_address
 *
 * Makes an address of the top of the stack and 
 * returns it.  A STR_item has its length thrown
 * away. A WN constant gets an LDA, otherwise its
 * assumed an WN is an address. An INTCONST is 
 * entered into the symbol table & an expression
 * may be saved. An address on the stack may be NULL
 * if the dope vector routines have already processed it.
 * FLD items are similar to WN_items, but get an
 * ADD of the offset to the component. The TY of
 * the FLD is thrown away.
 *
 * Sets ST_addr bits depending of setting of flag
 *
 *===============================================
 */ 
extern WN *
cwh_expr_address(FLAG flag)
{
  WN * wn ;
  ST * st ;
  
  FLD_det  det ;
  
  switch(cwh_stk_get_class()) {
  case WN_item:
  case WN_item_whole_array:
  case ADDR_item:
  case DEREF_item:
    wn = cwh_stk_pop_WHIRL();
    
    if (wn) {
       if (flag) { 
	  st = cwh_addr_WN_ST(wn);
	  cwh_expr_set_flags(st, flag);
       }
    }
    break;
       
  case ST_item:
  case ST_item_whole_array:
    st = cwh_stk_pop_ST();
    wn = cwh_addr_address_ST(st);
    if (flag) 
       cwh_expr_set_flags(st, flag);
    break;

  case STR_item:
    cwh_stk_pop_STR();
    WN_Delete(cwh_expr_operand(NULL)); /* Get rid of the length */
    wn = cwh_expr_address(flag);
    break;

  case FLD_item:
    det = cwh_addr_offset();

    if (cwh_stk_get_class() == ST_item || 
	cwh_stk_get_class() == ST_item_whole_array) {

      st  = cwh_stk_pop_ST();
      wn  = cwh_addr_address_ST(st,det.off,det.type);
      if (flag) 
	cwh_expr_set_flags(st, flag);

    } else {
      wn = cwh_expr_address(flag);
      wn = cwh_expr_bincalc(OPR_ADD,wn,WN_Intconst(Pointer_Mtype,det.off));
    }
    break ;

  default:
    DevAssert((0),("Odd address"));
  }

  return (wn);
}

/*===============================================
 *
 * fei_<binop>
 *
 * Use common binary op routine - one of
 * cwh_expr_binop,cwh_expr_compare,
 * cwh_expr_binop_shift
 * cwh_expr_compare_logical
 * cwh_expr_compare_bitwise

 *===============================================
 */ 

#define binop_routine(name,opr) \
extern void name (TYPE type) \
{ \
  cwh_expr_binop(opr,cast_to_TY(t_TY(type))); \
}

#define binop_shift_routine(name,opr) \
extern void name (TYPE type) \
{ \
  cwh_expr_binop_shift(opr,cast_to_TY(t_TY(type))); \
}

#define compare_routine(name,opr) \
extern void name (TYPE type) \
{ \
  cwh_expr_compare(opr,cast_to_TY(t_TY(type))); \
}

#define compare_logical(name,opr_l,opr_c) \
extern void name (TYPE type) \
{ \
  cwh_expr_compare_logical(FTN_Short_Circuit_On ? opr_c : opr_l,cast_to_TY(t_TY(type))); \
}

#define compare_bitwise(name,opr) \
extern void name (TYPE type) \
{ \
  cwh_expr_compare_bitwise(opr,cast_to_TY(t_TY(type))); \
}
binop_routine(fei_plus,OPR_ADD)
binop_routine(fei_minus,OPR_SUB)
binop_routine(fei_mult,OPR_MPY)
binop_routine(fei_div,OPR_DIV)
compare_routine(fei_gt,OPR_GT)
compare_routine(fei_ge,OPR_GE)
compare_routine(fei_lt,OPR_LT)
compare_routine(fei_le,OPR_LE)
compare_routine(fei_eq,OPR_EQ)
compare_routine(fei_ne,OPR_NE)
compare_bitwise(fei_and,OPR_BAND)
compare_bitwise(fei_xor,OPR_BXOR)
compare_logical(fei_land ,OPC_I4LAND, OPC_I4CAND)
compare_routine(fei_leqv ,OPR_EQ)
compare_logical(fei_lor ,OPC_I4LIOR, OPC_I4CIOR)
binop_shift_routine(fei_lshift ,OPR_SHL)
binop_shift_routine(fei_rshift ,OPR_LSHR)
binop_shift_routine(fei_ashift ,OPR_ASHR)
compare_routine(fei_lxor ,OPR_NE)
compare_bitwise(fei_or ,OPR_BIOR)


/* Bitwise equivalence */
extern void 
fei_eqv(TYPE type)
{
   fei_xor(type);
   fei_bneg(type);
}

/*===============================================
 *
 * fei_islg
 *
 * != comparison, implemented as is less than or 
 * greater than comparison.
 * 
 *===============================================
 */ 
extern void 
fei_islg(TYPE type)
{
   WN *arg1, *arg2, *r1, *r2;

   arg1 = cwh_expr_operand(NULL);
   arg2 = cwh_expr_operand(NULL);
   cwh_stk_push(WN_COPY_Tree(arg2),WN_item);
   cwh_stk_push(WN_COPY_Tree(arg1),WN_item);
   cwh_expr_compare(OPR_LT,0);

   r1 = cwh_expr_operand(NULL);
   cwh_stk_push(arg2,WN_item);
   cwh_stk_push(arg1,WN_item);
   cwh_expr_compare(OPR_GT,0);

   r2 = cwh_expr_operand(NULL);
   cwh_stk_push(r1,WN_item);
   cwh_stk_push(r2,WN_item);
   fei_lor(type);
}

extern void 
fei_multiply_high(TYPE type)
{
   /* Can't use cwh_expr_binop, since we need UNSIGNED multiply */
   WN *rhs ;
   WN *lhs ;
   WN *wn  ;
   OPCODE  opc ;
   TYPE_ID ot;
   WN *ae=NULL;
   
   rhs = cwh_expr_operand(&ae);
   lhs = cwh_expr_operand(&ae);

   ot  = cwh_get_highest_type(rhs,lhs);
   if (ot == MTYPE_I8) {
      opc = OPC_U8HIGHMPY;
      ot = MTYPE_U8;
   } else {
      opc = OPC_U4HIGHMPY;
      ot = MTYPE_U4;
   }
      
   lhs = cwh_convert_to_ty(lhs,ot);
   rhs = cwh_convert_to_ty(rhs,ot);
  
   wn = WN_CreateExp2 ( opc, lhs, rhs) ;
  
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * fei_<unop>
 *
 * Use common unary op routine
 *
 *===============================================
 */ 
#define unop_routine(name,opr) \
extern void name (TYPE type) \
{ \
  cwh_expr_unop(opr,cast_to_TY(t_TY(type))); \
}

/*===============================================
 *
 * fei_imag
 *
 * Get the imaginary part
 *
 *===============================================
 */ 
extern void
fei_imag(TYPE type)
{
   WN *rhs, *wn;
   TY_IDX ty;
   TYPE_ID t,rt;
   WN *ae=NULL;

   ty = cast_to_TY(t_TY(type));
   t  = TY_mtype(ty) ;
   rhs = cwh_expr_operand(&ae);
   rt = Mtype_complex_to_real(WN_rtype(rhs));
   wn = WN_CreateExp1(cwh_make_typed_opcode(OPR_IMAGPART,rt,MTYPE_V),rhs);
   
   wn = cwh_convert_to_ty(wn,t);
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * fei_bneg
 *
 * Bitwise NOT on top of stack
 *
 *===============================================
 */ 
extern void
fei_bneg(TYPE type)
{

  WN *lhs ;
  WN *wn  ;
  WN *ae=NULL;

  TYPE_ID bt, lhs_t  ;
  OPCODE  opc ;
  TY_IDX ta, result_ty;

  result_ty = cast_to_TY(t_TY(type));
  bt  = TY_mtype(result_ty) ;

  if (MTYPE_is_unsigned(bt)) {
     bt = MTYPE_complement(bt);
  }

  lhs = cwh_expr_operand(&ae) ;
  lhs_t = WN_rtype(lhs);

  ta  = cwh_types_scalar_TY(cwh_types_WN_TY(lhs,FALSE));
  if (!MTYPE_is_integral(lhs_t)) {
     lhs = WN_Tas(bt,ta,lhs)  ;
  } 

  opc = cwh_make_typed_opcode(OPR_BNOT, bt, MTYPE_V);

  wn = WN_CreateExp1 ( opc, lhs) ;
  wn = cwh_wrap_cvtl(wn,bt);
  
  wn = cwh_expr_restore_arrayexp(wn,ae);
  cwh_stk_push_typed(wn,WN_item,Be_Type_Tbl(bt));
}

unop_routine(fei_uminus,OPR_NEG)

/*===============================================
 *
 * fei_paren
 *
 * If type is integer/logical ignore, as WHIRL
 * doesn't have an suitable operator code. Otherwise
 * use the unary operator routine.
 *
 *===============================================
 */ 
extern void
fei_paren(TYPE type)
{

  TY_IDX  ty ;
  TYPE_ID t;

  ty = cast_to_TY(t_TY(type));
  ty = cwh_types_scalar_TY(ty);
  t = TY_mtype(ty);
  
  if (MTYPE_is_float(t) || MTYPE_is_complex(t)) { 
     cwh_expr_unop(OPR_PAREN,ty);
  }
}

/*===============================================
 *
 * fei_max/min
 *
 * create max/min via common binop routine.
 *
 *===============================================
 */ 
extern void 
fei_max(INT count, TYPE type)
{
   INT i;
   for (i = 1; i < count; i++) {
      cwh_expr_binop(OPR_MAX,cast_to_TY(t_TY(type)));
   }

}
extern void 
fei_min(INT count, TYPE type)
{
   INT i;
   for (i = 1; i < count; i++) {
      cwh_expr_binop(OPR_MIN,cast_to_TY(t_TY(type)));
   }
}

/*===============================================
 *
 * fei_select
 *
 * Implement the MERGE intrinsic. The mask is
 * TOS, then T items, F items and the destination.
 *
 * Generally a select is issued, and pushed onto
 * the stack so the result can be used, eg: by
 * fei_store. If it's an aggregate - a character 
 * or derived type, then the MERGE intrinsic op 
 * is used.
 *
 * This also implements the CVM... intrinsics
 *
 *===============================================
 */ 
extern void 
#ifdef KEY /* Bug 10410 */
fei_select(TYPE type, int cselect)
#else /* KEY Bug 10410 */
fei_select(TYPE type)
#endif /* KEY Bug 10410 */
{
   WN *t_case,*f_case,*condition;
   WN * wn;
   TY_IDX  ty;
   WN *strlen;
   WN *addr;
   TYPE_ID bt;
   TYPE_ID rt;
   WN *args[3];
   WN *ae=NULL;

   ty = cast_to_TY(t_TY(type));

   condition = cwh_expr_operand(&ae);

   if (TY_is_character(ty)) {
      cwh_stk_pop_STR();
      strlen = cwh_expr_operand(NULL);
      addr   = cwh_expr_address(f_NONE);
      f_case = cwh_addr_mload(addr,0,ty,strlen);

      cwh_stk_pop_STR();
      strlen = cwh_expr_operand(NULL);
      addr   = cwh_expr_address(f_NONE);
      t_case = cwh_addr_mload(addr,0,ty,strlen);

   } else {
      f_case = cwh_expr_operand(&ae);
      t_case = cwh_expr_operand(&ae);
   }

   bt = WN_rtype(t_case);

   if (bt == MTYPE_M) {  

     /* character,derived type - build an intrinsic_op */
    
      args[0] = cwh_intrin_wrap_value_parm(condition);
      args[1] = cwh_intrin_wrap_value_parm(t_case);
      args[2] = cwh_intrin_wrap_value_parm(f_case);
      if (TY_is_character(ty)) {
         wn = WN_Create_Intrinsic(OPC_U4INTRINSIC_OP,INTRN_MERGE,3,args);
	 cwh_stk_push_STR(WN_COPY_Tree(strlen),wn,ty,WN_item);
      } else {
         wn = WN_Create_Intrinsic(OPC_MINTRINSIC_OP,INTRN_MERGE,3,args);
	 wn = cwh_expr_restore_arrayexp(wn,ae);
	 cwh_stk_push(wn,WN_item);
      }
   } else {
      /* 
       * In order to handle the CVMGT intrinsic and its ilk, we check the base types
       * of the TRUE and FALSE cases and wrap them with TAS as necessary.
       */
      rt = TY_mtype(ty);
      if (MTYPE_is_integral(rt)) {
	 if (!MTYPE_is_integral(WNRTY(t_case))) {
	    t_case = WN_Tas(rt,Be_Type_Tbl(WNRTY(t_case)),t_case) ;
	 }
	 if (!MTYPE_is_integral(WNRTY(f_case))) {
	    f_case = WN_Tas(rt,Be_Type_Tbl(WNRTY(f_case)),f_case) ;
	 }
      }
      
#ifdef KEY /* Bug 10410 */
      wn = WN_CreateExp3(
        cwh_make_typed_opcode((cselect ? OPR_CSELECT : OPR_SELECT),rt,MTYPE_V),
	  condition,t_case,f_case);
#else /* KEY Bug 10410 */
      wn = WN_CreateExp3(cwh_make_typed_opcode(OPR_SELECT,rt,MTYPE_V),condition,t_case,f_case);
#endif /* KEY Bug 10410 */
      wn = cwh_wrap_cvtl(wn,rt);
      wn = cwh_expr_restore_arrayexp(wn,ae);
      cwh_stk_push_typed(wn,WN_item,Be_Type_Tbl(rt));
   }
}


/*=============================================
 *
 * fei_cvtop
 *
 * Convert TOS to the given type. 
 *
 *=============================================
 */
extern void 
fei_cvtop(TYPE type)
{
  WN *wn  ;
  TYPE_ID bt ;
  TYPE_ID ot ;
  WN *addr;
  TY_IDX ty;
  WN *ival;
  WN *icall;
  WN *ae=NULL;

  ty = cast_to_TY(t_TY(type));
  
  /* Check for converts to TYPELESS, for which we just use a TAS */
  
  if (type.basic_type == T_ypeless) {
    wn = cwh_expr_operand(&ae);
    ot = WNRTY(wn) ;

    if (!MTYPE_is_integral(ot)) {
       wn = WN_Tas(TY_mtype(ty),Be_Type_Tbl(ot),wn) ;
    }

    wn = cwh_expr_restore_arrayexp(wn,ae);
    cwh_stk_push_typed(wn,WN_item,ty);

  } else if (TY_is_character(ty)) {
    
    ival = cwh_intrin_wrap_value_parm(cwh_expr_operand(&ae));
    icall = WN_Create_Intrinsic(OPC_U4INTRINSIC_OP,INTRN_CHAR,1,&ival);
    icall = cwh_expr_restore_arrayexp(icall,ae);
    cwh_stk_push_STR(WN_Intconst(MTYPE_I4,1),icall,ty,WN_item);
    
  } else { 
    
    bt = TY_mtype(ty);
    
    if (cwh_stk_get_class() == STR_item) {
      cwh_stk_pop_STR();
      WN_Delete(cwh_expr_operand(NULL)); /* Get rid of the length */
      addr = cwh_expr_address(f_NONE);
	
      if (WN_opcode(addr) == OPC_U4INTRINSIC_OP &&
	  WN_intrinsic(addr) == INTRN_CHAR) {
	   
	addr = cwh_expr_dispose_of_char(addr);	   

	/* ichar(char(x)) ==> x & 255 */
	wn = WN_Band(MTYPE_I4,addr,WN_Intconst(MTYPE_I4,255));
	wn = F90_Wrap_ARREXP(wn);
      } else {

	wn = cwh_addr_load_WN(addr,0,Be_Type_Tbl(MTYPE_U1));
	wn = WN_Band(MTYPE_I4,wn,WN_Intconst(MTYPE_I4,255));
	/* Convert to bt */
	wn = cwh_convert_to_ty(wn,bt);
      }
    } else {
      wn = cwh_get_typed_operand(bt,&ae);
    }

    if (WNOPR(wn) == OPR_INTCONST) {
       wn = cwh_expr_restore_arrayexp(wn,ae);
       cwh_stk_push_typed(wn,WN_item,ty);
    } else {
       wn = cwh_expr_restore_arrayexp(wn,ae);
       cwh_stk_push_typed(wn,WN_item,ty);
    }
    
  } /* Non-character */
  
}

/*=============================================
 *
 * fei_len
 *
 * Used to establish the length of a DUMMY argument
 * or to implement the LEN function applied
 * to a character expression.
 *
 * TOS is an ST if the length of a DUMMY or
 * a STR_item if it's a character expression.
 * In both cases we push the length. For a
 * dummy we have to look up the value in the
 * list of arguments associated with the 
 * current entry point (dummy args are
 * processed in the preamble).
 * 
 * An ST may also occur for a CHARACTER pointer.
 *
 *=============================================
 */
extern void
fei_len(TYPE type)
{
  ST * st ;  
  ST * ln ;
  WN * wn ;

 
  switch(cwh_stk_get_class()) {
  case ST_item:
  case ST_item_whole_array:
    st = cwh_stk_pop_ST();
    ln = cwh_auxst_find_dummy_len(st);
    if (ln == NULL) {
      if (ST_sclass(st) == SCLASS_FORMAL) 
	  Fatal_Error ("Unsupported LEN on character dummy : %s",ST_name(st));
      else 
	  Fatal_Error ("No LEN type parameter: %s", ST_name(st));
			 
    }
    cwh_stk_push(ln,ST_item);
    break;

  case STR_item:
    cwh_stk_pop_STR();
    wn = cwh_expr_operand(NULL);
    cwh_stk_pop_whatever();
    cwh_stk_push(wn,WN_item);
    break;

  default:
    DevAssert((0),("Odd LEN"));

  }
}

/*=============================================
 *
 * fei_null_expr
 *
 * push a null WN onto the stack.
 *
 *=============================================
 */
extern void
fei_null_expr (void)
{
  WN *null_wn = NULL;
  cwh_stk_push(null_wn,WN_item);
}


/*
 * Generate a mask of len bits of type TY
 * valid for 0<= len <= 64
 */
extern 
WN * cwh_generate_bitmask(WN *len, TYPE_ID ty)
{
   WN *mask;
   if (MTYPE_size_reg(ty) != 64 || !ARCH_mask_shift_counts) {
      /* Optimization: we use 64 bit shift */
      mask = WN_Intconst(MTYPE_I8,1);
   } else {
      /* Need to be a little more clever */
      mask = WN_NE(ty,WN_Intconst(MTYPE_I8,64),WN_COPY_Tree(len));
      mask = cwh_convert_to_ty(mask,MTYPE_I8);
   }
   
   mask = WN_Shl(MTYPE_I8,mask,len);
   mask = cwh_expr_bincalc(OPR_SUB,mask,WN_Intconst(MTYPE_I8,1));
   mask = cwh_convert_to_ty(mask,ty);
   return (mask);
}

/*=============================================
 *
 * fei_mask
 *
 * push a bitmask on the stack.
 * MASK(I) is 
 *   set leftmost I bits if I < bitlen. This is -1 << (bitlen - I)
 *   set rightmost 2*bitlen-I bits if I >= bitlen
 *
 *=============================================
 */
extern void
fei_mask (TYPE type)
{
#ifdef KEY /* Bug 10177 */
   WN *wn = 0;
   WN *arg,*t1,*t2;
#else /* KEY Bug 10177 */
   WN *wn,*arg,*t1,*t2;
#endif /* KEY Bug 10177 */
   TYPE_ID t;
   WN *ae=NULL;

   t = TY_mtype(cast_to_TY(t_TY(type)));

   arg = cwh_expr_operand(&ae);
   
   switch (t) {

    case MTYPE_U1:
    case MTYPE_I1:
      /* we can use a neat little trick for this */
      wn = WN_CreateExp2(OPC_I4LSHR,WN_Intconst(MTYPE_I4,0xff00LL),arg);
      wn = cwh_convert_to_ty(wn,MTYPE_I1);
      break;

    case MTYPE_U2:
    case MTYPE_I2:
      /* we can use a neat little trick for this */
      wn = WN_CreateExp2(OPC_I4LSHR,WN_Intconst(MTYPE_U4,0xffff0000LL),arg);
      wn = cwh_convert_to_ty(wn,MTYPE_I2);
      break;

    case MTYPE_U4:
    case MTYPE_I4:
      /* we can use a neat little trick for this */
      wn = WN_CreateExp2(OPC_I8LSHR,WN_Intconst(MTYPE_I8,0xffffffff00000000LL),arg);
      wn = cwh_convert_to_ty(wn,MTYPE_I4);
      break;
    case MTYPE_U8:
    case MTYPE_I8:
       /* Uglier code:
	* ~(-1 >> arg) if arg < 64
	*  bitmask (128 - arg) if arg >= 64
	*/
       t1 = cwh_expr_bincalc(OPR_LSHR,WN_Intconst(MTYPE_I8,-1),WN_COPY_Tree(arg));
       t1 = WN_CreateExp1(OPC_I8BNOT,t1);
       t2 = cwh_expr_bincalc(OPR_SUB,WN_Intconst(MTYPE_I8,128),WN_COPY_Tree(arg));
       t2 = cwh_generate_bitmask(t2,MTYPE_I8);
       wn = WN_CreateExp2(OPC_I4I8LT,arg,WN_Intconst(t,64));
       wn = WN_CreateExp3(OPC_I8SELECT,wn,t1,t2);
   }
   
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push_typed(wn,WN_item,Be_Type_Tbl(t));
}


/*=============================================
 *
 * fei_mbits
 *
 * CSMG(x1, x2, x3)
 *
 * Bit by bit selective merge.  (x1 AND x3) OR (x2 AND   NOT x3)
 *
 * x1, x2, x3 are all the same length objects  (the FE assures this)
 *
 *
 *=============================================
 */
extern void
fei_mbits (TYPE type)
{
   WN *wn,*a1,*a2,*mask;
   WN *ae=NULL;

   mask = cwh_expr_operand(&ae);
   a2 = cwh_expr_operand(&ae);
   a1 = cwh_expr_operand(&ae);

   cwh_stk_push(a1,WN_item);
   cwh_stk_push(WN_COPY_Tree(mask),WN_item);
   fei_and(type);  /* Stack contains a1 & mask */


   cwh_stk_push(mask,WN_item);
   fei_bneg(type); /* Stack contains NOT mask */


   cwh_stk_push(a2,WN_item);
   fei_and(type);
   fei_or(type);
   
   wn =  cwh_expr_operand(NULL);
   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}



/*=============================================
 *
 * fei_new_binop_cshift
 *
 * Implement a circular left-shift. May be applied
 * to types other than integers, so convert operand
 * via TAS if required.
 *
 *=============================================
 */
extern void
fei_new_binop_cshift (TYPE type)
{
   WN *wn,*shift,*arg;
   WN *t1;
   WN *ae=NULL;
   INT64 bitlen;
   TYPE_ID bt ;
   TYPE_ID br ;

   shift = cwh_expr_operand(&ae);
   arg   = cwh_expr_operand(&ae);
   
   bt = WNRTY(arg);
   bitlen = MTYPE_size_best(bt);
   
   if (bitlen <= MTYPE_size_best(MTYPE_U4))
     br = MTYPE_I4 ;
   else
     br = MTYPE_I8 ;

   if (!MTYPE_is_integral(bt)) 
     arg = WN_Tas(br,Be_Type_Tbl(bt),arg) ;

   t1 = cwh_expr_bincalc(OPR_SUB,WN_Intconst(MTYPE_I4,bitlen),WN_COPY_Tree(shift));
   t1 = cwh_expr_bincalc(OPR_LSHR,WN_COPY_Tree(arg),t1);
   
   wn = cwh_expr_bincalc(OPR_SHL,arg,shift);
   wn = cwh_expr_bincalc(OPR_BIOR,wn,t1);
   wn = cwh_wrap_cvtl(wn,bt);

   wn = cwh_expr_restore_arrayexp(wn,ae);
   cwh_stk_push(wn,WN_item);
}


/*=============================================
 *
 * cwh_expr_temp
 *
 * Return the address of a temp. If e_sz is 
 * NULL or constant the temp depends on the TY, 
 * and a temp is generated, otherwise an alloca 
 * is issued and a preg returned with the address. 
 *
 * If an array valued temp is needed, then the TY 
 * should reflect this. e_sz is intepreted as the 
 * size of an element, so if TY_size == 0, the 
 * element size can stand in.
 *
 * If the temp is created within a parallel region
 * it should be marked as LOCAL in the pragma list
 *
 * Flag is PASSED/SAVED etc - typical for an address
 *
 *=============================================
 */
extern WN * 
cwh_expr_temp(TY_IDX  ty, WN * e_sz, FLAG flag)
{
  ST * st ;
  TY_IDX  tp ;
  WN * wr ;
  WN * nl[1] ;  
  WN * wn[1] ;
  BOOL va[1] ;
  WN *free_stmt;

  PREG_det  det;

  if (e_sz == NULL && TY_size(ty) != 0) {

    st = cwh_stab_temp_ST(ty,TY_name(ty));
    cwh_expr_set_flags(st,flag);
    wr = cwh_addr_address_ST(st);


  } else if (WNOPR(e_sz) == OPR_INTCONST && TY_size(ty) != 0) {

    st = cwh_stab_temp_ST(ty,TY_name(ty));
    cwh_expr_set_flags(st,flag);
    wr = cwh_addr_address_ST(st);
    
  } else {
     DevAssert((e_sz!=NULL),("NULL element size in cwh_expr_temp"));

    if (TY_kind(ty) == KIND_ARRAY)
      wn[0] = cwh_types_size_WN(ty,e_sz);
    else
      wn[0] = e_sz  ;

    nl[0] = NULL;
    va[0] = TRUE;

    /* pregs are assumed LOCAL by the MP lowerer */

    det = cwh_preg_next_preg(Pointer_Mtype,"concat_temp",NULL);
    wr  = cwh_intrin_op(INTRN_F90_STACKTEMPALLOC,1,wn,nl,va,Pointer_Mtype);

    tp = cwh_types_make_pointer_type(ty, FALSE);

    cwh_addr_store_ST(det.preg_st,det.preg,tp,wr);

    wr  = cwh_addr_load_ST(det.preg_st,det.preg,tp);

    /* Add call to free temp to defer list, set flags so it won't be moved before alloc */

    wn[0] = cwh_intrin_wrap_value_parm(WN_COPY_Tree(wr));
    free_stmt = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,INTRN_F90_STACKTEMPFREE,1,wn);
    WN_Set_Call_Non_Parm_Ref(free_stmt);
    WN_Set_Call_Non_Parm_Mod(free_stmt);
    cwh_block_append_given_id(free_stmt,Defer_Block,FALSE);
  }

  return(wr);
}

/*===============================================
 *
 * cwh_expr_temp_set_pragma
 *
 * If a temp is created within a parallel region
 * it may require a LOCAL pragma to be set in 
 * the parallel region.
 *
 *===============================================
 */
extern void
cwh_expr_temp_set_pragma(ST *st)
{
  cwh_block_add_to_enclosing_regions(WN_PRAGMA_LOCAL,st);
}

/*===============================================
 *
 * cwh_expr_str_operand
 *
 * Assumes stack top contains an STR_item; pops the 
 * string marker and then returns the address and the
 * length of the string in the argument passed to the 
 * routine.
 *
 *===============================================
 */

extern void
cwh_expr_str_operand(W_node expr[2])
{
  WN * wn;

  cwh_stk_pop_STR();

  wn = cwh_expr_operand(NULL);
  W_ty(expr[0]) = cwh_types_WN_TY(wn,FALSE);
  W_wn(expr[0]) = wn;

  W_ty(expr[1]) = cwh_stk_get_TY();
  W_wn(expr[1]) = cwh_expr_address(f_NONE);

}

/*===============================================
 *
 * cwh_expr_set_flags
 *
 * set the requested flags on an ST. In 7.3, the
 * addr_passed flag is removed & the information
 * computed by wopt.
 *
 *===============================================
 */
extern void
cwh_expr_set_flags(ST *st, FLAG flag)
{
   if (st != NULL)
     if ((ST_class(st) == CLASS_VAR) ||
	 (ST_class(st) == CLASS_FUNC)) {
	if (flag & f_T_SAVED)  Set_ST_addr_saved(st);
     }
}

/*===============================================
 *
 * cwh_expr_dispose_of_char
 *
 * The CHAR intrinsic is converted to temp and
 * address of temp in the f90 lowerer. In some
 * circumstances we want the value immediately.
 *
 * Take the intrinsic op and PARM off the top,
 * and return the value. There may be an ARRAYEXP
 * in the way, as it would have a ARRAY argumnet
 * to the intrinsic.
 *
 *===============================================
 */
extern WN *
cwh_expr_dispose_of_char(WN * src)
{
  WN * wn;
  WN * wn1;

  if (WN_operator(src) == OPR_ARRAYEXP) {
    wn  = WN_kid0(src);
    wn1 =  cwh_expr_dispose_of_char(wn);
    if (wn != wn1)
      WN_kid0(src) = wn1;

  } else if (WN_operator(src) == OPR_INTRINSIC_OP &&
      WN_intrinsic(src) == INTRN_CHAR) {
	   
    wn = WN_kid0(src);
    WN_Delete(src);
    src = WN_kid0(wn);
    WN_Delete(wn);
  }
  return src;
}
