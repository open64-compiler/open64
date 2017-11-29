/*
 * Copyright (C) 2008, 2009. PathScale, LLC. All Rights Reserved.
 */
/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: cwh_addr
 * $Revision: 1.5 $
 * $Date: 05/07/01 14:43:32-07:00 $
 * $Author: fchow@fluorspar.internal.keyresearch.com $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: contains routines to convert address operations.
 *              Entry points from PDGCS layer are 
 *               fei_seq_subscr              
 *               fei_subscr_triplet
 *               fei_subscr_size
 *               fei_substr
 *               fei_as_ref
 *               fei_field_dot 
 *               fei_addr
 *               fei_fcd
 * 
 *              general routines to address,store or load STs & WNs
 *              are here.
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "stab.h"
#include "strtab.h"
#include "errors.h"
#include "config_targ.h"
#include "config_debug.h"
#include "wn.h"
#include "wn_util.h"
#include "wn_trap.h"
#include "f90_utils.h"
#include "pu_info.h"

/* Cray includes */

#include "i_cvrt.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_stk.h"
#include "cwh_preg.h"
#include "cwh_stab.h"
#include "cwh_auxst.h"
#include "cwh_block.h"
#include "cwh_types.h"
#include "cwh_stmt.h"
#include "cwh_stab.h"
#include "cwh_expr.h"
#include "cwh_io.h"
#include "cwh_intrin.h"
#include "cwh_dst.h"
#include "sgi_cmd_line.h"
#include "cwh_addr.h"
#include "cwh_addr.i"

/*===============================================
 *
 * fei_seq_subscr
 *
 * Handles a single subscript in a reference to
 * a sequential array (cf. fei_nseq_subscr), hence
 * the stride multiplier is ignored - if there is
 * a stride then it has already been folded into 
 * the subscript triplet (and the stride multiplier
 * would be one..).
 *
 * The stack has stride mult, extent, lower bound,
 * subscript. The subscript may be a scalar, or an
 * array value (OPC_TRIPLET). Below the subscript 
 * is the address expression - an ST or OPC_ARRAY 
 * or OPC_ARRSECTION. If the address is an ST or 
 * OPC_ARRAY, it may be a section subscript hasn't
 * been seen yet, so if one appears, make an OPC_ARRSECTION.
 * 
 * Pop lb,extent, (ignore stride mult) & subscript 
 * & look at the address TOS. Possibilities :
 * 
 * TOS is ST - make OPC_ARRAY, or if subscript
 *             array valued an OPC_ARRSECTION.
 *
 * TOS is WN - may be OPC_ARRAY (convert to OPC_ARRSECTION
 *             if array-valued subscript) or OPC_ARRSECTION 
 *             
 * TOS is FLD - add the offset to an OPC_ARRAY or
 *              OPC_ARRSECTION.
 *
 * subscript is OPC_TRIPLET, or ST or WN  - tack it in
 *              to the address OPC_ARRAY/ARRSECTION.
 *
 * subscript is OPC_ARRSECTION - vv valued subscript
 *              tack on an OPC_ARRAYEXP and LDID to
 *              subscript & add to the address 
 *              OPC_ARRAY/ARRSECTION.
 *  
 * Make the bound zero-based.
 *
 *===============================================
 */ 

extern void
fei_seq_subscr( TYPE result_type )
{
  WN *ex  ;
  WN *lb  ;
  WN *sb  ;
  WN *ar  ;
  WN *ad  ;
  WN *wt  ;
  ST *st  ;
  TY_IDX ty  ;

  BOOL    array_val ;
  BOOL    sect ;
  BOOL    trip ;
  TY_IDX  ta   ;

  OPCODE  op   ;
  FLD_det det  ;
  WN * bounds_assertion;
  char *field_name,*array_name;

  (void) cwh_stk_pop_whatever(); /* stride mult*/
  ex = cwh_expr_operand(NULL) ;
  lb = cwh_expr_operand(NULL) ;
  sb = cwh_expr_operand(NULL) ;
  bounds_assertion = cwh_addr_do_bounds_check(sb, lb, ex);

  trip = cwh_addr_is_triplet(sb); 
  sb   = cwh_addr_zero_based(sb,lb);   
  sb   = F90_Wrap_ARREXP(sb);
  sect = WNOPR(sb) == OPR_ARRAYEXP;

  array_val = sect || trip ;
  op = array_val ? opc_section : opc_array ;

  switch(cwh_stk_get_class()) {
  case ADDR_item:
  case WN_item:
    ta = cwh_stk_get_TY();
    ar = cwh_expr_address(f_NONE);    
    /* ar had better be an ARRAY or ARRSECTION node */
    if (array_val) 
      if (cwh_addr_is_array(ar))
	WN_set_opcode(ar, opc_section) ; 

    cwh_addr_insert_bounds_check(bounds_assertion,ar);
    ar = cwh_addr_add_bound(ar,ex,sb);
    cwh_stk_push_typed(ar,WN_item,ta);
    break  ;

  case WN_item_whole_array:
    ta = cwh_stk_get_TY();	/* TRAP HERE dlai DLAI */
    ar = cwh_expr_address(f_NONE);    
    if (array_val) 
      if (cwh_addr_is_array(ar))
	WN_set_opcode(ar, opc_section) ; 

    cwh_addr_insert_bounds_check(bounds_assertion,ar);
    ar = cwh_addr_add_bound(ar,ex,sb);
    cwh_stk_push_typed(ar,WN_item_whole_array,ta);
    break  ;

  case ST_item:
    st = cwh_stk_pop_ST();
    ty = ST_type(st);
    ad = cwh_addr_address_ST(st) ;
    ar = cwh_addr_array(op,ad,ty);
    SET_ARRAY_NAME_MAP(ar,ST_name(st));
    cwh_addr_insert_bounds_check(bounds_assertion,ar);
    ar = cwh_addr_add_bound(ar,ex,sb);
    cwh_stk_push(ar,WN_item);
    break ;

  case ST_item_whole_array:
    st = cwh_stk_pop_ST();
    ty = ST_type(st);
    ad = cwh_addr_address_ST(st) ;
    ar = cwh_addr_array(op,ad,ty);
    SET_ARRAY_NAME_MAP(ar,ST_name(st));
    cwh_addr_insert_bounds_check(bounds_assertion,ar);
    ar = cwh_addr_add_bound(ar,ex,sb);
    cwh_stk_push(ar,WN_item_whole_array);
    break ;

  case FLD_item:
    field_name = cwh_stk_fld_name();
    det = cwh_addr_offset() ;
    
      /* Preserve TY info for the FLD        */
      /* (OPC_ARRAY doesn't hold a type      */
      /* a type and the fundemental address  */ 
      /* TY is that of a parent object )     */

    if (cwh_stk_get_class() == ST_item || 
        cwh_stk_get_class() == ST_item_whole_array) {

      st = cwh_stk_pop_ST();
      ad = cwh_addr_address_ST(st,det.off,det.type);
      array_name = ST_name(st);

    } else { 

      /* is array of array of derived type   */
      /* or similar.                         */

      ad = cwh_expr_address(f_NONE);
      array_name = GET_ARRAY_NAME_MAP(ad);
      wt = WN_CreateIntconst(opc_pint,det.off);
      ad = cwh_expr_bincalc(OPR_ADD,ad,wt);
      
    }

    ar = cwh_addr_array(op,ad,det.type) ;
    if (strlen(field_name) > 0) {

       if (array_name) {
	  array_name = Index_To_Str(Save_Str2(array_name,field_name));
       } else {
	  array_name = Index_To_Str(Save_Str2("(unknown)",field_name));
       }
       free(field_name);
       SET_ARRAY_NAME_MAP(ar,array_name);
    }
    cwh_addr_insert_bounds_check(bounds_assertion,ar);
    ar = cwh_addr_add_bound(ar,ex,sb);
    cwh_stk_push_typed(ar,WN_item,det.type);
    break ;

  default:
    DevAssert((0),(" odd item in subscr"));
  }
}

/*===============================================
 *
 * cwh_addr_compute_stride_fudge_factor
 *
 * This routine takes a TY and computes whether 
 * the stride multiplier is in words or bytes.
 * It returns 4 if it's in words, 1 if in bytes.  
 *
 * ifndef NONCONTIG_BY_DIVIDE version
 *===============================================
 */ 
static INT64 
cwh_addr_compute_stride_fudge_factor(TY_IDX in)
{

   TY_IDX ty_idx = cwh_types_array_TY(in);
   TY& t = Ty_Table[ty_idx];
   DevAssert((TY_kind(t)==KIND_ARRAY),("can't get fudge factor for non-array type"));
   TY& ty = Ty_Table[TY_etype(t)];

#define RETURN4 return(-4)
#define RETURN2 return(-2)
#define RETURN1 return(-1)

   switch (TY_kind(ty)) {
    case KIND_SCALAR:
      /* Should be in words */
      if (TY_size(ty) >= 4) {
	 RETURN4;
      } else if (TY_size(ty) == 2) {
	 RETURN2;
      } else {
	 RETURN1;
      }

    case KIND_ARRAY:
      RETURN1;

    case KIND_STRUCT:
      if (TY_is_packed(ty)) {
	 RETURN1;
      } else {
	 RETURN4;
      }
      
    default:
      DevAssert((0),("Don't know how to deal with this ty"));
   }
   RETURN4;
}


/* ================================================================

Notes about non-contiguous array lowering

A noncontiguous array (an F90 pointer or assumed-shape dummy)
is passed by a dope vector containing 

1) a base address
2) lower bounds for each dimension
3) stride multipliers for each dimension

The unfortunate consequence of this is that the indexing methodology of the ARRAY
node no longer works. Also unfortunately, the stride multipliers are in words
(or sometimes bytes), not elements. So, the actual address expression for an array element
A(I1...In) is

base + element_size*(SUM(i=1,n) (Ii-lbound(i))*(stride_mult(i)/fudge))

So, we build:

ARRAY (-4 or -1)
   base address
   stride_mult_n
     .
     .
     .
   stride_mult_1
   (In - lbound n)
     .
     .
     .
   (I1 - lbound 1)

The negative element size indicates that the extents are actually stride multipliers.

Ugly, but it works.

================================================================*/

static void cwh_addr_fixup_nseq(WN **ex, WN **sb, WN *sm)
{
   
   /* This is a helper routine which alters the subscript and extent
    * for the cases in which we need to fold in the stride_multiplier.
    */
   if (!may_be_noncontig) return;
   
   WN_DELETE_Tree(*ex);
   *ex = sm;
   return;
}

/*===============================================
 *
 * fei_nseq_subscr
 *
 * Non-contiguous section subscript. This is
 * similar to fei_seq_subscr, but the stride
 * multiplier is used to compute the stride.
 *
 * The stack has stride mult extent,lb,subscript,
 * address. The address is a pointer though, so  
 * it's converted into an OPC_ARRSECTION or OPC_ARRAY 
 *
 * See notes above on non-contiguous sections.
 * and the description of the stack in in fei_seq_subscr.
 *
 *===============================================
 */ 
extern void
fei_nseq_subscr( TYPE result_type )
{
   WN *ex  ;
   WN *lb  ;
   WN *sb  ;
   WN *sm  ;
   WN *ar  ;
   WN *ad  ;
   WN *wt  ;
   ST *st  ;
#ifdef KEY /* Bug 10177 */
   TY_IDX ty  = 0;
#else /* KEY Bug 10177 */
   TY_IDX ty  ;
#endif /* KEY Bug 10177 */
   TY_IDX dope_ty  ;
#ifdef KEY /* Bug 10177 */
   WN_ESIZE  esize = 0;
#else /* KEY Bug 10177 */
   WN_ESIZE  esize;
#endif /* KEY Bug 10177 */
   
   TY_IDX  ta ;
   BOOL    array_val ;
   BOOL    sect ;
   BOOL    trip ;
   
   OPCODE  op   ;
   FLD_det det  ;
   WN * bounds_assertion;
   char *field_name,*array_name;
   
   sm = cwh_expr_operand(NULL) ;	/* stride mult*/
   ex = cwh_expr_operand(NULL) ;
   lb = cwh_expr_operand(NULL) ;
   sb = cwh_expr_operand(NULL) ;
   bounds_assertion = cwh_addr_do_bounds_check(sb, lb, ex);
   
   trip = cwh_addr_is_triplet(sb); 
   sb   = cwh_addr_zero_based(sb,lb);   
   sb   = F90_Wrap_ARREXP(sb);
   sect = WNOPR(sb) == OPR_ARRAYEXP;

   array_val = sect || trip ;
   op = array_val ? opc_section : opc_array ;
   
   switch(cwh_stk_get_class()) {
    case ADDR_item:
    case WN_item:
    case WN_item_whole_array:
      ta = cwh_stk_get_TY();
      ar = cwh_expr_address(f_NONE);    
      if (array_val) 
	if (cwh_addr_is_array(ar))
	  WN_set_opcode(ar, opc_section) ; 

      if (WNOPR(ar)==OPR_ARRSECTION || WNOPR(ar)==OPR_ARRAY) {
	 may_be_noncontig = (WN_element_size(ar) < 0 );
      }      
      cwh_addr_fixup_nseq(&ex,&sb,sm);
      cwh_addr_insert_bounds_check(bounds_assertion,ar);
      ar = cwh_addr_add_bound(ar,ex,sb);
      cwh_stk_push_typed(ar,WN_item,ta);
      break  ;

    case DEREF_item:
      may_be_noncontig = FALSE;
      dope_ty = cwh_stk_get_TY();
      if (dope_ty) {
         TY& t = Ty_Table[dope_ty];
         ty = FLD_type(TY_fld(t));
	 may_be_noncontig = TY_is_f90_pointer(t);
      }
      ar = cwh_expr_address(f_NONE);
      st = cwh_addr_WN_ST(ar);
      if (!dope_ty) {
	 ty = ST_type(st);
	 ty = cwh_types_dope_basic_TY(ty);
      }	 

      if (ST_sclass(st) == SCLASS_FORMAL || 
	  ST_auxst_is_non_contiguous(st) ||
	  may_be_noncontig) {
	 may_be_noncontig = TRUE;
	 esize = cwh_addr_compute_stride_fudge_factor(ty);
      }
      array_name = GET_ARRAY_NAME_MAP(ar);
      ar = cwh_addr_array(op,ar,ty);
      if (array_name) {
	 SET_ARRAY_NAME_MAP(ar,Index_To_Str(Save_Str2(ST_name(st),array_name)));
      } else {
	 SET_ARRAY_NAME_MAP(ar,ST_name(st));
      }
      if (may_be_noncontig) WN_element_size(ar) = esize;

      if (array_val) 
	if (cwh_addr_is_array(ar))
	  WN_set_opcode(ar, opc_section) ; 
      
      cwh_addr_fixup_nseq(&ex,&sb,sm);
      cwh_addr_insert_bounds_check(bounds_assertion,ar);
      ar = cwh_addr_add_bound(ar,ex,sb);
      cwh_stk_push(ar,WN_item);
      break;
  
    case ST_item:
    case ST_item_whole_array:
      may_be_noncontig = FALSE;
      st = cwh_stk_pop_ST();
      ty = ST_type(st);

      if (ST_sclass(st) == SCLASS_FORMAL || 
	  ST_auxst_is_non_contiguous(st) ||
	  TY_is_f90_pointer(Ty_Table[ty])) {

	 may_be_noncontig = TRUE;
	 esize = cwh_addr_compute_stride_fudge_factor(ty);
      }
      ad = cwh_addr_address_ST(st) ;
      ar = cwh_addr_array(op,ad,ty);
      SET_ARRAY_NAME_MAP(ar,ST_name(st));
      if (may_be_noncontig) WN_element_size(ar) = esize;

      cwh_addr_fixup_nseq(&ex,&sb,sm);
      cwh_addr_insert_bounds_check(bounds_assertion,ar);
      ar = cwh_addr_add_bound(ar,ex,sb);
      cwh_stk_push(ar,WN_item);
      break ;

    case FLD_item:
      may_be_noncontig = FALSE;
      field_name = cwh_stk_fld_name();
      det = cwh_addr_offset() ;

      if (TY_is_f90_pointer(Ty_Table[det.type])) {

	 may_be_noncontig = TRUE;
	 esize = cwh_addr_compute_stride_fudge_factor(ty);
      }

      /* Preserve TY info for  the FLD       */
      /* (OPC_ARRAY doesn't hold a type      */
      /* a type and the fundemental address  */ 
      /* TY is that of a parent object )     */

      if (cwh_stk_get_class() == ST_item || cwh_stk_get_class() == ST_item_whole_array) {
	 st = cwh_stk_pop_ST();
	 ad = cwh_addr_address_ST(st,det.off,det.type) ;
	 array_name = ST_name(st);

      } else { 

	 /* is array of array of derived type   */
	 /* or similar.                         */

	 ad = cwh_expr_address(f_NONE);
	 array_name = GET_ARRAY_NAME_MAP(ad);
	 wt = WN_CreateIntconst(opc_pint,det.off);
	 ad = cwh_expr_bincalc(OPR_ADD,ad,wt);
      }

      ar = cwh_addr_array(op,ad,det.type) ;
      if (strlen(field_name) > 0) {

	 if (array_name) {
	    array_name = Index_To_Str(Save_Str2(array_name,field_name));
	 } else {
	    array_name = Index_To_Str(Save_Str2("(unknown)",field_name));
	 }
	 free(field_name);
	 SET_ARRAY_NAME_MAP(ar,array_name);
      }

      if (may_be_noncontig) WN_element_size(ar) = esize;
      cwh_addr_fixup_nseq(&ex,&sb,sm);
      cwh_addr_insert_bounds_check(bounds_assertion,ar);
      ar = cwh_addr_add_bound(ar,ex,sb);
      cwh_stk_push_typed(ar,WN_item,det.type);
      break ;

    default:
      DevAssert((0),(" odd item in subscr"));
   }
}

/*===============================================
 *
 * fei_subscr_triplet
 *
 * A subscript triplet will have lb,ub,str on
 * the stack with the subscripted item beneath.
 * The expressions are as written in the source,
 * so make a zero-based OPC_TRIPLET, & push it.
 *
 *===============================================
 */ 
extern void 
fei_subscr_triplet(TYPE result_type )
{
  WN *lb  ;
  WN *ub  ;
  WN *str ;
  WN *wt  ;

  str = cwh_expr_operand(NULL) ;
  ub  = cwh_expr_operand(NULL) ;
  lb  = cwh_expr_operand(NULL) ;

  wt  = cwh_addr_triplet(lb,ub,str);

  cwh_stk_push(wt,WN_item);
}

/*===============================================
 *
 * fei_subscr_size
 *
 * An axis size description is on the stack -
 * stride multiplier, extent, and declared lb. An
 * OPC_ARRAY/ARRSECTION doesn't need the size - it
 * uses the declared size, so this routine just
 * saves the state of the bounds_check flag
 *
 *===============================================
 */ 
extern void
fei_subscr_size( TYPE result_type, INT32 bounds_check)
{
   check_bounds_this_access = (bounds_check != 0) && (cwh_io_in_ioblock==0);
}

/*===============================================
 *
 * fei_substr
 *
 * A substring operator for character types.
 * the stack contains the size,lb and address.
 * Make the address & length into a STR_item.
 * 
 * Convert the substring details to an OPC_ARRAY
 * so the bounds of the substring are established 
 * as a 1d array of chars (bytes).
 *
 * If there is an address (WN) on the stack, eg: 
 * an OPC_ARRAY or OPC_ARRSECTION - we have an
 * character array reference, so just wrap the 
 * OPC_ARRAY of the substring around the address.
 * If not, convert the ST (scalar character varbl)
 * into an address, then wrap it. FLDs are similar.
 *
 *===============================================
 */ 

extern void
fei_substr(INT32 bounds_check)
{
  WN * asz;
  WN * sz ;
  WN * lb ;
  WN * one;
  WN * ar ;
  TY_IDX ts ;
  
#ifdef KEY /* Bug 10177 */
  W_node  ad  = { 0, 0};
#else /* KEY Bug 10177 */
  W_node  ad  ;
#endif /* KEY Bug 10177 */
  FLD_det det ;
  
  sz = cwh_expr_operand(NULL);
  lb = cwh_expr_operand(NULL);
  
  switch(cwh_stk_get_class()){
  case ST_item:
  case ST_item_whole_array:
  case WN_item:
  case WN_item_whole_array:
    ts = cwh_stk_get_TY();
    ad = cwh_addr_substr_util(0,ts);
    break;

  case DEREF_item:
    ad = cwh_addr_substr_util(0,0);
    break;

  case FLD_item:
    det = cwh_addr_offset() ;    
    ad  = cwh_addr_substr_util(det.off,det.type) ;
    break ;
     
  default:
    DevAssert((0),(" Odd string"));
  }
  
  one = WN_CreateIntconst (opc_pint,1);
  lb  = cwh_addr_zero_based(lb,one);
  asz = WN_COPY_Tree(sz);
  ar  = cwh_addr_add_bound(W_wn(ad),asz,lb);
  
  cwh_stk_push_STR(sz,ar,W_ty(ad),WN_item);      
}

/*===============================================
 *
 * fei_addr
 *
 * Compute the address of TOS & push it back. This
 * used as a flag when context isn't sufficient to
 * tell the TOS is an address, eg: computing
 * the address of an element & storing into a 
 * compiler temp.
 *
 *===============================================
 */ 
extern void 
fei_addr(TYPE basic)
{
  WN * wn ;

  wn = cwh_expr_address(f_T_SAVED);
  if (cwh_addr_is_array(wn)) {
     /* need to "hide" this so that expr operand won't deref it if other 
      *	operations are done to it. 
      */
     wn = WN_CreateComma(OPCODE_make_op(OPR_COMMA,Pointer_Mtype,MTYPE_V),
			 WN_CreateBlock(),wn);
     
  }
  cwh_stk_push(wn,ADDR_item);
}

/*===============================================
 *
 * fei_as_ref
 *
 * TOS is the address of a non-contiguous array,
 * created by fei_dv_deref. Turn it into
 * an OPC_ARRSECTION & push it back. Uses the
 * general DV routines to extract information,
 * so each setup & result is on the stack.
 * 
 * The dope bounds are in fortran order &
 * WHIRL bounds in C order
 *
 *===============================================
 */ 
extern void
fei_as_ref( TYPE result_type )
{
  WN * ub;
  WN * ad;
  WN * sz;
  ST * st;  
  TY_IDX ty;

  TYPE_ID bt;
  INT32 nd,i;

  ad = cwh_stk_pop_DEREF();
  st = WN_st(ad);
  nd = cwh_types_dope_rank(ST_type(st));
  ty = cwh_types_dope_basic_TY(ST_type(st));
  ad = cwh_addr_array(opc_section,ad,ty);
  bt = cwh_bound_int_typeid;

  for (i = 0 ; i < nd ; i++) {

    cwh_stk_push(st,ST_item);
    fei_get_dv_extent(nd-i,0);
    sz = cwh_stk_pop_WN();
    ub = cwh_expr_bincalc(OPR_SUB,WN_COPY_Tree(sz),WN_Intconst(bt,1)) ;
    ub = cwh_addr_triplet(WN_Intconst(bt,0),ub,WN_Intconst(bt,1)) ;
    ad = cwh_addr_add_bound(ad,sz,ub);
  }
  cwh_stk_push(ad,WN_item);
}

/*===============================================
 *
 * cwh_addr_array
 *
 * create an OPC_ARRAY or OPC_ARRSECTION
 * for the given address & TY. 
 *
 *===============================================
 */ 
static WN *
cwh_addr_array(OPCODE op, WN * addr, TY_IDX ty)
{
  WN * wn   ;
  TY_IDX aty  ;
  INT16 nkids,i ;

  aty = cwh_types_array_TY(ty);

  TY& t = Ty_Table[aty];
  nkids = 2 * TY_AR_ndims(t) +1 ;
  wn = WN_Create ( op, nkids );
  WN_element_size(wn) = TY_size(TY_etype(t));

  WN_kid(wn,0) = addr ;

  FOREACH_AXIS(i,nkids) {
    WN_kid(wn,i+SZ_OFF(nkids))  = NULL ;
    WN_kid(wn,i+SUB_OFF(nkids)) = NULL ;
  }
  return wn ;
}


#ifdef KEY /* Bug 5398 */
/*================================================================
 *
 * cwh_compare_index_and_bound
 *  Compare index and array bound, whose data types might not initially
 *  match, requiring type conversion. Modeled after cwh_expr_compare.
 *
 *  inputs:
 *
 *    OPERATOR op - comparison operator
 *    WN * lhs - lhs of comparison
 *    WN * rhs - rhs of comparison
 *
 *  returns: logical expression comparing the lhs and rhs
 *================================================================
 */
static WN *
cwh_compare_index_and_bound(OPERATOR op, WN *lhs, WN *rhs) {
  TYPE_ID bt = cwh_get_highest_type(rhs, lhs);
  OPCODE opc = cwh_make_typed_opcode(op, MTYPE_I4, Mtype_comparison(bt));
  lhs = cwh_convert_to_ty(lhs,bt);
  rhs = cwh_convert_to_ty(rhs,bt);
  WN *wn  = WN_CreateExp2(opc, lhs, rhs) ;
  return wn;
  }
#endif /* KEY Bug 5398 */



/*================================================================
 *
 * cwh_addr_do_bounds_check
 *
 *  Implement the bounds checking stuff
 *  
 *  inputs:
 *    WN * subscript - a subscript expression, in user space 
 *         (i.e. not lbound normalized. If it's a TRIPLET, its
 *         lower bound has not yet been normalized.
 *
 *    WN * lbound - declared lower bound.
 *    WN * extent - size of the index. 
 *
 *  returns: logical expression (or NULL) which indicates whether the bounds check passed
 *           A NULL means that no bounds check should be done. 
 *
 *================================================================
 */
static WN *
cwh_addr_do_bounds_check(WN *subscript_in, WN *lbound, WN *extent)
{
  WN *lbc,*ubc,*assertion,*subscript;
  WN *stride,*ub,*ubdecl;
  WN *arrexp;
  WN *temp;
  static OPCODE ge_op=OPCODE_UNKNOWN,lt_op=OPCODE_UNKNOWN,le_op=OPCODE_UNKNOWN;
  static TYPE log_type;
  PREG_NUM bc_preg;

  if (!DEBUG_Subscript_Check || !check_bounds_this_access) return (NULL);
  if (ge_op == OPCODE_UNKNOWN) {
     ge_op = OPCODE_make_op(OPR_GE,MTYPE_I4,cwh_bound_int_typeid);
     lt_op = OPCODE_make_op(OPR_LT,MTYPE_I4,cwh_bound_int_typeid);
     le_op = OPCODE_make_op(OPR_LE,MTYPE_I4,cwh_bound_int_typeid);
     t_TY(log_type) = cast_to_int(logical4_ty);
  }

  ubdecl = cwh_expr_bincalc(OPR_ADD,WN_COPY_Tree(lbound),WN_COPY_Tree(extent));
 
  // Wrap an ARRAYEXP if necessary
  subscript = F90_Wrap_ARREXP(subscript_in);

  if (WNOPR(subscript)==OPR_TRIPLET) {
     /* Three cases: stride constant and positive, constant and negative, non-constant */
     stride = WN_kid1(subscript);
     ub = cwh_addr_ubound_from_triplet(subscript);
     
     if (WNOPR(stride) != OPR_INTCONST) {
	/* Check that the lower bound is OK */
#ifdef KEY /* Bug 5398 */
	temp = cwh_compare_index_and_bound(OPR_GE,
	  WN_COPY_Tree(WN_kid0(subscript)),WN_COPY_Tree(lbound));
	lbc = cwh_compare_index_and_bound(OPR_LT,
	  WN_COPY_Tree(WN_kid0(subscript)),WN_COPY_Tree(ubdecl));
#else /* KEY Bug 5398 */
	temp = WN_CreateExp2(ge_op,WN_COPY_Tree(WN_kid0(subscript)),WN_COPY_Tree(lbound));
	lbc = WN_CreateExp2(lt_op,WN_COPY_Tree(WN_kid0(subscript)),WN_COPY_Tree(ubdecl));
#endif /* KEY Bug 5398 */
	lbc = WN_LAND(temp,lbc);
	
	/* Check that the upper bound is OK */
#ifdef KEY /* Bug 5398 */
	temp = cwh_compare_index_and_bound(OPR_GE,WN_COPY_Tree(ub),
	  WN_COPY_Tree(lbound));
	ubc = cwh_compare_index_and_bound(OPR_LT,WN_COPY_Tree(ub),ubdecl);
#else /* KEY Bug 5398 */
	temp = WN_CreateExp2(ge_op,WN_COPY_Tree(ub),WN_COPY_Tree(lbound));
	ubc = WN_CreateExp2(lt_op,WN_COPY_Tree(ub),ubdecl);
#endif /* KEY Bug 5398 */
	ubc = WN_LAND(temp,ubc);
     } else {
	/* Constant stride */
	if (WN_const_val(stride) > 0) {
	   /* Only need to check lb > declared lb and ub < declared ub */
#ifdef KEY /* Bug 5398 */
	   lbc = cwh_compare_index_and_bound(OPR_GE,
	     WN_COPY_Tree(WN_kid0(subscript)),WN_COPY_Tree(lbound));
	   ubc = cwh_compare_index_and_bound(OPR_LT,WN_COPY_Tree(ub),ubdecl);
#else /* KEY Bug 5398 */
	   lbc = WN_CreateExp2(ge_op,WN_COPY_Tree(WN_kid0(subscript)),WN_COPY_Tree(lbound));
	   ubc = WN_CreateExp2(lt_op,WN_COPY_Tree(ub),ubdecl);
#endif /* KEY Bug 5398 */
	} else {
	   /* check that first element is < top, last > bottom */
#ifdef KEY /* Bug 5398 */
	   lbc = cwh_compare_index_and_bound(OPR_LT,
	     WN_COPY_Tree(WN_kid0(subscript)), ubdecl);
	   ubc = cwh_compare_index_and_bound(OPR_GE,
	     WN_COPY_Tree(ub),WN_COPY_Tree(lbound));
#else /* KEY Bug 5398 */
	   lbc = WN_CreateExp2(lt_op,WN_COPY_Tree(WN_kid0(subscript)),ubdecl);
	   ubc = WN_CreateExp2(ge_op,WN_COPY_Tree(ub),WN_COPY_Tree(lbound));
#endif /* KEY Bug 5398 */
	}
     }
     assertion = WN_LAND(lbc,ubc);
     
  } else if (WNOPR(subscript)==OPR_ARRAYEXP) {
     /* Array expression, need to build up an ANY node */
     arrexp = WN_COPY_Tree(subscript);
#ifdef KEY /* Bug 5398 */
     lbc = cwh_compare_index_and_bound(OPR_GE,WN_COPY_Tree(WN_kid0(subscript)),
       WN_COPY_Tree(lbound));
     ubc = cwh_compare_index_and_bound(OPR_LT,WN_COPY_Tree(WN_kid0(arrexp)),
       ubdecl);
#else /* KEY Bug 5398 */
     lbc = WN_CreateExp2(ge_op,WN_COPY_Tree(WN_kid0(subscript)),WN_COPY_Tree(lbound));
     ubc = WN_CreateExp2(lt_op,WN_COPY_Tree(WN_kid0(arrexp)),ubdecl);
#endif /* KEY Bug 5398 */
     assertion = WN_LAND(lbc,ubc);
     WN_kid0(arrexp) = assertion;
     cwh_stk_push(arrexp,WN_item);
     fei_null_expr();
     fei_all(log_type);
     assertion = cwh_expr_operand(NULL);
     bc_preg = Create_Preg(MTYPE_I4,"bounds_check");
     cwh_block_append(WN_StidPreg(MTYPE_I4,bc_preg,assertion));
     assertion = WN_LdidPreg(MTYPE_I4,bc_preg);
  } else {
     /* Scalar expression */
#ifdef KEY /* Bug 5398 */
     lbc = cwh_compare_index_and_bound(OPR_GE,WN_COPY_Tree(subscript),
       WN_COPY_Tree(lbound));
     ubc = cwh_compare_index_and_bound(OPR_LT,WN_COPY_Tree(subscript),ubdecl);
#else /* KEY Bug 5398 */
     lbc = WN_CreateExp2(ge_op,WN_COPY_Tree(subscript),WN_COPY_Tree(lbound));
     ubc = WN_CreateExp2(lt_op,WN_COPY_Tree(subscript),ubdecl);
#endif /* KEY Bug 5398 */
     assertion = WN_LAND(lbc,ubc);
  }
  if (WNOPR(assertion) == OPR_INTCONST) {
     if (WN_const_val(assertion) != 0) {
	/* Assertion is always true, don't need to check it */
	WN_DELETE_Tree(assertion);
	return (NULL);
     }
  }
  return (assertion);
}

/*================================================================
 *
 * cwh_addr_insert_bounds_check
 *
 *  Implement the bounds checking stuff
 *  
 *  inputs:
 *      WN * assertion - a logical expression which is TRUE if everyting is OK with the access
 *      WN * ar - an ARRAY node (only partially filled in) so that the name and axis number can be
 *                determined. The array_name_map is used to figure out the array name. 
 *
 *================================================================
 */
static void 
cwh_addr_insert_bounds_check(WN *assertion, WN *ar)
{
   WN *args[4];
   BOOL byval[4];
   WN *save_block,*fail_block;
   char *proc_name;
   char *array_name;
   INT axis,ndim;
   INT64 lineno;
  
   if (assertion == NULL) return;
   
   /* Figure out the axis */
   ndim = WN_num_dim(ar);
   for (axis = 0; axis < ndim; axis++) {
      if (WN_array_dim(ar,axis) == NULL) break;
   }
   axis = ndim - axis; /* convert to user axis number */

   /* Figure out the name */


   /* make up the IF */
   fail_block = WN_CreateBlock();
   assertion = WN_CreateIf(assertion, WN_CreateBlock(), fail_block);
   cwh_block_append(assertion);
   save_block = cwh_block_exchange_current(fail_block);
   
   /* build up the intrinsic call */
   lineno = USRCPOS_linenum(current_srcpos);
   args[1] = WN_Intconst(MTYPE_I4,lineno);
   byval[1] = TRUE;
   //proc_name = ST_name(Procedure_ST);
   proc_name = cwh_dst_filename_from_filenum(SRCPOS_filenum(current_srcpos));
   args[0] = WN_LdaString(proc_name, 0, strlen(proc_name));
   byval[0] = TRUE;
   
   array_name = GET_ARRAY_NAME_MAP(ar);
   if (array_name) {
      args[2] = WN_LdaString(array_name, 0, strlen(array_name)+1);
   } else {
      args[2] = WN_Intconst(Pointer_Mtype,0);
   }
   byval[2] = TRUE;
   args[3] = WN_Intconst(MTYPE_I4,axis);
   byval[3] = TRUE;
   cwh_intrin_call(INTRN_F90BOUNDS_CHECK, 4, args, NULL, byval, MTYPE_V);
   cwh_block_set_current(save_block);
}

/*===============================================
 *
 * cwh_addr_add_bound
 *
 * Add the size and subscript to the given
 * OPC_ARRAY or OPC_ARRSECTION. When the
 * node was created, the kids were nulled.
 * Look for the first free kid. (Axes 
 * are processed high to 1, but WH requires C order)
 *
 * Return the modified WN.
 *
 *===============================================
 */ 
static WN *
cwh_addr_add_bound(WN * ar, WN * sz, WN *subscript)
{
  INT16 nkids,i ;

  nkids = WN_kid_count(ar) ;

  FOREACH_AXIS(i,nkids) {
    if (WN_kid(ar,i) == NULL) {
      WN_kid(ar,i+SZ_OFF(nkids)) = sz;
      WN_kid(ar,i+SUB_OFF(nkids)) = subscript ;
      break ;
    }
  }


  return ar ;
}

/*===============================================
 *
 * cwh_addr_use_mstid_mldid
 *
 * Given an ST, and the WHIRL flag, see if the
 * ST is a candidate for mldid/mstid. There is a
 * test, because the F90 lowerer does not do
 * dependency checking on mldid/mstid. It does
 * on the more general mload/mstore.
 *
 *===============================================
 */ 

static inline bool
cwh_addr_use_mstid_mldid(ST *st)
{
  BOOL res =  WHIRL_Mldid_Mstid_On   && 
             !ST_is_equivalenced(st) &&
             !ST_is_f90_target(st);

  return res ;
}

/*===============================================
 *
 * cwh_addr_ldid
 *
 * Given an ST, offset and ty, make an LDID. If
 * the ST is a dummy the ty will be KIND_POINTER
 * and the address loaded.
 *
 *===============================================
 */ 
extern  WN *
cwh_addr_ldid(ST *st, OFFSET_64 off, TY_IDX ty)
{

  WN * wn ;
  TYPE_ID bt ;

  if (cwh_addr_use_mstid_mldid(st)) {

    if (TY_kind(ty) != KIND_SCALAR && TY_kind(ty) != KIND_STRUCT) 
      bt = Pointer_Mtype;
    else
      bt = TY_mtype(ty);

  } else {

    if (TY_kind(ty) != KIND_SCALAR)
      bt = Pointer_Mtype;
    else
      bt = TY_mtype(ty);
  }

  if (BIG_OFFSET(off)) {
    wn = cwh_addr_lda(st,off,ty);
    wn = cwh_addr_iload(wn,0,ty);

  } else {

    wn = cwh_addr_mk_ldid(st,off,bt,ty);
  }
  cwh_addr_access_flags(st,ACCESSED_LOAD);
  return (wn) ;
}

/*===============================================
 *
 * cwh_addr_mk_ldid
 *
 * Given an ST,offset,type id and ty, make an LDID. 
 * The opcode of the LDID is derived from rt, and 
 * the WN_ty from the ty. This is just a lookup
 * of the opcode, so suitable for odd return 
 * temps and so forth. Does not deal with large 
 * offsets.
 *
 *===============================================
 */ 
extern  WN *
cwh_addr_mk_ldid(ST *st, OFFSET_64 off, TYPE_ID bt, TY_IDX ty)
{

  WN * wn ;
  OPCODE opc ;

  opc = Ldid_Opcode [bt];

  if (cwh_addr_use_mstid_mldid(st)) {

    if (TY_size(ty) != MTYPE_byte_size (bt) &&
        TY_kind(ty) != KIND_STRUCT)
        Set_TY_IDX_index (ty, TY_IDX_index (MTYPE_To_TY (bt)));

  }  else {

    if (TY_size(ty) != MTYPE_byte_size (bt))
        Set_TY_IDX_index (ty, TY_IDX_index (MTYPE_To_TY (bt)));
  }

  wn  = WN_CreateLdid (opc,off,st,ty) ;

  return wn ;
}

/*===============================================
 *
 * cwh_addr_mload
 *
 * Given a WN which is an address, an offset and 
 * scalar ty, make an OPC_MLOAD.
 *
 *===============================================
 */ 

extern  WN *
cwh_addr_mload(WN *wt, OFFSET_64 off, TY_IDX ty, WN * sz)
{
  WN * wn ;
  TY_IDX tp ;

  if (cwh_addr_f90_pointer_reference(wt)) {
     tp = cwh_types_mk_f90_pointer_ty(ty);
  } else {
     tp = cwh_types_make_pointer_type(ty, FALSE);
  }     

  if (BIG_OFFSET(off)) {
     wt  = cwh_expr_bincalc(OPR_ADD,wt,WN_Intconst(Pointer_Mtype,off));
     off = 0;
  }

  if (! sz)
    sz = WN_CreateIntconst (opc_pint, TY_size(ty)) ;

  wn = WN_CreateMload(off,tp,wt,sz);

  return (wn) ;
}

/*===============================================
 *
 * cwh_addr_iload
 *
 * Given an WN which is an address, an offset 
 * and scalar ty, make an OPC_ILOAD. 
 *
 *===============================================
 */ 
static  WN *
cwh_addr_iload(WN *wt, OFFSET_64 off, TY_IDX ty)
{
  WN * wn ;
  TY_IDX tp ;
  OPCODE op;

  if (cwh_addr_f90_pointer_reference(wt)) {
     tp = cwh_types_mk_f90_pointer_ty(ty);
  } else {
     tp = cwh_types_make_pointer_type(ty, FALSE);
  }     

  if (BIG_OFFSET(off)) {
     wt = cwh_expr_bincalc(OPR_ADD,wt,WN_Intconst(Pointer_Mtype,off));
     off = 0;
  }
  op = Load_Opcode [TY_mtype(ty)];
  wn = WN_CreateIload (op,off,ty,tp,wt);

  return (wn) ;
}

/*===================================================
 *
 * cwh_addr_WN_ST
 *
 * Given a WN, find the ST of what it addresses. Not
 * general - used in figuring out STs when building
 * addresses. 
 *
 ====================================================
*/

extern ST *
cwh_addr_WN_ST(WN * wn)
{
  ST * st = NULL ;
  WN *kid;
  INT i;

  switch (WNOPR(wn)) {
  case OPR_ARRAY:
  case OPR_ARRSECTION:
  case OPR_ARRAYEXP:
  case OPR_ILOAD:
    st = cwh_addr_WN_ST(WN_kid0(wn));
    break ;

  case OPR_LDA:
  case OPR_LDID:
    st = WN_st(wn) ;
    break;

  case OPR_INTCONST:
    /* return a NULL st */
    break;
    
  /* Special case for ADD */
  case OPR_ADD:
    for (i=0; i <= 1; i++) {
       kid = WN_kid(wn,i);
       switch (WNOPR(kid)) {
	case OPR_ARRAY:
	case OPR_ARRSECTION:
	case OPR_ARRAYEXP:
	case OPR_LDA:
	case OPR_LDID:
	case OPR_ILOAD:
	  st = cwh_addr_WN_ST(kid);
	  return (st);
       }
    }
    /* Fall through */

  default:
    DevAssert((OPCODE_is_expression(WN_opcode(wn))),(" Unexpected WN"));
    break;
  }

  return (st) ;
}

/*===============================================
 *
 * cwh_addr_load_WN
 *
 * Given a WN which is an address, make an OPC_ILOAD or
 * OPC_MLOAD. If TY argument dty is null, the type
 * will be inferred from the WN. Dty tends to be 
 * used for derived type components.
 * 
 *===============================================
 */ 
extern WN *
cwh_addr_load_WN(WN * awn, OFFSET_64 off, TY_IDX dty)
{
  TY_IDX ty ;
  TY_IDX ts ;
  WN * wn = NULL;

  if (dty == 0)
    ty = cwh_types_WN_TY(awn,FALSE);
  else
    ty = dty ;

  switch(TY_kind(ty)) {

  case KIND_POINTER:
  case KIND_SCALAR  :
    wn = cwh_addr_iload(awn,off,ty);
    break ;

  case KIND_ARRAY :
    ts = cwh_types_scalar_TY(ty);
    if (TY_kind(ts) == KIND_STRUCT)
      wn = cwh_addr_mload(awn,off,ts, NULL);
    else
      wn = cwh_addr_iload(awn,off,ts);
    break;

  case KIND_STRUCT :
    ts = cwh_types_scalar_TY(ty);
    wn = cwh_addr_mload(awn,off,ts, NULL);
    break ;

  default:
    DevAssert((0),("unimplemented WN load"));
    break;
  }

  return (wn);
}

/*===============================================
 *
 * cwh_addr_load_ST
 *
 * Given a ST make an LDID, ILOAD or MLOAD. The ST
 * may be a basic or a derived type. The TY argument
 * may be NULL unless addressing a component of
 * a derived type, although it will be used if present.
 *
 *===============================================
 */ 
extern WN *
cwh_addr_load_ST(ST * st, OFFSET_64 off, TY_IDX dty)
{

  WN * wn = NULL;
  WN * wa;
  TY_IDX ts;
  TY_IDX ty;

  INT fg ;

  ty = ST_type(st);
  fg = ACCESSED_LOAD;

  switch (ST_sclass(st)) {
  case SCLASS_FORMAL:
    if (dty)
      ts = dty;
    else if (TY_kind(ty) == KIND_POINTER)
      ts = TY_pointed(ty);
    else
      ts = ty;
    
    if (BY_VALUE(ty)) {
       wn = cwh_addr_ldid(st,off,ts);
    } else {
       wa = cwh_addr_address_ST(st);
       wn = cwh_addr_load_WN(wa,off,ts);
    }
    break ;

  case SCLASS_AUTO:
  case SCLASS_FSTATIC:
  case SCLASS_PSTATIC:
  case SCLASS_REG:
  case SCLASS_COMMON:
  case SCLASS_DGLOBAL:
  case SCLASS_FORMAL_REF:

    switch(TY_kind(ty)) {
      
    case KIND_POINTER :	    
      fg |= ACCESSED_STORE | ACCESSED_ILOAD | ACCESSED_ISTORE;
      
    case KIND_SCALAR :
      
      ts = (dty ? dty : ty);
      if (ST_class(st)==CLASS_VAR && ST_auxst_is_auto_or_cpointer(st)) {
	/* need to load it through its base */
	wa = cwh_addr_address_ST(st);
	wn = cwh_addr_load_WN(wa,0,ts);
	fg |= ACCESSED_ILOAD;
      } else {
	wn = cwh_addr_ldid(st,off,ts);
      }
      break ;
      
    case KIND_ARRAY :	
      wa = cwh_addr_address_ST(st,off);
      wn = cwh_addr_load_WN(wa,0,0);
      break ;
      
    case KIND_STRUCT :	
      ts = (dty ? dty : ty);
      
      if (cwh_addr_use_mstid_mldid(st)) {
	
        if (TY_kind(ts) == KIND_POINTER){  /* dope */
          fg |= ACCESSED_STORE | ACCESSED_ILOAD | ACCESSED_ISTORE;
        }
        wn = cwh_addr_ldid(st,off,ts);
      }
      else {
        if (TY_kind(ts) == KIND_SCALAR) 
	  wn = cwh_addr_ldid(st,off,ts);
      
        else if (TY_kind(ts) == KIND_POINTER){  /* dope */
	  fg |= ACCESSED_STORE | ACCESSED_ILOAD | ACCESSED_ISTORE;
	  wn  = cwh_addr_ldid(st,off,ts);
	
        } else {
	  wa = cwh_addr_address_ST(st,off,ts);
	  wn = cwh_addr_load_WN(wa,0,ts);
        }
      }
      break ;
      
    default:
      DevAssert((0),("unimplemented ST load"));
      break;
    }
    break ;

  default:
    DevAssert((0),("Odd ST load"));
    break;
  }     

  cwh_addr_access_flags(st,fg);
  return (wn);
}

/*===================================================
 *
 * cwh_addr_stid
 *
 * Create an OPC_STID, given an ST, offset, ty 
 * and a rhs WN. If a derived type component the 
 * offset and TY need not be those of the ST.
 *
 ====================================================
 */
extern WN *
cwh_addr_stid(ST *st, OFFSET_64 off, TY_IDX ty , WN * rhs) 
{
  WN * wn ;
  WN * wt ;
  TY_IDX tl ;

  TYPE    t ;
  TYPE_ID bt;
  OPCODE  op;

#ifdef KEY // bug 7612
  if (WN_operator(rhs) == OPR_LDA) {
    ST *lda_st = WN_st(rhs);
    Set_ST_addr_saved(lda_st);
  }
#endif

  rhs = cwh_convert_to_ty(rhs, TY_mtype(ty)); 

  if (BIG_OFFSET(off)) {
    wn = cwh_addr_lda(st,off,ty);
    wn = cwh_addr_istore(wn,0,ty,rhs);

  } else {

    tl = ty;
    bt = TY_mtype(ty) ;

# if ! (defined (linux) || defined(BUILD_OS_DARWIN))
    if (IS_ALTENTRY_TEMP(st)) {
      if (MTYPE_is_integral(bt)) {
	  tl = cwh_stab_altentry_TY(st,TRUE);
	  st = ST_base(st);
	  bt = TY_mtype(tl);

      } else if (! ST_auxst_altentry_shareTY(ST_base(st))) {
	
	if ((bt == MTYPE_C4) && (ST_ofst(st) != 0)) {

	  op  = Stid_Opcode [bt];
	  wn  = WN_CreateStid (op,off,st,ty,WN_COPY_Tree(rhs));
	  cwh_block_append(wn);

	  bt  = MTYPE_F4;
	  tl  = Be_Type_Tbl(bt);
	  wt  = cwh_convert_to_ty(WN_COPY_Tree(rhs),bt); 
	  op  = Stid_Opcode [bt];
	  wn  = WN_CreateStid (op,4,ST_base(st),tl,wt);
	  cwh_block_append(wn);

	  t_TY((t)) = cast_to_uint(tl);
	  cwh_stk_push(rhs,WN_item);
	  fei_imag(t);
	  rhs = cwh_stk_pop_WN();
	  off = 12;
	  st  = ST_base(st);

	} 
      }
    }
# endif

     op  = Stid_Opcode [bt];
     wn  = WN_CreateStid (op,off,st,tl,rhs);
  }

  cwh_addr_access_flags(st,ACCESSED_STORE);
  return (wn);
}

/*===================================================
 *
 * cwh_addr_istore
 *
 * Create an OPC_ISTORE, given an address, offset,
 * scalar ty and a rhs.
 *
 ====================================================
 */
extern WN *
cwh_addr_istore(WN * lhs, OFFSET_64 off, TY_IDX ty, WN * rhs) 
{
  WN * wn ;
  TY_IDX tp ;
  OPCODE op ;

  if (cwh_addr_f90_pointer_reference(lhs)) {
     tp = cwh_types_mk_f90_pointer_ty(ty);
  } else {
     tp = cwh_types_make_pointer_type(ty, FALSE);
  }     

  if (BIG_OFFSET(off)) {
     lhs = cwh_expr_bincalc(OPR_ADD,lhs,WN_Intconst(Pointer_Mtype,off));
     off = 0;
  }
  rhs = cwh_convert_to_ty(rhs, TY_mtype(ty)); 
  op  = Store_Opcode [TY_mtype(ty)];
  wn  = WN_CreateIstore(op,off,tp,rhs,lhs);

  return (wn);
}

/*===================================================
 *
 * cwh_addr_mstore
 *
 * Create an OPC_MSTORE, given address,offset,
 * ty and rhs WN. TY is type of store, eg: of
 * derived type conponent.
 * 
 ====================================================
 */
extern WN *
cwh_addr_mstore(WN * ad, OFFSET_64 off, TY_IDX ty, WN * rhs) 
{
  TY_IDX tp ;
  WN * wn ;  
  WN * sz ;

  if (cwh_addr_f90_pointer_reference(ad)) {
     tp = cwh_types_mk_f90_pointer_ty(ty);
  } else {
     tp = cwh_types_make_pointer_type(ty, FALSE);
  }     

  if (BIG_OFFSET(off)) {
     ad = cwh_expr_bincalc(OPR_ADD,ad,WN_Intconst(Pointer_Mtype,off));
     off = 0;
  }
  sz  = WN_CreateIntconst (opc_pint, TY_size(ty)) ;
  wn  = WN_CreateMstore (off,tp,rhs,ad,sz);

  return (wn);
}

/*===================================================
 *
 * cwh_addr_store_ST
 *
 * Create the appropriate store, given an ST. The
 * argument TY will be NULL, unless it's a store of
 * component of a derived type.
 *
 ====================================================
 */
extern void
cwh_addr_store_ST(ST * st, OFFSET_64 off, TY_IDX dty,  WN * rhs) 
{
  WN * wn;  
  WN * wa;
  TY_IDX ts;
  TY_IDX ty;
  INT fg ;

  ty = ST_type(st);
  fg = ACCESSED_STORE;

  switch (ST_sclass(st)) {

  case SCLASS_FORMAL:
    if (dty)
      ts = dty;
    else if (TY_kind(ty) == KIND_POINTER)
      ts = TY_pointed(ty);
    else
      ts = ty;
    
    if (BY_VALUE(ty)) {
      wn = cwh_addr_stid(st,0,ts,rhs);
      cwh_block_append(wn) ;

    } else {
      wa = cwh_addr_address_ST(st);
      cwh_addr_store_WN(wa,off,ts,rhs);
    }
    break ;

  case SCLASS_AUTO:
  case SCLASS_PSTATIC:
  case SCLASS_FSTATIC:
  case SCLASS_REG:
  case SCLASS_COMMON:
  case SCLASS_DGLOBAL:
  case SCLASS_FORMAL_REF:

    ts = (dty ? dty : ty);
    switch(TY_kind(ty)) {
      
    case KIND_POINTER:
      fg |= ACCESSED_LOAD | ACCESSED_ILOAD | ACCESSED_ISTORE;
      
    case KIND_SCALAR :
      if (ST_class(st)==CLASS_VAR && ST_auxst_is_auto_or_cpointer(st)) {
   	wa = cwh_addr_address_ST(st);
   	cwh_addr_store_WN(wa,off,0,rhs);
   	fg |= ACCESSED_ISTORE;
	
      } else {
   	wn = cwh_addr_stid(st,off,ts,rhs);
   	cwh_block_append(wn) ;
   	
   	/* if CQ function result & shared entry temp */
   	/* store via the result address too          */
   	
# if ! (defined (linux) || defined(BUILD_OS_DARWIN))
   	if (IS_ALTENTRY_TEMP(st)) {
   	  if (TY_mtype(ts) == MTYPE_CQ){
   	    if(!ST_auxst_altentry_shareTY(ST_base(st))) {
   	      wn = cwh_addr_load_ST(st,0,NULL);
   	      cwh_addr_store_ST(Altaddress_ST,0,NULL,wn);
   	    }
   	  }
   	}
# endif

	/* if in preamble, may be storing bound, or character length  */
        /* set the COPYIN flag. This is just for temps created by     */
        /* by whirlconvert, those created by the FE will be in the    */
	/* preamble block, via fei_array_dimen                        */

	if (still_in_preamble)
	  cwh_types_copyin_pragma(st);
      }
      break ;
      
    case KIND_ARRAY:
      wa = cwh_addr_address_ST(st,off);
      cwh_addr_store_WN(wa,0,0,rhs);
      break ;
      
    case KIND_STRUCT:	
	if ( cwh_addr_use_mstid_mldid(st)) {

	  if(TY_kind(ts) == KIND_POINTER){  /* dope */
	    fg |= ACCESSED_LOAD | ACCESSED_ILOAD | ACCESSED_ISTORE;
	  }
	  wn  = cwh_addr_stid(st,off,ts,rhs);
	  cwh_block_append(wn) ;

      } else {

        if (TY_kind(ts) == KIND_SCALAR) {
   	  wn = cwh_addr_stid(st,off,ts,rhs);
   	  cwh_block_append(wn) ;
	
        } else if(TY_kind(ts) == KIND_POINTER){  /* dope */
   	  fg |= ACCESSED_LOAD | ACCESSED_ILOAD | ACCESSED_ISTORE;
   	  wn  = cwh_addr_stid(st,off,ts,rhs);
   	  cwh_block_append(wn) ;
	
        } else {
   	  wa = cwh_addr_address_ST(st,off);
   	  cwh_addr_store_WN(wa,0,ts,rhs);
        }
      }
      break ;
      
    default:
      DevAssert((0),("Odd ST store"));
      break;
    }
    break ;
    
  default:
    DevAssert((0),("Odd ST store"));
    break;
  }
  cwh_addr_access_flags(st,fg);
}

/*===================================================
 *
 * cwh_addr_store_WN
 *
 * Create an OPC_ISTORE, or OPC_MSTORE given an 
 * address and WN. Use the TY of the address, unless
 * the the TY argument is not NULL, when it's 
 * probably a derived type component. 
 *
 * Add conversions to RHS if required.
 *
 ====================================================
 */
extern void
cwh_addr_store_WN(WN * lhs, OFFSET_64 off, TY_IDX dty, WN * rhs) 
{
#ifdef KEY /* Bug 10177 */
  WN * wn = 0;
#else /* KEY Bug 10177 */
  WN * wn ;
#endif /* KEY Bug 10177 */
  TY_IDX ts ;
  TY_IDX ty ;

  if (dty) 
    ty = dty ;
  else
    ty = cwh_types_WN_TY(lhs,FALSE);

  switch(TY_kind(ty)) {

  case KIND_SCALAR:
  case KIND_POINTER:
    wn = cwh_addr_istore(lhs,off,ty,rhs);
    break ;

  case KIND_ARRAY:
    ts = cwh_types_scalar_TY(ty);
    if (TY_kind(ts) == KIND_STRUCT)
      wn = cwh_addr_mstore(lhs,off,ts,rhs);
    else 
      wn = cwh_addr_istore(lhs,off,ts,rhs);
    break;

  case KIND_STRUCT:
    wn = cwh_addr_mstore(lhs,off,ty,rhs);
    break ;

  default:
    DevAssert((0),("Odd WN store"));
  }

  cwh_block_append(wn) ;
} 

/*===============================================
 *
 * cwh_addr_address_ST
 *
 *
 * Given a ST make an LDA of its address, unless 
 * it's a formal, then make an LDID.
 *
 * For BASED variables, we load the BASE if its
 * not a COMMON or static base.
 *
 * Offset and ty are optional and default to
 * 0 and the TY of the ST. Otherwise the TY
 * is the type associated with an offset within 
 * a struct. the lda routine will make this into
 * a pointer.
 *
 *===============================================
 */ 
extern WN *
cwh_addr_address_ST(ST * st, OFFSET_64 off, TY_IDX ty)
{
  WN * wn ;
  INT fg ;
  TY_IDX tp;

  if (ty == 0) {
    if (ST_class(st) == CLASS_FUNC)
      ty = ST_pu_type(st);
    else
      ty = ST_type(st);
  }

  switch (ST_sclass(st)){
  case SCLASS_FORMAL:

    DevAssert((TY_kind(ty) == KIND_POINTER),("formal & non-pointer"));

    if (ST_is_value_parm(st) && !ST_auxst_is_rslt_tmp(st) &&
       !BY_VALUE(ty)) {
       wn = cwh_addr_lda(st,off,ty);
       return wn;
    }

    wn = cwh_addr_ldid(st,0,ty);
    if (off != 0)
      wn = cwh_expr_bincalc(OPR_ADD,wn,WN_Intconst(Pointer_Mtype,off));

    fg = ACCESSED_LOAD|ACCESSED_ILOAD|ACCESSED_ISTORE ;
    cwh_addr_access_flags(st,fg);
    break;

  default:
    if (Has_Base_Block(st) && ST_auxst_is_auto_or_cpointer(st)) {

       tp = cwh_types_make_pointer_type(ty,FALSE);
       wn = cwh_addr_ldid(ST_base(st),0,tp);
       if (off != 0)
         wn = cwh_expr_bincalc(OPR_ADD,wn,WN_Intconst(Pointer_Mtype,off));
    } else {
       wn = cwh_addr_lda(st,off,ty) ;
    }
    break;
  }

  return (wn);
}

/*===================================================
 *
 * cwh_addr_lda
 *  
 * Make an LDA for an ST. The TY is that of the object
 * - a pointer TY will be made here.
 *
 ====================================================
*/
static WN *
cwh_addr_lda(ST * st, OFFSET_64 off, TY_IDX ty)
{
  TY_IDX tp ;
  WN * wn ;
  INT  fg ;

  tp = cwh_types_make_pointer_type(ty, FALSE);

//  cwh_expr_set_flags(st, f_USED_LOCALLY);

  if (BIG_OFFSET(off)) {
     wn = WN_CreateLda (opc_lda,0,tp,st);
     wn = cwh_expr_bincalc(OPR_ADD,wn,WN_Intconst(Pointer_Mtype,off));

  } else {
     wn = WN_CreateLda (opc_lda,off,tp,st);
  }

  fg = ACCESSED_LOAD|ACCESSED_ILOAD ;
  cwh_addr_access_flags(st,fg);

  return (wn);
}

/*===============================================
 *
 * cwh_addr_triplet
 *
 * Make an OPC_TRIPLET - we get an upper bound,
 * but need an extent.
 *
 *===============================================
 */ 
static WN *
cwh_addr_triplet(WN *lb,WN *ub,WN *str)
{
  WN * wn ;

  wn = WN_Create (opc_triplet, 3) ;
  WN_kid0(wn) = lb;
  WN_kid2(wn) = cwh_addr_extent(lb,ub,str);
  WN_kid1(wn) = str;

  return (wn); 
}

/*===============================================
 *
 * cwh_addr_zero_based
 *
 * Make an WN subscript zero based. A triplet
 * was created as specified in the source text,
 * so make the lower bound zero based.
 *
 *===============================================
 */ 
static WN *
cwh_addr_zero_based(WN *sub, WN * lb)
{

  if (cwh_addr_is_triplet(sub)) 
    WN_kid0(sub) = cwh_expr_bincalc(OPR_SUB,WN_kid0(sub),lb);
  else
    sub = cwh_expr_bincalc(OPR_SUB,sub,lb);

  return (sub);
}

/*===============================================
 *
 * cwh_addr_extent
 *
 * Make an extent from a ub, lb, str.
 * all nodes are copied, 
 *  
 *===============================================
 */ 
extern WN *
cwh_addr_extent(WN * lb, WN * ub, WN * str)
{
  WN * wt  ;
  WN * wub ;
  WN * wlb ;
  WN * ws1 ;
  WN * ws2 ;
  
  ws1 = WN_COPY_Tree(str) ;
  ws2 = WN_COPY_Tree(str);
  wlb = WN_COPY_Tree(lb) ;
  wub = WN_COPY_Tree(ub);
  
  wt = cwh_expr_bincalc(OPR_SUB,wub,wlb);
  wt = cwh_expr_bincalc(OPR_ADD,wt,ws1);
  wt = cwh_expr_bincalc(OPR_DIV,wt,ws2);
  
  return (wt);
}

/*===============================================
 *
 * cwh_addr_ubound_from_triplet
 *
 * Make get a ubound from a triplet
 *  
 *===============================================
 */ 
extern WN *
cwh_addr_ubound_from_triplet(WN * triplet)
{
   WN *lb;
   WN *st;
   WN *ex;
   WN *ub;

   lb = WN_COPY_Tree(WN_kid0(triplet));
   st = WN_COPY_Tree(WN_kid1(triplet));
   ex = WN_COPY_Tree(WN_kid2(triplet));
   
   /* UB = LB + ST*(EX-1) */
   ex = cwh_expr_bincalc(OPR_SUB,ex,WN_Intconst(cwh_bound_int_typeid,1));
   
   ub = cwh_expr_bincalc(OPR_MPY,ex,st);
   ub = cwh_expr_bincalc(OPR_ADD,ub,lb);

   return (ub);
}



/*===============================================
 *
 * cwh_addr_adjust_array
 *
 * The element size of this OPC_ARRAY or
 * OPC_ARRSECTION was unknown. Make the element
 * size 1, the address 0, and add it to a pointer.
 * Used for characters whose len type parameter is
 * unknown.
 *  
 *===============================================
 */ 
static WN *
cwh_addr_adjust_array(WN *wn, TY_IDX ty)
{

  WN * sz ;
  WN * extent;
  TY_IDX tl ;
  INT i,ndim;

  ndim = WN_num_dim(wn);
  /* use the new spiffy non-contiguous array addressing method */
  WN_element_size(wn) = -1;

  if (TY_kind(TY_AR_etype(ty)) == KIND_ARRAY) {

    tl = TY_AR_etype(ty);
    
    sz = cwh_types_bound_WN(tl,0,UPPER);
    for (i=ndim-1; i >= 0; i--) {
      extent = WN_array_dim(wn,i);
      WN_array_dim(wn,i) = sz;
      sz = cwh_expr_bincalc(OPR_MPY,extent,WN_COPY_Tree(sz));
    }
    WN_DELETE_Tree(sz);
  }
  
  return(wn);
}
/*===============================================
 *
 * cwh_addr_offset
 *
 * TOS is a FLD_item. Look below and pop any
 * FLD_items, accumulating the offset and
 * returning the type of the innermost field (TOS).
 *  
 *===============================================
 */ 
extern FLD_det
cwh_addr_offset(void)
{
  FLD_det det ;
  FLD_HANDLE fld (cwh_stk_pop_FLD());

  det.off  = FLD_ofst(fld);
  det.type = FLD_type(fld);

  while (cwh_stk_get_class() == FLD_item) 
    det.off += FLD_ofst(FLD_HANDLE (cwh_stk_pop_FLD()));

  return(det);
}

/*===============================================
 *
 * cwh_addr_is_*
 *
 * Is this WN an OPC_*
 *
 *===============================================
 */ 
extern BOOL
cwh_addr_is_array(WN * wn)
{
  return(WN_opcode(wn) == opc_array);
}
extern BOOL
cwh_addr_is_section(WN * wn)
{
  return(WN_opcode(wn) == opc_section);
}
static  BOOL
cwh_addr_is_triplet(WN * wn)
{
  return(WN_opcode(wn) == opc_triplet);
}

/*===============================================
 *
 * cwh_addr_find_section
 *
 * Sometimes an OPC_ARRSECTION has a load 
 * on top. May be a unary operator too. See if 
 * there's a section here. If not, return NULL.
 * 
 * To find the section - use p_RETURN_SECTION.
 * To find the section's parent - use p_RETURN_PARENT.
 * (if a section exists, but doesn't have a parent
 * the section itself is returned).
 *
 *===============================================
 */ 
extern WN *
cwh_addr_find_section(WN * awn , enum p_flag flag)
{
   WN * wn = NULL ;
  
   if (awn == NULL)
     return (wn);

   switch (WNOPR(awn)){
    case OPR_ARRSECTION:
      wn = awn ;
      break;

    case OPR_ARRAYEXP:
      wn = cwh_addr_find_section(WN_kid0(awn),flag);
      break;

    case OPR_ARRAY:
    case OPR_ILOAD:
    case OPR_MLOAD:
      wn = cwh_addr_find_section(WN_kid0(awn),flag);
      if (wn == WN_kid0(awn))
	if (flag == p_RETURN_PARENT)
	  wn = awn;

      break;

    case OPR_ADD:
    case OPR_SUB:
      wn = cwh_addr_find_section(WN_kid0(awn),flag);

      if (wn == WN_kid0(awn)) 
	if (flag == p_RETURN_PARENT)
	  wn = awn;

      if (wn == NULL) {
	 wn = cwh_addr_find_section(WN_kid1(awn),flag);
	 if (wn == WN_kid1(awn)) {
	    if (flag == p_RETURN_PARENT)
	      wn = awn;
	 }
      }
      break;

    default:
      wn = NULL;
      break;
   }
   return(wn) ;
}

/*===============================================
 *
 * cwh_addr_find_address
 *
 * Find the load of the address under the WN,
 * and return it.
 * 
 *===============================================
 */ 
extern WN *
cwh_addr_find_address(WN * wn)
{

  switch (WNOPR(wn)){
  case OPR_ILOAD:
  case OPR_MLOAD:
  case OPR_LDA:
    break;

  case OPR_ARRAY:
  case OPR_ARRSECTION:
  case OPR_ARRAYEXP:
    wn = cwh_addr_find_address(WN_kid0(wn));
    break ;

  case OPR_LDID:
    break ;

  default:
    if (OPCODE_is_expression(WN_opcode(wn))) 
      wn = cwh_addr_find_address(WN_kid0(wn));

  }
  return(wn) ;
}

/*===============================================
 *
 * cwh_addr_substr_util
 *
 * Utility routine for fei_substr. If the
 * character varbl is a component of a derived
 * then then offset and dtype are passed in & used.
 * Otherwise the offset passed is 0 & the TY may have
 * to be found. Pop the address TOS and convert
 * it into an OPC_ARRAY of chars. Return the 
 * OPC_ARRAY to get the substring bounds filled in.
 * 
 * For a character array whose length type parameter
 * is a variable, TY_size == 0 in the OPC_ARRAY (of
 * the array, not substring). Make the element size
 * 1 & offset the address by the len=size * subscript.
 *
 * substring TY is a KIND_ARRAY of chars, so array of
 * substrings is ARRAY of ARRAY of chars..
 *
 *===============================================
 */ 
static W_node 
cwh_addr_substr_util(OFFSET_64 off, TY_IDX dty )
{
  TY_IDX ty ;
  TY_IDX te ;
  ST * st ;
  WN * ad ;
  W_node r;
  
  ty = dty ;

  if (cwh_stk_get_class() == ST_item || cwh_stk_get_class() == ST_item_whole_array) {

    st = cwh_stk_pop_ST();
    if (ty == 0)
      ty = ST_type(st);
    ad = cwh_addr_address_ST(st,off,ty);

    ty = cwh_types_array_TY(ty);

  } else {

    ad = cwh_expr_address(f_NONE);

    if (ty == 0) {
      ty = cwh_types_WN_TY(ad,TRUE);
      ty = cwh_types_array_TY(ty);
    }

    if (WNOPR(ad) == OPR_ARRSECTION || WNOPR(ad) == OPR_ARRAY)
      if (WN_element_size(ad) == 0) 
	ad = cwh_addr_adjust_array(ad,ty);

    ad = cwh_expr_bincalc(OPR_ADD,ad,WN_Intconst(Pointer_Mtype,off));
  }

  te = ty ;
  if (TY_kind(TY_AR_etype(ty)) == KIND_ARRAY)
    te = TY_AR_etype(ty);

  W_wn(r) = cwh_addr_array(opc_array,ad,te);
  W_ty(r) = ty;

  return(r);
}

/*===============================================
 *
 * cwh_addr_temp_section
 *
 * Given an address & a TY which describes an
 * array, make an OPC_ARRSECTION for the full
 * array.
 *
 *===============================================
 */ 

extern WN *
cwh_addr_temp_section(WN * ad, TY_IDX ty)
{
  WN * ar;
  WN * lb;
  WN * ub;
  WN * sz;
  WN * szmult;
#ifdef KEY /* Bug 10177 */
  TY_IDX aty = 0;
#else /* KEY Bug 10177 */
  TY_IDX aty;
#endif /* KEY Bug 10177 */
  BOOL noncontig;
  INT32 ndims;
  

  INT16 i ;

  ar = cwh_addr_array(opc_section,ad,ty);
  if (WN_element_size(ar) <= 0) {
     aty = TY_AR_etype(cwh_types_array_TY(ty));
     noncontig = TRUE;
  } else {
     noncontig = FALSE;
  }

  ndims = TY_AR_ndims(Ty_Table[ty]);

  if (noncontig) {
     WN_element_size(ar) = -1;
     sz = cwh_types_bound_WN(aty,0,UPPER);
     for (i = ndims-1 ; i >=  0 ; i--) {
	lb = cwh_types_bound_WN(ty,i,LOW);
	ub = cwh_types_bound_WN(ty,i,UPPER);
	szmult = cwh_expr_bincalc(OPR_ADD,
			      cwh_addr_zero_based(WN_COPY_Tree(ub),WN_COPY_Tree(lb)),
			      WN_Intconst(cwh_bound_int_typeid,1)) ;
	
	lb = cwh_addr_triplet(lb,ub,WN_Intconst(cwh_bound_int_typeid,1)) ;
	ar = cwh_addr_add_bound(ar,WN_COPY_Tree(sz),lb);
	sz = cwh_expr_bincalc(OPR_MPY,sz,szmult);
     }
     WN_DELETE_Tree(sz); /* Clean up */
  } else {
     for (i = ndims-1 ; i >=  0 ; i--) {
	lb = cwh_types_bound_WN(ty,i,LOW);
	ub = cwh_types_bound_WN(ty,i,UPPER);
	sz = cwh_expr_bincalc(OPR_ADD,
			      cwh_addr_zero_based(WN_COPY_Tree(ub),WN_COPY_Tree(lb)),
			      WN_Intconst(cwh_bound_int_typeid,1)) ;
	
	lb = cwh_addr_triplet(lb,ub,WN_Intconst(cwh_bound_int_typeid,1)) ;
	ar = cwh_addr_add_bound(ar,sz,lb);
     }
  }
  return(ar);
}

/*===============================================
 *
 * cwh_addr_nonc_util
 * 
 * Utility function for fei_non_conform_store.
 *
 * One of the two WN trees is a 1d temp, and the
 * other isn't. Find the ARRSECTION for the temp
 * and make it describe the nd shape of the other
 * tree's ARRSECTION. If the 1d expression was
 * just a 1d ARRSECTION, then the new ARRSECTION
 * is returned via the arguments.
 *
 *===============================================
 */ 
extern void
cwh_addr_nonc_util(WN **aa, WN **bb)
{
  WN *a  ;
  WN *b  ;
  WN *wn ;
  WN *as ;
  WN *bs ;
  WN *pa ;

  WN  *s1d ;
  WN  *p1d ;
  WN **a1d ;
  WN  *snd ;

  INT16 ar ;
  INT16 br ;

  a  = *aa;
  b  = *bb;
  as = cwh_addr_find_section(a,p_RETURN_SECTION); 
  bs = cwh_addr_find_section(b,p_RETURN_SECTION); 

  DevAssert((as != NULL), ("missing section"));
  DevAssert((bs != NULL), ("missing section"));

  ar = WN_kid_count(as);
  br = WN_kid_count(bs);


  /* find & revamp the 1d section */

  if (ar == br ) 
    return ;

  if (ar < br ) {

    s1d = as ;
    p1d = a  ;
    a1d = aa ;
    snd = bs ;
    

  } else {

    s1d = bs ;
    p1d = b  ;
    a1d = bb ;
    snd = as ;
  }

  pa = cwh_addr_find_section(p1d,p_RETURN_PARENT);
  wn = cwh_addr_nonc_recast(s1d,snd) ;

  if (pa != s1d) {

    if (WN_kid0(pa) == s1d) 
      WN_kid0(pa) = wn;
    else 
      WN_kid1(pa) = wn;

    wn = NULL;
  } else
    *a1d = wn ;
}

/*===============================================
 *
 * cwh_addr_nonc_recast
 * 
 * Utility function for fei_non_conform_store.
 *
 * The first argument is an ARRSECTION of a 1d
 * temp. Make a new ARRSECTION with the same
 * shape as the second argument. Deletes the
 * 1d tree.
 *
 *===============================================
 */ 
static WN *
cwh_addr_nonc_recast(WN *wt, WN *wa)
{
  WN * wn  ;
  WN * sc0 ;
  WN * sc1 ;
  WN * zr0 ;
  WN * one ;
  WN * lin ;

  INT16 nk ;
  INT16 i  ;

  BOOL  dope ;

  nk = WN_kid_count(wa);
  wn = WN_Create (opc_section,nk);
  WN_element_size(wn) = WN_element_size(wt) ;

  dope = (WN_element_size(wa) < 0) ;

  WN_kid(wn,0) = WN_kid(wt,0);
  WN_kid(wt,0) = NULL;

  DevAssert((WN_kid_count(wt) == 3),(" Not 1d"));

  FOREACH_AXIS(i,nk) {

    /* find extent of axis, maybe in dope, or size of VV subscript */

    if (dope) {
      sc0 = WN_kid(wa,i+SUB_OFF(nk));

      if (WNOPR(sc0) == OPR_ARRAYEXP)
	sc0 = WN_kid(sc0,1); 

      else {
	DevAssert((WNOPR(sc0) == OPR_TRIPLET),("nonc rhs"));
	sc0 = WN_COPY_Tree(WN_kid2(sc0));
      }

    } else
      sc0 = WN_COPY_Tree(WN_kid(wa,i+SZ_OFF(nk))) ;

    sc1 = WN_COPY_Tree(sc0);
    zr0 = WN_Intconst(cwh_bound_int_typeid,0);
    one = WN_Intconst(cwh_bound_int_typeid,1);
    sc1 = cwh_expr_bincalc(OPR_SUB,sc1,one);
    one = WN_Intconst(cwh_bound_int_typeid,1);

    WN_kid(wn,i+SZ_OFF(nk))  = sc0;
    WN_kid(wn,i+SUB_OFF(nk)) = cwh_addr_triplet(zr0,sc1,one);
  }

  /* it may be there was an offset into the 1d temp, if so */
  /* it was a linearization, so bump the address along     */

  DevAssert((WNOPR(WN_kid(wt,1+SUB_OFF(2))) == OPR_TRIPLET),(" No triplet"));

  lin = WN_kid0(WN_kid(wt,1+SUB_OFF(2)));

  if ((WNOPR(lin) != OPR_INTCONST) ||
      (WN_const_val(lin) != 0))  {

    lin = WN_COPY_Tree(lin);
    lin = cwh_expr_bincalc(OPR_MPY,lin,WN_CreateIntconst(opc_pint,WN_element_size(wt)));
    wn  = cwh_expr_bincalc(OPR_ADD,lin,wn);
  }

  WN_DELETE_Tree(wt);
  return wn ;
}


/*===============================================
 *
 * cwh_addr_access_flags
 *
 * Set the given ACCESS ID flags on the given ST,
 *
 *===============================================
 */ 
static  void
cwh_addr_access_flags(ST *st , INT fg)
{

  if (IN_NESTED_PU)
    if (HOST_ASSOCIATED(st))  {
      cwh_stab_add_pragma(st,(WN_PRAGMA_ACCESSED_FLAGS) fg ) ;
    }
}


/*===============================================
 *
 * cwh_addr_init_target
 *
 * Initialize all variables which set up 
 * target-dependent variables. eg: -n32/64.
 *
 *===============================================
 */ 

extern  void
cwh_addr_init_target(void)
{

  if (Pointer_Size == 4) {

    opc_lda   = OPC_U4LDA;
    opc_call  = OPC_U4CALL ;
    opc_array = OPC_U4ARRAY;
    opc_pint  = OPC_U4INTCONST;
    opc_sint  = OPC_I4INTCONST;
    opc_section  = OPC_U4ARRSECTION;
    opc_triplet  = OPC_I4TRIPLET ;
    cwh_addr_char_len_typeid = MTYPE_I4;
    cwh_bound_int_typeid = MTYPE_I4;
    cwh_doloop_typeid = MTYPE_I4;

  }  else {
    
    opc_lda   = OPC_U8LDA;
    opc_call  = OPC_U8CALL ;
    opc_pint  = OPC_U8INTCONST;
    opc_sint  = OPC_I8INTCONST;
    opc_array = OPC_U8ARRAY;
    opc_section  = OPC_U8ARRSECTION;
    opc_triplet  = OPC_I8TRIPLET ;
    cwh_addr_char_len_typeid = MTYPE_I4;
    cwh_bound_int_typeid = MTYPE_I8;
    cwh_doloop_typeid = MTYPE_I8;
  }
  cwh_types_init_target();
}




/*================================================================ 
 *
 * BOOL cwh_addr_f90_pointer_reference(WN *addr)
 *
 * Given an addressing node, this routine returns TRUE if
 * the ILOAD or ISTORE loads or stores to memory addressed by an 
 * F90 pointer. It is only reliable if the WHIRL coming out of 
 * the F90 lowerer has not had addressing nodes altered. 
 *
 *================================================================
 */

/* This routines works on the LOAD/STORE instead of the addresses */

static BOOL  cwh_addr_f90_pointer_reference_ls(WN * ls)
{
   OPERATOR opr;
   INT i,nkids;
   BOOL r;

   opr = WN_operator(ls);
   switch (opr) {
    case OPR_LDID:
    case OPR_LDA:
       return (FALSE);

    case OPR_ILOAD:
    case OPR_MLOAD:
       return ( cwh_addr_f90_pointer_reference(WN_kid0(ls)));

    case OPR_ISTORE:
    case OPR_MSTORE:
       return ( cwh_addr_f90_pointer_reference(WN_kid1(ls)));

    default:
       nkids = WN_kid_count(ls);
       r = FALSE;
       for (i=0 ; i < nkids; i++) {
	  r |= cwh_addr_f90_pointer_reference(WN_kid(ls,i));
       }
       return (r);
   }
}

extern BOOL 
cwh_addr_f90_pointer_reference(WN * addr)
{
   OPERATOR opr;
   ST *st;
   opr = WN_operator(addr);

   switch (opr) {
    case OPR_LDID:
       st = WN_st(addr);
       if (ST_class(st) == CLASS_VAR) {
	  return (ST_auxst_is_f90_pointer(st));
       }
       return (FALSE);
       
    case OPR_LDA:
       return (FALSE);
       
    case OPR_ILOAD:
       if (TY_is_f90_pointer(WN_load_addr_ty(addr)) || 
	   TY_is_f90_pointer(TY_pointed(WN_load_addr_ty(addr)))) {
	  return (TRUE);
       }
       return (FALSE);
       
    case OPR_ARRSECTION:
    case OPR_ARRAY:
    case OPR_ARRAYEXP:
       return (cwh_addr_f90_pointer_reference(WN_kid0(addr)));
       
    case OPR_INTCONST:
       return (FALSE);
       
    default:
       /* Treat as expression again */
       return ( cwh_addr_f90_pointer_reference_ls (addr));
   }
}


extern void
fei_field_dot(TYPE type)
{
   /* Doesn't do anything right now */
   return;
}
