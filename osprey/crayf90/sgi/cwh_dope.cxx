/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: cwh_dope
 * $Revision: 1.8 $
 * $Date: 05/09/27 07:36:38-07:00 $
 * $Author: scorrell@soapstone.pathscale.com $
 * $Source: 
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: contains routines to manipulate dope vectors. Although
 *              the FLDs of the type contain offset and size information,
 *              these routines rely on defines to avoid repetition of 
 *              the same search. The definitions are in cwh_types.h.
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_dope.cxx $ $Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "symtab.h"
#include "strtab.h"
#include "errors.h"
#include "config_targ.h"
#include "wn.h"
#include "wn_util.h"
#include "f90_utils.h"

/* Cray includes */

#include "i_cvrt.h"


/* conversion includes */

#include "cwh_defines.h"
#include "cwh_stk.h"
#include "cwh_stmt.h"
#include "cwh_types.h"
#include "cwh_expr.h"
#include "cwh_addr.h"


#define opc_dim OPC_I8INTCONST

static void  cwh_dope_store_bound(INT32 offset, INT32 dim) ;
static void  cwh_dope_read_bound(INT32 offset, INT32 dim) ;
#ifdef KEY /* Bug 6845 */
static void  cwh_dope_initialize(ST *st, WN * wa, TY_IDX ty, WN *dp[DOPE_USED],
  WN **bd, INT16 num_bnds, WN **alloc_cpnt, int n_alloc_cpnt);
#else /* KEY Bug 6845 */
static void  cwh_dope_initialize(ST *st, WN * wa, TY_IDX ty, WN *dp[DOPE_USED],WN **bd, INT16 num_bnds ) ;
#endif /* KEY Bug 6845 */
static void  cwh_dope_store (ST *st, WN *wa, OFFSET_64 off, TY_IDX  ty, WN *rhs) ;


/*===============================================
 *
 * fei_dv_def
 *
 * Dope vector initialization. The stack has
 * the dope fields, or nulls for empty slots
 * and these should be stored into the address
 * at the base of the stack. Unused fields in
 * the descriptor were not pushed onto the stack.
 *
 * Push a null operation for fei_store to ignore.
 *
 *===============================================
 */ 
extern void 
#ifdef KEY /* Bug 6845 */
fei_dv_def(INT32 num_dims, INT32 n_alloc_cpnt )
#else /* KEY Bug 6845 */
fei_dv_def(INT32 num_dims )
#endif /* KEY Bug 6845 */
{
  WN * dp[DOPE_USED];
  WN * bd[BOUND_NM * MAX_ARY_DIMS];
  ST * st   ;
  WN * wa;
  FLD_IDX fld ;
  TY_IDX ty;

  INT16 n,i;

#ifdef KEY /* Bug 6845 */
  WN ** alloc_cpnt = (WN **) alloca(n_alloc_cpnt * sizeof *alloc_cpnt);
  for (i = n_alloc_cpnt - 1; i >= 0; i -= 1) {
    alloc_cpnt[i] = cwh_expr_operand(NULL);
  }
#endif /* KEY Bug 6845 */

  n = num_dims * BOUND_NM ;

  for( i = n-1 ; i >= 0  ; i --) 
    bd[i] = cwh_expr_operand(NULL);

  for( i = DOPE_USED-1 ; i >= 1 ; i--)
    dp[i] = cwh_expr_operand(NULL);
  
  dp[0] = cwh_expr_address(f_NONE);

  if (cwh_stk_get_class() == ST_item || cwh_stk_get_class() == ST_item_whole_array) {
     st = cwh_stk_pop_ST();
     wa = NULL;
     ty = 0;

  } else if (cwh_stk_get_class() == FLD_item) {
     fld = cwh_stk_pop_FLD();
     cwh_stk_push((void *) (INTPTR)fld,FLD_item);
     ty = FLD_type(FLD_HANDLE (fld));
     wa = cwh_expr_address(f_NONE);
     st = NULL;

  } else {
     wa = cwh_expr_address(f_NONE);
     st = NULL;
     ty = 0;
  }
#ifdef KEY /* Bug 6845 */
  cwh_dope_initialize(st,wa,ty,dp,bd,n,alloc_cpnt,n_alloc_cpnt);
#else /* KEY Bug 6845 */
  cwh_dope_initialize(st,wa,ty,dp,bd,n);
#endif /* KEY Bug 6845 */

  /* These are going to be ignored */
  cwh_stk_push(st,ST_item);
  cwh_stk_push(NULL,WN_item);

}

/*===============================================
 *
 * fei_get_dv_low_bnd
 *
 * Get low bound for dimension dim.
 *
 *===============================================
 */ 
extern void 
fei_get_dv_low_bnd(INT32 dim,INT32 expand)
{
  cwh_dope_read_bound(0,dim);
}

/*===============================================
 *
 * fei_get_dv_extent
 *
 * Get extent for dimension dim.
 *
 *===============================================
 */ 
extern void 
fei_get_dv_extent(INT32 dim,INT32 expand)
{
  cwh_dope_read_bound(DOPE_bound_sz,dim);
}

/*===============================================
 *
 * fei_get_dv_str_mult
 *
 * Get extent for dimension dim.
 *
 *===============================================
 */ 
extern void 
fei_get_dv_str_mult(INT32 dim,INT32 expand)
{
  cwh_dope_read_bound((2 * DOPE_bound_sz),dim);
}

/*===============================================
 *
 * fei_set_dv_low_bnd
 *
 * Set low bound for dimension dim.
 *
 *===============================================
 */ 
extern void 
fei_set_dv_low_bnd(INT32 dim)
{
  cwh_dope_store_bound(0,dim);
}

/*===============================================
 *
 * fei_set_dv_extent
 *
 * Set extent for dimension dim.
 *
 *===============================================
 */ 
extern void 
fei_set_dv_extent(INT32 dim)
{
  cwh_dope_store_bound(DOPE_bound_sz,dim);
}

/*===============================================
 *
 * fei_set_dv_str_mult
 *
 * Set extent for dimension dim.
 *
 *===============================================
 */ 
extern void 
fei_set_dv_str_mult(INT32 dim)
{
  cwh_dope_store_bound((2 * DOPE_bound_sz),dim);
}

/*===============================================
 *
 * fei_dv_deref
 *
 * get the address of the space pointed to by
 * the dope. Assumes address is 1st field.
 * If it's an assumed shape dummy then load the
 * address first.
 *
 *===============================================
 */ 
extern void 
fei_dv_deref(TYPE result)
{
  ST  * st ;
  WN  * wn ;
  WN  * wa;
  TY_IDX  ty, tp ;
  FLD_IDX  fld;
  TY_IDX dope_ty;
  char *field_name;

  if (cwh_stk_get_class() == ST_item || cwh_stk_get_class() == ST_item_whole_array) {
     st = cwh_stk_pop_ST();
     dope_ty = ST_type(st);
     
     if (ST_sclass(st) == SCLASS_FORMAL) {
	dope_ty = cwh_types_array_TY(dope_ty); 
     }
     ty = FLD_type(TY_fld(Ty_Table[dope_ty]));
     wn  = cwh_addr_load_ST(st,ADDR_OFFSET,ty);

  } else if (cwh_stk_get_class() == FLD_item) {

     field_name = cwh_stk_fld_name();
     fld = cwh_stk_pop_FLD();
     cwh_stk_push((void *)(INTPTR)fld,FLD_item);
     wn = cwh_expr_address(f_NONE);
     dope_ty = FLD_type(FLD_HANDLE (fld)); /* get the dope TY_IDX */
     ty = FLD_type(TY_fld(Ty_Table[dope_ty]));
     if (cwh_addr_f90_pointer_reference(wn)) {
	tp = cwh_types_mk_f90_pointer_ty(ty);
     } else {
	tp = cwh_types_make_pointer_type(dope_ty, FALSE);
     }
     
     wn = WN_CreateIload (OPCODE_make_op(OPR_ILOAD,Pointer_Mtype,Pointer_Mtype),
			  ADDR_OFFSET,ty,tp,wn);
     SET_ARRAY_NAME_MAP(wn,field_name);
  } else {

     wn = cwh_expr_operand(NULL);
     dope_ty = 0;
  }
  cwh_stk_push_typed(wn,DEREF_item,dope_ty);
}

/*===============================================
 *
 * fei_get_dv_hdr_fld 
 *
 * get the appropriate field information from a dope vector
 * Assumes dope vector is on the stack.
 *
 *===============================================
 */ 
extern void 
#ifdef KEY /* Bug6845 */
fei_get_dv_hdr_fld(dv_idx_type field)
#else /* KEY Bug6845 */
fei_get_dv_hdr_fld(INT32 field)
#endif /* KEY Bug6845 */
{
   INT32 offset;
   INT32 rshift;
   INT64 mask;
   TYPE_ID ty;

   ST *st;
#ifdef KEY /* Bug 10177 */
   WN *wn = 0;
#else /* KEY Bug 10177 */
   WN *wn;
#endif /* KEY Bug 10177 */

   /* Get the information about the appropriate fields needed */
   cwh_types_get_dope_info(field, &offset, &rshift, &mask, &ty);
   
   switch(cwh_stk_get_class()) {
    case ST_item:
    case ST_item_whole_array:
      st = cwh_stk_pop_ST();
      wn = cwh_addr_load_ST(st,offset,Be_Type_Tbl(ty));
      break ;
      
    case WN_item:
    case WN_item_whole_array:
    case FLD_item:
      wn = cwh_expr_address(f_NONE);
      wn = cwh_addr_load_WN(wn,offset,Be_Type_Tbl(ty)); 
      break ;
      
    default:
      DevAssert((0),(" Odd dope load"));
      break;
   }

   /* See if we need to shift and mask */
   if (rshift != 0) {
      wn = cwh_expr_bincalc(OPR_LSHR,wn,WN_Intconst(MTYPE_I4,rshift));
   }
   if (mask != 0) {
      wn = cwh_expr_bincalc(OPR_BAND,wn,WN_Intconst(ty,mask));
   }

   cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * fei_set_dv_hdr_fld 
 *
 * set the appropriate field information into a dope vector
 * Assumes dope vector is on the stack.
 *
 *===============================================
 */ 
extern void 
#ifdef KEY /* Bug6845 */
fei_set_dv_hdr_fld(dv_idx_type field)
#else /* KEY Bug6845 */
fei_set_dv_hdr_fld(INT32 field)
#endif /* KEY Bug6845 */
{
   INT32 offset;
   INT32 rshift;
   INT64 mask,mask_complement;
   TYPE_ID ty;
   TYPE_ID addr_ty;
   BOOL needs_load;
   FLD_HANDLE fl;

   ST *st;
   WN *wn;
   WN *arg,*old_value;

   /* Get the information about the appropriate fields needed */
   cwh_types_get_dope_info(field, &offset, &rshift, &mask, &ty);
   mask_complement = mask;
   needs_load = FALSE;

   /* Special cases for 1 and 9 base_address and orig_base */
#ifdef KEY /* Bug6845 */
   if (field == DV_BASE_IDX || field == DV_ORIG_BASE_IDX)
#else /* KEY Bug6845 */
   if (field == 1 || field == 9)
#endif /* KEY Bug6845 */
   {
      arg = cwh_expr_address(f_NONE);
   } else {
      arg = cwh_expr_operand(NULL);
   }

   /* Get arg in the right place if need be */
   if (mask != 0) {
      arg = cwh_expr_bincalc(OPR_BAND,arg,WN_Intconst(ty,mask));
      needs_load = TRUE;
   }
   if (rshift != 0) {
      arg = cwh_expr_bincalc(OPR_SHL,arg,WN_Intconst(MTYPE_I4,rshift));
      mask_complement <<= rshift;
      needs_load = TRUE;
   }
   mask_complement = ~mask_complement;
      
#ifdef KEY /* Bug 8128 */
   int tos_class = cwh_stk_get_class();
#endif /* KEY Bug 8128 */
   switch(cwh_stk_get_class()) {
    case ST_item:
    case ST_item_whole_array:

      addr_ty = cwh_stk_get_TY();

      st = cwh_stk_pop_ST();

      if (! addr_ty) {
         addr_ty = ST_type(st);
      }

      if (needs_load) {
	 old_value = cwh_addr_load_ST(st,offset,Be_Type_Tbl(ty));
	 if (mask != 0) {
	    old_value = cwh_expr_bincalc(OPR_BAND,old_value,WN_Intconst(ty,mask_complement));
	    arg = cwh_expr_bincalc(OPR_BIOR,arg,old_value);
	 }
      }

#ifdef KEY /* Bug6845 */
      if (field == DV_BASE_IDX || field == DV_ORIG_BASE_IDX)
#else /* KEY Bug6845 */
      if (field == 1 || field == 9)
#endif /* KEY Bug6845 */
      {
         if (TY_kind(addr_ty) == KIND_POINTER) addr_ty = TY_pointed(addr_ty);

         /* addr_ty should be the TY of a dope vector Dope */

         TY & tt = Ty_Table[addr_ty];
         fl = TY_fld(tt);
         addr_ty = FLD_type(fl);
         DevAssert((TY_kind(addr_ty) == KIND_POINTER),(" base not pointer "));
      } else {
         addr_ty = Be_Type_Tbl(ty);
      }
      cwh_addr_store_ST(st,offset,addr_ty,arg);
      break ;
      
    case WN_item:
    case WN_item_whole_array:
    case FLD_item:

      if (cwh_stk_get_class() == FLD_item) {
         addr_ty = cwh_stk_get_FLD_TY();
      } else {
         addr_ty = cwh_stk_get_TY();
      }

      wn = cwh_expr_address(f_NONE);

      if (! addr_ty) {
         addr_ty = cwh_types_WN_TY(wn, TRUE);
      }

      if (needs_load) {
	 old_value = cwh_addr_load_WN(WN_COPY_Tree(wn),offset,Be_Type_Tbl(ty));
	 if (mask != 0) {
	    old_value = cwh_expr_bincalc(OPR_BAND,old_value,WN_Intconst(ty,mask_complement));
	    arg = cwh_expr_bincalc(OPR_BIOR,arg,old_value);
	 }
      }

#ifdef KEY /* Bug6845 */
      if (field == DV_BASE_IDX || field == DV_ORIG_BASE_IDX)
#else /* KEY Bug6845 */
      if (field == 1 || field == 9)
#endif /* KEY Bug6845 */
      {
         if (TY_kind(addr_ty) == KIND_POINTER) addr_ty = TY_pointed(addr_ty);

         /* addr_ty should be the TY of a dope vector Dope */

         TY & tt = Ty_Table[addr_ty];
         fl = TY_fld(tt);
         addr_ty = FLD_type(fl);
         DevAssert((TY_kind(addr_ty) == KIND_POINTER),(" base not pointer "));
      } else {
         addr_ty = Be_Type_Tbl(ty);
      }
#ifdef KEY /* Bug 8128 */
      /*
       * Special case: array of structure containing a member of type pointer.
       * Can't generate a whole-array or array-section reference to such a
       * member explicitly in the source program because you get error 408,
       * "This pointer is to the right of a part-ref with nonzero rank." But
       * it comes up implicitly when the front end tries to generate dope info.
       */
      if (FLD_item == tos_class && 0 < WN_kid_count(wn) &&
         OPR_ARRSECTION == WN_operator(WN_kid(wn, 0))) {
	 wn = F90_Wrap_ARREXP(wn) ;
      }
#endif /* KEY Bug 8128 */
      cwh_addr_store_WN(wn,offset,addr_ty,arg); 
      break ;
      
    default:
      DevAssert((0),(" Odd dope store"));
      break;
   }
}



/*===============================================
 *
 * arrsection_to_array 
 *
 * Turn ARRSECTION nodes into ARRAY nodes pointing at the 
 * base address of the arrsection. Expects as input an ARRSECTION
 * or ARRAY node. Does its work in place.
 * 
 *===============================================
 */ 
static void arrsection_to_array(WN *addr)
{
   INT i,ndim;
   WN *temp;
   OPERATOR opr;

   opr = WNOPR(addr);

   if (opr == OPR_ARRSECTION || opr == OPR_ARRAY) {
      WN_set_opcode(addr,OPCODE_make_op(OPR_ARRAY,Pointer_Mtype,MTYPE_V));
      arrsection_to_array(WN_kid0(addr));
      ndim = (WN_kid_count(addr)-1)/2;
      for (i=ndim+1; i < 2*ndim + 1; i++) {
	 if (WNOPR(WN_kid(addr,i)) == OPR_TRIPLET) {
	    temp = WN_kid(addr,i);
	    WN_kid(addr,i) = WN_kid0(temp);
	    WN_DELETE_Tree(WN_kid1(temp));
	    WN_DELETE_Tree(WN_kid2(temp));
	    WN_Delete(temp);
	 }
      }
   } else if (opr == OPR_ADD || opr == OPR_MPY || opr == OPR_SUB) {
      /* character(*) case */
      arrsection_to_array(WN_kid0(addr));
      arrsection_to_array(WN_kid1(addr));
   }
   return;
}

/*===============================================
 *
 * fei_dv_ptr_asg 
 *
 * Set the address field in a dope vector.
 * Assumes dope vector is on the stack.
 *
 *===============================================
 */ 
extern void 
fei_dv_ptr_asg(void)
{
   WN *addr;
   
   /* Probably should set the address/taken/saved bit */
   addr = cwh_expr_address(f_T_SAVED);
   arrsection_to_array(addr);
   cwh_stk_push(addr,WN_item);
#ifdef KEY /* Bug6845 */
   fei_set_dv_hdr_fld(DV_BASE_IDX); /* store it */
#else /* KEY Bug6845 */
   fei_set_dv_hdr_fld(1); /* store it */
#endif /* KEY Bug6845 */
}

/*===============================================
 *
 * cwh_dope_read_bound
 *
 * Read a  bound of dimension dim. The offset is
 * the extra for a lb,extent or stride, in addition
 * to the basic dimension,
 *
 *===============================================
 */ 
static void
cwh_dope_read_bound(INT32 offset, INT32 dim)
{
  WN  * wa ;
#ifdef KEY /* Bug 10177 */
  WN  * wn = 0;
#else /* KEY Bug 10177 */
  WN  * wn ;
#endif /* KEY Bug 10177 */
  ST  * st ;
  WN_OFFSET off;

  off = DOPE_dim_offset + offset + (DIM_SZ * (dim-1)) ;

  switch(cwh_stk_get_class()) {
  case ST_item:
  case ST_item_whole_array:
    st = cwh_stk_pop_ST();
    wn = cwh_addr_load_ST(st,off,DOPE_bound_ty);
    break ;

  case WN_item:
  case WN_item_whole_array:
  case FLD_item:
    wa = cwh_expr_address(f_NONE);
    wn = cwh_addr_load_WN(wa,off,DOPE_bound_ty); 
    break ;

  default:
    DevAssert((0),(" Odd dope load"));
    break;
  }

  wn = cwh_convert_to_ty(wn,cwh_bound_int_typeid);

  cwh_stk_push(wn,WN_item);
}

/*===============================================
 *
 * cwh_dope_store_bound
 *
 * Store a bound of dimension dim. The offset is 
 * the extra for a lb,extent or stride, in addition
 * to the basic dimension,
 *
 *===============================================
 */ 
static void
cwh_dope_store_bound(INT32 offset, INT32 dim)
{
  WN  * wn ;
  WN  * wa ;
  ST  * st ;
  OFFSET_64 off;

  off = DOPE_dim_offset + offset + (DIM_SZ * (dim-1)) ;
  wn  = cwh_expr_operand(NULL);

  switch(cwh_stk_get_class()) {
  case ST_item:
  case ST_item_whole_array:
    st = cwh_stk_pop_ST();
    cwh_addr_store_ST(st,off,DOPE_bound_ty,wn);
    break ;

  case WN_item:
  case WN_item_whole_array:
  case FLD_item:
    wa = cwh_expr_address(f_NONE);
    cwh_addr_store_WN(wa,off,DOPE_bound_ty,wn); 
    break ;

  default:
    DevAssert((0),(" Odd dope store"));
    break;
  }
}


/*================================================================
 *
 * cwh_dope_get_dope_fudge_factor
 *
 * This routine returns 1 for INTEGER(1) base types, 2 for INTEGER(2)
 * base types, and 4 for everything else. It is needed to interpret
 * the stride in dope vectors.
 *
 * Stride is in word (4 byte) elements for types whose size is >= I4, 
 * unless it's a character-only thing, when bytes are used. 
 * I1,I2,L1,L2 get element size stride (1 or 2 bytes).
 *
 *================================================================
 */

static INT64
cwh_dope_get_dope_fudge_factor(TY_IDX ty)
{
#ifdef KEY /* Bug 10177 */
   TY_IDX base_ty = 0;
#else /* KEY Bug 10177 */
   TY_IDX base_ty;
#endif /* KEY Bug 10177 */
   TYPE_ID t;

   TY& tt = Ty_Table[ty];
   if (TY_kind(ty) == KIND_ARRAY) {
      return (cwh_dope_get_dope_fudge_factor(TY_etype(tt))); 
   } else if (TY_kind(ty) == KIND_STRUCT) {
      if (TY_is_packed(tt)) return(1);
      return (4);
   } else if (TY_kind(ty) == KIND_SCALAR) {
      base_ty = ty;
   } else {
      DevAssert((0),("Do not know what to do with type"));
   }

   if (TY_is_character(Ty_Table[base_ty])) {
      return (1);
   }
   t = TY_mtype(base_ty);
   if (MTYPE_byte_size(t) < 4) {
      return (MTYPE_byte_size(t));
   }
   return (4);
}

/*===============================================
 *
 * cwh_dope_from_expression
 *
 * expr must be an ILOAD, an MLOAD, or any expression which represents the
 * address of something. If array is NULL expr must contain exactly one
 * ARRSECTION node. If array is non-null, expr must not contain an
 * ARRSECTION. If char_len is non-null, the dope vector will be made to
 * represent a character expression of length char_len. 
 * 
 * tarray is the TY of the array object the dope is for - the 
 * base expression may be from a structure.
 *
 *================================================================
 */

extern WN * 
cwh_dope_from_expression(WN *expr, WN *array, WN *char_len, TY_IDX tarray,
WN *craytype_wn)
{
  WN * wn ;
  WN * wt ;
  ST * st ;
  TY_IDX  tc ;
  TY_IDX  ty ;
  WN * se;
  WN * lower_bound;
  WN * stride_mult_accum;
  WN * address_fixup;
  INT64 element_size_multiplier;
  INT64 craytype;
  WN_ESIZE element_size;
  BOOL     non_contig;
  INT64  offset;
  
  FLD_IDX  fl ;

  WN * dp[DOPE_USED];
  WN * bd[BOUND_NM * MAX_ARY_DIMS];

  INT32 nd ;
  INT16 i,j ;

  if (WNOPR(expr) == OPR_ILOAD || WNOPR(expr) == OPR_MLOAD) {
     /* 
      * Get the offset and the scalar type, 
      *	then clean up expr and set it to point to tha address child.
      */
     offset = WN_offset(expr);
     if (WN_kid_count(expr)==2) {
	WN_DELETE_Tree(WN_kid1(expr));
     }
     se = WN_kid0(expr);
     WN_Delete(expr);
     expr = se;
  } else {
     offset = 0;
  }

  se = cwh_addr_find_section(expr,p_RETURN_SECTION);
  if (!se) {
     se = array;
  }

  DevAssert((se),("Can't find an array section or an array to use"));
#ifdef KEY
  DevAssert((tarray != 0),("Missing TY"));
#else
  DevAssert((tarray != NULL),("Missing TY"));
#endif

  element_size = WN_element_size(se);
  if (element_size < 0) {
     element_size = -element_size;
     non_contig = TRUE;
  } else {
     non_contig = FALSE;
  }
  nd = WN_num_dim(se);

  /* Step 1: get the stride multiplier scale factor */
  element_size_multiplier = element_size/cwh_dope_get_dope_fudge_factor(tarray);
  if (element_size_multiplier == 0) element_size_multiplier = 1; 

  if (char_len) {
     dp[1] = WN_COPY_Tree(char_len);
     /* The stride multiplier is in bytes for this type */
     stride_mult_accum = WN_Intconst(cwh_bound_int_typeid,element_size);
  } else {
     dp[1] = WN_Intconst(Pointer_Mtype,element_size*8);
     stride_mult_accum = WN_Intconst(cwh_bound_int_typeid,element_size_multiplier);
  }

  /* Step 2, build up the lbound, extents, and stride_multiplier fields, 
   * and alter the section node to be an ARRAY node.
   * bounds from section - these are always
   * simple triplets or scalar subscripts. 
  */
  
  j = 0 ;

  for (i = 2*nd; i >= nd+1 ; i --) {
     wt = WN_kid(se,i) ;
     if (WNOPR(wt) == OPR_TRIPLET) {
	/* Replace a TRIPLET node in the section with its lower bound */
	WN_kid(se,i) = WN_kid0(wt);
	/* Copy the extent to the dope vector */
	bd[j+1] = cwh_expr_bincalc(OPR_MAX,WN_kid2(wt),WN_Zerocon(cwh_bound_int_typeid));
	if (non_contig) {
	   bd[j+2] = cwh_expr_bincalc(OPR_MPY,WN_COPY_Tree(WN_kid(se,i-nd)),
			              WN_kid1(wt));
	   /* Correct for character, etc, in derived types */
	   bd[j+2] = cwh_expr_bincalc(OPR_MPY,bd[j+2],WN_Intconst(cwh_bound_int_typeid,
								  element_size_multiplier));
	} else {
	   bd[j+2] = cwh_expr_bincalc(OPR_MPY,WN_kid1(wt),WN_COPY_Tree(stride_mult_accum));
	}
	/* Don't need the triplet anymore */
	WN_Delete(wt);
     } else {
	/* Extent = 1 */
	bd[j+1] = WN_Intconst(cwh_bound_int_typeid,1);
	if (non_contig) {
	   bd[j+2] = WN_COPY_Tree(WN_kid(se,i-nd));
	   /* Correct for character, etc, in derived types */
	   bd[j+2] = cwh_expr_bincalc(OPR_MPY,bd[j+2],WN_Intconst(cwh_bound_int_typeid,
								  element_size_multiplier));
	} else {
	   bd[j+2] = WN_COPY_Tree(stride_mult_accum);
	}
     }
     bd[j] = WN_Intconst(cwh_bound_int_typeid,1);
     j+= BOUND_NM;
     if (i != nd+1 && !non_contig) {
	stride_mult_accum = cwh_expr_bincalc(OPR_MPY,stride_mult_accum,WN_COPY_Tree(WN_kid(se,i-nd)));
     }
  }
  WN_DELETE_Tree(stride_mult_accum);

  /* Turn the ARRSECTION into an ARRAY node */ 
  WN_set_opcode(se,OPCODE_make_op(OPR_ARRAY,Pointer_Mtype,MTYPE_V));
  /* Add the constant offset to the expression */
  expr = cwh_expr_bincalc(OPR_ADD,expr,WN_Intconst(Pointer_Mtype,offset));
  
  /* Set the base address */
  dp[0] = expr;

  /* Step 3, fill in the flag fields of the dope vector */
  /* contig, ptr flags, rank; assume non-contiguous, associated non-pointer */

  dp[2] = WN_Intconst(MTYPE_U4,1); /* associated */
  dp[3] = WN_Intconst(MTYPE_U4,0); /* allocated by pointer */
  dp[4] = WN_Intconst(MTYPE_U4,0); /* p_or_a */
  dp[5] = WN_Intconst(MTYPE_U4,0); /* contig */
  dp[6] = WN_Intconst(MTYPE_U4,nd);

  if (craytype_wn == NULL) {
      /* type code  **FIX */
      if (!char_len) {
         craytype = cwh_cray_type_from_TY(tarray);
      } else {
         f90_type_t *f90_type_ptr;
         f90_type_ptr = (f90_type_t *)&craytype;
         craytype = 0;
         f90_type_ptr->type = 6;
         f90_type_ptr->int_len = 8;
      }
      craytype_wn = WN_Intconst(MTYPE_U8,craytype);
  }

  dp[7] = WN_COPY_Tree(craytype_wn);

  /* original base and address - 0 unless allocatable */
  dp[8] = WN_Intconst(Pointer_Mtype,0);
  dp[9] = WN_Intconst(Pointer_Mtype,0);

  /* Create the dope vector */
  ty = cwh_types_dope_TY(nd,tarray,FALSE,FALSE,
#ifdef KEY /* Bug6845 */
    /* I hope this can't be an array of derived types having allocatable
     * components. If that's ever possible, we need to figure out from the
     * source type how many allocatable components there are. */
    0
#endif /* KEY Bug 6845 */
    );
  wn = cwh_expr_temp(ty,NULL,f_T_PASSED);
#ifdef KEY /* Bug 6845 */
  dp[10] = WN_Intconst(MTYPE_U4, 0);
  cwh_dope_initialize(WN_st(wn),NULL,0,dp,bd,nd*BOUND_NM,0,0);  
#else /* KEY Bug 6845 */
  cwh_dope_initialize(WN_st(wn),NULL,0,dp,bd,nd*BOUND_NM);  
#endif /* KEY Bug 6845 */
  return(wn);

}

/*===============================================
 *
 * cwh_dope_initialize_body
 *
 * Given the ST of a dope vector, initialize
 * all fields except the bounds.
 *
 *===============================================
 */ 
static void
#ifdef KEY /* Bug 6845 */
cwh_dope_initialize(ST *st, WN *wa, TY_IDX dope_ty, WN *dp[DOPE_USED],WN **bd,
  INT16 num_bnds, WN **alloc_cpnt, INT16 n_alloc_cpnt)
#else /* KEY Bug 6845 */
cwh_dope_initialize(ST *st, WN *wa, TY_IDX dope_ty, WN *dp[DOPE_USED],WN **bd, INT16 num_bnds )
#endif /* KEY Bug 6845 */
{
  INT16  i ;
  INT16 sz ;

  FLD_HANDLE  fli ;
  FLD_HANDLE  fl ;
  FLD_HANDLE  ft ;
  TY_IDX ty ;
  WN  * wr ;
  WN  * wt ;

#ifdef KEY /* Bug 10177 */
  OFFSET_64 off = 0;
#else /* KEY Bug 10177 */
  OFFSET_64 off;
#endif /* KEY Bug 10177 */
  OFFSET_64 invar_off;
  INT shift;

  if (dope_ty == 0) {
     if ( wa == NULL ) {
	fli = TY_fld(Ty_Table[ST_type(st)]);
     } else {
	fli = TY_fld(Ty_Table[cwh_types_WN_TY(wa, FALSE)]);
     }
  } else {
     fli = TY_fld(Ty_Table[dope_ty]);
  }

  /* address, element len */

  if (dp[0] != NULL )
      cwh_dope_store(st,wa,FLD_ofst(fli),FLD_type(fli),dp[0]) ;

  fli = FLD_next(fli);
  invar_off = FLD_ofst(fli);
  fl = TY_fld(Ty_Table[FLD_type(fli)]);
  if (dp[1] != NULL )
     cwh_dope_store(st,wa,invar_off + FLD_ofst(fl),FLD_type(fl),dp[1]);
  
  /* assoc, ptr_alloc,ptr_or_a, a_contig */
  
  wr = NULL;
  fl = FLD_next(fl);
  sz = MTYPE_size_best(TY_mtype(FLD_type(fl)));
  ft = fl ;
  
# if (defined(linux) || defined(BUILD_OS_DARWIN))
  {
    dope_header1_type	dh1;
 
  // assoc 
    if (dp[2] != NULL)
      dh1.assoc = WN_const_val(dp[2]);
    else
      dh1.assoc = 0;
     ft = FLD_next(ft);

  // ptr_alloc

    if (dp[3] != NULL)
      dh1.ptr_alloc = WN_const_val(dp[3]);
    else
      dh1.ptr_alloc = 0;
     ft = FLD_next(ft);

  // ptr_or_a

    if (dp[4] != NULL)
      dh1.p_or_a = WN_const_val(dp[4]);
    else
      dh1.p_or_a = 0;
     ft = FLD_next(ft);

  // a_contig
    if (dp[5] != NULL)
      dh1.a_contig = WN_const_val(dp[5]);
    else
      dh1.a_contig = 0;
     ft = FLD_next(ft);

#ifdef KEY /* Bug 6845 */
  // alloc_cpnt
    dh1.alloc_cpnt = WN_const_val(dp[10]);
    ft = FLD_next(ft);
#endif /* KEY Bug 6845 */

    dh1.unused = 0;

    wr = WN_Intconst(MTYPE_U4,*(UINT32*)&dh1);

  }
# else
  for (i = 0 ; i < 4 ; i ++ ) {
     if (dp[i+2] != NULL ) {
	shift = sz - FLD_bofst(ft) - FLD_bsize(ft);
	if (shift != 0) {
	   wt = WN_Intconst(MTYPE_U4,shift);
	   wt = cwh_expr_bincalc(OPR_SHL,dp[i+2],wt);
	} else {
	   wt = dp[i+2];
	}
	
	if (wr == NULL) 
	   wr = wt ;
	else
	   wr = cwh_expr_bincalc(OPR_BIOR,wr,wt);
     }
     ft = FLD_next(ft);
  }
# endif
  
  if (wr != NULL) 
     cwh_dope_store(st,wa,invar_off + FLD_ofst(fl),FLD_type(fl),wr);    
  
  /* ignore unused fields & add in rank */
  fl = FLD_next(ft);
  
  if (dp[6] != NULL ) {
# if (defined(linux) || defined(BUILD_OS_DARWIN))
     dope_header2_type dh2;

     dh2.unused = 0;
     dh2.n_dim = WN_const_val(dp[6]);
     wr = WN_Intconst(MTYPE_U4,*(UINT32*)&dh2);
     cwh_dope_store(st,wa,invar_off + FLD_ofst(fl),FLD_type(fl),wr);
# else
     cwh_dope_store(st,wa,invar_off + FLD_ofst(fl),FLD_type(fl),dp[6]);    
# endif
  }
  
//  /* Initialize the first four unused bytes of the f90_type structure */
//  fl = FLD_next(fl);
//  cwh_dope_store(st,wa,invar_off + FLD_ofst(fl),FLD_type(fl),
//		 WN_Intconst(MTYPE_U4,0));
  
  /* type code */
  fl = FLD_next(fl);
  
  if (dp[7] != NULL) 
     cwh_dope_store(st,wa,invar_off + FLD_ofst(fl),FLD_type(fl),dp[7]);    
  
  /* original base and address */
  
  fl = FLD_next(fl);
  if (dp[8] != NULL) 
     cwh_dope_store(st,wa,invar_off + FLD_ofst(fl),FLD_type(fl),dp[8]);    
  
  fl = FLD_next(fl);
  if (dp[9] != NULL) 
     cwh_dope_store(st,wa,invar_off + FLD_ofst(fl),FLD_type(fl),dp[9]);
  
  
  /* add bounds - assumes all same size */
  
  if (num_bnds > 0 ) {
     
     fli  = FLD_next(fli) ;
     off = FLD_ofst(fli) ;
     ty  = DOPE_bound_ty     ;
     sz  = bit_to_byte(MTYPE_size_best(TY_mtype(ty)));
     
     for (i = 0 ; i < num_bnds  ; i ++ ) {
	if (bd[i] != NULL ) 
	   cwh_dope_store(st,wa,off,ty,bd[i]);
      off += sz ;
     }
  }

#ifdef KEY /* Bug 6845 */
  /* If this is an allocatable array whose element is a derived type having
   * component(s) which are themselves allocatable, emit a list of byte offsets
   * within the structure to each of the allocatable components, preceded by
   * a count of the number of offsets.
   */
  if (n_alloc_cpnt) {

     cwh_dope_store(st,wa,off,DOPE_bound_ty,
       WN_Intconst(MTYPE_U4, n_alloc_cpnt));
     off += DOPE_bound_sz;

     for (i = 0; i < n_alloc_cpnt; i += 1) {
       cwh_dope_store(st,wa,off,DOPE_bound_ty,alloc_cpnt[i]);
       off += DOPE_bound_sz;
     }
  }
#endif /* KEY Bug 6845 */
}

/*===============================================
 *
 * cwh_dope_store
 *
 * Utility routine to store dope fields for 
 * an address, or an ST.
 *
 *===============================================
 */ 
static void
cwh_dope_store (ST *st, WN *wa, OFFSET_64 off, TY_IDX  ty, WN *rhs)
{
  if (wa == NULL) {
     cwh_addr_store_ST(st,off,ty,rhs);
  } else {
     wa = F90_Wrap_ARREXP(WN_COPY_Tree(wa));
     cwh_addr_store_WN(wa,off,ty,rhs);
  }
}
