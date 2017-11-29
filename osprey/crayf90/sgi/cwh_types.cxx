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
 * Module: cwh_types.c
 * $Revision: 1.7 $
 * $Date: 05/04/18 13:43:18-07:00 $
 * $Author: scorrell@limestone.keyresearch $
 * $Source: crayf90/sgi/SCCS/s.cwh_types.cxx $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Handles the conversion of types to TYs. The
 *              entry points are *
 *
 *                fei_new_descriptor - new intrinsic/arraytype
 *                fei_array_dimen    - new bound for array type
 *                fei_next_type_idx  - new TY for derived type
 *                fei_user_type      - new derived type
 *                fei_member         - new component of derived type
 *                fei_new_dope_vector - new descriptor for deferred shape
 *                                      or pointer objects.
 *
 *              The TY is created, possibly in stages, then handed back
 *              to the interface which will store it and pass it back
 *              when a type is required.
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_types.cxx $ $Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "stab.h"
#include "ttype.h"
#include "strtab.h"
#include "config_targ.h"
#include "errors.h"
#include "wn.h"
#include "wn_util.h"

/* Cray includes */

#include "i_cvrt.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_expr.h"
#include "cwh_addr.h"
#include "cwh_block.h"
#include "cwh_preg.h" 
#include "cwh_stab.h"
#include "cwh_auxst.h"
/* #include "cwh_stmt.h" */
#include "cwh_types.h"
#include "cwh_stk.h"
#include "cwh_types.i"
#include "sgi_cmd_line.h"

/*#include "cwh_stats.h"  */

#define BUMP_TY_COUNTER(x) 

/*===================================================
 *
 * fei_descriptor
 *
 * This is the PDGCS call to make a new type.
 * Fill in the blanks of the PDGCS TYPE and return
 * it (space is allocated in caller). We only use 
 * the TY from the TYPE, via a cast for now. 
 * 
 * For an array, take the information set up in
 * decl_bounds, ty_dim1 & last_bitsize by calls to
 * fei_array_dimen and pass them to a utility routine.
 *
 ====================================================
*/

TYPE
fei_descriptor (INT32        flag_matrix,
		INT32        table_type,
		INTPTR       size,
		INT32        basic_type,
		INT32        aux_info,
		INT32        alignment)
    
{
  TYPE t     ;
  mUINT16 al ;
  BOOL  hosted ;
#ifdef KEY /* Bug 10177 */
  TY_IDX ty_idx = 0;
#else /* KEY Bug 10177 */
  TY_IDX ty_idx;
#endif /* KEY Bug 10177 */

  hosted = test_flag(flag_matrix,FEI_DESCRIPTOR_HOSTED_TYPE) || in_hosted_dtype ;

  switch(table_type) {
  case Basic:
    al = bit_to_byte(size);      
    ty_idx = cwh_types_mk_basic_TY((BASIC_TYPE)basic_type,size,al) ; 
    break;

  case Array:
    Is_True((top_of_decl_bounds != ANULL),("Bad array info"));
    ty_idx = cwh_types_mk_array_TY(decl_bounds,
				   top_of_decl_bounds + 1,
				   ty_dim1,
				   bit_to_byte(last_bitsize));
    if (hosted)
      (void) cwh_types_mk_pointer_TY(ty_idx,TRUE);

    /* now generate a list of distribute pragmas, dont associate with
       the ST yet */
    if (decl_distributed_pragma_id!=WN_PRAGMA_UNDEFINED) {
      int i;
      WN *wn;
      decl_distribute_pragmas=WN_CreateBlock();
      for(i=top_of_decl_bounds; i>=0; i--) {
	  /* from last to first */
	/* create a DISTRIBUTE or DISTRIBUTE_RESHAPE pragma for the dimension */
	/* use ST==NULL for now, we fill in this later */
	WN *lb,*ub,*st;
	wn = WN_CreatePragma(decl_distributed_pragma_id, (ST_IDX) NULL, 0, 0);
	WN_pragma_distr_type(wn) =decl_distribution[i];
	WN_pragma_index(wn) = top_of_decl_bounds-i;
	switch(decl_distribution[i]) {
	  case DISTRIBUTE_CYCLIC_EXPR:
	    WN_INSERT_BlockLast(decl_distribute_pragmas,wn);
	    /* create an Xpragma with the value */
	    wn = WN_CreateXpragma(decl_distributed_pragma_id, (ST_IDX) NULL, 1);
	    WN_kid0(wn) = decl_cyclic_val[i].wn;
	    WN_INSERT_BlockLast(decl_distribute_pragmas,wn);
	    break;
	  case DISTRIBUTE_CYCLIC_CONST:
	    WN_pragma_preg(wn) = decl_cyclic_val[i].val;
	    WN_INSERT_BlockLast(decl_distribute_pragmas,wn);
	    break;
	  default:
	    WN_INSERT_BlockLast(decl_distribute_pragmas,wn);
	    break;
	}
	/* generate an Xpragma for the array bound value for this dimension */
	lb = cwh_types_bound_WN(ty_idx,i,LOW);
	ub = cwh_types_bound_WN(ty_idx,i,UPPER);
	st = WN_Intconst(MTYPE_I4,1);
	wn = WN_CreateXpragma(decl_distributed_pragma_id, (ST_IDX) NULL, 1);
	WN_kid0(wn) = cwh_addr_extent(lb,ub,st);
	WN_INSERT_BlockLast(decl_distribute_pragmas,wn);
      } /* for each extent */
      /* now do a second pass if we have an ONTO clause */
      if (distribute_onto) {
        for(i=top_of_decl_bounds; i>=0; i--) {
	  /* ONTO clause args only apply to each non-* distribution */
	  if (decl_distribution[i]!=DISTRIBUTE_STAR) {
	    /* make an Xpragma */
	    wn = WN_CreateXpragma(WN_PRAGMA_ONTO, (ST_IDX) NULL, 1);
	    WN_kid0(wn) = decl_onto[i];
	    WN_INSERT_BlockLast(decl_distribute_pragmas,wn);
	  }
	} /* for */
      }
    }
    top_of_decl_bounds = ANULL ;
    break ;

  case Func_tion:                  /* external, passed as arg */
    ty_idx = cwh_types_mk_procedure_TY(Be_Type_Tbl(MTYPE_V), 0,TRUE,FALSE);
    break ;

  default:

    DevWarn((" Unsupported type "));
  } 

  t.table_type = (TABLE_TYPE)table_type ;
  t.basic_type = (BASIC_TYPE)basic_type ;

  cwh_types_fill_type(flag_matrix,&t,ty_idx);

  return(t);
}  

/*===================================================
 *
 * fei_array_dimen
 *
 * This is the PDGCS call to make a new array
 * dimension. The dimensions are processed in
 * order 1->rank of array, but the rank isn't known
 * here, so put each bound in decl_bounds[dim], and
 * create the TY in fei_descriptor.
 * 
 * If the bound isn't a constant, the FE puts it
 * into a temp, so the temp just has to be addressed.
 *
 * The result is returned but ignored. Save the 
 * TY associated with dim=1, as it's needed for
 * TY_etype of the TY_ARI.
 *
 ====================================================
 */
extern INTPTR
fei_array_dimen(INT32  flag_bits,
		INT64  low_bound,
		INT64  extent,
		INT32  axis,
		TYPE   span_type,
		INT64  bitsize,
		INT    distribution,
                INT64  upper_bound)
{
  ST * st;
  STB_pkt *b;
  WN  *wn ;
  BOOL hosted ;
  ST_IDX st_idx;
  ARB_HANDLE p;
  BOOL flow_dependent;

  hosted = test_flag(flag_bits,FEI_ARRAY_DIMEN_HOSTED_TYPE) || in_hosted_dtype ;

  top_of_decl_bounds = axis - 1 ;
  if (top_of_decl_bounds == 0) {
     decl_bounds = New_ARB();
     p = decl_bounds;
  } else {
     p = New_ARB();
  }

  flow_dependent = test_flag(flag_bits,FEI_ARRAY_DIMEN_FLOW_DEPENDENT);

  ARB_Init (p, 1, 1, 1);

  if (test_flag(flag_bits,FEI_ARRAY_DIMEN_VARY_LB)) {

    b = cast_to_STB((UINTPS) low_bound) ;
    Is_True((b->form == is_ST),("Odd lbound"));

    st = cast_to_ST(b->item);
    Clear_ARB_const_lbnd(p);
    Set_ARB_lbnd_var(p, ST_st_idx(st));

    if (!hosted && !flow_dependent)
	cwh_types_copyin_pragma(st);

  } else {

    Set_ARB_const_lbnd(p);
    Set_ARB_lbnd_val (p, low_bound);
  }
  
  if (test_flag(flag_bits,FEI_ARRAY_DIMEN_VARY_UB)) {

    b = cast_to_STB((UINTPS) upper_bound) ;
    if (b != NULL) {
      Is_True((b->form == is_ST),("Odd extent"));
      
      st = cast_to_ST(b->item);

      Clear_ARB_const_ubnd(p);
      Set_ARB_ubnd_var(p, ST_st_idx(st));

      if (!hosted && !flow_dependent)
  	  cwh_types_copyin_pragma(st);

    } else {

      Set_ARB_const_ubnd(p);
      Set_ARB_ubnd_val (p, 0);
    }

  } else {  /* constant ub */
    
    Set_ARB_const_ubnd(p);
    Set_ARB_ubnd_val (p, upper_bound);
  }

  /* set pragma on extent, for MP/LNO, doesn't go into ARB */

  if (test_flag(flag_bits,FEI_ARRAY_DIMEN_VARY_EXT)) {

    b = cast_to_STB((UINTPS) extent) ;
    if (b != NULL) {
      Is_True((b->form == is_ST),("Odd extent"));
      
      st = cast_to_ST(b->item);

      if (!hosted && !flow_dependent)
	cwh_types_copyin_pragma(st);
    }
  }

  /* update stride - the argument is the bitsize of the */
  /* current axis, but a TY has the size of an element  */
  /* so save the bitsize till next dimension. If stride */
  /* isn't constant bitsize becomes 0, and the TY tree  */
  /* seems to require the element size */
  
  if (axis == 1) {
     
     ty_dim1 = cast_to_TY(t_TY(span_type)) ;
     
     Set_ARB_const_stride(p);
     Set_ARB_stride_val(p, TY_size(Ty_Table[ty_dim1]));
     
  } else { 
     ARB_HANDLE q = p[-1];
     if (ARB_const_ubnd(p) && 
	 ARB_const_lbnd(p) && 
	 ARB_const_stride(q)) {
	
	Set_ARB_const_stride(p);
	Set_ARB_stride_val(p, bit_to_byte(last_bitsize));
	
     } else {
	
	Set_ARB_const_stride(p);
	Set_ARB_stride_val(p, ARB_stride_val(decl_bounds[0]));
     }
  }
  
  last_bitsize = bitsize ;

  if (axis == 1) {     /* initialize */

    distribute_onto=FALSE;
    decl_distributed_pragma_id=WN_PRAGMA_UNDEFINED;
    decl_distribute_pragmas =NULL;
  }

  if (test_flag(flag_bits,FEI_ARRAY_DIMEN_ONTO_EXPR)) {
    distribute_onto=TRUE;
    /* get the WN for the constant */
    wn = cwh_expr_operand(NULL);
    Is_True( (WN_operator(wn)==OPR_INTCONST),("ONTO: expected integer constant"));
    Is_True( (distribution!=Star_Dist),("ONTO: unexpected for * distribution"));
    decl_onto[top_of_decl_bounds]=wn;
  }

  /* if this array is distributed, save the distribution information */
  switch(distribution) {
    case Block_Dist:
	decl_distribution[top_of_decl_bounds] = DISTRIBUTE_BLOCK;
	break;
    case Star_Dist:
	decl_distribution[top_of_decl_bounds]=DISTRIBUTE_STAR;
	break;
    case Cyclic_Dist:
	if (test_flag(flag_bits,FEI_ARRAY_DIMEN_DIST_EXPR)) {
	  /* get the WN for the constant */
	  wn = cwh_expr_operand(NULL);
	  if(WN_operator(wn)==OPR_INTCONST) {
	    decl_cyclic_val[top_of_decl_bounds].val=WN_const_val(wn);
	    decl_distribution[top_of_decl_bounds]=DISTRIBUTE_CYCLIC_CONST;
	  } else {
	    /* this is a expression */
	    decl_cyclic_val[top_of_decl_bounds].wn=wn;
	    decl_distribution[top_of_decl_bounds]=DISTRIBUTE_CYCLIC_EXPR;
	  } 
	} else {
	  /* cyclic by itself is same as cyclic(1) */
	  decl_cyclic_val[top_of_decl_bounds].val=1;
	  decl_distribution[top_of_decl_bounds]=DISTRIBUTE_CYCLIC_CONST;
	}
	break;
  }
  
  if (distribution != No_Dist) {
    decl_distributed_pragma_id=test_flag(flag_bits,FEI_ARRAY_DIMEN_DIST_RESHAPE)?WN_PRAGMA_DISTRIBUTE_RESHAPE:WN_PRAGMA_DISTRIBUTE;
  }

  return 0;
}

/*===================================================
 *
 * fei_next_type_idx
 *
 * get a new TY to hand to fei_user_type   
 *
 ====================================================
*/
extern INT32
fei_next_type_idx(INT32 flag, INT32 align)
{
  TY_IDX ty_idx;

  if (!cwh_types_in_dtype())
    in_hosted_dtype =  test_flag(flag,FEI_NEXT_TYPE_IDX_HOSTED_TYPE);

  ty_idx = cwh_types_new_TY(in_hosted_dtype,
			    bit_to_byte(align)) ;

  BUMP_TY_COUNTER(c_TY_DTYPE);

  return(cast_to_int(ty_idx));
}

/*===================================================
 *
 * fei_user_type
 *
 * The definition of a new derived type. Create 
 * the STRUCT type, and add components with 
 * fei_new_member - the TY to fill is cr_ty_idx.
 *
 * This may be a derived type component of a
 * derived type, so a stack preserves the
 * parent derived type and makes the new
 * one current for fei_member.
 *
 * The alignments and offsets are provided by
 * fei_member, so make a default alignment here
 * and patch it up in fei_member.
 *
 ====================================================
*/
/*ARGSUSED*/
void
fei_user_type(char         *name_string,
	      INT32         nbr_components,
	      INT32         first_idx,
	      INT64         size,
	      INT32         sequence_arg,
	      INTPTR         cr_ty_idx,
	      INT32         align)
    
{
  TY_IDX ty_idx    ;
  dtype_t  d ;
#ifdef KEY /* Bug 10177 */
  memset(&d, 0, sizeof d);
#endif /* KEY Bug 10177 */
  FORT_SEQUENCE sequence;
  INT32 i;

  sequence = (FORT_SEQUENCE) sequence_arg;

  ty_idx = cast_to_TY(cr_ty_idx);

  TY& ty = Ty_Table[ty_idx];

  TY_Init (ty, bit_to_byte(size), KIND_STRUCT, MTYPE_M, Save_Str(name_string));

  for (i=0; i<nbr_components; i++) {
     FLD_HANDLE fld = New_FLD ();
     if (i == 0) {
	Set_TY_fld(ty, fld);
	d.dty_last = fld.Idx ();
     }
  }
	
  /* this acts as a flag that stride_multipliers in 
   * dope vectors are in bytes, not words.
   */
  if (sequence ==  Seq_Char) {
     Set_TY_is_packed(ty);
  }

  d.dty = ty_idx ;
  d.ncompos  = nbr_components ;
  d.seq      = (sequence != Seq_None);
  d.hosted   = in_hosted_dtype ;

  cwh_types_push_dtype(d);

}

/*===================================================
 *
 * fei_member
 *
 * Add the definition of a new component to the 
 * current derived type. If it's the last
 * component, enter the TY, otherwise push it
 * back on the stack
 *
 ====================================================
*/
/*ARGSUSED*/
INT32
fei_member(char          *name_string,
	   TYPE           member_type,
	   INT64          offset,
	   INT64          size,
 	   INT32          alignment,
	   INT32          lineno,
	   INT64          flag_bits,
	   INT64          io_code)
{
  dtype_t d ;
  TY_IDX ty_idx;
  BOOL p1   ;
  INT64  off;
  INT32 ret_val;

  ty_idx = cast_to_TY(t_TY(member_type));
  p1 = test_flag(flag_bits, FEI_OBJECT_DV_IS_PTR);

  /* does offset imply component is misaligned? */

  off = bit_to_byte(offset);

  Is_True((off%TY_align(ty_idx) == 0), ("Misalign"));

  if (p1) {
    Is_True(TY_is_f90_pointer(Ty_Table[ty_idx]),(" Missing f90ptr"));
  } else {
    Is_True(!TY_is_f90_pointer(Ty_Table[ty_idx]),(" extra f90ptr"));
  }

  d = cwh_types_pop_dtype();

  /* is enclosing derived type on more stringent alignment? */

  Is_True((TY_align(d.dty) >= TY_align(ty_idx)), ("Misalign, enclosing"));

  FLD_HANDLE fld (d.dty_last);

  FLD_Init (fld, Save_Str(name_string), ty_idx, off);

  ret_val = d.dty_last;

  d.dty_last++;

  if (--d.ncompos == 0) {
  
    Set_FLD_last_field(fld);
//    d.dty = cwh_types_unique_TY(d.dty);

    if (!cwh_types_in_dtype())
      in_hosted_dtype = FALSE ;

  } else 
    cwh_types_push_dtype(d);

  return (ret_val);
}

/*===================================================
 *
 * fei_dope_vector
 *
 * Build a dope vector TY for an array of the given
 * rank and scalar type.
 *
 ====================================================
 */
extern TYPE 
fei_dope_vector(INT32  num_dims,TYPE base_type, INT32 flag,
#ifdef KEY /* Bug 6845 */
  INT32 n_allocatable_cpnt
#endif /* KEY Bug 6845 */
)
{                                     
  TY_IDX ty_idx   ;
  TY_IDX ts_idx   ;
  TYPE  t    ;
  BOOL  b    ;
  
  ts_idx = cast_to_TY(t_TY(base_type));
  b  = test_flag(flag,FEI_DOPE_VECTOR_HOSTED_TYPE) || in_hosted_dtype;
  ty_idx = cwh_types_dope_TY(num_dims,ts_idx,b,test_flag(flag,FEI_DOPE_VECTOR_POINTER),
#ifdef KEY /* Bug 6845 */
    n_allocatable_cpnt
#endif /* KEY Bug 6845 */
  ) ;

  t.table_type = Basic ;
  t.basic_type = S_tructure ;

  cwh_types_fill_type(0,&t,ty_idx);

  return(t);
}

/*===================================================
 *
 * cwh_types_mk_basic_TY
 *
 * Given a PDGCS basic type, return a TY.
 *
 * Integer, real and complex variants are predefined
 * TYs, if they are aligned on a natural boundary, 
 * but logicals and characters aren't. The size
 * of a character type is a WN * which describes 
 * the len= type parameter. May be temp or constant.
 *
 * Basic types are entered in the global symbol 
 * table, except structures, which are entered
 * when all components have been seen (fei_member)
 *
 ====================================================
*/

static TY_IDX
cwh_types_mk_basic_TY (BASIC_TYPE    basic_type,
		       INTPTR        size,
		       mUINT16       alignment)
{
  TY_IDX    ty_idx ;
  TYPE_ID   bt ;
  STB_pkt * p ;
  WN      * wn;
  static TY_IDX char_ptr_ty_idx = 0 ;

  ty_idx = 0 ;

  switch(basic_type) {

  case L_ogical:
    ty_idx = cwh_types_mk_logical_TY(size,alignment);
    break ;

  case Char_Fortran:
    p = cast_to_STB(size);

    switch (p->form) {
    case is_WN:
      wn = cast_to_WN(p->item);
      if (WNOPR(wn) == OPR_INTCONST)
	wn = bit_to_byte_WN(wn);
      ty_idx = cwh_types_mk_character_TY(wn,NULL,TRUE);
      break;

    case is_ST:
      ty_idx = cwh_types_mk_character_TY(NULL,cast_to_ST(p->item),FALSE);
      break;

    default:
      Is_True((0),("odd TY const"));
    }
    break ;

  case C_omplex:
    bt = Mtypes[align_index(size/2)][basic_index(basic_type)]; 	 
    ty_idx = Be_Type_Tbl(bt);
    ty_idx = cwh_types_mk_misaligned_TY(ty_idx,alignment) ;
    break ;

  case S_tructure:
    ty_idx = cast_to_TY(size);
    break ;

  case CRI_Pointer_Char:

     /* We will make this a struct with two fields for now */

    if (char_ptr_ty_idx == 0 ) {

      FLD_HANDLE list = cwh_types_fld_util("base", Be_Type_Tbl(Pointer_Mtype),
					   0,TRUE);
      FLD_HANDLE fld =  cwh_types_fld_util("len", Be_Type_Tbl(Pointer_Mtype),
					   Pointer_Size,TRUE);
      Set_FLD_last_field(fld);
      char_ptr_ty_idx = cwh_types_mk_struct(2*Pointer_Size, Pointer_Size, list,
					    ".char_pointer");
    }
    
    ty_idx = char_ptr_ty_idx ;
    break;


  case CRI_Pointer:
    ty_idx = Be_Type_Tbl(Pointer_Mtype);
    break ;

  case T_ypeless: 

    /* if large, Make a 1d array TY of unsigned bytes - make it global     */
    /* because it's probably for a pattern con which requires a global TY  */
    /* as the constant is inserted into Strtab                             */
    
    if (size == 8) {
       ty_idx = Be_Type_Tbl(MTYPE_U1);
    } else if (size==16) {
       ty_idx = Be_Type_Tbl(MTYPE_U2);
    } else if (size==32) {
       ty_idx = Be_Type_Tbl(MTYPE_U4);
    } else if (size==64) {
       ty_idx = Be_Type_Tbl(MTYPE_U8);
    } else {
      
      ty_idx = cwh_types_array_util(1,Be_Type_Tbl(MTYPE_U1),1,bit_to_byte(size),".typeless.",TRUE);
     
      ARB_HANDLE arb = TY_arb(ty_idx);
      Set_ARB_stride_val(arb, 1);
      Set_ARB_ubnd_val(arb, bit_to_byte(size) - 1);
      
      ty_idx = cwh_types_unique_TY(ty_idx);
    }
    break ;

  default:
    bt = Mtypes[align_index(size)][basic_index(basic_type)]; 	 
    ty_idx = Be_Type_Tbl(bt);
    ty_idx = cwh_types_mk_misaligned_TY(ty_idx,alignment) ;
    break;
  }
  
  return(ty_idx);
}

/*===================================================
 *
 * cwh_types_mk_misaligned_TY
 *
 * If the alignment of the TY isn't small enough
 * a copy of the TY with the correct alignment will
 * be returned, otherwise the original TY.
 * 
 * alignment is # of bytes 1 - byte
 *                         2 - 16 bit
 *                         4 - 32 bit
 *                         8 - 64 bit
 *
 * and the 64 bit one is ignored. Works only for
 * basic types.
 *
 ====================================================
*/
static TY_IDX
cwh_types_mk_misaligned_TY(TY_IDX ty_idx, mUINT16 alignment)
{
  TY_IDX tc_idx = ty_idx ;
  TY& ty = Ty_Table[ty_idx];
  
  if (TY_kind(ty) == KIND_SCALAR) {
    if (alignment <= 4) {
      if (alignment > 0) {
	if (TY_align(ty_idx) > alignment ) {

	  tc_idx = unaligned_type [TY_mtype(ty)][alignment_to_align(alignment)];

	  if (tc_idx == 0) {

	    BUMP_TY_COUNTER(c_TY_MISC);
	    tc_idx = cwh_types_new_TY ( TRUE , alignment);
	    TY& tc = Ty_Table[tc_idx];

	    TY_Init (tc, TY_size(ty), TY_kind(ty), TY_mtype(ty), Save_Str2(TY_name(ty),alstr[alignment_to_align(alignment)])); 

	    Set_TY_flags(tc, TY_flags(ty));

            tc_idx = cwh_types_unique_TY(tc_idx);

	    unaligned_type [TY_mtype(ty)][alignment_to_align(alignment)] = tc_idx ;
	  }
	}
      }
    }
  }
  return tc_idx ;
}

/*===================================================
 *
 * cwh_types_form_misaligned_TY
 *
 * Given a TY and an alignment which is smaller
 * than its natural alignment, copy the TY but
 * enforce the alignment provided.
 * 
 * alignment is # of bytes ie: 1-8.
 *
 * In general derived types should have the correct
 * alignment set up in fei_member. If a dummy has to 
 * be misaligned though, it may be a derived type that
 * was aligned to a large value. eg: (F8,F8) Recursive
 * types can only arise via dope vectors, which are 
 * assumed to be correctly aligned.
 *
 * The assumption is that the TY is in the 
 * Current_Symtab, if not, then a new hosted 
 * flag is required. 
 *
 ====================================================
*/
extern TY_IDX
cwh_types_form_misaligned_TY(TY_IDX ty_idx, mUINT16 alignment)
{
  TY_IDX  tr_idx ;
  TY_IDX  tt_idx ;
  INT num ;
  const char *  const misstr = ".mis";

  TY& ty = Ty_Table[ty_idx];

  if (TY_align(ty_idx) <= alignment)
    return ty_idx ;

  switch(TY_kind(ty)) {
  case KIND_SCALAR:
    if (TY_is_logical(ty))
      tr_idx = cwh_types_mk_logical_TY(byte_to_bit(TY_size(ty)),alignment) ;
    else 
      tr_idx = cwh_types_mk_misaligned_TY(ty_idx,alignment) ;
    break ;

  case KIND_ARRAY:
   {
    tt_idx = cwh_types_form_misaligned_TY(TY_etype(ty),alignment);
    tr_idx = Copy_TY(ty_idx);  /* copies whole TY ***  */
    TY &tr = Ty_Table[tr_idx];
    Set_TY_etype(tr, tt_idx);
    Set_TY_align(tr_idx, alignment);
    Set_TY_name_idx(tr, Save_Str2(TY_name(tr),misstr));
    break;
   }
  case KIND_STRUCT:
    if (cwh_types_is_dope(ty_idx)) {
      tr_idx = ty_idx ;

    } else {
      FLD_ITER fld_iter = Make_fld_iter (TY_fld (ty));
      FLD_HANDLE c_fld;
      do {
	FLD_HANDLE p (fld_iter);
	FLD_HANDLE fld = New_FLD ();
	if (p == TY_fld (ty))
	    c_fld = fld;
	 FLD_Init (fld, 
		   Save_Str2(FLD_name(p),misstr), 
		   cwh_types_form_misaligned_TY(FLD_type(p), alignment),
		   FLD_ofst(p));
         Set_FLD_bofst(fld, FLD_bofst(p));
	 Set_FLD_bsize(fld, FLD_bsize(p));
	 Set_FLD_flags(fld, FLD_flags(p));
      } while (!FLD_last_field (fld_iter++));

      tr_idx = Copy_TY(ty_idx);
      TY &tr = Ty_Table[tr_idx];
      Set_TY_align(tr_idx, alignment);
      Set_TY_fld(tr, c_fld);

      Set_TY_name_idx(tr, Save_Str2(TY_name(ty),misstr));
    }
    break;
    

  case KIND_POINTER:
    tr_idx = ty_idx ;
    break ;

  default:
    Is_True((0),("Odd misalignment"));
    
  }
  
  return tr_idx;
}

/*===================================================
 *
 * cwh_types_mk_procedure_TY
 *
 * Given a TY for a return type, create a corresponding 
 * TY for an external procedure. Dummy arguments are 
 * not set up here, but an FTI of the correct size is
 * created. fei_object adds the dummies.Later : we omit
 * the TY parm list, as it doesn't seem to be needed.
 * Finally, we make sure we build hosted procedure types
 * in the right symbol table.
 *
 ====================================================
*/
extern TY_IDX 
cwh_types_mk_procedure_TY (TY_IDX ret_typ_idx, INT32 nparms, BOOL global, BOOL host)
{
  TY_IDX ty_idx ;
  TYLIST tylist_idx;

  static TY_IDX basic_subroutine_TY_idx = 0 ;
  TY &ret_typ = Ty_Table[ret_typ_idx];
  
  if ( nparms == 0 )
    if (MTYPE_is_void(TY_mtype(ret_typ)))  
      if (basic_subroutine_TY_idx != 0) 
	return (basic_subroutine_TY_idx) ;
      else
	global = TRUE;

  BUMP_TY_COUNTER(c_TY_PROC) ;
  ty_idx  = cwh_types_new_TY (global,1) ;
  TY &ty = Ty_Table[ty_idx];

  TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(cwh_types_mk_anon_name(".proc.")));

  /* If nparms is > 0, we set the ret_type, but may change it
   * later for structs by value (don't have context yet. The 
   * TY_list is established in fei_proc_body. Hence don't match
   * TYs yet. (special case is void types, changed to structs).
   */

  if (nparms == 0) {
    (void) New_TYLIST (tylist_idx);
    Set_TY_tylist(ty, tylist_idx);
    Tylist_Table [tylist_idx] = ret_typ_idx;
    (void) New_TYLIST (tylist_idx);
    Tylist_Table [tylist_idx] = 0;
  }

  if (nparms == 0)
    if (MTYPE_is_void(TY_mtype(ret_typ)))
      basic_subroutine_TY_idx = ty_idx ;
  
  return (ty_idx);
  
}

/*===================================================
 *
 * cwh_types_mk_array_TY
 *
 * Create an array TY. The ARB information 
 * has been setup in the argument bounds,
 * the element TY in base, and the size of
 * the array in size. 
 * 
 * Allocate the TY and ARI nodes, copy & return.
 *
 * Ignore any first/last dimension stuff in the
 * bounds array.
 *
 * Note that the bounds pointed at by the ARB_HANDLE are in
 * Fortran order, and need to be reversed to C order
 *
 ====================================================
*/
static TY_IDX
cwh_types_mk_array_TY(ARB_HANDLE bounds,INT16 n,TY_IDX base_idx, INT64 size)
{
  TY_IDX ty_idx   ;
  BOOL const_str = TRUE; 
  int     i;


  ty_idx = cwh_types_array_util(n,base_idx,TY_align(base_idx),0,".array.",FALSE);
  Set_TY_arb(ty_idx,bounds);


  // Step 1, reverse the bounds
  for (i = 0; i < n/2; i++) {
     ARB_swap(bounds[i],bounds[n-i-1]);
  }

  // Step 2, set the first, last and dimension bits
  for (i = 0; i < n ; i++) {
     Clear_ARB_first_dimen(bounds[i]);
     Clear_ARB_last_dimen(bounds[i]);
     Set_ARB_dimension(bounds[i],n-i);
     const_str = const_str && ARB_const_stride(bounds[i]);
  }
  Set_ARB_first_dimen(bounds[0]);
  Set_ARB_last_dimen(bounds[n-1]);

  if ( const_str ) {
    
    Set_TY_size(ty_idx, size);

  } else {

    Set_TY_size(ty_idx, 0);

  }

  ty_idx = cwh_types_unique_TY(ty_idx);

  return (ty_idx);
}

/*===================================================
 *
 * cwh_types_mk_logical_TY
 *
 * Make a logical TY - use a single global one
 * for each kind (size) and alignment.
 *
 ====================================================
*/
extern TY_IDX
cwh_types_mk_logical_TY(INT32 size, mUINT16 alignment)
{

  TYPE_ID  bt ;  
  TY_IDX   ty_idx ;
  INT16    i  ;
  const char   * csz;
  const char   * aln;
  INT32 size_in_bytes;

  i = align_index(size) ;
  
  Is_True((i < NUM_LOG_KINDS),("Odd logical type")) ;
  
  if (basic_logical_ty[i][alignment_to_align(alignment)] == 0) {

    csz = logstr[i];
    aln = "";

    bt  = Mtypes[align_index(size)][basic_index(L_ogical)]; 	 
    ty_idx  = cwh_types_new_TY (TRUE,alignment) ; 
    
    BUMP_TY_COUNTER(c_TY_MISC);

    size_in_bytes = bit_to_byte(size);

    if (size_in_bytes != alignment)
	aln = alstr[alignment_to_align(alignment)];

    TY &ty = Ty_Table[ty_idx];
    
    TY_Init (ty, size_in_bytes, KIND_SCALAR, bt, Save_Str2(csz,aln));

    Set_TY_is_logical(ty);

    ty_idx = cwh_types_unique_TY(ty_idx);

    basic_logical_ty[i][alignment_to_align(alignment)] = ty_idx ;

  }
  return (basic_logical_ty[i][alignment_to_align(alignment)]);
}

/*===================================================
 *
 * cwh_types_mk_character_TY
 *
 * Make a character TY. The TY should be an array
 * of sz bytes, but the char*1 TY may have to be 
 * made first. The sz is in bytes. It may be an ST 
 * or a WN, with a flag to distinguish.
 *
 ====================================================
*/
TY_IDX
cwh_types_mk_character_TY(WN *sz_wn, ST *sz_st, BOOL sz_is_wn)
{
  INT64 i    ;
  TY_IDX ty_idx   ;
  BOOL global;
  BOOL const_sz;

  static TY_IDX basic_character_ty_idx = 0;

  if (basic_character_ty_idx == 0) {

    BUMP_TY_COUNTER(c_TY_MISC)  ;

    ty_idx = cwh_types_new_TY (TRUE,1) ; 
    TY &ty = Ty_Table[ty_idx];

    TY_Init (ty, 1, KIND_SCALAR, MTYPE_U1, Save_Str(".character."));
    Set_TY_is_character(ty);

    ty_idx = cwh_types_unique_TY(ty_idx);

    basic_character_ty_idx = ty_idx ;
  }
   
  ty_idx = cwh_types_array_util(1,basic_character_ty_idx,1,0,".ch_str.",TRUE);
  TY& ty = Ty_Table[ty_idx];

  ARB_HANDLE arb = TY_arb(ty);

  Set_ARB_lbnd_val(arb, 1);
  Set_ARB_stride_val(arb, 1);

  /* could be an ST, or WN constant or WN expression */

  if (!sz_is_wn) {

    Clear_ARB_const_ubnd(arb);
    Set_TY_size(ty, 0);
    Set_ARB_ubnd_var(arb,ST_st_idx(sz_st));

  } else if (WNOPR(sz_wn) == OPR_INTCONST) {

    i = WN_const_val(sz_wn) ;
    Set_ARB_ubnd_val(arb,i) ;
    Set_TY_size(ty, i);

  } else {  /* expression into temp, & temp into ARB */

      ST *st = cwh_types_make_bounds_ST();

      Clear_ARB_const_ubnd(arb);
      Set_TY_size(ty, 0);
      cwh_addr_store_ST(st,0,0,sz_wn);
      Set_ARB_ubnd_var(arb, ST_st_idx(st));
  }

  Set_TY_is_character(ty);
  ty_idx = cwh_types_unique_TY(ty_idx);
  return(ty_idx);
}


/*===================================================
 *
 * cwh_types_scalar_TY
 *
 * Given a TY, find its scalar ty, ie: the bottom
 * of any KIND_ARRAYs
 *
 ====================================================
*/
extern TY_IDX
cwh_types_scalar_TY(TY_IDX ty_idx)
{
#ifdef KEY /* Bug 10177 */
  TY_IDX rty_idx = 0;
#else /* KEY Bug 10177 */
  TY_IDX rty_idx ;
#endif /* KEY Bug 10177 */
 
  TY& ty = Ty_Table[ty_idx];

  switch(TY_kind(ty)) {

  case KIND_VOID:  
  case KIND_SCALAR:  
  case KIND_STRUCT:  
  case KIND_POINTER:  
  case KIND_FUNCTION:  
    rty_idx = ty_idx;
    break;

  case KIND_ARRAY:
    rty_idx = cwh_types_scalar_TY(TY_etype(ty)) ;
    break;

  default:
    DUMP_TY(ty_idx);
    Is_True((0),("Odd ty"));
    break;
  }

  return(rty_idx);
}

/*===================================================
 *
 * cwh_types_array_TY
 *
 * Given a TY, find its array ty, ie: the bottom
 * of any KIND_POINTERS to a KIND_ARRAY etc...
 * Given a scalar, just hand it back..
 *
 ====================================================
*/
extern TY_IDX
cwh_types_array_TY(TY_IDX ty_idx)
{
#ifdef KEY /* Bug 10177 */
  TY_IDX rty_idx = 0;
#else /* KEY Bug 10177 */
  TY_IDX rty_idx ;
#endif /* KEY Bug 10177 */

  TY& ty = Ty_Table[ty_idx];

  switch(TY_kind(ty)) {
  case KIND_ARRAY:
  case KIND_SCALAR:
  case KIND_STRUCT:
  case KIND_FUNCTION:
  case KIND_VOID:  
    rty_idx = ty_idx;
    break;

  case KIND_POINTER:  
    rty_idx = cwh_types_array_TY(TY_pointed(ty)) ;
    break;

  default:
    DUMP_TY(ty_idx);
    Is_True((0),("Odd array ty"));
    break;
  }

  return(rty_idx);
}

/*===================================================
 *
 * cwh_types_WN_TY
 *
 * Given a WN, find the TY of what it addresses. Not
 * general - used in figuring out TYs when building
 * addresses. If flag addr is TRUE, the pointer TY
 * for a load is passed back rather than deref'd.
 * eg: for a PARM node the TY of an LDA can be
 * plucked off as is.
 * 
 * Some logical operators would yield an integer 
 * TY (no TY_is_logical flag) without the special 
 * checking here.
 *
 ====================================================
*/

extern TY_IDX
cwh_types_WN_TY(WN * wn, BOOL addr)
{
  TY_IDX ty_idx = 0 ;
  WN *kid;
  INT i;

  switch (WNOPR(wn)) {
  case OPR_ARRAY:
  case OPR_ARRSECTION:
  case OPR_ARRAYEXP:
  case OPR_MLOAD:
  case OPR_PARM:
    ty_idx = cwh_types_WN_TY(WN_kid0(wn),addr);
    break ;

  case OPR_INTCONST:
    if (addr) {
      ty_idx = Make_Pointer_Type(Be_Type_Tbl(MTYPE_V));
    } else {
      ty_idx = Be_Type_Tbl(WN_rtype(wn));
    }
    break;

  case OPR_INTRINSIC_OP:
    /* Special case so that we handle character transformationals correctly */

    if (MTYPE_is_pointer(WN_rtype(wn)) || WN_opcode(wn) == OPC_MINTRINSIC_OP) {
       ty_idx = cwh_types_WN_TY(WN_kid0(wn),addr);
    } else {
       ty_idx = Be_Type_Tbl(WN_rtype(wn));
    }
    break;
    
  case OPR_LDA:
  case OPR_ILOAD:
  case OPR_LDID:
   {
    ty_idx = WN_ty(wn) ;
    TY &ty = Ty_Table[ty_idx];

    if (! addr) 
      if (TY_kind(ty) == KIND_POINTER)
	ty_idx = TY_pointed(ty);
   }
   break;

  case OPR_CIOR:
  case OPR_CAND:
  case OPR_LIOR:
  case OPR_LAND:
  case OPR_LNOT:
  case OPR_EQ:
  case OPR_NE:
    ty_idx = cwh_types_WN_TY(WN_kid0(wn),addr);
    break;

  /* Special case for ADD */
  case OPR_ADD:
  case OPR_SUB:
    for (i=0; i <= 1; i++) {
       kid = WN_kid(wn,i);
       switch (WNOPR(kid)) {
	case OPR_ARRAY:
	case OPR_ARRSECTION:
	case OPR_ARRAYEXP:
	case OPR_LDA:
	case OPR_LDID:
	case OPR_ILOAD:
	  ty_idx = cwh_types_WN_TY(kid,addr);
	  return (ty_idx);
       }
    }
    /* Fall through */

  default:
    Is_True((OPCODE_is_expression(WN_opcode(wn))),(" Unexpected WN"));

    ty_idx = Be_Type_Tbl(WN_rtype(wn));
    break;
  }

  return (ty_idx) ;
}

/*===================================================
 *
 * cwh_types_ch_parm_ty
 *
 * Make a pointer to a character TY of the 
 * appropriate substring size - 
 *
 ====================================================
*/
extern TY_IDX
cwh_types_ch_parm_TY(WN *ln)
{
  TY_IDX ty_idx ;

  ty_idx = cwh_types_mk_character_TY(ln,NULL,TRUE);
  ty_idx = Make_Pointer_Type( ty_idx);
  
  return(ty_idx);
}

/*===================================================
 *
 * cwh_types_is_character
 *
 * return T if this is a character TY
 *
 ====================================================
*/
extern BOOL
cwh_types_is_character(TY_IDX ty_idx)
{
  TY_IDX ts_idx ;

  ts_idx = cwh_types_array_TY(ty_idx);
  ts_idx = cwh_types_scalar_TY(ts_idx);

  TY& ts = Ty_Table[ts_idx];

  return (TY_is_character(ts));
}

/*===================================================
 *
 * cwh_types_is_logical
 *
 * return T if this is a logical TY
 *
 ====================================================
*/
extern BOOL
cwh_types_is_logical(TY_IDX ty_idx)
{
  TY_IDX ts_idx ;

  ts_idx = cwh_types_array_TY(ty_idx);
  ts_idx = cwh_types_scalar_TY(ts_idx);

  TY& ts = Ty_Table[ts_idx];

  return (TY_is_logical(ts));
}

/*===================================================
 *
 * cwh_types_is_character_function
 *
 * return T if this is a character function TY
 *
 ====================================================
*/
extern BOOL
cwh_types_is_character_function(TY_IDX ty_idx)
{
  TY_IDX ts_idx ;

  ts_idx = cwh_types_array_TY(ty_idx);
  ts_idx = cwh_types_scalar_TY(ts_idx);

  TY& ts = Ty_Table[ts_idx];

  if (TY_kind(ts) != KIND_FUNCTION) return (FALSE);

  ts_idx = Tylist_Table[TY_tylist(ts)];

  ts_idx = cwh_types_scalar_TY(ts_idx);

  return (TY_is_character(Ty_Table[ts_idx]));
}

/*===================================================
 *
 * cwh_types_character_extra
 *
 * The length argument associated with a character
 * dummy is not explicit in the call. Make a 
 * temporary ST and hand it back.
 *
 ====================================================
*/
extern ST *
cwh_types_character_extra(ST *dummy)
{
  TY_IDX ty_idx ;
  ST * st ;

  st = NULL;

  if (cwh_types_is_character(ST_type(dummy))) {

    ty_idx = Be_Type_Tbl(cwh_addr_char_len_typeid);
    st = cwh_types_formal_util(ty_idx);
    Set_ST_is_value_parm(st);
    Set_ST_is_temp_var(st);
  }

  return(st);
}


/*===================================================
 *
 * cwh_types_formal_util
 *
 * Make a formal of given TY & entr into the
 * symbol table. Used for character len type
 * parameters.
 *
 ====================================================
*/
static ST *
cwh_types_formal_util(TY_IDX ty_idx)
{
  ST * st;

  st = New_ST(CURRENT_SYMTAB);
  cwh_auxst_clear(st);
  
  ST_Init(st, Save_Str(cwh_types_mk_anon_name(".len")), CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, ty_idx);

  return st ;
}

/*===================================================
 *
 * cwh_types_mk_struct
 *
 * Make a TY for a struct - utility routine
 * for generic structs. TY entered into symbol table.
 * FLDs are not created, but the head of the list of
 * flds should appear in 'list'. Will always return
 * a unique idx, because don't know if FLDs are
 * filled at this point.
 *
 ====================================================
*/
static TY_IDX
cwh_types_mk_struct(INT64 size, INT32 align, FLD_HANDLE list, const char *name)
{
  TY_IDX ty_idx ;

  BUMP_TY_COUNTER(c_TY_STRUCT) ;

  ty_idx = cwh_types_new_TY(TRUE,align) ;
  TY& ty = Ty_Table[ty_idx];

  TY_Init (ty, size, KIND_STRUCT, MTYPE_M, Save_Str(cwh_types_mk_anon_name(name)));

  Set_TY_fld(ty, list);
  return (ty_idx);

}

/*===================================================
 *
 * cwh_types_array_util
 *
 * Make a TY for an N-d array - utility routine
 * for filling in a few generic details. 
 *
 * Sets TY_AR_*_val to 0 and
 *      TY_AR_const_* to TRUE
 * 
 * alloc_arbs sets up the ARB information if TRUE, otherwise, it leaves it blank
 *
 ====================================================
*/
extern TY_IDX
cwh_types_array_util(INT16 rank, TY_IDX ety_idx, INT32 align, INT64 size, const char * name, BOOL alloc_arbs)
{
  TY_IDX  ty_idx ;
  INT16 i ;

  if (rank == 0)
    return (0);

  BUMP_TY_COUNTER(c_TY_ARRAY);

  ty_idx = cwh_types_new_TY(TRUE,align);
  TY &ty = Ty_Table[ty_idx];
  TY_Init (ty, size, KIND_ARRAY, MTYPE_UNKNOWN, Save_Str(cwh_types_mk_anon_name(name)));

  Set_TY_etype(ty, ety_idx);

  if (alloc_arbs) {
     for (i = 0 ; i < rank ; i++) {
	
	ARB_HANDLE arb = New_ARB();
	ARB_Init (arb, 1, 1, 1);
	
	if (i == 0) {
	   Set_ARB_first_dimen(arb);
	   Set_TY_arb (ty, arb);
	}
	
	Set_ARB_dimension (arb, rank - i );
	
	if (i == rank - 1)
	   Set_ARB_last_dimen (arb);
	
	Set_ARB_const_lbnd (arb);
	Set_ARB_lbnd_val (arb, 0);
	
	Set_ARB_const_stride (arb);
	Set_ARB_stride_val (arb, 0);
	
	Set_ARB_const_ubnd (arb);
	Set_ARB_ubnd_val (arb, 0);
	
     }
  }

  return (ty_idx);
}

/*===================================================
 *
 * cwh_types_dim_struct_TY
 *
 * Make a struct TY for a set of dope bounds. Saves
 * duplication, cached in global symtab.
 *
 ====================================================
*/
static TY_IDX
cwh_types_dim_struct_TY(void)
{
  INT16 i   ;
  
  static TY_IDX dim_TY_idx = 0;
  
  INT32 sz  ;
  
  if (dim_TY_idx == 0) {

    sz   = DOPE_bound_sz ;

    DOPE_bound_ty = Be_Type_Tbl(cwh_bound_int_typeid);

    FLD_HANDLE first;
    for (i=0; i < BOUND_NM; i++) {
       FLD_HANDLE fld = cwh_types_fld_util(bound_name[i],DOPE_bound_ty,(OFFSET_64)i*sz, TRUE);
       if (i == 0)
          first = fld;
       if (i == BOUND_NM - 1)
          Set_FLD_last_field(fld);
    }

    dim_TY_idx = cwh_types_mk_struct(DIM_SZ,Pointer_Size,first,".dope_bnd.");
  } 

  return(dim_TY_idx);
}

/*===================================================
 *
 * cwh_types_dim_TY
 *
 * Make an array TY for n dope bound structs. Saves
 * duplication, cached in global symtab.
 *
 ====================================================
*/
static TY_IDX
cwh_types_dim_TY(INT32 num_dims)
{
  INT32 sz ;
  TY_IDX ta_idx  ;
  TY_IDX tb_idx  ;
  ARB_HANDLE arb;

  static TY_IDX tbl[MAX_ARY_DIMS+1] = {0,0,0,0,0,0,0,0};

  if (num_dims == 0) 
    return (0);

  if (tbl[num_dims] == 0) {

    tb_idx = cwh_types_dim_struct_TY() ;
    
    sz = num_dims * DIM_SZ ;
    ta_idx = cwh_types_array_util(1,tb_idx,Pointer_Size,sz,".dims.",TRUE) ;
   
    arb = TY_arb(ta_idx);

    Set_ARB_ubnd_val(arb, num_dims - 1);
    Set_ARB_stride_val(arb, DIM_SZ);
    
    ta_idx = cwh_types_unique_TY(ta_idx);

    tbl[num_dims] = ta_idx ;
  }
    
  return(tbl[num_dims]) ;
}

/*===================================================
 *
 * cwh_types_dope_TY
 *
 * Make an dope_vector TY for a scalar TY of base
 * and rank num_dims. NB - there should be flag in
 * the TY saying this is a dope struct, but there 
 * isn't yet. We use the name '.dope.' to recognize
 * a dope vector. See cwh_types_is_dope.
 *
 ====================================================
*/
extern TY_IDX
cwh_types_dope_TY(INT32 num_dims,TY_IDX base_idx, BOOL host, BOOL ptr,
#ifdef KEY /* Bug 6845 */
  INT32 n_allocatable_cpnt
#endif /* KEY Bug 6845 */
  )
{
  TY_IDX ty_idx   ;
  TY_IDX ta_idx   ;
  TY_IDX  dope_invariant_ty;
  INT i;

  static BOOL    dims_ty_inited = FALSE;
  static TY_IDX  dims_ty[MAX_ARY_DIMS];

  // Create the dims part of the structure
  if (!dims_ty_inited) {
     for(i=0; i < MAX_ARY_DIMS; i++) {
	dims_ty[i] = 0;
     }
     dims_ty_inited = TRUE;
  }
  
  if (num_dims > 0 && dims_ty[num_dims-1] == 0) {
     dims_ty[num_dims-1] = cwh_types_dim_TY(num_dims);
  }

  // Create the invariant part of the structure
  dope_invariant_ty = cwh_types_mk_dope_invariant_TY();

  /* add address FLD, with type a pointer  */
  /* to an array of the basic type.        */

  FLD_HANDLE base_fld = cwh_types_fld_util(dope_name[0],
					   Be_Type_Tbl(dope_btype[0]),
					   (OFFSET_64)dope_offset[0],
					   TRUE);
  
  /* create dope vector elements. */
  /* descriptors of the same rank (ie: same dims FLD)           */

  FLD_HANDLE fld = cwh_types_fld_util(".flds",
				      dope_invariant_ty,
				      (OFFSET_64)dope_offset[1],
				      TRUE);
  
  if (num_dims != 0) {
     fld = cwh_types_fld_util(".dims.",
				  dims_ty[num_dims-1],
				  (OFFSET_64)DOPE_sz,
				  TRUE);
  }
  
  Set_FLD_last_field(fld);
  
  ta_idx = cwh_types_array_util(num_dims,base_idx,Pointer_Size,0,".base.",TRUE);
  
  if (ta_idx != 0) 
    ta_idx = cwh_types_unique_TY(ta_idx);
  else
    ta_idx = base_idx ;

  /* If a pointer within a derived type, then the dope  */
  /* is created before the TY FLDs are complete. Ensure */
  /* don't get false match on lookup of pointer to TY   */
  /* when TY_flist == 0                              */

  TY& ta = Ty_Table[ta_idx];

  if ((TY_kind(ta) == KIND_STRUCT) && (TY_fld(ta).Is_Null ()))
    Set_FLD_type(base_fld, cwh_types_mk_unique_pointer_TY(ta_idx, host));
  else
    Set_FLD_type(base_fld, cwh_types_mk_pointer_TY(ta_idx, host));
  
  /* make dope vector TY */
  
  ty_idx = cwh_types_shared_dope(base_fld,num_dims,ptr,
#ifdef KEY /* Bug 6845 */
    n_allocatable_cpnt
#endif /* KEY Bug 6845 */
    );
  
  return(ty_idx);
}

/*===================================================
 *
 * cwh_types_mk_dope_invariant_TY
 *
 * Make the ty for the invariant part of a dope vector (i.e., 
 * the part containing everything but the base and the bounds)
 *
 ====================================================
*/
static TY_IDX
cwh_types_mk_dope_invariant_TY(void)
{
  INT i ;
  OFFSET_64 first_offset;
  static TY_IDX invariant_ty=0;
  
  if (invariant_ty != 0) return (invariant_ty);
  
  // We need to create the TY.
  // Create the fields. We start with 1 to skip the 
  // base, and we offset the byte offset by the dope_offset of the first element
  // so that this part of the structure is 0 based.

  FLD_HANDLE first = cwh_types_fld_util(dope_name[1],
					Be_Type_Tbl(dope_btype[1]),
					(OFFSET_64) 0,
					TRUE);
  first_offset = dope_offset[1];

  FLD_HANDLE fld;
  for(i=2; i < DOPE_NM; i++) {
     fld = cwh_types_fld_util(dope_name[i],
			      Be_Type_Tbl(dope_btype[i]),
			      (OFFSET_64)dope_offset[i] - first_offset ,
			      TRUE);
     Set_FLD_bofst(fld, dope_bofst[i]);
     Set_FLD_bsize(fld, dope_bsize[i]);
     if (dope_bsize[i] != 0)
	Set_FLD_is_bit_field(fld);
  }
  Set_FLD_last_field(fld);
  
  // Create the TY
  invariant_ty = cwh_types_mk_struct(DOPE_sz - first_offset,
				     Pointer_Size,first,(char *)dope_invariant_str); 
  return (invariant_ty);
}

/*===================================================
 *
 * cwh_types_shared_dope
 *
 * Given a dope vector's field list, base type
 * and pointer attribute, decide if we have a
 * dope vector we can use. If so use it. Pointers
 * and arrays have different dope vectors because
 * they have different TY flags.
 *
 * Dope vectors for scalar, intrinsic types are
 * always GLOBAL, and shared, one per rank.
 *
 ====================================================
*/
static TY_IDX
cwh_types_shared_dope(FLD_HANDLE fld, int ndims, BOOL is_ptr,
#ifdef KEY /* Bug 6845 */
  int n_allocatable_cpnt
#endif /* KEY Bug 6845 */
  )
{
  static TY_IDX intrn_dope[MAX_ARY_DIMS+1][NUM_DOPE_TYPES] ;
  static TY_IDX intrn_ptrs_dope[MAX_ARY_DIMS+1][NUM_DOPE_TYPES] ;
  TY_IDX *p  ;
  TY_IDX  dv_idx ;
  TY_IDX  tp_idx ;      
  TY_IDX  tb_idx ;      
  TYPE_ID bt ;      

  INT64 sz   ;
  INT32 al   ;
  
  /* get TY of object dope describes */

  dv_idx = 0 ;

  tp_idx = TY_pointed(Ty_Table[FLD_type(fld)]);
  tb_idx = cwh_types_scalar_TY(tp_idx); 

  TY& tb = Ty_Table[tb_idx];

  
  if (IS_SHARED_DOPE_BASE(tb)) {
    
    bt = TY_mtype(tb);

    if (TY_is_logical(tb))
	bt = LOGICAL_OFFSET(bt);

    if (is_ptr) 
      p = &intrn_ptrs_dope[ndims][bt];
    else
      p = &intrn_dope[ndims][bt];
    
    if (*p == 0) {

      sz = DOPE_sz + ndims * DIM_SZ ;
      al = Pointer_Size;
      *p = cwh_types_mk_struct(sz,al,fld,(char *)dope_str); 

      TY& ty = Ty_Table[*p];

      if (is_ptr) 
	Set_TY_is_f90_pointer(ty);
      else
	Clear_TY_is_f90_pointer(ty);

    } 

    BUMP_TY_COUNTER(c_TY_DOPE_INTRIN);
    dv_idx = *p;
    
  } else { /* either dtype component, or dtype */

    sz  = DOPE_sz + ndims * DIM_SZ ;
#ifdef KEY /* Bug 6845 */
    /*
     * If the type is a derived type, then the code takes this branch and does
     * not use the quick-and-dirty cache of types. If this dope vector
     * represents an allocatable arary of a derived type, and the derived type
     * has allocatable components, we need enough extra room to hold a count
     * of the allocatable components plus an offset for each one.
     */
    if (n_allocatable_cpnt) {
      n_allocatable_cpnt += 1; /* Space for count of allocatable components */
      sz += n_allocatable_cpnt * DOPE_bound_sz;
    }
#endif /* KEY Bug 6845 */

    al  = Pointer_Size;
    dv_idx  = cwh_types_mk_struct(sz,al,fld,(char *)dope_str); 

    TY&  dv = Ty_Table[dv_idx];

    if (is_ptr) 
      Set_TY_is_f90_pointer(dv);
    else
      Clear_TY_is_f90_pointer(dv);
  }

  return dv_idx ;
}


/*===================================================
 *
 * cwh_types_is_dope
 *
 * return TRUE if this is a dope TY. 
 *
 * better would be TY flag in stab.h, but this routine
 * is seldom used.
 *
 ====================================================
*/
extern BOOL
cwh_types_is_dope(TY_IDX ty)
{

  while (TY_kind(ty) == KIND_POINTER) {
    ty = TY_pointed(ty);
  }

  if (strncmp(TY_name(ty),dope_str,DOPENM_LEN) == 0 )
    return TRUE;

  return FALSE ;
}

/*===================================================
 *
 * cwh_types_dope_rank
 *
 * Given a dope vector TY, return its rank.
 *
 ====================================================
*/
extern INT32 
cwh_types_dope_rank(TY_IDX ty_idx)
{
  INT32 nd ;

  nd = 0 ;

  TY &ty = Ty_Table[ty_idx];

  FLD_HANDLE fl = TY_fld(ty);

  while(!FLD_last_field(fl)) 
     fl = FLD_next(fl);

  if (!fl.Is_Null ()) {

     if (FLD_ofst(fl) > dope_offset[DOPE_NM-1]) {
	ARB_HANDLE arb = TY_arb(FLD_type(fl));
	nd = 1 + ARB_ubnd_val(arb);
     }
  }

  return (nd);
}

/*===================================================
 *
 * cwh_types_dope_basic_TY
 *
 * Given a dope vector TY, return the array TY
 * of the info the dope describes.
 *
 ====================================================
*/
extern TY_IDX
cwh_types_dope_basic_TY(TY_IDX ty)
{
  while (TY_kind(ty) == KIND_POINTER)
    ty = TY_pointed(ty);

  return (TY_pointed(FLD_type(TY_fld(Ty_Table[ty]))));
}

/*===================================================
 *
 * cwh_types_dope_dims_FLD
 *
 * Given a dope vector TY, return the dims FLD
 * - it describes the bounds struct.
 *
 ====================================================
*/
extern FLD_HANDLE
cwh_types_dope_dims_FLD(TY_IDX ty)
{
  while (TY_kind(ty) == KIND_POINTER)
    ty = TY_pointed(ty);

  FLD_HANDLE fl = TY_fld(Ty_Table[ty]);

  while (!FLD_last_field(fl)) {
    fl = FLD_next(fl);
  }

  if (FLD_ofst(fl) <= dope_offset[DOPE_NM-1])
    fl = FLD_HANDLE ();

  return fl;
}

/*===================================================
 *
 * cwh_types_contains_dope
 *
 * Utility for functions returning structures.
 * Does the structure contain a dope vector. If so
 * return T. ( the consequence is that the struct
 * result must be passed by address, so the FE 
 * fills in the dope).
 *
 ====================================================
*/
extern bool
cwh_types_contains_dope(TY_IDX ty)
{
  bool res = false;

  if (TY_kind(ty) == KIND_STRUCT) {
    res = cwh_types_is_dope(ty);

    if (!res) {

      FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));

      do {

	FLD_HANDLE p (fld_iter);
	res = cwh_types_contains_dope(FLD_type(p));

      } while (!res && !FLD_last_field(fld_iter++)) ;
    }
  }

  return res;
}

/*===================================================
 *
 * cwh_types_fld_util
 *
 * Utility to make a FLD which has the bofst and
 * bsize set to zero. Not entered into symtab.
 *
 ====================================================
*/
static FLD_HANDLE
cwh_types_fld_util(const char* name_string, TY_IDX fld_ty,  OFFSET_64 offset, BOOL global)
{

  FLD_HANDLE fld;

  if (fld_ty == 0)
    return(fld);

  fld = New_FLD ();
  FLD_Init (fld, Save_Str(name_string), fld_ty, offset);
  Set_FLD_bofst(fld, 0);
  Set_FLD_bsize(fld, 0);

  return(fld);
}

/*===================================================
 *
 * cwh_types_fld_dummy
 *
 * Make a FLD with an offset and type TY,
 * so type and offset information can be propagated 
 * on the stack. 
 *
 ====================================================
*/
extern FLD_HANDLE
cwh_types_fld_dummy(OFFSET_64 off,TY_IDX ty)
{
  FLD_HANDLE fld ;

  fld = cwh_types_fld_util(".dummy.",ty,off,FALSE);
  return (fld);
}

/*===================================================
 *
 * cwh_types_array_temp_TY
 *
 * Given an OPC_ARRAYEXP and a TY of a scalar
 * element, make an array TY of the shape
 * described in the OPC_ARRAYEXP.
 *
 * The ARRAY node has the bounds in C order, but
 * they are translated to fortran order, because 
 * that's the way the utility routine expects them.
 *
 ====================================================
*/
extern TY_IDX
cwh_types_array_temp_TY(WN *ar, TY_IDX sc )
{
  TY_IDX ty    ;
  WN  * wn    ;
  ARB_HANDLE  bound; 
  TYPE_ID   bt ;
  INT64 size;
  INT16 nd,i,j  ;  


  nd = WN_kid_count(ar) - 1;
  bt = cwh_bound_int_typeid ;

  /* setup bounds */

  for (i = 0 ; i < nd  ; i ++) {

    j = nd - i;

    ARB_HANDLE arb = New_ARB();
    ARB_Init (arb, 1, 1, 1);
    if (i == 0) {
       bound = arb;
    }

    Set_ARB_const_lbnd(arb);
    Set_ARB_lbnd_val(arb, 0);
    Clear_ARB_first_dimen(arb);
    Clear_ARB_last_dimen(arb);

    if (WNOPR(WN_kid(ar,j)) == OPR_INTCONST) {

      Set_ARB_const_ubnd(arb);
      Set_ARB_ubnd_val(arb, WN_const_val(WN_kid(ar,j)) -1);

    } else {

      WN *expr;
      ST *st;

      expr = cwh_expr_bincalc(OPR_SUB,
			      WN_COPY_Tree(WN_kid(ar,j)),
                              WN_Intconst(bt,1));

      Clear_ARB_const_ubnd(arb);

      st = cwh_types_make_bounds_ST();
      cwh_addr_store_ST(st,0,0,expr);
      Set_ARB_ubnd_var(arb, ST_st_idx(st));

    }
  }

  /* setup strides */
  
  if (TY_size(sc) != 0) {

    Set_ARB_const_stride(bound[0]);
    Set_ARB_stride_val(bound[0], TY_size(sc));

  } else {    /* must be character substring definition */

    ARB_HANDLE sc_arb = TY_arb(sc);

    Clear_ARB_const_stride(bound[0]);
    Set_ARB_stride_var(bound[0], ARB_ubnd_var(sc_arb));
  }

  for (i = 1 ; i < nd ; i ++) {

    ARB_HANDLE arb = bound[i-1];

    if (ARB_const_stride(arb)) {
      if (ARB_const_ubnd(arb)) {

	ARB_HANDLE arb2 = bound[i];

	Set_ARB_const_stride(arb2);
	Set_ARB_stride_val(arb2, ARB_stride_val(arb) * (ARB_ubnd_val(arb) + 1 ));
      } else {
 
        ST *st;
        WN *wn2;
       
	ARB_HANDLE arb2 = bound[i];

	Clear_ARB_const_stride(arb2);

	wn = WN_Intconst(cwh_bound_int_typeid,1 + ARB_const_ubnd(arb));
        wn2 = cwh_addr_load_ST(&St_Table[ARB_ubnd_var(arb)],0,0);
        wn = cwh_expr_bincalc(OPR_MPY, wn2, wn);

	st = cwh_types_make_bounds_ST();
        cwh_addr_store_ST(st,0,0,wn);
	Set_ARB_stride_var(arb2, ST_st_idx(st));
      }
    } else {

      ARB_HANDLE arb2 = bound[i];
      ST *st;

      Clear_ARB_const_stride(arb2);

      if (ARB_const_ubnd(arb)) {
	wn = cwh_expr_bincalc(OPR_ADD,
			      WN_Intconst(bt,ARB_ubnd_val(arb)),
			      WN_Intconst(bt,1));
      } else {
        WN *wn2 = cwh_addr_load_ST(&St_Table[ARB_ubnd_var(arb)],0,0);
	wn = cwh_expr_bincalc(OPR_ADD,wn2,WN_Intconst(bt,1));
      }

      wn = cwh_expr_bincalc(OPR_MPY,
                            wn,
                            cwh_addr_load_ST(&St_Table[ARB_stride_var(arb)], 0, 0)); 
      st = cwh_types_make_bounds_ST();
      cwh_addr_store_ST(st,0,0,wn);
      Set_ARB_stride_var(arb2, ST_st_idx(st));
    }
  }

  ARB_HANDLE last_arb = bound[nd-1];
 
  if (ARB_const_stride(last_arb) && ARB_const_ubnd(last_arb)
      && ARB_const_lbnd(last_arb)) {
     size = ARB_stride_val(last_arb)*(ARB_ubnd_val(last_arb) 
					 - ARB_lbnd_val(last_arb)
					 + 1);
  } else {
     size = 0;
  }

  Set_ARB_first_dimen(bound[0]);
  Set_ARB_last_dimen(last_arb);

  ty = cwh_types_mk_array_TY(bound,nd,sc,size);
  return(ty);
}

/*===================================================
 *
 * cwh_types_size_WN
 *
 * Given an array TY & an element size, return a WN with
 * the size of a TY of that shape and size.
 *
 ====================================================
*/
extern WN * 
cwh_types_size_WN(TY_IDX ty, WN *e_sz)
{
  INT16 nd ;
  WN   *wn ;
  WN   *lb ;
  WN   *ub ;
  WN   *st ;
  WN   *wt ;
  INT  i;

  Is_True((TY_kind(ty) == KIND_ARRAY),("Odd size calc"));

  nd = ARB_dimension (TY_arb (ty));
  wn = e_sz;

  for (i = 0; i < nd ; i++) {
     lb = cwh_types_bound_WN(ty,i,LOW);
     ub = cwh_types_bound_WN(ty,i,UPPER);
     st = WN_Intconst(MTYPE_I4,1);
     wt = cwh_addr_extent(lb,ub,st);
     wn = cwh_expr_bincalc(OPR_MPY,wt,wn);
  }
  
  return(wn);
}

/*===================================================
 *
 * cwh_types_bound_WN
 *
 * Given an TY, index (0..n-1), and bound flag
 * return the bound value as  WN.
 *
 ====================================================
*/
extern WN *
cwh_types_bound_WN(TY_IDX ty, INT16 i, enum ty_bound_enum  b)
{
#ifdef KEY /* Bug 10177 */
  WN * wn = 0;
#else /* KEY Bug 10177 */
  WN * wn ;
#endif /* KEY Bug 10177 */

  ARB_HANDLE  arb = TY_arb(ty);
  INT16     nd = ARB_dimension(arb);
  arb = arb[nd-i-1];
  
  switch (b) {
  case LOW:
    if (ARB_const_lbnd(arb)) 
      wn = WN_Intconst(cwh_bound_int_typeid,ARB_lbnd_val(arb)) ;
    else
      wn = cwh_addr_load_ST(&St_Table[ARB_lbnd_var(arb)],0,0);
    break ;

  case UPPER:
    if (ARB_const_ubnd(arb)) 
      wn = WN_Intconst(cwh_bound_int_typeid,ARB_ubnd_val(arb)) ;
    else
      wn = cwh_addr_load_ST(&St_Table[ARB_ubnd_var(arb)],0,0);
    break ;

  case STRIDE:
    if (ARB_const_stride(arb)) 
      wn = WN_Intconst(cwh_bound_int_typeid,ARB_stride_val(arb)) ;
    else
      wn = cwh_addr_load_ST(&St_Table[ARB_stride_var(arb)],0,0);
    break ;
  }

  return (wn) ;
}

/*===================================================
 *
 * cwh_types_get_dope_info
 *
 * Get information about where in a dope vector things are
 * crayfield - field from the Cray dopevector defintion
#ifdef KEY
 *		Use typedef enum dv_idx_type instead of these integers:
#endif
 *              1.base_addr 
 *              2.el_len 
 *              3.assoc 
 *              4.ptr_alloc 
 *              5.p_or_a 
 *              6.contig 
 *              7.n_dim 
 *              8.typ_code 
 *              9.orig_base 
 *             10.orig_size 
 *
 * offset - byte offset of the word to read
 * rshift - number of bits to right-shift the field, or 0
 * mask   - bit mask to get the field, or 0
 * ty     - TYPE_ID to use for loads and operations on the field
 *
 ====================================================
*/
extern void  
cwh_types_get_dope_info(
#ifdef KEY /* Bug6845 */
  dv_idx_type crayfield,
#else /* KEY Bug6845 */
  INT32 crayfield,
#endif /* KEY Bug6845 */
  INT32 *offset, INT32 *rshift, 
				     INT64 *mask, TYPE_ID *ty)
{
   INT real_field;
   INT shift;
   INT size;
   INT ty_size;
   
#ifdef KEY /* Bug 6845 */
   real_field = (INT) crayfield;
#else /* KEY Bug6845 */
   /* Skip unused fields */
   if (crayfield >= 8) {
//      real_field = crayfield + 1;
      real_field = crayfield;
   } else if (crayfield == 7) {
      real_field = crayfield;
   } else {
      real_field = crayfield - 1;
   }
#endif /* KEY Bug 6845 */

   *offset = dope_offset[real_field];
   *ty = dope_btype[real_field];
   shift = dope_bofst[real_field];
   size = dope_bsize[real_field];
   ty_size = MTYPE_size_best(*ty);
   
#ifdef KEY /* Bug 6371 */
   if (size == (sizeof(1LL) * CHAR_BIT)) {
      *mask = 0;
   } else
#endif /* KEY Bug 6371 */
   if (size != 0) {
      *mask = (1LL << size) - 1;
   } else {
      *mask = 0;
   }
   if (shift != 0 || size != 0) {
# if defined(linux) || defined(BUILD_OS_DARWIN)
      *rshift = shift;
# else
      *rshift = ty_size - shift - size;
# endif
   } else {
      *rshift = 0;
   } 
   return;
}

/*===================================================
 *
 * cwh_types_mk_pointer_TY
 *
 * Make a POINTER TY for the TY handed in.
 *
 ====================================================
*/
extern TY_IDX
cwh_types_mk_pointer_TY(TY_IDX ty_idx, BOOL host)
{
  TY_IDX  tr_idx   ;

  tr_idx = Make_Pointer_Type(ty_idx);

  return(tr_idx);
}

/*===================================================
 *
 * cwh_types_mk_unique_pointer_TY
 *
 * Make a POINTER TY for the TY handed in. Don't look
 * up other pointers with Make_Pointer_TY
 *
 ====================================================
*/
static TY_IDX
cwh_types_mk_unique_pointer_TY(TY_IDX ty, BOOL host)
{
  TY_IDX tp_idx;

  BUMP_TY_COUNTER(c_TY_UNIQ_POINTER) ;

  tp_idx = cwh_types_new_TY (TRUE,Pointer_Size);
  TY& tp = Ty_Table[tp_idx];
  TY_Init(tp, Pointer_Size, KIND_POINTER, Pointer_Mtype, Save_Str(cwh_types_mk_anon_name(".uniq_p.")));

  Set_TY_pointed(tp, ty);

  tp_idx = cwh_types_unique_TY(tp_idx);

  return tp_idx;
}


/*===================================================
 *
 * cwh_types_mk_common_TY
 *
 * Make a TY for a common block name. Don't
 * know the items in the common yet, so just
 * create the TY and return.
 *
 * If the alignment is 0, we choose a default
 * of 4. cwh_types_mk_element will bump it,
 * if an element requires a bigger alignment.
 *
 ====================================================
*/
extern TY_IDX
cwh_types_mk_common_TY(INT64 size, mUINT16 al )
{
  TY_IDX ty    ;
  INT64 sz  ;

  if (al == 0) 
    al = 4;
  
  sz = bit_to_byte(size);
  ty = cwh_types_mk_struct(sz,al,FLD_HANDLE(),".common.");
                            
  return(ty);
}

/*===================================================
 *
 * cwh_types_mk_equiv_TY
 *
 * Make a TY for an equivalence block name. Don't
 * know the items in the common yet, so just
 * create the TY and return
 *
 ====================================================
*/
extern TY_IDX
cwh_types_mk_equiv_TY(INT64 size)
{
  TY_IDX ty    ;
  INT64 sz  ;
  
  sz = bit_to_byte(size);
  ty = cwh_types_mk_struct(sz,MAX_ALIGN,FLD_HANDLE(),".equiv.") ;
                            
  return ty ;
}

/*===================================================
 *
 * cwh_types_mk_namelist_TY
 *
 * Make a TY for a namelist. There are two parts -
 * an array of namelist item entries and a namelist 
 * name.
 *
 * The items themselves weem irrelevant. Why?
 *
 ====================================================
*/
extern TY_IDX
cwh_types_mk_namelist_TY(INT32 nitems)
{
  TY_IDX ty   ;
  TY_IDX tn   ;
  TY_IDX te   ;
  TY_IDX ta_idx   ;
  WN *wn   ;
  FLD_HANDLE f1 ;
  FLD_HANDLE f2 ;

  /* array of namelist items */

  te = cwh_types_mk_namelist_item_TY();
  ta_idx = cwh_types_array_util(1,
			    te,
			    NL_Tables[ALIGN_Nlentry][NL_Table_Index],
			    TY_size(te),
			    ".NL_item_array.",
			    TRUE);

  TY& ta = Ty_Table[ta_idx];

  Set_TY_AR_ubnd_val(ta, 0, nitems - 1);
  Set_TY_AR_stride_val(ta, 0, TY_size(te));

  ta_idx = cwh_types_unique_TY(ta_idx);



  /* namelist name */

  wn = WN_Intconst(MTYPE_I4,NL_Name_Length) ;
  tn = cwh_types_mk_character_TY(wn,NULL,TRUE);
  f1 = cwh_types_fld_util(".NL_name.",tn,NL_Tables[OFFSET_Namelist_nlname][NL_Table_Index],TRUE);

  f2 = cwh_types_fld_util(".NL_vars.",ta_idx,NL_Tables[OFFSET_Namelist_nlvnames][NL_Table_Index],TRUE);

  Set_FLD_last_field(f2);
  
  ty = cwh_types_mk_struct(TY_size(tn) + TY_size(ta),
			   NL_Tables[ALIGN_Namelist][NL_Table_Index],
			   f1,
			   ".Namelist.");
  return ty ;
}

/*===================================================
 *
 * cwh_types_mk_namelist_item_TY
 *
 * Make a STRUCT for a namelist item
 *    item name
 *    item address ptr
 *    item type
 *    item dims?
 *
 *  Use one global TY for all items.
 *
 ====================================================
*/
static TY_IDX
cwh_types_mk_namelist_item_TY(void)
{
  TY_IDX ty ;
  TY_IDX tp ;
  TY_IDX tc ;
  FLD_HANDLE f1 ;
  FLD_HANDLE f2 ;
  FLD_HANDLE f3 ;
  FLD_HANDLE f4 ;
  WN  * wn ;

  static TY_IDX gl_ty = 0 ;

  if (gl_ty == 0) {

    tp = Make_Pointer_Type(Be_Type_Tbl(MTYPE_V));

    ty = Be_Type_Tbl(MTYPE_I4); 

    wn = WN_Intconst(MTYPE_I4,NL_Name_Length) ;

    tc = cwh_types_mk_character_TY(wn,NULL,TRUE);

  
    f1 = cwh_types_fld_util("varname",tc,NL_Tables[OFFSET_Nlentry_varname][NL_Table_Index],TRUE);
    f2 = cwh_types_fld_util("varaddr",tp,NL_Tables[OFFSET_Nlentry_varaddr][NL_Table_Index],TRUE);
    f3 = cwh_types_fld_util("type",ty,NL_Tables[OFFSET_Nlentry_type][NL_Table_Index],TRUE);
    f4 = cwh_types_fld_util("dimp",tp,NL_Tables[OFFSET_Nlentry_dimp][NL_Table_Index],TRUE);

    Set_FLD_last_field(f4);

    WN_DELETE_Tree(wn);
    gl_ty = cwh_types_mk_struct(NL_Tables[SIZE_Nlentry][NL_Table_Index],
				NL_Tables[ALIGN_Nlentry][NL_Table_Index],
				f1,
				".NL_item.");
  }

  return gl_ty ;			  
}

/*===================================================
 *
 * cwh_types_mk_element
 *
 * Make a FLD for a common block element or 
 * equivalence class. The ST of the common or is
 * equivalence is the first argument & the element 
 * the second. Set equivalence flags based on ST.
 *
 *
 ====================================================
*/
extern void
cwh_types_mk_element(ST *c, ST * st)
{
  TY_IDX cbty ;
  FLD_HANDLE fld  ;
  FLD_HANDLE nfld ;
  FLD_HANDLE pfld ;

  cbty = ST_type(c);
  fld  = cwh_types_fld_util(ST_name(st),ST_type(st),ST_ofst(st),TRUE);



  if (ST_is_equivalenced(st))
      if (!(IS_COMMON(st)))
        Set_FLD_equivalence(fld);

  if (ST_sclass(st) == SCLASS_COMMON) {
    Set_FLD_st(fld, ST_st_idx(st)); 
    Is_True((ST_level(st) == 1),("Bad common st level"));
  }


/*  Is_True((TY_align(ST_type(st))) <= TY_align(cbty),("Common align")); */

  if (TY_align(ST_type(st)) > TY_align(cbty)) {
      Set_TY_align(cbty, TY_align(ST_type(st)));
      Set_ST_type(*c,cbty);
  }

  if (TY_fld(Ty_Table[cbty]).Is_Null ()) {
    Set_TY_fld(Ty_Table[cbty], fld);
  } else {
    Clear_FLD_last_field(FLD_HANDLE (fld.Idx () - 1));
  }
  Set_FLD_last_field(fld);

}

/*===================================================
 *
 * cwh_types_mk_rslt_temp_TY
 *
 * Make a struct_ty for a struct-by-value temp for
 * a function result. This is so a result which
 * isn't 8 or 16 bytes won't write over adjacent
 * locations.
 *
 ====================================================
*/
extern TY_IDX
cwh_types_mk_result_temp_TY(void)
{
  TY_IDX ty  ;
  FLD_HANDLE f1 ;
  FLD_HANDLE f2 ;

  f1 = cwh_types_fld_util("rt1", Be_Type_Tbl(MTYPE_I8),0,TRUE);
  f2 = cwh_types_fld_util("rt2", Be_Type_Tbl(MTYPE_I8),0,TRUE);

  Set_FLD_last_field(f2);

  ty = cwh_types_mk_struct(RESULT_SIZE, RESULT_ALIGN,f1,"res_temp");

  return ty;
}
#ifdef KEY /* Bug 14110 */
/*
 * t		i_cvrt.c representation of a TY_IDX
 * return	same TY_IDX with "volatile" bit set
 */
extern unsigned
fei_set_volatile(unsigned t) {
  TY_IDX tmp = t;
  Set_TY_is_volatile(tmp);
  return tmp;
}
#endif /* KEY Bug 14110 */
/*===================================================
 *
 * cwh_types_fill_type
 *
 * Fill in the blanks of a PDGCS type
 *
 ====================================================
*/
static void
cwh_types_fill_type(INT32 flag_bits, TYPE *t, TY_IDX ty)
{

  t->const_flag    = test_flag(flag_bits,FEI_DESCRIPTOR_CONST_C);
  t->volatile_flag = test_flag(flag_bits,FEI_DESCRIPTOR_VOLAT_C);
  t->signed_flag   = test_flag(flag_bits,FEI_DESCRIPTOR_SIGN_C);
  t->automatic     = test_flag(flag_bits,FEI_DESCRIPTOR_AUTO_F);
  t->restricted    = test_flag(flag_bits,FEI_DESCRIPTOR_RESTR_C);
  t->short_flag    = test_flag(flag_bits,FEI_DESCRIPTOR_SHORT_C);
  t->long_flag     = test_flag(flag_bits,FEI_DESCRIPTOR_LONG_C);
  t->bitfield      = test_flag(flag_bits,FEI_DESCRIPTOR_BITFLD_C);
  t->aux_info      = 0 ;
  t->shrd_pointee  = test_flag(flag_bits,FEI_DESCRIPTOR_SHRD_PTEE);
  t_TY((*t))       =  cast_to_uint(ty);

}

/*===================================================
 *
 * cwh_types_mk_anon_name
 *
 * Provide a name for an anonymous type. If a 
 * string (<40 bytes) is passed, then it's used as
 * the stem, otherwise a NULL uses the stem ".anon."
 *
 *
 ====================================================
*/

extern char *
cwh_types_mk_anon_name (const char * nm)
{
  static char anonymous_str [64] ;
  static INT32 anonymous_index = 0;

  INT32 len ;

  if (nm == NULL) {
    len = 6;
    strcpy(anonymous_str,".anon.");

  } else {

    len = strlen(nm);
    Is_True((len < 40),("name too long"));
    strcpy(anonymous_str,nm);
  }


  return(anonymous_str);
}

/*===================================================
 *
 * cwh_types_push_dtype
 *
 * Push the details of a derived type on a stack. Used
 * to preserve details of an enclosing derived type 
 * while processing an inner type.
 *
 ====================================================
*/
static void
cwh_types_push_dtype(dtype_t d)
{

  dtype_top ++ ;

  if (dtype_top >= dtype_stk_size) {
     dtype_stk_size += STK_SIZE_CHANGE;
     dtype_stk = (dtype_t *) realloc(dtype_stk,sizeof(dtype_t)*dtype_stk_size);
  }

  dtype_stk[dtype_top].dty = d.dty ;
  dtype_stk[dtype_top].dty_last = d.dty_last; 
  dtype_stk[dtype_top].ncompos = d.ncompos ;
  dtype_stk[dtype_top].seq = d.seq;
  dtype_stk[dtype_top].hosted = d.hosted;

  return ;
}

/*===================================================
 *
 * cwh_types_pop_dtype
 *
 * Pop details of a derived type.
 *
 ====================================================
*/
static dtype_t 
cwh_types_pop_dtype(void)
{
  dtype_t d ;

  Is_True((dtype_top >= 0),(" Dtype stack underflow"));

  d.dty      = dtype_stk[dtype_top].dty ;
  d.dty_last = dtype_stk[dtype_top].dty_last;
  d.ncompos  = dtype_stk[dtype_top].ncompos ;
  d.seq      = dtype_stk[dtype_top].seq ;
  d.hosted   = dtype_stk[dtype_top].hosted ;

  dtype_top --;

  return(d);
}

/*===================================================
 *
 * cwh_types_in_dtype
 *
 * Are we in a derived type? If so, honor offset
 * and alignments of dtype.
 *
 ====================================================
*/
static BOOL
cwh_types_in_dtype(void)
{
  BOOL res = FALSE ;

  if (dtype_top >= 0 ) 
    res = TRUE ;

  return res ;
}
/*===================================================
 *
 * cwh_cray_type_from_TY
 *
 * return a Cray type from a TY - values
 * determined empirically from examination of FE output
 *
 ====================================================
*/
extern INT64
cwh_cray_type_from_TY(TY_IDX ty_idx)
{
#ifdef KEY /* Bug 10177 */
   TY_IDX base_ty_idx = 0;
#else /* KEY Bug 10177 */
   TY_IDX base_ty_idx;
#endif /* KEY Bug 10177 */
   INT64 rtype;
   f90_type_t  *f90_type_ptr;

   TY& ty = Ty_Table[ty_idx];

   rtype = 0;
   f90_type_ptr = (f90_type_t *)&rtype;

   if (TY_kind(ty) == KIND_ARRAY) {
      return (cwh_cray_type_from_TY(TY_etype(ty))); 
   } else if (TY_kind(ty) == KIND_SCALAR) {
      base_ty_idx = ty_idx;
   } else if (TY_kind(ty) == KIND_STRUCT) {
      f90_type_ptr->type = 8;		// DVTYPE_DERIVEDWORD
      return (rtype);
   } else {
      Is_True((0),("Do not know what to do with type"));
   }

   TY& base_ty = Ty_Table[base_ty_idx];

   if (TY_is_character(base_ty)) {
      f90_type_ptr->type = 6; 		// DVTYPE_ASCII
      f90_type_ptr->int_len = 8;
      return (rtype);
   }

   rtype = cwh_cray_type_from_MTYPE(TY_mtype(base_ty));
   if (TY_is_logical(base_ty)) {
      f90_type_ptr->type = 5;		// DVTYPE_LOGICAL
   }
   
   return (rtype);
}


/*===================================================
 *
 * cwh_cray_type_from_MTYPE
 *
 * return a Cray type from an MTYPE. Note that there is no 
 * way to tell that we are dealing with a LOGICAL type here
 *
 ====================================================
*/
extern INT64
cwh_cray_type_from_MTYPE(TYPE_ID ty)
{
   INT64 rtype;
   f90_type_t  *f90_type_ptr;

   rtype = 0;
   f90_type_ptr = (f90_type_t *)&rtype;

   switch (ty) {
    case MTYPE_I1: 
	f90_type_ptr->type = 2;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 8;
        f90_type_ptr->dec_len = 1;
	return (rtype);
	// return (0x2300801);

    case MTYPE_I2: 
	f90_type_ptr->type = 2;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 16;
        f90_type_ptr->dec_len = 2;
	return (rtype);
	// return (0x2301002);

    case MTYPE_I4: 
	f90_type_ptr->type = 2;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 32;
        f90_type_ptr->dec_len = 4;
	return (rtype);
	// return (0x2302004);

    case MTYPE_I8: 
	f90_type_ptr->type = 2;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 64;
        f90_type_ptr->dec_len = 8;
	return (rtype);
	// return (0x2304008);

    case MTYPE_F4: 
	f90_type_ptr->type = 3;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 32;
        f90_type_ptr->dec_len = 4;
	return (rtype);
	// return (0x3302004);

    case MTYPE_F8: 
	f90_type_ptr->type = 3;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 64;
        f90_type_ptr->dec_len = 8;
	return (rtype);
	// return (0x3304008);

    case MTYPE_FQ: 
	f90_type_ptr->type = 3;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 128;
        f90_type_ptr->dec_len = 16;
	return (rtype);
	// return (0x3308010);

    case MTYPE_C4: 
	f90_type_ptr->type = 4;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 64;
        f90_type_ptr->dec_len = 4;
	return (rtype);
	// return (0x4304004);

    case MTYPE_C8: 
	f90_type_ptr->type = 4;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 128;
        f90_type_ptr->dec_len = 8;
	return (rtype);
	// return (0x4308008);

    case MTYPE_CQ: 
	f90_type_ptr->type = 4;
	f90_type_ptr->kind_or_star = 3;
	f90_type_ptr->int_len = 256;
        f90_type_ptr->dec_len = 16;
	return (rtype);
	// return (0x4310010);
   }
   Is_True(0,("Do not know what to do with type"));

   return(rtype);
}

/*================================================================
 * 
 * cwh_types_init_target
 *
 * Set up the target-specific variables.
 *
 *================================================================
 */
extern void 
cwh_types_init_target(void)
{
   if (Pointer_Size == 4) {
      DOPE_bound_sz = 4;
      DOPE_dim_offset = 32;
      DOPE_sz = 32;
      dope_btype = dope_btype_32;
      dope_offset = dope_offset_32;
      NL_Table_Index = 0 ;

   } else {
      DOPE_bound_sz = 8;
      DOPE_dim_offset = 48;
      DOPE_sz = 48;
      dope_btype = dope_btype_64;
      dope_offset = dope_offset_64;
      NL_Table_Index = 1 ;
   }
   logical4_ty = cwh_types_mk_logical_TY(32,4);
}

/*===================================================
 *
 * The routines below are for making and marking the TY's 
 * on ILOADs, ISTOREs, MLOADs and MSTOREs which are through F90 
 * Pointers. These need to be marked so that the optimizer alias analysis can 
 * do its job.
 *
 *=====================================================
 */

typedef struct {
   TY_IDX ty;
   TY_IDX f90_pointed;
} type_pair_t;

static type_pair_t * pairs;
static INT num_type_pairs=0;
static INT max_type_pairs=0;
static INT pair_typenum=0;
#define TYPE_ALLOC_CHUNK_SIZE 32

static TY_IDX 
cwh_types_find_f90_pointer_ty (TY_IDX ty)
{
   INT i;
   for (i=0; i < num_type_pairs; i++) {
      if (pairs[i].ty == ty) {
	 return(pairs[i].f90_pointed);
      }
   }
   return 0;
}

extern TY_IDX
cwh_types_mk_f90_pointer_ty (TY_IDX ty)
{
   static BOOL made_real_types=FALSE;
   static BOOL made_unsigned_types=FALSE;
   TY_IDX t_idx;
   char buf[32];

   t_idx = cwh_types_find_f90_pointer_ty (ty);
   if (t_idx) return (t_idx);

   num_type_pairs += 1;
   if (num_type_pairs > max_type_pairs) {
      /* Allocate another chunk in the pairs array */
      max_type_pairs += TYPE_ALLOC_CHUNK_SIZE;
      if (max_type_pairs==TYPE_ALLOC_CHUNK_SIZE) {
	 pairs = (type_pair_t *) malloc(max_type_pairs * sizeof(type_pair_t));
      } else {
	 pairs = (type_pair_t *) realloc(pairs,max_type_pairs * sizeof(type_pair_t));
      }
   }
   
   /* Make up the type */

   BUMP_TY_COUNTER(c_TY_f90_POINTER) ;

   sprintf ( buf, ".anon_f90pointer.%d",++pair_typenum);

   t_idx = cwh_types_new_TY ( TRUE, Pointer_Size);
   TY& t = Ty_Table[t_idx];
   TY_Init(t, Pointer_Size, KIND_POINTER, Pointer_Mtype, Save_Str (buf));
   Set_TY_pointed(t, ty);
   Set_TY_is_f90_pointer(t);

   t_idx = cwh_types_unique_TY(t_idx);

   pairs[num_type_pairs-1].ty = ty;
   pairs[num_type_pairs-1].f90_pointed = t_idx;

   /* If the ty is for a complex type, make up the corresponding real pointers.
    * They will probably be there, but just in case, we make them up.
    */
   if (!made_real_types && MTYPE_is_complex(TY_mtype(ty))) {

      made_real_types = TRUE;
      (void) cwh_types_mk_f90_pointer_ty (Be_Type_Tbl(MTYPE_F4));
      (void) cwh_types_mk_f90_pointer_ty (Be_Type_Tbl(MTYPE_F8));
      (void) cwh_types_mk_f90_pointer_ty (Be_Type_Tbl(MTYPE_FQ));
   }

   /* If the ty is for an MLOAD or MSTORE, make up the types for U1, U4 and U8 */
   if (!made_unsigned_types && MTYPE_is_m(TY_mtype(ty))) {
      made_unsigned_types = TRUE;
      (void) cwh_types_mk_f90_pointer_ty (Be_Type_Tbl(MTYPE_U8));
      (void) cwh_types_mk_f90_pointer_ty (Be_Type_Tbl(MTYPE_U4));
      (void) cwh_types_mk_f90_pointer_ty (Be_Type_Tbl(MTYPE_U1));
   }
       
   return t_idx;
}

/*================================================================
 *
 * cwh_types_new_TY
 *
 * Create a new TY of the given alignment. Update the id of the
 * last created, so it can be deallocated if a duplicate.
 *
 * ================================================================
 */
static TY_IDX 
cwh_types_new_TY(BOOL global, INT32 align) 
{
  TY_IDX idx;

  TY& ty = New_TY(idx);

  Set_TY_align(idx,align);

  Last_TY_Created = idx;

  return idx ;
}

/*================================================================
 *
 * cwh_types_unique_TY
 *
 * check to see if a similar type has been created. 
 * Use it if so & delete input TY. It's expected to
 * be the last created. Alignment is handled
 *
 *  a) TY_is_unique updates just index ie: preserves flags
 *  b) Last_TY_Created is created with alignment.
 * 
 * Last_TY_Created is decremented, so can delete n-1, with luck,
 * if nested...
 *
 * ================================================================
 */
TY_IDX 
cwh_types_unique_TY(TY_IDX ty_idx) 
{
  TY_IDX new_ty_idx;

  new_ty_idx = TY_is_unique(ty_idx);

  if (new_ty_idx != ty_idx) {
    if (ty_idx == Last_TY_Created) {
      Ty_tab.Delete_last();
      Last_TY_Created-- ;
    }

  }
  return new_ty_idx;
}

TY_IDX
cwh_types_make_pointer_type(TY_IDX ty, BOOL f90_pointer) 
{
   if (f90_pointer) {
      return Make_F90_Pointer_Type (ty);
   } else {
      return Make_Pointer_Type (ty);
   }
}

/*================================================================
 *
 * cwh_types_make_bounds_ST
 *
 * Makes a bounds ST for non-constant cases.
 *
 * ================================================================
 */
static ST *
cwh_types_make_bounds_ST(void)
{
  ST * st; 

  TY_IDX bnd_ty = Be_Type_Tbl(cwh_bound_int_typeid);

  st = cwh_stab_temp_ST(bnd_ty,"bnd") ;
  return st;
}

/*================================================================
 *
 * cwh_types_copyin_pragma
 *
 * Makes a copyin xpragma for the bounds ST passed in. Assumes
 * not called for hosted ST (not reqd, & no hosted Preamble block yet...)
 * Note special case for character lengths, in cwh_addr_store_ST.
 *
 * ================================================================
 */
extern void
cwh_types_copyin_pragma(ST *st)
{
  WN *pragma;

    if (ST_sym_class(st) == CLASS_VAR &&
        !ST_auxst_xpragma_copyin(st)) {

      pragma = WN_CreateXpragma ( WN_PRAGMA_COPYIN_BOUND, (ST_IDX) NULL, 1 );
      WN_kid0(pragma) = cwh_addr_load_ST(st,0,0);
      cwh_block_append_given_id(pragma,Preamble_Block,FALSE);
      Set_ST_auxst_xpragma_copyin(st,TRUE);
    }
}

