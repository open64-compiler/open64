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
 * $Revision: 1.7 $
 * $Date: 04/12/21 14:57:31-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_data.cxx $
 *
 * Description: This static data initialization
 *
 * ====================================================================
  *====================================================================
 */

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_data.cxx $ $Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "stab.h"
#include "strtab.h"
#include "erfe90.h"
#include "errors.h"
#include "targ_const.h"
#include "config_targ.h"
#include "const.h"
#include "wn.h"
#include "wn_util.h"
#include "irbdata.h"
#include "cxx_memory.h"

/* Cray includes */

#include "i_cvrt.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_types.h"
#include "cwh_addr.h"
#include "cwh_stmt.h"
#include "cwh_preg.h"
#include "cwh_stab.h"
#include "cwh_auxst.h"
#include "cwh_stk.h"
#include "cwh_expr.h"
#include "cwh_data.h"

#include "cwh_data.i"

/*================================================================
 * fei_static_base
 * Get the base for the static initialization.
 * Also set up the initialization object structure.
 *================================================================
 */
extern void
fei_static_base(INTPTR sym_idx)
{
   STB_pkt *p ;
   ST *base;
   INT64 offset;
   BOOL init_common_or_module;
   
   p = cast_to_STB(sym_idx);
   DevAssert((p->form == is_ST),("Odd object ref"));

   current_st = (ST *) p->item;
   orig_st = current_st;
   current_ty = ST_type(current_st);
   current_pos = 0;
   array_pos = 0;
   init_common_or_module = FALSE;
   offset = 0;
   base = current_st;
   /* Check for based as a symbol in common */
   while (ST_base(base) != base) {
      offset += ST_ofst(base);
      base = ST_base(base);
   }

   //   cwh_auxst_dump (base);

   if ((ST_sclass(base) == SCLASS_DGLOBAL) || 
       (ST_sclass(base) == SCLASS_COMMON)) {

       /* convert the COMMON into a DGLOBAL, makeing sure the sclasses
        * are consistent, and the the ST_initialized flag is set. This
        * includes equivalence classes 
        */
       
       Set_ST_sclass(base, SCLASS_DGLOBAL); 

       cwh_data_set_init_flag(base,l_COMLIST);

       current_st = base;
       current_pos = offset;
       init_common_or_module = TRUE; 

    } else if (base != current_st) {  

      /* equivalenced item */

     cwh_data_set_init_flag(base,l_EQVLIST);
     current_st = base;
     current_pos = offset;
     init_common_or_module = TRUE; 
   }

#ifdef KEY // bug 13276
   if (ST_sclass(base) == SCLASS_FORMAL_REF)
     return;
#endif
 
   Set_ST_is_initialized(current_st);

   if (TY_kind(current_ty) == KIND_ARRAY) {
      is_struct_or_array = TRUE;
      
      current_bytesize = TY_size(TY_etype(current_ty));
   } else {
      if (TY_kind(current_ty) == KIND_STRUCT) {
	 is_struct_or_array = TRUE;
      } else {
	 is_struct_or_array = FALSE;
      }
      current_bytesize = TY_size(current_ty);
   }
   if (init_common_or_module) {
      is_struct_or_array = TRUE;
   }

   current_data_info = ST_auxst_data_info(current_st);
   if (!current_data_info) {
      current_data_info = CXX_NEW(data_info_s(current_st), Malloc_Mem_Pool);
      Set_ST_auxst_data_info(current_st,current_data_info);
   }
   current_inito = current_data_info->Get_Inito();
   return;
}
   
/*================================================================
 *
 * fei_static_subscripts
 *
 * Get the subscripts for a static initialization. The FE provides
 * subscripts with the least-contiguous subscript in 
 * static_subscripts[0] & in the ARB, the least-contiguous dimension 
 * is the lowest (first) idx.
 * 
 *
 *================================================================
 */
extern void
fei_static_subscripts( INT64 static_subscripts[STATIC_SUBSCRIPT_SIZE] )
{
   INT i;
   INT rank;
   INT64 offset;


   ARB_HANDLE arb;

   /* Create an initial start subscript position from the data given */
   DevAssert((TY_kind(current_ty)==KIND_ARRAY),("Not an array ty"));
   rank = TY_AR_ndims(Ty_Table[current_ty]);
   offset = 0;

   arb = TY_arb(Ty_Table[current_ty])[rank - 1];

   for (i=rank-1; i >= 0; i--) {
      offset += (static_subscripts[i] -
		 ARB_lbnd_val(arb))*
		 ARB_stride_val(arb);
      arb = arb[-1];
   }

   array_pos += offset;
   return;
}

/*================================================================
 * fei_static_substr
 * Get the position in a string to initialize.
 *================================================================
 */
extern void
fei_static_substr( INT32 start)
{

   /* Create an initial start subscript position from the data given */
   /* For a CHARACTER object, the lower bound is always 1 */
   array_pos += start - 1;
   return;
}

/*================================================================
 * fei_static_member
 * Get the position of a member to initialize
 *================================================================
 */
extern 
void fei_static_member (INTPTR st_idx )
{
   FLD_IDX  fld ;

   fld = (FLD_IDX ) (INTPTR)cast_to_void(st_idx);

   FLD_HANDLE f (fld);
   DevAssert((FLD_bofst(f) == 0),("Can't handle non 0 bofst"));
   current_pos += FLD_ofst(f);

   current_ty = FLD_type(f);
   if (TY_kind(current_ty) == KIND_ARRAY) {
      current_bytesize = TY_size(TY_etype(current_ty));
   } else {
      current_bytesize = TY_size(current_ty);
   }
}

/* Get the size of a TCON */
static INT32 get_TCON_size(TCON_IDX tc)
{
   TYPE_ID t;
   INT32 esize;
   if (tc == 0) {
      /* Must be doing an init of a symbol, rather than 
       * a constant. Return Pointer_Size.
       */
      esize = Pointer_Size;
   } else {
      TCON& tcon = Tcon_Table[tc];
      t = TCON_ty(tcon);
      if (t == MTYPE_STR) {
	 esize = Targ_String_Length(tcon);
      } else {
	 esize = MTYPE_size_min(t)/8;
      }
   }
   return (esize);
}

/* Get a TCON from a WHIRL node */
static TCON_IDX TCON_from_stack(void)
{
#ifdef KEY /* Bug 10177 */
   TCON_IDX tcp = 0;
#else /* KEY Bug 10177 */
   TCON_IDX tcp;
#endif /* KEY Bug 10177 */
   ST *cst;
   WN *slen;
   WN *w;
   TY_IDX ty;

   switch (cwh_stk_get_class()) {
    case STR_item:
      cwh_stk_pop_STR();
      slen = cwh_stk_pop_WN();
      WN_DELETE_Tree(slen);
      cst = cwh_stk_pop_ST();
      tcp = ST_tcon(cst);
      break;

    case PCONST_item:
      cst = (ST *) cwh_stk_pop_PCONST();
      tcp = ST_tcon(cst);
      break;

    case WN_item:
    case WN_item_whole_array:
       ty = cwh_stk_get_TY();
       w = cwh_stk_pop_WN();
       if (WNOPR(w) == OPR_INTCONST) {
	  if (ty) {
	     tcp = Enter_tcon(Host_To_Targ(TY_mtype(ty),WN_const_val(w)));
	  } else {
	     tcp = Enter_tcon(Host_To_Targ(WN_rtype(w),WN_const_val(w)));
	  }
       } else if (WNOPR(w) == OPR_CONST) {
	  tcp = Enter_tcon(Const_Val(w));
       } else {
	  DevAssert((0),("not a const node"));
       }
       WN_Delete(w);
       break;
   }
   return (tcp);
}

/*================================================================
 * get_base_and_offset(WN *wn) 
 *
 * Given a WHIRL node, return an ST and an offset
 * from the base of the ST.
 * It assumes all addressing that is done is with constant sizes, 
 * offsets, etc.
 *
 *================================================================
 */

static
b_and_o get_base_and_offset(WN *wn)
{
   b_and_o r,t1,t2;
   INT64 offset;
   INT ndim,i;
   INT64 esize;
   INT64 smult;
   WN *adim, *aindex;
   INT64 adim_val,aindex_val;

   r.base = NULL;
   r.offset = 0;
   
   switch (WN_operator(wn)) {
    case OPR_ADD:
      t1 = get_base_and_offset(WN_kid0(wn));
      t2 = get_base_and_offset(WN_kid1(wn));
      r.offset = t1.offset + t2.offset;
      if (t1.base) {
	 r.base = t1.base;
      } else if (t2.base) {
	 r.base = t2.base;
      } else {
	 DevAssert((0),("No base found"));
      }
      break;

    case OPR_SUB:
      t1 = get_base_and_offset(WN_kid0(wn));
      t2 = get_base_and_offset(WN_kid1(wn));
      r.offset = t1.offset - t2.offset;
      if (t1.base) {
	 r.base = t1.base;
      } else {
	 DevAssert((0),("No base or bad base"));
      }
      break;
     
    case OPR_INTCONST:
      r.base = NULL;
      r.offset = WN_const_val(wn);
      break;

    case OPR_LDA:
      r.base = WN_st(wn);
      r.offset = WN_offset(wn);

# if (defined(linux) || defined(BUILD_OS_DARWIN))
      /* Check for based as a symbol in common */
      while (ST_base(r.base) != r.base) {
         r.offset += ST_ofst(r.base);
         r.base = ST_base(r.base);
      }
# endif
      break;

    case OPR_ARRAYEXP:
      r = get_base_and_offset(WN_kid0(wn));
      break;

    case OPR_ARRAY:
    case OPR_ARRSECTION:
      /* Build up an offset from the ARRAY portion, and add it to the base */
      r = get_base_and_offset(WN_kid0(wn));
      esize = WN_element_size(wn);
      ndim = WN_num_dim(wn);
      offset = 0;
      
      smult = 1;
      for (i = ndim-1; i >= 0; i--) {
	 aindex = WN_array_index(wn,i);
	 adim = WN_array_dim(wn,i);
	 DevAssert((WN_operator(aindex)==OPR_INTCONST),("Non-constant index"));
	 DevAssert((WN_operator(adim)==OPR_INTCONST),("Non-constant dim"));
	 aindex_val = WN_const_val(aindex);
	 adim_val = WN_const_val(adim);
	 if (esize < 0) {
	    /* Non-contiguous */
	    offset += aindex_val * adim_val;
	 } else {
	    /* contiguous */
	    offset += aindex_val * smult;
	    smult *= adim_val;
	 }
      }
      
      /* Scale by element_size */
      if (esize > 0) {
	 offset *= esize;
      } else {
	 offset *= (-esize);
      }
      /* add into rest */
      r.offset += offset;
      break;

    case OPR_COMMA:
       r = get_base_and_offset(WN_kid1(wn));
       break;

    default:
      /* Don't know what to do here */
      DevAssert((0),("strange offset expression"));
      break;
   }
   return (r);
}


/*================================================================
 * static_simple_init_helper
 * Build up the init for a given symbol
 * dup_count - number of times to duplicate
 * stride - step for multiple inits
 * value - pointer to a TCON for a value init
 * bo - pointer to a base and offset for a symbol init
 *================================================================
 */


/* Create an initv node for use by the initializing routines */
static INITV_IDX create_initv(INITO_IDX ino, INT32 repeat, TCON_IDX tc,
			    b_and_o *bo)
{
   if (tc) {
      return (Irb_Init_Val(ino, 0, repeat, tc));
   } else {
      return (Irb_Init_Symoff(ino, 0, repeat, bo->base, bo->offset));
   }
}

static 
void static_simple_init_helper (INT64 dup_count,
				INT64 stride,
				TCON_IDX value,
				b_and_o *bo)
     
{
   INT64 byte_stride;
   INT32 init_size;
   INT64 i;
   INT64 element_offset;
   
   if (!is_struct_or_array) {
      create_initv(current_inito,1,value, bo);
      return;
   }

   if (dup_count == 0) return;
   element_offset = array_pos;

   byte_stride = stride/8;
   if (byte_stride < 0) {
      /* Need to turn this around so it runs upward */
      element_offset += byte_stride*(dup_count-1);
      byte_stride = -byte_stride;
   }

   init_size = get_TCON_size(value);
   if (byte_stride == 0) byte_stride = init_size;
   
   /* Multiple inserts */
   current_data_info->Reserve(dup_count);
   for (i=0; i < dup_count; i++) {
     current_data_info->Add_Data_Element(current_pos+element_offset,value,bo,init_size);
     element_offset += byte_stride;
   }
   
   return;
}
   

extern 
void fei_static_simple_init (INT64 dup_count,
			     INT64 stride,
			     INT32 ignore_types,
			     INT32 string_literal)
				    
{
   TCON_IDX value;
   value = TCON_from_stack();
   static_simple_init_helper(dup_count,stride,value,NULL);
}
     

/*================================================================
 * fei_static_next_simple_init
 * Build up the init for a given symbol
 *================================================================
 */
extern 
void fei_static_next_simple_init   ( INT64 bit_increment, 
                                     INT64 dup_count,
                                     INT64 init_offset,
                                     INT32 string_literal )
{
   array_pos += (init_offset >> 3);
   fei_static_simple_init(dup_count,bit_increment,0,string_literal);
}

/*================================================================
 * fei_static_simple_reloc_init
 * Build up the init for a given symbol
 *================================================================
 */

extern
void fei_static_simple_reloc_init  ( INT64 bit_offset, 
                                     INT64 dup_count,
                                     INT64 stride,
                                     INT64 bit_size,
                                     INT32 ignore_types )
{
   WN *addr;
   b_and_o value;

   /* Pull apart the address to get a symbol and an offset */
   /* Everything should be constant */
   addr = cwh_expr_operand(NULL);
   value = get_base_and_offset(addr);
   WN_DELETE_Tree(addr);
   DevAssert((value.base),("NULL base found"));
   cwh_expr_set_flags(value.base,f_T_SAVED);
   
   static_simple_init_helper(dup_count,stride,0,&value);
}


//================================================================
//
// Emit initializations for a symbol
//
//================================================================

static void emit_inits_for_symbol(ST_IDX st_idx, ST *st)
{
   DATA_INFO d;
   INITO_IDX inito;
   DATA_ELEMENTS *v;
   INT vsize,i;
   INT64 pos;
   
   INITV_IDX pad;
   INITV_IDX initv;
   INITV_IDX prev_initv;
   INT64 offset;
   INT64 init_size;
   INT64 pad_size;

   if (ST_sym_class(st) != CLASS_VAR) return;  // Only emit inits for variables

   d = ST_auxst_data_info(st);
   if (d==NULL) return;
   if (d->Get_Data_Elements()==NULL) return;

   // Get the inito for the symbol
   inito = d->Get_Inito();
   // Sort the data 
   d->sort_data(st);
   v = d->Get_Data_Elements();
     
   // v is now an array of sorted size/offset/initv triples. 
   vsize = v->size();
   if (vsize == 0) return; 
   
#ifdef DATA_DEBUG     
   printf("Emitting inits for %s ====================\n",ST_name(st));
#endif
   
   prev_initv = 0;
   pos = 0;
   for (i = 0; i < vsize; i++) {
     if (!(*v)[i].is_valid) continue;
     offset = (*v)[i].offset;
     init_size = (*v)[i].initv_size;
     initv = (*v)[i].create_initv();
      
     // Do we need to pad before? 
     pad_size = offset - pos;
     if (pad_size > 0) {
       pad = Irb_Init_Pad(0,prev_initv,pad_size);
       if (prev_initv == 0) {
	 prev_initv = Irb_Init_Block(inito,0,1);
	 Set_INITV_blk(prev_initv,pad);
       } else {
	 Set_INITV_next(prev_initv,pad);
       }
       prev_initv = pad;
     }

#ifdef DATA_DEBUG     
     printf("pad %lld, offset %lld, size %lld, next %lld\n",pad_size,
	    offset,init_size, offset + init_size);
#endif
      
     // Add the initv to the chain
     if (prev_initv == 0) {
       prev_initv = Irb_Init_Block(inito,0,1);
       Set_INITV_blk(prev_initv,initv);
     } else {
       Set_INITV_next(prev_initv,initv);
     }
     prev_initv = initv;
     pos = offset + init_size;
   }
   
   // Do we need to pad the end? 
   pad_size = TY_size(ST_type(st)) - pos;
   if (pad_size > 0) {
      pad = Irb_Init_Pad(0,prev_initv,pad_size);
   }

   // Delete the structure
   // use malloc mempool because there's direct call to delete from destructors
   // causing nested calls to operator delete 
   CXX_DELETE(d,Malloc_Mem_Pool);
   Set_ST_auxst_data_info(st,NULL);

   return;
}
   


//================================================================
//
// Emit initializations for all symbols in a symbol level
//
//================================================================

extern void
cwh_data_emit_symbol_inits(SYMTAB_IDX level)
{
   For_all(St_Table, level,&emit_inits_for_symbol);
}

/*================================================================
 *
 * cwh_data_set_init_flag
 *
 * sets ST_initialized flag on COMMON or EQUIVALENCE components.
 * also sets the base of components to that of base. Ditto CRI
 * pointer pointee pairs.
 *
 * Utility for fei_static_base.
 *
 *================================================================
 */
static void
cwh_data_set_init_flag(ST * st, enum list_name list)
{

  ITEM * el = NULL;
  
  while ((el = cwh_auxst_next_element(st,el,list)) != NULL) {

       ST * el_st = I_element(el);
       Set_ST_sclass(el_st, ST_sclass(st));
       Set_ST_is_initialized(el_st);

       ST * p_st = cwh_auxst_cri_pointee(el_st,0);

       if (p_st) {
	 Set_ST_sclass(p_st, ST_sclass(st));
	 Set_ST_is_initialized(p_st);
       }

       if (list == l_COMLIST)
	 cwh_data_set_init_flag(el_st, l_EQVLIST);
  }
}

