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
 * Module: wn_simp
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "wn.h"
#include "stab.h"
#include "wn_util.h"
#include "ir_reader.h"

#include "config.h"
#include "config_opt.h"
#include "config_targ.h"

#include "const.h"
#include "targ_const.h"

#ifdef BACK_END
#include "opt_alias_interface.h"
#endif

#include "wn_simp.h"

#if defined(KEY) && defined(Is_True_On)
#include "config_opt.h"
#endif

#ifdef BACK_END
BOOL WN_Simp_Fold_ILOAD = TRUE;
#else
BOOL WN_Simp_Fold_ILOAD = FALSE;
#endif

BOOL WN_Simp_Fold_LDA = FALSE;

#ifdef KEY
BOOL WN_Simp_Rsqrt_Newton_Raphson = TRUE;
#endif

/* Parent maps from LNO and anyone else who wants it */
WN_MAP WN_SimpParentMap = WN_MAP_UNDEFINED;

#define TRACEFILE TFile


/* To tell wn_simp_code.h that it's working on WHIRL */
#define WN_SIMP_WORKING_ON_WHIRL

/* Definitions for wn_simp_code.h */
/* Type definition */

typedef WN * simpnode;

/* Accessors */
#define SIMPNODE_operator(x) WN_operator(x)
#define SIMPNODE_rtype(x) WN_rtype(x)
#define SIMPNODE_desc(x) WN_desc(x)
#define SIMPNODE_opcode WN_opcode
#define SIMPNODE_load_offset WN_load_offset
#define SIMPNODE_cvtl_bits WN_cvtl_bits
#define SIMPNODE_st WN_st
#define SIMPNODE_st_idx WN_st_idx
#define SIMPNODE_ty WN_ty
#define SIMPNODE_object_ty WN_object_ty
#define SIMPNODE_load_addr_ty WN_load_addr_ty
#define SIMPNODE_kid0 WN_kid0
#define SIMPNODE_kid1 WN_kid1
#define SIMPNODE_kid WN_kid
#define SIMPNODE_element_size WN_element_size
#define SIMPNODE_idname_offset WN_idname_offset
#define SIMPNODE_lda_offset WN_lda_offset
#define SIMPNODE_label_number WN_label_number
#define SIMPNODE_num_dim WN_num_dim
#define SIMPNODE_array_base WN_array_base
#define SIMPNODE_array_index WN_array_index
#define SIMPNODE_array_dim WN_array_dim
#define SIMPNODE_intrinsic WN_intrinsic
#define SIMPNODE_kid_count WN_kid_count
#define SIMPNODE_kid WN_kid
#define SIMPNODE_const_val WN_const_val
#define SIMPNODE_fconst_val Const_Val
#define SIMPNODE_field_id WN_field_id               // get the field id
#define SIMPNODE_i_field_id WN_field_id             // get the field id
#define SIMPNODE_bit_offset WN_bit_offset           // get the bit offset
#define SIMPNODE_i_bit_offset WN_bit_offset         // get the bit offset
#define SIMPNODE_enable Enable_WN_Simp
#define SIMPNODE_op_bit_offset WN_bit_offset
#define SIMPNODE_op_bit_size WN_bit_size

/* Functions */

#define SIMPNODE_SimpCreateExp1 WN_SimpCreateExp1
#define SIMPNODE_SimpCreateExp2 WN_SimpCreateExp2
#define SIMPNODE_SimpCreateExp3 WN_SimpCreateExp3
#define SIMPNODE_SimpCreateCvtl WN_SimpCreateCvtl
#define SIMPNODE_SimpCreateExtract WN_SimpCreateExtract
#define SIMPNODE_SimpCreateDeposit WN_SimpCreateDeposit
#define SIMPNODE_TREE_DELETE WN_DELETE_Tree
#define SIMPNODE_DELETE WN_Delete
#define SIMPNODE_CopyNode WN_CopyNode
#define SIMPNODE_CreateIntconst WN_CreateIntconst
#define SIMPNODE_CreateFloatconstFromTcon Make_Const
#ifdef TARG_X8664
#define SIMPNODE_CreateSIMDconstFromTcon Make_Const
#endif
#define SIMPNODE_Simplify_Initialize WN_Simplify_Initialize
#define SIMPNODE_Compare_Symbols WN_Compare_Symbols
#define SIMPNODE_is_volatile WN_Is_Volatile_Mem
/* externally visible routines. These three are defined in wn_simp_code.h. 
 * They need a name defined here and in whatever external interface file 
 * exists for the routine
 */
#define SIMPNODE_SimplifyExp1 WN_SimplifyExp1
#define SIMPNODE_SimplifyExp2 WN_SimplifyExp2
#define SIMPNODE_SimplifyExp3 WN_SimplifyExp3
#define SIMPNODE_SimplifyCvtl WN_SimplifyCvtl
#define SIMPNODE_SimplifyIntrinsic WN_SimplifyIntrinsic
#define SIMPNODE_SimplifyIload WN_SimplifyIload
#define SIMPNODE_SimplifyIstore WN_SimplifyIstore
#define SIMPNODE_Simp_Compare_Trees WN_Simp_Compare_Trees

static void show_tree(OPCODE opc, WN *k0, WN *k1, WN *r)
{
   fprintf(TRACEFILE,"\nBefore:\n");
   fdump_tree(TRACEFILE,k0);
   if (OPCODE_operator(opc) != OPR_CVTL) {
      if (k1)
	fdump_tree(TRACEFILE,k1); 
      fprintf(TRACEFILE,"%s\n",OPCODE_name(opc));
   } else
      fprintf(TRACEFILE,"%s %d\n",OPCODE_name(opc),(INT) (INTPS) k1);
   fprintf(TRACEFILE,"=====\nAfter:\n");
   fdump_tree(TRACEFILE,r);
   fprintf(TRACEFILE,
	   "-----------------------------------------------------\n");
}


/* Walk a tree, simplifying from the bottom up. For operators
 * that the simplifier doesn't know how to deal with, simplify and replace the
 * children. For those that it does, simplify the children, then try and 
 * simplify the argument with new children.
 */
WN *WN_Simplify_Tree(WN *t, ALIAS_MANAGER *alias_manager)
{
   OPCODE op;
   OPERATOR opr;
   WN *k0, *k1, *k2, *r=NULL, *temp, *result, *next, *prev;
   INT16 numkids;
   INT32 i;

   numkids = WN_kid_count(t);
   op = WN_opcode(t);
   opr = OPCODE_operator(op);
   result = t;
   
   if (op == OPC_BLOCK) {
      result = t;
      r = WN_first(t);
      while (r) {
	 prev = WN_prev(r);
	 next = WN_next(r);
	 temp = WN_Simplify_Tree(r);
	 if (temp != r) {
	    /* a simplification happened */ 
	    WN_next(temp) = next;
	    WN_prev(temp) = prev;
	    if (next) WN_prev(next) = temp;
	    if (prev) WN_next(prev) = temp;
	    if (WN_first(t) == r) WN_first(t) = temp;
	    if (WN_last(t) == r) WN_last(t) = temp;
	 }
	 r = next;
      }
   } else if (opr == OPR_ILOAD) {
      k0 = WN_Simplify_Tree(WN_kid0(t));
      r = WN_SimplifyIload(op,WN_load_offset(t),WN_ty(t),WN_field_id(t),WN_load_addr_ty(t),k0);
      if (r) {
#ifdef BACK_END
	 if (alias_manager) {
	    Copy_alias_info(alias_manager,t,r);
	 }
#endif
	 WN_Delete(t);
	 result = r;
      } else {
	 WN_kid0(t) = k0;
	 result = t;
      }
   } else if (opr == OPR_ISTORE) {
      k0 = WN_Simplify_Tree(WN_kid0(t));
      k1 = WN_Simplify_Tree(WN_kid1(t));
      r = WN_SimplifyIstore(op,WN_load_offset(t),WN_ty(t),WN_field_id(t),k0,k1);
      if (r) {
#ifdef BACK_END
	 if (alias_manager) {
	    Copy_alias_info(alias_manager,t,r);
	 }
#endif
	 WN_Delete(t);
	 result = r;
      } else {
	 WN_kid0(t) = k0;
	 WN_kid1(t) = k1;
	 result = t;
      }
   } else if (opr == OPR_INTRINSIC_OP) {
      for (i=0; i < numkids; i++) {
	 WN_kid(t,i) = WN_Simplify_Tree(WN_kid(t,i));
      }
      r = WN_SimplifyIntrinsic(op, WN_intrinsic(t), numkids, &WN_kid0(t));
      if (r) {
	 WN_Delete(t);
	 result = r;
      } else {
	 result = t;
      }
   } else if (opr == OPR_IO_ITEM) {
      // For IO_ITEM, just simplify the kids
      for (i=0; i < numkids; i++) {
	 WN_kid(t,i) = WN_Simplify_Tree(WN_kid(t,i));
      }
      result = t;
   } else if (numkids == 1) {
      k0 = WN_Simplify_Tree(WN_kid0(t));

      if (WN_operator(t) != OPR_CVTL) {
	 r = WN_SimplifyExp1(op, k0);
      } else {
	 r = WN_SimplifyCvtl(op, WN_cvtl_bits(t),k0);
      }
      if (r) {
	 WN_Delete(t);
	 result = r;
      } else {
	 WN_kid0(t) = k0;
	 result = t;
      }
   } else if (numkids == 2) {
      k0 = WN_Simplify_Tree(WN_kid0(t));
      k1 = WN_Simplify_Tree(WN_kid1(t));
      r = WN_SimplifyExp2(op, k0, k1);
      if (r) {
	 WN_Delete(t);
	 result = r;
      } else {
	 WN_kid0(t) = k0;
	 WN_kid1(t) = k1;
	 result = t;
      }
   } else if (numkids == 3) {
      k0 = WN_Simplify_Tree(WN_kid0(t));
      k1 = WN_Simplify_Tree(WN_kid1(t));
      k2 = WN_Simplify_Tree(WN_kid(t,2));
      r = WN_SimplifyExp3(op, k0, k1, k2);
      if (r) {
	 WN_Delete(t);
	 result = r;
      } else {
	 WN_kid0(t) = k0;
	 WN_kid1(t) = k1;
	 WN_kid(t,2) = k2;
	 result = t;
      }
   } else {
      for (i=0; i < numkids; i++) {
	 WN_kid(t,i) = WN_Simplify_Tree(WN_kid(t,i));
      }
      result = t;
   }

   /* Update parent pointers */
   if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      numkids = WN_kid_count(result);
      for (i=0; i < numkids; i++) {
	 WN_MAP_Set(WN_SimpParentMap, WN_kid(result,i), (void *) result);
      }
   }
   
   return (result);
}

/* Assume that all children are already simplified, rebuild a tree using the new children.
 * This only applies itself to a subset of expression nodes.
 */
WN *WN_Simplify_Rebuild_Expr_Tree(WN *t,ALIAS_MANAGER *alias_manager)
{
   OPCODE op;
   OPERATOR opr;
   WN *k0, *k1, *k2, *r=NULL, *result;
   INT16 numkids;
# if defined(KEY) && defined(Is_True_On)
   static INT cur_idx = 0;
# endif

   op = WN_opcode(t);
   if (!OPCODE_is_expression(op)) return (t);

   numkids = WN_kid_count(t);
   opr = OPCODE_operator(op);
   result = t;

   if (opr == OPR_ILOAD) {
      k0 = WN_kid0(t);
# if defined (KEY) && defined (Is_True_On)
      if (Enable_WN_Simp_Expr_Limit == -1 || (Enable_WN_Simp_Expr_Limit != -1 && cur_idx < Enable_WN_Simp_Expr_Limit))
# endif
      r = WN_SimplifyIload(op,WN_load_offset(t),WN_ty(t),WN_field_id(t),WN_load_addr_ty(t),k0);
      if (r) {
# ifdef BACK_END
	 if (alias_manager) {
	    Copy_alias_info(alias_manager,t,r);
	 }
# endif
        WN_CopyMap(r, WN_MAP_ALIAS_CGNODE, t);
	 WN_Delete(t);
	 result = r;
# if defined (KEY) && defined (Is_True_On)
         cur_idx ++;
# endif
      } else {
	 result = t;
      }
   } else if (opr == OPR_INTRINSIC_OP) {
# if defined (KEY) && defined (Is_True_On)
      if (Enable_WN_Simp_Expr_Limit == -1 || (Enable_WN_Simp_Expr_Limit != -1 && cur_idx < Enable_WN_Simp_Expr_Limit))
# endif
      r = WN_SimplifyIntrinsic(op, WN_intrinsic(t), numkids, &WN_kid0(t));
      if (r) {
        WN_CopyMap(r, WN_MAP_ALIAS_CGNODE, t);
	 WN_Delete(t);
	 result = r;
# if defined (KEY) && defined (Is_True_On)
         cur_idx ++;
# endif
      } else {
	 result = t;
      }
   } else if (numkids == 1) {
      k0 = WN_kid0(t);

      if (WN_operator(t) != OPR_CVTL) {
          if (WN_operator(t) == OPR_EXTRACT_BITS) {
              r = WN_SimplifyExp1(op,t);
          } else {
# if defined (KEY) && defined (Is_True_On)
            if (Enable_WN_Simp_Expr_Limit == -1 || (Enable_WN_Simp_Expr_Limit != -1 && cur_idx < Enable_WN_Simp_Expr_Limit))
# endif
        	 r = WN_SimplifyExp1(op, k0);
          }
      } else {
# if defined (KEY) && defined (Is_True_On)
        if (Enable_WN_Simp_Expr_Limit == -1 || (Enable_WN_Simp_Expr_Limit != -1 && cur_idx < Enable_WN_Simp_Expr_Limit))
# endif
	 r = WN_SimplifyCvtl(op, WN_cvtl_bits(t),k0);
      }
      if (r) {
        WN_CopyMap(r, WN_MAP_ALIAS_CGNODE, t);
	 WN_Delete(t);
	 result = r;
# if defined (KEY) && defined (Is_True_On)
         cur_idx ++;
# endif
      } else {
	 WN_kid0(t) = k0;
	 result = t;
      }
   } else if (numkids == 2) {
      k0 = WN_kid0(t);
      k1 = WN_kid1(t);
# if defined (KEY) && defined (Is_True_On)
      if (Enable_WN_Simp_Expr_Limit == -1 || (Enable_WN_Simp_Expr_Limit != -1 && cur_idx < Enable_WN_Simp_Expr_Limit))
# endif
#ifdef KEY // bug 13507
      if (opr != OPR_PAIR)
#endif
      r = WN_SimplifyExp2(op, k0, k1);
      if (r) {
        WN_CopyMap(r, WN_MAP_ALIAS_CGNODE, t);
	 WN_Delete(t);
	 result = r;
# if defined (KEY) && defined (Is_True_On)
         cur_idx ++;
# endif
      } else {
	 result = t;
      }
   } else if (numkids == 3) {
      k0 = WN_kid0(t);
      k1 = WN_kid1(t);
      k2 = WN_kid(t,2);
# if defined (KEY) && defined (Is_True_On)
      if (Enable_WN_Simp_Expr_Limit == -1 || (Enable_WN_Simp_Expr_Limit != -1 && cur_idx < Enable_WN_Simp_Expr_Limit))
# endif
      r = WN_SimplifyExp3(op, k0, k1, k2);
      if (r) {
        WN_CopyMap(r, WN_MAP_ALIAS_CGNODE, t);
	 WN_Delete(t);
	 result = r;
# if defined (KEY) && defined (Is_True_On)
         cur_idx ++;
# endif
      } else {
	 result = t;
      }
   } else {
      result = t;
   }
   
   return (result);
}

/* Allow the simplifier to be turned on and off */

BOOL WN_Simplifier_Enable(BOOL enable) 
{
   BOOL r = Enable_WN_Simp;
   Enable_WN_Simp = enable;
   return (r);
}

/* Utility procedure which does a comparison on two symbols */
static INT32 WN_Compare_Symbols(simpnode t1, simpnode t2)
{
   ST_IDX s1 = SIMPNODE_st_idx(t1);
   ST_IDX s2 = SIMPNODE_st_idx(t2);

   if (s1 < s2)
       return -1;
   else if (s1 > s2)
       return 1;
   else
       return 0;
}

/************  The code is here *******************/

#include "wn_simp_code.h"

/**************************************************/

/* Things which need to be written by the user */

/* Interface to WN_CreateExp3, checking for parent updates */
static simpnode WN_SimpCreateExp3(OPCODE opc, 
				  simpnode k0, simpnode k1, simpnode k2)
{
   simpnode  wn;
   wn = WN_SimplifyExp3(opc, k0, k1, k2);
   if (!wn) {
      wn = WN_Create(opc,3);
      WN_kid0(wn) = k0;
      WN_kid1(wn) = k1;
      WN_kid(wn,2) = k2;
      if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
	 WN_MAP_Set(WN_SimpParentMap, k0, (void *) wn);
	 WN_MAP_Set(WN_SimpParentMap, k1, (void *) wn);
	 WN_MAP_Set(WN_SimpParentMap, k2, (void *) wn);
      }
   }
   
   return(wn);
}

/* Interface to WN_CreateExp2, checking for parent updates */
static simpnode  WN_SimpCreateExp2(OPCODE opc, simpnode k0, simpnode k1)
{
   simpnode  wn;
   wn = WN_SimplifyExp2(opc, k0, k1);
   if (!wn) {
      wn = WN_Create(opc,2);
      WN_kid0(wn) = k0;
      WN_kid1(wn) = k1;
      if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
	 WN_MAP_Set(WN_SimpParentMap, k0, (void *) wn);
	 WN_MAP_Set(WN_SimpParentMap, k1, (void *) wn);
      }
   }
   
   return(wn);
}

/* Interface to WN_CreateExp1, checking for parent updates */
static simpnode  WN_SimpCreateExp1(OPCODE opc, simpnode k0)
{
   simpnode wn;
   wn = WN_SimplifyExp1(opc, k0);
   if (!wn) {
      wn = WN_Create(opc,1);
      WN_kid0(wn) = k0;
      if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
	 WN_MAP_Set(WN_SimpParentMap, k0, (void *) wn);
      }
   }
   
   return(wn);
}

/* Interface to WN_CreateCvtl, checking for parent updates */
static simpnode  WN_SimpCreateCvtl(OPCODE opc, INT16 bits, simpnode k0)
{
   simpnode wn;
   wn = WN_SimplifyCvtl(opc, bits, k0);
   if (!wn) {
      wn = WN_Create(opc,1);
      WN_kid0(wn) = k0;
      WN_cvtl_bits(wn) = bits;
      if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
	 WN_MAP_Set(WN_SimpParentMap, k0, (void *) wn);
      }
   }
   
   return(wn);
}

static simpnode WN_SimpCreateExtract(OPCODE opc, INT16 boffset, INT16 bsize, simpnode k0)
{
   simpnode wn;
   wn = WN_Create(opc,1);
   WN_kid0(wn) = k0;
   WN_set_bit_offset_size(wn,boffset,bsize);
   if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
     WN_MAP_Set(WN_SimpParentMap, k0, (void *) wn);
   }
   
   return(wn);
}

static simpnode WN_SimpCreateDeposit(OPCODE opc, INT16 boffset, INT16 bsize, simpnode k0, simpnode k1)
{
   simpnode wn;
   wn = WN_Create(opc,2);
   WN_kid0(wn) = k0;
   WN_kid1(wn) = k1;
   WN_set_bit_offset_size(wn,boffset,bsize);
   if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
     WN_MAP_Set(WN_SimpParentMap, k0, (void *) wn);
     WN_MAP_Set(WN_SimpParentMap, k1, (void *) wn);
   }
   
   return(wn);
}


static void SIMPNODE_Simplify_Initialize( void )
{
  trace_rules = (Get_Trace(TP_WHIRLSIMP, 1) != 0);
  trace_trees = (Get_Trace(TP_WHIRLSIMP, 2) != 0);
  SIMPNODE_simp_initialized = TRUE;
}
