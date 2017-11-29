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



// -*-C++-*-
// ====================================================================
// ====================================================================
//
// Module: al_ref.cxx
// $Revision: 1.5 $
// $Date: 04/12/21 14:57:11-08:00 $
// $Author: bos@eng-25.internal.keyresearch.com $
// $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.al_ref.cxx $
//
// Revision history:
//  dd-mmm-95 - Original Version
//
// Description:
//
//     Representation of references to reshaped arrays.
// 
// ====================================================================
// ====================================================================
//

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.al_ref.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "lnopt_main.h"
#include "config_targ.h"
#include "erbe.h"
#include "lnoutils.h"
#include "lwn_util.h"
#include "snl_utils.h"
#include "lego_pragma.h"
#include "lego_util.h"
#include "lego_opts.h"
#include "array_lower.h"
#include "al_loop.h"
#include "al_ref.h"
#include "move.h"

/* ========================================================================
   Public Function Declarations
   ======================================================================== */

extern mBOOL Single_Loop_Coeff(ACCESS_VECTOR *av, INT64 *stride, INT64 *offset,
                               mINT32 *depth);

extern void Try_Hoist_Expression (WN* expr_wn);

/* ========================================================================
   Private Function Declarations
   ======================================================================== */

static WN *Get_Call_Parent(WN *curr_nd);
static TY_IDX Get_Callee_TY(WN *call_nd, ST *array_st);
static INT Find_Param_Num(WN *array_param, WN *call_nd);
static void Fixup_DU_Under_IO (WN* ref, WN* io_wn);
static WN* Get_IO_Parent (WN* ref);
static void Hoist_Reshaped_Reference (WN* array_wn);
static WN* Lower_Reshaped_Reference (WN* array_wn);


/* ========================================================================
   Class Implementations
   ======================================================================== */

ARRAY_LOWER_REF::ARRAY_LOWER_REF(WN *array_ref, 
				 DISTR_ARRAY *dact, 
				 ARRAY_LOWER_LOOP *parent_loop,
				 mBOOL is_param /* = FALSE */)
{
  if (is_param) {
    Is_True(array_ref &&
            ((WN_operator(array_ref) == OPR_LDA) ||
             (WN_operator(array_ref) == OPR_LDID)),
      ("ARRAY_LOWER_REF constuctor: bad array_ref for array param\n"));
  } else {
    Is_True(array_ref && (WN_operator(array_ref) == OPR_ARRAY), 
      ("ARRAY_LOWER_REF constructor: bad array_ref for array expr\n"));
  }

  Is_True(dact, ("ARRAY_LOWER_REF constructor: dact is NULL\n"));
  Is_True(parent_loop, ("ARRAY_LOWER_REF constructor: parent_loop is NULL\n"));

  _array_ref = array_ref;
  _dact = dact;
  _parent_loop = parent_loop;
  _is_param = is_param;
}


ARRAY_LOWER_REF::~ARRAY_LOWER_REF(void)
{ }


void 
ARRAY_LOWER_REF::Lower_Ref(void)
{
  if (Is_Param()) Lower_Array_Param();
  else            Lower_Array_Expr();
}


/*
 * Get the loop corresponding to the array access, e.g. for the reference
 * a[3i+5] return the i loop.  Also sets the stride and offset for the
 * reference. If the array access is not in a form that we can handle -- that is
 * a linear function plus a constant -- or the corresponding loop can't be found,
 * then NULL is returned.
 */
ARRAY_LOWER_LOOP 
*ARRAY_LOWER_REF::Get_Ref_Loop(INT dim, 
			       INT64 *ref_stride,
			       INT64 *ref_offset)
{
  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, Array_Ref());
  if (aa == NULL) {
    DevWarn ("Get_Ref_Loop: No ACCESS_ARRAY for ref\n");
    return NULL;
  }
  ACCESS_VECTOR *av = aa->Dim(dim);

  // Check if the array access is an access we can handle -- 
  // a linear function plus a constant, e.g. a[3i+5] 
  //
  *ref_offset = 0;
  *ref_stride = 0;
  mINT32 ref_depth;
  if (!Single_Loop_Coeff(av, ref_stride, ref_offset, &ref_depth)) 
    return NULL;

  // Get the corresponding loop for the array access, 
  // e.g. the 'i' loop in a[3i+5] 
  //
  ARRAY_LOWER_LOOP *ref_loop = 
    Parent_Loop()->Get_Ancestor(av->Nest_Depth() - ref_depth - 1);
  Is_True(ref_loop, 
	  ("Get_Ref_Loop: Ref loop not found at depth %d\n", ref_depth));

  DO_LOOP_INFO *loop_info = Get_Do_Loop_Info(ref_loop->Doloop());
  Is_True(loop_info->Depth == ref_depth, 
	  ("Get_Ref_Loop: Mismatch in loop depths\n"));

  return ref_loop;
}


/***********************************************************************
 *
 * Return an I/O parent if this reference is underneath I/O,
 * NULL otherwise.
 *
 ***********************************************************************/
static WN* Get_IO_Parent (WN* ref) {
  WN* parent = LWN_Get_Parent(ref);
  while (parent) {
    if (WN_operator(parent) == OPR_IO) return parent;
    if (OPCODE_is_stmt(WN_opcode(parent))) return NULL;
    parent = LWN_Get_Parent(parent);
  }
  return NULL;
}

/***********************************************************************
 *
 * Given a lowered array reference underneath an I/O node, 
 * move all DU chains to the I/O node.
 *
 ***********************************************************************/
static void Fixup_DU_Under_IO (WN* ref, WN* io_wn) {
  USE_LIST* use_list = Du_Mgr->Du_Get_Use(ref);
  USE_LIST_ITER use_iter(use_list);
  DU_NODE* node;
  // walk all the uses from the current node "ref", and replace
  // their defs to be io_wn
  for (node = use_iter.First(); !use_iter.Is_Empty();
                                node = use_iter.Next()) {
    WN* uwn = node->Wn();
    Du_Mgr->Delete_Def_Use (ref, uwn);
    Du_Mgr->Add_Def_Use (io_wn, uwn);
  }

  DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(ref);
  DEF_LIST_ITER def_iter(def_list);
  // walk all the defs for the current node "ref", and replace
  // their uses to be io_wn
  for (node = def_iter.First(); !def_iter.Is_Empty();
                                node = def_iter.Next()) {
    WN* dwn = node->Wn();
    Du_Mgr->Delete_Def_Use (dwn, ref);
    Du_Mgr->Add_Def_Use (dwn, io_wn);
  }

  // now recurse on kids
  for (INT i=0; i<WN_kid_count(ref); i++)
    Fixup_DU_Under_IO (WN_kid(ref, i), io_wn);
}

/*
 * Given an array access and the distribution, translate the array access.
 * For each array access, two new array access are created: one
 * is an index into the array of processors and the other is an index
 * into the local processor's memory.  
 * The array dimensions are processed in the same order as the
 * original access.  
 */
void 
ARRAY_LOWER_REF::Lower_Array_Expr(void)
{
  TYPE_ID type;
  OPCODE op;

  DISTR_INFO *dinfo = Dact()->Dinfo();

  // 
  // Count number of each kind of distribution
  //
  INT i;
  INT num_block = 0;        
  INT num_cyclic = 0;
  INT num_blkcyc = 0;
  INT num_star = 0;
  INT num_dim = dinfo->Num_Dim();

  for (i = 0; i < num_dim; i++) {
    switch (Dact()->Dims()[i].Distr_Type()) {
      case DISTRIBUTE_BLOCK:  
        num_block++;
        break;
      case DISTRIBUTE_STAR: 
	num_star++;
        break;
      case DISTRIBUTE_CYCLIC_CONST:
        if (Dact()->Dims()[i].Chunk_Const_Val() == 1) num_cyclic++;
        else num_blkcyc++;
        break;
      case DISTRIBUTE_CYCLIC_EXPR:
        num_blkcyc++;
        break;
    }
  }

  INT num_distr = num_block + num_cyclic + num_blkcyc;
  INT num_local = num_distr + num_blkcyc + num_star;

  if (num_distr == 0) return;

  //
  // Create new array nodes and set indices and bounds
  //
  op = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
  WN *distr_addr = WN_Create(op, 2*num_distr + 1);   
  WN *local_addr = WN_Create(op, 2*num_local + 1);  

  INT curr_local = 0;
  INT curr_distr = 0;
  for (i = 0; i < num_dim; i++) {
    switch (Dact()->Dims()[i].Distr_Type()) {

      // BLOCK distribution
      //
      case DISTRIBUTE_BLOCK: {
        Create_Block_Dim(i, distr_addr, local_addr, &curr_distr, &curr_local);
        break;
      }

      // * Distribution
      //
      case DISTRIBUTE_STAR: {
        Create_Star_Dim(i, local_addr, &curr_local);
        break;
      }

      case DISTRIBUTE_CYCLIC_CONST: {
        if (Dact()->Dims()[i].Chunk_Const_Val() == 1) {
          //
          // CYCLIC(1) Distribution
          //
          Create_Cyclic_Dim(i, distr_addr, local_addr, &curr_distr, &curr_local);
        } else {
          //
          // CYCLIC(k) Distribution, k is constant
          //
          TYPE_ID type = 
            WN_rtype(WN_array_index(Array_Ref(), i));
          WN *chunk_nd = 
            LWN_Make_Icon(type, Dact()->Dims()[i].Chunk_Const_Val());

          Create_Blkcyc_Dim(i, distr_addr, local_addr, &curr_distr, 
                            &curr_local, chunk_nd);
        }
        break;
      }

      // CYCLIC(b) Distribution, b is a variable
      //
      case DISTRIBUTE_CYCLIC_EXPR: {
        WN *chunk_nd = Dact()->Chunksize (i);
        Create_Blkcyc_Dim(i, distr_addr, local_addr, &curr_distr, 
			  &curr_local, chunk_nd);
      }
    }
  }

  // 
  // Fill in the array bases and element sizes
  //
  ST *array_st = dinfo->Array_ST();
  TY_IDX distr_ty_idx = ST_type(array_st);
  Is_True((TY_kind(distr_ty_idx) == KIND_POINTER) &&
          (TY_ptr_as_array(distr_ty_idx)),
    ("Lower_Array_Expr: Bad type for reshaped array %s\n", ST_name(array_st)));

  TY_IDX local_ty_idx = TY_pointed(distr_ty_idx);
  Is_True((TY_kind(local_ty_idx) == KIND_POINTER) &&
          (TY_ptr_as_array(local_ty_idx)),
    ("Lower_Array_Expr: Bad type for reshaped array %s\n", ST_name(array_st)));

  op = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  WN *distr_array_base = dinfo->Load_Distr_Array ();
  WN_array_base(distr_addr) = distr_array_base;

  // Handle alias information and DU chains for base of the reshaped array
  //
  Du_Mgr->Ud_Get_Def(distr_array_base)->Set_loop_stmt(NULL);

  LWN_Set_Parent(distr_array_base, distr_addr);

  op = OPCODE_make_op(OPR_ILOAD, Pointer_type, Pointer_type);
  WN_array_base(local_addr) = LWN_CreateIload(op, 0, local_ty_idx, 
		distr_ty_idx,  distr_addr);

  // Create new alias for array dereferences
  //
  WN* array_ptr_alias_wn = dinfo->Get_Array_Ptr_Alias_WN();
  if (array_ptr_alias_wn) 
    Copy_alias_info (Alias_Mgr, array_ptr_alias_wn, WN_array_base(local_addr));
  else {
    Create_unique_pointer_alias (Alias_Mgr, array_st, NULL,
                                 WN_array_base(local_addr));
    WN* copy_wn = LWN_Copy_Tree (WN_array_base(local_addr));
    Copy_alias_info (Alias_Mgr, WN_array_base(local_addr), copy_wn);
    dinfo->Set_Array_Ptr_Alias_WN (copy_wn);
  }
  LWN_Set_Parent(WN_array_base(local_addr), local_addr);

  WN_element_size(distr_addr) = Pointer_Size;
  WN_element_size(local_addr) = TY_size(TY_pointed(local_ty_idx));
  
  Replace_WN(_array_ref, local_addr);
  LWN_Delete_Tree(_array_ref);
  _array_ref = local_addr;

  if (LNO_enabled && !Get_Trace(TP_LNOPT2, TT_LEGO_DISABLE_HOIST)) {

    // Build access vectors, since lowering will need to copy-tree
    //
    DOLOOP_STACK do_stack(LEGO_pool);
    Build_Doloop_Stack(LWN_Get_Parent(distr_addr), &do_stack);
    LNO_Build_Access(distr_addr, &do_stack, LEGO_pool);
    LNO_Build_Access(local_addr, &do_stack, LEGO_pool);

    local_addr = Lower_Reshaped_Reference (local_addr);
    distr_addr = WN_kid0(WN_array_base(local_addr));

    // Build access vectors again, since hoisting will need to copy-tree
    //
    Build_Doloop_Stack(LWN_Get_Parent(distr_addr), &do_stack);
    LNO_Build_Access(distr_addr, &do_stack, LEGO_pool);
    LNO_Build_Access(local_addr, &do_stack, LEGO_pool);

    Hoist_Reshaped_Reference (local_addr);
    _array_ref = local_addr;
  }

  // Update access vectors
  //
  DOLOOP_STACK do_stack(LEGO_pool);
  Build_Doloop_Stack(LWN_Get_Parent(distr_addr), &do_stack);
  LNO_Build_Access(distr_addr, &do_stack, LEGO_pool);
  LNO_Build_Access(local_addr, &do_stack, LEGO_pool);

  {
    // fixup DU-chains if underneath I/O
    WN* io_wn = Get_IO_Parent(_array_ref);
    if (io_wn) Fixup_DU_Under_IO (_array_ref, io_wn);
  }
}

/* Given an array parameter, translate the array access.
 * Change an LDA of the array to an LDID -- instead of passing the address
 * of the array, pass the base directly. 
 */
void ARRAY_LOWER_REF::Lower_Array_Param(void)
{
  INT i;
  DISTR_INFO *dinfo = Dact()->Dinfo();

  WN *call_nd = Get_Call_Parent(Array_Ref());
  Is_True(call_nd, ("Lower_Array_Param: No call parent for array param\n"));

  ST *array_st = dinfo->Array_ST();
  TY_IDX distr_ty = ST_type(array_st);

  OPCODE op = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  WN *distr_array_base = dinfo->Load_Distr_Array();

  // Update the type of the callee function
  //
  TY_IDX callee_ty = Get_Callee_TY(call_nd, array_st);
  INT param_num = Find_Param_Num(Array_Ref(), call_nd);
  Is_True(param_num >= 0, ("Lower_Array_Param: Bad parameter number\n")); 

  if (callee_ty) {
#ifdef _NEW_SYMTAB
    TYLIST_IDX curr_ty = TY_parms(callee_ty);
#else
    struct tylist *curr_ty = FTI_parms(TY_ftinfo(callee_ty));
#endif
    if (curr_ty) {
      for (i = 0; i < param_num; i++) {
        curr_ty = TYLIST_next(curr_ty);
        // sometimes the prototype may have fewer formals
        // than the actuals we're passing. in which case don't do anything.
        if (curr_ty == TY_IDX_ZERO) break;
      }
#ifdef _NEW_SYMTAB
      Set_TYLIST_type(curr_ty,distr_ty);
#else
      if (curr_ty) TYLIST_item(curr_ty) = distr_ty;
#endif
    }
    else if (WN_operator(call_nd) == OPR_ICALL) {
      ErrMsgSrcpos(EC_LNO_Generic, WN_Get_Linenum(call_nd),
                   "Reshaped array passed to function pointer\n");
    }
  }

  // Update the type in the parm node
  //
  WN* parm_wn = LWN_Get_Parent(Array_Ref());
  FmtAssert (parm_wn && (WN_operator(parm_wn) == OPR_PARM),
             ("Parent of actual should be OPR_PARM"));
  WN_set_ty(parm_wn, distr_ty);
  
  
  // Handle alias information and DU chains for base of the reshaped array
  //
  Du_Mgr->Ud_Get_Def(distr_array_base)->Set_loop_stmt(NULL);

  Replace_WN(_array_ref, distr_array_base);
  LWN_Delete_Tree(_array_ref);
  _array_ref = distr_array_base;
}


/* 
 * Lower array dimension with block distribution
 */
void
ARRAY_LOWER_REF::Create_Block_Dim(INT dim,
				  WN *distr_addr,
				  WN *local_addr,
				  INT *curr_distr,
				  INT *curr_local)
{
  if (!disable_divmod_opts && Is_Block_Dim_Local(dim)) {
    Create_Local_Block_Dim(dim, distr_addr, local_addr,
			   curr_distr, curr_local);
  } else {
    Create_Remote_Block_Dim(dim, distr_addr, local_addr, 
			    curr_distr, curr_local); 
  }
}


/* 
 * Returns TRUE if it the array access is local, FALSE otherwise.
 */
mBOOL
ARRAY_LOWER_REF::Is_Block_Dim_Local(INT dim)
{
  INT64 ref_stride;
  INT64 ref_offset;

  if (!disable_rr_maps) {
    RR_INFO* rri = Get_RR_Map(Array_Ref());
    if (rri && rri->Dim(dim)->Do_Loop()) return TRUE;
    return FALSE;
  }

  ARRAY_LOWER_LOOP *ref_loop = Get_Ref_Loop(dim, &ref_stride, &ref_offset);
  if (!ref_loop) return FALSE;

  LEGO_INFO *lego_info = ref_loop->Get_Lego_Info();
  if (!lego_info || lego_info->Is_Too_Messy()) return FALSE;

  SYMBOL *affinity_array = lego_info->Array();
  DISTR_ARRAY *affinity_dact = Lookup_DACT(affinity_array->St());
 
  // If the affinity array and the current array have equivalent distributions
  // and the offset is within the peeled sections, then the access is local
  // 
  if (Dact()->DACT_Equiv(affinity_dact, dim, lego_info->Dim_Num())) {

    if (ref_stride != lego_info->Stride()) return FALSE;

    INT64 step = lego_info->Get_Local_Step(ref_loop->Doloop());
    if (step <= 0) return FALSE;

    ref_offset -= lego_info->Offset();
    if (ref_offset >= 0) {
      return (INT64(lego_info->Back_Peel()) >= (ref_offset - (step - 1)));
    } else {
      return (INT64(lego_info->Front_Peel()) >= -ref_offset);
    }
  }

  return FALSE;
}


/*
 * Block Distribution for access on local processor:
 *
 * A[j] ==>  A[pid][jlocal], 
 * 
 *                 Dim sizes:           Indexes:
 *  distr_addr:       p                   pid
 *  local_addr:       b                   jlocal
 *
 *  where jlocal is initialized to ((stride*lb + offset) % b) outside the loop
 *  and incremented by stride at the end of the loop (see lego_opts.cxx)
 */
void 
ARRAY_LOWER_REF::Create_Local_Block_Dim(INT dim,
					WN *distr_addr,
					WN *local_addr,
					INT *curr_distr,
					INT *curr_local)
{

  if (!disable_rr_maps) {
    RR_INFO* rri = Get_RR_Map (_array_ref);
    Is_True (rri, ("Missing RR-Map"));
    RR_DIM* rrdim = rri->Dim(dim);
    Is_True (rrdim, ("Missing RR-Map"));

    DISTR_INFO *dinfo = Dact()->Dinfo();
  
    // Set distr_addr dimension
    //
    WN *bound = dinfo->Numprocs(dim);  // returns ldid of p
    WN* proc_wn = rrdim->Do_Loop();
    SYMBOL sym (WN_start(proc_wn));
    WN* index = AWN_LdidSym(&sym);
    printf ("Hey -- set DU info\n");

    Set_Array_Dim(distr_addr, *curr_distr, bound, index);
    (*curr_distr)++;

    // Set local_addr dimension
    //
    bound = dinfo->Dimsize(dim);  // returns ldid of dimsize
    index = LWN_Copy_Tree(WN_array_index(_array_ref, dim),
                          TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use (WN_array_index(_array_ref, dim), index, Du_Mgr);

    DO_LOOP_INFO* pdli = Get_Do_Loop_Info (proc_wn);
    index = AWN_Sub (WN_rtype(index),
                     index,
                     AWN_LdidSym(&(pdli->Lego_LB_Symbols[rrdim->Index()])));

    Set_Array_Dim(local_addr, *curr_local, bound, index);
    (*curr_local)++;

    return;
  }


  INT64 ref_stride;
  INT64 ref_offset;
  ARRAY_LOWER_LOOP *ref_loop = Get_Ref_Loop(dim, &ref_stride, &ref_offset);
  Is_True(ref_loop, ("Create_Local_Block_Dim: ref_loop is NULL\n"));

  LEGO_INFO *lego_info = ref_loop->Get_Lego_Info();
  Is_True(lego_info && !lego_info->Is_Too_Messy(), 
	  ("Create_Local_Block_Dim: bad LEGO_INFO for ref_loop\n"));

  DISTR_INFO *dinfo = Dact()->Dinfo();

  // Set distr_addr dimension
  //
  WN *bound = dinfo->Numprocs(dim);  // returns ldid of p
  WN* index = lego_info->Pid0(Array_Ref()); // returns ldid of pid

  Set_Array_Dim(distr_addr, *curr_distr, bound, index);
  (*curr_distr)++;

  // Set local_addr dimension
  //
  if (!lego_info->Has_Local_Index()) 
    lego_info->Create_Local_Index(ref_loop->Doloop());

  bound = dinfo->Dimsize(dim);  // returns ldid of dimsize
  index = lego_info->Local_Index();  // return ldid of local_index

  TYPE_ID type = WN_rtype(index);
  WN *offset_wn = LWN_Make_Icon(type, (ref_offset - lego_info->Offset()));
  index = AWN_Add(type, index, offset_wn);

  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;
}


/*
 * Block Distribution for access on remote or unknown processor:
 *
 * A[j] ==>  A[j/b][j%b], b=divceil(N,p), where N is the dim size 
 * 
 *                 Dim sizes:           Indexes:
 *  distr_addr:       p                   j/b
 *  local_addr:       b                   j%b
 *
 */
void 
ARRAY_LOWER_REF::Create_Remote_Block_Dim(INT dim,
					 WN *distr_addr,
					 WN *local_addr,
					 INT *curr_distr,
					 INT *curr_local)
{
  DISTR_INFO *dinfo = Dact()->Dinfo();

  // Set distr_addr dimension
  //
  WN *bound = dinfo->Numprocs(dim);  // returns ldid of p

  WN *old_index = WN_array_index(Array_Ref(), dim);
  WN *old_index_copy = LWN_Copy_Tree(old_index, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_index, old_index_copy, Du_Mgr);

  WN *dim_size = dinfo->Dimsize(dim);  // returns ldid of divceil(N,p) 
  TYPE_ID type = Max_Wtype(WN_rtype(dim_size), 
                           WN_rtype(old_index));
  WN *index = AWN_Div_Safe(type, old_index_copy, dim_size);

  Set_Array_Dim(distr_addr, *curr_distr, bound, index);
  (*curr_distr)++;

  // Set local_addr dimension
  //
  old_index_copy = LWN_Copy_Tree(old_index, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_index, old_index_copy, Du_Mgr);

  bound = dinfo->Dimsize(dim);
  index = AWN_Rem_Safe(type, old_index_copy, dinfo->Dimsize(dim));

  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;
}


/* 
 * Lower array dimension with cyclic distribution
 */
void
ARRAY_LOWER_REF::Create_Cyclic_Dim(INT dim,
				   WN *distr_addr,
				   WN *local_addr,
				   INT *curr_distr,
				   INT *curr_local)
{
  if (!disable_divmod_opts && Is_Cyclic_Dim_Local(dim)) {
    Create_Local_Cyclic_Dim(dim, distr_addr, local_addr,
			    curr_distr, curr_local);
  } else {
    Create_Remote_Cyclic_Dim(dim, distr_addr, local_addr, 
			     curr_distr, curr_local); 
  }
}


/* 
 * Returns TRUE if it the array access is local, FALSE otherwise.
 */
mBOOL
ARRAY_LOWER_REF::Is_Cyclic_Dim_Local(INT dim)
{
  INT64 ref_stride;
  INT64 ref_offset;

  ARRAY_LOWER_LOOP *ref_loop = Get_Ref_Loop(dim, &ref_stride, &ref_offset);
  if (!ref_loop) return FALSE;

  LEGO_INFO *lego_info = ref_loop->Get_Lego_Info();
  if (!lego_info || lego_info->Is_Too_Messy()) return FALSE;

  SYMBOL *affinity_array = lego_info->Array();
  DISTR_ARRAY *affinity_dact = Lookup_DACT(affinity_array->St());
 
  // If the affinity array and the current array have equivalent distributions
  // then the access is local.
  // 
  if (Dact()->DACT_Equiv(affinity_dact, dim, lego_info->Dim_Num())) {

    INT64 step = lego_info->Get_Local_Step(ref_loop->Doloop());
    if (step <= 0) return FALSE;

    return ((ref_stride == lego_info->Stride()) &&
	    (ref_offset == lego_info->Offset()));
  }

  return FALSE;
}



/*
 * Cyclic(1) Distribution for access on local processor:
 *
 * A[j] ==>  A[pid][jlocal], 
 * 
 *                 Dim sizes:           Indexes:
 *  distr_addr:       p                   pid
 *  local_addr:       divceil(N,p)        jlocal
 *
 *  where jlocal is initialized to ((stride*lb + offset) / p) outside the loop
 *  and incremented by stride at the end of the loop (see lego_opts.cxx)
 */
void 
ARRAY_LOWER_REF::Create_Local_Cyclic_Dim(INT dim,
					 WN *distr_addr,
					 WN *local_addr,
					 INT *curr_distr,
					 INT *curr_local)
{
  INT64 ref_stride;
  INT64 ref_offset;
  ARRAY_LOWER_LOOP *ref_loop = Get_Ref_Loop(dim, &ref_stride, &ref_offset);
  Is_True(ref_loop, ("Create_Local_Cyclic_Dim: ref_loop is NULL\n"));

  LEGO_INFO *lego_info = ref_loop->Get_Lego_Info();
  Is_True(lego_info && !lego_info->Is_Too_Messy(), 
	  ("Create_Local_Cyclic_Dim: bad LEGO_INFO for ref_loop\n"));

  DISTR_INFO *dinfo = Dact()->Dinfo();

  // Set distr_addr dimension
  //
  WN *bound = dinfo->Numprocs(dim);  // returns ldid of p
  WN *index = lego_info->Pid0(Array_Ref()); // returns ldid of pid

  Set_Array_Dim(distr_addr, *curr_distr, bound, index);
  (*curr_distr)++;

  // Set local_addr dimension
  //
  if (!lego_info->Has_Local_Index()) 
    lego_info->Create_Local_Index(ref_loop->Doloop());

  bound = dinfo->Dimsize(dim);  // returns ldid of divceil(N,p)
  index = lego_info->Local_Index();  // returns ldid of local_index

  TYPE_ID type = WN_rtype(index);
  WN *offset_wn = LWN_Make_Icon(type, (ref_offset - lego_info->Offset()));
  index = AWN_Add(type, index, offset_wn);

  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;
}


/*
 * Cyclic(1) Distribution for access on remote processor:
 *
 * A[j] ==> A[j%p][j/p] 
 *
 *                 Dim sizes:           Indexes:
 *  distr_addr:       p                   j%p
 *  local_addr:       divceil(N,p)        j/p
 *
 */
void 
ARRAY_LOWER_REF::Create_Remote_Cyclic_Dim(INT dim,
					  WN *distr_addr,
					  WN *local_addr,
					  INT *curr_distr,
					  INT *curr_local)
{
  DISTR_INFO *dinfo = Dact()->Dinfo();

  // Set distr_addr dimension
  //
  WN *bound = dinfo->Numprocs(dim);  // returns ldid of p

  WN *old_index = WN_array_index(Array_Ref(), dim);
  WN *old_index_copy = LWN_Copy_Tree(old_index, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_index, old_index_copy, Du_Mgr);

  WN *num_procs = dinfo->Numprocs(dim);
  TYPE_ID type = Max_Wtype(WN_rtype(num_procs),
			   WN_rtype(old_index));
  WN *index = AWN_Rem_Safe(type, old_index_copy, num_procs);

  Set_Array_Dim(distr_addr, *curr_distr, bound, index);
  (*curr_distr)++;

  // Set local_addr dimension
  //
  old_index_copy = LWN_Copy_Tree(old_index, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_index, old_index_copy, Du_Mgr);

  bound = dinfo->Dimsize(dim);  // returns ldid of divceil(N,p)
  index = AWN_Div_Safe(type, old_index_copy, dinfo->Numprocs(dim));
  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;
}


/* 
 * Lower array dimension with block-cyclic distribution
 */
void
ARRAY_LOWER_REF::Create_Blkcyc_Dim(INT dim,
				   WN *distr_addr,
				   WN *local_addr,
				   INT *curr_distr,
				   INT *curr_local,
				   WN *chunk_nd) /* Load of preg or int const 
						    for b */
{
  if (!disable_divmod_opts && Is_Blkcyc_Dim_Local(dim)) {
    Create_Local_Blkcyc_Dim(dim, distr_addr, local_addr,
			    curr_distr, curr_local, chunk_nd);
  } else {
    Create_Remote_Blkcyc_Dim(dim, distr_addr, local_addr, 
			     curr_distr, curr_local, chunk_nd); 
  }
}


/* 
 * Returns TRUE if it the array access is local, FALSE otherwise.
 */
mBOOL
ARRAY_LOWER_REF::Is_Blkcyc_Dim_Local(INT dim)
{
  INT64 ref_stride;
  INT64 ref_offset;

  ARRAY_LOWER_LOOP *ref_loop = Get_Ref_Loop(dim, &ref_stride, &ref_offset);
  if (!ref_loop) return FALSE;

  LEGO_INFO *lego_info = ref_loop->Get_Lego_Info();
  if (!lego_info || lego_info->Is_Too_Messy()) return FALSE;

  SYMBOL *affinity_array = lego_info->Array();
  DISTR_ARRAY *affinity_dact = Lookup_DACT(affinity_array->St());
 
  // If the affinity array and the current array have equivalent distributions
  // then the access is local.
  // 
  if (Dact()->DACT_Equiv(affinity_dact, dim, lego_info->Dim_Num())) {

    INT64 step = lego_info->Get_Local_Step(ref_loop->Doloop());
    if (step <= 0) return FALSE;

    return ((ref_stride == lego_info->Stride()) &&
	    (ref_offset == lego_info->Offset()));
  }

  return FALSE;
}



/*
 * Cyclic(b) distribution for access on local processor:
 *
 * A[j] ==>  A[pid0][pid1][jlocal], 
 * 
 *                 Dim sizes:           Indexes:
 *  distr_addr:       p                   pid0
 *  local_addr1:      divceil(N,pb)       pid1
 *  local_addr2:      b                   jlocal
 *
 *  where jlocal is initialized to ((stride*lb + offset) % b) outside the loop
 *  and incremented by stride at the end of the loop (see lego_opts.cxx)
 */
void 
ARRAY_LOWER_REF::Create_Local_Blkcyc_Dim(INT dim,
					 WN *distr_addr,
					 WN *local_addr,
					 INT *curr_distr,
					 INT *curr_local,
					 WN *chunk_nd) /* Load of preg or int const 
							  for b */
{
  INT64 ref_stride;
  INT64 ref_offset;
  ARRAY_LOWER_LOOP *ref_loop = Get_Ref_Loop(dim, &ref_stride, &ref_offset);
  Is_True(ref_loop, ("Create_Local_Blkcyc_Dim: ref_loop is NULL\n"));

  LEGO_INFO *lego_info = ref_loop->Get_Lego_Info();
  Is_True(lego_info && !lego_info->Is_Too_Messy(), 
	  ("Create_Local_Blkcyc_Dim: bad LEGO_INFO for ref_loop\n"));

  DISTR_INFO *dinfo = Dact()->Dinfo();

  // Set distr_addr dimension
  //
  WN *bound = dinfo->Numprocs(dim);  // returns ldid of p
  WN *index = lego_info->Pid0(Array_Ref()); // returns ldid of pid0

  Set_Array_Dim(distr_addr, *curr_distr, bound, index);
  (*curr_distr)++;

  // Set local_addr1 dimension
  //
  //
  bound = dinfo->Dimsize(dim);    // returns ldid of divceil(N,pb)
  index = lego_info->Pid1(Array_Ref()); // returns ldid of pid1

  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;

  // Set local_addr2 dimension
  //
  if (!lego_info->Has_Local_Index()) 
    lego_info->Create_Local_Index(ref_loop->Doloop());

  bound = chunk_nd;              
  index = lego_info->Local_Index();  // returns ldid of local_index

  TYPE_ID type = WN_rtype(index);
  WN *offset_wn = LWN_Make_Icon(type, (ref_offset - lego_info->Offset()));
  index = AWN_Add(type, index, offset_wn);

  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;
}

 
/*
 * Cyclic(b) distribution for access on a remote processor:
 *
 *
 * A[j] ==> A[(j/b)%p][j/pb][j%b] 
 *
 *                 Dim sizes:           Indexes:
 *  distr_addr:       p                    (j/b)%p
 *  local_addr1:      divceil(N,pb)        j/pb
 *  local_addr2:      b                    j%b
 *
 */
void 
ARRAY_LOWER_REF::Create_Remote_Blkcyc_Dim(INT dim,
					  WN *distr_addr,
					  WN *local_addr,
					  INT *curr_distr,
					  INT *curr_local,
					  WN *chunk_nd) /* Load of preg or int const 
							   for b */
{
  DISTR_INFO *dinfo = Dact()->Dinfo();

  // Set distr_addr dimension
  //
  WN *bound = dinfo->Numprocs(dim);  // returns ldid of p

  WN *old_index = WN_array_index(Array_Ref(), dim);
  WN *old_index_copy = LWN_Copy_Tree(old_index, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_index, old_index_copy, Du_Mgr);
  WN *chunk_nd_copy = LWN_Copy_Tree(chunk_nd, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(chunk_nd, chunk_nd_copy, Du_Mgr);

  WN *num_procs = dinfo->Numprocs(dim);
  TYPE_ID type = Max_Wtype(WN_rtype(num_procs),
			   WN_rtype(old_index));
  type = Max_Wtype(type, WN_rtype(chunk_nd));
  WN *div = AWN_Div_Safe(type, old_index_copy, chunk_nd_copy);
  WN *index = AWN_Rem_Safe(type, div, num_procs);

  Set_Array_Dim(distr_addr, *curr_distr, bound, index);
  (*curr_distr)++;

  // Set local_addr1 dimension
  //  
  old_index_copy = LWN_Copy_Tree(old_index, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_index, old_index_copy, Du_Mgr);
  chunk_nd_copy = LWN_Copy_Tree(chunk_nd, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(chunk_nd, chunk_nd_copy, Du_Mgr);

  WN *pb = AWN_Mpy(type, dinfo->Numprocs(dim), chunk_nd_copy);
  bound = dinfo->Dimsize(dim);    // returns ldid of divceil(N,pb)
  index = AWN_Div_Safe(type, old_index_copy, pb);
  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;
						       
  // Set local_addr2 dimension
  //  
  old_index_copy = LWN_Copy_Tree(old_index, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_index, old_index_copy, Du_Mgr);
  chunk_nd_copy = LWN_Copy_Tree(chunk_nd, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(chunk_nd, chunk_nd_copy, Du_Mgr);

  bound = chunk_nd_copy;
  index = AWN_Rem_Safe(type, old_index_copy, chunk_nd);      
  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;
}


/*
 * Star Distribution:
 *
 * A[j] ==> A[j]
 * 
 *                 Dim sizes:           Indexes:
 *  distr_addr:       n/a                  n/a
 *  local_addr:       N                     j
 *
 */
void 
ARRAY_LOWER_REF::Create_Star_Dim(INT dim,
				 WN *local_addr,
				 INT *curr_local)
{
  // Set local_addr dimension
  //  
  WN *old_index = WN_array_index(Array_Ref(), dim);
  WN *old_bound = WN_array_dim(Array_Ref(), dim);

  WN *bound = LWN_Copy_Tree(old_bound, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_bound, bound, Du_Mgr);

  WN *index = LWN_Copy_Tree(old_index, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(old_index, index, Du_Mgr);

  Set_Array_Dim(local_addr, *curr_local, bound, index);
  (*curr_local)++;
}


/*
 * Does the access vector represent an array access that's a 
 * a single loop coefficient plus an offset, e.g. a[3i+5]
 * is ok, but a[i+j+5] is not.  If true, also returns the loop coeff,
 * the constant offset and the loop depth in 'stride',
 * 'offset' and 'depth', respectively.
 */
extern mBOOL Single_Loop_Coeff(ACCESS_VECTOR *av, 
			INT64 *stride, 
			INT64 *offset,
			mINT32 *depth)
{
  *stride = 0;
  *offset = 0;
  *depth = -1;

  if (av->Too_Messy ||
      av->Contains_Lin_Symb() || 
      av->Contains_Non_Lin_Symb() || 
      av->Is_Const() ||
      !av->Has_Loop_Coeff())
    return FALSE;

  for (INT i = 0; i < av->Nest_Depth(); i++) {
    if (av->Loop_Coeff(i) != 0) {
      if (*stride != 0) return FALSE; 

      *stride = av->Loop_Coeff(i);
      *depth = i;
    }
  }

  *offset = av->Const_Offset;
  return TRUE;
}

/* ========================================================================
   Private Function Implementations
   ======================================================================== */


/*
 * Return the call node that is a proper ancestor of curr_nd, if any
 */
WN *Get_Call_Parent(WN *curr_nd)
{
  WN *parent = LWN_Get_Parent(curr_nd);

  while (parent) {
    if (OPCODE_is_call(WN_opcode(parent)) ||
        WN_operator(parent) == OPR_INTRINSIC_OP
#ifdef KEY
	|| WN_operator(parent) == OPR_PURE_CALL_OP
#endif
	) {
      return parent;
    } else {
      parent = LWN_Get_Parent(parent);
    }
  }

  return NULL;
}


/*
 * Get the TY for the callee in the call_nd
 */
TY_IDX Get_Callee_TY(WN *call_nd, 
                  ST *array_st)
{
  if (WN_operator(call_nd) == OPR_CALL
#ifdef KEY
      || WN_operator(call_nd) == OPR_PURE_CALL_OP
#endif
     ) {
    ST* callee_st = ST_ptr(WN_entry_name(call_nd));
    return (ST_type(callee_st));

  } else if (WN_operator(call_nd) == OPR_ICALL) {
    return WN_ty(call_nd);

  } else if (WN_operator(call_nd) != OPR_INTRINSIC_OP) {
    // let intrinsics just go
    //
    DevWarn("Bad reference at line %d: Reshaped array %s in invalid call statement\n",
	    Srcpos_To_Line(LWN_Get_Linenum(call_nd)), ST_name(array_st));
  }
  return TY_IDX_ZERO;
}

/* 
 * Return the number of the actual parameter in call_nd that contains
 * array_param.
 */ 
INT Find_Param_Num(WN *array_param, 
		   WN *call_nd)
{
  WN *kid;
  for (INT kidno = 0; kidno < WN_kid_count(call_nd); kidno++) {
    kid = WN_kid(call_nd, kidno);

    if (Is_Descendent(array_param, kid)) return kidno;
  }

  return -1;
}


static WN* Lower_Single_Array_Dim (WN* array_wn) {

  Is_True (WN_operator(array_wn) == OPR_ARRAY,
           ("Lower_Reshaped_Reference called on non-array\n"));
  INT ndims = WN_kid_count(array_wn) >> 1;

  if (ndims > 1) {
    // multi-dim
    WN* bounds_wn = NULL;
    for (INT i=0; i<ndims; i++) {

      // compute index(i) * Prod (dim(j)) for all j>i
      WN* prod_wn = LWN_Copy_Tree(WN_array_index(array_wn, i),
                                  TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use (WN_array_index(array_wn, i), prod_wn, Du_Mgr);
      TYPE_ID type = WN_rtype(prod_wn);

      for (INT j=i+1; j<ndims; j++) {
        WN* dim_wn = LWN_Copy_Tree (WN_array_dim(array_wn, j),
                                    TRUE, LNO_Info_Map);
        LWN_Copy_Def_Use (WN_array_dim(array_wn, j), dim_wn, Du_Mgr);
        prod_wn = AWN_Mpy (type, prod_wn, dim_wn);
      }
      if (bounds_wn == NULL) bounds_wn = prod_wn;
      else {
        bounds_wn = AWN_Add (type, bounds_wn, prod_wn);
      }
    }

    TYPE_ID type = WN_rtype(bounds_wn);
    OPCODE op = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
    WN* new_array_wn = WN_Create(op, 3);
    WN_array_base(new_array_wn) = LWN_Copy_Tree (WN_array_base(array_wn),
                                                 TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use (WN_array_base(array_wn),
                      WN_array_base(new_array_wn),
                      Du_Mgr);
    op = OPCODE_make_op(OPR_INTCONST, type, MTYPE_V);
    WN_array_dim(new_array_wn, 0) = WN_CreateIntconst (op, 1);
    WN_array_index(new_array_wn, 0) = bounds_wn;
    WN_element_size(new_array_wn) = WN_element_size(array_wn);
    LWN_Parentize(new_array_wn);

    Replace_WN (array_wn, new_array_wn);
    LWN_Delete_Tree (array_wn);
    array_wn = new_array_wn;
  }
  return array_wn;
} /* Lower_Single_Array_Dim () */

/***********************************************************************
 *
 * Given a lowered reshaped array reference, do 'array-lowering' of
 * the individual array nodes from multi-d down to 1-d.
 *
 ***********************************************************************/
static WN* Lower_Reshaped_Reference (WN* array_wn) {

  Is_True (WN_operator(array_wn) == OPR_ARRAY,
           ("Lower_Reshaped_Reference called on non-array\n"));
  Is_True (WN_operator(WN_array_base(array_wn)) == OPR_ILOAD,
           ("Lower_Reshaped_Reference called on array with non-iload base\n"));
  Is_True (WN_operator(WN_kid0(WN_array_base(array_wn))) ==
    OPR_ARRAY,
    ("Lower_Reshaped_Reference called on array with non-array iload base\n"));

  array_wn = Lower_Single_Array_Dim (array_wn);
  Lower_Single_Array_Dim (WN_kid0(WN_array_base(array_wn)));
  return array_wn;
}

/***********************************************************************
 *
 * Given an expression and a hoist-to place, hoist it.
 *  - hoist_place might be an OPR_BLOCK, in which case hoist to first
 *    thing in the block.
 *  - hoist_place2 is default NULL, but if non-NULL then also hoist to 
 *    just AFTER hoist_place2. After, since that is usually the last
 *    statement in the loop that we're hoisting into.
 *  - this routine currently allows hoist_place2 to be non-NULL only for 
 *    hoisting iloads. It can easily change to be more flexible if needed.
 *
 * Return a pointer to either expr_wn, or the created copy of expr_wn:
 * (either the iload, or kid0 of the eval node).
 *
 ***********************************************************************/
static WN* Hoist_Expression (WN* expr_wn,
                             WN* hoist_place,
                             WN* hoist_place2 = NULL) {
  if (LWN_Get_Parent(expr_wn) == LWN_Get_Parent(hoist_place))
    return expr_wn;

  // don't hoist outside the mp-do-loop, since that will get moved
  // by daveb to outside the nested PU, and WOPT won't see/CSE it.

  if (WN_operator(hoist_place) == OPR_DO_LOOP &&
      Do_Loop_Is_Mp(hoist_place)) {

    WN* first_wn = WN_first(WN_do_body(hoist_place));
    if (first_wn) hoist_place = first_wn;
    else {
      // do loop is empty. Set place_loop to the BLOCK node,
      // and check at the insertion points.
      hoist_place = WN_do_body(hoist_place);
    }
  }

  // hoist just iloads, and eval the rest
  if (WN_operator(expr_wn) == OPR_ILOAD) {
    TYPE_ID expr_type = WN_rtype(expr_wn);


    WN* array_wn = WN_kid0(expr_wn);
    Is_True (WN_operator(array_wn) == OPR_ARRAY,
             ("Hoist_Expr ILOAD: expected an OPR_ARRAY"));
    WN* base_wn = WN_array_base(array_wn);
    Is_True (WN_operator(base_wn) == OPR_LDID ||
             WN_operator(base_wn) == OPR_LDA,
             ("Hoist_Expr ARRAY: expected an LDID/LDA as base"));
    ST* array_st = WN_st(base_wn);
    DISTR_INFO* dinfo = da_hash->Find(array_st);
    Is_True (dinfo && dinfo->IsReshaped(),
             ("Hoist_Expr ARRAY-BASE: expected a reshaped array"));

    dinfo->Hoist_Proc_Init (expr_type);
    SYMBOL sym (dinfo->Hoist_Proc_Array(),
                dinfo->Hoist_Proc_Next_Offset(),
                expr_type);
    // resume regular stuff

    Set_ST_pt_to_unique_mem(sym.St());
    Set_ST_pt_to_compiler_generated_mem(sym.St());
    WN* ldid_wn = AWN_LdidSym (&sym);
    // we're hoisting an ILOAD, the array base. 
    // so set the high-level type appropriately
    WN_set_ty(ldid_wn, WN_ty(expr_wn));
    Set_TY_ptr_as_array(WN_ty(ldid_wn));

    // replace expr_wn with ldid_wn
    Replace_WN (expr_wn, ldid_wn);

    // also rewrite the indirect reference (ILOAD/ISTORE)
    // since its alias info is screwed up
    // we don't compute any new alias info for this node anymore, 
    // but this is late enough in LNO that it hopefully doesn't matter.
    // (Ref: pv 497793).
    WN *iwn = LWN_Get_Parent(ldid_wn);
    while (iwn) {
      OPERATOR opr = WN_operator(iwn);
      if (opr == OPR_ILOAD ||
          opr == OPR_ISTORE ||
          opr == OPR_PREFETCH ||
          opr == OPR_PREFETCHX ||
          opr == OPR_PARM ||
          (WN_io_item(iwn) == IOL_VAR)) {
        break;
      }
      
      iwn = LWN_Get_Parent(iwn);
    }
    // iwn should be non-NULL, but to be safe handle NULL case also.
    if (iwn && (WN_operator(iwn) == OPR_ILOAD ||
                WN_operator(iwn) == OPR_ISTORE)) {
      dinfo->Hoist_Proc_Alias(iwn);
    }
    else if (iwn == NULL) {
      DevWarn ("Missing iload/istore/prefetch/parm for reshaped reference");
    }

    WN* stid_wn = AWN_StidIntoSym (&sym, expr_wn);
    WN_set_ty(stid_wn, WN_ty(ldid_wn));
  
    if (WN_operator(hoist_place) == OPR_BLOCK)
      // do loop had empty body
      LWN_Insert_Block_Before(hoist_place, NULL, stid_wn);
    else
      LWN_Insert_Block_Before(LWN_Get_Parent(hoist_place),hoist_place,stid_wn);
    Copy_alias_info (Alias_Mgr, ldid_wn, stid_wn);
    Du_Mgr->Add_Def_Use(stid_wn, ldid_wn);
    Add_Pragma_To_MP_Region (stid_wn, sym.St(), 0, WN_PRAGMA_LOCAL);

    if (hoist_place2) {
      WN* stid_wn2 = LWN_Copy_Tree (stid_wn, TRUE, LNO_Info_Map);
      Copy_alias_info (Alias_Mgr, stid_wn, stid_wn2);
      LWN_Copy_Def_Use (WN_kid0(stid_wn), WN_kid0(stid_wn2), Du_Mgr);
      Du_Mgr->Add_Def_Use (stid_wn2, ldid_wn);
      if (WN_operator(hoist_place2) == OPR_BLOCK)
        // do loop had empty body
        LWN_Insert_Block_Before(hoist_place2, NULL, stid_wn2);
      else
        // hoist AFTER hoist_place2
        LWN_Insert_Block_After(LWN_Get_Parent(hoist_place2),
                                hoist_place2,
                                stid_wn2);
    }
    return expr_wn;
  }
  else {
    // just generate an eval
    FmtAssert (hoist_place2 == NULL,
               ("Hoisting a non-ILOAD expression, unexpected hoist_place2"));

    OPCODE op = OPCODE_make_op(OPR_EVAL, MTYPE_V, MTYPE_V);
    WN* eval_wn = WN_Create (op, 1);
    WN_kid0(eval_wn) = LWN_Copy_Tree (expr_wn, TRUE, LNO_Info_Map);
    LWN_Set_Parent (WN_kid0(eval_wn), eval_wn);
    LWN_Copy_Def_Use (expr_wn, WN_kid0(eval_wn), Du_Mgr);
    if (WN_operator(hoist_place) == OPR_BLOCK)
      // do loop had empty body
      LWN_Insert_Block_Before(hoist_place, NULL, eval_wn);
    else
      LWN_Insert_Block_Before(LWN_Get_Parent(hoist_place),hoist_place,eval_wn);
    return WN_kid0(eval_wn);
  }
} /* Hoist_Expression () */

/***********************************************************************
 *
 * Given an expression, try to hoist it as high as possible.
 * Then hoist sub-expr higher, if possible.
 *
 ***********************************************************************/
extern void Try_Hoist_Expression (WN* expr_wn) {
  OPERATOR opr = WN_operator(expr_wn);

  // avoid hoisting simple loads, or constants
  if (opr == OPR_LDID ||
      opr == OPR_LDA ||
      opr == OPR_INTCONST ||
      opr == OPR_CONST)
    return;

  // avoid cheap arithmetic: add/sub of (constant +/- var/constant)
  if (opr == OPR_ADD || opr == OPR_SUB) {
    OPERATOR opr1 = WN_operator(WN_kid0(expr_wn));
    OPERATOR opr2 = WN_operator(WN_kid1(expr_wn));
    if ((opr1 == OPR_INTCONST) &&
        (opr2 == OPR_INTCONST || opr2 == OPR_LDID))
      return;
    if ((opr2 == OPR_INTCONST) &&
        (opr1 == OPR_INTCONST || opr1 == OPR_LDID))
      return;
  }
  

  WN* hoist_place = Hoistable_Place (expr_wn, Du_Mgr);
  if (hoist_place) {
    WN* my_stmt = expr_wn;
    OPCODE opc = WN_opcode(my_stmt);
    while (!OPCODE_is_scf(opc) && !OPCODE_is_stmt(opc)) {
      my_stmt = LWN_Get_Parent(my_stmt);
      opc = WN_opcode(my_stmt);
    }
    
    if (LWN_Get_Parent(my_stmt) != LWN_Get_Parent(hoist_place)) {
      // do the hoisting
      expr_wn = Hoist_Expression (expr_wn, hoist_place);
    }
  }
  
  // now see if any sub-expressions are further hoistable
  for (INT i=0; i<WN_kid_count(expr_wn); i++) {
    Try_Hoist_Expression (WN_kid(expr_wn, i));
  }
} /* Try_Hoist_Expression() */

/***********************************************************************
 *
 * array_wn is the array node after lowering a reshaped array reference.
 * Try to hoist each of
 *  - index expressions in reference into processors portion
 *  - entire base (if possible) i.e. pointer to local processor's portion
 *  - otherwise index expressions in local processor pointer
 *
 ***********************************************************************/
static void Hoist_Reshaped_Reference (WN* array_wn) {
  INT i;
  INT ndim;

  Is_True (WN_operator(array_wn) == OPR_ARRAY,
           ("Hoist_Reshaped_Reference called on non-array\n"));
  Is_True (WN_operator(WN_array_base(array_wn)) == OPR_ILOAD,
           ("Hoist_Reshaped_Reference called on array with non-iload base\n"));
  Is_True (WN_operator(WN_kid0(WN_array_base(array_wn))) ==
    OPR_ARRAY,
    ("Hoist_Reshaped_Reference called on array with non-array iload base\n"));

  // first try each index expression in local reference
  ndim = WN_kid_count(array_wn) >> 1;
  for (i=0; i<ndim; i++) {
    WN* index_wn = WN_array_index (array_wn, i);
    Try_Hoist_Expression (index_wn);
  }

  // now try the array base, or else subexpressions thereof
  WN* iload_wn = WN_array_base(array_wn);
  WN* hoist_place = Hoistable_Place (iload_wn, Du_Mgr);
  if (hoist_place &&
      (LWN_Get_Parent(iload_wn) != LWN_Get_Parent(hoist_place))) {
    FmtAssert (FALSE, ("Trying to hoist an ILOAD"));
    Hoist_Expression (iload_wn, hoist_place);
    return;
  }

  // we know that the base is hoistable - let's try again
  WN* base_wn = WN_kid0(iload_wn);
  WN* base_sym = WN_kid0(base_wn);
  Is_True (WN_operator(base_sym) == OPR_LDID ||
           WN_operator(base_sym) == OPR_LDA,
           ("Base of reshaped array not an LDID/LDA"));
  ndim = WN_kid_count(base_wn);

  hoist_place = Initial_Hoist_Place (base_wn);
  Is_True (hoist_place, ("Initial_Hoist_Place returned NULL"));
  for (i=0; i<ndim; i++) {
    WN* this_hoist = Hoistable_Place (WN_kid(base_wn,i), Du_Mgr);
    if (this_hoist == NULL) break;
    
    hoist_place = Hoist_Merge (hoist_place, this_hoist);
  }

  if (i == ndim) {
    // hoist the iload, even if just before the statement containing iload
    // (for aliasing reasons)
    Hoist_Expression (iload_wn, hoist_place);
  }
  else {
    // had to break out of the loop, not hoistable at all
    // even if it was not hoistable, we want to dereference the array
    // through the proc-ptr variable for aliasing reasons.
    hoist_place = LWN_Get_Parent(iload_wn);
    WN* hoist_place2 = NULL;

    OPCODE opc = WN_opcode(hoist_place);
    while (!OPCODE_is_scf(opc) && !OPCODE_is_stmt(opc)) {
      hoist_place = LWN_Get_Parent(hoist_place);
      opc = WN_opcode(hoist_place);
    }

    if (OPCODE_is_scf(opc)) {
      // need to create the initialization at multiple places
      switch (OPCODE_operator(opc)) {
      case OPR_DO_LOOP:
        // end of do body
        hoist_place2 = WN_last(WN_do_body(hoist_place));
        if (hoist_place2 == NULL)
          hoist_place2 = WN_do_body(hoist_place);
        break;
      case OPR_DO_WHILE:
      case OPR_WHILE_DO:
        hoist_place2 = WN_last(WN_while_body(hoist_place));
        if (hoist_place2 == NULL)
          hoist_place2 = WN_do_body(hoist_place);
        break;
      }
    }
    Hoist_Expression (iload_wn, hoist_place, hoist_place2);
  }

  // now do the index expressions further (regardless of hoisting of iload)
  ndim = WN_kid_count(base_wn) >> 1;
  for (i=0; i<ndim; i++) {
    WN* index_wn = WN_array_index (base_wn, i);
    Try_Hoist_Expression (index_wn);
  }
}
