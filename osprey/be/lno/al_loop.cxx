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
// Module: al_loop.cxx
// $Revision: 1.5 $
// $Date: 04/12/21 14:57:11-08:00 $
// $Author: bos@eng-25.internal.keyresearch.com $
// $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.al_loop.cxx $
//
// Revision history:
//  dd-mmm-95 - Original Version
//
// Description:
//
//   Representation of loops containing references to reshaped arrays.
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
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.al_loop.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "lnopt_main.h"
#include "lnoutils.h"
#include "lwn_util.h"
#include "snl_utils.h"
#include "lego_pragma.h"
#include "lego_util.h"
#include "array_lower.h"
#include "al_loop.h"
#include "errors.h"
#include "erbe.h"

/* ========================================================================
   Public Function Declarations
   ======================================================================== */

/* ========================================================================
   Private Function Declarations
   ======================================================================== */

static void Find_Array_Expr(WN *curr_nd);
static mBOOL Is_Reshaped_Array_Expr(const WN *curr_nd, DISTR_ARRAY **dact);
static mBOOL Is_Reshaped_Array_Param(const WN *curr_nd, DISTR_ARRAY **dact);
static void Check_Invalid_Accesses(const WN *curr_nd);

static ST *Get_Reshaped_Array_St(const WN *array_expr, mBOOL *valid);
static mBOOL Is_Allowed_Invalid_Access(const WN *curr_nd);
static mBOOL Is_Array_Base(const WN *curr_nd);
static mBOOL Is_Array_Param(const WN *curr_nd);
static WN *Get_Memop_Parent(const WN *curr_nd);
static mBOOL Has_Reshaped_Type(ST *array_st);
static DISTR_ARRAY *Lookup_Array_Distr(ST *array_st);

/* ========================================================================
   Class Implementations
   ======================================================================== */

ARRAY_LOWER_LOOP::~ARRAY_LOWER_LOOP()
{
  while (_children.Elements())   CXX_DELETE (_children.Pop(), LEGO_pool);
  while (_array_refs.Elements()) CXX_DELETE (_array_refs.Pop(), LEGO_pool);
}

/*
 * Return the nth ancestor of this loop.  n <= 0 returns current loop;
 * if no such ancestor exists, return the top level loop.
 */
ARRAY_LOWER_LOOP *ARRAY_LOWER_LOOP::Get_Ancestor(INT n)
{
  ARRAY_LOWER_LOOP *curr_loop = this;
  INT curr_num = 0;

  while (curr_loop && (curr_num < n)) {
    curr_loop = curr_loop->Parent();
    curr_num++;
  }

  return curr_loop;
}


LEGO_INFO *ARRAY_LOWER_LOOP::Get_Lego_Info()
{
  Is_True(Doloop() && (WN_operator(Doloop()) == OPR_DO_LOOP),
    ("ARRAY_LOWER_LOOP::Get_Lego_Info: called with bad do loop\n"));

  DO_LOOP_INFO *loop_info = Get_Do_Loop_Info(Doloop());
  LEGO_INFO *lego_info = loop_info->Lego_Info;

  return (lego_info);
}


/* 
 * Walk the whirl tree in post-order looking for uses of reshaped arrays.  
 * Check to make sure uses are only in array expressions or as
 * array parameters and that the expressions are well-formed.
 */
void ARRAY_LOWER_LOOP::Build_Loop(WN *curr_nd)
{
  WN *kid;

  if (!curr_nd) return;

  if (WN_opcode(curr_nd) == OPC_BLOCK) {
    kid = WN_first(curr_nd);
    while (kid) {

      if (WN_opcode(kid) == OPC_DO_LOOP) {
        ARRAY_LOWER_LOOP *child_node = 
          CXX_NEW(ARRAY_LOWER_LOOP(this, kid, Depth()+1), LEGO_pool);
        _children.Push(child_node);
        child_node->Build_Loop(kid);
      } else {
        Build_Loop(kid);
      }

      kid = WN_next(kid);
    }
    return;
  } 

  for (INT kidno = 0; kidno < WN_kid_count(curr_nd); kidno++) {
    kid = WN_kid(curr_nd, kidno);

    if (WN_opcode(kid) == OPC_DO_LOOP) {
      ARRAY_LOWER_LOOP *child_node = 
        CXX_NEW(ARRAY_LOWER_LOOP(this, kid, Depth()+1), LEGO_pool);
      _children.Push(child_node);
      child_node->Build_Loop(kid);
    } else {
      Build_Loop(kid);
    }
  }

  Build_Refs(curr_nd);
}


/*
 * Check if curr_nd is an array expression that should be lowered
 */
void ARRAY_LOWER_LOOP::Build_Refs(WN *curr_nd)
{
  DISTR_ARRAY *dact = NULL;

  if (Is_Reshaped_Array_Expr(curr_nd, &dact)) {
    ARRAY_LOWER_REF *ref = 
      CXX_NEW(ARRAY_LOWER_REF(curr_nd, dact, this, FALSE), LEGO_pool);
    Add_Ref(ref);

  } else if (Is_Reshaped_Array_Param(curr_nd, &dact)) {
    ARRAY_LOWER_REF *ref = 
      CXX_NEW(ARRAY_LOWER_REF(curr_nd, dact, this, TRUE), LEGO_pool);
    Add_Ref(ref);

  } else {
    Check_Invalid_Accesses(curr_nd);
  }
}


void ARRAY_LOWER_LOOP::Process_Loop(void)
{
  INT i;

  for (i = 0; i < _children.Elements(); i++) {
    _children.Bottom_nth(i)->Process_Loop();
  }

  Process_Refs();
}


void ARRAY_LOWER_LOOP::Process_Refs(void)
{
  INT i;

  for (i = 0; i < _array_refs.Elements(); i++) {
    _array_refs.Bottom_nth(i)->Lower_Ref();
  }
}


/* ========================================================================
   Private Function Implementations
   ======================================================================== */

/* 
 * Check if curr_wn accesses a reshaped array.  If so and if the access
 * is well-formed, set dact and return TRUE.  
 */
mBOOL
Is_Reshaped_Array_Expr(const WN *curr_nd,
                       DISTR_ARRAY **dact)
{
  *dact = NULL;
  ST *array_st = NULL;

  // See if the array access is to a reshaped array and if it's legal
  // to lower it
  //
  if (WN_operator(curr_nd) == OPR_ARRAY) {
    mBOOL valid = TRUE;
    array_st = Get_Reshaped_Array_St(curr_nd, &valid);

    if (array_st) {
      *dact = Lookup_Array_Distr(array_st);

      if (*dact) {  
        DISTR_INFO *dinfo = (*dact)->Dinfo();

        if (!valid) {   
          ErrMsgSrcpos (EC_LNO_Generic2String, LWN_Get_Linenum(curr_nd),
                        "Suspicious reference to reshaped array, lowering anyway",
                        ST_name(array_st));
          return TRUE;
        } else if (dinfo->Num_Dim() != WN_num_dim(curr_nd)) {
          ErrMsgSrcpos (EC_LNO_Generic2String, LWN_Get_Linenum(curr_nd),
                        "Bad reference to reshaped array (mismatch in number of dimensions)",
                        ST_name(array_st));
        } else {
          return TRUE;
        }
      }
    }
  }

  return FALSE;
}


/* 
 * Check if curr_wn is passing the address of a reshaped array. 
 */
mBOOL
Is_Reshaped_Array_Param(const WN *curr_nd,
                        DISTR_ARRAY **dact)
{
  *dact = NULL;

  if (Is_Array_Param(curr_nd)) {

    ST *array_st = WN_st(curr_nd);

    if (array_st) {
      *dact = Lookup_Array_Distr(array_st);
      if (*dact) return TRUE;
    }
  }

  return FALSE;
}


/* 
 * Check for invalid uses of reshaped arrays.
 */
void Check_Invalid_Accesses(const WN *curr_nd)
{
  if (!OPCODE_is_not_executable(WN_opcode(curr_nd)) &&
       OPCODE_has_sym(WN_opcode(curr_nd)) && 
      !Is_Allowed_Invalid_Access(curr_nd) &&
      !Is_Array_Base(curr_nd) &&
      !Is_Array_Param(curr_nd) &&
      !WN_opcode(curr_nd) == OPC_IDNAME) {

    ST *array_st = WN_st(curr_nd);

    if (array_st && Lookup_Array_Distr(array_st)) {
      ErrMsgSrcpos (EC_LNO_Generic2String, LWN_Get_Linenum(curr_nd),
                    "Bad reference to reshaped array ", ST_name(array_st));
    }   
  }
}


/*
 * Check if the array access can be lowered and return the
 * ST of the array. The address calculated by the array node must be
 * used to load from or store into and the base of the array node must
 * be the base of the array itself.  
 * This means:
 * -- Ancestor of array node must be ILOAD/ISTORE or IO_ITEM of kind IOL_VAR
 *    with offset 0 (can have CVT or CVTL between array node and ancestor).
 *    or can be a PARM node if portion of array is being passed
 * -- Base of the array node must be LDA, or LDID with offset 0.
 */
ST *
Get_Reshaped_Array_St(const WN *array_expr, 
                      mBOOL *valid)
{
  Is_True(WN_operator(array_expr) == OPR_ARRAY, 
    ("Get_Reshaped_Array_St: called with non-array node (got opcode=%d)\n",
    WN_opcode(array_expr)));

  ST *array_st = NULL;
  WN *array_base = WN_array_base(array_expr);

  *valid = TRUE;

  // Get the array's ST entry.  If there's no ST entry in the base
  // then just return (with valid = TRUE)
  //
  if (OPCODE_has_sym(WN_opcode(array_base))) {
    array_st = WN_st(array_base);
  } else {
    return NULL;
  }

  // Check ancestor of the array node
  //
  WN *mem_wn = Get_Memop_Parent(array_expr);
  Is_True(mem_wn, ("Get_Reshaped_Array_St: No ancestor found for array\n"));


  // Tue Apr 29 12:09:34 PDT 1997
  // allow non-zero offsets: they can arise in structure references
  // and passing addresses into the middle of structures.


  // 
  // Check that the parent is an ILOAD/ISTORE/PREFETCH or IO_ITEM of kind IOL_VAR.
  // Need special case for handling IOL_VAR kinds of IO_ITEMs.  The
  // kid of the IO_ITEM is an address.  If that address is an array
  // expression, then we can lower it.  Note: of the remaining kinds of
  // I/O list IO_ITEMS, IOL_ARRAY, IOL_CHAR_ARRAY and IOL_RECORD all 
  // have kids that are addresses. Since we can't have array accesses
  // within them, we don't handle them here.  All other I/O is correctly
  // handled in the usual way.
  //
  if ((WN_operator(mem_wn) != OPR_ILOAD) &&
      (WN_operator(mem_wn) != OPR_ISTORE) &&
      (WN_operator(mem_wn) != OPR_PREFETCH) &&
      (WN_operator(mem_wn) != OPR_PREFETCHX) &&
      (WN_operator(mem_wn) != OPR_PARM) &&
      (WN_io_item(mem_wn) != IOL_VAR)) {

    *valid = FALSE;
    return array_st;
  }

  //
  // Check that the base is an LDA, or LDID with offset 0
  //
  if (WN_operator(array_base) == OPR_LDID) {
    if (WN_offset(array_base) != 0) {
      *valid = FALSE;
      return array_st;
    }
  } else if (WN_operator(array_base) != OPR_LDA) {
    *valid = FALSE;
    return array_st;
  }

  return array_st;
}


/*
 * Return the node that's a proper ancestor of curr_nd, ignoring CVTs and CVTLs.
 * Return NULL if no such ancestor is found
 */
WN *
Get_Memop_Parent(const WN *curr_nd)
{
  WN* parent = LWN_Get_Parent(curr_nd);

  while (parent) {
    if ((WN_operator(parent) != OPR_CVT) &&
        (WN_operator(parent) != OPR_CVTL)) {

      return parent;

    } else {
      parent = LWN_Get_Parent(parent);
    }
  }

  return NULL;
}

/*
 * Is this a special case of an invalid access to a reshaped arrays that is
 * known to be ok:
 * -- Assignment of storage to the reshaped array, generated during lego pragma
 *    processing, e.g. array = __dsm_Alloc_Reshaped_Array
 */
mBOOL
Is_Allowed_Invalid_Access(const WN *curr_nd)
{
  if (WN_operator(curr_nd) == OPR_STID) {
    WN *kid = WN_kid0(curr_nd);

    if (WN_operator(kid) == OPR_LDID) {
      DEF_LIST *defs = Du_Mgr->Ud_Get_Def(kid);
      if (defs && defs->Len() == 1) { 
        WN *def_wn = defs->Head()->Wn();
        if (WN_operator(def_wn) == OPR_PARM) {
          def_wn = LWN_Get_Parent(def_wn);
        }
        if (WN_operator(def_wn) == OPR_CALL &&
            WN_st(def_wn) == distr_st_entries[Alloc_Reshape]) return TRUE;
      }
    }
  }

  return FALSE;
}


/* 
 * Is the given node the base of an array expression?
 */
mBOOL 
Is_Array_Base(const WN *curr_nd)
{
   WN *parent = LWN_Get_Parent(curr_nd);
   if (!parent) return FALSE; 

   mBOOL result = ((WN_operator(parent) == OPR_ARRAY) &&
                   (WN_array_base(parent) == curr_nd));
   return result;
}


/*
 * Is the entire array being passed as a parameter?
 */
mBOOL
Is_Array_Param(const WN *curr_nd)
{
  Is_True (LNO_Use_Parm,
           ("LNO_Use_Parm should be always on\n"));
  if ((WN_operator(curr_nd) == OPR_LDA) ||
      (WN_operator(curr_nd) == OPR_LDID)) {

    WN *parent = LWN_Get_Parent(curr_nd);
    if ((WN_operator(parent) == OPR_PARM) &&
        (OPCODE_is_call(WN_opcode(LWN_Get_Parent(parent))) ||
         WN_operator(LWN_Get_Parent(parent)) == OPR_INTRINSIC_OP
#ifdef KEY
         || WN_operator(LWN_Get_Parent(parent)) == OPR_PURE_CALL_OP
#endif
	 ))
      return TRUE;
  }
    
  return FALSE;
}


/*
 * See if array_st is a reshaped array, and if so return the distribution
 */
DISTR_ARRAY*
Lookup_Array_Distr(ST *array_st)
{
  if (Has_Reshaped_Type(array_st)) {
    DISTR_ARRAY *dact = Lookup_DACT(array_st);
    if (dact && dact->Dinfo()->IsReshaped()) return dact;
  }

  return NULL;
}

 
/*
 * If there is a reshape pragma, then the type of the array_st has been
 * modified to be a **array_element_type.  By checking if the array_st
 * has a reshaped type it saves hash table lookups.
 */
mBOOL 
Has_Reshaped_Type(ST *array_st)
{
  if (ST_class(array_st) != CLASS_VAR) return FALSE;

  TY_IDX array_ty = ST_type(array_st);
  return ((TY_kind(array_ty) == KIND_POINTER) &&
          (TY_kind(TY_pointed(array_ty)) == KIND_POINTER));
}
