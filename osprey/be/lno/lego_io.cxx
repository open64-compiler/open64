/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//                     LEGO I/O
//                     --------
//
// Description:
//
//	Deal with Fortran I/O on distribute-reshared arrays
//	We use the following strategy.  If we're doing I/O on single
//	elements of a reshaped array, do nothing.  The normal mechanism
//	is good enough.  If we're doing I/O on the whole array 
//	do I/O on a temporary array instead
//	and copy the temporary to/from the original as needed.
//	
//	If a reshaped array is refered to once in an I/O statement and if
//	it's inside an implied do and certain dimensions of the array reference
//	are constant, then copy only the non-constant dimensions into a smaller
//	temporary array.  If we're not sure that some dimensions are constant,
//      do nothing (just like for single elements).
//
// Exported Functions:
//
//	void Fix_Up_Loop_Info(WN *IO_node, WN **loops, INT num_dims)
//
//	Fix up the loop infos for copy-in or copy-out. 'IO_node' is the
//	tree for which the copy-in or copy-out is done. 'loops' is an
//	array of WN* which contain the loop nest for copy. 'num_dims'
//	gives the size of the array 'loops'.
//
/* ====================================================================
 * ====================================================================
 *
 * Module: lego_io.cxx
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:57:13-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lego_io.cxx $
 *
 * Revision history:
 *  09-07-96 - Original Version
 *
 * Description: Lego I/O
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lego_io.cxx $ $Revision: 1.5 $";

#include <sys/types.h>
#include <alloca.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "lnopt_main.h"
#include "config_targ.h"
#include "erbe.h"
#include "stab.h"
#include "strtab.h"
#include "stblock.h"
#include "lwn_util.h"
#include "dep.h"
#include "lnoutils.h"
#include "lego_util.h"
#include "lego_pragma.h"
#include "scalar_expand.h"
#include "wio.h"          // From "common/com"
#include "lego_affinity.h"
#include "debug.h"

enum REF_TYPE {SINGLE_ELEMENT_REF,IMPLIED_DO_REF,TOTAL_REF,OTHER_REF};

class REFERENCE
{
public:
  REF_TYPE Ref_Type;
  mBOOL Array_Loaded;
  WN *Ref;
  REFERENCE(WN *ref, REF_TYPE ref_type, BOOL array_loaded) { 
	Ref=ref; Ref_Type = ref_type; Array_Loaded = array_loaded; }
};

typedef STACK<WN *> STACK_OF_WN;
typedef STACK<REFERENCE> STACK_OF_REFERENCES;

static ST* Create_Tmp_Array(ST *st, WN *IO_node, WN **tmp_array_def,
				mBOOL *const_dimensions=FALSE);
static void Copy_Array(WN *orig_array, ST* local_st, WN *IO_node, 
			BOOL copy_in, WN *tmp_array_def);
static void Copy_Array_Section(WN *orig_array, ST* local_st, WN *IO_node, 
		BOOL copy_in, WN *tmp_array_def, mBOOL *const_dimensions);
static void Get_IOS_Reshaped_Array_Refs(WN *io_stmt, STACK_OF_WN *namelists, 
			    STACK_OF_REFERENCES *refs); 
static void Lego_Fix_IO_Rec(WN *wn, STACK_OF_WN *namelists,
				BOOL *has_do_loops);
static BOOL Constant_Dimension(WN *wn, mBOOL *well_behaved, BOOL nrs_var_read);
static void Set_Constant_Dimensions(WN *ref, mBOOL *const_dimensions, 
				INT num_dims, mBOOL *well_behaved);
// Fix up the loop infos
extern void Fix_Up_Loop_Info(WN *IO_node, WN **loops, INT num_dims);

// Copy all I/Oed reshaped arrays into temporary non-reshaped arrays
void Lego_Fix_IO(WN *func_nd, BOOL *has_do_loops)
{
  STACK_OF_WN namelists(Malloc_Mem_Pool);
  Lego_Fix_IO_Rec(func_nd,&namelists,has_do_loops);
}

static inline BOOL ST_is_reshaped_var(ST *st)
{
  return ST_class(st) == CLASS_VAR && ST_is_reshaped(st);
}

// are there any reshaped arrays underneath 
static BOOL Contains_Reshaped_Array(WN *wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      if (Contains_Reshaped_Array(kid)) return TRUE;
      kid = WN_next(kid);
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      if (Contains_Reshaped_Array(WN_kid(wn,kidno))) return TRUE;
    }
  }
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_LDID) || (oper == OPR_LDA)) {
    if (ST_is_reshaped_var(WN_st(wn))) {
      return TRUE;
    }
  }
  return FALSE;
}

// Is this a valtmp
static BOOL Is_Valtmp(WN *wn)
{
  if (WN_operator(wn) == OPR_INTRINSIC_OP) {
    switch (WN_intrinsic(wn)) {

      case INTRN_I4VALTMP:	
      case INTRN_I8VALTMP:	
      case INTRN_U4VALTMP:
      case INTRN_U8VALTMP:
      case INTRN_F4VALTMP:	
      case INTRN_F8VALTMP:	
      case INTRN_FQVALTMP:	
      case INTRN_C4VALTMP:	
      case INTRN_C8VALTMP:	
      case INTRN_CQVALTMP:  return TRUE;
    }
  }
  return FALSE;
}



// are there any calls inside wn
// are there any intrinsic ops that take a reshaped array as 
// an argument
static BOOL Contains_Calls(WN *wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      if (Contains_Calls(kid)) return TRUE;
      kid = WN_next(kid);
    }
  } else if (OPCODE_is_call(opcode)) {
    return TRUE;
  } else if (OPCODE_operator(opcode) == OPR_INTRINSIC_OP
#ifdef KEY
	     || OPCODE_operator(opcode) == OPR_PURE_CALL_OP
#endif
            ) {
    if (Contains_Reshaped_Array(wn)) {
      return TRUE;
    }
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      if (Contains_Calls(WN_kid(wn,kidno))) return TRUE;
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      if (Contains_Calls(WN_kid(wn,kidno))) return TRUE;
    }
  }
  return FALSE;
}

// Change the I/O to use the tmp array rather than the original array
// Substitute an LDID/LDA of orig_base with an LDID of tmp_array
// if some dimensions aren't used, we need to create a new OPR_ARRAY deleting
//	the unused dimensions
static void Substitute_Array(WN *orig_base, WN *tmp_array_def,
				INT num_dims, mBOOL *const_dimensions)
{
  WN *orig_array = LWN_Get_Parent(orig_base);
  INT kidno=0;
  while (WN_kid(orig_array,kidno) != orig_base) kidno++;

  WN *io = orig_base;
  while (WN_opcode(io) != OPC_IO) io = LWN_Get_Parent(io);


  // Fix the base

  WN *ldid = LWN_CreateLdid(
    OPCODE_make_op(OPR_LDID,Pointer_type,Pointer_type),
	tmp_array_def);
  // attch the def use to the IO node rather than the ldid to match preopt
  Du_Mgr->Add_Def_Use(tmp_array_def,io);
  LWN_Delete_Tree(WN_kid(orig_array,kidno));
  WN_kid(orig_array,kidno) = ldid;
  LWN_Set_Parent(ldid,orig_array);

  // Fix the indexing if some dimensions have been collapsed away
  if (const_dimensions) { // need to erase the unused dimensions
    INT num_used_dimensions=0;
    INT i;
    for (i=0; i<num_dims; i++) {
      if (!const_dimensions[i]) num_used_dimensions++;
    }
    Is_True(num_used_dimensions,("Completely constant ref in Substitute_Array"));
    WN *parent = LWN_Get_Parent(orig_array);
    INT kidno=0;
    while (WN_kid(parent,kidno) != orig_array) kidno++;

    WN *new_array = WN_Create(WN_opcode(orig_array),1+2*num_used_dimensions);
    WN_element_size(new_array) = WN_element_size(orig_array);
    WN_array_base(new_array) = LWN_Copy_Tree(WN_array_base(orig_array));
    INT j=0;
    for (i=0; i<num_used_dimensions; i++) {
      while (const_dimensions[j]) j++;
      WN_array_dim(new_array,i) = LWN_Copy_Tree(WN_array_dim(orig_array,j));
      WN_array_index(new_array,i) = LWN_Copy_Tree(WN_array_index(orig_array,j));
      j++;
    }
    for (i=0; i<WN_kid_count(new_array); i++) {
      LWN_Set_Parent(WN_kid(new_array,i),new_array);
    }
    LWN_Delete_Tree(orig_array);
    WN_kid(parent,kidno) = new_array;
    LWN_Set_Parent(new_array,parent);
  }
}

static void Lego_Fix_IO_Rec(WN *wn, STACK_OF_WN *namelists, BOOL *has_do_loops)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Lego_Fix_IO_Rec(kid,namelists,has_do_loops);
      kid = WN_next(kid);
    }
  } else if (opcode == OPC_IO) {
    MEM_POOL_Push(&LNO_local_pool);
    STACK_OF_REFERENCES *refs = 
	CXX_NEW(STACK_OF_REFERENCES(&LNO_local_pool),&LNO_local_pool);

    Get_IOS_Reshaped_Array_Refs(wn,namelists,refs);

    if (refs->Elements()) {
      if (Contains_Calls(wn)) {
	ErrMsgSrcpos(EC_LNO_Generic_Error, WN_Get_Linenum(wn),
	  "We currently do not support IO statements containing calls and reshaped arrays\n");
      }
    }

    ST **array_sts = CXX_NEW_ARRAY(ST *, refs->Elements(), &LNO_local_pool);
    INT i;
    for (i=0; i<refs->Elements(); i++) {
      array_sts[i] = WN_st(refs->Bottom_nth(i).Ref);
    }
    for (i=0; i<refs->Elements(); i++) {
      ST *st = array_sts[i];
      BOOL array_loaded = refs->Bottom_nth(i).Array_Loaded;
      BOOL non_single_element_refs = 
		(refs->Bottom_nth(i).Ref_Type != SINGLE_ELEMENT_REF);
      BOOL non_array_ref = 
		(refs->Bottom_nth(i).Ref_Type == OTHER_REF);
      BOOL total_ref = (refs->Bottom_nth(i).Ref_Type == TOTAL_REF);
      BOOL array_stored = !array_loaded;
      // make sure we haven't done it before
      BOOL done_before = FALSE;
      for (INT j=0; j<i && !done_before; j++) {
        ST *j_st = array_sts[j];
	if (j_st == st) {
	  done_before = TRUE;
        }
      }
      if (!done_before) {
	INT multiple_refs = FALSE;
        INT j;
        for (j=i+1; j<refs->Elements(); j++) {
	  if (array_sts[j] == st) {
	    multiple_refs = TRUE;
	    if (refs->Bottom_nth(j).Ref_Type != SINGLE_ELEMENT_REF) {
	      non_single_element_refs = TRUE;
            }
	    if (refs->Bottom_nth(j).Ref_Type == OTHER_REF) {
	      non_array_ref = TRUE;
            } else if (refs->Bottom_nth(j).Ref_Type == TOTAL_REF) {
	      total_ref = TRUE;
            }
	    array_loaded |= refs->Bottom_nth(j).Array_Loaded;
	    array_stored |= !(refs->Bottom_nth(j).Array_Loaded);
          }
	}

        DISTR_ARRAY* dact = Lookup_DACT (array_sts[i]);
        INT num_dims = dact->Dinfo()->Num_Dim();
	if (non_single_element_refs) { // don't substiture single element io
	  if (!multiple_refs && (refs->Bottom_nth(i).Ref_Type == IMPLIED_DO_REF)) {
	    MEM_POOL_Push(&LNO_local_pool);
	    mBOOL *const_dimensions=
	      const_dimensions=CXX_NEW_ARRAY(mBOOL,num_dims,&LNO_local_pool);
            mBOOL well_behaved = TRUE;
	    Set_Constant_Dimensions(refs->Bottom_nth(i).Ref, const_dimensions,
						num_dims, &well_behaved);
	    BOOL contains_non_const = FALSE;
	    for (INT count=0; count<num_dims && !contains_non_const; count++) {
	      if (!const_dimensions[count]) contains_non_const = TRUE;
            }
	    if (well_behaved && contains_non_const) {
	      WN *orig_array = LWN_Get_Parent(refs->Bottom_nth(i).Ref);
	      WN *tmp_array_def;
	      ST* tmp_array = 
	       Create_Tmp_Array(st,wn,&tmp_array_def, const_dimensions);
              if (array_loaded) {
		Copy_Array_Section(orig_array, tmp_array,wn,TRUE,tmp_array_def,
						const_dimensions);
                *has_do_loops = TRUE;
              }
              if (array_stored) {
		Copy_Array_Section(orig_array, tmp_array,wn,FALSE,tmp_array_def,
						const_dimensions);
                *has_do_loops = TRUE;
              }
	      Substitute_Array(refs->Bottom_nth(i).Ref,tmp_array_def,
				num_dims,const_dimensions);
	    }
	    CXX_DELETE_ARRAY(const_dimensions,&LNO_local_pool);
	    MEM_POOL_Pop(&LNO_local_pool);
	  } else if (total_ref || non_array_ref || multiple_refs) {
	    WN *orig_array = LWN_Get_Parent(refs->Bottom_nth(i).Ref);
	    WN *tmp_array_def;
	    ST* tmp_array = Create_Tmp_Array(st,wn,&tmp_array_def);

            if (array_loaded || !total_ref) {
	      Copy_Array(orig_array,tmp_array,wn,TRUE,tmp_array_def);
              *has_do_loops = TRUE;
            }
            if (array_stored) {
	      Copy_Array(orig_array,tmp_array,wn,FALSE, tmp_array_def);
              *has_do_loops = TRUE;
            }

	    Substitute_Array(refs->Bottom_nth(i).Ref,tmp_array_def,
				num_dims,0);
            for (j=i+1; multiple_refs && j<refs->Elements(); j++) {
              ST *j_st = array_sts[j];
	      if (j_st == st) {
	        Substitute_Array(refs->Bottom_nth(j).Ref,tmp_array_def,
					num_dims,0);
              }
            }
          }
        }
      }
    }

    CXX_DELETE_ARRAY(array_sts,&LNO_local_pool);
    CXX_DELETE(refs,&LNO_local_pool);
    MEM_POOL_Pop(&LNO_local_pool);
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Lego_Fix_IO_Rec(WN_kid(wn,kidno),namelists,has_do_loops);
    }
  }
}

inline BOOL 
Is_IO_List_Item(WN *item)
{
   return (WN_io_item(item) >= IOL_ARRAY && WN_io_item(item) <= IOL_VAR);
}

// Are any non-reshaped variables read anywhere in the I/O statement
// Given something like read *,x,(a(i,n), i=1,n), we want
// to know that 'n' is constant throughout the I/O statement, but we can't
// tell because 'n' might be aliased to 'x' and we don't have alias information
// There are two cases, though, where we know for sure
//   1. the I/O statement is a write (no scalars can be read)
//   2. the reshaped array is the only thing being read


BOOL Nrs_Var_Read(WN *io)
{
  if ((WN_io_statement(io) == IOS_READ) ||
      (WN_io_statement(io) == IOS_CR_FRF) ||
      (WN_io_statement(io) == IOS_CR_FRU) ||
      (WN_io_statement(io) == IOS_ACCEPT) ||
      (WN_io_statement(io) == IOS_DECODE)) {
    INT count = 0;
    for (INT i = 0; i < WN_kid_count(io); i++) {
      WN *item = WN_kid(io, i);
      if (Is_IO_List_Item(item)) {
        count++;
	if (count > 1) return TRUE;
      }
    }
    return FALSE;
  } else {
    return FALSE;
  }
}

// Is this loop bound constant (or triangular) in the I/O statement
static BOOL Loop_Bound_Constant(WN *wn, BOOL nrs_var_read)
{
  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);
  if (OPCODE_is_load(opcode)) {
    if (nrs_var_read) return FALSE;
    if ((oper == OPR_LDID) || (oper == OPR_LDA)) {
      if (ST_is_reshaped_var(WN_st(wn))) { 
	return FALSE;
      }
    }
  } else if (oper == OPR_INTRINSIC_OP) {
    if (Is_Valtmp(wn)) {
      return 
	Loop_Bound_Constant(WN_kid0(wn),nrs_var_read);
    }
    return FALSE;
  }
#ifdef KEY
  else if (oper == OPR_PURE_CALL_OP)
    return FALSE;
#endif
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    if (!Loop_Bound_Constant(WN_kid(wn,kidno),nrs_var_read)) {
      return FALSE;
    }
  }
  return TRUE;
}

// Given an implied do io, are all the bounds of the implied do 
// constant in the I/O statement and are all the steps literal
static BOOL Loop_Bounds_Constant(WN *ref, BOOL nrs_var_read)
{
  WN *do_loop=LWN_Get_Parent(ref);
  while (WN_opcode(do_loop) != OPC_IO) {
    do_loop = LWN_Get_Parent(do_loop);
    OPERATOR oper = WN_operator(do_loop);
    if ((oper == OPR_IO_ITEM) &&
        (WN_io_item(do_loop) == IOL_IMPLIED_DO)) {
      if (!Loop_Bound_Constant(WN_start(do_loop),nrs_var_read)) {
	return FALSE;
      }
      if (!Loop_Bound_Constant(WN_end(do_loop),nrs_var_read)) {
	return FALSE;
      }
      if (WN_operator(WN_step(do_loop)) != OPR_INTCONST) {
	return FALSE;
      }
    }
  }
  return TRUE;
}

// Given a reference inside an implied do loop, figure out which dimensions of the
// array are constant.  If necessary, conservatively set them all to be non-constant
//
// set *well_behaved to false if we're not sure if a dimension is being used
//	or if the bounds of the do loop is not constant
static void Set_Constant_Dimensions(WN *ref, mBOOL *const_dimensions, INT num_dims,
					mBOOL *well_behaved)
{
  WN *array = LWN_Get_Parent(ref);
  WN *parent = array;
  while (WN_opcode(parent) != OPC_IO) parent = LWN_Get_Parent(parent);
  BOOL nrs_var_read = Nrs_Var_Read(parent);
  if (!Loop_Bounds_Constant(array,nrs_var_read)) {
    *well_behaved = FALSE;
    return;
  }
  for (INT i=0; i<num_dims; i++) {
    const_dimensions[i] = Constant_Dimension(WN_array_index(array,i),
	well_behaved, nrs_var_read);
  }
}


// Is a particular subtree constant inside the I/O statement
static BOOL Constant_Dimension(WN *wn, mBOOL *well_behaved, BOOL nrs_var_read)
{

  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);
  if (OPCODE_is_load(opcode)) {
    if (nrs_var_read) {
      *well_behaved = FALSE;
      return FALSE; // because of aliaing must be conservative
    }
    if (oper != OPR_LDID) {
      for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
        if (!Constant_Dimension(WN_kid(wn,kidno),well_behaved,nrs_var_read)) {
	  return FALSE;
        }
      }
    } else {
      ST *st = WN_st(wn);
      if (ST_class(st) == CLASS_PREG) return TRUE; // can't read pregs
      // return FALSE iff the scalar is an implied do variable
      WN *do_loop = LWN_Get_Parent(wn);
      OPCODE opcode = WN_opcode(do_loop);
      while (opcode != OPC_IO) {
        OPERATOR oper = OPCODE_operator(opcode);
        if ((oper == OPR_IO_ITEM) &&
            (WN_io_item(do_loop) == IOL_IMPLIED_DO)) {
          WN *index = WN_index(do_loop);
          if ((WN_st(index) == st) &&
	    (WN_offset(index) == WN_offset(wn))) {
            return FALSE;
          }
        }
        do_loop = LWN_Get_Parent(do_loop);
        opcode = WN_opcode(do_loop);
      }
      return TRUE;
    }
  } else if (oper == OPR_LDA) {
    if (ST_is_reshaped_var(WN_st(wn))) {
      *well_behaved = FALSE;
      return FALSE;
    }
    return TRUE;
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      if (!Constant_Dimension(WN_kid(wn,kidno),well_behaved,nrs_var_read)) {
	return FALSE;
      }
    }
  }
  return TRUE;
}


// Create a temporary array to be used inside the I/O node
static ST* Create_Tmp_Array(ST *array_st, WN *IO_node, WN **tmp_array_def,
				mBOOL *const_dimensions)
{
  SYMBOL symb;
  static INT number;
  TY_IDX element_type;
  if (TY_kind(ST_type(array_st)) == KIND_POINTER) {
    element_type = TY_AR_etype(TY_pointed(ST_type(array_st)));
  } else {
    Is_True(TY_kind(ST_type(array_st)) == KIND_ARRAY,
	("Non-array,non-pointer in Create_Tmp_Array"));
    element_type = TY_AR_etype(ST_type(array_st));
  }
  TYPE_ID machine_element_type = TY_mtype(element_type);


  DISTR_ARRAY* dact = Lookup_DACT (array_st);
  INT j=0;
  if (const_dimensions) {
    while (const_dimensions[j]) j++;
  } 
  INT element_size;
  switch(machine_element_type) {
    case MTYPE_I1: case MTYPE_U1: element_size=1; break; 
    case MTYPE_I2: case MTYPE_U2: element_size=2; break;
    case MTYPE_I4: case MTYPE_U4: case MTYPE_F4: element_size=4; break;
    case MTYPE_I8: case MTYPE_U8: case MTYPE_F8: case MTYPE_C4: element_size=8; break;
#if defined(TARG_IA64) || defined(TARG_X8664)
    case MTYPE_F10: element_size=16; break;
    case MTYPE_C10: element_size=32; break;
#endif    
    case MTYPE_C8: case MTYPE_FQ: element_size=16; break;
    case MTYPE_CQ: element_size=32; break;
  }

  WN *bsz = LWN_CreateExp2(OPCODE_make_op(OPR_MPY,Pointer_type,MTYPE_V),
    	LWN_Make_Icon(Pointer_type,element_size),dact->Array_Size_WN(j));
  INT num_dims = dact->Dinfo()->Num_Dim();
  for (INT i=j+1; i<num_dims; i++) {
    if (!const_dimensions || !const_dimensions[i]) {
      bsz = LWN_CreateExp2(OPCODE_make_op(OPR_MPY,Pointer_type,MTYPE_V),
    			bsz,dact->Array_Size_WN(i));
    }
  }

  SE_Symbols_For_SE(&symb,"tmp_io",number,machine_element_type);
  *tmp_array_def = 
	Get_Expansion_Space(symb,bsz,"tmp_io",number++,machine_element_type,
		IO_node,IO_node,IO_node);
  return symb.St();
}

// For every load in expr, copy all the Defs from the I/O's def-list to the load
// This is conservative, but we have no choice since we have no DU inside IO
// Except
//	if it's an ldid of one of the dos, set the def to the start and step
//      don't copy any edges from I/O to itself
//	don't copy edges that alias analysis say can't possibly be real
static void IO_Copy_Defs(WN *io, WN *expr, WN **loops, INT num_loops)
{
  OPCODE opc = WN_opcode(expr);
  if (OPCODE_is_load(opc)) {
    OPERATOR oper = OPCODE_operator(opc);
    BOOL is_index = FALSE;
    WN *index;
    if (oper == OPR_LDID) {
      for (INT i=0; i<num_loops && !is_index; i++) {
	if (WN_st(expr) == WN_st(WN_start(loops[i]))) {
	  if (WN_offset(expr) == WN_offset(WN_start(loops[i]))) {
	    is_index = TRUE;
	    index = loops[i];
          }
        }
      }
    } else {
      for (INT kidno=0; kidno<WN_kid_count(expr); kidno++) {
        IO_Copy_Defs(io,WN_kid(expr,kidno),loops,num_loops);
      }
    }
    if (is_index) {
      Du_Mgr->Add_Def_Use(WN_start(index),expr);
      Du_Mgr->Add_Def_Use(WN_step(index),expr);
      DEF_LIST* deflist = Du_Mgr->Ud_Get_Def(expr);
      deflist->Set_loop_stmt(index);
    } else {
      DEF_LIST* deflist = Du_Mgr->Ud_Get_Def(io);
      if (deflist) {
        DEF_LIST_ITER iter(deflist);
        for (DU_NODE* n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
          WN* def = n->Wn();
          if (def != io) {
	    if (deflist->Incomplete()) {
	        Du_Mgr->Add_Def_Use(def, expr);
	    } else if ((oper == OPR_LDID) ||
	        (WN_operator(def) == OPR_STID)) {
	      if (!OPCODE_is_store(WN_opcode(def)) || 
		  (Aliased(Alias_Mgr,expr,def) != NOT_ALIASED)) {
	        Du_Mgr->Add_Def_Use(def, expr);
              }
            }
          }
        }
        DEF_LIST* deflist2 = Du_Mgr->Ud_Get_Def(expr);
        if (deflist2) deflist2->Set_loop_stmt(deflist->Loop_stmt());
        if (deflist->Incomplete()) deflist2->Set_Incomplete();
      }
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(expr); kidno++) {
      IO_Copy_Defs(io,WN_kid(expr,kidno),loops,num_loops);
    }
  }
}


// Copy-in or copy-out an array before/after an I/O node
static void Copy_Array(WN *orig_array, ST* local_st, WN *IO_node, 
			BOOL copy_in, WN *tmp_array_def)
{
  ST *array_st = WN_st(WN_array_base(orig_array));
  MEM_POOL_Push(&LNO_local_pool);
  DISTR_ARRAY* dact = Lookup_DACT (array_st);
  Is_True(dact && dact->Dinfo()->IsReshaped(),
	("Copy_Array called on non-reshaped array "));
  INT num_dims = dact->Dinfo()->Num_Dim();
  WN *alias_host = NULL;

  // Create a num_dims dimensional loop nest
  WN *insert_before;
  WN *insert_parent = LWN_Get_Parent(IO_node);
  WN *do_loop;
  if (copy_in) {
    insert_before = IO_node;
  } else {
    insert_before = WN_next(IO_node);
  }

  TYPE_ID index_type;
// >> WHIRL 0.30: Added MTYPE_A8
// TODO WHIRL 0.30: get rid of MTYPE_I8 and MTYPE_U8
  if ((Pointer_type == MTYPE_A8) || (Pointer_type == MTYPE_U8) ||
      (Pointer_type == MTYPE_I8)) {
// << WHIRL 0.30: Added MTYPE_A8
    index_type = MTYPE_I8;
  } else {
    index_type = MTYPE_I4;
  }

  char name[20];
  WN **loop_starts = CXX_NEW_ARRAY(WN *,num_dims,&LNO_local_pool);
  WN **loop_steps = CXX_NEW_ARRAY(WN *,num_dims,&LNO_local_pool);
  WN **loops = CXX_NEW_ARRAY(WN *,num_dims,&LNO_local_pool);
  INT i;
  for (i=0; i<num_dims; i++) {
    sprintf(name,"copy_%d",i);
    WN_OFFSET index_var_num;
    ST* index_var_st;
#ifdef _NEW_SYMTAB
    index_var_num = Create_Preg(index_type,name);
#else
    index_var_num = Create_Preg(index_type,name,NULL);
#endif
    index_var_st = MTYPE_To_PREG(index_type);
    WN *index = WN_CreateIdname(index_var_num,index_var_st);
    WN *start = LWN_CreateStid(OPCODE_make_op(OPR_STID,MTYPE_V,index_type),
	index_var_num, index_var_st,Be_Type_Tbl(index_type),
	LWN_Make_Icon(index_type,0));
    Create_alias(Alias_Mgr,start);
    LWN_Copy_Linenumber(IO_node,start);
    loop_starts[i] = start;
    WN *end_use = 
	LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),start);
    WN *num_iters = dact->Array_Size_WN(i);
    WN *end = LWN_CreateExp2
       (OPCODE_make_op(OPR_LT,Boolean_type,index_type),end_use, num_iters);
    WN *step_use = 
	LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),start);
    WN *add = LWN_CreateExp2(OPCODE_make_op(OPR_ADD,index_type,MTYPE_V),
			 	step_use, LWN_Make_Icon(index_type,1));
    WN *step = LWN_CreateStid(WN_opcode(start),start,add);
    LWN_Copy_Linenumber(IO_node,step);
    loop_steps[i] = start;
    do_loop = LWN_CreateDO(index,start,end,step,WN_CreateBlock());
    loops[i] = do_loop;
    LWN_Copy_Linenumber(IO_node,do_loop);
    LWN_Copy_Linenumber(IO_node,WN_do_body(do_loop));
    LWN_Insert_Block_Before(insert_parent,insert_before,do_loop);
    insert_before = NULL;
    insert_parent = WN_do_body(do_loop);

    Du_Mgr->Add_Def_Use(start,end_use);
    Du_Mgr->Add_Def_Use(step,end_use);
    Du_Mgr->Add_Def_Use(start,step_use);
    Du_Mgr->Add_Def_Use(step,step_use);
    DEF_LIST *deflist = Du_Mgr->Ud_Get_Def(end_use);
    deflist->Set_loop_stmt(do_loop);
    deflist = Du_Mgr->Ud_Get_Def(step_use);
    deflist->Set_loop_stmt(do_loop);
    
  }


  TY_IDX element_type;
  if (TY_kind(ST_type(array_st)) == KIND_POINTER) {
    element_type = TY_AR_etype(TY_pointed(ST_type(array_st)));
  } else {
    Is_True(TY_kind(ST_type(array_st)) == KIND_ARRAY,
	("Non-array,non-pointer in Copy_Array"));
    element_type = TY_AR_etype(ST_type(array_st));
  }
  TYPE_ID machine_element_type = TY_mtype(element_type);

  // now do the copy
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY,Pointer_type,MTYPE_V);

  WN *array_base=dact->Dinfo()->Load_Distr_Array();

  WN *local_base =
    LWN_CreateLdid(OPCODE_make_op(OPR_LDID,Pointer_type,Pointer_type),tmp_array_def);
  Du_Mgr->Add_Def_Use(tmp_array_def,local_base);


  WN *array_wn = WN_Create(op_array,1+2*num_dims);
  WN *local_wn = WN_Create(op_array,1+2*num_dims);
  WN_element_size(array_wn) = TY_size(element_type);
  WN_element_size(local_wn) = TY_size(element_type);
  WN_array_base(array_wn) = array_base;
  WN_array_base(local_wn) = local_base;

  for (i=0; i<num_dims; i++) {
    WN_array_index(local_wn,i) = 
      LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),loop_starts[i]);
    Du_Mgr->Add_Def_Use(loop_starts[i],WN_array_index(local_wn,i));
    Du_Mgr->Add_Def_Use(loop_steps[i],WN_array_index(local_wn,i));
    DEF_LIST *deflist = Du_Mgr->Ud_Get_Def(WN_array_index(local_wn,i));
    deflist->Set_loop_stmt(loops[i]);
    WN_array_dim(local_wn,i) = dact->Array_Size_WN(i);
  }

  for (i=0; i<num_dims; i++) {
    WN_array_index(array_wn,i) = 
        LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),loop_starts[i]);
    Du_Mgr->Add_Def_Use(loop_starts[i],WN_array_index(array_wn,i));
    Du_Mgr->Add_Def_Use(loop_steps[i],WN_array_index(array_wn,i));
    DEF_LIST *deflist = Du_Mgr->Ud_Get_Def(WN_array_index(array_wn,i));
    deflist->Set_loop_stmt(loops[i]);
    WN_array_dim(array_wn,i) = dact->Array_Size_WN(i);
  }
  for (i=0; i<WN_kid_count(array_wn); i++) {
    LWN_Set_Parent(WN_kid(array_wn,i),array_wn);
  }
  for (i=0; i<WN_kid_count(local_wn); i++) {
    LWN_Set_Parent(WN_kid(local_wn,i),local_wn);
  }
  WN *store;
  if (copy_in) { // store array into local
    WN *value = LWN_CreateIload(
	OPCODE_make_op(OPR_ILOAD,machine_element_type,machine_element_type),
	0,element_type,Make_Pointer_Type(element_type),array_wn);
    if (ST_sclass(array_st) == SCLASS_FORMAL) {
      Create_formal_alias(Alias_Mgr,WN_st(array_base),array_base,value);
    } else if (WN_operator(array_base)==OPR_LDA) {
      Create_lda_array_alias(Alias_Mgr,array_base,value);
    } else {
      Create_unique_pointer_alias(Alias_Mgr,WN_st(array_base),array_base,value);
    }

    store = LWN_CreateIstore(
	OPCODE_make_op(OPR_ISTORE,MTYPE_V,machine_element_type),
	0,Make_Pointer_Type(element_type),value,local_wn);
    if (alias_host==NULL) {
      Create_unique_pointer_alias(Alias_Mgr,local_st,NULL, store);
      alias_host = store;
    } else {
      Copy_alias_info(Alias_Mgr, alias_host, store);
    }

    LWN_Copy_Linenumber(IO_node,store);
  } else {
    WN *value = LWN_CreateIload(
	OPCODE_make_op(OPR_ILOAD,machine_element_type,machine_element_type),
	0,element_type,Make_Pointer_Type(element_type),local_wn);
    if (alias_host==NULL) {
      Create_unique_pointer_alias(Alias_Mgr,local_st,NULL, value);
      alias_host = value;
    } else {
      Copy_alias_info(Alias_Mgr, alias_host, value);
    }

    store = LWN_CreateIstore(
	OPCODE_make_op(OPR_ISTORE,MTYPE_V,machine_element_type),
	0,Make_Pointer_Type(element_type),value,array_wn);
    if (ST_sclass(array_st) == SCLASS_FORMAL) {
      Create_formal_alias(Alias_Mgr,WN_st(array_base),array_base,store);
    } else if (WN_operator(array_base)==OPR_LDA) {
      Create_lda_array_alias(Alias_Mgr,array_base,store);
    } else {
      Create_unique_pointer_alias(Alias_Mgr,WN_st(array_base),array_base,store);
    }
    LWN_Copy_Linenumber(IO_node,store);
  }

  Fix_Up_Loop_Info(IO_node,loops,num_dims);

  LWN_Insert_Block_Before(WN_do_body(do_loop),NULL,store);
  CXX_DELETE_ARRAY(loop_starts,&LNO_local_pool);
  CXX_DELETE_ARRAY(loop_steps,&LNO_local_pool);
  CXX_DELETE_ARRAY(loops,&LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
}

// Copy a section of an array to/from a tmp
static void Copy_Array_Section(WN *orig_array, ST* local_st, WN *IO_node, 
		BOOL copy_in, WN *tmp_array_def, mBOOL *const_dimensions)
{
  ST *array_st = WN_st(WN_array_base(orig_array));
  MEM_POOL_Push(&LNO_local_pool);
  DISTR_ARRAY* dact = Lookup_DACT (array_st);
  Is_True(dact && dact->Dinfo()->IsReshaped(),
	("Copy_Array called on non-reshaped array "));
  INT array_dims = dact->Dinfo()->Num_Dim();
  INT local_dims = array_dims;
  INT i;
  for (i=0; i<array_dims; i++) {
    if (const_dimensions[i]) {
	local_dims--;
    }
  }
  WN *alias_host = NULL;

  // Create a real do loop mimicking the implied to loop
  STACK_OF_WN *implied_dos = CXX_NEW(STACK_OF_WN(&LNO_local_pool),&LNO_local_pool);
  WN *idos = LWN_Get_Parent(orig_array);
  while (WN_opcode(idos) != OPC_IO) {
    idos = LWN_Get_Parent(idos);
    OPERATOR oper = WN_operator(idos);
    if ((oper == OPR_IO_ITEM) &&
      (WN_io_item(idos) == IOL_IMPLIED_DO)) {
      implied_dos->Push(idos);
    }
  }

  WN *insert_before;
  WN *insert_parent = LWN_Get_Parent(IO_node);
  WN *do_loop;
  if (copy_in) {
    insert_before = IO_node;
  } else {
    insert_before = WN_next(IO_node);
  }

  WN **loop_starts = CXX_NEW_ARRAY(WN *,implied_dos->Elements(),&LNO_local_pool);
  WN **loop_steps = CXX_NEW_ARRAY(WN *,implied_dos->Elements(),&LNO_local_pool);
  WN **loops = CXX_NEW_ARRAY(WN *,implied_dos->Elements(),&LNO_local_pool);
  for (i=0; i<implied_dos->Elements(); i++) {
    WN *implied_do = implied_dos->Top_nth(i);
    TYPE_ID index_type = WN_rtype(WN_start(implied_do));

    WN *index = WN_CreateIdname(WN_offset(WN_index(implied_do)),WN_st(WN_index(implied_do)));
    WN *start = LWN_CreateStid(OPCODE_make_op(OPR_STID,MTYPE_V,index_type),
                               WN_offset(WN_index(implied_do)),
                               WN_st(WN_index(implied_do)),
                               Be_Type_Tbl(index_type),
                               LWN_Copy_Tree(WN_start(implied_do)));
    Create_alias(Alias_Mgr,start);
    LWN_Copy_Linenumber(IO_node,start);
    loop_starts[i] = start;
    WN *end;
    WN *end_use = LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),
							start);
    WN *end_bound;
    if (Is_Valtmp(WN_end(implied_do))) {
      end_bound = LWN_Copy_Tree(WN_kid0(WN_end(implied_do)));
    } else {
      end_bound = LWN_Copy_Tree(WN_end(implied_do));
    }

    if (WN_const_val(WN_step(implied_do)) > 0) {
      end = LWN_CreateExp2(OPCODE_make_op(OPR_LE,Boolean_type,index_type),
       				end_use, end_bound);
    } else {
      end = LWN_CreateExp2(OPCODE_make_op(OPR_GE,Boolean_type,index_type),
       				end_use, end_bound);
    }
    WN *step_use = 
	LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),start);
    WN *add = LWN_CreateExp2(OPCODE_make_op(OPR_ADD,index_type,MTYPE_V),
			 	step_use, LWN_Copy_Tree(WN_step(implied_do)));
    WN *step = LWN_CreateStid(WN_opcode(start),start,add);

    LWN_Copy_Linenumber(IO_node,step);
    loop_steps[i] = start;
    do_loop = LWN_CreateDO(index,start,end,step,WN_CreateBlock());
    loops[i] = do_loop;
    LWN_Copy_Linenumber(IO_node,do_loop);
    LWN_Copy_Linenumber(IO_node,WN_do_body(do_loop));
    LWN_Insert_Block_Before(insert_parent,insert_before,do_loop);
    insert_before = NULL;
    insert_parent = WN_do_body(do_loop);

    Du_Mgr->Add_Def_Use(start,step_use);
    Du_Mgr->Add_Def_Use(step,step_use);
    DEF_LIST *deflist = Du_Mgr->Ud_Get_Def(step_use);
    deflist->Set_loop_stmt(do_loop);
  }
  INT num_loops = implied_dos->Elements();
  for (i=0; i<num_loops; i++) {
    WN *loop = loops[i];
    IO_Copy_Defs(IO_node,WN_end(loop),loops,num_loops);
  }

  TY_IDX element_type;
  if (TY_kind(ST_type(array_st)) == KIND_POINTER) {
    element_type = TY_AR_etype(TY_pointed(ST_type(array_st)));
  } else {
    Is_True(TY_kind(ST_type(array_st)) == KIND_ARRAY,
	("Non-array,non-pointer in Copy_Array"));
    element_type = TY_AR_etype(ST_type(array_st));
  }
  TYPE_ID machine_element_type = TY_mtype(element_type);

  // now do the copy
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY,Pointer_type,MTYPE_V);

  WN *array_base=dact->Dinfo()->Load_Distr_Array();

  WN *local_base =
    LWN_CreateLdid(OPCODE_make_op(OPR_LDID,Pointer_type,Pointer_type),tmp_array_def);
  Du_Mgr->Add_Def_Use(tmp_array_def,local_base);


  WN *array_wn = WN_Create(op_array,1+2*array_dims);
  WN *local_wn = WN_Create(op_array,1+2*local_dims);
  WN_element_size(array_wn) = TY_size(element_type);
  WN_element_size(local_wn) = TY_size(element_type);
  WN_array_base(array_wn) = array_base;
  WN_array_base(local_wn) = local_base;

  INT j=0;
  for (i=0; i<local_dims; i++) {
    if (const_dimensions) {
      while (const_dimensions[j]) j++;
    }
    WN *index = LWN_Copy_Tree(WN_array_index(orig_array,j));
    WN_array_index(local_wn,i) = index;
    IO_Copy_Defs(IO_node,index,loops,num_loops);
    WN_array_dim(local_wn,i) = dact->Array_Size_WN(j);
    j++;
  }

  j=0;
  for (i=0; i<array_dims; i++) {
    WN_array_index(array_wn,i) = LWN_Copy_Tree(WN_array_index(orig_array,i)); 
    IO_Copy_Defs(IO_node,WN_array_index(array_wn,i),loops,num_loops);
    WN_array_dim(array_wn,i) = dact->Array_Size_WN(i);
  }
  for (i=0; i<WN_kid_count(array_wn); i++) {
    LWN_Set_Parent(WN_kid(array_wn,i),array_wn);
  }
  for (i=0; i<WN_kid_count(local_wn); i++) {
    LWN_Set_Parent(WN_kid(local_wn,i),local_wn);
  }
  WN *store;
  if (copy_in) { // store array into local
    WN *value = LWN_CreateIload(
	OPCODE_make_op(OPR_ILOAD,machine_element_type,machine_element_type),
	0,element_type,Make_Pointer_Type(element_type),array_wn);
    if (ST_sclass(array_st) == SCLASS_FORMAL) {
      Create_formal_alias(Alias_Mgr,WN_st(array_base),array_base,value);
    } else if (WN_operator(array_base)==OPR_LDA) {
      Create_lda_array_alias(Alias_Mgr,array_base,value);
    } else {
      Create_unique_pointer_alias(Alias_Mgr,WN_st(array_base),array_base,value);
    }
    store = LWN_CreateIstore(
	OPCODE_make_op(OPR_ISTORE,MTYPE_V,machine_element_type),
	0,Make_Pointer_Type(element_type),value,local_wn);
    if (alias_host==NULL) {
      Create_unique_pointer_alias(Alias_Mgr,local_st,NULL, store);
      alias_host = store;
    } else {
      Copy_alias_info(Alias_Mgr, alias_host, store);
    }

    LWN_Copy_Linenumber(IO_node,store);
  } else {
    WN *value = LWN_CreateIload(
	OPCODE_make_op(OPR_ILOAD,machine_element_type,machine_element_type),
	0,element_type,Make_Pointer_Type(element_type),local_wn);
    if (alias_host==NULL) {
      Create_unique_pointer_alias(Alias_Mgr,local_st,NULL, value);
      alias_host = value;
    } else {
      Copy_alias_info(Alias_Mgr, alias_host, value);
    }

    store = LWN_CreateIstore(
	OPCODE_make_op(OPR_ISTORE,MTYPE_V,machine_element_type),
	0,Make_Pointer_Type(element_type),value,array_wn);
    if (ST_sclass(array_st) == SCLASS_FORMAL) {
      Create_formal_alias(Alias_Mgr,WN_st(array_base),array_base,store);
    } else if (WN_operator(array_base)==OPR_LDA) {
      Create_lda_array_alias(Alias_Mgr,array_base,store);
    } else {
      Create_unique_pointer_alias(Alias_Mgr,WN_st(array_base),array_base,store);
    }
    LWN_Copy_Linenumber(IO_node,store);
  }
  LWN_Insert_Block_Before(WN_do_body(do_loop),NULL,store);

  // replace all the loop variables with pregs
  // this improves alias analysis since the implied variables
  // are under I/O and therefore have too conservative info
  for (i=0; i<num_loops; i++) {
    WN *loop = loops[i];
    WN *implied_loop = implied_dos->Top_nth(i);
    TYPE_ID index_type = WN_rtype(WN_start(implied_loop));
    char name[20];
    sprintf(name,"copy_%d",i);
    WN_OFFSET index_var_num;
    ST *index_var_st;
#ifdef _NEW_SYMTAB
    index_var_num = Create_Preg(index_type,name);
#else
    index_var_num = Create_Preg(index_type,name,NULL);
#endif
    index_var_st = MTYPE_To_PREG(index_type);
    Replace_Symbol(loop,
		   SYMBOL(WN_st(WN_index(implied_loop)),
			  WN_offset(WN_index(implied_loop)),
			  index_type),
		   SYMBOL(index_var_st,index_var_num,index_type),
		   NULL,loop);
  }

  Fix_Up_Loop_Info(IO_node,loops,num_loops);

  CXX_DELETE(implied_dos,&LNO_local_pool);
  CXX_DELETE_ARRAY(loop_starts,&LNO_local_pool);
  CXX_DELETE_ARRAY(loop_steps,&LNO_local_pool);
  CXX_DELETE_ARRAY(loops,&LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
}

// Fix up the loop infos
extern void Fix_Up_Loop_Info(WN *IO_node, WN **loops, INT num_loops)
{
  WN *enclosing_loop = LWN_Get_Parent(IO_node);
  INT depth = -1;
  while (enclosing_loop && (WN_opcode(enclosing_loop) != OPC_DO_LOOP)) {
    enclosing_loop = LWN_Get_Parent(enclosing_loop);
  }
  if (enclosing_loop) {
    depth = Do_Loop_Depth(enclosing_loop);
  }
  while (enclosing_loop) {
    if (WN_opcode(enclosing_loop) == OPC_DO_LOOP) {
      DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,
		enclosing_loop);
      dli->Is_Inner = FALSE;
    }
    enclosing_loop = LWN_Get_Parent(enclosing_loop);
  }
  for (INT i=0; i<num_loops; i++) {
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *)
	CXX_NEW(DO_LOOP_INFO(&LNO_default_pool,NULL,NULL,NULL,FALSE,
          FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE) ,&LNO_default_pool);
    dli->Depth = depth+i+1;
    dli->Is_Backward = FALSE;
    dli->Has_Calls=FALSE;
    dli->Has_Unsummarized_Calls=FALSE;
    dli->Has_Gotos=FALSE;
    dli->Is_Inner = (i == (num_loops-1));
    dli->Has_Bad_Mem = FALSE;
    WN_MAP_Set(LNO_Info_Map,loops[i],(void *)dli);
  }
  Whack_Do_Loops(loops[0]);
}



inline OPERATOR 
WN_opc_operator(WN *wn)
{
   return WN_operator(wn);
}




static BOOL 
Is_In_Namelist_Stack(STACK_OF_WN *namelists, const char *name)
{
   // The second kid (kid 1) of a NAMELIST OPR_IO denotes the name of 
   // the namelist by means of a NAMELIST_DIRECTED io item, which again
   // denotes an LDA.
   //
   BOOL found = FALSE;

   for (INT i = 0; !found && (i < namelists->Elements()); i++)
   {
      found = 
	 !strcmp(name, 
		 ST_name(WN_st(WN_kid0(WN_kid1(namelists->Bottom_nth(i))))));
   }
   return found;
} // Is_In_Namelist_Stack



// Add any IOL item referring to a reshaped array element or
// segment to the set of "refs".
static void
Get_IOL_Reshaped_Array_Ref(STACK_OF_REFERENCES *refs, WN *wn, BOOL in_implied_do,
			    BOOL in_load, BOOL is_read)
{
  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_IO_ITEM) &&
      (WN_io_item(wn) == IOL_IMPLIED_DO)) {
    in_implied_do = TRUE;
    for (INT i=0; i<WN_kid_count(wn); i++) {
      Get_IOL_Reshaped_Array_Ref(refs,WN_kid(wn,i),in_implied_do, in_load, is_read);
    }
  } else if ((oper == OPR_LDA) || (oper == OPR_LDID)) {
    if (ST_is_reshaped_var(WN_st(wn))) {
      BOOL ref_is_loaded = in_load || !is_read;
      REF_TYPE ref_type;
      WN *parent = LWN_Get_Parent(wn);
      if (WN_operator(parent) == OPR_IO_ITEM) {
	ref_type = TOTAL_REF;
      } else if ((WN_operator(parent) == OPR_ARRAY) &&
	  (wn == WN_array_base(parent))) {
        WN *grand_parent = LWN_Get_Parent(parent);
	OPCODE grand_op = WN_opcode(grand_parent);
	if (OPCODE_is_load(grand_op) ||
	    ((OPCODE_operator(grand_op) == OPR_IO_ITEM) &&
	     (WN_io_item(grand_parent) == IOL_VAR))) {
	  if (in_implied_do) {
	    ref_type = IMPLIED_DO_REF;
	  } else {
	    ref_type = SINGLE_ELEMENT_REF;
	  }
	} else {
	  ref_type = OTHER_REF;
        }
      } else {
	ref_type = OTHER_REF;
      }
      refs->Push(REFERENCE(wn,ref_type,ref_is_loaded));
    }
  } else if (OPCODE_is_load(opcode)) {
    in_load = TRUE;
    for (INT i=0; i<WN_kid_count(wn); i++) {
      Get_IOL_Reshaped_Array_Ref(refs,WN_kid(wn,i),in_implied_do, in_load, 
							is_read);
    }
  } else {
    if (opcode == OPC_BLOCK) {
      WN *kid = WN_first (wn);
      while (kid) {
        Get_IOL_Reshaped_Array_Ref(refs,kid,in_implied_do, in_load, is_read);
        kid = WN_next(kid);
      }
    } else {
      for (INT i=0; i<WN_kid_count(wn); i++) {
        Get_IOL_Reshaped_Array_Ref(refs,WN_kid(wn,i),in_implied_do, in_load, is_read);
      }
    }
  }
}


// Process an I/O statement
static void
Get_IOS_Reshaped_Array_Refs(WN          *io_stmt,   // in
			    STACK_OF_WN *namelists, // in/out
			    STACK_OF_REFERENCES *refs)      // out
{
   // Given an IO statement and a stack of namelists active at this
   // point, search for any IO of reshaped arrays and push such array
   // references (LDIDs or LDAs) onto the stack of "refs".  If a new
   // namelist is defined, only push it onto the stack of "namelists"
   // if it referes to one or more reshaped arrays. A namelist specifier
   // must precede its use! 
   //
   INT  i;
   BOOL done;

   Is_True(WN_opc_operator(io_stmt) == OPR_IO, 
	   ("Unexpected OPERATOR in Get_IOS_Reshaped_Array_Refs()"));

   switch (WN_io_statement(io_stmt))
   {
   case IOS_NAMELIST:
   case IOS_CR_FWN:
   case IOS_CR_FRN:
      // Push the namelist onto the stack if it referes to any reshaped
      // array.
      //
      for (done = FALSE, i = 2; !done && (i < WN_kid_count(io_stmt)); i++)
      {
	 WN *ref = WN_kid0(WN_kid(io_stmt, i)); // The kid of an IOL node

	 Is_True(WN_opc_operator(ref) == OPR_LDA ||
		 WN_opc_operator(ref) == OPR_LDID, 
		 ("Unexpected OPERATOR in IOS_NAMELIST"));

	 if (ST_is_reshaped_var(WN_st(ref)))
	 {
	     namelists->Push(io_stmt);
	     done = TRUE;
	  }
      }
      break;

   case IOS_DECODE:
   case IOS_ACCEPT:
   case IOS_READ:
   case IOS_CR_FRF:
   case IOS_CR_FRU:
   case IOS_CR_FWF:
   case IOS_CR_FWU:
   case IOS_ENCODE:
   case IOS_PRINT:
   case IOS_REWRITE:
   case IOS_TYPE:
   case IOS_WRITE:
      // A read or a write IO statement.  For every IOL node, determine
      // whether or not it refers to a reshaped array element or segment, 
      // and, if it does, enter it into the set of "refs".  If an
      // IOF_NAMELIST_DIRECTED node refers to a reshaped array, then
      // emit an error message and empty the ref stack.
      //
      for (done = FALSE, i = 0; !done && (i < WN_kid_count(io_stmt)); i++)
      {
	 WN *item = WN_kid(io_stmt, i);

	 if (WN_io_item(item) == IOF_NAMELIST_DIRECTED &&
	     Is_In_Namelist_Stack(namelists, ST_name(WN_st(WN_kid0(item)))))
	 {
	    ErrMsgSrcpos(EC_LNO_Generic_Error, WN_Get_Linenum(io_stmt),
	  	"Cannot do IO with reshaped array in namelist\n");
	    refs->Clear();
	    done = TRUE;
	 }
	 else if (Is_IO_List_Item(item))
	 {
	   if ((WN_io_statement(io_stmt) == IOS_READ) ||
               (WN_io_statement(io_stmt) == IOS_CR_FRF) ||
               (WN_io_statement(io_stmt) == IOS_CR_FRU) ||
	       (WN_io_statement(io_stmt) == IOS_ACCEPT) ||
	       (WN_io_statement(io_stmt) == IOS_DECODE)) {
	    Get_IOL_Reshaped_Array_Ref(refs, item, FALSE/*in_implied_do*/,FALSE,TRUE);
           } else {
	    Get_IOL_Reshaped_Array_Ref(refs, item, FALSE,FALSE,FALSE);
           }
	 }
      }
      break;

   default:
      // We do not care about other IO statements.
      //
      break;
   } // switch on the IOS kind

} // Get_IOS_Reshaped_Array_Refs




