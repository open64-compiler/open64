/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#include <stdint.h>
#include <alloca.h>

#include "defs.h"
#include "opcode.h"
#include "symtab.h"
#include "const.h"			// for Make_Const
#include "cxx_template.h"		// needed by ipa_cprop.h
#include "wn_tree_util.h"		// tree iterator
#include "lwn_util.h"			// for LWN_CreateIstore, etc.
#include "wn_simp.h"                    // WN_Simplify_Tree

#include "ipo_defs.h"			
#include "ipa_df.h"			// needed by ipa_cprop.h
#include "ipaa.h"			// for mod/ref info
#include "ipa_cprop.h"			// for cprop annot
#include "ipa_option.h"			// option variables
#include "ipo_parent.h"			// for WN_Set_Parent
#include "ipo_const.h"
#include "ipc_symtab_merge.h"           // IPC_GLOBAL_IDX_MAP
#include "ipa_section_annot.h"		// for STATE_ARRAY


typedef HASH_TABLE<ST*,WN*> ST_TO_WN_MAP;
typedef HASH_TABLE<TY_IDX,TY_IDX> TY_TO_TY_MAP;


// -----------------------------------------------------
// Check if ST that comes from an array bound/stride has
// STID in the preamble whose RHS is an integer constant
// -----------------------------------------------------
static BOOL
ST_has_const_value (ST* st,
                    WN* stid,
                    ST_TO_WN_MAP* st_to_intconst_map)
{
  Is_True (WN_operator(stid) == OPR_STID && 
           WN_st(stid) == st &&
           WN_offset(stid) == 0,
           ("ST_has_const_value: mismatched ST_IDX in STID node"));
  
  WN* rhs = WN_kid0(stid);
  if (WN_operator(rhs) == OPR_INTCONST) {
    st_to_intconst_map->Enter_If_Unique(st, rhs);
    return TRUE;
  }
  
  return FALSE;
}


// --------------------------------------------------------
// Check if the array type has symbolic bounds/strides that
// have become constant after IPA constant propagation
// --------------------------------------------------------
static BOOL
TY_AR_has_new_const (TY_IDX ty_idx, 
                     const ST_TO_WN_MAP* st_to_stid_map, 
                     ST_TO_WN_MAP* st_to_intconst_map)
{
  Is_True (TY_kind(ty_idx) == KIND_ARRAY, 
           ("TY_AR_has_new_const: non-array type passed"));
  
  BOOL found_new_constants = FALSE;

  INT32 num_dims = TY_AR_ndims(ty_idx);

  for (INT32 i = 0; i < num_dims; ++i) {
    if (!TY_AR_const_lbnd(ty_idx, i) && TY_AR_lbnd_var(ty_idx, i)) {
      ST* lbnd_st = ST_ptr(TY_AR_lbnd_var(ty_idx, i));
      WN* lbnd_stid = st_to_stid_map->Find(lbnd_st);
      if (lbnd_stid && 
          ST_has_const_value(lbnd_st, lbnd_stid, st_to_intconst_map)) {
        found_new_constants = TRUE;
      }
    }
    if (!TY_AR_const_ubnd(ty_idx, i) && TY_AR_ubnd_var(ty_idx, i)) {
      ST* ubnd_st = ST_ptr(TY_AR_ubnd_var(ty_idx, i));
      WN* ubnd_stid = st_to_stid_map->Find(ubnd_st);
      if (ubnd_stid && 
          ST_has_const_value(ubnd_st, ubnd_stid, st_to_intconst_map)) {
        found_new_constants = TRUE;
      }
    }
    if (!TY_AR_const_stride(ty_idx, i) && TY_AR_stride_var(ty_idx, i)) {
      ST* stride_st = ST_ptr(TY_AR_stride_var(ty_idx, i));
      WN* stride_stid = st_to_stid_map->Find(stride_st);
      if (stride_stid && 
          ST_has_const_value(stride_st, stride_stid, st_to_intconst_map)){
        found_new_constants = TRUE;
      }
    }
  }
  
  return found_new_constants;
}
  

// ------------------------------
// Create a copy of an array type
// ------------------------------
static TY_IDX
Copy_array_type (TY_IDX ty_idx)
{
  const TY& old_ty = Ty_Table[ty_idx];
  Is_True (TY_kind(old_ty) == KIND_ARRAY, ("Copy_array_type: non-array TY"));

  // create and initialize new array type
  TY_IDX new_ty_idx;
  TY& new_ty = New_TY(new_ty_idx);
  TY_Init(new_ty, TY_size(old_ty), KIND_ARRAY, MTYPE_UNKNOWN, 0);
  Set_TY_align(new_ty_idx, TY_align(ty_idx));
  Set_TY_etype(new_ty, TY_etype(old_ty));

  INT32 num_dims = TY_AR_ndims(old_ty);
  Is_True (num_dims > 0, ("Copy_array_type: no array dimensions"));

  // create ARB for the first dimension and point TY_arb to it
  ARB_HANDLE new_arb = New_ARB();
  Set_TY_arb(new_ty, new_arb);

  // copy ARBs from the old type to the new type
  ARB_HANDLE old_arb = TY_arb(old_ty);
  ARB_copy(new_arb, old_arb);
  for (INT32 i = 1; i < num_dims; ++i) {
    ARB_copy(New_ARB(), old_arb[i]);
  }
  
  return new_ty_idx;
}
  

// ------------------------------------------
// Replace each bound/stride ST found in the 
// st_to_intconst_map with the constant value
// ------------------------------------------
static void
TY_AR_propagate_constants (TY_IDX ty_idx, 
                           const ST_TO_WN_MAP* st_to_intconst_map)
{
  Is_True (TY_kind(ty_idx) == KIND_ARRAY,
           ("TY_AR_propagate_constants: non-array type passed"));
  
  INT32 num_dims = TY_AR_ndims(ty_idx);

  for (INT32 i = 0; i < num_dims; ++i) {
    if (!TY_AR_const_lbnd(ty_idx, i) && TY_AR_lbnd_var(ty_idx, i)) {
      ST* lbnd_st = ST_ptr(TY_AR_lbnd_var(ty_idx, i));
      WN* lbnd_intconst = st_to_intconst_map->Find(lbnd_st);
      if (lbnd_intconst) {
        Is_True (WN_operator(lbnd_intconst) == OPR_INTCONST,
                 ("TY_AR_propagate_constants: expected INTCONST node"));
        Set_TY_AR_const_lbnd(ty_idx, i);
        Set_TY_AR_lbnd_val(ty_idx, i, WN_const_val(lbnd_intconst));
      }
    }
    if (!TY_AR_const_ubnd(ty_idx, i) && TY_AR_ubnd_var(ty_idx, i)) {
      ST* ubnd_st = ST_ptr(TY_AR_ubnd_var(ty_idx, i));
      WN* ubnd_intconst = st_to_intconst_map->Find(ubnd_st);
      if (ubnd_intconst) {
        Is_True (WN_operator(ubnd_intconst) == OPR_INTCONST,
                 ("TY_AR_propagate_constants: expected INTCONST node"));
        Set_TY_AR_const_ubnd(ty_idx, i);
        Set_TY_AR_ubnd_val(ty_idx, i, WN_const_val(ubnd_intconst));
      }
    }
    if (!TY_AR_const_stride(ty_idx, i) && TY_AR_stride_var(ty_idx, i)) {
      ST* stride_st = ST_ptr(TY_AR_stride_var(ty_idx, i));
      WN* stride_intconst = st_to_intconst_map->Find(stride_st);
      if (stride_intconst) {
        Is_True (WN_operator(stride_intconst) == OPR_INTCONST,
                 ("TY_AR_propagate_constants: expected INTCONST node"));
        Set_TY_AR_const_stride(ty_idx, i);
        Set_TY_AR_stride_val(ty_idx, i, WN_const_val(stride_intconst));
      }
    }
  }
}
  

// -------------------------------------------------------------
// For each ST of array type in the local symtab, check if some
// previously symbolic bounds can become constant. If so, create
// a new type for that ST, and enter it into hash table so that
// all references to the old type can be replaced by the new one
// -------------------------------------------------------------
struct fix_array_bounds
{
  const ST_TO_WN_MAP* st_to_stid_map;
  TY_TO_TY_MAP* old_to_new_ty_map;
  
  fix_array_bounds (const ST_TO_WN_MAP* st_map, TY_TO_TY_MAP* ty_map) 
    : st_to_stid_map (st_map),
      old_to_new_ty_map (ty_map)
  {}
  
  void operator() (UINT32, ST* st) const 
  {
    TY_IDX ptr_ty_idx = TY_IDX_ZERO;
    TY_IDX ty_idx = ST_type(st);

    if (TY_kind(ty_idx) == KIND_POINTER) {
      ptr_ty_idx = ty_idx;
      ty_idx = TY_pointed(ptr_ty_idx);
    }
    
    if (TY_kind(ty_idx) == KIND_ARRAY) {

      // check if the new type has already been created
      TY_IDX new_ty_idx = old_to_new_ty_map->Find(ty_idx);
      if (new_ty_idx) {
        if (ptr_ty_idx == TY_IDX_ZERO) {
          Set_ST_type(st, new_ty_idx);
        }
        else {
          // check if the new pointer type has already been created
          TY_IDX new_ptr_ty_idx = old_to_new_ty_map->Find(ptr_ty_idx);
          if (!new_ptr_ty_idx) {
            new_ptr_ty_idx = TY_is_f90_pointer(ptr_ty_idx) ?
              Make_F90_Pointer_Type(new_ty_idx) : 
              Make_Pointer_Type(new_ty_idx);
            old_to_new_ty_map->Enter(ptr_ty_idx, new_ptr_ty_idx);
          }
          Set_ST_type(st, new_ptr_ty_idx);
        }
      }
      
      else {
        
        ST_TO_WN_MAP st_to_intconst_map(16, Malloc_Mem_Pool);

        if (TY_AR_has_new_const(ty_idx, st_to_stid_map, &st_to_intconst_map)) {
        
          TY_IDX new_ty_idx = Copy_array_type(ty_idx);
          TY_AR_propagate_constants(new_ty_idx, &st_to_intconst_map);
          old_to_new_ty_map->Enter(ty_idx, new_ty_idx);

          if (ptr_ty_idx == TY_IDX_ZERO) {
            Set_ST_type(st, new_ty_idx);
          }
          else {
            TY_IDX new_ptr_ty_idx = TY_is_f90_pointer(ptr_ty_idx) ?
              Make_F90_Pointer_Type(new_ty_idx) : 
              Make_Pointer_Type(new_ty_idx);
            Set_ST_type(st, new_ptr_ty_idx);
            old_to_new_ty_map->Enter(ptr_ty_idx, new_ptr_ty_idx);
          }
        }
      }
    }
  }
};


// -------------------------------------------------------------
// Using the hash-based map, replace all references to old types
// to use the new types that reflect IPA propagated constants
// -------------------------------------------------------------
static void
Update_wn_types (WN* pu, const TY_TO_TY_MAP* old_to_new_ty_map)
{
  Is_True (WN_operator(pu) == OPR_FUNC_ENTRY, 
           ("Update_wn_types: expected FUNC_ENTRY node"));

  for (WN_ITER* wni = WN_WALK_TreeIter(pu); wni; wni = WN_WALK_TreeNext(wni)) {

    WN* wn = WN_ITER_wn(wni);
    OPERATOR opr = WN_operator(wn);

    if (OPERATOR_has_1ty(opr)) {
      TY_IDX new_ty = old_to_new_ty_map->Find(WN_ty(wn));
      if (new_ty) {
        WN_set_ty(wn, new_ty);
      }
    }

    else if (OPERATOR_has_2ty(opr)) {
      TY_IDX new_ty = old_to_new_ty_map->Find(WN_ty(wn));
      if (new_ty) {
        WN_set_ty(wn, new_ty);
      }
      new_ty = old_to_new_ty_map->Find(WN_load_addr_ty(wn));
      if (new_ty) {
        WN_set_load_addr_ty(wn, new_ty);
      }
    }
  }
}


// -----------------------------------------------------------------------
// After constant formals have been substituted in array bound expressions
// simplify STIDs to symbolic bound variables to try to make them constant
// -----------------------------------------------------------------------
static void
Update_array_bounds (WN* pu)
{
  Is_True (WN_operator(pu) == OPR_FUNC_ENTRY, 
           ("Update_array_bounds: expected FUNC_ENTRY node"));
  
  ST_TO_WN_MAP st_to_stid_map(16, Malloc_Mem_Pool);

  for (WN* wn = WN_entry_first(pu); wn; wn = WN_next(wn)) {
    // stop when the end of preamble is reached
    if (WN_operator(wn) == OPR_PRAGMA && 
        WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END) {
      break;
    }
    // replace all LDIDs of the formal by the constant
    // simplify only STIDs with offset 0 (array bounds are in that group)
    else if (WN_operator(wn) == OPR_STID && WN_offset(wn) == 0) {
      WN* new_stid = WN_Simplify_Tree(wn);
      WN* rhs = WN_kid0(new_stid);
      if (WN_operator(rhs) == OPR_INTCONST) {
        ST* st = WN_st(wn);
        st_to_stid_map.Enter(st, new_stid);
          
        TREE_ITER iter(pu);
        while (WN* node = iter.Wn()) {
          OPERATOR opr = WN_operator(node);
          // stop when the end of preamble is reached
          if (opr == OPR_PRAGMA && WN_pragma(node) == WN_PRAGMA_PREAMBLE_END){
            break;
          }
          // replace all LDIDs of the formal by the constant
          else if (opr == OPR_LDID && WN_st(node) == st
#ifdef KEY
          // Bug 14197: Above, the STID was to the ST with offset 0, so
          // we can only replace offset 0 LDIDs. Also note, there is really
          // no check to limit this replacement to only formals.
                   && WN_offset(node) == 0
#endif
		   ) {
            iter.Replace(WN_COPY_Tree(rhs));
          }
          ++iter;
        }
        Is_True (iter.Wn(), ("Update_array_bounds: no PREAMBLE_END"));
      }
    }
  }

  TY_TO_TY_MAP old_to_new_ty_map(16, Malloc_Mem_Pool);
  
  For_all (St_Table, 
           CURRENT_SYMTAB, 
           fix_array_bounds(&st_to_stid_map, &old_to_new_ty_map));

  Update_wn_types(pu, &old_to_new_ty_map);
}


// ----------------------------------------------------------------------
// Create an INTCONST node that corresponds to the constant summary value
// passed either by value or by reference. Return NULL if not integer.
// ----------------------------------------------------------------------
static WN* 
Summary_value_to_intconst (const SUMMARY_VALUE& value, TYPE_ID mtype,
			   BOOL is_ref_param)
{
    if (is_ref_param) {
	if (!value.Is_addr_of ())
	    return NULL;
    } else if (value.Is_addr_of ())
	return NULL;
    
    if (value.Is_int_const()) {
	return WN_Intconst(mtype, value.Get_int_const_value());
    }
    if (value.Is_const_st()) {
	TCON& tcon = Tcon_Table[value.Get_tcon_idx()];
	INT64 intval;
	if (Targ_Is_Integral(tcon, &intval)) {
	    return WN_Intconst(mtype, intval);
	}
    }
    return NULL;
}
  

// -------------------------------------------
// Propagate formal constant into array bounds
// -------------------------------------------
static BOOL
IPA_constant_in_array_bounds (const SUMMARY_VALUE& value,
                              WN* pu,
                              ST* formal_st)
{
  TYPE_ID mtype = TY_mtype(ST_type(formal_st));
  // only propagate integer constants
  if (!MTYPE_is_integral(mtype)) {
    return FALSE;
  }
    
  WN* const_wn =
      Summary_value_to_intconst(value, Mtype_comparison(mtype),
				ST_sclass(formal_st) == SCLASS_FORMAL_REF);
  if (!const_wn) {
    return FALSE;
  }
  
  Is_True (WN_operator(pu) == OPR_FUNC_ENTRY, 
           ("IPA_constant_in_array_bounds: expected FUNC_ENTRY node"));

  BOOL found_formal_ldid = FALSE;

  TREE_ITER iter(pu);
  while (WN* wn = iter.Wn()) {
    OPERATOR opr = WN_operator(wn);
    // stop when the end of preamble is reached
    if (opr == OPR_PRAGMA && WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END) {
      break;
    }
    // replace all LDIDs of the formal by the constant
    else if (opr == OPR_LDID && WN_st(wn) == formal_st) {
#ifdef KEY
      TYPE_ID rtype = WN_rtype(wn);
#endif
      iter.Replace(WN_COPY_Tree(const_wn));
#ifdef KEY
      // bug 8666: The usage type of the constant may be different than
      // the inherent type, e.g. I4 constant used as I8. Use the proper type.
      WN_set_rtype (iter.Wn(), rtype);
#endif
      found_formal_ldid = TRUE;
    }
    ++iter;
  }

  Is_True (iter.Wn(), 
           ("IPA_constant_in_array_bounds: can't find PREAMBLE_END"));
  
  return found_formal_ldid;
}


#ifdef Is_True_On
// create assert statement to verify if "formal" has value equals to
// "const_wn" 
static inline WN*
Create_Assert (ST* formal, WN* const_wn, TYPE_ID desc)
{
    WN* load = WN_Ldid (desc, 0, formal, ST_type (formal));
    return WN_CreateAssert (0, WN_EQ (Mtype_comparison (desc), load,
				      WN_COPY_Tree (const_wn))); 
	
} // Create_Assert
#endif // Is_True_On


// Given a simple (no LDA) SUMMARY_VALUE, return a WHIRL node representing 
// that constant value
static WN*
Gen_WN_Const (const SUMMARY_VALUE& value)
{
    Is_True (! value.Is_addr_of (), ("Invalid value for simple constant"));

    switch (value.Get_const_type ()) {
    case VALUE_INT_CONST:
	return WN_Intconst (Mtype_comparison (value.Get_mtype ()), 
			    value.Get_int_const_value()); 

    case VALUE_CONST:
	Is_True (! MTYPE_is_integral (value.Get_mtype ()),
		 ("Integral value should not be a const ST"));
	const ST* st = NULL;
        if (value.Get_const_st_idx ())
            st = &St_Table[value.Get_const_st_idx ()];
	const TCON& tc = Tcon_Table[value.Get_tcon_idx ()];
	if (st == NULL || ST_class (st) != CLASS_CONST) {
	    Is_True (st == NULL || ST_is_const_var (st),
		     ("Non constant found during constant propagation"));
	    st = New_Const_Sym (value.Get_tcon_idx (),
				MTYPE_To_TY (TCON_ty (tc)));
	}
	OPCODE op = OPCODE_make_op (OPR_CONST, TCON_ty (tc),
				    MTYPE_V);
	return WN_CreateConst (op, ST_st_idx (st));
    }
    
    Fail_FmtAssertion ("Unsupported constant type");
    // should never reach here
    return NULL;
} // Gen_WN_Const


// create a global ST from the given SUMMARY_VALUE, return NULL if symbol
// is not global
static ST*
#ifdef KEY
Create_Global_ST (const SUMMARY_VALUE& value, BOOL * incompatible = NULL)
#else
Create_Global_ST (const SUMMARY_VALUE& value)
#endif
{
    Is_True (value.Is_addr_of (), ("Expecting a memory address"));

    TYPE_ID mtype = value.Is_addr_of () ? value.Target_mtype () :
	value.Get_mtype ();

    switch (value.Get_const_type ()) {

    case VALUE_INT_CONST:
	if (!value.Is_convertible_to_global ())
	    return NULL;
	else {
	    TYPE_ID mtype = value.Is_addr_of () ? value.Target_mtype () :
		value.Get_mtype (); 
#ifdef KEY
	    if (!MTYPE_is_integral (mtype) && incompatible) {
	      *incompatible = TRUE;
	      return NULL;
	    }
#endif
	    return New_Const_Sym (Enter_tcon (Host_To_Targ (mtype,
							    value.Get_int_const_value ())),
				  MTYPE_To_TY (mtype));
	}
	
    case VALUE_CONST:
	if (!value.Is_convertible_to_global ())
	    return NULL;
	else {
	    ST* st = NULL;
            if (value.Get_const_st_idx ())
	        st = &St_Table[value.Get_const_st_idx ()];
	    if (st == NULL || ST_class (st) != CLASS_CONST) {
		Is_True (st == NULL || ST_is_const_var (st),
			 ("Non constant found during constant propagation"));
		const TCON& tc = Tcon_Table[value.Get_tcon_idx ()];
		st = New_Const_Sym (value.Get_tcon_idx (),
				    MTYPE_To_TY (TCON_ty (tc)));
	    }
	    return st;
	}
	
    case VALUE_GLOBAL:
	return &St_Table[value.Get_global_st_idx ()];
	
    default:
	return NULL;
    }
} // Create_Global_ST


// Given a SUMMARY_VALUE that is an address of a variable, generate a LDA node
static inline WN*
Gen_LDA (const SUMMARY_VALUE& value, const ST* formal)
{
    ST* st = Create_Global_ST (value);

    if (st == NULL)
	return NULL;
    
    Is_True (ST_level (st) == GLOBAL_SYMTAB, ("Invalid ST level"));

    OPCODE op = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
    return WN_CreateLda (op, 0, ST_type (formal), ST_st_idx (st));
} // Gen_LDA


// generate a statement assigning the constant value to the formal
// parameter.  We let the backend propagate the constant value to the rest
// of the PU
static void
Generate_Assignment_Stmt (ST* formal, WN* block, WN* const_wn)
{
    TYPE_ID desc = TY_mtype (ST_type (formal));
    const_wn = WN_Type_Conversion (const_wn, desc);
    WN* stmt = WN_Stid (desc, 0, formal, ST_type (formal), const_wn);
    LWN_Set_Parent (const_wn, stmt);
    WN_INSERT_BlockBefore(block, NULL, stmt);
	    
#ifdef Is_True_On
    if (IPA_Enable_Assert && !IPA_Enable_Cprop2) {
	WN_INSERT_BlockBefore (block, stmt,
			       Create_Assert (formal, const_wn, desc));
    }
#endif // Is_True_On

} // Generate_Assignment_Stmt


namespace {
    // replace the formal parameter by the specified symbol.
    struct simple_replace {
	const ST_IDX formal;
	const ST_IDX actual;

	simple_replace (ST_IDX f, ST_IDX a) : formal (f), actual (a) {}

	void operator() (TREE_ITER& iter) const {
	    WN* wn = iter.Wn ();
	    if (OPERATOR_has_sym (WN_operator (wn)) &&
		WN_st_idx (wn) == formal) {
		WN_st_idx (wn) = actual;
	    }
	}
    };

    struct replace_by_int_const {
	const ST_IDX formal;
	const ST_IDX actual;
	const TYPE_ID mtype;
	const INT64 value;
	BOOL modified;

	replace_by_int_const (ST_IDX f, ST_IDX a, TYPE_ID m, INT64 v) :
	    formal (f), actual (a), mtype (m), value (v), modified (FALSE) {}

	void operator() (TREE_ITER& iter) {
	    WN* wn = iter.Wn ();
	    OPERATOR opr = WN_operator (wn);
	    if (OPERATOR_has_sym (opr) && WN_st_idx (wn) == formal) {
		if (opr == OPR_LDID) {
		    iter.Replace (WN_Intconst (Mtype_comparison (mtype),
					       value)); 
		    modified = TRUE;
		} else
		    WN_st_idx (wn) == actual;
	    }
	}
    };
	
    template <class OPERATION>
    inline void
    for_all_wn (WN* wn, OPERATION& op) {
	TREE_ITER iter (wn);
	while (iter.Wn () != NULL) {
	    op (iter);
	    ++iter;
	}
    }
}


static inline void
Replace_Formal_By_Actual (WN* func_body, ST_IDX formal, const ST* actual)
{
    // can't take func_entry because we don't want to replace the IDNAME
    // node for parameter declaration
    Is_True (WN_operator (func_body) != OPR_FUNC_ENTRY,
	     ("expecting function body"));

    const TY& ty = Ty_Table[ST_type (actual)];
    simple_replace simp_op (formal, ST_st_idx (actual));

    if (ST_class (actual) == CLASS_CONST &&
	MTYPE_is_integral (TY_mtype (ty))) {
	// the symbol is a constant integer, try to change it to a INTCONST 
	// node
	TCON& tcon = Tcon_Table[ST_tcon (actual)];
	TYPE_ID mtype = TY_mtype (ty);
	Is_True (mtype == TCON_ty (tcon),
		 ("Inconsistent mtype between TCON and TY"));
	INT64 int_value = Targ_To_Host (tcon);

	replace_by_int_const fix_expr (formal, ST_st_idx (actual), mtype,
				       int_value);
	TREE_ITER iter (func_body);
	while (iter.Wn () != NULL) {
	    WN* wn = iter.Wn ();
	    OPERATOR opr = WN_operator (wn);
	    if (opr == OPR_IO_ITEM) {
		for_all_wn (wn, simp_op);
		iter.WN_TREE_next_skip ();
		continue;
	    } else if (OPERATOR_is_expression (opr)) {
		fix_expr.modified = FALSE;
		if (OPERATOR_is_leaf (opr))
		    fix_expr (iter);
		else
		    for_all_wn (wn, fix_expr);
		if (fix_expr.modified)
		    iter.Replace (WN_Simplify_Tree (iter.Wn ()));
		iter.WN_TREE_next_skip ();
		continue;
	    } else if (OPERATOR_has_sym (opr) && WN_st_idx (wn) == formal) {
		WN_st_idx (wn) = ST_st_idx (actual);
	    }
	    ++iter;
	}
    } else {

	for_all_wn (func_body, simp_op);
    }
} // Replace_Formal_By_Actual


// replace an icall by a direct call
static void
Replace_Icall (TREE_ITER& iter, const WN* icall, ST* actual)
{
    WN* call = WN_generic_call (OPR_CALL, WN_rtype (icall), WN_desc (icall), 
				WN_kid_count (icall) - 1, actual);
    for (INT i = 0; i < WN_kid_count (icall) - 1; ++i) {
	WN_kid (call, i) = WN_kid (icall, i);
    }
#ifdef KEY // bug 1050
    WN_call_flag(call) = WN_call_flag(icall);
#endif
    iter.Replace (call);
} // Replace_Icall


// replace the formal parameter by the address of the specified symbol
// simplify iload/lda and istore/lda if possible.
// return TRUE if *ALL* occurrences of the formal are successfully replaced
static BOOL
Replace_Formal_By_LDA (WN* func_body, ST_IDX formal, ST* actual)
{
    // can't take func_entry because we don't want to replace the IDNAME
    // node for parameter declaration
    Is_True (WN_operator (func_body) != OPR_FUNC_ENTRY,
	     ("expecting function body"));

    TREE_ITER iter (func_body);
    OPCODE lda = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
    UINT fail_count = 0;		// number of given up replacements
    
    while (iter.Wn () != NULL) {
	WN* wn = iter.Wn ();
	OPERATOR opr = WN_operator (wn);

	switch (opr) {
	case OPR_LDID:
	    if (WN_st_idx (wn) == formal && WN_offset (wn) == 0)
		iter.Replace (WN_CreateLda (lda, 0, WN_ty (wn), actual));
	    else
		++fail_count;
	    break;

	case OPR_ILOAD:
	    if (WN_operator (WN_kid0 (wn)) == OPR_LDID &&
		WN_st_idx (WN_kid0 (wn)) == formal) {
		if (WN_field_id (wn)) {
#ifdef KEY
	            // Bug 942
	            // Should pass down appropriate rtypes for bit-fields.
		    // replace indirect load by direct load
		    iter.Replace (WN_CreateLdid (OPR_LDID, WN_rtype(wn), 
		    				 WN_desc (wn), WN_offset (wn),
						 actual, WN_ty (wn),
						 WN_field_id (wn)));
#else
		    // replace indirect load by direct load
		    iter.Replace (WN_Ldid (WN_desc (wn), WN_offset (wn),
					   actual, WN_ty (wn),
					   WN_field_id (wn)));
#endif
		} else if (WN_offset (WN_kid0 (wn)) == 0) {
		    // replace indirect load by direct load
		    iter.Replace (WN_Ldid (WN_desc (wn), WN_offset (wn),
					   actual, WN_ty (wn)));
		} else
		    ++fail_count;
	    } 
	    break;
	    
	case OPR_ISTORE:
	    if (WN_operator (WN_kid1 (wn)) == OPR_LDID &&
		WN_st_idx (WN_kid1 (wn)) == formal) {
		if (WN_field_id (wn)) {
		    // replace indirect store by direct store
		    iter.Replace (WN_Stid (WN_desc (wn), WN_offset (wn),
					   actual, TY_pointed (WN_ty (wn)),
					   WN_kid0(wn), WN_field_id (wn)));
		} else if (WN_offset (WN_kid1 (wn)) == 0) {
		    // replace indirect store by direct store
		    iter.Replace (WN_Stid (WN_desc (wn), WN_offset (wn),
					   actual, TY_pointed (WN_ty (wn)),
					   WN_kid0(wn)));
		} else
		    ++fail_count;
	    }
	    break;
		
	case OPR_ICALL:
	    {
		const WN* func_addr = WN_kid (wn, WN_kid_count (wn) - 1);
		if (WN_operator (func_addr) == OPR_LDID &&
		    WN_st_idx (func_addr) == formal) {
		    if (WN_offset (func_addr) == 0) {
			Replace_Icall (iter, wn, actual);
		    } else
			++fail_count;
		}
		break;
	    }
	    
	default:
	    if (OPERATOR_has_sym (opr) && WN_st_idx (wn) == formal) {
		Is_True (opr != OPR_STID, ("formal parameter is not readonly"));
		++fail_count;
	    }
	    break;
	}
	++iter;
    }

    return fail_count == 0;
} // Replace_Formal_By_LDA


namespace
{
    struct replace_lda {
	const ST_IDX formal;
	const WN* actual;

	replace_lda (ST_IDX f, const WN* a) : formal(f), actual (a) {}

	void operator() (TREE_ITER& iter) const {
	    WN* wn = iter.Wn ();
	    if (WN_operator (wn) == OPR_LDA && WN_st_idx (wn) == formal &&
		WN_offset (wn) == 0)
		iter.Replace (WN_CopyNode (actual));
	}
    };
}

static void
Replace_Addr_Of_Formal_By_Actual (WN* func_body, ST_IDX formal,
				  const WN* actual)
{
    // can't take func_entry because we don't want to replace the IDNAME
    // node for parameter declaration
    Is_True (WN_operator (func_body) != OPR_FUNC_ENTRY,
	     ("expecting function body"));

    replace_lda op (formal, actual);

    for_all_wn (func_body, op); 
	    
} // Replace_Addr_Of_Formal_By_Actual


// -------------------------------------------------------------------
// When a formal is constant of type VALUE_GLOBAL, check if the global 
// is a common block element that has constant value coming into this 
// PU, and if so, generate constant assignment statement.
// -------------------------------------------------------------------
static void
Check_If_Global_Has_Const_Value (IPA_NODE* node, 
                                 WN* block,
                                 ST* global_st)
{
  // do this only for common block elements
  ST* base_st = ST_base(global_st);
  if (base_st != global_st && ST_sclass(base_st) == SCLASS_COMMON) {
    GLOBAL_ANNOT* gannot = node->Global_Annot();
    if (gannot) {
      const GLOBAL_VALUE* gval = 
        gannot->Find(GLOBAL_ANNOT::Index(ST_st_idx(base_st)),
                     ST_ofst(global_st),
                     (UINT32) TY_size(ST_type(global_st)));
      if (gval) {
        SUMMARY_VALUE* value = gval->Value();
        if (value) {
          Generate_Assignment_Stmt(global_st, block, Gen_WN_Const(*value));
        }
      }
    }
  }
}

#ifdef KEY
BOOL
Store_To_Formal ( ST* formal, WN* w )
{
  OPCODE opc = WN_opcode(w);

  if (opc == OPC_BLOCK) {
    for (WN* stmt = WN_first(w); stmt; stmt = WN_next(stmt)) {
      if (WN_operator(stmt) == OPR_STID &&
	  WN_st(stmt) == formal)
	return TRUE;
      for (UINT kidno = 0; kidno < WN_kid_count(stmt); kidno++) {
	if (Store_To_Formal (formal, WN_kid(stmt, kidno)))
	  return TRUE;
      }
    }
  } else {
    for (UINT kidno = 0; kidno < WN_kid_count(w); kidno++) {
      if (Store_To_Formal (formal, WN_kid(w, kidno)))
	return TRUE;
    }
  }
  return FALSE;
}
#endif
//-------------------------------------------------------------------
// propagate the constant value to the formal parameter by either
// generating an assignment statement or a global replacement
//-------------------------------------------------------------------
static void
Propagate_Constants (IPA_NODE* node, WN* w, VALUE_DYN_ARRAY* cprop_annot)
{
    Is_True (w != NULL && cprop_annot != NULL &&
	     cprop_annot != (VALUE_DYN_ARRAY*) -1,
	     ("Invalid input to Propagate_Constants"));

    BOOL need_to_update_array_bounds = FALSE;

    WN *block = WN_CreateBlock();

    for (INT i = 0; i < WN_num_formals(w); ++i) {
	
	SUMMARY_VALUE& annot_node = (*cprop_annot)[i];

	if (!annot_node.Is_constant ())
	    continue;

	ST* formal = WN_st(WN_formal(w,i));
	ST* const_st = NULL;

#ifdef KEY
        // complex formal passed by value
        if (ST_sclass (formal) != SCLASS_FORMAL_REF &&
            MTYPE_is_complex(TY_mtype(ST_type(formal))))
            continue;
#endif
	// check if constant can be propagated into array bounds
	if (IPA_constant_in_array_bounds(annot_node, w, formal)) {
          need_to_update_array_bounds = TRUE;
        }

	if (ST_sclass (formal) != SCLASS_FORMAL_REF) {
	    // passed by value

	    WN *const_wn = NULL;
	    if (!annot_node.Is_addr_of ()) {
		Generate_Assignment_Stmt (formal, block,
					  Gen_WN_Const (annot_node));
		annot_node.Remove_param ();
	    } else {
		const IPAA_NODE_INFO* mod_ref_info = node->Mod_Ref_Info ();
		if (mod_ref_info && ! mod_ref_info->Is_formal_dmod_elmt (i)) {
		    // formal is readonly, so we can do global replacement
		    const_st = Create_Global_ST (annot_node);
		    if (const_st) {
			if (Replace_Formal_By_LDA (WN_func_body (w),
						   ST_st_idx (formal),
						   const_st)) {
			    annot_node.Remove_param ();
			}
		    } 
		} else if (annot_node.Is_global ()) {
		    // passing in the address of a global variable, we may
		    // propagate by generating an assignment statement
		    const_st = &St_Table[annot_node.Get_global_st_idx ()];

		    // if the address of the symbol is not saved, we'd
		    // rather not propagate.  Otherwise, we need to set 
		    // the addr_saved bit, which might have negative
		    // performance impact.
		    if (!ST_addr_saved (const_st))
			continue;
		    
		    WN* lda = Gen_LDA (annot_node, formal);
		    if (lda == NULL)
			continue;
		    Generate_Assignment_Stmt (formal, block, lda);
		    annot_node.Remove_param ();
		}
	    }
	} else {
#ifdef KEY
	// Source program can intentionally change the value of 
	// formal parameters. In C, actual parameters are not modified 
	// (unless passed by reference) and in Fortran, actual parameters are
	// modified. This module looks at incoming parameters to decide 
	// whether a reference to the parameter can be replaced by the constant
	// value, if the incoming parameter value is always a constant.
	// It does not check whether the parameter is modified inside the 
	// current_pu.
	// Skip the optimization if there is a store to the incoming parameter,
	// even though it may be a constant.
	// Bug exposed by -O3 -IPA NAS/CG
	if (Store_To_Formal (formal /* the formal parameter */, 
			     w      /* current_pu */ ))
	  continue;				  
#endif
    //here we will met the recursive case, where in the following case
    //   caller(){
    //       x       = 0, y =0;
    //       callee(x,y)//x, y both are by reference
    //           }
    //   callee(int p, int q){
    //             p = 0;
    //             q = 1
    //            }
    // since p and q are killed in callee, so it will cprop p,q in callee, but
    // will cprop x, y in caller, will changed into
    //   callee(0,0);
    // this will introduce the problem in callee, where 0 are in rodata section,
    // it try to modify the readonly section
    // the safe fix here is just disable such optimization if we find this
    // annot_node is passed by reference since we can't now if the callee have
    // been cproped

    //better fix is when the callee is done, if we can pass some information to
    //the caller, then we will get better result.
        
    // parameter passed by reference
//     if(annot_node.Is_addr_of ()){
//       continue;
//     }
    if (annot_node.Is_addr_of ()) {
		// "normal" case: we can peel off the LDA from the actual
		const IPAA_NODE_INFO* mod_ref_info = node->Mod_Ref_Info ();
		if (mod_ref_info &&
		    ! mod_ref_info->Is_formal_dmod_elmt (i) &&
		    ! mod_ref_info->Is_formal_imod_elmt (i))
		    annot_node.Set_convertible_to_global ();
#ifdef KEY
		// bug 9205: We may be passing integer constant for an
		// addr_of node, where it is an address of a non-integer.
		// Don't propagate the constant in this case.
		BOOL incompatible = FALSE;
		const_st = Create_Global_ST (annot_node, &incompatible);
		if (incompatible) continue;
#else
		const_st = Create_Global_ST (annot_node);
#endif
		if (const_st) {
                  if (annot_node.Is_global()) {
                    Check_If_Global_Has_Const_Value(node, block, const_st);
                  }
                  Replace_Formal_By_Actual (WN_func_body (w),
                                            ST_st_idx (formal), 
                                            const_st);
                  annot_node.Remove_param ();
		} else {
		    SUMMARY_VALUE value = annot_node;
		    value.Clear_is_addr_of ();
		    value.Set_mtype (annot_node.Target_mtype ());
		    Generate_Assignment_Stmt (formal, block,
					      Gen_WN_Const (value));
		}
	    } else {
		// a const value is passed to a reference parameter!!
		const WN* actual = Gen_WN_Const (annot_node);
		if (actual) {
		    Replace_Addr_Of_Formal_By_Actual (WN_func_body (w),
						      ST_st_idx (formal),
						      actual);
		}
	    }
	}
    }

    // if constant formals are used in array bounds, we need to update them
    if (need_to_update_array_bounds) {
      Update_array_bounds (w);
    }

    // if there are assignment statements then stick them into the tree
    if (WN_first(block)) {
	LWN_Insert_Block_Before(WN_func_body(w), WN_first(WN_func_body(w)),
				block);
    }

    LWN_Parentize(w);
    WN_verifier (w);

} // Propagate_Constants


struct fix_aliased_formals
{
    void operator() (UINT32, ST* st) const {
	if ((ST_sclass (st) == SCLASS_FORMAL ||
	     ST_sclass (st) == SCLASS_FORMAL_REF) &&
	    ST_base_idx (st) != ST_st_idx (st)) {
	    
	    const ST& base_st = St_Table[ST_base_idx (st)];
	    if (ST_sclass (base_st) == SCLASS_AUTO) {
		Set_ST_sclass (st, SCLASS_AUTO);
		Clear_ST_is_value_parm (st);
	    }
	}
    }
};

//-------------------------------------------------------------------
// a) Find the number of constants for each procedure
// b) create a new function entry node, adding kids only as needed
// c) for constants, then generate assignment statements.
// d) update the st entry to reflect a local auto variable
// e) fix up all the callsites to now contain fewer parameters
//-------------------------------------------------------------------
void
IPA_Propagate_Constants (IPA_NODE* n, BOOL delete_const_param)
{
    INT i;

    VALUE_DYN_ARRAY* cprop_annot = n->Cprop_Annot();

    if (cprop_annot == NULL || cprop_annot == (VALUE_DYN_ARRAY*) -1)
	return;

    IPA_NODE_CONTEXT context (n);		// switch the symtab context

    WN* w = n->Whirl_Tree();

    Propagate_Constants (n, w, cprop_annot);

    if (!delete_const_param) {
	for (i = 0; i < cprop_annot->Sizeof (); ++i)
	    (*cprop_annot)[i].Reset_remove_param ();
	return;
    }

    UINT param_count = WN_num_formals (w);
    for (i = 0; i < WN_num_formals (w); ++i)
	if ((*cprop_annot)[i].Is_remove_param ())
	    --param_count;

    if (param_count == WN_num_formals (w))
	return;

    // create the necessary opcode
    WN* func_node = WN_CreateEntry (param_count, WN_st_idx(w),
				    WN_func_body(w),
				    WN_func_pragmas(w),
				    WN_func_varrefs(w)); 

    WN_set_map_id(func_node, WN_map_id(w));
    WN_linenum(func_node) = WN_linenum(w);
    // copy over all the kids from the original node to this node
    // excluding nodes that are constant
    STATE_ARRAY* state_formal_array = NULL; 
    VALUE_DYN_ARRAY* const_formal_array = NULL; 
    if (IPA_Enable_Array_Sections) {
	IPA_NODE_SECTION_INFO* sec = n->Section_Annot();
	if (sec != NULL)
	    state_formal_array = (STATE_ARRAY*) sec->Get_formals();
    }
    INT k=0;
#ifdef KEY
    TYLIST_IDX tylist_idx;
    TYLIST_IDX from_idx = TY_tylist(PU_prototype(Pu_Table[ST_pu(WN_st(w))]));
    // function return type
    Set_TYLIST_type (New_TYLIST (tylist_idx), Tylist_Table[from_idx]);
    Set_TY_tylist (PU_prototype(Pu_Table[ST_pu(WN_st(w))]), tylist_idx);
    ++from_idx;
#endif

    for (i = 0; i < WN_num_formals(w); i++) {
	WN* id = WN_kid(w,i);
	if ((*cprop_annot)[i].Is_remove_param ()) {
	    // if this parameter needs to be removed, we change
	    // the st entry to be a local variable
	    ST* st = WN_st(id);
	    Clear_ST_is_value_parm(st);
	    Set_ST_sclass(st, SCLASS_AUTO);
	    if (IPA_Enable_Array_Sections) { 
		if (state_formal_array != NULL)
		    (*state_formal_array)[i].Set_is_removed(); 
	    }
	} else {
	    WN_kid(func_node,k) = id;
	    ++k;
#ifdef KEY
	    Set_TYLIST_type (New_TYLIST (tylist_idx), Tylist_Table[from_idx]);
#endif

	}
#ifdef KEY
	++from_idx;
#endif

    }
#ifdef KEY
    Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
#endif

    if (n->Has_Aliased_Formal ()) {
	// there are STs that are based on the deleted formals, so we
	// need to change their storage_class to SCLASS_AUTO
	For_all (St_Table, CURRENT_SYMTAB, fix_aliased_formals ());
    }
	
    WN_func_body(func_node) = WN_func_body(w);
    LWN_Parentize(func_node);
    n->Set_Whirl_Tree(func_node);

} // IPA_Propagate_Constants


//-------------------------------------------------------------------
// compute the actual number of parameters used.
//-------------------------------------------------------------------
static UINT32
Compute_param_count (INT kid, const VALUE_DYN_ARRAY& cprop_annot)
{
#ifdef KEY
    INT last = MIN (kid, cprop_annot.Lastidx () + 1);
#else
    INT last = min (kid, cprop_annot.Lastidx () + 1);
#endif

    for (INT i = 0; i < last; ++i) {
	if (cprop_annot[i].Is_remove_param())
	    --kid;
    }

    return kid;
}


//-------------------------------------------------------------------
// update the call whirl node
//-------------------------------------------------------------------
void
Reset_param_list (IPA_NODE *caller, IPA_NODE *callee, IPA_EDGE *edge,
		  IPA_CALL_GRAPH* cg)
{
    IPA_NODE_CONTEXT context (caller);
    cg->Map_Callsites (caller);
    WN* caller_wn = caller->Whirl_Tree();
    WN* call = edge->Whirl_Node();
    UINT orig_param_count = WN_kid_count (call);
    
    // TODO: handle indirect call
    Is_True (WN_operator(call) != OPR_ICALL,
	     ("Cannot constant propagate through indirect call"));

    VALUE_DYN_ARRAY *cprop_annot = callee->Cprop_Annot();
    if (cprop_annot == NULL) {
      return;
    }

    UINT used_param_count = Compute_param_count (orig_param_count,
						 *cprop_annot); 

    if (used_param_count == orig_param_count)
	// no parameter to be deleted
	return;

    INT fake_param_count = 0;

    if (IPA_Enable_Readonly_Ref && (edge->Has_Readonly_Param() ||
				    edge->Has_Pass_Not_Saved_Param())) {
	UINT annot_size = cprop_annot->Lastidx() + 1;
	for (INT i = 0; i < orig_param_count; ++i)
	    if ((WN_Parm_Read_Only(WN_kid(call, i)) ||
		 WN_Parm_Passed_Not_Saved(WN_kid(call, i))) &&
		(i >= annot_size || (*cprop_annot)[i].Is_remove_param()))
		++fake_param_count;
    }

    WN* parent_block = WN_Get_Parent (call, Parent_Map, Current_Map_Tab);
    
    if (Trace_IPA)
	fprintf(TFile, "Fixing call site in caller %s \n",
		ST_name(WN_st(caller_wn)));

    TYPE_ID rtype = WN_rtype(call);
    TYPE_ID desc = WN_desc(call);
    WN* new_call = WN_Call (rtype, desc, used_param_count + fake_param_count,
			    WN_st(call));
    WN_call_flag (new_call) = WN_call_flag (call);
#ifdef KEY
    WN_Set_Linenum (new_call, WN_Get_Linenum (call));
#endif // KEY

    if (caller->Has_frequency ())
	IPA_WN_MAP32_Set (Current_Map_Tab, WN_MAP_FEEDBACK, new_call,
			  IPA_WN_MAP32_Get (Current_Map_Tab,
					    WN_MAP_FEEDBACK, call));
    
    INT first=0;
    INT last = used_param_count;
    INT i;
    
    if (Trace_IPA || Trace_Perf) {
	fprintf (TFile, "%s called from ", DEMANGLE (callee->Name ()));
	fprintf (TFile, "%s (parm pos): ", DEMANGLE (caller->Name ()));
    }

    for (i = 0; i < orig_param_count; ++i) {
	if (i > cprop_annot->Lastidx() ||
	    ! (*cprop_annot)[i].Is_remove_param ()) {
	    WN_kid(new_call,first) = WN_kid(call,i);  
	    WN_Set_Parent(WN_kid(call,i), new_call, Parent_Map,Current_Map_Tab);
	    ++first;
	} else {

	    if (WN_Parm_Read_Only(WN_kid(call, i)) ||
		WN_Parm_Passed_Not_Saved(WN_kid(call, i))) {
		
		WN_kid(new_call,last) = WN_kid(call, i);
		WN_Set_Parm_Dummy (WN_kid (new_call, last));
		WN_Set_Parent(WN_kid(call,i), new_call, Parent_Map,
			      Current_Map_Tab);
		++last;
		if (Trace_IPA || Trace_Perf)
		    fprintf (TFile, "%d faked ", i);
	    } else if (Trace_IPA || Trace_Perf)
		fprintf (TFile, "%d removed ", i);
	    
	} 
    }

#ifdef KEY
    // bug 2636
    // We have created used_param_count+fake_param_count kids, but it won't
    // always have that many kids.
    if (WN_kid_count (new_call) > last)
    	WN_set_kid_count (new_call, last);

    // Also update the # of parameters in the summary_callsite.
    edge->Summary_Callsite()->Set_param_count (WN_kid_count (new_call));
#endif

    if (Trace_IPA || Trace_Perf)
	fputc ('\n', TFile);

    // insert the new call node and remove the old one
    if (WN_operator (parent_block) == OPR_BLOCK) {
	LWN_Insert_Block_Before (parent_block, call, new_call);
	LWN_Extract_From_Block (parent_block, call);
    } else {
	for (i = 0; i < WN_kid_count (parent_block); ++i) {
	    if (WN_kid (parent_block, i) == call) {
		WN_kid (parent_block, i) = new_call;
		LWN_Set_Parent (new_call, parent_block);
	    }
	}
    }
    edge->Set_Whirl_Node(new_call);
} // Reset_param_list


//-------------------------------------------------------------------
// create a constant st
//-------------------------------------------------------------------
static ST*
Create_Const_ST (const SUMMARY_VALUE& value)
{
  if (value.Is_int_const()) {
    return 
      New_Const_Sym(Enter_tcon(Host_To_Targ(value.Get_mtype(),
                                            value.Get_int_const_value())),
                    MTYPE_To_TY(value.Get_mtype()));
  }
  else if (value.Is_const_st() && value.Get_const_st_idx()) {
    ST* st = ST_ptr(value.Get_const_st_idx());
    if (ST_class (st) != CLASS_CONST) {
      Is_True (ST_is_const_var(st),
               ("Non constant found during constant propagation"));
      const TCON& tc = Tcon_Table[value.Get_tcon_idx()];
      st = New_Const_Sym(value.Get_tcon_idx(), MTYPE_To_TY(TCON_ty(tc)));
    }
    return st;
  }
  
  return 0;
} 


//-------------------------------------------------------------------
// propagate globals into this PU
//-------------------------------------------------------------------
extern void
IPO_propagate_globals(IPA_NODE *n)
{

  IPAA_NODE_INFO* modref_info = n->Mod_Ref_Info();
  GLOBAL_ANNOT* gannot = n->Global_Annot();

  if (gannot && modref_info) {

    BOOL need_to_update_array_bounds = FALSE;
    WN* new_block = WN_CreateBlock();
    WN* pu = n->Whirl_Tree();
#ifdef KEY // bug 12371
    LWN_Parentize(pu);
#endif
    // ST_IDX_HASH_TABLE current_hash_table(8, &Temp_pool);
    
    for (UINT32 i = 0; i < GLOBAL_ANNOT::Size; ++i) {
      if (!gannot->Top(i) && !gannot->Bottom(i)) {
        INT32 modref_key = ST_IDX_index(GLOBAL_ANNOT::Common_ST[i]);
        const GLOBAL_DYN_ARRAY& gvals = gannot->Global_Value_Array(i);
        for (UINT32 j = 0; j < gvals.Elements(); ++j) {
          const SUMMARY_VALUE* value = gvals[j].Value();
          if (value) {
            OFFSET_SIZE os(gvals[j].Offset(), gvals[j].Size());
            ST_IDX st_idx = GLOBAL_ANNOT::Offset_Size_To_ST[i][os];
            if (st_idx && TY_mtype(ST_type(st_idx)) == value->Get_mtype()) {
              
              // check if constant can be propagated into array bounds
              if (IPA_constant_in_array_bounds(*value, pu, ST_ptr(st_idx))) {
                need_to_update_array_bounds = TRUE;
              }

              if (modref_info->Is_def_elmt(modref_key)) {
                Generate_Assignment_Stmt(ST_ptr(st_idx), 
                                         new_block,
                                         Gen_WN_Const(*value));
              }
              else {
                ST* const_st = Create_Const_ST(*value);
                if (const_st) {
                  Replace_Formal_By_Actual(WN_func_body(pu),st_idx,const_st);
                }
              }
            }
          }
        }
      }
    }
      
    // if new constants are found in array bounds, we need to update them
    if (need_to_update_array_bounds) {
      Update_array_bounds(pu);
    }

    // if there are assignment statements then stick them into the tree
    if (WN_first(new_block)) {
      LWN_Insert_Block_Before(WN_func_body(pu), 
                              WN_first(WN_func_body(pu)),
                              new_block);
    }

    LWN_Parentize(pu);
    WN_verifier(pu);
  }

}



