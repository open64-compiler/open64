/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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



#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <stdio.h>
#include <alloca.h>

#include <ext/hash_map>			// stl hash table
#include <ext/algorithm>

#include "defs.h"
#include "config.h"
#include "tracing.h"			// for DBar
#include "cxx_memory.h"			// for CXX_NEW

#include "symtab.h"
#include "irbdata.h"                    // for INITO_st_idx
#include "const.h"                      // for MAX_SYMBOLIC_CONST_NAME_LEN
#include "ttype.h"
#include "targ_sim.h"
#include "config_asm.h"

// global tables
FILE_INFO	File_info;
SCOPE		*Scope_tab;
PU_TAB		Pu_Table;
SYMBOL_TABLE	St_Table;
TY_TAB		Ty_tab;
TYPE_TABLE	Ty_Table;
FLD_TAB		Fld_Table;
TYLIST_TAB	Tylist_Table;
ARB_TAB		Arb_Table;
TCON_TAB	Tcon_Table;
INITV_TAB	Initv_Table;
INITO_TABLE	Inito_Table;
PREG_TABLE	Preg_Table;
LABEL_TABLE	Label_Table;
BLK_TAB		Blk_Table;
ST_ATTR_TABLE	St_Attr_Table;

SYMTAB_IDX Current_scope;		// index to current scope
PU *Current_pu;				// ptr to current PU

//----------------------------------------------------------------------
// ST-related utilities
//----------------------------------------------------------------------

INT64
ST_size (const ST *st)
{
  switch (ST_class(st)) {
  case CLASS_BLOCK:
    return STB_size(st);
  case CLASS_VAR:
  case CLASS_PREG:
    return TY_size(ST_type(st));
  case CLASS_CONST:
    if (TCON_ty(STC_val(st)) == MTYPE_STR)
        return Targ_String_Length (STC_val(st))
                + (TCON_add_null(STC_val(st)) ? 1 : 0);
    else
        return TY_size(ST_type(st));
  case CLASS_FUNC:
    return 0;
  }
  FmtAssert(FALSE, ("ST_size: unexpected CLASS"));
  /* NOTREACHED */
  return 0;
}

// make a copy of a ST in the SAME ST table
ST *
Copy_ST (ST *st)
{
    if (ST_sym_class (st) == CLASS_PREG)
	return st;

    SYMTAB_IDX level = ST_IDX_level (ST_st_idx (st));

    ST_TAB *st_tab = Scope_tab[level].st_tab;
    ST_IDX st_idx = make_ST_IDX (st_tab->Insert (*st), level);
    ST& copy = St_Table[st_idx];
    Set_ST_st_idx (copy, st_idx);
    if (ST_base_idx (st) == ST_st_idx (st))
	Set_ST_base_idx (copy, st_idx);
    return &copy;
} // Copy_ST


ST *
Copy_ST (ST *st, SYMTAB_IDX scope)
{
    if (scope == ST_level (st))
	return Copy_ST (st);

    ST_TAB *st_tab = Scope_tab[scope].st_tab;
    ST_IDX st_idx = make_ST_IDX (st_tab->Insert (*st), scope);
    ST& copy = St_Table[st_idx];
    Set_ST_st_idx (copy, st_idx);
    if (ST_base_idx (st) == ST_st_idx (st))
	Set_ST_base_idx (copy, st_idx);
    else if (scope != ST_IDX_level (ST_base_idx (st))) {
	ST *base_st = Copy_ST (&St_Table[ST_base_idx (st)], scope);
	Set_ST_base_idx (copy, ST_st_idx (base_st));
    }
    return &copy;
} // Copy_ST

ST *
Copy_ST_No_Base (ST *st, SYMTAB_IDX scope)
{
    static INT Temp_Index = 0;
    STR_IDX new_name;
    if (scope == GLOBAL_SYMTAB) 
#ifdef TARG_NVISA
        new_name = Save_Str2i(ST_name(st),"__", Temp_Index++);
#else
        new_name = Save_Str2i(ST_name(st),"..", Temp_Index++);
#endif
    else
	new_name = ST_name_idx(st);

    if (scope == ST_level (st)) {
	ST* new_st = Copy_ST (st);
        Set_ST_name_idx (*new_st, new_name);
	return new_st;
    }

    ST_TAB *st_tab = Scope_tab[scope].st_tab;
    ST_IDX st_idx = make_ST_IDX (st_tab->Insert (*st), scope);
    ST& copy = St_Table[st_idx];
    Set_ST_st_idx (copy, st_idx);
    Set_ST_name_idx (copy, new_name);
    if (ST_base_idx (st) == ST_st_idx (st)) {
	Set_ST_base_idx (copy, st_idx);
    }
    return &copy;
} // Copy_ST


// Return TRUE if the ST is a constant literal or a const variable.
BOOL
ST_is_constant (const ST *st)
{
    switch (ST_sym_class(st)) {
    case CLASS_CONST:
	return TRUE;
    case CLASS_VAR:
	return ST_is_const_var(st);
    default:
	return FALSE;
    }
}

// ====================================================================
// Returns FALSE if the current PU is recursive or the ST is not in
// the local symtab. May return TRUE otherwise; the more we return
// TRUE, the better we optimize.
// ====================================================================

extern BOOL
ST_is_private_local(const ST *st)
{
  Is_True((PU_src_lang(Get_Current_PU()) &
	   (PU_src_lang(Get_Current_PU()) - 1)) == 0,
	  ("ST_is_private_local: Mixed-language inlining not "
	   "allowed; need to know original PU language"));
 
  if (ST_IDX_level(ST_st_idx(st)) != CURRENT_SYMTAB)
    return FALSE;

  // Fix 643997:  for F90, -LANG:recursive=on force the compiler
  // to assume every procedure is recursive.
  //
  // If -LANG:recursive=[on|off] is set by user
  // it overrides the language default.
  if (LANG_Recursive_Set)
    return !LANG_Recursive;
  
  // TODO:  should examine PU_recursive flag for all languages.
  //
  switch (PU_src_lang(Get_Current_PU())) {
  case PU_F77_LANG:
    // F77 default is to assume procedure are not recursive.
    return TRUE;

  case PU_F90_LANG:
    // The default is to rely on the PU_recursive flag set by frontend.
    return (!PU_recursive(Get_Current_PU()));
  }
  return FALSE;
}

//----------------------------------------------------------------------
// TY-related utilities
//----------------------------------------------------------------------


TY_IDX
Copy_TY (TY_IDX ty)
{

    TY_IDX copy = ty;
    Set_TY_IDX_index (copy, Ty_tab.Insert (Ty_Table[ty]));
    return copy;
}


// assume align is a power of 2.
UINT
TY_log_base2 (UINT align)
{
    UINT result = 0;

    Is_True (align, ("Invalid alignment for TY"));

    while ((align & 0x7) == 0) {
	result += 3;
	align >>= 3;
    }

    return result + (align >> 1);
} // TY_log_base2


/* ====================================================================
 *
 * FLD_get_to_field
 *
 * Given the TY of a struct, get to the field corresponding to field_id.
 * cur_field_id gives the field_id of the struct itself.  If the field is
 * found, cur_field_id is set equal to field_id.  Otherwise, Is_Null() is
 * true for the field handle returned, and cur_field_id is set to 
 * correspond to the last field of this struct.
 *
 * ==================================================================== */
FLD_HANDLE 
FLD_get_to_field (TY_IDX struct_ty_idx, UINT field_id, UINT &cur_field_id)
{
    FLD_ITER fld_iter = Make_fld_iter(TY_fld(struct_ty_idx));
    do {
	FLD_HANDLE fld(fld_iter);
	cur_field_id++;
	if (cur_field_id == field_id)
	    return fld;
	if (TY_kind(FLD_type(fld)) == KIND_STRUCT &&
            TY_fld(FLD_type(fld)) != FLD_HANDLE()) {
	    fld = FLD_get_to_field(FLD_type(fld), field_id, cur_field_id);
	    if (cur_field_id == field_id)
		return fld;
	} 
    } while (!FLD_last_field(fld_iter++));
    return FLD_HANDLE();
} // FLD_get_to_field


// reverse map for getting the pointer TY given the pointee


// Utility functions for mapping a TY to the one that points to it.
// A hash_map is used because the number of pointer types should be small
// compared to all TYs.

// we uniquely identify a pointee by it's TY_IDX and the TY_flags of the
// pointer 
typedef std::pair<TY_IDX, UINT32> TY_POINTEE_KEY;

struct hash_pointee_key
{
    UINT32 operator() (TY_POINTEE_KEY key) const {
	return (key.first | key.second);
    }
};

typedef __gnu_cxx::hash_map<TY_POINTEE_KEY, TY_IDX, hash_pointee_key>
    TY_IDX_POINTER_MAP;

static TY_IDX_POINTER_MAP pointer_map;
static std::pair<TY_POINTEE_KEY, TY_IDX> last_valid_map_entry;

// pointer_map maps a pointee to its corresponding pointer

static BOOL
Invalid_Pointer_Map_Entry (const TY_IDX_POINTER_MAP::value_type& entry)
{
    TY_IDX pointer = entry.second;
    TY_IDX pointee = entry.first.first;

    if (TY_IDX_index (pointer) >= TY_Table_Size () ||
	TY_IDX_index (pointee) >= TY_Table_Size ())
	return TRUE;

    const TY& ty = Ty_Table[pointer];

    if (TY_kind (ty) != KIND_POINTER ||
	TY_pointed (ty) != pointee ||
	TY_flags (ty) != entry.first.second)
	return TRUE;

    return FALSE;
} // Invalid_Pointer_Map_Entry


// in case the pointer_map is corrupted, we need to delete all invalid
// entries.  This should rarely happen, but still possible if some pointer
// types which have been inserted into the pointer_map are deleted.
static void
Validate_Pointer_Map ()
{
    typedef TY_IDX_POINTER_MAP::iterator ITER;
    std::vector<ITER> invalid_entries;
    UINT last_valid = 0;
    last_valid_map_entry.second = 0;
    
    for (ITER i = pointer_map.begin (); i != pointer_map.end (); ++i) {
	if (Invalid_Pointer_Map_Entry (*i))
	    invalid_entries.push_back (i);
	else {
	    if (last_valid < TY_IDX_index ((*i).second)) {
		last_valid = TY_IDX_index ((*i).second);
		last_valid_map_entry = *i;
	    }
	}
    }

    for (std::vector<ITER>::const_iterator first = invalid_entries.begin ();
	 first != invalid_entries.end (); ++first)
	pointer_map.erase (*first);
}


struct update_pointer_map
{
    void operator() (UINT32 idx, const TY* ty) const {
	if (TY_kind (*ty) != KIND_POINTER || TY_mtype (*ty) != Pointer_Mtype)
	    return;
	TY_POINTEE_KEY key (TY_pointed (*ty), TY_flags (*ty));
	last_valid_map_entry = std::make_pair (key, make_TY_IDX (idx));
	pointer_map.insert (last_valid_map_entry);
    }
};

static void
Update_Pointer_Map ()
{
    if (TY_IDX_index (last_valid_map_entry.second) != 0 &&
	Invalid_Pointer_Map_Entry (last_valid_map_entry)) {
	DevWarn ("Rehashing TY pointer map -- this should NOT happen often");
	Validate_Pointer_Map ();
    }
    For_all_entries (Ty_tab, update_pointer_map (),
		     TY_IDX_index (last_valid_map_entry.second) + 1);
}


static inline TY_IDX
Find_Ty_Pointer (TY_POINTEE_KEY key)
{
    TY_IDX_POINTER_MAP::iterator result = pointer_map.find (key);

    if (result == pointer_map.end ())
	return 0;

    if (Invalid_Pointer_Map_Entry (*result)) {
	DevWarn ("Rehashing TY pointer map -- this should NOT happen often");
	Validate_Pointer_Map ();
	return 0;
    }

    TY_IDX pointer = (*result).second;
    Set_TY_align (pointer, Pointer_Size);
    return pointer;

} // Find_Ty_Pointer	


TY_IDX
TY_pointer (TY_IDX pointee, BOOL f90_pointer)
{
    TY_POINTEE_KEY key (pointee, f90_pointer ? TY_IS_F90_POINTER : 0);

    TY_IDX result = Find_Ty_Pointer (key);

    if (result == 0) {
	Update_Pointer_Map ();
	return Find_Ty_Pointer (key);
    }

    return result;

} // TY_pointer


TY_IDX
make_ptr_type (TY_IDX ty_idx, BOOL f90_pointer)
{
    TY_IDX ty_ptr = TY_pointer (ty_idx, f90_pointer);

    if (ty_ptr != 0)
	return ty_ptr;
   
    TY &ty = New_TY (ty_ptr);
    TY_Init (ty, Pointer_Size, KIND_POINTER, Pointer_Mtype,
	     Save_Str ("anon_ptr."));
    Set_TY_pointed (ty, ty_idx);
    if (f90_pointer)
	Set_TY_is_f90_pointer (ty);
    Set_TY_align (ty_ptr, Pointer_Size);
    
    return ty_ptr;
}

TY_IDX
Make_Pointer_Type (TY_IDX ty_idx, BOOL)
{
    //
    // This now only returns non-f90 pointers. It ignores the second argument
    //
    return make_ptr_type (ty_idx, FALSE);
} // Make_Pointer_Type

TY_IDX
Make_F90_Pointer_Type (TY_IDX ty_idx)
{
   //
   // This now only returns f90 pointers
   //
    return make_ptr_type (ty_idx, TRUE);
    
} // Make_F90_Pointer_Type


TY_IDX
Make_Function_Type(TY_IDX return_ty_idx)
{
  // Return the type of a function with unspecified parameters and the
  // specified return type. First, iterate through the known types and
  // see if we can return an existing type. Then as a last resort,
  // create the new type if we didn't find it.
#ifdef ROBERT
  //
  // Note: Check for duplicates only against functions of no
  // parameters. I believe functions that return values through their
  // first parameter have TY_arg_area_size != 0, and so are excluded
  // from being seen as duplicates. Wilson says we currently call
  // Make_Function_Type only for functions of no parameters.
  //      -- RK 971013
  TY_ITER ty;
  // TODO: There should be some way to get at Ty_tab.begin() and
  // Ty_tab.end() through Ty_Table. In other words, Ty_Table should be
  // the only visible way of getting at the table of types. Having two
  // names for the same thing is nasty. -- RK
  ty = Ty_tab.begin();
  for (++ty;
       ty != Ty_tab.end();
       ty++) {
    TYLIST_IDX known_return_tylist;
    if (TY_kind(*ty) == KIND_FUNCTION &&
	(known_return_tylist = TY_tylist(*ty)) != (TYLIST_IDX) 0 &&
	TYLIST_type(known_return_tylist) == return_ty_idx &&
	(return_ty_idx == 0 ||
	 TYLIST_type(known_return_tylist + 1) == (TY_IDX) 0)) {
      return ty.Index();
    }
  }
  TYLIST_IDX  new_return_tylist_idx;
  TYLIST     &new_return_tylist = New_TYLIST(new_return_tylist_idx);
  Set_TYLIST_type(new_return_tylist, return_ty_idx);

  if (return_ty_idx != (TY_IDX) 0) {
    TYLIST_IDX tail_return_tylist_idx;
    new_return_tylist = New_TYLIST(tail_return_tylist_idx);
    Is_True(tail_return_tylist_idx == new_return_tylist_idx + 1,
	    ("Make_Function_Type: TYLIST allocation messed up"));
    Set_TYLIST_type(new_return_tylist, (TY_IDX) 0);
  }

  TY_IDX  function_ty_idx;
  TY     &function_ty = New_TY(function_ty_idx);
  static char  buf[32];
  static INT32 tcount = 0;

  ++tcount;
  sprintf(buf, ".anon_functype.%d", tcount);
  TY_Init(function_ty, (UINT64) 0, KIND_FUNCTION, MTYPE_UNKNOWN,
	  Save_Str(buf));
  return function_ty_idx;
#else
  TY_IDX ty_idx;
  TY&    ty = New_TY (ty_idx);

  TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align (ty_idx, 1);

  TYLIST_IDX tylist_idx;

  (void) New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = return_ty_idx;
  Set_TY_tylist (ty, tylist_idx);

  (void) New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = 0;

  // check that TY is unique
  TY_IDX new_ty_idx = TY_is_unique (ty_idx);
  if (new_ty_idx != ty_idx && TY_IDX_index (ty_idx) == Ty_tab.Size () - 1) {
	// found duplicate
	// delete last two tylist entries
        Tylist_Table.Delete_last (2);
	Ty_tab.Delete_last ();
	ty_idx = new_ty_idx;
  }
  return ty_idx;
#endif
}

static std::vector<ST *> intrinsic_list;

struct matches_pu_ty_and_name {
  const TY_IDX        ty;
  const char   *const name;

  matches_pu_ty_and_name(const TY_IDX type, const char *const func_name) :
    ty(type), name(func_name)
      { }

  BOOL operator()(const ST *const st) {
    return (ST_pu_type(st) == ty &&
	    strcmp(ST_name(st), name) == 0);
  }
};

static void INTRINSIC_LIST_add(ST *st)
{
	intrinsic_list.push_back(st);
}

ST *
INTRINSIC_LIST_lookup(TY_IDX  ty,
		      const char   *function_name)
{
  vector<ST *>::iterator result =
                std::find_if(intrinsic_list.begin(),
                             intrinsic_list.end(),
                             matches_pu_ty_and_name(ty, function_name));
  if (result == intrinsic_list.end()) {
    return NULL;
  }
  else {
    return *result;
  }
}

ST *
Gen_Intrinsic_Function(TY_IDX  ty, const char   *function_name)
{
  ST *st = INTRINSIC_LIST_lookup(ty, function_name);

  if (st == NULL) {
    st = New_ST(GLOBAL_SYMTAB);

    PU_IDX pu_idx;
    PU&    pu = New_PU (pu_idx);

    PU_Init (pu, ty, CURRENT_SYMTAB);

    ST_Init (st, Save_Str(function_name), CLASS_FUNC, SCLASS_EXTERN,
             EXPORT_PREEMPTIBLE, (TY_IDX) pu_idx);

#ifdef TARG_X8664
    if (Is_Target_32bit()) {
      if (Use_Sse_Reg_Parm)
        Set_TY_has_sseregister_parm (ty);
      if (Use_Reg_Parm)
        Set_TY_register_parm (ty, Use_Reg_Parm);
    }
#endif

    INTRINSIC_LIST_add(st);
  }
  return st;
}


// Make an array type of equal len in each dimension
TY_IDX
Make_Array_Type (TYPE_ID element, INT32 ndim, INT64 len)
{
    FmtAssert(MTYPE_RegisterSize(element) > 0 && MTYPE_align_best(element) > 0,
              ("Cannot make an array of %s", MTYPE_name(element))); 
    ARB_HANDLE arb,arb_first;
    for (UINT i = 0; i < ndim; ++i) {
       arb = New_ARB ();
       if (i==0) {
	 arb_first = arb;
       }
       ARB_Init (arb, 0, len - 1, MTYPE_RegisterSize (element));
       Set_ARB_dimension (arb, ndim-i);
    }
    
    Set_ARB_last_dimen (arb);
    Set_ARB_first_dimen (arb_first);

    TY_IDX ty_idx;
    TY& ty = New_TY (ty_idx);
    TY_Init (ty, MTYPE_RegisterSize(element) * ndim * len, KIND_ARRAY,
	     MTYPE_UNKNOWN, 0);
    Set_TY_align (ty_idx, MTYPE_align_best(element));
    Set_TY_etype (ty, MTYPE_To_TY (element));
    Set_TY_arb (ty, arb_first);

    return ty_idx;

} // Make_Array_Type

//-------------------------------------------------------------------------
// The next few TY related functions allow us to check for TYPE_EQUIVALENCE
//-------------------------------------------------------------------------

// Hash Map
// -------------
// We keep a STL hash_map (see below) on the side to narrow down our search
// for duplicate types.
//
// The hash_map maps all TY_IDXs that represent identical types, all present
// in our global type tables, to ONE representative TY_IDX (first encountered).
// In other words, we have a map from multiple TY_IDXs to one unique
// representative.
//
// The hash_map stores the TY_IDX of every unique TY encountered so far.
// To enter types into the hash-table, call TY_is_unique(); if an equivalent
// type does not already exist the given TY_IDX will be entered into the
// hash-table as a unique type; otherwise the equivalent TY_IDX is returned.
// Note that both our hashing function and our "equal" function operate
// on the TY, not just on the TY_IDX.

// Why multiple hash_maps?
// ----------------------------
// Since the TY_are_equivalent function is fairly expensive we need good hash
// functions to reduce the number of likely matches. To that end we define 
// five different hash multimaps (rather than one) depending on the TY_kind; 
// one map each for 
// (a) scalars (and void), (b) pointers, (c) functions, (d) arrays, (e) structs

// the type of the hash table key (i.e., what the hash function takes as input)
typedef TY_IDX HashKeyType;

// Hash functions based on TY_Kind
// -------------------------------
// We use TY_kind to determine an appropriate hash-function:
// a For scalars (and void) we just use the "mtype", 
// b For pointers we use the mtype of what the pointer points to
// c For functions we use a hash based on the function signature (TY_fun_hash)
// d For arrays we use a hash function based on dim/base/bound (TY_array_hash)
// e For structs we use a hash function based on the struct name/size
// These hash functions are now defined

//----------------------------------------------------------------------
// TY_fun_hash   : maps a function prototype into a size_t value
// If the function has a signature: t1 * .. * tn -> t
// then it is represented in the TYLIST_TABLE as <t,t1,...,tn>
// We hash it in base MTYPE_LAST as the number <t_m,t1_m,...,tn_m>
// We create an MTYPE_LAST-ary number by extracting the mtypes of 
// each ti denoted by ti_m i.e., (let ML denote MTYPE_LAST)
//  (((t_m *ML) + t1_m) * ML) + ... + tn_m
//----------------------------------------------------------------------
size_t 
TY_fun_hash (const TY& ty) {
  size_t value = 0;

  TYLIST_ITER fn_iter = Make_tylist_iter(TY_tylist(ty));

  while (*fn_iter) {
    value = MTYPE_LAST * value + TY_mtype(Ty_Table[*fn_iter++]);
  }

  return value;
} // TY_fun_hash

//----------------------------------------------------------------------
// TY_array_hash : hashes an array into a size_t value determined by
//  upper_bound_value (for first dim) + (#dimensions * mtype(array_elmt))
//  if two arrays have the same base type, the same number of dimensions
//  and the same value for the upper bound of the first dimension then
//  they are likely the same and are hashed to the same bucket
//  eg int a[10][2]   and int a[10][3] would be hashed to the same bucket
//  whereas  int a[10] and double b[10] would be hashed to diff buckets 
//  likewise int a[10] and int b[11] would be hashed to different buckets
//  just as  int a[10] and int a[10][2] would be hashed to different buckets
//----------------------------------------------------------------------
size_t 
TY_array_hash (const TY& ty) {
  return ARB_ubnd_val(TY_arb(ty)) + 
    ARB_dimension(TY_arb(ty))  * TY_mtype(Ty_Table[TY_etype(ty)]) ;

} // TY_array_hash

//----------------------------------------------------------------------
// TY_struct_hash: maps a struct into a size_t value
// Return the name_idx (if set) or the size
// If the struct has a name use the unique Str_Table index as a hash
// If the struct is anonymous, use its size as a distinguishing feature
//----------------------------------------------------------------------
inline size_t 
TY_struct_hash (const TY& ty) {
  return TY_name_idx(ty) ? TY_name_idx(ty) : TY_size(ty);
} // TY_struct_hash
 
// The hash function: TY_IDX --> size_t
struct TY_hash {
  size_t operator()(TY_IDX ty_id) const { 
    TY &ty = Ty_Table[ty_id];
    switch (TY_kind(ty)) {
    case KIND_SCALAR:
    case KIND_VOID:
      return TY_mtype(ty);
    case KIND_ARRAY:
      return TY_array_hash(ty);
    case KIND_POINTER:
      return TY_mtype(Ty_Table [TY_pointed(ty)]);
    case KIND_FUNCTION:
      return TY_fun_hash(ty);
    case KIND_STRUCT:
      return TY_struct_hash(ty);
    case KIND_INVALID:
    default:
    case KIND_LAST:     
      Fail_FmtAssertion ("invalid TY_KIND in hash fn");
      return 0;
    } // switch
  } // operator()
}; // struct TY_hash


// Hash key comparison function
// When are two entries in the hash table "equal"?

struct TY_EQUIV
{
  bool operator () ( TY_IDX k1,  TY_IDX k2) const 
    {
      return TY_are_equivalent(k1, k2);
    }
};

// The type definition of the hash table that we will use

typedef __gnu_cxx::hash_map<TY_IDX, TY_IDX, TY_hash, TY_EQUIV> HASH_TY_TABLE;

// The global Hash Table data structures
// Depending on the TY_KIND we pick one of the five tables below.

HASH_TY_TABLE Hash_ty_scalar_table; 
HASH_TY_TABLE Hash_ty_array_table; 
HASH_TY_TABLE Hash_ty_struct_table; 
HASH_TY_TABLE Hash_ty_pointer_table; 
HASH_TY_TABLE Hash_ty_function_table; 

// The following is necessary to handle recursively defined types,
// such as struct with a pointer to itself as a member field.
// We maintain a record of the order in which types are visited
// in the global type table for each of the two types being compared.
// The following datastructure should be initialized to zero between
// top-level calls to TY_are_equivalent(), and Ty_Equiv_Visit_Number
// should also be zero between such top-level calls.
//
typedef SEGMENTED_ARRAY<UINT32> TY_EQUIV_VISIT_ORDER;
static TY_EQUIV_VISIT_ORDER Ty_Equiv_Visit1;
static TY_EQUIV_VISIT_ORDER Ty_Equiv_Visit2;
static UINT32               Ty_Equiv_Visit_Number = 0;

static BOOL
Push_Equiv_Visit_Order(UINT32 ty_index1, UINT32 ty_index2, BOOL &are_equiv)
{
  const UINT32 ty_tbl_size = TY_Table_Size();
  BOOL         pushed = FALSE;

  // Make sure the number of entries match the number of types,
  // and that entries are initialized to zero.
  //
  for (UINT32 i = Ty_Equiv_Visit1.Size(); i < ty_tbl_size; i++)
  {
     UINT dummy;
     Ty_Equiv_Visit1.New_entry(dummy) = 0;
     Ty_Equiv_Visit2.New_entry(dummy) = 0;
  }
  
  if (Ty_Equiv_Visit1[ty_index1] != Ty_Equiv_Visit2[ty_index2])
     are_equiv = FALSE;
  else if (Ty_Equiv_Visit1[ty_index1] > 0)
     are_equiv = TRUE;
  else
  {
     Ty_Equiv_Visit_Number++;
     Ty_Equiv_Visit1[ty_index1] = Ty_Equiv_Visit_Number;
     Ty_Equiv_Visit2[ty_index2] = Ty_Equiv_Visit_Number;
     are_equiv = FALSE;
     pushed = TRUE;
  }
  return pushed;
} // Push_Equiv_Visit_Order

static void
Pop_Equiv_Visit_Order(UINT32 ty_index1, UINT32 ty_index2)
{
  Ty_Equiv_Visit_Number--;
  Ty_Equiv_Visit1[ty_index1] = 0;
  Ty_Equiv_Visit2[ty_index2] = 0;
} // Push_Equiv_Visit_Order


// ARB_are_equivalent determines when two array types are equivalent?
// ARB_are_equivalent is presumably  called with indices for which
// first_dim is set.
// For each entry in the Arb_Table  in the range
// arb_id1 .. arb_id1'(for which last_dim is set)
// ensure that Arb_Table[arb_id1++] == Arb_Table[arb_id2++]

BOOL
ARB_are_equivalent(ARB_HANDLE arb_id1, ARB_HANDLE arb_id2, UINT32 flags) 
{
  if (arb_id1 == arb_id2)
    return TRUE;

  ARB_HANDLE arb1=arb_id1;
  ARB_HANDLE arb2=arb_id2;
  BOOL keep_going;

  do {
     
    keep_going = ! ARB_last_dimen(arb1) && ! ARB_last_dimen(arb2);
     
    if (ARB_dimension(arb1) != ARB_dimension(arb2))
      return FALSE;

    if (ARB_flags(arb1) != ARB_flags( arb2))
      return FALSE;

    if (ARB_const_lbnd(arb1)) {
      if (ARB_lbnd_val( arb1) != ARB_lbnd_val( arb2))
        return FALSE;
    } else {
      if (ARB_lbnd_var( arb1) != ARB_lbnd_var( arb2))
        return FALSE;
    }

    if (ARB_const_ubnd(arb1)) {
      if (ARB_ubnd_val( arb1) != ARB_ubnd_val( arb2))
        return FALSE;
    } else {
      if (ARB_ubnd_var( arb1) != ARB_ubnd_var( arb2))
        return FALSE;
    }

    if (ARB_const_stride(arb1)) {
      if (ARB_stride_val( arb1) != ARB_stride_val( arb2))
        return FALSE;
    } else {
      if (ARB_stride_var(arb1) != ARB_stride_var( arb2))
        return FALSE;
    }
      
    // All tests pass; continue until you reach "last Dimension"
    if (keep_going) {
       arb1 = arb1[1];
       arb2 = arb2[1];
    }
    
  } while (keep_going);
  
  if ( ARB_last_dimen(arb2) && ARB_last_dimen(arb1))
     return TRUE;
  else
     return FALSE;
           
} // ARB_are_equivalent 

// ARB_swap swaps the positions of two ARB's in the table
void
ARB_swap(ARB_HANDLE arb1, ARB_HANDLE arb2)
{
   ARB temp;
   temp = *(arb1.Entry());
   *(arb1.Entry()) = *(arb2.Entry());
   *(arb2.Entry()) = temp;
}
   


// FLD_are_equivalent determines when two struct fields are type equivalent
// T1 fld_id1     ==   T2 fld_id2 (where fld_idi are struct fields) iff
//    a. T1 == T2 AND
//    b. FLD_name_idx  for fld_id1 == FLD_name_idx  for fld_id2
//    c. FLD_attribute for fld_id1 == FLD_attribute for fld_id2
BOOL
FLD_are_equivalent (FLD_HANDLE fld1, FLD_HANDLE fld2, UINT32 flags)
{
    if (!(flags & TY_EQUIV_IGNORE_NAMES) &&
	FLD_name_idx(fld1) != FLD_name_idx(fld2))
	return FALSE;

    if (FLD_ofst(fld1) != FLD_ofst(fld2) ||
	FLD_bsize(fld1) != FLD_bsize(fld2) ||
	FLD_bofst(fld1) != FLD_bofst(fld2) ||
	FLD_flags(fld1) != FLD_flags(fld2) ||
	FLD_st(fld1) != FLD_st(fld2))
      return FALSE;

    // For recursive type definitions, the front end may not YET have
    // set the TY_IDX (ie set it to 0) when it tries to do TY_are_equivalent
    // We have to special case this case and return FALSE (ie not duplicates)
    // Presumably this case should NOT arise in the backend and hence
    // the #if defined (FRONT_END)
    // TODO: should we restrict to NONF77 front ends?
    // If  either ty_idx is 0  return FALSE

    TY_IDX ty_id1 = FLD_type(fld1);
    TY_IDX ty_id2 = FLD_type(fld2);

#ifdef FRONT_END
    if ( (ty_id1 == 0) || (ty_id2 == 0))
	return FALSE;
#endif //FRONT_END  

    // All fields except type are equivalent; now do structural recursion

    return TY_are_equivalent(ty_id1, ty_id2, flags);

} // FLD_are_equivalent


// go through all members of a struct and return true if all of them are
// equivalent 
static BOOL
Struct_are_equivalent (FLD_HANDLE fld_id1, FLD_HANDLE fld_id2, UINT32 flags)
{
    if (fld_id1 == fld_id2)
	return TRUE;

    FLD_ITER fld1_iter = Make_fld_iter (fld_id1);
    FLD_ITER fld2_iter = Make_fld_iter (fld_id2);

    while (!FLD_last_field (fld1_iter) && !FLD_last_field (fld2_iter)) {
	if (! FLD_are_equivalent (fld1_iter, fld2_iter, flags))
	    return FALSE;
	++fld1_iter;
	++fld2_iter;
    }

    return FLD_are_equivalent (fld1_iter, fld2_iter, flags);
} // Struct_are_equivalent


// TYLIST_are_equivalent determines when two function prototypes are equivalent
// does a  pointwise comparison of component TYs (including return types)

BOOL
TYLIST_are_equivalent (TYLIST_IDX tylist_id1, 
		       TYLIST_IDX tylist_id2, 
		       UINT32     flags)
{
    if (tylist_id1 == tylist_id2)
	return TRUE;
  
    // Function prototypes are lists of tys terminated with a "0" TY_IDX entry
    //
    TY_IDX ty_id1;
    TY_IDX ty_id2;  

    while (Tylist_Table[tylist_id1] != 0) {
    
	ty_id1 = Tylist_Table[tylist_id1];
	ty_id2 = Tylist_Table[tylist_id2];
    
	if ( !TY_are_equivalent(ty_id1, ty_id2, flags))
	    return FALSE;
    
	tylist_id1++;
	tylist_id2++;
    } // end while 

    if (Tylist_Table[tylist_id2] == 0)
	return TRUE;
    else
	return FALSE;
} // TYLIST_are_equivalent


// TY_are_equivalent
// Returns TRUE iff ty1 and ty2 are structurally equivalent
// also checks that names of fields are identical 
// TODO: whether one  COMMON block is a complete subset of the other

BOOL
TY_are_equivalent (TY_IDX ty_id1, TY_IDX ty_id2, UINT32 flags)
{

  // The two types are equivalent when they have identical TY_IDX.
  // They are not necessarily equivalent if their alignments or
  // qualifiers are different.
  //
  const UINT32 ty_index1 = TY_IDX_index(ty_id1);
  const UINT32 ty_index2 = TY_IDX_index(ty_id2);
   
  if (ty_id1 == ty_id2)
    return TRUE;
  else if ((flags & TY_EQUIV_ALIGN) && 
	   TY_align_exp(ty_id1) != TY_align_exp(ty_id2))
    return FALSE;
  else if ((flags & TY_EQUIV_QUALIFIER) &&
	   (TY_is_const(ty_id1) != TY_is_const(ty_id2)       ||
	    TY_is_volatile(ty_id1) != TY_is_volatile(ty_id2) ||
	    TY_is_restrict(ty_id1) != TY_is_restrict(ty_id2)))
    return FALSE;
  else if (ty_index1 == ty_index2)
    return TRUE;

  // Handle nested structural equivalence.  If either of these types were
  // encountered before, then consider the types equivalent only if they
  // were encountered in the same order in recursive applications of this
  // subroutine.
  //
  BOOL are_equiv = FALSE;

  if (Push_Equiv_Visit_Order(ty_index1, ty_index2, are_equiv))
  {
    // Not visited before.
    //
    const TY &ty1 = Ty_Table[ty_id1];
    const TY &ty2 = Ty_Table[ty_id2];
  
    // Check attributes and names, and if they match, check for structural
    // equivalence.
    //
    if ( TY_size(ty1) != TY_size(ty2) ||
	 TY_kind(ty1) != TY_kind(ty2) ||
	 TY_mtype(ty1)!= TY_mtype(ty2)||
	 TY_flags(ty1)!= TY_flags(ty2) )
    { 
      // TODO: do we want to refine this to allow ignoring certain flags?
      are_equiv = FALSE;
    }
    else if (!(flags & TY_EQUIV_IGNORE_NAMES) &&
	     TY_name_idx (ty1) != TY_name_idx (ty2))
    { 
      are_equiv = FALSE;
    }
    else
    {
      // Set flags for recursive calls to this routine.  We always check 
      // that alignment and qualifiers are identical for nested type 
      // references.
      //
      flags |= (TY_EQUIV_ALIGN | TY_EQUIV_QUALIFIER);
       
      // At this point all the above attributes are equal, 
      // so we are ready to do "structural equivalence" checks

      switch (TY_kind (ty1)) {

      case KIND_SCALAR:
      case KIND_VOID:
	are_equiv = TRUE;
	break;

      case KIND_ARRAY:
	// array bounds check and structural recursion on base type
	are_equiv = (ARB_are_equivalent(TY_arb(ty1), TY_arb(ty2), flags) &&
		     TY_are_equivalent( TY_etype(ty1), TY_etype(ty2), flags));
	break;
	  
      case KIND_STRUCT:
	// At this point both the ty have NON_ZERO FLD_IDX

	// FLD equivalence check 
	are_equiv = Struct_are_equivalent(TY_fld(ty1), TY_fld(ty2), flags);
#ifdef KEY
	are_equiv &= (TY_copy_constructor(ty1) == TY_copy_constructor(ty2));
#endif
	break;

      case KIND_POINTER:
	// structural recursion
	are_equiv = TY_are_equivalent (TY_pointed(ty1),TY_pointed(ty2), flags);
	break;

      case KIND_FUNCTION:
	// PU flags check and structural recursion on function prototype
	are_equiv = ( (ty1.Pu_flags() == ty2.Pu_flags()) &&
		      (TYLIST_are_equivalent (TY_tylist(ty1), 
					      TY_tylist(ty2),
					      flags)));
	break;

      case KIND_INVALID:
      default:
	// TODO: do we want to assert here instead of returning FALSE?
	are_equiv = FALSE;
	break;
      }
    }
    Pop_Equiv_Visit_Order(ty_index1, ty_index2);
  }
  return are_equiv;
} // TY_are_equivalent


// TY_is_unique: returns
// a. the input TY_IDX if the input defines a unique ("new") TY. 
// b. TY_IDX with index of an existing type if the input defines a TY that
// is equivalent to the existing TY (uses TY_are_equivalent for this purpose)

static inline TY_IDX
TY_is_unique_op (const TY_IDX ty_idx, HASH_TY_TABLE& hash_ty_table)
{
  TY_IDX return_idx = ty_idx;
  HASH_TY_TABLE::const_iterator hash_entry = hash_ty_table.find (ty_idx);
  if (hash_entry != hash_ty_table.end ())
    // step b: If found, return the TY_IDX of the duplicate
     Set_TY_IDX_index(return_idx, TY_IDX_index((*hash_entry).second));
  else
    hash_ty_table.insert (HASH_TY_TABLE::value_type(ty_idx, ty_idx));
  return return_idx;
}


TY_IDX
TY_is_unique (const TY_IDX ty_idx) 
{
  TY    &ty = Ty_Table[ty_idx];

  // search for the ty in the hash table determined by its TY_kind 

  // step a. Find "potential" matches (using hash tables).  Note that
  // the hashing and equivalence functions ignore any alignment and 
  // qualifiers in the ty_idx.
  //
  switch (TY_kind(ty)) {
    
  case KIND_SCALAR:
  case KIND_VOID:
    return TY_is_unique_op (ty_idx, Hash_ty_scalar_table);

  case KIND_ARRAY:
    return TY_is_unique_op (ty_idx, Hash_ty_array_table);

  case KIND_POINTER:
    return TY_is_unique_op (ty_idx, Hash_ty_pointer_table);

  case KIND_FUNCTION:
    return TY_is_unique_op (ty_idx, Hash_ty_function_table);

  case KIND_STRUCT:
    return TY_is_unique_op (ty_idx, Hash_ty_struct_table);

  case KIND_INVALID:
  default:
  case KIND_LAST:
    Fail_FmtAssertion ("invalid TY_KIND in TY_is_unique");
    return ty_idx;			// should never reach this
  };
} // TY_is_unique

/* ty either is union or has union in one of its fields (called recursively) */
BOOL
TY_has_union (TY_IDX ty)
{
  if (TY_kind(ty) != KIND_STRUCT) 
    return FALSE;
  if (TY_is_union(ty))
    return TRUE;

  FLD_HANDLE fld = TY_fld (ty);
  TY_IDX fty;
  do {
    fty = FLD_type(fld);
    if (TY_has_union(fty))
      return TRUE;
    fld = FLD_next (fld);
  } while (!fld.Is_Null ());
  return FALSE;
}

/* ty has volatile flag in any of its members */
BOOL
TY_has_volatile (TY_IDX ty)
{
  if (TY_is_volatile(ty)) {
    return TRUE;
  }
  if (TY_kind(ty) != KIND_STRUCT) {
    return FALSE;
  }
  
  FLD_HANDLE fld = TY_fld(ty);
  do {
    TY_IDX fty = FLD_type(fld);
    if (TY_has_volatile(fty)) {
      return TRUE;
    }
    fld = FLD_next (fld);
  } while (!fld.Is_Null());
  return FALSE;
}

#ifdef TARG_NVISA
/* number of elements in the vector */
UINT
TY_vector_count (TY_IDX ty)
{
  FmtAssert(TY_can_be_vector(ty), ("not a vector type"));
  FmtAssert(TY_kind(ty) == KIND_STRUCT, ("vector not a struct type"));
  INT count = 0;
  FLD_HANDLE fld = TY_fld (ty);
  do {
	++count;
    	fld = FLD_next (fld);
  } while (!fld.Is_Null ());
  return count;
}
#endif

// return mtype associated with type and offset
TYPE_ID
Mtype_For_Type_Offset (TY_IDX ty, INT64 offset)
{
  switch (TY_kind(ty)) {
  case KIND_STRUCT:
    {
      // return mtype of field
      FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
      do {
        FLD_HANDLE fld(fld_iter);
        if (Is_Composite_Type(FLD_type(fld))
          && offset >= FLD_ofst(fld)
          && offset < FLD_ofst(fld) + TY_size(FLD_type(fld)))
        {
          return Mtype_For_Type_Offset (FLD_type(fld), offset - FLD_ofst(fld));
        }
        if (FLD_ofst(fld) == offset)
          return TY_mtype(FLD_type(fld));
      } while (!FLD_last_field(fld_iter++));
      FmtAssert(FALSE, ("couldn't find matching field"));
    }
  case KIND_ARRAY:
    // return mtype of elements, recursing in case array of structs
    return Mtype_For_Type_Offset (TY_etype(ty),
        offset % TY_size(TY_etype(ty)));
  default:
    return TY_mtype(ty);
  }
}

//----------------------------------------------------------------------
// PREG-related utilities
//----------------------------------------------------------------------

const char *
Preg_Name (PREG_NUM i)
{
    Is_True((i > Last_Dedicated_Preg_Offset),
	    ("Preg_Name:  not valid for dedicated preg"));

    const PREG& preg = Preg_Table[i - Last_Dedicated_Preg_Offset];

    if (PREG_name_idx (preg) == 0)
	return "<preg>";
    else
	return PREG_name (preg);
} // Preg_Name 


// PREG_IDX == PREG_NUM - Last_Dedicated_Preg_Offset

PREG_NUM
Create_Preg_explicit(TYPE_ID mtype, const char *name, 
		     SCOPE *scope_tab, SYMTAB_IDX level)
{
	PREG_IDX preg_idx;
	PREG_IDX preg_idx2;
	PREG &p = New_PREG_explicit (scope_tab, level, preg_idx);
	Set_PREG_name_idx (p, Save_Str(name));
	// pregs of simulated types have to allocate space for
	// all the parts of the lowered types.
	switch (mtype) {
	case MTYPE_C4:
	case MTYPE_C8:
	case MTYPE_C10:
	case MTYPE_FQ:
	case MTYPE_F16:
		// reserve space for another preg
		(void) New_PREG_explicit (scope_tab, level, preg_idx2);
                Set_PREG_name_idx ((*scope_tab[level].preg_tab)[preg_idx2], 0);
		break;
	case MTYPE_CQ:
	case MTYPE_C16:
		// reserve space for 3 more pregs
		(void) New_PREG_explicit (scope_tab, level, preg_idx2);
                Set_PREG_name_idx ((*scope_tab[level].preg_tab)[preg_idx2], 0);
		(void) New_PREG_explicit (scope_tab, level, preg_idx2);
                Set_PREG_name_idx ((*scope_tab[level].preg_tab)[preg_idx2], 0);
		(void) New_PREG_explicit (scope_tab, level, preg_idx2);
                Set_PREG_name_idx ((*scope_tab[level].preg_tab)[preg_idx2], 0);
		break;
	case MTYPE_B:
		// bool needs 2 pregs cause can have complement predicate tn
		(void) New_PREG_explicit (scope_tab, level, preg_idx2);
                Set_PREG_name_idx ((*scope_tab[level].preg_tab)[preg_idx2], 0);
		break;
	}
	// return preg-num of first preg
	return (PREG_NUM) preg_idx + Last_Dedicated_Preg_Offset;
}

PREG_NUM
Create_Preg (TYPE_ID mtype, const char *name)
{
  PREG_NUM preg_num = Create_Preg_explicit(mtype, name, Scope_tab, CURRENT_SYMTAB);
  return preg_num;
}

// uses the real preg size because simulated pregs might take more than one
// preg number.
INT32
Preg_Increment (TYPE_ID mtype)
{
    switch (mtype) {

    case MTYPE_C4:
    case MTYPE_C8:
#if defined(TARG_IA64) || defined(TARG_X8664)
    case MTYPE_C10:
#elif !defined(TARG_LOONGSON)
    case MTYPE_FQ:
#endif
    case MTYPE_F16:
	return 2;

    case MTYPE_C16:
	return 4;

    case MTYPE_CQ:
#if !defined(TARG_X8664) && !defined(TARG_LOONGSON)
	return 4;
#else
        return 2;
#endif

    case MTYPE_I8:
    case MTYPE_U8:
	if (MTYPE_size_reg(MTYPE_I8) > MTYPE_size_reg(Spill_Int_Mtype))
	    return 2;
        break;
    case MTYPE_B:
	// bool mtype not usually used, but if used, saves space for
	// complement preg.
	return 2;
    }
    return 1;
}


//----------------------------------------------------------------------
// BLK utilities
//----------------------------------------------------------------------

BLK_IDX
Copy_BLK (BLK_IDX b)
{
    BLK_IDX copy = b;
    copy = Blk_Table.Insert (Blk_Table[b]);
    return copy;
}

/* ====================================================================
 *
 * Base_Symbol_And_Offset
 *      Input:  ST *st                   Symbol to analyze
 *      Result: ST **base_symbol         primary base of st
 *      Result: INT64 *offset_from_base  offset from primary base
 *
 *
 * ====================================================================
 */
void
Base_Symbol_And_Offset (ST     *st,
                        ST    **base_symbol,
                        INT64  *offset_from_base)
{
  INT64 ofst = 0;
  ST *base = st;

  while ( ST_base(base) != base  ) {
      ofst += ST_ofst(base);
      base = ST_base(base);
  }

  *base_symbol      = base;
  *offset_from_base = ofst;
}


//----------------------------------------------------------------------
// Printing routines
//----------------------------------------------------------------------

/* ====================================================================
 *
 * Class_Name / Sclass_Name / Export_Name / Kind_Name
 *
 * Return ASCII names of ST fields for tracing purposes.
 *
 * ====================================================================
 */

const char *
Class_Name (INT cl)
{
    switch (cl) {
    case CLASS_UNK :
	return "CLASS_UNK";
    case CLASS_VAR :
	return "CLASS_VAR";
    case CLASS_FUNC :
	return "CLASS_FUNC";
    case CLASS_CONST :
	return "CLASS_CONST";
    case CLASS_PREG :
	return "CLASS_PREG";
    case CLASS_BLOCK :
	return "CLASS_BLOCK";
    case CLASS_NAME:
	return "CLASS_NAME";
    default:
	{
	  static char buf[32];
	  sprintf(buf, "Unknown_CLASS(%d)", cl);
	  return buf;
	}
    }
} // Class_Name


const char *
Sclass_Name (INT s)
{
    switch (s) {
    case SCLASS_UNKNOWN: 
	return "UNKNOWN";
    case SCLASS_AUTO:    
	return "AUTO";
    case SCLASS_FORMAL:  
	return "FORMAL";
    case SCLASS_FORMAL_REF:  
	return "FORMAL_REF";
    case SCLASS_PSTATIC: 
	return "PSTATIC";
    case SCLASS_FSTATIC: 
	return "FSTATIC";
    case SCLASS_COMMON:  
	return "COMMON";
    case SCLASS_EXTERN:  
	return "EXTERN";
    case SCLASS_UGLOBAL: 
	return "UGLOBAL";
    case SCLASS_DGLOBAL: 
	return "DGLOBAL";
    case SCLASS_TEXT:    
	return "TEXT";
    case SCLASS_REG:    
	return "REG";
    case SCLASS_CPLINIT:    
	return "CPLINIT";
    case SCLASS_EH_REGION:    
	return "EH_REGION";
    case SCLASS_EH_REGION_SUPP:
	return "EH_REGION_SUPP";
    case SCLASS_DISTR_ARRAY:    
	return "DISTR_ARRAY";
    case SCLASS_THREAD_PRIVATE_FUNCS:    
	return "THREAD_PRIVATE_FUNCS";
    case SCLASS_COMMENT:    
	return "COMMENT";
    default:
	{
	  static char buf[32];
	  sprintf(buf, "Unknown_SCLASS(%d)", s);
	  return buf;
	}
    }
} // Sclass_Name

const char *
Export_Name (INT e)
{
    switch (e) {
    case EXPORT_LOCAL:
	return "XLOCAL";
    case EXPORT_LOCAL_INTERNAL:
	return "XLOCAL_INTERNAL";
    case EXPORT_INTERNAL:
	return "XINTERNAL";
    case EXPORT_HIDDEN:
	return "XHIDDEN";
    case EXPORT_PROTECTED:
	return "XPROTECTED";
    case EXPORT_PREEMPTIBLE:
	return "XPREEMPTIBLE";
    case EXPORT_OPTIONAL:
	return "XOPTIONAL";
    default:
	{
	  static char buf[32];
	  sprintf(buf, "Unknown_Export_Scope(%d)", e);
	  return buf;
	}
    }
} // Export_Name

const char *
Kind_Name (INT k)
{
    struct knm {
	char name[32];
    };
    static struct knm knb[4];
    static INT16 knb_used;
    char *r;

    switch ( k ) {
    case KIND_SCALAR:
	return "KIND_SCALAR";
    case KIND_ARRAY:
	return "KIND_ARRAY";
    case KIND_STRUCT:
	return "KIND_STRUCT";
    case KIND_POINTER:
	return "KIND_POINTER";
    case KIND_FUNCTION:
	return "KIND_FUNCTION";
    case KIND_VOID:
	return "KIND_VOID";
    }

    r = knb[knb_used].name;
    knb_used = (knb_used + 1) % 4;
    sprintf ( r, "KIND_%1d", k );
    return r;
} // Kind_Name


static void
Print_type_attributes (FILE *f, TY_IDX ty)
{
    if (TY_is_const (ty))
	fputs ("const ", f);
    if (TY_is_volatile (ty))
	fputs ("volatile ", f);
    if (TY_is_restrict (ty))
	fputs ("restrict ", f);
} // Print_type_attributes

static void
Print_type_attributes (std::ostream& os, TY_IDX ty)
{
    if (TY_is_const (ty))
	os << "const ";
    if (TY_is_volatile (ty))
	os << "volatile ";
    if (TY_is_restrict (ty))
	os << "restrict ";
} // Print_type_attributes

static void
Print_TY_IDX_verbose (FILE *f, TY_IDX idx)
{
    Print_type_attributes (f, idx);
    if (TY_IDX_index (idx) == 0) {
	fputs ("<NULL>", f);
	return;
    }
	
    const TY& ty = Ty_Table[idx];
    const char *name = TY_name_idx (ty) == 0 ? "(anon)" : TY_name (ty);
    fprintf (f, "%s (#%d) align %d", name, TY_IDX_index (idx), TY_align (idx));
} // Print_TY_IDX_verbose


static const char *
TY_kind_name (const TY& ty)
{
    if (TY_kind (ty) == KIND_SCALAR && TY_mtype (ty) != MTYPE_UNKNOWN)
	return MTYPE_name (TY_mtype (ty));
    else
	return Kind_Name (TY_kind (ty));
}

void
ST::Print (FILE *f, BOOL verbose) const
{
    const char *name_str = (sym_class == CLASS_CONST) ?
	"<constant>" : &Str_Table[u1.name_idx];

    fprintf (f, "%-14s\t<%d,%d> ", name_str, ST_IDX_level (st_idx),
	     ST_IDX_index (st_idx));
    if (strlen (name_str) > 20)
	fputs ("\n\t\t", f);

    TY_IDX ty_idx = 0;

    switch (sym_class) {

    case CLASS_UNK:
	fputs ("Class unknown", f);
	break;

    case CLASS_VAR:
	fputs ("Variable", f);
	ty_idx = u2.type;
	break;

    case CLASS_FUNC:
	fputs ("Subprogram", f);
	ty_idx = PU_prototype (Pu_Table[u2.pu]);
	break;

    case CLASS_CONST:
	fputs ("Constant", f);
	ty_idx = u2.type;
	break;

    case CLASS_PREG:
	fputs ("Pseudo-Register", f);
	ty_idx = u2.type;
	break;

    case CLASS_BLOCK:
	fputs ("Block", f);
	fprintf (f, " (#%d)", u2.blk);
	break;

    case CLASS_NAME:
	fputs ("Name-only", f);
	break;
	
    default:
	fprintf (f, "INVALID CLASS (%d)", sym_class);
	break;
    }

    const TY& ty = Ty_Table [ty_idx];

    if (ty_idx != 0) {

	name_str = TY_name_idx (ty) == 0 ? NULL : TY_name (ty);

	if (!(sym_class == CLASS_FUNC) || name_str != NULL) {
	    fputs (" of type ", f);

	    Print_type_attributes (f, ty_idx);

	    fputs (name_str ? name_str : "(anon)", f);
	    
	    const TY *pty = &ty;
	    INT pcount = 0;
	    while (TY_kind (*pty) == KIND_POINTER) {
		pty = &Ty_Table[TY_pointed (*pty)];
		++pcount;
	    }
	    
	    if (verbose) {
		name_str = TY_kind_name (*pty);
		fprintf (f, " (#%d, %s", TY_IDX_index (ty_idx), name_str);
		while (pcount-- > 0)
		    fputc ('*', f);
		fputc (')', f);
	    } else
		fprintf (f, " (#%d)", TY_IDX_index (ty_idx));
	}
    }

    if (!verbose) {
	/* quick address */
	fprintf (f, " @ 0x%llx", offset);
	if (base_idx != 0)
	    fprintf (f, "(%s)", ST_name (base_idx));
    }
    fputc ('\n', f);

    if (sym_class == CLASS_FUNC && verbose) {
	/* Give info about the type being returned, which is different
	 * than the type of the function.
	 */
	if (ty_idx != 0 && TY_tylist (ty) != 0) {
	    TY_IDX rettype_idx = Tylist_Table[TY_tylist (ty)];
	    const TY& rettype = Ty_Table[rettype_idx];
	    fputs ("\t\tReturning ", f);
	    Print_type_attributes (f, rettype_idx);
	    fputs (TY_name (rettype), f);
	    name_str = TY_kind_name (rettype);
	    fprintf (f, " (#%d, %s)  ", TY_IDX_index (rettype_idx), name_str);

	    fprintf (f, "PU[%d] ", u2.pu);
	    if (Pu_Table[u2.pu].src_lang & PU_C_LANG)	fprintf (f, "C  ");
	    if (Pu_Table[u2.pu].src_lang & PU_CXX_LANG)	fprintf (f, "C++  ");
	    if (Pu_Table[u2.pu].src_lang & PU_F77_LANG)	fprintf (f, "F77  ");
	    if (Pu_Table[u2.pu].src_lang & PU_F90_LANG)	fprintf (f, "F90  ");

	    mUINT64 flags = Pu_Table[u2.pu].flags;
	    fprintf (f, "flags:");
	    if (flags & PU_IS_PURE)		fprintf (f, " pure");
	    if (flags & PU_NO_SIDE_EFFECTS)	fprintf (f, " no_side_effects");
	    if (flags & PU_IS_INLINE_FUNCTION)	fprintf (f, " inline");
	    if (flags & PU_NO_INLINE)		fprintf (f, " no_inline");
	    if (flags & PU_MUST_INLINE)		fprintf (f, " must_inline");
	    if (flags & PU_NO_DELETE)		fprintf (f, " no_delete");
	    if (flags & PU_HAS_EXC_SCOPES)	fprintf (f, " exc_scopes");
	    if (flags & PU_IS_NESTED_FUNC)	fprintf (f, " nested_func");
	    if (flags & PU_HAS_NON_MANGLED_CALL)fprintf (f, " non_mangled_call");
	    if (flags & PU_ARGS_ALIASED)	fprintf (f, " args_aliased");
	    if (flags & PU_NEEDS_FILL_ALIGN_LOWERING)fprintf (f, " fill_align");
	    if (flags & PU_NEEDS_T9)		fprintf (f, " t9");
	    if (flags & PU_HAS_VERY_HIGH_WHIRL)	fprintf (f, " very_high_whirl");
	    if (flags & PU_HAS_ALTENTRY)	fprintf (f, " altentry");
	    if (flags & PU_RECURSIVE)		fprintf (f, " recursive");
	    if (flags & PU_IS_MAINPU)		fprintf (f, " main");
	    if (flags & PU_UPLEVEL)		fprintf (f, " uplevel");
	    if (flags & PU_MP_NEEDS_LNO)	fprintf (f, " mp_needs_lno");
	    if (flags & PU_HAS_ALLOCA)		fprintf (f, " alloca");
	    if (flags & PU_IN_ELF_SECTION)	fprintf (f, " in_elf_section");
	    if (flags & PU_HAS_MP)		fprintf (f, " has_mp");
	    if (flags & PU_MP)			fprintf (f, " mp");
	    if (flags & PU_HAS_NAMELIST)	fprintf (f, " namelist");
	    if (flags & PU_HAS_RETURN_ADDRESS)	fprintf (f, " return_address");
	    if (flags & PU_HAS_REGION)		fprintf (f, " has_region");
	    if (flags & PU_HAS_INLINES)		fprintf (f, " has_inlines");
	    if (flags & PU_CALLS_SETJMP)	fprintf (f, " calls_setjmp");
	    if (flags & PU_CALLS_LONGJMP)	fprintf (f, " calls_longjmp");
	    if (flags & PU_IPA_ADDR_ANALYSIS)	fprintf (f, " ipa_addr");
	    if (flags & PU_SMART_ADDR_ANALYSIS)	fprintf (f, " smart_addr");
	    if (flags & PU_HAS_GLOBAL_PRAGMAS)	fprintf (f, " global_pragmas");
	    if (flags & PU_HAS_USER_ALLOCA)	fprintf (f, " user_alloca");
	    if (flags & PU_HAS_UNKNOWN_CONTROL_FLOW)	fprintf (f, " unknown_control_flow");
	    if (flags & PU_IS_THUNK)		fprintf (f, " thunk");
#ifdef KEY
	    if (flags & PU_NEEDS_MANUAL_UNWINDING) fprintf (f, " needs_manual_unwinding");
	    if (flags & PU_IS_EXTERN_INLINE) fprintf (f, " extern_inline");
	    if (flags & PU_IS_MARKED_INLINE) fprintf (f, " inline_keyword");
	    if (flags & PU_NO_INSTRUMENT) fprintf (f, " no_instrument");
	    if (flags & PU_HAS_ATTR_MALLOC) fprintf (f, " attr_malloc");
	    if (flags & PU_NEED_TRAMPOLINE) fprintf (f, " need_trampoline");
#endif
#ifdef TARG_X8664
	    if (flags & PU_FF2C_ABI) fprintf (f, " ff2c_abi");
#endif
	    if (flags & PU_IS_CDECL) fprintf (f, " cdecl");
	    if (TY_return_to_param(ty_idx))	fprintf (f, " return_to_param");
	    if (TY_is_varargs(ty_idx))		fprintf (f, " varargs");
	    if (TY_has_prototype(ty_idx))	fprintf (f, " prototype");
#ifdef TARG_X8664
	    if (TY_has_sseregister_parm(ty_idx)) fprintf (f, " sseregisterparm");
	    INT register_parms = TY_register_parm(ty_idx);
	    if (register_parms) fprintf (f, " %d-registerparm", register_parms);
            if (TY_has_stdcall(ty_idx))    fprintf (f, " stdcall");
            if (TY_has_fastcall(ty_idx))   fprintf (f, " fastcall");
#endif
	    fprintf (f, "\n");
	}
    }
	
    if (sym_class == CLASS_CONST)
	fprintf (f, "\t\tvalue: %s\n", Targ_Print (NULL, Tcon_Table[u1.tcon]));

    if (verbose) {
	// Print address
	if (base_idx != 0) {
	    const ST& base_st = St_Table[base_idx];
	    fprintf (f, "\t\tAddress: %lld(%s<%d,%d>)  ", offset,
		     ST_class (base_st) == CLASS_CONST ? "" :
		     ST_name (base_idx), ST_IDX_level (base_idx),
		     ST_IDX_index (base_idx));
	}

	if (ty_idx != 0) {
	    if (base_idx == 0 && offset == 0)
		fputs ("\t\t", f);
	    fprintf (f, "Alignment: %d bytes", TY_align (ty_idx));
	}
	fprintf (f, "\n");
	extern char *Orig_Src_File_Name, *Src_File_Name;
	fprintf (f, "\t\tlocation: file %s, line %d\n", 
	         (Orig_Src_File_Name ? Orig_Src_File_Name : Src_File_Name), line);

	fprintf (f, "\t\tFlags:\t0x%08x", flags);
	if (flags) {
	    if (flags & ST_IS_WEAK_SYMBOL)	fprintf (f, " weak");
	    if (flags & ST_IS_SPLIT_COMMON)	fprintf (f, " split_common");
	    if (flags & ST_IS_NOT_USED)		fprintf (f, " not_used");
	    if (flags & ST_IS_INITIALIZED)	fprintf (f, " initialized");
	    if (flags & ST_IS_RETURN_VAR)	fprintf (f, " return_var");
	    if (flags & ST_IS_VALUE_PARM)	fprintf (f, " value_parm");
	    if (flags & ST_PROMOTE_PARM)	fprintf (f, " promote_parm");
	    if (flags & ST_KEEP_NAME_W2F)	fprintf (f, " keep_name_w2f");
	    if (flags & ST_IS_DATAPOOL)		fprintf (f, " datapool");
	    if (flags & ST_IS_RESHAPED)		fprintf (f, " reshaped");
	    if (flags & ST_EMIT_SYMBOL)		fprintf (f, " emit_symbol");
	    if (flags & ST_HAS_NESTED_REF)	fprintf (f, " has_nested_ref");
	    if (flags & ST_INIT_VALUE_ZERO)	fprintf (f, " init_value_zero");
	    if (flags & ST_GPREL)		fprintf (f, " gprel");
	    if (flags & ST_NOT_GPREL)		fprintf (f, " not_gprel");
	    if (flags & ST_IS_NAMELIST)		fprintf (f, " namelist");
	    if (flags & ST_IS_F90_TARGET)	fprintf (f, " f90_target");
	    if (flags & ST_DECLARED_STATIC)	fprintf (f, " static");
	    if (flags & ST_IS_EQUIVALENCED)	fprintf (f, " equivalenced");
	    if (flags & ST_IS_FILL_ALIGN)	fprintf (f, " fill_align");
	    if (flags & ST_IS_OPTIONAL_ARGUMENT)fprintf (f, " optional");
	    if (flags & ST_PT_TO_UNIQUE_MEM)	fprintf (f, " pt_to_unique_mem");
	    if (flags & ST_IS_TEMP_VAR)		fprintf (f, " temp");
	    if (flags & ST_IS_CONST_VAR)	fprintf (f, " const");
	    if (flags & ST_ADDR_SAVED)		fprintf (f, " addr_saved");
	    if (flags & ST_ADDR_PASSED)		fprintf (f, " addr_passed");
	    if (flags & ST_IS_THREAD_PRIVATE)	fprintf (f, " thread_private");
	    if (flags & ST_ASSIGNED_TO_DEDICATED_PREG)
		fprintf (f, " assigned_to_dedicated_preg");
	}

#ifdef KEY
	if (flags_ext) {
	    fprintf (f, "\t\tFlags_ext:\t0x%08x", flags_ext);
	    if (flags_ext & ST_ONE_PER_PU)
		fprintf (f, " one_per_pu");
	    if (flags_ext & ST_COPY_CONSTRUCTOR_ST)
		fprintf (f, " copy_constructor_st");
            if (flags_ext & ST_INITV_IN_OTHER_ST)
                fprintf (f, " st_used_as_initialization");
            if (flags_ext & ST_IS_THREAD_LOCAL)
                fprintf (f, " thread_local");
	    if (flags_ext & ST_IS_GLOBAL_AS_LOCAL)
	        fprintf (f, " global_as_local");
            if (flags_ext & ST_IS_VTABLE)
                fprintf (f, " vtable");
	}
#endif
#ifdef TARG_NVISA
	    if (memory_space == MEMORY_GLOBAL)
		fprintf (f, " __global__");
	    if (memory_space == MEMORY_SHARED)
		fprintf (f, " __shared__");
	    if (memory_space == MEMORY_CONSTANT)
                fprintf (f, " __constant__");
	    if (memory_space == MEMORY_LOCAL)
                fprintf (f, " __local__");
	    if (memory_space == MEMORY_TEXTURE)
                fprintf (f, " __texture__");
	    if (memory_space == MEMORY_PARAM)
                fprintf (f, " __param__");
#else
        // tls-model
	if (flags_ext & ST_IS_THREAD_LOCAL) {
            switch (tls_model) {
            case TLS_NONE:
                fputs (", TLS:none", f);
                break;
            case TLS_EMULATED:
                fputs (", TLS:emulated", f);
                break;
            case TLS_GLOBAL_DYNAMIC:
                fputs (", TLS:global-dynamic", f);
                break;
            case TLS_LOCAL_DYNAMIC:
                fputs (", TLS:local-dynamic", f);
                break;
            case TLS_INITIAL_EXEC:
                fputs (", TLS:initial-exec", f);
                break;
            case TLS_LOCAL_EXEC:
                fputs (", TLS:local-exec", f);
                break;
            }
        }
#endif /* TARG_NVISA */

	switch (export_class) {

	case EXPORT_LOCAL:
	    fputs (", XLOCAL", f);
	    break;

	case EXPORT_LOCAL_INTERNAL:
	    fputs (", XLOCAL(INTERNAL)", f);
	    break;

	case EXPORT_INTERNAL:
	    fputs (", XINTERNAL", f);
	    break;

	case EXPORT_HIDDEN:
	    fputs (", XHIDDEN", f);
	    break;

	case EXPORT_PROTECTED:
	    fputs (", XPROTECTED", f);
	    break;

	case EXPORT_PREEMPTIBLE:
	    fputs (", XPREEMPTIBLE", f);
	    break;

	case EXPORT_OPTIONAL:
	    fputs (", XOPTIONAL", f);
	    break;

	default:
	    fputs (", Export class unknown", f);
	    break;
	}

	fprintf (f, "\n\t\tSclass: %s\n", Sclass_Name (storage_class));
	if(vtable_ty_idx)
	{
	    fprintf (f, "\t\tVtable for type: %s\n", TY_name (vtable_ty_idx));
	}
    }
} // ST::Print

std::ostream& operator<<(std::ostream &os, const ST &st )
{
    BOOL verbose = TRUE;

    const char *name_str = (st.sym_class == CLASS_CONST) ?
	"<constant>" : &Str_Table[st.u1.name_idx];

    int level = ST_IDX_level(st.st_idx);
    os << name_str << " \t<" << level << "," 
       << ST_IDX_index (st.st_idx) << "> ";

    if (strlen (name_str) > 20)
        os << "\t\t" << std::endl;

    TY_IDX ty_idx = 0;

    switch (st.sym_class) {

    case CLASS_UNK:
        os << "Class unknown";
	break;

    case CLASS_VAR:
        os << "Variable";
	ty_idx = st.u2.type;
	break;

    case CLASS_FUNC:
        os << "Subprogram";
	ty_idx = PU_prototype (Pu_Table[st.u2.pu]);
	break;

    case CLASS_CONST:
        os << "Constant";
	ty_idx = st.u2.type;
	break;

    case CLASS_PREG:
        os << "Pseudo-Register";
	ty_idx = st.u2.type;
	break;

    case CLASS_BLOCK:
        os << "Block" << " (#" << st.u2.blk << ")";
	break;

    case CLASS_NAME:
        os << "Name-only";
	break;
	
    default:
        os << "INVALID CLASS (" << st.sym_class << ")";
	break;
    }

    const TY& ty = Ty_Table [ty_idx];

    if (ty_idx != 0) {

	name_str = TY_name_idx (ty) == 0 ? NULL : TY_name (ty);

	if (!(st.sym_class == CLASS_FUNC) || name_str != NULL) {
            os << " of type ";

	    Print_type_attributes (os, ty_idx);

            os << (name_str ? name_str : "(anon)");
	    
	    const TY *pty = &ty;
	    INT pcount = 0;
	    while (TY_kind (*pty) == KIND_POINTER) {
		pty = &Ty_Table[TY_pointed (*pty)];
		++pcount;
	    }
	    
	    if (verbose) {
		name_str = TY_kind_name (*pty);
                os << " (#" << TY_IDX_index (ty_idx) << ", " << name_str;
		while (pcount-- > 0)
                    os << '*';
                os << ')';
	    } else
                os << " (#" << TY_IDX_index (ty_idx) << ")";
	}
    }

    if (!verbose) {
	/* quick address */
        os << " @ 0x" << std::hex << st.offset << std::dec;
	if (st.base_idx != 0)
            os << "(" << ST_name (st.base_idx) << ")";
    }
    os << std::endl;
    

    if (st.sym_class == CLASS_FUNC && verbose) {
	/* Give info about the type being returned, which is different
	 * than the type of the function.
	 */
	if (ty_idx != 0 && TY_tylist (ty) != 0) {
	    TY_IDX rettype_idx = Tylist_Table[TY_tylist (ty)];
	    const TY& rettype = Ty_Table[rettype_idx];
            os << "\t\tReturning ";
	    Print_type_attributes (os, rettype_idx);
            os << TY_name(rettype);
	    name_str = TY_kind_name (rettype);
            os << " (#" << TY_IDX_index (rettype_idx) << ", " << name_str;

            os << "PU[" << st.u2.pu << "] ";

	    if (Pu_Table[st.u2.pu].src_lang & PU_C_LANG)	os <<  "C  ";
	    if (Pu_Table[st.u2.pu].src_lang & PU_CXX_LANG)	os << "C++  ";
	    if (Pu_Table[st.u2.pu].src_lang & PU_F77_LANG)	os << "F77  ";
	    if (Pu_Table[st.u2.pu].src_lang & PU_F90_LANG)	os << "F90  ";

	    mUINT64 flags = Pu_Table[st.u2.pu].flags;
            os << "flags:";
	    if (flags & PU_IS_PURE)		os << " pure";
	    if (flags & PU_NO_SIDE_EFFECTS)	os << " no_side_effects";
	    if (flags & PU_IS_INLINE_FUNCTION)	os << " inline";
	    if (flags & PU_NO_INLINE)		os << " no_inline";
	    if (flags & PU_MUST_INLINE)		os << " must_inline";
	    if (flags & PU_NO_DELETE)		os << " no_delete";
	    if (flags & PU_HAS_EXC_SCOPES)	os << " exc_scopes";
	    if (flags & PU_IS_NESTED_FUNC)	os << " nested_func";
	    if (flags & PU_HAS_NON_MANGLED_CALL)os << " non_mangled_call";
	    if (flags & PU_ARGS_ALIASED)	os << " args_aliased";
	    if (flags & PU_NEEDS_FILL_ALIGN_LOWERING)os << " fill_align";
	    if (flags & PU_NEEDS_T9)		os << " t9";
	    if (flags & PU_HAS_VERY_HIGH_WHIRL)	os << " very_high_whirl";
	    if (flags & PU_HAS_ALTENTRY)	os << " altentry";
	    if (flags & PU_RECURSIVE)		os << " recursive";
	    if (flags & PU_IS_MAINPU)		os << " main";
	    if (flags & PU_UPLEVEL)		os << " uplevel";
	    if (flags & PU_MP_NEEDS_LNO)	os << " mp_needs_lno";
	    if (flags & PU_HAS_ALLOCA)		os << " alloca";
	    if (flags & PU_IN_ELF_SECTION)	os << " in_elf_section";
	    if (flags & PU_HAS_MP)		os << " has_mp";
	    if (flags & PU_MP)			os << " mp";
	    if (flags & PU_HAS_NAMELIST)	os << " namelist";
	    if (flags & PU_HAS_RETURN_ADDRESS)	os << " return_address";
	    if (flags & PU_HAS_REGION)		os << " has_region";
	    if (flags & PU_HAS_INLINES)		os << " has_inlines";
	    if (flags & PU_CALLS_SETJMP)	os << " calls_setjmp";
	    if (flags & PU_CALLS_LONGJMP)	os << " calls_longjmp";
	    if (flags & PU_IPA_ADDR_ANALYSIS)	os << " ipa_addr";
	    if (flags & PU_SMART_ADDR_ANALYSIS)	os << " smart_addr";
	    if (flags & PU_HAS_GLOBAL_PRAGMAS)	os << " global_pragmas";
	    if (flags & PU_HAS_USER_ALLOCA)	os << " user_alloca";
	    if (flags & PU_HAS_UNKNOWN_CONTROL_FLOW)	os << " unknown_control_flow";
	    if (flags & PU_IS_THUNK)		os << " thunk";
#ifdef KEY
	    if (flags & PU_NEEDS_MANUAL_UNWINDING) os << " needs_manual_unwinding";
	    if (flags & PU_IS_EXTERN_INLINE) os << " extern_inline";
	    if (flags & PU_IS_MARKED_INLINE) os << " inline_keyword";
	    if (flags & PU_NO_INSTRUMENT) os << " no_instrument";
	    if (flags & PU_NEED_TRAMPOLINE) os << " need_trampoline";
#endif
#ifdef TARG_X8664
	    if (flags & PU_FF2C_ABI) os << " ff2c_abi";
#endif
	    if (flags & PU_IS_CDECL) os << " cdecl";
	    if (TY_return_to_param(ty_idx))	os << " return_to_param";
	    if (TY_is_varargs(ty_idx))		os << " varargs";
	    if (TY_has_prototype(ty_idx))	os << " prototype";
#ifdef TARG_X8664
	    if (TY_has_sseregister_parm(ty_idx)) os << " sseregisterparm";
	    INT register_parms = TY_register_parm(ty_idx);
	    if (register_parms) os << " " << register_parms << "-registerparm";
            if (TY_has_stdcall(ty_idx))    os << " stdcall";
            if (TY_has_fastcall(ty_idx))   os << " fastcall";
#endif
	    os << std::endl;
	}
    }
	
    if (st.sym_class == CLASS_CONST)
        os << "\t\tvalue: " 
           << Targ_Print (NULL, Tcon_Table[st.u1.tcon]) << std::endl;

    if (verbose) {
	// Print address
	if (st.base_idx != 0) {
	    const ST& base_st = St_Table[st.base_idx];
            int level = ST_IDX_level(st.base_idx);
            os << "\t\tAddress: " << st.offset << "("
               << (ST_class (base_st) == CLASS_CONST ? ""
                                                     : ST_name(st.base_idx))
               << "<" << level << "," 
               << ST_IDX_index (st.base_idx) << ">) ";
	}

	if (ty_idx != 0) {
	    if (st.base_idx == 0 && st.offset == 0)
                os << "\t\t";
            os << "Alignment: " <<  TY_align (ty_idx) << "bytes";

	}
        os << std::endl;

	mUINT64 flags = st.flags;
        os << "\t\tFlags:\t0x" << std::hex << flags << std::dec;
	if (flags) {
	    if (flags & ST_IS_WEAK_SYMBOL)	os << " weak";
	    if (flags & ST_IS_SPLIT_COMMON)	os << " split_common";
	    if (flags & ST_IS_NOT_USED)		os << " not_used";
	    if (flags & ST_IS_INITIALIZED)	os << " initialized";
	    if (flags & ST_IS_RETURN_VAR)	os << " return_var";
	    if (flags & ST_IS_VALUE_PARM)	os << " value_parm";
	    if (flags & ST_PROMOTE_PARM)	os << " promote_parm";
	    if (flags & ST_KEEP_NAME_W2F)	os << " keep_name_w2f";
	    if (flags & ST_IS_DATAPOOL)		os << " datapool";
	    if (flags & ST_IS_RESHAPED)		os << " reshaped";
	    if (flags & ST_EMIT_SYMBOL)		os << " emit_symbol";
	    if (flags & ST_HAS_NESTED_REF)	os << " has_nested_ref";
	    if (flags & ST_INIT_VALUE_ZERO)	os << " init_value_zero";
	    if (flags & ST_GPREL)		os << " gprel";
	    if (flags & ST_NOT_GPREL)		os << " not_gprel";
	    if (flags & ST_IS_NAMELIST)		os << " namelist";
	    if (flags & ST_IS_F90_TARGET)	os << " f90_target";
	    if (flags & ST_DECLARED_STATIC)	os << " static";
	    if (flags & ST_IS_EQUIVALENCED)	os << " equivalenced";
	    if (flags & ST_IS_FILL_ALIGN)	os << " fill_align";
	    if (flags & ST_IS_OPTIONAL_ARGUMENT)os << " optional";
	    if (flags & ST_PT_TO_UNIQUE_MEM)	os << " pt_to_unique_mem";
	    if (flags & ST_IS_TEMP_VAR)		os << " temp";
	    if (flags & ST_IS_CONST_VAR)	os << " const";
	    if (flags & ST_ADDR_SAVED)		os << " addr_saved";
	    if (flags & ST_ADDR_PASSED)		os << " addr_passed";
	    if (flags & ST_IS_THREAD_PRIVATE)	os << " thread_private";
	    if (flags & ST_ASSIGNED_TO_DEDICATED_PREG)
		os << " assigned_to_dedicated_preg";
	}

#ifdef KEY
	mUINT64 flags_ext = st.flags_ext;
	if (flags_ext) {
            os << "\t\tFlags_ext:\t0x" << std::hex << flags_ext << std::dec;
	    if (flags_ext & ST_ONE_PER_PU)
		os << " one_per_pu";
	    if (flags_ext & ST_COPY_CONSTRUCTOR_ST)
		os << " copy_constructor_st";
            if (flags_ext & ST_INITV_IN_OTHER_ST)
                os << " st_used_as_initialization";
            if (flags_ext & ST_IS_THREAD_LOCAL)
                os << " thread_local";
	}
#endif
#ifdef TARG_NVISA
	    if (memory_space == MEMORY_GLOBAL)
		os << " __global__";
	    if (memory_space == MEMORY_SHARED)
		os << " __shared__";
	    if (memory_space == MEMORY_CONSTANT)
                os << " __constant__";
	    if (memory_space == MEMORY_LOCAL)
                os << " __local__";
	    if (memory_space == MEMORY_TEXTURE)
                os << " __texture__";
	    if (memory_space == MEMORY_PARAM)
                os << " __param__";
#else
        // tls-model
	if (flags_ext & ST_IS_THREAD_LOCAL) {
            switch (st.tls_model) {
            case TLS_NONE:
                os << ", TLS:none";
                break;
            case TLS_EMULATED:
                os << ", TLS:emulated";
                break;
            case TLS_GLOBAL_DYNAMIC:
                os << ", TLS:global-dynamic";
                break;
            case TLS_LOCAL_DYNAMIC:
                os << ", TLS:local-dynamic";
                break;
            case TLS_INITIAL_EXEC:
                os << ", TLS:initial-exec";
                break;
            case TLS_LOCAL_EXEC:
                os << ", TLS:local-exec";
                break;
            }
        }
#endif /* TARG_NVISA */

	switch (st.export_class) {

	case EXPORT_LOCAL:
	    os << ", XLOCAL";
	    break;

	case EXPORT_LOCAL_INTERNAL:
	    os << ", XLOCAL(INTERNAL)";
	    break;

	case EXPORT_INTERNAL:
	    os << ", XINTERNAL";
	    break;

	case EXPORT_HIDDEN:
	    os << ", XHIDDEN";
	    break;

	case EXPORT_PROTECTED:
	    os << ", XPROTECTED";
	    break;

	case EXPORT_PREEMPTIBLE:
	    os << ", XPREEMPTIBLE";
	    break;

	case EXPORT_OPTIONAL:
	    os << ", XOPTIONAL";
	    break;

	default:
	    os << ", Export class unknown";
	    break;
	}

        os << std::endl << "\t\tSclass: "
           << Sclass_Name(st.storage_class) << std::endl;
    }
} // ST::Print

void
FLD::Print (FILE *f) const
{
    fprintf (f, "\t%6lld  %-8s\t", ofst, &Str_Table[name_idx]);
    Print_TY_IDX_verbose (f, type);
    fprintf (f, "\n\t\tfl:0x%04x", flags);
    if (flags) {
	if (flags & FLD_LAST_FIELD)	fprintf (f, " last_field");
	if (flags & FLD_EQUIVALENCE)	fprintf (f, " equivalence");
	if (flags & FLD_BEGIN_UNION)	fprintf (f, " begin_union");
	if (flags & FLD_END_UNION)	fprintf (f, " end_union");
	if (flags & FLD_BEGIN_MAP)	fprintf (f, " begin_map");
	if (flags & FLD_END_MAP)	fprintf (f, " end_map");
	if (flags & FLD_IS_BIT_FIELD)	fprintf (f, " bit_field");
    }
    if (st != 0)
	fprintf (f, " st (%d,%d)", ST_IDX_level (st), ST_IDX_index (st));
    if (flags & FLD_IS_BIT_FIELD)
	fprintf (f, " bit field size:%d ofst:%d", bsize, bofst);
    fputc ('\n', f);
} // FLD::Print


void
ARB::Print (FILE *f) const
{
    if (flags & ARB_CONST_LBND)
	fprintf (f, "%lld:", Lbnd_val ());
    else
	fprintf (f, "st(%d):", ST_IDX_index (Lbnd_var ()));

    if (flags & ARB_CONST_UBND)
	fprintf (f, "%lld:", Ubnd_val ());
    else
	fprintf (f, "st(%d):", ST_IDX_index (Ubnd_var ()));

    if (flags & ARB_CONST_STRIDE)
	fprintf (f, "%lld:", Stride_val ());
    else
	fprintf (f, "st(%d):", ST_IDX_index (Stride_var ()));

} // ARB::Print


void
TY::Print (FILE *f) const
{
    fprintf (f, "%-14s:",
	     name_idx ? &Str_Table[name_idx] : "(anon)");

    fprintf (f, " (f: 0x%04x", flags);
    if (flags) {
	if (flags & TY_IS_CHARACTER)	fprintf (f, " character");
	if (flags & TY_IS_LOGICAL)	fprintf (f, " logical");
	if (flags & TY_IS_UNION)	fprintf (f, " union");
	if (flags & TY_IS_PACKED)	fprintf (f, " packed");
	if (flags & TY_PTR_AS_ARRAY)	fprintf (f, " ptr_as_array");
	if (flags & TY_ANONYMOUS)	fprintf (f, " anonymous");
	if (flags & TY_SPLIT)		fprintf (f, " split");
	if (flags & TY_IS_F90_POINTER)	fprintf (f, " f90_pointer");
	if (flags & TY_NOT_IN_UNION)	fprintf (f, " not_in_union");
	if (flags & TY_NO_ANSI_ALIAS)	fprintf (f, " no_ansi_alias");
	if (flags & TY_IS_NON_POD)	fprintf (f, " non_pod");
#ifdef KEY
	if (flags & TY_RETURN_IN_MEM)	fprintf (f, " return_in_mem");
	if (flags & TY_CONTENT_SEEN)	fprintf (f, " content_seen");
        if (flags & TY_IS_INCOMPLETE)   fprintf (f, " incomplete");
	if (flags & TY_NO_SPLIT)        fprintf (f, " no_split");
#endif
#ifdef TARG_NVISA
	if (flags & TY_CAN_BE_VECTOR) 	fprintf (f, "  vector");
#endif
    }
    fprintf (f, ")");

    fprintf (f, " size %lld %s: ",
	     size, (mtype != 0) ? Mtype_Name (mtype) : ""); 

    switch (kind) {
    case KIND_SCALAR:
	fprintf (f, "SCALAR (%s)", Mtype_Name (mtype));
	fputc ('\n', f);
	break;

    case KIND_ARRAY:
	fputs ("ARRAY of ", f);
	Print_TY_IDX_verbose (f, Etype ());
	if (Arb () != 0) {
	   ARB_HANDLE arb(Arb());
	   INT i,ndim;
	   
	   ndim = ARB_dimension(arb);
	   for (i = 0; i < ndim; i++) {
	      fputs (" (", f);
	      (*arb[i].Entry()).Print (f);
	      fputc (')', f);
	      // possible early exit for broken tables
	      if (ARB_last_dimen(arb[i])) break;
	   }
	}
	fputc ('\n', f);
	break;
	    
    case KIND_STRUCT:
	if (TY_is_packed (*this))
	    fputs ("packed ", f);
	fputs (kind == KIND_STRUCT ? "STRUCT\n": "CLASS\n", f);
	if (Fld () != 0) {
	    FLD_ITER iter = Make_fld_iter (FLD_HANDLE (Fld ()));
	    do {
		(*iter).Print (f);
	    } while (! FLD_last_field (iter++));
	} else
	    fputc ('\n', f);
	if (TY_vtable(*this))
	    fprintf(f, "VTABLE: %s\n", ST_name(TY_vtable(*this)));
	break;

    case KIND_POINTER:
	fputs ("-> ", f);
	Print_TY_IDX_verbose (f, Pointed ());
	fputc ('\n', f);
	break;

    case KIND_FUNCTION:
	fprintf (f, "FUNCTION (f: 0x%04x)\n", Pu_flags());
	{
	    TYLIST_IDX idx = Tylist ();
	    fprintf (f, "\treturns ");
	    Print_TY_IDX_verbose (f, Tylist_Table[idx]);
    	    if (Tylist_Table[idx] == 0) {
	    	fputc ('\n', f);
		break;
	    }
	    ++idx;
	    while (Tylist_Table[idx] != 0) {
		fputs ("\n\tparameter ", f);
		Print_TY_IDX_verbose (f, Tylist_Table[idx]);
		++idx;
	    }
	    fputc ('\n', f);
	}
	break;

    case KIND_VOID:
	fputs ("VOID", f);
	fputc ('\n', f);
	break;

    default:
	fprintf (f, "Unknown type (%d)", kind);
	fputc ('\n', f);
	break;
    }
} // TY::Print


void
PU::Print (FILE *f) const
{
    Print_TY_IDX_verbose (f, prototype);
#ifdef KEY
    fprintf (f, ", flags 0x%016llx,\n"
	     "\tlexical level %d, LANG 0x%02x, TARGET_INFO %d,\n"
	     "\tMisc. Info (misc) %d\n",
	     flags, lexical_level, src_lang, target_idx, (INT32)misc); 
#else
    fprintf (f, ", flags 0x%016llx,\n"
	     "\tlexical level %d, LANG 0x%02x, TARGET_INFO %d\n",
	     flags, lexical_level, src_lang, target_idx); 
#endif
} // PU::Print

void
BLK::Print (FILE *f) const
{
    fprintf (f, "size %lld, align %d, flags 0x%04x, section %d, scninfo %d\n",
		size, align, flags, section_idx, scninfo_idx);
} // BLK::Print


void
LABEL::Print (FILE *f) const
{
    const char *name_str = name_idx ? &Str_Table[name_idx] : "(anon)";

    fprintf (f, "%s: kind = 0x%08x fl = 0x%08x", name_str, kind, flags);
    if (flags & LABEL_TARGET_OF_GOTO_OUTER_BLOCK)
      fprintf (f, " target_of_goto_outer_block"); 
    if (flags & LABEL_ADDR_SAVED)  fprintf (f, " addr_saved");
    if (flags & LABEL_ADDR_PASSED) fprintf (f, " addr_passed");
    fprintf (f, "\n");
} // LABEL::Print


void
PREG::Print (FILE *f) const
{
    const char *name_str = name_idx ? &Str_Table[name_idx] : "(anon)";

    fprintf (f, "%s\n", name_str);
} // PREG::Print


void
ST_ATTR::Print (FILE* f) const
{
    fprintf (f, "0x%x (%s) --> ", st_idx, ST_name (st_idx));
    switch (kind) {
    case ST_ATTR_UNKNOWN:
	fprintf (f, "(NOT USED)\n");
	break;
    case ST_ATTR_DEDICATED_REGISTER:
	fprintf (f, "(DREG) %d\n", u.reg_id);
	break;
    case ST_ATTR_SECTION_NAME:
	fprintf (f, "(SECTION) %s\n", Index_To_Str (u.section_name));
	break;
    default:
	fprintf (f, "(UNKNOWN) 0x%x\n", u.value);
	break;
    }
}

void
FILE_INFO::Print (FILE *f) const
{
    fprintf (f, "gp_group: %d, flags: 0x%08x", gp_group, flags);
    if (flags) {
	if (flags & FI_IPA)		fputs (" IPA-generated", f);
	if (flags & FI_NEEDS_LNO)	fputs (" needs_LNO", f);
	if (flags & FI_HAS_INLINES)	fputs (" has_inlines", f);
	if (flags & FI_HAS_MP)		fputs (" has_mp", f);
    }

    fputs ("\n", f);

} // FILE_INFO::Print
    


struct clear_addr_flag_op
{
    clear_addr_flag_op() {};

    void operator () (UINT idx, ST *entry) const {
      if( (ST_sclass(*entry) == SCLASS_AUTO) && ST_addr_saved(*entry))
        Clear_ST_addr_saved(*entry); 
    } ;
}; // clear_addr_flag_op  

void
Clear_local_symtab_addr_flags(const SCOPE& scope)
{
  For_all_entries (*scope.st_tab, clear_addr_flag_op(), 1);
}

// function object used in "For_all"
template <class T>
struct print_op
{
    FILE *fid;

    print_op (FILE *f) : fid (f) {}

    void operator () (UINT idx, T *entry) const;
}; // print_op


template <class T>
inline void
print_op<T>::operator () (UINT idx, T *entry) const {
	fprintf (fid, "[%d]: ", idx);
	entry->Print (fid);
}


// specialization for printing TCONs
template<>
inline void
print_op<TCON>::operator () (UINT idx, TCON *c) const
{
    fprintf (fid, "[%d] %s: %s\n", idx, MTYPE_name(TCON_ty(*c)),Targ_Print (NULL, *c));
} // print_op<TCON>::operator ()


// Print all local symbol tables corresponding to a PU
void
Print_local_symtab (FILE *f, const SCOPE& scope)
{
    // Print the function name

    fprintf (f, "\n%sSYMTAB for %s: level %d, st %d, label %d, preg %d,"
 	     " inito %d, st_attr %d\n%s\n", DBar, ST_name (scope.st),
	     PU_lexical_level (scope.st), scope.st_tab->Size () - 1,
	     scope.label_tab->Size () - 1, scope.preg_tab->Size () - 1,
	     scope.inito_tab->Size () - 1, scope.st_attr_tab->Size () - 1, DBar);

    fputs ("Symbols:\n", f);
    For_all_entries (*scope.st_tab, print_op<ST> (f), 1);

    fprintf (f, "%sLabels:\n", DBar);
    For_all_entries (*scope.label_tab, print_op<LABEL> (f), 1);
    
    fprintf (f, "%sPseudo-registers:\n", DBar);
    For_all_entries (*scope.preg_tab, print_op<PREG> (f), 1);
    
    fprintf (f, "%sINITOs:\n", DBar);
    For_all_entries (*scope.inito_tab, print_op<INITO> (f), 1);

    fprintf (f, "%sST_ATTRs:\n", DBar);
    For_all_entries (*scope.st_attr_tab, print_op<ST_ATTR> (f), 1);

    fprintf (f, "%s\n", DBar);
    
} // Print_local_symtab


void
Print_global_symtab (FILE *f)
{
    fprintf(f,"%sGLOBAL SYMTAB:\n", DBar);

    fprintf (f, "%sFile Info:\n", DBar);
    File_info.Print (f);

    fprintf (f, "%sSymbols:\n", DBar);
    For_all (St_Table, GLOBAL_SYMTAB, print_op<ST> (f));

    fprintf (f, "%sPUs:\n", DBar);
    For_all (Pu_Table, print_op<PU> (f));

    fprintf (f, "%sTypes:\n", DBar);
    For_all (Ty_Table, print_op<TY> (f));

    fprintf (f, "%sBlocks:\n", DBar);
    For_all (Blk_Table, print_op<BLK> (f));

    fprintf (f, "%sTcons:\n", DBar);
    For_all (Tcon_Table, print_op<TCON> (f));

    fprintf (f, "%sINITOs:\n", DBar);
    For_all (Inito_Table, GLOBAL_SYMTAB, print_op<INITO> (f));

    fprintf (f, "%sST_ATTRs:\n", DBar);
    For_all (St_Attr_Table, GLOBAL_SYMTAB, print_op<ST_ATTR> (f));

    fprintf (f, "%sString table size = %lld\n", DBar, STR_Table_Size());
    fprintf (f, "%s\n", DBar);
} // Print_global_symtab

// for ease of debugging, because I don't know how to call Print
// routines from dbx, add simple dump routines.
void
dump_st (ST *st)
{
	st->Print(stdout);
    	switch (st->sym_class) {
	case CLASS_BLOCK:
		Blk_Table[st->u2.blk].Print (stdout);
		break;
	case CLASS_FUNC:
		Pu_Table[st->u2.pu].Print (stdout);
		break;
	}
}

void
dump_st(ST_IDX st_idx)
{
   ST *st = &St_Table[st_idx];
   dump_st(st);
}

// Since dbx cannot handle overloaded functions...
void 
dump_st_idx (ST_IDX st) 
{
  dump_st(st);
}


void
dump_ty (TY_IDX ty_idx)
{
	TY& ty = Ty_Table[ty_idx];
	ty.Print(stdout);
}

// Since dbx cannot handle overloaded function...
void
dump_ty_idx (TY_IDX ty_idx)
{
  dump_ty (ty_idx);
}

void
dump_ty(const TY &ty)
{
  ty.Print(stdout);
}

void 
dump_label (LABEL_IDX idx)
{
   Label_Table[idx].Print(stdout);
}

void 
dump_inito(INITO_IDX idx)
{
   Inito_Table[idx].Print(stdout);
}

void
dump_st_attr (ST_ATTR_IDX idx)
{
    St_Attr_Table[idx].Print(stdout);
}

#ifndef KEY
static
#endif // !KEY
ST *
Gen_Temp_Named_Symbol (TY_IDX ty, const char *rootname,
		       ST_CLASS sym_class, ST_SCLASS storage_class)
{
  static INT Temp_Index = 0;
  ST *st = New_ST(CURRENT_SYMTAB);
  STR_IDX str_idx = Save_Str2i(
    // put _temp first so don't need knowledge of legal prefix in Gen_Temp call
    (Temp_Symbol_Prefix), 
    rootname, Temp_Index++);
  ST_Init(st, str_idx, sym_class, storage_class, EXPORT_LOCAL, ty);
  return st;
}

// Create stack symbol for temp var.
ST *
Gen_Temp_Symbol (TY_IDX      ty,	// type of the desired symbol
		 const char *rootname)	// root of the name to use
{
  ST *st = Gen_Temp_Named_Symbol(ty, rootname, CLASS_VAR, SCLASS_AUTO);
  Set_ST_is_temp_var(st);
  return st;
}

// Create a read-only file-level static symbol of the given type in
// the current symbol table.
ST *
Gen_Read_Only_Symbol(TY_IDX ty, const char *rootname)
{
  ST *st = Gen_Temp_Named_Symbol(ty, rootname, CLASS_VAR, SCLASS_FSTATIC);
  Set_ST_is_const_var(st);
  return st;
}

//----------------------------------------------------------------------
//----------------------------------------------------------------------

TY_IDX
Promoted_Parm_Type(const ST *formal_parm)
{
  TY_IDX ptype = ST_type(formal_parm);
  TY_IDX newtype;
  BOOL parm_float;

  if ( ! ST_promote_parm(formal_parm) ) return ptype;

  /* Otherwise, figure out the conversion: */
  ptype = ST_type(formal_parm);
  parm_float = ST_is_value_parm(formal_parm) &&
		(TY_kind(ptype) == KIND_SCALAR) &&
		(MTYPE_float(TY_mtype(ptype)));
  if ( parm_float ) {
    /* Must be a promotion from float to double: */
    newtype = MTYPE_To_TY(MTYPE_F8);
  } else {
    /* Must be a promotion to int: */
    newtype = MTYPE_To_TY(Integer_type);
  }

  /* For sanity, assert that we don't demote: */
  Is_True(TY_size(newtype) >= TY_size(ptype),
	  ("Nonsensical demotion of parameter type"));
  return newtype;
}

//----------------------------------------------------------------------
// Initialization routines
//----------------------------------------------------------------------

// for fast conversion of predefined types and preg.
ST *MTYPE_TO_PREG_array[MTYPE_LAST+1];

ST *Int_Preg, *Float_Preg, *Return_Val_Preg;
// bug fix for OSP_87
ST *Branch_Preg;
#ifdef TARG_X8664
ST* X87_Preg = NULL;
#endif

TY_IDX MTYPE_TO_TY_array[MTYPE_LAST+1];

TY_IDX Quad_Type, Void_Type, FE_int_Type, FE_double_Type;
TY_IDX Spill_Int_Type, Spill_Float_Type;
#ifdef KEY
TY_IDX Spill_Int32_Type;
TY_IDX Spill_Float32_Type;
#endif

#if defined(FRONT_END) && !defined(FRONT_END_MFEF77)
extern "C" TYPE_ID FE_int_To_Mtype (void);
#endif

static INT Max_scope = 8;

// Set reserve_index_zero to FALSE when reading the symtab from a file
void
New_Scope (SYMTAB_IDX level, MEM_POOL *pool, BOOL reserve_index_zero)
{
    if (level >= Max_scope) {
	UINT size = Max_scope * sizeof(SCOPE);
	Max_scope *= 2;
	Scope_tab = (SCOPE *) MEM_POOL_Realloc (Malloc_Mem_Pool, Scope_tab,
						size, size * 2);
    }

    Current_scope = level;

    ST_TAB *st_tab = CXX_NEW (ST_TAB (pool), pool);
    INITO_TAB *inito_tab = CXX_NEW (INITO_TAB (pool), pool);
    ST_ATTR_TAB *st_attr_tab = CXX_NEW (ST_ATTR_TAB (pool), pool);
    LABEL_TAB *label_tab = NULL;
    PREG_TAB *preg_tab = NULL;

    UINT32 dummy_idx;
    if (reserve_index_zero) {
	st_tab->New_entry ((ST_IDX&) dummy_idx);
	inito_tab->New_entry ((INITO_IDX&) dummy_idx);
	st_attr_tab->New_entry ((ST_ATTR_IDX&) dummy_idx);
    }
    
    if (level > GLOBAL_SYMTAB) {
	label_tab = CXX_NEW (LABEL_TAB (pool), pool);
	preg_tab = CXX_NEW (PREG_TAB (pool), pool);

	if (reserve_index_zero) {
	    label_tab->New_entry ((LABEL_IDX&) dummy_idx);
	    preg_tab->New_entry ((PREG_IDX&) dummy_idx);
	}
    
    }
	

    // Allocate tables for STs, LABELS, PREGs and INITOs
    Scope_tab[level].Init (st_tab, label_tab, preg_tab, inito_tab,
			   st_attr_tab, pool);

} // New_Scope


void
Delete_Scope (SYMTAB_IDX level)
{
    SCOPE& scope = Scope_tab[level];

    CXX_DELETE (scope.st_tab, scope.pool);
    CXX_DELETE (scope.label_tab, scope.pool);
    CXX_DELETE (scope.preg_tab, scope.pool);
    CXX_DELETE (scope.inito_tab, scope.pool);
    CXX_DELETE (scope.st_attr_tab, scope.pool);
    
} // Delete_Scope


static void
Setup_Preg_Pointers ()
{
    for (TYPE_ID i = MTYPE_FIRST; i <= MTYPE_LAST; ++i) {
	if (MTYPE_To_PREG (i) != NULL || MTYPE_To_TY (i) == 0)
	    continue;
	Is_True (MTYPE_byte_size (i) < 4, ("Invalid mtype"));
	MTYPE_To_PREG (i) = MTYPE_signed (i) ? MTYPE_To_PREG (MTYPE_I4) :
	    MTYPE_To_PREG (MTYPE_U4);
    }
    
    if (ST_type(Int64_Preg) == Spill_Int_Type)
	Int_Preg = Int64_Preg;
    else
	Int_Preg = Int32_Preg;
    if (ST_type(Float32_Preg) == Spill_Float_Type)
	Float_Preg = Float32_Preg;
    else
	Float_Preg = Float64_Preg;

    // bug fix for OSP_87
    Branch_Preg = MTYPE_To_PREG(MTYPE_A8);
    
#ifdef TARG_X8664
    X87_Preg = MTYPE_To_PREG( MTYPE_F10 );
#endif
} // Setup_Preg_Pointers


static void
Create_All_Preg_Symbols ()
{
    for (TYPE_ID i = MTYPE_FIRST; i <= MTYPE_LAST; ++i) {
	if (MTYPE_To_TY (i) == 0)
	    continue;
	if (MTYPE_To_PREG (i) != NULL)
	    continue;
	if (MTYPE_byte_size(i) < 4) {
#ifdef TARG_X8664 
            // Bugs 482, 505, 626
	    // we will allow all forms of preg_ for the integer type.
	    // This is done to accommodate the inline asm syntaxes in x86/x86-64
	    if ( i != MTYPE_B && !MTYPE_is_integral(i) ) continue;
#else
	    // special case:  allow mtype_B
	    if (i != MTYPE_B) continue;
#endif
	}

	ST *st = New_ST (GLOBAL_SYMTAB);
	ST_Init (st, Save_Str2 (".preg_", MTYPE_name(i)),
		 CLASS_PREG, SCLASS_REG, EXPORT_LOCAL, MTYPE_To_TY (i));
	MTYPE_To_PREG(i) = st;
    }

    if (Return_Val_Preg == NULL) {

	ST *st = New_ST (GLOBAL_SYMTAB);
	ST_Init (st, Save_Str (".preg_return_val"),
		 CLASS_PREG, SCLASS_REG, EXPORT_LOCAL, 0);
	Return_Val_Preg = st;
    }

    Setup_Preg_Pointers ();
} // Create_All_Preg_Symbols


// for phases other then the front end, we read in the global symtab from
// file, so we don't create the preg sybmols.  But we still need to intialize
// the MTYPE_TO_PREG_array.
static void
Set_up_all_preg_symbols ()
{
    UINT32 idx = 1;
    
    UINT32 last = MIN (MTYPE_LAST + 1, ST_Table_Size (GLOBAL_SYMTAB));

    for (INT i = MTYPE_FIRST; i < last; ++i) {
	ST_IDX st_idx = make_ST_IDX (idx++, GLOBAL_SYMTAB);
	ST& st = St_Table[st_idx];
	if (ST_sym_class (st) != CLASS_PREG)
	    break;			// ASSUME all predefined pregs are
					// placed before all other symbols

	if (Return_Val_Preg == NULL) {
	    if (strcmp (ST_name (&st), ".preg_return_val") == 0) {
		Return_Val_Preg = &st;
		continue;
	    }
	}

	TY_IDX ty_idx = ST_type (st);
	const TY& ty = Ty_Table[ty_idx];
	TYPE_ID mtype = TY_mtype (ty);
	if (ty_idx != MTYPE_To_TY (mtype))
	    continue;
	MTYPE_To_PREG (mtype) = &st;
    }

    // create any missing preg
    Create_All_Preg_Symbols ();

} // Set_up_all_preg_symbols
    
/* ====================================================================
 *
 * Gen_Predef_Type_Name
 *
 * Just allocate and create a predefined type name
 *
 * ====================================================================
 */
static inline STR_IDX
Gen_predef_type_name (const char *basename)
{
    return Save_Str2 (".predef_", basename);
}


/* ====================================================================
 *
 * Create_Special_Global_Symbols
 *
 * Create special global symbols and types.  These are created before
 * the per-file initialization, because they are used for various
 * global purposes like initializing the segment descriptors.
 *
 * This should only be called once per execution.
 *
 * ====================================================================
 */

static void
Create_Special_Global_Symbols ()
{
    TY_IDX ty_idx;
    TY_KIND ty_kind = KIND_SCALAR;
    
    /* Make predefined types for the machine types: */
    for (TYPE_ID i = MTYPE_FIRST; i <= MTYPE_LAST; ++i) {

	if (MTYPE_To_TY (i) != 0)
	    continue;

	TY &ty = New_TY (ty_idx);

	ty_kind = KIND_SCALAR;
        if (MTYPE_align_req (i))
	    Set_TY_align (ty_idx, MTYPE_align_req(i));

	/* If this is one of the basic spill types, remember it: */
	if ( i == Spill_Int_Mtype )	{
	    Spill_Int_Type = ty_idx;
	} else if ( i == Spill_Float_Mtype ) {
	    Spill_Float_Type = ty_idx;
	}

#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
#ifndef FRONT_END_MFEF77
	if ( i == FE_int_To_Mtype() )
	    FE_int_Type = ty_idx;
#endif /* FRONT_END_MFEF77 */
	if ( i == MTYPE_F8 )
	    FE_double_Type = ty_idx;
#endif /* FRONT_END_C || FRONT_END_CPLUSPLUS */
	if ( i == MTYPE_V ) {
	    Void_Type = ty_idx;
	    ty_kind = KIND_VOID;
	}

	if (i == MTYPE_A4 || i == MTYPE_A8) {
	    if (MTYPE_byte_size (i) != Pointer_Size) {
		Ty_tab.Delete_last ();
		MTYPE_To_TY (i) = 0;
		continue;
	    }

	    ty_kind = KIND_POINTER;
	    TY_Init (ty, MTYPE_byte_size (i), ty_kind, i,
		     Gen_predef_type_name (MTYPE_name (i)));
	    Is_True (i > MTYPE_V,
		     ("Create_Special_Global_Symbols() assumes MTYPE_V < "
		      "MTYPE_A4 and MTYPE_A8"));
	    Set_TY_pointed (ty, MTYPE_To_TY (MTYPE_V));
	    MTYPE_To_TY (i) = ty_idx;
	    continue;
	}

	TY_Init (ty, MTYPE_byte_size (i), ty_kind, i,
		 Gen_predef_type_name (MTYPE_name (i)));
	MTYPE_To_TY (i) = ty_idx;
    }

} // Create_Special_Global_Symbols


static inline BOOL
Predefined_Void_Type (TY_IDX ty_idx)
{
    const TY& ty = Ty_Table[ty_idx];
    return (ty_idx == make_TY_IDX (TY_IDX_index (ty_idx)) &&
	    TY_mtype (ty) == MTYPE_V && TY_kind (ty) == KIND_VOID);
    
} // Predefined_Void_Type


// In phases other then the frontends, we read in the global symtab from the
// file, and that includes all the predefined global symbols and types.  We
// don't need to create them again, but we still need to initialize the
// MTYPE_TO_TY array.
void
Initialize_Special_Global_Symbols ()
{
    // ASSUME all mtype TYs, if defined, are placed before any other TY's

    UINT32 last = MIN (MTYPE_LAST + 1, TY_Table_Size());
    // for trivial program, the type table may have less entry than MTYPE_LAST

    for (UINT32 i = MTYPE_FIRST; i < last; ++i) {
	TY_IDX ty_idx = make_TY_IDX (i);
	const TY& ty = Ty_Table[ty_idx];

	TYPE_ID mtype = TY_mtype (ty);
	if (MTYPE_byte_size (mtype) != TY_size (ty) ||
	    TY_flags (ty) != 0)
	    continue;
	switch (mtype) {
	case MTYPE_V:
	    if (TY_kind (ty) != KIND_VOID)
		continue;
	    break;

	case MTYPE_A4:
	case MTYPE_A8:
	    if (TY_kind (ty) != KIND_POINTER || 
		! Predefined_Void_Type (TY_pointed (ty)))
		continue;
	    break;

	default:
	    if (TY_kind (ty) != KIND_SCALAR)
		continue;
	    break;
	}
	if (MTYPE_align_req (mtype))
	    Set_TY_align (ty_idx, MTYPE_align_req (mtype));
	MTYPE_To_TY (mtype) = ty_idx;
    }
	
    // create any missing predefined types
    Create_Special_Global_Symbols ();
	    
    Spill_Int_Type = MTYPE_To_TY (Spill_Int_Mtype);
    Spill_Float_Type = MTYPE_To_TY (Spill_Float_Mtype);
#ifdef KEY
    /* Bug#246
       MTYPE_FQ is reserved for 'long double' type.
     */
    Quad_Type = MTYPE_To_TY (MTYPE_F16);
#else
    Quad_Type = MTYPE_To_TY (MTYPE_FQ);
#endif // KEY
    Void_Type = MTYPE_To_TY (MTYPE_V);

#if defined(TARG_X8664) || defined(TARG_MIPS)
    Spill_Int32_Type   = MTYPE_To_TY (Spill_Int32_Mtype);
    Spill_Float32_Type = MTYPE_To_TY (Spill_Float32_Mtype);
#endif

    Set_up_all_preg_symbols ();

} // Initialize_Special_Global_Symbols


// Initialize all symbol tables.  This function must be called before any
// symbol table operation is performed.
// If reading the symtab from a file, then reserve_index_zero should be FALSE
void
Initialize_Symbol_Tables (BOOL reserve_index_zero)
{
    if (Scope_tab != NULL)
	return;
    
    Scope_tab = (SCOPE *) MEM_POOL_Alloc (Malloc_Mem_Pool,
					  Max_scope * sizeof(SCOPE));
    BZERO(Scope_tab, Max_scope * sizeof(SCOPE));

    BZERO (MTYPE_TO_PREG_array, sizeof(ST*) * (MTYPE_LAST + 1));
    BZERO (MTYPE_TO_TY_array, sizeof(TY_IDX) * (MTYPE_LAST + 1));

    if (reserve_index_zero) {
	// For producer, we reserve first entry for all global tables

	Initialize_Strtab (0x1000);	// start with 4Kbytes for strtab.

	UINT dummy_idx;
	BZERO (&New_PU ((PU_IDX&) dummy_idx), sizeof(PU));
	BZERO (&New_TY ((TY_IDX&) dummy_idx), sizeof(TY));
	BZERO (New_FLD ().Entry(), sizeof(FLD));
	BZERO (&New_TYLIST ((TYLIST_IDX&) dummy_idx), sizeof(TYLIST));
	BZERO (New_ARB ().Entry(), sizeof(ARB));
	BZERO (&New_BLK ((BLK_IDX&) dummy_idx), sizeof(BLK));
	BZERO (&Initv_Table.New_entry ((INITV_IDX&) dummy_idx), sizeof(INITV));
	Init_Constab ();
	New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, TRUE);
	Create_Special_Global_Symbols ();
	Create_All_Preg_Symbols();
    }
#ifdef BACK_END
    if (!Read_Global_Data) {
       // reserve zero index in BLK table
       UINT blk_idx;
       BZERO (&New_BLK ((BLK_IDX&) blk_idx), sizeof(BLK));
    }
#endif
}

//----------------------------------------------------------------------
// TCON
//----------------------------------------------------------------------
TCON_IDX
Enter_tcon (const TCON& tcon)
{
    switch (TCON_ty (tcon)) {
    case MTYPE_F4:
	if (TCON_ival (tcon) == 0)
	    return 1;
    case MTYPE_F8:
        if (TCON_k0 (tcon) == 0)
            return 2;
    default:
	return Tcon_Table.Insert (tcon);
    }
} // Enter_tcon


inline void
Init_Constab ()
{
    if (Tcon_Table.Size () == 0) {
	TCON Zero;
	UINT32 idx;
        BZERO (&Zero, sizeof(TCON));
        idx = Tcon_Table.Insert (Zero);	// index 0: dummy
	Set_TCON_ty (Zero, MTYPE_F4); 
        idx = Tcon_Table.Insert (Zero);	// index 1: float (0.0)
	Set_TCON_ty (Zero, MTYPE_F8); 
        idx = Tcon_Table.Insert (Zero);	// index 2: double (0.0)

	Is_True (idx == MAX_PREDEFINED_TCON_IDX,
		 ("Number of predefined tcons incorrect"));

	Initialize_TCON_strtab (1024);	// string table for TCONs
    }
}
// Overriding the operator == to allow comparison of two ST

// efficiently and safely. Using bcmp to compare class is incorrect

// because it compares the padded space between class fields

// unnecessarily

BOOL

ST::operator==( ST &st ) const

{

  if (st.u1.name_idx == u1.name_idx &&

      st.flags == flags &&

      st.flags_ext == flags_ext &&

      st.sym_class == sym_class &&

      st.storage_class == storage_class &&

      st.export_class == export_class &&
      
      st.u2.type == u2.type &&

      st.offset == offset &&

      st.base_idx == base_idx &&

      st.st_idx == st_idx)

         return TRUE;

  
  return FALSE;
  
}


#ifdef Is_True_On

//
// Additional debugging functions because Workshop can't deal with 
// the [] operator correctly.
//

ST ST_from_IDX(ST_IDX x) {return St_Table[x];}
TY TY_from_IDX(TY_IDX x) {return Ty_Table[x];}
INITO INITO_from_IDX(INITO_IDX x) {return Inito_Table[x];}
INITV INITV_from_IDX(INITV_IDX x) {return Initv_Table[x];}
PU PU_from_IDX(PU_IDX x) {return Pu_Table[x];}
ARB ARB_from_IDX(ARB_IDX x) {return Arb_Table[x];}
TCON TCON_from_IDX(TCON_IDX x) {return Tcon_Table[x];}
FLD FLD_from_IDX(FLD_IDX x) {return Fld_Table[x];}
LABEL LABEL_from_IDX(LABEL_IDX x) {return Label_Table[x];}
TYLIST TYLIST_from_IDX(TYLIST_IDX x) {return Tylist_Table[x];}

#endif // Is_True_On
