/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


#include <stdint.h>
#include "defs.h"
#include "symtab.h"

#include "ipc_symtab_merge.h"		// for IPC_GLOBAL_TABS, etc.
#include "ipc_ty_hash.h"		// for merging types hash tables
#include "ipc_type_merge.h"

#include "config_ipa.h"
#include <ext/hash_set>
#include <ext/hash_map>
using __gnu_cxx::hash_set;
using __gnu_cxx::hash_map;
using __gnu_cxx::hash_multimap;

// For every struct type, this table maps its name to its TY index
hash_map <STR_IDX, TY_INDEX, __new_hash::hash<STR_IDX>, std::equal_to<STR_IDX> > struct_by_name_idx;

// For each struct type, if there are more than one type in the merged type table
typedef hash_multimap<STR_IDX, TY_INDEX, 
              __new_hash::hash<STR_IDX>, 
              std::equal_to<STR_IDX> > multimap_type;
multimap_type duplicated_types;

// For the new incomplete struct TYs that should be updated after main type merge phase 
hash_set <TY_INDEX> to_update_incomplete_ty;

// For the old incomplete struct TYs that meets a new same complete struct TY 
hash_set <TY_INDEX> updating_incomplete_ty;

// Algorithm:
//
// 1) for leaf nodes, we simple insert_unqiue
//
// 2) for non leaf node, we start an loop detection phase
//    a) if no loop is found, we insert_unqiue from the leaf up.
//    b) if a loop is found, we start an validation phase
//
// 3) validation phase:
//    a) find a list of all merged TY that match the current node except
//       the indices
//    b) for each of such TY, set the mapped idx temporarily and follow the 
//       chain to verify
//    c) if validation is ok, commit the current node


static inline BOOL
ARB_equal (const ARB_HANDLE merged_arb, const ARB& new_arb)
{
    const UINT64* p1 =  reinterpret_cast<const UINT64*> (merged_arb.Entry ());
    const UINT64* p2 =  reinterpret_cast<const UINT64*> (&new_arb);

    return (p1[0] == p2[0] &&
	    p1[1] == p2[1] &&
	    p1[2] == p2[2] &&
	    p1[3] == p2[3]);
}

// ======================================================================
// function objects to provide uniform access to different TY kinds
// ======================================================================

static const IPC_GLOBAL_TABS* file_tables;

struct array_access
{
    TY_KIND kind () const { return KIND_ARRAY; }

    UINT kid_count () const { return 1; }
	
    TY_IDX kid (const TY& ty, UINT) const {
	return TY_etype (ty);
    }
    
    // validate the array-specific fields
    BOOL validate (const TY& merged_ty, const TY& new_ty) const {
	return Partial_Compare_Arb (TY_arb (merged_ty), 
				    file_tables->arb_tab + new_ty.Arb ());
    }

}; // array_access


struct struct_access
{
    const TY& new_ty;
    const TY& merged_ty;
	
    struct_access (const TY& _new_ty, const TY& _merged_ty) :
	new_ty (_new_ty), merged_ty (_merged_ty) {}
	
    TY_KIND kind () const { return KIND_STRUCT; }

    UINT kid_count () const {
	if (new_ty.Fld () == 0)
	    return 0;
	UINT count = 0;
	const FLD* fld = file_tables->fld_tab + new_ty.Fld () ;
	do {
	    ++count;
	} while ((fld++->flags & FLD_LAST_FIELD) == 0);
	return count;
    }

    TY_IDX kid (const TY& ty, UINT i) const {
	if (&ty == &new_ty) {
	    const FLD* fld = file_tables->fld_tab + new_ty.Fld ();
	    return fld[i].type;
	} else {
	    FLD_HANDLE fld (TY_fld (merged_ty).Idx () + i);
	    return FLD_type (fld);
	}
    }

    BOOL validate (const TY& merged_ty, const TY& new_ty) const {
	if (new_ty.Fld () == 0 || merged_ty.Fld () == 0)
	    return new_ty.Fld () == merged_ty.Fld ();
	return Partial_Compare_Fld (TY_fld (merged_ty),
				    file_tables->fld_tab + new_ty.Fld ());
    }
    
}; // struct_access


struct pointer_access
{
    TY_KIND kind () const { return KIND_POINTER; }

    UINT kid_count () const { return 1; }

    TY_IDX kid (const TY& ty, UINT) const {
	return TY_pointed (ty);
    }

    BOOL validate (const TY&, const TY&) const {
	return TRUE;
    }
}; // pointer_access


struct function_access
{
    const TY& new_ty;
    const TY& merged_ty;
	
    function_access (const TY& _new_ty, const TY& _merged_ty) :
	new_ty (_new_ty), merged_ty (_merged_ty) {}

    TY_KIND kind () const { return KIND_FUNCTION; }

    UINT kid_count () const {
	UINT count = 0;
	const TYLIST* tylist = file_tables->tylist_tab + TY_tylist (new_ty);
	while (*tylist != 0) {
	    ++count;
	    ++tylist;
	}
	return count;
    }

    TY_IDX kid (const TY& ty, UINT i) const {
	if (&ty == &new_ty) {
	    return file_tables->tylist_tab[TY_tylist (ty) + i];
	} else {
	    TYLIST_IDX idx = TY_tylist (merged_ty) + i;
	    return Tylist_Table[idx];
	}
    }

    BOOL validate (const TY& merged_ty, const TY& new_ty) const {
	return new_ty.Pu_flags () == merged_ty.Pu_flags ();
    }
}; // function_access
	



// ======================================================================
// validation state for Validate_Recursive_Type ()
// ======================================================================
enum VALIDATION_STATE
{
    VALIDATE_OK		= 1,		// no error found
    VALIDATE_FAIL	= 2,		
    VALIDATE_COMMIT	= 3		// no error found + already
					// inserted into hash table
};


static VALIDATION_STATE
Validate_Recursive_Type (TY_INDEX index, TY_IDX_MAP& ty_map,
			 const SYMSTR_IDX_MAP& str_map, TY_IDX merged_ty_idx);

namespace
{
   
    /* check if the TY is 
       1. incomplete struct, or
       2. pointer to incomplete struct */
    inline BOOL 
    TY_is_incomplete (TY_IDX tyi, BOOL in_file) {
        if (in_file) {
            TY &ty = file_tables->ty_tab[TY_IDX_index(tyi)];
            if (TY_kind(ty) == KIND_STRUCT)
                return TY_is_incomplete_struct(ty);
            if (TY_kind(ty) == KIND_POINTER)
                return TY_is_incomplete_struct(file_tables->ty_tab[TY_IDX_index(TY_pointed(ty))]);
        }
        else {
            if (TY_kind(tyi) == KIND_STRUCT)
                return TY_is_incomplete_struct(tyi) || TY_align(tyi) == 1;
            if (TY_kind(tyi) == KIND_POINTER)
                return TY_is_incomplete_struct(TY_pointed(tyi)) 
                       || TY_align(TY_pointed(tyi)) == 1;
        }   
        return FALSE;
    }

    // This old partial match function should be removed when the old type merge is removed. Yao Shi, 2007.4.19 
    template <class ACCESS>
    VALIDATION_STATE
    Partial_Match (const TY& merged_ty, const TY& new_ty,
		   TY_IDX_MAP& ty_map,
		   const SYMSTR_IDX_MAP& str_map,
		   const ACCESS& ty_node)
    {
	Is_True (TY_kind (new_ty) == ty_node.kind (),
		 ("Invalid type attributes for array"));

	if (TY_size (merged_ty) != TY_size (new_ty) ||
	    TY_kind (merged_ty) != TY_kind (new_ty) ||
	    TY_mtype (merged_ty) != TY_mtype (new_ty) ||
	    TY_name_idx (merged_ty) != str_map[TY_name_idx (new_ty)])
	    return VALIDATE_FAIL;

        // for struct types, the TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE
        // flag may be set.  This should not block type merging.
        mUINT32 merged_ty_flags_copy = TY_flags(merged_ty);
        mUINT32 new_ty_flags_copy = TY_flags(new_ty);
        if (TY_kind(merged_ty) == KIND_STRUCT)
        {
          // we already know the kinds are the same
          merged_ty_flags_copy &= ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE;
          new_ty_flags_copy &= ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE;
        }
        if (merged_ty_flags_copy != new_ty_flags_copy)
          return VALIDATE_FAIL;

	UINT kid_count = ty_node.kid_count ();
	UINT i;
	for (i = 0; i < kid_count; ++i) {
	    if (TY_IDX_Attributes (ty_node.kid (merged_ty, i)) !=
		TY_IDX_Attributes (ty_node.kid (new_ty, i)))
		return VALIDATE_FAIL;
	}

	if (! ty_node.validate (merged_ty, new_ty))
	    return VALIDATE_FAIL;

	BOOL all_kids_committed = TRUE;
	// everything matches so far, now look at the kids
	for (i = 0; i < kid_count; ++i) {
	    TY_INDEX kid_index = TY_IDX_index (ty_node.kid (new_ty, i));
	    TY_IDX kid_mapped_idx = ty_map.map_[kid_index];

	    if (TY_Inserted (kid_mapped_idx))
		return VALIDATE_FAIL;

	    if (Is_TY_Temp_Idx (kid_mapped_idx)) {
		all_kids_committed = FALSE;
		Clean_TY_IDX (kid_mapped_idx);
	    }
	    
	    if (Valid_TY_IDX (kid_mapped_idx)) {
		if (TY_IDX_index (ty_node.kid (merged_ty, i)) !=
		    TY_IDX_index (kid_mapped_idx))
		    return VALIDATE_FAIL;
	    } else {
		VALIDATION_STATE v_state =
		    Validate_Recursive_Type (kid_index, ty_map, str_map, 
					     ty_node.kid (merged_ty, i));
		
		if (v_state == VALIDATE_FAIL)
		    return v_state;
		else if (v_state == VALIDATE_OK)
		    all_kids_committed = FALSE;
	    }
	}

	// all kids' types match
	if (all_kids_committed) {
	    return VALIDATE_COMMIT;
	} else
	    return VALIDATE_OK;
    } // Partial_Match

    template <class ACCESS>
    VALIDATION_STATE
    New_Partial_Match (const TY& merged_ty, const TY& new_ty,
                   TY_IDX_MAP& ty_map,
                   const SYMSTR_IDX_MAP& str_map,
                   const ACCESS& ty_node)
    {
        Is_True (TY_kind (new_ty) == ty_node.kind (),
                 ("Invalid type attributes for array"));

        if (TY_kind (merged_ty) != TY_kind (new_ty))
            return VALIDATE_FAIL;
        if (TY_kind (merged_ty) == KIND_STRUCT || TY_kind (merged_ty) == KIND_ARRAY) {
            if (TY_anonymous(merged_ty) || TY_anonymous(new_ty)) {
                if (TY_anonymous(merged_ty) != TY_anonymous(new_ty))
                    return VALIDATE_FAIL;
            }
            else {
                if (TY_name_idx (merged_ty) != str_map[TY_name_idx (new_ty)])
                    return VALIDATE_FAIL;
            }
            if (TY_is_incomplete_struct(merged_ty) || TY_is_incomplete_struct(new_ty))
                return VALIDATE_OK;
        } 
        else
           if (TY_name_idx (merged_ty) != str_map[TY_name_idx (new_ty)]) 
               return VALIDATE_FAIL;
        if (TY_size (merged_ty) != TY_size (new_ty) ||
            TY_mtype (merged_ty) != TY_mtype (new_ty))
            return VALIDATE_FAIL;

        // for struct types, the TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE
        // flag may be set.  This should not block type merging.
        mUINT32 merged_ty_flags_copy = TY_flags(merged_ty);
        mUINT32 new_ty_flags_copy = TY_flags(new_ty);
        if (TY_kind(merged_ty) == KIND_STRUCT)
        {
          // we already know the kinds are the same
          merged_ty_flags_copy &= ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE;
          new_ty_flags_copy &= ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE;
        }
        if (merged_ty_flags_copy != new_ty_flags_copy)
          return VALIDATE_FAIL;

        UINT kid_count = ty_node.kid_count ();
        UINT i;
        for (i = 0; i < kid_count; ++i) {
            TY_IDX merged_kid_idx = ty_node.kid(merged_ty, i);
            TY_IDX new_kid_idx = ty_node.kid(new_ty, i);
            // It only compares the attributes of 2 TYs if they are both complete type.
            if ((TY_IDX_Attributes(merged_kid_idx) != TY_IDX_Attributes(new_kid_idx))
                && (!TY_is_incomplete(merged_kid_idx, FALSE))
                && (!TY_is_incomplete(new_kid_idx, TRUE)))                
                return VALIDATE_FAIL;
        }

        if (! ty_node.validate (merged_ty, new_ty))
            return VALIDATE_FAIL;

        BOOL all_kids_committed = TRUE;
        // everything matches so far, now look at the kids
        for (i = 0; i < kid_count; ++i) {
            TY_INDEX kid_index = TY_IDX_index (ty_node.kid (new_ty, i));
            TY_IDX kid_mapped_idx = ty_map.map_[kid_index];

            if (TY_Inserted (kid_mapped_idx))
                return VALIDATE_FAIL;

            if (Is_TY_Temp_Idx (kid_mapped_idx)) {
                all_kids_committed = FALSE;
                Clean_TY_IDX (kid_mapped_idx);
            }

            if (Valid_TY_IDX (kid_mapped_idx)) {
                if (TY_IDX_index (ty_node.kid (merged_ty, i)) !=
                    TY_IDX_index (kid_mapped_idx))
                    return VALIDATE_FAIL;
            } else {
                VALIDATION_STATE v_state =
                    Validate_Recursive_Type (kid_index, ty_map, str_map,
                                             ty_node.kid (merged_ty, i));

                if (v_state == VALIDATE_FAIL)
                    return v_state;
                else if (v_state == VALIDATE_OK)
                    all_kids_committed = FALSE;
            }
       }

        // all kids' types match
        if (all_kids_committed) {
            return VALIDATE_COMMIT;
        } else
            return VALIDATE_OK;
    } // Partial_Match
}


static VALIDATION_STATE
Validate_Recursive_Type (TY_INDEX index, TY_IDX_MAP& ty_map,
			 const SYMSTR_IDX_MAP& str_map, TY_IDX merged_ty_idx)
{
    TY_IDX& my_mapped_idx = ty_map.map_[index];

    Is_True (! Valid_TY_IDX (my_mapped_idx),
	     ("Invalid call to Validate_Recursive_Type"));

    const TY& new_ty = file_tables->ty_tab[index];
    const TY& merged_ty = Ty_Table[merged_ty_idx];

    if (TY_kind (new_ty) != TY_kind (merged_ty))
	return VALIDATE_FAIL;

    VALIDATION_STATE v_state;
    
    switch (TY_kind (new_ty)) {
	
    case KIND_SCALAR:
    case KIND_VOID:
	ty_map.set_map (index, Insert_Unique_Ty (new_ty));
	if (ty_map.map_[index] == merged_ty_idx) {
	    return VALIDATE_COMMIT;
	} else
	    return VALIDATE_FAIL;

    case KIND_ARRAY:
	Set_TY_Temp_Idx (my_mapped_idx, merged_ty_idx);
	if (IPA_Enable_Old_Type_Merge)
            v_state = Partial_Match (merged_ty, new_ty, ty_map, str_map,
				 	array_access ());
        else
            v_state = New_Partial_Match (merged_ty, new_ty, ty_map, str_map,
                                        array_access ());
	break;

    case KIND_STRUCT:
	Set_TY_Temp_Idx (my_mapped_idx, merged_ty_idx);
        if (IPA_Enable_Old_Type_Merge)
	    v_state = Partial_Match (merged_ty, new_ty, ty_map, str_map,
                                     struct_access (new_ty, merged_ty));
        else 
            v_state = New_Partial_Match (merged_ty, new_ty, ty_map, str_map,
                                     struct_access (new_ty, merged_ty));
	break;
	
    case KIND_POINTER:
	Set_TY_Temp_Idx (my_mapped_idx, merged_ty_idx);
        if (IPA_Enable_Old_Type_Merge)
	    v_state = Partial_Match (merged_ty, new_ty, ty_map, str_map,
				 	pointer_access ());
        else
            v_state = New_Partial_Match (merged_ty, new_ty, ty_map, str_map,
                                        pointer_access ());
	break;

    case KIND_FUNCTION:
	Set_TY_Temp_Idx (my_mapped_idx, merged_ty_idx);
        if (IPA_Enable_Old_Type_Merge)
	    v_state = Partial_Match (merged_ty, new_ty, ty_map, str_map,
				 function_access (new_ty, merged_ty));
        else 
            v_state = New_Partial_Match (merged_ty, new_ty, ty_map, str_map,
                                 function_access (new_ty, merged_ty));
	break;
    }
    
    if (v_state == VALIDATE_COMMIT)
	ty_map.set_map (index, Insert_Unique_Ty (new_ty));
    return v_state;
} // Validate_Recursive_Type

static void
Clear_All_Temp_Idx (TY_INDEX index, TY_IDX_MAP& ty_map);

namespace {

    template <class ACCESS>
    void
    Clear_Temp_Idx_Specific (const TY& new_ty, TY_IDX_MAP& ty_map,
			const ACCESS& ty_node)
    {
	UINT kid_count = ty_node.kid_count ();
	for (UINT i = 0; i < kid_count; ++i) {
	    TY_INDEX kid_index = TY_IDX_index (ty_node.kid (new_ty, i));
	    TY_IDX mapped_kid_idx = ty_map.map_[kid_index];
	    if (Is_TY_Temp_Idx (mapped_kid_idx))
		Clear_All_Temp_Idx (kid_index, ty_map);
	}
    }
}


// Clear all temp_idx that have been set
static void
Clear_All_Temp_Idx (TY_INDEX index, TY_IDX_MAP& ty_map)
{
    TY_IDX& my_mapped_idx = ty_map.map_[index];

    if (! Is_TY_Temp_Idx (my_mapped_idx))
	return;

    Clear_TY_Temp_Idx (my_mapped_idx);

    const TY& new_ty = file_tables->ty_tab[index];

    switch (TY_kind (new_ty)) {
    case KIND_SCALAR:
    case KIND_VOID:
	return;

    case KIND_ARRAY:
	Clear_Temp_Idx_Specific (new_ty, ty_map, array_access ());
	break;

    case KIND_STRUCT:
	Clear_Temp_Idx_Specific (new_ty, ty_map, struct_access (new_ty,
								new_ty));  
	break;

    case KIND_POINTER:
	Clear_Temp_Idx_Specific (new_ty, ty_map, pointer_access ()); 
	break;
	
    case KIND_FUNCTION:
	Clear_Temp_Idx_Specific (new_ty, ty_map, function_access (new_ty,
								  new_ty)); 
	
	break;
    }
    
} // Clear_All_Temp_Idx 


// Given a recursive type, search in the merged symtab for a matching one.
static TY_IDX
Find_Recursive_Type (TY_INDEX index, TY_IDX_MAP& ty_map,
		     const SYMSTR_IDX_MAP& str_map, const TY& ty)
{
    if (!IPA_Enable_Old_Type_Merge) {
        if (TY_kind(ty) == KIND_STRUCT) {
            STR_IDX struct_name = str_map[TY_name_idx(ty)];
            TY_INDEX find_ty = struct_by_name_idx[struct_name];
            // if a merged TY is found and one of the new and the merged TY is incomplete,
            // do not continue to find TY, return the merged TY directly.
            if (find_ty &&
                (TY_is_incomplete_struct(make_TY_IDX(find_ty)) || TY_is_incomplete_struct(ty)))
                return make_TY_IDX(find_ty);
        }
    }

    // Get a list of all types in the merged symtab that match "ty" except
    // for the indices
    TY_IDX_VEC ty_list;
    Find_Matching_Ty (ty, ty_list);
    
    for (TY_IDX_VEC::const_iterator iter (ty_list.begin ());
	 iter != ty_list.end (); ++iter) {
	VALIDATION_STATE state =
	    Validate_Recursive_Type (index, ty_map, str_map, *iter);
	
	if (state == VALIDATE_OK) {
	    return *iter;
	}
	
	Is_True (state != VALIDATE_COMMIT, ("Inconsistent cycles in types"));

	Clear_All_Temp_Idx (index, ty_map);
    }

    return 0;
} // Find_Recursive_Type


// Map the given recursive type to the matching one in the merged symtab
static void
Commit_Recursive_Type (TY_INDEX index, TY_IDX_MAP& ty_map,
		       TY_IDX merged_ty_idx);
namespace
{

    template <class ACCESS>
    void
    Commit_Ty_Specific (const TY& new_ty, TY_IDX_MAP& ty_map,
			TY_IDX merged_ty_idx, const ACCESS& ty_node)
    {
	const TY& merged_ty = Ty_Table[merged_ty_idx];
	Is_True (TY_kind (merged_ty) == ty_node.kind (),
		 ("Invalid type in Commit_Recursive_Type"));
	
	UINT kid_count = ty_node.kid_count ();
	for (UINT i = 0; i < kid_count; ++i) {
	    TY_INDEX kid_index = TY_IDX_index (ty_node.kid (new_ty, i));
	    TY_IDX mapped_kid_idx = ty_map.map_[kid_index];
	    if (! Valid_TY_IDX (mapped_kid_idx))
		Commit_Recursive_Type (kid_index, ty_map,
				       ty_node.kid (merged_ty, i));
	}
    }
}

	   
// Map the given recursive type to the matching one in the merged symtab
static void
Commit_Recursive_Type (TY_INDEX index, TY_IDX_MAP& ty_map,
		       TY_IDX merged_ty_idx)
{
    const TY& new_ty = file_tables->ty_tab[index];
    
    if (!IPA_Enable_Old_Type_Merge) {
        if (TY_is_incomplete_struct(new_ty) || TY_is_incomplete_struct(merged_ty_idx)) {
            // If the new TY is complete and the merged TY is incomplete,
            // the merged TY should be updated.
            if (!TY_is_incomplete_struct(new_ty)) {
                // If 2 TY in a object file have the same name, do not merge them.
                // So if the first one is merged, insert a new TY for the second one.
                if (IN_SET(updating_incomplete_ty, TY_IDX_index(merged_ty_idx)))
                    TY_Init(New_TY(merged_ty_idx), 0, KIND_STRUCT, MTYPE_M, 0);
                to_update_incomplete_ty.insert(index);
                updating_incomplete_ty.insert(TY_IDX_index(merged_ty_idx));
            }
            ty_map.set_map(index, merged_ty_idx);
            return;
        }
    }

    TY_IDX& my_mapped_idx = ty_map.map_[index];

    Is_True (! Valid_TY_IDX (my_mapped_idx), ("Invalid type index"));

    ty_map.set_map (index, merged_ty_idx);

    switch (TY_kind (new_ty)) {
    case KIND_SCALAR:
    case KIND_VOID:
	return;

    case KIND_ARRAY:
	Commit_Ty_Specific (new_ty, ty_map, merged_ty_idx, array_access ());
	break;

    case KIND_STRUCT:
        // flags should technically already be identical if we are
        // merging, but when we compared, we stripped out the
        // TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE flag
        Set_TY_flags(merged_ty_idx, 
                     TY_flags(merged_ty_idx) | TY_flags(new_ty));
	Commit_Ty_Specific (new_ty, ty_map, merged_ty_idx,
			    struct_access (new_ty, Ty_Table[merged_ty_idx])); 
	break;

    case KIND_POINTER:
	Commit_Ty_Specific (new_ty, ty_map, merged_ty_idx, pointer_access ()); 
	break;
	
    case KIND_FUNCTION:
	Commit_Ty_Specific (new_ty, ty_map, merged_ty_idx,
			    function_access (new_ty, Ty_Table[merged_ty_idx]));
	break;
    }
} // Commit_Recursive_Type


static TY_IDX
Insert_Ty (TY_INDEX index, TY_IDX_MAP& ty_map, const SYMSTR_IDX_MAP& str_map);

namespace
{

    template <class ACCESS>
    TY_IDX
    Insert_Ty_Specific (TY_INDEX index, TY_IDX_MAP& ty_map,
			const SYMSTR_IDX_MAP& str_map, const ACCESS& ty_node)
    {
	const TY& ty = file_tables->ty_tab[index];
	TY_IDX& my_mapped_idx = ty_map.map_[index];

	// check if the kid type has been merged
	UINT kid_count = ty_node.kid_count ();
	BOOL new_recursive_type = FALSE;
	UINT i;
	for (i = 0; i < kid_count; ++i ) {
	    TY_IDX kid_idx = ty_node.kid (ty, i);
	    TY_IDX mapped_kid_idx = ty_map.map_[TY_IDX_index (kid_idx)];

	    Is_True (! Is_TY_Temp_Idx (mapped_kid_idx),
		     ("Overlapping insertion and validation phase"));

	    if (mapped_kid_idx == 0) {
		mapped_kid_idx = Insert_Ty (TY_IDX_index (kid_idx), ty_map,
					    str_map);
		if (Valid_TY_IDX (my_mapped_idx)) {
		    // The kid recursively points back to myself AND found a
		    // match for myself in the merged TY table
		    return my_mapped_idx;
		}
	    } else if (TY_Inserted (mapped_kid_idx))
		continue;

	    if (!Valid_TY_IDX (mapped_kid_idx)) {
		TY_IDX original_ty_idx;
		Is_True (TY_Merging (mapped_kid_idx),
			 ("Invalid state in type merging"));

		// store original idx value
		original_ty_idx = my_mapped_idx;

		// this is a recursive type, the kid is pointing back to a
		// previously visited TY.  We now need to search for a
		// matching recursive type in the merged symtab, if we
		// haven't already done so.
		TY_IDX matched_idx = new_recursive_type ? 0 :
		    Find_Recursive_Type (index, ty_map, str_map, ty);

		if (matched_idx == 0) {
			my_mapped_idx = Replace_TY_IDX_index(my_mapped_idx, original_ty_idx);
		}
		
		if (matched_idx == 0) {
		    // No matching cycle is found
		     TY_IDX recursive_ty_idx;
		     (void) New_TY (recursive_ty_idx);
		     Set_TY_Inserted (ty_map.map_[TY_IDX_index (kid_idx)],
				      recursive_ty_idx);
                     if (IPA_Enable_Old_Type_Merge) 
                         Initialize_New_Recursive_Type (recursive_ty_idx);
		} else {
		    Is_True (Valid_TY_IDX (matched_idx), ("Invalid TY_IDX"));
		    // found a match, we can commit the entire recursive type 
		    // (connected component) and return.
		    Commit_Recursive_Type (index, ty_map, matched_idx); 
		    return matched_idx;
		}
	    }
	}

	// Decided to insert this type into the merged symtab
	if (TY_Inserted (my_mapped_idx)) {
	    // an entry has already been reserved for me
	    Clean_TY_IDX (my_mapped_idx);
	    TY& new_ty = Ty_Table[my_mapped_idx];
	    new_ty = ty;
	    Insert_Allocated_Ty (new_ty, my_mapped_idx);
            if (IPA_Enable_Old_Type_Merge) {
	        Finalize_New_Recursive_Type ();
            }
            else {
                if (TY_is_incomplete_struct(my_mapped_idx))
                    updating_incomplete_ty.insert(TY_IDX_index(my_mapped_idx));
            }
	    return my_mapped_idx;
	} else {
            if (IPA_Enable_Old_Type_Merge) {
	        return Insert_Unique_Ty (ty);
            }
            else {
                TY_IDX return_idx = Insert_Unique_Ty(ty);
                if (TY_is_incomplete_struct(return_idx))
                    updating_incomplete_ty.insert(TY_IDX_index(return_idx));
                return return_idx;
            }
        }
    } // Insert_Ty_Specific
} // namespace


static TY_IDX
Insert_Ty (TY_INDEX index, TY_IDX_MAP& ty_map, const SYMSTR_IDX_MAP& str_map)
{
    TY_IDX& mapped_idx = ty_map.map_[index];

    Is_True (mapped_idx == 0, ("Invalid call to Insert_Ty"));

    Set_TY_Merging (mapped_idx);

    const TY& ty = file_tables->ty_tab[index];

    TY_IDX return_idx;

    if (!IPA_Enable_Old_Type_Merge) {
        if (TY_kind(ty) == KIND_STRUCT) {
            STR_IDX struct_name = str_map[ty.name_idx];
            return_idx = make_TY_IDX(struct_by_name_idx[struct_name]);
            // If the new ty is incomplete struct and find a TY has the same name,
            // then map the mew ty to the old one.
            if (TY_is_incomplete_struct(ty) && return_idx > 0) {
                ty_map.set_map(index, return_idx);
                return return_idx;
            }
            // If ty is complete struct and a corresponding incomplete one exists,
            // then map the new ty to the old one and update the old one.
            if ((!TY_is_incomplete_struct(ty)) && return_idx > 0 
                && TY_is_incomplete_struct(return_idx)) {
                // If 2 TY in a object file have the same name, do not merge them.
                // So if the first one is merged, insert a new TY for the second one.
                if (IN_SET(updating_incomplete_ty, TY_IDX_index(return_idx))) 
                    TY_Init(New_TY(return_idx), 0, KIND_STRUCT, MTYPE_M, 0);
                ty_map.set_map(index, return_idx);
                to_update_incomplete_ty.insert(index);
                updating_incomplete_ty.insert(TY_IDX_index(return_idx));
                return return_idx;
            }
        }
    }

    switch (TY_kind (ty)) {
    case KIND_SCALAR:
    case KIND_VOID:
	return_idx = Insert_Unique_Ty (ty);
	break;
	
    case KIND_ARRAY:
	return_idx = Insert_Ty_Specific (index, ty_map, str_map,
					 array_access ());
	break;
	
    case KIND_STRUCT:
	return_idx = Insert_Ty_Specific (index, ty_map, str_map,
					 struct_access (ty, ty));
	break;

    case KIND_POINTER:
	return_idx = Insert_Ty_Specific (index, ty_map, str_map,
					 pointer_access ());
	break;
	
    case KIND_FUNCTION:
	return_idx = Insert_Ty_Specific (index, ty_map, str_map,
					 function_access (ty, ty));
	break;
	
    default:
	Fail_FmtAssertion ("Invalid TY_kind");
	return 0;
    }
    
    ty_map.set_map (index, return_idx);

    if (!IPA_Enable_Old_Type_Merge) {
        switch (TY_kind(return_idx)) {
            case KIND_POINTER:
                // Update the pointer to incomplete struct 
                // when updating the incomplete struct
                if (TY_align(TY_pointed(return_idx)) == 1)
                    Set_TY_align(Ty_Table[return_idx].u2.pointed, TY_align(TY_pointed(ty)));
                break;
            case KIND_STRUCT: 
            {
                STR_IDX name_idx = TY_name_idx(return_idx);
#ifdef HANDLE_DUPLICATED_STRUCT_TYPE
                // handle duplicated struct/class type
                if (struct_by_name_idx.find(name_idx) != struct_by_name_idx.end() &&
                    struct_by_name_idx[name_idx] != 0 &&
                    struct_by_name_idx[name_idx] != TY_IDX_index(return_idx)) {
                   // the type index are different although they have the same
                   // it is caused by the fact the the fields are not merged to
                   // the corresponding fields of the old merged struct type 
                   // fields
                   TY_INDEX find_ty = struct_by_name_idx[name_idx];
                   duplicated_types.insert(multimap_type::value_type(name_idx, find_ty));
                   TY_IDX find_ty_idx = make_TY_IDX(find_ty);
                   if (TY_vtable(find_ty_idx)) {
                      // propogate the vtable info to new type
                      Is_True(!TY_vtable(return_idx) || 
                              TY_vtable(find_ty_idx) == TY_vtable(return_idx),
                              ("Invalid vtable symbol"));
                      Set_TY_vtable(return_idx, TY_vtable(find_ty_idx));

                   } else if (TY_vtable(return_idx)) {
                      // the new type has vtable set but not the old one,
                      // propogate the vtable info to old type index
                      pair<multimap_type::const_iterator, 
                           multimap_type::const_iterator> p =
                             duplicated_types.equal_range(name_idx);
                      for (multimap_type::const_iterator i = p.first; 
                                                         i != p.second; ++i) {
                         TY_INDEX ty_index = (*i).second;
                         TY_IDX ty_idx = make_TY_IDX(ty_index);
                         if (!TY_vtable(ty_idx)) {
                            Set_TY_vtable(ty_idx, TY_vtable(return_idx));
                         }
                      }
                   }
                }
#endif
                struct_by_name_idx[name_idx] = TY_IDX_index(return_idx);
                break;
            }
        }
    }

    return return_idx;
} // Insert_Ty

/**
 * To speed up finding recursive type, it must reduce the recursive table.
 * We know that below types should be inserted into the recursive table.
 * 1. Explicit recursive types, or
 * 2. Types has kids with incomplete types.
 * So we only recognize the complete non-recursive types and add them into a set.
 * The elements not in the set may be recursive and add them into the recursive table.
 */
hash_set <TY_INDEX> complete_nonrecursive_type;
hash_set <TY_INDEX> processing_recursive_type;
BOOL Is_Incomplete_Or_Recursive(TY_INDEX ty_index) {
    if (IN_SET(complete_nonrecursive_type, ty_index))
        return FALSE;
    if (IN_SET(processing_recursive_type, ty_index)) 
        return TRUE;
    if (TY_is_incomplete_struct(Ty_tab[ty_index]))
        return TRUE;

    processing_recursive_type.insert(ty_index);
    
    BOOL result = FALSE;
    TY_IDX ty_idx = make_TY_IDX(ty_index);
    FLD_HANDLE fld;
    FLD_ITER fld_iter;
    TYLIST_ITER tylist_iter;
    switch (TY_kind(ty_idx)) {
        case KIND_STRUCT:
            if (Ty_tab[ty_index].Fld() == 0)
                break;
            fld = TY_fld(ty_idx);
            fld_iter = Make_fld_iter(fld);
            do {
                if (Is_Incomplete_Or_Recursive(TY_IDX_index(FLD_type(fld)))) {
                    result = TRUE;
                    break;
                }
                if (FLD_last_field(fld))
                    break;
                ++fld_iter;
                fld = fld_iter;
            } while (1);
            break;
        case KIND_POINTER:
            result = Is_Incomplete_Or_Recursive(TY_IDX_index(TY_pointed(ty_idx)));
            break;
        case KIND_FUNCTION:
            tylist_iter = Make_tylist_iter(TY_tylist(ty_idx));
            while (*tylist_iter != 0) {
                if (Is_Incomplete_Or_Recursive(TY_IDX_index(*tylist_iter))) {
                    result = TRUE;
                    break;
                }
                ++tylist_iter;
            }
            break;
        case KIND_ARRAY:
            result = Is_Incomplete_Or_Recursive(TY_IDX_index(TY_etype(ty_idx)));
            break;
    }
    processing_recursive_type.erase(ty_index);
    if (!result)
        complete_nonrecursive_type.insert(ty_index);
    return result;
}

void
Merge_All_Types (const IPC_GLOBAL_TABS& original_tabs,
		 IPC_GLOBAL_IDX_MAP& idx_map)
{
    Temporary_Error_Phase ephase ("IPA Type Merging");

    Setup_Type_Merging_Hash_Tables (original_tabs, idx_map);
    
    file_tables = &original_tabs;
    TY_IDX_MAP& ty_map = idx_map.ty;
    const SYMSTR_IDX_MAP& str_map = idx_map.sym_str;
    
    if (!IPA_Enable_Old_Type_Merge) {
        to_update_incomplete_ty.clear();
        updating_incomplete_ty.clear();
    }

    for (UINT idx = 1; idx < file_tables->ty_tab_size; ++idx) {
	if (ty_map.map_[idx] == 0)
	    (void) Insert_Ty (idx, ty_map, str_map);
	else
	    Is_True (Valid_TY_IDX (ty_map.map_[idx]), ("Invalid TY_IDX"));
    }

    if (!IPA_Enable_Old_Type_Merge) {
        // Update incomplete structs
        for (hash_set <TY_INDEX>::iterator iter = to_update_incomplete_ty.begin();
            iter != to_update_incomplete_ty.end(); iter++) {
            TY_INDEX new_ty_idx = *iter;
            TY_IDX merged_ty_idx = ty_map.map_[new_ty_idx];
            if (TY_is_incomplete_struct(merged_ty_idx)) {
                TY &new_ty = Ty_Table[merged_ty_idx];
                new_ty = file_tables->ty_tab[new_ty_idx];
                Insert_Allocated_Ty(new_ty, merged_ty_idx);
            }
            ty_map.set_map(new_ty_idx, merged_ty_idx);
        }
        for (UINT idx = 1; idx < file_tables->ty_tab_size; ++idx)
            // Check if the TY is complete and non-recursive.
            // If it is recursive or not complete, insert it into recursive table.
            if (Is_Incomplete_Or_Recursive(TY_IDX_index(ty_map.map_[idx])))
                Insert_Recursive_Type(ty_map.map_[idx]);
    }
} // Merge_All_Types

