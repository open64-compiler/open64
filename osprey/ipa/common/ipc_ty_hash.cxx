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
#include <ext/hash_set>
#include <ext/functional>
using __gnu_cxx::identity;
using __gnu_cxx::hashtable;
using __gnu_cxx::hash_multiset;
using std::equal_to;

#include "defs.h"
#include "symtab.h"

#include "ipc_symtab_merge.h"		// for TY_IDX_MAP, etc.
#include "ipc_ty_hash.h"
#include "config_ipa.h"

// maps that convert raw indices (to the tables in file) to merged symtab
// indices 
static TY_IDX_MAP* ty_map;
static SYMSTR_IDX_MAP* str_map;

// base pointers to the tables in file
static const TY* ty_table;
static const FLD* fld_table;
static const ARB* arb_table;
static const TYLIST* tylist_table;

static const TY* ty_to_be_inserted;

#include <ext/hash_map>
using __gnu_cxx::hash_map;
extern hash_map <STR_IDX, TY_INDEX, __new_hash::hash<STR_IDX>, std::equal_to<STR_IDX> > struct_by_name_idx;

static inline TY_IDX
Get_Kid_TY_IDX (TY_IDX ty_idx)
{
    TY_IDX kid_idx = ty_map->map_[TY_IDX_index (ty_idx)];
    if (TY_Inserted (kid_idx))
	Clean_TY_IDX (kid_idx);
    Is_True (Valid_TY_IDX (kid_idx), ("Invalid entry in ty_map"));
    return Replace_TY_IDX_index (ty_idx, kid_idx);
}



// ======================================================================
// Hash table for TY
// ======================================================================
// Fast comparision of two TY's.  Correctness guaranteed by the assertion
// at the beginning of Initialize_Type_Merging_Hash_Tables ()
static inline BOOL
operator== (const TY& ty1, const TY& ty2)
{
    const UINT64* p1 = reinterpret_cast<const UINT64*> (&ty1);
    const UINT64* p2 = reinterpret_cast<const UINT64*> (&ty2);

    return (p1 == p2 || (p1[0] == p2[0] &&
			 p1[1] == p2[1] &&
			 p1[2] == p2[2] &&
			 ty1.Pu_flags() == ty2.Pu_flags()));
}


namespace
{
    // function objects for the type hash table

    struct TY_EXTRACT_KEY {
	const TY& operator() (TY_IDX ty_idx) const {
	    if (ty_idx == (TY_IDX) -1)
		return *ty_to_be_inserted;
	    else
		return Ty_Table[ty_idx];
	}
    };

    struct TY_HASH {
	size_t operator() (const TY& key) const {
            if (TY_kind(key) == KIND_STRUCT && !TY_anonymous(key)) {
               // for named struct/class, use its name index as hash key
               // the hash method down below has problem when two
               // struct/class has same name but the pre-merge field 
               // ids are different
               return key.name_idx;  // the name_idx is after merge idx
            }
	    const UINT64 *p = reinterpret_cast<const UINT64*> (&key);
	    UINT64 tmp;
            if (IPA_Enable_Old_Type_Merge) {
                tmp  = (p[0] ^ p[1]) + p[2] + key.Pu_flags();
            }
            else {
                // The alignment of TY_POINTER may be changed 
                // after the pointed incomplete struct is upadted. 
                // So add TY.u2 without last 3 bits.
                tmp = (p[0] ^ p[1]) + p[2] + TY_IDX_without_attribute(key.Pu_flags());
                // If TY is anonymous, do not add its name.
                if (TY_anonymous(key))
                    tmp -= p[2];
            }
	    return (size_t) (tmp ^ (tmp >> 32));
	}
    };

    // Set the condition to check 2 TYs are equal
    struct TY_IS_EQUIVALENT {
        BOOL operator() (const TY &k1, const TY &k2) const {
            // some fields like vtable is refreshed after TY merge
            // so do not compare them at this time
            // 
            // if we want to merge class definitions (fields)
            // we cannot compare fld either
            if (k1.size != k2.size || 
                k1.kind != k2.kind || 
                k1.mtype != k2.mtype ||
                k1.u1.fld != k2.u1.fld)
                return FALSE;
            
            // for struct types, the TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE
            // flag may be set.  This should not block type merging.
            UINT k1_flags_copy = k1.flags;
            UINT k2_flags_copy = k2.flags;
            if (k1.kind == KIND_STRUCT)
            {
              k1_flags_copy &= ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE;
              k2_flags_copy &= ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE;
            }
            if (k1_flags_copy != k2_flags_copy)
              return FALSE;

            // Struct and array maybe anonymous.
            // Do not compare their names if the 2 TY are both anonymous
            if (TY_kind(k1) == KIND_STRUCT || TY_kind(k1) == KIND_ARRAY) {
                if (TY_anonymous(k1) || TY_anonymous(k2)) {
                    if (TY_anonymous(k1) != TY_anonymous(k2))
                        return FALSE;
                }
                else {
                    if (k1.name_idx != k2.name_idx)
                        return FALSE;
                }
            }
            if (k1.u2.etype == k2.u2.etype)
                return TRUE;
            // The alignment of TY_POINTER may be changed
            // after the pointed incomplete struct is upadted.
            // So compare TY.u2 without last 3 bits if the TY is pointer.
            return TY_kind(k1) == KIND_POINTER &&
                   TY_IDX_without_attribute(k1.u2.pointed) 
                   == TY_IDX_without_attribute(k2.u2.pointed);
        }
    };
}

// This type is used in old type merge phase, it should be removed when the old type merge is removed.
typedef hashtable<TY_IDX, TY, TY_HASH, TY_EXTRACT_KEY, equal_to<TY>,
		  mempool_allocator<TY_IDX> > TY_HASH_TABLE;

// This is the new type hash table used in new type merge phase
typedef hashtable<TY_IDX, TY, TY_HASH, TY_EXTRACT_KEY, TY_IS_EQUIVALENT,
                  mempool_allocator<TY_IDX> > NEW_TY_HASH_TABLE;


// We steal a bit from the XXX_IDX to distinguish between real IDX and the
// raw index to the tables in the file
namespace
{

    template <class IDX>
    inline BOOL Is_File_Idx (IDX idx) {
	return (idx & 0x80000000);
    }

    template <class IDX>
    inline IDX Get_Idx (IDX idx) {
	return (idx & ~0x80000000);
    }

    template <class IDX>
    inline IDX File_Idx (IDX idx) {
	return (idx | 0x80000000);
    }
}

inline BOOL
FLD_is_anonymous (const FLD *fld)  { return fld->flags & FLD_IS_ANONYMOUS; }

inline BOOL
FLD_last_field (const FLD *fld)  { return fld->flags & FLD_LAST_FIELD; }

// ======================================================================
// Hash table for FLD
// ======================================================================
namespace
{

    // function objects for the fld hash tables

    struct FLD_HASH {
	size_t operator() (FLD_IDX key) const {
	    size_t value = 0;
	    if (Is_File_Idx (key)) {
		const FLD* fld = fld_table + Get_Idx (key);
                if (IPA_Enable_Old_Type_Merge) {
                    do {
                        value +=
                        Get_Kid_TY_IDX (fld->type) + (*str_map)[fld->name_idx];
                    } while ((fld++->flags & FLD_LAST_FIELD) == 0);
                }
                else {
                    do {
                        TY_IDX mapped_ty_idx = Get_Kid_TY_IDX(fld->type);
                        // Do not care the last 3 bits of a TY
                        // because they may be changed
                        value += TY_IDX_without_attribute(mapped_ty_idx);
                        if (!FLD_is_anonymous(fld))
                            value += (*str_map)[fld->name_idx];
                   } while ((fld++->flags & FLD_LAST_FIELD) == 0);
                }
		return value;
	    } else {
		FLD_HANDLE fld (key);
		FLD_ITER fld_iter = Make_fld_iter (fld);
		do {
		    fld = fld_iter;
                    if (IPA_Enable_Old_Type_Merge) {
                        value += FLD_type (fld) + FLD_name_idx (fld);
                    }
                    else {
                        // Do not care the last 3 bits of a TY
                        // because they may be changed
                        value += TY_IDX_without_attribute(FLD_type (fld));
                        if (!FLD_is_anonymous(fld))
                            value += FLD_name_idx(fld);
                    }
		    ++fld_iter;
		} while (! FLD_last_field (fld));
		return value;
	    }
	}

    }; // FLD_HASH

    
    // bitwise comparison of two structs
    struct FLD_IS_EQUIVALENT {
	BOOL operator() (FLD_IDX k1, FLD_IDX k2) const {

	    if (k1 == k2)
		return TRUE;

	    if (!Is_File_Idx (k1) && !Is_File_Idx (k2))
		return k1 == k2;

	    // cannot have both being indices to the table in file
	    Is_True (!Is_File_Idx (k1) || !Is_File_Idx (k2),
		     ("Invalid FLD_IDX in fld hash table"));
	    
	    FLD_HANDLE merged_fld;
	    const FLD* new_fld;

	    if (Is_File_Idx (k1)) {
		new_fld = fld_table + Get_Idx (k1);
		merged_fld = FLD_HANDLE (k2);
	    } else {
		new_fld = fld_table + Get_Idx (k2);
		merged_fld = FLD_HANDLE (k1);
	    }

	    FLD_ITER fld_iter = Make_fld_iter (merged_fld);
	    do {
		merged_fld = fld_iter;

                if (IPA_Enable_Old_Type_Merge) {
                    if (FLD_type (merged_fld) != (*ty_map)[new_fld->type] ||
		        FLD_name_idx (merged_fld) != (*str_map)[new_fld->name_idx])
		        return FALSE;
                }
                else {
                    if (TY_IDX_without_attribute(FLD_type (merged_fld)) !=
                        TY_IDX_without_attribute((*ty_map)[new_fld->type]))
                        return FALSE;
                    if (FLD_is_anonymous(merged_fld) || FLD_is_anonymous((FLD *)new_fld)) {
                        if (FLD_is_anonymous(merged_fld) != FLD_is_anonymous((FLD *)new_fld))
                            return FALSE;
                    }
                    else {
                        if (FLD_name_idx (merged_fld) != (*str_map)[new_fld->name_idx])
                            return FALSE;
                    }
                }

		const UINT64* p1 = reinterpret_cast<const UINT64*> (new_fld);
		const UINT64* p2 =
		    reinterpret_cast<const UINT64*> (merged_fld.Entry ());

		if (p1[1] != p2[1] || p1[2] != p2[2])
		    return FALSE;

		++fld_iter;
		++new_fld;
	    } while (! FLD_last_field (merged_fld));

	    return TRUE;
	}
    }; // FLD_IS_EQUIVALENT
} // namespace

typedef hashtable<FLD_IDX, FLD_IDX, FLD_HASH, identity<FLD_IDX>,
		  FLD_IS_EQUIVALENT, mempool_allocator<FLD_IDX> > FLD_HASH_TABLE;


// ======================================================================
// Hash table for ARB
// ======================================================================
namespace
{

    // function objects for the arb hash tables

    struct ARB_HASH {
	size_t operator() (ARB_IDX key) const {
	    size_t value = 0;
	    if (Is_File_Idx (key)) {
		const ARB* arb = arb_table + Get_Idx (key);
		UINT dim = arb->dimension;
		value = arb->flags + dim;
		for (UINT i = 0; i < dim; ++i) {
		    value += (arb->Lbnd_val () + arb->Ubnd_val () +
			      arb->Stride_val ()) << i;
#ifdef KEY // bug 9181
		    ++arb;
#endif
		} 
	    } else {
		ARB_ITER arb_iter = Make_arb_iter (ARB_HANDLE (key));
		UINT dim = ARB_dimension (arb_iter);
		value = ARB_flags (arb_iter) + dim;
		for (UINT i = 0; i < dim; ++i) {
		    ARB_HANDLE arb (arb_iter);
		    ++arb_iter;
		    value += (ARB_lbnd_val (arb) + ARB_ubnd_val (arb) +
			      ARB_stride_val (arb)) << i;
		} 
	    }
	    return ~value;
	}
    }; // ARB_HASH

    
    // bitwise comparison of two structs
    struct ARB_IS_EQUIVALENT {
	BOOL operator() (ARB_IDX k1, ARB_IDX k2) const {

	    if (k1 == k2)
		return TRUE;

	    if (!Is_File_Idx (k1) && !Is_File_Idx (k2))
		return k1 == k2;

	    // cannot have both being indices to the table in file
	    Is_True (!Is_File_Idx (k1) || !Is_File_Idx (k2),
		     ("Invalid ARB_IDX in arb hash table"));
	    
	    ARB_HANDLE merged_arb;
	    const ARB* new_arb;

	    if (Is_File_Idx (k1)) {
		new_arb = arb_table + Get_Idx (k1);
		merged_arb = ARB_HANDLE (k2);
	    } else {
		new_arb = arb_table + Get_Idx (k2);
		merged_arb = ARB_HANDLE (k1);
	    }

	    ARB_ITER arb_iter = Make_arb_iter (merged_arb);
	    UINT dim = new_arb->dimension;
	    for (UINT i = 0; i < dim; ++i, ++arb_iter) {
		merged_arb = arb_iter;

		const UINT64* p1 =
		    reinterpret_cast<const UINT64*> (new_arb + i);
		const UINT64* p2 =
		    reinterpret_cast<const UINT64*> (merged_arb.Entry ());

		if (p1[0] != p2[0] ||
		    p1[1] != p2[1] ||
		    p1[2] != p2[2] ||
		    p1[3] != p2[3])
		    return FALSE;

	    }

	    return TRUE;
	}
    }; // ARB_IS_EQUIVALENT
} // namespace


typedef hashtable<ARB_IDX, ARB_IDX, ARB_HASH, identity<ARB_IDX>,
		  ARB_IS_EQUIVALENT, mempool_allocator<ARB_IDX> > ARB_HASH_TABLE;


// ======================================================================
// Hash table for TYLIST
// ======================================================================
namespace
{

    // function objects for the tylist hash tables

    struct TYLIST_HASH {
	size_t operator() (TYLIST_IDX key) const {
	    size_t value = 0;
	    if (Is_File_Idx (key)) {
		const TYLIST* tylist = tylist_table + Get_Idx (key);
                if (IPA_Enable_Old_Type_Merge) {
                    while (*tylist != 0) {
                        value += Get_Kid_TY_IDX (*tylist);
                        ++tylist;
                    }
                }
                else {
                    while (*tylist != 0) {
                        value += TY_IDX_without_attribute(Get_Kid_TY_IDX(*tylist));
                        ++tylist;
                    }
                }
	    } else {
		TYLIST_ITER tylist_iter = Make_tylist_iter (key);
                if (IPA_Enable_Old_Type_Merge) {
                    while (*tylist_iter != 0) {
                        value += *tylist_iter;
                        ++tylist_iter;
                    }
                }
                else {
                    while (*tylist_iter != 0) {
                        value += TY_IDX_without_attribute(*tylist_iter);;
                        ++tylist_iter;
                    }
                }
	    }
	    return (~value);
	}
    }; // TYLIST_HASH

    
    // bitwise comparison of two structs
    struct TYLIST_IS_EQUIVALENT {
	BOOL operator() (TYLIST_IDX k1, TYLIST_IDX k2) const {

	    if (k1 == k2)
		return TRUE;

	    if (!Is_File_Idx (k1) && !Is_File_Idx (k2))
		return k1 == k2;

	    // cannot have both being indices to the table in file
	    Is_True (!Is_File_Idx (k1) || !Is_File_Idx (k2),
		     ("Invalid TYLIST_IDX in tylist hash table"));
	    
	    TYLIST_ITER merged_tylist;
	    const TYLIST* new_tylist;

	    if (Is_File_Idx (k1)) {
		new_tylist = tylist_table + Get_Idx (k1);
		merged_tylist = Make_tylist_iter (k2);
	    } else {
		new_tylist = tylist_table + Get_Idx (k2);
		merged_tylist = Make_tylist_iter (k1);
	    }

	    while (*new_tylist != 0) {
                if (IPA_Enable_Old_Type_Merge) {
		    if ((*ty_map)[*new_tylist] != *merged_tylist)
		        return FALSE;
                }
                else {
                    // First, compare the TY without alignment
                    if (TY_IDX_without_attribute((*ty_map)[*new_tylist])
                        != TY_IDX_without_attribute(*merged_tylist))
                        return FALSE;
                    // If the alignments of the 2 TYs are different 
                    // and they are both larger than 1, the 2 TYLIST are different
                    if (TY_align((*ty_map)[*new_tylist]) != TY_align(*merged_tylist)
                        && TY_align((*ty_map)[*new_tylist]) > 1
                        && TY_align(*merged_tylist) > 1)
                        return FALSE;
                }
		++new_tylist;
		++merged_tylist;
	    }
	    return (*merged_tylist ? FALSE : TRUE);
	}
    }; // TYLIST_IS_EQUIVALENT
} // namespace


typedef hashtable<TYLIST_IDX, TYLIST_IDX, TYLIST_HASH, identity<TYLIST_IDX>,
    TYLIST_IS_EQUIVALENT, mempool_allocator<TYLIST_IDX> > TYLIST_HASH_TABLE;

// ======================================================================
// The following hash_multimap is for obtaining a list of TY's that match
// the given ty except the indices
// ======================================================================

// The old Partial_Compare_Fld is for 1 file based TY and 1 global merged TY. 
// This new Partial_Compare_Fld is for 2 TYs those can be either file based or global merged.
BOOL
Partial_Compare_Fld (const TY &ty1, BOOL is_file1, const TY &ty2, BOOL is_file2)
{  
    UINT i = 0; 
    
    while (1) {        
        const FLD *fld1 = is_file1 ? (fld_table+ty1.Fld()+i) : &Fld_Table[ty1.Fld()+i];
        const FLD *fld2 = is_file2 ? (fld_table+ty2.Fld()+i) : &Fld_Table[ty2.Fld()+i];
       
       if (FLD_is_anonymous(fld1) || FLD_is_anonymous(fld2)) {
            if (FLD_is_anonymous(fld1) != FLD_is_anonymous(fld2))
                return FALSE;
        }       
        else {                    
            STR_IDX name_str1 = is_file1 ? (*str_map)[fld1->name_idx] : fld1->name_idx;
            STR_IDX name_str2 = is_file2 ? (*str_map)[fld2->name_idx] : fld2->name_idx;
            if (name_str1 != name_str2)
                return FALSE;
        }

        const UINT64* p1 = reinterpret_cast<const UINT64*> (fld1);
        const UINT64* p2 = reinterpret_cast<const UINT64*> (fld2);
                                                                   
        if (p1[1] != p2[1] || p1[2] != p2[2])
            return FALSE;

        if (FLD_last_field(fld1))
            break;

        ++i;
    } 
                           
    return TRUE;
}             

BOOL
Partial_Compare_Fld (FLD_HANDLE merged_fld, const FLD* new_fld)
{
    FLD_ITER fld_iter = Make_fld_iter (merged_fld);

    do {
        if (IPA_Enable_Old_Type_Merge) {
	    if (FLD_name_idx (fld_iter) != (*str_map)[new_fld->name_idx])
	        return FALSE;
        }
        else {
            if (FLD_is_anonymous(fld_iter) || FLD_is_anonymous(new_fld)) {
                if (FLD_is_anonymous(fld_iter) != FLD_is_anonymous(new_fld))
                    return FALSE;
            }
            else {
                if (FLD_name_idx(fld_iter) != (*str_map)[new_fld->name_idx])
                    return FALSE;
            }            
        }

	const UINT64* p1 = reinterpret_cast<const UINT64*> (&(*fld_iter));
	const UINT64* p2 = reinterpret_cast<const UINT64*> (new_fld);

	if (p1[1] != p2[1] || p1[2] != p2[2])
	    return FALSE;
	++fld_iter;
    } while ((new_fld++->flags & FLD_LAST_FIELD) == 0);

    return TRUE;
} 

// The old ARB_equal is for 1 file based TY and 1 global merged TY.
// This new ARB_equal is for 2 TYs those can be either file based or global merged.
static inline BOOL
ARB_equal (const ARB* arb1, const ARB* arb2)
{
    const UINT64* p1 =  reinterpret_cast<const UINT64*> (arb1);
    const UINT64* p2 =  reinterpret_cast<const UINT64*> (arb2);

    return (p1[0] == p2[0] &&
            p1[1] == p2[1] &&
            p1[2] == p2[2] &&
            p1[3] == p2[3]);
}

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

// The old Partial_Compare_Arb is for 1 file based TY and 1 global merged TY.
// This new Partial_Compare_Arb is for 2 TYs those can be either file based or global merged.
BOOL
Partial_Compare_Arb (const TY &ty1, BOOL is_file1, const TY &ty2, BOOL is_file2)
{
    const ARB *arb1 = is_file1 ? (arb_table+ty1.Arb()) : &Arb_Table[ty1.Arb()]; 
    const ARB *arb2 = is_file2 ? (arb_table+ty2.Arb()) : &Arb_Table[ty2.Arb()];
    
    UINT dim = arb1->dimension;

    if (arb2->dimension != dim)
        return FALSE;

    for (UINT i = 0; i < dim; ++i) {
         arb1 = is_file1 ? (arb_table+ty1.Arb()+i) : &Arb_Table[ty1.Arb()+i];
         arb2 = is_file2 ? (arb_table+ty2.Arb()+i) : &Arb_Table[ty2.Arb()+i];
         if (!ARB_equal (arb1, arb2))
             return FALSE;
    }
    return TRUE;
}

BOOL
Partial_Compare_Arb (ARB_HANDLE merged_arb, const ARB* new_arb)
{
    UINT dim = new_arb->dimension;
    
    if (ARB_dimension (merged_arb) != dim)
	return FALSE;

    for (UINT i = 0; i < dim; ++i) {
	if (!ARB_equal (merged_arb[i], new_arb[i]))
	    return FALSE;
    }
    return TRUE;
}


namespace
{

    // This is the new hash function of recursive table
    size_t recursive_ty_hash (TY_INDEX idx) {
        const TY& ty = Is_File_Idx (idx) ?
                       ty_table[Get_Idx (idx)] : Ty_Table[make_TY_IDX (idx)];
        const UINT32* p = reinterpret_cast<const UINT32*> (&ty);
        // TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE flag may be set on
        // this type, but should not affect type merging.  Strip out
        // that flag value before computing hash value
        // TY_flags is 10th word in TY entry
        UINT32 p_2 = p[2] & ~(TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE << 16);
        size_t value = p[0] + p[1] + p_2;
        switch (TY_kind (ty)) {
            case KIND_SCALAR:
            case KIND_VOID:
            default:
                Fail_FmtAssertion ("Unexpected TY_kind in recursive type %d\n",
                                    TY_kind (ty));
 
            case KIND_POINTER:
            {
                const TY& pointed = Is_File_Idx (idx) ?
                                    ty_table[TY_IDX_index (TY_pointed (ty))] :
                                    Ty_Table[TY_pointed (ty)];
                return ~(value + pointed.kind + pointed.mtype);
            }
            case KIND_ARRAY:
                if (Is_File_Idx (idx)) {
                    const ARB* arb = arb_table + ty.Arb ();
                    UINT dim = arb->dimension;
                    value = arb->flags + dim;
                    for (UINT i = 0; i < dim; ++i) {
                        value += (arb->Lbnd_val () + arb->Ubnd_val () +
                                  arb->Stride_val ()) << i;
                    }
                } else {
                    ARB_ITER arb_iter = Make_arb_iter (TY_arb (ty));
                    UINT dim = ARB_dimension (arb_iter);
                    value = ARB_flags (arb_iter) + dim;
                    for (UINT i = 0; i < dim; ++i) {
                        ARB_HANDLE arb (arb_iter);
                        ++arb_iter;
                        value += (ARB_lbnd_val (arb) + ARB_ubnd_val (arb) +
                        ARB_stride_val (arb)) << i;
                    }
                }
                return ~value;
           
                case KIND_STRUCT:
                    if (ty.Fld () == 0)
                        return ~value;
                    if (Is_File_Idx (idx)) {
                        if (!TY_anonymous(ty))
                           value += (*str_map)[TY_name_idx (ty)];
                        const FLD* fld = fld_table + ty.Fld ();
                        do {
                            if (!FLD_is_anonymous((FLD *)fld))
                                value += (*str_map)[fld->name_idx];
                        } while ((fld++->flags & FLD_LAST_FIELD) == 0);
                    } else {
                        if (!TY_anonymous(ty))
                            value += TY_name_idx (ty);
                        FLD_ITER fld_iter = Make_fld_iter (TY_fld (ty));
                        BOOL done = FALSE;
                        do {
                            if (!FLD_is_anonymous(fld_iter))
                                value += FLD_name_idx (fld_iter);
                            done = FLD_last_field (fld_iter);
                            ++fld_iter;
                        } while (!done);
                    }
                    return ~value;

                case KIND_FUNCTION:
                    if (Is_File_Idx (idx)) {
                        value += (*str_map)[TY_name_idx (ty)];
                        const TYLIST* tylist = tylist_table + TY_tylist (ty);
                        while (*tylist != 0) {
                            const TY* parm_ty = ty_table + TY_IDX_index (*tylist);
                            if (TY_kind(*parm_ty) != KIND_STRUCT) {
                                p = reinterpret_cast<const UINT32*> (parm_ty);
                                value += (p[0] + p[1] + p[2]);
                            }
                            ++tylist;
                        }
                    } else {
                        value += TY_name_idx (ty);
                        TYLIST_ITER tylist = Make_tylist_iter (TY_tylist (ty));
                        while (*tylist != 0) {
                            const TY& parm_ty = Ty_Table[*tylist];
                            if (TY_kind(parm_ty) != KIND_STRUCT) {
                                p = reinterpret_cast<const UINT32*> (&parm_ty);
                                value += (p[0] + p[1] + p[2]);
                            }
                            ++tylist;
                        }
                    }
                    return ~value;
            }
    }

    struct partial_ty_hash {
	size_t operator() (TY_INDEX idx) const {

            if (!IPA_Enable_Old_Type_Merge) {
                // Use the new hash function.
                // The old one did not consider anonymous TYs and fields,
                // changed pointer alignment, etc.
                return recursive_ty_hash(idx);            
            }

	    const TY& ty = Is_File_Idx (idx) ?
		ty_table[Get_Idx (idx)] : Ty_Table[make_TY_IDX (idx)];

	    const UINT32* p = reinterpret_cast<const UINT32*> (&ty);
            UINT32 p_2 = p[2] & ~(TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE << 16);
	    size_t value = p[0] + p[1] + p_2;

	    switch (TY_kind (ty)) {
	    case KIND_SCALAR:
	    case KIND_VOID:
	    default:
		Fail_FmtAssertion ("Unexpected TY_kind in recursive type %d\n",
				   TY_kind (ty));

	    case KIND_POINTER:
		{
		    const TY& pointed = Is_File_Idx (idx) ?
			ty_table[TY_IDX_index (TY_pointed (ty))] :
			Ty_Table[TY_pointed (ty)];
		    p = reinterpret_cast<const UINT32*> (&pointed);
		    return ~(value + (p[0] + p[1] + p[2]));
		}
		
	    case KIND_ARRAY:
		if (Is_File_Idx (idx)) {
		    const ARB* arb = arb_table + ty.Arb ();
		    UINT dim = arb->dimension;
		    value = arb->flags + dim;
		    for (UINT i = 0; i < dim; ++i) {
		    value += (arb->Lbnd_val () + arb->Ubnd_val () +
			      arb->Stride_val ()) << i;
		    }
		} else {
		    ARB_ITER arb_iter = Make_arb_iter (TY_arb (ty));
		    UINT dim = ARB_dimension (arb_iter);
		    value = ARB_flags (arb_iter) + dim;
		    for (UINT i = 0; i < dim; ++i) {
			ARB_HANDLE arb (arb_iter);
			++arb_iter;
			value += (ARB_lbnd_val (arb) + ARB_ubnd_val (arb) +
				  ARB_stride_val (arb)) << i;
		    } 
		}
		return ~value;
		
		
	    case KIND_STRUCT:
		if (ty.Fld () == 0)
		    return ~value;
		
		if (Is_File_Idx (idx)) {
		    value += (*str_map)[TY_name_idx (ty)];
		    const FLD* fld = fld_table + ty.Fld ();
		    do {
			value += (*str_map)[fld->name_idx];
		    } while ((fld++->flags & FLD_LAST_FIELD) == 0);
		} else {
		    value += TY_name_idx (ty);
		    FLD_ITER fld_iter = Make_fld_iter (TY_fld (ty));
		    BOOL done = FALSE;
		    do {
			value += FLD_name_idx (fld_iter);
			done = FLD_last_field (fld_iter);
			++fld_iter;
		    } while (!done);
		}
		return ~value;
		    
	    case KIND_FUNCTION:
		if (Is_File_Idx (idx)) {
		    value += (*str_map)[TY_name_idx (ty)];
		    const TYLIST* tylist = tylist_table + TY_tylist (ty);
		    while (*tylist != 0) {
			const TY* parm_ty = ty_table + TY_IDX_index (*tylist);
			p = reinterpret_cast<const UINT32*> (parm_ty);
			value += (p[0] + p[1] + p[2]);
			++tylist;
		    }
		} else {
		    value += TY_name_idx (ty);
		    TYLIST_ITER tylist = Make_tylist_iter (TY_tylist (ty));
		    while (*tylist != 0) {
			const TY& parm_ty = Ty_Table[*tylist];
			p = reinterpret_cast<const UINT32*> (&parm_ty);
			value += (p[0] + p[1] + p[2]);
			++tylist;
		    }
		}
		return ~value;
	    }
	}
    }; // partial_ty_hash

    
    struct ty_index_compare {
	BOOL operator() (TY_INDEX idx1, TY_INDEX idx2) const {

            if (IPA_Enable_Old_Type_Merge) {
                if (idx1 == idx2)
		    return TRUE;

	        if (!Is_File_Idx (idx1) && !Is_File_Idx (idx2))
		    return idx1 == idx2;

	        Is_True (!Is_File_Idx (idx1) || !Is_File_Idx (idx2),
		         ("Invalid TY_INDEX in recursive_ty_hash_table"));

	        const TY& new_ty = Is_File_Idx (idx1) ? ty_table[Get_Idx (idx1)] :
		    ty_table[Get_Idx (idx2)];
	        const TY& merged_ty = Is_File_Idx (idx1) ?
		    Ty_Table[make_TY_IDX (idx2)] : Ty_Table[make_TY_IDX (idx1)];

	        if (TY_size (new_ty) != TY_size (merged_ty) ||
		    (*str_map)[TY_name_idx (new_ty)] != TY_name_idx (merged_ty))
		    return FALSE;


                if (TY_kind(new_ty) != TY_kind(merged_ty))
                  return FALSE;
                
                if (TY_mtype(new_ty) != TY_mtype(merged_ty))
                  return FALSE;
                
                if ((TY_flags(new_ty) & 
                     ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE) !=
                    (TY_flags(merged_ty) &
                     ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE))
                  return FALSE;

	        switch (TY_kind (new_ty)) {
	        case KIND_SCALAR:
	        case KIND_VOID:
	        default:
		    Fail_FmtAssertion ("Unexpected TY_kind in recursive type %d\n",
		                       TY_kind (new_ty));

	        case KIND_POINTER:
		    return (TY_IDX_Attributes (TY_pointed (new_ty)) ==
		    	    TY_IDX_Attributes (TY_pointed (merged_ty)));
		
	        case KIND_ARRAY:
		    if (TY_IDX_Attributes (TY_etype (new_ty)) !=
		        TY_IDX_Attributes (TY_etype (merged_ty)))
		        return FALSE;
		    return Partial_Compare_Arb (TY_arb (merged_ty),
					        arb_table + new_ty.Arb ());

	        case KIND_STRUCT:
		    if (new_ty.Fld () == 0 || merged_ty.Fld () == 0)
		        return new_ty.Fld () == merged_ty.Fld ();
		    return Partial_Compare_Fld (TY_fld (merged_ty),
					        fld_table + new_ty.Fld ());
		
	        case KIND_FUNCTION:
		    return (new_ty.Pu_flags () == merged_ty.Pu_flags ());
	        }
            }
            else { // new type merge
                if (idx1 == idx2)
                    return TRUE;
            
                if (recursive_ty_hash(idx1) != recursive_ty_hash(idx2))
                    return FALSE;
            
                const TY& ty1 = Is_File_Idx (idx1) ? ty_table[Get_Idx (idx1)] : Ty_tab[idx1];
                const TY& ty2 = Is_File_Idx (idx2) ? ty_table[Get_Idx (idx2)] : Ty_tab[idx2];

                if (TY_size (ty1) != TY_size (ty2) || TY_kind (ty1) != TY_kind (ty2))
                    return FALSE;
               
                if (TY_kind(ty1) == KIND_STRUCT || TY_kind(ty1) == KIND_ARRAY) {
                    if (TY_anonymous(ty1) || TY_anonymous(ty2)) {
                        if (TY_anonymous(ty1) != TY_anonymous(ty2))
                            return FALSE; 
                    }   
                    else {
                        STR_IDX name_str1 = Is_File_Idx (idx1) ? 
                                            (*str_map)[TY_name_idx (ty1)] : TY_name_idx (ty1);
                        STR_IDX name_str2 = Is_File_Idx (idx2) ? 
                                            (*str_map)[TY_name_idx (ty2)] : TY_name_idx (ty2);
                        if (name_str1 != name_str2)
                            return FALSE;
                    }
                }
                else {
                    STR_IDX name_str1 = Is_File_Idx (idx1) ?
                                        (*str_map)[TY_name_idx (ty1)] : TY_name_idx (ty1);
                    STR_IDX name_str2 = Is_File_Idx (idx2) ?
                                        (*str_map)[TY_name_idx (ty2)] : TY_name_idx (ty2);
                    if (name_str1 != name_str2)
                        return FALSE;
                }
          
                if (TY_mtype(ty1) != TY_mtype(ty2))
                  return FALSE;
                
                // byte 10 contains TY_flags.  Strip out
                // TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE flag which
                // should not influence type merging
                if (TY_flags(ty1) & ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE !=
                    TY_flags(ty2) & ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE)
                  return FALSE;
          
                switch (TY_kind (ty1)) {
                    case KIND_SCALAR:
                    case KIND_VOID:
                    default:
                        Fail_FmtAssertion ("Unexpected TY_kind in recursive type %d\n",
                                           TY_kind (ty1));
                    case KIND_POINTER:
                        return TRUE;
                    case KIND_ARRAY:
                        if (TY_IDX_Attributes (TY_etype (ty1)) != TY_IDX_Attributes (TY_etype (ty2)))
                            return FALSE;
                        return Partial_Compare_Arb (ty1, Is_File_Idx(idx1), ty2, Is_File_Idx(idx2));
                    case KIND_STRUCT:
                        if (ty1.Fld () == 0 || ty2.Fld () == 0)
                            return ty1.Fld () == ty2.Fld ();
                        return Partial_Compare_Fld (ty1, Is_File_Idx(idx1), ty2, Is_File_Idx(idx2));
                    case KIND_FUNCTION:
                        return (ty1.Pu_flags () == ty2.Pu_flags ());
                }
            }
	}
    }; // ty_index_compare
} // namespace


typedef hash_multiset<TY_INDEX, partial_ty_hash, ty_index_compare,
		      mempool_allocator<TY_INDEX> > RECURSIVE_TY_HASH_TABLE;

// ======================================================================


static NEW_TY_HASH_TABLE* ty_hash_table;
static FLD_HASH_TABLE* fld_hash_table;
static ARB_HASH_TABLE* arb_hash_table;
static TYLIST_HASH_TABLE* tylist_hash_table;

static RECURSIVE_TY_HASH_TABLE* recursive_table;

typedef vector<TY_IDX, mempool_allocator<TY_IDX> > RECURSIVE_TYPE;

// These 2 variables are only for old type merge, they should be removed when the old type merge is removed. 
static RECURSIVE_TYPE *recursive_type;	// temp. record all TY_IDX of a
					// newly inserted recursive type
static UINT collecting_recursive_ty = 0;

void
Initialize_Type_Merging_Hash_Tables (MEM_POOL* pool)
{
    ty_hash_table = CXX_NEW (NEW_TY_HASH_TABLE (1000, TY_HASH (), TY_IS_EQUIVALENT(), 
					    TY_EXTRACT_KEY (), pool),
			     pool);
    fld_hash_table = CXX_NEW (FLD_HASH_TABLE (100, FLD_HASH (),
					      FLD_IS_EQUIVALENT(),
					      identity<FLD_IDX>(), pool),
			      pool);
    arb_hash_table = CXX_NEW (ARB_HASH_TABLE (100, ARB_HASH (),
					      ARB_IS_EQUIVALENT(),
					      identity<ARB_IDX>(), pool),
			      pool);
    tylist_hash_table = CXX_NEW (TYLIST_HASH_TABLE (100, TYLIST_HASH (),
						    TYLIST_IS_EQUIVALENT(), 
						    identity<TYLIST_IDX>(),
						    pool),
				 pool);

    recursive_table =
	CXX_NEW (RECURSIVE_TY_HASH_TABLE (500, partial_ty_hash (),
					  ty_index_compare (), pool),
		 pool);

    for (UINT i = 1; i < TY_Table_Size (); ++i)
	ty_hash_table->find_or_insert (make_TY_IDX (i));

    if (IPA_Enable_Old_Type_Merge)
        recursive_type = CXX_NEW (RECURSIVE_TYPE (pool), pool);

} // Initialize_Type_Merging_Hash_Tables



void
Setup_Type_Merging_Hash_Tables (const IPC_GLOBAL_TABS& original_tabs,
				IPC_GLOBAL_IDX_MAP& idx_map)
{
    ty_map = &idx_map.ty;
    str_map = &idx_map.sym_str;

    ty_table = original_tabs.ty_tab;
    fld_table = original_tabs.fld_tab;
    arb_table = original_tabs.arb_tab;
    tylist_table = original_tabs.tylist_tab;
} // Setup_Type_Merging_Hash_Tables


static void
Setup_Ty (TY& ty)
{
    Set_TY_name_idx (ty, (*str_map)[TY_name_idx (ty)]);

    switch (TY_kind (ty)) {
	ARB_IDX* arb_idx;
	FLD_IDX* fld_idx;
	TYLIST_IDX* tylist_idx;
	
    case KIND_SCALAR:
    case KIND_VOID:
	break;

    case KIND_ARRAY:
	arb_idx = &(arb_hash_table->find_or_insert (File_Idx (ty.Arb ())));
	if (Is_File_Idx (*arb_idx)) {
	    const ARB* arb = arb_table + ty.Arb ();
	    *arb_idx = Arb_Table.Insert (*arb);
	    while ((arb->flags & ARB_LAST_DIMEN) == 0) {
		Arb_Table.Insert (*++arb);
	    }
	}
	Set_TY_arb (ty, ARB_HANDLE (*arb_idx));
	Set_TY_etype (ty, Get_Kid_TY_IDX (TY_etype (ty)));
	break;

    case KIND_STRUCT:
	if (ty.Fld () == 0)
	    break;
	fld_idx = &(fld_hash_table->find_or_insert (File_Idx (ty.Fld ())));
	if (Is_File_Idx (*fld_idx)) {
	    const FLD* fld = fld_table + ty.Fld();
	    *fld_idx = Fld_Table.Insert (*fld);
	    for (FLD_HANDLE merged_fld (*fld_idx);;) {
		Set_FLD_name_idx (merged_fld, (*str_map)[fld->name_idx]);
		Set_FLD_type (merged_fld, Get_Kid_TY_IDX (fld->type));
		Set_FLD_st (merged_fld, 0);
		if (FLD_last_field (merged_fld))
		    break;
		++fld;
		merged_fld = FLD_HANDLE (Fld_Table.Insert (*fld));
	    }
	}
	Set_TY_fld (ty, FLD_HANDLE (*fld_idx));
	break;

    case KIND_POINTER:
	Set_TY_pointed (ty, Get_Kid_TY_IDX (TY_pointed (ty)));
	break;

    case KIND_FUNCTION:
	tylist_idx =
	    &(tylist_hash_table->find_or_insert (File_Idx (TY_tylist (ty))));
	if (Is_File_Idx (*tylist_idx)) {
	    const TYLIST* tylist = tylist_table + TY_tylist (ty);
	    *tylist_idx = Tylist_Table.Insert (Get_Kid_TY_IDX (*tylist));
	    ++tylist;
	    while (*tylist != 0) {
		Tylist_Table.Insert (Get_Kid_TY_IDX (*tylist));
		++tylist;
	    }
	    Tylist_Table.Insert (0);
	}
 	Set_TY_tylist (ty, *tylist_idx);
	break;

    default:
	Fail_FmtAssertion ("Invalid TY_kind");
    }
} // Setup_Ty


TY_IDX
Insert_Unique_Ty (const TY& ty)
{
    TY new_ty = ty;

    Setup_Ty (new_ty);

    ty_to_be_inserted = &new_ty;
    TY_IDX& result = ty_hash_table->find_or_insert ((TY_IDX) -1);
    if (result == (TY_IDX) -1) {
        result = make_TY_IDX (Ty_tab.Insert (new_ty));
        if (IPA_Enable_Old_Type_Merge) {
            if (collecting_recursive_ty)
	            recursive_type->push_back (result);
        }
    }
    return result;
}


void
Insert_Allocated_Ty (TY& ty, TY_IDX ty_idx)
{
    Setup_Ty (ty);
    ty_to_be_inserted = &ty;
    TY_IDX& result = ty_hash_table->find_or_insert ((TY_IDX) -1);

    if (IPA_Enable_Old_Type_Merge)
        Is_True (result == (TY_IDX) -1, ("Trying to insert duplicated TY"));
    result = ty_idx;
    if (!IPA_Enable_Old_Type_Merge) {
        if (TY_kind(ty) == KIND_STRUCT)
            struct_by_name_idx[ty.name_idx] = TY_IDX_index(ty_idx);
    }
}


// insert a TY into the recursive type hash table
void
Insert_Recursive_Type (TY_IDX ty_idx)
{
    const TY& ty = Ty_Table[ty_idx];

    if (!IPA_Enable_Old_Type_Merge) {
        if (TY_is_incomplete_struct(ty))
            return;
    }
    switch (TY_kind (ty)) {
    case KIND_POINTER:
    case KIND_STRUCT:
    case KIND_FUNCTION:
	recursive_table->insert (TY_IDX_index (ty_idx));
    }
}

// This function should be removed when the old type merge is removed.
void
Initialize_New_Recursive_Type (TY_IDX ty_idx)
{
    Is_True (collecting_recursive_ty || recursive_type->empty (),
	     ("Found uninserted recursive types"));

    ++collecting_recursive_ty;
    recursive_type->push_back (ty_idx);
} // Start_New_Recursive_Type


void
Finalize_New_Recursive_Type ()
{
    Is_True (collecting_recursive_ty, ("inconsistent recursive type"));

    --collecting_recursive_ty;

    if (collecting_recursive_ty != 0)
	return;

    for (RECURSIVE_TYPE::const_iterator i (recursive_type->begin ());
	 i != recursive_type->end (); ++i) {

	Insert_Recursive_Type (*i);
    }
    
    recursive_type->erase (recursive_type->begin (), recursive_type->end ());
    
} // Finalize_New_Recursive_Type


// go through the recursive hash table and find all the matching types
void
Find_Matching_Ty (const TY& ty, TY_IDX_VEC& matched_list)
{
    TY_INDEX idx = &ty - ty_table;

    typedef RECURSIVE_TY_HASH_TABLE::iterator ITER;

    pair<ITER, ITER> found = recursive_table->equal_range (File_Idx (idx));
    for (ITER first = found.first; first != found.second; ++first)
	matched_list.push_back (make_TY_IDX (*first));
}


