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


// ====================================================================
// ====================================================================
//
// Module: symtab_idx_map.cxx
//
// Description:
//
//    Enters all global symbol table entries into a unified merged
//    global symbol table, and returns maps for indices into the
//    old table, such that a client of this module can get hold of
//    the corresponding merged symbol table indices.
//
//    Note that we still enter all global symbols into the elf symbol
//    table, and we in turn inquire the elf symbol table about symbol
//    attributes determined by the symbol resolution mechanism in LD,
//    which takes into account ELF objects and DSOs in addition to the
//    WHIRL symbols.
//
//    A noteworthy characteristic of the Merge_xxx routines is that they
//    all return a IDX that have all the same flags as the original
//    IDX passed in as a parameter.
//
// ====================================================================
// ====================================================================
//

#include <stdint.h>
#include "linker.h"

#include "defs.h"
#include "cxx_memory.h"			// for CXX_NEW
#include "errors.h"
#include "irbdata.h"
#include "strtab.h"

#include "ld_ipa_interface.h"		// for ld_get_sym_attr ()
#include "ipc_defs.h"
#include "dwarf_DST_mem.h"              // Needed by ipc_file.h
#include "ipc_file.h"			// for IP_FILE_HDR
#include "ipc_weak.h"			// for tmpdir
#include "ipa_option.h"			// for ipa options
#include "ipc_type_merge.h"		// for type merging hash tables

#include "ipc_symtab_merge.h"

AUX_ST_TAB Aux_St_Tab;
AUX_ST_TABLE Aux_St_Table;
static UINT32 num_predefined_st;	// number of predefined symbols (pregs)


AUX_PU_TAB Aux_Pu_Table;

static IP_FILE_HDR* current_file_hdr;

#include "ipc_ty_hash.h"

// --------------------------------------------------------------
// Define accessors for index maps we need internally, but which
// are never exported.
// --------------------------------------------------------------
//
typedef STR_IDX_MAP    TCONSTR_IDX_MAP;
typedef hash_map<UINT32,UINT32> ST_TO_INITO_MAP;

// --------------------------------------------------------------
// Define the hash table for merging TCONS and CLASS_CONST STs
// --------------------------------------------------------------
struct eq_tcon
{
    bool operator () (const TCON* t1, const TCON* t2) const {
	if (TCON_ty (*t1) != TCON_ty (*t2))
	    return FALSE;
	if (t1->flags != t2->flags)
	    return FALSE;
	switch (TCON_ty (*t1)) {

	case MTYPE_I1:
	case MTYPE_I2:
	case MTYPE_I4:
	case MTYPE_I8:
	case MTYPE_U1:
	case MTYPE_U2:
	case MTYPE_U4:
	case MTYPE_U8:
	    return TCON_i0 (*t1) == TCON_i0 (*t2);

	case MTYPE_F4:
	    return TCON_ival (*t1) == TCON_ival (*t2);

	case MTYPE_F8:
	    return TCON_k0 (*t1) == TCON_k0 (*t2);
	    
	case MTYPE_STR:
	    return (TCON_str_idx (*t1) == TCON_str_idx (*t2) &&
		    TCON_str_len (*t1) == TCON_str_len (*t2));

	default:
	    return memcmp (t1, t2, sizeof(TCON)) == 0;
	}
    }
};


struct tcon_hash
{
    size_t operator() (const TCON* tcon) const {
	size_t val = TCON_ty (*tcon);
	val ^= TCON_ival (*tcon);
	return val;
    }
};

typedef hash_map<const TCON*, TCON_IDX, tcon_hash, eq_tcon,
    mempool_allocator<TCON_IDX> > TCON_MERGE_MAP;

struct eq_const_st
{
    bool operator() (const ST* st1, const ST* st2) const {
      return memcmp (st1, st2, sizeof(ST) - sizeof(ST_IDX) * 2 - sizeof(TY_IDX) - sizeof(mUINT32)) == 0;
    }
};

struct const_st_hash
{
    size_t operator() (const ST* st) const {
	Is_True (ST_class (st) == CLASS_CONST,
		 ("cannot merge non-constant STs"));
	return ST_tcon (st);
    }
};

typedef hash_map<const ST*, ST_IDX, const_st_hash, eq_const_st,
    mempool_allocator<ST_IDX> > ST_MERGE_MAP;

static TCON_MERGE_MAP* tcon_merge_map;
static ST_MERGE_MAP* st_merge_map;
static MEM_POOL merge_pool;

// --------------------------------------------------------------
// Define the index maps used in merging a symbol table.
// --------------------------------------------------------------
//
static SYMSTR_IDX_MAP  *New_Symstr_Idx;
static ST_IDX_MAP      *New_St_Idx;
static TY_IDX_MAP      *New_Ty_Idx;
static TCON_IDX_MAP    *New_Tcon_Idx;
static ST_TO_INITO_MAP *St_To_Inito_Map;

// map from ST_IDX to corresponding INITO -- used for constant propagation
ST_TO_INITO_MAP ST_To_INITO_Map;

// map from a common block ST_IDX to the array of its elements
COMMON_BLOCK_ELEMENTS_MAP *Common_Block_Elements_Map = NULL;
static MEM_POOL Common_map_pool;

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))

static UINT32
count_pregs()
{
    int i;
    ST* s;
    UINT32 result=0;
    FOREACH_SYMBOL (GLOBAL_SYMTAB, s, i) {
	if (ST_class (s) == CLASS_PREG)
	    ++result;
    }
    return result;
}

static ST*
get_global_st(char* symname)
{
    int i;
    ST* s;
    FOREACH_SYMBOL (GLOBAL_SYMTAB, s, i) {
	if ((*ST_name(s) == *symname) && (strcmp(ST_name(s), symname) == 0))
	    return s;
    }
    return NULL;
}
#endif // _STANDALONE_INLINER

//----------------------------------------------------------------------
// Auxiliary tables
//----------------------------------------------------------------------
void
Initialize_Auxiliary_Tables ()
{
    const UINT32 st_size = ST_Table_Size (GLOBAL_SYMTAB);
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
    num_predefined_st = count_pregs()+1;
#else // _STANDALONE_INLINER
    num_predefined_st = st_size;
#endif // _STANDALONE_INLINER
    
    Scope_tab[GLOBAL_SYMTAB].st_tab->Register (Aux_St_Tab);

    const AUX_PU aux_pu;
    Aux_Pu_Table.Insert (aux_pu);

    MEM_POOL_Initialize (&merge_pool, "tcon merge pool", FALSE);
    tcon_merge_map = CXX_NEW (TCON_MERGE_MAP (1000, tcon_hash (),
					      eq_tcon (), &merge_pool),
			      &merge_pool);
    st_merge_map = CXX_NEW (ST_MERGE_MAP (1000, const_st_hash (),
					  eq_const_st (), &merge_pool),
			    &merge_pool);

    MEM_POOL_Initialize (&Common_map_pool, "common map pool", FALSE);
    Common_Block_Elements_Map = 
      CXX_NEW (COMMON_BLOCK_ELEMENTS_MAP (100, __gnu_cxx::hash<ST_IDX> (),
					  std::equal_to<ST_IDX> (),
                                          &Common_map_pool), 
               &Common_map_pool);

} // Initailize_Auxiliary_Tables

void
Print_AUX_ST_flags ( FILE *fp, const mUINT32 flags )
{
	char c = '(';

	fprintf(fp, "\t\tAUX_ST_flag: %x ", flags );
	if ( !flags ) {
		fprintf(fp, "\n");
		return;
	}
	if ( flags & USED_IN_OBJ ) {
		fprintf(fp, "%cUSED_IN_OBJ", c);
		c = '|';
	}
	if ( flags & USED_IN_DSO ) {
		fprintf(fp, "%cUSED_IN_DSO", c);
		c = '|';
	}
	if ( flags & DEF_IN_OBJ ) {
		fprintf(fp, "%cDEF_IN_OBJ", c);
		c = '|';
	}
	if ( flags & DEF_IN_DSO ) {
		fprintf(fp, "%cDEF_IN_DSO", c);
		c = '|';
	}
	if ( flags & OBJ_COMMON ) {
		fprintf(fp, "%cOBJ_COMMON", c);
		c = '|';
	}
	if ( flags & ADDR_TAKEN_IN_OBJ ) {
		fprintf(fp, "%cADDR_TAKEN_IN_OBJ", c);
		c = '|';
	}
	if ( flags & COMMON_USED_IN_IO ) {
		fprintf(fp, "%cCOMMON_USED_IN_IO", c);
		c = '|';
	}
	fprintf(fp, ")\n");
} // Print_AUX_ST_flags


void
Clear_Extra_Auxiliary_Tables ()
{
    MEM_POOL_Delete (&merge_pool);
    tcon_merge_map = NULL;
    st_merge_map = NULL;
}

void
Clear_Common_Block_Element_Map ()
{
  if (Common_Block_Elements_Map) {
    MEM_POOL_Delete (&Common_map_pool);
    Common_Block_Elements_Map = NULL;
  }
}

// --------------------------------------------------------------
// Utility routines 
// --------------------------------------------------------------
//

// The following supplements definitions in the symbol table to
// access fields needed for the symbol table merge algorithm.
//
static inline
void Set_INITV_next(INITV &inv, INITV_IDX next)
{
   inv.next = next;
}

static inline
ST_IDX INITV_symoff_st( const INITV &inv)
{
   return inv.u.sto.st;
}

static inline
void Set_INITV_symoff_st(INITV &inv, ST_IDX st_idx)
{
   inv.u.sto.st = st_idx;
}

static inline
ST_IDX INITV_symdiff_st( const INITV &inv)
{
   return inv.u.stdiff.st2;
}

static inline
void Set_INITV_symdiff_st(INITV &inv, ST_IDX st_idx)
{
   inv.u.stdiff.st2 = st_idx;
}

static inline
INITV_IDX INITV_block_first( const INITV &inv)
{
   return inv.u.blk.blk;
}

static inline
void Set_INITV_block_first(INITV &inv, INITV_IDX first_member)
{
   inv.u.blk.blk = first_member;
}


static inline
TCON_IDX INITV_tcon(const INITV &inv)
{
   return inv.u.tcval.u.tc;
}

static inline
void Set_INITV_tcon(INITV &inv, TCON_IDX tcon_idx)
{
   inv.u.tcval.u.tc = tcon_idx;
}


static inline
void Set_TCON_string_ptr(TCON &tc, STR_IDX cp)
{
   tc.vals.sval.cp = cp;
}

static inline
STR_IDX TCON_string_ptr(const TCON &tc)
{
   return tc.vals.sval.cp;
}

static inline ST_IDX
ST_raw_base_idx (const ST& st)
{
    return st.base_idx;
}
static inline void
Set_ST_raw_base_idx (ST& st, ST_IDX base)
{
    st.base_idx = base;
}

static inline BOOL
FLD_last_field (const FLD* fld)
{
    return fld->flags & FLD_LAST_FIELD;
}
// --------------------------------------------------------------
// Routines for merging TYs and associated global symbol table
// records.
// --------------------------------------------------------------
//

static ST_IDX
Merge_Global_St(UINT                   idx,
		const IPC_GLOBAL_TABS& original_tabs); // Defined below.

static TY_IDX
Merge_Global_Ty(TY_IDX	              ty_idx,
		const IPC_GLOBAL_TABS &original_tabs,
		BOOL                  &created_new,
		TY_INDEX              &recursive_type); // Defined below.


// After inserting a recursive types, and if we found that there exists a
// duplicate, we need to reset the mappings to point to the new indices.
static void
Reset_recursive_type_mapping (TY_INDEX orig_idx, TY_IDX new_ty_idx,
			      const IPC_GLOBAL_TABS& original_tabs)
{
    if (New_Ty_Idx->is_merging (orig_idx))
	return;

    if (TY_IDX_index (New_Ty_Idx->map_[orig_idx]) == TY_IDX_index (new_ty_idx))
	return;

    New_Ty_Idx->set_map (orig_idx, new_ty_idx);
    New_Ty_Idx->set_is_merging (orig_idx);

    const TY& old_ty = original_tabs.ty_tab[orig_idx];
    const TY& new_ty = Ty_Table[new_ty_idx];

    switch (TY_kind (old_ty)) {

    case KIND_ARRAY:
	Reset_recursive_type_mapping (TY_IDX_index (TY_etype (old_ty)),
				      TY_etype (new_ty), original_tabs);
	break;

    case KIND_POINTER:
	Reset_recursive_type_mapping (TY_IDX_index (TY_pointed (old_ty)),
				      TY_pointed (new_ty), original_tabs);
	break;
	
    case KIND_FUNCTION:
	{
	    TYLIST_IDX old_tylist_idx = TY_tylist (old_ty);
	    TYLIST_IDX new_tylist_idx = TY_tylist (new_ty);

	    TYLIST old_tylist = original_tabs.tylist_tab[old_tylist_idx];
	    TYLIST new_tylist = Tylist_Table[new_tylist_idx];

	    do {
		
		TY_INDEX old_index = TY_IDX_index (TYLIST_type (old_tylist));
		TY_IDX new_index = TYLIST_type (new_tylist);
		
		Reset_recursive_type_mapping (old_index, new_index,
					      original_tabs);

		++old_tylist_idx;
		++new_tylist_idx;

		old_tylist = original_tabs.tylist_tab[old_tylist_idx];
		new_tylist = Tylist_Table[new_tylist_idx];
	    } while (TYLIST_type (old_tylist) != 0);

	    break;
	}

    case KIND_STRUCT:
	{
	    FLD_IDX old_fld_idx = old_ty.Fld ();
	    FLD_IDX new_fld_idx = new_ty.Fld ();

	    if (new_fld_idx == 0)	// forward declaration 
		break;

	    const FLD* old_fld;
	    const FLD* new_fld;

	    do {
		old_fld = &original_tabs.fld_tab[old_fld_idx];
		new_fld = &Fld_Table[new_fld_idx];

		TY_INDEX old_index = TY_IDX_index (old_fld->type);
		TY_IDX new_index = new_fld->type;

		Reset_recursive_type_mapping (old_index, new_index,
					      original_tabs);
		++old_fld_idx;
		++new_fld_idx;
	    } while (!FLD_last_field (old_fld));
	    
	    break;
	}
		
    default:
	break;
    }
	    
    New_Ty_Idx->clear_all_index_flags (orig_idx);

} // Reset_recursive_type_mapping


static ARB_IDX
Merge_Arb(ARB_IDX arb_idx, ARB *arb_tab)
{
   // ARB entries may only refer to one other kind of symbol table
   // entry, namely ST information local to a PU.  Hence we just
   // blindly copy ARB entries.
   //

    ARB_IDX new_idx = Arb_Table.Insert (arb_tab[arb_idx]);
    ARB_IDX idx=new_idx;

    while (!ARB_last_dimen (ARB_HANDLE(idx))) {
	++arb_idx; 
	idx = Arb_Table.Insert (arb_tab[arb_idx]);
    }

    return new_idx;
} // Merge_Arb


static TYLIST_IDX
Merge_Tylist(TYLIST_IDX            tylist_idx,
	     const IPC_GLOBAL_TABS &original_tabs,
	     BOOL                  &created_new,
	     TY_INDEX              &recursive_type)
{
    created_new = FALSE;	// True if *any* new type was created
    recursive_type = 0;		// True if *any* type being merged is encountered

    if (tylist_idx == 0)
	return 0;

    TYLIST_IDX result = TYLIST_Table_Size ();

    TYLIST* orig_tylist = &(original_tabs.tylist_tab[tylist_idx]);

    // copy all tylist entries so they are contiguous in Tylist_Table
    do {
	(void) Tylist_Table.Insert (*orig_tylist);
    } while (TYLIST_type (*orig_tylist++) != 0);

    orig_tylist = &(original_tabs.tylist_tab[tylist_idx]);
    TYLIST_IDX idx = result;

    while (TYLIST_type (*orig_tylist) != 0) {
	BOOL ty_is_created;
	TY_INDEX ty_is_recursive;
	TY_IDX new_ty_idx =
	    Merge_Global_Ty (TYLIST_type (*orig_tylist), original_tabs,
			     ty_is_created, ty_is_recursive);
	// Update TYLIST_IDX map and the TYLIST_type.
	//
	Set_TYLIST_type (Tylist_Table[idx], new_ty_idx);
      
	created_new = (created_new || ty_is_created);
	if (recursive_type == 0)
	    recursive_type = ty_is_recursive;
	else if (recursive_type > ty_is_recursive)
	    recursive_type = ty_is_recursive;
	    
	++idx;
	++orig_tylist;
    }

    return result;

} // Merge_Tylist


static FLD_HANDLE
Merge_Flds(FLD_IDX               fld_idx,
	   const IPC_GLOBAL_TABS &original_tabs,
	   BOOL                  &created_new,
	   TY_INDEX              &recursive_type)
{
    created_new = FALSE; // True if *any* new type was created
    recursive_type = 0;	// True if *any* type being merged is encountered

    if (fld_idx == 0)
	return FLD_HANDLE (0);

    FLD_IDX result = FLD_Table_Size ();

    // copy all fld entries so that they are contiguous in the Fld_Table
    UINT count = 0;
    const FLD* orig_fld = original_tabs.fld_tab + fld_idx;
    do {
	(void) Fld_Table.Insert (*orig_fld);
	++count;
    } while (!FLD_last_field (orig_fld++));

    FLD_ITER iter = Make_fld_iter (FLD_HANDLE (result));

    while (count--) {
	FLD_HANDLE fld (iter);

	STR_IDX new_name_idx = (*New_Symstr_Idx)[FLD_name_idx (fld)];
	Set_FLD_name_idx (fld, new_name_idx);
	
	BOOL ty_is_created;
	TY_INDEX ty_is_recursive;

	TY_IDX new_ty_idx = 
	    Merge_Global_Ty (FLD_type (fld), original_tabs, ty_is_created,
			     ty_is_recursive);

	Set_FLD_type (fld, new_ty_idx);

	created_new = (created_new || ty_is_created);
	if (ty_is_recursive) {
	    if (recursive_type == 0)
		recursive_type = ty_is_recursive;
	    else if (recursive_type > ty_is_recursive)
		recursive_type = ty_is_recursive;
	}
	
	// explicitly set the ST_IDX to 0, and let the symbol merge
	// routines to fill it in later.
	Set_FLD_st (fld, 0);
	++iter;
    }

    return FLD_HANDLE (result);

} // Merge_Flds

// Take a snapshot of all type related tables, in case we need to undo
struct CHECKPOINT
{
    UINT32 ty_tab_idx;
    UINT32 fld_tab_idx;
    UINT32 arb_tab_idx;
    UINT32 tylist_tab_idx;
    
    CHECKPOINT () {
	ty_tab_idx = TY_Table_Size ();
	fld_tab_idx = FLD_Table_Size ();
	arb_tab_idx = ARB_Table_Size ();
	tylist_tab_idx = TYLIST_Table_Size ();
    }

    void restore () const {
	Ty_tab.Delete_down_to (ty_tab_idx);
	Fld_Table.Delete_down_to (fld_tab_idx);
	Arb_Table.Delete_down_to (arb_tab_idx);
	Tylist_Table.Delete_down_to (tylist_tab_idx);
    }
};

struct merge_array
{
    void operator() (const TY& orig_ty, TY& new_ty,
		     const IPC_GLOBAL_TABS& original_tabs,
		     BOOL& created_new, TY_INDEX& recursive_type) const {

	// need to init all TY_IDX fields in a TY to 0 before calling
	// Merge_Global_Ty because for recursive types, TY_is_unique might
	// be called before returning here.  If this TY contains invalid
	// TY_IDX, the comparison function in TY_is_unique might be
	// confused.  See PV 604386.
	Set_TY_etype (new_ty, 0);
	Set_TY_arb (new_ty, ARB_HANDLE());

	TY_IDX etype_idx = TY_etype (orig_ty);
	TY_IDX new_etype_idx =
	    Merge_Global_Ty (etype_idx, original_tabs, created_new,
			     recursive_type); 
	Set_TY_etype (new_ty, Replace_TY_IDX_index (etype_idx,
						    new_etype_idx));

	// Insert the ARB info, and update the new array type accordingly
	//
	ARB_IDX new_arb_idx = Merge_Arb (orig_ty.Arb (),
					 original_tabs.arb_tab); 
	Set_TY_arb(new_ty, ARB_HANDLE(new_arb_idx));
    }

}; // merge_array


struct merge_pointer
{
    void operator() (const TY& orig_ty, TY& new_ty,
		     const IPC_GLOBAL_TABS &original_tabs,
		     BOOL &created_new, TY_INDEX& recursive_type) const {

	Set_TY_pointed (new_ty, 0);	// see comment in merge_array 

	TY_IDX pointed_idx = TY_pointed(orig_ty); // original pointed type
	TY_IDX new_pointed_idx =
	    Merge_Global_Ty (pointed_idx, original_tabs, created_new,
			     recursive_type); 
	Set_TY_pointed (new_ty, Replace_TY_IDX_index (pointed_idx,
						      new_pointed_idx));
    }

}; // merge_pointer


struct merge_function
{
    void operator() (const TY& orig_ty, TY& new_ty,
		     const IPC_GLOBAL_TABS &original_tabs,
		     BOOL &created_new, TY_INDEX& recursive_type) const {

	Set_TY_tylist (new_ty, 0);	// see comment in merge_array

	TYLIST_IDX tylist_idx = TY_tylist(orig_ty); // index for param/ret type
	TYLIST_IDX new_tylist_idx =
	    Merge_Tylist (tylist_idx, original_tabs, created_new,
			  recursive_type);  
	Set_TY_tylist(new_ty, new_tylist_idx);

    }
    
}; // merge_function


struct merge_struct
{
    void operator() (const TY& orig_ty, TY& new_ty,
		     const IPC_GLOBAL_TABS &original_tabs,
		     BOOL &created_new, TY_INDEX& recursive_type) const {

	Set_TY_fld (new_ty, FLD_HANDLE (0)); // see comment in merge_array

	FLD_HANDLE new_fld = Merge_Flds (orig_ty.Fld(), original_tabs, 
					 created_new, recursive_type);
	Set_TY_fld (new_ty, new_fld);
    }

}; // merge_struct


template <class MERGE_OPERATION>
static TY_IDX
merge_ty (TY_IDX ty_idx, const IPC_GLOBAL_TABS &original_tabs,
	  BOOL& created_new, TY_INDEX& recursive_type,
	  const MERGE_OPERATION& do_merge)
{
   
    // See comment in Merge_Global_Ty for general algorithm!
    // This template implements the part of the merging that is common to
    // all types, the function object "do_merge" specifies the
    // type-specific operations.
    //
    CHECKPOINT chkpt;

    TY_INDEX idx = TY_IDX_index (ty_idx);
    TY &this_ty = original_tabs.ty_tab[idx]; // type in original table
    TY_INDEX new_idx = Ty_tab.Insert(this_ty); // insert into merged table
    created_new = TRUE;
    TY &new_ty = Ty_tab[new_idx];

    // Map the old index to the new one, and mark the new idx as being that
    // of a type in the process of being merged into the new table.
    //
    TY_IDX new_ty_idx = make_TY_IDX (new_idx, ty_idx);
    New_Ty_Idx->set_map(idx, new_ty_idx);
    New_Ty_Idx->set_is_merging(idx);

    // Set the name pointer
    //
    Set_TY_name_idx(new_ty, (*New_Symstr_Idx)[TY_name_idx(this_ty)]);

    // Merge in the element type, and update the new array type 
    // accordingly.
    //
    BOOL is_new = TRUE;

    do_merge (this_ty, new_ty, original_tabs, is_new, recursive_type);

    New_Ty_Idx->clear_all_index_flags (idx); // Done with this type

    // There are two cases we don't need to check if this is a duplicate types:
    // (1) a new entry has been created, meaning that no existing ty can match
    // this one, and (2) the type is recursive but it is pointing to some
    // previously entered entry, meaning this is not the root of a recursive
    // type. 

    if (!is_new) {
	// recursive types at the lower level should have been resolved already
	Is_True (recursive_type <= new_idx, ("Invalid recursive type merge"));

	if (recursive_type == 0 || recursive_type == new_idx) {

	    recursive_type = 0;
	    TY_IDX unique_idx = TY_is_unique(new_ty_idx);
	    
	    if (unique_idx != new_ty_idx) {
		// Pop the TY and ARB tables!
		// An existing TY entry
		new_ty_idx = unique_idx;
		Reset_recursive_type_mapping (idx, new_ty_idx, original_tabs);

		chkpt.restore ();
		created_new = FALSE;
	    }
	} else
	    created_new = FALSE;
    } else {
	// just enter it into the hash table for uniqueness test
	TY_is_unique (new_ty_idx);
    }
    return new_ty_idx;
} // merge_ty;


static TY_IDX
Merge_Global_Ty(TY_IDX                ty_idx,
		const IPC_GLOBAL_TABS &original_tabs,
		BOOL                  &created_new,
		TY_INDEX              &recursive_type)
{
    // Precondition 1: all original index maps are created.
    // Precondition 2: the global tables have been initialized.
    // Precondition 3: all string table mappings are completed and merged.
    //
    // If there already exists a type that is equivalent to this type,
    // then set the map to point to the existing type; otherwise, enter
    // the new type into the merged symbol table and the return the new idx.
    //
    // The general algorithm is as follows, where for most types we
    // do this in specialized merge-routines (defined above this routine):
    //
    //   1. Insert this type, which will give us a valid TY_IDX to 
    //      use in the New_Ty_Map.  Then mark this type as being in
    //      the process of being merged in, by means of Set_TY_is_merging().
    //
    //   2. Insert records indexed from a type in a recursive manner. Avoid
    //      recursive calls for TY indices for which TY_is_merging() is TRUE.
    //
    //   3. Check this type for uniqueness, and if not unique then pop it 
    //      and its associated ARB, FLD, and TYLIST records off the type-
    //      related tables.  TY_is_unique() presents a problem for recursive
    //      types.
    //
    // Variable sized arrays have ARBs denoting STs in local symbol
    // tables.  We consider them equivalent if they denote a variable
    // at the same index, even if they refer to different local
    // symbol tables.
    //
    // A struct mey denote a common block, in which case the associated
    // FLD entries may index into the ST table.  We do a bit of premature
    // ST merging to get the correct ST_IDX for such cases.
    //
    // Postcondition : all TY, ARB, TYLIST, and FLD table mappings are
    //                 completed and the records merged.
    //
    // If a map already exists for this type, then just return it.
    //

    TY_INDEX idx = TY_IDX_index (ty_idx);
    TY_IDX new_idx = (*New_Ty_Idx)[ty_idx];

    if (TY_IDX_index (new_idx) != 0) {
	created_new = FALSE;
	if (New_Ty_Idx->is_merging (idx)) {
	    recursive_type = TY_IDX_index (new_idx);
	} else
	    recursive_type = 0;
    } else {

	// Do steps 1..3 as outlined above.
	//

	created_new = FALSE;
	recursive_type = 0;

	const TY& ty = original_tabs.ty_tab[idx];

	switch (TY_kind (ty)) {
	case KIND_SCALAR:
	case KIND_VOID:
	    new_idx = MTYPE_To_TY(TY_mtype (ty));
	    if (TY_flags(ty) != TY_flags (Ty_Table[new_idx])) {
		new_idx = make_TY_IDX (Ty_tab.Insert (ty), ty_idx);
		TY& new_ty = Ty_Table[new_idx];
		Set_TY_name_idx (new_ty, (*New_Symstr_Idx)[TY_name_idx(ty)]);
	    } 
	    New_Ty_Idx->set_map(idx, new_idx);
	    break;
	    
	case KIND_ARRAY:
	    new_idx = merge_ty (ty_idx, original_tabs, created_new,
				recursive_type, merge_array ());
	    break;

	case KIND_POINTER:
	    new_idx = merge_ty (ty_idx, original_tabs, created_new,
				recursive_type, merge_pointer ()); 
	    break;

	case KIND_FUNCTION:
	    new_idx = merge_ty (ty_idx, original_tabs, created_new,
				recursive_type, merge_function ()); 
	    break;

	case KIND_STRUCT:
	    new_idx = merge_ty (ty_idx, original_tabs, created_new,
				recursive_type, merge_struct ()); 
	    break;
	 
	case KIND_INVALID:
	case KIND_LAST:
	default:
	    Fail_FmtAssertion ("invalid TY_KIND in Merge_Global_Ty");
	    break;
	}

    } // if already merged

    return new_idx;
} // Merge_Global_Ty


// --------------------------------------------------------------
// Routines for merging TCON records.
// --------------------------------------------------------------
//
static inline void
copy_tcon (TCON& dest, const TCON& src, const TCONSTR_IDX_MAP& tconstr_map)
{
    dest.ty = src.ty;
    dest.flags = src.flags;
    switch (TCON_ty (src)) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_I8:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
    case MTYPE_U8:
	dest.vals.i0 = TCON_i0 (src);
	break;
    case MTYPE_F4:
	dest.vals.fval = TCON_fval (src);
	break;
    case MTYPE_F8:
	dest.vals.dval = TCON_dval (src);
	break;
    case MTYPE_STR:
	Set_TCON_string_ptr (dest, tconstr_map[TCON_string_ptr(src)]);
	dest.vals.sval.len = TCON_str_len (src);
	break;
    default:
	dest.vals = src.vals;
	dest.cmplxval = src.cmplxval;
    }
} // copy_tcon 


static void
Merge_Global_Tcon(const TCON* tcon_tab, UINT32 size,
		  TCON_IDX_MAP& tcon_map, const TCONSTR_IDX_MAP& tconstr_map)
{
    // Precondition 1: all original index maps are created.
    // Precondition 2: the global tables have been initialized.
    // Precondition 3: all string table mappings are completed and merged.
    // Precondition 4: all TY table mappings are completed and merged.
    //
    // If a mapping already exists for the given idx, then just return
    // it; otherwise create the mapping and the return the new idx.
    //
    // Postcondition : all TCON table mappings are completed and the 
    //                 records merged.
    //

    UINT32 idx;
    for (idx = 0; idx <= MAX_PREDEFINED_TCON_IDX; ++idx)
	tcon_map.set_map (idx, idx);
    
    for (; idx < size; ++idx) {

	// Insert the TCON into the merged table of TCONs, and create
	// a map entry from the old tcon_idx to the new idx.
	//
	// the front end often generates TCON with garbage in the padded
	// (unused) area.  To make comparision of two TCON easier, we
	// explicity clear the unused bits.

	const TCON& this_tcon = tcon_tab[idx];
	TCON new_tcon = { 0 };
	copy_tcon (new_tcon, this_tcon, tconstr_map);

	TCON_MERGE_MAP::const_iterator tc =
	    tcon_merge_map->find (&new_tcon);

	if (tc != tcon_merge_map->end ()) {
	    tcon_map.set_map (idx, (*tc).second);
	    continue;
	}

	TCON_IDX new_idx = Tcon_Table.Insert(new_tcon);
	(*tcon_merge_map)[&Tcon_Table[new_idx]] = new_idx;
	tcon_map.set_map(idx, new_idx);
    }
} // Merge_Global_Tcon


// --------------------------------------------------------------
// Routines for merging STs, and as a side-effect PUs.
// --------------------------------------------------------------
//
static PU_IDX
Merge_Global_Pu(UINT pu_idx, const IPC_GLOBAL_TABS& original_tabs)
{
    // Precondition : all TY table mappings are completed and merged.
    //
    // Note that we need not maintain any New_Pu_Map[]!
    //
    Is_True (pu_idx != 0, ("Invalid pu_idx"));

    PU &this_pu = original_tabs.pu_tab[pu_idx];

    PU_IDX new_idx = Pu_Table.Insert(this_pu); 

    // Fix the reference to the prototype, such that it becomes
    // a reference into the new merged TY table.
    //
    TY_IDX prototype_idx = (*New_Ty_Idx)[PU_prototype(this_pu)];
    Set_PU_prototype(Pu_Table[new_idx], prototype_idx);

    // merge base class in PU 
    if (PU_base_class(this_pu) != TY_IDX_ZERO) { 
        TY_IDX base_class_idx = (*New_Ty_Idx)[PU_base_class(this_pu)]; 
        Set_PU_base_class(Pu_Table[new_idx], base_class_idx); 
    } 

    // sync. up with the Aux_Pu_Table
    UINT32 aux_idx;
    AUX_PU& aux_pu = Aux_Pu_Table.New_entry (aux_idx);
    aux_pu.construct ();

    Is_True (new_idx == aux_idx, ("Aux_PU_Table out of sync."));
    
    return new_idx;
} // Merge_Global_Pu


static inline void
Invalidate_inito (const ST& st, INITO *inito_tab)
{
    UINT32 st_index = ST_IDX_index(ST_st_idx(st));
    INITO_IDX inito_idx = (*St_To_Inito_Map)[st_index];
    INITO& inito = inito_tab[inito_idx];
      
    Set_INITO_st_idx(inito, UNINITIALIZED_INDEX);
} // Invalidate_inito


static void
Resolve_Sclass(const IPC_GLOBAL_TABS &original_tabs, 
	       ST                    &merged_st,
	       const ST              &original_st)
{
   // Precondition: Both symbols refer to the same storage location.
   //               multiply defined symbols already removed
   //
   // Given a "merged" ST from the merged symbol-table and an original
   // ST from a WHIRL object file, we update the merged symbol accordingly.
   //
   ST_SCLASS       merged_sclass = ST_storage_class(merged_st);
   const ST_SCLASS original_sclass = ST_storage_class(original_st);

   switch (original_sclass) {
   case SCLASS_EXTERN:
       if (ST_is_weak_symbol (original_st) && merged_sclass == SCLASS_EXTERN) {

	   // need to propagate the weak attribute
	   Set_ST_is_weak_symbol (merged_st);
	   // if both symbols have weak aliases, then this is a multiply
	   // defined weak symbols and should have been reported by ld.  We
	   // can then just ignore the second one.  Otherwise, we just
	   // propagate the weak alias info.
	   if (ST_strong_idx (merged_st) == ST_st_idx (merged_st) &&
	       ST_strong_idx (original_st) != ST_st_idx (original_st)) {
	       ST_IDX st_idx =
		   Merge_Global_St (ST_IDX_index (ST_strong_idx (original_st)),
				    original_tabs);
	       Set_ST_strong_idx (merged_st, st_idx);
	   }
       }
       break;

   case SCLASS_COMMON:
       
       if (merged_sclass == SCLASS_EXTERN)
	   Set_ST_storage_class (merged_st, SCLASS_COMMON);
       break;

   case SCLASS_TEXT:
       
       Set_AUX_PU_file_hdr (Aux_Pu_Table[ST_pu (merged_st)],
			    current_file_hdr);
       if (IPA_Enable_Simple_Alias)
	   Set_PU_ipa_addr_analysis (Pu_Table[ST_pu (merged_st)]);
       // fall through
       
   case SCLASS_UGLOBAL:
   case SCLASS_DGLOBAL:

       Set_ST_storage_class (merged_st, original_sclass);
       
       break;
   }

} // Resolve_Sclass


static void
Synch_Pu_With_Pu (PU& merged_pu, const PU& original_pu)
{
    // Precondition: Both PUs denote the same storage location.
    //
    // Sets flags and other attributes of the merged_pu to account for
    // the original_pu.
    //

    // Attributes that can be set iff both symbols set them
    const UINT64 pu_flags_and_mask = PU_IS_PURE | PU_NO_SIDE_EFFECTS;

    // Attributes that need to be set if any of the two symbols sets them
    const UINT64 pu_flags_or_mask =
	PU_IS_INLINE_FUNCTION | PU_NO_INLINE | PU_MUST_INLINE |
	PU_NO_DELETE | PU_HAS_EXC_SCOPES | PU_HAS_NON_MANGLED_CALL |
	PU_ARGS_ALIASED | PU_NEEDS_FILL_ALIGN_LOWERING | PU_NEEDS_T9 |
	PU_HAS_VERY_HIGH_WHIRL | PU_HAS_ALTENTRY | PU_RECURSIVE |
	PU_IS_MAINPU | PU_UPLEVEL | PU_MP_NEEDS_LNO | PU_HAS_ALLOCA |
	PU_IN_ELF_SECTION | PU_HAS_MP | PU_MP | PU_HAS_NAMELIST |
	PU_HAS_RETURN_ADDRESS | PU_HAS_REGION | PU_HAS_INLINES |
	PU_CALLS_SETJMP | PU_CALLS_LONGJMP | PU_HAS_USER_ALLOCA |
	PU_HAS_ATTR_MALLOC | PU_HAS_ATTR_NORETURN | PU_NOTHROW;

    const UINT64 original_flags = original_pu.flags;
    UINT64 merged_flags = merged_pu.flags;

    merged_flags = (merged_flags & ~pu_flags_and_mask) |
	(merged_flags & original_flags & pu_flags_and_mask);

    merged_flags |= (original_flags & pu_flags_or_mask);

    merged_pu.flags = merged_flags;

    merged_pu.src_lang |= original_pu.src_lang;
#ifdef KEY
    if (!merged_pu.misc)
    	merged_pu.misc = original_pu.misc; // EH/C nested function information

    merged_pu.unused = original_pu.unused;
#endif
} // Synch_Pu_With_Pu


static inline TY_IDX
Synch_TY_IDX (TY_IDX merged_ty_idx, TY_IDX original_ty_idx)
{
    TY_IDX result = merged_ty_idx;

    if (!TY_is_const (merged_ty_idx) || !TY_is_const (original_ty_idx))
	Clear_TY_is_const (result);

    if (!TY_is_restrict (merged_ty_idx) || !TY_is_restrict (original_ty_idx))
	Clear_TY_is_restrict (result);

    if (TY_is_volatile (merged_ty_idx) || TY_is_volatile (original_ty_idx))
	Set_TY_is_volatile (result);

#ifdef KEY
    Set_TY_align_exp (result, MAX (TY_align_exp (merged_ty_idx),
				   TY_align_exp (original_ty_idx)));
#else
    Set_TY_align_exp (result, max (TY_align_exp (merged_ty_idx),
				   TY_align_exp (original_ty_idx)));
#endif
    return result;
} // Synch_TY_IDX


void
Synch_ST_flags (ST& merged_st, const ST& original_st)
{
    // attributes that can be set iff both ST entries set them
    const UINT32 st_flags_and_mask =
	ST_IS_NOT_USED | ST_IS_CONST_VAR | ST_PT_TO_UNIQUE_MEM;

    // attributes that must be set if any one of the ST entries sets them
    const UINT32 st_flags_or_mask =
	ST_KEEP_NAME_W2F | ST_IS_RESHAPED | ST_EMIT_SYMBOL | ST_GPREL |
	ST_NOT_GPREL | ST_IS_NAMELIST | ST_IS_F90_TARGET |
	ST_DECLARED_STATIC | ST_IS_THREAD_PRIVATE | ST_ADDR_SAVED |
	ST_ADDR_PASSED | ST_INIT_VALUE_ZERO | ST_IS_INITIALIZED |
        ST_HAS_NAMED_SECTION; 

    UINT32 original_flags = original_st.flags;
    UINT32 merged_flags = merged_st.flags;

    merged_flags = (merged_flags & ~st_flags_and_mask) |
	(merged_flags & original_flags & st_flags_and_mask);

    merged_flags |= original_flags & st_flags_or_mask;

    merged_st.flags = merged_flags;

    // check for conflicts
    if ((merged_flags & (ST_GPREL | ST_NOT_GPREL)) ==
	(ST_GPREL | ST_NOT_GPREL)) {
	Clear_ST_gprel (merged_st); 
    }

    // if the named_section starts with .rodata., we
    // need to set the ST as const_var, as gas will
    // treat it as a rodata.
    if (ST_is_const_var(merged_st))
        return;

    BOOL set_const_var = FALSE;
    if (ST_has_named_section(&merged_st))
    {
        STR_IDX name = Find_Section_Name_For_ST(&merged_st);
        if (strncmp(Index_To_Str(name), ".rodata.", 8) == 0)
            set_const_var = TRUE;
    }
    if (!set_const_var && ST_has_named_section(&original_st))
    {
        STR_IDX name = Find_Section_Name_For_ST(&original_st);
        if (strncmp(Index_To_Str(name), ".rodata.", 8) == 0)
            set_const_var = TRUE;
    }
    if (set_const_var)
        Set_ST_is_const_var(&merged_st);

} // Synch_ST_flags
 


static void
Synch_St_With_St(const IPC_GLOBAL_TABS& original_tabs,
		 ST&                    merged_st,
		 const ST&              original_st)
{
    // Precondition: Both STs denote the same storage location.
    //
    // Sets storage class and flags of the merged_st to account for
    // the original_st.
    //

    // Resolve the storage class and weak-extern attributes.
    // 
    Resolve_Sclass (original_tabs, merged_st, original_st);

    // Synch the TY attributes, if they are different
    switch (ST_sym_class (merged_st)) {
    case CLASS_FUNC:
	{
	    PU& merged_pu = Pu_Table [ST_pu (merged_st)];
	    const PU& original_pu = original_tabs.pu_tab[ST_pu (original_st)];
	    TY_IDX new_ty_idx =
		Synch_TY_IDX (PU_prototype (merged_pu),
			      (*New_Ty_Idx)[PU_prototype (original_pu)]);
	    Set_PU_prototype (merged_pu, new_ty_idx);
	}
	break;

    default:
	Set_ST_type (merged_st,
		     Synch_TY_IDX (ST_type (merged_st),
				   (*New_Ty_Idx)[ST_type (original_st)]));
	break;
    }
					     
    Synch_ST_flags (merged_st, original_st);

    if (ST_sym_class (merged_st) == CLASS_FUNC)
	Synch_Pu_With_Pu (Pu_Table[ST_pu (merged_st)],
			  original_tabs.pu_tab[ST_pu (original_st)]);
} // Synch_St_With_St


static ST_IDX
Enter_Original_St(const IPC_GLOBAL_TABS& original_tabs,
		  const ST& original_st)
{
    if (ST_sym_class (original_st) == CLASS_CONST) {
	ST st = original_st;
	Set_ST_tcon(st, (*New_Tcon_Idx)[ST_tcon(original_st)]);
	Set_ST_type(st, (*New_Ty_Idx)[ST_type(original_st)]);
	ST_MERGE_MAP::const_iterator iter = st_merge_map->find (&st);
	if (iter != st_merge_map->end ()) {
	    // found a match
	    (*New_St_Idx).set_map(ST_st_idx(original_st), (*iter).second);
	    return (*iter).second;
	}
    }

    // Inserts original_st into the merged symbol table, and sets the
    // New_St_Idx accordingly.  Flags remain unaltered, while indices
    // are updated to point into the merged symbol table.
    //
    ST_TAB &tab = *(Scope_tab[GLOBAL_SYMTAB].st_tab);
    UINT32 new_idx = tab.Insert(original_st); 
    ST     &new_st = tab[new_idx];
    ST_IDX new_st_idx = make_ST_IDX (new_idx, GLOBAL_SYMTAB);

    (*New_St_Idx).set_map(ST_st_idx(original_st), new_st_idx);

    Is_True (tab.Size () == Aux_St_Tab.Size (), ("Aux St Table out of sync"));

    // Update indices to other merged symbol-table entities.
    //
    if (ST_sym_class(original_st) == CLASS_CONST) {
	Set_ST_tcon(new_st, (*New_Tcon_Idx)[ST_tcon(original_st)]);
	Set_ST_type(new_st, (*New_Ty_Idx)[ST_type(original_st)]);
	(*st_merge_map)[&new_st] = new_st_idx;
    } else {
	Set_ST_name_idx(new_st, (*New_Symstr_Idx)[ST_name_idx(original_st)]);

	if (ST_sym_class(original_st) == CLASS_FUNC
#ifdef KEY
	    // Bug 14465: Merge the PU for a dummy function representing
	    // a global-scope ASM, so that merged symbol table entries
	    // get the updated PU idx.
	    || ST_sym_class(original_st) == CLASS_NAME
#endif
	   ) {
	    PU_IDX new_pu_idx = 
		Merge_Global_Pu (ST_pu(original_st), original_tabs);

	    Set_ST_pu(new_st, new_pu_idx);
	    if (ST_storage_class (new_st) != SCLASS_EXTERN) {
		// a function definition
		Set_AUX_PU_file_hdr (Aux_Pu_Table[new_pu_idx],
				     current_file_hdr);
		if (IPA_Enable_Simple_Alias)
		    Set_PU_ipa_addr_analysis (Pu_Table[new_pu_idx]);
	    }
	} else if (ST_sym_class(original_st) == CLASS_BLOCK) {
	    Fail_FmtAssertion("invalid ST_sym_class in Merge_Global_St");
	} else {
	    Set_ST_type(new_st, (*New_Ty_Idx)[ST_type(original_st)]);
	}
    }
    Set_ST_st_idx(new_st, new_st_idx);
    if(ST_vtable_ty_idx(original_st))
    {
        TY_IDX ty_idx = (*New_Ty_Idx)[ST_vtable_ty_idx(original_st)];
        Set_ST_vtable_ty_idx(new_st, ty_idx);
        Set_TY_vtable(ty_idx, new_st_idx);
    }

    ST_IDX base_idx =
	ST_raw_base_idx (original_st) == ST_st_idx (original_st) ?
	new_st_idx :
	Merge_Global_St (ST_IDX_index (ST_raw_base_idx(original_st)),
			 original_tabs);
    Set_ST_raw_base_idx(new_st, base_idx);

#if defined(TARG_X8664) || defined(TARG_SL)
    if ( ST_sclass(new_st) != SCLASS_COMMON &&
	 // Avoid Fortran Equivalenced arrays (to complete fix for bug 1988)
	 !ST_is_equivalenced(new_st) &&
         ST_sym_class(new_st) == CLASS_VAR ) {
      TY_IDX ty = ST_type(new_st);
      if (TY_kind(ST_type(new_st)) == KIND_POINTER) {
        ty = TY_pointed(ST_type(new_st));
      }
      if (TY_kind(ty) != KIND_FUNCTION) {
        TY_IDX st_ty_idx = ST_type(new_st);
        Set_TY_align_exp(st_ty_idx, 4);
        Set_ST_type(new_st, st_ty_idx);
      }
    }
#endif

    return new_st_idx;
} // Enter_Original_St


static ST_IDX
Merge_St_With_Pext(const IPC_GLOBAL_TABS& original_tabs, 
		   const ST&		  original_st,
		   void*		  pext)
{
   // Precondition: The pext has already been updated to account for the
   //     original_st, and merged_st has sym_class CLASS_FUNC or CLASS_VAR.
   //
   // We enter the original ST into our merged symbol table and
   // take into account the ELF symbol table resolution results,
   // as exhibited by pext, in setting the flags of the new symbol.
   //
   const ST_IDX new_st_idx = Enter_Original_St (original_tabs, original_st);
   ST& new_st = St_Table[new_st_idx];

   switch (ST_storage_class (new_st)) {
   case SCLASS_TEXT:
   case SCLASS_UGLOBAL:
   case SCLASS_DGLOBAL:
       if (!ld_resolved_to_obj (pext, original_tabs.p_obj)) {
	   // multiply defined symbol
	   if (ST_storage_class (new_st) == SCLASS_DGLOBAL)
	       Invalidate_inito (new_st, original_tabs.inito_tab);
	   else if (ST_storage_class (new_st) == SCLASS_TEXT)
	       Set_AUX_PU_file_hdr (Aux_Pu_Table[ST_pu (new_st)], NULL);
	   Set_ST_storage_class (new_st, SCLASS_EXTERN);
       }
       break;
   }

   ld_set_st_idx (pext, new_st_idx);

   return new_st_idx;

} // Merge_St_With_Pext


// When two global STs of the same name have conflicting type, we use the
// types of the defining ST.
static void
Handle_Incompatible_Func_Types (const ST& merged_st, const ST& original_st,
				PU& merged_pu, TY_IDX original_ty_idx)
{
    if (ST_storage_class (merged_st) == SCLASS_TEXT)
	return;

    if (ST_storage_class (original_st) == SCLASS_TEXT) {
	TY_IDX ty_idx = PU_prototype (merged_pu);
	Set_PU_prototype (merged_pu,
			  Replace_TY_IDX_index (ty_idx, original_ty_idx));
	return;
    }
    
    // both functions are extern, pick the one with longer tylist
    TYLIST_IDX i = TY_tylist (Ty_Table[PU_prototype (merged_pu)]);
    TYLIST_IDX j = TY_tylist (Ty_Table[original_ty_idx]);
    
    while (TYLIST_type (Tylist_Table[i]) != 0 &&
	   TYLIST_type (Tylist_Table[j]) != 0) {
	++i;
	++j;
    }
	
    if (TYLIST_type (Tylist_Table[j]) != 0) {
	TY_IDX ty_idx = PU_prototype (merged_pu);
	Set_PU_prototype (merged_pu,
			  Replace_TY_IDX_index (ty_idx, original_ty_idx));
    }
    return;
    
} // Handle_Incompatible_Func_Types


static void
Handle_Incompatible_Data_Types (ST& merged_st, const ST& original_st)
{
    if (ST_storage_class (merged_st) == SCLASS_UGLOBAL ||
	ST_storage_class (merged_st) == SCLASS_DGLOBAL)
	return;

    TY_IDX original_ty_idx = (*New_Ty_Idx)[ST_type (original_st)];

    if (ST_storage_class (original_st) == SCLASS_UGLOBAL ||
	ST_storage_class (original_st) == SCLASS_DGLOBAL) {
	TY_IDX ty_idx = ST_type (merged_st);
	Set_ST_type (merged_st,
		     Replace_TY_IDX_index (ty_idx, original_ty_idx));
	return;
    }

    const TY& merged_ty = Ty_Table[ST_type (merged_st)];
    const TY& original_ty = Ty_Table[original_ty_idx];

    if (TY_size (original_ty) > TY_size (merged_ty)) {
	TY_IDX ty_idx = ST_type (merged_st);
	Set_ST_type (merged_st,
		     Replace_TY_IDX_index (ty_idx, original_ty_idx));
    }

    return;
} // Handle_Incompatible_Data_Types


static ST_IDX
Merge_St_With_St(const IPC_GLOBAL_TABS &original_tabs, 
		 ST                    &merged_st,
		 const ST              &original_st, 
		 void	               *pext)
{
    // Precondition: the pext has already been updated to account for both
    //   the merged_st and the original_st, and the original_st has
    //   sym_class CLASS_FUNC or CLASS_VAR.
    //
    // We merge the original_st with the merged_st, the latter already 
    // present in the merged symbol table.  The original_st is already 
    // accounted for by the pext, and whenever possible we use the pext
    // attributes instead of the attributes of the original_st in updating
    // the merged_st.
    //

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

    switch (ST_storage_class (original_st)) {
    case SCLASS_TEXT:
    case SCLASS_UGLOBAL:
    case SCLASS_DGLOBAL:
	if (!ld_resolved_to_obj (pext, original_tabs.p_obj)) {
	    // multiply defined symbol
	    if (ST_storage_class (original_st) == SCLASS_DGLOBAL)
		Invalidate_inito (original_st, original_tabs.inito_tab);
	    (*New_St_Idx).set_map (ST_st_idx(original_st),
				   ST_st_idx(merged_st)); 
	    if (ST_addr_passed (original_st))
		Set_ST_addr_passed (merged_st);
	    if (ST_addr_saved (original_st))
		Set_ST_addr_saved (merged_st);
	    return ST_st_idx (merged_st);
	}
	break;
    }
#endif // _STANDALONE_INLINER

    if (ST_sym_class (original_st) != ST_sym_class (merged_st)) {
#ifdef KEY
	// bug 2461
	ErrMsg (EC_Inc_Types, ST_name (merged_st)); 
#else
	Fail_FmtAssertion ("symbol %s declared both as function and variable",
			   ST_name (merged_st)); 
#endif
    }

    if (ST_sym_class (original_st) == CLASS_FUNC) {
	const PU& original_pu = original_tabs.pu_tab[ST_pu (original_st)];
	PU& merged_pu = Pu_Table[ST_pu (merged_st)];
	TY_IDX original_ty_idx = (*New_Ty_Idx)[PU_prototype (original_pu)];
	if (TY_IDX_index (PU_prototype (merged_pu)) !=
	    TY_IDX_index (original_ty_idx))
	    Handle_Incompatible_Func_Types (merged_st, original_st,
					    merged_pu, original_ty_idx);
    } else if (TY_IDX_index (ST_type (merged_st)) !=
	       TY_IDX_index ((*New_Ty_Idx)[ST_type (original_st)])) {

	Handle_Incompatible_Data_Types (merged_st, original_st);
    }

    Synch_St_With_St (original_tabs, merged_st, original_st);

    (*New_St_Idx).set_map (ST_st_idx(original_st), ST_st_idx(merged_st));
#if defined(TARG_X8664) || defined(TARG_SL)
    if ( ST_sclass(merged_st) != SCLASS_COMMON &&
	 // Avoid Fortran Equivalenced arrays (to complete fix for bug 1988)
	 !ST_is_equivalenced(merged_st) &&
         ST_sym_class(merged_st) == CLASS_VAR ) {
      TY_IDX ty = ST_type(merged_st);
      if (TY_kind(ST_type(merged_st)) == KIND_POINTER) {
        ty = TY_pointed(ST_type(merged_st));
      }
      if (TY_kind(ty) != KIND_FUNCTION) {
        TY_IDX st_ty_idx = ST_type(merged_st);
        Set_TY_align_exp(st_ty_idx, 4);
        Set_ST_type(merged_st, st_ty_idx);
      }
    }
#endif
    return ST_st_idx (merged_st);
} // Merge_St_With_St


static void
Verify_Predefined_Symbols (const ST& st)
{
    ST mapped_st = st;
    Set_ST_name_idx (mapped_st, (*New_Symstr_Idx)[ST_name_idx (st)]);
    
    FmtAssert (bcmp (&mapped_st, &St_Table[ST_st_idx (st)], sizeof(ST)) == 0,
	       ("Incompatible predefined pregs in global symbol table"));
    (*New_St_Idx).set_map (ST_st_idx (st), ST_st_idx (st));
}


// for each common block, we maintain a list of elements sorted by offset
// and size.  Elements of the same type, size, and offset are considered
// the same, regardless of their names.
static ST_IDX
Process_Common_Element (const IPC_GLOBAL_TABS& original_tabs,
			const ST& original_st)
{
    ST_IDX base_idx = (*New_St_Idx)[ST_base_idx (original_st)];

    Is_True (base_idx != 0, ("Missing base idx for common block element"));

    const TY& ty = Ty_Table[(*New_Ty_Idx)[ST_type (original_st)]];

    Is_True (!ST_is_split_common (original_st),
	     ("Front end should not generate split common under -IPA"));


    COMMON_BLOCK_ELEMENTS_MAP::const_iterator block_iter =
	Common_Block_Elements_Map->find (base_idx);

    BLOCK_ELEMENTS* block;
    if (block_iter == Common_Block_Elements_Map->end ()) {
	block = CXX_NEW (BLOCK_ELEMENTS (block_element_compare (),
					 &Common_map_pool),
			 &Common_map_pool);
	(*Common_Block_Elements_Map)[base_idx] = block;
    } else
	block = (*block_iter).second;

    BLOCK_ELEMENT_DESC desc (&ty, ST_ofst (original_st));
    BLOCK_ELEMENTS::iterator element = block->find (desc);

    ST_IDX new_st_idx;
    if (element != block->end ()) {
	// already in the list, no need to insert
	new_st_idx = (*element).second;
	(*New_St_Idx).set_map (ST_st_idx (original_st), new_st_idx);
	Synch_ST_flags (St_Table[new_st_idx], original_st);
	return new_st_idx;
    }

    new_st_idx = Enter_Original_St (original_tabs, original_st);
    block->insert (element, make_pair (desc, new_st_idx));
    return new_st_idx;
} // Process_Common_Element


#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))

static ST_IDX
Merge_Global_St(UINT                   idx,
		const IPC_GLOBAL_TABS& original_tabs)
{
    // Precondition 1: all original index maps are created.
    // Precondition 2: the global tables have been initialized.
    // Precondition 3: St_To_Inito_Map has been initialized.
    // Precondition 4: all string table mappings are completed and merged.
    // Precondition 5: all TY table mappings are completed and merged.
    // Precondition 6: all TCON table mappings are completed and merged.
    //
    // If a mapping already exists for the given idx, then just return
    // it; otherwise create the mapping and the return the new idx.
    //
    // Postcondition : all ST, PU table mappings are completed and the 
    //                 records merged.  All tables in the merged symbol
    //                 table now refer to other items in either the merged 
    //                 symbol table or to items in local symbol tables!
    //

    Is_True (idx != 0, ("Invalid ST_IDX for Merge_Global_St()"));

    ST_IDX new_st_idx = New_St_Idx->map_[idx];

    if (new_st_idx != UNINITIALIZED_INDEX)
	return new_st_idx;

    ST& original_st = original_tabs.st_tab[idx]; // This WHIRL symbol

    // We only merge external symbols (never file-static symbols).
    if (ST_export(original_st) == EXPORT_LOCAL ||
	ST_export(original_st) == EXPORT_LOCAL_INTERNAL) {

	if (ST_raw_base_idx (original_st) == ST_st_idx (original_st))
	    return Enter_Original_St (original_tabs, original_st);
	else
	    // this might be a common block element
	    return Process_Common_Element (original_tabs, original_st);
    }
	  
    // global symbols
    
    char *st_name = &original_tabs.symstr_tab[ST_name_idx (original_st)];

    ST* global_st = get_global_st(st_name);
   
    if (global_st == NULL) {
	// this is the first time we see this symbol and it comes from a
	// WHIRL object, so we just enter it.
	return(Enter_Original_St (original_tabs, original_st));
    } else {
	return Merge_St_With_St (original_tabs, *global_st,
				 original_st, NULL);
    }
} // Merge_Global_St
#else // _STANDALONE_INLINER

static ST_IDX
Merge_Global_St(UINT                   idx,
		const IPC_GLOBAL_TABS& original_tabs)
{
    // Precondition 1: all original index maps are created.
    // Precondition 2: the global tables have been initialized.
    // Precondition 3: St_To_Inito_Map has been initialized.
    // Precondition 4: all string table mappings are completed and merged.
    // Precondition 5: all TY table mappings are completed and merged.
    // Precondition 6: all TCON table mappings are completed and merged.
    //
    // If a mapping already exists for the given idx, then just return
    // it; otherwise create the mapping and the return the new idx.
    //
    // Postcondition : all ST, PU table mappings are completed and the 
    //                 records merged.  All tables in the merged symbol
    //                 table now refer to other items in either the merged 
    //                 symbol table or to items in local symbol tables!
    //

    Is_True (idx != 0, ("Invalid ST_IDX for Merge_Global_St()"));

    ST_IDX new_st_idx = New_St_Idx->map_[idx];

    if (new_st_idx != UNINITIALIZED_INDEX)
	return new_st_idx;

    ST& original_st = original_tabs.st_tab[idx]; // This WHIRL symbol

    // We only merge external symbols (never file-static symbols).
    if (ST_export(original_st) == EXPORT_LOCAL ||
	ST_export(original_st) == EXPORT_LOCAL_INTERNAL) {

#ifdef KEY
	// Bug 8443: A function cannot be a common block element.
	if (ST_raw_base_idx (original_st) == ST_st_idx (original_st) ||
	    ST_sym_class (original_st) == CLASS_FUNC)
#else
	if (ST_raw_base_idx (original_st) == ST_st_idx (original_st))
#endif
	    return Enter_Original_St (original_tabs, original_st);
	else
	    // this might be a common block element
	    return Process_Common_Element (original_tabs, original_st);
    }
	  
    // global symbols
    
    // Look up the symbol in the ELF symbol table and determine
    // whether or not another WHIRL symbol matches this one.
    // If no global WHIRL symbol is referenced by the ELF symbol, 
    // then this original ST is the first one and 
    // ST_IDX_level(pext_st_idx) != GLOBAL_SYMTAB (actually the
    // pext_st_idx is zero).
    //
    char *st_name = &original_tabs.symstr_tab[ST_name_idx (original_st)];

#if defined(TARG_IA64) || defined(TARG_X8664) || defined(TARG_MIPS) || defined(TARG_SL) || defined(TARG_LOONGSON)
    void *pext = ld_slookup_mext(st_name,
    	    	    	    	(ST_storage_class (original_st) == SCLASS_EXTERN));
#else
    void *pext = slookup_mext(st_name);
#endif

    if (pext == NULL)
	Fail_FmtAssertion("Cannot find pext in Merge_Global_St");

    ST_IDX pext_st_idx = ld_get_st_idx (pext);

    if (pext_st_idx == WHIRL_ST_IDX_UNINITIALIZED) {
	// this is the first time we see this symbol and it comes from a
	// WHIRL object, so we just enter it.
	pext_st_idx = Enter_Original_St (original_tabs, original_st);
	ld_set_st_idx (pext, pext_st_idx);
	return pext_st_idx;
    } else if (pext_st_idx == WHIRL_ST_IDX_NOT_AVAILABLE) {
	// We need to take into account the ELF version of this symbol,
	// which may also have information about ELF symbols encountered
	// in DSOs or ELF object files.  This ST is the first WHIRL symbol
	// that corresponds to the given ELF symbol (pext).  Enter the
	// ST (and PU for CLASS_FUNC symbols).
	//
	return Merge_St_With_Pext (original_tabs, original_st, pext);
    } else {
	// There is both a WHIRL and ELF symbol that match this one.
	//
	return Merge_St_With_St (original_tabs, St_Table[pext_st_idx],
				 original_st, pext);
    }
} // Merge_Global_St

#endif // _STANDALONE_INLINER


// --------------------------------------------------------------
// Routines for merging INITVs and INITOs.
// --------------------------------------------------------------
//
static INITV_IDX
Merge_Global_Initv(UINT                  initv_idx,
		   const IPC_GLOBAL_TABS &original_tabs,
		   INITV_IDX_MAP	 &initv_map)
{
    // Precondition 1: all original index maps are created.
    // Precondition 2: the global tables have been initialized.
    // Precondition 3: all string table mappings are completed and merged.
    // Precondition 4: all TY table mappings are completed and merged.
    // Precondition 5: all TCON table mappings are completed and merged.
    // Precondition 6: all ST table mappings are completed and merged.
    //
    // If a mapping already exists for the given idx, then just return
    // it; otherwise create the mapping and the return the new idx.
    //
    // Postcondition : all INITV table mappings are completed and 
    //                 the records merged.
    //

    if (initv_map[initv_idx] != 0)
	return initv_map[initv_idx];

    INITV     &original_initv = original_tabs.initv_tab[initv_idx];
    INITV_IDX  new_idx = Initv_Table.Insert(original_initv);
    INITV     &new_initv = Initv_Table[new_idx];
    ST_IDX     st_idx, new_st_idx;
    INITV_IDX  nested_initv_idx;

    initv_map.set_map (initv_idx, new_idx);

    // Recursively process initv's linked together.
    //
    if (INITV_next(original_initv) != 0) {
	nested_initv_idx = 
	    Merge_Global_Initv(INITV_next(original_initv), original_tabs,
			       initv_map); 
	Set_INITV_next(new_initv, nested_initv_idx);
    }

    // Update references to other symtab entities.
    //
    switch (INITV_kind(original_initv)) {
    case INITVKIND_SYMOFF:
#ifdef TARG_IA64
    case INITVKIND_SYMIPLT:
#endif
	st_idx = INITV_symoff_st(original_initv);
	if (ST_IDX_level (st_idx) == GLOBAL_SYMTAB) {
	    new_st_idx = (*New_St_Idx)[st_idx];
	    Set_INITV_symoff_st(new_initv, new_st_idx);
	}
	break;

    case INITVKIND_VAL:
	Set_INITV_tcon (new_initv,
			(*New_Tcon_Idx)[INITV_tcon(original_initv)]);
	break;
	 
    case INITVKIND_BLOCK:
#ifdef KEY // fixes bug 1010
        if (INITV_block_first(original_initv) == INITV_IDX_ZERO)
	  nested_initv_idx = INITV_IDX_ZERO;
	else
#endif
	nested_initv_idx = 
	    Merge_Global_Initv(INITV_block_first(original_initv), 
			       original_tabs, initv_map);
	Set_INITV_block_first(new_initv, nested_initv_idx);
	break;
	 
    case INITVKIND_SYMDIFF:
    case INITVKIND_SYMDIFF16:
	st_idx = INITV_symdiff_st(original_initv);
	if (ST_IDX_level (st_idx) == GLOBAL_SYMTAB) {
	    new_st_idx = (*New_St_Idx)[st_idx];
	    Set_INITV_symdiff_st(new_initv, new_st_idx);
	}
	break;
	 
    case INITVKIND_ZERO:
    case INITVKIND_ONE:
    case INITVKIND_PAD:
    case INITVKIND_LABEL:
	break;

    default:
	Fail_FmtAssertion ("invalid INITVKIND in Merge_Global_Initv");
	break;
    }
    return new_idx;
} // Merge_Global_Initv


static void
Merge_Global_Inito(const INITO* inito_tab, UINT32 inito_tab_size,
		   const INITV_IDX_MAP& initv_map)
{
    // Precondition 1 : all ST table mappings are completed and merged.
    // Precondition 2 : all INITV table mappings are completed and merged.
    //
    // An inito may have been marked as unnecessary by virtue of an ST
    // initializer being overridden by another initializer for the same
    // symbol.  Such marks are signified by:
    //
    //   INITO_st_idx(original_inito) == UNINITIALIZED_INDEX
    //
    // Unneccessary INITOs are not copied over to the merged symbol table.
    //
    //

    for (UINT32 idx = 1; idx < inito_tab_size; ++idx) {

	const INITO& original_inito = inito_tab[idx];

	if (INITO_st_idx (original_inito) == UNINITIALIZED_INDEX)
	    continue;
	
	INITO_TAB &tab = *(Scope_tab[GLOBAL_SYMTAB].inito_tab);
	UINT32  new_idx = tab.Insert(original_inito);
	INITO& new_inito = tab[new_idx];

	// Fix the reference to the ST.
	//
	ST_IDX st_idx = (*New_St_Idx)[INITO_st_idx(original_inito)];
	Set_INITO_st_idx(new_inito, st_idx);

	// Fix the reference to the INITV.
	//
	INITV_IDX initv_idx = initv_map[INITO_val(original_inito)];
	Set_INITO_val(new_inito, initv_idx);

	// Update the "global" st to inito map
	ST_To_INITO_Map[st_idx] = make_INITO_IDX (new_idx, GLOBAL_SYMTAB);
    }
} // Merge_Global_Inito

static BOOL St_Attr_Entry_Existed(ST_IDX new_st_idx, ST_ATTR_KIND kind) {
	UINT32 i;
	ST_ATTR* st_attr_entry;
	for (i = 0; i < ST_ATTR_Table_Size(GLOBAL_SYMTAB) && (st_attr_entry = &St_Attr_Table(GLOBAL_SYMTAB,i)); ++i) {
	    if (st_attr_entry->kind == kind && st_attr_entry->st_idx == new_st_idx) {
	      return TRUE;
	    }
	}
	return FALSE;
} // St_Attr_Entry_Existed

static void
Merge_Global_St_Attr(const ST_ATTR* st_attr_tab, UINT32 size)
{
    // Precondition 1: all string table mappings are completed and merged.
    // Precondition 2: all ST table mappings are completed and merged.
    //

    UINT32 idx;
    
    for (idx = 1; idx < size; ++idx) {

	// Insert the ST_ATTR into the merged table of ST_ATTRs.
	// The entry is appended to the end because the entry
	// is unique both for ST_ATTR_DEDICATED_REGISTER as well
	// ST_ATTR_SECTION_NAME.
	//

	const ST_ATTR old_st_attr = st_attr_tab[idx];
	ST_IDX st_idx = (*New_St_Idx) [ST_ATTR_st_idx (old_st_attr)];
	Update_reference_count (&St_Table [st_idx], /*refcount*/ 1,
				/*modcount*/ 0, /*is_cmod*/ FALSE);

	if (St_Attr_Entry_Existed(st_idx, old_st_attr.kind)) continue;

	ST_ATTR_KIND akind = old_st_attr.kind;
	ST_ATTR_IDX new_st_attr_idx;
	ST_ATTR&    new_st_attr = New_ST_ATTR (GLOBAL_SYMTAB, new_st_attr_idx);

	switch (old_st_attr.kind) {

	    case ST_ATTR_DEDICATED_REGISTER:
		ST_ATTR_Init (new_st_attr, st_idx, ST_ATTR_DEDICATED_REGISTER,
			      ST_ATTR_reg_id (old_st_attr));
		break;

	    case ST_ATTR_SECTION_NAME:
		ST_ATTR_Init (new_st_attr, st_idx, ST_ATTR_SECTION_NAME,
			      (*New_Symstr_Idx) [ST_ATTR_section_name (old_st_attr)]);
		break;

	    default:
		Fail_FmtAssertion ("invalid ATTR_KIND in Merge_Global_St_Attr");
	    break;
	}
    }
} // Merge_Global_St_Attr


static inline void
Merge_File_Info (const FILE_INFO& info)
{
    File_info.flags |= info.flags;
} // Merge_File_Info


// --------------------------------------------------------------
// Routines for entering WHIRL symbols into the ELF symbol table
// and to inquire about attributes in the ELF symbol table
//
// These routines are exported by this module
// --------------------------------------------------------------
//
IPC_GLOBAL_IDX_MAP *
IPC_merge_global_tab (const IPC_GLOBAL_TABS &original_tabs,
		      IP_FILE_HDR& hdr, MEM_POOL *mempool)
{
    // The algorithm we employ is as follows:
    //
    //  Precondition: All STs have been entered into the ELF symbol table
    //                for symbol resolution with ELF .o and .so objects.
    //
    //  1) Merge string tables to produce complete New_Symstr_Idx and
    //     New_Tconstr_Idx maps.
    //
    //  2) Merge the TY table, and in doing so recursively also traverse
    //     the TYLIST, FLD, ARB, and ST tables.  This will have to be a 
    //     depth-first traversal algorithm, since that will be required
    //     to enter and then pop a type off the merged table.  Note that
    //     the indices to ST entries in ARBs will always be to local symbols,
    //     and as such these entries can be blindly copied into the merged
    //     table.  Indices to ST entries from FLDs will always be global
    //     symbols that must be resolved to accurately create FLD entries.
    //     Delete the New_Fld_Idx maps
    //     after this is all done.  This gives us a complete New_Ty_Idx map.
    //
    //  3) Walk the TCON table and append the entries to the end of the
    //     merged table.  This gives us a complete New_Tcon_Idx. 
    //     TODO: avoid duplicates.
    //
    //  4) Walk the ST table, merge in each ST, and in the process of doing
    //     so also merge in PU entries.  Note that some STs may already 
    //     have been entered as a result of processing FLDs.  This gives us
    //     complete New_St_Idx and New_Pu_Idx tables.
    //
    //  5) Walk the INITV and INITO tables, in that order, and append 
    //     them to the end of the respective merged tables.  This completes
    //     the merge. TODO: avoid duplicates.
    //
    //  6) Walk the ST_ATTR tables and append them to the end of the
    //     merged table.
    //
    //  7) Delete all maps except those returned by this routine.
    //

    Merge_File_Info (IP_FILE_HDR_file_info (hdr));

    // Create the index maps we need to coordinate the original versus the
    // merged versions of the global symbol table, and initialize them to
    // hold undefined index values.
    //
    IPC_GLOBAL_IDX_MAP *idx_map =
	CXX_NEW (IPC_GLOBAL_IDX_MAP (original_tabs.st_tab_size,
				     original_tabs.ty_tab_size,
				     original_tabs.tcon_tab_size,
				     original_tabs.initv_tab_size,
				     mempool),
		 mempool);

    // Step 1 : Initialize the string table maps.
    //
    Merge_Strtab (original_tabs.symstr_tab,
		  original_tabs.symstr_tab_size,
		  idx_map->sym_str);

    TCONSTR_IDX_MAP tcon_str;
    Merge_TCON_Strtab (original_tabs.tconstr_tab,
		       original_tabs.tconstr_tab_size,
		       tcon_str);

    // Step 2 :  Complete the TY map, and merge the TYs into the merged
    // global symbol table.  Since types are entered in depth-first order, 
    // we may encounter types that are already entered which we skip here.
    //
    New_Ty_Idx = &idx_map->ty;
    New_St_Idx = &idx_map->st;
    New_Symstr_Idx = &idx_map->sym_str;
   
#ifdef Is_True_On
    if (getenv ("no_fast_merge")) {
	UINT idx;
	for (idx = 1; idx < original_tabs.ty_tab_size; idx++)
	    if (New_Ty_Idx->map_[idx] == 0) {
		BOOL is_new;
		TY_INDEX is_recursive;
		Merge_Global_Ty(make_TY_IDX (idx), original_tabs, is_new,
				is_recursive); 
	    }
    } else
#endif
	Merge_All_Types (original_tabs, *idx_map);

    UINT idx;
#ifdef Is_True_On

    // verify the map
    for (idx = 1; idx < original_tabs.ty_tab_size; ++idx)
	Is_True ((New_Ty_Idx->map_[idx] & 0xff) == 0, ("errors in TY_IDX maps"));

#endif

    //  Step 3 : Walk the TCON table and append the records to the
    //  end of the merged table.
    //

    Merge_Global_Tcon (original_tabs.tcon_tab, original_tabs.tcon_tab_size,
		       idx_map->tcon, tcon_str);


    //  Step 4 : Walk the ST table and merge the records into the merged
    //  global symbol table.

    // Initialize the ST to INITO map (in terms of original indices).
    // This will later be used to mark INITO's as invalid for cases when
    // an ST is initialized several times (the first initializer is chosen).
    // The "mark" is in form of INITO_st_idx() == UNINITIALIZED_INDEX.
    //
    //
    ST_TO_INITO_MAP st_to_inito_map;

    for (idx = 1; idx < original_tabs.inito_tab_size; idx++) {
	ST_IDX st_idx = INITO_st_idx(original_tabs.inito_tab[idx]);
	st_to_inito_map[ST_IDX_index (st_idx)] = idx;
    }

    St_To_Inito_Map = &st_to_inito_map;
    New_Tcon_Idx = &idx_map->tcon;
    current_file_hdr = &hdr;

    for (idx = 1; idx < num_predefined_st; ++idx)
	Verify_Predefined_Symbols (original_tabs.st_tab[idx]);

    for (idx = num_predefined_st; idx < original_tabs.st_tab_size; idx++)
	Merge_Global_St (idx, original_tabs);

    current_file_hdr = NULL;
    New_Ty_Idx = NULL;

   //  Step 5 : Walk the INITV and INITO tables and append the records
   //  to the end of the respective merged tables.
   //
    idx_map->initv.set_map (0, 0);
    for (idx = 1; idx < original_tabs.initv_tab_size; idx++)
	if (idx_map->initv[idx] == UNINITIALIZED_INDEX)
	    Merge_Global_Initv (idx, original_tabs, idx_map->initv);

    Merge_Global_Inito (original_tabs.inito_tab,
			original_tabs.inito_tab_size, idx_map->initv);

    //  Set 6 : Walk the ST_ATTR table and append the records to the end
    //  end of the merged table.
    //

    Merge_Global_St_Attr (original_tabs.st_attr_tab,
			  original_tabs.st_attr_tab_size);

    return idx_map;
} // IPC_merge_global_tab


// ======================================================================
//
// Handling of auxiliary symbol table information
//
// ======================================================================

static inline ST*
Get_base_st (ST* st)
{
    while (ST_base_idx (st) != ST_st_idx (st))
	st = &St_Table[ST_base_idx (st)];
    return st;
}


// update the mod/ref count of a symbol
void
Update_reference_count (ST* st, INT32 refcount, INT32 modcount,
			BOOL is_cmod)
{
    st = Get_base_st (st);
    AUX_ST& aux_st = Aux_St_Table[ST_st_idx (st)];

    if (is_cmod) {
	static BOOL reported = FALSE;
	if (!reported) {
	    DevWarn ("TODO: handle cmod");
	    reported = TRUE;
	}
	// compare if the initialized value is the same
    }

    Inc_AUX_ST_refcount (aux_st, refcount);
    Inc_AUX_ST_modcount (aux_st, modcount);

    if (ST_addr_saved (st) || ST_addr_passed (st))
	return;

    if (AUX_ST_flags (aux_st, IGNORE_REFCOUNTS))
	return;

    if (IPA_Enable_DVE && refcount < 0 && AUX_ST_refcount (aux_st) == 0) {
	Set_ST_is_not_used (st);
	if (Trace_IPA || Trace_Perf)
	    fprintf(TFile, "%s is marked NOT USED\n", ST_name (st));

	if ((ST_sclass(st) == SCLASS_COMMON) || (ST_sclass(st) == SCLASS_DGLOBAL)) {
            // Need to mark all its member not_used also
	    COMMON_BLOCK_ELEMENTS_MAP::const_iterator block_iter =
            Common_Block_Elements_Map->find(ST_st_idx(st));
    	    if (block_iter != Common_Block_Elements_Map->end()) {
	        const BLOCK_ELEMENTS* block = (*block_iter).second;
                BLOCK_ELEMENTS::const_iterator element = block->begin();
                BLOCK_ELEMENTS::const_iterator element_end = block->end();
	        for (; element != element_end; ++element) {
                    ST_IDX elem_st_idx = (*element).second;
		    Set_ST_is_not_used (St_Table[elem_st_idx]);
		    if (Trace_IPA || Trace_Perf)
            	        fprintf(TFile, "%s is marked NOT USED\n", ST_name (elem_st_idx));
	        }
	    }
	}
    } else if (IPA_Enable_CGI && modcount < 0 &&
	       AUX_ST_modcount (aux_st) == 0) {
	Set_ST_is_const_var (st);
	if (Trace_IPA || Trace_Perf)
	    fprintf(TFile, "%s is marked CONSTANT\n", ST_name (st));

        // Need to mark all its member not_used also
	if ((ST_sclass(st) == SCLASS_COMMON) || (ST_sclass(st) == SCLASS_DGLOBAL)) {
	    COMMON_BLOCK_ELEMENTS_MAP::const_iterator block_iter =
            Common_Block_Elements_Map->find(ST_st_idx(st));
	    if (block_iter != Common_Block_Elements_Map->end()) {
	        const BLOCK_ELEMENTS* block = (*block_iter).second;
                BLOCK_ELEMENTS::const_iterator element = block->begin();
                BLOCK_ELEMENTS::const_iterator element_end = block->end();
	        for (; element != element_end; ++element) {
                    ST_IDX elem_st_idx = (*element).second;
		    Set_ST_is_const_var (St_Table[elem_st_idx]);
		    if (Trace_IPA || Trace_Perf)
	    	        fprintf(TFile, "%s is marked CONSTANT\n", ST_name (elem_st_idx));
	        }
	    }
	}
    }
    
} // Set_reference_count


// merge in the result of the symbol resolution from the Elf symtab
void
Sync_symbol_attributes (ST_IDX st_idx, UINT32 sym_attr, BOOL is_weak,
			UINT32 export_type)
{
    // merge in symbol attributes from the Elf's symtab.
    Is_True ((sym_attr & ~OBJ_ATTR_MASK) == 0, ("invalid aux_st flags"));
    AUX_ST& aux_st = Aux_St_Table[st_idx];
    aux_st.flags |= sym_attr;

    ST& st = St_Table[st_idx];

    if (aux_st.flags & (USED_IN_OBJ|USED_IN_DSO|OBJ_COMMON))
	Clear_ST_is_not_used (st);
    if (aux_st.flags & ADDR_TAKEN_IN_OBJ) {
	Set_ST_addr_saved (st);
	Set_ST_addr_passed (st);
    }

    if (is_weak)
	Set_ST_is_weak_symbol (st);
    else if (ST_is_weak_symbol (st)) {
	// reset the weak flag
	Clear_ST_is_weak_symbol (st);
	Set_ST_base_idx (st, ST_st_idx(st));
    }

    switch (export_type) {
    case STO_DEFAULT:
    case STO_OPTIONAL:
	Set_ST_export (st, EXPORT_PREEMPTIBLE);
	break;

    case STO_PROTECTED:
	Set_ST_export (st, EXPORT_PROTECTED);
	break;

    case STO_HIDDEN:
	Set_ST_export (st, EXPORT_HIDDEN);
	break;

    case STO_INTERNAL:
	Set_ST_export (st, EXPORT_INTERNAL);
	break;
    }
	
} // Sync_symbol_attributes


// interface routine called from ld to mark a symbol not gp_rel
void
Linker_mark_not_gp_rel (ST_IDX st_idx)
{
    Is_True (st_idx != WHIRL_ST_IDX_UNINITIALIZED &&
	     st_idx != WHIRL_ST_IDX_NOT_AVAILABLE, ("Invalid ST_IDX"));

    ST& st = St_Table[st_idx];
    Set_ST_not_gprel (st);
}


// after merging all elements of a common block in a list (sorted by offsets
// and sizes), verify if any element overlaps.  If so, set the
// ST_is_equivalenced bit and clear the ST_is_split_common bit.
static void
Fix_Common_Block (const ST& block_st, const BLOCK_ELEMENTS *block)
{
    BLOCK_ELEMENTS::const_iterator element_iter = block->begin ();
    BLOCK_ELEMENTS::const_iterator element_end = block->end ();

    ST* last_st = NULL;
    UINT64 offset = 0;
    UINT64 size = 0;

    while (element_iter != element_end) {
	const TY& ty = *(*element_iter).first.first;
	UINT64 ofst = (*element_iter).first.second;
	ST_IDX st_idx = (*element_iter).second;
	ST& st = St_Table[st_idx];

	Is_True (ST_sym_class (st) == CLASS_VAR &&
		 (ST_export (st) == EXPORT_LOCAL ||
		  ST_export (st) == EXPORT_LOCAL_INTERNAL),
		 ("Expecting a common block elements"));

	// sync. up the ST attributes
	Set_ST_storage_class (st, ST_storage_class (block_st));
	if (ST_is_initialized (block_st)) {
	    Set_ST_is_initialized (st);
	    if (ST_init_value_zero (block_st))
		Set_ST_init_value_zero (st);
	}
	    
	if (ofst >= offset + size) {
	    offset = ofst;
	    size = TY_size (ty);
	    last_st = &st;
	} else {
	    // overlapped elements:  mark equivalence
	    Set_ST_is_equivalenced (st);
	    Set_ST_is_equivalenced (last_st);
	    if (ST_is_f90_target (st) || ST_is_f90_target (last_st)) {
		// propagate the f90_target bit, see PV 640442
		Set_ST_is_f90_target (st);
		Set_ST_is_f90_target (last_st);
	    }

	    if (ofst + TY_size (ty) >= offset + size) {
		offset = ofst;
		size = TY_size (ty);
		last_st = &st;
	    }
	}

	++element_iter;
    }
} // Fix_Common_Block


// return the number of FLD entries corresponding to the TY of a common block.
static UINT32
Num_Of_Elements (const ST& st)
{
    const TY& ty = Ty_Table[ST_type (st)];

    if (TY_kind (ty) != KIND_STRUCT)
	return UINT32_MAX;		// no longer a common block,
					// possibly preempted by a UGLOBAL
					// or DGLOBAL array.
    
    FLD_ITER iter = Make_fld_iter (TY_fld (ty));

    UINT count = 0;

    do {
	++count;
    } while (!FLD_last_field (iter++));

    return count;
} // Num_Of_Elements


static void
Fix_Common_Block_Type (ST& block_st, const BLOCK_ELEMENTS *block)
{
    CHECKPOINT chkpt;

    TY_IDX ty_idx = Copy_TY (ST_type (block_st));

    FLD_IDX first_field = FLD_Table_Size ();

    BLOCK_ELEMENTS::const_iterator element_iter = block->begin ();
    BLOCK_ELEMENTS::const_iterator element_end = block->end ();
    FLD_HANDLE fld;

    while (element_iter != element_end) {
	const ST& st = St_Table[(*element_iter).second];
	UINT64 ofst = (*element_iter).first.second;

	fld = New_FLD ();
	FLD_Init (fld, ST_name_idx (st), ST_type (st), ofst);
	if (ST_is_equivalenced (st))
	    Set_FLD_equivalence (fld);
	++element_iter;
    } 

    Set_FLD_last_field (fld);

    TY& ty = Ty_Table[ty_idx];
    Set_TY_fld (ty, FLD_HANDLE (first_field));

    if (ST_sclass (block_st) == SCLASS_DGLOBAL) {
	// check if any other common block is larger than the initialized
	// entry
	if (FLD_ofst (fld) + TY_size (FLD_type (fld)) > TY_size (ty))
	    Set_TY_size (ty, FLD_ofst (fld) + TY_size (FLD_type (fld)));
    }

    TY_IDX unique_idx = TY_is_unique (ty_idx);

    if (unique_idx != ty_idx) {
	// already have such a type, so undo the new entries and use the
	// existing one.
	chkpt.restore ();
	Set_ST_type (block_st, unique_idx);
    } else
	Set_ST_type (block_st, ty_idx);

} // Fix_Common_Block_Type


void
Verify_Common_Block_Layout ()
{
    COMMON_BLOCK_ELEMENTS_MAP::const_iterator block_iter =
	Common_Block_Elements_Map->begin ();
    COMMON_BLOCK_ELEMENTS_MAP::const_iterator block_end =
	Common_Block_Elements_Map->end ();

    while (block_iter != block_end) {
	ST& block_st = St_Table[(*block_iter).first];
	const BLOCK_ELEMENTS *block = (*block_iter).second;

	Fix_Common_Block (block_st, block);

	// check if the type of the block_st needs to be updated
	if (block->size () > Num_Of_Elements (block_st))
	    Fix_Common_Block_Type (block_st, block);

	++block_iter;
    }
} // Verify_Common_Block_Layout
