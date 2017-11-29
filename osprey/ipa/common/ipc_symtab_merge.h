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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
#ifndef ipc_symtab_merge_INCLUDED
#define ipc_symtab_merge_INCLUDED

/* The following constant values are shared by both ld and ipa.  Each
   symtab entry in ld's merged symbol table has an ST_IDX field pointing
   back to the corresponding entry (if any) in the WHIRL merged symbol
   table.  When a new entry is added to ld's merged symbol table, if the
   symbol comes from an Elf object, we set the ST_IDX field to
   WHIRL_ST_IDX_NOT_AVAILABLE because there is no corresponding ST entry in
   the WHIRL merged symbol table. If the symbol comes from a WHIRL object,
   we set the ST_IDX field to WHIRL_ST_IDX_UNINITIALIZED, which means that
   there will be a corresponding entry in the WHIRL merged symtab but we
   don't know the value yet.
 */

#define WHIRL_ST_IDX_UNINITIALIZED (0)
#define WHIRL_ST_IDX_NOT_AVAILABLE (-1)

#ifdef __cplusplus

#include <map>	


enum AUX_ST_FLAG
{
    // attributes from an Elf symbols

    USED_IN_OBJ		= 0x00000001,	// referenced in an Elf object
    USED_IN_DSO		= 0x00000002,	// referenced in a DSO
    DEF_IN_OBJ		= 0x00000004,	// defined in an Elf object
    DEF_IN_DSO		= 0x00000008,	// defined in a DSO
    OBJ_COMMON		= 0x00000010,	// defined in OBJ/DSO as common
    ADDR_TAKEN_IN_OBJ	= 0x00000020,	// address taken by Elf object or dso
	// Update Print_AUX_ST_flags when adding new attribute

    OBJ_ATTR_MASK	= 0x0000003f,	// mask for the above bits

    // attributes from an ST
    
    COMMON_USED_IN_IO   = 0x00000040,	// common block passed to IO routines
    IGNORE_REFCOUNTS	= 0x00000080	// ignore the mod/ref counts even
					// when they hit zero
};

extern "C" void
Sync_symbol_attributes (ST_IDX st_idx, UINT32 sym_attr, BOOL is_weak,
			UINT32 export_class);
#ifdef SHARED_BUILD
#pragma weak Sync_symbol_attributes
#endif

extern "C" void
Linker_mark_not_gp_rel (ST_IDX st_idx);
#ifdef SHARED_BUILD
#pragma weak Linker_mark_not_gp_rel
#endif

#endif 

#ifndef _LD_IPA_INTERFACE

#include <vector>

#ifndef mempool_allocator_INCLUDED
#include "mempool_allocator.h"
#endif

#ifndef symtab_INCLUDED
#include "symtab.h"
#endif

#ifndef ipa_cg_INCLUDED
#include "ipa_cg.h"			// for CALL_GRAPH_NODE
#endif 

const UINT32 UNINITIALIZED_INDEX = 0;  // The value of an uninitialized index mapd

typedef vector<mUINT32, mempool_allocator<mUINT32> > IPC_IDX_VECTOR;

// A structure to hold all the information we need about a global
// symbol table to merge it into the current merged global symbol
// table.
//
struct IPC_GLOBAL_TABS
{
   void *p_obj;			// The object file containing these tabs.
		     
   UINT32  symstr_tab_size;	// Size in number of bytes
   UINT32  tconstr_tab_size;
   UINT32  pu_tab_size;		// Size in number of elements
   UINT32  st_tab_size;
   UINT32  ty_tab_size;
   UINT32  tylist_tab_size;
   UINT32  fld_tab_size;
   UINT32  arb_tab_size;
   UINT32  tcon_tab_size;
   UINT32  inito_tab_size;
   UINT32  initv_tab_size;
   UINT32  st_attr_tab_size;
   
   char   *symstr_tab;  // NULL if empty
   char   *tconstr_tab;  // NULL if empty
   PU     *pu_tab;
   ST     *st_tab;
   TY     *ty_tab;
   TYLIST *tylist_tab;
   FLD    *fld_tab;
   ARB    *arb_tab;
   TCON   *tcon_tab;
   INITO  *inito_tab;
   INITV  *initv_tab;
   ST_ATTR *st_attr_tab;
};

// The following define some general purpose classes with uniform
// interfaces for manipulating the index maps that result from a 
// merge of the global symbol tables.
//

// An idx map where the IDX only contains an index value into the
// tables, with no additional info encoded as part of the idx
//
template <class MAP>
struct IDX_MAP
{
    MAP map;

    typedef typename MAP::allocator_type allocator_type;
    typedef typename MAP::value_type value_type;

    IDX_MAP (mUINT32 size = 0, allocator_type a = allocator_type()) :
	map (size, value_type(), a) {} 

    mUINT32 operator[] (mUINT32 old_idx) const {
	return map[old_idx];
    }

    void set_map (mUINT32 old_idx, mUINT32 new_idx) {
	map[old_idx] = new_idx;
    }

}; // IDX_MAP


// An ST IDX will also encode scope "level" infomation, which
// we want to pass through when looking up what a given IDX 
// refers to, but which we do not want to keep in the mapping.
// Hence, we define a specialized IDX_MAP class for this.
//
struct ST_IDX_MAP
{
    IPC_IDX_VECTOR map_;

    typedef IPC_IDX_VECTOR::allocator_type allocator_type;

    ST_IDX_MAP (mUINT32 size, allocator_type alloc) :
	map_ (size, UNINITIALIZED_INDEX, alloc) {}

    ST_IDX operator[] (ST_IDX old_idx) const {
	UINT idx = ST_IDX_index(old_idx);
        Is_True(ST_IDX_level(old_idx) == GLOBAL_SYMTAB,
                ("ST_IDX_MAP used for non-global symbol %ld:%ld",
                ST_IDX_level(old_idx), idx));
        Is_True(idx < map_.size(),
                ("ST_IDX_MAP: Index %ld out of range. (Max = %ld)",
                 idx, map_.size() - 1));
	return map_[idx];
    }
    
    void set_map(ST_IDX old_idx, ST_IDX new_idx) {
	Is_True (ST_IDX_level (old_idx) == GLOBAL_SYMTAB,
		 ("ST_IDX_MAP is for global symbols only"));
	UINT idx = ST_IDX_index(old_idx);
	map_[idx] = new_idx;
    }

}; // ST_IDX_MAP


// The TY IDX also encode additional information, and we define
// a special map for it in much the same way we did for the ST_IDX.
// We also add a few more methods to deal with recursively defined
// types when we merge symbol tables.
//

typedef UINT32 TY_INDEX;		// the index part of an TY_IDX

// we steal a TY_IDX bit for temporary marking of merge state
enum TY_MERGE_FLAG
{
    TY_INDEX_IS_MERGING		= TY_RESTRICT,
};

inline TY_IDX
Replace_TY_IDX_index (TY_IDX orig_idx, TY_IDX new_idx)
{
    return (new_idx & ~0xff) | (orig_idx & 0xff);
}


inline TY_IDX
make_TY_IDX (TY_INDEX idx, TY_IDX attributes)
{
    return (idx << 8) | (attributes & 0xff);
}


struct TY_IDX_MAP
{
    IPC_IDX_VECTOR map_;

    typedef IPC_IDX_VECTOR::allocator_type allocator_type;

    TY_IDX_MAP (mUINT32 size, allocator_type alloc) :
	map_ (size, UNINITIALIZED_INDEX, alloc) {}

    TY_IDX operator[] (TY_IDX old_idx) const {
	TY_IDX new_idx = map_[TY_IDX_index (old_idx)];
	return Replace_TY_IDX_index (old_idx, new_idx);
    }

    void set_map(TY_INDEX old_idx, TY_IDX new_idx) {
	// Will also reset any "set" "is_merging" flag!
	//
	map_[old_idx] = new_idx & ~0xff;
    }

    void set_is_merging (TY_INDEX old_idx) {
	map_[old_idx] |= TY_INDEX_IS_MERGING;
    }

    BOOL is_merging (TY_INDEX old_idx) {
	return map_[old_idx] & TY_INDEX_IS_MERGING;
    }

    void clear_all_index_flags (TY_INDEX old_idx) {
	map_[old_idx] &= ~0xff;
    }
}; // TY_IDX_MAP


typedef STR_IDX_MAP    SYMSTR_IDX_MAP;
typedef IDX_MAP<IPC_IDX_VECTOR> TCON_IDX_MAP;
typedef IDX_MAP<IPC_IDX_VECTOR> INITV_IDX_MAP;

struct IPC_GLOBAL_IDX_MAP
{
    SYMSTR_IDX_MAP  sym_str;
    ST_IDX_MAP      st;
    TY_IDX_MAP      ty;
    TCON_IDX_MAP    tcon;
    INITV_IDX_MAP   initv;

    IPC_GLOBAL_IDX_MAP (UINT32 st_size, UINT32 ty_size, UINT32 tcon_size,
			UINT32 initv_size, MEM_POOL *pool) :
	sym_str (0, __new_hash::hash<STR_IDX>(), std::equal_to<STR_IDX> (),
		 SYMSTR_IDX_MAP::allocator_type (pool)),
	st (st_size, ST_IDX_MAP::allocator_type (pool)),
	ty (ty_size, TY_IDX_MAP::allocator_type (pool)),
	tcon (tcon_size, TCON_IDX_MAP::allocator_type (pool)),
	initv (initv_size, INITV_IDX_MAP::allocator_type (pool)) {}
	
}; // IPC_GLOBAL_IDX_MAP

typedef hash_map<ST_IDX, INITO_IDX> ST_TO_INITO_MAP;
extern ST_TO_INITO_MAP ST_To_INITO_Map;

// ----------------------------------------------------------------------
// Keep track of all elements of a common block
// ----------------------------------------------------------------------

// the key for sorting the common block elements: type (size) and offset
typedef pair<const TY*, mUINT64> BLOCK_ELEMENT_DESC;

struct block_element_compare
{
    bool operator() (const BLOCK_ELEMENT_DESC& desc1,
		     const BLOCK_ELEMENT_DESC& desc2) const {
	if (desc1.second != desc2.second)
	    return desc1.second < desc2.second;
	if (TY_size (*desc1.first) != TY_size (*desc2.first))
	    return TY_size (*desc1.first) < TY_size (*desc2.first);
	return desc1.first < desc2.first;
    }
};

typedef std::map<const BLOCK_ELEMENT_DESC, ST_IDX, block_element_compare,
    mempool_allocator< std::pair<const BLOCK_ELEMENT_DESC, ST_IDX> > > BLOCK_ELEMENTS;
typedef hash_map<ST_IDX, BLOCK_ELEMENTS*, __gnu_cxx::hash<ST_IDX>, std::equal_to<ST_IDX>,
    mempool_allocator<BLOCK_ELEMENTS*> > COMMON_BLOCK_ELEMENTS_MAP;

extern COMMON_BLOCK_ELEMENTS_MAP *Common_Block_Elements_Map;

//----------------------------------------------------------------------
// Auxiliary tables for STs and PUs
//----------------------------------------------------------------------

struct AUX_ST
{
    mUINT32 flags;			// misc. attributes
    mUINT32 refcount;
    mUINT32 modcount;

    AUX_ST () : flags (0), refcount (0), modcount (0) {}

    void construct () {
	new (this) AUX_ST ();
    }
};

inline BOOL
AUX_ST_flags (const AUX_ST& aux_st, UINT32 f) {
    return aux_st.flags & f;
}
inline void
Set_AUX_ST_flags (AUX_ST& aux_st, UINT32 f) {
    aux_st.flags |= f;
}
inline void
Clear_AUX_ST_flags (AUX_ST& aux_st, UINT32 f) {
    aux_st.flags &= ~f;
}

inline UINT32
AUX_ST_refcount (const AUX_ST& aux_st) {
    return aux_st.refcount;
}
inline void
Inc_AUX_ST_refcount (AUX_ST& aux_st, INT32 count) {
    aux_st.refcount += count;
}

inline UINT32
AUX_ST_modcount (const AUX_ST& aux_st) {
    return aux_st.modcount;
}
inline void
Inc_AUX_ST_modcount (AUX_ST& aux_st, INT32 count) {
    aux_st.modcount += count;
}



typedef RELATED_SEGMENTED_ARRAY<AUX_ST,1024> AUX_ST_TAB;
extern AUX_ST_TAB Aux_St_Tab;

struct AUX_ST_TABLE
{
    AUX_ST& operator[] (ST_IDX st_idx) const {
	Is_True (ST_IDX_level(st_idx) == GLOBAL_SYMTAB, ("Invalid st_idx"));
	return Aux_St_Tab[ST_IDX_index (st_idx)];
    }

};

extern AUX_ST_TABLE Aux_St_Table;

struct AUX_PU
{
    NODE_INDEX node;		// index to the call graph node
    IP_FILE_HDR* file_hdr;		// file where this PU is defined
					// NULL if it is an extern function

    AUX_PU () : node (INVALID_NODE_INDEX), file_hdr (NULL) {}

    void construct () {
	new (this) AUX_PU ();
    }
};

inline void
Set_AUX_PU_node (AUX_PU& pu, NODE_INDEX node)
{
    pu.node = node;
}
inline NODE_INDEX
AUX_PU_node (const AUX_PU& pu)
{
    return pu.node;
}

inline void
Set_AUX_PU_file_hdr (AUX_PU& pu, IP_FILE_HDR* file_hdr)
{
    pu.file_hdr = file_hdr;
}
inline IP_FILE_HDR*
AUX_PU_file_hdr (const AUX_PU& pu)
{
    return pu.file_hdr;
}
	


typedef SEGMENTED_ARRAY<AUX_PU,256> AUX_PU_TAB;
extern AUX_PU_TAB Aux_Pu_Table;
    

extern void Initialize_Auxiliary_Tables ();

extern void Clear_Extra_Auxiliary_Tables ();

extern void Clear_Common_Block_Element_Map ();

extern IPC_GLOBAL_IDX_MAP *
IPC_merge_global_tab(const IPC_GLOBAL_TABS &original_tabs,
		     IP_FILE_HDR& hdr, MEM_POOL* mempool);

extern void
Update_reference_count (ST* st, INT32 refcount, INT32 modcount,
			BOOL is_cmod);

extern void
Print_AUX_ST_flags ( FILE *fp, const mUINT32 flags );

extern void
Verify_Common_Block_Layout ();

extern void
Synch_ST_flags (ST& merged_st, const ST& original_st);

#endif /* _LD_IPA_INTERFACE */
#endif /* ipc_symtab_merge_INCLUDED */
