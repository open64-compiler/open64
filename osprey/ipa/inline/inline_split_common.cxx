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
/* ====================================================================
 * ====================================================================
 *
 # inline_split_common: split commons
 *
 * Description:
 * Implementation of the utilities necessary to split common arrays
 *
 * ====================================================================
 * ====================================================================
 */

#include "cxx_memory.h"			// CXX_NEW, etc.
#include "tracing.h"			// for TFile
#include "symtab.h"			// symbol table



static BOOL trace_split_common = FALSE;
static UINT32 IPA_SPLIT_COMMON = 128;

// for each common block element, specify which split group it belongs to
class SPLIT_GROUP
{
private:
    UINT64 size;			// size of this element in bytes
    ST* st;				// common block element
    UINT32 group;			// split group it belongs to

public:
    SPLIT_GROUP (ST* s) :
	st (s),
	size (TY_size (ST_type (s))),
	group (0) {}

    ST* St () const { return st; }

    UINT64 Size () const { return size; }

    UINT64 Offset () const { return ST_ofst (st); }

    void Set_group (UINT32 g) { group = g; }
    UINT32 Group () const { return group; }
};


// vector of all elements of a given common block
typedef vector<SPLIT_GROUP, mempool_allocator<SPLIT_GROUP> > COMMON_BLOCK;


// description of each split group
class SPLIT_GROUP_DESC
{
private:
    UINT64 offset;
    UINT64 size;

public:
    SPLIT_GROUP_DESC (UINT64 ofst, UINT64 s) : offset (ofst), size (s) {}

    UINT64 Offset () const { return offset; }
    UINT64 Size () const { return size; }
};


// vector indexed by the split group id describing each split group
typedef vector<SPLIT_GROUP_DESC, mempool_allocator<SPLIT_GROUP_DESC> >
	SPLIT_BLOCK;


// for each common block, hashed by its name, map to the vector of elements 
// and description of how it is splitted
struct SPLIT_COMMON_DESC
{
    COMMON_BLOCK* common_block;
    SPLIT_BLOCK* split_block;

    SPLIT_COMMON_DESC () : common_block (NULL), split_block (NULL) {}
};

typedef hash_map<STR_IDX, SPLIT_COMMON_DESC, hash<STR_IDX>,
    equal_to<STR_IDX>, mempool_allocator<SPLIT_COMMON_DESC*> > 
	SPLIT_COMMON_TBL;


// ----------------------------------------------------------------------
// Analysis part:
//
// 1) Go through entire global symtab, collect all unintialized comnon
// blocks and their elements
// 2) Disgard those containing equivalences that prohibit splitting
// 3) Analyze the shape of each remaining common block to see if they
// can/should be splitted.  If so, specify how the splitting is done
// ----------------------------------------------------------------------

namespace {
    // unnamed namespace for function objects

    struct collect_commons {
	SPLIT_COMMON_TBL& table;
	MEM_POOL* pool;

	collect_commons (SPLIT_COMMON_TBL& t, MEM_POOL* p) :
	    table (t), pool (p) {}

	void operator() (UINT32, ST* st) const {
	    if (ST_sclass (st) == SCLASS_COMMON &&
		ST_base_idx (st) != ST_st_idx (st)) {

		STR_IDX block_name = ST_name_idx (St_Table[ST_base_idx (st)]);
		SPLIT_COMMON_DESC& desc = table[block_name];
		if (desc.common_block == NULL) {
		    // new common block
		    desc.common_block = CXX_NEW (COMMON_BLOCK (pool), pool);
		}

		desc.common_block->push_back (SPLIT_GROUP (st));
	    }
	}
    }; // collect_commons
    

    struct process_block_data {
	SPLIT_COMMON_TBL& table;

	process_block_data (SPLIT_COMMON_TBL& t) : table (t) {}

	void operator() (UINT32, const ST* st) const {
	    if (ST_sclass (st) == SCLASS_DGLOBAL) {
		SPLIT_COMMON_TBL::iterator block =
		    table.find (ST_name_idx (st));
		if (block != table.end ()) {
		    table.erase (block);
		}
	    }
	}
    }; // process_block_data
		

    struct block_element_compare {
	// sort by offset, and then (reversely) by size
	bool operator() (const SPLIT_GROUP& g1, const SPLIT_GROUP& g2) const {
	    if (g1.Offset () != g2.Offset ())
		return g1.Offset () < g2.Offset ();
	    if (g1.Size () != g2.Size ())
		return g1.Size () > g2.Size ();
	    return g1.St () < g2.St ();	// same shape, just a tie-breaker
	}
    }; // block_element_compare

}

// go through the symbol table and for each common block, build a vector of 
// all its element.  If the common block is initialized, remove it.
static void
Group_Synonymous_Commons (SPLIT_COMMON_TBL& table, MEM_POOL* pool)
{
    For_all (St_Table, GLOBAL_SYMTAB, collect_commons (table, pool));
    For_all (St_Table, GLOBAL_SYMTAB, process_block_data (table));

} // Group_Synonymous_Commons


// iterator adaptor for COMMON_BLOCK::iterator
// returns only the largest element in each split group.  This should also
// be the first element in each split group
struct SPLIT_GROUP_ITERATOR
{
    COMMON_BLOCK::iterator iter;
    COMMON_BLOCK::const_iterator last;
    UINT32 gp_id;
    
    SPLIT_GROUP_ITERATOR (COMMON_BLOCK::iterator i,
			  COMMON_BLOCK::const_iterator l) :
	iter (i), last (l), gp_id ((*i).Group ())  {}

    SPLIT_GROUP_ITERATOR (const SPLIT_GROUP_ITERATOR& i) :
	iter (i.iter), last (i.last), gp_id (i.gp_id) {}

    SPLIT_GROUP_ITERATOR& operator++ () {
	while (++iter != last) {
	    if ((*iter).Group () != gp_id) {
		gp_id = (*iter).Group ();
		return *this;
	    }
	}
	return *this;
    }

    SPLIT_GROUP& operator* () const {
	return *iter;
    }

    BOOL operator!= (COMMON_BLOCK::const_iterator i) const {
	return iter != i;
    }

    BOOL operator!= (const SPLIT_GROUP_ITERATOR& i) const {
	return iter != i.iter;
    }
};
    

// update the common bock elements group index
static inline void
Update_Group_Idx (COMMON_BLOCK::iterator first,
		  COMMON_BLOCK::const_iterator last, UINT group_idx)
{
    while (first != last) {
	(*first).Set_group (group_idx);
	++first;
    }
} // Update_Group_Idx 


// ===========================================================
// Dror's split needed heuristic. It uses cache sizes to
// determine if there is a benefit to splitting
// A common block is split only if the following criteria are met
//
//  1.  field represents an array >= 16384 bytes
//
//  2.  offset of field is >= 16384 - 819
//
//  3.  offset of field modulo 16384 is within 5% of 16384.
//      this offset is with respect to start of current split common block
//      as well as arrays >= 16384 in current split common block which
//      did not cause a split
//
//  4.  offset of field modulo 512 * 1024 is within 5% of 512 * 1024
//      (this evaluates to 26214).
//      this offset is with respect to start of current split common block
//      as well as arrays >=16384 in current split common block which
//      did not cause a split.
// ===========================================================
static const UINT Primary_Cache = 16*1024;
static const UINT Secondary_Cache = 512*1024;
static const UINT Primary_Delta = 819;	// 5% of primary cache
static const UINT Secondary_Delta = 26214; // 5% of secondary cache

static inline BOOL
need_to_split (UINT64 offset, UINT64 cache_size, UINT64 delta)
{
    offset %= cache_size;
    return (offset < delta) || (offset > cache_size - delta);
}

//---------------------------------------------------------------
static BOOL
Compute_Split_Regions (SPLIT_COMMON_DESC& common_desc, MEM_POOL* pool)
{
    COMMON_BLOCK& common_block = *common_desc.common_block;

    SPLIT_GROUP_ITERATOR current_split (common_block.begin (),
					common_block.end ());
    UINT64 offset = 0;
    UINT64 size = 0;
    UINT group_idx = 0;

    for (SPLIT_GROUP_ITERATOR iter (current_split);
	 iter != common_block.end ();
	 ++iter) {

	const SPLIT_GROUP& element = *iter;

	// conditions 1 and 2
	if (element.Size () < Primary_Cache ||
	    element.Offset () - offset < Primary_Cache - Primary_Delta) {
	    size = element.Offset () + element.Size () - offset;
	    continue;
	}

	BOOL split_useful = FALSE;
	for (SPLIT_GROUP_ITERATOR i (current_split); i != iter; ++i) {

	    const SPLIT_GROUP& i_elmt = *i;

	    // for arrays within the current split common that
	    // are >= Primary_Cache size
	    if (i_elmt.Size () >= Primary_Cache) {
		UINT64 current_offset = element.Offset () - i_elmt.Offset ();

		if (need_to_split (current_offset, Primary_Cache,
				   Primary_Delta) ||
		    need_to_split (current_offset, Secondary_Cache,
				   Secondary_Delta))
		    split_useful = TRUE;
	    }
	}

	if (split_useful) {
	    if (common_desc.split_block == NULL)
		common_desc.split_block = CXX_NEW (SPLIT_BLOCK (pool), pool);

	    Is_True (group_idx == common_desc.split_block->size (),
		     ("Inconsistent split common block descriptor"));

	    // update the group id of each common block element in this
	    // split group
	    Update_Group_Idx (current_split.iter, iter.iter, group_idx);

	    common_desc.split_block->push_back (SPLIT_GROUP_DESC (offset, size));
	    offset = element.Offset ();
	    size = element.Size ();
	    current_split = iter;
	    ++group_idx;
	} else
	    size = element.Offset () + element.Size () - offset;
    }

    if (common_desc.split_block == NULL)
	// nothing to split
	return FALSE;
    else {
	// include the last split region
	Update_Group_Idx (current_split.iter, common_block.end (), group_idx);
	common_desc.split_block->push_back (SPLIT_GROUP_DESC (offset, size));
	return TRUE; 
    }
} // Compute_Split_Regions


// check for bad equivalence in the common block
static BOOL
Consistent_Layout (COMMON_BLOCK& block)
{
    sort (block.begin(), block.end (), block_element_compare ());

    UINT64 offset = 0;
    UINT64 size = 0;
    INT32 group_idx = -1;

    for (COMMON_BLOCK::iterator iter = block.begin ();
	 iter != block.end ();
	 ++iter) {

	SPLIT_GROUP& element = *iter;
	UINT64 elm_ofst = element.Offset ();
	UINT64 elm_size = element.Size ();

	if (elm_ofst >= offset + size) {
	    ++group_idx;
	    offset = elm_ofst;
	    size = elm_size;
	} else if (elm_ofst + elm_size > offset + size) {
	    // overlapping block, cannot split
	    return FALSE;
	}

	element.Set_group (group_idx);
    }

    return group_idx > 0;
} // Consistent_Layout


// ===========================================================
// Top level routine for determining whether or not the
// synonymous common-blocks in a list can all be split
// ===========================================================
static BOOL
Split_Common_Possible (SPLIT_COMMON_DESC& common_desc, MEM_POOL* pool)
{
    // compute the split layout and check for consistency
    if (!Consistent_Layout (*common_desc.common_block))
	return FALSE;

    return Compute_Split_Regions (common_desc, pool);

} // Split_Common_Possible


// ===========================================================
// Print out the split information for the current common
// ===========================================================
static void
Print_Split (const char* name, FILE* fp, const SPLIT_COMMON_DESC& desc)
{
    fprintf(fp, "=======Recording Split Information for common %s ======= \n",
	    name); 

    if (desc.split_block == NULL)
	fputs ("Not splitted\n", fp);
    else {
	const SPLIT_BLOCK& split = *desc.split_block;
	for (COMMON_BLOCK::const_iterator iter (desc.common_block->begin ());
	     iter != desc.common_block->end ();
	     ++iter) {

	    UINT id = (*iter).Group ();
	    const ST* st = (*iter).St ();
	    fprintf (fp, "offset = %lld, size = %lld, split = %d: <%d,%d,%s>\n",
		     split[id].Offset (), split[id].Size (), id,
		     ST_level (st), ST_index (st), ST_name ((*iter).St ()));
	}
    }
} // Print_Split


//--------------------------------------------------------------
// IPA_Split_Analysis, determines if a common needs to be split
//--------------------------------------------------------------
static BOOL
Split_Analysis (SPLIT_COMMON_TBL& split_common_tbl, MEM_POOL* pool)
{
    if (trace_split_common)
	fprintf(TFile, "+++++ INLINE split common analysis ++++\n");

    // group synonomous common-blocks together
    Group_Synonymous_Commons (split_common_tbl, pool);

    BOOL contain_split_common = FALSE;

    for (SPLIT_COMMON_TBL::iterator iter = split_common_tbl.begin ();
	 iter != split_common_tbl.end ();
	 ++iter) {

	STR_IDX name = (*iter).first;
	SPLIT_COMMON_DESC& common_desc = (*iter).second;

	if (trace_split_common)
	    fprintf (TFile, "checking split needed for common %s \n",
		     Index_To_Str (name));

	// Check for any disabling flag in the summary information
	if (Split_Common_Possible (common_desc, pool)) {

	    contain_split_common = TRUE;

	    if (trace_split_common) {
		const char* common_name = Index_To_Str (name);
		fprintf(TFile, "split needed for common %s \n",
			common_name);
		Print_Split (common_name, TFile, common_desc);
	    }
	} else if (trace_split_common) {
	    fprintf (TFile, "split not needed for common %s \n",
		     Index_To_Str (name));
	}
    }

    return contain_split_common;
} // Split_Analysis



// ----------------------------------------------------------------------
// Transformation part:
//
// Based on the analysis, create new splitted common blocks and update the
// symbol table accordingly
// ----------------------------------------------------------------------

//--------------------------------------------------------------------
//
// 1) Create SCLASS_COMMON symbols for each split common
// 2) Update the ST entry for block elements belonging
//    to the split common
// 3) For the original common, create a new type entry containing the
//    split commons as fields
//
// Example /a/ x(1024,8), y(1024,8), z(1024,8)
//
// after split
// common a -> 3 fields a_0, a_1024*8, a_2*1024*8
// common a_0 -> 1 field x, ST_full points to a, ST_base of x points
// to a_0
// common a_1024*8 -> 1 field y, ST_full points to a, ST_base of y
// points to  a_1024*8
// common a_2*1024*8 -> 1 field z, ST_full points to a, ST_base of z
// points to a_2*1024*8
//
//--------------------------------------------------------------------

namespace {

    // ST_ITER is a forward iterator with value_type equals ST*
    // create a new FLD matching these ST's
    template <class ST_ITER>
    FLD_HANDLE
    make_fld (ST_ITER first, ST_ITER last, UINT64 new_ofst)
    {
	FLD_IDX first_field = FLD_Table_Size ();
	FLD_HANDLE fld;

	while (first != last) {
	    const ST* st = *first;
	    UINT64 offset = ST_ofst (st) - new_ofst;

	    fld = New_FLD ();
	    FLD_Init (fld, ST_name_idx (st), ST_type (st), offset);
	    if (ST_is_equivalenced (st))
		Set_FLD_equivalence (fld);
	    ++first;
	}

	Set_FLD_last_field (fld);

	return FLD_HANDLE (fld);
	
    } // make_fld    


    // set the st_base of the given range of ST's to the specified base
    template <class ST_ITER>
    void
    Update_element (ST_ITER first, ST_ITER last, ST_IDX base, UINT64 offset)
    {
	while (first != last) {
	    ST* st = *first;
	    Set_ST_base_idx (st, base);
	    Set_ST_ofst (st, ST_ofst (st) - offset);
	    ++first;
	}
    }

    // iterator adaptor that convert a COMMON_BLOCK iterator to an ST*
    // iterator 
    struct ST_ITERATOR {
	COMMON_BLOCK::const_iterator iter;

	ST_ITERATOR (COMMON_BLOCK::const_iterator i) : iter (i) {}

	ST* operator* () const {
	    return (*iter).St ();
	}

	ST_ITERATOR& operator++ () {
	    ++iter;
	    return *this;
	}

	BOOL operator!= (const ST_ITERATOR& i) const {
	    return iter != i.iter;
	}
	    
    };

}


// create a split common symbol
static ST*
Create_Split_Common (COMMON_BLOCK::const_iterator first,
		     COMMON_BLOCK::const_iterator last,
		     const SPLIT_GROUP_DESC& desc,
		     const ST* base)
{
    FLD_HANDLE fld = make_fld (ST_ITERATOR (first), ST_ITERATOR (last),
			       desc.Offset ());

    TY_IDX idx;
    TY& ty = New_TY (idx);
    TY_Init (ty, desc.Size (), KIND_STRUCT, MTYPE_M, 0);
    Set_TY_fld (ty, fld);
    Set_TY_split (ty);

    // copy the TY_IDX attributes from the based ST
    TY_IDX ty_idx = ST_type (base);
    Set_TY_IDX_index (ty_idx, TY_IDX_index (idx));

    // create the name
    char name[30];
    sprintf (name, ".%lld", desc.Offset ());

    ST* split_common = New_ST (GLOBAL_SYMTAB);
    ST_Init (split_common, Save_Str2 (ST_name (base), name), CLASS_VAR,
	     SCLASS_COMMON, ST_export (base), ty_idx);
    Set_ST_ofst (split_common, 0);
    split_common->flags = base->flags;
    split_common->flags_ext = base->flags_ext;
    Set_ST_is_split_common (split_common);
    Set_ST_full_idx (*split_common, ST_st_idx (base));

    Update_element (ST_ITERATOR (first), ST_ITERATOR (last),
		    ST_st_idx (split_common), desc.Offset ()); 

    return split_common;
} // Create_Split_Common


// create new TY for the based ST, and set the base of all the split common
// to the based ST
static void
Fixup_Base (const vector<ST*>& split_common, ST* base)
{
    FLD_HANDLE fld = make_fld (split_common.begin (), split_common.end (), 0);

    TY_IDX idx;
    TY& ty = New_TY (idx);
    TY_Init (ty, TY_size (ST_type (base)), KIND_STRUCT, MTYPE_M, 0);
    Set_TY_fld (ty, fld);

    TY_IDX ty_idx = ST_type (base);
    Set_TY_IDX_index (ty_idx, TY_IDX_index (idx));

    Set_ST_type (base, ty_idx);
    Update_element (split_common.begin (), split_common.end (),
		    ST_st_idx (base), 0);
} // Fixup_Base


static void
Fixup_Common (COMMON_BLOCK::const_iterator first,
	      COMMON_BLOCK::const_iterator last,
	      const SPLIT_BLOCK& split)
{
    ST* base = ST_base ((*first).St ());

    vector<ST*> split_common_st;
    while (first != last) {
	UINT group = (*first).Group ();
	COMMON_BLOCK::const_iterator split_pt (first);
	// find the range of elements in this split common
	while (++split_pt != last && (*split_pt).Group () == group);

	ST* split_st =
	    Create_Split_Common (first, split_pt, split[group], base);
	split_common_st.push_back (split_st);

	first = split_pt;
    }

    Fixup_Base (split_common_st, base);
} // Fixup_Common


namespace {
    struct split_common_compare {
	// sort by the ST_IDX of the base, then by offset then by size
	bool operator() (const SPLIT_GROUP& g1, const SPLIT_GROUP& g2) const {
	    if (ST_base_idx (g1.St ()) != ST_base_idx (g2.St ()))
		return ST_base_idx (g1.St ()) < ST_base_idx (g2.St ());
	    if (g1.Offset () != g2.Offset ())
		return g1.Offset () < g2.Offset ();
	    if (g1.Size () != g2.Size ())
		return g1.Size () < g2.Size ();
	    return g1.St () < g2.St ();	// same shape, just a tie-breaker
	}
    }; // split_common_compare
}


// Split this given block
// There may be multiple definition of this block (one from each PU)
static void
Split_Common_Block (const SPLIT_COMMON_DESC& common_desc)
{
    COMMON_BLOCK& common_block = *common_desc.common_block;

    // re-group the elements by their corresponding base (this should be
    // the same as re-grouping based on their defining PU
    sort (common_block.begin (), common_block.end (),
	  split_common_compare ());

    COMMON_BLOCK::const_iterator first (common_block.begin ());
    COMMON_BLOCK::const_iterator last (common_block.end ());
    while (first != last) {
	ST_IDX base = ST_base_idx ((*first).St ());
	COMMON_BLOCK::const_iterator next_block (first);
	while (++next_block != last) {
	    if (ST_base_idx ((*next_block).St ()) != base)
		break;
	}
	Fixup_Common (first, next_block, *common_desc.split_block);
	first = next_block;
    }	
} // Split_Common_Block


// Perform actual splitting of the common blocks based on the analysis
// result in split_common_tbl
static void
Split_Commons (const SPLIT_COMMON_TBL& split_common_tbl)
{
    for (SPLIT_COMMON_TBL::const_iterator iter (split_common_tbl.begin ());
	 iter != split_common_tbl.end ();
	 ++iter) {

	const SPLIT_COMMON_DESC& common_desc = (*iter).second;
	if (common_desc.split_block == NULL)
	    continue;

	Split_Common_Block (common_desc);

    }
} // Split_Commons

void
INLINE_Split_Common ()
{
    CXX_MEM_POOL split_pool ("INLINE_split_pool", 0);

    trace_split_common = Get_Trace(TP_INLINE, IPA_SPLIT_COMMON);
    
    SPLIT_COMMON_TBL
	split_common_tbl (20, hash<STR_IDX> (), equal_to<STR_IDX> (),
			  split_pool());
    
    if (Split_Analysis (split_common_tbl, split_pool())) {
	Split_Commons (split_common_tbl);
#ifdef Is_True_On
	Verify_SYMTAB (GLOBAL_SYMTAB);
#endif
    }
} 
