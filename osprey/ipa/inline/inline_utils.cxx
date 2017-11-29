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
// Module: inline_utils.cxx
//
// Description:
//
// Standalone (intra-file) inliner.
//
// ====================================================================
//  Copied from ld/process.c
// ====================================================================

#ifdef __MINGW32__
#include <WINDOWS.h>
#endif /* __MINGW32__ */
#include <stdint.h>
#include <map>				// STL map
#include <cxx_memory.h>
#include "inline_utils.h"

IP_FILE_HDR&
Setup_Inliner_File_Header (char *input_name, void *mmap_addr)
{
    UINT index;

    IP_FILE_HDR& file_header = IP_File_header.New_entry (index);
    new (&file_header) IP_FILE_HDR (input_name, mmap_addr, 0);

    return file_header;
} // Setup_Inliner_File_Header


// ======================================================================
//
// mark all common blocks as aliased by setting the ST_base_idx field.
//
// ======================================================================

// for each common block, we have a vector holding the ST_IDX of all
// its elements.  The first elemetn is always the ST_IDX of the common
// block symbol
    
typedef vector<ST*, mempool_allocator<ST*> > COMMON_BLOCK;

// comparison function for sorting common blocks
struct compare_block
{
    bool operator() (const ST* st1, const ST* st2) const {
	if (st1 == st2)
	    return false;
	
	if (ST_name_idx (st1) != ST_name_idx (st2))
	    return ST_name_idx (st1) < ST_name_idx (st2);
	if (ST_sclass (st1) != ST_sclass (st2)) 
	    return ST_sclass (st1) == SCLASS_DGLOBAL;
	UINT64 size1 = TY_size (ST_type (st1));
	UINT64 size2 = TY_size (ST_type (st2));
	if (size1 != size2)
	    return size1 > size2;	// put the largest block first
	return st1 < st2;
    }
};

typedef std::map<const ST*, COMMON_BLOCK*, compare_block, 
                 mempool_allocator <std::pair<const ST* const, COMMON_BLOCK*> > > 
        COMMON_MAP;

// function object to collect all common block and blockdata, and put their 
// elements in a list
struct collect_commons
{
    COMMON_MAP& common_map;
    MEM_POOL* pool;

    collect_commons (COMMON_MAP& cm, MEM_POOL* p) :
	common_map (cm), pool (p) {}

    COMMON_BLOCK* get_common_block (ST* st) const {
	COMMON_MAP::iterator iter = common_map.find (st);
	if (iter == common_map.end ()) {
	    COMMON_BLOCK* p = CXX_NEW (COMMON_BLOCK (pool), pool);
	    common_map[st] = p;
	    p->push_back (st);
	    return p;
	}
	return (*iter).second;
    }

    ST* ST_raw_base_idx (ST* st) const {
	if (st->base_idx == ST_st_idx (st))
	    return st;
	ST* base_st = &St_Table[st->base_idx];
	while (ST_st_idx (base_st) != base_st->base_idx)
	    base_st = &St_Table[base_st->base_idx];
	return base_st;
    }

    void operator() (UINT32, ST* st) const {
	if (ST_sclass (st) == SCLASS_DGLOBAL ||
	    ST_sclass (st) == SCLASS_COMMON) {

	    if (ST_export (st) == EXPORT_LOCAL ||
		ST_export (st) == EXPORT_LOCAL_INTERNAL) {
		// a block element
		if (ST_base_idx (st) == ST_st_idx (st))
		    return;
	    }
	    
	    ST* base_st = ST_raw_base_idx (st);
	    COMMON_BLOCK* p = get_common_block (base_st);
	    p->push_back (st);
	}
    }
};


// increase the size of the based symbol
static void
Fix_Base_ST (ST* base_st, UINT64 size)
{
    TY_IDX ty_idx = Copy_TY (ST_type (base_st));
    Set_TY_size (Ty_Table[ty_idx], size);

    TY_IDX unique_idx = TY_is_unique (ty_idx);
    if (unique_idx != ty_idx) {
        // already have such a type, so undo the new entries and use the
        // existing one.
	Ty_tab.Delete_last ();
        Set_ST_type (base_st, unique_idx);
    } else
        Set_ST_type (base_st, ty_idx);

} // Fix_Base_ST

// set the base_idx of the common block to point to base_st
// if their storage class is different, propagate the base_st's storage
// class to the common block and its elements.
static void
Mark_Alaised (ST* base_st, const COMMON_BLOCK* block)
{
    ST* block_st = block->front ();	// first element is the common
					// block ST


    if (ST_sclass (base_st) == SCLASS_DGLOBAL &&
	TY_size (ST_type (base_st)) < TY_size (ST_type (block_st))) {
	// the initialized block is smaller than the largest common block,
	// this is actually a source code error. But we can fix this by
	// allocating more space.
	Fix_Base_ST (base_st, TY_size (ST_type (block_st)));
    }

    Set_ST_base_idx (block_st, ST_st_idx (base_st));

    if (ST_sclass (base_st) == ST_sclass (block_st))
	return;

    for (COMMON_BLOCK::const_iterator iter (block->begin ());
	 iter != block->end (); ++iter) {
	
	Set_ST_sclass (*iter, ST_sclass (base_st));
	// sync. up the ST attributes
	if (ST_is_initialized (base_st)) {
	    Set_ST_is_initialized (*iter);
	    if (ST_init_value_zero (base_st))
		Set_ST_init_value_zero (*iter);
	}
    }
    
} // Mark_Aliased


// The frontend sometimes might create multiple ST entries in the global
// symbol table refering to the same common block.  Typically, this is the
// case in fortran where the frontend creates a new ST entry for each
// common block declared in each PU.  With any interprocedural code motion
// (such as inlining), we might end up with two different STs being
// referenced in the same PU, when these two ST entries actually refer to
// the same common block (or same memory location).  Hence, we need to mark 
// these ST entries as aliased.
void
Fix_Aliased_Commons ()
{
    CXX_MEM_POOL pool ("aliased_commons", FALSE);

    compare_block tmp;
    COMMON_MAP common_map (tmp, pool());

    For_all (St_Table, GLOBAL_SYMTAB, collect_commons (common_map, pool()));

    COMMON_MAP::const_iterator iter (common_map.begin ());
    while (iter != common_map.end ()) {
	ST* base_st = iter->second->front ();
	++iter;
	while (iter != common_map.end () &&
	       ST_name_idx (base_st) == ST_name_idx (iter->second->front ())) {
	    // found two common blocks with same name
	    Mark_Alaised (base_st, iter->second);
	    ++iter;
	}
    }
} // Fix_Aliased_Commons

#ifdef _LIGHTWEIGHT_INLINER

#include <unistd.h>                 /* for unlink() */
#include <fcntl.h>                  /* for open() */
#ifndef __MINGW32__
#include <sys/mman.h>               /* for mmap() */
#endif /* __MINGW32__ */
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>                    /* for all Elf stuff */
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>          /* for WHIRL sections' sh_info */
#include "defs.h"                   /* for wn_core.h */

#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "ir_bread.h"

#include "wn_tree_util.h"
#include "ipa_cg.h"

//---------------------------------------------------------------------
// Check to see if this procedure contains calls to inlinable functions
//---------------------------------------------------------------------
BOOL 
Need_To_Inline_Callee (WN *w)
{
    WN* w2;

    // walk the tree
    for (WN_TREE_ITER<PRE_ORDER, WN*> iter (w); iter.Wn () != NULL; ++iter) {

	w2 = iter.Wn ();
	
	switch (WN_operator(w2)) {
	      
	case OPR_CALL: 

	//    if (Is_Node_Inlinable_In_Call_Graph(ST_st_idx(WN_st(w2)))) {
	    if (PU_is_inline_function(Pu_Table[ST_pu(WN_st(w2))])) {
#ifdef DEBUG
	        printf ("Need to inline %s into %s\n", ST_name(WN_st(w2)), ST_name(WN_st(w));

#endif // DEBUG
		return TRUE;
	    } 
	    break;
        
	default: 
	    break;
	}
    }

    return FALSE;

} // Need_To_Inline_Callee



void
Copy_Input_Info_To_Output(char *input_name, char *output_name)
{

    off_t mapped_size;
#ifdef __MINGW32__
    HANDLE handle;
    void *i_handle = WN_open_input (input_name, &mapped_size, 0, &handle);
#else
    void *i_handle = WN_open_input (input_name, &mapped_size);
#endif // __MINGW32__
    void *o_handle = WN_open_output(output_name);
    (void) ir_b_copy_file (i_handle, mapped_size, o_handle);
    WN_close_file(o_handle);
}


#endif // _LIGHTWEIGHT_INLINER
