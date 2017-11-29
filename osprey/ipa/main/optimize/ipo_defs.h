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

// Definitions that are needed by almost every ipo_*.cxx.

#ifndef ipo_defs_INCLUDED
#define ipo_defs_INCLUDED


#ifndef symtab_INCLUDED
#include "symtab.h"
#endif

#ifndef cxx_ipa_cg_INCLUDED
#include "ipa_cg.h"
#endif

extern "C" WN_MAP RID_map;		// #include "region_util.h" 
extern WN_MAP Parent_Map;		// #include "ipo_main.h" 

//----------------------------------------------------------------------
// Save/restore the context of an IPA_NODE
//----------------------------------------------------------------------
class IPA_NODE_CONTEXT
{
private:

    SYMTAB_IDX _current_scope;
    SCOPE* _scope_tab;
    PU* _current_pu;
    PU_Info* _current_pu_info;

    MEM_POOL* src_pool;
    MEM_POOL* pu_pool;
    MEM_POOL* wn_pool;

    WN_MAP parent_map;
    WN_MAP region_map;
    WN_MAP_TAB* map_tab;

    DST_TYPE dst;
    FEEDBACK* feedback;

public:

    IPA_NODE_CONTEXT (IPA_NODE *node) {
	_current_scope = CURRENT_SYMTAB;
	_scope_tab = Scope_tab;
	_current_pu = Current_pu;
	_current_pu_info = Current_PU_Info;
	src_pool = MEM_src_pool_ptr;
	pu_pool = MEM_pu_pool_ptr;
	wn_pool = WN_mem_pool_ptr;
	parent_map = Parent_Map;
	region_map = RID_map;
	map_tab = Current_Map_Tab;
        dst = Current_DST;
	feedback = Cur_PU_Feedback;

#ifdef KEY
	// Support these as necessary.
	if (node->Is_Builtin()) {
	  MEM_src_pool_ptr = NULL;
	  Current_PU_Info = node->Builtin_PU_Info();
	  Current_DST = NULL;
	  Parent_Map = 0;
	} else
#endif
	{
	MEM_src_pool_ptr = IP_FILE_HDR_mem_pool (node->File_Header ());
	Current_PU_Info = node->PU_Info ();
        Current_DST = node->File_Dst();
	Parent_Map = node->Parent_Map();
	}
	Current_Map_Tab = PU_Info_maptab (Current_PU_Info);
	WN_mem_pool_ptr = node->Mem_Pool();
	MEM_pu_pool_ptr = WN_mem_pool_ptr;
	Current_pu = &(node->Get_PU ());
	CURRENT_SYMTAB = PU_lexical_level (*Current_pu);
	Scope_tab = node->Scope ();
	Cur_PU_Feedback = node->Feedback_Info ();
    }

    ~IPA_NODE_CONTEXT () {
	CURRENT_SYMTAB = _current_scope;
	Scope_tab = _scope_tab;
	Current_pu = _current_pu;
	Current_PU_Info = _current_pu_info;
	MEM_src_pool_ptr = src_pool;
	WN_mem_pool_ptr = wn_pool;
	MEM_pu_pool_ptr = pu_pool;
	Parent_Map = parent_map;
	RID_map = region_map;
	Current_Map_Tab = map_tab;
        Current_DST = dst;
	Cur_PU_Feedback = feedback;
    }
}; // IPA_NODE_CONTEXT

#endif//  ipo_defs_INCLUDED
