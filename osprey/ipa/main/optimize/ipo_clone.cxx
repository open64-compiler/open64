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


/* ====================================================================
 * ====================================================================
 *
 * Module: ipo_clone.cxx
 *
 * Description:
 *	Perform cloning of subroutines.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#include <alloca.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>		  
#endif /* defined(BUILD_OS_DARWIN) */

#include "defs.h"
#include "wn.h"	                        // WN
#include "cxx_memory.h"		        // CXX_NEW
#define USE_DST_INTERNALS
#include "dwarf_DST.h"	    
#include "dwarf_DST_producer.h"	    
#include "dwarf_DST_mem.h"	        // DST 
#include "pu_info.h"                    // PU_Info
#include "ipc_file.h"		        // IP_FILE_HDR
#include "ipc_bread.h"                  // IP_READ_file_info
#include "ipc_symtab_merge.h"           // AUX_PU
#include "config_ipa.h"                 // IPA_Enable_DST
#include "ipa_cg.h"		        // IPA_NODE
#include "clone_DST_utils.h"	        // DST_enter_cloned_subroutine
#include "ipo_defs.h"                   // Parent_Map

#include "ipo_clone.h"

//----------------------------------------------------------------------  
// Interface for the main IPA
// It's assumed that the cloned_node has already had the MEM_POOL m
// (cloned_node->GetM()) initialized and the bit SetMIndex() set.
//----------------------------------------------------------------------
void
IPO_Clone (IPA_NODE* orig_node, IPA_NODE* clone_node)
{
  IP_FILE_HDR& file_hdr = orig_node->File_Header();

  ST* orig_st = orig_node->Func_ST();
  
  // Create PU entry for the clone
  PU_IDX clone_pu_idx;
  PU& clone_pu = New_PU (clone_pu_idx);
  Pu_Table[clone_pu_idx] = Pu_Table[ST_pu(orig_st)];

  // Add clone to Aux_Pu_Table
  UINT32 clone_aux_idx;
  AUX_PU& clone_aux_pu = Aux_Pu_Table.New_entry (clone_aux_idx);
  Set_AUX_PU_node (clone_aux_pu, clone_node->Node_Index());
  Set_AUX_PU_file_hdr (clone_aux_pu, &file_hdr);

  Is_True (clone_aux_idx == clone_pu_idx,
           ("IPO_Clone: Aux_Pu_Table index (%u) != Pu_Table index (%u)",
            clone_aux_idx, clone_pu_idx));

  // Find the order number for this clone
  IPA_CLONE_ARRAY* clone_array = IPA_Call_Graph->Clone_Array (orig_node);
  Is_True (clone_array,
           ("IPO_CLONE: clone_array for node %s is NULL", ST_name(orig_st)));

  INT32 clone_num;
  for (clone_num = 0; clone_num < clone_array->Elements(); ++clone_num) {
    if ((*clone_array)[clone_num] == clone_node) {
      break;
    }
  }
  Is_True(clone_num < clone_array->Elements(),
          ("IPO_CLONE: clone_node not found in clone_array for node %s",
           ST_name(orig_st)));
    
  // Create new name and ST for the clone
  char* clone_name = (char*) alloca(strlen(orig_node->Name())+15);
  sprintf(clone_name, "%s..clone..%u", ST_name(orig_st), clone_num);
  ST* clone_st = New_ST (ST_level(orig_st));
  ST_Init (clone_st, 
           Save_Str(clone_name),     
           CLASS_FUNC, 
           SCLASS_TEXT,
           ST_export(orig_st),
           clone_pu_idx);

  clone_node->Set_Func_ST (clone_st);

  // create PU_Info struct for the clone
  PU_Info* clone_pu_info = IP_FILE_HDR_Add_New_PU (file_hdr);
  clone_node->Set_Proc_Info_Index (IP_FILE_HDR_num_procs(file_hdr)-1);

  IPO_CLONE clone (orig_node->Whirl_Tree(),
                   orig_node->Scope(),
                   orig_node->Lexical_Level(),
                   orig_node->Map_Table(),
                   clone_node->Mem_Pool(), 
                   clone_node->Mem_Pool());

  
  // save current pointers to standard memory pools
  MEM_POOL* save_pu_pool_ptr = MEM_pu_pool_ptr;
  MEM_POOL* save_wn_pool_ptr = WN_mem_pool_ptr;

  // set standard memory pools
  MEM_pu_pool_ptr = clone_node->Mem_Pool();
  WN_mem_pool_ptr = clone_node->Mem_Pool();

  // perform actual tree and symtab cloning
  clone.New_Clone (clone_st);

  clone_node->Set_Scope (clone.Get_Cloned_Symtab());
  clone_node->Set_Whirl_Tree (clone.Get_Cloned_PU());
  clone_node->Set_Parent_Map (clone.Get_parent_map ());

  clone_node->Scope()[orig_node->Lexical_Level()].st = clone_st;
  
  // set pu_info fields and state
  Set_PU_Info_tree_ptr (clone_pu_info, clone.Get_Cloned_PU());
  PU_Info_maptab (clone_pu_info) = clone.Get_Cloned_maptab ();
  PU_Info_proc_sym (clone_pu_info) = ST_st_idx (clone_st);

  Set_PU_Info_state (clone_pu_info, WT_TREE,     Subsect_InMem);
  Set_PU_Info_state (clone_pu_info, WT_SYMTAB,   Subsect_InMem);
  Set_PU_Info_state (clone_pu_info, WT_PROC_SYM, Subsect_InMem);
  //Set_PU_Info_state (clone_pu_info, WT_FEEDBACK, Subsect_InMem);

#ifdef KEY
  if (orig_node->Has_Feedback () && clone_node->Has_Feedback ())
  {
      IP_PROC_INFO& info = IP_FILE_HDR_proc_info (file_hdr)[orig_node->Proc_Info_Index()];
      const PU_Info * pu = IP_PROC_INFO_pu_info (info);
      const Pu_Hdr* orig_fb = (const Pu_Hdr*) PU_Info_feedback_ptr (pu);
      FEEDBACK * fb = CXX_NEW (FEEDBACK (PU_Info_tree_ptr (clone_pu_info),
      					 clone_node->Mem_Pool(),
					 orig_fb->pu_num_inv_entries,
					 orig_fb->pu_num_br_entries,
					 orig_fb->pu_num_loop_entries,
					 orig_fb->pu_num_scircuit_entries,
					 orig_fb->pu_num_call_entries,
					 orig_fb->pu_num_icall_entries,
					 orig_fb->pu_num_switch_entries,
					 orig_fb->pu_num_value_entries,
					 orig_fb->pu_num_value_fp_bin_entries,
					 orig_fb->runtime_fun_address), 
      			       clone_node->Mem_Pool());
      clone_node->Set_Feedback_Info (fb);

      // original frequency, i.e. sum of invocations of original-node and
      // cloned-node.
      FB_FREQ orig_freq = orig_node->Has_frequency() ? 
      			  orig_node->Get_frequency() : FB_FREQ_UNKNOWN;
      FB_FREQ clone_freq (0.0, TRUE);
      IPA_PRED_ITER preds (IPA_Call_Graph, clone_node);
      for (preds.First(); !preds.Is_Empty(); preds.Next())
      {
      	IPA_EDGE * e = preds.Current_Edge();
	if (e && e->Has_frequency())
	  clone_freq += e->Get_frequency();
      }
      FB_IPA_Clone(orig_node->Feedback_Info(), clone_node->Feedback_Info(),
                    orig_node->Whirl_Tree (FALSE), clone.Get_Cloned_PU(),
                    clone_freq/orig_freq);
  }
#endif

  DST_IDX clone_dst = DST_INVALID_IDX;
  // this is a way to tell dbx to do more work for the given CU
  // doesn't mean that CU has to have inlined functions
  Set_FILE_INFO_has_inlines(File_info);
  clone_dst = DST_enter_cloned_subroutine (DST_get_compile_unit(), 
                                           orig_node->Dst_Index(), 
                                           clone.Get_Func_ST(), 
                                           orig_node->File_Dst(), 
                                           clone.Get_sym());
  Set_PU_Info_pu_dst (clone_pu_info, clone_dst);

  // Restore pointers to standard memory pools
  MEM_pu_pool_ptr = save_pu_pool_ptr;
  WN_mem_pool_ptr = save_wn_pool_ptr;

} // IPO_Clone


//----------------------------------------------------------------------
// copy the tree
//----------------------------------------------------------------------
WN *
IPO_Copy_Tree (WN *wn)
{
    extern WN_MAP Parent_Map;
    
    IPO_CLONE copy (Current_Map_Tab, Current_Map_Tab, Parent_Map);

    WN* result = copy.Clone_Tree (wn);

    return result;
} // IPO_Copy_Tree



//----------------------------------------------------------------------
// constructor for the cloned node
//----------------------------------------------------------------------
CLONED_NODE::CLONED_NODE(ST* orig, ST* copy) : _st(orig), _copy_st(copy) 
{
}

//----------------------------------------------------------------------
// clone list, append an entry
//----------------------------------------------------------------------
void
CLONED_LIST::Append(ST* orig_st, ST* copy_st, MEM_POOL *m)
{

  CLONED_NODE *clone_node;

  clone_node = (CLONED_NODE*)CXX_NEW(CLONED_NODE(orig_st, copy_st),
				     m);

  SLIST::Append((SLIST_NODE*)clone_node);
}

//----------------------------------------------------------------------
// clone list, lookup an entry
//----------------------------------------------------------------------
ST* 
CLONED_LIST::Lookup(ST* orig_st)
{
  CLONED_LIST_ITER list_iter(this);
  for (list_iter.First(); !list_iter.Is_Empty();
       list_iter.Next())
    {
      CLONED_NODE *tmp = list_iter.Cur();
      if (tmp->Get_orig_st() == orig_st)
	return tmp->Get_copy_st();
    }
  return NULL; // not found;
}

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

//----------------------------------------------------------------------
// fix up the symbol table entry of the call
// w = callsite node
// s = cloned routine's symbol table entry
// callee_sym = callee's symbol table
// m = mem pool to be 
// note, file and pu scoped caller mem pools must be set 
//----------------------------------------------------------------------
extern void 
Clone_update_st(WN *w, ST_IDX s, SYMTAB_IDX callee_sym_idx, 
                MEM_POOL*m,
                IPA_NODE *callee)
{
    IPO_SYMTAB *symtab;

    MEM_POOL_Push (m);

    symtab = (IPO_SYMTAB*) CXX_NEW (IPO_SYMTAB (callee->Scope(), callee_sym_idx, m), m);
    
    ST *cp = symtab->Get_Cloned_ST (ST_ptr(s)); // Local offset maps
    WN_st_idx(w) = ST_st_idx(cp);

    MEM_POOL_Pop (m);

}

#endif /* // STANDALONE_INLINER */
