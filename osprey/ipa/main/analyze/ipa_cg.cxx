/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
// ====================================================================
// ====================================================================
//
// Module: ipa_cg.cxx
//
// Revision history:
//  19-Oct-94 - Original Version
//
// Description:
//
// Implementation of the callgraph used in IPA's analysis and
// optimization phases.  
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>
#include <alloca.h>

#include <ext/hash_map>

#include <sys/types.h>

#include "defs.h"
#include "erglob.h"			// error message strings
#include "mempool.h"			// memory pools
#include "cxx_memory.h"			// CXX_NEW, etc.
#include "wn.h"				// whirl definitions
#include "wn_util.h"                    // WN_ITER
#include "dwarf_DST.h"			// for DST_IDX
#include "pu_info.h"			// PU_Info
#include "ir_bread.h"			// WN_get_section_base ()
#include "region_util.h"		// for WN_Fake_Call_EH_Region

#include "ipc_file.h"			// file header defs.
#include "ipc_symtab_merge.h"		// Aux_XX_Tables
#include "ipc_option.h"         	// -INLINE options
#include "ipc_bread.h"			// IP_READ_*
#include "ipa_cprop_annot.h"            // Edges_Have_Equiv_Cprop_Annots
#include "ipa_feedback.h"		// feedback related types
#include "ipa_option.h"			// option flags
#include "ipa_summary.h"		// IPA_get_*
#include "ipa_section.h"                // IVAR
#include "ipo_parent.h"			// WN_Parentize
#include "ipo_clone.h"                  // IPO_Clone
#include "ipo_defs.h"                   // IPA_NODE_CONTEXT
#include "ipaa.h"			// IPAA_NODE_INFO
#include "ipa_be_summary.h"             // SUMMARY_CONSTRAINT_GRAPH_*

#include "ipa_nested_pu.h"
#include "ipa_cg.h"
#include "ipa_inline.h"

#include "symtab_idx.h"         //for make_TY_IDX()-- in reorder
#include "ipa_reorder.h"        //for merged_access --reorder
#include "ipa_option.h"         // for IPA_Enable_Reorder and Merge_struct_access();
#include "opt_defs.h"           // -ttALI tracing options
#include "ir_reader.h"

#if !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
#include "ipa_nystrom_alias_analyzer.h"
#endif

IPA_CALL_GRAPH* IPA_Call_Graph;     // "The" call graph of IPA
#ifdef KEY
// Temporary graph built for pu-reordering based on edge frequencies.
IPA_CALL_GRAPH* IPA_Graph_Undirected;
// IPA_Call_Graph is a global variable used widely, even in member functions
// of IPA_CALL_GRAPH (where it is appropriate to either use 'this' or 
// nothing). Building another call graph (IPA_Graph_Undirected) becomes
// non-trivial, since the process starts using IPA_Call_Graph instead. So
// when we process IPA_Graph_Undirected, we store IPA_Call_Graph in
// IPA_Call_Graph_Tmp, and use IPA_Call_Graph and IPA_Graph_Undirected
// interchangeably.
static IPA_CALL_GRAPH* IPA_Call_Graph_Tmp = NULL;
// hash_map mapping nodes in IPA_Call_Graph to IPA_Graph_Undirected, and
// vice versa.
static hash_map<IPA_NODE*, IPA_NODE*, hashfn, eqnode> node_map;
static vector<Nodes_To_Edge *> q_order;
#endif
BOOL IPA_Call_Graph_Built = FALSE;

typedef hash_map<NODE_INDEX, NODE_INDEX> ALT_ENTRY_MAP;
ALT_ENTRY_MAP *alt_entry_map;		// map from alt entry to base entry

UINT32 Total_Dead_Function_Weight = 0;
UINT32 Orig_Prog_Weight = 0;

//INLINING_TUNING^
UINT32 Orig_Prog_WN_Count = 0;
UINT32 Total_Dead_Function_WN_Count = 0;
#ifdef KEY
FB_FREQ Total_cycle_count_2(0.0);
#else
FB_FREQ Total_cycle_count_2(0);
#endif
//INLINING_TUNING$

INT Total_Must_Inlined = 0;
INT Total_Must_Not_Inlined = 0;
#ifdef KEY
FB_FREQ Total_call_freq(0.0);
FB_FREQ Total_cycle_count(0.0);
#else
FB_FREQ Total_call_freq(0);
FB_FREQ Total_cycle_count(0);
#endif

//-----------------------------------------------------------------------
// NAME: Main_Entry
// FUNCTION: Return the main entry point for the possible alternate entry
//   'ipan_alt'.  
//-----------------------------------------------------------------------

extern IPA_NODE* Main_Entry(IPA_NODE* ipan_alt)
{
  if (alt_entry_map == NULL)
    return ipan_alt; 
  NODE_INDEX alt_entry_index = INVALID_NODE_INDEX;
  if (ipan_alt->Summary_Proc()->Is_alt_entry()) {
    NODE_INDEX v = ipan_alt->Node_Index();
    ALT_ENTRY_MAP::iterator iter = alt_entry_map->find(v);
    if (iter != alt_entry_map->end())
      alt_entry_index = (*iter).second;
  } 
  if (alt_entry_index == INVALID_NODE_INDEX)
    return ipan_alt;
  return IPA_Call_Graph->Graph()->Node_User(alt_entry_index);  
} 


#ifndef _LIGHTWEIGHT_INLINER

// ---------------------------------------------------------------
// Update summary ST_IDX's so that they point to the merged symtab
// ---------------------------------------------------------------
void
IPA_update_summary_st_idx (const IP_FILE_HDR& hdr)
{
  const IPC_GLOBAL_IDX_MAP* idx_maps = IP_FILE_HDR_idx_maps(hdr);
  INT i;

  // process all global ST_IDXs found in SUMMARY_SYMBOLs
  INT32 num_symbols;
  SUMMARY_SYMBOL* symbols = IPA_get_symbol_file_array(hdr, num_symbols);
  for (i = 0; i < num_symbols; ++i) {
    // a summary symbol can have two ST_IDXs:
    // one for the symbol itself 
    ST_IDX old_st_idx = symbols[i].St_idx();
    if (ST_IDX_level(old_st_idx) == GLOBAL_SYMTAB) {
      symbols[i].Set_st_idx(idx_maps->st[old_st_idx]);
    }
    // and the other for the parent function
    ST_IDX old_func_st_idx = symbols[i].Get_st_idx_func();
    if (ST_IDX_level(old_func_st_idx) == GLOBAL_SYMTAB) {
      symbols[i].Set_st_idx_func(idx_maps->st[old_func_st_idx]);
    }
  }
  
  // process all global ST_IDXs found in SUMMARY_VALUEs
  INT32 num_values; 
  SUMMARY_VALUE* values = IPA_get_value_file_array(hdr, num_values);
  for (INT j = 0; j < num_values; ++j) {
    if (values[j].Is_const_st()) {
      ST_IDX old_const_st_idx = values[j].Get_const_st_idx();
      if (ST_IDX_level(old_const_st_idx) == GLOBAL_SYMTAB) {
        values[j].Set_const_st_idx(idx_maps->st[old_const_st_idx]);
        values[j].Set_merged_const_st_idx();
      }
    } else if (values[j].Is_global () &&
	       values[j].Get_global_index () == -1) {
      ST_IDX old_global_st_idx = values[j].Get_global_st_idx ();
      if (ST_IDX_index (old_global_st_idx) != 0) {
	values[j].Set_global_st_idx (idx_maps->st[old_global_st_idx]);
      }
    }
  }

  // process all TY_IDXs found in SUMMARY_FORMALs
  INT32 num_formals;
  SUMMARY_FORMAL* formals = IPA_get_formal_file_array(hdr, num_formals);
  for (i = 0; i < num_formals; ++i) {
    TY_IDX old_ty_idx = formals[i].Get_ty();
    if (old_ty_idx) {
      formals[i].Set_ty(idx_maps->ty[old_ty_idx]);
    }
  }
  
  // process all TY_IDXs found in SUMMARY_ACTUALs
  INT32 num_actuals;
  SUMMARY_ACTUAL* actuals = IPA_get_actual_file_array(hdr, num_actuals);
  for (i = 0; i < num_actuals; ++i) {
    TY_IDX old_ty_idx = actuals[i].Get_ty();
    if (old_ty_idx) {
      actuals[i].Set_ty(idx_maps->ty[old_ty_idx]);
    }
  }

  // process all TY_IDXs found in SUMMARY_CALLSITEs 
  INT32 num_callsites; 
  SUMMARY_CALLSITE *callsites = IPA_get_callsite_file_array(hdr, num_callsites); 
  for (i = 0; i < num_callsites; ++i) { 
    TY_IDX old_ty_idx = callsites[i].Get_virtual_class(); 
    if (old_ty_idx) { 
      callsites[i].Set_virtual_class(idx_maps->ty[old_ty_idx]); 
    } 
  } 
 
  // process all ST_IDXs found in IVARs
  INT32 num_ivars;
  IVAR* ivars = IPA_get_ivar_file_array(hdr, num_ivars);
  for (i = 0; i < num_ivars; ++i) {
    if (!ivars[i].Is_Formal()) {
      Is_True(ST_IDX_level(ivars[i].St_Idx()) == GLOBAL_SYMTAB,
              ("Non-formal IVAR must have a global ST in IPA"));
      ivars[i].Set_St_Idx(idx_maps->st[ivars[i].St_Idx()]);
    }
  }

#ifdef KEY
  INT32 num_ty_infos;
  SUMMARY_TY_INFO* ty_infos = IPA_get_ty_info_file_array(hdr, num_ty_infos);
  for (i = 0; i < num_ty_infos; ++i) {
    TY_IDX old_ty_idx = ty_infos[i].Get_ty();
    Is_True (old_ty_idx, ("Non-zero type ids expected in SUMMARY_TYPE"));
    if (old_ty_idx) {
      ty_infos[i].Set_ty(idx_maps->ty[old_ty_idx]);
      if (ty_infos[i].Is_ty_no_split())
        Set_TY_no_split (ty_infos[i].Get_ty());
    }
  }
#endif

  // Process all constraint graph nodes. Remap the global st indices
  INT32 num_cg_nodes;
  SUMMARY_CONSTRAINT_GRAPH_NODE *cg_nodes = 
                    IPA_get_constraint_graph_nodes_array(hdr, num_cg_nodes);
  for (i = 0; i < num_cg_nodes; ++i) {
    ST_IDX old_st_idx = SYM_ST_IDX(cg_nodes[i].cg_st_idx());
    if (ST_IDX_level(old_st_idx) == GLOBAL_SYMTAB) {
      cg_nodes[i].cg_st_idx(idx_maps->st[old_st_idx] & 0x00000000ffffffffLL);
    }
    TY_IDX old_ty_idx = cg_nodes[i].ty_idx();
    cg_nodes[i].ty_idx(idx_maps->ty[old_ty_idx]);
  }
  INT32 num_cg_stinfos;
  SUMMARY_CONSTRAINT_GRAPH_STINFO *cg_stinfos = 
                    IPA_get_constraint_graph_stinfos_array(hdr, num_cg_stinfos);
  for (i = 0; i < num_cg_stinfos; ++i) {
    ST_IDX old_st_idx = SYM_ST_IDX(cg_stinfos[i].cg_st_idx());
    if (ST_IDX_level(old_st_idx) == GLOBAL_SYMTAB) {
      cg_stinfos[i].cg_st_idx(idx_maps->st[old_st_idx] & 0x00000000ffffffffLL);
    }
    TY_IDX old_ty_idx = cg_stinfos[i].ty_idx();
    cg_stinfos[i].ty_idx(idx_maps->ty[old_ty_idx]);
  }
#if Is_True_On
  INT32 num_cg_modranges;
  SUMMARY_CONSTRAINT_GRAPH_MODRANGE *cg_modranges = 
                IPA_get_constraint_graph_modranges_array(hdr, num_cg_modranges);
  for (i = 0; i < num_cg_modranges; ++i) {
    TY_IDX old_ty_idx = cg_modranges[i].ty_idx();
    cg_modranges[i].ty_idx(idx_maps->ty[old_ty_idx]);
  }
#endif
  INT32 num_cg_callsites;
  SUMMARY_CONSTRAINT_GRAPH_CALLSITE *cg_callsites = 
                IPA_get_constraint_graph_callsites_array(hdr, num_cg_callsites);
  for (i = 0; i < num_cg_callsites; ++i) {
    if ((cg_callsites[i].flags() & (CS_FLAGS_UNKNOWN | CS_FLAGS_INDIRECT |
                                    CS_FLAGS_INTRN)) == 0) {
      ST_IDX old_st_idx = cg_callsites[i].st_idx();
      if (ST_IDX_level(old_st_idx) == GLOBAL_SYMTAB) {
        cg_callsites[i].st_idx(idx_maps->st[old_st_idx]);
      }
    }
    if (cg_callsites[i].flags() & CS_FLAGS_VIRTUAL) {
      TY_IDX old_ty_idx = cg_callsites[i].virtualClass();
      cg_callsites[i].virtualClass(idx_maps->ty[old_ty_idx]);
    }
  }

  // process all ty_idxs found in SUMMARY_STRUCT_ACCESS, and sum them up!
  if(IPA_Enable_Reorder){
      INT32 num_tys,new_ty;
      SUMMARY_STRUCT_ACCESS* access_array = IPA_get_struct_access_file_array(hdr, num_tys);
      SUMMARY_STRUCT_ACCESS* cur_access;
      for (i = 0; i < num_tys; ++i) {
      	  cur_access=&access_array[i];
          new_ty=idx_maps->ty[make_TY_IDX (cur_access->Get_ty())];
          Merge_struct_access(cur_access,new_ty>>8);
          //TODO: put new_ty's access_info to merge_access_list
      }
  }
}
#endif // !_STANDALONE_INLINER

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
#include "inline.h"
#else // _STANDALONE_INLINER

//-------------------------------------------------------------------------
// process globals
//-------------------------------------------------------------------------
static void
IPA_process_globals (const IP_FILE_HDR& hdr)
{
    INT32 sym_size;
    SUMMARY_SYMBOL *Symbol_array = IPA_get_symbol_file_array (hdr, sym_size);
    INT32 glob_size;
    SUMMARY_GLOBAL *Globals_array = IPA_get_global_file_array(hdr, glob_size);

    if (glob_size == 0 || sym_size == 0)
	return;

    for (INT i = 0; i < glob_size; ++i) {
	SUMMARY_GLOBAL *gnode = &Globals_array[i];

	if (gnode->Get_refcount() || gnode->Get_modcount()) {
	    SUMMARY_SYMBOL *snode = &Symbol_array[gnode->Get_symbol_index()];
	    ST_IDX st_idx = snode->St_idx ();
	    Update_reference_count (&St_Table[st_idx],
				    gnode->Get_refcount (),
				    gnode->Get_modcount (),
				    snode->Is_cmod ());
	}
    }
} // IPA_process_globals

// ---------------------------------------------------------
// Mark STs of common blocks that are passed to I/O routines
// Since we don't analyze I/O items precisely, we cannot 
// propagate potential constants from thoe common blocks.
// ---------------------------------------------------------
static void
IPA_mark_commons_used_in_io (const IP_FILE_HDR& hdr)
{
  INT32 num_symbols;
  SUMMARY_SYMBOL* symbol = IPA_get_symbol_file_array(hdr, num_symbols);
  for (INT i = 0; i < num_symbols; ++i, ++symbol) {
    if (symbol->Is_common_block() && symbol->Common_read_no_cprop()) {
      Set_AUX_ST_flags (Aux_St_Table[symbol->St_idx()], COMMON_USED_IN_IO);
    }
  }
}

#ifdef KEY
void
IPA_update_ehinfo_in_pu (IPA_NODE *node)
{
	if (!(PU_src_lang (node->Get_PU()) & PU_CXX_LANG) ||
	    !PU_misc_info (node->Get_PU()))
	    return;

        int sym_size;
        SUMMARY_SYMBOL* sym_array = IPA_get_symbol_file_array(node->File_Header(), sym_size);
        FmtAssert (sym_array != NULL, ("Missing SUMMARY_SYMBOL section"));
                                                                                
        INITV_IDX tinfo = INITV_next (INITV_next (INITO_val (PU_misc_info (node->Get_PU()))));
        INITO_IDX inito = TCON_uval (INITV_tc_val (tinfo));
        if (inito)
        {
	    INITV_IDX idx = INITO_val (inito);
            do
            {
                INITV_IDX st_entry = INITV_blk (idx);
                if (INITV_kind (st_entry) == INITVKIND_ZERO)
                {
                    idx = INITV_next (idx);
                    continue;
                }
                int st_idx = TCON_uval (INITV_tc_val (st_entry));
		// bug fix for OSP_317
		// 
                if (st_idx < 0 || st_idx >= sym_size)
                {
                    idx = INITV_next (idx);
                    continue;
                }
                ST_IDX new_idx = sym_array[st_idx].St_idx();
		// This st would be used at least in the exception table, mark
		// it so that ipa does not remove it in DVE
		// TODO: Record this ref in IPL to prevent this situation.

		if (ST_IDX_level(new_idx) == GLOBAL_SYMTAB) {
      		  Set_AUX_ST_flags (Aux_St_Table[new_idx], USED_IN_OBJ);
                  Clear_ST_is_not_used (St_Table[new_idx]);
                  INITV_IDX filter = INITV_next (st_entry); // for backup
                  INITV_Set_VAL (Initv_Table[st_entry], Enter_tcon (
                       Host_To_Targ (MTYPE_U4, new_idx)), 1);
                  Set_INITV_next (st_entry, filter);
		}
                idx = INITV_next (idx);
            } while (idx);
        }
        tinfo = INITV_next (tinfo);
        inito = TCON_uval (INITV_tc_val (tinfo));
        if (inito)
        {
	    INITV_IDX idx = INITV_blk (INITO_val (inito));
	    do
	    {
	    	if (INITV_kind (idx) == INITVKIND_ZERO)
		{
		    idx = INITV_next (idx);
		    continue;
		}
		int st_idx = TCON_uval (INITV_tc_val (idx));
		FmtAssert (st_idx > 0, ("Invalid st entry in eh-spec table"));
		ST_IDX new_idx = sym_array[st_idx].St_idx();
		// TODO: Record this ref in IPL to prevent this situation.
		if (ST_IDX_level(new_idx) == GLOBAL_SYMTAB) {
		   Set_AUX_ST_flags (Aux_St_Table[new_idx], USED_IN_OBJ);
		   Clear_ST_is_not_used (St_Table[new_idx]);
		   INITV_IDX bkup = INITV_next (idx);
		   INITV_Set_VAL (Initv_Table[idx], Enter_tcon (
			   Host_To_Targ (MTYPE_U4, new_idx)), 1);
		   Set_INITV_next (idx, bkup);
		}
		idx = INITV_next (idx);
	    } while (idx);
	}
}

static inline IPA_NODE *
pu_info_to_node (PU_Info *pu)
{
    return IPA_Call_Graph->Graph()->Node_User (AUX_PU_node(Aux_Pu_Table[ST_pu(St_Table[PU_Info_proc_sym (pu)])]));
}

void
Mark_PUs_With_File_Id (PU_Info * pu, UINT id)
{
  for (; pu; pu = PU_Info_next (pu))
  {
    IPA_NODE * node = pu_info_to_node (pu);
    if (node) node->Set_File_Id (id);
    if (PU_Info_child (pu))
    	Mark_PUs_With_File_Id (PU_Info_child (pu), id);
  }
}

std::vector<char *> options;

BOOL Opt_Options_Inconsistent = FALSE;
mINT32 IPA_NODE::next_file_id = -1;

// This is a simple scheme to check if there is mismatch in options passed
// to different files doing IPA.
// TODO: Accept different options as the user requested, and remove this
// warning.
static void
IPA_Check_Optimization_Options (IP_FILE_HDR& hdr)
{
  static bool warned = false;

  if (warned) return;

  char * base_addr = (char *) 
  	WN_get_section_base (IP_FILE_HDR_input_map_addr (hdr), WT_COMP_FLAGS);

  if (base_addr == (char*) -1)
    ErrMsg (EC_IR_Scn_Read, "command line", IP_FILE_HDR_file_name (hdr));

  Elf64_Word argc = *((Elf64_Word *) base_addr);

  // Detect early if the # of options doesn't match
  // argv[0] == compiler-name (e.g. pathcc), so we start from argv[1]
  if (!options.empty() && ((argc-1) != options.size()))
  {
    warned = true;
    Opt_Options_Inconsistent = TRUE;
    ErrMsg (EC_Ipa_Options);
    return;
  }

  Elf64_Word* args = (Elf64_Word *) (base_addr + sizeof(Elf64_Word));

  if (options.empty())
  {
    options.reserve (sizeof (char *) * (argc-1));
    for (int i=1; i<argc; ++i)
	options.push_back (base_addr + args[i]);
    std::sort (options.begin(), options.end(), option_cmp());
    return;
  }

  Is_True (argc-1 == options.size(), ("IPA_Check_Optimization_Options error"));

  std::vector<char *> current_options;

  current_options.reserve (sizeof (char *) * (argc-1));
  for (int i=1; i<argc; ++i)
    current_options.push_back (base_addr + args[i]);

  std::sort (current_options.begin(), current_options.end(), option_cmp());

  for (int i=0; i<argc-1; ++i)
  {
    if (strcmp (options[i], current_options[i]))
    {
    	warned = true;
	Opt_Options_Inconsistent = TRUE;
	current_options.clear();
    	ErrMsg (EC_Ipa_Options);
	return;
    }
  }
  current_options.clear();
  return;
}
static void
IPA_update_pragma_in_pu (IPA_NODE *node)
{
        int sym_size;
        SUMMARY_SYMBOL* sym_array = IPA_get_symbol_file_array(node->File_Header(), sym_size);
        FmtAssert (sym_array != NULL, ("Missing SUMMARY_SYMBOL section"));
        WN* prags = WN_func_pragmas(node->Whirl_Tree());
        prags = WN_first(prags);
                                                                                                                                                             
        while (prags) {
          if (WN_opcode(prags) == OPC_PRAGMA &&
              WN_pragma(prags) == WN_PRAGMA_THREADPRIVATE) {
            ST_IDX new_idx = sym_array[WN_pragma_arg2(prags)].St_idx();
            WN_pragma_arg2(prags) = new_idx;
          }
          prags = WN_next(prags);
        }
        
}
#endif

//-------------------------------------------------------------------------
// for each file record the file offset for the whirl section and the 
// symtab section and update the is_written flag when needed
//-------------------------------------------------------------------------
void
IPA_Process_File (IP_FILE_HDR& hdr)
{
#ifdef KEY
  if (IPA_Check_Options)
    IPA_Check_Optimization_Options (hdr);
#endif
  IP_READ_pu_infos (hdr);

  IPA_update_summary_st_idx (hdr);

  if (IPA_Enable_AutoGnum || IPA_Enable_CGI || IPA_Enable_DVE)
    IPA_process_globals (hdr);

  if (IPA_Enable_Common_Const)
    IPA_mark_commons_used_in_io (hdr);

} // IPA_process_file


#endif // !_STANDALONE_INLINER

static void
Mark_inline_overrides(IPA_NODE* ipa_node, ST* st)
{
    UINT info = User_Specified_Name_Info(ST_name(st));

    if (Is_User_Not_Specified(info)) {
	if ((ipa_node->Summary_Proc()->Get_lang() == LANG_F77) ||
            (ipa_node->Summary_Proc()->Get_lang() == LANG_F90)) {
	    // For FORTRAN
	    // look for the string name with without an underbar in it
 	    int lastchar = strlen(ST_name(st)) - 1;
	    if (ST_name(st)[lastchar] != '_')
		return;
	    char *newname = strdup(ST_name(st));
	    newname[lastchar] = '\0';
	    info = User_Specified_Name_Info(newname);
	    free(newname);
	    if (Is_User_Not_Specified(info))
		return;
	}
	else
	    return;
    }

    // Find the the item
    if (Is_User_Must_Inline(info)) {
	ipa_node->Set_Must_Inline_Attrib();
	Total_Must_Inlined++;
	if (Trace_IPA || Trace_Perf)
	    fprintf (TFile, "%s marked \"must inlined\"\n", DEMANGLE(ST_name(st)));
    }
    else if (Is_User_No_Inline(info)) {
	ipa_node->Set_Noinline_Attrib();
	Total_Must_Not_Inlined++;
 	if (Trace_IPA || Trace_Perf)
           fprintf (TFile, "%s marked \"no inlined\"\n", DEMANGLE (ST_name(st)));
    }
}


static void
Mark_inline_edge_overrides(IPA_EDGE* ipa_edge)
{
    IPA_NODE* caller = IPA_Call_Graph->Caller(ipa_edge->Edge_Index());
    IPA_NODE* callee = IPA_Call_Graph->Callee(ipa_edge->Edge_Index());

    if ((ipa_edge->Edge_Index() > INLINE_Skip_After) ||
		(ipa_edge->Edge_Index() < INLINE_Skip_Before)) {
	ipa_edge->Set_Noinline_Attrib();
	Total_Must_Not_Inlined++;
 	if (Trace_IPA || Trace_Perf)
	    fprintf (TFile, "%s marked \"no inlined\" into %s\n", DEMANGLE(callee->Name()), DEMANGLE(caller->Name()));
    }

    UINT info = User_Specified_Edge_Info(ipa_edge->Edge_Index());

    if (Is_User_Not_Specified(info)) {
	return;
    }

    // Find the the item
    if (Is_User_Must_Inline(info)) {
	ipa_edge->Set_Must_Inline_Attrib();
	Total_Must_Inlined++;
	if (Trace_IPA || Trace_Perf)
	    fprintf (TFile, "%s marked \"must inlined\" into %s\n", DEMANGLE(callee->Name()), DEMANGLE(caller->Name()));
    }
    else if (Is_User_No_Inline(info)) {
	ipa_edge->Set_Noinline_Attrib();
	Total_Must_Not_Inlined++;
 	if (Trace_IPA || Trace_Perf)
	    fprintf (TFile, "%s marked \"no inlined\" into %s\n", DEMANGLE(callee->Name()), DEMANGLE(caller->Name()));
    }
}

#include <map>

typedef std::map<UINT64, IPA_NODE*> ADDR_NODE_MAP;
static ADDR_NODE_MAP addr_node_map;

IPA_NODE* 
Add_One_Node (IP_FILE_HDR& s, INT32 file_idx, INT i, NODE_INDEX& orig_entry_index)
{

    IPA_NODE *ipa_node = NULL;
    INT32 size;
    SUMMARY_PROCEDURE *Proc_array = IPA_get_procedure_file_array (s, size);
    UINT32 sindex = Proc_array[i].Get_symbol_index();

    INT32 sym_size;
    SUMMARY_SYMBOL* sym_array = IPA_get_symbol_file_array(s, sym_size);
    Is_True (sym_array != NULL, ("Missing SUMMARY_SYMBOL section"));
	
    SUMMARY_SYMBOL& sum_symbol = sym_array[sindex];

    ST_IDX st_idx = sum_symbol.St_idx ();

    FmtAssert (ST_IDX_level (st_idx) == GLOBAL_SYMTAB,
		("Invalid ST_IDX for procedure"));
	
    ST* st = &St_Table[st_idx];

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))

    NODE_INDEX node_idx = AUX_PU_node(Aux_Pu_Table[ST_pu(st)]);
    if (node_idx != INVALID_NODE_INDEX) // Already has this node, 
					// node is multiply defined
	return ipa_node;

#endif // _STANDALONE_INLINER

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

    // symbol resolution should have set the NOT_USED bit of duplicated
    // PUs

#ifdef KEY
    if (IPA_Call_Graph_Tmp == NULL /*Do it only the first pass */ && 
        AUX_PU_file_hdr (Aux_Pu_Table[ST_pu (st)]) != &s)
#else
    if (AUX_PU_file_hdr (Aux_Pu_Table[ST_pu (st)]) != &s)
#endif
    {
	Is_True (ST_export (st) != EXPORT_LOCAL &&
		     ST_export (st) != EXPORT_LOCAL_INTERNAL,
		     ("Multiply defined symbols should not be EXPORT_LOCAL"));
	    
	/* handle the case where there is multiply defined symbols and
	 * this is the definition that the linker has chosen to ignored
	 */

	if (Trace_IPA || Trace_Perf)
	    fprintf (TFile, "%s from %s deleted (multiply defined"
			 " procedure)\n",
			 DEMANGLE (ST_name (st)), IP_FILE_HDR_file_name(s));

	Delete_Function_In_File (s, i);

	if (Proc_array[i].Has_alt_entry()) {
	    while (i + 1 < size) {
		if (Proc_array[i+1].Is_alt_entry()) {
		    Delete_Function_In_File (s, i+1);
		    i++;
		} else
		    break;
	    }	
	}
	return ipa_node;
    }

#endif // _STANDALONE_INLINER 
		
    ipa_node = 
          IPA_Call_Graph->Add_New_Node (st, file_idx, i, i);

#ifdef KEY
    if (IPA_Enable_PU_Reorder == REORDER_BY_EDGE_FREQ && IPA_Call_Graph_Tmp)
    {
	// Get the original node from THE call graph
    	IPA_NODE * node_dup = IPA_Call_Graph_Tmp->Graph()->Node_User (AUX_PU_node(Aux_Pu_Table[ST_pu(st)]));
    	node_map [ipa_node] = node_dup;
    	node_map [node_dup] = ipa_node;
    }
    else
    {
#endif
//;;printf( "PU   %-50s (freq = %.1f) \n", IPA_Node_Name(ipa_node), (ipa_node->Get_frequency())._value);//pengzhao
    NODE_INDEX cg_node = ipa_node->Node_Index ();
	
    Set_AUX_PU_node (Aux_Pu_Table[ST_pu (st)], cg_node);

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
    ipa_node->Set_Scope(Inliner_Aux_Pu_Table[ST_pu (st)]);
#endif // _STANDALONE_INLINER

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    if (IPA_Enable_DFE || IPA_Enable_Picopt || IPA_Enable_Array_Sections
	    || IPA_Enable_Relocatable_Opt) {
        if (Proc_array[i].Has_alt_entry()) {
	    orig_entry_index = cg_node;
	} else if (Proc_array[i].Is_alt_entry()) {
	    if (alt_entry_map == NULL)
		alt_entry_map = CXX_NEW (ALT_ENTRY_MAP(), Malloc_Mem_Pool);
	    (*alt_entry_map)[cg_node] = orig_entry_index;
                // Set the EXPORT Class of this alternate entry point to
                // be the same as that of its corresponding main entry point
                ST* orig_st = IPA_Call_Graph->Graph()->Node_User(orig_entry_index)->Func_ST();
                if (ST_export(orig_st) > ST_export(st))
                    Set_ST_export(st, ST_export(orig_st));
	} else {
	    orig_entry_index = INVALID_NODE_INDEX;
	}
    }

    static BOOL reported = FALSE;
    if (!reported) {
	DevWarn ("TODO: support GP and call graph partitioning");
	reported = TRUE;
    }

#ifdef KEY
    // Has static variables
    if (IPA_Enable_Pure_Call_Opt &&
        ipa_node->Summary_Proc()->Has_pstatic())
      ipa_node->Summary_Proc()->Set_has_side_effect();
#endif // KEY
#endif // _STANDALONE_INLINER

    Orig_Prog_Weight += ipa_node->Weight ();
	Orig_Prog_WN_Count += (UINT32)(ipa_node->Get_wn_count());

    if ((ipa_node->Summary_Proc()->Get_lang() == LANG_F77) ||
	    (ipa_node->Summary_Proc()->Get_lang() == LANG_F90))
     	IPA_Has_Fortran = TRUE;


#ifdef TODO

#ifndef _STANDALONE_INLINER
	// for partitioning, setting the user-specified partitions
    if (IPA_Enable_GP_Partition || IPA_Enable_SP_Partition) {
	void *pext = linker->IP_get_mext(nme);
	if (pext) {
            ipa_node->Set_partition_group(linker->IP_get_mext_partition_grp(pext));
            if (linker->IP_is_mext_internal_to_partition(pext))
                ipa_node->Set_partition_internal();
	}
    }
#endif
#endif // TODO

    if (ipa_node->Has_frequency ()) {
	Total_cycle_count += ipa_node->Get_cycle_count ();
	Total_cycle_count_2 += ipa_node->Get_cycle_count_2 ();
    }

    // Mark overrides for externally visible routines

    Mark_inline_overrides(ipa_node, st);

#if !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)

    IPA_NystromAliasAnalyzer *ipa_naa = 
                              IPA_NystromAliasAnalyzer::aliasAnalyzer();
    if (ipa_naa) {
        ipa_naa->buildIPAConstraintGraph(ipa_node);
    }

#endif
#ifdef KEY
    } // else of '(REORDER_BY_EDGE_FREQ && IPA_Call_Graph_Tmp)'
#endif

#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    // bug 4880
    // If lang of main pu is C++, -IPA:pu_reorder defaults to 1 w/ feedback
    if (!IPA_Enable_PU_Reorder_Set && Annotation_Filename &&
        ipa_node && !strcmp (ipa_node->Name(), "main") &&
	(PU_src_lang (ipa_node->Get_PU()) & PU_CXX_LANG))
    {
      // Remind us to fix this place if default changes
      Is_True (IPA_Enable_PU_Reorder == REORDER_DISABLE,
               ("Attempt to change default of -IPA:pu_reorder"));
      IPA_Enable_PU_Reorder = REORDER_BY_NODE_FREQ;
    }

    const UINT64 runtime_addr = ipa_node->Get_func_runtime_addr ();
    if (runtime_addr) addr_node_map[runtime_addr] = ipa_node;
#endif // KEY && !_STANDALONE_INLINER && !_LIGHTWEIGHT_INLINER



    return ipa_node;
	
}


//---------------------------------------------------------------------
// add all the nodes of a file to the call graph
//---------------------------------------------------------------------
static void
Add_nodes (IP_FILE_HDR& s, INT32 file_idx)
{

    INT32 size;
    SUMMARY_PROCEDURE *Proc_array = IPA_get_procedure_file_array (s, size);
    // index of last node with alt. entry
    NODE_INDEX index = INVALID_NODE_INDEX;

    if (size == 0)
	return;

    for (INT i = 0; i < size; ++i) {
 
	(void)Add_One_Node(s, file_idx, i, index );

    }

} // Add_nodes


static inline void
append_icall_list (IPA_ICALL_LIST& ilist, SUMMARY_CALLSITE *c)
{
    IPA_ICALL_NODE *cnode = CXX_NEW (IPA_ICALL_NODE (c), Malloc_Mem_Pool);
    ilist.push_back (cnode);
}

#ifdef KEY
// Check if we already have an edge between the 2 nodes (in ne).
static vector<Nodes_To_Edge *>::iterator 
find_if_equal (vector<Nodes_To_Edge*>::iterator b, vector<Nodes_To_Edge*>::iterator e, Nodes_To_Edge * ne)
{
    vector<Nodes_To_Edge *>::iterator it = b;
    for (; it!=e; it++)
    	if (((*it)->Caller() == ne->Caller() && 
	     (*it)->Callee() == ne->Callee()) ||
	     // Edges are considered undirected
	     ((*it)->Caller() == ne->Callee() &&
	     (*it)->Callee() == ne->Caller()))
	    return b;
    return e;
}

// Update frequency of the edge with the frequency in the callsite
static void Update_freq (SUMMARY_CALLSITE *, IPA_EDGE *);
static bool Check_Heuristic(IPA_NODE *, IPA_NODE *, INT64, IPA_CALL_GRAPH *);
#endif

void
Add_Edges_For_Node (IP_FILE_HDR& s, INT i, SUMMARY_PROCEDURE* proc_array, SUMMARY_SYMBOL* symbol_array)
{
#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    BOOL has_icalls = FALSE;
#endif

    if (proc_array == NULL) {
        INT32 size;
        proc_array = IPA_get_procedure_file_array(s, size);
    }

    if (symbol_array == NULL) {
	INT32 symbol_size;
        symbol_array = IPA_get_symbol_file_array (s, symbol_size);
    }

    UINT32 sindex = proc_array[i].Get_symbol_index();
    ST_IDX temp_st_idx = symbol_array[sindex].St_idx ();
    const ST* caller_st = &St_Table[temp_st_idx];

    NODE_INDEX caller_idx = AUX_PU_node (Aux_Pu_Table[ST_pu(caller_st)]);
    IPA_NODE* caller;
#ifdef KEY
    if (IPA_Call_Graph_Tmp) // Actual call graph
      caller = IPA_Call_Graph_Tmp->Graph()->Node_User (caller_idx);
    else
#endif
      caller = IPA_Call_Graph->Graph()->Node_User (caller_idx);
    SUMMARY_CALLSITE *callsite_array = IPA_get_callsite_array (caller);
    
    INT callsite_count = proc_array[i].Get_callsite_count();
    INT callsite_index = proc_array[i].Get_callsite_index();
	
    if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
      fprintf(TFile, "\n[Add_Edges_For_Node %s] callsite_count=%d, callsite_index=%d\n",
                     ST_name(caller_st), callsite_count, callsite_index);
    }
    for (INT j = 0; j < callsite_count; ++j, ++callsite_index) {
#ifdef KEY
      if (IPA_Enable_Pure_Call_Opt &&
	  (callsite_array[callsite_index].Is_func_ptr() ||
	   callsite_array[callsite_index].Is_intrinsic()))
	caller->Summary_Proc()->Set_has_side_effect ();
#endif	       

      if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
        fprintf(TFile, "\t [idx %d] cid=%d state=%#x %s ", callsite_index, 
                       callsite_array[callsite_index].Get_callsite_id(),
                       callsite_array[callsite_index].Get_state(),
                       callsite_array[callsite_index].Is_func_ptr() ? "FPTR" : 
                        callsite_array[callsite_index].Is_icall_target() ? "ICALL" : 
                         callsite_array[callsite_index].Is_virtual_function_target() ? "VCALL" : "");
      }
      // for indirect call sites
      if ( callsite_array[callsite_index].Is_func_ptr() ) {
      	if (!IPA_Call_Graph_Tmp) { // KEY
#ifdef _LIGHTWEIGHT_INLINER
          if (!INLINE_Inlined_Pu_Call_Graph)
#endif // _LIGHTWEIGHT_INLINER
              append_icall_list (caller->Icall_List(), 
                           &callsite_array[callsite_index]);
	} // KEY
      } 
#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
      else if (callsite_array[callsite_index].Is_icall_target()) {

        has_icalls = TRUE;
        if (!IPA_Enable_Icall_Opt)
          continue; // don't do anything with this callsite

        sindex = callsite_array[callsite_index].Get_symbol_index();
        temp_st_idx = symbol_array[sindex].St_idx ();
        ST* callee_st = &St_Table[temp_st_idx];

        Is_True ((!strcmp (ST_name (callee_st), "__dummy_icall_target") ||
                 !strcmp (ST_name (callee_st), "__dummy_virtual_function_target")),
             ("Process_procedure: Expected ICALL target function as callee"));

        mUINT64 target_addr = callsite_array[callsite_index].Get_targ_runtime_addr();
        IPA_NODE * callee = addr_node_map [target_addr];

        if (!callee) continue;

        if (IPA_Consult_Inliner_For_Icall_Opt)
        {
          mUINT64 callee_counter = (mUINT64)
                callsite_array[callsite_index].Get_frequency_count().Value();

          if (!Check_Heuristic (caller,
                                callee,
                                callee_counter,
                                IPA_Call_Graph))
            continue;
        }
        IPA_EDGE* ipa_edge = 
            IPA_Call_Graph->Add_New_Edge (&callsite_array[callsite_index],
                                          caller_idx, 
                                          callee->Node_Index());
        if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
          fprintf(TFile, " => %s", IPA_Node_Name(callee));
        }
        if (ipa_edge->Has_frequency ())
            Total_call_freq += ipa_edge->Get_frequency ();
        Mark_inline_edge_overrides(ipa_edge);

        caller->Set_Pending_Icalls();
        // No longer an icall target, treat like a normal call.
        callsite_array[callsite_index].Reset_icall_target();
      }
#endif // KEY && !_STANDALONE_INLINER && !_LIGHTWEIGHT_INLINER
      // for direct calls
      else if (!callsite_array[callsite_index].Is_intrinsic()) {

        sindex = callsite_array[callsite_index].Get_symbol_index();
        temp_st_idx = symbol_array[sindex].St_idx ();
        ST* callee_st = &St_Table[temp_st_idx];

        if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
          fprintf(TFile, "callee \"%s\" ", ST_name(callee_st));
        }
#ifdef TODO
        // if static function, then force same partition
        if (Symbol_array[sindex].Is_local()) {
          // local functions should have the same partition number
          // as its caller when IPA_Enable_SP_Partition is on
          if (IPA_Enable_SP_Partition)
            callee->Set_partition_group(caller->Get_partition_group());
        }
#endif
		
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
        // check to see if it is a weak symbol 
        // can't do this for inliner because the weak may be preempted
        while (ST_is_weak_symbol (callee_st) &&
               ST_st_idx (callee_st) != ST_strong_idx (*callee_st))
          /* find the corresponding strong */
          callee_st = ST_strong (callee_st);
#endif // _STANDALONE_INLINER

        Clear_ST_is_not_used (callee_st);

        NODE_INDEX callee_idx = AUX_PU_node (Aux_Pu_Table[ST_pu (callee_st)]);

#ifdef KEY
        if (callee_idx == INVALID_NODE_INDEX &&
            ST_export (callee_st) == EXPORT_LOCAL) {
          // Bugs 3224, 7842
          // The callee is marked static, so the definition must be in this
          // file (or executable for IPA). This can happen for non-ANSI C
          // when a static function decl is in function scope.
          // NOTE: The fix-up is done only in the IPA call-graph, the
          // callsite ST and the callee ST would still remain different.
          // For IPA, we must also fix-up the func name at the callsite.

          IPA_CALL_GRAPH * cg =
             (IPA_Enable_PU_Reorder == REORDER_BY_EDGE_FREQ &&
              IPA_Call_Graph_Tmp) ? IPA_Call_Graph_Tmp : IPA_Call_Graph;

          for (INT count=0; count<Aux_Pu_Table.Size(); count++) {

            NODE_INDEX node_idx = AUX_PU_node (Aux_Pu_Table[count]); 

            if (node_idx != INVALID_NODE_INDEX) {
              IPA_NODE * node = cg->Graph()->Node_User (node_idx);
              // There must be a single match, verify it in debug mode
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
              if (!strcmp (node->Name(), ST_name (callee_st)))
#else // ipa
              const char * cur_node_name = node->Name();
              // We are looking for:
              // cur_node_name == orig_fn_name<..EXT>
              // callee_st == orig_fn_name
              const int callsite_name_len = strlen (ST_name (callee_st));
              if (strlen (cur_node_name) > callsite_name_len + 2 &&
                  !strncmp (cur_node_name, ST_name (callee_st),
                            callsite_name_len) &&
                  cur_node_name[callsite_name_len] == '.' &&
                  cur_node_name[callsite_name_len+1] == '.' &&
                  // make sure they are from the same file
                  caller->File_Index() == node->File_Index())
#endif
              {
                Is_True (callee_idx == INVALID_NODE_INDEX,
                         ("Duplicate static fn defn ?"));
                callee_idx = node_idx;
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
                Is_True (ST_export (node->Func_ST()) == EXPORT_LOCAL,
                         ("Unexpected export scope for func %s", node->Name()));
#else
                Is_True (ST_export (node->Func_ST()) == EXPORT_INTERNAL,
                         ("Unexpected export scope for func %s", node->Name()));
                Set_ST_name_idx (callee_st, ST_name_idx (node->Func_ST()));
#endif
#ifndef Is_True_On
                break;
#endif // !Is_True_On
              }
            }
          }
        }
#endif
        // If the callee is not in a WHIRL IR file, its index will 
        // be invalid. In that case we do not add the edge, but we
        // add the callsite to a special list of opaque calls.
#ifdef KEY
	if (callee_idx != INVALID_NODE_INDEX && 
	    IPA_Enable_PU_Reorder == REORDER_BY_EDGE_FREQ && 
	    IPA_Call_Graph_Tmp) {
	  IPA_NODE * callee = IPA_Call_Graph_Tmp->Graph()->Node_User (callee_idx);
	  IPA_NODE * caller_u = node_map [ caller ];
	  IPA_NODE * callee_u = node_map [ callee ];
	  NODE_INDEX caller_idx_u = caller_u->Node_Index();
	  NODE_INDEX callee_idx_u = callee_u->Node_Index();
	  Nodes_To_Edge * ne = new Nodes_To_Edge (caller_idx_u, callee_idx_u);
	  vector<Nodes_To_Edge *>::iterator it;
	  if ((it = find_if_equal (q_order.begin(), q_order.end(), ne)) == q_order.end())
	  { // No edge between these 2 nodes
	    IPA_EDGE * edge_u = 
	        IPA_Call_Graph->Add_New_Edge 
				    (&callsite_array[callsite_index],
                                     caller_idx_u, callee_idx_u);
            if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
              fprintf(TFile, " => %s", IPA_Node_Name(callee));
            }
	    Nodes_To_Edge * o = new Nodes_To_Edge (caller_idx_u,
	      					callee_idx_u, edge_u);
	    q_order.push_back (o);
	  }
	  else
	  { // Already have edge between the 2 nodes.
	    SUMMARY_CALLSITE * c = &callsite_array[callsite_index];
	    Update_freq (c, (*it)->Edge());
	  }
	  delete ne;
	  continue;
	}
#endif
        if (callee_idx != INVALID_NODE_INDEX) {
          IPA_EDGE* ipa_edge = 
            IPA_Call_Graph->Add_New_Edge (&callsite_array[callsite_index],
                                          caller_idx, 
                                          callee_idx);
          IPA_NODE * callee = IPA_Call_Graph->Graph()->Node_User (callee_idx);
          if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
            fprintf(TFile, " => %s", IPA_Node_Name(callee));
          }
          if (ipa_edge->Has_frequency ()) {
            Total_call_freq += ipa_edge->Get_frequency ();
          }
	  Mark_inline_edge_overrides(ipa_edge);
        }
        else {
#ifdef _LIGHTWEIGHT_INLINER
            if (INLINE_Inlined_Pu_Call_Graph)
	        continue;
	    else 
#endif // _LIGHTWEIGHT_INLINER
                append_icall_list (caller->Ocall_List(), 
                             &callsite_array[callsite_index]);
#ifdef KEY
		// If we have no WHIRL, we assume any C++ PU can throw
		if (IPA_Enable_EH_Region_Removal &&
		    (PU_src_lang (Pu_Table [ST_pu (caller->Func_ST())]) & 
		     PU_CXX_LANG))
		    caller->Set_PU_Can_Throw ();

		// If we have no WHIRL, assume it may have side-effect
		if (IPA_Enable_Pure_Call_Opt)
		    caller->Summary_Proc()->Set_has_side_effect ();
#endif
        }
      }
      if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
        fprintf(TFile, "\n");
      }
    }

#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    if (has_icalls) {
      // Need to fix up the callsite ids.
      // If icall opt is disabled, we need to fix up callsite id of all
      // callsites. If enabled, we still need to loop through the callsites
      // to remove any callsites not being transformed.
      //
      INT cs_index = proc_array[i].Get_callsite_index();
      INT count = 0;
      for (INT j = 0; j < callsite_count; ++j, ++cs_index) {
        if (!callsite_array[cs_index].Is_icall_target()) {
           if (callsite_array[cs_index].Get_callsite_id() != count) {
              if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
                fprintf(TFile, "\ncallsite_array[%d] callsite_id %d is reset to %d!!!\n",
                    cs_index, callsite_array[cs_index].Get_callsite_id(), count);
              }
           }
           callsite_array[cs_index].Set_callsite_id (count++);
        }
      }
    }
#endif

    return;
}

//---------------------------------------------------------------------
// add all the edges to the graph
//---------------------------------------------------------------------
static void
Add_edges (IP_FILE_HDR& s)
{
  INT32 size;
  SUMMARY_PROCEDURE *proc_array = IPA_get_procedure_file_array(s, size);
  if (size == 0)
    return;
    
  INT32 symbol_size;
  SUMMARY_SYMBOL* symbol_array = IPA_get_symbol_file_array (s, symbol_size);

  for (INT i = 0; i < size; ++i) {
	
    if (IP_PROC_INFO_state (IP_FILE_HDR_proc_info(s)[i]) == IPA_DELETED)
      continue;

    (void)Add_Edges_For_Node(s, i, proc_array, symbol_array);

  }

} // Add_edges


static UINT32
Mark_reachable (NODE_INDEX root, mBOOL* visited)
{
  UINT32 count = 0;

  if (!visited[root]) {
    visited[root] = TRUE;
    ++count;
  }

  NODE_ITER viter(IPA_Call_Graph->Graph(), root);

  for (NODE_INDEX vi = viter.First_Succ(); vi != -1; vi = viter.Next_Succ()) {
    if (!visited[vi])
      count += Mark_reachable(vi, visited);
  }

  return count;
} // Mark_reachable


static inline UINT32 
connect_to_root (NODE_INDEX node, mBOOL* visited)
{
  EDGE_INDEX cg_edge = 
    IPA_Call_Graph->Graph()->Add_Edge (IPA_Call_Graph->Root(), node, NULL);
  return Mark_reachable (node, visited);
}

//------------------------------------------------------------------------
// connect the graph                                                      
// I'm doing this since with DSO's, even if one of the connected          
// components is not part of main, it is possible that it will be invoked 
//------------------------------------------------------------------------
static void
Connect_call_graph()
{
    UINT num_nodes = GRAPH_vcnt (IPA_Call_Graph->Graph());
    mBOOL *visited = (mBOOL *) alloca ((GRAPH_vmax(IPA_Call_Graph->Graph())+1) * sizeof(mBOOL)); // Adding one to prepare for the dummy ROOT
    BZERO (visited, (GRAPH_vmax(IPA_Call_Graph->Graph())+1) * sizeof(mBOOL));

    UINT32 visited_count = 0;

    if (IPA_Call_Graph->Root() == INVALID_NODE_INDEX) {
	// create a dummy root node
        NODE_INDEX root = IPA_Call_Graph->Graph()->Add_Node (NULL);
	++num_nodes;
	visited[root] = TRUE;
	visited_count = 1;
	IPA_Call_Graph->Set_Root (root);
    } else
        visited_count += Mark_reachable (IPA_Call_Graph->Root(), visited);

    
    // 2 walks through the graph
    // first connect all nodes that have no predecessors
    // mark all the reachable nodes
    // next walk the graph looking for unreachable nodes, connect them
    // and mark all the reachable nodes
    NODE_INDEX v;
    for (v = 0; v < GRAPH_vmax(IPA_Call_Graph->Graph()); ++v) {
	if (NODE_fcnt(&GRAPH_v_i(IPA_Call_Graph->Graph(), v)) != -1 &&
	    IPA_Call_Graph->Graph()->Num_Preds (v) == 0 &&
	    v != IPA_Call_Graph->Root())
	    visited_count += connect_to_root (v, visited);
    }
	     
    if (visited_count == num_nodes)	// no more disconnected components
	return;

    // handle the case where there is a strongly connected subcomponent
    // that is not reachable from the rest of the graph (e.g. a self
    // recursive node that is never called.

    for (v = 0;
	 visited_count != num_nodes && v < GRAPH_vmax(IPA_Call_Graph->Graph());
	 ++v) {
	if (NODE_fcnt(&GRAPH_v_i(IPA_Call_Graph->Graph(), v)) != -1 && !visited[v]) {
	    visited_count += connect_to_root (v, visited);
	}
    }
} // Connect_call_graph


//----------------------------------------------------------------------
// Build the call graph
//----------------------------------------------------------------------
struct add_nodes
{
    void operator() (UINT32 idx, IP_FILE_HDR* hdr) const {
	Add_nodes (*hdr, idx);
    }
}; // add_nodes

struct add_edges
{
    void operator() (UINT32, IP_FILE_HDR* hdr) const {
	Add_edges (*hdr);
    }
}; // add_edges

#ifdef KEY
// Used by sort.
struct order : public binary_function<IPA_EDGE *, IPA_EDGE *, bool>
{
  bool operator() (IPA_EDGE * e1, IPA_EDGE * e2)
  {
    // Don't use operator< since it exposes a g++ bug.
    // Don't use operator<= since it fails the requirements of 'sort'
    return (e1->Has_frequency() || e2->Has_frequency()) && 
    	   e1->Get_frequency()._value < e2->Get_frequency()._value;
  }
};

static void
Update_freq (SUMMARY_CALLSITE *c, IPA_EDGE *e)
{
  FB_FREQ sum = 0.0;
  if (c && c->Has_callsite_freq ())
    sum = c->Get_frequency_count ();
  if (e->Has_frequency())
  {
    sum += e->Get_frequency ();
    e->Set_frequency (sum);
  }
}

// List of edges in the changing undirected graph.
static vector<IPA_EDGE *> edges;
// Assumption: Called with the undirected graph for pu reordering
// Merges the 2 nodes into 'caller_idx'.
void
IPA_CALL_GRAPH::Merge_Nodes (NODE_INDEX caller_idx, NODE_INDEX callee_idx)
{
  EDGE_INDEX nf;
  EDGE_INDEX f = NODE_from (&(_graph->v[callee_idx]));
  // Update the 'from' edges from callee to start from caller.
  while (f != INVALID_EDGE_INDEX)
  {
    nf = EDGE_nfrom (&(_graph->e[f]));
    IPA_EDGE * from = _graph->Edge_User (f);

    if (!from)
    {
    	f = nf;
	continue;
    }
    // Create an edge between caller_idx and to_idx, so that we can later delete
    // 'from'.
    NODE_INDEX to_idx = Callee (from)->Node_Index();

    Nodes_To_Edge * ne = new Nodes_To_Edge (caller_idx, to_idx);
    vector<Nodes_To_Edge *>::iterator it;

    if ((it = find_if_equal (q_order.begin(), q_order.end(), ne)) == q_order.end())
    {
	IPA_EDGE* ipa_edge = 
            Add_New_Edge (from->Summary_Callsite(), caller_idx, to_idx);
	edges.push_back (ipa_edge);
    	// No need to push this edge into q_order, since we don't expect
	// this edge to appear again in the undirected call graph.
    }
    else
    {
	Update_freq (from->Summary_Callsite(), (*it)->Edge());
    }
    delete ne;
    f = nf;
  }

  // Update the 'to' edges to callee to end at caller.
  EDGE_INDEX nt, t = NODE_to (&(_graph->v[callee_idx]));
  while (t != INVALID_EDGE_INDEX)
  {
    nt = EDGE_nto (&(_graph->e[t]));
    IPA_EDGE * to = _graph->Edge_User (t);

    if (!to)
    {
    	t = nt;
	continue;
    }
    
    NODE_INDEX from_idx = Caller (to)->Node_Index();
    if (from_idx == caller_idx)
    {
	t = nt;
    	continue;
    }

    Nodes_To_Edge * ne = new Nodes_To_Edge (from_idx, caller_idx);
    vector<Nodes_To_Edge *>::iterator it;
    if ((it = find_if_equal (q_order.begin(), q_order.end(), ne)) == q_order.end())
    {
    	IPA_EDGE* ipa_edge =
	    Add_New_Edge (to->Summary_Callsite(), from_idx, caller_idx);
	edges.push_back (ipa_edge);
    }
    else
    {
    	Update_freq (to->Summary_Callsite(), (*it)->Edge());
    }
    delete ne;
    t = nt;
  }
  _graph->Node_User (caller_idx)->Set_Merged ();
}

NODE_INDEX node_g;
struct matching : public unary_function<IPA_EDGE *, bool>
{ // Must assign to node_g before using this predicate
  bool operator() (IPA_EDGE * e)
  {
    return IPA_Call_Graph->Caller(e)->Node_Index() == node_g ||
    	   IPA_Call_Graph->Callee(e)->Node_Index() == node_g;
  }
};

// Store our order in which the nodes should be emitted, for later use in
// optimize phase.
vector<IPA_NODE *> emit_order;
// Determine_Affinity is currently not called.
#ifdef TODO_KEY
static void
Determine_Affinity (vector<IPA_NODE *> v)
{
  FB_FREQ f[] = {0.0, 0.0, 0.0, 0.0};

    {
    	Nodes_To_Edge * ne = new Nodes_To_Edge (v[0]->Node_Index(), v[2]->Node_Index());
	vector<Nodes_To_Edge *>::iterator it;
	if ((it = find_if_equal (q_order.begin(), q_order.end(), ne)) != q_order.end())
	    f[0] = (*it)->Edge()->Get_frequency();
    	delete ne;
    }
    if (v.size() == 4)
    {
    	Nodes_To_Edge * ne = new Nodes_To_Edge (v[0]->Node_Index(), v[3]->Node_Index());
	vector<Nodes_To_Edge *>::iterator it;
	if ((it = find_if_equal (q_order.begin(), q_order.end(), ne)) != q_order.end())
	    f[1] = (*it)->Edge()->Get_frequency();
    	delete ne;
    }
    {
    	Nodes_To_Edge * ne = new Nodes_To_Edge (v[1]->Node_Index(), v[2]->Node_Index());
	vector<Nodes_To_Edge *>::iterator it;
	if ((it = find_if_equal (q_order.begin(), q_order.end(), ne)) != q_order.end())
	    f[2] = (*it)->Edge()->Get_frequency();
    	delete ne;
    }
    if (v.size() == 4)
    {
    	Nodes_To_Edge * ne = new Nodes_To_Edge (v[1]->Node_Index(), v[3]->Node_Index());
	vector<Nodes_To_Edge *>::iterator it;
	if ((it = find_if_equal (q_order.begin(), q_order.end(), ne)) != q_order.end())
	    f[3] = (*it)->Edge()->Get_frequency();
    	delete ne;
    }

    bool swapped = false;
    if (f[0]+f[1] > f[2]+f[3])
    { // swap v[0] and v[1]
    	IPA_NODE * tmp = v[0];
	v[0] = v[1];
	v[1] = tmp;
	swapped = true;
    }
    if (v.size() == 4)
    {
      FB_FREQ f0, f1;
      if (swapped)
      {
    	f0 = f[0];
	f1 = f[1];
      }
      else
      {
    	f0 = f[2];
	f1 = f[3];
      }
      if (f0 < f1)
      { // swap v[2] and v[3]
    	IPA_NODE * tmp = v[2];
	v[2] = v[3];
	v[3] = tmp;
      }
    }
    for (vector<IPA_NODE *>::iterator it=v.begin(); it!=v.end(); ++it)
    	emit_order.push_back ((*it));
}
#endif // TODO_KEY

// Top level function for doing REORDER_BY_EDGE_FREQ
static void
Determine_Emit_Order (IPA_CALL_GRAPH * cg)
{
  IPA_NODE_ITER walk (cg, PREORDER);

  for (walk.First (); !walk.Is_Empty (); walk.Next ())
  {
    IPA_NODE * node = walk.Current ();
    if (!node) continue;
    IPA_SUCC_ITER succ_iter (cg, node);
    for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next())
    {
	IPA_EDGE * edge = succ_iter.Current_Edge ();
	if (edge)
    	  edges.push_back (edge);
    }
  }
  sort (edges.begin(), edges.end(), order());

#ifdef TODO_KEY
  vector<IPA_NODE *> last;
#endif
  while (!edges.empty())
  {
    IPA_EDGE * e = edges.back ();
    IPA_NODE * caller = IPA_Call_Graph->Caller (e);
    IPA_NODE * callee = IPA_Call_Graph->Callee (e);

    NODE_INDEX caller_idx = caller->Node_Index();
    NODE_INDEX callee_idx = callee->Node_Index();

#ifdef TODO_KEY
    if (last.empty())
    {
    	if (!caller->Is_Merged())
	  last.push_back (node_map[caller]);
	if (!callee->Is_Merged())
	  last.push_back (node_map[callee]);
    }
    else
    {
    	if (!caller->Is_Merged())
	  last.push_back (node_map[caller]);
	if (!callee->Is_Merged())
	  last.push_back (node_map[callee]);
	if (last.size() <= 2)
	{
	  emit_order.push_back (last[0]);
	  if (last.size() == 2) emit_order.push_back (last[1]);
	}
	else Determine_Affinity (last);
	last.clear();
    }
#else
    
    if (!caller->Is_Merged())
    	emit_order.push_back (node_map[caller]);
    if (!callee->Is_Merged())
    	emit_order.push_back (node_map[callee]);
#endif

    // Merge the callee information into the caller, then delete the callee.
    IPA_Call_Graph->Merge_Nodes (caller_idx, callee_idx);
    node_g = callee_idx;
    // node_g is used by remove_if
    vector<IPA_EDGE *>::iterator new_end = remove_if (edges.begin(), edges.end(), matching());
    edges.erase (new_end, edges.end());

    IPA_Call_Graph->Graph()->Delete_Node (callee_idx);
    
    sort (edges.begin(), edges.end(), order());
  }
}
#endif

void
Build_Call_Graph ()
{
  IPA_Call_Graph = CXX_NEW(IPA_CALL_GRAPH(Malloc_Mem_Pool), Malloc_Mem_Pool);
#ifdef KEY
  if (IPA_Enable_PU_Reorder == REORDER_BY_EDGE_FREQ)
      IPA_Graph_Undirected = CXX_NEW(IPA_CALL_GRAPH(Malloc_Mem_Pool), Malloc_Mem_Pool);
  for (int iter=0; iter<2; ++iter)
  {
#endif

    For_all_entries (IP_File_header, add_nodes ());

    For_all_entries (IP_File_header, add_edges ());

    Connect_call_graph();
#ifdef KEY
    if (IPA_Enable_PU_Reorder != REORDER_BY_EDGE_FREQ)
    	break;
    else if (!iter)
    { // Prepare to build the "undirected" graph
    	IPA_Call_Graph_Tmp = IPA_Call_Graph;
	IPA_Call_Graph = IPA_Graph_Undirected;
    }
    else
    { // Restore pointers
	Determine_Emit_Order (IPA_Call_Graph);
	FmtAssert (IPA_Call_Graph == IPA_Graph_Undirected, (""));
    	IPA_Call_Graph = IPA_Call_Graph_Tmp;
	IPA_Call_Graph_Tmp = NULL;
	q_order.clear();
	node_map.clear();
    }
  }
#endif

#if !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    if (Alias_Nystrom_Analyzer && Get_Trace(TP_ALIAS,NYSTROM_CG_PRE_FLAG))
    {
      fprintf(stderr, "Printing initial ConstraintGraphs...\n");
      IPA_NystromAliasAnalyzer::aliasAnalyzer()->print(stderr);
    }
#endif

  IPA_Call_Graph_Built = TRUE;

  if (Get_Trace(TP_IPA, IPA_TRACE_TUNING)) {
    FILE *tmp_call_graph = fopen("cg_dump.log", "w");
    if (tmp_call_graph != NULL) {
      IPA_Call_Graph->Print_vobose(tmp_call_graph);
      fclose(tmp_call_graph);
    }
  }
} // Build_Call_Graph



static bool Check_Heuristic( IPA_NODE* caller,
			     IPA_NODE* callee,
			     INT64     edge_freq,
			     IPA_CALL_GRAPH* cg )
{
  /* Check whether inlining <callee> is allowed.
   */

  if( !IPA_Enable_Inline )
    return false;

  if( callee->Should_Be_Skipped() )
    return false;

  if( !IPA_Enable_Inline_Nested_PU && caller->Is_Nested_PU () )
    return false;

  if( caller == callee && !INLINE_Recursive )
    return false;

  if( callee->Has_Varargs() )
    return false;

  if( callee->Summary_Proc()->Is_alt_entry()  ||
      callee->Summary_Proc()->Has_alt_entry() || 
      caller->Summary_Proc()->Is_alt_entry() )
    return false;

  if( callee->Summary_Proc()->Has_formal_pragma() )
    return false;

  if( callee->Summary_Proc()->Has_mp_needs_lno() )
    return false;

  if( callee->Summary_Proc()->Has_noinline_parallel_pragma() )
    return false;

  if( (caller->Summary_Proc()->Has_parallel_pragma() ||
       caller->Summary_Proc()->Has_parallel_region_pragma()) &&
      callee->Summary_Proc()->Has_var_dim_array() )
    return false;

  if( caller->Summary_Proc()->Has_parallel_region_pragma() &&
      callee->Summary_Proc()->Has_pdo_pragma() )
    return false;

  if( callee->Summary_Proc()->Is_exc_inline() && !IPA_Enable_Exc )
    return false;

  if( callee->Summary_Proc()->Is_exc_inline() &&
      callee->Summary_Proc()->Has_pstatic() )
    return false;

  if( (UINT)cg->Node_Depth(callee) > IPA_Max_Depth )
    return false;

  if( !IPA_Enable_Lang ){
    if( (callee->Summary_Proc()->Get_lang() == LANG_F77) || 
	(caller->Summary_Proc()->Get_lang() == LANG_F77) ){
      if( (callee->Summary_Proc()->Get_lang() != LANG_F77) || 
	  (caller->Summary_Proc()->Get_lang() != LANG_F77) )
	return false;

      else if( (callee->Summary_Proc()->Get_lang() == LANG_F90) || 
	       (caller->Summary_Proc()->Get_lang() == LANG_F90) ){
	if( (callee->Summary_Proc()->Get_lang() != LANG_F90) || 
	    (caller->Summary_Proc()->Get_lang() != LANG_F90) )
	  return false;
      }
    }
  }

  return true;

  /* Now check the hotness of <callee>.
   */

  UINT32 callee_weight = callee->Weight();

  if( IPA_Use_Effective_Size && callee->Has_frequency() ){
    SUMMARY_FEEDBACK* fb = callee->Get_feedback();
    callee_weight = PU_Weight( fb->Get_effective_bb_count(),
			       fb->Get_effective_stmt_count(),
			       callee->PU_Size().Call_Count() );
  }

  const FB_FREQ cycle_ratio = (edge_freq / callee->Get_frequency() *
			       callee->Get_cycle_count()) / Total_cycle_count;
  const float cycle_ratio_float = cycle_ratio.Value();
  const float size_ratio = (float)callee_weight / (float)Orig_Prog_Weight;
  const float hotness = ( 100.0 * cycle_ratio_float / size_ratio );

  if( hotness < (float)IPA_Min_Hotness ){
    return false;
  }

  return true;
}



// ======================================================================
// Dead function elimination
// ======================================================================

enum DFE_STATE
{
    NOT_VISITED = 0,
    VISITED_BUT_UNDECIDED,
    VISITED_AND_KEEP,
    VISITED_AND_DELETE
};


enum DFE_ACTION
{
    MARK_USED,			// mark function and its descendants as used
    SEARCH_FOR_USED,		// search for externally callable
				// functions among unreachable functions
    MARK_DELETED		// definitely mark as deletable
};
 
//-------------------------------------------------------------------------
// Walk the graph pre-order, set the "deletable" bit if walk_only is false
//-------------------------------------------------------------------------
static void
Mark_Deletable_Funcs (NODE_INDEX v, DFE_ACTION action, mUINT8 *visited)
{
    IPA_NODE* node = IPA_Call_Graph->Graph()->Node_User(v);

    NODE_INDEX alt_entry_index = INVALID_NODE_INDEX;

    if (node->Summary_Proc()->Is_alt_entry()) {
	ALT_ENTRY_MAP::iterator iter = alt_entry_map->find (v);
	if (iter != alt_entry_map->end())
	    alt_entry_index = (*iter).second;
    }

    switch (action) {

    case MARK_USED:
	node->Clear_Deletable ();
	if (node->Is_Externally_Callable() || node->Is_Undeletable()) {
	    node->Set_Undeletable();
	}
	visited[v] = VISITED_AND_KEEP;
	break;

    case SEARCH_FOR_USED:
	if (node->Is_Externally_Callable() || node->Is_Undeletable()) {
	    node->Clear_Deletable ();
	    node->Set_Undeletable();
	    action = MARK_USED;
	    visited[v] = VISITED_AND_KEEP;
#if defined(_LIGHTWEIGHT_INLINER) 
        /* O0 does not delete functions with no inline attrib */
        } else if (Opt_Level == 0 && ! node->Has_Inline_Attrib()) {
          node->Clear_Deletable();
          node->Set_Undeletable();
          action = MARK_USED;
          visited[v] = VISITED_AND_KEEP;
#endif
	} else if (visited[v] == 0)
	    visited[v] = VISITED_BUT_UNDECIDED;
	break;

    case MARK_DELETED:
	if (node->Is_Externally_Callable () || node->Is_Undeletable() ||
	    PU_has_global_pragmas (node->Get_PU ())
#ifdef TODO
	    || node->Should_Be_Skipped()
#endif
	    ) {

	    node->Clear_Deletable ();
	    node->Set_Undeletable();
	    action = MARK_USED;
	    visited[v] = VISITED_AND_KEEP;
	} else {
	    node->Set_Deletable ();
	    IP_FILE_HDR& s = node->File_Header ();
	    s.proc_info[node->Proc_Info_Index()].state = IPA_UNUSED;
	    visited[v] = VISITED_AND_DELETE;
	}
	break;
    }

#ifdef KEY
    BOOL alt_entry_update = FALSE;
#endif

    if (alt_entry_index != INVALID_NODE_INDEX && action == MARK_USED &&
	visited[alt_entry_index] != VISITED_AND_KEEP) {
	IPA_NODE *n = IPA_Call_Graph->Graph()->Node_User(alt_entry_index);
	n->Clear_Deletable ();
	IP_FILE_HDR& s = n->File_Header();
	if (s.proc_info[node->Proc_Info_Index()].state == IPA_UNUSED)
	    s.proc_info[node->Proc_Info_Index()].state = IPA_ORIG;
	if (n->Is_Externally_Callable() || n->Is_Undeletable()) {
	    n->Set_Undeletable();
	}
	visited[alt_entry_index] = VISITED_AND_KEEP;
#ifdef KEY 
	alt_entry_update = TRUE;
#endif
    }

#ifdef TODO
    if (IPA_Enable_daVinci) {
	switch (action) {
	case MARK_USED:
	    cg_display->Mark_Used (v);
	    break;
	case MARK_DELETED:
	    cg_display->Mark_Deleted (v);
	    break;
	}
    }
#endif
    
    NODE_ITER vitr(IPA_Call_Graph->Graph(), v);
    for (NODE_INDEX vi = vitr.First_Succ(); vi != -1; vi = vitr.Next_Succ()) {
	if (visited[vi] == 0 || (action != SEARCH_FOR_USED &&
				 visited[vi] == VISITED_BUT_UNDECIDED))
	    Mark_Deletable_Funcs (vi, action, visited);
    }
   
#ifdef KEY 
    if (alt_entry_update)
    {
      // Bug 12048: It has been decided to keep the alternate entry point,
      // traverse its successors to note they are reachable.
      Is_True (alt_entry_index != INVALID_NODE_INDEX,
               ("Mark_Deletable_Funcs: Invalid alternate entry point"));
      NODE_ITER vitr(IPA_Call_Graph->Graph(), alt_entry_index);
      for (NODE_INDEX vi = vitr.First_Succ(); vi != -1; vi = vitr.Next_Succ()) {
        if (visited[vi] == 0 || (action != SEARCH_FOR_USED &&
                                 visited[vi] == VISITED_BUT_UNDECIDED))
          Mark_Deletable_Funcs (vi, action, visited);
      }
    }
#endif
 
} // Mark_Deletable_Funcs


#ifndef _LIGHTWEIGHT_INLINER
/* reset the mod/ref count corresponding to the deleted functions */
static void
Reset_modref_count (IPA_NODE *node)
{

    IP_FILE_HDR& hdr = node->File_Header ();
    SUMMARY_SYMBOL* sym_table = IPA_get_symbol_array (node);
    SUMMARY_GLOBAL* gnode = IPA_get_global_array (node);

    if (gnode == NULL || sym_table == NULL)
	return;

    SUMMARY_PROCEDURE *pnode = node->Summary_Proc();
    INT32 max_size = pnode->Get_global_index () + pnode->Get_global_count ();

    gnode += pnode->Get_global_index ();
    for (INT i = pnode->Get_global_index (); i < max_size; i++, gnode++)
	if (gnode->Get_refcount () || gnode->Get_modcount ()) {
	    SUMMARY_SYMBOL *snode = sym_table + gnode->Get_symbol_index ();
	    ST_IDX st_idx = snode->St_idx ();
	    Update_reference_count (&St_Table[st_idx],
				    - gnode->Get_refcount (),
				    - gnode->Get_modcount (),
				    snode->Is_cmod ());
	}
} // Reset_modref_count
#endif // _LIGHTWEIGHT_INLINER


static UINT32
Delete_Function (NODE_INDEX node, BOOL update_modref_count, mUINT8 *visited)
{
    IPA_PRED_ITER pred_iter (node);

    for (pred_iter.First (); !pred_iter.Is_Empty (); pred_iter.Next ()) {
	IPA_EDGE *edge = pred_iter.Current_Edge ();
	if (edge) {
	    pred_iter.Set_Current_Edge(0);
	}
    }

    IPA_SUCC_ITER succ_iter (node);
    
    for (succ_iter.First (); !succ_iter.Is_Empty (); succ_iter.Next ()) {
	IPA_EDGE *edge = succ_iter.Current_Edge ();
	if (edge) {
	    succ_iter.Set_Current_Edge(0);
	}
    }

    IPA_NODE* ipa_node = (IPA_NODE*) IPA_Call_Graph->Graph()->Delete_Node (node);

    if (Trace_IPA || Trace_Perf)
	fprintf (TFile, "%s deleted (unused)\n",
		 ipa_node->Name ());

#if !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    if (Alias_Nystrom_Analyzer)
      IPA_NystromAliasAnalyzer::aliasAnalyzer()->
                                deleteConstraintGraph(ipa_node);
#endif

#ifndef _LIGHTWEIGHT_INLINER

    if (update_modref_count) 
        Reset_modref_count (ipa_node);

#endif // _LIGHTWEIGHT_INLINER

    Set_ST_is_not_used (ipa_node->Func_ST ());

    Delete_Function_In_File (ipa_node->File_Header (),
			     ipa_node->Proc_Info_Index ());

    const PU_Info* pu = ipa_node->PU_Info ();
    UINT32 size = 0;
    if (pu != NULL) {
        for (pu = PU_Info_child (pu); pu; pu = PU_Info_next (pu)) {
	    // If there are nested functions, all need to be deleted

            const AUX_PU& aux_pu =
                Aux_Pu_Table [ST_pu (St_Table [PU_Info_proc_sym (pu)])];
            IPA_NODE* child = IPA_Call_Graph->Graph()->Node_User (AUX_PU_node (aux_pu));
            if (child) {
		NODE_INDEX c_vi = child->Node_Index();
		visited[c_vi] = VISITED_AND_KEEP;
	        size += Delete_Function (c_vi, update_modref_count, visited);
            Orig_Prog_WN_Count -= (UINT32)(child->Get_wn_count());//INLINING_TUNING
	    }
	}
    }

    Orig_Prog_WN_Count -= (UINT32)(ipa_node->Get_wn_count());//INLINING_TUNING
    return ipa_node->Weight () + size;
} // Delete_Function 


//-------------------------------------------------------------------------
// Walk the call graph and search for unreachable nodes, mark them as
// deletable.  Unreachable nodes are those that are not externally
// callable *AND* not reachable from any node that is externally
// callable.  See IPA_NODE::Is_Externally_Callable().
//
// return the total size of the nodes deleted
//-------------------------------------------------------------------------
UINT32
Eliminate_Dead_Func (BOOL update_modref_count)
{
    mUINT8 *visited = (mUINT8 *)
	alloca (GRAPH_vmax (IPA_Call_Graph->Graph()) * sizeof(mUINT8));
    BZERO (visited, sizeof(mUINT8) * GRAPH_vmax(IPA_Call_Graph->Graph()));

    NODE_INDEX vi;

    // First, mark all reachable nodes
    NODE_ITER vitr(IPA_Call_Graph->Graph(), IPA_Call_Graph->Root());
    for (vi = vitr.First_Succ(); vi != -1; vi = vitr.Next_Succ())
	Mark_Deletable_Funcs (vi, SEARCH_FOR_USED, visited);

    // Reinitiallize the iterator
    new (&vitr) NODE_ITER (IPA_Call_Graph->Graph(), IPA_Call_Graph->Root());

    // Now, work on all unreachable nodes
    for (vi = vitr.First_Succ(); vi != -1; vi = vitr.Next_Succ()) 
	if (visited[vi] == 0 || visited[vi] == VISITED_BUT_UNDECIDED)
	    Mark_Deletable_Funcs (vi, MARK_DELETED, visited);

    UINT32 size = 0;
    for (vi = 0; vi < GRAPH_vmax(IPA_Call_Graph->Graph()); vi++)
	if (visited[vi] == VISITED_AND_DELETE)
	    size += Delete_Function (vi, update_modref_count, visited);

    DevWarn ("TODO: implement *skip* option");

    /* reconnect dangling nodes, if any.  This is possible if all
       predecessors of a node are deleted, but the node itself is not */
    Connect_call_graph();

    return size;
} // Eliminate_Dead_Func 



//----------------------------------------------------------------
// Read the PU from the input file
// as a side effect, create parent pointers              
//----------------------------------------------------------------
void
IPA_NODE::Read_PU (BOOL readtree)
{
    if (!Is_Mempool_Initialized()) {
	Set_Mempool_Initialized();
	MEM_POOL_Initialize(Mem_Pool(),Name(),1); /* the name of the MEMPOOL is null */
	MEM_POOL_Push(Mem_Pool());              /* just to save time and energy */
    }

    if (readtree)
        IP_READ_pu (this, File_Header(), Proc_Info_Index(), Mem_Pool() );

    if (Cur_PU_Feedback)
	Set_Feedback ();

    if (!Parent_Map()) {
        ::PU_Info *pu = PU_Info();
        Current_Map_Tab = PU_Info_maptab(pu);
        Set_Parent_Map(WN_MAP_Create(Mem_Pool()));
        WN_Parentize(PU_Info_tree_ptr(pu), Parent_Map(), Current_Map_Tab);
    }
}

static void
read_pu_including_parents(IPA_NODE* node)
{

    IPA_NODE* parent = Get_Parent_Of_Nested_PU(node);
    if (parent != NULL) {
	read_pu_including_parents(parent);
    }

    if (node->Scope_Table() == NULL) {
        node->Read_PU(TRUE);
        node->Set_Scope(Scope_tab);
    }
    else { 
	// To set up Scope_tab correctly, copy the node's Scope_table info
 	// to Scope_tab
        node->Read_PU(FALSE);
        SYMTAB_IDX lexical_level = node->Lexical_Level();
	Scope_tab[lexical_level] = node->Scope_Table()[lexical_level];
    }
	
}


/* Release the memory possessed by the PU */
void
IPA_NODE::Un_Read_PU ()
{
  MEM_POOL_Pop(Mem_Pool());
  MEM_POOL_Delete(Mem_Pool());
  Clear_Mempool_Initialized();
  WN_MAP_Delete(Parent_Map());
  Set_Scope(NULL);
  Set_Parent_Map(0);
}


SCOPE *
IPA_NODE::Scope() 
{
    SCOPE* old_scope = Scope_tab;

    if (_scope_tab != NULL) {
	Scope_tab = _scope_tab;
#ifdef KEY
	// read pu only if not builtin
        if (!this->Is_Builtin())
#endif
        read_pu_including_parents(this); // Tree somehow has been read already,
					 // as in the case of the standalone inliner
	Scope_tab = old_scope;
	return _scope_tab;
    }

    // set up a scope table that is large enough for this lexical level
    INT size = (Lexical_Level()+1) * sizeof(SCOPE);
    SCOPE *new_scope_tab = (SCOPE *)
        MEM_POOL_Alloc (Malloc_Mem_Pool, size);
    BZERO(new_scope_tab, size);

    // Copy only the Global SYMTAB info
    memcpy(new_scope_tab, Scope_tab, sizeof(SCOPE)*2);

    Scope_tab = new_scope_tab;

    // Read in itself and all its parents
#ifdef KEY
    // read pu only if not builtin
    if (!this->Is_Builtin())
#endif
    read_pu_including_parents(this);

    Scope_tab = old_scope;
    return new_scope_tab;
}

//----------------------------------------------------------------
// given a procedure node, return the whirl node
//----------------------------------------------------------------
WN*
IPA_NODE::Whirl_Tree (BOOL readtree)
{
    ::PU_Info *pu = PU_Info();
    if (PU_Info_state(pu, WT_TREE) != Subsect_InMem) {
        if (!readtree) return NULL;
        Read_PU(TRUE);
    }
    return PU_Info_tree_ptr(pu);
}

//----------------------------------------------------------------
// set the whirl node
//----------------------------------------------------------------
void
IPA_NODE::Set_Whirl_Tree (WN *wn)
{
    ::PU_Info *pu = this->PU_Info();
    Set_PU_Info_tree_ptr(pu, wn);
    Set_PU_Info_state(pu, WT_TREE, Subsect_InMem);
}

#if defined(KEY) && !defined(_LIGHTWEIGHT_INLINER)
#include "be_ipa_util.h"

// This function returns TRUE if the input block of WN is a simple straight line
// code without a return statement; otherwise it returns FALSE.
static BOOL
block_is_straight_line_no_return(WN *wn)
{
  if (wn == NULL)
    return TRUE; // degenerate straight line code
  if (WN_operator(wn) != OPR_BLOCK)
    return FALSE;
  wn = WN_first(wn);
  while (wn != NULL)
  {
    if (WN_operator(wn) == OPR_PRAGMA ||
        WN_operator(wn) == OPR_CALL ||
        WN_operator(wn) == OPR_ICALL ||
        WN_operator(wn) == OPR_STID)
    {
      // OK
    }
    else if (WN_operator(wn) == OPR_IF)
    {
      if (!block_is_straight_line_no_return(WN_then(wn)) ||
          !block_is_straight_line_no_return(WN_else(wn)))
        return FALSE;
    }
    else
      return FALSE;
    wn = WN_next(wn);
  }
  return TRUE;
}

static ST *tracked_global_var_st = NULL;

// This function identifies global vars inside the input function whose function
// exit value is the same as their entry value.
static UINT32
analyze_global_var_returns_same_entry_value(IPA_NODE *node)
{
  WN *wn;
  ST *tracked_local_var_st = NULL;
  int same_as_entry_value = 0;

  // current restrictions: the input function must only have straight line code
  // (and if constructs), and we only identify one such global var for now
  wn = node->Whirl_Tree(TRUE);
  if (WN_operator(wn) != OPR_FUNC_ENTRY)
    return 0;
  wn = WN_kid(wn, WN_kid_count(wn)-1);
  if (WN_operator(wn) != OPR_BLOCK)
    return 0;
  wn = WN_first(wn); // body
  while (wn != NULL)
  {
    switch (WN_operator(wn))
    {
      case OPR_PRAGMA:
        break;
      case OPR_IF:
        if (!block_is_straight_line_no_return(WN_then(wn)) ||
            !block_is_straight_line_no_return(WN_else(wn)))
          return 0; // a return statement inside a if/then/else block is bad
        // fall through
      case OPR_CALL:
        same_as_entry_value = 0; // the value of global var may change
        break;
      case OPR_STID:
        if (tracked_global_var_st == NULL && Is_Global_Symbol(WN_st(wn)))
          return 0; // some global var gets updated before it's saved; no good
        if (tracked_global_var_st == NULL &&
            Is_Local_Symbol(WN_st(wn)) && !ST_addr_taken(WN_st(wn)) &&
            WN_operator(WN_kid0(wn)) == OPR_LDID &&
            Is_Global_Symbol(WN_st(WN_kid0(wn))))
        {
          // found the first occurrence of "local_var = global_var"; start
          // tracking.  (We will only track one (the first) such for now.)
          tracked_global_var_st = WN_st(WN_kid0(wn));
          tracked_local_var_st = WN_st(wn);
          if (tracked_global_var_st == NULL || tracked_local_var_st == NULL)
            return 0;
          break;
        }
        if (tracked_local_var_st != NULL && WN_st(wn) == tracked_local_var_st)
          return 0;  // this same local_var gets updated; that is, the saved
            // value of global_var may be gone; all bets are off
        if (WN_st(wn) == tracked_global_var_st)
        {
          if (WN_operator(WN_kid0(wn)) == OPR_LDID &&
              WN_st(WN_kid0(wn)) == tracked_local_var_st)
          {
            // found "global_var = local_var", and we know that the value of
            // local_var has not been changed (because its address is not taken,
            // and there have not been an explicit assignment into it)
            same_as_entry_value = 1;
          }
          else
            same_as_entry_value = 0; // global_var = ...
        }
        break;
      case OPR_RETURN:
      case OPR_RETURN_VAL:
        if (same_as_entry_value == 0)
          return 0;
        // debug print
        // fprintf(stderr, "same_entry_exit var (%d %s) in function %s\n", ST_IDX_index(ST_st_idx(tracked_global_var_st)), ST_name(tracked_global_var_st), node->Name());
        return ST_IDX_index(ST_st_idx(tracked_global_var_st));
      default:
        return 0; // loops, switches, etc. are not allowed, for now
    }
    wn = WN_next(wn);
  }
  return 0;
}

// Given the input STID wn, this function returns TRUE if the RHS of this STID
// can be proved to be the integer constant 1; otherwise, it returns FALSE.
static BOOL
RHS_is_1(WN *stid_wn)
{
  WN *ldid_wn;
  ST *var_st;
  WN *wn;

  if (stid_wn == NULL || WN_operator(stid_wn) != OPR_STID)
    return FALSE;
  ldid_wn = WN_kid0(stid_wn);
  if (WN_operator(ldid_wn) == OPR_INTCONST && WN_const_val(ldid_wn) == 1)
    return TRUE;
  if (WN_operator(ldid_wn) == OPR_LDID)
  {
    var_st = WN_st(ldid_wn);
    // walk up the IR to find a (local) reaching def for this RHS
    wn = WN_prev(stid_wn);
    if (wn != NULL && WN_operator(wn) == OPR_PRAGMA)
      wn = WN_prev(wn);
    while (wn != NULL && WN_operator(wn) == OPR_STID)
    {
      if (WN_st(wn) == var_st)
      {
        if (WN_operator(WN_kid0(wn)) == OPR_INTCONST &&
            WN_const_val(WN_kid0(wn)) == 1)
          return TRUE;
        else
          return FALSE;
      }
      wn = WN_prev(wn);
      if (wn != NULL && WN_operator(wn) == OPR_PRAGMA)
        wn = WN_prev(wn);
    }
    return FALSE;
  }
  // anything harder than the above, give up
  return FALSE;
}

static WN *return_wn;
static int continue_with_walk_tree_for_returns_and_global_var;

// This function walks the input wn in postorder, counts the number of return
// statements, and marks the last-visited definition of a global variable whose
// RHS of the assignment is 1.
static void
walk_tree_for_returns_and_global_var(WN *wn)
{
  if (wn == NULL || continue_with_walk_tree_for_returns_and_global_var == 0)
    return;

  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        walk_tree_for_returns_and_global_var(child_wn);
        if (continue_with_walk_tree_for_returns_and_global_var == 0)
          return;
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
        {
          walk_tree_for_returns_and_global_var(child_wn);
          if (continue_with_walk_tree_for_returns_and_global_var == 0)
            return;
        }
      }
    }
  }

  // nodes of interest
  switch (WN_operator(wn))
  {
    case OPR_STID:
      if (Is_Global_Symbol(WN_st(wn)) && RHS_is_1(wn))
        tracked_global_var_st = WN_st(wn); // last one wins
      break;
    case OPR_RETURN:
    case OPR_RETURN_VAL:
      if (return_wn != NULL)
      {
        // more than one return statement
        return_wn = NULL;
        tracked_global_var_st = NULL;
        continue_with_walk_tree_for_returns_and_global_var = 0;
        return;
      }
      return_wn = wn;
      break;
    default:
      break;
  }
  return;
}

#define ENTRY 0
#define ONE 1
#define UNKNOWN 2
static int phi_outermost_argument;
static int phi_if_true_argument;
static int phi_if_false_argument;
static int in_if_true_branch;
static int in_if_false_branch;
static int continue_with_finding_reaching_def;

// This function checks if there is a definition of the tracked_global_var_st or
// a call inside the input wn.
static BOOL
found_def_or_call(WN *wn)
{
  if (wn == NULL)
    return FALSE;

  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        if (found_def_or_call(child_wn))
          return TRUE;
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
        {
          if (found_def_or_call(child_wn))
            return TRUE;
        }
      }
    }
  }

  // nodes of interest
  switch (WN_operator(wn))
  {
    case OPR_STID:
      if (WN_st(wn) == tracked_global_var_st)
        return TRUE;
      break;
    case OPR_CALL:
      return TRUE;
    default:
      break;
  }
  return FALSE;
}

// This function finds the reaching def of tracked_global_var_st at program
// exit.  The answer is encoded as a SSA phi function, with 3 arguments:
// phi_outermost_argument, phi_if_true_argument, phi_if_false_argument.
static void
find_reaching_def_for_tracked_global_var_st(WN *wn)
{
  if (continue_with_finding_reaching_def == 0)
    return;

  while (wn != NULL)
  {
    switch (WN_operator(wn))
    {
      case OPR_BLOCK:
        find_reaching_def_for_tracked_global_var_st(WN_first(wn));
        if (continue_with_finding_reaching_def == 0)
          return;
        break;
      case OPR_PRAGMA:
      case OPR_LABEL:
      case OPR_ISTORE:
      case OPR_RETURN:
      case OPR_RETURN_VAL:
        break;
      case OPR_STID:
        if (WN_st(wn) == tracked_global_var_st)
        {
          // found a def
          if (in_if_true_branch == 1)
          {
            if (RHS_is_1(wn))
              phi_if_true_argument = ONE;
            else
              phi_if_true_argument = UNKNOWN;
          }
          else if (in_if_false_branch == 1)
          {
            if (RHS_is_1(wn))
              phi_if_false_argument = ONE;
            else
              phi_if_false_argument = UNKNOWN;
          }
          else
          {
            if (RHS_is_1(wn))
              phi_outermost_argument = ONE;
            else
              phi_outermost_argument = UNKNOWN;
            // this kills any previous phi's
            phi_if_true_argument = phi_outermost_argument;
            phi_if_false_argument = phi_outermost_argument;
          }
        }
        break;
      case OPR_IF:
        // for now, only analyze one level of if statement
        if (in_if_true_branch == 1)
        {
          if (found_def_or_call(WN_then(wn)) || found_def_or_call(WN_else(wn)))
            phi_if_true_argument = UNKNOWN;
        }
        else if (in_if_false_branch == 1)
        {
          if (found_def_or_call(WN_then(wn)) || found_def_or_call(WN_else(wn)))
            phi_if_false_argument = UNKNOWN;
        }
        else
        {
          in_if_true_branch = 1;
          find_reaching_def_for_tracked_global_var_st(WN_then(wn));
          in_if_true_branch = 0;
          if (continue_with_finding_reaching_def == 0)
            return;
          // if we get UNKNOWN result in this outermost if, there is no hope
          if (phi_if_true_argument == UNKNOWN)
          {
            continue_with_finding_reaching_def = 0;
            return;
          }
          in_if_false_branch = 1;
          find_reaching_def_for_tracked_global_var_st(WN_else(wn));
          in_if_false_branch = 0;
          if (continue_with_finding_reaching_def == 0)
            return;
          // if we get UNKNOWN result in this outermost if, there is no hope
          if (phi_if_false_argument == UNKNOWN)
          {
            continue_with_finding_reaching_def = 0;
            return;
          }
        }
        break;
      case OPR_CALL:
      default:
        // assume the value of tracked_global_var_st may change
        if (in_if_true_branch == 1)
          phi_if_true_argument = UNKNOWN;
        else if (in_if_false_branch == 1)
          phi_if_false_argument = UNKNOWN;
        else
        {
          phi_outermost_argument = UNKNOWN;
          // this kills any previous phi's
          phi_if_true_argument = phi_outermost_argument;
          phi_if_false_argument = phi_outermost_argument;
        }
        break;
    }
    wn = WN_next(wn);
  }
  return;
}

// Walks the func body to determine if the function has a call to 
// either an exit sys call or to another function that never returns
// Ideally, this is true if the block containing the exit call (or a call
// to another function that does not return) post-dominates the entry block
// For now we look for the most basic patterns to determine the same.
static BOOL IPA_check_if_proc_does_not_return(IPA_NODE *node)
{
  PU& pu = node->Get_PU();
  if (node->PU_Can_Throw() || PU_calls_setjmp(pu) || PU_calls_longjmp(pu))
    return FALSE;

  TY_IDX ret_type = TY_ret_type(PU_prototype(pu));
  if (TY_kind(ret_type) != KIND_VOID)
    return FALSE;

  WN *func_entry = node->Whirl_Tree(FALSE);
  if (WN_operator(func_entry) != OPR_FUNC_ENTRY)
    return FALSE;

  WN *func_body = WN_kid(func_entry, WN_kid_count(func_entry)-1);
  if (WN_operator(func_body) != OPR_BLOCK)
    return FALSE;

  for (WN *wn = WN_first(func_body); wn; wn = WN_next(wn)) 
  {
    switch (WN_operator(wn)) {
      case OPR_PRAGMA:
      case OPR_LDID:
      case OPR_STID:
        break;
      case OPR_IF:
        // check if there is a return inside the then/else block 
        if (!block_is_straight_line_no_return(WN_then(wn)) ||
            !block_is_straight_line_no_return(WN_else(wn)))
          return FALSE; 
        break;
      case OPR_CALL:
        if (!strcmp("exit", ST_name(WN_st(wn))) ||
            WN_Call_Never_Return(wn))
          return TRUE;
        break;
      default: 
        return FALSE;
    }
  }
  return FALSE;
}

// Checks if the function does not return, and if so, sets the
// no return bit at the call site and recursively checks for the
// same for each of the callers.
static void IPA_identify_no_return_proc_recursive(IPA_NODE *node)
{
  BOOL no_return = IPA_check_if_proc_does_not_return(node);

  if (!no_return)
    return;

  // Mark its callers appropriately
  IPA_PRED_ITER preds (node->Node_Index());
  for (preds.First(); !preds.Is_Empty(); preds.Next())
  {
    IPA_EDGE * edge = preds.Current_Edge();
    if (edge) 
    {
      IPA_NODE *caller = IPA_Call_Graph->Caller(edge);
      IPA_NODE_CONTEXT context(caller);
      caller->Whirl_Tree(TRUE);
      if (caller->Has_Pending_Icalls() || 
          caller->Has_Pending_Virtual_Functions())
        continue;
      IPA_Call_Graph->Map_Callsites(caller);
      WN *call = edge->Whirl_Node();
      WN_Set_Call_Never_Return(call);
      IPA_identify_no_return_proc_recursive(caller);
    }
  }
}

// Identify if a procedure returns to its caller
void IPA_identify_no_return_procs()
{
  IPA_NODE_ITER cg_iter(IPA_Call_Graph, DONTCARE);
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next())
  {
    IPA_NODE *node = cg_iter.Current();
    if (!node)
      continue;

    // We cannot process on nested PUs because IPL requires
    // that their parent PUs be processed first
    if (node->Is_Nested_PU() || node->Summary_Proc()->Is_alt_entry())
      return;

    // Start with the leaf nodes
    IPA_SUCC_ITER succs(node->Node_Index());
    succs.First();
    if (succs.Is_Empty()) 
    {
      IPA_NODE_CONTEXT context(node);
      IPA_identify_no_return_proc_recursive(node);
    }
  }
}

// This function identifies global vars inside the input function whose function
// exit value is the same as their entry value, or that value is 1.
static UINT32
analyze_global_var_returns_same_entry_value_or_1(IPA_NODE *node)
{
  WN *wn;

  // current restrictions: the input function must only have straight line code
  // (and if constructs), and we only identify one such global var for now
  wn = node->Whirl_Tree(TRUE);
  if (WN_operator(wn) != OPR_FUNC_ENTRY)
    return 0;
  wn = WN_kid(wn, WN_kid_count(wn)-1);
  if (WN_operator(wn) != OPR_BLOCK)
    return 0;

  // make sure that there is only one return in the entire program and identify
  // the most promising global variable to track
  return_wn = NULL;
  tracked_global_var_st = NULL;
  continue_with_walk_tree_for_returns_and_global_var = 1;
  walk_tree_for_returns_and_global_var(wn);
  if (return_wn == NULL || return_wn != WN_last(wn) ||
      tracked_global_var_st == NULL || ST_addr_taken(tracked_global_var_st))
    return 0;

  // now attempt to find the reaching def of tracked_global_var_st at program
  // exit.  If it is program entry, "=1", or phi(program entry,"=1"), return the
  // tracked_global_var_st.  (Do we have SSA here?)

  // we will only allow one level of if-then-else in the analysis.  So there can
  // only be a maximum of 3 phi arguments:  outermost (not inside any if
  // statement), if_true (inside true branch of if statement), if_false (inside
  // false branch of if statement)
  phi_outermost_argument = ENTRY; // program entry is the default value
  phi_if_true_argument = ENTRY;
  phi_if_false_argument = ENTRY;
  in_if_true_branch = 0;
  in_if_false_branch = 0;
  continue_with_finding_reaching_def = 1;
  find_reaching_def_for_tracked_global_var_st(wn);

  if (phi_outermost_argument == UNKNOWN || phi_if_true_argument == UNKNOWN ||
      phi_if_false_argument == UNKNOWN)
    return 0;
  // all the phi arguments are either ONE or ENTRY
  // debug print
  // fprintf(stderr, "same_entry_exit_or_1 var (%d %s) in function %s\n", ST_IDX_index(ST_st_idx(tracked_global_var_st)), ST_name(tracked_global_var_st), node->Name());
  return ST_IDX_index(ST_st_idx(tracked_global_var_st));
}

static void
Add_Mod_Ref_Info (IPA_NODE * node)
{
  UINT32 index;
  UINT32 same_entry_exit_value_or_1_var_st_index;

  // NOTE: Lots of optimizations can be done in the implementation
  // below.
  //
  const INT bits_per_byte = 8;
  const int bitsize = sizeof (mUINT8) * bits_per_byte;
  const IPAA_NODE_INFO * info = node->Mod_Ref_Info();

  Is_True (info, ("Add_Mod_Ref_Info: Node should have mod-ref info"));
  New_Mod_Ref_Info (index);

  // PU id
  Mod_Ref_Info_Table[index].pu_idx = ST_pu (node->Func_ST());

  // How many bytes do we need?
  INT bv_size = ST_Table_Size (GLOBAL_SYMTAB);
  if (bv_size % bits_per_byte > 0)
    bv_size = bv_size / bits_per_byte + 1;
  else
    bv_size /= bits_per_byte;

  // MOD
  mUINT8 * MOD = CXX_NEW_ARRAY (mUINT8, bv_size, Malloc_Mem_Pool);
  BZERO (MOD, bv_size);

  // REF
  mUINT8 * REF = CXX_NEW_ARRAY (mUINT8, bv_size, Malloc_Mem_Pool);
  BZERO (REF, bv_size);

  // SAME_ENTRY_EXIT_VALUE_OR_1
  mUINT8 * SAME_ENTRY_EXIT_VALUE_OR_1 = CXX_NEW_ARRAY (mUINT8, bv_size,
    Malloc_Mem_Pool);
  bzero (SAME_ENTRY_EXIT_VALUE_OR_1, bv_size);

  same_entry_exit_value_or_1_var_st_index =
    analyze_global_var_returns_same_entry_value_or_1(node); // currently, there
    // will only be one such global var (if at all); however, the following
    // framework is ready to handle arbitrarily many such global vars in the
    // future
  for (INT i=1; i<ST_Table_Size (GLOBAL_SYMTAB); i++)
  {
    ST * st = &St_Table(GLOBAL_SYMTAB, i);
    if (ST_class (st) != CLASS_VAR) continue;

    BOOL mod_info = info->Is_def_elmt (i);
    BOOL ref_info = info->Is_eref_elmt (i);
    BOOL same_entry_exit_value_or_1_info =
      (i == same_entry_exit_value_or_1_var_st_index);

    mUINT8 bit_to_set = 1 << (bitsize - 1 - (i % bitsize));

    if (mod_info)
      *(MOD + i / bitsize) |= bit_to_set;

    if (ref_info)
      *(REF + i / bitsize) |= bit_to_set;

    if (same_entry_exit_value_or_1_info)
      *(SAME_ENTRY_EXIT_VALUE_OR_1 + i / bitsize) |= bit_to_set;
  }

  Mod_Ref_Info_Table[index].mod = MOD;
  Mod_Ref_Info_Table[index].ref = REF;
  Mod_Ref_Info_Table[index].same_entry_exit_value_or_1 =
    SAME_ENTRY_EXIT_VALUE_OR_1;
  // This can be optimized later, and be different for different PUs
  Mod_Ref_Info_Table[index].size = bv_size;
}
#endif // KEY && !_LIGHTWEIGHT_INLINER

// --------------------
// Write PU out to file
// --------------------
void 
IPA_NODE::Write_PU ()
{ 
  IP_FILE_HDR& file_hdr = File_Header();
  IP_PROC_INFO& proc_info = IP_FILE_HDR_proc_info (file_hdr)[Proc_Info_Index()];
  
  if (Summary_Proc()->Is_alt_entry()) {
    Set_IP_PROC_INFO_state (proc_info, IPA_DELETED);
    Inc_IP_FILE_HDR_num_procs_processed (file_hdr);
  } else {
    IPA_NODE_CONTEXT context(this);
#ifdef Is_True
    WN* w = Whirl_Tree(FALSE);
    if (w && !Is_Nested_PU()) {
      WN_verifier(w);
    }
#endif
    if ((IP_PROC_INFO_state (proc_info) != IPA_WRITTEN) || !Has_Recursive_In_Edge())
    {
#if defined(KEY) && !defined(_LIGHTWEIGHT_INLINER)
        // Use IPA mod/ref before IP_WRITE_pu frees resources
        if (Mod_Ref_Info())
          Add_Mod_Ref_Info (this);
#endif
        IP_WRITE_pu(&file_hdr, Proc_Info_Index()); 
#ifdef KEY
// Mark this node as written, which actually implies all its EH information
// have been processed and must not be processed if this PU is written again.
        Set_PU_Write_Complete ();
#endif
    }
  }
/* To enable this code once resolving Write_PU() of a newly created PU from
   partial inlining.
  if (Part_Inl_Clone()) {
     Part_Inl_Clone()->Write_PU();
  }
*/
}

//----------------------------------------------------------------
//  conservatively determine if the function can potentially be called
//  from outside.  Any function whose address is taken and is neither
//  EXPORT_INTERNAL nor EXPORT_LOCAL is considered callable.
//----------------------------------------------------------------
BOOL
IPA_NODE::Is_Externally_Callable ()
{
    // note, if the node has been cloned then it is can
    // no longer be called from the outside

    const ST* func_st = Func_ST ();

    if (ST_addr_saved(func_st) || ST_addr_passed (func_st)) 
	return TRUE;

    if (ST_export (func_st) == EXPORT_LOCAL_INTERNAL ||
        ST_export (func_st) == EXPORT_LOCAL)
	return FALSE;

#ifndef _LIGHTWEIGHT_INLINER
    const AUX_ST& aux_st = Aux_St_Table[ST_st_idx (func_st)];

    if (AUX_ST_flags (aux_st, USED_IN_OBJ|USED_IN_DSO|ADDR_TAKEN_IN_OBJ))
	return TRUE;

    if (ST_export (func_st) == EXPORT_INTERNAL ||
	ST_export (func_st) == EXPORT_HIDDEN )
	return FALSE;
#else
    // since we are in standalone inliner which is invoked for
    // single translation unit, all global functions are callable
    // by other TUs in the same modules (DSO or a.out)
    // even for inline function  which is not preemptible we
    // still need to export them since in different .o (in same 
    // module), they can be called from there, and C doesn't require
    // inline function be defined in every translation unit.
    // for C++, One Definition Rule requires each inline function 
    // be defined at every TU using the inline function, so
    // these inline function can not be called by other .o
    // in theory
    if (PU_is_marked_inline(Pu_Table [ST_pu (func_st)]) &&
        Is_Lang_CXX() &&
        (ST_export (func_st) == EXPORT_INTERNAL ||
	ST_export (func_st) == EXPORT_HIDDEN ))
	return FALSE;
#endif // _LIGHTWEIGHT_INLINER

    return TRUE;

} // IPA_NODE::Is_Externally_Callable

//------------------------------------------------------------------
// Clear out the cloned callee symtab for other inlining pairs
//-------------------------------------------------------------------
void
IPA_NODE::Clear_Cloned_Symtab ()
{
    if (Cloned_Symtab()) {
        CXX_DELETE (Cloned_Symtab(), Malloc_Mem_Pool);
	Set_Cloned_Symtab(NULL);
    }
} // IPA_NODE::Clear_cloned_symtab


// ====================================================================
//
// IPA_EDGE::Print / IPA_EDGE::Trace
//
// Print the information associated with a callgraph edge.  The invert
// parameter to these routines means to invert the sense of caller and
// callee in printing the callee.  This is useful in cases where the
// callgraph has been inverted so that the caller appears to be the
// callee.  It is defaulted to FALSE in the header.
//
// ====================================================================
void
IPA_EDGE::Print ( const FILE* fp,		// File to which to print
                  const IPA_CALL_GRAPH* cg,	// Underlying callgraph
                  BOOL invert ) const		// Invert edge?
{
  IPA_NODE* caller = cg->Caller(Edge_Index());
  IPA_NODE* callee = cg->Callee(Edge_Index());

  fprintf ( (FILE*) fp,
	    "name = %-20s (ix:%d, f:%02x:%02x, @%p) callsite %p:%d,%x\n",
	    invert ? caller->Name() : callee->Name(), 
            Edge_Index(), 
            _flags,
	    EDGE_etype(&GRAPH_e_i(cg->Graph(), Edge_Index())),
	    this,
	    Summary_Callsite(),Summary_Callsite()->Get_callsite_id(),
	    Summary_Callsite()->Get_state());
}

// ====================================================================
void
IPA_EDGE::Trace ( const IPA_CALL_GRAPH *cg, BOOL invert ) const
{
  Print ( TFile, cg, invert );
}

// Given a pu, get its corresponding call graph node.
IPA_NODE* Get_Node_From_PU(PU_Info* pu) 
{
  Is_True(pu != 0, ("Get_Node_From_PU: pu must not be null"));
  Is_True(IPA_Call_Graph->Graph() != 0, ("Get_Node_From_PU: Call graph not initialized"));

  // Get the ST index from the pu.
  ST_IDX idx = PU_Info_proc_sym(pu);
  Is_True(ST_IDX_level(idx) == GLOBAL_SYMTAB,
          ("Get_Node_From_PU: bad st index level %d, should be %d",
           ST_IDX_level(idx), GLOBAL_SYMTAB));

  // Get the PU index from the ST index.
  PU_IDX pu_idx = ST_pu(St_Table[idx]);
  Is_True(pu_idx > PU_IDX_ZERO && pu_idx < PU_Table_Size(),
          ("Get_Node_From_PU: bad pu index %d, not in range 0 <= idx < %d",
           pu_idx, PU_Table_Size()));

  // Using auxiliary table, get the call graph node index.
  NODE_INDEX node_idx = AUX_PU_node(Aux_Pu_Table[pu_idx]);
  IPA_NODE* result = IPA_Call_Graph->Graph()->Node_User(node_idx);

#ifdef KEY
  // bug 11647: Verify that the node is sane. The node may be null if
  // it has been deleted by DFE (see PU_Deleted()). The caller should
  // handle a null return value in such a case.
  if (result == NULL)
    return NULL;

  // If it's a builtin, skip checks for info it doesn't have.
  if (result->Is_Builtin())
    return result;
#else
  // Verify that the node is sane.
  Is_True(result != 0, ("Get_Node_From_PU: null call graph node"));
#endif
  Is_True(result->PU_Info() != 0, ("Get_Node_From_PU: node has null pu"));
  Is_True(PU_Info_proc_sym(result->PU_Info()) == idx,
          ("Get_Node_From_PU: pu has st idx %ld, node has st_idx %ld",
           PU_Info_proc_sym(result->PU_Info()), idx));

  return result;
}


// ======================
// IPA_CALL_GRAPH methods
// ======================
 
//-------------------------------------------------------------------
// get the number of call edges from caller to callee
//-------------------------------------------------------------------
INT32
IPA_CALL_GRAPH::Num_Calls (IPA_NODE* caller, IPA_NODE* callee) const
{
  INT32 count = 0;
  NODE_INDEX callee_idx = callee->Node_Index();
  NODE_INDEX caller_idx = caller->Node_Index();

  for (EDGE_INDEX e = NODE_from(&GRAPH_v_i(_graph, caller_idx));
       e != -1;
       e = EDGE_nfrom(&GRAPH_e_i(_graph, e))) {

    if (EDGE_to(&GRAPH_e_i(_graph, e)) == callee_idx) {
      ++count;
    }
  }
  
  return count;
}

// ----------------------------------------------------------------
// map callsites in the caller to WN nodes
// Precondition: WHIRL tree for the caller has already been read in
// ----------------------------------------------------------------
void 
IPA_CALL_GRAPH::Map_Callsites (IPA_NODE* caller)
{
    // Visited flag is used to avoid building the callsite map more than once
    if (caller->Is_Visited()) {
	return;
    }
    caller->Set_Visited();

  // Check if there are any calls at all
    if (caller->Total_Succ() == 0) {
	return;
    }
  
    WN** callsite_map = (WN**) alloca (caller->Total_Succ() * sizeof(WN*));
    memset(callsite_map, 0, caller->Total_Succ() * sizeof(WN*));
    UINT32 num_calls = 0;

    for (WN_ITER* wni = WN_WALK_TreeIter(caller->Whirl_Tree(FALSE)); 
	 wni != NULL;
	 wni = WN_WALK_TreeNext(wni)) {

	WN* wn = WN_ITER_wn (wni);

	switch (WN_operator(wn)) {

	case OPR_CALL:
	    if (WN_opcode(wn) == OPC_VCALL &&
		WN_Fake_Call_EH_Region (wn, Parent_Map))
		break;
	    // fall through
	case OPR_ICALL:
	case OPR_INTRINSIC_CALL:
	    callsite_map[num_calls++] = wn;
	    break;
	}
    }

    IPA_SUCC_ITER succ_iter (caller);

    for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {
	IPA_EDGE *edge = succ_iter.Current_Edge();
	if (edge) {
            WN *wn = callsite_map[edge->Callsite_Id()];
	    edge->Set_Whirl_Node (wn);
            FmtAssert(edge->Callsite_Id() < caller->Total_Succ(), ("edge callsite_id %d is greater than callsite_map length %d", edge->Callsite_Id() , caller->Total_Succ()));
            if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
              BOOL old_print_mapinfo = IR_dump_map_info;
              IR_dump_map_info = 1; 
              INT32 map_id = edge->Summary_Callsite()->Get_map_id();
              fprintf(TFile, "\n[edge %d, map_id %d] ", edge->Edge_Index(),
                             map_id);
              edge->Print(TFile, this, FALSE);
              if (!wn) {
                 fprintf(TFile, " Set_Whirl_Node(0)!!!\n");
              } 
              else {
                if (map_id != WN_map_id(wn))
                  fprintf(TFile, " !!! callsite map_id %d is not the same as wn map_id %d !!! ",
                          map_id, WN_map_id(wn));
                fdump_wn(TFile, wn);
              }
              IR_dump_map_info = old_print_mapinfo;
            }
	}
    }
}


#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

extern void
Rename_Call_To_Cloned_PU (IPA_NODE *caller, 
                          IPA_NODE *callee,
                          IPA_EDGE *e, 
                          IPA_CALL_GRAPH *cg);

// ------------------------------------------------------------------
// Add given procedure to the array of SUMMARY_PROCEDUREs in the file
// that contains given node and adjust summary header information
// ------------------------------------------------------------------
static INT32
IPA_add_new_procedure (const IPA_NODE* node)
{
  static INT32* max_proc_in_file = NULL;

  if (max_proc_in_file == NULL) {
    UINT32 bytes = IP_File_header.size() * sizeof(INT32);
    max_proc_in_file = (INT32*) MEM_POOL_Alloc(Malloc_Mem_Pool, bytes);
    BZERO (max_proc_in_file, bytes);
  }
  
  INT32& max_proc_size = max_proc_in_file[node->File_Index()];
  IP_FILE_HDR& file_hdr = node->File_Header();
  SUMMARY_FILE_HEADER* summary_header = IP_FILE_HDR_file_header (file_hdr);

  INT32 num_proc;
  SUMMARY_PROCEDURE* old_proc_array = 
    IPA_get_procedure_file_array (file_hdr, num_proc);
  SUMMARY_PROCEDURE* new_proc_array;
  INT32 num_bytes = num_proc * sizeof(SUMMARY_PROCEDURE);
  
  // If max_proc_size is 0, we are extending the array for the first time
  if (max_proc_size == 0) {
    max_proc_size = num_proc * 2;
    new_proc_array = (SUMMARY_PROCEDURE*) 
      MEM_POOL_Alloc (Malloc_Mem_Pool, num_bytes * 2);
    memcpy (new_proc_array, old_proc_array, num_bytes);
    Elf64_Word new_offset = summary_header->Get_proc_offset() +
                            ((char*) new_proc_array - (char*) old_proc_array);
    summary_header->Set_proc_offset (new_offset);
  }
  // Reallocating when the array is extended more than once
  else if (max_proc_size <= num_proc) {
    max_proc_size = num_proc * 2;
    new_proc_array = (SUMMARY_PROCEDURE*) 
      MEM_POOL_Realloc(Malloc_Mem_Pool, old_proc_array, num_bytes,num_bytes*2);
    Elf64_Word new_offset = summary_header->Get_proc_offset() +
                            ((char*) new_proc_array - (char*) old_proc_array);
    summary_header->Set_proc_offset (new_offset);
  }
  else {
    new_proc_array = old_proc_array;
  }
  
  summary_header->Set_proc_size(num_proc + 1);

  return num_proc;
}

//------------------------------------------------------------------
// clone a procedure, 
// If update_cg is TRUE, copy all the from and to edges that contain
// summary information, delete the from and to edges from the clone
//------------------------------------------------------------------
IPA_NODE*
IPA_CALL_GRAPH::Create_Clone (IPA_NODE* node, BOOL update_cg)
{
  IPA_NODE* clone = Add_New_Node (node->Func_ST(),
                                  node->File_Index(),
                                  node->Proc_Info_Index(),
                                  node->Summary_Proc_Index());

  // If necessary, initialize clone<->origin maps
  if (_clone_to_orig_node_map == NULL) {
    _clone_to_orig_node_map =
      CXX_NEW (IPA_CLONE_TO_IPA_NODE_MAP (31, _pool), _pool);
    _orig_node_to_clones_map = 
      CXX_NEW (IPA_NODE_TO_IPA_CLONES_MAP (31, _pool), _pool);
  }

  // Store clone-to-orig mapping
  _clone_to_orig_node_map->Enter (clone, node);

  // Store origin to clone mapping
  IPA_CLONE_ARRAY* clone_array = Clone_Array (node);
  if (clone_array == NULL) {
    clone_array = CXX_NEW (IPA_CLONE_ARRAY (_pool), _pool);
   _orig_node_to_clones_map->Enter (node, clone_array);
  }
  clone_array->AddElement (clone);

  /* In the partial inlining case, we use Create_Clone() to create the
     leftover function, but we don't want to change the call graph edges.
   */
  if (update_cg) {
     // get the number of successor edges
     EDGE_INDEX* out_edges = 
       (EDGE_INDEX*) alloca (Num_Out_Edges(node) * sizeof(EDGE_INDEX));

     // move all successor edges from the original node to the clone
     INT32 out_count = 0;
     IPA_SUCC_ITER succ_iter (this, node);
     for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {    

       IPA_EDGE* edge = succ_iter.Current_Edge();
       IPA_NODE* callee = Callee (edge);

       if (callee == node) { // recursive edge
         Add_Edge (clone, clone, edge); 
       }
       else {
         Add_Edge (clone, callee, edge); 
       }

       // keep track of the edges that need to be deleted
       out_edges[out_count++] = succ_iter.Current_Edge_Index();
     }
  

     // get the number of predecessor edges
     EDGE_INDEX* in_edges = 
       (EDGE_INDEX*) alloca (Num_In_Edges(node) * sizeof(EDGE_INDEX));

     // move all predecessor edges from the original node to the clone
     // also, delete those edges from the original procedure
     INT32 in_count = 0;
     IPA_PRED_ITER pred_iter (this, node);
     for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next()) {

       IPA_EDGE* edge = pred_iter.Current_Edge();
       // NULL edges come from the entry node (Root ?)
       if (edge) { 
         IPA_NODE* caller = Caller (edge);

         // skip recursive edges; they had been handled above
         if (caller != node) {  
   	   Add_Edge (caller, clone, edge);
           // keep track of the edges that need to be deleted 
           // do not delete edges from the root to keeo the graph connected
           if (caller->Node_Index() != Root()) {
   	     in_edges[in_count++] = pred_iter.Current_Edge_Index();
           }
         }
       }
     }
  
     INT32 i;
     // delete the incoming edges
     for (i = 0; i < in_count; ++i) {
       _graph->Delete_Edge (in_edges[i]);
     }

     // delete the outgoing edges 
     for (i = 0 ; i < out_count; ++i) {
       _graph->Delete_Edge (out_edges[i]);
     }
  }

  // Set the clone flag
  clone->Set_Clone();

  // initialize clone's mempool
  MEM_POOL_Initialize (clone->Mem_Pool(), node->Name(), 1); 
  MEM_POOL_Push (clone->Mem_Pool());
  clone->Set_Mempool_Initialized();

  clone->Set_Total_Succ (node->Total_Succ());
  clone->Set_Mod_Ref_Info (CXX_NEW (IPAA_NODE_INFO (*(node->Mod_Ref_Info ())),
				    Malloc_Mem_Pool));
  clone->Set_Cprop_Annot (node->Cprop_Annot());

  // this performs actual tree and symtab cloning and
  // sets all PU_Info related information
  IPO_Clone (node, clone);

  // we need to add a new entry to the SUMMARY_PROCEDURE array
  clone->Set_Summary_Proc_Index (IPA_add_new_procedure (clone));
  *(clone->Summary_Proc()) = *(node->Summary_Proc());

  // return created clone node
  return clone;
}

// -----------------------------------------------------------------
// If the source edge cprop annotation is Top (0) or Bottom (-1),
// set the destination edge annotation to the same value; otherwise,
// create new annotation for the destination edge and do a deep copy
// -----------------------------------------------------------------
static void
Copy_edge_cprop_annot (IPA_EDGE* src_edge, IPA_EDGE* dst_edge)
{
  VALUE_DYN_ARRAY* dst_annot;
  VALUE_DYN_ARRAY* src_annot = src_edge->Cprop_Annot();
  if (src_annot != NULL && src_annot != (void*)-1) {
    dst_annot = 
      CXX_NEW (VALUE_DYN_ARRAY (MEM_local_nz_pool_ptr), MEM_local_nz_pool_ptr);
    *dst_annot = *src_annot;
  }
  else {
    dst_annot = src_annot;
  }
  dst_edge->Set_Cprop_Annot (dst_annot);
}

// ------------------------------------------------------------------
// Create a quasi clone of the callee based on a specific edge
//
// Two major differences between this method and Clone_node are:
//
//  1. In Clone_node all edges incident to the original node
//     are MOVED to the clone. 
//     Here, only the incoming edges equivalent to the parameter
//     edge are redirected to the clone; all outgoing edges are
//     COPIED to the clone, but they are only removed from the
//     original node when that node has no more incoming edges
//     (except possibly for the one coming from the root).
//
//  2. Here, we only clone the IPA_NODE (and its cprop annotations)
//     for the callee and adjust edges appropriately. WHIRL, PU_Info, 
//     and ST will be generated on demand by Quasi_To_Real_Clone.
// ------------------------------------------------------------------
IPA_NODE* 
IPA_CALL_GRAPH::Create_Quasi_Clone (IPA_EDGE* call_edge)
{
  // We must never clone for self-recursive edges 
  Is_True (Caller(call_edge) != Callee(call_edge),
           ("Self-recursive edge in IPA_CALL_GRAPH::Create_Quasi_Clone"));
      
  IPA_NODE* node = Callee (call_edge);
   
  // add new node to the graph, and set its quasi_clone flag
  IPA_NODE* clone = Add_New_Node (node->Func_ST(),
                                  node->File_Index(),
                                  node->Proc_Info_Index(),
                                  node->Summary_Proc_Index());
  clone->Set_Quasi_Clone();
  
  // First time in this routine, initialize clone<->origin maps
  if (_clone_to_orig_node_map == NULL) {
    _clone_to_orig_node_map =
      CXX_NEW (IPA_CLONE_TO_IPA_NODE_MAP (31, _pool), _pool);
    _orig_node_to_clones_map = 
      CXX_NEW (IPA_NODE_TO_IPA_CLONES_MAP (31, _pool), _pool);
  }
  
  // Store clone-to-orig mapping
  _clone_to_orig_node_map->Enter (clone, node);

  // Store origin to clone mapping
  IPA_CLONE_ARRAY* clone_array = _orig_node_to_clones_map->Find (node);
  if (clone_array == NULL) {
    clone_array = CXX_NEW (IPA_CLONE_ARRAY (_pool), _pool);
   _orig_node_to_clones_map->Enter (node, clone_array);
  }
  clone_array->AddElement (clone);

  // set mod_ref and cprop annotations for the clone
  clone->Set_Mod_Ref_Info (CXX_NEW (IPAA_NODE_INFO (*(node->Mod_Ref_Info ())),
				    Malloc_Mem_Pool));
  Init_Cprop_Annotations (clone);

  // get the number of predecessor edges
  EDGE_INDEX* in_edges = 
    (EDGE_INDEX*) alloca (Num_In_Edges(node) * sizeof(EDGE_INDEX));

  INT32 in_count = 0;
  IPA_PRED_ITER pred_iter (this, node);
  for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next()) {

    IPA_EDGE* edge = pred_iter.Current_Edge();

    // calls to the original node that are equivalent
    // to the call_edge are redirected to the to clone 
    if (edge && Edges_Have_Equiv_Cprop_Annots (edge, call_edge)) { 
      
      Add_Edge (Caller(edge), clone, edge);
      in_edges[in_count++] = pred_iter.Current_Edge_Index();
    }
  }

  // get the number of successor edges
  EDGE_INDEX* out_edges = 
    (EDGE_INDEX*) alloca (Num_Out_Edges (node) * sizeof(EDGE_INDEX));

  // move all successor edges from the original node to the clone
  INT32 out_count = 0;
  IPA_SUCC_ITER succ_iter (this, node);
  for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {    

    // calls from the original node are not MOVED, but COPIED to the clone
    IPA_EDGE* edge = succ_iter.Current_Edge();
    if (edge) {
      IPA_EDGE* ecopy = Add_New_Edge (edge->Summary_Callsite(),
                                      clone->Node_Index(),
                                      Callee(edge)->Node_Index());
      Copy_edge_cprop_annot (edge, ecopy);
    }
  }

  // delete the incoming edges
  for (INT32 i = 0; i < in_count; ++i) {
    _graph->Delete_Edge (in_edges[i]);
  }

  // if all in-edges are moved to clones, then we also need to 
  // delete the outgoing edges and connect original node to the root 
  if (Num_In_Edges (node) == 0) {
    _graph->Add_Edge (GRAPH_root(_graph), node->Node_Index(), NULL);
  }

  // return created clone node 
  return clone;
}

//----------------------------------------------------------------------
// Turn a quasi clone into a real one by cloning its PU_Info,
// WHIRL, ST and everything else that a real IPA_NODE needs.
//----------------------------------------------------------------------
void
IPA_CALL_GRAPH::Quasi_To_Real_Clone (IPA_NODE* clone)
{
  Is_True(clone->Is_Quasi_Clone(),
          ("IPA_CALL_GRAPH::Quasi_to_real_clone called on a non-quasi node"));
  
  clone->Clear_Quasi_Clone();     // Reset the IPA_QUASI_CLONE flag 
  clone->Set_Clone();             // Set IPA_CLONE flag
  clone->Clear_Clone_Candidate(); // Disable cloning of clones
  
  // initialize clone's mempool
  MEM_POOL_Initialize (clone->Mem_Pool(), clone->Name(), 1); 
  MEM_POOL_Push (clone->Mem_Pool());
  clone->Set_Mempool_Initialized();

  IPA_NODE* origin = Clone_Origin (clone);

  // set all global-context variables for the original node
  IPA_NODE_CONTEXT context (origin);
  
  // this performs actual tree and symtab cloning and
  // sets all PU_Info related information
  IPO_Clone (origin, clone);

  // we need to add a new entry to the SUMMARY_PROCEDURE array
  clone->Set_Summary_Proc_Index (IPA_add_new_procedure (clone));
  *(clone->Summary_Proc()) = *(origin->Summary_Proc());

  // check for the fake formal ST used with vla-s (661817)
  if (origin->Has_Aliased_Formal()) {
    clone->Set_Aliased_Formal();
  }

  // Before renaming calls to the cloned PU all callers that are 
  // themselves quasi clones must be converted into the real ones.
  // Furthermore, if the caller has spawned some quasi clones, 
  // they also must be converted before calls are renamed.
  IPA_PRED_ITER pred_iter (this, clone);
  for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next()) {
    IPA_EDGE* edge = pred_iter.Current_Edge();
    if (edge) { // NULL edges come from the entry node (graph root)
      IPA_NODE* caller = Caller (edge);
      if (caller->Is_Quasi_Clone()) {
        Quasi_To_Real_Clone (caller);
      } 
      else {
        IPA_CLONE_ARRAY* caller_clones = Clone_Array (caller);
        if (caller_clones) {
          for (UINT32 i = 0; i < caller_clones->Elements(); ++i) {
            if (((*caller_clones)[i])->Is_Quasi_Clone()) {
              Quasi_To_Real_Clone((*caller_clones)[i]);
            }
          }
        }
      }
    }
  }
  
  // Now walk over all callers and rename calls to the clone
  new (&pred_iter) IPA_PRED_ITER (this, clone);
  for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next()) {
    IPA_EDGE* edge = pred_iter.Current_Edge();
    if (edge) {
      Rename_Call_To_Cloned_PU (Caller(edge), clone, edge, this);
    }
  }

}

// --------------------------------------------------------------
// Remove a quasi-clone node and update the edges and annotations
// --------------------------------------------------------------
void
IPA_CALL_GRAPH::Remove_Quasi_Clone (IPA_NODE* clone)
{
  // If the clone has calls to other quasi-clones, 
  // those quasi-clone callees must be removed first
  IPA_SUCC_ITER succ_iter (this, clone);
  for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) { 
    IPA_NODE* callee = Callee (succ_iter.Current_Edge());
    if (callee->Is_Quasi_Clone()) {
      Remove_Quasi_Clone (callee);
    }
  }
  
  // Redirect all incoming edges back to the original node
  IPA_NODE* origin = Clone_Origin (clone);

  EDGE_INDEX* in_edges = 
    (EDGE_INDEX*) alloca (Num_In_Edges(clone) * sizeof(EDGE_INDEX));

  INT32 in_count = 0;
  IPA_PRED_ITER pred_iter (this, clone);
  for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next()) {
    IPA_EDGE* edge = pred_iter.Current_Edge();
    Add_Edge (Caller(edge), origin, edge);
    in_edges[in_count++] = pred_iter.Current_Edge_Index();
  }

  // delete the incoming edges
  for (INT32 i = 0; i < in_count; ++i) {
    _graph->Delete_Edge (in_edges[i]);
  }

  // Union the quasi-clone annotations with those of the original node
  Union_Quasi_Clone_Cprop_Annot (origin, clone);
  
  // Remove the clone node from the graph
  (void) _graph->Delete_Node (clone->Node_Index());
}


// ------------------------------------------------------------------
// Update following info for IPA_NODE after it has been preoptimized:
//   - outgoing edges (call sites may be changed)
//   - SUMMARY_PROCEDURE
//   - WHIRL node
//   - enter IPA_NODE to IPL_SUMMARY_PTRS mapping into hash table
// ------------------------------------------------------------------
void 
IPA_CALL_GRAPH::Update_Node_After_Preopt (IPA_NODE* node,
                                          WN* opt_wn,
                                          SUMMARY_CALLSITE* callsite_array,
                                          IPL_SUMMARY_PTRS* summary_ptrs)
{
  // Delete indirect and opaque call edges
  node->Icall_List().clear();
  node->Ocall_List().clear();

  // Delete direct call edges
  EDGE_INDEX* out_edges = 
    (EDGE_INDEX*) alloca (Num_Out_Edges(node) * sizeof(EDGE_INDEX));

  INT32 out_count = 0;
  IPA_SUCC_ITER succ_iter (this, node);
  for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {    
    out_edges[out_count++] = succ_iter.Current_Edge_Index();
  }

  for (INT32 i = 0; i < out_count; ++i) {
    _graph->Delete_Edge (out_edges[i]);
  }

  // Node may have been marked as visited in IPA_CALL_GRAPH::Map_Callsites
  // if some of its callees had been cloned. Since WHIRL is now changed,
  // Map_Callsits should be called again
  node->Clear_Visited();

  // Set the new WHIRL tree, summary procedure, successor count, pu_size
  node->Set_Whirl_Tree (opt_wn);
  node->Set_Preoptimized();
  WN_Parentize (opt_wn, node->Parent_Map(), node->Map_Table());

  SUMMARY_PROCEDURE* summary_proc = node->Summary_Proc();
  UINT16 callsite_count = summary_proc->Get_callsite_count();
  node->Set_Total_Succ (callsite_count);
  node->Set_PU_Size (PU_SIZE (summary_proc->Get_bb_count(),
                              summary_proc->Get_stmt_count(),
                              summary_proc->Get_call_count()));

  SUMMARY_SYMBOL* symbol_array = IPA_get_symbol_array (node);
  
  // Iterate over regenerated call sites and update edges
  for (UINT16 j = 0; j < callsite_count; ++j) {

    // indirect calls
    if (callsite_array[j].Is_func_ptr()) {
        if (!callsite_array[j].Is_virtual_call()) 
            append_icall_list (node->Icall_List(), &callsite_array[j]);
    }
    else if (!callsite_array[j].Is_intrinsic()) {
      // direct calls
      INT32 callee_sym_index = callsite_array[j].Get_symbol_index();
      ST* callee_st = ST_ptr(symbol_array[callee_sym_index].St_idx());

      // if it is a weak symbol, find the corresponding strong
      while (ST_is_weak_symbol (callee_st) &&
             ST_st_idx (callee_st) != ST_base_idx (callee_st)) {
        callee_st = ST_base (callee_st);
      }
      Clear_ST_is_not_used (callee_st);

      // If the callee is not in a WHIRL IR file, its index will 
      // be invalid. In that case we do not add the edge, but we
      // add the callsite to a special list of opaque calls.
      NODE_INDEX callee_idx = AUX_PU_node(Aux_Pu_Table[ST_pu(callee_st)]);
      if (callee_idx != INVALID_NODE_INDEX) {
        IPA_EDGE* edge = Add_New_Edge(&callsite_array[j],
                                      node->Node_Index(), 
                                      callee_idx);
        IPA_NODE* callee = _graph->Node_User(callee_idx);
        if (callee->Has_Propagated_Const()) {
          edge->Set_Propagated_Const();
        }
      }
      else {
        append_icall_list (node->Ocall_List(), &callsite_array[j]);
      }
    }
  }

  // If not done before, initialize node to new summary info map
  // and then enter summary and array summary pointers into it
  if (_preopt_node_to_new_summary_map == 0) {
    _preopt_node_to_new_summary_map = 
      CXX_NEW (IPA_NODE_TO_IPL_SUMMARY_MAP (32, _pool), _pool);
  }
  _preopt_node_to_new_summary_map->Enter (node, summary_ptrs);

}

#endif // _STANDALONE_INLINER


char*
IPA_Node_Name(IPA_NODE* node)
{
  if (!node->Is_Quasi_Clone()) {
    return node->Name();
  }
  
  IPA_NODE* origin = IPA_Call_Graph->Clone_Origin(node);
  IPA_CLONE_ARRAY* clone_array = IPA_Call_Graph->Clone_Array(origin);
  UINT32 clone_num;
  for (clone_num = 0; clone_num < clone_array->Elements(); ++clone_num) {
    if ((*clone_array)[clone_num] == node) {
      break;
    }
  }

  size_t size = strlen(origin->Name())+15;
  char* name = TYPE_MEM_POOL_ALLOC_N(char, Malloc_Mem_Pool, size);
  sprintf(name, "%s..clone..%u", origin->Name(), clone_num);
  
  return name;
}

  
// -------------------------------------------
// Print all nodes and edges in the call graph
// -------------------------------------------
void 
IPA_CALL_GRAPH::Print (FILE* fp)
{
  Print(fp, PREORDER);
}

extern "C" void 
print_ipa_cg(IPA_CALL_GRAPH *cg)
{
   cg->Print(stdout, PREORDER);
}

extern "C" void 
print_ipa_cg_v(IPA_CALL_GRAPH *cg)
{
   cg->Print_vobose(stdout, PREORDER, FALSE);
}

void 
IPA_CALL_GRAPH::Print_vobose (FILE* fp)
{
  Print_vobose(fp, PREORDER, FALSE);
}
UINT32
EFFECTIVE_WEIGHT (const IPA_NODE* node)  {
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    if (IPA_Use_Effective_Size && node->Has_frequency ()) {
	SUMMARY_FEEDBACK *fb = node->Get_feedback ();
	return PU_Weight (fb->Get_effective_bb_count (),
			  fb->Get_effective_stmt_count (),
			  node->PU_Size().Call_Count ());
    } else
#endif // _STANDALONE_INLINER
	return node->Weight ();
}

void 
IPA_CALL_GRAPH::Print_vobose (FILE* fp, TRAVERSAL_ORDER order, BOOL do_callsite_map)
{
  char YN;
  float hotness=-1.0;
  float hotness2=-1.0;
  float density = -1.0;
  vector<IPA_EDGE_INDEX> callsite_list;
#ifdef KEY
// An effort to at least partially fix the problem that we use the call-graph
// global variable everywhere. Similar changes follow.
  IPA_NODE_ITER cg_iter(this, order);
  AUX_IPA_EDGE<INT32> cost_vector (this, _pool);
#else
  IPA_NODE_ITER cg_iter(IPA_Call_Graph, order);
  AUX_IPA_EDGE<INT32> cost_vector (IPA_Call_Graph, _pool);
#endif

fprintf(fp, "Finally, Total_Prog_Size = %d\n", Total_Prog_Size);
fprintf(fp, SBar);
fprintf(fp, "Reason0: callee is skipped\n");			
fprintf(fp, "Reason1: edge is skipped\n");				
fprintf(fp, "Reason2: call deleted by DCE\n");			
fprintf(fp, "Reason3: caller is a nested procedure\n");
fprintf(fp, "Reason4: callee has nested procedure(s) so ignore user MUST inline request\n");
fprintf(fp, "Reason5: callee has nested procedure(s)\n");
fprintf(fp, "Reason6: callee is recursive\n");
fprintf(fp, "Reason7: callee is varargs\n");
fprintf(fp, "Reason8: function with alternate entry point\n");
fprintf(fp, "Reason9: number of parameters mismatched\n"); 
fprintf(fp, "Reason10: callee has pragmas which are associated with formals\n"); 
fprintf(fp, "Reason11: callee has flag that suggested that it should be MPed\n"); 
fprintf(fp, "Reason12: callee has parallel pragmas that suggest turning off inlining\n"); 
fprintf(fp, "Reason13: callee has VLAs and caller has parallel_pragma\n"); 
fprintf(fp, "Reason14: callee has PDO pramgas and caller has parallel_pragma\n");  
fprintf(fp, "Reason15: callsite pragma requested not to inline\n"); 
fprintf(fp, "Reason16: exception handling function\n"); 
fprintf(fp, "Reason17: exception handling code with pstatics\n");
fprintf(fp, "Reason18: depth in call graph exceeds specified maximum\n");
fprintf(fp, "Reason19: user requested not to inline\n"); 
fprintf(fp, "Reason20: function has local fstatics and is set preemptible\n"); 
fprintf(fp, "Reason21: function is preemptible and has not been set to mustinline\n"); 
fprintf(fp, "Reason22: incompatible return types\n"); 
fprintf(fp, "Reason23: incompatible parameter types\n"); 
fprintf(fp, "Reason24: not inlining across language boundaries\n"); 
fprintf(fp, "Reason25: not inlining across language boundaries\n"); 

fprintf(fp, "Reason26: $combined_weight exceeds -IPA:plimit=%d\n", IPA_PU_Limit); 
fprintf(fp, "Reason27: $hotness < -IPA:min_hotness %d\n", IPA_Min_Hotness); 
fprintf(fp, "Reason28: $callee_weight > -IPA:callee_limit=%d\n", IPA_Small_Callee_Limit);
fprintf(fp, "Reason29: $callee_weight > -INLINE:aggressive=off callee limit %d\n", IPA_PU_Minimum_Size + (IPA_PU_Minimum_Size / 2));
fprintf(fp, "Reason30: small, but $combined_weight exceeds hard function size limit %d\n", IPA_PU_Hard_Limit);
fprintf(fp, "Reason31: Olimit $Get_combined_olimit(caller->PU_Size(), callee->PU_Size(), callee) exceeds -OPT:Olimit= %d\n", Olimit);
fprintf(fp, "Reason32: Edge is never invoked\n");
fprintf(fp, "Reason33: Density is too high (infrequent called but contains hot loops) > %d\n",IPA_Max_Density);
#ifdef KEY
fprintf(fp, "Reason34: optimization options are different for caller and callee\n");
fprintf(fp, "Reason35: Trying to do pure-call-optimization for this callsite\n");
fprintf(fp, "Reason36: not inlining C++ with exceptions into non-C++\n");
fprintf(fp, "Reason37: formal parameter is a loop index\n");
fprintf(fp, "Reason38: not inlining nested functions\n");
fprintf(fp, "Reason39: not inlining non-tiny noreturn functions\n");
fprintf(fp, "Reason40: not inlining __builtin_apply_args functions\n");
#endif
fprintf(fp, SBar);
  
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) //all nodes
  {
    IPA_NODE* node = cg_iter.Current();
    if (node) {
	  IPA_NODE_CONTEXT context (node);
          if (do_callsite_map) {
#ifdef KEY
	    Map_Callsites (node);
#else
	    IPA_Call_Graph->Map_Callsites (node);
#endif
          }

	  float caller_freq=-1.0;
	  float cycle = -1.0;
	  UINT16 wn_count = 0;
	  if(node->Has_frequency ()) {
#ifdef KEY
	    caller_freq = (node->Get_frequency()).Value();
            cycle = node->Get_cycle_count_2().Value();
#else
	    caller_freq = (node->Get_frequency())._value;
            cycle = node->Get_cycle_count_2()._value;
#endif
            wn_count=node->Get_wn_count();
	  }
	  
          fprintf(fp, "PU   %-40s Weight=%-5d Freq=%-10.1f WNs=%-7d Cc=%-15.1f\n", IPA_Node_Name(node), node->Weight(), caller_freq, wn_count, cycle);

          BOOL seen_callee = FALSE;
          callsite_list.clear ();
#ifdef KEY
          Get_Sorted_Callsite_List(node, this, cost_vector, callsite_list);
#else
          Get_Sorted_Callsite_List(node, IPA_Call_Graph, cost_vector, callsite_list);
#endif
          vector<IPA_EDGE_INDEX>::const_iterator last = callsite_list.end ();
	  for(vector<IPA_EDGE_INDEX>::iterator first = callsite_list.begin (); first != last; ++first) {
#ifdef KEY
              IPA_EDGE* tmp_edge = Edge (*first) ; 
#else
              IPA_EDGE* tmp_edge = IPA_Call_Graph->Edge (*first) ; 
#endif
              IPA_EDGE_INDEX idx = tmp_edge->Array_Index ();
              INT32 callsite_linenum = 0;
              WN* call_wn = tmp_edge->Whirl_Node();
              USRCPOS callsite_srcpos;

              if (!do_callsite_map || call_wn == NULL) {
                  callsite_linenum = 0;	
              }else{
                  USRCPOS_srcpos(callsite_srcpos) = WN_Get_Linenum (call_wn);
                  callsite_linenum = USRCPOS_linenum(callsite_srcpos);
              }


          if (IPA_NODE* callee = Callee(tmp_edge)) {
              if(IPA_Enable_Inline && tmp_edge->Has_Inline_Attrib () && !callee->Has_Noinline_Attrib()) {
                  YN= 'Y';
              }else{
                  YN= 'N';
          }

          SUMMARY_FEEDBACK *fb = callee->Get_feedback();
          INT e_bb_cnt, e_stmt_cnt;
          e_bb_cnt= e_stmt_cnt = (unsigned) -1;

          if(callee->Has_frequency ()) {
              e_bb_cnt = (fb==NULL)? (unsigned) -1 : fb->Get_effective_bb_count ();
              e_stmt_cnt = (fb==NULL)? (unsigned) -1 : fb->Get_effective_stmt_count ();
          }
		  
          if (!seen_callee) {
              fprintf(fp, "CALLS: \n");
              seen_callee = TRUE;
          }

          char why[50];
          float callee_freq,edge_freq,callee_cycle_count;

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
    INT32 cost = callee->Weight ();
#else
    INT32 cost = EFFECTIVE_WEIGHT (callee); 
#endif
	      if(callee->Has_frequency ()) {
#ifdef KEY
                  callee_freq = (callee->Get_frequency()).Value();
                  callee_cycle_count = (callee->Get_cycle_count()).Value();
#else
                  callee_freq = (callee->Get_frequency())._value;
                  callee_cycle_count = callee->Get_cycle_count()._value;
#endif
              }else{
                  callee_freq = -1.0;
                  callee_cycle_count = -1.0;
              }
		  
              if(tmp_edge->Has_frequency()) {
#ifdef KEY
                  edge_freq = (tmp_edge->Get_frequency()).Value();
#else
                  edge_freq = (tmp_edge->Get_frequency())._value;
#endif
              }else{
                  edge_freq = -1.0;
              }


              if(tmp_edge->reason_id() > 25){
                  sprintf(why, "%d,%f", tmp_edge->reason_id(),tmp_edge->reason_data());
              }else{
                  sprintf(why, "%d", tmp_edge->reason_id());
              }

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
           if (tmp_edge->Has_frequency () && callee->Has_frequency () &&
               tmp_edge->Get_frequency().Known() && callee->Get_frequency().Known()) {
               hotness = compute_hotness (tmp_edge, callee, EFFECTIVE_WEIGHT(callee));
               FB_FREQ cycle_ratio =
                   (tmp_edge->Get_frequency () / callee->Get_frequency () *
                    callee->Get_cycle_count_2 ()) / Total_cycle_count_2;

               float size_ratio = (float) (callee->Get_wn_count()) / (float) Orig_Prog_WN_Count;
#ifdef KEY
               hotness2 = (cycle_ratio.Value() / size_ratio * 100.0);
#else
               hotness2 = (cycle_ratio._value / size_ratio * 100.0);
#endif /* KEY */
               density = (float) callee->Get_cycle_count().Value() / ((float)EFFECTIVE_WEIGHT (callee) * (float)callee->Get_frequency().Value());
           }else if(callee->Summary_Proc()->Is_Never_Invoked()) {
               hotness = -1.0;
               hotness2 = -1.0;
               density = -1.0;
           }
#endif


           fprintf(fp, "%c %-6.1f %-6.1f %s-->%-20s(l=%-5d cid=%-5d eid=%-5d ef=%-10.1f cf=%-10.1f ew=%-5d den=%-5.1f Cc=%-12.1f)[?%s]\n", 
						  YN, 
						  hotness,
						  hotness2,
						  IPA_Node_Name(node),
						  IPA_Node_Name(callee),
                                                  callsite_linenum,
						  tmp_edge->Callsite_Id(), 
						  tmp_edge->Edge_Index(), 
						  edge_freq,//(tmp_edge->Get_frequency())._value, 
						  callee_freq, //(callee->Get_frequency())._value,   
						  EFFECTIVE_WEIGHT (callee), 
						  density,
						  callee_cycle_count,//callee->Get_cycle_count()._value,
						  why
						  ); 
        }//if callee is ok
      }// for all callee (edge)

      if (!seen_callee) {
          fprintf(fp, "HAS NO CALLS\n");
      }
      fprintf(fp, "\n");
    }//if caller is ok
  }//for all nodes
  for ( cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next() ){
       IPA_NODE *node=cg_iter.Current();
       if ( node ){
               node->Clear_Visited();
       }
  }
}//Print-vobose()

void
IPA_NODE::Print(FILE *fp, IPA_CALL_GRAPH *cg)
{
   IPA_NODE *node = this;
   fprintf(fp, "PU    %s (freq = %.1f) \n", IPA_Node_Name(node),
           (node->Get_frequency()).Value());
   IPA_SUCC_ITER succ_iter(node);
   for ( succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next() ) {
       IPA_EDGE *edge = succ_iter.Current_Edge();
       edge->Print(fp,cg,false);
   }
}

// ---------------------------------------------
// Print all node indices in the specified order
// ---------------------------------------------
void 
IPA_CALL_GRAPH::Print (FILE* fp, TRAVERSAL_ORDER order)
{
#ifdef KEY
  IPA_NODE_ITER cg_iter(this, order);
#else
  IPA_NODE_ITER cg_iter(IPA_Call_Graph, order);
#endif
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {

    IPA_NODE* node = cg_iter.Current();
    if (node) {

//pengzhao
#ifdef KEY
      fprintf(fp, "PU    %s (freq = %.1f) \n", IPA_Node_Name(node),
	      (node->Get_frequency()).Value());
#else
      fprintf(fp, "PU    %s (freq = %.1f) \n", IPA_Node_Name(node),
	      (node->Get_frequency())._value);
#endif
      BOOL seen_callee = FALSE;

      IPA_SUCC_ITER succ_iter(node);
      for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {
        if (IPA_NODE* callee = Callee(succ_iter.Current_Edge())) {
          if (!seen_callee) {
            fprintf(fp, "CALLS: \n");
            seen_callee = TRUE;
          }
//pengzhao
//          fprintf(fp, "\t%s\n", IPA_Node_Name(callee));
#ifdef KEY
	    fprintf(fp, "    %s(%f)->%s(cid=%-5d ef=%.1f, cf=%.1f)\n",
		    IPA_Node_Name(node),
		    (node->Get_frequency()).Value(),
		    IPA_Node_Name(callee),
		    succ_iter.Current_Edge()->Callsite_Id(),
		    (succ_iter.Current_Edge()->Get_frequency()).Value(),
		    (callee->Get_frequency()).Value());
#else
	    fprintf(fp, "    %s(%f)->%s(ef= %.1f,cf=%.1f)\n",
		    IPA_Node_Name(node),
		    (node->Get_frequency())._value,
		    IPA_Node_Name(callee),
		    (succ_iter.Current_Edge()->Get_frequency())._value,
		    (callee->Get_frequency())._value);
#endif /* KEY */
        }
      }

      if (!seen_callee) {
        fprintf(fp, "HAS NO CALLS\n");
      }
      fprintf(fp, "\n");
    }
  }
}


#ifdef _LIGHTWEIGHT_INLINER
void 
IPA_NODE::Free_inlined_list()
{
  const INLINED_BODY_LIST& inlined_list = Inlined_list ();

  for (INLINED_BODY_LIST::const_iterator iter = inlined_list.begin ();
	iter != inlined_list.end (); ++iter) {
      MEM_POOL_FREE(Malloc_Mem_Pool, *iter);
  }
}

BOOL
Is_Node_Inlinable_In_Call_Graph(ST_IDX idx)
{
   IPA_NODE_ITER cg_iter (PREORDER, Malloc_Mem_Pool);
   for (cg_iter.First (); !cg_iter.Is_Empty(); cg_iter.Next ()) {

      IPA_NODE* node = cg_iter.Current ();

      if (node && (ST_st_idx(node->Func_ST()) == idx) && node->Has_Inline_Attrib())
	  return TRUE;
  }
  return FALSE;
}

BOOL
Pred_Is_Root(const IPA_NODE* node) 
{
    IPA_PRED_ITER pred_iter (node->Node_Index());

    for (pred_iter.First (); !pred_iter.Is_Empty (); pred_iter.Next ()) {
        IPA_EDGE *edge = pred_iter.Current_Edge ();

        if (edge) {
        }
	else
	    return TRUE;  	// NULL edge connected to ROOT
    }
    return FALSE;
}

#endif // _LIGHTWEIGHT_INLINER

