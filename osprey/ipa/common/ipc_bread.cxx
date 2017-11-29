/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>
#include <errno.h>
#include <sys/types.h>

#include "defs.h"
#include "wn.h"
#include "pu_info.h"
#include "erglob.h"			// error codes.
#include "dwarf_DST.h"			// for DST structures
#include "ir_bread.h"			// low-level WHIRL I/O routines
#include "xstats.h"                     // Initialize_PU_Stats

#include "ipc_file.h"			// for IP_FILE_HDR
#include "ipa_summary.h"		// for IPA_get_file_header
#include "ipc_bread.h"			// for IP_READ_file_info
#include "ipc_symtab_merge.h"		// for idx_map
#include "ipl_summary.h"		// for SUMMARY_PROCEDURE, etc.

#if defined(BACK_END) || defined(IR_TOOLS)
#include "wssa_mgr.h"
#endif

BOOL IPA_Has_Feedback = FALSE;      /* set if ANY input file contains
                                       feedback information */

// temporary placeholder until feedback is fixed
// extern BOOL FB_PU_Has_Feedback;

static const int ERROR_VALUE = -1;

// Remap a PU_Info tree: map any global symbol table indices into 
//  merged global symbol table indices.  Used by IP_READ_pu_infos.
void IP_READ_remap_pu_infos(PU_Info* base, const IPC_GLOBAL_IDX_MAP* idx_map)
{
  for ( ; base != NULL ; base = PU_Info_next(base)) {

    // The only information to be fixed up is the PU's ST_IDX.
    PU_Info_proc_sym(base) = idx_map->st[PU_Info_proc_sym(base)];

    if (PU_Info_child(base) != NULL)
      IP_READ_remap_pu_infos(PU_Info_child(base), idx_map);
  }
}


// Recursively traverse the PU_Infos in depth-first order to match them
// up with the summaries.  Assume that the PU list and the summary
// section use the same order.  Used by IP_READ_pu_infos.


INT
IPA_process_PUs (IP_FILE_HDR &s, const SUMMARY_PROCEDURE *proc_array,
                 PU_Info *pu_tree, INT proc_idx)
{
  for (PU_Info *pu = pu_tree; pu; pu = PU_Info_next(pu)) {
    IP_PROC_INFO *proci = IP_FILE_HDR_proc_info(s) + proc_idx;


    /* skip over alternate entry points */
    while (proc_array[proc_idx].Is_alt_entry()) {
      Set_IP_PROC_INFO_pu_info (*proci, NULL);
      Set_IP_PROC_INFO_state (*proci, IPA_ORIG);
      ++proc_idx;
      ++proci;
    }
        
    Set_IP_PROC_INFO_pu_info (*proci, pu);
    Set_IP_PROC_INFO_state (*proci, IPA_ORIG);

    ++proc_idx;

    if (PU_Info_child(pu)) {
      proc_idx = IPA_process_PUs (s, proc_array, PU_Info_child(pu),
                                  proc_idx);
      Set_IP_FILE_HDR_has_nested_pu(s);
    }
  }
  return proc_idx;
} // IPA_process_PUs

// Read the tree of pu_infos, and then set up the array of
// IP_PROC_INFOs.  The array contains essentially the same information
// as the tree; the main difference is the different data structure.

void
IP_READ_pu_infos (IP_FILE_HDR& s)
{
  if (IP_FILE_HDR_pu_list(s) == NULL) {
    void *fhandle = (void *)IP_FILE_HDR_input_map_addr(s);

    Temporary_Error_Phase ephase("Reading WHIRL file");

    // Read global information, if necessary.
    IP_READ_file_info(s);

    // Read the tree of PU_INFOs.
    PU_Info* pu_tree = WN_get_PU_Infos(fhandle, NULL);
    if (pu_tree == (PU_Info *)-1) {
      ErrMsg ( EC_IR_Scn_Read, "PU headers", IP_FILE_HDR_file_name(s));
    }

    IP_READ_remap_pu_infos(pu_tree, IP_FILE_HDR_idx_maps(s));

    Set_IP_FILE_HDR_pu_list(s, pu_tree);

    // Setup the summary information.

    if (IP_FILE_HDR_summary (s) == 0) {
      char *base = (char*)
        WN_get_section_base (IP_FILE_HDR_input_map_addr (s),
                             WT_IPA_SUMMARY);
      Set_IP_FILE_HDR_summary(s, base);
      Set_IP_FILE_HDR_file_header (s, IPA_get_file_header (base));
    }

    SUMMARY_FILE_HEADER *file_header = IP_FILE_HDR_file_header (s);
    UINT vv=file_header->Get_version_number();
    UINT vv2=IPA_SUMMARY_REVISION;

    if (file_header &&
        file_header->Get_version_number() != IPA_SUMMARY_REVISION) {
      ErrMsg(EC_IR_Revision, "summary info", IP_FILE_HDR_file_name(s));
    }

    // Setup the array of IP_PROC_INFOs.

    INT32 count;
    SUMMARY_PROCEDURE *proc_array = IPA_get_procedure_file_array(s, count);

    Set_IP_FILE_HDR_num_procs (s, count);

    INT size_proc = 0;
    if (count != 0) {
        // reserve room for inlined procs to minimize reallocating.
        size_proc = count < 32 ? count + 8 : count + count / 4;
    }

    Set_IP_FILE_HDR_proc_info (s, (IP_PROC_INFO*)
                               MEM_POOL_Alloc (Malloc_Mem_Pool,
                                               (sizeof(IP_PROC_INFO)*size_proc)));
    
    Set_IP_FILE_HDR_max_size (s, size_proc); 

    if (count != 0) {
      INT proc_idx = IPA_process_PUs (s, proc_array,
                                      IP_FILE_HDR_pu_list(s), 0); 
      while ((proc_idx < count) && proc_array[proc_idx].Is_alt_entry()) {
        IP_PROC_INFO &proci = IP_FILE_HDR_proc_info(s)[proc_idx];
        Set_IP_PROC_INFO_pu_info (proci, NULL);
        Set_IP_PROC_INFO_state (proci, IPA_ORIG);
        ++proc_idx;
      }
      FmtAssert((proc_idx == count),
                ("procedure headers do not match summary information"));
    }
  }
}

namespace {                     // This unnamed namespace contains DST-related
                                // routines used by IP_READ_file_info.

// This must be global because the DST traversal routine takes a 
// function pointer, not a function object with local state.
const IPC_GLOBAL_IDX_MAP* dst_idx_map;

void remap_ST_index(DST_ASSOC_INFO& idx) {
  if (DST_ASSOC_INFO_st_level(idx) == GLOBAL_SYMTAB) {
    const ST_IDX new_idx = dst_idx_map->st[DST_ASSOC_INFO_st_idx(idx)];
    DST_ASSOC_INFO_st_idx(idx) = new_idx;
  }
}

INT32 remap_dst_entry(INT32 /* dummy */, DST_DW_tag tag, DST_flag flag,
                      DST_ATTR_IDX attr, DST_INFO_IDX info_idx) 
{
  Is_True(!DST_IS_NULL(attr) && !DST_IS_NULL(info_idx),
          ("remap_dst_entry: invalid DST index"));
  Is_True(DST_ARE_EQUAL(DST_INFO_attributes(DST_INFO_IDX_TO_PTR(info_idx)), attr),
          ("remap_dst_entry: DST_ATTR doesn't match DST_INFO"));
  Is_True(DST_INFO_tag(DST_INFO_IDX_TO_PTR(info_idx)) == tag,
          ("remap_dst_entry: tag doesn't match DST_INFO"));
  Is_True(DST_INFO_flag(DST_INFO_IDX_TO_PTR(info_idx)) == flag,
          ("remap_dst_entry: flag doesn't match DST_INFO"));

  switch(tag) {
  case DW_TAG_subprogram:
    {
      DST_subprogram* sub = DST_ATTR_IDX_TO_PTR(attr, DST_subprogram);
      if (DST_IS_memdef(flag)) {
        remap_ST_index(DST_SUBPROGRAM_memdef_st(sub));
      }
      else if (DST_IS_declaration(flag)) {
        // Do nothing, no ST index here.
      }
      else {                      // It's a subprogram definition.
        remap_ST_index(DST_SUBPROGRAM_def_st(sub));
      }
    }
    break;

  case DW_TAG_entry_point:
    {
      DST_entry_point* entry = DST_ATTR_IDX_TO_PTR(attr, DST_entry_point);
      remap_ST_index(DST_ENTRY_POINT_st(entry));
    }
    break;
    
  case DW_TAG_common_block:
    {
      DST_common_block* block = DST_ATTR_IDX_TO_PTR(attr, DST_common_block);
      remap_ST_index(DST_COMMON_BLOCK_st(block));
    }
    break;

  case DW_TAG_variable:
    {
      DST_variable* var = DST_ATTR_IDX_TO_PTR(attr, DST_variable);

      if (DST_IS_declaration(flag)) {
        // Do nothing, no ST index here.
      }
      else if (DST_IS_const(flag)) {
        // Do nothing, no ST index here.
      }
      else if (DST_IS_comm(flag)) {
        remap_ST_index(DST_VARIABLE_comm_st(var));
      }
      else if (DST_IS_memdef(flag)) {
        remap_ST_index(DST_VARIABLE_memdef_st(var));
      }
      else {                      // It's a variable definition.
        remap_ST_index(DST_VARIABLE_def_st(var));
      }
    }
    break;

  case DW_TAG_formal_parameter:
    {
      DST_formal_parameter* param =
        DST_ATTR_IDX_TO_PTR(attr, DST_formal_parameter);
      remap_ST_index(DST_FORMAL_PARAMETER_st(param));
    }
    break;

  // NOTE: This is not an exhaustive list.  There are several dozen 
  // kinds of DST entires.  This is supposed to be an exhaustive list
  // of the DST entries that have ST indices.

  default:
    break;
  }

  return 0;
}

// Change all symtab indices in the current DST so that they refer to the
// merged global symbol table.
void remap_dst(const IP_FILE_HDR& s) {
  dst_idx_map = IP_FILE_HDR_idx_maps(s);
  Is_True(dst_idx_map != 0,
          ("Index map not initialized for file header %s",
           IP_FILE_HDR_file_name(s)));

  Is_True(Current_DST == IP_FILE_HDR_dst(s),
          ("File header %s: DST mismatch", IP_FILE_HDR_file_name(s)));

  DST_IDX compile_unit = DST_get_compile_unit();
  Is_True(!DST_IS_NULL(compile_unit),
          ("File header %s: DST has no DST_COMPILE_UNIT entry",
           IP_FILE_HDR_file_name(s)));

  DST_preorder_visit(compile_unit,
                     0 /* dummy value */,
                     remap_dst_entry);
}

} // Close unnamed namespace.

void
IP_READ_file_info (IP_FILE_HDR& s)
{
  // There's very little to do.  We already have a merged global symbol
  //  table, and the FILE_INFO struct is read in when the IP_FILE_HDR
  //  is first set up.

  void *fhandle = IP_FILE_HDR_input_map_addr(s);
  const char* file_name = IP_FILE_HDR_file_name(s);

  Temporary_Error_Phase ephase("Reading WHIRL file"); 

  if (IP_FILE_HDR_dst(s)) {     // We have already read in the DST.
    Current_DST = IP_FILE_HDR_dst(s);
  }
  else {                        // Read the DST.
    Current_DST = 0;
    if (WN_get_dst(fhandle) == -1) {
        ErrMsg ( EC_IR_Scn_Read, "dst", IP_FILE_HDR_file_name(s));
    }
    Set_IP_FILE_HDR_dst(s, Current_DST);

#ifndef _LIGHTWEIGHT_INLINER
    IP_build_global_filelists(s, IP_FILE_HDR_incl_map(s), IP_FILE_HDR_fn_map(s));
#endif
    remap_dst(s);    
  }
}


template <class T>
inline T* convert_offset(T* p, char* base) {
  return reinterpret_cast<T*>(base + reinterpret_cast<INTPTR>(p));
}

#ifdef KEY
// This function is similar to the versions at ipl_summarize_template.h
// and xstats.cxx. This function is called when IPA reads in PUs. It
// must only access wn, and hence cannot have any context info.
// Called to set the global variables keeping track of pusize stats.
// IPA/inlining heuristics does not use these variables, but gets
// stats from summary info.
static void
Count_WN (WN * wn, INT32& bbs, INT32& stmts, INT32& calls)
{
    OPERATOR opr = WN_operator (wn);
    TYPE_ID rtype = WN_rtype (wn);

    /* count nscf stmts as bbs, not stmts */
    if (OPERATOR_is_non_scf(opr)) {
        switch (opr) {
          case OPR_LABEL:
            if (WN_Label_Is_Not_Used (wn))
              return;
            else break;
          case OPR_RETURN:
          case OPR_RETURN_VAL:
            return;
        }
        ++bbs;
    } else if (OPERATOR_is_stmt(opr)) {
        if (OPERATOR_is_call(opr)) {
            ++bbs;
            ++calls;
        } else if (opr == OPR_IO) {
            /* TODO:  ideally would look at values of IO_ITEMs,
             * but then have to pass more than opcode. */
            ++bbs;
            ++calls;
        } else if (! OPERATOR_is_not_executable(opr)) {
            ++stmts;
            if (MTYPE_is_complex(rtype) && OPERATOR_is_store(opr)) {
                ++stmts;
            }
        }
    } else if (OPERATOR_is_scf(opr)) {
        if (opr != OPR_BLOCK && opr != OPR_FUNC_ENTRY) {
            /* blocks are counted by parent node */
            ++bbs;
        }
        /* if may create two blocks if else present,
         * but can't tell just from opcode */
    } else if ((rtype == MTYPE_FQ || rtype == MTYPE_CQ) &&
               OPERATOR_is_expression(opr) &&
               !OPERATOR_is_load(opr) &&
               !OPERATOR_is_leaf(opr) ) {
        /* quad operators get turned into calls */
        ++bbs;
        ++calls;
    } else if (opr == OPR_CAND || opr == OPR_CIOR) {
        /* these may get expanded to if-then-else sequences,
         * or they may be optimized to logical expressions.
         * use the halfway average of 1 bb */
        ++bbs;
    }
}
#endif // KEY

// The following two functions are almost identical to the corresponding
// functions in common/com/ir_bread.cxx.  They are duplicated because, in
// ipa, we need to remap ST_IDX's while walking the tree.

INT
IP_READ_fix_tree (PU_Info* pu, WN *node, char *base, Elf64_Word size,
                  const IPC_GLOBAL_IDX_MAP* idx_map, incl_name_map_t& fn_map)
{
    OPCODE opcode = (OPCODE) WN_opcode (node);
    WN *wn;

    if (OPCODE_has_sym(opcode) || OPCODE_has_label(opcode)) {
      const ST_IDX idx = WN_st_idx(node);
      if (ST_IDX_level(idx) == GLOBAL_SYMTAB) // Global symbol
        WN_st_idx(node) = idx_map->st[idx];
    }
#ifdef KEY
    if (OPCODE_operator(opcode) == OPR_PRAGMA &&
        WN_pragma(node) == WN_PRAGMA_THREADPRIVATE) {
      const ST_IDX idx = WN_pragma_arg2(node);
      if (ST_IDX_level(idx) == GLOBAL_SYMTAB) {
        #pragma frequency_hint frequent
        WN_pragma_arg2(node) = idx_map->st[idx];
      }
    }
#endif
    if (OPCODE_has_1ty(opcode)) {
      WN_set_ty(node, idx_map->ty[WN_ty(node)]);
    }
    else if (OPCODE_has_2ty(opcode)) {
      WN_set_ty(node, idx_map->ty[WN_ty(node)]);
      WN_set_load_addr_ty(node, idx_map->ty[WN_load_addr_ty(node)]);
    }
    else if (OPCODE_operator(opcode) == OPR_PRAGMA &&
             WN_pragma(node) == WN_PRAGMA_TYPE_OF_RESHAPED_ARRAY) {
      WN_pragma_arg1(node) = idx_map->ty[WN_pragma_arg1(node)];
    }
    
             
    
#ifndef CFE
    if (opcode == OPC_REGION)
        Set_Max_Region_Id (WN_region_id(node));
#endif
    
#if defined(BACK_END)
    if (opcode == OPC_ALTENTRY)
        Set_PU_has_altentry (Get_Current_PU ());
    
    /* Count whirl nodes that are useful for Olimit and other stats */
#ifdef KEY
    Count_WN (node, PU_WN_BB_Cnt, PU_WN_Stmt_Cnt, PU_WN_Call_Cnt);
#else
    Count_WN_Opcode (opcode, &PU_WN_BB_Cnt, &PU_WN_Stmt_Cnt);
#endif // KEY
#endif

    if (opcode == OPC_BLOCK) {
        wn = WN_first(node);
        if (wn == (WN *) -1) {
            WN_first(node) = NULL;
            WN_last(node) = NULL;
        } else {
            wn = convert_offset(wn, base);
            WN_first(node) = wn;
            WN_last(node) = convert_offset(WN_last(node), base);

            do {
                if (IP_READ_fix_tree (pu, wn, base, size, idx_map, fn_map) == ERROR_VALUE)
                    return ERROR_VALUE;
                wn = WN_next(wn);
            } while (wn);
        }
    }
    else if (!OPCODE_is_leaf(opcode)) {
        const INT cnt = WN_kid_count(node);
        WN** wn_ptr = &WN_kid(node, 0);
        for (INT i = 0; i < cnt; i++, wn_ptr++) {
            wn = *wn_ptr;
            if (wn == (WN *) -1) {
                *wn_ptr = NULL;
            }
            else {
                wn = convert_offset(wn, base);
                *wn_ptr = wn;
                if (IP_READ_fix_tree (pu, wn, base, size, idx_map, fn_map) == ERROR_VALUE)
                    return ERROR_VALUE;
            }
        }
    }
    
    if (OPCODE_has_next_prev(opcode)) {
        // update STMT SRCPOS file nbr
        mUINT64 isrcpos = WN_linenum(node);
        if( SRCPOS_filenum(isrcpos) != 0 ) {
           UINT32 mapped_fn = fn_map[SRCPOS_filenum(isrcpos)];
           FmtAssert(mapped_fn != 0,
                     ("mapped file number is zero, node=%p, filenum=%u, linenum=%d",
                      node, SRCPOS_filenum(isrcpos),  SRCPOS_linenum(isrcpos)));
           SRCPOS_filenum(isrcpos) = mapped_fn;
           WN_Set_Linenum(node, isrcpos);
        }
        else {
           DevWarn("original source position file nbr is zero, node=%p", node);
        }

        wn = WN_prev(node);
        if (wn == (WN *) -1) {
            WN_prev(node) = NULL;
        } else {
            wn = convert_offset(wn, base);
            WN_prev(node) = wn;
        }
        wn = WN_next(node);
        if (wn == (WN *) -1) {
            WN_next(node) = NULL;
        } else {
            wn = convert_offset(wn, base);
            WN_next(node) = wn;
        }
    }

    /* keep track of the last map ID in each opcode category */
    if (WN_map_id(node) != -1) {
        register OPERATOR_MAPCAT category = OPCODE_mapcat(opcode);
        register INT32 map_id = WN_map_id(node);
        register INT32 *last_id_ptr;

        last_id_ptr = &WN_MAP_TAB_Last_ID(Current_Map_Tab, category);
        if (map_id > *last_id_ptr) {
            *last_id_ptr = map_id;
        }
    }

#if defined(BACK_END) || defined(IR_TOOLS)
    WSSA::WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(pu);
    if (WN_map_id(node) != -1 && mgr != NULL) {
        // create the map between WN* and map_id
        mgr->Add_wn(node);
    }
#endif

    return 0;
} // IP_READ_fix_tree 

WN *
IP_READ_get_tree (void *handle, PU_Info *pu,
                  const IPC_GLOBAL_IDX_MAP* idx_map,
                  incl_name_map_t& fn_map)
{
    Subsect_State st = PU_Info_state(pu, WT_TREE);
    if (st == Subsect_InMem)
        return PU_Info_tree_ptr(pu);
    if (st != Subsect_Exists)
        return reinterpret_cast<WN*>(ERROR_VALUE);

    Elf64_Word offset = PU_Info_subsect_offset(pu, WT_TREE);
    Elf64_Word size = PU_Info_subsect_size(pu, WT_TREE);

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
    if (shdr.offset == 0)
      return reinterpret_cast<WN*>(ERROR_VALUE);

    if (offset + size > shdr.size) {
        errno = EINVAL;
        return reinterpret_cast<WN*>(ERROR_VALUE);
    }

    char* section_base = (char *) handle + shdr.offset;
    char* tree_base = section_base + offset;

    // The offset of the first node is at the beginning of the subsection
    Elf64_Word first_node = *(Elf64_Word *)tree_base;
    WN* wn = (WN *) (tree_base + first_node);

#ifndef CFE
    Set_Max_Region_Id(0);       // Reset max id for pu 
#endif

    // Fix up the pointers in the WNs, and remap symbol table indices.

    Current_Map_Tab = PU_Info_maptab(pu);
    if (IP_READ_fix_tree (pu, wn, tree_base, size, idx_map, fn_map) == ERROR_VALUE)
      return reinterpret_cast<WN*>(ERROR_VALUE);

    WN_next(wn) = NULL;
    WN_prev(wn) = NULL;

    Set_PU_Info_tree_ptr(pu, wn);
    Set_PU_Info_state(pu, WT_TREE, Subsect_InMem);

    return wn;
} // IP_READ_get_tree

namespace {

// The structs and functions in this unnamed namespace are used by
// IP_READ_pu.


  // Function object that updates all indices, in an ST entry in a local
  // symtab, that might have been changed by merging the global symtabs.
  struct fix_st_entry {
    const SYMSTR_IDX_MAP& str_map;
    const ST_IDX_MAP& st_map;
    const TY_IDX_MAP& ty_map;
    const TCON_IDX_MAP& tcon_map;
    IPA_NODE* node;

    fix_st_entry(IPA_NODE* n, const IPC_GLOBAL_IDX_MAP* idx_map)
      : str_map(idx_map->sym_str),
        st_map(idx_map->st),
        ty_map(idx_map->ty),
        tcon_map(idx_map->tcon),
	node (n)
      {}

    void operator()(UINT32, ST* st) const {
      const ST_CLASS sym_class = ST_sym_class(*st);
    
      if (sym_class == CLASS_CONST)
        Set_ST_tcon(*st, tcon_map[ST_tcon(*st)]);
      else
        Set_ST_name_idx(*st, str_map[ST_name_idx(*st)]);

      if (sym_class != CLASS_FUNC &&  // We don't need to remap PU_IDX or
          sym_class != CLASS_BLOCK && // BLK_IDX.
          sym_class != CLASS_NAME)
        Set_ST_type(*st, ty_map[ST_type(*st)]);

      const ST_IDX base = ST_base_idx(*st);
      if (base != ST_st_idx(*st)) {
	if (ST_IDX_level(base) == GLOBAL_SYMTAB) {
	  const ST& mapped_base = St_Table[st_map[base]];
	  Set_ST_base_idx(*st, ST_st_idx (mapped_base));
	  Set_ST_storage_class (*st, ST_storage_class (mapped_base));
	  if (ST_is_initialized (mapped_base)) {
	    Set_ST_is_initialized (st);
	    if (ST_init_value_zero (mapped_base))
	      Set_ST_init_value_zero (st);
	  }
	  if (ST_is_not_used (mapped_base))
	    Set_ST_is_not_used (st);
	  if (ST_is_const_var (mapped_base))
	    Set_ST_is_const_var (st);
	} else {
	  const ST& base_st = St_Table[base];
	  if (ST_sclass (base_st) == SCLASS_FORMAL ||
	      ST_sclass (base_st) == SCLASS_FORMAL_REF)
	    if (node)
	        node->Set_Aliased_Formal ();
	}
      }
	  
      if (base != ST_st_idx(*st) && ST_IDX_level(base) == GLOBAL_SYMTAB)
        Set_ST_base_idx(*st, st_map[base]);
    }
  };

  // Function object that updates all indices, in a label entry, that
  // might have been changed by merging the global symtabs.  The only
  // such index is the STR_IDX.
  struct fix_label_entry {
    const SYMSTR_IDX_MAP& str_map;

    fix_label_entry(const IPC_GLOBAL_IDX_MAP* idx_map)
      : str_map(idx_map->sym_str)
      {}

    void operator()(UINT32, LABEL* lbl) const {
      Set_LABEL_name_idx(*lbl, str_map[LABEL_name_idx(*lbl)]);
    }
  };

  // Function object that updates all indices, in a preg entry, that
  // might have been changed by merging the global symtabs.  The only
  // such index is the STR_IDX.
  struct fix_preg_entry {
    const SYMSTR_IDX_MAP& str_map;

    fix_preg_entry(const IPC_GLOBAL_IDX_MAP* idx_map)
      : str_map(idx_map->sym_str)
      {}

    void operator()(UINT32, PREG* preg) const {
      Set_PREG_name_idx(*preg, str_map[PREG_name_idx(*preg)]);
    }
  };

  // Function object that updates all indices, in an inito entry, that
  // might have been changed by merging the global symtabs.
  struct fix_inito_entry {
    const ST_IDX_MAP& st_map;
    const INITV_IDX_MAP initv_map;

    fix_inito_entry(const IPC_GLOBAL_IDX_MAP* idx_map)
      : st_map(idx_map->st),
        initv_map(idx_map->initv)
      {}

    void operator()(UINT32, INITO* inito) const {
      if (ST_IDX_level(INITO_st_idx(*inito)) == GLOBAL_SYMTAB)
        Set_INITO_st_idx(*inito, st_map[INITO_st_idx(*inito)]);

      Set_INITO_val(*inito, initv_map[INITO_val(*inito)]);
    }
  };

  void fix_local_st_tab(IPA_NODE* node,
			SYMTAB_IDX level,
                        const IPC_GLOBAL_IDX_MAP* idx_map) {
    For_all(St_Table, level, fix_st_entry(node, idx_map));
  }

  void fix_local_label_tab(const IPC_GLOBAL_IDX_MAP* idx_map) {
    For_all(Label_Table, fix_label_entry(idx_map));
  }

  void fix_local_preg_tab(const IPC_GLOBAL_IDX_MAP* idx_map) {
    For_all(Preg_Table, fix_preg_entry(idx_map));
  }

  void fix_local_inito_tab(SYMTAB_IDX level,
                           const IPC_GLOBAL_IDX_MAP* idx_map) {
    For_all(Inito_Table, level, fix_inito_entry(idx_map));
  }

} // Close the unnamed namespace.

void
IP_READ_pu (IPA_NODE* node, IP_FILE_HDR& s, INT p_index, MEM_POOL *pool)
{
  Temporary_Error_Phase ephase( "Reading WHIRL file" );
  IP_PROC_INFO& proci = IP_FILE_HDR_proc_info(s)[p_index];

  // Check to see if the pu has already been read.
  if (IP_PROC_INFO_state(proci) == IPA_ORIG) {
    PU_Info* const pu = IP_PROC_INFO_pu_info(proci);

    FmtAssert((pu != 0), ("IP_READ_pu called for alternate entry point"));

    void* fhandle = IP_FILE_HDR_input_map_addr(s);

    // FB_PU_Has_Feedback = FALSE;

    IP_READ_file_info (s);      // Make sure the global tables are set up.
    
    Set_IP_PROC_INFO_state(proci, IPA_MODIFIED);

    /* set the map table */
    Current_Map_Tab = PU_Info_maptab(pu);
    if (!Current_Map_Tab) {
      Current_Map_Tab = WN_MAP_TAB_Create(pool);
      PU_Info_maptab(pu) = Current_Map_Tab;
    }

    // to get PU_WN_BB_Cnt & PU_WN_Stmt_Cnt right
    Initialize_PU_Stats();

    CURRENT_SYMTAB = PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]);
    Is_True(CURRENT_SYMTAB >= 2,
            ("Incorrect lexical level %d for pu %s",
             CURRENT_SYMTAB,
             ST_name(St_Table[PU_Info_proc_sym(pu)])));

#if Is_True_On
    for (UINT i = GLOBAL_SYMTAB; i < CURRENT_SYMTAB; ++i) {
      Is_True(Scope_tab[i].st_tab != NULL,
	      ("Parent PU's symtab must be set up"));
    }
#endif

    New_Scope (CURRENT_SYMTAB, pool, FALSE);
    if (WN_get_symtab (fhandle, pu) == -1)
      ErrMsg ( EC_IR_Scn_Read, "local symtab", IP_FILE_HDR_file_name(s));

    Scope_tab[CURRENT_SYMTAB].st = WN_get_proc_sym (pu);
    if (Scope_tab[CURRENT_SYMTAB].st == reinterpret_cast<ST *>(-1))
      ErrMsg ( EC_IR_Scn_Read, "proc ST", IP_FILE_HDR_file_name(s));
    else
      Current_pu = &Pu_Table[ST_pu (Scope_tab[CURRENT_SYMTAB].st)];

#if defined(BACK_END) || defined(IR_TOOLS)
    if (PU_Info_state(pu, WT_SSA) == Subsect_Exists) {
        // having WSSA in the IR file, create the SSA manager
        Set_PU_Info_ssa_ptr(pu, new WSSA::WHIRL_SSA_MANAGER(pool));
    }
#endif

    if (IP_READ_get_tree (fhandle,
                          pu,
                          IP_FILE_HDR_idx_maps(s),
                          IP_FILE_HDR_fn_map(s)) == (WN*) -1) {
      ErrMsg ( EC_IR_Scn_Read, "tree", IP_FILE_HDR_file_name(s));
    }

    // The local symbol table consists of ST_TAB, INITO_TAB, LABEL_TAB, and
    // PREG_TAB.  We have to fix any entries into the global symbol table
    // to accommodate the merged symtab.

    fix_local_st_tab(node, CURRENT_SYMTAB, IP_FILE_HDR_idx_maps(s));
    fix_local_label_tab(IP_FILE_HDR_idx_maps(s));
    fix_local_preg_tab(IP_FILE_HDR_idx_maps(s));
    fix_local_inito_tab(CURRENT_SYMTAB, IP_FILE_HDR_idx_maps(s));

#ifdef BACK_END
    if (WN_get_depgraph (fhandle, pu) == (void *) -1) {
      ErrMsg ( EC_IR_Scn_Read, "dependence graph", IP_FILE_HDR_file_name(s));
    }
#endif

#if defined(BACK_END) || defined(BUILD_WHIRL2C) || defined(BUILD_WHIRL2F)
    if (WN_get_prefetch (fhandle, pu) == -1) {
      ErrMsg ( EC_IR_Scn_Read, "prefetch map", IP_FILE_HDR_file_name(s));
    }
#endif

#if defined(BACK_END) || defined(IR_TOOLS)
    if (PU_Info_state(pu, WT_SSA) == Subsect_Exists) {
      // We should call
      //    WN_get_SSA (fhandle, pu, pool);
      // But WSSA doesn't support IPA so far, onlu set the state
      Set_PU_Info_state(pu, WT_SSA, Subsect_InMem);
    }
#endif

    if (WN_get_feedback (fhandle, pu, pool) == -1)
	ErrMsg ( EC_IR_Scn_Read, "feedback info", IP_FILE_HDR_file_name(s));

    if (PU_Info_state (pu, WT_FEEDBACK) == Subsect_InMem) {
	const Pu_Hdr* pu_hdr = (const Pu_Hdr*)
	    PU_Info_feedback_ptr (pu);
#ifdef KEY
	Cur_PU_Feedback = CXX_NEW (FEEDBACK (PU_Info_tree_ptr (pu),
					     pool,
					     pu_hdr->pu_num_inv_entries,
					     pu_hdr->pu_num_br_entries,
					     pu_hdr->pu_num_loop_entries,
					     pu_hdr->pu_num_scircuit_entries,
					     pu_hdr->pu_num_call_entries,
					     pu_hdr->pu_num_icall_entries,
					     pu_hdr->pu_num_switch_entries,
					     pu_hdr->pu_num_value_entries,
					    pu_hdr->pu_num_value_fp_bin_entries,
					     pu_hdr->runtime_fun_address),
				   pool);
#else
	Cur_PU_Feedback = CXX_NEW (FEEDBACK (PU_Info_tree_ptr (pu),
					     pool,
					     pu_hdr->pu_num_inv_entries,
					     pu_hdr->pu_num_br_entries,
					     pu_hdr->pu_num_loop_entries,
					     pu_hdr->pu_num_scircuit_entries,
					     pu_hdr->pu_num_call_entries,
					     pu_hdr->pu_num_icall_entries,
					     pu_hdr->pu_num_switch_entries),
				   pool);
#endif
	Read_Feedback_Info (Cur_PU_Feedback, PU_Info_tree_ptr (pu), *pu_hdr);
	// FB_PU_Has_Feedback = TRUE;
	IPA_Has_Feedback = TRUE;
	node->Set_Feedback_Info (Cur_PU_Feedback);
    } else {
	Cur_PU_Feedback = NULL;
    }

    // Read in the constraint graph summary for Nystrom Alias Analyzer
    if (WN_get_INT32_map(fhandle, pu,
                         WT_ALIAS_CGNODE, WN_MAP_ALIAS_CGNODE) == -1) {
      ErrMsg(EC_IR_Scn_Read, "alias cgnode map", IP_FILE_HDR_file_name(s));
    }
  }
}
