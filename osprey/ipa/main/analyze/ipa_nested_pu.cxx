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
#include "pu_info.h"			// PU_Info
#include "ir_bread.h"			// WN_get_section_base ()
#include "ipc_file.h"			// file header defs.
#include "ipc_symtab_merge.h"		// Aux_XX_Tables
#include "ipa_cg.h"
#include "ipa_nested_pu.h"

typedef hash_map<PU_IDX, PU_IDX> IPA_NESTED_PU_PARENT_MAP;

static IPA_NESTED_PU_PARENT_MAP* nested_pu_parent_map;

static void
Build_Parent_Child_Relations_For_One_Parent(PU_Info *parent)
{
  PU_Info* child = PU_Info_child(parent);
  while (child) {
    (*nested_pu_parent_map)[ST_pu(St_Table[PU_Info_proc_sym(child)])] = 
      ST_pu(St_Table[PU_Info_proc_sym(parent)]);   
    PU_Info *grandchild = PU_Info_child(child);
    if (grandchild)
      Build_Parent_Child_Relations_For_One_Parent(child);

    child = PU_Info_next(child);
  }
}

static void
Build_Parent_Child_Relations(IP_FILE_HDR& hdr)
{
    PU_Info* parent = IP_FILE_HDR_pu_list(hdr);

    while (parent) {
      Build_Parent_Child_Relations_For_One_Parent(parent);
      parent = PU_Info_next(parent);
    }
}

IPA_NODE* 
Get_Parent_Of_Nested_PU(IPA_NODE* child) 
{
    if ((child->Lexical_Level()-1) == GLOBAL_SYMTAB)
	return NULL;

    PU_IDX child_pu_idx = ST_pu(child->Func_ST());
    PU_IDX parent_pu_idx = (*nested_pu_parent_map)[child_pu_idx];

    Is_True(parent_pu_idx > PU_IDX_ZERO && parent_pu_idx < PU_Table_Size(),
          ("Get_Parent_Of_Nested_PU: bad pu index %d, not in range 0 <= idx < %d",
           parent_pu_idx, PU_Table_Size()));

    NODE_INDEX node_idx = AUX_PU_node(Aux_Pu_Table[parent_pu_idx]);
    IPA_NODE* result = IPA_Call_Graph->Graph()->Node_User(node_idx);

    Is_True(result != NULL, ("Get_Parent_Of_Nested_PU: Bad parent IPA_NODE\n"));

    return result;
}


void 
Build_Nested_Pu_Relations()
{

    if (nested_pu_parent_map == NULL)
        nested_pu_parent_map = CXX_NEW(IPA_NESTED_PU_PARENT_MAP(), Malloc_Mem_Pool);

    for (UINT i = 0; i < IP_File_header.size(); ++i) {
      if (IP_FILE_HDR_has_nested_pu(IP_File_header[i]))
          Build_Parent_Child_Relations (IP_File_header[i]);
    }
}

