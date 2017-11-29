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
// Module: ipo_across_file_utils.cxx
//
// Revision history:
//  31 July -96 - Original Version
//
// Description:  utilities used by the across file inlining
//
// ===================================================================
//====================================================================
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#define USE_STANDARD_TYPES          /* override unwanted defines in defs.h */
#include "defs.h"
#include "config_ipa.h"             // for INLINE/IPA options
#include "mempool.h"
#include "stab.h"
#include "wn.h"
#include "dwarf_DST_mem.h"          // for ipc_file.h
#include "ipc_file.h"
#include "cxx_base.h"
#include "cxx_memory.h"             // for CXX_NEW, etc.
#include "clone.h"
#include "ipo_clone.h"
#include "ipo_st_utils.h"
#include "clone.h"
#include "ipo_clone.h"
#include "ipo_across_file_utils.h"

//------------------------------------------------------------
// initialize the mappings
//------------------------------------------------------------
void Init_ty_maps(IP_FILE_HDR *shd_callee, IP_FILE_HDR *shd_caller,
		  SYMTAB *caller_gs)
{

  MEM_POOL* caller_aux_mem_pool = &IP_FILE_HDR_aux_mem_pool(shd_caller);
  MEM_POOL* callee_aux_mem_pool = &IP_FILE_HDR_aux_mem_pool(shd_callee);
	
  if (shd_callee != shd_caller) {
    if (IP_FILE_HDR_global_ty_map(shd_caller) == 0) {
      IP_FILE_HDR_global_ty_map(shd_caller) = (void *)
	CXX_NEW (IPO_TY_HASH (caller_aux_mem_pool),
		 caller_aux_mem_pool);
    }
    if (IPA_Enable_Merge_ty) {
      IPO_TY_HASH *map =
	(IPO_TY_HASH *) IP_FILE_HDR_global_ty_map(shd_caller);
      map->Init_Hash (SYMTAB_types(caller_gs));
      
      if (IP_FILE_HDR_global_ty_map(shd_callee) == 0) {
	IP_FILE_HDR_global_ty_map(shd_callee) = (void *)
	  CXX_NEW (IPO_TY_HASH (callee_aux_mem_pool),
		   callee_aux_mem_pool);
      }
    }
  }
}
