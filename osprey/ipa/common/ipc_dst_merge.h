/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
#ifndef __cplusplus
#error This header must be compiled as C++
#endif

#ifndef IPC_DST_MERGE_included
#define IPC_DST_MERGE_included

#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"
#include "pu_info.h"
#include "mempool.h"

void Add_files_to_global_file_list(DST_TYPE src, DST_TYPE dest,
                                   incl_name_map_t& incl_map,
                                   incl_name_map_t& fn_map,
                                   MEM_POOL* p);
DST_TYPE IPC_merge_DSTs(pu_info* pu_tree, DST_TYPE gbl_dst,  MEM_POOL* p);
void dump_path_file_lists(DST_TYPE gbl_file_list);

#endif /* IPC_DST_MERGE_included */
