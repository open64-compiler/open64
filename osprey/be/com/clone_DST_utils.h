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

#ifndef clone_DST_utils_INCLUDED
#define clone_DST_utils_INCLUDED


#include "defs.h"

#ifndef dwarf_DST_mem_INCLUDED
#include "dwarf_DST_mem.h"              // for DST_IDX
#endif

#include "symtab.h"

// To avoid extra includes:
typedef struct mem_pool MEM_POOL;
class  IPO_SYMTAB;

extern mUINT16
DST_Enter_Callee_File_Dst(DST_TYPE caller_file_dst,
                          DST_TYPE callee_file_dst);

extern mUINT16
DST_get_cross_file_id(DST_IDX parent,
                      DST_IDX inl_routine, 
                      DST_TYPE caller_file_dst,
                      DST_TYPE callee_file_dst);

extern void
DST_enter_inlined_subroutine(DST_IDX parent,
                             DST_IDX inl_routine,
                             LABEL_IDX begin_label,
                             LABEL_IDX end_label,
                             DST_TYPE caller_file_dst,
                             DST_TYPE callee_file_dst,
                             IPO_SYMTAB *symtab,
                             MEM_POOL *caller_file_m,
                             MEM_POOL *callee_file_m,
                             mUINT16 cross_file_id);

extern DST_IDX 
DST_enter_cloned_subroutine(DST_IDX parent, 
                            DST_IDX orig_node, 
                            ST *cloned_st, 
                            DST_TYPE cur_file_dst,
                            IPO_SYMTAB *sym);

#endif /* clone_DST_utils_INCLUDED */
