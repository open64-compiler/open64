/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef __IR_BREAD_H__
#define __IR_BREAD_H__

#include "wn_map.h"

#define REVISION_MISMATCH 0
#define READER_ERROR -1
#define ABI_MISMATCH -2

extern BOOL Read_ALIAS_CGNODE_Map;
				       

struct OFFSET_AND_SIZE
{
    INT64 offset;
    UINT64 size;

    OFFSET_AND_SIZE (INT64 o, UINT64 s) : offset (o), size (s) {}
};


/*
 * Exported interface for reading binary WHIRL from a file.
 */

OFFSET_AND_SIZE
get_section (void *handle, Elf64_Word type, Elf64_Word info);    

/* Enable/disable verbose info for debugging. */
extern void Set_Verbose_Info (BOOL val);

/* Check the Elf headers (called from elfdump) */
extern INT WN_massage_input (char *baseaddr, Elf64_Word size, char*);

/* Find the beginning of a particular section */
extern void *WN_get_section_base (void *handle, INT sect);

/* return size of elf section */
extern Elf64_Word Get_Elf_Section_Size (void *handle, Elf64_Word type, Elf64_Word info);


/* Call these once at the beginning and end, respectively. */
extern void *WN_inline_open_file (char* file_name, off_t *mapped_size,
				  char* file_revision); 

#ifdef __MINGW32__
#include <windows.h>
void *
WN_open_input (char *filename, off_t *mapped_size, int *fd, HANDLE *mapHd);
#else
extern void *
WN_open_input (char *filename, off_t *mapped_size);
#endif /* __MINGW32__ */

/*
 * Read the PU subsection headers into a tree of PU_Info structures.
 * Returns a pointer to the root of the tree or -1 on error.  The
 * number of PUs is returned through the p_num_PUs parameter.
 */

extern PU_Info *WN_get_PU_Infos (void *handle, INT32 *p_num_PUs);


/*
 * Read the PU subsections.  These return -1 for errors.
 */

extern WN *WN_get_tree (void *handle, PU_Info *pu);
extern INT WN_get_symtab (void *handle, PU_Info *pu);
extern void *WN_get_depgraph (void *handle, PU_Info *pu);
extern INT WN_get_prefetch (void *handle, PU_Info *pu);

extern INT WN_get_INT32_map(void *handle, PU_Info *pu,
			    INT32 subsection_type, WN_MAP value_map);

extern ST *WN_get_proc_sym (PU_Info *pu);


/*
 * Read the global tables.  These are usually called right after opening
 * the input file.  They return -1 for errors.
 */


extern INT WN_get_global_symtab (void *handle);
extern INT WN_get_strtab (void *handle);


extern INT WN_get_dst (void *handle);
extern INT WN_get_localmap (void *handle);
extern INT WN_get_feedback (void* handle, PU_Info*pu, MEM_POOL* pool);    
extern INT WN_get_SSA (void *handle, PU_Info *pu, MEM_POOL* pool);

/*
 * Read the command line flags used to compile the input file.  The
 * return value is argc (or 0 if the flags are missing) and the argv
 * array is returned through the argv parameter.
 */

extern INT WN_get_flags (void *handle, char ***argv);


#ifndef OWN_ERROR_PACKAGE
/*
 * Define common routines for reading all the whirl sections.
 * These routines use the standard compiler error reporting mechanism.
 */

/* Open_Input_Info opens a single file for global and local info;
 * Open_Global_Input only opens the global file,
 * Open_Local_Input only opens the local file.
 */
extern void* Open_Input_Info (char *input_file);
extern void* Open_Global_Input (char *input_file);
extern void* Open_Local_Input (char *input_file);
extern PU_Info *Read_Global_Info (INT32 *p_num_PUs);
extern void Read_Local_Info (MEM_POOL *pool, PU_Info *pu);
extern void Free_Local_Info (PU_Info *pu);
extern void Free_Dep_Graph (void);
extern void Free_Local_Input(void);
extern void Free_Input_Info (void);

#endif

extern void *local_fhandle;

#endif /* __IR_BREAD_H__ */
