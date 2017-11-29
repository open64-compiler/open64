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


#ifndef __IR_BWRITE_H__
#define __IR_BWRITE_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#ifdef __MINGW32__
#include<WINDOWS.H> /* for HANDLE */
/* ideally should move def of output_file to private header
 * so this windows.h doesn't percolate to callers.
 * Problem is that windows.h needs to be included before defs.h
 * due to conflict with mUINT32, but defs.h usually before ir_bwrite.h */
#endif /* __MINGW32__ */
				       
/*
 * This flag is usually FALSE, but bedriver may set it to TRUE so that
 * the backend's internal maps will be written out.
 */
#ifdef BACK_END
extern BOOL Write_BE_Maps;
extern BOOL Write_AC_INTERNAL_Map;
extern BOOL Write_ALIAS_CLASS_Map;
extern BOOL Write_ALIAS_CGNODE_Map;
#endif /* BACK_END */


/*
 * Exported interface for writing WHIRL in binary form to a file.
 * All functions return -1 upon failure, which should be treated as
 * a fatal error and the process should be aborted.  Also, "errno" will
 * be set and can be used for printing appropriate error messages.
 * If the operation is successful, 0 will be returned.
 */


typedef struct section {
    const char *name;		    /* section name */
    Elf64_Shdr shdr;		    /* Elf section */
} Section;


typedef struct output_file {
    char *file_name;
#ifdef __MINGW32__
    int output_fd;		    /* handle for output file */
    HANDLE mapHd;		    /* Handle for mapped region */
#else
    INT output_fd;		    /* file id for output file */
#endif /* __MINGW32__ */
    char *map_addr; 		    /* base address of the mapped region */
    off_t mapped_size;		    /* max. size of the mapped region */
    off_t file_size;
    Section *section_list;	    /* array of Elf sections */
    INT max_num_of_section;
    INT num_of_section;
    Section *cur_section;	    /* set by get_section() */
} Output_File;


extern Output_File *Current_Output;

/* Call these once at the beginning and end, respectively. */
extern Output_File *WN_open_output (char *file_name);
extern void WN_close_output (Output_File *fl);
extern void WN_close_file (void *fl);

extern Section *
get_section (Elf64_Word sh_info, const char *name, Output_File *fl);

/*
 * Write global tables to their own Elf sections.  These should only be
 * called after all the PUs have been written out.
 */

extern void WN_write_globals (Output_File *fl);
extern void WN_write_dst (void *dst, Output_File *fl);
extern void WN_write_strtab (const void *strtab, UINT64 size, Output_File *fl);
extern void WN_write_localmap (void *localmap, Output_File *fl);
extern void IPA_write_summary (void (*IPA_irb_write_summary) (Output_File*),
			      Output_File *fl);
extern void IPA_copy_PU(PU_Info *pu, char *section_base, Output_File *outfile);

extern void WN_write_flags (INT argc, char **argv, Output_File *fl);
extern void WN_write_revision (Output_File *fl);
extern void WN_close_file (void *this_fl);
#if defined(TARG_SL)
extern void WN_write_isr_cg (vector<mINT32>& cg, Output_File *fl);
#endif

/*
 * Write PU section header.  This must be called after writing out all the
 * PUs but before writing any of the global tables.
 */

extern void WN_write_PU_Infos (PU_Info *pu_list, Output_File *fl);


/*
 * Write PU-specific structions to subsections of the PU section.
 * WN_write_tree uses the "off_map" mapping to store the subsection offsets
 * of the WHIRL nodes that may be referenced by the dependence graph or
 * prefetch pointers.
 */

extern void WN_write_tree (PU_Info *pu, WN_MAP off_map, Output_File *fl);
extern void WN_write_symtab (PU_Info *pu, Output_File *fl);
extern void WN_write_depgraph (PU_Info *pu, WN_MAP off_map, Output_File *fl);
extern void WN_write_prefetch (PU_Info *pu, WN_MAP off_map, Output_File *fl);
extern void WN_write_feedback (PU_Info *pu, Output_File *fl);
extern void WN_write_INT32_map(PU_Info     *pu,
			       WN_MAP       off_map,
			       Output_File *fl,
			       INT32        subsection_type,
			       WN_MAP       value_map,
			       const char  *subsection_name);
extern void WN_write_voidptr_map(PU_Info     *pu,
				 WN_MAP       off_map,
				 Output_File *fl,
				 INT32        subsection_type,
				 WN_MAP       value_map,
				 const char  *subsection_name);
extern void WN_write_SSA(PU_Info *pu, Output_File *fl);
#ifndef OWN_ERROR_PACKAGE

/*
 * Define common routines for writing all the whirl sections.
 * These routines use the standard compiler error reporting mechanism.
 */

extern Output_File *Open_Output_Info (char *output_file);
extern void Write_PU_Info (PU_Info *pu);
extern void Write_Global_Info (PU_Info *pu_tree);
extern void Close_Output_Info (void);
extern void WN_write_elf_symtab (const void* symtab, UINT64 size,
				 UINT64 entsize, UINT align, Output_File* fl);

#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* __IR_BWRITE_H__ */
