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


#ifndef __IPC_SYMTAB_H__
#define __IPC_SYMTAB_H__

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

#ifdef __cplusplus
extern "C" {
#endif 

#ifdef _LD_IPA_INTERFACE

/* Given a WHIRL file, this routine will merge the global symbol-table
   in with the current merged global symbol table which should already
   have been set up as part of the SCOPE array.  We also check that
   file format and revision is as expected.

   We return a map from old indices into the global symbol table to
   to new indices into the merged global symbol table.  Note that the
   IPC_GLOBAL_IDX_MAP structure will be allocated in a Malloc_Mem_Pool,
   and must be freed up by the caller of this routine when appropriate.
 */
extern void
process_whirl32 (an_object_file_ptr p_obj, int nsec,
                 const Elf32_Shdr* section_table,
                 boolean check_whirl_revision, const char* file_name, off_t);

extern void
process_whirl64 (an_object_file_ptr p_obj, int nsec,
                 const Elf64_Shdr* section_table,
                 boolean check_whirl_revision, const char* file_name, off_t);


#ifdef _64BIT_OBJECTS

#pragma weak process_whirl64

#else /* _64BIT_OBJECTS */

#pragma weak process_whirl32

#endif /* _64BIT_OBJECTS */

#else /* _LD_IPA_INTERFACE */

extern void enter_mext32 (ST *st, LD_OBJECT_FILE *p_obj);
extern void enter_mext64 (ST *st, LD_OBJECT_FILE *p_obj);


#endif /* _LD_IPA_INTERFACE */

#ifdef __cplusplus
}
#endif

#else // _STANDALONE_INLINER || _LIGHTWEIGHT_INLINER
extern void Process_Global_Symtab (void* handle, IP_FILE_HDR& file_header);
#endif // _STANDALONE_INLINER


#endif /* __IPC_SYMTAB_H__ */
