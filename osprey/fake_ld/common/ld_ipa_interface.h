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


/* #ifdef _TARG_MIPS */

#ifndef __IPA_INTERFACE_H__
#define __IPA_INTERFACE_H__

/* Do NOT put any inlined function definitions in this header file.  We
   need to maintain a pure functional interface between ld and ipa.
 */

#ifdef __cplusplus
extern "C" {
#endif

extern void *
ld_slookup_mext(char *, boolean);

extern UINT64
ld_get_section_size (an_object_file_ptr pobj, int index);
#pragma weak ld_get_section_size

extern char *
ld_get_section_name (an_object_file_ptr pobj, int index);
#pragma weak ld_get_section_name

extern char *
ld_get_section_base (an_object_file_ptr pobj, int index);
#pragma weak ld_get_section_base

extern void *
ld_get_mmap_addr (an_object_file_ptr pobj);
#pragma weak ld_get_mmap_addr

extern unsigned int
ld_get_sym_attr (void* pext);
#pragma weak ld_get_sym_attr

extern int
ld_is_weak_symbol (void* pext);
#pragma weak ld_is_weak_symbol

extern ST_EXPORT
ld_get_export (void* pext);
#pragma weak ld_get_export

extern void
ld_set_st_idx (void* pext, ST_IDX st_idx);
#pragma weak ld_set_st_idx

extern ST_IDX
ld_get_st_idx (void* pext);
#pragma weak ld_get_st_idx


extern BOOL
ld_resolved_to_obj (void* pext, void* pobj);
#pragma weak ld_resolved_to_obj

extern void
cleanup_symtab_for_ipa (void);

extern INT 
Count_elf_external_gots (void);
#pragma weak Count_elf_external_gots

#ifdef __cplusplus
}
#endif

#endif /*  __IPA_INTERFACE_H__ */

