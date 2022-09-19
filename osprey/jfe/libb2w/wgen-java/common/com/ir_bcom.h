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


#ifndef __IR_BCOM_H__
#define __IR_BCOM_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern char *Whirl_Revision;

/* This variable is used by IPAA to pass its local map information
 * to ir_bwrite.c, and by ir_bread.c to pass it to WOPT:
 */
extern void *IPAA_Local_Map;

extern BOOL Doing_mmapped_io;

inline off_t
ir_b_align (off_t offset, UINT32 addralign, UINT32 padding)
{
     if (addralign-- > 1)
	return ((offset + padding + addralign) & ~((off_t)addralign)) - padding;
    else
	return offset;
}

extern Elf64_Word
ir_b_save_buf (const void *buf, Elf64_Word size, UINT32 align,
	       UINT32 padding, Output_File *fl);

extern Elf64_Word
ir_b_copy_file (const void *buf, Elf64_Word size, void *fl);

extern char*
ir_b_grow_map (Elf64_Word min_size, Output_File *fl);

extern char *
ir_b_create_map (Output_File *fl);

#if defined(KEY) && !defined(FRONT_END) && !defined(IR_TOOLS)
extern Elf64_Word
ir_b_write_tree (WN *, off_t, Output_File *, WN_MAP, PU_Info *);
#else
extern Elf64_Word
ir_b_write_tree (WN *node, off_t base_offset, Output_File *fl, WN_MAP off_map);
#endif

extern Elf64_Word
ir_b_write_dst (DST_TYPE dst, off_t base_offset, Output_File *fl);


extern Elf64_Word
ir_b_write_global_symtab (off_t base_offset, Output_File *fl);

extern Elf64_Word
ir_b_write_local_symtab (const SCOPE& pu, off_t base_offset, Output_File *fl);


extern void
IPA_irb_write_summary(Output_File *fl);

extern void
IPA_irb_write_mod_ref_info(Output_File *);

#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* __IR_BCOM_H__ */
