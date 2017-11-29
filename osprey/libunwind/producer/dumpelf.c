/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



#include <stdlib.h>
#include <stdio.h>
#include <elf.h>
#include <sys/unwindP.h>

#define USE_STANDARD_TYPES 1
#include "defs.h"
#include <elf.h>
#include "elf_stuff.h"
#include "em_elf.h"



/* unwind table and/or unwind info dumping to elf */
/* convention: last arg is text elf section pointer */
__unw_error_t unwind_dump2elf(char *unwind_table_ptr,
					__uint64_t unwind_table_size,
					char *unwind_info_ptr,
					__uint64_t unwind_info_size,
					void *arg) {
	__uint64_t i, unwind_table_size_in_entries =
			unwind_table_size/sizeof(__unw_table_entry_t);
	__unw_table_entry_t *unwind_table =
		(__unw_table_entry_t *)unwind_table_ptr;
	pSCNINFO scninfo_table, scninfo_info, scninfo_text = (pSCNINFO)arg;
	Elf64_Word symindex_table, symindex_info, symindex_text;
	__unw_addr_t zero_offset = 0L;

	/* check that both pointers are valid */
	if ((NULL == unwind_table_ptr) || (NULL == unwind_info_ptr)) {
		return __UNW_INV_ARG_ERROR;
	}

	/* create elf sections */
	scninfo_table = Em_New_Section(IA64_UNWIND, SHT_IA64_UNWIND,
				SHF_ALLOC | SHF_IRIX_NOSTRIP,
				(Elf64_Xword)sizeof(__unw_table_entry_t),
				ELF64_FSZ_WORD);
	scninfo_info = Em_New_Section(IA64_UNWIND_INFO, SHT_IA64_UNWIND_INFO,
				SHF_ALLOC | SHF_IRIX_NOSTRIP,
				(Elf64_Xword)sizeof(__unw_dbl_word_t),
				ELF64_FSZ_XWORD);

	/* get symbol indices for the elf sections needed for relocations */
	symindex_table = Em_Create_Section_Symbol(scninfo_table);
	symindex_info = Em_Create_Section_Symbol(scninfo_info);
	symindex_text = Em_Create_Section_Symbol(scninfo_text);

	/* set up buffers for better performance */
	Em_New_Data_Buffer(scninfo_info, (Elf64_Xword)unwind_table_size,
								ELF64_FSZ_WORD);
	Em_New_Data_Buffer(scninfo_table, (Elf64_Xword)unwind_info_size,
								ELF64_FSZ_XWORD);

	/* dump unwind table */
	for (i = 0; i < unwind_table_size_in_entries; i++) {
		Em_Add_Bytes_To_Scn(scninfo_table, (void *)&zero_offset,
			(Elf64_Xword)sizeof(__unw_addr_t), ELF64_FSZ_WORD);
		Em_Add_New_Rela(symindex_text, (unsigned char)R_IA64_SEGREL64MSB,
			(Elf64_Addr)(i*sizeof(__unw_table_entry_t)),
			(Elf64_Sxword)unwind_table[i]._start,
			scninfo_table);
		Em_Add_Bytes_To_Scn(scninfo_table, (void *)&zero_offset,
			(Elf64_Xword)sizeof(__unw_addr_t), ELF64_FSZ_WORD);
		Em_Add_New_Rela(symindex_text, (unsigned char)R_IA64_SEGREL64MSB,
			(Elf64_Addr)(i*sizeof(__unw_table_entry_t) +
							1*sizeof(__unw_addr_t)),
			(Elf64_Sxword)unwind_table[i]._end,
			scninfo_table);
		Em_Add_Bytes_To_Scn(scninfo_table, (void *)&zero_offset,
			(Elf64_Xword)sizeof(__unw_addr_t), ELF64_FSZ_WORD);
		Em_Add_New_Rela(symindex_info, (unsigned char)R_IA64_SEGREL64MSB,
			(Elf64_Addr)(i*sizeof(__unw_table_entry_t) +
							2*sizeof(__unw_addr_t)),
			(Elf64_Sxword)unwind_table[i]._info,
			scninfo_table);
	}

	/* dump unwind info */
	Em_Add_Bytes_To_Scn(scninfo_info, (void *)unwind_info_ptr,
		(Elf64_Xword)unwind_info_size, ELF64_FSZ_XWORD);

	/* finalize elf sections */
	Em_End_Section(scninfo_table);
	Em_End_Section(scninfo_info);

	return __UNW_OK;
}
