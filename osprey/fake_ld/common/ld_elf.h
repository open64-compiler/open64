/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/fake_ld/common/ld_elf.h,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

#ifndef __LD_ELF_H__
#define __LD_ELF_H__

#include <elf.h>
#include "elfaccess.h"
#include "sys/elf_whirl.h"
#include "libelf/libelf.h"

#ifdef _64BIT_OBJECTS

#define an_elf_e_header Elf64_Ehdr			    /* Elf header */
#define an_elf_program_header Elf64_Phdr		    /* Program header */
#define an_elf_section_header Elf64_Shdr		    /* Section header */
#define an_elf_sym_record  Elf64_Sym			    /* Symbol table entry */
#define an_elf_rel_record  Elf64_Rel			    /* relocation records */
#define an_elf_rela_record Elf64_Rela			    /* relocation with addend */
#define an_elf_reginfo_record Elf64_RegInfo		    /* register info */
#define an_elf_gp_reginfo_record Elf64_GP_RegInfo
#define an_elf_dyn_record Elf64_Dyn			    /* .dynamic section */
#define an_elf_lib_record Elf64_Lib			    /* .liblist */
#define an_elf_conflict_record Elf64_Conflict		    /* .conflict list */
#define an_elf_interface_descriptor Elf_Interface_Descriptor /* .MIPS.interfaces */

#define ELF_ST_BIND ELF64_ST_BIND
#define ELF_ST_TYPE ELF64_ST_TYPE
#define ELF_ST_INFO ELF64_ST_INFO

#define ELF_MS_INFO ELF64_MS_INFO   /* set msym ms_info */

#undef ELF_AR_SYMTAB_NAME
#define ELF_AR_SYMTAB_NAME      "/SYM64/         "
#define ELF_OTHER_AR_NAME       "/               "

#define ELF_WORD Elf64_Word	    /* general type */

#else

#define an_elf_e_header Elf32_Ehdr		    /* Elf header */
#define an_elf_program_header Elf32_Phdr	    /* Program header */
#define an_elf_section_header Elf32_Shdr	    /* Section header */
#define an_elf_sym_record  Elf32_Sym		    /* Symbol table entry */
#define an_elf_rel_record  Elf32_Rel		    /* relocation records */
#define an_elf_rela_record Elf32_Rela		    /* relocation with addend */
#define an_elf_reginfo_record Elf32_RegInfo	    /* register info */
#define an_elf_gp_reginfo_record Elf32_GP_RegInfo
#define an_elf_dyn_record Elf32_Dyn		    /* .dynamic section */
#define an_elf_lib_record Elf32_Lib		    /* liblist */
#define an_elf_conflict_record Elf32_Conflict	    /* conflict list */
#define an_elf_interface_descriptor Elf_Interface_Descriptor /* .MIPS.interfaces */

#define ELF_ST_BIND ELF32_ST_BIND
#define ELF_ST_TYPE ELF32_ST_TYPE
#define ELF_ST_INFO ELF32_ST_INFO

#undef ELF_AR_SYMTAB_NAME
#define ELF_AR_SYMTAB_NAME      "/               "
#define ELF_OTHER_AR_NAME       "/SYM64/         "

#define ELF_MS_INFO ELF32_MS_INFO   /* set msym ms_info */

#define ELF_WORD Elf32_Word	    /* general type */

#endif /*_64BIT_OBJECTS */

#define ELF_AR_STRING_NAME      "//              "
#undef ELF_MSYM
#define ELF_MSYM Elf32_Msym         /* Msym structure */

#endif /* __LD_ELF_H__ */
