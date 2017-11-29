/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



#ifndef cgemit_INCLUDED
#define cgemit_INCLUDED

#ifdef _KEEP_RCS_ID
static char *cgemit_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cgemit.h $ $Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

#include "dwarf_DST_mem.h"
#include "symtab.h"

/* Initialize for emitting of a source file: */
extern void EMT_Begin_File (
  char *process_name,	/* Back end process name */
  char *options 	/* Option string used for compilation */
);

/* Finalize for emitting of a source file: */
extern void EMT_End_File ( void );

/* Emit the contents of a PU: */
extern void EMT_Emit_PU ( ST *pu, DST_IDX pu_dst, WN *rwn);

/* put symbol in elf symbol table */ 
#if defined(BUILD_OS_DARWIN)
/* Generally, global-scope symbols require a preceding underscore
 * Calls to external functions require a jump to a stub
 * Other references to external functions require an indirect pointer
 */
typedef enum { DO_UNDERSCORE, DO_STUB, DO_NON_LAZY_PTR } darwin_indirect_t;
extern mINT32 EMT_Put_Elf_Symbol (ST *sym,
  darwin_indirect_t indirect = DO_UNDERSCORE);
#else
extern mINT32 EMT_Put_Elf_Symbol (ST *sym);
#endif /* defined(BUILD_OS_DARWIN) */

/* change existing elf symbol to undefined */
extern void EMT_Change_Symbol_To_Undefined (ST *sym);

/* change existing elf symbol to weak binding */
extern void EMT_Change_Symbol_To_Weak (ST *sym);

/*
 * Check if bb should be aligned,
 * and return number of instructions it should be aligned with.
 */
extern INT Check_If_Should_Align_BB (BB *bb, INT32 curpc);

extern BOOL CG_emit_asm_dwarf;
extern BOOL CG_emit_unwind_info;
extern BOOL CG_emit_unwind_directives;
#ifdef TARG_X8664
extern BOOL CG_emit_unwind_info_Set;
#endif
#ifdef KEY
extern BOOL CG_emit_non_gas_syntax;
extern BOOL CG_inhibit_size_directive;
#endif

/* Write a qualified name to a file. */
extern void EMT_Write_Qualified_Name (FILE *f, ST *st);

#if defined(BUILD_OS_DARWIN)
/* Return log base two of an integer (used for .align) */
extern int logtwo(int value);
extern const char *map_section_name(const char *section_name);
extern int is_debug_section(const char *section_name);
#endif /* defined(BUILD_OS_DARWIN) */

#endif /* cgemit_INCLUDED */
