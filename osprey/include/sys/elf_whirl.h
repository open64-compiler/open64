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


#ifndef __SYS_ELF_WHIRL_H__
#define __SYS_ELF_WHIRL_H__

/* $Header$ */

/* Elf extension for WHIRL object files */

/* Revision Number: Should be incremented whenever INCOMPATIBLE changes are
   made.  This string is put in the ".comment" section.  Refer to ELF
   object file spec. for format */
#if defined(TARG_IA64) && !defined(__ia64)
#define WHIRL_REVISION	"WHIRL::0.33:IA64X"
#else
#define WHIRL_REVISION	"WHIRL::0.33:"
#endif

/*
 * sh_info field for section of type SHT_MIPS_WHIRL 
 */

#define WT_NULL		0x0
#define WT_PU_SECTION	0x1	    /* all PU-specific information */
#define WT_GLOBALS	0x2	    /* WHIRL global symbol table */
#define WT_COMP_FLAGS	0x3	    /* compilation flags for this object */
#define WT_STRTAB	0x4	    /* WHIRL string table */
#define WT_CONSTAB	0x5	    /* WHIRL constant table */
#define WT_IPA_SUMMARY	0x6	    /* IPA summary information */
#define WT_DST		0x7	    /* WHIRL Debug Symbol Table */
#define WT_LOCALMAP	0x8	    /* IPAA local map */
#if defined(TARG_SL) || defined(TARG_MIPS)
#define WT_CALLGRAPH    0x9	/* Interrupt Service Routine register allocation info */ 
#endif

/*
 * Special WHIRL section names.
 */
#define MIPS_WHIRL_PU_SECTION	".WHIRL.pu_section"
#define MIPS_WHIRL_GLOBALS	".WHIRL.globals"
#define MIPS_WHIRL_COMP_FLAGS	".WHIRL.flags"
#define MIPS_WHIRL_STRTAB	".WHIRL.strtab"
#define MIPS_WHIRL_CONSTAB	".WHIRL.constab"
#define MIPS_WHIRL_SUMMARY	".WHIRL.summary"
#define MIPS_WHIRL_DST		".WHIRL.dst"
#define MIPS_WHIRL_LOCALMAP	".WHIRL.localmap"
#if defined(TARG_SL)
#define MIPS_WHIRL_CALLGRAPH    ".WHIRL.callgraph"
#endif

#endif /* __SYS_ELF_WHIRL_H__ */
