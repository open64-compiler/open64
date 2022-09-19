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


#define DWARF_TARGET_FLAG	DW_DLC_ISA_IA64

// Define all instruction bytes that go into a CIE.
// DW_CFA_def_cfa 
//	- opcode is 0x0c
//     	- register number 4 (x86) / number 7 (x86-64)
//	- offset 4 (x86) / offset 8 (x86-64)
// DW_CFA_offset
//	- opcode computed as 
//		0x80 + 0x8 (x86) or 0x80 + 0x10 (x86-64)
// 	- offset = 1
#define TARG_INIT_BYTES	{ 0x0c,  0x7, 0x8, 0x90, 0x1 }
#define TARG_INIT_X86_BYTES { 0x0c,  0x4, 0x4, 0x88, 0x1 }

typedef void (*Cg_Dwarf_Sym_To_Elfsym_Ofst)(Dwarf_Unsigned,
					    Dwarf_Unsigned *,
					    Dwarf_Unsigned *);

typedef Dwarf_Unsigned (*symbol_index_recorder)(Elf64_Word);

typedef int            (*advancer_to_next_stream)(Dwarf_Signed);
typedef Dwarf_Ptr      (*next_buffer_retriever)(void);
typedef Dwarf_Unsigned (*next_bufsize_retriever)(void);

Dwarf_Ptr Em_Dwarf_Symbolic_Relocs_To_Elf(next_buffer_retriever,
					  next_bufsize_retriever,
					  advancer_to_next_stream,
					  Dwarf_Signed,
					  Dwarf_Relocation_Data,
					  Dwarf_Unsigned,
					  int,
					  Cg_Dwarf_Sym_To_Elfsym_Ofst,
					  Dwarf_Unsigned *);
