/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#define DWARF_TARGET_FLAG	DW_DLC_ISA_IA64

#define TARG_INIT_BYTES	{ 0 }

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
