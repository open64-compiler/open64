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


// convert a WHIRL symbol table into Elf

#include "elf_stuff.h"
#include <sys/types.h>

#include "defs.h"
#include "symtab.h"
#include "pu_info.h"
#include "opcode.h"			// needed by wn_map.h
#include "wn.h"				// needed by wn_map.h
#include "wn_map.h"			// needed by ir_bwrite.h
#include "ir_elf.h"
#include "ir_bwrite.h"

#include "ipl_main.h"			// for IPL_Generate_Elf_Symtab
template <class ELF>
static void
ST_to_Elfsym (const ST* st, typename ELF::Elf_Sym& elfsym, const ELF& tag)
{
    elfsym.st_name = ST_name_idx (st);

    ST_SCLASS sclass = ST_sclass(st);

    elfsym.st_value = (sclass == SCLASS_EXTERN) ? 0 : ST_ofst (st);
    elfsym.st_size = 0;

    INT binding;
    
    if (ST_is_weak_symbol (st)) {
	binding = STB_WEAK;
	if (ST_strong_idx (*st) != ST_st_idx (st)) {
	    const ST &alias_base = St_Table[ST_strong_idx (*st)];
	    sclass = ST_storage_class(alias_base);
	    elfsym.st_value =
		(sclass == SCLASS_EXTERN) ? 0 : ST_ofst (alias_base);
	}
    } else
	binding = STB_GLOBAL;
    
    INT type;
    
    switch (ST_sym_class (st)) {
    case CLASS_VAR:
	type = STT_OBJECT;
	elfsym.st_size = TY_size (ST_type (st));
	switch (sclass) {
	case SCLASS_EXTERN:
	    elfsym.st_shndx = SHN_UNDEF;
	    break;
	case SCLASS_COMMON:
	    elfsym.st_shndx = SHN_COMMON;
	    elfsym.st_value = TY_align (ST_type (st));
	    break;
	default:
	    elfsym.st_shndx = SHN_MIPS_DATA;
	    break;
	} 
	break;
    case CLASS_FUNC:
	type = STT_FUNC;
	elfsym.st_shndx = (sclass == SCLASS_EXTERN) ? SHN_UNDEF : SHN_MIPS_TEXT;
	break;
    default:
	type = STT_NOTYPE;
	elfsym.st_shndx = (sclass == SCLASS_EXTERN) ? SHN_UNDEF : SHN_MIPS_DATA;
	break;
    }

    elfsym.st_info = tag.Elf_st_info (binding, type);

    switch (ST_export(st)) {
    case EXPORT_INTERNAL:
	elfsym.st_other = STO_INTERNAL;
	break;
    case EXPORT_HIDDEN:
	elfsym.st_other = STO_HIDDEN;
	break;
    case EXPORT_PROTECTED:
	elfsym.st_other = STO_PROTECTED;
	break;
    case EXPORT_OPTIONAL:
	elfsym.st_other = STO_OPTIONAL;
	break;
    case EXPORT_PREEMPTIBLE:
    default:
	elfsym.st_other = STO_DEFAULT;
	break;
    }
} // ST_to_Elfsym

namespace
{
    template <class Sym>
    struct Elf_Symtab {
	Sym* table;			// Elf symtab to be returned
	UINT size;			// number of entries
	UINT max_size;			// size of buffer

	Elf_Symtab (UINT n) : size (1), max_size (n+1) {
	    table = (Sym*) MEM_POOL_Alloc (Malloc_Mem_Pool,
					   (n+1) * sizeof(Sym));
	    bzero (table, sizeof(Sym));	// first entry always zero
	}

	~Elf_Symtab () {
	    MEM_POOL_FREE (Malloc_Mem_Pool, table);
	}
    };

    template <class SYMTAB, class ELF>
    struct st_to_elfsym {
	SYMTAB* symtab;
	const ELF& tag;

	st_to_elfsym (SYMTAB* sym, const ELF& t) : symtab (sym), tag (t) {}

	void operator() (UINT, const ST* st) const {
	    if (ST_export(st) != EXPORT_LOCAL &&
		ST_export(st) != EXPORT_LOCAL_INTERNAL) {
		ST_to_Elfsym (st, symtab->table[symtab->size++], tag);
	    }
	}
    };
     
}


// walk through the global symbol table, and convert each non-local entry
// to Elf symtab format, then write it out.
template <class ELF>
static inline void
Write_Elf_Symtab (Output_File* fl, const ELF& tag)
{
    typedef Elf_Symtab<typename ELF::Elf_Sym> ELFSYMTAB;
    ELFSYMTAB symtab (ST_Table_Size (GLOBAL_SYMTAB));

    For_all (St_Table, GLOBAL_SYMTAB,
	     st_to_elfsym<ELFSYMTAB, ELF> (&symtab, tag));

#ifndef __GNUC__
    UINT align = __builtin_alignof(typename ELF::Elf_Sym);
#else
    UINT align = __alignof(typename ELF::Elf_Sym);
#endif
    

    WN_write_elf_symtab (symtab.table,
			 symtab.size * sizeof(typename ELF::Elf_Sym),
			 sizeof(typename ELF::Elf_Sym), align, fl);
}

#if defined(__linux__) || defined(BUILD_OS_DARWIN)
void
IPL_Write_Elf_Symtab (Output_File *fl)
{
    if (! IPL_Generate_Elf_Symtab)
	return;
    
#ifndef __ALWAYS_USE_64BIT_ELF__
    if (Use_32_Bit_Pointers)
	Write_Elf_Symtab (fl, ELF32());
    else
#endif
	Write_Elf_Symtab (fl, ELF64());
}
#endif // __linux__
