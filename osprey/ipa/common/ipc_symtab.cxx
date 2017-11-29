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


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>		// for WHIRL_REVISION

#include "linker.h"			// interface exported by ld
#include "read.h"			// for read_one_section
#include "ipc_weak.h"

#include "defs.h"
#include "cxx_memory.h"			// needed by ipc_symtab_merge.h
#include "erglob.h"			// error codes
#include "symtab.h"			// symbol table

#include "ld_ipa_interface.h"		// for interface routine
#include "ipc_link.h"			// for ipa_insert_whirl_obj_marker
#include "dwarf_DST_mem.h"              // Needed by ipc_file.h
#include "ipc_file.h"			// for IP_FILE_HDR
#include "ipc_symtab_merge.h"
#include "ipc_main.h"			// for ipa_dot_so_init
#include "ir_bread.h"                   // low-level WHIRL I/O routines

static void
get_global_symtab (IPC_GLOBAL_TABS &gtabs, FILE_INFO &finfo,
		   an_object_file_ptr p_obj,
		   const GLOBAL_SYMTAB_HEADER_TABLE *gsymtab)
{
    // Get references to the global symbol table subtables and initialize
    // "gtabs" and "finfo" accordingly.
    //

    gtabs.p_obj = p_obj;

    for (UINT i = 0; i < GLOBAL_SYMTAB_TABLES; ++i) {
	const SYMTAB_HEADER &hdr = gsymtab->header[i];
	char                *addr = (char *)gsymtab + hdr.offset;
       
	switch (hdr.type) {

	case SHDR_FILE:
	    finfo = *((FILE_INFO *) addr);
	    break;

	case SHDR_ST:
	    gtabs.st_tab_size = hdr.size / hdr.entsize;
	    gtabs.st_tab = (ST *)addr;
	    break;

	case SHDR_TY:
	    gtabs.ty_tab_size = hdr.size / hdr.entsize;
	    gtabs.ty_tab = (TY *)addr;
	    break;

	case SHDR_PU:
	    gtabs.pu_tab_size = hdr.size / hdr.entsize;
	    gtabs.pu_tab = (PU *) addr;
	    break;

	case SHDR_FLD:
	    gtabs.fld_tab_size = hdr.size / hdr.entsize;
	    gtabs.fld_tab = (FLD *) addr;
	    break;

	case SHDR_ARB:
	    gtabs.arb_tab_size = hdr.size / hdr.entsize;
	    gtabs.arb_tab = (ARB *) addr;
	    break;

	case SHDR_TYLIST:
	    gtabs.tylist_tab_size = hdr.size / hdr.entsize;
	    gtabs.tylist_tab = (TYLIST *) addr;
	    break;

	case SHDR_TCON:
	    gtabs.tcon_tab_size = hdr.size / hdr.entsize;
	    gtabs.tcon_tab = (TCON *) addr;
	    break;

	case SHDR_STR:
	    gtabs.tconstr_tab_size = hdr.size;
	    gtabs.tconstr_tab = addr;
	    break;

	case SHDR_INITO:
	    gtabs.inito_tab_size = hdr.size / hdr.entsize;
	    gtabs.inito_tab = (INITO *) addr;
	    break;

	case SHDR_INITV:
	    gtabs.initv_tab_size = hdr.size / hdr.entsize;
	    gtabs.initv_tab = (INITV *) addr;
	    break;

	case SHDR_ST_ATTR:
	    gtabs.st_attr_tab_size = hdr.size / hdr.entsize;
	    gtabs.st_attr_tab = (ST_ATTR *) addr;
	    break;
	} // switch on table type
    } // For each symtab table
} // get_global_symtab

#define DEFAULT_MAX_EXT_NUM	(1024)

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

inline BOOL 
ST_is_weak_alias(const ST &st)
{
   return (ST_is_weak_symbol(st) && ST_strong_idx(st) != ST_st_idx(st));
}


inline const ST &
ST_alias_base(const ST &st, const IPC_GLOBAL_TABS &gtabs)
{
   return gtabs.st_tab[ST_IDX_index(ST_strong_idx(st))];
}

template <class Sym>
static void
enter_st (const IPC_GLOBAL_TABS &gtabs, const ST &st, Sym &esym)
{
    INT type, binding;

    esym.st_name = ST_name_idx (st); // Offset into string-table

    ST_SCLASS sclass = ST_storage_class(st);

    esym.st_value = (sclass == SCLASS_EXTERN) ? 0 : ST_ofst (st);
    esym.st_size = 0;

    if (ST_is_weak_alias (st)) {
	const ST &alias_base = ST_alias_base(st, gtabs);
	sclass = ST_storage_class(alias_base);
	esym.st_value = (sclass == SCLASS_EXTERN) ? 0 : ST_ofst (alias_base);
	binding = STB_WEAK;
    } else if (ST_is_weak_symbol (st))
	binding = STB_WEAK;
    else
	binding = STB_GLOBAL;
    
    switch (ST_sym_class (st)) {
    case CLASS_VAR:
	type = STT_OBJECT;
	esym.st_size = TY_size (gtabs.ty_tab[TY_IDX_index (ST_type (st))]);
	switch (sclass) {
	case SCLASS_EXTERN:
	    esym.st_shndx = SHN_UNDEF;
	    break;
	case SCLASS_COMMON:
	    esym.st_shndx = SHN_COMMON;
	    esym.st_value = TY_align (ST_type (st));
	    break;
	default:
	    esym.st_shndx = SHN_MIPS_DATA;
	    break;
	} 
	break;
    case CLASS_FUNC:
	type = STT_FUNC;
	esym.st_shndx = (sclass == SCLASS_EXTERN) ? SHN_UNDEF : SHN_MIPS_TEXT;
	break;
    default:
	type = STT_NOTYPE;
	esym.st_shndx = (sclass == SCLASS_EXTERN) ? SHN_UNDEF : SHN_MIPS_DATA;
	break;
    }

    esym.st_info = ELF_ST_INFO (binding, type);

    switch (ST_export(st)) {
    case EXPORT_INTERNAL:
	esym.st_other = STO_INTERNAL;
	break;
    case EXPORT_HIDDEN:
	esym.st_other = STO_HIDDEN;
	break;
    case EXPORT_PROTECTED:
	esym.st_other = STO_PROTECTED;
	break;
    case EXPORT_OPTIONAL:
	esym.st_other = STO_OPTIONAL;
	break;
    case EXPORT_PREEMPTIBLE:
    default:
	esym.st_other = STO_DEFAULT;
	break;
    }

} /* enter_st */


template <class Sym>
static pair<Sym *, UINT>
walk_st_list (const IPC_GLOBAL_TABS& gtabs, const Sym*)
{
    UINT max_ext_num = DEFAULT_MAX_EXT_NUM;
    UINT size = 0;
    Sym *ext_symtab = (Sym *)
	MEM_POOL_Alloc (Malloc_Mem_Pool, sizeof(Sym) * max_ext_num);
    
    for (UINT32 idx = 1; idx < gtabs.st_tab_size; idx++) {
	const ST &st = gtabs.st_tab[idx];
       
	if (ST_export(st) != EXPORT_LOCAL &&
	    ST_export(st) != EXPORT_LOCAL_INTERNAL) {

	    if (size >= max_ext_num) {
		ext_symtab = (Sym *)
		    MEM_POOL_Realloc (Malloc_Mem_Pool, ext_symtab,
				      sizeof(Sym) * max_ext_num,
				      sizeof(Sym) * max_ext_num * 2);
		max_ext_num *= 2;
	    }
	    enter_st(gtabs, st, ext_symtab[size++]);
	}
    }
    return pair<Sym *, UINT> (ext_symtab, size);
} /* walk_st_list */


static void
check_revision (const char *base, UINT64 sh_size, const char* file_name)
{
    const char* eob;
    static const char *revision = WHIRL_REVISION;
    int len = strlen (revision);
    
    const char* p = base;
    eob = p + sh_size - 1;

    if (*eob != 0)
	/* section not NULL-terminated */
	ErrMsg (EC_IR_Scn_Read, ".comment", file_name);

    while (p <= eob) {
	if (strncmp (p, revision, len) == 0)
	    return;
	p += strlen (p) + 1;
    }

    ErrMsg (EC_IR_Revision, base, file_name);
} /* check_revision */




template <class Shdr, class Sym>
void
process_whirl (an_object_file_ptr p_obj, int nsec, const Shdr* section_table,
	       BOOL check_whirl_revision, const char* file_name,
	       const Sym* elf_symtab, off_t mmap_size) 
{
    // Given a WHIRL file, this routine will merge the global symbol-table
    // in with the current merged global symbol table which should already
    // have been set up as part of the SCOPE array.  We also check that
    // file format and revision is as expected.
    //
    // We return a map from old indices into the global symbol table to
    // to new indices into the merged global symbol table.  Note that the
    // IPC_GLOBAL_IDX_MAP structure will be allocated in a Malloc_Mem_Pool,
    // and must be freed up by the caller of this routine when appropriate.
    //
    // TODO: this procedure used to also set p_obj->est_const_size, which it
    // no longer does.  If necessary, this ought to be calculated by walking
    // the INITO list (Inito_Table for per-file, the merged INITO table
    // for a total number).
    
    int                    i;
    int                    gsymtab_idx = 0;
    int                    strtab_idx = 0;
    int                    found_summary = 0;
    int                    pu_section_idx = 0;

    if (ld_ipa_opt[LD_IPA_HIDES].flag == HS_DEFAULT)
	ld_ipa_opt[LD_IPA_HIDES].flag = HS_EXPORTS;

    for (i = 1; i < nsec; i++) {
	if (check_whirl_revision &&
	    section_table[i].sh_type == SHT_PROGBITS &&
	    strcmp (ld_get_section_name (p_obj, i), ELF_COMMENT) == 0) {

	    read_one_section (i, p_obj);
	    check_revision (ld_get_section_base (p_obj, i),
			    section_table[i].sh_size, file_name);
	} 
	
	if (section_table[i].sh_type != SHT_MIPS_WHIRL)
	    continue;

	switch (section_table[i].sh_info) {
	case WT_GLOBALS:
	    gsymtab_idx = i;
	    break;
	case WT_STRTAB:
	    strtab_idx = i;
	    break;
	case WT_IPA_SUMMARY:
	    found_summary = 1;
	    break;
	case WT_PU_SECTION:
	    pu_section_idx = i;
	    break;
	default:
	    break;
	}
    }

    if (gsymtab_idx == 0 || strtab_idx == 0 || pu_section_idx == 0)
	ErrMsg (EC_IR_Scn_Read, "symbol table or pu_info", file_name);

    if (!found_summary)
	ErrMsg (EC_IR_Scn_Read, "summary info", file_name);

    Elf64_Word gsymtab_size = section_table[gsymtab_idx].sh_size;
    const GLOBAL_SYMTAB_HEADER_TABLE *gsymtab =
       (GLOBAL_SYMTAB_HEADER_TABLE *) ld_get_section_base (p_obj, gsymtab_idx);

    if (gsymtab->size < sizeof(gsymtab) ||
	gsymtab->size > gsymtab_size    ||
	gsymtab->entries < GLOBAL_SYMTAB_TABLES) {
	ErrMsg (EC_IR_Scn_Read, "global symbol table", file_name);
    }

    for (i = 0; i < GLOBAL_SYMTAB_TABLES; ++i) {
	const SYMTAB_HEADER &hdr = gsymtab->header[i];
	if (hdr.offset + hdr.size > gsymtab_size)
	    ErrMsg (EC_IR_Scn_Read, "global symbol table", file_name);
    }

    IP_FILE_HDR &file_header =
	Setup_File_Header (file_name, ld_get_mmap_addr (p_obj), mmap_size);
 
    // Get all tables that make up the global symbol table, as well as
    // the FILE_INFO record for this WHIRL file.  
    //
    // Extract the external symbols from the WHIRL symbol table and convert
    // them into Elf symbol table format.  We no longer need to search local
    // symbol tables for global symbols, such as COMMONs, since these will 
    // now only be entered in the global symbol table with our new symbol 
    // table format.
    //
    IPC_GLOBAL_TABS gtabs;

    gtabs.symstr_tab_size = ld_get_section_size (p_obj, strtab_idx);
    gtabs.symstr_tab = (char*) ld_get_section_base (p_obj, strtab_idx);
    get_global_symtab (gtabs, IP_FILE_HDR_file_info (file_header), p_obj,
		       gsymtab); 
#if !defined(TARG_IA64) && !defined(TARG_X8664) && !defined(TARG_MIPS) && !defined(TARG_SL)&& !defined(TARG_LOONGSON)
    // Merge the new ELF symbol table entries with the existing ones.
    pair<Sym *, UINT> ext_symtab = walk_st_list (gtabs, elf_symtab);

    if (ext_symtab.second  > 0) {
    	an_elf_sym_record *p_sym = (an_elf_sym_record *)ext_symtab.first;
	merge_ext (p_sym,
		   ld_get_section_base (p_obj, strtab_idx),
		   ext_symtab.second, p_obj);
    }

    MEM_POOL_FREE (Malloc_Mem_Pool, ext_symtab.first);
#endif

    // Merge the new global WHIRL symbol table in with current global
    // WHIRL symbol table.  Note that we create a temporary version of
    // an LD_INTERFACE object, which will be used to access any LD 
    // datastructures in IPC_merge_global_tab().  This is because
    // an LD_INTERFACE object is not malloced until we do ipa_driver().
    // TODO: malloc the LD_INTERFACE object before processing a WHIRL file!
    //
    IPC_GLOBAL_IDX_MAP *idx_maps =
	IPC_merge_global_tab (gtabs, file_header,
			      IP_FILE_HDR_mem_pool(file_header));
    
    Set_IP_FILE_HDR_idx_maps (file_header, idx_maps);

    ipa_insert_whirl_obj_marker ();

} /* process_whirl */


static BOOL ipa_dot_so_initialized = FALSE;

#ifdef KEY
#include "ipc_defs.h"

 IP_TARGET_TYPE IPA_Target_Type;
#endif

extern "C" void
process_whirl32 (an_object_file_ptr p_obj, INT nsec,
		 const Elf32_Shdr* section_table,
		 BOOL check_whirl_revision, const char* file_name, off_t mmap_size) 
{
    if (!ipa_dot_so_initialized) {
#ifdef KEY
        IPA_Target_Type = IP_32_bit_ABI;
#endif
	ipa_dot_so_initialized = TRUE;
	ipa_dot_so_init ();
    }
    
    Elf32_Sym *tag = 0;
    process_whirl (p_obj, nsec, section_table, check_whirl_revision,
		   file_name, tag, mmap_size); 
}

extern "C" void
process_whirl64 (an_object_file_ptr p_obj, INT nsec,
		 const Elf64_Shdr* section_table,
		 BOOL check_whirl_revision, const char* file_name, off_t mmap_size)
{ 
    if (!ipa_dot_so_initialized) {
#ifdef KEY
        IPA_Target_Type = IP_64_bit_ABI;
#endif
	ipa_dot_so_initialized = TRUE;
	ipa_dot_so_init ();
    }

    Elf64_Sym *tag = 0;
    process_whirl (p_obj, nsec, section_table, check_whirl_revision,
		   file_name, tag, mmap_size); 
}

extern "C" void *
ipa_open_input(char *name, off_t *p_size)
{
    return WN_open_input(name,p_size);
}

#else // _STANDALONE_INLINER

#include "ir_bread.h"
void
Process_Global_Symtab (void* handle, IP_FILE_HDR& file_header)
{
 
    // Get all tables that make up the global symbol table, as well as
    // the FILE_INFO record for this WHIRL file.  
    //
    // Extract the external symbols from the WHIRL symbol table and convert
    // them into Elf symbol table format.  We no longer need to search local
    // symbol tables for global symbols, such as COMMONs, since these will 
    // now only be entered in the global symbol table with our new symbol 
    // table format.
    //
    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_GLOBALS);
    char *base = (char *) handle + shdr.offset;

    const GLOBAL_SYMTAB_HEADER_TABLE *gsymtab =
        (GLOBAL_SYMTAB_HEADER_TABLE *) base;

    IPC_GLOBAL_TABS gtabs;

    shdr = get_section (handle, SHT_MIPS_WHIRL, WT_STRTAB);
    base = (char *) handle + shdr.offset;
    gtabs.symstr_tab_size = shdr.size;
    gtabs.symstr_tab = base;
    get_global_symtab (gtabs, IP_FILE_HDR_file_info (file_header), NULL,
		       gsymtab); 

    // Merge the new global WHIRL symbol table in with current global
    // WHIRL symbol table.  Note that we create a temporary version of
    // an LD_INTERFACE object, which will be used to access any LD 
    // datastructures in IPC_merge_global_tab().  This is because
    // an LD_INTERFACE object is not malloced until we do ipa_driver().
    // TODO: malloc the LD_INTERFACE object before processing a WHIRL file!
    //
    IPC_GLOBAL_IDX_MAP *idx_maps =
	IPC_merge_global_tab (gtabs, file_header,
			      IP_FILE_HDR_mem_pool(file_header));
    
    Set_IP_FILE_HDR_idx_maps (file_header, idx_maps);

} /* Process_Global_Symtab */


#endif // _STANDALONE_INLINER
