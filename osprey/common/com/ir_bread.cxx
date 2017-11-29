/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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



#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <unistd.h>		    /* for close(), etc. */
#include <sys/stat.h>		    /* for fstat() */
#ifdef __MINGW32__
#include <WINDOWS.h>
#else
#include <sys/mman.h>		    /* for mmap() */
#endif
#include <fcntl.h>		    /* for open() */
#include "elf_stuff.h"		    /* for all Elf stuff */
#include <sys/elf_whirl.h>	    /* for WHIRL sections */
#include <errno.h>		    /* for error code */

#define USE_STANDARD_TYPES	    /* override unwanted defines in "defs.h" */

#include "defs.h"		    /* for wn_core.h */
#ifdef OWN_ERROR_PACKAGE
/* Turn off assertion in the opcode handling routines, we assume the tree
 * is clean.  Also, this removes the dependency on "errors.h", which is not
 * used by all programs.
 */
#define Is_True(x,y) (0)
#define ERRORS_INCLUDED
#else
#include "erglob.h"
#endif
#include "errors.h"
#include "opcode.h"		    /* for wn_core.h */
#include "mempool.h"		    /* for MEM_POOL (for maps) */
#include "strtab.h"		    /* for strtab */
#include "symtab.h"		    /* for symtab */
#include "const.h"		    /* for constab */
#include "targ_const.h"		    /* for tcon */
#include "config_targ.h"	    /* for Target_ABI */
#include "config_debug.h"	    /* for ir_version_check */
#include "config_elf_targ.h"
#include "irbdata.h"		    /* for init_data */
#include "wn_core.h"		    /* for WN */
#include "wn.h"		            /* for max_region_id */
#include "wn_map.h"		    /* for WN maps */

#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"	    /* for dst */
#include "pu_info.h"
#include "ir_elf.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "ir_bread.h"
#include "config_opt.h"

#if defined(BACK_END)
#include "xstats.h"
#include "ipa_be_read.h"
#endif
#if defined(BACK_END) || defined(BUILD_WHIRL2C) || defined(BUILD_WHIRL2F)
#include "pf_cg.h"
#endif

#if defined(BACK_END) || defined(IR_TOOLS) || defined(BUILD_WHIRL2C) || defined(BUILD_WHIRL2F)
#include "wssa_io.h"
#endif

#ifdef BACK_END
extern "C" {
extern void Init_Dep_Graph(void *g);
extern void Dealloc_Dep_Graph(void);
extern void *Depgraph_Read(char *cur_addr, char *end_addr, char *tree_base);
}
#endif /* BACK_END */

static BOOL verbose_info = FALSE;
static WN *last_node = NULL;

static off_t global_mapped_size;
static off_t local_mapped_size;

static char file_revision[80];	/* save revision string */

BOOL Read_ALIAS_CGNODE_Map = FALSE;

#define DOUBLE_ALIGNED(sz)	(((sz) % 8) == 0 ? (sz) : (sz)+(8-((sz)%8)))
#define ERROR_VALUE -1
/* should be under Is_True_On, but we don't use that for ir_reader,
 * so add another define */
#if defined(Is_True_On) || defined(DEBUG_IR)
/* to make debugging easier, have all error returns go through this routine */
static INT
Error_Return_Func (void)
{
    if (verbose_info && last_node != NULL) {
	printf("last_node at %p, opcode = %s\n",
	       last_node, OPCODE_name(WN_opcode(last_node)));
    }
    return ERROR_VALUE;
}

#define ERROR_RETURN Error_Return_Func()
#else
#define ERROR_RETURN ERROR_VALUE
#endif


template <class Shdr>
static const Shdr*
get_section_header (const Shdr* shdr, UINT n, Elf64_Word type, Elf64_Word info)
{
    for (INT i = 1; i < n; ++i) {
	if (shdr[i].sh_type == type && shdr[i].sh_info == info)
	    return shdr + i;
    }
    
    return NULL;
}

OFFSET_AND_SIZE
get_section (void *handle, Elf64_Word type, Elf64_Word info)
{
    if (handle == 0 || handle == (void *)(ERROR_VALUE)) {
	errno = EINVAL;
	return OFFSET_AND_SIZE (0, 0);
    }

    errno = 0;

    Elf64_Ehdr *eh = (Elf64_Ehdr *) handle;

#ifndef __ALWAYS_USE_64BIT_ELF__
    if (eh->e_ident[EI_CLASS] == ELFCLASS32) {
	Elf32_Ehdr* ehdr = (Elf32_Ehdr*) handle;
	const Elf32_Shdr* shdr = (const Elf32_Shdr *)
	    ((char *) handle + ehdr->e_shoff);
	shdr = get_section_header (shdr, ehdr->e_shnum, type, info);
	if (!shdr)
	    return OFFSET_AND_SIZE (0, 0);
	return OFFSET_AND_SIZE (shdr->sh_offset, shdr->sh_size);
    } else
#endif
	{
	const Elf64_Shdr* shdr = (const Elf64_Shdr *)
	    ((char *) handle + eh->e_shoff);
	shdr = get_section_header (shdr, eh->e_shnum, type, info);
	if (!shdr)
	    return OFFSET_AND_SIZE (0, 0);
	return OFFSET_AND_SIZE (shdr->sh_offset, shdr->sh_size);
    }
} /* get_section */


/* If the CHECK_INPUT_FILES macro is defined, the binary reader tries to
   check for offsets that are outside the current (sub)section and for
   symbol table indices that are outside the valid ranges.  To make the
   reading process as fast as possible, you can leave CHECK_INPUT_FILES
   undefined. */

#if defined(Is_True_On) || defined(DEBUG_IR)
#define CHECK_INPUT_FILES
#endif

#ifdef CHECK_INPUT_FILES

#define CONVERT_OFFSET(typ, fld) \
    if ((INTPTR)(fld) >= size) return ERROR_RETURN; \
    else (fld) = (typ)(base + (INTPTR)(fld))

#else

#define CONVERT_OFFSET(typ, fld) \
    (fld) = (typ)(base + (INTPTR)(fld))

#endif // CHECK_INPUT_FILES


// read the global symbol table from the file and set up the various global
// symtab data structures.  try to reuse the mmap'ed memory buffer if
// possible (i.e., avoid copying). 

INT
WN_get_global_symtab (void *handle)
{

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_GLOBALS);
    if (shdr.offset == 0)
	return ERROR_RETURN;

    const char *base = (char *) handle + shdr.offset;

    const GLOBAL_SYMTAB_HEADER_TABLE *gsymtab =
	(GLOBAL_SYMTAB_HEADER_TABLE *) base;

    UINT64 size = shdr.size;

    if (gsymtab->size < sizeof(gsymtab) ||
	gsymtab->entries < GLOBAL_SYMTAB_TABLES || gsymtab->size > size)
	return ERROR_RETURN;

    UINT i;
    for (i = 0; i < GLOBAL_SYMTAB_TABLES; ++i)
	if (gsymtab->header[i].offset + gsymtab->header[i].size > size)
	    return ERROR_RETURN;

    for (i = 0; i < GLOBAL_SYMTAB_TABLES; ++i) {
	const SYMTAB_HEADER& hdr = gsymtab->header[i];
	const char *addr = base + hdr.offset;

	switch (hdr.type) {

	case SHDR_FILE:
	    File_info = *((FILE_INFO *) addr);
	    break;

	case SHDR_ST:
	    Scope_tab[GLOBAL_SYMTAB].st_tab->
		Transfer ((ST *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_TY:
	    Ty_tab.Transfer ((TY *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_PU:
	    Pu_Table.Transfer ((PU *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_FLD:
	    Fld_Table.Transfer ((FLD *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_ARB:
	    Arb_Table.Transfer ((ARB *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_TYLIST:
	    Tylist_Table.Transfer ((TYLIST *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_TCON:
	    Tcon_Table.Transfer ((TCON *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_STR:
	    Initialize_TCON_strtab (addr, hdr.size);
	    break;

	case SHDR_INITO:
	    Scope_tab[GLOBAL_SYMTAB].inito_tab->
		Transfer ((INITO *) addr, hdr.size / hdr.entsize);
	   break;

	case SHDR_INITV:
	    Initv_Table.Transfer ((INITV *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_BLK:
	    Blk_Table.Transfer ((BLK *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_ST_ATTR:
	    Scope_tab[GLOBAL_SYMTAB].st_attr_tab->
		Transfer ((ST_ATTR *) addr, hdr.size / hdr.entsize);
	   break;
	}
    }

    return 0;
} // WN_get_global_symtab


/* Given a handle returned by WN_open_input(), this function 
 * sets up the SCOPE structure.
 * Returns -1 if error, 0 if success.
 * Assume that global symtab and string table are already read.
 */

INT
WN_get_symtab (void *handle, PU_Info *pu)
{
    Subsect_State st = PU_Info_state(pu, WT_SYMTAB);
    if (st == Subsect_InMem)
	return 0;
    else if (st != Subsect_Exists)
	return ERROR_RETURN;

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
    if (shdr.offset == 0)
	return ERROR_RETURN;

    const char *base = (char *) handle + shdr.offset +
	PU_Info_subsect_offset (pu, WT_SYMTAB);
    Elf64_Word size = PU_Info_subsect_size(pu, WT_SYMTAB);
    const LOCAL_SYMTAB_HEADER_TABLE *lsymtab =
	(LOCAL_SYMTAB_HEADER_TABLE *) base;

    if (lsymtab->size < sizeof(lsymtab) ||
	lsymtab->entries < LOCAL_SYMTAB_TABLES || lsymtab->size > size) {
	errno = EINVAL;
	return ERROR_RETURN;
    }

    UINT i;
    for (i = 0; i < LOCAL_SYMTAB_TABLES; ++i)
	if (lsymtab->header[i].offset + lsymtab->header[i].size > size) {
	    errno = EINVAL;
	    return ERROR_RETURN;
	}

    for (i = 0; i < LOCAL_SYMTAB_TABLES; ++i) {
	const SYMTAB_HEADER& hdr = lsymtab->header[i];
	const char *addr = base + hdr.offset;

	switch (hdr.type) {

	case SHDR_ST:
            Is_True(Scope_tab[CURRENT_SYMTAB].st_tab != 0,
                    ("Null st_tab, level = %d", CURRENT_SYMTAB));
	    Scope_tab[CURRENT_SYMTAB].st_tab->
		Transfer ((ST *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_LABEL:
            Is_True(Scope_tab[CURRENT_SYMTAB].label_tab != 0,
                    ("Null label_tab, level = %d", CURRENT_SYMTAB));
	    Scope_tab[CURRENT_SYMTAB].label_tab->
		Transfer ((LABEL *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_PREG:
            Is_True(Scope_tab[CURRENT_SYMTAB].preg_tab != 0,
                    ("Null preg_tab, level = %d", CURRENT_SYMTAB));
	    Scope_tab[CURRENT_SYMTAB].preg_tab->
		Transfer ((PREG *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_INITO:
            Is_True(Scope_tab[CURRENT_SYMTAB].inito_tab != 0,
                    ("Null inito_tab, level = %d", CURRENT_SYMTAB));
	    Scope_tab[CURRENT_SYMTAB].inito_tab->
		Transfer ((INITO *) addr, hdr.size / hdr.entsize);
	    break;

	case SHDR_ST_ATTR:
            Is_True(Scope_tab[CURRENT_SYMTAB].st_attr_tab != 0,
                    ("Null st_attr_tab, level = %d", CURRENT_SYMTAB));
	    Scope_tab[CURRENT_SYMTAB].st_attr_tab->
		Transfer ((ST_ATTR *) addr, hdr.size / hdr.entsize);
	    break;
	}
    }

    Set_PU_Info_state(pu, WT_SYMTAB, Subsect_InMem);
 
    return 0;
} /* WN_get_symtab */


INT
WN_get_strtab (void *handle)
{
    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_STRTAB);
    if (shdr.offset == 0)
	return ERROR_RETURN;

    Initialize_Strtab ((char *) handle + shdr.offset, shdr.size);

    return 0;
} // WN_get_strtab

/*
 *  Note: get SSA info from file into memory 
 */
#ifdef Is_True_On
WSSA::WHIRL_SSA_MANAGER* G_ssa;
#endif

INT
WN_get_SSA (void *handle, PU_Info *pu, MEM_POOL* pool)
{
  WSSA::WHIRL_SSA_MANAGER* wssa_mgr;
  Subsect_State st = PU_Info_state(pu, WT_SSA);
  if (st == Subsect_InMem) {
    Is_True(PU_Info_ssa_ptr(pu) != NULL, ("WSSA manager is null"));
    return 0;
  }
  else if (st == Subsect_Written) {
    return ERROR_RETURN;
  }
  else if (st != Subsect_Exists) {
    wssa_mgr = new WSSA::WHIRL_SSA_MANAGER(pool);
#ifdef Is_True_On
    G_ssa = wssa_mgr;
    Is_True(PU_Info_ssa_ptr(pu) == NULL, ("WSSA manager is not null"));
#endif
    Set_PU_Info_ssa_ptr(pu, wssa_mgr);
    Set_PU_Info_state(pu, WT_SSA, Subsect_InMem);
    return 0;
  }

  OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
  if (shdr.offset == 0)
    return ERROR_RETURN;

  char *base = (char *) handle + shdr.offset +
  PU_Info_subsect_offset (pu, WT_SSA);

  wssa_mgr = PU_Info_ssa_ptr(pu);
  FmtAssert(wssa_mgr != NULL, ("WSSA manager is null"));

#ifdef Is_True_On
  G_ssa = wssa_mgr;
#endif

  //read PHI,CHI,MU tables into memory
  WSSA::WHIRL_SSA_IO wssa_io(wssa_mgr);
  wssa_io.Read_SSA_From_File(base);
  
  Set_PU_Info_state(pu, WT_SSA, Subsect_InMem);
}

/*
 *  Note: fix_tree is a hot spot for the binary reader, so be very careful
 *  to make everything as fast as possible.
 */

static INT
fix_tree (PU_Info* pu, WN *node, char *base, Elf64_Word size)
{
    OPCODE opcode = (OPCODE) WN_opcode (node);
    WN *wn;

#if defined(Is_True_On) || defined(DEBUG_IR)
    if (verbose_info) {
	printf("opcode %s\n", OPCODE_name(opcode));
	last_node = node;
    }
#endif
    
#ifndef CFE
    if (opcode == OPC_REGION)
	Set_Max_Region_Id (WN_region_id(node));
#endif
    
#if defined(BACK_END)
    if (opcode == OPC_ALTENTRY)
	Set_PU_has_altentry (Get_Current_PU ());
    
    /* Count whirl nodes that are useful for Olimit and other stats */
    Count_WN_Opcode (opcode, &PU_WN_BB_Cnt, &PU_WN_Stmt_Cnt);
#endif

    if (opcode == OPC_BLOCK) {
	wn = WN_first(node);
	if (wn == (WN *) -1) {
	    WN_first(node) = NULL;
	    WN_last(node) = NULL;
	} else {
	    CONVERT_OFFSET(WN*, wn);
	    WN_first(node) = wn;
	    CONVERT_OFFSET(WN*, WN_last(node));

	    do {
		if (fix_tree (pu, wn, base, size) == ERROR_VALUE)
		    return ERROR_VALUE;
		wn = WN_next(wn);
	    } while (wn);
	}
    } else if (!OPCODE_is_leaf(opcode)) {
	register INT i, cnt;
	register WN **wn_ptr;

	cnt = WN_kid_count(node);
	wn_ptr = &WN_kid(node, 0);
	for (i = 0; i < cnt; i++, wn_ptr++) {
	    wn = *wn_ptr;
	    if (wn == (WN *) -1) {
		*wn_ptr = NULL;
	    } else {
		CONVERT_OFFSET(WN*, wn);
		*wn_ptr = wn;
		if (fix_tree (pu, wn, base, size) == ERROR_VALUE)
    	            return ERROR_VALUE;
	    }
	}
    }
    
    if (OPCODE_has_next_prev(opcode)) {
	wn = WN_prev(node);
	if (wn == (WN *) -1) {
	    WN_prev(node) = NULL;
	} else {
	    CONVERT_OFFSET(WN*, wn);
	    WN_prev(node) = wn;
	}
	wn = WN_next(node);
	if (wn == (WN *) -1) {
	    WN_next(node) = NULL;
	} else {
	    CONVERT_OFFSET(WN*, wn);
	    WN_next(node) = wn;
	}
    }

    /* keep track of the last map ID in each opcode category */
    if (WN_map_id(node) != -1) {
	register OPERATOR_MAPCAT category = OPCODE_mapcat(opcode);
        register INT32 map_id = WN_map_id(node);
	register INT32 *last_id_ptr;

	last_id_ptr = &WN_MAP_TAB_Last_ID(Current_Map_Tab, category);
	if (map_id > *last_id_ptr) {
	    *last_id_ptr = map_id;
	}
    }

#if defined(BACK_END) || defined(IR_TOOLS)
    WSSA::WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(pu);
    if (WN_map_id(node) != -1 && mgr != NULL) {
        // create the map between WN* and map_id
        mgr->Add_wn(node);
    }
#endif

    return 0;
} /* fix_tree */



void
Set_Verbose_Info (BOOL val)
{
    verbose_info = val;
}


template <class ELF>
static INT
check_elf_header (char* baseaddr, Elf64_Word size, const ELF& tag)
{
    typename ELF::Elf_Ehdr* ehdr = (typename ELF::Elf_Ehdr*) baseaddr;
    if (ehdr->e_ident[EI_VERSION] != EV_CURRENT ||
	ehdr->e_version != EV_CURRENT)
	return ERROR_RETURN;
    if (ehdr->e_type != ET_IR ||
	ehdr->e_shentsize != sizeof(typename ELF::Elf_Shdr))
	return ERROR_RETURN;
    if (Target_ABI != ABI_UNDEF && 
	// only check if expecting a certain target
	ehdr->e_machine != Get_Elf_Target_Machine())
	return ABI_MISMATCH;
    BOOL is_64bit;
    INT isa;
    Config_Target_From_ELF (ehdr->e_flags, &is_64bit, &isa);
    if ( ! Set_Target_ABI (is_64bit, isa))
	return ABI_MISMATCH;
    if (ehdr->e_shstrndx >= ehdr->e_shnum)
	return ERROR_RETURN;
    if (ehdr->e_shoff >= size ||
	ehdr->e_shoff + ehdr->e_shnum * sizeof(typename ELF::Elf_Shdr) > size)
	return ERROR_RETURN;
    typename ELF::Elf_Shdr* shdr =
	(typename ELF::Elf_Shdr *) (baseaddr + ehdr->e_shoff);
    if ((long) shdr & (
#ifndef __GNUC__
		       __builtin_alignof(typename ELF::Elf_Shdr)
#else
		       __alignof__(typename ELF::Elf_Shdr)
#endif
		       - 1))
	return ERROR_RETURN;
    return tag.Elf_class();
}


static INT
check_elf_header (char *baseaddr, Elf64_Word size)
{
    if (size < sizeof(Elf64_Ehdr))
	return ERROR_RETURN;
    Elf64_Ehdr* ehdr = (Elf64_Ehdr *) baseaddr;
    
    if (!IS_ELF (*ehdr))
	return ERROR_RETURN;

    if (ehdr->e_ident[EI_CLASS] == ELFCLASS64)
	return check_elf_header (baseaddr, size, ELF64());
#ifndef __ALWAYS_USE_64BIT_ELF__
    else if (ehdr->e_ident[EI_CLASS] == ELFCLASS32)
	return check_elf_header (baseaddr, size, ELF32());
#endif
    else
	return ERROR_RETURN;
} /* check_elf_header */


template <class ELF>
static INT
check_section_headers (char *baseaddr, Elf64_Word size, char* file_revision, 
		       const ELF& tag) 
{
    typename ELF::Elf_Ehdr* ehdr = (typename ELF::Elf_Ehdr*) baseaddr;
    typename ELF::Elf_Shdr* shdr =
	(typename ELF::Elf_Shdr*) (baseaddr + ehdr->e_shoff);
	
    errno = ENOEXEC;
	
    if (shdr[ehdr->e_shstrndx].sh_offset >= size ||
	shdr[ehdr->e_shstrndx].sh_offset +
	shdr[ehdr->e_shstrndx].sh_size > size)
	return ERROR_RETURN;
    
    char* shstrtab = baseaddr + shdr[ehdr->e_shstrndx].sh_offset;

    for (INT i = 1; i < ehdr->e_shnum; i++) {
	if (shdr[i].sh_offset >= size ||
	    shdr[i].sh_offset + shdr[i].sh_size > size)
	    return ERROR_RETURN;
	if (shdr[i].sh_name >= shdr[ehdr->e_shstrndx].sh_size)
	    return ERROR_RETURN;
	if (shdr[i].sh_link >= ehdr->e_shnum)
	    return ERROR_RETURN;
	if (shdr[i].sh_addralign & (shdr[i].sh_addralign - 1))
	    return ERROR_RETURN; 
	if (shdr[i].sh_addralign > 1 &&
	    (long)(baseaddr + shdr[i].sh_offset) & ((shdr[i].sh_addralign - 1)))
	    return ERROR_RETURN;
	
	/* search for and verify the revision string */
	if (shdr[i].sh_type == SHT_PROGBITS &&
	    strcmp (shstrtab + shdr[i].sh_name, ELF_COMMENT) == 0) {
	    register char *p = baseaddr + shdr[i].sh_offset;
	    register char *eob = p + shdr[i].sh_size - 1;
	    register int match = 0;
		
	    if (*eob != 0)
		/* section not NULL-terminated */
		return ERROR_RETURN;
		
	    while (p <= eob) {
		if (strncmp ("WHIRL:", p, 6) == 0) {
		    strcpy (file_revision, p);
		    if (strcmp (WHIRL_REVISION, p) == 0) {
			match = 1;
			break;
		    }
		} 
		p += strlen (p) + 1;
	    }
		
	    if (!match && DEBUG_Ir_Version_Check)
		return REVISION_MISMATCH;
	}
    }

    errno = 0;
    return 1;
} /* check_section_headers */

static INT
fix_comp_flags (char *base, Elf64_Word size)
{
    register char **argv;
    register Elf64_Word argc;

    if (sizeof(Elf64_Word) > size)
	return ERROR_RETURN;

    argc = *((Elf64_Word *) base);

    if (sizeof(Elf64_Word) + argc * sizeof(Elf64_Word) > size)
	return ERROR_RETURN;
    argv = (char **) (base + sizeof(Elf64_Word));
    
    while (argc--) {
	register Elf64_Word offset = *((Elf64_Word *)argv);

	if (offset > size)
	    return ERROR_RETURN;
	*argv = base + offset;
	argv++;
    }

    return 0;
} /* fix_comp_flags */


INT
WN_massage_input (char *baseaddr, Elf64_Word size, char* file_revision)
{
    INT st;

    if ((st = check_elf_header (baseaddr, size)) < 0)
	return st;

#ifndef __ALWAYS_USE_64BIT_ELF__
    if (st == ELFCLASS32) {
	if ((st = check_section_headers (baseaddr, size, file_revision,
					 ELF32())) <= 0)
	    return st;
    } else
#endif
    {
	if ((st = check_section_headers (baseaddr, size, file_revision,
					 ELF64())) <= 0) 
	    return st;
    }

    return 1;
} /* WN_massage_input */


#ifdef __MINGW32__
static void *
read_file (char *filename, off_t* mapped_size, char* file_revision, int *ret_fd, HANDLE *retMapHd)
#else
static void *
read_file (char *filename, off_t* mapped_size, char* file_revision)
#endif /* __MINGW32__ */
{
    int fd;
    INT st;
    struct stat stat_buf;
    register char *map_addr;

    fd = open (filename, O_RDONLY);
    if (fd < 0)
	return (void *) (INTPTR) ERROR_RETURN;

    if (fstat (fd, &stat_buf) != 0)
	return (void *) (INTPTR) ERROR_RETURN;

#ifdef __MINGW32__
    *retMapHd = CreateFileMapping((HANDLE)_get_osfhandle(fd), NULL, 
                                  PAGE_READONLY, 0, stat_buf.st_size, filename);
    if (*retMapHd == NULL)
	return (void *) (INTPTR)ERROR_RETURN;
    map_addr = (char *)MapViewOfFileEx(*retMapHd, FILE_MAP_COPY,
                                       0,0,stat_buf.st_size, 0);
#else
    map_addr = (char *) mmap (0, stat_buf.st_size, PROT_READ|PROT_WRITE,
			      MAP_PRIVATE, fd, 0);
#endif
    if (map_addr == (char *)(INTPTR)(ERROR_VALUE)) {
	close (fd);
	return (void *) (INTPTR) ERROR_RETURN;
    }

#ifdef __MINGW32__
    CloseHandle((HANDLE)_get_osfhandle(fd));
    if (ret_fd) *ret_fd = fd;
#else
    close (fd);
#endif /* __MINGW32__ */

    if ((st = WN_massage_input (map_addr, stat_buf.st_size,file_revision)) <= 0) {
#ifdef __MINGW32__
        UnmapViewOfFile(map_addr);
        CloseHandle(*retMapHd);
#else
	munmap (map_addr, stat_buf.st_size);
#endif /* __MINGW32__ */
	return (void *) (INTPTR) (st);
    }

    /* if everything is fine, save the size of the file */ 
    *mapped_size = stat_buf.st_size;

    return map_addr;
    
} /* read_file */


Elf64_Word
Get_Elf_Section_Size (void *handle, Elf64_Word type, Elf64_Word info)
{
    OFFSET_AND_SIZE shdr = get_section (handle, type, info);
    if (shdr.offset == 0) return 0;
    return shdr.size;
}

/*
 * Specify the name of the WHIRL file for reading.  If successful, a handle
 * will be returned.  If the binary was a different revision than what we
 * expected, it returns (void*) REVISION_MISMATCH.
 * Otherwise, it returns (void *) READER_ERROR and sets errno.
 */

#ifdef __MINGW32__
void *
WN_open_input (char *filename, off_t *mapped_size, int *fd, HANDLE *mapHd)
#else
void *
WN_open_input (char *filename, off_t *mapped_size)
#endif /* __MINGW32__ */
{
    if (filename == 0) {
	errno = ENOENT;
	return (void *) (INTPTR) ERROR_RETURN;
    }

    errno = 0;

#ifdef __MINGW32__
    return read_file (filename, mapped_size, file_revision, fd, mapHd);
#else
    return read_file (filename, mapped_size, file_revision);
#endif /* __MINGW32__ */
    
} /* WN_open_input */


/* open a new file, used by the inliner when performing across
 * file inlining. Note, mapped file size is not saved in the
 * static variable.
 */
#ifdef __MINGW32__
extern void *
WN_inline_open_file(char* file_name, off_t *mapped_size, char* file_revision, HANDLE *mapHd)
#else
extern void *
WN_inline_open_file(char* file_name, off_t *mapped_size, char* file_revision)
#endif /* __MINGW32__ */
{
    if (file_name == 0) {
	errno = ENOENT;
	return (void *) (INTPTR) ERROR_RETURN;
    }

    errno = 0;

#ifdef __MINGW32__
    return read_file	 (file_name, mapped_size, file_revision, 0, mapHd);
#else
    return read_file (file_name, mapped_size, file_revision);
#endif /* __MINGW32__ */
      
}


/*
 * Read the PU headers.  Returns a pointer to the root of the PU_Info
 * tree or -1 on error.  Returns the number of PUs through the p_num_PUs
 * parameter.
 */

PU_Info *
WN_get_PU_Infos (void *handle, INT32 *p_num_PUs)
{
    char *base;
    INT32 size;
    PU_Info *pu_tree;

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
    if (shdr.offset == 0) return (PU_Info *)(INTPTR)ERROR_RETURN;

    base = (char *) handle + shdr.offset;
    size = shdr.size;

    pu_tree = Read_PU_Infos (base, size, p_num_PUs);
    if (pu_tree == (PU_Info *)ERROR_VALUE)
	return (PU_Info *)(INTPTR)ERROR_RETURN;

    return pu_tree;
}


#ifdef BACK_END
void *
WN_get_section_base (void *handle, INT sect)
{
    char *base;

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, sect);
    if (shdr.offset == 0) return (void *) (INTPTR)ERROR_RETURN;

    base = (char *) handle + shdr.offset;
    return (void *) base;
}
#endif


/*
 *  Convert the procedure ST in a PU_Info from an ID number to an
 *  ST pointer.
 */

ST *
WN_get_proc_sym (PU_Info *pu)
{
    Subsect_State st = PU_Info_state(pu, WT_PROC_SYM);

    if (st == Subsect_InMem)
	return &St_Table[PU_Info_proc_sym(pu)];
    if (st != Subsect_Exists)
	return (ST *) (INTPTR) ERROR_RETURN;
    ST *ps = &St_Table[PU_Info_proc_sym(pu)];
    if (ps == NULL)
	return (ST *) (INTPTR) ERROR_RETURN;
    
    Set_PU_Info_state(pu, WT_PROC_SYM, Subsect_InMem);

    return ps;
}


/*
 * Given a handle returned by WN_open_input() and a PU_Info structure,
 * this function reads the WHIRL tree for the PU.  It returns (WN *)(-1)
 * if there is an error.
 */

WN *
WN_get_tree (void *handle, PU_Info *pu)
{
    register char *section_base, *tree_base;
    register Elf64_Word offset, size;
    WN *wn;
    Elf64_Word first_node;
    Subsect_State st = PU_Info_state(pu, WT_TREE);

    if (st == Subsect_InMem)
	return PU_Info_tree_ptr(pu);
    if (st != Subsect_Exists)
	return (WN *) (INTPTR) ERROR_RETURN;

    offset = PU_Info_subsect_offset(pu, WT_TREE);
    size = PU_Info_subsect_size(pu, WT_TREE);

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
    if (shdr.offset == 0) return (WN *) (INTPTR) ERROR_RETURN;

    if (offset + size > shdr.size) {
	errno = EINVAL;
	return (WN *) (INTPTR) ERROR_RETURN;
    }

    section_base = (char *) handle + shdr.offset;
    tree_base = section_base + offset;

    /* the offset of the first node is at the beginning of the subsection */
    first_node = *(Elf64_Word *)tree_base;
    wn = (WN *) (tree_base + first_node);

#ifndef CFE
    Set_Max_Region_Id(0);	/* reset max id for pu */
#endif

    /* fix up the pointers in the WNs (Note: this must happen after the map
       table is set up because it also updates the map ID info in the
       map table */

    Current_Map_Tab = PU_Info_maptab(pu);
    if (fix_tree (pu, wn, tree_base, size) == ERROR_VALUE)
	return (WN *)ERROR_VALUE;

    WN_next(wn) = NULL;
    WN_prev(wn) = NULL;

    Set_PU_Info_tree_ptr(pu, wn);
    Set_PU_Info_state(pu, WT_TREE, Subsect_InMem);

    return wn;
} /* WN_get_tree */


/*
 * Given a handle returned by WN_open_input(), this functions returns the
 * argc and argv pairs for the command line for compiling the IR.  The
 * value of argc is returned as function return value and is set to 0 if
 * the command line cannot be found (or if it is corrupted).  The argv is
 * passed as a referenced paramenter as the second argument.
 */  

INT
WN_get_flags (void *handle, char ***argv)
{
    Elf64_Word argc;
    char *baseaddr;

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_COMP_FLAGS);
    if (shdr.offset == 0) return 0;

    baseaddr = (char *) handle + shdr.offset;
    if (fix_comp_flags (baseaddr, shdr.size) == ERROR_VALUE)
	return 0;
    argc = *((Elf64_Word *) baseaddr);
    *argv =  (char **) (baseaddr + sizeof(Elf64_Word));

    return argc;
} /* WN_get_flags */


/*
 * Given a handle returned by WN_open_input(), this function 
 * initializes the debug symbol table (dst) with the data in the file.
 * Returns -1 if fails, else the size of the section.
 */  

INT
WN_get_dst (void *handle)
{
    register INT size, num_blocks;
    char *base, *ptr, *blk;
    DST_BLOCK_IDX j;

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_DST);
    if (shdr.offset == 0) return ERROR_RETURN;

    base = (char *) handle + shdr.offset;
    size = shdr.size;

    /* sections contains data, then array of block_headers, then num_blocks. */
    ptr = base + size - sizeof(mINT32);
    num_blocks = *(INT32*) ptr;
    num_blocks++;	/* last_block_number + 1 == number of blocks */
    ptr -= (num_blocks * sizeof(block_header));

    DST_Init (ptr, num_blocks);
    FOREACH_DST_BLOCK(j) {
	blk = ((DST_Type *)Current_DST)->dst_blocks[j].offset;
	if (blk == (char *)-1) {
	    ((DST_Type *)Current_DST)->dst_blocks[j].offset = NULL;
	} else {
	    CONVERT_OFFSET(char*, blk);
	    ((DST_Type *)Current_DST)->dst_blocks[j].offset = blk;
	}
    }

    return size;
} /* WN_get_dst */


#if defined(BACK_END) || defined(IR_TOOLS)
// read the feedback info, if any
INT
WN_get_feedback (void* handle, PU_Info* pu, MEM_POOL* pool)
{
    Subsect_State st = PU_Info_state(pu, WT_FEEDBACK);

    switch (st) {
    case Subsect_Missing:
    case Subsect_InMem:
    default:
	return 0;
    case Subsect_Exists:
	break;
    case Subsect_Written:
	return ERROR_RETURN;
    }

    Elf64_Word offset = PU_Info_subsect_offset (pu, WT_FEEDBACK);
    Elf64_Word size = PU_Info_subsect_size (pu, WT_FEEDBACK);
    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
    if (shdr.offset == 0)
	return ERROR_RETURN;

    if (offset + size >= shdr.size) {
	errno = EINVAL;
	return ERROR_RETURN;
    }

    INTPTR baseaddr = (INTPTR)handle + shdr.offset + offset;
	
    Set_PU_Info_feedback_ptr (pu, baseaddr);
    Set_PU_Info_state (pu, WT_FEEDBACK, Subsect_InMem);

    return 0;
} // WN_get_feedback
#endif 

#ifdef BACK_END
/* Read the dependence graph mapping */
void *
WN_get_depgraph (void *handle, PU_Info *pu)
{
    Elf64_Word offset, size;
    char *cur_addr, *end_addr, *tree_base;
    void *g;
    Subsect_State st = PU_Info_state(pu, WT_DEPGRAPH);
    Current_Map_Tab = PU_Info_maptab(pu);

    if (st == Subsect_Written)
	return (void *) (INTPTR) ERROR_RETURN;
    if (st == Subsect_InMem)
	return PU_Info_depgraph_ptr(pu);
    if (st != Subsect_Exists) {
	Init_Dep_Graph(NULL);
	return NULL;
    }

    offset = PU_Info_subsect_offset(pu, WT_DEPGRAPH);
    size = PU_Info_subsect_size(pu, WT_DEPGRAPH);

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
    if (shdr.offset == 0) return (void *) (INTPTR) ERROR_RETURN;

    if (offset + size >= shdr.size) {
	errno = EINVAL;
	return (void *) (INTPTR) ERROR_RETURN;
    }

    /* find the start of the tree subsection */
    tree_base = (char *) handle + shdr.offset +
	PU_Info_subsect_offset(pu, WT_TREE);

    cur_addr = (char *)handle + shdr.offset + offset;
    end_addr = cur_addr + size;

    /* read the graph */
    g = Depgraph_Read(cur_addr, end_addr, tree_base);

    /* if g is NULL there was an error reading the graph -- it is safe to
       return it anyway since an empty graph will be created in its place */
    Is_True(g, ("Error: Unable to read the dependence graph"));

    Init_Dep_Graph(g);
    Set_PU_Info_depgraph_ptr(pu, g);
    Set_PU_Info_state(pu, WT_DEPGRAPH, Subsect_InMem);
 
    return g;
} /* WN_get_depgraph */
#endif /* BACK_END */


#if defined(BACK_END) || defined(BUILD_WHIRL2C) || defined(BUILD_WHIRL2F)
/* Read the prefetch pointer mapping */
INT
WN_get_prefetch (void *handle, PU_Info *pu)
{
    Elf64_Word offset, size;
    char *cur_addr, *end_addr, *tree_base;
    Elf64_Word node_offset;
    WN *node;
    PF_POINTER *pf_ptr;
    Subsect_State st = PU_Info_state(pu, WT_PREFETCH);
    Current_Map_Tab = PU_Info_maptab(pu);

    if (st == Subsect_Written)
	return ERROR_RETURN;
    if (st != Subsect_Exists)
	return 0;

    offset = PU_Info_subsect_offset(pu, WT_PREFETCH);
    size = PU_Info_subsect_size(pu, WT_PREFETCH);

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
    if (shdr.offset == 0) return ERROR_RETURN;

    if (offset + size >= shdr.size) {
	errno = EINVAL;
	return ERROR_RETURN;
    }

    /* find the start of the tree subsection */
    tree_base = (char *) handle + shdr.offset +
	PU_Info_subsect_offset(pu, WT_TREE);

    cur_addr = (char *)handle + shdr.offset + offset;
    end_addr = cur_addr + size;

    for (;;) {
	node_offset = *(Elf64_Word *)cur_addr;
	cur_addr += (sizeof(Elf64_Word));

	if (node_offset == -1) break;

	cur_addr = (char *)(INTPS)ir_b_align((off_t)(INTPS) cur_addr,
#ifndef __GNUC__
				      __builtin_alignof(PF_POINTER),
#else
				      __alignof__(PF_POINTER),
#endif
				      0);
	pf_ptr = (PF_POINTER *)cur_addr;
	cur_addr += (sizeof(PF_POINTER));

	/* fixup the WN pointers. If -1 then store NULL */
	node = (WN *)(tree_base + node_offset);
        if (pf_ptr->wn_pref_1L == (WN*) -1) {
	    pf_ptr->wn_pref_1L = NULL;
	} else {
	    pf_ptr->wn_pref_1L =
		(WN*)(tree_base + (INTPTR)pf_ptr->wn_pref_1L);
	}

        if (pf_ptr->wn_pref_2L == (WN*) -1) {
	    pf_ptr->wn_pref_2L = NULL;
        } else {
	    pf_ptr->wn_pref_2L =
		(WN*)(tree_base + (INTPTR)pf_ptr->wn_pref_2L);
	}

	/* add the pointer to the prefetch mapping */
	WN_MAP_Set(WN_MAP_PREFETCH, node, (void *)pf_ptr);

	if (cur_addr > end_addr) return ERROR_RETURN;
    }

    PU_Info_subsect_ptr(pu, WT_PREFETCH) = NULL;
    Set_PU_Info_state(pu, WT_PREFETCH, Subsect_InMem);

    return 0;
} /* WN_get_prefetch */

#endif /* BACK_END || BUILD_WHIRL2C || BUILD_WHIRL2F */

#if defined(KEY) && defined(BACK_END)
#include "cxx_memory.h" // for CXX_NEW_ARRAY
#include "be_ipa_util.h"
static void
WN_get_mod_ref_table (void * handle)
{
  OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_IPA_SUMMARY);

  if (shdr.offset == 0)
    return;

  const char *base = (char *) handle + shdr.offset;

  // First is the offset to the header
  const BE_SUMMARY_HEADER *header =
               (BE_SUMMARY_HEADER *) (base + *((Elf64_Word *)base));

  const char *addr = base + header->offset;

  const char * p = addr; // travelling pointer to data

  for (INT i=0; i<header->size; i++)
  {
    UINT32 index;
    New_Mod_Ref_Info (index);

    Mod_Ref_Info_Table[index].pu_idx = *((PU_IDX *) p);
    p += sizeof (PU_IDX);

    mUINT32 bv_size = Mod_Ref_Info_Table[index].size = *((mUINT32 *) p);
    p += sizeof (mUINT32);

    Mod_Ref_Info_Table[index].mod = CXX_NEW_ARRAY (mUINT8, bv_size, Malloc_Mem_Pool);
    memcpy (Mod_Ref_Info_Table[index].mod, p, bv_size);
    p += bv_size;

    Mod_Ref_Info_Table[index].ref = CXX_NEW_ARRAY (mUINT8, bv_size, Malloc_Mem_Pool);
    memcpy (Mod_Ref_Info_Table[index].ref, p, bv_size);
    p += bv_size;
    
    Mod_Ref_Info_Table[index].same_entry_exit_value_or_1  = CXX_NEW_ARRAY (mUINT8, bv_size, Malloc_Mem_Pool);
    memcpy (Mod_Ref_Info_Table[index].same_entry_exit_value_or_1, p, bv_size);
    p += bv_size;
  }
}
#endif

static inline void
WN_MAP_put(WN_MAP wn_map, WN *wn, INT32 value)
{
  WN_MAP32_Set(wn_map, wn, value);
}

static inline void
WN_MAP_put(WN_MAP wn_map, WN *wn, INT64 value)
{
  WN_MAP64_Set(wn_map, wn, value);
}

static inline void
WN_MAP_put(WN_MAP wn_map, WN *wn, void *value)
{
  WN_MAP_Set(wn_map, wn, value);
}

template<class MAP_ENTRY_TYPE>
static inline INT
WN_read_generic_map(void           *handle,
		    PU_Info        *pu,
		    INT32           subsection_type,
		    WN_MAP          value_map,
		    MAP_ENTRY_TYPE)
{
  Subsect_State state = PU_Info_state (pu, subsection_type);
  Elf64_Word offset, size;
  char *cur_addr, *end_addr, *tree_base;

  if (state == Subsect_Written) {
    return ERROR_RETURN;
  }
  if (state != Subsect_Exists) {
    return 0;
  }

  OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_PU_SECTION);
  if (shdr.offset == 0) {
    return ERROR_RETURN;
  }

  offset = PU_Info_subsect_offset(pu, subsection_type);
  size = PU_Info_subsect_size(pu, subsection_type);

  if (offset + size >= shdr.size) {
    errno = EINVAL;
    return ERROR_RETURN;
  }

  /* find the start of the tree subsection */
  tree_base = (char *) handle + shdr.offset +
    PU_Info_subsect_offset(pu, WT_TREE);

  cur_addr = (char *)handle + shdr.offset + offset;
  end_addr = cur_addr + size;

  for (;;) {
    MAP_ENTRY_TYPE map_value;
    Elf64_Word node_offset = *(Elf64_Word *) cur_addr;
    WN *node;
	
    cur_addr += (sizeof(Elf64_Word));	// advance over WN offset

    if (node_offset == -1)
      break;

    // Why do we align here but not for the WN offset? -- RK 980615
    cur_addr = (char *)(INTPS)ir_b_align ((off_t) (INTPS)cur_addr,
				    sizeof(MAP_ENTRY_TYPE),
				    0);
    map_value = * (MAP_ENTRY_TYPE *) cur_addr;
    cur_addr += sizeof(MAP_ENTRY_TYPE);		// advance over map entry

    node = (WN *) (tree_base + node_offset);

    // TODO: The following must be made generic.
    /* add the value to the WHIRL map in memory */
    WN_MAP_put(value_map, node, map_value);

    if (cur_addr > end_addr)
      return ERROR_RETURN;
  }
    
  PU_Info_subsect_ptr(pu, subsection_type) = NULL;
  Set_PU_Info_state(pu, subsection_type, Subsect_InMem);

  return 0;
}

INT
WN_get_INT32_map(void    *handle,
		 PU_Info *pu,
		 INT32    subsection_type,
		 WN_MAP   value_map)
{
  return WN_read_generic_map(handle, pu, subsection_type,
			     value_map, (INT32) 0);
}

INT
WN_get_voidptr_map(void    *handle,
		   PU_Info *pu,
		   INT32    subsection_type,
		   WN_MAP   value_map)
{
  return WN_read_generic_map(handle, pu, subsection_type,
			     value_map, (void *) 0);
}



/*
 * Given a handle returned by WN_open_input(), this function releases all
 * the memory used for reading the WHIRL file.  Note that previously
 * pointers returned by WN_get_tree() will no longer be valid.
 */

#ifdef __MINGW32__
void
WN_free_input (void *handle, HANDLE *mapHd, off_t mapped_size)
#else
void
WN_free_input (void *handle, off_t mapped_size)
#endif /* __MINGW32__ */
{
    if (handle == 0 || handle == (void *)(-1))
	return;

#ifdef __MINGW32__
    UnmapViewOfFile((char *)handle);
    CloseHandle(*mapHd);
#else
    munmap (handle, mapped_size);
#endif /* __MINGW32__ */
} /* WN_free_input */


#ifndef OWN_ERROR_PACKAGE
/*
 * Define common routines for reading all the whirl sections.
 * These routines use the standard compiler error reporting mechanism.
 */

#ifdef __MINGW32__
static void *global_fhandle = NULL;	/* file handle */
void *local_fhandle = NULL;	/* file handle */
static HANDLE global_mapHandle = NULL;	/* file handle */
static HANDLE local_mapHandle = NULL;	/* file handle */
#else
static void *global_fhandle;	/* file handle */
void *local_fhandle;	/* file handle, used in isr.cxx  */
#endif /* __MINGW32__ */
static char *global_ir_file;	/* name of ir input file */
static char *local_ir_file;	/* name of ir input file */

#ifdef __MINGW32__
static void
open_specified_input (char *input_file,
	char **ir_input, void **fhandle, HANDLE * mapHd, off_t *mapped_size, int *fd = 0)
#else
static void
open_specified_input (char *input_file, 
	char **ir_input, void **fhandle, off_t *mapped_size)
#endif /* __MINGW32__ */
{
    Set_Error_Phase ( "Reading WHIRL file" );
    *ir_input = input_file;
#ifdef __MINGW32__
    *fhandle = WN_open_input (input_file, mapped_size, fd, mapHd);
#else
    *fhandle = WN_open_input (input_file, mapped_size);
#endif /* __MINGW32__ */
    if (*fhandle == (void*) REVISION_MISMATCH) {
	ErrMsg ( EC_IR_Revision, file_revision, *ir_input);
    } else if (*fhandle == (void*) ABI_MISMATCH) {
	ErrMsg ( EC_IR_Revision,
		 "abi of whirl file doesn't match abi from command-line",
		 *ir_input);
    } else if (*fhandle == (void*) READER_ERROR) {
	ErrMsg ( EC_IR_Open, *ir_input, errno );
    }
}

// same ir file for both global and local
void *
Open_Input_Info (char *input_file)
{
#ifdef __MINGW32__
	open_specified_input (input_file,
		&global_ir_file, &global_fhandle, &global_mapHandle, &global_mapped_size);
	local_mapHandle = global_mapHandle;
#else
	open_specified_input (input_file, 
		&global_ir_file, &global_fhandle, &global_mapped_size);
#endif /* __MINGW32__ */
	local_ir_file = global_ir_file;
	local_fhandle = global_fhandle;
	local_mapped_size = global_mapped_size;
	return global_fhandle;
}

void *
Open_Global_Input (char *input_file)
{
#ifdef __MINGW32__
	open_specified_input (input_file,
		&global_ir_file, &global_fhandle, &global_mapHandle, &global_mapped_size);
#else
	open_specified_input (input_file, 
		&global_ir_file, &global_fhandle, &global_mapped_size);
#endif /* __MINGW32__ */
	return global_fhandle;
}

void *
Open_Local_Input (char *input_file)
{
#ifdef __MINGW32__
	open_specified_input (input_file,
		&local_ir_file, &local_fhandle, &local_mapHandle, &local_mapped_size);
#else
	open_specified_input (input_file, 
		&local_ir_file, &local_fhandle, &local_mapped_size);
#endif /* __MINGW32__ */
	return local_fhandle;
}

/*
 * Read all the global tables and the PU section header.  Returns a
 * pointer to the PU_Info tree.  If "p_num_PUs" is non-NULL, the number
 * of PUs is returned through it.
 */

PU_Info *
Read_Global_Info (INT32 *p_num_PUs)
{
    PU_Info *pu_tree;

    Set_Error_Phase ( "Reading WHIRL file" );

    if (WN_get_strtab(global_fhandle) == -1) {
	ErrMsg ( EC_IR_Scn_Read, "strtab", global_ir_file);
    }

    if ((INT) WN_get_global_symtab (global_fhandle) == -1) {
	ErrMsg ( EC_IR_Scn_Read, "global symtab", global_ir_file);
    }

#if defined(KEY) && defined(BACK_END)
    WN_get_mod_ref_table (global_fhandle);
#endif

    
    // for now, get dst from local file (later change to global)
    if (WN_get_dst(local_fhandle) == -1) {
	ErrMsg ( EC_IR_Scn_Read, "dst", local_ir_file);
    }

    // PU_Info is stored in local file
    pu_tree = WN_get_PU_Infos (local_fhandle, p_num_PUs);
    if (pu_tree == (PU_Info *)-1) {
	ErrMsg ( EC_IR_Scn_Read, "PU headers", local_ir_file);
    }
    if (verbose_info) {
	printf("done reading global info\n");
    }

    return pu_tree;
}


void
Read_Local_Info (MEM_POOL *pool, PU_Info *pu)
{
    const char *save_phase = Get_Error_Phase();
    Set_Error_Phase ( "Reading WHIRL file" );

    /* set the map table */
    Current_Map_Tab = PU_Info_maptab(pu);
    if (!Current_Map_Tab) {
	Current_Map_Tab = WN_MAP_TAB_Create(pool);
	PU_Info_maptab(pu) = Current_Map_Tab;
    }

    CURRENT_SYMTAB = PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]);
    New_Scope (CURRENT_SYMTAB, MEM_pu_nz_pool_ptr, FALSE);

    if (WN_get_symtab (local_fhandle, pu) == -1)
	ErrMsg ( EC_IR_Scn_Read, "local symtab", local_ir_file);

    Scope_tab[CURRENT_SYMTAB].st = WN_get_proc_sym (pu);
    if (Scope_tab[CURRENT_SYMTAB].st == (ST *) -1)
	ErrMsg ( EC_IR_Scn_Read, "proc ST", local_ir_file);
    else
	Current_pu = &Pu_Table[ST_pu (Scope_tab[CURRENT_SYMTAB].st)];

#if defined(BACK_END) || defined(IR_TOOLS)
    if (PU_Info_state(pu, WT_SSA) == Subsect_Exists) {
        // having WSSA in the IR file, create the SSA manager
        Set_PU_Info_ssa_ptr(pu, new WSSA::WHIRL_SSA_MANAGER(pool));
    }
#endif

    if (WN_get_tree (local_fhandle, pu) == (WN*) -1) {
	ErrMsg ( EC_IR_Scn_Read, "tree", local_ir_file);
    }

#if defined(BACK_END) || defined(IR_TOOLS)
    if (WN_get_feedback (local_fhandle, pu, pool) == ERROR_RETURN) {
	ErrMsg ( EC_IR_Scn_Read, "feedback info", local_ir_file);
    }
#endif

#ifdef BACK_END
    if (WN_get_depgraph (local_fhandle, pu) == (void *) -1) {
	ErrMsg ( EC_IR_Scn_Read, "dependence graph", local_ir_file);
    }
#endif

#if defined(BACK_END) || defined(BUILD_WHIRL2C) || defined(BUILD_WHIRL2F)
    if (WN_get_prefetch (local_fhandle, pu) == -1) {
	ErrMsg ( EC_IR_Scn_Read, "prefetch map", local_ir_file);
    }
#endif

#if defined(BACK_END) || defined(IR_TOOLS)
    if (WN_get_SSA (local_fhandle, pu, pool) == -1) {
	ErrMsg ( EC_IR_Scn_Read, "WHIRL SSA", local_ir_file);
    }
#endif

    if (WN_get_INT32_map(local_fhandle, pu,
			 WT_ALIAS_CLASS, WN_MAP_ALIAS_CLASS) == -1) {
      ErrMsg ( EC_IR_Scn_Read, "alias class map", local_ir_file);
    }

    if (WN_get_INT32_map(local_fhandle, pu,
			 WT_ALIAS_CGNODE, WN_MAP_ALIAS_CGNODE) == -1) {
      ErrMsg ( EC_IR_Scn_Read, "alias cgnode map", local_ir_file);
    }
    // Check if we have read in the WN to CGNodeId map
    if (PU_Info_state(pu, WT_ALIAS_CGNODE) == Subsect_InMem)
      Read_ALIAS_CGNODE_Map = TRUE;

    if (WN_get_voidptr_map(local_fhandle, pu,
			   WT_AC_INTERNAL, WN_MAP_AC_INTERNAL) == -1) {
      ErrMsg ( EC_IR_Scn_Read, "alias class internal map", local_ir_file);
    }
    
#ifdef BACK_END
    IPA_read_alias_summary(local_fhandle, pu, pool);
#endif
    Set_Error_Phase(save_phase);
}


void
Free_Local_Info (PU_Info *pu)
{
#ifdef BACK_END
    Dealloc_Dep_Graph();
#endif /* BACK_END */

    Delete_Scope (PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]));

    /* deallocate the old map table */
    if (PU_Info_maptab(pu)) {
	WN_MAP_TAB_Delete(PU_Info_maptab(pu));
	PU_Info_maptab(pu) = NULL;
    }
}

void 
Free_Dep_Graph (void)
{
#ifdef BACK_END
    Dealloc_Dep_Graph();
#endif /* BACK_END */
} 

#ifdef __MINGW32__
void
Free_Local_Input(void)
{
  WN_free_input(local_fhandle, &local_mapHandle, local_mapped_size);
  //local_fhandle = 0;
  //local_mapHandle = NULL;
}

void
Free_Input_Info (void)
{
	WN_free_input(global_fhandle, &global_mapHandle, global_mapped_size);
    if (global_fhandle != local_fhandle) {
      Free_Local_Input();
    }
    else
    {
	    local_fhandle = 0;
	    local_mapHandle = NULL;
    }
    global_fhandle = 0;
	global_mapHandle = NULL;
}

#else
void
Free_Local_Input(void)
{
  WN_free_input(local_fhandle, local_mapped_size);
  local_fhandle = 0;
}

void
Free_Input_Info (void)
{
    WN_free_input(global_fhandle, global_mapped_size);
    if (global_fhandle != local_fhandle) {
      Free_Local_Input();
    }
    global_fhandle = 0;
}
#endif /* __MINGW32__ */

#endif	/* OWN_ERROR_PACKAGE */

