/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/*
 * This file contains the routines to read and write the PU_Info
 * headers.  The current file format for the headers is:
 *
 *	version number
 *	number of PUs (num_PUs)
 *	array [num_PUs] of {
 *	    proc ST id
 *	    flags
 *	    DST index
 *	    array index of previous PU_Info (or -1 if this is a child)
 *	}
 *	number of subsection kinds (scn_kinds)
 *	array [scn_kinds] of {
 *	    subsection kind identifier
 *	    array [num_PUs] of {
 *		subsection offset
 *		subsection size
 *	    }
 *	}
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "cxx_memory.h"
#include "defs.h"
#include "symtab.h"
#include "pu_info.h"

PU_Info *Current_PU_Info = NULL;

char *
Current_PU_Name()
{
  return ST_name(PU_Info_proc_sym(Current_PU_Info));
}

void
PU_Info_init (PU_Info *pu)
{
    pu->state = 0;
    PU_Info_next(pu) = NULL;
    PU_Info_child(pu) = NULL;
    PU_Info_proc_id(pu) = 0;
    PU_Info_flags(pu) = 0;
    PU_Info_pu_dst(pu) = DST_INVALID_IDX;
    PU_Info_cu_dst(pu) = DST_INVALID_IDX;
    PU_Info_maptab(pu) = NULL;
}


static INT number_PU_Infos(PU_Info *pu_tree, INT num_PUs);

INT
Sizeof_PU_Infos (PU_Info *pu_tree)
{
    INT32 hdr_sz, num_PUs;

    /* count the number of PUs */
    num_PUs = number_PU_Infos(pu_tree, 0);

    /* calculate the header size:
       . 3 INT32s				version, PU & subsection cnts
       . WT_SUBSECTIONS INT32s			subsection kind identifiers
       . num_PUs * 3 INT32s			proc ST IDs, flags, prev index
       . num_PUs * DST_IDXs			DST indices
       . num_PUs * WT_SUBSECTIONS Elf64_Words	subsection offsets
       . num_PUs * WT_SUBSECTIONS Elf64_Words	subsection sizes
       (factor out the constants so it can be folded to X + num_PUs * Y) */

    hdr_sz = (sizeof(mINT32) * (3 + WT_SUBSECTIONS)) +
	num_PUs * ((3 * sizeof(mINT32)) + sizeof(DST_IDX) +
		   (2 * sizeof(Elf64_Word) * WT_SUBSECTIONS));

    return hdr_sz;
}


INT
number_PU_Infos (PU_Info *pu_tree, INT num_PUs)
{
    PU_Info *pu;
    for (pu = pu_tree; pu; pu = PU_Info_next(pu)) {

	/* stash the array index in the map table field */
	PU_Info_maptab(pu) = (struct wn_map_tab *)(INTPTR)num_PUs;

	num_PUs += 1;

	if (PU_Info_child(pu)) {
	    num_PUs = number_PU_Infos(PU_Info_child(pu), num_PUs);
	}
    }
    return num_PUs;
}


/*
 * This procedure writes the tree of PU_Infos to the address specified
 * in the base parameter.  Note that unlike Read_PU_Infos, the base
 * address here refers to the actual address where the header will be
 * written, not the beginning of the WT_PU_SECTION.  If no errors occur,
 * the procedure returns 0; otherwise -1.
 */

static char *write_PU_fields(PU_Info *pu_tree, char *addr, INT32 *p_num_PUs);
static char *write_PU_subsect_info(PU_Info *pu_tree, INT32 k, char *addr);

INT
Write_PU_Infos (PU_Info *pu_tree, char *base)
{
    INT32 k, num_PUs = 0;
    char *addr;

    /* write the version number */
    *(INT32 *)base = PU_HEADER_VERSION;
    base += sizeof(mINT32);

    /* leave space for the number of PUs */
    addr = base + sizeof(mINT32);

    addr = write_PU_fields(pu_tree, addr, &num_PUs);

    /* write out the number of subsection kinds */
    *(INT32 *)addr = WT_SUBSECTIONS;
    addr += sizeof(mINT32);

    for (k = 0; k < WT_SUBSECTIONS; k++) {

	/* write out the subsection identifier */
	*(INT32 *)addr = k;
	addr += sizeof(mINT32);

	addr = write_PU_subsect_info(pu_tree, k, addr);
	if (!addr) return -1;
    }

    /* write out the number of PUs */
    *(INT32 *)base = num_PUs;

    return 0;
}


char *
write_PU_fields (PU_Info *pu_tree, char *addr, INT32 *p_num_PUs)
{
    PU_Info *pu, *prev_pu = NULL;
    for (pu = pu_tree; pu; pu = PU_Info_next(pu)) {

	/* write out the proc ST IDs */
	*(INT32 *)addr = (INT32) PU_Info_proc_sym(pu);
	addr += sizeof(mINT32);

	/* write the flags */
	*(INT32 *)addr = PU_Info_flags(pu);
	addr += sizeof(mINT32);

	/* write the DST index */
	*(DST_IDX *)addr = PU_Info_pu_dst(pu);
	addr += sizeof(DST_IDX);

	/* write the array index of the previous PU_Info or -1 if this
	   is the first child of the immediately preceding PU_Info */
	if (prev_pu) {
	    *(INT32 *)addr = (INT32) (INTPS) PU_Info_maptab(prev_pu);
	} else {
	    *(INT32 *)addr = -1;
	}
	addr += sizeof(mINT32);

	*p_num_PUs += 1;

	if (PU_Info_child(pu)) {
	    addr = write_PU_fields(PU_Info_child(pu), addr, p_num_PUs);
	}

	prev_pu = pu;
    }
    return addr;
}


char *
write_PU_subsect_info (PU_Info *pu_tree, INT32 k, char *addr)
{
    PU_Info *pu;
    for (pu = pu_tree; pu; pu = PU_Info_next(pu)) {

	if (PU_Info_state(pu, k) == Subsect_Missing) {
	    PU_Info_subsect_offset(pu, k) = 0;
	    PU_Info_subsect_size(pu, k) = 0;
	} else if (PU_Info_state(pu, k) != Subsect_Written) {
	    return NULL;
	}

	/* write out the subsection offset and size */
	*(Elf64_Word *)addr = PU_Info_subsect_offset(pu, k);
	addr += sizeof(Elf64_Word);
	*(Elf64_Word *)addr = PU_Info_subsect_size(pu, k);
	addr += sizeof(Elf64_Word);

	if (PU_Info_child(pu)) {
	    addr = write_PU_subsect_info(PU_Info_child(pu), k, addr);
	    if (!addr) return NULL;
	}
    }
    return addr;
}


/*
 * Given a pointer to the beginning of the WT_PU_SECTION and the section
 * size, this function reads the PU headers into a tree of PU_Info structures.
 * It returns a pointer to the root of the tree and returns the number of
 * PUs through the p_num_PUs parameter.  The return value is -1 if an
 * error occurs.
 */

PU_Info *
Read_PU_Infos (char *base, INT32 size, INT32 *p_num_PUs)
{
    INT n, k;
    char *addr;
    INT32 version_num, num_PUs, prev_index, subsect_kinds, kind;
    PU_Info *pu_array, *pu;
    Elf64_Word hdr_offset;

    hdr_offset = *(Elf64_Word *)base;
    addr = base + hdr_offset;

    /* check the version number */
    version_num = *(INT32 *)addr;
    addr += sizeof(mINT32);
    if (version_num != PU_HEADER_VERSION) {
	return (PU_Info *)-1;
    }

    num_PUs = *(INT32 *)addr;
    addr += sizeof(mINT32);

    if (p_num_PUs) *p_num_PUs = num_PUs;
    if (num_PUs < 0) {
	return (PU_Info *)-1;
    }
    if (num_PUs == 0) {
	return NULL;
    }

    pu_array = (PU_Info *)malloc(sizeof(PU_Info) * num_PUs);

    BZERO(pu_array, sizeof(PU_Info) * num_PUs);

    for (n = 0, pu = pu_array; n < num_PUs; n++, pu++) {
	/* initialize */
	PU_Info_init(pu);

	/* read the procedure ST IDs */
	Set_PU_Info_state(pu, WT_PROC_SYM, Subsect_Exists);
	PU_Info_proc_id(pu) = *(INT32 *)addr;
	addr += sizeof(mINT32);

	/* read the flags */
	PU_Info_flags(pu) = *(INT32 *)addr;
	addr += sizeof(mINT32);

	/* read the DST_IDX */
	PU_Info_pu_dst(pu) = *(DST_IDX *)addr;
	addr += sizeof(DST_IDX);

	/* link up the PU_Infos */
	prev_index = *(INT32 *)addr;
	addr += sizeof(mINT32);
	if (prev_index == -1) {
	    if (n != 0) PU_Info_child(&pu_array[n-1]) = pu;
	} else {
	    PU_Info_next(&pu_array[prev_index]) = pu;
	}
    }

    subsect_kinds = *(INT32 *)addr;
    addr += sizeof(mINT32);

    for (k = 0; k < subsect_kinds; k++) {

	/* read the subsection kind identifier */
	kind = *(INT32 *)addr;
	addr += sizeof(mINT32);

	/* ignore things we don't understand */
	if (kind >= WT_SUBSECTIONS) {
	    addr += (num_PUs * 2 * sizeof(Elf64_Word));
	    continue;
	}

	for (n = 0, pu = pu_array; n < num_PUs; n++, pu++) {
	    /* read the offset and size of the subsection */
	    PU_Info_subsect_offset(pu, kind) = *(Elf64_Word *)addr;
	    addr += sizeof(Elf64_Word);
	    PU_Info_subsect_size(pu, kind) = *(Elf64_Word *)addr;
	    addr += sizeof(Elf64_Word);

	    if (PU_Info_subsect_size(pu, kind) != 0) {
		Set_PU_Info_state(pu, kind, Subsect_Exists);
	    }
	}
    }

    /* make sure we didn't run off the end of the header */
    if (addr > base + size) return (PU_Info *)-1;

    return pu_array;
}

  // assertion-checking data structure for Save_Local_Symtab()/
  // Restore_Local_Symtab()
struct SAVED_SCOPE {
  enum {
    MAGIC = 0x23456789
  };
  INT32 magic;
  ST *parent_pu_st;
  SCOPE *saved_scope;

  SAVED_SCOPE(SCOPE *sc, SYMTAB_IDX level) :
      magic(MAGIC), parent_pu_st(Scope_tab[level - 1].st), saved_scope(sc)
    { }

    // get saved scope, with extensive assertion checking
  SCOPE *Get_Scope(SYMTAB_IDX level) {
    if (magic != MAGIC)
      Fail_FmtAssertion("bad SAVED_SCOPE magic number");
    if (Scope_tab[level - 1].st != parent_pu_st)
      Fail_FmtAssertion("parent PU ST mismatch between save and restore");

    return saved_scope;
  }
};


// make a copy of the specified entry of Scope_tab, so that it can be later 
// restored by Restore_Local_Symtab.  This is primarily used by the
// mp-lowerer where multiple nested procedures are created before they are
// compiled. We need a place holder for their corresponding tree nodes and
// local symtab.  Note that the Scope_tab entry will be re-initialized for
// the next nested procedure.
void
Save_Local_Symtab (SYMTAB_IDX level, PU_Info *pu)
{
    // Why all this assertion checking?  Because this way of saving/
    // restoring scopes is a hack that the symbol table and PU_Info
    // classes weren't really designed to support, so this could easily
    // break if it's used in an unexpected way.
  FmtAssert (PU_Info_state (pu, WT_SYMTAB) == Subsect_InMem,
             ("Local symbol table not in memory"));
  FmtAssert (level >= 2,
             ("saving symtab at too high a level (%d)", level));
  FmtAssert (Current_scope >= level, ("bad level and/or Current_scope"));

    // to make sure that save/restore symtab hack is used only by those
    // who understand it, the caller must set symtab_ptr to NULL
  FmtAssert (PU_Info_symtab_ptr(pu) == NULL,  
             ("Incorrect call to Save_Local_Symtab"));
  FmtAssert (level == PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]),
             ("Invalid pu_info in Save_Local_Symtab"));

  SCOPE *scope = (SCOPE*) MEM_POOL_Alloc (Scope_tab[CURRENT_SYMTAB].pool,
                                          sizeof(SCOPE));
  *scope = Scope_tab[level];
  SAVED_SCOPE *saved_scope = CXX_NEW(SAVED_SCOPE(scope, level),
                                     Scope_tab[CURRENT_SYMTAB].pool);

  Set_PU_Info_symtab_ptr (pu, saved_scope);
} // Save_Local_Symtab


void
Restore_Local_Symtab (PU_Info *pu)
{
    FmtAssert (PU_Info_state (pu, WT_SYMTAB) == Subsect_InMem,
	       ("Local symbol table not in memory"));

    FmtAssert (PU_Info_symtab_ptr(pu),
               ("Incorrect call to Save/Restore_Local_Symtab pair"));

    SYMTAB_IDX level = PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]);

    FmtAssert(Current_scope >= level, ("bad level and/or Current_scope"));

    SAVED_SCOPE *saved_scope = (SAVED_SCOPE *) PU_Info_symtab_ptr(pu);

    Scope_tab[level] = *saved_scope->Get_Scope(level);

} // Restore_Local_Symtab


#ifdef Is_True_On
/* Names of subsections for fdump_PU_Info_state()
 */
const static char *PU_Info_subsect_name[WT_SUBSECTIONS] = {
  "WT_SYMTAB",
  "WT_TREE",
  "WT_DEPGRAPH",
  "WT_PREFETCH",
  "WT_REGIONS",
  "WT_FEEDBACK",
  "WT_FREQ",
  "WT_AC_INTERNAL",
  "WT_ALIAS_CLASS",
  "WT_ALIAS_CGNODE"
};

void fdump_PU_Info_state(FILE *f, PU_Info *pu_info)
{
  INT i;

  /* For each subsection, show the state of the subsection. */
  for (i = WT_PROC_SYM; i < WT_SUBSECTIONS; i++) {
    /* WT_PROC_SYM is special; it is -1. */
    if (i == WT_PROC_SYM) {
      (void) fprintf(f, "WT_PROC_SYM: ");
    }
    else {
      (void) fprintf(f, "%s: ", PU_Info_subsect_name[i]);
    }
    switch (PU_Info_state(pu_info, i)) {
    case Subsect_Missing:
      (void) fprintf(f, "Subsect_Missing\n");
      break;
    case Subsect_Exists:
      (void) fprintf(f, "Subsect_Exists\n");
      break;
    case Subsect_InMem:
      (void) fprintf(f, "Subsect_InMem\n");
      break;
    case Subsect_Written:
      (void) fprintf(f, "Subsect_Written\n");
      break;
    default:
      (void) fprintf(f, "Unknown state\n");
      break;
    }
  }
}

void dump_PU_Info_state(PU_Info *pu_info)
{
  fdump_PU_Info_state(stdout, pu_info);
}
#endif
