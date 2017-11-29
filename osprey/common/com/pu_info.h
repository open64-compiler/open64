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


#ifndef pu_info_INCLUDED
#define pu_info_INCLUDED

#if defined(BUILD_OS_DARWIN)
#include "darwin_elf.h"		/* for Elf64_Word */
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>		/* for Elf64_Word */
#endif /* defined(BUILD_OS_DARWIN) */

#include "dwarf_DST_mem.h"

#ifndef _symtab_INCLUDED
#include "symtab.h"
#endif

/*
 * This file defines the interface to the program unit (PU) header
 * information.  All the data structures associated with PUs are
 * stored in the same section, but each structure has its own
 * subsection.  The PU header information identifies the positions and
 * sizes of the subsections.  To make it easy to access the headers,
 * they are read into the PU_Info structures defined below.
 * Each file has a tree of PU_Infos to record the header information
 * for both the input and output files and to collect pointers to the
 * PU data structures in memory.
 */


/* Version number to identify the structure of the PU section header */
#define PU_HEADER_VERSION 1


/*
 * Each subsection has a unique identifier that is stored in the binary
 * output file.  This provides some chance of adding new subsections
 * without making the binary format incompatible with older versions.
 *
 * CAUTION: We cannot define more than 15 subsection types here
 * without changing the implementation of PU_Info_state() because the
 * state info is stored in a 32-bit word in the PU_Info structure, at
 * two bits per subsection type. See PU_Info_state() for details.
 */

#define WT_SYMTAB	0x0		/* symbol table */
#define WT_TREE		0x1		/* WHIRL tree nodes */
#define WT_DEPGRAPH	0x2		/* dependence graph */
#define WT_PREFETCH	0x3		/* prefetch pointers */
#define WT_REGIONS	0x4		/* RID trees */
#define WT_FEEDBACK	0x5		/* feedback data */
#define WT_FREQ		0x6		/* obsolete */
#define WT_AC_INTERNAL	0x7		/* alias classification temporary */
#define WT_ALIAS_CLASS	0x8		/* alias classification */
#define WT_SSA          0x9        /*WHIRL SSA info*/
#define WT_ALIAS_CGNODE	0xa		/* constraint graph node */

#define WT_SUBSECTIONS	0xb		/* number of subsection types */
#define WT_PROC_SYM	-1		/* special value for proc sym state */

/*
 * These are the misc. flags associated to each PU
 */
#define PU_HAS_GLOBALS	         0x1 /* has globals symbols in local symtab,
				        i.e., ST_EXPORT != EXPORT_LOCAL */ 
#define PU_HAS_NESTED_PU         0x2 /* has other PU nested under this one */
#define PU_IS_COMPILER_GENERATED 0x4 /* compiler_generated PU */
#define PU_IS_DRA_CLONE          0x8 /* clone generated for reshaped arrays */
#define PU_PREGS_RENUMBERED     0x10 /* Preopt for IPL has processed
					this PU; used to decide
					whether to repeat alias
					classification within preopt
					for LNO */
#ifdef KEY
#define PU_IS_PROFILER          0x20
#endif
/* reserve the top 4 bits for internal use within IPA */
#define PU_INTERNAL_MASK 0xf0000000
#define PU_DELETED	0x10000000  /* only used internally within IPA */

/*
 * The WHIRL subsection header information is stored internally as a
 * tree of PU_Info structures.  (The actual subsection headers
 * in the binary files are compacted into arrays but that is too
 * inflexible to use internally.)  For each subsection, the PU_Info
 * structure records the current state and its position in memory or
 * in a file.  The pointer to the structure in memory overlaps with the
 * subsection size, so that the subsection file offset is still available
 * after reading the structure into memory.  (Some subsections refer to
 * WN nodes by their offsets within the WT_TREE subsection, and we need
 * to convert those references to pointers.)  To identify the PU,
 * the ID for the procedure symbol is also stored in the file.  This
 * can be converted to a pointer to the actual ST, and so we also keep
 * track of which form is stored.  The state flags are described below.
 */

typedef struct pu_info {
    struct pu_info *next;		/* next PU_Info in the list */
    struct pu_info *child;		/* first child (nested procedure) */
    mINT32 state;			/* current state of each subsection */

    ST_IDX proc_sym;			/* ST_IDX of the function st */
    mINT32 flags;			/* flags for any other attributes */
    DST_IDX pu_dst;			/* index for the PU's DST entry */
    DST_IDX cu_dst;			/* index for the compilation units
					 * DST entry
					 */

    struct {
	Elf64_Word offset;		/* section offset */
	union {
	    Elf64_Word size;		/* subsection size */
	    void *ptr;			/* pointer to structure in memory */
	} u;
    } subsect[WT_SUBSECTIONS];

    /* other information not stored in the binary files */
    struct wn_map_tab *maptab;
} PU_Info;


#define PU_Info_proc_sym(pu)		(pu)->proc_sym
#define PU_Info_proc_id(pu)		(pu)->proc_sym
	
#ifdef __cplusplus
  // convenience function
inline PU &
PU_Info_pu(PU_Info *ppui)
{
  return Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(ppui)])];
}
#endif

#define PU_Info_subsect_offset(pu,scn)	(pu)->subsect[scn].offset
#define PU_Info_subsect_size(pu,scn)	(pu)->subsect[scn].u.size
#define PU_Info_subsect_ptr(pu,scn)	(pu)->subsect[scn].u.ptr
#define PU_Info_flags(pu)		(pu)->flags
#define PU_Info_pu_dst(pu)		(pu)->pu_dst
#define PU_Info_cu_dst(pu)		(pu)->cu_dst
#define PU_Info_cu_dst(pu)		(pu)->cu_dst
#define PU_Info_maptab(pu)		(pu)->maptab
#define PU_Info_next(pu)		(pu)->next
#define PU_Info_child(pu)		(pu)->child

#define Set_PU_Info_flags(pu,flag_value) (pu)->flags |= (flag_value)
#define Clear_PU_Info_flags(pu,flag_value) (pu)->flags &= ~(flag_value)
#define Is_Set_PU_Info_flags(pu,flag_value) ((pu)->flags & (flag_value))


/*
 *  The current state of each subsection for a PU is recorded in the
 *  state field of the PU_Info structure.  Since there are four
 *  possible states, we need two bits for each subsection.  We also
 *  need to keep track of whether the procedure's ST is stored as an
 *  ID number or as a pointer, so the first two bits are reserved for
 *  that (set ndx = -1 in the macros below to get/set the state of the
 *  procedure's ST).
 */

typedef enum subsect_state {
    Subsect_Missing = 0,		/* information not available */
    Subsect_Exists = 1,			/* in the input file */
    Subsect_InMem = 2,			/* currently in memory */
    Subsect_Written = 3			/* in the output file */
} Subsect_State;

#define PU_Info_state(pu, ndx) \
    (Subsect_State)(((pu)->state >> (((ndx) + 1) << 1)) & 0x3)
#define Set_PU_Info_state(pu, ndx, st) (pu)->state = (pu)->state & \
    ~(0x3 << (((ndx) + 1) << 1)) | (st << (((ndx) + 1) << 1))


/*
 *  High-level interface
 */

#define PU_Info_symtab_ptr(pu) \
     (LOCAL_SYMTAB_HEADER_TABLE *) PU_Info_subsect_ptr((pu), WT_SYMTAB)
#define PU_Info_tree_ptr(pu) \
     (WN *)PU_Info_subsect_ptr((pu), WT_TREE)
#define PU_Info_depgraph_ptr(pu) \
     (void *)PU_Info_subsect_ptr((pu), WT_DEPGRAPH)
#define PU_Info_regions_ptr(pu) \
     (struct region_id *)PU_Info_subsect_ptr((pu), WT_REGIONS)
#define PU_Info_feedback_ptr(pu) \
     (void *)PU_Info_subsect_ptr((pu), WT_FEEDBACK)
#define PU_Info_frequencies_ptr(pu) \
     (INT32 *) PU_Info_subsect_ptr((pu), WT_FREQ)
#define PU_Info_alias_class_ptr(pu) \
     (INT32 *) PU_Info_subsect_ptr((pu), WT_ALIAS_CLASS)
#define PU_Info_alias_cgnode_ptr(pu) \
     (INT32 *) PU_Info_subsect_ptr((pu), WT_ALIAS_CGNODE)
#define PU_Info_ac_internal_ptr(pu) \
     (void *) PU_Info_subsect_ptr((pu), WT_AC_INTERNAL)
//get WHIRL ssa info to pu_info
#define PU_Info_ssa_ptr(pu) \
     (WSSA::WHIRL_SSA_MANAGER *) PU_Info_subsect_ptr((pu), WT_SSA)

#define Set_PU_Info_tree_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_TREE) = (void *)(x)
#define Set_PU_Info_symtab_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_SYMTAB) = (void *)(x)
#define Set_PU_Info_depgraph_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_DEPGRAPH) = (void *)(x)
#define Set_PU_Info_regions_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_REGIONS) = (void *)(x)
#define Set_PU_Info_feedback_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_FEEDBACK) = (void *)(x)
#define Set_PU_Info_frequencies_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_FREQ) = (void *) (x)
#define Set_PU_Info_alias_class_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_ALIAS_CLASS) = (void *) (x)
#define Set_PU_Info_alias_cgnode_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_ALIAS_CGNODE) = (void *) (x)
#define Set_PU_Info_ac_internal_ptr(pu,x) \
     PU_Info_subsect_ptr((pu), WT_AC_INTERNAL) = (void *) (x)
//set WHIRL ssa info to pu_info
#define Set_PU_Info_ssa_ptr(pu,x) \
    PU_Info_subsect_ptr((pu), WT_SSA) = (WSSA::WHIRL_SSA_MANAGER *) (x)

#define Set_PU_Info_pu_dst(pu, x) \
	PU_Info_pu_dst(pu) = x;
#define Set_PU_Info_cu_dst(pu, x) \
	PU_Info_cu_dst(pu) = x;


#ifdef __cplusplus
extern "C" {
#endif

extern void PU_Info_init (PU_Info *pu);
extern INT Sizeof_PU_Infos (PU_Info *pu_tree);
extern INT Write_PU_Infos (PU_Info *pu_tree, char *base);
extern PU_Info *Read_PU_Infos (char *base, INT32 size, INT32 *p_num_PUs);

extern PU_Info *Current_PU_Info;

extern void Save_Local_Symtab (SYMTAB_IDX level, PU_Info *pu);
extern void Restore_Local_Symtab (PU_Info *pu);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
  // convenience function
inline PU &
Current_PU_Info_pu()
{
  return PU_Info_pu(Current_PU_Info);
}
#endif

#ifdef FIL
extern PU_Info *Last_PU;
#endif
#endif /* pu_info_INCLUDED */
