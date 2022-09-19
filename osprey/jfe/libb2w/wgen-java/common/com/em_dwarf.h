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


#ifndef em_dwarf_INCLUDED
#define em_dwarf_INCLUDED

#include "dwarf_stuff.h"
#include "srcpos.h"
#include "targ_em_dwarf.h"
#include "dwarf_DST.h"		//czw

#if defined(__cplusplus)
extern Dwarf_P_Debug Em_Dwarf_Begin (BOOL is_64bit,
				     BOOL dwarf_trace,
				     DST_language dst_lang,		//czw
				     symbol_index_recorder record_symidx = NULL);

extern void Em_Dwarf_Write_Scns (Cg_Dwarf_Sym_To_Elfsym_Ofst translate_elfsym = NULL);
#else
extern Dwarf_P_Debug Em_Dwarf_Begin (BOOL is_64bit,
				     BOOL dwarf_trace,
				     DST_language dst_lang);		//czw

extern void Em_Dwarf_Write_Scns (void);
#endif

#if defined(__cplusplus)
extern "C" {
#endif

extern INT data_alignment_factor;

extern void Em_Dwarf_End (void);

extern pSCNINFO Em_Dwarf_Find_Dwarf_Scn (size_t scndx);

extern pSCNINFO Em_Dwarf_Find_Dwarf_Scn_By_Name (char *name);

extern INT Em_Dwarf_Prepare_Output (void);

extern void Em_Dwarf_Add_Line_Entry ( INT code_address, SRCPOS srcpos);

extern void Em_Dwarf_Add_Include (UINT16 incl_idx, char *name);

extern void Em_Dwarf_Add_File (
    UINT16 file_idx,
    char *name,
    UINT16 incl_idx,
    Dwarf_Unsigned modification_time,
    Dwarf_Unsigned file_size);


/* Given a file_index, return the filename and the path for the file. */
extern void Em_Dwarf_File_Index_To_Name (
    INT file_index, 
    char **filename, 
    char **path);

/* eh_offset should be -1 if no eh region */
extern void Em_Dwarf_Process_PU (Dwarf_Unsigned begin_label,
				 Dwarf_Unsigned end_label,
				 INT32          begin_offset,
				 INT32          end_offset,
				 Dwarf_P_Die    PU_die,
				 Dwarf_P_Fde    fde,
#ifdef TARG_X8664
				 Dwarf_P_Fde    eh_fde,
#endif
				 Elf64_Word     eh_symindex,
				 INT            eh_offset);
#ifdef TARG_X8664
/* To add FDEs for ALT ENTRY points for a PU (if any) */
extern void Em_Dwarf_Add_PU_Entries (Dwarf_Unsigned begin_label,
				     Dwarf_Unsigned end_label,
				     INT32          begin_offset,
				     INT32          end_offset,
				     Dwarf_P_Die    PU_die,
				     Dwarf_P_Fde    fde);
#endif

extern void Em_Dwarf_Start_Text_Region (pSCNINFO scninfo, INT start_offset);

extern void Em_Dwarf_Start_Text_Region_Semi_Symbolic (pSCNINFO,
						      INT,
						      Dwarf_Unsigned,
						      Dwarf_Addr);

extern void Em_Dwarf_End_Text_Region (pSCNINFO scninfo, INT end_offset);

extern void Em_Dwarf_End_Text_Region_Semi_Symbolic (pSCNINFO,
						    INT,
						    Dwarf_Unsigned,
						    Dwarf_Addr);
#if defined(__cplusplus)
}
#endif
#endif /* em_dwarf_INCLUDED */
