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



/* ====================================================================
 * ====================================================================
 *
 * Module: stblock.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:39-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.stblock.h $
 *
 * Revision history:
 *  11-Nov-94 - Original Version
 *
 * Description:
 *
 * General support for ST blocks and bases.
 *
 * See also:  data_layout.h
 *
 * ====================================================================
 * ====================================================================
 */
#ifndef	stblock_INCLUDED
#define	stblock_INCLUDED
#include "strtab.h"
#ifdef __cplusplus
extern "C" {
#endif

/* 
 * Union two objects to use the same memory space.
 * Merges blk2 into container for blk1 (creating base if needed).
 */
extern void St_Block_Union(ST *blk1, ST *blk2);

/* return new ST of type CLASS_BLOCK */
extern ST *New_ST_Block (STR_IDX name, BOOL is_global, ST_SCLASS sclass, UINT16 align, INT64 size);
extern ST* Copy_ST_Block (ST *orig_blk); /* return copy of class_block st */

extern ST *Base_Symbol (ST *st);	/* return root block symbol */

/* return offset from base */
extern INT64 Offset_From_Base_Symbol ( ST *st );

/* Get the base symbol and the offset from base that should be used 
 * for addressing. */
extern void  Base_Symbol_And_Offset_For_Addressing (
  ST *sym,			/* Symbol to analyze */
  INT64 ofst,			/* Offset from Symbol */
  ST **base_symbol,		/* Result: root base of st */
  INT64 *offset_from_base	/* Result: offset from primary base */
);

/* 
 * Determine the alignment of a ST.
 * Start with the TY_align value,
 * but for aggregates use Aggregate_Alignment if it is larger.
 */
extern INT32 Adjusted_Alignment ( ST *st);

/* 
 * Determine the alignment of a ST.
 * Start with the Adjusted_Alignment, 
 * but if allocated figure out an improved alignment.
 */
extern INT32 ST_alignment ( ST *st);

/* create a slink symbol in current symtab */
extern void Create_Slink_Symbol (void);

/* find slink symbol in specified symtab */
extern ST* Find_Slink_Symbol (SYMTAB_IDX stab);
/* return slink symbol that matches the ST_symtab_id(st) */
extern ST *Find_Slink_For_ST(ST *st);

/*
 * return slink symbol at level ST_scope_id(function) - ST_scope_id(sym)
 */
extern ST *Find_Slink_For_Scope(ST *function, ST *sym);

/* Determine if the ST is an uplevel reference */
extern BOOL ST_is_uplevelTemp(const ST *);

#ifdef KEY
extern ST *Create_FPSave_Symbol(void);
extern ST *Find_FPSave_Symbol(SYMTAB_IDX);
extern ST *Create_SPSave_Symbol(void);
extern ST *Find_SPSave_Symbol(SYMTAB_IDX);
#endif

#ifdef __cplusplus
}
#endif
#endif /* stblock_INCLUDED */

