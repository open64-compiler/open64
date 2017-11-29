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


/**
***		Fortran MP Lowering Support
***		---------------------------
***
*** Description:
***
***	This interface describes all the declarations and data needed to
***     perform and support Fortran MP lowering.
***
*** Exported functions:
***
***  These functions are from wn_mp.cxx:
***
***	lower_mp	called when first MP pragma is encountered, identifies,
***			extracts, converts and replaces an entire parallel
***			region in the whirl tree
***
***	LowerMP_PU_Init	per-PU initialization for the MP lowerer
***
***	Gen_MP_Getlock,
***	Gen_MP_Unlock,
***	Gen_MP_Setlock,
***	Gen_MP_Unsetlock	Generate Whirl for mutex operations
***
***  These functions are from wn_mp_dg.cxx:
***
***	Copy_Non_MP_Tree	Copy all but MP pragmas in a tree
***
***	MP_Fix_Dependence_Graph	what the name suggests
***
**/

#ifndef wnmp_INCLUDED
#define wnmp_INCLUDED "wn_mp.h"

#if (! defined(__ELF_H__)) && (! defined(BUILD_OS_DARWIN))
#include <elf.h>            /* pu_info.h can't compile without this */
#endif

#ifndef dwarf_DST_INCLUDED
#include "dwarf_DST.h"      /* for DST_IDX and DST_INFO_IDX */
#endif

#ifndef pu_info_INCLUDED    /* for PU_Info */
#include "pu_info.h"
#endif

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"   /* for DYN_ARRAY */
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern WN * lower_mp (WN *, WN *, INT32);
extern void LowerMP_PU_Init (void);

extern WN * Gen_MP_Getlock ( ST * lock );
extern WN * Gen_MP_Unlock ( ST * lock ) ;
extern WN * Gen_MP_Setlock ( void );
extern WN * Gen_MP_Unsetlock ( void );

extern void Verify_No_MP(WN *tree);

  // list of the STORE nodes for the REDUCTION pragmas of an MP construct
typedef DYN_ARRAY<WN *> REDUCTION_LIST;
extern INT
MP_Reduction_Combine_Cycles(REDUCTION_LIST *rlist, BOOL *using_critical);

typedef enum {
  MPP_UNKNOWN,
  MPP_COPYIN,
  MPP_CRITICAL_SECTION,
  MPP_SINGLE,
  MPP_ORPHANED_SINGLE,
  MPP_PDO,
  MPP_ORPHANED_PDO,
  MPP_PARALLEL_DO,
  MPP_PARALLEL_REGION,
  MPP_MASTER,
  MPP_ORPHANED_MASTER,
#ifdef KEY /* Bug 4828 */
  MPP_WORKSHARE,
  MPP_ORPHANED_WORKSHARE,
  MPP_ORPHAN
#endif
} MP_process_type;

extern WN * Gen_OMP_Begin_SPR (MP_process_type mpt);
extern WN * Gen_OMP_End_SPR   (MP_process_type mpt);

extern BOOL Is_Nonpod_Finalization_IF(WN *wn, BOOL *is_first_and_last);

extern void Move_Non_POD_Finalization_Code(WN *block);

extern void MP_Fix_Dependence_Graph(PU_Info *parent_pu_info,
				    PU_Info *child_pu_info, WN *child_wn);
extern WN * Copy_Non_MP_Tree ( WN * tree );

extern DST_IDX Find_DST_From_ST ( ST *st, PU_Info *pu_info );
extern void
Create_New_DST ( DST_INFO_IDX dst, ST *st , BOOL append_to_nested );

extern BOOL WN_has_pragma_with_side_effect ( WN *wn );


#ifdef __cplusplus
}
#endif

#endif
