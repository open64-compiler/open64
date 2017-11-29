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


#ifndef	cg_region_INCLUDED
#define	cg_region_INCLUDED

#include "bb.h"
#include "tn.h"
#include "tn_list.h"

class WN;

typedef struct cgrin {
  BB           *first_bb;            /* a REGION always corresponds to a contiguous chain of BBs */
  BB           *last_bb;             /* pointers to first and last BBs are provided */
  BB_NUM        min_bb_id;           /* lowest id-number of any BB in the REGION */
  TN          **PREG_to_TN_mapping;  /* saved mapping for cases with unallocated TNs */
  TN_NUM        first_regular_tn;    /* first non_dedicated tn referenced in this REGION */
  INT32         first_gtn;           /* first gtn referenced in this REGION */
  BB           *entry;
  BB          **exits;               /* If there is more than one exit, the array of exits */
                                     /* is in 1-1 correspondence */
                                     /* with the GOTO targets in kid1 of the WN */
  LABEL_IDX    *exit_labels;         /* external labels on the exits */
  TN_LIST      *tns_in;              /* upward exposed (allocated) tns */
  TN_LIST     **tns_out;             /* downward exposed (allocated) tns */
  WN           *entry_glue;          /* WHIRL BLOCK */
  WN          **exit_glue;           /* each one is a WHIRL BLOCK */
  WN 	       *nested_exit;	     /* alternative exit block for nested region */
} CGRIN;
  

#define CGRIN_first_bb(c)           ((c)->first_bb)
#define CGRIN_last_bb(c)            ((c)->last_bb)
#define CGRIN_min_bb_id(c)          ((c)->min_bb_id)
#define CGRIN_preg_to_tn(c,i)       (((c)->PREG_to_TN_mapping)[(i)])
#define CGRIN_preg_to_tn_mapping(c) ((c)->PREG_to_TN_mapping)
#define CGRIN_first_regular_tn(c)   ((c)->first_regular_tn)
#define CGRIN_first_gtn(c)          ((c)->first_gtn)
#define CGRIN_entry(c)              ((c)->entry)
#define CGRIN_exits(c)              ((c)->exits)
#define CGRIN_exit_i(c,i)           (((c)->exits)[(i)])
#define CGRIN_exit_labels(c)        ((c)->exit_labels)
#define CGRIN_exit_label_i(c,i)     (((c)->exit_labels)[(i)])
#define CGRIN_tns_in(c)             ((c)->tns_in)
#define CGRIN_tns_out(c)            ((c)->tns_out)
#define CGRIN_tns_out_i(c,i)        (((c)->tns_out)[(i)])
#define CGRIN_entry_glue(c)         ((c)->entry_glue)
#define CGRIN_exit_glue(c)          ((c)->exit_glue)
#define CGRIN_exit_glue_i(c,i)      (((c)->exit_glue)[(i)])
#define CGRIN_nested_exit(c)        ((c)->nested_exit)

extern BOOL Trace_REGION_Interface;

/*
 * Create a CGRIN in the REGION_mem_pool
 * Entry and exit glue WHIRL BLOCKs are also created.  These are in the
 * standard WHIRL memory pool.
 */
extern CGRIN *CGRIN_Create( INT );

/*
 * Create a the WHIRL for mapping PREGs to the the allocated TNs in inlist
 */
extern void REGION_Entry_PREG_Whirl( RID *, WN *, TN_LIST *,
				    struct ALIAS_MANAGER * );

/*
 * exit_bb should contain a branch to external_label.
 * This branch is converted to a branch to a new label at the
 * beginning of exit_whirl, and a GOTO/REGION_EXIT external_label is
 * added to the end of exit_whirl, unless exit_whirl ends with
 * an OPC_RETURN.
 */
extern LABEL_IDX REGION_Exit_Whirl_Labels( WN *exit_whirl,  BB *exit_bb,
				    LABEL_IDX external_label, RID *rid );

/*
 * Create the WHIRL code for mapping allocated TNs in outlist to PREGs
 */
extern void REGION_Exit_PREG_Whirl( RID *, INT, WN *, TN_LIST *,
				   struct ALIAS_MANAGER * );

/*
 * return TRUE if bb is the entry of the REGION described by rid.
 */
inline BOOL BB_REGION_Entry( BB *bb, RID *rid )
{
  return (rid != NULL
	&& RID_cginfo(rid) != NULL
	&& CGRIN_entry(RID_cginfo(rid)) == bb);
}

#define NO_REGION_EXIT -1
/*
 * return the exit index if bb is one of the exits of the REGION described 
 * by rid; else return NO_REGION_EXIT.
 */
extern INT BB_REGION_Exit( BB *bb, RID *rid );

/*
 * Print the given TN_LIST to TFile
 */
extern void TN_LIST_Print( TN_LIST *tnl );

/* 
 * finds the appropriate cginfo for a given RID* depending on whether its
 * a glue code region or not
 */
extern CGRIN* RID_Find_Cginfo( BB* );

/*
 * Return non-transparent (according to cg) rid.
 * Wopt and other phases have a different view of transparency,
 * but for CG it means not a olimit or user region.
 */
extern RID* Non_Transparent_RID (RID *);

// get list of tns in boundary set
extern TN_LIST * REGION_Get_TN_In_List (RID *rid);
extern TN_LIST * REGION_Get_TN_Out_List (RID *rid, INT exit_num);

#endif /* cg_region_INCLUDED */
