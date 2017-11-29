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


//-*-c++-*-
#ifndef ff_utils_INCLUDED
#define ff_utils_INCLUDED

/**
*** Module: ff_utils.h
*** $Revision$
*** $Date$
*** $Author$
*** $Source$
*** 
*** Description:
*** 
***   Bunch of utilities related to Fission and Fusion.
***   To be described later.
*** 
**/

#ifdef _KEEP_RCS_ID
static char *ff_utils_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "wn_core.h"
#include "name.h"
#include "dep_graph.h"
#include "lwn_util.h"
#include "reduc.h"
#include "btree.h"
#include "lno_bv.h"

#define Gather_Scalar_Refs 0x0001
#define Gather_Array_Refs  0x0002
#define Gather_Params  0x0004
#define Gather_Other_Refs  0x0008
#define Gather_Calls       0x0010
#define Gather_IOs         0x0020
#define Reset_Ref_Counter  0x1000

typedef enum {
  Failed=0,
  Succeeded=1,
  Succeeded_and_Inner_Loop_Removed=2,
    // for cases where innermost loop disappear after fusion and peeling
  Partially_fused=3,
    // in this case, two SNL were partially fused and a non-SNL is created
  Try_Level_By_Level=4,
  // Retry after 1st SNL is peeled.
  Retry_and_Peel_First_SNL = 5,
  // Retry after 2nd SNL is peeled.
  Retry_and_Peel_Second_SNL = 6,
} FISSION_FUSION_STATUS;

typedef STACK<SCALAR_REF> SCALAR_REF_STACK;

// trace whirl tree upward starting from source_wn and sink_wn
// and try to find the two stmt, source_stmt and sink_stmt which are
// the ancesstors of source_wn and sink_wn, respectively
// and these two statements are sibling themselves
extern UINT Get_Stmt_For_Stmt_Dep_Graph(
        WN* source_wn, WN* sink_wn,
        WN** source_stmt_out, WN** sink_stmt_out);

extern INT Map_Stmt_To_Level_Graph(WN* wn, ARRAY_DIRECTED_GRAPH16 *sdg);
extern void toplogical_reordering(WN* loop, UINT depth,
	ARRAY_DIRECTED_GRAPH16* adg);

// Scan all scalar variables defined in the given loop and determine if
// they can be renamed.
extern BOOL Scalar_Variable_Renaming(WN* loop);

extern BOOL scalar_rename(WN* ref, HASH_TABLE<WN*,INT>* checked=NULL);

// Update DU-chains in 'Du_Mgr' after fission.
// wn_starts, wn_ends, wn_steps are three arrays of WN expressions for
// loop start expression, end conditions, and step expressions. They
// 'total_loops' elements if there are 'total_loops' loops after
// fission. 'loop' is the first fissioned loop.
extern void Fission_DU_Update(DU_MANAGER* Du_Mgr, REDUCTION_MANAGER* Red_Mgr,
  WN** wn_starts, WN** wn_ends, WN** wn_steps,
  UINT total_loops, WN** loop, BOOL index_DU_to_first=FALSE);

// return -1 if statement containing wn1 is lexically before
//              statement containing wn2
// return  0 if statement containing wn1 is the same as
//              statement containing wn2
// return +1 if statement containing wn1 is lexically after
//              statement containing wn2
extern INT Lexical_Order(WN* wn1, WN* wn2);

// find a inner loop enclosed by 'parent_wn'. Used to determine if
// 'parent_wn' has inner loop or not after fusion, vintr_fission, etc.

extern WN* Find_SCF_Inside(WN* parent_wn, OPCODE opc);

extern void FF_Mark_Inner_Loop_Info(WN* loop);

extern void Init_Ref_Stmt_Counter();

extern INT32 New_Gather_References(WN *wn, REF_LIST_STACK *writes,
        REF_LIST_STACK *reads, DOLOOP_STACK *stack,
        SCALAR_STACK *scalar_writes, SCALAR_STACK *scalar_reads,
        SCALAR_REF_STACK *params, MEM_POOL *pool,
        INT mode=Gather_Array_Refs | Gather_Scalar_Refs | Gather_Params);

BOOL Generate_Scalar_Dependence_For_Statement_Dependence_Graph(
  WN* in_loop,
  SCALAR_STACK *scalar_reads,
  SCALAR_STACK *scalar_writes,
  SCALAR_REF_STACK *params,
  ARRAY_DIRECTED_GRAPH16* sdg,
  REDUCTION_MANAGER* Red_Mgr,
  BIT_VECTOR *Expandable_Scalar_Set=NULL,  // set of expandable scalars
  BINARY_TREE<NAME2BIT> *mapping_dictionary=NULL
                // dictionary which records mapping from
                // symbol names to bit positions
  );

BOOL Generate_Array_Dependence_For_Statement_Dependence_Graph(
  WN* in_loop,
  REF_LIST_STACK *reads,
  REF_LIST_STACK *writes,
  ARRAY_DIRECTED_GRAPH16* sdg,
  REDUCTION_MANAGER* Red_Mgr,
  ARRAY_DIRECTED_GRAPH16* adg);

extern ARRAY_DIRECTED_GRAPH16* Statement_Dependence_Graph; 

extern ARRAY_DIRECTED_GRAPH16* Build_Statement_Dependence_Graph(
	WN* in_loop, REDUCTION_MANAGER* Red_Mgr,
	ARRAY_DIRECTED_GRAPH16* adg, WN_MAP sdm, MEM_POOL* pool);

// Test if a dependence edge is part of a reduction
extern BOOL Edge_Is_Reduction_Dependence(EINDEX16 edge,
					 ARRAY_DIRECTED_GRAPH16 *dg,
					 REDUCTION_MANAGER *Red_Mgr);

#endif // ff_utils_INCLUDED

