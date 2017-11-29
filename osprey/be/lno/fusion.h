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


// -*-C++-*-

/**
*** Module: fusion.h
*** $Revision$
*** $Date$
*** $Author$
*** $Source$
***
***			Fusion Utilities
***			-----------------
***
*** Description:
***
***     This file contains interface for all fusion utilities
***     available to LNO. They handle fusion for multiply-nested
***     loops. The loops to be fused have to be adjacent and have
***     the same stride.  Currently, the stride has to be known at
***     compile time while unknown bounds can be handled in a
***     few cases. Another constraint is that the difference
***     in lower bounds has to be known constant.  Otherwise,
***     it handles non-stride-one stride and negative stride.
***
***     For single level fusion (fusing loops with level 1),
***     prolog and epilog will be generated as a result of
***     alignment or mismatch in loop bounds. They will be
***     guarded with a IF statement but the IF can be
***     optimized away if the condition can be resolved at
***     compile time.  No loops will be generated for prolog
***     and epilog because we use fusion to get SNL
***     (Singly-Nested-Loop).  A threshold of how many
***     iterations can be unrolled for prolog and epilog can
***     be specified with a parameter to the Fuse() routine.
***
*** Exported functions:
***
***     FISSION_FUSION_STATUS
***         Fuse(WN* in_loop1, WN* in_loop2, mUINT8 fusion_level,
***             UINT32 threshold, BOOL peeling_unrolled,
***             UINT64 *prolog_out=NULL,
***             UINT64 *epilog_out=NULL, WN** epilog_loop_out = NULL,
***             UINT32 *offset_out[] = NULL)
***
***         This routine performs the fusion. 'in_loop1' and
***         'in_loop2' points to the (outermost loop of the) two
***         loop nests to be fused.  'fusion_level' gives the
***         desired nesting level of the fused loop.  A
***         'threshold' can be specified to limit amount of work
***         in prolog and epilog which are the peeled iterations
***         from the two loops to be fused.  When 'peeling_unrolled'
***         is set to FALSE, loops are created for the peeled
***         portion of the fused loops. If 'prolog_out',
***         'epilog_out', 'epilog_loop_out' and 'offset_out' are
***         not given, they will be computed automatically and the
***         results will be passed out through these variables and array.
***         If values are given for them, they will be used to
***         perform the fusion and no dependence analysis will be
***         done.  The function returns TRUE if the fusion is
***         successful, or FALSE otherwise.
***
***     void Pre_loop_peeling(WN* in_loop, UINT32 iter_count,
***          BOOL unrolled=TRUE, BOOL preserve_loop_index=TRUE);
***
***         Peel the first 'iter_count' iterations from 'in_loop'
***         and put them in front of the peeled 'in_loop'.
***         If 'unrolled' is TRUE, the peeled iterations are fully
***         unrolled, otherwise a loop will be generated.
***         Both array dependences and DU info are updated.
***         When 'preserve_loop_index' is set to TRUE, loop
***         index fanal value is preserved if it is live at loop exit.
***
***     void Post_loop_peeling(WN* in_loop, UINT32 iter_count,
***          BOOL unrolled=TRUE, BOOL preserve_loop_index=TRUE)
***
***         Peel the last 'iter_count' iterations from 'in_loop'
***         and put them after the peeled 'in_loop'.
***         Both array dependences and DU info are updated.
***         If 'unrolled' is TRUE, the peeled iterations are fully
***         unrolled, otherwise a loop will be generated.
***         When 'preserve_loop_index' is set to TRUE, loop
***         index fanal value is preserved if it is live at loop exit.
***
***     void Fusion_Init()
***
***         Fusion initialization routine which
***         initializes default mem_pool.
***
***     WN* Get_Only_Loop_Inside(const WN* wn, BOOL regions_ok)
***
***         A utility routine which find the only loop inside the given
***         loop 'wn'. Returns NULL if there are 0 or more than 1 loop inside.
***         When 'regions_ok' is FALSE and there are regions under wn,
***         returns NULL except when all the regions are under the
***         returned loop.
***
***     BOOL Move_Adjacent(WN* stmt1, WN* stmt2, BOOL move_up_only)
***
***         Move the two statements closer to each other to increase
***         the chance of a successful fusion. Dependence informations
***         are computed from the level dependence graph. Return TRUE
***         if stmt1 immediately preceeds stmt2 and FALSE otherwise.
***
***    INT Compare_Bounds(WN* bound1, WN* index1, WN* bound2, WN* index2)
***
***   	    Compare the lower (or upper) bounds of two loops
***   	    return 0 if the tree of the input bounds are identical
***   	    return -1 if the tree of the input bounds are different
***   	       bound1 =	the tree of the first bound
***   	       index1 =	the loop index of the first bound
***   	       bound2 = the tree of the second bound
***   	       index2 =	the loop index of the second bound
**/

#ifndef fusion_INCLUDED
#define fusion_INCLUDED

#include "wn.h"
#include "dep_graph.h"
#include "ff_utils.h"
#include "be_util.h"

// fuse two loops
extern FISSION_FUSION_STATUS
Fuse(WN* in_loop1, WN* in_loop2, mUINT8 fusion_level,
  UINT32 threshold, BOOL peeling_unrolled,
  UINT64 *prolog_out=NULL, UINT64 *epilog_out=NULL,
  WN** epilog_loop_out = NULL, mINT32 offset_out[] = NULL);

extern void Pre_loop_peeling(WN* in_loop, UINT32 iter_count,
                             BOOL unrolled=TRUE,
                             BOOL preserve_loop_index=TRUE,
                             BOOL do_precom = FALSE
                             );
extern void Post_loop_peeling(WN* in_loop, UINT32 iter_count,
                              BOOL unrolled=TRUE,
                              BOOL preserve_loop_index=TRUE);

extern void Fusion_Init();

extern void Fusion_Finish();

extern WN* Get_Only_Loop_Inside(const WN* wn, BOOL regions_ok);
extern BOOL Move_Adjacent(WN* stmt1, WN* stmt2, BOOL move_up_only);

extern INT Compare_Bounds(WN* bound1, WN* index1, WN* bound2, WN* index2);
extern BOOL Same_Bounds(WN *, WN *);
extern BOOL Pass_Child_Prefer_Fuse(WN *);
extern BOOL Has_Unit_Refs(WN *, int);
extern BOOL Check_Removable_Branch(WN *, int *, int *, WN **);
extern void Remove_Cond_Branch(WN *, WN *);
extern BOOL Has_Loop_Carried_Dependence(WN *);
extern BOOL Do_Loop_Depth(WN *);
#endif

