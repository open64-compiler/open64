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


#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <alloca.h>
#include "snl.h"
#include "snl_xbounds.h"
#include "config_targ.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "cxx_graph.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "wintrinsic.h"
#include "scalar_expand.h"
#include "strtab.h"
#include "dvector.h"
#include "lnopt_main.h"
#include "move.h"
#include "small_trips.h"
#include "sxlimit.h"
#include "ir_reader.h"

//-----------------------------------------------------------------------
// NAME: Scalar_Expansion_Tile
// FUNCTION: Returns TRUE if the 'wn_loop' is not a candidate for scalar
//   expansion tiling of size 'tile_size' because we believe the loop
//   will have fewer iterations than 'tile_size'.  FALSE otherwise.
// NOTE: We aren't always SURE that 'wn_loop' will have 'tile_size' or
//   fewer iterations, but we're at least pretty sure we're in the
//   ball park.
//-----------------------------------------------------------------------

static BOOL Scalar_Expansion_Tile(WN* wn_loop,
                                  INT tile_size)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  return !(!dli->Num_Iterations_Symbolic
      && dli->Est_Num_Iterations <= tile_size
      || dli->Est_Max_Iterations_Index != -1
      && dli->Est_Max_Iterations_Index <= tile_size
      || dli->Is_Inner_Tile && dli->Tile_Size > 0
      && dli->Tile_Size <= tile_size);
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Compute_Tile_Size
// FUNCTION: Compute the tile size of a scalar expanded tile, given that
//   the array will have 'depth' dimensions.
// RETURNS: The computed tile size.
//-----------------------------------------------------------------------

extern INT SNL_INV_Compute_Tile_Size(INT depth)
{
  FmtAssert(depth > 0,
    ("Scalar expanded array has null or negative number of dimensions."));
  if (LNO_SE_Tile_Size != 0)
    return LNO_SE_Tile_Size;
  if (depth == 1)
    return 1000;
  if (depth == 2)
    return 300;
  if (depth >= 3)
    return 80;
  return 1000/depth;
}

//-----------------------------------------------------------------------
// NAME: SE_Tile_Inner_Loop
// FUNCTION: Tile the 'loop' for scalar expansion, given that the dependence
//   graph is 'dg' and temporary memory is allocated out of 'pool'.  The
//   outer tile loop immediately encloses 'loop' when we are finished, and
//   its value is returned.
//-----------------------------------------------------------------------

extern WN* SE_Tile_Inner_Loop(WN* loop,
                            MEM_POOL *pool)
{
  if (!Scalar_Expansion_Tile(loop, SNL_INV_Compute_Tile_Size(1)))
    return NULL;
  if (!Upper_Bound_Standardize(WN_end(loop), TRUE))
    return NULL;
  return Tile_Loop(loop, SNL_INV_Compute_Tile_Size(1), 0, SNL_INV_SE_ONLY,
    NULL, pool);
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Add_Tile
// FUNCTION: Record the fact that the 'loop_index' loop has a tile size
//   of 'tile_size' in the 'iloop[]' and 'stripsz[]' arrays which before
//   the call are of length 'strips'.  Also add the reason for tiling 
//   'add_reason' to 'reason[]'. 
// RETURNS: The new length of 'strips'.
//-----------------------------------------------------------------------

static INT SNL_INV_Add_Tile(INT loop_index,
                             INT strips,
                             INT tile_size,
                             INT level,
			     SNL_INV_CACHE_BLOCK_REASON add_reason,  
                             INT iloop[],
                             INT stripsz[],
                             INT striplevel[], 
			     SNL_INV_CACHE_BLOCK_REASON reason[])
{
  INT i;
  for (i = 0; i < strips; i++)
    if (iloop[i] > loop_index)
      break;
  for (INT j = strips - 1; j >= i; j--) {
    iloop[j+1] = iloop[j];
    stripsz[j+1] = stripsz[j];
    striplevel[j+1] = striplevel[j];
  }
  iloop[i] = loop_index;
  stripsz[i] = tile_size;
  striplevel[i] = level;
  reason[i] = add_reason; 
  return ++strips;
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Remove_Tile
// FUNCTION: Remove the record that the 'loop_index' loop has a tile size
//   of 'tile_size' in the 'iloop[]' and 'stripsz[]' arrays which before
//   the call are of length 'strips'.
// RETURNS: The new length of 'strips'.
//-----------------------------------------------------------------------

static INT SNL_INV_Remove_Tile(INT loop_index,
                             INT strips,
                             INT iloop[],
                             INT stripsz[],
                             INT striplevel[])
{
  INT i;
  for (i = 0; i < strips; i++)
    if (iloop[i] == loop_index)
      break;
  FmtAssert(i < strips, ("Tried to remove non-existent tile."));
  for (INT j = i + 1; j < strips; j++) {
    iloop[j-1] = iloop[j];
    stripsz[j-1] = stripsz[j];
    striplevel[j-1] = striplevel[j];
  }
  return --strips;
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Update_Tile
// FUNCTION: Update the existing record to show that the 'loop_index' i
//   loop has a tile size of 'tile_size' in the 'iloop[]' and 'stripsz[]'
//   arrays which before the call are of length 'strips'.  Also change 
//   the reason for tiling to 'update_reason' in 'reason[]'. 
//-----------------------------------------------------------------------

static void SNL_INV_Update_Tile(INT loop_index,
                             INT strips,
                             INT tile_size,
                             INT level,
			     SNL_INV_CACHE_BLOCK_REASON update_reason, 
                             INT iloop[],
                             INT stripsz[],
                             INT striplevel[],
			     SNL_INV_CACHE_BLOCK_REASON reason[])
{
  INT i;
  for (i = 0; i < strips; i++)
    if (iloop[i] == loop_index)
      break;
  FmtAssert(i < strips, ("Tried to update non-existent tile."));
  stripsz[i] = tile_size;
  striplevel[i] = level;
  reason[i] = update_reason; 
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Get_Tile_Size
// FUNCTION: Given a list of 'strips' strip candidates, with candidate
//   indices stored in 'iloop[]' and strip sizes stored in 'stripsz[]',
//   return the size of the tile for the loop with index 'loop_index'.
//   If this loop is not included in the list, return 0.
//-----------------------------------------------------------------------

static INT SNL_INV_Get_Tile_Size(INT loop_index,
                                  INT strips,
                                  INT iloop[],
                                  INT stripsz[],
                                  INT striplevel[],
                                  INT* level)
{
  INT i;
  for (i = 0; i < strips; i++)
    if (iloop[i] == loop_index)
      break;
  return (i == strips ? 0 : (*level = striplevel[i], stripsz[i]));
}

//-----------------------------------------------------------------------
// NAME: SE_CT_New_Tile_Infos 
// FUNCTION: Given the SNL 'wn_outer' of 'nloops' loops to which we are 
//   applying the 'permutation' of length 'nloops', and which originally 
//   had a cache tiling info of 'ti', compute the new cache tiling info
//   ti_ct' and the new scalar expansion tiling info 'ti_se' which will 
//   result in fixed-size scalar expansion tiles for the SNL 'wn_outer' 
//   with scalar expansion variable list 'plist'.
//-----------------------------------------------------------------------

extern void SE_CT_New_Tile_Infos(WN* wn_outer, 
				 SX_PLIST* plist, 
                                 SNL_TILE_INFO *ti,
				 INT permutation[], 
                                 INT nloops,
                                 MEM_POOL* pool,
                                 SNL_TILE_INFO **ti_se,
                                 SNL_TILE_INFO **ti_ct, 
				 BOOL full_dist)
{
  // 797054: When creating scalar expansion tiles, we must take care not
  // to make loop nests deeper than 15, which is the maximum allowed for
  // dependence vectors (nenad, 2000/08/23).
  extern INT Num_Cache_Strips; // snl_test.cxx:Do_Automatic_Transformation
  INT max_strips_se = 15 - nloops - Num_Cache_Strips;

  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack); 

  FmtAssert(ti == NULL || ti->Rectangular(),
    ("Trying invariant code on non-invariant case."));
  FmtAssert(*ti_se == NULL,
    ("Scalar expansion tiles have not yet been computed."));
  FmtAssert(*ti_ct == NULL,
    ("Revised cache tiles have not yet been computed."));

  INT strips_se = 0;
  INT strips_ct = ti != NULL ? ti->Strips() : 0;
  INT iloop_se[SNL_MAX_LOOPS];
  INT* iloop_ct = (INT*)alloca(sizeof(INT)*strips_ct);
  INT stripsz_se[SNL_MAX_LOOPS];
  INT* stripsz_ct = (INT*)alloca(sizeof(INT)*strips_ct);
  INT striplevel_se[SNL_MAX_LOOPS];
  INT* striplevel_ct = (INT*)alloca(sizeof(INT)*strips_ct);
  INT strip_invariant[SNL_MAX_LOOPS];

  SNL_INV_CACHE_BLOCK_REASON reason_se[SNL_MAX_LOOPS]; 
  SNL_INV_CACHE_BLOCK_REASON reason_ct[SNL_MAX_LOOPS]; 

  INT i;
  for (i = 0; i < SNL_MAX_LOOPS; i++) 
    reason_se[i] = reason_ct[i] = SNL_INV_UNDEFINED; 

  for (i = 0; i < strips_ct; i++) {
    iloop_ct[i] = ti->Iloop(i);
    stripsz_ct[i] = ti->Stripsz(i);
    striplevel_ct[i] = ti->Striplevel(i);
  }
  for (i = 0; i < nloops; i++)
    strip_invariant[i] = FALSE;

  // Scan the privatization info, determining loops which must be stripped
  // for scalar expansion.
  INT first_in_stack = Do_Loop_Depth(wn_inner) - nloops + 1;
  INT outer = first_in_stack;
  SX_PITER ii(plist);
  INT* tpermutation = !full_dist ? permutation : NULL; 
  for (SX_PNODE* np = ii.First(); !ii.Is_Empty(); np = ii.Next()) {
    SX_PNODE::STATUS status = np->Transformable(outer, tpermutation, nloops);
    if (status == SX_PNODE::SE_NOT_REQD)
      continue;
    INT depth = np->Expansion_Depth() - first_in_stack + 1;
    for (i = 0; i < depth && strips_se < max_strips_se; i++) {
      INT lvl;
      if (!Scalar_Expansion_Tile(stack.Bottom_nth(first_in_stack+i),
          SNL_INV_Compute_Tile_Size(depth)))
        continue;
      if (strip_invariant[i])
        continue;
      INT cache_size = SNL_INV_Get_Tile_Size(i, strips_ct, iloop_ct,
        stripsz_ct, striplevel_ct, &lvl);
      if (cache_size > 0) {
        strips_se = SNL_INV_Add_Tile(i, strips_se, cache_size, lvl, 
          SNL_INV_TILE_SE, iloop_se, stripsz_se, striplevel_se, reason_se);
        strips_ct = SNL_INV_Remove_Tile(i, strips_ct, iloop_ct, stripsz_ct,
          striplevel_ct);
        strip_invariant[i] = TRUE;
        continue;
      }
      cache_size = SNL_INV_Get_Tile_Size(i, strips_se, iloop_se, stripsz_se,
        striplevel_se, &lvl);
      INT new_cache_size = SNL_INV_Compute_Tile_Size(depth);
      if (cache_size == 0) {
        strips_se = SNL_INV_Add_Tile(i, strips_se, new_cache_size, 0, 
          SNL_INV_SE_ONLY, iloop_se, stripsz_se, striplevel_se, reason_se);
      } else if (new_cache_size < cache_size) {
        SNL_INV_Update_Tile(i, strips_se, cache_size, lvl, SNL_INV_TILE_SE,
          iloop_se, stripsz_se, striplevel_se, reason_se);
      }
    }
  }

  // Check if all of the se-tile loops have constant bounds which are
  // small enough to preclude constant size scalar expansion tiling.

  BOOL found_big_loop = FALSE;
  INT max_block = 1;
  INT scalar_block = 1;
  for (i = 0; i < strips_se; i++) {
    WN* loop = stack.Bottom_nth(first_in_stack + iloop_se[i]);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
    if (dli->Num_Iterations_Symbolic && dli->Est_Max_Iterations_Index == -1) {
      found_big_loop = TRUE;
      break;
    }
    INT est_iterations = dli->Num_Iterations_Symbolic
      ? -1 : dli->Est_Num_Iterations;
    INT index_iterations = dli->Est_Max_Iterations_Index;
    FmtAssert(index_iterations != -1 || est_iterations != -1,
      ("Don't really have a loop with known loop iteration limit."));
    INT iterations = est_iterations == -1 ? index_iterations
      : index_iterations == -1 ? est_iterations
      : index_iterations < est_iterations ? index_iterations
      : est_iterations;
    max_block *= iterations;
    scalar_block *= SNL_INV_Compute_Tile_Size(strips_se);
  }
  if (!found_big_loop && max_block <= scalar_block)
    return;

  // Create new tile infos for scalar expansion and cache tiling.
  if (strips_se > 0)
    *ti_se = CXX_NEW(SNL_TILE_INFO(nloops, strips_se, iloop_se, stripsz_se,
      striplevel_se, reason_se, pool), pool);
  if (strips_ct > 0)
    *ti_ct = CXX_NEW(SNL_TILE_INFO(nloops, strips_ct, iloop_ct, stripsz_ct,
      striplevel_ct, reason_ct, pool), pool);
}

//-----------------------------------------------------------------------
// NAME: SE_New_Tile_Infos 
// RETURNS: For the SNL wn_outer to which we are applying a 'permutation' 
//   of 'nloops' loops and list of expandable scalars 'plist', compute a 
//   SNL_TILE_INFO 'ti_se' to describe the limited scalar expansion tiling 
//   which must be performed on it.  
//-----------------------------------------------------------------------

extern void SE_New_Tile_Infos(WN* wn_outer, 
			      SX_PLIST* plist, 
			      INT permutation[], 
                              INT nloops,
                              MEM_POOL* pool,
                              SNL_TILE_INFO **ti_se,
			      BOOL full_dist)
{
  SNL_TILE_INFO* ti_ct = NULL; 
  SE_CT_New_Tile_Infos(wn_outer, plist, NULL, permutation, nloops, pool, 
    ti_se, &ti_ct, full_dist); 
  FmtAssert(ti_ct == NULL, ("Cache tiling not being done here.")); 
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Get_Next_Outermost_Loop
// RETURNS: The loop immediately enclosing 'loop', if there is one, or
//   NULL if there is not.
//-----------------------------------------------------------------------

static WN* SNL_INV_Get_Next_Outermost_Loop(WN* loop)
{
  WN *wn;
  for (wn = LWN_Get_Parent(loop);
       wn != NULL && WN_opcode(wn) != OPC_DO_LOOP;
       wn = LWN_Get_Parent(wn));
  return wn;
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Scalar_Expand_Tile
// FUNCTION: For the SNL 'wn_outer' of 'nloops' loops and the tile info 
//   'ti', tile the nest so that constant size scalar expansion tiles 
//   will be created.  Update the 'region' over which we are transforming 
//   as well as finding 'the_newest_outer_loop'.  The created tile loops 
//   are returned in 'tile_loops[]'.
//-----------------------------------------------------------------------

static void SNL_INV_Scalar_Expand_Tile(WN* wn_outer, 
                                       SNL_TILE_INFO *ti,
                                       INT nloops,
                                       MEM_POOL *pool,
                                       WN* tile_loops[],
                                       WN** the_newest_outer_loop)
{
  if (ti == NULL)
    return;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  INT first_in_stack = Do_Loop_Depth(wn_inner) - nloops + 1;
  WN** compressed_tile_loops = (WN**)alloca(sizeof(WN**)*ti->Strips());
  INT i;
  for (i = 0; i < ti->Strips(); i++) {
    WN* loop_one = stack.Bottom_nth(first_in_stack + ti->Iloop(i));
    WN* new_outer_loop = Tile_Loop(loop_one, ti->Stripsz(i), ti->Striplevel(i),
      ti->Reason(i), NULL, &LNO_local_pool); 
    compressed_tile_loops[i] = SNL_INV_Get_Next_Outermost_Loop(loop_one);
  }
  for (i = 0; i < ti->Strips(); i++)
    tile_loops[i] = compressed_tile_loops[i];
  if (ti->Strips() > 0) {
    DOLOOP_STACK dostack(pool);
    Build_Doloop_Stack(LWN_Get_Parent(compressed_tile_loops[0]), &dostack);
    LNO_Build_Access(compressed_tile_loops[0], &dostack, &LNO_default_pool);
  }
  if (Do_Loop_Depth(compressed_tile_loops[0]) 
      < Do_Loop_Depth(*the_newest_outer_loop))
    *the_newest_outer_loop = compressed_tile_loops[0];
}
//-----------------------------------------------------------------------
// NAME: SNL_INV_Get_Dist_Limits
// FUNCTION: Compute the 'first_dist_index' and 'dist_count'.  These are
//   the index of the first loop and the number of loops after and in-
//   cluding it in the stack 'all_loop_stack' over which distribution
//   and permutation are to be applied for the 'section_number' section
//   of the nest.  We assume that 'nstrips' number of 'tile_loops[]'
//   have been created, so that valid section numbers start at 0 and
//   continue to 'nstrips'.  The created tile loops are stored in
//   'tile_loops[]'.  There are 'nloops_total' loops, counting the
//   original loops and the tile loops.
//-----------------------------------------------------------------------

static void SNL_INV_Get_Dist_Limits(INT section_number,
                                    INT nstrips,
                                    DOLOOP_STACK* all_loop_stack,
                                    WN* tile_loops[],
                                    INT first_stack_index,
                                    INT nloops_total,
                                    INT& first_dist_index,
                                    INT& dist_count)
{
  FmtAssert(section_number >= 0 && section_number <= nstrips,
    ("Section number out of range."));
  INT last_dist_index = 0;
  INT upper_limit = first_stack_index + nloops_total;
  if (section_number == 0) {
    first_dist_index = first_stack_index;
  } else {
    INT i;
    for (i = first_stack_index; i < upper_limit; i++)
      if (all_loop_stack->Bottom_nth(i) == tile_loops[section_number - 1])
        break;
    FmtAssert(i < upper_limit, ("Stack index out of range."));
    first_dist_index = i + 1;
  }
  if (section_number == nstrips) {
    last_dist_index = nloops_total - 1;
  } else {
    INT i;
    for (i = first_dist_index; i < upper_limit; i++)
      if (all_loop_stack->Bottom_nth(i) == tile_loops[section_number])
        break;
    FmtAssert(i < upper_limit, ("Stack index out of range."));
    last_dist_index = i - 1;
  }
  dist_count = last_dist_index - first_dist_index + 1;
  if (dist_count < 0)
    dist_count = 0;
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Distribute_Block_Of_Loops
// FUNCTION: Distribute the section of loops in 'all_loop_stack' from 
//   'first_loop_index' to 'first_loop_index' + 'loop_count'.  The first 
//   loop in 'all_loop_stack' which is part of the singly nested loop has 
//   index 'first_in_stack'.  The 'region' over which the transformations 
//   are applied and 'the_newest_outer_loop' are also updated.
//-----------------------------------------------------------------------

static void SNL_INV_Distribute_Block_Of_Loops(DOLOOP_STACK* all_loop_stack,
                                              INT first_in_stack,
                                              INT first_loop_index,
                                              INT loop_count,
                                              WN** the_newest_outer_loop)
{
  WN* newup = NULL;
  WN* newdown = NULL;
  for (INT lp = first_loop_index + 1; lp < first_loop_index + loop_count + 1;
    lp++) {
    if (lp >= all_loop_stack->Elements())
      continue;
    WN* wn = all_loop_stack->Bottom_nth(lp);
    if (WN_prev_executable(wn)) {
      if (newup == NULL)
        newup = SNL_Distribute(all_loop_stack, lp, first_loop_index, TRUE);
      else
        SNL_Distribute(all_loop_stack, lp, first_loop_index, TRUE);
    }
    if (WN_next_executable(wn)) {
      if (newdown == NULL)
        newdown = SNL_Distribute(all_loop_stack, lp, first_loop_index, FALSE);
      else
        SNL_Distribute(all_loop_stack, lp, first_loop_index, FALSE);
    }
  }
  if (first_loop_index == first_in_stack) {
    if (newup) 
      *the_newest_outer_loop = newup;
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Shift_Loops
// FUNCTION: Perform a circular shift right (e.g. 0 1 2 -> 2 0 1) of
//   the loops in 'all_loop_stack' between 'first_loop_index' and
//   'second_loop_index'. Update the 'region' on which the transforma-
//   tions are applied and 'the_newest_outer_loop'.  There are 'nloops_total'
//   loops in the nest being considered.  The 'plist' of privatization
//   information may be updated as well.
//-----------------------------------------------------------------------

static void SNL_INV_Shift_Loops(DOLOOP_STACK* all_loop_stack,
                                INT first_in_stack,
                                INT first_loop_index,
                                INT second_loop_index,
                                INT nloops_total,
                                WN** the_newest_outer_loop)
{
  if (first_loop_index == second_loop_index)
    return;
  if (first_loop_index < first_in_stack || second_loop_index >= nloops_total)
    return;
  INT permutation[SNL_MAX_LOOPS];
  WN* permloop[SNL_MAX_LOOPS];
  INT i;
  for (i = 0; i < nloops_total - first_in_stack; i++)
    permutation[i] = i;
  for (i = second_loop_index; i >= first_loop_index + 1; i--)
    permutation[i-first_in_stack] = permutation[i-1-first_in_stack];
  permutation[first_loop_index-first_in_stack] =
    second_loop_index-first_in_stack;
  for (i = 0; i < nloops_total - first_in_stack; i++) 
    permloop[i] = all_loop_stack->Bottom_nth(first_in_stack + permutation[i]);
  INT short_permutation[SNL_MAX_LOOPS]; 
  INT j;
  for (j = 0; j < nloops_total - first_in_stack; j++)
    if (permutation[j] != j) 
      break;
  for (INT k = j; k < nloops_total - first_in_stack; k++) 
    short_permutation[k - j] = permutation[k] - j; 
  WN* wn_outer_loop = all_loop_stack->Bottom_nth(first_in_stack + j);
  WN* outer_perm_loop = SNL_INV_Permute_Loops(wn_outer_loop, 
    short_permutation, nloops_total - first_in_stack - j, TRUE); 
  for (i = 0; i < nloops_total - first_in_stack; i++)
    all_loop_stack->Bottom_nth(first_in_stack + i) = permloop[i]; 
  if (first_loop_index == first_in_stack)
    *the_newest_outer_loop = outer_perm_loop;
}
//-----------------------------------------------------------------------
// NAME: SNL_INV_SE_Distribute
// FUNCTION: Perform a scalar expansion distribution of the SNL 
//   'wn_new_outer' which origianlly had 'nloops' loops but now has 
//   'nloops + nstrips' loops.  The 'nstrips' tile loops have been 
//    already been added, which are specified in 'tile_loops[]'.  The 
//   'region' operated on and 'the_newest_outer_loop' are also updated.
//-----------------------------------------------------------------------
static void SNL_INV_SE_Distribute(WN* wn_new_outer, 
				  INT first_in_stack, 
                                  INT nloops,
                                  WN* tile_loops[],
                                  INT nstrips,
                                  MEM_POOL* pool,
                                  WN** the_newest_outer_loop)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;

  DOLOOP_STACK all_loop_stack(pool);
  INT nloops_total = first_in_stack + nloops + nstrips;
  INT first_dist_index = 0;
  INT dist_count = 0;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_new_outer, nloops + nstrips);  
  Build_Doloop_Stack(wn_inner, &all_loop_stack); 
  for (INT i = 0; i <= nstrips; i++) {
    SNL_INV_Get_Dist_Limits(i, nstrips, &all_loop_stack, tile_loops,
      first_in_stack, nloops_total, first_dist_index, dist_count);
    SNL_INV_Distribute_Block_Of_Loops(&all_loop_stack, first_in_stack,
      first_dist_index, dist_count, the_newest_outer_loop);
    INT second_dist_index = first_dist_index + dist_count;
    SNL_INV_Shift_Loops(&all_loop_stack, first_in_stack, first_dist_index, 
      second_dist_index, nloops_total, the_newest_outer_loop);
  }
  DOLOOP_STACK do_stack(pool);
  Build_Doloop_Stack(LWN_Get_Parent(*the_newest_outer_loop), &do_stack);
  LNO_Build_Access(*the_newest_outer_loop, &do_stack, &LNO_default_pool);
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Compact_Scalar_Expand
// FUNCTION: Apply scalar expansion to the SNL described by 'original_stack' 
//   whose first loop is at the 'first_in_stack' index, given that this 
//   inner kernel of loops will be permuted by the 'permutation' of length
//   'nloops'.  Fixed size scalar tiles are created. The tile-info 'ti_se' 
//   summarizes how the loops are tiled for scalar expansion.  We assume 
//   that this tiling has already been done, and that 'nstrips' tile loops, 
//   stored in 'tile_loops' was the result.
//-----------------------------------------------------------------------

static void SNL_INV_Compact_Scalar_Expand(DOLOOP_STACK* original_stack,
					  INT first_in_stack, 
					  SX_PLIST* plist, 
                                          SNL_TILE_INFO* ti_se,
                                          WN* tile_loops[],
                                          INT nstrips,
                                          INT permutation[],
					  INT nloops, 
					  WN* guard_tests[], 
					  BOOL full_dist)
{
  ARRAY_DIRECTED_GRAPH16*       dg = Array_Dependence_Graph;
  DU_MANAGER*                   du = Du_Mgr;
  SX_PITER ii(plist);
  SX_PNODE* nnext = NULL;

  INT* tpermutation = !full_dist ? permutation : NULL; 
  for (SX_PNODE* n = ii.First(); !ii.Is_Empty(); n = nnext) {
    nnext = ii.Next();

    SNL_DEBUG1(3, "SNL_INV_Compact_Scalar_Expand() consider expanding %s\n",
               n->Symbol().Name());

    SX_PNODE::STATUS status = n->Transformable(first_in_stack, tpermutation, 
      nloops);
    if (status == SX_PNODE::SE_NOT_REQD)
      continue;
    FmtAssert(status == SX_PNODE::SE_REQD,
              ("Bug: can't expand scalar %s", n->Symbol().Name()));

    // The purpose of the variable 'order' is as a mini-permutation matrix.
    // Scalar_Expand is told the future order of the loops, so that
    // it can choose a sensible stride one loop to expand into.

    WN* loops[SNL_MAX_LOOPS];
    INT order[SNL_MAX_LOOPS];
    INT dimcnt = n->Expansion_Depth() - first_in_stack + 1;
    INT lp;
    for (lp = 0; lp < dimcnt; lp++) {
      loops[lp] = original_stack->Bottom_nth(first_in_stack+lp);
      order[lp] = lp;
    }

    INT strip_sizes[SNL_MAX_LOOPS];
    INT i;
    for (i = 0; i < dimcnt; i++)
      strip_sizes[i] = 0;
    for (i = 0; i < ti_se->Strips(); i++)
      strip_sizes[ti_se->Iloop(i)] = ti_se->Stripsz(i);

    if (permutation) {
      for (i = 0; i < lp; i++) {
        INT jsmall = -1;
        for (INT j = 0; j < lp; j++) {
          BOOL ok = TRUE;
          for (INT ii = 0; ii < i; ii++)
            if (order[ii] == j)
              ok = FALSE;
          if (ok && (jsmall == -1 || permutation[jsmall] > permutation[j]))
            jsmall = j;
        }
        order[i] = jsmall;
      }
    }

    INT this_symbols_nstrips = 0;
    for (i = 0; i < dimcnt; i++)
      if (strip_sizes[i] > 0)
        this_symbols_nstrips++;

    WN* outermost_loop = loops[0];
    if (nstrips > 0 && Get_Do_Loop_Info(tile_loops[0])->Depth
        < Get_Do_Loop_Info(loops[0])->Depth)
      outermost_loop = tile_loops[0];

    Scalar_Expand(outermost_loop,
                  original_stack->Bottom_nth(n->Expansion_Depth()),
                  n->Wn_Symbol(), n->Symbol(), loops, order, dimcnt, 
                  TRUE, n->Finalize(), FALSE, guard_tests, NULL, 
		  tile_loops, strip_sizes, this_symbols_nstrips);

    plist->Remove(n);
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Limited_SE_And_Dist  
// FUNCTION: For the innermost SNL 'wn_outer' of 'nloops' loops, tile it 
//  for scalar expansion and distribute the loops according to the infor-
//  mation in 'ti_se'. The 'plist' is a list of expandable scalars made
//  by calling SX_INFO::Make_Sx_Info().  Subscripts in the scalar expanded
//  array are made to correspond to the 'permutation' of length 'nloops'.  
//  The 'full_dist' is TRUE if the scalar expansion is being provided to i
//  transform a SNL into a perfect nest via distribution, FALSE if the 
//  scalar expansion is being provided only to enable the permutation. 
//-----------------------------------------------------------------------

extern WN* SNL_INV_Limited_SE_And_Dist(WN* wn_outer,
				       SNL_TILE_INFO* ti_se, 
                                       INT permutation[], 
                                       INT nloops,
                                       SX_PLIST* plist,
				       BOOL full_dist)
{
  if (ti_se == NULL) 
    return NULL; 

  INT outer_depth = Do_Loop_Depth(wn_outer);
  INT guard_depth = SE_Guard_Depth(wn_outer, permutation, nloops, plist, 
    -1, NULL, FALSE, full_dist);  
  INT guard_loops = guard_depth - outer_depth + 1;
  WN** guard_tests = guard_depth == -1 
    ? NULL : CXX_NEW_ARRAY(WN*, guard_loops, &LNO_local_pool);
  SE_Guard_Tests(wn_outer, nloops, guard_tests, guard_depth);

  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK original_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &original_stack);
  INT first_in_stack = Do_Loop_Depth(wn_inner) - nloops + 1;
  WN* wn_new_outer = wn_outer; 
  WN** tile_loops = (WN**) alloca(sizeof(WN**) * ti_se->Strips());
  SNL_INV_Scalar_Expand_Tile(wn_outer, ti_se, nloops, &LNO_local_pool,
    tile_loops, &wn_new_outer);
  SNL_INV_Compact_Scalar_Expand(&original_stack, first_in_stack, plist,
    ti_se, tile_loops, ti_se->Strips(), permutation, nloops, guard_tests,
    full_dist);
  SNL_INV_SE_Distribute(wn_new_outer, first_in_stack, nloops, tile_loops,
    ti_se->Strips(), &LNO_local_pool, &wn_new_outer);
  return wn_new_outer;
}

