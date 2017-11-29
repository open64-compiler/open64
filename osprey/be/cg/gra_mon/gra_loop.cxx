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

//
//  GRA_LOOP implementation

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "defs.h"
#include "errors.h"
#include "cg.h"
#include "cg_region.h"
#include "bb.h"
#include "findloops.h"
#include "cg_flags.h"
#include "gra_bb.h"
#include "gra_loop.h"
#include "gra_trace.h"

GRA_LOOP_MGR gra_loop_mgr;

/////////////////////////////////////
// set the register used bitset in the loop for a register.
void
GRA_LOOP::Make_Register_Used( ISA_REGISTER_CLASS rc, REGISTER reg,
			      BOOL reclaim  )
{
#ifdef KEY
  // Nothing to do for reclaiming, since REG was already marked used.
  if (reclaim)
    return;
#endif

  if (this == NULL) return;

  registers_used[rc] = REGISTER_SET_Union1(registers_used[rc],reg);
  GRA_LOOP *li;
  for (li = parent; li != NULL; li = li->parent) {
    li->registers_used[rc] = REGISTER_SET_Union1(li->registers_used[rc],reg);
  }
}


#ifdef KEY
/////////////////////////////////////
// Return the set of registers referenced in the loop.
REGISTER_SET
GRA_LOOP::Registers_Referenced (ISA_REGISTER_CLASS rc)
{
  BB *bb;
  BB_SET *loop_bbs = LOOP_DESCR_bbset(Loop_Descr());
  REGISTER_SET referenced = REGISTER_SET_EMPTY_SET;

  for (bb = BB_SET_Choose(loop_bbs);
       bb != BB_SET_CHOOSE_FAILURE;
       bb = BB_SET_Choose_Next(loop_bbs, bb)) {
    GRA_BB* gbb = gbb_mgr.Get(bb);
    Is_True(gbb != NULL, ("GRA_LOOP::Registers_Referenced: cannot find gbb"));
    referenced = REGISTER_SET_Union(referenced, gbb->Registers_Referenced(rc));
  }
  return referenced;
}

#endif

/////////////////////////////////////
//  create a GRA_LOOP node for a loop represented by LOOP_DESCR ld.
//  get its nesting level, and set up a map from the head of the loop
//  to the GRA_LOOP structure created for it.
GRA_LOOP*
GRA_LOOP_MGR::Create(LOOP_DESCR *ld)
{
  ISA_REGISTER_CLASS rc;

  GRA_LOOP* gloop = TYPE_MEM_POOL_ALLOC(GRA_LOOP, GRA_pool);
#ifdef KEY
  BZERO( gloop, sizeof(gloop[0]) );
  gloop->Loop_Descr_Set(ld);
#endif
  gloop->Nest_Level_Set(LOOP_DESCR_nestlevel(ld));
  gloop->Loop_Head_Set(LOOP_DESCR_loophead(ld));
  BB_MAP_Set(_map, LOOP_DESCR_loophead(ld), (void *) gloop);
  
  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    gloop->Registers_Used_Init(rc);
  }
  return gloop;
}


/////////////////////////////////////
//  traverse list of loops, and build heirarchical representation of
//  loop nests using the nest depth of the loops (they are given in
//  order, beginning with the outermost).
GRA_LOOP*
GRA_LOOP::Make_Loop_Nest(GRA_LOOP *outermost, GRA_LOOP *parent)
{
  GRA_LOOP *loop, *next;

  for (loop = this; loop != NULL; loop = next) {
    next = loop->Next_Loop();
    loop->Parent_Set(parent);
    loop->Outermost_Set(outermost);

    if (next != NULL) {
      INT next_level = next->Nest_Level();
      INT cur_level = loop->Nest_Level();
      //
      // found an inner nest.  begin new nest with it as parent.
      if ( next_level > cur_level ) {
	next = next->Make_Loop_Nest(outermost, loop);
      } else if (next_level < cur_level) {
	//
	// popped out of inner nest.  return this loop.  it will
	// be the sibling of the outermost loop of this sub-nest.
	//
	break;
      }
    }
  }
  return(next);
}

/////////////////////////////////////
// use cg's loop finding code to detect loops.  GRA's heirarchical
// loop representation is constructed from this list of loops.
// we also construct a map that allows us to find the GRA_LOOP
// structure for any block.
void 
GRA_LOOP_MGR::Find_Loops(void)
{
  LOOP_DESCR *cgl;
  GRA_LOOP *graloop, *prev;

  //
  // find the loops in the PU (or region)
  //
  cg_loops = LOOP_DESCR_Detect_Loops(GRA_pool);
  if (cg_loops == NULL) return;
  if (GRA_Trace_Loops()) {
    LOOP_DESCR_Print_List();
  }

  _map = BB_MAP_Create();

  // process the loops on a nest by nest basis.  make a list of all the
  // loops in a nest.  i.e. traverse list until a loop of level 1 is 
  // found.  this is the outermost loop, and all loops seen before it
  // (and after the last outermost loop) are innermore loops to its nest.
  //
  for (prev = NULL, cgl = cg_loops;
       cgl != NULL;
       cgl = LOOP_DESCR_next(cgl), prev = graloop) {
    graloop = Create(cgl);
    graloop->Next_Loop_Set(prev);
    if (LOOP_DESCR_nestlevel(cgl) == 1) {
      (void) graloop->Make_Loop_Nest(graloop, NULL);
    }
  }
}
  
/////////////////////////////////////
// see if a GRA_BB represents a block in a loop, and if so, point
// it at its corresponding GRA_LOOP node.
void 
GRA_LOOP_MGR::Set_GBB_Loop(GRA_BB *gbb)
{
  LOOP_DESCR *bb_loop;

  if (!GRA_loop_splitting) {
    gbb->Loop_Set(NULL);
  } else {
    bb_loop = LOOP_DESCR_Find_Loop(gbb->Bb());
    if (bb_loop != NULL) {
      gbb->Loop_Set( (GRA_LOOP *) BB_MAP_Get(_map,
					     LOOP_DESCR_loophead(bb_loop)));
    } else {
      gbb->Loop_Set(NULL);
    }
  }
}

