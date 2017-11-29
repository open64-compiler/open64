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
//============================================================================
//
// Module: rail.cxx
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:58-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.rail.cxx $
//
// Revision history:
//  24-AUG-95 dahl - Original Version
//
// Description:
//	Regions Around Inner Loops (RAIL)
//	Find inner loops, insert regions around them.
//
// This code is linked in with lno.so
//
//============================================================================

#define rail_CXX	"rail.cxx"
#ifdef _KEEP_RCS_ID
static char *rcs_id = rail_CXX"$Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "wn.h"			/* WN type		*/
#include "wn_util.h"		/* wn accessors		*/
#include "rail.h"		/* RAIL class		*/
#include "ir_reader.h"		/* for fdump_tree	*/
#include "region_util.h"	/* RID structure	*/
#include "tracing.h"		/* TFile		*/

RAIL::RAIL(void)
{
  _trace_flag = Get_Trace(TP_REGION,TT_REGION_RAIL_DEBUG,);
}

// max number of nested blocks
#define BLOCK_STACK_SIZE	20

void
RAIL::Process_func_entry(WN *wn)
{
  // loop through all func_entries in this WHIRL tree
  Set_fe(wn);		// set pointer to first PU
  do {
    FmtAssert(WN_opcode(Get_fe()) == OPC_FUNC_ENTRY,
	    ("RAIL::Process_func_entry, could not find function entry"));
    if (Trace())
      fprintf(TFile,"===== RAIL::Process_func_entry: %s\n",
	      ST_name(WN_st(Get_fe())));
    Process_block(WN_func_body(Get_fe()),1); // level 0 is PU
    RID *rid = REGION_get_rid(Get_fe());
    RID_level(rid) = RL_RAIL; // processed by RAIL
    RID_bounds_exist(rid) = REGION_BOUND_UNKNOWN; // no boundary info yet
    RID_has_return(rid) = REGION_NO_RETURN; // not known yet
    Next_fe();		// go to next PU
  } while (Get_fe());	// while there are no more PUs
}

// returns TRUE if found a loop, FALSE otherwise
BOOL
RAIL::Process_block(WN *wn_block, INT32 nest_level)
{
  WN *wn_after_loop = NULL;
  BOOL found_loop = FALSE;

  FmtAssert(WN_opcode(wn_block) == OPC_BLOCK,
	    ("RAIL::Process_block, could not find block"));

  // go through all statements in block
  for (WN *wtmp=WN_first(wn_block); wtmp!=NULL; wtmp=WN_next(wtmp)) {
    const OPCODE opc = WN_opcode(wtmp);
    const OPERATOR opr = OPCODE_operator(opc);

    // look for loop beginning
    if (opr == OPR_DO_LOOP || opr == OPR_WHILE_DO || opr == OPR_DO_WHILE) {
      found_loop = TRUE;	// there is a loop in this block
      if (WN_next(wtmp))
	wn_after_loop = WN_next(wtmp);

      // follow block recursively
      WN *wn_body = (opr == OPR_DO_LOOP) ? WN_do_body(wtmp) :
      					   WN_while_body(wtmp);
      if (!Process_block(wn_body,nest_level+1)) {
	if (Trace())
	  fprintf(TFile,
	    "===== RAIL::Process_block, %s begin, level %d, inner loop\n",
		  OPCODE_name(opc),nest_level);
	// pull loop out of block
	wtmp = WN_EXTRACT_FromBlock(wn_block,wtmp);
	// add region around the loop, create RID, connect to RID tree
	WN *wnew = Add_region_around_loop(wtmp,nest_level);
	// add gotos for exits and label for fall-through
	wn_after_loop = REGION_add_exit(wn_block,wn_after_loop,wnew);
	// patch result back into block
	WN_INSERT_BlockBefore(wn_block,wn_after_loop,wnew);
	// point wtmp to inserted region and continue scanning from there
	wtmp = wnew;
      } else {
	if (Trace())
	  fprintf(TFile,"===== RAIL::Process_block, %s begin, level %d\n",
		  OPCODE_name(opc),nest_level);
      }
    }

    // handle SCF (Structured Control Flow) nodes, loops handled above
    switch (opc) {
      case OPC_BLOCK:
      	found_loop |= Process_block(wtmp,nest_level);
	break;
      case OPC_REGION:
      	found_loop |= Process_block(WN_region_body(wtmp),nest_level);
	break;
      case OPC_IF:
      	found_loop |= Process_block(WN_then(wtmp),nest_level);
      	found_loop |= Process_block(WN_else(wtmp),nest_level);
	break;
      default:
	break;
    }

  } // end for loop

  return found_loop;
}

// TODO: need to search through PU and find all exits and build goto table
WN *
RAIL::Add_region_around_loop(WN *wn_loop, INT32 nest_level)
{
  // create a new region for this inner loop
  WN *wnew = WN_CreateRegion(REGION_KIND_PRAGMA, wn_loop,NULL,NULL,
			     RID_CREATE_NEW_ID,NULL);

  // create RID for the new region and add to map
  RID *rid = RID_Create(WN_region_id(wnew), nest_level, wnew); // sets depth
  RID_level(rid) = RL_RAIL;
  RID_bounds_exist(rid) = REGION_BOUND_UNKNOWN; // no boundary info yet
  RID_has_return(rid) = REGION_NO_RETURN; // not known yet
  RID_num_exits(rid) = 1;
  RID_TYPE_loop_Set(rid); // this RID is strictly a loop
  WN_MAP_Set(RID_map,wnew,(void *)rid);
  if (Trace())
    fprintf(TFile,"===== RAIL::Add_regions_around_loop, New RID: %d\n",
	    RID_id(rid));

  // link new RID into RID tree, the PU has the root RID
  RID *root_rid = REGION_get_rid(Get_fe());
  FmtAssert(root_rid != NULL,
	    ("RAIL::Add_region_around_loop, can't find root RID"));
  RID_Add_kid(rid,root_rid);

  return wnew;
}

extern "C"	/* so lnodriver.c can call this entry point	*/
void
Rail(WN *itree)
{
  RAIL rtmp;

  if (!PU_has_region (Get_Current_PU ()))
      return;
  
  FmtAssert(WN_opcode(itree) == OPC_FUNC_ENTRY,
	    ("Rail must be called on a PU"));

  // go through all statements looking for inner loops
  rtmp.Process_func_entry(itree);

  if (rtmp.Trace()) {
    fprintf(TFile,"===== RAIL finished\n");
    RID_WN_Tree_Print(TFile,itree);
  }
}
