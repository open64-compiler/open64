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


#include "defs.h"
#include "config.h"
#include "cg_flags.h"
#include "bb.h"
#include "hb.h"
#include "hb_cflow.h"


static void Delete_HB_From_List(HB *hb)
{
  std::list<HB *>::iterator hbi;
  for (hbi = HB_list.begin(); hbi != HB_list.end(); ++hbi) {
    if (*hbi == hb) {
      HB_list.erase(hbi);
      return;
    }
  }
}


//////////////////////////////////
void HB_CFLOW_Replace_Block(BB *old_bb, BB *new_bb)
/////////////////////////////////////
//  Replace block old_bb with block new_bb in hyperblock hb
/////////////////////////////////////
{
  HB *hb;
  HB_bb_list::iterator bbi;
  if (!HB_formation) return;
  hb = (HB *) BB_MAP_Get(HB_bb_map,old_bb);
  if (!hb) return;
  // Fix up the entry block
  if (HB_Entry(hb) == old_bb) {
    HB_Entry_Set(hb,new_bb);
  }
  // Replace the blocks in the bitsets
  if (HB_Contains_Block(hb,new_bb)) return;
  hb->blocks = BB_SET_Difference1D(HB_Blocks(hb), old_bb);
  hb->blocks = BB_SET_Union1D(HB_Blocks(hb), new_bb, &MEM_pu_pool);
  // replace the block in the list
  for (bbi = hb->block_list.begin(); bbi != hb->block_list.end(); ++bbi ) {
    if (*bbi == old_bb) {
      hb->block_list.insert(bbi,new_bb);
      hb->block_list.erase(bbi);
      // We found it, so we can exit early
      break;
    }
  }
  // redo the map.
  BB_MAP_Set(HB_bb_map,new_bb,hb);
  BB_MAP_Set(HB_bb_map,old_bb,NULL);
}

//
// See if it's safe to merge two BBs. We make sure they are not in different hyperblocks
//

BOOL HB_CFLOW_Can_Merge_BBs(BB *bb1, BB *bb2)
{
  HB *hb1,*hb2;
  if (!HB_formation) return TRUE;
  hb1 = (HB *) BB_MAP_Get(HB_bb_map,bb1);
  hb2 = (HB *) BB_MAP_Get(HB_bb_map,bb2);
  if ((hb1 == hb2) || (hb1 == NULL) || (hb2 == NULL)) return TRUE;
  return FALSE;
}
  
void HB_CFLOW_Remove_Block(BB *bb)
/////////////////////////////////////
//  Remove a block from its hyperblock
/////////////////////////////////////
{
  HB *hb;
  HB_bb_list::iterator bbi;
  if (!HB_formation) return;
  hb = (HB *) BB_MAP_Get(HB_bb_map,bb);
  if (!hb) return;
  // Replace the blocks in the bitsets
  hb->blocks = BB_SET_Difference1D(HB_Blocks(hb), bb);
  // replace the block in the list
  for (bbi = hb->block_list.begin(); bbi != hb->block_list.end(); ++bbi) {
    if (*bbi == bb) {
      hb->block_list.erase(bbi);
      break;
    }
  }
  // Fix up the entry block
  if (HB_Entry(hb) == bb) {
    HB_Entry_Set(hb,*hb->block_list.begin());
  }
  
  if (BB_SET_EmptyP(HB_Blocks(hb))) {
    Delete_HB_From_List(hb);
  }

  // redo the map.
  BB_MAP_Set(HB_bb_map,bb,NULL);
}
