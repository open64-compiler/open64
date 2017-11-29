/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
#include "wn.h"
#include "opcode.h"


/*------------------------------------------------------------------------*/
/* parent pointers for all nodes makes it possible to replace one node    */
/* with another tree node, with ease                                      */
/*------------------------------------------------------------------------*/
extern void WN_Set_Parent (WN* wn, const WN* parent_wn, WN_MAP Parent_Map,
			   WN_MAP_TAB *map_tab) 
{
  IPA_WN_MAP_Set(map_tab, Parent_Map, wn,(void *) parent_wn);
}

/*------------------------------------------------------------------------*/
/* get the parent of wn                                                   */
/*------------------------------------------------------------------------*/
extern WN* WN_Get_Parent (const WN* wn, WN_MAP Parent_Map,
			  WN_MAP_TAB *map_tab)
{
  WN* parent_wn = (WN*) IPA_WN_MAP_Get(map_tab, Parent_Map,(WN*)wn);
  return parent_wn;
}

/*------------------------------------------------------------------------*/
/* set the parents in the side array                                      */
/*------------------------------------------------------------------------*/
extern void WN_Parentize (WN* wn, WN_MAP Parent_Map, WN_MAP_TAB *map_tab) 
{
  if (!OPCODE_is_leaf (WN_opcode (wn))) { 
    if (WN_operator(wn) == OPR_BLOCK) {
      WN* kid = WN_first (wn);
      while (kid) {
        WN_Set_Parent (kid, wn, Parent_Map, map_tab);
        WN_Parentize (kid, Parent_Map, map_tab);
        kid = WN_next (kid);
      }
    }
    else {
      INT kidno;
      WN* kid;
      for (kidno=0; kidno<WN_kid_count(wn); kidno++) {
        kid = WN_kid (wn, kidno);
        if (kid) { 
          WN_Set_Parent (kid, wn, Parent_Map, map_tab);
          WN_Parentize (kid, Parent_Map, map_tab);
        }
      }
    }
  }
}


/*------------------------------------------------------------------------*/
/* Given a tree, check that its parent pointers are consistent.           */
/* Return TRUE if ok, FALSE otherwise.                                    */
/*------------------------------------------------------------------------*/
BOOL WN_Check_Parentize (const WN* wn, WN_MAP Parent_Map, WN_MAP_TAB *map_tab)
{
  BOOL isok = TRUE;
  if (!OPCODE_is_leaf (WN_opcode (wn))) { 
    if (WN_operator(wn) == OPR_BLOCK) {
      WN* kid = WN_first (wn);
      while (kid) {
        Is_True ((WN_Get_Parent (kid, Parent_Map, map_tab) == wn),
                 ("CheckParentize Error: kid 0x%x, parent 0x%x\n", kid, wn));
        isok &= (WN_Get_Parent (kid, Parent_Map,map_tab) == wn);
        if (!isok) break;
        isok &= WN_Check_Parentize (kid, Parent_Map, map_tab);
        if (!isok) break;
        kid = WN_next (kid);
      }
    }
    else {
      INT kidno;
      WN* kid;
      for (kidno=0; kidno<WN_kid_count(wn); kidno++) {
        kid = WN_kid (wn, kidno);
        if (kid) { 
          Is_True ((WN_Get_Parent (kid, Parent_Map, map_tab) == wn),
                   ("CheckParentize Error: kid 0x%x, parent 0x%x\n", kid, wn));
          isok &= (WN_Get_Parent (kid, Parent_Map, map_tab) == wn);
          if (!isok) break;
          isok &= WN_Check_Parentize (kid, Parent_Map, map_tab);
          if (!isok) break;
        }
      }
    }
  }
  return isok;
}








