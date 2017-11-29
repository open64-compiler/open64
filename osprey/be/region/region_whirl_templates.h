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
// Module: region_templates.h
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:58-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.region_whirl_templates.h $
//
// Revision history:
//  1-SEP-97 dahl - Original Version
//
//============================================================================

// =======================================================================
// WHIRL search comparison functions for REGION_search_block
// can search two ways:
// 1) same pragma number in a pragma list
// 2) same label_number in a region exit list
struct comp_same_pragma {
 private:
  const WN_PRAGMA_ID _pragma;
 public:
  comp_same_pragma(const WN_PRAGMA_ID pragma) : _pragma(pragma) { }
  BOOL operator()(const WN *x) const {
    Is_True(WN_opcode(x) == OPC_PRAGMA,
	    ("comp_same_pragma, not a pragma"));
    if (WN_pragma(x) == _pragma)
      return TRUE;
    return FALSE;
  }
};

struct comp_same_label_no {
 private:
  const INT32 _label_number;
 public:
  comp_same_label_no(const INT32 label_no) : _label_number(label_no) { }
  BOOL operator()(const WN *x) const {
    // in CG these can be converted to gotos or labels, rest of the time they
    // are always region_exits
    Is_True(WN_opcode(x) == OPC_REGION_EXIT || WN_opcode(x) == OPC_LABEL ||
	    WN_opcode(x) == OPC_GOTO, 
	    ("comp_same_label_no, not a region exit or label"));
    return (WN_label_number(x) == _label_number);
  }
};

// =======================================================================
// REGION_search_block: search statements in a WHIRL block
// returns address of WHIRL node if found
template <class COMP>
WN *REGION_search_block(WN *block, COMP comp)
{
  Is_True(WN_opcode(block) == OPC_BLOCK, ("REGION_search_block, not a block"));
  for (WN *wtmp=WN_first(block); wtmp; wtmp=WN_next(wtmp))
    if (comp(wtmp))
      return wtmp;
  return NULL;
}
