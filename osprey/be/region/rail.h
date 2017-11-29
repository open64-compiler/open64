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
//=====================================================================
//
// Module: rail.h
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:58-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.rail.h $
//
// Revision history:
//  24-AUG-95 dahl - Original Version
//
// Description:
//	Regions Around Inner Loops (RAIL)
//	Finds inner loops and places regions around them.
//
//======================================================================
//======================================================================

#ifndef rail_INCLUDED
#define rail_INCLUDED "rail.h"
#ifdef _KEEP_RCS_ID
static char *railrcs_id = rail_INCLUDED"$ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

class RAIL {

private:
  BOOL _trace_flag;			// -ttTP_REGION(41)
  WN *_func_entry;			// WN func_entry

public:
  RAIL(void);				// default constructor sets trace flag
  ~RAIL(void)				{ }

  BOOL Trace(void)			{ return _trace_flag;	}
  void Set_fe(WN *wn)			{ _func_entry = wn;	}
  WN *Get_fe(void)			{ return _func_entry;	}
  void Next_fe(void)			{ _func_entry = WN_next(_func_entry); }

  void Process_func_entry(WN *);	// go through all functions
  BOOL Process_block(WN *, INT32);	// TRUE if found loop, FALSE otherwise
  WN *Add_region_around_loop(WN *, INT32); // add region node

};	// end of class RAIL

#endif /* ifdef rail_INCLUDED */
