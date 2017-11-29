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


/***********************************************************************
 *
 * Top-level routines/variables for Lego processing.
 *
 * extern void Lego_Read_Pragmas (WN* func_nd);
 *      Read the data distribution pragmas and build up internal 
 *      representations.
 *
 * extern void Lego_Lower_Pragmas (WN* func_nd);
 *      Generate code for the data distribution pragmas, and 
 *      lower array accesses to reshaped arrays.
 *
 * extern void Lego_PU_Init ();
 *      Do PU-level initialization of lego-processing data structures.
 *      Called at the beginning of LNO for each PU.
 *
 * extern void Lego_PU_Fini ();
 *      Do PU-level finalization of lego-processing data structures.
 *      Called at the end of LNO for each PU.
 *
 * extern void Lego_Compute_Tile_Peel (WN* func_nd);
 *      Compute the tiling and peeling factors to optimize references
 *      to reshaped arrays, based on a combination of affinity hints
 *      (if any) and analysis of references to reshaped arrays within the loop.
 *
 * extern MEM_POOL LEGO_memory_pool, *LEGO_pool;
 *      Memory pool for lego processing.
 *
 *
 ***********************************************************************/

#ifndef _LEGO_INCLUDED_
#define _LEGO_INCLUDED_

#include "wn.h"
#include "cxx_memory.h"

extern void Lego_OZero_Driver(PU_Info* current_pu, WN* func_nd);
extern void Lego_Read_Pragmas(WN* func_nd);
extern void Lego_Lower_Pragmas(WN* func_nd);
extern void Lego_PU_Init();
extern void Lego_PU_Fini();
extern void Lego_Compute_Tile_Peel(WN* func_nd);

extern MEM_POOL LEGO_memory_pool, *LEGO_pool;

#endif /* _LEGO_INCLUDED_ */
