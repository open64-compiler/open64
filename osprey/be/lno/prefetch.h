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


/*      Main prefetching pass
 *
 * This file contains the driver routine for the prefetching phase, 
 * along with some support routines.
 *
 * Exported Functions:
 *
 *  void Prefetch (WN* func_nd)
 *
 *      Given the WN* for a PU, 
 *          - perform prefetching analysis
 *          - update func_nd to contain prefetch information
 */

#ifndef prefetch_INCLUDED
#define prefetch_INCLUDED

#include <sys/types.h>
#include "wn.h"
#include "dep_graph.h"

extern BOOL Debug_Prefetch;
extern BOOL Verbose_Prefetch;

extern MEM_POOL *PF_mpool;
extern void Prefetch_Driver (WN* func_nd,
                             ARRAY_DIRECTED_GRAPH16 *array_dep_graph);
extern void Init_Prefetch_Options (WN* func_nd);

void Initialize_Lvs ();
void Cleanup_Lvs ();

#endif // prefetch_INCLUDED
