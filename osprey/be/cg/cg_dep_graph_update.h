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


/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_dep_graph_update.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:21-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_dep_graph_update.h $
 *
 *  Revision comments:
 *
 *  3-Aug-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Some semi-private routines called only from the low-level BB and OP
 *  manipulation routines to keep an active dependence graph up-to-date.
 *  These functions are implemented in "cg_dep_graph.cxx".
 *
 *  See "cg_dep_graph.h" for the main CG dep graph interface.
 *
 *
 *  Keeping the dependence graph up to date requires calling the following
 *  "update" functions at the appropriate time by the client.
 *
 *  IMPORTANT: When adding/changing/deleting multiple OPs, these update
 *  routines must be called as each *individual* OP is added/changed/deleted.
 *  They may not work correctly (one symptom is "OP has no CG_DEP info" error)
 *  if a bunch of ops are added/changed/deleted before any of the update
 *  routines are called.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef CG_DEP_GRAPH_UPDATE_INCLUDED
#define CG_DEP_GRAPH_UPDATE_INCLUDED

typedef struct bb BB;

extern BB * _cg_dep_bb;

inline BOOL CG_DEP_Has_Graph(BB *bb)
{
  return _cg_dep_bb == bb;
}

#endif /* CG_DEP_GRAPH_UPDATE_INCLUDED */
