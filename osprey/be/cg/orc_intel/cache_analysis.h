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
 *  Module: cg_dep_graph.h
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/cache_analysis.h,v $
 *
 *  Revision comments:
 *
 *  23-Apr-2003 - Initial version
 *
 *  Description:
 *  ============
 *
 *  This module implements the analysis and optimization related with 
 *  cache. 
 */

#ifndef CACHE_ANALYSIS_INCLUDED
#define CACHE_ANALYSIS_INCLUDED

#include "op.h"

// Cache function.
extern BOOL Cache_Has_Conflict(OP *pred, OP *op, INT *distance, BOOL *equal);
extern BOOL Cache_Has_Conflict(OP *pred, OP *op, CG_DEP_KIND kind);

extern void Cache_Location_Analysis(void);
extern void Cache_Analysis_End(void);

extern BOOL Cache_Access_Same_Line(const struct ALIAS_MANAGER *, WN *, WN *, INT *diff);
extern BOOL Cache_Access_Same_Line(OP *op1, OP *op2, INT *diff);
extern void Cache_Adjust_Latency(OP *pred, OP *op, CG_DEP_KIND kind, INT *latency);

extern BOOL Cache_L2_Has_Data(OP *op);
extern BOOL Cache_L1_Has_Data(OP *op);
#endif /* CACHE_ANALYSIS_INCLUDED */
