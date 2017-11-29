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
// Module: rbi.cxx
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:58-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.rbi.cxx $
//
// Revision history:
//  31-AUG-95 dahl - Original Version
//
// Description:
//	Region Boundary Info (RBI). Using DU-chains and alias
//      information, calculate the boundary information for regions in
//      a PU.
//
// This code is linked in and called by lno.so
//
// TODO items:
// *) Write query routines to provide region boundary information from
//    the used_in, def_in_live_out, and def_in_dead_out sets to
//    consumers. This should be fairly simple, using
//    POINTS_TO::Overlap(POINTS_TO *) from
//    be/wopt[_dev]/opt_alias_analysis.cxx. Some wrapper code will be
//    required to iterate through the appropriate set of POINTS_TOs,
//    and do something reasonable when the base-ST of the given
//    POINTS_TO differs from that of one of the set elements.
//
// NOTE:
//  Any POINTS_TO nodes created by RBI must be allocated from the 
//  REGION_mem_pool so that they do not disappear when the alias
//  manager is freed.
//
//============================================================================

#define rbi_CXX	"rbi.cxx"
#ifdef _KEEP_RCS_ID
static char *rcs_id = rbi_CXX"$Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "opt_wn.h"
#include "opt_base.h"
#include "wn_util.h"
#include "stab.h"
#include "cxx_base.h"
#include "cxx_memory.h"
#include "region_util.h"	/* for region trace flags, REGION_mem_pool */
#include "ir_reader.h"		/* for fdump_tree			*/
#include "tracing.h"		/* TFile				*/
#include "opt_du.h"		/* for DU_MANAGER class declaration	*/
#include "opt_points_to.h"      /* for POINTS_TO class declaration	*/
#include "opt_alias_mgr.h"      /* for ALIAS_MANAGER class declaration	*/
#include "rbi.h"		/* class RBI				*/

// ALIAS_MANAGER::Id(), USE_LIST::Print(), DEF_LIST::Print()
// defined in wopt.so and exported
// these pragmas only work for Mongoose (bootstrap), Ucode has no clue.
#ifdef AFTER_MR
#pragma weak Print__8USE_LISTGP8__file_s
#pragma weak Print__8DEF_LISTGP8__file_s
#pragma weak Id__13ALIAS_MANAGERCGPC2WN
#endif

// -----------------------------------------------------------------
// RBI interface to the outside world.
// -----------------------------------------------------------------

extern "C"	/* so lnodriver.c can call this entry point	*/
void
Region_Bound_Info(WN *tree, DU_MANAGER *du_mgr, ALIAS_MANAGER *am)
{
}

