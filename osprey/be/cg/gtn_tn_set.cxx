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
 *  Module: gtn_tn_set.c
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/gtn_tn_set.cxx,v $
 *
 *  Revision comments:
 *
 *  14-Oct-1994 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Implements the translation from GTN sets to TN sets.
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "defs.h"
#include "errors.h"		    /* for definition of Is_True() */
#include "mempool.h"
#include "bitset.h"
#include "cgir.h"
#include "tn_set.h"
#include "tn_list.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "bb_list.h"
#include "register.h"
#include "bbregs.h"
#include "gtn_tn_set.h"


MEM_POOL local_pool;
BOOL     local_pool_initialized = FALSE;


/* =======================================================================
 *
 *  Check_Local_Pool_Initialized
 *
 *  Check that the local pool is initialized and do so if required.
 *
 * =======================================================================
 */
inline void
Check_Local_Pool_Initialized(void)
{
  if ( ! local_pool_initialized ) {
    MEM_POOL_Initialize(&local_pool,"GTN_TN_SET_local_pool",FALSE);
    local_pool_initialized = TRUE;
  }
}


/* =======================================================================
 *
 *  GTN_TN_SET_Print
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GTN_TN_SET_Print(GTN_SET *gtn_set, FILE *file)
{
  Check_Local_Pool_Initialized();
  MEM_POOL_Push(&local_pool);

  TN_SET *tn_set = TN_SET_Create_Empty (Last_TN + 1,&local_pool);
  for ( TN *tn = GTN_SET_Choose(gtn_set);
	tn != GTN_SET_CHOOSE_FAILURE && tn != NULL;
	tn = GTN_SET_Choose_Next(gtn_set,tn)) 
  {
    FmtAssert(TN_is_global_reg(tn),("TN%d is not global",TN_number(tn)));
    tn_set = TN_SET_Union1D(tn_set, tn, &local_pool);
  }

  TN_SET_Print(tn_set,file);

  MEM_POOL_Pop(&local_pool);
}
