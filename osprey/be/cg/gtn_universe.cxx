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
 *  Module: gtn_universe.c
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:25-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.gtn_universe.cxx $
 *
 *  Revision comments:
 *
 *  13-Oct-1994 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Implementation of the GTN Universe
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define GTN_IMPLEMENTATION

#include "defs.h"
#include "mempool.h"
#include "tracing.h"
#include "erglob.h"
#include "tn.h"
#include "gtn_universe.h"

INT32  GTN_UNIVERSE_size;
INT32  First_REGION_GTN;
INT32 *GTN_UNIVERSE_tn_int_map;
TN   **GTN_UNIVERSE_int_tn_map;

static MEM_POOL tn_int_map_pool;
static MEM_POOL int_tn_map_pool;
static BOOL     pools_initialized;
INT32    tn_int_map_allocated_size;
INT32    int_tn_map_allocated_size;


/* =======================================================================
 *
 *  GTN_UNIVERSE_Pu_Begin
 *
 *  See interface description.
 *
 * =======================================================================
 */
extern void
GTN_UNIVERSE_Pu_Begin(void)
{
  if ( ! pools_initialized ) {
    MEM_POOL_Initialize(&tn_int_map_pool,
                        "GTN_tn->int_map",TRUE);
    MEM_POOL_Initialize(&int_tn_map_pool,
                        "GTN_int->tn_map",TRUE);
    pools_initialized = TRUE;
  }

  MEM_POOL_Push(&tn_int_map_pool);
  MEM_POOL_Push(&int_tn_map_pool);
  /* Reserve 0 for non universe-members:
   */
  GTN_UNIVERSE_size = 1;
  First_REGION_GTN = GTN_UNIVERSE_size;
}

/* =======================================================================
 *
 *  GTN_UNIVERSE_REGION_Begin
 *
 *  See interface description.
 *
 * =======================================================================
 */
extern void
GTN_UNIVERSE_REGION_Begin(void)
{
  if ( ! pools_initialized ) {
    MEM_POOL_Initialize(&tn_int_map_pool,
                        "GTN_tn->int_map",TRUE);
    MEM_POOL_Initialize(&int_tn_map_pool,
                        "GTN_int->tn_map",TRUE);
    pools_initialized = TRUE;
  }

  First_REGION_GTN = GTN_UNIVERSE_size;
}


/* =======================================================================
 *
 *  GTN_UNIVERSE_Pu_End
 *
 *  See interface description.
 *
 * =======================================================================
 */
extern void
GTN_UNIVERSE_Pu_End(void)
{
  MEM_POOL_Pop(&tn_int_map_pool);
  MEM_POOL_Pop(&int_tn_map_pool);
  tn_int_map_allocated_size = 0;
  int_tn_map_allocated_size = 1;
  GTN_UNIVERSE_size = -1;
  GTN_UNIVERSE_int_tn_map = NULL;
  GTN_UNIVERSE_tn_int_map = NULL;
}


/* =======================================================================
 *
 *  GTN_Universe_Add_TN
 *
 *  See interface description.
 *
 * =======================================================================
 */
extern void
GTN_UNIVERSE_Add_TN(
  TN *tn
)
{
  INT32 tn_num = TN_number(tn);

  if (tn_num < tn_int_map_allocated_size && TN_is_global_reg(tn) &&
      GTN_UNIVERSE_tn_int_map[tn_num] <= GTN_UNIVERSE_size &&
      GTN_UNIVERSE_int_tn_map[GTN_UNIVERSE_tn_int_map[tn_num]] == tn)
      {
    /* Already added.
     */
    return;
  }

  /* Horrible boring checks to see if everything is allocated and
   * allocated large enough.
   */

  if ( GTN_UNIVERSE_size == 1 ) {
    tn_int_map_allocated_size = Last_TN + 1;
    int_tn_map_allocated_size = Last_TN + 1;

    GTN_UNIVERSE_tn_int_map = TYPE_MEM_POOL_ALLOC_N(INT32,
                                                    &tn_int_map_pool,
                                                    Last_TN + 1);
    GTN_UNIVERSE_int_tn_map = TYPE_MEM_POOL_ALLOC_N(TN *,
                                                    &int_tn_map_pool,
                                                    Last_TN + 1);
  }
  else {
    if ( GTN_UNIVERSE_size == int_tn_map_allocated_size ) {
      INT32 new_size = int_tn_map_allocated_size * 2;

      GTN_UNIVERSE_int_tn_map =
        TYPE_MEM_POOL_REALLOC_N(TN *,&int_tn_map_pool,
                                     GTN_UNIVERSE_int_tn_map,
                                     int_tn_map_allocated_size,
                                     new_size);

      int_tn_map_allocated_size = new_size;
    }

    if ( TN_number(tn) >= tn_int_map_allocated_size ) {
      INT32 new_size = MAX(tn_int_map_allocated_size * 2, Last_TN + 1);

      GTN_UNIVERSE_tn_int_map =
        TYPE_MEM_POOL_REALLOC_N(INT32,&tn_int_map_pool,
                                      GTN_UNIVERSE_tn_int_map,
                                      tn_int_map_allocated_size,
                                      new_size);
      tn_int_map_allocated_size = new_size;
    }
  }

  GTN_UNIVERSE_tn_int_map[tn_num] = GTN_UNIVERSE_size;
  Set_TN_is_global_reg (tn);
  GTN_UNIVERSE_int_tn_map[GTN_UNIVERSE_size++] = tn;
}
