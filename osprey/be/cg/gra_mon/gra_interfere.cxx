/*
 * Copyright (C) 2012 Advanced Micro Devices, Inc.  All Rights Reserved.
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

//  GRA interferece representation implementation
/////////////////////////////////////
//
//  Description:
//
//      Details of creation of complement style interference sets.
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:29-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_interfere.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_interfere.cxx $ $Revision: 1.2 $";
#endif

#include "defs.h"
#include "mempool.h"
#include "gra_bb.h"
#include "gra_lunit.h"
#include "gra_lrange.h"
#include "gra_interfere.h"
#include "gra_lrange_subuniverse.h"
#include "gra_lrange_vset.h"
#include "lrange_set.h"
#include "gra_region.h"

INTERFERE_MGR intf_mgr;

/////////////////////////////////////
// Add <lrange> to <interference> which must have been created by
// intf_mgr.Create_Empty with the given <subuniverse>.  Returns
// the union of <interfere> and <lrange>.  This is destructive
// and the value of <intefere> is no longer valid after this
// call, so be careful.
INTERFERE
INTERFERE_DEREF::Add_Neighbor(LRANGE* lrange, LRANGE_SUBUNIVERSE* subuniverse)
{
  LRANGE_SET* set = Bitset();
  return intf_mgr.Make_Bitset(LRANGE_SET_Union1DS(set,lrange,GRA_pool,
                                                         subuniverse));
}

/////////////////////////////////////
// Remove <lrange> from <interference>.  As with _Add_Neighbor,
// returns the new value and invalidates <intefere>
INTERFERE
INTERFERE_DEREF::Remove_Neighbor(LRANGE* lrange, 
				 LRANGE_SUBUNIVERSE* subuniverse)
{
  if ( Is_Vec() ) {
    Vec()->Delete_Element(lrange);
    return this;
  }
  else {
    LRANGE_SET* set =
      LRANGE_SET_Difference1DS(Bitset(),lrange, subuniverse);
    return intf_mgr.Make_Bitset(set);
  }
}

/////////////////////////////////////
// Replace <old> with <new> in <intefere> which is relative to <subuniverse>.
INTERFERE
INTERFERE_DEREF::Replace_Neighbor(LRANGE* old_lr, LRANGE* new_lr,
  			          LRANGE_SUBUNIVERSE* subuniverse)
{
  if ( Is_Vec() ) {
    Vec()->Replace_Element(old_lr,new_lr);
    return this;
  }
  else {
    LRANGE_SET* set = Bitset();

    set = LRANGE_SET_Difference1DS(set,old_lr,subuniverse);
    set = LRANGE_SET_Union1DS(set,new_lr,GRA_pool,subuniverse);
    return intf_mgr.Make_Bitset(set);
  }
}

/////////////////////////////////////
// Create and return a new empty INTEFERE node suitable for fine
// grained interference representation.  All the interfering
// nodes must be in the given <subuniverse>
INTERFERE
INTERFERE_MGR::Create_Empty(LRANGE_SUBUNIVERSE* subuniverse)
{
  size_t size = subuniverse->Count();
  return intf_mgr.Make_Bitset(LRANGE_SET_Create_Empty(size,GRA_pool));
}

/////////////////////////////////////
// Start creation of a new INTERFERE.  Taken together with Create_Add_Neighbor
// and Create_End, this is the way to create INTERFERE nodes
// when it is possible to present all the neighbors of a node at
// the time of its creation.
void
INTERFERE_MGR::Create_Begin( LRANGE_SUBUNIVERSE* su )
{
  MEM_POOL_Push(&neighbor_vec_pool);

  neighbor_vec_size = su->Count();
  neighbor_vec = TYPE_MEM_POOL_ALLOC_N(LRANGE*,&neighbor_vec_pool,
                                               neighbor_vec_size);
  neighbor_subuniverse = su;
  neighbor_count = 0;
  neighbor_id_max = -1;
  lrange_mgr.Clear_One_Set();
}

/////////////////////////////////////
// Present an interference graph neighbor of the the node under construction.
void
INTERFERE_MGR::Create_Add_Neighbor( LRANGE* neighbor )
{
  if ( ! lrange_mgr.One_Set_MemberP(neighbor) ) {
    DevAssert(neighbor_count + 1 <= neighbor_vec_size,
              ("GRA interference creation vector overflow"));
    lrange_mgr.One_Set_Union1(neighbor);
    neighbor_vec[neighbor_count++] = neighbor;
    if ( neighbor->Id() > neighbor_id_max )
      neighbor_id_max = neighbor->Id();
  }
}

/////////////////////////////////////
// Create and return a new empty INTEFERE node suitable for fine
// grained interference representation.  All the interfering
// nodes must be in the given <subuniverse>
INTERFERE
INTERFERE_MGR::Create_End( void )
{
  INTERFERE result;
#ifndef TARG_X8664  
  size_t v_size = neighbor_count * sizeof(LRANGE*);
#else
  /* This change was made to ensure that register allocation is
   * the same independent of the compiler being built -m32 or -m64.
   * Note that the change may waste memory when the compiler is built
   * -m64, but given the larger memory space this is less of an issue.
   * TODO:
   *    Determine whether LRANGE_NEIGHBOR_ITER can be made to generate
   *    the same edge list independent of the storage medium (vector
   *    or bitset).
   *
   *    For debugging purposes, add a CG option that will force the
   *    interference information to be stored as a bitset or vector.
   */
   
  size_t v_size = neighbor_count * 4;
#endif
  size_t s_size = LRANGE_SET_Size_Alloc_Size(neighbor_id_max + 1);

  //  Decide whether to represent it with a bitset or with a vector based on
  //  which representation is smaller.  Since we never have to grow either
  //  representation (due to the fact that we only delete, never add,
  //  interference) we can use just the exact right size for either.

  if ( v_size < s_size ) {
    result = intf_mgr.Make_Vec(LRANGE_VSET_Create(neighbor_vec,
                                                   neighbor_count,
                                                   GRA_pool));
  }
  else {
    INT32 i;
    LRANGE_SET* ns = LRANGE_SET_Create_Empty(neighbor_id_max,GRA_pool);

    for ( i = neighbor_count - 1; i >= 0; --i ) {
      ns = LRANGE_SET_Union1DS(ns,neighbor_vec[i],GRA_pool,
                                                  neighbor_subuniverse);
    }

    result = intf_mgr.Make_Bitset(ns);
  }

  MEM_POOL_Pop(&neighbor_vec_pool);

  return result;
}
