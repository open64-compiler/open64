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

//  Preference classes for GRA
/////////////////////////////////////
//  
//  Description:
//
//      Union find algorithm implementation.
//
//  
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_pref.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "defs.h"
#include "mempool.h"
#include "gra_bb.h"
#include "gra_pref.h"
#include "gra_lrange.h"

GRA_PREF_MGR gra_pref_mgr;

/////////////////////////////////////
//  Find the root of <pref>.  Uses Union-find style path-compression. so
//  future Finds on the same path will be direct.
GRA_PREF*
GRA_PREF::Find(void)
{
  if ( this == NULL ) return NULL;

  GRA_PREF* root;
  for ( root = this; root->parent != NULL; root = root->parent );

  GRA_PREF* pref;
  GRA_PREF* next_parent;
  for ( pref = this, next_parent = pref->parent;
        next_parent != NULL;
        pref = next_parent, next_parent = next_parent->parent ) {
    pref->parent = root;
  }

  return root;
}

/////////////////////////////////////
// Destructively equate (or unify) <pref0> and <pref1>.  After
// this operation all references to <pref0> or <pref1> refer to
// the union of the two sets.
GRA_PREF*
GRA_PREF_MGR::UnionD( GRA_PREF* pref0, GRA_PREF* pref1 )
{
  pref0 = pref0->Find();
  pref1 = pref1->Find();

  if ( pref0 == pref1 )
    return pref0;
  else if ( pref0->count > pref1->count ) {
    pref0->Make_Parent_Of(pref1);
    return pref0;
  }
  else {
    pref1->Make_Parent_Of(pref0);
    return pref1;
  }
}

/////////////////////////////////////
// Note that the given member <lrange> has been allocated a
// register.  WARNING: No More _UnionD operations allowed after
// the first _Allocate_LRANGE operation.  (This is unchecked.)
void
GRA_PREF::Allocate_LRANGE( LRANGE* lrange )
{
  LRANGE_LIST  sentinal;
  LRANGE_LIST* prev;
  GRA_PREF*    found_pref = Find();
  float pref_priotity = lrange->Pref_Priority();

  //  TODO: here is a linear search that I think we'll eventually have to
  //  replace with something better:

  sentinal.rest = found_pref->lranges;

  for ( prev = &sentinal; prev->rest != NULL; prev = prev->rest ) {
    LRANGE* first = prev->rest->first;

    if ( first->Pref_Priority() <= pref_priotity )
      break;
  }

  prev->rest = LRANGE_LIST_Push(lrange,prev->rest,GRA_pool);
  found_pref->lranges = sentinal.rest;
}

