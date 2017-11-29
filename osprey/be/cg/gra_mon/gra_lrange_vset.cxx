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

// LRANGE_VSET implementation
/////////////////////////////////////

//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange_vset.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange_vset.cxx $ $Revision: 1.2 $";
#endif

#include "defs.h"
#ifndef __GNUC__
#include "bstring.h"
#else
#include "string.h"
#endif
#include "gra_bb.h"
#include "gra_lrange.h"
#include "gra_lrange_vset.h"

/////////////////////////////////////
// Create and return a new LRANGE_VSET (in the GRA_pool).  Its
// elements are given in <vec>, a vector containing <count>
// distinct LRANGE pointers.
LRANGE_VSET*
LRANGE_VSET_Create( LRANGE** vec, size_t count, MEM_POOL *pool )
{
  LRANGE_VSET* result =
    (LRANGE_VSET*) MEM_POOL_Alloc(pool,  sizeof(LRANGE_VSET)
                                       + (count - 1) * sizeof(LRANGE*));
  //  To use bcopy or not?  I think that on banyon (due to improved shared
  //  library version), it will probably do better with long vectors than
  //  anything we can write in simple C without really understanding which
  //  compiler will be used.  *Sigh*
  bcopy((void*) vec,
        (void*) (result->vec),
        count * sizeof(LRANGE*));
  result->count = count;
  return result;
}

/////////////////////////////////////
// Deletes <lrange> from <vec>.  Raises an error if it doesn't
// find <lrange> in <vec>.
void
LRANGE_VSET::Delete_Element( LRANGE* lrange )
{
  size_t   i;

  for ( i = 0; i < count; ++i ) {
    if ( vec[i] == lrange ) {
      if ( i < count - 1 )
        vec[i] = vec[count - 1];
      --count;
      return;
    }
  }

  DevWarn("LRANGE_VSET::Delete_Element -- LRANGE not found");
}

/////////////////////////////////////
// <new> takes the place of <old>
void
LRANGE_VSET::Replace_Element( LRANGE* old_lr, LRANGE* new_lr )
{
  size_t   i;

  for ( i = 0; i < count; ++i ) {
    if ( vec[i] == old_lr ) {
      vec[i] = new_lr;
      return;
    }
  }

  DevWarn("LRANGE_VSET_Replace_Element -- old LRANGE not found");
}
