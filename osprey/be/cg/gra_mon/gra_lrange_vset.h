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

// LRANGE sets represented as vectors
/////////////////////////////////////
//
//  Description:
//
//      When an LRANGE_SET would be too sparse, this might be just the thing.
//      This is a very specialized and simple data structure and is probably
//      only useful for GRA_INTERFERE.  In praticular, it provides no
//      protection against adding the same element twice -- this is handled by
//      GRA_INTEFERE.

//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange_vset.h $


#ifndef GRA_LRANGE_VSET_INCLUDED
#define GRA_LRANGE_VSET_INCLUDED

#ifndef GRA_LRANGE_VSET_RCS_ID
#define GRA_LRANGE_VSET_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_lrange_vset_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange_vset.h $ $Revision: 1.2 $";
#endif
#endif

#include "defs.h"
#include "gra_lrange.h"


// A set of LRANGEs impleneted as a dynamically created vector of pointers
// to LRANGEs.  
class LRANGE_VSET {
friend class LRANGE_VSET_ITER;
friend LRANGE_VSET *LRANGE_VSET_Create( LRANGE** vec, size_t count, MEM_POOL *pool );
  size_t  count;    // Number of elements
  LRANGE* vec[1];   // Here they are
public:
  LRANGE_VSET(void) {}
  ~LRANGE_VSET(void) {}

  // access functions
  INT Count(void)		{ return count; }

  // non-inlined member functions
  void Delete_Element( LRANGE* lrange );
  void Replace_Element( LRANGE* old_lr, LRANGE* new_lr);
};

extern LRANGE_VSET* LRANGE_VSET_Create( LRANGE** vec, size_t count, MEM_POOL *pool );

// An iterator type used to loop over the LRANGEs in a LRANGE_VSET.
class LRANGE_VSET_ITER {
  LRANGE** vec;         // The vector of LRANGEs
  INT32    current;     // Current index.  Counted down to -1
public:
  void Init(LRANGE_VSET *vset)	{ vec = vset->vec; current = vset->count-1; }
  BOOL Done(void)		{ return current < 0; }
  LRANGE *Current(void)		{ return vec[current]; }
  void Step(void)		{ --current; }
};


#endif
