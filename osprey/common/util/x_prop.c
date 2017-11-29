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



static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/x_prop.c,v $ $Revision: 1.1 $";

#include <bstring.h>


/* ====================================================================
 *
 *  X_PROP_Create
 *
 *  See interface description
 *
 * ====================================================================
 */

_X_PROP_TYPE_ *
_X_PROP_CREATE_(
  INT32     universe_size,
  MEM_POOL *pool
)
{
  /* We allocate fixed size bit vector, preceeded by its size.
   * Round up size to words and add 1 for the size.
   */
  UINT32 words = (  (universe_size + _X_PROP_TYPE_SIZE_ - 1)
		  >> _X_PROP_TYPE_SIZE_LOG2_) + 1;
  _X_PROP_TYPE_ *prop = TYPE_MEM_POOL_ALLOC_N(_X_PROP_TYPE_, pool, words);
  if ( ! MEM_POOL_Zeroed(pool) ) BZERO(prop, words * sizeof(_X_PROP_TYPE_));
  prop[0] = universe_size;
  return prop;
}


/* ====================================================================
 *
 *  X_PROP_Set
 *
 *  See interface description
 *
 * ====================================================================
 */

void
_X_PROP_SET_(
  _X_PROP_TYPE_            *prop,
  _X_PROP_LOCAL_BASE_TYPE_  x
)
{
  UINT32 id = _X_id_(x);

  Is_True(id < prop[0], ("property id out of range"));

  (prop+1)[id >> _X_PROP_TYPE_SIZE_LOG2_] |=
	(_X_PROP_TYPE_)1 << (id & (_X_PROP_TYPE_SIZE_-1));
}


/* ====================================================================
 *
 *  X_PROP_Reset
 *
 *  See interface description
 *
 * ====================================================================
 */

void
_X_PROP_RESET_(
  _X_PROP_TYPE_            *prop,
  _X_PROP_LOCAL_BASE_TYPE_  x
)
{
  UINT32 id = _X_id_(x);

  Is_True(id < prop[0], ("property id out of range"));

  (prop+1)[id >> _X_PROP_TYPE_SIZE_LOG2_] &= 
	~((_X_PROP_TYPE_)1 << (id & (_X_PROP_TYPE_SIZE_-1)));
}


/* ====================================================================
 *
 *  X_PROP_Get
 *
 *  See interface description
 *
 * ====================================================================
 */

BOOL
_X_PROP_GET_(
  _X_PROP_TYPE_            *prop,
  _X_PROP_LOCAL_BASE_TYPE_  x
)
{
  UINT32 id = _X_id_(x);

  Is_True(id < prop[0], ("property id out of range"));

  return   ((prop+1)[id >> _X_PROP_TYPE_SIZE_LOG2_] 
	 & ((_X_PROP_TYPE_)1 << (id & (_X_PROP_TYPE_SIZE_-1)))) != 0;
}


/* ====================================================================
 *
 *  X_PROP_Uniond
 *
 *  See interface description
 *
 * ====================================================================
 */

void
_X_PROP_UNIOND_(
  _X_PROP_TYPE_ *prop0,
  _X_PROP_TYPE_ *prop1
)
{
  UINT32 i;
  UINT32 universe_size = prop0[0];
  UINT32 words =   (universe_size + _X_PROP_TYPE_SIZE_-1) 
		>> _X_PROP_TYPE_SIZE_LOG2_;

  Is_True(universe_size == prop1[0], ("vectors are different length"));

  for ( i = 1; i <= words; ++i ) prop0[i] |= prop1[i];
}


/* ====================================================================
 *
 *  X_PROP_Intersection_Is_NonEmpty
 *
 *  See interface description
 *
 * ====================================================================
 */

BOOL
_X_PROP_INTERSECTION_IS_NONEMPTY_(
  _X_PROP_TYPE_ *prop0,
  _X_PROP_TYPE_ *prop1
)
{
  UINT32 i;
  UINT32 universe_size = prop0[0];
  UINT32 words =   (universe_size + _X_PROP_TYPE_SIZE_-1) 
		>> _X_PROP_TYPE_SIZE_LOG2_;

  Is_True(universe_size == prop1[0], ("vectors are different length"));

  for ( i = 1; i <= words; ++i ) {
    if ( (prop0[i] & prop1[i]) != 0 ) return TRUE;
  }
  return FALSE;
}
