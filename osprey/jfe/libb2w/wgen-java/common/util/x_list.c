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
 *  Module: x_list.c
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/x_list.c,v $
 *
 *  Revision comments:
 *
 *  13-Dec-1991 - Initial version
 *  27-May-1993 - Changed to MEM_POOL style
 *
 *  Description:
 *  ============
 *
 *  Generalised lisp implementation (Common lisp style.)
 *
 * =======================================================================
 * =======================================================================
 */

static char *source_file = __FILE__;
static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/util/x_list.c,v $ $Revision: 1.1.1.1 $";

#define _X_first_(x)    ((x)->first)
#define _X_rest_(x)     ((x)->rest)

/* =======================================================================
 *
 *  _X_PUSH_
 *
 *  See interface description.
 *
 * =======================================================================
 */
_X_LIST_TYPE_ *
_X_PUSH_(
  _X_LIST_LOCAL_BASE_TYPE_  x,
  _X_LIST_TYPE_            *list,
  MEM_POOL                 *pool
)
{
  _X_LIST_TYPE_ *result = TYPE_MEM_POOL_ALLOC(_X_LIST_TYPE_,pool);

  _X_first_(result) = x;
  _X_rest_(result) = list;
  return result;
}


/* =======================================================================
 *
 *  _X_DELETE_
 *
 *  See interface description.
 *
 * =======================================================================
 */
_X_LIST_TYPE_ *
_X_DELETE_(
    _X_LIST_LOCAL_BASE_TYPE_ x,
    _X_LIST_TYPE_           *xl
)
{
  /* Oh, for a little tail recursion...
   */

  _X_LIST_TYPE_  sentinal;
  _X_LIST_TYPE_ *l;
  _X_LIST_TYPE_ *last = &sentinal;

  _X_rest_(last) = NULL;
  for ( l = xl; l; l = _X_rest_(l) ) {
    if (x != _X_first_(l)) {
      _X_rest_(last) = l;
      last = l;
    }
  }
  _X_rest_(last) = NULL;
  return _X_rest_(&sentinal);
}
