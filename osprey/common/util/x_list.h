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


#ifdef __cplusplus
extern "C" {
#endif


/* =======================================================================
 * =======================================================================
 *
 *  Module: x_list.h
 *  $Revision: 1.1 $
 *  $Date: 2005/07/27 02:17:57 $
 *  $Author: kevinlo $
 *  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/x_list.h,v $
 *
 *  Revision comments:
 *
 *  -2-Mar-1992 - Initial version
 *
 *  Description:
 *  ============
 *
 *      Template for generating externally linked list modules.  I've tried
 *      to use Common Lisp like names for the various functions so I didn't
 *      have to invent any names.  The list of supported functions is very
 *      incomplete currently, but it can grow as required.  (It will have
 *      to grow a good bit to take on the BB lists.)
 *  
 *  Instructions for use:
 *
 *      This file is only a template.  See MTP_ROOT/bin/gen_x_list for
 *      instructions on how to create instantiations based on specific
 *      types.
 *  
 *  Prefixes:     X_LIST  -- for type specific lists of Xs
 *      
 *  Interface Description:
 *
 *      Exported types:
 *
 *          typedef struct x_list X_LIST
 *
 *              The "con" cell of an X_LIST.
 *
 *              A structure with the following fields:
 *
 *                  XT  first
 *
 *                      First XT in the list.
 *
 *                  X_LIST *rest
 *
 *                      Rest of the list.
 *
 *      Exported functions:
 *
 *          X_LIST *X_LIST_Push(
 *              XT        first,
 *              X_LIST   *rest,
 *              MEM_POOL *pool
 *              
 *          )
 *
 *              Return a list consisting of 'first' . 'rest'.  Memory
 *              allocated in the given 'pool'.
 *
 *	    X_LIST *X_LIST_Delete(
 *		XT	x,
 *		X_LIST	*list
 *	    )
 *
 *		Destructively remove element <x> from the list. The result
 *		of this function must be used, since the argument 'list'
 *		is no longer valid after this function is called.
 *
 *
 * =======================================================================
 * =======================================================================
 */

typedef struct _X_LIST_TAG_ _X_LIST_TYPE_;

typedef _X_BASE_TYPE_ _X_LIST_LOCAL_BASE_TYPE_;

struct _X_LIST_TAG_ {
  _X_LIST_LOCAL_BASE_TYPE_      first;
  _X_LIST_TYPE_                *rest;
};

extern _X_LIST_TYPE_ *
_X_PUSH_(
  _X_LIST_LOCAL_BASE_TYPE_  x,
  _X_LIST_TYPE_            *xl,
  MEM_POOL                 *pool
);

extern _X_LIST_TYPE_ *
_X_DELETE_(
  _X_LIST_LOCAL_BASE_TYPE_ x,
  _X_LIST_TYPE_	          *xl
);

#ifdef __cplusplus
}
#endif
