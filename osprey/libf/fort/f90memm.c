/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



#pragma ident "@(#) libf/fort/f90memm.c	92.1	06/24/99 10:18:36"
#include <fortran.h>
#include <stddef.h>
#include <stdlib.h>
#include <cray/portdefs.h>

/* Routines called by the SPARC compiler only to allocate and
 * deallocate temporary memory.
 *
 *   void * _F90ALLOC(_f_int size, _f_int flag)
 *      
 *  where size  = default integer containing the requested size in bytes
 *        aflag = default integer containing:
 *                	nonzero - abort
 *                	zero    - do not abort
 *  malloc() returns a NULL if it does not allocate space.
 *  _F90ALLOC does not check for a zero size input argument.
 */

void *
_F90ALLOC( _f_int size,
	   _f_int aflag)
{
	void *retval;
	/*
	 * size will not be .le.zero on cf77 but can be
	 * .le.zero on cf90
	 */
	if (size <= 0)
		return(NULL);
	if ((retval = malloc(size)) == NULL) {
		if (aflag)
			abort();
	}
	return(retval);
}

/*
 *   _F90FREE(void *addr, _f_int flag)
 *      
 *  where addr  = address of storage to be freed.  The address was
 *                from a previous _F90ALLOC call.
 *        flag  = default integer containing:
 *                	nonzero - abort
 *                	zero    - do not abort
 *  Free() does not return an error status.
 *  _F90FREE does not check for a null address input argument.
 */
_f_int
_F90FREE( void *addr,
	_f_int aflag)
{
	_f_int retval = 0;
	/*
	 * addr may be a NULL pointer in cf90 but will
	 * not be a NULL pointer for cf77.
	 */
	if (addr != NULL)
		free(addr);
	return(retval);
}
