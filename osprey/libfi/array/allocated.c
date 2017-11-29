/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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


#pragma ident "@(#) libfi/array/allocated.c	92.1	07/07/99 15:52:02"

#include <cray/dopevec.h>

#define TRUE 1
#define FALSE 0

/*
 *    ALLOCATED Returns a logical scalar indicating whether an allocatable
 *                array is allocated.
 *              Returns TRUE if the allocatable array is allocated.
 *                Otherwise returrn FALSE.
 *              The result is undefined if the allocation status of the
 *                array is undefined.
 */

_f_log
_ALLOCATED (DopeVectorType * source)
{

	_f_log iresult;
	iresult = FALSE;
	/* Is source is an allocatable array and allocated. */
	if ((source->p_or_a == ALLOC_ARRY) && (source->assoc))
		iresult = TRUE;
        return(_btol(iresult));
}


#ifdef	_F_LOG4
_f_log4
_ALLOCATED_4 (DopeVectorType * source)
{

	_f_log4 iresult;
	iresult = FALSE;
	/* Is source is an allocatable array and allocated. */
	if ((source->p_or_a == ALLOC_ARRY) && (source->assoc))
		iresult = TRUE;
        return(_btol(iresult));
}
#endif


#ifdef	_F_LOG8
_f_log8
_ALLOCATED_8 (DopeVectorType * source)
{

	_f_log8 iresult;
	iresult = FALSE;
	/* Is source is an allocatable array and allocated. */
	if ((source->p_or_a == ALLOC_ARRY) && (source->assoc))
		iresult = TRUE;
        return(_btol(iresult));
}
#endif
