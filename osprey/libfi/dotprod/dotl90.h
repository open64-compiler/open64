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


CDIR$ ID "@(#) libfi/dotprod/dotl90.h	92.0	10/08/98 14:37:14"
*********************************************************************
*  Special version for logical                                      *
*                                                                   *
*  Header file for all dot product routines.  This file is included *
*  by each of the specific dot product routines.  The file that     *
*  includes this one just declares the name of the routine, and the *
*  types of the variables x, y, and s.  This routine does the       *
*  actual computation.                                              *
*                                                                   *
*  Author:                                                          *
*  Math Software Group                                              *
*  Cray Research, Inc.                                              *
*  Eagan, Minnesota                                                 *
*                                                                   *
*********************************************************************

* p_x is a pointer to array x (i.e, contains base address of x)
* p_y is a pointer to array y (i.e, contains base address of y)
*
	pointer (p_x, x)
	pointer (p_y, y)
*
* Both arguments are dope vectors.
*
	integer dopevec1(*), dopevec2(*)
*
	include "dotcom.h"	! common block definition
*
* Predot is the subroutine used to parse the dope vectors,
* storing the required values in the common block.
*
	external _PREDOT
	integer i
*
* Parse the dope vectors, and store the vector addresses
* and increments in the common block.
*
	call _PREDOT(dopevec1, dopevec2, n, locx, incx, locy, incy)
*
*	Store base address of vectors in pointer variables.
*
	p_x = locx
	p_y = locy
*
* Compute the dot product
*
	s = .FALSE.

	do, i = 1, n
	    s = s .OR. ( x(1 + (i-1)*incx) .AND. y(1 + (i-1)*incy) )
	end do
