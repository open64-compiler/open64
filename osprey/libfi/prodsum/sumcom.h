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


* USMID @(#) libfi/prodsum/sumcom.h	92.0	10/08/98 14:37:14
*	Declaration of variables used by the SUM and PRODUCT array
*	intrinsic routines.
*
*	If these variables change, you must also change presum.c
*
*	Author:
*	Math Software Group
*
*       These are F77-style declarations
*	.. variables ..
	integer maxdim
	parameter (maxdim = 7)
	integer ndim		! number of dimensions
	logical	dimarg		! true iff DIM arg was given
	integer dimx		! dimension to sum (if DIM arg given) 
	logical maskarg		! true iff MASK arg was given
	logical scalar		! true iff sum is a scalar
	integer loca		! address of array A
	integer lima(maxdim)	! extent(i) of array A
	integer inca(maxdim)	! increment(i) of array A
	integer locb		! address of array B
	integer limb(maxdim)	! extent(i) of array B
	integer incb(maxdim)	! increment(i) of array B
	integer locm		! address of array MASK (if present)
	integer limm(maxdim)	! extent(i) of array MASK (if present)
	integer incm(maxdim)	! increment(i) of array MASK (if present)
	integer sizem		! size of mask in bytes
	integer loc4m		! address of array MASK (4-byte)
