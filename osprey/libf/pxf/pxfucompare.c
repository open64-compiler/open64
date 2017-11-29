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


#pragma ident "@(#) libf/pxf/pxfucompare.c	92.1	06/29/99 11:36:06"


#include <errno.h>
#include <liberrno.h>
#include <fortran.h>

/*
 *  PXFUCOMPARE  -- unsigned comparison
 *  (section 8.11.1 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFUCOMPARE(I1, I2, ICMPR, IDIFF)
 *     INTEGER I1, I2, ICMPR, IDIFF
 *
 *  Variable arguements:
 *
 *  I1     is an input integer variable for a C unsigned integer.
 *
 *  I2     is an input integer variable for a C unsigned integer.
 *
 *  ICMPR  is an output integer variable that on return from the
 *         routine contains one of these values:
 *
 *           -1    if I1 < I2.
 *            0    if I1 equals I2.
 *            1    if I1 > I2.
 *
 *         All of the comparisons are made using C unsigned integer
 *         comparisons.
 *
 *  IDIFF  is an output integer variable that on return from the
 *         routine contains the absolute value of the difference
 *         of I1 and I2. Since the values are C unsigned integers
 *         and Fortran 77 does not directly support unsigned integers
 *         the value may be negative, which indicates the value
 *         is beyond the maximum positive value of a Fortran 77
 *         integer.
 *
 */


#ifdef _UNICOS
void
PXFUCOMPARE(
#else
void
_PXFUCOMPARE(
#endif
	    _f_int *I1,
	    _f_int *I2,
	    _f_int *ICMPR,
	    _f_int *IDIFF
)
{
  if (((unsigned)*I1) < ((unsigned)*I2)) {
    *ICMPR = -1;
    *IDIFF = ((unsigned)*I2) - ((unsigned)*I1);
  } else if (((unsigned)*I1) == ((unsigned)*I2)) {
    *ICMPR = 0;
    *IDIFF = 0;
  } else if (((unsigned)*I1) > ((unsigned)*I2)) {
    *ICMPR = 1;
    *IDIFF = ((unsigned)*I1) - ((unsigned)*I2);
  }

}

#ifndef _UNICOS
void
pxfucompare_(
	     _f_int *I1,
	    _f_int *I2,
	    _f_int *ICMPR,
	    _f_int *IDIFF
)
{
  _PXFUCOMPARE(I1, I2, ICMPR, IDIFF);
}
#endif
