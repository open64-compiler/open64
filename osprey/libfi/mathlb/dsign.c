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


#pragma ident "@(#) libfi/mathlb/dsign.c	92.1	09/29/99 18:35:37"

#include <fortran.h>
#include <math.h>

/*
 * DSIGN: SIGN of real(kind=16)  - pass by address
 * 128-bit float sign
 *    Return the absolute value of x with the sign of y.
 */

/*
 * On mips, only 32-bit float and 64-bit double sign can be passed as
 * arguments and thus have addresses of values rather than values for
 * arguments.  (SIGN and DSIGN)
 *
 * When f90 -default64 is used, the two entry points for DSIGN are:
 *  
 *   __q_sign_ when f90 -LANG:IEEE_minus_zero is present.
 *   __q_sign when f90 -LANG:IEEE_minus_zero is not present.
 *
 * This routine is necessary to get the rout8ine to recognize -0.0
 */

union _ldflt {
	_f_real16	dbldbl;
	struct {
		unsigned int sign	: 1;
		unsigned int flt1	: 31;
		unsigned int flt2	: 32;
		unsigned int flt3	: 32;
		unsigned int flt4	: 32;
	} parts;
};

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
extern _f_real16  __q_sign_(_f_real16 *x, _f_real16 *y);
_f_real16
__q_sign_( _f_real16 *x,
	_f_real16 *y)
{
	union _ldflt	f, rslt;
	rslt.dbldbl	= *x;
	f.dbldbl	= *y;
	rslt.parts.sign	= f.parts.sign;
	return (rslt.dbldbl);
}

extern _f_real16  __q_sign(_f_real16 *x, _f_real16 *y);
_f_real16
__q_sign( _f_real16 *x,
	_f_real16 *y)
{
	union _ldflt	f, rslt;
	rslt.dbldbl	= *x;
	f.dbldbl	= *y;
	/* If the second argument is -0.0, reset the sign. */
	if (f.dbldbl == 0)
		f.parts.sign	= 0;
	rslt.parts.sign	= f.parts.sign;
	return (rslt.dbldbl);
}
#else
extern _f_real16  _DSIGN_(_f_real16 *x, _f_real16 *y);
_f_real16
_DSIGN_( _f_real16 *x,
	_f_real16 *y)
{
	union _ldflt	f, rslt;
	rslt.dbldbl	= *x;
	f.dbldbl	= *y;
	rslt.parts.sign	= f.parts.sign;
	return (rslt.dbldbl);
}
#endif
