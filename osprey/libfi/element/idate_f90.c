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


#pragma ident "@(#) libfi/element/idate_f90.c	92.2	06/16/99 15:47:23"
#include <fortran.h>
#include <sys/types.h>
#include <time.h>

/*
 * IDATE returns date in numerical form:
 *
 *	integer month, day, year
 *	call IDATE(month, day, year)
 *
 * where:
 *	month will receive the current month, day the current
 *	day, and year the current year modulo 100.
 *      Year will be not be a four-digit year since it is the
 *	year since 1900 
 *
 * or an older form:
 *
 *	integer iarray(3)
 *	call IDATE(iarray)
 *
 * where:
 *	iarray will receive the current date; day, mon, year.
 *	The year will be a four-digit year, i.e., the year
 *	since 1900 will have 1900 added.
 *
 */

/* Use time() and localtime() to get the fields for idate */
#define __IDATE_SCALAR()				\
	struct tm	*locltm;			\
	time_t		t;				\
	t	= time(0);				\
	locltm	= localtime(&t);			\
	*imon	= locltm->tm_mon + 1;			\
	*jday	= locltm->tm_mday;			\
	*kyear	= locltm->tm_year % 100;

#define __IDATE_ARRAY()					\
	struct tm	*locltm;			\
	time_t		t;				\
	t	= time(0);				\
	locltm	= localtime(&t);			\
	iarray[0]	= locltm->tm_mday;		\
	iarray[1]	= locltm->tm_mon + 1;		\
	iarray[2]	= locltm->tm_year + 1900;

/* Note that the year is not four digits, i.e., year since 1900 in
 * localtime() % 100. The order of the scalar arguments is month,
 * day, and year.
 */
extern void _IDATE_I(int *imon, int *jday, int *kyear)
{
	__IDATE_SCALAR();
}

extern void _IDATE_I1(_f_int1 *imon, _f_int1 *jday, _f_int1 *kyear)
{
	__IDATE_SCALAR();
}

extern void _IDATE_I2(_f_int2 *imon, _f_int2 *jday, _f_int2 *kyear)
{
	__IDATE_SCALAR();
}

extern void _IDATE_I4(_f_int4 *imon, _f_int4 *jday, _f_int4 *kyear)
{
	__IDATE_SCALAR();
}

extern void _IDATE_I8(_f_int8 *imon, _f_int8 *jday, _f_int8 *kyear)
{
	__IDATE_SCALAR();
}

/* On IRIX, f77 with an array returns the order as day, month, and
 * year rather than month, day, year.  The year is four digits.
 */
extern void _IDATE_A(int iarray[3])
{
	__IDATE_ARRAY();
}

extern void _IDATE_A1(_f_int1 iarray[3])
{
	__IDATE_ARRAY();
}

extern void _IDATE_A2(_f_int2 iarray[3])
{
	__IDATE_ARRAY();
}

extern void _IDATE_A4(_f_int4 iarray[3])
{
	__IDATE_ARRAY();
}

extern void _IDATE_A8(_f_int8 iarray[3])
{
	__IDATE_ARRAY();
}

