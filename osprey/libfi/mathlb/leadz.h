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


/* USMID @(#) libfi/mathlb/leadz.h	92.0	10/08/98 14:37:14 */


#include <cray/portdefs.h>
/* functions in this file are for inline use only.  They are NOT user
 * callable as declared in this file.
 */
#ifndef LEADZ_H
#define LEADZ_H

#define BIC(a, b) (a & (~ b))

/******************************   _my_leadz4 and _my_leadz8
 * Emulate CRI 64-bit leadz for 32-bit values and 64-bit values
 * # compute leading zeros of value in R01
 * # result is in R03
 * # uses registers R01-R04
 ************************************/
static int32
_leadz4(uint32 r01)
{
	uint32 r02 = 0, r03 = 0, r04 = 0;
	r02 = r01 >> 16;
	if (r02 > 0)
		r01 = r02;
	else /* if (r02 == 0) */
		r04 = 16;
	r02 = r01 >> 8;
	r03 = r04;
	if (r02 > 0)
		r01 = r02;
	else /* if (r02 == 0) */
	r04 = 8;
	r02 = r01 >> 4;
	r03 |= r04;
	if (r02 > 0)
		r01 = r02;
	else /*  if (r02 == 0) */
		r04 = 4;
	r02 = r01 >> 2;
	r03 |= r04;
	if (r02 > 0)
		r01 = r02;
	else /* if (r02 == 0) */
		r04 = 2;
	r02 = r01 < 2 ? 1 : 0;
	r01 = r01 < 1 ? 1 : 0;
	r03 |= r04;
	return r03 + r02 + r01;
}

static int64
_leadz8(uint64 r01)
{
	uint64  r02 = 0, r03 = 0, r04 = 0;
	r02 = r01 >> 32;
	if (r02 > 0)
		r01 = r02;
	else if (r02 == 0)
		r04 = 32;
	r02 = r01 >> 16;
	r03 = r04;
	if (r02 > 0)
		r01 = r02;
	else if (r02 == 0)
		r04 = 16;
	r02 = r01 >> 8;
	r03 |= r04;
	if (r02 > 0)
		r01 = r02;
	else if (r02 == 0)
		r04 = 8;
	r02 = r01 >> 4;
	r03 |= r04;
	if (r02 > 0)
		r01 = r02;
	else if (r02 == 0)
		r04 = 4;
	r02 = r01 >> 2;
	r03 |= r04;
	if (r02 > 0)
		r01 = r02;
	else if (r02 == 0)
		r04 = 2;
	r02 = r01 < 2 ? 1 : 0;
	r01 = r01 < 1 ? 1 : 0;
	r03 |= r04;
	return r03 + r02 + r01;
}
#endif                                 /* !LEADZ_H */
