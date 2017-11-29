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


#pragma ident "@(#) libu/waio/wdset.c	92.1	06/23/99 13:55:03"
#include <ctype.h>
#include <string.h>
#include <cray/fortio.h>
#include "waio.h"

/*
 * 	Table for constructing file names which correspond to unit numbers.
 */
static long table[] = {
	'0\0001\0002\0003\000', '4\0005\0006\0007\000', 
	'8\0009\0001011',	'12131415',
	'16171819',		'20212223',
	'24252627',		'28293031',
	'32333435',		'36373839',
	'40414243',		'44454647',
	'48495051',		'52535455',
	'56575859',		'60616263',
	'64656667',		'68697071',
	'72737475',		'76777879',
	'80818283',		'84858687',
	'88899091',		'92939495',
	'96979899'
};

#define LOOKUP(xx)    (((table[xx >> 2] << ((xx&03)<<4)) & (0177777<<48)) >> 40)

static long last_iunit = 0;		/* Store last input/output values of */
static long last_idn   = 'fort.0\0\0';	/* WDSET as an optimization.	     */

/*
 *    WDSET - set file name
 *
 *    CALL WDSET(IUNIT, IDN)
 *    where:   IUNIT(i) = unit number or (left-justified) ascii file name
 *             IDN  (o) = null-padded ascii file name if ok
 *                      = 0 on exit if unit number error
 *                      = 1 on ascii name error
 */
void
WDSET(iunit, idn)
long	*iunit, *idn;
{
	char	*cp;
	int 	i, j;
	long	iunit_val;
	long	idn_val;

	iunit_val = *iunit;
	if (iunit_val == last_iunit) 
		idn_val = last_idn;
	else if (iunit_val < 0)
		idn_val = 0; 
	else if (iunit_val >> 56) {	/* an ascii file name */
		idn_val = RBN(&iunit_val);
	}
	else {				/* a numeric unit number */
		register char *c;
  
		if (iunit_val >= 100)
			idn_val = 0; 
		else {
			idn_val  = 'fort.\0\0\0' | LOOKUP(iunit_val);
		}
	}
	last_iunit = iunit_val;		/* save last input value */
	last_idn   = idn_val;		/* save last output value */
	*idn	   = idn_val;
}

/*
 *	WDSETB - return current value of the buffer address
 */
WDSETB(dn, index, addr)
long	*dn, *index, **addr;
{
	WAFIL	*f;
	f     = &wafils[*index-1];
	*addr = (long*)f->wa_buffer;
}
