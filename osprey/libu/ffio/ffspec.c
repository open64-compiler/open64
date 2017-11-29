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


#pragma ident "@(#) libu/ffio/ffspec.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
/*
 * _ff_nparm_getv
 *
 * Gets numeric parameter value.  If the parameter was not valid, a value of 0 
 * is returned.  *isvalid is set to 1 if the requested parameter is valid, 0
 * if not.
 *
 * As an example, in the spec "user::3:0":
 *
 *		parameter 1 is invalid
 *		parameter 2 is valid and has a value of 3
 *		parameter 3 is valid and has a value of 0
 *		parameters 4,5,6... are invalid
 *
 * This routine should be used by the open routines in any layer which
 * must process numeric parameters.
 *
 * Note: _ff_nparm_getv works with ffopens and asgcmd from release
 * 5.1 and later.  
 */
_nparmtype
_ff_nparm_getv(union spec_u *spec, int parnum, int *isvalid)
{
	int count;
	_nparmtype value;
	int valid;

	valid = 0;	/* assume invalid parameter number */
	value = 0;	/* assume invalid parameter number */

	count = 1;
	while (spec[count-1].info.ext)
		count++;		/* count the number of spec words */

	if (spec->fss.class == CLASS_SDS || spec->fss.class == CLASS_MR) {
		/*
		 * These classes have a style of spec which forces exactly
		 * three numeric parameters. 
		 */
		if (parnum <= count) {
			valid = 1;
			if (parnum == 1)
				value = spec[0].fss.min;
			else
				value = spec[parnum-1].info.quan;
		}
	}
	else {
		count -= 1;			/* number of numeric params */

		if (count == 0) {		/* check for old spec style */
			/*
			 * Force valid if we spot recsize or mbs with nonzero
			 * values in them.  This spec must have been created
			 * by 7.C and previous versions of asgcmd.
			 *
			 * This check can be removed when asgcmd from UNICOS 7.C
			 * and previous need not be compatible with programs
			 * compiled with this level of _ff_nparm_getv().
			 */
			if (parnum == 1) {
				if ((value = spec[0].fld.recsize) != 0)
					valid = 1;
			}
			else if (parnum == 2) {
				if ((value = spec[0].fld.mbs) != 0)
					valid = 1;
			}
		}
		else if (parnum <= count && spec[parnum].info.valid) {
			valid = 1;
			value = spec[parnum].info.quan;
		}
	}
	*isvalid = valid;
	return(value);
}
/*
 * _ff_nparm_getcnt
 *
 * Gets the number of the last numeric parameter passed in an FFIO spec list.
 * For example, '-F user::2::4' and '-F user:1:2:3:4' both return 4.  And
 * '-F user' and '-F user:::' both return 0.
 *
 * Note: _ff_nparm_getcnt works only with ffopens and asgcmd from release
 * 5.1 and later.
 */
int
_ff_nparm_getcnt(union spec_u *spec)
{
	int count;

	if (spec->fss.class == CLASS_SDS || spec->fss.class == CLASS_MR)
		/*
		 * These classes have a style of spec which forces exactly
		 * three numeric parameters. 
		 */
		return(3);

	count = 0;
	/*
	 * Count the number of spec words beyond the first.
	 */
	while (spec[count].info.ext)
		count++;
	/*
	 * Subtract any trailing parameters which are invalid.
 	 */
	while (count > 0 && spec[count].info.valid == 0)
		count--;

	if (count == 0) {		/* check for old spec style */
		/*
		 * Assume old spec format if we spot recsize or mbs with nonzero
		 * values in them.  This spec must have been created by 6.1 
		 * and previous versions of asgcmd.
		 */
		if (spec[0].fld.mbs != 0)
			count = 2;
		else if (spec[0].fld.recsize != 0)
			count = 1;
	}

	return(count);
}
