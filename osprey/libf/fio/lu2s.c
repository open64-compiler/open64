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



#pragma ident "@(#) libf/fio/lu2s.c	92.1	06/21/99 10:37:21"
#include <fortran.h>
#include <cray/fmtconv.h>

 extern ic_func _LU2S;		/* Interface must match ic_func prototype */

/*
 *	_LU2S() Convert Fortran logical input to internal format.
 *
 *      Valid Fortran logical input consists of the following:
 *
 *		Zero or more blanks, optionally followed by:
 *		  Zero or one period, optionally followed by:
 *		    Zero or more blanks, immediately followed by:
 *		  'T' or 'F' or 't' or 'f', optionally followed by:
 *		    Zero or more characters
 *
 *		A field of all blanks is interpreted as .FALSE.
 *
 *	Entry:
 *		fca	Address of first unpacked character
 *		width	Field width
 *		lcap1	Pointer to pointer to last character address plus one
 *		mode	Unused
 *		result	Address to store result
 *		status	Pointer to status word
 *		digits	Unused
 *		scale	Unused
 *
 *	Exit:
 *		lcap1	Pointer updated to last character read (plus one)
 *		result	Updated to .TRUE. or .FALSE., if valid input
 *		status	Updated status:
 *			0		  Valid Fortran logical input
 *			EX_INVLOGI (-10)  Invalid Fortran logical input
 *
 *	The function result is the same as the status.
 *
 *	Note:	This routine has the same parameters as IU2S, etc. in
 *		libc.
 */

int
_LU2S(
const long	*fca,
const long	*width,
long		**lcap1,
const long	*mode,
void		*result,
long		*status,
const long	*digits,
const long	*scale
)
{
	char	ch;
	int	fw;
	int	stat;
	int	value;

	stat	= 0;
	value	= 0;			/* Assume .FALSE. */
	fw	= *lcap1 - fca;		/* Maximum possible field width */

	if (*width < fw)
		fw	= *width;	/* Set actual width */

	if (fw > 0) {	/* If there is a field */

		*lcap1	= (long *)fca + fw;
		ch	= (char) *fca++;

		/* Skip optional blanks */

		while (ch == ' ' && fw > 0) {
			ch	= (char) *fca++;
			fw--;
		}

		/* Process nonblank character */

		if (fw > 0) {		/* If not at end of field */

			/* Skip optional period */

			if (ch == '.') {
				ch	= (char) *fca++;
				value	= -1;	/* TRUE or FALSE now required */
				fw--;
			}

			/* Skip optional blanks */

			while (ch == ' ' && fw > 0) {
				ch	= (char) *fca++;
				fw--;
			}

			/* Process required character */

			if (fw > 0) {	/* If not at end of field */

				if (ch == 'T' || ch == 't')
					value	= 1;
				else
					if (ch == 'F' || ch == 'f')
						value	= 0;
					else
						value	= -1;
			}
		}

	}

	if (value >= 0)	{	/* If we found something */
#ifdef _F_LOG4
		if (*mode & MODEHP) 
			*(_f_log4 *)result	= _btol(value);
		else
#if	defined(_F_LOG2) && (defined(__mips) || defined(_LITTLE_ENDIAN))
		    if (*mode & MODEWP) 
			*(_f_log2 *)result	= (_f_log2) _btol(value);
		else if (*mode & MODEBP) 
			*(_f_log1 *)result	= (_f_log1) _btol(value);
		else
#endif	/* _F_LOG2 and (mips or little endian) */
#endif	/* _F_LOG4 */
			*(_f_log8 *)result	= _btol(value);
	}
	else
		stat	= EX_INVLOGI;

	*status	= stat;

	return (stat);
}
