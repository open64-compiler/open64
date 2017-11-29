/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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



#pragma ident "@(#) libf/fio/s2ul.c	92.1	06/18/99 18:41:02"
#include <fortran.h>
#include <cray/fmtconv.h>

extern oc_func _S2UL;		/* Interface must match oc_func prototype */

/*
 *	_S2UL() Convert Fortran logical variable to external format.
 *
 *	Entry:
 *		value	Address of logical variable
 *		fca	Address of first unpacked character
 *		mode	Unused
 *		width	Field width
 *		digits	Unused
 *		exp	Unused
 *		scale	Unused
 *
 *	Exit:
 *		result	Points to end of output field
 *
 *	Note:	This routine has the same parameters as S2UI, etc. in
 *		libc.
 */

long *
_S2UL(
const void	*value,
long		*fca,
const long	*mode,
const long	*width,
const long	*digits,
const long	*exp,
const long	*scale
)
{
	int	i;
	long	fw, *ptr;
	char	ch;
	fw	= *width - 1;
#ifdef	_F_LOG4
	if ((*mode & MODEHP) != 0)
		ch	= _lvtob( *(_f_log4 *)value) ? 'T' : 'F';
	else
/* KEY: This used to have __mips */
#if	defined(_F_LOG2)
		if ((*mode & MODEWP) != 0) {
			ch	= _lvtob( *(_f_log2 *)value) ? 'T' : 'F';
	} else if ((*mode & MODEBP) != 0) {
		ch	= _lvtob( *(_f_log1 *)value) ? 'T' : 'F';
	} else 
#endif	/* _F_LOG2 and MIPS */
#endif	/* _F_LOG4 */
	{
		ch	= _lvtob( *(_f_log8 *)value) ? 'T' : 'F';
	}

	/* The following loop should vectorize */

	for (i = 0; i < fw; i++)
		fca[i]	= (long) ' ';

	ptr	= fca + fw;
	*ptr++	= (long) ch;

	return (ptr);
}
