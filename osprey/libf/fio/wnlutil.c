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



#pragma ident "@(#) libf/fio/wnlutil.c	92.1	06/21/99 10:37:55"

#include <liberrno.h>
#include <fortran.h>
#include "fio.h"

#define	WNLMIN	8L			/* Minimum WNLLONG value */

long	_wnlrecsiz	= -1;		/* Length of output line.  -1 indicates
					   the default */
long	OUT_CHAR	= (long) '&';	/* Delimiter character preceding the
					   group name and END */
long	OUT_SEP		= (long) ',';	/* Separator character immediately
					   following each value */
long	OUT_EQ		= (long) '=';	/* Replacement operator that comes
					   between name and value */
long	OUT_ECHO	= (long) ' ';	/* Character written to column 1 */

long	OUT_LINE	= 0;		/* If nonzero, write a new line for
					   each variable */

/*
 * Return a character, which might be 'A', 'A'L, 'A'R, or 'A'H format.
 */
long
_getfchar(_fcd fc)
{
	long	ret, x;

#if defined(_UNICOS) && (!defined(_ADDR64) && !defined(_WORD32))
	if (!_isfcd(fc)) {
		x	= *(long *)_fcdtocp(fc);
		ret	= (x >> 56) & 0377;	/* 'A'L or 'A'H format */

		if (ret == 0)
			ret	= x & 0377;	/* 'A'R format */
	}
	else 
#endif
		ret	= *(_fcdtocp(fc));	/* 'A' format */

	if (ret == 0)
		_ferr(NULL, FENLTYPE);

	return(ret);
}

#if defined(_UNICOS)
/*
 * WNLLONG changes the maximum output record length for namelist writes
 * (note restrictions in wnly.c).
 */
WNLLONG(_f_int *length)
{
	long	len;

	len	= (long) *length;

	if (len < 0)		/* Restore default value */
		_wnlrecsiz = -1L;
	else				/* Enforce a floor */
		if (len < WNLMIN)
			_wnlrecsiz = WNLMIN;
		else
			_wnlrecsiz = len;

	return(0);
}
#endif

/*
 * WNLDELM specifies the namelist delimiter.
 */
#ifdef _UNICOS
void
WNLDELM(_fcd fc)
{
	if (_numargs() != sizeof(_fcd)/sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST,"WNLDELM");
	OUT_CHAR = _getfchar(fc);
}
#else
void
wnldelm_(char *fc, int fclen)
{
	OUT_CHAR = _getfchar(_cptofcd(fc,(long)fclen));
}
#endif


/*
 * WNLSEP specifies the namelist seporator.
 */
#ifdef	_UNICOS
void
WNLSEP(_fcd fc)
{
	if (_numargs() != sizeof(_fcd)/sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST,"WNLSEP");
	OUT_SEP = _getfchar(fc);
}
#else
void
wnlsep_(char *fc, int fclen)
{
	OUT_SEP = _getfchar(_cptofcd(fc,(long)fclen));
}
#endif

/*
 * WNLREP specifies the namelist assignment operator character.
 */
#ifdef	_UNICOS
void
WNLREP(_fcd fc)
{
	if (_numargs() != sizeof(_fcd)/sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST,"WNLREP");
	OUT_EQ = _getfchar(fc);
}
#else
void
wnlrep_(char *fc, int fclen)
{
	OUT_EQ = _getfchar(_cptofcd(fc,(long)fclen));
}
#endif

/*
 * WNLFLAG specifies the character which is printed in the first column
 * of every line.
 */
#ifdef	_UNICOS
void
WNLFLAG(_fcd fc)
{
	if (_numargs() != sizeof(_fcd)/sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST,"WNLFLAG");
	OUT_ECHO = _getfchar(fc);
}
#else
void
wnlflag_(char *fc, int fclen)
{
	OUT_ECHO = _getfchar(_cptofcd(fc,(long)fclen));
}
#endif

/*
 * WNLLINE specifies the mode for new lines:
 *	0 => A new line is not started for every variable.
 *	1 => A new line is started for each variable.
 */
#ifdef	_UNICOS
void
WNLLINE(_f_int *x)
{
	OUT_LINE = (long) *x;
}
#else
void
wnlline_(_f_int *x)
{
	OUT_LINE = (long) *x;
}
#endif
