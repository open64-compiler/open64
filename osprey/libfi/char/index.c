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


#pragma ident "@(#) libfi/char/index.c	92.1	07/08/99 10:41:51"

/*
 *	INDEX -	Returns the starting position of a substring within
 *		a string.
 */

#include <fortran.h>
#include <string.h>
#include <stddef.h>
#include <cray/portdefs.h>

/*
 *	Duplicate names
 *
 *	_INDEX		- for f90 intrinsic when explicitly called
 *	$INDEX		- for cf77 intrinsic
 *	INDEX		- for cf77 when declared EXTERNAL (necessary to permit
 *			  passing the optional argument with cf77).
 *
 *	The F90 compiler generates a call to _INDEX to process explicit
 *	calls to the INDEX intrinsic, which have one optional argument.	
 */
#ifdef	_UNICOS
#pragma	_CRI	duplicate _INDEX as $INDEX
#pragma	_CRI	duplicate _INDEX as INDEX
#endif

_f_int
_INDEX(
	_fcd	fstr1,
	_fcd	fstr2,
	_f_log	*fback)		/* NULL if argument not passed */
{
	register short	forward; /* Nonzero if searching forward */
	register long	indx;	/* Position where substring located */
	register long	len1, len2;
	char		*str1, *str2;

	indx	= 0;		/* Assume substring not found */
	forward	= 1;		/* Assume forward search */

	/* Check if backwards search */

#if	defined(_UNICOS) && defined(_ADDR64)
	if ((_numargs()  > 2 * sizeof(_fcd) / sizeof(long)) && fback != NULL &&
		_ltob(fback))
#elif	defined(_UNICOS)
	if (_numargs() > 2 && fback != NULL && _ltob(fback))
#else
	if (fback != NULL && _ltob(fback))
#endif
		forward	= 0;

	/* Convert Fortran character descriptors to C pointers and lengths */

	str1	= _fcdtocp(fstr1);
	str2	= _fcdtocp(fstr2);

	len1	= _fcdlen (fstr1);
	len2	= _fcdlen (fstr2);

	if (len1 >= len2) {	/* If string can at least hold substring */
		char	*offset;	/* Offset of substring */

		if (len2 == 1 && forward)	/* Optimize this common case */
			offset	= memchr(str1, (int) *str2, len1);
		else
			offset	= forward ?
				  strnstrn (str1, len1, str2, len2) :
				  strnrstrn(str1, len1, str2, len2);

		if (offset != NULL)	/* If substring found */
			indx	= 1 + (offset - str1);

	}

	return ((_f_int) indx);
}

/*
 *	_INDEX_		- for f90 intrinsic when passed as an argument
 *
 *	The implicit call to the INDEX intrinsic in these cases (through
 *	_INDEX_) must be made with only two arguments.  This minimal
 *	support for passing INDEX as an actual argument is in Fortran 90
 *	for upward compatibility with the Fortran 77 standard.
 */
_f_int
_INDEX_(
	_fcd	fstr1,
	_fcd	fstr2)
{
	return( _INDEX(fstr1, fstr2, NULL));	/* Only two arguments passed */
}


#ifdef	_F_INT4
/*
 *	Duplicate names
 *
 *	_INDEX_4	- for f90 intrinsic when explicitly called
 *	$INDEX_4	- for cf77 intrinsic
 *	INDEX_4		- for cf77 when declared EXTERNAL (necessary to permit
 *			  passing the optional argument with cf77).
 *
 *	The F90 compiler generates a call to _INDEX to process explicit
 *	calls to the INDEX intrinsic, which have one optional argument.	
 */
#ifdef	_UNICOS
#pragma	_CRI	duplicate _INDEX_4 as $INDEX_4
#pragma	_CRI	duplicate _INDEX_4 as INDEX_4
#endif

_f_int4
_INDEX_4(
	_fcd	fstr1,
	_fcd	fstr2,
	_f_log	*fback)		/* NULL if argument not passed */
{
	register short	forward; /* Nonzero if searching forward */
	register long	indx;	/* Position where substring located */
	register long	len1, len2;
	char		*str1, *str2;

	indx	= 0;		/* Assume substring not found */
	forward	= 1;		/* Assume forward search */

	/* Check if backwards search */

#if	defined(_UNICOS) && defined(_ADDR64)
	if ((_numargs()  > 2 * sizeof(_fcd) / sizeof(long)) && fback != NULL &&
		_ltob(fback))
#elif	defined(_UNICOS)
	if (_numargs() > 2 && fback != NULL && _ltob(fback))
#else
	if (fback != NULL && _ltob(fback))
#endif
		forward	= 0;

	/* Convert Fortran character descriptors to C pointers and lengths */

	str1	= _fcdtocp(fstr1);
	str2	= _fcdtocp(fstr2);

	len1	= _fcdlen (fstr1);
	len2	= _fcdlen (fstr2);

	if (len1 >= len2) {	/* If string can at least hold substring */
		char	*offset;	/* Offset of substring */

		if (len2 == 1 && forward)	/* Optimize this common case */
			offset	= memchr(str1, (int) *str2, len1);
		else
			offset	= forward ?
				  strnstrn (str1, len1, str2, len2) :
				  strnrstrn(str1, len1, str2, len2);

		if (offset != NULL)	/* If substring found */
			indx	= 1 + (offset - str1);

	}

	return ((_f_int4) indx);
}

/*
 *	_INDEX_4_	- for f90 intrinsic when passed as an argument
 *
 *	The implicit call to the INDEX_4 intrinsic in these cases (through
 *	_INDEX_4_) must be made with only two arguments.  This minimal
 *	support for passing INDEX_4 as an actual argument is in Fortran 90
 *	for upward compatibility with the Fortran 77 standard.
 */
_f_int4
_INDEX_4_(
	_fcd	fstr1,
	_fcd	fstr2)
{
	return( _INDEX_4(fstr1, fstr2, NULL));	/* Only two arguments passed */
}
#endif


#ifdef	_F_INT8
/*
 *	Duplicate names
 *
 *	_INDEX_8	- for f90 intrinsic when explicitly called
 *	$INDEX_8	- for cf77 intrinsic
 *	INDEX_8		- for cf77 when declared EXTERNAL (necessary to permit
 *			  passing the optional argument with cf77).
 *
 *	The F90 compiler generates a call to _INDEX_8 to process explicit
 *	calls to the INDEX intrinsic, which have one optional argument.	
 */
#ifdef	_UNICOS
#pragma	_CRI	duplicate _INDEX_8 as $INDEX_8
#pragma	_CRI	duplicate _INDEX_8 as INDEX_8
#endif

_f_int8
_INDEX_8(
	_fcd	fstr1,
	_fcd	fstr2,
	_f_log	*fback)		/* NULL if argument not passed */
{
	register long	indx;	/* Position where substring located */
	register long	len1, len2;
	register short	forward; /* Nonzero if searching forward */
	char		*str1, *str2;

	indx	= 0;		/* Assume substring not found */
	forward	= 1;		/* Assume forward search */

	/* Check if backwards search */

#if	defined(_UNICOS) && defined(_ADDR64)
	if ((_numargs()  > 2 * sizeof(_fcd) / sizeof(long)) && fback != NULL &&
		_ltob(fback))
#elif	defined(_UNICOS)
	if (_numargs() > 2 && fback != NULL && _ltob(fback))
#else
	if (fback != NULL && _ltob(fback))
#endif
		forward	= 0;

	/* Convert Fortran character descriptors to C pointers and lengths */

	str1	= _fcdtocp(fstr1);
	str2	= _fcdtocp(fstr2);

	len1	= _fcdlen (fstr1);
	len2	= _fcdlen (fstr2);

	if (len1 >= len2) {	/* If string can at least hold substring */
		char	*offset;	/* Offset of substring */

		if (len2 == 1 && forward)	/* Optimize this common case */
			offset	= memchr(str1, (int) *str2, len1);
		else
			offset	= forward ?
				  strnstrn (str1, len1, str2, len2) :
				  strnrstrn(str1, len1, str2, len2);

		if (offset != NULL)	/* If substring found */
			indx	= 1 + (offset - str1);

	}

	return ((_f_int8) indx);
}

/*
 *	_INDEX_8_	- for f90 intrinsic when passed as an argument
 *
 *	The implicit call to the INDEX_8 intrinsic in these cases (through
 *	_INDEX_8_) must be made with only two arguments.  This minimal
 *	support for passing INDEX as an actual argument is in Fortran 90
 *	for upward compatibility with the Fortran 77 standard.
 */
_f_int8
_INDEX_8_(
	_fcd	fstr1,
	_fcd	fstr2)
{
	return( _INDEX_8(fstr1, fstr2, NULL));	/* Only two arguments passed */
}
#endif

#ifdef	_F_INT2
/*
 *	Duplicate names
 *
 *	_INDEX_2	- for f90 intrinsic when explicitly called
 *
 *	The F90 compiler generates a call to _INDEX to process explicit
 *	calls to the INDEX intrinsic, which have one optional argument.	
 */
_f_int2
_INDEX_2(
	_fcd	fstr1,
	_fcd	fstr2,
	_f_log	*fback)		/* NULL if argument not passed */
{
	register short	forward; /* Nonzero if searching forward */
	register long	indx;	/* Position where substring located */
	register long	len1, len2;
	char		*str1, *str2;

	indx	= 0;		/* Assume substring not found */
	forward	= 1;		/* Assume forward search */

	/* Check if backwards search */

#if	defined(_UNICOS) && defined(_ADDR64)
	if ((_numargs()  > 2 * sizeof(_fcd) / sizeof(long)) && fback != NULL &&
		_ltob(fback))
#elif	defined(_UNICOS)
	if (_numargs() > 2 && fback != NULL && _ltob(fback))
#else
	if (fback != NULL && _ltob(fback))
#endif
		forward	= 0;

	/* Convert Fortran character descriptors to C pointers and lengths */

	str1	= _fcdtocp(fstr1);
	str2	= _fcdtocp(fstr2);

	len1	= _fcdlen (fstr1);
	len2	= _fcdlen (fstr2);

	if (len1 >= len2) {	/* If string can at least hold substring */
		char	*offset;	/* Offset of substring */

		if (len2 == 1 && forward)	/* Optimize this common case */
			offset	= memchr(str1, (int) *str2, len1);
		else
			offset	= forward ?
				  strnstrn (str1, len1, str2, len2) :
				  strnrstrn(str1, len1, str2, len2);

		if (offset != NULL)	/* If substring found */
			indx	= 1 + (offset - str1);

	}

	return ((_f_int2) indx);
}

/*
 *	_INDEX_2_	- for f90 intrinsic when passed as an argument
 *
 *	The implicit call to the INDEX_2 intrinsic in these cases (through
 *	_INDEX_2_) must be made with only two arguments.  This minimal
 *	support for passing INDEX_2 as an actual argument is in Fortran 90
 *	for upward compatibility with the Fortran 77 standard.
 */
_f_int2
_INDEX_2_(
	_fcd	fstr1,
	_fcd	fstr2)
{
	return( _INDEX_2(fstr1, fstr2, NULL));	/* Only two arguments passed */
}
#endif

#ifdef	_F_INT1
/*
 *	Duplicate names
 *
 *	_INDEX_1	- for f90 intrinsic when explicitly called
 *
 *	The F90 compiler generates a call to _INDEX to process explicit
 *	calls to the INDEX intrinsic, which have one optional argument.	
 */
_f_int1
_INDEX_1(
	_fcd	fstr1,
	_fcd	fstr2,
	_f_log	*fback)		/* NULL if argument not passed */
{
	register short	forward; /* Nonzero if searching forward */
	register long	indx;	/* Position where substring located */
	register long	len1, len2;
	char		*str1, *str2;

	indx	= 0;		/* Assume substring not found */
	forward	= 1;		/* Assume forward search */

	/* Check if backwards search */

#if	defined(_UNICOS) && defined(_ADDR64)
	if ((_numargs()  > 2 * sizeof(_fcd) / sizeof(long)) && fback != NULL &&
		_ltob(fback))
#elif	defined(_UNICOS)
	if (_numargs() > 2 && fback != NULL && _ltob(fback))
#else
	if (fback != NULL && _ltob(fback))
#endif
		forward	= 0;

	/* Convert Fortran character descriptors to C pointers and lengths */

	str1	= _fcdtocp(fstr1);
	str2	= _fcdtocp(fstr2);

	len1	= _fcdlen (fstr1);
	len2	= _fcdlen (fstr2);

	if (len1 >= len2) {	/* If string can at least hold substring */
		char	*offset;	/* Offset of substring */

		if (len2 == 1 && forward)	/* Optimize this common case */
			offset	= memchr(str1, (int) *str2, len1);
		else
			offset	= forward ?
				  strnstrn (str1, len1, str2, len2) :
				  strnrstrn(str1, len1, str2, len2);

		if (offset != NULL)	/* If substring found */
			indx	= 1 + (offset - str1);

	}

	return ((_f_int1) indx);
}

/*
 *	_INDEX_1_	- for f90 intrinsic when passed as an argument
 *
 *	The implicit call to the INDEX_1 intrinsic in these cases (through
 *	_INDEX_1_) must be made with only two arguments.  This minimal
 *	support for passing INDEX_1 as an actual argument is in Fortran 90
 *	for upward compatibility with the Fortran 77 standard.
 */
_f_int1
_INDEX_1_(
	_fcd	fstr1,
	_fcd	fstr2)
{
	return( _INDEX_1(fstr1, fstr2, NULL));	/* Only two arguments passed */
}
#endif
