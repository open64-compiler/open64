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


#pragma ident "@(#) libfi/char/scan.c	92.3	07/30/99 10:09:59"

/*
 *	SCAN -	Scan a string for any one of the characters in a set
 *		of characters.
 */

#include <fortran.h>
#include <string.h>

/*
 * This routine assumes that an eight-bit character can occur in
 * a string, i.e., the characters do not have to be 7-bit ASCII
 * codes.  Therefore, there are 256 entries rather than 128-bit
 * entries.
 *
 * The word size on mips platforms assumes a default integer is
 * always 32 bits.  The mips platform calls the 64-bit integer
 * entry point.  
 *
 *  WORDS  = number of the words needed to hold a flag for each
 *           of 256 different characters.
 *  BITS = number of bits in an integer 'word'.
 *  MASK = number of octal bits to use from the character.
 *  SHIFT = number of bits to shift the character to the right.
 */

#if defined(__mips)
#define WORDS           8
#define BITS            31
#define MASK            07
#define SHIFT           3
#elif !defined(_WORD32)
#define	WORDS		4
#define	BITS		63
#define MASK		03
#define SHIFT		2
#else				/* this includes little endian */
#define	WORDS		8
#define	BITS		31
#define MASK		07
#define SHIFT		3
#endif

#define	FOUND(a)	(mask[a & MASK] << (a >> SHIFT)) < 0

_f_int
_SCAN (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	long	len1, len2;
	int	bck, num;
	_f_int	i;
	_f_int	mask[WORDS];

/*	Determine direction to scan	*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num == 2 * sizeof(_fcd) / sizeof(long))
#else
	if (num == 2)
#endif
	    bck = 0;
	else if (back == NULL)
	    bck = 0;
	else if (_ltob(back))
	    bck = 1;
	else
	    bck = 0;
#endif

/*	Convert Fortran character descriptor to C pointers.	*/

	len1 = _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return 0	*/

	if (len1 == 0 || len2 == 0)
	    return (0);

/*
 *	Initialize bit map to all 0's.  Then set bit for each character in
 *	search string.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string, and return index of first element in string
 *	which is also in search string.
 */
	if (!bck) {
	    for (i = 0; i < len1; i++)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}
	else {
	    for (i = len1-1; i >= 0; i--)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}

/*	If no characters in search string are found, return 0	*/

	return (0);
}


#ifdef	_F_INT4

#undef	WORDS
#undef	BITS
#undef	MASK
#undef	SHIFT

#if defined(__mips)
#define	WORDS		8
#define	BITS		31
#define MASK		07
#define SHIFT		3
#elif !defined(_WORD32)
#define	WORDS		4
#define	BITS		63
#define MASK		03
#define SHIFT		2
#else					/* includes little endian */
#define	WORDS		8
#define	BITS		31
#define MASK		07
#define SHIFT		3
#endif

_f_int4
_SCAN_4 (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	long	len1, len2;
	int	bck, num;
#ifdef	_CRAYMPP
	_f_int	i;
	_f_int	mask[WORDS];
#else
	_f_int4	i;
	_f_int4	mask[WORDS];
#endif

/*	Determine direction to scan	*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num == 2 * sizeof(_fcd) / sizeof(long))
#else
	if (num == 2)
#endif
	    bck = 0;
	else if (back == NULL)
	    bck = 0;
	else if (_ltob(back))
	    bck = 1;
	else
	    bck = 0;
#endif

/*	Convert Fortran character descriptor to C pointers.	*/

	len1 = _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return 0	*/

	if (len1 == 0 || len2 == 0)
	    return (0);

/*
 *	Initialize bit map to all 0's.  Then set bit for each character in
 *	search string.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string, and return index of first element in string
 *	which is also in search string.
 */
	if (!bck) {
	    for (i = 0; i < len1; i++)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}
	else {
	    for (i = len1-1; i >= 0; i--)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}

/*	If no characters in search string are found, return 0	*/

	return (0);
}
#endif


#ifdef	_F_INT8

#undef	WORDS
#undef	BITS
#undef	MASK
#undef	SHIFT
#define	WORDS		8
#define	BITS		63
#define MASK		03
#define SHIFT		2

_f_int8
_SCAN_8 (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	long	len1, len2;
	int	bck, num;
	_f_int8	i;
	_f_int8	mask[WORDS];

/*	Determine direction to scan	*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num == 2 * sizeof(_fcd) / sizeof(long))
#else
	if (num == 2)
#endif
	    bck = 0;
	else if (back == NULL)
	    bck = 0;
	else if (_ltob(back))
	    bck = 1;
	else
	    bck = 0;
#endif

/*	Convert Fortran character descriptor to C pointers.	*/

	len1 = _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return 0	*/

	if (len1 == 0 || len2 == 0)
	    return (0);

/*
 *	Initialize bit map to all 0's.  Then set bit for each character in
 *	search string.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string, and return index of first element in string
 *	which is also in search string.
 */
	if (!bck) {
	    for (i = 0; i < len1; i++)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}
	else {
	    for (i = len1-1; i >= 0; i--)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}

/*	If no characters in search string are found, return 0	*/

	return (0);
}
#endif

#ifdef	_F_INT2

#undef	WORDS
#undef	BITS
#undef	MASK
#undef	SHIFT

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define	WORDS		16
#define	BITS		15
#define MASK		017
#define SHIFT		4
#elif	!defined(_WORD32)
#define	WORDS		4
#define	BITS		63
#define MASK		03
#define SHIFT		2
#else
#define	WORDS		8
#define	BITS		31
#define MASK		07
#define SHIFT		3
#endif

_f_int2
_SCAN_2 (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	long	len1, len2;
	int	bck, num;
	_f_int2	i;
	_f_int2	mask[WORDS];

/*	Determine direction to scan	*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num == 2 * sizeof(_fcd) / sizeof(long))
#else
	if (num == 2)
#endif
	    bck = 0;
	else if (back == NULL)
	    bck = 0;
	else if (_ltob(back))
	    bck = 1;
	else
	    bck = 0;
#endif

/*	Convert Fortran character descriptor to C pointers.	*/

	len1 = _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return 0	*/

	if (len1 == 0 || len2 == 0)
	    return (0);

/*
 *	Initialize bit map to all 0's.  Then set bit for each character in
 *	search string.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string, and return index of first element in string
 *	which is also in search string.
 */
	if (!bck) {
	    for (i = 0; i < len1; i++)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}
	else {
	    for (i = len1-1; i >= 0; i--)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}

/*	If no characters in search string are found, return 0	*/

	return (0);
}
#endif	/* _F_INT2 */

#ifdef	_F_INT1

#undef	WORDS
#undef	BITS
#undef	MASK
#undef	SHIFT

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define	WORDS		32
#define	BITS		7
#define MASK		037
#define SHIFT		5
#elif	!defined(_WORD32)
#define	WORDS		4
#define	BITS		63
#define MASK		03
#define SHIFT		2
#else
#define	WORDS		8
#define	BITS		31
#define MASK		07
#define SHIFT		3
#endif

_f_int1
_SCAN_1 (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	long	len1, len2;
	int	bck, num;
	_f_int1	i;
	_f_int1	mask[WORDS];

/*	Determine direction to scan	*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num == 2 * sizeof(_fcd) / sizeof(long))
#else
	if (num == 2)
#endif
	    bck = 0;
	else if (back == NULL)
	    bck = 0;
	else if (_ltob(back))
	    bck = 1;
	else
	    bck = 0;
#endif

/*	Convert Fortran character descriptor to C pointers.	*/

	len1 = (_f_int1) _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = (_f_int1) _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return 0	*/

	if (len1 == 0 || len2 == 0)
	    return (0);

/*
 *	Initialize bit map to all 0's.  Then set bit for each character in
 *	search string.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string, and return index of first element in string
 *	which is also in search string.
 */
	if (!bck) {
	    for (i = 0; i < len1; i++)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}
	else {
	    for (i = len1-1; i >= 0; i--)
		if (FOUND(ptr1[i]))
		    return (i+1);
	}

/*	If no characters in search string are found, return 0	*/

	return (0);
}
#endif	/* _F_INT1 */
