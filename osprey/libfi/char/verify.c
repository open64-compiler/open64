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


#pragma ident "@(#) libfi/char/verify.c	92.2	07/30/99 10:09:59"

/*
 *	VERIFY - Verify that a set of characters contains all the
 *		 characters in a string by identifying the position
 *		 of the first character in a string of characters that
 *		 does not appear in a given set of characters.
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
 * always 32 bits.
 */

#if defined(__mips)
#define	WORDS		8
#define BITS		31
#define	MASK		07
#define SHIFT		3
#elif !defined(_WORD32)
#define	WORDS		4
#define BITS		63
#define	MASK		03
#define SHIFT		2
#else					/* includes little endian */
#define	WORDS		8
#define BITS		31
#define	MASK		07
#define SHIFT		3
#endif

#define	FOUND(a)	(mask[a & MASK] << (a >> SHIFT)) >= 0

_f_int
_VERIFY (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	_f_int	len1, len2;
	int	num;
	_f_int	bck;
	_f_int	i;
	_f_int	mask[WORDS];

/*	Determine direction of search		*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num  == 2 * sizeof(_fcd) / sizeof(long))
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

/*	Convert Fortran character descriptors to C pointers	*/
	len1 = (_f_int) _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = (_f_int) _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return	*/

	if (len1 == 0)
	    return (0);
	if (len2 == 0) {
	    if (bck == 1)
		return (len1);
	    else
		return (1);
	}

/*	Initialize mask and set bit for each character in search string	*/

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string.  When a character which is not in the
 *	search string is found, return that index.
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

/*	If all characters are part of search string, return 0	*/

	return (0);
}


#ifdef	_F_INT4

#undef	WORDS
#undef	BITS
#undef	MASK
#undef	SHIFT
#if defined(__mips)
#define	WORDS		8
#define BITS		31
#define	MASK		07
#define SHIFT		3
#elif !defined(_WORD32)
#define	WORDS		4
#define BITS		63
#define	MASK		03
#define SHIFT		2
#else					/* includes little endian */
#define	WORDS		8
#define BITS		31
#define	MASK		07
#define SHIFT		3
#endif

_f_int4
_VERIFY_4 (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	_f_int4	len1, len2;
	int	num;
	_f_int	bck;
	_f_int4	i;
	long	mask[WORDS];

/*	Determine direction of search		*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num  == 2 * sizeof(_fcd) / sizeof(long))
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

/*	Convert Fortran character descriptors to C pointers	*/
	len1 = (_f_int4) _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = (_f_int4) _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return	*/

	if (len1 == 0)
	    return (0);
	if (len2 == 0) {
	    if (bck == 1)
		return (len1);
	    else
		return (1);
	}

/*	Initialize mask and set bit for each character in search string	*/

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string.  When a character which is not in the
 *	search string is found, return that index.
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

/*	If all characters are part of search string, return 0	*/

	return (0);
}
#endif


#ifdef	_F_INT8

#undef	WORDS
#undef	BITS
#undef	MASK
#undef	SHIFT
#define	WORDS		4
#define BITS		63
#define	MASK		03
#define SHIFT		2

_f_int8
_VERIFY_8 (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	_f_int8	len1, len2;
	int	num;
	_f_int	bck;
	_f_int8	i;
	long	mask[WORDS];

/*	Determine direction of search		*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num  == 2 * sizeof(_fcd) / sizeof(long))
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

/*	Convert Fortran character descriptors to C pointers	*/
	len1 = (_f_int8) _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = (_f_int8)_fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return	*/

	if (len1 == 0)
	    return (0);
	if (len2 == 0) {
	    if (bck == 1)
		return (len1);
	    else
		return (1);
	}

/*	Initialize mask and set bit for each character in search string	*/

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string.  When a character which is not in the
 *	search string is found, return that index.
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

/*	If all characters are part of search string, return 0	*/

	return (0);
}
#endif


#ifdef	_F_INT2

#undef	WORDS
#undef	BITS
#undef	MASK
#undef	SHIFT
#if defined(__mips) || defined(_LITTLE_ENDIAN)
#define	WORDS		16
#define BITS		15
#define	MASK		017
#define SHIFT		4
#elif !defined(_WORD32)
#define	WORDS		4
#define BITS		63
#define	MASK		03
#define SHIFT		2
#else
#define	WORDS		8
#define BITS		31
#define	MASK		07
#define SHIFT		3
#endif

_f_int2
_VERIFY_2 (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	_f_int2	len1, len2;
	int	num;
	_f_int	bck;
	_f_int2	i;
	long	mask[WORDS];

/*	Determine direction of search		*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num  == 2 * sizeof(_fcd) / sizeof(long))
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

/*	Convert Fortran character descriptors to C pointers	*/
	len1 = (_f_int2) _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = (_f_int2) _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return	*/

	if (len1 == 0)
	    return (0);
	if (len2 == 0) {
	    if (bck == 1)
		return (len1);
	    else
		return (1);
	}

/*	Initialize mask and set bit for each character in search string	*/

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string.  When a character which is not in the
 *	search string is found, return that index.
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

/*	If all characters are part of search string, return 0	*/

	return (0);
}
#endif

#ifdef	_F_INT1

#undef	WORDS
#undef	BITS
#undef	MASK
#undef	SHIFT
#if defined(__mips) || defined(_LITTLE_ENDIAN)
#define	WORDS		32
#define BITS		7
#define	MASK		037
#define SHIFT		5
#elif !defined(_WORD32)
#define	WORDS		4
#define BITS		63
#define	MASK		03
#define SHIFT		2
#else
#define	WORDS		8
#define BITS		31
#define	MASK		07
#define SHIFT		3
#endif

_f_int1
_VERIFY_1 (_fcd str1, _fcd str2, _f_log *back)
{
	char	*ptr1, *ptr2;
	_f_int1	len1, len2;
	int	num;
	_f_int	bck;
	_f_int1	i;
	long	mask[WORDS];

/*	Determine direction of search		*/

#ifndef	_UNICOS
	if (back != NULL && _ltob(back))
	    bck = 1;
	else
	    bck = 0;
#else
	num = _numargs();
#ifdef _ADDR64
	if (num  == 2 * sizeof(_fcd) / sizeof(long))
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

/*	Convert Fortran character descriptors to C pointers	*/
	len1 = (_f_int1) _fcdlen (str1);
	ptr1 = _fcdtocp (str1);

	len2 = (_f_int1) _fcdlen (str2);
	ptr2 = _fcdtocp (str2);

/*	If either string is null, return	*/

	if (len1 == 0)
	    return (0);
	if (len2 == 0) {
	    if (bck == 1)
		return (len1);
	    else
		return (1);
	}

/*	Initialize mask and set bit for each character in search string	*/

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	for (i = 0; i < WORDS; i++)
	    mask[i] = 0;
	for (i = 0; i < len2; i++) 
	    mask[ptr2[i] & MASK] |= 1 << (BITS - (ptr2[i] >> SHIFT));

/*
 *	Step through string.  When a character which is not in the
 *	search string is found, return that index.
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

/*	If all characters are part of search string, return 0	*/

	return (0);
}
#endif
