/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/scatgath.c	92.1	06/18/99 18:41:02"

#include <stdint.h>
#if	defined(__mips)
#include <sgidefs.h>
#endif

#include <memory.h>
#include <cray/nassert.h>

#ifdef _CRAY1
#define GET_PTR(ptr)	( (long) _dshiftl((long) ptr, (long) ptr, 3) )
#else
#define GET_PTR(ptr)	( (unsigned long) ptr )
#endif

#define PTR_ALIGNED(ptr,type)	( ( GET_PTR(ptr) & (sizeof(type) - 1) ) == 0UL)

#define SIZE_ALIGNED(len,type)	( ( len & (sizeof(type) - 1) ) == 0)

/*
 *	These routines handle gather/scatter operations using native data
 *	types, where applicable.  Due to the different sizes of various
 *	data types on different architectures, these routines use the
 *	following notation:
 *
 *		LARGE	8-byte
 *		MEDIUM	4-byte
 *		SMALL	2-byte
 *
 *
 *	Sizes (in bytes) of various types on different architectures:
 *
 *                        short    int     long  long long
 *	=========        ======= ======= ======= =========
 *	Solaris             2       4       4        8
 *	Mips -n32           2       4       4        8
 *	Mips -64            2       4       8        8
 *	CRAY MPP            4       8       8        8
 *	CRAY PVP            8       8       8        8
 *
 *	Note that the CRAY architectures do not support all sizes.
 */

typedef int64_t LARGE;
typedef int32_t MEDIUM;
typedef int16_t SHORT;

/*
 *	_gather_data(buf, items, inc, len, ptr)
 *
 *	Gathers data items into a local buffer when there is a non-unit
 *	stride.
 *
 *	buf	Local target buffer for gathered data
 *	items	Number of items to be gathered
 *	inc	Increment in bytes between the start of the items to be 
 *		gathered (inc must be a multiple of len).
 *	len	Length of each item in bytes
 *	ptr	User array to be gathered
 */

void
_gather_data(
	void	*buf,
	long	items,
	long	inc,
	int	len,
	void	*ptr)
{
	register long	i, j, k;
	register long	step;

	assert ( inc % len == 0 );  /* assumption simplifies alignment check */

	j	= 0;
	k	= 0;

/*
 *	Process items which are 8, 16 or 32 bytes long and which are
 *	aligned on an 8-byte boundary.
 */

	if ( SIZE_ALIGNED(len, LARGE) && PTR_ALIGNED(ptr, LARGE) ) {
		LARGE	*lp, *lb;

		lp	= (LARGE *) ptr;
		lb	= (LARGE *) buf;
		step	= inc >> 3;

		assert ( sizeof(LARGE) == 8 );

		if (len == sizeof(LARGE)) {	/* If LARGE items */
#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
			for (i = 0; i < items; i++) {
				lb[i]	= lp[k];
				k	= k + step;
			}
		}
		else if (len == (sizeof(LARGE) << 1)) {	/* If 2X LARGE items */
#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
			for (i = 0; i < items; i++) {
				lb[j]   = lp[k];
				lb[j+1] = lp[k+1];
				j	= j + 2;
				k	= k + step;
			}
		}
		else if (len == (sizeof(LARGE) << 2)) {	/* If 4X LARGE items */
#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
			for (i = 0 ; i < items; i++) {
				lb[j]   = lp[k];
				lb[j+1] = lp[k+1];
				lb[j+2] = lp[k+2];
				lb[j+3] = lp[k+3];
				j	= j + 4;
				k	= k + step;
			}
		}
		else
			goto general_case;
	}

/*
 *	Process items which are 4 or 8 bytes long and which are aligned
 *	on a 4-byte boundary.
 */

#ifdef	MEDIUM
	else if ( SIZE_ALIGNED(len, MEDIUM) && PTR_ALIGNED(ptr, MEDIUM) ) {
		MEDIUM	*ip, *ib;

		ip	= (MEDIUM *) ptr;
		ib	= (MEDIUM *) buf;
		step	= inc >> 2;

		assert ( sizeof(MEDIUM) == 4 );

		if (len == sizeof(MEDIUM)) {	/* If MEDIUM items */
			for (i = 0; i < items; i++) {
				ib[i]	= ip[k];
				k	= k + step;
			}
		}
		else if (len == (sizeof(MEDIUM) << 1)) { /* If 2X MEDIUM items */
			for (i = 0; i < items; i++) {
				ib[j]   = ip[k];
				ib[j+1] = ip[k+1];
				j	= j + 2;
				k	= k + step;
			}
		}
		else
			goto general_case;
	}
#endif	/* MEDIUM */

/*
 *	Process items which are 2 or 4 bytes long and which are aligned
 *	on a 2-byte boundary.
 */

#ifdef	SMALL
	else if ( SIZE_ALIGNED(len, SMALL) && PTR_ALIGNED(ptr, SMALL) ) {
		SMALL	*sp, *sb;

		sp	= (SMALL *) ptr;
		sb	= (SMALL *) buf;
		step	= inc >> 1;

		assert ( sizeof(SMALL) == 2 );

		if (len == sizeof(SMALL)) {	/* If SMALL items */
			for (i = 0; i < items; i++) {
				sb[i]	= sp[k];
				k	= k + step;
			}
		}
		else if (len == (sizeof(SMALL) << 1)) {	/* If 2X SMALL items */
			for (i = 0; i < items; i++) {
				sb[j]   = sp[k];
				sb[j+1] = sp[k+1];
				j	= j + 2;
				k	= k + step;
			}
		}
		else
			goto general_case;
	}
#endif	/* SMALL */

/*
 *	Process single byte items or anything else which is not naturally
 *	aligned.
 */

	else {
		char	*cp, *cb;
general_case:
		cp	= (char *) ptr;
		cb	= (char *) buf;

		/* Don't call memcpy() for single byte transfers */

		if (len == 1)
			for (i = 0; i < items; i++) {
				*cb++	= *cp;
				cp	= cp + inc;
			}
		else
			for (i = 0; i < items; i++) {
				(void) memcpy(cb, cp, len);
				cb	= cb + len;
				cp	= cp + inc;
			}
	}

	return;
}

/*
 *	_scatter_data(ptr, items, inc, len, buf)
 *
 *	Scatters data items from a local buffer to a user array when there
 *	is a non-unit stride.
 *
 *	ptr	User array into which local data will be scattered
 *	items	Number of items to be scattered
 *	inc	Increment in bytes between the start of the items to be 
 *		gathered (inc must be a multiple of len).
 *	len	Length of each item in bytes
 *	buf	Local buffer containing packed data
 */
void
_scatter_data (
	void	*ptr,
	long	items,
	long	inc,
	int	len,
	void	*buf)
{
	register long	i, j, k;
	register long	step;

	assert ( inc % len == 0 ); /* assumption simplifies alignment check */

	j	= 0;
	k	= 0;

/*
 *	Process items which are 8, 16 or 32 bytes long and which are
 *	aligned on an 8-byte boundary.
 */

	if ( SIZE_ALIGNED(len, LARGE) && PTR_ALIGNED(ptr, LARGE) ) {
		LARGE	*lp, *lb;

		lp	= (LARGE *) ptr;
		lb	= (LARGE *) buf;
		step	= inc >> 3;

		assert ( sizeof(LARGE) == 8 );

		if (len == sizeof(LARGE)) {	/* If LARGE items */
#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
			for (i = 0; i < items; i++) {
				lp[k]	= lb[i];
				k	= k + step;
			}
		}
		else if (len == (sizeof(LARGE) << 1)) {	/* If 2X LARGE items */
#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
			for (i = 0; i < items; i++) {
				lp[k]   = lb[j];
				lp[k+1] = lb[j+1];
				j	= j + 2;
				k	= k + step;
			}
		}
		else if (len == (sizeof(LARGE) << 2)) {	/* If 4X LARGE items */
#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
			for (i = 0 ; i < items; i++) {
				lp[k]   = lb[j];
				lp[k+1] = lb[j+1];
				lp[k+2] = lb[j+2];
				lp[k+3] = lb[j+3];
				j	= j + 4;
				k	= k + step;
			}
		}
		else
			goto general_case;
	}

/*
 *	Process items which are 4 or 8 bytes long and which are aligned
 *	on a 4-byte boundary.
 */

#ifdef	MEDIUM
	else if ( SIZE_ALIGNED(len, MEDIUM) && PTR_ALIGNED(ptr, MEDIUM) ) {
		MEDIUM	*ip, *ib;

		ip	= (MEDIUM *) ptr;
		ib	= (MEDIUM *) buf;
		step	= inc >> 2;

		assert ( sizeof(MEDIUM) == 4 );

		if (len == sizeof(MEDIUM)) {	/* If MEDIUM items */
			for (i = 0; i < items; i++) {
				ip[k]	= ib[i];
				k	= k + step;
			}
		}
		else if (len == (sizeof(MEDIUM) << 1)) { /* If 2X MEDIUM items */
			for (i = 0; i < items; i++) {
				ip[k]   = ib[j];
				ip[k+1] = ib[j+1];
				j	= j + 2;
				k	= k + step;
			}
		}
		else
			goto general_case;
	}
#endif	/* MEDIUM */

/*
 *	Process items which are 2 or 4 bytes long and which are aligned
 *	on a 2-byte boundary.
 */

#ifdef	SMALL
	else if ( SIZE_ALIGNED(len, SMALL) && PTR_ALIGNED(ptr, SMALL) ) {
		SMALL	*sp, *sb;

		sp	= (SMALL *) ptr;
		sb	= (SMALL *) buf;
		step	= inc >> 1;

		assert ( sizeof(SMALL) == 2 );

		if (len == sizeof(SMALL)) {	/* If SMALL items */
			for (i = 0; i < items; i++) {
				sp[k]	= sb[i];
				k	= k + step;
			}
		}
		else if (len == (sizeof(SMALL) << 1)) {	/* If 2X SMALL items */
			for (i = 0; i < items; i++) {
				sp[k]   = sb[j];
				sp[k+1] = sb[j+1];
				j	= j + 2;
				k	= k + step;
			}
		}
		else
			goto general_case;
	}
#endif	/* SMALL */

/*
 *	Process single byte items or anything else which is not naturally
 *	aligned.
 */

	else {
		char	*cp, *cb;
general_case:
		cp	= (char *) ptr;
		cb	= (char *) buf;

		/* Don't call memcpy() for single byte transfers */

		if (len == 1)
			for (i = 0; i < items; i++) {
				*cp	= *cb++;
				cp	= cp + inc;
			}
		else
			for (i = 0; i < items; i++) {
				(void) memcpy(cp, cb, len);
				cb	= cb + len;
				cp	= cp + inc;
			}
	}

	return;
}
