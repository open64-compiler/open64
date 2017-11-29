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



#pragma ident "@(#) libf/fio/setstride.c	92.2	06/21/99 10:37:55"

#include <fortran.h>
#include <memory.h>
#include <cray/nassert.h>
#include <cray/portdefs.h>
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
struct eightby		{ int w[2]; };
struct sixteenby	{ int w[4]; };
struct thirtytwoby	{ int w[8]; };
#else
struct biword	{ long w[2]; };

struct quadword { long w[4]; };
#endif
#define	BYTE	unsigned char	

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define EIGHTBWORD	struct eightby
#define DOUBLEWORD	struct sixteenby
#define QUADWORD	struct thirtytwoby
#else
#define	WORD		int64
#ifdef	_CRAYMPP	/* Temporary until `long double' is implemented */
#define	DOUBLEWORD	struct biword
#elif	!defined(_WORD32)
#define	DOUBLEWORD	long double
#else
#define	DOUBLEWORD	long long
#endif

#define	QUADWORD	struct quadword
#endif

#define XFER_CASE(TYPE)				\
{						\
	register TYPE	datum;			\
	TYPE		*data;			\
	datum	= *(TYPE *)src;			\
	data	= (TYPE *)dest;			\
	for (i = 0; i < count; i++)		\
		data[i*inc]	= datum;	\
}						\

/*
 *	_set_stride	Copies an item consisting of a contiguous chunk of
 *			bytes into a strided vector of items.  This is a
 *			generalized version of memcpy() and memwcpy(), with
 *			support for striding and item sizes of greater than
 *			one byte or one word.
 */
void
_set_stride(
	void	*dest,		/* Destination address		*/
	void	*src,		/* Source address		*/
	long	count,		/* Number of items to transfer	*/
	int	elsize,		/* Size of each item, in bytes	*/
	long	stride)		/* Stride between items (bytes)	*/
{
	register int	i;
	register long	inc;	/* Stride between items (in elsize units) */

	/* Assertions */

	assert (dest != NULL);
	assert (src != NULL);
	assert (count > 0);
	assert (elsize > 0);

	inc	= stride / elsize;

	switch (elsize) {

		case 1:
			XFER_CASE(BYTE)		/* does not vectorize */
			break;

#ifdef	_F_INT2
		case 2:
			XFER_CASE(short)	/* Temporary until _f_int2 */
			break;
#endif

#ifdef	_F_INT4
		case 4:
			XFER_CASE(_f_int4)
			break;
#endif

		case 8:
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
			XFER_CASE(EIGHTBWORD)	
#else
			XFER_CASE(_f_int8)	/* vectorizes! */
#endif
			break;

		case 16:
			XFER_CASE(DOUBLEWORD)	/* vectorizes! */
			break;

		case 32:
			XFER_CASE(QUADWORD)	/* should vectorize with CC 4.0 (?) */
			break;

		default:
			for (i = 0; i < count; i++)
#ifdef KEY /* Bug 14261 */
				/* Other cases implicitly multiply by elsize
				 * due to C array-indexing or pointer-addition
				 * semantics, but here we treat "dest" as a
				 * (char *), so we must multiply explicitly */
				(void) memcpy((char *)dest + i * inc * elsize,
				   src, elsize);
#else /* KEY Bug 14261 */
				(void) memcpy((char *)dest + i*inc, src, elsize);
#endif /* KEY Bug 14261 */
	} /* switch */

	return;
}
