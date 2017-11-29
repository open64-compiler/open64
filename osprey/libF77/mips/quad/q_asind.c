/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#include <errno.h>
#include <inttypes.h>
#include "quad.h"

	/* intrinsic QASIND */

	/* by address version only */

typedef union
{
	struct
	{
		uint32_t hi[2];
		uint32_t lo[2];
	} word;

	long double	ld;
} ldu;

static const	ldu	degprad =
{0x404ca5dc,	0x1a63c1f8,
 0xbce1e7ab,	0x456405f9};

long double __q_asind(long double *x)
{
ldquad	u;
long double	result;

	u.ld = *x;

	if ( u.q.hi != u.q.hi )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _IP_NAN_SETS_ERRNO

		*__errnoaddr = EDOM;
#endif
		return ( __libm_qnan_ld );
	}

	if ( *x == 0.0L )
		return ( 0.0L );

	if ( __qabs(*x) < 1.0L )
	{
		result = __qasin(*x);

		return ( result*degprad.ld );
	}

	if ( *x == 1.0L )
		return ( 90.0L );

	if ( *x == -1.0L )
		return ( -90.0L );

	*__errnoaddr = EDOM;

	return ( __libm_qnan_ld );
}

