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

	/* intrinsic QCOSD */

	/* by address version only */

typedef union
{
	struct
	{
		unsigned int hi;
		unsigned int lo;
	} word;

	double	d;
} du;

typedef union
{
	struct
	{
		uint32_t hi[2];
		uint32_t lo[2];
	} word;

	long double	ld;
} ldu;

#define	ROUND(d)	(int)(((d) >= 0.0) ? ((d) + 0.5) : ((d) - 0.5))

static const	ldu	radspdeg =
{0x3f91df46,	0xa2529d39,
 0x3c15c1d8,	0xbecdd290};

static const	du	rninety =
{0x3f86c16c,	0x16c16c17};

long double __q_cosd(long double *x)
{
ldquad	u;
int	n;
double	dn;
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

	if ( __qabs(*x) == __libm_inf_ld )
	{
		/* x is +/-Inf; return a quiet NaN */

		*__errnoaddr = EDOM;

		return ( __libm_qnan_ld );
	}

	if ( *x == 0.0L )
		return ( 1.0L );

	/* reduce arg to +/- 360 degrees */

	u.ld = __qmod(u.ld, 360.0L);

	/* next, reduce to +/-45.0 */

	if ( __qabs(u.ld) <= 45.0L )
	{
		n = 0;
	}
	else
	{
		dn = u.q.hi*rninety.d;
		n = ROUND(dn);
		dn = n;

		u.ld = u.ld - dn*90.0L;
	}

	/* convert x to radians */

	u.ld *= radspdeg.ld;

	if ( n&1)
	{
		if ( n&2 )
		{
			/*
			 *  n%4 = 3
			 *  result is sin(u)
			 */

			result = __qsin(u.ld);
		}
		else
		{
			/*
			 *  n%4 = 1
			 *  result is -sin(u)
			 */

			result = -__qsin(u.ld);
		}

		return ( result );
	}

	if ( n&2 )
	{
		/*
		 *  n%4 = 2
		 *  result is -cos(u)
		 */

		result = -__qcos(u.ld);
	}
	else
	{
		/*
		 *  n%4 = 0
		 *  result is cos(u)
		 */

		result = __qcos(u.ld);
	}

	return( result );
}

