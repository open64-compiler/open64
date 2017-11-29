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


#include <inttypes.h>
#include "quad.h"

	/* intrinsic QINT */

	/* by value version */

	/* truncates a long double, i.e. computes the integral part of
	   a long double
	*/

extern	double	__trunc(double);

extern	double	fabs(double);
#pragma intrinsic (fabs)

typedef union
{
	struct
	{
		uint32_t hi;
		uint32_t lo;
	} word;

	double	d;
} du;

static const du		twop52 =
{0x43300000,	0x00000000};

long double
__qint(double  uhi,double  ulo )
{
ldquad	result;

#include "msg.h"

	if ( uhi != uhi )
	{
		result.q.hi = uhi;
		result.q.lo = ulo;

		return ( result.ld );
	}

	if ( uhi >= 0.0 )
	{
		if ( uhi < twop52.d )
		{
			/* binary point occurs in uhi; truncate uhi to an integer
			*/

			result.q.hi = __trunc(uhi);

			result.q.lo = 0.0;

			if ( result.q.hi < uhi )
				return ( result.ld );

			/* must adjust result by one if ulo < 0.0 */

			if ( ulo < 0.0 )
			{
				result.q.hi -= 1.0;

				return ( result.ld );
			}

			return ( result.ld );
		}
		else if ( fabs(ulo) < twop52.d )
		{
			/* binary point occurs in ulo; truncate ulo to an integer
			*/

			result.q.hi = uhi;

			result.q.lo = __trunc(ulo);

			if ( result.q.lo > ulo )
			{
				result.q.lo -= 1.0;
			}

			return ( result.ld );
		}

		/* arg is an integer */

		result.q.hi = uhi;
		result.q.lo = ulo;

		return ( result.ld );
	}
	else
	{
		if ( fabs(uhi) < twop52.d )
		{
			/* binary point occurs in uhi; truncate uhi to an integer
			*/

			result.q.hi = __trunc(uhi);

			result.q.lo = 0.0;

			if ( result.q.hi > uhi )
				return ( result.ld );

			/* must adjust result by one if ulo > 0.0 */

			if ( ulo > 0.0 )
			{
				result.q.hi += 1.0;

				return ( result.ld );
			}

			return ( result.ld );
		}
		else if ( fabs(ulo) < twop52.d )
		{
			/* binary point occurs in ulo; truncate ulo to an integer
			*/

			result.q.hi = uhi;

			result.q.lo = __trunc(ulo);

			if ( result.q.lo < ulo )
			{
				result.q.lo += 1.0;
			}

			return ( result.ld );
		}

		/* arg is an integer */

		result.q.hi = uhi;
		result.q.lo = ulo;

		return ( result.ld );
	}

}

