
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

#include <errno.h>

extern	int	errno;

typedef union
{
	long long lword;
        double  d;
} du;

static const du Quiet_nan = 
{
0x7ff8000000000000ll,
};

extern	double	fabs(double);

#ifdef mips
extern	double	trunc(double);

#pragma weak trunc = __trunc
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __trunc(double);
#pragma weak trunc
double trunc( double arg ) {
  return __trunc( arg );
}
#elif defined(__GNUC__)
extern  double  __trunc(double);
double    trunc() __attribute__ ((weak, alias ("__trunc")));
#endif

#define	DMANTWIDTH	52
#define	DEXPWIDTH	11
#define	DSIGNMASK	0x7fffffffffffffffll
#define	DEXPMASK	0x800fffffffffffffll
#define	DQNANBITMASK	0xfff7ffffffffffffll

#define	MANTWIDTH	23
#define	EXPWIDTH	8
#define	SIGNMASK	0x7fffffff
#define	EXPMASK		0x807fffff
#define	QNANBITMASK	0xffbfffff

double
__trunc( double arg )
{
unsigned long long	ll, exp, mantissa;
int	sign;
int	shift_count;
double	result;


	ll = *(unsigned long long *)&arg;

	exp = (ll >> DMANTWIDTH);
	sign = (exp >> DEXPWIDTH);
	exp &= 0x7ff;
	mantissa = (ll & (DSIGNMASK & DEXPMASK));

	if ( exp == 0x7ff )
	{
		/* arg is an infinity, or a NaN */

		if ( mantissa == 0 )
			return ( arg );
		else 
		{
			errno = EDOM;
			return ( Quiet_nan.d );
		}
	}

	if ( exp >= 0x433 )
		return ( arg );

	if ( fabs(arg) < 1.0 )
		return ( (sign == 0) ? 0.0 : -0.0 );

	shift_count = 0x433 - exp;

	ll >>= shift_count;
	ll <<= shift_count;

	*(long long *)&result = ll;

	return ( result );
}

