/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: libm.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/libm.h,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:  various typedefs, pragmas, and externs for libm functions
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef libm_INCLUDED
#define libm_INCLUDED

#include <errno.h>

#if defined(mips) && !(defined(__linux__) || defined(BUILD_OS_DARWIN))
#include <sgidefs.h>
#include <svr4_math.h>

#ifndef __MATH_H__

extern	float	fabsf(float);

extern	double	fabs(double);

extern float    sqrtf(float);
extern	double	sqrt(double);

#pragma intrinsic (fabsf)

#pragma intrinsic (fabs)

#if _MIPS_ISA != _MIPS_ISA_MIPS1
#pragma intrinsic (sqrtf)
#pragma intrinsic (sqrt)
#endif

#endif

#define	QNANHI	0x7ff10000
#define	QNANLO	0x00000000

#define	QNANF	0x7f810000

extern	const double	__libm_qnan_d;
extern	const float	__libm_qnan_f;
extern	const double	__libm_inf_d;
extern	const float	__libm_inf_f;
extern	const double	__libm_neginf_d;
extern	const float	__libm_neginf_f;

#ifdef _MIPSEL
#       define D(h,l) l,h
#endif
#ifdef _MIPSEB
#       define D(h,l) h,l
#endif

extern	int	*__errnoaddr;

#define SETERRNO(x)     \
        {       \
                *__errnoaddr = x; \
        }

#ifdef _IP_NAN_SETS_ERRNO

#define NAN_SETERRNO(x)     \
        {       \
                *__errnoaddr = x; \
        }
#else

#define NAN_SETERRNO(x)

#endif


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
	unsigned int	i;
	float	f;
} fu;

#define	ISNANF(x)	((x)!=(x))
#define	ISNAND(x)	((x)!=(x))
#define	FLT2LL(x, ix)	ix = *(int *)&x
#define LL2FLT(ix, x)	x = *((float *)&ix + 1)


#define	EXPBIAS 0x7f
#define	EXPMASK	0x807fffff
#define	SIGNMASK	0x7fffffff

#define	EXPWIDTH	8

#define	DEXPBIAS 0x3ff
#define	DEXPWIDTH	11

#if (_MIPS_SIM == _MIPS_SIM_ABI32)

#define	_TABLE_BASED_REDUCTION

#endif

#if (_MIPS_ISA == _MIPS_ISA_MIPS1 || _MIPS_ISA == _MIPS_ISA_MIPS2)

#define	_32BIT_MACHINE

#else

#define	_64BIT_MACHINE

#endif

#if (_MIPS_ISA == _MIPS_ISA_MIPS1 || _MIPS_ISA == _MIPS_ISA_MIPS2)

#define	ROUND(d)	(int)(((d) >= 0.0) ? ((d) + 0.5) : ((d) - 0.5))

#define	ROUNDF(d)	(int)(((d) >= (float)0.0) ? ((d) + (float)0.5) : ((d) - (float)0.5))

#define	INT	int
#define	UINT	unsigned int
#define	DEXPMASK	0x800fffff
#define	DSIGNMASK	0x7fffffff
#define	DMANTWIDTH	20
#define	MANTWIDTH	23

#define	DBLHI2INT(x, n)	n = *(int *)&x
#define	DBLLO2INT(x, n)	n = *((int *)&x + 1)
#define	INT2DBLHI(n, x)	*(int *)&x = n
#define	INT2DBLLO(n, x)	*((int *)&x + 1) = n

#else

#define	ROUND(d)	round(d)
#define	ROUNDF(f)	roundf(f)

int	roundf(float);
#pragma intrinsic (roundf)

int	round(double);
#pragma intrinsic (round)

#define	INT	long long
#define	UINT	unsigned long long
#define	DEXPMASK	0x800fffffffffffffll
#define	DSIGNMASK	0x7fffffffffffffffll
#define	DMANTWIDTH	52
#define	MANTWIDTH	23

#endif

#if (_MIPS_ISA == _MIPS_ISA_MIPS1 || _MIPS_ISA == _MIPS_ISA_MIPS2)

#define	FLT2INT(x, n)	n = *(int *)&x
#define	INT2FLT(n, x)	x = *(float *)&n
#define	DBL2LL(x, l)	l = *(long long *)&x
#define	LL2DBL(l, x)	x = *(double *)&l

#else

#pragma intrinsic (__builtin_cast_f2i);
#pragma intrinsic (__builtin_cast_i2f);

#pragma intrinsic (__builtin_cast_d2ll);
#pragma intrinsic (__builtin_cast_ll2d);

#define	FLT2INT(x, n)	n = __builtin_cast_f2i(x)
#define	INT2FLT(n, x)	x = __builtin_cast_i2f(n)
#define	DBL2LL(x, l)	l = __builtin_cast_d2ll(x)
#define	LL2DBL(l, x)	x = __builtin_cast_ll2d(l)

#endif

#endif /* mips */

#if defined(__linux__) || defined(BUILD_OS_DARWIN)

extern	float	fabsf(float);

extern	double	fabs(double);

extern float    sqrtf(float);
extern	double	sqrt(double);

#define	QNANHI	0xfff80000
#define	QNANLO	0x00000000

#define	QNANF	0xffc00000

extern	const double	__libm_qnan_d;
extern	const float	__libm_qnan_f;
extern	const double	__libm_inf_d;
extern	const float	__libm_inf_f;
extern	const double	__libm_neginf_d;
extern	const float	__libm_neginf_f;

#define D(h,l) l,h

extern	int	errno;

#define SETERRNO(x)     \
        {       \
                errno = x; \
        }

#ifdef _IP_NAN_SETS_ERRNO

#define NAN_SETERRNO(x)     \
        {       \
                errno = x; \
        }
#else

#define NAN_SETERRNO(x)

#endif


typedef union
{
	struct
	{
		unsigned int lo;
		unsigned int hi;
	} word;

	double	d;
} du;

typedef union
{
	unsigned int	i;
	float	f;
} fu;

#define	ISNANF(x)	((x)!=(x))
#define	ISNAND(x)	((x)!=(x))
#define	EXPBIAS 0x7f
#define	EXPMASK	0x807fffff
#define	SIGNMASK	0x7fffffff

#define	EXPWIDTH	8

#define	DBLHI2INT(x, n)	n = *((int *)&x + 1)
#define	DBLLO2INT(x, n)	n = *(int *)&x
#define	INT2DBLHI(n, x)	*((int *)&x + 1) = n
#define	INT2DBLLO(n, x)	*(int *)&x = n

#ifdef MACHINE_IA64

#define	__float80	long double

#define	DEXPBIAS 0x3ff
#define	DEXPWIDTH	11

#define	_64BIT_MACHINE

//Workaround, since these builtin function are not supported by our compiler currently. 
#define	ROUND(d)	(int)(((d) >= 0.0) ? ((d) + 0.5) : ((d) - 0.5))

#define	ROUNDF(d)	(int)((d) >= 0.0f ? ((d) + 0.5f) : ((d) - 0.5f))

#define	FLT2INT(x, n)	n = *(int *)&x

#define	INT2FLT(n, x)	\
{ \
int tmpmac; \
tmpmac = n; \
x = *(float *)&tmpmac; \
}

#define	DBL2LL(x, l)	l = *(long long *)&x

#define	LL2DBL(l, x)	\
{ \
long long tmpmac; \
tmpmac = l; \
x = *(double *)&tmpmac; \
}

#define	INT	long long
#define	UINT	unsigned long long
#define	DEXPMASK	0x800fffffffffffffll
#define	DSIGNMASK	0x7fffffffffffffffll
#define	DMANTWIDTH	52
#define	MANTWIDTH	23

#define	MAXEDEXP	0x1ffff
#define	EDEXPBIAS	0xffff
#define	EDSIGNMASK	0xffff
#define	EDEXPMASK	0x10000
#define	EDEXPWIDTH	17

#define	RFSIGNMASK	0x1ffff
#define	RFEXPBIAS	0xffff
#define	RFEXPWIDTH	17
#define	RFEXPMASK	0x20000ll

#define ISNANED(x)	((x)!=(x))



#define	EDH2LL(x, n) \
	n = *(int *)&x; \
	n >>= 52; \
	n &= 0xfff

#define LL2EDH(n, x) \
	*(long long *)&x &= (DSIGNMASK & DEXPMASK); \
	*(long long *)&x |= (n << 52)

#else

#define	DEXPBIAS 0x3ff
#define	DEXPWIDTH	11

#define	ROUND(d)	(int)(((d) >= 0.0) ? ((d) + 0.5) : ((d) - 0.5))

#define	ROUNDF(d)	(int)((d) >= 0.0f ? ((d) + 0.5f) : ((d) - 0.5f))

#define	FLT2INT(x, n)	n = *(int *)&x

#define	INT2FLT(n, x)	\
{ \
int tmpmac; \
tmpmac = n; \
x = *(float *)&tmpmac; \
}

#define	DBL2LL(x, l)	l = *(long long *)&x

#define	LL2DBL(l, x)	\
{ \
long long tmpmac; \
tmpmac = l; \
x = *(double *)&tmpmac; \
}

#define	_32BIT_MACHINE

#define	INT	int
#define	UINT	unsigned int
#define	DEXPMASK	0x800fffff
#define	DSIGNMASK	0x7fffffff
#define	DMANTWIDTH	20
#define	MANTWIDTH	23


#endif /* _ABI_I64 */
#endif /* __linux__ */

#endif /* libm_INCLUDED */

