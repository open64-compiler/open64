/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libfi/mathlb/random.c	92.2	07/09/99 11:00:36"


#include <fortran.h>
#include <stddef.h>
#include <string.h>

#if  !(defined( __mips) || defined(__ia64) || defined(__ia32) || defined(KEY))

#if defined(_SOLARIS)
int _rand_r(unsigned int *seed);
#else
/* FIX: Absoft implementation not multi-thread safe */
#include <stdlib.h>
#endif

#define DEFAULTSEED 01274321477

/*
 * rand_r returns spseudo-random numbers in the range from 0 to (2^15)-1.
 * Use (rand_r(_ranfi) + 1) in the numerator and 32769 in the denominator
 * to get 0 < y < 1 result
 * 32769 = ((2^15)-1) + 2
 */

/*
 * _RANF  - expects a null argument that is ignored.  It always returns
 *          the result as a function result.
 */
_f_real8
_RANF(_f_real8 x)
{
	extern unsigned  int _RANFI;
#if defined(_SOLARIS)
	return( ((_f_real8) _rand_r(&_RANFI) + 1) / (_f_real8) 32769);
#else
/* [JAK032796] per AMH use RAND_MAX */
	return( ((_f_real8) rand() + 1) / (_f_real8) (RAND_MAX) );
#endif
}

/*
 * _RANSET  - pass by value for compiler calls for RANDOM_NUMBER and
 *            RANDOM_SEED and for ia=ranset(iarg)
 */
_RANSET(long x)
{
	extern unsigned int 	_RANFI;
	if (x != 0)
		_RANFI = x;
	else
		_RANFI =  DEFAULTSEED;  /* default seed */
#if !defined(_SOLARIS)
	srand(_RANFI);
#endif
	return((long) _RANFI);
}

/*
 * _RANSET_ADR  - pass by address for compiler calls for  ia=ranset(iarg)
 *            where iarg may not be present.
 */
_RANSET_ADR(long *x)
{
	extern unsigned int 	_RANFI;
	if (x != NULL)
		_RANFI = *x;
	else
		_RANFI =  DEFAULTSEED;  /* default seed */
#if !defined(_SOLARIS)
	srand(_RANFI);
#endif
	return((long) _RANFI);
}

#include <stdio.h>

/*
 * ranget_  - pass by address - CALL RANGET() is legal.  The argument is
 *            optional and must be a NULL argument if not present.
 */

__attribute__ ((weak)) ranget_(long *x)
{
	extern unsigned int _RANFI;
	if (x != NULL)
		*x = _RANFI;
	return((long) _RANFI);
}

/*
 * _RANGET  - pass by address - ia=ranget([iarg]) and for f90 compiler calls
 *            for RANDOM_NUMBER and RANDOM_SEED.
 *            a placeholder.
 */

_RANGET(long *x)
{
	extern unsigned int _RANFI;
	if (x != NULL)
		*x = _RANFI;
	return((long) _RANFI);
}

#else		/* NOT __mips */

#define TWOMINUS53 1.11022302462515654E-16
#define TWOMINUS52 2.22044604925031308E-16
#define TWOMINUS24 5.960464478E-8

#if !defined(KEY)
#undef  USE_RANDOM
#else
#define USE_RANDOM
#endif

#ifndef USE_RANDOM  /* Use a linear congruential generator of period 2^62 */

static unsigned long long randstate=01274321477413155LL;

void _RANGET(char *seed)
{
	memcpy(seed,&randstate,sizeof(randstate));
}

void _RANSET(char *seed)
{
	memcpy(&randstate,seed,sizeof(randstate));

	/* Make sure the seed is odd */
	randstate |= 1;
}


#define multiplier -9165615817502304611LL


double _RANF_8(void) 
{
	double	d;

	/* Use a LC RNG to get 52 bits of randomness since 53 causes a
	 * performance hit.
	 */
	randstate	= (multiplier * randstate) & 0xffffffffffffffffLL;
	d	= TWOMINUS52 * (double) ((long long) (randstate >> 12));
	return (d);
}

float _RANF_4(void) 
{
	float	f;

	/* Use a LC RNG to get 24 bits of randomness */
	randstate = (multiplier * randstate) & 0xffffffffffffffffLL;
	f = (float) TWOMINUS24 * (float) ((long long) (randstate >> 40));
	return (f);
}

/*
 * ranget_  - pass by address - CALL RANGET() is legal.
 */

__attribute__ ((weak)) _f_int8 ranget_(_f_int8 *x)
{
        if (x != NULL)
                *x	= randstate;
        return(randstate);
}

/*
 * ranset_  - pass by address - CALL RANSET() is legal.
 */

__attribute__ ((weak)) _f_int8 ranset_(_f_int8 *x)
{
        if (x != NULL)
                randstate	= *x;;
	/* Make sure the seed is odd */
	randstate |= 1;
        return(randstate);
}

#else	/* NOT USE_RANDOM */

/* This implements the random number generator on the MIPS platforms.
 * It has been cribbed from random().
 * This is the not a linear congruential random number generator.  Keep
 * this in case it is needed in the future.
 */


#define SEED_WORDS 31
#define	SEP 3

static int front=0,rear=SEP;
#ifdef _LITTLE_ENDIAN
static struct {
	int	index;
	int	randtbl[SEED_WORDS];
} randstate = { 0, {  
        0x90399a31, 0xc02432d9, 0x31829b66, 0xf3425da1,
        0x81e0de3b, 0x6fb5df0a, 0xbc02f103, 0x40fb48f3,
        0xe56b7449, 0xdbb0beb1, 0x5918ab5c, 0x54fd9465,
        0x680f8c2e, 0x799feb3d, 0xe0b7b11e, 0x6b862d43,
        0x2e2ada67, 0xca881588, 0x735de369, 0x35f7904f,
        0x8fd6d715, 0xf0516fa6, 0x6b96616e, 0xefdcac94,
        0x3f933641, 0xc298c622, 0x2ab8f5a4, 0xd77b8a88,
        0x9d0ef5ad, 0x220b8999, 0x47b927fb }
};
#else
static struct {
	int	index;
	int	randtbl[SEED_WORDS];
} randstate = { 0, {  
	0x9a319039, 0x32d9c024, 0x9b663182, 0x5da1f342, 
	0xde3b81e0, 0xdf0a6fb5, 0xf103bc02, 0x48f340fb, 
	0x7449e56b, 0xbeb1dbb0, 0xab5c5918, 0x946554fd, 
	0x8c2e680f, 0xeb3d799f, 0xb11ee0b7, 0x2d436b86, 
	0xda672e2a, 0x1588ca88, 0xe369735d, 0x904f35f7, 
	0xd7158fd6, 0x6fa6f051, 0x616e6b96, 0xac94efdc, 
	0x36413f93, 0xc622c298, 0xf5a42ab8, 0x8a88d77b, 
	0xf5ad9d0e, 0x8999220b, 0x27fb47b9 }
};
#endif

void _RANGET(void *seed)
{
	randstate.index	= front;
	memcpy(seed,&randstate,sizeof(randstate));
}

#include "cray/mtlock.h"
static plock_t mut = MEM_LOCK_INIT;

void _RANSET(void *seed)
{
#ifdef KEY /* Bug 5019 */
	MEM_LOCK(&mut);
#endif /* KEY Bug 5019 */
	memcpy(&randstate,seed,sizeof(randstate));
	front	= randstate.index % SEED_WORDS;
	rear	= (front + SEP) % SEED_WORDS;
#ifdef KEY /* Bug 5019 */
	MEM_UNLOCK(&mut);
#endif /* KEY Bug 5019 */
}

double _RANF_8(void) 
{
	double		d;
	long long	i;
#ifdef KEY /* Bug 5019 */
	MEM_LOCK(&mut);
#endif /* KEY Bug 5019 */
   
	randstate.randtbl[front] += randstate.randtbl[rear];

	/* 32 bits */
	i = ((unsigned long long) ((unsigned)randstate.randtbl[front])) << 21;
	front	= (front + 1) % SEED_WORDS;
	rear	= (rear + 1) % SEED_WORDS;
	randstate.randtbl[front] += randstate.randtbl[rear];

	/* 20 bits, not 21 bits because of performance and matching the
	 * the size of the linear congruential method
	 */
	i |= (unsigned long long) ((unsigned)randstate.randtbl[front] >> 12);
	front	= (front + 1) % SEED_WORDS;
	rear	= (rear + 1) % SEED_WORDS;
#ifdef KEY
// Bug 1818
	d	= TWOMINUS53 * (double) i;
#else
	d	= TWOMINUS52 * (double) i;
#endif
#ifdef KEY /* Bug 5019 */
	MEM_UNLOCK(&mut);
#endif /* KEY Bug 5019 */
	return (d);
}

float _RANF_4(void) 
{
#ifdef KEY /* Bug 11627 */
  /* Depending on how you read it, the standard may require all precisions to
   * use same sequence */
  return (float) _RANF_8();
#else /* KEY Bug 11627 */
	int	i;
	float	f;

#ifdef KEY /* Bug 5019 */
	MEM_LOCK(&mut);
#endif /* KEY Bug 5019 */
	randstate.randtbl[front] += randstate.randtbl[rear];

	/* 24 bits */
	i = ((unsigned int) randstate.randtbl[front]) >> 8;
	front	= (front + 1) % SEED_WORDS;
	rear	= (rear + 1) % SEED_WORDS;
	f	= (float) TWOMINUS24 * (float) i;
#ifdef KEY /* Bug 5019 */
	MEM_UNLOCK(&mut);
#endif /* KEY Bug 5019 */
	return (f);
#endif /* KEY Bug 11627 */
}

#endif	/* NOT USE_RANDOM */
#endif	/* NOT __mips */
