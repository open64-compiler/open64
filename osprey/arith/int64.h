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


/* Some macros to implement 64-bit integer operations with arrays of 16-bit
 * unsigned integers.
 */

#if _CRAY
#define	ZERO64(a) ( *((long*)&(a)) = 0 )
#else
#define	ZERO64(a) ( (a).part1 = (a).part2 = (a).part3 = (a).part4 = 0 )
#endif

#if _CRAY
#define	COPY64(to,fr) ( *((long*)&(to)) = *((long*)&(fr)) )
#else
#define	COPY64(to,fr) ( (to).part1 = (fr).part1,			\
			(to).part2 = (fr).part2,			\
			(to).part3 = (fr).part3,			\
			(to).part4 = (fr).part4 )
#endif

#if _CRAY
#define	SHLEFT64(x) ( *((long*)&(x)) = *((long*)&(x))<<1 )
#else
#define	SHLEFT64(x) do {						\
	(x).part1 = ((x).part1 << 1) | ((x).part2 >> 15);		\
	(x).part2 = ((x).part2 << 1) | ((x).part3 >> 15);		\
	(x).part3 = ((x).part3 << 1) | ((x).part4 >> 15);		\
	(x).part4 = (x).part4 << 1;					\
} while (0)
#endif

#if _CRAY
#define	SHLEFT64N(x,n) ( *((long*)&(x)) = *((long*)&(x))<<(n) )
#else
#define	SHLEFT64N(x,n) do {						\
	register int _n = n;						\
	switch(_n>>4){							\
	case 0:								\
		(x).part1 = ((x).part1 << _n) | ((x).part2 >> (16-_n));	\
		(x).part2 = ((x).part2 << _n) | ((x).part3 >> (16-_n));	\
		(x).part3 = ((x).part3 << _n) | ((x).part4 >> (16-_n));	\
		(x).part4 = ((x).part4 << _n);				\
		break;							\
	case 1:								\
		(x).part1 = ((x).part2 << (_n-16)) | ((x).part3 >> (32-_n));\
		(x).part2 = ((x).part3 << (_n-16)) | ((x).part4 >> (32-_n));\
		(x).part3 = ((x).part4 << (_n-16));			\
		(x).part4 = 0;						\
		break;							\
	case 2:								\
		(x).part1 = ((x).part3 << (_n-32)) | ((x).part4 >> (48-_n));\
		(x).part2 = ((x).part4 << (_n-32));			\
		(x).part3 = (x).part4 = 0;				\
		break;							\
	case 3:								\
		(x).part1 = ((x).part4 << (_n-48));			\
		(x).part2 = (x).part3 = (x).part4 = 0;			\
		break;							\
	default:							\
		(x).part1 = (x).part2 = (x).part3 = (x).part4 = 0;	\
	}								\
} while(0)
#endif

#if _CRAY
#define	SHRIGHT64(x) ( *((unsigned long*)&(x)) = *((unsigned long*)&(x))>>1 )
#else
#define SHRIGHT64(x) do {						\
	(x).part4 = ((x).part4 >> 1) | (((x).part3 & 1) << 15);		\
	(x).part3 = ((x).part3 >> 1) | (((x).part2 & 1) << 15);		\
	(x).part2 = ((x).part2 >> 1) | (((x).part1 & 1) << 15);		\
	(x).part1 >>= 1;						\
} while (0)
#endif

#if _CRAY
#define	SHRIGHT64N(x,n) ( *((unsigned long*)&(x)) = *((unsigned long*)&(x))>>(n) )
#else
#define	SHRIGHT64N(x,n) do {						\
	register int _n = n;						\
	switch(_n>>4) {							\
	case 0:								\
		(x).part4 = ((x).part4 >> _n) | ((x).part3 << (16-_n));	\
		(x).part3 = ((x).part3 >> _n) | ((x).part2 << (16-_n));	\
		(x).part2 = ((x).part2 >> _n) | ((x).part1 << (16-_n));	\
		(x).part1 = ((x).part1 >> _n);				\
		break;							\
	case 1:								\
		(x).part4 = ((x).part3 >> (_n-16)) | ((x).part2 << (32-_n));\
		(x).part3 = ((x).part2 >> (_n-16)) | ((x).part1 << (32-_n));\
		(x).part2 = ((x).part1 >> (_n-16));			\
		(x).part1 = 0;						\
		break;							\
	case 2:								\
		(x).part4 = ((x).part2 >> (_n-32)) | ((x).part1 << (48-_n));\
		(x).part3 = ((x).part1 >> (_n-32));			\
		(x).part2 = (x).part1 = 0;				\
		break;							\
	case 3:								\
		(x).part4 = ((x).part1 >> (_n-48));			\
		(x).part3 = (x).part2 = (x).part1 = 0;			\
		break;							\
	default:							\
		(x).part4 = (x).part3 = (x).part2 = (x).part1 = 0;	\
	}								\
} while(0)
#endif

#if _CRAY
#define	SHRIGHT64X(x) ( *((unsigned long*)&(x)) = \
	(*((unsigned long*)&(x))&0x8000000000000000) | (*((unsigned long*)&(x))>>1) )
#else
#define SHRIGHT64X(x) do {						\
	(x).part4 = ((x).part4 >> 1) | (((x).part3 & 1) << 15);		\
	(x).part3 = ((x).part3 >> 1) | (((x).part2 & 1) << 15);		\
	(x).part2 = ((x).part2 >> 1) | (((x).part1 & 1) << 15);		\
	(x).part1 = ((x).part1 >> 1) | ((x).part1 & (1 << 15));		\
} while (0)
#endif

#if _CRAY
#define	SIGNBIT(x)	( *((long*)&(x))>>63 )
#else
#define	SIGNBIT(x)	( (x).part1 >> 15 )
#endif

#if _CRAY
#define	ADD64(sum,a,b) ( *((long*)&(sum)) = *((long*)&(a)) + *((long*)&(b)) )
#else
#define	ADD64(sum,a,b) do {						\
	unsigned long t = (a).part4 + (b).part4;			\
	(sum).part4 = t;						\
	t >>= 16;							\
	(sum).part3 = t += (a).part3 + (b).part3;			\
	t >>= 16;							\
	(sum).part2 = t += (a).part2 + (b).part2;			\
	(sum).part1 = (t >> 16) + (a).part1 + (b).part1;		\
} while (0)
#endif

#if _CRAY
#define	INC64(a) ( (*((long*)&(a)))++ )
#else
#define INC64(a) do {							\
	unsigned long t = (a).part4 + 1;				\
	(a).part4 = t;							\
	(a).part3 = t = (t >> 16) + (a).part3;				\
	(a).part2 = t = (t >> 16) + (a).part2;				\
	(a).part1 += t >> 16;						\
} while (0)
#endif

#if _CRAY
#define	DEC64(a) ( (*((long*)&(a)))-- )
#else
#define DEC64(a) do {							\
	unsigned long t = (a).part4 + MASKR (16);			\
	(a).part4 = t;							\
	(a).part3 = t = (t >> 16) + (a).part3 + MASKR (16);		\
	(a).part2 = t = (t >> 16) + (a).part2 + MASKR (16);		\
	(a).part1 += (t >> 16) + MASKR (16);				\
} while (0)
#endif

#if _CRAY
#define	NEG64(a) ( *((long*)&(a)) = -(*((long*)&(a))) )
#else
#define NEG64(a) do {							\
	NOT64 (a);							\
	INC64 (a);							\
} while (0)
#endif

#define	MULSTEP(sum,bits,val,ct) do {					\
	int i;								\
	for (i = 0; i < (ct); i++) {					\
		SHLEFT64 (bits);					\
		if (SIGNBIT (bits))					\
			ADD64 (sum, sum, val);				\
		SHRIGHT64 (val);					\
	}								\
} while (0)

#if _CRAY
#define	NOT64(a) ( *((long*)&(a)) = ~(*((long*)&(a))) )
#else
#define	NOT64(a) ( (a).part1 ^= MASKR (16),				\
		   (a).part2 ^= MASKR (16),				\
		   (a).part3 ^= MASKR (16),				\
		   (a).part4 ^= MASKR (16) )
#endif


#ifdef _LITTLE_ENDIAN
#define WORD_SWAP(x)    \
  { unsigned long t  ;  \
    t = (x).part1 ;   (x).part1 = (x).part4; (x).part4=t;             \
    t = (x).part2 ;   (x).part2 = (x).part3; (x).part3=t;             \
   }
#else
#define WORD_SWAP(x) 
#endif

/* $Id: int64.h,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
