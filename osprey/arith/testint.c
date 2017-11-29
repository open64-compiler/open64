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


#include <stdio.h>
#include "arith.h"
#include "arith.internal.h"

#if !defined __sun && (!defined __STDC__ || !defined _CRAY || _RELEASE < 4)
#error  Must be compiled with gcc or Cray Standard C Compiler, version >= 4.0.
#endif

#if defined _CRAY

    typedef signed int    SINT46;
    typedef unsigned int  UINT46;
    typedef unsigned long UINT64;
    typedef signed long   SINT64;

#   define LS46(v)   (*(SINT46*)v)
#   define LU46(v)   (*(UINT46*)v)
#   define LS64(v)   (*(SINT64*)v)
#   define LU64(v)   (*(UINT64*)v)

#elif defined __sun

    typedef unsigned long long int UINT64;
    typedef signed long long int   SINT64;
    typedef signed long long int   SINT46;

    /*
     * The Sun needs these because data alignment issues prevent us from
     * casting AR_INT_64 addresses directly to SINT64 addresses and then
     * loading the pointed-to doubleword.
     */
    SINT46 LS46(AR_INT_64 *vp)
    {
	return((((SINT46) vp->part1) << 48) |
	       (((SINT46) vp->part2) << 32) |
	       (((SINT46) vp->part3) << 16) |
	       (((SINT46) vp->part4)      ));
    }

    SINT64 LS64(AR_INT_64 *vp)
    {
	return((((SINT64) vp->part1) << 48) |
	       (((SINT64) vp->part2) << 32) |
	       (((SINT64) vp->part3) << 16) |
	       (((SINT64) vp->part4)      ));
    }

    UINT64 LU64(AR_INT_64 *vp)
    {
	return((((UINT64) vp->part1) << 48) |
	       (((UINT64) vp->part2) << 32) |
	       (((UINT64) vp->part3) << 16) |
	       (((UINT64) vp->part4)      ));
    }
#endif


AR_INT_64 cool_ints[] =
{
   /* march a one bit though a positive 64-bit int */
   {0x0000, 0x0000, 0x0000, 0x0000},
   {0x0000, 0x0000, 0x0000, 0x0001},
   {0x0000, 0x0000, 0x0000, 0x0002},
   {0x0000, 0x0000, 0x0000, 0x0004},
   {0x0000, 0x0000, 0x0000, 0x0010},
   {0x0000, 0x0000, 0x0000, 0x0040},
   {0x0000, 0x0000, 0x0000, 0x0100},
   {0x0000, 0x0000, 0x0000, 0x0400},
   {0x0000, 0x0000, 0x0000, 0x1000},
   {0x0000, 0x0000, 0x0000, 0x4000},
   {0x0000, 0x0000, 0x0001, 0x0000},
   {0x0000, 0x0000, 0x0004, 0x0000},
   {0x0000, 0x0000, 0x0010, 0x0000},
   {0x0000, 0x0000, 0x0040, 0x0000},
   {0x0000, 0x0000, 0x0100, 0x0000},
   {0x0000, 0x0000, 0x0400, 0x0000},
   {0x0000, 0x0000, 0x1000, 0x0000},
   {0x0000, 0x0000, 0x4000, 0x0000},
   {0x0000, 0x0001, 0x0000, 0x0000},
   {0x0000, 0x0004, 0x0000, 0x0000},
   {0x0000, 0x0010, 0x0000, 0x0000},
   {0x0000, 0x0040, 0x0000, 0x0000},
   {0x0000, 0x0100, 0x0000, 0x0000},
   {0x0000, 0x0400, 0x0000, 0x0000},
   {0x0000, 0x1000, 0x0000, 0x0000},
   {0x0000, 0x4000, 0x0000, 0x0000},
   {0x0001, 0x0000, 0x0000, 0x0000},
   {0x0004, 0x0000, 0x0000, 0x0000},
   {0x0010, 0x0000, 0x0000, 0x0000},
   {0x0040, 0x0000, 0x0000, 0x0000},
   {0x0100, 0x0000, 0x0000, 0x0000},
   {0x0400, 0x0000, 0x0000, 0x0000},
   {0x1000, 0x0000, 0x0000, 0x0000},
   {0x2000, 0x0000, 0x0000, 0x0000},
   {0x4000, 0x0000, 0x0000, 0x0000},
   {0x8000, 0x0000, 0x0000, 0x0000},

   /* march a one bit though a negative 32/64-bit int */
   {0x8000, 0x0000, 0x8000, 0x0000},
   {0x8000, 0x0000, 0x8000, 0x0001},
   {0x8000, 0x0000, 0x8000, 0x0002},
   {0x8000, 0x0000, 0x8000, 0x0004},
   {0x8000, 0x0000, 0x8000, 0x0010},
   {0x8000, 0x0000, 0x8000, 0x0040},
   {0x8000, 0x0000, 0x8000, 0x0100},
   {0x8000, 0x0000, 0x8000, 0x0400},
   {0x8000, 0x0000, 0x8000, 0x1000},
   {0x8000, 0x0000, 0x8000, 0x4000},
   {0x8000, 0x0000, 0x8001, 0x0000},
   {0x8000, 0x0000, 0x8004, 0x0000},
   {0x8000, 0x0000, 0x8010, 0x0000},
   {0x8000, 0x0000, 0x8040, 0x0000},
   {0x8000, 0x0000, 0x8100, 0x0000},
   {0x8000, 0x0000, 0x8400, 0x0000},
   {0x8000, 0x0000, 0x9000, 0x0000},
   {0x8000, 0x0000, 0xc000, 0x0000},
   {0x8000, 0x0001, 0x0000, 0x0000},
   {0x8000, 0x0004, 0x0000, 0x0000},
   {0x8000, 0x0010, 0x0000, 0x0000},
   {0x8000, 0x0040, 0x0000, 0x0000},
   {0x8000, 0x0100, 0x0000, 0x0000},
   {0x8000, 0x0400, 0x0000, 0x0000},
   {0x8000, 0x1000, 0x0000, 0x0000},
   {0x8000, 0x4000, 0x0000, 0x0000},
   {0x8001, 0x0000, 0x0000, 0x0000},
   {0x8004, 0x0000, 0x0000, 0x0000},
   {0x8010, 0x0000, 0x0000, 0x0000},
   {0x8040, 0x0000, 0x0000, 0x0000},
   {0x8100, 0x0000, 0x0000, 0x0000},
   {0x8400, 0x0000, 0x0000, 0x0000},
   {0x9000, 0x0000, 0x0000, 0x0000},
   {0xa000, 0x0000, 0x0000, 0x0000},
   {0xc000, 0x0000, 0x0000, 0x0000},

   /* shift 64 one bits through a 64-bit positive int */
/* {0x0000, 0x0000, 0x0000, 0x0001}, */  /* already have this one */
   {0x0000, 0x0000, 0x0000, 0x0003},
   {0x0000, 0x0000, 0x0000, 0x0007},
   {0x0000, 0x0000, 0x0000, 0x000f},
   {0x0000, 0x0000, 0x0000, 0x003f},
   {0x0000, 0x0000, 0x0000, 0x00ff},
   {0x0000, 0x0000, 0x0000, 0x03ff},
   {0x0000, 0x0000, 0x0000, 0x0fff},
   {0x0000, 0x0000, 0x0000, 0x3fff},
   {0x0000, 0x0000, 0x0000, 0xffff},
   {0x0000, 0x0000, 0x0003, 0xffff},
   {0x0000, 0x0000, 0x000f, 0xffff},
   {0x0000, 0x0000, 0x003f, 0xffff},
   {0x0000, 0x0000, 0x00ff, 0xffff},
   {0x0000, 0x0000, 0x03ff, 0xffff},
   {0x0000, 0x0000, 0x0fff, 0xffff},
   {0x0000, 0x0000, 0x3fff, 0xffff},
   {0x0000, 0x0000, 0xffff, 0xffff},
   {0x0000, 0x0003, 0xffff, 0xffff},
   {0x0000, 0x000f, 0xffff, 0xffff},
   {0x0000, 0x003f, 0xffff, 0xffff},
   {0x0000, 0x00ff, 0xffff, 0xffff},
   {0x0000, 0x03ff, 0xffff, 0xffff},
   {0x0000, 0x0fff, 0xffff, 0xffff},
   {0x0000, 0x3fff, 0xffff, 0xffff},
   {0x0000, 0xffff, 0xffff, 0xffff},
   {0x0003, 0xffff, 0xffff, 0xffff},
   {0x000f, 0xffff, 0xffff, 0xffff},
   {0x003f, 0xffff, 0xffff, 0xffff},
   {0x00ff, 0xffff, 0xffff, 0xffff},
   {0x03ff, 0xffff, 0xffff, 0xffff},
   {0x0fff, 0xffff, 0xffff, 0xffff},
   {0x3fff, 0xffff, 0xffff, 0xffff},
   {0x7fff, 0xffff, 0xffff, 0xfffe},
   {0x7fff, 0xffff, 0xffff, 0xfff8},
   {0x7fff, 0xffff, 0xffff, 0xffe0},
   {0x7fff, 0xffff, 0xffff, 0xff80},
   {0x7fff, 0xffff, 0xffff, 0xfe00},
   {0x7fff, 0xffff, 0xffff, 0xf800},
   {0x7fff, 0xffff, 0xffff, 0xe000},
   {0x7fff, 0xffff, 0xffff, 0x8000},
   {0x7fff, 0xffff, 0xfffe, 0x0000},
   {0x7fff, 0xffff, 0xfff8, 0x0000},
   {0x7fff, 0xffff, 0xffe0, 0x0000},
   {0x7fff, 0xffff, 0xff80, 0x0000},
   {0x7fff, 0xffff, 0xfe00, 0x0000},
   {0x7fff, 0xffff, 0xf800, 0x0000},
   {0x7fff, 0xffff, 0xe000, 0x0000},
   {0x7fff, 0xffff, 0x8000, 0x0000},
   {0x7fff, 0xfffe, 0x0000, 0x0000},
   {0x7fff, 0xfff8, 0x0000, 0x0000},
   {0x7fff, 0xffe0, 0x0000, 0x0000},
   {0x7fff, 0xff80, 0x0000, 0x0000},
   {0x7fff, 0xfe00, 0x0000, 0x0000},
   {0x7fff, 0xf800, 0x0000, 0x0000},
   {0x7fff, 0xe000, 0x0000, 0x0000},
   {0x7fff, 0x8000, 0x0000, 0x0000},
   {0x7ffe, 0x0000, 0x0000, 0x0000},
   {0x7ff8, 0x0000, 0x0000, 0x0000},
   {0x7fe0, 0x0000, 0x0000, 0x0000},
   {0x7f80, 0x0000, 0x0000, 0x0000},
   {0x7e00, 0x0000, 0x0000, 0x0000},
   {0x7800, 0x0000, 0x0000, 0x0000},
   {0x7000, 0x0000, 0x0000, 0x0000},
   {0x6000, 0x0000, 0x0000, 0x0000},
   {0x4000, 0x0000, 0x0000, 0x0000},

   /* shift 64 one bits through a 64-bit negative int */
   {0x8000, 0x0000, 0x0000, 0x0001},
   {0x8000, 0x0000, 0x0000, 0x0003},
   {0x8000, 0x0000, 0x0000, 0x0007},
   {0x8000, 0x0000, 0x0000, 0x000f},
   {0x8000, 0x0000, 0x0000, 0x001f},
   {0x8000, 0x0000, 0x0000, 0x007f},
   {0x8000, 0x0000, 0x0000, 0x01ff},
   {0x8000, 0x0000, 0x0000, 0x07ff},
   {0x8000, 0x0000, 0x0000, 0x1fff},
   {0x8000, 0x0000, 0x0000, 0x7fff},
   {0x8000, 0x0000, 0x0001, 0xffff},
   {0x8000, 0x0000, 0x0007, 0xffff},
   {0x8000, 0x0000, 0x001f, 0xffff},
   {0x8000, 0x0000, 0x007f, 0xffff},
   {0x8000, 0x0000, 0x01ff, 0xffff},
   {0x8000, 0x0000, 0x07ff, 0xffff},
   {0x8000, 0x0000, 0x1fff, 0xffff},
   {0x8000, 0x0000, 0x7fff, 0xffff},
   {0x8000, 0x0001, 0xffff, 0xffff},
   {0x8000, 0x0007, 0xffff, 0xffff},
   {0x8000, 0x001f, 0xffff, 0xffff},
   {0x8000, 0x007f, 0xffff, 0xffff},
   {0x8000, 0x01ff, 0xffff, 0xffff},
   {0x8000, 0x07ff, 0xffff, 0xffff},
   {0x8000, 0x1fff, 0xffff, 0xffff},
   {0x8000, 0x7fff, 0xffff, 0xffff},
   {0x8001, 0xffff, 0xffff, 0xffff},
   {0x8007, 0xffff, 0xffff, 0xffff},
   {0x801f, 0xffff, 0xffff, 0xffff},
   {0x807f, 0xffff, 0xffff, 0xffff},
   {0x81ff, 0xffff, 0xffff, 0xffff},
   {0x87ff, 0xffff, 0xffff, 0xffff},
   {0x9fff, 0xffff, 0xffff, 0xffff},
   {0xffff, 0xffff, 0xffff, 0xffff},
   {0xffff, 0xffff, 0xffff, 0xfffc},
   {0xffff, 0xffff, 0xffff, 0xfff0},
   {0xffff, 0xffff, 0xffff, 0xffc0},
   {0xffff, 0xffff, 0xffff, 0xff00},
   {0xffff, 0xffff, 0xffff, 0xfc00},
   {0xffff, 0xffff, 0xffff, 0xf000},
   {0xffff, 0xffff, 0xffff, 0xc000},
   {0xffff, 0xffff, 0xffff, 0x0000},
   {0xffff, 0xffff, 0xfffc, 0x0000},
   {0xffff, 0xffff, 0xfff0, 0x0000},
   {0xffff, 0xffff, 0xffc0, 0x0000},
   {0xffff, 0xffff, 0xff00, 0x0000},
   {0xffff, 0xffff, 0xfc00, 0x0000},
   {0xffff, 0xffff, 0xf000, 0x0000},
   {0xffff, 0xffff, 0xc000, 0x0000},
   {0xffff, 0xffff, 0x0000, 0x0000},
   {0xffff, 0xfffc, 0x0000, 0x0000},
   {0xffff, 0xfff0, 0x0000, 0x0000},
   {0xffff, 0xffc0, 0x0000, 0x0000},
   {0xffff, 0xff00, 0x0000, 0x0000},
   {0xffff, 0xfc00, 0x0000, 0x0000},
   {0xffff, 0xf000, 0x0000, 0x0000},
   {0xffff, 0xc000, 0x0000, 0x0000},
   {0xffff, 0x0000, 0x0000, 0x0000},
   {0xfffc, 0x0000, 0x0000, 0x0000},
   {0xfff0, 0x0000, 0x0000, 0x0000},
   {0xffc0, 0x0000, 0x0000, 0x0000},
   {0xff00, 0x0000, 0x0000, 0x0000},
   {0xfc00, 0x0000, 0x0000, 0x0000},
   {0xf000, 0x0000, 0x0000, 0x0000},
   {0xe000, 0x0000, 0x0000, 0x0000},
   {0xc000, 0x0000, 0x0000, 0x0000}
};
int num_cool_ints = sizeof(cool_ints)/sizeof(AR_INT_64);


AR_INT_64 dshift_vals[] =
{
   {0x0000, 0x0000, 0x0000, 0x0000},  /*  0           */
   {0x0000, 0x0000, 0x0000, 0x0001},  /*  1           */
   {0x0000, 0x0000, 0x0000, 0x000a},  /* 10           */
   {0x0000, 0x0000, 0x0000, 0x0010},  /* 16           */
   {0x0000, 0x0000, 0x0000, 0x0015},  /* 20           */
   {0x0000, 0x0000, 0x0000, 0x001f},  /* 31           */
   {0x0000, 0x0000, 0x0000, 0x0020},  /* 32           */
   {0x0000, 0x0000, 0x0000, 0x0021},  /* 33           */
   {0x0000, 0x0000, 0x0000, 0x002b},  /* 44           */
   {0x0000, 0x0000, 0x0000, 0x003f},  /* 63           */
   {0x0000, 0x0000, 0x0000, 0x0040},  /* 64           */
   {0x0000, 0x0000, 0x0000, 0x0041},  /* 65           */
   {0xffff, 0xffff, 0xffff, 0xffff}   /* -1           */
};
int num_dshift_vals = sizeof(dshift_vals)/sizeof(AR_INT_64);


AR_TYPE ar_types[] =
{
   AR_Int_8_S,
   AR_Int_8_U,
   AR_Int_16_S,
   AR_Int_16_U,
   AR_Int_24_S,
   AR_Int_24_U,
   AR_Int_32_S,
   AR_Int_32_U,
   AR_Int_46_S,
   AR_Int_64_S,
   AR_Int_64_U
};
int num_ar_types = sizeof(ar_types) / sizeof(AR_TYPE);

#define IS_SUPPORTED_INT_TYPE(t)			\
	((t) == AR_Int_8_S  || (t) == AR_Int_8_U  ||	\
         (t) == AR_Int_16_S || (t) == AR_Int_16_U ||	\
	 (t) == AR_Int_32_S || (t) == AR_Int_32_U ||	\
	 (t) == AR_Int_46_S                       ||	\
	 (t) == AR_Int_64_S || (t) == AR_Int_64_U)

AR_TYPE sint8_artype  = AR_Int_8_S;
AR_TYPE uint8_artype  = AR_Int_8_U;
AR_TYPE sint16_artype = AR_Int_16_S;
AR_TYPE uint16_artype = AR_Int_16_U;
AR_TYPE sint24_artype = AR_Int_24_S;
AR_TYPE uint24_artype = AR_Int_24_U;
AR_TYPE sint32_artype = AR_Int_32_S;
AR_TYPE uint32_artype = AR_Int_32_U;
AR_TYPE sint46_artype = AR_Int_46_S;
AR_TYPE sint64_artype = AR_Int_64_S;
AR_TYPE uint64_artype = AR_Int_64_U;

SINT64 act_result  [sizeof(cool_ints)/sizeof(AR_INT_64)];
int    act_status  [sizeof(cool_ints)/sizeof(AR_INT_64)];

AR_INT_64 test_result[sizeof(cool_ints)/sizeof(AR_INT_64)];
int       test_status[sizeof(cool_ints)/sizeof(AR_INT_64)];


/*========================================
 *
 * SUN SUPPORT
 */
#if defined __sun
    /*
     * On the Sun, these are not intrinsics.  Also, the Sun 64 bit ("long
     * long") shift code is limited to shifting by 0 .. 63 bits, so shift
     * counts of 64, and some of those of 0, have to be handled specially.
     */
    static SINT64 _dshiftl(SINT64 op1, SINT64 op2, SINT64 shcnt)
    {
	SINT64 rshcnt = ((SINT64) 64) - shcnt;

	if (shcnt == 0)
	    return(op1);
	else if (shcnt == 64)
	    return(op2);
	return((op1 << shcnt) | (((UINT64) op2) >> rshcnt));
    }

    static SINT64 _dshiftr(SINT64 op1, SINT64 op2, SINT64 shcnt)
    {
	SINT64 rshcnt = ((SINT64) 64) - shcnt;

	if (shcnt == 0)
	    return(op2);
	else if (shcnt == 64)
	    return(op1);
	return((op1 << rshcnt) | (((UINT64) op2) >> shcnt));
    }

    static UINT64 shftl64(UINT64 op, SINT64 shcnt)
    {
	if (shcnt == 0)
	    return(op);
	else if (shcnt == 64)
	    return(0);
	return(op << shcnt);
    }

    static UINT64 shftr64(UINT64 op, SINT64 shcnt)
    {
	if (shcnt == 0)
	    return(op);
	else if (shcnt == 64)
	    return(0);
	return(op >> shcnt);
    }

    static SINT64 _mask(SINT64 bits)
    {
	/*
	 * Kludge time ...
	 *
	 * Gcc 2.6.3 apparently has a bug, in that (~((UINT64) 0)) does
	 * not, in fact generate 0xFFFFFFFFFFFFFFFF.  Instead, it makes
	 * a 0x00000000FFFFFFFF.  The following is used to generate the
	 * correct value instead.  I would recommend switching to Sun's
	 * unbundled ANSI cc, but it only #defines __STC__ as 1 when a
	 * -Xc (strict ANSI compliance) switch is given, and in that
	 * mode, it doesn't allow the "long long" types.
	 */
	union
	{
	    AR_INT_64 u64p;
	    UINT64    u64w;
	}               u;

	u.u64p.part1 = u.u64p.part2 = u.u64p.part3 = u.u64p.part4 = 0xFFFF;

	if (bits > 0 && bits <= 63)
	    return((SINT64) (u.u64w << (((SINT64) 64) - bits)));
	else if (bits >= 64 && bits < 128)
	    return((SINT64) (u.u64w >> (bits - ((SINT64) 64))));
	return(0);
    }

    static SINT64 _leadz(SINT64 val)
    {
	int lz_bits;

	if (val == 0)
	    return(64);
	for (lz_bits = 0; val > 0; val <<= 1, lz_bits++)
	    ;
	return((SINT64) lz_bits);
    }

    static SINT64 _popcnt(SINT64 val)
    {
	int pop_bits, bit_count;

	for (pop_bits = bit_count = 0;
	     bit_count < 64;
	     pop_bits += val & 0x1, val >>= 1, bit_count++)
	    ;
	return((SINT64) pop_bits);
    }

    static SINT64 _poppar(SINT64 val)
    {
	return(_popcnt(val) & ((SINT64) 0x1));
    }
#endif


/*========================================
 *
 * 8-BIT SUPPORT
 */

/*
 * These un-sign-extend and sign-extend 8 bit values stored in
 * 64 bit integers.
 */
SINT64 DSEXT8(SINT64 v)
{
    return(v & 0xFF);
}

SINT64 SEXT8(SINT64 v)
{
    return((( - ((v >> 7) & 0x1)) << 8) | DSEXT8(v));
}


/*
 * These load a 8 bit value and if necessary sign extend it to 8 bits.
 */
SINT64 LS8(AR_INT_64 *vp)
{
    return(SEXT8((UINT64) (vp->part4 & 0xFF)));
}

UINT64 LU8(AR_INT_64 *vp)
{
    return((UINT64) (vp->part4 & 0xFF));
}


/*
 * This determines whether or not 8-bit values are zero/negative.
 */
#define is_zero8(v)    (DSEXT8(v) == 0)
#define is_neg8(v)     (((v) & 0x80) != 0)


/*
 * New 8-bit equivalents to the 64-bit intrinsics.
 */
static SINT64 _dshiftl8(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    SINT64 rshcnt = ((SINT64) 8) - shcnt;

    op2 &= 0xFF;
    return(((op1 << shcnt) | (op2 >> rshcnt)) & 0xFF);
}

static SINT64 _dshiftr8(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    SINT64 rshcnt = ((SINT64) 8) - shcnt;

    op2 &= 0xFF;
    return(((op1 << rshcnt) | (op2 >> shcnt)) & 0xFF);
}

static SINT64 _mask8(SINT64 bits)
{
    bits &= 0xFF;
    if (bits > 0 && bits < 8)
	return((SINT64) ((0xFF << (((SINT64) 8) - bits)) & 0xFF));
    else if (bits >= 8 && bits < 16)
	return((SINT64) (((UINT64) 0xFF) >> (bits - 8)));
    return(0);
}

static SINT64 _leadz8(SINT64 val)
{
    int lz_bits;

    val <<= 56;
    if (val == 0)
	return(8);
    for (lz_bits = 0; val >= 0; val <<= 1) {
	lz_bits++;
    }
    return((SINT64) lz_bits);
}

static SINT64 _popcnt8(SINT64 val)
{
    int pop_bits, bit_count;

    for (pop_bits = bit_count = 0;
	 bit_count < 8;
	 pop_bits += val & 0x1, val >>= 1, bit_count++)
	;
    return((SINT64) pop_bits);
}

static SINT64 _poppar8(SINT64 val)
{
    return(_popcnt8(val) & ((SINT64) 0x1));
}


/*========================================
 *
 * 16-BIT SUPPORT
 */

/*
 * These un-sign-extend and sign-extend 16 bit values stored in
 * 64 bit integers.
 */
SINT64 DSEXT16(SINT64 v)
{
    return(v & 0xFFFF);
}

SINT64 SEXT16(SINT64 v)
{
    return((( - ((v >> 15) & 0x1)) << 16) | DSEXT16(v));
}


/*
 * These load a 16 bit value and if necessary sign extend it to 16 bits.
 */
SINT64 LS16(AR_INT_64 *vp)
{
    return(SEXT16((UINT64) vp->part4));
}

UINT64 LU16(AR_INT_64 *vp)
{
    return((UINT64) vp->part4);
}


/*
 * This determines whether or not 16-bit values are zero/negative.
 */
#define is_zero16(v)    (DSEXT16(v) == 0)
#define is_neg16(v)     (((v) & 0x8000) != 0)


/*
 * New 16-bit equivalents to the 64-bit intrinsics.
 */
static SINT64 _dshiftl16(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    SINT64 rshcnt = ((SINT64) 16) - shcnt;

    op2 &= 0xFFFF;
    return(((op1 << shcnt) | (op2 >> rshcnt)) & 0xFFFF);
}

static SINT64 _dshiftr16(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    SINT64 rshcnt = ((SINT64) 16) - shcnt;

    op2 &= 0xFFFF;
    return(((op1 << rshcnt) | (op2 >> shcnt)) & 0xFFFF);
}

static SINT64 _mask16(SINT64 bits)
{
    bits &= 0xFFFF;
    if (bits > 0 && bits < 16)
	return((SINT64) ((0xFFFF << (((SINT64) 16) - bits)) & 0xFFFF));
    else if (bits >= 16 && bits < 32)
	return((SINT64) (((UINT64) 0xFFFF) >> (bits - 16)));
    return(0);
}

static SINT64 _leadz16(SINT64 val)
{
    int lz_bits;

    val <<= 48;
    if (val == 0)
	return(16);
    for (lz_bits = 0; val >= 0; val <<= 1) {
	lz_bits++;
    }
    return((SINT64) lz_bits);
}

static SINT64 _popcnt16(SINT64 val)
{
    int pop_bits, bit_count;

    for (pop_bits = bit_count = 0;
	 bit_count < 16;
	 pop_bits += val & 0x1, val >>= 1, bit_count++)
	;
    return((SINT64) pop_bits);
}

static SINT64 _poppar16(SINT64 val)
{
    return(_popcnt16(val) & ((SINT64) 0x1));
}


/*========================================
 *
 * 32-BIT SUPPORT
 */

/*
 * These un-sign-extend and sign-extend 32 bit values stored in
 * 64 bit integers.
 */
SINT64 DSEXT32(SINT64 v)
{
    return(v & 0xFFFFFFFF);
}

SINT64 SEXT32(SINT64 v)
{
    return((( - ((v >> 31) & 0x1)) << 32) | DSEXT32(v));
}


/*
 * These load a 32 bit value and if necessary sign extend it to 32 bits.
 */
SINT64 LS32(AR_INT_64 *vp)
{
    return(SEXT32((((UINT64) vp->part3) << 16) | ((UINT64) vp->part4)));
}

UINT64 LU32(AR_INT_64 *vp)
{
    return((((UINT64) vp->part3) << 16) | (((UINT64) vp->part4)));
}


/*
 * This determines whether or not 32-bit values are zero/negative.
 */
#define is_zero32(v)    (DSEXT32(v) == 0)
#define is_neg32(v)     (((v) & 0x80000000) != 0)


/*
 * New 32-bit equivalents to the 64-bit intrinsics.
 */
static SINT64 _dshiftl32(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    SINT64 rshcnt = ((SINT64) 32) - shcnt;

    op2 &= 0xFFFFFFFF;
    return(((op1 << shcnt) | (op2 >> rshcnt)) & 0xFFFFFFFF);
}

static SINT64 _dshiftr32(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    SINT64 rshcnt = ((SINT64) 32) - shcnt;

    op2 &= 0xFFFFFFFF;
    return(((op1 << rshcnt) | (op2 >> shcnt)) & 0xFFFFFFFF);
}

static SINT64 _mask32(SINT64 bits)
{
    bits &= 0xFFFFFFFF;
    if (bits > 0 && bits < 32)
	return((SINT64) ((0xFFFFFFFF << (((SINT64) 32) - bits)) & 0xFFFFFFFF));
    else if (bits >= 32 && bits < 64)
	return((SINT64) (((UINT64) 0xFFFFFFFF) >> (bits - 32)));
    return(0);
}

static SINT64 _leadz32(SINT64 val)
{
    int lz_bits;

    val <<= 32;
    if (val == 0)
	return(32);
    for (lz_bits = 0; val >= 0; val <<= 1) {
	lz_bits++;
    }
    return((SINT64) lz_bits);
}

static SINT64 _popcnt32(SINT64 val)
{
    int pop_bits, bit_count;

    for (pop_bits = bit_count = 0;
	 bit_count < 32;
	 pop_bits += val & 0x1, val >>= 1, bit_count++)
	;
    return((SINT64) pop_bits);
}

static SINT64 _poppar32(SINT64 val)
{
    return(_popcnt32(val) & ((SINT64) 0x1));
}


/*========================================
 *
 * 46-BIT SUPPORT
 */

/*
 * These un-sign-extend and sign-extend 46 bit values stored in
 * 64 bit integers.  They're no-ops, but they make writing the
 * code easier.
 */
SINT64 DSEXT46(SINT64 v)
{
    return(v);
}

SINT64 SEXT46(SINT64 v)
{
    return(v);
}


/*
 * This determines whether or not 46-bit values are zero/negative.
 */
#define is_zero46(v)    ((v) == 0)
#define is_neg46(v)     ((v) < 0)


/*
 * New 46-bit equivalents to the 64-bit intrinsics.  Also no-ops,
 * to make writing the code easier.
 */
static SINT64 _dshiftl46(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    return(_dshiftl(op1, op2, shcnt));
}

static SINT64 _dshiftr46(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    return(_dshiftr(op1, op2, shcnt));
}

static SINT64 _mask46(SINT64 bits)
{
    return(_mask(bits));
}

static SINT64 _leadz46(SINT64 val)
{
    return(_leadz(val));
}

static SINT64 _popcnt46(SINT64 val)
{
    return(_popcnt(val));
}

static SINT64 _poppar46(SINT64 val)
{
    return(_poppar(val));
}


/*========================================
 *
 * 64-BIT SUPPORT
 */

/*
 * These un-sign-extend and sign-extend 64 bit values stored in
 * 64 bit integers.  They're no-ops, but they make writing the
 * code easier.
 */
SINT64 DSEXT64(SINT64 v)
{
    return(v);
}

SINT64 SEXT64(SINT64 v)
{
    return(v);
}


/*
 * This determines whether or not 64-bit values are zero/negative.
 */
#define is_zero64(v)    ((v) == 0)
#define is_neg64(v)     ((v) < 0)


/*
 * New 64-bit equivalents to the 64-bit intrinsics.  Also no-ops,
 * to make writing the code easier.
 */
static SINT64 _dshiftl64(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    return(_dshiftl(op1, op2, shcnt));
}

static SINT64 _dshiftr64(SINT64 op1, SINT64 op2, SINT64 shcnt)
{
    return(_dshiftr(op1, op2, shcnt));
}

static SINT64 _mask64(SINT64 bits)
{
    return(_mask(bits));
}

static SINT64 _leadz64(SINT64 val)
{
    return(_leadz(val));
}

static SINT64 _popcnt64(SINT64 val)
{
    return(_popcnt(val));
}

static SINT64 _poppar64(SINT64 val)
{
    return(_poppar(val));
}


main()
{
   test_bitor();
   test_bitand();
   test_bitxor();
   test_bitcomplement();
   test_shiftl();
   test_shiftr();
   test_dshiftl();
   test_dshiftr();
   test_ishft();
   test_ishftc();
   test_ibits();
   test_mask();
   test_leadz();
   test_popcnt();
   test_poppar();
   test_arstatus();
   test_add();
   test_subtract();
   test_negate();
   test_abs();
   test_multiply();
   test_divide();
   test_modulo();
   test_compare();
   exit(0);
}


/*======================================================================
 *
 *	BIT OPERATORS
 *
 */

#define TEST_BINARY_BIT_OP_SIGNED(size, opr, func)			\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_result[j] = DSEXT##size(LS##size(&cool_ints[i]) opr	\
					LS##size(&cool_ints[j]));	\
									\
	    act_status[j] = is_zero##size(act_result[j])		\
			    ? AR_STAT_ZERO : AR_STAT_OK;		\
	    if (is_neg##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_NEGATIVE;			\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = func((AR_DATA *)&test_result[j],		\
				  &sint##size##_artype,			\
				  (AR_DATA *)&cool_ints[i],		\
				  &sint##size##_artype,			\
				  (AR_DATA *)&cool_ints[j],		\
				  &sint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != act_result[j])			\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_BINARY_BIT_OP_UNSIGNED(size, opr, func)			\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_result[j] = DSEXT##size(LU##size(&cool_ints[i]) opr	\
					LU##size(&cool_ints[j]));	\
									\
	    act_status[j] =   is_zero##size(act_result[j])		\
			    ? AR_STAT_ZERO				\
			    : AR_STAT_OK;				\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = func((AR_DATA *)&test_result[j],		\
				  &uint##size##_artype,			\
				  (AR_DATA *)&cool_ints[i],		\
				  &uint##size##_artype,			\
				  (AR_DATA *)&cool_ints[j],		\
				  &uint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_BINARY_BIT_OP_TYPES(opr, func)				\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing:           types  %-10s: ", #opr);		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       act_status[k] = AR_STAT_OK;				\
	       if (ar_types[i] != ar_types[j] ||			\
		   ar_types[j] != ar_types[k] ||			\
		   !IS_SUPPORTED_INT_TYPE(ar_types[i]))			\
		  act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	       test_status[k] = func((AR_DATA *)&test_result[1],	\
				     &ar_types[i],			\
				     (AR_DATA *)&cool_ints[1],		\
				     &ar_types[j],			\
				     (AR_DATA *)&cool_ints[1],		\
				     &ar_types[k]);			\
									\
	       if ((act_status[k] ^ test_status[k]) &			\
		   AR_STAT_INVALID_TYPE)				\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_UNARY_BIT_OP_SIGNED(size, opr, func)			\
   do {									\
      int j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_result[j] = DSEXT##size(opr LS##size(&cool_ints[j]));	\
									\
	 act_status[j] =   is_zero##size(act_result[j])			\
			 ? AR_STAT_ZERO					\
			 : AR_STAT_OK;					\
	 if (is_neg##size(act_result[j]))				\
	    act_status[j] |= AR_STAT_NEGATIVE;				\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = func((AR_DATA *)&test_result[j],		\
			       &sint##size##_artype,			\
			       (AR_DATA *)&cool_ints[j],		\
			       &sint##size##_artype);			\
									\
	 if (LU64(&test_result[j]) != act_result[j])			\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_UNARY_BIT_OP_UNSIGNED(size, opr, func)			\
   do {									\
      int j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_result[j] = DSEXT##size(opr LU##size(&cool_ints[j]));	\
									\
	 act_status[j] =   is_zero##size(act_result[j])			\
			 ? AR_STAT_ZERO					\
			 : AR_STAT_OK;					\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = func((AR_DATA *)&test_result[j],		\
			       &uint##size##_artype,			\
			       (AR_DATA *)&cool_ints[j],		\
			       &uint##size##_artype);			\
									\
	 if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_UNARY_BIT_OP_TYPES(opr, func)				\
   do {									\
      int j, k;								\
									\
      printf(" Testing:           types  %-10s: ", #opr);		\
									\
      for (j=0; j<num_ar_types; j++)					\
      {									\
	 for (k=0; k<num_ar_types; k++)					\
	 {								\
	    act_status[k] = AR_STAT_OK;					\
	    if (ar_types[j] != ar_types[k] ||				\
		!IS_SUPPORTED_INT_TYPE(ar_types[j]))			\
	       act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	    test_status[k] = func((AR_DATA *)&test_result[1],		\
				  &ar_types[j],				\
				  (AR_DATA *)&cool_ints[1],		\
				  &ar_types[k]);			\
									\
	    if ((act_status[k] ^ test_status[k]) &			\
		AR_STAT_INVALID_TYPE)					\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", j, k);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_bitor()
{

   TEST_BINARY_BIT_OP_SIGNED(   8, |, AR_bitor );
   TEST_BINARY_BIT_OP_UNSIGNED( 8, |, AR_bitor );

   TEST_BINARY_BIT_OP_SIGNED(  16, |, AR_bitor );
   TEST_BINARY_BIT_OP_UNSIGNED(16, |, AR_bitor );

   TEST_BINARY_BIT_OP_SIGNED(  32, |, AR_bitor );
   TEST_BINARY_BIT_OP_UNSIGNED(32, |, AR_bitor );

   TEST_BINARY_BIT_OP_SIGNED(  46, |, AR_bitor );

   TEST_BINARY_BIT_OP_SIGNED(  64, |, AR_bitor );
   TEST_BINARY_BIT_OP_UNSIGNED(64, |, AR_bitor );

   TEST_BINARY_BIT_OP_TYPES(       |, AR_bitor );

}  /* test_bitor */


test_bitand()
{

   TEST_BINARY_BIT_OP_SIGNED(   8, &, AR_bitand);
   TEST_BINARY_BIT_OP_UNSIGNED( 8, &, AR_bitand);

   TEST_BINARY_BIT_OP_SIGNED(  16, &, AR_bitand);
   TEST_BINARY_BIT_OP_UNSIGNED(16, &, AR_bitand);

   TEST_BINARY_BIT_OP_SIGNED(  32, &, AR_bitand);
   TEST_BINARY_BIT_OP_UNSIGNED(32, &, AR_bitand);

   TEST_BINARY_BIT_OP_SIGNED(  46, &, AR_bitand);

   TEST_BINARY_BIT_OP_SIGNED(  64, &, AR_bitand);
   TEST_BINARY_BIT_OP_UNSIGNED(64, &, AR_bitand);

   TEST_BINARY_BIT_OP_TYPES(       &, AR_bitand);

}  /* test_bitand */


test_bitxor()
{

   TEST_BINARY_BIT_OP_SIGNED(   8, ^, AR_bitxor);
   TEST_BINARY_BIT_OP_UNSIGNED( 8, ^, AR_bitxor);

   TEST_BINARY_BIT_OP_SIGNED(  16, ^, AR_bitxor);
   TEST_BINARY_BIT_OP_UNSIGNED(16, ^, AR_bitxor);

   TEST_BINARY_BIT_OP_SIGNED(  32, ^, AR_bitxor);
   TEST_BINARY_BIT_OP_UNSIGNED(32, ^, AR_bitxor);

   TEST_BINARY_BIT_OP_SIGNED(  46, ^, AR_bitxor);

   TEST_BINARY_BIT_OP_SIGNED(  64, ^, AR_bitxor);
   TEST_BINARY_BIT_OP_UNSIGNED(64, ^, AR_bitxor);

   TEST_BINARY_BIT_OP_TYPES(       ^, AR_bitxor);

}  /* test_bitxor */


test_bitcomplement()
{

   TEST_UNARY_BIT_OP_SIGNED(   8, ~, AR_bitcomplement);
   TEST_UNARY_BIT_OP_UNSIGNED( 8, ~, AR_bitcomplement);

   TEST_UNARY_BIT_OP_SIGNED(  16, ~, AR_bitcomplement);
   TEST_UNARY_BIT_OP_UNSIGNED(16, ~, AR_bitcomplement);

   TEST_UNARY_BIT_OP_SIGNED(  32, ~, AR_bitcomplement);
   TEST_UNARY_BIT_OP_UNSIGNED(32, ~, AR_bitcomplement);

   TEST_UNARY_BIT_OP_SIGNED(  46, ~, AR_bitcomplement);

   TEST_UNARY_BIT_OP_SIGNED(  64, ~, AR_bitcomplement);
   TEST_UNARY_BIT_OP_UNSIGNED(64, ~, AR_bitcomplement);

   TEST_UNARY_BIT_OP_TYPES(       ~, AR_bitcomplement);

}  /* test_bitcomplement */


#undef TEST_BINARY_BIT_OP_SIGNED
#undef TEST_BINARY_BIT_OP_UNSIGNED
#undef TEST_BINARY_BIT_OP_TYPES
#undef TEST_UNARY_BIT_OP_SIGNED
#undef TEST_UNARY_BIT_OP_UNSIGNED
#undef TEST_UNARY_BIT_OP_TYPES


/*======================================================================
 *
 *	SHIFT OPERATORS
 *
 */

#define TEST_SHIFT_OP_SIGNED(size, max_shift, opr, func)		\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    if (LS##size(&cool_ints[j]) >= max_shift ||			\
		LS##size(&cool_ints[j]) < 0)				\
	    {  /* status is undefined, result is zero */		\
	       act_status[j] = AR_STAT_UNDEFINED;			\
	       act_result[j] = 0;					\
	    }								\
	    else							\
	    {								\
	       act_result[j] = DSEXT##size(LS##size(&cool_ints[i]) opr	\
					   LS##size(&cool_ints[j]));	\
									\
	       if (size >= 46 &&					\
		   strcmp(#opr, ">>") == 0 &&				\
		   LS64(&cool_ints[i]) < 0)				\
	       {  /* the first operand is negative */			\
		  /* fill vacated bit(s) with sign bit */		\
		  act_result[j] |= (SINT64)				\
				   ~(-1 >> LS46(&cool_ints[j]));	\
	       }							\
									\
	       act_status[j] =   is_zero##size(act_result[j])		\
			       ? AR_STAT_ZERO				\
			       : AR_STAT_OK;				\
	       if (is_neg##size(act_result[j]))				\
		  act_status[j] |= AR_STAT_NEGATIVE;			\
	    }								\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = func((AR_DATA *)&test_result[j],		\
				  &sint##size##_artype,			\
				  (AR_DATA *)&cool_ints[i],		\
				  &sint##size##_artype,			\
				  (AR_DATA *)&cool_ints[j],		\
				  &sint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != act_result[j])			\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_SHIFT_OP_UNSIGNED(size, max_shift, opr, func)		\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    if (LU##size(&cool_ints[j]) >= max_shift)			\
	    {  /* status is undefined, result is zero */		\
	       act_status[j] = AR_STAT_UNDEFINED;			\
	       act_result[j] = 0;					\
	    }								\
	    else							\
	    {								\
	       act_result[j] = DSEXT##size(LU##size(&cool_ints[i]) opr	\
					   LU##size(&cool_ints[j]));	\
									\
	       act_status[j] =   is_zero##size(act_result[j])		\
			       ? AR_STAT_ZERO				\
			       : AR_STAT_OK;				\
	    }								\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = func((AR_DATA *)&test_result[j],		\
				  &uint##size##_artype,			\
				  (AR_DATA *)&cool_ints[i],		\
				  &uint##size##_artype,			\
				  (AR_DATA *)&cool_ints[j],		\
				  &uint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_SHIFT_OP_TYPES(opr, func)					\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing:           types  %-10s: ", #opr);		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       act_status[k] = AR_STAT_OK;				\
	       if (ar_types[i] != ar_types[j] ||			\
		   !IS_SUPPORTED_INT_TYPE(ar_types[i]))			\
		  act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	       test_status[k] = func((AR_DATA *)&test_result[1],	\
				     &ar_types[i],			\
				     (AR_DATA *)&cool_ints[1],		\
				     &ar_types[j],			\
				     (AR_DATA *)&cool_ints[1],		\
				     &ar_types[k]);			\
									\
	       if ((act_status[k] ^ test_status[k]) &			\
		   AR_STAT_INVALID_TYPE)				\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_shiftl()
{

   TEST_SHIFT_OP_SIGNED(   8,  8, <<, AR_shiftl);
   TEST_SHIFT_OP_UNSIGNED( 8,  8, <<, AR_shiftl);

   TEST_SHIFT_OP_SIGNED(  16, 16, <<, AR_shiftl);
   TEST_SHIFT_OP_UNSIGNED(16, 16, <<, AR_shiftl);

   TEST_SHIFT_OP_SIGNED(  32, 32, <<, AR_shiftl);
   TEST_SHIFT_OP_UNSIGNED(32, 32, <<, AR_shiftl);

   TEST_SHIFT_OP_SIGNED(  46, 64, <<, AR_shiftl);

   TEST_SHIFT_OP_SIGNED(  64, 64, <<, AR_shiftl);
   TEST_SHIFT_OP_UNSIGNED(64, 64, <<, AR_shiftl);

   TEST_SHIFT_OP_TYPES(       <<, AR_shiftl);

}  /* test_shiftl */


test_shiftr()
{

   TEST_SHIFT_OP_SIGNED(   8,  8, >>, AR_shiftr);
   TEST_SHIFT_OP_UNSIGNED( 8,  8, >>, AR_shiftr);

   TEST_SHIFT_OP_SIGNED(  16, 16, >>, AR_shiftr);
   TEST_SHIFT_OP_UNSIGNED(16, 16, >>, AR_shiftr);

   TEST_SHIFT_OP_SIGNED(  32, 32, >>, AR_shiftr);
   TEST_SHIFT_OP_UNSIGNED(32, 32, >>, AR_shiftr);

   TEST_SHIFT_OP_SIGNED(  46, 64, >>, AR_shiftr);

   TEST_SHIFT_OP_SIGNED(  64, 64, >>, AR_shiftr);
   TEST_SHIFT_OP_UNSIGNED(64, 64, >>, AR_shiftr);

   TEST_SHIFT_OP_TYPES(       >>, AR_shiftr);

}  /* test_shiftr */


#undef TEST_SHIFT_OP_SIGNED
#undef TEST_SHIFT_OP_UNSIGNED
#undef TEST_SHIFT_OP_TYPES


#define TEST_DSHIFT_OP_SIGNED(size, max_shift, opr)			\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    for (k=0; k<num_dshift_vals; k++)				\
	    {								\
	       if (LS##size(&dshift_vals[k]) > max_shift ||		\
		   LS##size(&dshift_vals[k]) < 0)			\
	       {  /* status is undefined, result is zero */		\
		  act_status[k] = AR_STAT_UNDEFINED;			\
		  act_result[k] = 0;					\
	       }							\
	       else							\
	       {							\
		  act_result[k] = DSEXT##size(				\
				     opr##size(				\
					LS##size(&cool_ints[i]),	\
					LS##size(&cool_ints[j]),	\
					LS##size(&dshift_vals[k])));	\
									\
		  act_status[k] =   is_zero##size(act_result[k])	\
				  ? AR_STAT_ZERO			\
				  : AR_STAT_OK;				\
		  if (is_neg##size(act_result[k]))			\
		     act_status[k] |= AR_STAT_NEGATIVE;			\
	       }							\
									\
	       test_result[k].part1 = 0;				\
	       test_result[k].part2 = 0;				\
	       test_result[k].part3 = 0;				\
	       test_result[k].part4 = 0;				\
									\
	       test_status[k] = AR##opr((AR_DATA *)&test_result[k],	\
					&sint##size##_artype,		\
					(AR_DATA *)&cool_ints[i],	\
					&sint##size##_artype,		\
					(AR_DATA *)&cool_ints[j],	\
					&sint##size##_artype,		\
					(AR_DATA *)&dshift_vals[k],	\
					&sint##size##_artype);		\
									\
	       if (LU64(&test_result[k]) != (UINT64)act_result[k])	\
	       {							\
		  printf(" ERROR:  incorrect result at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
									\
	       if (act_status[k] != test_status[k])			\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_DSHIFT_OP_UNSIGNED(size, max_shift, opr)			\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    for (k=0; k<num_dshift_vals; k++)				\
	    {								\
	       if (LU##size(&dshift_vals[k]) > max_shift)		\
	       {  /* status is undefined, result is zero */		\
		  act_status[k] = AR_STAT_UNDEFINED;			\
		  act_result[k] = 0;					\
	       }							\
	       else							\
	       {							\
		  act_result[k] = DSEXT##size(				\
				     opr##size(				\
					LU##size(&cool_ints[i]),	\
					LU##size(&cool_ints[j]),	\
					LU##size(&dshift_vals[k])));	\
									\
		  act_status[k] =   is_zero##size(act_result[k])	\
				  ? AR_STAT_ZERO			\
				  : AR_STAT_OK;				\
	       }							\
									\
	       test_result[k].part1 = 0;				\
	       test_result[k].part2 = 0;				\
	       test_result[k].part3 = 0;				\
	       test_result[k].part4 = 0;				\
									\
	       test_status[k] = AR##opr((AR_DATA *)&test_result[k],	\
					&uint##size##_artype,		\
					(AR_DATA *)&cool_ints[i],	\
					&uint##size##_artype,		\
					(AR_DATA *)&cool_ints[j],	\
					&uint##size##_artype,		\
					(AR_DATA *)&dshift_vals[k],	\
					&uint##size##_artype);		\
									\
	       if (LU64(&test_result[k]) != (UINT64)act_result[k])	\
	       {							\
		  printf(" ERROR:  incorrect result at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
									\
	       if (act_status[k] != test_status[k])			\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_DSHIFT_OP_TYPES(opr)					\
   do {									\
      int h, i, j, k;							\
									\
      printf(" Testing:           types  %-10s: ", #opr);		\
									\
      for (h=0; h<num_ar_types; h++)					\
      {									\
	 for (i=0; i<num_ar_types; i++)					\
	 {								\
	    for (j=0; j<num_ar_types; j++)				\
	    {								\
	       for (k=0; k<num_ar_types; k++)				\
	       {							\
		  act_status[k] = AR_STAT_OK;				\
		  if (ar_types[h] != ar_types[i] ||			\
		      ar_types[i] != ar_types[j] ||			\
		      !IS_SUPPORTED_INT_TYPE(ar_types[h]))		\
		     act_status[k] = AR_STAT_INVALID_TYPE;		\
									\
		  test_status[k] = AR##opr((AR_DATA *)&test_result[1],	\
					   &ar_types[h],		\
					   (AR_DATA *)&cool_ints[1],	\
					   &ar_types[i],		\
					   (AR_DATA *)&cool_ints[1],	\
					   &ar_types[j],		\
					   (AR_DATA *)&dshift_vals[1],	\
					   &ar_types[k]);		\
									\
		  if ((act_status[k] ^ test_status[k]) &		\
		      AR_STAT_INVALID_TYPE) {				\
		     printf(" ERROR:  incorrect status at "		\
			    "%d,%d,%d,%d\n",				\
			    h, i, j, k);				\
		     abort();						\
		  }							\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_dshiftl()
{

   TEST_DSHIFT_OP_SIGNED(   8,  8, _dshiftl);
   TEST_DSHIFT_OP_UNSIGNED( 8,  8, _dshiftl);

   TEST_DSHIFT_OP_SIGNED(  16, 16, _dshiftl);
   TEST_DSHIFT_OP_UNSIGNED(16, 16, _dshiftl);

   TEST_DSHIFT_OP_SIGNED(  32, 32, _dshiftl);
   TEST_DSHIFT_OP_UNSIGNED(32, 32, _dshiftl);

   TEST_DSHIFT_OP_SIGNED(  46, 64, _dshiftl);

   TEST_DSHIFT_OP_SIGNED(  64, 64, _dshiftl);
   TEST_DSHIFT_OP_UNSIGNED(64, 64, _dshiftl);

   TEST_DSHIFT_OP_TYPES(       _dshiftl);

}  /* test_dshiftl */


test_dshiftr()
{

   TEST_DSHIFT_OP_SIGNED(   8,  8, _dshiftr);
   TEST_DSHIFT_OP_UNSIGNED( 8,  8, _dshiftr);

   TEST_DSHIFT_OP_SIGNED(  16, 16, _dshiftr);
   TEST_DSHIFT_OP_UNSIGNED(16, 16, _dshiftr);

   TEST_DSHIFT_OP_SIGNED(  32, 32, _dshiftr);
   TEST_DSHIFT_OP_UNSIGNED(32, 32, _dshiftr);

   TEST_DSHIFT_OP_SIGNED(  46, 64, _dshiftr);

   TEST_DSHIFT_OP_SIGNED(  64, 64, _dshiftr);
   TEST_DSHIFT_OP_UNSIGNED(64, 64, _dshiftr);

   TEST_DSHIFT_OP_TYPES(       _dshiftr);

}  /* test_dshiftr */


#undef TEST_DSHIFT_OP_SIGNED
#undef TEST_DSHIFT_OP_UNSIGNED
#undef TEST_DSHIFT_OP_TYPES


static
void
test_ishft_signed(int size)
{
    int i, j;

    printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "ISHFT");

    for (i=0; i<num_cool_ints; i++)
    {
	for (j=0; j<num_cool_ints; j++)
	{
	    SINT64 shift;

	    test_result[j].part1 = 0;
	    test_result[j].part2 = 0;
	    test_result[j].part3 = 0;
	    test_result[j].part4 = 0;

	    switch (size)
	    {
	    case 8:
		shift = LS8(&cool_ints[j]);
		if ((shift > 0 && shift >  size) ||
		    (shift < 0 && shift < -size))
		{  /* status is undefined, result is zero */
		    act_status[j] = AR_STAT_UNDEFINED;
		    act_result[j] = 0;
		}
		else
		{
		    if (shift > 0)
			act_result[j] = DSEXT8(LU8(&cool_ints[i]) <<
					       shift);
		    else
			act_result[j] = DSEXT8(LU8(&cool_ints[i]) >>
					       (-shift));

		    act_status[j] =   is_zero8(act_result[j])
				    ? AR_STAT_ZERO
				    : AR_STAT_OK;
		    if (is_neg8(act_result[j]))
			act_status[j] |= AR_STAT_NEGATIVE;
		}

		test_status[j] = AR_ishft((AR_DATA *)&test_result[j],
					  &sint8_artype,
					  (AR_DATA *)&cool_ints[i],
					  &sint8_artype,
					  (AR_DATA *)&cool_ints[j],
					  &sint8_artype);
		break;

	    case 16:
		shift = LS16(&cool_ints[j]);
		if ((shift > 0 && shift >  size) ||
		    (shift < 0 && shift < -size))
		{  /* status is undefined, result is zero */
		    act_status[j] = AR_STAT_UNDEFINED;
		    act_result[j] = 0;
		}
		else
		{
		    if (shift > 0)
			act_result[j] = DSEXT16(LU16(&cool_ints[i]) <<
						shift);
		    else
			act_result[j] = DSEXT16(LU16(&cool_ints[i]) >>
						(-shift));

		    act_status[j] =   is_zero16(act_result[j])
				    ? AR_STAT_ZERO
				    : AR_STAT_OK;
		    if (is_neg16(act_result[j]))
			act_status[j] |= AR_STAT_NEGATIVE;
		}

		test_status[j] = AR_ishft((AR_DATA *)&test_result[j],
					  &sint16_artype,
					  (AR_DATA *)&cool_ints[i],
					  &sint16_artype,
					  (AR_DATA *)&cool_ints[j],
					  &sint16_artype);
		break;

	    case 32:
		shift = LS32(&cool_ints[j]);
		if ((shift > 0 && shift >  size) ||
		    (shift < 0 && shift < -size))
		{  /* status is undefined, result is zero */
		    act_status[j] = AR_STAT_UNDEFINED;
		    act_result[j] = 0;
		}
		else
		{
		    if (shift > 0)
			act_result[j] = DSEXT32(LU32(&cool_ints[i]) <<
						shift);
		    else
			act_result[j] = DSEXT32(LU32(&cool_ints[i]) >>
						(-shift));

		    act_status[j] =   is_zero32(act_result[j])
				    ? AR_STAT_ZERO
				    : AR_STAT_OK;
		    if (is_neg32(act_result[j]))
			act_status[j] |= AR_STAT_NEGATIVE;
		}

		test_status[j] = AR_ishft((AR_DATA *)&test_result[j],
					  &sint32_artype,
					  (AR_DATA *)&cool_ints[i],
					  &sint32_artype,
					  (AR_DATA *)&cool_ints[j],
					  &sint32_artype);
		break;

	    case 46:
	    case 64:
		shift = LS64(&cool_ints[j]);
		if ((shift > 0 && shift >  64) ||
		    (shift < 0 && shift < -64))
		{  /* status is undefined, result is zero */
		    act_status[j] = AR_STAT_UNDEFINED;
		    act_result[j] = 0;
		}
		else
		{
		    if (shift > 0)
#if defined __sun
			act_result[j] = DSEXT64(shftl64(LU64(&cool_ints[i]),
							shift));
#else
			act_result[j] = DSEXT64(LU64(&cool_ints[i]) <<
						shift);
#endif
		    else
#if defined __sun
			act_result[j] = DSEXT64(shftr64(LU64(&cool_ints[i]),
							(-shift)));
#else
			act_result[j] = DSEXT64(LU64(&cool_ints[i]) >>
						(-shift));
#endif

		    act_status[j] =   is_zero64(act_result[j])
				    ? AR_STAT_ZERO
				    : AR_STAT_OK;
		    if (is_neg64(act_result[j]))
			act_status[j] |= AR_STAT_NEGATIVE;
		}

		test_status[j] = AR_ishft((AR_DATA *)&test_result[j],
					  &sint64_artype,
					  (AR_DATA *)&cool_ints[i],
					  &sint64_artype,
					  (AR_DATA *)&cool_ints[j],
					  &sint64_artype);
		break;
	    }

	    if (LU64(&test_result[j]) != (UINT64)act_result[j])
	    {
		printf(" ERROR:  incorrect result at %d,%d\n", i, j);
		abort();
	    }

	    if (act_status[j] != test_status[j])
	    {
		printf(" ERROR:  incorrect status at %d,%d\n", i, j);
		abort();
	    }
	}
    }

    printf("PASSED\n");
}


#define TEST_ISHFT_TYPES						\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing:           types  %-10s: ", "ISHFT");		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       act_status[k] = AR_STAT_OK;				\
	       if (ar_types[i] != ar_types[j] ||			\
		   !IS_SUPPORTED_INT_TYPE(ar_types[i]) ||		\
		   !IS_SUPPORTED_INT_TYPE(ar_types[k]))			\
		  act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	       test_status[k] = AR_ishft((AR_DATA *)&test_result[1],	\
					 &ar_types[i],			\
					 (AR_DATA *)&cool_ints[1],	\
					 &ar_types[j],			\
					 (AR_DATA *)&cool_ints[1],	\
					 &ar_types[k]);			\
									\
	       if ((act_status[k] ^ test_status[k]) &			\
		   AR_STAT_INVALID_TYPE)				\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_ishft()
{

   test_ishft_signed( 8);

   test_ishft_signed(16);

   test_ishft_signed(32);

   test_ishft_signed(46);

   test_ishft_signed(64);

   TEST_ISHFT_TYPES;

}  /* test_ishft */


#undef TEST_ISHFT_TYPES


static
void
test_ishftc_signed(int size)
{
    int i, j;

    printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "ISHFTC");

    if (size == 46)
	size = 64;

    for (i=0; i<num_cool_ints; i++)
    {
	for (j=0; j<num_cool_ints; j++)
	{
	    SINT64 width;
	    SINT64 max_shift;
	    SINT64 shift;
	    SINT64 unch_mask;
	    SINT64 rot_mask;

	    switch (size)
	    {
	    case 8:
		width = LS8(&cool_ints[j]);
		break;

	    case 16:
		width = LS16(&cool_ints[j]);
		break;

	    case 32:
		width = LS32(&cool_ints[j]);
		break;

	    case 46:
	    case 64:
		width = LS64(&cool_ints[j]);
		break;
	    }

	    if (width < 1 || width > size) {
		max_shift = 0;
		unch_mask = 0;
		rot_mask  = 0;
	    }
	    else {
		max_shift = width + 1;
#if defined(__sun)
		unch_mask = _mask(64 - width);
#else
		unch_mask = MASKL(64 - width);
#endif
		rot_mask  = ~unch_mask;
	    }

	    for (shift = -(max_shift + 1); shift < max_shift; shift++)
	    {
		UINT64	opnd;
		ar_data	shift_data;

		test_result[j].part1 = 0;
		test_result[j].part2 = 0;
		test_result[j].part3 = 0;
		test_result[j].part4 = 0;

		act_status[j] = AR_STAT_OK;

		if (width < 1 ||
		    width > size ||
		    (shift <  0 && shift < -size) ||
		    (shift >= 0 && shift >  size))
		{  /* status is undefined, result is zero */
		    act_status[j] = AR_STAT_UNDEFINED;
		    act_result[j] = 0;
		}

		switch (size)
		{
		case 8:
		    if ((shift <  0 && shift < -width) ||
			(shift >= 0 && shift >  width))
		    {
			act_status[j] = AR_STAT_UNDEFINED;
			act_result[j] = 0;
		    }
		    else if (act_status[j] == AR_STAT_OK)
		    {
			opnd = LU8(&cool_ints[i]);

			if (shift == 0)
			    act_result[j] = opnd;
			else if (shift > 0)
			    act_result[j] = (opnd & unch_mask) |
					    (((opnd << shift) |
					      ((opnd & rot_mask) >>
					       (width - shift))) &
					     rot_mask);
			else
			    act_result[j] = (opnd & unch_mask) |
					    ((((opnd & rot_mask) >> -shift) |
					      (opnd << (width + shift))) &
					     rot_mask);
			act_result[j] = DSEXT8(act_result[j]);

			act_status[j] =   is_zero8(act_result[j])
					? AR_STAT_ZERO
					: AR_STAT_OK;
			if (is_neg8(act_result[j]))
			    act_status[j] |= AR_STAT_NEGATIVE;
		    }

		    ZERO_INT8_UPPER(&shift_data);
		    shift_data.ar_i8.part5 = shift & 0xFF;

		    test_status[j] = AR_ishftc((AR_DATA *)&test_result[j],
					       &sint8_artype,
					       (AR_DATA *)&cool_ints[i],
					       &sint8_artype,
					       (AR_DATA *)&shift_data,
					       &sint8_artype,
					       (AR_DATA *)&cool_ints[j],
					       &sint8_artype);
		    break;

		case 16:
		    if ((shift <  0 && shift < -width) ||
			(shift >= 0 && shift >  width))
		    {
			act_status[j] = AR_STAT_UNDEFINED;
			act_result[j] = 0;
		    }
		    else if (act_status[j] == AR_STAT_OK)
		    {
			opnd = LU16(&cool_ints[i]);

			if (shift == 0)
			    act_result[j] = opnd;
			else if (shift > 0)
			    act_result[j] = (opnd & unch_mask) |
					    (((opnd << shift) |
					      ((opnd & rot_mask) >>
					       (width - shift))) &
					     rot_mask);
			else
			    act_result[j] = (opnd & unch_mask) |
					    ((((opnd & rot_mask) >> -shift) |
					      (opnd << (width + shift))) &
					     rot_mask);
			act_result[j] = DSEXT16(act_result[j]);

			act_status[j] =   is_zero16(act_result[j])
					? AR_STAT_ZERO
					: AR_STAT_OK;
			if (is_neg16(act_result[j]))
			    act_status[j] |= AR_STAT_NEGATIVE;
		    }

		    ZERO_INT16_UPPER(&shift_data);
		    shift_data.ar_i64.part4 = shift & 0xFFFF;

		    test_status[j] = AR_ishftc((AR_DATA *)&test_result[j],
					       &sint16_artype,
					       (AR_DATA *)&cool_ints[i],
					       &sint16_artype,
					       (AR_DATA *)&shift_data,
					       &sint16_artype,
					       (AR_DATA *)&cool_ints[j],
					       &sint16_artype);
		    break;

		case 32:
		    if ((shift <  0 && shift < -width) ||
			(shift >= 0 && shift >  width))
		    {
			act_status[j] = AR_STAT_UNDEFINED;
			act_result[j] = 0;
		    }
		    else if (act_status[j] == AR_STAT_OK)
		    {
			opnd = LU32(&cool_ints[i]);

			if (shift == 0)
			    act_result[j] = opnd;
			else if (shift > 0)
			    act_result[j] = (opnd & unch_mask) |
					    (((opnd << shift) |
					      ((opnd & rot_mask) >>
					       (width - shift))) &
					     rot_mask);
			else
			    act_result[j] = (opnd & unch_mask) |
					    ((((opnd & rot_mask) >> -shift) |
					      (opnd << (width + shift))) &
					     rot_mask);
			act_result[j] = DSEXT32(act_result[j]);

			act_status[j] =   is_zero32(act_result[j])
					? AR_STAT_ZERO
					: AR_STAT_OK;
			if (is_neg32(act_result[j]))
			    act_status[j] |= AR_STAT_NEGATIVE;
		    }

		    ZERO_INT32_UPPER(&shift_data);
		    shift_data.ar_i64.part3 = (shift >> 16) & 0xFFFF;
		    shift_data.ar_i64.part4 = (shift      ) & 0xFFFF;

		    test_status[j] = AR_ishftc((AR_DATA *)&test_result[j],
					       &sint32_artype,
					       (AR_DATA *)&cool_ints[i],
					       &sint32_artype,
					       (AR_DATA *)&shift_data,
					       &sint32_artype,
					       (AR_DATA *)&cool_ints[j],
					       &sint32_artype);
		    break;

		case 46:
		case 64:
		    if ((shift <  0 && shift < -width) ||
			(shift >= 0 && shift >  width))
		    {
			act_status[j] = AR_STAT_UNDEFINED;
			act_result[j] = 0;
		    }
		    else if (act_status[j] == AR_STAT_OK)
		    {
			opnd = LU64(&cool_ints[i]);

			if (shift == 0)
			    act_result[j] = opnd;
			else if (shift > 0)
#if defined __sun
			    act_result[j] = (opnd & unch_mask) |
					    ((shftl64(opnd, shift) |
					      shftr64((opnd & rot_mask),
						      (width - shift))) &
					     rot_mask);
#else
			    act_result[j] = (opnd & unch_mask) |
					    (((opnd << shift) |
					      ((opnd & rot_mask) >>
					       (width - shift))) &
					     rot_mask);
#endif
			else
#if defined __sun
			    act_result[j] = (opnd & unch_mask) |
					    ((shftr64((opnd & rot_mask),
						      (-shift)) |
					      shftl64(opnd, (width + shift))) &
					     rot_mask);
#else
			    act_result[j] = (opnd & unch_mask) |
					    ((((opnd & rot_mask) >> -shift) |
					      (opnd << (width + shift))) &
					     rot_mask);
#endif
			act_result[j] = DSEXT64(act_result[j]);

			act_status[j] =   is_zero64(act_result[j])
					? AR_STAT_ZERO
					: AR_STAT_OK;
			if (is_neg64(act_result[j]))
			    act_status[j] |= AR_STAT_NEGATIVE;
		    }

		    shift_data.ar_i64.part1 = (shift >> 48) & 0xFFFF;
		    shift_data.ar_i64.part2 = (shift >> 32) & 0xFFFF;
		    shift_data.ar_i64.part3 = (shift >> 16) & 0xFFFF;
		    shift_data.ar_i64.part4 = (shift      ) & 0xFFFF;

		    test_status[j] = AR_ishftc((AR_DATA *)&test_result[j],
					       &sint64_artype,
					       (AR_DATA *)&cool_ints[i],
					       &sint64_artype,
					       (AR_DATA *)&shift_data,
					       &sint64_artype,
					       (AR_DATA *)&cool_ints[j],
					       &sint64_artype);
		    break;
		}

		if (LU64(&test_result[j]) != (UINT64)act_result[j])
		{
		    printf(" ERROR:  incorrect result at %d,%d,%d\n",
			   i, j, (int) shift);
		    abort();
		}

		if (act_status[j] != test_status[j])
		{
		    printf(" ERROR:  incorrect status at %d,%d,%d\n",
			   i, j, (int) shift);
		    abort();
		}
	    }
	}
    }

    printf("PASSED\n");
}


#define TEST_ISHFTC_TYPES						\
   do {									\
      int i, j, k, l;							\
									\
      printf(" Testing:           types  %-10s: ", "ISHFTC");		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       for (l=0; l<num_ar_types; l++)				\
	       {							\
		  act_status[l] = AR_STAT_OK;				\
		  if (ar_types[i] != ar_types[j] ||			\
		      !IS_SUPPORTED_INT_TYPE(ar_types[i]) ||		\
		      !IS_SUPPORTED_INT_TYPE(ar_types[k]) ||		\
		      !IS_SUPPORTED_INT_TYPE(ar_types[l]))		\
		     act_status[l] = AR_STAT_INVALID_TYPE;		\
									\
		  test_status[l] = AR_ishftc((AR_DATA *)&test_result[1],\
					     &ar_types[i],		\
					     (AR_DATA *)&cool_ints[1],	\
					     &ar_types[j],		\
					     (AR_DATA *)&cool_ints[1],	\
					     &ar_types[l],		\
					     (AR_DATA *)&cool_ints[1],	\
					     &ar_types[k]);		\
									\
		  if ((act_status[l] ^ test_status[l]) &		\
		      AR_STAT_INVALID_TYPE)				\
		  {							\
		     printf(" ERROR:  incorrect status at %d,%d,%d,%d"	\
			    "\n",					\
			    i, j, k, l);				\
		     abort();						\
		  }							\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_ishftc()
{

   test_ishftc_signed( 8);

   test_ishftc_signed(16);

   test_ishftc_signed(32);

   test_ishftc_signed(46);

   test_ishftc_signed(64);

   TEST_ISHFTC_TYPES;

}  /* test_ishftc */


#undef TEST_ISHFTC_TYPES


/*======================================================================
 *
 *	IBITS OPERATOR
 *
 */

static
void
test_ibits_signed(int size)
{
    int i, j;

    printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "IBITS");

    if (size == 46)
	size = 64;

    for (i=0; i<num_cool_ints; i++)
    {
	for (j=0; j<num_cool_ints; j++)
	{
	    SINT64 width;
	    SINT64 max_shift;
	    SINT64 shift;
	    SINT64 mask;

	    switch (size)
	    {
	    case 8:
		width = LS8(&cool_ints[j]);
		break;

	    case 16:
		width = LS16(&cool_ints[j]);
		break;

	    case 32:
		width = LS32(&cool_ints[j]);
		break;

	    case 46:
	    case 64:
		width = LS64(&cool_ints[j]);
		break;
	    }

	    if (width < 0 || width > size) {
		max_shift = 0;
		mask = 0;
	    }
	    else {
		max_shift = width + 1;
#if defined(__sun)
		mask = _mask(128 - width);
#else
		mask = MASKR(width);
#endif
	    }

	    for (shift = -1; shift < max_shift + 1; shift++)
	    {
		UINT64	opnd;
		ar_data	shift_data;

		test_result[j].part1 = 0;
		test_result[j].part2 = 0;
		test_result[j].part3 = 0;
		test_result[j].part4 = 0;

		act_status[j] = AR_STAT_OK;

		if (width < 0 ||
		    shift < 0 || shift > (size - width))
		{  /* status is undefined, result is zero */
		    act_status[j] = AR_STAT_UNDEFINED;
		    act_result[j] = 0;
		}

		switch (size)
		{
		case 8:
		    if (act_status[j] == AR_STAT_OK)
		    {
			opnd = LU8(&cool_ints[i]);

			if (shift == 0)
			    act_result[j] = opnd & mask;
			else if (shift == size)
			    act_result[j] = 0;
			else
			    act_result[j] = (opnd >> shift) & mask;

			act_status[j] =   is_zero8(act_result[j])
					? AR_STAT_ZERO
					: AR_STAT_OK;
			if (is_neg8(act_result[j]))
			    act_status[j] |= AR_STAT_NEGATIVE;
		    }

		    ZERO_INT8_UPPER(&shift_data);
		    shift_data.ar_i8.part5 = shift & 0xFF;

		    test_status[j] = AR_ibits((AR_DATA *)&test_result[j],
					      &sint8_artype,
					      (AR_DATA *)&cool_ints[i],
					      &sint8_artype,
					      (AR_DATA *)&shift_data,
					      &sint8_artype,
					      (AR_DATA *)&cool_ints[j],
					      &sint8_artype);
		    break;

		case 16:
		    if (act_status[j] == AR_STAT_OK)
		    {
			opnd = LU16(&cool_ints[i]);

			if (shift == 0)
			    act_result[j] = opnd & mask;
			else if (shift == size)
			    act_result[j] = 0;
			else
			    act_result[j] = (opnd >> shift) & mask;

			act_status[j] =   is_zero16(act_result[j])
					? AR_STAT_ZERO
					: AR_STAT_OK;
			if (is_neg16(act_result[j]))
			    act_status[j] |= AR_STAT_NEGATIVE;
		    }

		    ZERO_INT16_UPPER(&shift_data);
		    shift_data.ar_i64.part4 = shift & 0xFFFF;

		    test_status[j] = AR_ibits((AR_DATA *)&test_result[j],
					      &sint16_artype,
					      (AR_DATA *)&cool_ints[i],
					      &sint16_artype,
					      (AR_DATA *)&shift_data,
					      &sint16_artype,
					      (AR_DATA *)&cool_ints[j],
					      &sint16_artype);
		    break;

		case 32:
		    if (act_status[j] == AR_STAT_OK)
		    {
			opnd = LU32(&cool_ints[i]);

			if (shift == 0)
			    act_result[j] = opnd & mask;
			else if (shift == size)
			    act_result[j] = 0;
			else
			    act_result[j] = (opnd >> shift) & mask;

			act_status[j] =   is_zero32(act_result[j])
					? AR_STAT_ZERO
					: AR_STAT_OK;
			if (is_neg32(act_result[j]))
			    act_status[j] |= AR_STAT_NEGATIVE;
		    }

		    ZERO_INT32_UPPER(&shift_data);
		    shift_data.ar_i64.part3 = (shift >> 16) & 0xFFFF;
		    shift_data.ar_i64.part4 = (shift      ) & 0xFFFF;

		    test_status[j] = AR_ibits((AR_DATA *)&test_result[j],
					      &sint32_artype,
					      (AR_DATA *)&cool_ints[i],
					      &sint32_artype,
					      (AR_DATA *)&shift_data,
					      &sint32_artype,
					      (AR_DATA *)&cool_ints[j],
					      &sint32_artype);
		    break;

		case 46:
		case 64:
		    if (act_status[j] == AR_STAT_OK)
		    {
			opnd = LU64(&cool_ints[i]);

			if (shift == 0)
			    act_result[j] = opnd & mask;
			else if (shift == size)
			    act_result[j] = 0;
			else
#if defined __sun
			    act_result[j] = shftr64(opnd, shift) & mask;
#else
			    act_result[j] = (opnd >> shift) & mask;
#endif

			act_status[j] =   is_zero64(act_result[j])
					? AR_STAT_ZERO
					: AR_STAT_OK;
			if (is_neg64(act_result[j]))
			    act_status[j] |= AR_STAT_NEGATIVE;
		    }

		    shift_data.ar_i64.part1 = (shift >> 48) & 0xFFFF;
		    shift_data.ar_i64.part2 = (shift >> 32) & 0xFFFF;
		    shift_data.ar_i64.part3 = (shift >> 16) & 0xFFFF;
		    shift_data.ar_i64.part4 = (shift      ) & 0xFFFF;

		    test_status[j] = AR_ibits((AR_DATA *)&test_result[j],
					      &sint64_artype,
					      (AR_DATA *)&cool_ints[i],
					      &sint64_artype,
					      (AR_DATA *)&shift_data,
					      &sint64_artype,
					      (AR_DATA *)&cool_ints[j],
					      &sint64_artype);
		    break;
		}

		if (LU64(&test_result[j]) != (UINT64)act_result[j])
		{
		    printf(" ERROR:  incorrect result at %d,%d,%d\n",
			   i, j, (int) shift);
		    abort();
		}

		if (act_status[j] != test_status[j])
		{
		    printf(" ERROR:  incorrect status at %d,%d,%d\n",
			   i, j, (int) shift);
		    abort();
		}
	    }
	}
    }

    printf("PASSED\n");
}


#define TEST_IBITS_TYPES						\
   do {									\
      int i, j, k, l;							\
									\
      printf(" Testing:           types  %-10s: ", "IBITS");		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       for (l=0; l<num_ar_types; l++)				\
	       {							\
		  act_status[l] = AR_STAT_OK;				\
		  if (ar_types[i] != ar_types[j] ||			\
		      !IS_SUPPORTED_INT_TYPE(ar_types[i]) ||		\
		      !IS_SUPPORTED_INT_TYPE(ar_types[k]) ||		\
		      !IS_SUPPORTED_INT_TYPE(ar_types[l]))		\
		     act_status[l] = AR_STAT_INVALID_TYPE;		\
									\
		  test_status[l] = AR_ibits((AR_DATA *)&test_result[1],\
					     &ar_types[i],		\
					     (AR_DATA *)&cool_ints[1],	\
					     &ar_types[j],		\
					     (AR_DATA *)&cool_ints[1],	\
					     &ar_types[l],		\
					     (AR_DATA *)&cool_ints[1],	\
					     &ar_types[k]);		\
									\
		  if ((act_status[l] ^ test_status[l]) &		\
		      AR_STAT_INVALID_TYPE)				\
		  {							\
		     printf(" ERROR:  incorrect status at %d,%d,%d,%d"	\
			    "\n",					\
			    i, j, k, l);				\
		     abort();						\
		  }							\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_ibits()
{

   test_ibits_signed( 8);

   test_ibits_signed(16);

   test_ibits_signed(32);

   test_ibits_signed(46);

   test_ibits_signed(64);

   TEST_IBITS_TYPES;

}  /* test_ibits */


#undef TEST_IBITS_TYPES


/*======================================================================
 *
 *	MASK/LEADZ/POP OPERATORS
 *
 */

#define TEST_MASK_OP_SIGNED(size, max_shift)				\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "_mask");	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 if (LS##size(&cool_ints[j]) > max_shift ||			\
	     LS##size(&cool_ints[j]) < 0)				\
	 {  /* status is undefined, result is zero */			\
	    act_result[j] = 0;						\
	    act_status[j] = AR_STAT_UNDEFINED;				\
	 }								\
	 else								\
	 {								\
	    act_result[j] = DSEXT##size(				\
			       _mask##size(LS##size(&cool_ints[j])));	\
	    act_status[j] = is_zero##size(act_result[j])		\
			    ? AR_STAT_ZERO : AR_STAT_OK;		\
	    if (is_neg##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_NEGATIVE;			\
	 }								\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = AR_mask((AR_DATA *)&test_result[j],		\
				  &sint##size##_artype,			\
				  (AR_DATA *)&cool_ints[j],		\
				  &sint##size##_artype);		\
									\
	 if (LU64(&test_result[j]) != act_result[j])			\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_MASK_OP_UNSIGNED(size, max_shift)				\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, "_mask");	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 if (LU##size(&cool_ints[j]) > max_shift)			\
	 {  /* status is undefined, result is zero */			\
	    act_result[j] = 0;						\
	    act_status[j] = AR_STAT_UNDEFINED;				\
	 }								\
	 else								\
	 {								\
	    act_result[j] = DSEXT##size(				\
			       _mask##size(LU##size(&cool_ints[j])));	\
	    act_status[j] = is_zero##size(act_result[j])		\
			    ? AR_STAT_ZERO : AR_STAT_OK;		\
	 }								\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = AR_mask((AR_DATA *)&test_result[j],		\
				  &uint##size##_artype,			\
				  (AR_DATA *)&cool_ints[j],		\
				  &uint##size##_artype);		\
									\
	 if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_MASK_OP_TYPES						\
   do {									\
      int j, k;								\
									\
      printf(" Testing:           types  %-10s: ", "_mask");		\
									\
      for (j=0; j<num_ar_types; j++)					\
      {									\
	 for (k=0; k<num_ar_types; k++)					\
	 {								\
	    act_status[k] = AR_STAT_OK;					\
	    if (ar_types[j] != ar_types[k] ||				\
		!IS_SUPPORTED_INT_TYPE(ar_types[j]))			\
	       act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	    test_status[k] = AR_mask((AR_DATA *)&test_result[1],	\
				     &ar_types[j],			\
				     (AR_DATA *)&cool_ints[1],		\
				     &ar_types[k]);			\
									\
	    if ((act_status[k] ^ test_status[k]) &			\
		AR_STAT_INVALID_TYPE)					\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", j, k);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_LP_OP_SIGNED(size, opr, func)				\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_result[j] = opr##size(LS##size(&cool_ints[j]));		\
									\
	 act_status[j] = is_zero##size(act_result[j])			\
			 ? AR_STAT_ZERO : AR_STAT_OK;			\
	 if (is_neg##size(act_result[j]))				\
	    act_status[j] |= AR_STAT_NEGATIVE;				\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = func((AR_DATA *)&test_result[j],		\
			       &sint##size##_artype,			\
			       (AR_DATA *)&cool_ints[j],		\
			       &sint##size##_artype);			\
									\
	 if (LU64(&test_result[j]) != act_result[j])			\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_LP_OP_UNSIGNED(size, opr, func)				\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_result[j] = opr##size(LU##size(&cool_ints[j]));		\
	 act_status[j] = is_zero##size(act_result[j])			\
			 ? AR_STAT_ZERO : AR_STAT_OK;			\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = func((AR_DATA *)&test_result[j],		\
			       &uint##size##_artype,			\
			       (AR_DATA *)&cool_ints[j],		\
			       &uint##size##_artype);			\
									\
	 if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_LP_OP_TYPES(opr, func)					\
   do {									\
      int j, k;								\
									\
      printf(" Testing:           types  %-10s: ", #opr);		\
									\
      for (j=0; j<num_ar_types; j++)					\
      {									\
	 for (k=0; k<num_ar_types; k++)					\
	 {								\
	    act_status[k] = AR_STAT_OK;					\
	    if (!IS_SUPPORTED_INT_TYPE(ar_types[j]) ||			\
		!IS_SUPPORTED_INT_TYPE(ar_types[k]))			\
	       act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	    test_status[k] = func((AR_DATA *)&test_result[1],		\
				  &ar_types[j],				\
				  (AR_DATA *)&cool_ints[1],		\
				  &ar_types[k]);			\
									\
	    if ((act_status[k] ^ test_status[k]) &			\
		AR_STAT_INVALID_TYPE)					\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", j, k);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_mask()
{

   TEST_MASK_OP_SIGNED(   8,  16);
   TEST_MASK_OP_UNSIGNED( 8,  16);

   TEST_MASK_OP_SIGNED(  16,  32);
   TEST_MASK_OP_UNSIGNED(16,  32);

   TEST_MASK_OP_SIGNED(  32,  64);
   TEST_MASK_OP_UNSIGNED(32,  64);

   TEST_MASK_OP_SIGNED(  46, 128);

   TEST_MASK_OP_SIGNED(  64, 128);
   TEST_MASK_OP_UNSIGNED(64, 128);

   TEST_MASK_OP_TYPES;

}  /* test_mask */


test_leadz()
{

   TEST_LP_OP_SIGNED(   8, _leadz , AR_leadz );
   TEST_LP_OP_UNSIGNED( 8, _leadz , AR_leadz );

   TEST_LP_OP_SIGNED(  16, _leadz , AR_leadz );
   TEST_LP_OP_UNSIGNED(16, _leadz , AR_leadz );

   TEST_LP_OP_SIGNED(  32, _leadz , AR_leadz );
   TEST_LP_OP_UNSIGNED(32, _leadz , AR_leadz );

   TEST_LP_OP_SIGNED(  46, _leadz , AR_leadz );

   TEST_LP_OP_SIGNED(  64, _leadz , AR_leadz );
   TEST_LP_OP_UNSIGNED(64, _leadz , AR_leadz );

   TEST_LP_OP_TYPES(       _leadz , AR_leadz );

}  /* test_leadz */


test_popcnt()
{

   TEST_LP_OP_SIGNED(   8, _popcnt, AR_popcnt);
   TEST_LP_OP_UNSIGNED( 8, _popcnt, AR_popcnt);

   TEST_LP_OP_SIGNED(  16, _popcnt, AR_popcnt);
   TEST_LP_OP_UNSIGNED(16, _popcnt, AR_popcnt);

   TEST_LP_OP_SIGNED(  32, _popcnt, AR_popcnt);
   TEST_LP_OP_UNSIGNED(32, _popcnt, AR_popcnt);

   TEST_LP_OP_SIGNED(  46, _popcnt, AR_popcnt);

   TEST_LP_OP_SIGNED(  64, _popcnt, AR_popcnt);
   TEST_LP_OP_UNSIGNED(64, _popcnt, AR_popcnt);

   TEST_LP_OP_TYPES(       _popcnt, AR_popcnt);

}  /* test_popcnt */


test_poppar()
{

   TEST_LP_OP_SIGNED(   8, _poppar, AR_poppar);
   TEST_LP_OP_UNSIGNED( 8, _poppar, AR_poppar);

   TEST_LP_OP_SIGNED(  16, _poppar, AR_poppar);
   TEST_LP_OP_UNSIGNED(16, _poppar, AR_poppar);

   TEST_LP_OP_SIGNED(  32, _poppar, AR_poppar);
   TEST_LP_OP_UNSIGNED(32, _poppar, AR_poppar);

   TEST_LP_OP_SIGNED(  46, _poppar, AR_poppar);

   TEST_LP_OP_SIGNED(  64, _poppar, AR_poppar);
   TEST_LP_OP_UNSIGNED(64, _poppar, AR_poppar);

   TEST_LP_OP_TYPES(       _poppar, AR_poppar);

}  /* test_poppar */


#undef TEST_MASK_SIGNED
#undef TEST_MASK_UNSIGNED
#undef TEST_MASK_TYPES
#undef TEST_LP_OP_SIGNED
#undef TEST_LP_OP_UNSIGNED
#undef TEST_LP_OP_TYPES


/*======================================================================
 *
 *	STATUS
 *
 */

#define TEST_STATUS_SIGNED(size)					\
   do {									\
      int j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "status");	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_status[j] = is_zero##size(					\
			    DSEXT##size(LS##size(&cool_ints[j])))	\
			 ? AR_STAT_ZERO : AR_STAT_OK;			\
									\
	 if (is_neg##size(LS##size(&cool_ints[j])))			\
	    act_status[j] |= AR_STAT_NEGATIVE;				\
									\
	 test_status[j] = AR_status((AR_DATA *)&cool_ints[j],		\
				    &sint##size##_artype);		\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_STATUS_UNSIGNED(size)					\
   do {									\
      int j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "status");	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_status[j] = is_zero##size(LU##size(&cool_ints[j]))		\
			 ? AR_STAT_ZERO : AR_STAT_OK;			\
									\
	 test_status[j] = AR_status((AR_DATA *)&cool_ints[j],		\
				    &uint##size##_artype);		\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_STATUS_TYPES						\
   do {									\
      int k;								\
									\
      printf(" Testing:           types  %-10s: ", "status");		\
									\
      for (k=0; k<num_ar_types; k++)					\
      {									\
	 test_status[k] = AR_status((AR_DATA *)&cool_ints[1],		\
				    &ar_types[k]);			\
									\
	 if (test_status[k] & AR_STAT_INVALID_TYPE)			\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", k);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_arstatus()
{

   TEST_STATUS_SIGNED(   8);
   TEST_STATUS_UNSIGNED( 8);

   TEST_STATUS_SIGNED(  16);
   TEST_STATUS_UNSIGNED(16);

   TEST_STATUS_SIGNED(  32);
   TEST_STATUS_UNSIGNED(32);

   TEST_STATUS_SIGNED(  46);

   TEST_STATUS_SIGNED(  64);
   TEST_STATUS_UNSIGNED(64);

   TEST_STATUS_TYPES;

}  /* test_arstatus */


#undef TEST_STATUS_SIGNED
#undef TEST_STATUS_UNSIGNED
#undef TEST_STATUS_TYPES


/*======================================================================
 *
 *	ARITHMETIC
 *
 */


#define TEST_ADDSUB_OP_SIGNED(size, opr, func)				\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_result[j] = DSEXT##size(LS##size(&cool_ints[i]) opr	\
					LS##size(&cool_ints[j]));	\
									\
	    act_status[j] = is_zero##size(act_result[j])		\
			    ? AR_STAT_ZERO : AR_STAT_OK;		\
	    if (is_neg##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_NEGATIVE;			\
									\
	    /* Overflow check. */					\
	    if ((strcmp(#opr, "+") == 0                   &&		\
		 is_neg##size(LS##size(&cool_ints[i])) ==		\
		 is_neg##size(LS##size(&cool_ints[j]))    &&		\
		 is_neg##size(LS##size(&cool_ints[i])) !=		\
		 is_neg##size(act_result[j]))                ||		\
		(strcmp(#opr, "-") == 0                   &&		\
		 is_neg##size(LS##size(&cool_ints[i])) !=		\
		 is_neg##size(LS##size(&cool_ints[j]))    &&		\
		 is_neg##size(LS##size(&cool_ints[i])) !=		\
		 is_neg##size(act_result[j])))				\
	    {								\
	       act_status[j] |= AR_STAT_OVERFLOW;			\
	    }								\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = func((AR_DATA *)&test_result[j],		\
				  &sint##size##_artype,			\
				  (AR_DATA *)&cool_ints[i],		\
				  &sint##size##_artype,			\
				  (AR_DATA *)&cool_ints[j],		\
				  &sint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != act_result[j])			\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_ADDSUB_OP_UNSIGNED(size, opr, func)			\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, #opr);	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_result[j] = DSEXT##size(LU##size(&cool_ints[i]) opr	\
					LU##size(&cool_ints[j]));	\
									\
	    act_status[j] = is_zero##size(act_result[j])		\
			    ? AR_STAT_ZERO : AR_STAT_OK;		\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = func((AR_DATA *)&test_result[j],		\
				  &uint##size##_artype,			\
				  (AR_DATA *)&cool_ints[i],		\
				  &uint##size##_artype,			\
				  (AR_DATA *)&cool_ints[j],		\
				  &uint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_ADDSUB_OP_TYPES(opr, func)					\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing:           types  %-10s: ", #opr);		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       act_status[k] = AR_STAT_OK;				\
	       if (ar_types[i] != ar_types[j] ||			\
		   ar_types[j] != ar_types[k] ||			\
		   !IS_SUPPORTED_INT_TYPE(ar_types[i]))			\
		  act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	       test_status[k] = func((AR_DATA *)&test_result[1],	\
				     &ar_types[i],			\
				     (AR_DATA *)&cool_ints[1],		\
				     &ar_types[j],			\
				     (AR_DATA *)&cool_ints[1],		\
				     &ar_types[k]);			\
									\
	       if ((act_status[k] ^ test_status[k]) &			\
		   AR_STAT_INVALID_TYPE)				\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_add()
{

   TEST_ADDSUB_OP_SIGNED(   8, +, AR_add);
   TEST_ADDSUB_OP_UNSIGNED( 8, +, AR_add);

   TEST_ADDSUB_OP_SIGNED(  16, +, AR_add);
   TEST_ADDSUB_OP_UNSIGNED(16, +, AR_add);

   TEST_ADDSUB_OP_SIGNED(  32, +, AR_add);
   TEST_ADDSUB_OP_UNSIGNED(32, +, AR_add);

   TEST_ADDSUB_OP_SIGNED(  46, +, AR_add);

   TEST_ADDSUB_OP_SIGNED(  64, +, AR_add);
   TEST_ADDSUB_OP_UNSIGNED(64, +, AR_add);

   TEST_ADDSUB_OP_TYPES(       +, AR_add);

}  /* test_add */


test_subtract()
{

   TEST_ADDSUB_OP_SIGNED(   8, -, AR_subtract);
   TEST_ADDSUB_OP_UNSIGNED( 8, -, AR_subtract);

   TEST_ADDSUB_OP_SIGNED(  16, -, AR_subtract);
   TEST_ADDSUB_OP_UNSIGNED(16, -, AR_subtract);

   TEST_ADDSUB_OP_SIGNED(  32, -, AR_subtract);
   TEST_ADDSUB_OP_UNSIGNED(32, -, AR_subtract);

   TEST_ADDSUB_OP_SIGNED(  46, -, AR_subtract);

   TEST_ADDSUB_OP_SIGNED(  64, -, AR_subtract);
   TEST_ADDSUB_OP_UNSIGNED(64, -, AR_subtract);

   TEST_ADDSUB_OP_TYPES(       -, AR_subtract);

}  /* test_subtract */


#undef ADDSUB_OP_SIGNED
#undef ADDSUB_OP_UNSIGNED
#undef ADDSUB_OP_TYPES


#define TEST_NEG_OP_SIGNED(size)					\
   do {									\
      int j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "unary -");	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_result[j] = DSEXT##size(- LS##size(&cool_ints[j]));	\
									\
	 if (is_zero##size(act_result[j]))				\
	 {  /* result is zero */					\
	    act_status[j] = AR_STAT_ZERO;				\
	 }								\
	 else								\
	 {  /* result is non-zero */					\
	    if (is_neg##size(LS##size(&cool_ints[j])) ==		\
		is_neg##size(act_result[j]))				\
	    {  /* The sign bit of the operand and the result match.	\
		  Overflow except negate of maximum negative int.  */	\
	       if(LS##size(&cool_ints[j]) !=				\
		  LS##size((AR_INT_64*)&act_result[j]))			\
		  act_status[j] = AR_STAT_OVERFLOW;			\
	       else							\
		  act_status[j] = AR_STAT_SEMIVALID;			\
	    }								\
	    else							\
	    {  /* No overflow.  */					\
	       act_status[j] = AR_STAT_OK;				\
	    }								\
	 }								\
									\
	 if (is_neg##size(act_result[j]))				\
	    act_status[j] |= AR_STAT_NEGATIVE;				\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = AR_negate((AR_DATA *)&test_result[j],		\
				    &sint##size##_artype,		\
				    (AR_DATA *)&cool_ints[j],		\
				    &sint##size##_artype);		\
									\
	 if (LU64(&test_result[j]) != act_result[j])			\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_NEG_OP_UNSIGNED(size)					\
   do {									\
      int j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, "unary -");	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_result[j] = DSEXT##size(- LU##size(&cool_ints[j]));	\
									\
	 act_status[j] = is_zero##size(act_result[j])			\
			 ? AR_STAT_ZERO : AR_STAT_OK;			\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = AR_negate((AR_DATA *)&test_result[j],		\
				    &uint##size##_artype,		\
				    (AR_DATA *)&cool_ints[j],		\
				    &uint##size##_artype);		\
									\
	 if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_NEG_OP_TYPES						\
   do {									\
      int j, k;								\
									\
      printf(" Testing:           types  %-10s: ", "unary -");		\
									\
      for (j=0; j<num_ar_types; j++)					\
      {									\
	 for (k=0; k<num_ar_types; k++)					\
	 {								\
	    act_status[k] = AR_STAT_OK;					\
	    if (ar_types[j] != ar_types[k] ||				\
		!IS_SUPPORTED_INT_TYPE(ar_types[j]))			\
	       act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	    test_status[k] = AR_negate((AR_DATA *)&test_result[1],	\
				       &ar_types[j],			\
				       (AR_DATA *)&cool_ints[1],	\
				       &ar_types[k]);			\
									\
	    if ((act_status[k] ^ test_status[k]) &			\
		AR_STAT_INVALID_TYPE)					\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", j, k);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_negate()
{

   TEST_NEG_OP_SIGNED(   8);
   TEST_NEG_OP_UNSIGNED( 8);

   TEST_NEG_OP_SIGNED(  16);
   TEST_NEG_OP_UNSIGNED(16);

   TEST_NEG_OP_SIGNED(  32);
   TEST_NEG_OP_UNSIGNED(32);

   TEST_NEG_OP_SIGNED(  46);

   TEST_NEG_OP_SIGNED(  64);
   TEST_NEG_OP_UNSIGNED(64);

   TEST_NEG_OP_TYPES;

}  /* test_negate */


#undef TEST_NEG_SIGNED
#undef TEST_NEG_UNSIGNED
#undef TEST_NEG_TYPES


#define TEST_ABS_OP_SIGNED(size)					\
   do {									\
      int j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "abs");	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 if (is_neg##size(LS##size(&cool_ints[j])))			\
	    act_result[j] = DSEXT##size( - LS##size(&cool_ints[j]));	\
	 else								\
	    act_result[j] = DSEXT##size(   LS##size(&cool_ints[j]));	\
									\
	 act_status[j] = is_zero##size(act_result[j])			\
			 ? AR_STAT_ZERO : AR_STAT_OK;			\
									\
	 if (is_neg##size(act_result[j]))				\
	 {  /* The result can be negative only if it overflows.  */	\
	    act_status[j] |= AR_STAT_NEGATIVE;				\
	    act_status[j] |= AR_STAT_OVERFLOW;				\
	 }								\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = AR_abs((AR_DATA *)&test_result[j],		\
				 &sint##size##_artype,			\
				 (AR_DATA *)&cool_ints[j],		\
				 &sint##size##_artype);			\
									\
	 if (LU64(&test_result[j]) != act_result[j])			\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_ABS_OP_UNSIGNED(size)					\
   do {									\
      int j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, "abs");	\
									\
      for (j=0; j<num_cool_ints; j++)					\
      {									\
	 act_result[j] = DSEXT##size(LU64(&cool_ints[j]));		\
									\
	 act_status[j] = is_zero##size(act_result[j])			\
			 ? AR_STAT_ZERO : AR_STAT_OK;			\
									\
	 test_result[j].part1 = 0;					\
	 test_result[j].part2 = 0;					\
	 test_result[j].part3 = 0;					\
	 test_result[j].part4 = 0;					\
									\
	 test_status[j] = AR_abs((AR_DATA *)&test_result[j],		\
				 &uint##size##_artype,			\
				 (AR_DATA *)&cool_ints[j],		\
				 &uint##size##_artype);			\
									\
	 if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	 {								\
	    printf(" ERROR:  incorrect result at %d\n", j);		\
	    abort();							\
	 }								\
									\
	 if (act_status[j] != test_status[j])				\
	 {								\
	    printf(" ERROR:  incorrect status at %d\n", j);		\
	    abort();							\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_ABS_OP_TYPES						\
   do {									\
      int j, k;								\
									\
      printf(" Testing:           types  %-10s: ", "abs");		\
									\
      for (j=0; j<num_ar_types; j++)					\
      {									\
	 for (k=0; k<num_ar_types; k++)					\
	 {								\
	    act_status[k] = AR_STAT_OK;					\
	    if (ar_types[j] != ar_types[k] ||				\
		!IS_SUPPORTED_INT_TYPE(ar_types[j]))			\
	       act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	    test_status[k] = AR_abs((AR_DATA *)&test_result[1],		\
				    &ar_types[j],			\
				    (AR_DATA *)&cool_ints[1],		\
				    &ar_types[k]);			\
									\
	    if ((act_status[k] ^ test_status[k]) &			\
		AR_STAT_INVALID_TYPE)					\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", j, k);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_abs()
{

   TEST_ABS_OP_SIGNED(   8);
   TEST_ABS_OP_UNSIGNED( 8);

   TEST_ABS_OP_SIGNED(  16);
   TEST_ABS_OP_UNSIGNED(16);

   TEST_ABS_OP_SIGNED(  32);
   TEST_ABS_OP_UNSIGNED(32);

   TEST_ABS_OP_SIGNED(  46);

   TEST_ABS_OP_SIGNED(  64);
   TEST_ABS_OP_UNSIGNED(64);

   TEST_ABS_OP_TYPES;

}  /* test_abs */


#undef TEST_ABS_OP_SIGNED
#undef TEST_ABS_OP_UNSIGNED
#undef TEST_ABS_OP_TYPES


#define TEST_MUL_OP_SIGNED(size)					\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "*");		\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    if (size == 46)						\
	       act_result[j] = (SINT64)(LS64(&cool_ints[i]) *		\
					LS64(&cool_ints[j]));		\
	    else							\
	       act_result[j] = DSEXT##size(LS##size(&cool_ints[i]) *	\
					   LS##size(&cool_ints[j]));	\
									\
	    act_status[j] = is_zero##size(act_result[j])		\
			    ? AR_STAT_ZERO : AR_STAT_OK;		\
	    if (is_neg##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_NEGATIVE;			\
									\
	    /* Overflow checks. */					\
	    if (size == 46)						\
	    {								\
	       if (((LS64(&cool_ints[i]) & ((~((SINT64) 0)) << 45))	\
		    != 0) &&						\
		   ((LS64(&cool_ints[i]) & ((~((SINT64) 0)) << 45))	\
		    != ((~((SINT64) 0)) << 45)))			\
	       {  /* first operand too big to be represented as a	\
		     46-bit int */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	       else							\
	       if (((LS64(&cool_ints[j]) & ((~((SINT64) 0)) << 45))	\
		    != 0) &&						\
		   ((LS64(&cool_ints[j]) & ((~((SINT64) 0)) << 45))	\
		    != ((~((SINT64) 0)) << 45)))			\
	       {  /* second operand too big to be represented as a	\
		     46-bit int */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	       else							\
	       if (((act_result[j] & ((~((SINT64) 0)) << 45))		\
		    != 0) &&						\
		   ((act_result[j] & ((~((SINT64) 0)) << 45))		\
		    != ((~((SINT64) 0)) << 45)))			\
	       {  /* result too big to be represented as a		\
		     46-bit int */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	    }								\
									\
	    if (LS##size(&cool_ints[i]) != 0 &&				\
		LS##size(&cool_ints[j]) != 0)				\
	    {  /* Check for overflow if neither operand is zero.	\
		  (If either is zero, an overflow can not occur		\
		  and dealing with zeros in the checks below		\
		  causes headaches.)  */				\
	       if (is_neg##size(LS##size(&cool_ints[i]) ^		\
				LS##size(&cool_ints[j]) ^		\
				act_result[j]))				\
	       {  /* The sign of the result does not match the XOR	\
		     of the signs of the two operands. Overflow.  */	\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	       else if ((SEXT##size(act_result[j]) /			\
			 LS##size(&cool_ints[i])) !=			\
			LS##size(&cool_ints[j]))			\
	       {  /* The above test does not catch all overflows.	\
		     This test catches all remaining overflows by	\
		     checking to see that the result divided by the	\
		     the first operand equals the second operand.  */	\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	    }								\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = AR_multiply((AR_DATA *)&test_result[j],	\
					 &sint##size##_artype,		\
					 (AR_DATA *)&cool_ints[i],	\
					 &sint##size##_artype,		\
					 (AR_DATA *)&cool_ints[j],	\
					 &sint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != act_result[j])			\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_MUL_OP_UNSIGNED(size)					\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, "*");		\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_result[j] = DSEXT##size(LU##size(&cool_ints[i]) *	\
					LU##size(&cool_ints[j]));	\
									\
	    act_status[j] = is_zero##size(act_result[j])		\
			    ? AR_STAT_ZERO : AR_STAT_OK;		\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = AR_multiply((AR_DATA *)&test_result[j],	\
					 &uint##size##_artype,		\
					 (AR_DATA *)&cool_ints[i],	\
					 &uint##size##_artype,		\
					 (AR_DATA *)&cool_ints[j],	\
					 &uint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_MUL_OP_TYPES						\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing:           types  %-10s: ", "*");		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       act_status[k] = AR_STAT_OK;				\
	       if (ar_types[i] != ar_types[j] ||			\
		   ar_types[j] != ar_types[k] ||			\
		   !IS_SUPPORTED_INT_TYPE(ar_types[i]))			\
		  act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	       test_status[k] = AR_multiply((AR_DATA *)&test_result[1],	\
					    &ar_types[i],		\
					    (AR_DATA *)&cool_ints[1],	\
					    &ar_types[j],		\
					    (AR_DATA *)&cool_ints[1],	\
					    &ar_types[k]);		\
									\
	       if ((act_status[k] ^ test_status[k]) &			\
		   AR_STAT_INVALID_TYPE)				\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_multiply()
{

   TEST_MUL_OP_SIGNED(   8);
   TEST_MUL_OP_UNSIGNED( 8);

   TEST_MUL_OP_SIGNED(  16);
   TEST_MUL_OP_UNSIGNED(16);

   TEST_MUL_OP_SIGNED(  32);
   TEST_MUL_OP_UNSIGNED(32);

   TEST_MUL_OP_SIGNED(  46);

   TEST_MUL_OP_SIGNED(  64);
   TEST_MUL_OP_UNSIGNED(64);

   TEST_MUL_OP_TYPES;

}  /* test_multiply */


#undef TEST_MUL_OP_SIGNED
#undef TEST_MUL_OP_UNSIGNED
#undef TEST_MUL_OP_TYPES


#define TEST_DIV_OP_SIGNED(size)					\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "/");		\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_status[j] = AR_STAT_OK;					\
									\
	    if (LS##size(&cool_ints[j]) == 0)				\
	    {  /* divide by zero */					\
	       act_result[j] = 0;					\
	       act_status[j] |= AR_STAT_OVERFLOW;			\
	    }								\
	    else if (size == 46)					\
	    {  /* 46-bit divide      */					\
	       /* not divide by zero */					\
	       /* Perform division using 64-bit precision to avoid	\
		  "fastmd" problems.  */				\
	       act_result[j] = (SINT64)(LS64(&cool_ints[i]) /		\
					LS64(&cool_ints[j]));		\
									\
	       if (((LS64(&cool_ints[i]) & ((~((SINT64) 0)) << 45))	\
		    != 0) &&						\
		   ((LS64(&cool_ints[i]) & ((~((SINT64) 0)) << 45))	\
		    != ((~((SINT64) 0)) << 45)))			\
	       {  /* first operand too big to be represented as a	\
		     46-bit int */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	       else if (((LS64(&cool_ints[j]) & ((~((SINT64) 0)) << 45))\
			 != 0) &&					\
			((LS64(&cool_ints[j]) & ((~((SINT64) 0)) << 45))\
			 != ((~((SINT64) 0)) << 45)))			\
	       {  /* second operand too big to be represented as a	\
		     46-bit int */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	       else if (((act_result[j] & ((~((SINT64) 0)) << 45))	\
			 != 0) &&					\
			((act_result[j] & ((~((SINT64) 0)) << 45))	\
			 != ((~((SINT64) 0)) << 45)))			\
	       {  /* result too big to be represented as a		\
		     46-bit int */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	       else if (!is_zero##size(act_result [j]) &&		\
			(LS64(&cool_ints[i]) ^				\
			 LS64(&cool_ints[j]) ^				\
			 act_result[j]) < 0)				\
	       {  /* Overflow.  */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	    }								\
	    else							\
	    {  /* not 46-bit divide  */					\
	       /* not divide by zero */					\
	       act_result[j] = DSEXT##size(LS##size(&cool_ints[i]) /	\
					   LS##size(&cool_ints[j]));	\
									\
	       if (!is_zero##size(act_result[j]) &&			\
		   is_neg##size(LS##size(&cool_ints[i]) ^		\
				LS##size(&cool_ints[j]) ^		\
				act_result[j]))				\
	       {  /* Overflow.  */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	    }								\
									\
	    if (is_zero##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_ZERO;				\
									\
	    if (is_neg##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_NEGATIVE;			\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = AR_divide((AR_DATA *)&test_result[j],	\
				       &sint##size##_artype,		\
				       (AR_DATA *)&cool_ints[i],	\
				       &sint##size##_artype,		\
				       (AR_DATA *)&cool_ints[j],	\
				       &sint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != act_result[j])			\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_DIV_OP_UNSIGNED(size)					\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, "/");		\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_status[j] = AR_STAT_OK;					\
									\
	    if (LU##size(&cool_ints[j]) == 0)				\
	    {  /* divide by zero */					\
	       act_result[j] = 0;					\
	       act_status[j] |= AR_STAT_OVERFLOW;			\
	    }								\
	    else							\
	    {  /* not divide by zero */					\
	       act_result[j] = LU##size(&cool_ints[i]) /		\
			       LU##size(&cool_ints[j]);			\
	    }								\
									\
	    if (is_zero##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_ZERO;				\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = AR_divide((AR_DATA *)&test_result[j],	\
				       &uint##size##_artype,		\
				       (AR_DATA *)&cool_ints[i],	\
				       &uint##size##_artype,		\
				       (AR_DATA *)&cool_ints[j],	\
				       &uint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_DIV_OP_TYPES						\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing:           types  %-10s: ", "/");		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       act_status[k] = AR_STAT_OK;				\
	       if (ar_types[i] != ar_types[j] ||			\
		   ar_types[j] != ar_types[k] ||			\
		   !IS_SUPPORTED_INT_TYPE(ar_types[i]))			\
		  act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	       test_status[k] = AR_divide((AR_DATA *)&test_result[1],	\
					  &ar_types[i],			\
					  (AR_DATA *)&cool_ints[1],	\
					  &ar_types[j],			\
					  (AR_DATA *)&cool_ints[1],	\
					  &ar_types[k]);		\
									\
	       if ((act_status[k] ^ test_status[k]) &			\
		   AR_STAT_INVALID_TYPE)				\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_divide()
{
   TEST_DIV_OP_SIGNED(   8);
   TEST_DIV_OP_UNSIGNED( 8);

   TEST_DIV_OP_SIGNED(  16);
   TEST_DIV_OP_UNSIGNED(16);

   TEST_DIV_OP_SIGNED(  32);
   TEST_DIV_OP_UNSIGNED(32);

   TEST_DIV_OP_SIGNED(  46);

   TEST_DIV_OP_SIGNED(  64);
   TEST_DIV_OP_UNSIGNED(64);

   TEST_DIV_OP_TYPES;

}  /* test_divide */


#undef TEST_DIV_OP_SIGNED
#undef TEST_DIV_OP_UNSIGNED
#undef TEST_DIV_OP_TYPES


#define TEST_MOD_OP_SIGNED(size)					\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "%");		\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_status[j] = AR_STAT_OK;					\
									\
	    if (LS##size(&cool_ints[j]) == 0)				\
	    {  /* modulo by zero */					\
	       act_result[j] = 0;					\
	       act_status[j] |= AR_STAT_OVERFLOW;			\
	    }								\
	    else							\
	    {  /* not modulo by zero */					\
	       act_result[j] = DSEXT##size(LS##size(&cool_ints[i]) %	\
					   LS##size(&cool_ints[j]));	\
									\
	       if (!is_zero##size(act_result[j]) &&			\
		   is_neg##size(LS##size(&cool_ints[i]) ^		\
				act_result[j]))				\
	       {  /* Overflow.  */					\
		  act_status[j] |= AR_STAT_OVERFLOW;			\
	       }							\
	    }								\
									\
	    if (is_zero##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_ZERO;				\
									\
	    if (is_neg##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_NEGATIVE;			\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = AR_modulo((AR_DATA *)&test_result[j],	\
				       &sint##size##_artype,		\
				       (AR_DATA *)&cool_ints[i],	\
				       &sint##size##_artype,		\
				       (AR_DATA *)&cool_ints[j],	\
				       &sint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != act_result[j])			\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_MOD_OP_UNSIGNED(size)					\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, "%");		\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    act_status[j] = AR_STAT_OK;					\
									\
	    if (LU##size(&cool_ints[j]) == 0)				\
	    {  /* modulo by zero */					\
	       act_result[j] = 0;					\
	       act_status[j] |= AR_STAT_OVERFLOW;			\
	    }								\
	    else							\
	    {  /* not modulo by zero */					\
	       act_result[j] = LU##size(&cool_ints[i]) %		\
			       LU##size(&cool_ints[j]);			\
	    }								\
									\
	    if (is_zero##size(act_result[j]))				\
	       act_status[j] |= AR_STAT_ZERO;				\
									\
	    test_result[j].part1 = 0;					\
	    test_result[j].part2 = 0;					\
	    test_result[j].part3 = 0;					\
	    test_result[j].part4 = 0;					\
									\
	    test_status[j] = AR_modulo((AR_DATA *)&test_result[j],	\
				       &uint##size##_artype,		\
				       (AR_DATA *)&cool_ints[i],	\
				       &uint##size##_artype,		\
				       (AR_DATA *)&cool_ints[j],	\
				       &uint##size##_artype);		\
									\
	    if (LU64(&test_result[j]) != (UINT64)act_result[j])		\
	    {								\
	       printf(" ERROR:  incorrect result at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_MOD_OP_TYPES						\
   do {									\
      int i, j, k;							\
									\
      printf(" Testing:           types  %-10s: ", "%");		\
									\
      for (i=0; i<num_ar_types; i++)					\
      {									\
	 for (j=0; j<num_ar_types; j++)					\
	 {								\
	    for (k=0; k<num_ar_types; k++)				\
	    {								\
	       act_status[k] = AR_STAT_OK;				\
	       if (ar_types[i] != ar_types[j] ||			\
		   ar_types[j] != ar_types[k] ||			\
		   !IS_SUPPORTED_INT_TYPE(ar_types[i]))			\
		  act_status[k] = AR_STAT_INVALID_TYPE;			\
									\
	       test_status[k] = AR_modulo((AR_DATA *)&test_result[1],	\
					  &ar_types[i],			\
					  (AR_DATA *)&cool_ints[1],	\
					  &ar_types[j],			\
					  (AR_DATA *)&cool_ints[1],	\
					  &ar_types[k]);		\
									\
	       if ((act_status[k] ^ test_status[k]) &			\
		   AR_STAT_INVALID_TYPE)				\
	       {							\
		  printf(" ERROR:  incorrect status at %d,%d,%d\n",	\
			 i, j, k);					\
		  abort();						\
	       }							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_modulo()
{

   TEST_MOD_OP_SIGNED(   8);
   TEST_MOD_OP_UNSIGNED( 8);

   TEST_MOD_OP_SIGNED(  16);
   TEST_MOD_OP_UNSIGNED(16);

   TEST_MOD_OP_SIGNED(  32);
   TEST_MOD_OP_UNSIGNED(32);

   TEST_MOD_OP_SIGNED(  46);

   TEST_MOD_OP_SIGNED(  64);
   TEST_MOD_OP_UNSIGNED(64);

   TEST_MOD_OP_TYPES;

}  /* test_modulo */


#undef TEST_MOD_OP_SIGNED
#undef TEST_MOD_OP_UNSIGNED
#undef TEST_MOD_OP_TYPES


/*======================================================================
 *
 *	COMPARISONS
 *
 */


#define TEST_COMPARE_OP_SIGNED(size)					\
   do {									\
      int i, j;								\
									\
      printf(" Testing:   SIGNED %2d INT  %-10s: ", size, "compare");	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    if (size == 46) {						\
	       if (LS64(&cool_ints[i]) <				\
		   LS64(&cool_ints[j]))					\
		  act_status[j] = AR_Compare_LT;			\
	       else if (LS64(&cool_ints[i]) >				\
			LS64(&cool_ints[j]))				\
		  act_status[j] = AR_Compare_GT;			\
	       else							\
		  act_status[j] = AR_Compare_EQ;			\
	    }								\
	    else {							\
	       if (LS##size(&cool_ints[i]) <				\
		   LS##size(&cool_ints[j]))				\
		  act_status[j] = AR_Compare_LT;			\
	       else if (LS##size(&cool_ints[i]) >			\
			LS##size(&cool_ints[j]))			\
		  act_status[j] = AR_Compare_GT;			\
	       else							\
		  act_status[j] = AR_Compare_EQ;			\
	    }								\
									\
	    test_status[j] = AR_compare((AR_DATA *)&cool_ints[i],	\
					&sint##size##_artype,		\
					(AR_DATA *)&cool_ints[j],	\
					&sint##size##_artype);		\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_COMPARE_OP_UNSIGNED(size)					\
   do {									\
      int i, j;								\
									\
      printf(" Testing: UNSIGNED %2d INT  %-10s: ", size, "compare");	\
									\
      for (i=0; i<num_cool_ints; i++)					\
      {									\
	 for (j=0; j<num_cool_ints; j++)				\
	 {								\
	    if (LU##size(&cool_ints[i]) < LU##size(&cool_ints[j]))	\
	       act_status[j] = AR_Compare_LT;				\
	    else if (LU##size(&cool_ints[i]) > LU##size(&cool_ints[j]))	\
	       act_status[j] = AR_Compare_GT;				\
	    else							\
	       act_status[j] = AR_Compare_EQ;				\
									\
	    test_status[j] = AR_compare((AR_DATA *)&cool_ints[i],	\
					&uint##size##_artype,		\
					(AR_DATA *)&cool_ints[j],	\
					&uint##size##_artype);		\
									\
	    if (act_status[j] != test_status[j])			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", i, j);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


#define TEST_COMPARE_OP_TYPES						\
   do {									\
      int j, k;								\
									\
      printf(" Testing:           types  %-10s: ", "compare");		\
									\
      for (j=0; j<num_ar_types; j++)					\
      {									\
	 for (k=0; k<num_ar_types; k++)					\
	 {								\
	    act_status[k] = AR_STAT_OK;					\
	    if (ar_types[j] != ar_types[k] ||				\
		!IS_SUPPORTED_INT_TYPE(ar_types[j]))			\
	       act_status[k] = AR_Compare_Invalid;			\
									\
	    test_status[k] = AR_compare((AR_DATA *)&cool_ints[1],	\
					&ar_types[j],			\
					(AR_DATA *)&cool_ints[1],	\
					&ar_types[k]);			\
									\
	    if ((act_status[k] == AR_Compare_Invalid) ^			\
		(test_status[k] == AR_Compare_Invalid))			\
	    {								\
	       printf(" ERROR:  incorrect status at %d,%d\n", j, k);	\
	       abort();							\
	    }								\
	 }								\
      }									\
									\
      printf("PASSED\n");						\
   } while (0)


test_compare()
{

   TEST_COMPARE_OP_SIGNED(   8);
   TEST_COMPARE_OP_UNSIGNED( 8);

   TEST_COMPARE_OP_SIGNED(  16);
   TEST_COMPARE_OP_UNSIGNED(16);

   TEST_COMPARE_OP_SIGNED(  32);
   TEST_COMPARE_OP_UNSIGNED(32);

   TEST_COMPARE_OP_SIGNED(  46);

   TEST_COMPARE_OP_SIGNED(  64);
   TEST_COMPARE_OP_UNSIGNED(64);

   TEST_COMPARE_OP_TYPES;

}  /* test_compare */


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: testint.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
