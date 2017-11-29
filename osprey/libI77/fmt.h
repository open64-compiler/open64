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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/fmt.h,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
#ifndef FMT_INCLUDED
#define FMT_INCLUDED

#include "cmplrs/host.h"
#include <stdlib.h>

#define RET 1
#define REVERT 2
#define GOTO 3
#define X 4
#define SLASH 5
#define STACK 6
#define I 7
#define ED 8
#define NED 9
#define IM 10
#define APOS 11
#define H 12
#define TL 13
#define TR 14
#define T 15
#define COLON 16
#define S 17
#define SP 18
#define SS 19
#define P 20
#define BN 21
#define BZ 22
#define F 23
#define E 24
#define EE 25
#define D 26
#define G 27
#define GE 28
#define L 29
#define A 30
#define AW 31
#define O 32
#define OM 33
#define Z 34
#define ZM 35
#define NONL 36
#define Q 37

#ifdef I90
#define EN 38
#define ENE 39
#define ES 40
#define ESE 41
#define B 42
#define BM 43
#endif

/* fix bug 6534 
extern struct f77syl f77syl[];
 */
/* PROTOTYPES from fmt.c */
extern int   pars_f(unit *, char *);
extern int   ne_d(unit *, char *, char **);
extern int   e_d(unit *, char *, char **);
extern int   op_gen(unit *, int, long, long, int);
extern int   en_fio(unit **);
extern int   do_fio(ftnint *, ftnint *, char *, ftnlen);
extern int   do_fio_1dim(ftnint *, char *, flex *, ftnint *,ftnint *, ftnint *, ftnlen, ftnlen);
extern int   do_fio_mp(ftnint *, ftnint *, char *, unit **, ftnlen);
#ifdef I90
extern int	test_type(int op, ftnint type);
#endif
extern void  fmt_bg(unit *);
extern char  *ap_end(unit *, char *);

extern int   do_fioi4(unsigned int);
extern int   do_fioi8(long long);
extern int   do_fior4(float);
extern int   do_fio8(double);
extern int   do_f4f8(void *, ftnlen);
extern int   do_fioi4_mp(unsigned int, unit *);
extern int   do_fioi8_mp(long long, unit *);
extern int   do_fior4_mp(float, unit *);
extern int   do_fio8_mp(double, unit *);
extern int   do_f4f8_mp(ftnint *, void *, unit *, ftnlen);

extern int   do_fioxa4 (char *, XINT);
extern int   do_fioxa8 (char *, XINT);
extern int   do_fioxh1 (char *, XINT, XINT);
extern int   do_fioxi1 (char *, XINT);
extern int   do_fioxi2 (char *, XINT);
extern int   do_fioxi4 (char *, XINT);
extern int   do_fioxi8 (char *, XINT);
extern int   do_fioxl1 (char *, XINT);
extern int   do_fioxl2 (char *, XINT);
extern int   do_fioxl4 (char *, XINT);
extern int   do_fioxl8 (char *, XINT);
extern int   do_fioxr4 (char *, XINT);
extern int   do_fioxr8 (char *, XINT);
extern int   do_fioxr16 (char *, XINT);
extern int   do_fioxc4 (char *, XINT);
extern int   do_fioxc8 (char *, XINT);
extern int   do_fioxc16 (char *, XINT);
extern int   do_fioxa4v (ftnint);
extern int   do_fioxa8v (ftnll);
extern int   do_fioxh1v (char);
extern int   do_fioxi1v (char);
extern int   do_fioxi2v (short);
extern int   do_fioxi4v (ftnint);
extern int   do_fioxi8v (ftnll);
extern int   do_fioxl1v (char);
extern int   do_fioxl2v (short);
extern int   do_fioxl4v (ftnint);
extern int   do_fioxl8v (ftnll);
extern int   do_fioxr4v (float);
extern int   do_fioxr8v (double);
extern int   do_fioxr16v (long double);
extern int   do_fioxc4v (float, float);
extern int   do_fioxc8v (double, double);
extern int   do_fioxc16v (long double, long double);
extern int   do_fioxa4_mp (char *, XINT, unit **);
extern int   do_fioxa8_mp (char *, XINT, unit **);
extern int   do_fioxh1_mp (char *, XINT, XINT, unit **);
extern int   do_fioxi1_mp (char *, XINT, unit **);
extern int   do_fioxi2_mp (char *, XINT, unit **);
extern int   do_fioxi4_mp (char *, XINT, unit **);
extern int   do_fioxi8_mp (char *, XINT, unit **);
extern int   do_fioxl1_mp (char *, XINT, unit **);
extern int   do_fioxl2_mp (char *, XINT, unit **);
extern int   do_fioxl4_mp (char *, XINT, unit **);
extern int   do_fioxl8_mp (char *, XINT, unit **);
extern int   do_fioxr4_mp (char *, XINT, unit **);
extern int   do_fioxr8_mp (char *, XINT, unit **);
extern int   do_fioxr16_mp (char *, XINT, unit **);
extern int   do_fioxc4_mp (char *, XINT, unit **);
extern int   do_fioxc8_mp (char *, XINT, unit **);
extern int   do_fioxc16_mp (char *, XINT, unit **);
extern int   do_fioxa4v_mp (ftnint, unit **);
extern int   do_fioxa8v_mp (ftnll, unit **);
extern int   do_fioxh1v_mp (char, unit **);
extern int   do_fioxi1v_mp (char, unit **);
extern int   do_fioxi2v_mp (short, unit **);
extern int   do_fioxi4v_mp (ftnint, unit **);
extern int   do_fioxi8v_mp (ftnll, unit **);
extern int   do_fioxl1v_mp (char, unit **);
extern int   do_fioxl2v_mp (short, unit **);
extern int   do_fioxl4v_mp (ftnint, unit **);
extern int   do_fioxl8v_mp (ftnll, unit **);
extern int   do_fioxr4v_mp (float, unit **);
extern int   do_fioxr8v_mp (double, unit **);
extern int   do_fioxr16v_mp (long double, unit **);
extern int   do_fioxc4v_mp (float, float, unit **);
extern int   do_fioxc8v_mp (double, double, unit **);
extern int   do_fioxc16v_mp (long double, long double, unit **);

extern int   do_fio64(ftnint *, XINT *, char *, ftnlen);
extern int   do_fio64_1dim(ftnint *, char *, flex *, XINT *,XINT *, XINT *, ftnlen, ftnlen);
extern int do_fio64_mp_1dim(  ftnint *type, char *ptr,
                flex *do_idx, XINT *lb,
                XINT *ub, XINT *step,
                unit **fu,
                ftnlen len, ftnlen idxlen);
extern int   do_fio64_mp(ftnint *, XINT *, char *, unit **, ftnlen);
#define do_fio_SIZE_mp	do_fio64_mp


/* wsj FIO_ALLOC defined as 128 in MIPS, 32*1024 in cypress */
#define FIO_ALLOC   8*1024
typedef union
{	float pf;
	double pd;
	long double pld;
} ufloat;
typedef union
{	short is;
	signed char ic;		/* BN-8190. Change "char" to "signed char" */
	int ii;
#if defined(_LONGLONG)
	long long ill;
#else
  	long ill;
#endif
} uinteger;
#define GET(x) if((x=(*ftnunit->f77getn)(ftnunit))<0) return(x)
#define VAL(x) (x!='\n'?x:' ')
#define PUT(c,y,s) (*ftnunit->f77putn)(ftnunit, c,y,s)
#define UNGETC(x) ((*ftnunit->f77ungetn)(ftnunit, x))
#define GETC(x) (x=(*ftnunit->f77getn)(ftnunit))
#define GETS(s,w,c) ((*ftnunit->f77gets)(ftnunit,s,w,c))

/*	copy of ftypes from the compiler */
/* variable types
 * numeric assumptions:
 *	int < reals < complexes
 *	TYDREAL-TYREAL = TYDCOMPLEX-TYCOMPLEX
 */

#define TYUNKNOWN 0
#define TYADDR 1
#define TYBYTE 2
#define TYSHORT 3
#define TYINT 4
#define TYLONGLONG 5
#define TYREAL 6
#define TYDREAL 7
#define TYCOMPLEX 8
#define TYDCOMPLEX 9
#define TYLOGICAL1 10
#define TYLOGICAL2 11
#define TYLOGICAL4 12
#define TYLOGICAL8 13
#define TYCHAR 14
#define TYSUBR 15
#define TYSTRUCTURE 16
#define TYNML 17
#define TYQUAD 18
#define TYQUADCOMPLEX 19
#define TYQUADLONG 20
#define TYERROR 21

#define NTYPES (TYERROR+1)

/* Old type defines, as they were in 3.0 and before, with new types
   added at the end. match_type[] array peforms remapping, present in
   do_lio() */

#define OLD_TYUNKNOWN 0
#define OLD_TYADDR 1
#define OLD_TYBYTE 2
#define OLD_TYSHORT 3
#define OLD_TYINT 4
#define OLD_TYREAL 5
#define OLD_TYDREAL 6
#define OLD_TYCOMPLEX 7
#define OLD_TYDCOMPLEX 8
#define OLD_TYLOGICAL1 9
#define OLD_TYLOGICAL2 10
#define OLD_TYLOGICAL4 11
#define OLD_TYCHAR 12
#define OLD_TYSUBR 13
#define OLD_TYSTRUCTURE 14
#define OLD_TYNML 15
#define OLD_TYQUAD 16
#define OLD_TYERROR 17

extern ftnint          match_type[];

#endif
