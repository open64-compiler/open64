/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: c_q_to_a
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/c_q_to_a.c,v $
 *
 * Revision history:
 *  26-jul-93 - Original Version
 *
 * Description: Convert a quad precision binary number to decimal.
 *
 * ====================================================================
 * ====================================================================
 */


#if QUAD_DEBUG
#include <stdio.h>
#endif

/* Macros to pull apart parts of single and  double precision
 * floating point numbers in IEEE format
 */

/* double precision */

typedef  union {
	struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
		unsigned int  lo	:32;
		unsigned int  hi	:20;
		unsigned int  exp	:11;
		unsigned int  sign	:1;
#else
		unsigned int  sign	:1;
		unsigned int  exp	:11;
		unsigned int  hi	:20;
		unsigned int  lo	:32;
#endif
	} fparts;
	struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
		unsigned int  lo	:32;
		unsigned int  hi	:19;
		unsigned int  qnan_bit	:1;
		unsigned int  exp	:11;
		unsigned int  sign	:1;
#else
		unsigned int  sign	:1;
		unsigned int  exp	:11;
		unsigned int  qnan_bit	:1;
		unsigned int  hi	:19;
		unsigned int  lo	:32;
#endif
	} nparts;
	struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
		unsigned int lo;
		unsigned int hi;
#else
		unsigned int hi;
		unsigned int lo;
#endif
	} fwords;
	double	d;
} _dval;

/* quad precision */

typedef  union {
	struct {
		_dval	hi;
		_dval	lo;
	} qparts;
	struct {
		unsigned long long hi;
		unsigned long long lo;
	} fwords;
} _ldblval;

/* parts of a double precision floating point number */

#define	SIGNBIT(X)	(((_dval *)&(X))->fparts.sign)
#define EXPONENT(X)	(((_dval *)&(X))->fparts.exp)

#include "defs.h"
#include "quad.h"


#define min(x,y) ((x)<(y)? (x): (y))
#define max(x,y) ((x)>(y)? (x): (y))

extern void	c_qtenscale( _ldblval *, INT32, INT32 *);
static INT32	c_qtoa(char *buffer, INT32 ndigit, QUAD x, INT32 fflag);
extern INT	c_q_to_a(char *str, QUAD x, INT *p_err );

#pragma weak c_q_to_a = __c_q_to_a
#define	c_q_to_a __c_q_to_a


/* ====================================================================
 *
 * FunctionName: c_q_to_a
 *
 * Description: Convert a quad precision binary number to decimal.
 *
 * ====================================================================
 */


INT
c_q_to_a(char *str, QUAD x, INT *p_err )
{
char	buffer[42];
char	*pbuff;
INT32	ndigit;
INT32	fflag;
INT32	m, n;

	*p_err = 0;

	ndigit = 34;
	fflag = 0;

	n = c_qtoa(buffer, ndigit, x, fflag);

	if ( strncmp(buffer, "inf", 3) == 0 )
	{
		strncpy( str, buffer, 3 );

		*(str + 3) = '\0';

		return ( *p_err );
	}

	if ( strncmp(buffer, "nan", 3) == 0 )
	{
		strncpy( str, buffer, 3 );

		*(str + 3) = '\0';

		return ( *p_err );
	}

	if ( strncmp(buffer+1, "inf", 3) == 0 )
	{
		strncpy( str, buffer, 4 );

		*(str + 4) = '\0';

		return ( *p_err );
	}

	if ( strncmp(buffer+1, "nan", 3) == 0 )
	{
		strncpy( str, buffer+1, 3 );

		*(str + 3) = '\0';

		return ( *p_err );
	}

	if ( (n <= -1000) || (n >= 1000) )
	{
		*p_err = 1;

		*str = '\0';

		return ( *p_err );
	}

	pbuff = buffer;

	if ( (buffer[0] == ' ') || (buffer[0] == '-') )
		*str++ = *pbuff++;

	*str++ = *pbuff++;	/* copy first digit to output string */

	*str++ = '.';

	if ( ndigit > 1 )
	{
		strncpy(str, pbuff, ndigit-1);

		str += ndigit - 1;	/* skip over digits copied */
	}

	*str++ = 'E';

	if ( n < 0 )
	{
		n = -n;
		*str++ = '-';
	}

	m = n/100;

	if ( m!= 0 )
		*str++ = m + '0';

	n = n%100;

	m = n/10;

	*str++ = m + '0';

	m = n%10;

	*str++ = m + '0';

	*str++ = '\0';

	return ( *p_err );
}


/* ====================================================================
 *
 * FunctionName: c_qtoa
 *
 * Description: Convert a quad precision binary number to decimal.
 *
 * Except for minor changes, this routine is the same as _qtoa()
 * in libc.  Included here, so we can do cross compilations on
 * machines not supporting quad precision.
 *
 * ====================================================================
 */

static
INT32				/* returns decimal exponent */
c_qtoa(buffer,ndigit,x,fflag) 
     char *buffer;		/* Destination for ascii string */ 
     INT32 ndigit;		/* number of digits to convert.
  				 * 1 <= ndigits <= 34
				 * Log2(1e34) = 112.9 bits, so the
 				 * 107 significant bits can be con-
 				 * verted to decimal with room left
 				 * over.
				 */ 
     QUAD x;			/* number to convert to ascii */ 
     INT32 fflag;		/* 0 for e format, 1 for f format */ 
{
  _ldblval value;		/* the value manipulated */ 
#define HI (value.fwords.hi)	/* top 64b of 128b fraction */ 
#define LO (value.fwords.lo)	/* bottom 64b of 128b fraction */

  INT32 signhi, signlo;		/* sign of HI = sign of number */ 
  INT32 exphi, explo;		/* exponents of HI and LO */ 
  INT32 dexp;			/* decimal exponent */ 
  INT32 bexp;			/* binary exponent */ 
  UINT32 lohi;			/* top 4 bits of lo */ 
  UINT64 lolo;			/* bottom 60 bits of lo */ 
  INT32 nd;			/* number of digits output */ 
  INT32 guard;			/* guard digit */

#if QUAD_DEBUG 
  char *buffer0=buffer;		/* initial value of buffer */
#endif


  value.qparts.hi.d = x.hi;
  value.qparts.lo.d = x.lo;

#if QUAD_DEBUG 
  printf("initially: HI=0x%016llx\n",HI); 
  printf("LO=0x%016llx\n",LO); 
#endif

  signhi = SIGNBIT(HI); 
  HI = (HI<<1)>>1;		/* discard sign bit */ 
  *buffer++ = signhi? '-': ' ';

#if QUAD_DEBUG 
  printf("after de-signing hi: HI=0x%016llx\n",HI); 
  printf("signhi=%d\n",signhi); 
  *buffer = '\0'; 
  printf(" buffer=%s\n",buffer0);
#endif

  if( HI == 0 ) {		/* true zero */ 
    char *b;			/* fencepost for buffer */

    b = buffer + ndigit; 
    while(buffer < b) 
      *buffer++ = '0';

#if QUAD_DEBUG 
    printf("HI was zero: HI=0x%016llx\n",HI); 
    printf("signhi=%d\n",signhi); 
    *buffer = '\0'; 
    printf(" buffer=%s\n",buffer0);
#endif

    return(0);
  }

  exphi = EXPONENT(HI); 
  EXPONENT(HI) = 0;

#if QUAD_DEBUG 
  printf("HI non-zero: HI=0x%016llx\n",HI); 
  printf("exphi=%d\n",exphi); 
#endif

  if(exphi == 2047) {		/* infinity or NaN */ 
    char *s;			/* infinity or nan string */ 
    char *infinity = "inf"; 
    char *nan = "nan";

    s = HI==0? infinity: nan;

    do 
      *buffer++ = *s; 
    while(*s++);

#if QUAD_DEBUG 
    printf("NaN or Infinity: buffer=%s\n",buffer0); 
#endif

    return(0); 
  }

  /* processing a number. create 128b fraction */ 
  /* hi is non-zero, or the following while loop wouldn't terminate */ 
  if(exphi == 0) {		/* HI denorm */ 
    exphi++;
    while( !(HI & (1ULL<<52))) { /* normalize - need not be fast */ 
      HI <<=1; 
      exphi--;

#if QUAD_DEBUG 
      printf("HI denorm: HI=0x%016llx\n",HI); 
      printf("exphi=%d\n",exphi); 
#endif

    }
  } 
  else {			/* HI is normal number */ 
    HI |= 1ULL<<52;		/* expose hidden bit */

#if QUAD_DEBUG 
    printf("HI normal: HI=0x%016llx\n",HI); 
    printf("exphi=%d\n",exphi); 
#endif

  }

  HI <<=11;			/* left align */

#if QUAD_DEBUG 
  printf("HI aligned: HI=0x%016llx\n",HI); 
#endif

  /* handle LO */

  signlo = SIGNBIT(LO) ^ signhi; /* HI,LO signs differ? */
  SIGNBIT(LO) = 0;		/* discard sign */

#if QUAD_DEBUG
  printf("handle LO: LO=0x%016llx\n",LO);
  printf("           signlo=%d\n",signlo);
#endif

  if(LO) {			/* lo is non-zero */

    explo = EXPONENT(LO);
    EXPONENT(LO) = 0;

#if QUAD_DEBUG
  printf("non-zero LO: LO=0x%016llx\n",LO);
  printf("             explo=%d\n",explo);
#endif

    LO <<=1;			/* shift LO to expose 107th bit */

#if QUAD_DEBUG
    printf("left-shifted LO: LO=0x%016llx\n",LO);
#endif

    if(explo == 0) {		/* denorm */
      INT32 shift_n;		/* shift count */

      LO <<= 1;			/* shift up one more bit */
#if QUAD_DEBUG
      printf("shifted up LO: LO=0x%016llx\n",LO);
#endif
      shift_n = 53 - (exphi-explo);
      /* normalize to HI */
      if(shift_n > 0) {
	LO <<= shift_n; 
      }
      else if(shift_n < 0) {
	LO >>= -shift_n;
      }
    }
    else {			/* LO normal number */
      LO |= 1ULL<<53;		/* expose hidden bit */
      
#if QUAD_DEBUG
      printf("expose hidden bit: LO=0x%016llx\n",LO);
#endif
      
      LO >>= (exphi-explo) - 53; /* normalize to HI */
    }

#if QUAD_DEBUG
    printf("normalize LO to HI: LO=0x%016llx\n",LO);
#endif
    
    if( signlo ) {		/* LO is negative */
      HI-= 1<<11;			/* unround HI */
      LO = (1ULL<<54) - LO;	/* complement LO */
      
#if QUAD_DEBUG
      printf("LO negative: HI=0x%016llx\n",HI);
      printf("             LO=0x%016llx\n",LO);
#endif
      
    }

    HI |= LO>>43;		/* combine HI and LO into 107b fract. */
    LO <<=21;
#if QUAD_DEBUG
  printf("LO&HI combined: HI=0x%016llx\n",HI);
  printf("                LO=0x%016llx\n",LO);
#endif

  }

  /* At this point we have a 128b binary fraction HILO, the low 21
   * bits of which are zero, and a biased binary exponent exphi.
   *
   * Generate the decimal exponent by multiplying the binary 
   * exponent by log10(2). 
   */

  /* exphi is at most 9 bits plus sign. Shifts avoid overflow
   * and recover integer part.
   */

  dexp = ( ((INT32) (exphi-1023)) * (0x4D104D42L >> 21) );
  /* truncate to -infinity */
  dexp >>= 11;
  dexp++;

#if QUAD_DEBUG
  printf("before scaling: dexp=%d\n",dexp);
#endif

  /* Scale fraction by decimal exponent. */

  if(dexp)			/* value *= 10 ** -dexp */
    c_qtenscale(&value, -dexp, &bexp); 
  else
    bexp=0;

#if QUAD_DEBUG
  printf("after scaling: HI=0x%016llx\n",HI);
  printf("               LO=0x%016llx\n",LO);
  printf("               bexp=%d\n",bexp);
#endif

  exphi += bexp;

#if QUAD_DEBUG
  printf("adjusted exphi: exphi=%d\n",exphi);
#endif

  /* unbias exphi */

  exphi -= 1022;

  /* Remove binary exponent by denormalizing */
  /* Shift an extra 4 bits to allow for later digit generation */

  LO = (LO >> 4-exphi) | (HI << (60+exphi));
  HI >>= 4-exphi;

#if QUAD_DEBUG
  printf("denormalized: HI=0x%016llx\n",HI);
  printf("              LO=0x%016llx\n",LO);
#endif

  /* Split LO inpreparation to producing digits */

  lohi = LO >> 60;
  lolo = LO & ((1ULL<<60)-1);

#if QUAD_DEBUG
  printf("split LO: lohi=0x%02x\n",lohi);
  printf("          lolo=0x%016llx\n",lolo);
#endif

  /* find first non-zero digit */
  while( !(HI >> 60) ) {
    lolo *= 10;
    lohi = lohi * 10 + (lolo>>60);
    lolo &= (1ULL<<60)-1;
    HI = HI * 10 + (lohi>>4);
    lohi &= 0xF;
    dexp--;

#if QUAD_DEBUG
  printf("finding first digit: HI=0x%016llx\n",HI);
  printf("                     lohi=0x%02x\n",lohi);
  printf("                     lolo=0x%016llx\n",lolo);
  printf("                     dexp=%d\n",dexp);
#endif

  }
  /* produce digits */

  if(fflag) {			/* f format */
    nd = min(34, ndigit + dexp + 1); /* need 1 integer digit */
  }
  else {
    nd = ndigit;		/* e format */
  }

#if QUAD_DEBUG
  printf("number of digits: nd=%d\n",nd);
  printf("                  ndigit=%d\n",ndigit);
  printf("                  dexp=%d\n",dexp);
#endif

  /* produce digits - always at least one */
  do {
    *buffer++ = '0' + (HI>>60);
    HI &= (1ULL<<60) - 1;
    lolo *= 10;
    lohi = lohi * 10 + (lolo>>60);
    lolo &= (1ULL<<60)-1;
    HI = HI * 10 + (lohi>>4);
    lohi &= 0xF;

#if QUAD_DEBUG
  printf("another digit: HI=0x%016llx\n",HI);
  printf("               lohi=0x%02x\n",lohi);
  printf("               lolo=0x%016llx\n",lolo);
  *buffer = '\0';
  printf("               buffer=%s\n",buffer0);
#endif

  }
  while(--nd > 0);

  /* decimal round */

  /*	*buffer	guard	rest	Action
   * 	
   * 	dc	0-4	dc	none
   *	dc	6-9	dc	round
   * 	odd	5	dc	round
   * 	even	5	0	none
   * 	even	5	!=0	round
   */

  guard = HI>>60;
  if(guard >= 5) {
    if(guard > 5 ||
       (*buffer-1)&1 ||		/* buffer odd */
       HI & ((1ULL<<60) - 1) ||
       lohi || lolo) {		/* rest non-zero */
      char *b=buffer;		/* buffer pointer */

      /* must round */

#if QUAD_DEBUG
  printf("must round\n");
#endif
      for(b=buffer-1; ; b--) {
	if(*b < '0' || '9' < *b) { /* attempt to carry into sign */
	  *(b+1) = '1';
	  dexp++;
	  if(fflag && nd != min(34, ndigit + dexp + 1)) { 
	    *buffer++ = '0';	/* need one more digit */
#if QUAD_DEBUG
	    printf("added a zero: fflag=%d\n",fflag);
	    printf("              ndigit=%d\n",ndigit);
	    printf("              nd=%d\n",nd);
#endif
	  }

#if QUAD_DEBUG
  printf("carry into sign: dexp=%d\n",dexp);
  printf("                 buffer=%s\n",buffer0);
#endif

	  break;
	}
	(*b)++;

#if QUAD_DEBUG
  *buffer = '\0';
  printf("round: buffer=%s\n",buffer0);
#endif

	if(*b > '9') {		/* carry */
	  *b = '0';

#if QUAD_DEBUG
  printf("carry: buffer=%s\n",buffer0);
#endif

	}
	else
	  break;		/* no carry */
      }
      
    }
  }

#if REALLYNEEDED
  if(fflag && ndigit+dexp < 0) { /* no significant digits */

    dexp--;			/* This is a lie. However, that's the
				 * way dtoa behaves (sometimes!), so 
				 * make qtoa work the same way.
				 */
  }
#endif

  *buffer = '\0';

#if QUAD_DEBUG
  printf("done: buffer=%s\n",buffer0);
  printf("      dexp=%d\n", dexp);
#endif

  return dexp;
}

