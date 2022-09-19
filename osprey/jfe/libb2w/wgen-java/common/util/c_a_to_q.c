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
 * Module: c_a_to_q
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/c_a_to_q.c,v $
 *
 * Revision history:
 *  28-jun-93 - Original Version
 *
 * Description: Convert a decimal number to a quad precision binary.
 *
 * ====================================================================
 * ====================================================================
 */

static char *source_file = __FILE__;
static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/util/c_a_to_q.c,v $ $Revision: 1.1.1.1 $";

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
#define	MAXCOUNT	34


#include <math.h>
#include "defs.h"
#include "quad.h"


extern	void	c_qtenscale( _ldblval *, INT32, INT32 *);
extern	QUAD	c_a_to_q(char *str, INT *p_err );
static	QUAD	c_atoq(char *buffer, INT32 ndigit, INT32 exp);

/* ascii to quad */

#pragma weak c_a_to_q = __c_a_to_q
#define	c_a_to_q __c_a_to_q

#define MAXDIGITS 34


/* ====================================================================
 *
 * FunctionName: c_a_to_q
 *
 * Description: Convert a decimal number to quad precision
 * Except for minor changes, this routine is the same as atold()
 * in libc.  Included here, so we can do cross compilations on
 * machines not supporting quad precision.
 *
 *
 * ====================================================================
 */

QUAD
c_a_to_q(char *s, INT *p_err )
{
    register UINT32 c;
    register UINT32 negate, decimal_point;
    register char *d;
    register INT32 exp;
    QUAD	x;
    register INT32 dpchar;
    char digits[MAXDIGITS];

    *p_err = 0;

    while ( (c = *s++) == ' ' )
		;

    /* process sign */
    negate = 0;
    if (c == '+') {
	c = *s++;
    }
    else if (c == '-') {
	negate = 1;
	c = *s++;
    }
    d = digits;
    dpchar = '.' - '0';
    decimal_point = 0;
    exp = 0;
    while (1) {
	c -= '0';
	if (c < 10) {
	    if (d == digits+MAXDIGITS) {
		/* ignore more than 34 digits, but adjust exponent */
		exp += (decimal_point ^ 1);
	    }
	    else {
		if (c == 0 && d == digits) {
		    /* ignore leading zeros */
			;
		}
		else {
		    *d++ = c;
		}
		exp -= decimal_point;
	    }
	}
	else if (c == dpchar && !decimal_point) {    /* INTERNATIONAL */
	    decimal_point = 1;
	}
	else {
	    break;
	}
	c = *s++;
    }
    if (d == digits) {
	x.hi = 0.0;
	x.lo = 0.0;
	return x;
    }
    if (c == 'e'-'0' || c == 'E'-'0') {
	register UINT32 negate_exp = 0;
	register INT32 e = 0;
	c = *s++;
	if (c == '+' || c == ' ') {
	    c = *s++;
	}
	else if (c == '-') {
	    negate_exp = 1;
	    c = *s++;
	}
	if (c -= '0', c < 10) {
	    do {
	        if (e <= 340) 
	            e = e * 10 + c;
		else break;
	        c = *s++;
	    }
	    while (c -= '0', c < 10);
	    if (negate_exp) {
	        e = -e;
	    }
            if (e < -(323+MAXDIGITS) || e > 308) 
		exp = e;
	    else 
		exp += e;
	}
    }

    if (exp < -(324+MAXDIGITS)) {
	x.hi = 0.0;
	x.lo = 0.0;
    }
    else if (exp > 308) {
        x.hi = HUGE_VAL;
	x.lo = 0.0;
    }
    else {
	/* let c_atoq diagnose under- and over-flows */
	/* if the input was == 0.0, we have already returned,
	   so retval of +-Inf signals OVERFLOW, 0.0 UNDERFLOW
	*/

	x = c_atoq (digits, d - digits, exp);
    }
    if (negate) {
	x.hi = -x.hi;
	x.lo = -x.lo;
    }
    return x;
}

/* ====================================================================
 *
 * FunctionName: c_atoq
 *
 * Description: Convert a decimal number to a quad precision binary.
 *
 * Except for minor changes, this routine is the same as _atoq()
 * in libc.  Included here, so we can do cross compilations on
 * machines not supporting quad precision.
 *
 * ====================================================================
 */

static
QUAD
c_atoq(buffer,ndigit,exp)
     char *buffer;		/* Values from 0 to 9, NOT ascii. 
				 * Leading zeros have been discarded, 
				 * except for the case of zero in which
				 * case there is a single 0 digit. 
				 */

     INT32  ndigit;		/* 1 <= ndigit <= 37.
				 * Log2(1e37) = 122.9 bits, so the 
				 * number can be converted to a 128 bit
				 * integer with room left over.
				 */

     INT32 exp;			/* The minimum quad numbers are
				 *
				 * 4.9406564584124654e-324 (denorm) and 
				 * 2.2250738585072014e-308 (norm).
				 *
				 * The maximum value is 
				 *
				 * 1.7976931348623157e+308, 
				 *
				 * therefore 
				 * -324-ndigit = -361 <= exp <= 308.
				 */
{
  _ldblval value;		/* the result */
#define HI (value.fwords.hi)	/* top 64b of 124b integer */
#define LO (value.fwords.lo)	/* bottom 60b of 124b integer */
				/* later converted to 128b fraction */

  QUAD	result;
  UINT32 guard;			/* First guard bit */
  UINT32 rest;			/* Remaining 21 guard bits */
  UINT64 lolo;			/* lower 60b of LO */
  UINT32 lohi;			/* upper  4b of LO */
  double z;			/* canonical HI */

  INT32 exphi, explo;		/* binary exponent */
  INT32 nzero;			/* number of non-zeros */
  INT32 i;			/* loop index */
  INT32 x;			/* temporary */
  INT32 losign=0;		/* sign of LO */

  char *bufferend;		/* pointer to char after last digit */
  
  HI = 0;

  /* Check for zero and treat it as a special case */

  if ((INT32) *buffer == 0){
    result.hi = result.lo = 0.0;
    return ( result );
  }

  /* Convert the decimal digits to a binary integer. */

  bufferend = buffer + ndigit;
  HI = 0;			
  LO = *buffer++;

  /* the first batch of digits fits in LO */
  for( i=1 ; buffer < bufferend && i<19; buffer++, i++ ){
    LO *= 10;
    LO += *buffer;
  }

#if QUAD_DEBUG
  printf("halfway thru conversion: HI=0x%016llx\n",HI);
  printf("                         LO=0x%016llx\n",LO);
  printf("                         i=%d\n",i);
#endif

  if( buffer < bufferend ) {
    /* The second batch of digits affects both HI and LO */

    lolo = LO & (1Ull<<60)-1;	/* Split LO into 60b  */
    lohi = LO >> 60;		/*            and 4b pieces */

#if QUAD_DEBUG
  printf("After LO split: lohi=0x%x\n",lohi);
  printf("                lolo=0x%016llx\n",lolo);
#endif

    for( ; buffer < bufferend; buffer++ ){
      lolo = 10*lolo + *buffer;	/* Multiply by 10 and add digit */
#if QUAD_DEBUG
  printf("After 10*: lolo=0x%016llx\n",lolo);
#endif
      lohi = 10*lohi + (lolo>>60);
      HI   = 10*HI   + (lohi>>4);
      lolo &= (1Ull<<60)-1;	/* discard carries already propagated */
#if QUAD_DEBUG
  printf("After carry discard: lolo=0x%016llx\n",lolo);
#endif
      lohi &= (1U  << 4)-1;	/* Are these two statements needed??? */

#if QUAD_DEBUG
  printf("After iteration: lohi=0x%x\n",lohi);
  printf("                 lolo=0x%016llx\n",lolo);
#endif

    }
    /* Reconstitute LO from its pieces. */
    LO = lolo | ((UINT64) lohi)<<60;

#if QUAD_DEBUG
  printf("After reconstitution: HI=0x%016llx\n",HI);
  printf("                      LO=0x%016llx\n",LO);
#endif

  }

#if QUAD_DEBUG
  printf("after conversion: HI=0x%016llx\n",HI);
  printf("                  LO=0x%016llx\n",LO);
#endif

  /* Normalize HI-LO */

  exphi = 128;			/* convert from 128b int to fraction */
  if (HI == 0){			/* 64 bit shift */
    HI = LO;
    LO = 0;
    exphi -= 64;
  }

  /* Count number of non-zeroes in HI */
  nzero = 0;
  if ( HI >> (32        ) ){ nzero  = 32; }
  if ( HI >> (16 + nzero) ){ nzero += 16; }
  if ( HI >> ( 8 + nzero) ){ nzero +=  8; }
  if ( HI >> ( 4 + nzero) ){ nzero +=  4; }
  if ( HI >> ( 2 + nzero) ){ nzero +=  2; }
  if ( HI >> ( 1 + nzero) ){ nzero +=  1; }
  if ( HI >> (     nzero) ){ nzero +=  1; }

  /* Normalize */
  HI = (HI << (64-nzero)) | (LO >> (nzero));
  LO <<= 64-nzero;
  exphi -= 64-nzero;

  /* At this point we have a 128b fraction and a binary exponent 
   * but have yet to incorporate the decimal exponent.
   */

#if QUAD_DEBUG
  printf("after normalization: HI=0x%016llx\n",HI);
  printf("                     LO=0x%016llx\n",LO);
  printf("                     nzero=%d\n",nzero);
  printf("                     exphi=%d\n",exphi);
#endif

  /* multiply by 10^exp */

  c_qtenscale(&value,exp,&x);
  exphi += x;

#if QUAD_DEBUG
  printf("after multiplication: HI=0x%016llx\n",HI);
  printf("                      LO=0x%016llx\n",LO);
  printf("                     exphi=%d\n",exphi);
#endif
  /* Round to 107 bits */
  /* Take the 128 bits of HI and LO and divide them as follows
   *
   * before	HI	LO	guard	rest
   *            64b     64b
   * after	53b	54b	1b	20b
   *		       =11 of HI
   *		       +43 of LO
   */

  rest = LO & (1ULL<<20)-1;
  LO >>= 20;

#if QUAD_DEBUG
  printf("during split: LO=0x%016llx\n",LO);
#endif

  guard = LO & 1;
  LO >>= 1;

#if QUAD_DEBUG
  printf("during split: LO=0x%016llx\n",LO);
#endif

  LO |= (HI & (1ULL<<11)-1) << 43;
  HI >>= 11;

#if QUAD_DEBUG
  printf("after split: HI=0x%016llx\n",HI);
  printf("             LO=0x%016llx\n",LO);
  printf("             guard=%d\n",guard);
  printf("             rest=%lx\n",rest);
#endif

  /*	LO&1	guard	rest	Action
   * 	
   * 	dc	0	dc	none
   * 	1	1	dc	round
   * 	0	1	0	none
   * 	0	1	!=0	round
   */

  if(guard) {
    if(LO&1 || rest) {
      LO++;			/* round */
      HI += LO>>54;
      LO &= (1ULL<<54)-1;
      if(HI>>53) {		/* carry all the way across */
	HI >>= 1;		/* renormalize */
	exphi ++;
      }
    }
  }

  explo = exphi-53;
  
#if QUAD_DEBUG
  printf("after rounding: HI=0x%016llx\n",HI);
  printf("                LO=0x%016llx\n",LO);
  printf("                exphi=%d\n",exphi);
  printf("                explo=%d\n",explo);
#endif

  /* Apply Dekker's algorithm */
  /* Determine whether HI <- (double) HI+LO would change HI */
  if( LO & (1ULL<<53) ) {	/* high bit of LO on */
    if(HI & 1  ||  LO & ((1ULL<<53)-1)) { 
      HI++;			/* round */
      if(HI & (1ULL<<53)) {	/* ripple carry */
	HI >>= 1;
	exphi++;
      }
      LO = (1ULL<<54) - LO;	/* complement LO */
      losign = 1;

#if QUAD_DEBUG
  printf("after dekker: HI=0x%016llx\n",HI);
  printf("              LO=0x%016llx\n",LO);
  printf("              exphi=%d\n",exphi);
  printf("              explo=%d\n",explo);
  printf("              losign=%d\n",losign);
#endif
      
    }
  }
  if( LO ) {
    /* normalize LO */
    if(LO & (1ULL<<53)) {	/* LO = 0x0020000000000000 */
      explo++;			/* in all other cases this bit's zero */
      LO >>=1;

#if QUAD_DEBUG
  printf("after right shift: LO=0x%016llx\n",LO);
  printf("                   explo=%d\n",explo);
#endif
      
    }
    else {
      while( ! (LO & 0x0010000000000000ULL) ){

#if QUAD_DEBUG
  printf("before left shift: LO=0x%016llx\n",LO);
  printf("                  explo=%d\n",explo);
#endif
      
	explo--;
	LO <<= 1;
      }
    }
    explo--;			/* we now have only 53 bits */
  }
  
#if QUAD_DEBUG
  printf("after LO normalize: LO=0x%016llx\n",LO);
  printf("                    explo=%d\n",explo);
#endif

  /*
   * Check for overflow and denorm......
   * IEEE Double Precision Format
   * (From Table 7-8 of Kane and Heinrich)
   * 
   * Fraction bits               52
   * Emax                     +1023
   * Emin                     -1022
   * Exponent bias            +1023
   * Exponent bits               11
   * Integer bit             hidden
   * Total width in bits         64
   */
  
  if (exphi > 1024) {		/* overflow */
    value.qparts.hi.d = value.qparts.lo.d =  HUGE_VAL;

#if QUAD_DEBUG
  printf("Overflow: value.qparts.hi.d=0x%016llx\n",value.qparts.hi.d);
  printf("          value.qparts.lo.d=0x%016llx\n",value.qparts.lo.d);
#endif

    result.hi = value.qparts.hi.d;
    result.lo = value.qparts.lo.d;
    return ( result );
  }
  if (exphi <= -1022) {		/* HI denorm or underflow */

    value.qparts.lo.d = 0;	/* therefore LO is zero */
    exphi += 1022;
    if( exphi < -52 ) {		/* underflow */
      value.qparts.hi.d = 0;

#if QUAD_DEBUG
  printf("HI underflow: HI=0x%016llx\n",HI);
  printf("              exphi=%d\n",exphi);
#endif

    }
    else {			/* denorm */
      rest = HI & (1UL << exphi-1)-1;
      guard = (HI & (1UL << exphi)) >> exphi;
      HI >>= 1-exphi;		/* exponent is zero */

#if QUAD_DEBUG
  printf("HI denorm: HI=0x%016llx\n",HI);
  printf("           rest=0x%016llx\n",rest);
  printf("           guard=%d\n",guard);
  printf("           exphi=%d\n",exphi);
#endif

      /* Round */
      if( guard ) {
	if( HI&1 || rest ) {
	  HI++;
	  if( HI == (1ULL << 52) ) { /* carry created a normal number */
	    HI = 0;
	    EXPONENT(HI) = 1;
	  }

#if QUAD_DEBUG
  printf("Round denorm: HI=0x%016llx\n",HI);
#endif

	}
      }
    }
  }
  else {			/* HI is normal */
    HI &= ~(1ULL << 52);	/* hide hidden bit */
    EXPONENT(HI) = exphi + 1022; /* add bias */

#if QUAD_DEBUG
  printf("Normal HI: HI=0x%016llx\n",HI);
#endif

  }

  if( explo <= -1022 ) {	/* LO denorm or underflow */
    explo += 1022;
    if( explo < -52 ) {		/* underflow */
      value.qparts.lo.d = 0;

#if QUAD_DEBUG
  printf("LO underflow: LO=0x%016llx\n",LO);
  printf("              explo=%d\n",explo);
#endif

    }
    else {			/* denorm */
      rest = LO & (1UL << explo-1)-1;
      guard = (LO & (1UL << explo)) >> explo;
      LO >>= 1-explo;		/* exponent is zero */

#if QUAD_DEBUG
  printf("LO denorm: LO=0x%016llx\n",LO);
  printf("           explo=%d\n",explo);
  printf("           guard=%d\n",guard);
  printf("           rest=0x%016llx\n",rest);
#endif

      /* Round */
      if( guard ) {
	if( LO&1 || rest ) {
	  LO++;
	  if( LO == (1ULL << 52) ) { /* carry created normal number */
	    LO = 0;
	    EXPONENT(LO) = 1;	
  }

#if QUAD_DEBUG
  printf("After LO round: LO=0x%016llx\n",LO);
#endif

	}
      }
    }
  }
  else {			/* LO is normal */
    if(LO) {
      LO &= ~(1ULL << 52);	/* hide hidden bit */
      EXPONENT(LO) = explo + 1022; /* add bias */
    }
#if QUAD_DEBUG
  printf("Normal LO before making canonical: LO=0x%016llx\n",LO);
#endif
    /* Make representation canonical */
    z = value.qparts.lo.d + value.qparts.hi.d;
    value.qparts.lo.d -= (z - value.qparts.hi.d);
    value.qparts.hi.d = z;
#if QUAD_DEBUG
  printf("After making canonical: HI=0x%016llx\n",HI);
  printf("                      : LO=0x%016llx\n",LO);
#endif

  }
    
  SIGNBIT(LO) = losign;
  result.hi = value.qparts.hi.d;
  result.lo = value.qparts.lo.d;
  return ( result );
}

