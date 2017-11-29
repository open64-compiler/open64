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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/lread.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

#include <ctype.h>
#include <mutex.h>
#include <malloc.h>
#include <limits.h>
#include <cmplrs/fio.h>
#include "cmplrs/f_errno.h"
#include "fmt.h"
#include "lread.h"
#include "iomode.h"
#include "lio.h"
#include "vmsflags.h"
#include "iio.h"
#include "uio.h"
#include "err.h"
#include "util.h"
#include "bcompat.h"

#define isblnk(x) (f77ltab[x+1]&BX)
#define issep(x) (f77ltab[x+1]&SX)
#define isapos(x) (f77ltab[x+1]&AX)
#define isexp(x) (f77ltab[x+1]&EX)
#define issign(x) (f77ltab[x+1]&SG)
#define SX 1
#define BX 2
#define AX 4
#define EX 8
#define SG 16


static char     f77ltab[128 + 1] ={	/* offset one for EOF */
				   0,
/*                      \002                     \n                       */
		   0, 0, AX, 0, 0, 0, 0, 0, 0, 0, SX, 0, 0, 0, 0, 0,
		     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*          " "       \"              '            +    ,   -     /      */
	  SX | BX, 0, AX, 0, 0, 0, 0, AX, 0, 0, 0, SG, SX, SG, 0, SX,
		     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*                              D   E                                 */
                   0, 0, 0, 0, EX, EX, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*                      Q					      */
		     0, EX, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*                 `           d    e                                */
		  AX, 0, 0, 0, EX, EX, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*                      q					      */
		     0, EX, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/* To avoid innumerable calls to GETC() and UNGETC() assume that there is
always a one character look ahead 'ftnunit->nextch' within the function 
l_read
*/
static int l_R(unit *, double *, int);
static int l_R16(unit *, long double *, int);
static int l_C(unit *, void *, void *, int);
static int l_L(unit *, double *, int);
static int l_CHAR(unit *, ftnlen, int isnamelist);	
					/* make sure they are
					 * referenced as static
					 * functions */

/* we allow users to go up to 2 ** 48 for repitition count for 64 bit I/O */
/* Why not 2**64?  Because the repitition count is converted in a double
   and there is only 52 bits of mantissa! */
#if (_MIPS_SIM == _MIPS_SIM_ABI64)
#define MAX_REP 281474976710656LL	/* 2 ** 48 */
#else
#define MAX_REP INT_MAX
#endif

int
t_gets (unit *ftnunit, char *s, int w, char c)
{
   register int    ch, n;

   for (n = 0; n < w; n++) {
      ch = getc (ftnunit->ufd);
      if (ch == EOF || ch == c || ch == '\n')
	 break;
      *(s++) = c;
   }
   if (ch == c || ch == '\n')
      return (n);
   if (feof (ftnunit->ufd) && (ftnunit->ufd != stdin || !f77vms_flag_[VMS_IN]))
      ftnunit->uend = 1;
   return (EOF);
}

/* These two dummy flags are turned off and on but are not used at all!!
   Pretty good attempt at obfuscation.
   I changed l_first so that it is set when l_read is called for the first
   time i.e. when a look ahead character is needed.
*/

int
t_getc (unit *ftnunit)
{
   int             ch;

   if ((ch = getc (ftnunit->ufd)) != EOF)
      return (ch);
   if (feof (ftnunit->ufd) && (ftnunit->ufd != stdin || !f77vms_flag_[VMS_IN]))
      ftnunit->uend = 1;
   return (EOF);
}

int
t_ungetc (unit *ftnunit, int ch)
{
   ungetc (ch, ftnunit->ufd);
   return 0;
}

int e_rsle (void)
{
    return( e_rsle_mp( &f77curunit ) );
}

int e_rsle_mp (unit **fu)
{
   unit		*ftnunit = *fu;

   if (ftnunit->ufmt != 2) {
      if (ftnunit->l_first)
	 ftnunit->nextch = t_getc (ftnunit);
      while (ftnunit->nextch != '\n' && ftnunit->nextch != EOF)
	 ftnunit->nextch = t_getc (ftnunit);
   }
   ftnunit->lock_unit = 0;
   return (0);
}

int e_rsli (void)
{
   return (0);
}

int e_rsli_mp (unit **fu)
{
   (*fu)->lock_unit = 0;
   return (0);
}


 /* ltype has 3 values: NULL, TYINT and TYCHAR and is used as a
  * YES/NO flag to indicate a valid value has been read. Change it to
  * have values: NULL, TYINT, TYCHAR, or TYERROR to indicate a null
  * value, a numeric value, a character value, or an invalid value
  * has been read respectively */

/* Note that type also has information about whether this is a 
 *  called for namelist I/O. Bit 16 is true for namelist, bit 17 for F90
 *  namelist.
 */

int
l_read (unit *ftnunit, XINT *number, flex *ptr, ftnlen len, ftnint type)
{
   XINT             i;
   double         *yy;
   float          *xx;
   int            isnamelist;
   int            isnamelist90;
   int		  n;

   isnamelist = (type >> 16) & 1;
   isnamelist90 = (type >> 17) & 1;
   type &= ((1<<16) - 1);
   if (ftnunit->l_first || isnamelist) {
      ftnunit->l_first = 0;
      GETC (ftnunit->nextch);
   }
   for (i = 0; i < *number; i++) {
      if (ftnunit->lquit)
	 return 0;
      /* If there is an existing value, use it */
      if (ftnunit->lcount)
	 goto setvalue;
      for (;; GETC (ftnunit->nextch)) {
   donext:
	 switch (ftnunit->nextch) {
	 case EOF:
	    goto loopend;
	 case ' ':
	    /* optimize for the most common case */
	    while (GETC (ftnunit->nextch) == ' ');
	    goto donext;
	 case '\t':
	 case '\n':
	    continue;
	 case '/':
	    ftnunit->lquit = 1;
	    goto quit;
	 case ',':
/* A comma would represent a null value provided that the previous READ
has skipped through a "value separator" which may have a comma in it
*/
	    ftnunit->lcount = 1;
	    GETC (ftnunit->nextch);
	    goto bump;
/* LHL 5/24/89
 * change it so that comment is allowed anywhere within the namelist
 * file
 */
	 case '!':
	    if (isnamelist) {
	       for (; ftnunit->nextch != '\n' && ftnunit->nextch != EOF; GETC (ftnunit->nextch));

	       if (ftnunit->nextch == EOF)
		  goto loopend;
	       continue;
	    } else {
	       ftnunit->lcount = i;
	       err(CILISTERR, 112, "! in list input")
	    }
/* end of change */
	       /* ^Z is the character for EOF record */
	 case '\032':
	       if (f77vms_flag_[VMS_EF]
		   && GETC (ftnunit->nextch) && ftnunit->nextch == '\n') {
	       /* endfile record: OK to return here */
	          ftnunit->lcount = i;
		  return (EOF);
	       }
	    else {
	       ftnunit->lcount = i;
	       err(CILISTERR, 112, "^Z in list input")
	    }
/* PFH 7/22/94
 * change it so that comment is allowed to start in 1st column for namelist.
 */
	 case '*':
	 case 'C':
	 case 'c':
/* This doesn't work, since no one has ever bothered to set 
 * f77recpos to anything other than 0.  This should be looked at later.
 */
	 default:
	    goto rddata;
	 }
      }
rddata:
      /* if there is already an existing count don't do any more
       * reading stuff */
      /* This whole thing which is supposed to
       * check for error doesn't do any good.  The error checking can
       * be done quite well, and more complete in the respective
       * input routine.  This is just a  waste of time */
      switch ((int) type) {
      case TYBYTE:
      case TYSHORT:
      case TYINT:
      case TYREAL:
      case TYDREAL:
	 if (n=l_R(ftnunit, (double *) &ftnunit->lqx, 0)) {
	    if (!isnamelist) {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else if (isalpha( ftnunit->nextch)) {
	    /* namelist error cause by an alphabetic character which is
	    most likely the next namelist variable name while reading 
	    more than one values of an array.  In this case, ignore the
	    rest of the array elements without generating an error.
	    */
	       UNGETC (ftnunit->nextch);
	       return( 0 );
	    }
	    else if (ftnunit->nextch != '$' && ftnunit->nextch != '&') {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else {
	       ftnunit->lquit = 1;
	       UNGETC (ftnunit->nextch);
	       goto bump;
	    }
	 }
	 break;
      case TYQUAD:
      case TYLONGLONG:
	 if (n=l_R16 (ftnunit, (long double *) &ftnunit->lqx, 0)) {
	    if (!isnamelist) {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else if (isalpha( ftnunit->nextch)) {
	    /* namelist error cause by an alphabetic character which is
	    most likely the next namelist variable name while reading 
	    more than one values of an array.  In this case, ignore the
	    rest of the array elements without generating an error.
	    */
	       UNGETC (ftnunit->nextch);
	       return( 0 );
	    }
	    else if (ftnunit->nextch != '$' && ftnunit->nextch != '&') {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else {
	       ftnunit->lquit = 1;
	       UNGETC (ftnunit->nextch);
	       goto bump;
	    }
	 }
	 break;
      case TYCOMPLEX:
      case TYDCOMPLEX:
	 if (n=l_C (ftnunit, (double *) &ftnunit->lqx, &ftnunit->lqy, 0)) {
	    if (!isnamelist) {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else if (isalpha( ftnunit->nextch)) {
	    /* namelist error cause by an alphabetic character which is
	    most likely the next namelist variable name while reading 
	    more than one values of an array.  In this case, ignore the
	    rest of the array elements without generating an error.
	    */
	       UNGETC (ftnunit->nextch);
	       return( 0 );
	    }
	    else if (ftnunit->nextch != '$' && ftnunit->nextch != '&') {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else {
	       ftnunit->lquit = 1;
	       UNGETC (ftnunit->nextch);
	       goto bump;
	    }
	 }
	 break;
      case TYQUADCOMPLEX:
	 if (n=l_C (ftnunit, (long double *) &ftnunit->lqx, &ftnunit->lqy, 1)) {
	    if (!isnamelist) {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else if (isalpha( ftnunit->nextch)) {
	    /* namelist error cause by an alphabetic character which is
	    most likely the next namelist variable name while reading 
	    more than one values of an array.  In this case, ignore the
	    rest of the array elements without generating an error.
	    */
	       UNGETC (ftnunit->nextch);
	       return( 0 );
	    }
	    else if (ftnunit->nextch != '$' && ftnunit->nextch != '&') {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else {
	       ftnunit->lquit = 1;
	       UNGETC (ftnunit->nextch);
	       goto bump;
	    }
	 }
	 break;
      case TYLOGICAL1:
      case TYLOGICAL2:
      case TYLOGICAL4:
      case TYLOGICAL8:
	 if (n=l_L (ftnunit, (double *)&ftnunit->lqx, isnamelist)) {
	    if (!isnamelist) {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else if (isalpha( ftnunit->nextch)) {
	    /* namelist error cause by an alphabetic character which is
	    most likely the next namelist variable name while reading 
	    more than one values of an array.  In this case, ignore the
	    rest of the array elements without generating an error.
	    */
	       UNGETC (ftnunit->nextch);
	       return( 0 );
	    }
	    else if (ftnunit->nextch != '$' && ftnunit->nextch != '&') {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else {
	       ftnunit->lquit = 1;
	       UNGETC (ftnunit->nextch);
	       goto bump;
	    }
	 }
	 break;
      case TYCHAR:
	 if (n=l_CHAR (ftnunit, len, isnamelist)) {
	    if (!isnamelist) {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else if (isalpha( ftnunit->nextch)) {
	    /* namelist error cause by an alphabetic character which is
	    most likely the next namelist variable name while reading 
	    more than one values of an array.  In this case, ignore the
	    rest of the array elements without generating an error.
	    */
	       UNGETC (ftnunit->nextch);
	       return( 0 );
	    }
	    else if (ftnunit->nextch != '$' && ftnunit->nextch != '&') {
	        ftnunit->lcount = i; 
		return(112);
	    }
	    else {
	       ftnunit->lquit = 1;
	       UNGETC (ftnunit->nextch);
	       goto bump;
	    }
	 }
	 break;
      }

      if (isnamelist) {
	 while (ftnunit->nextch == ' ' || ftnunit->nextch == '\t' || ftnunit-> nextch == '\n' )
	    GETC (ftnunit->nextch);
	 if ( ftnunit->nextch == EOF ) {
	    fseek( ftnunit->ufd, -1, SEEK_END );
	    GETC (ftnunit->nextch);
	 }
      } else {
      while (ftnunit->nextch == ' ' || ftnunit->nextch == '\t')
	 GETC (ftnunit->nextch);
      }

      /* skip over the first comma which is part of a
       * "<SPACE>comma<SPACE>" value separator */
      if (ftnunit->nextch == ',')
	 GETC (ftnunit->nextch);

loopend:
      /* You should never get here when you quit */
      if (!ftnunit->f77errlist.iciunit && feof (ftnunit->ufd)) {
	 ftnunit->lcount = i;
	 err(ftnunit->f77errlist.ciend, (EOF), "list in")
      } else if (!ftnunit->f77errlist.iciunit && ftnunit->ufd && ferror (ftnunit->ufd)) {
	 ftnunit->lcount = i;
	 clearerr(ftnunit->ufd);
	 err(CILISTERR, errno, "list in")
      } else if (ftnunit->f77errlist.iciunit && ftnunit->nextch==EOF) {
	 ftnunit->lcount = i;
	 err(ftnunit->f77errlist.ciend, (EOF), "list in")
      }

setvalue:
      switch (ftnunit->ltype) {
      case NULL:		/* The data is a NULL value */
	 goto bump;
      case TYINT:
      case TYCHAR:
	 break;
      case TYERROR:             /* The data is no good */
	 ftnunit->lcount = 0;
	 if (isnamelist) {
	    goto quit;
	 } else {
	    ftnunit->lcount = i;
	    err(CILISTERR, 112, "list input");
	 }
      default:
	 return(ftnunit->uerror);
      }

      switch ((int) type) {
      case TYLOGICAL1:
      case TYBYTE:
	 ptr->flbyte = (signed char) ftnunit->lqx.fldouble;
	 break;
      case TYLOGICAL2:
      case TYSHORT:
	 ptr->flshort = (short) ftnunit->lqx.fldouble;
	 break;
      case TYLOGICAL4:
      case TYINT:
	 ptr->flint = (int) ftnunit->lqx.fldouble;
	 break;
      case TYLOGICAL8:
      case TYLONGLONG:
	 ptr->flll = (long long) ftnunit->lqx.flquad;
	 break;
      case TYREAL:
	 ptr->flreal = (float) ftnunit->lqx.fldouble;
	 break;
      case TYDREAL:
	 ptr->fldouble = ftnunit->lqx.fldouble;
	 break;
      case TYQUAD:
	 ptr->flquad = ftnunit->lqx.flquad;
	 break;
      case TYCOMPLEX:
	 xx = (float *) ptr;
	 *xx++ = (float) ftnunit->lqx.fldouble;
	 *xx = (float) ftnunit->lqy.fldouble;
	 break;
      case TYDCOMPLEX:
	 yy = (double *) ptr;
	 *yy++ = ftnunit->lqx.fldouble;
	 *yy = ftnunit->lqy.fldouble;
	 break;
      case TYQUADCOMPLEX:
	 ptr->flquad = ftnunit->lqx.flquad;
	 * (&ptr->flquad+1) = ftnunit->lqy.flquad;
	 break;
      case TYCHAR:
	 b_char (ftnunit->f77fio_buf, (char *) ptr, len);
	 break;
      }
bump:
      if (ftnunit->lcount > 0)
	 ftnunit->lcount--;
      ptr = (flex *) ((char *) ptr + len);
   }
quit:
   if (isnamelist)
      UNGETC (ftnunit->nextch);
   return (0);
}


static long double   f77ten_powq[] =
		{ 1e1L, 1e2L, 1e3L, 1e4L, 1e5L, 1e6L, 1e7L, 1e8L, 1e9L, 
		1e10L, 1e11L, 1e12L, 1e13L, 1e14L, 1e15L, 1e16L, 1e17L, 
		1e18L, 1e19L, 1e20L, 1e21L, 1e22L, 1e23L, 1e24L, 1e25L, 
		1e26L, 1e27L, 1e28L, 1e29L, 1e30L, 1e31L, 1e32L, 1e33L, 
		1e34L, 1e35L };

static int l_R16(unit *ftnunit, long double *qx, int skipcount)
{
   double          b, d;
   long double     c;
   int             sign = 0, db, dd;
   int             nfrac, se, exp;

   b = d = 0;
   c = 0;
   if (issign (ftnunit->nextch)) {
      sign = (ftnunit->nextch == '-');
      GETC (ftnunit->nextch);
   }
   for (db = 0, b = 0; isdigit (ftnunit->nextch) && db<15; GETC (ftnunit->nextch), db++)
      b = 10 * b + ftnunit->nextch - '0';
   if (skipcount)
       goto lcount_done;
   ftnunit->lcount = 1;
   if (ftnunit->nextch == '*') {
      /* Give an error for count too big**31 */
      if (db >= 15)
	for ( ;isdigit (ftnunit->nextch); GETC (ftnunit->nextch))
	    ;
      if (b > MAX_REP || sign || b == 0.0)
	 err(ftnunit->f77errlist.cierr, F_ERNREP, "repetition");
      ftnunit->lcount = (XINT) b;
      GETC (ftnunit->nextch);
      if (issep (ftnunit->nextch)) {
	 *qx = 0;
	 return (ftnunit->ltype=0);		/* return with ltype==NULL */
      }
      if (issign (ftnunit->nextch)) {
         sign = (ftnunit->nextch == '-');
         GETC (ftnunit->nextch);
      }
      for (db = 0, b = 0; isdigit(ftnunit->nextch) && db < 15; GETC (ftnunit->nextch), db++)
         b = 10 * b + ftnunit->nextch - '0';
   }
lcount_done:
   *qx = b;
   if (db >= 15) {
        /* Take care of precision for integer*8 constant */
	for ( ;isdigit (ftnunit->nextch); GETC (ftnunit->nextch), db++)
	    *qx = 10* (*qx) + ftnunit->nextch - '0';
   }

   if (db > 0)
       ftnunit->ltype = TYINT;
   else
       ftnunit->ltype = TYERROR;
   if (issep(ftnunit->nextch))
       goto okvalue;
   /* Behave as in INTEGER*4 by scanning the FP values as well */
   /* For the case of FP constants being used as input for INTEGER*8 variables
     take the performance hit of using quad prevision to retain the precision
    */
   nfrac = 0;
   if (ftnunit->nextch == '.') {
      int             d, nz = 0;

      GETC (ftnunit->nextch);
      while ((d = ftnunit->nextch - '0') >= 0 && d <= 9) {
         if (!d) {
            if (++nz >= sizeof (f77ten_powq) / sizeof (double) && c) {
               /* Skip the rest of the digits. They will be
                * ignored anyway */
               while (ftnunit->nextch >= '0' && ftnunit->nextch <= '9')
                  GETC (ftnunit->nextch);
               break;
            }
         } else {
            c = c * f77ten_powq[nz] + d;
            nfrac += (nz + 1);
            nz = 0;
         }
         GETC (ftnunit->nextch);
      }
   } else if (!db && ftnunit->ltype != TYINT) {
      /* something that does not start with either digit or '.' */
      ftnunit->lcount = 0;
      return( ftnunit->ltype = TYERROR );
   }
   /* Get exponent part = d */
   /* Character D or E */
   ftnunit->ltype = TYINT;
   se = 0;
   if (isexp (ftnunit->nextch) && GETC (ftnunit->nextch) || issign (ftnunit->nextch)) {
      if (issign (ftnunit->nextch)) {
         se = (ftnunit->nextch == '-');
         GETC (ftnunit->nextch);
      }
      for (dd = 0, d = 0; isdigit (ftnunit->nextch); GETC (ftnunit->nextch), dd++)
         d = 10 * d + ftnunit->nextch - '0';
      if (!dd)
         ftnunit->ltype = TYERROR;
   }
   /* Aint't got no good numeric value in the forms numval or
    * integer*numval */
   if (ftnunit->ltype == TYERROR)
      return (TYERROR);
   if (c) {
      /* normalize the fractional part only when it exists */
      if (nfrac < sizeof (f77ten_powq) / sizeof (double))
          c /= f77ten_powq[nfrac - 1];
      else {
         while (nfrac > 32) {
            c *= 1e-32L;
            nfrac -= 32;
         }
         c /= f77ten_powq[nfrac - 1];
      }
      c += *qx;
   }
   else
      c = *qx;
   exp = (int) (se ? -d : d);
   if (exp > 0) {
      while (exp > 32) {
         c *= 1e32L;
         exp -= 32;
      }
      c *= f77ten_powq[exp - 1];
   } else if (exp < 0) {
      exp = -exp;
      while (exp > 32) {
         c *= 1e-32L;
         exp -= 32;
      }
      c /= f77ten_powq[exp - 1];
   }
   *qx = c;
okvalue:
   if (sign)
      *qx = -(*qx);
   return(0);
}


static double   f77ten_pow[] ={1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11,
			  1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18};
static int l_R(unit *ftnunit, double *lx, int skipcount)
{
   double          b, c, d;
   int             sign = 0, db, dd;
   int             nfrac, se, exp;

   b = c = d = 0;
   /* get integral part = b */
   if (issign (ftnunit->nextch)) {
      sign = (ftnunit->nextch == '-');
      GETC (ftnunit->nextch);
   }
   for (db = 0, b = 0; isdigit (ftnunit->nextch); GETC (ftnunit->nextch), db++)
      b = 10 * b + ftnunit->nextch - '0';
   if (skipcount)
      goto lcount_done;
   ftnunit->lcount = 1;
   if (ftnunit->nextch == '*') {
      if (b > MAX_REP || sign || b == 0.0)
	 err(ftnunit->f77errlist.cierr, 112, "repetition");
      ftnunit->lcount = (XINT) b;
      GETC (ftnunit->nextch);
      if (issep (ftnunit->nextch)) {
	 *lx = 0.0;
	 return (ftnunit->ltype=0);		/* return with ltype==NULL */
      }
      if (issign (ftnunit->nextch)) {
	 sign = (ftnunit->nextch == '-');
	 GETC (ftnunit->nextch);
      }
      for (db = 0, b = 0; ftnunit->nextch >= '0' && ftnunit->nextch <= '9'; GETC (ftnunit->nextch), db++)
	 b = 10 * b + ftnunit->nextch - '0';
   }
lcount_done:
   if (db)
      ftnunit->ltype = TYINT;
   else ftnunit->ltype = TYERROR;
   /* let's play on a good probability of having integer here */
   if (issep (ftnunit->nextch))
      goto okvalue;
   /* get fractional part = c */
   nfrac = 0;
   if (ftnunit->nextch == '.') {
      int             d, nz = 0;

      GETC (ftnunit->nextch);
      while ((d = ftnunit->nextch - '0') >= 0 && d <= 9) {
	 if (!d) {
	    if (++nz >= 19 && c) {
	       /* Skip the rest of the digits. They will be
	        * ignored anyway */
	       while (ftnunit->nextch >= '0' && ftnunit->nextch <= '9')
		  GETC (ftnunit->nextch);
	       break;
	    }
	 } else {
	    c = c * f77ten_pow[nz] + d;
	    nfrac += (nz + 1);
	    nz = 0;
	 }
	 GETC (ftnunit->nextch);
      }
   } else if (!db && ftnunit->ltype != TYINT) {
      /* something that does not start with either digit or '.' */
      ftnunit->lcount = 0;
      return (ftnunit->ltype = TYERROR);
   }
   /* Get exponent part = d */
   /* Character D or E */
   ftnunit->ltype = TYINT;
   se = 0;
   if (isexp (ftnunit->nextch) && GETC (ftnunit->nextch) || issign (ftnunit->nextch)) {
      if (issign (ftnunit->nextch)) {
	 se = (ftnunit->nextch == '-');
	 GETC (ftnunit->nextch);
      }
      for (dd = 0, d = 0; isdigit (ftnunit->nextch); GETC (ftnunit->nextch), dd++)
	 d = 10 * d + ftnunit->nextch - '0';
      if (!dd)
	 ftnunit->ltype = TYERROR;
   }
   /* Aint't got no good numeric value in the forms numval or
    * integer*numval */
   if (ftnunit->ltype == TYERROR)
      return (TYERROR);
   if (c) {
      /* normalize the fractional part only when it exists */
      if (nfrac < sizeof (f77ten_pow) / sizeof (double))
          c /= f77ten_pow[nfrac - 1];
      else {
         while (nfrac > 16) {
	    c *= 1e-16;
	    nfrac -= 16;
         }
         c /= f77ten_pow[nfrac - 1];
      }
      b += c;
   }
   exp = (int) (se ? -d : d);
   if (exp > 0) {
      while (exp > 16) {
	 b *= 1e16;
	 exp -= 16;
      }
      b *= f77ten_pow[exp - 1];
   } else if (exp < 0) {
      exp = -exp;
      while (exp > 16) {
	 b *= 1e-16;
	 exp -= 16;
      }
      b /= f77ten_pow[exp - 1];
   }
okvalue:
   *lx = (sign) ? -b : b;
   return (0);
}


static int l_C(unit *ftnunit,  void *lx, void *ly, int quad)
{
   int             dumy;
   char           *sbuf;
   int		  n;

   /* This should have been taken care of in l_read */
   if (ftnunit->nextch != '(') {
      ftnunit->lcount = 0;
      while (isdigit (ftnunit->nextch)) {
	 ftnunit->lcount = ftnunit->lcount * 10 + ftnunit->nextch - '0';
	 GETC (ftnunit->nextch);
      }
      if (ftnunit->nextch != '*') {
	 if (ftnunit->f77errlist.iciunit || !feof (ftnunit->ufd)) {
	    err(ftnunit->f77errlist.cierr, 112, "complex input");
	 } else
	    err(ftnunit->f77errlist.cierr, (EOF), "lread");
      }
      if (GETC (ftnunit->nextch) != '(') {
	 if (issep (ftnunit->nextch)) {
	    if (quad)
	        *(long double *)lx = *(long double *)ly = 0;
	    else
		*(double *)lx = *(double *)ly = 0;
	    return (ftnunit->ltype = 0);	/* return with ltype==NULL */
	 } else {
	    err(ftnunit->f77errlist.cierr, 112, "no ( in complex data")
	 }
      }
   } else
      ftnunit->lcount = 1;

   ftnunit->ltype = TYINT;
   GETC(ftnunit->nextch);
   while (isspace (ftnunit->nextch)) GETC(ftnunit->nextch);

   if (!ftnunit->f77errlist.iciunit) {
      if (quad) {
	 if (n=l_R16 (ftnunit, lx, 1)) {
	     return(n);
	 }
      } else {
	 if (n=l_R (ftnunit, lx, 1)) {
	     return(n);
	 }
      }
   } else {
      icptr--;
      for (sbuf = icptr; icpos < ftnunit->f77errlist.icirlen && *icptr != ' ' && *icptr != ','; icpos++, icptr++);
      ftnunit->nextch = *icptr;
      ftnunit->f77recpos = icpos;
      *icptr = '\0';
      if (sscanf (sbuf, "%lf%c", lx, &dumy) != 1)
	 err(ftnunit->f77errlist.cierr, 112, "illegal real part in complex data");
      *icptr++ = (char) ftnunit->nextch;
   }

   while (isspace (ftnunit->nextch) || ftnunit->nextch == '\n') GETC(ftnunit->nextch);
   if ( ftnunit->nextch == ',' ) {
      GETC(ftnunit->nextch);
   } else {
      err(ftnunit->f77errlist.cierr, 112, "no comma");
   }
   while (isspace (ftnunit->nextch) || ftnunit->nextch == '\n') GETC(ftnunit->nextch);

   if (!ftnunit->f77errlist.iciunit) {
      if (quad) {
	 if (n = l_R16 (ftnunit, ly, 1)) {
	     return(n);
	 }
      } else {
	 if (n=l_R (ftnunit, ly, 1)) {
	     return(n);
	 }
      }
   } else {
      icptr--;
      for (sbuf = icptr; icpos < ftnunit->f77errlist.icirlen && *icptr != ' ' && *icptr != ')'; icpos++, icptr++);
      ftnunit->nextch = *icptr;
      ftnunit->f77recpos = icpos;
      *icptr = '\0';
      if (sscanf (sbuf, "%lf%c", ly, &dumy) != 1)
	 err(ftnunit->f77errlist.cierr, 112, "illegal imaginary part in complex data")
      *icptr++ = (char) ftnunit->nextch;
   }
   while (isspace (ftnunit->nextch)) GETC (ftnunit->nextch);

   if (ftnunit->nextch != ')') {
      err(ftnunit->f77errlist.cierr, 112, "no )")
   } else
      GETC (ftnunit->nextch);

   return (0);
}

static int l_L(unit *ftnunit, double *lx, int isnamelist)
{
   ftnll             loc=0;
   int               savechar;
   int             isblank;
   double          lvalue;
   int             isfalse_or_true = -1;

   /* This should have been taken care of in l_read */
   if (isdigit (ftnunit->nextch)) {
      ftnunit->lcount = 0;
      while (isdigit (ftnunit->nextch)) {
	 ftnunit->lcount = ftnunit->lcount * 10 + ftnunit->nextch - '0';
	 GETC (ftnunit->nextch);
      }
      if (ftnunit->nextch != '*')
	 if (ftnunit->f77errlist.iciunit || !feof (ftnunit->ufd)) {
	    err(ftnunit->f77errlist.cierr, 112, "no star")
	 } else
	    err(ftnunit->f77errlist.cierr, (EOF), "lread")
	       GETC (ftnunit->nextch);
      if (issep (ftnunit->nextch))
	 return (ftnunit->ltype=0);		/* return with ltype==NULL */
   } else
      ftnunit->lcount = 1;
   if (ftnunit->nextch == '.')
      GETC (ftnunit->nextch);
   if (isnamelist) {
      loc = FTELL (ftnunit->ufd);
      savechar = ftnunit->nextch;
   }
   switch (ftnunit->nextch) {
   case 't':
   case 'T':
      lvalue = 1;
      break;
   case 'f':
   case 'F':
      lvalue = 0;
      break;
   default:
      /* it could be namelist item */
      return (TYERROR);
   }
   ftnunit->ltype = TYINT;
   if (isnamelist) {
      /* In order to deal with the case in which the charcater we just saw was really the
       * first character in a NAMELIST variable name beginning with T or F, we need to go until
       * we hit the first non-alphanumeric character. If it's a ' ', tab or newline, we check the
       * next non-blank character. If it's an '=', we have the next variable in a namelist.
       * Otherwise, assume it's just a logical and skip to the next separator.
       */
      while (isalnum (GETC (ftnunit->nextch)));
      if (ftnunit->nextch == '.')
	 GETC (ftnunit->nextch);
      
      /* watch out for the case of namelist input where the leading
       * 'f' or 't' can be the name of a namelist variable followed
       * by '=' */
      if (!issep (ftnunit->nextch) &&
	  ftnunit->nextch != '$' &&
	  ftnunit->nextch != '!') {
	 isfalse_or_true = 0;
      } else {
	 isblank = (ftnunit->nextch == ' ' || ftnunit->nextch == '\t' ||
		    ftnunit->nextch == '\n' || ftnunit->nextch == '!');
	 if (!isblank) /* must be a comma or a / or a $ */ {
	    isfalse_or_true = 1;
	 } else {
	    if (ftnunit->nextch == '!') {
	       /* Skip to EOL */
	       while (GETC(ftnunit->nextch) != '\n' &&
		      ftnunit->nextch != EOF);
	       GETC(ftnunit->nextch);
	    }
	    while (ftnunit->nextch == ' ' || ftnunit->nextch == '\t' ||
		   ftnunit->nextch == '\n')
	      GETC (ftnunit->nextch);
	    if (ftnunit->nextch == '!') {
	       /* Skip to EOL */
	       while (GETC(ftnunit->nextch) != '\n' &&
		      ftnunit->nextch != EOF);
	       GETC(ftnunit->nextch);
	    }
	    if (ftnunit->nextch == '=' ||
		ftnunit->nextch == '(' ||
		ftnunit->nextch == '%') {
	       isfalse_or_true = 0;
	    } else {
	       isfalse_or_true = 1;
	    }
	 }
      }
      if (isfalse_or_true) {
	 *lx = lvalue;
      } else {
	 /* ignore fseek error from a terminal which will result in
	  * an error later */
	 FSEEK (ftnunit->ufd, loc, SEEK_SET);
	 ftnunit->nextch=savechar;
	 return (TYERROR);
      }
   } else {
      *lx = lvalue;
      while (!issep (GETC (ftnunit->nextch)) && ftnunit->nextch != EOF);
      if (ftnunit->nextch == '.')
	 GETC (ftnunit->nextch);
   }
   return (0);
}

static int l_CHAR(unit *ftnunit, ftnlen len, int isnamelist)
/* there were too many changes to this function for Fortran 90 to have
   small sections of differences, so entire function is unfortunately
   duplicated.

   Use the I90 version which is O.K. for F77 list-directed in the fact
   that it doesn't require delimiters.
*/
{

   int             i = 0;
   char            delim, *p;
   char firstch;
   XINT beginpos;
   XINT64 beginoff;
   char *beginptr;

   if (isdigit(ftnunit->nextch)) {

	firstch = ftnunit->nextch;

        if ( !ftnunit->f77errlist.iciunit ) {
	    beginpos = ftnunit->f77recpos;
	    beginoff = FTELL(ftnunit->ufd);
            while(isdigit(GETC(ftnunit->nextch)));
            FSEEK( ftnunit->ufd, beginoff, SEEK_SET );
	    ftnunit->f77recpos = beginpos;
	} else {
	    beginpos = icpos = ftnunit->f77recpos;
	    beginptr = icptr;
            while(isdigit(GETC(ftnunit->nextch))) {
		if ( icpos == ftnunit->f77errlist.icirlen ) {
		    ftnunit->nextch = '\n';
		    break;
		}
	    }
	    icptr = beginptr;
	    icpos = ftnunit->f77recpos = beginpos;
	}

        if (ftnunit->nextch != '*' ) {

#ifdef I90
	   /* Namelist I/O requires a delimiter */
	    if (ftnunit->f90sw == 1 && !isnamelist ) { /* assumed to be non-deliminated character string */

		ftnunit->lcount = 1;
		delim = NULL;
		ftnunit->ltype = TYCHAR;
		ftnunit->nextch = firstch;

	    } else {

		if (!ftnunit->f77errlist.iciunit || !feof (ftnunit->ufd)) {
		    err(ftnunit->f77errlist.cierr, 112, "no star")
		} else {
		    err(ftnunit->f77errlist.cierr, (EOF), "lread");
		}

	    }
#else
	   if (!ftnunit->f77errlist.iciunit || !feof (ftnunit->ufd)) {
		err(ftnunit->f77errlist.cierr, 112, "no star")
	   } else {
		err(ftnunit->f77errlist.cierr, (EOF), "lread");
	   }
#endif

	} else {  /* determining lcount should have been taken care of in l_read */

      	    ftnunit->lcount = 0;
	    ftnunit->nextch = firstch;
	    while (isdigit (ftnunit->nextch)) {
		ftnunit->lcount = ftnunit->lcount * 10 + ftnunit->nextch - '0';
		GETC (ftnunit->nextch);
	    }
	    GETC (ftnunit->nextch);  /* step over the '*' */
	    if (issep (ftnunit->nextch)) 
		return (ftnunit->ltype=0);	/* return with ltype==NULL */
	}

   } else {

      ftnunit->lcount = 1;

   }

   if (ftnunit->nextch == '\'' || ftnunit->nextch == '"') {
	delim = (char) ftnunit->nextch;
   } else {
#ifdef I90
      /* Namelist I/O requires a delimiter */
	if ( ftnunit->f90sw == 1 && !isnamelist) {
	    delim = NULL;
	    UNGETC(ftnunit->nextch);
	} else {
	    return (TYERROR);
	}
#else
	return (TYERROR);
#endif
   }

   ftnunit->ltype = TYCHAR;
   check_buflen( ftnunit, len );
   p = ftnunit->f77fio_buf;
   if ( delim != NULL ) {
      for(i=0;;) {
          while(GETC(ftnunit->nextch)!=delim) {
	     if (ftnunit->nextch==EOF) {
		return(EOF);
	     } else if (ftnunit->nextch=='\n') {
#ifdef I90
		if(i == 0 || *(p-1) != '\\' || ftnunit->f90sw == 1)
#else
		if(i == 0 || *(p-1) != '\\' )
#endif
		   continue;
		i--;
		p--;
	     } else if (++i <= len)
		*p++ = (char) ftnunit->nextch;
	  }
	  /* skip over the quote, check for two consecutive quotes */
	  if(GETC(ftnunit->nextch)==delim) {
             if (++i <= len)
		 *p++ = (char) ftnunit->nextch;
	  } else {
	     /* got the delimiting quote */
	     *p++ = 0;
	     return(0);
	  }
      }
   } else {
      /* no delimiters, just read until getting a separator */
      while( !issep(GETC(ftnunit->nextch)) ) {
         if (ftnunit->nextch==EOF) {
    	    return(EOF);
         } else if (++i <= len)
    	    *p++ = (char) ftnunit->nextch;
      }
      *p++ = 0;
      return(0);
   }
}

static
int s_rsle_com (cilist64 *a, unit **fu)
{
   int             n;
   unit		  *ftnunit;

   if (!f77init)
      f_init ();
   if (n = c_le (a, fu))
      return (n);
   ftnunit = *fu;
#ifdef I90
   ftnunit->f90sw=0;
   if (ftnunit->uaction == WRITEONLY )
       errret(a->cierr,180,"startread");
#endif
   ftnunit->l_first = 1;
   ftnunit->f77lioproc = l_read;
   ftnunit->f77getn = t_getc;
   ftnunit->f77gets = t_gets;
   ftnunit->f77ungetn = t_ungetc;
   ftnunit->lquit = 0;
   ftnunit->lcount = 0;
   if (ftnunit->ufd == stdin && feof (ftnunit->ufd) && f77vms_flag_[VMS_IN])
      clearerr(ftnunit->ufd);
   if (ftnunit->ualias->ucc == CC_FORTRAN && ftnunit->ualias->ucchar) {
      putc (ftnunit->ualias->ucchar, ftnunit->ualias->ufd);
      ftnunit->ualias->ucchar = '\0';
   }

   /* PV 291109: When iolist is empty do a GETC to check for
      empty file; if the file is non-empty do an UNGETC before
      proceeding */

   GETC(ftnunit->nextch);
   if (ftnunit->nextch == EOF)
     err(ftnunit->f77errlist.ciend, (EOF), "list in");
   UNGETC(ftnunit->nextch);
   return (f77nowreading (ftnunit));
}

int s_rsle (cilist *a)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_rsle_com(&dst, &f77curunit);
}

int s_rsle_mp (cilist *a, unit **fu)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_rsle_com(&dst, fu);
}

int s_rsle64 (cilist64 *a)
{
    return( s_rsle_com( a, &f77curunit ) );
}

int s_rsle64_mp (cilist64 *a, unit **fu)
{
    return( s_rsle_com( a, fu ) );
}

static
int s_rsli_com (icilist64 *a, unit **fu)
{
   int             n;
   unit           *ftnunit;

   if (!f77init)
      f_init ();
   f77curunit = ftnunit = *fu = Internal_File;
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   c_li (a);
#ifdef I90
   ftnunit->f90sw = 0;
#endif
   ftnunit->uwrt &= ~WR_OP;
   ftnunit->l_first = 1;
   ftnunit->f77lioproc = l_read;
   ftnunit->f77getn = z_getc;
   ftnunit->f77gets = z_gets;
   ftnunit->f77ungetn = z_ungetc;
   ftnunit->lquit = 0;
   ftnunit->lcount = 0;
   return (0);
}

int s_rsli (icilist *a)
{
  icilist64 dst;
  get_icilist64(&dst, a);
  return s_rsli_com(&dst, &f77curunit);
}

int s_rsli_mp (icilist *a, unit **fu)
{
  icilist64 dst;
  get_icilist64(&dst, a);
  return s_rsli_com(&dst, fu);
}

int s_rsli64 (icilist64 *a)
{
    return( s_rsli_com( a, &f77curunit ) );
}

int s_rsli64_mp (icilist64 *a, unit **fu)
{
    return( s_rsli_com( a, fu ) );
}


#pragma weak e_rsle64 = e_rsle
#pragma weak e_rsle64_mp = e_rsle_mp
#pragma weak e_rsli64 = e_rsli
#pragma weak e_rsli64_mp = e_rsli_mp

