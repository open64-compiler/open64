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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/wrtfmt.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
/* who	ref.		date		description			*/
/* sjc	#2113	4Dec87		coding error kills dp exponent		*/

#include <cmplrs/fio.h>
#include <ctype.h>
#include <string.h>
#include "fmt.h"
#include "ecvt_mp.h"
#include "err.h"
#include "wrtfmt.h"
#include "fmtlib.h"
#include "lio.h"
#include "wsfe.h"
#include "dfe.h"
#include "typecheck.h"
#include "../include/cmplrs/f_errno.h"

extern int fmt_check;

#include "ctype.h"
#if defined(_SYSTYPE_SVR4)
#define fp_class_d _fp_class_d
#endif
/* Fix BN 8559  ---ravi---    10/28/91 */
#include <fp_class.h>

static double exp10_array[41] = { 1e-20, 1e-19, 1e-18, 1e-17, 1e-16, 1e-15, 1e-14, 1e-13, 1e-12, 1e-11,
				  1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1,
				  1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10,
				  1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20 };


static double roundup_array[22] = { .5e-20, .5e-20, .5e-19, .5e-18, .5e-17, .5e-16, .5e-15,
				    .5e-14, .5e-13, .5e-12, .5e-11, .5e-10, .5e-9, .5e-8, .5e-7,
				    .5e-6, .5e-5, .5e-4, .5e-3, .5e-2, .5e-1, .5e0 };

static int dmax = 20; /* maximum index for exp10 and roundup macros */
#define exp10(e) exp10_array[(e)+dmax]
#define roundup(e) roundup_array[(e)+dmax+1]

static int exceed_length(unit *, int);

static int mv_cur (unit *ftnunit)
{				/* buggy, could move off front of
				 * record */
   int result;
   if (ftnunit->f77cursor > 0) {
      if (result = PUT (ftnunit->f77cursor, '\0', NULL))
	  return(result);
      ftnunit->f77cursor = 0;
   }
   if (ftnunit->f77cursor < 0) {
      if (ftnunit->f77cursor < -ftnunit->f77recpos)
	 ftnunit->f77cursor = -ftnunit->f77recpos;
      for (; ftnunit->f77cursor < 0; ftnunit->f77cursor++)
	 if ((*ftnunit->f77ungetn) (ftnunit, 0) < 0) {
	    err(CILISTERR, 106, "fmt");
	 }
   }
   return (0);
}

int wrt_I (unit *ftnunit, uinteger *n, int w, ftnlen len)
{
   int             ndigit, sign, spare;
   int             x;
   char           *ans;
   char		   buf[MAXOCTLENGTH];

   if (len == sizeof (short))
      x = n->is;
   else if (len == sizeof (char))
      x = n->ic;
   else if (len == sizeof (ftnll))
      return (wrt_LL (ftnunit, n, w));
   else
      x = n->ii;
   if (w == 0)
      w = len < 4 ? 7 : 12;
   if (exceed_length(ftnunit, w)) return(110);
   ans = icvt (x, &ndigit, &sign, buf);
   spare = w - ndigit;
   if (sign || ftnunit->f77cplus)
      spare--;
   if (spare < 0)
      PUT (w, '*', NULL);
   else {
      PUT (spare, ' ', NULL);
      if (sign)
	 PUT (1, '-', NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);
      PUT (ndigit, 0, ans);
   }
   return (0);
}

int wrt_LL (unit *ftnunit, uinteger *n, int w)
{
   int             ndigit, sign, spare;
   ftnll           x;
   char           *ans;
   char		   buf[MAXOCTLENGTH];

   x = n->ill;
   if (w == 0)
      w = 21;
   if (exceed_length(ftnunit, w)) return(110);
   ans = llcvt (x, &ndigit, &sign, buf);
   spare = w - ndigit;
   if (sign || ftnunit->f77cplus)
      spare--;
   if (spare < 0)
      PUT (w, '*', NULL);
   else {
      PUT (spare, ' ', NULL);
      if (sign)
	 PUT (1, '-', NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);
      PUT (ndigit, 0, ans);
   }
   return (0);
}

static int wrt_OZ (unit *ftnunit, unsigned char *n, int w, ftnlen len, int base)
{
   char           *ans;
   int             ndigit, spare, shift;
   char		   buf[MAXOCTLENGTH];

   shift = base == 8 ? 3 : 4;
   if (w == 0)
      w = len < 4 ? 7 : (len < 8 ? 12 :
		    (len > 8 ? (len * 8 + shift - 1) / shift : 23));
   if (exceed_length(ftnunit, w)) return(110);
   ans = ozcvt (n, len, &ndigit, base, buf);
   spare = w - ndigit;
   if (spare < 0)
      PUT (w, '*', NULL);
   else {
      PUT (spare, ' ', NULL);
      PUT (ndigit, 0, ans);
   }
   return (0);
}

static int wrt_OZM (unit *ftnunit, unsigned char *n, int w, int m, ftnlen len, int base)
{
   char           *ans;
   int             ndigit, spare;
   char		   buf[MAXOCTLENGTH];

   ans = ozcvt (n, len, &ndigit, base, buf);
   if (ndigit >= m)
      spare = w - ndigit;
   else
      spare = w - m;
   if (exceed_length(ftnunit, w)) return(110);

   if (spare < 0)
      PUT (w, '*', NULL);
   else {
      PUT (spare, ' ', NULL);
      if (m > ndigit)
	 PUT (m - ndigit, '0', NULL);
      PUT (ndigit, 0, ans);
   }
   return (0);
}

static int wrt_LLM (unit *ftnunit, uinteger *n, int w, int m)
{
   int             ndigit, sign, spare, xsign;
   ftnll           x;
   char           *ans;
   char		   buf[MAXOCTLENGTH];

   x = n->ill;
   if (exceed_length(ftnunit, w)) return(110);
   ans = llcvt (x, &ndigit, &sign, buf);
   if (sign || ftnunit->f77cplus)
      xsign = 1;
   else
      xsign = 0;
   if (ndigit + xsign > w || m + xsign > w) {
      PUT (w, '*', NULL);
      return (0);
   }
   if (x == 0 && m == 0) {
      PUT (w, ' ', NULL);
      return (0);
   }
   if (ndigit >= m)
      spare = w - ndigit - xsign;
   else
      spare = w - m - xsign;
   PUT (spare, ' ', NULL);
   if (sign)
      PUT (1, '-', NULL);
   else if (ftnunit->f77cplus)
      PUT (1, '+', NULL);
   if (m > ndigit)
      PUT (m - ndigit, '0', NULL);
   PUT (ndigit, 0, ans);
   return (0);
}
static int wrt_IM (unit *ftnunit, uinteger *n, int w, int m, ftnlen len)
{
   int             ndigit, sign, spare, xsign;
   int             x;
   char           *ans;
   char		   buf[MAXOCTLENGTH];

   if (exceed_length(ftnunit, w)) return(110);
   if (sizeof (short) == len)
      x = n->is;
   else if (len == sizeof (char))
      x = n->ic;
   else if (len == sizeof (ftnll))
      return (wrt_LLM (ftnunit, n, w, m));
   else
      x = n->ii;
   ans = icvt (x, &ndigit, &sign, buf);
   if (sign || ftnunit->f77cplus)
      xsign = 1;
   else
      xsign = 0;
   if (ndigit + xsign > w || m + xsign > w) {
      PUT (w, '*', NULL);
      return (0);
   }
   if (x == 0 && m == 0) {
      PUT (w, ' ', NULL);
      return (0);
   }
   if (ndigit >= m)
      spare = w - ndigit - xsign;
   else
      spare = w - m - xsign;
   PUT (spare, ' ', NULL);
   if (sign)
      PUT (1, '-', NULL);
   else if (ftnunit->f77cplus)
      PUT (1, '+', NULL);
   if (m > ndigit)
      PUT (m - ndigit, '0', NULL);
   PUT (ndigit, 0, ans);
   return (0);
}


static int wrt_AP (unit *ftnunit, char *s)
{
   char           quote;
   int result;

   if (result = mv_cur (ftnunit))
      return (result);
   quote = *s++;
   for (; *s && !result; s++) {
      if (*s != quote)
	 result = PUT (1, *s, NULL);
      else if (*++s == quote)
	 result = PUT (1, quote, NULL);
      else
	 return (0);
   }
   return (result);
}

static int wrt_H (unit *ftnunit, int a, char *s)
{
   int result;

   if (result = mv_cur (ftnunit))
      return (result);
   result = PUT (a, 0, s);
   return(result);
}

int wrt_L (unit *ftnunit, uinteger *n, int len, ftnlen sz)
{
   int             i;
   ftnll           x;

   if (sizeof (short) == sz)
      x = n->is;
   else if (sz == sizeof (char))
      x = n->ic;
   else if (sz == sizeof (ftnll))
      x = n->ill;
   else
      x = n->ii;
   len = len ? len : 2;
   for (i = 0; i < len - 1; i++)
      PUT (1, ' ', NULL);
   if (x)
      return(PUT (1, 'T', NULL));
   else
      return(PUT (1, 'F', NULL));
}

static int wrt_A (unit *ftnunit, char *p, ftnlen len)
{
   return(PUT (len, 0, p));
}

static int wrt_AW (unit *ftnunit, char *p, int w, ftnlen len)
{
   if (exceed_length(ftnunit, w)) return(110);
   if (w > len) {
      PUT (w - len, ' ', NULL);
      PUT (len, 0, p);
   } else
      PUT (w, 0, p);
   return (0);
}

static int 
wrt_E (unit *ftnunit, ufloat *p, int w, int d, int e, ftnlen len, char symbol, flag doblank)
{
   char           *s = NULL;
   int             dp, absdp, sign, i, delta, pow10, leading0;
   double          dd;
   char			buffer[100];

   if (len == sizeof (float))
      dd = p->pf;
   else
      dd = p->pd;
   if (w == 0) {
      if (len == 4) {
	 w = 15;
	 d = 7;
      } else {
	 w = 25;
	 d = 16;
      }
      e = 2;
   }
   if (exceed_length(ftnunit, w)) return(110);
   dp = ftnunit->f77scale > 0 ? d + 1 : d + ftnunit->f77scale;
/* 
 * BN 8559 . There is a problem with ecvt ( from libc ) and if the number dd
 * passed to it is Infinity -ve or positive and dp ( the length of the
 * string passed back ) is less than 8, then the string "Infinity" gets
 * truncated. Rather than try to fix this in ecvt  , have akludge here to
 * take care of the problem. Test to see if dd is inf. and if it is set the
 * string s = "infinity"
 * ---ravi---   10/28/91
  
	if ( dp > 0 ) s=ecvt_mp( dd ,dp,&dp,&sign, buffer);
  
*/
   if (dp > 0) {
      s = ecvt_mp (dd, dp, &dp, &sign, buffer);
      if (fp_class_d (dd) == FP_POS_INF)
	 s = "Inf";
      else if (fp_class_d (dd) == FP_NEG_INF)
	 s = "-Inf";

   }
/* Fix BN 11222.
 * Make sure s is not NULL before dereferencing it.
 * ---ravi---1/27/92
 */
   /* if (!isdigit(*s)) { *//* just print Infinity, Nan, etc. */
   if (s && !isdigit (*s)) {	/* just print Infinity, Nan, etc. */
      PUT ((i = (int) strlen (s)), 0, s);
      if (doblank && w > i)
	 PUT (w - i, ' ', NULL);/* pad to proper width */
   } else {			/* do the normal thing */
      if (dd != 0) {		/* sjc #2113 */
	 dp -= ftnunit->f77scale;
	 absdp = dp < 0 ? -dp : dp;
      } else
	 absdp = dp = 0;
      delta = e ? e + 3 : 5;
      if (sign || ftnunit->f77cplus)
	 delta++;		/* AGC 10/29/86 */
      if (ftnunit->f77scale > 0)
	 delta++;
      if ((w > delta + d) && (ftnunit->f77scale <= 0)) {
	 delta++;
	 leading0 = 1;
      } else
	 leading0 = 0;
      if ((w < delta + d) 
	   || (e == 1 && absdp > 9) 
	   || (e == 2 && absdp > 99) 
	   || (ftnunit->f77scale <= 0 && ftnunit->f77scale <= -d) 
	   || (ftnunit->f77scale > 0 && ftnunit->f77scale > d + 1)) {
	 PUT (w, '*', NULL);
	 return (0);
      }
      if (doblank && w > (delta + d))
	 PUT (w - (delta + d), ' ', NULL);
      if (sign)
	 PUT (1, (char) (dd==0 ? ' ' : '-'), NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);
      if (ftnunit->f77scale <= 0) {
	 if (leading0)
	    PUT (1, '0', NULL);
	 PUT (1, '.', NULL);
	 PUT (-ftnunit->f77scale, '0', NULL);
	 if ((d + ftnunit->f77scale) > 0)
	    PUT (d + ftnunit->f77scale, 0, s);
      } else {
	 PUT (ftnunit->f77scale, 0, s);
	 PUT (1, '.', NULL);
/* 6/26/89
 * fix bug 4547
 */
	 if (d >= ftnunit->f77scale)
	    PUT (d - ftnunit->f77scale + 1, 0, s + ftnunit->f77scale);
      }
      if ((e > 0) || (absdp < 100))
	 PUT (1, symbol, NULL);
      PUT (1, (char) (dp < 0 ? '-' : '+'), NULL);
      if (!e) {
	 /* For the common case this is much faster than the general
	    algorithm
	 */
         if (absdp > 99.0) {
	     PUT (1, (char) ((i = absdp / 1e2 ) + '0'), NULL);
	     absdp -= i * 1e2;
         }
         PUT (1, (char) ((i = absdp / 1e1)  + '0'), NULL);
         absdp -= i * 1e1;
         PUT (1, (char) ((i = absdp) + '0'), NULL);
      }
      else {
         for (pow10 = 1, i = e; --i; pow10 *= 10);
         while (e--) {
	    PUT (1, (char) ((i = absdp / pow10) + '0'), NULL);
	    absdp -= i * pow10;
	    pow10 /= 10;
         }
      }

   }
   return (0);
}

static int 
wrt_EQ (unit *ftnunit, ufloat *p, int w, int d, int e, ftnlen notused_len, char symbol, flag doblank)
{
   char           *s = NULL;
   int             dp, absdp, sign, i, delta, pow10, leading0;
   long double     dd;
   char			buffer[100];

   dd = p->pld;
   if (w == 0) {
      w = 40;
      d = 31;
      e = 2;
   }
   if (exceed_length(ftnunit, w)) return(110);
   dp = ftnunit->f77scale > 0 ? d + 1 : d + ftnunit->f77scale;
   if (dp > 0) {
      s = qecvt_mp (dd, dp, &dp, &sign, buffer);
      if (fp_class_d (dd) == FP_POS_INF)
	 s = "Inf";
      else if (fp_class_d (dd) == FP_NEG_INF)
	 s = "-Inf";

   }
   if (s && !isdigit (*s)) {	/* just print Infinity, Nan, etc. */
      PUT ((i = (int) strlen (s)), 0, s);
      if (doblank && w > i)
	 PUT (w - i, ' ', NULL);/* pad to proper width */
   } else {			/* do the normal thing */
      if (dd != 0) {		/* sjc #2113 */
	 dp -= ftnunit->f77scale;
	 absdp = dp < 0 ? -dp : dp;
      } else
	 absdp = dp = 0;
      delta = e ? e + 3 : 5;
      if (sign || ftnunit->f77cplus)
	 delta++;		/* AGC 10/29/86 */
      if (ftnunit->f77scale > 0)
	 delta++;
      if ((w > delta + d) && (ftnunit->f77scale <= 0)) {
	 delta++;
	 leading0 = 1;
      } else
	 leading0 = 0;
      if ((w < delta + d) 
	  || (e == 1 && absdp > 9) 
	  || (e == 2 && absdp > 99) 
	  || (ftnunit->f77scale <= 0 && ftnunit->f77scale <= -d) 
	  || (ftnunit->f77scale > 0 && ftnunit->f77scale > d + 1)) {
	 PUT (w, '*', NULL);
	 return (0);
      }
      if (doblank && w > (delta + d))
	 PUT (w - (delta + d), ' ', NULL);
      if (sign)
	 PUT (1, (char) (dd==0 ? ' ' : '-'), NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);
      if (ftnunit->f77scale <= 0) {
	 if (leading0)
	    PUT (1, '0', NULL);
	 PUT (1, '.', NULL);
	 PUT (-ftnunit->f77scale, '0', NULL);
	 if ((d + ftnunit->f77scale) > 0)
	    PUT (d + ftnunit->f77scale, 0, s);
      } else {
	 PUT (ftnunit->f77scale, 0, s);
	 PUT (1, '.', NULL);
/* 6/26/89
 * fix bug 4547
 */
	 if (d >= ftnunit->f77scale)
	    PUT (d - ftnunit->f77scale + 1, 0, s + ftnunit->f77scale);
      }
      if ((e > 0) || (absdp < 100))
	 PUT (1, symbol, NULL);
      PUT (1, (char) (dp < 0 ? '-' : '+'), NULL);
      if (!e)
	 e = absdp > 99 ? 3 : 2;
      for (pow10 = 1, i = e; --i; pow10 *= 10);
      while (e--) {
	 PUT (1, (char) ((i = absdp / pow10) + '0'), NULL);
	 absdp -= i * pow10;
	 pow10 /= 10;
      }
   }
   return (0);
}

#ifdef I90
static int wrt_BM (unit *ftnunit, unsigned char *n, int w, int m, ftnlen len)
{
   char           *ans;
   int             ndigit, spare;
   char		   buf[MAXOCTLENGTH*3+1];

   if (exceed_length(ftnunit, w)) return(110);

   ans = bcvt (n, len, &ndigit, buf);

   if (ndigit > m )
      spare = w - ndigit;
   else
      spare = w - m;

   if (spare < 0)
      PUT (w, '*', NULL);
   else {
      if ( spare > 0 ) PUT (spare, ' ', NULL);
      if ( m > ndigit ) PUT (m - ndigit, '0', NULL);
      if ( ndigit > 0 ) PUT (ndigit, 0, ans);
   }
   return (0);
}

static int wrt_EN (unit *ftnunit, ufloat *p, int w, int d, int e, ftnlen len, char symbol, flag doblank)
{
   char           *s = NULL;
   int             dp, absdp, sign, i;
   double          dd;
   char			buffer[100];
   int		   left_digits = 3; /* maximum value */
   int		   spaces_needed;

   if (len == sizeof (float))
      dd = p->pf;
   else
      dd = p->pd;

   dp = d + left_digits;

   if (dp > 0) {
      s = ecvt_mp (dd, dp, &dp, &sign, buffer);
      if (fp_class_d (dd) == FP_POS_INF)
	 s = "Inf";
      else if (fp_class_d (dd) == FP_NEG_INF)
	 s = "-Inf";
   }

   if (s && !isdigit (*s)) {	/* just print Infinity, Nan, etc. */
      PUT ((i = (int) strlen (s)), 0, s);
      if (doblank && w > i) PUT (w - i, ' ', NULL);/* pad to proper width */

   } else {			/* do the normal thing */

      if (dd != 0) {
	 if ( dp > 0 ) {
            left_digits = (dp-1) % 3 + 1;
	 } else {
            left_digits = 3 + dp % 3;
	 }
	 if ( left_digits < 3 ) { /* maybe roundoff error */
	    dp = d + left_digits; /* try again with smaller number of significant digits */
	    s = ecvt_mp (dd, dp, &dp, &sign, buffer);
	    if ( dp > 0 ) {
		left_digits = (dp-1) % 3 + 1;
	    } else {
		left_digits = 3 + dp % 3;
	    }
	 }
         dp = dp - left_digits;  /* interpret string s as   x.xxxxxxxE+3n */
						    /* or  xx.xxxxxxxE+3n */
						    /* or xxx.xxxxxxxE+3n */
      } else {
	 dp = 0;
	 left_digits = 1;
      }
      if (dd != 0) {		/* sjc #2113 */
	 dp -= ftnunit->f77scale;
	 absdp = dp < 0 ? -dp : dp;
      } else
	 absdp = dp = 0;

      if ( e == 0 && absdp > 999 ) return(100);

      spaces_needed = e ? e + 3 : 5;

      if (sign || ftnunit->f77cplus) spaces_needed++;

      spaces_needed += d + left_digits;

      if ((w < spaces_needed) 
	  || (e == 1 && absdp > 9) 
	  || (e == 2 && absdp > 99)) {
	 PUT (w, '*', NULL);
	 return (0);
      }

      if (doblank && w > spaces_needed)
	 PUT (w - spaces_needed, ' ', NULL);

      if (sign)
	 PUT (1, (char) (dd==0 ? ' ' : '-'), NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);

      PUT (left_digits, 0, s);
      PUT (1, '.', NULL);
      PUT (d, 0, &s[left_digits]);

      if ((e > 0) || (absdp < 100)) PUT (1, symbol, NULL);

      PUT (1, (char) (dp < 0 ? '-' : '+'), NULL);

      if (!e) e = absdp > 99 ? 3 : 2;

      while (e--) {
	 PUT (1, (char) ((i = absdp / exp10(e)) + '0'), NULL);
	 absdp -= i * exp10(e);
      }

   }
   return (0);
}

static int wrt_ENQ (unit *ftnunit, ufloat *p, int w, int d, int e, ftnlen notused_len, char symbol, flag doblank)
{
   char           *s = NULL;
   int             dp, absdp, sign, i;
   long double     dd;
   char			buffer[100];
   int		   left_digits = 3; /* maximum value */
   int		   spaces_needed;

   dd = p->pld;

   dp = d + left_digits;

   if (dp > 0) {
      s = qecvt_mp (dd, dp, &dp, &sign, buffer);
      if (fp_class_d (dd) == FP_POS_INF)
	 s = "Inf";
      else if (fp_class_d (dd) == FP_NEG_INF)
	 s = "-Inf";
   }

   if (s && !isdigit (*s)) {	/* just print Infinity, Nan, etc. */
      PUT ((i = (int) strlen (s)), 0, s);
      if (doblank && w > i)
	 PUT (w - i, ' ', NULL);/* pad to proper width */

   } else {			/* do the normal thing */

      if (dd != 0) {
	 if ( dp > 0 ) {
            left_digits = (dp-1) % 3 + 1;
	 } else {
            left_digits = 3 + dp % 3;
	 }
	 if ( left_digits < 3 ) { /* maybe roundoff error */
	    dp = d + left_digits; /* try again with smaller number of significant digits */
	    s = qecvt_mp (dd, dp, &dp, &sign, buffer);
	    if ( dp > 0 ) {
		left_digits = (dp-1) % 3 + 1;
	    } else {
		left_digits = 3 + dp % 3;
	    }
	 }
         dp = dp - left_digits;  /* interpret string s as   x.xxxxxxxE+3n */
						    /* or  xx.xxxxxxxE+3n */
						    /* or xxx.xxxxxxxE+3n */
      } else {
	 dp = 0;
	 left_digits = 1;
      }
      if (dd != 0) {		/* sjc #2113 */
	 dp -= ftnunit->f77scale;
	 absdp = dp < 0 ? -dp : dp;
      } else
	 absdp = dp = 0;

      if ( e == 0 && absdp > 999 ) return(100);

      spaces_needed = e ? e + 3 : 5;

      if (sign || ftnunit->f77cplus) spaces_needed++;

      spaces_needed += d + left_digits;

      if ((w < spaces_needed) 
	  || (e == 1 && absdp > 9) 
	  || (e == 2 && absdp > 99)) {
	 PUT (w, '*', NULL);
	 return (0);
      }

      if (doblank && w > spaces_needed )
	 PUT (w - spaces_needed, ' ', NULL);

      if (sign)
	 PUT (1, (char) (dd==0 ? ' ' : '-'), NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);

      PUT (left_digits, 0, s);
      PUT (1, '.', NULL);
      PUT (d, 0, &s[left_digits]);

      if ((e > 0) || (absdp < 100)) PUT (1, symbol, NULL);

      PUT (1, (char) (dp < 0 ? '-' : '+'), NULL);

      if (!e) e = absdp > 99 ? 3 : 2;

      while (e--) {
	 PUT (1, (char) ((i = absdp / exp10(e)) + '0'), NULL);
	 absdp -= i * exp10(e);
      }

   }
   return (0);
}

static int wrt_ES (unit *ftnunit, ufloat *p, int w, int d, int e, ftnlen len, char symbol, flag doblank)
{
   char           *s = NULL;
   int             dp, absdp, sign, i;
   double          dd;
   char			buffer[100];
   int		   left_digits = 1; /* maximum value */
   int             spaces_needed;

   if (len == sizeof (float))
      dd = p->pf;
   else
      dd = p->pd;

   dp = d + left_digits;

   if (dp > 0) {
      s = ecvt_mp (dd, dp, &dp, &sign, buffer);
      if (fp_class_d (dd) == FP_POS_INF)
	 s = "Inf";
      else if (fp_class_d (dd) == FP_NEG_INF)
	 s = "-Inf";
   }

   if (s && !isdigit (*s)) {	/* just print Infinity, Nan, etc. */
      PUT ((i = (int) strlen (s)), 0, s);
      if (doblank && w > i) PUT (w - i, ' ', NULL);/* pad to proper width */

   } else {			/* do the normal thing */

      dp = dp - left_digits;  /* interpret string s as x.xxxxxxx */
      if (dd != 0) {		/* sjc #2113 */
	 dp -= ftnunit->f77scale;
	 absdp = dp < 0 ? -dp : dp;
      } else
	 absdp = dp = 0;

      if ( e == 0 && absdp > 999 ) return(100);

      spaces_needed = e ? e + 3 : 5;

      if (sign || ftnunit->f77cplus) spaces_needed++;

      spaces_needed += d + left_digits;

      if ((w < spaces_needed) 
	  || (e == 1 && absdp > 9) 
	  || (e == 2 && absdp > 99)) {
	 PUT (w, '*', NULL);
	 return (0);
      }

      if (doblank && w > spaces_needed)
	 PUT (w - spaces_needed, ' ', NULL);

      if (sign)
	 PUT (1, (char) (dd==0 ? ' ' : '-'), NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);

      PUT (left_digits, 0, s);
      PUT (1, '.', NULL);
      PUT (d, 0, &s[left_digits]);

      if ((e > 0) || (absdp < 100)) PUT (1, symbol, NULL);

      PUT (1, (char) (dp < 0 ? '-' : '+'), NULL);

      if (!e) e = absdp > 99 ? 3 : 2;

      while (e--) {
	 PUT (1, (char) ((i = absdp / exp10(e)) + '0'), NULL);
	 absdp -= i * exp10(e);
      }

   }
   return (0);
}

static int wrt_ESQ (unit *ftnunit, ufloat *p, int w, int d, int e, ftnlen notused_len, char symbol, flag doblank)
{
   char           *s = NULL;
   int             dp, absdp, sign, i;
   long double     dd;
   char			buffer[100];
   int		   left_digits = 1; /* maximum value */
   int             spaces_needed;

   dd = p->pld;

   dp = d + left_digits;

   if (dp > 0) {
      s = qecvt_mp (dd, dp, &dp, &sign, buffer);
      if (fp_class_d (dd) == FP_POS_INF)
	 s = "Inf";
      else if (fp_class_d (dd) == FP_NEG_INF)
	 s = "-Inf";
   }

   if (s && !isdigit (*s)) {	/* just print Infinity, Nan, etc. */
      PUT ((i = (int) strlen (s)), 0, s);
      if (doblank && w > i)
	 PUT (w - i, ' ', NULL);/* pad to proper width */

   } else {			/* do the normal thing */

      dp = dp - left_digits;  /* interpret string s as x.xxxxxxx */
      if (dd != 0) {		/* sjc #2113 */
	 dp -= ftnunit->f77scale;
	 absdp = dp < 0 ? -dp : dp;
      } else
	 absdp = dp = 0;

      if ( e == 0 && absdp > 999 ) return(100);

      spaces_needed = e ? e + 3 : 5;

      if (sign || ftnunit->f77cplus) spaces_needed++;

      spaces_needed += d + left_digits;

      if ((w < spaces_needed) 
	  || (e == 1 && absdp > 9) 
	  || (e == 2 && absdp > 99)) {
	 PUT (w, '*', NULL);
	 return (0);
      }

      if (doblank && w > spaces_needed)
	 PUT (w - spaces_needed, ' ', NULL);

      if (sign)
	 PUT (1, (char) (dd==0 ? ' ' : '-'), NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);

      PUT (1, s[0],  NULL);
      PUT (1,  '.',  NULL);
      PUT (d,    0, &s[1]);

      if ((e > 0) || (absdp < 100)) PUT (1, symbol, NULL);

      PUT (1, (char) (dp < 0 ? '-' : '+'), NULL);

      if (!e) e = absdp > 99 ? 3 : 2;

      while (e--) {
	 PUT (1, (char) ((i = absdp / exp10(e)) + '0'), NULL);
	 absdp -= i * exp10(e);
      }

   }
   return (0);
}
#endif

static int 
wrt_F (unit *ftnunit, ufloat *p, int w, int d, ftnlen len, flag doblank)
{
   int             i, delta, dp, sign, n, leading0;
   double          x;
   /* extern FILE *debugfile; */
   char			buffer[100];
   char           *s;

   x = (len == sizeof (float) ? p->pf : p->pd);
   if (w == 0) {
      if (len == 4) {
	 w = 15;
	 d = 7;
      } else {
	 w = 25;
	 d = 16;
      }
   }
   if (exceed_length(ftnunit, w)) return(110);
   if (ftnunit->f77scale) {
      if (ftnunit->f77scale > 0)
	 for (i = 0; i < ftnunit->f77scale; i++)
	    x *= 10;
      else
	 for (i = 0; i < -ftnunit->f77scale; i++)
	    x /= 10;
   }
   s = fcvt_mp (x, d, &dp, &sign, buffer);
/* fprintf(debugfile, "Writing %s for thread %d\n", s, mp_my_threadnum_()); */
/* Fix BN 11269. 
 * If the the string s does not contain a digit as the first character , then
 * the result is either "Infinity", or "NaN" . Print this out and do not
 * process the string according format specification.
 * ---ravi---
 */
/* Fix BN 11584. There is a possibility that d may be 0 in which case the string
 * s is empty and then the above test suceeds which should not happen. So add
 * the conditon that d should be non-zero.
 * ---ravi---1/29/92
 */
   if (!isdigit (*s) && strlen (s)) {	/* just print Infinity, Nan,
					 * etc. */
      PUT ((i = (int) strlen (s)), 0, s);
      if (doblank && w > i)
	 PUT (w - i, ' ', NULL);/* pad to proper width */
   } else {
      if (-dp >= d)
	 sign = 0;
      if (sign || ftnunit->f77cplus)
	 delta = 2;
      else
	 delta = 1;
      n = w - (d + delta + (dp > 0 ? dp : 0));
      if (n > 0 && dp <= 0) {
	 leading0 = 1;
	 n--;
      }
/* 9/28/89 fix bug 5046 */
      else if (!strlen (s) && !d && dp == 1)	/* taking care of the
						 * case */
	 leading0 = 1;		/* where x = 0 and d = 0 */
      else
	 leading0 = 0;
      /* fix for zero to print as '.0' with fmt f2.1 */
      if (n == -1 &&		/* missed by one */
	  dp == 1 &&		/* There is a character before the
				 * decimal pt */
	  *s == '0') {		/* and it is a '0' */
	 n = dp = 0;
	 s++;			/* skip leading 0 and start with the
				 * dec pt */
      }
      if (n < 0) {
	 PUT (w, '*', NULL);
	 return (0);
      }
      if (doblank)
	 PUT (n, ' ', NULL);
      if (sign)
	 PUT (1, (char) (x==0 ? ' ' : '-'), NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);
      if (leading0)
	 PUT (1, '0', NULL);
      else if (dp > 0) {
	 PUT (dp, 0, s);
	 s += dp;
      }
      PUT (1, '.', NULL);
      i = (-dp) < d ? -dp : d;
      if (i > 0) {
	 PUT (i, '0', NULL);
	 d -= i;
      }
      if (d) {
	 if ((i = (int) strlen (s))) {
	    PUT (i > d ? d : i, 0, s);
	    if (d > i)
	       PUT (d - i, '0', NULL);
	 } else
	    PUT (d, '0', NULL);
      }
   }				/* end else */
   return (0);
}

static int 
wrt_FQ (unit *ftnunit, ufloat *p, int w, int d, flag doblank)
{
   int             i, delta, dp, sign, n, leading0;
   long double     x;
   char           *s;
   char			buffer[100];

   x = p->pld;
   if (w == 0) {
      w = 40;
      d = 31;
   }
   if (exceed_length(ftnunit, w)) return(110);
   if (ftnunit->f77scale) {
      if (ftnunit->f77scale > 0)
	 for (i = 0; i < ftnunit->f77scale; i++)
	    x *= 10;
      else
	 for (i = 0; i < -ftnunit->f77scale; i++)
	    x /= 10;
   }
   s = qfcvt_mp (x, d, &dp, &sign, buffer);
/* Fix BN 11584. There is a possibility that d may be 0 in which case the string
 * s is empty and then the above test suceeds which should not happen. So add
 * the conditon that d should be non-zero.
 * ---ravi---1/29/92
 */
   if (!isdigit (*s) && strlen (s)) {	/* just print Infinity, Nan,
					 * etc. */
      PUT ((i = (int) strlen (s)), 0, s);
      if (doblank && w > i)
	 PUT (w - i, ' ', NULL);/* pad to proper width */
   } else {
      if (-dp >= d)
	 sign = 0;
      if (sign || ftnunit->f77cplus)
	 delta = 2;
      else
	 delta = 1;
      n = w - (d + delta + (dp > 0 ? dp : 0));
      if (n > 0 && dp <= 0) {
	 leading0 = 1;
	 n--;
      }
/* 9/28/89 fix bug 5046 */
      else if (!strlen (s) && !d && dp == 1)	/* taking care of the
						 * case */
	 leading0 = 1;		/* where x = 0 and d = 0 */
      else
	 leading0 = 0;
      /* fix for zero to print as '.0' with fmt f2.1 */
      if (n == -1 &&		/* missed by one */
	  dp == 1 &&		/* There is a character before the
				 * decimal pt */
	  *s == '0') {		/* and it is a '0' */
	 n = dp = 0;
	 s++;			/* skip leading 0 and start with the
				 * dec pt */
      }
      if (n < 0) {
	 PUT (w, '*', NULL);
	 return (0);
      }
      if (doblank)
	 PUT (n, ' ', NULL);
      if (sign)
	 PUT (1, (char) (x==0 ? ' ' : '-'), NULL);
      else if (ftnunit->f77cplus)
	 PUT (1, '+', NULL);
      if (leading0)
	 PUT (1, '0', NULL);
      else if (dp > 0) {
	 PUT (dp, 0, s);
	 s += dp;
      }
      PUT (1, '.', NULL);
      i = (-dp) < d ? -dp : d;
      if (i > 0) {
	 PUT (i, '0', NULL);
	 d -= i;
      }
      if (d) {
	 if ((i = (int) strlen (s))) {
	    PUT (i > d ? d : i, 0, s);
	    if (d > i)
	       PUT (d - i, '0', NULL);
	 } else
	    PUT (d, '0', NULL);
      }
   }				/* end else */
   return (0);
}


int wrt_G (unit *ftnunit, void *ptr, int op, int w, int d, int e, ftnlen len, ftnint type, flag doblank)
{
    double	upper, lower, x;
    int		i, n, nd, ierr;
    short	oldscale = ftnunit->f77scale;
    ufloat	*p = (ufloat *) ptr;

    switch (type) {
    default:
/*
	fprintf (stderr, "w_ed, bad variable type: %d\n%s\n", type, ftnunit->f77fmtbuf);
*/
	err(CILISTERR, 117, "fmt");
    case TYCHAR:
	if (w) {
		return (wrt_AW (ftnunit, (char *)ptr, w, len));
	} else {
		return (wrt_A (ftnunit, (char *)ptr, len));
	}
    case TYLOGICAL1:
    case TYLOGICAL2:
    case TYLOGICAL4:
    case TYLOGICAL8:
	return (wrt_L (ftnunit, (uinteger *) ptr, w, len));
    case TYBYTE:
    case TYSHORT:
    case TYINT:
    case TYLONGLONG:
	return (wrt_I (ftnunit, (uinteger *) ptr, w, len));
    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
    case TYQUAD:
    case TYQUADCOMPLEX:
	x = (len < sizeof (double)) ? p->pf : (len == sizeof (double)) ? p->pd : (double) p->pld;
	if (w == 0) {
	    if (len < 8) {
		w = 15;
		d = 7;
	    } else if (len == 8) {
		w = 25;
		d = 16;
	    } else {
		w = 40;
		d = 31;
	    }
	    e = 2;
	}

	if ( d > dmax ) {	/* handle cases when d > dmax */
	    upper = exp10(dmax);
	    lower = roundup(-dmax);
	    nd = dmax;
	    while ( d - nd > dmax-1 ) {
		upper *= exp10(dmax);
		lower *= exp10(-dmax);
		nd += dmax;
	    }
	    upper *= exp10(d-nd);
	    lower *= exp10(-d+nd-1);
 	    upper = upper - 0.5;
	    lower = .1 - lower;
	} else {
	    upper = exp10(d) - roundup(0);
	    lower = exp10(-1) - roundup(-d-1);
	}
 
	if (x < 0) x = -x;

	if ( (x == 0 && ftnunit->f90sw) || ( lower <= x && x < upper ) ) { /* range for effective use of F editing */

	    ftnunit->f77scale = 0;
	    if (e == 0) {
		    n = 4;
	    } else {
		    n = e + 2;
	    }

	    for ( i = 0; i <= d; ++i ) {

		if ( i > dmax ) {	/* handle cases when i > dmax */
		    upper = exp10(dmax);
		    nd = dmax;
		    while ( i - nd > dmax ) {
			upper *= exp10(dmax);
			nd += dmax;
		    }
		    upper *= exp10(i-nd);
		} else {
		    upper = exp10(i);
		}
		if ( d - i > dmax ) {	/* handle cases when d - i > dmax */
		    lower = roundup(-dmax);
		    nd = dmax;
		    while ( d - i - nd > dmax ) {
			lower *= exp10(-dmax);
			nd += dmax;
		    }
		    lower *= exp10(i-d+nd);
		} else {
		    lower = roundup(i-d);
		}
		upper = upper - lower;

		if ( x < upper || i == d  ) {

		    if (len > 8) {
			ierr = wrt_FQ (ftnunit, p, w - n, ( x==0 ? d-1 : d-i ), doblank);
		    } else {
			ierr = wrt_F (ftnunit, p, w - n, ( x==0 ? d-1 : d-i ), len, doblank);
		    }
		    if (doblank) PUT (n, ' ', NULL);
		    ftnunit->f77scale = oldscale;
		    return (ierr);

		} /* endif */

	    } /* endloop */

	} else { /* exponential notation */

	    if ( op == (int)GE && e == 0 ) {
		err(CILISTERR, 100, "fmt");
	    }

	    if (len > 8) {
		    return (wrt_EQ (ftnunit, p, w, d, e, len, 'E', doblank));
	    } else {
		    return (wrt_E (ftnunit, p, w, d, e, len, 'E', doblank));
	    }

	}

    } /* switch */

    return (0);
}


int w_ed (unit *ftnunit, struct f77syl *p, char *ptr, ftnlen len, ftnint type)
{
   if (mv_cur (ftnunit))
      return (mv_cur (ftnunit));
#ifdef I90
	if (ftnunit->f90sw != 0 ) {
	    if (ftnunit->url > 0 ) {
		if ( p->op == A || p->op == AW ) {
		    if (ftnunit->f77recpos + len > ftnunit->url )
			err(ftnunit->f77errlist.cierr,110,"fmt");
		} else {
		    if (ftnunit->f77recpos + p->p1 > ftnunit->url )
			err(ftnunit->f77errlist.cierr,110,"fmt");
		}
	    }
	    if ( test_type(p->op,type) != 0 )
		err(ftnunit->f77errlist.cierr,117,"wrtfmt");
	}
#endif

   if (fmt_check && _WCHK[p->op][type]) {
      err(CILISTERR, F_TYPECONFLICT, "formatted write");
   }
   switch (p->op) {
   default:
/*
      fprintf (stderr, "w_ed, unexpected code: %d\n%s\n",
	       p->op, ftnunit->f77fmtbuf);
*/
      err(CILISTERR, 167, "fmt");
   case I:
      return (wrt_I (ftnunit, (uinteger *) ptr, p->p1, len));
   case IM:
      return (wrt_IM (ftnunit, (uinteger *) ptr, p->p1, p->p2, len));
   case O:
      return (wrt_OZ (ftnunit, (unsigned char *) ptr, p->p1, len, 8));
   case OM:
      return (wrt_OZM (ftnunit, (unsigned char *) ptr, p->p1, p->p2, len, 8));
   case Z:
      return (wrt_OZ (ftnunit, (unsigned char *) ptr, p->p1, len, 16));
   case ZM:
      return (wrt_OZM (ftnunit, (unsigned char *) ptr, p->p1, p->p2, len, 16));
   case L:
      return (wrt_L (ftnunit, (uinteger *) ptr, p->p1, len));
   case Q:
      return (0);
   case A:
      return (wrt_A (ftnunit, ptr, len));
   case AW:
      return (wrt_AW (ftnunit, ptr, p->p1, len));
   case D:
      if (len > 8)
         return (wrt_EQ (ftnunit, (ufloat *) ptr, p->p1, p->p2, p->p3, len, 'E', 1));
      else
         return (wrt_E (ftnunit, (ufloat *) ptr, p->p1, p->p2, p->p3, len, 'D', 1));
   case E:
   case EE:
      if (len > 8)
         return (wrt_EQ (ftnunit, (ufloat *) ptr, p->p1, p->p2, p->p3, len, 'E', 1));
      else
         return (wrt_E (ftnunit, (ufloat *) ptr, p->p1, p->p2, p->p3, len, 'E', 1));
   case G:
   case GE:
      return (wrt_G (ftnunit, (void *) ptr, p->op, p->p1, p->p2, p->p3, len, type, 1));
   case F:
      if (len > 8)
	 return(wrt_FQ (ftnunit, (ufloat *) ptr, p->p1, p->p2, 1));
      else
         return (wrt_F (ftnunit, (ufloat *) ptr, p->p1, p->p2, len, 1));
#ifdef I90
   case B:
      return (wrt_BM (ftnunit, (unsigned char *) ptr, p->p1, 1, len));
   case BM:
      return (wrt_BM (ftnunit, (unsigned char *) ptr, p->p1, p->p2, len));
   case EN:
   case ENE:
      if (exceed_length(ftnunit, p->p1)) return(110);
      if ( p->p1 == 0 || ( p->op == ENE && p->p3 == 0 ) ) {
	 err(CILISTERR, 100, "fmt");
      }
      if (len > 8)
         return (wrt_ENQ (ftnunit, (ufloat *) ptr, p->p1, p->p2, p->p3, len, 'E', 1));
      else
         return (wrt_EN (ftnunit, (ufloat *) ptr, p->p1, p->p2, p->p3, len, 'E', 1));
   case ES:
   case ESE:
      if (exceed_length(ftnunit, p->p1)) return(110);
      if ( p->p1 == 0 || ( p->op == ESE && p->p3 == 0 ) ) {
	 err(CILISTERR, 100, "fmt");
      }
      if (len > 8)
         return (wrt_ESQ (ftnunit, (ufloat *) ptr, p->p1, p->p2, p->p3, len, 'E', 1));
      else
         return (wrt_ES (ftnunit, (ufloat *) ptr, p->p1, p->p2, p->p3, len, 'E', 1));
#endif
   }
}

#ifdef I90
static int
wr_slash( unit *ftnunit, long repeat_count )
{
   int rslt;
   while ( repeat_count-- ) {
      rslt = (*ftnunit->f77donewrec)(ftnunit);
      if (rslt) return (rslt);
   }
   return (0);
}
#endif

int w_ned (unit *ftnunit, struct f77syl *p)
{
   switch (p->op) {
   default:
/*
      fprintf (stderr, "w_ned, unexpected code: %d\n%s\n",
	       p->op, ftnunit->f77fmtbuf);
*/
      err(CILISTERR, 167, "fmt");
   case SLASH:
#ifdef I90
      return (wr_slash (ftnunit, p->p1));
#else
      return ((*ftnunit->f77donewrec) (ftnunit));
#endif
   case T:
      ftnunit->f77cursor = p->p1 - ftnunit->f77recpos - 1;
      return (0);
   case TL:
      ftnunit->f77cursor -= p->p1;
      if (ftnunit->f77cursor < (-ftnunit->f77recpos))
	 ftnunit->f77cursor = -ftnunit->f77recpos;
      return (0);
   case TR:
   case X:
      ftnunit->f77cursor += p->p1;
      return (0);
   case APOS:
      return (wrt_AP (ftnunit, (char *)p->p1));
   case H:
      return (wrt_H (ftnunit, p->p1, (char *)p->p2));
   }
}


static int exceed_length(unit *ftnunit, int w)
{
    if (ftnunit->f77putn==x_putc || ftnunit->f77putn==t_putc)
	return(0);
    else if (ftnunit->f77putn==y_putc) {
	/* direct formatted I/O */
	if (ftnunit->f77recpos+w > ftnunit->url && ftnunit->url > 1)
	    return(1);
    } else if ((icptr + w) > icend)
	/* internal formatted I/O */
	return(1);
    return(0);
}
