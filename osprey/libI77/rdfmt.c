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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/rdfmt.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */


#include <cmplrs/fio.h>
#include <mutex.h>
#include "fmt.h"
#include "vmsflags.h"
#include "rdfmt.h"
#include "iio.h"
#include "iomode.h"
#include "err.h"
#include "bcompat.h"
#include "../include/cmplrs/f_errno.h"
#include "typecheck.h"

extern double atof(const char *str);
extern int fmt_check;

#define MAX_INPUT_SIZE	84

int __s_rsfi_com (icilist64 *a, unit **fu, int f90sw)
{
   int             n;
   unit *ftnunit;
   
   if (!f77init)
      f_init ();
   ftnunit = *fu = Internal_File;
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;

#ifdef I90
   ftnunit->f90sw = f90sw;
#endif
   if (n = c_si (a, ftnunit))
      return (n);
   ftnunit->uwrt &= ~WR_OP;
   ftnunit->f77doed = rd_ed;
   ftnunit->f77doned = rd_ned;
   ftnunit->f77getn = z_getc;
   ftnunit->f77gets = z_gets;
   ftnunit->f77ungetn = z_ungetc;
   ftnunit->f77donewrec = z_rSL;
   ftnunit->f77dorevert = ftnunit->f77doend = z_rnew;
   ftnunit->f77recend = ftnunit->f77errlist.icirlen;
   return (0);
}

int s_rsfi (icilist *a)
{
  icilist64 dst;
  get_icilist64(&dst, a);
  return __s_rsfi_com(&dst, &f77curunit, 0);
}

int s_rsfi_mp (icilist *a, unit **fu)
{
  icilist64 dst;
  get_icilist64(&dst, a);
  return __s_rsfi_com(&dst, fu, 0);
}


/* 64 bit versions */
int s_rsfi64 (icilist64 *a)
{
    return( __s_rsfi_com( a, &f77curunit, 0 ) );
}

int s_rsfi64_mp (icilist64 *a, unit **fu)
{
    return( __s_rsfi_com( a, fu, 0 ) );
}


int
rd_ed (unit * ftnunit, struct f77syl *p, char *ptr, ftnlen len, ftnint type)
{
   int             ch=0;
  XINT    needed_size=0;

   for (; ftnunit->f77cursor > 0; ftnunit->f77cursor--)
      if ((ch = (*ftnunit->f77getn) (ftnunit)) < 0)
	 return (ch);
   if (ftnunit->f77cursor < 0) {
      if (ftnunit->f77cursor < -ftnunit->f77recpos)
	 ftnunit->f77cursor = -ftnunit->f77recpos;
      for (; ftnunit->f77cursor < 0; ftnunit->f77cursor++)
	 if ((*ftnunit->f77ungetn) (ftnunit, ch) < 0) {
	    err(ftnunit->f77errlist.cierr, 106, "fmt");
	 }
   }
#ifdef I90
   if (ftnunit->f90sw == 1 ) {
        if ( p->op == A ) {
	    needed_size = len;
        } else if ( p->op == AW ) {
	    needed_size = ( p->p1 < len ? p->p1 : len );
        } else {
	    needed_size = p->p1;
        }
	if ( ftnunit->f77recpos + needed_size > ftnunit->f77recend ) { 
	    if ( ftnunit->unpad != 0) { /* padding is not done */
		if (ftnunit->f90nadv == 1) {
		    ftnunit->f90eor = 1;
		    if (ftnunit->f77errlist.cisize) {
			    *ftnunit->f77errlist.cisize += ftnunit->f77recend - ftnunit->f77recpos;
		    }
		    return(0);
		} else {
		    err(ftnunit->f77errlist.cierr,177,"fmt");
		}
	    } else { /* padding is done */
		if (ftnunit->f90nadv == 1) {
		    ftnunit->f90eor = 1;
		    if (ftnunit->f77errlist.cisize) {
			    *ftnunit->f77errlist.cisize += ftnunit->f77recend - ftnunit->f77recpos;
		    }
		}
	    }
	} else {
	    if (ftnunit->f77errlist.cisize) {
		*ftnunit->f77errlist.cisize += needed_size;
	    }
	}
	if ( test_type(p->op,type) != 0 )
	    err(ftnunit->f77errlist.cierr,117,"rdfmt");
   }
#endif
   if (fmt_check && _RCHK[p->op][type]) {
      err(CILISTERR, F_TYPECONFLICT, "formatted read");
   }
   switch (p->op) {
   default:
/*
      fprintf (stderr, "rd_ed, unexpected code: %d\n%s\n",
	       p->op, ftnunit->f77fmtbuf);
*/
      return (100);
   case I:
   case IM:
      ch = (rd_I (ftnunit, (uinteger *) ptr, p->p1, len));
      break;
#ifdef I90
   case B:
   case BM:
      ch = (rd_B (ftnunit, (unsigned char *) ptr, p->p1, len));
      break;
#endif
   case O:
   case OM:
      ch = (rd_OZ (ftnunit, (unsigned char *) ptr, p->p1, len, 8));
      break;
   case Z:
   case ZM:
      ch = (rd_OZ (ftnunit, (unsigned char *) ptr, p->p1, len, 16));
      break;
   case L:
      ch = (rd_L (ftnunit, (uinteger *) ptr, p->p1, len));
      break;
   case A:
      ch = (rd_A (ftnunit, ptr, len));
      break;
   case AW:
      ch = (rd_AW (ftnunit, ptr, p->p1, len));
      break;
   case E:
   case EE:
   case D:
   case F:
#ifdef I90
   case ES:
   case EN:
   case ESE:
   case ENE:
#endif
      ch = (rd_F (ftnunit, (ufloat *) ptr, p->p1, p->p2, len));
      break;
   case G:
   case GE:
      switch(type) {
      default:
      case TYCHAR:
	if (p->p1 != 0 ) { 
	    ch = (rd_AW (ftnunit, ptr, p->p1, len));
	} else {
	    ch = (rd_A (ftnunit, ptr, len));
	}
	break;
      case TYLOGICAL1:
      case TYLOGICAL2:
      case TYLOGICAL4:
      case TYLOGICAL8:
	ch = (rd_L (ftnunit, (uinteger *) ptr, p->p1, len));
	break;
      case TYBYTE:
      case TYSHORT:
      case TYINT:
      case TYLONGLONG:
	ch = (rd_I (ftnunit, (uinteger *) ptr, p->p1, len));
	break;
      case TYREAL:
      case TYDREAL:
      case TYQUAD:
      case TYCOMPLEX:
      case TYDCOMPLEX:
      case TYQUADCOMPLEX:
	ch = (rd_F (ftnunit, (ufloat *) ptr, p->p1, p->p2, len));
	break;
      } /* switch */
      break;
   case Q:
      ch = (rd_Q (ftnunit, (uinteger *) ptr, len));
      break;
   }
   if (ch == 0 || ftnunit->f77errlist.iciunit || (f77vms_flag_[VMS_EF] && ch < 0))
      return (errno = ch);
   else if (feof (ftnunit->ufd))
      return (EOF);
   clearerr(ftnunit->ufd);
   return (errno);
}

#ifdef I90
static int
rd_slash( unit *ftnunit, long repeat_count )
{
   int rslt;
   while ( repeat_count-- ) {
      rslt = (*ftnunit->f77donewrec)(ftnunit);
      if (rslt) return (rslt);
   }
   return (0);
}
#endif /* -I90 */

int
rd_ned (unit *ftnunit, struct f77syl *p)
{
   switch (p->op) {
   default:
/*
      fprintf (stderr, "rd_ned, unexpected code: %d\n%s\n",
	       p->op, ftnunit->f77fmtbuf);
*/
      return (100);
   case APOS:
      return (rd_POS (ftnunit, (char *) p->p1));	/* DAG -- bug fix (added
					 * cast) */
   case H:
      return (rd_H (ftnunit, p->p1));	/* DAG -- bug fix (added
						 * cast) */
   case SLASH:
#ifdef I90
      return (rd_slash (ftnunit, p->p1));
#else
      return ((*ftnunit->f77donewrec) (ftnunit));
#endif
   case TR:
   case X:
      ftnunit->f77cursor += p->p1;
      return (0);
   case T:
      ftnunit->f77cursor = p->p1 - ftnunit->f77recpos - 1;
      return (0);
   case TL:
      ftnunit->f77cursor -= p->p1;
      if (ftnunit->f77cursor < (-ftnunit->f77recpos))
	 ftnunit->f77cursor = -ftnunit->f77recpos;
      return (0);
   }
}

int
rd_I (unit *ftnunit, uinteger *n, long w, ftnlen len)
{
   register ftnll  x = 0;
   int             sign, ch;
   char            s[MAX_INPUT_SIZE];
   register char  *ps, c;
   int		   i, ich;
   char		   cc;

   if (w == 0)
      w = len < 4 ? 7 : len < 8 ? 12 : 21;
   if ((int) w >= MAX_INPUT_SIZE) {
      ch = GETS (s, MAX_INPUT_SIZE-1, ','); 
      for (i = MAX_INPUT_SIZE-1; i < w; i++) {
	  ich = GETS (&cc, 1, ',');
	  if (!ch) break;
	  if (!isspace(cc))
	     return (errno = 186);
      }
   } else
      ch = GETS (s, (int) w, ',');
   if (ch < 0)
      return (ch);
   ps = s;
   ps[ch] = '\0';
   while (*ps == ' ')
      ps++;
   if (*ps == '-') {
      sign = 1;
      ps++;
   } else {
      sign = 0;
      if (*ps == '+')
	 ps++;
   }
   for (c = (*ps);; c = (*++ps)) {
      if (c >= '0' && c <= '9')
	 x = x * 10 + c - '0';
      else if (c == ' ') {
/* change x *= 10 to this because there is some problem with cfe.
 * ---ravi---
 */
	 if (ftnunit->f77cblank)
	    x = x * 10;
      } else {
	 if (ftnunit->f77cblank && c == '\0' && ch < w)
	    while (ch++ < w) x = x *10;
	 break;
      }
   }

   if (sign)
      x = -x;
   if (len == sizeof (short))
      n->is = (short) x;
   else if (len == sizeof (char))
      n->ic = (signed char) x;
   else if (len == sizeof (ftnll))
      n->ill = x;
   else
      n->ii = (int) x;
   if (*ps)
      return (errno = 115);
   return (0);
}

int
rd_OZ (unit *ftnunit, unsigned char *n, long w, ftnlen len, int base)
{
   unsigned char   s[84];
   register unsigned char *ps, *vbuf;
   register int c=0;
   register unsigned int d, bits, shift;

   ps = s;
   shift = base == 8 ? 3 : 4;
   if (w == 0)
      w = (int) (len < 4 ? 7 : (len < 8 ? 12 :
		    (len > 8 ? (len * 8 + shift - 1) / shift : 23)));
   while (w--) {
      GET (c);
      if (c == ',' || c == '\n')
	 break;
      if (c == ' ' && ps == s)
	 continue;
      *ps = (char) c;
      ps++;
   }

   d = bits = 0;

#ifdef _MIPSEB
   vbuf = n + len - 1;
#else
   vbuf = n;
#endif


   while (--ps >= s) {

      c = *ps;
      if (c >= '0' && c <= '9')
	 c -= '0';
      else if (c >= 'a' && c <= 'f')
	 c -= 'a' - 10;
      else if (c >= 'A' && c <= 'F')
	 c -= 'A' - 10;
      else if (c == ' ') {
	 if (ftnunit->f77cblank)
	    c = 0;
	 else
	    continue;
      }
/* fix bug 5221 */
      else if ((ps == s) && (c == '+' || c == '-'))
	 continue;
      else
	 return (errno = 115);
      if (c >= base)
	 return (errno = 115);

      d |= (c << bits);
      bits += shift;

      if (bits >= 8) {
#ifdef _MIPSEB
	 if (vbuf < n && d != 0)
	    return (errno = 115);
	 if (vbuf >= n)
	    (*vbuf--) = (char) (d & 0xff);
#else
	 if (vbuf >= (n + len) && d != 0)
	    return (errno = 115);
	 if (vbuf < (n + len))
	    (*vbuf++) = (char) (d & 0xff);
#endif
	 d = d >> 8;
	 bits -= 8;
      }
   }

#ifdef _MIPSEB
   if (d) {
      if (vbuf < n)
	 return (errno = 115);
      (*vbuf--) = (char) (d & 0xff);
   }
   while (vbuf >= n)
      (*vbuf--) = '\0';
/* fix bug 5221 */
   if (c == '-') {
      for (vbuf = n; vbuf < (n + len); vbuf++)
	 *vbuf = (unsigned char) (~(*vbuf));
      (*(vbuf - 1))++;
   }
#else
   if (d) {
      if (vbuf >= (n + len))
	 return (errno = 115);
      (*vbuf++) = d & 0xff;
   }
   while (vbuf < (n + len))
      (*vbuf++) = '\0';
/* fix bug 5221 */
   if (c == '-') {
      for (vbuf = (n + len - 1); vbuf >= (n); vbuf--)
	 *vbuf = ~(*vbuf);
      *(vbuf + 1) = *(vbuf + 1) + 0x80;
   }
#endif

   return (0);

}

#ifdef I90
int rd_B (unit *ftnunit, unsigned char *n, long w, ftnlen len)
{
   unsigned char   s[84];
   register unsigned char *ps, *vbuf;
   register int c=0;
   register unsigned int d, bits, shift = 1;

   ps = s;
/*
   if (w == 0) w = len * 8;
*/
   while (w--) {
      GET (c);
      if (c == ',' || c == '\n')
	 break;
      if (c == ' ' && ps == s)
	 continue;
      *ps = (char) c;
      ps++;
   }

   d = bits = 0;

#ifdef _MIPSEB
   vbuf = n + len - 1;
#else
   vbuf = n;
#endif

   while (--ps >= s) {

      c = *ps;
      switch(c) {
      case ' ':
	if ( !ftnunit->f77cblank ) continue;
      case '0':
	break;
      case '1':
	d |= 1 << bits;
	break;
      case '+':
      case '-':
	if ( ps == s ) continue;
      default:
	 return (errno = 115);
      }
      bits += shift;

      if (bits >= 8) {
#ifdef _MIPSEB
	 if (vbuf < n && d != 0)
	    return (errno = 115);
	 if (vbuf >= n)
	    (*vbuf--) = (char) (d & 0xff);
#else
	 if (vbuf >= (n + len) && d != 0)
	    return (errno = 115);
	 if (vbuf < (n + len))
	    (*vbuf++) = (char) (d & 0xff);
#endif
	 d = d >> 8;
	 bits -= 8;
      }
   }

#ifdef _MIPSEB
   if (d) {
      if (vbuf < n)
	 return (errno = 115);
      (*vbuf--) = (char) (d & 0xff);
   }
   while (vbuf >= n)
      (*vbuf--) = '\0';
/* fix bug 5221 */
   if (c == '-') {
      for (vbuf = n; vbuf < (n + len); vbuf++)
	 *vbuf = (unsigned char) (~(*vbuf));
      (*(vbuf - 1))++;
   }
#else
   if (d) {
      if (vbuf >= (n + len))
	 return (errno = 115);
      (*vbuf++) = d & 0xff;
   }
   while (vbuf < (n + len))
      (*vbuf++) = '\0';
/* fix bug 5221 */
   if (c == '-') {
      for (vbuf = (n + len - 1); vbuf >= (n); vbuf--)
	 *vbuf = ~(*vbuf);
      *(vbuf + 1) = *(vbuf + 1) + 0x80;
   }
#endif

   return (0);

}
#endif

int
rd_Q (unit *ftnunit, uinteger *n, ftnlen len)
{
   int            x;

   x = ftnunit->f77recpos < ftnunit->f77recend ? ftnunit->f77recend - ftnunit->f77recpos : 0;
   if (len == sizeof (short))
      n->is = (short) x;
   else if (len == sizeof (char))
      n->ic = (signed char) x;
   else if (len == sizeof (ftnll))
      n->ill = x;
   else
      n->ii = x;
   return (0);
}

int
rd_L (unit *ftnunit, uinteger *n, long w, ftnlen len)
{
   int             ch;
   char            s[84], *ps;

   ps = s;
   w = w ? w : 2;
   while (w) {
      GET (ch);
      if (ch == ',' || ch == '\n')
	 break;
      *ps = (char) ch;
      ps++;
      w--;
   }
   *ps = '\0';
   ps = s;
   while (*ps == ' ')
      ps++;
   if (*ps == '.')
      ps++;
   if (*ps == 't' || *ps == 'T')
      ch = 1;
   else if (*ps == 'f' || *ps == 'F')
      ch = 0;
   else
      return (errno = 116);
   if (len == sizeof (short))
      n->is = (short) ch;
   else if (len == sizeof (char))
      n->ic = (signed char) ch;
   else if (len == sizeof (ftnll))
      n->ill = ch;
   else
      n->ii = ch;
   return (0);
}


int
rd_F (unit *ftnunit, ufloat *p, long w, long d, ftnlen len)
{
   char            s[MAX_INPUT_SIZE], *sp, *c, *exppos=NULL;
   int             ch, nfrac, exp, dot, se;
   int		   extrachars=0;
   int		   i, ich;
   char		   cc;

   dot = 1;			/* no dot */
   if (w == 0) {
      if (len < 8) {
	 w = 15;
	 d = 7;
      } else if (len == 8) {
	 w = 25;
	 d = 16;
      }
      else {
	 w = 40;
	 d = 32;
      }
   }
   if ((int) w >= MAX_INPUT_SIZE) {
      ch = GETS (sp = s, MAX_INPUT_SIZE-1, ','); 
      for (i = MAX_INPUT_SIZE-1; i < w; i++) {
	  ich = GETS (&cc, 1, ',');
	  if (!ch) break;
	  if (!isspace(cc))
	     return (errno = 186);
      }
   } else
      ch = GETS (sp = s, (int) w, ',');
   if (ch < 0)
      return (ch);
   sp[ch] = '\0';
   while (*sp == ' ')
      sp++;
   if (*sp == '-') {
      sp++;
   } else {
      if (*sp == '+')
	 sp++;
   }
loop1:
   while (*sp >= '0' && *sp <= '9') {
      sp++;
   }
   if (*sp == ' ') {
      if (ftnunit->f77cblank) { *sp++='0'; }
      else {c=sp; while (*c) { *c=*(c+1); c++; } }
      goto loop1;
   }
   nfrac = 0;
   if (*sp == '.') {
      {c=sp; while (*c) { *c=*(c+1); c++; } }
      dot = 0;
loop2:
      while (*sp >= '0' && *sp <= '9') {
	 nfrac--;
	 sp++;
      }
      if (*sp == ' ') {
      if (ftnunit->f77cblank) { *sp++='0'; nfrac--; }
      else {c=sp; while (*c) { *c=*(c+1); c++; } }
	 goto loop2;
      }
   }
   if (*sp == 'd' || *sp == 'e' || *sp == 'D' || *sp == 'E'
       || *sp == 'q' || *sp == 'Q') {
      {
      exppos=sp;
      *sp++='e';
      }
   } else if (*sp != '+' && *sp != '-')
      {
      nfrac -= ftnunit->f77scale;
      exppos=sp;
      }
   while (*sp == ' ') {c=sp; while (*c) { *c=*(c+1); c++; } }
   if (*sp == '-') {
      if (exppos==NULL) exppos=sp;
      sp++;
      se = 1;
   } else {
      if (exppos==NULL) exppos=sp;
      se = 0;
      if (*sp == '+')
	 sp++;
   }
   exp = 0;
loop3:
   while (*sp >= '0' && *sp <= '9') {
      exp = exp * 10 + (*sp - '0');
      sp++;
   }
   if (*sp == ' ') {
      if (ftnunit->f77cblank)
	 exp *= 10;
      sp++;
      goto loop3;
   }
   /* here we figure out if there is any unexpected characters */
   if (*sp) extrachars=1;
   if (se)
      exp = nfrac - exp;
   else
      exp += nfrac;
   if (dot)
      exp -= d;
   *exppos++='e';
   /* re-write exponent */
   if (exp < 0)
	{
	*exppos++='-';
	exp = - exp;
	}
   if (exp > 999) exp=999;	/* currently cant handle exponents that large */
   if (exp > 99)
	{
	*exppos++ = (char) ((exp/100) + '0');
	exp = exp % 100;
	if (exp < 10)
	    *exppos++ = '0';
	}
   if (exp > 9)
	{
	*exppos++ = (char) ((exp/10) + '0');
	exp = exp % 10;
	}
   *exppos++ = (char) (exp + '0');
   *exppos = '\0';
   /* now convert */
   if (len < sizeof (double))
      p->pf = atof(s);
   else if (len == sizeof (double))
      p->pd =  atof(s);
   else
      p->pld =  atold(s);
   if (extrachars)
      return (errno = 115);
   else
      return (0);
}

int
rd_A (unit *ftnunit, char *p, ftnlen len)
{
   register int    i;

   i = GETS (p, (int)len, '\n');
   if (i < 0)
      return (i);
   while (i < len)
      p[i++] = ' ';
   return (0);
}

int
rd_AW (unit *ftnunit, char *p, long w, ftnlen len)
{
   register int    i, ch;

   while (w > len) {
      GET (ch);
      w--;
   }
   i = GETS (p, (int)w, '\n');
   if (i < 0)
      return i;
   while (i < len)
      p[i++] = ' ';
   return (0);
}

int
rd_H (unit *ftnunit, long n)
{
   int             i, ch;

   for (i = 0; i < n; i++)
      if ((ch = (*ftnunit->f77getn) (ftnunit)) < 0)
	 return (ch);
   /* AGC 6/30/87 Do not save the characters in the format string
    * because it is in read only data area. Fortran standard does not
    * allow Quoted format modifiers on input anyway.  
    *
    * else *s = ch=='\n'?' ':ch; */
   return (0);
}

int
rd_POS (unit *ftnunit, char *s)
{
   char            quote;
   int             ch;

   quote = *s++;
   for (; *s; s++)
      if (*s == quote && *(s + 1) != quote)
	 break;
      else if ((ch = (*ftnunit->f77getn) (ftnunit)) < 0)
	 return (ch);
   /* AGC 6/30/87 Do not save the characters in the format string
    * because it is in read only data area. Fortran standard does not
    * allow Quoted format modifiers on input anyway.  
    *
    * else *s = ch=='\n'?' ':ch; */
   return (0);
}
