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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/fmt.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/

#include <cmplrs/fio.h>
#include <limits.h>
#include "fmt.h"
#include "err.h"
#include "varfmt.h"
#include "iomode.h"
#include "bcompat.h"
#include "uio.h"

#define skip(s) while(*s==' ') s++
#ifdef interdata
#define SYLMX 300
#endif
#ifdef pdp11
#define SYLMX 300
#endif
#ifdef vax
#define SYLMX 300
#endif
#ifndef SYLMX
#define SYLMX 300
#endif
#define MYQUOTE '\2'
#define MYHOLL  '\3'
#define MYESC   '\7'
 /* special quote character for stu */
extern vfmt_struct f77vfmt_com_;

/* ADD0220 */
/* static int      parenlvl, pc, revloc; */

int      _type_f(int);
static char    *f_s(unit *, char *, int);
static char    *f_list(unit *, char *);
static char    *i_tem(unit *, char *);
static char    *gt_num(unit *, char *, int *);
static char    *gt_wnum(unit *, char *, int *);

int
pars_f (unit *ftnunit, char *s)
{
   ftnunit->parenlvl = ftnunit->revloc = ftnunit->pc = 0;
   if ((s = f_s (ftnunit, s, 0)) == NULL) {
      return (-1);
   }
   return (0);
}

static char *
f_s(unit *ftnunit, char *s, int curloc)
{
   skip (s);
   if (*s++ != '(') {
      return (NULL);
   }
   if (ftnunit->parenlvl++ == 1)
      ftnunit->revloc = curloc;
   if (op_gen (ftnunit, RET, curloc, 0, 0) < 0 ||
       (s = f_list (ftnunit, s)) == NULL) {
      return (NULL);
   }
   skip (s);
   return (s);
}

static char *
f_list(unit *ftnunit, char *s)
{
   for (; *s != 0;) {
      skip (s);
      if ((s = i_tem (ftnunit, s)) == NULL)
	 return (NULL);
      skip (s);
      if (*s == ',')
	 s++;
      else if (*s == ')') {
	 if (--ftnunit->parenlvl == 0) {
	    (void) op_gen (ftnunit, REVERT, ftnunit->revloc, 0, 0);
	    return (++s);
	 }
	 (void) op_gen (ftnunit, GOTO, 0, 0, 0);
	 return (++s);
      }
   }
   return (NULL);
}

static char *
i_tem(unit *ftnunit, char *s)
{
   char           *t;
   int             n, curloc;

   if (*s == ')')
      return (s);
   if (ne_d (ftnunit, s, &t))
      return (t);
   if (e_d (ftnunit, s, &t))
      return (t);
   s = gt_num (ftnunit, s, &n);
   if ((curloc = op_gen (ftnunit, STACK, n, 0, 0)) < 0)
      return (NULL);
   return (f_s (ftnunit, s, curloc));
}

int
ne_d (unit *ftnunit, char *s, char **p)
{
   int             n, x, sign = 0;

   switch (*s) {
   default:
      return (0);
   case ':':
      (void) op_gen (ftnunit, COLON, 0, 0, 0);
      break;
   case '$':
      if (ftnunit->uwrt & WR_OP)
          (void) op_gen (ftnunit, NONL, 0, 0, 0);
      break;
   case 'B':
   case 'b':
      if (*++s == 'z' || *s == 'Z')
	 (void) op_gen (ftnunit, BZ, 0, 0, 0);
#ifdef I90
      else if (*s == 'n' || *s == 'N')
	 (void) op_gen (ftnunit, BN, 0, 0, 0);
      else {
	 s--;	/* get back to the initial 'B' */
	 return(0);
      }
#else
      else
	 (void) op_gen (ftnunit, BN, 0, 0, 0);
#endif
      break;
   case 'S':
   case 's':
      if (*(s + 1) == 's' || *(s + 1) == 'S') {
	 x = SS;
	 s++;
      } else if (*(s + 1) == 'p' || *(s + 1) == 'P') {
	 x = SP;
	 s++;
      } else
	 x = S;
      (void) op_gen (ftnunit, x, 0, 0, 0);
      break;
   case '/':
#ifdef I90
      (void) op_gen (ftnunit, SLASH, 1, 0, 0);
#else
      (void) op_gen (ftnunit, SLASH, 0, 0, 0);
#endif
      break;
   case '-':
      sign = 1;			/* OUTRAGEOUS CODING TRICK */
   case '+':
      s++;			/* OUTRAGEOUS CODING TRICK */
   case '0':
   case '1':
   case '2':
   case '3':
   case '4':
   case '5':
   case '6':
   case '7':
   case '8':
   case '9':
   case MYESC:
      s = gt_num (ftnunit, s, &n);
      switch (*s) {
      default:
	 return (0);
      case 'P':
      case 'p':
	 if (sign) n = -n;
	 (void) op_gen (ftnunit, P, n, 0, 0);
	 break;
      case 'X':
      case 'x':
	 if (sign) return (0);
	 (void) op_gen (ftnunit, X, n, 0, 0);
	 break;
      case 'H':
      case 'h':
	 if (sign) return (0);
	 (void) op_gen (ftnunit, H, n, (long) (s + 1), 0);
	 s += n;
	 break;
#ifdef I90
      case '/':
	 if (sign) return (0);
	 (void) op_gen (ftnunit, SLASH, n, 0, 0);
	 break;
#endif
      }
      break;
   case MYQUOTE:
   case MYHOLL:
   case '"':
   case '\'':
      (void) op_gen (ftnunit, APOS, (long) s, 0, 0);
      if ((*p = ap_end (ftnunit, s)) == NULL)
	 return (0);
      return (1);
   case 'T':
   case 't':
      if (*(s + 1) == 'l' || *(s + 1) == 'L') {
	 x = TL;
	 s++;
      } else if (*(s + 1) == 'r' || *(s + 1) == 'R') {
	 x = TR;
	 s++;
      } else
	 x = T;
      s = gt_num (ftnunit, s + 1, &n);
      s--;
      (void) op_gen (ftnunit, x, n, 0, 0);
      break;
   case 'X':
   case 'x':
      (void) op_gen (ftnunit, X, 1, 0, 0);
      break;
   case 'P':
   case 'p':
      (void) op_gen (ftnunit, P, 1, 0, 0);
      break;
   }
   s++;
   *p = s;
   return (1);
}

int
e_d (unit *ftnunit, char *s, char **p)
{
   int             n, w, d, e, found = 0, x = 0;
   char           *sv = s;

   s = gt_num (ftnunit, s, &n);
   (void) op_gen (ftnunit, STACK, n, 0, 0);
   switch (*s++) {
   default:
      break;
   case 'E':
   case 'e':
      x = 1;
#ifdef I90
      if ( ftnunit->f90sw == 1 ) {
	if ( *s == 'S' || *s == 's' ) {
	    x = 2;
	    s++;
	} else if ( *s == 'N' || *s == 'n' ) {
	    x = 3;
	    s++;
	}
      }
#endif
   case 'G':
   case 'g':
      found = 1;
      s = gt_wnum (ftnunit, s, &w);
      if (*s == '.') {
	 s++;
	 s = gt_num (ftnunit, s, &d);
      } else
	 d = 0;
#ifdef I90
      if (*s != 'E' && *s != 'e')
	 (void) op_gen (ftnunit, x == 1 ? E : x == 2 ? ES : x == 3 ? EN : G, w, d, 0);
      else {
	 s++;
	 s = gt_num (ftnunit, s, &e);
	 (void) op_gen (ftnunit, x == 1 ? EE : x == 2 ? ESE : x == 3 ? ENE : GE, w, d, e);
      }
#else 
      if (*s != 'E' && *s != 'e')
	 (void) op_gen (ftnunit, x == 1 ? E : G, w, d, 0);
      else {
	 s++;
	 s = gt_num (ftnunit, s, &e);
	 (void) op_gen (ftnunit, x == 1 ? EE : GE, w, d, e);
      }
#endif
      break;
   case 'O':
   case 'o':
      found = 1;
      s = gt_wnum (ftnunit, s, &w);
      if (*s != '.') {
	 (void) op_gen (ftnunit, O, w, 0, 0);
	 break;
      }
      s++;
      s = gt_num (ftnunit, s, &d);
      (void) op_gen (ftnunit, OM, w, d, 0);
      break;
   case 'Z':
   case 'z':
      found = 1;
      s = gt_wnum (ftnunit, s, &w);
      if (*s != '.') {
	 (void) op_gen (ftnunit, Z, w, 0, 0);
	 break;
      }
      s++;
      s = gt_num (ftnunit, s, &d);
      (void) op_gen (ftnunit, ZM, w, d, 0);
      break;
#ifdef I90
   case 'B':
   case 'b':
      if ( ftnunit->f90sw == 1 ) {
	if ( *s == 'N' || *s == 'n' || *s == 'L' || *s == 'l' ) break;
	found = 1;
	s = gt_wnum (ftnunit, s, &w);
	if (*s != '.') {
	    (void) op_gen (ftnunit, B, w, 0, 0);
	    break;
	}
	s++;
	s = gt_num (ftnunit, s, &d);
	(void) op_gen (ftnunit, BM, w, d, 0);
      }
      break;
#endif
   case 'Q':
   case 'q':
      found = 1;
      (void) op_gen (ftnunit, Q, 0, 0, 0);
      break;
   case 'L':
   case 'l':
      found = 1;
      s = gt_wnum (ftnunit, s, &w);
      (void) op_gen (ftnunit, L, w, 0, 0);
      break;
   case 'A':
   case 'a':
      found = 1;
      s = gt_wnum (ftnunit, s, &w);
      if (w)
	 (void) op_gen (ftnunit, AW, w, 0, 0);
      else
	 (void) op_gen (ftnunit, A, 0, 0, 0);
      break;
   case 'F':
   case 'f':
      found = 1;
      s = gt_wnum (ftnunit, s, &w);
      if (*s == '.') {
	 s++;
	 s = gt_num (ftnunit, s, &d);
      } else
	 d = 0;
      (void) op_gen (ftnunit, F, w, d, 0);
      break;
   case 'D':
   case 'd':
      found = 1;
      s = gt_wnum (ftnunit, s, &w);
      if (*s == '.') {
	 s++;
	 s = gt_num (ftnunit, s, &d);
      } else
	 d = 0;
      (void) op_gen (ftnunit, D, w, d, 0);
      break;
   case 'I':
   case 'i':
      found = 1;
      s = gt_wnum (ftnunit, s, &w);
      if (*s != '.') {
	 (void) op_gen (ftnunit, I, w, 0, 0);
	 break;
      }
      s++;
      s = gt_num (ftnunit, s, &d);
      (void) op_gen (ftnunit, IM, w, d, 0);
      break;
   }
   if (found == 0) {
      ftnunit->pc--;			/* unSTACK */
      *p = sv;
      return (0);
   }
   *p = s;
   return (1);
}

int
op_gen (unit *ftnunit, int a, long b, long c, int d)
{
   struct f77syl  *p;

   if (!ftnunit->f77syl) {
      ftnunit->f77syl_size = SYLMX;
      ftnunit->f77syl = (struct f77syl *) malloc (ftnunit->f77syl_size * sizeof (struct f77syl));
   } else if (ftnunit->f77syl_size <= ftnunit->pc) {
      ftnunit->f77syl_size += SYLMX;
      ftnunit->f77syl = (struct f77syl *) realloc (ftnunit->f77syl, ftnunit->f77syl_size * sizeof (struct f77syl));
   }
   p = &ftnunit->f77syl[ftnunit->pc];
   p->op = a;
   p->p1 = b;
   p->p2 = c;
   p->p3 = d;
   return (ftnunit->pc++);
}

static char    *
gt_wnum(unit *ftnunit, char *s, int *n)
{
   skip (s);
   if (*s != MYESC && (*s < '0' || *s > '9'))
      *n = 0;
   else
      s = gt_num (ftnunit, s, n);
   return (s);
}

static char *
gt_num(unit *ftnunit, char *s, int *n)
{
   int             m = 0, cnt = 0, escape = 0;
   char            c;

   for (c = *s;; c = *s) {
      if (c == ' ') {
	 s++;
	 continue;
      }
      if (c == MYESC) {
	 escape = 1;
	 s++;
	 continue;
      }
      if (c > '9' || c < '0')
	 break;
      m = 10 * m + c - '0';
      cnt++;
      s++;
   }
   if (escape)
      *n = call_vfmt(&m, ftnunit->vfmt, ftnunit->vfmtfp);
   else if (cnt == 0)
      *n = 1;
   else
      *n = m;
   return (s);
}


int
en_fio (unit **ftnunit)
{
   XINT          one = 1;
   ftnint          type = TYINT;

   return (do_fio_SIZE_mp (&type, &one, (char *) NULL, ftnunit, 0L));
}

int
do_fio_1dim( 	ftnint *type, char *ptr,
	     	flex *do_idx, ftnint *lb, 
		ftnint *ub, ftnint *step, 
		ftnlen len, ftnlen idxlen)	
{
    XINT llb = *lb, lub = *ub, lstep = *step;
    return( do_fio64_mp_1dim( type, ptr, do_idx, &llb, &lub, &lstep, &f77curunit, len, idxlen ) );
}

int
do_fio64_1dim(  ftnint *type, char *ptr,
                flex *do_idx, XINT *lb,
                XINT *ub, XINT *step,
                ftnlen len, ftnlen idxlen)
{
   return( do_fio64_mp_1dim( type, ptr, do_idx, lb, ub, step, &f77curunit, len, idxlen) );
}

int
do_fio64_mp_1dim(  ftnint *type, char *ptr,
                flex *do_idx, XINT *lb,
                XINT *ub, XINT *step,
		unit **fu,
                ftnlen len, ftnlen idxlen)
/* 
** This function carries out the I/O operation on a single-dimension
** implied-DO loop in the form (ARR(I),I=N,M,L).  It takes these 
** arguments:
**
**	type	: Fortran type of the I/O array
**	ptr	: address of the first array element involved in the I/O
**		  in the example above it is the address of ARR(N)
**	lastptr : address of the last array element involved in the I/O
**		  in the example above it is the address of ARR(M)
**	step	: the implied-do loop step size which is L in this case.
**	len	: length of each array element
*/
{
   XINT nelem;
   int ierr;
   char *lastptr, *nptr;
   int iolen;
   unit *ftnunit = *fu;

   lastptr = ptr + (*ub - 1) * len;
   ptr += (*lb - 1) * len;
   if (*type == TYCOMPLEX || *type == TYDCOMPLEX || *type == TYQUADCOMPLEX) {
      iolen = len / 2;
      nelem = 2;
   } else {
      iolen = len;
      nelem = 1;
   }
   if (*step == 1) {
   /* consecutive elements */
      if ((nelem = nelem *(*ub - *lb + 1)) > 0) {
	  if (ierr = do_fio_SIZE_mp( type, &nelem, ptr, fu, iolen ) ) {
	      set_do_idx( do_idx, idxlen, (ftnll) (*lb + (*fu)->lcount) / nelem );
	      (*fu)->lock_unit = 0;
	      err((*fu)->f77errlist.cierr,ierr,"formatted I/O");
	  }
	  set_do_idx( do_idx, idxlen, (ftnll) (*ub + 1) );
      }
      /* if last element <= first: do nothing */
      return( 0 );
   }
   
   if (*step > 0) {
       for (nptr = ptr; nptr <= lastptr; nptr += *step*len)
	   if (ierr = do_fio_SIZE_mp( type, &nelem, nptr, fu, iolen ) ) {
	      set_do_idx( do_idx, idxlen, (ftnll) (*lb + (nptr - ptr)/len) );
	      (*fu)->lock_unit = 0;
	      err((*fu)->f77errlist.cierr,ierr,"formatted I/O");
	   }
   }
   else
       for (nptr = ptr; nptr >= lastptr; nptr += *step*len)
	   if (ierr = do_fio_SIZE_mp( type, &nelem, nptr, fu, iolen ) ) {
	      set_do_idx( do_idx, idxlen, (ftnll) (*lb - (ptr - nptr)/len) );
	      (*fu)->lock_unit = 0;
	      err((*fu)->f77errlist.cierr,ierr,"formatted I/O");
	   }
   set_do_idx( do_idx, idxlen, (ftnll) (*lb + ((*ub - *lb) / *step + 1) * *step) );
   return(0);
}


int
do_fio (ftnint *type, ftnint *number, char *ptr, ftnlen len)
{
    return( do_fio_mp( type, number, ptr, &f77curunit, len ) );
}

#define TBUFLEN		80


int
do_fio64 (ftnint *type, XINT *number, char *ptr, ftnlen len)
{
    return( do_fio64_mp( type, number, ptr, &f77curunit, len ) );
}

int
do_fio_mp (ftnint *type, ftnint *number, char *ptr, unit **fu, ftnlen len)
{
    XINT lnum = *number;
    return( do_fio64_mp( type, &lnum, ptr, fu, len ) );
}

int
do_fio64_mp (ftnint *type, XINT *number, char *ptr, unit **fu, ftnlen len)
{
   struct f77syl  *p;

   /* fix bug 7625 */
   XINT   i, num = *number;
   int n;
   unit *ftnunit = *fu;

   for (i = 0; i < num; i++, ptr += len) {
loop:switch (_type_f ((p = &ftnunit->f77syl[ftnunit->pc])->op)) {
      default:
/*
	 fprintf (stderr, "unknown code in do_fio: %d\n%s\n",
		  p->op, ftnunit->f77fmtbuf);
*/
	 ftnunit->lcount = i;
	 errret(ftnunit->f77errlist.cierr, 100, "do_fio");
      case NED:
	 if ((n = (*ftnunit->f77doned) (ftnunit, p)) > 0) {
	    ftnunit->lcount = i;
	    errret(ftnunit->f77errlist.cierr, n, "fmt");
	 }
	 if (n < 0) {
	    ftnunit->lcount = i;
	    errret(ftnunit->f77errlist.ciend, (EOF), "fmt");
	 }
	 ftnunit->pc++;
	 goto loop;
      case ED:
	 /* fix bug 7625 */
	 if (ftnunit->cnt[ftnunit->cp] <= 0) {
	    ftnunit->cp--;
	    ftnunit->pc++;
	    goto loop;
	 }
	 if (ptr == NULL) {
	    if (n = (*ftnunit->f77doend) (ftnunit))
	       ftnunit->lock_unit = 0;
	    return( n );
	 }
	 ftnunit->cnt[ftnunit->cp]--;
	 ftnunit->f77workdone = 1;
	 if ((n = (*ftnunit->f77doed) (ftnunit, p, ptr, len, *type)) > 0) {
	    ftnunit->lcount = i;
	    errret(ftnunit->f77errlist.cierr, n, "fmt");
	 }
	 if (n < 0) {
	    ftnunit->lcount = i;
	    errret(ftnunit->f77errlist.ciend, (EOF), "fmt");
	 }
	 continue;
      case STACK:
	 ftnunit->cnt[++ftnunit->cp] = p->p1;
	 ftnunit->pc++;
	 goto loop;
      case RET:
	 ftnunit->ret[++ftnunit->rp] = p->p1;
	 ftnunit->pc++;
	 goto loop;
      case GOTO:
	 if (--ftnunit->cnt[ftnunit->cp] <= 0) {
	    ftnunit->cp--;
	    ftnunit->rp--;
	    ftnunit->pc++;
	    goto loop;
	 }
	 ftnunit->pc = 1 + ftnunit->ret[ftnunit->rp--];
	 goto loop;
      case REVERT:
	 ftnunit->rp = ftnunit->cp = 0;
	 ftnunit->pc = p->p1;
	 /* fix bug 7625 */
	 if (ptr == NULL) {
	    if (n = (*ftnunit->f77doend) (ftnunit))
	        ftnunit->lock_unit = 0;
	    return(n);
	 }
	 if (!ftnunit->f77workdone)
	    return (0);
	 if ((n = (*ftnunit->f77dorevert) (ftnunit)) != 0) {
	    ftnunit->lock_unit = 0;
	    return (n);
	 }
	 goto loop;
      case COLON:
	 if (ptr == NULL) {
	    if (n = (*ftnunit->f77doend) (ftnunit))
	       ftnunit->lock_unit = 0;
	    return(n);
	 }
	 ftnunit->pc++;
	 goto loop;
      case NONL:
	 ftnunit->f77nonl = 1;
	 ftnunit->pc++;
	 goto loop;
      case S:
      case SS:
	 ftnunit->f77cplus = 0;
	 ftnunit->pc++;
	 goto loop;
      case SP:
	 ftnunit->f77cplus = 1;
	 ftnunit->pc++;
	 goto loop;
      case P:
	 ftnunit->f77scale = (short) p->p1;
	 ftnunit->pc++;
	 goto loop;
      case BN:
	 ftnunit->f77cblank = 0;
	 ftnunit->pc++;
	 goto loop;
      case BZ:
	 ftnunit->f77cblank = 1;
	 ftnunit->pc++;
	 goto loop;
      }
   }
   return (0);
}


void
fmt_bg (unit *ftnunit)
{
   ftnunit->f77workdone = ftnunit->cp = ftnunit->rp = ftnunit->pc = 0;
#ifndef I90
   ftnunit->f77cursor = 0;
#endif
   ftnunit->cnt[0] = ftnunit->ret[0] = 0;
}

int
_type_f(int n)
{
   switch(n) {
      default:
      return(n);
   case RET:
      return (RET);
   case REVERT:
      return (REVERT);
   case GOTO:
      return (GOTO);
   case STACK:
      return (STACK);
   case X:
   case SLASH:
   case APOS:
   case H:
   case T:
   case TL:
   case TR:
      return (NED);
   case F:
   case I:
   case IM:
   case A:
   case AW:
   case O:
   case OM:
   case Z:
   case ZM:
   case L:
   case E:
   case EE:
   case D:
   case G:
   case GE:
   case Q:
#ifdef I90
   case ES:
   case ESE:
   case EN:
   case ENE:
   case B:
   case BM:
#endif
      return (ED);
   }
}


char *
ap_end(unit *ftnunit, char *s)
{
   char            quote;

   quote = *s++;
   for (; *s; s++) {
      if (*s != quote)
	 continue;
      if (*++s != quote)
	 return (s);
   }
   if (ftnunit->f77errlist.cierr) {
      errno = 100;
      return (NULL);
   }
   f77fatal (ftnunit, 100, "bad string");
   /* NOTREACHED */
   return(NULL); 
}

/* fix bug 5919 */
int
do_fioi4 (unsigned int val)
{
    return( do_fioi4_mp( val, f77curunit ) );
}


int
do_fioi4_mp (unsigned int val, unit *f77curunit)
{
   ftnint type = TYINT;
   return do_f4f8_mp (&type, &val, f77curunit, 4);
}

int
do_fioi8 (long long val)
{
    return( do_fioi8_mp( val, f77curunit ) );
}


int
do_fioi8_mp (long long val, unit *f77curunit)
{
   ftnint type = TYLONGLONG;
   return do_f4f8_mp (&type, &val, f77curunit, 8);
}

int
do_fior4 (float val)
{
   return( do_fior4_mp ( val, f77curunit ) );
}


int
do_fior4_mp (float val, unit *f77curunit)
{
   ftnint type = TYREAL;
   return do_f4f8_mp (&type, &val, f77curunit, 4);
}

int
do_fio8 (double val)
{
    return( do_fio8_mp( val, f77curunit ) );
}


int
do_fio8_mp (double val, unit *f77curunit)
{
   ftnint type = TYDREAL;
   return do_f4f8_mp (&type, &val, f77curunit, 8);
}

int
do_f4f8 (void *inptr, ftnlen len)
{
   ftnint type;
   type = (len == 4 ? TYREAL : TYDREAL );
   return( do_f4f8_mp(&type, inptr, f77curunit, len ) );
}



int
do_f4f8_mp (ftnint *type, void *inptr, unit *ftnunit, ftnlen len)
{
   char           *ptr = (char *) inptr;
   struct f77syl  *p;
   int             n;

loop:switch (_type_f ((p = &ftnunit->f77syl[ftnunit->pc])->op)) {
   default:
/*
      fprintf (stderr, "unknown code in do_fio: %d\n%s\n",
	       p->op, ftnunit->f77fmtbuf);
*/
      errret(ftnunit->f77errlist.cierr, 100, "do_fio");
   case NED:
      if ((n = (*ftnunit->f77doned) (ftnunit, p)) > 0)
	 errret(ftnunit->f77errlist.cierr, n, "fmt");
      if (n < 0)
	 errret(ftnunit->f77errlist.ciend, (EOF), "fmt");
      ftnunit->pc++;
      goto loop;
   case ED:
      if (ftnunit->cnt[ftnunit->cp] <= 0) {
	 ftnunit->cp--;
	 ftnunit->pc++;
	 goto loop;
      }
      if (ptr == NULL) {
	 if (n = (*ftnunit->f77doend) (ftnunit))
	    ftnunit->lock_unit = 0;
	 return(n);
      }
      ftnunit->cnt[ftnunit->cp]--;
      ftnunit->f77workdone = 1;
      if ((n = (*ftnunit->f77doed) (ftnunit, p, ptr, len, *type)) > 0)
	 errret(ftnunit->f77errlist.cierr, n, "fmt");
      if (n < 0)
	 errret(ftnunit->f77errlist.ciend, (EOF), "fmt");
      break;
   case STACK:
      ftnunit->cnt[++ftnunit->cp] = p->p1;
      ftnunit->pc++;
      goto loop;
   case RET:
      ftnunit->ret[++ftnunit->rp] = p->p1;
      ftnunit->pc++;
      goto loop;
   case GOTO:
      if (--ftnunit->cnt[ftnunit->cp] <= 0) {
	 ftnunit->cp--;
	 ftnunit->rp--;
	 ftnunit->pc++;
	 goto loop;
      }
      ftnunit->pc = 1 + ftnunit->ret[ftnunit->rp--];
      goto loop;
   case REVERT:
      ftnunit->rp = ftnunit->cp = 0;
      ftnunit->pc = p->p1;
      if (ptr == NULL) {
	 if (n = (*ftnunit->f77doend) (ftnunit))
	    ftnunit->lock_unit = 0;
	 return(n);
      }
      if (!ftnunit->f77workdone)
	 return (0);
      if ((n = (*ftnunit->f77dorevert) (ftnunit)) != 0) {
	 ftnunit->lock_unit = 0;
	 return (n);
      }
      goto loop;
   case COLON:
      if (ptr == NULL) {
	 if (n = (*ftnunit->f77doend) (ftnunit))
	    ftnunit->lock_unit = 0;
	 return(n);
      }
      ftnunit->pc++;
      goto loop;
   case NONL:
      ftnunit->f77nonl = 1;
      ftnunit->pc++;
      goto loop;
   case S:
   case SS:
      ftnunit->f77cplus = 0;
      ftnunit->pc++;
      goto loop;
   case SP:
      ftnunit->f77cplus = 1;
      ftnunit->pc++;
      goto loop;
   case P:
      ftnunit->f77scale = (short) p->p1;
      ftnunit->pc++;
      goto loop;
   case BN:
      ftnunit->f77cblank = 0;
      ftnunit->pc++;
      goto loop;
   case BZ:
      ftnunit->f77cblank = 1;
      ftnunit->pc++;
      goto loop;
   }
   return (0);
}

#ifdef I90
int test_type(int op, ftnint type)
{
    switch (op) {
    case F:
    case D:
    case E:
    case EE:
    case EN:
    case ENE:
    case ES:
    case ESE:
	switch (type) {
	case TYREAL:
	case TYDREAL:
	case TYCOMPLEX:
	case TYDCOMPLEX:
	case TYQUAD:
	case TYQUADCOMPLEX:
	    return(0);
	default:
	    return(117);
	}
    case I:
    case IM:
    case B:
    case BM:
    case O:
    case OM:
    case Z:
    case ZM:
	switch (type) {
	case TYBYTE:
	case TYSHORT:
	case TYINT:
	case TYLONGLONG:
	    return(0);
	default:
	    return(117);
	}
    case L:
	switch (type) {
	case TYLOGICAL1:
	case TYLOGICAL2:
	case TYLOGICAL4:
	case TYLOGICAL8:
	    return(0);
	default:
	    return(117);
	}
    case A:
    case AW:
	switch (type) {
	case TYCHAR:
	    return(0);
	default:
	    return(117);
	}
    default:
	break;
    }
    return(0);
}
#endif


/*  ========================================================================  */
/*									      */
/*  The following entries are a minor efficiency improvement in calling       */
/*  the formatted I/O routines.  In a few cases, they overlap existing        */
/*  entries but are an attempt to clean up the interface.  In truth, the      */
/*  entire I/O interface could use an overhaul.				      */
/*									      */
/*  ========================================================================  */

int
do_fioxa4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYADDR;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 4 ) );
}

int
do_fioxa4 (char *ptr, XINT num)
{
    ftnint type = TYADDR;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 4 ) );
}

int
do_fioxa8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYADDR;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 8 ) );
}

int
do_fioxa8 (char *ptr, XINT num)
{
    ftnint type = TYADDR;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 8 ) );
}

int
do_fioxh1_mp (char *ptr, XINT clen, XINT num, unit **fu)
{
    ftnint type = TYCHAR;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, clen ) );
}

int
do_fioxh1 (char *ptr, XINT clen, XINT num)
{
    ftnint type = TYCHAR;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, clen ) );
}

int
do_fioxi1_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYBYTE;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 1 ) );
}

int
do_fioxi1 (char *ptr, XINT num)
{
    ftnint type = TYBYTE;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 1 ) );
}

int
do_fioxi2_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYSHORT;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 2 ) );
}

int
do_fioxi2 (char *ptr, XINT num)
{
    ftnint type = TYSHORT;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 2 ) );
}

int
do_fioxi4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYINT;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 4 ) );
}

int
do_fioxi4 (char *ptr, XINT num)
{
    ftnint type = TYINT;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 4 ) );
}

int
do_fioxi8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLONGLONG;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 8 ) );
}

int
do_fioxi8 (char *ptr, XINT num)
{
    ftnint type = TYLONGLONG;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 8 ) );
}

int
do_fioxl1_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLOGICAL1;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 1 ) );
}

int
do_fioxl1 (char *ptr, XINT num)
{
    ftnint type = TYLOGICAL1;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 1 ) );
}

int
do_fioxl2_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLOGICAL2;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 2 ) );
}

int
do_fioxl2 (char *ptr, XINT num)
{
    ftnint type = TYLOGICAL2;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 2 ) );
}

int
do_fioxl4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLOGICAL4;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 4 ) );
}

int
do_fioxl4 (char *ptr, XINT num)
{
    ftnint type = TYLOGICAL4;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 4 ) );
}

int
do_fioxl8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLOGICAL8;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 8 ) );
}

int
do_fioxl8 (char *ptr, XINT num)
{
    ftnint type = TYLOGICAL8;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 8 ) );
}

int
do_fioxr4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYREAL;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 4 ) );
}

int
do_fioxr4 (char *ptr, XINT num)
{
    ftnint type = TYREAL;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 4 ) );
}

int
do_fioxr8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYDREAL;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 8 ) );
}

int
do_fioxr8 (char *ptr, XINT num)
{
    ftnint type = TYDREAL;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 8 ) );
}

int
do_fioxr16_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYQUAD;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, fu, 16 ) );
}

int
do_fioxr16 (char *ptr, XINT num)
{
    ftnint type = TYQUAD;
    XINT number = num;
    return( do_fio64_mp( &type, &number, ptr, &f77curunit, 16 ) );
}

int
do_fioxc4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYREAL;
    XINT number = 1;
    char *endptr = ptr + (8 * num);
    int status;
    for (; ptr<endptr; ptr+=4)
      if (status = do_fio64_mp( &type, &number, ptr, fu, 4 )) break;
    return( status );
}

int
do_fioxc4 (char *ptr, XINT num)
{
    ftnint type = TYREAL;
    XINT number = 1;
    char *endptr = ptr + (8 * num);
    int status;
    for (; ptr<endptr; ptr+=4)
      if (status = do_fio64_mp( &type, &number, ptr, &f77curunit, 4 )) break;
    return( status );
}

int
do_fioxc8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYDREAL;
    XINT number = 1;
    char *endptr = ptr + (16 * num);
    int status;
    for (; ptr<endptr; ptr+=8)
      if (status = do_fio64_mp( &type, &number, ptr, fu, 8 )) break;
    return( status );
}

int
do_fioxc8 (char *ptr, XINT num)
{
    ftnint type = TYDREAL;
    XINT number = 1;
    char *endptr = ptr + (16 * num);
    int status;
    for (; ptr<endptr; ptr+=8)
      if (status = do_fio64_mp( &type, &number, ptr, &f77curunit, 8 )) break;
    return( status );
}

int
do_fioxc16_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYQUAD;
    XINT number = 1;
    char *endptr = ptr + (32 * num);
    int status;
    for (; ptr<endptr; ptr+=16)
      if (status = do_fio64_mp( &type, &number, ptr, fu, 16 )) break;
    return( status );
}

int
do_fioxc16 (char *ptr, XINT num)
{
    ftnint type = TYQUAD;
    XINT number = 1;
    char *endptr = ptr + (32 * num);
    int status;
    for (; ptr<endptr; ptr+=16)
      if (status = do_fio64_mp( &type, &number, ptr, &f77curunit, 16 )) break;
    return( status );
}

int
do_fioxa4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    ftnint type = TYADDR;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 4 ) );
}
int
do_fioxa4v (ftnint val)
{
    ftnint value = val;
    ftnint type = TYADDR;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 4 ) );
}

int
do_fioxa8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    ftnint type = TYADDR;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 8 ) );
}

int
do_fioxa8v (ftnll val)
{
    ftnll value = val;
    ftnint type = TYADDR;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_fioxh1v_mp (char val, unit **fu)
{
    char value = val;
    ftnint type = TYCHAR;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 1 ) );
}

int
do_fioxh1v (char val)
{
    char value = val;
    ftnint type = TYCHAR;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 1 ) );
}

int
do_fioxi1v_mp (char val, unit **fu)
{
    char value = val;
    ftnint type = TYBYTE;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 1 ) );
}

int
do_fioxi1v (char val)
{
    char value = val;
    ftnint type = TYBYTE;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 1 ) );
}

int
do_fioxi2v_mp (short val, unit **fu)
{
    short value = val;
    ftnint type = TYSHORT;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 2 ) );
}

int
do_fioxi2v (short val)
{
    short value = val;
    ftnint type = TYSHORT;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 2 ) );
}

int
do_fioxi4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    ftnint type = TYINT;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 4 ) );
}

int
do_fioxi4v (ftnint val)
{
    ftnint value = val;
    ftnint type = TYINT;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 4 ) );
}

int
do_fioxi8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    ftnint type = TYLONGLONG;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 8 ) );
}

int
do_fioxi8v (ftnll val)
{
    ftnll value = val;
    ftnint type = TYLONGLONG;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_fioxl1v_mp (char val, unit **fu)
{
    char value = val;
    ftnint type = TYLOGICAL1;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 1 ) );
}

int
do_fioxl1v (char val)
{
    char value = val;
    ftnint type = TYLOGICAL1;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 1 ) );
}

int
do_fioxl2v_mp (short val, unit **fu)
{
    short value = val;
    ftnint type = TYLOGICAL2;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 2 ) );
}

int
do_fioxl2v (short val)
{
    short value = val;
    ftnint type = TYLOGICAL2;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 2 ) );
}

int
do_fioxl4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    ftnint type = TYLOGICAL4;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 4 ) );
}

int
do_fioxl4v (ftnint val)
{
    ftnint value = val;
    ftnint type = TYLOGICAL4;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 4 ) );
}

int
do_fioxl8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    ftnint type = TYLOGICAL8;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 8 ) );
}

int
do_fioxl8v (ftnll val)
{
    ftnll value = val;
    ftnint type = TYLOGICAL8;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_fioxr4v_mp (float val, unit **fu)
{
    float value = val;
    ftnint type = TYREAL;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 4 ) );
}

int
do_fioxr4v (float val)
{
    float value = val;
    ftnint type = TYREAL;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 4 ) );
}

int
do_fioxr8v_mp (double val, unit **fu)
{
    double value = val;
    ftnint type = TYDREAL;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 8 ) );
}

int
do_fioxr8v (double val)
{
    double value = val;
    ftnint type = TYDREAL;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_fioxr16v_mp (long double val, unit **fu)
{
    long double value = val;
    ftnint type = TYQUAD;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, fu, 16 ) );
}

int
do_fioxr16v (long double val)
{
    long double value = val;
    ftnint type = TYQUAD;
    XINT number = 1;
    return( do_fio64_mp( &type, &number, (char *)&value, &f77curunit, 16 ) );
}

int
do_fioxc4v_mp (float rval, float ival, unit **fu)
{
    float rvalue = rval;
    float ivalue = ival;
    ftnint type = TYREAL;
    XINT number = 1;
    int status;
    if (status = do_fio64_mp( &type, &number, (char *)&rvalue, fu, 4 ))
      return( status );
    return( do_fio64_mp( &type, &number, (char *)&ivalue, fu, 4 ) );
}

int
do_fioxc4v (float rval, float ival)
{
    float rvalue = rval;
    float ivalue = ival;
    ftnint type = TYREAL;
    XINT number = 1;
    int status;
    if (status = do_fio64_mp( &type, &number, (char *)&rvalue, &f77curunit, 4 ))
      return( status );
    return( do_fio64_mp( &type, &number, (char *)&ivalue, &f77curunit, 4 ) );
}

int
do_fioxc8v_mp (double rval, double ival, unit **fu)
{
    double rvalue = rval;
    double ivalue = ival;
    ftnint type = TYDREAL;
    XINT number = 1;
    int status;
    if (status = do_fio64_mp( &type, &number, (char *)&rvalue, fu, 8 ))
      return( status );
    return( do_fio64_mp( &type, &number, (char *)&ivalue, fu, 8 ) );
}

int
do_fioxc8v (double rval, double ival)
{
    double rvalue = rval;
    double ivalue = ival;
    ftnint type = TYDREAL;
    XINT number = 1;
    int status;
    if (status = do_fio64_mp( &type, &number, (char *)&rvalue, &f77curunit, 8 ))
      return( status );
    return( do_fio64_mp( &type, &number, (char *)&ivalue, &f77curunit, 8 ) );
}

int
do_fioxc16v_mp (long double rval, long double ival, unit **fu)
{
    long double rvalue = rval;
    long double ivalue = ival;
    ftnint type = TYQUAD;
    XINT number = 1;
    int status;
    if (status = do_fio64_mp( &type, &number, (char *)&rvalue, fu, 16 ))
      return( status );
    return( do_fio64_mp( &type, &number, (char *)&ivalue, fu, 16 ) );
}

int
do_fioxc16v (long double rval, long double ival)
{
    long double rvalue = rval;
    long double ivalue = ival;
    ftnint type = TYQUAD;
    XINT number = 1;
    int status;
    if (status = do_fio64_mp( &type, &number, (char *)&rvalue, &f77curunit, 16 ))
      return( status );
    return( do_fio64_mp( &type, &number, (char *)&ivalue, &f77curunit, 16 ) );
}

/*  ========================================================================  */
