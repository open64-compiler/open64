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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/iio.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

#include <cmplrs/fio.h>
#include <mutex.h>
#include <string.h>
#include "fmt.h"
#include "vmsflags.h"
#include "iio.h"
#include "iomode.h"
#include "err.h"
#include "wrtfmt.h"

extern vfmt_struct f77vfmt_com_;
extern char    *icptr, *icend;

int
z_ungetc (unit *ftnunit, int ch)
{
   if (ch != '\n') {
      if (icptr <= ftnunit->f77errlist.iciunit)
	 return (-1);
      icptr--;
      ftnunit->f77recpos--;
      icpos--;
   } else {
      if (icnum > 0)
	 icnum--;
      icpos = ftnunit->f77errlist.icirlen;
   }
   return (0);
}

int
z_getc (unit *ftnunit)
{
   if (icptr >= icend && icpos == 0)
      err (ftnunit->f77errlist.ciend, (EOF), "endfile");
   if (icpos++ < ftnunit->f77errlist.icirlen) {
      ftnunit->f77recpos++;
      return (*icptr++);
   } else {
      z_rnew (ftnunit);
      return ('\n');
   }
}

int
z_gets (unit *ftnunit, char *s, int w, char c)
{
   register int    n;

   /*
	Don't return EOF for internal read.  Remaining items will be zeros
   if (icptr >= icend && icpos == 0)
      err (ftnunit->f77errlist.iciend, (EOF), "endfile");
    */
   n = ftnunit->f77errlist.icirlen - ftnunit->f77recpos;
   w = n < w ? n : w;
   for (n = 0; n < w; n++) {
      /* '\n' character doesn not have any meaning in internal I/O */
      if (*icptr == c && c != '\n') {
	 icptr++;
	 ftnunit->f77recpos++;
	 break;
      }
      *(s++) = *(icptr++);
   }
   ftnunit->f77recpos += n;
   return (n);
}

int
z_putc (unit *ftnunit, XINT count, char con, char *buf)
{
   register XINT    i = count;

   if ((icptr + count) > icend)
      err (ftnunit->f77errlist.cierr, 110, "inwrite");
   if (buf) {
      memcpy (icptr, buf, count);
      icptr += count;
   } else {
      if (con == '\n') {
	 while (i--) z_wnew (ftnunit);
	 return(0);
      } else if (con) {
	 while (i--) *(icptr++) = con;
      } else if ((ftnunit->f77recpos + count) > ftnunit->f77recend) {
	 icptr += (ftnunit->f77recend - ftnunit->f77recpos);
	 i -= (ftnunit->f77recend - ftnunit->f77recpos);
	 while (i--) *(icptr++) = ' ';
      }
      else icptr += count;
   }

   if ((icpos += count) > ftnunit->f77errlist.icirlen)
      ftnunit->f77recend = ftnunit->f77cursor = ftnunit->f77recpos = icpos = icpos % ftnunit->f77errlist.icirlen;
   else if ((ftnunit->f77recpos += count) > ftnunit->f77recend)
      ftnunit->f77recend = ftnunit->f77recpos;
   return 0; /* the default */
}

int 
z_wnew (unit *ftnunit)
{
   while (icpos < ftnunit->f77errlist.icirlen && icptr < icend) {
      *icptr++ = ' ';
      icpos++;
   }
   icptr = ftnunit->f77errlist.iciunit + (++icnum) * ftnunit->f77errlist.icirlen;
   ftnunit->f77cursor = ftnunit->f77recpos = ftnunit->f77recend = icpos = 0;
   return (0);
}

int
z_rnew (unit *ftnunit)
{
   icptr = ftnunit->f77errlist.iciunit + 
	 (++icnum) * ftnunit->f77errlist.icirlen;
   ftnunit->f77cursor = ftnunit->f77recpos = icpos = 0;
   return 0;			/* DAG -- bug fix (was missing) */
}

int s_wsfi (icilist *a)
{
    return( s_wsfi_mp( a, &f77curunit ) );
}

int s_wsfi64 (icilist64 *a)
{
    return( s_wsfi64_mp( a, &f77curunit ) );
}

int s_wsfi_mp (icilist *a, unit** fu)
{
    icilist64 a64;
    a64.icierr = a->icierr;
    a64.iciunit = a->iciunit;
    a64.iciend = a->iciend;
    a64.icifmt = a->icifmt;
    a64.icirlen = a->icirlen;
    a64.icirnum = a->icirnum;
    return( s_wsfi64_mp( &a64, fu ) );
}

int s_wsfi64_mp (icilist64 *a, unit** fu)
{
   int             n;
   unit *ftnunit;
   
   if (!f77init)
      f_init ();
   ftnunit = *fu = Internal_File;

   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
#ifdef I90
   ftnunit->f90sw = 0;
#endif
   if (n = c_si (a, ftnunit)) {
      return (n);
   }
   ftnunit->uwrt |= WR_OP;
   ftnunit->f77doed = w_ed;
   ftnunit->f77doned = w_ned;
   ftnunit->f77putn = z_putc;
   ftnunit->f77ungetn = z_ungetc;
   ftnunit->f77donewrec = z_wSL;
   ftnunit->f77dorevert = ftnunit->f77doend = z_wnew;
   return (0);
}

int
c_si (icilist64 *a, unit *ftnunit)
{
   ftnunit->f77fmtbuf = a->icifmt;
   if (f77vfmt_com_.PFI != NULL) {
      /*
       * If something has been placed in the global static f77vfmt_com_
       * structure, then use it.  This means we must be running an
       * executable built with a pre-mongoose compiler which is using
       * variable format I/O.
       */
      ftnunit->vfmt = f77vfmt_com_.PFI;
      ftnunit->vfmtfp = f77vfmt_com_.static_link;
   } else {
      /*
       * Otherwise, grab the same information from the icilist structure.
       * This means one of three things:
       * 1) executable built with pre-mongoose compiler, no variable format:
       *       The civfmt and civfmtfp fields are actually past the end
       *       of the space allocated for the icilist structure, so we
       *       are reading something else which is unknown, and it will
       *       not be used.  This is hopefully not a problem.
       * 2) executable built with mongoose compiler, no variable format:
       *       The civfmt and civfmtfp fields are there in the icilist
       *       structure, but are uninitialized and the values will not
       *       be used.  This is not a problem.
       * 3) executable built with mongoose compiler, variable format:
       *       The values are there, are valid, and will be used.
       */
      ftnunit->vfmt = a->icivfmt;
      ftnunit->vfmtfp = a->icivfmtfp;
   }
   ftnunit->f77cblank = ftnunit->f77cplus = ftnunit->f77scale = 0;
   ftnunit->f77errlist.cierr = a->icierr;
   ftnunit->f77errlist.ciend = a->iciend;
   ftnunit->f77errlist.cieor = 0;
   ftnunit->f77errlist.cisize = 0;
   ftnunit->f77errlist.iciunit = a->iciunit;
   ftnunit->f77errlist.icirlen = a->icirlen;
   ftnunit->f77errlist.icirnum = a->icirnum;
   ftnunit->f77errlist.iciunit = a->iciunit;
   ftnunit->f77recpos = ftnunit->f77recend = icnum = icpos = 0;
   if (pars_f (ftnunit, ftnunit->f77fmtbuf) < 0)
      errret(a->icierr, 100, "startint");
   fmt_bg (ftnunit);
   icptr = ftnunit->f77errlist.iciunit;
   icend = icptr + ftnunit->f77errlist.icirlen * ftnunit->f77errlist.icirnum;
   return (0);
}

int
z_rSL (unit *ftnunit)
{
   z_rnew (ftnunit);
   return (0);
}

int
z_wSL (unit *ftnunit)
{
   z_wnew (ftnunit);
   return (0);
}

#pragma weak e_rsfi64 = e_rsfi

int e_rsfi ()
{
    return( e_rsfi_mp( &f77curunit ) );
}

#pragma weak e_rsfi64_mp = e_rsfi_mp

int e_rsfi_mp (unit **fu)
{
   int             n;

   n = en_fio (fu);
   /* (*fu)->f77fmtbuf = NULL; */
   (*fu)->lock_unit = 0;
   return (0);
}

#pragma weak e_wsfi64 = e_wsfi

int e_wsfi ()
{
    return( e_wsfi_mp( &f77curunit ) );
}

#pragma weak e_wsfi64_mp = e_wsfi_mp

int e_wsfi_mp (unit **fu)
{
   int             n;

   n = en_fio (fu);
   /* (*fu)->f77fmtbuf = NULL; */


   /* Don't do this; it's already been done by en_fio 
       Richard Shapiro 8/9/95*/
   /* z_wnew (*fu); */
   (*fu)->lock_unit = 0;
   return (n);
}

int
y_ierr (unit *ftnunit)
{
   err (ftnunit->f77errlist.cierr, 110, "iio");
}
