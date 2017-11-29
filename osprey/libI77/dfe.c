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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/dfe.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/

#include <cmplrs/fio.h>
#include <mutex.h>
#include "fmt.h"
#include "iomode.h"
#include "dfe.h"
#include "err.h"
#include "util.h"
#include "rdfmt.h"
#include "wrtfmt.h"
#include "open.h"
#include "bcompat.h"

#define ASSOCV	12

static int c_dfe (cilist64 *a, unit **fu);

int
s_rdfe (cilist *a)
{
    return( s_rdfe_mp( a, &f77curunit ) );
}


int
s_rdfe_mp (cilist *a, unit **fu) {
    cilist64 a64;

    get_cilist64( &a64, a );
    return( s_rdfe64_mp( &a64, fu ) );
}

int
s_rdfe64 (cilist64 *a)
{
    return( s_rdfe64_mp( a, &f77curunit ) );
}

int s_rdfe64_mp(cilist64 *a, unit **fu) {

   int             n;
   unit            *ftnunit;

   if (!f77init)
      f_init ();
   if (n = c_dfe (a, fu)) {
      if (*fu) (*fu)->lock_unit = 0;
      return (n);
   }
   ftnunit = *fu;
   if (ftnunit->uwrt & WR_OP)
      (void) f77nowreading (ftnunit);
#ifdef I90
   if (ftnunit->uaction == WRITEONLY )
	errret(ftnunit->f77errlist.cierr,180,"startread");
#endif
   ftnunit->f77getn = y_getc;
   ftnunit->f77gets = y_gets;
   ftnunit->f77ungetn = y_ungetc;
   ftnunit->f77doed = rd_ed;
   ftnunit->f77doned = rd_ned;
   ftnunit->f77donewrec = yrd_SL;
   ftnunit->f77dorevert = ftnunit->f77doend = y_rsk;
   if (pars_f (ftnunit, ftnunit->f77fmtbuf) < 0)
      errret(a->cierr, 100, "startio");
   fmt_bg (ftnunit);
   return (0);
}

int
s_wdfe (cilist *a) {
    return( s_wdfe_mp( a, &f77curunit ) );
}


int
s_wdfe_mp (cilist *a, unit **fu) {
    cilist64 a64;

    get_cilist64( &a64, a );
    return( s_wdfe64_mp( &a64, fu ) );
}

int
s_wdfe64 (cilist64 *a)
{
    return( s_wdfe64_mp( a, &f77curunit ) );
}

int s_wdfe64_mp (cilist64 *a, unit **fu) {

   int             n;
   unit		*ftnunit;

   if (!f77init)
      f_init ();
   if (n = c_dfe (a, fu)) {
      if (*fu) (*fu)->lock_unit = 0;
      return (n);
   }
   ftnunit = *fu;
   if (ftnunit->uwrt != WR_READY && f77nowwriting (ftnunit))
      errret(a->cierr, 160, "startwrt");
   ftnunit->f77putn = y_putc;
   ftnunit->f77ungetn = y_ungetc;
   ftnunit->f77doed = w_ed;
   ftnunit->f77doned = w_ned;
   ftnunit->f77donewrec = y_wSL;
   ftnunit->f77dorevert = y_rev;
   ftnunit->f77doend = y_end;
   ftnunit->uirec = a->cirec;
   if (ftnunit->umaxrec && (a->cirec > ftnunit->umaxrec))
      errret(a->cierr, 159, "startwrt");
   if (pars_f (ftnunit, ftnunit->f77fmtbuf) < 0)
      errret(a->cierr, 100, "startwrt");
   fmt_bg (ftnunit);
   return (0);
}


int
e_rdfe ()
{
    return( e_rdfe_mp( &f77curunit ) );
}



int
e_rdfe_mp (unit **fu)
{
   unit *ftnunit = *fu;
   (void) en_fio (fu);
   if (ftnunit->ufd && ferror (ftnunit->ufd))
      errret(ftnunit->f77errlist.cierr, errno, "dfe");
   ftnunit->lock_unit = 0;
   return (0);
}


int
e_wdfe ()
{
   return( e_wdfe_mp( &f77curunit ) );
}


int
e_wdfe_mp (unit **fu)
{
   unit *ftnunit = *fu;
   (void) en_fio (fu);
   if (ftnunit->ufd && ferror (ftnunit->ufd))
      errret(ftnunit->f77errlist.cierr, errno, "dfe");
   if (ftnunit->ushared)
      fflush (ftnunit->ufd);
   ftnunit->lock_unit = 0;
   return (0);
}

static int
c_dfe (cilist64 *a, unit **fu) 
{
   unit		*ftnunit;
extern FILE *debugfile;

   if ((ftnunit = *fu = find_luno (a->ciunit)) == NULL)
      if (fk_open (DIR, FMT, a->ciunit))
	 err(a->cierr, 104, "dfe");
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   ftnunit->f77errlist.cierr = a->cierr;
   ftnunit->f77errlist.ciend = a->ciend;
   ftnunit->f77errlist.cieor = a->cieor;
   ftnunit->f77errlist.cisize = a->cisize;
   ftnunit->f77errlist.iciunit = 0;
   ftnunit->f77cursor = ftnunit->f77recpos = ftnunit->f77recend = 0;
   ftnunit->f77scale = 0;
   ftnunit->ufd = ftnunit->ufd;

   if (!ftnunit->ufmt)
      err(a->cierr, 102, "dfe")
	 if (!ftnunit->useek)
	 err(a->cierr, 104, "dfe")
	    if (a->cirec < 1)
	    err(a->cierr, 168, "dfe");
   ftnunit->f77fmtbuf = a->cifmt;

   /* fprintf( debugfile, "At position %d for thread %d, oldrec = %d, newrec = %d\n", ftell( ftnunit->ufd ), mp_my_threadnum_(), ftnunit->uirec, a->cirec ); */
   if (FSEEK (ftnunit->ufd,  (ftnll)ftnunit->url * (a->cirec - 1), 0))
      err( a->cierr, errno, "Direct formatted");
   if (ftnunit->uassocv)
      set_var ((ftnintu *)ftnunit->uassocv,
	       ftnunit->umask, ASSOCV, a->cirec);
   ftnunit->uend = 0;
   return (0);
}

int
y_rsk (unit *ftnunit)
{
   yrd_SL (ftnunit);
   return (0);
}

int
yrd_SL (unit *ftnunit)
{
   inc_var ((ftnintu *)ftnunit->uassocv, ftnunit->umask, ASSOCV);
   if (ftnunit->uend || ftnunit->url <= ftnunit->f77recpos
       || ftnunit->url == 1) {
      if (ftnunit->url > 1)
	 ftnunit->f77cursor = ftnunit->f77recpos = ftnunit->f77recend = 0;
      return(0);
   }
   do {
      getc (ftnunit->ufd);
   } while (++ftnunit->f77recpos < ftnunit->url);
   ftnunit->f77cursor = ftnunit->f77recpos = ftnunit->f77recend = 0;
   return (0);
}

int
y_ungetc (unit *ftnunit, int ch)
{
   if (!ftnunit->useek)
      return (-1);
   (void) fseek (ftnunit->ufd, -1L, 1);
   ftnunit->f77recpos--;
   return (ch);
}

int
y_getc (unit *ftnunit)
{
   int             ch;

   if (ftnunit->uend)
      return (-1);
   if ((ch = getc (ftnunit->ufd)) != EOF) {
      ftnunit->f77recpos++;
      if (ftnunit->url >= ftnunit->f77recpos ||
	  ftnunit->url == 1)
	 return (ch);
      else
	 return (' ');
   }
   if (feof (ftnunit->ufd)) {
      ftnunit->uend = 1;
      errno = 0;
      return (-1);
   }
   err(ftnunit->f77errlist.cierr, errno, "readingd");
}

int
y_gets (unit *ftnunit, char *s, int w, char unused_c) {
   register int    ch, n;

   if (ftnunit->uend)
      return (-1);
   if (ftnunit->url > 1) {
      n = ftnunit->url - ftnunit->f77recpos;
      w = n < w ? n : w;
   }
   for (n = 0; n < w; n++) {
      if ((ch = getc (ftnunit->ufd)) == EOF)
	 break;
      *(s++) = (char) ch;
   }
   ftnunit->f77recpos += n;
   if (n == w)
      return (n);
   if (feof (ftnunit->ufd)) {
      ftnunit->uend = 1;
      errno = 0;
      return (EOF);
   }
   err(ftnunit->f77errlist.cierr, errno, "readingd");
}

int
y_putc (unit *ftnunit, register XINT count, register char con, register char *buf) {
   register XINT    new_size;
   register int    i;

   new_size = ftnunit->f77recpos + count;
   if (new_size > ftnunit->url && ftnunit->url > 1)
      err(ftnunit->f77errlist.cierr, 110, "dout");

   if (buf)
      while (count--)
	 putc (*buf++, ftnunit->ufd);

   else {
      if (con)
	 while (count--)
	    putc (con, ftnunit->ufd);
      else if ((ftnunit->f77recpos + count) > ftnunit->f77recend) {
	 i = ftnunit->f77recend - ftnunit->f77recpos;
	 if (!ftnunit->useek)
	    return (-1);
	 (void) fseek (ftnunit->ufd, (long) i, 1);
	 i = count - i;
	 while (i--)
	    putc (' ', ftnunit->ufd);
      } else {
	 if (!ftnunit->useek)
	    return (-1);
	 (void) fseek (ftnunit->ufd, (long) count, 1);
      }
   }


   ftnunit->f77recpos = new_size;

   if (ftnunit->f77recpos > ftnunit->f77recend)
      ftnunit->f77recend = ftnunit->f77recpos;
   return (0);

}

int
y_rev (unit *ftnunit)
{				/* what about work done? */
   if (ftnunit->url != 1 && ftnunit->f77recpos < ftnunit->url)
      y_wSL (ftnunit);
   else {
      ftnunit->f77cursor = ftnunit->f77recpos = ftnunit->f77recend = 0;
      inc_var ((ftnintu *)ftnunit->uassocv, ftnunit->umask, ASSOCV);
   }
   return (0);
}

int
y_end (unit *ftnunit)
{
   y_wSL (ftnunit);
   return (0);
}

int
y_wSL (unit *ftnunit)
{
   if (ftnunit->f77recpos < ftnunit->url)
      (*ftnunit->f77putn) (ftnunit, ftnunit->url - ftnunit->f77recpos, ' ', NULL);
   inc_var ((ftnintu *)ftnunit->uassocv, ftnunit->umask, ASSOCV);
   ftnunit->f77cursor = ftnunit->f77recpos = ftnunit->f77recend = 0;
   return (0);
}

#pragma weak e_rdfe64 = e_rdfe
#pragma weak e_rdfe64_mp = e_rdfe_mp
#pragma weak e_wdfe64 = e_wdfe
#pragma weak e_wdfe64_mp = e_wdfe_mp

