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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/rsfe.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

#include <stdio.h>
#include <cmplrs/fio.h>
#include "fmt.h"
#include "iomode.h"
#include "vmsflags.h"
#include "idxio.h"
#include "rsfe.h"
#include "rdfmt.h"
#include "err.h"
#include "uio.h"
#include "sfe.h"
#include "util.h"
#include "bcompat.h"
#include <string.h>

#define NOADVANCE_AT_REV	2  /* also in rsfe90.c */

extern int lnblnk_ (char *s, int len);	/* in libF77 */

static
int s_rsfe_com (cilist64 *a, unit **fu)
{
   int             n;
   unit           *ftnunit;

   if (!f77init)
      f_init ();
   if (n = c_sfe (a, fu))
      return (n);
   ftnunit = *fu;

/* Fix BN 9768.
 * If user tries to read from stdout  then cause startio to abort with an
 * error. This is particularly importrant if a user is tries to read from
 * fortran unit 6 , which by default is connected to stdout. Even though
 * the standard says nothing about the way unit 5 and unit 6 should be
 * handled, I generally think that it is a good idea to distinguish 
 * between these and not allow users to read from stdout.
 * ---ravi---1/7/92
 */
   /* Fix 12308: Read error from stdout should occur for only files
    * connected to terminal. The change is taken from the 'fix'
    * section of the bug description - Bhaskar 08/14/92 */
   if (ftnunit->ufd == stdout && isatty (fileno (ftnunit->ufd)))
      errret(a->cierr, 173, "startio");
   ftnunit->f77errlist.cierr = a->cierr;
   ftnunit->f77errlist.ciend = a->ciend;
   ftnunit->f77errlist.cieor = a->cieor;
   ftnunit->f77errlist.cisize = a->cisize;
   ftnunit->f77errlist.iciunit = 0; 
   ftnunit->f77scale = 0;
   ftnunit->f77fmtbuf = a->cifmt;
   if (ftnunit->uacc == KEYED) {
      ftnunit->f77idxlist.cimatch = a->cimatch;
      ftnunit->f77idxlist.cikeytype = a->cikeytype;
      ftnunit->f77idxlist.cikeyval.cicharval = a->cikeyval.cicharval;
      ftnunit->f77idxlist.cikeyid = a->cikeyid;
      ftnunit->f77idxlist.cinml = a->cinml;
      ftnunit->f77idxlist.cikeyvallen = a->cikeyvallen;
   }
   else {
      if (ftnunit->uacc == DIRECT)
	 errret(a->cierr, 171, "sequential read");
      if (ftnunit->ualias->ucc == CC_FORTRAN &&
	  ftnunit->ualias->ucchar) {
	 putc (ftnunit->ualias->ucchar, ftnunit->ualias->ufd);
	 ftnunit->ualias->ucchar = '\0';
      } else if (ftnunit->ucc == CC_FORTRAN && ftnunit->ucchar) {
	 putc (ftnunit->ucchar, ftnunit->ufd);
	 ftnunit->ucchar = '\0';
      }
   }
#ifdef I90
   ftnunit->f90sw = 0;
   ftnunit->f90nadv = 0;
#endif
   if (pars_f (ftnunit, ftnunit->f77fmtbuf) < 0)
      errret(a->cierr, 100, "startio");
   ftnunit->f77getn = x_getc;
   ftnunit->f77gets = x_gets;
   ftnunit->f77ungetn = x_ungetc;
   ftnunit->f77doed = rd_ed;
   ftnunit->f77doned = rd_ned;
   fmt_bg (ftnunit);
   ftnunit->f77doend = x_endp;
   ftnunit->f77donewrec = xrd_SL;
   ftnunit->f77dorevert = x_rev;
   ftnunit->f77cblank = ftnunit->ublnk;
   ftnunit->f77cplus = 0;

   if (ftnunit->ufd == stdin && feof (ftnunit->ufd) && f77vms_flag_[VMS_IN])
      clearerr(ftnunit->ufd);
   (void) f77nowreading (ftnunit);
   check_buflen( ftnunit, ftnunit->url > FIO_ALLOC ? ftnunit->url : FIO_ALLOC );

#ifdef I90
   if (ftnunit->uaction == WRITEONLY ) 
       errret(ftnunit->f77errlist.cierr,180,"startread");
   ftnunit->f77recpos = 0;
   ftnunit->f77recend = 0;
#endif

   n = xrd_SL (ftnunit);
   if (n > 0) {
     errret(a->cierr, n, "s_rsfe");
   } else if (n < 0) {
     errret(a->ciend, n, "s_rsfe");
   } else 
     return(0);
}

int
s_rsfe (cilist *a)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_rsfe_com(&dst, &f77curunit);
}


int
s_rsfe_mp (cilist *a, unit **fu)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_rsfe_com(&dst, fu);
}


int
s_rsfe64 (cilist64 *a)
{
    return( s_rsfe_com( a, &f77curunit) );
}

int
s_rsfe64_mp (cilist64 *a, unit **fu)
{
    return( s_rsfe_com( a, fu) );
}



int
xrd_SL (unit *ftnunit)
{
   int             n;


   if (ftnunit->uacc == KEYED) {
      if (n = idxread(ftnunit))
	 return n;
   } else {
      if (ftnunit->ufmt == 2) {
         if (fread ((char *) &ftnunit->f77recend, sizeof (int), 1, ftnunit->ufd) != 1) {
	    clearerr(ftnunit->ufd);
	    return (errno);
         }
         /* make sure there is enough buffer space */
         check_buflen (ftnunit, ftnunit->f77recend + (int) sizeof(int));
         if (fread (ftnunit->f77fio_buf, ftnunit->f77recend + sizeof (int), 1, ftnunit->ufd) != 1) {
	    clearerr(ftnunit->ufd);
	    return (errno);
         }
         ftnunit->f77cursor = ftnunit->f77recpos = 0;
         return (0);
      }
         /* For regular formatted file */
#ifdef I90
  /* f77recpos cannot be used as a flag for all f77donewrec functions
  ** since it could point to y_rdXL to skip to the end of the
  ** current record and destroying the current f77recpos by
  ** resetting it to 0 would amke that impossible.  My guess
  ** is that the whole reason for this setting of f77recpos to 0
  ** in the first place is simply to deal with the NOADVACE
  ** feature in F90 when reaching the end of a FORMAT specification
  ** (and the function x_rev is called.  
  */
  if (ftnunit->f90nadv != NOADVANCE_AT_REV) {
#endif
      ftnunit->f77recend = 0;
      while ((n = getc (ftnunit->ufd)) != EOF && n != '\n') {
         ftnunit->f77fio_buf[ftnunit->f77recend] = (char) n;
         if (++ftnunit->f77recend == ftnunit->f77fio_size)
	 ftnunit->f77fio_buf = realloc (ftnunit->f77fio_buf, ftnunit->f77fio_size += FIO_ALLOC);
      }
#ifdef I90
    }
#endif
      ftnunit->f77fio_buf[ftnunit->f77recend] = '\0';/* replace '\n' with null */
      if (n == EOF && ftnunit->f77recend == 0) {
         ftnunit->uend = 1;
         return (EOF);
      }
   }
   ftnunit->f77cursor = ftnunit->f77recpos = 0;
   return (0);
}

int
x_ungetc (unit *ftnunit, int i)
{
   ftnunit->f77recpos--;
   return (i);
}

int
x_gets (unit *ftnunit, char *s, int w, char c)
{
   register int    n;
   register char  *t;

   /* Do not return EOF for pipe input since it might be closed and
    * reopened again.  See SCR 12282 */
   if (ftnunit->uend && !ftnunit->uistty)
      return (EOF);
   if (f77vms_flag_[VMS_EF] && ftnunit->f77recend == 1 && ftnunit->f77fio_buf[0] == '\032')
      return (EOF);
   n = ftnunit->f77recend - ftnunit->f77recpos;
   t = ftnunit->f77fio_buf + ftnunit->f77recpos;
   w = n < w ? n : w;
   if (ftnunit->ufmt == 2) {
      memcpy (s, t, w);
      ftnunit->f77recpos += w;
      return (w);
   } else
      for (n = 0; n < w; n++) {
	 if (*t == c) {
	    ftnunit->f77recpos++;
	    break;
	 }
	 *(s++) = *(t++);
      }
   ftnunit->f77recpos += n;
   return (n);
}

int
x_getc (unit *ftnunit)
{
   if (f77vms_flag_[VMS_EF] && ftnunit->f77recend == 1 && ftnunit->f77fio_buf[0] == '\032')
      return (EOF);
   if (ftnunit->f77recpos < ftnunit->f77recend)
      return (ftnunit->f77fio_buf[ftnunit->f77recpos++]);
   return ((int)'\n');
}

int
x_endp (unit *ftnunit)
{
#ifdef I90
    if (ftnunit->f90sw)
	if (ftnunit->f90nadv == 0 || ftnunit->f90eor != 0 ) {
	    ftnunit->f77recpos = ftnunit->f77cursor = ftnunit->f77recend = 0;
        }
#endif
   return (0);
}

int
x_rev (unit *ftnunit)
{
   int             n;
   int		   save_f90nadv = ftnunit->f90nadv;
   XINT            save_recpos = ftnunit->f77recpos;

   if (ftnunit->f90nadv) ftnunit->f90nadv = NOADVANCE_AT_REV;
   n = xrd_SL(ftnunit);
   if (save_f90nadv) {
      ftnunit->f90nadv = save_f90nadv;
      ftnunit->f77recpos = save_recpos;
   }
   return n;
}
