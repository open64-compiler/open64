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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/sfe.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
/* sequential formatted external common routines*/
#ifdef _BSD
#include <sys/errno.h>
#endif				/* _BSD */
#include <mutex.h>
#include <stdio.h>
#include <string.h>
#include <cmplrs/fio.h>
#include "fmt.h"
#include "uio.h"
#include "due.h"
#include "err.h"
#include "open.h"
#include "sfe.h"
#include "idxio.h"
#include "due.h"
#include "bcompat.h"

int
e_rsfe (void)
{
    return( e_rsfe_mp( &f77curunit ) );
}

int
e_rsfe_mp (unit **fu)
{
   int             n;
   unit 	  *ftnunit = *fu;

   if (ftnunit->ufmt == 2)
      n = 0;
   else
#ifdef _BSD
redo:
#endif
      n = en_fio (&ftnunit);
   ftnunit->f77fmtbuf = NULL;
   /* BN-8126 & BN-8222 */
#ifdef _BSD
   if (ftnunit->ufd && ferror (ftnunit->ufd)) {
      if (errno == EWOULDBLOCK) {
	 clearerr(ftnunit->ufd);
	 goto redo;
      }
      errret(ftnunit->f77errlist.cierr, errno, "sfe");
   }
#else
   if (ftnunit->ufd && ferror (ftnunit->ufd))
      errret(ftnunit->f77errlist.cierr, errno, "sfe");
#endif				/* _BSD */
   if (ftnunit->ushared)
       fflush(ftnunit->ufd);

   ftnunit->lock_unit = 0;
   return (n);
}


int c_sfe (cilist64 *a, unit **fu)
{
   extern vfmt_struct f77vfmt_com_;
   unit           *ftnunit;

   if ((ftnunit = *fu = map_luno (a->ciunit)) == NULL)
      errret(a->cierr, 101, "startio");
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   if (ftnunit->uconn <= 0 && fk_open (SEQ, FMT, a->ciunit)) {
      ftnunit->uconn = 0;
      errret(a->cierr, 114, "sfe")
   }
   if(!ftnunit->ufmt) errret(a->cierr,102,"sfe")
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
       * Otherwise, grab the same information from the cilist structure.
       * This means one of three things:
       * 1) executable built with pre-mongoose compiler, no variable format:
       *       The civfmt and civfmtfp fields are actually past the end
       *       of the space allocated for the cilist structure, so we
       *       are reading something else which is unknown, and it will
       *       not be used.  This is hopefully not a problem.
       * 2) executable built with mongoose compiler, no variable format:
       *       The civfmt and civfmtfp fields are there in the cilist
       *       structure, but are uninitialized and the values will not
       *       be used.  This is not a problem.
       * 3) executable built with mongoose compiler, variable format:
       *       The values are there, are valid, and will be used.
       */
      ftnunit->vfmt = a->civfmt;
      ftnunit->vfmtfp = a->civfmtfp;
   }
   return (0);
}

int
e_wsfe (void)
{
    return( e_wsfe_mp( &f77curunit ) );
}


int
e_wsfe_mp (unit **fu)
{
   unit 	*ftnunit = *fu;
   int n;

   if (ftnunit->ufmt == 2)
      n = e_wsue_mp (fu);
   else
      n = e_rsfe_mp (fu);
   return (n);
}


int
e_xsfe (void)
{
    return( e_xsfe_mp( &f77curunit ) );
}

int
e_xsfe_mp (unit **fu)
{
   return (e_rsfe_mp (fu));
}

int
e_wsue (void)
{
    return( e_wsue_mp( &f77curunit ) );
}


int
e_wsue_mp (unit **fu)
{
   ftnll            loc;
   unit		  *ftnunit = *fu;
   int n;

   if (ftnunit->uacc == KEYED) {
      n = idxwrite(ftnunit);
      ftnunit->lock_unit = 0;
      return (n);
   } else if (ftnunit->ufmt == 2) {
/* FORMATTED BINARY CASE */
      (void) fwrite ((char *) &ftnunit->f77recend, sizeof (int), 1, ftnunit->ufd); /* CALVIN */
      (void) fwrite (ftnunit->f77fio_buf, ftnunit->f77recend, 1, ftnunit->ufd);
      if (fwrite ((char *) &ftnunit->f77recend, sizeof (int), 1, ftnunit->ufd) != 1)
	 errret(ftnunit->f77errlist.cierr, errno, "system write error");
      ftnunit->f77nonl = ftnunit->f77recpos = ftnunit->f77recend = ftnunit->f77cursor = 0;
   } else if (ftnunit->url != 1) {
      /* Fix a bug where direct-access files are called without REC=
       * clause */
      if (ftnunit->uacc == DIRECT) {
	 return( e_wdue_mp (fu) );
      } else if (ftnunit->overflowed) {
	 if (est_reclen && est_reclen == ftnunit->f77reclen) {
	    goto no_overflowed;
	 } else {
	    if (ftnunit->f77recpos)
	       fwrite (ftnunit->f77fio_buf, ftnunit->f77recpos, 1, ftnunit->ufd);
	    {
	    int f77reclen_32bit = ftnunit->f77reclen;
	    fwrite ((char *) &f77reclen_32bit, sizeof (int), 1, ftnunit->ufd);
	    }
	    if ((loc = FTELL (ftnunit->ufd)) == -1)
	       errret(ftnunit->f77errlist.cierr, 169, "sue");
	    (void) FSEEK (ftnunit->ufd, -((ftnll)ftnunit->f77reclen + 8), 1);
	    {
	    int f77reclen_32bit = ftnunit->f77reclen;
	    if (fwrite ((char *) &f77reclen_32bit, sizeof (int), 1, ftnunit->ufd) != 1)
	       errret(ftnunit->f77errlist.cierr, errno, "system write error");
	    }
	    (void) FSEEK (ftnunit->ufd, loc, 0);
	 }
      } else {
	 *(int *) ftnunit->f77fio_buf = ftnunit->f77reclen;
   no_overflowed:
	 if (!ftnunit->f77recpos) {
	    int f77reclen_32bit = ftnunit->f77reclen;
	    if (fwrite (&f77reclen_32bit, sizeof (int), 1, ftnunit->ufd) != 1)
	       errret(ftnunit->f77errlist.cierr, errno, "system write error");
	 } else {
	    int f77reclen_32bit = ftnunit->f77reclen;
	    memcpy (ftnunit->f77fio_buf + ftnunit->f77recpos, &f77reclen_32bit, 4);
	    if (fwrite (ftnunit->f77fio_buf, ftnunit->f77recpos + 4, 1, ftnunit->ufd) != 1)
	       errret(ftnunit->f77errlist.cierr, errno, "system write error");
	 }
	 if (ftnunit->ufd && ferror (ftnunit->ufd))
	    errret(ftnunit->f77errlist.cierr, errno, "sue");
      }
   }
   if (ftnunit->ushared)
       fflush(ftnunit->ufd);
   ftnunit->lock_unit = 0;
   return (0);
}

#pragma weak e_rsfe64 = e_rsfe		/* extern int e_rsfe64(void); */
#pragma weak e_rsfe64_mp = e_rsfe_mp	/* extern int e_rsfe64_mp(unit **); */
#pragma weak e_wsfe64 = e_wsfe		/* extern int e_wsfe64(void); */
#pragma weak e_wsfe64_mp = e_wsfe_mp	/* extern int e_wsfe64_mp(unit **); */
#pragma weak e_xsfe64 = e_xsfe		/* extern int e_xsfe64(void); */
#pragma weak e_xsfe64_mp = e_xsfe_mp	/* extern int e_xsfe64_mp(unit **); */
#pragma weak e_wsue64 = e_wsue		/* extern int e_wsue64(void); */
#pragma weak e_wsue64_mp = e_wsue_mp	/* extern int e_wsue64_mp(unit **); */

