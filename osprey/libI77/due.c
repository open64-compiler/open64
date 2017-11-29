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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/due.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/

#include <cmplrs/fio.h>
#include <mutex.h>
#include <string.h>
#include "fmt.h"
#include "iomode.h"
#include "due.h"
#include "err.h"
#include "open.h"
#include "uio.h"
#include "fio_direct_io.h"
#include <sys/types.h>
#include <unistd.h>
#include "bcompat.h"


#define ASSOCV	12

/* fwd reference */
static int c_due (cilist64 *a, unit **fu);
static int f_find_com (flist64 *a, int lock);

int
f_find (flist *a)
{
    flist64 a64;

    a64.ferr = a->ferr;;
    a64.funit = a->funit;;
    a64.frec  = a->frec;
    return( f_find_com( &a64, 0 ) );
}

int
f_find_mp (flist *a)
{
    flist64 a64;

    a64.ferr = a->ferr;;
    a64.funit = a->funit;;
    a64.frec  = a->frec;
    return( f_find_com( &a64, 1 ) );
}

int
f_find64 (flist64 *a)
{
    return( f_find_com( a, 0 ) );
}

int
f_find64_mp (flist64 *a)
{
    return( f_find_com( a, 1 ) );
}


static int
f_find_com (flist64 *a, int lock)
{
   unit *ftnunit;

   if (!f77init)
      f_init ();
   if ((ftnunit = map_luno (a->funit)) == NULL)
      err(a->ferr, 101, "find");
   while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   if (ftnunit->uconn <= 0 && fk_open (DIR, UNF, a->funit)) {
      ftnunit->uconn = 0;
      errret(a->ferr, 114, "find");
   }
   ftnunit->f77recpos = 0;
   ftnunit->ufd = ftnunit->ufd;
   if (!ftnunit->useek)
      errret(a->ferr, 104, "find");
   /* This would coredump with a direct unformatted file 
   (void) fseek (ftnunit->ufd, (long) (a->frec - 1) * ftnunit->url, 0);
   */
   if (ftnunit->ufmt == 0) {
      ftnll offset = (a->frec - 1) * ftnunit->url;
      (void) LSEEK ((int) ftnunit->ufd,  offset, 0);
      /* need to change the internal buffer position in fio_direct_io as well */
      _fio_set_seek((int) ftnunit->ufd,  offset, 0);
   } else
      (void) FSEEK ( ftnunit->ufd,  (ftnll)(a->frec - 1) * ftnunit->url, 0);

   if (ftnunit->uassocv)
      set_var ((ftnintu *)ftnunit->uassocv, ftnunit->umask, ASSOCV, a->frec);
   if (ftnunit->umaxrec && (a->frec > ftnunit->umaxrec))
      errret(a->ferr, 159, "find");
   ftnunit->uend = 0;
   if (lock) ftnunit->lock_unit = 0;
   return (0);
}

int
s_rdue (cilist *a)
{
   return (s_rdue_mp (a, &f77curunit));
}


int
s_rdue_mp (cilist *a, unit **fu)
{
    cilist64 a64;

    get_cilist64( &a64, a );
    return( s_rdue64_mp( &a64, fu ) );
}

int
s_rdue64(cilist64 *a)
 {
 return( s_rdue64_mp( a, &f77curunit ));
 }

int
s_rdue64_mp (cilist64 *a, unit **fu)
{
   int             n;
   unit	          *ftnunit;

   if (n = c_due (a, fu)) {
      if (*fu) (*fu)->lock_unit = 0;
      return (n);
   }
   /* Don't ever call f77nowreading here.  It takes FILE* only */
   ftnunit = *fu;
   ftnunit->uwrt &= ~WR_OP;
   ftnunit->f77do_unf = do_ud;
#ifdef I90
   if (ftnunit->uaction == WRITEONLY )
	errret(ftnunit->f77errlist.cierr,180,"startread");
#endif
   return (0);
}

int
s_wdue (cilist *a)
{
   return( s_wdue_mp( a, &f77curunit ));
}


int
s_wdue_mp (cilist *a, unit **fu)
{
    cilist64 a64;

    get_cilist64( &a64, a );
    return( s_wdue64_mp( &a64, fu ) );
}

int
s_wdue64(cilist64 *a)
 {
 return( s_wdue64_mp( a, &f77curunit ));
 }

int
s_wdue64_mp (cilist64 *a, unit **fu)
{
   int             n;

   if (n = c_due (a, fu)) {
      if (*fu) (*fu)->lock_unit = 0;
      return (n);
   }

   /* Don't ever call f77nowwriting here.  It takes FILE* on;y */
   (*fu)->uwrt |= WR_OP;
   (*fu)->f77do_unf = do_ud;
   return (0);
}

static int
c_due (cilist64 *a, unit **fu)
{
   unit *ftnunit;
   if (!f77init)
      f_init ();

   ftnunit = *fu = map_luno (a->ciunit);
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   /* 
    * If unit is not OPENed, we open it. 
    */

   if (ftnunit->uconn <= 0 && fk_open (DIR, UNF, a->ciunit)) {
      ftnunit->uconn = 0;
      err(a->cierr, 104, "due");
   }
   ftnunit->f77recpos = 0;
   
   ftnunit->f77errlist.cierr = a->cierr;
   ftnunit->f77errlist.ciend = a->ciend;
   ftnunit->f77errlist.cieor = a->cieor;
   ftnunit->f77errlist.cisize = a->cisize;
   ftnunit->f77errlist.iciunit = 0;

   ftnunit->ufd = ftnunit->ufd;

   if (ftnunit->ufmt > 0)
      err(a->cierr, 102, "cdue");
   if (!ftnunit->useek)
      err(a->cierr, 104, "cdue");
   if (ftnunit->uconn <= 0)
      err(a->cierr, 114, "cdue");
   if (a->cirec < 1)
      err(a->cierr, 168, "dfe");

   /* 
    * Set the record number. 
    */

   if (ftnunit->url != 1) {
      ftnunit->uirec = a->cirec;
   } else 
      ftnunit->uirec = a->cirec - 1;

   if (ftnunit->uassocv)
      set_var ((ftnintu *)ftnunit->uassocv, ftnunit->umask, ASSOCV, a->cirec);
   if (ftnunit->umaxrec && (a->cirec > ftnunit->umaxrec))
      err(a->cierr, 159, "cdue");
   ftnunit->uend = 0;
   return (0);
}


int
e_rdue (void)
{
   return( e_rdue_mp( &f77curunit ) );
}


int
e_rdue_mp (unit **fu)
{
   inc_var ((ftnintu *)(*fu)->uassocv, (*fu)->umask, ASSOCV);
   (*fu)->lock_unit = 0;
   return (0);
}


int
e_wdue (void)
{
   return( e_wdue_mp( &f77curunit ) );
}


int
e_wdue_mp (unit **fu)
{
   int             n;
   unit *ftnunit = *fu;

   if (ftnunit->uassocv)
      inc_var ((ftnintu *)ftnunit->uassocv, ftnunit->umask, ASSOCV);

   if (ftnunit->url == 1 || ftnunit->f77recpos == ftnunit->url) {
      if (ftnunit->ushared)
	 _fio_du_flush ((int) ftnunit->ufd);
      ftnunit->lock_unit = 0;
      return (0);
   }
   n = ftnunit->url - ftnunit->f77recpos;
   if (n > ftnunit->f77fio_size)
      check_buflen (ftnunit, n);

   memset (ftnunit->f77fio_buf, 0, n);	/* fill remaining of record with 0 */
   if (ftnunit->url != 1) {
     if (-1 == (_fio_du_write (ftnunit, ftnunit->f77fio_buf, n,
	       ((ftnunit->uirec - 1) * ftnunit->url) + ftnunit->f77recpos,
			     (int) ftnunit->ufd)))
        errret(ftnunit->f77errlist.cierr, errno, "system write error");
   } else {
     if (-1 == (_fio_du_write (ftnunit, ftnunit->f77fio_buf, n,
	       ftnunit->uirec, (int) ftnunit->ufd)))
         errret(ftnunit->f77errlist.cierr, errno, "system write error");
   }

   if (ftnunit->ushared)
      _fio_du_flush ((int) ftnunit->ufd);

   ftnunit->lock_unit = 0;
   return (0);
}

#pragma weak e_rdue64 = e_rdue
#pragma weak e_rdue64_mp = e_rdue_mp
#pragma weak e_wdue64 = e_wdue
#pragma weak e_wdue64_mp = e_wdue_mp

