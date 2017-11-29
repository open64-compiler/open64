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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/rewind.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
#include <cmplrs/fio.h>
#include <mutex.h>
#include "fmt.h"
#include "iomode.h"
#include "vmsflags.h"
#include "err.h"
#include "close.h"
#include "unistd.h"
#include "util.h"
#include "bcompat.h"

#define ASSOCV	12

#define get_alist64(p_dst, p_src) ((p_dst)->aerr = (p_src)->aerr, (p_dst)->aunit = (p_src)->aunit)

static int
f_rew_com (alist *a, int lock)
{
   unit           *ftnunit;
  
   if ((ftnunit = find_luno (a->aunit)) == NULL)
      return(0);
   while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
       ;

   if (ftnunit->uacc == KEYED)
      errret(a->aerr, 164, "rewind");

   if (ftnunit->uconn <= 0) {
      ftnunit->lock_unit = 0;
      return (0);
   }

   if (!ftnunit->useek && !ftnunit->uistty)
      errret(a->aerr, 106, "rewind");
   ftnunit->uend = 0;

   /* Need to reset the associate variable to 1 if exists */
   if (ftnunit->uassocv)
      set_var (ftnunit->uassocv, ftnunit->umask, ASSOCV, (ftnll) 1);

   /* Rewind of a direct unformatted file. */

   if ((ftnunit->uacc == DIRECT) && (ftnunit->ufmt == 0)) {
      if (-1 == lseek ((int) ftnunit->ufd, 0, SEEK_SET)) {
	 errret(a->aerr, 106, "rewind");
      }
      /* need to change the internal buffer position in fio_direct_io as well */
      _fio_set_seek((int) ftnunit->ufd,  (ftnll) 0, 0);
      ftnunit->uirec = 0;
      ftnunit->lock_unit = 0;
      return (1);
   }

#ifdef I90
   /* Make sure these variables are zeroed out to allow record to be reread. */
   ftnunit->f77recpos = 0;
   ftnunit->f77recend = 0;
#endif

   if (f77vms_flag_[VMS_EF]) {	/* rewind to the last endfile record
				 * or beginning of file */
      char            buf[513];
      XINT64             y, x;
      int	 i, n;
      char            ch;

      /*  If last operation was a WRITE, truncate the file and then make
      sure that the file mode is switched to READ so the the next 
      REWIND/BACKSPACE won't truncate the file again
      */
      if (ftnunit->uwrt & WR_OP) {
#ifdef I90
	 /* If in Fortran-90 nonadvancing mode, write endfile record (\n only). */
	 if (ftnunit->f90sw == 1 && ftnunit->f90nadv == 1 ) {
	     putc ('\n', ftnunit->ufd);
	     ftnunit->f90nadv = 0;
	 }
#endif
	 (void) t_runc (ftnunit, a->aerr);
	 /* If the file is in write-only mode make sure that it is readable */
	 if (f77nowreading(ftnunit))
	    errret(a->aerr, 106, "rewind");
      }
      if (ftnunit->ufmt != 1) {
	 if (ftell (ftnunit->ufd) == 0) {
            ftnunit->lock_unit = 0;
	    return (0);		/* already at beginning of file */
	 }
	 if (fseek (ftnunit->ufd, (long) (-sizeof (int)), 1) < 0)
	    errret(a->aerr, 106, "rewind");
	 for (i = 0;; i++) {
	    (void) fread ((char *) &n, sizeof (int), 1, ftnunit->ufd);
	    if (n != 1 || i == 0) {
	       if (fseek (ftnunit->ufd, (long) (-n - 3 * sizeof (int)), 1)) {
		  rewind (ftnunit->ufd);
                  ftnunit->lock_unit = 0;
		  return (0);
	       }
	    } else {
	       if (fseek (ftnunit->ufd, -(sizeof (int) + 1), 1)) {
		  rewind (ftnunit->ufd);
                  ftnunit->lock_unit = 0;
		  return (0);
	       }
	       (void) fread ((char *) &ch, 1, 1, ftnunit->ufd);
	       if (ch == '\032') {
		  fseek (ftnunit->ufd, sizeof (int), 1);
                  ftnunit->lock_unit = 0;
		  return (0);
	       }
	       fseek (ftnunit->ufd, -(2 * sizeof (int) + 1), 1);
	    }
	 }
      }
      y = x = FTELL (ftnunit->ufd) - 2;	/* skip the last endfile
					 * record */
      if (y < 0) {
	  (void) fseek(ftnunit->ufd, 0L, 0);
          ftnunit->lock_unit = 0;
	  return(0);
      }
      ch = '\0';
      for (;;) {
	 if (x < sizeof (buf) - 1)
	    x = 0;
	 else
	    x -= sizeof (buf) - 1;
	 (void) FSEEK (ftnunit->ufd, x, 0);
	 n = (int) fread (buf, 1, (int) (y - x), ftnunit->ufd);
	 buf[n] = ch;
	 for (i = n - 1; i >= 1; i--) {
	    if (buf[i] != '\032' || buf[i + 1] != '\n')
	       continue;
	    (void) fseek (ftnunit->ufd, (long) (i + 2 - n), 1);
            ftnunit->lock_unit = 0;
	    return (0);
	 }
	 if (x == 0) {
	    (void) fseek (ftnunit->ufd, 0L, 0);
            ftnunit->lock_unit = 0;
	    return (0);
	 }
	 y = x;
	 ch = buf[0];
      }
   }
      /*  If last operation was a WRITE, truncate the file and then make
      sure that the file mode is switched to READ so the the next 
      REWIND/BACKSPACE won't truncate the file again
      */
   if (ftnunit->uwrt & WR_OP) {
#ifdef I90
      /* If in Fortran-90 nonadvancing mode, write endfile record (\n only). */
      if (ftnunit->f90sw == 1 && ftnunit->f90nadv == 1 ) {
	  putc ('\n', ftnunit->ufd);
	  ftnunit->f90nadv = 0;
      }
#endif
      (void) t_runc (ftnunit, a->aerr);
      /* If the file is in write-only mode make sure that it is readable */
      if (f77nowreading(ftnunit))
	 errret(a->aerr, 106, "backspace");
   }
   rewind (ftnunit->ufd);
   ftnunit->lock_unit = 0;
   return (0);
}

int
f_rew (alist *a)
{
    return( f_rew_com( a, 0 ) );
}

int
f_rew64_mp (alist *a)
{
    return( f_rew_com( a, 1 ) );
}

#pragma weak f_rew64 = f_rew
