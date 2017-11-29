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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/backspace.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/

#include <cmplrs/fio.h>
#include <mutex.h>
#include "fmt.h"
#include "iomode.h"
#include "backspace.h"
#include "bcompat.h"
#include "util.h"
#include "err.h"
#include "close.h"
#include "sue.h"

/* fwd reference */
static ftnint
__f77_f_back_com (alist *a, int lock) ;


ftnint
__f77_f_back (alist *a) 
{
   return( __f77_f_back_com (a, 0 ) );
}

ftnint
__f77_f_back64_mp (alist *a) 
{
    return( __f77_f_back_com (a, 1 ) );
}

static ftnint
__f77_f_back_com (alist *a, int lock) 

{
   unit           *ftnunit;
   int             n, i;
   ftnll            x, y;
   char            buf[512];

   if ((ftnunit = find_luno (a->aunit)) == NULL)
      err(a->aerr, 114, "backspace");
   while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   if (ftnunit->uacc == APPEND || ftnunit->uacc == KEYED)
      errret(a->aerr, 165, "backspace");
   if (ftnunit->useek == 0 || ftnunit->url == 1)
      errret(a->aerr, 106, "backspace");
   if (ftnunit->uend == 1) {
      ftnunit->uend = 0;
      ftnunit->lock_unit = 0;
      return (0);
   }
   if (ftnunit->uwrt & WR_OP) {
#ifdef I90
      /* If in Fortran-90 nonadvancing mode, write endfile record (\n only). */
      if (ftnunit->f90sw == 1 && ftnunit->f90nadv == 1 ) {
	  putc ('\n', ftnunit->ufd);
	  ftnunit->f90nadv = 0;
      }
#endif
      /* Just completed a write operation, a backspace would force
      the truncation of the file at the current position.
      */
      (void) t_runc (ftnunit, a->aerr);
      /* make sure it gets switched back to reading mode so the
      file won't get truncated again if it gets backspace/rewind again
      */
      if (f77nowreading(ftnunit))
	 errret(a->aerr, 106, "backspace");
   }
   /* Backspace a direct unformatted file. */

   if ((ftnunit->uacc == DIRECT) && (ftnunit->ufmt == 0)) {
      if (ftnunit->uirec != 0)
	 ftnunit->uirec--;
      ftnunit->lock_unit = 0;
      return (0);
   }
   if (ftnunit->ufmt != 1) {
      if (ftnunit->uerror)
	 unf_position (ftnunit->ufd, ftnunit);
      if (fseek (ftnunit->ufd, -(long) sizeof (int), 1)) {
	  fseek(ftnunit->ufd, 0L, 0);
          ftnunit->lock_unit = 0;
	  return(0);
      }
      /* NEED TO CHANGE HERE DLAI */
      (void) fread ((char *) &n, sizeof (int), 1, ftnunit->ufd);
      (void) fseek (ftnunit->ufd, (long) (-n - 2 * sizeof (int)), 1);
      ftnunit->lock_unit = 0;
      return (0);
   }

   y = x = FTELL (ftnunit->ufd) - 1;	/* skip the last CR */

   /* If already at the beginning of file, ignore the backspace */

   if (x < 0) {
      ftnunit->lock_unit = 0;
      return (0);
   }

#ifdef I90
   /* Make sure these variables are zeroed out to allow record to be reread. */
   ftnunit->f77recpos = 0;
   ftnunit->f77recend = 0;
#endif

   for (;;) {
      if (x < sizeof (buf))
	 x = 0;
      else
	 x -= sizeof (buf);
      (void) FSEEK (ftnunit->ufd, x, 0);
      /* n should be ll for 64 bit records */
      n = (int) fread (buf, 1, (int) (y - x), ftnunit->ufd);
      for (i = n - 1; i >= 0; i--) {
	 if (buf[i] != '\n')
	    continue;
	 (void) fseek (ftnunit->ufd, (long) (i + 1 - n), 1);
         ftnunit->lock_unit = 0;
	 return (0);
      }
      if (x == 0) {
	 (void) fseek (ftnunit->ufd, 0L, 0);
         ftnunit->lock_unit = 0;
	 return (0);
      } else if (n <= 0)
	 errret (a->aerr, (EOF), "backspace")
	    (void) FSEEK (ftnunit->ufd, x, 0);
      y = x;
   }
}

#pragma weak __f77_f_back64 = __f77_f_back
#pragma weak f_back64 = __f77_f_back
#pragma weak f_back64_mp = __f77_f_back
