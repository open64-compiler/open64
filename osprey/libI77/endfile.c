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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/endfile.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/* sjc  #1951	10Dec87		Use ftruncate rather than fork/exec of  */
/* 				/bin/cp                                 */

#include <cmplrs/fio.h>
#include <mutex.h>
#include <string.h>
#include "endfile.h"

#include "close.h"
#include "err.h"
#include "fmt.h"
#include "iomode.h"
#include "util.h"

#ifdef _BSD
#include <sys/types.h>
#include <sys/file.h>
#endif				/* _BSD */
#if defined(_SYSV) || defined(_SYSTYPE_SVR4)
#include <sys/fcntl.h>
#endif

#include "vmsflags.h"
static int one = 1;
static char     ctlz = '\032';

/* fwd decl. */
static int f_end_com (alist *a, int lock);

int
f_end (alist *a)
{
    return( f_end_com( a, 0 ) );
}

int
f_end_mp (alist *a)
{
    return( f_end_com( a, 1 ) );
}

static int
f_end_com (alist *a, int lock)
{
   unit           *b, *ftnunit;
   int		   n;

   if ((ftnunit = b = map_luno (a->aunit)) == NULL)
      err (a->aerr, 114, "endfile");
   while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
       ;

   if (b->uacc == KEYED || b->uacc == DIRECT)
      errret(a->aerr, 169, "endfile");

   if (b->uconn <= 0)
      goto end;
   b->uend = 1;
   if (b->useek == 0)
      goto end;
   if (f77vms_flag_[VMS_EF]) {	/* write an endfile record */
      if (b->uwrt != WR_READY && f77nowwriting (ftnunit))
         errret(a->aerr, 160, "endfile");
      if (b->ufmt == 1) {	/* ASCII formatted file */
#ifdef I90
	 /* If in Fortran-90 nonadvancing mode, write endfile record (\n only). */
	 if (ftnunit->f90sw == 1 && ftnunit->f90nadv == 1 ) {
	    putc ('\n', ftnunit->ufd);
	    ftnunit->f90nadv = 0;
	 }
#endif
	 putc ('\032', b->ufd);
	 putc ('\n', b->ufd);
      } else {			/* unformatted */
	 (void) fwrite (&one, sizeof (int), 1, b->ufd);
	 (void) fwrite (&ctlz, 1, 1, b->ufd);
	 if (fwrite (&one, sizeof (int), 1, b->ufd) != 1)
	    errret(a->aerr, errno, "system write error");
      }
   } else {
      if (b->uwrt != WR_READY && f77nowwriting (ftnunit))
         errret(a->aerr, 160, "endfile");

#ifdef I90
      /* If in Fortran-90 nonadvancing mode, write endfile record (\n only). */
      if (ftnunit->f90sw == 1 && ftnunit->f90nadv == 1 ) {
	  putc ('\n', b->ufd);
	  ftnunit->f90nadv = 0;
      }
#endif
   }
   
   n = t_runc (b, a->aerr);
   if (lock) ftnunit->lock_unit = 0;
   return (n);
end:
   if (lock) ftnunit->lock_unit = 0;
   return(0);
}

#pragma weak f_end64 = f_end
#pragma weak f_end64_mp = f_end_mp
