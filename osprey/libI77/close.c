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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/close.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/

/* who	ref.		date		description			*/
/* AGC	#587	28Jan87		return error on closing non-open unit   */
/* SJC	#1668	12Oct87		undo bug fix 587 (violates ANSI std)    */
/* sjc  #1963	11Dec87		Dynamically allocate unit table		*/
/* bcn		22Nov88		Add mp_cleanup to f_exit		*/

#include <sys/types.h>

#include <string.h>
#include <mutex.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <cmplrs/fio.h>
#include "fmt.h"
#include "iomode.h"
#include "idxio.h"
#include "close.h"
#include "err.h"
#include "fio_direct_io.h"
#include "util.h"
#include "bcompat.h"
#include "cmplrs/f_errno.h"

extern void __checktraps(void);


/* forward declaration */
static ftnint
f_clos_com (cllist *a, int lock) ;

ftnint
f_clos (cllist *a) 
{
    return( f_clos_com( a, 0 ) );
}

ftnint
f_clos64_mp (cllist *a) 
{
    return( f_clos_com( a, 1 ) );
}



static ftnint
f_clos_com (cllist *a, int lock) 
{
   unit           *ftnunit;
   char           *cbuf, c, buf[256], tbuf[12];
   int		   n, istat;

   if ((ftnunit = find_luno (a->cunit)) == NULL) {
      return 0;
   }
   while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   if (ftnunit->uconn <= 0) {
       /* could be disconnected by other threads */
       ftnunit->uconn = 0;
       ftnunit->lock_unit = 0;
       return(0);
   }
   ftnunit->uend = 0;
   if (cbuf = a->csta)
      switch (up_low (*cbuf++)) {
      case 'd':
	 ftnunit->udisp = DELETE;
	 break;
      case 'p':
	 ftnunit->udisp = PRINT;
	 goto checkdelete;

/*
 * Fix BN 7869.
 * This is very sloppy code for checking the specifiers to close. Currently
 * both DISP and STATUS cannot be used as specifiers to close. This is a kludge
 * that allows SAVE to be passed and treats it like KEEP instead of SUBMIT.
 * ---ravi---   10/30/91
 *		case 's':  ftnunit->udisp = SUBMIT;
 */
      case 's':
	 ftnunit->udisp = up_low (*cbuf) == 'a' ? KEEP : SUBMIT;
   checkdelete:
	 while (c = (*cbuf++))
	    if ((c == '/') && (c = (*cbuf)) && (up_low (c) == 'd'))
	       ftnunit->udisp |= DELETE;
	 break;

     case 'k':
	 if (ftnunit->uscrtch == 1)
	   err(a->cerr, F_ERKEEPSCRATCH, "close");
     default:
	 ftnunit->udisp = KEEP;
      }
   if (ftnunit->uscrtch == 1)
      ftnunit->udisp |= DELETE;
   if (ftnunit->uacc == KEYED) {
      n = idxclose(ftnunit, a->cerr);
      ftnunit->lock_unit = 0;
      return (n);
   }

#ifdef I90
   /* If in Fortran-90 nonadvancing mode, write endfile record (\n only). */
   if ( (ftnunit->f90sw == 1) && (ftnunit->f90nadv == 1) && (ftnunit->uwrt & WR_OP) ) {
	putc ('\n', ftnunit->ufd);
	ftnunit->f90nadv = 0;
   }
#endif

   if (ftnunit->ucc == CC_FORTRAN && ftnunit->ucchar)
      putc (ftnunit->ucchar, ftnunit->ufd);

   if (ftnunit->ufd == stdin || ftnunit->ufd == stdout || ftnunit->ufd == stderr) {
     /* 
      * Don't close stdin, stdout, and stderr otherwise other files
      * can be opened using those pointers and caused a lot of confusion
      */
      fflush(ftnunit->ufd);
      goto cont;
   }
   if (ftnunit->uwrt & WR_OP)
      (void) t_runc (ftnunit, a->cerr);

   /* Close the file. */

   if ((ftnunit->uacc == DIRECT) && (ftnunit->ufmt == 0)) {	/* direct unformatted */
      while (lock && test_and_set( &io_lock, 1L ))
         ;
      if (ftnunit->uistty) {
	 _fio_du_close ((int) ftnunit->ufd);	/* no error */
      } else if (((int)ftnunit->ufd) !=  _fio_du_close ((int) ftnunit->ufd)) {
	 io_lock = 0;
         if (lock) ftnunit->lock_unit = 0;
	 err (a->cerr, errno, "close");
      }
      io_lock = 0;
   } else {
      if (ftnunit->uistty) {		/* have to call isatty() first to get
				 * correct result */
	    /* obtain exclusive lock for special I/O operation */
	    while (lock && test_and_set( &io_lock, 1L ))
	       ;
	    istat = fclose (ftnunit->ufd);
	    io_lock = 0;
      } else {
	 /* obtain exclusive lock for special I/O operation */
	 while (lock && test_and_set( &io_lock, 1L ))
	    ;
	 istat = fclose (ftnunit->ufd);
	 io_lock = 0;
	 if (istat) {
            if (lock) ftnunit->lock_unit = 0;
	    err (a->cerr, errno, "close");
	 }
      }
   }

   if (ftnunit->ufnm) {
      if (ftnunit->udisp & SUBMIT) {
	 (void) strcpy (tbuf, "tmp.FXXXXXX");
	 (void) mktemp (tbuf);
	 sprintf (buf, "cp %s %s", ftnunit->ufnm, tbuf);
	 system (buf);
	 sprintf (buf, "( chmod +x %s; %s; rm %s ) &",
		  tbuf, tbuf, tbuf);
	 system (buf);
      } else if (ftnunit->udisp & PRINT) {
	 sprintf (buf, "lpr %s", ftnunit->ufnm);
	 system (buf);
      }
      if (ftnunit->udisp & DELETE)
	 (void) unlink (ftnunit->ufnm);	/* SYSDEP */
      free (ftnunit->ufnm);
      ftnunit->ufnm = NULL;
   }
cont:
   /*
     The following fixes bug #231656.  The pointers involved are initialized
     to zero (both when originally allocated in f_init() and when reallocated
     in map_luno()).  So, if non-zero, the buffers must have been allocated,
     and we should free them.
   */
   if (ftnunit->f77syl) {
    free(ftnunit->f77syl);
    ftnunit->f77syl = NULL;
   }
   if (ftnunit->f77fio_buf) {
    free(ftnunit->f77fio_buf);
    ftnunit->f77fio_buf = NULL;
    ftnunit->f77fio_size = 0;
   }
   if (ftnunit->ukeys) {
    free(ftnunit->ukeys);
    ftnunit->ukeys = NULL;
   }
   ftnunit->ufd = NULL;
   ftnunit->uconn = 0;
   ftnunit->luno = 0;
   if (lock) ftnunit->lock_unit = 0;
   /* added in MIPS version 2.20 fix bug 6084 BN-8077. Undo 6084 fix */
   return (0);
}

#ifdef BUG_6084
/* fix bug 6084 
**ftnint
**f_clos2 (cllist *a) {
**   unit           *ftnunit;
**   char           *cbuf, c, buf[256], tbuf[12];
**   int		   n, istat;
**
**   if ((ftnunit = map_luno (a->cunit)) == NULL) {
**      return 0;
**   }
**   while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
**       ;
**   if (ftnunit->uconn <= 0) {
**      return 0;
**   }
**   ftnunit->uend = 0;
**   if (cbuf = a->csta)
**      switch (up_low (*cbuf++)) {
**      case 'd':
**	 ftnunit->udisp = DELETE;
**	 break;
**
**      case 'p':
**	 ftnunit->udisp = PRINT;
**	 goto checkdelete;
**
**      case 's':
**	 ftnunit->udisp = SUBMIT;
**   checkdelete:
**	 while (c = (*cbuf++))
**	    if ((c == '/') && (c = (*cbuf)) && (up_low (c) == 'd'))
**	       ftnunit->udisp |= DELETE;
**	 break;
**      default:
**	 ftnunit->udisp = KEEP;
**      }
**   if (ftnunit->uscrtch == 1)
**      ftnunit->udisp |= DELETE;
**   if (ftnunit->uacc == KEYED) {
**      n = idxclose(ftnunit, a->cerr);
**      return (n);
**   }
**   if (ftnunit->ucc == CC_FORTRAN && ftnunit->ucchar)
**      putc (ftnunit->ucchar, ftnunit->ufd);
**   if (ftnunit->uwrt)
**      (void) t_runc (ftnunit, a->cerr);
**
**
**   if ((ftnunit->ufd != stdin) && (ftnunit->ufd != stdout) && (ftnunit->ufd != stderr)) {
**      if (ftnunit->uacc == DIRECT) {
**	 if (ftnunit->uistty) {
**	    _fio_du_close ((int) ftnunit->ufd);
**	 } else if (ftnunit->ufd != (FILE *) _fio_du_close ((int) ftnunit->ufd)) {
**	    err (a->cerr, errno, "close");
**	 }
**      } else {
**	 if (ftnunit->uistty) {	
**	    fclose (ftnunit->ufd);
**	 } else if (fclose (ftnunit->ufd) != 0) {
**	    err (a->cerr, errno, "close");
**	 }
**      }
**   }
**   if (ftnunit->ufnm) {
**      if (ftnunit->udisp & SUBMIT) {
**	 (void) strcpy (tbuf, "tmp.FXXXXXX");
**	 (void) mktemp (tbuf);
**	 sprintf (buf, "cp %s %s", ftnunit->ufnm, tbuf);
**	 system (buf);
**	 sprintf (buf, "( chmod +x %s; %s; rm %s ) &",
**		  tbuf, tbuf, tbuf);
**	 system (buf);
**      } else if (ftnunit->udisp & PRINT) {
**	 sprintf (buf, "lpr %s", ftnunit->ufnm);
**	 system (buf);
**      }
**      if (ftnunit->udisp & DELETE)
**	 (void) unlink (ftnunit->ufnm);	
**      free (ftnunit->ufnm);
**   }
**   if (ftnunit->f77syl) {
**    free(ftnunit->f77syl);
**    ftnunit->f77syl = NULL;
**   }
**   if (ftnunit->f77fio_buf) {
**    free(ftnunit->f77fio_buf);
**    ftnunit->f77fio_buf = NULL;
**    ftnunit->f77fio_size = 0;
**   }
**   if (ftnunit->ukeys) {
**    free(ftnunit->ukeys);
**    ftnunit->ukeys = NULL;
**   }
**   ftnunit->ufnm = NULL;
**   ftnunit->ufd = NULL;
**   ftnunit->uconn = NULL;
**   ftnunit->luno = 0;
**   return (0);
**}
*/
#endif

void
f_exit ()
{
   int             i;
   cllist          xx;

   /* trap handler : print the count upon exit of the program, if
    * enabled */
   __checktraps ();
   xx.cerr = 1;
   xx.csta = NULL;
   for (i = 0; i < mxunit; i++) {	/* sjc #1963 11Dec 87 */
      xx.cunit = f77units[i].luno;
      if (xx.cunit > 0)		/* don't close the artificial LU created
				   for internal file I/O */
	/* Since other threads could be exitting and closing the files
	** at the same time, make sure they don't step on each others
	*/
        (void) f_clos64_mp (&xx);
   }
}


/* Truncate a file using ftruncate rather than fork/exec of /bin/cp.
  #1951 sjc 10Dec87 */
ftnint
t_runc (unit *ftnunit, flag xerr) {
   extern void     exit();	/* DAG -- added */

#ifdef _BSD
   off_t           ftell();

#endif				/* _BSD */
#if defined  (_SYSV) || defined(_SYSTYPE_SVR4)
#ifndef sgi
   long            ftell();

#endif
#endif				/* SYSV */

   ftnll            loc, len;

   if (ftnunit->uacc == DIRECT)
      return (0);		/* don't truncate direct files */

   if (ftnunit->useek == 0 || ftnunit->ufnm == NULL)
      return (0);

   /* do not truncate "/dev/null" */
   if (strncmp("/dev/null", ftnunit->ufnm, 9) == 0)
      return (0);

   loc = FTELL (ftnunit->ufd);
   (void) fseek (ftnunit->ufd, 0L, 2);
   len = FTELL (ftnunit->ufd);
   /* if ftell() fails then ignore the pipe/device */
   if (loc == len || loc < 0)
      return (0);
   if (TRUNCATE (ftnunit->ufnm, loc))
      err (xerr, 111, "endfile");
   (void) FSEEK (ftnunit->ufd, loc, 0);
   return 0;
}

#pragma weak f_clos64 = f_clos
