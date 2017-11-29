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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/util.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <mutex.h>
#include <cmplrs/fio.h>
#include <sys/prctl.h>
#undef uint
#include "iomode.h"
#include "err.h"
#include "util.h"
#include "bcompat.h"

static unsigned long io_lock;

char *_I90_uppercase( char *name, char *uname )
{
    int i;

    for (i = 0; name[i] && i < PATH_MAX; i++)
	uname[i] = (char) ((name[i] >= 'a' && name[i] <= 'z') ? name[i]+'A'-'a' : name[i]);
    if (i < PATH_MAX-1) {
       uname[i] = '\0';
       return(uname);
    } else return((char *) 0);
}

int
f77inode (char *a, ino_t *inod)
{
   struct stat     x;
   char		uname[PATH_MAX];

   if (a[3] == '$' && _I90_uppercase(a, uname) &&
      (!strcmp (uname, "SYS$INPUT") || !strcmp (uname, "SYS$OUTPUT") ||
       !strcmp (uname, "SYS$ERROR")))
      return (0);
   /* bug fix 12763 and 12983, add new parameter */
#ifdef SHLIB
   if ((*_libI77_stat) (a, &x) < 0)
      return (-1);
#else
   if (stat (a, &x) < 0)
      return (-1);
#endif
   *inod = x.st_ino;
   return (1);
}


/* Given a logical unit integer, return a pointer to the unit-table entry
  (possibly newly created), or return NULL for error */

unit *
map_luno(ftnint luno)
{
   register int    i, space_available;
   register unit  *a;
   static unit 	*f77curunit = 0;
   unit	*ftnunit;
   static unsigned long  expand_table_lock = 0;

   if (!f77init)
      f_init ();

   /* THe following region is critical in terms of performance.  A lock 
   should not be here at all else it would effect every single READ/WRITE
   operation */
   if (f77curunit) {
       ftnunit = f77curunit;
       if (ftnunit->luno == luno && ftnunit->uconn) {
          return (ftnunit);	/* has just been mapped */
       }
   }

   /* Find unit if it has been opened/connected */
   for (i = 0, a = f77units; i < space_assigned; i++, a++) {
      if (a->luno == luno && a->uconn) {
	 return (a);
      }
   }

   /* The unit has not been opened.  The following regionis a critical
   region and a lock must be obtained before entering it
   */
   while (test_and_set( &expand_table_lock, 1L ))
      ;

   /* Look for a slot which has been disconnected.  We try to use every 
   single available slot upfront so that the value of space_assigned
   is small and speed up the search in the loop above
   */
   for (i = 0, a = f77units; i < space_assigned; i++, a++)
      if (a->uconn == 0) {
	 space_available = i;
	 goto unused_slot;
      }
   if (space_assigned >= mxunit) {
      int             old_mxunit = mxunit;
      int 		ii, nthreads;

      /* Open new file in case there is no disconnected slot available */
      /* Enlarge the table */
      /* This is done only when the table needs to be enlarged, i.e.
      very rarely, so we can afford to have test_and_set() called for
      single process I/O here with almost no performance penalty
      */
      nthreads = prctl( PR_GETNSHARE );
      if (nthreads > 1) {
      /* cannot do reallow while multiple threads are running as some
      of the thread might be using pointers to the old f77units
      */
	  fprintf( stderr, "Exceeding %d opened files while running in MP I/O mode, please set the environment FORTRAN_OPENED_UNITS to a higher number then rerun the program\n", mxunit );
	 abort();
      }
      i = mxunit;
      f77curunit = f77units = (unit *) realloc (f77units, (mxunit <<= 1) *
				      (sizeof (unit)));
      memset( &f77units[i], 0, (size_t) i*(sizeof (unit)) );
      if (f77units == 0) {
	 expand_table_lock = 0;
	 return (NULL);
      } 
/* Resetting Internal_File value to the new realloc'ed table */
      for (ii = 0, a = f77units; ii < old_mxunit; ii++, a++)
	 if (a->luno == -1) {
            Internal_File = a;
            break;
	 }
      space_assigned = old_mxunit;
   }
   space_available = space_assigned++;
unused_slot:
/* fprintf(stderr, "Assigning slot %d to unit %d\n", space_available, luno ); */
   a = f77units + space_available;
   /* Clear unit control block everytime a new one is assigned to
    * avoid having garbage in it. */
   memset (a, '\0', sizeof (unit));
   a->luno = luno;
   a->uconn = -1;	/* make sure no other threads take this slot
			without really marking it as connected */
   a->ualias = a;
   f77curunit = a;
   expand_table_lock = 0;	/* unlock */
   return (a);
}



unit *
find_luno(ftnint luno)
{
   register int    i;
   register unit  *a;
   static unit 	*f77curunit = 0;
   unit	*ftnunit;

   if (!f77init)
      f_init ();
   if (f77curunit) {
       ftnunit = f77curunit;
       if (ftnunit->luno == luno && ftnunit->uconn > 0)
          return (ftnunit);	/* has already been mapped */
   }
   for (i = 0, a = f77units; i < space_assigned; i++, a++)	
      /* Find unit or empty one */
      if (a->luno == luno)
	 return (a);
   return (NULL);
}

/*
	with the exception for pipes and tty files, this is the general
	plan for the file mode strategy.
	File is opened as:
	    - read-only if it exists
	    - write-only if it doesn't exist
	The file can be reopened as:
	    - read-only (uwrt==0) or write-only (uwrt==1) if the I/O mode 
	is switched from read to write, or vice versa, at the beginning of 
	the file since many benchmarks have a sequence  of WRITEs followed 
	by REWIND and a sequence of READs.  Keeping the file in read-only
	and write-only mode makes I/O faster in these scenarios.
	   - read-write (uwrt==2 or 3) if the I/O mode is switched from 
	read to write, or vice versa, in the middle of the file.  This
	is usually for files which are continually read and updated.
	In this mode the file is not reopened anymore for subsequent
	I/O mode change and therefore is faster for transactional-kind
	I/O operation where I/O mode changes are frequent.  For example,
	for United Airlines operations where they do a lot of READ, REWIND,
	and WRITE together.

	Note that since the file has to be truncated in a REWIND/BACKSPACE
	statement if the last I/O operation was a WRITE, the I/O mode
	has to be reset to READ after REWIND/BACKSPACE is executed so
	that the file won't be truncated again if REWIND/BACKSPACE is
	executed again.
*/

int
f77nowreading (unit *x)
{
   XINT64 loc;
   FILE   *nfd;

   if (x->uacc == KEYED) goto read_mode;
   if (!(x->uwrt & WR_OP))
       return(0);
   if (x->uwrt == WR_OP) {
       /* write-only file */
      loc = FTELL (x->ufd);
      if (!loc) {
         /* obtain exclusive lock for special I/O operation */
         while (test_and_set( &io_lock, 1L ))
           ;
         nfd = freopen (x->ufnm, "r", x->ufd);
         io_lock = 0;
         if (!nfd)
	     return(1);
      } else {
         /* obtain exclusive lock for special I/O operation */
         while (test_and_set( &io_lock, 1L ))
           ;
         nfd = freopen (x->ufnm, "r+", x->ufd);
         io_lock = 0;
         if (!nfd)
            return (1);
	 x->uwrt = WR_READY;
	 FSEEK (x->ufd, loc, SEEK_SET);
      }
   }
   else
      fseek (x->ufd, 0L, SEEK_CUR);	/* dummy seek to reset FILE structure */
read_mode:
   x->uwrt &= ~WR_OP;
   return (0);
}

int
f77nowwriting (unit *x)
{
   XINT64            loc;
   FILE		    *nfd;

#ifdef I90
   if (x->ureadonly || x->uaction == READONLY) {
#else
   if (x->ureadonly) {
#endif
      x->uwrt |= WR_OP;
      return (1);
   }
   if (x->uacc == KEYED) goto write_mode;
   if (x->uwrt & WR_OP)
      return(0);
   if ((x->uwrt & RW_FILE) == 0) {
      loc = FTELL (x->ufd);
      if (!loc && x->uacc == SEQUENTIAL) {
         /* obtain exclusive lock for special I/O operation */
         while (test_and_set( &io_lock, 1L ))
           ;
         nfd = freopen (x->ufnm, "w", x->ufd);
         io_lock = 0;
         if (!nfd)
	     return(1);
      } else {
         /* obtain exclusive lock for special I/O operation */
         while (test_and_set( &io_lock, 1L ))
           ;
         nfd = freopen (x->ufnm, "r+", x->ufd);
         io_lock = 0;
         if (nfd)
	    x->uwrt = WR_READY;
         else {
            /* obtain exclusive lock for special I/O operation */
            while (test_and_set( &io_lock, 1L ))
              ;
            nfd = freopen (x->ufnm, "w+", x->ufd);
            io_lock = 0;
            if (!nfd)
             return (1);
         }
#ifdef I90
      /* When doing a write after a nonadvancing read the file pointer needs  *
       * to be changed to the correct position.                               */
	 if ( x->f90sw == 1 && x->f90nadv == 1 ) {
	     loc = loc - (long)(x->f77recend + 1) + (long)x->f77recpos;
	 }
#endif
	 FSEEK (x->ufd, loc, SEEK_SET);
      }
   } else {
      fseek (x->ufd, 0L, SEEK_CUR);	/* dummy seek to reset FILE structure */
   }

write_mode:
   x->uwrt |= WR_OP;
   return (0);
}
