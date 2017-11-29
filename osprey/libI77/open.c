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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/open.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/* AGC	#537	10Jan87		return error if "new" file exist        */
/* AGC	#538	10Jan87		rewind files upon open                  */
/*                    		assume files are open for read only     */
/* AGC	#950    8Apr87		error produced for invalid format	*/
/* sjc	#1649   5Oct87		Made f77vms_flag_ an array 		*/
/* sjc  #1827	24Nov87		Dynamically allocate f_open "buf"	*/
/* sjc  #1963	11Dec87		Dynamically allocate unit table		*/
/* sjc  #2313	26Feb88		Like 1827, but for a different buffer   */

#include <sys/types.h>
#include <mutex.h>
#include <sys/stat.h>
#include <cmplrs/fio.h>
#include "err.h"
#include "open.h"
#include "fio_direct_io.h"
#include "util.h"
#include "close.h"
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <cmplrs/f_errno.h>

#ifndef ultrix
/* Digital doesn't support isam (Bozarth 900418) */
#include "isam.h"
#endif

#define ASSOCV	12

#include "iomode.h"
#include "vmsflags.h"
#include "bcompat.h"
#include "idxio.h"
#include "uio.h"

unsigned long	io_lock;

static int
f_open_com (olist64 *a, ftnint *mask, char **mode_, char **buf_, unit **fu)
{
   unit           *b;
   ino_t           inod;
   int             n, org;
   char           *mode = "r";
   char           *abuf, c, *cbuf, errstr[80];
   char    	  buf[PATH_MAX];		/* temp buffer */
   char    	  ubuf[PATH_MAX];		/* temp buffer */
   unsigned int   need;
   cllist          x;
   struct stat     sbuf;
   static char     seed[] = "aa";
   char		   *q = seed;
   char            ch;
   unit		  *dupunit;
   int 	  	   dupopen;
   int             istty = 0;	/* Flag to indicate whether file
				 * being opened is /dev/tty */

   /*
   extern	FILE *debugfile;
   */
   struct stat     stat_struct;
   unit           *ftnunit;

   /* bug fix 12787 : need to initialize to zero */

   /* sjc #1827: The cretin who coded this originally assumed that an
    * 80-byte temporary string would always be enough. We dynamically
    * allocate it to be 80 bytes plus whatever we can easily find out
    * about the length of the filename being passed to us. That may
    * not be enough (the string gets passed all over creation, so
    * it's hard to know) but it's better than before. Note that this
    * relies on f_open continuing not to be recursive. */

   if (a->ofnm)
      istty = !strncmp ("/dev/tty", a->ofnm, 8);
   need = a->odfnm ? a->odfnmlen : 0;
   need += a->ofnm ? a->ofnmlen : 0;
   need += 40;
   if ((*fu = ftnunit = b = map_luno (a->ounit)) == NULL)
      err(a->oerr, 101, "open");
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L )) {
      sginap(0);
   }
   /* obtain exclusive lock for special I/O operation, this should always
   be done after the lock onthe unit has been done to avoid deadlock */
   while (test_and_set( &io_lock, 1L ))
      sginap(0);
   * buf_ = buf;

/* Fix BN 9310 . If the the terminal is being opened do not test to see if this
 * file is already connected to a fortran unit since the terminal should be
 * able to be connected to various fortran units simultaneously
 * ---ravi---1/7/91
 */

 /* From the ANSI standard: to make this clear once and for all:
 ** 	If a unit is connnected to a file that exists, execution of an OPEN
 ** statement for that unit is permitted.   If the FILE= specifier is not
 ** included in the OPEN statement, the file to be connected to the unit is
 ** the same as the file to which the unit is connected.
 ** 	If the file to be connected to the unit does not exist, but is the
 ** same as the file to which the unit is preconnected, the properties
 ** specifies by the OPEN statement become a part of the connection.
 **	If the file to be connected to the unit is not the same as the
 ** file to which the unit is conencted, the effect is as if a CLOSE
 ** statement without a STATUS= specifier had been executed for the unit
 ** immediately to the execution of the OPEN statement.
 **	If the file to be connected to the unit is the same as the file
 ** to which the unit is connected, only the BLANK= specifier may have a
 ** value different from the one currently in effect.  The position of
 ** the file is unaffected.
 **	If a file is connected to a unit, execution of an OPEN statement
 ** on that file and a different unit is not permitted
 */

   if (!istty) {
      if (dupopen = f_duped (a, ftnunit, &dupunit))
         if (!a->oshared)
            return(dupopen);
   }
   else
       dupopen = 0;

   if (a->odfnm) {
      g_char (a->odfnm, a->odfnmlen, buf);
      abuf = &buf[strlen(buf)];
   } else
      abuf = buf;
   if (b->uconn > 0 && (!a->osta || up_low (*a->osta) != 's')) {
      if (a->ofnm == 0) {
same:if (a->oblnk != 0)
	 b->ublnk = up_low (*a->oblnk) == 'z' ? 1 : 0;
	 /* Ignore this open statement if it is not a preconnected unit
	 ** otherwise redefine the unit characteristics
	 */
	 if ((b->ufd == stdin || b->ufd == stdout || b->ufd == stderr)
	   && b->ufnm == NULL)
	   dupopen = 1;
	 else
           return (0);
      }
      if (a->ofnm) {
         g_char (a->ofnm, a->ofnmlen, abuf);
         if (b->uacc == KEYED)
            mkidxname (buf, buf);
         f77inode (buf, &inod);
         if ((inod == b->uinode) && inod)
            goto same;
         buf[a->ofnmlen] = '\0';
      }
      x.cunit = a->ounit;
      x.csta = 0;
      x.cerr = a->oerr;
/* fix bug 6084 */
   /* BN-8077 */
   /* Leave the stdin, stdout, stderr alone without closing them,
    * since if that is done a normal file will be opened which will
    * have the ufd value of stdin, stdout, or stderr and mess up all
    * the conditional testing for stdin, stdout, and stderr */
      if (b->ufd == stdin || b->ufd == stdout || b->ufd == stderr) {
	 if (!dupopen) {
            b->uconn = 0;
            b->ufd = NULL;
	 }
#define NAMEf_clos	f_clos
      } else if ((n = NAMEf_clos (&x)) != 0)
         return (n);
      b->luno = a->ounit;
#undef NAMEf_clos
   }

   org = a->oorg ? up_low (*a->oorg) : 0;
   b->umask = *mask;
   if (a->oacc == 0)
      switch (org) {
      case 'r':
	 b->uacc = DIRECT;
	 break;
      case 'i':
         if (dupopen)
           err(a->oerr, 186, "open")
	 b->uacc = KEYED;
	 break;
      default:
	 b->uacc = SEQUENTIAL;
      }
   else
      switch (up_low (*a->oacc)) {
      case 'd':
	 b->uacc = DIRECT;
	 if (org == 'i')
	    err(a->oerr, 149, "open")
	       break;
      case 'k':
	 b->uacc = KEYED;
	 if (org == 's')
	    err(a->oerr, 150, "open")
	       if (org == 'r')
	       err(a->oerr, 151, "open")
		  break;
      case 'a':
	 b->uacc = APPEND;
	 if (org == 'i')
	    err(a->oerr, 152, "open")
	       break;
/* Fix BN 11769 
 * Currently if the access parameter is not a keywords, it
 * sets it to the default ,sequential. Generate error instead.
 * ---ravi---2/21/92
 *
	      case 's':
	      default:  b->uacc = org == 'i' ? KEYED : SEQUENTIAL;
*/
      case 's':
	 b->uacc = org == 'i' ? KEYED : SEQUENTIAL;
	 break;
      default:
	 err(a->oerr, 130, "open");
      }
   if (a->oassocv && b->uacc == DIRECT)
      set_var ((ftnintu *)(b->uassocv = a->oassocv), b->umask, ASSOCV, 1);
   else
      b->uassocv = NULL;
   if (a->omaxrec && b->uacc == DIRECT)
      b->umaxrec = a->omaxrec;
   else
      b->umaxrec = 0;
   if (cbuf = a->odisp)
      switch (up_low (*cbuf++)) {
      case 'd':
	 b->udisp = DELETE;
	 break;
      case 'p':
	 b->udisp = PRINT;
	 goto checkdelete;
      case 's':
	 if (up_low (*cbuf) == 'a')
	    goto keep;
	 b->udisp = SUBMIT;
   checkdelete:
	 while (c = (*cbuf++))
	    if ((c == '/') && (c = (*cbuf)) && (up_low (c) == 'd'))
	       b->udisp |= DELETE;
	 break;
   keep:
      default:
	 b->udisp = KEEP;
      }
   else
      b->udisp = KEEP;

   b->ushared = a->oshared;
   b->ureadonly = a->oreadonly;
   if (a->oblnk && up_low (*a->oblnk) == 'z')
      b->ublnk = 1;
   else
      b->ublnk = 0;
#ifdef I90
	b->uaction = b->ureadonly ? READONLY : READWRITE;
	b->unpad = 0;
	b->udelim = DELIM_NONE;
#endif
   b->url = a->orl;
   if (a->ofm == 0) {
      if (b->uacc == DIRECT || b->uacc == KEYED) {
	 b->ufmt = 0;
	 if (!f77vms_flag_[OLD_RL])
	    b->url *= sizeof (int);
      } else
	 b->ufmt = 1;
   } else if (up_low (*a->ofm) == 'f')
      b->ufmt = 1;
   else if (up_low (*a->ofm) == 'b')
      b->ufmt = 2;
   else if (up_low (*a->ofm) == 's') {
      /* system file = direct unformatted file with record length = 1 */
      b->ufmt = 0;
      b->url = 1;
      b->uacc = DIRECT;
   } else {
      b->ufmt = 0;
      if (!f77vms_flag_[OLD_RL])
	 b->url *= sizeof (int);
      /* all sequential unformatted must need a minimum of 1K buffer to
	 avoid fseek() operations when reading which causes data to be
	 read from the disk each time and cause a 12X performance loss.
      */ 
      check_buflen( b, 1024 );
   }
   if (a->orectype)
      switch (up_low (*a->orectype)) {
      case 'f':
	 if (b->uacc != DIRECT && b->uacc != KEYED)
	    err(a->oerr, 156, "open")
	       break;
      case 'v':
	 if (b->uacc == DIRECT || b->uacc == KEYED ||
	     b->ufmt == 1)
	    err(a->oerr, 157, "open")
	       break;
      case 's':
	 if (b->uacc == DIRECT || b->uacc == KEYED ||
	     b->ufmt != 1)
	    err(a->oerr, 158, "open")
      default:
	    break;
      }
   if (a->occ == 0)
	b->ucc = (char) (b->ufmt ? ((b->luno == 6 && f77vms_flag_[VMS_CC]) ?
		CC_FORTRAN : CC_LIST) : CC_NONE);
   else
   switch (up_low (*a->occ)) {
   case 'l':
      b->ucc = CC_LIST;
      break;
   case 'f':
      b->ucc = CC_FORTRAN;
      b->ucchar = '\0';
      break;
   case 'n':
      b->ucc = CC_NONE;
      break;
   default:
      b->ucc = (char) (b->ufmt ? ((b->luno == 6 && f77vms_flag_[VMS_CC]) ?
			  CC_FORTRAN : CC_LIST) : CC_NONE);
   }

   if (!b->ufmt && b->ucc != CC_NONE)
      err(a->oerr, 162, "open");

   if (a->ofnm == 0)
#ifdef SIZEOF_LUNO_IS_64
      (void) sprintf (abuf, "fort.%lld", a->ounit);
#else
      (void) sprintf (abuf, "fort.%d", a->ounit);
#endif
   else
      g_char (a->ofnm, a->ofnmlen, abuf);

   {				/* #2313 sjc 26Feb88 */
#define OPEN "open("
#define RPAREN ")"
#define ELLIPSES "...)"
      strcpy (errstr, OPEN);
      strncat (errstr, buf, (sizeof errstr) - (sizeof OPEN));
      if (strlen (errstr) + (sizeof RPAREN) < sizeof errstr)
	 strcat (errstr, RPAREN);
      else
	 strcat (errstr + (sizeof errstr) - (sizeof ELLIPSES), ELLIPSES);
   }

   /* Don't open pipe with "r" mode since that will make it hangs
    * inside the open call */
   sbuf.st_mode = 0;
   if (stat (buf, &sbuf) >= 0 && (sbuf.st_mode & S_IFIFO))
   {
      mode = "a+";
      b->uwrt = WR_READY;
   }
   if (sbuf.st_mode & S_IFDIR) {
	/* returning error if trying to open a directory */
	errno = F_EROPENDIR;
	err(a->oerr, F_EROPENDIR, errstr);
   }
   if (a->osta == 0)
      goto osta_unknown;
   switch (up_low (*a->osta)) {
   case 'o':
      if (absent (buf, b->uacc)) {
	 err(a->oerr, errno, errstr)
      }
osta_unknown:
   default:
      b->uscrtch = 0;
      /* if status unknown and file does not exist, create it AGC
       * 1/9/87 #538 */
      if (absent (buf, b->uacc)) {
	 if (a->oreadonly)
	    err(a->oerr, 160, errstr);
	 /* file does not exist, set mode to create it */
	 /* (void) close(creat(buf, 0666)); */
	 mode = "w";
      }

      /* If this is a duplicated open of a pre-connected unit, return
      ** after changing the file characteristics without reopening 
      ** the file.
      */
      if (dupopen) {
	 /* What a mess !!  If the file is to be converted to direct
	 ** unformatted file then the ufd has to be converted to file
	 ** descriptor and cannot be a FILE *.   Also all the buffering
	 ** setup in fio_direct_io has to be done.
	 */
	 if (b->uacc == DIRECT && b->ufmt == 0) {
	   mode = (b->ufd == stdin) ? "r" : "w";
	   b->ufd = (FILE *) fileno( b->ufd );
	    if(_fio_du_open( buf, mode, 1, (int) b->ufd ) < 0)
	      err(a->oerr, errno, buf);
	 }
	 return( 0 );
      }
/* Fix BN 9310. /dev/tty should always be opened with w+ since we are sure that
 * it is read and write by the world.
 * ---ravi--1/7/91
 */
      if (istty)
	 mode = "r+";
done:
      b->ufnm = (char *) malloc ((unsigned int) (strlen (buf) + 1));
      if (b->ufnm == NULL)
	 err(a->oerr, 113, "no space");
      (void) strcpy (b->ufnm, buf);
      _I90_uppercase(buf, ubuf);
      b->uend = 0;

      /* Open files with mode and don't seek to eof AGC #538 1/9/87 */

      if (b->uacc == KEYED) {
	 if (b->unkeys = a->onkeys)
	    b->ukeys = (Keyspec *) malloc (sizeof (Keyspec) * a->onkeys);
	 for (n = 0; n < a->onkeys; n++) {
	    b->ukeys[n].e1 = a->okeys[n].e1;
	    b->ukeys[n].e2 = a->okeys[n].e2;
	    b->ukeys[n].dt = a->okeys[n].dt;
	 }
	 b->useek = b->uconn = 1;
	 *mode_ = mode;
	 return (0);
      } else if (!strcmp (ubuf, "SYS$INPUT")) {

	 /* This fix for BN 8077 doesn't make any sense. Just because
	  * the SYS$ files are being associated with somee other
	  * fortran unit numbers does not mean that stdin , stdout
	  * and stderr should be closed ---ravi---1/16/92 */

	 b->ufd = stdin;
	 b->uconn = 1;
	 b->uwrt = 0;
      } else if (!strcmp (ubuf, "SYS$OUTPUT")) {
	 b->ufd = stdout;
	 b->uconn = 1;
	 b->uwrt = WR_READY;
      } else if (!strcmp (ubuf, "SYS$ERROR")) {
	 b->ufd = stderr;
	 b->uconn = 1;
	 b->uwrt = WR_READY;
      } else if ((b->uacc == DIRECT) && (b->ufmt == 0)) {

	 /* Open for direct unformatted files. */

	 if (b->url == 0) {	/* must specify record length for
				 * direct access */
	    err(a->oerr, 153, "open");
	 }
	 if (!b->ureadonly)
	     b->ufd = (FILE *) _fio_du_open (buf, "r+", 0, 0);
	 if (b->ureadonly || (int)b->ufd == -1)  {
	     b->ufd =(FILE*)_fio_du_open (buf, "r", 0, 0);
	     /* 
	     ** If the file can't be written to then set the READONLY
	     ** flag so if the user attempts to write to it later
	     ** we can return a sensible error rather than error status 9.
	     */
	     b->ureadonly = 1;
	 }
	 if ((int)b->ufd == -1) {
	    if (b->ureadonly)
	        err(a->oerr, 160, errstr);
	    b->ufd =(FILE*)_fio_du_open (buf, "w", 0, 0);
	 }

	 if ((int) b->ufd < 0) {
	    err(a->oerr, errno, buf);
	 }
	 b->uconn = 1;
	 b->uwrt = WR_READY;
	 /*
	 For direct unformatted files.  THe file mode has to be WR_READY
	 always
	 if (!(sbuf.st_mode & S_IFIFO))
	    b->uwrt = mode[1] == '+' ? WR_READY : (mode[0] == 'w') ? WR_ONLY : RD_ONLY;
	 */

	 b->uistty = isatty ((int) b->ufd);
	 if (sbuf.st_mode & S_IFIFO)
	    b->uistty = 2;
	 if (b->uistty && mode[1] == '\0') {	/* reopen read/write */
	    if (-1 == _fio_du_close ((int) b->ufd))
	       return (1);
	    b->ufd = (FILE *) _fio_du_open (buf, "r+", 0, 0);
	    if ((int) b->ufd == -1)
	       return (1);
	 }
	 if (b->uacc == APPEND)
	    lseek ((int) b->ufd, 0L, SEEK_SET);

	 a->ofnmlen = (int) strlen (buf);

	 /* getting file status after it is opned */
	 (void) fstat ((int) b->ufd, &stat_struct);
	 if (stat_struct.st_nlink > 0 /* pipe */ && !isatty ((int) b->ufd)) {
	    b->useek = 1;
	 } else {
	    b->useek = 0;
	 }

	 if (f77inode (buf, &b->uinode) == -1)
	    err(a->oerr, 108, "open");
	return (0);
      } else {
         if (!dupopen) {
            b->ufd = fopen (buf, mode);
	 }
         else {
	    int dupfd, ufd;
	    ufd = ((b->uacc == DIRECT) && (b->ufmt == 0)) ? (int) dupunit->ufd : fileno( dupunit->ufd );
	    dupfd = dup( ufd );
	    if (dupfd < 0)
	       err(a->oerr, errno, buf)
	    b->ufd = fdopen( dupfd, mode );
         }
         if (b->ufd == NULL) {
	    err(a->oerr, errno, buf)
         } else {
	    if (b->ushared && b->url)
	        setvbuf( b->ufd, NULL, _IOFBF, (size_t) b->url);
	    b->uconn = 1;
	    if (!(sbuf.st_mode & S_IFIFO))
	       b->uwrt = mode[1] == '+' ? WR_READY : (mode[0] == 'w') ? WR_ONLY : RD_ONLY;
         }
      }

      if ((b->uacc == DIRECT) && (b->url == 0)) {
	 err(a->oerr, 153, "open");
      }
      if (b->ufd != stdin && b->ufd != stdout && b->ufd != stderr
	  && (b->uacc != DIRECT)) {
	 b->uistty = isatty (fileno (b->ufd));
	 if (sbuf.st_mode & S_IFIFO)
	    b->uistty = 2;
	 if (b->uistty && mode[1] == '\0') {
            b->ufd = freopen (buf, "r+", b->ufd);
            if (!b->ufd)
	       return (1);
	    b->uwrt = WR_READY;
	 }
	 if (b->uacc == APPEND)
	    fseek (b->ufd, 0L, 2);
      }
      /* Old code assumed we would open for write.	AGC #538
       * 1/9/87 if(f77isdev(buf)) {	b->ufd = fopen(buf,"r");
       * if(b->ufd==NULL) err(a->oerr,errno,buf) else	b->uwrt = 0;
       * } else {	if ((b->ufd = fopen(buf, "r+")) || (b->ufd =
       * fopen(buf, "w+"))) { if (fseek(b->ufd, 0L, 2 ))
       * err(a->oerr,errno,"open") else b->uwrt = 1; } else
       * if((b->ufd = fopen(buf, "r")) != NULL) {	(void)
       * fseek(b->ufd, 0L, 2); b->uwrt = 0; } else { err(a->oerr,
       * errno, buf) } } */

	/* Don't buffer shared files */
/*
      if (b->ushared && (b->uacc != DIRECT || b->ufmt != 0))
	 setbuf( b->ufd, NULL );
*/
/*      fprintf( debugfile, "Open logical unit %d as FILE %x, urel = %d\n", b->luno, b->ufd, b->url );
*/
      a->ofnmlen = (int) strlen (buf);
      if (b->uacc != KEYED)
	 b->useek = f77canseek (b->ufd);
      else
	 mkidxname (buf, buf);

      if (f77inode (buf, &b->uinode) == -1)
	 err(a->oerr, 108, "open");
      buf[a->ofnmlen] = '\0';

      /* do not need to rewind any more    AGC #538 1/9/87 if(a->orl
       * && b->useek) rewind(b->ufd); */
      return (0);

   case 's':
      if (a->oreadonly)
	 err(a->oerr, 160, "open");
      b->uscrtch = 1;
      /* Only generate name if user doesn't specify his own name */
      /* This scheme allows upto 26**3 = 17456 different names */
      ch = buf[0];		/* save the first char in the
				 * template */
newname:
      (void) strcpy (abuf, "tmp.FxxXXXXXX");
      abuf[5] = *seed;
      abuf[6] = *(seed + 1);
      /* Use 'buf', not 'abuf' since mktemp() needs the full path
       * name to check for the file existence */
      (void) mktemp (buf);
      if (!buf[0]) {
	 /* Use up the 26 different names allowed by mktemp */
	 /* Have to change the seed */
	 buf[0] = ch;		/* recreate the destroyed character */
	 if (++*q > 'z') {
	    q++;
	    if (*q == '\0')
	       /* Assuming that the user won't open 17456 scratch
	        * files AT THE SAME TIME, let's try to recycle
	        * oldnames */
	       seed[0] = 'a';
	    seed[1] = 'a';
	    q = seed;
	 }
	 goto newname;
      }
      mode = "w+";
      goto done;
   case 'n':
      if (a->oreadonly)
	 err(a->oerr, 160, "open");
      b->uscrtch = 0;
      /* SYSDEP access */
      if (absent (buf, b->uacc)) {	/* AGC 1/10/87 #537 */
	 /* file does not exist, set mode to create it */
	 /* (void) close(creat(buf, 0666)); */
	 mode = "w+";
      } else {
	 /* file exists, should return error */
	 err(a->oerr, 126, buf)
      }
      goto done;
   }
}

int
f_open0 (olist *a, int *mask)
{				/* provided for 3.3 object file
				 * compatibility */
   char           *mode;
   char           *buf;
   int             i;
   unit		  *ftnunit;
   olist64 dst;
   get_olist64(&dst, a);
#define WORK_ARG (&dst)

   i = f_open_com(WORK_ARG, mask, &mode, &buf, &ftnunit);
   io_lock = 0;
   if (ftnunit->uacc == KEYED) {
      if (i) {
         ftnunit->lock_unit = 0;
         return (i);
      }
      i = idxopen (ftnunit, buf, *mode == 'w', WORK_ARG->oerr);
      ftnunit->lock_unit = 0;
      return (i);
   } 
   ftnunit->lock_unit = 0;
   return (i);
#undef WORK_ARG
}

int
f_open1 (olist *a, int *mask)
{
   char           *buf;
   char           *mode;
   int 		   n;

   olist64 dst;
   get_olist64(&dst, a);
#define WORK_ARG (&dst)
   n = f_open_com (WORK_ARG, mask, &mode, &buf, &f77curunit);
   io_lock = 0;
   return (n);
#undef WORK_ARG
}

/* declare pure 64 bit versions */

int
f_open064 (olist64 *a, int *mask)
{				/* provided for 3.3 object file
				 * compatibility */
   char           *mode;
   char           *buf;
   int             i;
   unit		  *ftnunit;

   i = f_open_com(a, mask, &mode, &buf, &ftnunit);
   io_lock = 0;
   if (ftnunit->uacc == KEYED) {
      if (i) {
         ftnunit->lock_unit = 0;
         return (i);
      }
      i = idxopen (ftnunit, buf, *mode == 'w', a->oerr);
      ftnunit->lock_unit = 0;
      return (i);
   } 
   ftnunit->lock_unit = 0;
   return (i);
}

#pragma weak f_open064_mp = f_open064




int
fk_open (int seq, int fmt, ftnint n)
{
   char            nbuf[10];
   int             i = 0;
   static olist    a_init ={
	       1, 0, NULL, 0, NULL, NULL, NULL, 0, NULL, NULL, NULL,
			 0, 0, 0, NULL, NULL, NULL, 0, NULL, 0, NULL,
			 NULL, 0, 0, 0
#ifdef I90
			 ,NULL,0,NULL,0,NULL,0,NULL,0
#endif
	};
   
   olist    a = a_init;

#ifdef SIZEOF_LUNO_IS_64
   (void) sprintf (nbuf, "fort.%lld", n);
#else
   (void) sprintf (nbuf, "fort.%d", n);
#endif
   a.ounit = n;
   a.ofnm = nbuf;
   a.ofnmlen = (int) strlen (nbuf);
   a.oacc = seq == SEQ ? "s" : "d";
   a.oorg = seq == SEQ ? "s" : seq == DIR ? "r" : "i";
   a.ofm = fmt == FMT ? "f" : "u";
   a.orl = seq == DIR ? 1 : 0;
   /* Have to use the non_MP version here to avoid deadlock since the
   routines calling fk_open() have already obtained a lock on this
   logical unit
   */

   return (f_open1 (&a, &i));
}


/*	This is a replacement for f_dfnf for the mongoose 7.0 compiler
**	and use the same interface of f_open064 for simplicity
*/

int
f_df64x(olist64 *a, XINT xmask)
{
   int mask = xmask;
   int rlflag = f77vms_flag_[OLD_RL];
   int n;

   a->oacc = "d";
   a->oorg = "r";
   a->ofm = "u";
   a->occ = "n";
   a->orl = a->orl << 1;

   f77vms_flag_[OLD_RL] = 1;
   n = f_open064 (a, &mask);
   f77vms_flag_[OLD_RL] = (unsigned short) rlflag;
   return ( n );
}


int
f_dfnf (struct dfnf_struct *b)
{
	static ftnint i = 0;
	return(f_dfnf1(b, &i));
}

int
f_dfnf1 (struct dfnf_struct *b, ftnint *mask)
{
   char            nbuf[10];
   olist           a;
   int n, rlflag;

#if SIZEOF_LUNO_IS_64
   (void) sprintf (nbuf, "fort.%lld", b->unit);
#else
   (void) sprintf (nbuf, "fort.%d", b->unit);
#endif
   a.oerr = 0;
   a.ounit = b->unit;
   a.ofnm = nbuf;
   a.ofnmlen = (int) strlen (nbuf);
   a.osta = NULL;
   a.oacc = "d";
   a.oorg = "r";
   a.ofm = "u";
   a.occ = "n";
   a.orl = b->recl << 1;
   a.oblnk = NULL;
   a.oassocv = b->assocv;
   a.odisp = NULL;
   a.omaxrec = b->maxrec;
   a.orectype = NULL;
   a.odfnm = NULL;
/* 
 * Fix BN 11785 
 * Set the readonly switch to zero.
 * ---ravi---2/20/92
 */
   a.oreadonly = 0;
   /* Make sure that the OLD_RL flag is turned on so that the length
   will be interpreted as number of bytes */
   rlflag = f77vms_flag_[OLD_RL];
   f77vms_flag_[OLD_RL] = 1;
   n = f_open1 (&a, mask);
   f77vms_flag_[OLD_RL] = (unsigned short) rlflag;
   return(n);
}


void 
flush_connected_units ()
{	/* this is called from libU77 */
   register int    i;
   register unit  *a;

   for (i = 0, a = f77units; i < space_assigned; i++, a++)
      if (a->uconn > 0)
	    if ((a->uacc != DIRECT) || (a->url == 0)) {
	       (void) fflush (a->ufd);
            } else {  /* DIRECT UNFORMATTED */
	       (void) _fio_du_flush((int) a->ufd);
            }
}

int absent(char *name, int acc)
{
   int             l, r;
   char		   ubuf[PATH_MAX];

/* Fix BN 11327.
 * If the file being tested is either of the SYS$ files then do not test for
 * their existance since we know they do not exist but return a sucess 
 * becuase these file are associated with stdin , stdout and stderr
 * respectively.
 * ---ravi---1/16/92
 */
   if (name[3] == '$' && _I90_uppercase(name, ubuf) &&
   (!strcmp (ubuf, "SYS$INPUT") || !strcmp (ubuf, "SYS$OUTPUT") ||
       !strcmp (ubuf, "SYS$ERROR")))
      return (0);
   if (acc == KEYED) {
      l = (int) strlen (name);
      mkidxname (name, name);
      if (r = access (name, 0))
	 goto ret;
      name[l] = '\0';
      mkdatname (name, name);
      if (r = access (name, 0))
	 goto ret;
#ifndef SYSV
      name[l] = '\0';
      mklokname (name, name);
      r = access (name, 0);
#endif
ret: name[l] = '\0';
      return r;
   } else
      return access (name, 0);
}

int
inc_var (ftnintu *var, ftnint mask, int shift)
{
   if (var)
      switch (((mask) >> shift) & 3) {
      case 1:
	 (var->byte)++;
	 break;
      case 2:
	 (var->word++);
	 break;
      default:
	 (var->longword)++;
      }
   return(0);
}

int
set_var (ftnintu *var, ftnint mask, int shift, long long value)
{
   switch (((mask) >> shift) & 3) {
   case 1:
      var->byte = (char) value;
      break;
   case 2:
      var->word = (short) value;
      break;
   case 3:
      var->longlongword = value;
      break;
   default:
      var->longword = (int) value;
   }
   return(0);
}

/* BN-8077 */
void
f_dconn (luno)
   ftnint          luno;
{
   register int    i;
   register unit  *a;

   for (i = 0, a = f77units; i < mxunit; i++, a++) {
      if (a->luno == luno) {
	 a->ufd = 0;
	 a->uconn = 0;
      }
   }
}

int
f_duped (olist64 *a, unit *ftnunit, unit **dupunit)
{
   register int    i;
   register unit  *u;
   int nlen;

   *dupunit = NULL;
   if (!a->ofnm)
      return(0);

   for (i = 0, u = f77units; i < mxunit; i++, u++) {
      if (u->uconn > 0 && u->luno != a->ounit &&
	  u->ufnm && (nlen = strlen(u->ufnm)) == a->ofnmlen
	  && !strncmp (u->ufnm, a->ofnm, nlen)) {
	 *dupunit = u;
	 err(a->oerr, 109, "open")
      }
   }
   return(0);
}

/*  ========================================================================  */
/*									      */
/*  The following entry is a minor efficiency improvement in calling	      */
/*  the open I/O routine.  In truth, the entire I/O interface could use an    */
/*  overhaul.								      */
/*									      */
/*  ========================================================================  */

int
f_open064x (olist64 *a, XINT xmask)
{
    int mask = xmask;
    return ( f_open064 ( a, &mask ) );
}

#pragma weak f_open064x_mp = f_open064x
