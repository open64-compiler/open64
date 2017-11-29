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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/inquire.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/* sjc  #1963	11Dec87		Dynamically allocate unit table		*/

#include <cmplrs/fio.h>
#include <mutex.h>
#include <cmplrs/f_errno.h>
#include <sys/errno.h>
#include <limits.h>
#include <unistd.h>

#define INEX     0
#define INOPEN   2
#define INNUM	 4
#define INNAMED  6
#define INRECL   8
#define INNREC   10
#include "vmsflags.h"

#include <sys/types.h>
#include <string.h>
#include "util.h"
#include "open.h"
#include "err.h"
#include "bcompat.h"
#include "isam.h"

extern void     _cleanup(void);

static int
f_inqu0_com (inlist64 *a, int *mask, int lock)
{
   flag            byfile;
   flag            sysfile = 0;	/* set this flag if the file name is
				 * "SYS$*" */
   int             i;
   char            buf[PATH_MAX], *abuf;
   int             x = 0;
   ino_t           inod;
   unit           *ftnunit;

   if (a->infile != NULL) {
      char           *flname;

      flname = a->infile;
      byfile = 1;
      if (flname[3] == '$' && _I90_uppercase(flname, buf) &&
	 (!strcmp (buf, "SYS$INPUT") || !strcmp (buf, "SYS$OUTPUT")
	  || !strcmp (buf, "SYS$ERROR")))
	 sysfile = 1;

      if (a->indefaultfile) {
	 g_char (a->indefaultfile, a->indefaultfilelen, buf);
	 abuf = buf + strlen (buf);
      } else
	 abuf = buf;
      g_char (a->infile, a->infilen, abuf);
      /* bug fix 12983 */
      x = f77inode (buf, &inod);
      if (x < 0) {
	 mkidxname (buf, buf);
	 x = f77inode (buf, &inod);
      }
      if (strlen (buf) > PATH_MAX) {
	 /* can't use err() with inquire since f77curunit is never defined
	   by a call to map_luno
	 err (a->inerr, 145, "inquire");
	 */
	 if (a->inerr) {
	     return(errno=F_ERFNAME);
	 } else {
	     fprintf(stderr, "Error in INQUIRE: file name too long: %s\n", buf);
	     _cleanup ();
	     exit(F_ERFNAME);
	 }
      }
      ftnunit = NULL;
      if (x < 0)
	 goto setvar;
      for (i = 0; i < mxunit; i++)	/* sjc #1963 11Dec 87 */
	 if (f77units[i].uinode == inod && f77units[i].uconn > 0) {
	    ftnunit = &f77units[i];
      	    /*
      	    while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
          	;
      	    */
	    break;
	 }
   } else {
      byfile = 0;
      ftnunit = map_luno (a->inunit);
      /*
      while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
          ;
      */
   }
setvar:
   if (a->inex)
/* Fix BN 11327 .
 * If the file name is SYS$* then the file always exists. The sysfile
 * flag gets set above and hence when a user does and inquire with EXIST, then
 * the exist variable will be set .
 *
 * ---ravi---1/16/92
 */
      set_var (a->inex, *mask, INEX,
      (byfile && x > 0 || !byfile && ftnunit != NULL || sysfile) ? 1 : 0);
   if (a->inopen)
      set_var (a->inopen, *mask, INOPEN, byfile ? (ftnunit != NULL) : (ftnunit && ftnunit->uconn > 0));
   if (a->innum)
      set_var (a->innum, *mask, INNUM, ftnunit ? ftnunit->luno : 0);
   if (a->innamed)
      set_var (a->innamed, *mask, INNAMED,
	       (byfile || ftnunit != NULL && ftnunit->ufnm != NULL) ? 1 : 0);
   if (a->inname != NULL)
      if (byfile)
	 b_char (buf, a->inname, a->innamlen);
      else if (ftnunit != NULL && ftnunit->ufnm != NULL)
	 b_char (ftnunit->ufnm, a->inname, a->innamlen);
      else
	 b_char ("", a->inname, a->innamlen);
   if (a->inacc)
      if (ftnunit && ftnunit->uconn > 0)
	 switch (ftnunit->uacc) {
	 case SEQUENTIAL:
	    b_char ("SEQUENTIAL", a->inacc, a->inacclen);
	    break;
	 case DIRECT:
	    b_char ("DIRECT", a->inacc, a->inacclen);
	    break;
	 case KEYED:
	    b_char ("KEYED", a->inacc, a->inacclen);
	    break;
	 default:
	    b_char ("UNKNOWN", a->inacc, a->inacclen);
	 }
      else
	 b_char ("UNKNOWN", a->inacc, a->inacclen);
   if (a->inseq != NULL)
      if (ftnunit)
	 b_char ((ftnunit->uacc == SEQUENTIAL) ? "YES" : "NO",
		 a->inseq, a->inseqlen);
      else
	 b_char ("UNKNOWN", a->inseq, a->inseqlen);
   if (a->indir != NULL)
      if (ftnunit)
	 b_char ((ftnunit->uacc == DIRECT) ? "YES" : "NO",
		 a->indir, a->indirlen);
      else
	 b_char ("UNKNOWN", a->indir, a->indirlen);
   if (a->infmt != NULL)
      if (ftnunit)
	 if (!ftnunit->ufmt)
	    b_char ("UNFORMATTED", a->infmt, a->infmtlen);
	 else if (ftnunit->ufmt == 1)
	    b_char ("FORMATTED", a->infmt, a->infmtlen);
	 else
	    b_char ("BINARY", a->infmt, a->infmtlen);
      else
	 b_char ("UNKNOWN", a->infmt, a->infmtlen);
   if (a->inform != NULL)
      if (ftnunit)
	 b_char (ftnunit->ufmt > 0 ? "YES" : "NO", a->inform, a->informlen);
      else
	 b_char ("UNKNOWN", a->inform, a->informlen);
   if (a->inunf)
      if (ftnunit)
	 b_char (ftnunit->ufmt > 0 ? "NO" : "YES", a->inunf, a->inunflen);
      else
	 b_char ("UNKNOWN", a->inunf, a->inunflen);
   if (a->inrecl)
      set_var (a->inrecl, *mask, INRECL,
	       (int) (ftnunit ? (ftnunit->ufmt || f77vms_flag_[OLD_RL] ?
		    ftnunit->url : ftnunit->url / sizeof (int)) : 0));
   if (a->innrec) {
      /* CALVIN: need to determine if a->innrec points to a *4 or a *8 */
      if (ftnunit && (ftnunit->uacc == DIRECT) && (ftnunit->ufmt == 0)) {
	 set_var (a->innrec, *mask, INNREC,
	     (ftnunit && ftnunit->uacc == DIRECT && ftnunit->url) ? ftnunit->uirec + 1 : 0);
      } else {
	 set_var (a->innrec, *mask, INNREC,
		  (ftnunit && ftnunit->uacc == DIRECT && ftnunit->url) ? ftell (ftnunit->ufd) / ftnunit->url + 1 : 0);
      }

   }
   if (a->inblank)
      if (ftnunit && ftnunit->ufmt > 0)
	 b_char (ftnunit->ublnk ? "ZERO" : "NULL", a->inblank, a->inblanklen);
      else
	 b_char ("UNKNOWN", a->inblank, a->inblanklen);
   if (a->incc)
      if (ftnunit && ftnunit->ufmt > 0)
	 switch (ftnunit->ucc) {
	 case CC_FORTRAN:
	    b_char ("FORTRAN", a->incc, a->incclen);
	    break;
	 case CC_LIST:
	    b_char ("LIST", a->incc, a->incclen);
	    break;
	 case CC_NONE:
	    b_char ("NONE", a->incc, a->incclen);
	    break;
	 default:
	    b_char ("UNKNOWN", a->incc, a->incclen);
	 }
      else
	 b_char ("UNKNOWN", a->incc, a->incclen);
   if (a->inkeyed)
      if (ftnunit)
	 b_char (ftnunit->uacc == KEYED ? "YES" : "NO", a->inkeyed, a->inkeyedlen);
      else
	 b_char ("UNKNOWN", a->inkeyed, a->inkeyedlen);
   if (a->inorg)
      if (ftnunit)
	 switch (ftnunit->uacc) {
	 case SEQUENTIAL:
	    b_char ("SEQUNTIAL", a->inorg, a->inorglen);
	    break;
	 case DIRECT:
	    b_char ("RELATIVE", a->inorg, a->inorglen);
	    break;
	 case KEYED:
	    b_char ("INDEXED", a->inorg, a->inorglen);
	    break;
	 default:
	    b_char ("UNKNOWN", a->inorg, a->inorglen);
	 }
      else
	 b_char ("UNKNOWN", a->inorg, a->inorglen);
   if (a->inrecordtype)
      if (ftnunit)
	 switch (ftnunit->uacc) {
	 case SEQUENTIAL:
	    b_char (ftnunit->ufmt == 1 ? "STREAM_LF" : "VARIABLE",
		    a->inrecordtype, a->inrecordtypelen);
	    break;
	 case DIRECT:
	 case KEYED:
	    b_char ("FIXED", a->inrecordtype, a->inrecordtypelen);
	    break;
	 default:
	    b_char ("UNKNOWN", a->inrecordtype, a->inrecordtypelen);
	 }
      else
	 b_char ("UNKNOWN", a->inrecordtype, a->inrecordtypelen);
   /*
   if (ftnunit) {
       ftnunit->lock_unit = 0;
   }
   */
   return (0);
}

int
f_inqu0 (inlist *a, int *mask)
{
  inlist64 dst;
  get_inlist64(&dst, a);
  return( f_inqu0_com(&dst, mask, 0));
}

int
f_inqu0_mp (inlist *a, int *mask)
{
  inlist64 dst;
  get_inlist64(&dst, a);
  return( f_inqu0_com(&dst, mask, 1));
}


int
f_inqu064(inlist64 *a, int *mask)
{
    return( f_inqu0_com( a, mask, 0 ) );
}

int
f_inqu064_mp (inlist64 *a, int *mask)
{
    return( f_inqu0_com( a, mask, 1 ) );
}


/*  ========================================================================  */
/*									      */
/*  The following entry is a minor efficiency improvement in calling	      */
/*  the inquire I/O routine.  In truth, the entire I/O interface could use    */
/*  an overhaul.							      */
/*									      */
/*  ========================================================================  */

int
f_inqu064x (inlist64 *a, XINT xmask)
{
    int mask = xmask;
    return ( f_inqu0_com ( a, &mask, 0 ) );
}

#pragma weak f_inqu064x_mp = f_inqu064x

/*  ========================================================================  */
