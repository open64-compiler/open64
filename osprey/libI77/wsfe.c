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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/wsfe.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
/*write sequential formatted external*/

#include <cmplrs/fio.h>
#include "cmplrs/host.h"
#include "fmt.h"
#include "iomode.h"
#include "wsfe.h"
#include "wrtfmt.h"
#include "idxio.h"
#include "err.h"
#include "sfe.h"
#include "util.h"
#include "uio.h"
#include "rsfe.h"
#include "bcompat.h"
#include <string.h>

extern int lnblnk_ (char *s, int len); /* in libF77 */

int32 wsfe (cilist64 *a, unit **fu, int f90sw)
{
   int32             n;
   unit             *ftnunit;

   if (!f77init)
      f_init ();

   if (n = c_sfe (a, fu))
      return n;

   ftnunit = *fu;
   if (ftnunit->uacc == DIRECT)
      errret(a->cierr, 171, "sequential write");

   ftnunit->f77cursor = ftnunit->f77recpos = ftnunit->f77recend = 0;
   ftnunit->f77cplus = 0;
   ftnunit->f77scale = 0;
#ifdef I90
   ftnunit->f90sw = f90sw;
   if (!f90sw) ftnunit->f77recpos = ftnunit->f77recend = 0;
   /* in f90 mode we initialize f77recpos and f77recend in the caller, dont
      know why. */
#else
   ftnunit->f77recpos = ftnunit->f77recend = 0;
#endif
   ftnunit->f77errlist.cierr = a->cierr;
   ftnunit->f77errlist.ciend = a->ciend;
   ftnunit->f77errlist.cieor = a->cieor;
   ftnunit->f77errlist.cisize = a->cisize;
   ftnunit->f77errlist.iciunit = 0; 
   ftnunit->f77fmtbuf = a->cifmt;
   if (ftnunit->uacc == KEYED) {
      ftnunit->f77idxlist.cimatch = a->cimatch;
      ftnunit->f77idxlist.cikeytype = a->cikeytype;
      ftnunit->f77idxlist.cikeyval.cicharval = a->cikeyval.cicharval;
      ftnunit->f77idxlist.cikeyid = a->cikeyid;
      ftnunit->f77idxlist.cinml = a->cinml;
      ftnunit->f77idxlist.cikeyvallen = a->cikeyvallen;
   }
   if (pars_f (ftnunit, ftnunit->f77fmtbuf) < 0)
      errret(a->cierr, 100, "startio");
   ftnunit->f77putn = x_putc;
   ftnunit->f77ungetn = x_ungetc;
   ftnunit->f77doed = w_ed;
   ftnunit->f77doned = w_ned;
   ftnunit->f77doend = xw_end;
   ftnunit->f77dorevert = xw_rev;
   ftnunit->f77donewrec = x_wSL;
   fmt_bg (ftnunit);
   ftnunit->f77cblank = ftnunit->ublnk;
   if (ftnunit->url > ftnunit->f77fio_size)
       check_buflen( ftnunit, ftnunit->url > FIO_ALLOC ? ftnunit->url : FIO_ALLOC );
   return 0;
}

static int32 s_wsfe_com (cilist64 *a, unit **fu)
{
   unit           *ftnunit;
   int             n = wsfe(a, fu, 0);

   ftnunit = *fu;
   if (n)
      return n;
   if (ftnunit->uacc != KEYED) {
      if (f77nowwriting (ftnunit))
	 errret(a->cierr, 160, "startwrt");
   }

#ifdef I90
   ftnunit->f90sw = 0;
   ftnunit->f90nadv = 0;
   ftnunit->f77recpos = 0;
   ftnunit->f77recend = 0;
#endif
   ftnunit->dowrite = x_wEND;
   return (0);
}

int32 s_wsfe (cilist *a)
{

  cilist64 dst;
  get_cilist64(&dst, a);
  return s_wsfe_com(&dst, &f77curunit);
}

int32 s_wsfe_mp (cilist *a, unit **fu)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_wsfe_com(&dst, fu);
}

int s_wsfe64 (cilist64 *a)
{
    return( s_wsfe_com( a, &f77curunit ) );
}

int s_wsfe64_mp (cilist64 *a, unit **fu)
{
    return( s_wsfe_com( a, fu ) );
}



int
x_putc (register unit *ftnunit, register XINT count, char con, char *buf)
{
   register char  *iobuf;
   XINT    new_size;

   /*
   It is very expensive to declare irrelevant symbols as 'register'
   in this routine
   */
   if (((new_size = ftnunit->f77recpos + count)) > ftnunit->f77fio_size)
      check_buflen( ftnunit, new_size + FIO_ALLOC );

   iobuf = ftnunit->f77fio_buf + ftnunit->f77recpos;
   ftnunit->f77recpos = new_size;

   /* LHL 4/27/89 change it back to use memcpy for performance
    * reason. if ( buf ) for ( ; count--; iobuf++, buf++ ) *iobuf =
    * *buf; */
   if (buf)
      memcpy (iobuf, buf, count);
   else {
      if (con)
	 while (count--)
	    *(iobuf++) = con;
      else if ((count = ftnunit->f77recpos - ftnunit->f77recend) > 0) {
	 iobuf = ftnunit->f77fio_buf + ftnunit->f77recend;
	 while (count--)
	    *(iobuf++) = ' ';
      }
   }

   if (ftnunit->f77recpos > ftnunit->f77recend)
      ftnunit->f77recend = ftnunit->f77recpos;
   return (0);
}

int32
x_wSL (unit *ftnunit)
{
   int rslt;
   int nadv = ftnunit->f90nadv;
   ftnunit->f90nadv = 0;
   rslt = (int)((*ftnunit->dowrite) (ftnunit));
   ftnunit->f90nadv = nadv;
   return rslt;
}

int32
x_wEND (unit *ftnunit)
{
   register char  *abuf, cc;
   register XINT    count;
   register int n;

   if (ftnunit->uacc == KEYED) {
      if (n = idxwrite (ftnunit)) return n;
      ftnunit->f77nonl = ftnunit->f77recpos = ftnunit->f77recend = ftnunit->f77cursor = 0;
   } else if (ftnunit->ufmt == 2) {
      return 0;
   } else {
      ftnunit->f77recpos = 0;
      if (ftnunit->ucc == CC_FORTRAN) {
	 cc = (char) ((ftnunit->f77recpos < ftnunit->f77recend) 
		? ftnunit->f77fio_buf[ftnunit->f77recpos++] : ' ');
	 switch (cc) {
	 case '1':
	    putc ('\f', ftnunit->ufd);
	    break;
	 case '0':
	    putc ('\n', ftnunit->ufd);
	 case '$':
	 default:
	    if (ftnunit->ucchar)
	       putc (ftnunit->ucchar, ftnunit->ufd);
	 case '+':
	 case '\0':
	    break;
	 }
      }
      count = ftnunit->f77recend - ftnunit->f77recpos;
      abuf = ftnunit->f77fio_buf + ftnunit->f77recpos;
      while (count--)
	 putc (*abuf++, ftnunit->ufd);
      if (ftnunit->ucc == CC_FORTRAN) {
	 ftnunit->ucchar = '\0';
	 switch (cc) {
	 case '+':
	 default:
	    if (ftnunit->f77nonl)
	       break;
	 case '0':
	 case '1':
	    putc ('\r', ftnunit->ufd);
	    if (ftnunit->ufd == stdout)
	       fflush (stdout);
	    ftnunit->ucchar = '\n';
	 case '$':
	 case '\0':
	    break;
	 }
      }
      if ( ftnunit->ucc == CC_LIST
      &&  !ftnunit->f77nonl 
#ifdef I90
      &&   ftnunit->f90nadv == 0
#endif
	 ) putc ('\n', ftnunit->ufd);

      ftnunit->f77nonl = ftnunit->f77recpos = ftnunit->f77recend = 0;
#ifdef I90
      if (ftnunit->f90nadv == 0) {
	 ftnunit->f77cursor = 0;
      }
#else
      ftnunit->f77cursor = 0;
#endif

      if (ftnunit->ufd && ferror (ftnunit->ufd))
	 return errno;
      else
	 return 0;
   }
   return 0;
}

int32
xw_end (unit *ftnunit)
{
   int             n =(*ftnunit->dowrite)(ftnunit);

   return n == 1 ? 0 : n;
}

int32
xw_rev (unit *ftnunit)
{
   if (ftnunit->f77workdone)
      x_wSL (ftnunit);
   return ((int) (ftnunit->f77workdone = 0));
}
