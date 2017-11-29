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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/sue.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
#include <limits.h>
#include <mutex.h>
#include <cmplrs/fio.h>
#include "fmt.h"
#include "iomode.h"
#include "uio.h"
#include "sue.h"
#include "err.h"
#include "util.h"
#include "open.h"
#include "due.h"
#include "idxio.h"
#include "bcompat.h"

int
do_ui(unit *ftnunit, XINT *number, register char *ptr, ftnlen len)
{
   register char  *recp;
   register XINT    i = *number * len;

   if (!(ftnunit->uwrt & WR_OP)) {
      recp = ftnunit->f77fio_buf + ftnunit->f77recpos;
      if ((ftnunit->f77recpos += i) > ftnunit->url)
	 err(ftnunit->f77errlist.cierr, 147, "indexed read");
      while (i-- > 0)
	 *ptr++ = *recp++;
   } else {
      if (ftnunit->url > ftnunit->f77fio_size)
	 check_buflen (ftnunit, ftnunit->url);
      recp = ftnunit->f77fio_buf + ftnunit->f77reclen;
      if ((ftnunit->f77reclen += i) > ftnunit->url)
	 err(ftnunit->f77errlist.cierr, 148, "indexed write");
      while (i-- > 0)
	 *recp++ = *ptr++;
   }
   return 0;
}

static int
s_rsue_com (cilist64 *a, unit **fu)
{
   int             n;
   unit           *ftnunit;
   int		f77reclen_32bit;

   if (!f77init)
      f_init ();

   n = c_sue (a, fu);
   ftnunit = *fu;
   if (n) {
     if (n > 0) {
       errret(a->cierr, n, "s_rsue");
     } else {
       errret(a->ciend, n, "s_rsue");
     }
   }

   ftnunit->f77recpos = ftnunit->f77reclen = 0;

#ifdef I90
   if (ftnunit->uaction == WRITEONLY ) 
      errret(ftnunit->f77errlist.cierr,180,"startread");
#endif

   /* 
    * The direct unformatted case, yup, in the sequential unformatted
    * file.  
    */

   if ((ftnunit->uacc == DIRECT) && (ftnunit->ufmt == 0)) {
      if (ftnunit->url != 1) {
	 ftnunit->f77do_unf = do_ud;
	 ftnunit->f77reclen = ftnunit->url;
      } else {
	 /* For 'SYSTEM' file set a very large MAX_INT value for record
	 length so it cannot be exceeded
	 */
#if (_MIPS_SIM == _MIPS_SIM_ABI64)
	 ftnunit->f77reclen = LONGLONG_MAX;
#else
	 ftnunit->f77reclen = LONG_MAX;
#endif
	 ftnunit->f77do_unf = do_ud;
      }
      _fio_seq_pos( ftnunit->ufd, ftnunit );
      ftnunit->uwrt &= ~WR_OP;
      return (0);
   } else {
      if (ftnunit->uwrt & WR_OP)
	 (void) f77nowreading (ftnunit);
   }

   /* The normal case. */

   if (ftnunit->uacc == KEYED) {
      ftnunit->f77do_unf = do_ui;
      ftnunit->f77idxlist.cimatch = a->cimatch;
      ftnunit->f77idxlist.cikeytype = a->cikeytype;
      ftnunit->f77idxlist.cikeyval.cicharval = a->cikeyval.cicharval;
      ftnunit->f77idxlist.cikeyid = a->cikeyid;
      ftnunit->f77idxlist.cinml = a->cinml;
      ftnunit->f77idxlist.cikeyvallen = a->cikeyvallen;
      if (n = idxread(ftnunit)) {
        if (n > 0) {
          errret(a->cierr, n, "s_rsue");
        } else {
          errret(a->ciend, n, "s_rsue");
	}
      }
   } else if (ftnunit->url != 1) {
      ftnunit->f77do_unf = do_us;
      if (ftnunit->uerror)
	 unf_position (ftnunit->ufd, ftnunit);
      if (fread ((char *) &f77reclen_32bit, sizeof (int), 1, ftnunit->ufd) != 1) {
	 if (feof (ftnunit->ufd)) {
	    ftnunit->uend = 1;
	    errret(a->ciend, EOF, "start");
	 }
	 clearerr(ftnunit->ufd);
	 errret(a->cierr, errno, "start");
      }
      ftnunit->f77reclen = f77reclen_32bit;
   } else {
      ftnunit->f77reclen = INT_MAX;
      ftnunit->f77do_unf = do_ud;
   }
   return (0);
}

int
s_rsue (cilist *a)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_rsue_com(&dst, &f77curunit);
}

int
s_rsue_mp (cilist *a, unit **fu)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_rsue_com(&dst, fu);
}


int
s_rsue64 (cilist64 *a)
{
    return( s_rsue_com( a, &f77curunit) );
}

int
s_rsue64_mp (cilist64 *a, unit **fu)
{
    return( s_rsue_com( a, fu) );
}



int
wsue (cilist64 *a, unit **fu)
{
   int             n;
   unit *ftnunit;

   if (!f77init)
      f_init ();
   if (n = c_sue (a, fu))
      return n;
   ftnunit = *fu;
   (void) f77nowwriting( ftnunit );
   ftnunit->f77reclen = 0;
   /*
   if (ftnunit->f77fio_buf == NULL)
      ftnunit->f77fio_buf = malloc (ftnunit->f77fio_size = FIO_ALLOC);
   */
   return (0);
}

static int
s_wsue_com (cilist64 *a, unit **fu)
{
   unit		  *ftnunit;
   int             n;

   n = wsue(a, fu);
   ftnunit = *fu;
   if (n) {
     errret(a->cierr, n, "s_wsue");
   }
   if (ftnunit->uacc == KEYED) {
      ftnunit->f77idxlist.cimatch = a->cimatch;
      ftnunit->f77idxlist.cikeytype = a->cikeytype;
      ftnunit->f77idxlist.cikeyval.cicharval = a->cikeyval.cicharval;
      ftnunit->f77idxlist.cikeyid = a->cikeyid;
      ftnunit->f77idxlist.cinml = a->cinml;
      ftnunit->f77idxlist.cikeyvallen = a->cikeyvallen;
      ftnunit->f77do_unf = do_ui;
   }
   else {
      if (ftnunit->uacc == DIRECT) {
	 ftnunit->f77recpos = 0;
	 ftnunit->f77do_unf = do_ud;
	 _fio_seq_pos( ftnunit->ufd, ftnunit );
      } else {
         if (ftnunit->uwrt != WR_READY && f77nowwriting (ftnunit))
	    errret(a->cierr, 160, "startwrt");
	 est_reclen = ftnunit->f77reclen = 0;
	 ftnunit->overflowed = 0;
	 ftnunit->f77recpos = 4;
	 ftnunit->f77do_unf = do_us;
	 if (ftnunit->uerror)
	    unf_position (ftnunit->ufd, ftnunit);
      }
   }
   return 0;
}

int
s_wsue (cilist *a)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_wsue_com(&dst, &f77curunit);
}

int
s_wsue_mp (cilist *a, unit **fu)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_wsue_com(&dst, fu);
}


int
s_wsue64 (cilist64 *a)
{
    return( s_wsue_com( a, &f77curunit) );
}

int
s_wsue64_mp (cilist64 *a, unit **fu)
{
    return( s_wsue_com( a, fu) );
}


int
c_sue (cilist64 *a, unit **fu)
{
   unit *ftnunit;
   
   if ((ftnunit = map_luno (a->ciunit)) == NULL)
      errret(a->cierr, 101, "startio");
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   *fu = ftnunit;
   if (ftnunit->uconn <= 0 && fk_open (SEQ, UNF, a->ciunit)) {
      ftnunit->uconn = 0;
      errret(a->cierr, 114, "sue");
   }
   ftnunit->f77errlist.cierr = a->cierr;
   ftnunit->f77errlist.ciend = a->ciend;
   ftnunit->f77errlist.cieor = a->cieor;
   ftnunit->f77errlist.cisize = a->cisize;
   ftnunit->f77errlist.iciunit = 0; 
   if (ftnunit->ufmt > 0) {
      if ((ftnunit->ufd == stdin || ftnunit->ufd == stdout ||
	ftnunit->ufd == stderr) && ftnunit->useek)
	/* these guys can be redirected so it might not be an error,
	** let's assume it is correct here.   If there is any error
	** it can be caught later
	*/
	    ftnunit->ufmt = 1;
      else
        errret(a->cierr, 103, "sue");
   }
   if (!ftnunit->useek && ftnunit->uacc == SEQUENTIAL)
      errret(a->cierr, 103, "sue");
   return (0);
}

int
e_rsue (void)
{
    return( e_rsue_mp( &f77curunit ) );
}

int
e_rsue_mp (unit **fu)
{
   unit		*ftnunit = *fu;
   int n;
   if (ftnunit->uacc != KEYED && ftnunit->url != 1) {
      XINT             nleft = ftnunit->f77reclen - ftnunit->f77recpos;

      if ((ftnunit->uacc == DIRECT) && (ftnunit->ufmt == 0)) {
	 return( e_rdue_mp (fu) );
      } else if (ftnunit->uacc == DIRECT) {
	 if (nleft > 0) {
	    if (nleft <= ftnunit->f77fio_size && nleft < 1000) {
	       fread (ftnunit->f77fio_buf, nleft, 1, ftnunit->ufd);
	    } else {
	       (void) fseek (ftnunit->ufd, nleft, 1);
	    }
	 }
      } else {
	 if (nleft + sizeof (int) <= ftnunit->f77fio_size && nleft < 1000) {
	    fread (ftnunit->f77fio_buf, nleft + sizeof (int), 1, ftnunit->ufd);
	 } else {
	    (void) fseek (ftnunit->ufd, (long) (nleft + sizeof (int)), 1);
	 }
      }
      if (ferror (ftnunit->ufd))
	 errret(ftnunit->f77errlist.cierr, errno, "sue");
   }
   ftnunit->lock_unit = 0;
   return (0);
}


/* THIS NEEDS ADJUSTING for >2Gb records - CALVIN */
int
unf_position (FILE *fd, unit *ftnunit)
{
/* Last I/O has an error, must make sure that the file
pointer is positioned at the right place, i.e. must be
between two unformatted records */
   ftnll             pos = FTELL(fd);
   /* Use local 'reclen' instead of global 'ftnunit->f77reclen' to avoid
   side effect
   */
   int reclen_short;

   if (ftnunit->uerror == EOF) {ftnunit->uerror = 0; return(0);}
   ftnunit->uerror = 0;
   fseek (fd, 0, 0);
   while (FTELL (fd) < pos) {
      /* need to change this for > 2gb records DLAI */
      fread ((char *) &reclen_short, sizeof (int), 1, fd);
      fseek (fd, (long) (reclen_short + sizeof (int)), SEEK_CUR);
   }
   return (0);
}

#pragma weak e_rsue64 = e_rsue		/* extern int e_rsue64(void); */
#pragma weak e_rsue64_mp = e_rsue_mp	/* extern int e_rsue64_mp(unit**); */
