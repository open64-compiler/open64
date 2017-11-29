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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/idxio.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

#include <isam.h>
#include <mutex.h>
#include <cmplrs/fio.h>
#include <cmplrs/f_errno.h>
#include "fmt.h"
#include "idxio.h"
#include "err.h"
#include "wsfe.h"
#include "lio.h"
#include "sue.h"
#include "open.h"
#include "uio.h"
#include "bcompat.h"
#include "util.h"

/* These ought to be defined in isam.h, they're here temporarily. */
extern int isread(int, char *, int);
extern int isstart(int, struct keydesc *, int, char *, int);
extern int isrewcurr(int, char *);
extern int isrelease(int);
extern int isbuild(char *, int, struct keydesc *, int);
extern int isaddindex(int, struct keydesc *);
extern int isclose(int);
extern int isopen(char *, int);
extern int isindexinfo(int, void *, int);
extern int iserase(char *);
extern int isdelcurr(int);
extern int iswrite(int, char *);
extern int iscleanup(void);
extern void stlong(ftnint, char *);
extern int iserrno;



#define KEYOFF(x)  keys[x].e1
#define KEYLEN(x)  keys[x].e2 - keys[x].e1 + 1
#define KEYTYPE(x) keys[x].dt
#define KEYEND(x)  keys[x].e2

#define SUCCESS 0

#define ONEKEY  1
#define UNITKEY 2
#define NOMATCH 3

Keyspec        *keys;
struct keydesc  onekey ={ISNODUPS, 1};

int
idxopen (unit *ftnunit, char *name, int create, flag idxerr)
{
   struct dictinfo info;
   int             mode =(ftnunit->ureadonly ? ISINPUT : ISINOUT) +
   (ftnunit->ushared ? ISAUTOLOCK : ISEXCLLOCK);
   register int    i;

   keys = ftnunit->ukeys;
   if (create) {
      if (ftnunit->url == 0)
	 err (idxerr, 153, "indexed open");
      if (ftnunit->unkeys == 0 || keys == NULL)
	 err (idxerr, 161, "indexed open");
      onekey.k_flags = ISNODUPS;
      KEYOFF (0)--;
      KEYEND (0)--;
      dokey (0, ONEKEY);
      if (ftnunit->unkeys == 1) {
	/* DLAI: need to change isbuild 2nd arg to XINT */
	 if ((ftnunit->isfd = isbuild (name, ftnunit->url, &onekey, mode)) < SUCCESS)
	    ierr (idxerr, iserrno, "indexed open");
      } else {
	 if ((ftnunit->isfd =
	      isbuild (name, ftnunit->url, &onekey, ISINOUT + ISEXCLLOCK)) < SUCCESS)
	    ierr (idxerr, iserrno, "indexed open");
	 onekey.k_flags = ISDUPS;
	 for (i = 1; i < ftnunit->unkeys; i++) {
	    KEYOFF (i)--;
	    KEYEND (i)--;
	    dokey (i, ONEKEY);
	    if (isaddindex (ftnunit->isfd, &onekey) < SUCCESS)
	       ierr (idxerr, iserrno, "indexed open");
	 }
	 if (ftnunit->ushared) {
	    if (isclose (ftnunit->isfd) < SUCCESS)
	       ierr (idxerr, iserrno, "indexed open");
	    if ((ftnunit->isfd = isopen (name, mode)) < SUCCESS)
	       ierr (idxerr, iserrno, "indexed open");
	 }
      }
   } else {
      if ((ftnunit->isfd = isopen (name, mode)) < SUCCESS)
	 ierr (idxerr, iserrno, "indexed open");
      if (isindexinfo (ftnunit->isfd, &info, 0) < SUCCESS)
	 ierr (idxerr, iserrno, "indexed open");
      if (ftnunit->unkeys != info.di_nkeys) {
	 if (ftnunit->unkeys) {
	    err (idxerr, 148, "indexed open");
	 } else
	    ftnunit->unkeys = info.di_nkeys;
      }
      if (!keys)
	 keys = (Keyspec *) malloc (sizeof (Keyspec) * info.di_nkeys);
      for (i = 0; i < info.di_nkeys; i++) {
	 if (isindexinfo (ftnunit->isfd, &onekey, i + 1) < SUCCESS)
	    ierr (idxerr, iserrno, "indexed open");
/* LHL 5/4/89
 * To fix bug 4428, problem about trying to open an existing indexed file.
 * This is put here because when the indexed file is created, the keys
 * are being stored like this.  Refer to the above code.
 */
	 KEYOFF (i)--;
	 KEYEND (i)--;
/* end of fix */
	 if (ftnunit->ukeys && dokey (i, NOMATCH)) {
	    err (idxerr, 148, "indexed open");
	 } else
	    dokey (i, UNITKEY);
      }
      ftnunit->url = info.di_recsize;
      ftnunit->ukeys = keys;
   }
   return SUCCESS;
}

int
idxclose (unit *ftnunit, flag idxerr)
{
   if (isclose (ftnunit->isfd) < SUCCESS)
      ierr (idxerr, iserrno, "indexed close");
   if (ftnunit->ufnm) {
      if (ftnunit->udisp & DELETE)
	 if (iserase (ftnunit->ufnm) < SUCCESS)
	    ierr (idxerr, iserrno, "indexed close");
      free (ftnunit->ufnm);
      ftnunit->ufnm = NULL;
   }
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
   ftnunit->ukeyid = -1;
   return SUCCESS;
}

int
idxread (unit *ftnunit)
{
   register int    i;
   register char  *keyval, *field;
   int             newkeyid = 0, mode = ftnunit->f77idxlist.cimatch;

   keys = ftnunit->ukeys;
   ftnunit->f77recend = i = ftnunit->url;
   if (ftnunit->url > ftnunit->f77fio_size)
      check_buflen (ftnunit, ftnunit->url);
   while (i-- > 0)
      ftnunit->f77fio_buf[i] = '\0';
   if (ftnunit->f77idxlist.cikeyid >= 0 && ftnunit->ukeyid != ftnunit->f77idxlist.cikeyid) {
      ftnunit->ukeyid = ftnunit->f77idxlist.cikeyid;
      newkeyid = 1;
   }
/* 8/23/89 fix bug 4847 */
   else if (ftnunit->ukeyid < 0) {
      ftnunit->ukeyid = ftnunit->f77idxlist.cikeyid >= 0 ? ftnunit->f77idxlist.cikeyid : 0;
      newkeyid = 1;
   }
   if (mode) {
      field = ftnunit->f77fio_buf + KEYOFF (ftnunit->ukeyid);
      if (KEYTYPE (ftnunit->ukeyid) != ftnunit->f77idxlist.cikeytype)
	 err (ftnunit->f77errlist.cierr, 154, "indexed read");
      if (KEYTYPE (ftnunit->ukeyid) == CHARTYPE) {
	 keyval = ftnunit->f77idxlist.cikeyval.cicharval;
/* fix bug 4779 */
	 if ((i = ftnunit->f77idxlist.cikeyvallen) > KEYLEN (ftnunit->ukeyid))
	    err (ftnunit->f77errlist.cierr, 155, "indexed read");
	 /* For Fortran, there's no such thing as null terminators for
	 the string values, so use the string len value only 
	 while (*keyval && i-- > 0)
	    *field++ = *keyval++;
	 while (i-- > 0)
	    *field++ = ' ';
	*/
	 while (i-- > 0)
	    *field++ = *keyval++;
      } else
	 stlong (ftnunit->f77idxlist.cikeyval.ciintval, field);
   } else
      /* If the last read operation was locked try readign the same record
      again
      mode = (newkeyid ? ISFIRST : ISNEXT);
      */
      mode = (newkeyid ? ISFIRST : ftnunit->uerror == F_ERLOCKED ? ISCURR : ISNEXT);
      ftnunit->uerror = 0;	/* clear error for new operation */
   if (newkeyid) {
      dokey (ftnunit->ukeyid, ONEKEY);
      if (isstart (ftnunit->isfd, &onekey, ftnunit->url, ftnunit->f77fio_buf, mode)
	  < SUCCESS)
	 ierr (ftnunit->f77errlist.cierr, iserrno, "indexed read");
   }
   if (isread (ftnunit->isfd, ftnunit->f77fio_buf, mode) < SUCCESS)
      if (iserrno == EENDFILE) {
	 ftnunit->uend = 1;
	 err (ftnunit->f77errlist.ciend, EOF, "indexed read");
      } else
	 ierr (ftnunit->f77errlist.cierr, iserrno, "indexed read");
   /* When the read is successful, make sure the EOF flag is not set */
   ftnunit->uend = 0;
   return SUCCESS;
}

void
s_idxwrite (unit *ftnunit)
{
/* Fix BN 10498.
 * Check to ensure that the f77fio_buf is equal to the record length
 * ftnunit->url. Since this was not being done previously, f77fio_buf
 * was getting written with more characters then malloced which inturn
 * caused svr4 realloc and malloc to fail becuase these routines keep
 * size info in the first word of the block. The next  block was getting over
 * written.
 * ---ravi---1/15/92
 */
   check_buflen (ftnunit, ftnunit->url);
   if (ftnunit->ufmt)
      while (ftnunit->f77recpos++ < ftnunit->url)
	 ftnunit->f77fio_buf[ftnunit->f77recpos] = ' ';
   else
      while (ftnunit->f77reclen++ < ftnunit->url)
	 ftnunit->f77fio_buf[ftnunit->f77reclen] = '\0';
   ftnunit->f77fio_buf[ftnunit->url] = '\0';
}

int
idxwrite (unit *ftnunit)
{
   s_idxwrite (ftnunit);
   if (iswrite (ftnunit->isfd, ftnunit->f77fio_buf) < SUCCESS)
      ierr (ftnunit->f77errlist.cierr, iserrno, "indexed write");
   return SUCCESS;
}

int
idxrewrite (unit *ftnunit)
{
   s_idxwrite (ftnunit);
   if (isrewcurr (ftnunit->isfd, ftnunit->f77fio_buf) < SUCCESS)
      ierr (ftnunit->f77errlist.cierr, iserrno, "indexed rewrite");
   return 0;
}

int
dokey(int keyid, int mode)
{
   switch (mode) {
   case NOMATCH:
      return KEYOFF (keyid) != onekey.k_start ||
	 KEYLEN (keyid) != onekey.k_leng ||
	 KEYTYPE (keyid) != onekey.k_type;

   case ONEKEY:
      onekey.k_start = KEYOFF (keyid);
      onekey.k_leng = (short) (KEYLEN (keyid));
      onekey.k_type = KEYTYPE (keyid);
      break;

   case UNITKEY:
      KEYOFF (keyid) = onekey.k_start;
      KEYEND (keyid) = (short) (onekey.k_start + onekey.k_leng - 1);
      KEYTYPE (keyid) = onekey.k_type;
      break;
   }
   return 0; /* return value is ignored */
}

static int f_del_com (alist *a, int lock);
static int f_unl_com (alist *a, int lock);

int
f_del (alist *a)
{
  return( f_del_com( a, 0 ) );
}

int
f_del_mp (alist *a)
{
  return( f_del_com( a, 1 ) );
}

static int
f_del_com (alist *a, int lock)
{
   unit           *ftnunit = find_luno(a->aunit);

   if (ftnunit == NULL)
      err (a->aerr, 101, "delete");
   while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   if (ftnunit->uconn > 0) {
      if (ftnunit->uacc != KEYED)
	 errret(a->aerr, 163, "delete");
      if (isdelcurr (ftnunit->isfd) < SUCCESS)
	 ierrret(a->aerr, iserrno, "delete");
   }
   else
      err (a->aerr, 101, "delete");
   if (lock) ftnunit->lock_unit = 0;
   return 0;
}

int
f_unl (alist *a)
{
   return( f_unl_com( a, 0 ) );
}

int
f_unl_mp (alist *a)
{
   return( f_unl_com( a, 1 ) );
}

static int
f_unl_com (alist *a, int lock)
{
   unit           *ftnunit = find_luno(a->aunit);

   if (ftnunit == NULL)
      err (a->aerr, 101, "unlock");
   while (lock && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   if (ftnunit->uconn > 0) {
      if (ftnunit->uacc != KEYED)
	 errret(a->aerr, 163, "unlock");
      if (isrelease (ftnunit->isfd) < SUCCESS)
	 ierrret(a->aerr, iserrno, "unlock");
   }
   else
      err (a->aerr, 101, "delete");
   if (lock) ftnunit->lock_unit = 0;
   return 0;
}

int
s_xsue (cilist *a)
{
   return( s_xsue_mp( a, &f77curunit ) );
}


int
s_xsue64 (cilist64 *a)
{
   return( s_xsue64_mp( a, &f77curunit ) );
}

int
s_xsue_mp (cilist *a, unit **fu)
{
    cilist64 a64;
    get_cilist64( &a64, a );
    return( s_xsue64_mp( &a64, fu ) );
}

int
s_xsue64_mp (cilist64 *a, unit **fu)
{
   unit *ftnunit;
   (void) wsue(a, fu);
   ftnunit = *fu;

   ftnunit->f77idxlist.cimatch = a->cimatch;
   ftnunit->f77idxlist.cikeytype = a->cikeytype;
   ftnunit->f77idxlist.cikeyval.cicharval = a->cikeyval.cicharval;
   ftnunit->f77idxlist.cikeyid = a->cikeyid;
   ftnunit->f77idxlist.cinml = a->cinml;
   ftnunit->f77idxlist.cikeyvallen = a->cikeyvallen;
   if (ftnunit->uacc != KEYED)
      err (a->cierr, 162, "rewrite");
   return 0;
}

#pragma weak e_xsue64 = e_xsue

int
e_xsue (void)
{
   return( e_xsue_mp( &f77curunit) );
}
	
#pragma weak e_xsue64_mp = e_xsue_mp

int
e_xsue_mp (unit **fu)
{
/* fix bug 4944 */
   int             n = idxrewrite(*fu);

   (*fu)->lock_unit = 0;
   return (n == 1 ? 0 : n);
}

int
s_xsle (cilist *a)
{
   return( s_xsle_mp( a, &f77curunit ) );
}

int
s_xsle64 (cilist64 *a)
{
   return( s_xsle64_mp( a, &f77curunit ) );
}

int
s_xsle_mp (cilist *a, unit **fu)
{
    cilist64 a64;
    get_cilist64( &a64, a );
    return( s_xsle64_mp( &a64, fu ) );
}

int
s_xsle64_mp (cilist64 *a, unit **fu)
{
   int             n = s_wsle64_mp(a, fu);
   unit		*ftnunit = *fu;

   if (n) {
      return n;
   }
   ftnunit->f77idxlist.cimatch = a->cimatch;
   ftnunit->f77idxlist.cikeytype = a->cikeytype;
   ftnunit->f77idxlist.cikeyval.cicharval = a->cikeyval.cicharval;
   ftnunit->f77idxlist.cikeyid = a->cikeyid;
   ftnunit->f77idxlist.cinml = a->cinml;
   ftnunit->f77idxlist.cikeyvallen = a->cikeyvallen;
   if (ftnunit->uacc != KEYED)
      errret(a->cierr, 162, "rewrite");
   ftnunit->dowrite = idxrewrite;
   return (0);
}

int
s_xsfe (cilist *a)
{
   return( s_xsfe_mp( a, &f77curunit ) );
}


int
s_xsfe64 (cilist64 *a)
{
   return( s_xsfe64_mp( a, &f77curunit ) );
}

int
s_xsfe_mp (cilist *a, unit **fu)
{
    cilist64 a64;
    get_cilist64( &a64, a );
    return( s_xsfe64_mp( &a64, fu ) );
}

int
s_xsfe64_mp (cilist64 *a, unit **fu)
{
   int             n = wsfe(a, fu, 0);
   unit		  *ftnunit;
   
   ftnunit = *fu;
   ftnunit->f77idxlist.cimatch = a->cimatch;
   ftnunit->f77idxlist.cikeytype = a->cikeytype;
   ftnunit->f77idxlist.cikeyval.cicharval = a->cikeyval.cicharval;
   ftnunit->f77idxlist.cikeyid = a->cikeyid;
   ftnunit->f77idxlist.cinml = a->cinml;
   ftnunit->f77idxlist.cikeyvallen = a->cikeyvallen;
   if (n) {
      return n;
   }
   if (ftnunit->uacc != KEYED)
      errret(a->cierr, 162, "rewrite");
   ftnunit->dowrite = idxrewrite;
   return (0);
}

#pragma weak f_del64 = f_del
#pragma weak f_del64_mp = f_del_mp
#pragma weak f_unl64 = f_unl
#pragma weak f_unl64_mp = f_unl_mp
