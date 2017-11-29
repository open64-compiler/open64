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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/uio.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
#include <cmplrs/fio.h>
#include <limits.h>
#include "vmsflags.h"
#include "fmt.h"
#include "iomode.h"
#include "uio.h"
#include "err.h"
#include "fio_direct_io.h"
#include "cmplrs/f_errno.h"
#include <string.h>
int             est_reclen;	/* need to change if > 2Gb records CALVIN */

#define		FORWARD		0
#define		BACKWARD	1
#define MIN(a,b)	(((a) < (b)) ? a : b)

static int do_uio_com (XINT *number, char *ptr, unit **fu, ftnlen len);

int
do_us (unit *ftnunit, XINT *number, char *ptr, ftnlen len)
{
   if (!(ftnunit->uwrt & WR_OP)) {
      XINT             nread = *number * len;

      ftnunit->f77recpos += nread;
      if (ftnunit->f77reclen == 1 && f77vms_flag_[VMS_EF]) {
	 /* VMS endfile record */
	 (void) fread (ptr, 1, 1, ftnunit->ufd);
	 if (*ptr == '\032') {
	    (void) fseek (ftnunit->ufd, sizeof (int), 1);
	    return( EOF );	/* Endfile record */
	 } else if (ftnunit->f77recpos > 1)
	    errret(ftnunit->f77errlist.cierr, 110, "eof/uio");
	 return (0);
      }
      if (ftnunit->f77recpos > ftnunit->f77reclen) {
	 ftnunit->f77recpos -= nread;
	 (void) fread (ptr, (int) ftnunit->f77reclen - ftnunit->f77recpos, 1, ftnunit->ufd);
	 errret(ftnunit->f77errlist.cierr, 110, "eof/uio");
      }
      (void) fread (ptr, (int) nread, 1, ftnunit->ufd);
      return (0);
   } else {

/* 7/18/90 MNH  REPLACE FWRITE WITH MEMCPY -- FWRITE BEGAN AT BYTE #5 TO */
/* ALLOW SPACE FOR EVENTUAL FRWITE OF "f77reclen" TO BEGINNING OF FILE */
      XINT             n = *number * len;
      int             seekdone = 0;

      if (ftnunit->f77recpos + n > ftnunit->f77fio_size || n >= BUFSIZ || est_reclen) {
	 if (!ftnunit->overflowed) {
	    ftnunit->overflowed = 1;
	    if (!est_reclen) {
	       if (ftnunit->f77recpos == 4) {
		  fseek (ftnunit->ufd, 4, 1);
		  seekdone = 1;
		  ftnunit->f77recpos = 0;
	       }
	    } else
	       *(int *) ftnunit->f77fio_buf = est_reclen;
	 }
	 if (ftnunit->f77recpos) {
	    if (fwrite (ftnunit->f77fio_buf, ftnunit->f77recpos, 1, ftnunit->ufd) != 1)
	       errret(ftnunit->f77errlist.cierr, errno, "system write error");
	    ftnunit->f77recpos = 0;
	 }
	 if (n >= BUFSIZ) {
	    /* for large user data it's preferable to flush the
	     * system buffer to disk so that it won't have to copy
	     * the user array, which could be several megs, to the
	     * system buffer before writing it out */
	    if (!seekdone)
	       fseek (ftnunit->ufd, 0, 1);
	    if (fwrite (ptr, n, 1, ftnunit->ufd) != 1)
	       errret(ftnunit->f77errlist.cierr, errno, "system write error");
	    ftnunit->f77reclen += n;
	    return (0);
	 }
      }
      if (!est_reclen) {
	 if (ftnunit->f77recpos + n > ftnunit->f77fio_size)
             check_buflen( ftnunit, ftnunit->f77recpos + n );
	 memcpy (ftnunit->f77fio_buf + ftnunit->f77recpos, ptr, n);
	 ftnunit->f77recpos += n;
      } else if (fwrite (ptr, n, 1, ftnunit->ufd) != 1) {
	 ftnunit->f77recpos = 0;
	 errret(ftnunit->f77errlist.cierr, errno, "system write error");
      }
      ftnunit->f77reclen += n;
      return (0);
   }
}


int
s_usrecsize (int reclen)
{
   est_reclen = reclen;	/* CALVIN - > 2Gb */
   return(0);
}


int
do_uio64_mp_1dim( char *ptr, flex *do_idx,
		 XINT *lb, XINT *ub,
		 XINT *step, unit **fu,
		 ftnlen len, ftnlen idxlen)
{
   return(do_uio_1dim_work(ptr, do_idx, *lb, *ub, *step, fu, len, idxlen));
}


static int
do_uio_1dim_work (	char *ptr, flex *do_idx, 
		XINT lb, XINT ub, 
		XINT step, unit **fu,
		ftnlen len, ftnlen idxlen)
/* 
** This function carries out the I/O operation on a single-dimension
** implied-DO loop in the form (ARR(I),I=N,M,L).  It takes these 
** arguments:
**
**	ptr	: address of the first array element involved in the I/O
**		  in the example above it is the address of ARR(N)
**	do_idx  : the implied-do variable
**	lb	: the loop lower bound
**	ub	: the loop upper bound
**	step	: the implied-do loop step size which is L in this case.
**	len	: length of each array element
**	idxlen  : length of the implied-do variable
*/
{
   XINT nelem;
   int ierr;
   char *lastptr, *nptr;
   char *f77fio_buf;
   static char *f77fio_buf_com;
   static int size = FIO_ALLOC*4;
   unit *ftnunit = *fu;
   int mp_mode = (*fu != f77curunit);
      
   lastptr = ptr + (ub - 1) * len;
   ptr += (lb - 1) * len;
   if (step == 1) {
   /* consecutive elements */
      if ((nelem = ub - lb + 1) > 0) {
	  if ( ierr = do_uio_com( &nelem, ptr, fu, len ) ) {
	      set_do_idx( do_idx, idxlen, (ftnll) ((((*fu)->f77reclen - (*fu)->f77recpos) / len) + 1) );
	      return( ierr );
	  }
	  set_do_idx( do_idx, idxlen, (ftnll) (ub + 1) );
      }
      /* if last element <= first: dothing */
      return( 0 );
   }
   
   if (step >= 0) {
     if (ftnunit->uwrt & WR_OP) {
       if (!mp_mode) {
         /* Non MP mode: use a single buffer */
         if (!f77fio_buf_com) {
           if (!(f77fio_buf_com = malloc( size )))
	    err(ftnunit->f77errlist.cierr, 113, "malloc");
         }
         f77fio_buf = f77fio_buf_com;
       } else {
         if (!(ftnunit->unf_buf = malloc( size )))
           err(ftnunit->f77errlist.cierr, 113, "malloc");
	 f77fio_buf = ftnunit->unf_buf;
       }
       for (nptr = ptr; nptr <= lastptr; nptr += nelem*len*step) {
	 nelem = gather_in_f77fio_buf( f77fio_buf, size, nptr, lastptr, step, len, FORWARD );
	 if (ierr = do_uio_com( &nelem, f77fio_buf, fu, len ) ) {
	    set_do_idx( do_idx, idxlen, (ftnll) (((ftnunit->f77reclen - ftnunit->f77recpos) / len) + 1) );
	    return( ierr );
	 }
       }
     }
     else {
       for (nptr = ptr; nptr <= lastptr; nptr += nelem*len*step) {
	 nelem = MIN( ftnunit->f77fio_size/len, (lastptr-nptr) / (len*step) + 1);
	 if (ierr = do_uio_com( &nelem, ftnunit->f77fio_buf, fu, len ) ) {
	    set_do_idx( do_idx, idxlen, (ftnll) (((ftnunit->f77reclen - ftnunit->f77recpos) / len) + 1) );
	    return( ierr );
	 }
	 scatter_from_f77fio_buf( ftnunit, nptr, lastptr, step, len, FORWARD );
       }
     }
   }
   else {
     if (ftnunit->uwrt & WR_OP) {
       if (!mp_mode) {
         /* Non MP mode: use a single buffer */
         if (!f77fio_buf_com) {
           if (!(f77fio_buf_com = malloc( size )))
	    err(ftnunit->f77errlist.cierr, 113, "malloc");
         }
         f77fio_buf = f77fio_buf_com;
       } else {
         if (!(ftnunit->unf_buf = malloc( size )))
           err(ftnunit->f77errlist.cierr, 113, "malloc");
	 f77fio_buf = ftnunit->unf_buf;
       }
       for (nptr = ptr; nptr >= lastptr; nptr += nelem*step*len) {
	   nelem = gather_in_f77fio_buf( f77fio_buf, size, nptr, lastptr, step, len, BACKWARD );
	   if (ierr = do_uio_com( &nelem, f77fio_buf, fu, len ) ) {
	     set_do_idx( do_idx, idxlen, (ftnll) (((ftnunit->f77reclen - ftnunit->f77recpos) / len) + 1) );
	     return( ierr );
	   }
       }
     } 
     else {
       for (nptr = ptr; nptr >= lastptr; nptr += nelem*step*len) {
         nelem = MIN( ftnunit->f77fio_size/len, (nptr-lastptr) / (len*step) + 1);
         if (ierr = do_uio_com( &nelem, ftnunit->f77fio_buf, fu, len ) ) {
            set_do_idx( do_idx, idxlen, (ftnll) (((ftnunit->f77reclen - ftnunit->f77recpos) / len) + 1) );
            return( ierr );
         }
         scatter_from_f77fio_buf( ftnunit, nptr, lastptr, step, len, BACKWARD );
       }
     }
   }
   set_do_idx( do_idx, idxlen, (ftnll) (lb + ((ub - lb) / step + 1) * step) );
   return(0);
}

int
do_uio_1dim (	char *ptr, flex *do_idx, 
		ftnint *lb, ftnint *ub, 
		ftnint *step, ftnlen len, 
		ftnlen idxlen)
{
 return(do_uio_1dim_work(ptr, do_idx, *lb, *ub, *step, &f77curunit, len, idxlen));
}

/* do_uio64_1dim is identical to do_uio_1dim in 32 bit mode */
int
do_uio64_1dim (	char *ptr, flex *do_idx, 
		XINT *lb, XINT *ub, 
		XINT *step, ftnlen len, 
		ftnlen idxlen)
{
 return(do_uio_1dim_work(ptr, do_idx, *lb, *ub, *step, &f77curunit, len, idxlen));
}


void
set_do_idx( flex *idx, ftnlen len, ftnll val )
{
   switch (len) {
      case 1:
	 idx->flbyte = (char) val;
	 return;
      case 2:
	 idx->flshort = (short) val;
	 return;
      case 4:
	 idx->flint = (int) val;
	 return;
      case 8:
	 idx->flll = val;
	 return;
   }
}

static int
do_uio_com (XINT *number, char *ptr, unit **fu, ftnlen len)
{
   if (*number <= 0)
      return (0);

   return (*(*fu)->f77do_unf) (*fu, number, ptr, len);
}

int
do_uio (ftnint *number, char *ptr, ftnlen len)
{
#if (_MIPS_SIM == _MIPS_SIM_ABI64)
    XINT num;
    num = *number;
    return( do_uio_com(&num, ptr,  &f77curunit, len));
#else
    return( do_uio_com( number, ptr,  &f77curunit, len ) );
#endif
}

int
do_uio_mp (ftnint *number, char *ptr, unit **fu, ftnlen len)
{
#if (_MIPS_SIM == _MIPS_SIM_ABI64)
    XINT num;
    num = *number;
    return( do_uio_com(&num, ptr,  fu, len));
#else
    return( do_uio_com( number, ptr,  fu, len ) );
#endif
}


int
do_uio64 (XINT *number, char *ptr, ftnlen len)
{
    return( do_uio_com( number, ptr,  &f77curunit, len ) );
}

int
do_uio64_mp (XINT *number, char *ptr, unit **fu, ftnlen len)
{
    return( do_uio_com( number, ptr,  fu, len ) );
}


int
do_ud (unit *ftnunit, XINT *number, char *ptr, ftnlen len)
{
   XINT             nread = *number * len;
   XINT64             disk_loc; 
     
   if (ftnunit->url != 1) {   /* Normal case.          */
      disk_loc =(ftnunit->uirec - 1) * ftnunit->url + ftnunit->f77recpos;
      ftnunit->f77recpos += nread;
      if (ftnunit->f77recpos > ftnunit->url && ftnunit->url != 1)
         errret(ftnunit->f77errlist.cierr, 110, "eof/uio");
   } else {                      /* Record length of one. */
      disk_loc = ftnunit->uirec;
      ftnunit->uirec += nread;
   }
 

   /* 
    * Read or write the data. 
    */

   if (!(ftnunit->uwrt & WR_OP)) {
      if (-1 == _fio_du_read (ftnunit, ptr, nread, disk_loc, (int) ftnunit->ufd))
	 errret(ftnunit->f77errlist.cierr, errno, "eof/uio");
   } else {
      if (ftnunit->ureadonly)
	 errret( ftnunit->f77errlist.cierr, F_ERREADONLY, "direct unformatted write" );
      if (-1 == _fio_du_write (ftnunit, ptr, nread, disk_loc, (int) ftnunit->ufd))
	 errret(ftnunit->f77errlist.cierr, errno, "system write error");
   }
   return (0);
}

int
check_buflen (unit *ftnunit, XINT n)
{
   if (ftnunit->f77fio_size > n) 
      return(0);
   if (!ftnunit->f77fio_buf) {
      ftnunit->f77fio_size = (n > FIO_ALLOC) ? n : FIO_ALLOC;
      ftnunit->f77fio_buf = malloc (ftnunit->f77fio_size);
   } else {
      ftnunit->f77fio_size = ( n > ftnunit->f77fio_size * 2) ? n : ftnunit->f77fio_size * 2;
      ftnunit->f77fio_buf = realloc (ftnunit->f77fio_buf, ftnunit->f77fio_size);
   }
   if (ftnunit->f77fio_buf == NULL)
      err(ftnunit->f77errlist.cierr, 113, "malloc");
   return (0);
}


/*  ========================================================================  */
/*									      */
/*  The following entries are a minor efficiency improvement in calling       */
/*  the unformatted I/O routines.  In a few cases, they overlap existing      */
/*  entries but are an attempt to clean up the interface.  In truth, the      */
/*  entire I/O interface could use an overhaul.				      */
/*									      */
/*  ========================================================================  */

int
do_uioxa4_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 4 ) );
}

int
do_uioxa4 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 4 ) );
}

int
do_uioxa8_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 8 ) );
}

int
do_uioxa8 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 8 ) );
}

int
do_uioxh1_mp (char *ptr, XINT clen, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, clen ) );
}

int
do_uioxh1 (char *ptr, XINT clen, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, clen ) );
}

int
do_uioxi1_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 1 ) );
}

int
do_uioxi1 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 1 ) );
}

int
do_uioxi2_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 2 ) );
}

int
do_uioxi2 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 2 ) );
}

int
do_uioxi4_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 4 ) );
}

int
do_uioxi4 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 4 ) );
}

int
do_uioxi8_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 8 ) );
}

int
do_uioxi8 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 8 ) );
}

int
do_uioxl1_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 1 ) );
}

int
do_uioxl1 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 1 ) );
}

int
do_uioxl2_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 2 ) );
}

int
do_uioxl2 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 2 ) );
}

int
do_uioxl4_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 4 ) );
}

int
do_uioxl4 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 4 ) );
}

int
do_uioxl8_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 8 ) );
}

int
do_uioxl8 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 8 ) );
}

int
do_uioxr4_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 4 ) );
}

int
do_uioxr4 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 4 ) );
}

int
do_uioxr8_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 8 ) );
}

int
do_uioxr8 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 8 ) );
}

int
do_uioxr16_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 16 ) );
}

int
do_uioxr16 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 16 ) );
}

int
do_uioxc4_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 8 ) );
}

int
do_uioxc4 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 8 ) );
}

int
do_uioxc8_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 16 ) );
}

int
do_uioxc8 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 16 ) );
}

int
do_uioxc16_mp (char *ptr, XINT num, unit **fu)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, fu, 32 ) );
}

int
do_uioxc16 (char *ptr, XINT num)
{
    XINT number = num;
    return( do_uio_com( &number, ptr, &f77curunit, 32 ) );
}

int
do_uioxa4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 4 ) );
}
int
do_uioxa4v (ftnint val)
{
    ftnint value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 4 ) );
}

int
do_uioxa8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 8 ) );
}

int
do_uioxa8v (ftnll val)
{
    ftnll value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_uioxh1v_mp (char val, unit **fu)
{
    char value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 1 ) );
}

int
do_uioxh1v (char val)
{
    char value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 1 ) );
}

int
do_uioxi1v_mp (char val, unit **fu)
{
    char value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 1 ) );
}

int
do_uioxi1v (char val)
{
    char value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 1 ) );
}

int
do_uioxi2v_mp (short val, unit **fu)
{
    short value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 2 ) );
}

int
do_uioxi2v (short val)
{
    short value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 2 ) );
}

int
do_uioxi4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 4 ) );
}

int
do_uioxi4v (ftnint val)
{
    ftnint value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 4 ) );
}

int
do_uioxi8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 8 ) );
}

int
do_uioxi8v (ftnll val)
{
    ftnll value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_uioxl1v_mp (char val, unit **fu)
{
    char value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 1 ) );
}

int
do_uioxl1v (char val)
{
    char value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 1 ) );
}

int
do_uioxl2v_mp (short val, unit **fu)
{
    short value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 2 ) );
}

int
do_uioxl2v (short val)
{
    short value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 2 ) );
}

int
do_uioxl4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 4 ) );
}

int
do_uioxl4v (ftnint val)
{
    ftnint value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 4 ) );
}

int
do_uioxl8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 8 ) );
}

int
do_uioxl8v (ftnll val)
{
    ftnll value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_uioxr4v_mp (float val, unit **fu)
{
    float value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 4 ) );
}

int
do_uioxr4v (float val)
{
    float value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 4 ) );
}

int
do_uioxr8v_mp (double val, unit **fu)
{
    double value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 8 ) );
}

int
do_uioxr8v (double val)
{
    double value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_uioxr16v_mp (long double val, unit **fu)
{
    long double value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, fu, 16 ) );
}

int
do_uioxr16v (long double val)
{
    long double value = val;
    XINT number = 1;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 16 ) );
}

int
do_uioxc4v_mp (float rval, float ival, unit **fu)
{
    float value[2];
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_uio_com( &number, (char *)&value, fu, 8 ) );
}

int
do_uioxc4v (float rval, float ival)
{
    float value[2];
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 8 ) );
}

int
do_uioxc8v_mp (double rval, double ival, unit **fu)
{
    double value[2];
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_uio_com( &number, (char *)&value, fu, 16 ) );
}

int
do_uioxc8v (double rval, double ival)
{
    double value[2];
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 16 ) );
}

int
do_uioxc16v_mp (long double rval, long double ival, unit **fu)
{
    long double value[2];
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_uio_com( &number, (char *)&value, fu, 32 ) );
}

int
do_uioxc16v (long double rval, long double ival)
{
    long double value[2];
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_uio_com( &number, (char *)&value, &f77curunit, 32 ) );
}

/*  ========================================================================  */


int
gather_in_f77fio_buf( 
		char *f77fio_buf, int f77fio_size, 
		char *nptr, char *lastptr, XINT step, 
		ftnlen len, flag direction)
{
   int nelem, i;
   int stepsize = step*len;
   char *ptr;

   nelem = MIN( f77fio_size/len, (lastptr-nptr) / (len*step) + 1); 
   if (len == 4) {
     for (i = 0, ptr = f77fio_buf; i < nelem; i++) {
	*(int *)ptr = *(int *) nptr;
	nptr += stepsize;
	ptr += 4;
     }
   }
   else if (len == 8) {
      for (i = 0, ptr = f77fio_buf; i < nelem; i++) {
	*(double *)ptr = *(double *) nptr;
	nptr += stepsize;
	ptr += 8;
      }
   }
   else if (len == 16) {
      for (i = 0, ptr = f77fio_buf; i < nelem; i++) {
        *(double *)ptr = *(double *) nptr;
	nptr += stepsize;
	ptr += 8;
	*(double *)ptr = *(double *) nptr;
	nptr += stepsize;
	ptr += 8;
      }
   }
   else if (len == 32) {
      for (i = 0, ptr = f77fio_buf; i < nelem; i++) {
	*(double *)ptr = *(double *) nptr;
	nptr += stepsize;
	ptr += 8;
	*(double *)ptr = *(double *) nptr;
	nptr += stepsize;
	ptr += 8;
	*(double *)ptr = *(double *) nptr;
	nptr += stepsize;
	ptr += 8;
	*(double *)ptr = *(double *) nptr;
	nptr += stepsize;
	ptr += 8;
      }
   }
   else if (len == 2) {
      for (i = 0, ptr = f77fio_buf; i < nelem; i++) {
	*(short *)ptr = *(short *) nptr;
	nptr += stepsize;
	ptr += 2;
      }
   }
   else {
      for (i = 0, ptr = f77fio_buf; i < nelem; i++) {
	*ptr = *nptr;
	nptr += stepsize;
	ptr += 1;
      }
   }
   return( nelem );
}

int
scatter_from_f77fio_buf( 
		unit *ftnunit, char *nptr, char *lastptr, 
		XINT step, ftnlen len, flag direction)
{
   int nelem, i;
   int stepsize = step*len;
   char *ptr;

   if (direction == FORWARD)
      nelem = MIN( ftnunit->f77fio_size/len, (lastptr-nptr) / (len*step) + 1); 
   else
      nelem = MIN( ftnunit->f77fio_size/len, (nptr-lastptr) / (len*step) + 1);
   if (len == 4) {
     for (i = 0, ptr = ftnunit->f77fio_buf; i < nelem; i++) {
	*(int *)nptr = *(int *)ptr;
	nptr += stepsize;
	ptr += 4;
     }
   }
   else if (len == 8) {
      for (i = 0, ptr = ftnunit->f77fio_buf; i < nelem; i++) {
	*(double *)nptr = *(double *)ptr;
	nptr += stepsize;
	ptr += 8;
      }
   }
   else if (len == 16) {
      for (i = 0, ptr = ftnunit->f77fio_buf; i < nelem; i++) {
        *(double *)nptr = *(double *)ptr;
	nptr += stepsize;
	ptr += 8;
	*(double *)nptr = *(double *)ptr;
	nptr += stepsize;
	ptr += 8;
     }
   }
   else if (len == 32) {
      for (i = 0, ptr = ftnunit->f77fio_buf; i < nelem; i++) {
	*(double *)nptr = *(double *)ptr;
	nptr += stepsize;
	ptr += 8;
	*(double *)nptr = *(double *)ptr;
	nptr += stepsize;
	ptr += 8;
	*(double *)nptr = *(double *)ptr;
	nptr += stepsize;
	ptr += 8;
	*(double *)nptr = *(double *)ptr;
	nptr += stepsize;
	ptr += 8;
      }
   }
   else if (len == 2) {
      for (i = 0, ptr = ftnunit->f77fio_buf; i < nelem; i++) {
	*(short *)nptr = *(short *)ptr;
	nptr += stepsize;
	ptr += 2;
      }
   }
   else {
      for (i = 0, ptr = ftnunit->f77fio_buf; i < nelem; i++) {
	*nptr = *ptr;
	nptr += stepsize;
	ptr += 1;
      }
   }
   return( nelem );
}
