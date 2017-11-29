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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/lio.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.3	*/

#include <cmplrs/fio.h>
#include <limits.h>
#include <mutex.h>
#include "fmt.h"
#include "iomode.h"
#include "lio.h"
#include "iio.h"
#include "idxio.h"
#include "err.h"
#include "open.h"
#include "util.h"
#include "bcompat.h"
#include "sfe.h"
#include <string.h>
#include "wrtfmt.h"
#include "uio.h"
#include "lread.h"

/* type matching for compatibility between do_lio() and do_lio1(),
   see fmt.h for updating */
ftnint          match_type[NTYPES - 4] =	/* -4 because TYLONGLONG, TYLOGICAL8,	* 
						 * TYQUADCOMPLEX, and TYQUADLONG absent	*
						 * in 3.0 and below			*/
{
 TYUNKNOWN
 ,TYADDR
 ,TYBYTE
 ,TYSHORT
 ,TYINT
 ,TYREAL
 ,TYDREAL
 ,TYCOMPLEX
 ,TYDCOMPLEX
 ,TYLOGICAL1
 ,TYLOGICAL2
 ,TYLOGICAL4
 ,TYCHAR
 ,TYSUBR
 ,TYSTRUCTURE
 ,TYNML
 ,TYQUAD
 ,TYERROR
};

static int
s_wsle_com (cilist64 *a, unit **fu)
{
   int             n;
   unit		  *ftnunit;

   if (!f77init)
      f_init ();
   if (n = c_le (a, fu)) {
      return (n);
   }
   ftnunit = *fu;
#ifdef I90
   ftnunit->f90sw = 0;
#endif
   ftnunit->f77putn = t_putc;
   /* switch to write mode */
   if (ftnunit->uwrt != WR_READY && f77nowwriting (ftnunit))
      errret(a->cierr, 160, "startwrt");
   if (ftnunit->ualias->ucc == CC_FORTRAN && ftnunit->ualias->ucchar) {
      putc (ftnunit->ualias->ucchar, ftnunit->ualias->ufd);
      ftnunit->ualias->ucchar = '\0';
   }
   ftnunit->f77lioproc = l_write;
   return (0);
}

int s_wsle (cilist *a)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_wsle_com(&dst, &f77curunit);
}

int s_wsle_mp (cilist *a, unit **fu)
{
  cilist64 dst;
  get_cilist64(&dst, a);
  return s_wsle_com(&dst, fu);
}

int s_wsle64 (cilist64 *a)
{
    return( s_wsle_com( a, &f77curunit ) );
}

int s_wsle64_mp (cilist64 *a, unit **fu)
{
    return( s_wsle_com( a, fu ) );
}


static
int s_wsli_com (icilist64 *a, unit **fu)
{
   int             n;
   unit 	*ftnunit;
   
   if (!f77init)
      f_init ();
   *(fu) = ftnunit = Internal_File;
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;

   c_li (a);
#ifdef I90
   ftnunit->f90sw = 0;
#endif
   ftnunit->uwrt |= WR_OP;
   ftnunit->f77lioproc = l_write;
   ftnunit->f77putn = z_putc;
   return (0);
}

int s_wsli (icilist *a)
{
   icilist64 dst;
   get_icilist64(&dst, a);
   return(s_wsli_com(&dst, &f77curunit));
}

int s_wsli_mp (icilist *a, unit **fu)
{
   icilist64 dst;
   get_icilist64(&dst, a);
   return(s_wsli_com(&dst, fu));
}

int s_wsli64 (icilist64 *a)
{
    return(s_wsli_com(a, &f77curunit));
}

int s_wsli64_mp (icilist64 *a, unit **fu)
{
    return(s_wsli_com(a, fu));
}


int e_wsle (void)
{
    return( e_wsle_mp( &f77curunit ) );
}

int e_wsle_mp (unit **fu)
{
   unit *ftnunit = *fu;
   int n;

   if (ftnunit->ufmt == 2) {
      ftnunit->f77recend = ftnunit->f77recpos;
      return (e_wsue ());
   }
   switch (ftnunit->ucc) {
   case CC_FORTRAN:
      if (ftnunit->f77putn (ftnunit, 1, '\r', NULL))
	errret( ftnunit->f77errlist.cierr, errno, "system write error" );
      if (ftnunit->ufd == stdout)
	 fflush (stdout);
      ftnunit->ucchar = '\n';
      break;
   case CC_LIST:
      if (ftnunit->f77putn (ftnunit, 1, '\n', NULL))
	errret( ftnunit->f77errlist.cierr, errno, "system write error" );
   default:
      break;
   }
   ftnunit->f77recpos = 0;
   /*
   if (ftnunit->ushared)
      fflush(ftnunit->ufd);
   */
   ftnunit->lock_unit = 0;
   return (0);
}

int e_xsle (void)
{
    return( e_xsle_mp (&f77curunit) );
}

int e_xsle_mp (unit **fu)
{
   return( e_wsle_mp (fu) );
}

int e_wsli (void)
{
    return(e_wsli_mp());
}

int e_wsli_mp (void)
{
   unit		*ftnunit = Internal_File;
   z_wnew (ftnunit);
   ftnunit->lock_unit = 0;
   return (0);
}


int
t_putc (unit *ftnunit, register XINT count, register char con, register char *buf)
{
   if (ftnunit->ufmt == 2) {
      if (ftnunit->f77recpos + count > ftnunit->f77fio_size)
	 ftnunit->f77fio_buf = realloc (ftnunit->f77fio_buf, ftnunit->f77fio_size += FIO_ALLOC);
      if (buf)
	 memcpy (ftnunit->f77fio_buf + ftnunit->f77recpos, buf, count);
      else
	 memset (ftnunit->f77fio_buf + ftnunit->f77recpos, con ? con : (int) ' ', count);
      ftnunit->f77recpos += count;
      return (0);
   }
   ftnunit->f77recpos += count;
   if (buf)
      while (count--) {
	 if (putc (*buf++, ftnunit->ufd) == EOF)
	   return( errno );
      }
   else {
      if (con == 0)
	 con = ' ';
      while (count--) {
	 if (putc (con, ftnunit->ufd) == EOF)
	   return( errno );
      }
   };
   return (0);
}

int
lwrt_I (unit *ftnunit, uinteger *ptr, int w, int len, int start)
{
#ifdef I90
   int maxrl;

   maxrl = (ftnunit->f90sw == 1 && ftnunit->url > 0 ? ftnunit->url : LINE );
   if (ftnunit->f77recpos + w >= maxrl)
#else
   if (ftnunit->f77recpos + w >= LINE)
#endif
      {
      if (ftnunit->f77putn (ftnunit, 1, '\n', NULL))
	err( ftnunit->f77errlist.cierr, errno, "system write error" );
      ftnunit->f77recpos = 0;
      if (start) {		/* for indentation in namelist writes  */
	 t_putc (ftnunit, start, ' ', NULL);
#ifdef I90
	 if (ftnunit->f90sw == 0 ) t_putc (ftnunit, 4, 0, "\t   ");
#else
	 t_putc (ftnunit, 4, 0, "\t   ");
#endif
      }
   }

#ifdef I90
   if ( ftnunit->f90sw == 1 ) {
	if(ftnunit->f77recpos==0 && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
   } else {
	if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
   }
#else
   if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	ftnunit->f77putn(ftnunit, 1,' ',NULL);
#endif

   (void) wrt_I (ftnunit, ptr, w, len);
   return 0;
}

int
lwrt_L (unit *ftnunit, uinteger *ptr, int w, int len, int start)
{
#ifdef I90
   int maxrl;
   maxrl = (ftnunit->f90sw == 1 && ftnunit->url > 0 ? ftnunit->url : LINE );
   if (ftnunit->f77recpos + w >= maxrl)
#else
   if (ftnunit->f77recpos + w >= LINE)
#endif
      {
      ftnunit->f77putn (ftnunit, 1, '\n', NULL);
      ftnunit->f77recpos = 0;
      if (start) {		/* for indentation in namelist writes  */
	 t_putc (ftnunit, start, ' ', NULL);
#ifdef I90
	 if (ftnunit->f90sw == 0 ) t_putc (ftnunit, 4, 0, "\t   ");
#else
	 t_putc (ftnunit, 4, 0, "\t   ");
#endif
      }
   }

#ifdef I90
   if ( ftnunit->f90sw == 1 ) {
	if(ftnunit->f77recpos==0 && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
   } else {
	if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
   }
#else
   if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	ftnunit->f77putn(ftnunit, 1,' ',NULL);
#endif

   (void) wrt_L (ftnunit, ptr, w, len);
   return 0;
}

int
lwrt_A (unit *ftnunit, char *p, ftnlen len, int start)
#ifdef I90
{
	int i, maxrl;
	maxrl = (ftnunit->f90sw == 1 && ftnunit->url > 0 ? ftnunit->url : LINE );

	if (ftnunit->udelim==QUOTE) {

	    if( ( ftnunit->f77recpos > 0) && ( ftnunit->f77recpos+len+4 >= maxrl ) && ( start >= 0 ) ) {
		ftnunit->f77putn(ftnunit, 1,'\n',NULL);
		ftnunit->f77recpos=0;
		if ( start ) { /* for indentation in namelist writes */
	   	    t_putc(ftnunit, start,' ',NULL);
		    if (ftnunit->f90sw == 0 ) t_putc (ftnunit, 4, 0, "\t   ");
		}
	    }

	    putc(' ',ftnunit->ufd);
	    ++ftnunit->f77recpos;
	    putc('\"',ftnunit->ufd);
	    ++ftnunit->f77recpos;
	    if ( p != NULL ) {
		for (i=0;i<len;i++) {
		    if ( ftnunit->f77recpos == maxrl ) {
			putc('\n',ftnunit->ufd);
			ftnunit->f77recpos = 0;
		    }
		    if (*p=='\"') {
		    	if ( ftnunit->f77recpos == maxrl-1 ) {
			    putc('\n',ftnunit->ufd);
			    ftnunit->f77recpos = 0;
			}
		        putc('\"',ftnunit->ufd);
		        ++ftnunit->f77recpos;
		    }
		    putc(*p++,ftnunit->ufd);
		    ++ftnunit->f77recpos;
		}
	    } else {
		for (i=0;i<len;i++) {
		    if ( ftnunit->f77recpos == maxrl ) {
			putc('\n',ftnunit->ufd);
			ftnunit->f77recpos = 0;
		    }
		    putc(' ',ftnunit->ufd);
		    ++ftnunit->f77recpos;
		}
	    }
	    if ( ftnunit->f77recpos == maxrl ) {
		putc('\n',ftnunit->ufd);
		ftnunit->f77recpos = 0;
	    }
	    putc('\"',ftnunit->ufd);
	    ++ftnunit->f77recpos;
	    if ( ftnunit->f77recpos < maxrl) {
		putc(' ',ftnunit->ufd);
	        ++ftnunit->f77recpos;
	    }

	} else if (ftnunit->udelim==APOSTROPHE) {

	    if( ( ftnunit->f77recpos > 0) && ( ftnunit->f77recpos+len+4 >= maxrl ) && ( start >= 0 ) ) {
		ftnunit->f77putn(ftnunit, 1,'\n',NULL);
		ftnunit->f77recpos=0;
		if ( start ) { /* for indentation in namelist writes */
	   	    t_putc(ftnunit, start,' ',NULL);
		    if (ftnunit->f90sw == 0 ) t_putc (ftnunit, 4, 0, "\t   ");
		}
	    }

	    putc(' ',ftnunit->ufd);
	    ++ftnunit->f77recpos;
	    putc('\'',ftnunit->ufd);
	    ++ftnunit->f77recpos;
	    if ( p != NULL ) {
		for (i=0;i<len;i++) {
		    if ( ftnunit->f77recpos == maxrl ) {
			putc('\n',ftnunit->ufd);
			ftnunit->f77recpos = 0;
		    }
		    if (*p=='\'') {
		    	if ( ftnunit->f77recpos == maxrl-1 ) {
			    putc('\n',ftnunit->ufd);
			    ftnunit->f77recpos = 0;
			}
		        putc('\'',ftnunit->ufd);
		        ++ftnunit->f77recpos;
		    }
		    putc(*p++,ftnunit->ufd);
		    ++ftnunit->f77recpos;
		}
	    } else {
		for (i=0;i<len;i++) {
		    if ( ftnunit->f77recpos == maxrl ) {
			putc('\n',ftnunit->ufd);
			ftnunit->f77recpos = 0;
		    }
		    putc(' ',ftnunit->ufd);
		    ++ftnunit->f77recpos;
		}
	    }
	    if ( ftnunit->f77recpos == maxrl ) {
		putc('\n',ftnunit->ufd);
		ftnunit->f77recpos = 0;
	    }
	    putc('\'',ftnunit->ufd);
	    ++ftnunit->f77recpos;
	    if ( ftnunit->f77recpos < maxrl) {
		putc(' ',ftnunit->ufd);
	        ++ftnunit->f77recpos;
	    }

	} else {

	    if((ftnunit->f77recpos>0)&&(ftnunit->f77recpos+len>=maxrl)) {
		ftnunit->f77putn(ftnunit, 1,'\n',NULL);
		ftnunit->f77recpos=0;
		if ( start ) { /* for indentation in namelist writes */
	   	    t_putc (ftnunit, start,' ',NULL);
		    if (ftnunit->f90sw == 0 ) t_putc (ftnunit, 4, 0, "\t   ");
		}
	    }

	    if ( ftnunit->f90sw == 1 ) {
		if(ftnunit->f77recpos==0 && ftnunit->ucc==CC_LIST)
		    ftnunit->f77putn(ftnunit, 1,' ',NULL);
	    } else {
		if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
		    ftnunit->f77putn(ftnunit, 1,' ',NULL);
	    }

            if ( ftnunit->f90sw == 1 && ftnunit->url > 0 && ftnunit->f77recpos + len > maxrl ) {
		err( ftnunit->f77errlist.cierr ,110,"lio");
	    } else {
		ftnunit->f77putn(ftnunit, len,0,p);
	    }

	}

return 0;
}
#else
{
   if ((ftnunit->f77recpos > 0) && (ftnunit->f77recpos + len >= LINE)) {
      ftnunit->f77putn (ftnunit, 1, '\n', NULL);
      ftnunit->f77recpos = 0;
      if (start) {		/* for indentation in namelist writes */
	 t_putc (ftnunit, start, ' ', NULL);
	 t_putc (ftnunit, 4, 0, "\t   ");
      }
   }
   if (ftnunit->f77recpos == 0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc == CC_LIST)
      t_putc (ftnunit, 1, ' ', NULL);
   ftnunit->f77putn (ftnunit, len, 0, p);
   return 0;
}
#endif

int
lwrt_G (unit *ftnunit, ufloat *ptr, int w, int d, int e, int len, int type, int doblank, int start)
{
#ifdef I90
   int maxrl;
   maxrl = (ftnunit->f90sw == 1 && ftnunit->url > 0 ? ftnunit->url : LINE );
   if (ftnunit->f77recpos + w >= maxrl)
#else
   if (ftnunit->f77recpos + w >= LINE)
#endif
      {
      ftnunit->f77putn (ftnunit, 1, '\n', NULL);
      ftnunit->f77recpos = 0;
      if (start) {		/* for indentation in namelist writes   */
	 t_putc (ftnunit, start, ' ', NULL);
#ifdef I90
	 if (ftnunit->f90sw == 0 ) t_putc (ftnunit, 4, 0, "\t   ");
#else
	 t_putc (ftnunit, 4, 0, "\t   ");
#endif
      };
   }

#ifdef I90
   if ( ftnunit->f90sw == 1 ) {
	if(ftnunit->f77recpos==0 && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
   } else {
	if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
   }
#else
   if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	ftnunit->f77putn(ftnunit, 1,' ',NULL);
#endif

   ftnunit->f77scale = 1;
   (void) wrt_G (ftnunit, ptr, (int)G, w, d, e, len, type, doblank);
   return 0;
}

int
lwrt_C (unit *ftnunit, ufloat *a, ufloat *b, int w, int d, int e, int len, int type, int start)
{
   int maxrl;

#ifdef I90
   maxrl = (ftnunit->f90sw == 1 && ftnunit->url > 0 ? ftnunit->url : LINE );
   if ( ftnunit->f90sw == 1 ) {
	if ( ftnunit->f77recpos +w+3 >= maxrl ||
	   ( ftnunit->f77recpos +w+w+4 >= maxrl && !ftnunit->f77errlist.iciunit ) ) {
	    ftnunit->f77putn (ftnunit, 1, '\n', NULL);
	    ftnunit->f77recpos = 0;
	    if (start) {		/* for indentation in namelist writes  */
		t_putc (ftnunit, start, ' ', NULL);
	    }
	}
	if(ftnunit->f77recpos==0 && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
	else if (ftnunit->f77recpos > 0)
	    ftnunit->f77putn (ftnunit, 1, ' ', NULL);
   } else {
	if (ftnunit->f77recpos + w+w+4 >= maxrl) {
	    ftnunit->f77putn (ftnunit, 1, '\n', NULL);
	    ftnunit->f77recpos = 0;
	    if (start) {		/* for indentation in namelist writes  */
		t_putc (ftnunit, start, ' ', NULL);
		t_putc (ftnunit, 4, 0, "\t   ");
	    }
	}
	if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
	else if (ftnunit->f77recpos > 0)
	    ftnunit->f77putn (ftnunit, 1, ' ', NULL);
   }
#else
   maxrl = LINE;
   if (ftnunit->f77recpos + w+w+4 >= maxrl) {
      ftnunit->f77putn (ftnunit, 1, '\n', NULL);
      ftnunit->f77recpos = 0;
      if (start) {		/* for indentation in namelist writes  */
	 t_putc (ftnunit, start, ' ', NULL);
	 t_putc (ftnunit, 4, 0, "\t   ");
      }
   }
   if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	ftnunit->f77putn(ftnunit, 1,' ',NULL);
   else if (ftnunit->f77recpos > 0)
      ftnunit->f77putn (ftnunit, 1, ' ', NULL);
#endif

   ftnunit->f77putn (ftnunit, 1, '(', NULL);
   ftnunit->f77scale = 1;
   (void) wrt_G (ftnunit, a, (int)G, w, d, e, len >> 1, type, 0);
   ftnunit->f77putn (ftnunit, 1, ',', NULL);

   if(ftnunit->f77recpos+w+1>=maxrl) {
      ftnunit->f77putn (ftnunit, 1, '\n', NULL);
      ftnunit->f77recpos = 0;
   }

#ifdef I90
   if ( ftnunit->f90sw == 1 ) {
	if(ftnunit->f77recpos==0 && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
   } else {
	if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	    ftnunit->f77putn(ftnunit, 1,' ',NULL);
   }
#else
   if(ftnunit->f77recpos==0 && !ftnunit->f77errlist.iciunit && ftnunit->ucc==CC_LIST)
	ftnunit->f77putn(ftnunit, 1,' ',NULL);
#endif

   (void) wrt_G (ftnunit, b, (int)G, w, d, e, len >> 1, type, 0);
   ftnunit->f77putn (ftnunit, 1, ')', NULL);
   return 0;
}

/*
   Note: Fortran-90 manual 10.8.2, List-directed output:  Except for continuation of delimited
   character constants, each output record begins with a blank character to provide carrige control
   when the record is printed.
*/

int l_write (unit *ftnunit, XINT *number, flex *ptr, ftnlen len, ftnint type)
{
   XINT		i;
   int		w=0;
   int		n;

   for (i = 0; i < *number; i++) {
      switch ((int) type) {
      default:
	 ftnunit->lcount = i;
	 err( ftnunit->f77errlist.cierr ,249,"unknown type in lio");
/* fix bug 4585
		case TYLOGICAL1:
   end 6/15/89 */
      case TYBYTE:
      case TYSHORT:
      case TYINT:
      case TYLONGLONG:
/* fix bug 5675 */
      case TYADDR:
	 switch ((int) type) {
	 case TYLOGICAL1:
	 case TYBYTE:
	    w = 5;
	    break;
	 case TYSHORT:
	    w = 7;
	    break;
	 case TYINT:
	    w = 12;
	    break;
/* fix bug 5675 */
	 case TYADDR:
#if (_MIPS_SZPTR == 64)
	    w = 21;
#else
	    w = 12;
#endif
	    break;

	 case TYLONGLONG:
	    w = 21;
	 }
         if (n = lwrt_I (ftnunit, (uinteger *) ptr, w, len, 0))
	   return( n );
	 break;
      case TYREAL:
	 if (n = lwrt_G (ftnunit, (ufloat *) ptr, 15, 7, 2, len, type, 1, 0))
	   return( n );
	 break;
      case TYDREAL:
	 if (n = lwrt_G (ftnunit, (ufloat *) ptr, 24, 16, 0, len, type, 1, 0))
	   return( n );
	 break;
      case TYQUAD:
	 if (n = lwrt_G (ftnunit, (ufloat *) ptr, 40, 31, 0, len, type, 1, 0))
	   return( n );
	 break;
      case TYCOMPLEX:
	 if (n = lwrt_C (ftnunit, (ufloat *) ptr, (ufloat *) ((float *)ptr + 1), 15, 7, 2, len, type, 0))
	   return( n );
	 break;
      case TYDCOMPLEX:
	 if (n = lwrt_C (ftnunit, (ufloat *) ptr, (ufloat *) ((double *)ptr + 1), 24, 16, 0, len, type, 0))
	   return( n );
	 break;
      case TYQUADCOMPLEX:
	 /* Still SIMULATING quad precision using double! */
	 if (n = lwrt_C (ftnunit, (ufloat *) ptr, (ufloat *) ((long double *)ptr + 1), 40, 31, 0, len, type, 0))
	   return( n );
	 break;
/* fix bug 4585 */
      case TYLOGICAL1:
/* end 6/15/89 */
      case TYLOGICAL2:
      case TYLOGICAL4:
      case TYLOGICAL8:
	 if (n = lwrt_L (ftnunit, (uinteger *) ptr, 2, len, 0))
	   return( n );
	 break;
      case TYCHAR:
	 if (n = lwrt_A (ftnunit, (char *) ptr, len, 0))
	   return( n );
	 break;
      }
      ptr = (flex *) ((char *) ptr + len);
   }
   return (0);
}

int
c_le (cilist64 *a, unit **fu)
{
   unit *ftnunit;
   if ((ftnunit = *fu = map_luno (a->ciunit)) == NULL)
      err(a->cierr, 101, "lio");
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   if (ftnunit->uconn <= 0 && fk_open (SEQ, FMT, a->ciunit)) {
      ftnunit->uconn = 0;
      errret(a->cierr, 102, "lio");
   }
   ftnunit->f77fmtbuf = "list io";
   ftnunit->f77recpos = 0;
   ftnunit->f77scale = 0;
   
   ftnunit->f77errlist.cierr = a->cierr;
   ftnunit->f77errlist.ciend = a->ciend;
   ftnunit->f77errlist.cieor = a->cieor;
   ftnunit->f77errlist.cisize = a->cisize;
   ftnunit->f77errlist.iciunit = 0;
   if (ftnunit->uacc == KEYED) {
      ftnunit->f77idxlist.cimatch = a->cimatch;
      ftnunit->f77idxlist.cikeytype = a->cikeytype;
      ftnunit->f77idxlist.cikeyval.cicharval = a->cikeyval.cicharval;
      ftnunit->f77idxlist.cikeyid = a->cikeyid;
      ftnunit->f77idxlist.cinml = a->cinml;
      ftnunit->f77idxlist.cikeyvallen = a->cikeyvallen;
   }
   if (ftnunit->ufmt != 1)
      errret(a->cierr, 102, "lio");
   return (0);
}

void
c_li (icilist64 *a)
{
   unit 	*ftnunit = Internal_File;

   ftnunit->f77fmtbuf = "list io";
   ftnunit->f77errlist.cierr = a->icierr;
   ftnunit->f77errlist.ciend = a->iciend;
   ftnunit->f77errlist.cieor = 0;
   ftnunit->f77errlist.cisize = 0;
   ftnunit->f77errlist.iciunit = a->iciunit;
   ftnunit->f77errlist.icirlen = ftnunit->url = a->icirlen;
   ftnunit->f77errlist.icirnum = a->icirnum;
   ftnunit->f77scale = 0;
   ftnunit->f77recpos = ftnunit->f77recend = icnum = icpos = 0;
   icptr = ftnunit->f77errlist.iciunit;
   icend = icptr + ftnunit->f77errlist.icirlen * ftnunit->f77errlist.icirnum;
}

#pragma weak __kai_do_lio = do_Lio
#pragma weak __kai_do_lio_1dim = do_Lio_1dim
#pragma weak __kai_do_lio_mp = do_Lio_mp
#pragma weak do_lio64 = do_Lio64



#pragma weak __kai_do_lio64 = do_Lio64
#pragma weak __kai_do_lio64_1dim = do_Lio64_1dim
#pragma weak __kai_do_lio64_mp = do_Lio64_mp
#pragma weak __kai_do_lio64_mp_1dim = do_Lio64_mp_1dim



static int
do_Lio_com(ftnint *type, XINT *number, flex *ptr, unit **fu, ftnlen len)
{
/* LHL 5/25/89
 * to indicate it's not a namelist read
	return((*ftnunit->f77lioproc)(number,ptr,len,*type));
 */
   int n;
   if (n = (*(*fu)->f77lioproc) (*fu, number, ptr, len, *type)) {
      if ((*fu)->f77lioproc == l_read)
        /* after an error in list-directed read need to skip to the
        next record */
         for (; (*fu)->nextch != '\n' && (*fu)->nextch != EOF; (*fu)->nextch = (*(*fu)->f77getn)((*fu)));
      (*fu)->lock_unit = 0;
   }
   return(n);
}

int
do_Lio (ftnint *type, ftnint *number, flex *ptr, ftnlen len)
{
    XINT xnumber;
    xnumber = *number;
    return( do_Lio_com( type, &xnumber, ptr, &f77curunit, len ) );
}

int
do_Lio64 (ftnint *type, XINT *number, flex *ptr, ftnlen len)
{
    return( do_Lio_com( type, number, ptr, &f77curunit, len ) );
}

static int
do_Lio_1dim_com( ftnint *type, flex *ptr, 
		flex *do_idx, XINT *lb,
		XINT  *ub, XINT  *step, 
		unit **fu,
		ftnlen len, ftnlen idxlen)
/* 
** This function carries out the I/O operation on a single-dimension
** implied-DO loop in the form (ARR(I),I=N,M,L).  It takes these 
** arguments:
**
**	type	: Fortran type of the I/O array
**	ptr	: address of the first array element involved in the I/O
**		  in the example above it is the address of ARR(N)
**	step	: the implied-do loop step size which is L in this case.
**	len	: length of each array element
*/
{
   XINT nelem;
   int ierr;
   char *lastptr, *nptr;

   lastptr = (char *) ptr + (*ub - 1) * len;
   ptr = (flex *) ((char *) ptr + (*lb - 1) * len);
   if (*step == 1) {
   /* consecutive elements */
      if ((nelem = *ub - *lb + 1) > 0) {
	  if (ierr = do_Lio_com( type, &nelem, ptr, fu, len ) ) {
	      set_do_idx( do_idx, idxlen, (ftnll) (*lb + (*fu)->lcount) );
	      return( ierr );
	  }
	  set_do_idx( do_idx, idxlen, (ftnll) (*ub + 1) );
      }
      /* if last element <= first: dothing */
      return( 0 );
   }
   
   nelem = 1;
   if (*step > 0) {
       for (nptr = (char *) ptr;nptr <= lastptr; nptr += *step*len)
	   if (ierr = do_Lio_com( type, &nelem, (flex *)nptr, fu, len ) ) {
		  set_do_idx( do_idx, idxlen, (ftnll) (*lb + (nptr - (char *)ptr)/len) );
		  return( ierr );
	   }
   }
   else
       for (nptr = (char *) ptr; nptr >= lastptr; nptr += *step*len)
	   if (ierr = do_Lio_com( type, &nelem, (flex *)nptr, fu, len ) ) {
		  set_do_idx( do_idx, idxlen, (ftnll) (*lb - ((char *)ptr - nptr)/len) );
		  return( ierr );
	   }
   set_do_idx( do_idx, idxlen, (ftnll) (*lb + ((*ub - *lb) / *step + 1) * *step) );
   return(0);
}

int
do_Lio_1dim( ftnint *type, flex *ptr, 
		flex *do_idx, ftnint *lb,
		ftnint *ub, ftnint *step, 
		ftnlen len, ftnlen idxlen)
{
	/* 6.0, 6.01 compatibility function */
	XINT xlb, xub, xstep;
	xlb = *lb;
	xub = *ub;
	xstep = *step;
	return (do_Lio_1dim_com(type, ptr, do_idx, &xlb, &xub, &xstep, &f77curunit, len, idxlen));
}

int
do_Lio64_1dim(ftnint *type, flex *ptr,
		flex *do_idx, XINT *lb,
		XINT *ub, XINT *step,
		ftnlen len, ftnlen idxlen)
{
    return( do_Lio_1dim_com( type, ptr, do_idx, lb, ub, step, &f77curunit, len, idxlen));
}

int
do_Lio64_mp_1dim(ftnint *type, flex *ptr,
                flex *do_idx, XINT *lb,
                XINT *ub, XINT *step,
		unit **fu,
                ftnlen len, ftnlen idxlen)
{
    return( do_Lio_1dim_com( type, ptr, do_idx, lb, ub, step, fu, len, idxlen));
}


int
do_Lio_mp (ftnint *type, ftnint *number, flex *ptr, unit **fu, ftnlen len)
{
    XINT xnumber;
    xnumber = *number;
    return( do_Lio_com( type, &xnumber, ptr, fu, len ) );
}

int
do_Lio64_mp (ftnint *type, XINT *number, flex *ptr, unit **fu, ftnlen len)
{
	return (do_Lio_com(type, number, ptr, fu, len ));
}


int
do_lio (ftnint *type, ftnint *number, flex *ptr, ftnlen len)
{
/* This routine provided for compatibility, since LONGLONG type is added
   in the middle in fcom. Uses match_type[] to rematch the types - 
   match_type[] declared in fmt.h */

	XINT numb = *number;
   return( do_Lio_com( &match_type[*type], &numb, ptr, &f77curunit, len ) );
}

#pragma weak e_wsle64 = e_wsle		/* extern int e_wsle64(void); */
#pragma weak e_wsle64_mp = e_wsle_mp	/* extern int e_wsle64_mp(unit **); */
#pragma weak e_xsle64 = e_xsle		/* extern int e_xsle64(void); */
#pragma weak e_xsle64_mp = e_xsle_mp	/* extern int e_xsle64_mp(unit **); */
#pragma weak e_wsli64 = e_wsli		/* extern int e_wsli64(void); */
#pragma weak e_wsli64_mp = e_wsli_mp	/* extern int e_wsli64_mp(void); */


#pragma weak do_lio90_mp = do_Lio_mp
#pragma weak do_lio90 = do_Lio
#pragma weak do_lio9064 = do_Lio64
#pragma weak do_lio9064_mp = do_Lio64_mp


/*  ========================================================================  */
/*									      */
/*  The following entries are a minor efficiency improvement in calling       */
/*  the list-directed I/O routines.  In a few cases, they overlap existing    */
/*  entries but are an attempt to clean up the interface.  In truth, the      */
/*  entire I/O interface could use an overhaul.				      */
/*									      */
/*  ========================================================================  */

int
do_lioxa4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYADDR;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 4 ) );
}

int
do_lioxa4 (char *ptr, XINT num)
{
    ftnint type = TYADDR;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 4 ) );
}

int
do_lioxa8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYADDR;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 8 ) );
}

int
do_lioxa8 (char *ptr, XINT num)
{
    ftnint type = TYADDR;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 8 ) );
}

int
do_lioxh1_mp (char *ptr, XINT clen, XINT num, unit **fu)
{
    ftnint type = TYCHAR;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, clen ) );
}

int
do_lioxh1 (char *ptr, XINT clen, XINT num)
{
    ftnint type = TYCHAR;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, clen ) );
}

int
do_lioxi1_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYBYTE;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 1 ) );
}

int
do_lioxi1 (char *ptr, XINT num)
{
    ftnint type = TYBYTE;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 1 ) );
}

int
do_lioxi2_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYSHORT;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 2 ) );
}

int
do_lioxi2 (char *ptr, XINT num)
{
    ftnint type = TYSHORT;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 2 ) );
}

int
do_lioxi4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYINT;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 4 ) );
}

int
do_lioxi4 (char *ptr, XINT num)
{
    ftnint type = TYINT;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 4 ) );
}

int
do_lioxi8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLONGLONG;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 8 ) );
}

int
do_lioxi8 (char *ptr, XINT num)
{
    ftnint type = TYLONGLONG;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 8 ) );
}

int
do_lioxl1_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLOGICAL1;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 1 ) );
}

int
do_lioxl1 (char *ptr, XINT num)
{
    ftnint type = TYLOGICAL1;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 1 ) );
}

int
do_lioxl2_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLOGICAL2;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 2 ) );
}

int
do_lioxl2 (char *ptr, XINT num)
{
    ftnint type = TYLOGICAL2;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 2 ) );
}

int
do_lioxl4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLOGICAL4;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 4 ) );
}

int
do_lioxl4 (char *ptr, XINT num)
{
    ftnint type = TYLOGICAL4;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 4 ) );
}

int
do_lioxl8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYLOGICAL8;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 8 ) );
}

int
do_lioxl8 (char *ptr, XINT num)
{
    ftnint type = TYLOGICAL8;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 8 ) );
}

int
do_lioxr4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYREAL;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 4 ) );
}

int
do_lioxr4 (char *ptr, XINT num)
{
    ftnint type = TYREAL;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 4 ) );
}

int
do_lioxr8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYDREAL;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 8 ) );
}

int
do_lioxr8 (char *ptr, XINT num)
{
    ftnint type = TYDREAL;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 8 ) );
}

int
do_lioxr16_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYQUAD;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 16 ) );
}

int
do_lioxr16 (char *ptr, XINT num)
{
    ftnint type = TYQUAD;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 16 ) );
}

int
do_lioxc4_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYCOMPLEX;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 8 ) );
}

int
do_lioxc4 (char *ptr, XINT num)
{
    ftnint type = TYCOMPLEX;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 8 ) );
}

int
do_lioxc8_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYDCOMPLEX;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 16 ) );
}

int
do_lioxc8 (char *ptr, XINT num)
{
    ftnint type = TYDCOMPLEX;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 16 ) );
}

int
do_lioxc16_mp (char *ptr, XINT num, unit **fu)
{
    ftnint type = TYQUADCOMPLEX;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, fu, 32 ) );
}

int
do_lioxc16 (char *ptr, XINT num)
{
    ftnint type = TYQUADCOMPLEX;
    XINT number = num;
    return( do_Lio_com( &type, &number, (flex *)ptr, &f77curunit, 32 ) );
}

int
do_lioxa4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    ftnint type = TYADDR;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 4 ) );
}

int
do_lioxa4v (ftnint val)
{
    ftnint value = val;
    ftnint type = TYADDR;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 4 ) );
}

int
do_lioxa8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    ftnint type = TYADDR;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 8 ) );
}

int
do_lioxa8v (ftnll val)
{
    ftnll value = val;
    ftnint type = TYADDR;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 8 ) );
}

int
do_lioxh1v_mp (char val, unit **fu)
{
    char value = val;
    ftnint type = TYCHAR;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 1 ) );
}

int
do_lioxh1v (char val)
{
    char value = val;
    ftnint type = TYCHAR;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 1 ) );
}

int
do_lioxi1v_mp (char val, unit **fu)
{
    char value = val;
    ftnint type = TYBYTE;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 1 ) );
}

int
do_lioxi1v (char val)
{
    char value = val;
    ftnint type = TYBYTE;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 1 ) );
}

int
do_lioxi2v_mp (short val, unit **fu)
{
    short value = val;
    ftnint type = TYSHORT;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 2 ) );
}

int
do_lioxi2v (short val)
{
    short value = val;
    ftnint type = TYSHORT;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 2 ) );
}

int
do_lioxi4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    ftnint type = TYINT;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 4 ) );
}

int
do_lioxi4v (ftnint val)
{
    ftnint value = val;
    ftnint type = TYINT;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 4 ) );
}

int
do_lioxi8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    ftnint type = TYLONGLONG;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 8 ) );
}

int
do_lioxi8v (ftnll val)
{
    ftnll value = val;
    ftnint type = TYLONGLONG;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 8 ) );
}

int
do_lioxl1v_mp (char val, unit **fu)
{
    char value = val;
    ftnint type = TYLOGICAL1;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 1 ) );
}

int
do_lioxl1v (char val)
{
    char value = val;
    ftnint type = TYLOGICAL1;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 1 ) );
}

int
do_lioxl2v_mp (short val, unit **fu)
{
    short value = val;
    ftnint type = TYLOGICAL2;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 2 ) );
}

int
do_lioxl2v (short val)
{
    short value = val;
    ftnint type = TYLOGICAL2;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 2 ) );
}

int
do_lioxl4v_mp (ftnint val, unit **fu)
{
    ftnint value = val;
    ftnint type = TYLOGICAL4;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 4 ) );
}

int
do_lioxl4v (ftnint val)
{
    ftnint value = val;
    ftnint type = TYLOGICAL4;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 4 ) );
}

int
do_lioxl8v_mp (ftnll val, unit **fu)
{
    ftnll value = val;
    ftnint type = TYLOGICAL8;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 8 ) );
}

int
do_lioxl8v (ftnll val)
{
    ftnll value = val;
    ftnint type = TYLOGICAL8;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 8 ) );
}

int
do_lioxr4v_mp (float val, unit **fu)
{
    float value = val;
    ftnint type = TYREAL;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 4 ) );
}

int
do_lioxr4v (float val)
{
    float value = val;
    ftnint type = TYREAL;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 4 ) );
}

int
do_lioxr8v_mp (double val, unit **fu)
{
    double value = val;
    ftnint type = TYDREAL;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 8 ) );
}

int
do_lioxr8v (double val)
{
    double value = val;
    ftnint type = TYDREAL;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 8 ) );
}

int
do_lioxr16v_mp (long double val, unit **fu)
{
    long double value = val;
    ftnint type = TYQUAD;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 16 ) );
}

int
do_lioxr16v (long double val)
{
    long double value = val;
    ftnint type = TYQUAD;
    XINT number = 1;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 16 ) );
}

int
do_lioxc4v_mp (float rval, float ival, unit **fu)
{
    float value[2];
    ftnint type = TYCOMPLEX;
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 8 ) );
}

int
do_lioxc4v (float rval, float ival)
{
    float value[2];
    ftnint type = TYCOMPLEX;
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 8 ) );
}

int
do_lioxc8v_mp (double rval, double ival, unit **fu)
{
    double value[2];
    ftnint type = TYDCOMPLEX;
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 16 ) );
}

int
do_lioxc8v (double rval, double ival)
{
    double value[2];
    ftnint type = TYDCOMPLEX;
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 16 ) );
}

int
do_lioxc16v_mp (long double rval, long double ival, unit **fu)
{
    long double value[2];
    ftnint type = TYQUADCOMPLEX;
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_Lio_com( &type, &number, (flex *)&value, fu, 32 ) );
}

int
do_lioxc16v (long double rval, long double ival)
{
    long double value[2];
    ftnint type = TYQUADCOMPLEX;
    XINT number = 1;
    value[0] = rval;
    value[1] = ival;
    return( do_Lio_com( &type, &number, (flex *)&value, &f77curunit, 32 ) );
}

/*  ========================================================================  */
