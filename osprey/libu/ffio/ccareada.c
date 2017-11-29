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


#pragma ident "@(#) libu/ffio/ccareada.c	92.2	10/11/99 15:29:41"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "ccaio.h"

/*
 * _cca_reada
 *
 * Process reada requests for the cachea layer.
 *
 * Return value:
 *
 *	If an error occurs, -1 is returned.  
 *
 *	The stat structure is not set until asynchronous I/O completion,
 *	unless an immediately detected error is encountered.   
 */
_cca_reada(
struct fdinfo	*fio, 		/* ffio file descriptor. */
bitptr		bufptr,		/* bit pointer to where data is to go. */
int		nbytes,		/* Number of bytes to be read. */
struct ffsw	*stat,		/* pointer to status return word */
int		fulp,		/* full or partial read mode flag */
int		*ubcp)		/* pointer to unused bit count.  On return, */
				/* *ubcp is updated to contain the unused bit */
				/* count in the data returned. */ 
{
	int		nbits, lftbits;
	int		bs;
	int		moved;		/* number of bits transfered */
	int		bytes_moved;	/* number of bytes transfered */
	int		cpos;
	int		morebits;
	int		olpos;
	int		pgoff;		/* offset within page of the data */
	int		fileaddr;
	struct fdinfo	*llfio;
	struct cca_f	*cca_info;
	struct cca_buf	*cubuf;
        FILE_PAGE       file_page;
        struct cca_async_tracker *this_tracker;
        int completed;
        struct ffsw	err_stat;
        struct ffsw	locstat;
        int ret;
	int err;

	cca_info = (struct cca_f *)fio->lyr_info;
	llfio	 = fio->fioptr;

        if (CCA_SOFT_BYPASS) 
             return ( XRCALL(llfio,readartn) llfio, bufptr, nbytes, stat, fulp,
			     ubcp ) );

        if ( cca_info->optflags.no_read ) {
	    errno = EBADF;
	    ERETURN(stat, EBADF, 0);
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
        /* Although this layer is capable of handling non-zero ubc */
        /* and bitptrs that aren't on a byte boundary, we are not */
        /* supporting this right now on mips systems. */
        if (*ubcp != 0){
                err = FDC_ERR_UBC;
                goto err1_ret;
        }
        if ((BPBITOFF(bufptr) & 07) != 0) {
                err = FDC_ERR_UBC;
                goto err1_ret;
        }
#endif

	nbits = (nbytes << 3) - *ubcp;

	fio->rwflag = READIN;

	if (nbits == 0) {
		SETSTAT(stat, FFCNT, 0);
		return(0);
	}
/*
 *      Guarantee that we will not go past the actual EOD on the file
 *      by reducing the request if necessary
 */
	lftbits = cca_info->fsize - cca_info->cpos;

	if (lftbits < nbits) {
		if (lftbits <= 0) {
			fio->recbits = 0;
			fio->ateof = 0;
			fio->ateod = 1;
			SETSTAT(stat, FFEOD, 0)
			return(0);
		}
		nbits = lftbits;
	}

/*
 *	Initialize user's async status structure.
 */
        stat->sw_flag	= 0;
        stat->sw_count	= 0;
        stat->sw_stat	= 0;
        stat->sw_error	= 0;

/*
 *	Move data from buffer to user
 */
	moved	 = 0;
	bs	 = cca_info->bsize;	/* bit size of each buffer */
	cpos	 = cca_info->cpos;	/* current file position */
	olpos	 = cpos;		/* save original position */
	fileaddr = CCAFLOOR(cpos,bs);	/* bit offset in file of start of current page */
        completed = TRUE;

	while (nbits > 0) {

		pgoff	  = cpos - fileaddr;	/* offset within the page */

                file_page.parts.page_number = fileaddr/bs;
                file_page.parts.file_number = cca_info->file_number;

		CCA_FINDBLK(cca_info, file_page, cubuf, err_stat, ret);
		if (ret == ERR) {
			goto err_ret;
		}

		if (cubuf == NULL) /* if data not buffer-resident*/
                {
                        cca_info->read_miss ++;
			cubuf = _cca_getblk_p(cca_info,llfio, fileaddr, TRUE,
					      &err_stat, NULL, 'r' , 'a' );
                        if ( cubuf == NULL )
			{
				goto err_ret;
			}
		}
		else {
                        cca_info->read_hit ++;
		}

                morebits = MIN(nbits, bs - pgoff);

                CHRONOMETER(cubuf,"reading"); /* adjust last access time*/

                this_tracker = _cca_start_tracker( llfio, cca_info, cubuf, stat,
						   CCA_TRACKER_READA, bufptr,
						   pgoff, morebits );

                if ( this_tracker == (struct cca_async_tracker *)(ERR) ) {
			err_stat = *stat;
			goto err_ret;
                }
                else if ( this_tracker ) {
                   completed = FALSE;
                }

		SET_BPTR(bufptr, INC_BPTR(bufptr, morebits));

		cpos  += morebits;
		nbits -= morebits;
		fileaddr = cpos;	/* cpos is page-aligned after 1st pass*/
	}

	moved		 = cpos - olpos;
	cca_info->cpos   = cpos;
	fio->recbits	+= moved;

	bytes_moved = BITS2BYTES(moved);
	*ubcp = (bytes_moved << 3) - moved;
        if ( completed ) {
           stat->sw_flag = 1;
           stat->sw_stat = 0;
        }
	return (bytes_moved);

err_ret:
	/*
	 * If outstanding asynchronous requests connected to this one 
	 * have been started, wait for them.
	 */
	if (nbits != BYTES2BITS(nbytes) - *ubcp) {
		(void)_cca_fcntl(fio, FC_RECALL, stat, &locstat);
	}
	*stat = err_stat;
	stat->sw_flag = 1;
	errno = stat->sw_error;
	return(ERR);
err1_ret:
	ERETURN(stat, err, 0);
}
