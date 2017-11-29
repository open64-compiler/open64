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


#pragma ident "@(#) libu/ffio/blxwrite.c	92.3	08/20/99 15:55:29"

#include <stdio.h>
#include <memory.h>
#include <ffio.h>
#include "blxio.h"

/*
 * Global definitions.
 */
static int	zero           = 0;


/*
 * Write a "blx" format file.
 *
 *     This layer does not really write "records."  Instead, bytes are 
 *     passed through unless three or more blanks are found.  In this
 *     case, the blanks are replaced with a blank compression character
 *     and an adjacent byte containing the number of blanks compressed (plus
 *     a bias of blx_dat->blx_off).
 *
 * Parameters:
 *      fio     - fdinfo block pointer
 *      bufptr  - bit pointer to user's data.
 *      nbytes  - Number of bytes to be written
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count (not used for IBM)
 */

ssize_t
_blx_write(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
size_t nbytes; 
int fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	int64      	blx_cnt,
			nbits, 
			blxnum, 
			t_cnt,
			remcnt,
			t_bits,
			tword;
	ssize_t         ret;
	char    	*blx_ptr;
	bitptr		t_bptr,
			tbufptr;
	struct	blx_info *blx_dat;

	tbufptr = bufptr;
	nbits = (uint64)nbytes << 3;     /* convert to bits */
	if (*ubc != 0)          /* can only write in bytes */
		{
		ERETURN(stat, FDC_ERR_UBC, 0);
		}
          
/*
 *     	If NOT writing or just positioned, then error.
 */
	if (fio->rwflag == READIN)
          	{
          	ERETURN(stat, FDC_ERR_WRARD, 0);
          	}
     	fio->rwflag = WRITIN;      /* set operation flag */

/*
 *	Get blx_info. 
 */
	blx_dat = (struct blx_info *)fio->lyr_info;

/*
 *     	Loop through the caller's buffer looking for blanks.  If we find more 
 *     	than two, get the data up to the blank, put a blx_dat->blx_char and the 
 *     	blank count in fio buffer and continue until we've looked through all 
 *	incoming data.
 */

	while (nbits > 0 )
       		{

		blx_ptr = memchr(BPTR2CP(tbufptr), (blx_dat->cmp_char >> 56), 
				(nbits >> 3));
	
        	if (blx_ptr != NULL)
			{
	       		SET_BPTR(t_bptr, CPTR2BP(blx_ptr));
               		t_cnt = SUBT_BPTR(t_bptr,tbufptr);
			}
		else
			t_cnt = nbits;

      		/*
       	 	 * Move the data that precedes the blank.
		 *
		 * remcnt = space remaining in buffer. In BITS
		 */

		remcnt = fio->_ffbufsiz - fio->_cnt;
		while (remcnt < t_cnt)
			{
			PUTDATA(tbufptr, fio, remcnt);	
       			WRITEBLK(ret, fio,(size_t)(fio->_cnt  >> 3), stat, 
				PARTIAL, &zero);
       			if (ret < 0)
       				{
       				return(ret);
       				}

			t_cnt -= remcnt;
			nbits -= remcnt;
       			SET_BPTR(tbufptr,INC_BPTR(tbufptr,remcnt));
			remcnt = fio->_ffbufsiz;
       	 		}
       		PUTDATA(tbufptr, fio, t_cnt);
       		SET_BPTR(tbufptr,INC_BPTR(tbufptr,t_cnt));
       		nbits -= t_cnt;
		if (nbits != 0)
			{
			/*
			 * We are positioned at a blank.
			 * Get a byte at a time checking to see if there
			 * are more than 3 blanks in the sequence.
			 */
			t_bptr = tbufptr;
			t_bits = nbits;
			blx_cnt = 0;
			while (t_bits > 0)
				{
				GET_BITS(tword, t_bptr, CHAR_LEN); 
				if (tword == blx_dat->cmp_char)
					{
					blx_cnt++;
                       			SET_BPTR(t_bptr,
						INC_BPTR(t_bptr,CHAR_LEN));
					t_bits -= CHAR_LEN;
					}
				else
					{
					t_bits = 0;	/* break out of loop */
					}
				}
			while (blx_cnt > 0)
				{
				if ((((unsigned)(blx_cnt + blx_dat->blx_off)<<
				56)>>56) != 
				(blx_cnt + blx_dat->blx_off))
					blxnum = 0xFF-blx_dat->blx_off;
				else
					blxnum = blx_cnt;
				if (blxnum >= 3)
					{
		                       	/* 
					 * Put blx_dat->blx_char and count in fio 
					 * buffer and increment tbufptr, fio->_ptr 
					 * and fio->_cnt.
		                       	 */
					SET_BPTR(tbufptr,
						INC_BPTR(tbufptr,(blxnum << 3)));
					/*
					 * Make sure there is enough room in the
					 * buffer.
					 */
					if (fio->_ffbufsiz - fio->_cnt < 2*CHAR_LEN)
						{
	          				WRITEBLK(ret, fio, (size_t)(fio->_cnt  >> 3), stat, 
							 PARTIAL, &zero);
       		   				if (ret < 0)
       	        					{
		       	        			return(ret);
       	       			 			}
						}
                         		PUT_BITS(fio->_ptr,blx_dat->blx_char,CHAR_LEN);
		                        	SET_BPTR(fio->_ptr,
					INC_BPTR(fio->_ptr,CHAR_LEN));
					PUT_BITS(fio->_ptr,
						((blx_dat->blx_off + blxnum) << 56),
						CHAR_LEN);
		                         SET_BPTR(fio->_ptr,
					INC_BPTR(fio->_ptr,CHAR_LEN));
                       		  	fio->_cnt +=  2*CHAR_LEN;
					nbits -= (blxnum << 3);
					}
				else
					{
     					if ((fio->_cnt + (blxnum << 3)) > fio->_ffbufsiz) 
          					{
          					WRITEBLK(ret, fio, fio->_cnt >> 3, stat, 						 PARTIAL, &zero);
		          			if (ret < 0)
               						{
               						return(ret);
               						}
         	 				}
		               		PUTDATA(tbufptr, fio, (blxnum << 3));
               				SET_BPTR(tbufptr,
						INC_BPTR(tbufptr,(blxnum << 3)));
					nbits -= (blxnum << 3);
					}
				blx_cnt -= blxnum;
				}
			} /* end if nbits != 0 */
		}	/* end while */
	switch(fulp)
		{
		case FULL:
			blx_dat->u_blx = 0;     
          	        WRITEBLK(ret, fio, (size_t)(fio->_cnt >> 3), stat, FULL, &zero);
			if (ret < 0)
				{
				return(ret);
				}
			if (blx_dat->llblocked)
				{
				SETSTAT(stat, FFEOR, nbytes);
				}
			else
				{
				SETSTAT(stat, FFCNT, nbytes);
				}
			break;

		case PARTIAL:
                    	SETSTAT(stat, FFCNT, nbytes);
			break;
               } 

	return(nbytes);

	}

/*
 * Mark the EOF. (Actually, just pass the WEOF along to the next layer.)
 */
int
_blx_weof(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo      *llfio;
	int      ret;

/* 
 *      Check for  Write after Read !! 
 */
	if (fio->rwflag == READIN)
		{
		ERETURN(stat, FDC_ERR_WRARD, 0);
		}

	fio->rwflag = WRITIN;

/*
 *     Pass the WEOF call through to the next layer.
 */
	llfio = fio->fioptr;
	ret = XRCALL(llfio, weofrtn) llfio, stat);
	if (ret < 0)
		return(ERR);

	SETSTAT(stat, FFEOF, 0);
	fio->segbits = 0;
	fio->ateof = 1;
	fio->ateod = 0;

	return(0);
	}

/*
 * Mark the EOD. (Actually, just pass the WEOF along to the next layer.)
 */
int
_blx_weod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	int ret;

/* 
 *	Check for  Write after Read !! 
 */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);


	fio->rwflag = WRITIN;

	if (fio->_cnt != 0)
		{
		WRITEBLK(ret, fio,(size_t)(fio->_cnt >> 3), stat, PARTIAL, &zero);
		if (ret < 0) 
	 		{
			return (ERR);
			}
		}
/*
 *	Everybody gets truncated here (we hope).
 */
	llfio = fio->fioptr;
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	if (ret < 0)
		{
		return(ERR);
		}

	SETSTAT(stat, FFEOD, 0)
	fio->segbits = 0;
	fio->ateof = 0;
	fio->ateod = 1;

	return(0);
	}
