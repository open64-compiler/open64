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


#pragma ident "@(#) libu/ffio/ccaclose.c	92.2	10/11/99 15:29:41"


#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <unistd.h>
#include <ffio.h>
#include <cray/mtlock.h>
#include "ccaio.h"


extern plock_t _CCA_scache_lock[2];	/* declared in ccaopen.c */

/*
 *	close a cached file.
 *
 *      Returns: -1 if error occurred
 *		  0 if OK
 */
int
_cca_close(
struct fdinfo	*fio,
struct ffsw	*stat
)
{
	int		errv;
	struct fdinfo	*llfio;
	struct cca_f	*cca_info;
        int   shared_cache;
        int   file_number; 
        int   release_cache_buffers;
        int   i;

	cca_info = (struct cca_f *)fio->lyr_info;

	errv = 0;
	llfio  = fio->fioptr;


	/*
	 *	Flush all dirty buffers to the underlying layer.
	 */
	if (_cca_flush(fio, stat) == ERR)
		errv = errv ? errv : stat->sw_error;

        for(i=0;i<cca_info->nbufs;i++)
        {
          if( cca_info->bufs[i].file_page.parts.file_number == cca_info->file_number )
          {
             cca_info->bufs[i].file_page.all = NULL_FILE_PAGE;
          }
        }


	/*
	 *	Truncate the underlying file appropriately.  When buffer 
	 *	flushes occur, the underlying file is normally extended 
	 *	to a multiple of the buffer size.  Now we hack off the
	 *	excess at the end of the file.
	 */

	if (cca_info->fsize < cca_info->feof && cca_info->is_multup == 0) {
	
		if (XRCALL(llfio,seekrtn) llfio, (off_t)((uint64)(cca_info->fsize+7)>>3),
					SEEK_SET, stat) == ERR)
			errv = errv ? errv : stat->sw_error;

		if (XRCALL(llfio,weodrtn) llfio, stat) == ERR)
			errv = errv ? errv : stat->sw_error;
	}

	/*
	 *	Close the underlying layers.
	 */
	if (XRCALL(llfio,closertn) llfio, stat) == ERR)
		errv = errv ? errv : stat->sw_error;

	fio->fioptr = NULL;
#if defined(_CRAY1) || defined(__mips) || defined(_LITTLE_ENDIAN)
	if ((MULTI_ON) && fio->parptr && cca_info->is_shrdlck){
		_locklyr_unlock(fio);
	}
#endif
#ifdef SDS_SUPPORTED
        if(cca_info->optflags.sds && !cca_info->is_welfrm)
		if (XRCALL(cca_info->frontdoorfio,closertn)
			cca_info->frontdoorfio, stat) == ERR)
			errv = errv ? errv : stat->sw_error;
#endif
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
        if(cca_info->optflags.diags)   _cca_close_diags( fio, cca_info, TRUE, TRUE );

#endif
        if( cca_info->optflags.scr ) unlink(cca_info->file_name);

        shared_cache = cca_info->shared_cache;
        file_number  = cca_info->file_number;

        if( shared_cache == 0 )
        {
           release_cache_buffers = TRUE;
        }
        else
        {

           SCACHE_LOCK(_CCA_scache_lock);
           _CCA_scache[shared_cache].cca_info[file_number] = NULL;
           _CCA_scache[shared_cache].file_count--;
           if( _CCA_scache[shared_cache].file_count != 0 ) 
           {
              release_cache_buffers = FALSE;
           }
           else
           {
              release_cache_buffers = TRUE;
              /* cache is done  print out diags if necessary before nuking cca_info[0] */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
              if(_CCA_scache[shared_cache].cca_info[0]->optflags.diags)
                      _cca_close_diags( fio, _CCA_scache[shared_cache].cca_info[0], 0, TRUE );
#endif
              free( _CCA_scache[shared_cache].cca_info[0] );  /* release the shared cca_info */
              free( _CCA_scache[shared_cache].cca_info    );  /* release the array of cca_info's */
           }
           SCACHE_UNLOCK(_CCA_scache_lock);
        }

	_cca_clfree(fio,llfio,release_cache_buffers);

	if (errv)
		ERETURN(stat, errv, 0);


	return(0);
}

/*
 * _cca_clfree()
 *
 *	Free the memory blocks used by the cache layer and set corresponding
 * 	pointers to NULL..
 */
void
_cca_clfree(
struct fdinfo	*fio,
void *fioptr,
int		release_cache_buffers)
{
	int		i;
	int		nb;
	struct cca_f	*cca_info;
	struct cca_buf	*cbufs;
        struct cca_async_tracker *this_tracker;
        struct cca_async_tracker *next_tracker;
        int sds;

	cca_info = (struct cca_f *)fio->lyr_info;


	if (fio->lyr_info != NULL) {
#ifdef SDS_SUPPORTED
	    if ((cca_info->optflags.sds) &&
		(cca_info->frontdoorfio != NULL)) {
		free(cca_info->frontdoorfio);
	    }
#endif
            if( release_cache_buffers )
            {
		if (cca_info->bufs != NULL) 
                {
			nb = cca_info->nbufs;
			cbufs = cca_info->bufs;

			if (cca_info->dirty_sectors_check != NULL) {

       	                   /* This chunk of memory includes the shared */
       	                   /* cache lock. Before we free it, we need to*/
       	                   /* make sure it is unlocked by the locking layer */
       	                   free(cca_info->dirty_sectors_check);
			}

			/*
			 * Buffers were allocated in one chunk.
			 */
			if (cca_info->optflags.sds) {
#ifdef	SDS_SUPPORTED
				int blknum, istat;
				if (cbufs[0].buf != -1){
				    blknum = (int)BPTR2WP(cbufs[0].buf)
					 / WPBLOCK;
				    if (sdsfree(blknum, &istat) != 0)
					_lerror(_LELVL_ABORT, istat);
				}
#endif
			}
			else 
				free(BPTR2CP(cbufs[0].buf));
			for (i=0; i<nb; i++)
				SET_BPTR(cbufs[i].buf,  CPTR2BP(NULL));
			free(cca_info->bufs);
			cca_info->bufs = NULL;
		}
                this_tracker = cca_info->async_tracker;
                while( this_tracker )
                {
                   next_tracker = this_tracker->next_tracker;
                   free( this_tracker );
                   this_tracker = next_tracker;
                }
            }
            free(fio->lyr_info);
            fio->lyr_info = NULL;
	}
	if (fioptr != NULL) {
		free(fioptr);
		fio->fioptr = NULL;
	}

	return;
}
