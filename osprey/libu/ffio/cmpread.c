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


#pragma ident "@(#) libu/ffio/cmpread.c	92.2	06/29/99 13:16:47"

#include <math.h> 
#include <ffio.h>
#include <errno.h>
#include <fcntl.h>
#include <liberrno.h>
#include <malloc.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/unistd.h>
#include "cmpio.h"
 
int
_cmpread(fio, bufptr, nbytes, stat, fulp, ubc)
struct	fdinfo *fio;
int 	nbytes, fulp, *ubc;
bitptr 	bufptr;
struct 	ffsw *stat;
{
	char	*usrBuf;
	char	*unCmpSeg;
	cmp_lyr *cinfo; 
	int	segStart, segEnd;
	int	posInSeg; 
        int 	i, bytes, usrBufPos;
	char	*rtnName = "_cmpread";

        struct 	fdinfo *llfio;

        llfio = fio->fioptr;
	cinfo = (cmp_lyr *)fio->lyr_info;

	SETSTAT(stat, FFERR, 0);

	TRC_ENTER;

	/*
	 * Check that indeed the file was opened for reading. If it 
	 * was opened with the O_RDWR flag check to see if we have
	 * done a read or a write to it.
	 */
	if (cinfo->OpnFlags & O_WRONLY) {
		ERETURN(stat, FDC_ERR_BADFLAGS, 0);
	}

	/*
	 * Check that header has been loaded, otherwise return
	 * an error..
	 */
	if (cinfo->InitHeader == FALSE) {
		ERETURN(stat, FDC_ERR_INITHDR, 0);
	}

	/*
	 * Check that the current position plus the bytes to be
	 * read do not take us pass the end of the file. If they
	 * do then we only need to read what's left between the 
	 * current file position 'currPos' and the end of file.
	 */
	if (cinfo->CurrPos + nbytes > cinfo->FileSz) {

		if (cinfo->CurrPos >= cinfo->FileSz) {
			SETSTAT(stat, FFEOD, 0);				

			TRC_LOG("EOD %d, %d, %d, %d", nbytes,
			    cinfo->CurrPos, cinfo->FileSz, stat->sw_stat);

			TRC_LEAVE;

			return(0);
		}

		nbytes = cinfo->FileSz - cinfo->CurrPos;
	}

	/*
	 * Compute the beginning and end data segments where the
	 * requested bytes are found. With this we can index the
	 * data segment descriptor cache and get to the correct
	 * data segments.
	 *
	 * Assume that the current file offset position into the
	 * uncompressed file is always valid (i.e. 'cinfo->CurrPos'
	 * since the only way to set it pass the end of the file
	 * is through the seek and we check for this there.
	 */
	segStart = cinfo->CurrPos / cinfo->Header.h_DataSegSz;
	segEnd   = (cinfo->CurrPos + nbytes) / cinfo->Header.h_DataSegSz;
	segEnd++;

	TRC_LOG("Start %d, End %d, nbytes %d", segStart, segEnd, nbytes);

	/*
	 * Get a character pointer into the user buffer and allocate
	 * space to store the uncompressed data. Data will then be 
	 * copied from this buffer into the user buffer.
	 */
	usrBufPos = 0;
	usrBuf = BPTR2CP(bufptr);

	for (i = segStart; i < segEnd; i++) {
		TRC_LOG("CurrPos %d, nbytes %d", cinfo->CurrPos, nbytes);

		posInSeg = cinfo->CurrPos % cinfo->Header.h_DataSegSz;

		if ((unCmpSeg = _findInCache(llfio, 
				stat, cinfo, i)) == NULL) {
			return(ERR);
		}

		bytes = (posInSeg + nbytes > cinfo->Header.h_DataSegSz) ?
			(cinfo->Header.h_DataSegSz - posInSeg) : nbytes;

		memcpy((void *)(usrBuf + usrBufPos), 
		       (void *)(unCmpSeg + posInSeg), bytes);

		TRC_LOG("posInSeg %d, bytes %d, usrBufPos %d",
			 posInSeg, bytes, usrBufPos);

		nbytes -= bytes;
		cinfo->CurrPos += bytes;
		usrBufPos += bytes;

		TRC_LOG("nbytes %d, CurrPos %d, usrBufPos %d",
			 nbytes, cinfo->CurrPos, usrBufPos);

		if (nbytes <= 0) break;
	}

	FFSTAT(*stat) = FFCNT;

	TRC_LEAVE;
	return(usrBufPos);
}

/*
 * This routine checks in the data segment list cache to see if the
 * given segment (i.e. 'segNum') has already been uncompressed. If 
 * successful, we return the uncompressed segment otherwise, NULL 
 * is returned.
 */
char *
_findInCache(llfio, stat, cinfo, segNum)
	struct fdinfo	*llfio;
	struct ffsw	*stat;
	cmp_lyr		*cinfo;
	int		 segNum;
{
	int	  i, LUidx;
	char	 *unCmpSeg;
	UnCmpSeg *unCmpSegList;
	char	 *rtnName = "_findInCache";

	struct DataSegD *DSegDCache;

	TRC_ENTER;

	DSegDCache = cinfo->DSegDCache;
	unCmpSegList = cinfo->UnCmpSegList;

	for (LUidx = i = 0; i < cinfo->DSegCacheSize; i++) {
		/*
		 * While we parse the list, keep track of which entry in
		 * the data segment buffer cache has been the least used. 
		 * This is in case we need to reuse an entry.
		 */
		if (unCmpSegList[LUidx].LUcount < 
				    unCmpSegList[i].LUcount) {
			LUidx = i;
		}
			
		/*
		 * Break when we find the first unused data segment buffer. 
		 * We can do this because we know that the rest of the en-
		 * tries will also be unused.
		 */
		if (unCmpSegList[i].LUcount == 0) {
			TRC_LOG("Found UnUsed unCmpSegList[%d]", i);
			break;
		}

		/*
		 * Break if we find the uncompressed segment already in
		 * the segment cache list.
		 */
		if (unCmpSegList[i].SegNum == segNum) {
			cinfo->Hit++; /* for statistics sake */
			TRC_LOG("Found unCmpSegList[%d], %d", i, segNum);
			break;
		}
	}

	/*
	 * If we run past the end of the uncompressed buffers then 
	 * reuse one of the cached data segment entries. We know 
	 * which entry to use because we kept track of it in the 
	 * 'LUidx' variable ... boy, are we smart!
	 */
	if (i >= cinfo->DSegCacheSize) {
		i = LUidx;

		if (unCmpSegList[i].Buffer != NULL) {
			TRC_LOG("Freeing unCmpSegList[%d].Buffer", i);
			free(unCmpSegList[i].Buffer);
		}

		memset(&(unCmpSegList[i]), 0, sizeof(UnCmpSeg));
		TRC_LOG("ReUsing unCmpSegList[%d]", i);
	}

	/*
	 * We either found an unused data segment buffer to store 
	 * our uncompressed data, or we are reusing a data segment
	 * buffer since there are no unused ones left.
	 */
	if (unCmpSegList[i].LUcount == 0) {
		if ((unCmpSeg = _unCmpDSeg(llfio, stat, 
				DSegDCache[segNum], cinfo)) == NULL) {
                        return(NULL);
                }

		TRC_LOG("Idx %d, SegNum %d", i, segNum);

		unCmpSegList[i].Buffer = unCmpSeg;
		unCmpSegList[i].SegNum = segNum;

		cinfo->Miss++; /* for statistics sake */
	}

	unCmpSegList[i].LUcount++;

	/*
	 * Safety check to be REALLY sure that the buffer returned
	 * is the one specified in the 'SegNum' argument.
	 */
	if ((unCmpSegList[i].LUcount <= 0) ||
	    (unCmpSegList[i].SegNum != segNum)) {
		_SETERROR(stat, FDC_ERR_BADSEGBUF, 0);
		return(NULL);
	}

	TRC_LEAVE;

	return(unCmpSegList[i].Buffer);
}

char *
_unCmpDSeg(llfio, stat, dSegD, cinfo)
	struct fdinfo	*llfio;
	struct ffsw	*stat;
	struct DataSegD  dSegD;
	cmp_lyr		*cinfo;
{
	char   *cmpBuf = NULL;
	char   *unCmpBuf = NULL;
	int	ubc = 0, ret = 0;
	char   *rtnName = "_unCmpDSeg";

	TRC_ENTER;

	/*
	 * Allocate buffer space for the compressed data segment
	 * to be read from file. 
	 */
	cmpBuf = (char *) calloc(dSegD.CmpLength, sizeof(char));
	TRC_LOG("Allocating cmpBuf, %d bytes", 
			dSegD.CmpLength * sizeof(char));

	if (cmpBuf == NULL) {
		TRC_LOG("Unable to allocate cmpBuf, %d bytes\n", 
			dSegD.CmpLength * sizeof(char));
		_SETERROR(stat, FDC_ERR_CMPBUF, 0);
		return(NULL);
	}

	TRC_LOG("Seeking to compressed file offset %d", dSegD.FileAddr);

	/*
	 * Seek and read the appropriate data segment in the
	 * compressed file. Where to seek and how much to read
	 * is given in the data segment descriptor.
	 */
	if (XRCALL(llfio, seekrtn) llfio,
		    dSegD.FileAddr, SEEK_SET, stat) < 0) {
		ret = FDC_ERR_SEEKDSEG;
		goto done;
	}

	TRC_LOG("Reading %d bytes into cmpBuf", dSegD.CmpLength);

	if (cinfo->Reada == TRUE) {
		if (XRCALL(llfio, readartn) llfio, CPTR2BP(cmpBuf), 
		    dSegD.CmpLength, stat, FULL, &ubc) < 0) {
			ret = FDC_ERR_RDADSEG;
			goto done;
		}
	} else {
		if (XRCALL(llfio, readrtn) llfio, CPTR2BP(cmpBuf),
		    dSegD.CmpLength, stat, FULL, &ubc) < 0) {
			ret = FDC_ERR_RDDSEG;
			goto done;
		}
	}
	
	/*
	 * Check if we need to uncompress the data (i.e. look
	 * at the value of the 'Compressed' field in the data
	 * segment descriptor).
	 */
	if (dSegD.Compressed == FALSE) {
		/*
		 * Allocate buffer space to store the uncompressed data 
		 * segment.
		 */
		unCmpBuf = (char *) calloc(dSegD.Length, sizeof(char));
		TRC_LOG("Allocating unCmpBuf, %d bytes",
				dSegD.Length * sizeof(char));

		if (unCmpBuf == NULL) {
			ret = FDC_ERR_UNCMPBUF;
			goto done;
		}

		memcpy((void *)unCmpBuf, (void*)cmpBuf, dSegD.CmpLength);

		/*
		 * Verify that the size given for the uncompressed data
		 * length is the same as that given in the compressed
		 * data length.
		 */
		if (dSegD.Length != dSegD.CmpLength) {
			ret = FDC_ERR_SEGLEN;
			goto done;
		}
	} else {
		/*
		 * Uncompress the data using the correct algorithm.
		 */
		if ((ret = _lz_decompress(cinfo, cmpBuf, dSegD.CmpLength, 
			dSegD.Length, &unCmpBuf)) != 0) {
			goto done;
		}
		TRC_LOG("Allocated unCmpBuf (%d) ", dSegD.Length);
	}
		
done:
	if (ret != 0) {
		TRC_LOG("Error %d\n", ret);
		_SETERROR(stat, ret, 0);

		if (unCmpBuf != NULL) {
			free (unCmpBuf);
			unCmpBuf = NULL;
			TRC_LOG("Deallocated unCmpBuf");
		}
	}

	free(cmpBuf);
	TRC_LOG("Freeing cmpBuf");

	TRC_LEAVE;
	return(unCmpBuf);
}

/*
 * read requests
 *
 * Parameters:
 *  fio     - Pointer to fdinfo block
 *  bufptr  - bit pointer to where data is to go.
 *  nbytes  - Number of bytes to be read
 *  stat    - pointer to status return word
 *  fulp    - full or partial read mode flag
 *  ubc     - pointer to unused bit count
 */

int
_cmp_read(fio, bufptr, nbytes, stat, fulp, ubc)
struct	fdinfo *fio;
int 	nbytes, fulp, *ubc;
bitptr 	bufptr;
struct 	ffsw *stat;
{
        int   	ret;
	cmp_lyr *cinfo;
	char	*rtnName = "_cmp_read";

	cinfo = (cmp_lyr *)fio->lyr_info;
	cinfo->Reada = FALSE;
 
	TRC_ENTER;

        ret = _cmpread(fio, bufptr, nbytes, stat, fulp, ubc);

	TRC_LEAVE;

        return(ret);
}

/*
 * reada (asynchronous read) requests
 *
 * Parameters:
 *  fio     - Pointer to fdinfo block
 *  bufptr  - bit pointer to where data is to go.
 *  nbytes  - Number of bytes to be read
 *  stat    - pointer to status return word
 *  fulp    - full or partial read mode flag
 *  ubc     - pointer to unused bit count
 */

int
_cmp_reada(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
bitptr bufptr;
int nbytes, fulp, *ubc;
struct ffsw *stat;
{
        int 	ret;
	cmp_lyr *cinfo;
	char	*rtnName = "_cmp_reada";

	cinfo = (cmp_lyr *)fio->lyr_info;
	cinfo->Reada = TRUE;
 
	TRC_ENTER;

        ret = _cmpread(fio, bufptr, nbytes, stat, fulp, ubc);

	TRC_LEAVE;
	printf("reada: count %d, ret %d\n", stat->sw_count, ret);

        return(ret);
}
 

/*
 * _cmp_seek()
 *
 * When writing a compressed file the user is allowed to do forward seeks
 * but is not allowed to do backward seeks. An error is returned if the
 * later case is attempted.
 */

int
_cmp_seek(fio, pos, whence, stat)
struct fdinfo *fio;
int pos, whence;
struct ffsw *stat;
{
	cmp_lyr	*cinfo;
        int 	 tmp;
	char	*rtnName = "_cmp_seek";
 
	cinfo = (cmp_lyr *) fio->lyr_info;

	if (cinfo == NULL) {
		return (0);
	}
	
	TRC_ENTER;

	/*
	 * We return an error message if the user opened the compressed
	 * file for writing since we can't allow seeks that will mess
	 * the structure of the compressed file.
	 */
	if ((cinfo->OpnFlags & O_WRONLY) == O_WRONLY) {
		TRC_LOG("FDC_ERR_WRSEEK, 0%o, whence %d, pos %d", 
			cinfo->OpnFlags, whence, pos);
		ERETURN(stat, FDC_ERR_WRSEEK, 0);
	}

	switch(whence) {
	case SEEK_SET:
		tmp = cinfo->CurrPos;
		cinfo->CurrPos = (pos == 0) ? 0 : pos;
		if (cinfo->CurrPos > cinfo->FileSz) {
			cinfo->CurrPos = tmp;

			TRC_LOG("FDC_ERR_SEEKSET, %d, %d, %d", 
				cinfo->CurrPos, pos, cinfo->FileSz);			
			ERETURN(stat, FDC_ERR_SEEKSET, 0);
		}
		break;
	case SEEK_CUR:
		break; /* no effect */
	case SEEK_END:
		cinfo->CurrPos = cinfo->FileSz + pos;
		break;
	default:
		TRC_LOG("FDC_ERR_NOSUP, %d, %d, %d",
			whence, cinfo->CurrPos, cinfo->FileSz);
		ERETURN(stat, FDC_ERR_NOSUP, 0);
	}

	TRC_LOG("Set CurrPos to %d, pos %d, whence %d, fileSz %d", 
		cinfo->CurrPos, pos, whence, cinfo->FileSz);

	TRC_LEAVE;

	return(cinfo->CurrPos);
}
