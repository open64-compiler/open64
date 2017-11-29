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


#pragma ident "@(#) libu/ffio/cmpwrite.c	92.3	06/29/99 13:16:47"
 
#include <ffio.h>
#include <fcntl.h>
#include <liberrno.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "cmpio.h"
 
/* Forward declarations */

int
_cmpDSeg(cmp_lyr *cinfo, struct ffsw *stat, char **cmpBuffer, int *cmpSize);

int
_writeHeader(struct fdinfo *llfio, struct ffsw *stat, int fulp, int *ubc,
	cmp_lyr *cinfo);

int
_cmpwrite(fio, bufptr, nbytes, stat, fulp, ubc)
struct 	fdinfo *fio;
int 	nbytes, fulp, *ubc;
bitptr 	bufptr;
struct 	ffsw *stat;
{
	int	dSegSize;
	int	ret, bytes;
	int	nbytesWrtn ;
	char	*buffer;
        struct  fdinfo *llfio;
	cmp_lyr *cinfo;
	char	*rtnName = "_cmpwrite";
 
        llfio = fio->fioptr;
	cinfo = (cmp_lyr *)fio->lyr_info;

	TRC_ENTER;

	/*
	 * Check that this file was opened for writing.
	 */
	if ((cinfo->OpnFlags & O_WRONLY) == 0) {
		ERETURN(stat, FDC_ERR_BADFLAGS, 0);
	}

	/* 
	 *  Write header if at beginning of file 
	 */	
	if (cinfo->WroteHeader == FALSE) {
		if (_writeHeader(llfio, stat, fulp, ubc, cinfo) == ERR) {
			return(ERR);
		}
	}

	/*
	 * If a zero or negative number of bytes is given then let the
	 * lower FFIO routines deal with it since they will most probably
	 * return an error back to the user.
	 */
	if (nbytes <= 0) {
		if (cinfo->Writea == TRUE) {
		    ret = XRCALL(llfio, writeartn) llfio, 
			     bufptr, nbytes, stat, fulp, ubc);
		} else {
		    ret = XRCALL(llfio, writertn) llfio, 
			     bufptr, nbytes, stat, fulp, ubc);
		}

		TRC_LOG("Number of bytes is %d (%d)!", nbytes, ret);
		return(ret);
	}

	/*
	 * Save the number of bytes which the user is intending to write
	 * (i.e. compress), we need this to return it back if we succeed
	 * compressing and writing this data. Also, get the size for the
	 * data segment block.
	 */
	nbytesWrtn = nbytes;
	dSegSize   = cinfo->DSegSize;

	do {
		buffer = BPTR2CP(bufptr);

		/*
		 * If the number of bytes to write is less than the data 
		 * segment size, then store these in a buffer and write
		 * them to the file later, when we receive more bytes to
		 * complete the rest of the data segment.
		 */
		if ((cinfo->BytesInBuf + nbytes) < dSegSize) {
			memcpy((void *)(cinfo->DSegBuf + cinfo->BytesInBuf),
			       (void *)buffer, nbytes);

			cinfo->BytesInBuf += nbytes;

			TRC_LOG("Copied %d bytes (%d) into DSegBuf 0x%x (0x%x)", 
				nbytes, cinfo->BytesInBuf, cinfo->DSegBuf,
				cinfo->DSegBuf + cinfo->BytesInBuf);
			break;
		}

		/*
		 * The given number of bytes is enough (or more than enough) to 
		 * complete the data segment which we've been storing. Proceed to 
		 * write the completed data segment and keep on writing under we
		 * are left with less than a data segment block size. 
		 */
		bytes = dSegSize - cinfo->BytesInBuf;
			
		memcpy((void *)(cinfo->DSegBuf + cinfo->BytesInBuf),
		       (void *)buffer, bytes);

		cinfo->BytesInBuf += bytes;

		TRC_LOG("Appended %d bytes (%d) into DSegBuf 0x%x (0x%x)", 
			 bytes, cinfo->BytesInBuf, cinfo->DSegBuf,
			 cinfo->DSegBuf + cinfo->BytesInBuf);

		if (_writeDSeg(llfio, stat, fulp, ubc, cinfo) == ERR) {
			return(ERR);
		}

		nbytes -= bytes;

		/*
		 * Increment the bit pointer by the amount of data that we
		 * just wrote to the compressed file.
		 */
		if (nbytes > 0) {
			buffer  = BPTR2CP(bufptr);
			buffer += bytes;
			bufptr  = CPTR2BP(buffer); 
		}

		TRC_LOG("Bytes to go %d", nbytes);
	} while (nbytes > 0);

	TRC_LEAVE;

        return(nbytesWrtn);
}
 
/*
 * This routine writes out the header of the compressed file. It is
 * that the header was assembled in the _cmp_open() routine and that
 * 'cinfo->InitHeader' was set to TRUE to indicate to inidicate this. 
 * On success we return the number of bytes written as part of the 
 * header, on failure we return ERR.
 */
int
_writeHeader(llfio, stat, fulp, ubc, cinfo)
	struct 	fdinfo *llfio;
	struct 	ffsw *stat;
	int 	fulp, *ubc;
	cmp_lyr	*cinfo;
{
	bitptr	bufptr;
	int	hdrSz, ret;
	char	*rtnName = "_writeHeader";

	TRC_ENTER;

	/*
 	 * Verify that indeed the header information was initialized
	 * already. If not return an error.
	 */
	if (cinfo->InitHeader == FALSE) {
		ERETURN(stat, FDC_ERR_INITHDR, 0);
	}

	bufptr = CPTR2BP(&(cinfo->Header));
	hdrSz  = sizeof(cinfo->Header.CmpFileHdr);

	/*
	 * If 'cinfo->CurrPos' is not zero that means that we are
	 * appending data to this file.
	 */
	if (cinfo->CurrPos != 0) {
		/*
		 * Check that we are indeed doing an append.
		 */
		if ((cinfo->OpnFlags & O_APPEND) == 0) {
			ERETURN(stat, FDC_ERR_BADAPPEND, 0);
		}

		TRC_LOG("Append currPos %d", cinfo->CurrPos);
	
		/*
		 * Seek to the beginning of the file and rewrite
	 	 * the header which contains the current date.
		 */
		if (XRCALL(llfio, seekrtn) llfio,
				    0, SEEK_SET, stat) < 0) {
			ERETURN(stat, FDC_ERR_SEEKBEG, 0);
		}

		ret = XRCALL(llfio, writertn) llfio, 
			    bufptr, hdrSz, stat, fulp, ubc);
		if (ret < 0) {
			ERETURN(stat, FDC_ERR_UPDHDR, 0);
		}

		/*
		 * Go to the end of the last data segment where we 
		 * are to append the rest of the data.
		 */
		TRC_LOG("AppendAddr 0x%x", cinfo->CurrPos);

		if (XRCALL(llfio, seekrtn) llfio,
			   cinfo->CurrPos, SEEK_SET, stat) < 0) {
			ERETURN(stat, FDC_ERR_SEEKEND, 0);
		}
	} else {
		ret = XRCALL(llfio, writertn) llfio, 
			   bufptr, hdrSz, stat, fulp, ubc);
		if (ret < 0) {
			ERETURN(stat, FDC_ERR_WRHDR, 0);
		}
		cinfo->CurrPos += hdrSz;
	}

	cinfo->WroteHeader = TRUE;

	TRC_LOG("Header size %d, totBytes %d (%d)", 
		hdrSz, cinfo->CurrPos, ret);
	TRC_LEAVE;

	return(ret);
}

/*
 * This routine writes out the trailer for the compressed file. In
 * addition, this routine writes out the data segment descriptor
 * cache before writing the trailer. If successful, we return the  
 * number of bytes written for the trailer, otherwise, ERR is 
 * returned on failure.
 */
int
_writeTrailer(llfio, stat, fulp, ubc, cinfo)
	struct	fdinfo *llfio;
	struct	ffsw *stat;
	int	fulp, *ubc;
	cmp_lyr *cinfo;
{
	int 	ret, cacheSize;
	int 	dSegDCacheAddress;
	char	*rtnName = "_writeTrailer";

	CmpFile	    trailer;
	int	    trailerSz;
	
	TRC_ENTER;

	/*
	 * Write out the data segment descriptor cache list.
	 */
	dSegDCacheAddress = cinfo->CurrPos;
	cacheSize = cinfo->NumDSegD * sizeof(struct DataSegD);

	TRC_LOG("Cache addr 0x%x, cache size %d, CurrPos %d", 
		 dSegDCacheAddress, cacheSize, cinfo->CurrPos);


	ret = XRCALL(llfio, writertn) llfio, 
		     CPTR2BP(cinfo->DSegDCache), cacheSize, 
		     stat, fulp, ubc);

	if (ret < 0) {
		TRC_LOG("Error %d\n", stat->sw_error);
		ERETURN(stat, FDC_ERR_WRCACHE, 0);
	}

	cinfo->CurrPos += cacheSize;

	TRC_LOG("NumDSegD %d, origFileSz %d, CurrPos %d",
	    cinfo->NumDSegD, cinfo->OrigFileSz, cinfo->CurrPos);
		
	/*
	 * Set up the trailer and write it out.
	 */
	trailerSz = sizeof(trailer);
	memset(&trailer, 0, trailerSz);

	strncpy(trailer.t_FileID, CMP_FILEID, sizeof(trailer.t_FileID));
	strncpy(trailer.t_FileMagic, CMP_FILEMAGIC, sizeof(trailer.t_FileMagic));

	strncpy(trailer.t_Date, cinfo->Header.t_Date, 8);
	strncpy(trailer.t_Time, cinfo->Header.t_Time, 8);

	trailer.t_DataSegDAddr = dSegDCacheAddress;
	trailer.t_NumDataSegD  = cinfo->NumDSegD;
	trailer.t_ActualFileSz = cinfo->OrigFileSz;

	if (cinfo->DebugLvl > DEBUG_LVL1) {
		TRC_IT("t_FileID: %0.8s", trailer.t_FileID);
		TRC_IT("t_Date:   %0.8s", trailer.t_Date);
		TRC_IT("t_Time:   %0.8s", trailer.t_Time);
		TRC_IT("t_FileMagic:   %0.8s",  trailer.t_FileMagic);
		TRC_IT("t_DataSegAddr: 0x%llx", trailer.t_DataSegDAddr);
		TRC_IT("t_NumDataSegD: %lld",   trailer.t_NumDataSegD);
		TRC_IT("t_ActualFileSz: %d",    trailer.t_ActualFileSz);

		TRC_IT("Compressed file by %.2f%%", 100.0 * 
			((float) (cinfo->OrigFileSz - cinfo->CurrPos) 
		        / (float) cinfo->OrigFileSz));
	}

	ret = XRCALL(llfio, writertn) llfio, CPTR2BP(&trailer), 
		     trailerSz, stat, fulp, ubc);

	if (ret < 0) {
		ERETURN(stat, FDC_ERR_WRTRLR, 0);
	}

	cinfo->CurrPos += trailerSz;

	TRC_LOG("Trailer size %d, CurrPos %d (%d)",
          	 trailerSz, cinfo->CurrPos, ret);

	TRC_LEAVE;

	return(ret);
}

/*
 * Write out the data segment descriptor to the compressed file.
 * This routine also udpates the cache list to contain this last
 * segment descriptor. If successful, the number of bytes written
 * for the data segment descriptor is returned, othewise, ERR is
 * returned on failure.
 */
int
_writeDSegD(llfio, stat, fulp, ubc, cinfo, cmpDSegSize)
	struct 	fdinfo *llfio;
	struct 	ffsw *stat;
	int 	fulp, *ubc;
	cmp_lyr	*cinfo;
	int	cmpDSegSize;
{
	int    ret;
	struct DataSegD *tmpCache;
	int    dSegDSize = sizeof(struct DataSegD);
	char   *rtnName = "_writeDSegD";

	TRC_ENTER;

	cinfo->NumDSegD++;
	if (cinfo->DSegDCache == NULL) {
		tmpCache = (struct DataSegD *) calloc(1, dSegDSize);
	} else {
		tmpCache = (struct DataSegD *) realloc(cinfo->DSegDCache, 
			   cinfo->NumDSegD * dSegDSize);
	}

	if (tmpCache == NULL) {
		TRC_LOG("Unable to re(allocate) tmpCache (%d, %d)",
			dSegDSize, cinfo->NumDSegD * dSegDSize);
		ERETURN(stat, FDC_ERR_DSEGD, 0);
	}
	
	cinfo->DSegDCache = tmpCache;
	tmpCache += cinfo->NumDSegD - 1;

	memset(tmpCache, 0, dSegDSize);

	strncpy(tmpCache->SegmentID, CMP_FILEMAGIC, 8);

	tmpCache->FileAddr    = cinfo->CurrPos + dSegDSize;
	tmpCache->Length      = cinfo->BytesInBuf;
	tmpCache->CmpLength   = cmpDSegSize;
	tmpCache->Compressed  = (cinfo->BytesInBuf > cmpDSegSize) ? 
				TRUE : FALSE;
	tmpCache->NextDataSegD= tmpCache->FileAddr + cmpDSegSize;;
	
	TRC_LOG("dSegDSize = %d, NumDSegD %d", dSegDSize, 
		 cinfo->NumDSegD);
	TRC_LOG("TmpCache FileAddr 0x%llx, Length %d, CmpLength %d",
		 tmpCache->FileAddr, tmpCache->Length, 
		 tmpCache->CmpLength);
	TRC_LOG("TmpCache SegID %0.8s Comp %d, NextDataSegD 0x%llx",
		 tmpCache->SegmentID, tmpCache->Compressed, 
		 tmpCache->NextDataSegD);

	/*
	 * Write the data segment descriptor.
	 */
	if (cinfo->Writea) {
		ret = XRCALL(llfio, writeartn) llfio, CPTR2BP(tmpCache), 
			dSegDSize, stat, fulp, ubc);
	} else {
		ret = XRCALL(llfio, writertn) llfio, CPTR2BP(tmpCache), 
			dSegDSize, stat, fulp, ubc);
	}

	if (ret < 0) {
		TRC_LOG("Error %d\n", stat->sw_error);
		ERETURN(stat, FDC_ERR_DSEGD, 0);
	}

	/*
	 * Increment the compressed file size by the size of the
	 * data segment descriptor.
	 */
	cinfo->CurrPos += dSegDSize;

	TRC_LOG("tmpCache 0x%x, CurrPos %d, origFileSz %d (%d)",
	     tmpCache, cinfo->CurrPos, cinfo->OrigFileSz, ret);

	TRC_LEAVE;

	return(ret);
}

/*
 * Writes a compressed data segment to file. This routine also writes
 * the data segment descriptor for this data segment. The appropriate
 * compression routine is called to compress the data. If successful,
 * we return the number of compressed data bytes written to file. On
 * a failure ERR is returned.
 */
int
_writeDSeg(llfio, stat, fulp, ubc, cinfo)
	struct	fdinfo *llfio;
	struct 	ffsw *stat;
	int 	fulp, *ubc;
	cmp_lyr	*cinfo;
{
	int	ret, cmpSize;
	char	*cmpBuffer = NULL;
	char	*rtnName = "_writeDSeg";

	TRC_ENTER;

	/*
 	 * Compress data segment.
	 */
	if (!_cmpDSeg(cinfo, stat, &cmpBuffer, &cmpSize)) {
		return(ERR);
	}

	/*
	 * Write the data segment descriptor.
	 */
	if ((ret = _writeDSegD(llfio, stat, fulp, 
			       ubc, cinfo, cmpSize)) == ERR) {
		return(ERR);
	}

	/*
	 * Write the compressed data segment.
	 */
	if (cinfo->Writea) {
		ret = XRCALL(llfio, writeartn) llfio, 
			CPTR2BP(cmpBuffer), cmpSize, stat, fulp, ubc);
	} else {
		ret = XRCALL(llfio, writertn) llfio, 
			CPTR2BP(cmpBuffer), cmpSize, stat, fulp, ubc);
	}

	if (ret < 0) {
		ERETURN(stat, FDC_ERR_DSEG, 0);
	}

	/*
	 * Increment the compressed file size by the size of the
	 * compressed data segment. This is given by the CurrPos
	 * field which is basically the number of bytes written.
	 */
	cinfo->CurrPos    += cmpSize;
	cinfo->OrigFileSz += cinfo->BytesInBuf;
	cinfo->BytesInBuf  = 0;

	if (NULL != cmpBuffer) {
		free(cmpBuffer);
	}

	TRC_LOG("cmpSize %d, origFileSz %d, CurrPos %d (%d)",
	        cmpSize, cinfo->OrigFileSz, cinfo->CurrPos, ret);
	TRC_LEAVE;

	return (ret);
}

/*
 * Compresses a data segment and returns it in a buffer.
 * This routine returns 1 if successful and 0 otherwise.
 */
int
_cmpDSeg(cinfo, stat, cmpBuffer, cmpSize)
	cmp_lyr     *cinfo;
	struct ffsw *stat;
	char	   **cmpBuffer;
	int	    *cmpSize;
{
	int	ret;
	char	*rtnName = "_cmpDSeg";

	TRC_ENTER;

	*cmpBuffer = NULL;

	ret = _lz_compress(cinfo, cinfo->DSegBuf, 
		          cinfo->BytesInBuf, cmpBuffer, cmpSize);

	if ((ret != 0) && (ret != FDC_ERR_LZ_NOCOMP)) {
		_SETERROR(stat, ret, 0);
		return(0);
	} 

	/*
	 * If there was no compression done then return a copy 
	 * of the original buffer back.
	 */
	if (ret == FDC_ERR_LZ_NOCOMP) {
		*cmpBuffer = (char *) calloc(cinfo->BytesInBuf, sizeof(char));

		if (*cmpBuffer == NULL) {
			_SETERROR(stat, FDC_ERR_DSEGD, 0);
			return(0);
		}

		memcpy((void *)(*cmpBuffer),
		       (void *)cinfo->DSegBuf, cinfo->BytesInBuf);

		*cmpSize = cinfo->BytesInBuf;
		
		TRC_LOG("No compression required");
	}

	/*
	 * If the debug level is set to DEBUG_LVL1 then we check that the
	 * the ompression routine is working correctly. We decompress the
	 * buffer and verify that it isi the same as the previous one.
	 */
	if ((ret == 0) && (cinfo->DebugLvl == DEBUG_LVL1)) {
		char	*dcmpBuffer = NULL;

		ret = _lz_decompress(cinfo, *cmpBuffer, 
			*cmpSize, cinfo->BytesInBuf, &dcmpBuffer);
		
		if ((ret == 0) && memcmp((void *) cinfo->DSegBuf, 
			(void *) dcmpBuffer, cinfo->BytesInBuf)) {
			ret = FDC_ERR_LZ_BADCOMP;
		}
	
		if (ret) {
			TRC_LOG("Bad block #%d\n", cinfo->NumDSegD + 1);
			_SETERROR(stat, ret, 0);
			return(0);
		}

		free(dcmpBuffer);
	}

	TRC_LOG("Compression rate %f%% (%d, %d)",
		100.0 * (float) ((cinfo->BytesInBuf - *cmpSize) /
		(float) cinfo->BytesInBuf), cinfo->BytesInBuf, *cmpSize);

	TRC_LOG("BytesInBuf %d, cmpSize %d", 
			cinfo->BytesInBuf, *cmpSize);
	TRC_LEAVE;

	return(1);
}

/*
 * write requests
 *
 * Parameters:
 *  fio     - Pointer to fdinfo block
 *  bufptr  - bit pointer to where data is to go.
 *  nbytes  - Number of bytes to be written
 *  stat    - pointer to status return word
 *  fulp    - full or partial write mode flag
 *  ubc     - pointer to unused bit count (not used for IBM)
 */

int
_cmp_write(fio, bufptr, nbytes, stat, fulp, ubc)
struct  fdinfo *fio;
int     nbytes, fulp, *ubc;
bitptr  bufptr;
struct  ffsw *stat;
{
	int	ret;
	cmp_lyr	*cinfo;
	char	*rtnName = "_cmp_write";

	cinfo = (cmp_lyr *)fio->lyr_info;
	cinfo->Writea = FALSE;

	TRC_ENTER;

	ret = _cmpwrite(fio, bufptr, nbytes, stat, fulp, ubc);

	TRC_LEAVE;

	return(ret);
}

/*
 * writea requests
 *
 * Parameters:
 *  fio     - Pointer to fdinfo block
 *  bufptr  - bit pointer to where data is to go.
 *  nbytes  - Number of bytes to be written
 *  stat    - pointer to status return word
 *  fulp    - full or partial write mode flag
 *  ubc     - pointer to unused bit count (not used for IBM)
 */

int
_cmp_writea(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
{
	int	ret;
	cmp_lyr	*cinfo;
	char	*rtnName = "_cmp_writea";

	cinfo = (cmp_lyr *)fio->lyr_info;
	cinfo->Writea = TRUE;

	TRC_ENTER;

	ret = _cmpwrite(fio, bufptr, nbytes, stat, fulp, ubc);

	TRC_LEAVE;

	return(ret);
}

/*
 * Flush the buffer and clean up
 * This routine should return 0, or -1 on error.
 */
int
_cmp_flush(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
        int      ret;
	cmp_lyr *cinfo;
        struct   fdinfo *llfio;
	char	*rtnName = "_cmp_flush";
 
        llfio = fio->fioptr;
	cinfo = (cmp_lyr *)fio->lyr_info;

	TRC_ENTER;

        ret = XRCALL(llfio, flushrtn) llfio, stat);

	TRC_LEAVE;

        return(ret);
}
 

/*
 * trace WEOF calls
 *
 * The EOF is a very specific concept.   Don't confuse it with the
 * UNICOS EOF, or the trunc(2) system call.
 */
int
_cmp_weof(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
        int      ret;
	cmp_lyr *cinfo;
        struct   fdinfo *llfio;
	char	*rtnName = "_cmp_weof";
 
        llfio = fio->fioptr;
	cinfo = (cmp_lyr *)fio->lyr_info;
 
	TRC_ENTER;

	ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret < 0) {
		goto done;
	}

        ret = XRCALL(llfio, weofrtn) llfio, stat);

done:
	TRC_LEAVE;

        return(ret);
}
 
/*
 * trace WEOD calls
 *
 * The EOD is a very specific concept.  Don't confuse it with the UNICOS
 *  EOF.  It is usually mapped to the trunc(2) system call.
 */
int
_cmp_weod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
        int      ret;
	cmp_lyr *cinfo;
        struct   fdinfo *llfio;
	char	*rtnName = "_cmp_weod";
 
        llfio = fio->fioptr;
	cinfo = (cmp_lyr *)fio->lyr_info;
 
	TRC_ENTER;

	ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret < 0) {
		goto done;
	}

        ret = XRCALL(llfio, weodrtn) llfio, stat);

done:
	TRC_LEAVE;

        return(ret);
}
