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


#pragma ident "@(#) libu/ffio/cmpopen.c	92.2	06/29/99 13:16:47"

#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <malloc.h>
#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include <sys/stat.h>
#include "fxlist.h"
#include "cmpio.h"

DECLARE(CMP_XLIST);
struct xtr_s CMP_XLIST_P = { CMP_XLIST };
 
/* Forward references */

int
_initAppend(struct fdinfo *nfioptr, char *name, union spec_u *nspec,
	int mode, int cbits, struct ffsw *stat, int cblks,
	struct gl_o_inf *oinf, cmp_lyr *cinfo);

int
_parse_specs(union spec_u **spec_arg, struct ffsw *stat, cmp_lyr *cinfo);

int 
_validateFile(struct fdinfo *nfioptr, struct ffsw *stat, cmp_lyr *cinfo);

/*
 * The compression user layer accepts the following options when
 * writing or reading from a compressed file:
 *
 * "cmp:segSz:cacheSz:debug"
 *
 * segSize	
 *
 * Size of data segments in bytes, if none is given then we default to 
 * DEFAULT_SEGSZ. This value should be between MIN_SEGSZ and MAX_SEGSZ. 
 * This option is only valid if	we write to a compressed file. If we 
 * are reading from a compressed file then this value is overwritten 
 * by the segment size given in the compressed file header.
 *
 * cacheSize	
 *
 * Size of uncompressed data segment buffers to use when reading from
 * a compressed data file. If none is given we default to using ten
 * buffers (UNCMP_BUFFER_NUM). This field is only used when reading
 * from a compressed file. This value must be greater than zero.
 *
 * debug
 *
 * The level of debug information to print out to the screen. The
 * possible values are:
 *
 *
 * DBG_LVL0  No debug. This is the default value.
 *
 * DBG_LVL1  Checks the the compression algorithm is working well.
 *
 * DBG_LVL2  Prints statistical information about the compressed file
 *	     such as the percentage of compression from the original
 *	     file (writing only), the hits/misses for the uncompressed
 *	     data segment cache (reading only), and the value for the
 * 	     header and trailer in the compressed file. 
 *		
 * DBG_LVL3  Print traces when entering or leaving a compression layer
 *	     routine (i.e. TRC_ENTER and TRC_LEAVE macros).
 *
 * DBG_LVL4  Print detailed information about every possible trace in
 *	     the compression layer routines. The amount of info that
 *	     gets printed may be overwhelming.
 */

int
_cmp_open(name, flags, mode, fio, spec, stat, cbits, cblks, oinf)
	char 	      *name;
	int 	       flags, mode, cbits, cblks;
	struct fdinfo *fio;
	union spec_u  *spec;
	struct ffsw   *stat;
	struct gl_o_inf *oinf;	     
{
	_ffopen_t  nextfio;
	cmp_lyr   *cinfo = NULL;
	int	   opnFlags;
	char	  *rtnName = "_cmp_open";

	union  spec_u  *nspec;
	struct fdinfo  *nfioptr;
	struct ffsw     clstat;

	/* 
	 * Allocate and initialize the compression layer specific 
	 * data area.
	 */
	cinfo = (cmp_lyr *) calloc(1, sizeof(cmp_lyr));
	if (cinfo == NULL) {
		_SETERROR(stat, FDC_ERR_CMPLYR, 0);
		return(_FFOPEN_ERR);
	}

	fio->lyr_info = (char *) cinfo;

	cinfo->DSegDCache  = NULL;
	cinfo->WroteHeader = FALSE;
	cinfo->InitHeader  = FALSE;
	cinfo->OpnFlags    = flags;
	cinfo->DbgOpenCnt  = 0;

	/*
	 * Get the user given options to the compression layer.
	 */
	if (_parse_specs(&spec, stat, cinfo) == ERR) {
		return(_FFOPEN_ERR);
	}

	/* 
	 * Get the FFIO spec for the next lower layer and then
	 * open the lower layers.
	 */
	nspec = spec;
	NEXT_SPEC(nspec);

	/*
	 * Open trace file, if one has been specified through the
	 * FF_CMP_TRACE_FILE environment variable or if a debug
	 * level has been given. Because we rely on the debug level
	 * in the 'cinfo->DbgLvl' field we must invoke this routine
	 * after the compression layer arguments have been parsed.
	 */
	if (_OpenTraceFile(stat, cinfo) == ERR) {
		return(_FFOPEN_ERR);
	}

	TRC_ENTER;

	TRC_LOG("Open flags 0%o, name %s", cinfo->OpnFlags, name);

	/*
	 * The user has to open the file for reading, writing or
	 * read/write. If none of these flags are given then read
	 * is assumed as default.
	 */
	if (cinfo->OpnFlags & O_WRONLY) {
writeOnly:
		/*
		 * Allocate a buffer to contain the uncompressed data
		 * which is to be written to the file.
		 */
		if ((cinfo->DSegBuf = (char *) 
			calloc(cinfo->DSegSize, sizeof(char))) == NULL) {
			ERETURN(stat, FDC_ERR_DSEGBUF, 0);
		}
	
		if ((cinfo->OpnFlags & O_CREAT) || 
		    (cinfo->OpnFlags & O_TRUNC)) {
		    /*
	                nextfio = __ffopen(name, cinfo->OpnFlags,
						 mode, nspec, stat, cbits);
		     */
			nextfio = _ffopen(name, cinfo->OpnFlags, mode, nspec,
					stat, cbits, cblks, NULL, oinf);
                	if (nextfio == _FFOPEN_ERR) {
				goto badopen;
			}
			nfioptr = (struct fdinfo *) nextfio;

			/*
 			 * We are going to be writing a compressed file so
			 * load a header to write at the top of the file.
			 */
			_initHeader(cinfo);
		} else if (cinfo->OpnFlags & O_APPEND) {
			/*
			 * Turn off the O_APPEND flag from what the user passed,
			 * otherwise, we won't be able to update the header or
			 * write to the last data segment (i.e. if this happened
			 * to be less than the data segment size).
			 */
			opnFlags = cinfo->OpnFlags & ~(O_APPEND);

			/*
                        nextfio = __ffopen(name, opnFlags, 
						 mode, nspec, stat, cbits); 
			*/
			nextfio = _ffopen(name, opnFlags, mode, nspec, stat,
					cbits, cblks, NULL, oinf);
                        if (nextfio == _FFOPEN_ERR) {
			        goto badopen;
                        }
			nfioptr = (struct fdinfo *) nextfio;

			/*
			 * Now, update the time stamp in the header and then
			 * seek to the beginning to the data segment descrip-
			 * tor cache to start appending data.
			 */
			if (_initAppend(nfioptr, name, nspec, mode, 
				cbits, stat, cblks, oinf, cinfo) == ERR) {
				goto close;
			}
		} else { /* Return an error */
			_SETERROR(stat, FDC_ERR_BADFLAGS, 0);
			goto badopen;
		}

	} else { /* Either O_RDONLY or O_RDWR where given */
		/*
		 * If the O_APPEND, O_CREAT or O_TRUNC flags are passed 
		 * assume that we can only write to the file so goto the
		 * code above to open the file for writing only.
		 */
		if (cinfo->OpnFlags & (O_APPEND | O_CREAT | O_TRUNC)) {
			cinfo->OpnFlags &= ~(O_RDWR);
			cinfo->OpnFlags |= O_WRONLY;
			goto writeOnly;
		}
		
		/*
		 * Assume that the file is open for read only.
		 */
		/*
		nextfio = __ffopen(name, cinfo->OpnFlags, 
					 mode, nspec, stat, cbits);
		*/
		nextfio = _ffopen(name, cinfo->OpnFlags, mode, nspec, 
				stat, cbits, cblks, NULL, oinf);

		if (nextfio == _FFOPEN_ERR) {
			goto badopen;
		}
		nfioptr = (struct fdinfo *) nextfio;
		
		/*
		 * The file must exist so validate it to make sure that
		 * it is in compressed format. If it isn't then return
		 * an error.
		 */
		if (_validateFile(nfioptr, stat, cinfo) == ERR) {
			goto close;
		}
	}

	TRC_LEAVE;
	return(nextfio);

close:
	(void) XRCALL(nfioptr, closertn) nfioptr, &clstat);
	free(nfioptr);

badopen:
	if (fio->lyr_info != NULL) {
		_free_cmp_lyr((cmp_lyr *)fio->lyr_info);
		fio->lyr_info = NULL;
	}

	TRC_LEAVE;

	return(_FFOPEN_ERR);
}


/*
 * This function frees all the memory allocated to the compress
 * layer structure.
 */
void
_free_cmp_lyr(cmp_lyr *cinfo)
{
	int 	 i;
	char	*rtnName = "_free_cmp_lyr";

	TRC_ENTER;

	if (cinfo == NULL) {
		TRC_LOG("cmp_lyr cinfo field is NULL");
		return;
	}

	if (cinfo->DSegDCache != NULL) {
		free(cinfo->DSegDCache);
		TRC_LOG("Freed cinfo->DSegDCache");
	}

	if (cinfo->DSegBuf != NULL) {
		free(cinfo->DSegBuf);
		TRC_LOG("Freed cinfo->DSegBuf");
	}

	if (cinfo->UnCmpSegList != NULL) {
		for (i = 0; i < cinfo->DSegCacheSize; i++) {
			if (cinfo->UnCmpSegList[i].Buffer != NULL) {
				free(cinfo->UnCmpSegList[i].Buffer);
			}
		}
		free(cinfo->UnCmpSegList);
		TRC_LOG("Freed cinfo->UnCmpSegList");
	}

	free(cinfo);
	TRC_LOG("Freed cmp_lyr cinfo structure");

	TRC_LEAVE;
}

/*
 * This function loads the 'CmpFile' union of the cinfo structure.
 */
void
_initHeader(cinfo)
	cmp_lyr     *cinfo;
{
	time_t	   tbuf;
	struct tm *timeSt;
	int	   hdrSz;
	CmpFile    header = cinfo->Header;
	char	  *rtnName = "_initHeader";

	TRC_ENTER;

	hdrSz = sizeof(cinfo->Header.CmpFileHdr);
	
        strncpy(header.h_FileID, CMP_FILEID, sizeof(header.h_FileID));
        strncpy(header.h_FileMagic, CMP_FILEMAGIC, sizeof(header.h_FileMagic));

	tzset();
	time(&tbuf);
	timeSt = localtime(&tbuf);

	timeSt->tm_mon++; /* month returned is 0-11 */  
	timeSt->tm_year %= 100; /* watch out for Y2K! */

	sprintf(header.h_Date, "%02d/%02d/%02d", timeSt->tm_mon, 
		timeSt->tm_mday, timeSt->tm_year);

	sprintf(header.h_Time, "%02d:%02d:%02d", timeSt->tm_hour, 
		timeSt->tm_min, timeSt->tm_sec);


	strncpy(header.h_TransType, cinfo->Trans, 8);

	header.h_DataSegSz = cinfo->DSegSize;
	header.h_FirstDataSegD = hdrSz;

	memcpy((void *)&(cinfo->Header), (void *)&header, hdrSz); 

	if (cinfo->DebugLvl > DEBUG_LVL1) {
		TRC_IT("h_FileID: %0.8s", cinfo->Header.h_FileID); 
		TRC_IT("h_Date:   %0.8s", cinfo->Header.h_Date); 
		TRC_IT("h_Time:   %0.8s", cinfo->Header.h_Time); 
		TRC_IT("h_DataSegSz: %lld",  cinfo->Header.h_DataSegSz); 
		TRC_IT("h_FileMagic: %0.8s", cinfo->Header.h_FileMagic); 
		TRC_IT("h_TransType: %0.8s", cinfo->Header.h_TransType); 
		TRC_IT("h_FirstDataSegD: 0x%llx", 
					 cinfo->Header.h_FirstDataSegD); 
	}

	cinfo->InitHeader = TRUE;

	TRC_LEAVE;
}


/*
 * This function validates the header and the trailer of the input file 
 * by verifying the 'FileID' and the 'FileMagic' fields of the header 
 * and trailer. This routine also rebuilds the data segment descriptor
 * cache from the information in the compressed file. If the file is
 * corrupted we try and recover as much data segment information as it
 * is possible by sequentially parsing the data segment descriptors
 * until we hit the descriptor cache or reach a bad descriptor.
 */
int 
_validateFile(nfioptr, stat, cinfo) 
	struct fdinfo  *nfioptr;
	struct ffsw    *stat;
	cmp_lyr        *cinfo;
{
	int	     cacheSz;
	int	     hdrSz, trlSz;
	CmpFile      header, trailer;
	int 	     ubc = 0;
	long	     fileAddr = 0;
	int	     errorFound = 0;
	int	     dSegDSz = sizeof(struct DataSegD);
	char	     *rtnName = "_validateFile";

	struct DataSegD *curDSegD;

	TRC_ENTER;

	/*
	 * Read and validate the header information
	 */
	hdrSz  = sizeof(header.CmpFileHdr);

	if (XRCALL(nfioptr, readrtn) nfioptr, 
	    CPTR2BP(&header), hdrSz, stat, FULL, &ubc) < 0) {
		ERETURN(stat, FDC_ERR_RDHDR, 0);
	}

        if ((strncmp(CMP_FILEID, header.h_FileID, 8) != 0) &&
	    (strncmp(CMP_FILEMAGIC, header.h_FileMagic, 8) != 0)) {
		ERETURN(stat, FDC_ERR_BADHDR, 0);
        }

	if (strncmp(LZW_STRING, header.h_TransType, 8)) {
		ERETURN(stat, FDC_ERR_BADTRANS, 0);
	}

	memcpy((void *)&(cinfo->Header), (void *)&header, hdrSz); 
	cinfo->InitHeader = TRUE;

	if (cinfo->DebugLvl > DEBUG_LVL1) {
		TRC_IT("h_FileID: %0.8s", cinfo->Header.h_FileID); 
		TRC_IT("h_Date:   %0.8s", cinfo->Header.h_Date); 
		TRC_IT("h_Time:   %0.8s", cinfo->Header.h_Time); 
		TRC_IT("h_DataSegSz: %lld",  cinfo->Header.h_DataSegSz); 
		TRC_IT("h_FileMagic: %0.8s", cinfo->Header.h_FileMagic); 
		TRC_IT("h_TransType: %0.8s", cinfo->Header.h_TransType); 
		TRC_IT("h_FirstDataSegD: 0x%llx", 
				         cinfo->Header.h_FirstDataSegD); 
	}

	/*
	 * Load the data segment size and the transformation type
	 * from the header. This invalidates any values passed as
	 * arguments in the user layer invocation string.
	 */
	strncpy(cinfo->Trans, cinfo->Header.h_TransType, 8);
	cinfo->DSegSize = cinfo->Header.h_DataSegSz;

	/*
 	 * Seek to the end of the file and read the trailer.
	 */
	trlSz = sizeof(trailer.CmpFileTrl);
	fileAddr = XRCALL(nfioptr, seekrtn) 
		          nfioptr, 0 - trlSz, SEEK_END, stat);

	if (fileAddr < 0) {
		ERETURN(stat, FDC_ERR_SEEKTRL, 0);
	}

	if (XRCALL(nfioptr, readrtn) nfioptr, CPTR2BP(&trailer), 
	    trlSz, stat, FULL, &ubc) < 0) {
		ERETURN(stat, FDC_ERR_RDTRL, 0);
	}

	/* Validate integrity of trailer. */

        if ((strncmp(CMP_FILEID, trailer.t_FileID, 8) != 0) ||
	    (strncmp(CMP_FILEMAGIC, trailer.t_FileMagic, 8) != 0)) {
		errorFound++;
        } else if (strncmp(trailer.t_Date, header.t_Date, 8) != 0) {
		errorFound++;
	}

	if (errorFound == 0) {
		if (cinfo->DebugLvl > DEBUG_LVL1) {
			TRC_IT("t_FileID: %0.8s", trailer.t_FileID);
			TRC_IT("t_Date:   %0.8s", trailer.t_Date);
			TRC_IT("t_Time:   %0.8s", trailer.t_Time);
			TRC_IT("t_FileMagic:   %0.8s",  trailer.t_FileMagic); 
			TRC_IT("t_DataSegAddr: 0x%llx", trailer.t_DataSegDAddr);
			TRC_IT("t_NumDataSegD: %lld",   trailer.t_NumDataSegD);
			TRC_IT("t_ActualFileSz: %d",    trailer.t_ActualFileSz);
		}

		/*
		 * Load the data segment descriptor cache from the end
		 * of the compressed file and validate its integrity. 
		 * But first, check if the address for the descriptor
		 * cache is a valid one.
	         *
		 */
		fileAddr -= (dSegDSz * trailer.t_NumDataSegD);

		if (fileAddr == trailer.t_DataSegDAddr) {
			if (XRCALL(nfioptr, seekrtn) nfioptr, 
				     fileAddr, SEEK_SET, stat) < 0) {
				ERETURN(stat, FDC_ERR_SEEKCACHE, 0);
			}

			cinfo->DSegDCache = (struct DataSegD *) 
				calloc(trailer.t_NumDataSegD, dSegDSz);

			if (cinfo->DSegDCache == NULL) {
				ERETURN(stat, FDC_ERR_CACHE, 0);
			}

			cacheSz = trailer.t_NumDataSegD * dSegDSz;

			if (XRCALL(nfioptr, readrtn) nfioptr,
			    CPTR2BP(cinfo->DSegDCache), cacheSz, stat, 
			    FULL, &ubc) < 0) {
				ERETURN(stat, FDC_ERR_RDCACHE, 0);
			}

			if (strncmp(cinfo->DSegDCache->SegmentID, 
				    CMP_FILEMAGIC, 8)) {
				ERETURN(stat, FDC_ERR_BADCACHE, 0);
			}

			cinfo->NumDSegD = trailer.t_NumDataSegD;

			/*
			 * Compute the file size from the data segment
			 * descriptors and verify that it's the same as
			 * that given in the trailer.
			 */
			cinfo->FileSz = 
				(cinfo->NumDSegD - 1) * header.h_DataSegSz;
			cinfo->FileSz += 
			    cinfo->DSegDCache[cinfo->NumDSegD - 1].Length;

			if (cinfo->FileSz != trailer.t_ActualFileSz) {
				ERETURN(stat, FDC_ERR_FILESZ, 0);
			}

			TRC_LOG("Cached DSegDs %d, file size %d", 
				cinfo->NumDSegD, cinfo->FileSz);
			goto done;
		}
	}

	/*
	 * At this point we know that the the compressed file is not
	 * healthy. Get the location of the first data segment 
	 * descriptor and then start looking for segment descriptors
	 * until some validation error, an EOF, or the descriptor
	 * cache is  encountered. We want to build a cache of these 
	 * segment descriptors.
	 */
	fileAddr = header.h_FirstDataSegD;

	for(;;) {
		cacheSz = (cinfo->NumDSegD + 1) * dSegDSz;
		cinfo->DSegDCache = (struct DataSegD *)
				    realloc(cinfo->DSegDCache, cacheSz);

		if (cinfo->DSegDCache == NULL) {
			ERETURN(stat, FDC_ERR_CACHE, 0);
		}
	
		curDSegD = cinfo->DSegDCache + cinfo->NumDSegD;

		if (XRCALL(nfioptr, seekrtn) nfioptr, 
		     	   fileAddr, SEEK_SET, stat) < 0) {
			break;
		}

		if (XRCALL(nfioptr, readrtn) nfioptr, 
		    CPTR2BP(curDSegD), dSegDSz, stat, FULL, &ubc) < 0) {
			break;
		}

		/*
		 * Check the validity of the segment by looking at
		 * its 'SegmentID' field.
		 */
		if (strncmp(curDSegD->SegmentID, CMP_FILEMAGIC, 8)) {
			break;
		}

		/*
		 * Check if we found the first segment descriptor in 
		 * the segment cache list. If so, we are done.
	 	 */
		if (curDSegD->FileAddr < fileAddr) {
			break;
		}

		cinfo->NumDSegD++;
		cinfo->FileSz += curDSegD->Length;
		fileAddr = curDSegD->NextDataSegD;
	}

	TRC_INFO("DSegDs found %d, file size %d", 
		     cinfo->NumDSegD, cinfo->FileSz);

	if (cinfo->NumDSegD <= 0) {
		ERETURN(stat, FDC_ERR_NODSEGD, 0);
	}

done:
	/*
	 * Allocate memory for the uncompressed buffer cache.
	 */
	if ((cinfo->UnCmpSegList = (UnCmpSeg *) 
	     calloc(cinfo->DSegCacheSize, sizeof(UnCmpSeg))) == NULL) {
		ERETURN(stat, FDC_ERR_UNCMPSEGLIST, 0);
	}
	
	TRC_LEAVE;

	return (0);
}

int
_parse_specs(spec_arg, stat, cinfo)
	union spec_u  **spec_arg;
	struct ffsw    *stat;
	cmp_lyr	       *cinfo;
{
	int   segSize; 
	int   value, isvalid;	
	int   cacheSize, debugLvl;

	union spec_u *spec = *spec_arg;
	char *rtnName = "_parse_specs";

	value = (int) _ff_nparm_getv(spec, 1, &isvalid);
	segSize = (!isvalid) ? DEFAULT_SEGSZ : value;

	if ((segSize < MIN_SEGSZ) || (segSize > MAX_SEGSZ)) {
		_SETERROR(stat, FDC_ERR_SEGSIZ, 0);
		return(ERR);
	}

	value = (int) _ff_nparm_getv(spec, 2, &isvalid);
	cacheSize = (!isvalid) ? UNCMP_BUFFER_NUM : value;

	if (cacheSize <= 0) {
		_SETERROR(stat, FDC_ERR_CACHESZ, 0);
                return(ERR);
        }
	
	value = (int) _ff_nparm_getv(spec, 3, &isvalid);
	debugLvl = (!isvalid) ? DEBUG_LVL0 : value;

	if ((debugLvl < DEBUG_LVL0) || (debugLvl > DEBUG_LVL_MAX)) {
		_SETERROR(stat, FDC_ERR_DEBUGLVL, 0);
                return(ERR);
        }

	cinfo->DSegSize = segSize;
	strncpy(cinfo->Trans, LZW_STRING, 8);
	cinfo->DSegCacheSize = cacheSize;
	cinfo->DebugLvl = debugLvl;

	TRC_LOG("Debug Level: %d (%d,%d)", debugLvl, value, isvalid);
	TRC_LOG("Segment size: %d (%d,%d)", segSize, value, isvalid);
	TRC_LOG("Cache size: %d (%d,%d)", cacheSize, value, isvalid);

	TRC_LEAVE;
	return(1);
}

/*
 * This routine updates the time stamp in the header and then seeks to
 * the beginning to the data segment descriptor cache to start appending 
 * data segments.
 */
int
_initAppend(nfioptr, name, nspec, mode, cbits, stat, cblks, oinf, cinfo)
	struct fdinfo   *nfioptr;
	char	        *name;
	struct ffsw     *stat;
	union  spec_u   *nspec;
	int		 mode, cbits;
	int		 cblks;
	struct gl_o_inf *oinf;
	cmp_lyr         *cinfo;
{
	time_t 		 tbuf;
	struct tm 	*timeSt;
	_ffopen_t 	 nextfio;
	char		*unCmpSeg;
	struct fdinfo 	*fioptr;
	struct DataSegD	 lastDSegD;
	char		*rtnName = "_initAppend";

	TRC_ENTER;

	/*
	 * Validate the compressed file. If there is a problem with the
	 * compressed file format then return an error. We need to open
	 * the file for reading first ... YUCK!
	 */
	nextfio = _ffopen(name, O_RDONLY, mode, nspec, stat, 
			    cbits, cblks, NULL, oinf);
	if (nextfio == _FFOPEN_ERR) {
		ERETURN(stat, FDC_ERR_APPOPEN, 0);	
	}
	fioptr = (struct fdinfo *) nextfio;

	if (_validateFile(fioptr, stat, cinfo) == ERR) {
		return(ERR);
	}

	lastDSegD = cinfo->DSegDCache[cinfo->NumDSegD-1];

	TRC_LOG("Append 0x%x, file size %d", cinfo->CurrPos, cinfo->FileSz);
	TRC_LOG("lastDSegD: cmplen %d, len %d, addr 0x%x, next 0x%x, %d",
		 lastDSegD.CmpLength, lastDSegD.Length, lastDSegD.FileAddr,
		 lastDSegD.NextDataSegD, lastDSegD.Compressed);

	/*
	 * Now check the last data segment and see if it is partially
	 * filled with data. If it is, we want to keep on writing to
	 * it until we fill this data segment up to the given segment
	 * size. The first thing to do is to read it from the compressed
	 * file and uncompress it ... fun!!!
	 */
	if (lastDSegD.CmpLength < cinfo->Header.h_DataSegSz) {
		if ((unCmpSeg = _unCmpDSeg(fioptr, stat, 
					lastDSegD, cinfo)) == NULL) {
			return(ERR);
		}

		cinfo->BytesInBuf = lastDSegD.Length;	
		memcpy((void *)cinfo->DSegBuf, 
		       (void *)unCmpSeg, cinfo->BytesInBuf);

		free(unCmpSeg);		

		/*
		 * Decrease the number of data segment descriptors by
		 * one to allow the _writeDSegD() routine to rewrite
		 * this data segment descriptor.
		 */
		cinfo->NumDSegD--;

		/*
		 * Set the compressed file position to the beginning of
		 * the data segment descriptor. This will need to be
		 * rewritten to contain the added data to this segment.
		 */
		cinfo->CurrPos = lastDSegD.FileAddr - 
				 sizeof(struct DataSegD);
		cinfo->OrigFileSz = cinfo->FileSz - cinfo->BytesInBuf;

		TRC_LOG("BytesInBuf %d, NumDSegD %d, CurrPos %d", 
			 cinfo->BytesInBuf, cinfo->NumDSegD, cinfo->CurrPos);
	} else {
		/*
		 * Set the compressed file position to point where the
		 * the last data segment ends in the compressed file.
		 * The next data segment descriptor will be written after
		 * this.
		 */
		cinfo->CurrPos = lastDSegD.FileAddr + lastDSegD.CmpLength;
		cinfo->OrigFileSz = cinfo->FileSz;
	}
	
	/*
	 * Now that we've done all the reading we needed go ahead
	 * and close the file.
	 */
	if (XRCALL(fioptr, closertn) fioptr, &stat) < 0) {
		ERETURN(stat, FDC_ERR_APPCLOSE, 0);
	}

	/*
	 * Update the header with the current date. The header is 
	 * then written when the ffwrite() routine is first called.
	 */
	tzset();
	time(&tbuf);
	timeSt = localtime(&tbuf);

	timeSt->tm_mon++; /* month returned is 0-11*/
	timeSt->tm_year %= 100; /* watch out for Y2K! */

	sprintf(cinfo->Header.h_Date, "%02d/%02d/%02d", 
		timeSt->tm_mon, timeSt->tm_mday, timeSt->tm_year);

	sprintf(cinfo->Header.h_Time, "%02d:%02d:%02d", 
		timeSt->tm_hour, timeSt->tm_min, timeSt->tm_sec);

	TRC_LEAVE;

	return(1);
}
