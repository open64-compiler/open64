/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/txtbksp.c	92.1	06/29/99 13:16:47"	
#include <stdio.h>
#include <ffio.h>
#include <string.h>
#include <cray/nassert.h>
#include "txtio.h"
#define ATBOF 1
#define ATEOR 2
#define ATEOF 3
#define ATREC 4
#define EORLEN 8

static int iseof(bitptr p, struct text_f *text_info);
static int iseor(bitptr p, struct text_f *text_info);

/*
 * Backspace TEXT records
 *
 *	Records and blocks are supported only in multiples of 8 bits.
 *	There is no logical limit for the maximum record size.  The 
 *	maximum block size is determined arbitrarily to be 512 words.
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *      stat    - pointer to status return word
 * Returns:
 *	0 on success 
 *	-1 on error with sw_error containing error code
 */

_txt_bksp(struct fdinfo *fio, struct ffsw *stat)
	{
	struct text_f *text_info;

	int ret;
	ssize_t rret;
	long rdamt;
	long ptradj;
	off_t diskpos;	/* our current disk position */
	off_t startpos;	/* byte position where we started last read */
	int readnewbuf = 0;	/* flag */
	char *p;
	char *peor;
	char *peof;
	long searchlen = 0;	/* the amount of buffer we want to search */
	int done = 0;
	struct fdinfo *llfio;
	bitptr eob;
	int ubc = 0;
	char *rptr;
	int restorechar = 0;
	long tword;
	char eorchar;

	text_info = (struct text_f *)fio->lyr_info;
	llfio = fio->fioptr;
	assert(text_info->eor_len <= EORLEN);
	assert((text_info->eof_len == 0 ) || (text_info->eof_len == 8) ||
		(text_info->eof_len == 24));

	if (text_info->eof_len == 24){
		if (text_info->eof_mark != (TEXT_MAGIC_MARKER<<TEXTNL_JUSTIFY_SHIFT)){
			ERETURN(stat,FDC_ERR_INTERR,0);
		}
	}

	eorchar = (char)((unsigned long)text_info->eor_char >> CHAR_JUSTIFY);
	/* If we've been writing, flush previously written data to disk */
	if (fio->rwflag == WRITIN)  {
		ret = XRCALL(fio, flushrtn) fio, stat);
		if (ret < 0) return (ERR);
	}
	diskpos = XRCALL(llfio, seekrtn)llfio, 0, 1, stat);
	if (diskpos < (_ffseek_t)0) return(ERR);
	if (fio->rwflag == WRITIN) {
		startpos = diskpos;
		eob = fio->_base;
	} 
	else {
		eob = INC_BPTR(fio->_ptr, fio->_cnt);
		startpos =  SUBT_BPTR(fio->_ptr, fio->_base) + fio->_cnt;
		startpos = diskpos - (startpos >> 3);
	}
	if (fio->ateor) {
ateor:
		/* We are positioned after an EOR mark. */
		/* We want to look for the last EOR or EOF mark that  */
		/* preceded this. */
		searchlen = SUBT_BPTR(fio->_ptr,fio->_base);
		searchlen = searchlen >> 3;	/* convert to bytes */
		searchlen -= 1;	/* for the eor mark that immediately precedes*/
				/* our current position */
		if (searchlen <= 0) {
			/* Eor mark is only thing in buffer, or it is not */
			/* even in the buffer. We need to read a new buffer */
			readnewbuf = 1;
			/* Adjust our position so this EOR won't be part of */
			/* the buffer we read. */
			if (searchlen == 0)
				startpos -=1;
			else
				startpos -=2;
		}
		else {
			/* replace this eor with a null char to stop our */
			/* search */
			rptr = BPTR2CP(fio->_ptr);
			rptr--;
			restorechar = 1;
			*rptr = '\0';
		}
	}
	else if ((fio->ateof) || (fio->ateod)) {
		/* Look at what comes before our current position. */
		/* If we are at eof and there is an EOF mark, then we want */
		/* see what precedes it. If there is no EOF mark, then we */
		/* want to see what precedes our current position. */
		/* If we are at eod, then we want to see what precedes our */
		/* current position, and act accordingly (i.e., like we are */
		/* at eof, or eor ) */
top:
		searchlen = SUBT_BPTR(fio->_ptr,fio->_base);
		ptradj = searchlen;
		if (fio->ateof){
			searchlen -= text_info->eof_len;
		}
		if (searchlen < MAX(text_info->eof_len, EORLEN)){
			/* The data we want to look at is not in the buffer */
			/* Position ourselves so that buffer will contain */
			/* current EOF (if any) plus preceding EOF or EOR */
			long diff;
			diff = searchlen - MAX(text_info->eof_len,EORLEN);
			startpos += (diff/8);
			ptradj -= diff;
			if (startpos < 0) {
				ptradj += (startpos*8);
				startpos = 0;
			}
			fio->_ptr = fio->_base;
			rdamt = fio->maxblksize >> 3;
			diskpos = XRCALL(llfio, seekrtn)llfio, startpos, 0, stat);
			if (diskpos < (_ffseek_t)0) return(ERR);
			READBLK(rret, fio, (size_t)rdamt, stat, PARTIAL, &ubc);
			if (rret < 0) return(ERR);
			eob = INC_BPTR(fio->_base, fio->_cnt);
			fio->_ptr = INC_BPTR(fio->_base,ptradj);

			fio->_cnt -= ptradj;
			/* add a null terminating character */
			p = BPTR2CP(eob);
			*p = '\0';
		}
		if (!(fio->ateof)) {
			searchlen = SUBT_BPTR(fio->_ptr,fio->_base);
			if (text_info->eof_len && searchlen >= text_info->eof_len) {
				if (iseof(INC_BPTR(fio->_ptr, -text_info->eof_len), text_info)){
					fio->ateof = 1;
					fio->ateod = 0;
					goto top;
				}
			}
			if ((searchlen > 0) && iseor(INC_BPTR(fio->_ptr, -EORLEN), text_info)) {
				goto ateor;
			}
			goto midl;
		}

		/* Now what we need to look at is in the buffer. */
		/* fio->_ptr  points to our current position. */
		if (text_info->eof_len) {
			/* There is an EOF mark, and we are positioned */
			/* after it. We want to back up over it, but */
			/* we also want to see what precedes it */
			searchlen = SUBT_BPTR(fio->_ptr,fio->_base);
			if (searchlen < text_info->eof_len) {
				if (fio->_ptr != fio->_base){
					ERETURN(stat,FDC_ERR_INTERR,0);
				}
				else {
					done = ATBOF;
					goto fin;
				}
			}
			fio->_ptr = INC_BPTR(fio->_ptr, -text_info->eof_len);
			fio->_cnt += text_info->eof_len;
			/* check that this really is an eof mark */
			if (!iseof(fio->_ptr , text_info)) {
				ERETURN(stat,FDC_ERR_INTERR,0);
			}
			/* see what precedes it */
			if (iseof(INC_BPTR(fio->_ptr, -text_info->eof_len), text_info)){
				done = ATEOF;
			}
			else if (iseor(INC_BPTR(fio->_ptr, -EORLEN), text_info)) {
				done = ATEOR;
			}
			else {
				done = ATREC;
			}
		}	
		else {
			/* there is NO EOF mark.  */
			/* If preceding character is an eor, it's like */
			/* we are at eor. */
			/* Otherwise, it's like we are in the middle of */
			/* a record. */
			/* Note, an EOF does not necessarily have an EOR before it */
			if (fio->_ptr != fio->_base) {
				/* preceding char is in buffer */
				if (iseor(INC_BPTR(fio->_ptr, -EORLEN), text_info)){
					goto ateor;
				}
				else {
					/* drop through */
				}
			}
			else {
				if (startpos == 0){
					done = ATBOF;
				}
				else {
					ERETURN(stat,FDC_ERR_INTERR,0);
				}
			}
		}
	}
	else {
		if (fio->_ptr == fio->_base)
			readnewbuf = 1;
		/* We are in the middle of a record. This record counts */
		/* as one to skip over. Just look for eor or eof that  */
		/* precedes us. */
	}
midl:
    while (!done) {
	/* We keep backing up until we find an EOR or EOF */
	if (readnewbuf) {
		if (startpos == 0) {
			/* we are at BOF */
			done = ATBOF;
			fio->_cnt = fio->_cnt + SUBT_BPTR(fio->_ptr,fio->_base);
			fio->_ptr = fio->_base;
			if (restorechar){
				*rptr = eorchar;
			}
			continue;
		}
		restorechar = 0;
		rdamt = fio->maxblksize >> 3;
		if (startpos < rdamt) {
			rdamt = startpos;
		}
		startpos -= rdamt;
		/* seek to startpos */	
		diskpos = XRCALL(llfio, seekrtn)llfio, startpos, 0, stat);
		if (diskpos < 0) return(ERR);
		fio->_ptr = fio->_base;
		READBLK(rret, fio, (size_t)rdamt, stat, PARTIAL, &ubc);
		eob = INC_BPTR(fio->_base, fio->_cnt);
		if (rret < 0) return(ERR);
		if (rret != rdamt) {
			/* We are not prepared to handle this. */
			/* Maybe the lower layer encountered an EOF, EOD */
			/* or something */
			ERETURN(stat,FDC_ERR_NOSUP,0);
		}
		else {
			/* add a null terminating character */
			p = BPTR2CP(INC_BPTR(fio->_base, fio->_cnt));
			*p = '\0';
		}
	}
	/* Look for EOR */
	peor = strrchr(BPTR2CP(fio->_base),eorchar);
	if (peor != NULL) {
		/* We found an EOR. This should be the EOR that precedes*/
		/* where we want to be, unless there is an EOF that follows it*/
		if (text_info->eof_len == 0) {
			done = ATEOR;	
			fio->_ptr = CPTR2BP(peor+1);
		}
		else if (text_info->eof_len == 8) {
			peof = strrchr(peor,
			   (int)((unsigned long)text_info->eof_mark>>CHAR_JUSTIFY));
			if (peof != NULL) {
				/* this is where we want to be */
				done = ATEOF;	
				peor = peof;
			}
			else {
				/* we're done, and peor is where we want to be */
				done = ATEOR;	
			}
			fio->_ptr = CPTR2BP(peor+1);
		}
		else if (text_info->eof_len == 24) {
			/* If any character of EOF == the EOR char, then */
			/* we know we don't have any EOFs beyond this. */
			/* However, this might be an EOF instead of an EOR */
			/* Also, it might not all be in the buffer */
			if ((eorchar == '~') || (eorchar == 'e') || 
			    (eorchar == '\n')) {
				int len; /* no. of bits that precede eor char */
					 /* in an eof */
				searchlen =SUBT_BPTR(CPTR2BP(peor),fio->_base);
				if (eorchar == '~'){
					len = 0;
				}
				else if (eorchar == 'e'){
					len = 8;
				}
				else {
					len = 16;
				}
				if (searchlen >= len) {
					if (iseof(INC_BPTR(CPTR2BP(peor),-len),
					    text_info)) {
					    done = ATEOF;
					    fio->_ptr = CPTR2BP(peor + ((24-len)>>3));
					}
					else {
					    done = ATEOR;
					    fio->_ptr = CPTR2BP(peor+1);
					}
				}
				else {
					/* we don't have enough data in this */
					/* buffer to determine if this is an */
					/* eof. Move startpos artifically so */
					/* we'll read it next time */
					if (startpos <(len-searchlen)>>3 ) {
					    done = ATEOR;
					    fio->_ptr = CPTR2BP(peor+1);
					}
					startpos = startpos + (searchlen>>3) +((24-len)>>3);
				}
			}else {
				/* EOR and EOF don't have any characters */
				/* in common.  */
				/* Search remainder of buffer to see if */
				/* we have 1st char of EOF */
				peof = strrchr(peor,
			   		(int)((unsigned long)text_info->eof_mark>>CHAR_JUSTIFY));
				done = ATEOR;
				if (peof != NULL) {
					if (iseof(CPTR2BP(peof),text_info)) {
						done = ATEOF;
						peor = peof;
					}
				}
				fio->_ptr = CPTR2BP(peor+1);
			}
		}
		if (done){
			fio->_cnt = SUBT_BPTR( eob, fio->_ptr);
		}
	}
	else if (text_info->eof_len){
		/* No EOR, but there might be an EOF */
		/* search for last char of EOF */
		if (text_info->eof_len == 8)
			peof = strrchr(BPTR2CP(fio->_base),
			   		(int)((unsigned long)text_info->eof_mark>>CHAR_JUSTIFY));
		else
			peof = strrchr(BPTR2CP(fio->_base), '\n');
		if (peof != NULL) {
			searchlen = SUBT_BPTR(CPTR2BP(peof),fio->_base) +8;
			if (searchlen >= text_info->eof_len) {
				if (iseof(INC_BPTR(CPTR2BP(peof),8-text_info->eof_len),text_info)) {
					done = ATEOF;
					fio->_ptr = CPTR2BP(peof+1);
					fio->_cnt = SUBT_BPTR(eob,fio->_ptr);
				}
			}else {
				startpos = startpos + (searchlen>>3) +1;
			}
		}	
	}
	readnewbuf = 1;
    }
fin:
    if (done) {
	fio->rwflag = POSITIN;
	if (restorechar){
		*rptr = eorchar;
	}
	fio->recbits = 0;
	fio->last_recbits = 0;
	fio->scc = SCCFULL;
	fio->lastscc = SCCFULL;
	fio->segbits = 0;
	fio->ateor = 0;
	fio->ateof = 0;
	fio->ateod = 0;
	if (done == ATEOR)
		fio->ateor = 1;
	else if (done == ATEOF)
		fio->ateof = 1;
    }
    return(0);
}	
static int
iseof(bitptr p, struct text_f *text_info)
{
	long tword;
	GET_BITS(tword, p, text_info->eof_len);
	if (tword == text_info->eof_mark)
		return 1;
	return 0;
}
static int
iseor(bitptr p, struct text_f *text_info)
{
	long tword;
	GET_BITS(tword, p, EORLEN);
	if (tword == text_info->eor_char)
		return 1;
	return 0;
}
