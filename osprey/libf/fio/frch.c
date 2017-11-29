/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/frch.c	92.2	06/22/99 11:11:33"

#include <errno.h>
#include <liberrno.h>
#if	defined(_SOLARIS) || (defined(_LITTLE_ENDIAN) && !defined(__sv2))
#include <stdio.h>
#endif
#include <stdlib.h>
#include <string.h>
#include "fio.h"
#ifdef	_ABSOFT
#include "ac_sysdep.h"
#else		/* NOT _ABSOFT */

#if     defined(_LITTLE_ENDIAN) && !defined(__sv2)
#ifndef FILE_CNT
#define FILE_CNT(__f)   (__f)->_IO_write_end - (__f)->_IO_write_ptr
#endif

#ifndef FILE_PTR
#define FILE_PTR(__f)   (__f)->_IO_write_ptr
#endif

#ifndef FILE_FLAG
#define FILE_FLAG(__f)  (__f)->_flags
#endif

#ifndef IOWRT
#define IOWRT = _IO_CURRENTLY_PUTTING
#endif
#ifndef IOREAD
#define IOREAD = _IO_CURRENTLY_APPENDING
#endif
#ifndef IORW
#define IORW = _IO_TIED_PUTGET
#endif

#else           /* LITTLE_ENDIAN and not sv2 */
#ifndef FILE_CNT
#define FILE_CNT(__f)	(__f)->_cnt
#endif

#ifndef FILE_PTR
#define FILE_PTR(__f)	(__f)->_ptr
#endif

#ifndef FILE_FLAG
#define FILE_FLAG(__f)	(__f)->_flag
#endif

#endif		/* LITTLE_ENDIAN and not sv2 */

#endif		/* _ABSOFT */

#ifdef	_SOLARIS
#undef getc
#ifndef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif
#endif		/* end _SOLARIS */
#ifdef	_CRAY
extern int G@INTIO;
#define _GINTIO G@INTIO
#else
extern int _intio;
#define _GINTIO _intio
#endif
/*
 *	_frch
 *		Read and unpack characters from the current record of a file.
 *
 *	Arguments
 *		cup:	unit pointer
 *		uda:	character pointer to user's data area
 *		chars:	Number of characters to read
 *		mode:	Full or partial mode flag
 *		status:	Address of caller's status word.
 *
 *	Side effects
 *
 *		Sets *status as follows:
 *
 *			CNT - if the requested number of items were read, and
 *				more data remains in the current record (if
 *				the file has records).
 *			EOR - if all remaining data in the current record was 
 *				read by this request. 
 *			EOF - if a physical endfile record was read and no data 
 *				preceded the endfile record.
 *			EOD - if the end-of-data mark was reached and no data 
 *				preceded the the end-of-data.
 *
 *	Return value
 *		The number of characters read from the current record.  The
 *		record delimitting character (for example a newline) is not 
 *		counted.  An error results in a -1 return value with errno set 
 *		to the error code.
 *
 *	Note
 *
 * 		If a nonzero amount of data was read and an EOF mark or the 
 *		end of file (EOD) was encountered before reading the full amount
 *		of data requested and before encountering EOR, beware that
 *		CNT status is returned.
 */

long
_frch(
	unit	*cup,
	long	*uda,
	long	chars,
	int	mode,
	long	*status)
{
	register int	bytsiz;
	register int	chr;
	register long	nchr;
	register long	ncnt;
	unsigned char	tbuf[TBUFSZB], *tp;	/* Line buffer */
	FILE		*fptr;
	struct ffsw	stat;
	struct fdinfo	*fio;
#if	defined(_SOLARIS) || (defined(_LITTLE_ENDIAN) && !defined(__sv2))
	register long	count;
	char		*tpinterim;
#endif
 
	switch (cup->ufs) {

	case FS_TEXT:
	case STD:
		fptr	= cup->ufp.std;
/*
 *		Switch the FILE structure into read mode
 */
#if	!defined(_LITTLE_ENDIAN) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
		if ((FILE_FLAG(fptr) & (_IOREAD | _IORW)) == _IORW) {

			if (FILE_FLAG(fptr) & _IOWRT)
				(void) fseek(fptr, 0, SEEK_CUR);

			FILE_FLAG(fptr) |= _IOREAD;
		}
#endif
/*
 * 		Loop on getc until the character count has been 
 *		exhausted, an end of file is encountered, or end
 *		of record.
 */
		nchr	= 0;
#if	defined(_SOLARIS) || (defined(_LITTLE_ENDIAN) && !defined(__sv2))
		while (nchr < chars) {
fill:
			errno	= 0;
			count	= chars - nchr;
			tp	= tbuf;
			count	= MIN(TBUFSZB, (count + 1));
#ifdef KEY /* Bug 3797, 8405 */
			off_t oldpos = ftello(fptr);
			if (-1 == oldpos & ESPIPE != errno) {
			   return IOERR;
			}
#endif /* KEY Bug 3797, 8405 */
			tpinterim	= fgets((char *)tp, count, fptr);
			if (tpinterim == NULL) {
			/*
			 * Search for the newline char in the buffer, but
			 * search only the number of characters left in
			 * the request, plus one in case it is the
			 * newline (if it is in the buffer).  If we find
			 * newline, we're done.
			 */
			/* EOF here means incomplete record. */
				if (ferror(fptr)) {
					if ( errno == EINTR  && _GINTIO == 0) {
						clearerr(fptr);
						goto fill;
					}
					if (errno == 0)
						errno	= FESTIOER;
					return(IOERR);
				}
				/*
				 * If nchr > zero, file has no newline.
				 * Make sure we handle it properly.
				 */
				if (feof(fptr)) {
					if (nchr == 0) {
						*status	= EOD;
						return(0);
					}
					*status	= CNT;
					return(nchr);
				}
			} else {
				unsigned char	*tmpptr;
				ncnt	= count - 1;
#ifdef KEY /* Bug 3797, 8405 */
				/* On Linux, if fgets() reads any characters
				 * before it encounters EOF, then it doesn't
				 * return NULL but does set feof(), so this
				 * is where we arrive if the last record of
				 * the file lacks '\n'. Distinguishing between
				 * '\0' placed in the buffer by fgets() versus
				 * a '\0' appearing in the record itself
				 * presents a dilemma when there's no '\n'. If
				 * we can, we use ftello() before and after the
				 * fgets() to determine how many characters it
				 * gave us; otherwise we use strlen() even
				 * though it stops at the first '\0'.
				 *
				 * Note that it's necessary to test for
				 * EOF before calling "memchr" to look for
				 * '\n', because when the record is shorter
				 * than ncnt bytes and does not end in '\n',
				 * there might be a spurious '\n' in memory
				 * after the '\0' which indicates the end of
				 * the data read from the file but before the
				 * ncnt'th byte. */
				if (feof(fptr)) {
					*status = EOR;
					off_t newpos = ftello(fptr);
					if (-1 == newpos && ESPIPE != errno) {
					   return IOERR;
					}
					ncnt = (-1 == oldpos && -1 == newpos) ?
					  strlen((char *)tp) :
					  (newpos - oldpos);
					nchr += ncnt;
					_unpack((char *)tp, uda, ncnt, -1);
					return nchr;
				}
				/* If not EOF, the line will either contain
				 * '\n', or (if it was too long) it
				 * will have "ncnt" characters. Using "memchr"
				 * instead of "strchr" ensures that a '\0'
				 * within the line won't stop us prematurely. */
#endif /* KEY Bug 3797, 8405 */
				tmpptr	= memchr(tp, '\n', ncnt);
				if (tmpptr != NULL) {	/* eor */
					*status	= EOR;
					ncnt	= tmpptr - tp;
					nchr	+= ncnt;
					_unpack((char *)tp, uda, ncnt, -1);
					/* Return number of chars read */
					return(nchr);
				}
				_unpack((char *)tp, uda, ncnt, -1);
				nchr		+= ncnt;
				uda		+= ncnt;

				/* go refill the buffer */
			}
		}
#else
		while (nchr < chars) {
			if (FILE_CNT(fptr) <= 0) {
fill:
				errno	= 0;
				chr	= _filbuf(fptr);

				/* EOF here means incomplete record. */

				if (chr == EOF) {

					if (ferror(fptr)) {

						if ( errno == EINTR &&
						 _GINTIO == 0 ) {
							clearerr(fptr);
							goto fill;
						}

						if (errno == 0)
							errno	= FESTIOER;

						return(IOERR);
					}
/*
 *					If nchr > zero, file has no newline.
 *					Make sure we handle it properly.
 */
					if (nchr == 0) {
						*status	= EOD;
						return(0);
					}

					*status	= CNT;
					return(nchr);
				}
/*
 *				Put character returned by filbuf() back
 */
				FILE_CNT(fptr)++;
				FILE_PTR(fptr)--;
			}
/*
 *			Search for a newline char in the buffer, but search
 *			only the number of characters left in the request,
 *			plus one in case it is the newline (if it is in
 *			the buffer).  If we find the newline, we're done.
 */
			if ((chars - nchr) < FILE_CNT(fptr)) {
				ncnt	= chars - nchr;
				tp	= memchr(FILE_PTR(fptr), '\n', ncnt + 1);
			}
			else {
				ncnt	= FILE_CNT(fptr);	/* assume no EOR yet */
				tp	= memchr(FILE_PTR(fptr), '\n', ncnt);
			}

			if (tp != NULL) {	/* Found end of record */

				*status	= EOR;
				ncnt	= tp - FILE_PTR(fptr);
				nchr	+= ncnt;

				_unpack((char *)FILE_PTR(fptr), uda, ncnt, -1);

				FILE_CNT(fptr)	-= ncnt + 1;
				FILE_PTR(fptr)	+= ncnt + 1;

				return(nchr);	/* Return number of characters read */
			}

			_unpack((char *)FILE_PTR(fptr), uda, ncnt, -1);

			FILE_CNT(fptr)	-= ncnt;
			FILE_PTR(fptr)	+= ncnt;
			nchr		+= ncnt;
			uda		+= ncnt;

			/* go refill the buffer */
		}
#endif
/*
 *		Get the next character to see if at end of record.
 * 		Set the user's status word accordingly.
 */
		chr	= getc(fptr);

		*status	= CNT;

		if (chr == '\n' ) {
			*status	= EOR;
			return(nchr);	/* Return number of characters read */
		}
/*
 *		We are not at end of record.  Thus if reading in full
 *		record mode skip until EOR is found.  If reading in 
 *		partial record mode, unget the last character read.
 */
		if (mode == FULL)
#if	defined(_SOLARIS) || (defined(_LITTLE_ENDIAN) && !defined(__sv2))
		{
fill2:
			count	= TBUFSZB;
			tp	= tbuf;
			tpinterim = fgets((char *)tp, count, fptr);
			if (tpinterim == NULL) {
				/* EOF means incomplete record. */
				if (ferror(fptr)) {
					if ( errno == EINTR && _GINTIO == 0 ) {
						clearerr(fptr);
						goto fill2;
					}
				}
				/* Return number of chars read. */
				return(nchr);
			} else {
				unsigned char	*tmpptr;
				ncnt	= count - 1;
				tmpptr	= memchr(tp, '\n', ncnt);
				if (tmpptr != NULL) {	/* Found eor */
					/* Return number of chars read */
					return(nchr);
				} else
					goto fill2;
			}
		}
#else
			while (1) {
				if (FILE_CNT(fptr) <= 0) {
fill2:
					chr	= _filbuf(fptr);

					/* EOF here means incomplete record. */

					if (chr == EOF) {

						if (ferror(fptr)) {

							if ( errno == EINTR &&
							 _GINTIO == 0 ) {
								clearerr(fptr);
								goto fill2;
							}

						}

						/* Return number of characters read */

						return(nchr);
					}

					FILE_CNT(fptr)++;
					FILE_PTR(fptr)--;
				}

				tp	= memchr(FILE_PTR(fptr), '\n', FILE_CNT(fptr));

				if (tp != NULL) {

					tp++;
					FILE_CNT(fptr)	-= tp - FILE_PTR(fptr);
					FILE_PTR(fptr)	= tp;

					return(nchr);
				}
				else
					FILE_CNT(fptr)	= 0;
			}
#endif
		else {
			ungetc ((char) chr, fptr);
		}

		return(nchr);	/* return number of character read */

	case FS_FDC:
		nchr	= 0;
		fio	= cup->ufp.fdc;
/*
 *		If no conversion is to be done, or no characters requested
 *		(usually for null request, FULL mode), make it simple and direct.
 */
		if (cup->ucharset == 0 || chars == 0) {
			register long	breq;
			register int	ffstat;
			register int	ret;
/*
 *			If file is direct access, we know that all reads are
 *			going to be whole records in FULL mode.  We also
 *			know that the open code would not let us get this far
 *			if we were not dealing with a stream layer.
 */

			breq	= chars;

			ret	= XRCALL(fio, readcrtn) fio, CPTR2BP(uda),
					breq, &stat, mode);

			if (ret < 0) {
				errno	= stat.sw_error;
				return(IOERR);
			}

			ffstat	= FFSTAT(stat);

			if (!cup->useq && !cup->ublkd && ffstat == FFCNT)
				ffstat	= FFEOR;

			*status	= FF2FTNST(ffstat);
			nchr	= ret;

			return(nchr);	/* Return number of characters read */
		}
/*
 *		Get proper byte size (might not be 8-bits if doing conversion).
 */

#if	NUMERIC_DATA_CONVERSION_ENABLED
		bytsiz	= __fndc_charsz[cup->ucharset];
#else
		bytsiz	= 8;
#endif
		FFSTAT(cup->uffsw)	= FFCNT;
		*status	= CNT;

		while (nchr < chars && FFSTAT(cup->uffsw) != FFEOR) {
			register long	bgot;
			register long	breq;
			register long	padc;
			register int	ret;
			register long	totbits;
			int		ubc;
/*
 *			Calculate the number of bits that need to be taken
 *			from the foreign data stream.
 *
 *			ncnt	= number of resultant bytes
 */
			ncnt	= chars - nchr;

			if (ncnt > TBUFSZB)
				ncnt	= TBUFSZB;

			totbits	= bytsiz * ncnt;	/* bit count */
			breq	= (totbits + 7) >> 3;	/* full 8-bit bytes */
			ubc	= (breq << 3) - totbits;/* unused bits */

			ret	= XRCALL(fio, readrtn) fio, CPTR2BP(tbuf),
					breq, &cup->uffsw, PARTIAL, &ubc);

			if (ret < 0) { 	/* if an error */
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}

			/* if end of file */

			if (ret == 0) {
				if (nchr == 0)
					*status	= FF2FTNST(FFSTAT(stat));
				return(nchr);	/* Return number of characters read */
			}

/*
 *			how many bits did we get?  Convert back to foreign
 *			character count.
 */
			totbits	= (ret << 3) - ubc;
			bgot	= totbits / bytsiz;	/* foreign bytes */
			ubc	= totbits - (bgot * bytsiz);

			if (ubc != 0) {
				errno	= FEINTUNK;
				return(IOERR);
			}

			padc	= 0;

			if (FFSTAT(cup->uffsw) == FFEOR) {
				padc	= chars - (bgot + nchr);
				*status	= EOR;
			}

			if (_fdc_unpackc(tbuf, &uda[nchr], bgot, padc,
				cup->ucharset) < 0) {
				return(IOERR);
			}

			nchr	+= bgot;
		}

		/* check for null request, non-EOR */

		if (FFSTAT(cup->uffsw) == FFCNT && mode == FULL) {
			register int	ret;
			int		ubc;

			ret	= XRCALL(fio, readrtn) fio, CPTR2BP(tbuf), 0,
					&cup->uffsw, FULL, &ubc);

			if (ret < 0) { 	/* if an error */
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}
		}
		return(nchr);

	case FS_AUX:
		errno	= FEMIXAUX;
		return(IOERR);

	default:
		errno	= FEINTFST;
		return(IOERR);
	}
}
