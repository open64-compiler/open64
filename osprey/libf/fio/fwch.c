/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/fwch.c	92.2	06/22/99 11:11:33"

#include <errno.h>
#include <liberrno.h>
#include <stdlib.h>
#include "fio.h"
#include "ffio.h"
#ifdef	_ABSOFT
#include "ac_sysdep.h"
#else		/* NOT _ABSOFT */

#if	defined(_LITTLE_ENDIAN) && !defined(__sv2)
#ifndef FILE_CNT
#define FILE_CNT(__f)	(__f)->_IO_write_end - (__f)->_IO_write_ptr
#endif

#ifndef FILE_PTR
#define FILE_PTR(__f)	(__f)->_IO_write_ptr
#endif

#ifndef FILE_FLAG
#define FILE_FLAG(__f)	(__f)->_flags
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

#else		/* LITTLE_ENDIAN and not sv2 */
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
#undef putc
#endif

#ifdef KEY /* Bug 5926 */
/*
 * Return a terminator to pass to function _pack().
 *
 * If this is a full record, and we're about to write the last chunk of it,
 * and there's room in the buffer, return newline so that pack() will append
 * it to the buffer, and set the mode to "PARTIAL" so we don't write the
 * newline separately later on. Otherwise, return -1. In case the file
 * descriptor has been set to "unbuffered" (e.g. by a parent process) and
 * this is a parallel program, we don't want to write the newline separately
 * for fear that output from some other process will intervene between our
 * text and our newline.
 */
static int terminator(int *mode, long nchr, long count, long chars)
{
	if (FULL == *mode && (nchr + count) >= chars && count < TBUFSZB) {
		*mode = PARTIAL;
		return '\n';
	}
	return -1;
}
#endif /* KEY Bug 5926 */

/*
 *	_fwch
 *
 *	 	Pack and then write an array of unpacked characters to the 
 *	 	current record of a file.
 *
 *	Arguments
 *		cup	- unit pointer
 *		uda	- array of characters, one character per word
 *		chars	- number of characters in uda to write
 *		mode	- PARTIAL or FULL record mode
 *
 *	Return value
 *		chars on successful completion, and -1 on error, with errno set 
 *		to the error code.
 *
 *	Side effects
 *		Clears cup->uend if the file supports multiple endfile
 *		records.
 */

long
_fwch(
	unit	*cup,
	long	*uda,
	long	chars,
	int	mode)
{
	register int	bytsiz;
	register long	nchr;
	unsigned char	tbuf[TBUFSZB];	/* Line packing buffer */
	FILE		*fptr;

/*
 *	If positioned after an endfile, and the file does not
 *	support multiple endfiles, a write is invalid.
 */
	if (cup->uend && !cup->umultfil && !cup->uspcproc) {
		errno	= FEWRAFEN;
		return(IOERR);
	}

	nchr	= 0;

	switch (cup->ufs) {

	case FS_TEXT:
	case STD:
		fptr	= cup->ufp.std;

		/* switch the FILE structure into write mode */

#if	!defined(_LITTLE_ENDIAN) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
		if ((FILE_FLAG(fptr) & (_IOWRT | _IORW)) == _IORW) {
			if (FILE_FLAG(fptr) & _IOREAD)
				(void) fseek(fptr, 0, SEEK_CUR);

			FILE_FLAG(fptr) |= _IOWRT;
		}
#endif

#if	defined(_SOLARIS) || (defined(_LITTLE_ENDIAN) && !defined(__sv2))
		while (nchr < chars) {
			register long	count;
			register int	ret;
			/* Pack chars into temp buffer and write them */
			count	= chars - nchr;
			if (count > TBUFSZB)
				count	= TBUFSZB;
#ifdef KEY /* Bug 5926 */
			count = _pack(&uda[nchr], (char *)tbuf, count,
			  terminator(&mode, nchr, count, chars));
#else /* KEY Bug 5926 */
			_pack(&uda[nchr], (char *)tbuf, count, -1);
#endif /* KEY Bug 5926 */
			ret	= fwrite(tbuf, 1, count, fptr);
			if ( ret != count || ferror(fptr) ) {
				if ( ret != count || errno == 0)
					errno	= FESTIOER;
				return(IOERR);
			}
			nchr	+= count;
		}
#else

		/* If the stream is unbuffered... */

		if (FILE_FLAG(fptr) & (_IONBF | _IOLBF)) {
			while (nchr < chars) {
				register long	count;
				register long	ret;

				/* Pack chars into temp buffer and write them */

				count	= chars - nchr;

				if (count > TBUFSZB)
					count	= TBUFSZB;

#ifdef KEY /* Bug 5926 */
			count= _pack(&uda[nchr], (char *)tbuf, count,
			  terminator(&mode, nchr, count, chars));
#else /* KEY Bug 5926 */
				_pack(&uda[nchr], (char *)tbuf, count, -1);

#endif /* KEY Bug 5926 */
				ret	= fwrite(tbuf, 1, count, fptr);

				if ( ret != count || ferror(fptr) ) {
					if ( ret != count || errno == 0)
						errno	= FESTIOER;
					return(IOERR);
				}

				nchr	+= count;
			}
		}
		else {	/* for a buffered stream... */

			while (FILE_CNT(fptr) < chars - nchr) {
				register long	count;
				register int	ret;

				count	= FILE_CNT(fptr); /* space left in buffer */
				if (count > 0) {
					/* pack data into the buffer */
					_pack(&uda[nchr], (char *)FILE_PTR(fptr),
						count, -1);
					FILE_PTR(fptr)	+= count;
					FILE_CNT(fptr)	= 0;
				}

				/*
				 * We set errno to 0 here in case the following
				 * buffer flush fails.  UNICOS 8.2 fputc (and
				 * previous) was not X/Open compliant and did
				 * not always set errno when a buffer flush
				 * completed partially due to a disk full
				 * conditon.  The zeroing of errno may be
				 * removed when we can assume that the fputc()
				 * from UNICOS and Solaris are X/Open compliant.
				 */

				errno	= 0;

				/*
				 * This fputc() will either trigger a buffer
				 * flush or cause the buffer to be allocated
				 * for the first time.
				 */

				ret	= fputc(uda[nchr + count], fptr);

				if (ret == EOF && ferror(fptr)) {
					if (errno == 0)
						errno	= FESTIOER;
					return(IOERR);
				}

				nchr	+= count + 1;
			}

			if (nchr < chars) {	/* Put data in buffer */
				_pack(&uda[nchr], (char *)FILE_PTR(fptr),
					chars - nchr, -1);

				FILE_CNT(fptr)	-= chars - nchr;
				FILE_PTR(fptr)	+= chars - nchr;
			}
		}
#endif

		if (mode == FULL) {
			register int	ret;

 			ret	= putc('\n', fptr);;

			if (ret == EOF && ferror(fptr)) {
				if (errno == 0)
					errno	= FESTIOER;
				return(IOERR);
			}
			chars++;
		}

		return(chars);

	case FS_FDC:

		/*
		 * If a logical endfile record had just been read,
		 * replace it with a physical endfile record before
		 * starting the current data record.
		 */
		if ((cup->uend == LOGICAL_ENDFILE) && !(cup->uspcproc)) {
			if (XRCALL(cup->ufp.fdc, weofrtn)cup->ufp.fdc,
				&cup->uffsw) < 0){
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}
		}

		cup->uend	= BEFORE_ENDFILE;

		if (cup->ucharset == 0) {
			register long	ret;

			ret	= XRCALL(cup->ufp.fdc, writecrtn) cup->ufp.fdc,
					WPTR2BP(uda),
					chars, &cup->uffsw, mode);

			if (ret < 0) {
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}

			return(chars);
		}

/*
 *		Get proper byte size (might not be 8-bits if doing conversion).
 */

#if	NUMERIC_DATA_CONVERSION_ENABLED
		bytsiz	= __fndc_charsz[cup->ucharset];
#else
		bytsiz	= 8;
#endif

		do {
			register long	breq;
			register int	fulp;
			register long	ncnt;
			register long	ret;
			int		ubc;

			ncnt	= TBUFSZB;
			breq	= 0;
			ubc	= 0;

			if ((chars - nchr) > 0) {
				register long	totbits;

				if (ncnt > (chars - nchr))
					ncnt	= chars - nchr;

				if (_fdc_packc((char *)tbuf, &uda[nchr], ncnt,
					cup->ucharset) < 0) {
					return(IOERR);
				}


				totbits	= bytsiz * ncnt;	/* bit count */
				breq	= (totbits + 7) >> 3; /* 8-bit bytes */
				ubc	= (breq << 3) - totbits;
			}

			nchr	+= ncnt;

			if ((nchr >= chars) && ( mode == FULL ))
				fulp	= FULL;
			else
				fulp	= PARTIAL;

			ret	= XRCALL(cup->ufp.fdc, writertn) cup->ufp.fdc,
					CPTR2BP(tbuf),
					breq, &cup->uffsw, fulp, &ubc);

			if (ret != breq) { 	/* if an error */
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}

		} while (nchr < chars);

		return(chars);
/*
 *	unsupported structure if not TEXT/STD, or FDC
 */
	default:
		errno	= FEINTFST;
		return(IOERR);
	}
}
