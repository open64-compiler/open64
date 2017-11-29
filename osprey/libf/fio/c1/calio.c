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


static char USMID[] = "@(#) libf/fio/c1/calio.c	92.0	10/08/98 14:30:10";

/*
 *	Cal i/o drivers
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 *	Cal i/o completion status
 */

#define CNT     1       /*  count exhausted             */
#define EOR     0       /*  end-of-record               */
#ifndef EOF
#define EOF     -1      /*  end-of-file                 */
#endif

#define OK      0       
#define IOERR   -1      /*  Cal I/O error		*/

/*
 *	Cal i/o request modes
 */

#define PARTIAL 0	/*  partial record i/o		*/
#define FULL    1	/*  full record i/o		*/

#define BLANK	((long) ' ')	/* ASCII blank			*/

#define MYRMAX	256

/*
 *	cal_rch - cal read characters
 */

long
cal_rch(fp, uda, chars, mode, status )   
FILE *fp;
long *uda, chars, mode, *status;
{

	long nc, i, n, c;
	char *cp;
 
/*
 *	If the number of characters to read is zero and full record
 *  	mode has been requested, skip to the end of record.  If in
 *	partial record mode, the position remains as is.
 */
	if (chars == 0)
	        {
	        *status = CNT;
	        if (mode == FULL )
			{
			while (1)
				{
				if (fp->_cnt <= 0)
					{
					c = _filbuf(fp);
					/* EOF here means incomplete record. */
					if (c == EOF) return(0);
					fp->_cnt++;
					fp->_ptr--;
					}
				cp = memchr(fp->_ptr, '\n', fp->_cnt);
				if (cp != NULL)
					{
					cp++;
					fp->_cnt -= cp - (char *)fp->_ptr;
					fp->_ptr = (unsigned char *)cp;
					return(0);
					}
				else
					fp->_cnt = 0;
				}                     
			}
	        }
/*
 * 	Loop until the character count has been exhausted,
 *      an end of file is encountered, or end of record.   
 */
	n = 0;
	while (n < chars)
	        {
	        if (fp->_cnt <= 0)
			{
			c = _filbuf(fp);
			/* EOF here means incomplete record. */
			if (c == EOF) {
				*status = EOF; 
				/* blank fill the record */
				(void) _memwset(uda, BLANK, chars - n);
				return(0);}
			fp->_cnt++;
			fp->_ptr--;
			}
	        /* if not enough chars in buffer to satisfy request, */
	        if ((chars - n) >= fp->_cnt)
			{
			cp = memchr(fp->_ptr, '\n', fp->_cnt);
			if (cp != NULL)
				{
				/* found end of record */
				nc = cp - (char *)fp->_ptr;
				n += nc;
				_unpack((char *)fp->_ptr, uda, nc);
				(void) _memwset(&uda[nc], BLANK, chars - n);
				fp->_cnt -= nc+1;
				fp->_ptr = (unsigned char *)++cp;
				*status = EOR;
				return(n);
				}
			else
				{
				/* not found end of record */
				nc = fp->_cnt;
				_unpack((char *)fp->_ptr, uda, nc);
				uda += nc;
				fp->_cnt -= nc;
				n += nc;
				fp->_ptr += nc;
				/* go refill the buffer */
				}
			}
	        else /* have enough characters to satisfy request */
			{
			cp = memchr(fp->_ptr, '\n', chars - n);
			if (cp != NULL)
				{
				/* found end of record */
				nc = cp - (char *)fp->_ptr;
				n += nc;
				_unpack((char *)fp->_ptr, uda, nc);
				(void) _memwset(&uda[nc], BLANK, chars - n);
				fp->_cnt -= nc+1;
				fp->_ptr = (unsigned char *)++cp;
				*status = EOR;
				return(n);
				}
			else
				{
				nc = chars - n;
				n += nc;
				_unpack((char *)fp->_ptr, uda, nc);
				(void) _memwset(&uda[nc], BLANK, chars - n);
				fp->_cnt -= nc;
				fp->_ptr += nc;
				uda += nc;
				}
			}
	        }
/*
 *	Get the next character to see if at end of record.  Set the
 * 	user's status word accordingly.
 */
	c = getc(fp);
	if (c == '\n' ) {
		*status = EOR;
		return(n);
	}
	else
		*status = CNT;
/*
 *	We are not at end of record.  Thus if reading in full record 
 *	mode skip until EOR is found.  If reading in partial record
 * 	mode, unget the last character read.
 */
	if (mode == FULL)
	        while (1)
			{
			if (fp->_cnt <= 0)
				{
				c = _filbuf(fp);
				/* EOF here means incomplete record. */
				if (c == EOF) return(n);
				fp->_cnt++;
				fp->_ptr--;
				}
			cp = memchr(fp->_ptr, '\n', fp->_cnt);
			if (cp != NULL)
				{
				cp++;
				fp->_cnt -= cp - (char *)fp->_ptr;
				fp->_ptr = (unsigned char *)cp;
				return(n);
				}
			else
				fp->_cnt = 0;
			}
	else {
		(void) ungetc ((char) c, fp);
	}

	return(n);	/* return number of character read */

}

/*
 *	cal_wch - cal write characters
 */

long
cal_wch(fp, uda, chars, mode, status )   
FILE *fp;
long *uda, chars, mode, *status;
{
	int  n, c, j, i, tpi, ist;
	char tp[MYRMAX+sizeof(long)];

	*status = OK;

	/* in case I/O has not been initialized... */
	if (fp->_base == NULL)
		c = _findbuf(fp);	/* let stdio do its thing */

	j = 0;
	/* If the stream is unbuffered... */
	if (fp->_flag &(_IONBF | _IOLBF))
		{
		i = 0;
		tpi = 0;
		while (j < chars)
			{
			/* Pack chars into temp buffer and write em out */
			i = chars - j;
			if (chars - j > MYRMAX)
				i = MYRMAX;
			_pack(&uda[j], &tp[tpi], i, -1);
			tpi += i;
			/* avoid syscalls, only write at EOL */
			if (tpi >= MYRMAX)
				{
				ist = write(fileno(fp),tp,tpi);
				if( ist != tpi)
					{
					fp->_flag |= _IOERR;
					*status = IOERR;
					return(EOF);
					}
				else
					{
					tpi = 0;
					}
				}
			j += i;
			}
		if (mode == FULL) 
			{
			tp[tpi++] = '\n';
			chars++;
			}
		ist = write(fileno(fp),tp,tpi);
		if( ist != tpi)
			{
			fp->_flag |= _IOERR;
			*status = IOERR;
			return(EOF);
			}
		}
	else
		{
		while (fp->_cnt < chars - j)
		        {
		        i = fp->_cnt;     /* we have this much room */
		        if (i == 0)
				{
				/* this one putc will init many things */
				/* and make the rest of this loop work */
				c = uda[j];
				c = putc(c, fp);
				i = 1;
				}
			else
				{
				_pack(&uda[j], (char *)fp->_ptr, i, -1);
				fp->_ptr += i;
				fp->_cnt = -1;    /* dummy up count for _xflsbuf() */
				}
		        _xflsbuf(fp);
			if (ferror(fp))
				{
				fp->_flag |= _IOERR;
				*status = IOERR;
				return(EOF);
				}
		        j += i;
		        }
		if ((chars - j) > 0)
		        {
		        _pack(&uda[j], (char *)fp->_ptr, chars - j, -1);
		        fp->_cnt -= chars - j;
		        fp->_ptr += chars - j;
		        }

		if (mode == FULL) 
			{
	 		putc('\n', fp);
			chars++;
			}
		}
 
	return(chars);
}

/*
 *	cal_rwd - cal read words
 */

long
cal_rwd(fp, uda, words, mode, status )   
FILE  *fp;
long *uda, words, mode, *status;
{
	long n, chars, c;

/*
 *	If the number of words to read is zero return to caller. 
 *      Std. UNICOS binary files have no record structure.  Except
 *      for end of file, a read request always returns EOR status.
 */
	*status = EOR;
	if (words == 0)
		return(0);

	n = fread((char *)uda, sizeof(long), words, fp );

	if (n == 0) {		/* if no data read */
		*status = EOF;
		return(0);
	}
 
	if (n < 0) {		/* if an error or end of file */
		if (n != EOF)
			return(IOERR);
		*status = EOF;
 		return(0);
	}

	return(n);	/* return number of words read */

}

/*
 *	cal_wwd - cal write words
 */

long
cal_wwd(fp, uda, words, mode, status )   
FILE  *fp;
long *uda, words, mode, *status;
{
	long n;
/*
 *	If number of words to write is zero return to caller.  
 *	Std. UNICOS binary files have no record structures.
 */
	if (words == 0)
		return(0);
/*
 *	Use low-level binary i/o routine to write the requested
 *	amount of data.
 */
 	if (n = fwrite((char *)uda, sizeof(long), words, fp) != words)
		return(IOERR);

	return(words);	/* return number of words written */

}

