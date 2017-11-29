/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/wnly.c	92.1	06/21/99 10:37:55"

/*
 * 	Namelist output
 */

#include <stdio.h>
#include <errno.h>
#include <fortran.h>
#include <memory.h>
#include <malloc.h>
#include <liberrno.h>
#include <stdlib.h>
#include <cray/fmtconv.h>
#include "fio.h"
#include "fmt.h"	
#include "lio.h"
#include "rnl.h"

extern void _memwcpy (long *_S1, long *_S2, int _N);

/*
 * YMP80 is true if the LISTIO_PRECISION environment variable is set to
 * 'YMP80'.  This is a compatibility mode which prints namelist output
 * in the same form as was seen in UNICOS 8.0.
 */

#define YMP80	(_dreal8 == DREAL8_YMP80)

/*
 * This structure contains an unpacked buffer where output is stored and,
 * for some bizarre reason, another unpacked buffer where output is formatted.
 * The first buffer is equated to the line buffer in the unit table and the
 * second buffer is malloc'ed to match (the code assumes that the two buffers
 * are the same size).  Someday, this BUFFERS structure can be tossed into
 * the bit bucket and much of this code can be replaced with the list-
 * directed output routines.
 */

struct BUFFERS {
	long	*outbuff;	/* Output buffer		*/
	long	*outptr;	/* Next free spot in outbuff	*/
	int	outcnt;		/* Remaining space in outbuff	*/
	long	*f_lbuf;	/* Buffer for formatting output	*/
	long	*f_lbufptr;	/* Next free spot in f_lbufptr	*/
	int	f_lbufcnt;	/* Number of elements in f_lbuf	*/
	int	lcomma;		/* 1 => comma before next value	*/
};

static char	*char_rep(char *_P, int _Cn, unsigned int _Ln, int *_Lc,
			struct BUFFERS *_Bp);

static long	*find_rep(long *_P, int _Cn, int _In, int _Ty, int *_Lc,
			struct BUFFERS *_Bp);

static int	l_write(FIOSPTR css, unit *cup, void *dptr, unsigned elsize,
			int count, int inc, int type, long recsize, int errf,
			struct BUFFERS *bptr);

static int	lw_A(FIOSPTR css, char *_P, int _Cl, long _Rc, unit *_Cu,
			int _Er, struct BUFFERS *_Bp);

static void	writ_rep(long repcnt, struct BUFFERS *buffers);

/*
 * NLPUT adds a character to the output buffer.
 */

#define NLPUT(x) {	\
	*(bptr->outptr)++	= (long) x;	\
	bptr->outcnt--;				\
}

#define NLPUTS(string) {	\
	s	= string;	\
	while (c = *s++) {	\
		NLPUT(c);	\
	}			\
}

/* 
 * LPUT adds a character to the formatting buffer.
 */

#define LPUT(x) {	\
	(*(bptr->f_lbufptr)++	= (long) x);	\
	bptr->f_lbufcnt++;			\
}

#define LPUTS(string) {		\
	s	= string;	\
	while (c = *s++) {	\
		LPUT(c);	\
	}			\
}

/*
 * NLINE determines whether user specified new line for each variable.
 */

#define NLINE() { \
	bptr->lcomma	= 0;	/* suppress commas except for arrays */	\
	if (OUT_LINE) {	\
		REPFLUSH();	/* Write out what's in outbuff */	\
	}		\
}

/*
 * REPFLUSH writes what's in outbuff.
 * Reset pointers and counters so we start at the beginning of the buffer.
 * The first character in outbuff is used for carriage control.
 */

#define REPFLUSH() {			\
	if (_fwch(cup, bptr->outbuff, recsize - bptr->outcnt, 1) < 0)\
		RERR(css, errno);		\
	bptr->outptr	= bptr->outbuff;\
	*bptr->outptr++	= (long) ' ';	\
	*bptr->outptr++	= (long) ' ';	\
	bptr->outcnt	= recsize - 2;	\
}

/*
 * @WNL	- write namelist
 *
 * @WNL	
 *	set up namelist and entry pointers
 *	output namelist name in proper format
 *	do
 *		output variable name
 *		output value based on type
 *		point to next entry
 *	output end line
 * end @WNL
 */

int
@WNL(
	_f_int		*unump,		/* Unit number or dataset name */
	Namelist	*nl,		/* Namelist structure */
	int		errf		/* Nonzero if ERR specified */
)
{
	unum_t		unum;
	int		errn;
	int		n, ss; 
	void		*vaddr;		/* variable address */
	unsigned	elsize;		/* size in bytes of the variable */
	long		recsize;	/* number of characters to output per
					 * line.  Used by REPFLUSH.*/
	char		c;		/* needed by NLPUTS macro */
	char		*s;		/* needed by NLPUTS macro */
	unit		*cup;		/* unit pointer */
	Nlentry		*nlent;
	FIOSPTR		css;
	struct BUFFERS	wnlbuffers;
	struct BUFFERS	*bptr;
	bptr		= &wnlbuffers;
	bptr->f_lbuf	= NULL;

	unum		= *unump;

	GET_FIOS_PTR(css);
	STMT_BEGIN(unum, 0, T_WNL, NULL, css, cup);

	if (cup == NULL) {	/* if not connected */
		cup	= _imp_open77(css, SEQ, FMT, unum, errf, &errn);
		/*
		 * If the open failed, cup is NULL and errn contains
		 * the error number.
		 */
		if (cup == NULL)
			RERR(css, errn);
	}

	/* Set various unit table fields */

	cup->uflag	= (errf != 0 ? _UERRF : 0);
	cup->ulineptr	= cup->ulinebuf;
	cup->uwrt	= 1;		/* Set write flag */

	/* Set fields in the Fortran statement state structure */

	css->u.fmt.nonl		= 0;	/* Clear no-newline flag */


	if (cup->useq == 0)	/* If direct access file */
		RERR(css, FESEQTIV); /* Sequential attempted on direct access */

	if (!cup->ufmt)		/* If unformatted file */
		RERR(css, FEFMTTIV); /* Formatted attempted on unformatted */

	if ((cup->uaction & OS_WRITE) == 0) 
		RERR(css, FENOWRIT);

	bptr		= &wnlbuffers;
	bptr->lcomma	= 0;

	/*
	 * Set up record size.  The hierarchy for determining Namelist
	 * output record size is as follows:
	 *	1) RECL, if specified
	 *	2) WNLLONG(), if set and does not exceed cup->urecsize
	 *	3) list-directed output record size (cup->uldwsize)
	 *
	 * Note that while (1) and (3) are established at OPEN time, (2)
	 * can be changed ``on the fly''; therefore, this check has to
	 * be performed here.
	 */

	recsize	= cup->uldwsize;

	if (cup->urecl == 0 && _wnlrecsiz > 0)	/* No RECL and WNLLONG() set */
			recsize	= MIN(cup->urecsize, _wnlrecsiz);

	bptr->outcnt	= recsize - 1; 	/* First char. for carriage control */
	bptr->outbuff	= cup->ulinebuf;
	bptr->outptr	= bptr->outbuff;
	*bptr->outptr++	= OUT_ECHO;	/* First character of first line */
	bptr->f_lbuf	= (long *) malloc((recsize + 1) * sizeof(long));

	if (bptr->f_lbuf == NULL)
		RERR(css, FENOMEMY);	/* No memory */

	/* NAMELIST delimiter to output line */

	NLPUT(OUT_CHAR);		/* output delimiter */	
	NLPUTS(nl->nlname);		/* unpack group name to buffer */
	NLPUT(' ');
	NLPUT(' ');
	NLINE();	/* Did user specify new line for each variable? */

	nlent	= nl->nlvnames;

	do {
		int	ntype;

		ntype	= _old_namelist_to_f77_type_cnvt[nlent->na.type];

		/*
		 * Always format output into f_lbufptr.
		 * After formatting, if it will fit, move it into outbuff.
		 * If it will not fit, write out what is already in outbuff,
		 * and then move in the newly formatted data.
		 */

		bptr->f_lbufptr	= bptr->f_lbuf;	
		bptr->f_lbufcnt	= 0;

		LPUTS(nlent->varname);		/* output variable name */
		LPUT(' ');
		LPUT(OUT_EQ);			/* output the replacement
						 * character. '=' by default. */

		n	= (nlent->na.offdim) ? nlent->na.nels : 1;

		if (ntype == DT_CHAR) {
			_fcd f;
			f	= *(_fcd *)(((unsigned long) nlent->va.varaddr +
					(long *)nl));
			vaddr = _fcdtocp(f);
			elsize = _fcdlen(f);
		}
		else {
			vaddr	= (void *)nlent->va.varaddr;
			elsize	= 0;
		}

		LPUT(' ');

		/* Output value */

		ss	= l_write(css, cup, vaddr, elsize, n, 1, ntype, recsize,
					errf, bptr);

		if (ss != 0) {
			RERR(css, ss);
		}

		NLINE();

		nlent++;	/* point to next variable description */

	} while (nlent->varname[0]);

	if (bptr->outcnt < 6) {
		REPFLUSH();	/* Make sure there's room for "  &END" */
		bptr->outptr--; /* start in col. 2 */
		bptr->outcnt++;
	}

	NLPUT(OUT_CHAR);
	NLPUTS("END");
	REPFLUSH();
ret:

	STMT_END(cup, T_WNL, NULL, css);	/* Unlock the unit */

	if (bptr->f_lbuf != NULL)		/* Free formatting buffer */
		free(bptr->f_lbuf);

	return(CFT77_RETVAL(ss));
}

/*
 * l_write - output the value.
 */

static int
l_write(
	FIOSPTR		css,
	unit		*cup,	/* Current unit pointer */
	void		*dptr,	/* Address of data */
	unsigned	elsize,	/* Bytes per element (used for char type only)*/
	int		count,	/* Number of elements */
	int		inc,	/* Number of words per element */
	int		type,	/* Type of data */
	long		recsize,/* Number of characters to output per line */
	int		errf,
	struct BUFFERS	*bptr	/* Structure containing formatting buffers */
)
{
	unsigned int len77;
	char	*cp;		/* points to data if type is DT_CHAR */
	long	*ptr;		/* points to data if type is not DT_CHAR */
	long	ugly[ITEMBUFSIZ]; /* temporary buffer used for numeric output */
	long	dig;
	long	exp;
	long	mod;
	long	scl;
	long	ss;
	long	wid;
	long	*ib_ptr;	/* pointer into the current item buffer */
	long	*newp;
	int	lcount;		/* repeat count of current input data group */
	oc_func *gcf;		/* Generic NOCV-type conversion func */
	ftype_t	f90type;

	if (type == DT_CHAR) {
		/*
		 * Character data is unique in that one value may span
		 * more than one record when output. 
		 * When we can handle opening the output file with a
		 * 'DELIM=' descriptor (see Ansi 8x Fortran standard), this
		 * code will need to change. For now, delimit the constant
		 * with apostrophes, and double all internal apostrophes.
		 */

		cp	= dptr;
		len77	= elsize;

		for (; count > 0; count-- ) {

			bptr->lcomma	= 0;

			if (count > 1) {
				/*
				 * If we have an array of character data, 
				 * determine if any values are repeated.
				 */
				cp	= char_rep(cp, count, len77, &lcount,
							bptr);
				count	= count - (lcount - 1);
			}	

			/* Write the character constant */

			ss	= lw_A(css, cp, len77, recsize, cup, errf, bptr);

			if (ss != 0) {
				RERR(css, ss);
			}

			cp	= cp + len77;
		} /* for */

		return(0);

	} /* if (type == DT_CHAR) */

	/* Noncharacter data */

	ptr	= (long *)dptr;
	f90type	= _f77_to_f90_type_cnvt[type];

	if ((type == DT_DBLE) || (type == DT_CMPLX))
		inc	= inc + inc;

	for (; count > 0; count--, ptr += inc) {

		if (count > 1) {	/* find repeat values */

			ptr	= find_rep(ptr, count, inc, type, &lcount,
						bptr);

			count	= count - (lcount - 1);
		}

		ib_ptr = bptr->f_lbufptr;

		switch (type) {		/* set up for each data type */

		case DT_NONE:
			gcf = _s2uo;	mod = MODEUN;	wid = WOCTWRD;	
			dig = WOCTWRD;	exp = 0;	scl = 0;
			break;

		case DT_SINT:
		case DT_INT:
			gcf = _s2ui;	mod = 0;	wid = WINT;	
			dig = 1;	exp = 0;	scl = 0;
			break;

		case DT_REAL:
		case DT_CMPLX:
			gcf = _sd2uge;	mod = 0;	wid = WREAL8;
			dig = _dreal8;	exp = DEXP8;	scl = 1;
			if (YMP80) dig = 9;
			break;

		case DT_DBLE:
			/*
			 * When printing with D format, decrease
			 * the digits by one because we are setting
			 * the scale factor to 1.  This ensures that
			 * _dreal16 digits of precision are printed.
			 */
			gcf = _sd2udee;	mod = MODEDP;	wid = WREAL16;
			dig = _dreal16-1; exp = DEXP16;	scl = 1;
			if (YMP80) dig = 25;
			break;
		}

		/*
		 *	Perform the output conversion.
		 */

		switch (type) {		/* set up for each data type */

		default:	/* Integer, Short Integer, Real, or Double */

#if	_F_REAL16 == 1		/* suppress if _f_dble is not fully supported */
			if (YMP80 && !cup->uft90 && type == DT_DBLE &&
				*(_f_dble *)ptr == 0.0) {

				static const char *zero_dp = "0.0E+00";
				ib_ptr += _unpack(zero_dp, ib_ptr,
						strlen(zero_dp), -1);
				break;
			}
#endif

			newp	= gcf(ptr, ugly, &mod, &wid, &dig, &exp, &scl);

			if (type == DT_NONE)
				*newp++	= 'B';

			ib_ptr	= ib_ptr + _wnl_beautify(f90type, ugly, newp,
							ib_ptr, cup->uft90);
			break;	

		case DT_CMPLX:

			*ib_ptr++	= '(';

			newp	= gcf(ptr, ugly, &mod, &wid, &dig, &exp, &scl);

			ib_ptr	= ib_ptr + _wnl_beautify(f90type, ugly, newp,
						ib_ptr, cup->uft90);

			*ib_ptr++	= COMMA;

			newp	= gcf((_f_real *)ptr + 1, ugly,
					&mod, &wid, &dig, &exp, &scl);

			ib_ptr	= ib_ptr + _wnl_beautify(f90type, ugly, newp,
						ib_ptr, cup->uft90);

			*ib_ptr++	= ')';

			break;

		case DT_LOG:
			*ib_ptr++	= _lvtob(*(_f_log8 *)ptr)? 'T':'F';
			break;
		} /* switch */

		/*
		 *	Update the item buffer pointers before using LPUT again.
		 */
		bptr->f_lbufcnt += ib_ptr - bptr->f_lbufptr;
		bptr->f_lbufptr = ib_ptr;

		LPUT(OUT_SEP);
		LPUT(' '); 		/* put 2 blanks between items */
		LPUT(' '); 

		if (bptr->outcnt <= bptr->f_lbufcnt) {
			/*
			 * If there is not enough room in the line buffer
			 * to copy the next output value, flush out the line
			 * and start a new line.
			 */

			REPFLUSH();
		}

		bptr->f_lbufptr	= bptr->f_lbuf;

		_memwcpy(bptr->outptr, bptr->f_lbufptr, bptr->f_lbufcnt);

		bptr->outptr   += bptr->f_lbufcnt;
		bptr->outcnt   -= bptr->f_lbufcnt;
		bptr->f_lbufptr	= bptr->f_lbuf;
		bptr->f_lbufcnt	= 0;
	}

	return(0);

ret:
	return(ss);
}

static int dont_display_repeats;

static void should_display_repeats(void) __attribute__((constructor));

static void should_display_repeats(void)
{
	dont_display_repeats = getenv("FTN_SUPPRESS_REPEATS") != NULL;
}

/*
 * find_rep: find and put out the repeat count.
 * Returns a pointer to the last repeated value.
 * Sets lcount to the repeat count.
 */

static long *
find_rep(
	long		*ptr,	/* Pointer to the value */
	int		count,	/* Number of elements in array */
	int		inc,	/* Size (in words) of each value */
	int		type,	/* Type of data */
	int		*lcount,/* Repeat count */
	struct BUFFERS	*bptr	/* Structure containing formatting buffers */
)
{
	int	i;
	long	*p1, *p2, *q1, *q2; 

	if (dont_display_repeats) {
		*lcount = 1;
		return ptr;
	}
	
	p1	= ptr;
	q1	= ptr + inc;

	if (type == DT_CMPLX || type == DT_DBLE) {

		p2	= p1 + 1;
		q2	= q1 + 1;

		for (i = 1; i < count; i++) {

			if ((*p1 != *q1) || (*p2 != *q2)) {
				break;
			}
			else {
				p1	= q1;
				p2	= p1 + 1;
				q1	= q1 + inc;
				q2	= q1 + 1;
			}
		}
	}
	else {
		for (i = 1; i < count; i++) {

			if (*p1 != *q1) {
				break;
			}
			else {
				p1	= q1;
				q1	= q1 + inc;
			}
		}
	}

	*lcount	= (long) i;

	if (i > 1)		/* put out repeat count */
		writ_rep(i, bptr);

	return(p1);
}

static void
writ_rep(
	long		repcnt,/* Repeat count */
	struct BUFFERS	*bptr	/* Structure containing formatting buffers */
)
{
	long	mode;		/* Used by conversion routine */
	long	wid;
	long	dig;
	long	zero = 0;
	long	*newp;		/* Used by conversion routine */
	long	*q;
	long	buf[WINT];

	mode	= 0;
	wid	= WINT;
	dig	= 0;

	newp	= _s2ui((long*)&repcnt, buf, &mode, &wid, &dig, &zero, &zero);

	for (q = buf; q < newp; q++)
		if ((char)*q != ' ')	/* suppress leading blanks */
			break;

	while (q < newp) {
		*bptr->f_lbufptr++	= *q++;
		bptr->f_lbufcnt++;
	}

	*bptr->f_lbufptr++	= (long) '*';	/* put out '*' */
	bptr->f_lbufcnt++;
}

/*
 * char_rep: find and put out the repeat count for character data.
 * Returns a pointer to the last repeated value.
 * Sets lcount to the repeat count.
 */

static char *
char_rep(
	char		*ptr,	/* Pointer to first data value */
	int		count,	/* Number of elements in array */
	unsigned int	len77, 	/* Length of character variable */	
	int		*lcount,/* Repeat count */
	struct BUFFERS	*bptr	/* Structure containing formatting buffers */
)
{
	int	i;
	char	*qptr;

	qptr	= ptr + len77;	/* point to start of next array */

	for (i = 1; i < count; i++) {

		if (memcmp(ptr, qptr, len77))
			break;

		qptr	= qptr + len77;
	}

	*lcount	= (long)i;

	if (i > 1) 	/* put out repeat count */
		writ_rep(i, bptr);

	return(ptr + (*lcount - 1) * len77);	
}

/*
 *	lw_A - write ASCII character data 
 */

static int
lw_A(
	FIOSPTR		css,
	char		*ptr,	/* Points to character data to be output */
	int		charlen,/* Length of data to be output */
	long		recsize,/* Number of characters per line for REPFLUSH */
	unit		*cup,	/* Unit table pointer */
	int		errf,	/* Error flag */
	struct BUFFERS	*bptr	/* Structure containing formatting buffers */
)
{
	int	m;
	char	*aposptr;
	int	ss;
	int	fflag;
	int	recmax;

	/*
	 * Copy the data into the formatting buffer. The data is
	 * surrounded by apostrophes. If there is an apostrophe in 
	 * the data it must be output as two apostrophes.
	 */

	fflag			= 0;
	*bptr->f_lbufptr++	= (long) '\'';
	bptr->f_lbufcnt++;

	for (; charlen > 0; ) {

		if (fflag == 0) {
			recmax	= recsize - 2;
			m	= MIN(charlen, recmax - bptr->f_lbufcnt);
		}
		else {
			recmax	= recsize - 1;
			m	= MIN(charlen, recmax - bptr->f_lbufcnt);
		}

		/* Is there an apostrophe in the data? */

		aposptr	= memchr(ptr, '\'', m);

		if (aposptr != 0) {
			/* aposptr points to next apostrophe */
			m	= aposptr + 1 - ptr;
			/* Move everything up to, and including, apostrophe */

			(void) _unpack(ptr, bptr->f_lbufptr, m, -1);

			*(bptr->f_lbufptr + m)	= '\'';	/* Double apostrophe */
			ptr			= ptr + m;
			charlen			= charlen - m;
			m++;
		}
		else {
			/* Move everything */

			(void) _unpack(ptr, bptr->f_lbufptr, m, -1);

			ptr	= ptr + m;
			charlen	= charlen - m;
		}

		bptr->f_lbufptr += m;
		bptr->f_lbufcnt += m;
			
		/*
		 * If we've filled a record, write it out.
		 */

		if (bptr->f_lbufcnt >= recmax) {
			if (bptr->outcnt <= bptr->f_lbufcnt) {
				REPFLUSH();
				/* If this is a continuation of one */
				/* character variable, start it in col. 2 */
				/* Otherwise, start it in col. 3 */
				if (fflag == 1) {
					bptr->outptr--; /* start in col. 2 */
					bptr->outcnt++;
				}
				fflag	= 1;
			}
			bptr->f_lbufptr	= bptr->f_lbuf;

			_memwcpy(bptr->outptr, bptr->f_lbufptr,
					bptr->f_lbufcnt);

			bptr->outptr   += bptr->f_lbufcnt;
			bptr->outcnt   -= bptr->f_lbufcnt;
			bptr->f_lbufptr	= bptr->f_lbuf;
			bptr->f_lbufcnt	= 0;
		}
	} /* for */

	*bptr->f_lbufptr++	= (long) '\'';
	bptr->f_lbufcnt++;

	LPUT(OUT_SEP);
	LPUT(' ');
	LPUT(' ');

	bptr->lcomma	= 1;

	if (bptr->outcnt <= bptr->f_lbufcnt) {
		/* If there is not enough room in outbuff to copy 
		 * in the contents of f_lbuf,
		 * write what's in outbuff
		 */
		REPFLUSH();
		/* If this is a continuation of 1 character variable, */
		/* start it in col. 2. Otherwise, start it in col. 3*/
		if (fflag == 1) {
			bptr->outptr--; 
			bptr->outcnt++;
		}
	}

	bptr->f_lbufptr	= bptr->f_lbuf;

	_memwcpy(bptr->outptr, bptr->f_lbufptr, bptr->f_lbufcnt);

	bptr->outptr   += bptr->f_lbufcnt;
	bptr->outcnt   -= bptr->f_lbufcnt;
	bptr->f_lbufptr	= bptr->f_lbuf;
	bptr->f_lbufcnt	= 0;

	return(0);

ret:
	return(ss);
}

/*
 *	_wnl_beautify
 *
 *		Beautify numeric output by deleting blanks and
 *		truncating unnecessary trailing zeroes.  The altered
 *		ascii number is placed in "pretty".
 *
 *		Input is in this form: {LH part}[{E}{exponent}]
 *
 *		This routine is temporary, and is needed only as long
 *		as the YMP UNICOS 8.0 compatibility mode is preserved for
 *		namelist real output.
 *
 *	Return value:
 *		The number of characters in the beautified output.
 */
int
_wnl_beautify(
	ftype_t		typ90,	/* f90 data type of the number */
	long		*ugly,	/* the ugly ascii representation of a number */
	long		*p_limit,/* ptr to one past end of ascii data in ugly */
	long		*pretty,/* receives the beautified output */
	unsigned	isf90)	/* 1 iff Fortran 90 style printing of 0.E+0 */

{
	int	ret;

	ret	= _beautify(typ90, ugly, p_limit, pretty, isf90);

/*
 *	In YMP80 mode an extra 0 is always added in G-as-F conversions which
 *	have no digits following the decimal point.
 */
	if (YMP80 && !isf90 && typ90 == DVTYPE_REAL || typ90 == DVTYPE_COMPLEX){
		if (pretty[ret - 1] == '.')
			pretty[ret++]	= '0';
	}

	return (ret);
}
