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



#pragma ident "@(#) libf/fio/rnl90to77.c	92.3	06/21/99 10:37:55"

#include <stdio.h>
#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <stdlib.h>
#include <cray/fmtconv.h>
#include <cray/nassert.h>
#if !defined(_ABSOFT)
#include <sys/unistd.h>
#endif
#include "fio.h"
#include "namelist.h"
#include "rnl90def.h"
#include "fmt.h"

/* EXTERNAL entry points */
extern int _s_scan_extensions(void *ptr, ftype_t type, unsigned elsize,
	long *field_begin, unsigned rec_chars, int *fwptr, long cmode);
extern int _nicverr(const int _Nicverror);

/* use SUBGTC when the character retrieval cannot hit an end of file until
 * the retrieval is complete.  This occurs when retrieving the characters of
 * a name.  CMTSUBGT is the same except a constant is allowed in the scan.
 * These two macros are used from functions outside the main namelist FRN
 * routine.
 */

#define SUBGTC(x) { \
	while (cup->ulinecnt == 0) {				\
		if (errn = _nlrd_fillrec(css, cup, echoptr)) {	\
			return(errn);				\
		}						\
	}							\
	x	= (char) *cup->ulineptr++;			\
	cup->ulinecnt--;					\
}

#define CMTSUBGT(x) { \
	while (cup->ulinecnt == 0) {				\
		if (errn = _nlrd_fillrec(css, cup, echoptr)) {	\
			return(errn);				\
		}						\
	}							\
	x	= (char) *cup->ulineptr++;			\
	/* An f90 input comment is now part of RNLCOMM */	\
	if (MATCH(x, _MASKS, MRNLCOMM)) {			\
		x	= ' ';					\
		cup->ulinecnt	= 1;				\
	}							\
	cup->ulinecnt--;					\
}

#define CMTSUBGTNOEOR(x) { \
	if (cup->ulinecnt == 0) {				\
		x	= ' ';					\
	} else {						\
		x	= (char) *cup->ulineptr++;		\
		cup->ulinecnt--;				\
	}							\
	/* An f90 input comment is now part of RNLCOMM */	\
	if (MATCH(x, _MASKS, MRNLCOMM)) {			\
		x	= ' ';					\
		cup->ulinecnt	= 1;				\
	}							\
}

/* use MAINGT when the character retrieval can hit an end of file before 
 * retrieval is complete.  This occurs when retrieving '=', delimiters,
 * , etc.  CMTMAINGT is the same except a comment is allowed in the scan.
 * These two macros are used from functions within the main namelist FRN
 * routine.
 */

#define MAINGT(x) { \
	while (cup->ulinecnt == 0) {					\
		if (errn = _nlrd_fillrec(css, cup, echoptr)) {		\
			if (errn < 0) {				\
				ENDD(endf, css, FERDPEOF);		\
			}						\
			else {						\
				ERROR0(errf, css, errn);		\
			}						\
		}							\
	}								\
	x	= (char) *cup->ulineptr++;				\
	cup->ulinecnt--;						\
}

#define CMTMAINGT(x) { \
	while (cup->ulinecnt == 0) {					\
		if (errn = _nlrd_fillrec(css, cup, echoptr)) {		\
			if (errn < 0) {				\
				ENDD(endf, css, FERDPEOF);		\
			}						\
			else {						\
				ERROR0(errf, css, errn);		\
			}						\
		}							\
	}								\
	x	= (char) *cup->ulineptr++;				\
	/* An f90 input comment is now part of RNLCOMM */		\
	if (MATCH(x, _MASKS, MRNLCOMM)) {				\
		x	= ' ';						\
		cup->ulinecnt	= 1;					\
	}								\
	cup->ulinecnt--;						\
}

#define GETSECTION(x) { \
		field_begin	= cup->ulineptr;			\
		field_end	= cup->ulineptr;			\
		for (j = 0; j < cup->ulinecnt; j++) {			\
			x	= (char) *field_end;			\
			if (x == ')' || x == ',' || x == ':')		\
				break;					\
			field_end++;					\
		}							\
		field_width	= j;					\
}

/*
 * Use GETSTR77 to read a character string surrounded by quotes or
 * apostrophes. Comment characters are not recognized as such inside a
 * quoted string, so SUBGTC is used.  Skip the ending blank.
 */
#define GETSTR77() {							\
	if (cup->ulinecnt <= 1) {					\
		SUBGTC(ch);						\
	}								\
	SUBGTC(ch);							\
	if (ch == enddelim) {						\
		eos	= -1; /* end of string */			\
		SUBGTC(ch); /* unless string delimiter is doubled */	\
		if (ch == enddelim)					\
		eos	= 0;						\
		else {							\
			cup->ulineptr--;				\
			cup->ulinecnt++;				\
		}							\
	}								\
}

/*
 * eunit is unit for echoing inpt.  If rnlecho is 1, always echo.
 * If rnlecho is 0, echo only if 'E' in first column.
 */

struct Echoinfo {
	unum_t	eunit;
	int	rnlecho;
};

/*
 *	This table is used to drive the f90 input conversion based on the
 *	type of the data.
 */
ic_func *ncf_tab77[] = {
	NULL,		/* DVTYPE_UNUSED */
	NULL,		/* DVTYPE_TYPELESS */
	_iu2s,		/* DVTYPE_INTEGER */
	_defgu2sd,	/* DVTYPE_REAL */
	_defgu2sd,	/* DVTYPE_COMPLEX */
	NULL,		/* DVTYPE_LOGICAL */
	NULL,		/* DVTYPE_ASCII */
};


/* MATCH(c,a,b) determines whether the bit for character 'c' is set.
 * a[b] and a[b+1] are bit masks for each ASCII character
 */
#define MATCH(c,a,b)	(a[(c >= 0x3f) ? b+1 : b] & (1 << (IND(c))))

/* IND computes the bit index of a character */
#define IND(c)	((c >= 0x3f) ? 0x7f - (unsigned)c : (unsigned)(0x40 - c - 1))

static void _nlrdecho(unum_t eunit, long *input_ptr, long nchrs, FIOSPTR css);

static int _nlrd_fillrec(FIOSPTR css, unit *cup, struct Echoinfo *echoptr);

static void _setunit(char *string, void *u);

static int _getname(FIOSPTR css, unit *cup, char *buf, char *lastc,
	struct Echoinfo *echoptr);

static void _pr_echomsg(char *string);

static void _cnvrt_toupper(char *bufr);

static int _ishol(long *hlptr, unit *cup);

static nmlist_goli_t *_findname(char *key, nmlist_goli_t *nlvar,
		unsigned countitm);

static int _getnlval(FIOSPTR css, nmlist_goli_t *nlvar, char *lastc, 
	unit *cup, struct Echoinfo *echoptr);

static int _indx_nl(FIOSPTR css, unit *cup, struct Echoinfo *echoptr,
	long *begcnt, int *ndim, long strbegend[3], int *encnt, int *icnt,
	int arryflag);

static int _nlread(FIOSPTR css, ftype_t type, void *ptr, int cnt, int inc,
	char *lastc, unit *cup, struct Echoinfo *echoptr, int elsize);

static int _nexdata(FIOSPTR css, ftype_t type, void *ptr, int cnt, int inc,
	char lastc, unit *cup, struct Echoinfo *echoptr, long *lval,
	int *lcount, int elsize, int *nullvlu);

static int _g_charstr(FIOSPTR css, unit *cup, void *p, int cnt, char c,
	struct Echoinfo *echoptr, int lcount,int elsize, int *nullvlu);

static int _g_complx(FIOSPTR css, unit *cup, ftype_t type,
	struct Echoinfo *echoptr, long *lval,int elsize);

static int _g_number(ftype_t type, unit *cup,long *lval, int elsize);

static int _gocthex(FIOSPTR css, unit *cup, ftype_t type,
	struct Echoinfo *echoptr, long *lval, int base, int elsize,
	int *nullvlu);

static int _get_holl(FIOSPTR css, unit *cup, char holltype, int count,
	ftype_t type, struct Echoinfo *echoptr, long *lval, int elsize);

static int _get_quoholl(FIOSPTR css, unit *cup, char cdelim, ftype_t type,
	struct Echoinfo *echoptr, long *lval, int elsize);

/*
 *	_rnl90to77 - called by wnl90.c to process a cf77 namelist input
 *		file.
 *	Synopsis
 *		int _rnl90to77(FIOSPTR css,
 *				unit *cup,
 *				nmlist_group *namlist,
 *				void *stck,
 *				int errf);
 *		Where
 *			css	- pointer to css
 *			cup	- pointer to unit information
 *			namlist - pointer to the namelist table.
 *			stck	- pointer to stack space which is passed
 *				  to each call to _FRU for a particular
 *				  statement.  This is used by the library.
 *			errf	- error processing flag.
 *			endf	- end processing flag.
 *	Return value
 *		errn
 */

int
_rnl90to77(
	FIOSPTR		css,
	unit		*cup,
	nmlist_group	*namlist,
	void		*stck,
	int		errf,
	int		endf)
{
	long		stat;
	long		*hlptr;
	int		ret;
	int		ss;
	char		buf[MAXNAML + 5], c;
	char		skipmsg[sizeof(SKIPMSG) + sizeof(UNITSTR) +
				MAXNAML + 8 + 2];
	char		tmpbuf[MXUNITSZ];/* Unit number buffer for warn msgs */
	int		errn;		/* Error number			*/
	long		flag;		/* Error flag			*/
	unum_t		unum;		/* Actual unit number		*/
	unsigned	rlen;		/* group name length		*/
	unsigned	rcount;		/* count of namelist items	*/
	char		*rptr;		/* pointer to group name	*/
	char		*varptr;	/* ptr to group_obj_list item	*/
	unsigned	varlen;		/* len of group_obj_list name	*/
	nmlist_goli_t	*nlvar;		/* ptr to next variable entry	*/
	nmlist_goli_t	*fdvar;		/* ptr to next variable entry	*/
	ftype_t		type;
	struct Echoinfo echoinfo;
	struct Echoinfo *echoptr;
	type		= DVTYPE_UNUSED;
	varptr		= NULL;

/* **************************************************************************
 *	Data Transfer Section
 ************************************************************************* */

	unum	= cup->uid;
	echoptr	= &echoinfo;

	/* set up extended record. */
	if (cup->ulinecnt == 0)
		cup->ulinecnt	= 1;
	*(cup->ulinebuf + cup->ulinecnt)	= (_f_int) BLANK;
	(void) strcpy(skipmsg, SKIPMSG);

	/* Set up the unit used for echoing input lines */
	if (_OUT_UNIT < 0) {
		echoinfo.eunit	= 101; /* default = stdout */
		echoinfo.rnlecho = 0; /* no echo til 'E' in col 1 */
	}
	else {
		echoinfo.eunit	= _OUT_UNIT;
		echoinfo.rnlecho = 1;	/* always echo, ignore col1 */
	}
	/* Input record preREAD before this point.  Check for echoing. */
	if ((echoptr->rnlecho) ||
	    (MATCH(*cup->ulinebuf, _MASKS, MRNLFLAG))) {
		/* Begin echoing input */
		echoptr->rnlecho	= 1;
		_nlrdecho(echoptr->eunit, cup->ulinebuf, cup->ulinecnt, css);
	}
	cup->ulineptr	= cup->ulinebuf + 1;
fill:
	while (cup->ulinecnt == 0) {
		errn	= _nlrd_fillrec(css, cup, &echoinfo);
		if (errn != 0)
			goto err_eof;
	}
fill1:
	do {
		CMTMAINGT(c)
	} while (ISBLANK(c));
	if (!(MATCH(c, _MASKS, MRNLDELIM))) {
		/* irix f77 and cft77 skip the input record when the
		 * first nonblank character is not a dollar sign or
		 * an ampersand which delimits a namelist group name.
		 */
		cup->ulinecnt	= 0;
		goto fill;	/* Comment statement */
	}

	/* get first character of namelist group name from input record */
	MAINGT(c);
	/* and get namelist group name from input record */
	errn	= _getname(css, cup, buf, &c, &echoinfo);
	if (errn != 0)
		goto err_eof;
	/* convert group name to uppercase */
	_cnvrt_toupper(buf);

	assert ( (cup != NULL));
	rcount	= namlist->icount;	/* number of name table entries	*/
	rptr	= _fcdtocp(namlist->group_name);/* ptr to groupname */
	rlen	= _fcdlen(namlist->group_name);	/* len of groupname */
	nlvar	= namlist->goli;		/* group object ptr	*/

	if (strncmp(rptr,buf,rlen)) {
		int i;
		/* do not put out skipped record message for assign -f
		 * irixf77 or irixf90 option, or 'assign -Y on' option.
		 */
		if ((cup->ufnl_skip != 0) ||
		    (cup->ufcompat == AS_IRIX_F77) ||
		    (cup->ufcompat == AS_IRIX_F90))
			goto get_delim;
		if (_SKP_MESS > 0) {
			/* Skip record and issue a logfile message */
			(void) strcpy(&skipmsg[sizeof(SKIPMSG)-1], buf);
			(void) strcat(skipmsg, UNITSTR);
			_setunit(tmpbuf, &unum);
			/*
			 * The following truncates the file name/unit number
			 * to seven characters, which will result in a loss
			 * of information when the unit number is larger than
			 * 9,999,999.
			 */
			(void) strncat(skipmsg, tmpbuf, sizeof(long) - 1);
			(void) strcat(skipmsg, "\n");
			_pr_echomsg(skipmsg);
		}
		else if (_SKP_MESS < 0) {
			/* Abort job or go to optional ERR= branch */
			errn	= FENLIVGP;
			ERROR1(errf, css, errn, buf);
		}
get_delim:
		/* the name is not the namelist group name needed,
		 * read until delimiter found.
		 */
		while (!MATCH(c, _MASKS, MRNLDELIM) && c != '/') {
			if (c == '\'' || c == '"') {
				char	qchar;
				qchar	= c;
rquote:
				do {
					MAINGT(c);
				} while (c != qchar);
				MAINGT(c);
				/* check for double quote */
				if (c == qchar)
					goto rquote;
			}
			else {
				CMTMAINGT(c);
			}
		}
		/*
		 * Try to determine whether delimiter is part of a
		 * Hollerith string by looking back in record.  If it
		 * is part of a Hollerith string, it's not really an
		 * end delimiter.
		 */
		hlptr	= cup->ulineptr - 2;
		/*
		 * Search for nH, nh, nl, nL, nr, nR where n = digit.
		 * Only look back the number of characters in a default
		 * integer or to the beginning of this line of input
		 */
		for (i = 0; i < (sizeof(_f_int)) &&
		   hlptr > &cup->ulinebuf[2]; i++, hlptr--) {
			switch((char) *hlptr) {
				case 'h':
				case 'H':
				case 'l':
				case 'L':
				case 'r':
				case 'R':
					if (_ishol(hlptr, cup)) {
						CMTMAINGT(c);
						goto get_delim;
					}
					break;
				default:
					break;
			} /* switch */
		}
		goto fill1;
	}
	/*
	 *	This is the correct namelist group name.  Process the
	 *	input record. Read until the input record or records
	 *	until the terminating character is found.  This is a
	 *	slash or ampersand or MRNLDELIM.
 	 */
	while (c != '/') { 
		int	sepcnt;
		if (MATCH(c, _MASKS, MRNLDELIM))
			goto finalization;
		/* get group_object_name from input record */
		errn	= _getname(css, cup, buf, &c, &echoinfo);
		if (errn != 0)
			goto err_eof;
		_cnvrt_toupper(buf);
		/* find matching group_object_name from namelist table */
		if (!(fdvar = _findname(buf, nlvar, rcount))) {
			if (strlen(buf) > 0) {
				/* An objectlistname in input record */
				errn	= FENLNREC;
				ERROR1(errf, css, errn, buf);
			}
			else {
				/* No object list name in input record */
				errn	= 0; /* empty variable entry */
				goto finalization;
			}
		}
		/* we're positioned just after the object name
		 * so get following value(s)
		 */
		errn	= _getnlval(css, fdvar, &c, cup, &echoinfo);
		if (errn != 0)
			goto err_eof;
		sepcnt	= 0;
		for ( ; ; ) {
			if (!(ISBLANK(c))) {
				if ((MATCH(c, _MASKS, MRNLSEP)) &&
				    (sepcnt == 0)) {
					/* skip separator */
					sepcnt++;
				}
				else
					break;
			}
			CMTMAINGT(c);
		}
	}

/***************************************************************************
 *	Statement Finalization Section
 ***************************************************************************/
finalization:
	return(errn);
err_eof:
	/* err and eof handling */
	if(errn < 0) {
		ENDD(endf, css, FERDPEOF);
	} else if (errn == FENLSTRN || errn == FENLSTRG ||
		   errn == FENLSUBD || errn == FENLSUBN ||
		   errn == FENLSUBS || errn == FENLIVIT ||
		   errn == FENLARSC || errn == FENLLGNM ||
		   errn == FENLUNKI || errn == FENLUNKN) {
			ERROR1(errf, css, errn, buf);
	} else {
		ERROR0(errf, css, errn);
	}
	goto finalization;
}

/* _nlrd_fillrec - namelist read of one record from a file
 *	returns		0 - successful
 *			EOF - end of file
 *			ERR - error was encountered
 *			cup->uend is set if EOF encountered
 */

static int
_nlrd_fillrec(FIOSPTR css, unit *cup, struct Echoinfo *echoptr)
{
	register int	errn;

	errn	= css->u.fmt.endrec(css, cup, 1);

	if (errn != 0) {
		return(errn);
	} else {
		if (cup->ulinecnt == 0)
			cup->ulinecnt	= 1; /* Assume it has 1 blank */
		/* Add a blank character to end of record */
		*(cup->ulinebuf + cup->ulinecnt)	= (long) BLANK;
		if ((echoptr->rnlecho) ||
		    (MATCH(*cup->ulinebuf, _MASKS, MRNLFLAG))) {
			/* Begin echoing input */
			echoptr->rnlecho	= 1;
			_nlrdecho(echoptr->eunit, cup->ulinebuf,
				cup->ulinecnt, css);
		}
		/* Always skip the first character in a record.
		 * Don't adjust ulinecnt because blank added at the end.
		 */
		cup->ulineptr++;
	}
	return(errn);
}

/*
 *	_getname - Get variable name or group name
 *
 *	On entry:
 *		- Positioned to a name possibly preceded by blanks
 *	On exit:
 *		- 0 if successful
 *		  EOF if end of file read
 *		  > 0 if other error (errno will be set)
 *		- *cup->ulineptr is record position after the name.
 *		- *lastc contains the last character read.
 *	In looking for the name, we stop when we see a space, '=', or
 *	'(', or delimiter ('&'), or the replacement character for '='.
 */

static int
_getname(FIOSPTR css, unit *cup, char *s, char *lastc, struct Echoinfo *echoptr)
{
	char	*p, c;
	int	n, errn;
	errn 	= 0;
	n	= MAXNAML + 5; /* real*16 input can be 34 characters long */
	p	= s;
	c	= *lastc;
	/*
	 * Names cannot have embedded blanks.  In cf77 compatibility mode,
	 * a comment can immediately follow the name and will terminate it.
	 */
	while (ISBLANK(c))
		CMTSUBGT(c);

	while (!(ISBLANK(c)) && (c != '(') && !(MATCH(c, _MASKS, MRNLREP)) &&
	       !(MATCH(c, _MASKS, MRNLDELIM)) && (c != '/')) {
		*p++	= c;
		CMTSUBGTNOEOR(c);
		if (n-- == 0) {
			errn	= FENLLGNM;	/* name too long */
			p--;
			break;
		}
	}
	*lastc	= c;
	*p	= '\0';
	return (errn);
}

/*
 * _findname - find variable name in list of nmlist_goli_t entries
 *		of namelist table
 * On entry:
 *	- lastc points to character following name in input buffer.
 * Returns:
 *	pointer to matching object list entry
 *	NULL if variable name was not found.
 */

static nmlist_goli_t
*_findname(char *key, nmlist_goli_t *nlvar, unsigned countitm)
{
	char		*varptr;
	unsigned	varlen;
	nmlist_goli_t	*newitem;
	int		cnt, lcnt;

	newitem	= nlvar;
	cnt	= countitm;
	lcnt	= strlen(key);

	while (cnt--) {
		varptr	= _fcdtocp(newitem->goli_name);
		varlen	= _fcdlen(newitem->goli_name);
		if ((varlen == lcnt) && (!strncmp(key, varptr, lcnt)))
			return (newitem);
		else
#if defined(__mips) && (_MIPS_SZLONG == 32)
			newitem	= (nmlist_goli_t*)((long *)newitem +
				3 + (sizeof(_fcd))/(sizeof(long)));
#else
			newitem	= (nmlist_goli_t*)((long *)newitem +
				2 + (sizeof(_fcd))/(sizeof(long)));
#endif
	}
	return (NULL);
}

/* _getnlval - get values for namelist io
 *
 * On entry:
 *	- positioned after variable name
 *	- lastc contains the character following the name
 * On exit:
 *	- *lastc contains the character following the value
 *	- cup->ulineptr is pointing to the character following lastc
 * 	- returns: 0 if successful
 *		-value if EOF detected
 *		> 0 if error detected
 */

static int
_getnlval(FIOSPTR css, nmlist_goli_t *nlvar, char *lastc, unit *cup,
	struct Echoinfo *echoptr)
{
	long		ss, cntp;
	long		stat;
	int		ndim = 0;
	int		i;
	int		encnt = 0;
	int		icnt = 0;
	long		begcnt[MAXDIM];
	long		strbegend[3];
	char		*cp;
	char		c;
	long		vaddr;
	long		errn = 0;
	/* clear array element and substring information */
	for (i=0; i < MAXDIM; i++) {
		begcnt[i]	= 0;
	}
	strbegend[0]	= -1;
	strbegend[1]	= -1;
	strbegend[2]	= -1;

	switch (nlvar->valtype) {
	case IO_SCALAR:
	{
		nmlist_scalar_t *nlscalar; /* nmlist scalar entry */
		unsigned	elsize;
		unsigned	int_len;
		void		*vaddr;
		ftype_t		type;	/* fortran data type */

		nlscalar	= nlvar->goli_addr.ptr; /* ptr to scalar */
		type	= nlscalar->tinfo.type;
		int_len	= nlscalar->tinfo.int_len;
		/* Assertions */
		assert (type >= DVTYPE_TYPELESS && type <= DVTYPE_ASCII);
		assert(nlscalar->tinfo.int_len > 0 );
		if ((type != DVTYPE_ASCII) && (*lastc == '(')) {
			errn	= FENLUNKI;
			break;
		}
		if (type == DVTYPE_ASCII)
			strbegend[0]	= 0;
		/* find offset if indexed array */
		if (*lastc == '(') {
			errn	= _indx_nl(css, cup, echoptr, begcnt, &ndim,
					strbegend, &encnt, &icnt, 0);
			if (errn != 0) {
				if (errn == FENLSUBS)
					errn	= FENLSTRG;
				else if (errn == FENLSUBN)
					errn	= FENLSTRN;
				break;
			}
		}
		else {
			while (ISBLANK(*lastc)) {
				CMTSUBGT(*lastc);
			}
			if (MATCH(*lastc, _MASKS, MRNLDELIM) ||
			    (*lastc == '/')) {
				errn	= 0;
				break;
			}
			/* match '=' or special character */
			if (!(MATCH(*lastc, _MASKS, MRNLREP))) {
				errn	= FENLNOVL;
				break;
			}
		}
		CMTSUBGT(*lastc);

		/* Currently positioned after the '=' sign, but lastc is
		 * pointing at the '=' sign.  Update lastc for nlread and
		 * compute:
	 	 * cntp = number of array elements to be read
	 	 *	(1 if not an array).
	 	 * elsize = size of a variable or array element
	 	 *	(words for nonchar, bytes for char).
	 	 * vaddr = target address for the input value.  For
	 	 *	character, a Fortran character descriptor.
	 	 */
		if (type == DVTYPE_ASCII) {
			char	*wptr;
			const int bytesperchar = 1;
			int 	begt	= strbegend[1];
			int 	endt	= strbegend[2];
			wptr	= _fcdtocp(nlscalar->scal_addr.charptr);
			elsize	= _fcdlen(nlscalar->scal_addr.charptr);
			elsize	= elsize * bytesperchar;
			/* check for character substrings in input record */
			if (strbegend[0] > 0) {
				if (begt < 1 )
					begt	= 1;
				else if (begt > elsize) {
					errn	= FENLUNKN;
					break;
				}
				if (endt < 1 )
					endt	= elsize;
				else if ((endt > elsize) || (endt < begt)) {
					errn	= FENLUNKN;
					break;
				}
				wptr	= wptr + (begt - 1);
				elsize	= (endt - begt) + 1;
			}
			vaddr	= wptr;
		}
		else {
			vaddr	= nlscalar->scal_addr.ptr;
			elsize	= int_len >> 3;
		}
		c	= *lastc;
		cntp	= 1;
		errn	= _nlread(css, type, vaddr, cntp, 0, &c, cup, echoptr,
				elsize);
		*lastc	= c;
		break;
	}
	case IO_DOPEVEC:
	{
		struct DvDimen	*dvdimn;
		struct DvDimen	dimen[MAXDIM];
		DopeVectorType	*nldv;
		unsigned	elsize;
		unsigned	extent = 1;
		unsigned	int_len;
		void		*vaddr;
		int		nc, mult, offs;
		ftype_t		type;	/* fortran data type */
		nldv	= nlvar->goli_addr.dv; /* ptr to dope vector */
		mult	= 1;
		offs	= 0;

		/* Assertions */
		assert ( nldv != NULL );
		assert ( nldv->type_lens.int_len > 0 );
		type	= nldv->type_lens.type;
		if (type == DVTYPE_ASCII)
			strbegend[0]	= 0;
		for (i=0; i < nldv->n_dim; i++) {
			begcnt[i]	= nldv->dimension[i].low_bound;
		}

		/* find offset if indexed array */
		if (*lastc == '(') {
			errn	= _indx_nl(css, cup, echoptr, begcnt, &ndim,
					strbegend, &encnt, &icnt, 1);
			if (errn != 0)
				break;
		}
		else {
			while (ISBLANK(*lastc)) {
				CMTSUBGT(*lastc);
			}
			/* match '=' or special character */
			if (!(MATCH(*lastc, _MASKS, MRNLREP))) {
				return(FENLNOVL);
			}
		}
		CMTSUBGT(*lastc);

		/* Currently positioned after the '=' sign, but lastc is
		 * pointing at the '=' sign.  Update lastc for nlread and
		 * compute:
	 	 * cntp = number of array elements to be read
	 	 *	(1 if not an array).
	 	 * elsize = size of a variable or array element
	 	 *	(words for nonchar, bytes for char).
	 	 * vaddr = target address for the input value.  For
	 	 *	character, a Fortran character descriptor.
	 	 */
		int_len	= nldv->type_lens.int_len;
		if ((ndim != 0) && (ndim != nldv->n_dim)) {
			errn	= FENLBNDY;
			break;
		}
		for (nc = 0; nc < nldv->n_dim; nc++) {
			extent *= nldv->dimension[nc].extent;
		}
		if (ndim > 0) {
			offs	= begcnt[0] - (nldv->dimension[0].low_bound);
			for (nc = 1; nc < ndim; nc++) {
				mult	= mult * (nldv->dimension[nc-1].extent);
				offs	= offs + ((begcnt[nc] -
				   nldv->dimension[nc].low_bound) * mult);
			}
			extent	= extent - offs;
		}
		if (type == DVTYPE_ASCII) {
			char	*wptr;
			const int bytesperchar = 1;
			int	begt = strbegend[1];
			int	endt = strbegend[2];
			wptr	= _fcdtocp(nldv->base_addr.charptr);
			elsize	= _fcdlen(nldv->base_addr.charptr);
			elsize	= elsize * bytesperchar;
			/* check for character substrings in input record */
			wptr += offs * elsize;
			if (strbegend[0] > 0) {
				if (begt < 1 )
					begt	= 1;
				else if (begt > elsize) {
					errn	= FENLUNKN;
					return(errn);
				}
				if (endt < 1 )
					endt	= elsize;
				else if ((endt > elsize) || (endt < begt)) {
					errn	= FENLUNKN;
					break;
				}
				wptr	= wptr + (begt - 1);
				elsize	= (endt - begt) + 1;
			}
			vaddr	= wptr;
		}
		else {
			bcont	*iwptr;
			iwptr	= (bcont*)nldv->base_addr.a.ptr;
			elsize	= int_len >> 3;
			iwptr  += offs * (elsize / (sizeof(bcont)));
			vaddr	= iwptr;
		}
		/* Assertions */
		assert ( elsize > 0 && extent > 0 );
		c	= *lastc;
		cntp	= extent;
		errn	= _nlread(css, type, vaddr, cntp, 1, &c, cup, echoptr,
				elsize);
		*lastc	= c;
		break;
	}
	case IO_STRUC_A:
	case IO_STRUC_S:
	{
		/* do not allow structures in cf77 files. */
		errn	= FENLSTCT;
		break;
	}
	default:
		 errn	= FEINTUNK;
	}
	return(errn);
}

/*	_nlread - calls _nexdata to get the next value and stores the
 *		result in the namelist object entry.
 *	On Entry - cup_ulineptr points to the first character following the
 *		value.
 *	On Exit - lastc will contain the first nonblank, nonseparator
 * 		character following the value.
 */

static int
_nlread(FIOSPTR css, ftype_t type, void *ptr, int cntp, int incrm,
	char *lastc, unit *cup, struct Echoinfo *echoptr, int elsize)
{
	long		ss, ncntp;
	long		stat;
	char		c;
	void		*vaddr;
	long		errn = 0;
	int		lcount;		/* repeat count for values */
	long		lval[9];	/* convert space */
	bcont		*sval;
	int		nullvlu;
	c	= *lastc;
	ncntp	= cntp;
	vaddr	= ptr;
	nullvlu	= 0;

	while (ncntp > 0) {
		errn	= _nexdata(css, type, vaddr, ncntp, 1, c, cup, echoptr,
				lval, &lcount, elsize, &nullvlu);
		if (errn != 0)
			return(errn);
		else {
			if (nullvlu == 2) {
				lcount	= 0;
				ncntp	= 0;
			}
		}
		if (lcount > ncntp) {
			errn	= FENLTOOM;
			return(errn);
		}
		if (type == DVTYPE_ASCII) {
			char	*wptr;
			wptr	= vaddr;
			/* character data already stored, adjust
			 * ptr and count only.
			 */
			ncntp	= ncntp - lcount;
			wptr	= wptr + (lcount * elsize);
			vaddr	= wptr;
		}
		else {
			int move;
			int *iptr;
			int ix, lim;
			bcont *siptr;
			move	= MIN(ncntp,lcount);
			lim	= elsize/(sizeof(bcont));
			siptr	= (bcont*) vaddr;
			/* move what's needed from data group */
			while (move != 0) {
				sval	= (bcont*) lval;
				/* do not move null values */
				if (!nullvlu) {
				 	for (ix=0; ix < lim; ix++) {
						*siptr	= *sval;
						siptr++;
						sval++;
					}
				} else
					siptr	= siptr + lim;
				vaddr	= siptr;
				move--;
				ncntp--;
				lcount--;
			}
		}
		/* get separator following value */
		do {
			CMTSUBGT(*lastc);
		} while (ISBLANK(*lastc));
		/* if separator, get next nonblank character on the
		 * same line or on a new line.
		 */

		if (MATCH(*lastc, _MASKS, MRNLSEP)) {
			do {
				CMTSUBGT(*lastc);
			} while (ISBLANK(*lastc));
		}
	c	= *lastc;
	}
	return(0);
}

/*	_indx_nl	- compute the dimension information of an
 *			indexed array in the input record.
 *	On entry:
 *		_ positioned just after the '('
 *	On exit:
 *		- returns:	0 on success
 *				-value on eof
 *		- positioned just after the '='
 *		- the lastc argument is not changed
 */

static int
_indx_nl(
	FIOSPTR css, unit *cup, struct Echoinfo *echoptr,
	long *begcnt, int *ndima, long strbegend[3],
	int *encnt, int *icnt, int arryflag)
{
	long	*oldp, *newp;
	long	mode, ss;
	long	offs, mult;
	char	c;
	int	i, j, ir1, en1;
	long	dummy;
	int	errn = 0;
	long	stat;
	long	field_width;
	long	*field_begin;
	long	*field_end;
	long	tempbuf[2];
	en1	= 0;
	ir1	= 0;
	if (arryflag) {
		for (i = 0; i < MAXDIMS; ) {
			long	dummy;
			/* no comments in namelist input here and
			 * skip leading blanks here only.
			 */
			do {
				SUBGTC(c);
			} while (ISBLANK(c));

			/* Was end of subscripts reached in input record */
			if (c == ')')
				break;
			cup->ulinecnt++;
			cup->ulineptr--;

			/* Get the low_bound subscript information first */
			GETSECTION(c);
			if (field_width == 0)
				goto indxgetext;
			/* pass field_end + 1 */
			field_end++;
			tempbuf[0]	= 0;
			tempbuf[1]	= 0;
			mode		= 0;
			(void) _iu2s(field_begin, &field_width,
				&field_end, &mode, tempbuf, &stat,
				&dummy, &dummy);
			if(stat < 0) {
				errn	= FENLSUBS;
				return(errn);
			}
			begcnt[i]	= *((_f_int8 *)tempbuf);
indxgetext:
			/* point beyond subscript. */
			cup->ulineptr	= field_begin + field_width;
			cup->ulinecnt	= cup->ulinecnt - field_width;

			/* Get the extent subscript information */
			if (c == ':')
				return(FENLARSC);
			
			/* increment the number of subscripts */
			i++;
			do {
				SUBGTC(c);	/* get to ',' or ')' */
			} while (ISBLANK(c));	/* NO EOR here */
			/* check for end of subscripts */
			if (c == ')')
				break;
			if (c != ',') {
				errn	= FENLSUBD;	/* not a comma */
				return(errn);
			}
		}
		*ndima	= i;
		*encnt	= en1;
		*icnt	= ir1;
		if (i == 0) {
			errn	= FENLSUBN;	/* null index */
			return(errn);
		}
	}
	if (strbegend[0] == 0) {
		j	= 0;
		if (arryflag) {
			SUBGTC(c);
		} else
			c	= '('; 
		/* Check for substring information after array element */
		if (c == '(') {
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
			/* skip leading blanks in input here */
			do {
				SUBGTC(c);
			} while (ISBLANK(c));
			if (c == ')') {
				errn	= FENLSTRN;	/* null index */
				return(errn);
			}
			cup->ulinecnt++;
			cup->ulineptr--;
#endif
			GETSECTION(c);
			if (field_width == 0)
				goto indxstrend;
			/* pass field_end + 1 */
			field_end++;
			tempbuf[0]	= 0;
			tempbuf[1]	= 0;
			mode		= 0;
			(void) _iu2s(field_begin, &field_width, &field_end,
				&mode, tempbuf, &stat, &dummy, &dummy);
			if(stat < 0) {
				errn	= FENLSTRG;
				return(errn);
			}
			strbegend[1]	= *((_f_int8 *)tempbuf);
			j++;
indxstrend:
			/* point beyond colon. */
			cup->ulineptr	= field_begin + field_width;
			cup->ulinecnt	= cup->ulinecnt - field_width;
			if (c == ':') {
				/* update ulineptr */
				SUBGTC(c);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
				/* skip leading blanks in input here */
				do {
					SUBGTC(c);
				} while (ISBLANK(c) || (c == ':'));
				if (c == ')')
					goto indxstrout;
				cup->ulinecnt++;
				cup->ulineptr--;
#endif
				GETSECTION(c);
				if (field_width == 0)
					goto indxstrdon;
				/* pass field_end + 1 */
				field_end++;
				tempbuf[0]	= 0;
				tempbuf[1]	= 0;
				mode		= 0;
				(void) _iu2s(field_begin, &field_width,
					&field_end, &mode, tempbuf,
					&stat, &dummy, &dummy);
				if(stat < 0) {
					errn	= FENLSTRG;
					return(errn);
				}
				strbegend[2]	= *((_f_int8 *)tempbuf);
				j++;
indxstrdon:
				/* point to right paren? */
				cup->ulineptr	= field_begin + field_width;
				cup->ulinecnt	= cup->ulinecnt - field_width;
			}
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
			else {
				errn	= FENLSTRN;	/* null index */
				return(errn);
			}
#endif
indxstrout:
			strbegend[0]	= j;
		}
	}
	/*
	 * Look for the equal sign or the replacement character for that
	 * character
	 */
	while (!(MATCH(c, _MASKS, MRNLREP))) {
		SUBGTC(c);
	}
	return(errn);
}

/*
 * Set echo unit and Echo the line in input_ptr of length ncrs for cft77
 * with RNLECHO. 
 */

static void
_nlrdecho(
	unum_t		eunit,
	long		*input_ptr,
	long		ncrs,
	FIOSPTR css)
{
	long	stat;
	unit	*echoup;
	long	blk = BLANK;
	echoup	= _get_cup(eunit);	/* lock the unit */
	if (echoup == NULL) {
		unit	*cupsave;
		cupsave	= css->f_cu;	/* Save for _imp_open() */
		echoup	= _imp_open77(css, SEQ, FMT, eunit, 1, NULL);
		css->f_cu	= cupsave;
		if (echoup == NULL)	/* If OPEN failed */
			return;
	}
	else {
		if (echoup->ufmt == 0)		/* If unformatted file */
			_ferr(css, FEFMTTIV);
		if (echoup->useq == 0)		/* If direct access file */
			_ferr(css, FESEQTIV);
	}
	/*
	 * Output the blank that precedes the buffer for carriage control
	 * Add one to cup->ulinecnt, so that the preceding blank is counted.
	 */
	(void) _fwch(echoup, &blk, 1, PARTIAL);
	(void) _fwch(echoup, input_ptr, ncrs, FULL);
	(void) _release_cup(echoup);	/* unlock the unit */
	return;
}

/* _setunit - setup
 * Format the unit number or file name and copies to 'string'
 * for warning messages and echo of input lines for RNLECHO for cf77
 * compatibility.
 */

static void
_setunit(
	char	*string,
	void	*u)
{
	register unum_t	unum;

	if (_is_file_name(*((long *)u)))
		(void) strncpy(string, (char *)u, sizeof(long) - 1);
	else {
		unum	= *((unum_t *)u);
		(void) sprintf(string, "%lld", unum);
	}

	return;
}

static void
_pr_echomsg(char *string)
{
	(void) write(fileno(errfile), string, strlen(string));

	return;
}

/* Converts the string in buf to upper case letters */

static void
_cnvrt_toupper(char *buf)
{
	register char	c;

	while ((c = *buf) != '\0')
		*buf++	= toupper(c);

	return;
}

/*
 * ishol is only called by cf77 compatible entry
 * ENTRY: 	hlptr is a pointer to a possible Hollerith character
 * Returns:	0 if delimiter is not part of hollerith string
 *		1 if delimiter is part of hollerith string
 */

static int
_ishol(long *hlptr, unit *cup)
{
	char	hlval;

	hlval	= (char) *(hlptr - 1);
	if (isdigit(hlval) && ((hlval - '0') <= (sizeof(_f_int))) && ((hlval - '0') > 0)) {
		/*
	 	 * We have digit followed by Hollerith designator, check
	 	 * the preceding character.
		 */
		if (((hlval - '0') + hlptr) >= ((cup->ulineptr) - 1)) {
		 	/* Column 1 of ulinebuf[1] and is ignored */
			if (hlptr > &cup->ulinebuf[3]) {
				hlval	= (char) *(hlptr - 2);
			 	if (!ISBLANK(hlval) && hlval != '*' &&
				    !MATCH(hlval, _MASKS, MRNLREP) &&
				    !MATCH(hlval, _MASKS, MRNLSEP) )
					return(0);
			}
			return(1);
		}
		return(0);	/* Delimiter is beyond Hollerith string */
	}
	return(0);
}

/*	_nexdata - get the next data group - position at the first character
 *		following the value or values.
 *	On return, lval will contain the value and lcount the repeat count
 *	Outptr will point to character immediately following value
 *
 *	The return value is:	-value for EOF
 *				0 for ok
 *				>0 if an error
 *		nullvlu =	1 for null value read
 *				2 for null value, followed by possible
 *				  variable name
 */
static int
_nexdata(
	FIOSPTR		css,
	ftype_t 	type,	/* Type of data item */
	void		*ptr,	/* Address of data item */
	int		cnt,	/* Number of values to look for */
	int		inc,
	char		lastc,	/* First character of value, may be blank */
	unit		*cup,	/* Input unit */
	struct Echoinfo *echoptr,
	long		*lval,	/* Value is placed here */
	int		*lcount, /* Repeat count is returned here */
	int		elsize,
	int		*nullvlu) /* indicate if any nulls returned */
{
	char	c, oc;
	int	ocnt, ss;
	long	*optr;
	int	holcnt;		/* Length of hollerith string */
	long	stat;
	char	newc;
	int	errn;
	*nullvlu	= 0;
	c	= lastc;
	while (ISBLANK(c)) {
		CMTSUBGT(c);
	}
	*lcount	= 1;	/* set repeat count */
	if (isdigit((int) c)) {
		/* Look for repeat count.  We can have a repeat count
		 * for any type of data, including character.
		 */
		*lcount	= c - '0';
		ocnt	= cup->ulinecnt; /* save count and pointer, in case */
		optr	= cup->ulineptr; /* this isn't repeat count */
		oc	= c;
		for (;;) {
			if (cup->ulinecnt > 0) {
				SUBGTC(c); /* Ignore comments */
			} else
				break;
			if (isdigit((int) c))
				*lcount	= (*lcount * 10) + c - '0';
			else
				break;
		}
		/*
		 * Could have r*c, rH, rL, or rR, where r is the number just
		 * read.  No embedded blanks allowed in r*c, rH, rL, or rR.
		 */
		switch (c) {
		case '*':
			/* get next character with comment */
			CMTSUBGT(c);
			if (isdigit((int) c)) {
				/* See if we have a repeat count followed
				 * by hollerith, like 3*4Habcd
				 */
				holcnt	= c - '0';
				ocnt	= cup->ulinecnt;
				optr	= cup->ulineptr;
				oc	= c;
				for (;;) {
					SUBGTC(c);
					if (isdigit((int) c))
						holcnt	= (holcnt * 10) +
							c - '0';
					else
						break;
				}
				switch (c) {
				case 'H':
				case 'h':
				case 'R':
				case 'r':
				case 'L':
				case 'l':
					return(_get_holl(css, cup, c, holcnt,
						type, echoptr, lval, elsize));
				default:
					/* backup restore */
					cup->ulineptr	= optr;
					/* cnt and ptr */
					cup->ulinecnt	= ocnt;
					c	= oc;
					ocnt	= 1;
					break;
				} /* switch */
			}
			break;	/* Ordinary repeat count */
		case 'H':
		case 'h':
		case 'R':
		case 'r':
		case 'L':
		case 'l':
		/* Assume it is a Hollerith string, like 3Habc */
			holcnt	= *lcount;
			*lcount	= 1;	/* No repeats */
			return(_get_holl(css, cup, c, holcnt, type, echoptr,
				lval, elsize));
		default:
			/* No repeat count, backup restore, cnt & ptr */
			cup->ulineptr	= optr;
			cup->ulinecnt	= ocnt;
			c		= oc;
			ocnt		= 1;
			*lcount		= 1;
			break;
		} /* switch */
	}
	/* END of isdigit()
	 * Looking for a value.  When we get here we are at a nonblank
	 * character, unless we had the form r*, in which case it may
	 * be followed by a blank (NULL).
	 */
	if (MATCH(c, _MASKS, MRNLSEP)) {
		cup->ulineptr--; /* reset cnt and ptr so */
		cup->ulinecnt++; /* we can read separator again */
		*nullvlu	= 1;
		return(0);	/* return null value */
	}
	else if (ISBLANK(c)) {
		*nullvlu	= 1;
		return(0);	/* return null value */
	}
	else {
		if (MATCH(c, _MASKS, MRNLCOMM)) {
			/* Use this path with input like:
			 * A = 5*; 
			 */
			*lval	= *(lval+1)	= 0;
			/* reset cnt and ptr so rest in record is read as
			 * as null values
			 */
			cup->ulinecnt++;
			cup->ulineptr--;
			*nullvlu	= 1;
			return(0);	/* return null value */
		}
		else
			if (MATCH(c, _MASKS, MRNLDELIM) || (c == '/')) {
				/* treated terminating slash or ampersand
				 * the same for f90 to allow simpler
				 * non-f90 compatibility.
				 */
				cup->ulineptr--; /* reset cnt and ptr so */
				cup->ulinecnt++; /* read delimiter again */
				*nullvlu	= 2;
				return(0);	/* Return null value */
			}
	}
	/*
	 * It is important that we handle the special cases of types logical
	 * and character first, because the format of their data is treated
	 * differently.
	 */
	if (type == DVTYPE_LOGICAL) {
		bcont	*slval;
		slval	= (bcont*)lval;
		/* Looking for a logical value.  Logical values must be of
		 * the form: optional decimal point, followed by a 'T' for
		 * true or an 'F' for false, optionally followed by one
		 * or more additional characters.  Those additional
		 * characters cannot include '=', ',', ':', ';', '(', '$'
		 * or '&'.
		 */
		if (c == '.') {
			SUBGTC(c);
			/* .T or .t assumed to be a logical value */
			if ((c == 'T') || (c == 't')) {
				switch (elsize) {
#ifdef _F_REAL4
				case 4:
					*(_f_log4 *)slval	= _btol(1);
					break;
#endif
				case 8:
					*(_f_log8 *)slval	= _btol(1);
					break;
				default:
					return(FEKNTSUP); /* kind not supported */
				}

			/* F and .f are assumed to be a logical value */
			} else if ((c == 'F') || (c == 'f')) {
				switch (elsize) {
#ifdef _F_REAL4
				case 4:
					*(_f_log4 *)slval	= _btol(0);
					break;
#endif
				case 8:
					*(_f_log8 *)slval	= _btol(0);
					break;
				default:
					return(FEKNTSUP); /* kind not supported */
				}
			} else {
				errn	= FENLIVLG;	/* Invalid logical */
				return(errn);
			}
		}
		else {
			/* If the string does not start with a '.', it could
			 * be a logical value or a variable name.  Try to
			 * determine which by seeing if it is followed by a
			 * replacement character or '('.  Save count and
			 * pointer in case this isn't a value.
			 */
			ocnt	= cup->ulinecnt;
			optr	= cup->ulineptr;
			newc	= *optr++;
			ocnt--;
			while (!(ISBLANK(newc))) {
				if (MATCH(newc, _MASKS, MRNLSEP) ||
				    MATCH(newc, _MASKS, MRNLDELIM) ||
				   (newc == '/'))
					break; /* Assume value */
				if (MATCH(newc, _MASKS, MRNLREP) ||
				   (newc == '(')) {
					/* Reset, this MAY be the first
					 * letter of a variable name
					 */
					cup->ulineptr--;
					cup->ulinecnt++;
					*nullvlu	= 2;
					return(0); /* Null value */
				}
				newc	= *optr++;
				ocnt--;
			}
			while ((ISBLANK(newc)) && ocnt-- > 0)
				newc	= *optr++;
			if (MATCH(newc, _MASKS, MRNLREP)) {
				/*
				 * Reset, because this MAY have been
				 * the first letter of a variable name
				 */
				cup->ulineptr--;
				cup->ulinecnt++;
				*nullvlu	= 2;
				return(0);	/* Null value */
			}
			if ((c == 'T') || (c == 't')) {
				switch (elsize) {
#ifdef _F_REAL4
				case 4:
					*(_f_log4 *)slval	= _btol(1);
					break;
#endif
				case 8:
					*(_f_log8 *)slval	= _btol(1);
					break;
				default:
					return(FEKNTSUP); /* kind not supported */
				}
			}
			else if ((c == 'F') || (c == 'f')) {
				switch (elsize) {
#ifdef _F_REAL4
				case 4:
					*(_f_log4 *)slval	= _btol(0);
					break;
#endif
				case 8:
					*(_f_log8 *)slval	= _btol(0);
					break;
				default:
					return(FEKNTSUP); /* kind not supported */
				}
			}
			else if (MATCH(c, _MASKS, MRNLSEP) ||
				 ISBLANK(c) || (c == ',')) {
				*nullvlu	= 1;
				return(0);	/* Indicate null value */
			}
			else {
				errn	= FENLIVLG;	/* Invalid logical */
				return(errn);
			}
		}
		/* We assume we're reading a logical value.
		 * Skip to the end of this value.
		 */
		while ( !(ISBLANK(c))) {
			CMTSUBGT(c);
			/* check for separator or terminating character */
			if (MATCH(c, _MASKS, MRNLDELIM) || c == '/' ||
			    MATCH(c, _MASKS, MRNLSEP)) {
				/* Reset cnt and ptr for conversion routine */
				cup->ulineptr--;
				cup->ulinecnt++;
				return(0); /* return logical value */
			}
		}
		return(0);	/* return logical value */
	} /* End of type logical */
	/* if type character, read character data */
	if (type == DVTYPE_ASCII)
		return (_g_charstr(css, cup, ptr, cnt, c, echoptr, *lcount,
			elsize, nullvlu));
	/* Get value for variable that is not type LOGICAL or CHARACTER */
	if (isdigit((int) c) || c == '+' || c == '-' || c == '.') {
		if (type == DVTYPE_COMPLEX) {
			errn	= FENLIVCX;
			return(errn);
		}
		return(_g_number(type, cup, lval, elsize));
	}
	/* When we get here we are looking for a VALUE.  We are at a
	 * nonblank character which is not a digit, +, or -, separator,
	 * comment or delimiter.
	 * A left parenthesis indicates complex data
	 * An apostrophe or quote indicates hollerith data
	 * A letter o indicates octal data
	 * A letter z indicates hexadecimal data
	 */
	if (c == '(') {
		return(_g_complx(css, cup, type, echoptr, lval, elsize));
	}
	else if ((c == '\'') || (c == '"')) {
		return(_get_quoholl(css, cup, c, type, echoptr, lval, elsize));
	}
	else if (c == 'O' || c == 'o') {
		return(_gocthex(css, cup, type, echoptr, lval, OCTAL, elsize,
			nullvlu));
	}
	else if (c == 'Z' || c == 'z') {
		return(_gocthex(css, cup, type, echoptr, lval, HEX, elsize,
			nullvlu));
	}
	else {
		/* No valid value.
		 * Reset cup->ulineptr, because this MAY have been the first
		 * character of a variable name.  For example, if we have:
		 * integer var1(3),var2, with input: var1=2, var2 = 5
		 * then when we try to read the value for var1(2), we will
		 * see 'var2'
		 */
		cup->ulineptr--;
		cup->ulinecnt++;
		*nullvlu	= 2;
		return(0);	/* Return null value */
	}
}

/* _g_complx - get the value for a complex number.
 * On entry:
 *		positioned at '(' for a complex number.
 * Returns:	0 if OK,
 *		-value if EOF
 *		> 0 with valid error number if an error
 */

static int
_g_complx(
	FIOSPTR css, unit*cup, ftype_t type, struct Echoinfo *echoptr,
	long *lval, int	elsize)
{
	char	c;
	long	*oldp;
	long	mode, stat;
	long	zero = 0;
	long 	field_width;
	long	*field_begin;
	long	*field_end;
	int	ss, i, errn;
	int	nc;
	long	*lptr;
	ic_func *ngcf;
	int	inc;
	int	ptrfw;
	bcont	*slval;
	/*
	 * IN reading the complex number, assume
	 * intervening EOR is OK
	 */
	if (type != DVTYPE_COMPLEX) {
		errn	= FENLIVCX;	/* type not complex */
		return(errn);
	}
	/*
	 * Call the function from the ncf_tab77 table.
	 */

	ngcf	= ncf_tab77[type];
	mode	= 0;

	switch (elsize) {
#ifdef _F_REAL4
	case 8:
		mode	= MODEHP;
		break;
#endif
	case 16:
		break;
	case 32:
		mode	= MODEDP;
		break;
	default:
		return(FEKNTSUP);	/* kind not supported */
	}
	inc	= (elsize / 2) / (sizeof(bcont));
	slval	= (bcont*)lval;
	/*
	 * If the user had turned off blanks as separator, tell
	 * conversion to ignore them.  Otherwise, blanks are significant.
	 */
	if (_BLNKSEP == 0)
		mode   |= MODEBN;
	/* loop and get both real and imaginary */
	for (i = 0; i < 2; i++) {
		do {
			SUBGTC(c);	/* skip the '(' */
		} while (ISBLANK(c));	/* skip blanks */
		cup->ulinecnt++;	/* backup 1 character */
		cup->ulineptr--;	/* backup 1 character */
		field_begin	= cup->ulineptr;
		field_end	= cup->ulineptr;
		field_width	= cup->ulinecnt;
		nc		= 0;
		/* while not MRNLSEP (comma),
		 *	MRNLDELM (ampersand, dollarsign, or slash),
		 *	blank if a separator, or left parenthesis
		 */
		while (nc < cup->ulinecnt && !(*field_end == ')' ||
		   MATCH(*field_end, _MASKS, MRNLSEP) ||
		   MATCH(*field_end, _MASKS, MRNLDELIM) ||
		   (*field_end == '/') ||
		   (isspace(*field_end) && (_BLNKSEP != 0)) ) ) {
			field_end++;
			nc++;
		}
		/* pass field_end + 1 */
		field_end++;
		field_width	= nc;
		/* convert both the real and imaginary parts */
		errn	= ngcf(field_begin, &field_width, &field_end,
			&mode, slval + (i * inc), &stat, &zero, &zero);

		/* If the scan failed, the input data might be
		 * Hollerith or hex or octal.  Allow _s_scan_extensions
		 * _s_scan_extensions to rescan the input and
		 * recompute the field width.
		 */
		if (errn < 0) {
			errn	= _nicverr(stat);
		} else
			errn	= 0;

		/* if (errn == EX_ILLCHAR) */
		if (errn == FENICVIC) {
			int errn2;
			int new_elsize;
			ftype_t new_type;
			new_type	= DVTYPE_INTEGER;
			/* complex(kind=16) not allowed in cft77 */
			if (elsize == 32) {
				return(errn);
			}
			new_elsize	= elsize >> 1;
			/* store into float without conversion. */
			errn2	= _s_scan_extensions((slval + (i * inc)),
				new_type, new_elsize, field_begin,
				field_width, &ptrfw, mode);

			cup->ulineptr += ptrfw;
			cup->ulinecnt -= ptrfw;
			if (errn2 <= 0)
				errn	= 0;
			else
				/* errors FELDUNKI and FELDSTRL
				 * are currently returned.
				 */
				return(FENLIVCX);
		} else {
			cup->ulineptr	= field_begin + field_width;
			cup->ulinecnt  -= cup->ulineptr - field_begin;
			if (errn != 0)
				return(errn);
		}
		do {
			SUBGTC(c);
		} while (ISBLANK(c));
		if ((c != ',') && (i == 0))
			return(FENLIVCX); /* err in complex number format */
	}
	if ( c != ')')
		return(FENLIVCX); /* err in complex number format */
	return(0);
}

/*
 * _g_number - Read a number.
 * Returns:	0 if ok
 *		-value if EOF
 *		> 0 if error
 */

static int
_g_number(
	ftype_t 	type,
	unit		*cup,
	long		*lval,
	int		elsize)
{
	long	*oldp;
	long	mode, stat;
	long	zero = 0;
	long 	field_width;
	long	*field_begin;
	long	*field_end;
	long	*s_field_end;
	int	ss;
	int	errn = 0;
	int	nc;
	long	*lptr;
	ic_func *ngcf;
	int	ptrfw;
	bcont	*slval;

	mode	= 0;

	switch (type) {
	case DVTYPE_REAL:
		switch (elsize) {
#ifdef _F_REAL4
		case 4:
			mode	= MODEHP;
			break;
#endif
		case 8:
			break;
		case 16:
			mode	= MODEDP;
			break;
		default:
			return(FEKNTSUP);
		}
		break;
	case DVTYPE_INTEGER:
		switch (elsize) {
#ifdef _F_INT4
		case 4:
			mode	= MODEHP;
			break;
#endif
		case 8:
			break;
		default:
			return(FEKNTSUP);
		}
		break;
	}
	/*
	 * Call the function from the ncf_tab77 table.
	 */

	ngcf	= ncf_tab77[type];

	/*
	 * If the user had turned off blanks as separator, tell NICONV
	 * to ignore them.  Otherwise, blanks are significant.
	 */
	if (_BLNKSEP == 0)
		mode   |= MODEBN;
	cup->ulinecnt++;	/* backup 1 character */
	cup->ulineptr--;	/* backup 1 character */
	field_begin	= cup->ulineptr;
	field_end	= cup->ulineptr;
	field_width	= cup->ulinecnt;
	slval		= (bcont*)lval;
	nc		= 0;
	/* while not MRNLSEP (comma)
	 *	MRNLDELM (ampersand, dollarsign, or slash)
	 *	or blank if a separator
	 */
	while (nc < cup->ulinecnt &&
	   !(MATCH(*field_end, _MASKS, MRNLSEP) ||
	     MATCH(*field_end, _MASKS, MRNLDELIM) || (*field_end == '/') ||
	   (isspace(*field_end) && (_BLNKSEP != 0)) ) ) {
		field_end++;
		nc++;
	}
	/* pass field_end + 1 */
	field_end++;
	field_width	= nc;
	s_field_end	= field_end;
	errn	= ngcf(field_begin, &field_width, &field_end,
			&mode, slval, &stat, &zero, &zero);

	/* If the scan failed, the input data might be
	 * Hollerith or hex or octal.  Allow _s_scan_extensions
	 * _s_scan_extensions to rescan the input and
	 * recompute the field width.
	 */
	if (errn < 0) {
		errn	= _nicverr(stat);
	} else
		errn	= 0;

	/* if (errn == EX_ILLCHAR) */
	if (errn == FENICVIC) {
		int	errn2;
		switch (type) {
		case DVTYPE_REAL:
			{
			long cmode;
			int new_elsize;
			int new_inc = 0;
			ftype_t new_type;
			new_type	= DVTYPE_INTEGER;
			cmode		= mode;
			new_elsize	= elsize;
			if (elsize == 16) {
				new_elsize	= 8;
				cmode		= 0;
				new_inc		= new_elsize / (sizeof(bcont));
			}
			/* store into float without conversion. */
			errn2	= _s_scan_extensions((slval + new_inc),
				new_type, new_elsize, field_begin,
				field_width, &ptrfw, cmode);

			/* store zero in first part of real 16 */
			if ((elsize == 16) && (errn2 == 0))
				*(_f_int8 *)slval	= 0;
			if (errn2 >= 0)
				errn	= 0;
			else
				errn	= FENLUNKI;
			break;
			}
		case DVTYPE_INTEGER:
			errn2	= _s_scan_extensions(slval, type, elsize,
				field_begin, field_width, &ptrfw, mode);

			/* errors FELDUNKI and FELDSTRL are
			 * currently returned.
			 */
			if (errn2 >= 0) {
				errn	= 0;
			} else if (errn == FENICVIC) {
				errn2	= 0;
				ngcf	= ncf_tab77[DVTYPE_REAL];
				field_end	= s_field_end;
				errn2	= ngcf(field_begin, &field_width,
				   	&field_end, &mode, slval, &stat,
				   	&zero, &zero);
				if (errn2 < 0)
					errn	= FENLUNKI;
				else {
					errn	= 0;
					switch (errn2) {
#ifdef _F_REAL4
					case EX_REAL32:
						{
						_f_real4 val4;
						union {
							_f_int4		n;
							_f_real4	f;
						} uval32;
						if (!_TYP_CONV) {
							errn	= FENLIVIT;
							break;
						}
						uval32.n	= *(_f_int4 *)slval;
						val4		= uval32.f;
						*(_f_int4 *)slval = (_f_int4) val4;
						break;
						}
#endif
					case EX_REAL64:
						{
						_f_real8 val8;
						union {
							_f_int8		n;
							_f_real8	f;
						} uval64;
						if (!_TYP_CONV) {
							errn	= FENLIVIT;
							break;
						}
						uval64.n	= *(_f_int8 *)slval;
						val8	= uval64.f;
						*(_f_int8 *)slval =
						   (_f_int8) val8;
						break;
						}
#if _F_REAL16 == 1
					case EX_REAL128:
						{
						_f_real16 val16;
						_f_int8 *int8ptr;
						union {
							_f_int8		n[2];
							_f_real16	f;
						} uval128;
						if (!_TYP_CONV) {
							errn	= FENLIVIT;
							break;
						}
						int8ptr	= (_f_int8 *)slval;
						uval128.n[0]	= int8ptr[0];
						uval128.n[1]	= int8ptr[1];
						val16		= uval128.f;
						*(_f_int8 *)slval = (_f_int8) val16;
						break;
						}
#endif
					default:
						errn	= FENLUNKI;
					}
				}

			} else
				errn	= FENLUNKI;
			break;
		}
	}
	cup->ulineptr	= field_begin + field_width;
	cup->ulinecnt  -= cup->ulineptr - field_begin;
	return(errn);
}

/* _g_charstr - read a character string
 *
 * Input: cup_ulineptr will point one past the first character of the string.
 *	"c" will contain the first character of the string.
 * Returns:	0 if ok,
 *		-value if EOF
 *		> 0 if error
 */

static int
_g_charstr(
	FIOSPTR		css,
	unit		*cup,
	void		*p,	/* Address of variable being read */
	int		cnt,	/* Number of strings we expect to read */
	char		c,	/* First character of string. */
	struct Echoinfo *echoptr,
	int		lcount, /* Repeat count */
	int		elsize,
	int		*nullvlu)
{
	int	eos;		/* eos == -1 if end or beginning of string */
	int	i, ch;
	unsigned int	len77;
	char	*cp;
	long	stat;
	char	enddelim;
	char	c1;
	int	repcount;
	char	*cpold;
	int	ss;
	int	errn = 0;
	long	*optr;
	int	ocnt;
	void	*fchp;
	*nullvlu	= 0;
	/*
	 * Character data may be enclosed in apostrophes or quotation marks.
	 * Each apostrophe within a character constant delimited by
	 * apostrophes must be represented by 2 consecutive apostrophes
	 * without an intervening blank or end of record. The same holds
	 * true for quotation marks. Character constants may be continued
	 * from the end of one record to the beginning of the next record.
	 * The end of the record does not cause a blank or any other
	 * character to become part of the constant.
	 * Blank characters, separator characters, comment characters, and
	 * delimiter characters may appear in character constants.
	 *
	 * For cf77 only (F90 does not allow undelimited character on input):
	 * If the character constant has the following properties:
	 * 1. It does not contain blank characters,
	 *    separator characters, comment characters, left parenthesis
	 *    or delimiter characters.
	 * 2. It does not cross a record boundary,
	 * 3. the first nonblank character is not a quotation mark or
	 *    apostrophe,
	 * 4. the leading characters are not numeric followed by asterisk,
	 * 5. the leading characters are not numeric followed by R, H, or L
	 * then the enclosing apostrophes or quotation marks are not required
	 * and apostrophes or quotation marks within the character constant
	 * are not to be doubled.
	 *
	 * Let len be the length of the list item, and let w be the length
	 * of the character constant. If len is less than or equal to w,
	 * the leftmost len characters of the constant are transmitted to the
	 * variable. If len is greater than w, the constant is transmitted to
	 * the leftmost w characters of the variable and the remaining len-w
	 * characters of the list item are filled with blanks.
	 *
	 * f90 allows zero-length character and it uses one input data item
	 * from the input record.  It does not store the value to the
	 * the zero-sized character entity.  cf77 does not allow this feature.
	 */
	eos	= 0;
	fchp	= p;
	len77	= elsize;	/* Get character length */
	/* cf77 does not allow zero-length character entities */
	if (len77 != 0) {
		cp		= fchp;
		repcount	= MIN(lcount,cnt);
		/*
		 * If the first character is a quote or apostrophe, we expect
		 * that character to delimit the end of the string.
		 */
		if ((c == '\'') || (c == '"')) {
			enddelim	= c;
			/* find characters in string */
			for (i = 0; i < len77 && eos == 0; i++) {
				GETSTR77();
				if (eos == 0)
					*cp++	= ch;
			}
			if (eos == -1)
				i--;
			i	= len77 - i; /* If declared len > read len */
			if ( i > 0 )
				(void) memset(cp, BLANK, i);	/* blank fill */
			cp	= cp + i;
			while (eos != -1) {
				/*
				 * We didn't hit the end of the string yet.
				 * Search for it.
				 */
				GETSTR77();
			}
			while (--repcount) {
				/* We have a repeat count.
				 * cp will point to the next element.
				 * Copy len77 characters to the next
				 * element.
				 */
				cpold	= fchp;
				(void) memcpy(cp, cpold, len77);
				cp	= cp + len77;	/* Next element */
			}
		}
		else {
			/*
			 * We have a character string that's not surrounded
			 * by quotes (or apostrophes).  Read until we see a
			 * blank, separator, comment, or EOR (which looks
			 * like a blank to us).  Store as many of them as
			 * we have room for.  We cannot have a repeat count
			 * unless we're surrounded by quotes or apostrophes.
			 */
			if (lcount > 1) {
				return(FENLNOVL);	/* invalid char data */
			}
			/*
			 * Determine if this is a value or a variable name.
			 * Save count and pointer in case this isn't a value.
			 */
			ocnt	= cup->ulinecnt;
			optr	= cup->ulineptr;
			c1	= *optr++;
			ocnt--;

			while (!(ISBLANK(c1))) {
				/* check for separator or 
				 * terminating character
				 */
				if (MATCH(c1, _MASKS, MRNLSEP) ||
				    MATCH(c1, _MASKS, MRNLDELIM))
					break; /* Assume value */
				if (MATCH(c1, _MASKS, MRNLREP) ||
				   c1 == '(') {
					/* Reset, this MAY be the first
					 * letter of a variable name.
					 */
					cup->ulineptr--;
					cup->ulinecnt++;
					*nullvlu	= 2;
					return(0); /* Null value */
				}
				c1	= *optr++;
				ocnt--;
			}
			while ((ISBLANK(c1)) && ocnt-- > 0)
				c1	= *optr++;
			if (MATCH(c1, _MASKS, MRNLREP) || c1 == '(') {
				/*
				 * Reset, this MAY be the first letter
				 * of a variable name.
				 */
				cup->ulineptr--;
				cup->ulinecnt++;
				*nullvlu	= 2;
				return(0);	/* Null value */
			}
			i	= 0;
			c1	= c;
			while (!(ISBLANK(c1))) {
				if (i < len77) {
					*cp++	= c1;
					i++;
				}
				SUBGTC(c1);
				if (MATCH(c1, _MASKS, MRNLSEP) ||
				    MATCH(c1, _MASKS, MRNLCOMM)) {
					/* reset to handle next time */
					cup->ulineptr--;
					cup->ulinecnt++;
					break;
				}
			}
			/* If declared length > amount read, blank fill */
			i	= len77 - i;
			(void) memset(cp, BLANK, i);
			cp	= cp + i;
		}
	}
	else {
		/* cf77 does not have zero-length character entities */
		return(FENLIOER);
	}
	return(errn);
}

/* _get_holl - Read a hollerith string.
 *
 * Returns:	0 if a value was found,
 *		-value if EOF
 *		> 0 if an error occurred
 */

static int
_get_holl(
	FIOSPTR		css,
	unit		*cup,
	char		holltype,
	int		count,	/* Number of characters in string */
	ftype_t		type,	/* Type of data item */
	struct Echoinfo *echoptr,
	long		*lval,
	int		elsize)
{
	int	i;
	char	*holbufptr;
	char	c;
	long	stat;
	int	ss;
	int	errn = 0;
	int	fill;
	/*
	 * Read 'count' characters from the current word, packing them
	 * left justified into lval[0].
	 *
	 * Can't have hollerith input for DOUBLE, COMPLEX or CHARACTER data.
	 * Hollerith input is supported for compatibility with
	 * old versions of namelist.
	 *
	 * Because we don't allow CHARACTER data, we can make the
	 * simplifying assumption that we start on a word boundary.
	 * Also, we are going to assume that whatever we read in will
	 * need to fit in a word the size of a default integer.  Repeat
	 * counts are allowed. If it becomes necessary to allow hollerith
	 * strings of > the sizeof the number of characters in a default
	 * integer, some thought will need to be given as to how to
	 * handle repeat counts.
	 */
	if (type == DVTYPE_COMPLEX || type == DVTYPE_ASCII ||
	   ((type == DVTYPE_REAL) && elsize == sizeof(_f_real16))) {
		return(FENLUNKI);
	}
	if (count > elsize) {
		return(FENLIOER);
	}
	fill		= BLANK;
	holbufptr	= (char *)lval;
	if (holltype == 'R' || holltype == 'r') {
		/* right justified */
		fill		= NULLC;
		holbufptr	= holbufptr + (elsize - count);
	}
	else
		if (holltype == 'L' || holltype == 'l')
			fill	= NULLC;
	/* Last character in buffer is the EOR character,
	 * that's why we check for cup->ulinecnt > 1
	 */
	for (i = 0; i < count && (cup->ulinecnt > 1) ; i++) {
		SUBGTC(c); /* comment characters are not special
			    * within hollerith string */
		*holbufptr++	= c;
	}
	if (i == count) {
		/* Do we need to fill the last word? */
		if (holltype == 'R' || holltype == 'r') /* right justified? */
			holbufptr	= (char *)lval;
		(void) memset(holbufptr, fill, elsize - count);
	}
	else {
		/*
		 * We hit EOR before we read enough characters _or_ we had
		 * too many characters.
		 */
		return(FENLIOER);
	}
	return(errn);
}

/* _get_quoholl
 * Get a hollerith string that is surrounded by quotes or apostrophes
 * Legal syntax is '----'L, '----'R, or '----'H
 *
 * Returns:	0 if a value was found,
 *		-value if EOF
 *		> 0 if an error occurred
 */

static int
_get_quoholl(
	FIOSPTR		css,
	unit		*cup,
	char		cdelim, /* Quote or apostrophe (to end hollerith) */
	ftype_t		type,	/* Type of data */
	struct Echoinfo *echoptr,
	long		*lval,	/* Value is placed here */
	int		elsize)	/* size */
{
	int	numchar;	/* character counter */
	int	j;
	int	fill;		/* Fill character is either ' ' or '\0' */
	long	holbuf;		/* Data is stored here until we know whether
				   it is right or left justified. */
	char	*holbufptr;	/* pointer into holbuf */
	char	c;		/* Character read */
	long	stat;
	char	*lvalcharptr;	/* Pointer to value */
	int	ss;
	int	errn = 0;
	/*
	 * Can't have hollerith input for DOUBLE, COMPLEX or CHARACTER data.
	 * Hollerith input is supported for compatibility with
	 * old versions of namelist.
	 *
	 * Because we don't allow CHARACTER data, we can make the
	 * simplifying assumption that we start on a word boundary.
	 * Also, we are going to assume that whatever we read in will
	 * need to fit in one word. Repeat counts are allowed. If it
	 * becomes necessary to allow hollerith strings of greater than
	 * the number of characters in a default integer, some thought
	 * will need to be given as to how to handle repeat counts.
	 */
	if (type == DVTYPE_COMPLEX || type == DVTYPE_ASCII ||
	   (type == DVTYPE_REAL && elsize == sizeof(_f_real16))) {
		return(FENLUNKI);
	}
	lvalcharptr	= (char *)lval;
	holbufptr	= (char *) &holbuf;
	/* Do not allow quoted strings to be continued on another record. */
	numchar	= 0;
	for (;;) {
		SUBGTC(c);
		if (c == cdelim) {
			/* Allow Comment characters within quoted string */
			SUBGTC(c);
			if (c != cdelim)
				break;	/* That was the end of the quoted
					 * string.  Otherwise, we saw two
					 * quotes in a row, which means
					 * we store one.
					 */
		}
		if (++numchar > elsize) {
			return(FENLIOER);
		}
		*holbufptr++	= c;	/* Save the character */
		/*
		 * Last character in the input buffer is the EOR character, 
		 * that's why we check for cup->ulinecnt <= 1
		 */
		if (cup->ulinecnt <= 1) {
			return(FENLIOER);
		}
	} /* On exit from this loop, numchar = number of chars. stored */
	if (c == 'L' || c == 'l')
		fill	= NULLC;
	else if (c == 'R' || c == 'r') {
		/* Right justify and store the value just read */
		holbufptr	= holbufptr - 1;	/* Last character */
		lvalcharptr	= lvalcharptr + (elsize - 1);
		j		= elsize - numchar;
		while (numchar-- > 0)
			*lvalcharptr--	= *holbufptr--;

		/* Fill word with 0's if necessary */
		while (j-- > 0)
			*lvalcharptr--	= '\0';
		return(0);
	}
	else {
		/* H format */
		fill	= BLANK;
		if (c != 'H' && c != 'h') {
			/* Reset pointers since the character does */
			/* not belong to this value */
			cup->ulineptr--;
			cup->ulinecnt++;
		}
	}
	/* Do we need to fill the last word? */
	(void) memset(holbufptr, fill, elsize - numchar);
	*lval	= holbuf;
	return(errn);
}

/* _gocthex - provides octal or hex editing for compatibility with old
 * versions of namelist.
 *	Legal formats: O'123 or O'123'. Octal number may not contain blanks,
 *	and this is a difference with the old version of namelist.
 *	Legal formats: Z'1a3 or Z'1a3'.
 *
 * On input:
 * 	cup_ulineptr should point to the character immediately following the O
 * Returns:	0 if a value was found,
 *		-value if EOF
 *		> 0 if an error occurred
 * nullvlu =	1 if a null value was found
 *		2 if a null value was found, and it is not followed
 *		  by another value
 */

static int
_gocthex(
	FIOSPTR 	css,
	unit		*cup,
	ftype_t		type,
	struct Echoinfo *echoptr,
	long		*lval,
	int		base,
	int		elsize,
	int		*nullvlu)
{
	char	c;
	long	stat;
	char	strbuf[2];
	int	ss;
	int	errn = 0;
	int	octshift = OCTSHFT;
	int	hexshift = HEXSHFT;
	/* check size in bytes of incoming variable. */
#if defined(_F_REAL4) && defined(_F_INT4)
	if (elsize <= 4) {
		octshift	= OCTSHFT4;
		hexshift	= HEXSHFT4;
	}
#endif
	*nullvlu	= 0;
	if (*cup->ulineptr != '\'') {
		/* Can't be a value, might be a variable name */
		cup->ulineptr--;
		cup->ulinecnt++;
		*nullvlu	= 2;
		return(0);	/* NULL value */
	}
	/* This type of format won't work for complex or double precision */
	if (type == DVTYPE_COMPLEX || (type == DVTYPE_REAL &&
		elsize == sizeof(_f_real16))) {
		return(FENLUNKI);	/* type mismatch */
	}
	SUBGTC(c);	/* Skip the apostrophe */
	SUBGTC(c);	/* and get the next character */
	*lval		= 0;
	strbuf[1]	= '\0';
	while (!(ISBLANK(c)) && c != '\'') {
		if (base == OCTAL) {
			if ((!isdigit((int) c)) || (c == '9') ||
				(*lval >> octshift)) {
					return(FENICVIC); /* NICV type err */
			}
			*lval	= (*lval * (sizeof(_f_int))) + c - '0';
		}
		else { /* Check for hex digit or overflow */
			if ((!isxdigit(c)) || (*lval >> hexshift)) {
				return(FENICVIC);	/* NICV type err */
			}
			strbuf[0]	= c;
			*lval	= (*lval * 16) +
				(int) strtol(strbuf, (char **)NULL, 16);
		}
		CMTSUBGT(c); /* Check for comment after value */
		if (MATCH(c, _MASKS, MRNLSEP)) {
			cup->ulineptr--;
			cup->ulinecnt++; /* to read separator after */
			break;	/* return from this routine */
		}
	}
	return(errn);	/* indicate value */
}
