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



#pragma ident "@(#) libf/fio/rnly.c	92.1	06/21/99 10:37:55"

#include <stdio.h>
#include <errno.h>
#include <liberrno.h>
#include <ctype.h>
#include <foreign.h>
#include <fortran.h>	/* for _fcdlen() */
#include <memory.h>
#include <values.h>
#include <malloc.h>
#include <cray/fmtconv.h>
#include "fio.h"
#include "fmt.h"
#include "rnl.h"

#define	BLANK	((int) ' ')
#define	NULLC	((int) '\0')

#define	OCTAL	1		/* reading octal input */
#define	HEX	2		/* reading hex input */
#define	SKIPMSG	" - Skipped record named: "
#define	UNITSTR	" On Unit: "

struct Echoinfo	{
	unum_t	eunit;	/* unit for echoing input */
	int	rnlecho;/* 1 if we always echo, 0 if we echo only if 'E' is
			 * in the first column */
};

static void	_rnlecho(unum_t _Un, struct Inpinfo *_Ip);

static int	_rnl_fillrec(unit *_Cu, struct Echoinfo	*_Ec, struct Inpinfo
			*_Ip);

static void	fmt_unit(char *_Str, void *_U);

static int	g_charstr(long _P, int _Cn, char _C, unit *_Cu, struct
			Echoinfo *_Ec, struct Inpinfo *_Ip, int _Lc,
			int _Sz);

static int	g_complx(int _Ty, unit *_Cu, struct Echoinfo *_Ec, struct
			Inpinfo *_Ip, long *_Lv);

static int	g_number(int _Ty, unit *_Cu, long *_Lv, struct Inpinfo *_Ip);

static int	g_octhex(int _Ty, unit *_Cu, struct Echoinfo *_Ec, struct
			Inpinfo *_Ip, long *_Lv, int _Base);

static int	get_holl(char _Ho, int _Cn, int _Ty, unit *_Cu, struct
			Echoinfo *_Ec, struct Inpinfo *_Ip, long *_Lv);

static int	get_quoholl(char _Cd, int _Ty, unit *_Cu, struct
			Echoinfo *_Ec, struct Inpinfo *_Ip, long *_Lv);

static int	isholl(long *_Hp, struct Inpinfo *_Ip);

static int	l_convert(long *_Val, int _Ty, long _Stat);

static Nlentry	*n_findn(char *_Key, Nlentry *_List);

static int	n_getn(char *_S, char *_Lc, unit *_Cu, struct Echoinfo *_Ec,
			struct Inpinfo *_Ip);

static int	n_getv(Nlentry *_Nlent, char *_Lc, Namelist *_Nlbase,
			unit *_Cu, struct Echoinfo *_Ec, struct Inpinfo *_Ip);

static int	n_indx(int *_Of, Nlentry *_Nlent, Namelist *_Nlbase,
			unit *_Cu, struct Echoinfo *_Ec, struct Inpinfo *_Ip);

static int	nex_data(int _Ty, long _Pt, int _Cn, int _In, char _La,
			unit *_Cu, struct Echoinfo *_Ec, struct Inpinfo *_Ip,
			long *_Lv, int *_Lc, int _Sz);

static int	nl_read(long _P, int _Cn, int _In, int _Ty, char *_Lc,
			unit *_Cu, struct Echoinfo *_Ec, struct Inpinfo *_Ip,
			int _Sz);

static void	pr_msg(char *_Str);

static void	to_upper(char *_Str);

/*
 * TONICV is an interface to NICONV used when reading values. 
 */

#define TONICV(value) {					\
	inptr->inptr--;					\
	inptr->incnt++;	/* backup 1 character */	\
	oldp	= inptr->inptr;				\
	(void) NICONV(oldp, &zero, &zero, &zero, &mode,	\
		value, &inptr->inptr, &stat);		\
	inptr->incnt	-= inptr->inptr - oldp;		\
}

/*
 * Use GETSTR to read a character string surrounded by
 * quotes or apostrophes. Comment characters are not
 * recognized as such inside a quoted string, so LGET is used.
 */ 
#define GETSTR() { \
	if (inptr->incnt == 1) {	\
		LGET(ch); /* skip the blank put in at EOR */\
	}				\
	LGET(ch);			\
	if (ch == enddelim) {		\
		eos	= -1; /* end of string */\
		LGET(ch);/* unless the string delimiter is doubled */\
		if (ch == enddelim)	\
			eos	= 0;	\
		else {			\
			inptr->inptr--;	\
			inptr->incnt++;	\
		}			\
	}				\
}
 

static int	zero	= 0;		/* for TONICV */


/*
 * @RNL - read namelist external
 *
 * @RNL
 *	read a record into the record buffer
 *	find namelist delimiter
 *	read namelist name
 *	if (not correct namelist name)
 *		skip the namelist record
 *	Until we come to a '&'
 *		read variable name
 *		find the matching variable descriptor
 *		get the value(s) for the variable/array
 *
 * Returns:	0 for success
 *		1 for error
 *		2 for endfile
 *
 *		in both s1 and s3 (for cft77).
 *
 * end @RNL
 */

@RNL(
	_f_int		*unump,	/* Unit number */
	Namelist	*nl,	/* Namelist structure */
	int		errf,	/* Nonzero if ERR specified */
	int		endf	/* Nonzero if END specified */
)
{
	unum_t	unum;
	int	errn;
	int	i;
	int	ss;
	long	stat;
	long	*hlptr;
	Nlentry *nlent;
	char	buf[MAXNAML + 1], c;
	char	skipmsg[sizeof(SKIPMSG) + sizeof(UNITSTR) + MAXNAML + 8 + 2];
	char	tmpbuf[MXUNITSZ];	/* Unit number buffer for warning messages */
	unit	*cup;
	FIOSPTR	css;
	struct Echoinfo	echoinfo;
	struct Echoinfo	*echoptr;
	struct Inpinfo	ininfo;
	struct Inpinfo	*inptr;


	echoptr	= &echoinfo;
	unum	= *unump;

	GET_FIOS_PTR(css);
	STMT_BEGIN(unum, 0, T_RNL, NULL, css, cup);

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

	cup->uflag	= (errf != 0 ? _UERRF : 0) | (endf != 0 ? _UENDF : 0);
	cup->uwrt	= 0;		/* Clear write flag */

	if (cup->useq == 0)	/* If direct access file */
		RERR(css, FESEQTIV);	/* Sequential attempted on direct access */

	if (!cup->ufmt)		/* If unformatted file */ 
		RERR(css, FEFMTTIV);	/* Formatted attempted on unform.*/

#if	DEBUG
	{
	int	i;
	Nlentry	*nz;

	printf("\n@RNL: ENTER \n");
	printf(" group %s\n", nl->nlname);

	nz	= nl->nlvnames;

	for (i = 0; i < 50; i++, nz++) {
		if (!nz->varname[0])
			break;
		printf("\n %s: \n", nz->varname);
		printf("type:%d (%d) nels:%d ndims:%d taskcm:%d lmf:%d \n",
			nz->na.type, _old_namelist_to_f77_type_cnvt[nz->na.type],
			nz->na.nels, nz->na.ndims, nz->na.taskcm, nz->na.lmf);
		printf("stkf:%d offdim:%o \n", nz->na.stkf, nz->na.offdim);
#ifdef _ADDR64
 		printf("varaddr:%o\n", nz->va.varaddr);
#else
 		printf("lmaddr:%o varaddr:%o\n", nz->va.lmaddr,
			nz->va.varaddr);
#endif
		if (_old_namelist_to_f77_type_cnvt[nz->na.type] == DT_CHAR) {
			_fcd	*kaddr;
			printf("Character variable!,");
			kaddr	= (_fcd *) (nz->va.varaddr + (long)nl);
			printf("length = %d, waddr = %o, charptr = %o\n",
				_fcdlen(*kaddr), kaddr, _fcdtocp(*kaddr));
		}
	}
	}
#endif

	inptr		= &ininfo;	/* Set up input buffer, pointers */
	inptr->inbuff	= cup->ulinebuf;
	inptr->inbuff[0]= (long) ' ';	/* Carriage control when echoing */
	inptr->incnt	= 0;
	inptr->inptr	= inptr->inbuff + 1;
	inptr->instart	= inptr->inptr;

	(void) strcpy(skipmsg, SKIPMSG);

	/* Set up the unit used for echoing input lines */ 

	if (_OUT_UNIT < 0) {
		echoinfo.eunit	= 101;	/* default = stdout */
		echoinfo.rnlecho	= 0;	/* no echoing until 'E' in col 1 */
	}
	else {
		echoinfo.eunit	= _OUT_UNIT;
 		echoinfo.rnlecho	= 1;	/* echo regardless of flag in col 1 */ 
	}

	if ((cup->uaction & OS_READ) == 0) {
		RERR(css, FENOREAD); 	/* No read permission */
	}

	if (cup->uwrt) {
		RERR(css, FERDAFWR);		/* Read after write */
	}

fill:
	ss	= _rnl_fillrec(cup, &echoinfo, inptr);

	if (ss != 0)
		goto err_eof;

fill1:
	do {
		MAINCMNTLGET(c)
	} while (ISSPTB(c));

	if (!MATCH(c, _MASKS, MRNLDELIM))
		goto fill;	/* Assume a comment statement */

	MAINLGET(c);

	ss	= n_getn(buf, &c, cup, &echoinfo, inptr);

	if (ss != 0)
		goto err_eof;

	to_upper(buf);

	if (strcmp(nl->nlname, buf)) {	/* group name unmatched */
		if (_SKP_MESS > 0) {

			/* Skip the record and issue a logfile message */

			(void) strcpy(&skipmsg[sizeof(SKIPMSG)-1], buf);
			(void) strcat(skipmsg, UNITSTR);
			fmt_unit(tmpbuf, unump);
			/*
			 * The following truncates the file name/unit number
			 * to seven characters, which will result in a loss
			 * of information when the unit number is larger than
			 * 9,999,999.
			 */
			(void) strncat(skipmsg, tmpbuf, sizeof(long) - 1);
			(void) strcat(skipmsg, "\n");
			pr_msg(skipmsg);
		}
		else if (_SKP_MESS < 0) {
			/* Aborts the job or goes to the optional ERR= branch */
			RERR(css, FENLIVGN);
		}
del_look:
		/* Read until we find a delimiter */

		while (!MATCH(c, _MASKS, MRNLDELIM) && c!= '/') {

			if (c == '\'' || c == '"') {
				char	qchar;

				qchar	= c;
rquote:
				do {
					MAINLGET(c);
				} while (c != qchar);

				MAINLGET(c);	/* See if it's a double quote */

				if (c == qchar)
					goto rquote;
			}
			else {
				MAINCMNTLGET(c);
			}
		}

		/*
		 * Try to determine whether this delimiter is part of a
		 * Hollerith string by looking back in the line.  If it
		 * is part of a Hollerith string, it's not really an
		 * end delimiter.
		 */

		hlptr	= inptr->inptr - 2;

		/*
		 * Search for nH, nh, nl, nL, nr, nR where n is a digit.
		 * Only look back 8 characters or to the beginning of 
		 * this line of input 
		 */

		for (i = 0; i < 8 && hlptr > &inptr->inbuff[2]; i++, hlptr--) {
			switch((char) *hlptr) {
				case 'h':
				case 'H':
				case 'l':
				case 'L':
				case 'r':
				case 'R':
					if (isholl(hlptr, inptr)) {
						MAINCMNTLGET(c);
						goto del_look;
					}
					break;

				default:
					break;
			}	/* switch */
		}
		goto fill1;
	}

	/*
	 * 	Have found the correct namelist group.
	 *	Process the input record. Read until we 
	 * 	see trailing delimiter.
	 */
	while (!MATCH(c, _MASKS, MRNLDELIM) && (c != '/')) {
		int	sepcnt;

		ss	= n_getn(buf, &c, cup, &echoinfo, inptr);

		if (ss != 0)
			goto err_eof;

		to_upper(buf);	

		if (!(nlent = n_findn(buf, nl->nlvnames)))
			if (strlen(buf) > 0) {
				RERR2(css, FENLNREC, buf);	/* variable not found */
			}
			else {
				Nreturn(IO_OKAY); /* empty variable entry */
			}

		/* we're positioned just after the var/array name */

		/* get value(s) */

		ss	= n_getv(nlent, &c, nl, cup, &echoinfo, inptr);

		if (ss != 0)
			goto err_eof;

		sepcnt	= 0;

		for ( ; ; ) {

			if (!(ISSPTB(c))) {

				if ((MATCH(c, _MASKS, MRNLSEP)) &&
				    (sepcnt == 0))
					sepcnt++; /* skip 1 separator */
				else
					break;
			}

			MAINCMNTLGET(c);
		}
	}

ret:		/* Return to user */

	STMT_END(cup, T_RNL, NULL, css);	/* unlock the unit */

	return(CFT77_RETVAL(ss));

err_eof:	/* Handle EOF or error */

	if (ss == EOF) {
		NEND(css, FERDNLEF);	
	}
	else {
		if (errno == FENLTYPI) {
			RERR3(css, errno, nlent->varname,
				_f77_type_name[_old_namelist_to_f77_type_cnvt[nlent->na.type]]);
		}
		else
			RERR(css, errno);
	}

	goto ret;
}

/*
 *	n_getn - Get variable name or group name
 *
 *	On entry: 
 *		- we're positioned to name possibly preceeded by blanks
 *		
 *	On exit:
 *		- we return 0 if success
 *			EOF if end of file read
 *			RNL_ERROR if other error (errno will be set)
 *		- we're positioned just after the name.
 *		- *lastc contains the last character read.
 *
 *	In looking for the name, we stop when we see a
 *	space, replacement character ('='), or '(', or delimiter ('&')
 */

static int
n_getn(
	char		*s,
	char		*lastc,
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr
)
{
	char	*p, c;
	int	n;
	int	ss;

	n	= MAXNAML;
	p	= s;
	c	= *lastc;

	while (ISSPTB(c))
		CMNTLGET(c);

	/*
	 * Names can never have embedded blanks.
	 * A comment can immediately follow the name
	 * and will terminate it.
	 */

	while (!(ISSPTB(c)) && c != '(' && !(MATCH(c, _MASKS, MRNLREP)) &&
		!(MATCH(c, _MASKS, MRNLDELIM))) {

		*p++	= c;

		CMNTLGET(c);		

		if (n-- == 0) {
			RNLERROR(FENLLONG);	/* name too long */
		}
	}

	*lastc	= c;
	*p	= '\0';

	return (0);
}

/*
 * n_findn - find variable name in list of Nlentrys
 * 
 * Returns: 
 *	pointer to matching variable descriptor or
 *	NULL if variable name was not found.
 */

static Nlentry
*n_findn(
	char	*key,	/* Pointer to variable name we're searching for */
	Nlentry	*list
)
{
	while (strlen(list->varname) > 0) {
		if (!strcmp(key, list->varname))
			return (list);
		else
			list++;
	}

	return (NULL);
}

/* n_getv - get values for namelist io
 *
 * n_getv uses nl_read to do all the dirty work
 *
 * On entry:
 *	- positioned just after the variable/array name
 *	- lastc contains the character following variable/array name
 *
 * On exit:
 *	- *lastc contains the character following the value
 *	- inptr is pointing to the character following that
 *	- returns
 *		 0 if success
 *		-1 if EOF detected
 *		valid error number if error detected
 */

static int
n_getv(
	Nlentry		*nlent,
	char		*lastc,	
	Namelist	*nlbase,
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr
)
{
	long	ss, cnt;
	long	stat;
	int	offset, size, ret;
	char	*cp;
	long	vaddr;

	/*	
	 * find the offset in the case of an indexed array 
	 */
	if (*lastc == '(') {

		ret	= n_indx(&offset, nlent, nlbase, cup, echoptr, inptr);

		if (ret != 0)
			return(ret);
	}
	else {				/* get to the '=' */
		offset	= 0;

		while (ISSPTB(*lastc)) {
			CMNTLGET(*lastc);
		}

		if (!(MATCH(*lastc, _MASKS, MRNLREP))) { /* match '=' */
			RNLERROR(FENLNOVL);
		}
	}
	
	/* Now we're positioned after the '=' */

/*
 * Compute:
 *	cnt	number of array elements to be read (1 if not an array).
 *	size 	size of the variable or array element (words for nonchar,
 *		bytes for char).
 * 	vaddr	the pointer to common memory where data is to be transferred.
 *		For type DT_CHAR, this is a Fortran character descriptor.
 */

	if (nlent->na.offdim == 0) 	/* variable */
		cnt	= 1;
	else		/* complete array, adjusted for any dimension */
 		cnt	= nlent->na.nels - offset;

	if (_old_namelist_to_f77_type_cnvt[nlent->na.type] == DT_CHAR) {
		_fcd	f;

		/* get Fortran character descriptor */
		f	= *(_fcd *) ((unsigned long) nlent->va.varaddr +
				(long) nlbase);
		/* get character element length */
		size	= _fcdlen(f);
		/* calculate character address as c pointer */
		cp	= _fcdtocp(f) + (offset * size);
		f	= _cptofcd(cp, size);
		/* pass character pointer and element size as args */
		vaddr	= (long) cp;
	}
	else {
		size	= _f77_type_len[_old_namelist_to_f77_type_cnvt[nlent->na.type]] >> 3;
		vaddr	= (long)nlent->va.varaddr + offset * size;
	}

	ss	= nl_read(vaddr, cnt, 1, _old_namelist_to_f77_type_cnvt[nlent->na.type],
			lastc, cup, echoptr, inptr, size);

	return(ss);
}

/*	n_indx - calculate the offset of the indexed array
 *	
 *	On entry:
 *		- positioned just after the '('
 *
 *	On exit:
 *		- returns:
 *			0 on success
 *			-1 on eof
 *			RNL_ERROR on error (errno is set)
 *		- positioned just after the '='
 *		- the "lastc" argument is not changed
 *
 */

static int
n_indx(
	int		*offset,
	Nlentry		*nlent,
	Namelist	*nlbase,
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr
)
{
	long	subs[MAXDIMS];		/* subscripts */
	long	*oldp, *newp;
	long	mode, ss;
	long	offs, mult;
	char	c;
	Dims	*p;
	int	i, j;
	long	stat;	
	long	vaddr;

	/* Get the indices */

	mode	= 0;
			
	for (i = 0; i < MAXDIMS; ) {
		long	dummy;

		do {
			LGET(c);	/* Not OK for comments here */
		} while (ISSPTB(c));

		if (c == ')')
			break;

		inptr->incnt++;
		inptr->inptr--;

		/* Get the subscript */

		oldp	= inptr->inptr;


		for (j = 0; j < inptr->incnt; j++) {

			c	= (char) oldp[j];

			if (c == ')' || c == ',')
				break;
		}

		newp	= oldp + j;

		(void) _iu2s(oldp, &inptr->incnt, &newp, &mode, &subs[i],
				&stat, &dummy, &dummy);

		if (stat < 0) {
			RNLERROR(FENLBNDY);	/* is there a better error? */
		}

#if	defined(_CRAY1) || defined(_WORD32)
		if (stat != EX_INTS) {
			RNLERROR(FENLBNDY);
		}
#endif


		inptr->inptr	= newp;
		inptr->incnt	= inptr->incnt - (newp - oldp);

		i++;			/* increment the number of subscripts */

		do {
			LGET(c); 	/* get to ',' or ')' */
		} while (ISSPTB(c));	/* NOT OK to have an EOR here */

		if (c == ')')
			break;

		if (c != ',') {
			RNLERROR(FENLIOER);	/* bad character */
		}
	}

	if (i == 0) {	
		RNLERROR(FENLIOER);		/* null index */
	}

	while (!(MATCH(c, _MASKS, MRNLREP))) {	/* Look for the replacement */
		LGET(c);
	}

	/*
	 *	compute the offset of the array element
	 */

	p	= (Dims *)(nlent->na.offdim + (long)nlbase);
	mult	= 1;

	offs	= subs[0] - p[0].lower;

	/*
	 * for example: a three dimension array in Fortran column major format 
	 * offs	= span[0] * span[1] * (sub[2] - p[2].lower)
	 *	  span[0] * (sub[1] - p[1].lower)
	 *	  (sub[0] - p[0].lower)
	 */

	/*
	 * Check that we did not read in more dimensions than 
	 * we should have.
	 */

	if (i > nlent->na.ndims) {
		RNLERROR(FENLBNDY);
	}

	for (j = 1; j < i; j++) {
		mult	= mult * p[j-1].span;
		offs	= offs + ((subs[j] - p[j].lower) * mult);
	}

	/* Check that the dimension read is not too large */

	if (offs >= nlent->na.nels) {
		RNLERROR(FENLBNDY);
	}

	*offset	= offs;

	return(0);
}

/*
 * Echo the line in inptr->inbuff.
 */

static void
_rnlecho(
	unum_t		eunit,	/* Unit for echoing */
	struct Inpinfo	*inptr
)
{
	unit	*echoup;
	FIOSPTR css;
	GET_FIOS_PTR(css);

	echoup	= _get_cup(eunit);		/* lock the unit */

	if (echoup == NULL) {
		unit	*cupsave;

		cupsave		= css->f_cu;	/* Save for _imp_open() */
		echoup		= _imp_open77(css, SEQ, FMT, eunit, 1, NULL);
		css->f_cu	= cupsave;

		if (echoup == NULL)	/* If OPEN failed */
			return;
	}
	else {
	
		if (echoup->ufmt == 0) 		/* If unformatted file */
			_ferr(css, FEFMTTIV);

		if (echoup->useq == 0)		/* If direct access file */
			_ferr(css, FESEQTIV);
	}

	/*
	 * Output the blank that precedes the buffer for carriage control
	 * Add one to incnt, so that the preceding blank is counted.
	 */

	(void) _fwch(echoup, inptr->inbuff, inptr->incnt + 1, FULL);

	(void) _release_cup(echoup);		/* unlock the unit */

	return;
}

/*
 * Formats the unit number or file name and copies to 'string'.
 */

static void
fmt_unit(
	char	*string,
	void	*u
)
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

/*
 * Converts the string in buf to upper case letters
 */

static void
to_upper(char *buf)
{
	char	c;

	while ((c = *buf) != '\0') {
		*buf++	= toupper(c);
	}

	return;
}

/*
 * nl_read is used to read and store values for the data item.
 * 
 * On input, inptr points to the character immediately following the '='
 * 
 * On exit, lastc will contain the first nonblank, nonseparator character
 * following the value.
 */

static int
nl_read(
	long		ptr,		/* Address of the data item */
	int		count,		/* Number of values to read */
	int		inc,		/* Always 1 on input */
	int		type,		/* Type of the data item */
	char		*lastc,		/* On exit, lastc contains the first
					   nonblank, nonseparator character
					   following the value */
	unit		*cup,		/* Pointer to unit table */
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr,
	int		elsize		/* declared size of char item */
)
{
	int	ss;
	int	cntp;
	int	nullvlu;	/* Indicates whether null values were found */
	char	*cp;
	long	stat;
	long	lval[3];	/* NICONV requires an extra word here */
	int	lcount;		/* Repeat count for values */

	if ((type < 0) || (type >= DT_MAX))
		RNLERROR(FEINTDTY);	/* type error */

	if (type == DT_CMPLX || type == DT_DBLE)
		inc	= inc + inc;

	CMNTLGET(*lastc);	/* Get the first character */

	lcount	= 0;	/* Repeat count */
	cntp	= count;

	while (cntp > 0) {

		if (cup->uend)
			return(EOF);	

		/* get next data group */

		nullvlu	= nex_data(type, ptr, cntp, inc, *lastc, cup, echoptr,
					inptr, lval, &lcount, elsize);

		if (nullvlu == RNL_ERROR) {
			return(RNL_ERROR);
		}
		else if (nullvlu == 2) { /* No more values for this variable */
				lcount	= 0;
				cntp	= 0;
		}

		if (type == DT_CHAR) {
			/*
			 * Character data is already in place.
			 * Adjust ptr and cntp.
			 */

			if (lcount > cntp)
				RNLERROR(FENLTOOM);	/* too many elements specified */
			/* ptr is a c pointer.  When the data type is
			 * character, the declared length is passed in
			 * argument size.  An fcd is not passed.
			 */
			cp	= (char *) ptr;
			cntp	= cntp - lcount;
			
			cp	= cp + (lcount * elsize);
			ptr	= (long) cp;

		}
			
		else {
			int	move;

			move	= MIN(cntp, lcount);

			/* Move what's needed from data group */

			while (move != 0) {
				if (!nullvlu) { /* move data in, unless nulls */
					*(long *)ptr	= lval[0];
					if ((type == DT_DBLE) ||
					    (type == DT_CMPLX))
						*((long *)ptr+1) = lval[1]; 
				}

				ptr	= ptr + inc;
				move	= move - 1;
				cntp	= cntp - 1;
				lcount	= lcount - 1;
			}

			if (lcount)
				RNLERROR(FENLTOOM);	/* too many elements specified */
		}

		/*
		 * nex_data() will have positioned us at the first character
		 * following the value.  Read this character so that we can
		 * skip trailing blanks and the trailing separator, if any.
		 */

		do {
			CMNTLGET(*lastc);
		} while (ISSPTB(*lastc));

		if (MATCH(*lastc, _MASKS, MRNLSEP)) {
			do {
				CMNTLGET(*lastc);
			} while (ISSPTB(*lastc));
		}
	}
	
	return(0);
}

/*	nex_data - get the next data group 
 *
 *	On return, lval will contain the value and lcount the repeat count 
 *	Outptr will point to character immediately following value 
 *
 *	The return value is:	-1 for EOF
 *				 0 for ok 
 *				 1 for null value read 
 *				 2 for null value, followed by possible
 *				   variable name 
 *				 valid error number if an error 
 */

static int
nex_data(
	int		type,	/* Type of data item */
	long		ptr,	/* Address of data item */
	int		cnt,	/* Number of values to look for */
	int		inc,
	char		lastc,	/* First character of value (may be blank) */
	unit		*cup,	/* Input unit */
	struct Echoinfo	*echoptr,	
	struct Inpinfo	*inptr,	/* Describes input buffer */
	long		*lval,	/* Value is placed here */
	int		*lcount,	/* Repeat count is returned here */
	int		elsize	/* declared size of character item */
)
{
	char	c, oc;
	int	ocnt, ss;
	long	*optr;
	int	holcnt;		/* Length of hollerith string */
	long	stat;	
	char	newc;

	c	= lastc;

	while (ISSPTB(c)) {
		CMNTLGET(c);
	}
	
	*lcount	= 1;	/* set repeat count */

	if (isdigit((int) c)) {

		/*
		 * Look for repeat count.  We can have a repeat count 
		 * for any type of data, including character.
		 */

		*lcount	= c - '0';
		ocnt	= inptr->incnt;	/* save count and pointer, in case */
		optr	= inptr->inptr;	/* this isn't repeat count */
		oc	= c;

		for (;;) {

			LGET(c);	/* Ignore comments while doing this */

			if (isdigit((int) c))
				*lcount	= (*lcount * 10) + c - '0';
			else
				break;
		}

		/*
		 * Could have r*c, rH, rL, or rR, where r is the number just
		 * read.  No embedded blanks are allowed in r*c, rH, rL, or rR.
		 */

		switch (c) {

			case '*':
				CMNTLGET(c);	/* Get next character. */

				if (isdigit((int) c)) {
					/* See if we have a repeat count
					 * followed by hollerith, like
					 * 3*4Habcd
					 */
					holcnt	= c - '0';
					ocnt	= inptr->incnt;
					optr	= inptr->inptr;
					oc	= c;

					for (;;) {

						LGET(c);

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
							return(get_holl(c,
								holcnt, type,
								cup, echoptr,
								inptr, lval));

						default:
							/* backup restore */
							inptr->inptr	= optr;
							/* cnt and ptr */
							inptr->incnt	= ocnt;
							c	= oc;
							ocnt	= 1;
							break; 

					}	/* switch */
				}
				break;		/* Ordinary repeat count */

			case 'H':
			case 'h':
			case 'R':
			case 'r':
			case 'L':
			case 'l':
				/*
				 * Assuming we have a Hollerith string, like 3Habc
				 */
				holcnt	= *lcount;
				*lcount	= 1;	/* No repeats */

				return(get_holl(c, holcnt, type, cup, echoptr,
						inptr, lval));

			default:
				/* No repeat count, backup restore, cnt & ptr */
				inptr->inptr	= optr;
				inptr->incnt	= ocnt;
				c		= oc;
				ocnt		= 1;
				*lcount		= 1;
				break; 
		}	/* switch */
	}

	/*
	 * Looking for a value.  When we get here we are at a nonblank
	 * character, unless we had the form r*, in which case it may
	 * be followed by a blank (NULL).
	 */

	if (MATCH(c, _MASKS, MRNLSEP)) {
		inptr->inptr--;	/* reset cnt and ptr so */
		inptr->incnt++;	/* we can read separator again */
		return(1);	/* return null value */
	}
	else if (ISSPTB(c)) {
		return(1);	/* return null value */
	}

	else
		if (MATCH(c, _MASKS, MRNLCOMM)) {
			/*
			 * The only time we would see this is if we have
			 * input like: A = 5*; 
			 */

			*lval	= *(lval+1)	= 0;
			inptr->incnt++;	/* reset cnt and ptr so rest on line */
			inptr->inptr--;	/* is read in a null values */
			return(1);	/* return null value */
		}
		else
			if (MATCH(c, _MASKS, MRNLDELIM)) {
				inptr->inptr--;	/* reset cnt and ptr so */
				inptr->incnt++;	/* read delimiter again */
				return(2);	/* Return null value */
			}
	/*
	 * It is important that we handle the special cases of types logical
	 * and character first, because the format of their data is treated
	 * differently.
	 */

	if (type == DT_LOG) {
		/* Looking for a logical value.  Logical values must be of
		 * the form: optional decimal point, followed by a 'T' for
		 * true or an 'F' for false, optionally followed by one
		 * or more additional characters.  Those additional
		 * characters cannot include '=', ',', ':', ';', '(', '$'
		 * or '&'.
		 */
		if (c == '.') {

			LGET(c);

			if ((c == 'T') || (c == 't')) {
				/* .T or .t assumed to be a logical value */
				*lval	= (long) TRUE;
			}
			else if ((c == 'F') || (c == 'f')) {
				/* .F or .f assumed to be a logical value */
				*lval	= (long) FALSE;
			}
			else
				RNLERROR(FENLIVLG);	/* Invalid logical */
		}

		else {
			/*
			 * If the string does not start with a '.', it could
			 * be a logical value or a variable name.  Try to
			 * determine which by seeing if it is followed by a 
			 * replacement character or '('.  Save count and
			 * pointer in case this isn't a value.
			 */

			ocnt	= inptr->incnt;
			optr	= inptr->inptr;
			newc	= *optr++;
			ocnt--;

			while (!(ISSPTB(newc))) {
				if (MATCH(newc, _MASKS, MRNLSEP) ||
				    MATCH(newc, _MASKS, MRNLDELIM))
					break;	/* Assume value */
				if (MATCH(newc, _MASKS, MRNLREP) ||
				    (newc == '(')) {
					/*
					 * Reset, because this MAY have been
					 * the first letter of a variable name
					 */
					inptr->inptr--;
					inptr->incnt++;
					return(2);	/* Null value */
				}
				newc	= *optr++;
				ocnt--;
			}

			while ((ISSPTB(newc)) && ocnt-- > 0)
				newc	= *optr++;

			if (MATCH(newc, _MASKS, MRNLREP)) {
				/*
				 * Reset, because this MAY have been
				 * the first letter of a variable name
				 */
				inptr->inptr--;
				inptr->incnt++;
				return(2);	/* Null value */
			}

			if ((c == 'T') || (c == 't')) {
				*lval	= (long) TRUE;
			}
			else if ((c == 'F') || (c == 'f')) {
				*lval	= (long) FALSE;
			}
			else if (ISSPTB(c) || (MATCH(c, _MASKS, MRNLSEP))) {
				return(1);	/* Indicate null value */
			}
			else {
				RNLERROR(FENLIVLG);	/* Invalid logical */
			}
		}
		/*
		 * We assume we're reading a logical value.
		 * Skip to the end of this value. 
		 */

		while ( !(ISSPTB(c))) {

			CMNTLGET(c);

			if (MATCH(c, _MASKS, MRNLDELIM) ||
			    MATCH(c, _MASKS, MRNLSEP)) {
				/*
				 * Reset cnt and prt so this will be read and
				 * handled correctly.
				 */
				inptr->inptr--;
				inptr->incnt++;

				return(0);	/* return logical value */
			}
		
		}
		return(0);	/* return logical value */

	}	/* End of type logical */

	if (type == DT_CHAR)	/* Read character data */
		return (g_charstr(ptr, cnt, c, cup, echoptr, inptr, *lcount, elsize));

	/*
	 * Get value for variable that is not type LOGICAL or CHARACTER
	 */

	if (isdigit((int) c) || c == '+' || c == '-' || c == '.') {

		if (type == DT_CMPLX)
			RNLERROR(FENLIVCX);

		return(g_number(type, cup, lval, inptr));
	}

	/*
	 * When we get here we are looking for a VALUE.  We are at a
 	 * nonblank character which is not a digit, +, or -, separator,
	 * comment or delimiter.
	 */

	if (c == '(') {
		return(g_complx(type, cup, echoptr, inptr, lval));
	}
	else if ((c == '\'') || (c == '"')) {	/* look for Hollerith string */
		return(get_quoholl(c, type, cup, echoptr, inptr, lval));
	}

	else if (c == 'O' || c == 'o') {	/* look for octal number */
		return(g_octhex(type, cup, echoptr, inptr, lval, OCTAL));
	}
	else if (c == 'Z' || c == 'z') {	/* look for hexadecimal number */
		return(g_octhex(type, cup, echoptr, inptr, lval, HEX));
	}
	else {
		/*
		 * No valid value.
		 * Reset inptr, because this MAY have been the first 
		 * character of a variable name.
		 * For example, if we have: integer var1(3),var2, with input:
		 * var1 = 2, var2 = 5
		 * then when we try to read the value for var1(2), we will see 
		 * 'var2'
		 */

		inptr->inptr--;
		inptr->incnt++;

		return(2);	/* Return null value */
	}
}

/*
 * Get the value for a complex number.
 * On entry, we are at '(' in the representation of a complex number.
 * 
 * Returns:	0 if OK,
 *		-1 if EOF
 *		valid error number if an error
 */

static int
g_complx(
	int		type,
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr,
	long		*lval
)
{
	char	c;
	long	*oldp;
	long	mode, stat;
	int	ss, i;

	/* 
	 * IN reading the complex number, assume
	 * intervening EOR is OK
	 */

	if (type != DT_CMPLX) {
		RNLERROR(FENLTYPI);	/* type mismatch */
	}

	mode	= 0;

	/*
	 * If the user had turned off blanks as separator, tell
	 * NICONV to ignore them.  Otherwise, blanks are significant.
	 */

	if (_BLNKSEP == 0)
		mode   |= MBN;

	/* loop and get both real and imaginary */

	for (i = 0; i < 2; i++) {

		do {
			LGET(c);	/* skip the '(' */
		} while (ISSPTB(c)); 	/* skip blanks */

		TONICV(lval + i);	/* convert real/imaginary part */

		if (l_convert(lval + i, DT_REAL, stat))	/* make &lval[0] real */
			RNLERROR(FENLTYPI);	/* type mismatch */

		do {
			LGET(c);
		} while (ISSPTB(c));

		if ((c != ',') && (i == 0)) {
			RNLERROR(FENLIVCX);	/* error in complex number
						 * format */
		}
	}

	if ( c != ')') {
		RNLERROR(FENLIVCX);	/* error in complex number format */
	}

	return(0);
}

/*
 * Read a number.
 * 
 * Returns:	0 if ok
 *		-1 if EOF
 *		RNL_ERROR if error (errno is set)
 */

static int
g_number(
	int		type,
	unit		*cup,
	long		*lval,
	struct Inpinfo	*inptr
)
{
	long	*oldp;
	long	mode, stat;
	int	ss;	

	mode	= 0;

	if (type == DT_DBLE)
		mode   |= MD;

	/*
	 * If the user had turned off blanks as separator, tell NICONV
	 * to ignore them.  Otherwise, blanks are significant.
	 */

	if (_BLNKSEP == 0)
		mode   |= MBN;

	TONICV(lval);

	if (l_convert(lval, type, stat)) {
		RNLERROR(FENLTYPI);
	}

	return(0);
}

/*
 * Convert value read to proper type for storage.
 * If _TYP_CONV indicates, issue an error when 
 * value read does not match type of variable.
 *
 * returns:	0 if conversion was ok
 *		RNL_ERROR if error
 */

static int
l_convert(
	long	*val,
	int	type,	/* Data type */
	long	stat
)
{
	short	sval;
	long	lval;
	union {
		long	l;
		double	f;
	} uval;

	if (stat <= 0 || stat > NVDOUB)
		return(RNL_ERROR);

	/*
	 * Switch on value read type
	 */

	switch (stat) {

	case NV32I:
	case NV64I:
		/*
		 * Value read is integer.
		 */
		switch (type) {
			case DT_SINT:
			case DT_INT:
				break;

			case DT_REAL: 
			case DT_DBLE: 
				if (!_TYP_CONV)
					return(RNL_ERROR);

				uval.f	= (double) *val;
				*val	= uval.l;
				break;

			case DT_LOG:
			default:
				/* Can't convert to logical or character */
				return(RNL_ERROR);
		}
		break;

	case NVREAL:
	case NVDOUB:
		/*
		 * Value read is real.
		 */
		uval.l	= *val;

		switch (type) {
			case DT_SINT:
				if (!_TYP_CONV)
					return(RNL_ERROR);

				sval	= (short) uval.f;
				*val	= sval;
				break;			

			case DT_INT:
				if (!_TYP_CONV)
					return(RNL_ERROR);

				lval	= (long) uval.f;
				*val	= lval;
				break;			

			case DT_REAL:
			case DT_DBLE:
				break;

			case DT_LOG:
			default:	
				return(RNL_ERROR);

 		} /* switch */
	}

	return(0);
}

/*
 * Read a character string.
 *
 * Input: inptr will point one past the first character of the string.
 *	"c" will contain the first character of the string.
 *
 * Returns:	0 if ok,
 *		-1 if EOF
 *		RNL_ERROR if error
 */

static int
g_charstr(
	long		p,	/* Address of variable being read */
	int		cnt,	/* Number of strings we expect to read */
	char		c,	/* First character of string. */
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr,
	int		lcount,	/* Repeat count */
	int		elsize	/* declared size of character item */
)
{
	int	eos;	/* eos == -1 if end or beginning of string */
	int	i, ch;
	unsigned int	len77;
	char	*cp;
	long	stat;
	char	enddelim;
	char	c1;	
	int	repcount;
	char	*cpold;
	int	ss;
	long	*optr;
	int	ocnt; 
	_fcd	fchp;

	/*
	 * Character data may be enclosed in apostrophes or quotation marks.
	 * Each apostrophe within a character constant
	 * delimited by apostrophes must be represented by
	 * 2 consecutive apostrophes without an intervening blank or
	 * end of record. The same holds true for quotation marks. Character
	 * constants may be continued from the end of one record to the
	 * beginning of the next record. The end of the record does not
	 * cause a blank or any other character to become part of the constant.
	 * Blank characters, separator characters, comment characters, and
	 * delimiter characters may appear in character constants.
	 * If the character constant has the following properties:
	 * 1. It does not contain blank characters,
	 *    separator characters, comment characters, left parenthesis 
	 *    or delimiter characters.
	 * 2. It does not cross a record boundary,
	 * 3. the first nonblank character is not a quotation mark or apostrophe,
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
	 */

	eos	= 0;
	len77	= elsize;	/* Get character element length */

	if (len77 != 0) {

		/* p is a c pointer to the character data */
		cp		= (char *) p;
		repcount	= MIN(lcount,cnt);

		/*
		 * If the first character is a quote or apostrophe, we expect
		 * that character to delimit the end of the string.
		 */

		if ((c == '\'') || (c == '"')) {
			enddelim	= c;

			/* find characters in string */

			for (i = 0; i < len77 && eos == 0; i++) {
				GETSTR();
				if (eos == 0)
					*cp++	= ch;
			} 

			if (eos == -1)
				i--;

			i	= len77 - i;	/* If declared len > read len */

			(void) memset(cp, BLANK, i);	/* then blank fill */

			cp	= cp + i;

			while (eos != -1) {
				/*
				 * We didn't hit the end of the string yet.
				 * Search for it.
				 */

				GETSTR();
			}

			while (--repcount) {
				/* We have a repeat count.
				 * cp will point to the next element.
				 * Copy len77 characters to the next
				 * element.
				 */
					
				cpold	= (char *) p;
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
			if (lcount > 1)
				RNLERROR(FENLNOVL); /* invalid character data */

			/*
			 * Determine if this is a value or a variable name.
			 * Save count and pointer in case this isn't a value.
			 */

			ocnt	= inptr->incnt;
			optr	= inptr->inptr;
			c1	= *optr++;
			ocnt--;

			while (!(ISSPTB(c1))) {

				if (MATCH(c1, _MASKS, MRNLSEP) ||
				    MATCH(c1, _MASKS, MRNLDELIM))
					break;	/* Assume value */

				if (MATCH(c1, _MASKS, MRNLREP) || c1 == '(' ) {
					/*
					 * Reset, because this MAY have been
					 * the first letter of a variable name.
					 */
					inptr->inptr--;
					inptr->incnt++;

					return(2);	/* Null value */
				}

				c1	= *optr++;
				ocnt--;
			}

			while ((ISSPTB(c1)) && ocnt-- > 0)
				c1	= *optr++;

			if (MATCH(c1, _MASKS, MRNLREP) || (c1 == '(')) {
				/*
				 * Reset, because this MAY have been
				 * the first letter of a variable name.
				 */
				inptr->inptr--;
				inptr->incnt++;
				return(2);	/* Null value */
			}

			i	= 0;
			c1	= c;

			while (!(ISSPTB(c1))) {

				if (i < len77) {
					*cp++	= c1;
					i++; 
				} 

				LGET(c1); 

				if (MATCH(c1, _MASKS, MRNLSEP) ||
				    MATCH(c1, _MASKS, MRNLCOMM)) {
					/* Want to read and handle next time */
					inptr->inptr--;
					inptr->incnt++;
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
		RNLERROR(FENLIOER);	/* indicate error */
	}

	return(0);
}

/*
 * Read a hollerith string. 
 * 
 * Returns:	0 if a value was found,
 *		-1 if EOF
 *		RNLERROR if an error occurred (errno is set)
 */

static int
get_holl(
	char		holltype,	
	int		count,	/* Number of characters in string */
	int		type,	/* Type of data item */
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr,
	long		*lval
)
{
	int	i;
	char	*holbufptr;
	char	c;
	long	stat;
	int	ss;
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
	 * Also, we are going to assume that whatever we read in will need
	 * to fit in one word. Repeat counts are allowed. If it becomes
	 * necessary to allow hollerith strings of > 8 characters, some
	 * thought will need to be given as to how to handle repeat counts.
	 */

	if (type == DT_CMPLX || type == DT_DBLE || type == DT_CHAR)
		RNLERROR(FENLTYPI);	/* Indicate error: type mismatch */

	if (count > sizeof(long)) {
		RNLERROR(FENLIOER);	/* Indicate error */
	}

	fill		= BLANK;
	holbufptr	= (char *)lval;

	if (holltype == 'R' || holltype == 'r') {
		/* right justified */
		fill		= NULLC;
		holbufptr	= holbufptr + (sizeof(long) - count);
	}
	else
		if (holltype == 'L' || holltype == 'l')
			fill	= NULLC;

	/*
	 * Last character in buffer is the EOR character,
	 * that's why we check for incnt > 1 
	 */

	for (i = 0; i < count && (inptr->incnt > 1) ; i++) {
		LGET(c);	/* use LGET because comment characters are not 
				 * special inside hollerith string */
		*holbufptr++	= c;
	}

	if (i == count) {
		/* Do we need to fill the last word? */
		if (holltype == 'R' || holltype == 'r')	/* right justified? */
			holbufptr	= (char *)lval;

		(void) memset(holbufptr, fill, sizeof(long) - count);
	}
	else {
		/*
		 * We hit EOR before we read enough characters _or_ we had
		 * too many characters.
		 */

		RNLERROR(FENLIOER);
	}

	return(0);
}

/*
 * Get a hollerith string that is surrounded by quotes or apostrophes
 * Legal syntax is '----'L, '----'R, or '----'H
 *
 * Returns:	0 if a value was found,
 *		-1 if EOF
 *		RNL_ERROR if an error occurred (errno is set)
 */

static int
get_quoholl(
	char		cdelim,	/* Quote or apostrophe (to end hollerith) */
	int		type,	/* Type of data */
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr,
	long		*lval	/* Value is placed here */
)
{
	int	numchar;	/* character counter */
	int	j;
	int	fill;		/* Character to fill with: either ' ' or '\0' */
	long	holbuf;		/* Data is stored here until we know whether 
			 	   it is right or left justified. */
	char	*holbufptr;	/* pointer into holbuf */
	char	c;		/* Character read */
	long	stat;
	char	*lvalcharptr;	/* Pointer to value */
	int	ss;

	/*
	 * Can't have hollerith input for DOUBLE, COMPLEX or CHARACTER data.
	 * Hollerith input is supported for compatibility with
	 * old versions of namelist. 
	 *
	 * Because we don't allow CHARACTER data, we can make the
	 * simplifying assumption that we start on a word boundary.
	 * Also, we are going to assume that whatever we read in will need
	 * to fit in one word. Repeat counts are allowed. If it becomes
	 * necessary to allow hollerith strings of > 8 characters, some
	 * thought will need to be given as to how to handle repeat counts.
	 */

	if (type == DT_CMPLX || type == DT_DBLE || type == DT_CHAR)
		RNLERROR(FENLTYPI);

	lvalcharptr	= (char *)lval;
	holbufptr	= (char *) &holbuf;

	/*
	 * We do not allow these quoted strings to be continued on
	 * another record.
	 */

	numchar	= 0;

	for (;;) {

		LGET(c);

		if (c == cdelim) {

			/* Comment characters allowed inside quoted string */

			LGET(c);

			if (c != cdelim)
				break;	/* That was the end of the quoted
					 * string.  Otherwise, we saw two
					 * quotes in a row, which means
					 * we store one.
					 */
		}

		if (++numchar > sizeof(long))
			RNLERROR(FENLIOER);

		*holbufptr++	= c;	/* Save the character */

		/*
		 * Last character in the input buffer is the EOR character,
		 * that's why we check for incnt <= 1 
		 */
		if (inptr->incnt <= 1) {
			RNLERROR(FENLIOER);
		}

	}	/* On exit from this loop, numchar = number of chars. stored */

	
	if (c == 'L' || c == 'l')
		fill	= NULLC;
	else if (c == 'R' || c == 'r') {

		/* Right justify and store the value just read */

		holbufptr	= holbufptr - 1;	/* Last character */
		lvalcharptr	= lvalcharptr + (sizeof(long) - 1);
		j		= sizeof(long) - numchar;	

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
			/* Reset pointers, this character does */
			/* not belong to this value */
			inptr->inptr--;
			inptr->incnt++;
		}
	}

	/* Do we need to fill the last word? */

	(void) memset(holbufptr, fill, sizeof(long) - numchar);

	*lval	= holbuf;

	return(0);
}


/*
 * Octal or hex editing, provided for compatibility with old versions
 * of namelist.
 * Legal formats: O'123 or O'123'. Octal number may not contain blanks,
 * and this is a difference with the old version of namelist.
 * Legal formats: Z'1a3 or Z'1a3'. 
 * 
 * On input: inptr should point to the character immediately following the O
 *
 * Returns:	0 if a value was found,
 *		1 if a null value was found
 *		2 if a null value was found, and it is not followed
 *		  by another value
 *		-1 if EOF
 *		RNLERROR if an error occurred (errno is set)
 */

static int
g_octhex(
	int		type,
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr,
	long		*lval,
	int		base
)
{
	char	c;
	long	stat;
	char	strbuf[2];
	int	ss;

	if (*inptr->inptr != '\'') {
		/* Can't be a value, might be a variable name */
		inptr->inptr--;
		inptr->incnt++;

		return(2);	/* NULL value */
	}

	/*
	 * This type of format won't work for complex or double precision
	 */

	if (type == DT_CMPLX || type == DT_DBLE) {
		RNLERROR(FENLTYPI);	/* type mismatch */
	}

	LGET(c);	/* Skip the apostrophe */
	LGET(c);	/* and get the next character */
	*lval		= 0;
	strbuf[1]	= '\0';

	while (!(ISSPTB(c)) && c != '\'') {

		if (base == OCTAL) {

			if ((!isdigit((int) c)) || (c == '9') ||
			    (*lval >> 61)) {
				RNLERROR(FENICVIC);	/* NICV type error */
			}

			*lval	= (*lval * 8) + c - '0';
		}
		else {	/* Check for hex digit or overflow */

			if ((!isxdigit(c)) || (*lval >> 60)) {
				RNLERROR(FENICVIC);	/* NICV type error */
 			} 

			strbuf[0]	= c;
			*lval		= (*lval * 16) +
					(int) strtol(strbuf, (char **)NULL, 16);
		}	

		CMNTLGET(c);		/* Check for comments following value */

		if (MATCH(c, _MASKS, MRNLSEP)) {
			inptr->inptr--;
			inptr->incnt++;	/* Want to read separator after */
			break;		/* after we return from this routine */
		}
	}

	return(0);	/* indicate value */
}


/*
 * _rnl_fillrec - reads one line from a file.
 *
 * return value: 	0 - normal return
 *			EOF - end of file
 *			RNL_ERROR - error was encountered (errno is set)
 *			cup->uend is set if EOF encountered
 */

static int
_rnl_fillrec(
	unit		*cup,
	struct Echoinfo	*echoptr,
	struct Inpinfo	*inptr
)
{
	long	stat;
	int	ss;

	inptr->incnt	= _frch(cup, inptr->instart, cup->urecsize, 1, &stat);

	if (inptr->incnt < 0 || stat != EOR) {
		if (stat == EOF) {
			inptr->incnt	= 1; /* Treat as if it had 1 blank */
			cup->uend	= PHYSICAL_ENDFILE;
			return(EOF);
		}
		else if (stat == EOD) {
			inptr->incnt	= 1; /* Treat as if it had 1 blank */
			if (cup->uend == 0)
				cup->uend	= LOGICAL_ENDFILE;
			return(EOF);
		}
		else if (stat == CNT) {
			errno	= FENLRECL;	/* Too much in a record */
			return(RNL_ERROR);
		}

		if (inptr->incnt < 0) {
			return(RNL_ERROR); 	/* error code already in errno*/
		}
	}

	cup->uend	= 0;

	if (inptr->incnt == 0)
		inptr->incnt	= 1;	/* Treat this as if it had 1 blank */

	/* Add a blank character to end of record */

	*(inptr->instart+inptr->incnt)	= (long) ' ';

	if ((echoptr->rnlecho) ||
	    (MATCH(*inptr->instart, _MASKS, MRNLFLAG))) { 
		/* Begin echoing input */
		echoptr->rnlecho	= 1;
		_rnlecho(echoptr->eunit, inptr);
	}

	/* Always skip the first character in a record.*/
	/* Don't need to adjust incnt because we added a blank at the end. */

	inptr->inptr	= inptr->instart + 1;

	return(0);
}

static void
pr_msg(char *string)
{
        (void) write(fileno(errfile), string, strlen(string));

	return;
}


/*
 * Returns:	0 if delimiter is not part of hollerith string
 *		1 if delimiter is part of hollerith string
 */

static int
isholl(
	long		*hlptr,	/* Pointer to possible hollerith character */
	struct Inpinfo	*inptr
)
{
	char	hlval;

	hlval	= (char) *(hlptr - 1);

	if (isdigit(hlval) && ((hlval - '0') <= 8) && ((hlval - '0') > 0)) {
		/*
		 * We have digit followed by Hollerith designator, check
		 * the preceding character.
		 */
		if (((hlval - '0') + hlptr) >= (inptr->inptr - 1)) {

			/* Column 1 of input is in inbuff[1] and is ignored */

			if (hlptr > &inptr->inbuff[3]) {

				hlval	= (char) *(hlptr - 2);

				if (!ISSPTB(hlval) && hlval != '*' &&
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
