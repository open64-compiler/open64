/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/rnl90.c	92.9	10/12/99 13:16:22"

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

/* EXTERNAL entry points */
extern int _s_scan_extensions(void *ptr, ftype_t type,
	unsigned long elsize, long *field_begin,
	unsigned long rec_chars, int *fwptr, long cmode);
extern int _nicverr(const int _Nicverror);

/* use SUBGTC when the character retrieval cannot hit an end of file until
 * the retrieval is complete.  This occurs when retrieving the characters of
 * a name.  This macro is used in functions outside the main namelist FRN
 * routine.
 */

#define SUBGTC(x) {					\
	while (cup->ulinecnt == 0) {			\
		if (errn = _nlrd_fillrec(css, cup)) {	\
			return(errn);			\
		}					\
	}						\
	x	= (char) *cup->ulineptr++;		\
	cup->ulinecnt--;				\
}

#define SUBGTCNOEOR(x) {				\
	if (!cup->ulinecnt) {				\
		x	= ' ';				\
	} else {					\
		x	= (char) *cup->ulineptr++;	\
		cup->ulinecnt--;			\
	}						\
}

/* Fortran 95 provides an exclamation point as an input record
 * comment character.  Prepare for its addition in 3.0+.  If
 * comment begins an input record, ignore the rest of the
 * input record and get the next input record.
 */

#define CMTE_SUBGTC(x) {				\
	while (cup->ulinecnt == 0) {			\
		if (errn = _nlrd_fillrec(css, cup)) {	\
			return(errn);			\
		}					\
	}						\
	x	= (char) *cup->ulineptr++;		\
	if (x == '!') {					\
		x	= ' ';				\
		cup->ulinecnt	= 1;			\
	}						\
	cup->ulinecnt--;				\
}

#define CMTE_SUBGTCNOEOR(x) {  				\
	if (!cup->ulinecnt) { 				\
		x	= ' ';				\
	} else {					\
		x	= (char) *cup->ulineptr++;	\
		cup->ulinecnt--;			\
	}						\
	if (x == '!') {					\
		x	= ' ';				\
		cup->ulinecnt	= 0;			\
	}						\
}
		


/* use MAINGT when the character retrieval can hit an end of file before 
 * retrieval is complete.  This occurs when retrieving '=', delimiters,
 * , etc.  This macro is used in functions within the main namelist FRN
 * routine.  CMTE_MAINGT is the same except comments are allowed for F95.
 */

#define MAINGT(x) {						\
	while (cup->ulinecnt == 0) {				\
		if (errn = _nlrd_fillrec(css, cup)) {		\
			if (errn < 0) {				\
				ENDD(endf, css, FERDPEOF);	\
			}					\
			else {					\
				ERROR0(errf, css, errn);	\
			}					\
		}						\
	}							\
	x	= (char) *cup->ulineptr++;			\
	cup->ulinecnt--;					\
}

#define CMTE_MAINGT(x) {					\
	while (cup->ulinecnt == 0) {				\
		if (errn = _nlrd_fillrec(css, cup)) {		\
			if (errn < 0) {				\
				ENDD(endf, css, FERDPEOF);	\
			}					\
			else {					\
				ERROR0(errf, css, errn);	\
			}					\
		}						\
	}							\
	x	= (char) *cup->ulineptr++;			\
	/* CHECK for comment */					\
	if (x == '!') {						\
		x	= ' ';					\
		cup->ulinecnt	= 1;				\
	}							\
	cup->ulinecnt--;					\
}

#define GETSECTION(x) {  \
		field_begin	= cup->ulineptr;		\
		field_end	= cup->ulineptr;		\
		for (j = 0; j < cup->ulinecnt; j++) {		\
			x	= (char) *field_end;		\
			if (x == ')' || x == ',' || x == ':')	\
				break;				\
			field_end++;				\
		}						\
		field_width	= j;				\
}

/*
 *      This table is used to drive the f90 input conversion based on the
 *      type of the data.
 */
ic_func *ncf_tab90[] = {
	NULL,		/* DVTYPE_UNUSED */
	NULL,		/* DVTYPE_TYPELESS */
	_iu2s,		/* DVTYPE_INTEGER */
	_defgu2sd,	/* DVTYPE_REAL */
	_defgu2sd,	/* DVTYPE_COMPLEX */
	NULL,		/* DVTYPE_LOGICAL */
	NULL,		/* DVTYPE_ASCII */
};

static int _nlrd_fillrec(FIOSPTR css, unit *cup);

static int _getname(FIOSPTR css, unit *cup, char *buf, char *lastc);

static void _cnvrt_toupper(char *bufr);

static nmlist_goli_t *_findname(char *key, nmlist_goli_t *nlvar,
		unsigned countitm);

static int _getnlval(FIOSPTR css, nmlist_goli_t *nlvar, char *lastc, 
	unit *cup);

static int _indx_nl(FIOSPTR css, unit *cup, struct DvDimen *dvdn,
   int *ndim, long strbegend[3], int *encnt, int *icnt, int arryflag);

static int _nlrdent(FIOSPTR css,unit *cup,nmlist_goli_t *nalist,
	unsigned count, char *lastc, int byt);

static int _nlread(FIOSPTR css, ftype_t type, unit *cup, void *ptr,
   long elsize, int cnt, int inc, char *lastc);

static int _nexdata(FIOSPTR css, ftype_t type, void *ptr, int cnt, int inc,
   char lastc, unit *cup, long *lval, int *lcount, long elsize, int *nullvlu);

static int _g_charstr(FIOSPTR css, unit *cup, void *p, int cnt, char c,
   int lcount, long elsize, int *nullvlu);

static int _g_complx(FIOSPTR css, unit *cup, ftype_t type, long *lval,
   long elsize);

static int _g_number(ftype_t type, unit *cup,long *lval, long elsize);

static int _gocthex(FIOSPTR css, unit *cup, ftype_t type, long *lval, int base,
   long elsize, int *nullvlu);

static int _get_holl(FIOSPTR css, unit *cup, char holltype, int count,
	ftype_t type, long *lval, long elsize);

static int  _get_quoholl(FIOSPTR css, unit *cup, char cdelim, ftype_t type,
   long *lval, long elsize);

static int _nl_stride_dv(FIOSPTR css, unit *cup, DopeVectorType *dv,
	struct DvDimen *sectn, char *lastc, long strbegend[3]);

static int _nl_strd_derv( FIOSPTR css, unit *cup, DopeVectorType *dv,
   struct DvDimen *sectn, char *lastch, nmlist_goli_t *vdr,
   unsigned int cnt, long bte);

/*
 *      _FRN    - called by compiled Fortran programs to process a namelist 
 *                read statement.
 *      Synopsis
 *              int _FRN(       ControlListType *cilist,
 *                              nmlist_group *namlist,
 *                              void *stck);
 *              Where
 *                      cilist  - pointer to the control information list
 *                                information.  This describes the specifiers
 *                                for the current I/O statement.
 *                      namlist  - pointer to the namelist table.
 *                      stck    - pointer to stack space which is passed
 *                                to each call to _FRU for a particular
 *                                statement.  This is used by the library.
 *      Return value
 *              IO_OKAY, IO_END, or IO_ERR
 */

int
_FRN(ControlListType *cilist, nmlist_group *namlist, void *stck)
{
	char		buf[MAXNAML + 5], c;
	int		errf;		/* Error processing flag	*/
	int		endf;		/* EOF processing flag		*/
	int		errn;		/* Error number			*/
	register unum_t	unum;		/* Actual unit number		*/
	unit		*cup;		/* Pointer to unit table entry	*/
	unsigned long	rlen;		/* group name length		*/
	unsigned long	rcount;		/* count of namelist items	*/
	char		*rptr;		/* pointer to group name	*/
	char		*varptr;	/* ptr to group_obj_list item	*/
	unsigned long	varlen;		/* len of group_obj_list name	*/
	nmlist_goli_t	*nlvar;		/* ptr to next variable entry	*/
	nmlist_goli_t	*fdvar;		/* ptr to next variable entry	*/
	ftype_t		type;
	char		endnmlchar;	/* namelist group name char	*/
	FIOSPTR		css;

	/* Assertions */
	/* Validate that the size of *stck is large enough */
	assert ( cilist->stksize >= sizeof(struct fiostate)/sizeof(long) );

	/* The compiler flags namelist with fmt flag */
	assert( (cilist->fmt == CI_NAMELIST));

	/* The compiler disallows namelist with internal files */
	assert( !(cilist->internal && cilist->fmt == CI_NAMELIST));

	/* The compiler disallows namelist with direct files */
	assert( !(cilist->dflag && cilist->fmt == CI_NAMELIST));

	css	= stck;
	errn	= 0;
	type	= DVTYPE_UNUSED;
	varptr	= NULL;

/* **************************************************************************
 *	Statement Initialization Section
 ************************************************************************* */

	/* Establish error processing options */
	errf	= (cilist->errflag || cilist->iostatflg);
	endf	= (cilist->endflag || cilist->iostatflg);

	if (cilist->uflag == CI_UNITASTERK)
		unum	= STDIN_U;
	else
		unum	= *cilist->unit.wa;

	STMT_BEGIN(unum, 0, T_RNL, NULL, css, cup);

	if (cup == NULL) {	/* If not connected */
		cup	= _imp_open(css, SEQ, FMT, unum, errf, &errn);
		/*
		 * If the open failed, cup is NULL and errn contains
		 * the error number.
		 */
		if (cup == NULL)
			goto finalization;
	}
	/* All paths which lead here have set cup to a non-null value */
	assert (cup != NULL);

	/* Copy the user's error processing options into the unit table */
	cup->uflag	= (cilist->errflag		?  _UERRF : 0) |
			  (cilist->endflag		?  _UENDF : 0) |
			  (cilist->iostat_spec != NULL	? _UIOSTF : 0);
	css->u.fmt.nonadv	= 0;

	/* If trying to read a file without read permission */
	if ((cup->uaction & OS_READ) == 0) {
		errn	= FENOREAD;
		ERROR0(errf, css, errn);
	}
	/* If attempting formatted I/O on an unformatted file */
	if (!cup->ufmt) {
		errn	= FEFMTTIV;
		ERROR0(errf, css, errn);
	}
	/* If sequential and writing, disallow read after write */
	if (cup->useq && cup->uwrt != 0) {
		errn	= FERDAFWR;
		ERROR0(errf, css, errn);
	}

	/* Preset fields in unit table */

	cup->uwrt	= 0;

	/* Initialize fields in the Fortran statement state structure */

	css->u.fmt.icp		= NULL;
	css->u.fmt.blank0	= cup->ublnk;
	css->u.fmt.lcomma	= 0;
	css->u.fmt.slash	= 0;

	if (cup->useq == 0) {	/* If seq. attempted on direct file */
		errn	= FESEQTIV;	/* Sequential not allowed */
		ERROR0(errf, css, errn);
	}
	/* external sequential formatted I/O */
	if (cup->uend && !cup->umultfil) {	/* If after endfile */
		errn	= FERDENDR;	/* Read after endfile */
		ERROR0(endf, css, errn);
	}

	css->u.fmt.endrec	= _sr_endrec;

	if (cup->pnonadv == 0)		/* if previous ADVANCE='YES' */
		errn	= (*css->u.fmt.endrec)(css, cup, 1); /* Read a record */
	else				/* else previous ADVANCE='NO' */
		css->u.fmt.leftablim	= cup->ulineptr; /* set left tablimit */

	if (errn != 0)
		if (errn < 0 ) {
			ENDD(endf, css, FERDPEOF);
		}
		else {
			ERROR0(errf, css, errn);
		}
	cup->pnonadv	= css->u.fmt.nonadv;	/* remember prev ADVANCE= */

/* **************************************************************************
 *	Data Transfer Section
 ************************************************************************* */

#if defined(__mips) || !defined(_WORD32)
	if (!(cup->uft90)) {
		errn	= _rnl90to77(css, cup, namlist, stck, errf, endf);
		goto finalization;

	}
#endif
skiprec:
	while (cup->ulinecnt == 0) {
		errn	= _nlrd_fillrec(css, cup);
		if (errn != 0)
			goto err_eof;
	}
rrd:
	do {
		CMTE_MAINGT(c)
	} while (ISBLANK(c));
	if (c != '&' && c != '$') {
		/* irix f77 and f90, and 'assign -Y on' skip an input
		 * record when the first part of the record is not an
		 * ampersand or dollar sign delimiting a namelist
		 * group name.
		 */
		if ((cup->ufnl_skip != 0) ||
		    (cup->ufcompat == AS_IRIX_F77) ||
		    (cup->ufcompat == AS_IRIX_F90)) {
			cup->ulinecnt	= 0;
			goto skiprec;
		}
		errn	= FENLONEC;
		ERROR0(errf, css, errn);
	}
	/* save beginning character to check against ending char */
	endnmlchar	= c;

	/* get first character of namelist group name from input record */
	MAINGT(c);
	/* and get namelist group name from input record */
	errn	= _getname(css, cup, buf, &c);
	if (errn != 0)
		goto err_eof;
	/* convert group name to uppercase */
	_cnvrt_toupper(buf);

	assert ( (cup != NULL));
	rcount	= namlist->icount;	/* number of name table entries	*/
	rptr	= _fcdtocp(namlist->group_name); /* ptr to groupname	*/
	rlen	= _fcdlen(namlist->group_name);	/* len of groupname	*/
	nlvar	= namlist->goli;		/* group object ptr	*/

	if (strncmp(rptr,buf,rlen)) {
		if (cup->ufnl_skip == 0) {
			errn	= FENLIVGP;
			ERROR1(errf, css, errn, buf);
		}

		/* Skip unmatched namelist group. The slash terminates
		 * the f90 namelist input.
		 */
		while (c != '/') {

			/* check to see if old namelist ending (&END)
			 * is present rather than the slash in f90.
			 */
			if (c == '&' || c == '$') {

				/* check to see that beginning namelist
				 * group name delimiter matches the
				 * ending delimiter before END.
				 */
				if (c == endnmlchar) {

					/* get END, if present. */
#ifdef KEY /* Bug 14200 */
					/* Correct erroneous assumption in
					 * original code that a blank will
					 * follow '&end'. */
					char c_e, c_n, c_d;
					MAINGT(c_e);
					MAINGT(c_n);
					MAINGT(c_d);
					if (tolower(c_e) != 'e' ||
					  tolower(c_n) != 'n' ||
					  tolower(c_d) != 'd') {
					  errn = FERDNLEF;
					  ERROR1(errf, css, errn, buf);
					}
#else /* KEY Bug 14200 */
					do {
						MAINGT(c);
					} while (!ISBLANK(c));
#endif /* KEY Bug 14200 */
					goto rrd;
				}
			}

			/* check for delimited character string */
			if ((c == '\'') || (c == '"')) {
				char	qcr;
				qcr	= c;
rqte:
				do {
					MAINGT(c);
				} while (c != qcr);
				MAINGT(c);
				/* embedded double quote? */
				if (c == qcr)
					goto rqte;
			} else {
				CMTE_MAINGT(c);
			}
		}

		goto rrd;
	}
	/*
	 *	This is the correct namelist group name.  Process the
	 *	input record. Read until the input record or records
	 *	until the terminating character is found.  This is a
	 *	slash or ampersand or MRNLDELIM.
 	 */
	while (c != '/') {
		int	sepcnt;
		if (c == '&' || c == '$') {
			if (c != endnmlchar) {
				/* begin character did not match end char */
				errn	= FENLONEC;
				ERROR0(errf, css, errn);
			}
			else
				goto finalization;
		}
		/* get group_object_name from input record */
		errn	= _getname(css, cup, buf, &c);
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
		/*
		 * c is a '%' to indicate a structure component follows.
		 * Look for a component name to follow the percent sign.
		 */
		while (c == '%') {
			nmlist_struclist_t *nlstruc; /* nmlist struc entry */
			unsigned	scount;
			nmlist_goli_t	*vaddr;
			assert ((fdvar->valtype == IO_STRUC_A) || 
				(fdvar->valtype == IO_STRUC_S));
			if ((fdvar->valtype == IO_SCALAR) ||
			    (fdvar->valtype == IO_DOPEVEC)) {
				/* structure indicator in object name
				 * that is not a structure
				 */
				errn	= FENLNREC;
				ERROR1(errf, css, errn, buf);
			}
			/* input variable is a structure */
			nlstruc	= fdvar->goli_addr.sptr; /* ptr to struc */
			vaddr	= nlstruc->goli;	/* ptr to list */
			scount	= nlstruc->structlen;	/* number entries */
			/*
			 * Get the namelist object list name from the
			 * input record
			 */
			MAINGT(c);
			errn	= _getname(css, cup, buf, &c);
			if (errn != 0)
				goto err_eof;
			_cnvrt_toupper(buf);
			/*
			 * Find the matching namelist object list name
			 * for the object list name in the input record
			 */
			if (!(fdvar = _findname(buf, vaddr, scount))) {
				if (strlen(buf) > 0) {
					/* objectlistname in input record */
					errn	= FENLNREC;
					ERROR1(errf, css, errn, buf);
				}
				else {
					/* No name in input record */
					errn	= 0; /* empty variable entry */
					goto finalization;
				}
			}
		}
		/* we're positioned just after the object name
		 * so get following value(s)
		 */
		errn	= _getnlval(css, fdvar, &c, cup);
		if (errn != 0)
			goto err_eof;
		sepcnt	= 0;
		for ( ; ; ) {
			if (!(ISBLANK(c))) {
				if ((c == ',') && (sepcnt == 0)) {
					/* skip separator */
					sepcnt++;
				}
				else
					break;
			}
			CMTE_MAINGT(c);
		}
	}

/***************************************************************************
 *	Statement Finalization Section
 ***************************************************************************/
finalization:

	/* Set IOSTAT variable to 0 if no error, >0 error code otherwise */
	if (cilist->iostat_spec != NULL)
		*cilist->iostat_spec	= errn;

	/* End the Beguine */
	STMT_END(cup, TF_READ, NULL, css);	/* Unlock unit */

	/* Return proper status */
	if (errn == 0)
		return(IO_OKAY);
	else if (errn < 0) {
		cup->pnonadv	= 0;	/* no current record if EOF */
		return(IO_END);
	}
	return(IO_ERR);
err_eof:
	/* err and eof handling */
	if(errn < 0) {
		ENDD(endf, css, FERDPEOF);
	} else if (errn == FENLSTRN || errn == FENLSTRG ||
		   errn == FENLSUBD || errn == FENLSUBN ||
		   errn == FENLSUBS || errn == FENLLGNM ||
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
_nlrd_fillrec(FIOSPTR css, unit *cup)
{
	register int	errn;

	errn	= css->u.fmt.endrec(css, cup, 1);

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
 *	In looking for the name, we stop when we see a space, '=',
 *	'(', '%', or ending namelist delimiter ('&' or '$').
 */

static int
_getname(FIOSPTR css, unit *cup, char *s, char *lastc)
{
	char	*p, c;
	int	n, errn;
	errn 	= 0;
	n	= MAXNAML + 5; /* real*16 input can be 34 characters long */
	p	= s;
	c	= *lastc;
	/*
	 * Names cannot have embedded blanks.  In cf77 compatibility mode,
	 * a comment can immediately follow the name and terminates it.
	 * An unknown comment character may be nonstandard for Fortran 90.
	 * Allow the terminating ampersand for simpler non-f90 namelist
	 * compatibility.
	 */

	while (ISBLANK(c))
		CMTE_SUBGTC(c);
	while (!(ISBLANK(c)) && c != '(' && c != '=' && c != '/' &&
			 c != '&' && c != '%' && c != '$') {
		*p++	= c;
		CMTE_SUBGTCNOEOR(c);
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
		else {
			/* cannot do this in a switch since some word32
			 * systems do not have the extra padding.
			 */
#if (defined(__mips) && (_MIPS_SZLONG == 32)) || (defined(_LITTLE_ENDIAN) && !defined(_LP64))
			newitem	= (nmlist_goli_t*)((long *)newitem +
				3 + (sizeof(_fcd))/(sizeof(long)));
#else
			newitem	= (nmlist_goli_t*)((long *)newitem +
				2 + (sizeof(_fcd))/(sizeof(long)));
#endif
		}
	}
	return (NULL);
}

#ifdef KEY /* Bug 11889 */
/*
 * dvdm		Bounds of array
 * dimnsn	Bounds of array reference
 * ndim		Rank of array
 * return	1 if array reference obeys Fortran in-bounds rules, else 0
 */
static int
bounds_ok(struct DvDimen *dvdm, struct DvDimen *dimnsn, int ndim) {
			
  int nc;
  for (nc = 0; nc < ndim; nc++) {
    if (dvdm[nc].extent) {
      int lb = dimnsn[nc].low_bound;
      int ext = (dimnsn[nc].stride_mult < 0) ?
	(-dimnsn[nc].extent) :
        dimnsn[nc].extent;
      int glb = dvdm[nc].low_bound;
      if (dimnsn[nc].stride_mult < 0) {
        if (lb >= (glb + dvdm[nc].extent) || (lb - ext + 1) < glb) {
	  return 0;
	}
      } else {
	if (lb < glb || ((lb + ext) > (glb + dvdm[nc].extent))) {
	  return 0;
	}
      }
    }
  }
  return 1;
}
#endif /* KEY Bug 11889 */

/* _getnlval - get values for namelist io
 *
 * On entry:
 *	- positioned after variable name
 *	- lastc contains the character following the name
 * On exit:
 *	- *lastc contains the character following the value
 *	- cup->ulineptr is pointing to the character following lastc
 * 	- returns: 0 if successful
 *		  -value if EOF detected
 *		  > 0 if error detected
 */

static int
_getnlval(FIOSPTR css, nmlist_goli_t *nlvar, char *lastc, unit *cup)
{
	long		cntp	= 0;
	int		i;
	int		ndim	= 0;
	int		encnt	= 0;
	int		icnt	= 0;
	long		strbegend[3];
	char		*cp;
	char		c;
	long		vaddr;
	long		errn	= 0;
	struct DvDimen	dimnsn[MAXDIM];
	struct DvDimen	*dvdn = dimnsn;
	/* find offset if indexed array */
	/* clear array element, section, and substring information */
	for (i=0; i < MAXDIM; i++) {
		dimnsn[i].stride_mult	= 0;
		dimnsn[i].extent	= 0;
		dimnsn[i].low_bound	= 0;
	}
	strbegend[0]	= -1;	/* flag indicating string */
	strbegend[1]	= -1;	/* string begin		*/
	strbegend[2]	= -1;	/* string end 		*/

	switch (nlvar->valtype) {
	case IO_SCALAR:
	{
		nmlist_scalar_t *nlscalar; /* nmlist scalar entry */
		unsigned long	elsize;
		unsigned int	int_len;
		void		*vaddr;
		ftype_t		type;	/* fortran data type */
		nlscalar	= nlvar->goli_addr.ptr; /* ptr to scalar */
		type		= nlscalar->tinfo.type;
		int_len		= nlscalar->tinfo.int_len;
		/* Assertions */
		assert (type >= DVTYPE_TYPELESS && type <= DVTYPE_ASCII);
		assert(nlscalar->tinfo.int_len > 0 );
		if ((type != DVTYPE_ASCII) && (*lastc == '(')) {
			errn	= FENLUNKI;
			break;
		}
		if (type == DVTYPE_ASCII)
			strbegend[0]	= 0;
		if (*lastc == '(') {
			errn	= _indx_nl(css, cup, dvdn, &ndim, strbegend,
				&encnt, &icnt, 0);
			if (errn != 0) {
				if (errn == FENLSUBS)
					errn	= FENLSTRG;
				else if (errn == FENLSUBN)
					errn	= FENLSTRN;
				break;
			}
		} else {
			while (ISBLANK(*lastc)) {
				CMTE_SUBGTC(*lastc);
			}
			/* namelist is terminated by slash, ampersand, or $ */
			if ((*lastc == '/') || (*lastc == '&') || (*lastc == '$')) {
				errn	= 0;
				break;
			}
			/* Not a value here. */
			if (*lastc != '=') {
				errn	= FENLNOVL;
				break;
			}
		}

		/* Currently positioned after the '=' sign, but lastc is
		 * pointing at the '=' sign.  Update lastc for nlread and
		 * compute:
		 * cntp = number of array elements to be read
		 *	(1 if not an array).
		 * elsize = size of a variable or array element
		 *	(words for nonchar, bytes for char).
		 * vaddr = target address for input value.  For character,
	 	 *	this is a Fortran character descriptor.
	 	 */
		CMTE_SUBGTC(*lastc);
		if (type == DVTYPE_ASCII) {
			char	*wptr;
			const int bytesperchar = 1;
			long	begt	= strbegend[1];
			long	endt	= strbegend[2];
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
		errn	= _nlread(css, type, cup, vaddr, elsize, cntp, 0, &c);
		*lastc	= c;
		break;
	}
	case IO_DOPEVEC:
	{
		DopeVectorType	*nldv;
		ftype_t		type;	/* fortran data type */
		nldv	= nlvar->goli_addr.dv; /* ptr to dope vector */
		/* Assertions */
		assert ( nldv != NULL );
		assert ( nldv->type_lens.int_len > 0 );
		type	= nldv->type_lens.type;
		if (type == DVTYPE_ASCII)
			strbegend[0]	= 0;
		for (i=0; i < nldv->n_dim; i++) {
			dimnsn[i].stride_mult	= nldv->dimension[i].stride_mult;
			dimnsn[i].extent	= nldv->dimension[i].extent;
			dimnsn[i].low_bound	= nldv->dimension[i].low_bound;
		}
		if (*lastc == '(') {
			errn	= _indx_nl(css, cup, dvdn, &ndim, strbegend,
				&encnt, &icnt, 1);
			if (errn != 0)
				break;
		} else {
			while (ISBLANK(*lastc)) {
				CMTE_SUBGTC(*lastc);
			}
			/* namelist is terminated by slash, ampersand, or $ */
			if ((*lastc == '/') || (*lastc == '&') || (*lastc == '$')) {
				errn	= 0;
				break;
			}
			/* Not a value or structure qualification here. */
			if (*lastc != '=') {
				errn	= FENLNOVL;
				break;
			}
		}

		/* Currently positioned after the '=' sign, but lastc is
		 * pointing at the '=' sign.  Update lastc for nlread and
		 * compute:
		 * cntp = number of array elements to be read
		 *	(1 if not an array).
		 * elsize = size of a variable or array element
		 *	(words for nonchar, bytes for char).
		 * vaddr = target address for input value.  For character,
	 	 *	this is a Fortran character descriptor.
	 	 */
		CMTE_SUBGTC(*lastc);
		if ((ndim != 0) && (ndim != nldv->n_dim)) {
			errn	= FENLBNDY;
			break;
		}

		/* call nlread directly for array elements. */
		if (ndim != 0) {
			struct DvDimen	*dvdm = nldv->dimension;
			void		*vaddr;
			long		extent = 1;
			long		elsize;
			long		mult = 1;
			long		offs = 0;
			long		incrmt;
			int		int_len = nldv->type_lens.int_len;
			register long	nc;
			for (nc = 0; nc < nldv->n_dim; nc++) {
				extent *= dvdm[nc].extent;
			}

			/* array element. */
			if (encnt == 0 && icnt == 0) {
#ifdef KEY /* Bug 11889 */
                                if (!bounds_ok(dvdm, dimnsn, ndim)) {
				  return(FENLBNDY);
				}
#endif /* KEY Bug 11889 */
				offs	= dimnsn[0].low_bound - (dvdm[0].low_bound);
				incrmt	= 1;
				for (nc = 1; nc < ndim; nc++) {
					mult	= mult * (dvdm[nc-1].extent);
					offs	= offs +
					       ((dimnsn[nc].low_bound -
					   dvdm[nc].low_bound) * mult);
				}
				extent	= extent - offs;
				if (type == DVTYPE_ASCII) {
					char		*wptr;
					const int	 bytesperchar = 1;
					long	begt = strbegend[1];
					long	endt = strbegend[2];
					wptr =
					  _fcdtocp(nldv->base_addr.charptr);
					elsize	=
					   _fcdlen(nldv->base_addr.charptr);
					elsize	= elsize * bytesperchar;
					/* check for character
					 * substrings in input record.
					 */
					wptr += offs * elsize;

					if (strbegend[0] > 0) {
						if (begt < 1 )
							begt	= 1;
						else if (begt > elsize) {
							errn	= FENLUNKN;
							break;
						}
						if (endt < 1 )
							endt	= elsize;
						else if ((endt >
							elsize) ||
							(endt < begt)) {
							errn	= FENLUNKN;
							break;
						}
						wptr	= wptr + (begt - 1);
						elsize	= (endt - begt) + 1;
					}

					vaddr	= wptr;
				} else {
					bcont	*iwptr;
					iwptr	= (bcont*)nldv->base_addr.a.ptr;
					elsize	= int_len >> 3;
					iwptr  += offs * (elsize /
						(sizeof(bcont)));
					vaddr	= iwptr;
				}
				/* Assertions */
				assert ( elsize > 0 && extent > 0 );
				c	= *lastc;
				cntp	= extent;
				errn	= _nlread(css, type, cup, vaddr,
						elsize, cntp, incrmt, &c);
				*lastc	= c;
			} else {
#ifdef KEY /* Bug 11889 */
                                if (!bounds_ok(dvdm, dimnsn, ndim)) {
				  return(FENLBNDY);
				}
#endif /* KEY Bug 11889 */
				for (nc = 0; nc < ndim; nc++) {
#ifndef KEY /* Bug 11889 */
					if (dimnsn[nc].extent !=
						dvdm[nc].extent) {
						if (dimnsn[nc].extent >
						    dvdm[nc].extent) {
							return(FENLBNDY);
						}
					}
#endif /* KEY Bug 11889 */
#ifdef KEY /* Bug 11635 */
				   /* Multiply stride for consecutive elements
				    * of the array times stride for the section
				    * of the array. Not clear why original
				    * author wanted to skip this. For example,
				    * for an i*8 array, the consecutive-element
				    * stride is 2; and if the section stride
				    * is also 2, we want the resulting stride
				    * to be 4 (skipping every other element)
				    * even though 2 == 2.
				    *
				    * Note also the weird (but correct) code
				    * above: "if (x!=y) if (x > y) ..."; it
				    * looks like something is missing, but
				    * no problems have surfaced yet.
				    */
						dimnsn[nc].stride_mult =
						  dimnsn[nc].stride_mult *
						  dvdm[nc].stride_mult;
#else /* KEY Bug 11635 */
					if (dimnsn[nc].stride_mult !=
					    dvdm[nc].stride_mult) {
						dimnsn[nc].stride_mult =
						  dimnsn[nc].stride_mult *
						  dvdm[nc].stride_mult;
					}
#endif /* KEY Bug 11635 */
				}
				c	= *lastc;
				errn	= _nl_stride_dv(css, cup, nldv,
						dvdn, &c, strbegend);
				*lastc	= c;
			}
			
		/* call nlread directly for noncharacter whole arrays */
		} else if (type != DVTYPE_ASCII) {
			int		n_dm = nldv->n_dim;
			unsigned long	elsize = nldv->type_lens.int_len >> 3;
			unsigned long	extent = nldv->dimension[0].extent;
			struct DvDimen	*dvdm = nldv->dimension;
			long		incrmt;
	
			if (n_dm != 1) {
				register long	nc;
				if (n_dm == 2) {
					if (dvdm[0].stride_mult * extent !=
					    dvdm[1].stride_mult)
						goto gen_dv_process;
					extent *= dvdm[1].extent;
				} else if (n_dm == 0) {
					extent	= 1;
				} else {
					for (nc = 0; nc < (n_dm-1); nc++) {
						register int st	=
						   dvdm[nc].stride_mult;
						register int ex	=
						   dvdm[nc].extent;
						if ( (st * ex) !=
						   dvdm[nc+1].stride_mult)
							goto gen_dv_process;
						extent *= dvdm[nc+1].extent;
					}
				}
			}
			if (extent > 1) {
				register long sm =
					nldv->dimension[0].stride_mult;
				if (sm * (signed)SMSCALE(nldv) == elsize)
					incrmt	= 1;
				else {
					int bytes_per_sm	= sm *
						(signed)SMSCALE(nldv);
					incrmt	= bytes_per_sm / elsize;
					/* if stride not a multiple of size... */
					if (elsize * incrmt != bytes_per_sm)
						goto gen_dv_process;
				}
			} else
				incrmt	= 0;

			/* Assertions */
			assert ( elsize > 0 && extent > 0 );
			c	= *lastc;
			errn	= _nlread(css, type, cup,
					nldv->base_addr.a.ptr, elsize, extent,
					incrmt, &c);
			*lastc	= c;
		} else {
gen_dv_process:
			c	= *lastc;
			errn	= _nl_stride_dv(css, cup, nldv, 0, &c, strbegend);
			*lastc	= c;
		}
		break;
	}
	case IO_STRUC_A:
	{
		nmlist_struclist_t *nlstruc; /* nmlist struc entry */
		unsigned long	elsize;
		unsigned int	int_len;
		unsigned int	scount;
		char		*cp;
		nmlist_goli_t	*vaddr;
		ftype_t		type;	/* fortran data type */
		int		byt = 0; /* scalar struct has 0 offset */
		nlstruc	= nlvar->goli_addr.sptr; /* ptr to structure. */
		vaddr	= nlstruc->goli;		/* ptr to list. */
		scount	= nlstruc->structlen;	/* number of entries. */
		if (*lastc == '(') {
			/* This is not an array or substring - err */
			errn	= FENLUNKI;
			break;
		} else {
			while (ISBLANK(*lastc)) {
				CMTE_SUBGTC(*lastc);
			}
			/* namelist terminated by slash, ampersand, or $ */
			if ((*lastc == '/') || (*lastc == '&') ||
			    (*lastc == '$')) {
				errn	= 0;
				break;
			}
			/* Check for structure qualification. */
			if (*lastc == '%') {
				errn	= FENLIOER;
				break;
			} else if (*lastc != '=') {
				errn	= FENLNOVL;
				break;
			}
		}

		/* Currently positioned after the '=' sign, but lastc is
		 * pointing at the '=' sign.  Update lastc for nlread and
		 * compute:
		 * cntp = number of array elements to be read
		 *	(1 if not an array).
		 * elsize = size of a variable or array element
		 *	(words for nonchar, bytes for char).
		 * vaddr = target address for input value.  For character,
	 	 *	this is a Fortran character descriptor.
	 	 */
		CMTE_SUBGTC(*lastc);
		cp	= lastc;
		errn	= _nlrdent(css, cup, vaddr, scount, cp, byt);
		*lastc	= *cp;
		break;
	}
	case IO_STRUC_S:
	{
		nmlist_struclist_t *nlstruc; /* nmlist struc entry */
		unsigned long	elsize;
		unsigned int	int_len;
		unsigned int	scount;
		int		nc;
		long		ic;
		char		*cp;
		long		extnt = 1;
		nmlist_goli_t	*vaddr;
		DopeVectorType	*nlsdv;
		ftype_t		type;	/* fortran data type */
		int		byt = 0; /* arraystruct offset in bytes */
		unsigned int	compflag = 0;
		nmlist_goli_t	*fdvar;
		char		abuf[MAXNAML + 5];
		nlstruc	= nlvar->goli_addr.sptr; /* ptr to struc */

		/* number of entries */
		scount	= nlstruc->structlen;

		/* ptr to list */
		vaddr	= nlstruc->goli;
		fdvar	= nlvar;

		/* ptr to dope vector */
		nlsdv	= nlstruc->struc_addr.dv;
		elsize	= nlsdv->base_addr.a.el_len;
		type	= nlsdv->type_lens.type;

		for (i=0; i < nlsdv->n_dim; i++) {
			dimnsn[i].stride_mult	= nlsdv->dimension[i].stride_mult;
			dimnsn[i].extent	= nlsdv->dimension[i].extent;
			dimnsn[i].low_bound	= nlsdv->dimension[i].low_bound;
		}
		if (*lastc == '(') {
			errn	= _indx_nl(css, cup, dvdn, &ndim, strbegend,
				&encnt, &icnt, 1);
			if (errn != 0)
				break;
		} else {
			while (ISBLANK(*lastc)) {
				CMTE_SUBGTC(*lastc);
			}
			/* namelist terminated by slash, ampersand, or $ */
			if ((*lastc == '/') || (*lastc == '&') ||
			    (*lastc == '$')) {
				errn	= 0;
				break;
			}
		}

		/* Currently positioned after the '=' sign, but lastc is
		 * pointing at the '=' sign.  Update lastc for nlread and
		 * compute:
		 * cntp = number of array elements to be read
		 *	(1 if not an array).
		 * elsize = size of a variable or array element
		 *	(words for nonchar, bytes for char).
		 * vaddr = target address for input value.  For character,
	 	 *	this is a Fortran character descriptor.
	 	 * 
		 * byt is used when the structure is an array of
		 * structures.  Each component must add an offset to
		 * its base address after the first array element.
		 * With derived types, the bits must be changed to bytes.
		 */
		CMTE_SUBGTC(*lastc);
		if ((ndim != 0) && (ndim != nlsdv->n_dim)) {
			errn	= FENLBNDY;
			break;
		}
		/* Check for structure qualification. */
		while (*lastc == '%') {
			compflag++;
			nlstruc	= fdvar->goli_addr.sptr;
			vaddr	= nlstruc->goli;
			scount	= nlstruc->structlen;
			/* Check for structure qualification. */
			SUBGTC(*lastc);
			errn	= _getname(css, cup, abuf, lastc);
			if (errn != 0)
				break;
			_cnvrt_toupper(abuf);
			/* find matching namelist object list
			* name in input record
			*/
			if (!(fdvar = _findname(abuf, vaddr, scount))) {
				if (strlen(abuf) > 0) {
				/* objectlistname in record */
					errn	= FENLNREC;
					break;
				} else {
				/* no name in record. May be
				 * empty record. Quit process.
				 */
				errn	= 0;
				break;
				}
			} else
				vaddr	= fdvar;
			while (ISBLANK(*lastc)) {
				CMTE_SUBGTC(*lastc);
			}
			if (*lastc != '=') {
				errn	= FENLNOVL;
				break;
			}
			CMTE_SUBGTC(*lastc);
		}
		if (ndim != 0) {
			struct DvDimen	*dvdm = nlsdv->dimension;
			long		mult = 1;
			long		offs = 0;
			register long	nc;
			for (nc = 0; nc < nlsdv->n_dim; nc++)
				extnt *= nlsdv->dimension[nc].extent;
			/* array element. */
			if (encnt == 0 && icnt == 0) {
				offs	= dimnsn[0].low_bound - (dvdm[0].low_bound);
				for (nc = 1; nc < ndim; nc++) {
					mult	= mult * (dvdm[nc-1].extent);
					offs	= offs +
					   ((dimnsn[nc].low_bound -
					   dvdm[nc].low_bound) * mult); 
				}
				extnt	= extnt - offs;
				elsize	= elsize >> 3;
				byt	= offs * elsize;
				assert ( elsize > 0 && extnt > 0);
				cp	= lastc;
				if (compflag)
					scount	= 1;
				errn	= _nlrdent(css, cup, vaddr, scount,
						cp, byt);
				*lastc	= *cp;
			} else {
				for (nc = 0; nc < ndim; nc++) {
					if (dimnsn[nc].extent !=
					    dvdm[nc].extent) {
						if (dimnsn[nc].extent >
						   dvdm[nc].extent) {
							return(FENLBNDY);
						}
					}
					if (dimnsn[nc].stride_mult !=
					   dvdm[nc].stride_mult) {
						dimnsn[nc].stride_mult =
						   dimnsn[nc].stride_mult *
						   dvdm[nc].stride_mult;
					}
				}
				cp	= lastc;
				if (compflag)
					scount	= 1;
				errn	= _nl_strd_derv(css, cup, nlsdv, dvdn,
						cp, vaddr, scount, byt);
				*lastc	= *cp;
			}
		} else {
			cp	= lastc;
			errn	= _nl_strd_derv(css, cup, nlsdv, 0, cp,
					vaddr, scount, byt);
			*lastc	= *cp;
		}
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
 *	On Exit - lastc  will contain the first nonblank, nonseparator
 * 		character following the value.
 */

static int
_nlread(FIOSPTR css, ftype_t type, unit *cup, void *ptr, long elsize,
	int cntp, int incrm, char *lastc)
{
	/* bug 10772: Enforce strictest alignment on lval */
 	long double	lval[5];	/* convert space */
	long		ss, ncntp;
	long		stat;
	char		c;
	void		*vaddr;
	long		errn = 0;
	int		lcount;		/* repeat count for values */
	bcont		*sval;
	int		nullvlu;
	c	= *lastc;
	ncntp	= cntp;
	vaddr	= ptr;
	nullvlu	= 0;

	while (ncntp > 0) {
		errn	= _nexdata(css, type, vaddr, ncntp, 1, c, cup,
				(long *) lval, &lcount, elsize, &nullvlu);
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
#ifdef KEY /* Bug 11635 */
			/* Stride may be 0 (for scalar), pos, or neg */
                        wptr += incrm ? (elsize * (incrm - 1)) : 0;
#endif /* KEY Bug 11635 */
			vaddr	= wptr;
		}
		else {
			int move;
			int *iptr;
			int ix, lim;
			bcont *siptr;
#ifdef KEY /* Bug 11635 */
			/* stride may be zero (for scalar), positive,
			 * or negative */
			long abs_incrm = (incrm > 0) ? incrm : (-incrm);
			long ncntp_tmp = abs_incrm ?
			  ((ncntp + abs_incrm + 1) / abs_incrm) :
			  ncntp;
			move	= MIN(ncntp_tmp,lcount);
			lim	= elsize/(sizeof(bcont));
			long extra_dest_stride =
			  incrm ? (lim * (incrm - 1)) : 0;
#else /* KEY Bug 11635 */
			lim	= elsize/(sizeof(bcont));
			move	= MIN(ncntp,lcount);
#endif /* KEY Bug 11635 */
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
#ifdef KEY /* Bug 11635 */
                                ncntp -= abs_incrm ? abs_incrm : 1;
				siptr = (vaddr += extra_dest_stride);
#else /* KEY Bug 11635 */
				ncntp--;
#endif /* KEY Bug 11635 */
				lcount--;
			}
		}
		do {
			CMTE_SUBGTC(*lastc);
		} while (ISBLANK(*lastc));
		if (*lastc == ',') {
			do {
				CMTE_SUBGTC(*lastc);
			} while (ISBLANK(*lastc));
		}
	c	= *lastc;
	}
	return(0);
}

/*	_indx_nl	compute the dimension information of an
 *			indexed array in the input record.
 *	On entry:
 *		_ positioned just after the '('
 *	On exit:
 *		- returns:	0 on success
 *				-value on eof
 *		- positioned just after the '='
 *		- if % occurred, the scan is backed up one
 *		- the lastc argument is not changed
 */

static int
_indx_nl(
	FIOSPTR css, unit *cup, struct DvDimen *dvdn, int *ndima,
	long strbegend[3],int *encnt, int *icnt, int arryflag)
{
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
#ifdef KEY /* Bug 8046 */
			int orig_low_bound = dvdn[i].low_bound;
#endif /* KEY Bug 8046 */
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
			dvdn[i].low_bound	= *((_f_int8 *)tempbuf);
indxgetext:
			/* point beyond subscript or lowbound. */
			cup->ulineptr	= field_begin + field_width;
			cup->ulinecnt	= cup->ulinecnt - field_width;

			/* Get extent subscript information */
			if (c == ':') {
				/* update ulineptr */
				SUBGTC(c);
				GETSECTION(c);
#ifdef KEY /* Bug 8046 */
				/* When there's a ":" after low bound,
				 * this is a section, not an element */
				en1++;
				if (field_width == 0) {
					/* No explicit upper bound, so extent
					 * is reduced by change (if any) in
					 * low bound */
					dvdn[i].extent -=
					  (dvdn[i].low_bound - orig_low_bound);
					goto indxgetinc;
				}
#else /* KEY Bug 8046 */
				if (field_width == 0)
					goto indxgetinc;
#endif /* KEY Bug 8046 */
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
				/* calculate extent from upper bound
				 * (upperbound - lowerbound) + 1
				 */
#ifdef KEY /* Bug 8046, 11635 */
				/* Array section might stride downward: Cray
				 * dope vector extents are never negative,
				 * but strides may be */
				long extent_tmp = (*((_f_int8 *)tempbuf) -
						   dvdn[i].low_bound);
				extent_tmp =
				  (extent_tmp < 0) ? (-extent_tmp) : extent_tmp;
				dvdn[i].extent = extent_tmp + 1;
#else /* KEY Bug 8046, 11635 */
				dvdn[i].extent	= (*((_f_int8 *)tempbuf) -
						   dvdn[i].low_bound) + 1;
				en1++;
#endif /* KEY Bug 8046, 11635 */
indxgetinc:
				/* point beyond subscript extent. */
				cup->ulineptr	= field_begin + field_width;
				cup->ulinecnt	= cup->ulinecnt - field_width;

				/* Get stride_mult subscript information */
				if (c == ':') {
					/* update ulineptr */
					SUBGTC(c);
					GETSECTION(c);
					if (field_width == 0)
						goto indxforloop;
					/* pass field_end + 1 */
					field_end++;
					tempbuf[0]	= 0;
					tempbuf[1]	= 0;
					mode		= 0;
					(void) _iu2s(field_begin,
						&field_width, &field_end,
						&mode, tempbuf, &stat,
						&dummy, &dummy);
					if(stat < 0) {
						errn	= FENLSUBS;
						return(errn);
					}
					dvdn[i].stride_mult	= *((_f_int8 *)tempbuf);
					ir1++;
indxforloop:
					/* point beyond subscript stride_mult. */
					cup->ulineptr	= field_begin + field_width;
					cup->ulinecnt	= cup->ulinecnt - field_width;
				}
#ifdef KEY /* Bug 11635 */
				/* No ":" after extent implies stride == 1 */
				else {
					dvdn[i].stride_mult = 1;
				}
#endif /* KEY Bug 11635 */
			}
#ifdef KEY /* Bug 8046 */
			/* No ":" after low bound implies extent, stride == 1 */
                        else {
				dvdn[i].extent = 1;
				dvdn[i].stride_mult = 1;
			}
#endif /* KEY Bug 8046 */
			/* increment the number of subscripts */
			i++;
			do {
				SUBGTC(c);	/* get to ',' or ')' */
			} while (ISBLANK(c));	/* NO EOR allowed here */
			/* check for end of subscripts */
			if (c == ')')
				break;
			if (c != ',') {
				errn	= FENLSUBD;	/* Not a comma */
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
			/* skip leading blanks in input here */
			do {
				SUBGTC(c);
			} while (ISBLANK(c));
			/* End of subscripts found in input record? */
			if (c == ')') {
				errn	= FENLSTRN;	/* null index */
				return(errn);
			}
			cup->ulinecnt++;
			cup->ulineptr--;
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
				/* skip leading blanks in input here */
				do {
					SUBGTC(c);
				} while (ISBLANK(c));
				/* End of subscripts found in input rec */
				if (c == ')')
					goto indxstrout;
				cup->ulinecnt++;
				cup->ulineptr--;
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
indxstrout:
			strbegend[0]	= j;
		}
	}
	/*
	 * Look for the equal sign or the structure qualification
	 * character
	 */
	while (!(c == '=') && !(c == '%')) {
		SUBGTC(c);
	}
	if (c == '%') {
		cup->ulineptr--;
		cup->ulinecnt++;
	}
	return(errn);
}

/* Converts the string in buf to upper case letters */

static void
_cnvrt_toupper(char *buf)
{
	char	c;
	while ((c = *buf) != '\0') {
		*buf++	= toupper(c);
	}
	return;
}

/*
 *	_nlrdent - namelist input of structure entries
 *		Recursive call to handle structure table entries for
 *		namelist.
 *	Return value:
 *		0 on success.  lval contains result.
 *			lcount contains repeat count.
 *		>0 error code if error encountered
 */

static int
_nlrdent(FIOSPTR css, unit *cup, nmlist_goli_t *nalist, unsigned count,
	char *lastc, int byt)
{
	char	c, oc;
	int	ocnt, ss;
	long	*optr;
	unsigned	scnt;	/* count of namelist struc items */
	nmlist_goli_t	*nlvar;	/* ptr to NEXT Var entry */
	int		errn;	/* error number */
	int		cntp;
	c	= *lastc;
	scnt	= count;
	errn	= 0;
	nlvar	= nalist;	/* group object pointer */

	while (scnt--) {
		switch(nlvar->valtype) {
		case IO_SCALAR:
		{
			nmlist_scalar_t *nlscalar; /* nmlist scalar entry */
			unsigned long	elsize;
			unsigned int	int_len;
			void		*vaddr;
			ftype_t		type;	/* fortran data type */
			int		adj = 0;
			cntp	= 1;
			nlscalar = nlvar->goli_addr.ptr; /* ptr to scalar */
			type	= nlscalar->tinfo.type;
			int_len	= nlscalar->tinfo.int_len;
			/* Assertions */
			assert (type >= DVTYPE_TYPELESS &&
					type <= DVTYPE_ASCII);
			assert(nlscalar->tinfo.int_len > 0 );
			if (type == DVTYPE_ASCII) {
				char	*wptr;
				const int bytesperchar = 1;
				wptr =
				   _fcdtocp(nlscalar->scal_addr.charptr) +
				   byt;
				elsize =
				   _fcdlen(nlscalar->scal_addr.charptr);
				elsize	= elsize * bytesperchar;
				/* Any character substring in input record */
				vaddr	= wptr;
			}
			else {
				if (byt > 0)
					adj	= byt/(sizeof(bcont));
				vaddr	= ((bcont*)nlscalar->scal_addr.ptr) +
						adj;
				elsize	= int_len >> 3;
			}
			errn	= _nlread(css, type, cup, vaddr, elsize,
					cntp, 0, &c);
			if (errn != 0)
				return(errn);
			*lastc	= c;
			break;
		}
		case IO_DOPEVEC:
		{
			DopeVectorType	*nldv;
			unsigned long	elsize;
			unsigned long	extent = 1;
			unsigned int	int_len;
			void		*vaddr;
			int		nc;
			ftype_t		type;	/* fortran data type */
			int		adj = 0;
			nldv	= nlvar->goli_addr.dv; /* ptr to dope vector */
			/* Assertions */
			assert ( nldv != NULL );
			assert ( nldv->type_lens.int_len > 0 );
			type	= nldv->type_lens.type;
			int_len	= nldv->type_lens.int_len;
			if (type == DVTYPE_ASCII) {
				char	*wptr;
				const int	bytesperchar = 1;
				wptr	= _fcdtocp(nldv->base_addr.charptr) +
					byt;
				elsize	= _fcdlen(nldv->base_addr.charptr);
				elsize	= elsize * bytesperchar;
				vaddr	= wptr;
			}
			else {
				if (byt > 0)
					adj	= byt/(sizeof(bcont));
				vaddr	= ((bcont*)nldv->base_addr.a.ptr) + adj;
				elsize	= int_len >> 3;
			}
			for (nc = 0; nc < nldv->n_dim; nc++) {
				extent *= nldv->dimension[nc].extent;
			}
			/* Assertions */
			assert ( elsize > 0 && extent > 0 );
			cntp	= extent;
			errn	= _nlread(css, type, cup, vaddr, elsize,
					cntp, 1, &c);
			if (errn != 0)
				return(errn);
			*lastc	= c;
			break;
		}
		case IO_STRUC_A:
		{
			nmlist_struclist_t *nlstruc; /* nmlist struc entry */
			unsigned long	elsize;
			unsigned int	int_len;
			unsigned int	scount;
			nmlist_goli_t	*vaddr;
			ftype_t		type;	/* fortran data type */
			int		bytoff;
			nlstruc	= nlvar->goli_addr.sptr; /* ptr to struc */
			scount	= nlstruc->structlen;	/* number entries */
			vaddr	= nlstruc->goli;	/* ptr to list */
			/*
			 * No additional offset needed, pass current
			 * offset on to next version.
			 */
			bytoff	= byt;
			errn =
			   _nlrdent(css, cup, vaddr, scount, &c, bytoff);
			if (errn != 0)
				return(errn);
			*lastc	= c;
			break;
		}
		case IO_STRUC_S:
		{
			nmlist_struclist_t *nlstruc; /* nmlist struc entry */
			unsigned long	elsize;
			unsigned int	int_len;
			unsigned int	scount;
			int		nc;
			long		ic;
			long		extnt=1;
			nmlist_goli_t	*vaddr;
			DopeVectorType	*nlsdv;
			ftype_t		type;	/* fortran data type */
			int		bytoff;
			nlstruc	= nlvar->goli_addr.sptr; /* ptr to struc */
			scount	= nlstruc->structlen;	/* number entries */
			vaddr	= nlstruc->goli;	/* ptr to list */
			nlsdv	= nlstruc->struc_addr.dv; /* ptr to dopevec */
			/*
			 * byt is used when the structure is an array
			 * of structures.  Each element must add an offset
			 * to its address after the first array element.
			 */
			elsize	= nlsdv->base_addr.a.el_len;
			for (nc = 0; nc < nlsdv->n_dim; nc++) {
				extnt *= nlsdv->dimension[nc].extent;
			}
			for (ic = 0; ic < extnt; ic++) {
				/*
				 * create another byte offset for this
				 * nesting of a structure of arrays.  Must
				 * change elsize from bits to bytes.
				 */
				bytoff	= byt + ((elsize >> 3) * ic);
				errn	= _nlrdent(css, cup, vaddr, scount,
					&c, bytoff);
				if (errn != 0)
					return(errn);
			}
			*lastc	= c;
			break;
		}
		default:
			errn	= FEINTUNK;
		}
		if (errn !=0)
			return(errn);
#if (defined(__mips) && (_MIPS_SZLONG == 32)) || (defined(_LITTLE_ENDIAN) && !defined(_LP64))
		nlvar	= (nmlist_goli_t*)((long *)nlvar + 3 +
			(sizeof(_fcd))/(sizeof(long)));
#else
		nlvar	= (nmlist_goli_t*)((long *)nlvar + 2 +
			(sizeof(_fcd))/(sizeof(long)));
#endif
	}
	return(errn);
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
	long		*lval,	/* Value is placed here */
	int		*lcount, /* Repeat count is returned here */
	long		elsize,
	int		*nullvlu) /* indicate if any nulls returned */
{
	char	c, oc;
	int	ocnt;
	long	*optr;
	int	holcnt;		/* Length of hollerith string */
	char	newc;
	int	errn;
	*nullvlu	= 0;
	c	= lastc;
	while (ISBLANK(c)) {
		CMTE_SUBGTC(c);
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
			/* get next character */
			/* blank character if end-of-record */
			SUBGTCNOEOR(c);
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
			/* get next character */
			/* blank character if end-of-record */
			CMTE_SUBGTCNOEOR(c);
			if (isdigit((int) c)) {
				/* See if we have a repeat count followed
				 * by hollerith, like 3*4Habcd
				 */
				holcnt	= c - '0';
				ocnt	= cup->ulinecnt;
				optr	= cup->ulineptr;
				oc	= c;
				for (;;) {
					/* blank character if end-of-record */
					SUBGTCNOEOR(c);
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
						type, lval, elsize));
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
			return(_get_holl(css, cup, c, holcnt, type,
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
	if (c == ',') {
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
		if (c == '!') {
			/* use this path with input like:  a = 5,!comment */
			cup->ulineptr--; /* reset cnt and ptr so */
			cup->ulinecnt++; /* we can read separator again */
			*nullvlu	= 1;
			return(0);	/* return null value */
		} else
			if (c == '/' || c == '&' || c == '$') {
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
		slval	= (bcont *)lval;

		/* Looking for a logical value.  Logical values must be of
		 * the form: optional decimal point, followed by a 'T' for
		 * true or an 'F' for false, optionally followed by one
		 * or more additional characters.  Those additional
		 * characters cannot include '=', ',', ':', ';', '(', '$'
		 * or '&'.
		 */
		if (c == '.') {
			/* blank character if end-of-record */
			SUBGTCNOEOR(c);
			/* .T or .t assumed to be a logical value */
			if ((c == 'T') || (c == 't')) {
				switch (elsize) {
#ifdef _F_INT4
				case 4:
					*(_f_log4 *)slval	= _btol(1);
					break;
/* KEY: This used to have __mips */
#ifdef _F_INT2
				case 2:
					*(_f_log2 *)slval	= _btol(1);
					break;
				case 1:
					*(_f_log1 *)slval	= _btol(1);
					break;
#endif	/* _F_INT2 */
#endif	/* _F_INT4 */
				case 8:
					*(_f_log8 *)slval	= _btol(1);
					break;
				default:
					return(FEKNTSUP); /* kind not supported */
				}

			/* F and .f are assumed to be a logical value */
			} else if ((c == 'F') || (c == 'f')) {
				switch (elsize) {
#ifdef _F_INT4
				case 4:
					*(_f_log4 *)slval	= _btol(0);
					break;
/* KEY: This used to have __mips */
#ifdef _F_INT2
				case 2:
					*(_f_log2 *)slval	= _btol(0);
					break;
				case 1:
					*(_f_log1 *)slval	= _btol(0);
					break;
#endif	/* _F_INT2 */
#endif	/* _F_INT4 */
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
			/* do not go beyond the end of the buffer */
			if (ocnt > 0) {
				newc	= *optr++;
				ocnt--;
				while (!(ISBLANK(newc))) {
				/* check for terminating or separator char */
					if (newc == ',' || newc == '/' ||
					    newc == '&' || newc == '$')
						break;
					if ((newc == '=') || (newc == '(') ||
					    (newc == '%')) {
					/* Reset, this MAY be the first
					 * letter of a variable name
					 */
						cup->ulineptr--;
						cup->ulinecnt++;
						*nullvlu	= 2;
						return(0); /* Null value */
					}
					if (ocnt <= 0)
						break;
					newc	= *optr++;
					ocnt--;
				}
				while ((ISBLANK(newc)) && ocnt-- > 0)
					newc	= *optr++;
				if (newc == '=') {
				/*
				 * Reset, because this MAY have been
				 * the first letter of a variable name
				 */
					cup->ulineptr--;
					cup->ulinecnt++;
					*nullvlu	= 2;
					return(0);	/* Null value */
				}
			}
			if ((c == 'T') || (c == 't')) {
				switch (elsize) {
#ifdef _F_REAL4
				case 4:
					*(_f_log4 *)slval	= _btol(1);
					break;
/* KEY: This used to have __mips */
#ifdef _F_INT2
				case 2:
					*(_f_log2 *)slval	= _btol(1);
					break;
				case 1:
					*(_f_log1 *)slval	= _btol(1);
					break;
#endif	/* _F_INT2 */
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
/* KEY: This used to have __mips */
#ifdef _F_INT2
				case 2:
					*(_f_log2 *)slval	= _btol(0);
					break;
				case 1:
					*(_f_log1 *)slval	= _btol(0);
					break;
#endif	/* _F_INT2 */
#endif
				case 8:
					*(_f_log8 *)slval	= _btol(0);
					break;
				default:
					return(FEKNTSUP); /* kind not supported */
				}
			}
			else if (ISBLANK(c) || c == ',') {
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
			CMTE_SUBGTCNOEOR(c);
			/* check for separator or terminating character */
			if (c == '/' || c == ',' || c == '&' || c == '$') {
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
		return (_g_charstr(css, cup, ptr, cnt, c, *lcount,
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
		return(_g_complx(css, cup, type, lval, elsize));
	}
	else if ((c == '\'') || (c == '"')) {
		return(_get_quoholl(css, cup, c, type, lval, elsize));
	}
	else if (c == 'O' || c == 'o') {
		return(_gocthex(css, cup, type, lval, OCTAL, elsize, nullvlu));
	}
	else if (c == 'Z' || c == 'z') {
		return(_gocthex(css, cup, type, lval, HEX, elsize, nullvlu));
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
	FIOSPTR css, unit*cup, ftype_t type, long *lval, long elsize)
{
	char	c;
	long	mode, stat;
	long	zero	= 0;
	long 	field_width;
	long	*field_begin;
	long	*field_end;
	int	i, errn;
	int	nc;
	ic_func *ngcf;
	int	inc;
	int	ptrfw;
	bcont	*slval;
	/*
	 * IN reading the complex number, assume
	 * intervening EOR is OK
	 */
	if (type != DVTYPE_COMPLEX) {
		errn	= FENLIVCX;	/* not complex type */
		return(errn);
	}
	/*
	 *	Call the function from the ncf_tab90 table.
	 */

	ngcf	= ncf_tab90[type];
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

	/* loop and get both real and imaginary */
	for (i = 0; i < 2; i++) {
		do {
			SUBGTC(c);	/* skip the '(' */
		} while (ISBLANK(c));	/* skip blanks */
		cup->ulinecnt++; 	/* backup 1 character */
		cup->ulineptr--; 	/* backup 1 character */
		field_begin	= cup->ulineptr;
		field_end	= cup->ulineptr;
		field_width	= cup->ulinecnt;
		nc		= 0;

		while (nc < cup->ulinecnt && !(ISSEP(*field_end) ||
		   *field_end == ')' || *field_end == '&' ||
		   *field_end == '$' )) {
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
			int	errn2;
			errn2	= _s_scan_extensions(slval + (i * inc),
				type, elsize, field_begin,
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
			return(FENLIVCX); /* err in cmplx no. form */
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
	long		elsize)
{
	long	mode, stat;
	long	zero = 0;
	long 	field_width;
	long	*field_begin;
	long	*field_end;
	int	ss = 0;
	int	errn = 0;
	int	nc;
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
/* KEY: This used to have __mips */
#if	defined(_F_INT2)
		case 2:
			mode	= MODEWP;
			break;
		case 1:
			mode	= MODEBP;
			break;
#endif	/* _F_INT2 */
#endif	/* _F_INT4 */
		case 8:
			break;
		default:
			return(FEKNTSUP);
		}
		break;
	}
	/*
	 * Call the function from the ncf_tab90 table.
	 */
	ngcf		= ncf_tab90[type];
	cup->ulinecnt++;	/* backup 1 character */
	cup->ulineptr--;	/* backup 1 character */
	field_begin	= cup->ulineptr;
	field_end	= cup->ulineptr;
	field_width	= cup->ulinecnt;
	slval		= (bcont*)lval;
	nc		= 0;
	while (nc < cup->ulinecnt && !(ISSEP(*field_end) ||
	   *field_end == '&' || *field_end == '$')) {
		field_end++;
		nc++;
	}
	/* pass field_end + 1 */
	field_end++;
	field_width	= nc;
	errn	= ngcf(field_begin, &field_width, &field_end,
		&mode, slval, &stat, &zero, &zero);

	/* If the scan failed, the input data might be
	 * Hollerith or hex or octal.  Allow _s_scan_extensions
	 * _s_scan_extensions to rescan the input and
	 * recompute the field width.
	 */
	if (errn < 0) {
		ss	= _nicverr(stat);
		if (ss == 0)
			errn	= 0;
	} else
		errn	= 0;

	/* if (errn == EX_ILLCHAR) */
	if (ss == FENICVIC) {
		int	errn2;
		errn2	= _s_scan_extensions(slval,
			type, elsize, field_begin,
			field_width, &ptrfw, mode);

		cup->ulineptr	= field_begin + field_width;
		cup->ulinecnt  -= cup->ulineptr - field_begin;
		if (errn2 >= 0)
			errn	= 0;
		else
			/* errors FELDUNKI and FELDSTRL
			 * are currently returned.
			 */
			errn	= FENLUNKI;
			return(errn);
	} else {
		cup->ulineptr	= field_begin + field_width;
		cup->ulinecnt  -= cup->ulineptr - field_begin;
	}
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
	int		lcount, /* Repeat count */
	long		elsize,
	int		*nullvlu)
{
	int	eos;	/* eos == -1 if end or beginning of string */
	int	i, ch;
	unsigned int	len77;
	char	*cp;
	char	enddelim;
	char	c1;
	int	repcount;
	char	*cpold;
	int	errn = 0;
	long	*optr;
	int	ocnt;
	void	*fchp;
	*nullvlu = 0;
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
	/* f90 allows zero-length character entities */
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
			GETSTRD();
				if (eos == 0)
					*cp++	= ch;
		}
		if (eos == -1)
			i--;
		i	= len77 - i; /* If declared len > read len */
		if (i > 0)
			(void) memset(cp, BLANK, i);	/* blank fill */
		cp	= cp + i;
		while (eos != -1) {
			/*
			 * We didn't hit the end of the string yet.
			 * Search for it.
			 */
			GETSTRD();
		}
		while (--repcount) {
			/* We have a repeat count.
			 * cp will point to the next element.
			 * Copy len77 characters to the next element. 
			 */
			cpold	= fchp;
			(void) memcpy(cp, cpold, len77);
			cp	= cp + len77;	/* Next element */
		}
	} else {
		/*
		 * We have a character string that's not surrounded
		 * by quotes (or apostrophes).  Read until we see a
		 * blank, separator, comment, or EOR (which looks
		 * like a blank to us).  Store as many of them as
		 * we have room for.  We cannot have a repeat count
		 * unless we're surrounded by quotes or apostrophes.
		 */
		if (lcount > 1) {
			errn	= FENLNOVL; /* invalid char data */
			return(errn);
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
			/* check for separator or terminating character */
			if (c1 == ',' || c1 == '/' || c1 == '&' || c == '$')
				break;	/* Assume value */
			if (c1 == '=' || c1 == '(' || c1 == '%') {
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
		if (c1 == '=' || c1 == '(' || c1 == '%') {
			/*
			 * Reset, this MAY be the first letter
			 * of a variable name.
			 */
			cup->ulineptr--;
			cup->ulinecnt++;
			*nullvlu	= 2;
			return(0);	/* Null value */
		}
		/* f90 does not allow undelimited character */
		errn	= FENLUNKI; /* undelimited char */
		return(errn);
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
	long		*lval,
	long		elsize)
{
	int	i;
	char	*holbufptr;
	char	c;
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
	 * Also, we are going to assume that whatever we read in will need
	 * to fit in one word. Repeat counts are allowed. If it becomes
	 * necessary to allow hollerith strings of > 8 characters, some
	 * thought will need to be given as to how to handle repeat counts.
	 */
	if (type == DVTYPE_COMPLEX || type == DVTYPE_ASCII ||
	  ((type == DVTYPE_REAL) && elsize == sizeof(_f_real16))) {
		errn	= FENLUNKI;
		return(errn);
	}
	if (count > elsize) {
		errn	= FENLIOER;
		return(errn);
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
		errn	= FENLIOER;
		return(errn);
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
	long		*lval,	/* Value is placed here */
	long		elsize)	/* size */
{
	int	numchar;	/* character counter */
	int	j;
	int	fill;		/* Fill character is either ' ' or '\0' */
	long	holbuf;		/* Data is stored here until we know whether
				   it is right or left justified. */
	char	*holbufptr;	/* pointer into holbuf */
	char	c;		/* Character read */
	char	*lvalcharptr;	/* Pointer to value */
	int	errn = 0;
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
	if (type == DVTYPE_COMPLEX || type == DVTYPE_ASCII ||
	   (type == DVTYPE_REAL && elsize == sizeof(_f_real16))) {
		errn	= FENLUNKI;
		return(errn);
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
			errn	= FENLIOER;
			return(errn);
		}
		*holbufptr++	= c;	/* Save the character */
		/*
		 * Last character in input buffer is not EOR character,
		 * that's why we check for cup->ulinecnt <= 0
		 */
		if (cup->ulinecnt <= 0) {
			errn	= FENLIOER;
			return(errn);
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
 *		>0 if an error occurred
 * nullvlu =	1 if a null value was found
 *		2 if a null value was found, and it is not followed
 *		  by another value
 */

static int
_gocthex(
	FIOSPTR	css,
	unit	*cup,
	ftype_t	type,
	long	*lval,
	int	base,
	long	elsize,
	int	*nullvlu)
{
	char	c;
	char	strbuf[2];
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
		errn	= FENLUNKI;	/* type mismatch */
		return(errn);
	}
	/* if not enough characters in record for octal/hex constant, err */
	if (cup->ulinecnt <= 1) {
		errn	= FENLIOER;
		return(errn);
	}
	SUBGTC(c);	/* Skip the apostrophe */
	SUBGTC(c);	/* and get the next character */
	*lval		= 0;
	strbuf[1]	= '\0';
	while (!(ISBLANK(c)) && c != '\'') {
		if (base == OCTAL) {
			if ((!isdigit((int) c)) || (c == '9') ||
				(*lval >> octshift)) {
					errn	= FENICVIC; /* NICV type err */
					return(errn);
			}
			*lval	= (*lval * 8) + c - '0';
		}
		else { /* Check for hex digit or overflow */
			if ((!isxdigit(c)) || (*lval >> hexshift)) {
				errn	= FENICVIC;	/* NICV type err */
				return(errn);
			}
			strbuf[0]	= c;
			*lval	= (*lval * 16) +
					(int) strtol(strbuf, (char **)NULL, 16);
		}
		/* check for comment after value */
		CMTE_SUBGTC(c);
		if (c == ',') {
			cup->ulineptr--;
			cup->ulinecnt++; /* to read separator after */
			break;	/* return from this routine */
		}
	}
	return(errn);	/* indicate value */
}

/*
 *	_nl_stride_dv
 *		Call a specified function to transfer a data area defined
 *		by a dopevector.  This corresponds to an array section.
 *	Arguments
 *		dv	- dope vector which describes the array section.
 *		sectn	- Dimension information in input record.
 *	Return Value
 *		0		normal return
 *		FERDPEOF	if end of file condition
 *		>0		if error condition
 */

static int
_nl_stride_dv(
	FIOSPTR		css,
	unit		*cup,
	DopeVectorType	*dv,
	struct DvDimen	*sectn,
	char		*lastch,
	long		strbegend[3])
{
	int		nd;
	int		i;
	long		extent;			/* extent of first dimension */
	long		inc;			/* stride in items */
	long		ret = 0;
	ftype_t		f90type;		/* F90 data type code */
	long		elsize;			/* byte size of each element */
	long		element_stride;		/* 1 iff elsize divides stride*/
	register long	id1, id2, id3, id4, id5, id6, id7;
	struct DvDimen	*dvdimen;
	long		badjust;		/* offset for collapsed dims */
	bcont		*addr; 			/* for numeric data */
	char		*baddr;			/* for byte-oriented data */
	void		*addr2, *addr3, *addr4;
	void		*addr5, *addr6;
	struct DvDimen	dimen[MAXDIM];
	long		begt = strbegend[1];
	long		endt = strbegend[2];

	/* Assertions */
	assert ( dv != NULL );
	assert ( dv->type_lens.int_len > 0 );

	if (dv->p_or_a && (dv->assoc == 0))
		return(FEPTRNAS);		/* pointer not associated */

	f90type	= dv->type_lens.type;
	nd	= dv->n_dim;
	badjust	= 0;

/*
 *	Make a local copy of dimension information so we may optimize it.
 */
	for (i = 0; i < nd; i++)
		dimen[i]	= dv->dimension[i];

/*
 *	Fold any indexes into the new dimension structure.  The
 *	result is that we can ignore the low_bound field in the
 *	nested loops. 
 *
 *	We also collapse (remove) indexed dimensions and 
 *	unindexed dimensions with extents of one.
 */
	dvdimen	= dv->dimension;
	for (i = 0; i < nd; i++) {
		if (sectn == NULL) {

			/* bail out here if any extent is 0 */
			if (dvdimen[i].extent == 0)
				return(0);	
		}
		else {
			/* collapse this indexed dimension */
			badjust	+= (sectn[i].low_bound -
				    dvdimen[i].low_bound) * 
					dvdimen[i].stride_mult;
			if (dvdimen[i].extent != sectn[i].extent)
				dimen[i].extent = sectn[i].extent;
			if (dvdimen[i].stride_mult != sectn[i].stride_mult)
				dimen[i].stride_mult = sectn[i].stride_mult;
		}
	}

	if (f90type == DVTYPE_ASCII) {

		elsize		= _fcdlen(dv->base_addr.charptr); /* in bytes */
		extent		= dimen[0].extent;
		inc		= 0;
		element_stride	= 1;

		if (extent > 1) {
			register int	stm = dimen[0].stride_mult;

			inc	= stm / elsize;
			if (inc * elsize != stm)
			element_stride	= 0;	/* it's a section of substrings */
		}
	
		baddr	= _fcdtocp(dv->base_addr.charptr) +
				badjust * (dv->type_lens.int_len >> 3);
		
		switch(nd) {
		case 7:
		    for (id7 = 0; id7 < dimen[6].extent; id7++) {
		      addr6	= baddr;
		case 6:
		      for (id6 = 0; id6 < dimen[5].extent; id6++) {
		        addr5	= baddr;
		case 5:
			for (id5 = 0; id5 < dimen[4].extent; id5++) {
		          addr4	= baddr;
		case 4:
			  for (id4 = 0; id4 < dimen[3].extent; id4++) {
			    addr3	= baddr;
		case 3:
			    for (id3 = 0; id3 < dimen[2].extent; id3++) {
			      addr2	= baddr;
		case 2:
			      for (id2 = 0; id2 < dimen[1].extent; id2++) {
		case 1:
				if ((element_stride == 1) && (strbegend[0] == 0)) {
				  ret	= _nlread(css, f90type, cup, baddr,
					     elsize, extent, inc, lastch);
				  if (ret != 0) goto done;
				}
				else {
				  char	*ba;
				  char	*newba;
				  int	newelsz;
				  ba	= baddr;
				  if (strbegend[0] == 0) {
				    for (id1 = 0; id1 < extent; id1++) {
				      ret = _nlread(css, f90type, cup, ba,
					       elsize, 1, 0, lastch);
				      if (ret != 0) goto done;
				      ba	+= dimen[0].stride_mult;
				    }
				  } else {
				    if (begt < 1 )
				      begt = 1;
				    else if (begt > elsize) {
				      ret	= FENLUNKN;
				      goto done;
				    }
				    if (endt < 1 )
				      endt = elsize;
				    else if ((endt > elsize) || (endt < begt)) {
				      ret	= FENLUNKN;
				      goto done;
				    }
				    for (id1 = 0; id1 < extent; id1++) {
				      newba	= ba + (begt - 1);
				      newelsz	= (endt - begt) + 1;
				      ret = _nlread(css, f90type, cup,
					newba, newelsz, 1, 0, lastch);
				      if (ret != 0)
					goto done;
				      ba	+= dimen[0].stride_mult;
				    }
				  }
				}
	
				if (nd == 1) goto done;
		      	        baddr	+= dimen[1].stride_mult;
			      }
			      if (nd == 2) goto done;
		      	      baddr	= addr2;
		      	      baddr	+= dimen[2].stride_mult;
			    }
			    if (nd == 3) goto done;
		      	    baddr	= addr3;
		      	    baddr	+= dimen[3].stride_mult;
			  }
			  if (nd == 4) goto done;
		      	  baddr	= addr4;
		      	  baddr	+= dimen[4].stride_mult;
			}
			if (nd == 5) goto done;
		      	baddr	= addr5;
		      	baddr	+= dimen[5].stride_mult;
		      }
		      if (nd == 6) goto done;
		      baddr	= addr6;
		      baddr	+= dimen[6].stride_mult;
		    }
		}

	}
	else {				/* numeric data */

		int bshft;	/* 0 or 1; shift count for ratio of		*/
				/* stride_mult units to basic storage unit	*/
				/* size.					*/

		/*
		 *	We only support dopevector stride mults with units
		 *	scaled by sizeof(long) or sizeof(bcont).
 		 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN) || defined(__sv2)
		assert( SMSCALE(dv) == sizeof(bcont) ||
			SMSCALE(dv) == sizeof(_f_int2) ||
			SMSCALE(dv) == sizeof(_f_int4) ||
			SMSCALE(dv) == sizeof(long)	);
#else
		assert( SMSCALE(dv) == sizeof(bcont) ||
			SMSCALE(dv) == sizeof(long)	);
#endif

		/* the -1 is not possible but check for it */
		assert( SMSHIFT(dv) != -1);

		element_stride	= 1;
		elsize		= dv->type_lens.int_len >> 3;
		extent		= dimen[0].extent;
		inc		= 0;
		bshft		= SMSHIFT(dv);

		if (extent > 1) {
		    int	bytes_per_sm = dimen[0].stride_mult*(signed)SMSCALE(dv);
		    inc	= bytes_per_sm / elsize;
		    if (inc * elsize != bytes_per_sm)
			element_stride	= 0;	/* section across derived type */
		}

		addr	= (bcont*)dv->base_addr.a.ptr + (badjust << bshft);
		
		switch(nd) {
		case 7:
		    for (id7 = 0; id7 < dimen[6].extent; id7++) {
		      addr6	= addr;
		case 6:
		      for (id6 = 0; id6 < dimen[5].extent; id6++) {
		        addr5	= addr;
		case 5:
			for (id5 = 0; id5 < dimen[4].extent; id5++) {
		          addr4	= addr;
		case 4:
			  for (id4 = 0; id4 < dimen[3].extent; id4++) {
			    addr3	= addr;
		case 3:
			    for (id3 = 0; id3 < dimen[2].extent; id3++) {
			      addr2	= addr;
		case 2:
			      for (id2 = 0; id2 < dimen[1].extent; id2++) {
		case 1:
				if (element_stride)  {
				  ret	= _nlread(css, f90type, cup, addr,
					elsize, extent, inc, lastch);
				}
				else {
				  bcont	*ad;
				  ad	= addr;
				  /* 
				   * If derived type foo contains two fields,
				   * real a and double precision d,  then
				   * foo(1:2)%d generates this type of 
				   * dopevector with a stride which is not
				   * a multiple of the element size.
				   */
				  for (id1 = 0; id1 < extent; id1++) {
				    ret	= _nlread(css, f90type, cup, ad,
					       elsize, 1, 0, lastch);
				    if (ret != 0) goto done;
				    ad	+= dimen[0].stride_mult;
				  }
				}
	

				if (ret != 0) goto done;
	
				if (nd == 1) goto done;
		      	        addr	+= dimen[1].stride_mult << bshft;
			      }
			      if (nd == 2) goto done;
		      	      addr	= addr2;
		      	      addr	+= dimen[2].stride_mult << bshft;
			    }
			    if (nd == 3) goto done;
		      	    addr	= addr3;
		      	    addr	+= dimen[3].stride_mult << bshft;
			  }
			  if (nd == 4) goto done;
		      	  addr	= addr4;
		      	  addr	+= dimen[4].stride_mult << bshft;
			}
			if (nd == 5) goto done;
		      	addr	= addr5;
		      	addr	+= dimen[5].stride_mult << bshft;
		      }
		      if (nd == 6) goto done;
		      addr	= addr6;
		      addr	+= dimen[6].stride_mult << bshft;
		    }
		}
	}

done:	return(ret);
}

static int
_nl_strd_derv(
	FIOSPTR		css,
	unit		*cup,
	DopeVectorType	*dv,
	struct DvDimen	*sectn,
	char		*lastch,
	nmlist_goli_t	*vdr,
	unsigned int	cnt,
	long		bte)
{
	const int	bytesperchar = 1;
	int		nd;
	int		i;
	long		badjust;	/* offset for collapsed dims */
	long		elsize;		/* byte size of each element */
	long		ret = 0;
	long		sizeamt;	/* unit for stride mult */
	register long	id1, id2, id3, id4, id5, id6, id7;
	struct DvDimen	*dvdimen;
	struct DvDimen	dimen[MAXDIM];

	nd	= dv->n_dim;
	badjust	= 0;

/*	Make a local copy of dimension information to optimize it. */
	for (i = 0; i < nd; i++)
		dimen[i]	= dv->dimension[i];

/*	Fold any indexes into the new dimension structure.  The
 *	result is that we can ignore the low_bound field in the
 *	nested loops. 
 *
 *	We also collapse (remove) indexed dimensions and 
 *	unindexed dimensions with extents of one.
 */
	dvdimen	= dv->dimension;
	for (i = 0; i < nd; i++) {
		if (sectn == NULL) {

			/* bail out here if any extent is 0 */
			if (dvdimen[i].extent == 0)
				return(0);	
		}
		else {
			/* collapse this indexed dimension */
			badjust	+= (sectn[i].low_bound -
				    dvdimen[i].low_bound) * 
					dvdimen[i].stride_mult;
			if (dvdimen[i].extent != sectn[i].extent)
				dimen[i].extent = sectn[i].extent;
			if (dvdimen[i].stride_mult != sectn[i].stride_mult)
				dimen[i].stride_mult = sectn[i].stride_mult;
		}
	}

	elsize	= dv->base_addr.a.el_len>> 3;
	bte	= (badjust * elsize);
	if (dv->type_lens.type == DVTYPE_DERIVEDWORD) {
		sizeamt	= sizeof(int);
	} else if (dv->type_lens.type == DVTYPE_DERIVEDBYTE) {
		sizeamt	= 1 * bytesperchar;
	} else {
		sizeamt	= (signed)SMSCALE(dv);
	}

	switch(nd) {
	case 7:
		for (id7 = 0; id7 < dimen[6].extent; id7++) {
	case 6:
		 for (id6 = 0; id6 < dimen[5].extent; id6++) {
	case 5:
		  for (id5 = 0; id5 < dimen[4].extent; id5++) {
	case 4:
		   for (id4 = 0; id4 < dimen[3].extent; id4++) {
	case 3:
		    for (id3 = 0; id3 < dimen[2].extent; id3++) {
	case 2:
		     for (id2 = 0; id2 < dimen[1].extent; id2++) {
	case 1:
		      for (id1 = 0; id1 < dimen[0].extent; id1++) {
			ret	= _nlrdent(css, cup, vdr, cnt, lastch, bte);

			if (ret != 0) goto done;
			bte += dimen[0].stride_mult * sizeamt;
		      }
		      if (nd == 1) goto done;
		      bte += dimen[1].stride_mult * sizeamt;
		     }
		     if (nd == 2) goto done;
		     bte += dimen[2].stride_mult * sizeamt;
		    }
		    if (nd == 3) goto done;
		    bte	+= dimen[3].stride_mult * sizeamt;
		   }
		   if (nd == 4) goto done;
		   bte += dimen[4].stride_mult * sizeamt;
		  }
		  if (nd == 5) goto done;
		  bte += dimen[5].stride_mult * sizeamt;
		 }
		 if (nd == 6) goto done;
		 bte += dimen[6].stride_mult * sizeamt;
		}
	}
done:	return(ret);
}
