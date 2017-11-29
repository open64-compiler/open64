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



#pragma ident "@(#) libf/fio/inquire.c	92.1	06/21/99 10:37:55"

#include <errno.h>
#include <limits.h>
#if !defined(_ABSOFT)
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <string.h>
#include <cray/assign.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fio.h"
  
  
/*
 *	_f_inqu - process INQUIRE statement.
 *
 *	Return value
 *		Returns 0 on success, positive error code if an error
 *		is encountered and ERR= or IOSTAT= are unspecified.
 *		This routine aborts on error conditions if no ERR=
 *		or IOSTAT= are specified.
 */
int _f_inqu(
FIOSPTR	css,		/* statement state			*/
unit	*cup,		/* locked unit pointer if INQUIRE by   
			 * unit and unit is connected.		*/
inlist	*a)		/* list of INQUIRE specifiers		*/
{
	int	aifound;	/* Assign info found flag	*/
	int	byfile;		/* INQUIRE by file/unit flag	*/
	int	exists;		/* File exists flag		*/
	int	opened;		/* File opened flag		*/
	int	valunit;	/* Valid unit number flag	*/
	int	errn;
	char	*buf, *fn, *s;
	struct	stat	st;	/* Stat system call packet	*/
	assign_info	ai;	/* Assign information packet	*/
	unit	*p;
	
	p	= cup;
	errn	= 0;

/*
 *	Lock _openlock to ensure that no other task opens or closes units 
 *	during the unit table scan for inquire-by-file processing.  
 */
	OPENLOCK();

	if (a->infile != NULL)		/* if INQUIRE by file */
		byfile	= 1;
	else {				/* else INQUIRE by unit */
		byfile	= 0;
		valunit	= GOOD_UNUM(a->inunit) &&
			  !RSVD_UNUM(a->inunit);	/* Valid Unit Number? */
	}
  
	if ((buf = malloc(MAX(a->infilen + 1, MXUNITSZ + 1))) == NULL) {
		errn	= FENOMEMY;
		if (a->inerr)
			goto out_of_here;
		_ferr(css, errn);
	}
  
	*buf	= '\0';		/* Assume no name */
	opened	= 0;		/* Assume not opened */
	fn	= buf;
  
	if (byfile) {		/* If INQUIRE by file */
  
		_copy_n_trim(a->infile, a->infilen, buf);
  
		if ((aifound = _get_a_options(0, buf, -1, 0, &ai, NULL,
					_LELVL_RETURN)) == -1) {

			errn	= errno;

			if (a->inerr) {
				free(buf);
				goto out_of_here;
			}
			_ferr(css, errn);
		}
  
		if (aifound && ai.a_actfil_flg)		/* If assign alias */
			s	= ai.a_actfil; /* Use -a attribute as file name */
		else
			s	= buf;
  
		exists	= (stat(s, &st) != -1);
  
		if (exists) {

			p	= _get_next_unit(NULL, 1, 1);

			while (p != NULL) {	/* while more open units */
				unum_t	unum;

				unum	= p->uid;

				if (! RSVD_UNUM(unum) &&
				    (p->uinode  == st.st_ino) &&
				    (p->udevice == st.st_dev)) {
					fn	= p->ufnm;
					opened	= 1;
					break;
				}
				p	= _get_next_unit(p, 1, 1);
			}
			/*
 			 * If p is non-null here, it points to a locked unit.
			 * The unit is locked to ensure a consistent set of
			 * INQUIRE'd attributes is returned.
			 */
		}
	}
	else {			/* Else INQUIRE by unit */
		if (valunit) {
			opened	= (cup != NULL);
			if (opened) {		/* If opened, get name */
				p	= cup;
				fn	= p->ufnm;
			}
		}
	}
  
	if (fn == NULL)		/* If no name available, return blanks */
		fn	= "";

	/* EXIST specifier */
  
	if (a->inex != NULL)
		if (byfile)	/* If INQUIRE by file */
			*a->inex	= _btol(exists);
		else		/* INQUIRE by unit    */
			*a->inex	= _btol(valunit); 
 
	/* OPENED specifier */
  
	if (a->inopen != NULL)
		*a->inopen	= _btol(opened);

	/* NAMED specifier */
  
	if (a->innamed != NULL)
		if (byfile)	/* If INQUIRE by file */
			*a->innamed	= _btol(1);		/* .TRUE. */
		else		/* INQUIRE by unit    */
			*a->innamed	= _btol(opened && p->ufnm != NULL);
  
	/* NUMBER specifier */
  
	if (a->innum != NULL) {
		if (opened) {
			if (byfile)	/* If INQUIRE by file */
				*a->innum	= (opened) ? p->uid : -1;
			else		/* INQUIRE by unit    */
				*a->innum	= a->inunit; /* The law of identity */
		}
		else
			*a->innum = -1;
	}
  
	/* RECL specifier */
  
	if (a->inrecl != NULL)
		if (opened) {
			if (p->urecl > 0)	/* If recl was specified */
				*a->inrecl	= p->urecl;
			else	/* Recl not specified (i.e., sequential) */
				*a->inrecl	= (p->ufmt) ? p->urecsize : LONG_MAX;
		}
		else 
			*a->inrecl	= -1;
  
	/* NEXTREC specifier */
  
	if (a->innrec != NULL)
		if (opened && p->useq == 0)	/* If opened & direct access */
			*a->innrec	= p->udalast + 1;
		else
			*a->innrec	= -1;
  
	/* NAME specifier */
  
	if (a->inname != NULL)
		_b_char(fn, a->inname, a->innamlen);
  
	/* ACCESS specifier */

	if (a->inacc != NULL) {
		if (opened)
			s	= (p->useq) ? "SEQUENTIAL" : "DIRECT";
		else
			s	= "UNDEFINED";
		_b_char(s, a->inacc, a->inacclen);
	}
 
	/* SEQUENTIAL specifier */

	if (a->inseq != NULL) {
		if (opened)
			s	= (p->useq) ? "YES" : "NO";
		else
			s	= "UNKNOWN";
		_b_char(s, a->inseq, a->inseqlen);
	}
  
	/* DIRECT specifier */

	if (a->indir != NULL) {
		if (opened)
			s	= (p->useq) ? "NO" : "YES";
		else
			s	= "UNKNOWN";
		_b_char(s, a->indir, a->indirlen);
	}
  
	/* FORM specifier */

	if (a->inform != NULL) {
		if (opened)
			s	= (p->ufmt) ? "FORMATTED" : "UNFORMATTED";
		else
			s	= "UNDEFINED";
		_b_char(s, a->inform, (ftnlen)a->informlen);
	}
  
	/* FORMATTED specifier */

	if (a->infmt != NULL) {
		if (opened)
			s	= (p->ufmt) ? "YES" : "NO";
		else
			s	= "UNKNOWN";
		_b_char(s, a->infmt, a->infmtlen);
	}
  
	/* UNFORMATTED specifier */

	if (a->inunf != NULL) {
		if (opened)
			s	= (p->ufmt) ? "NO" : "YES";
		else
			s	= "UNKNOWN";
		_b_char(s, a->inunf, a->inunflen);
	}
  
	/* BLANK specifier */

	if (a->inblank != NULL) {
		if (opened && p->ufmt)
			s	= (p->ublnk) ? "ZERO" : "NULL";
		else
			s	= "UNDEFINED";
		_b_char(s, a->inblank, a->inblanklen);
	}
  
	/* POSITION specifier */

	if (a->inposit != NULL) {	/* Fortran 90 position control */
		if (opened && p->useq) {
			switch (p->uposition) {
				case OS_REWIND:
					s	= "REWIND";
					break;
				case OS_ASIS:
					s	= "ASIS";
					break;
				case OS_APPEND:
					s	= "APPEND";
					break;
				case 0:
					s	= "UNKNOWN";
					break;
				default:
					_ferr(css, FEINTUNK);
			}
		}
		else
			s	= "UNDEFINED";
		_b_char(s, a->inposit, a->inpositlen);
	}

	/* ACTION specifier */

	if (a->inaction != NULL) { 	/* Fortran 90 action control */
		if (opened) {
			switch (p->uaction) {
				case OS_READWRITE:
					s	= "READWRITE";
					break;
				case OS_READ:
					s	= "READ";
					break;
				case OS_WRITE:
					s	= "WRITE";
					break;
				default:
					_ferr(css, FEINTUNK);
			}
		}
		else	/* for an unconnected file */
			s	= "UNDEFINED";
		_b_char(s, a->inaction, a->inactonlen);
	}

	/* READ specifier */

	if (a->inread != NULL) {	/* Fortran 90 read action control */
		if (opened) {
			if ((p->uaction == OS_READ) ||
			    (p->uaction == OS_READWRITE))
				s	= "YES";
			else
				s	= "NO";
		}
		else
			s	= "UNKNOWN";
		_b_char(s, a->inread, a->inreadlen);
	}

	/* WRITE specifier */

	if (a->inwrite != NULL) {	/* Fortran 90 write action control */
		if (opened) {
			if ((p->uaction == OS_WRITE) ||
			    (p->uaction == OS_READWRITE))
				s	= "YES";
			else
				s	= "NO";
		}
		else
			s	= "UNKNOWN";
		_b_char(s, a->inwrite, a->inwritelen);
	}

	/* READWRITE specifier */

	if (a->inredwrit != NULL) {  /* Fortran 90 read/write action control */
		if (opened) {
			if (p->uaction == OS_READWRITE)
				s	= "YES";
			else
				s	= "NO";
		}
		else
			s	= "UNKNOWN";
		_b_char(s, a->inredwrit, a->inrdwrtlen);
	}

	/* DELIM specifier */

	if (a->indelim != NULL) { /* Fortran 90 delim control */
		if (opened && p->ufmt) {	/* if formatted */
			switch (p->udelim) {
				case OS_NONE:
					s	= "NONE";
					break;
				case OS_QUOTE:
					s	= "QUOTE";
					break;
				case OS_APOSTROPHE:
					s	= "APOSTROPHE";
					break;
				default:
					_ferr(css, FEINTUNK);
			}
		}
		else   /* UNDEFINED for unformatted or unconnected file */
			s	= "UNDEFINED";
		_b_char(s, a->indelim, a->indelimlen);
	}

	/* PAD specifier */

	if (a->inpad != NULL) {  /* Fortran 90 pad control */
		if(opened && p->ufmt) { 	/* if formatted */
			switch (p->upad) {
				case OS_YES:
					s	= "YES";
					break;
				case OS_NO:
					s	= "NO";
					break;
				default:
					_ferr(css, FEINTUNK);
			}
		}
		else /* Fortran 90 missed UNDEFINED if unformatted or unconnected */
			s	= "YES";   /* set to YES instead of UNDEFINED */
		_b_char(s, a->inpad, a->inpadlen);
	}

/*
 *	Unlock the unit if we have a pointer to an open unit.   Note that
 *	$INQ/_INQUIRE never unlocks the unit.
 */
out_of_here:

	OPENUNLOCK();

	if (p != NULL)
		_release_cup(p);	/* unlock the unit */

	free(buf);
	return(errn);
}
