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



#pragma ident "@(#) libf/fio/fcontext.c	92.1	06/18/99 16:08:47"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fio.h"

#define	MAX_ENT_LEN	32	/* Maximum entry point name length */

extern	int	_who_called_me();

/*
 *	_fcontext - Print the context of a Fortran library run-time error.
 *		    based on the following global flags:
 *
 *		f_iostmt	Identifies the type of I/O statement, NULL
 *				if none.
 *		f_curun		Identifies the unit number being operated on,
 *				-1 if none defined.  
 *		f_cu		Identifies current unit, if this is NULL, an
 *			 	unconnected unit (possibly an invalid unit
 *			  	number) is assumed.
 *		f_intflg	Identifies an internal file, 1 if internal.
 *
 */

void
_fcontext(FIOSPTR css)
{
	register short	is_int;		/* 1 if internal file I/O	*/
	register int	utindex;
	char  		*file, *fstruct, *idir, *oprn, *sepr;
	register unum_t	unum;		/* Fortran unit number		*/
	long		stmt;		/* I/O statement type		*/
	unit		*cup;		/* Pointer to unit table entry	*/

	/* Just return if no Fortran statement info is available */

	if (css == NULL)
		return;			

	/* Retrieve global data */

	cup	= css->f_cu;
	unum	= css->f_curun;
	stmt	= css->f_iostmt;
	is_int	= css->f_intflg;

	file	= (!OPEN_UPTR(cup) || cup->alfnm == NULL) ? NULL : cup->alfnm;

	if (stmt & TF_READ)
		idir	= " READ from";
	else
		if (stmt & TF_WRITE)
			idir	= " WRITE to";
		else
			idir	= "";

	/* Determine the type of error */

	switch (stmt) {

		case T_RSF:	/* Sequential formatted READ  */
		case T_WSF:	/* Sequential formatted WRITE */
			oprn	= " sequential formatted";
			break;

		case T_RSU:	/* Sequential unformatted READ  */
		case T_WSU:	/* Sequential unformatted WRITE */
			oprn	= " sequential unformatted";
			break;

		case T_RDF:	/* Direct formatted READ  */
		case T_WDF:	/* Direct formatted WRITE */
			oprn	= " direct access formatted";
			break;

		case T_RDU:	/* Direct unformatted READ  */
		case T_WDU:	/* Direct unformatted WRITE */
			oprn	= " direct access unformatted";
			break;

		case T_RLIST:	/* List-directed READ  */
		case T_WLIST:	/* List-directed WRITE */
			oprn	= " list-directed";
			break;

		case T_RNL:	/* Namelist READ  */
		case T_WNL:	/* Namelist WRITE */
			oprn	= " namelist";
			break;

		case T_BUFOUT:	/* BUFFER OUT */
			oprn	= " BUFFER OUT on";
			idir	= "";
			break;

		case T_BUFIN:	/* BUFFER IN */
			oprn	= " BUFFER IN from";
			idir	= "";
			break;

		case T_OPEN:	/* OPEN */
			oprn	= "n OPEN of";
			idir	= "";
			break;

		case T_REWIND:	/* REWIND */
			oprn	= " REWIND on";
			idir	= "";
			break;

		case T_BACKSPACE:/* BACKSPACE */
			oprn	= " BACKSPACE on";
			idir	= "";
			break;

		case T_ENDFILE:	/* ENDFILE */
			oprn	= "n ENDFILE on";
			idir	= "";
			break;

		case T_CLOSE:	/* CLOSE */
			oprn	= " CLOSE of";
			idir	= "";
			break;

		case T_INQF:	/* INQUIRE */
			oprn	= "n INQUIRE by file on";
			unum	= -1;
			idir	= "";
			break;

		case T_INQU:	/* INQUIRE */
			oprn	= "n INQUIRE by unit on";
			idir	= "";
			break;

		case T_GETPOS:	/* GETPOS */
			oprn	= " GETPOS on";
			idir	= "";
			break;

		case T_SETPOS:	/* SETPOS */
			oprn	= " SETPOS on";
			idir	= "";
			break;

		case T_LENGTH:	/* LENGTH */
			oprn	= " LENGTH function on";
			idir	= "";
			break;

		case T_UNIT:	/* UNIT */
			oprn	= " UNIT function on";
			idir	= "";
			break;

		case T_TAPE:	/* TAPE */
			oprn	= " tape operation on";
			idir	= "";
			break;

		default:
			oprn	= "n I/O operation on";
			break;

	} /* switch */

	(void) fprintf(errfile, "\nEncountered during a%s%s", oprn, idir);

	if (is_int)
		(void) fprintf(errfile,
			" an internal file (character variable)\n");
	else {
		if (unum != -1) {
			(void) fprintf(errfile, " unit %lld\n", unum);

			(void) fprintf(errfile, "Fortran unit %lld is ",
				unum);

			if (!OPEN_UPTR(cup)) {
				if (GOOD_UNUM(unum))
					(void) fprintf(errfile,
						"not connected\n");
				else
					(void) fprintf(errfile,
						"not a valid unit number\n");
			}
			else {
				(void) fprintf(errfile, "connected to ");

				utindex	= IO_TYPE(cup);
				fstruct	= FIO_STRUCT(_deduce_fstruct(
						cup->ufs,
                                        	(struct fdinfo*)cup->ufp.fdc,
						cup->ufmt));

				if (fstruct == NULL)
					fstruct	= "";

				(void) fprintf(errfile, "a %s %s file",
					FIO_METHOD(utindex), fstruct);

				if (file == NULL && cup->ufs != FS_FDC) {
					if (cup->ufp.std == stdin)
						file	= "standard input";
					else if (cup->ufp.std == stdout)
						file	= "standard output";
					else if (cup->ufp.std == stderr)
						file	= "standard error";
					else
						file	= "unnamed";

					(void) fprintf(errfile,
						"\n  (%s).\n", file);
				}
				else {	/* Format to under 80 chars. per line */
					if ((int)strlen(file) > 8)
						sepr	= ":\n  ";
					else
						sepr	= ": ";

					(void) fprintf(errfile, "%s\"%s\"\n",
						sepr, file);
				}

				/*
				 * If the connection is formatted and there's
				 * a format, print the format and point to
				 * the current position therein.
				 */

				if ((stmt & TF_FMT) &&
				    css->u.fmt.u.fe.fmtbuf != NULL) {

					int	i, offset;

					offset	= css->u.fmt.u.fe.fmtcol - 2 +
							fprintf(errfile,
							" Current format: ");

					if (css->u.fmt.u.fe.fmtnum > 0)
						/* If format label, print it */
						offset	= offset +
							fprintf(errfile,
							"%5d FORMAT",
							css->u.fmt.u.fe.fmtnum);

					(void) fprintf(errfile, "%.*s\n",
						css->u.fmt.u.fe.fmtlen,
						css->u.fmt.u.fe.fmtbuf);

					for (i = 0; i <= offset; i++)
						(void) fprintf(errfile, " ");

					(void) fprintf(errfile, "^\n");
				}
			}
		}
		else		/* Unknown state */
			if (file == NULL)
				(void) fprintf(errfile,
						" an indeterminate file\n");
			else
				(void) fprintf(errfile, " file \"%s\"\n", file);
	}

	/* 
	 * Print name of the routine which called _ferr() which called us. 
	 */

#ifdef	_UNICOS
	{
		int	len, lineno;
		char	name[MAX_ENT_LEN];

		len	= _who_called_me(&lineno, name, MAX_ENT_LEN, 2);

		if (len > 0) {		/* If no error */

			name[len]	= '\0';

			(void) fprintf(errfile,
				"Error initiated at line %d in routine '%s'.\n",
				lineno, name);
		}
	}
#endif

#ifdef	_CRAY2
	/*
	 * Print traceback
	 *
	 * On CX/CEA systems, the traceback is printed by the abort() call.
	 */

	(void) _tracebk(25, errfile);
#endif

	return;
}
