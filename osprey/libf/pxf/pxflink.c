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


#pragma ident "@(#) libf/pxf/pxflink.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <malloc.h>
#include <string.h>
#include <unistd.h>

extern char *_fc_acopy(_fcd f);

/*
 *	PXFLINK		Create a link to a file
 *
 *	Call from Fortran:
 *
 *		SUBROUTINE PXFLINK (EXIST, LENEXST, NEW, LENNEW, IERROR)
 *		CHARACTER * (*) EXIST, NEW
 *		INTEGER LENEXST, LENNEW, IERROR
 *
 *	Where:
 *
 *	EXIST	is an input character variable or array element
 *		containing the name of a file that exists.
 *
 *	LENEXST is an input integer variable containing the length
 *		of EXIST in characters.  If ILEN is zero, any and
 *		all trailing blanks are removed.
 *
 *	NEW  	is an input character variable or array element
 *		containing the name of a new file.
 *
 *	LENNEW  is an input integer variable containing the length
 *		of NEW in characters.  If ILEN is zero, any and
 *		all trailing blanks are removed.
 *
 *	IERROR	is an output integer variable that will contain the
 *		status:  Zero if PXFLINK is successful; otherwise
 *		nonzero.
 *
 *		In addition to the error statuses returned by the
 *		link(2) system call, PXFLINK may return the
 *		following error statuses:
 *
 *		EINVAL	If ILEN < 0 or ILEN > LEN(PATH)
 *
 *		ENOMEM	If PXFLINK is unable to obtain memory to
 *			copy EXIST or NEW.
 */

#ifdef _UNICOS
void
PXFLINK(
#else
void
_PXFLINK(
#endif
	_fcd	EXISTF,		/* Current file Name */
	_f_int	*LENEX,		/* Length of EXISTF name or zero */
	_fcd	NEWF,		/* New file Name */
	_f_int	*LENNEW,	/* Length of NEWF name or zero */
	_f_int	*IERROR		/* Error status */
)
{
	int	arglenex, lengex;
	int	arglennew, lengnew;
	int	errsts;
	char	*argstrex, *pthstrex;
	char	*argstrnew, *pthstrnew;

	errsts	= 0;
	argstrex	= _fcdtocp(EXISTF);
	arglenex	= (int)_fcdlen (EXISTF);
	lengex	= *LENEX;
	argstrnew	= _fcdtocp(NEWF);
	arglennew	= (int)_fcdlen (NEWF);
	lengnew	= *LENNEW;

	if ((lengex < 0 || lengex > arglenex) ||
		((lengnew < 0 || lengnew > arglennew)))
		errsts	= EINVAL;
	else {

		/*
		 * If length is zero, user wants trailing blanks stripped.
		 * Otherwise, malloc memory and copy the string; adding a
		 * NULL terminator.
		 */

		if (lengex == 0)
			pthstrex	= _fc_acopy(EXISTF);
		else
			pthstrex	= (char *) malloc(lengex + 1);

		if (lengnew == 0)
			pthstrnew	= _fc_acopy(NEWF);
		else
			pthstrnew	= (char *) malloc(lengnew + 1);

		/* if no memory allocated */
		if (pthstrex == NULL || pthstrnew == NULL) {
			errsts	= ENOMEM;
			if (pthstrex != NULL)
				free(pthstrex);
			if (pthstrnew != NULL)
				free(pthstrnew);
		} else {
			if (lengex != 0) {	/* Copy argument */
				(void) memcpy(pthstrex, argstrex, lengex);
				pthstrex[lengex]	= '\0';
			}
			if (lengnew != 0) {	/* Copy argument */
				(void) memcpy(pthstrnew, argstrnew, lengnew);
				pthstrnew[lengnew]	= '\0';
			}

			/* link the files */

			if (link(pthstrex,pthstrnew) == -1)
				errsts	= errno;

			free(pthstrex);
			free(pthstrnew);
		}
	}
	*IERROR	= errsts;
	return;
}


#ifndef _UNICOS
void
pxflink_(
	 char	*EXISTF,	/* Current file Name */
	 _f_int	*LENEX,		/* Length of EXISTF name or zero */
	 char *NEWF,		/* New file Name */
	 _f_int	*LENNEW,	/* Length of NEWF name or zero */
	 _f_int	*IERROR,	/* Error status */
	 _f_int existflen,
	 _f_int newflen
)
{
  _PXFLINK(_cptofcd(EXISTF,existflen), LENEX, _cptofcd(NEWF, newflen),
	   LENNEW, IERROR);
}
#endif
