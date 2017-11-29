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


#pragma ident "@(#) libu/util/c1/jname.c	92.1	07/07/99 13:22:26"
#include <fortran.h> 
#include <stdlib.h>
#include <string.h>

#define BLANK	((int) ' ')

/*
 *	JNAME 	Returns the name of the job if present in a QSUB. 
 *		Otherwise, it returns the logname.  The result is
 *              left justified and filled with blanks on the right.
 *
 *		May be called as either a function or a subroutine.
 *
 *              As a character function, the reference is:
 *
 *                EXTERNAL JNAME
 *                CHARACTER*10 JNAME, RESULT, NAME
 *                NAME = JNAME(RESULT)
 *
 *              As a noncharacter function, the reference is:
 *
 *                EXTERNAL JNAME
 *                INTEGER RESULT
 *                RESULT = JNAME()
 *
 *              As a subroutine call, the reference is:
 *
 *                CHARACTER*10 JOBNAM
 *                CALL JNAME (JOBNAM)
 *
 *              or
 *
 *                INTEGER RESULT
 *                CALL JNAME (RESULT)
 *
 */
_f_int
JNAME(name)
long	name;
{
 
	long	data;
	char	*qname;

	/*
 	 * retrieve either the QSUB name or the LOGNAME 
	 */

	if ((qname = getenv("QSUB_REQNAME")) == NULL)
		qname	= getenv("LOGNAME");
	/*
 	 * place name in integer for numeric variable target
	 */

	(void) memcpy((char *) &data, qname, sizeof(long));

	/*
 	 * determine if target variable is character
	 */

	if ( _numargs() > 0 ) {

#ifdef  _ADDR64
		if (_numargs() * sizeof(long) >= sizeof(_fcd)) {
#else
		if (_isfcd(name)) {
#endif
			/* destination is a character */
			unsigned int	nlen, qlen;
			char		*cp;
			_fcd		fcdtemp;

			qlen	= strlen(qname);
			fcdtemp = *(_fcd *)&name;
			cp	= _fcdtocp(fcdtemp);
			nlen	= _fcdlen(fcdtemp);

			(void) strncpy(cp, qname, nlen);

			if (qlen < nlen)	/* Pad with blanks */
				(void) memset(cp + qlen, BLANK, nlen - qlen);
		}
		else {			/* Destination is hollerith */
			long	*lp;

			lp	= (long *) (name);
			*lp	= data;
		}
	}

	return (data);
} 
