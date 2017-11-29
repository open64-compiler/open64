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

#pragma ident "@(#) libu/clib/uname.c	92.1	07/01/99 13:42:20"

#include <fortran.h>
#include <string.h>
#include <sys/utsname.h>

/*
 *	UNAME	Returns name(s) of current operating system
 *
 *	Call from Fortran:
 *
 *		CHARACTER * (*), SYS, NODE, REL, VER, MACH
 *			...
 *      	CALL UNAME(SYS, NODE, REL, VER, MACH)
 *
 *			- or -
 *
 *		CHARACTER * (*), SYS, NODE, REL, VER, MACH
 *		INTEGER UNAME
 *			...
 *		I = UNAME(SYS, NODE, REL, VER, MACH)
 *
 *			- or -
 *
 *		INTEGER SYS, NODE, REL, VER, MACH
 *			...
 *      	CALL UNAME(SYS, NODE, REL, VER, MACH)
 *
 *			- or -
 *
 *		INTEGER SYS, NODE, REL, VER, MACH, UNAME
 *			...
 *		I = UNAME(SYS, NODE, REL, VER, MACH)
 *
 *	Result:
 *
 *		SYS	Variable of type CHARACTER or INTEGER to receive
 *			the name of the current operating system
 *
 *		NODE	Variable of type CHARACTER or INTEGER to receive
 *			the name by which this system is known on a
 *			communications network
 *
 *		REL	Variable of type CHARACTER or INTEGER to receive
 *			the name of the operating system release
 *
 *		VER	Variable of type CHARACTER or INTEGER to receive
 *			the name of the operating system release version
 *
 *		MACH	Variable of type CHARACTER or INTEGER to receive
 *			the name of the hardware on which this system is
 *			running
 *
 *		I	Return value (type INTEGER):
 *			 -2	UNAME called with too many parameters
 *				(the first 5 parameters are still set)
 *			 -1	uname system call failed
 *			  0	UNAME called with no parameters
 *			  1-5	UNAME returned 1-5 values
 *
 *		The character variables will receive as much of the
 *		appropriate field as will fit; if the field is longer
 *		it will be truncated, if shorter the character variable
 *		will be padded with blanks.
 *
 *		The integer variables will receive no more than the first
 *		eight characters of the appropriate field, left-justified
 *		and zero-filled.
 *
 *		Refer to the UNAME(2) man page for a description of the
 *		contents and length of the various uname fields.
 */

/*
 *	blank_fill	Change nulls to blanks in string s of length l.
 */

static void
blank_fill(string, length)
char		*string;
unsigned int	length;
{
	register int	i;

	for (i = 0; i < length; i++)		/* For each element */
		if ( string[i] == '\0' )
			string[i]	= ' ';

	return;
}

_f_int
UNAME (sys, node, rel, ver, mach)
long	sys;
long	node;
long	rel;
long	ver;
long	mach;
{
	char		*str;
	int		rtn;
	unsigned int	cnt, sln;
	struct utsname	utn;

	cnt	= _numargs ();
	rtn	= uname (&utn);

	if ( rtn >= 0 )		/* Set number of parameters */
		rtn	= cnt;

	switch ( cnt ) {

		default:			/* Too many arguments */
			if ( rtn > 0 )
				rtn	= -2;

		case 5:					/* Machine name */
			if (_isfcd(mach)) {	/* If Fortran character */
				_fcd	fmach;

				fmach	= *(_fcd *) &mach;
				str	= _fcdtocp(fmach);
				sln	= _fcdlen (fmach);
			}
			else {		/* Return hollerith */
				str	= (char *) mach;
				sln	= sizeof(long);
			}

			(void) strncpy (str, utn.machine, sln);

			if (_isfcd(mach))
				blank_fill(str, sln);

		case 4:					/* Version */
			if (_isfcd(ver)) {	/* If Fortran character */
				_fcd	fver;

				fver	= *(_fcd *) &ver;
				str	= _fcdtocp(fver);
				sln	= _fcdlen (fver);
			}
			else {		/* Return hollerith */
				str	= (char *) ver;
				sln	= sizeof(long);
			}

			(void) strncpy (str, utn.version, sln);

			if (_isfcd(ver))
				blank_fill(str, sln);

		case 3:					/* Release */
			if (_isfcd(rel)) {	/* If Fortran character */
				_fcd	frel;

				frel	= *(_fcd *) &rel;
				str	= _fcdtocp(frel);
				sln	= _fcdlen (frel);
			}
			else {		/* Return hollerith */
				str	= (char *) rel;
				sln	= sizeof(long);
			}

			(void) strncpy (str, utn.release, sln);

			if (_isfcd(rel))
				blank_fill(str, sln);

		case 2:					/* Node name */
			if (_isfcd(node)) {	/* If Fortran character */
				_fcd	fnode;

				fnode	= *(_fcd *) &node;
				str	= _fcdtocp(fnode);
				sln	= _fcdlen (fnode);
			}
			else {		/* Return hollerith */
				str	= (char *) node;
				sln	= sizeof(long);
			}

			(void) strncpy (str, utn.nodename, sln);

			if (_isfcd(node))
				blank_fill(str, sln);

		case 1:					/* System name */
			if (_isfcd(sys)) {/* If a Fortran character pointer */
				_fcd	fsys;

				fsys	= *(_fcd *) &sys;
				str	= _fcdtocp(fsys);
				sln	= _fcdlen (fsys);
			}
			else {		/* Return hollerith */
				str	= (char *) sys;
				sln	= sizeof(long);
			}

			(void) strncpy (str, utn.sysname, sln);

			if (_isfcd(sys))
				blank_fill(str, sln);

		case 0:					/* No arguments */
			break;

	}

	return( (_f_int) rtn);
}
