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


#ifndef _FTRACEBK_
#pragma ident "@(#) libu/trbk/mpp/tracebk.c	92.1	07/01/99 13:49:30"
#endif
#include <stdio.h>
#include <fortran.h>
#include <malloc.h>

#if !defined(_FTRACEBK_)
#pragma _CRI duplicate _tracebk as tracebk
#endif

extern	int	_trbkdpth;		/* Traceback depth */
extern	int	_who_called_me();	/* Traceback routine */

#define	MXNAME	40

/*
 *  Print a traceback (on stdout (or optunit if supplied)),
 *  starting with my caller.  If "depth" argument is not supplied,
 *  25 is used.
 *
 *		 ( <0:  trace depth = 25
 *		 (  0:  Print 1-line "Where am I" message
 *	"depth" -(  1:  Print 1-line trace (caller's name/line number)
 *		 (  2:  Print "Where am I" and 1-line trace
 *		 ( >2:  trace depth (25 if above 50)
 *
 */

int 
#ifdef	_FTRACEBK_
TRACEBK(
	_f_int	*depth,
	_fcd	filenm
)
#else
_tracebk(
	int	depth,
	FILE	*optunit
)
#endif
{
	register int	deep;
	register int	len1, len2;
	register int	narg;
	register int	stat;
	int		addr1, addr2;	/* Address of call */
	int		line;		/* Line number of call */
	char		name1[MXNAME], name2[MXNAME];
	char		address1[24], address2[24];
	struct DSIB	*fp;
	FILE		*stream;

	stream	= stdout;
	deep	= 25;
	stat	= 0;

	if ((narg = _numargs()) > 0) {
#ifdef	_FTRACEBK_
		if (narg > 1) {
			int	namlen;
			char	*namptr;

			namlen	= _fcdlen(filenm);

			if ((namptr = _f2ccpy(filenm)) == (char *) NULL)
				return(1);

			stream	= fopen(namptr, "a");

			free(namptr);

			if (stream == (FILE *) NULL)
				return(1);
		}

		deep	= *depth;
#else
		if (narg > 1)
			stream	= optunit;

		deep	= depth;
#endif
		if (deep > 50)
			deep	= 25;
	}

	switch (deep) {

		case 0:
			len1	= _who_called_me(&line, name1, MXNAME, 1, &addr1);

			name1[len1]	= '\0';

#ifdef	_CRAY1
			(void) sprintf(address1, "0%o%c", addr1 >> 2, 'a' + (addr1 & 03));
#else
			(void) sprintf(address1, "0x%x", addr1);
#endif

			if (len1 == 0)
				(void) fprintf(stream,
					"\n Currently executing at address %s.\n", address1);
			else
				if (line == 0)
					(void) fprintf(stream,
					"\n Currently executing at address %s in routine '%s'.\n",
					address1, name1);
				else
					(void) fprintf(stream,
					"\n Currently executing at line %d (address %s) in routine '%s'.\n",
					line, address1, name1);
			break;

		case 1:
			len1	= _who_called_me(&line, name1, MXNAME, 1, &addr1);
			len2	= _who_called_me(&line, name2, MXNAME, 2, &addr2);

			name1[len1]	= '\0';
			name2[len2]	= '\0';

#ifdef	_CRAY1
			(void) sprintf(address1, "0%o%c", addr1 >> 2, 'a' + (addr1 & 03));
			(void) sprintf(address2, "0%o%c", addr2 >> 2, 'a' + (addr2 & 03));
#else
			(void) sprintf(address1, "0x%x", addr1);
			(void) sprintf(address2, "0x%x", addr2);
#endif

			if (len1 == 0)
				(void) fprintf(stream,
					"\n Currently executing at address %s.\n", address1);
			else
				if (len2 == 0)
					(void) fprintf(stream,
					"\n Routine '%s' called from address %s.\n",
					name1, address2);
				else
					if (line == 0)
					(void) fprintf(stream,
					"\n Routine '%s' called from address %s in routine '%s'.\n",
					name1, address2, name2);
					else
					(void) fprintf(stream,
					"\n Routine '%s' called from line %d (address %s) in routine '%s'.\n",
					name1, line, address2, name2);
			break;

		default:
#ifndef	_FTRACEBK_
			(void) fflush(stream);	/* Flush any pending output */
#endif

			_trbkdpth	= deep;

			if (_trbk(fileno(stream)) < 0)
				stat	= 1;

			break;
	}

#ifdef	_FTRACEBK_
	if (stream != stdout)
		(void) fclose(stream);
#endif

	return(stat);
}
