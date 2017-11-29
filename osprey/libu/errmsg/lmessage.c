/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

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
#ifdef KEY
/* DON'T ADD ANY #include HERE */
/* On Fedora Core 5, with gcc 4.1.1 installed, and "-D_XOPEN_SOURCE{,_EXTENDED}"
 * on the command line, "pathcc -gnu40" gives an "implicit declaration" error
 * for snprintf. Using -std=c99 fixes the problem, but applying that to the
 * entire libu requires zillions of fixes to obsolete C idioms (e.g., implicit
 * "int" typing of functions) plus a couple of FP-related changes that might
 * break things. I have no idea why _XOPEN_SOURCE and _XOPEN_SOURCE_EXTENDED
 * are required (perhaps stuff related to "struct timeval" in other source
 * files?) but we don't want it here.
 */
#undef _XOPEN_SOURCE
#undef _XOPEN_SOURCE_EXTENDED
#endif /* KEY */

#pragma ident "@(#) libu/errmsg/lmessage.c	92.4	10/14/99 17:05:30"
#include <liberrno.h>
#include <errno.h>
#include <fcntl.h>
#if !defined(_ABSOFT)
#include <nl_types.h>
#else
#include "ac_msg_type.h"
#endif
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#if defined(_ABSOFT) && defined(TARGET_NT)
#include <siohdr.h>
#endif
#if	defined(_LITTLE_ENDIAN)
#include <cray/nlcatmsg.h>
#endif
#include <unistd.h>

#ifdef	_UNICOS

#include <tapereq.h>

extern	char	**_argv;
#define	CMDNAME	(*_argv)

#else

#include <cray/portdefs.h>
#if	defined(_LITTLE_ENDIAN)
#include <cray/nlcatmsg.h>
#endif

#define	CMDNAME	""

#endif

extern	char	*libu_defgets(int msg_num);

/*
 *	_lmessage - run-time library message processor
 *
 *		errn		Message number
 *		severity	Pointer to message severity; NULL if
 *				unrecoverable error message
 */

void
_lmessage(int errn, char *severity, va_list args)
{
	register int	flmn;		/* library message number 	*/
	char		mbuf[MAXMLN+1];	/* Message buffer		*/
	char		tbuf[MAXMLN+1];	/* Temporary message buffer	*/
	char		*mcnm;		/* Pointer to msg. catalog name	*/
	char		*bmsg;		/* Pointer to back-up message	*/
	char		*smsg;		/* Pointer to severity text	*/
	char		*dmsg;		/* Pointer to default message text	*/

#ifdef	_UNICOS
	if (_numargs() == 0)
		(void) abort();
#endif

	mbuf[0]	= '\n';
	mbuf[1]	= '\0';
	bmsg	= NULL;		/* Assume no back-up message */
	smsg	= "";		/* Assume unset severity */
	flmn	= errn;

	/*
	 * Map -1 errors to FERDPEOF (temporarily, we hope)
	 * Map all negative errors to positive errors.
	 */

	if (flmn < 0)
		if (flmn == -1)
			flmn	= FERDPEOF;
		else
			flmn	= -flmn;

	if (flmn < BASE) {
		mcnm	= "sys";
#ifdef KEY /* Bug 4452 */
		smsg	= "UNRECOVERABLE error on system request\nlib-%d: %s";
#else /* KEY Bug 4452 */
		smsg	= "UNRECOVERABLE error on system request";
#endif /* KEY Bug 4452 */
#ifdef	_UNICOS
		/*
		 * Provide a back-up error message for those errors
		 * where we expect that the message system will fail
		 * to retrieve a message.
		 */

		if (flmn == ENOMEM)
			bmsg	= "Unable to allocate memory.";

		if (flmn == EMFILE)
			bmsg	= "Too many open files.";
#else
		/*
		 * Use strerror() to provide a back-up error message
		 * for 'sys' errors on non-UNICOS systems.  We don't
		 * want to load strerror() on UNICOS systems, however.
		 */

		bmsg	= strerror(flmn);
#ifdef KEY /* Bug 4452 */
		/* Write these messages and return, because we won't be able
		 * to find flmn in the message catalog.
		 */
		fprintf(stderr, smsg, flmn, bmsg);
		return;
#endif /* KEY Bug 4452 */
#endif
	}
	else
		if (flmn < (BASE+999) ||
		    (flmn >= FDC_ERRB && flmn < (FDC_ERRB+999))) {
			mcnm	= FEMCN;	/* Library catalog name */
			smsg	= "UNRECOVERABLE library error";

			/*
			 * Provide a minimal back-up message for
			 * out-of-memory errors.
			 */

			if (flmn == FENOMEMY || flmn == FEFMTMEM ||
			    flmn == FDC_ERR_NOMEM)
				bmsg	= "Unable to allocate memory.";
		}
#ifdef	_UNICOS
		else
			if (flmn >= ETFIRST && flmn <= ETLAST) {
				mcnm	= "tape";
				smsg	= "UNRECOVERABLE tape error";
				bmsg	= "Consult Appendix C, Tape Subsystem User's Guide, SG-2051";
			}
#endif
			else {
				mcnm	= "unknown";
				smsg	= "UNRECOVERABLE";
				bmsg	= "Unrecognized error number";
			}

	if (bmsg != NULL)	/* Set back-up message, if extant */
		(void) strcpy(&mbuf[1], bmsg);

	if (severity != NULL)
		smsg	= severity;

	/* Retrieve the raw message text */

	dmsg = libu_defgets(flmn);
	if (dmsg != NULL) {
		size_t len;

		len = strlen(dmsg); 
		if (len >= MAXMLN)
			len = MAXMLN - 1;

		(void) strncpy(&mbuf[1], dmsg, len);
		mbuf[len + 1] = '\0';
	}

	/* Edit the message */

	(void) vsprintf(tbuf, &mbuf[1], args);

	/* Format the message */

	(void) catmsgfmt(CMDNAME, mcnm, flmn, smsg, tbuf, &mbuf[1],
			 MAXMLN, NULL, NULL);

	/* Print the formatted message */

#if !defined(_ABSOFT)
	(void) write(fileno(stderr), mbuf, strlen(mbuf));
#else
	fwrite(mbuf, strlen(mbuf), 1, stderr);
#endif

	return;
}
