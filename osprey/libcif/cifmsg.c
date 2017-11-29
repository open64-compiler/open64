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


static char USMID[] = "@(#) libcif/cifmsg.c	30.3	07/26/96 07:19:13";


/*
 * Cif_Msginsert accepts a cif message structure and message text string.  It
 * returns a new text string with the message arguments inserted in place of
 * "%" printf fields in the old string.
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <stdio.h>
#include <string.h>

#include "cif_int.h"

int Cif_Msginsert
#ifdef __STDC__
(char *msgtext, struct Cif_generic *cr, char *newtext, int ntlen)
#else
(msgtext, cr, newtext, ntlen)
char *msgtext;							/* pointer to original message text */	
struct Cif_generic *cr;				/* pointer to cif message structure */
char *newtext;							/* pointer to new message text buffer*/
int ntlen;								/* length of 'newtext' */
#endif
{
	int i, ocptr, argno, nargs;
	char ichr, **args;
	char *fchars = "dgiopuxXfeEgGcsn";

	for (ocptr = 0; ocptr < ntlen; ocptr++) newtext[ocptr] = '\0';
	if (cr->rectype == CIF_MESSAGE) {
		if (_cif_version <= 2) { /* must use v1,2 cif records */
			nargs = CIFMSG1(cr)->nargs;
			args = CIFMSG1(cr)->args;
		}
		else {
			nargs = CIFMSG(cr)->nargs;
			args = CIFMSG(cr)->args;
		}
	}
	else if (cr->rectype == CIF_ND_MSG) {
		nargs = CIFNMSG(cr)->nargs;
		args = CIFNMSG(cr)->args;
	}
	else if (cr->rectype == CIF_C_MESSAGE) {
	  	if (_cif_version == 1) { /* must use v1 cif records */

			nargs = CIFCMSG1(cr)->nargs;
			args = CIFCMSG1(cr)->args;

		}
		else { /* version 2 cif */

			nargs = CIFCMSG(cr)->nargs;
			args = CIFCMSG(cr)->args;

		}
	}
	else
		return (CIF_BADREQ);
	
	argno = ocptr = 0;
	while (ichr = *msgtext++) {
		if (ichr != '%') {
			if (ocptr < ntlen-1) newtext[ocptr++] = ichr;
		}
		else {
			ichr = *msgtext++;
			if (ichr == '%') {
				if (ocptr < ntlen-1) newtext[ocptr++] = ichr;
			}
			else {
				while (strchr (fchars, ichr) == NULL)
					ichr = *msgtext++;
				if (argno >= nargs) {
				  /* something has gone wrong, but do the best we
				     can, so that the user gets some message */
				  (void) strncpy (&newtext[ocptr],"?",1);
				  ocptr += 1;
				  continue;
				  /* return (CIF_BADREQ); gave in previously */
				}
				i = strlen (args[argno]);
				if (ntlen-ocptr-1 < i) i = ntlen-ocptr-1;
				(void) strncpy (&newtext[ocptr], args[argno++], i);
				ocptr += i;
			}
		}
	}
	return (0);
}
