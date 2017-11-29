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


static char USMID[] = "@(#) libf/tape/c1/setsp.c	92.0	10/08/98 14:30:10";

#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include "fio.h"

#ifndef __mips
extern void _eov_load();
#endif

/*
 *	SETSP - Initializes/terminates special EOV processing
 *
 *	unump - name or unit # of file
 *
 *	iflag
 *	  0 - Turn BOV/EOV processing OFF
 *	  nonzero - Turn BOV/EOV processing ON
 *
 *      istat - status
 *	  0 - OK
 *        nonzero - error
 */

void
SETSP(_f_int *unump, _f_int *iflag, _f_int *istat)
{
	register int	ret;
	FIOSPTR		css;
	unit		*cup;

	GET_FIOS_PTR(css);
	STMT_BEGIN(*unump, 0, T_TAPE, NULL, css, cup);
/*
 *	If not connected, do an implicit open.  Abort if the open fails.
 */
	if (cup == NULL)
		cup	= _imp_open(css, SEQ, UNF, *unump, 0, NULL);

	*istat	= 0;

	if (cup->ufs == FS_FDC) {
#ifndef __mips
		_eov_load(1);	/* Call a routine that provides hard */
				/* references to eov routines. */
#endif
		ret	= XRCALL(cup->ufp.fdc, fcntlrtn) cup->ufp.fdc,
				FC_SETSP, (void *)*iflag, &cup->uffsw);
		if (ret < 0)
			*istat	= cup->uffsw.sw_error;
	}
	else 
		_ferr(css, FECONNTP);	

	STMT_END(cup, T_TAPE, NULL, css);

	return;
}
