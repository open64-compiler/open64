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


static char USMID[] = "@(#) libf/tape/c1/acptbad.c	92.0	10/08/98 14:30:10";

#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include "fio.h"

/*
 *	accept bad data
 */

void
ACPTBAD(unump, uda, wrdcnt, termcnd, ubcnt, acptcount)
long *unump, *uda, *wrdcnt, *termcnd, *ubcnt, *acptcount;
{
	int  ret;
	unit *cup;
	int nargs;
	struct ffc_baddata_s bddata;
	
	nargs = _numargs();

	STMT_BEGIN(*unump, 0, T_TAPE, NULL, NULL, cup);
/*
 *	If not connected, do an implicit open.  Abort if the open fails.
 */
	if (cup == NULL)
		cup = _implicit_open(SEQ, UNF, *unump, 0, NULL);

	switch(cup->ufs) {
		case FS_FDC:
			bddata.ffc_uda = (long)uda;
			if (nargs > 5) {
				bddata.ffc_maxflag = 1;
				bddata.ffc_maxwords = *acptcount;
			}
			else
				bddata.ffc_maxflag = 0;
			ret = XRCALL(cup->ufp.fdc, fcntlrtn) cup->ufp.fdc,
				FC_ACPTBAD, &bddata, &cup->uffsw);
			if (ret < 0)
				*termcnd = -cup->uffsw.sw_error;
			else {
				*wrdcnt = (bddata.ffc_bytes+7) >>3;
				if (nargs > 4) {
					if (bddata.ffc_bytes & 07)
						*ubcnt = 64-(bddata.ffc_bytes &07)<<3;
					else
						*ubcnt = 0;
				}
				*termcnd = bddata.ffc_termcnd;
			}
			break;
		default:
			*termcnd = -FECONNTP;
	}

	STMT_END(cup, T_TAPE, NULL, NULL);
	return;
}
