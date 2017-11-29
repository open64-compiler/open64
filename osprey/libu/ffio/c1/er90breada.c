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


static char USMID[] = "@(#) libu/ffio/c1/er90breada.c	92.0	10/08/98 14:57:41";


#include <sys/types.h>
#include <sys/iosw.h>
#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include "er90by.h"
#include "../sysio.h"

/*
 * ER90 byte stream asynchronous read
 * _er90b_reada() calls the system call reada(2), after doing appropriate
 * conversions on the parameters
 */

int
_er90b_reada(struct fdinfo *fio, bitptr bufptr, int nbytes, 
	struct ffsw *retstat, int fulp, int *ubc)
{
int ret = 0;
char *buf;
ER90BYT *f;

	buf = BPTR2CP(bufptr);
	if ((BPBITOFF(bufptr) & 7) != 0 || *ubc != 0)
		ERETURN(retstat, FDC_ERR_UBC, 0);

	if (fio->rwflag == POSITIN) {
		f = (ER90BYT *)fio->lyr_info;
		if (f->tpos){
			ret = _tape_tpwait(f->fd, &(f->tpos));
			if (ret < 0)
				ERETURN(retstat, errno, 0);
		}
	}
	else if (fio->rwflag == WRITIN) {
		ERETURN(retstat, FDC_ERR_RAWR, 0);
	}

	if (nbytes > 0) {
		CLRFFSTAT(*retstat);	/* flag async in progress */
		ret = reada(fio->realfd, buf, nbytes,
			(struct iosw *)retstat, 0);
		if (ret < 0)
			ERETURN(retstat, errno, 0);
	}
	else if (nbytes < 0) {
		ERETURN(retstat, FDC_ERR_REQ, 0);
	}
	else {	/* nbytes == 0 */
		retstat->sw_flag = 1;
		FFSTAT(*retstat) = FFCNT; /* I/O is done, and other stat */
					/* fields are set. */
	}

	fio->rwflag = READIN;
	return (ret);
}

