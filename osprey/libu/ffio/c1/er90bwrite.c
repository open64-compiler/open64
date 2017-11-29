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


static char USMID[] = "@(#) libu/ffio/c1/er90bwrite.c	92.0	10/08/98 14:57:41";



#include <errno.h>
#include <ffio.h>
#include "er90by.h"
#include "../sysio.h"

/*
 * ER90 byte stream write
 * _er90b_write() calls the system call write(2), after doing appropriate
 * conversions on the parameters
 */

int
_er90b_write(struct fdinfo *fio, bitptr bufptr, int nbytes, 
	struct ffsw *retstat, int fulp, int *ubc)
{
int ret;
int nbt = 0;	/* number of bytes transferred so far */
int nbreq;	/* number of bytes requested this request */
char *buf;
ER90BYT *f;
int zero = 0;
struct ffsw dumstat;

	buf = BPTR2CP(bufptr);
	if ((BPBITOFF(bufptr) & 7) != 0 || *ubc != 0)
		ERETURN(retstat, FDC_ERR_UBC, 0);

	nbreq = nbytes;
	if (fio->rwflag == POSITIN) {
		f = (ER90BYT *)fio->lyr_info;
		if (f->tpos) {
			ret = _tape_tpwait(f->fd, &(f->tpos));
			if (ret < 0)
				ERETURN(retstat, errno, 0);
		}		
	}
	else if (fio->rwflag == READIN) {
		/* write after read requires position to zero */
		ret = _er90b_pos(fio, FP_RSEEK, &zero, 1,
			&dumstat);
		if (ret < 0) {
			*retstat = dumstat;
			return(ERR);
		}
	}
	if (nbreq > 0) {
again:
		/* It is not safe to reissue the write if it fails */
		/* with EINTR. Some data may have been transferred */
		ret= write(fio->realfd, buf, nbreq);
		if (ret < 0)
			ERETURN(retstat, errno, nbt);
		nbt += ret;
/*
 *		The assumption is made here that the system will never return
 *		zero bytes on a non-zero request without an error!
 */
		if (nbt < nbytes) {
			buf += ret;
			nbreq -= ret;
			goto again;
		}
	}
	else if (nbytes < 0)
		ERETURN(retstat, FDC_ERR_REQ, 0);

	SETSTAT(retstat, FFCNT, nbt);
	fio->rwflag = WRITIN;
	return (nbt);
}

