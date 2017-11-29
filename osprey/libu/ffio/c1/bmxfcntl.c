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


static char USMID[] = "@(#) libu/ffio/c1/bmxfcntl.c	92.0	10/08/98 14:57:41";

#include <ffio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/iosw.h>
#include <errno.h>
#include "bmxio.h"
#include <sys/tpdctl.h>

/* 
 * Make the special end of volume routines soft externals.
 * C users must call ffsetsp to initiate special eov processing.
 */


#pragma soft _bmx_endsp
extern int _bmx_endsp();

#pragma soft _bmx_startsp
extern int _bmx_startsp();

#pragma soft _bmx_setsp
extern int _bmx_setsp();

/*
 * bmx fcntl requests
 *
 * Parameters:
 *	fio	- fdinfo pointer
 *	cmd	- command code
 *	arg	- command specific parameter
 *	stat	- pointer to status return word
 */
_bmx_fcntl(fio, cmd, arg, stat)
struct fdinfo *fio;
int cmd, arg;
struct ffsw *stat;
	{
	struct ffc_info_s *ffcp;
	int ret,  n;
	long s;
	BMXFIL *f;
	struct ffc_gettp_s *gtptr;
	struct ffc_baddata_s *bdptr;
	struct ffc_chktp_s *chktpptr;

	f = (BMXFIL *)fio->lyr_info;

	ret = 0;
	switch(cmd)
		{
		case FC_GETINFO:
			ffcp = (struct ffc_info_s *)arg;
			ffcp->ffc_flags = 
				FFC_REC |	/* records */
				FFC_WEOD |	/* can weod */

				FFC_BKSP |	/* can backspace */
				FFC_POSREC |	/* can position by record no. */
				FFC_POSFIL |	/* can position by EOF mark */
				FFC_RWND |	/* can rewind */

				FFC_FIXD |	/* can do fixed len recs */
				FFC_VAR |	/* can do variable len recs */
				FFC_BINARY |	/* can do binary */
				FFC_CODED |	/* can do chars */
				FFC_BKFIL |	/* can do backfile */
				FFC_BKFIL |	/* can do backfile */

				FFC_SEQ |	/* can do seq */
				FFC_WRTRUNC |	/* write implies truncation */
				FFC_NOTRN |	/* does no transformation */
				FFC_SKIPBAD |	/* can skip bad data */
				0;
			if ( f->bmx_rwtpmk)
				ffcp->ffc_flags |= FFC_WEOF;
	
			ffcp->ffc_gran = 8;	/* granularity is 1 byte */
			ffcp->ffc_reclen = 0;	/* no rec length */
			ffcp->ffc_fd = f->bmx_fd; /* fd from this layer */
			break;
		case FC_STAT:
			ret = fstat(f->bmx_fd, arg);
			if (ret < 0)
				stat->sw_error = errno;
			break;
		case FC_RECALL:
			break;
		case FC_AUTOBAD:
			/* automatically handle bad data according to */
			/* the argument */
			if (arg == AUTO_SKIP)
				f->bmx_flag |= BMXIO_SKIP;
			else if (arg == AUTO_ACPT)
				f->bmx_flag |= BMXIO_ACPT;
			else if (arg == AUTO_SKIPALL)
				f->bmx_flag |= BMXIO_SKIP | BMXIO_SKIPALL;
			else
				ERETURN(stat,FDC_ERR_REQ ,0);
			break;
		case FC_ACPTBAD:
			/* accept bad data */
			/* Arg is a pointer to struct ffc_baddata_s */
			bdptr = (struct ffc_baddata_s *)arg;
			if (bdptr->ffc_maxflag)
				n = _bmx_bdr(f, (char *)bdptr->ffc_uda, &s,
					BMX_ACPTBAD, bdptr->ffc_maxwords);
			else
				n = _bmx_bdr(f, (char *)bdptr->ffc_uda, &s,
					BMX_ACPTBAD);
			bdptr->ffc_termcnd = s;	
			bdptr->ffc_bytes = n;	
			break;
		case FC_SKIPBAD:
			/* skip bad data */
			/* Arg is a pointer to struct ffc_baddata_s. */
			bdptr = (struct ffc_baddata_s *)arg;
			n = _bmx_bdr(f, NULL, &s , BMX_SKIPBAD);
			bdptr->ffc_blocks  = n;
			bdptr->ffc_termcnd = s;		
			break;
		case FC_GETTP:
			/* get tape info */
			/* Arg is a pointer to structure gettp. */

			gtptr = (struct ffc_gettp_s *)arg;
			if (_bmx_gtpos(f, gtptr->ffc_pa, gtptr->ffc_glen,
				gtptr->ffc_synch) != 0)
				ERETURN(stat, errno, 0);
			break;
		case FC_CHECKTP:
			/* check for end of volume */
			chktpptr = (struct ffc_chktp_s *)arg;
			if (_bmx_checktp(f, &s, (long *)&n) != 0)
				ERETURN(stat, errno, 0);
			chktpptr->stat = s;
			chktpptr->libblk = n;
			break;
		case FC_ENDSP:
			if (_bmx_endsp(f) != 0)
				ERETURN(stat, errno, 0);
			break;
		case FC_STARTSP:
			if (_bmx_startsp(f) != 0)
				ERETURN(stat, errno, 0);
			break;
		case FC_CLOSEV:
			if (_bmx_closev(f) != 0)
				ERETURN(stat, errno, 0);
			break;
		case FC_SETSP:
			if (_bmx_setsp(f, arg) != 0)
				ERETURN(stat, errno, 0);
			break;
		case FC_TSYNC:
			if (_bmx_gtpos(f, NULL, 0, 1) != 0)
				ERETURN(stat, errno, 0);
			break;
		case FC_TPC_SDBSZ:
			/* A no-op if not er90. */
			if (f->er90) {
				if (f->bmx_tpos) {/* wait for positioning to finish */
					if (_tape_tpwait(f->bmx_fd, &(f->bmx_tpos)) != 0)
						ERETURN(stat, errno, 0);
				}
				if (fio->rwflag != POSITIN) {
					/* If we were writing, flush lists */
					/* If we were reading, take care of read-ahead */
					if (_bmx_stpos(f, FP_TPOS_BACK,0,0,0,0))
						ERETURN(stat, errno, 0);
				}
				if (ioctl(f->bmx_fd, TPC_SDBSZ, arg) < 0)
					ERETURN(stat, errno, 0);
			}
			break;
		default:
			ERETURN(stat, FDC_ERR_NOSUP, 0);
		}
	return(ret);
	}
