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


static char USMID[] = "@(#) libu/ffio/c1/er90bpos.c	92.0	10/08/98 14:57:41";


#include  <ffio.h>
#include  <sys/types.h>
#include  <sys/param.h>
#include  <sys/tpdctl.h>
#include  <sys/file.h>
#include  <sys/iosw.h>
#include  <sys/stat.h>
#include  <sys/jtab.h>
#include  <fcntl.h>
#include  <stdio.h>
#include  <tapereq.h>
#include  <errno.h>
#include  "er90by.h"

/*
 * _er90b_pos() ER90 byte stream positioning requests
 */

int
_er90b_pos(struct fdinfo *fio, int cmd, long *arg, int len, struct ffsw *stat)
{
int ret;
ER90BYT *f;
struct dmn_comm pos;
struct ctl_abspos ctl;
struct ffp_settp_s  *spos;

	f = (ER90BYT *)fio->lyr_info;

	/* The caller is responsible for waiting for outstanding I/O */

	if (f->tpos) {
		if ((ret = _tape_tpwait(f->fd, &(f->tpos))) != 0)
			return(-1);
	}
	

	switch(cmd) {
#ifdef _CRAYMPP
		case FP_SETTP:
			spos = (struct ffp_settp_s *)arg;
			if (_er90b_stpos(fio, spos->ffp_nbs_p, spos->ffp_nb, 
				spos->ffp_nvs_p, spos->ffp_nv, spos->ffp_vi))
				ERETURN(stat,errno,0);
			fio->rwflag = POSITIN;
			break;	
#endif
#ifndef _CRAYMPP
		case FP_GETPOS:
			if (len < 4) {
				ERETURN(stat, FEBIOSNT, 0);
			}
			if (fio->rwflag == WRITIN) {
				if (_tape_sync(f->fd) != 0) {
					ERETURN(stat, errno, 0);
				}
			}
			else if (fio->rwflag == READIN) {
				/* The TPC_GABS ioctl does account for */
				/* data in the controller buffer, but does */
				/* not account for data in the system buffer */
				/* So, we do a position request to take care */
				/* of that. */
				if (_er90b_relpos(f->fd,0) < 0)
					ERETURN(stat, errno, 0);
				fio->rwflag = POSITIN;
			}
			if (ioctl(f->fd, TPC_GABS, &ctl) < 0)
				ERETURN(stat, errno, 0);
			*arg = ctl.datablock;	
			*(arg+1) = ctl.absaddr;	
			*(arg+2) = ctl.partition;	
			*(arg+3) = ctl.filesec;	
			break;
		case FP_SETPOS:
			if (len < 4) {
				ERETURN(stat, FEBIOSNT, 0);
			}
			ctl.datablock = *arg;	
			ctl.absaddr = *(arg+1);	
			ctl.partition = *(arg+2);	
			ctl.filesec = *(arg+3);	
			pos.POS_REQ = TR_PABS;
			pos.POS_ABSADDR = (int)&ctl;
			if (ioctl(f->fd, TPC_DMN_REQ, &pos) < 0)
				ERETURN(stat, errno, 0);
			if (ioctl(f->fd, TPC_DMN_REP, &pos) < 0)
				ERETURN(stat, errno, 0);
			if (pos.POS_REP != 0) {
				ERETURN(stat, pos.POS_REP, 0);
			}
			fio->rwflag = POSITIN;
			break;
		case FP_RSEEK:
			if (_er90b_relpos(f->fd,*(arg)) < 0)
				ERETURN(stat, errno, 0);
			fio->rwflag = POSITIN;
			break;
		case FP_GABS:
			/* This relies on struct ffp_pos matching ctl_abspos */
			if (fio->rwflag == WRITIN) {
				if (_tape_sync(f->fd) != 0) {
					ERETURN(stat, errno, 0);
				}
			}
			if (ioctl(f->fd, TPC_GABS, arg) < 0)
				ERETURN(stat, errno, 0);
			break;
		case FP_SABS:
			pos.POS_REQ = TR_PABS;
			pos.POS_ABSADDR = (int)arg;
			if (ioctl(f->fd, TPC_DMN_REQ, &pos) < 0)
				ERETURN(stat, errno, 0);
			if (ioctl(f->fd, TPC_DMN_REP, &pos) < 0)
				ERETURN(stat, errno, 0);
			if (pos.POS_REP != 0) {
				ERETURN(stat, pos.POS_REP, 0);
			}
			fio->rwflag = POSITIN;
			break;
#endif
		default:
			ERETURN(stat, FDC_ERR_NOSUP, 0);
	}
	return(0);
}


/*
 * Change the tape position by nb bytes. For byte-stream mode, a
 * byte == a block.
 */
int 
_er90b_relpos(int fd, int nb)
{

	struct tpdpos ctl;

	ctl.pos_fcn = TR_PBLKS;
	ctl.pos_count = nb;

	if (ioctl(fd, TPC_SPOS, &ctl) < 0) 
		return(-1);
	if (ioctl(fd, TPC_GPOS, &ctl) < 0) 
		return(-1);
		
	if (ctl.pos_rc != 0) {
		errno = ctl.pos_rc;
		return(-1);
	}
	return(0);
}


