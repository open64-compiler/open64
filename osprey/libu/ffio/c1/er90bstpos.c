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


static char USMID[] = "@(#) libu/ffio/c1/er90bstpos.c	92.0	10/08/98 14:57:41";

#include  <ffio.h>
#include  <sys/types.h>
#include  <sys/param.h>
#include  <sys/bmxctl.h>
#include  <sys/file.h>
#include  <sys/iosw.h>
#include  <sys/stat.h>
#include  <sys/jtab.h>
#include  <fcntl.h>
#include  <stdio.h>
#include  <tapereq.h>
#include  <errno.h>
#include  "er90by.h"
#include  <liberrno.h>
#include  <stdarg.h>
 
#define RETURN(x) { ret = x; goto done;}

static int _er90_tpv(int fd, struct bmxpos *ctl);
static int _er90_tpblk(int fd, int nb);

extern int G@INTIO;

/*
 *	Set tape position
 */

_er90b_stpos(struct fdinfo *fio, long nbs, long nb, long nvs, long nv, long vi, ...)
{
	register int	  nb_request, lstcnt, lib = 0;
	struct bmxlist	*lstptr;
	long	pa[12];
	struct tsdata tsi;
	int ret = 0;
	struct bmxio *ptr;
	struct bmxpos ctl;
	int zro;
	va_list args;
	ER90BYT *f;


	/* it is the user's responsibility to wait for outstanding i/o */

	f = (ER90BYT *)fio->lyr_info;
	if ( nb != 0 ) {
		if (nbs == FP_TPOS_BACK ) {
			nb = -nb;
		} else if ( nbs == FP_TPOS_ABS ) {
			if ( (nv == 0) && (vi == 0) ){
/*
 *				Convert absolute block number to
 *				relative block number from current
 *				position. First find current position.
 */
				if (_er90b_gettp(fio,pa,12,1)!= 0)	
					RETURN(-1);
/*
 *				The tape subsystem adjusts for the blocks
 *				in the IOS. 
 *				pa[9] = block number. pa[11] = blocks in IOS
 */
				nb = nb -(pa[9]-pa[11]);
			}
			nb -=1;	/* absolute block numbers start with 1 */
			lib = 0;
		}
		nb_request = nb - lib;
	} else {
		if ((nv == 0) && (vi == 0))
			nb_request = nb - lib;
		else
			nb_request = 0;
	}

	if ( nv > 0 ) {
		/* Handle positioning by volume */
		if (nbs != FP_TPOS_ABS){
			errno = FETAPCMB;
			RETURN(-1);
		}
	
		if (nvs != FP_TPOS_ABS) {
			if (_tape_tptsi(&(f->tsireq), &tsi, NULL)) {
				RETURN(-1);
			}
			if (nvs == FP_TPOS_BACK) {
				nv = tsi.ts_cvsn - nv +1;
				if (nv <= 0) {
					errno = FETAPNVY;
					RETURN(-1);
				}
			}
			else { /* must be FP_TPOS_FORW */
				nv = tsi.ts_cvsn + nv +1;
				if (nv > tsi.ts_numvsn) {
					errno = FETAPNVY;
					RETURN(-1);
				}
			}
		}
		ctl.pos_fcn = TR_PVOL;
		ctl.pos_count = nv;
		if(_er90_tpv (f->fd, &ctl )){
			RETURN(-1);
		}

		/* Position to the correct block */
		if ( nb_request != 0 ) {
			if(_er90_tpblk(f->fd,nb_request)){
				RETURN(-1);
			}
		}

	} else if ( vi != 0 ) {

		ctl.pos_fcn = TR_PVSN;
		ctl.pos_vsn = vi;
		ctl.pos_count = 0;

		if (_er90_tpv (f->fd, &ctl )){
			RETURN(-1);
		}

		/* Position to the correct block */
		if ( nb_request != 0 ) {
			if(_er90_tpblk(f->fd, nb_request)){
				RETURN(-1);
			}
		}

	} else {
		if(_er90_tpblk(f->fd,nb_request)){
			RETURN(-1);
		}
	}

done:
	return(ret);

}

/*
 * Change the tape position by nb blocks.
 * Parameters:
 *  fd       - file descriptor
 *  nb       - number of blocks to position
 *
 */
static int
_er90_tpblk(int fd, int nb)
{

	struct bmxpos	ctl;

loop:
	ctl.pos_fcn = TR_PBLKS;
	ctl.pos_count = nb;

	if ( ioctl( fd, BXC_SPOS, &ctl) < 0 )
		return(-1);

	if ( ioctl( fd, BXC_GPOS, &ctl) < 0 )
		return(-1);

	if (ctl.pos_rc != 0){
		/* User tape marks are not supported by this layer yet */
		errno =ctl.pos_rc;
		return(-1);
	}
	else
		return(0);

}

/*
 * Issue a positioning request and wait for the response.
 */
static int
_er90_tpv(int fd, struct bmxpos *ctl)
{

	if ( ioctl( fd, BXC_SPOS, ctl) < 0 )
		return(-1);

	if ( ioctl( fd, BXC_GPOS, ctl) < 0 )
		return(-1);

	if (ctl->pos_rc != 0){
		errno = ctl->pos_rc;
		return(-1);
	}
	return(0);

}
