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


static char USMID[] = "@(#) libu/ffio/c1/bmxstpos.c	92.0	10/08/98 14:57:41";


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
#include  "bmxio.h"
#include  <liberrno.h>
#include  <stdarg.h>
 
#define RETURN(x) { ret = x; goto done;}

static int bmx_tpv(int fd, struct bmxpos *ctl);
static int bmx_tpblk(int fd, int nb, int lib_tpmk,int *usertm);

extern int G@INTIO;

/*
 *	Set tape position
 */

_bmx_stpos(BMXFIL *f, long nbs, long nb, long nvs, long nv, long vi, ...)
{
	register int	  nb_request, lstcnt, lib = 0;
	struct bmxlist	*lstptr;
	long	pa[12];
	struct tsdata tsi;
	int lib_tpmk = 0;
	int ret = 0;
	struct bmxio *ptr;
	struct bmxpos ctl;
	int *usertm;
	int zro;
	va_list args;

	if (_numargs() > 6) {
		va_start(args, vi);
		usertm = va_arg(args, int *);
		va_end(args);
	}
	else
		usertm = &zro;
	*usertm = 0;
	if ( f->bmx_tpos ) {
		if( _tape_tpwait( f->bmx_fd, &(f->bmx_tpos) ) != 0)
			RETURN(-1); 
	}

/*
 *      Make sure that the i/o is quiet before doing the positioning.
 */
	if(_bmx_quiet(f)){
		RETURN(-1);
	}
	
/*
 *	If previous i/o request was a write request make sure the
 *	list is flushed before positioning.
 */
	if (__bmxflush(f)){
		RETURN(-1);
	}
/*
 * 	If reading, count the blocks in the library buffers
 */
        if ( ((f->bmx_flag & BMXIO_RW) == 0) && (f->bmx_iocurrent->bmx_busy == DATA)){
		/* First count all the blocks in the current list */
		lstcnt = f->bmx_lstcnt;
		lstptr = f->bmx_lstptr;
/*
 *		If we're in the middle of a block, and we are positioning
 *		backwards, current block counts as 1 to be positioned over.
 */
		if (nb != 0 && nbs == FP_TPOS_BACK){
			if (((f->bmx_flag & BMXIO_NXT) == 0) && lstcnt > 0){
				lstcnt--;
				lstptr++;
			}
		}
		while (lstcnt != 0) {
			if (lstptr->state == 0)
				break;
			if (lstptr->state == BMS_EOD){
				if(_bmx_clrerr(f) < 0)
					RETURN(-1);
				break;
			}
			if (lstptr->state == BMS_EOF){
				lib_tpmk++;
				if(_bmx_clrerr(f) < 0)
					RETURN(-1);
			}
			lstcnt--;
			lstptr++;
			lib++;
		}
		/* Then count all the outstanding blocks from read-ahead */
		ptr = f->bmx_iofirst;
		while (ptr != NULL){
			if (ptr != f->bmx_iocurrent){
				if (ptr->bmx_busy ==DATA){
					lstcnt = f->bmx_lstsz;
					lstptr = ptr->bmx_list;
					while (lstcnt != 0) {
						if (lstptr->state == 0)
							break;
						if (lstptr->state == BMS_EOD){
							if(_bmx_clrerr(f) < 0)
								RETURN(-1);
							break;
						}
						if (lstptr->state == BMS_EOF){
							lib_tpmk++;
							if(_bmx_clrerr(f) < 0)
								RETURN(-1);
						}
						lstcnt--;
						lstptr++;
						lib++;
					}
				}
			}
			ptr = ptr->bmx_nxt;
		}
	}
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
				if (_bmx_gtpos(f,pa,12,1)!= 0)	
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
		if(bmx_tpv (f->bmx_fd, &ctl )){
			RETURN(-1);
		}

		/* Position to the correct block */
		if ( nb_request != 0 ) {
			if(bmx_tpblk(f->bmx_fd,nb_request,lib_tpmk,usertm)){
				RETURN(-1);
			}
		}

	} else if ( vi != 0 ) {

		ctl.pos_fcn = TR_PVSN;
		ctl.pos_vsn = vi;
		ctl.pos_count = 0;

		if (bmx_tpv (f->bmx_fd, &ctl )){
			RETURN(-1);
		}

		/* Position to the correct block */
		if ( nb_request != 0 ) {
			if(bmx_tpblk(f->bmx_fd, nb_request,lib_tpmk,usertm)){
				RETURN(-1);
			}
		}

	} else {
		if(bmx_tpblk(f->bmx_fd,nb_request,lib_tpmk,usertm)){
			RETURN(-1);
		}
	}

	if ((f->bmx_flag & BMXIO_SPPROC) && (f->bmx_flag & BMXIO_MODELE)){
		if (f->bmx_totblk >= f->bmx_bmblk)
			f->bmx_totblk -= nb_request;
		else{
			/* All 'buffer memory' is gone */
			f->eov_current = NULL;	/* So we don't read */
						/* from library buffer */
			f->bmx_bmblk = 0;
			f->bmx_totblk = -nb_request;
		}	
	}
done:
	_bmx_clear(f);		/* Clear all lists */
	f->bmx_bufptr = 0;      /*  indicate empty buffer   */
	f->bmx_recl   = 0;      /*  and clear record length */
	f->bmx_bufcnt   = f->bmx_bufsz;      /* buffer is empty */
	f->bmx_lstcnt = f->bmx_lstsz;
	return(ret);

}

/*
 * Change the tape position by nb blocks.
 * Parameters:
 *  fd       - file descriptor
 *  nb       - number of blocks to position
 *  lib_tpmk - number of library tape marks we need to position over
 *  usertm  - Set to 1 if we position over a tape mark 
 *     		that was not in the library's read ahead buffer 
 *
 */
static int
bmx_tpblk(int fd, int nb, int lib_tpmk,int *usertm)
{

	struct bmxpos	ctl;

	if ((nb > 0) && (lib_tpmk > 0)){
		/* Library read-ahead has positioned past a */
		/* tape mark, but user wants to position even */
		/* further ahead. */
		errno = ETTMS;
		return(-1);
	}
loop:
	ctl.pos_fcn = TR_PBLKS;
	ctl.pos_count = nb;

	if ( ioctl( fd, BXC_SPOS, &ctl) < 0 )
		return(-1);

	if ( ioctl( fd, BXC_GPOS, &ctl) < 0 )
		return(-1);

         if (ctl.pos_rc == ETTMS){
		/* Tape subsystem will not position beyond a tape mark. */
		/* ctl.pos_count contains residual count */

		if (ctl.pos_count == 0) {
			if (lib_tpmk == 0) {
				/* The tape mark just positioned over */
				/* was not in the library's read ahead */
				*usertm = 1;
			}
			return(0);
		}
		else if (lib_tpmk > 0){
			/* Are we attempting to position over tape marks */
			/* due to the library's read ahead? */
			/* If so, keep going. If not, inform the user by */
			/* passing back an error */
			lib_tpmk--;	

			if (nb < 0){
					nb = -ctl.pos_count;
					goto loop;
				}
			else{
				
				nb = ctl.pos_count;
				goto loop;
			}
		}
	}
	if (ctl.pos_rc != 0){
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
bmx_tpv(int fd, struct bmxpos *ctl)
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
