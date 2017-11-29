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


static char USMID[] = "@(#) libu/ffio/c1/bmxopen.c	92.0	10/08/98 14:57:41";

#include <fcntl.h>
#include <sys/types.h>
#include <sys/iosw.h>
#include <sys/bmxctl.h>
#include <sys/cnt.h>
#include <sys/stat.h>
#include <tapereq.h>
#include <errno.h>
#include <liberrno.h>
#include <malloc.h>
#include <ffio.h>
#include <unistd.h>
#include "bmxio.h"
#include "../fxlist.h"
#include "../sysio.h"

DECLARE(TAPE_XLIST);
struct xtr_s TAPE_XLIST_P = { TAPE_XLIST };

/*
 *	open on-line tape file
 */
#define TPC_MULTLIST_SPECIAL (~2) 
#ifndef BXC_MULTLIST
#define BXC_MULTLIST 41
#endif
#define ERET() {goto badret;}

#define MIN(x,y) ((x<=y)?(x):(y))
#define MAX(x,y) ((x<=y)?(y):(x))

_bmx_open(fname, oflag, mode, fio, spec, stat, cbits, cblks, oinf)
char	*fname;
int	oflag, mode, cbits, cblks;
struct	fdinfo *fio;
union spec_u *spec;
struct ffsw *stat;
struct gl_o_inf *oinf;
{
	BMXFIL 	 	*bmx_info;
	struct bmxio	*ptr, *oldptr;
	register int	fd, lsz, n, bs=0; 
	struct bmxctl ctl;
	struct tsdata tsi;
	struct stat x;
	int i,  save_err, totbs;
	int nb = 0;
	char *bufptr;
	struct bmxlist *blptr;
	int maxtot = MAX_DEF_TOTBSIZE;
	int oldmax;

/*
 *	allocate *BMXFIL
 */
	if((bmx_info = (BMXFIL *)calloc(sizeof(*bmx_info),1))==NULL){
		ERETURN(stat, FENOMEMY, 0);
	}

	if ((_numargs() > 8 ) && oinf->alreadyopen)  {
		fd = oinf->fd;
	}
	else {
/*
 *		Open the file.
 */
		oflag = (oflag | O_RAW) & (~O_CREAT);

		LOOP_SYSCALL(fd, open(fname, oflag, 0666));

		if (fd < 0){
			free(bmx_info);
			ERETURN(stat, errno, 0);
		}
	}

  	if ( ioctl( fd, BXC_GETFL, &ctl) < 0 ){
		/* If ioctl fails with ENOTTY, then this */
		/* isn't a tape file. Make the errno more meaningful */
		if (errno == ENOTTY)
			errno = FECONNTP;
		ERET();
	}
 
	if (ctl.tpc_type == DT_ER90) {
		/* this is an er90, not a tape */
		/* We can use the bmx layer if the ER90 was */
		/* mounted with block mode */
		if (ctl.tpc_dbsize == 1) {
			errno = FDC_ERR_NOTAPE;
			ERET();
		}
		bmx_info->er90 = 1;		
	}
 	bmx_info->bmx_mbs = ctl.bxc_mbs;
	bmx_info->bmx_fd = fd;		/* save the file descriptor	*/
	bmx_info->bmx_flag = BMXIO_NXT;
	if ((ctl.bxc_flgs & BXC_MODELE) != 0){
		bmx_info->bmx_flag |= BMXIO_MODELE;
		/* If connected to a MODELE IOS, initialize variables
	 	 * used in special processing
		 */	
		bmx_info->bmx_totblk = 0;
		bmx_info->bmx_bmblk = 0;
	}
	if (spec->fld.recsize != 0)
		bs = spec[0].fld.recsize * NBPC;

	if (spec->fld.mbs != 0)
		nb = spec[0].fld.mbs;
	else if ((bmx_info->bmx_flag & BMXIO_MODELE) == 0)
		nb = 1;	/* Model D */

        n = rtoc(bmx_info->bmx_mbs);          /* round mbs to sector boundary */
        if ( n > bs ) {
		if (bs == 0) {
			/* No buffer size was specified by the user */
	                bs = n;
			if (nb == 0) {
				/* must be a Model E */
				if (bmx_info->er90 == 1) {
					/* User did not specify buffer size or number of */
					/* buffers */
					if (bs < MAXER90BLK) {
						nb = 2;
						while (bs + n <= MAXER90BUF)
							bs += n;
					}
					else if (bs == MAXER90BLK) {
						nb = 2;
						bs = 2*MAXER90BLK;
					}
					else if (bs > MAXER90BLK) {
						/* this is currently not allowed */
						nb = 1;
					}
				}
				else {
					/* In previous versions of 6.E,  */
					/* the default for Model E systems */
					/* was x buffers of size mbs, where x <= 8 */
					/* and x*mbs was < MAX_DEF_TOTBSIZE.*/
					/* This meant that for mbs = 512 bytes, */
					/* we would get 8 512-byte buffers. */
					/* Now we are making the individual buffers */
					/* larger. Try to minimize the increase*/
					/* in the size of the program */
					/* with the following. What this means */
					/* is that the total buffer size will */
					/* be = maximum (old total buffer size, 2*64K) */
	
					oldmax = MIN(8*n, MAX_DEF_TOTBSIZE);
					maxtot = MAX(oldmax, 2*MIN_DEF_BSIZE);
	
					/* While we have enough buffer space to */
					/* maintain 2 buffers, pack them in */
					while (((bs +n) * 2) <= maxtot)
						bs += n;
				}
			}
			else {
				while (bs + n <= MIN_DEF_BSIZE)
					bs += n;
			}
		}
		else
			bs = n;
	}
	else {
		/*
		 * Buffer size must have been specified by the user.
		 * The amount to read must be expressed in multiples
		 * of mbs. Round bs down so that it is a multiple of mbs.
		 */
		if ((bs % n) != 0)
			bs = (bs/n)*n;
	}
        lsz = btoc(bs);

        if (nb == 0) {
                /* Must be Model E IOS */
				if (bmx_info->er90 == 1) {
					/* user specified buffer size */
					nb = 2;
				}
				else {
                	totbs = bs;
	                nb = 1;
					while (nb < 2 && totbs + bs <= maxtot) {
						nb++;
						totbs += bs;
					}
                }
        }

/* 
 * 	Initialize a linked list of structures that describe
 * 	outstanding i/o.
 */
	if ((ptr = (struct bmxio *)calloc(nb * sizeof(*ptr),1)) == NULL) {
			errno = FENOMEMY;
			ERET();
	}
	/* allocate space for data buffers. */
	if ((bufptr = (char *) malloc(bs * nb)) == NULL) {
			free(ptr);
			errno = FENOMEMY;
			ERET();
	}
	/* allocate space for the lists */
	if ((blptr = (struct bmxlist *)calloc(lsz * nb, NBPW)) == NULL) {
			free(ptr);
			free(bufptr);
			errno = FENOMEMY;
			ERET();
	}
	oldptr = NULL;
	/* Save ptr so we can later free it. We need to do this because
	 * eov processing can change the order of these pointers (i.e.,
         * change what is in bmx_iofirst).
	 */
	bmx_info->bmxio_alloc = ptr;
	for (i = 0; i < nb; i++){
		if (oldptr != NULL){
			oldptr->bmx_nxt = ptr;
		}else {
			bmx_info->bmx_iocurrent = bmx_info->bmx_iofirst = ptr;
		}
		ptr->bmx_prev = oldptr;
		ptr->bmx_busy = INACTIVE;
	        ptr->bmx_base = bufptr;
/*
 *      Set asynch status to done in anticipation of first i/o
 *      request.
 */
        	ptr->bmx_iosw.sw_flag = 0;
		ptr->bmx_list = blptr;
		oldptr = ptr;
		ptr++;
		blptr += lsz;
		bufptr += bs;
	}
	bmx_info->bmx_iofirst->bmx_prev = oldptr;
	oldptr->bmx_nxt = NULL;	/* Last one in linked list */
/*
 *      Initialize i/o buffer pointers.
 */
        bmx_info->bmx_bufptr = 0;
        
        bmx_info->bmx_bufsz  = bmx_info->bmx_bufszsv = bs;
        bmx_info->bmx_bufcnt = bs;
        bmx_info->bmx_recl = 0;
	bmx_info->bmx_tpos = TPOS_DONE;
 
/*
 *	Tell the driver we want tapelist i/o
 */
        ctl.bxc_address = (long) &bmx_info->bmx_listptr;
        ctl.bxc_count   = lsz;
        if ( ioctl( bmx_info->bmx_fd, BXC_MULTLIST, &ctl) < 0 ){
/*
 * 	The ioctl(.,BXC_MULTLIST,.) won't be available on Unicos/Mk
 *	and may not be available in Unicos 10.0. Issue the alternate
 *      that will be available on those systems.
 */
		if (ioctl( bmx_info->bmx_fd, TPC_MULTLIST_SPECIAL, &ctl) < 0 ){
			ERET();
		}
	}
 
	F_TRACE("list", bmx_info->bmx_listptr, lsz, 0);
 
/*
 *      Initialize list pointers.
 */
        bmx_info->bmx_lstptr = bmx_info->bmx_iocurrent->bmx_list;
        bmx_info->bmx_lstcnt = lsz;
        bmx_info->bmx_lstsz  = lsz;

/*
 *	Save device number and inode for tapeinfo requests
 */
	if (fstat(bmx_info->bmx_fd, &x) < 0){
		ERET();
	}

	bmx_info->tsireq.st_dev = x.st_dev;
	bmx_info->tsireq.st_ino = x.st_ino;

	bmx_info->tsireq.fd = fd;
#ifndef _CRAYMPP
	if (sysconf(_SC_CRAY_RELEASE) >= 8300) 
#endif
	{
		bmx_info->tsireq.ioctlreq = 1;
	}

/*
 *	Can we read/write tape marks? (was tpmnt -T used? )
 */
	if (_tape_tptsi(&(bmx_info->tsireq), &tsi, NULL)){
		ERET();
	}

	bmx_info->bmx_rwtpmk = tsi.ts_urwtm;

	fio->lyr_info = (char *)bmx_info;
	fio->realfd = bmx_info->bmx_fd;
	fio->rwflag = POSITIN;
	SETSTAT(stat, 0, 0);
	return(0);	/*  return pointer to bmxio file table  */

badret:
	save_err = errno;
	if (bmx_info->bmxio_alloc != NULL){
		free(bmx_info->bmxio_alloc->bmx_base);
		free(bmx_info->bmxio_alloc->bmx_list);
		free(bmx_info->bmxio_alloc);
	}
	free(bmx_info);
	(void) close(fd);
	errno = save_err;
	ERETURN(stat, errno, 0);
}	
 
/* Determine whether the device is a ER90 or round/cartridge tape. */
/* Returns -1 if open failed. Otherwise, returns correct CLASS */
/* Sets the fd field in the gl_o_info structure to be the file descriptor */
/* of the opened file */
int
_er90_bmx(const char *name, int oflag, struct gl_o_inf *oinf)
{
	int fd;
	struct bmxctl ctl;

	oflag = (oflag | O_RAW) & (~O_CREAT);
	LOOP_SYSCALL(fd,open(name, oflag, 0666));
	if (fd < 0) {
		return(-1);
	}
	if (ioctl(fd,BXC_GETFL,&ctl) < 0) {
		if (errno == ENOTTY){
			errno = FECONNTP;
		}
		close(fd);
		return(-1);
	}
	oinf->fd = fd;
	if ((ctl.tpc_type == DT_ER90) && (ctl.tpc_dbsize == 1)) {
		return(CLASS_ER90B);
	}
	else {
		return(CLASS_BMX);
	}
}

