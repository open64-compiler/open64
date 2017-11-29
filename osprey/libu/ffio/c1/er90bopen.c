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


static char USMID[] = "@(#) libu/ffio/c1/er90bopen.c	92.0	10/08/98 14:57:41";

#include <fcntl.h>
#include <sys/types.h>
#include <sys/iosw.h>
#include <sys/tpdctl.h>
#include <sys/cnt.h>
#include <sys/stat.h>
#include <tapereq.h>
#include <errno.h>
#include <liberrno.h>
#include <malloc.h>
#include <ffio.h>
#include <unistd.h>
#include <stdlib.h>
#include "er90by.h"
#include "../fxlist.h"


DECLARE(ER90B_XLIST);
struct xtr_s ER90B_XLIST_P = { ER90B_XLIST };

#define ERET() {goto badret;}

/*
 * ER90 byte-stream transparent i/o open routine. 
 * Open a 'real' file descriptor, and set up the
 *	info block accordingly.
 */

_er90b_open(char *fname, int oflag, int mode, struct fdinfo *fio, 
	union spec_u *spec, struct ffsw *retstat, int cbits, int cblks,
	struct gl_o_inf *oinf)
{
int 		fd;		/* file descriptor */
ER90BYT 	*er90b_info;
 
	struct tpdctl ctl;
	struct stat x;
	int save_err;

/*
 *	allocate *ER90BYT
 */
	if((er90b_info = (ER90BYT *)calloc(sizeof(*er90b_info),1))==NULL){
		ERETURN(retstat, FENOMEMY, 0);
	}

        if ((_numargs() > 8 ) && oinf->alreadyopen)  {
                fd = oinf->fd;   
        }
	else {

/*
 *		Open the file.
 */
		oflag = (oflag | O_RAW) & (~O_CREAT);

		fd = open(fname, oflag, 0666);

		if (fd < 0){
			free(er90b_info);
			ERETURN(retstat, errno, 0);
		}
	}

#ifndef _CRAYMPP
	/* This ioctl does not work on the MPP */
  	if ( ioctl( fd, TPC_GETFL, &ctl) < 0 ){
		/* If ioctl fails with ENOTTY, then this */
		/* isn't a tape file. Make the errno more meaningful */
		if (errno == ENOTTY)
			errno = FECONNTP;
		ERET();
	}
/*
 *	If this is not a byte-stream ER90, error 
 */
	if ((ctl.tpc_type != DT_ER90) || (ctl.tpc_dbsize != 1)) {
		errno = FDC_ERR_NOER90;
		ERET();
	}
#endif
    
 
	er90b_info->tpos = TPOS_DONE;
	er90b_info->fd = fd;
 
/*
 *	Save device number and inode for tapeinfo requests
 */
	if (fstat(fd, &x) < 0){
		ERET();
	}
#ifdef _CRAYMPP
	if (_gsys_qtape(&x) == 0) {
		errno = FECONNTP;
		ERET();
	}
	if (getenv("NOER90WARN")== NULL) {
		write(2,"Reads and writes to tape devices using the er90 layer\n",54);
		write(2,"may function differently than reads and writes using\n",53);
		write(2,"the tape layer.\n",16);
		write(2,"The use of the er90 layer with tape devices may not be\n",55);
		write(2,"supported in future releases.\n",30);
		write(2,"To disable this message, set the environment variable\n",54);
		write(2,"NOER90WARN to a non-zero value\n",31);
	}
#endif
	er90b_info->tsireq.st_dev = x.st_dev;
	er90b_info->tsireq.st_ino = x.st_ino;
	er90b_info->tsireq.fd = fd;
#ifndef _CRAYMPP
	/* Can't use an ioctl on MPP yet. */
	if (sysconf(_SC_CRAY_RELEASE) >= 8300) {
		/* With operating system versions >= 8300, we can */
 		/* use an ioctl to get tape info. Er90s only run on Model E IOS, */
		/* so this is safe. */
 		er90b_info->tsireq.ioctlreq = 1;
 	}
#endif


	fio->lyr_info = (char *)er90b_info;
	fio->realfd = fd;
	fio->rwflag = POSITIN;
	SETSTAT(retstat, 0, 0);
	return(0);

badret:
	save_err = errno;
	free(er90b_info);
	(void) close(fd);
	errno = save_err;
	ERETURN(retstat, errno, 0);
}	
