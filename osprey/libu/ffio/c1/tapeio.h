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


/* USMID @(#) libu/ffio/c1/tapeio.h	92.0	10/08/98 14:57:41 */


#include <sys/types.h>
#include <tapereq.h>

struct tptsireq {
        char            *reply_fname;   /*  reply pipe for daemon requests */
        char            *request_fname; /*  request pipe for daemon reqsts */
        dev_t           st_dev;
        ino_t           st_ino;
	int		fd;		/* file descriptor of tape */
	unsigned	ioctlreq:1;	/* ioctl can be used */
};

/*
 *      Positioning flags and definition
 */
 
#define TPOS_DONE       0
#define TPOS_REWIND     1

/* Prototypes */
int _tape_closev(int fd);
void _tape_gtpos(struct tsdata *tsdata, int vsn[], int lib, long *pa, long palen);
int _tape_rewd(int fd);
int _tape_sync(int fd);
int _tape_tptsi(struct tptsireq *req, struct tsdata *tsi, int *vsn);
int _tape_tpwait(int fd, int *tpos);


