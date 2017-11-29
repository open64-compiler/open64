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


static char USMID[] = "@(#) libu/ffio/c1/tapetptsi.c	92.0	10/08/98 14:57:41";


#include  <sys/types.h>
#include  <sys/tpdctl.h>
#include  <sys/file.h>
#include  <sys/iosw.h>
#include  <sys/stat.h>
#include  <sys/jtab.h>
#include  <sys/param.h>
#include  <fcntl.h>
#include  <tapereq.h>
#include  <stdio.h>
#include  <malloc.h>
#include  <errno.h>
#include  <liberrno.h>
#include  <ffio.h>
#include  "tapeio.h"
#include  "../sysio.h"


/* 
 * Issue request to get tape info 
 */
_tape_tptsi(struct tptsireq *req, struct tsdata *tsi, int *vsn)
{
	struct jtab	j;
	struct trinfo	request;
	struct rephdr	header;
	int request_fildes;
	int reply_fildes;

	char	request_fname[PATH_MAX];
	int	jid;
	char *tmpbuf;
	char *repptr;
	struct tpd_info_req tpdinfo;
	int ret, remsize;

	if (req->ioctlreq) {
		/* We can get this info with an ioctl */
		tpdinfo.tprh_repsize = 0;
		tpdinfo.tprh_repaddr = NULL;
		/* Request the data */
		LOOP_SYSCALL(ret,ioctl(req->fd, TPC_INFO, &tpdinfo));
		if (ret < 0) {
			return(-1);
		}
		if (tpdinfo.tprh_status != 0) {
			errno = tpdinfo.tprh_status;
			return(-1);
		}
		/* allocate reply buffer */
		if ((tmpbuf = malloc(tpdinfo.tprh_repsize)) == NULL){
			errno = FENOMEMY;
			goto errret;
		}
		/* Get reply buffer */
		LOOP_SYSCALL(ret,ioctl(req->fd, TPC_GET_INFO, tmpbuf));
		if (ret < 0) {
			free(tmpbuf);
			return(-1);
		}

		memcpy(tsi,tmpbuf,sizeof(struct tsdata));
		repptr = tmpbuf + sizeof(struct tsdata);

		/* Copy the vsn list */
		if (vsn != NULL) {
			remsize = tpdinfo.tprh_repsize - sizeof(struct tsdata);	
			if (remsize > MAXVSN * sizeof(int)) {
				remsize = MAXVSN * sizeof(int);	
			}
		        memcpy(vsn,repptr,remsize);
		}
	        free(tmpbuf);

        	return(0);
		
	}
/*
 *	Build request header.
 */
	jid = getjtab(&j);
	request.rh.size = sizeof(struct trinfo);
	request.rh.jid = jid;
	request.rh.code = TR_INFO;
/*
 *	Set reply pipe name.
 */
	if (req->reply_fname == NULL){
		if((req->reply_fname = tempnam(NULL,NULL)) == NULL) {
			return(-1);
		}
/*
 *	Set request pipe name. Tape daemon expects the request pipe to
 *	be named as follows:
 */
		sprintf(request_fname,"%s/TAPE_REQ_%d",getenv(USER_DIR),jid );
/*
 *		Make reply pipe 
 */
		if ( mknod(req->reply_fname, 0010700, 0) )  {
			return(-1);
		}

		if ((req->request_fname = malloc(strlen(request_fname)+1)) == NULL){
			errno = FENOMEMY;
			return(-1);
		}
		(void)strcpy(req->request_fname,request_fname);
	}

	strcpy(request.rh.rpn,req->reply_fname);

	LOOP_SYSCALL(reply_fildes, open(req->reply_fname, O_RDWR));
	if (reply_fildes < 0) {
		return(-1);
	}

	LOOP_SYSCALL(request_fildes, open(req->request_fname, O_WRONLY));
	if (request_fildes < 0) {
		(void) close(reply_fildes);
		return(-1);
	}

	request.st_dev = req->st_dev;
	request.st_ino = req->st_ino;

	LOOP_SYSCALL(ret, write(request_fildes, &request,sizeof(struct trinfo)));
	if (ret < 0) {
		goto errret;
	}

	close(request_fildes);
/*
 *	Get the reply.
 */

	LOOP_SYSCALL(ret, read(reply_fildes, &header, sizeof(struct rephdr)));
	if (ret < 0){
		goto errret;
	}

	header.size = header.size - sizeof(struct rephdr);

	if (header.rc != 0) {
		errno = header.rc;
		goto errret;
	}
	
	if ((tmpbuf = malloc(header.size)) == NULL){
		errno = FENOMEMY;
		goto errret;
	}
	LOOP_SYSCALL(ret, read(reply_fildes, tmpbuf, header.size));
	if (ret < 0){
		goto errret;
	}

	/* Copy the tape info data into structure tsi */
	repptr = tmpbuf+2*sizeof(int); 	/* 1st 2 words are seq and current */
	memcpy(tsi,repptr,sizeof(struct tsdata));
	repptr += sizeof(struct tsdata);

	/* Copy the vsn list */
	if (vsn != NULL) {
		memcpy(vsn,repptr,
			header.size - 2*sizeof(int) - sizeof(struct tsdata));
	}
	free(tmpbuf);

	(void) close(reply_fildes);
	return(0);

errret:
	(void) close(reply_fildes);
	(void) close(request_fildes);
	return(-1);
}

