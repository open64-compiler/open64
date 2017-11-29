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


#pragma ident "@(#) libu/ffio/evntwrite.c	92.2	10/07/99 22:15:19"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "evntio.h"

/*
 * _evnt_write
 *
 * Log a write request
 *
 * Input:
 *      fio             - ffio file descriptor
 *      bufptr          - bit pointer to where data is to go
 *      nbytes          - number of bytes to be written
 *      stat            - pointer to status return word
 *      fulp            - full or partial write mode flag
 *      ubcp            - pointer to unused bit count.  On return,
 *                        *ubcp is updated to contain the unused bit
 *                        count in the data returned.
 *
 * Output:
 * 	The number of bytes transferred is returned upon successful completion.
 *	if an error occurs, -1 is returned.
 *
 *	The stat->sw_stat field is set to FFCNT upon normal return.
 *
 */
ssize_t
_evnt_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes, 
	    struct ffsw *stat, int fulp, int *ubcp)
{
	struct fdinfo 	*llfio;
	struct evnt_f 	*evnt_info;
	int     	status;
	rtc_t     	start_rtc, finish_rtc;
	ssize_t     	ret;
	struct ffsw 	log_stat;
	int     	log_ubc = 0;

	evnt_info = (struct evnt_f *) fio->lyr_info;
	llfio = fio->fioptr;

	EVNT_UPDATE(write);

	start_rtc = RTC();
	ret = XRCALL(llfio, writertn) llfio, bufptr, nbytes, stat, fulp, ubcp);
	finish_rtc = RTC();

	evnt_info->cur_pos += nbytes;
	EVNT_CHECK_SIZE;

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace) {
		int     record[4];

		EVNT_LOCK_ON;
		record[0] = (_evnt_WRITE | evnt_info->fd) | nbytes;
		record[1] = start_rtc;
		record[2] = finish_rtc;
		record[3] = ret;
		status = EVNT_XR_WRITE(record, sizeof(int), 4);
		INC_GLOBAL_LOG_COUNT(write);
		EVNT_LOCK_OFF;
	}
#endif
	evnt_info->counts.total++;
	evnt_info->counts.write++;
	evnt_info->write.delivered += ret;
	evnt_info->write.time += (finish_rtc - start_rtc);

	return (ret);
}
