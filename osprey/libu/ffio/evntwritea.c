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


#pragma ident "@(#) libu/ffio/evntwritea.c	92.2	10/07/99 22:15:19"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "evntio.h"

/*
 * _evnt_writea
 *
 * Log a writea request
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
 *      The number of bytes to be transferred is returned upon successful
 *      completion.
 *
 *      If an error occurs, -1 is returned.
 *
 *      The stat structure is not set until asynchronous I/O completion,
 *      unless an immediately detected error is encountered.
 *
 */
ssize_t
_evnt_writea(struct fdinfo *fio, bitptr bufptr, size_t nbytes, 
	     struct ffsw *stat, int fulp, int *ubcp)
{
	struct fdinfo 	*llfio;
	struct evnt_f 	*evnt_info;
	int     	status;
	rtc_t     	start_rtc, finish_rtc;
	struct ffsw 	log_stat;
	int     	log_ubc = 0;
	size_t     	ret;
	ssize_t     	orig_ret;
	int     	i;
	struct evnt_async_tracker *this_tracker;

	evnt_info = (struct evnt_f *) fio->lyr_info;
	llfio = fio->fioptr;

	EVNT_UPDATE(writea);
	if (evnt_info->optflags.diag || evnt_info->optflags.trace) {
		this_tracker = _evnt_get_tracker(evnt_info, stat, 
						TRACKER_WRITEA, nbytes);
	}

	start_rtc = RTC();
	orig_ret = XRCALL(llfio, writeartn) llfio, bufptr, nbytes, stat, 
			  fulp, ubcp);
	finish_rtc = RTC();

	if (stat->sw_stat != 0 && stat->sw_flag != 0)
		ret = stat->sw_count;
	else
		ret = 0;

	evnt_info->cur_pos += nbytes;
	EVNT_CHECK_SIZE;

	if (evnt_info->optflags.diag)
		evnt_info->writea.time += (finish_rtc - start_rtc);

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace) {
		int     record[8];

		EVNT_LOCK_ON;
		record[0] = (_evnt_WRITEA | evnt_info->fd) | nbytes;
		record[1] = (int) stat;
		record[2] = start_rtc;
		record[3] = finish_rtc;
		record[4] = ret;
		record[5] = record[6] = record[7] = 0;
		status = EVNT_XR_WRITE(record, sizeof(int), 8);
		if (this_tracker)
			this_tracker->logpos = EVNT_XR_TELL() - sizeof(int)*3;
		INC_GLOBAL_LOG_COUNT(writea);
		EVNT_LOCK_OFF;
	}
#endif
	evnt_info->counts.total++;
	evnt_info->counts.writea++;

	if (this_tracker && (stat->sw_stat != 0 && stat->sw_flag != 0)) {
		evnt_info->writea.sync++;
		evnt_info->writea.delivered += stat->sw_count;
		evnt_info->writea.current--;
		this_tracker->mode = TRACKER_FREE;
		this_tracker->stat = NULL;
		this_tracker->logpos = 0;
	}
	return (orig_ret);
}
