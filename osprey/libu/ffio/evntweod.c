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


#pragma ident "@(#) libu/ffio/evntweod.c	92.2	10/07/99 22:15:19"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "evntio.h"

/*
 * _evnt_weod
 *
 * Log an EOD write operation
 *
 * Input:
 *      fio             - ffio file descriptor
 *      stat            - pointer to status return word
 *
 * Output:
 *	ret		- return value from weodrtn
 *
 */
int
_evnt_weod(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo 	*llfio;
	struct evnt_f 	*evnt_info;
	int     	status;
	rtc_t     	start_rtc, finish_rtc;
	struct ffsw 	log_stat;
	int     	log_ubc = 0;
	int     	ret;

	evnt_info = (struct evnt_f *) fio->lyr_info;
	llfio = fio->fioptr;

	start_rtc = RTC();
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	finish_rtc = RTC();

#ifdef __mips
	if (!(evnt_info->odirect) ||
	 (evnt_info->cur_pos % evnt_info->diskalign != 0))
#else
	if (evnt_info->cur_pos & evnt_info->sector_mask)
#endif
		evnt_info->ill_formed_weod++;

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace) {
		int     record[4];

		EVNT_LOCK_ON;
		record[0] = _evnt_WEOD | evnt_info->fd;
		record[1] = start_rtc;
		record[2] = finish_rtc;
		record[3] = ret;
		status = EVNT_XR_WRITE(record, sizeof(int), 4);
		INC_GLOBAL_LOG_COUNT(weod);
		EVNT_LOCK_OFF;
	}
#endif
	_GL_evnt_trc_file[evnt_info->trc_file_number].cur_usage -=
			(evnt_info->cur_size - evnt_info->cur_pos);

	evnt_info->cur_size = evnt_info->cur_pos;
	EVNT_TRACE_SIZE;

	evnt_info->counts.weod++;
	evnt_info->counts.total++;

	return (ret);
}
