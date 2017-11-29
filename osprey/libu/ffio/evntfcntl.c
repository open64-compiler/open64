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


#pragma ident "@(#) libu/ffio/evntfcntl.c	92.2	10/07/99 22:15:19"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "evntio.h"

/*
 * _evnt_fcntl
 *
 * event layer fcntl requests
 *
 * Input:
 *      fio     - ffio file descriptor (dummy)
 *      cmd     - command code
 *      arg     - command specific parameter
 *      iostat  - pointer to status return word
 *
 * Output:
 *      Returns: -1 if error occurred
 *                0 if OK
 */
int
_evnt_fcntl(struct fdinfo *fio, int cmd, void *arg, struct ffsw *iostat)
{
	struct fdinfo *llfio;
	struct evnt_f *evnt_info;
	int     status;
	struct ffsw log_stat;
	int     log_ubc = 0;
	int     ret;
	struct ffsw *check_ffsw;
	struct evnt_async_tracker *this_tracker;
	struct rw_stats *rwinfo;
	int     already_done;
	rtc_t     start_rtc, finish_rtc;
	int     record[8];
	struct ffc_mtinfo_s *mti;

	evnt_info = (struct evnt_f *) fio->lyr_info;
	llfio = fio->fioptr;

#ifdef WILL_ADD_LATER
	if (cmd == FC_TRACE_CACHE_STATE) {
		struct cache_state_trace *packet;

		packet = (struct cache_state_trace *) arg;
		switch (packet->type) {
		case CCA_QUERY_TRACE:
			/* 
			 * return a 1 or 2 depending on if we are 
			 * using rtc or cpc
			 */
			return (evnt_info->optflags.rtc);
			break;

		case CCA_RELEASE_PAGE:
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
			if (evnt_info->optflags.trace) {
				EVNT_LOCK_ON;
				record[0] = _evnt_EIE_RELEASE_PAGE |
					    evnt_info->fd | packet->page_size;
				record[1] = packet->page_start_byte;
				record[2] = packet->start_rtc;
				record[3] = packet->finish_rtc;
				record[4] = RTC();
				status = EVNT_XR_WRITE(record, sizeof(int), 5);
				INC_GLOBAL_LOG_COUNT(cache_page_release);
				EVNT_LOCK_OFF;
			}
#endif
		}
		ret = 0;
		goto all_return;
	}
#endif

	if (evnt_info->optflags.diag) {
		if (cmd == FC_RECALL) {
			evnt_info->counts.fcntl_recall++;
			check_ffsw = (struct ffsw *) arg;
			if (check_ffsw->sw_flag != 0)
				already_done = TRUE;
			else
				already_done = FALSE;
		} else if (cmd == FC_IALLOC) {
			evnt_info->counts.fcntl_ialloc++;
		} else {
			evnt_info->counts.fcntl_other++;
		}
	}

	start_rtc = RTC();
	ret = XRCALL(llfio, fcntlrtn) llfio, cmd, arg, iostat);

	finish_rtc = RTC();

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace) {
		EVNT_LOCK_ON;
		if (cmd == FC_IALLOC) {
			struct ff_ialloc_struct *ialloc_data;

			ialloc_data = (struct ff_ialloc_struct *) arg;

			record[0] = _evnt_FCNTL_IALLOC | evnt_info->fd;
			record[1] = cmd;
			record[2] = ialloc_data->ia_before;
			record[3] = ialloc_data->ia_nb;
			record[4] = ialloc_data->ia_avl;
			record[5] = start_rtc;
			record[6] = finish_rtc;
			record[7] = ret;
			status = EVNT_XR_WRITE(record, sizeof(int), 8);
		} else {
			record[0] = _evnt_FCNTL | evnt_info->fd;
			record[1] = cmd;
			record[2] = (int)arg;
			record[3] = start_rtc;
			record[4] = finish_rtc;
			record[5] = ret;
			status = EVNT_XR_WRITE(record, sizeof(int), 6);

			record[0] = start_rtc;
			record[1] = finish_rtc;
		}
		if (cmd == FC_RECALL)
			INC_GLOBAL_LOG_COUNT(fcntl_recall)
		else if (cmd == FC_IALLOC)
			INC_GLOBAL_LOG_COUNT(fcntl_ialloc)
		else
			INC_GLOBAL_LOG_COUNT(fcntl_other)
		EVNT_LOCK_OFF;
	}
#endif
	evnt_info->counts.total++;

	switch (cmd) {
	case FC_DUMPSTATS:		/* Dump the diags if they are being
		 			 * gathered */
		if (_GL_evnt_trc_file[evnt_info->trc_file_number].locked) {
			_GL_evnt_trc_file[evnt_info->trc_file_number].queued_flush = TRUE;
		} else {
			_evnt_close_diags(fio, evnt_info, FALSE);
		}
		if (_GL_evnt_logptr)
			fflush(_GL_evnt_logptr);
		break;

#ifdef WILL_ADD_LATER
	case FC_SDS_INCR:		/* trapping sds size changes */
		if (ret >= 0)
			evnt_info->cur_size = ret;
		EVNT_TRACE_SIZE;
		break;
#endif
	}

	if (evnt_info->optflags.diag || evnt_info->optflags.trace) {
	    if (cmd == FC_RECALL) {
		int     current_asyncs;

		check_ffsw = (struct ffsw *) arg;
		current_asyncs = evnt_info->reada.current +
				 evnt_info->writea.current +
				 evnt_info->listio_reada.current +
				 evnt_info->listio_writea.current;
		if (current_asyncs > evnt_info->async_high_water)
		    evnt_info->async_high_water = current_asyncs;

		this_tracker = evnt_info->async_tracker;
		while (this_tracker) {
		    if (this_tracker->mode != TRACKER_FREE &&
		        this_tracker->stat == check_ffsw) {
			if (check_ffsw->sw_flag == 0 || check_ffsw->sw_stat == 0) {
			    evnt_info->not_my_recall++;
			    break;
			}

			if (this_tracker->mode == TRACKER_READA)
			    rwinfo = &evnt_info->reada;
			else if (this_tracker->mode == TRACKER_WRITEA)
			    rwinfo = &evnt_info->writea;
			else if (this_tracker->mode == TRACKER_LISTIO_READA)
			    rwinfo = &evnt_info->listio_reada;
			else if (this_tracker->mode == TRACKER_LISTIO_WRITEA)
			    rwinfo = &evnt_info->listio_writea;

			rwinfo->high_water = 
			         (rwinfo->current > rwinfo->high_water) ?
			 	  rwinfo->current : rwinfo->high_water;
			rwinfo->current--;
			rwinfo->delivered += check_ffsw->sw_count;

			if (already_done)
			    rwinfo->all_hidden++;

			this_tracker->mode = TRACKER_FREE;
			this_tracker->stat = NULL;
			this_tracker->requested = -1;
			break;
		    }
		    this_tracker = this_tracker->next_tracker;
		}

		if (this_tracker == NULL) {
		    evnt_info->async_unknown_req++;
		} else {
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		    if (evnt_info->optflags.trace) {
			EVNT_LOCK_ON;
			status = EVNT_XR_SEEK(this_tracker->logpos, SEEK_SET);
			record[2] = check_ffsw->sw_count;
			status = EVNT_XR_WRITE(record, sizeof(int), 3);
			status = EVNT_XR_SEEK(0, SEEK_END);
			EVNT_LOCK_OFF;
		    }
#endif
		}
	    }

	    if (cmd == FC_RECALL) {
		evnt_info->fcntl_recall_time += (finish_rtc - start_rtc);
	    } else if (cmd == FC_IALLOC) {
		struct ff_ialloc_struct *ialloc_data;

		ialloc_data = (struct ff_ialloc_struct *) arg;
		evnt_info->fcntl_ialloc_time += (finish_rtc - start_rtc);
		evnt_info->fcntl_ialloc_req += ialloc_data->ia_nb;
		if (ialloc_data->ia_ret >= 0)
		    evnt_info->fcntl_ialloc_del += ialloc_data->ia_avl;
	    } else {
	    	evnt_info->fcntl_other_time += (finish_rtc - start_rtc);
	    }
	}

all_return:
	return (ret);
}
