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


#pragma ident "@(#) libu/ffio/evnt_get_tracker.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "evntio.h"

/*
 * _evnt_get_tracker
 *
 * Get pointer to tracker structure to use
 *
 * Input:
 *	evnt_info	- structure containing event information (timings, etc)
 * 	stat		- pointer to status return word
 *	mode		- mode of tracker structure (READA, etc, or FREE) 
 *	nbytes		- number of bytes to transfer
 *
 * Output:
 *	this_tracker	- pointer to tracker structure to use for async I/O
 *			  info
 *
 */
struct evnt_async_tracker *
_evnt_get_tracker(struct evnt_f *evnt_info, struct ffsw *stat, 
		  char mode, size_t nbytes)
{
	struct evnt_async_tracker *this_tracker;
	struct evnt_async_tracker *last_tracker;

	this_tracker = evnt_info->async_tracker;
	while (this_tracker != NULL) {
		if (this_tracker->stat == stat) {
			evnt_info->async_tracker_overwrites++;
			return (this_tracker);
		}
		last_tracker = this_tracker;
		this_tracker = this_tracker->next_tracker;
	}

	this_tracker = evnt_info->async_tracker;
	last_tracker = this_tracker;
	while (this_tracker != NULL) {
		if (this_tracker->mode == TRACKER_FREE)
			break;
		last_tracker = this_tracker;
		this_tracker = this_tracker->next_tracker;
	}

	if (this_tracker == NULL &&
	    (evnt_info->extend_trackers_failed == FALSE)) {
		if ((this_tracker = _evnt_add_trackers(NUM_TRACKERS)) == NULL) {
			evnt_info->extend_trackers_failed = TRUE;
		} else {
			last_tracker->next_tracker = this_tracker;
			this_tracker = this_tracker;
			evnt_info->num_async_trackers += NUM_TRACKERS;
		}
	}
	if (this_tracker) {
		this_tracker->mode = mode;
		this_tracker->stat = stat;
		this_tracker->requested = nbytes;
		if (mode == TRACKER_WRITEA)
			evnt_info->writea.current++;
		else if (mode == TRACKER_READA)
			evnt_info->reada.current++;
	} else {
		evnt_info->async_untracked_req++;
	}

	return (this_tracker);
}
