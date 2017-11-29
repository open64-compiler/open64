/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/evnt_close_diags.c	92.4	10/29/99 21:40:31"


#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <ffio.h>
#include <fcntl.h>
#include "evntio.h"

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define NUM_EVNT_OFLAGS 13   /* # of file status flags supported by event lyr */
#elif defined(_CRAYT3E)
#define NUM_EVNT_OFLAGS 20   /* # of file status flags supported by event lyr */
#else
#define NUM_EVNT_OFLAGS 19   /* # of file status flags supported by event lyr */
#endif
DECL_LOCK(_evntfillock)
#ifdef _CRAYT3E
DECL_MPP_LOCK(_evntfilpelock)
#endif
struct _evnt_oflag_struct {
	int     bit;
	char   *string;
};

struct _evnt_oflag_struct _EVNT_OFLAGS[NUM_EVNT_OFLAGS] =
{
	O_RAW, 		"RAW",
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	O_LDRAW, 	"LDRAW",
	O_SSD, 		"SSD",
#endif
	O_RDWR, 	"RDWR",
	O_RDONLY, 	"RDONLY",
	O_WRONLY, 	"WRONLY",
	O_APPEND, 	"APPEND",
	O_SYNC, 	"SYNC",
	O_CREAT, 	"CREAT",
	O_TRUNC, 	"TRUNC",
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	O_BIG, 		"BIG",
	O_PLACE, 	"PLACE",
	O_RESTART, 	"RESTART",
	O_ASYNC, 	"ASYNC",
#endif
	O_NDELAY, 	"NDELAY",
	O_NONBLOCK, 	"NONBLOCK",
	O_NOCTTY, 	"NOCTTY",
	O_EXCL, 	"EXCL",
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	O_WELLFORMED, 	"WELLFORMED",
#endif
#ifdef _CRAYT3E
	O_PARALLEL, 	"PARALLEL",
#endif
#ifdef __mips
	O_DIRECT,	"DIRECT",
#endif
};



/*
 * _evnt_close_diags
 *
 * Close a file
 *
 * Input:
 *	fio		- file descriptor
 *	evnt_info	- structure containing event information (timings, etc)
 *	final		- flag describing final or intermediate diagnostics
 *			  output
 *
 * Output:
 *
 */
void
_evnt_close_diags(struct fdinfo *fio, struct evnt_f *evnt_info, int final)
{
	static double rtc_factor = -1.0; /* conversion factor to get wallclock */
	double   wall;			/* wallclock time for I/O */
	char    units[16];		/* units to display statistics */
	int     i;
	struct rw_stats *rwinfo;
	char   	*parent;
	char   	*child;
	int     status;
	int     k, num_oflag_bits;
	int     unused_count;
	evint64     avg;
	struct ffsw log_stat;
	int     log_ubc = 0;
	struct evnt_async_tracker *this_tracker;
	char   *name[4] = {"write", "writea", "read", "reada"};
	struct evnt_count counts;

	_GL_evnt_trc_file[evnt_info->trc_file_number].queued_flush = FALSE;

	if (evnt_info->parent_name == NULL)
		_evnt_get_parent_child(fio,
				       &evnt_info->parent_name,
				       &evnt_info->child_name);

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace) {
	    rtc_t     close_rtc;
	    struct evnt_open_info *open_info;

	    _evnt_trace_flush(fio, evnt_info);

	    open_info = &evnt_info->open_info;

	    open_info->max_size = evnt_info->max_size;
	    open_info->close_rtc = RTC();
	    open_info->event_count =
	        open_info->logged.write + open_info->logged.writea +
		open_info->logged.read + open_info->logged.reada +
		open_info->logged.readc + open_info->logged.writec +
		open_info->logged.open + open_info->logged.close +
		open_info->logged.weof + open_info->logged.weod +
		open_info->logged.fcntl_recall + open_info->logged.fcntl_other +
		open_info->logged.fcntl_recall + open_info->logged.fcntl_ialloc+
		open_info->logged.seek + open_info->logged.listio +
		open_info->logged.size_changes + open_info->logged.flush +
		open_info->logged.pos;

	    status = EVNT_XR_SEEK(evnt_info->open_pos, SEEK_SET);
	    status = EVNT_XR_WRITE(open_info, 1, sizeof(struct evnt_open_info));

	    status = EVNT_XR_SEEK(0, SEEK_END);
	    if (evnt_info->file_ptr)
		EVNT_XR_FLUSH();
	}
#endif
	parent = evnt_info->parent_name;
	child = evnt_info->child_name;

	if (rtc_factor == -1.0)
		rtc_factor = RTC_FACTOR;

	if ((evnt_info->optflags.diag) || (evnt_info->optflags.summary) 
		|| (evnt_info->optflags.full_diag)) {
	    /* More than 1 file can be using the same log file */
	    MEM_LOCK(&_evntfillock);
#ifdef _CRAYT3E
	    /* If more than 1 PE is potentially using the same log file, */
	    /* lock it so that all data from a pe will appear together */
	    if (!evnt_info->optflags.perpelogfile)
	    	MPP_LOCK(&_evntfilpelock);
#endif
	}
	if (evnt_info->optflags.diag) {	/* if any level of diagnostics... */
	    rtc_t   total_rtc;	
	    evint64   delivered;
	    double   rate;

	    if (evnt_info->optflags.k)
		strcpy(units, "k");
	    else if (evnt_info->optflags.m)
		strcpy(units, "m");
	    else if (evnt_info->optflags.g)
		strcpy(units, "g");
	    else
		units[0] = 0;

	    if (evnt_info->optflags.blocks)
		strcat(units, "blocks");
	    else if (evnt_info->optflags.words)
		strcat(units, "words");
	    else
		strcat(units, "bytes");

	    total_rtc = 0;
	    delivered = 0;

	    total_rtc += evnt_info->open_time;
	    total_rtc += evnt_info->write.time;
	    total_rtc += evnt_info->read.time;
	    total_rtc += evnt_info->writea.time;
	    total_rtc += evnt_info->reada.time;
	    total_rtc += evnt_info->listio_time;
	    total_rtc += evnt_info->fcntl_recall_time;
	    total_rtc += evnt_info->fcntl_ialloc_time;
	    total_rtc += evnt_info->fcntl_other_time;
	    total_rtc += evnt_info->close_time;
	    total_rtc += evnt_info->flush_time;

	    wall = (double) total_rtc * rtc_factor;

	    delivered += evnt_info->write.delivered;
	    delivered += evnt_info->read.delivered;
	    delivered += evnt_info->writea.delivered;
	    delivered += evnt_info->reada.delivered;
	    delivered += evnt_info->listio_write.delivered;
	    delivered += evnt_info->listio_read.delivered;
	    delivered += evnt_info->listio_writea.delivered;
	    delivered += evnt_info->listio_reada.delivered;

	    delivered = _evnt_units(evnt_info, delivered);

	    if (wall > 0)
	    	rate = (double) delivered / wall;
	    else
		rate = 0.;

	    if (final) {
#ifdef _CRAYT3E
	    	fprintf(_GL_evnt_logptr, 
		    "final I/O activity monitored for:  %.256s on pe %d\n", 
			evnt_info->name,_my_pe());
#else
	    	fprintf(_GL_evnt_logptr, 
		    "final I/O activity monitored for:  %.256s\n", evnt_info->name);
#endif
	    } else {
#ifdef _CRAYT3E
	    	fprintf(_GL_evnt_logptr, 
		    "intermediate I/O activity monitored for:  %.256s on pe %d\n",
		    evnt_info->name,_my_pe());
#else
	    	fprintf(_GL_evnt_logptr, 
		    "intermediate I/O activity monitored for:  %.256s\n",
		    evnt_info->name);
#endif
	    }
	    fprintf(_GL_evnt_logptr, "\n");
	    fprintf(_GL_evnt_logptr,
		    "    layer information:\n");
	    fprintf(_GL_evnt_logptr,
		    "    -----------------\n");
	    fprintf(_GL_evnt_logptr, 
		    "    parent <--> child layer     %s <--> %s\n",
	  	    evnt_info->parent_name, evnt_info->child_name);	
	    fprintf(_GL_evnt_logptr, 
		    "    data transfered             %lld %s\n",
		    delivered, units);
	    fprintf(_GL_evnt_logptr, 
		    "    time for transfer           %fs\n", wall);
	    fprintf(_GL_evnt_logptr, 
		    "    transfer rate               %f %s/s\n", 
		    rate, units);
	    fprintf(_GL_evnt_logptr, "\n");
	}

	if (evnt_info->optflags.full_diag || evnt_info->optflags.summary) {
	    counts = evnt_info->counts;
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	    if (evnt_info->fake_sds) {
		_evnt_sds_diags(evnt_info, counts, units, 
			   	rtc_factor, final);
		goto diag_done;
	    }
#endif
	    fprintf(_GL_evnt_logptr,
		    "    file information:\n");
	    fprintf(_GL_evnt_logptr,
		    "    -----------------\n");
	    fprintf(_GL_evnt_logptr, 
		    "    open flags              0x%16.16x", 
		    evnt_info->oflags);
	    num_oflag_bits = 0;

	    for (k = 0; k < NUM_EVNT_OFLAGS; k++) {
	        if (evnt_info->oflags & _EVNT_OFLAGS[k].bit) {
		    if (num_oflag_bits == 0)
			fprintf(_GL_evnt_logptr, " = " );
		    else
			fprintf(_GL_evnt_logptr, "+" );

		    fprintf(_GL_evnt_logptr, "%s", _EVNT_OFLAGS[k].string);
		    num_oflag_bits++;
		}
	    }

	    fprintf(_GL_evnt_logptr, "\n");

	    fprintf(_GL_evnt_logptr, 
		    "    sector size             %lld (bytes)\n", 
		    (evint64) evnt_info->sector_mask + 1LL);
#ifdef _CRAY
	    fprintf(_GL_evnt_logptr, 
		    "    cblks                   %d\n",  
		    evnt_info->cblks);
    	    fprintf(_GL_evnt_logptr, 
		    "    cbits                   0x%16.16x\n",
		    evnt_info->cbits);

#endif
	    fprintf(_GL_evnt_logptr,
		    "    current file size       %lld %s\n",
		    _evnt_units(evnt_info, evnt_info->cur_size), units);
	    fprintf(_GL_evnt_logptr,
		    "    high water file size    %lld %s\n",
			 _evnt_units(evnt_info, evnt_info->max_size), units);

	    fprintf(_GL_evnt_logptr, "\n");
	    fprintf(_GL_evnt_logptr,
		    "    I/O activity:\n");
	    fprintf(_GL_evnt_logptr,
		    "    -------------\n");
	    fprintf(_GL_evnt_logptr,
		    "    function       times     ill       wait   %8s    %8s        min        max        avg        all\n", units, units);
	    fprintf(_GL_evnt_logptr,
		    "                  called     formed    time  requested   delivered    request    request    request     hidden\n");

	    wall = evnt_info->open_time * rtc_factor;

	    fprintf(_GL_evnt_logptr, "       open   %10lld           %8.2f\n",
		     counts.open, wall);

	    if (counts.seek || evnt_info->do_zeros)
		fprintf(_GL_evnt_logptr, "       seek   %10lld\n", 
			 counts.seek);

	    for (i = 0; i < 4; i++) {
		evint64     count;

		switch(i) {
			case 0:
				count = counts.write;
				rwinfo = &(evnt_info->write);
				break;
			case 1:
				count = counts.writea;
				rwinfo = &(evnt_info->writea);
				break;
			case 2:
				count = counts.read;
				rwinfo = &(evnt_info->read);
				break;
			case 3:
				count = counts.reada;
				rwinfo = &(evnt_info->reada);
				break;
		}
			
		if (count || evnt_info->do_zeros) {
		    if (count == 0) {
			rwinfo->min = 0;
			avg = 0;
		    } else
			avg = rwinfo->requested / count;

		    wall = (double) rwinfo->time * rtc_factor;
		    fprintf(_GL_evnt_logptr,
			 "       %-6s %10lld%10lld %8.2f %10lld  %10lld %10lld %10lld %10lld",
			     name[i], count, rwinfo->ill_formed, wall,
			     _evnt_units(evnt_info, rwinfo->requested),
			     _evnt_units(evnt_info, rwinfo->delivered),
			     _evnt_units(evnt_info, rwinfo->min),
			     _evnt_units(evnt_info, rwinfo->max),
			     _evnt_units(evnt_info, avg));
		    if (i == 1 || i == 3)
			fprintf(_GL_evnt_logptr, "%10lld\n", 
				 rwinfo->all_hidden);
		    else
			fprintf(_GL_evnt_logptr, "\n");
		}
	    }

	    wall = (double) (evnt_info->listio_time) * rtc_factor;

	    if (counts.listio || evnt_info->do_zeros) {
	        fprintf(_GL_evnt_logptr,
		         "       listio   %8lld           %8.2f\n",
			 counts.listio, wall);
		fprintf(_GL_evnt_logptr, "          seek  %8lld \n",
			 counts.listio_seek);

		for (i = 0; i < 4; i++) {
		    evint64     count;

		    switch(i) {
			case 0:
				count = counts.listio_write;
				rwinfo = &(evnt_info->listio_write);
				break;
			case 1:
				count = counts.listio_writea;
				rwinfo = &(evnt_info->listio_writea);
				break;
			case 2:
				count = counts.listio_read;
				rwinfo = &(evnt_info->listio_read);
				break;
			case 3:
				count = counts.listio_reada;
				rwinfo = &(evnt_info->listio_reada);
				break;
		    }

		    if (count || evnt_info->do_zeros) {
		    	if (count == 0) {
			    rwinfo->min = 0;
			    avg = 0;
		        } else
			    avg = rwinfo->requested / count;
		        fprintf(_GL_evnt_logptr,
		                 "          %-6s%8lld%10lld          %10lld  %10lld %10lld %10lld %10lld",
			         name[i], count, rwinfo->ill_formed,
			         _evnt_units(evnt_info, rwinfo->requested),
			         _evnt_units(evnt_info, rwinfo->delivered),
			         _evnt_units(evnt_info, rwinfo->min),
			         _evnt_units(evnt_info, rwinfo->max),
			         _evnt_units (evnt_info, avg));

		        if (i == 1 || i == 3)
			    fprintf(_GL_evnt_logptr, "%10lld\n", 
			         rwinfo->all_hidden);
		        else
			    fprintf(_GL_evnt_logptr, "\n");
		    }
		}
	    }

	    if (counts.fcntl_recall || counts.fcntl_other ||
	        counts.fcntl_ialloc || evnt_info->do_zeros) {
		fprintf(_GL_evnt_logptr, "       fcntl\n");

		if (counts.fcntl_recall || evnt_info->do_zeros) {
		    wall = (double) evnt_info->fcntl_recall_time * rtc_factor;
		    fprintf(_GL_evnt_logptr,
		             "          recall%8lld           %8.2f\n",
			     counts.fcntl_recall, wall);
		}

		if (counts.fcntl_ialloc || evnt_info->do_zeros) {
		    wall = (double) evnt_info->fcntl_ialloc_time * rtc_factor;
		    fprintf(_GL_evnt_logptr,
			     "          ialloc%8lld           %8.2f %10lld  %10lld\n",
		             counts.fcntl_ialloc, wall,
			     _evnt_units(evnt_info, evnt_info->fcntl_ialloc_req),
			     _evnt_units(evnt_info, evnt_info->fcntl_ialloc_del));

		}

		if (counts.fcntl_other || evnt_info->do_zeros) {
		    wall = (double) evnt_info->fcntl_other_time * rtc_factor;
		    fprintf(_GL_evnt_logptr,
			     "          other %8lld           %8.2f\n",
			    counts.fcntl_other, wall);
		}
	    }

	    if (counts.readc || evnt_info->do_zeros) {
		wall = (double) evnt_info->readc_time * rtc_factor;
		fprintf(_GL_evnt_logptr,
			 "       readc  %10lld           %8.2f\n",
			 counts.readc, wall);
	    }

	    if (counts.writec || evnt_info->do_zeros) {
		wall = (double) evnt_info->writec_time * rtc_factor;
		fprintf(_GL_evnt_logptr,
			 "       writec %10lld           %8.2f\n",
			 counts.writec, wall);
	    }

	    if (counts.flush || evnt_info->do_zeros) {
		wall = (double) evnt_info->flush_time * rtc_factor;
		fprintf(_GL_evnt_logptr, "       flush  %10lld           %8.2f\n",
			 counts.flush, wall);
	    }

	    if (counts.weof || evnt_info->do_zeros)
		fprintf(_GL_evnt_logptr, "       weof   %10lld\n", counts.weof);

	    if (counts.weod || evnt_info->do_zeros)
		fprintf(_GL_evnt_logptr, "       weod   %10lld%10lld\n",
			 counts.weod, evnt_info->ill_formed_weod);

	    if (counts.bksp || evnt_info->do_zeros)
		fprintf(_GL_evnt_logptr, "       bksp   %10lld\n", counts.bksp);

	    if (counts.close || evnt_info->do_zeros) {
		wall = (double) evnt_info->close_time * rtc_factor;
		fprintf(_GL_evnt_logptr, "       close  %10lld           %8.2f\n",
			 counts.close, wall);
 	    }

	    if (counts.size_changes || evnt_info->do_zeros) {
		fprintf(_GL_evnt_logptr, "       extends %9lld\n", 
			 counts.size_changes);
	    }

	    unused_count = 0;
	    this_tracker = evnt_info->async_tracker;

	    while (this_tracker) {
		if (this_tracker->mode != TRACKER_FREE)
			unused_count++;
		this_tracker = this_tracker->next_tracker;
	    }

	    if (unused_count || evnt_info->do_zeros)
		fprintf(_GL_evnt_logptr,
			 "       pending async requests    %10d\n", 
			 unused_count);
	}

diag_done:
	if ((evnt_info->optflags.diag) || (evnt_info->optflags.summary) 
		|| (evnt_info->optflags.full_diag)) {
		MEM_UNLOCK(&_evntfillock);
#ifdef _CRAYT3E
		if (!evnt_info->optflags.perpelogfile)
	    		MPP_UNLOCK(&_evntfilpelock);
#endif
	}
	return;

}



/*
 * _evnt_units
 *
 * Set units to be used for statistical output 
 *	(type of units can be requested by user)
 *
 * Input:
 *	evnt_info	- structure containing event information (timings, etc)
 *	value		- value to be converted to requested units
 *
 * Output:
 *	new_value	- value converted to new units
 *
 * Notes:
 *    	will need to alter for sparc support
 */
evint64
_evnt_units(struct evnt_f *evnt_info, evint64 value)
{
	evint64     new_value;

	if (value == 0)
		return (0);

	if (evnt_info->optflags.words)
		new_value = (value + 7) / 8;
	else if (evnt_info->optflags.blocks)
		new_value = (value + 4095) / 4096;
	else
		new_value = value;

	if (evnt_info->optflags.k)
		new_value = (new_value + (1024 - 1)) / 1024;
	else if (evnt_info->optflags.m)
		new_value = (new_value + (1024 * 1024 - 1)) / (1024 * 1024);
	else if (evnt_info->optflags.g)
		new_value = 
		  (new_value + (1024 * 1024 * 1024 - 1)) / (1024 * 1024 * 1024);

	return (new_value);
}




#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
/*
 * _evnt_sds_diags
 *
 *
 * Input:
 *	evnt_info	- structure containing event information (timings, etc)
 *	counts		- structure that holds event counts
 *	units		- units to display statistics in
 *	rtc_factor	- conversion factor to get wallclock time
 *	final		- flag to tell us if it's final or intermediate 
 *			  diagnostics output
 *
 * Output:
 *
 */
void
_evnt_sds_diags(struct evnt_f *evnt_info, struct evnt_count counts,
		char *units, double rtc_factor, int final)
{
	int     i;
	struct rw_stats *rwinfo;
	char   *name[3] = {"sswrite", "ssbreak", "ssread"};
	double   wall;
	evint64   count;
	evint64   avg;

	if (final)
		fprintf(_GL_evnt_logptr, "total sds usage for file: %.256s\n", 
			evnt_info->name);
	else
		fprintf(_GL_evnt_logptr,"intermediate sds usage for file: %.256s\n",
			evnt_info->name);

	fprintf(_GL_evnt_logptr,
		 "    function       times     wait   %8s    %8s        min        max        avg\n", units, units);

	fprintf(_GL_evnt_logptr,
		 "                  called     time  requested   delivered    request    request    request\n");

	fprintf(_GL_evnt_logptr, "       %-7s%10d\n",
		name[1], counts.size_changes);

	for (i = 0; i < 3; i += 2) {
		if (i == 0) {
			count = counts.listio_write;
			rwinfo = &evnt_info->listio_write;
		}
		else {
			count = counts.listio_read;
			rwinfo = &evnt_info->listio_read;
		}
		if (count == 0) {
			rwinfo->min = 0;
			avg = 0;
		} else {
			avg = rwinfo->requested / count;
		}
		wall = (double) rwinfo->time * rtc_factor;
		fprintf(_GL_evnt_logptr,
			 "       %-7s%10d %8.2f %10d  %10d %10d %10d %10d",
			 name[i], count, wall,
			 _evnt_units(evnt_info, rwinfo->requested),
			 _evnt_units(evnt_info, rwinfo->delivered),
			 _evnt_units(evnt_info, rwinfo->min),
			 _evnt_units(evnt_info, rwinfo->max),
			 _evnt_units(evnt_info, avg));
		fprintf(_GL_evnt_logptr, "\n");
	}

	fprintf(_GL_evnt_logptr, "\n");
}
#endif
