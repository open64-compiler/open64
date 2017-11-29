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


#pragma ident "@(#) libu/ffio/evntopen.c	92.3	10/14/99 17:06:10"


#include <errno.h>
#include <fcntl.h>
#include <ffio.h>
#include <malloc.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <cray/assign.h>
#include <cray/mtlock.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <limits.h>
#include <string.h>
#include "evntio.h"
#include "fxlist.h"

DECLARE(EVENT_XLIST);
struct xtr_s EVENT_XLIST_P = { EVENT_XLIST };

/*
 * the following global variables are used when opening a log
 * file for layer diagnostics
 */
FILE	*_GL_ffio_logptr = NULL;
FILE	*_GL_evnt_logptr;
int	_GL_checked_FFIO_LOGFILE = FALSE;
struct EVNT_FILE_TRACE _GL_evnt_trc_file[MAX_TRACE_FILES];

/*
 * _evnt_open
 *
 * Open routine for "event" layer.
 *
 * Input:
 *	name	- name of file to open
 *	oflags
 *	mode
 *	fio	- ffio file descriptor
 *	spec
 *	stat	- pointer to status return word
 *	cbits
 *	cblks
 *	oinf
 *
 * Output:
 *	Returns: pointer to fdinfo block from next lower layer on success.
 *		 -1 if an error occurred.
 */
_ffopen_t
_evnt_open(const char *name, int oflags, mode_t mode, struct fdinfo *fio,
	  union spec_u *spec, struct ffsw *stat, long cbits, int cblks,
	  struct gl_o_inf *oinf)
{
	_ffopen_t     	nextfio;
	int     	ret;
	union spec_u 	*nspec;
	int     	status;
	struct evnt_f 	*evnt_info;
	struct stat 	fstat;
	int     	trc_file_number = 0;
	rtc_t     	start_rtc, finish_rtc;
	struct ffsw 	log_stat;
	int         	log_ubc = 0;
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	int     	end_open_link = _evnt_OPEN_LINK;
#elif	defined(__mips)
	struct dioattr	dio;
#endif
	int		isvalid;
	static int 	evnt_atexit_called = FALSE;
	struct EVNT_FILE_TRACE *trace_file;


	/*
	 * post atexit so we can call it to close our 
	 * diagnostic and trace files
	 */
	if (evnt_atexit_called == FALSE) {
		evnt_atexit_called = TRUE;
		atexit(_evnt_atexit);
	}

	fio->lyr_info = NULL;  /* set to NULL for the benefit of _evnt_clfree */

	evnt_info = (struct evnt_f *) calloc(1, sizeof(struct evnt_f));
	if (evnt_info == NULL)
		goto nomem;

	fio->lyr_info = (char *) evnt_info;

	/*
	 * set default layer options in envt_info
	 */
	evnt_info->optflags.diag	= TRUE;	/* default is full diagnostics*/
	evnt_info->optflags.full_diag	= TRUE;	/* default is full diagnostics*/
	evnt_info->optflags.summary	= FALSE;
	evnt_info->optflags.brief	= FALSE;
	evnt_info->optflags.trace	= FALSE;
	evnt_info->optflags.rtc		= RTC_MODE;/* use rtc by default */
	evnt_info->optflags.k		= FALSE;
	evnt_info->optflags.m		= FALSE;
	evnt_info->optflags.g		= FALSE;

	evnt_info->optflags.bytes	= TRUE; /* default units */
	evnt_info->optflags.words	= FALSE; 
	evnt_info->optflags.blocks	= FALSE; 

	evnt_info->do_zeros = TRUE;	/* zero fill diag fields by default */


	evnt_info->extend_trackers_failed = FALSE;

	/*
	 * malloc space for async tracker structures
	 */
	if ((evnt_info->async_tracker = _evnt_add_trackers(NUM_TRACKERS))
	    == NULL)
		goto nomem;
	evnt_info->num_async_trackers = NUM_TRACKERS;

	/*
	 * pick up user requested options - 
	 * get values from the FFIO spec
	 */
	_evnt_getopts(evnt_info, spec);

	/*
	 * open diagnostics file
	 */
	if (evnt_info->optflags.diag == TRUE) {
		if(_evnt_open_log_file(evnt_info) == ERR) {
			goto badret;
		} else {
			if (_GL_ffio_logptr != NULL)
				_GL_evnt_logptr = _GL_ffio_logptr;
			else
				_GL_evnt_logptr = stderr;
		}
	}

	/*
	 * open trace file if we're tracing 
	 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace == TRUE) {
		trc_file_number = _ff_nparm_getv(spec, 1, &isvalid);

		/*
		 * check for valid file number - issue message to stats file
		 */
		if (evnt_info->optflags.diag == TRUE &&
		    trc_file_number < 0 || trc_file_number > 9) {
			fprintf(_GL_evnt_logptr,
				"Invalid trace file number specified: %d.  Valid numbers are 0-9.\n     ffio.trace will be used\n", trc_file_number);
			trc_file_number = 0;
		}

		evnt_info->trc_file_number = trc_file_number;
		trace_file = &(_GL_evnt_trc_file[trc_file_number]);

		if (_evnt_open_trace_file(fio, name, evnt_info) == ERR)
			goto badret;
	}
#endif
#ifdef EVNT_DEBUG
	fprintf(stderr,"event layer options:\n");
	fprintf(stderr,
	   "diagnostics: diag = %d, full_diag = %d, summary = %d, brief = %d\n",
			evnt_info->optflags.diag,
			evnt_info->optflags.full_diag, 
			evnt_info->optflags.summary,
			evnt_info->optflags.brief);
	fprintf(stderr,"tracing:  trace = %d\n", evnt_info->optflags.trace);
	fprintf(stderr,"trc_file_number = %d\n", trc_file_number);
	fprintf(stderr,"clock:  rtc = %d\n", evnt_info->optflags.rtc);
	fprintf(stderr,"units:  bytes = %d, words = %d, blocks = %d\n", 
			evnt_info->optflags.bytes,
			evnt_info->optflags.words,
			evnt_info->optflags.blocks);
	fprintf(stderr,"size:  k = %d, m = %d, g = %d\n", 
			evnt_info->optflags.k,
			evnt_info->optflags.m,
			evnt_info->optflags.g);
#endif

	/*
	 * initialize minimums to large values for diagnostics
	 */
	if (evnt_info->optflags.diag) {
		long bigmin;
		if (sizeof(size_t) == sizeof(long)){
			bigmin = LONG_MAX;
		}
		else {
			bigmin = INT_MAX;	
		}
		evnt_info->listio_write.min = bigmin;
		evnt_info->listio_writea.min = bigmin;
		evnt_info->listio_read.min = bigmin;
		evnt_info->listio_reada.min = bigmin;
		evnt_info->write.min = bigmin;
		evnt_info->writea.min = bigmin;
		evnt_info->read.min = bigmin;
		evnt_info->reada.min = bigmin;
		strncpy(evnt_info->name, name, NAMLEN);
	}

	/*
	 * Get the FFIO spec for the next lower layer.
	 */
	nspec = spec;
	NEXT_SPEC(nspec);

	/*
	 * Open the layers below this one.
	 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace == TRUE)
		trace_file->open_level_count++;
#endif

	start_rtc = RTC();
	nextfio = _ffopen(name, oflags, mode, nspec, stat, cbits, cblks, 
			  NULL, oinf);
	finish_rtc = RTC();

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace == TRUE)
		trace_file->open_level_count--;
#endif
	evnt_info->open_time = finish_rtc - start_rtc;

	if (nextfio == _FFOPEN_ERR)
		goto badret;

	if (1) {
		struct fdinfo *nfioptr;

		nfioptr = (struct fdinfo *) nextfio;

		ret = XRCALL(nfioptr, fcntlrtn) nfioptr, FC_STAT, &fstat, stat);

		if (oflags & O_TRUNC)
			evnt_info->cur_size = 0;
		else
			evnt_info->cur_size = fstat.st_size;

		if (oflags & O_APPEND)
			evnt_info->cur_pos = evnt_info->cur_size;
		else
			evnt_info->cur_pos = 0;

#ifdef __mips
		ret = XRCALL(nfioptr, fcntlrtn) nfioptr, FC_DIOINFO, &dio, stat);
		if (ret == ERR)evnt_info->odirect = 0;
		else {
			evnt_info->odirect = 1;
			evnt_info->miniosize = dio.d_miniosz;
			evnt_info->chunksize = dio.d_miniosz;
			evnt_info->diskalign = dio.d_miniosz;
			evnt_info->maxiosize = dio.d_maxiosz;
			evnt_info->memalign = dio.d_mem;
		}
#endif
		evnt_info->max_size = evnt_info->cur_size;

		evnt_info->oflags = oflags;

		evnt_info->cbits = cbits;
		evnt_info->cblks = cblks;

#ifdef DO_WE_WANT_FAKE_SDS
		if (get out of * oinf) {
			evnt_info->fake_sds = TRUE;
			evnt_info->sector_mask = 4095;
		} else {
			evnt_info->fake_sds = FALSE;
			evnt_info->sector_mask = fstat.st_blksize - 1;
		}
#endif
		/* we're not monitoring ssreads yet */
		evnt_info->fake_sds = FALSE;  
		evnt_info->sector_mask = fstat.st_blksize - 1;
	}

	/*
	 * write some initial information to the trace file
	 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace) {
	    if (trace_file->ptr) {
		struct evnt_open_info *open_info;
		int     next_open_link;

		open_info = &(evnt_info->open_info);

		evnt_info->fd = (trace_file->file_info.open_count) << 48;

		open_info->fd = _evnt_OPEN | evnt_info->fd;
		open_info->oflags = oflags;
		open_info->mode = mode;
		strncpy(open_info->name, name, 80);
		open_info->name[79] = 0;
		open_info->open_size = evnt_info->cur_size;
		open_info->rtc_14 = RTC();
		open_info->rtc_15 = RTC();
		open_info->nextfio = nextfio;
		strcpy(open_info->parent_child, "??? <-> ???");
		open_info->next_open_pos = 'UNUSED';
		open_info->max_size = evnt_info->max_size;
		open_info->close_rtc = 0;
		open_info->event_count = 0;
		open_info->start_rtc = start_rtc;
		open_info->finish_rtc = finish_rtc;
		open_info->d_nextfio = nextfio;

		next_open_link = EVNT_XR_TELL();
		evnt_info->open_pos = next_open_link + 8;

		if (trace_file->program_info.open_count == 0)
		    trace_file->program_info.first_open_pos = next_open_link;

		if (trace_file->file_info.open_count == 0) {
		    trace_file->file_info.first_open_pos = next_open_link;
		} else {
		    long    link_word;

		    status = EVNT_XR_SEEK(trace_file->file_info.last_open_link, 
				       SEEK_SET);
		    link_word = _evnt_OPEN_LINK | next_open_link;
		    status = EVNT_XR_WRITE(&link_word, 8, 1);
		    status = EVNT_XR_SEEK(0, SEEK_END);
		}

		trace_file->file_info.last_open_link = next_open_link;

		status = EVNT_XR_WRITE(&end_open_link, 1, sizeof(int));
		status = EVNT_XR_WRITE(open_info, 1, 
				       sizeof(struct evnt_open_info));
	    }

	    evnt_info->logged_count =
		&(_GL_evnt_trc_file[evnt_info->trc_file_number].program_info.event_count);
	    trace_file->file_info.open_count++;
	    trace_file->program_info.open_count++;

	    EVNT_TRACE_SIZE;
	    INC_GLOBAL_LOG_COUNT(open);
	    _evnt_trace_flush(fio, evnt_info);
	    evnt_info->need_parent_child = TRUE;
	}
#endif
	evnt_info->counts.size_changes = 0;
	evnt_info->counts.open = 1;
	evnt_info->counts.total = 1;

	return (nextfio);

nomem:
badret:
	_evnt_clfree(fio);
	return (_FFOPEN_ERR);

}


/*
 * _evnt_add_trackers
 *
 * Allocate space for async tracker structures 
 *
 * Input:
 *	num_to_add	- number of structures to allocate space for
 *
 * Output:
 *	Returns:	pointer to first structure if calloc successful
 *			0 if calloc unsuccessful
 *	
 */
struct evnt_async_tracker * 
_evnt_add_trackers(int num_to_add)
{
	struct evnt_async_tracker *first;
	struct evnt_async_tracker *this_tracker;
	int     i;

	first = (struct evnt_async_tracker *) calloc(1,
					  sizeof(struct evnt_async_tracker));
	if (first == NULL)
		goto nomem;

	this_tracker = first;

	for (i = 0; i < num_to_add - 1; i++) {
	    this_tracker->next_tracker = (struct evnt_async_tracker *) calloc(1,
					  sizeof(struct evnt_async_tracker));
	    this_tracker->mode = TRACKER_FREE;
	    if (this_tracker->next_tracker == NULL)
		goto nomem;
	    this_tracker = this_tracker->next_tracker;
	}

	this_tracker->next_tracker = NULL;
	this_tracker->mode = TRACKER_FREE;

	return (first);

nomem:
	return (0);
}



/*
 * _evnt_open_trace_file
 *
 * Open file that we'll put tracing info in
 *
 * Input:
 *	fio		- ffio file descriptor
 *	name		- name of file we're monitoring
 *	evnt_info	- structure containing event information (timings, etc)
 *
 * Output:
 *	Returns:	ERR if open unsuccessful
 *			0 if successful
 */
_evnt_open_trace_file(
struct fdinfo *fio,
const char *name,
struct evnt_f *evnt_info)
{
	char    	file_name[128];
	char   		*name_ptr;
	int     	trc_file_number;
	struct ffsw	open_ffsw;
	int     	status;
	int     	append;
	int     	oflags;
	long    	program_keyword;
	struct ffsw	log_stat;
	int		log_ubc = 0;
	struct EVNT_FILE_TRACE *trace_file;

	trc_file_number = evnt_info->trc_file_number;
	trace_file = &(_GL_evnt_trc_file[trc_file_number]);

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (evnt_info->optflags.trace) {
	    if (trace_file->ptr == NULL) {
		long    start_rtc;
		double   rtc_factor;

		file_name[0] = 0;
		if (trace_file->being_opened) {
		    if (_GL_evnt_logptr) {
			fprintf(_GL_evnt_logptr,
				"Attempting to use same event.fil_num=%d for events file %s\n",
				trc_file_number,
				name);
		    }

		    return (ERR);

		}

		if ((name_ptr = getenv("FF_IO_TRACE_FILE")) != NULL) {
		    strcpy(file_name, name_ptr);
		} else {
		    strcpy(file_name, "ffio.trace");
		}

		if (trc_file_number > 0) {
		    char    digits[8];

		    sprintf(digits, ".%d", trc_file_number);
		    strcat(file_name, digits);
		}

		name_ptr = file_name;
		append = FALSE;
		if (*name_ptr == '+') {
		    append = TRUE;
		    name_ptr++;
		}

		start_rtc = RTC();

		rtc_factor = RTC_FACTOR;

		trace_file->cur_usage = 0;
		trace_file->max_usage = 0;
		strncpy(trace_file->file_info.magic, EVNT_MAGIC_STRING, 7);
		strncpy(trace_file->file_info.version, "7.1BF2", 7);
		trace_file->file_info.start_rtc = start_rtc;
		trace_file->file_info.rtc_factor = rtc_factor;
		trace_file->file_info.event_count = 0;
		trace_file->file_info.open_count = 0;
		trace_file->file_info.program_count = 0;
		trace_file->file_info.reserve_1 = 'f_rsv_1';
		trace_file->file_info.reserve_2 = 'f_rsv_2';
		trace_file->file_info.reserve_3 = 'f_rsv_3';
		trace_file->file_info.reserve_4 = 'f_rsv_4';

		/* 
		 * set program_keyword so we can tell if we have valid
		 * trace file or not when running iox
		 */
		program_keyword =
		 ((_evnt_PROGRAM_START) | (sizeof(struct EVNT_PROGRAM_INFO) - 8));
		trace_file->program_info.keyword = program_keyword;
		trace_file->program_info.next_program_info_pos = 0;
		trace_file->program_info.start_rtc = start_rtc;
		trace_file->program_info.exit_rtc = start_rtc;
		trace_file->program_info.event_count = 0;
		trace_file->program_info.open_count = 0;
		trace_file->program_info.reserve_1 = 'p_rsv_1';
		trace_file->program_info.reserve_2 = 'p_rsv_2';
		trace_file->program_info.reserve_3 = 'p_rsv_3';
		trace_file->program_info.reserve_4 = 'p_rsv_4';
		strcpy(trace_file->program_info.name, _argv[0]);

try_again:
		if (append) {
		    oflags = O_RDWR;
		    trace_file->being_opened = TRUE;
		    evnt_info->file_ptr = EVNT_XR_OPEN(name_ptr);
		    trace_file->being_opened = FALSE;
		    if (evnt_info->file_ptr <= NULL) {
			/*
			 * file did not exist, lets just open
			 * it non-append
			 */
			evnt_info->file_ptr = NULL;
			append = FALSE;
			goto try_again;
		    }

		    trace_file->ptr = evnt_info->file_ptr;

		    /*
		     * check for valid trace file to
		     * append to (close if not valid)
		     */
		    if (_evnt_restart_trace_file(evnt_info->file_ptr,
			 		  trc_file_number, name_ptr) == FALSE) {
			status = EVNT_XR_CLOSE();
			trace_file->ptr = NULL;
			evnt_info->file_ptr = NULL;
		    }
		} else {
		    oflags = O_RDWR | O_CREAT | O_TRUNC;
		    trace_file->being_opened = TRUE;
		    evnt_info->file_ptr = EVNT_XR_OPEN(name_ptr);
		    trace_file->being_opened = FALSE;
		}

		trace_file->ptr = evnt_info->file_ptr;

		if ((int) evnt_info->file_ptr <= 0) {
		    evnt_info->file_ptr = NULL;
		    evnt_info->optflags.trace = FALSE;
		    if (_GL_evnt_logptr) {
			fprintf(_GL_evnt_logptr,
			      "Unable to open event trace file %s\n", name_ptr);
		    }
		}

		trace_file->ptr = evnt_info->file_ptr;
		trace_file->file_info.program_count++;
		trace_file->file_info_pos = 0;
		EVNT_WRITE_FILE_INFO(trc_file_number);

		trace_file->program_info_pos = EVNT_XR_SEEK(0, SEEK_END);

		if (trace_file->file_info.program_count == 1) {
		    /* update the first word in file_info */
		    trace_file->file_info.first_program_pos = 
				trace_file->program_info_pos;
		} else {
		    /* update the link word in the last program_info */
		    status = EVNT_XR_SEEK(trace_file->file_info.last_program_link, SEEK_SET);
		    status = EVNT_XR_WRITE(&(trace_file->program_info_pos), 
					   sizeof(int), 1);
		}

		trace_file->file_info.last_program_link = 
				trace_file->program_info_pos + 8;

		EVNT_WRITE_PROGRAM_INFO(trc_file_number);

	    } else {
		evnt_info->file_ptr = trace_file->ptr;
	    }

	}
#endif
	return (0);
}



#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
/*
 * _evnt_restart_trace_file
 *
 * Check for valid trace file to append to.  Seek to end of
 * file to start appending trace information.
 *
 * Input:
 *	file_ptr	- trace file descriptor
 *	trc_file_number	- number of file to use (1 of 10 files specified 
 *			  with environment variable 
 *	name		- name of trace file we're trying to restart
 *
 * Output:
 *	Returns:	FALSE if we were unable to restart file
 *			TRUE if restart was successful
 */
_evnt_restart_trace_file(int file_ptr, int trc_file_number, char *name)
{
	int     	status;
	struct ffsw	log_stat;
	int		log_ubc = 0;
	struct evnt_f 	evnt_info_dummy;
	struct evnt_f 	*evnt_info;
	struct EVNT_FILE_TRACE *trace_file;

	trace_file = &(_GL_evnt_trc_file[trc_file_number]);

	if ((int) file_ptr <= 0)
		return (FALSE);

	evnt_info_dummy.file_ptr = file_ptr;
	evnt_info = &evnt_info_dummy;

	EVNT_READ_FILE_INFO(trc_file_number);
	if (strcmp(trace_file->file_info.magic, EVNT_MAGIC_STRING))
		goto error;
	if (strcmp(trace_file->file_info.version, "7.1BF2") != NULL) {
		fprintf(_GL_evnt_logptr,
		   "\nUnable to open events file %s for appending\n", name);
		fprintf(_GL_evnt_logptr, 
		        "   file version =%s   event layer version=%s\n\n",
			trace_file->file_info.version, "7.1BF2");
		return (FALSE);
	}
	status = EVNT_XR_SEEK(0, SEEK_END);

	return (TRUE);

error:
	return (FALSE);
}
#endif



/*
 * _evnt_getopts(evnt_info, spec)
 *
 * Pick up options that user requested on assign
 * command
 *
 * Input:
 *	evnt_info     - structure containing event information (timings, etc)
 *	spec	      - pointer to ffio specification word
 *
 * Output:
 */
void
_evnt_getopts(struct evnt_f *evnt_info, union spec_u *spec)
{
	switch(spec->fld.recfmt) {
	case TR_EVNT_NOSTAT:
                evnt_info->optflags.diag = FALSE;	/* disable diagnostics*/
                evnt_info->optflags.full_diag = FALSE;	/* disable diagnostics*/
		evnt_info->do_zeros = FALSE;	
		break;
	
	case TR_EVNT_SUMMARY:
                evnt_info->optflags.summary = TRUE;	/* diagnostics summary*/
                evnt_info->optflags.full_diag = FALSE;
		evnt_info->do_zeros = FALSE;
		break;

	case TR_EVNT_BRIEF:
                evnt_info->optflags.brief = TRUE;	/* brief diagnostics */
                evnt_info->optflags.full_diag = FALSE;
		evnt_info->do_zeros = FALSE;
		break;

	/*
	 * the following record fmts will not
	 * be used until ASSIGN command-line
	 * parsing can handle more than 2 types
	 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	/* Before using this on mips, look at the RTC definition in evntio.h */
	case TR_EVNT_CPC:
                evnt_info->optflags.rtc = CPC_MODE;	/* use cpc */
		break;
#endif

	case TR_EVNT_WORDS:
                evnt_info->optflags.words = TRUE;
                evnt_info->optflags.bytes = FALSE;
		break;

	case TR_EVNT_BLOCKS:
                evnt_info->optflags.blocks = TRUE;	
                evnt_info->optflags.bytes = FALSE;	
		break;

	case TR_EVNT_K:
                evnt_info->optflags.k = TRUE;	
		break;

	case TR_EVNT_M:
                evnt_info->optflags.m = TRUE;	
		break;

	case TR_EVNT_G:
                evnt_info->optflags.g = TRUE;	
		break;
	}

	/* 
	 * pick up type 2
	 */
	switch(spec->fld.subtype) {
	/*
	 * We can write a trace file on PVP and MPP systems, but we 
	 * don't have any tools for looking at it. Writing a trace file
	 * on Irix systems isn't supported yet.
	 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	case TR_EVNT_TRACE:
                evnt_info->optflags.trace = TRUE;	/* enable tracing */
		break;
#endif
	}
}




/*
 * _evnt_timer()
 *
 * Use this routine in place of
 * rtc() when debugging so we
 * can diff the trace output more
 * easily.
 *
 * Input:
 *
 * Output:
 *      Counter that just counts the
 * 	number of times RTC() was called
 */
int
_evnt_timer()
{
	static int counter = 0;

	return(counter++);
}



/*
 * _evnt_open_log_file
 *
 * Open log file
 *
 * Input:
 *
 * Output:
 *      Returns:        ERR if open unsuccessful
 *                      0 if successful
 */
_evnt_open_log_file(struct evnt_f *evnt_info)
{
	char   *name;

	if (_GL_checked_FFIO_LOGFILE == FALSE) {

		_GL_checked_FFIO_LOGFILE = TRUE;
		/* 
		 * we need to change way we're picking up 
		 * the diagnostics file name when the ASSIGN command
		 * can handle more than 2 types for ffio - it will
		 * be specified on the command line, rather than with
		 * an environment variable 
		 * Well - actually it is a little more complicated than this.
		 * We don't have a way to pass strings with the assign info
		 */
#ifdef _CRAYT3E
		/*
		 * If the user specifies FF_IO_LOGFILEPE, then
		 * each PE has its own log file, whose name is
		 * x.pe where pe is the pe number.
		 * If both FF_IO_LOGFILE and FF_IO_LOGFILEPE are
		 * specified, FF_IO_LOGFILEPE is used.
		 */
		evnt_info->optflags.perpelogfile = 0;
		name = getenv("FF_IO_LOGFILEPE");
		if (name)
			evnt_info->optflags.perpelogfile = 1;
		else
#endif
		name = getenv("FF_IO_LOGFILE");
		if (name) {
#ifdef _CRAYT3E
			if (evnt_info->optflags.perpelogfile){
				char *newname;
				newname = malloc(strlen(name)+7); /* allow 5 digits for pe# */
				if (newname == NULL)
					return(ERR);
				sprintf(newname,"%s.%d",name,_my_pe());
				if (*name == '+')
					_GL_ffio_logptr = fopen(newname + 1, "a");
				else
					_GL_ffio_logptr = fopen(newname, "w");
				free(newname);
			}
			else
#endif
			if (*name == '+')
				_GL_ffio_logptr = fopen(name + 1, "a");
			else
				_GL_ffio_logptr = fopen(name, "w");
			if (_GL_ffio_logptr == NULL)
				return (ERR);
		}
#ifdef EVNT_DEBUG
		if (name)
			fprintf(stderr,"statistics going to %s\n", 
				name);
		else
			fprintf(stderr,"statistics going to stderr\n");
#endif
	}

	return(0);
}
