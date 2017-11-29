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


/* USMID @(#) libu/ffio/evntio.h	92.1	10/07/99 22:14:06 */


#include <stdio.h>

typedef long long evint64;

/*
 * Define event layer options -
 *     values are gathered from assign command
 */
#define TR_EVNT_NOSTAT          1
#define TR_EVNT_SUMMARY         2
#define TR_EVNT_BRIEF           3
#define TR_EVNT_TRACE           4
/*
 * the following TR_xxx defines will not be
 * used until ASSIGN command-line parsing
 * for ffio changes to allow more than
 * 2 types
 */
#define TR_EVNT_CPC             5
#define TR_EVNT_WORDS           6
#define TR_EVNT_BLOCKS          7
#define TR_EVNT_K               8
#define TR_EVNT_M               9
#define TR_EVNT_G               10

#ifndef FC_DUMPSTATS
#define FC_DUMPSTATS            102     /* signal intermediate diagnostics */
#endif

#if WILL_ADD_LATER
#define FC_TRACE_CACHE_STATE  105     /* request if eie should report 
				       * cache state changes to event layer */
#define FC_SDS_INCR           120     /* terminal layer to call ssbreak, 
				       * arg = nbytes */

struct cache_state_trace {
	int type;
	int page_start_byte;
	int page_size;
	int start_rtc;
	int finish_rtc;
};
#endif

/* add to ccaio.h when enable cachea tracing */
#define CCA_RELEASE_PAGE 18
#define CCA_QUERY_TRACE   0


#ifdef DO_LISTIO
#undef DO_LISTIO
#endif
#define DO_LISTIO( fio, cmd, list, nreq, ret, stat ) \
            ret = XRCALL((fio), listiortn) (cmd), (list), nreq, stat);

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#define DO_NEXT_LISTIO( llfio, cmd, list, nreq, ret, stat ) { 	\
	int i; 							\
	int fio_save;						\
								\
	fio_save = (int)llfio; 					\
	for(i=0;i<nreq;i++) list[i].li_fildes = (int)llfio; 	\
	DO_LISTIO( llfio, cmd, list, nreq, ret, stat) 		\
	for(i=0;i<nreq;i++) list[i].li_fildes = fio_save; 	\
}
#else
#define DO_NEXT_LISTIO( llfio, cmd, list, nreq, ret, stat ) { 	\
	int i; 							\
	struct fdinfo *fio_save;				\
								\
	fio_save = llfio; 					\
	for(i=0;i<nreq;i++) list[i].li_ffioptr = llfio;	 	\
	DO_LISTIO( llfio, cmd, list, nreq, ret, stat) 		\
	for(i=0;i<nreq;i++) list[i].li_ffioptr = fio_save; 	\
}
#endif
/* 
 * EVNT_MAGIC_STRING is used when restarting a trace file -
 * tells if trace file we're trying to open was created by 
 * event layer
 */
#define EVNT_MAGIC_STRING "ffioevt"	

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

typedef evint64 rtc_t;

#define RTC_MODE 1
#define CPC_MODE 2

#ifdef _CRAY

#ifdef EVNT_DEBUG
#define RTC() _evnt_timer()
#else
#define RTC() ((evnt_info->optflags.rtc == RTC_MODE) ? _rtc() : cpused())
#ifdef _CRAYT3E
#define RTC_FACTOR (1./((double) sysconf(_SC_CLK_TCK)))
#else
#define RTC_FACTOR ((double) sysconf(_SC_CRAY_CPCYCLE)) / 1000000000000.
#endif
#endif

#elif defined(__mips) || defined(_LITTLE_ENDIAN)
extern long long _sysclock_nowrap();
extern double _nowrap_cycles_per_sec;
#define RTC()  _sysclock_nowrap()
#define RTC_FACTOR 1./_nowrap_cycles_per_sec
#else	/* sparc */
#define RTC() clock()
#define RTC_FACTOR 0.000001	/* converts time to seconds */

#endif  /* machine type */

extern char **_argv;

#define EVNTFD evnt_info->file_ptr

/*
 * kludge...
 *
 * the following define gets around a compiler optimization - 
 * if the compiler thinks it may be dealing with an address,
 * it will write it out at subroutine boundaries.  EVNTFD is
 * never equal to 1, so the bit shifting will always happen.
 * We make the compiler think it cat be an address to force
 * it to write the var out at subroutine boundaries.
 */
#ifdef _CRAYT3E
#define XR_FIX(addr) ((EVNTFD)!=1) ? (CPTR2BP(addr)) : (int)(addr)
#else
#define XR_FIX(addr) ((EVNTFD)!=1) ? ((int)(addr)<<6) : (int)(addr)
#endif

#define EVNT_XR_OPEN(name) 					      \
                       ffopens(name, 				      \
                       oflags , 				      \
                       0644 , 0, 				      \
                       &open_ffsw, "cachea.mem:4:4:1" );

#define EVNT_XR_READ(source, size, nwords) 				      \
 (((int)(EVNTFD)>0)?((struct fdinfo *)(EVNTFD))->xr.readrtn( 		      \
       EVNTFD, XR_FIX(source), (size)*(nwords), &log_stat, FULL, &log_ubc ) :0)

#define EVNT_XR_WRITE(source, size, nwords) 				      \
    (((int)(EVNTFD)>0)?((struct fdinfo *) (EVNTFD))->xr.writertn( 	      \
       EVNTFD, XR_FIX(source), (size)*(nwords), &log_stat, FULL, &log_ubc ) :0)

#define EVNT_XR_SEEK(pos, whence)  				      	      \
    (((int)(EVNTFD)>0)?((struct fdinfo *)(EVNTFD))->xr.seekrtn( 	      \
       EVNTFD, pos , whence, &log_stat ) : 0 )

#define EVNT_XR_FLUSH( ) 						      \
    (((int)(EVNTFD)>0)?((struct fdinfo *)(EVNTFD))->xr.flushrtn( 	      \
       EVNTFD, &log_stat ) : 0 )

#define EVNT_XR_CLOSE( ) 						      \
    (((int)(EVNTFD)>0)?((struct fdinfo *)(EVNTFD))->xr.closertn( 	      \
       EVNTFD, &log_stat ) : 0 )

#define EVNT_XR_TELL( )  						      \
    (((int)(EVNTFD)>0)?((struct fdinfo *)(EVNTFD))->xr.seekrtn( 	      \
       EVNTFD, 0, SEEK_CUR, &log_stat ) : 0)

extern FILE *_GL_evnt_logptr;

/* 
 * Currently we put a limit of 10 trace files (file.0 - file.9).
 * The different trace files are specified through a numeric on the
 * assign command, and will be attached to the default trace file
 * name or to the name specified by the user through the FF_IO_TRACE_FILE
 * environment variable.
 */
#define MAX_TRACE_FILES 10

struct EVNT_FILE_INFO {
	char  magic[8];			/* string verifying valid trace file */
	char  version[8]; 		/* event layer version number */
	rtc_t start_rtc;		/* time stamp when file was opened */
	double rtc_factor;		/* used to convert times to wallclock */
	int   event_count;		/* which event we're at */
	int   open_count;		/* which open we're at, 0 == 1st open */
	int   first_open_pos;		/* pointer to head of list of open 
					 * structures */
	int   last_open_link;		/* pointer to tail of list of open
					 * structures */
	int   program_count;		/* number of program structures */
	int   first_program_pos;	/* pointer to head of list of program
					 * structures */
	int   last_program_link;	/* pointer to tail of list of program
					 * structures */
	rtc_t high_rtc;			/* time stamp when writing to trace 
					 * file */
	int   reserve_1;		/* reserved word */
	int   reserve_2;		/* reserved word */
	int   reserve_3;		/* reserved word */
	int   reserve_4;		/* reserved word */
};

struct EVNT_PROGRAM_INFO {
	long  keyword;			/* used to verify valid trace file */
	int   next_program_info_pos;	/* ?? (set but not used) */
	rtc_t start_rtc;		/* ?? */
	rtc_t exit_rtc;			/* time stamp when program info 
					 * written out to trace file */
	int   event_count;		/* which event we're at */
	int   open_count;		/* which open we're at, 0 == 1st open
					 * - used to decide if we print high
					 * water spaced used for files */
	int   first_open_pos;		/* pointer to head of list of open
					 * structures */
	int   last_open_link;		/* pointer to tail of list of open
					 * structures */
	int   reserve_1;		/* reserved word */
	int   reserve_2;		/* reserved word */
	int   reserve_3;		/* reserved word */
	int   reserve_4;		/* reserved word */
	char  name[128];		/* program name */
};

#define EVNT_WRITE_FILE_INFO(trc_file_number) { 			\
	int file_event_count_save; 					\
	int status; 							\
	struct EVNT_FILE_TRACE *f_info; 				\
									\
	f_info = &_GL_evnt_trc_file[trc_file_number]; 			\
	file_event_count_save = f_info->file_info.event_count; 		\
	f_info->file_info.event_count += 				\
        	f_info->program_info.event_count; 			\
	f_info->file_info.high_rtc = _rtc(); 				\
	status = EVNT_XR_SEEK (f_info->file_info_pos, SEEK_SET);	\
	status = EVNT_XR_WRITE(&(f_info->file_info), 			\
                       sizeof(f_info->file_info) , 1); 			\
	f_info->file_info.event_count = file_event_count_save; 		\
}

#define EVNT_READ_FILE_INFO(trc_file_number) { 				\
	int status; 							\
	struct EVNT_FILE_TRACE *f_info; 				\
									\
	f_info = &_GL_evnt_trc_file[trc_file_number]; 			\
	status = EVNT_XR_SEEK(0, SEEK_SET); 				\
	status = EVNT_XR_READ(&(f_info->file_info_pos),			\
                      sizeof(f_info->file_info), 1); 			\
}

#define EVNT_WRITE_PROGRAM_INFO(trc_file_number) {			\
	int status; 							\
	struct EVNT_FILE_TRACE *f_info; 				\
									\
	f_info = &_GL_evnt_trc_file[trc_file_number]; 			\
	f_info->program_info.exit_rtc = _rtc(); 			\
	status = EVNT_XR_SEEK(f_info->program_info_pos, SEEK_SET);	\
	status = EVNT_XR_WRITE(&(f_info->program_info), 		\
                       sizeof(f_info->program_info), 1); 		\
}

#define EVNT_READ_PROGRAM_INFO(trc_file_number) { 			\
	int status; 							\
	struct EVNT_FILE_TRACE *f_info; 				\
									\
	f_info = &_GL_evnt_trc_file[trc_file_number]; 			\
	status = EVNT_XR_SEEK(sizeof(struct EVNT_FILE_INFO), SEEK_SET);	\
	status = EVNT_XR_READ(&(f_info->program_info), 			\
                      sizeof(f_info->program_info), 1); 		\
}

#define EVNT_FLUSH_TRACE_INFO(trc_file_number) { 			\
	int status; 							\
									\
	EVNT_WRITE_FILE_INFO(trc_file_number );				\
	EVNT_WRITE_PROGRAM_INFO(trc_file_number); 			\
	status = EVNT_XR_FLUSH( ); 					\
}

struct EVNT_FILE_TRACE {
	int 	ptr;
	int  	cur_usage;		/* current trace file size */
	int  	max_usage;		/* max trace file size */
	int  	queued_flush;		/* flag to signal queued flush */
	int  	locked;			/* flag - TRUE if in locked region */
	int  	being_opened;		/* flag - TRUE while trace file is 
					 * being opened for appending */
	int  	open_level_count;	/* used to avoid redundant open calls */
	int  	lock;			/* not used ?? */
	int 	file_info_pos;		/* pointer to file structure position
					 * in trace file */
	struct EVNT_FILE_INFO file_info;
	int 	program_info_pos; 	/* pointer to program structure 
				 	 * position in trace file */
	struct EVNT_PROGRAM_INFO program_info;

};
extern struct EVNT_FILE_TRACE _GL_evnt_trc_file[MAX_TRACE_FILES];

/*
 * need to lock writes to stat file until official
 * layer locking code is added (not done yet)
 */
#define EVNT_LOCK_ON { 							\
	_GL_evnt_trc_file[evnt_info->trc_file_number].locked = TRUE; 	\
}

#define EVNT_LOCK_OFF { 						\
	_GL_evnt_trc_file[evnt_info->trc_file_number].locked = FALSE;	\
	if(_GL_evnt_trc_file[evnt_info->trc_file_number].queued_flush) {\
      		_evnt_close_diags(fio, evnt_info, FALSE); 		\
	} 								\
}

#define _evnt_OPEN       	0x100000000000000
#define _evnt_READ       	0x200000000000000
#define _evnt_READA      	0x300000000000000
#define _evnt_READC      	0x400000000000000
#define _evnt_WRITE      	0x500000000000000
#define _evnt_WRITEA     	0x600000000000000
#define _evnt_WRITEC     	0x700000000000000
#define _evnt_CLOSE      	0x800000000000000
#define _evnt_FLUSH      	0x900000000000000
#define _evnt_WEOF       	0xa00000000000000
#define _evnt_WEOD       	0xb00000000000000
#define _evnt_SEEK       	0xc00000000000000
#define _evnt_BKSP       	0xd00000000000000
#define _evnt_POS        	0xe00000000000000
#define _evnt_LISTIO     	0xf00000000000000
#define _evnt_FCNTL		0x1000000000000000
#define _evnt_SIZE             	0x1100000000000000
#define _evnt_EIE_RELEASE_PAGE 	0x1200000000000000
#define _evnt_PROGRAM_START    	0x1300000000000000
#define _evnt_FCNTL_IALLOC     	0x1400000000000000
#define _evnt_OPEN_LINK        	0x1500000000000000

/* 
 * tracker modes
 */
#define TRACKER_FREE   		 0
#define TRACKER_READA  		'R'
#define TRACKER_WRITEA 		'W'
#define TRACKER_LISTIO_READA	'r'
#define TRACKER_LISTIO_WRITEA 	'w'

#define NUM_TRACKERS	5
struct evnt_async_tracker {
        int mode;			/* tracker mode (free, reada, etc ) */
        struct ffsw *stat;		/* pointer to status return  word */
        int logpos;			/* current position in async tracker
					 * structure */
        size_t requested;		/* number of bytes requested to be
					 * transfered */
        struct evnt_async_tracker *next_tracker;
};

/* 
 * structure containing read/write statistics 
 */
struct rw_stats {
	evint64 ill_formed;		/* I/O request not on sector boundary */
	size_t min;			/* minimum number of bytes requested */
	size_t max;			/* maximum number of bytes requested */
	evint64 requested;		/* number of bytes requested to be
					 * read or written */
	evint64 delivered;		/* number of bytes actually read
					 * or written */
	rtc_t time;			/* time to complete read/write event */
	evint64 sync;			/* number of synchronized events ?? */
	int current;			/* number of pending async requests */
	evint64 high_water;		/* max num of pending async requests */
	evint64 all_hidden;		/* num of hidden async events ?? */
};

struct evnt_count {
        evint64 listio_write;		/* number of listio writes */
        evint64 listio_writea;		/* number of async listio writes */
        evint64 listio_read;		/* number of listio reads */
        evint64 listio_reada;		/* number of async listio reads */
        evint64 write;			/* number of write events */
        evint64 writea;			/* number of writea events */
        evint64 read;			/* number of read events */
        evint64 reada;			/* ... */
        evint64 open;
        evint64 readc;
        evint64 writec;
        evint64 close;
        evint64 flush;
        evint64 weof;
        evint64 weod;
        evint64 seek;
        evint64 bksp;
        evint64 pos;
        evint64 fcntl_recall;		/* number of async recalls */
        evint64 fcntl_other;		/* number of unmonitored commands ?? */
        evint64 listio;			/* number of listio requests */
        evint64 listio_seek;
        evint64 size_changes;		/* number of times file changed size */
        evint64 total;			/* number of total events */
        evint64 all_reads;		/* not used */
        evint64 all_writes;		/* not used */
        evint64 cache_page_release;	/* number of cache page releases */
        evint64 fcntl_ialloc;		/* number of ialloc requests */
};

struct evnt_open_info {
	int 	fd;			/* 0 */	     /* open file descriptor */
	int 	oflags;			/* 1 */	     /* open flags */
	mode_t 	mode;			/* 2 */
	char 	name[80];		/* 3 - 12 */ /* file name */
	off_t 	open_size;		/* 13 */     /* orig file size */
	int 	rtc_14;			/* 14 */     /* set, not used */
	int 	rtc_15;			/* 15 */     /* set, not used */
	_ffopen_t nextfio;		/* 16 */     /* pointer to next layer */
	char 	parent_child[80];	/* 17-26 */  /* layer names */
	int 	next_open_pos;		/* 27 */     /* unused */
	off_t 	max_size;		/* 28 */     /* max file size */
	rtc_t 	close_rtc;		/* 29 */     /* time file was closed */
	evint64 	event_count;		/* 30 */     /* total number of events*/
	struct 	evnt_count logged;		     /* struct to hold event
						      * counts */
	rtc_t 	start_rtc;			     /* time ffopen began */
	rtc_t 	finish_rtc;			     /* time ffopen completed */
	_ffopen_t d_nextfio;			     /* same as nexfio above */
};

struct evnt_optflags {
        unsigned diag:1;			/* flag - generate diagnostics */
        unsigned full_diag:1;
        unsigned summary:1;
        unsigned brief:1;
	unsigned trace:1;
	unsigned rtc:1;
	unsigned k:1;
	unsigned m:1;
	unsigned g:1;
	unsigned bytes:1;
	unsigned words:1;
	unsigned blocks:1;
	unsigned perpelogfile:1;
};
#define NAMLEN 256
struct evnt_f {
	struct evnt_optflags optflags;	/* event layer options */
        int     fd;            		/* the file descriptor from fcntl */
        int     trc_file_number;	/* trace file number */
        int     file_ptr;		/* pointer to trace file */
        char    *parent_name;		/* name of layer above */
        char    *child_name;		/* name of layer below */
	int	do_zeros;		/* all diag fields filled */
        int     num_async_trackers;	/* # of tracker structures allocated */
        int     extend_trackers_failed; /* flag - couldn't malloc more tracker
					 * structures */
        struct  evnt_async_tracker *async_tracker;
        int     async_high_water;	/* max number of async I/O requests 
					 * pending */
        int     async_current_count;	/* number of async I/O requests for
					 * current recall */
        int     async_tracker_overwrites;/* number of async tracker structures
					 * were overwritten */
        int     async_untracked_req;	/* # of async requests never recalled */
        int     async_unknown_req;
	int	output_units;		/* units word bitwise */
        int     *logged_count;		/* number of events logged (from 
					 * program_info.event_count) */
        struct  evnt_count counts;	/* number of reads,writes, etc */
        struct  evnt_count reported_counts; /* not used */

        int     not_my_recall;		/* # of async requests we ignored */

        rtc_t   open_time;		/* time to complete ffopen */
        rtc_t   listio_time;		/* time to complete listio request */
        rtc_t   close_time;		/* ... */
        rtc_t   readc_time;
        rtc_t   writec_time;
        rtc_t   flush_time;
        rtc_t   fcntl_recall_time;
        rtc_t   fcntl_ialloc_time;
        rtc_t   fcntl_other_time;
        rtc_t   fcntl_ialloc_req;	/* number of bytes requested */
        rtc_t   fcntl_ialloc_del;	/* number of bytes delivered */
        rtc_t   ill_formed_weod;	/* EOD write on non-sector bound. */
        struct rw_stats   listio_write;
        struct rw_stats   listio_writea;
        struct rw_stats   listio_read;
        struct rw_stats   listio_reada;
        struct rw_stats   write;
        struct rw_stats   writea;
        struct rw_stats   read;
        struct rw_stats   reada;
        char    name[NAMLEN];		/* name of file to be read or written */
        off_t  cur_pos;			/* current position in file */
        off_t cur_size;			/* current file size */
        off_t max_size;			/* high water file size */
        int    need_parent_child;	/* flag - TRUE if need layer names */
        off_t sector_mask;		/* determines if I/O is illformed */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	unsigned memalign;		/* used for O_DIRECT */
	unsigned odirect;		/* used for O_DIRECT */
	unsigned diskalign;
	unsigned miniosize;
	unsigned maxiosize;
	unsigned chunksize;
#endif
        int    oflags;
        int    cblks;
        long   cbits;
        int    open_pos;		/* addr of next open open structure to 
					 * use */
        int    fake_sds;		/* flag - TRUE if monitoring ssreads
					 * and sswrites */
        struct evnt_open_info open_info;
};

#define INC_GLOBAL_LOG_COUNT(type) { 					      \
	(*evnt_info->logged_count)++; evnt_info->open_info.logged.type++;     \
}

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define MEMALIGN(a) (((BPBITOFF(a) & 007) == 0) && (((long)(BPTR2CP(a)) % \
        evnt_info->memalign) == 0))
#elif defined(_CRAYMPP)
/* For well-formed i/o, MPPs require buffer alignment on an 8-word boundary */
#define MEMALIGN(a) (((BPBITOFF(a) & 07) == 0) && (((long)(BPTR2CP(a)) & 0x3f) ==0))
#else
/* On PVPs buffer must be aligned on a word boundary */
#define MEMALIGN(a) (((BPBITOFF(a) & 07) == 0) && (BPTR2CP(a) == (char *)(long *)BPTR2CP(a)))
#endif
#ifdef _CRAY
#define ILL_FORMED(pos, bytes, bufptr)		  	     	 	      \
   ( ((pos) & evnt_info->sector_mask) || ((bytes) & evnt_info->sector_mask) || \
	(!MEMALIGN(bufptr)))
#else
#define ILL_FORMED(pos, bytes, bufptr)			\
   ( (!evnt_info->odirect) || (((pos) % evnt_info->diskalign) != 0) ||  \
	 ((bytes) > evnt_info->maxiosize) || \
	(((bytes ) % evnt_info->chunksize) != 0) || (!MEMALIGN(bufptr) ))
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define EVNT_TRACE_SIZE 						      \
	evnt_info->counts.size_changes++; 				      \
	evnt_info->counts.total++; 					      
#else
#define EVNT_TRACE_SIZE 						      \
	evnt_info->counts.size_changes++; 				      \
	evnt_info->counts.total++; 					      \
	if (evnt_info->optflags.trace) { 				      \
	    int record[2]; 						      \
	    int nwords; 						      \
	    int trace_size_status; 					      \
									      \
	    record[0] = (_evnt_SIZE | evnt_info->fd) | evnt_info->cur_size;   \
	    if (evnt_info->optflags.rtc == RTC_MODE) {			      \
		record[1] = start_rtc; 					      \
		nwords = 2; 					              \
	    } else { 					      		      \
		nwords = 1; 					              \
	    } 					      			      \
	    _Pragma("_CRI suppress record"); 				      \
	    trace_size_status = EVNT_XR_WRITE(record, 8, nwords ); 	      \
	    INC_GLOBAL_LOG_COUNT(size_changes); 			      \
	}
#endif
#define EVNT_CHECK_SIZE 						      \
	if (evnt_info->cur_pos > evnt_info->cur_size) {			      \
            off_t increase; 						      \
									      \
            increase = evnt_info->cur_pos - evnt_info->cur_size; 	      \
            evnt_info->cur_size = evnt_info->cur_pos; 			      \
            EVNT_TRACE_SIZE; 						      \
            _GL_evnt_trc_file[evnt_info->trc_file_number].cur_usage += 	      \
							increase;	      \
            if (_GL_evnt_trc_file[evnt_info->trc_file_number].cur_usage >     \
                _GL_evnt_trc_file[evnt_info->trc_file_number].max_usage)      \
           	    _GL_evnt_trc_file[evnt_info->trc_file_number].max_usage = \
           	    _GL_evnt_trc_file[evnt_info->trc_file_number].cur_usage;  \
            if (evnt_info->cur_size > evnt_info->max_size) 		      \
		evnt_info->max_size = evnt_info->cur_size; 	      	      \
	}

#define EVNT_UPDATE(rw) 						\
        if (evnt_info->optflags.diag) {					\
        	evnt_info->rw.requested += nbytes; 			\
        	evnt_info->rw.min = (nbytes < evnt_info->rw.min) ? 	\
                    		    nbytes : evnt_info->rw.min;   	\
        	evnt_info->rw.max = (nbytes > evnt_info->rw.max) ? 	\
                    		    nbytes : evnt_info->rw.max;   	\
        	if (ILL_FORMED(evnt_info->cur_pos, nbytes, bufptr))	\
         		evnt_info->rw.ill_formed++; 			\
        }

/*
 *	Function prototypes.
 */
void _evnt_sds_diags(struct evnt_f *evnt_info, struct evnt_count counts,
                     char *units, double rtc_factor, int final);
void _evnt_get_parent_child(struct fdinfo *fio, char  **parent, char **child);
void _evnt_trace_flush(struct fdinfo *fio, struct evnt_f *evnt_info);
void _evnt_atexit();
void _evnt_clfree(struct fdinfo *fio);
void _evnt_getopts(struct evnt_f *evnt_info, union spec_u *spec);
void _evnt_close_diags(struct fdinfo *fio, struct evnt_f *evnt_info, int final);
evint64 _evnt_units(struct evnt_f *evnt_info, evint64 value);
int _evnt_bksp(struct fdinfo *fio, struct ffsw *stat);
int _evnt_close(struct fdinfo *fio, struct ffsw *stat);
int _evnt_fcntl(struct fdinfo *fio, int cmd, void *arg, struct ffsw *iostat);
int _evnt_flush(struct fdinfo *fio, struct ffsw *iostat);
int _evnt_listio(int cmd, struct fflistreq *list, int nreq, struct ffsw *iostat);
_ffopen_t _evnt_open(const char *name, int oflags, mode_t mode, 
	       struct fdinfo *fio,
               union spec_u *spec, struct ffsw *stat, long cbits, int cblks,
               struct gl_o_inf *oinf);
int _evnt_open_trace_file(struct fdinfo *fio, const char *name, 
			  struct evnt_f *evnt_info);
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
int _evnt_restart_trace_file(int file_ptr, int trc_file_number, char *name);
#endif
ssize_t _evnt_read(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
               struct ffsw *stat, int fulp, int *ubcp);
ssize_t _evnt_reada(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
                struct ffsw *stat, int fulp, int *ubcp);
ssize_t _evnt_readc(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
                struct ffsw *stat, int fulp);
_ffseek_t _evnt_seek(struct fdinfo *fio, off_t pos, int whence, struct ffsw *stat);
int _evnt_weod(struct fdinfo *fio, struct ffsw *stat);
int _evnt_weof(struct fdinfo *fio, struct ffsw *stat);
ssize_t _evnt_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
                struct ffsw *stat, int fulp, int *ubcp);
ssize_t _evnt_writea(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
                 struct ffsw *stat, int fulp, int *ubcp);
ssize_t _evnt_writec(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
                 struct ffsw *stat, int fulp);
int _evnt_timer();
struct evnt_async_tracker *_evnt_get_tracker(struct evnt_f *evnt_info, 
					      struct ffsw *stat,
			                      char mode, size_t nbytes);
struct evnt_async_tracker *_evnt_add_trackers(int num_to_add);
