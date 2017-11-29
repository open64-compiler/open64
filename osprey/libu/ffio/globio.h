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


/* USMID @(#) libu/ffio/globio.h	92.2	10/07/99 22:14:06 */

#if	_CRAYMPP || __mips
#define _GLOBLAYER_SUPPORTED  1
#endif

#if _GLOBLAYER_SUPPORTED 



#include <ffio.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fortran.h>
#include <stdarg.h>

#if !defined(_ABSOFT)
#include <malloc.h>
#include <sys/fcntl.h>
#include <mpi.h> 	
#include <mpp/shmem.h>
#endif

#if	! __mips && ! _ABSOFT && ! _LITTLE_ENDIAN
#include <sys/listio.h>
#include <sys/iosw.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>


#endif	/* _GLOBLAYER_SUPPORTED */



/*****************************************************************************
 *
 * Constants
 *
 ****************************************************************************/

#define PAR_MAX_FILES 16 

#define WORDSPERBLOCK 512		/* 8-byte words per block */

#define PAR_MAX_FNAME 128
#define PAR_FILE_CLOSED (-1)

#define GLOB_DEF_BUFSIZ	8		/* default buffer page size */
#define GLOB_DEF_NBUF  	4		/* default no. of pages per PE */

#define TR_GLOB_PRIVPOS 1		/* PE-private position */
#define TR_GLOB_GLOBPOS 2		/* application-global position */

#ifdef _CRAYT3E
#define STRMPAD		(3*64)		/* byte size of 3 cache lines */
#else
#define STRMPAD		64		/* for testing purposes */
#undef STRMPAD
#define STRMPAD		0
#endif

#define YMPBLOCK	4096		/* bytes per "block" */

#define GLIO_DELAY 2000		/* nanoseconds to delay in spin loops */

#define GLOBERRMSG	"\nError in \"global\" layer processing"
#define GLOBWARNMSG	"\nWarning in \"global\" layer processing"

/*
 * Define constants from mpi.h if needed.
 */
#if __mips && !defined(MPI_H_INCLUDED)
#define MPI_SUCCESS             0
#define MPI_MAX_ERROR_STRING    256
#define MPI_INT                 3
#define MPI_CHAR                1
#define MPI_ANY_SOURCE          (-2)
#define MPI_ANY_TAG             (-1)
#endif


#if _GLOBLAYER_SUPPORTED 		/* some fields are too wide for	   */
					/* systems where ints are 32 bits  */

/*****************************************************************************
 *
 * Function type macros
 *
 ****************************************************************************/

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

/*
 * Compute the address (void*) of buffer number "buf_num".
 */
#define SHARED_BUF_ADDR(buf_num) ((char *)glob_info->cchpages + \
		(buf_num * (STRMPAD + glob_info->pgsize_bytes)))

#if __mips
#define	_glio_shmem_quiet()		__synchronize()
#else
#define	_glio_shmem_quiet()		_shmem_quiet()
#endif

#if __mips
extern long long _sysclock_nowrap();
#define _rtc()	_sysclock_nowrap()
#endif

#define CEILING(val, chunk) \
	(((val) % (chunk)) ? ((val) + (chunk) - ((val) % (chunk))) : (val))

/*****************************************************************************
 *
 * Typedefs and structures
 *
 ****************************************************************************/

#ifdef _CRAY
typedef short     int32_t;
typedef long      int64_t;
typedef long      off64_t;
#endif

/*
 * glio_group_t describes a group of SHMEM/MPI/PVM processes.
 */
typedef struct glio_group {
	
	int groupsz;            /* number of processes in group */
	int myrank;		/* ordinal of this process within the group */

	enum progtype {
		GR_DEFAULT	=0,
		GR_MPI		=1,
		GR_PVM		=2,
		GR_SHMEM 	=3,
		GR_ONE 		=4	/* trivial group of size one */
	}
                        grtype;

	union {
		struct {
			int group;   	/* handle */
		} shmem;
		struct {
			MPI_Comm comm;
		} MPI;
	} u;
} glio_group_t;

/*
 * glio_arena_t manages a symmetric memory arena.    
 */

typedef struct glio_arena {

	glio_group_t grp;	/* the associated group of processes */

	size_t asize;		/* size of each process' arena portion. */
				/* the total arena size is asize * groupsz */
	void *mybase;		/* address of my part of arena */
	void *freespace;	/* address of my part of arena */
#if __mips
	void *ashm;		/* address of shared memory arena */
#endif /* __mips */

#if _CRAY
	struct  base_pe {
	  void *base;
	  int pe;
	} *peshm;		/* list of pointers to each PE's section */
				/* of the arena. */
#endif

} glio_arena_t;


#if __mips
#define REMPTR(arp, addr, rank) \
	((void *) \
	 ((char *)arp->ashm + (rank * arp->asize) + \
	  ((char *)addr - (char *)arp->mybase)) )

#else	/* T3E */

#define REMPTR(arp, addr, rank) \
	((void *) \
	((char *)(arp->peshm[rank].base) + \
	 ((char *)addr - (char *)arp->mybase)) )

#define REMPE(rank) (arp->peshm[rank].pe)

#endif

struct _par_trace_entry {
	int magic:   16;
	int data_pe: 11;	/* holds 2^11 or 2048 PEs */
	int io_pe:   11;
	int io_buf:  10;
	unsigned read: 1;	/* 1 if read, 0 if write */
	int fd:      15;
	int64_t rtc;
	int64_t offset;
};

/*
 * Data structures used for tracing I/O requests.
 * 
 * Revision: "par_io_trace.h,v 1.2 1996/01/08 04:03:07 nluzmoor Exp $"
 *
 */

/*
 * Don't make this structure larger than one word.  Only a single cache line
 * is invalidated when reading it on PE 0.
 */

struct _par_trace_info {
	int pe:	 	32;
	int index:	32;
}; 

#define PAR_IO_TRACEFILE "par_io.trace"

#define PAR_TRACE_ENTRIES 1024 /* in-memory trace buffers on each PE */

#define TRACE_REG_MAGIC 0x1aed
#define TRACE_OPEN_MAGIC 0x62fa
#define TRACE_TIMESTAMP_MAGIC 0x76a1
#define TRACE_MAGIC_SIZE 2	/* 2 bytes */
#define GLOBOPT_GLOBPOS 1	/* global file position */
#define GLOBOPT_PRIVPOS 2	/* PE-local file position */
#define PG_DIRTY  1
#define PG_VALID  2
#define PG_BUSY   4
#define PAR_WAKEUP_MAGIC 0xabcd0123

enum io_type { READ_IO, WRITE_IO };

#ifndef FDC_ERR_GLSYS
#define FDC_ERR_GLSYS   5065    /* System layer required under global layer*/
#endif


struct _par_trace_timestamp_entry {
	int magic:	16;
	int pe:		16;
	int pad:	32;
	time_t	time_ticks;
	long	time_rtc;
};

struct _par_trace_open_entry {
	int magic:  16;
	int fd:      8;
	int flags:   8;
	int pe:     16;
	int size:   16;
	long rtc;
};	/* next comes variable length file name string */
	



/*
 *	File state and access statistics.
 */
typedef struct par_file_data {
	int64_t	size;			/* size of file (bytes) */
	int64_t goff;			/* global position if "globpos" */
	int32_t	nwrite;
	int32_t	nread;
	int32_t	sequentially_extend;
	int32_t	seek_extend;
	int32_t	overwrote_page;
	int32_t	write_cache_hits;
	int32_t	write_cache_misses;
	int32_t	read_cache_hits;
	int32_t	read_cache_misses;
	int64_t	bytes_on_disk;
	int64_t	bytes_written;
	int64_t	bytes_read;
	int64_t	disk_bytes_written;
	int64_t	disk_bytes_read;
} par_file_data_t;



/*
 * par_page_description describes the cache pages on each PE.
 */
typedef struct par_page_description
{
	long	flags;	/* must be a full word since we update it remotely */
			/* 0 means the cache page is available. */
	int	pe;	/* owner of this page */
	int	page_num;
	long long last_used;
	int	buf_num;
} par_page_description_t;




/*
 * Cache parameters.  Some of these are being phased out and being put into
 * the glob_f structure.
 */
typedef struct _par_variables {
	short	pepages;	/* pages per PE */
	short	pgsize;		
	short	pgsize_bytes;
	short	maxfiles;
	short	print_stats;	/* keep */
	short	do_tracing;	/* keep */
	int	trace_fd;
	short	disksect;
} _par_variables_t;


/*
 *	gliocoms
 *
 *	Structure for collective communication within open and close
 *	processing.
 */
        struct gliocoms {
                int errn;
                ino_t inode;
                dev_t device;
                int glob_preall_flg;
                int glob_preall_pe;
        };


#define MAXAL 16		/* max number of dynamically allocated blocks */

/*
 *	glob_f structure 
 *
 *	The FFIO info structure.  It must be shmalloc'ed.
 */
struct glob_f {

	/* These fields describe unchanging properties of the file */

	char 	*fname;		/* file name */
	int	sysfd;		/* real underlying file descriptor */
	int	optflags;	/* TR_GLOB_PRIVPOS/GLOBOPT_GLOBPOS */
	int 	user_oflags;	/* flags passed in by user */
	int 	oflags;		/* flags passed to lower layer */
	ino_t	inode;		/* inode number */
	dev_t	device;		/* device number */
	void	*memlist[MAXAL];/* list of memory to free */
	int	memlistsz;	/* number of entries in memlist */
        int     groupsz;        /* size of process group accessing the file */
        int     myrank;         /* 0-based rank for this PE within group */

	unsigned int globpos:1;	/* 1 iff global position */

	/* These fields describe unchanging properties of the cache */
	int64_t pepages;	/* number of cache pages per pe */
	int64_t pgsize;		/* (4096-byte blocks) cache page size */
	int64_t pgsize_bytes;	/* (bytes) cache page size */
	int32_t	pgpad;		/* (bytes) cache page pad size for strmsafety */
	short   print_stats;
	short   do_tracing;
	int 	trace_fd;
	short   disksect;
	glio_arena_t *arp;

	struct gliocoms coms;

	int 	fstate_owner;	/* PE that owns the master fstate structure */


#if _CRAYT3E
	char  	_strmpad[STRMPAD];	/* for T3E streams safety */
#endif

	/* Changing file state attributes */ 

	int64_t	file_off;		/* byte position in file */
	par_file_data_t fstate;		/* state of the file */

	long	fstate_lock;		/* lock for file state */

	long	fsize_lock;		/* lock for file size in 	    */
					/* par_file_data structure.	    */


	par_page_description_t *ppages;	/* cache page state.  This contains */
					/* information for each file page   */
					/* currently held in cache on this  */
					/* PE.				    */

	par_page_description_t *myppages;
					/* myppages is used to hold a copy  */
					/* of the cache page state taken    */
					/* from an arbitrary PE while it is */
					/* locked.	*/

	long *ppage_table_locks;	/* Lock for ppages.  This lock must */
					/* be set when a page gets the 	    */
					/* PG_BUSY state bit set or 	    */
					/* In addition, this lock must be   */
					/* held when preparing a wakeup	    */
					/* word or when signalling the 	    */
					/* wakeup word for any PE waiting   */
					/* for a page on this PE.	    */
					/* There is one lock for every PE.  */
					/* That means that the lock is 	    */
					/* used for all pages owned by a PE.*/
					

	long long *wakeup_bitmap;	/* wakeup bits */

	long long *lwakeup_bitmap;	/* for local work on wakeup bits*/

	long wakeword;			/* used for waking up a PE waiting */
					/* on some BUSY page.		   */

	void *cchpages;			/* cache pages for this PE */

	long *my_iobuf;

	
	
};


/*
 * Macros for manipulating bitmaps.  These macros take a pointer to long
 * and an integer.  These were borrowed from "fsck".
 */
#if	_CRAY
#define BIT0 0x8000000000000000UL
#else
#define BIT0 0x8000000000000000ULL	/* should really work on T3E too */
#endif
#define BITPW 64
#define SET_BIT(lp,x)  ( *((lp) + ((x)>>6)) |=(BIT0 >> ((x) & 077)) )
#define CLR_BIT(lp,x) ( *((lp) + ((x)>>6)) &= ~(BIT0 >> ((x) & 077)) )
#define TST_BIT(lp,x)  ( *((lp) + ((x)>>6)) & (BIT0 >> ((x) & 077)) )


#if __mips
/*****************************************************************************
 *
 * Weak declarations
 *
 * On IRIX/MIPS systems we cannot assume that libsma will be installed or
 * linked in.  libffio.so must not have a dependency on libsma.so.   Of 
 * course a user of the global layer must link in either libsma or libmpi,
 * but FFIO users not utilizing the global layer need not do this.
 *
 ****************************************************************************/

#pragma weak _my_pe
#pragma weak _num_pes
#pragma weak shmalloc
#pragma weak shfree

#pragma weak _shmem_get64
#pragma weak _shmem_wait
#pragma weak _shmem_getmem
#pragma weak _shmem_putmem
#pragma weak _shmem_put64
#pragma weak _shmem_get32
#pragma weak _shmem_put32
#pragma weak _shmem_int_get
#pragma weak _shmem_int_put
#pragma weak _shmem_long_get
#pragma weak _shmem_long_put
#pragma weak _shmem_int_p
#pragma weak _shmem_int_g
#pragma weak _shmem_quiet
#pragma weak _sma_group_barrier
#pragma weak start_pes
#pragma weak _sma_global_heap_alloc
#pragma weak _sma_global_heap_free
#pragma weak _shmem_group_inquire
#pragma weak _shmem_barrier_all
#pragma weak shmem_barrier_all

#pragma weak MPI_Init
#pragma weak MPI_Barrier
#pragma weak MPI_Initialized
#pragma weak MPI_Send
#pragma weak MPI_Recv
#pragma weak MPI_Error_string
#pragma weak MPI_Comm_size
#pragma weak MPI_Comm_rank


/*
 * Functions with no prototypes imported from header files.
 */
void *_sma_global_heap_alloc();
void _sma_global_heap_free();
void _sma_group_barrier();


#endif /* __mips */

#if _CRAYT3E


#pragma soft MPI_Init
#pragma soft MPI_Barrier
#pragma soft MPI_Initialized
#pragma soft MPI_Send
#pragma soft MPI_Recv
#pragma soft MPI_Error_string
#pragma soft MPI_Comm_size
#pragma soft MPI_Comm_rank


#endif /* _CRAYT3E */


/*****************************************************************************
 *
 * Externs
 *
 ****************************************************************************/

extern short _glob_io_init_called;
extern _par_variables_t _par_vars;


/* extern  data */
extern struct _par_trace_info _par_trace_vars; /* PE 0's copy is used */
extern long _par_trace_lock;
extern struct _par_trace_entry *_par_trace_buf, *_par_local_tracebuf;
extern glio_group_t _glio_curgroup;

/* function prototypes */
extern void _par_get_new_page(struct fdinfo *fio,
		struct par_page_description * p, size_t size);
extern void _par_stats_print(struct fdinfo *fio);
extern void _par_write_page(struct fdinfo *fio,
		struct par_page_description * p);
extern int _par_get_file_size(int fd);
extern void _par_abort(glio_arena_t *arp, const char *s);
extern void _par_flush_bufs(struct fdinfo *fio, int target_pe,
		int do_deallocate);
extern void _par_deallocate_page(struct fdinfo *fio, 
		struct par_page_description *p);
extern struct par_page_description *_par_get_page_locked(struct fdinfo *fio,
		int page_num);
extern	struct par_page_description * _par_get_pe_dirty_page(struct fdinfo *fio,
		int pe, int deallocate_valid_pages);
extern void _par_unlock_page(struct fdinfo *fio,
		struct par_page_description * p);
extern void _par_environment_check(void);
extern void _par_pgsize_warning(int pgsize, int sectsize);
extern int _par_set_open_bits(const char * const name, long * const cbits,
		 int * const cblks);
extern int _find_fbs_clr(long long * bitmap, int max_bits);
extern void _par_wait_for_page(struct fdinfo *fio, int pe);
extern void _par_wakeup_waiting_PEs(int pe);
extern void _par_io_trace(struct fdinfo *fio, int pe, int buf, long offset,
		enum io_type io_direction);
extern void _par_write_trace(struct fdinfo *fio);
extern void _par_write_timestamp_trace(struct fdinfo *fio, int pe);
extern void _par_write_open_trace(struct fdinfo *fio, int flags,
		const char * fname);
extern void _par_do_io(struct fdinfo *fd, off64_t offset, void * addr,
	size_t size, enum io_type io_direction);
extern void _par_update_disk_stats(struct fdinfo *fio, int page_num,
		enum io_type io_direction, size_t size);
extern void _par_bcast0(glio_arena_t *arp, void *ptr, int size);
extern void *_glio_malloc(struct glob_f *glob_info, size_t size);
extern char *_glio_strdup(struct glob_f *glob_info, const char * string);
extern void *_glio_padmem(void *ptr);


glio_arena_t *_glio_arena_create(glio_group_t *gg, size_t asize);
void _glio_arena_destroy(glio_arena_t *arp);

void _glio_shmem_getmem(glio_arena_t *arp, void *trg, void *src, size_t bytes,
	int rank);
void _glio_shmem_putmem(glio_arena_t *arp, void *trg, void *src, size_t bytes,
	int rank);

void _glio_shmem_get32(glio_arena_t *arp, void *trg, void *src, size_t len,
        int rank);
void _glio_shmem_put32(glio_arena_t *arp, void *trg, void *src, size_t len,
        int rank);

void _glio_shmem_long_get(glio_arena_t *arp, long *trg, long *src, size_t len,
        int rank);
void _glio_shmem_long_put(glio_arena_t *arp, long *trg, long *src, size_t len,
        int rank);

void _glio_shmem_int_p(glio_arena_t *arp, int *trg, int val, int rank);
void _glio_shmem_long_p(glio_arena_t *arp, long *trg, long val, int rank);
void _glio_group_init(glio_group_t *gg);


void _glio_set_lock(glio_arena_t *arp, long *lock);
void _glio_clear_lock(glio_arena_t *arp, long *lock);
void _glio_barrier(glio_arena_t *arp);
void _glio_shmem_wait(long *addr, long value);



#endif	/* _GLOBLAYER_SUPPORTED */

