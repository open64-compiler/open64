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


/* USMID @(#) libu/ffio/ccaio.h	92.2	10/29/99 21:40:31 */


#include <errno.h>
#include <stdio.h>
#include <string.h>		/* for memset */
#include <cray/mtlock.h>	/* for lock_t */
#ifdef __mips
#include <aio.h>
#endif

#ifndef FC_DUMPSTATS
#define FC_DUMPSTATS            102     /* intermediate diagnostics dumping */
#endif

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
/* If _CCA_DIAGS is not defined, certain statistics are not updated. */
/* These statistics are printed in ccaclosdiags.c */
#define _CCA_DIAGS
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
/*
 * This requires that to_ptr and fr_ptr be aligned on a byte
 * boundary, and that nbits be a multiple of 8
 */
#define _CCA_MOV_BITS(to_ptr, fr_ptr, nbits) \
	{memcpy(BPTR2CP(to_ptr), BPTR2CP(fr_ptr), (nbits)/8);}
#else
#define _CCA_MOV_BITS(to_ptr, fr_ptr, nbits) \
	MOV_BITS((to_ptr), (fr_ptr), (nbits))
#endif

#define DO_LISTIO( fio, cmd, list, nreq, stat, ret)  \
	    ret = XRCALL((fio), listiortn) (cmd), (list), nreq, stat);

#define _CLRBIT(bit,word) *(word+((unsigned)(bit)>>6)) &= ~( (uint64)1 << ((bit)&63) )
#define _SETBIT(bit,word) *(word+((unsigned)(bit)>>6)) |=  ( (uint64)1 << ((bit)&63) )
#define _GETBIT(bit,word) (((uint64) *(word+((unsigned)(bit)>>6)) >> ((bit)&63) ) & 1)
	
#if defined(_CRAY1) || (defined(_CRAYMPP) && defined(_UNICOS_MAX))
#define SDS_SUPPORTED	1
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define SCACHE_LOCK(_x)		MEM_LOCK(_x)
#define SCACHE_UNLOCK(_x)	MEM_UNLOCK(_x)
#elif defined(_CRAYMPP)
#define SCACHE_LOCK(_x)			{}
#define SCACHE_UNLOCK(_x)		{}
#else
#define SCACHE_LOCK(_x)			{if (MULTI_ON) _nlockon(_x);}
#define SCACHE_UNLOCK(_x)		{if (MULTI_ON) _nlockoff(_x);}
#endif

/*
 *	Default settings
 */


#define WPBLOCK		512			/* words per 512 wd disk block*/
#define BYTPBLOCK	4096			/* bytes per 512 wd disk block*/
#define BITPBLOCK	32768			/* bits per 512 wd disk block*/
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CCA_DEF_BUFSIZ	8
#else
#define CCA_DEF_BUFSIZ	_VALUE(_def_cch_bufsiz) /* Dflt bufsize in blocks */
#endif
#ifdef _CRAYMPP
#define CCA_DEF_SIMBUFSIZ _VALUE(_def_cch_simbufsiz) 	/* Dflt bufsize in    */
							/* blocks for MPP     */
							/* simulator.         */
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CCA_DEF_NBUF	4			/* Default number of buffers */
#else
#define CCA_DEF_NBUF	_VALUE(_def_cch_nbuf)	/* Default number of buffers */
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CCA_MAX_BBUFSIZ 010000000000		/* 2**30, max value for buffer*/
						/* size  (in bits) */
#else
#define CCA_MAX_BBUFSIZ 01000000000000000	/* 2**45, max value for C int */
						/* type This is the largest */
						/* buffer size (in bits) */
						/* which allows 46 bit */
						/* divides. */
#endif

#define CCA_flags_UNUSED               1  /*  1 */
#define CCA_flags_SDS                  2  /*  2 */
#define CCA_flags_DUMP                 4  /*  3 */

#define CCA_flags_PARTIAL_WRITES      16  /*  5 */
#define CCA_flags_PREREAD_WRITES      32  /*  6 */
#define CCA_flags_READ_AHEAD          64  /*  7 */
#define CCA_flags_WRITE_BEHIND       128  /*  8 */
#define CCA_flags_SCRATCH            256  /*  9 */
#define CCA_flags_BYPASS_ON_NO_SPACE 512  /* 10 */

#define CCA_flags_DIAGS          0x0c00   /*  11,12 */
#define CCA_flags_DIAGS_PART     0x0400   /*  11 */
#define CCA_flags_DIAGS_FULL     0x0c00   /*  11,12 */

#define CCA_flags_units          0xf000  /* 13-16 */
#define CCA_flags_bytes          0x1000  /* 13 */
#define CCA_flags_words          0x2000  /* 14 */
#define CCA_flags_blocks         0x4000  /* 15 */

#define CCA_flags_mult           0xf0000 /* 17-20 */
#define CCA_flags_none           0x10000 /* 17 */
#define CCA_flags_k              0x20000 /* 18 */
#define CCA_flags_m              0x40000 /* 19 */
#define CCA_flags_g              0x80000 /* 20 */

#define CCA_flags_HOLD_WRITE_BEHIND 0x100000  /*  21 */


#define CCA_SOFT_BYPASS 0   /* cca_info->bypass */

struct cca_options {
	unsigned unused:1;
	unsigned sds:1;
	unsigned dump:1;
	unsigned diags:1;
	unsigned full_diags:1;
	unsigned pw:1 ;		/* writes of partial pages allowed */
	unsigned prw:1;		/* preread on writes of partial pages */
	unsigned ra:1 ;		/* read ahead */
	unsigned wb:1 ;		/* write behind */
	unsigned hldwb:1;
	unsigned scr:1;
	unsigned bpons:1;
	unsigned do_listio:1;
	unsigned do_sylistio:1;
	unsigned no_read:1;
	unsigned no_write:1;
};




#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

typedef union {
	struct {
	    int64   file_number:8;
	    int64   page_number:56;
	}	parts;
	int64	all;
} FILE_PAGE;

#define NULL_FILE_PAGE   0xffffffffffffffffLL

#define CCA_TRACKER_FREE	0
#define CCA_TRACKER_READA	'r'
#define CCA_TRACKER_WRITEA	'w'
#define CCA_TRACKER_COMPLETED	'c'

extern struct cca_async_tracker *_cca_add_trackers(int _num);

struct cca_async_tracker {
	int       mode;
	struct    ffsw *stat;
	bitptr    buf_ptr;
	int       pgoff;
	int       bit_count;
	struct cca_buf   *cubuf;
	FILE_PAGE file_page;
	struct cca_async_tracker *next_tracker;
};

#define FFSW_READING 1
#define FFSW_WRITING 2

struct ffsw_ext
	{
	unsigned int    sw_flag:1;
	unsigned int    sw_error:31;
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	size_t		sw_count;
#else
	unsigned int    sw_count:32;
#endif
	unsigned int    sw_stat:16;     /* FFCNT, etc. */
#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
	int		sw_user:32;	/* For user layer storage */
#else
	int             sw_user:48;     /* For user layer storage */
#endif
#if defined(_ADDR64) || defined(__mips) || defined(_LITTLE_ENDIAN)
	void		*sw_iptr;	/* pointer to layer data */
	void		*sw_sptr;	/* pointer to status chain, if used. */
#else
	int             sw_iptr:32;     /* pointer to layer data */
	int             sw_sptr:32;     /* pointer to status chain, if used. */
#endif
	int             sw_rsv1;        /* may need later.  Save space now */
#ifdef __mips
        aiocb_t         aiocbp;
#endif
	struct ffsw_ext *next;          /* pointer to my status chain */
	struct fdinfo	*llfio;		/* ptr to lower level layer for RECALL*/
	size_t          *inuse;         /* pointer to the byte count word to 
					 * update after recall */
	int             rw_mode;        /* is this ffsw reading or writing */
	FILE_PAGE       file_page;      /* is this ffsw reading or writing */
	size_t          bytes_req;      /* bytes requested for this request */
};


/*
 *	State flags to define the status of a buffer (flags field in struct
 *	cca_buf).
 */
#define CCA_DIRTY	02	/* buffer contains unflushed data */

#define NUM_BITS_PER_SECTOR 32768
#define NUM_BITS_PER_BLOCK  32768

/*
 *	Cache layer types (cache.xxx)
 */
#define TR_CCA_MEM	1	
#define TR_CCA_SDS	2

/*
 *	Option flags (for optflags field in struct cca_f).
 */
#define CCAOPT_SDS	01	/* SDS-resident buffer cache */
#define CCAOPT_RDAHEAD	02	/* read-ahead */

/*
 *	Buffer control block structure.
 */
struct cca_buf {
	FILE_PAGE       file_page;	/* the file page that this cache page 
					 * is currently holding */
	long		flags;		/* buffer state flags */
	long		atime;		/* CHRONOMETER value at last access 
					 * to this buffer */
	bitptr		buf;		/* Data array address, must be on a byte
					 * boundary. For SDS cache, this is the
					 * bit address in the SDS field length,
					 * which must be on a 512 word boundary
					 */
	struct ffsw_ext	sw;		/* stat struct to monitor async i/o
					 * completion*/
	int             index;		/* not used */
	int             eligible;	/* eligibility mask bits */
	int             protected; 	/* 1 => page may not be preempted */
	int             adv_read;	/* this page was advance read */
	int             exempt_count;	/* number of times an adv_read page has
					 * been rescued from purging */
	int             read_start_rtc;	/* rtc when async read was started */
	struct cca_buf *prev_cubuf;	/* pointer to the cache page that
					 * initiated this pages adv read */
	FILE_PAGE       prev_file_page;	/* use to check if prev_cubuf is still
					 * who who think */
	int64           *valid_sectors;	/* pointer to an array of words for
					 * setting valid bit for sectors */
	int64           *unsynced_sectors; /* pointer to an array of words for 
					 * setting unsynced bit for sectors */
	int64           *sector_used;	/* pointer to an array of words for 
					 * setting sector used bits */
	int             pre_init;	/* 1 if the entire page is valid 
					 * (read from disk before written to) */
	FILE_PAGE       should_call;	/* the file page this page sould 
					 * preread if used himself */
	int             direction;	/* the skipping increment */
	int64           chain_position;	/* Where this buffer is in the chain */
	int             pending_asyncs;	/* Number of asyncs call pending on 
					 * the cache page */
};

#define READ_AHEAD  1
#define READ_BEHIND 2
#define READ_EITHER 3

struct cca_f {

	struct fdinfo *nextfio;		/* pointer to the next layer	      */
	struct fdinfo *thisfio;		/* pointer to this layer's fdinfo     */
	struct fdinfo *frontdoorfio;	/* pointer to front door layer for SDS*/
	int     child_class;		/* the class number of the child class*/
	int     file_number;		/* the file number for use in shared 
					 * caching			      */
	int     shared_cache;		/* the shared cache being used  
					 * 0 => private			      */
	int	nbufs;			/* number of buffers */

	int	bsize;			/* bits per cache page		      */
	int     byte_per_pg;		/* bytes per cache page		      */
	int     blks_per_pg;		/* 512 word blocks per cache page     */
	int     sect_per_pg;		/* hardware sectors per cache page    */
	int     bits_per_sect;		/* bits per hardware sector	      */

	off_t	fsize;			/* bit size of file set by _cca_weod */
	off_t	st_blksize;		/* sector size of file */
	off_t	feof;			/* bit size of underlying layer */
	off_t	cpos;			/* current bit offset within the file */

	struct cca_buf	*bufs;		/* pointer to array of buffer control
					 * blocks*/
	struct cca_buf	*cubuf;		/* pointer to last buffer accessed */
	int     *chronometer;		/* global "time" access counter */
	int     *spilled;		/* has the cache spilled */
	plock_t	*cache_lock;		/* lock word for shared cache */

	struct cca_options	optflags;	/* flag of options */

	int     read_hit;      /* counter for cache read hits */
	int     read_miss;     /* counter for cache read misses */

	int     write_hit;     /* counter for cache write hits */
	int     write_miss;    /* counter for cache write misses */

	int     adv_start_count;     /* counter for number of advance reads started */
	int     adv_used_count;      /* counter for number of advance reads used */

	int     chain_start_count;  /* number of chains started */
	int64   max_count;          /* maximum chain length */
	int     write_behind_count; /* counter for write behind */
	int     max_lead;          /* number of pages to read ahead for each read ahead start */

	int     read_hide_time;		/* sum of time that async reads could
					 * hide behind */
	char    file_name[256];		/* copy of file name */
	int     dirty_sectwds;		/* number of words to flag if
					 * sector of page is dirty */
	int64   *dirty_sectors_check;	/* array of words with all sectors set 
					 * to be dirty */
	int     num_sectors_used;	/* number of sectors used in the cache*/
	int     sect_reused;		/* number of sectors reused in the
					 * cache */

	int     partial_pages_written;  /* number of partial cache pages
					 * written */
	int     cache_pages_not_preread;/* number of cache pages not preread
					 * for writes */
	int     unsynced_pages_read;    /* number of times an unsynced page had
					 * the necessary good blocks */

	int     cache_pages_synced_for_read; 
					/* number of dirty cache pages that 
					 * were read */
	int     cache_pages_synced_for_ill_formed_write;
					/* number of ill formed requests */

	int     exempts_issued;   
	int     exempts_failed;

	struct cca_async_tracker *async_tracker;
	int    extend_trackers_failed;
	int    unknown_recalls;

	int    output_units;
	size_t bytes_read_from_cca;
	size_t bytes_written_to_cca;
	size_t bytes_read_by_cca;
	size_t bytes_written_by_cca;
/*
 *	Bit subfields 
 */
	unsigned bypass		:1;	/* bypass the layer because of mem or 
					 * sds shortage */
	unsigned is_blkspec	:1;	/* 1 iff file is a block special file */
	unsigned is_multup	:1;	/* 1 iff the m on assign option set */
	unsigned is_shrdlck	:1;	/* 1 iff we are multitasking and     */
					/* using shared cache */
	unsigned is_welfrm	:1;	/* 1 iff -w on assign option set */


};

struct shared_cache_tab {
	int           file_count;	/* number of files using the cache */
	struct cca_f  **cca_info;	/* pointer to the shared cache's array
					 * of it's files cca_info's */
};

#define MAX_FILES_PER_SHARED_CACHE 127
#define NUM_SHARED_CACHE 16

#define CHRONOMETER(cubuf,string) \
     cubuf->atime = ++(*cca_info->chronometer);
/*
     printf("%s %3d file page %16.16x atime =%4d\n",__FILE__,__LINE__,cubuf->file_page,cubuf->atime);  \
     printf("       chronometer =%d  *chronometer =%d\n",cca_info->chronometer,*(cca_info->chronometer)); 
 */

/*
 *	Macros
 */

/*
 *	Macros which print debug output.
 */
#if defined(DEBUG)
#define CCA_DEBUG( ARGS )	{if (FDCTRACE(TRACE_CACHE)) printf ARGS;}
#elif defined(DEBUG_CACHE)
#define CCA_DEBUG( ARGS )	printf ARGS
#else
#define CCA_DEBUG( ARGS )
#endif
#define CACHE_DEBUG1(x,y) CCA_DEBUG((x,y))/* old but still used */

/*
 *	BITS2BYTES converts a bit count to a byte count.  If x is not a 
 *	multiple of 8, we round up to the next byte.
 */
#define BITS2BYTES(x)	((x + 7) >> 3)

/*
 *	BYTES2BITS converts a byte count to a bit count.  
 */
#define BYTES2BITS(x)	((x) << 3)

/*
 *	BITS2BLOCKS converts a bit count to a disk block count.  
 *	BYTES2BLOCKS converts a byte count to a disk block count.  
 */
#define BITS2BLOCKS(x)	((x) >> 15)
#define BYTES2BLOCKS(x)	((x) >> 12)

/*
 *	MIN and MAX
 */
#undef MIN
#undef MAX
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))


	
/*
 *	CCAFLOOR efficiently computes (num - (num % divisor)). 
 *      (divisor) & (divisor-1) == 0 when divisor is a power of 2
 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CCAFLOOR(num,divisor) (				\
	(((divisor) & (divisor-1)) == 0) ?		\
		(num) & ~((divisor) - 1)		\
	:						\
		((num) / (divisor)) * (divisor)		\
)
#else
#define CCAFLOOR(num,divisor) (				\
	(_popcnt(divisor) == 1) ?			\
		(num) & ~((divisor) - 1)		\
	:						\
		((num) / (divisor)) * (divisor)		\
)
#endif


/*
 * CCA_FINDBLK
 *
 * Searches for a cache buffer page assigned to a portion (page) of the file.  
 *
 * Parameters:
 *
 *	struct cca_f	*cca_info;    -	cca_f structure for the file 
 *	int64		need_page;    -	the file specific page needed
 *				 	This number must be a multiple of the 
 *					buffer size.
 *	struct cca_buf	*cubuf;	      -	OUTPUT PARAMETER.  Is assigned the 
 *					pointer to the page, or NULL if the 
 *					file page is not buffer resident.
 *  struct ffsw     wait_stat;      OUTPUT parameter. 
 *  int    ret;     OUTPUT parameter.  0 if OK, ERR if error.
 */
#define CCA_FINDBLK(cca_info, need_page, cubuf, wait_stat, ret) { \
	int		fb_i, fb_nbu; \
	struct cca_buf	*fb_cbufs; \
 \
	CCA_DEBUG(("CCA_FINDBLK EN: to file page %d\n",need_page)); \
	cubuf = cca_info->cubuf; \
\
	ret	 = 0; \
	fb_nbu	 = cca_info->nbufs; \
	fb_cbufs = cca_info->bufs; \
	for (fb_i = 0; fb_i < fb_nbu; fb_i++) {\
           if(fb_cbufs[fb_i].sw.file_page.all == need_page.all ) { \
              CCAWAITIO( fb_cbufs[fb_i].sw,&wait_stat,ret); \
              break; \
           } \
        } \
	if (cubuf == NULL || (cubuf->file_page.all != need_page.all) ) { \
		/* \
		 *	Search the buffers to see if any are assigned the \
		 *	requested file page.  \
		 */ \
		cubuf	 = NULL;	/* NULL unless search succeeds */ \
 \
		/* this search loop should vectorize */ \
		for (fb_i=0; fb_i<fb_nbu; fb_i++) \
			if (fb_cbufs[fb_i].file_page.all == need_page.all) \
				break; \
 \
		if ( fb_i != fb_nbu ) { \
			CCA_DEBUG(("CCA_FINDBLK EX: buffer # %d\n",fb_i)); \
			cubuf = &fb_cbufs[fb_i]; \
			cca_info->cubuf = cubuf; /* remember this buffer page*/\
		} \
	} \
}

#define CCAWAITIO(base_sw,statptr,ss) { \
	struct ffsw_ext *wi_swp, *next_swp; \
	struct ffsw dumstat;\
\
	wi_swp = &base_sw; \
	ss = 0; \
	while ( wi_swp && wi_swp->llfio ) { \
	    while ( wi_swp->sw_flag == 0 || wi_swp->sw_stat == 0) { \
		XRCALL(wi_swp->llfio,fcntlrtn) wi_swp->llfio, FC_RECALL, \
					       wi_swp, &dumstat); \
	    } \
	    if( (( wi_swp->sw_count != wi_swp->bytes_req ) && \
		 ( wi_swp->rw_mode == FFSW_WRITING )) ||  \
	        ( wi_swp->sw_stat == FFERR ) ) { \
		ss = ERR; \
		_SETERROR(statptr,wi_swp->sw_error,0); \
	    } \
	    else { \
	       *(wi_swp->inuse) += wi_swp->sw_count; \
	    } \
	    wi_swp->rw_mode       = 0; \
	    wi_swp->file_page.all = NULL_FILE_PAGE; \
	    wi_swp->llfio         = NULL; \
	    next_swp = (struct ffsw_ext *)(wi_swp->next); \
	    wi_swp->next = 0; \
	    wi_swp->inuse         = NULL; /* do this after other fields are changed */\
	    wi_swp = next_swp; \
	} \
}

/*
 *	Externals
 */
extern int _def_cca_bufsiz;
extern int _def_cca_nbuf;
extern FILE *_GL_cca_logptr;
extern struct shared_cache_tab _CCA_scache[];

/*
 *	Function prototypes.
 */

extern int _cca_close(struct fdinfo *fio, struct ffsw *stat);

extern void _cca_clfree(struct fdinfo *fio, void *fioptr, int release_cache_buffers );

extern int _cca_fcntl(
	struct fdinfo *fio,
	int cmd,
	void *arg, 
	struct ffsw *iostat);

extern struct cca_buf *_cca_getblk_p(
	struct cca_f *cin,
	struct fdinfo *lfio, 
	off_t fileaddr,
	int rd,
	struct ffsw *stat,
	struct cca_buf *wait_cubuf,
	char read_write_mode,
	char sync_mode);

extern struct cca_buf *_cca_getblk(
	struct cca_f *cin,
	struct fdinfo *lfio, 
	off_t fileaddr,
	int rd,
	struct ffsw *stat,
	char sync_mode,
	char read_write_mode);

extern _ffopen_t _cca_open(
	const char *name,
	int oflags,
	mode_t mode,
	struct fdinfo *fio,
	union spec_u *spec,
	struct ffsw *stat,
	long cbits,
	int cblks,
	struct gl_o_inf *oinf);

extern ssize_t _cca_read(
	struct fdinfo *fio,
	bitptr bufptr,
	size_t nbytes,
	struct ffsw *stat,
	int fulp,
	int *ubcp);

extern ssize_t _cca_write(
	struct fdinfo *fio,
	bitptr bufptr,
	size_t nbytes,
	struct ffsw *stat,
	int fulp,
	int *ubcp);

extern _ffseek_t _cca_seek(
	struct fdinfo *fio,
	off_t pos,
	int whence,
	struct ffsw *stat);

extern int _cca_rdabuf(
	struct cca_f *cca_info,
	struct fdinfo *fio, 
	struct cca_buf *bc,
	int bytes, 
	off_t bytoff, 
	char syncasync, 
	struct ffsw *stat);

extern int _cca_wrabuf(struct cca_f *cca_info,
	struct fdinfo *lfio, 
	struct cca_buf *bc,
	int bytes, 
	off_t bytoff,
	char syncasync, 
	struct ffsw *stat);

extern int _cca_clear_page(
	struct cca_f *cca_info, 
	struct cca_buf *fb, 
	struct ffsw *stat);

extern int _cca_sync_page(
	struct cca_f *cca_info,
	struct cca_buf *cubuf ,
	struct ffsw *stat);

extern struct cca_async_tracker *_cca_start_tracker(
	struct fdinfo *llfio, 
	struct cca_f *cca_info, 
	struct cca_buf *cubuf, 
	struct ffsw *stat, 
	int mode, 
	bitptr buf_ptr, 
	int pgoff, 
	int bit_count );

extern void _cca_complete_tracker(
	struct cca_f *cca_info,
	struct cca_async_tracker *this_tracker,
	int *ret_value,
	struct cca_buf *cubuf_in,
	int mode_in,
	int pgoff_in,
	bitptr buf_ptr_in,
	int bit_count_in,
	struct ffsw *stat_in );
extern int _cca_flush(
	struct fdinfo   *fio,
	struct ffsw     *stat);

/*
 *	Inline functions.
 */

/*
 * _clear_buffer
 *
 * Zeros out a memory or SDS resident buffer.   
 * 
 * Return value:
 *
 *	0 on success.  -1 on failure with stat->sw_error set to the error code.
 *
 */
_PRAGMA_INLINE(_clear_buffer)
static int
_clear_buffer(
bitptr		bp,		/* bit pointer to memory or SDS buffer */
int		is_sds,		/* 1 if SDS resident, 0 if memory resident */
int		nbytes,		/* bytes to zero */
struct ffsw	*stat)		/* status */
{
#if SDS_SUPPORTED
	if (is_sds) {
		int ret;
		ret = _sdsset_any( (BPTR2CP(bp) - (char*)NULL), 0, nbytes);
		if (ret == ERR)
			ERETURN(stat, errno, 0);
	}	
	else
#endif
	{
		(void)memset(BPTR2CP(bp), 0, nbytes);
	}
	return(0);
}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#pragma inline _clear_buffer
#endif
