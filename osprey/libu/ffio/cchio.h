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


/* USMID @(#) libu/ffio/cchio.h	92.1	10/07/99 22:13:28 */

/*
 *	Default settings
 */
#include <string.h>

#define WPBLOCK		512			/* words per 512 wd disk block*/
#define BYTPBLOCK	4096			/* bytes per 512 wd disk block*/
#define BITPBLOCK	32768			/* bits per 512 wd disk block*/
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CCH_DEF_BUFSIZ	8		/* Dflt bufsize in sectors */
#define CCH_DEF_NBUF	4		/* Default number of buffers */
#else
#define CCH_DEF_BUFSIZ	_VALUE(_def_cch_bufsiz)	/* Dflt bufsize in sectors */
#define CCH_DEF_NBUF	_VALUE(_def_cch_nbuf)	/* Default number of buffers */
#endif
#ifdef _CRAYMPP
#define CCH_DEF_SIMBUFSIZ _VALUE(_def_cch_simbufsiz)/* Dflt bufsize in */
						/* sectors when using mppsim */
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CCH_MAX_BBUFSIZ 010000000000		/* 2**30, max value for buffer*/
						/* size (in bits) */
#else
#define CCH_MAX_BBUFSIZ 01000000000000000	/* 2**45, max value for C int */
						/* type This is the largest */
						/* buffer size (in bits) */
						/* which allows 46 bit */
						/* divides. */
#endif
#define _CCH_SMSIZ	4096			/* size, in bytes, of small */
						/* buffer we use for temp */
						/* storage */

/*
 *	State flags to define the status of a buffer (flags field in struct
 *	cch_buf).
 */
#define CCH_VALID	01	/* buffer contains valid data if CCH_READING  */
				/* is not set.  If CCH_READING is set, the    */
				/* buffer will contain valid data when the    */
				/* read completes. */
#define CCH_DIRTY	02	/* buffer contains unflushed data */
#define CCH_WRITING	04	/* an async write is in process for the buffer*/
#define CCH_READING	010	/* an async read is in process for the buffer */
#define CCH_ZEROED	020	/* buffer following the eof has been zeroed */
#define CCH_VALIDBUFFER	040	/* entire buffer is valid - this means */
				/* either it has been preread or it is */
				/* entirely full of dirty data */

/*
 *	Cache layer types (cache.xxx)
 */
#define TR_CCH_MEM	1	
#define TR_CCH_SDS	2

/*
 *	Option flags (for optflags field in struct cch_f).
 */
#define CCHOPT_SDS	01	/* SDS-resident buffer cache */
#define CCHOPT_RDAHEAD	02	/* read-ahead */

#if defined(_CRAY1) || (defined(_CRAYT3D) && defined(_UNICOS_MAX))
#define CCH_SDS_SUPPORTED
#endif
/*
 *	Buffer control block structure.
 */
struct cch_buf {
	off_t		filead;	/* bit offset in file where page begins. -1  */
				/* indicates that the buffer is free. */
	long		flags;	/* buffer state flags */
	long		atime;	/* _rtc() value at last access to this buffer */
	bitptr		buf;	/* Data array address, must be on a byte 
				 * boundary. For SDS cache, this is the bit 
				 * address in the SDS field length, which 
				 * must be on a 512 word boundary */
	struct ffsw	sw;	/* stat struct to monitor async i/o completion*/
	int		lnkcnt;	/* number of chained buffers in this chain.
				 * This field is set only in the first buffer */
	int		lnk;	/* the number of buffers which precede this one
				 * in the chain. */
	int		firstdata; /* bit offset of first dirty byte in buffer*/
	int		lastdata; /* bit offset of first byte beyond last dirty byte in buffer */
};

struct cch_f {

	int	nbufs;		/* number of buffers */
	int	bsize;		/* size of each buffer in bits. The cache     */
				/* layer code assumes that bsize is a 	      */
				/* multiple of 8. */
	int64	bypasssize;	/* size at which we bypass buffering */
				/* in bits */
	off_t	fsize;		/* bit size of entire file set by _cch_weod */
	off_t	feof;		/* bit size of file from underlying layer */
	off_t	cpos;		/* current bit offset within the file */
	long	optflags;	/* flag of options */
	unsigned is_blkspec:1;	/* 1 iff file is a block special file */
	unsigned is_multup:1;	/* 1 iff the "-m on" assign attribute is set */
	unsigned bufs_alloc :1;	/* 1 iff the page bufs were allocated */
	unsigned do_sylistio :1; /* 1 iff can do simple listio */
	unsigned odirect :1; 	/* 1 iff opened with O_DIRECT (mips) */
	struct cch_buf	*bufs;	/* pointer to contiguous buffer control blocks*/
	struct cch_buf	*cubuf;	/* pointer to last buffer accessed */
	long	chronometer;
	unsigned	miniosize;	/* the minimum io size we can issue */
					/* when bypassing buffering */
	unsigned	maxiosize;	/* the maximum io size we can issue */
					/* when bypassing buffering */
	unsigned	chunksize;	/* when bypassing buffering, io must */
					/* be in multiples of chunksize */
	unsigned	memalign;	/* memory alignment when bypassing */
					/* buffering */
	unsigned	diskalign;	/* disk alignment when bypassing */
					/* buffering  in bytes */
	char		*savearea; /* temporary storage area */
};


/*
 *	Macros
 */

/*
 *	CCH_DEBUG prints debug output.
 */
#if defined(DEBUG)
#define CCH_DEBUG( ARGS )	{if (FDCTRACE(TRACE_CACHE)) _xrt_putf ARGS ;}
#elif defined(DEBUG_CACHE)
#define CCH_DEBUG( ARGS )	_xrt_putf ARGS 
#else
#define CCH_DEBUG( ARGS )
#endif

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
 *	MIN
 */
#ifdef MIN
#undef MIN
#endif
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#ifdef MAX
#undef MAX
#endif
#define MAX(a,b) ((a) > (b) ? (a) : (b))


/*
 *	CCHFLOOR efficiently computes (num - (num % divisor)). 
 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CCHFLOOR(num,divisor) (				\
	(((divisor) & (divisor-1)) == 0)  ?			\
		(num) & ~((divisor) - 1)		\
	:						\
		((num) / (divisor)) * (divisor)		\
)
#else
#define CCHFLOOR(num,divisor) (				\
	(_popcnt(divisor) == 1)  ?			\
		(num) & ~((divisor) - 1)		\
	:						\
		((num) / (divisor)) * (divisor)		\
)
#endif


/*
 * CCH_FINDBLK
 *
 * Searches for a cache buffer page assigned to a portion (page) of the file.  
 *
 * Parameters:
 *
 *	struct cch_f	*cch_info;    -	cch_f structure for the file 
 *	int		fileaddr;     -	bit offset within the file of file page
 *				 	This number must be a multiple of the 
 *					buffer size.
 *	struct cch_buf	*cubuf;	      -	OUTPUT PARAMETER.  Is assigned the 
 *					pointer to the page, or NULL if the 
 *					file page is not buffer resident.
 */
#define CCH_FINDBLK(cch_info, fileaddr, cubuf) { \
	int		fb_i, fb_nbu; \
	struct cch_buf	*fb_cbufs; \
 \
	CCH_DEBUG(("CCH_FINDBLK EN: to bit offset %d\n",fileaddr)); \
	cubuf = cch_info->cubuf; \
	if (cubuf == NULL || cubuf->filead != fileaddr) { \
		/* \
		 *	Search the buffers to see if any are assigned the \
		 *	requested file page.  \
		 */ \
		fb_nbu	 = cch_info->nbufs; \
		fb_cbufs = cch_info->bufs; \
		cubuf	 = NULL;	/* NULL unless search succeeds */ \
 \
		/* this search loop should vectorize */ \
		for (fb_i=0; fb_i<fb_nbu; fb_i++) \
			if (fb_cbufs[fb_i].filead == fileaddr) \
				break; \
 \
		if (fb_i != fb_nbu) { \
			CCH_DEBUG(("CCH_FINDBLK EX: buffer # %d\n",fb_i)); \
			cubuf = &fb_cbufs[fb_i]; \
			cch_info->cubuf = cubuf; /* remember this buffer page*/\
		} \
	} \
}

/*
 *	CCHWAITIO waits for I/O completion on a buffer and then clears the
 *	CCH_READING or CCH_WRITING flags.
 *
 *		struct fdinfo *lfio     - fdinfo pointer for underlying layer
 *		struct cch_buf *b       - pointer to buffer control block
 *		struct ffsw *statptr    - pointer to ffsw status word to
 *                                        receive status
 *		int ss                  - integer variable receives return
 *                                        status, or ERR if an error was
 *                                        detected in the I/O waited for.

 */
#ifdef DEBUG
#define CCHWAITIO(lfio,b,statptr,ss) {\
	if (FDCTRACE(TRACE_CACHE)) \
		_xrt_putf("CCHWAITIO: for addr %o bufstr addr is %o flag=%d\n",\
			(long)&(b)->sw,(long)(b),(b)->sw.sw_flag); \
	_CCHWAITIO(lfio,b,statptr,ss); \
}
#else
#define CCHWAITIO(lfio,b,statptr,ss)	_CCHWAITIO(lfio,b,statptr,ss);
#endif

#define _CCHWAITIO(lfio,b,statptr,ss) { \
	struct ffsw *wi_swp; \
	struct cch_buf *wi_cb; \
	int wi_i; \
 \
	wi_cb = b; \
	wi_cb -= wi_cb->lnk; \
	wi_swp = &wi_cb->sw; \
	while(wi_swp->sw_flag == 0 || wi_swp->sw_stat == 0) { \
		XRCALL(lfio,fcntlrtn) lfio, FC_RECALL, &(wi_cb)->sw, statptr); \
	} \
	for (wi_i=0; wi_i<wi_cb->lnkcnt; wi_i++) \
		wi_cb[wi_i].flags &= ~(CCH_READING | CCH_WRITING); \
	ss = 0; \
	if (wi_swp->sw_stat == FFERR) { \
		ss = ERR; \
		_SETERROR(statptr,wi_swp->sw_error,0); \
	} \
}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CCH_CHRONOMETER(x, cch_info) x.atime = ++(cch_info->chronometer);
#else
#define CCH_CHRONOMETER(x, cch_info) x.atime = _rtc();
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
/* The MIPS definition requires that a and b are byte-aligned and c */
/* is a multiple of 8 */
#define _CCH_MOV_BITS(a,b,c) {memcpy(BPTR2CP(a),BPTR2CP(b),(c)/8);}
#else
#define _CCH_MOV_BITS(a,b,c) MOV_BITS(a,b,c)
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
/* We don't allow bit offsets of mips */
#define CCH_MEMCLEAR(a,_nbits) {memset(BPTR2CP(a),0,(_nbits)/8);}
#else
#define CCH_MEMCLEAR(a,_nbits) { \
	if (((BPBITOFF(a) & 07) | (_nbits & 07)) == 0) {		\
		memset(BPTR2CP(a), 0, (_nbits)/8);		\
	}							\
	else {							\
		if ((BPBITOFF(a) & 07) == 0) {			\
			memset(BPTR2CP(a), 0, (_nbits)/8); 	\
			PUT_BITS((INC_BPTR((a), (_nbits & ~07))), 0, (_nbits & 07));\
		}						\
		else {						\
			/* first zero the head */		\
			PUT_BITS((a), 0, (64-BPBITOFF(a)));	\
			/* now zero the part that is byte-aligned */	\
                        memset((BPTR2CP(INC_BPTR((a),64-BPBITOFF(a)))), 0, \
                        ((_nbits) - (64-BPBITOFF(a)))/8);       \
			/* now zero the tail */			\
                        PUT_BITS(INC_BPTR((a),((64-BPBITOFF(a)) + (((_nbits) -(64-BPBITOFF(a))) & ~07))), \
                         0, \
                        (((_nbits) - (64-BPBITOFF(a)))& 07)); \
		}						\
	}							\
}
#endif
/*
 *	Externals
 */
extern int _def_cch_bufsiz;
extern int _def_cch_nbuf;
#ifdef _CRAYMPP
extern int _def_cch_simbufsiz;
#endif

/*
 *	Function prototypes.
 */
extern int		_cch_close(struct fdinfo *fio, struct ffsw *stat);
extern void		_cch_clfree(struct fdinfo *fio);
extern int		_cch_fcntl(struct fdinfo *fio, int cmd, void *arg, 
				   struct ffsw *iostat);
extern struct cch_buf	*_cch_getblk(struct cch_f *cin, struct fdinfo *lfio, 
				     off_t fileaddr, int64 *nblkp, int rd,
				     int valid, struct ffsw *stat);
extern _ffopen_t	_cch_open(const char *name, int oflags, mode_t mode,
				  struct fdinfo *fio, union spec_u *spec,
				  struct ffsw *stat, long cbits, int cblks,
				  struct gl_o_inf *oinf);
extern ssize_t		_cch_read(struct fdinfo *fio, bitptr bufptr,
				  size_t nbytes, struct ffsw *stat, int fulp,
				  int *ubcp);
extern ssize_t		_cch_write(struct fdinfo *fio, bitptr bufptr,
				   size_t nbytes, struct ffsw *stat, int fulp,
				   int *ubcp);
extern _ffseek_t	_cch_seek(struct fdinfo *fio, off_t pos, int whence,
				  struct ffsw *stat);
extern int		_cch_rdabuf(struct cch_f *cch_info, struct fdinfo *fio,
				    struct cch_buf *bc,
				    int bytes, off_t bytoff, int64 nblk,
				    char syncasync, struct ffsw *stat);
extern int		_cch_wrabuf(struct cch_f * cch_info,
				    struct fdinfo *lfio, struct cch_buf *bc,
				    int bytes, off_t bytoff, int64 nblk,
				    off_t *eof, char syncasync, struct ffsw *stat);

extern int64 		_cch_bypass(struct cch_f *cch_info, int64 nbits,
				off_t cpos,
				bitptr bufptr, off_t fileaddr, char rwmode,
				struct fdinfo *llfio, struct ffsw *stat);

