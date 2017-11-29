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


/* USMID @(#) libu/ffio/cosio.h	92.3	10/07/99 22:14:06 */

#ifdef	DEBUG 
#define register        /* nope */ 
#endif

#define	TR_AUTO		1
#define	TR_SYNC		2
#define	TR_ASYNC	3

/*
 * The following two values represent the default buffer size in 512-word
 * blocks, and the default buffer size at which the async (double buffered)
 * code kicks in.  These are externals in order to enable load-time
 * configuration.
 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define DEF_BSIZE	48
#define ASYNC_THRESH	64
#else
#define DEF_BSIZE	_VALUE(_def_cos_bs)
#define ASYNC_THRESH	_VALUE(_def_cos_thrsh)
#endif
/*
 * DEF_SIMBSIZE is used when running on mppsim in user virtual mode
 */
#ifdef	_CRAYMPP
#define DEF_SIMBSIZE	_VALUE(_def_cossim_bs)
#endif

#define COS_MAX_BBUFSIZ 010000000000            /* 2**30, max value for buffer*/
                                                /* size  (in bits) */

#define NBTSPW	64	/* Number of BITS per Word */

/* Values in cos_f.cos_flag */

#define COS_IOREAD	000001
#define COS_IOWRT	000002
#define COS_IOBOD	000100	/* Beginning Of Data */
#define COS_IOEOR	000200	/* End-of-record encountered */
#define COS_IOEOF	000400	/* End Of File */
#define COS_IOEOD	002000	/* End Of Data */
#define COS_IOWEOF	004000	/* EOF written */
#define COS_IOWRITE	010000	/* Last io was write */
#define COS_IODIRTY	020000	/* Buffer is dirty and needs to be flushed */
#define COS_IOCLOSE	040000	/* In the cos_close function */

/*
 *	Control word definitions 
 */

#define CWSIZE		64	/* Control word size in BITS */
#define CWBCW		0	/* BCW indicator */
#define CWEOR		010	/* RCW indicator - end of record  */
#define CWEOF		016	/* RCW indicator - end of file    */
#define CWEOD		017	/* RCW indicator - end of dataset */

#define BLKSZ		32768	/* Block size in BITS */
#define BLKSZ2		15	/* Block size as a power of 2 in BITS */
#define BLKSZ2BY	12	/* Block size as a power of 2 in BYTES */

/* Masks to check block boundaries */

#define BLKSZM		077777	/* In BITS */
#define BLKSZMBY	07777	/* In BYTES */
#define BLKSZMW		0777	/* In WORDS */

/*
 * The following mask represents the legal values that can be taken in the
 * mode field of a control word.  Zero bits represent the legal CW modes
 * numbered from zero on the left (i.e., values 0, 010, 016, and 017 are OK).
 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define LEGALRCW	0xFF7CFFFFFFFFFFFFLL
#else
#define LEGALRCW	0xFF7CFFFFFFFFFFFF
#endif

#define FLOOR(a,b)	((a) & ~((b)-1))
#define CEIL(a,b)	FLOOR((a)+(b)-1, (b))

typedef int64	_cw_type;
typedef uint64	_unsigned_cw_type;

typedef struct
	{
	uint64
		m:4,	/* type of control word. 0 for BCW */
		unused:7,
		bdf:1,	/* Bad data flag. */
		zero:19,
		bn:24,	/* Block number. modulo 2**24 */
		fwi:9;	/* forward index; the number of 64-bit words */
			/* to the next control word */
	} bcw;

typedef struct
	{
	uint64
		m:4,	/* type of control word, 010 for eor, 016 for eof */
			/* 017 for eod */
		ubc:6,	/* unused bit count; number of unused low-order */
			/* bits in the last word of previous record */
		tran:1,	/* Transparent record field */
		bdf:1,	/* bad data flag */
		srs:1,	/* skip remainder of sector */
		unused:7,
		pfi:20,	/* previous file index; offset modulo 2**20 to the */
			/* block where the current file starts (as defined */
			/* by the last EOF). */
		pri:15,	/* previous record index; offset modulo 2**15 to the */
			/* block where the current record starts. */
		fwi:9;	/* Forward index; number of 64-bit words to the */
			/* next control word. */
	} rcw;
 
#define	BNMASK	077777777	/* bn field mask */

/*
 *	GET field macros 
 */

#define GETBDF(x)	(((rcw *)(x))->bdf)
#define GETBN(x)	(((bcw *)(x))->bn)
#define GETFWI(x)	((((rcw *)(x))->fwi)+1) /* in WORDS! */
#define GETM(x)		(((rcw *)(x))->m)
#define GETPFI(x)	(((rcw *)(x))->pfi)
#define GETPRI(x)	(((rcw *)(x))->pri)
#define GETUBC(x)       (((rcw *)(x))->ubc) /* in BITS */

/*
 *	SET field macros 
 */

#define SETBDF(x,y)	(((rcw *)(x))->bdf = y)
#define SETBN(x,y)	(((bcw *)(x))->bn = y)
#define SETFWI(x,y)	(((rcw *)(x))->fwi = (y)-1) /* WORDS */
#define SETM(x,y)	(((rcw *)(x))->m = y)
#define SETPFI(x,y)	(((rcw *)(x))->pfi = y)
#define SETPRI(x,y)	(((rcw *)(x))->pri = y)
#define SETUBC(x,y)     (((rcw *)(x))->ubc = y) /* BITS */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define BLDBCW(cw_ptr, abdf, abn, afwi)				\
			{					\
			bcw	*bc_ptr;			\
			SANITYCHK(abdf,GT,1)			\
			SANITYCHK(afwi,GT,511)			\
			*cw_ptr = 0;				\
			bc_ptr = (bcw *)cw_ptr;			\
			bc_ptr->bdf = abdf;			\
			bc_ptr->bn = abn;			\
			bc_ptr->fwi =afwi;			\
			}
#else
#define BLDBCW(cw_ptr, bdf, bn, fwi)				\
			{					\
			SANITYCHK(bdf,GT,1)			\
			SANITYCHK(fwi,GT,511)			\
			*cw_ptr = (CWBCW << 60) |		\
				((bdf) <<52) |			\
				(((bn) & BNMASK) << 9) | 	\
				(fwi);				\
			}
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define BLDRCW(cw_ptr, cwm, aubc, atran, abdf, asrs, apfi, apri, afwi)	\
			{					\
			rcw	*rc_ptr;			\
			SANITYCHK(abdf,GT,1);			\
			SANITYCHK(aubc,GT,63);			\
			SANITYCHK(aubc,LT,0);			\
			SANITYCHK(apfi,GT,0xfffff);		\
			SANITYCHK(apfi,LT,0);			\
			SANITYCHK(apri,GT,0x7fff);		\
			SANITYCHK(apri,LT,0);			\
			SANITYCHK(afwi,GT,511);			\
			SANITYCHK(afwi,LT,0);			\
			*cw_ptr = 0;				\
			rc_ptr = (rcw *)cw_ptr;			\
			rc_ptr->m = cwm;			\
			rc_ptr->ubc = aubc;			\
			rc_ptr->tran = atran;			\
			rc_ptr->bdf = abdf;			\
			rc_ptr->srs = asrs;			\
			rc_ptr->pfi = apfi;			\
			rc_ptr->pri = apri;			\
			rc_ptr->fwi = afwi;			\
			}
#else
#define BLDRCW(cw_ptr, cwm, ubc, tran, bdf, srs, pfi, pri, fwi)	\
			{					\
			SANITYCHK(bdf,GT,1);			\
			SANITYCHK(ubc,GT,63);			\
			SANITYCHK(ubc,LT,0);			\
			SANITYCHK(pfi,GT,0xfffff);		\
			SANITYCHK(pfi,LT,0);			\
			SANITYCHK(pri,GT,0x7fff);		\
			SANITYCHK(pri,LT,0);			\
			SANITYCHK(fwi,GT,511);			\
			SANITYCHK(fwi,LT,0);			\
			*cw_ptr =	((cwm) << 60) |		\
					((ubc) << 54) | 	\
					((tran) << 53) | 	\
					((bdf) << 52) | 	\
					((srs) << 51) | 	\
					((pfi) << 24) | 	\
					((pri) << 9) | 		\
					(fwi);			\
			}
#endif
/*
 *	CHECK flags macros
 */

#define UEOR(x)		x & COS_IOEOR
#define UEOF(x)		x & COS_IOEOF
#define UEOD(x)		x & COS_IOEOD

/*
 *	SET flags macros
 */

#define SETEOR(x)	x |= COS_IOEOR
#define SETEOF(x)	x |= COS_IOEOF 
#define SETWEOF(x)	x |= COS_IOWEOF
#define SETEOD(x)	x |= COS_IOEOD

/*
 *	CLEAR flags macros
 */

#define CLEOR(x)	x &= ~COS_IOEOR
#define CLEOF(x)	x &= ~COS_IOEOF
#define CLWEOF(x)	x &= ~COS_IOWEOF
#define CLEOD(x)	x &= ~COS_IOEOD

#define CKBCW(cwptr, cbn, stat)						\
	{								\
	if ( (GETM(cwptr) != CWBCW) || (GETBN(cwptr) != (cbn & BNMASK)))\
		ERETURN(stat, FDC_ERR_BADBCW, 0);			\
	}
 
#define CKRCW(cwptr, ptr, base, blklmt, stat)				\
		{							\
		if (cwptr < (_cw_type *)BPTR2WP(blklmt))		\
			if (((_cw_type)(LEGALRCW << GETM(cwptr))) < 0)	\
				ERETURN(stat, FDC_ERR_BADRCW, 0);	\
		}
 
/*
 * BLKLMT returns the pointer to the next block
 * Note: BLKLMT(n,n) == n  i.e. the beginning of the block is
 *	considered to be part of the block.
 */
#define BLKLMT(ptr, base)	INC_BPTR(base, CEIL((SUBT_BPTR(ptr, base)), BLKSZ))

/*
 * BLKBASE returns a ptr to the base of the current block
 * Note: BLKBASE(n,n) == 0  i.e. the end of the block is
 *	considered to be part of the block.
 */
#define BLKBASE(ptr, base)	(INC_BPTR(base,				\
					FLOOR((SUBT_BPTR(ptr, base) - 1), \
						BLKSZ			\
						)			\
					)				\
				)

/*
 * Wait for outstanding async I/O to complete.
 *	pastat is pointer to async status struct
 */
#define ASWAIT(fio, pastat)					\
		{						\
		struct ffsw	locstat;			\
								\
		while ((pastat)->sw_flag == 0 || FFSTAT(*pastat) == 0) \
			XRCALL(fio->fioptr, fcntlrtn)		\
				fio->fioptr,			\
				FC_RECALL,			\
				(pastat),			\
				&locstat); /* toss status */	\
		}
#define ASWAITUP(fio, stat, pastat, cos_info, ret)		\
		{						\
		ASWAIT(fio, pastat);				\
		*stat = *pastat;				\
		if (stat->sw_error != 0)			\
			ERETURN(stat, stat->sw_error, 0);	\
		ret = stat->sw_count;				\
/*								\
 *		If 'opend' then this I/O was pending, and is	\
 *		therefore the last I/O done.			\
 *		Update diskpos.					\
 */								\
		if (cos_info->opend == YES)			\
			{					\
			cos_info->cos_diskpos = cos_info->opos + ret; \
			cos_info->osiz = ret << 3;			\
			cos_info->opend = NO;			\
			}					\
		}
/*
 * Switch buffers.
 */
#ifdef	DEBUG
#define ASWTCH_DBG(obuf,opos,osiz,cbuf,cpos,csiz,dpos)		\
	{							\
	if (FDCTRACE(TRACE_COS))				\
		{						\
		_xrt_putf(" \t_cos_ASWTCH:\tobuf,opos,osiz=%x,%x,%x\n", \
			obuf,opos,osiz);			\
		_xrt_putf(" \t\tfrom:\tcbuf,cpos,csiz=%x,%x,%x", \
			cbuf,cpos,csiz);			\
		_xrt_putf(" \tdiskpos=%x\n", dpos);		\
		}						\
	}
#else
#define ASWTCH_DBG(obuf,opos,osiz,cbuf,cpos,csiz,dpos)	{}
#endif

#define ASWTCH(fio, cos_info, base, pos, size)			\
		{						\
		register bitptr	ZZbas;				\
		register long	ZZsiz;				\
		register off_t	ZZpos;				\
								\
		ASWTCH_DBG(cos_info->obuf,			\
			cos_info->opos,				\
			cos_info->osiz,				\
			fio->_base,				\
			cos_info->cos_bufpos,			\
			cos_info->cos_size,			\
			cos_info->cos_diskpos);			\
								\
		ZZbas	= base;					\
		ZZpos	= pos;					\
		ZZsiz	= size;					\
		base	= cos_info->obuf;			\
		pos	= cos_info->opos;			\
		size	= cos_info->osiz;			\
								\
		cos_info->obuf	= ZZbas;			\
		cos_info->opos	= ZZpos;			\
		cos_info->osiz	= ZZsiz;			\
		}

#define FILL(fio, base, cwptr, ptr, blklmt, cnt, size, cbn, stat)	\
	{								\
	register off_t	reqpos;						\
									\
	if (cnt == 0)							\
		{							\
		register int		ret2;				\
		register ssize_t	ret;				\
		int			ubc = 0;			\
									\
		reqpos = (cbn + 1) << BLKSZ2BY;				\
		if (cos_info->opos == reqpos)				\
			{						\
			ASWAITUP(fio, stat, &cos_info->bstat,		\
					cos_info, ret);			\
			ASWTCH(fio, cos_info, base,			\
				cos_info->cos_bufpos, size);		\
			fio->_base = base;				\
			}						\
		else							\
			{						\
			if (reqpos != cos_info->cos_diskpos)		\
				{					\
				ret2 = _cos_poschng(cos_info,reqpos,fio,stat);\
				if (ret2 < 0)				\
					return(ERR);			\
				}					\
			ret = XRCALL(fio->fioptr, readrtn)		\
				fio->fioptr,				\
				base,					\
				fio->_ffbufsiz >> 3,			\
				stat,					\
				PARTIAL,				\
				&ubc);					\
/*									\
 *			In this context, we *must* get data,		\
 *			zero is an error.  Zero return is		\
 *			handled outside macro				\
 */									\
			if (ret < 0) return(ERR); /* stat is set */	\
			if (ubc != 0) ERETURN(stat, FDC_ERR_UBC, 0);	\
 									\
			cos_info->cos_bufpos = cos_info->cos_diskpos;	\
			cos_info->cos_diskpos += ret;			\
			}						\
		reqpos += ret;						\
/*									\
 *		If ASYNC, fire off readahead of next buffer		\
 */									\
		if (fio->rtype == TR_ASYNC && (ret << 3) == fio->_ffbufsiz) \
		   {							\
		   if (cos_info->cos_lasteod == 0 ||			\
		     reqpos < (cos_info->cos_lasteod>>3))		\
			{						\
			if (cos_info->opos != reqpos)			\
				{					\
				if (reqpos != cos_info->cos_diskpos)	\
					{				\
					ret2 = _cos_poschng(cos_info,reqpos, \
						fio,stat);\
					if (ret2 < 0)			\
						return(ERR);		\
					}				\
				cos_info->bstat.sw_flag = 0;		\
				cos_info->bstat.sw_error = 0;		\
				cos_info->bstat.sw_count = 0;		\
/* Shouldn't we check what this returns?? */				\
				(void) XRCALL(fio->fioptr, readartn)	\
					fio->fioptr,			\
					cos_info->obuf,			\
					fio->_ffbufsiz >> 3,		\
					&cos_info->bstat,		\
					PARTIAL,			\
					&ubc);				\
				cos_info->opend = YES;			\
				cos_info->opos = cos_info->cos_diskpos;	\
				}					\
			}						\
		   }							\
		ptr	= base;						\
		cnt	= ret << 3;					\
		size	= cnt;						\
		}							\
	cwptr = (_cw_type *)BPTR2WP(ptr); /* if did read, will be .eq. base */	\
	blklmt = ptr;							\
	}

#define NEXTBLK(fio, base, size, stat)					\
		{							\
		register int	ret;					\
		register ssize_t wret;					\
		register off_t	ret2;					\
		register off_t	reqpos;					\
		off_t		skpos;					\
									\
		reqpos = cos_info->cos_bufpos;				\
		SETFWI(cwptr,(_cw_type *)BPTR2WP(ptr) - cwptr);		\
		if (cnt == 0)						\
			{                	 			\
			if (fio->rtype == TR_ASYNC)			\
				{					\
				ASWAITUP(fio, stat, &cos_info->bstat,	\
					cos_info, ret);			\
/*									\
 *				Switch the main and alt buffers		\
 */									\
				ASWTCH(fio, cos_info, base,	\
					cos_info->cos_bufpos, 		\
					cos_info->cos_size);		\
				fio->_base = base;			\
				if (cos_info->cos_diskpos != reqpos)	\
					{				\
					cos_info->opend = NO;		\
					if (cos_info->ffci_flags & FFC_SEEKA)\
					   {				\
					   ret2 = XRCALL(fio->fioptr, seekrtn) \
								fio->fioptr,\
								reqpos,	\
								0,	\
								stat	\
								);	\
					   }				\
					else				\
					   {				\
					   skpos = reqpos - cos_info->cos_diskpos;\
					   ret2 = XRCALL(fio->fioptr,posrtn)\
								fio->fioptr,\
								FP_RSEEK,\
								&(skpos),\
								1,	\
								stat	\
								);	\
					   }				\
					if (ret2 < 0)			\
						return(ERR);		\
					cos_info->cos_diskpos = reqpos; \
					}				\
									\
				cos_info->bstat.sw_flag = 0;		\
				cos_info->bstat.sw_error = 0;		\
				cos_info->bstat.sw_count = 0;		\
				wret = XRCALL(fio->fioptr, writeartn)	\
						fio->fioptr,		\
						cos_info->obuf,		\
						(ssize_t)(size >> 3),	\
						&cos_info->bstat,	\
						FULL,			\
						&zero);			\
				if (wret < 0) return(ERR);		\
				cos_info->opend = YES;			\
				wret = size >> 3;			\
				}					\
			else						\
				{					\
				wret = XRCALL(fio->fioptr, writertn)	\
						fio->fioptr,		\
						base,			\
						(ssize_t)(size >> 3),	\
						stat,			\
						FULL,			\
						&zero);			\
				if (wret < 0) return(ERR);		\
				}					\
			cos_info->cos_diskpos += wret;			\
			cos_info->cos_bufpos = cos_info->cos_diskpos;	\
			ptr = fio->_base;				\
			cnt = fio->_ffbufsiz;				\
			flag &= ~COS_IODIRTY;				\
			}						\
		cwptr = (_cw_type *)BPTR2WP(ptr);			\
		cbn++;							\
		}

#define SKIPBCW(cwptr, ptr, base, blklmt, cnt, cbn, stat)	\
		{						\
		SET_BPTR(blklmt, INC_BPTR(blklmt, BLKSZ));	\
		CKBCW(cwptr, cbn, stat);			\
		/* next control word */				\
		cwptr += GETFWI(cwptr);				\
		CKRCW(cwptr, ptr, base, blklmt, stat);		\
		SET_BPTR(ptr, INC_BPTR(ptr, CWSIZE));		\
		cnt -= CWSIZE;			 		\
		}

#define SKIPRCW(cwptr, ptr, base, cnt, blklmt, size, stat)	\
		{						\
		SET_BPTR(ptr, WPTR2BP(cwptr + (CWSIZE>>6)));	\
		cnt = size - SUBT_BPTR(ptr, base);		\
		/* calc next control word */			\
		cwptr += GETFWI(cwptr);				\
		CKRCW(cwptr, ptr, base, blklmt, stat);		\
		}
struct cos_f
	{
	int     cos_cnt;	/* count of bits in buf on write */
				/* count of bits left to go on read */
	int     cos_flag;
	int64   cos_cbn;
	int64   cos_pri;	/* previous record index */
	int64   cos_pfi;	/* previous file index */
	int     cos_size;
	off_t   cos_lasteod;	/* bit offset of block following */
				/* most recent EOD written */
	_cw_type *cos_cwptr;
	bitptr	cos_blklmt;	/* pointer to next block boundary */
	off_t	cos_bufpos;	/* file byte position of current buffer */

	off_t	cos_diskpos;	/* current byte position of lower level */

	bitptr	obuf;		/* *other* buffer, same size */
	off_t	opos;		/* *other* buffer disk base addr */
	long	osiz;		/* *other* buffer data size */
	struct	ffsw bstat;	/* status for async I/O */
	int	opend;		/* Flag for I/O pending.  For diskpos update */
	long 	ffci_flags;	 /* info from lower layers */
	};

extern int _def_cos_bs;
#ifdef	_CRAYMPP
extern int _def_cossim_bs;
#endif
extern int _def_cos_thrsh;
/*
 * Prototypes
 */
extern void _cos_clfree(struct fdinfo *fio);
extern _ffopen_t _cos_open(const char *name, int flags, mode_t mode,
    struct fdinfo *fio, union spec_u *spec, struct ffsw *stat, long cbits,
    int cblks, struct gl_o_inf *oinf);
extern int _cos_close(struct fdinfo *fio, struct ffsw *stat);
extern ssize_t _cos_read(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
	struct ffsw *stat, int fulp, int *ubc);
extern int _cos_poschng(struct cos_f *cos_info, off_t reqpos, 
	struct fdinfo *fio, struct ffsw *stat);
extern int _cos_weof(struct fdinfo *fio, struct ffsw *stat);
extern int _cos_weod(struct fdinfo *fio, struct ffsw *stat);
extern int _cos_iflush(struct fdinfo *fio, struct ffsw *stat);
extern ssize_t _cos_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
	struct ffsw *stat, int fulp, int *ubc);
extern int _cos_setpos(struct fdinfo *fio, struct cos_f *cos_info, int len, off_t bytpos, long *pos, struct ffsw *stat);
extern int _cos_bksp(struct fdinfo *fio, struct ffsw *stat);
extern int _cos_bkfil(struct fdinfo *fio, struct ffsw *stat);
