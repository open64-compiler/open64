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


/* USMID @(#) libu/ffio/fssio.h	92.0	10/08/98 14:57:41 */

#ifndef _FSSIO_H
#define _FSSIO_H
#include <cray/mtlock.h>

#define SDSBLKBTS	4096*8	/* SDS block size in bits */
#define SDSBLKBY	4096	/* SDS block size in bytes */

/* CLASS SDS & MR */
#define TR_FSS_SAVE	1	/* Load from disk, and save back on close */
#define TR_FSS_SCR	2	/* scratch, don't touch disk */
#define TR_FSS_LCL	3	/* local, same as scratch, but remember */
				/* its allocation and keep it around */
				/* for subsequent opens */

#define FSS_OPT_NOVFL	1	/* abort on overflow */
#define FSS_OPT_OVFL	2	/* Void SDS/MR and continue on overflow */
#define _FSSASYNUM      35	/* number of structures allocated for async */
				/* requests. */
/*
 * default min increment for SDS and buf size used by OPEN code
 */
#define SDSMININC	256	/* minimum alloc increment in blocks */
#define	ALOPINC		32	/* SDS allocation table increment */

#define MRMININC	32	/* minimum alloc increment in blocks */

#define SECBSZ	16		/* local buffer (locbuf) size in blocks */

#define BYTES2BLKS(bytes)	((bytes) >> 12)
#define BITS2BLKS(bits)		((bits) >> 15)

#define BLKS2WORDS(blks)	((blks) << 9)
#define BLKS2BYTES(blks)	((blks) << 12)
#define BLKS2BITS(blks)		((blks) << 15)

#define SDSMINKLIK		4	/* minimum allocation unit for SDS */
#define ROUNDD60(num, sds_info)	(((num) + (sds_info->sdsklik-1)) & (~(sds_info->sdsklik-1))) /* up */
#define DROUNDD60(num, sds_info) ((num) & (~(sds_info->sdsklik-1)))		/* down */

/*
 * BYTEOFF(bits)
 *	Return the byte offset into the block pointed to by the bitptr
 * WORDOFF(bits)
 *	Return the word offset into the block pointed to by the bitptr
 * BLOCKOFF(bits)
 *	Return the bit offset into the block pointed to by the bitptr
 * BLOCKNUM(bits)
 *	Return the block number containing the given bit address
 */
#define BYTEOFF(bits)	(bits & 0x7)
#define WORDOFF(bits)	(bits & 0x3f)
#define BLOCKOFF(bits)	(bits & 0x7fff)
#define BLOCKNUM(bits)	((bits) >> 15)


/*
 * constants used in loading files from lower layers/disk.
 */
#define L_ISMORE  0	/* more to be loaded */
#define L_GOTALL  1	/* Got all of the file, quit */
#define L_OVFLWD  2	/* Quit reading due to some overflow condition. */

#define OVFL_ATTRS		(FFC_NOTRN | FFC_STRM | FFC_RDM | FFC_SEEKA)
#define CAN_OVERFLOW(ffci)	((ffci.ffc_flags & OVFL_ATTRS) == OVFL_ATTRS)
/*
 * SSREAD()
 * SSWRITE()
 */
#ifdef DEBUG
#define FSSTRACE(action)		if (FDCTRACE(TRACE_SDSIO)) action
#define SSREAD(mem, sdsblk, blks)	ssread_dbg(mem, sdsblk, blks)
#define SSWRITE(mem, sdsblk, blks)	sswrite_dbg(mem, sdsblk, blks)
#else
#define FSSTRACE(action)		{}
#define SSREAD(mem, sdsblk, blks)	ssread(mem, sdsblk, blks)
#define SSWRITE(mem, sdsblk, blks)	sswrite(mem, sdsblk, blks)
#endif
/*
 * SSREADM and SSWRITEM move data blocks between SDS and memory.
 *	If the number of allocations is one, then the shortcut is taken
 *	to avoid the added overhead in calling the handler routine.
 *	If more than one slice has been allocated, then call the routine
 *	that divides the request up among the slices.
 *
 *	Note that in the event of error, errno is used to specify it,
 *	but it is uniformly ignored in the code.  The errno returned
 *	by the system is not useful, so if any ssread/sswrite request
 *	fails, an FDC_ERR_SDSIO is returned.
 */
#define SSREADM(ret, mem, sds_info, sdsblk, blks)			\
			{						\
			struct sds_alo_s *alop;				\
									\
			alop = sds_info->sdsalo;			\
			if (sds_info->sdsalnum == 1)			\
				{					\
				ret = SSREAD(mem, alop[0].base + sdsblk,\
					blks);				\
				}					\
			else						\
				{					\
				ret = _sds_readblks(			\
					mem,				\
					sds_info,			\
					sdsblk,				\
					blks				\
					);				\
				}					\
			}
#define SSWRITEM(ret, mem, sds_info, sdsblk, blks)			\
			{						\
			struct sds_alo_s *alop;				\
									\
			alop = sds_info->sdsalo;			\
			if (sds_info->sdsalnum == 1)			\
				{					\
				ret = SSWRITE(mem, alop[0].base + sdsblk,\
					blks);				\
				}					\
			else						\
				{					\
				ret = _sds_writeblks(			\
					mem,				\
					sds_info,			\
					sdsblk,				\
					blks				\
					);				\
				}					\
			}

/*
 * Define some Back Door macros to aid in traces and debugging.
 */
#define BDIOSTR "%s: BD %s of 0x%x bytes, blk 0x%x, returns 0x%x\n"
#define BDSKSTR "%s: BD %s to 0x%x, whence=0x%x, returns 0x%x\n"

#define BDIO(str, ret, op, arg1, arg2, arg3)				\
		{							\
		ret = op(arg1, arg2, arg3);				\
		FSSTRACE(_xrt_putf(str, __FILE__, #op, arg3, arg2, ret)); \
		}

#define BDREAD(ret, fd, data, nb)					\
		BDIO(BDIOSTR, ret, read, fd, data, nb)			\

#define BDWRITE(ret, fd, data, nb)					\
		BDIO(BDIOSTR, ret, write, fd, data, nb)			\

#define BDSEEK(ret, fd, pos, whence)					\
		BDIO(BDSKSTR, ret, lseek, fd, pos, whence)		\

/*
 *	Structure definitions for flags and fields that are used as private
 *	information to the SDS and MR layers
 */
struct delayed_oi_s
	{
	int mode;
	int cbits;
	union spec_u *spec;
	char name[1];
	};

struct sds_alo_s
	{
	int	relbase;	/* allocation file relative base */
	int	base;		/* allocation base */
	int	size;		/* allocation size */
	};

typedef struct 
	{
	struct ffsw *user_sw;	/* address of ffsw passed in by user */
	int	sw_count;	/* Bytes already completed. Used if */
				/* the request is split into synch and */
				/* asynch parts. */
	struct ffsw local_sw;	/* status word passed to lower level */
	} _lociosw;

struct _loclink
	{
	_lociosw loc_first[_FSSASYNUM];
	struct _loclink *loc_nxt;
	};

struct sds_f
	{
	struct sds_alo_s *sdsalo; /* ptr to array of SDS descriptors */
	int	sdsalnum;	/* number of sds allocations */
	int	sdsalospc;	/* number of sds_alo_s 'slots' allocated */
	long	sdssize;	/* number of ==BITS== in SDS allocation */
	long	sdseof;		/* 'real' EOF on sds file in bits */
	long	sdsbufblk;	/* relative SDS block number in buffer */
	long	fileptr;	/* logical bit position in the layer */

	long	minsize;	/* minimum requested SDS allocation */
	long	maxsize;	/* maximum requested SDS allocation */
	long	mininc;		/* minimum requested SDS alloc increment */

	int	sdsdirty;	/* flag is true if data is altered in SDS */
	bitptr	locbuf;		/* scratch space for bit shifting */

	int	overflowed;
	long	ovoff;		/* bit offset in file of lower layer file */
				/* used for overflow. */
	char	*name;		/* save name for overflow messages */
	int	sds_gran;	/* SDS allocation granularity: to keep us */
				/* well formed when doing SDS backdoor ops */
	int	bdfd;		/* Back door file descriptor */
	int	dsk_blksize;	/* statbuf->blksize copy to avoid dup STAT() */
	plock_t	locsw_lock;	/* a lock for the local status words */
	plock_t	rcllock;	/* a lock for 'recall' */
	int	scrtch_flgs;	/* flags from previous FC_SCRATCH request */
	struct _loclink *loclist;	/* linked list of local status words*/
	int	sdsklik;	/* allocation chunk size for sds (in blocks) */
	};

struct mr_f
	{
	long	mrsize;		/* bit size of buffer */
	long	mreof;		/* 'real' EOF on mr file, in bits */

	long	minsize;	/* minimum requested MR allocation */
	long	maxsize;	/* maximum requested MR allocation */
	long	mininc;		/* minimum requested MR alloc increment */

	int	mrdirty;	/* flag is true if data is altered in buffer */

	int	overflowed;
	long	ovoff;		/* bit offset in file of lower layer file */
				/* used for overflow. */
	char	*name;		/* save name for overflow messages */
	plock_t	locsw_lock;	/* a lock for the local status words */
	plock_t	rcllock;	/* a lock for 'recall' */
	int	scrtch_flgs;	/* flags from previous FC_SCRATCH request */
	struct _loclink *loclist;	/* linked list of local status words*/
	};

struct local_s
	{
	struct local_s *link;	/* ptr to next in chain */
	struct sds_alo_s *sdsalo; /* ptr to array of SDS descriptors */
	int	alnum;		/* number of alo descriptors */
	long	size;		/* size of SDS allocation */
	long	eof;		/* logical EOF in file */
	char	name[1];	/* name */
	};

/*
 *	Prototypes for some routines
 */

extern _lociosw *_mr_locsw_set(struct mr_f *mr_info, struct ffsw *usersw,
	int count);

extern void _mr_locsw_clear(struct mr_f *mr_info, _lociosw *usersw);

extern _lociosw *_sds_locsw_set(struct sds_f *sds_info, struct ffsw *usersw,
	int count);

extern void _sds_locsw_clear(struct sds_f *sds_info, _lociosw *usersw);

extern int _fss_overflow( struct fdinfo *fio, struct ffsw *stat);
#endif
