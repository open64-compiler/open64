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


/* USMID @(#) libu/ffio/gxio.h	92.0	10/08/98 14:57:41 */
#ifndef _GXIO_H
#define _GXIO_H
#include <cray/portdefs.h>

/* CLASS X */
#define TR_NVE_V	1	/* NOS/VE V */
#define TR_CRAY_V	2	/* CRAY stream blocking */
#define TR_UX_VAX	3	/* UNIX tm blocking VAX */
#define TR_UX_SUN	4	/* UNIX tm blocking 68000/sun */
#define TR_205_W	5	/* Cyber 205 W type blocking */

/*
 * The CDC/NOSVE/205 SCC codes are as follows.
 */
#define NVSCCFULL 0x30 /* '0' */
#define NVSCCFRST 0x31 /* '1' */
#define NVSCCLAST 0x33 /* '3' */
#define NVSCCMIDL 0x32 /* '2' */

/*
 * Useful constants.
 */
#define WPBLOCK		512			/* words per 512 wd disk block*/
#define BYTPBLOCK	4096			/* bytes per 512 wd disk block*/
#define BITPBLOCK	32768			/* bits per 512 wd disk block*/
#define X_BUFSIZ	8			/* dflt bufsize in 512-wd blks*/

/*
 * Macro to swap bytes for VAX formats.  Works on low 32 bits only.
 */
#define SWAPB(jj)		{					\
				jj = ((jj >> 16) & 0xFFFF) | jj << 16;	\
				jj =	((jj >> 8) & 0x00FF00FF) |	\
					((jj << 8) & 0xFF00FF00);	\
				}

/*
 * X class control words
 */

/*
 * NOS/VE V format record desciptor
 */
union nosve_rdw_u
	{
	struct
		{
		char flag	;	/* 0 for normal record, 4 for EOF */
		char length[6]	;	/* integer, 6 bytes, length of data */
					/* in the record */
		char prev_addr[6];	/* previous byte record address */
		char mark;		/* always 0x1e */
		} fld;
	unsigned long wwords[2];
	};

/*
 * UX/F77 blocking
 */
union ux_rdw_u
	{
	struct
		{
		int len:32;		/* length of the record in bytes */
					/* one goes fore and aft of each rec */
					/* For VAX they are byte swapped */
		} fld;
	unsigned long wword;
	};

/*
 * Cyber 205 W control word
 */
union w_205_u
	{
	struct
		{
		int r:		3;	/* reserved for installation */
		int rsv1:	8;	/* reserved for CDC */
		unsigned p:	1;	/* maintain ODD parity with this */
		int fd:		2;	/* control word type:	*/
					/*	11 = EOF CW	*/
					/*	10 = EOG CW (group)*/
					/*	01 = deleted record */
					/*	00 = normal record */
		int wcr:	2;	/* record continuation flags */
					/* 0=FULL, 1=FRST, 2=MIDL, 3=LAST */
		int ps:		24;	/* previous size, includes pad */
		int bc:		24;	/* byte count.  actual data (not pad)*/
		} fld;
	unsigned long wword;
	};
#ifndef __FF_OFFT
#define __FF_OFFT
#ifdef __mips
typedef off64_t ff_offt;
#else
typedef off_t ff_offt;
#endif
#endif
/*
 *	Private info structure
 */
struct gen_xf			/* X blocking */
	{
	int	rdwlen;		/* segment descriptor word length */
				/* including any SCC */
	int	rdwtype;	/* segment descriptor word type */
				/* (for backpatching) */
	int	scclen;		/* segment control code length */
	int	scctype;	/* segment control code type */
	int	sccpos;		/* SCC position in RDW */

	int	lscc;		/* Local scc code for intra-segment work */

	int64	rembits;	/* remaining bits in record or segment */
				/* (used in read) */
	long	cwskip;		/* padd bits to skip between end of data in */
				/* segment and next CW. */
	/* the following are byte offsets in the file */
	ff_offt	last_lrdwaddr;	/* last leading rec desc file pointer */
	ff_offt	lrdwaddr;	/* leading record descriptor file pointer */
	ff_offt	frdwaddr;	/* following record descriptor file pointer */

	};
extern ssize_t _gen_xread(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
        struct ffsw *retstat, int fulp, int *ubc);
extern ssize_t _gen_xwrite(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
        struct ffsw *retstat, int fulp, int *ubc);
extern _ffseek_t _gen_xseek(struct fdinfo *fio, off_t pos, int whence,
        struct ffsw *stat);
extern _ffopen_t _gen_xopen(const char *name, int flags, mode_t mode, 
	struct fdinfo *fio, union spec_u *spec, struct ffsw *stat, long cbits,
	int cblks, struct gl_o_inf *oinf);
extern int _gen_xbksp(struct fdinfo *fio, struct ffsw *stat);

#endif
