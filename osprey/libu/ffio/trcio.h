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


/* USMID @(#) libu/ffio/trcio.h	92.0	10/08/98 14:57:41 */


#define TRC_OPEN	1
#define TRC_READ	2
#define TRC_READA	3
#define TRC_READC	4
#define TRC_WRITE	5
#define TRC_WRITEA	6
#define TRC_WRITEC	7
#define TRC_CLOSE	8
#define TRC_FLUSH	9
#define TRC_WEOF	10
#define TRC_WEOD	11
#define TRC_SEEK	12
#define TRC_BKSP	13
#define TRC_POS		14
#define TRC_ALGN	15	/* unused */
#define TRC_FCNTL	16
#define TRC_LISTIO	17

#define HIST_INIT	1024	/* words */
#define HIST_INCR	1024	/* increment */
#define HIST_MAX	1048576	/* max size */
#define HIST_BKT	512*8	/* one sector */

#define PFULP(fio, fulp)					\
			{					\
			char *str;				\
								\
			str = "PARTIAL";			\
			if (fulp == FULL)			\
				str = "FULL";			\
			_trc_info(fio, str, 0);			\
			}

#define UPDPOS(fio, ret, optype)					\
		{							\
		struct trace_f *trc_info;				\
									\
		trc_info = (struct trace_f *)fio->lyr_info;		\
/*									\
 *		Update counters						\
 */									\
		trc_info->optype++;					\
									\
		if (trc_info->lastseek == NO)				\
			trc_info->forw++;				\
		else if (trc_info->curpos == trc_info->last_endpos)	\
			trc_info->forw++;				\
		else if (ret >= 0 && 					\
			((trc_info->curpos + ret) == trc_info->last_stpos)) \
			trc_info->backw++;				\
		else	/* lastseek is YES */				\
			{						\
			if (trc_info->curpos == -1)			\
				trc_info->unknown++;			\
			}						\
/*									\
 *		Update position						\
 */									\
		trc_info->last_stpos = trc_info->curpos;		\
		if (trc_info->last_stpos >= 0 && ret >= 0)		\
			trc_info->last_endpos =	trc_info->last_stpos + ret; \
		else							\
			trc_info->last_endpos = -1; /* unknown */	\
		trc_info->curpos = trc_info->last_endpos;		\
									\
/*									\
 *		Update histogram					\
 */									\
		/* Hist stuff */					\
									\
		}

struct trace_f
	{
	char	*name;		/* name of the file (for overflow) */
	int	trcfd;		/* file descriptor of trace file */
	char	prtbuf[512];	/* print buffer */
	int	bufptr;		/* print buffer pointer */
	int	*hist;		/* Pointer to Histogram */
	int	histsiz;	/* Histogram current size */
	int	histmax;	/* Histogram max size */
	int	histincr;	/* Histogram increment size */
	int	histbkt;	/* bucket size in bytes */
	int	curpos;		/* current byte position */
	int	lastseek;	/* flag that last seek could have moved pos */
	int	last_stpos;	/* remembered start position */
	int	last_endpos;	/* remembered end position */
	int	forw;		/* forward seq counter */
	int	backw;		/* backward seq counter */
	int	unknown;	/* unknown counter (successive reada) */
	int	reads;
	int	readas;
	int	writes;
	int	writeas;
	int	lseeks;
	};
