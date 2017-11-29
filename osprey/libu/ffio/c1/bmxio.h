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


/* USMID @(#) libu/ffio/c1/bmxio.h	92.0	10/08/98 14:57:41 */
 

#ifndef NULL
#include <stdio.h>
#endif
 
#ifndef NBPW
#include <values.h>
#endif
#include <ffio.h>
#include "tapeio.h"

#define MAXER90BUF 1794048	/* maximum size of 1 buffer for er90 */
#define MAXER90BLK 1196032	/* current maximum block size allowed for er90 */

#define MAXBMXRCALL 100000

#define WAITBMXIO(f,ioptr) {					\
		int wt_ct = 0;					\
		struct iosw *__stat;				\
		while (BMXIO_BUSY(ioptr->bmx_iosw)) {		\
			__stat = &ioptr->bmx_iosw;		\
			(void) recall(f->bmx_fd,1,&__stat);	\
			if (wt_ct++ > MAXBMXRCALL){		\
				errno = FEINTUNK;		\
				return(-1);		\
			}					\
		}						\
	}
struct bmxio{
	char		*bmx_base;	/* base address of i/o buffer	*/
	struct bmxlist	*bmx_list;	/* bmx list base pointer	*/
	struct iosw 	bmx_iosw;	/* status word for asynch i/o	*/
	int 		bmx_busy;
	struct bmxio   	*bmx_nxt;	/* linked list			*/
	struct bmxio   	*bmx_prev;	/* linked list backwards 	*/
};

struct eovbuf{
	char		*bmx_bufmem;
	struct bmxlist 	*bmx_buflst;
	struct eovbuf	*eov_nxt;
};

typedef struct {
	int	        bmx_fd;		/*  file descriptor		*/
	int	        bmx_mbs;	/*  maximum block size		*/

	struct bmxio   *bmx_iofirst;
	struct bmxio   *bmx_iosavefirst;
	struct bmxio   *bmx_iocurrent;	
	char           *bmx_bufptr;	/*  current pointer into buffer	*/
	int             bmx_bufcnt;	/*  current offset in buffer	*/
	int          	bmx_bufsz;	/*  size of i/o buffer		*/
 
	struct bmxlist *bmx_lstptr;	/*  pointer to current list	*/
	int	        bmx_lstcnt;	/*  current offset in list	*/
	struct bmxlist *bmx_listptr;
	int	        bmx_lstsz; 	/*  size of list		*/
	struct tptsireq tsireq;
	int	        bmx_recl; 	/*  transferred record count	*/
	int	        bmx_flag;  	/*  flags			*/

	int		bmx_tpos;	/*  active tape position req	*/
	int		bmx_bufszsv;	/*  used to save size of i/o buffer*/
	int		bmx_rwtpmk;	/*  can we read/write tapemarks?*/
	int		bmx_totblk;	/*  blocks left on tape		*/
	int		bmx_bmblk;	/*  blocks buffered by system	*/
	struct eovbuf	*eov_first;	/*  First in linked list of	*/
					/*  library buffered data at eov*/
	struct eovbuf	*eov_current;	/*  current one in linked list of   */
					/*  library buffered data at eov    */
	char		*bmx_cbufmem;   /*  ptr into library buffered	*/
					/*  data at eov 		*/
	struct bmxlist	*bmx_cbuflst;   /*  pointer into list that describes */
					/*  data library has buffered at eov */
	struct bmxio	*bmx_lstcont;	/*  saves away linked list pointers  */
					/*  during eov processing	*/
	int		bmx_buflstsz;	/*  Size of lists describing the*/
					/*  data library has buffered at eov */
	int		bmx_buflstcnt;  /*  Count of number of entries 	*/
	struct bmxio	*bmxio_alloc;	/*  pointer to space allocated */
					/*  at open time.		*/
	unsigned int	er90:1;		/*  1 iff ER90 in block mode	*/
}BMXFIL;

extern BMXFIL	*bmx_open();
 
/* 
 *	State definitions
 */

#define BMX_DATA	1		/* data only		*/
#define BMX_EOR		2		/* end of tape block	*/
#define BMX_EOF		3		/* end of file 		*/
#define BMX_SKIPBAD	4		/* skip bad data	*/
#define BMX_ACPTBAD	5		/* accept bad data	*/
 

#ifndef NBPC
#define NBPC		4096		/* number of bytes per click	*/
#endif

#ifndef BPCSHIFT
#define BPCSHIFT	12		/* LOG2(NBPC)          	 */
#endif

/*
 *	Define a macro to round up a byte count to the next 
 *	click boundry.
 */

#define rtoc(x)		((x+NBPC-1) & ~(NBPC-1))

/*
 *	Define a macro to convert a byte count to a number    
 *	of clicks.  And a macro to convert clicks to bytes.
 */

#define btoc(x)		(x>>BPCSHIFT)
#define ctob(x)		(x<<BPCSHIFT)

#define IOSW_BUSY	0
#define IOSW_DONE	1

#define BMXIO_BUSY(x)        (((x.sw_flag) == (IOSW_BUSY)) ? (1) : (0) )
#define BMXIO_ERROR(x)        (x.sw_error)

/*
 *	Flag definitions
 */

#define BMXIO_RW	0001		/* previous i/o operation	*/
#define BMXIO_EOR	0004		/* User read all the data in rec */
#define BMXIO_DATA	0010		/* Data block encountered	*/
#define BMXIO_SKIP	0020		/* Skip bad data blocks  	*/
#define BMXIO_ACPT	0040		/* Accept bad data blocks	*/
#define BMXIO_EOV	0100		/* doing EOV processing		*/
#define BMXIO_EOVSEEN	01000		/* Hit EOV			*/
#define BMXIO_SPPROC	02000		/* In special EOV processing	*/
#define BMXIO_TPMK	04000		/* Tape mark read		*/
#define BMXIO_ERRSEEN	020000		/* Error occurred on last i/o 	*/
#define BMXIO_50	010000		/* Return EOD, when EOD, (not EOF) */
#define BMXIO_NXT	040000		/* EOR 				*/
#define BMXIO_MODELE	0100000		/* Model E IOS 			*/
#define BMXIO_WEOV	0200000		/* EOV reached while writing 	*/
#define BMXIO_SKIPALL	0400000		/* Skip all bad data blocks	*/
/*
 *	Positioning flags and definition
 */


#define INACTIVE 0
#define DATA 1
#define ACTIVE 2
/*
 * Define the minimum default buffer size in bytes and for
 * Model E systems, the default maximum total
 * size of all buffers in bytes
 */
#define MIN_DEF_BSIZE	_VALUE(_def_tape_bs)
#define MAX_DEF_TOTBSIZE _VALUE(_def_tape_totbuf)

#if TRACE		
#define F_TRACE(a, p1, p2, p3)	\
	f_trace(a, p1, p2, p3)
#else
#define F_TRACE(a, p1, p2, p3)
#endif

extern int _def_tape_bs;
extern int _def_tape_totbuf;
/*
 * Prototypes for some internal functions		
 */
int __bmxflush(BMXFIL *f);
int __bmxwrite(BMXFIL *f, const void *uda, long bytes, long state);
int _bmx_bdr(BMXFIL *f, void *uda, long *termcnd, long state, ...);
int _bmx_checktp(BMXFIL *f, long *istat, long *icbuf);
int _bmx_endsp(BMXFIL *f);
int _bmx_wait(BMXFIL *f);
int _disable_eov(int fd);
int _enable_eov(int fd);
int _end_eov(int fd);
int _eov_wait(BMXFIL *f, struct bmxio *ioptr);
int _eov_buf(BMXFIL *f, struct bmxio *ioptr);
void _eov_load(int i);
int _loop_write(BMXFIL *f, struct bmxio *ioptr, int bs);
int _er90_bmx(const char *name, int oflag, struct gl_o_inf *oinf);
int _start_eov(int fd);
void _bmx_clear(BMXFIL *f);
int _bmx_closev(BMXFIL *f);
int _bmx_clrerr(BMXFIL *f);
int _bmx_eovq(BMXFIL *f);
int _bmx_gabs(BMXFIL *f, void *blockid);
int _bmx_gtpos(BMXFIL *f, long *pa, long palen, long synch);
int _bmx_prepos(BMXFIL *f);
int _bmx_quiet(BMXFIL *f);
int _bmx_sabs(BMXFIL *f, ...);
int _bmx_setsp(BMXFIL *f, int iflag);
int _bmx_skipf(BMXFIL *f, long nb, int *count);
int _bmx_startsp(BMXFIL *f);
int _bmx_stpos(BMXFIL *f, long nbs, long nb, long nvs, long nv, long vi, ...);
int _bmx_tpmk(int fd, int nb, int *rescount);

