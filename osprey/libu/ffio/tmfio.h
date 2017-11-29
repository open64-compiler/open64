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


/* USMID @(#) libu/ffio/tmfio.h	92.0	10/08/98 14:57:41 */


#ifndef _TMFIO_H
#define _TMFIO_H

#include <errno.h>
#include <ffio.h>
#include <unistd.h>
#include <strings.h>
#include <cray/portdefs.h>
#include <tmf/tmfdefaults.h>
#include <tmf/sys/tmfctl.h>
#include <tmf/tmfreq.h>
#include <tmf/tmferr.h>

/*
 * Useful constants.
 */

#ifndef EOV_OFF
#define EOV_OFF 0
#endif

#ifndef EOV_ON
#define EOV_ON 1
#endif

#undef MIN
#undef MAX
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

#define TPOS_DONE 0
#define TPOS_REWD 1

/* Defines for ffp_type in struct ffp_tapepos */
#define POSBYBLOCK 1
/*
 *	Private info structure
 */
struct tmfio
	{
	char	*tmf_bufptr;	/* Pointer to next data in buffer. */
	char	*tmf_base;	/* Pointer to base of buffer. */
	size_t	tmf_cnt;	/* Byte count. Bytes left in buffer on read */
				/* Bytes accumulated on write. */
	size_t	tmf_bufsiz;	/* Byte size of buffer */
	int	tmf_fd;		/* file descriptor */
	short	rwflag;		/* READIN, WRITIN, POSITION */
	short	tmf_tpos;	/* nonzero means we're in the middle of some */
				/* positioning request. Can have the values: */
				/* TPOS_DONE (0) - no positioning going on */
				/* TPOS_REWD - 	issued a rewind request */
	/* These fields deal with tape characteristics */
	unsigned tmf_rwtpmk:1;	/* User can read/write tapemarks */

	/* These fields deal with eov processing */
	unsigned tmf_eovon:1;	/* User selected eov notification */
	unsigned tmf_eovhit:1;	/* User hit eov. Reset when startsp called */
	unsigned tmf_speov:1;	/* User started special eov processing */
	unsigned tmf_tpmk:1;	/* Buffered data is actually a tapemark */
	unsigned tmf_partblk:1;	/* The buffered data block is a partial one */
	long	spblocks;	/* Number of blocks remaining on the tape */
				/* Used in special eov processing */
	char	*eovbuf;	/* Buffered data, which could not be */
				/* written to the tape at eov, is stored here */
	char	*eovbase;	/* Base address of buffered data */
	size_t	eovbytes;	/* Number of bytes in eovbuf */
	};

extern ssize_t _tmf_read(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
	struct ffsw *retstat, int fulp, int *ubc);
extern ssize_t _tmf_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
	struct ffsw *retstat, int fulp, int *ubc);
extern _ffopen_t _tmf_open(const char *name, int flags, mode_t mode, 
	struct fdinfo *fio, union spec_u *spec, struct ffsw *stat, long cbits,
	int cblks, struct gl_o_inf *oinf);
extern int _tmf_bksp(struct fdinfo *fio, struct ffsw *stat);
extern int _tmf_skipf(struct fdinfo *fio, long nb, int *count, struct ffsw *st);
extern int _tmf_skiptpmk(int fd, int nt, int *count, struct ffsw *st);
extern int _tmf_stpos(struct fdinfo *fio, int nbs, long nb, int nvs, int nv,
	long long vi, int *usertm, struct ffsw *stat);
extern int _tmf_setsp(struct tmfio *xf_info, int iflag);
extern int _tmf_endsp(struct fdinfo *fio, struct ffsw *stat);
extern int _tmf_startsp(struct tmfio *xf_info);
extern int _tmf_tpwait(struct tmfio *xf_info);
extern int _tmf_tptsi(struct tmfio *xf_info, struct tsdata *tsi,
	char vsn[][L_MAXVSN]);
extern int _tmf_tpvsn(int fd, tmfpvsn_t *ctl);
extern int _tmf_tpvol(int fd, tmfpvol_t *ctl);
extern int _tmf_tpblk(int fd, long nb, int lib_tpmk, int *usertm);
extern int _tmf_weof(struct fdinfo *fio, struct ffsw *stat);
extern int _tmf_checkev(struct tmfio *xf_info, struct ffc_chkev_s *arg);
extern void __gtpos (struct tsdata *tsdata, char vsn[][L_MAXVSN], int lib,
	int partial, long long *pa, int palen);
extern int _tmf_gtpos(struct fdinfo *fio, long long *pa, int palen, int synch);
extern int _tmf_closev(struct fdinfo *fio, struct ffsw *stat);
extern int _tmf_gabs(struct fdinfo *fio, int *blockid);
extern int _tmf_sabs(struct fdinfo *fio, int abspos);

#endif	/* _TMFIO_H */
