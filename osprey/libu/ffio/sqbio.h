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


/* USMID @(#) libu/ffio/sqbio.h	92.0	10/08/98 14:57:41 */



/*
 *	Structure definitions for flags and fields that are used as private
 *	information to the buffering layer
 */

struct sqb_f
	{
	bitptr	locbuf;		/* scratch space for bit shifting */

	int	bufsiz;		/* size of each buffer, in bits */
	int	nbuf;		/* number of buffers */
	bitptr	_ptr;		/* bit pointer to current spot in buffer */
	struct sqbio *sqbio_base;	/* pointer to beginning of */
				/* structures describing data buffers */
	bitptr	sqb_buf;	/* pointer to base of data buffers */
	struct	sqbio *sqbio_cur;	/* pointer to current structure */
	long	ffci_flags;
	
	};

/* One structure for each data buffer */
struct sqbio
	{
	unsigned int	status:16;	/* status of buffer */
	unsigned int	userdata:1;	/* data in user's area? */
#ifdef _CRAY
	unsigned int 	unused:47;
#else
	unsigned int 	unused:15;
#endif
	int		_cnt;	/* in bits. If reading, amount of data */
				/* left in current buffer. */
				/* If writing, amount of space left in */
				/* current buffer. */
	struct ffsw iostat;	/* status word */
	bitptr	_base;		/* base of buffer */
	struct sqbio *nxt;	/* linked list */
	uint64 _iowritten;	/* amount of data written. In bytes */
	};

/* Defines for the state of each buffer: */
#define EMPTY 0		/* no outstanding I/O and no data in the buffer*/
#define IOACTIVE 1	/* I/O is outstanding */
#define IODATA 2	/* The buffer contains data */

#define BUFA_DEF_BUFSIZ	32768	/* default size of each buffer in bytes */
#define BUFA_DEF_NUMBUF	2	/* default number of buffers */
extern int _sqb_bksp(struct fdinfo *fio, struct ffsw *stat);
extern int _sqb_close(struct fdinfo *fio, struct ffsw *stat);
extern int _sqb_fcntl(struct fdinfo *fio, int cmd, void *arg, struct ffsw *stat);
extern int _sqb_flush(struct fdinfo *fio, struct ffsw *stat);
extern _ffopen_t _sqb_open(const char *name, int flags, mode_t mode,
        struct fdinfo *fio, union spec_u *spec, struct ffsw *stat, long cbits,
        int cblks, struct gl_o_inf *oinf);
extern ssize_t _sqb_read(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
        struct ffsw *retstat, int fulp, int *ubc);
extern _ffseek_t _sqb_seek(struct fdinfo *fio, off_t pos, int whence,
        struct ffsw *stat);
extern _ffseek_t _sqb_sync(struct fdinfo *fio, struct ffsw *stat, int sync);
extern int _sqb_weod(struct fdinfo *fio, struct ffsw *stat);
extern int _sqb_weof(struct fdinfo *fio, struct ffsw *stat);
extern ssize_t _sqb_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
        struct ffsw *retstat, int fulp, int *ubc);
