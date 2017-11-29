/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* USMID @(#) clibinc/ffio.h	92.11	11/09/99 17:20:39 */


/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

/*
 *	FFIO definitions, macros, and prototypes
 */

#ifndef _FFIO_H
#define _FFIO_H

#include <clibdefs.h>
#include <sys/types.h>		/* ffc_stat_s structure field declarations */
#include <cray/fortio.h>	/* unum_t typedef */
#if defined(__mips)
#include <aio.h>
#elif defined(_LITTLE_ENDIAN)
/* do not include pthread.h for little endian systems */
#else
#include <cray/libuni.h>	/* for _LIB_UMK, used in fflistreq declaration*/
#endif

/*
 * read/write record processing modes
 */
#define PARTIAL 0
#define FULL	1

/*
 * Status values for sw_status
 */
#define FFBSY	0
#define FFCNT	1
#define FFEOR	2
#define FFEOF	3
#define FFEOD	4
#define FFBOD	5
#define FFERR	6

/*
 * Command codes for fffcntl calls
 *	Values:	0 -	reserved for FORTRAN 'get fio pointer'
 *		1-99	reserved for CRI built-in layers
 *		100-199	reserved for user layers
 *		200-299	reserved for site layers
 */
#define FC_GETINFO	1
#define FC_STAT		2
#define FC_SETRECL	3
#define FC_RECALL	4
#define FC_ACPTBAD	5
#define FC_SKIPBAD	6
#define FC_GETTP	7
#define FC_AUTOBAD	8
#define FC_CHECKTP	9
#define FC_ENDSP	10
#define FC_STARTSP	11
#define FC_CLOSEV	12
#define FC_SETSP	13
#define FC_ASPOLL	14	/* give recall a shot at cleanup, no wait */
#define FC_SCRATCH	15	/* identify a file as a scratch file */
#define FC_TSYNC	16	/* sync a tape file */
#define FC_DUMPSTATS	17	/* dump intermediate layer statistics */
#define FC_TPC_SDBSZ	18	/* change block size on ER90 */
#define FC_IALLOC	19	/* bottom layer call ialloc */
#define FC_GETLK	20	/* get file lock */
#define FC_SETLK	21	/* set file lock */
#define FC_SETLKW	22	/* set file lock wait */
#define FC_FSTATFS	23	/* bottom layer call fstatfs */
#define FC_DIOINFO	24	/* get direct info (IRIX only) */
#define FC_CHECKEOV	25	/* check end-of-volume status (IRIX only) */
#define FC_TSDATA	26	/* get tape status information (IRIX only) */

/*
 * Mask bits for return word in FC_GETINFO fffcntl call
 */
#define	FFC_STRM	0x00000001	/* can handle stream I/O */
#define	FFC_REC		0x00000002	/* can handle records */
#define	FFC_WEOF	0x00000004	/* can represent EOF */
#define	FFC_WEOD	0x00000008	/* can represent EOD (all can do) */

#define	FFC_BKSP	0x00000010	/* can handle backspace */
#define	FFC_BKFIL	0x00000020	/* can handle backfile */
#define	FFC_SEEKA	0x00000040	/* can seek absolute */
#define	FFC_SEEKR	0x00000080	/* can seek relative */
#define	FFC_SEEKE	0x00000100	/* can seek to end */
#define	FFC_POSREC	0x00000200	/* can position by record number */
#define	FFC_POSFIL	0x00000400	/* can position by EOF mark */
#define	FFC_RWND	0x00000800	/* can rewind by seek(x,0,0) */

#define	FFC_FIXD	0x00001000	/* can do fixed len recs */
#define	FFC_VAR		0x00002000	/* can do variable len recs */
#define	FFC_BINARY	0x00004000	/* can do binary records */
#define	FFC_CODED	0x00008000	/* can do formatted records */

#define	FFC_RDM		0x00010000	/* can do random I/O (no trunc) */
#define	FFC_SEQ		0x00020000	/* can do sequential I/O */
#define	FFC_ASYNC	0x00040000	/* can do asynchronous I/O */
#define	FFC_WRTRUNC	0x00080000	/* write implies truncation */

#define	FFC_NOTRN	0x00100000	/* Does no transformation on data */
					/* hence, 'backdoor' ops can be OK */
#define	FFC_BCKDOOR	0x00200000	/* Can handle O_SSD flag on open call */
#define	FFC_SKIPBAD	0x00400000	/* Can skip bad data */
#define	FFC_NOCLOSE	0x00800000	/* Don't try to close layer on abort */
#define	FFC_CANLISTIO	0x01000000	/* The layer has a listio entry */
#define	FFC_CANSYLISTIO	0x02000000	/* The layer has a listio entry that  */
					/* is capable of limited listio */

/*
 * Command codes for POS calls
 */
#define FP_GETPOS	1	/* get position of file */
#define FP_SETPOS	2	/* set position of file */
#define FP_BSEEK	3	/* seek to bit position */
#define FP_BKSP		4	/* back up one record (BACKSPACE) */
#define FP_BKFIL	5	/* back up one file (BACKFILE) */
#define FP_GETREC	6	/* Get current record number */
#define FP_SETREC	7	/* Set position to specified record number */
#define FP_SKIPF	8	/* Skip file (SKIPF) */
#define FP_SETTP	9	/* set position of tape file (SETTP) */
#define FP_POS0		10	/* "position to current position" */
#define FP_RSEEK	11	/* relative seek */
#define FP_SKIPTPMK	12	/* skip tape mark */
#define FP_GABS		13	/* get absolute position */
#define FP_SABS		14	/* set absolute position */
#define FP_GETTAPEPOS	15	/* get tape position (IRIX only) */
#define FP_SETTAPEPOS	16	/* set tape position (IRIX only) */

/*
 * Codes used to indicate direction/meaning for some positioning requests
 */
#define FP_TPOS_ABS	0	/* absolute */
#define FP_TPOS_FORW	1	/* forward */
#define FP_TPOS_BACK	2	/* backwards */

/*
 * Argument values for FC_AUTOBAD fffcntl request
 */
#define AUTO_SKIP	1	/* skip 1 block of bad data */
#define AUTO_ACPT	2	/* accept bad data */
#define AUTO_SKIPALL	3	/* skip all bad data */

/*
 * Flag values for FC_SCRATCH fffcntl request status word.
 */
#define SCR_SINGLELINK	1	/* file is regular and not a linked file */
#define SCR_UNLINKED	2	/* an unlink() successfully deleted the file */
#define	SCR_NOFLUSH	4	/* ffclose() buffer flushing suppressed */

/*
 * Return the I/O status from an operation.
 */
#define FFSTAT(io_sw)		((io_sw).sw_stat)

/*
 *	Status return:  This structure is used in all internal calls to return
 *	status.  The first word is made compatible with the iosw for use
 *	with async I/O.
 */
struct ffsw
	{
	unsigned int	sw_flag:1;
	unsigned int	sw_error:31;
#ifdef	_UNICOS
	unsigned int	sw_count:32;
#else
	ssize_t		sw_count;
#endif
	unsigned int	sw_stat:16;	/* FFCNT, etc. */
#if	defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
	int		sw_user:32;	/* For user layer storage */
#else
	int		sw_user:48;	/* For user layer storage */
#endif
#if	defined(_ADDR64) || defined(__mips) || defined(_LITTLE_ENDIAN)
	void		*sw_iptr;	/* pointer to layer data */
	void		*sw_sptr;	/* pointer to status chain, if used. */
#else
	int		sw_iptr:32;	/* pointer to layer data */
	int		sw_sptr:32;	/* pointer to status chain, if used. */
#endif
	int		sw_rsv1;	/* may need later.  Save space now */
#ifdef	__mips
	aiocb_t		aiocbp;
#endif
#ifdef	_EXTRA_FFSW_STUFF
	_EXTRA_FFSW_STUFF		/* for internal Cray applications use */
#endif
	};

/*
 *	fffcntl FC_GETINFO structure
 */
struct ffc_info_s
	{
	long	ffc_flags;	/* flag word */
	int	ffc_gran;	/* minimum granularity, no. bits in smallest */
				/* I/O possible. (bits/bytes/words) */
	int	ffc_reclen;	/* Record length (in bits) */
	int	ffc_fd;		/* Lowest level file desc, -1 if invalid */
	int	ffc_poslen:16;	/* GETPOS/SETPOS pos array length */
#if	defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
	int	unused1:16;	/* reserved */
#else
	int	unused1:48;	/* reserved */
#endif
	int	unused2;	/* reserved */
	};

/*
 *	fffcntl FC_SKIPBAD or FC_ACPTBAD structure
 */
struct ffc_baddata_s
	{
	int	ffc_termcnd;	/* position after handling bad data */
	int	ffc_blocks;	/* number of blocks skipped */
	int	ffc_bytes;	/* number of bytes accepted */
	int	ffc_maxflag;	/* 1 if maximum number of words to accept */
	int	ffc_maxwords;	/* maximum number of words to accept */
	long	ffc_uda;	/* address of area to store data */
	int	unused1;	/* reserved */
	int	unused2;	/* reserved */
	};

/*
 *	fffcntl FC_GETTP structure
 */
struct ffc_gettp_s
	{
	int	ffc_glen;	/* number of words to copy to ffc_pa */
	int	ffc_synch;	/* 1 = synch, 0 = do not. Only after write */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	long long	*ffc_pa;/* info copied here */
#else
	long	*ffc_pa;	/* info copied here */
#endif
	int	unused1;	/* reserved */
	int	unused2;	/* reserved */
	};

/*
 *	fffcntl FC_CHECKTP structure
 */
struct ffc_chktp_s
	{
	int	stat;		/* status of tape */
	int	libblk;		/* Number of blocks in library */
	int	unused1;	/* reserved */
	int	unused2;	/* reserved */
	};

/*
 *	fffcntl FC_CHECKEOV structure
 */
struct ffc_chkev_s
	{
	int	stat;		/* status of tape: 1 = at eov */
	int	bufblk;		/* Number of buffered whole blocks */
	size_t	bufbytes;	/* number of bytes unwritten from last record
				   on the tape */
	int	unused1;	/* reserved */
	int	unused2;	/* reserved */
	};
	
#ifdef KEY /* Bug 1678 */
/* This was out of sync for SuSE9 Linux -m32 when -D_FILE_OFFSET_BITS=64
 * and -D_LARGEFILE_SOURCE were defined. Keeping a private version of
 * "struct stat" in sync with the real version is a losing proposition. */
#else /* KEY Bug 1678 */
/*
 *	fffcntl FC_STAT structure
 *
 *	This is a copy of the stat structure defined in <sys/stat.h>.
 *	Matches stat structure for:
 *
 *			UNICOS 8.0
 *			UNICOS-MAX 1.0
 *			Solaris 5.3
 */
#ifdef	_UNICOS

struct	ffc_stat_s
	{
	dev_t	st_dev;
	ino_t	st_ino;
	mode_t 	st_mode;
	nlink_t	st_nlink;
	uid_t 	st_uid;
	gid_t 	st_gid;
	dev_t	st_rdev;
	off_t	st_size;
	time_t	st_atime;
	long	st_spare1;		/* Reserved for microsec time stamps */
	time_t	st_mtime;
	long	st_spare2;		/* Reserved for microsec time stamps */
	time_t	st_ctime;
	long	st_spare3;		/* Reserved for microsec time stamps */
	_SHORTPAD
	unsigned short	st_acid;	/* Accounting id */
#if	defined(_CRAY1) || defined(_CRAYMPP)
	long	st_count;		/* Reference count */
	off_t	st_msize;		/* Modification size for signature */
#endif /* _CRAY1 || _CRAYMPP */
	unsigned int st_msref : 1,	/* Modification signature referenced */
		     st_ms    :31,	/* Modification signature */
		     st_gen   :32;	/* Generation number */
#if	defined(_CRAY1) || defined(_CRAYMPP)
/*18*/
	long	st_blocks;		/* #of sectors allocated to file */
	long	st_blksize;		/* Optimal small I/O xfer size (bytes)*/
	long	st_oblksize;		/* Optimal large I/O xfer size (bytes)*/
	long	st_allocf;		/* Allocation flags */
	long	st_cblks;		/* Cluster blocks (Cray-YMP) */
	long	st_cbits;		/* Cluster bits */
	long	st_param[8];		/* 8-pack track/sector addresses */
	long	st_spareparam[4];	/* Reserved for additional pack */
#endif /* _CRAY1 || _CRAYMPP */
/*36*/
	/* data migration fields */
	_SHORTPAD
	unsigned short	st_dm_mode;	/* actual file mode */
	unsigned int
		st_dm_port  : 3,	/* migrated file daemon port */
		st_dm_state : 5,	/* migrated file state */
		st_dm_status: 56;	/* migrated file status flags */
	long	st_dm_mid;		/* migrated file machine id */
	long	st_dm_key;		/* migrated file key */

/*40*/
	/* security fields */
	unsigned int	    : 62,
		st_hasacl   : 1,	/* File has an ACL */
		st_hascomps : 1;	/* File has compartments */
	_SHORTPAD
	short	st_slevel;		/* file level */
	_SHORTPAD
	short	st_secflg;		/* security flags */
	_SHORTPAD
	short	st_intcls;		/* integrity class */
	_SHORTPAD
	short	st_intcat;		/* integrity category */

/*45*/
	long	st_site;		/* site field from inode */
	long	st_nindir;		/* # of indirect extent blocks */
	long	st_resv_pad[6];		/* reserved pad space to 53 words */
	};

#elif	defined(_SOLARIS)

#include <sys/time.h>

struct	ffc_stat_s {
	dev_t	st_dev;
	long	st_pad1[3];
	ino_t	st_ino;
	mode_t	st_mode;
	nlink_t st_nlink;
	uid_t 	st_uid;
	gid_t 	st_gid;
	dev_t	st_rdev;
	long	st_pad2[2];
	off_t	st_size;
	long	st_pad3;
	timestruc_t st_atim;
	timestruc_t st_mtim;
	timestruc_t st_ctim;
	long	st_blksize;
	long	st_blocks;
	char	st_fstype[16 /* == _ST_FSTYPSZ */ ];
	long	st_pad4[8];
	};

#elif	defined(__mips)
#include <sys/stat.h>
#ifdef	_TIMESTRUCT_T
#define _TIMESTRUCT_FFIO timestruct_t
#else
#define _TIMESTRUCT_FFIO timespec_t
#endif
struct	ffc_stat_s {
	dev_t   st_dev;
	long    st_pad1[3];     /* reserved for network id */
	ino_t   st_ino;
	mode_t  st_mode;
	nlink_t st_nlink;
	uid_t   st_uid;
	gid_t   st_gid;
	dev_t   st_rdev;
	long    st_pad2[2];     /* dev and off_t expansion */
	off_t   st_size;
	long    st_pad3;        /* future off_t expansion */
	_TIMESTRUCT_FFIO st_atim;
	_TIMESTRUCT_FFIO st_mtim;
	_TIMESTRUCT_FFIO st_ctim;
	long    st_blksize;
	blkcnt_t st_blocks;
	char    st_fstype[_ST_FSTYPSZ];
	long    st_pad4[8];     /* expansion area */
};
#elif	defined(_LITTLE_ENDIAN)
#include <sys/stat.h>
#ifdef _LP64
struct	ffc_stat_s {
	dev_t	st_dev;
	ino_t	st_ino;
	nlink_t st_nlink;
	mode_t	st_mode;
	uid_t 	st_uid;
	gid_t 	st_gid;
        int     pad0;
	dev_t	st_rdev;
	off_t	st_size;
	time_t	st_atim;
        long int __reserved0;
	time_t	st_mtim;
        long int __reserved1;
	time_t	st_ctim;
        long int __reserved2;
	unsigned long	st_blksize;
	blkcnt_t	st_blocks;
	unsigned long	unused1[1];
	unsigned long	unused2[1];
	unsigned long	unused3[1];
	};
#else
struct	ffc_stat_s {
	dev_t	st_dev;
	unsigned short	st_pad1[1];
	ino_t	st_ino;
	mode_t	st_mode;
	nlink_t st_nlink;
	uid_t 	st_uid;
	gid_t 	st_gid;
	dev_t	st_rdev;
	unsigned short	st_pad2[1];
	off_t	st_size;
	unsigned long	st_blksize;
	blkcnt_t	st_blocks;
	time_t	st_atim;
	unsigned long	unused1[1];
	time_t	st_mtim;
	unsigned long	unused2[1];
	time_t	st_ctim;
	unsigned long	unused3[1];
	unsigned long	unused4[1];
	unsigned long	unused5[1];
	};
#endif /* _LP64 */

#endif	/* __mips */

#endif	/* KEY */

/*
 *	fffcntl FC_IALLOC structure
 */
struct ff_ialloc_struct 
	{
	long	ia_nb;
	int	ia_flag;
	int	ia_part;
	int	ia_avl;
	int	ia_ret;
	int	ia_before;
	};
/*
 *	FP_SKIPF structure
 */
struct ffp_skipf_s
	{
	int	ffp_nfil;	/* number of files to skip (input and output) */
	int	ffp_nrec;	/* number of records skipped (optional output)*/
	};

/*
 *	FP_SETTP structure
 */
struct ffp_settp_s
	{
	int		ffp_nbs_p;	/* forward, backward or absolute nb */
	long		ffp_nb;		/* number of blocks (always positive) */
	int		ffp_nvs_p;	/* forward, backward or absolute nv */
	int		ffp_nv;		/* number of volumes (always positive) */
#if	defined(__mips)
	__int64_t	ffp_vi;		/* volume identifier */
#elif	defined(_LITTLE_ENDIAN)
	long long	ffp_vi;		/* volume identifier */
#else
	long		ffp_vi;		/* volume identifier */
#endif
	int		unused1;	/* reserved */
	int		unused2;	/* reserved */
	};
	
/*
 *	FP_SKIPTPMK structure
 */
struct ffp_skiptpmk_s
	{
	int	ffp_ntpmk;	/* number of tpmks to skip (input and output) */
	int	unused1;	/* reserved */
	};

/*
 *	FP_GABS and FP_SABS structure
 */
	struct ffp_abs
	{
#if defined(__mips)
	__int64_t	ffp_absaddr;
#elif	defined(_LITTLE_ENDIAN)
	long long	ffp_absaddr;
#else
	long	ffp_absaddr;
#endif
	int	ffp_partition;
	int	ffp_filesec;
	int	ffp_datablock;
	};

/*
 *	FP_GETTAPEPOS and FP_SETTAPEPOS structures
 */
	struct ffp_tapepos
	{
	int	ffp_type;
	int	ffp_blockno;
#if	defined(__mips)
	__int64_t	ffp_vsn;
#elif	defined(_LITTLE_ENDIAN)
	long long	ffp_vsn;
#else
	long	ffp_vsn;
#endif
	int	ffp_resvd[4];
	};

/*
 *	fflistio request structure, similar to def in <sys/listio.h>
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
struct	fflistreq
	{
	int	li_opcode;		/* operation code */
	int	li_drvr:32,		/* driver dependent */
		li_flags:32;		/* request flags */
	off_t	li_offset;		/* starting byte offset */
	int	li_fildes;		/* file descriptor (returned by ffopen)*/
	char	*li_buf;		/* buffer address */
	size_t	li_nbyte;		/* bytes per stride count */
	struct ffsw *li_status;		/* status structure */
	int	li_signo;		/* signal to send upon completion */
	int	li_nstride;		/* number of strides */
	off_t	li_filstride;		/* file stride length in bytes */
	size_t	li_memstride;		/* buffer stride length in bytes */
	struct fdinfo *li_ffioptr;	/* for use by library routines */
	};
#else
struct	fflistreq
	{
	int	li_opcode;		/* operation code */
	int	li_drvr:32,		/* driver dependent */
		li_flags:32;		/* request flags */
	long	li_offset;		/* starting byte offset */
	int	li_fildes;		/* file descriptor (FFIO pointer) */
	char	*li_buf;		/* buffer address */
	long	li_nbyte;		/* bytes per stride count */
	struct ffsw *li_status;		/* status structure */
	int	li_signo;		/* signal to send upon completion */
	int	li_nstride;		/* number of strides */
	long	li_filstride;		/* file stride length in bytes */
	long	li_memstride;		/* buffer stride length in bytes */
#ifdef	_LIB_UMK
#ifndef _CRAY1
	int	li_remote_vpe;		/* distio vpe number */
	int	li_pad[3];		/* mpp extensions */
#endif
#endif
	};
#endif

#ifdef	_LIB_INTERNAL		/* defs below are not for user consumption */

#include <cray/portdefs.h>

#ifndef _TRUE
#define _TRUE 1
#endif
#ifndef _FALSE
#define _FALSE 0
#endif

#define NO	0
#define NONE	0
#define YES	1	/* it is important that this is 1 */

/*
 * Change this anytime the format of the environment variables changes.
 */
#define ASG_VERSION 1

#if	defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
#define MAGIC_ID 0x2d464443	/* '-FDC' only 4 chars per int */
#else
#define MAGIC_ID '--*FDC--'
#endif

/*
 * The following is a macro to change a CAL symbol declaration into a
 * value usable by C.
 */
#define _VALUE(xtern)	((int)(&(xtern)))

/*
 * General directions
 *	These are the constants that are used to keep track of the state of
 *	the I/O.
 */
#define READIN	1	/* last op was READ */
#define WRITIN	2	/* last op was WRITE */
#define POSITIN	4	/* last op was POSITION */

/*
 * Record Translation classes
 *	Note: All of the conversion types for IBM and VMS can be grouped into
 *	two classes.  The Fixed length record type, and the Variable
 *	length record type.  These types differ within the broad
 *	group according to the existence of blocking, control words,
 *	and padding.
 *
 * When adding new classes, be sure to look in libu/ffio/fxrmisc.c and update
 *	ownopen[] and in fxrmain.c update _recfm_tab[]
 */
#define CLASS_END	0	/* End of chain marker. This is a dummy layer */
				/* that is always tacked on the end of spec. */
				/* It selects an appropriate handler. */
				/* depending on the device. */
#define CLASS_SYSCALL	1	/* handlers for UNIX/stream record model */
#define CLASS_NULL	2	/* explicit NULL layer, does nothing */
#define CLASS_SYSTEM	3	/* same as End of Chain, above */
#define CLASS_COS	4	/* COS blocking */
#define CLASS_BMX	5	/* TAPE handlers */
#define CLASS_F		6	/* FIXED length, no control words */
#define CLASS_V		7	/* variable length, control words */
				/* as prefixes only */
#define CLASS_TEXT	8	/* records terminated by 'special character' */
#define CLASS_X		9	/* records with control words as both */
				/* prefix and suffix on each record */
#define CLASS_CDC	10	/* CDC class */
#define CLASS_SDS	11	/* SDS handlers */
#define CLASS_MR	12	/* memory resident layer */
#define CLASS_TRACE	13	/* TRACE handlers.  Debug tool for 1 file */
#define CLASS_USER	14
#define CLASS_SITE	15
#define CLASS_ERROR	16	/* error layer */
#define CLASS_FD	17	/* user specified file descriptor */
#define CLASS_BLX	18	/* blank expand/compress layer */
#define CLASS_CACHE	19	/* cache layer */
#define CLASS_ER90B	20	/* ER90 byte-stream layer */
#define CLASS_BUFA	21	/* bufa layer */
#define CLASS_CACHEA	22	/* cachea layer */
#define CLASS_EVENT	23	/* event layer */
#define CLASS_LOCK	24	/* lock layer - CRI internal use */
#define CLASS_GLOBAL	25	/* global layer (mpp systems only) */
#define CLASS_F77	26	/* f77 layer */
#define CLASS_TMF	27	/* tmf layer */
#define CLASS_CMP	28	/* compression layer */

#define CLASS_USER0	30
#define CLASS_USER1	31
#define CLASS_USER2	32
#define CLASS_USER3	33
#define CLASS_USER4	34
#define CLASS_USER5	35
#define CLASS_USER6	36
#define CLASS_USER7	37
#define CLASS_USER8	38
#define CLASS_USER9	39

#define NUM_CLASSES	40

/*
 * Get definitions of error numbers.
 */
#include <liberrno.h>

/*
 * Status returns
 */
#define ERR	-1

/*
 * ===== Define operation(s) on bit pointers =====
 */

/*
 * typedefs
 */

/*
 * The bitptr is the basic pointer for use by the FDC routines.
 *
 * For CRAY-2 and CX/CEA machines, it is a word pointer shifted
 * left 6 bits, with the bit offset in the lower 6 bits.
 *
 * For the CRAY-T3D, CRAY-T3E, and MIPS 64-bit architectures it is a character
 * pointer shifted left 3 bits, with the bit offset in the lower 3 bits.
 *
 * For 32-bit architectures, it is a 64 bit word that is a 32 bit
 * character pointer shifted left 3 bits, with the bit offset in
 * the lower 3 bits.
 *
 * Note that LP is LONG and POINTER are 64 bits on some systems.
 */

#ifdef	__mips
typedef __int64_t bitptr;	
#elif	defined(_WORD32)
typedef long long bitptr;
#elif	defined(_LITTLE_ENDIAN) && defined(_LP64)
typedef long bitptr;
#else
typedef long bitptr;
#endif

/*
 * GET_BPTR()
 *	Get the raw value of a bit pointer into a word.
 */
#define GET_BPTR(value,bit_ptr)	(value) = (bit_ptr)

/*
 * SET_BPTR()
 *	Set a bit pointer to a value
 */
#define SET_BPTR(bit_ptr,value)	(bit_ptr) = (value)

/*
 * INC_BPTR()
 *	increment a bit pointer.
 */
#define INC_BPTR(bit_ptr,inc)	((bitptr)(bit_ptr) + (bitptr)(inc))

/*
 * SUBT_BPTR(bptr1, bptr2)
 *	Subtract bptr2 from bptr1 and return a number of bits
 */
#define SUBT_BPTR(bptr1, bptr2)	((bitptr)(bptr1) - (bitptr)(bptr2))

/*
 * BPTR2WP()
 *	Convert a bit pointer to a C word pointer
 *
 * 	Retain virtual region number in bits 63 to 61.
 */
#if	(defined(_LITTLE_ENDIAN) && defined(_LP64))
#define BPTR2WP(bptr)		((long *)(((((long)(bptr) << 3) >> 6) & 0x1ffffffffffffff8L) | ((long)(bptr) & 0xe000000000000000L)))
#elif	defined(_WORD32)
#define BPTR2WP(bptr)		((long *)(((long)((bitptr)(bptr) >> 3)) & ~0x3))
#elif	defined (_CRAYMPP) || (_MIPS_SZLONG == 64)
#define BPTR2WP(bptr)		((long *)(((long)(bptr) >> 3) & ~0x7))
#else
#define BPTR2WP(bptr)		((long *)((long)(bptr) >> 6))
#endif

/*
 * BPTR2CP()
 *	Convert a bit pointer to a C character pointer
 *
 * 	Retain virtual region number in bits 63 to 61.
 */
#if	(defined(_LITTLE_ENDIAN) && defined(_LP64))
#define BPTR2CP(bptr)		((char *)(((long)(((long)(bptr) << 3) >> 6) & 0x1fffffffffffffffL) | ((long)(bptr) & 0xe000000000000000L)))
#elif	defined (_CRAYMPP) || defined (_WORD32) || defined(__mips)
#define BPTR2CP(bptr)		((char *)((long)((bitptr)(bptr) >> 3)))
#else
#define BPTR2CP(bptr) \
	((char *)(_dshiftr((long)(bptr), (long)(bptr), 6) & 0xe3ffffffffffffff))
#endif

/*
 * WPTR2BP()
 *	Convert a C word pointer to a bit ptr
 *
 * 	Retain virtual region number in bits 63 to 61.
 */
#if	(defined(_LITTLE_ENDIAN) && defined(_LP64))
#define WPTR2BP(wp)		((long)(((((long)((char *)(wp))) & 0x03ffffffffffffffL) << 3) | ((long)((char *)(wp)) & 0xe000000000000000L)))
#elif	defined (_CRAYMPP) || defined (_WORD32) || defined(__mips)
#define WPTR2BP(wp)		(((bitptr)((long)((char *)(wp)))) << 3)
#else
#define WPTR2BP(wp)		((long)(((long)((char *)(wp))) << 6))
#endif

/*
 * CPTR2BP()
 *	Convert a C character pointer to a bit ptr
 *
 * 	Retain virtual region number in bits 63 to 61.
 */
#if	(defined(_LITTLE_ENDIAN) && defined(_LP64))
#define CPTR2BP(cptr)		((bitptr)(((((long)((char *)(cptr))) & 0x03ffffffffffffffL) << 3) | ((long)((char *)(cptr)) & 0xe000000000000000L)))
#elif	defined (_CRAYMPP) || defined (_WORD32) || defined(__mips)
#define CPTR2BP(cptr)		(((bitptr)(long)(char *)(cptr)) << 3)
#else
#define CPTR2BP(cptr)		((long)_dshiftl((long)(cptr), (long)(cptr), 6))
#endif

/*
 * BPTR2DWP()
 *      Convert a bit ptr to a 64-bit-word ptr
 *
 * 	Retain virtual region number in bits 63 to 61.
 */
#if	(defined(_LITTLE_ENDIAN) && defined(_LP64))
#define BPTR2DWP(bptr)           ((long *)(((((long)(bptr) << 3) >> 6) & ~0x7) | ((long)(bptr) & 0xe000000000000000L)))
#elif	defined(_WORD32)
#define BPTR2DWP(bptr)          ((long *)(((long)((bitptr)(bptr) >> 3)) & ~0x7))
#elif	defined (_CRAYMPP) || (_MIPS_SZLONG == 64)
#define BPTR2DWP(bptr)           ((long *)(((long)(bptr) >> 3) & ~0x7))
#else
#define BPTR2DWP(bptr)		((long *)((long)(bptr) >> 6))
#endif

/*
 * BPBITOFF(bitptr)
 *	Return the bit offset within the current word.
 */
#ifdef	_WORD32 
#define BPBITOFF(bptr)		(long) ((bitptr)(bptr) & 0x1fLL)
#else
#define BPBITOFF(bptr)		((long)(bptr) & 0x3f)
#endif

/*
 * BPDWBITOFF(bitptr)
 *	Return the bit offset within the current 64-bit word.
 */
#ifdef	_WORD32
#define BPDWBITOFF(bptr)          (long) ((bitptr)(bptr) & 0x3fLL)
#else
#define BPDWBITOFF(bptr)          ((long)(bptr) & 0x3f)
#endif

/*
 * NUMARG()
 *	Get the number of words passed in the argument list of the current
 *	function.
 */
#ifdef	_UNICOS
#define NUMARG(x)	x = _numargs()
#endif

/*
 * Get the IO info block for a file descriptor
 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#define GETIOB(x) (struct fdinfo *)(x)
#else
extern struct fdinfo *_cnvrt2fdinfo(int _fd);
#define GETIOB(x) _cnvrt2fdinfo(x)
#endif

/*
 * XRCALL is a simple macro that allows control of the mechanism that
 * handles the function pointer references.  It also allows us to
 * insert debugging code at build time.
 */
#if	defined(DEBUG) && !defined(__mips) && !defined(_LITTLE_ENDIAN)
#define XRCALL(fio,ptrname)	_xrtrace((fio), (fio)->xr.ptrname,#ptrname,
#else
#define XRCALL(fio,ptrname)	(fio)->xr.ptrname(
#endif
/*
 * Add check for fdinfo pointer.
 */
#define CHECK_FIOPTR(fio, stat)						\
		{							\
		if (fio == (struct fdinfo *)0L || fio == (struct fdinfo *)-1L || \
			fio->magic != MAGIC_ID)				\
			{						\
			errno = FDC_ERR_BADPTR;				\
			ERETURN(stat, FDC_ERR_BADPTR, 0);		\
			}						\
		}

/*
 * Debugging stuff
 */
#define FDCTRACE(bit) ((__fdctrace_enable & (bit)) != 0)

/*
 * bit defines for debug output
 */
#define TRACE_CALLS	0x001
#define TRACE_STATUS	0x002
#define TRACE_ASG	0x004
#define TRACE_OPEN	0x008
#define TRACE_COS	0x010
#define TRACE_SDSIO	0x020
#define TRACE_WA	0x040
#define TRACE_CACHE	0x080
#define TRACE_SYS	0x100

/*
 * IS_RESERVED_FC evaluates to:
 *	1 if the fffcntl function code is one reserved for site or user layers.
 *	0 otherwise.
 */
#define IS_RESERVED_FC(x)	(100 <= (x) && (x) <= 299)

/*
 * DUMP_IOB dumps out the IOB in human readable format for
 * debugging.  It is conditionally compiled and is a no-op
 * in normal use.
 */
#if	defined(DEBUG) && !defined(__mips) && !defined(_LITTLE_ENDIAN)
#define DUMP_IOB(fio)							\
	{								\
	if (FDCTRACE(TRACE_OPEN))					\
		{							\
		_xrt_putf("DUMP_IOB: fio=%o B %d - rfd=%o B %d, nfio=%o\n",\
			fio, fio, (fio)->realfd, (fio)->realfd,		\
			(fio)->fioptr);					\
		_xrt_putf("DUMP_IOB:\trtype=%d, mxr=%d, mbs=%d\n",	\
			(fio)->rtype,					\
			(fio)->maxrecsize, (fio)->maxblksize);		\
		_xrt_putf("DUMP_IOB:\tbase=%o, bufsize=%o\n",		\
			(fio)->_base, (fio)->_ffbufsiz);		\
		}							\
	}

#define GTRACEBACK()	TRBK()

#define EQ ==
#define NE !=
#define LT <
#define GT >
#define LE <=
#define GE >=
#define SANITYCHK(aarg,rel,bbarg)					\
		if (aarg rel bbarg)					\
			{						\
			_xrt_putf("SANITY CHECK: %s %s %s !!\n",	\
				#aarg, #rel, #bbarg);			\
			_xrt_putf("%s=%d %16x, %s=%d %16x\n",		\
				#aarg,aarg,aarg,#bbarg,bbarg,bbarg);	\
			GTRACEBACK();					\
			exit(1);					\
			}

#define BUFCHK(fio,bits)						\
			{						\
			char *BCXbuflim, *BCXcurdat, *BCXptr, *BCXbase;	\
									\
			BCXbase = BPTR2CP((fio)->_base);		\
			BCXptr = BPTR2CP((fio)->_ptr);			\
			SANITYCHK(BCXptr,LT,BCXbase);			\
			BCXbuflim = BPTR2CP((fio)->_base) + (fio)->_ffbufsiz; \
			BCXcurdat = BPTR2CP((fio)->_ptr) + ((bits) >> 3); \
			SANITYCHK(BCXcurdat,GT,BCXbuflim);		\
			}
			
#else
#define DUMP_IOB(fio)		/* no-op */
#define SANITYCHK(a,rel,b)	/* no-op */
#define BUFCHK(fio,bits)	/* no-op */
#endif

/*
 * READBLK()
 *	Read one block from the next lower layer.
 *	Data is read in at fio->_ptr.
 */
#define READBLK(retval,fio,nbytes,stat,fulp,ubc)			\
			{						\
			struct fdinfo *RBllfio;				\
									\
			SANITYCHK((nbytes),LT,0);			\
			RBllfio = (fio)->fioptr;			\
			retval = XRCALL(RBllfio,readrtn)		\
					(RBllfio), /* fd of nxt lvl */	\
					(fio)->_ptr,			\
					(nbytes),			\
					(stat),				\
					(fulp),				\
					(ubc));				\
			(fio)->_cnt = 0;				\
			if (retval > 0)					\
				{					\
				(fio)->_cnt = (retval << 3) - *(ubc);	\
				}					\
			}

/*
 * WRITEBLK()
 *	Write one block from the buffer to the next lower layer
 */
#define WRITEBLK(retval,fio,nbytes,stat,fulp,ubc)			\
			{						\
			struct fdinfo *WBllfio;				\
									\
			SANITYCHK((nbytes),LT,0);			\
			WBllfio = (fio)->fioptr;			\
			retval = XRCALL(WBllfio, writertn)		\
					(WBllfio),/* fd of nxt lvl */	\
					(fio)->_base, /* addr of buf */	\
					(nbytes),			\
					(stat),				\
					(fulp),				\
					(ubc));				\
			if ((retval) >= 0)				\
				{					\
				(fio)->_cnt = 0;			\
				(fio)->_ptr = (fio)->_base;		\
				(fio)->segbits = 0;			\
				}					\
			}

/*
 * SKIP_BITS()
 * on reading, skip the given number of bits by adjusting counters
 */
#define SKIP_BITS(xfio,xnbits)						\
				{					\
				register int SKBtemp;			\
				SKBtemp = (xnbits);			\
				SET_BPTR((xfio)->_ptr,			\
					INC_BPTR((xfio)->_ptr,SKBtemp)); \
				(xfio)->segbits -= SKBtemp;		\
				(xfio)->_cnt -= SKBtemp;		\
				}

/*
 * Get data from buffer and update pointers
 */
#define GETDATA(bufptr, xfio, xnbits)				\
			{					\
			register int GTDtemp;			\
			GTDtemp = (xnbits);			\
			BUFCHK((xfio),(GTDtemp));		\
			MOV_BITS((bufptr), (xfio)->_ptr, (GTDtemp)); \
			SKIP_BITS((xfio),(GTDtemp));		\
			}

/*
 * Put the data in the buffer from the UDA
 */
#define PUTDATA(bufptr, xfio, xnbits)				\
			{					\
			register long PTDtemp;			\
			PTDtemp = (xnbits);			\
			BUFCHK((xfio),(PTDtemp));		\
			MOV_BITS((xfio)->_ptr, (bufptr), (PTDtemp)); \
			SET_BPTR((xfio)->_ptr,			\
				INC_BPTR((xfio)->_ptr,(PTDtemp))); \
			(xfio)->segbits += (PTDtemp);		\
			(xfio)->_cnt += (PTDtemp);		\
			}

/*
 * Get a small number of bits where it is not justified to call MOVBIT
 * The resultant bits are left justified in the word.
 * No checking is done.	POINTERS AND COUNTS ARE NOT INCREMENTED
 * nbits MUST be <= 64.  When sizeof(long) == 64, 64 bits are always 
 * returned. When sizeof(long) == 32, 32 bits are always returned.
 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define GET_BITS(outword,bufptr,nbits)                          \
                {                                               \
                register int qzshft;                            \
                qzshft = BPBITOFF(bufptr);                      \
                SANITYCHK((nbits),GT,64);                       \
                SANITYCHK((nbits),LT,1);                        \
                if (((qzshft & 07)== 0) && (((nbits) & 07) == 0)) \
                        {                                       \
                        char *_op;                              \
                        char *_bp;                              \
                        int   _iz;                              \
                        _bp = BPTR2CP(bufptr);                  \
                        _op = (char *)&(outword);               \
                        (outword) = 0;                          \
                        for (_iz = 0; _iz < (nbits)/8; _iz++)   \
                                {                               \
                                *_op++ = *_bp++;                \
                                }                               \
                        }                                       \
                else                                            \
                        {                                       \
                        (outword) = _dshiftl(*BPTR2WP(bufptr),  \
                                        *(BPTR2WP(bufptr)+1),   \
                                        qzshft)                 \
                        & _mask(nbits); /* mask from left */    \
                        }					\
                }
#else
#define GET_BITS(outword,bufptr,nbits)				\
		{						\
		register int qzshft;				\
		qzshft = BPBITOFF(bufptr);			\
		SANITYCHK((nbits),GT,64);			\
		SANITYCHK((nbits),LT,1);			\
		(outword) = _dshiftl(*BPTR2WP(bufptr),		\
					*(BPTR2WP(bufptr)+1),	\
					qzshft)			\
			& _mask(nbits); /* mask from left */	\
		}
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define PUT_BITS(bufptr,idata,nbits)                                    \
		{                                                       \
		long P_Btmp1, P_Btmp2, *P_Bp;                           \
		int P_Boff;                                             \
		P_Boff = BPBITOFF(bufptr);                              \
		if (((P_Boff & 07) == 0) && (((nbits)&07) == 0))        \
			{						\
			char *P_Bz;                                     \
			int __i;					\
			P_Bz =  BPTR2CP(bufptr);                        \
			for (__i = 0; __i < (nbits)/8; __i++)           \
				*P_Bz++ = ((unsigned long)(idata) >> ((sizeof(long) -(__i+1))*8)) & 0xff; \
			}                                               \
                else                                                    \
			{						\
			P_Bp = BPTR2WP(bufptr);				\
			if ((P_Boff+(nbits)) <= _BITS_PER_LONG)		\
				{					\
				P_Btmp1 = 0;				\
				P_Btmp2 = 0;				\
				if (P_Boff != 0)			\
					P_Btmp1 = (unsigned long)*P_Bp >> \
					 (_BITS_PER_LONG - P_Boff);	\
				if ((P_Boff + (nbits)) != _BITS_PER_LONG) \
					P_Btmp2 = *P_Bp << (P_Boff + (nbits)); \
				P_Btmp1 = _dshiftl(P_Btmp1, idata, (nbits)); \
				*P_Bp = _dshiftl(P_Btmp1, P_Btmp2,	\
					_BITS_PER_LONG - (P_Boff + (nbits))); \
				}                                               \
			else                                                    \
				{                                               \
				P_Btmp1 = (unsigned long)*P_Bp >>               \
				 (_BITS_PER_LONG - P_Boff);                      \
				P_Btmp2 = *(P_Bp + 1) << (P_Boff+(nbits)-_BITS_PER_LONG);\
				*P_Bp = _dshiftl(P_Btmp1, (idata), _BITS_PER_LONG-P_Boff);\
				P_Btmp1 = _dshiftl(P_Btmp1, (idata), (nbits));  \
				*(P_Bp+1) = _dshiftr(P_Btmp1, P_Btmp2,          \
					P_Boff + (nbits) - _BITS_PER_LONG);\
				}                                               \
			}                                               	\
		}

#else
/*
 * PUT_BITS
 * Put a small number of bits (idata) in a specified place. (bufptr)
 * input data is left justified in the word.
 * This works only if the number of bits is less than 64.
 * No checking is done.	POINTERS AND COUNTS ARE NOT INCREMENTED
 * This means that it works to put a few bits in place where there
 * is surrounding data (backpatching)
 * For 64-bit architectures, we assume idata is a 64-bit word.
 * For 32-bit architectures, we assume idata is a 32-bit word.
 */
#define PUT_BITS(bufptr,idata,nbits)					\
		{							\
		long P_Btmp1, P_Btmp2, *P_Bp;				\
		int P_Boff;						\
		P_Boff = BPBITOFF(bufptr);				\
		P_Bp = BPTR2WP(bufptr);					\
		if ((P_Boff+(nbits)) <= _BITS_PER_LONG)			\
			{						\
			P_Btmp1 = 0;					\
			P_Btmp2 = 0;					\
			if (P_Boff != 0)				\
				P_Btmp1 = (unsigned long)*P_Bp >>	\
				 (_BITS_PER_LONG - P_Boff); 		\
			if ((P_Boff + (nbits)) != _BITS_PER_LONG)	\
				P_Btmp2 = *P_Bp << (P_Boff + (nbits));	\
			P_Btmp1 = _dshiftl(P_Btmp1, idata, (nbits));	\
			*P_Bp = _dshiftl(P_Btmp1, P_Btmp2,		\
				_BITS_PER_LONG - (P_Boff + (nbits)));	\
			}						\
		else							\
			{						\
			P_Btmp1 = (unsigned long)*P_Bp >>		\
			 (_BITS_PER_LONG - P_Boff);			\
			P_Btmp2 = *(P_Bp + 1) << (P_Boff+(nbits)-_BITS_PER_LONG);\
			*P_Bp = _dshiftl(P_Btmp1, (idata), _BITS_PER_LONG-P_Boff);\
			P_Btmp1 = _dshiftl(P_Btmp1, (idata), (nbits));	\
			*(P_Bp+1) = _dshiftr(P_Btmp1, P_Btmp2,		\
					P_Boff + (nbits) - _BITS_PER_LONG);\
			}						\
		}
#endif

/*
 * MOV_BITS
 *	Move the specified bits from a_a to b_b.
 */
#if	defined(__mips) || defined(_SOLARIS) || defined(_LITTLE_ENDIAN)
#define MOVBIT movbit_
#define MOVBITZ movbitz_
#endif

#ifdef	_ABSOFT
#define MOVBIT movbit
#define MOVBITZ movbitz
#endif

extern void _memwcpy(long *_s, long *_s0, int _n);

#if	defined(__mips)
#define _MOVEM(to_b, frm_a, nbits)					\
			{						\
			long long	M_aoff, M_boff;			\
			M_aoff = BPBITOFF(frm_a);			\
			M_boff = BPBITOFF(to_b);			\
			if (((M_aoff & 07) | (M_boff & 07) |		\
					(nbits & 07)) == 0 )		\
				{					\
				memcpy(BPTR2CP(to_b), BPTR2CP(frm_a), \
				(nbits) / 8);				\
				}					\
			else						\
				{					\
				long long	M_Bits;			\
									\
				M_Bits = (nbits);			\
				M_aoff = BPBITOFF(frm_a);		\
				M_boff = BPBITOFF(to_b);		\
				SANITYCHK(M_aoff,GT,_BITS_PER_LONG)	\
				SANITYCHK(M_aoff,GT,_BITS_PER_LONG)	\
				MOVBITZ(BPTR2WP(frm_a), &M_aoff,	\
					&M_Bits, BPTR2WP(to_b), &M_boff);\
				}					\
			}
#elif	defined(_LITTLE_ENDIAN)
#define _MOVEM(to_b, frm_a, nbits)					\
			{						\
			int64		M_aoff, M_boff;			\
			M_aoff = BPBITOFF(frm_a);			\
			M_boff = BPBITOFF(to_b);			\
			if (((M_aoff & 07) | (M_boff & 07) |		\
					(nbits & 07)) == 0 )		\
				{					\
				memcpy(BPTR2CP(to_b), BPTR2CP(frm_a), \
				(nbits) / 8);				\
				}					\
			else						\
				{					\
				int64		M_Bits;			\
									\
				M_Bits = (nbits);			\
				M_aoff = BPBITOFF(frm_a);		\
				M_boff = BPBITOFF(to_b);		\
				SANITYCHK(M_aoff,GT,_BITS_PER_LONG)	\
				SANITYCHK(M_aoff,GT,_BITS_PER_LONG)	\
				MOVBITZ(BPTR2WP(frm_a), &M_aoff,	\
					&M_Bits, BPTR2WP(to_b), &M_boff);\
				}					\
			}
#else
#define _MOVEM(to_b, frm_a, nbits)					\
			{						\
			if ((BPBITOFF(frm_a) | BPBITOFF(to_b) |		\
					(nbits & 077)) == 0 )		\
				{					\
				_memwcpy(BPTR2WP(to_b), BPTR2WP(frm_a), \
				(nbits) / _BITS_PER_LONG);		\
				}					\
			else						\
				{					\
				long	M_aoff, M_boff, M_Bits;		\
									\
				M_Bits = (nbits);			\
				M_aoff = BPBITOFF(frm_a);		\
				M_boff = BPBITOFF(to_b);		\
				SANITYCHK(M_aoff,GT,_BITS_PER_LONG)	\
				SANITYCHK(M_aoff,GT,_BITS_PER_LONG)	\
				MOVBITZ(BPTR2WP(frm_a), &M_aoff,	\
					&M_Bits, BPTR2WP(to_b), &M_boff);\
				}					\
			}
#endif

#define MOV_BITS(to_b,frm_a,bits)					\
			{						\
			SANITYCHK(BPTR2WP(frm_a),LE,(long *)100)	\
			SANITYCHK(BPTR2WP(to_b),LE,(long *)100)		\
			_MOVEM(to_b, frm_a, bits);			\
			}

/*
 * Clear the status word.  These macros clear the iosw and ffsw structures.
 * Note that only the first three words of the ffsw are cleared.  This is
 * because the fourth word is new in UNICOS 7.0, and for safety is not used.
 * On machines where _ADDR64 is true (and MIPS), more space is cleared.
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CLRSTAT(io_sw)		{*(int *)&(io_sw) = 0; io_sw.sw_count=0;}
#else
#define CLRSTAT(io_sw)		*(long *)&(io_sw) = 0
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
/* Clear through sw_sptr */
#define CLRFFSTAT(io_sw)						\
			{						\
			long	*_ptr;					\
			_ptr	= (long *)&(io_sw);			\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			}
#elif	defined(_ADDR64) 
#define CLRFFSTAT(io_sw)						\
			{						\
			long	*_ptr;					\
			_ptr	= (long *)&(io_sw);			\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			}
#else
#define CLRFFSTAT(io_sw)						\
			{						\
			long	*_ptr;					\
			_ptr	= (long *)&(io_sw);			\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			*_ptr++	= 0;					\
			}
#endif

#if	defined(DEBUG) && !defined(__mips) && !defined(_LITTLE_ENDIAN)
#define _STATPRINT(trcvar, nstat)					\
			{						\
			if (FDCTRACE(TRACE_STATUS))			\
				{					\
				char _str[128];				\
									\
				sprintf(_str,				\
					"SETSTAT: %d, at line number %d in file %s\n", \
						nstat, __LINE__, __FILE__); \
				_xrt_putstr(_str);			\
				}					\
			}

#define _ERRPRINT(trcvar, errnum)					\
			{						\
			if (FDCTRACE(TRACE_STATUS))			\
				{					\
				char _str[128];				\
									\
				sprintf(_str,				\
					"SETERROR: %d, at line number %d in file %s\n", \
						errnum, __LINE__, __FILE__); \
				_xrt_putstr(_str);			\
				}					\
			}
#else
#define _STATPRINT(trcvar, nstat)
#define _ERRPRINT(trcvar, errnum)
#endif

/*
 * Set the status field in the status struct.
 */
#define _SETSTAT(io_sw,nstat,nbytes)	{				\
				(io_sw)->sw_flag = 1;			\
				(io_sw)->sw_error = 0;			\
				(io_sw)->sw_count = (nbytes);		\
				(io_sw)->sw_stat = (nstat);		\
				}

/*
 * Set the error status.  Always print the error in DEBUG mode
 */
#define _SETERROR(io_sw,errnum,nbytes)	{				\
				(io_sw)->sw_flag = 1;			\
				(io_sw)->sw_error = (errnum);		\
				(io_sw)->sw_count = (nbytes);		\
				FFSTAT(*io_sw) = FFERR;			\
				_ERRPRINT(TRACE_STATUS,errnum);		\
				}

#define SETSTAT(io_sw,nstat,nbytes)	{				\
				_SETSTAT(io_sw,nstat,nbytes);		\
				_STATPRINT(__fdctrace_enable,nstat);	\
				}

/*
 * Set the status and return an error.  Always print the status in DEBUG mode
 */
#define ERETURN(io_sw,errnum,nbytes)	{				\
					_SETERROR(io_sw,errnum,nbytes);	\
					return(ERR);			\
					}

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#define _FFOPEN_ERETURN(_a,_b,_c) ERETURN((_a),(_b),(_c))
#define _FFOPEN_ERR ERR
#else
#define _FFOPEN_ERR (_ffopen_t) -1L
#define _FFOPEN_ERETURN(_a,_b,_c) { \
        _SETERROR((_a),(_b),(_c)); \
        return(_FFOPEN_ERR); }
#endif

/*
 * These are the 'generic' SCC codes.  They are the ones that
 * are used internally and translated into those for IBM and VMS
 * Having the FULL code =0 has the advantage if no SCCs need to be
 * processed.
 */
#define SCCUNKN -1	/* This does not translate into a 'real' SCC code */
#define SCCFULL 0
#define SCCFRST 1
#define SCCLAST 2
#define SCCMIDL 3

/*
 * segment control code definitions fr all classes
 */
#define SCC_IBM		0	/* IBM type Segment Control code */
#define SCC_VMS		1	/* VMS type Segment Control code */
#define SCC_CDC		2	/* CDC type Segment Control code */
#define SCC_NVE		3	/* CDC type Segment Control code */

/*
 * Number of SCC types.  This is for the scc table dimension
 */
#define NUM_SCC_TYPES	4	/* IBM, VMS and CDC, and NOS/VE*/

/*
 * error codes for FDC parameters while parsing the -F assign attribute.
 */
#define FDC_PERR_CLASS	1
#define FDC_PERR_RFC	2
#define FDC_PERR_RFCS	3
#define FDC_PERR_RSZ	4
#define FDC_PERR_MBS	5
#define FDC_PERR_MIN	6
#define FDC_PERR_MAX	7
#define FDC_PERR_INC	8
#define FDC_PERR_BAD	9

#define NUM_PERRS	9

/*
 * fddcvrt_u: foreign dataset data conversion descriptor word.
 *	This word contains fields that describe the users
 *	request for data conversion.
 *
 *
 *  0                                                              64
 * /--------+--------+--------+----------------------------------------\
 * |vers    | bin    | char   |                                        |
 * |info    | cvrt   | set    |          unused     ...(40 bits)       |
 * | 8 bits | 8 bits | 8 bits |                                        |
 * \--------+--------+--------+----------------------------------------/
 *  ^        ^        ^
 *  zero     ^        ^
 *  for      ^        ^
 *  now      ^        ^
 *           ^        ^
 *           ^        ^
 *           ^        ^
 *           ^        ^
 *           ^        ^
 *           ^        1=ascii
 *           ^        2=ebcdic
 *           ^        etc...
 *           ^
 *           2=binary - IBM
 *           3=binary - CDC
 *           etc..
 */
union fddcvrt_u
	{
	struct
		{
		int	vers:8;		/* version ID, zero for now */
		int	cvrt:8;		/* numeric conversion code */
		int	cvch:8;		/* character conversion code */
		int	cvch_opt:8;	/* text record options */
		int	unused:32;
		} fld;
	unsigned long	wword;
	};

/*
 * spec_u:
 *
 *	FFIO specification word.  A sequence of one or more of consecutive
 *	spec_u words is used to pass FFIO layer parameters to any layer's
 *	open routine.
 *
 *	The first 8 bits of each spec_u word contain the extension bit and the
 *	class.  The extension bit is set in all but the last spec_u word
 *	in a spec_u word sequence.
 *
 *	The remainder of each word can contain different info depending on
 *	class.
 *
 *	Before changing spec_u, be sure the new structure will maintain
 *	compatibility between new versions of the asgcmd command and old
 *	libraries.
 *
 *	Starting in UNICOS 7.0, a "standard default sequence" of spec_u words
 *	is used for most layers.  The sequence generated by layer specification
 *
 *			layer.str1.str2:num1:num2:num3:...:numN
 *
 *	is:
 *
 * Word 0 (hdr):
 * -------------
 *  0                                                                   64
 * /-+-+-----+--------+----+----+--------------------+--------------------\
 * |e| |class| str1   |str2|          unused                              |
 * |x| |     |        |    |                                              |
 * |t| |6-bit| 8-bits |4bit|          44-bits                             |
 * \-+-+-----+--------+----+----+--------------------+--------------------/
 *
 * Words 1..N (info):
 * ------------------
 *  0                                                                   64
 * /-+-+-----+--------+----+----+--------------------+--------------------\
 * |e|v|class|                        value                               |
 * |x|a|     |                                                            |
 * |t|l|6-bit|                        56-bits                             |
 * \-+-+-----+--------+----+----+--------------------+--------------------/
 *
 *
 * Spec_u word format used for SDS and MR layers:
 *
 * Word 0 (fss):
 * -------------
 *  0                                                                   64
 * /-+-+-----+--------+----+----+--------------------+--------------------\
 * |e| |class| recfmt |styp|    |                min                      |
 * |x| |     |        |    |    |                                         |
 * |t| |6-bit| 8-bits |4bit|    |              40-bits                    |
 * \-+-+-----+--------+----+----+--------------------+--------------------/
 *
 *
 * Spec_u word 0 format used for compatibility between asgcmd and old libraries:
 *
 * Word 0 (fld):
 * -------------
 *  0                                                                   64
 * /-+-+-----+--------+----+----+--------------------+--------------------\
 * |e| |class| recfmt |styp|    |     recsize        |     mbs            |
 * |x| |     |        |    |    |                    |                    |
 * |t| |6-bit| 8-bits |4bit|    |     20-bits        |     20-bits        |
 * \-+-+-----+--------+----+----+--------------------+--------------------/
 *
 * Spec_u word 0 format used by new assign parser
 *
 * Word 0 (fld):
 * -------------
 *  0                                                                   64
 * /-+-+-----+--------+----+----+--------------------+--------------------\
 * |e| |class| info        |     recsize        |     mbs            |
 * |x| |     |             |    |                    |                    |
 * |t| |6-bit| 12-bits     |    |     20-bits        |     20-bits        |
 * \-+-+-----+--------+----+----+--------------------+--------------------/
 */
#if	defined(_CRAY) || defined(__mips) || defined(_LITTLE_ENDIAN)
/*
   #ifndef	uint64
   #define uint64 unsigned long long
   #endif
   #ifndef	int64
   #define int64 long long
   #endif
*/
typedef uint64 _spect;	/* 64-bit unsigned integer */
#else
typedef unsigned _spect;
#endif
union spec_u
	{
	struct
		{
		_spect ext	:1;	/* ext bit is on if not end of spec */
		_spect		:1;
#ifndef	__cplusplus
		_spect class	:6;	/* ffio CLASS */
#else
		_spect classsp	:6;	/* ffio CLASS */
#endif
		_spect str1	:8;	/* string parameter 1 */
		_spect str2	:4;	/* string parameter 2 */
		} hdr;
	struct
		{
		_spect ext	:1;	/* set to 1 if not end of spec */
		_spect valid	:1;	/* set to 1 if this param was passed */
#ifndef	__cplusplus
		_spect class	:6;	/* ffio CLASS */
#else
		_spect classsp :6;	/* ffio CLASS */
#endif
#if	defined(_WORD32) && !defined(__mips) && !defined(_LITTLE_ENDIAN)
		unsigned quan;		/* numeric parameter value */
#else
		_spect quan	:56;	/* numeric parameter value */
#endif
		} info;
	struct
		{
		_spect ext	:1;	/* ext bit is on if not end of spec */
		_spect		:1;
#ifndef	__cplusplus
		_spect class	:6;	/* ffio CLASS */
#else
		_spect classsp	:6;	/* ffio CLASS */
#endif
		_spect recfmt	:8;	/* This is the record format */
		_spect subtype	:4;	/* record format subtype. */
#if	defined(_WORD32)  && !defined(__mips) && !defined(_LITTLE_ENDIAN)
		unsigned min;
#else
		_spect		:4;
		_spect min	:40;
#endif
		} fss;
	struct
		{
		_spect ext	:1;	/* ext bit is on if not end of spec */
		_spect unused1	:1;
#ifndef	__cplusplus
		_spect class	:6;	/* ffio CLASS */
#else
		_spect cpasssp	:6;	/* ffio CLASS */
#endif
		_spect recfmt	:8;	/* This is the record format */
		_spect subtype	:4;	/* record format subtype. */
		_spect		:4;
		_spect recsize	:20;
		_spect mbs	:20;
		} fld;
	struct
		{
		_spect ext	:1;	/* ext bit is on if not end of spec */
		_spect unused1	:1;
#ifndef	__cplusplus
		_spect class	:6;	/* ffio CLASS */
#else
		_spect cpasssp	:6;	/* ffio CLASS */
#endif
		_spect info1	:24;    /* This contains non-numeric info */
		_spect info2	:32;    /* This contains more non-numeric info */
		} class_info;
#if	defined(_CRAY) || defined(__mips) || defined(_LITTLE_ENDIAN)
	_spect wword;	
#elif	defined(_WORD32)
	unsigned long long wword;
#else
#error "THIS CASE IS NOT HANDLED"
#endif
	};

/*
 * Find the next start of spec..
 *	Skip the extension words.
 */
#define NEXT_SPEC(nxtspec)	{ nxtspec = _next_spec(nxtspec); }
#include <cray/mtlock.h>

/*
 *	Global open information packet.  This structure contains information
 *	intended to be shared by all FFIO layers in a chain.
 */
struct gl_o_inf {
	int		version;	/* version number for gl_o_inf	*/
					/* structure.  Is 0 in 8.1+ libraries */
	enum {
	  OT_FORTRAN=1,
	  OT_USER1=2,
	  OT_USER2=4,
	  OT_USER3=8,
	  OT_USER4=16,
	  OT_USER5=32,
	  OT_USER6=64,
	  OT_USER7=128,
	  OT_USER8=256,
	  OT_USER9=512,
	  OT_SSIO=1024
	}		open_type;	/* code for type of open	*/

	union {
	 struct {			/* for Fortran files ...	*/
	  unum_t	unum;		/* unit number (-1 if unknown)	*/
	  unsigned	is_seq:1;	/* 1 iff access='sequential'	*/
	  unsigned	is_fmt:1;	/* 1 iff form='formatted'	*/
	  long		reclen;		/* 0 or RECL= value		*/
	 } fort;	
	 int		pad_a[15];	/* pad for future expansion	*/
	} u;

	unsigned	alreadyopen:1;	/* 1 iff file is already opened and */
					/* "fd" contains file descriptor.   */
					/* Used only by _gsys_open and 	    */
					/* _bmx_open.			    */


	int		fd;		/* file descriptor of already-open  */
					/* tape file.			    */

	int		depth;		/* FFIO layer depth (1 == top level)*/

	struct assign_info_s
			*aip;		/* assign info, or NULL	*/

	int		pad_b[4];	/* pad for future expansion */

	int64		user[2];	/* reserved for user layers */
	int64		site[2];	/* reserved for site layers */

};

/*
 * This structure defines the function pointers that comprise the standard
 * interface that is used between layers of the FDC process.
 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
typedef struct fdinfo * _ffopen_t;
typedef off_t		_ffseek_t;
#else
typedef int		_ffopen_t;
typedef int		_ffseek_t;
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define	___(A) A
#else
#define	___(A) ()
#endif

struct xtr_s
	{

	/* Specific open processing for the file type */
	_ffopen_t (* openrtn) ___((const char *_Name, int _Flags, mode_t _Mode, struct fdinfo *_Fio, union spec_u *_Spec, struct ffsw *_Retstat, long _Cbits, int _Cblks, struct gl_o_inf *_Oinf));

	/* Read one block/record of data packed */
	ssize_t (* readrtn) ___((struct fdinfo *_Fio, bitptr _Bufptr, size_t _Nbytes, struct ffsw *_Retstat, int _Fulp, int *_Ubc));

	/* Read one block/record of data async */
	ssize_t (* readartn) ___((struct fdinfo *_Fio, bitptr _Bufptr, size_t _Nbytes, struct ffsw *_Retstat, int _Fulp, int *_Ubc));

	/* Read one block/record of data 1 char/wd */
	ssize_t (* readcrtn) ___((struct fdinfo *fio, bitptr _Bufptr, size_t _Nbytes, struct ffsw *_Restat, int _Fulp));

	/* Write one block/record of data packed */
	ssize_t (* writertn) ___((struct fdinfo *_Fio, bitptr _Bufptr, size_t _Nbytes, struct ffsw *_Retstat, int _Fulp, int *_Ubc));

	/* Write one block/record of data async */
	ssize_t (* writeartn) ___((struct fdinfo *_Fio, bitptr _Bufptr, size_t _Nbytes, struct ffsw *_Retstat, int _Fulp, int *_Ubc));

	/* Write one block/record of data 1 char/wd */
	ssize_t (* writecrtn) ___((struct fdinfo *_Fio, bitptr _Bufptr, size_t _Nbytes, struct ffsw *_Restat, int _Fulp));

	/* Do any special cleanup and close file */
	int (* closertn) ___((struct fdinfo *_Fio, struct ffsw *_Retstat));

	/* Flush the buffers */
	int (* flushrtn) ___((struct fdinfo *_Fio, struct ffsw *_Retstat));

	/* endfile routine, not always meaningful */
	int (* weofrtn) ___((struct fdinfo *_Fio, struct ffsw *_Retstat));

	/* end of data routine "the REAL end" */
	int (* weodrtn) ___((struct fdinfo *_Fio, struct ffsw *_Retstat));

	/* seek on foreign file?? */
	_ffseek_t (* seekrtn) ___((struct fdinfo *_Fio, off_t _Pos, int whence, struct ffsw *_Retstat));

	/* backspace routine */
	int (* backrtn) ___((struct fdinfo *_Fio, struct ffsw *_Retstat));

	/* positioning routine */
	_ffseek_t (* posrtn) ___((struct fdinfo *_Fio, int _Cmd, void *_Arg, int _Len, struct ffsw *_Stat));

	/* listio routine */
	int (* listiortn) ___((int _Cmd, struct fflistreq *_List, int _Nent, struct ffsw *_Stat)); 

	int (* fcntlrtn) ___((struct fdinfo *_Fio, int _Cmd, void *_Arg, struct ffsw *_Stat));	/* fcntl routine */
	};

/* 
 * Remove definition of ___
 */
#undef	___

/*
 * the fdinfo block is a sort of 'DSP' for FDC.  It contains all of the
 * state information used by the various routines.
 */
struct fdinfo
	{
	long	magic;		/* magic number for ID */
	int	realfd;		/* Our real file descriptor number */
				/* this is only used by layers that have */
				/* either private implementations, or */
				/* 'real' file descriptors. */
	struct fdinfo *fioptr;	/* Pointer to fdinfo block for next */
				/* lower level. */
	struct fdinfo *parptr;	/* Pointer to fdinfo block for parent layer */

	struct stat *lstp;	/* Pointer to result of last stat/ffstat call */
				/* If null, none has been done */
	long	opn_flags;	/* Flags used in open call */
/*
 *	info about blocks and records
 */
	int	subtype;	/* record subtype (block type for CDC) */
	int64	maxrecsize;	/* max record size in BITS */
	int64	maxblksize;	/* Maximum Block Size (MBS) in BITS */
#ifndef	__cplusplus
	int	class;		/* record translation class */
#else
	int	classfdinfo;	/* record translation class */
#endif
	int	rtype;		/* Record type, as defined above */
/*
 *	Counters.  These are the counts of data and such in the buffer(s).
 *	Note:  All counts are kept and passed in bits.
 */
	int64	recbits;	/* total number of bits in logical record */
				/* This is only valid after a partial read */
				/* or write.  It is reset at each EOR/EOF */
	long	segbits;	/* number of bits left in segment */
				/* This always indicates the amount */
				/* of 'clear' data to be processed. */
				/* on write, this is the number of bits */
				/* already in the buffer, on read, the */
				/* number left in the buffer */
	int	scc;		/* segment control code */
	int	lastscc;	/* segment control code for prev. segment. */
	int64	last_recbits;	/* record length of last record in BITS */
				/* This is used differently in various layers */
/*
 *	Buffer info
 */
	bitptr	_base;		/* BIT pointer */
	bitptr	_ptr;		/* BIT pointer to next data in buffer */
	long	_cnt;		/* BIT count. Bits left in buffer on read*/
				/*	      Bits accumulated on write */
	long	_ffbufsiz;	/* BIT size of buffer */

/*
 *	Flags
 */
	int rwflag;		/* 4 for position, 2 for write, 1 for read */
				/* This tells what the last operation was, */
				/* nothing about open mode(s) */
	unsigned atbod:1;	/* _TRUE if at beginning of data */
	unsigned ateor:1;	/* _TRUE if at end of record */
	unsigned ateof:1;	/* _TRUE if EOF was last op (read or write) */
	unsigned ateod:1;	/* _TRUE if EOD was last op (read or write) */
	unsigned can_listio:1;	/* _TRUE if layer can handle listio */
	unsigned lock:1;	/* _TRUE if ffxxx routines need to lock */
	unsigned reg_lock:1;	/* _TRUE if ffxxx rtns use regular locks */
	unsigned free_lock:1;	/* _TRUE if a lock layer needs to free */
	plock_t *lock_word;	/* layers request that we use their lock */
	int	rsv6;		/* reserved for CRI */
/*
 *	Function pointers (!)
 */
	struct xtr_s xr;	/* pointers to routines that do the work. */
/*
 *	record type specific information
 */
	void	*lyr_info;	/* private layer info block ptr */
	};

/* extern declarations */

extern int __fdctrace_enable;	/* debugging flags */

extern int _scc_tab[NUM_SCC_TYPES][4];

/*
 *	These prototypes are for internal functions which a user may not
 *	use by default.
 */
#if	defined(__mips) || defined(_CRAY) || defined(_LITTLE_ENDIAN)
typedef int64 _nparmtype;	/* type of numeric parameters */
#else
typedef int _nparmtype;
#endif
extern _nparmtype _ff_nparm_getv (union spec_u *spec, int parnum, int *isvalid);
extern int _ff_nparm_getcnt (union spec_u *spec);
extern union spec_u *_next_spec(union spec_u *sp);

extern _ffopen_t _ffopen (const char *name, int flags, mode_t mode,
	union spec_u *spek, struct ffsw *stat, long cbits, int cblks,
	struct fdinfo *pa_fio, struct gl_o_inf *oinf);

extern int _ffopensg(const char *name, int flags, mode_t mode,
	long cbits, int cblks, struct ffsw *stat,
	const char *cspec, struct gl_o_inf *oinf);

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
extern int _ff_fdinfo_to_int(_ffopen_t fd, struct ffsw *pstat);
#else
#define	_ff_fdinfo_to_int(fd, pstat)	((int)(fd))
#endif

#endif	/* _LIB_INTERNAL */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
__BEGIN_DECLS
extern int ffassign (char *cmd);
extern int ffbksp (int _Fd);
extern int ffclose (int _Fd);
extern ssize_t ffread (int _Fd, void *_Buf, size_t _Nb);
extern ssize_t ffwrite (int _Fd, const void *_Buf, size_t _Nb );
extern off_t ffseek (int _Fd, off_t _Pos, int _Whence );
extern int ffweof (int _Fd);
extern int ffweod (int _Fd);
extern int ffopens (const char *_Name, int _Oflag, mode_t _Mode, long _Cbits,
	int _Cblks, struct ffsw *_Stat, const char *_Str);
extern int fffcntl (int _Fd, int _Cmd, void *_Arg, struct ffsw *_Stat);
extern off_t ffpos (int _Fd, int _Cmd, void *_Arg, int _Len,
	struct ffsw *_Stat);
extern int ffflush (int _Fd, struct ffsw *_Stat);
extern char *ffstrerror(int _Errnum);
__END_DECLS
#else
#include <cray/clibinc_config.h>
#if	!defined(_CRAYT3E) && _CRAYLIBS_RELEASE < 3000
#define _USE_OLD_PROTOTYPE 1
#endif
__BEGIN_DECLS
extern int ffbksp (int _Fd, ...);
extern int ffclose (int _Fd, ...);
extern int fffcntl (int _Fd, int _Cmd, ...);
extern int ffflush (int _Fd, ...);
extern int fflistio (int _Cmd, struct fflistreq *_List, int _Nent, ...);
extern int ffopens (const char *_Name, int _Oflag, int _Mode, long _Cbits,
	... );
extern int ffpos (int _Fd, int _Cmd, long *_Arg, int _Len,
	struct ffsw *_Stat);
extern char *ffstrerror(int _Errnum);
#if	_USE_OLD_PROTOTYPE
extern int ffread (int _Fd, char *_Buf, int _Nb, ...);
#else
extern int ffread (int _Fd, void *_Buf, size_t _Nb, ...);
#endif
extern int ffreada (int _Fd, char *_Buf, int _Nb, struct ffsw *_Stat, ...);
extern int ffreadc (int _Fd, char *_Buf, int _Nb, ...);
#if	_USE_OLD_PROTOTYPE
extern int ffseek (int _Fd, int _Pos, int _Whence, ...);
#else
extern int ffseek (int _Fd, off_t _Pos, int _Whence, ...);
#endif
extern int ffsetsp (int _Fd, ...);
extern int ffweod (int _Fd, ...);
extern int ffweof (int _Fd, ...);
#if	_USE_OLD_PROTOTYPE
extern int ffwrite (int _Fd, char *_Buf, int _Nb, ...);
#else
extern int ffwrite (int _Fd, const void *_Buf, size_t _Nb, ...);
#endif
extern int ffwritea (int _Fd, char *_Buf, int _Nb, struct ffsw *_Stat, ...);
extern int ffwritec (int _Fd, char *_Buf, int _Nb, ...);
__END_DECLS
#endif
__BEGIN_DECLS
extern int ffbkspf (int _Fd, struct ffsw *_Stat);
extern int ffopen (const char *_Name, int _Oflag, ...);
extern int ffopenf (const char *_Name, int _Oflag, mode_t _Mode, long _Cbits,
 int _Cblks, struct ffsw *_Pstat);
extern int ffiolock (int _fd, struct ffsw *_Stat);
extern int ffiounlock (int _fd, struct ffsw *_Stat);
extern int ffclosef (int _Fd, struct ffsw *_Stat);
extern int ffweoff (int _Fd, struct ffsw *_Stat);
extern int ffweodf (int _Fd, struct ffsw *_Stat);
extern off_t ffseekf (int _Fd, off_t _Pos, int _Whence, struct ffsw *_Stat);
extern ssize_t ffreadf (int _Fd, void *_Buf, size_t _Nb, struct ffsw *_stat, int _Fulp, int *_Ubc);

extern ssize_t ffwritef (int _Fd, const void *_Buf, size_t _Nb, struct ffsw *_stat, int _Fulp, int *_Ubc);
__END_DECLS

extern int __ffflush(struct fdinfo *, struct ffsw *);
extern int _fdc_unpackc(void *, long *, long, int, int);
extern int _fdc_packc(char *pbuf, long *ubuf, long count, int ctype);
extern int __ffclose(struct fdinfo *fio, struct ffsw *pstat);

#endif /* !_FFIO_H */
