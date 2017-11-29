/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* USMID @(#) libf/include/fio.h	92.6	08/02/99 10:40:42 */

#ifndef _FIO_H
#define _FIO_H

#include "fstats.h"
#include <errno.h>
#include <ffio.h>
#include <fortran.h>
#ifdef	_UNICOS
#include <procstat.h>
#endif
#include <stdio.h>
#include <sys/types.h>
#if	defined(_UNICOS) || defined(_SOLARIS)
#include <sys/iosw.h>
#endif
#include <cray/assign.h>
#include <cray/fndc.h>
#include <cray/format.h>
#include <cray/fortio.h>	/* For unum_t and recn_t */
#include <cray/mtlock.h>
#include <cray/dopevec.h>
#include <cray/nassert.h>
#include <cray/portdefs.h>
#ifdef	_CRAYMPP
#include <signal.h>
#endif

/************************************************************************
 *
 * Debug control
 *
 ***********************************************************************/

#ifdef	LIBDEBUG
#define _ASSERT_ON	1		/* turn on assertion checking */
#define DEBUG_MTIO	1		/* unconditional use of locks */
#endif

#define	PRINT_TIP(tipa) {	\
	fprintf(stderr, "tip address = %o\n", tipa);		\
	fprintf(stderr, "  type90 = %d\n", (tipa)->type90);	\
	fprintf(stderr, "  type77 = %d\n", (tipa)->type77);	\
	fprintf(stderr, "  intlen = %d\n", (tipa)->intlen);	\
	fprintf(stderr, "  extlen = %d\n", (tipa)->extlen);	\
	fprintf(stderr, " cnvindx = %d\n", (tipa)->cnvindx);	\
	fprintf(stderr, "   count = %ld\n", (tipa)->count);	\
	fprintf(stderr, "  stride = %ld\n", (tipa)->stride);	\
	fprintf(stderr, "  elsize = %d\n", (tipa)->elsize);	\
	if ((tipa)->cnvindx) {					\
		fprintf(stderr, " newfunc = %d\n", (tipa)->newfunc);\
		fprintf(stderr, " cnvtype = %d\n", (tipa)->cnvtype);\
		fprintf(stderr, " cnvfunc = %o\n", (tipa)->cnvfunc);\
	}							\
}
 
/************************************************************************
 *
 * Constants
 *
 ***********************************************************************/
 
#define HASH_SIZE	256	/* must be a power of 2 */

#ifdef KEY
#define STDIN_U		5	/* Special stdin unit */
#define STDOUT_U	6	/* Special stdout unit */
#define STDERR_U	0	/* Special stderr unit */
#else
#define STDIN_U		100	/* Special stdin unit */
#define STDOUT_U	101	/* Special stdout unit */
#define STDERR_U	102	/* Special stderr unit */
#endif

#ifdef KEY
#define	RECMAX		10240   /* Default (initial) size of line buffer */
#else
#define	RECMAX		1024	/* Default (initial) size of line buffer */
#endif
#define RECMAXLDO	133	/* List-directed output line length */
 
#define	ERROR	1
#define	OK	0
#define	YES	1
#define	NO	0
 
#define WRITE	1	/* write */
#define READ	2	/* read */
#define SEQ	3	/* sequential access */
#define DIR	4	/* direct access */
#define FMT	5	/* formatted */
#define UNF	6	/* unformatted */
#define EXT	7	/* external */
#define INT	8	/* internal */

/*
 *	Types of I/O statements
 */
		/* flags folded into the statement code */

#define TF_WRITE	001	/* write statement */
#define TF_READ		002	/* read statement */
#define TF_POS		004	/* statement which might reposition a file */
#define TF_FMT		010	/* formatted read or write statement */

		/* statement codes */

#define T_WSF		(00100 | TF_WRITE | TF_POS | TF_FMT		)
#define T_WSU		(00200 | TF_WRITE | TF_POS			)
#define T_WDF		(00300 | TF_WRITE | TF_POS | TF_FMT		)
#define T_WDU		(00400 | TF_WRITE | TF_POS			)
#define T_WLIST		(00500 | TF_WRITE | TF_POS			)
#define T_WNL		(00600 | TF_WRITE | TF_POS			)

#define T_RSF		(00700 | TF_READ  | TF_POS | TF_FMT		)
#define T_RSU		(01100 | TF_READ  | TF_POS			)
#define T_RDF		(01200 | TF_READ  | TF_POS | TF_FMT		)
#define T_RDU		(01300 | TF_READ  | TF_POS			)
#define T_RLIST		(01400 | TF_READ  | TF_POS			)
#define T_RNL		(01500 | TF_READ  | TF_POS			)

#define	T_BUFOUT	(01600		  | TF_POS			)
#define	T_BUFIN		(01700		  | TF_POS			)

#define T_OPEN		(02000						)
#define T_REWIND	(02100		  | TF_POS			)
#define T_BACKSPACE	(02200		  | TF_POS			)
#define T_ENDFILE	(02300		  | TF_POS			)
#define T_CLOSE 	(02400						)
#define T_INQF		(02500						)
#define T_INQU		(02600						)

#define T_MISC		(02700		  | TF_POS			)

#define T_GETPOS	(03000						)
#define T_SETPOS	(03100		  | TF_POS			)
#define T_LENGTH	(03200						)
#define T_UNIT		(03300						)
#define T_TAPE		(03400		  | TF_POS			)
#define T_FLUSH		(03500						)
#define T_NUMBLKS	(03600						)

/*
 *	Fortran 77 data types 
 */

#define DT_NONE		0	/* Typeless				*/
#define DT_INT		1	/* Integer				*/
#define DT_REAL		2	/* Real					*/
#define DT_DBLE		3	/* Double				*/
#define DT_CMPLX	4	/* Complex				*/
#define DT_LOG		5	/* Logical				*/
#define DT_CHAR		6	/* Character				*/
#define DT_SINT		7	/* Short integer			*/
#define DT_DBLCOM	8	/* Double complex (intended for fortran
				   90 support, but never used)		*/
#define DT_MAX		9	/* Number of data types			*/

/*
 *	Fortran I/O buffer size constants.
 */

#define BLKSIZE		4096	/* Bytes in a 512-word disk block	*/
#define SECTOR		BLKSIZE	/* Temporary synonym for BLKSIZE	*/
#ifdef	_UNICOS
#define DEF_BIN_BS	_VALUE(_def_bin_bs)
#define DEF_SBIN_BS	_VALUE(_def_sbin_bs)
#else
#define DEF_BIN_BS	1
#define DEF_SBIN_BS	1
#endif

#ifdef	_CRAYMPP
/* Values used when running on mppsim in user virtual mode */
#define DEF_BINSIM_BS	1
#define DEF_SBINSIM_BS	1
#endif

#define DFBUFSZ		8	/* Direct formatted buffer size		*/
#define SFBUFSZ		8	/* Sequential formatted buffer size	*/
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
				/* We choose this direct unformatted */
				/* buffer size to match MIPS f77 */
#define DUBUFSZ		16	/* Direct unformatted buffer size	*/
#else
#define DUBUFSZ		8	/* Direct unformatted buffer size	*/
#endif
#define SUBUFSZ		48	/* Sequential unformatted buffer size	*/

#define DEFAULT_NBUF	4	/* Default no. of direct-access buffers */

/* Temporary buffer size for packing/unpacking line buffers		*/

#ifdef	_MAXVL
#define	TBUFSZW		_MAXVL
#else
#define	TBUFSZW		36
#endif

#define	TBUFSZB	(TBUFSZW * sizeof(long))

#define CHBUFSIZE	(1024 * sizeof(long))	/* chunking buf size (bytes) */

/*
 *	CFT77 return status values (set in S3 on return)
 */

#define	IO_OKAY	0	/* Normal completion	*/
#define	IO_ERR	1	/* Error status		*/
#define	IO_END	2	/* End status		*/

/*
 *	Fortran I/O completion status
 */

#define CNT	1	/* Count exhausted 		*/
#define EOR	0	/* End-of-record 	 	*/
#ifndef EOF
#define EOF 	-1	/* End-of-file			*/
#endif
#define EOD 	-2	/* End-of-data			*/

#define IOERR	-1	/* Fortran I/O error		*/

/*
 *	Fortran I/O request modes
 */

#define PARTIAL	0	/* Partial record I/O		*/
#define FULL	1	/* Full record I/O	 	*/

/*
 *	Codes for open processing
 */

enum status_spec	{ OS_UNKNOWN = 1, OS_OLD, OS_NEW, OS_SCRATCH,
			  OS_REPLACE };
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
enum access_spec	{ OS_SEQUENTIAL = 1, OS_DIRECT, OS_OAPPEND, OS_KEYED };
#else
enum access_spec	{ OS_SEQUENTIAL = 1, OS_DIRECT };
#endif
enum form_spec		{ OS_FORMATTED = 1, OS_UNFORMATTED, OS_BINARY, OS_SYSTEM };
enum blank_spec		{ OS_NULL = 1, OS_ZERO };
enum position_spec	{ OS_REWIND = 1, OS_ASIS, OS_APPEND };
enum action_spec	{ OS_ACTION_UNSPECIFIED = 0, OS_READ = 1, OS_WRITE = 2,
			  OS_READWRITE = (OS_READ | OS_WRITE) };
enum delim_spec		{ OS_NONE = 1, OS_QUOTE, OS_APOSTROPHE };
enum pad_spec		{ OS_NO = 1, OS_YES };

/*
 *	Codes for close processing
 */

#define CLST_UNSPEC	0	/* STATUS= unspecified		*/
#define CLST_KEEP	1	/* STATUS='KEEP'		*/
#define CLST_DELETE	2	/* STATUS='DELETE'		*/

/************************************************************************
 *
 * Constants assigned to unit table fields.
 *
 ***********************************************************************/
/*
 * The following flags in 'uflag' are used within a READ/WRITE statement
 * and are cleared at finalization.
 */

#define _UERRF	 01		/* ERR= specified */
#define _UEORF	 02		/* EOR= specified */
#define _UENDF	 04		/* END= specified */
#define _UIOSTF 010		/* IOSTAT= specified */

#define _UERRC	020		/* ERR condition */
#define _UEORC	040		/* EOR condition with ADVANCE='NO' */
#define _UENDC 0100		/* ENDFILE condition */

/************************************************************************
 *
 * The CVOLATILE macro must be defined to expand to "volatile" on 
 * architectures like CRAY TS which support threading and where storing to 
 * memory might render another processor's cache invalid.
 *
 * On other cache-coherent architectures like Sparc we expand CVOLATILE to
 * "volatile" for conceptual correctness, and because there is no negative
 * performance impact.
 *
 * CVOLATILE is not expanded to "volatile" on T3D systems because no 
 * threading is supported and there would be a negative performance impact.
 *
 *
 * This keyword must be added to any globally accessed field or variable which 
 * might be loaded prior to obtaining a lock (lock routines always render
 * the cache coherent) and the field or dataword is not read-only.  This 
 * comes up for locked data areas which are dynamically allocated on the 
 * heap and are then initialized once and read-only from thence onward.  The 
 * fact that the data item previously was in the heap means that some processor
 * might have previously used the data area and then deallocated it with
 * free(3).  That processor might have a stale data cache entry assigned
 * to the data word.
 *
 ***********************************************************************/

#ifndef	_CRAYT3D

#define	CVOLATILE volatile

#else

#define	CVOLATILE 

#endif

/************************************************************************
 *
 * Structure and Typedef Definitions
 *
 ***********************************************************************/

typedef short		s_flag;
typedef long		ftnlen;
typedef	_f_comp8	_gen_complex;	/* Complex of largest supported kind */

/* FP is a union which may contain a variety of pointer types */

typedef union {
	FILE		*std;
	struct fdinfo	*fdc;
} FP;

/*
 *	Define the basic, or smallest, container for a noncharacter datum.
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
typedef _f_int1 bcont;		/* basic container is a half word */
#elif	!defined(_WORD32) && (defined(_F_INT4) || defined(_F_REAL4))
typedef short bcont;		/* basic container is a half word */
#else
typedef long bcont;		/* basic container is a word */
#endif

/*
 *	Fortran unit structure
 *
 *	UNIT_HEADER is the offset to the start of the part of the unit table 
 *	which is initialized by _init_unit().
 */

#define	UNIT_HEADER	(offsetof(unit, auxlockp))

typedef struct unit_s {

/******************************************************************************
 *									      *
 *	Hash Table Fields Section					      *
 *									      *
 *	Fields used to manage the hash table of units.			      *
 *									      *
 *	Fields in this section must be at the start of the structure.	      *
 *	This is to help _init_unit() efficiently initialize all the other     *
 *	fields.   All these fields (except uiolock) must never be reset	      *
 *	because they are needed to maintain a coherent hash table of units.   *
 *									      *
 *	The CVOLATILE keyword is added to hashlink, uid, private, and utid    *
 *	because these fields are loaded by _get_cup() prior to entering a     *
 *	critical region.						      *
 *									      *
 ******************************************************************************/

	struct unit_s * CVOLATILE hashlink;	/* Next unit in hash chain    */
	CVOLATILE unum_t	uid;		/* Unit number, -1 if internal*/
						/* unit			      */
	CVOLATILE int		private;	/* 1 if private, 0 if global  */
	CVOLATILE int		utid;		/* Tskid of owner of priv unit*/
	plock_t	uiolock;			/* Unit lock		      */

/******************************************************************************
 *									      *
 *	Connection Properties Section					      *
 *									      *
 *	Fields describing unchanging properties of the connection.	      *
 *									      *
 ******************************************************************************/

	plock_t	*auxlockp;	/* Pointer to optional 2nd lock (stdio)	*/
	int	ufs;		/* Describes the I/O processing method	*/
	char	*ufnm;		/* File name or alias			*/
	ino_t	uinode;		/* Unique file inode identifier		*/
	dev_t	udevice;	/* Unique file device number		*/
	char	*alfnm;		/* Actual file name			*/
	long 	urecl;		/* RECL value; 0 if absent on sequential*/
	int	usysfd;		/* File descriptor.  A value of -1 	*/
				/* usually indicates no 'normal' file 	*/
				/* is associated with the unit.		*/
	long	uflagword;	/* Returned flag word from FC_GETINFO	*/
	int	ucharset;	/* Foreign character set		*/
	int	unumcvrt;	/* Foreign numeric conversion		*/
	int	ualignmask;	/* 0 or __fndc_align[cup->unumcvrt]->gran - 1*/
	struct _dal_s	ualign;	/* FDC alignment information		*/

	unsigned
		uostatus:3,	/* STATUS value (enum status_specifier) */
		uposition:3,	/* POSITION value (enum position_specifier) */
		uaction :3,	/* ACTION value (enum action_specifier)	*/
		udelim  :3,	/* DELIM value (enum delim_spec)	*/
		upad    :3,	/* PAD value (enum pad_specifier)	*/
		utrunc	:1,	/* 1 if trunc. after sequential write	*/
		ubmx	:1,	/* 1 if -s bmx selected by user		*/
		usysread:1,	/* 1 if file has system read permission */
		usyswrite:1,	/* 1 if file has system write permission*/
		useek	:1,	/* 1 if backspace or use direct access	*/
		ublkd	:1,	/* 1 if record blocking is present	*/
	 	ublnk	:1,	/* 1 if BLANK='ZERO' on open		*/
		ufmt	:1,	/* 1 if FORM='FORMATTED'		*/
		useq	:1,	/* 1 if ACCESS='SEQUENTIAL'		*/
	 	uscrtch	:1,	/* 1 if file is a scratch or temp file	*/
	 	unlinked:1,	/* 1 if file has been unlinked		*/
	 	usnglink:1,	/* 1 if file is not a linked file	*/
		umultfil:1,	/* 1 if multiple endfile records allowed*/
		uft90	:1,	/* 0/1 if Fortran 77/90 compatible	*/
		umultup :1,	/* 1 if -m on permits multiple access	*/
		utmpfil :1,	/* 1 if assigned with -t		*/
		ok_wr_seq_fmt:1,/* 1 if seq fmt write is supported	*/
		ok_wr_seq_unf:1,/* 1 if seq unfmted write is supported	*/
		ok_wr_dir_fmt:1,/* 1 if dir fmt write is supported	*/
		ok_wr_dir_unf:1,/* 1 if dir unfmted write is supported	*/
		ok_rd_seq_fmt:1,/* 1 if seq fmt read is supported	*/
		ok_rd_seq_unf:1,/* 1 if seq unfmted read is supported	*/
		ok_rd_dir_fmt:1,/* 1 if dir fmt read is supported	*/
		ok_rd_dir_unf:1,/* 1 if dir unfmted read is supported	*/
		ufcompat:3,	/* 0/1/2/3 if cf77/cf90/irxf77/irxf90 compat */
		ufcomsep:1,	/* 1 no comma-separated list-dir output */
		ufunilist:1,	/* 1 if not unicos list-dir output	*/
		ufcomplen:1,	/* 1 if compressed leng list-dir output */
		ufrptcnt:1,	/* 1 no repeat count in list-dir output */
		ufnl_skip:1,	/* 1 if skipping mismatched nl grp name */
		ufnegzero:1,	/* 1 if skip minus sign in fmt write of -0.0 */
		ukeyed	:1,	/* 1 if ACCESS='KEYED'			*/
		ubinary	:1,	/* 1 if FORM='BINARY'			*/
		usystem	:1;	/* 1 if FORM='SYSTEM'			*/

/******************************************************************************
 *									      *
 *	Unit State Section						      *
 *									      *
 *	Fields which contain state information which must be remembered	      *
 *	from one I/O statement to the next.				      *
 *									      *
 ******************************************************************************/

	FP	ufp;		/* Low-level file pointer		*/

	struct repdata *urepdata;/* Repdata structure for list read	*/

	int	upfcstsz;	/* Size of parsed format count stack	*/
	int	*upfcstk;	/* Pointer to parsed format count stack	*/

	unsigned
		unitchk :1,	/* 1 if UNIT was called since last BUFFER I/O */
		urecmode:1,	/* FULL or PARTIAL record mode for buffer I/O */
		uerr	:1,	/* 1 if error on last buffer in/out	*/
	 	uwrt	:1,	/* 1 if last io was write		*/
		pnonadv	:1,	/* 1 if ADVANCE='NO' on prev I/O	*/
		uspcproc:1;	/* 1 if in EOV special processing (tapes) */

	/*
	 * The uend field is used to track a file's position relative to
	 * a final endfile record.  A 'logical endfile' record exists at
	 * the end of any sequential file which does not have a physical
	 * endfile preceding the EOD.
	 */

	enum {
	    BEFORE_ENDFILE   = 0, 
	    PHYSICAL_ENDFILE = 1, 
	    LOGICAL_ENDFILE  = 2
	} 	uend	:3;	/* 0 if not positioned past a final 	*
				 * endfile record.  PHYSICAL_ENDFILE if *
				 * positioned after a logical endfile 	*
				 * record.  LOGICAL_ENDFILE if 		*
				 * positioned after a logical endfile 	*
				 * record.				*/

	long	uwaddr;		/* Current word address for -s bin	*/

	int64	ulrecl;		/* Length in bits of previous BUFFER IN,*
				 * BUFFER OUT, READ, or WRITE.	Used for*
				 * the LENGTH function.			*/
	enum {
	    ASYNC_NOTOK  = 0,
	    ASYNC_OK     = 1,
	    ASYNC_ACTIVE = 2
	}	uasync;		/* > 0 if async permitted or active	*/

	struct ffsw uffsw;	/* Status word for asynchronous I/O	*/

	union stat_ntry *ftstat;/* Pointer to Fortran statistics packet */

	long	ufbitpos;	/* Bit position in file for PURE files	*/

	/* Direct access */

	recn_t	udamax;		/* Largest record number on file	*/
	recn_t	udalast;	/* Last record number read or written	*/

	/* Formatted, list-directed, or namelist I/O */

/******************************************************************************
 *	Line buffer conventions	(disregard at your own peril):		      *
 *									      *
 *	ulinebuf	Set in OPEN/CLOSE processing and--possibly--during    *
 *			I/O statement initialization.			      *
 *									      *
 *	ulineptr	Pointer to current position in line buffer.	      *
 *									      *
 *	uflshptr	Pointer to unflushed part of line buffer.  This is    *
 *			used only for sequential writes.	              *
 *									      *
 *	ulinecnt	On output (writing), this is the offset or current    *
 *			position in the line buffer; note that ulineptr is    *
 *			normally defined as &ulinebuf[ulinecnt].  On input    *
 *			(reading), this is the number of characters left in   *
 *			the line buffer.				      *
 *									      *
 *	ulinemax	On output (writing), this is the highwater mark in    *
 *			the line buffer (e.g., on a tab-left operation,       *
 *			ulineptr and ulinecnt are updated but ulinemax is     *
 *			unchanged).  Note that for output paths that do not   *
 *			support tabbing (list-directed output, for example),  *
 *			ulinemax is used in lieu of ulinecnt, and ulinecnt    *
 *			is undefined.  This field is never used on input      *
 *			(reading).					      *
 *									      *
 *	urecsize	Minimum physical size of line buffer and record	size  *
 *			(RECL) of current record.  ulinemax must NEVER be     *
 *			larger than this value;	ulinecnt may exceed it after  *
 *			a tab-right operation.				      *
 *									      *
 *	uldwsize	Analogue of urecsize for list-directed writes only.   *
 *									      *
 *	unmlsize	Analogue of urecsize for namelist writes only.	      *
 *									      *
 ******************************************************************************/

	long	*ulinebuf;	/* Unpacked line buffer			*/
	long	*ulineptr;	/* Current position in line buffer	*/
	long	*uflshptr;	/* Unflushed part of line buffer	*/
	long	ulinemax;	/* Max position in line buffer to write */
	long	ulinecnt;	/* Character count in line buffer	*/
	long	urecsize;	/* Length of line buffer (characters)	*/
	long	uldwsize;	/* Length for list-directed writes	*/
	long	unmlsize;	/* Length for namelist writes		*/

/******************************************************************************
 *									      *
 *	Statement State Section						      *
 *									      *
 *	Fields which contain the state of the current I/O statement.	      *
 *									      *
 ******************************************************************************/

	_f_int 	*uiostat;	/* IOSTAT parameter address for cf77	*/
	long	uflag;		/* Flag word used by interface routines */
	unsigned
		ueor_found:1,	/* Found eor already on read		*/
		f_lastwritten:1;	/* 1 if we already terminated	*/
					/* the record			*/
				/* Only those routines that care about	*/
				/* lastiolist currently look at this	*/
	void	*f_lastiolist;	/* Set in some of the compiler-library	*/
				/* interface routines.  If		*/
				/* iolist_header->iollast is set, this	*/
				/* can be set to the address of the end	*/
				/* of the iolist; otherwise NULL.	*/

	/* Unformatted I/O */

	int64	urecpos;	/* Bit position in current record	*/
	int	ulastyp;	/* Type of previous I/O list item	*/

} unit;

/*
 *	Fortran unit hash table structure.  A hash table consists of an
 *	array of these structures.  The units in each hash chain are
 *	linked together via the "hashlink" field in the unit table.
 *
 */
typedef CVOLATILE struct {
	unit	*ulist;	/* Unit at head of hash chain	*/
} unit_htable;

/*
 *	The per-task I/O statement state structure
 */

struct fiostate {
	unit	*f_cu;		/* Current unit				*/
	long	f_iostmt;	/* Current I/O statement type		*/
	unum_t	f_curun;	/* Current unit number			*/
	s_flag	f_intflg;	/* 1 if internal file			*/
	long	f_rtbgn;	/* RT value at start of I/O statement	*/
	unsigned f_shrdput:1;	/* 1 if we need to do a shmem_wait_put	*/
				/* Should be set only on MPP systems	*/

	union iostate {
	/*
	 * Unformatted I/O
	 */
	struct unfstate {
	int	recpos;		/* Bit position in current record	*/
	int	lastyp;		/* Type of previous I/O list item	*/
	} unf;

	/*
	 * Formatted/list-directed I/O
	 */
	struct fmtstate {
	int	(*endrec)(
			struct fiostate *css,
			unit		*cup,
			int		count);
				/* Record processing function		*/

	long	*leftablim;	/* Left tab limit for current statement	*/
	char	*icp;		/* Internal I/O character pointer	*/
	char	*tempicp;	/* Ptr to a free-able copy of internal file */
	int	icl;		/* Internal I/O character length	*/
	int	iiae;		/* Number of internal array elements	*/
				/* Negative if pre-5.0 code		*/
				/* Zero or greater if 5.0 code or later	*/

	unsigned
		freefmtbuf:1,	/* 1 if temporary unparsed format to be freed */
		freepfmt:1,	/* 1 if parsed format to be freed	*/
		lcomma	:1,	/* List-directed read comma flag	*/
		blank0	:1,	/* 1 if blanks currently treated as nulls */
		cplus	:1,	/* 1 if + sign is printed for pos numbers */
		nonl	:1,	/* 1 if no new line, 0 if newline	*/
		nonadv	:1,	/* 1 if ADVANCE='NO'			*/
		slash	:1;	/* 1 if list input encountered a slash	*/

	union {

	struct {
		char	*fmtbuf;	/* Pointer to unparsed (ASCII) format */
		int	fmtcol;		/* Current position in unparsed format*/
		int	fmtlen;		/* Length of unparsed format	*/
		int	fmtnum;		/* Fortran statement label of format */
		fmt_type *pfmt;		/* Pointer to parsed format	*/
		fmt_type *pfcp;		/* Parsed format current position */
		int	*pftocs;	/* Current top of count stack	*/

		int	charcnt;	/* Character count for SIZE on read */
		long	scale;		/* Current scale factor		*/
	} fe; /* formatted editing */

	/*
	 * The "le" structure contains state of the current list-directed
	 * output statement.  repcnt, value/copy, elsize, and type all
	 * are used to save the last output value processed.   We 
	 * defer printing of output items until we know that we have 
	 * no more opportunity to combine consecutive equivalent values
	 * into one value with a repeat count.
	 *
	 * The ndchar bit is set if the last *printed* value was 
	 * non-delimited character.  This must be remembered because it 
	 * affects the choice of value separator used.
	 */
	struct {
	    union {
		long	value[4];	/* Repeated output value	*/
		void	*copy;		/* Pointer to copy of output value */
	    } u;
	    int		repcnt;		/* Number of repeated output values */
	    int		elsize;		/* Byte size of repeated value	*/
	    ftype_t	type	:8;	/* Data type of repeated value	*/
	    unsigned	ndchar	:1;	/* 1 if prev type was nondelim char */
	    unsigned	item1	:1;	/* 1 if first item in I/O list	*/
	    unsigned	ldwinit	:1;	/* 1 if first call to _ld_write */
	  } le; /* list-directed editing */

	} u;

	} fmt;
    } u;
};

/*	FIOSPTR - synonym for (struct fiostate *) */

typedef struct	fiostate	*FIOSPTR;

/*	olist - for open processing */

typedef struct {
	s_flag			oerr;
	_f_int			ounit;
	char			*ofile;
	ftnlen			ofilelen;
	enum status_spec	ostatus;
	enum access_spec	oaccess;
	enum form_spec		oform;
	_f_int			orecl;
	enum blank_spec		oblank;
	enum position_spec	oposition;
	enum action_spec	oaction; 
	enum delim_spec		odelim; 
	enum pad_spec		opad;
} olist;

/*	cllist - for close processing */
 
typedef struct {
	s_flag	cerr;
	_f_int	cunit;
	char	*csta;
} cllist;

/*	inlist - for INQUIRE processing	*/

typedef struct {
	s_flag	inerr;
	_f_int	inunit;
	char	*infile;
	ftnlen	infilen;
	_f_log	*inex;	/* parameters in standard's order */
	_f_log	*inopen;
	_f_int	*innum;
	_f_log	*innamed;
	char	*inname;
	ftnlen	innamlen;
	char	*inacc;
	ftnlen	inacclen;
	char	*inseq;
	ftnlen	inseqlen;
	char 	*indir;
	ftnlen	indirlen;
	char	*infmt;
	ftnlen	infmtlen;
	char	*inform;
	_f_int	informlen;
	char	*inunf;
	ftnlen	inunflen;
	_f_int	*inrecl;
	_f_int	*innrec;
	char	*inblank;
	ftnlen	inblanklen;
	char	*inposit;		/* POSITION specifier pointer	*/
	ftnlen	inpositlen; 		/* POSITION specifier length	*/
	char	*inaction;		/* ACTION specifier pointer	*/
	ftnlen	inactonlen;		/* ACTION specifier length	*/
	char	*inread;		/* READ specifier pointer	*/
	ftnlen	inreadlen;		/* READ specifier length	*/
	char	*inwrite; 		/* WRITE specifier pointer	*/
	ftnlen	inwritelen;		/* WRITE specifier length	*/
	char	*inredwrit;		/* READWRITE specifier pointer	*/
	ftnlen	inrdwrtlen;		/* READWRITE specifier length	*/
	char	*indelim;		/* DELIM specifier pointer	*/
	ftnlen	indelimlen;		/* DELIM specifier length	*/
	char	*inpad;			/* PAD specifier pointer	*/
	ftnlen	inpadlen;		/* PAD specifier length		*/
} inlist;

/*
 *	Type Information Packet
 *
 *	This data structure is used extensively in the data-transfer I/O
 *	calls.  It exists to consolidate the data type information and to
 *	reduce the number of parameters in the mid-level libf routines.
 *	It also contains information relevant to numeric data conversion.
 *
 *	N.B. type77 may not always be correct, especially when libf is
 *	entered through one of the f90 interface routines; in which case
 *	it will be set to -1.  However, the mid- and low-level routines
 *	should all be using the type90 field.
 */

typedef struct type_information_packet {
	ftype_t	type90;		/* Fortran 90/95 data type	*/
	short	type77;		/* Fortran 77 data type		*/
	short	intlen;		/* Internal data length (bits)	*/
	short	extlen;		/* External data length (bits)	*/
	short	cnvindx;	/* Data conversion index	*/
	long	count;		/* Number of data items		*/
	long	stride;		/* Stride between data items	*/

/*	Element size is the internal length multiplied by the 	*/
/*	number of elements (equivalent to intlen for everything	*/
/*	but CHARACTER data), though expressed in bytes.		*/

	long	elsize;		/* Element size (bytes)		*/

/*	The following fields are defined iff cnvindx != 0	*/

	short	newfunc;	/* 1 if new-style conv. func.	*/
	short	cnvtype;	/* Data conversion pseudo-type	*/
	int	(* cnvfunc)();	/* Data conversion function	*/
} type_packet;

/*
 *	The xfer_func typedef is a prototype for any functions called by
 *	_xfer_iolist.
 */

typedef int xfer_func(FIOSPTR css, unit *cup, void *dptr, type_packet *tip,
			int mode);

/*
 *	The xfer_func_c	typedef is a prototype for functions which process 
 *	input or output of contiguous data.
 */

typedef long xfer_func_c(unit *cup, void *uda, type_packet *tip, int mode, 
			 int *ubc_ret, long *wr, int *status);

/************************************************************************
 *
 * Error Handling Macros
 *
 *	RERROR (error_number);
 *	RERROR1(error_number, parameter);
 *
 *	These routines either terminate the program with an error, or if
 *	the user is processing errors, returns the error number.  Two
 *	versions of each routine exist to allow for a parameter to be
 *	substituted in the error message.
 *
 *	RERROR and RERROR1 should be used once the unit table pointer has
 *	been established.
 *
 *	GOERROR and GOERROR1 are used where a jump-to-label is desired upon
 *	reaching an error condition.  They set a special variable (errn) to the 
 *	return value and jump to a label.
 *
 ***********************************************************************/


#define ABORT_ON_ERROR	(cup == NULL || (cup->uflag & (_UERRF | _UIOSTF)) == 0)

#define RERROR(n) {			\
	if (ABORT_ON_ERROR)		\
		_ferr(css, n);		\
	else				\
		return(n);		\
}

#define RERROR1(n, p) { 		\
	if (ABORT_ON_ERROR)		\
		_ferr(css, n, p);	\
	else				\
		return(n);		\
}

#define GOERROR(err, label)	{ errn = err; goto label; }
#define GOERROR1(err, p, label)	{ errn = err; parm = p; goto label; }

/************************************************************************
 *
 * EOF Handling Macro
 *
 *	REND(error_number);
 *
 *	REND either terminates the program with an EOF-type error, or if
 *	the user is processing errors, returns the negative error number.
 *
 ***********************************************************************/
#define REND(n) {	\
	if ((cup == NULL) || (cup->uflag & (_UENDF | _UIOSTF)) == 0)	\
		_ferr(css, n);		\
	else				\
		return(n);		\
}

/************************************************************************
 *
 * Multitasking Macros
 *
 ***********************************************************************/

#ifdef	_UNICOS
#define	INITIALIZE_LOCK(x)	{ (x) = 0; }
#elif   defined(KEY) /* Bug 6003 */
#define	INITIALIZE_LOCK(x)	{ if (pthread_mutex_init) pthread_mutex_init(&(x), NULL); }
#elif	defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
#define INITIALIZE_LOCK(x)	{ (x) = 0; }
#elif	defined(_SOLARIS)
#define INITIALIZE_LOCK(x)	mutex_init(&(x), USYNC_THREAD, NULL)
#elif	defined(_LITTLE_ENDIAN) && !defined(__sv2)
#define INITIALIZE_LOCK(x)	{ (x) = 0; }
#endif

#define OPENLOCK()	MEM_LOCK(&_openlock)

#define OPENUNLOCK()	MEM_UNLOCK(&_openlock)

#define PARSELOCK()	MEM_LOCK(&_parselock)

#define PARSEUNLOCK()	MEM_UNLOCK(&_parselock)

/*
 * 	FLSH_MEM
 *		Ensures that any prior stores complete before any following 
 *		stores.
 *
 *		On PVP systems, the "suppress" ensures stores are issued and 
 *		the _cmr() Waits for this CPU's memory stores to complete.
 *
 *		On Sparc systems, the call to the _flsh_mem routine ensures
 *		that all prior stores are issued (not held in registers).
 *
 *		On other systems, threading is not supported, so FLSH_MEM
 *		may be a no-op.
 *		
 */

#ifdef	_CRAY1
#define FLSH_MEM()	{ _Pragma("suppress"); _cmr(); }
#elif	defined(_SOLARIS)
#define FLSH_MEM()	{ _flsh_mem(); }
#else
#define FLSH_MEM()	{ }
#endif

/************************************************************************
 *
 * General Macros
 *
 ***********************************************************************/

/*
 *	MAX and MIN 
 *		Prevent compiler warnings by removing any definition added
 *		previously by other header files.
 */
#undef MAX
#define MAX(a,b)	((a) > (b) ? (a) : (b))

#undef MIN
#define MIN(a,b)	((a) < (b) ? (a) : (b))

#define FF2FTNST(ffstat)	_ffstat_cnvt[ffstat]

/*
 *	WAITIO
 *		Wait for any outstanding I/O to complete.  If an error in
 *		the completed asynchronous request is found, "error_handle"
 *		is executed.
 */
#ifdef	DEBUG
#define AIOCHK(cup)	{ if (cup->ufs != FS_FDC) _ferr(NULL, FEINTUNK); }
#else
#define AIOCHK(cup)
#endif

#define MAXRECALL 1000000

#define WAITIO(cup, error_handle) {					\
/*									\
 *		If uasync is ASYNC_ACTIVE, then the last operation was	\
 *		asynchronous.  If the FFSTAT is zero, it has not yet	\
 *		completed.						\
 */									\
		if (cup->uasync == ASYNC_ACTIVE) {			\
			register int	ct = 0;				\
			struct ffsw	zzstat;				\
			struct fdinfo	*llfio;				\
									\
			llfio	= (struct fdinfo *)cup->ufp.fdc;	\
			AIOCHK(cup);	/* Do some DEBUG checking */	\
/*									\
 *			If I/O is still busy, go into recall until the	\
 * 			request has been completed.  If FFSTAT is != 0, \
 *			then we really called a synchronous routine,	\
 *			and need not call the recall routine at all.	\
 */									\
			while (FFSTAT(cup->uffsw) == 0) {		\
				(void)XRCALL(llfio, fcntlrtn) llfio,	\
					FC_RECALL, &cup->uffsw, &zzstat);\
									\
				if (ct++ > MAXRECALL) _ferr(NULL, FEINTUNK);\
			}						\
/*									\
 *			Set ulrecl					\
 */									\
			cup->ulrecl = (uint64)cup->uffsw.sw_count << 3;	\
			cup->ufbitpos += cup->ulrecl;	/* for PURE */	\
			if (cup->urecmode == PARTIAL) 			\
				cup->urecpos  += cup->ulrecl;		\
									\
			switch (FFSTAT(cup->uffsw)) {			\
			case FFEOR:					\
				cup->ulastyp	= DT_NONE;		\
				cup->urecpos	= 0;			\
			case FFCNT:	/* fall through ! */		\
				cup->uend	= BEFORE_ENDFILE;	\
				break;					\
			case FFEOF:					\
				cup->uend	= PHYSICAL_ENDFILE;	\
				break;					\
			case FFEOD:					\
				if (cup->uend == BEFORE_ENDFILE)	\
					cup->uend	= LOGICAL_ENDFILE;\
				break;					\
			}						\
									\
			cup->uasync	= ASYNC_OK;			\
/*									\
 *			If error, perform requested action.		\
 */									\
			if (cup->uffsw.sw_error != 0) {			\
				error_handle;				\
			}						\
		}							\
	}

/*
 * POWER_OF_TWO	Define a macro to determine if an integer is a power of
 *		two.  Use the _popcnt() intrinsic on CRAY PVP sytems.
 */

#ifdef	_CRAY1
#define	POWER_OF_TWO(n)	(_popcnt(n) == 1)
#else
#define	POWER_OF_TWO(n)	((n & (n - 1)) == 0 && n != 0)
#endif

/*
 * COMPADD 	Compute padding requirements for a particular file structure 
 *		and data conversion type.
 *
 * Parameters:
 *
 *	unit	*cup		- (input) unit pointer
 *	int	pbytes		- (output) number of pad bytes needed
 *	int	pbits		- (output) number of pad bits to subtract from
 *				  pbytes
 *	int	pval		- (output) the data area written out to
 *				  the file to fill the pad space. 
 *	
 */

#if	NUMERIC_DATA_CONVERSION_ENABLED
#define COMPADD(cup, pbytes, pbits, pval) {				\
	register int64	bitpos, bits, gran;				\
									\
	if (cup->ualign.pflag) {	/* If aligning */		\
		bitpos	= cup->urecpos;					\
		gran	= cup->ualign.gran;				\
/*									\
 *	Use bit masks rather than (slow) mod function if granularity is \
 *	a power of two.							\
 */									\
		if (POWER_OF_TWO(gran)) {				\
			bits	= gran - (bitpos & (gran - 1));		\
			bits	&= gran - 1;				\
		}							\
		else {							\
			bits	= gran - (bitpos % gran);		\
			bits	= bits % gran;				\
		}							\
		pbytes	= (bits + 7) >> 3;				\
		pbits	= (pbytes << 3) - bits;				\
		pval	= cup->ualign.padval;				\
	}								\
	else	/* No aligning */					\
		pbytes = pbits = pval = 0;				\
}
#else
#define COMPADD(cup, pbytes, pbits, pval)	pbytes = pbits = pval = 0;
#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */

/*
 * SET_F90_INFO	Construct a pseudo-f90/f95 type descriptor and type
 *		information packet for f77 entry points.
 *
 * Parameters:
 *
 *	struct f90_type	ts	f90/f95 type descriptor (output)
 *	type_packet	tip	Type information packet (output)
 *	short		type77	Fortran 77 data type (input)
 *	
 */

#define CREATE_F90_INFO(ts, tip, type77) {			\
	ts.type		= _f77_to_f90_type_cnvt[type77];	\
	ts.dpflag	= (type77 == DT_DBLE) ? 1 : 0;		\
	ts.int_len	= _f77_type_len[type77] << 3;		\
	ts.dec_len	= ts.int_len >> 3;			\
	if (type77 == DT_SINT) {				\
		ts.kind_or_star	= DVD_STAR;			\
		if (ts.dec_len == sizeof(_f_int))		\
			ts.dec_len	= ts.dec_len >> 1;	\
	}							\
	else							\
		ts.kind_or_star	= DVD_DEFAULT;			\
	tip.type77	= type77;				\
	tip.type90	= ts.type;				\
	tip.intlen	= ts.int_len;				\
	tip.extlen	= ts.int_len;				\
	tip.elsize	= ts.int_len >> 3;			\
	tip.stride	= 1;					\
	tip.cnvindx	= 0;					\
}

/*
 * GOOD_UNUM(u) - verifies unit numbers.  Returns 1 if the given
 *		  unit number could be connected.   
 */

#define GOOD_UNUM(u)	((u) >= 0)

/*
 * RSVD_UNUM(u) - returns 1 iff u is a reserved unit number which cannot
 *		  be opened by the user.
 */

#ifdef KEY /* Bug 6433 */
/*
 * Originally, "print"  and "write(*,...)" referred to STDOUT_U which was 101;
 * and similarly for "read (*,...)" (STDIN_U, 100) and "punch" (STDERR_U, 102).
 * These were "reserved units" which did not obey all of the normal Fortran
 * semantics (e.g. "open(100, file='xyz', status='unknown')" would fail, and
 * "inquire(100,exist=m)" would set m==.false.)
 *
 * An explicit reference to "6" in a "write" would implicitly open stdout,
 * and "5" in a "read" would implicitly open stdin (in contrast to any other
 * unit value "n", which would implicitly open "fort.n".) However, 5 and 6
 * were not "reserved" units, so they obeyed the normal Fortran semantics
 * if (for example) you opened and closed them in connection with named files.
 *
 * We wanted "print" and "read (*,...)" to use 5 and 6, not 100 and 101, so we
 * changed STDIN_U and STDOUT_U earlier in this source file. But we don't want
 * 5 and 6 to have the "reserved", nonstandard behavior that 100..102 used to
 * have, so RSVD_UNUM must return false, because there are no longer any
 * "reserved" units.
 */
#define RSVD_UNUM(_U)	(0)
#else /* KEY Bug 6433 */
#define RSVD_UNUM(_U)	((_U) >= STDIN_U && (_U) <= STDERR_U)
#endif /* KEY Bug 6433 */

/*
 * OPEN_UPTR	- returns 1 iff u points to a connected unit.  Returns 0
 *		  if u is unconnected.
 */

#define OPEN_UPTR(_U)	((_U) != NULL && (_U)->ufs != 0)

/*
 * UHASH	- computes the hash value for a unit number = x mod HASH_SIZE
 */

#define UHASH(x)	(x & (HASH_SIZE - 1))

/*
 * UNIT_NUM and GT_UNUM get the unit number for a given unit pointer.
 */

#define UNIT_NUM(_U)    ((_U)->uid)

#define GT_UNUM(_U, _N)	{ _N = UNIT_NUM(_U); }

/*
 * MYTASK expands to a unique task identification number.
 */

#ifdef	_CRAY1
#define MYTASK  (t_tid())
#elif	defined(_CRAYMPP)
#define MYTASK  (_my_pe())
#else
#define MYTASK  0
#endif

/*
 *	STMT_BEGIN
 *
 *	This macro stores information about the current I/O statement,
 *	and snaps the current RT clock value for statistics gathering.
 *
 *	Returned is a (locked) current unit pointer if the unit is connected.
 *	Null is returned if the unit number is invalid or not connected.
 *
 *	Arguments
 *
 *		unum_t	_UNUM		- (input) unit number
 *		int	_INTFLAG	- (input) 1 if internal file
 *		int	_STMTCODE	- (input) statement code
 *		long	*_CILIST	- (unused) cilist from Fortran 90
 *		FIOSPTR	_LOCFIOSP	- (input) FIOSPTR from Fortran 90
 *					  or passed in from caller.  If this
 *					  argument is NULL, task common
 *					  is used.
 *		unit	*_CUP		- (output) unit pointer
 */
#ifndef	_UNICOS
#define _rtc()	0
#endif
#ifdef	_CRAYMPP
/* These macros are used for STOP_ALL processing */
#define INCRINIO	{_infio++;}					
#define CHKSTOP		{						\
				_infio--;				\
				if (_needtostop){			\
					_f_stopsig(SIGBUFIO);		\
				}					\
			}
#else
#define INCRINIO	
#define CHKSTOP
#endif
#define STMT_BEGIN(_UNUM, _INTFLAG, _STMTCODE, _CILIST, _LOCFIOSP, _CUP) {\
		FIOSPTR fiosp;						\
									\
		assert( _CILIST == NULL );				\
		if (_LOCFIOSP != NULL)					\
			fiosp	= _LOCFIOSP;				\
		else							\
			GET_FIOS_PTR(fiosp);				\
		if (_INTFLAG)						\
			_CUP	= _get_int_cup(); /* internal file */	\
		else							\
			_CUP	= _get_cup(_UNUM); /* external file */	\
									\
		/* Set fields which are accessed by _fcontext(). */	\
									\
		INCRINIO;						\
		fiosp->f_cu	= _CUP;					\
		fiosp->f_curun	= _UNUM;				\
		fiosp->f_intflg	= _INTFLAG;				\
		fiosp->f_iostmt	= _STMTCODE;				\
									\
		if ((_STMTCODE) & TF_FMT) 				\
			fiosp->u.fmt.u.fe.fmtbuf	= NULL;		\
									\
		fiosp->f_rtbgn	= _rtc();	/* for statistics */	\
}

/*
 *	STMT_END
 *
 *	This macro processes the end of an I/O statement.  The unit is
 *	unlocked and procstat statistics are optionally posted if _CUP
 *	is non-null.
 * 
 *
 *	Parameters
 *
 *		unit	*_CUP		- (input) unit pointer
 *		int	_STATSCODE	- (input) statement code or TF_WRITE
 *					  or TF_READ for statistics gathering
 *		long	*_CILIST	- (unused) cilist from Fortran 90
 *		FIOSPTR	_LOCFIOSP	- (input) FIOSPTR from Fortran 90
 *					  or passed in from caller.  If this
 *					  argument is NULL, task common
 *					  is used.
 */
#define STMT_END(_CUP, _STATSCODE, _CILIST, _LOCFIOSP) {		\
		FIOSPTR fiosp;						\
									\
		assert( _CILIST == NULL );				\
		if (_LOCFIOSP != NULL)					\
			fiosp	= _LOCFIOSP;				\
		else							\
			GET_FIOS_PTR(fiosp);				\
		if ((_CUP) != NULL) {					\
			if (fiosp->f_iostmt & TF_POS)			\
				cup->uposition	= 0;			\
			FSTATS_POST(_CUP, _STATSCODE, fiosp);		\
			_release_cup(_CUP);	/* unlock the unit */	\
		}							\
		CHKSTOP							\
		fiosp->f_curun	= -1;					\
		fiosp->f_iostmt	= 0;					\
		fiosp->f_cu	= NULL;					\
}

/*
 *	CFT77_RETVAL
 *
 *	This macro returns the specified value to the CFT77 compiler-
 *	generated code at the end of a library I/O interface routine.
 */

#if	defined(_CRAYMPP) || !defined(_UNICOS)
#define CFT77_RETVAL(_VAL)	(_VAL)
#else	/* _CRAYMPP || ! _UNICOS */
#define CFT77_RETVAL(_VAL)	_sets3(_VAL)
#endif

/*
 *	GET_FIOS_PTR	Assign a pointer to the Fortran I/O Statement
 *			state structure in task common.
 */

#define GET_FIOS_PTR(_P) _P = &_tsk_fiostate;

/*
 * IO_TYPE	- macro to categorize a unit as to the type of Fortran I/O which
 *		  is permitted on it.
 *		  Returns FIO_SU, FIO_SF, FIO_DU, or FIO_DF.
 *
 *	_U	pointer to the unit table entry.
 */

#define IO_TYPE(_U) ((_U->useq) ?		\
		(_U->ufmt ? FIO_SF : FIO_SU):	\
		(_U->ufmt ? FIO_DF : FIO_DU))

/************************************************************************
 *
 * External function prototypes
 *
 ***********************************************************************/
extern unit	*_alloc_unit(unum_t unum, int private);
extern void	_fcleanup(void);
extern void	_fcontext(FIOSPTR fiosp);
extern void	_ferr(FIOSPTR fiosp, int _Errno, ...);
extern long	_frch(unit *_Cup, long *_Uda, long _Chars, int _Mode,
			long *_Status);
extern long	_fwch(unit *_Cup, long *_Uda, long _Chars, int _Mode);
extern unit	*_search_unit_list(unit *p, unum_t unum);
extern unit	*_get_next_unit(unit *p, int iflock, int iftask);
extern unit	*_implicit_open(int acc, int form, unum_t unum, int errf,
			int *errn);
extern unit	*_imp_open(struct fiostate *css, int acc, int form, unum_t unum,
			int errf, int *errn);
extern unit	*_imp_open77(struct fiostate *css, int acc, int form,
			unum_t unum, int errf, int *errn);
extern void	_initialize_fortran_io(void);
extern void	_init_unit(unit *cup);
extern unit	*_init_internal_unit(void);
extern int	_parse(FIOSPTR _Css, unit *_Cup, fmt_type **_Prsfmt);
extern int	_unit_bksp(unit *cup);
extern int	_unit_close(unit *cup, int cstat, FIOSPTR css);
extern int	_unit_scratch(unit *cup);
extern int	_unit_seek(unit *cup, recn_t recn, int iost);
extern int	_unit_trunc(unit *cup);
extern int	_setpos(FIOSPTR css, unit *cup, int *pa, int len);
extern int	_uniqinod(unit *cup, assign_info *aip);
extern int	_f_opn(char *actnam, unit *cup, FIOSPTR css, int tufs,
			int aifound, assign_info *aip, struct stat *statp,
			int statp_valid, int catcherr, int o_sysflgs);
extern int	_do_open(unit *cup, FIOSPTR css, int tufs, char *actnam, 
			int flags, int aifound, assign_info *aip, 
			union spec_u *fdspec, int catcherr);
extern void	_set_device_and_inode(int sysfd, dev_t *devicep, ino_t *inodep);

extern xfer_func	_rdunf;
extern xfer_func	_wrunf;
extern xfer_func	_rdfmt;
extern xfer_func	_wrfmt;
extern xfer_func	_ld_read;
extern xfer_func	_ld_write;
extern xfer_func_c	_frwd;
extern xfer_func_c	_fwwd;

extern int	_dw_endrec(FIOSPTR _Css, unit *_Cup, int _Count);
extern int	_iw_endrec(FIOSPTR _Css, unit *_Cup, int _Count);
extern int	_sw_endrec(FIOSPTR _Css, unit *_Cup, int _Count);
extern int	_nonadv_endrec(FIOSPTR _Css, unit *_Cup);
extern int	_lw_after_nonadv(FIOSPTR _Css, unit *_Cup, int _Linelimit,
			int _Namelistflag);

extern int	_dr_endrec(FIOSPTR _Css, unit *_Cup, int _Count);
extern int	_ir_endrec(FIOSPTR _Css, unit *_Cup, int _Count);
extern int	_sr_endrec(FIOSPTR _Css, unit *_Cup, int _Count);

extern void	_gather_data(void *lbuf, long items, long inc, int len,
			void *ptr);
extern void	_scatter_data (void *ptr, long items, long inc, int len,
			void *lbuf);

extern void	_set_ok_flags(unit *cup);
extern int	_get_mismatch_error(int noabort, int iost, unit *cup,
			FIOSPTR css);

extern int	_iochunk(FIOSPTR css, unit *cup, xfer_func *func,
			struct DvDimen *dimen, type_packet *tip, short nd,
			long extent, int bshft, bcont *addr);

extern void	_flsh_mem(void);

extern int	_deduce_fstruct(int, struct fdinfo *, int);
extern void	_setup_cvrt(unit *cup);
extern void	_b_char(char *a, char *b, ftnlen blen);
extern void	_copy_n_trim(char *a, ftnlen alen, char *b);
/************************************************************************
 *
 * External symbols
 *
 ***********************************************************************/

#define	errfile	stderr

extern	unit_htable	_fort_unit[];	/* Hash table of units		*/

extern	plock_t _openlock;	/* Connecting unit lock			*/
extern	plock_t _ioblock;	/* __iob table lock			*/
extern	plock_t _parselock;	/* Format parsing lock			*/
extern  plock_t	_stdin_lock;	/* Stdin lock				*/
extern  plock_t	_stdout_lock;	/* Stdout lock				*/
extern  plock_t	_stderr_lock;	/* Stderr lock				*/

extern	int	_f_rcsz;	/* Default sequential formatted RECL	*/
extern	int	_f_ldsz;	/* Default list-directed output RECL	*/
extern	int	_def_bin_bs;	/* Default '-s bin' buffer size		*/
extern	int	_def_sbin_bs;	/* Default '-s sbin' buffer size	*/
#ifdef	_CRAYMPP
extern	volatile int	_infio;		/* Set when inside an i/o statement*/
extern	volatile int	_needtostop;	/* Set when we need to stop 	*/
#endif

/* External tables and structures (defined in tables.c)	*/

extern const ftype_t
	_f77_to_f90_type_cnvt[DT_MAX];	/* f77 to f90 type conversion	*/
extern const short
	_f90_to_f77_type_cnvt[DVTYPE_NTYPES]; /* f90 to f77 type conv.	*/
extern const short
	_f77_type_len[DT_MAX];		/* f77 type lengths (bytes)	*/
extern const char *
	_f90_type_name[DVTYPE_NTYPES];	/* f90 data type names		*/
extern const char *
	_f77_type_name[DT_MAX];		/* f77 data type names		*/
extern const short
	_charset_cnvt[CS_MAX];		/* Character to numeric conv.	*/
extern const short
	_ffstat_cnvt[7];		/* FFIO to frch status conv.	*/
extern const short
	_old_namelist_to_f77_type_cnvt[10]; /* Namelist to f77 type cnv.*/
extern type_packet	__tip_null;	/* Null type packet		*/

#ifdef	_CRAY1
#pragma _CRI taskcommon _tsk_fiostate
#endif
extern struct fiostate	_tsk_fiostate;	/* Task local I/O state */
extern short	_fortran_io_is_init;	/* 0 until Fortran I/O is init */
extern short	_e_fortran_io_is_init;	/* 0 until ext Fortran I/O is init */
extern short	_i_fortran_io_is_init;	/* 0 until int Fortran I/O is init */
extern unit	*_fort_internal_unit;	/* Pointer to internal unit	*/

/************************************************************************
 *
 * Inline Functions
 *
 ***********************************************************************/

/*
 *	_get_cup 
 *
 *		This inline function returns a unit pointer for a particular
 *		unit connected to an external file.  If the unit is not open, 
 *		then NULL is returned.  The errstat parameter is assigned an 
 *		error status if an error is encountered.
 *
 *		Any necessary locking of the unit is performed.
 *
 *		NOTE: this version of _get_cup will be rewritten soon
 *		when dynamic unit allocation is activated.
 *		
 *	Return value:
 *		Pointer to a connected unit.  If the unit number is not 
 *		connected NULL is returned. 
 */
_PRAGMA_INLINE(_get_cup)
static unit *
_get_cup(unum_t unum)
{
	unit	*cup;

	cup	= _fort_unit[UHASH(unum)].ulist;

	if (cup != NULL) {
#ifdef	_CRAYMPP
		if (cup->uid != unum)
#else
		if (cup->private || cup->uid != unum)
#endif
			cup	= _search_unit_list(cup, unum);
	}

	if (cup != NULL) {
		MEM_LOCK(&cup->uiolock);	/* lock the unit */
		if (OPEN_UPTR(cup)) {	/* if unit is connected */
			/*
			 * Lock the auxiliary lock if this unit has one.
			 * This lock is necessary for standard files, which 
			 * are connected to more than one unit.
			 */
			if (cup->auxlockp != NULL) {
				MEM_LOCK(cup->auxlockp);	
			}
		}
		else {			/* else unit is not connected */
			MEM_UNLOCK(&cup->uiolock);
			cup	= NULL;		/* unit not connected */
		}
	}
	return(cup);
}

/*
 *	_get_int_cup
 *
 *		This inline function returns a unit pointer for an internal
 *		file.
 *
 *		Any necessary locking of the unit is performed.
 *
 *	Return value:
 *		Pointer to a unit.  If an error occurs, then NULL is returned.
 */

_PRAGMA_INLINE(_get_int_cup)
static unit *
_get_int_cup(void)
{
	unit	*cup;
	cup	= _fort_internal_unit;
	if (cup == NULL)
		cup	= _init_internal_unit();
	MEM_LOCK(&cup->uiolock);
	return(cup);
}

/*
 *	_release_cup 
 *
 *		Unlock a unit pointer.
 */
_PRAGMA_INLINE(_release_cup)
static void
_release_cup(unit *cup)
{
	MEM_UNLOCK(&cup->uiolock);
	if (cup->auxlockp != NULL)
		MEM_UNLOCK(cup->auxlockp);
}

extern void _ferr(FIOSPTR, int, ...);
extern int _get_dc_param(FIOSPTR, unit *, struct f90_type, type_packet *);
extern int _is_file_name(long n);
#ifdef KEY /* Bug 6433 */
extern void flush_(_f_int4 *n);
#else /* KEY Bug 6433 */
extern void flush_(const unum_t *n);
#endif /* KEY Bug 6433 */
extern int _f_open(FIOSPTR css, unit **cup_p, olist *olptr, int isf90);
extern int _f_inqu(FIOSPTR css, unit *cup, inlist *a);
extern int _fortname(char *buf, unum_t n);
extern int _mixed_scope(unit *cup);
extern int _ft_stopen(unit *cup, char *atstr);
extern int _ft_stclose(unit *cup);
extern int _unpack_arry(void *dvc, DopeVectorType *dvnc);
extern int _nonadv_partrec(FIOSPTR css, unit *cup);

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
/* Putting the pragma inline before the function declaration wasn't */
/* effective on MIPS systems. */
#pragma inline _get_cup
#pragma inline _get_int_cup
#pragma inline _release_cup
#endif

#endif /* !_FIO_H */
