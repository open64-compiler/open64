/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/fopn.c	92.6	09/20/99 11:41:08"

#include <errno.h>
#include <fcntl.h>
#include <liberrno.h>
#ifndef	_ABSOFT
#include <malloc.h>
#endif
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <cray/nassert.h>
#ifdef	_UNICOS_MAX 
#include <mpp/globals.h>
#endif 
#include <cray/assign.h>
#include "fio.h"
#include "util/utildefs.h"
#include "ffio/spec_parse.h"

#define TRACK	(42*BLKSIZE)	/* Most common track size */
#define FPARMAX		3	/* Number of numeric layer parameters */
#define SPECSZ	(FPARMAX+2+1)	/* Room for largest FFIO spec used */
				/* FPARMAX + 1 for class + 1 for terminating zero spec */
				/* + 1 for cachea layer shared cache specification */
				/* (not used) */
#define ACCESS_PERMS	0666	/* Access permissions */

#if	defined(_LITTLE_ENDIAN) && !defined(__sv2)
#ifndef IOWRT
#define IOWRT	= _IO_CURRENTLY_PUTTING
#endif
#ifndef IORW
#define IORW	= _IO_TIED_PUTGET
#endif
extern FILE *fdopen(int, const char*);
#endif          /* LITTLE_ENDIAN and not sv2 */

extern int	__fdctrace_enable;
static int	_defbufsiz_warning;	/* set to 1 after first DEFBUFSIZ warning */

static int
make_fdspec(union spec_u *fdspec, char *layer, int intnum[FPARMAX]);

#ifdef KEY /* Bug 2559 */
/*
 *	Return value:
 *		1 if we want to suppress repeat factors during list-directed
 *		output, 0 otherwise
 *
 *	Side effects:
 *		initialize save_suppress_repeats variable to avoid calling
 *		getenv repeatedly
 */
static int suppress_repeats() {
  static int save_suppress_repeats = 2;
  return (2 != save_suppress_repeats) ?
    save_suppress_repeats :
    (save_suppress_repeats = (0 != getenv("FTN_SUPPRESS_REPEATS")));
  }
#endif /* KEY Bug 2559 */

#ifdef KEY /* Bug 1678 */
extern void	_ffconvert_stat(struct stat *src, struct stat *dest);
#endif /* KEY Bug 1678 */

/*
 *	_f_opn - fortran open
 *
 *	Return value:
 *		0 if OK, -1 on error with errno set
 *
 *	Side effects:
 *		statp structure is made valid if not valid upon entry.
 */

int
_f_opn(
char		*actnam,	/* actual file name */
unit		*cup,
FIOSPTR		css,
int		tufs,		/* used to store ufs till open is OK */
				/* cup->ufs set indicates open was OK */
int		aifound,	/* 1 if aip is valid */
assign_info	*aip,		/* assign information */
struct stat	*statp,		/* pointer to stat buf */
int		statp_valid,	/* 1 if statp is valid upon entry, 0 if the 
				 * file has not yet been stat'ed */
int		catcherr,	/* nonzero if abort on errors */
int		o_sysflgs)	/* O_TRUNC/O_CREAT/O_EXCL */
{
	register short	default_ftype;	/* 1 iff no -s or -F assigned */
	register short	disk_file;
	register short	i;
	register short	not_open;
	register int	fd;
	register int	flags;
	register int	opt_flags;
	register int	acc_mode;
	int		num[FPARMAX];
	char		*ffio_layer;
	union spec_u	specspace[SPECSZ];
	union spec_u	*fdspec;
	struct fdinfo	*fio;
	struct ffsw	ffiostat;
	struct ffc_info_s info;
#ifdef KEY /* Bug 1678 */
	struct stat ffio_statbuf;
#else /* KEY Bug 1678 */
	struct ffc_stat_s ffio_statbuf;
#endif /* KEY Bug 1678 */

	extern int	_def_cch_bufsiz;
#ifdef	_UNICOS_MAX
	extern int	_def_cch_simbufsiz;
#endif
#ifdef KEY /* Bug 1678 */
	/* See above */
#else /* KEY Bug 1678 */
	extern void	_ffconvert_stat(struct ffc_stat_s *src,
					struct stat *dest);
#endif /* KEY Bug 1678 */

	disk_file	= 1;	/* assume a disk file */
	not_open	= 1;	/* file is not yet open */
	opt_flags	= 0;	/* set no optional flags */
	flags		= 0;
	fdspec		= NULL;

/*
 *	Lookup the environment variable which was used in UNICOS 6.0 to
 *	set the default buffer size and issue a warning if the user has set it.
 */
	if (_defbufsiz_warning == 0 && ((char *)getenv("DEFBUFSIZ") != NULL)) {
		_defbufsiz_warning	= 1; /* suppress subsequent warnings */
		_fwarn(FWDEFBSZ);
	}

/*
 *	If -s and -F are not assigned, set flag to indicate that the default
 *	file type is being used.
 */
	if (! aifound || (! aip->s_fstrct_flg && ! aip->F_filter_flg))
		default_ftype	= 1;
	else
		default_ftype	= 0;

/*
 *	Initialize unit table fields ucharset and unumcvrt.
 */
	if (aifound && aip->C_chrcnv_flg)
		cup->ucharset	= aip->C_chrcnv;
	else
		cup->ucharset	= 0;

	if (aifound && aip->N_datcnv_flg)
		cup->unumcvrt	= aip->N_datcnv;
	else
		cup->unumcvrt	= 0;

	if (cup->unumcvrt || cup->ucharset) {
/*
 *		Numeric conversion implies matching character
 *		conversion if not otherwise specified.  Also,
 *		change NCV_NATIVE and CS_NATIVE to zero to
 *		simplify run-time checking.
 */
		_setup_cvrt(cup);
	}

#if	NUMERIC_DATA_CONVERSION_ENABLED
/*
 *	cup->ualignmask is set to the alignment "granularity" - 1.  This
 *	provides a mask for fast checking of whether alignment is 
 *	needed.  Note that a value of zero really implies a granularity
 *	of 1 (ie no padding).
 */
	cup->ualign	= *__fndc_align[cup->unumcvrt];

	if (cup->unumcvrt == 0)
		cup->ualignmask	= (sizeof(long) << 3) - 1;	/* 63 */
	else {
		cup->ualignmask	= MAX(cup->ualign.gran, 1) - 1;
/*
 *		If granularity is not a power of 2, then we force a
 *		computation in COMPADD on every unformatted I/O request.
 */
		if ( ! POWER_OF_TWO(cup->ualignmask + 1) )
			cup->ualignmask	= ~0;	/* mask of one's */
	}
#endif
/*
 *	Set up sequential file truncation flag and the flag for special
 *	handling of muliply-accessed files.   Stdout and stderr default
 *	to no truncation after final write.   This is to avoid problems
 *	if SHMEM applications redirect stdout to a regular file.  A truncation
 *	at close time can result in lost output from another process 
 *	sharing the same stdout file.  
 */
	cup->utrunc	= 1;

	if (cup->ufp.std != NULL) {
		register int fd	= fileno(cup->ufp.std);
		if (fd == STDOUT_FILENO || fd == STDERR_FILENO)
			cup->utrunc	= 0;
	}

	if (aifound && aip->T_utrunc_flg)
		cup->utrunc	= aip->T_utrunc;

	if (aifound && aip->m_multup_flg && aip->m_multup) {
		cup->umultup	= 1;
		cup->utrunc	= 0;
	}
/*
 *	Detect some cases of the file not being a disk file.
 */
	if (statp_valid && ! S_ISREG(statp->st_mode))
		disk_file	= 0;	/* file exists and isn't a regular file */

/*
 *	Now we check for specifications which are mapped to FFIO.
 */
	ffio_layer	= NULL;

	for (i = 0; i < FPARMAX; i++)
		num[i]	= -1;		/* assume we'll use layer defaults */


	switch (tufs) {

/*
 *	Map '-s sbin' to '-s text' for formatted opens.
 */
	case STD:
		if (cup->ufmt) {
			errno	= FEOPNFMT;	/* Can't open FORMATTED */
			return(-1);
		}
		break;
/*
 *	Map '-s text' to '-F text' if doing implicit data conversion.
 */
	case FS_TEXT:
		if (cup->ufmt && cup->useq && cup->ucharset)
			ffio_layer	= "text";
		break;
/*
 *	Map '-s u' to '-F syscall -T off'
 *	Map '-s u -a SDS -n xx'     to '-F sds.scr.novfl:xx:xx:0 -T off'
 */
	case FS_U:
		if (! (aifound && aip->T_utrunc_flg))
			cup->utrunc	= 0; /* no trunc by default with -s U */

		opt_flags	= O_RAW;

		if (aifound && aip->a_sdsfil_flg) {	/* '-a SDS' requested */
			register int	presize;

			if (aip->n_preall_flg)
				presize	= aip->n_preall;
			else {		/* SDS allocation failure */
				errno	= FESDSFSS;
				return(-1);
			}
		
			ffio_layer	= "sds.scr.novfl";
			num[0]		= presize;
			num[1]		= presize;
			num[2]		= 0;
		}
		else
			ffio_layer	= "syscall";
		break;

/*
 *	Map f77 blocking with -b xx to '-F f77::(xx*BLKSIZE)'
 */
	case FS_F77:
		ffio_layer	= "f77";
		if (aifound && aip->b_bufsiz_flg)
			num[1]	= aip->b_bufsiz * BLKSIZE;
		break;

/*
 *	Map '-s COS -b xx' to '-F cos:xx'
 */
	case FS_COS:
		ffio_layer	= "cos";
		if (aifound && aip->b_bufsiz_flg)
			num[0]	= aip->b_bufsiz;
		break;


#if	!defined(_UNICOS) && !defined(__mips) && !defined(_LITTLE_ENDIAN)
/*
 *	Map '-s unblocked'	      to '-s sbin'         on non-UNICOS systems
 *	Map '-s bin'	              to '-s sbin -T off'  on non-UNICOS systems
 */
	case FS_UNBLOCKED:
	case FS_BIN:
		if (tufs == FS_BIN) {
			if (! (aifound && aip->T_utrunc_flg))
				cup->utrunc	= 0; 
		}
		/*
		 * The only buffering scheme available on Sparc for unblocked
		 * files is stdio, so we use it.
		 */
		tufs	= STD;			
		break;
#endif	/* !_UNICOS && !__mips && !_LITTLE_ENDIAN */

#if	defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
/*
 *	Map '-s unblocked'	       to '-s sbin'         for non-disk files
 *	Map '-s unblocked -b bs'       to '-F bufa:bs'      for seq disk files on Unicos
 *	Map '-s unblocked -b bs'       to '-s sbin'         for seq disk files on MIPS
 *	Map '-s unblocked -b bs -u nb' to '-F cachea:bs:nb' for direct acc files
 *
 *	Map '-s bin' to the same, plus set "-T off" by default.
 *
 *	Map unformatted FORM='SYSTEM' and FORM='BINARY' to -s unblocked.
 *	There are no record control images for these two forms.
 */
	case FS_UNBLOCKED:
	case FS_BIN:
		if (!disk_file) {
			tufs	= STD;		/* ttys and pipes use stdio */
			break;
		}

		if (cup->useq) {		/* If sequential access file */
			ffio_layer	= "bufa";
			num[0]		= SUBUFSZ;	
		}
		else {
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
			ffio_layer	= "cache";
#else
			ffio_layer	= "cachea";
#endif
			num[0]		= DUBUFSZ;	
		}

		if (tufs == FS_BIN) {
			if (! (aifound && aip->T_utrunc_flg))
				cup->utrunc	= 0; 
			num[0]	= DEF_BIN_BS;	
		}

#ifdef	_UNICOS_MAX
		if (_MPP_MPPSIM > 0) {
			/*
			 * Running on simulator in user virtual mode. 
			 * Simulator cannot handle large reads/writes, 
			 * so adjust the default buffer size smaller.
			 */
			num[0]	= _VALUE(_def_cch_simbufsiz);
		}
#endif
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		/*
		 * If the user has not requested a buffer size
		 * with assign, ensure that the buffer size is
		 * at least as large as the record size up to
		 * a "reasonable" limit.  At some point, the
		 * user must take responsibility for assigning
		 * lots of memory to I/O buffers.
		 */

		if (!cup->useq && default_ftype &&
		    (DUBUFSZ * BLKSIZE) < cup->urecl) {
			register int	bsize;

			bsize	= (cup->urecl + (BLKSIZE - 1)) / BLKSIZE;

			if (bsize > 100)
				bsize	= 100;

			num[0]	= bsize;
		}

#endif

		if (aifound && aip->b_bufsiz_flg)
			num[0]	= aip->b_bufsiz;

		if (aifound && aip->u_bufcnt_flg)
			num[1]	= aip->u_bufcnt;

		break;
#endif	/* _UNICOS || __mips || _LITTLE_ENDIAN */

#ifdef	_UNICOS
/*
 *	Map '-s [bmx, tape, tape50, bmx50] -b bs -u nm' to '-F tape:bs:nm' 
 */
	case FS_TAPE:
		cup->ubmx	= 1;	/* Indicate that '-s tape' or '-s bmx'
				 * was selected by the user. */

	case FS_TAPE50:		/* Fall-through! */

		ffio_layer	= "tape";

		if (aifound && aip->b_bufsiz_flg)
			num[0]	= aip->b_bufsiz;

		if (aifound && aip->u_bufcnt_flg)
			num[1]	= aip->u_bufcnt;
		break;
#endif 	/* _UNICOS */
	}	/* end switch(tufs) */

	if (tufs == FS_FDC)
		fdspec	= &aip->F_filter[0];
	else if (ffio_layer != NULL) {
		fdspec	= specspace;
		make_fdspec(fdspec, ffio_layer, num);
		tufs	= FS_FDC;
	}

/*
 *	Only the FS_FDC paths through _frwd/_fwwd/_frch/_fwch handle 
 *	data conversion.
 */
	if (tufs != FS_FDC &&
	    (!cup->ufmt && cup->unumcvrt || (cup->ufmt && cup->ucharset))) {
		errno	= FENOICNV;
		return(-1);
	}

/*
 *	Open the file with permissions dictated by the ACTION= specifier and
 *	the system file permissions.  When ACTION= is unspecified on the open,
 *	try read/write, read-only, and write-only.
 *
 *	When ACTION='WRITE' is specified, try read/write and write-only if the
 *	file is a disk file.  Read permission may be needed for support of
 *	backspace and read-before-write buffer caching.  If the file is not a
 *	disk file, open as write-only since backspace and read-before-write
 *	caching support are not needed.
 */
	if (cup->uaction == OS_READWRITE ||
	   (cup->uaction == OS_WRITE && disk_file) ||
	    cup->uaction == OS_ACTION_UNSPECIFIED) {

		flags	= O_RDWR | o_sysflgs | opt_flags;

		if (_do_open(cup, css, tufs, actnam, flags, aifound, aip,
			fdspec, catcherr) == 0)
			not_open	= 0;
	}

	if (not_open &&
	    (cup->uaction == OS_READ ||
	     cup->uaction == OS_ACTION_UNSPECIFIED)) {

		flags	= O_RDONLY | o_sysflgs | opt_flags;

		if (_do_open(cup, css, tufs, actnam, flags, aifound, aip,
			fdspec, catcherr) == 0)
			not_open	= 0;
#ifdef KEY /* Bug 3782 */
/* When the file already exists but STATUS='new', this bug-workaround code
 * suppresses the error that ought to be reported. As a consequence, the
 * customer's program would later crash when it tried to write the file,
 * because although 'open' reported no error, it did not actually obtain
 * write access.
 */
#else
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
		else {
		/*
		 * There is a bug in NFS for IRIX 6.2 through 6.5 systems
		 * whereby an open with O_RDONLY|O_CREAT aborted if the
		 * process only had read-only access to an existing file.
		 * The O_CREAT is supposed to be ignored if the file
		 * already exists, but that wasn't happening.  To work
		 * around this bug, we remove the O_CREAT flag and retry
		 * the open.  Someday, when these systems are no longer
		 * supported, this code can be removed.
		 */
			flags	= flags & ~O_CREAT;	/* Remove O_CREAT bit */

			if (_do_open(cup, css, tufs, actnam, flags, aifound,
					aip, fdspec, catcherr) == 0)
				not_open	= 0;
		}
#endif
#endif /* KEY Bug 3782 */
	}

	if (not_open &&
	    (cup->uaction == OS_WRITE ||
	     cup->uaction == OS_ACTION_UNSPECIFIED)) {

		flags	= O_WRONLY | o_sysflgs | opt_flags;

		if (_do_open(cup, css, tufs, actnam, flags, aifound, aip,
			fdspec, catcherr) == 0)
			not_open	= 0;
	}

	if (not_open) {
		/*
		 * Replace error code EACCES with FEFILACT only when
		 * ACTION= was specified on OPEN.
		 */
		if (cup->uaction != OS_ACTION_UNSPECIFIED && errno == EACCES)
			errno	= FEFILACT; /* invalid ACTION for this file */
		return(-1);
	}

	acc_mode	= flags & O_ACCMODE;
	cup->usysread	= (acc_mode == O_RDONLY || acc_mode == O_RDWR);
	cup->usyswrite	= (acc_mode == O_WRONLY || acc_mode == O_RDWR);

/*
 *	Be sure to set uaction if ACTION= was not specified on the OPEN.
 */
	if (cup->uaction == OS_ACTION_UNSPECIFIED)
		cup->uaction	= cup->uaction |
				  (cup->usysread  ? OS_READ  : 0) |
				  (cup->usyswrite ? OS_WRITE : 0);

/*
 *	Set cup->ufs to indicate that the underlying open has succeeded.
 */
	cup->ufs	= tufs;

/*
 *	Tape file setup for the assign -d option.
 */
	if (aifound && aip->d_datrcv_flg) {
		register int	ret;

		if (tufs != FS_FDC) {
			errno	= FENOSKPB;
			return(-1);
		}
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		ret	= XRCALL(cup->ufp.fdc, fcntlrtn) cup->ufp.fdc, FC_AUTOBAD,
			((aip->d_datrcv == AS_SKIPBAD) ? AUTO_SKIP : AUTO_ACPT),
			 &cup->uffsw);

		if (ret < 0) {
			errno	= cup->uffsw.sw_error;
			return(-1);
		}
#else
		return(-1);
#endif
	}

/*
 *	Initialize the asynchronous status structure. This word is
 *	passed a little differently for async and for synchronous I/O.
 *	The WAITIO macro is called anytime an outstanding request could
 *	be pending, and appropriate processing done to ensure that the
 *	the status field is set in the ffsw.
 */
	FFSTAT(cup->uffsw)	= FFBOD;	/* at beginning of data */
	cup->uffsw.sw_error	= 0;
	cup->uffsw.sw_count	= 0;

/*
 *	Set cf77, cf90, irixf77, or irixf90 mode.  Retain uft90 for
 *	simple tests between f77 and f90.  Use the ufcompat for other
 *	differences.
 */

	cup->ufcompat	= 0;

	if ( aifound && aip->f_fortst_flg ) {
		(cup->uft90	= (aip->f_fortst == AS_FORTRAN90) ? 1 :
		(cup->uft90	= (aip->f_fortst == AS_IRIX_F90) ? 1 : 0));
		cup->ufcompat	= aip->f_fortst;
	}

	if ( aifound && aip->t_tmpfil_flg ) {
		cup->uscrtch	= 1;
		cup->utmpfil	= 1;
	}

/*
 *	Set up list-directed output format for MPP, PVP, and IRIX
 *	systems.  Set the global values first.  Allow individual
 *	flags to override the global flag.  Note that default is
 *	the same for all systems except for the namelist skip.
 *	Also set the default for writing -0.0 for formatted io.
 */
	cup->ufunilist	= 0;
#ifdef KEY /* Bug 5921 */
	/* Default is 1, which doesn't use commas */
	cup->ufcomsep	= 1;
#else /* KEY Bug 5921 */
	cup->ufcomsep	= 0;
#endif /* KEY Bug 5921 */
	cup->ufcomplen	= 0;
#ifdef KEY /* Bug 5921 */
	/* Default is 1, which doesn't use repeat count */
	cup->ufrptcnt	= 1;
#else /* KEY Bug 5921 */
	cup->ufrptcnt	= 0;
#endif /* KEY Bug 5921 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	cup->ufnl_skip	= 0;
	cup->ufnegzero	= 0;
#else
	cup->ufnl_skip	= 1;
	cup->ufnegzero	= 1;
#endif
	
/*
 *	If the global list-directed output flag is set for UNICOS,
 *	set the individual flags to the same value as the global
 *	flag.
 */
	if ( aifound && aip->U_unicoslist_flg ) {
		cup->ufunilist	= aip->U_unicoslist;
		cup->ufcomsep	= aip->U_unicoslist;
		cup->ufcomplen	= aip->U_unicoslist;
		cup->ufrptcnt	= aip->U_unicoslist;
	} else if (aifound && aip->f_fortst == AS_IRIX_F77) {
/*
 *	If the global list-directed output flag is not set and
 *	the f irixf77 option is specified, set the individual
 *	individual flags to the MIPS f77 default values.
 */
		cup->ufunilist	= 1;
		cup->ufcomsep	= 1;
		cup->ufcomplen	= 1;
		cup->ufrptcnt	= 1;
	}

/*	Override the global list-directed output settings if
 *	individual flags are set.
 */
	if ( aifound && aip->S_comsep_flg ) {
#ifdef KEY /* Bug 5921 */
		/* We reversed the meaning of S_comsep in the assign command
		 * parser, so that the default 0 now means "don't use commas"
		 * (but for backward compatibility remains associated with "on")
		 */
		cup->ufcomsep	= ! aip->S_comsep;
#else /* KEY Bug 5921 */
		cup->ufcomsep	= aip->S_comsep;
#endif /* KEY Bug 5921 */
	}

	if ( aifound && aip->W_compwidth_flg ) {
		cup->ufcomplen	= aip->W_compwidth;
	}

	if ( aifound && aip->y_reptcnt_flg ) {
#ifdef KEY /* Bug 5921 */
		/* We reversed the meaning of y_reptcnt in the assign command
		 * parser, so that the default 0 now means "don't use repeat*"
		 * (but for backward compatibility remains associated with "on")
		 */
		cup->ufrptcnt	= ! aip->y_reptcnt;
#else /* KEY Bug 5921 */
		cup->ufrptcnt	= aip->y_reptcnt;
#endif /* KEY Bug 5921 */
	}
#ifdef KEY /* Bug 2559 */
	if (suppress_repeats()) {
		cup->ufrptcnt = 1;
	}
#endif /* KEY Bug 2559 */

/*
 *	Override global setting for writing -0.0 in formatted io.
 *	Default setting for irix: Do not write minus sign for -0.0.
 *	Default setting for all PVP,ieee platforms: Write minus sign
 *	for -0.0.
 */
	if ( aifound && aip->Z_neg_zero_flg ) {
		cup->ufnegzero	= aip->Z_neg_zero;
	}

/*
 *	Set up namelist input behavior for MPP, PVP, IRIX systems
 *	when cilist namelist group name does not match the group
 *	name provided in the namelist input record.  The default
 *	is to error on UNICOS and UNICOS/mk and to skip on IRIX.
 */
	if ( aifound && aip->Y_nl_skip_flg ) {
		cup->ufnl_skip	= aip->Y_nl_skip;
	}

/*
 *	Set cup->ublkd and fd in a variety of ways.
 */
	switch( cup->ufs ) {
	register int	ret;

	case FS_TEXT:
		fd	= fileno ( cup->ufp.std );
		if (cup->ufmt == 0) {
			errno	= FEOPNUNF; /* Can't open UNFORMATTED */
			return(-1);
		}
		cup->ublkd	= 1;
		break;

	case STD:
		fd	= fileno ( cup->ufp.std );
		if (cup->ufmt)
			cup->ublkd	= 1;	/* no stdio blocking if formatted */
		else
			cup->ublkd	= 0;	/* no stdio blocking if unformtd */
		break;

	case FS_FDC:
/*
 *		Get file descriptor and other info for FDC files
 */
		ret	= XRCALL(cup->ufp.fdc, fcntlrtn) cup->ufp.fdc,
				FC_GETINFO, &info, &ffiostat);
		if (ret < 0) {
			errno	= ffiostat.sw_error;
			return(-1);
		}

		cup->uflagword	= info.ffc_flags;

/*
 *		Get stat information.
 */
		ret	= XRCALL(cup->ufp.fdc, fcntlrtn) cup->ufp.fdc,
				FC_STAT, &ffio_statbuf, &ffiostat);
		if (ret < 0) {
			errno	= ffiostat.sw_error;
			return(-1);
		}

		_ffconvert_stat(&ffio_statbuf, statp);
		statp_valid	= 1;

/*
 *		Indicate whether the file knows about EOR
 */
		cup->ublkd	= ((cup->uflagword & FFC_REC) ? 1 : 0);
/*
 *		Indicate whether the file permits multiple endfile records.
 *		Assume that a file has a physical endfile representation if
 *		and only if multiple endfile records are permitted.
 */
		cup->umultfil	= ((cup->uflagword & FFC_WEOF) ? 1 : 0);

		if (cup->ufmt) {
			/*
			 * Formatted files must be capable of handling text.
			 */
			if ((info.ffc_flags & FFC_CODED) == 0) {
				errno	= FEOPNFMT; /* Can't open FORMATTED */
				return(-1);
			}
			/*
			 * Sequential formatted files must have a record 
			 * structure.
			 */
			if (cup->useq && (info.ffc_flags & FFC_REC) == 0) {
				errno	= FEOPNFMT; /* Can't open FORMATTED */
				return(-1);
			}
		}
		else {
			/*
			 * Unformatted files must be capable of handling 
			 * binary data.
			 */
			if ((info.ffc_flags & FFC_BINARY) == 0) {
				errno	= FEOPNUNF; /* Can't open UNFORMATTED */
				return(-1);
			}
		}
/*
 *		Make sure the capabilities are right to do DA, if requested.
 */
		if (cup->useq == 0) {	/* If direct access */
/*
 *			If layer has a fixed record length, make sure it
 *			matches the REC= on the OPEN.
 */
			if (info.ffc_reclen != 0) {
				if (info.ffc_reclen != (cup->urecl << 3)) {
					/* reclen does not match file */
					errno	= FEOPIVRL;
					return(-1);
				}
			}
/*
 *			If not fixed length records, it better be stream.
 *			If not, (variable length recs?) blow the user off.
 */
			else if ((cup->uflagword & FFC_STRM) == 0) {
				errno	= FEOPNNDA;
				return(-1);
			}
		}
/*
 *		Make sure that user-requested suppression of truncation after
 *		write is supported by the top FFIO layer.
 */
		if (cup->useq && aifound && aip->T_utrunc_flg &&
		    aip->T_utrunc == 0 &&
		    (info.ffc_flags & FFC_WRTRUNC)) {

			errno	= FERQTRNC;	/* must trunc after write */
			return(-1);
		}

		fd	= info.ffc_fd;
		break;

	default:
		errno	= FEINTFST;
		return(-1);
	}

	cup->usysfd	= fd;

/*
 *	From this point on, the stat buffer pointed to by statp will be valid 
 *	for ALL files.
 */
	if (! statp_valid && fstat(fd, statp) == -1)
		return(-1);
	statp_valid	= 1;

/*
 *	The useek field is used only for STD files.
 */
	if (fd != -1) {
		if ((S_ISREG(statp->st_mode) && !isatty(fd)) || 
		    _gsys_qtape(statp))
			cup->useek	= YES;
		else
			cup->useek	= NO;
	}

#ifdef	_UNICOS
/*
 *	Do preallocation as specified by -n/-p/-q
 */
	if (fd != -1) {
		if (_prealloc(fd, aifound, aip, statp) == -1)
			return(-1);
	}
#endif

/*
 *	See if asynchronous I/O is OK.
 *	Asynchronous I/O is only allowed if no data conversion is active and
 *	some form of FFIO is being used.  All FFIO should have an async
 *	entry, even though it may not *actually* be asynchronous.
 */
	cup->uasync	= ASYNC_NOTOK;	/* assume that async is NOT OK. */

	if ((cup->unumcvrt | cup->ucharset) == 0 && cup->ufs == FS_FDC) {
		cup->uasync	= ASYNC_OK;	/* async I/O is OK */
	}

/*
 *	Set up the udamax field.  This is the 1-based maximum record number 
 *	for direct access files.
 */
	if (cup->useq == 0) {	/* If direct access */
		long	recl;
		recl	= cup->urecl;
		if (cup->ufmt && cup->ufs != FS_FDC)
			recl++;
		/* allow the last record to be shorter than recl */
		cup->udamax	= (statp->st_size + recl - 1) / recl;
	}

	return(0);
}

/*
 *	make_fdspec	- assembles the fdspec which can be passed to _ffopen
 *
 *	Return value 
 *		Always returns 0.
 */
static int
make_fdspec(
	union spec_u	*fdspec,		/* output - receives the fdspec */
	char		*layer,			/* the layer name */
	int		num[FPARMAX])		/* the numeric parameters; -1 
						 * indicates no parameter */
{
	register short	i;
	register int	ret;
	char	buf[FPARMAX][30];	/* each must be big enough for MAXINT */
	char	ffio_str[20+FPARMAX*30];/* holds layer string and up to 3 
					 * integers */

	for (i = 0; i < FPARMAX; i++) {
		if (num[i] == -1) 
			buf[i][0]	= '\0';	/* empty string */
		else
			sprintf(buf[i], "%d", num[i]);
	}

	/* Assume FPARMAX is <= 3 */

	(void) sprintf(ffio_str, "%s:%s:%s:%s", layer, buf[0], buf[1], buf[2]);

	ret	= _parse_forstr(fdspec, ffio_str, SPECSZ, 0, _LELVL_RETURN);

	if (ret < 0)
		_ferr(NULL, FEINTUNK);		/* shouldn't happen */

	return(0);
}

/*
 *	According to the file structure make the appropriate call to
 *	open the file.  Open routines are file structure dependent.
 *
 *	Return value is 0 on success, -1 on error with errno set.
 *
 *	If catcherr == 0, some errors will abort here.  Others do not cause
 * 	an abort because _do_open might be retried with different permissions.
 */
int
_do_open(
unit		*cup,
FIOSPTR		css,
int		tufs,
char		*actnam,
int		flags,
int		aifound,
assign_info	*aip,
union spec_u	*fdspec,
int		catcherr)
{
	long		bs;
	register int	cbits;
	register int	cblks;
	register int	i;
	long		asave[(AFLAGSIZE + sizeof(long)) / sizeof(long)];
	char		*flagstr;
	char		*attrstr;
	struct	ffsw	ffiostat;
	struct gl_o_inf	gloinf;
	_ffopen_t	otmp;

	(void) memset(&gloinf, 0, sizeof(gloinf));

	gloinf.open_type	= OT_FORTRAN;
	gloinf.aip		= aifound ? aip : NULL;
	gloinf.u.fort.unum	= cup->uid;
	gloinf.u.fort.is_seq	= cup->useq;
	gloinf.u.fort.is_fmt	= cup->ufmt;
	gloinf.u.fort.reclen	= cup->urecl;

	cbits	= 0;
	cblks	= 0;

/*
 *	Handle -B, -r, -x, -w,-L, and -l assign options.
 */
	if (aifound) 
		_ae_setoflags(aip, &flags);

#ifdef	_UNICOS
/*
 *	Handle -p and -q assign options.
 */
	if (aifound && aip->pr_partit_flg) {
		flags	= flags | O_PLACE;
		cbits	= aip->pr_partit;
	}

	if (aifound && (aip->n_stride_flg || aip->q_ocblks_flg)) {
		flags	= flags | O_PLACE;
 
		if (aip->q_ocblks_flg)
			cblks	= aip->q_ocblks;
		else
			cblks	= aip->n_stride;
	}
#endif	/* _UNICOS */

	switch ( tufs ) {

	case FS_TEXT:
	case STD:

/*
 *		If this is a standard file which is already open, skip
 *		the open system call, but validate the flags.
 */
		if (cup->ufp.std != NULL) {
			register int	fdflags;

#if	!defined(_LITTLE_ENDIAN) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
			if (cup->ufp.std->_flag & _IORW)
				fdflags	= O_RDWR;
			else if (cup->ufp.std->_flag & _IOWRT)
				fdflags	= O_WRONLY;
			else
				fdflags	= O_RDONLY;

			if ((fdflags & O_ACCMODE) != (flags & O_ACCMODE)) {
				errno	= EACCES;
				return(-1);
			}
#endif
		}
		else {

#ifdef	_UNICOS 
			/*
			 * _stdio_open is a duplicate name for open() 
			 * It is used ONLY by code in libc/stdio and here, 
			 * because this is nearly equivalent to an 
			 * "fopen". _stdio_open is used so that the opens 
			 * from these routines will be unique. It was 
			 * requested by marketing.
			 */
#ifndef	_CRAYMPP
			i	= _stdio_open(
#else
			i	= open(
#endif
				actnam, flags, ACCESS_PERMS, cbits, cblks);
#else	/* ! _UNICOS */
			i	= open( actnam, flags, ACCESS_PERMS);
#endif	/* ! _UNICOS */
			if (i < 0)
				return(-1);

			if ((flags & O_ACCMODE) == O_RDWR)
				flagstr	= "r+";
			else if ((flags & O_ACCMODE) == O_WRONLY) {
				if (flags & O_APPEND)
					flagstr	= "a";
				else
					flagstr	= "w";
			}
			else 
				flagstr	= "r";

			MEM_LOCK(&_ioblock);	/* Lock libf access to iob table*/

			cup->ufp.std	= fdopen(i, flagstr);

			MEM_UNLOCK(&_ioblock);	/* Unlock iob table */

			if (cup->ufp.std == NULL)
				return(-1);
		}

/*
 *		Set up buffer size.  If file is direct access, then there
 *		is no advantage in having a buffer size larger than the
 *		record size because stdio ends up flushing each record
 *		anyway.  For formatted I/O, add one byte for the newline.
 *		Finally, ensure than the buffer size is at least two words.
 */
		bs	= DEF_SBIN_BS * BLKSIZE;	/* Set default size */

#ifdef	_UNICOS_MAX
		/*
		 * If running on simulator in user virtual mode, restrict
		 * the since since the simulation cannot handle large reads
		 * and writes.
		 */
		if (_MPP_MPPSIM > 0)
			bs	= DEF_SBINSIM_BS * BLKSIZE;
#endif

		if (aifound && aip->b_bufsiz_flg)
			bs	= aip->b_bufsiz * BLKSIZE;
		else
			if (cup->useq == 0 && cup->urecl < bs) {
				bs	= cup->urecl + (cup->ufmt ? 1 : 0);
				if (bs < 16)
					bs	= 16;
			}

/*
 *		We may only call setvbuf if the file has seen no read/write
 *		activity.  The buffer will not be allocated unless there
 *		have been reads or writes.  This check is necessary for
 *		standard files.
 *
 *		We do not allow DEF_SBIN_BS or the -b option to override 
 *		the _IONBF buffering mode for stderr or _IOLBF buffering
 *		for tty files.  In both cases, stdio picks appropriate
 *		buffer sizes.
 */
#if     defined(BUILD_OS_DARWIN)
		if (cup->ufp.std->_bf._base == NULL &&
		    fileno(cup->ufp.std) != STDERR_FILENO &&
		    ! isatty(fileno(cup->ufp.std)))
			(void) setvbuf(cup->ufp.std, NULL, _IOFBF, bs);
#elif     defined(_LITTLE_ENDIAN) && !defined(__sv2)
		if (cup->ufp.std->_IO_buf_base == NULL &&
		    fileno(cup->ufp.std) != STDERR_FILENO &&
		    ! isatty(fileno(cup->ufp.std)))
			(void) setvbuf(cup->ufp.std, NULL, _IOFBF, bs);
#else
		if (cup->ufp.std->_base == NULL &&
		    fileno(cup->ufp.std) != STDERR_FILENO &&
		    ! isatty(fileno(cup->ufp.std)))

			(void) setvbuf(cup->ufp.std, NULL, _IOFBF, bs);
#endif

		break;			/* DONE */

	case FS_FDC:

#ifdef	_UNICOS
/*
 *		Set the raw bit.  It is unlikely to hurt, and will
 *		frequently help performance.
 */
		flags	= flags | O_RAW;
#endif
		if (aifound)
			_attr_copy(aip, (assign_info *)&asave);

		otmp	= _ffopen(actnam, flags, ACCESS_PERMS, fdspec,
				&ffiostat, cbits, cblks, NULL, &gloinf);
		if (otmp == _FFOPEN_ERR) {
			/*
			 * Restore the ATTR_USED flags in case _do_open
			 * is called in a retry.
			 */
			if (aifound)
				_attr_copy((assign_info *)&asave, aip);

			errno	= ffiostat.sw_error;
			return(-1);
		}

		cup->ufp.fdc	= (struct fdinfo *) otmp;
		cup->useek	= YES;
		attrstr		= NULL;

		if (aifound && _attr_used(aip, &attrstr) == -1) {
			if (catcherr == 0) 
				_ferr(css, errno, attrstr);
			return(-1);
		}

		break;

	default:
		errno	= FEINTFST;
		return(-1);

	}
	return(0);
}

/*
 *	This function converts an "ffc_stat_s" structure to a "stat" structure.
 *	Currently, these structures are identical.  If they ever diverge,
 *	this function would call sysconf() or in some other way detect which
 *	version of struct stat was being used.  Then the ffc_stat_s structure
 *	would be translated into the stat structure.
 */ 
void
#ifdef KEY /* Bug 1678 */
_ffconvert_stat(struct stat *src, struct stat *dest)
#else /* KEY Bug 1678 */
_ffconvert_stat(struct ffc_stat_s *src, struct stat *dest)
#endif /* KEY Bug 1678 */
{
	assert ( sizeof(*src) == sizeof(*dest) );

	*dest	= *(struct stat *)src;

	return;
}
