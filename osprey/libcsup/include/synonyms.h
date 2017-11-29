/* USMID @(#)92/include/synonyms.h	92.2	06/08/99 10:44:16 */


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
 * This file is used to build an ANSI-C compatible version of libc.
 * Basically, it allows us to enforce the rule that ANSI C routines
 * cannot reference other non-ANSI routines or global data that is
 * in the user's namespace (e.g fopen() cannot call open()).
 */

/* external data */
#define _iob		__iob
#define endcp		_endcp
#define environ		_environ
#ifndef _CRAYMPP
#define altzone		_altzone
#define daylight	_daylight
#define sys_errlist	_sys_errlist
#define sys_nerr	_sys_nerr
#define timezone	_timezone
#define tzname		_tzname
#endif

/* functions */
#define _assert		__assert
#define _filbuf		__filbuf
#define _flsbuf		__flsbuf
#define access		_access
#define atabort		_atabort
#define brk		_brk
#define close		_close
#define ecvt		_ecvt
#define execl		_execl
#define execve		_execve
#define fcvt		_fcvt
#define fork		_fork
#define fstat		_fstat
#define getgid		_getgid
#define getpid		_getpid
#define getppid		_getppid
#define getuid		_getuid
#define isatty		_isatty
#define ioctl		_ioctl
#define killm		_killm
#define lseek		_lseek
#define memccpy		_memccpy
#define memwcpy		_memwcpy
#define memwset		_memwset
#define mktemp		_mktemp
#define open		_open
#define read		_read
#define sbreak		_sbreak
#define sigaction	_sigaction
#define sigaddset	_sigaddset
#define sigdelset	_sigdelset
#define sigemptyset	_sigemptyset
#define sigfillset	_sigfillset
#define sigismember	_sigismember
#define sigoff		_Sigoff
#define sigon		_Sigon
#define sigprocmask	_sigprocmask
#define stat		_stat
#define strdup		_strdup
#define syscall		_syscall
#define sysconf		_sysconf
#define target		_target
#define tempnam		_tempnam
#define times		_times
#define tracebk		_tracebk
#define unlink		_unlink
#define vfork		_vfork
#define waitpid		_waitpid
#define write		_write
#ifdef _CRAYMPP
#define barrier		_barrier
#endif

/* libm functions */
#if defined(_CRAY1) || defined(_CRAYMPP)
#define ACOS		_ACOS_
#define ASIN		_ASIN_
#define ATAN		_ATAN_
#define ATAN2		_ATAN2_
#define CABS		_CABS_
#define CCOS		_CCOS_
#define CEXP		_CEXP_
#define CLOG		_CLOG_
#define CSIN		_CSIN_
#define CSQRT		_CSQRT_
#define COS		_COS_
#define COSH		_COSH_
#define DABS		_DABS_
#define DACOS		_DACOS_
#define DASIN		_DASIN_
#define DATAN2		_DATAN2_
#define DATAN		_DATAN_
#define DCOS		_DCOS_
#define DCOSH		_DCOSH_
#define DEXP		_DEXP_
#define DINT		_DINT_
#define DLOG		_DLOG_
#define DLOG10		_DLOG10_
#define DMOD		_DMOD_
#define DSIN		_DSIN_
#define DSINH		_DSINH_
#define DSQRT		_DSQRT_
#define DTANH		_DTANH_
#define DTAN		_DTAN_
#define EXP		_EXP_
#define AMOD		_AMOD_
#define ALOG		_ALOG_
#define ALOG10		_ALOG10_
#define SIN		_SIN_
#define SINH		_SINH_
#define SQRT		_SQRT_
#define TAN		_TAN_
#define TANH		_TANH_
#endif

/* half-precision libm functions */
#ifdef	_CRAYMPP
#define HACOS		$HACOS
#define HASIN		$HASIN
#define HATAN		$HATAN
#define HATAN2		$HATAN2
#define HCOS		$HCOS
#define HCOSH		$HCOSH
#define HEXP		$HEXP
#define HABS		$HABS
#define HINT		$HINT
#define HMOD		$HMOD
#define HLOG		$HLOG
#define HLOG10		$HLOG10
#define HSIN		$HSIN
#define HSINH		$HSINH
#define HSQRT		$HSQRT
#define HTAN		$HTAN
#define HTANH		$HTANH
#endif

#ifdef __STDC__

typedef void VOID;

#else

typedef char VOID;
#define const

#endif
