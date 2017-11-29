/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libf/pxf/pxfconst.c	92.5	10/04/99 12:48:35"
#include <fortran.h>
#include <string.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <malloc.h>
#include <errno.h>
#include <liberrno.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <sys/termios.h>
#ifdef __mips
#include <sys/ttydev.h>
#endif	/* __mips */

/* Struct ctbl is used to associate the name of the constant (as a string)   */
/* with the value of the constant. */
struct ctbl {
	char *str;
	_f_int val;
};

static
struct ctbl consttbl[] = {
/* Fcntl symbolic constants. From <fcntl.h> */
	{"F_GETLK",F_GETLK},
	{"F_SETLK",F_SETLK},
	{"F_SETLKW",F_SETLKW},
	{"F_RDLCK",F_RDLCK},
	{"F_WRLCK",F_WRLCK},
	{"F_UNLCK",F_UNLCK},
	{"F_DUPFD",F_DUPFD},
	{"F_GETFD",F_GETFD},
	{"F_SETFD",F_SETFD},
	{"F_GETFL",F_GETFL},
	{"F_SETFL",F_SETFL},
#ifdef	_UNICOS
	{"F_SETSB",F_SETSB},
	{"F_SETALF",F_SETALF},
	{"F_CLRALF",F_CLRALF},
#endif	/* _UNICOS */
	{"O_RDONLY",O_RDONLY},
	{"O_WRONLY",O_RDONLY},
	{"O_RDWR",O_RDWR},
	{"O_ACCMODE",O_ACCMODE},
	{"O_NDELAY",O_NDELAY},
	{"O_APPEND",O_APPEND},
	{"O_SYNC",O_SYNC},
	{"O_NONBLOCK",O_NONBLOCK},
#ifdef	_UNICOS
	{"O_RAW",O_RAW},
	{"O_SSD",O_SSD},
#endif	/* _UNICOS */
	{"O_CREAT",O_CREAT},
	{"O_TRUNC",O_TRUNC},
	{"O_EXCL",O_EXCL},
	{"O_NOCTTY",O_NOCTTY},
#ifdef	_UNICOS
	{"O_BIG",O_BIG},
	{"O_PLACE",O_PLACE},
	{"O_RESTART",O_RESTART},
	{"O_ASYNC",O_ASYNC},
	{"O_PTYIGN",O_PTYIGN},
	{"O_SFSXOP",O_SFSXOP},
	{"O_LDRAW",O_LDRAW},
	{"O_WELLFORMED",O_WELLFORMED},
	{"O_SFS_DEFER_TM",O_SFS_DEFER_TM},
/* Values used in fcntl calls. Defined in <sys/stat.h> */
	{"S_ALF_NOGROW",S_ALF_NOGROW},
	{"S_ALF_PARTR",S_ALF_PARTR},
#endif	/* _UNICOS */
/* Values used in chmod calls. Defined in <sys/stat.h> */
	{"S_ISUID",S_ISUID},
	{"S_ISGID",S_ISGID},
	{"S_IRWXU",S_IRWXU},
	{"S_IRUSR",S_IRUSR},
	{"S_IWUSR",S_IWUSR},
	{"S_IXUSR",S_IXUSR},
	{"S_IRWXG",S_IRWXG},
	{"S_IRGRP",S_IRGRP},
	{"S_IWGRP",S_IWGRP},
	{"S_IXGRP",S_IXGRP},
	{"S_IRWXO",S_IRWXO},
	{"S_IROTH",S_IROTH},
	{"S_IWOTH",S_IWOTH},
	{"S_IXOTH",S_IXOTH},
/* Errnos. From <errno.h> */
	{"EPERM",EPERM},
	{"ENOENT",ENOENT},
	{"ESRCH",ESRCH},
	{"EINTR",EINTR},
	{"EIO",EIO},
	{"ENXIO",ENXIO},
	{"E2BIG",E2BIG},
	{"ENOEXEC",ENOEXEC},
	{"EBADF",EBADF},
	{"ECHILD",ECHILD},
	{"EAGAIN",EAGAIN},
	{"ENOMEM",ENOMEM},
	{"EACCES",EACCES},
	{"EFAULT",EFAULT},
	{"ENOTBLK",ENOTBLK},
	{"EBUSY",EBUSY},
	{"EEXIST",EEXIST},
	{"EXDEV",EXDEV},
	{"ENODEV",ENODEV},
	{"ENOTDIR",ENOTDIR},
	{"EISDIR",EISDIR},
	{"EINVAL",EINVAL},
	{"ENFILE",ENFILE},
	{"EMFILE",EMFILE},
	{"ENOTTY",ENOTTY},
	{"ETXTBSY",ETXTBSY},
	{"EFBIG",EFBIG},
	{"ENOSPC",ENOSPC},
	{"ESPIPE",ESPIPE},
	{"EROFS",EROFS},
	{"EMLINK",EMLINK},
	{"EPIPE",EPIPE},
	{"EDOM",EDOM},
	{"ERANGE",ERANGE},
	{"ENOMSG",ENOMSG},
	{"EIDRM",EIDRM},
#if defined(BUILD_OS_DARWIN)
#else /* defined(BUILD_OS_DARWIN) */
	{"ECHRNG",ECHRNG},
	{"EL2NSYNC",EL2NSYNC},
	{"EL3HLT",EL3HLT},
	{"EL3RST",EL3RST},
	{"ELNRNG",ELNRNG},
	{"EUNATCH",EUNATCH},
	{"ENOCSI",ENOCSI},
	{"EL2HLT",EL2HLT},
#endif /* defined(BUILD_OS_DARWIN) */
	{"EDEADLK",EDEADLK},
	{"ENOLCK",ENOLCK},
#ifdef	_UNICOS
	{"EINVFS",EINVFS},
	{"EFILECH",EFILECH},
	{"EFILERM",EFILERM},
	{"ERFLOCK",ERFLOCK},
	{"ENOSDS",ENOSDS},
	{"EFILESH",EFILESH},
	{"EMALFORMED",EMALFORMED},
	{"EFOREIGNFS",EFOREIGNFS},
	{"EQUSR",EQUSR},
	{"EQGRP",EQGRP},
	{"EQACT",EQACT},
#endif	/* _UNICOS */
#ifdef __mips
	{"EBADE",EBADE},
	{"EBADR",EBADR},
	{"EXFULL",EXFULL},
	{"ENOANO",ENOANO},
	{"EBADRQC",EBADRQC},
	{"EBADSLT",EBADSLT},
	{"EDEADLOCK",EDEADLOCK},
	{"EBFONT",EBFONT},
	{"ENOSTR",ENOSTR},
	{"ENODATA",ENODATA},
	{"ETIME",ETIME},
	{"ENOSR",ENOSR},
	{"ENONET",ENONET},
	{"ENOPKG",ENOPKG},
#endif
	{"EREMOTE",EREMOTE},
#ifdef  __mips
	{"ENOLINK",ENOLINK},
	{"EADV",EADV},
	{"ESRMNT",ESRMNT},
	{"ECOMM",ECOMM},
	{"EPROTO",EPROTO},
#endif
	{"EMULTIHOP",EMULTIHOP},
#ifdef	_UNICOS
	{"EPROCLIM",EPROCLIM},
	{"EMEMLIM",EMEMLIM},
	{"EDISKLIM",EDISKLIM},
	{"ETOOMANYU",ETOOMANYU},
#endif	/* _UNICOS */
#ifdef  __mips
	{"EBADMSG",EBADMSG},
#endif
	{"ENAMETOOLONG",ENAMETOOLONG},
#ifdef  __mips
	{"EOVERFLOW",EOVERFLOW},
	{"ENOTUNIQ",ENOTUNIQ},
	{"EBADFD",EBADFD},
	{"EREMCHG",EREMCHG},
	{"ELIBACC",ELIBACC},
	{"ELIBBAD",ELIBBAD},
	{"ELIBSCN",ELIBSCN},
	{"ELIBMAX",ELIBMAX},
	{"ELIBEXEC",ELIBEXEC},
	{"EILSEQ",EILSEQ},
#endif
	{"ENOSYS",ENOSYS},
#ifdef __mips
	{"ERESTART",ERESTART},
	{"ESTRPIPE",ESTRPIPE},
#endif
	{"ENOTEMPTY",ENOTEMPTY},
#ifdef	_UNICOS
	{"ERENAMESELF",ERENAMESELF},
#endif	/* _UNICOS */
	{"ELOOP",ELOOP},
#ifdef __mips
	{"EUSERS",EUSERS},
	{"ENOTSOCK",ENOTSOCK},
	{"EDESTADDRREQ",EDESTADDRREQ},
	{"EMSGSIZE",EMSGSIZE},
	{"EPROTOTYPE",EPROTOTYPE},
	{"ENOPROTOOPT",ENOPROTOOPT},
#endif
/* Errnos from <liberrno.h> */
	{"ENONAME",ENONAME},
	{"ENOHANDLE",ENOHANDLE},
	{"ETRUNC",ETRUNC},
	{"EARRAYLEN",EARRAYLEN},
	{"EEND",EEND},
	{"EBADHANDLE",EBADHANDLE},
	{"EBADID",EBADID},
/* Posix 1003.9 constants from table 8.9 */
	{"STDIN_FILENO",0},
	{"STDOUT_FILENO",1},
	{"STDERR_FILENO",2},
/* From <stdio.h>. Used in fcntl */
	{"SEEK_SET",SEEK_SET},
	{"SEEK_CUR",SEEK_CUR},
	{"SEEK_END",SEEK_END},
/* From <unistd.h>. Used in pxfaccess */
	{"R_OK",R_OK},
	{"W_OK",W_OK},
	{"X_OK",X_OK},
	{"F_OK",F_OK},
/* From <unistd.h>. Used in pxfsysconf */
	{"ARG_MAX",_SC_ARG_MAX},
	{"CHILD_MAX",_SC_CHILD_MAX},
	{"CLK_TCK",_SC_CLK_TCK},
	{"NGROUPS_MAX",_SC_NGROUPS_MAX},
	{"OPEN_MAX",_SC_OPEN_MAX},
	{"STREAM_MAX",_SC_STREAM_MAX},
	{"TZNAME_MAX",_SC_TZNAME_MAX},
	{"_POSIX_JOB_CONTROL",_SC_JOB_CONTROL},
	{"_POSIX_SAVED_IDS",_SC_SAVED_IDS},
	{"_POSIX_VERSION",_SC_VERSION},
/* Values used in wait and waitpid calls. Defined in <sys/wait.h> */
	{"WNOHANG",WNOHANG},
	{"WUNTRACED",WUNTRACED},
#ifdef _UNICOS
	{"WMTWAIT",WMTWAIT},
	{"WLWPWAIT",WLWPWAIT},
#endif /* UNICOS */
/* Posix 1003.9 constants from Section 2.9.1 */
	{"STDIN_UNIT",100},
	{"STDOUT_UNIT",101},
	{"STDERR_UNIT",102},
/* values from sys/signal.h for pxfsigprocmask */
#ifdef __mips
        {"SIG_NOP",SIG_NOP},
#endif /* mips */
        {"SIG_BLOCK",SIG_BLOCK},
        {"SIG_UNBLOCK",SIG_UNBLOCK},
        {"SIG_SETMASK",SIG_SETMASK},
/* values from sys/signal.h for pxfkill and pxfsigaction, */
        {"SIGHUP",SIGHUP},
        {"SIGINT",SIGINT},
        {"SIGQUIT",SIGQUIT},
        {"SIGILL",SIGILL},
        {"SIGTRAP",SIGTRAP},
        {"SIGABRT",SIGABRT},
        {"SIGIOT",SIGIOT},
#ifdef _UNICOS
        {"SIGHWE",SIGHWE},
        {"SIGERR",SIGERR},
#endif	/* _UNICOS */
#ifndef	_LITTLE_ENDIAN
        {"SIGEMT",SIGEMT},
#endif	/* not _LITTLE_ENDIAN */
        {"SIGFPE",SIGFPE},
        {"SIGKILL",SIGKILL},
        {"SIGBUS",SIGBUS},
#ifdef _UNICOS
        {"SIGPRE",SIGPRE},
        {"SIGORE",SIGORE},
#endif	/* _UNICOS */
        {"SIGSEGV",SIGSEGV},
#ifndef	_LITTLE_ENDIAN
        {"SIGSYS",SIGSYS},
#endif	/* not _LITTLE_ENDIAN */
        {"SIGPIPE",SIGPIPE},
        {"SIGALRM",SIGALRM},
        {"SIGTERM",SIGTERM},
        {"SIGIO",SIGIO},
        {"SIGURG",SIGURG},
#if ! defined(BUILD_OS_DARWIN)
        {"SIGCLD",SIGCLD},
#endif /* defined(BUILD_OS_DARWIN) */
        {"SIGCHLD",SIGCHLD},
#if ! defined(BUILD_OS_DARWIN)
        {"SIGPWR",SIGPWR},
#endif /* defined(BUILD_OS_DARWIN) */
#ifdef _UNICOS
        {"SIGMT",SIGMT},
        {"SIGMTKILL",SIGMTKILL},
        {"SIGBUFIO",SIGBUFIO},
        {"SIGRECOVERY",SIGRECOVERY},
        {"SIGUME",SIGUME},
        {"SIGDLK",SIGDLK},
        {"SIGCPULIM",SIGCPULIM},
        {"SIGSHUTDN",SIGSHUTDN},
#endif	/* _UNICOS */
        {"SIGSTOP",SIGSTOP},
        {"SIGTSTP",SIGTSTP},
        {"SIGCONT",SIGCONT},
        {"SIGTTIN",SIGTTIN},
        {"SIGTTOU",SIGTTOU},
        {"SIGWINCH",SIGWINCH},
#ifdef _UNICOS
        {"SIGRPE",SIGRPE},
        {"SIGWRBKPT",SIGWRBKPT},
        {"SIGNOBDM",SIGNOBDM},
        {"SIGAMI",SIGAMI},
        {"SIGSMCE",SIGSMCE},
        {"SIGINFO",SIGINFO},
#endif	/* _UNICOS */
        {"SIGUSR1",SIGUSR1},
        {"SIGUSR2",SIGUSR2},
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#if ! defined(BUILD_OS_DARWIN)
        {"SIGPOLL",SIGPOLL},
#endif /* defined(BUILD_OS_DARWIN) */
        {"SIGVTALRM",SIGVTALRM},
        {"SIGPROF",SIGPROF},
        {"SIGXCPU",SIGXCPU},
        {"SIGXFSZ",SIGXFSZ},
#ifndef	_LITTLE_ENDIAN
        {"SIGCKPT",SIGCKPT},
        {"SIGRTMIN",SIGRTMIN},
        {"SIGRTMAX",SIGRTMAX},
#endif	/* not _LITTLE_ENDIAN */
#endif	/*  __mips or _LITTLE_ENDIAN */
        {"SIG_DFL",(long) SIG_DFL},
        {"SIG_IGN",(long) SIG_IGN},
/* sa_flags values in sys/signal.h for sigaction */
        {"SA_NOCLDSTOP",SA_NOCLDSTOP},
#ifndef	_LITTLE_ENDIAN
        {"SA_RESETHAND",SA_RESETHAND},
        {"SA_NODEFER",SA_NODEFER},
        {"SA_NOCLDWAIT",SA_NOCLDWAIT},
#endif	/* not _LITTLE_ENDIAN */
#ifdef _UNICOS
        {"SA_CLEARPEND",SA_CLEARPEND},
        {"SA_CLEARMASK",SA_CLEARMASK},
        {"SA_WAKEUP",SA_WAKEUP},
        {"SA_REGMTASK",SA_REGMTASK},
        {"SA_REGLWP",SA_REGLWP},
        {"SA_SIGOFFHAND",SA_SIGOFFHAND},
#elif	!defined(_LITTLE_ENDIAN)
        {"SA_ONSTACK",SA_ONSTACK},
        {"SA_RESTART",SA_RESTART},
        {"SA_SIGINFO",SA_SIGINFO},
#endif	/* _UNICOS */
/* c_iflag input mode values in sys/termios.h for pxfcf... */
        {"IGNBRK",IGNBRK},
        {"BRKINT",BRKINT},
        {"IGNPAR",IGNPAR},
        {"PARMRK",PARMRK},
        {"INPCK",INPCK},
        {"ISTRIP",ISTRIP},
        {"INLCR",INLCR},
        {"IGNCR",IGNCR},
        {"ICRNL",ICRNL},
#if ! defined(BUILD_OS_DARWIN)
        {"IUCLC",IUCLC},
#endif /* defined(BUILD_OS_DARWIN) */
        {"IXON",IXON},
        {"IXANY",IXANY},
        {"IXOFF",IXOFF},
#ifdef	__mips
        {"IMAXBEL",IMAXBEL},
        {"IBLKMD",IBLKMD},
#elif	defined(_LITTLE_ENDIAN)
        {"IMAXBEL",IMAXBEL},
#endif	/* __mips or _LITTLE_ENDIAN */
/* c_oflag output mode values in sys/termios.h for pxfcf... */
        {"OPOST",OPOST},
#if ! defined(BUILD_OS_DARWIN)
        {"OLCUC",OLCUC},
#endif /* defined(BUILD_OS_DARWIN) */
        {"ONLCR",ONLCR},
        {"OCRNL",OCRNL},
        {"ONOCR",ONOCR},
        {"ONLRET",ONLRET},
        {"OFILL",OFILL},
        {"OFDEL",OFDEL},
/* c_cflag control mode values in sys/termios.h for pxfcf... */
        {"CSIZE",CSIZE},
        {"CS5",CS5},
        {"CS6",CS6},
        {"CS7",CS7},
        {"CS8",CS8},
        {"CSTOPB",CSTOPB},
        {"CREAD",CREAD},
        {"PARENB",PARENB},
        {"PARODD",PARODD},
        {"HUPCL",HUPCL},
        {"CLOCAL",CLOCAL},
/* c_lflag local mode values in sys/termios.h for pxfcf... */
        {"ISIG",ISIG},
        {"ICANON",ICANON},
        {"ECHOE",ECHOE},
        {"ECHOK",ECHOK},
        {"ECHONL",ECHONL},
        {"NOFLSH",NOFLSH},
        {"TOSTOP",TOSTOP},
        {"IEXTEN",IEXTEN},
/* c_cc special control chars values in sys/termios.h for pxfcf... */
        {"VINTR",VINTR},
        {"VQUIT",VQUIT},
        {"VERASE",VERASE},
        {"VKILL",VKILL},
        {"VEOF",VEOF},
        {"VEOL",VEOL},
        {"VEOL2",VEOL2},
        {"VMIN",VMIN},
        {"VTIME",VTIME},
#if ! defined(BUILD_OS_DARWIN)
#ifdef	_LITTLE_ENDIAN
        {"VSWTC",VSWTC},
#else
        {"VSWTCH",VSWTCH},
#endif	/* _LITTLE_ENDIAN */
#endif	/* _LITTLE_ENDIAN */
        {"VSUSP",VSUSP},
        {"VSTART",VSTART},
        {"VSTOP",VSTOP},
#ifndef	_LITTLE_ENDIAN
        {"VDSUSP",VDSUSP},
#endif	/* not _LITTLE_ENDIAN */
        {"VREPRINT",VREPRINT},
        {"VDISCARD",VDISCARD},
        {"VWERASE",VWERASE},
        {"VLNEXT",VLNEXT},
#ifdef __mips
        {"VRPRNT",VRPRNT},
        {"VFLUSHO",VFLUSHO},
#endif /* __mips */
/* baud rate values from sys/termios.h for pxfcf... */
/* These are in ttydev.h on IRIX systems */
        {"B0",B0},
        {"B50",B50},
        {"B75",B75},
        {"B110",B110},
        {"B134",B134},
        {"B150",B150},
        {"B200",B200},
        {"B300",B300},
        {"B600",B600},
        {"B1200",B1200},
        {"B1800",B1800},
        {"B2400",B2400},
        {"B4800",B4800},
        {"B9600",B9600},
        {"B19200",B19200},
        {"B38400",B38400},
/* array size of control characters from sys/termios.h for pxfcf... */
        {"NCCS",NCCS},
/* Optional actions values for tcsetattr() function for pxftcsetattr */
        {"TCSANOW",TCSANOW},
        {"TCSADRAIN",TCSADRAIN},
        {"TCSAFLUSH",TCSAFLUSH},
/* Queue selector values for tcflush() function for pxftcflush */
        {"TCIFLUSH",TCIFLUSH},
        {"TCOFLUSH",TCOFLUSH},
        {"TCIOFLUSH",TCIOFLUSH},
/* Action values for tcflow() function for pxftcflow */
        {"TCOOFF",TCOOFF},
        {"TCOON",TCOON},
        {"TCIOFF",TCIOFF},
        {"TCION",TCION},
};

#define NUMCONST sizeof(consttbl)/sizeof(struct ctbl)

/*
 * Description:
 *    PXFCONST returns the value associated with "constname" in ival.
 * Standard:
 *    Section 8.2.1 of Posix 1003.9-1992
 * Parameters:
 *    constname (input)  a character string representing the name of a constant
 *	 		 It is case-sensitive, and trailing blanks are
 *			 ignored.
 *    ival	(output) integer value associated with constname
 *    ierr	(output) error code
 */
#ifdef _UNICOS
void
PXFCONST(
#else
void
_PXFCONST(
#endif
	  _fcd constname,
	  _f_int *ival,
	  _f_int *ierr
)
{
	char *constnm;
	int i;

	/* Strip trailing blanks. */
	constnm = _fc_acopy(constname);
	if (constnm == NULL) {
		*ierr = ENOMEM;
		return;
	}
	*ierr = 0;
	for (i = 0; i < NUMCONST; i++) {
		if (strcmp(constnm,consttbl[i].str) == 0) {
			*ival = consttbl[i].val;
			break;
		}
	}
	if (i == NUMCONST) {
		*ierr = ENONAME;
	}
	free(constnm);
}

#ifndef _UNICOS
void
pxfconst_(
	  char *constname,
	  _f_int *ival,
	  _f_int *ierr,
	  _f_int constnamelen
)
{
  _PXFCONST(_cptofcd(constname, constnamelen), ival, ierr);
}
#endif

/*
 * Description:
 *    Returns the value associated with "constname". No error checking
 * Standard:
 *    Section 8.2.1 of Posix 1003.9-1992
 * Parameters:
 *    constname (input)  a character string representing the name of a
 *	 		 constant. It is case-sensitive, and trailing blanks
 *			 are ignored.
 */

#ifdef _UNICOS
_f_int
IPXFCONST(
#else
_f_int
_IPXFCONST(
#endif
	   _fcd constname
)
{
	_f_int ival;
	_f_int ierr;
#ifdef _UNICOS
	PXFCONST(constname, &ival, &ierr);
#else
	_PXFCONST(constname, &ival, &ierr);
#endif
	if (ierr != 0) {
		return(-1);
	}
	return(ival);
}

#ifndef _UNICOS
_f_int
ipxfconst_(
	   char *constname,
	   _f_int constnamelen
)
{
  return _IPXFCONST(_cptofcd(constname, constnamelen));
}
#endif



/*
 * Description:
 *    Returns .TRUE. if and only if IPXFCONST() would return a valid
 *    value for the same "constname".
 * Standard:
 *    Section 8.2.1 of Posix 1003.9-1992
 * Parameters:
 *    constname (input)  a character string representing the name of a 
 *	 		 constant. It is case-sensitive, and trailing blanks
 *			 are ignored.
 */

#ifdef _UNICOS
_f_log
PXFISCONST(
#else
_f_log
_PXFISCONST(
#endif
	    _fcd constname
)
{
	_f_int ival;
	_f_int ierr;
#ifdef _UNICOS
	PXFCONST(constname, &ival, &ierr);
#else
	_PXFCONST(constname, &ival, &ierr);
#endif
	if (ierr == 0) {
		return(_btol(1)); /* TRUE */
	}
	return(_btol(0)); /* FALSE */
}


#ifndef _UNICOS
_f_log
pxfisconst_(
	    char *constname,
	    _f_int constnamelen
)
{
  return _PXFISCONST( _cptofcd(constname, constnamelen));
}
#endif
