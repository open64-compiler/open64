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


/* USMID @(#) libf/pxf/pxfstructtable.h	92.0	10/08/98 14:30:10 */

#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <fcntl.h>
#include <grp.h>
#include <utime.h>
#include <pwd.h>
#include <dirent.h>
#include <sys/signal.h>
#include <sys/termios.h>

struct strtble {
        char *str;	/* name of constant */
        int  size;	/* Size of the structure to be allocated */
	int type;	/* This value is stored in the pxftype field */
			/* of the structure returned. */
};

struct sig_set {
	sigset_t	samask;
};

static
struct strtble strtble[] = {
        {"flock",sizeof(struct flock),PXF_FLOCK},
        {"utimbuf",sizeof(struct utimbuf),PXF_UTIMBUF},
        {"utsname",sizeof(struct utsname),PXF_UNAMBUF},
        {"stat",sizeof(struct stat),PXF_STATBUF},
	{"tms",sizeof(struct tms),PXF_TMSBUF},
	{"group",sizeof(struct group),PXF_GROUP},
	{"passwd",sizeof(struct passwd),PXF_PASSWD},
	{"dirent",sizeof(struct dirent),PXF_DIRENT},
	{"sigset",sizeof(struct sig_set),PXF_SIGSET},
	{"sigaction",sizeof(struct sigaction),PXF_SIGACTION},
	{"termios",sizeof(struct termios),PXF_TERMIOS},
	{"subhand",sizeof(long),PXF_SUBHAND},
};
