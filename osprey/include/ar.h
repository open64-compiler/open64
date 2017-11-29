/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*

 Copyright (C) 2003 Tensilica, Inc.  All Rights Reserved.

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

*/

#ifdef __MINGW32__

/* We shouldn't have to do this; this file should be used
 * only for non-Cygwin Windows build, but we currently
 * share BUILD_CYGWIN and BUILD_MINGW.
 */
#include </usr/include/ar.h>

#else

#ifndef _AR_H
#define _AR_H 1

/* Archive files start with the ARMAG identifying string.  Then follows a
   `struct ar_hdr', and as many bytes of member file data as its `ar_size'
   member indicates, for each member file.  */

#define ARMAG	"!<arch>\n"	/* String that begins an archive file.  */
#define SARMAG	8		/* Size of that string.  */

#define ARFMAG	"`\n"		/* String in ar_fmag at end of each header.  */

struct ar_hdr
  {
    char ar_name[16];		/* Member file name, sometimes / terminated. */
    char ar_date[12];		/* File date, decimal seconds since Epoch.  */
    char ar_uid[6], ar_gid[6];	/* User and group IDs, in ASCII decimal.  */
    char ar_mode[8];		/* File mode, in ASCII octal.  */
    char ar_size[10];		/* File size, in ASCII decimal.  */
    char ar_fmag[2];		/* Always contains ARFMAG.  */
  };

#endif /* _AR_H */

#endif /* __CYGWIN__ */
