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


/* USMID @(#)uts/include/sys/cdefs.h	80.6	02/09/93 09:27:21 */


#ifndef _SYS_CDEFS_H
#define _SYS_CDEFS_H

#if defined(__cplusplus)
#define __BEGIN_DECLS	extern "C" {
#define __END_DECLS	};
#define _DOTDOTDOT ...
#else
#define __BEGIN_DECLS
#define __END_DECLS
#define _DOTDOTDOT
#endif

#define _ANSI_PROTO

#if defined(__STDC__) || defined(__cplusplus)
#define __(_A)	_A
#else
#define __(_A)	()
#endif

/* Allow both new and old form of generic pointer. */
#ifndef _GENERIC_PTR
#define _GENERIC_PTR
#ifdef __STDC__
typedef void *_GPTR;
typedef const void *_GPTR2CONST;
#else
typedef char *_GPTR;
typedef char *_GPTR2CONST;
#endif
#endif /* !_GENERIC_PTR */

/*
 * POSIX P1003.2 specifies that setting _POSIX_C_SOURCE to 1 or 2
 * will have the same effect as setting _POSIX_SOURCE
 */
#if !defined(_POSIX_SOURCE) && (_POSIX_C_SOURCE == 1 || _POSIX_C_SOURCE == 2)
#define _POSIX_SOURCE
#endif

#endif /* !_SYS_CDEFS_H */
