
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

#ifndef BZERO
#define BZERO(p,n) memset(p,0,n)
#endif
#ifndef BCOPY
#define BCOPY(s,d,n) memcpy(d,s,n)
#endif

#ifdef __linux__

/* 
 * On linux these functions are already declared in string.h, and we
 * want to avoid conflicting declarations.
 */ 
#include <string.h>

#else /* __linux__ */

#ifndef __BSTRING_H__
#define __BSTRING_H__
#ifdef __cplusplus
extern "C" {
#endif


#ident "$Revision: 1.1.1.1 $"

#if !defined(_SIZE_T) && !defined(_SIZE_T_)
#define _SIZE_T
#if (_MIPS_SZLONG == 32)
typedef unsigned int	size_t;
#endif
#if (_MIPS_SZLONG == 64)
typedef unsigned long	size_t;
#endif
#endif

extern void	bcopy(const void *, void *, size_t);
extern int	bcmp(const void *, const void *, size_t);
extern void	bzero(void *, size_t);
extern void	blkclr(void *, size_t);

#ifdef __cplusplus
}
#endif
#endif /* !__BSTRING_H__ */

#endif /* __linux__ */
