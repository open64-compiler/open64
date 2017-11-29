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


#ifdef	_UNICOS
#pragma _CRI duplicate _memwcpy as memwcpy
#else
#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
void _memwcpy(long *s, long *s0, int n );
void memwcpy(long *s, long *s0, int n ) { _memwcpy(s, s0, n); }
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak memwcpy = _memwcpy
#endif /* defined(BUILD_OS_DARWIN) */
#endif

#ifndef	_CRAY
#define memwcpy _memwcpy
#endif

void  
memwcpy(long *s, long *s0, int n )
{
	if (n != 0) {
   		register long *s1 = s;
		register long *s2 = s0;
		if (s1 <= s2) {
			do {
				*s1++ = *s2++;
			} while (--n != 0);
		} else {
			s2 += n;
			s1 += n;
			do {
				*--s1 = *--s2;
			} while (--n != 0);
		}
	}
	return;
}
