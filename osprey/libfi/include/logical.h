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


/* USMID @(#) libfi/include/logical.h	92.0	10/08/98 14:37:14 */
#if defined(_F_LOG4) && defined(_F_LOG8)
#include <limits.h>
#endif

#define TRUE 1
#define FALSE 0
#define BITS_PER_BYTE       (BITS_PER_WORD / BYTES_PER_WORD)

#if	defined(_F_LOG2) && defined(__mips)
#define LTOB(i,a) \
	((i == 8*CHAR_BIT) ? (_lvtob((*(_f_log8 *) (a)))) : \
	((i == 4*CHAR_BIT) ? (_lvtob((*(_f_log4 *) (a)))) : \
	((i == 2*CHAR_BIT) ? (_lvtob((*(_f_log2 *) (a)))) : \
	((i == 1*CHAR_BIT) ? (_lvtob((*(_f_log1 *) (a)))) : \
	(_lvtob((*(_f_log *) (a))))))))
#elif defined(_F_LOG4) && defined(_F_LOG8)
#define LTOB(i,a) \
  ((i == 4*CHAR_BIT) ? (_lvtob((*(_f_log4 *) (a)))) : (_lvtob((*(_f_log8 *) (a)))))
#else
#define LTOB(i,a) (_lvtob((*(_f_log *) (a))))
#endif

