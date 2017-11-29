
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

/* ====================================================================
 * ====================================================================
 *
 * Module: sinhtab.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/mips/sinhtab.c,v $
 *
 * Revision history:
 *  21-Jul-98 - Original Version
 *
 * Description:	constants used by sinh and cosh functions
 *
 * ====================================================================
 * ====================================================================
 */

#include "libm.h"

/* sinh(i*log(2)), i=0, ..., 46 */

const	du	__sinhtab[] =
{
D(0x00000000, 0x00000000),
D(0x3fe80000, 0x00000000),
D(0x3ffe0000, 0x00000000),
D(0x400f8000, 0x00000000),
D(0x401fe000, 0x00000000),
D(0x402ff800, 0x00000000),
D(0x403ffe00, 0x00000000),
D(0x404fff80, 0x00000000),
D(0x405fffe0, 0x00000000),
D(0x406ffff8, 0x00000000),
D(0x407ffffe, 0x00000000),
D(0x408fffff, 0x80000000),
D(0x409fffff, 0xe0000000),
D(0x40afffff, 0xf8000000),
D(0x40bfffff, 0xfe000000),
D(0x40cfffff, 0xff800000),
D(0x40dfffff, 0xffe00000),
D(0x40efffff, 0xfff80000),
D(0x40ffffff, 0xfffe0000),
D(0x410fffff, 0xffff8000),
D(0x411fffff, 0xffffe000),
D(0x412fffff, 0xfffff800),
D(0x413fffff, 0xfffffe00),
D(0x414fffff, 0xffffff80),
D(0x415fffff, 0xffffffe0),
D(0x416fffff, 0xfffffff8),
D(0x417fffff, 0xfffffffe),
D(0x418fffff, 0xffffffff),
D(0x41a00000, 0x00000000),
D(0x41b00000, 0x00000000),
D(0x41c00000, 0x00000000),
D(0x41d00000, 0x00000000),
D(0x41e00000, 0x00000000),
D(0x41f00000, 0x00000000),
D(0x42000000, 0x00000000),
D(0x42100000, 0x00000000),
D(0x42200000, 0x00000000),
D(0x42300000, 0x00000000),
D(0x42400000, 0x00000000),
D(0x42500000, 0x00000000),
D(0x42600000, 0x00000000),
D(0x42700000, 0x00000000),
D(0x42800000, 0x00000000),
D(0x42900000, 0x00000000),
D(0x42a00000, 0x00000000),
D(0x42b00000, 0x00000000),
D(0x42c00000, 0x00000000),
};

/* cosh(i*log(2)), i=0, ..., 46 */

const	du	__coshtab[] =
{
D(0x3ff00000, 0x00000000),
D(0x3ff40000, 0x00000000),
D(0x40010000, 0x00000000),
D(0x40104000, 0x00000000),
D(0x40201000, 0x00000000),
D(0x40300400, 0x00000000),
D(0x40400100, 0x00000000),
D(0x40500040, 0x00000000),
D(0x40600010, 0x00000000),
D(0x40700004, 0x00000000),
D(0x40800001, 0x00000000),
D(0x40900000, 0x40000000),
D(0x40a00000, 0x10000000),
D(0x40b00000, 0x04000000),
D(0x40c00000, 0x01000000),
D(0x40d00000, 0x00400000),
D(0x40e00000, 0x00100000),
D(0x40f00000, 0x00040000),
D(0x41000000, 0x00010000),
D(0x41100000, 0x00004000),
D(0x41200000, 0x00001000),
D(0x41300000, 0x00000400),
D(0x41400000, 0x00000100),
D(0x41500000, 0x00000040),
D(0x41600000, 0x00000010),
D(0x41700000, 0x00000004),
D(0x41800000, 0x00000001),
D(0x41900000, 0x00000000),
D(0x41a00000, 0x00000000),
D(0x41b00000, 0x00000000),
D(0x41c00000, 0x00000000),
D(0x41d00000, 0x00000000),
D(0x41e00000, 0x00000000),
D(0x41f00000, 0x00000000),
D(0x42000000, 0x00000000),
D(0x42100000, 0x00000000),
D(0x42200000, 0x00000000),
D(0x42300000, 0x00000000),
D(0x42400000, 0x00000000),
D(0x42500000, 0x00000000),
D(0x42600000, 0x00000000),
D(0x42700000, 0x00000000),
D(0x42800000, 0x00000000),
D(0x42900000, 0x00000000),
D(0x42a00000, 0x00000000),
D(0x42b00000, 0x00000000),
D(0x42c00000, 0x00000000),
};

