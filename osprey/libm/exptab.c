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
 * Module: exptab.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/exptab.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	tables used by exp, expm1, and tanh functions
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/exptab.c,v $ $Revision: 1.1.1.1 $";

#include "libm.h"

/* high and low parts of 2**k/32, with the high part chosen
 * so that its low 6 bits are zero.  See the article referenced
 * in file exp.c.
 */

const du	_exptabhi[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff059b0, 0xd3158540)},
{D(0x3ff0b558, 0x6cf98900)},
{D(0x3ff11301, 0xd0125b40)},
{D(0x3ff172b8, 0x3c7d5140)},
{D(0x3ff1d487, 0x3168b980)},
{D(0x3ff2387a, 0x6e756200)},
{D(0x3ff29e9d, 0xf51fdec0)},
{D(0x3ff306fe, 0x0a31b700)},
{D(0x3ff371a7, 0x373aa9c0)},
{D(0x3ff3dea6, 0x4c123400)},
{D(0x3ff44e08, 0x60618900)},
{D(0x3ff4bfda, 0xd5362a00)},
{D(0x3ff5342b, 0x569d4f80)},
{D(0x3ff5ab07, 0xdd485400)},
{D(0x3ff6247e, 0xb03a5580)},
{D(0x3ff6a09e, 0x667f3bc0)},
{D(0x3ff71f75, 0xe8ec5f40)},
{D(0x3ff7a114, 0x73eb0180)},
{D(0x3ff82589, 0x994cce00)},
{D(0x3ff8ace5, 0x422aa0c0)},
{D(0x3ff93737, 0xb0cdc5c0)},
{D(0x3ff9c491, 0x82a3f080)},
{D(0x3ffa5503, 0xb23e2540)},
{D(0x3ffae89f, 0x995ad380)},
{D(0x3ffb7f76, 0xf2fb5e40)},
{D(0x3ffc199b, 0xdd855280)},
{D(0x3ffcb720, 0xdcef9040)},
{D(0x3ffd5818, 0xdcfba480)},
{D(0x3ffdfc97, 0x337b9b40)},
{D(0x3ffea4af, 0xa2a490c0)},
{D(0x3fff5076, 0x5b6e4540)},
};


const du	_exptablo[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3d0a1d73, 0xe2a475b4)},
{D(0x3ceec531, 0x7256e308)},
{D(0x3cf0a4eb, 0xbf1aed93)},
{D(0x3d0d6e6f, 0xbe462876)},
{D(0x3d053c02, 0xdc0144c8)},
{D(0x3d0c3360, 0xfd6d8e0b)},
{D(0x3d009612, 0xe8afad12)},
{D(0x3cf52de8, 0xd5a46306)},
{D(0x3ce54e28, 0xaa05e8a9)},
{D(0x3d011ada, 0x0911f09f)},
{D(0x3d068189, 0xb7a04ef8)},
{D(0x3d038ea1, 0xcbd7f621)},
{D(0x3cbdf0a8, 0x3c49d86a)},
{D(0x3d04ac64, 0x980a8c8f)},
{D(0x3cd2c7c3, 0xe81bf4b7)},
{D(0x3ce92116, 0x5f626cdd)},
{D(0x3d09ee91, 0xb8797785)},
{D(0x3cdb5f54, 0x408fdb37)},
{D(0x3cf28acf, 0x88afab35)},
{D(0x3cfb5ba7, 0xc55a192d)},
{D(0x3d027a28, 0x0e1f92a0)},
{D(0x3cf01c7c, 0x46b071f3)},
{D(0x3cfc8b42, 0x4491caf8)},
{D(0x3d06af43, 0x9a68bb99)},
{D(0x3cdbaa9e, 0xc206ad4f)},
{D(0x3cfc2220, 0xcb12a092)},
{D(0x3d048a81, 0xe5e8f4a5)},
{D(0x3cdc9768, 0x16bad9b8)},
{D(0x3cfeb968, 0xcac39ed3)},
{D(0x3cf9858f, 0x73a18f5e)},
{D(0x3c99d3e1, 0x2dd8a18b)},
};

