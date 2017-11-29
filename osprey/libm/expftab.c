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
 * Module: expftab.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/expftab.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	tables used by exp, expm1, and tanh functions
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/expftab.c,v $ $Revision: 1.1.1.1 $";

#include "libm.h"

/* high and low parts of 2**k/32, with the high part chosen
 * so that its low 6 bits are zero.  See the article referenced
 * in file fexp.c.
 */

const fu	_expftabhi[] =
{
{0x3f800000},
{0x3f82cd80},
{0x3f85aac0},
{0x3f889800},
{0x3f8b95c0},
{0x3f8ea440},
{0x3f91c3c0},
{0x3f94f500},
{0x3f983800},
{0x3f9b8d40},
{0x3f9ef540},
{0x3fa27040},
{0x3fa5fec0},
{0x3fa9a140},
{0x3fad5840},
{0x3fb12400},
{0x3fb50500},
{0x3fb8fbc0},
{0x3fbd08c0},
{0x3fc12c40},
{0x3fc56740},
{0x3fc9b9c0},
{0x3fce2480},
{0x3fd2a800},
{0x3fd74500},
{0x3fdbfbc0},
{0x3fe0ccc0},
{0x3fe5b900},
{0x3feac0c0},
{0x3fefe4c0},
{0x3ff52580},
{0x3ffa83c0},
};


const fu	_expftablo[] =
{
{0x00000000},
{0x35531585},
{0x34d9f312},
{0x35e8092e},
{0x3471f546},
{0xb54e9746},
{0x361b9d59},
{0xb602b808},
{0xb5fae724},
{0xb548c556},
{0xb5d9f6e6},
{0x34c0c312},
{0x36354d8b},
{0x3655a754},
{0xb40adeaf},
{0xb5a7e2d5},
{0xb5ccc062},
{0xb605c4e8},
{0xb6630540},
{0x35cca667},
{0xb62f7558},
{0xb49e6474},
{0x35c151f8},
{0x366c8f89},
{0xb4cd4a59},
{0xb5868251},
{0x36776155},
{0x355cef90},
{0x355cfba5},
{0xb54c8465},
{0xb4bab6de},
{0xb5d248dd},
};

/* 2**k/32, in double precision */

const du	_expftab[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff059b0, 0xd3158574)},
{D(0x3ff0b558, 0x6cf9890f)},
{D(0x3ff11301, 0xd0125b51)},
{D(0x3ff172b8, 0x3c7d517b)},
{D(0x3ff1d487, 0x3168b9aa)},
{D(0x3ff2387a, 0x6e756238)},
{D(0x3ff29e9d, 0xf51fdee1)},
{D(0x3ff306fe, 0x0a31b715)},
{D(0x3ff371a7, 0x373aa9cb)},
{D(0x3ff3dea6, 0x4c123422)},
{D(0x3ff44e08, 0x6061892d)},
{D(0x3ff4bfda, 0xd5362a27)},
{D(0x3ff5342b, 0x569d4f82)},
{D(0x3ff5ab07, 0xdd485429)},
{D(0x3ff6247e, 0xb03a5585)},
{D(0x3ff6a09e, 0x667f3bcd)},
{D(0x3ff71f75, 0xe8ec5f74)},
{D(0x3ff7a114, 0x73eb0187)},
{D(0x3ff82589, 0x994cce13)},
{D(0x3ff8ace5, 0x422aa0db)},
{D(0x3ff93737, 0xb0cdc5e5)},
{D(0x3ff9c491, 0x82a3f090)},
{D(0x3ffa5503, 0xb23e255d)},
{D(0x3ffae89f, 0x995ad3ad)},
{D(0x3ffb7f76, 0xf2fb5e47)},
{D(0x3ffc199b, 0xdd85529c)},
{D(0x3ffcb720, 0xdcef9069)},
{D(0x3ffd5818, 0xdcfba487)},
{D(0x3ffdfc97, 0x337b9b5f)},
{D(0x3ffea4af, 0xa2a490da)},
{D(0x3fff5076, 0x5b6e4540)},
};

