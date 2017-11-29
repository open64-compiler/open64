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
 * Module: powtab.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/powtab.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	tables used by pow function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/powtab.c,v $ $Revision: 1.1.1.1 $";

#include "libm.h"

/* _log_rulo[k] + _log_ruhi[k] is the reciprocal of
   u[k] = (k + 128)/128.  We use only 9 significant bits
   in the high part, since we eventually will multiply
   by x - u[k], where u[k] is the nearest 128th to
   x.  Note that x - u[k] has at least 9 trailing
   zeroes in its representation.  The last half of
   the tables are actually adjusted to avoid problems
   of computing ln(x) for x near 2.0
*/

const du	_log_rulo[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3f0fc07f, 0x01fc07f0)},
{D(0x3f2f81f8, 0x1f81f820)},
{D(0x3f419679, 0x2909c560)},
{D(0x3f4f07c1, 0xf07c1f08)},
{D(0xbf3fc267, 0xf099fc26)},
{D(0x3f231abf, 0x0b7672a0)},
{D(0x3f4ceb24, 0x0795ceb2)},
{D(0xbf2e1e1e, 0x1e1e1e1e)},
{D(0x3f475b8f, 0xe21a291c)},
{D(0xbf29f894, 0x67e251a0)},
{D(0x3f4ed952, 0xe0b0ce46)},
{D(0x3f2d41d4, 0x1d41d41d)},
{D(0xbf3a5384, 0x89fc5e69)},
{D(0xbf4ea5db, 0xf193d4bb)},
{D(0x3f42cc15, 0x7b864407)},
{D(0x3f2c71c7, 0x1c71c71c)},
{D(0xbf0c3f8f, 0x01c3f8f0)},
{D(0xbf2f8fc7, 0xe3f1f8fc)},
{D(0xbf36a3b3, 0x5fc845a9)},
{D(0xbf383759, 0xf2298376)},
{D(0xbf349e11, 0x2e63a6a8)},
{D(0xbf27e4b1, 0x7e4b17e5)},
{D(0x3efb2036, 0x406c80d9)},
{D(0x3f3435e5, 0x0d79435e)},
{D(0x3f45c06b, 0x15c06b16)},
{D(0xbf4c427e, 0x567109f9)},
{D(0xbf37f2c9, 0x7f2c97f3)},
{D(0x3f2a41a4, 0x1a41a41a)},
{D(0x3f4b4fe5, 0xe92c0686)},
{D(0xbf3b8b57, 0x7e613717)},
{D(0x3f368a77, 0x25080ce1)},
{D(0xbf499999, 0x9999999a)},
{D(0x3f1c9f01, 0x970e4f81)},
{D(0xbf4d3c0c, 0xa4587e6b)},
{D(0x3f1f693a, 0x1c451ab3)},
{D(0xbf48f9c1, 0x8f9c18fa)},
{D(0x3f380c69, 0x80c6980c)},
{D(0xbf3a3784, 0xa062b2e4)},
{D(0x3f4b97c2, 0xaec12653)},
{D(0x3f286186, 0x18618618)},
{D(0xbf3b442a, 0x6a0916b9)},
{D(0xbf4f9f9f, 0x9f9f9fa0)},
{D(0x3f4017f4, 0x05fd017f)},
{D(0x3f07d05f, 0x417d05f4)},
{D(0xbf36efb8, 0xf899e55d)},
{D(0xbf46cdfa, 0x1d6cdfa2)},
{D(0x3f4f7390, 0xd2a6c406)},
{D(0x3f4745d1, 0x745d1746)},
{D(0x3f40a1fd, 0x1b7af017)},
{D(0x3f3702e0, 0x5c0b8170)},
{D(0x3f2f76b4, 0x337c6cb1)},
{D(0x3f26c16c, 0x16c16c17)},
{D(0x3f23cd15, 0x3729043e)},
{D(0x3f268168, 0x16816817)},
{D(0x3f2ec6a5, 0x122f9016)},
{D(0x3f3642c8, 0x590b2164)},
{D(0x3f3fd3b8, 0x0b11fd3c)},
{D(0x3f460581, 0x60581606)},
{D(0x3f4d6ee3, 0x40579d6f)},
{D(0xbf49df51, 0xb3bea367)},
{D(0xbf3fd4a7, 0xf529fd4a)},
{D(0xbf22dcf7, 0xea712dcf)},
{D(0x3f2ed3c5, 0x06b39a23)},
{D(0x3f455555, 0x55555555)},
{D(0xbf4bdadc, 0x2fc054e4)},
{D(0xbf37c0a8, 0xe83f5718)},
{D(0x3f250150, 0x15015015)},
{D(0x3f47829c, 0xbc14e5e1)},
{D(0xbf451de3, 0x6942462c)},
{D(0xbef4afd6, 0xa052bf5b)},
{D(0x3f44e78e, 0xcb419ba9)},
{D(0xbf447ae1, 0x47ae147b)},
{D(0x3f1978fe, 0xb9f34381)},
{D(0x3f4be195, 0x8b67ebb9)},
{D(0xbf34ced1, 0x5703c883)},
{D(0x3f405050, 0x50505050)},
{D(0xbf43fb01, 0x3fb013fb)},
{D(0x3f3165e7, 0x254813e2)},
{D(0xbf49a96e, 0x115062f0)},
{D(0x3f23b13b, 0x13b13b14)},
{D(0xbf4b8f4f, 0x9e027324)},
{D(0x3f238138, 0x13813814)},
{D(0xbf49c830, 0x87e2e1ab)},
{D(0x3f30e7d9, 0x5bc609a9)},
{D(0xbf446e92, 0xa10d387d)},
{D(0x3f3f1a51, 0x5885fb37)},
{D(0xbf3737f6, 0x79737f68)},
{D(0x3f4a12f6, 0x84bda12f)},
{D(0x3ef2e025, 0xc04b8097)},
{D(0xbf4812c9, 0xfb4d812d)},
{D(0x3f4012b4, 0x04ad012b)},
{D(0xbf2bed61, 0xbed61bed)},
{D(0xbf4d3fb5, 0xdd3fb5dd)},
{D(0x3f3a85c4, 0x0939a85c)},
{D(0xbf2dd8f7, 0xf6d0eec8)},
{D(0xbf4b6db6, 0xdb6db6db)},
{D(0x3f4159e2, 0x6af37c05)},
{D(0xbf021fb7, 0x8121fb78)},
{D(0xbf42e3ce, 0x60fc9de3)},
{D(0x3f4c11f7, 0x047dc11f)},
{D(0x3f3779d9, 0xfdc3a219)},
{D(0xbf1f2a4b, 0xafdc61f3)},
{D(0xbf42d6fe, 0xe44b5bfc)},
{D(0x3f4ee584, 0x69ee5847)},
{D(0x3f414e02, 0x328a7012)},
{D(0x3f218118, 0x11811812)},
{D(0xbf2f976b, 0xd8c8714b)},
{D(0xbf438682, 0x2b63cbef)},
{D(0xbf4e83c7, 0xd4cb125d)},
{D(0x3f472044, 0xd72044d7)},
{D(0x3f3ac73a, 0xe9819b50)},
{D(0x3f211111, 0x11111111)},
{D(0xbf20fef0, 0x10fef011)},
{D(0xbf3854a0, 0xcb1b810f)},
{D(0xbf437d5d, 0xc2e5a99d)},
{D(0xbf4a3ac1, 0x0c9714fc)},
{D(0x3f4f9bb0, 0x96771e4d)},
{D(0x3f4a0429, 0xa0429a04)},
{D(0x3f44fce4, 0x04254fce)},
{D(0x3f408421, 0x08421084)},
{D(0x3f393052, 0x3fbe3368)},
{D(0x3f326e97, 0x8d4fdf3b)},
{D(0x3f297f7d, 0x73404146)},
{D(0x3f204104, 0x10410410)},
{D(0x3f1236a3, 0xebc349de)},
{D(0x3f002040, 0x81020408)},
{D(0x3ee01010, 0x10101010)},
{D(0x00000000, 0x00000000)},
};

const fu	_log_ruhi[] =
{
{0x3f800000},
{0x3f7e0000},
{0x3f7c0000},
{0x3f7a0000},
{0x3f780000},
{0x3f768000},
{0x3f748000},
{0x3f728000},
{0x3f710000},
{0x3f6f0000},
{0x3f6d8000},
{0x3f6b8000},
{0x3f6a0000},
{0x3f688000},
{0x3f670000},
{0x3f650000},
{0x3f638000},
{0x3f620000},
{0x3f608000},
{0x3f5f0000},
{0x3f5d8000},
{0x3f5c0000},
{0x3f5a8000},
{0x3f590000},
{0x3f578000},
{0x3f560000},
{0x3f550000},
{0x3f538000},
{0x3f520000},
{0x3f508000},
{0x3f4f8000},
{0x3f4e0000},
{0x3f4d0000},
{0x3f4b8000},
{0x3f4a8000},
{0x3f490000},
{0x3f480000},
{0x3f468000},
{0x3f458000},
{0x3f440000},
{0x3f430000},
{0x3f420000},
{0x3f410000},
{0x3f3f8000},
{0x3f3e8000},
{0x3f3d8000},
{0x3f3c8000},
{0x3f3b0000},
{0x3f3a0000},
{0x3f390000},
{0x3f380000},
{0x3f370000},
{0x3f360000},
{0x3f350000},
{0x3f340000},
{0x3f330000},
{0x3f320000},
{0x3f310000},
{0x3f300000},
{0x3f2f0000},
{0x3f2e8000},
{0x3f2d8000},
{0x3f2c8000},
{0x3f2b8000},
{0x3f2a8000},
{0x3f2a0000},
{0x3f290000},
{0x3f280000},
{0x3f270000},
{0x3f268000},
{0x3f258000},
{0x3f248000},
{0x3f240000},
{0x3f230000},
{0x3f220000},
{0x3f218000},
{0x3f208000},
{0x3f200000},
{0x3f1f0000},
{0x3f1e8000},
{0x3f1d8000},
{0x3f1d0000},
{0x3f1c0000},
{0x3f1b8000},
{0x3f1a8000},
{0x3f1a0000},
{0x3f190000},
{0x3f188000},
{0x3f178000},
{0x3f170000},
{0x3f168000},
{0x3f158000},
{0x3f150000},
{0x3f148000},
{0x3f138000},
{0x3f130000},
{0x3f128000},
{0x3f118000},
{0x3f110000},
{0x3f108000},
{0x3f0f8000},
{0x3f0f0000},
{0x3f0e8000},
{0x3f0e0000},
{0x3f0d0000},
{0x3f0c8000},
{0x3f0c0000},
{0x3f0b8000},
{0x3f0b0000},
{0x3f0a8000},
{0x3f098000},
{0x3f090000},
{0x3f088000},
{0x3f080000},
{0x3f078000},
{0x3f070000},
{0x3f068000},
{0x3f058000},
{0x3f050000},
{0x3f048000},
{0x3f040000},
{0x3f038000},
{0x3f030000},
{0x3f028000},
{0x3f020000},
{0x3f018000},
{0x3f010000},
{0x3f008000},
{0x3f000000},
};

