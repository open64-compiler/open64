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
 * Module: vlogftab.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/vlogftab.c,v $
 *
 * Revision history:
 *  02-Dec-94 - Original Version
 *
 * Description:	tables used by vlogf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/vlogftab.c,v $ $Revision: 1.1.1.1 $";

#include "libm.h"

const du        _logtab[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3f7fe02a, 0x6b106789)},
{D(0x3f8fc0a8, 0xb0fc03e4)},
{D(0x3f97b91b, 0x07d5b11b)},
{D(0x3f9f829b, 0x0e783300)},
{D(0x3fa39e87, 0xb9febd60)},
{D(0x3fa77458, 0xf632dcfc)},
{D(0x3fab42dd, 0x711971bf)},
{D(0x3faf0a30, 0xc01162a6)},
{D(0x3fb16536, 0xeea37ae1)},
{D(0x3fb341d7, 0x961bd1d1)},
{D(0x3fb51b07, 0x3f06183f)},
{D(0x3fb6f0d2, 0x8ae56b4c)},
{D(0x3fb8c345, 0xd6319b21)},
{D(0x3fba926d, 0x3a4ad563)},
{D(0x3fbc5e54, 0x8f5bc743)},
{D(0x3fbe2707, 0x6e2af2e6)},
{D(0x3fbfec91, 0x31dbeabb)},
{D(0x3fc0d77e, 0x7cd08e59)},
{D(0x3fc1b72a, 0xd52f67a0)},
{D(0x3fc29552, 0xf81ff523)},
{D(0x3fc371fc, 0x201e8f74)},
{D(0x3fc44d2b, 0x6ccb7d1e)},
{D(0x3fc526e5, 0xe3a1b438)},
{D(0x3fc5ff30, 0x70a793d4)},
{D(0x3fc6d60f, 0xe719d21d)},
{D(0x3fc7ab89, 0x0210d909)},
{D(0x3fc87fa0, 0x6520c911)},
{D(0x3fc9525a, 0x9cf456b4)},
{D(0x3fca23bc, 0x1fe2b563)},
{D(0x3fcaf3c9, 0x4e80bff3)},
{D(0x3fcbc286, 0x742d8cd6)},
{D(0x3fcc8ff7, 0xc79a9a22)},
{D(0x3fcd5c21, 0x6b4fbb91)},
{D(0x3fce2707, 0x6e2af2e6)},
{D(0x3fcef0ad, 0xcbdc5936)},
{D(0x3fcfb918, 0x6d5e3e2b)},
{D(0x3fd04025, 0x94b4d041)},
{D(0x3fd0a324, 0xe27390e3)},
{D(0x3fd1058b, 0xf9ae4ad5)},
{D(0x3fd1675c, 0xababa60e)},
{D(0x3fd1c898, 0xc16999fb)},
{D(0x3fd22941, 0xfbcf7966)},
{D(0x3fd2895a, 0x13de86a3)},
{D(0x3fd2e8e2, 0xbae11d31)},
{D(0x3fd347dd, 0x9a987d55)},
{D(0x3fd3a64c, 0x556945ea)},
{D(0x3fd40430, 0x8686a7e4)},
{D(0x3fd4618b, 0xc21c5ec2)},
{D(0x3fd4be5f, 0x957778a1)},
{D(0x3fd51aad, 0x872df82d)},
{D(0x3fd57677, 0x17455a6c)},
{D(0x3fd5d1bd, 0xbf5809ca)},
{D(0x3fd62c82, 0xf2b9c795)},
{D(0x3fd686c8, 0x1e9b14af)},
{D(0x3fd6e08e, 0xaa2ba1e4)},
{D(0x3fd739d7, 0xf6bbd007)},
{D(0x3fd792a5, 0x5fdd47a2)},
{D(0x3fd7eaf8, 0x3b82afc3)},
{D(0x3fd842d1, 0xda1e8b17)},
{D(0x3fd89a33, 0x86c1425b)},
{D(0x3fd8f11e, 0x873662c7)},
{D(0x3fd94794, 0x1c2116fb)},
{D(0x3fd99d95, 0x8117e08b)},
{D(0x3fd9f323, 0xecbf984c)},
{D(0xbfd21445, 0x6d0eb8d4)},
{D(0xbfd1bf99, 0x635a6b95)},
{D(0xbfd16b5c, 0xcbacfb73)},
{D(0xbfd1178e, 0x8227e47c)},
{D(0xbfd0c42d, 0x676162e3)},
{D(0xbfd07138, 0x604d5862)},
{D(0xbfd01eae, 0x5626c691)},
{D(0xbfcf991c, 0x6cb3b379)},
{D(0xbfcef5ad, 0xe4dcffe6)},
{D(0xbfce530e, 0xffe71012)},
{D(0xbfcdb13d, 0xb0d48940)},
{D(0xbfcd1037, 0xf2655e7b)},
{D(0xbfcc6ffb, 0xc6f00f71)},
{D(0xbfcbd087, 0x383bd8ad)},
{D(0xbfcb31d8, 0x575bce3d)},
{D(0xbfca93ed, 0x3c8ad9e3)},
{D(0xbfc9f6c4, 0x07089664)},
{D(0xbfc95a5a, 0xdcf7017f)},
{D(0xbfc8beaf, 0xeb38fe8c)},
{D(0xbfc823c1, 0x6551a3c2)},
{D(0xbfc7898d, 0x85444c73)},
{D(0xbfc6f012, 0x8b756abc)},
{D(0xbfc6574e, 0xbe8c133a)},
{D(0xbfc5bf40, 0x6b543db2)},
{D(0xbfc527e5, 0xe4a1b58d)},
{D(0xbfc4913d, 0x8333b561)},
{D(0xbfc3fb45, 0xa59928cc)},
{D(0xbfc365fc, 0xb0159016)},
{D(0xbfc2d161, 0x0c86813a)},
{D(0xbfc23d71, 0x2a49c202)},
{D(0xbfc1aa2b, 0x7e23f72a)},
{D(0xbfc1178e, 0x8227e47c)},
{D(0xbfc08598, 0xb59e3a07)},
{D(0xbfbfe891, 0x39dbd566)},
{D(0xbfbec739, 0x830a1120)},
{D(0xbfbda727, 0x638446a2)},
{D(0xbfbc8858, 0x01bc4b23)},
{D(0xbfbb6ac8, 0x8dad5b1c)},
{D(0xbfba4e76, 0x40b1bc38)},
{D(0xbfb9335e, 0x5d594989)},
{D(0xbfb8197e, 0x2f40e3f0)},
{D(0xbfb700d3, 0x0aeac0e1)},
{D(0xbfb5e95a, 0x4d9791cb)},
{D(0xbfb4d311, 0x5d207eac)},
{D(0xbfb3bdf5, 0xa7d1ee64)},
{D(0xbfb2aa04, 0xa44717a5)},
{D(0xbfb1973b, 0xd1465567)},
{D(0xbfb08598, 0xb59e3a07)},
{D(0xbfaeea31, 0xc006b87c)},
{D(0xbfaccb73, 0xcdddb2cc)},
{D(0xbfaaaef2, 0xd0fb10fc)},
{D(0xbfa894aa, 0x149fb343)},
{D(0xbfa67c94, 0xf2d4bb58)},
{D(0xbfa466ae, 0xd42de3ea)},
{D(0xbfa252f3, 0x2f8d183f)},
{D(0xbfa0415d, 0x89e74444)},
{D(0xbf9c63d2, 0xec14aaf2)},
{D(0xbf984925, 0x28c8cabf)},
{D(0xbf9432a9, 0x25980cc1)},
{D(0xbf902056, 0x58935847)},
{D(0xbf882448, 0xa388a2aa)},
{D(0xbf801015, 0x7588de71)},
{D(0xbf700805, 0x59588b35)},
{D(0x00000000, 0x00000000)},
};

