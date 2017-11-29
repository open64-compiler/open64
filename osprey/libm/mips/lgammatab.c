
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
 * Module: lgammatab.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/mips/lgammatab.c,v $
 *
 * Revision history:
 *  14-Jan-98 - Original Version
 *
 * Description:	constants used by lgamma function
 *
 * ====================================================================
 * ====================================================================
 */

#include "libm.h"

/* polynomials used to rewrite factors (x-1)*(x-2)*... */

/* 4 <= ak < 5 */

const	du	__P4[4][2] =
{
/* ak = 4 */
D(0x40180000, 0x00000000),
D(0x40140000, 0x00000000),

/* ak = 4.25 */
D(0x401d4000, 0x00000000),
D(0x40160000, 0x00000000),

/* ak = 4.5 */
D(0x40218000, 0x00000000),
D(0x40180000, 0x00000000),

/* ak = 4.75 */
D(0x4024a000, 0x00000000),
D(0x401a0000, 0x00000000),

};

/* ak =  5 <= ak < 6 */

const	du	__P5[4][3] =
{
/* ak = 5 */
D(0x40380000, 0x00000000),
D(0x403a0000, 0x00000000),
D(0x40220000, 0x00000000),

/* ak = 5.25 */
D(0x403f1400, 0x00000000),
D(0x403eb000, 0x00000000),
D(0x40238000, 0x00000000),

/* ak = 5.5 */
D(0x4043b000, 0x00000000),
D(0x4041e000, 0x00000000),
D(0x40250000, 0x00000000),

/* ak = 5.75 */
D(0x40487e00, 0x00000000),
D(0x40449800, 0x00000000),
D(0x40268000, 0x00000000),

};

/*  6 <= ak < 7 */

const	du	__P6[4][4] =
{
/* ak = 6 */
D(0x405e0000, 0x00000000),
D(0x40634000, 0x00000000),
D(0x4051c000, 0x00000000),
D(0x402c0000, 0x00000000),

/* ak = 6.25 */
D(0x40646520, 0x00000000),
D(0x40680600, 0x00000000),
D(0x40547800, 0x00000000),
D(0x402e0000, 0x00000000),

/* ak = 6.5 */
D(0x406b1200, 0x00000000),
D(0x406d8000, 0x00000000),
D(0x40576000, 0x00000000),
D(0x40300000, 0x00000000),

/* ak = 6.75 */
D(0x40719a90, 0x00000000),
D(0x4071dd00, 0x00000000),
D(0x405a7800, 0x00000000),
D(0x40310000, 0x00000000),

};

/* 7 <= ak < 8 */

const	du	__P7[4][5] =
{
/* ak = 7.0 */
D(0x40868000, 0x00000000),
D(0x40905000, 0x00000000),
D(0x40822000, 0x00000000),
D(0x40636000, 0x00000000),
D(0x40340000, 0x00000000),

/* ak = 7.25 */
D(0x408fde02, 0x00000000),
D(0x40955154, 0x00000000),
D(0x4085ff40, 0x00000000),
D(0x4065f400, 0x00000000),
D(0x40354000, 0x00000000),

/* ak = 7.5 */
D(0x4095fea0, 0x00000000),
D(0x409b5a40, 0x00000000),
D(0x408a5e00, 0x00000000),
D(0x4068b000, 0x00000000),
D(0x40368000, 0x00000000),

/* ak = 7.75 */
D(0x409db4d3, 0x00000000),
D(0x40a145ca, 0x00000000),
D(0x408f43c0, 0x00000000),
D(0x406b9400, 0x00000000),
D(0x4037c000, 0x00000000),

};

/* ak = 8.0 */

const	du	__P8[6] =
{
D(0x40b3b000, 0x00000000),
D(0x40bf5c00, 0x00000000),
D(0x40b3f000, 0x00000000),
D(0x409a0400, 0x00000000),
D(0x40727000, 0x00000000),
D(0x403b0000, 0x00000000),
};

/* The coefficients for the various approximations have been combined
 * into two arrays
 */

const du __S0[8][8] =
{
D(0xbfb7f601, 0xdc0888ae), /* s1/t1 approximates gamma on [1.125, 1.375] */
D(0xbc3931cf, 0x103fc3ea),
D(0xbfca6398, 0xb8ba7ab9),
D(0x3fd86026, 0xd7898a33),
D(0x3fca4095, 0x1131940b),
D(0x3f570d14, 0xf8137c3e),
D(0xbf63e765, 0x9536a51e),
D(0x3f564354, 0xf7369fde),


D(0xbfbd203b, 0x725884ab), /* s2/t2 approximates gamma on [1.375, 1.625] */
D(0x3c4e70ee, 0x96bcb2ab),
D(0x3fa08ea8, 0x8ee561b1),
D(0x3fdbd268, 0xcb91303a),
D(0x3fc23001, 0xb1d09386),
D(0x3f2bfea4, 0x66c2e991),
D(0x3f5b7039, 0x48bed0fe),
D(0x3f4a16c7, 0x02e7ffe9),


D(0xbfb4b851, 0x783475f0), /* s3/t3 approximates gamma on [1.625, 1.875] */
D(0x3c3dc380, 0x28f4b85b),
D(0x3fcd1cd7, 0x4df2c579),
D(0x3fdd51f4, 0xf521d21c),
D(0x3fb55d62, 0x78f4b157),
D(0xbf49e705, 0xcd1587dd),
D(0x3f70eb98, 0x3a0596b3),
D(0x3f3468ee, 0x3b8ec056),


/* the next approximation overlaps the ones on both sides, as the
 * approximation s5/t5 isn't accurate enough near 2.125
 */

D(0x00000000, 0x00000000), /* s4/t4 approximates gamma on [1.8125, 2.1875] */
D(0x00000000, 0x00000000),
D(0x3fdb0ee6, 0x072093ce),
D(0x3fde364b, 0xd66f246b),
D(0x3fa6bb2a, 0x0c9a3b9f),
D(0x3f28e0e6, 0x9b00e602),
D(0x3f74c5cb, 0xa4ab2cfc),
D(0xbeb65ad4, 0x8083bed7),


D(0x3fc1063e, 0xd67aaa93), /* s5/t5 approximates gamma on [2.125, 2.375] */
D(0x3c58206c, 0xf14a61fd),
D(0x3fe4c220, 0x0ac4a891),
D(0x3fe0425b, 0x662a7155),
D(0x3f9f36fb, 0x7ffb97d9),
D(0x3f6fc9fa, 0x630cdc99),
D(0x3f762c5d, 0xa3a02c71),
D(0xbf212d00, 0x1d36923d),


D(0x3fd513e9, 0xb51ece40), /* s6/t6 approximates gamma on [2.375, 2.625] */
D(0xbc42569d, 0xed25111f),
D(0x3fede958, 0x5f1a7093),
D(0x3fe28199, 0xc0f921bb),
D(0x3fa1482d, 0x92f9aea9),
D(0x3f84d4ff, 0xba165591),
D(0x3f77ddf2, 0x591e924c),
D(0xbf23cbb8, 0x45eb8626),


D(0x3fe377ae, 0x2db48634), /* s7/t7 approximates gamma on [2.625, 2.875] */
D(0xbc8e5f4f, 0x21f3a11f),
D(0x3ff512ca, 0x0189d3d3),
D(0x3fe5c5ba, 0x13e02dce),
D(0x3fa855bd, 0xb87142ed),
D(0x3f93fc65, 0x10903a08),
D(0x3f7a5615, 0xc5c93763),
D(0xbf1b692d, 0xe2822a20),


D(0x3ff00000, 0x00000000), /* s8/t8 approximates gamma on [2.875, 3.125] */
D(0x00000000, 0x00000000),
D(0x3ffd8773, 0x039049e7),
D(0x3fe96712, 0x053a0c28),
D(0x3fb1462f, 0x13b606f4),
D(0x3fa0e752, 0x41b8ef0e),
D(0x3f7c222c, 0x40984eba),
D(0x3ee92419, 0x3e13b815),
};

const du __T0[8][7] =
{
D(0x3ff00000, 0x00000000), /* t1 */
D(0x3fecbf52, 0xbb1803f0),
D(0xbfbbbc4a, 0xc12a8cc8),
D(0xbfc178fd, 0xd3b8da6e),
D(0x3f93e56d, 0x2ff5d023),
D(0x3f77881b, 0xed47d3a3),
D(0xbf5266d3, 0x62920680),

D(0x3ff00000, 0x00000000), /* t2 */
D(0x3fe3b215, 0x19e7dc1f),
D(0xbfc776d8, 0x3196851e),
D(0xbfb29e12, 0x0d90ad8a),
D(0x3f952bdd, 0x1a0f9975),
D(0x3f52990c, 0x3134d4a6),
D(0xbf409d77, 0xbc91c49c),

D(0x3ff00000, 0x00000000), /* t3 */
D(0x3fd6306c, 0xeb3a6c37),
D(0xbfccce86, 0xa22556f0),
D(0xbf914f65, 0x0fd49d6c),
D(0x3f92c33c, 0xe78f4c05),
D(0xbf611673, 0x4f96d63a),
D(0xbef528cf, 0x8b80b073),

D(0x3ff00000, 0x00000000), /* t4 */
D(0x3fc23ba5, 0xef68fb11),
D(0xbfcd047c, 0x3945c775),
D(0x3f929cb5, 0x55976225),
D(0x3f8bf31d, 0x2068cec9),
D(0xbf6ae6db, 0xec1929f7),
D(0x3f2ccb47, 0xaeef082d),

D(0x3ff00000, 0x00000000), /* t5 */
D(0x3f850d0d, 0xf66a25cc),
D(0xbfca58d2, 0x1d4a3ede),
D(0x3fa17e84, 0xb23f2a2d),
D(0x3f82acbf, 0x93dd1a49),
D(0xbf6a538c, 0xa3417387),
D(0x3f3234c7, 0xf6af15df),

D(0x3ff00000, 0x00000000), /* t6 */
D(0xbfb4e134, 0x133a634e),
D(0xbfc6d4e7, 0x9609f300),
D(0x3fa4cae0, 0x5f8b6351),
D(0x3f7667b2, 0x00b2f3f3),
D(0xbf666ab7, 0xa91127e7),
D(0x3f315c5a, 0x1e89c930),

D(0x3ff00000, 0x00000000), /* t7 */
D(0xbfc47c0f, 0x4b7c5657),
D(0xbfc2ea40, 0xb2144e28),
D(0x3fa617ab, 0xc705784f),
D(0x3f630883, 0x3e658ad0),
D(0xbf61991a, 0x42686ed9),
D(0x3f2e045b, 0x7a054e74),

D(0x3ff00000, 0x00000000), /* t8 */
D(0xbfcf647b, 0xf31ec7db),
D(0xbfbbfe7c, 0x58346ea2),
D(0x3fa64441, 0x47882ec4),
D(0xbf4f176a, 0xfcc09835),
D(0xbf573603, 0x8db0bddf),
D(0x3f26f764, 0x41ad0f41),
};

const du __S1[7] =
{
D(0xbfe2788c, 0xfc6fb619),
D(0x3fd92873, 0x40bece3e),
D(0x3fcea91b, 0xc5464943),
D(0xbfa9ca1e, 0xef411a8a),
D(0xbf824e4f, 0x927d4475),
D(0x3f71e1ab, 0xcc3210eb),
D(0xbf3ede04, 0xce9eb5c6),
};

const du __T1[8] =
{
D(0x3ff00000, 0x00000000),
D(0x3ff08506, 0xfd2bc1bf),
D(0xbfcbe7a5, 0x6282c69b),
D(0xbfcab736, 0x2c5f5015),
D(0x3fac595b, 0x8559c42a),
D(0x3f81e308, 0x0dd9af79),
D(0xbf722323, 0x3e0ca37d),
D(0x3f3ccc71, 0x6e1b2020),
};

