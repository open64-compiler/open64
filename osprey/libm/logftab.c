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
 * Module: logftab.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/logftab.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	tables used by log, log10, and log1p functions
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/logftab.c,v $ $Revision: 1.1.1.1 $";

#include "libm.h"

const fu	_logftabhi[] =
{
{0x00000000},
{0x3bff0000},
{0x3c7e0000},
{0x3cbdc000},
{0x3cfc2000},
{0x3d1cf000},
{0x3d3ba000},
{0x3d5a1000},
{0x3d785000},
{0x3d8b2800},
{0x3d9a1000},
{0x3da8d800},
{0x3db78800},
{0x3dc61800},
{0x3dd49000},
{0x3de2f000},
{0x3df13800},
{0x3dff6800},
{0x3e06bc00},
{0x3e0db800},
{0x3e14ac00},
{0x3e1b9000},
{0x3e226800},
{0x3e293800},
{0x3e2ff800},
{0x3e36b000},
{0x3e3d5c00},
{0x3e43fc00},
{0x3e4a9400},
{0x3e511c00},
{0x3e57a000},
{0x3e5e1400},
{0x3e648000},
{0x3e6ae000},
{0x3e713800},
{0x3e778400},
{0x3e7dc800},
{0x3e820200},
{0x3e851a00},
{0x3e882c00},
{0x3e8b3a00},
{0x3e8e4400},
{0x3e914a00},
{0x3e944a00},
{0x3e974800},
{0x3e9a3e00},
{0x3e9d3200},
{0x3ea02200},
{0x3ea30c00},
{0x3ea5f200},
{0x3ea8d600},
{0x3eabb400},
{0x3eae8e00},
{0x3eb16400},
{0x3eb43600},
{0x3eb70400},
{0x3eb9ce00},
{0x3ebc9600},
{0x3ebf5800},
{0x3ec21600},
{0x3ec4d200},
{0x3ec78800},
{0x3eca3c00},
{0x3eccec00},
{0x3ecf9a00},
{0x3ed24200},
{0x3ed4e800},
{0x3ed78a00},
{0x3eda2800},
{0x3edcc200},
{0x3edf5a00},
{0x3ee1ee00},
{0x3ee48000},
{0x3ee70e00},
{0x3ee99800},
{0x3eec2000},
{0x3eeea400},
{0x3ef12400},
{0x3ef3a200},
{0x3ef61c00},
{0x3ef89400},
{0x3efb0a00},
{0x3efd7a00},
{0x3effea00},
{0x3f012b00},
{0x3f025f00},
{0x3f039200},
{0x3f04c300},
{0x3f05f400},
{0x3f072200},
{0x3f085000},
{0x3f097c00},
{0x3f0aa600},
{0x3f0bcf00},
{0x3f0cf700},
{0x3f0e1e00},
{0x3f0f4300},
{0x3f106700},
{0x3f118a00},
{0x3f12ab00},
{0x3f13cb00},
{0x3f14ea00},
{0x3f160700},
{0x3f172400},
{0x3f183f00},
{0x3f195900},
{0x3f1a7100},
{0x3f1b8900},
{0x3f1c9f00},
{0x3f1db400},
{0x3f1ec800},
{0x3f1fdb00},
{0x3f20ec00},
{0x3f21fd00},
{0x3f230c00},
{0x3f241b00},
{0x3f252800},
{0x3f263400},
{0x3f273f00},
{0x3f284900},
{0x3f295100},
{0x3f2a5900},
{0x3f2b6000},
{0x3f2c6500},
{0x3f2d6a00},
{0x3f2e6e00},
{0x3f2f7000},
{0x3f307200},
{0x3f317200},
};

const fu	_logftablo[] =
{
{0x00000000},
{0x3429ac42},
{0x35a8b0fc},
{0x368d83eb},
{0xb6b278c4},
{0x3687b9ff},
{0x3631ec66},
{0x36dd7119},
{0x35c30046},
{0x365bba8e},
{0xb621a791},
{0x34e7e0c3},
{0xb635d46a},
{0x368bac63},
{0x36da7496},
{0x36a91eb8},
{0x34edc55e},
{0xb6dd9c48},
{0xb44197b9},
{0x36ab54be},
{0xb6b41f80},
{0xb4f7f85c},
{0x36adb32e},
{0xb650e2f2},
{0x36c1c29e},
{0x35fe719d},
{0x3590210e},
{0x36819483},
{0xb6958c2f},
{0x36f07f8b},
{0xb6dac5fd},
{0x354e85b2},
{0xb5838656},
{0x3685ad3f},
{0x356dc55e},
{0x36b72f71},
{0x36436af2},
{0xb6d35a59},
{0xb6d8ec63},
{0x363f9ae5},
{0x36e55d5d},
{0x36c60b4d},
{0x34fde7bd},
{0x36d09ef4},
{0xb6ea28f7},
{0x36ecd4c4},
{0x36455694},
{0xb6779796},
{0x363c21c6},
{0x36fcabbc},
{0xb693c690},
{0xb60e8baa},
{0xb51029fe},
{0x353cae72},
{0x3601e9b1},
{0x366aa2ba},
{0x36bfb5df},
{0xb6d50116},
{0xb5f88faa},
{0x368ed0f4},
{0xb64793ec},
{0x36f439b3},
{0x36a0e109},
{0x36ac08bf},
{0xb6e09a03},
{0x3410e5bb},
{0xb69b2b30},
{0xb6b66dc4},
{0xb6084337},
{0x36c4b499},
{0x3659da72},
{0x36bd3e6d},
{0xb6038656},
{0xb687a3d0},
{0xb4c0ff8a},
{0xb6c6d3af},
{0xb6afd9f2},
{0x3601a7c7},
{0x351875a2},
{0x36ce9234},
{0x3675faf0},
{0xb6e02c7f},
{0x36c47bc8},
{0xb68fbd40},
{0xb6d5a5a3},
{0xb444adb2},
{0xb551f190},
{0x36f4f573},
{0xb6d1bdad},
{0x36985d1d},
{0xb6c61d2b},
{0xb6e6a6c1},
{0x35f4bd35},
{0x36abbd8a},
{0x36568cf9},
{0xb67c11d8},
{0xb4a18fbf},
{0xb5cb9b55},
{0xb6f28414},
{0xb6062ce1},
{0xb576bb27},
{0xb68013d5},
{0x369ed449},
{0xb6bc91c0},
{0xb68ccb0f},
{0xb6cc6ede},
{0x3689d9ce},
{0xb684ab8c},
{0x34d3562a},
{0x36094000},
{0x359a9c56},
{0xb60f65d2},
{0x36fe8467},
{0xb368318d},
{0x36bc21c6},
{0xb6c2e157},
{0xb67449f8},
{0xb64a0662},
{0xb67dc915},
{0xb6c33fe9},
{0x36d265bc},
{0x360cf333},
{0xb6454982},
{0x36db5cd8},
{0x34186b3e},
{0xb6e2393f},
{0x35aa4906},
{0xb6d0bb87},
{0x35bfbe8e},
};

