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


// -*-C++-*-

/**
*** Description:
***
*** This file contains defintions for functions which implement invariant 
*** SNL transformations. 
***
**/ 

#ifndef snl_inv_INCLUDED
#define snl_inv_INCLUDED "snl_inv.h"

extern WN* SNL_INV_Permute_Loops(WN* outer_loop, INT permutation[], 
  INT nloops, BOOL warn_lexneg);

extern WN* SNL_INV_Cache_Block(SNL_NEST_INFO* ni, const SNL_TILE_INFO* t,
  WN* permloop[], LS_IN_LOOP& loop_ls, SNL_REGION* region, 
  SNL_INV_CACHE_BLOCK_REASON reason, SYMBOL* outersym, MEM_POOL* pool);

extern SNL_REGION SNL_INV_Transforms(WN* wn_outer, INT permutation[],
  SNL_NEST_INFO* ni, INT  nloops, SNL_TILE_INFO* t, INT regstripsz[],
  EST_REGISTER_USAGE est_register_usage, BOOL want_se_and_dist, 
  SNL_REGION* old_region, BOOL hoist_outer_invar, SNL_REGION* rg_kernel);

#endif 
