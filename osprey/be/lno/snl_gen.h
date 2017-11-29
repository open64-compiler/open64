/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
*** This file contains defintions for functions which implement general 
*** SNL transformations.
***
**/

#ifndef snl_gen_INCLUDED
#define snl_gen_INCLUDED "snl_gen.h"

extern void SNL_Peel_Iteration(WN* wn, BOOL first_iter); 
extern void SNL_Peel_Iteration_Inner(WN* wn, WN* stmt, 
                                     BOOL exclusion, BOOL first_iter); 

extern SNL_REGION SNL_GEN_Protect_Nest_With_Conditionals(const SNL_NEST_INFO* 
  ni, BOOL* failed);

extern SNL_REGION SNL_GEN_Distribution(WN* wn_outer, IMAT* unimodular,
  SNL_TILE_INFO* ti, INT nloops, BOOL find_split_depth, SX_PLIST* plist,
  BOOL above_is_distributable, BOOL below_is_distributable);

extern SNL_REGION SNL_GEN_U_Ctiling(WN* wn_outer, INT nloops, IMAT* u,
  SNL_TILE_INFO* t, SNL_BOUNDS_INFO* bi, SX_PLIST* plist,
  EST_REGISTER_USAGE est_register_usage, BOOL warn_lexneg);

extern WN* SNL_GEN_Permute_Loops(WN* wn_outer, INT permutation[], INT nloops,
  BOOL warn_lexneg);

extern SNL_REGION SNL_GEN_2D_Regtile(SNL_NEST_INFO* ni, INT tilesz);

#endif
