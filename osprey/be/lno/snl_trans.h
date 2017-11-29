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
*** This file contains functions and classes for the transformation of
*** simply nested loops.  Code specific to general transformation is 
*** given in snl_gen.cxx, while code specific to invariant transformation
*** is given in snl_inv.cxx. 
***
*** The actual transformation theory is discussed in doc/Mongoose
*** /snl-transformation.mif.
***
*** The assumption is that access vectors are up to date upon entry to
*** SNL processing.  I do not keep them up-to-date during the SNL
*** transformation process, because while they are very useful for me to
*** gather the information I need, the information I gather is actually
*** tailored to my needs (SNL_NEST_INFO and SNL_TRANS_INDEX_DATA).
*** But the access vectors are used by SNL_NEST_INFO().
***
*** For loop distribution, a dependence test is supplied, and it's noted
*** where distribution would have to occur to make things legal.  Then, if
*** is a transformation is applied that can benefit by distribution, then
*** distribution is applied at that point.
***
*** UPDATING DU/UD INFORMATION:
***
*** For invariant nests:
***
***     -- interchange involves no changes.
***     -- cache tiling involves making new variables for tile indices,
***        and using them only in bounds further in.  Also, the bounds
***        for the tile index loops themselves.  But all the information
***        is right there.
***     -- regtiling is fine, since as long as everything is up-to-date
***        before, then the copy routines do the right thing.
***
*** For general nests:
***
***     -- Protect_Nest_With_Conditionals: easy, since this only involves
***        defs for the variables in the bounds information.
***     -- U_Ctiling() uses SNL_TRANS_INDEX_DATA, which is augmented
***        to include the defs for each index variable.  So as long as
***        that data structure is passed around when doing copies, no
***        problem.
***     -- General_2D_Regtile.  Again as long as things are up-to-date
***        before this code, no problem.
***
*** TODO: The Fix_Do_Du_Info() is a hack.  Isn't there some better way to
*** update index variable du chains?  Can't I let them all rot and recompute
*** them at the end (they're easy)?  This might be too slow.  It's not well
*** thought out about what the minimal number of calls to it would be.
***
**/

#ifndef snl_trans_INCLUDED
#define snl_trans_INCLUDED "snl_trans.h"

//-----------------------------------------------------------------------
// REDUCTION UTILITIES
//-----------------------------------------------------------------------

extern void SNL_Change_Reduction_Loop_Stmts(SX_PLIST* plist, 
  WN* from, WN* to);

extern void SNL_Expand_Reduction_Deps(WN* loop); 

extern BOOL SNL_Test_Reduction_Lexneg(EINDEX16 e, WN* awn, WN* bwn, 
  INT alex, INT blex);

//-----------------------------------------------------------------------
// ROUTINES FOR USELESS LOOP REMOVAL
//-----------------------------------------------------------------------

extern SNL_REGION SNL_Remove_Unity_Trip_Loop(WN* wdloop, 
  BOOL update_access);

extern SNL_REGION SNL_Remove_Useless_Loops(WN* wn_tree,
  BOOL update_access);

extern void Remove_Useless_Loops(SNL_REGION* region);

//-----------------------------------------------------------------------
// ROUTINES FOR INTERCHANGE
//-----------------------------------------------------------------------

extern void Print_Interchange(FILE* file, WN* outer_loop, INT permutation[],
  INT nloops);

extern void
LNO_FB_Inv_Interchange(WN* wn_outer, INT permutation[], INT nloops);

extern void
LNO_FB_MP_Tile(WN* wn_tile_loop, INT tile_loop_tripcount, WN *wn_orig_loop);

extern WN* SNL_Permute_Loops(WN* wn_outer, WN* wn_inner, INT permutation[],
  INT nloops, BOOL invariant, BOOL warn_lexneg); 

//-----------------------------------------------------------------------
// ROUTINES FOR TILING
//-----------------------------------------------------------------------

extern BOOL SNL_Update_Strip_Dependence(INT current_depth, INT s, INT i_for_s,
  EINDEX16 e, WN* awn, WN* bwn, INT alex, INT blex);

extern WN* Tile_Loop(WN* wn_loop, INT tile_size, INT tile_level, 
  SNL_INV_CACHE_BLOCK_REASON reason, SYMBOL* outersym, MEM_POOL *pool);

//-----------------------------------------------------------------------
// ROUTINES FOR REGISTER TILING
//-----------------------------------------------------------------------

extern SNL_REGION SNL_Regtile_Loop(WN* outerloop, INT u, INT nloops,
  BOOL unroll_just_inner, EST_REGISTER_USAGE est_register_usage,
  SX_INFO* pinfo, INT pinfo_depth, BOOL no_further_unroll,
  HASH_TABLE<WN*,WN*>** loop_map_ptr=NULL, SX_INFO** wdpinfo_ptr=NULL);

#endif
