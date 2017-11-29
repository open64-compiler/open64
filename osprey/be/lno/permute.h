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
***     Description:  This file contains functions used to do lego and mp 
***		      permutations. 
***
***	BOOL SNL_Legal_Permutation(WN* outer_loop,
***                                  WN* inner_loop,
***                                  INT permutation[],
***                                  INT nloops,
***                                  BOOL has_removable_branch)
***
***	    Returns TRUE if the SNL defined between loops 'outer_loop'
***	    and 'inner_loop' which consists of 'nloops' loops can be legally
***	    permuted with specified 'permutation'.  Returns FALSE otherwise.
***	    This test is more conservative than the one used in Phase 2
***	    LNO analysis.  We may wish to extend it later.
***
***	INT Permutation_Last(INT first,
***			     INT permutation[],
***                          INT nloops)
***
***	    Returns the last index in the permutation subsequence in
***	    'permutation' which begins with 'first'. (For example, see
***	    permute.cxx) 
***
***     void Mp_Compress(WN* wn_root)
***
***	    Compress the regions of nested doacrosses into single MP
***	    regions.
***
*** 	void Lego_Interchange(WN* wn_root)
***
***	    Attempt to interchange all outer lego tiles outermost in
***	    the function 'wn_root'.
***
***	void Lego_Peel(WN* wn_root)
***
***	    Carry out the peeling commands indicated in the LEGO_INFO
*** 	    of each loop in the function 'wn_root'.
***
**/

extern BOOL SNL_Legal_Permutation(WN* outer_loop,
				  WN* inner_loop, 
                                  INT permutation[], 
				  INT nloops,
                                  BOOL has_removable_branch); 

extern INT Permutation_Last(INT first,
                            INT permutation[],
                            INT nloops);

extern void Mp_Compress_Nested_Loop(WN* wn_loop); 
extern void Lego_Interchange(WN* wn_root); 
extern void Lego_Peel(WN* wn_root); 

