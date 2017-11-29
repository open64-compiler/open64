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
***	void SNL_GEN_Distribute(WN* wn_outer,
***			        INT split_depth, 
***                             INT nloops,
***                             BOOL above_is_distributable,
***                             BOOL below_is_distributable,
***                             WN** wn_new_first,
***                             WN** wn_new_last)
***
***	    Distribute out the imperfect code in the SNL 'wn_outer' of
***	    'nloops' loops, if permissible. The variables 
***	    'above_is_distributable' and 'below_is_distributable' determine 
***	    if the imperfect code above and below the main nest can be 
*** 	    distributed out.  The region of code created is bounded by 
***	    'wn_new_first' and 'wn_new_last'.  If 'split_depth' == -1, 
***	    a complete set of perfect nests are constructed, otherwise, 
***	    the first nest consists of the loops from Do_Loop_Depth(wn_
***	    outer) up to 'split_depth' - 1, and the other nests are as before.
***
***	void SNL_INV_Distribute(WN* wn_outer,
***			       INT split_depth, 
***                            INT nloops,
***                            WN** wn_new_first,
***                            WN** wn_new_last)
***
***	    Distribute out the imperfect code from the invariant SNL
***	    'wn_outer' of 'nloops' loops.  The region of code created
***	    is bounded by 'wn_new_first' and 'wn_new_last'.  If  
***	    'split_depth' == -1, a complete set of perfect nests are 
***	    constructed, otherwise, the first nest consists of the 
***	    loops from Do_Loop_Depth(wn_outer) up to 'split_depth' - 1,
***	    and the other nests are as before.    
***
***	INT Split_Depth(WN* wn_outer, INT nloops)
***
***	    Returns the split depth for the SNL whose outermost loop is
***         'wn_outer' and which has 'nloops' loops.
***
***	BOOL SNL_Permutation_Is_Distributable(WN* wn_outer, 
***         INT permutation[], INT nloops)                   
***                           
***         Returns TRUE if the SNL with outermost loop 'wn_outer' of 
***	    'nloops' loops, can have its imperfect code distributed out 
***         so that 'permutation' can be performed. (See snl_dist.cxx for 
***	    an example). 
***
***	BOOL SNL_Permutation_Needs_Distribution(WN* wn_outer, 
***	    INT permutation[], INT nloops)
***
***	    Returns TRUE if the SNL with outermost loop 'wn_outer' which
***	    has 'nloops' can have its loops permuted according to the 
***	    'permutation' without first applying permutation.  Returns 
***	    FALSE otherwise.
***
***	void SNL_Distribute_For_Permutation(WN* wn_outer, WN* wn_inner, 
***	    INT permutation[], INT nloops, DOLOOP_STACK* new_stack=NULL)
***
***	    Distribute out the required imperfect code sections for the
***	    SNL of 'nloops' loops with outermost loop 'wn_outer' so that 
***	    the 'permutation' can be performed. (See snl_dist.cxx for an 
***	    example). If 'new_stack' is not NULL, place pointers to any 
*** 	    newly created distributends on 'new_stack'.   
***
***	void SNL_Distribute_By_Splitting(WN* wn_outer, WN* wn_inner,
***         INT nloops, INT split_depth, DOLOOP_STACK* stack)
***
***	    Distribute the SNL with outermost loop 'wn_outer' above and
***	    below the loop at depth 'split_depth', and place any new SNLs 
***	    created on the 'stack'.
***
*** 	BOOL SNL_Is_Distributable(WN* wn_outer, INT nloops)
***
***	    Returns TRUE if the SNL with outer loop 'wn_outer' consisting
***	    of 'nloops' loops is distributable above and below.  Returns
***	    FALSE otherwise.
***
***	Code that implements distribution for SNLs.  None of this
***	is "exported" in that all user routines use the interface
***	described in snl.h, and snl.cxx uses these routines for its
***	own purposes.
***
***	BOOL SNL_Is_Distributable(WN* wn_dist, WN* wn_outer, WN* wn_inner, 
***	  BOOL above)
***
***	  For the SNL 'wn_outer' returns TRUE if it is possible to distri-
***	  bute out the sandwiched code between 'wn_outer' and 'wn_inner'.
***	  If 'above', then test if it is possible to distribute out the code
***	  between 'wn_outer' and 'wn_inner' which is ABOVE 'wn_inner', other-
***	  wise test if it is possible to distribute out the code between
***	  'wn_outer' and 'wn_inner' which is BELOW 'wn_inner'.  Dependences
***	  carried on loops outside 'wn_dist' are ignored.
*** 
***	WN* SNL_Distribute(DOLOOP_STACK* stack, INT inner, INT loopd,
***	  BOOL above);
***
***	  stack: the loop stack through the depth of the innermost loop
***	  inner: the inner loop
***	  loopd: the loop to distribute, which should be inner-1
***	  above: distribute the imperfect part above inner
***
***	 Returns new do loop, renumbered, with DU and depgraph fixed.
***
***		FOR outer
***		   s1
***		   FOR inner
***			s2
***		   s3
***	
***	 This routine ignores scalars: privatizability checks occur elsewhere.
***	 This discussion assumes 'above' is true -- we are trying to distribute
***	 out s1.
***	
***	 Return TRUE if
***	   All arcs ending at s1 ("in edges") do not arise from
***	    s2 or s3.  Actually, it's ok for an arc to go from
***	    s2 or s3 to s1 if we are are not distributing the loop
***	    carrying the dependence.  So when we have
***	
***		FOR distribute_me_too
***		  FOR outer
***		    s1
***		    FOR inner
***		      s2
***		    s3
***		  ...
***	
***	    and we are distributing the two outer loops, then any arc from s2
***	    to s1 or s3 to s1 must have a dependence carried outside the
***	    outermost loop being distributed.
***	
***	 WARNING: Notice that this doesn't take into account that if an array
***	 does not have a corresponding vertex, then there are all sorts of
***	 dependences unaccounted for.  The caller of Is_Distributable must take
***	 care of this.  Specifically, this routine can return the wrong answer
***	 if any array nodes in s2 do not have a corresponding vertex.
***	
***	 	outer: the loop containing s1, s2 and s3.
***		inner: the loop just inside the outer.
***		loopd: the depth of the loop that we wish to distribute.
***		above: distribute the stuff above the 'inner' loop
***		dg:    the dependence graph
***
**/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef snl_dist_INCLUDED
#define snl_dist_INCLUDED "snl_dist.h"

#ifdef _KEEP_RCS_ID
static char *snl_dist_rcs_id = snl_dist_INCLUDED "$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef snl_INCLUDED
#include "snl.h"
#endif

extern void SNL_GEN_Distribute(WN* wn_outer, INT split_depth, INT nloops, 
  BOOL above_is_distributable, BOOL below_is_distributable,
  WN** wn_new_first, WN** wn_new_last);

extern void SNL_INV_Distribute(WN* wn_outer, INT split_depth, INT nloops,  
  WN** wn_new_first, WN** wn_new_last);

extern INT Split_Depth(WN* wn_outer, INT nloops);

extern WN* SNL_Distribute(DOLOOP_STACK*	stack, INT inner, INT loopd,
  BOOL above);

extern BOOL SNL_Is_Distributable(WN* wn_dist, WN* wn_outer, WN* wn_inner, 
  BOOL above);

extern BOOL SNL_Permutation_Is_Distributable(WN* wn_outer, 
  INT permutation[], INT nloops); 

extern BOOL SNL_Permutation_Needs_Distribution(WN* wn_outer,
  INT permutation[], INT nloops);

extern void SNL_Distribute_For_Permutation(WN* wn_outer, WN* wn_inner, 
  INT permutation[], INT nloops, DOLOOP_STACK* stack = NULL); 

extern void SNL_Distribute_By_Splitting(WN* wn_outer, WN* wn_inner,
  INT nloops, INT split_depth, DOLOOP_STACK* stack);

extern BOOL SNL_Is_Distributable(WN* wn_outer, INT nloops);

#endif /* snl_dist_INCLUDED */
