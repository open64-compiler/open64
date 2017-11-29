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
***
***  Description: 
*** 
***     This file is intended for functions which optimize loops with 
***	small trip counts or reduce the trip counts of loops.  
***	
***  Exported types and functions:
***
***	extern void Remove_Zero_Trip_Loop(WN* wn_loop)
***
***	  Remove the zero trip loop 'wn_loop'
***
***     extern void Remove_Unity_Trip_Loop(WN* wn_loop, BOOL update_access,
***	  WN** wn_first, WN** wn_last, ARRAY_DIRECTED_GRAPH16* dg, 
***	  DU_MANAGER* du, BOOL verify_trip_count)
***
***	  Remove the unity trip loop 'wdloop'.  If 'update_access',
***	  update the access vectors, and 'update_do_depths' update the do
***	  loop depths.  The firsst and last statements in the new code block
***	  are returned in 'wn_first' and 'wn_last'.
***
***	extern DOLOOP_STACK* Finalizable_Loops(WN* wn_loop,
***	  ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du)
***
***	  Returns a DOLOOP_STACK of loops within 'wn_loop' which can
***	  be replaced by their final iteration.
***
***	extern WN* Finalize_Loops(WN* func_nd)
***
***	  If a loop isn't needed, replace the finalizable loops within 
***	  'func_nd' listed in 'stack_final' with their final value and an 
***	  appropriate guard test. 
***
***	extern void Remove_Unity_Trip_Loop_Dep_Update(WN* wn_loop,
***	  ARRAY_DIRECTED_GRAPH16* dg, BOOL will_not_remove_loop)
***
***	  Update dependence graphs assuming wn_loop becomes bad and
***	  the dependence vectors have to be shortened.
***	  If will_not_remove_loop, that means we are only making the
***	  loop bad so the unused dim will be incremented.
***
***     extern WN* SNL_Finalize_Loops(WN* wn_outer_loop, DOLOOP_STACK* 
*** 	  stack_final, ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du)
***
***	  Replace the finalizable loops within 'func_nd' listed in 
*** 	  'stack_final' with their final value and an appropriate guard test. 
***
***	extern void Optimize_Coupled_Loops(WN* wn_tree, DU_MANAGER* du)
***
***	  Perform Unify_Loop() and Trip_Reduce_Loop() on applicable
***       loop pairs, as described in small_trips.cxx  
***
**/

#ifndef small_trips_INCLUDED
#define small_trips_INCLUDED "small_trips.h"

extern void Remove_Zero_Trip_Loop(WN* wn_loop);

extern void Remove_Unity_Trip_Loop(WN* wn_loop, BOOL update_access,
  WN** wn_first, WN** wn_last, ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du, 
  BOOL verify_trip_count=TRUE);

extern DOLOOP_STACK* Finalizable_Loops(WN* wn_loop, 
  ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du);

extern void Finalize_Loops(WN* func_nd); 

extern void Remove_Unity_Trip_Loop_Dep_Update(WN* wn_loop,
  ARRAY_DIRECTED_GRAPH16* dg,
  BOOL will_not_remove_loop=FALSE);

extern void Optimize_Coupled_Loops(WN* wn_tree, DU_MANAGER* du);
extern WN* SNL_Finalize_Loops(WN* wn_outer_loop, DOLOOP_STACK* stack_final,
  ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du);


#endif /* small_trips__INCLUDED */ 

