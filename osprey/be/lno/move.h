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
***	Description: This file contains functions to do hoisting and 
***     sinking (mainly for lego tiling and nested doacrosses.) 
***
***	void Hoist_Statement(WN* stat, INT hoist_level);
***
***	    Hoist the statement 'stat' so that it is enclosed by a loop
***	    of level 'hoist_level'.  If 'hoist_level' is -1, hoist 'stat' 
***	    out of all loops.
***
***	INT Hoistable_Statement(WN* stat, DU_MANAGER* du);
***
***	     Returns the depth to which 'stat' can be hoisted.  This is
***	     the depth of the loop enclosing 'stat' if there will be one,  
***	     and -1 if 'stat' can be hoisted out of all loops.
***
***	void Hoist_Statements(WN* outer_loop, DU_MANAGER* du)
***
***	     Hoist all of the statements which are sandwiched code in
***	     'outer_loop' and can be legally hoisted out of the loop.
***
***	BOOL Statement_Sinkable_Out_Of_Loop(WN* wn_stat, WN* wn_loop)
***
***	     Returns TRUE if 'wn_stat' is sinkable out of the loop 'wn_loop'
***	     Returns FALSE if not, or if we can't prove it.
***
***	void Sink_Out_Sandwiched_Statement(WN* wn_stat, WN* wn_sink_loop)
***
***	     Sink the sandwiched statement 'wn_stat' out of the loop 
***	     'wn_sink_loop'.
***
***	BOOL Sandwiched_Code_Sinkable_In(WN* wn_outer_loop, 
***	     WN* wn_sink_loop, DU_MANAGER* du)
***
***	     Returns TRUE if the sandwiched code within 'wn_outer_loop'
***	     and 'wn_sink_loop' can be sunk inside 'wn_sink_loop'.  Returns
***	     FALSE otherwise.
***
***	WN* Sink_Sandwiched_Code_In(WN* wn_outer_loop, WN* wn_sink_loop) 
***
***	     Sink the sandwiched code between 'wn_outer_loop' and
***	     'wn_sink_loop' inside 'wn_sink_loop'.  Returns the first 
***	     statement sunk.
***
***	void SNL_Sink_Out_Sandwiched_Statements(WN* wn_loop, INT nloops,
***	     BOOL test_legality, ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du)
***
***	     Sink the sandwiched code out of the SNL whose outermost loop
***	     is 'wn_loop' and which consists of 'nloops' loops.  If 'test_
***	     legality', test for legality and sink only those statements
***	     which are proved to be sinkable. 
***
*** 	void Hoist_And_Sink_For_Nested_Doacross(WN* wn_loop, 
***	     ARRAY_DIRECTED_GRAPH16* dg, 
***	     DU_MANAGER* du)		
***
***	     Hoist and sink sandwiched code out of the nested doacross
***	     loop 'wn_loop'.
***
***	WN* Find_Sibling_Containing(WN* wn_stat, WN* wn_contained)
***
***	     Returns the sibling of 'wn_stat' after or including 'wn_stat'
***	     which contains 'wn_contained' if there is one, NULL otherwise.
***
*** 	BOOL Maybe_Assigned_Exp(WN* wn_exp, WN* wn_first, WN* wn_last)
***
***	     Returns TRUE if any node in 'wn_exp' may be assigned in the
***	     code segment between 'wn_first' and 'wn_last' inclusive.
**/

extern void Hoist_Statement(WN* wn_stat, INT hoist_level);
extern INT Hoistable_Statement(WN* wn_stat, DU_MANAGER* du);
extern void Hoist_Statements(WN* wn_outer_loop, DU_MANAGER* du);
extern void Hoist_Necessary_Code_Up(WN* wn_sunk, DU_MANAGER* du); 
extern BOOL Statement_Sinkable_Out_Of_Loop(WN* wn_stat, WN* wn_loop);
extern void Sink_Out_Sandwiched_Statement(WN* wn_stat, WN* wn_sink_loop);
extern BOOL Sandwiched_Code_Sinkable_In(WN* wn_outer_loop,
  WN* wn_sink_loop, DU_MANAGER* du);
extern WN* Sink_Sandwiched_Code_In(WN* wn_outer_loop,
  WN* wn_sink_loop);
extern void SNL_Sink_Out_Sandwiched_Statements(WN* wn_loop, INT nloops, 
  BOOL test_legality, ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du);
extern void Hoist_And_Sink_For_Nested_Doacross(WN* wn_loop,
  ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du);
extern WN* Initial_Hoist_Place(WN* wn_stat);
extern WN* Hoist_Merge(WN* wn1, WN* wn2);
extern WN* Hoistable_Place(WN* wn_stat, DU_MANAGER* du);
extern WN* Find_Sibling_Containing(WN* wn_stat, WN* wn_contained);
extern BOOL Maybe_Assigned_Exp(WN* wn_exp, WN* wn_first, WN* wn_last);

