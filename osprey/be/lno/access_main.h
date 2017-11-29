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


//-*-c++-*-
//------------------------------------------------------------------------
//    LNO_Build_Access(WN *func_nd,MEM_POOL *pool, BOOL Hoist_Bounds)
//
//		Build the access array for all the array statments and
//		all the do loops in the function.  Attach them to the
//		code using LNO_Info_Map. If 'Hoist_Bounds' is TRUE, 
//		promote bounds so that access vectors which are too messy 
//		are expessed in terms of promoted bounds.  
//
//    void LNO_Build_Access(WN *wn, DOLOOP_STACK *stack, MEM_POOL *pool,
//		INDX_RANGE_STACK *irs=0, BOOL Hoist_Bounds)
//
//		Build the access arrays for all the array statements and
//		all the do loops descended from wn.  stack must contain
//		all the outer do loops.  The bounds of all the outer loops
//		must be set.  irs, if it's set, is used to get bounds
//		on the loops using the array index expressions. 
//		Promote_Access same as above. 
//
//    void LNO_Build_Do_Access(WN *wn, DOLOOP_STACK *stack, BOOL Hoist_Bounds)
//
//		Build the access arrays for the bounds of the do loop headed
//		at wn.  Map the wn. 'Hoist_Bounds' has the same meaning as 
//		is in LNO_Build_Access.                
//
//    void LNO_Build_If_Access(WN *wn, DOLOOP_STACK *stack)
//
//		Build the access arrays for the if statement 'wn'.
//		Map the wn.
//
//    void LNO_Build_Access_Array(WN *wn, DOLOOP_STACK *stack, MEM_POOL *pool,
//				INDX_RANGE_STACK *irs=0)
//
//    void LNO_Print_One_Access(FILE *fp, WN* wn)
//       
//              Print a single access vector 
//	
//    LNO_Print_Access(FILE *fp,WN *func_nd)
//
//		Print all the access vectors in the routine
//
//    BOOL Bound_Is_Too_Messy(ACCESS_ARRAY* aa) 
//
//              Returns TRUE if the access array 'aa' has Too_Messy 
//              set globally or on any of its dimensions. 
//
//    BOOL Hoist_Lower_Bound(WN* wn_loop, DOLOOP_STACK* stack, 
//                           MEM_POOL* pool)
//
//		Hoist the lower bound of the loop 'wn_loop' and put it in 
// 		a statement in front of the loop. The 'stack' is a stack of 
//    		loops enclosing 'wn_loop'.  The 'pool' is used to rebuild 
//		the access vector. Returns FALSE if we could not promote the 
//		lower bound, TRUE if we could.  
//
//    BOOL Hoist_Upper_Bound(WN* wn_loop, DOLOOP_STACK* stack, 
//                           MEM_POOL* pool)
//
//		Hoist the upper bound of the loop 'wn_loop' and put it in 
// 		a statement in front of the loop. The 'stack' is a stack of 
//    		loops enclosing 'wn_loop'.  The 'pool' is used to rebuild 
//		the access vector. Returns FALSE if we could not promote the 
//		upper bound, TRUE if we could.  
//
//	void Hoist_Bounds_One_Level(WN* wn_tree)
//
//		If bounds any loop of 'wn_tree are "Too Messy", put them 
//		into a temp and recompute the access vectors in terms of 
//		the new temp value. 
//
//	void Hoist_Iload_Ldid_Upper_Bound_One_Level(WN* loop,
//		BOOL negative_stride)
//
//		If bounds 'loop' has ILOAD-LDID from lego-tiling, put it
//		into a temp and recompute the access vectors in terms of 
//		the new temp value. 
//
//	void Hoist_Varying_Lower_Bounds(WN* func_nd)
//
//		Put any loop lower bound which is assigned in its loop into
//		a preg and hoist it out of the loop.
//
//--------------------------------------------------------------------------

#ifndef access_main_INCLUDED
#define access_main_INCLUDED "access_main.h"

#ifndef wn_INCLUDED
#include "wn.h"
#endif
#ifndef cxx_memory_INCLUDED
#include "cxx_memory.h"
#endif
#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif
#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif
#ifndef stab_INCLUDED
#include "stab.h"
#endif
#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif

extern void LNO_Build_Access(WN *func_nd, MEM_POOL *pool, 
				BOOL Hoist_Bounds=FALSE);
extern void LNO_Build_Access(WN *wn, DOLOOP_STACK *stack, MEM_POOL *pool,
				INDX_RANGE_STACK *irs=0, 
				BOOL Hoist_Bounds=FALSE);
extern void LNO_Print_One_Access(FILE *fp, WN* wn);
extern void LNO_Print_Access(FILE *fp,WN *func_nd);
extern void LNO_Build_Do_Access(WN *wn, DOLOOP_STACK *stack, 
				BOOL Hoist_Bounds=FALSE);
extern void LNO_Build_If_Access(WN *wn, DOLOOP_STACK *stack);
extern void LNO_Build_Access_Array(WN *wn,DOLOOP_STACK *stack,MEM_POOL *pool,
				INDX_RANGE_STACK *irs=0);

extern BOOL Bound_Is_Too_Messy(ACCESS_ARRAY* aa);
extern BOOL Hoist_Lower_Bound(WN* wn_loop, DOLOOP_STACK* stack, 
			      MEM_POOL* pool);
extern BOOL Hoist_Upper_Bound(WN* wn_loop, DOLOOP_STACK* stack, 
			      MEM_POOL* pool);

extern void Hoist_Bounds_One_Level(WN* wn_tree);

extern void Hoist_Iload_Ldid_Upper_Bound_One_Level(
	WN* loop,
	BOOL negative_stride);

extern void Hoist_Varying_Lower_Bounds(WN* func_nd); 
extern BOOL Exp_Node_Varies_In_Loop(WN* wn_node, WN* wn_loop);

#endif
