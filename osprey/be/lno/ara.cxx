/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


// This may look like C code, but it is really -*- C++ -*-

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>
#include "pu_info.h"
#include "ara.h"
#include "opt_du.h"
#include "lnoutils.h"
#include "lwn_util.h"
#include "config.h" 
#include "config_lno.h"
#include "glob.h"
#include "fiz_fuse.h" 
#include "lego_util.h"
#include "parids.h"
#include "cond.h"
#include "access_main.h"

MEM_POOL ARA_memory_pool;
static BOOL ara_mem_pool_initialized=FALSE;


//*************************************************************************
//
// For each symbol used in wn's subscripts, mark the loops
// where the symbol is invariant.
//
//*************************************************************************
void
Set_Invariant_Symbols(ARA_LOOP_INFO *loop_info, WN* wn)
{
  Is_True(loop_info, ("No loop is given in Set_Invariant_Symbols\n"));

  if (WN_operator(wn) == OPR_ARRAY) {
    for (INT32 i = 0; i < WN_num_dim(wn); ++i) {
      Set_Invariant_Symbols(loop_info, WN_array_index(wn,i));
    }
    return;
  }

  if (WN_operator(wn)==OPR_LDID) {
    DEF_LIST *defs = Du_Mgr->Ud_Get_Def(wn);
    if (!defs) 
      return;

    if (defs->Incomplete()){
    // the "incomplete" means that there are some aliased def,
    // but the aliased defs possibly is outside of the loop. 
    // So we need to examine if it is aliased with some def inside of the loop
      if( WN_opcode(loop_info->Loop()) != OPC_DO_LOOP)
      // possibly be FUNC_ENTRY
        return;
      else if (Exp_Node_Varies_In_Loop(wn, (WN *) loop_info->Loop()))
        return;
    }
    if (!loop_info->Processed(wn)) {
      loop_info->Add_Processed(wn);

      // Find the deepest common loops
      INT depth = -1;
      WN* deepest_loop = NULL;
      DEF_LIST_ITER iter(defs);
      WN* cur_loop = (WN *) loop_info->Loop();
      for (DU_NODE* node=iter.First(); !iter.Is_Empty(); node=iter.Next()){
	WN* def = node->Wn();
	WN* common_loop = LNO_Common_Loop(def, cur_loop);
	INT depth_new = Do_Depth(common_loop);

	if (Do_Depth(common_loop)>depth) {
	  depth = depth_new;
	  deepest_loop = common_loop;
	}
      }
    
      // Consider it is invariant to every loop below the deepest_loop
      ARA_LOOP_INFO* cur_info = loop_info;
      while (cur_info && cur_info->Loop()!=deepest_loop) {
	cur_info->Add_Invariant(wn);
	cur_info = cur_info->Parent();
      }
    }
    
  } else 
    for (INT kidno=0; kidno<WN_kid_count(wn); ++kidno) 
      Set_Invariant_Symbols(loop_info,WN_kid(wn,kidno));

}

// Walk the wn tree and process each loop to set the ARA_LOOP_INFO.
// This is a simple depth-first order traversal.  
extern void ARA_Initialize_Loops(WN* wn, 
		                 ARA_LOOP_INFO *parent_info)
{


  if (WN_operator(wn) == OPR_ILOAD) {
    if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
//      ARA_REF * new_ref = CXX_NEW(ARA_REF(wn, parent_info), &ARA_memory_pool);
      Set_Invariant_Symbols(parent_info,WN_kid0(wn));
//      parent_info->Add_Use(new_ref);
    }

    return;

  } else if (WN_operator(wn) == OPR_ISTORE) {
    if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
//      ARA_REF * new_ref = CXX_NEW(ARA_REF(wn, parent_info),&ARA_memory_pool);
      Set_Invariant_Symbols(parent_info,WN_kid1(wn));
//      parent_info->Add_Def(new_ref);
    }

    return;

  }

  if (WN_opcode(wn) == OPC_DO_LOOP){
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
    ARA_LOOP_INFO *cur_loop_info;
    if (parent_info->Loop() != wn){
      cur_loop_info = CXX_NEW(ARA_LOOP_INFO(wn,
			    parent_info,
			    parent_info->Invariant_Bounds()), 
	      &ARA_memory_pool);
      parent_info->Add_Child(cur_loop_info);
    }else{
      cur_loop_info = parent_info;
    }
    dli->ARA_Info = cur_loop_info;

    // Take care of the loop control statement first
    for (INT kidno=1; kidno<=3; ++kidno){
      Set_Invariant_Symbols(cur_loop_info, WN_kid(wn,kidno));
    }

    // Analyze the loop body
    ARA_Initialize_Loops(WN_do_body(wn),cur_loop_info);

    return;

  }
  
  if (WN_opcode(wn)==OPC_BLOCK){
    for (WN* kid = WN_first(wn); kid != NULL; kid = WN_next(kid))
      ARA_Initialize_Loops(kid,parent_info);
    return;
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); ++kidno) {
    WN* kid = WN_kid(wn,kidno);
    ARA_Initialize_Loops(kid,parent_info);
  }

}

//**************************************************************************
// Process the children of root_info (inner loops) and then the root_info
// itself
//**************************************************************************
static void ARA_Print_Loops(ARA_LOOP_INFO *root_info)
{
  ARA_LOOP_INFO_ST & inner_loops = root_info->Children();
  WN* func_nd = (WN*) root_info->Loop(); 

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_VERBOSE) || 
      Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG))
  {  
    for (INT i = 0; i < inner_loops.Elements(); ++i) {
      inner_loops.Bottom_nth(i)->Print_Loop_Property();
    }
  
    for (INT i = 0; i < inner_loops.Elements(); ++i) {
      inner_loops.Bottom_nth(i)->Print_Analysis_Info();
    }
  }
}  

// Build a tree of ARA_LOOP_INFO.  The function entry is the root
// of the tree.  Inner loops are subroots of an outer loop. The tree
// is doubly linked.
void Perform_ARA_and_Parallelization(PU_Info* current_pu, 
				     WN* func_nd)
{

  ARA_LOOP_INFO *root = 
    CXX_NEW(ARA_LOOP_INFO(func_nd, NULL, TRUE), &ARA_memory_pool);

  ARA_Initialize_Loops(func_nd, root);

  // Perform array region analysis
  ARA_Walk_Loops(root);

  // Perform liveness analysis
  root->Create_Live_Use();
  
  // Determine last value of private arrays
  root->Determine_Last_Value();

  // Perform outer loop parallelization
  Walk_Loop_Dependence(func_nd);

  // Determine if peeling helps convexity 
  root->Determine_Peel(); 

  // Print their ARA info
  ARA_Print_Loops(root);

  for (INT i = 0; i < root->Children().Elements(); ++i) 
    root->Children().Bottom_nth(i)->Generate_Parallel_Pragma();

  if (Eliminate_Dead_SCF(func_nd, LWN_Delete_Tree))
    Mark_Code(func_nd, FALSE, FALSE);

  // Annotate loops with information about what the lowerer will name them.   
  Annotate_For_Mp_Lowering(current_pu, func_nd); 

  if (LNO_Prompl)
    Print_Prompl_Msgs(current_pu, func_nd); 

  ARA_Cleanup(func_nd); 
}

//**************************************************************************
// Process the children of root_info (inner loops) and then the root_info
// itself
//**************************************************************************
void ARA_Walk_Loops(ARA_LOOP_INFO *root_info)
{


  root_info->Default_For_Bad_Loop();

}  

static void ARA_Cleanup_Traverse(WN* wn_tree) 
{
   if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
     DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree); 
     CXX_DELETE(dli->ARA_Info, &ARA_memory_pool); 
     dli->ARA_Info = NULL; 
   } 

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      ARA_Cleanup_Traverse(wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      ARA_Cleanup_Traverse(WN_kid(wn_tree, i));
  }
}

// Cleanup connections of ARA to program tree
extern void ARA_Cleanup(WN* func_nd) 
{
   ARA_Cleanup_Traverse(func_nd);     
}


