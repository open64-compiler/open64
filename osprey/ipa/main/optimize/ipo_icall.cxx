/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement
 * or the like.  Any license provided herein, whether implied or
 * otherwise, applies only to this software file.  Patent licenses, if
 * any, provided herein do not apply to combinations of this program with
 * other software, or any other product whatsoever.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
// ====================================================================
// ====================================================================
//
// Module: ipo_icall.cxx
//
// Revision history:
//  02-Jan-2006 - Original Version
//
// Description:  Interprocedural Optimization of Indirect Calls
//
// ====================================================================
// ====================================================================

#include <stdint.h>

#include "ipo_defs.h"
#include "ipo_inline.h"
#include "ipo_alias.h"
#include "ipo_alias_class.h"
#include "ipo_parent.h"
#include "ipl_summarize.h"
#include "ipa_option.h"
#include "ipa_trace.h"
#include "fb_whirl.h"

#include <deque>
#include <queue>

struct order_by_callsite_id
{
    bool operator() (IPA_EDGE * first, IPA_EDGE * second)
    {
    	return first->Callsite_Id() > second->Callsite_Id();
    }
};

static BOOL
Is_Return_Store_Stmt (WN * wn)
{
  if (wn && WN_operator (wn) == OPR_STID)
  {
    WN *val = WN_kid (wn, 0);
    if (WN_operator (val) == OPR_LDID)
    {
      ST *st = WN_st (val);
      if (ST_sym_class (st) == CLASS_PREG
          && (st == Return_Val_Preg))
        return TRUE;
    }
  }
  return FALSE;
}

static BOOL
is_dummy_call_site( SUMMARY_CALLSITE *callsite )
{
   if  (callsite && (callsite->Is_dummy_callsite())) {
      return TRUE;
   }
   else return FALSE;
}

 void Copy_Return_STID(WN *block, WN *wn)
{
  OPERATOR opr =  WN_operator(wn);
  Is_True(opr == OPR_CALL || opr == OPR_ICALL,
          ("invalid call node"));
  // Put return stid statements in the end of the block
  WN* stmt = WN_next(wn);
  if (Is_Return_Store_Stmt (stmt))
    WN_INSERT_BlockLast (block, WN_COPY_Tree(stmt));

  return;
}

// ======================================================================
// wn: icall
// edge: edge added for call generated in icall transformation.
// Do the actual icall transformation (i.e. generation of if-else stmt)
// Set WN node in edge.
//
//    if (*fptr == foo) {
//       foo(...);
//    }
//    else {
//       if (*fptr == bar) {
//          bar(...);
//       }
//       else {
//          fptr(...);
//       }
//    }
//
// ======================================================================
static WN *
Convert_Icall (WN * wn,  vector<IPA_EDGE *> & dummy_edge_list, int start)
{
  int num = dummy_edge_list.size();
  Is_True(num > 0, ("invalid dummy callsite number"));
  WN * wn_first_if = NULL;
  // create if-then part 
  IPA_EDGE * edge = dummy_edge_list[start];
  IPA_NODE * caller = IPA_Call_Graph->Caller (edge);
  IPA_NODE * callee = IPA_Call_Graph->Callee (edge);
  ST * st_callee = callee->Func_ST();
  TY_IDX ty_callee = ST_pu_type (st_callee);

  // Address of a possible callee.
  WN* tmpkid0 = WN_CreateLda (Use_32_Bit_Pointers ? OPC_U4LDA : OPC_U8LDA,
                                  0, Make_Pointer_Type(ty_callee), st_callee);
  // Pointer from indirect call
  WN* tmpkid1 = WN_COPY_Tree_With_Map(WN_kid (wn, WN_kid_count(wn) - 1));

  // If the pointer is pointing to the callee, ...
  WN* test = WN_Create (Use_32_Bit_Pointers ? OPC_U4U4EQ : OPC_U8U8EQ, 2);

  WN_kid0(test) = tmpkid0;
  WN_kid1(test) = tmpkid1;

  // Then, call "that" callee.
  WN* if_then = WN_Create (WN_opcode(wn), WN_kid_count(wn) - 1);
  WN* if_then_block = WN_CreateBlock();
  WN_set_operator (if_then, OPR_CALL);

  // Set wn for the new edge.
  edge->Set_Whirl_Node (if_then);

  // Arguments for direct call
  for (int i = 0; i < WN_kid_count (if_then); i++)
    WN_kid (if_then, i) = WN_COPY_Tree_With_Map (WN_kid (wn, i));

  WN_st_idx(if_then) = ST_st_idx(st_callee);

  WN_INSERT_BlockLast (if_then_block, if_then);
  // copy return stid if there is any
  Copy_Return_STID(if_then_block, wn);

  // Else part,
  WN* if_else;
  WN* if_else_block = WN_CreateBlock();
  if (start == num -1) { 
    // last else, do the indirect call.
    if_else = WN_COPY_Tree_With_Map (wn);
    WN_INSERT_BlockLast (if_else_block, if_else);
    // copy return stid if there is any
    Copy_Return_STID(if_else_block, wn);
  }
  else {
    // handle next indirect call candidate
    if_else = Convert_Icall (wn,  dummy_edge_list, start+1);
    WN_INSERT_BlockLast (if_else_block, if_else);
  }

  WN * block = WN_Get_Parent (wn, caller->Parent_Map(), caller->Map_Table());

  WN_Parentize (if_then_block, caller->Parent_Map(), caller->Map_Table());
  WN_Parentize (if_else_block, caller->Parent_Map(), caller->Map_Table());

  WN* wn_if = WN_CreateIf (test, if_then_block, if_else_block);

  // ************* TEMPORARY WORKAROUND ************************
  // Why does not IPA have feedback at -O0/-O1?
  // We reach here actually means the compilation has feedback.
  if (Cur_PU_Feedback)
  {
    Cur_PU_Feedback->FB_lower_icall (wn, start == num -1 ? if_else
                                                         : NULL, if_then, wn_if, start);
  }

  // clean up and fix up the old icall node
  if ( start == 0 ) {
    if (Cur_PU_Feedback) {
      // Delete the map info if all other icall targets are promoted. 
      // We delete it from <Cur_PU_Feedback>
      Cur_PU_Feedback->Delete(wn);
    }
    
    // remove return stid statements from the original block
    WN* stmt = WN_next(wn);
    if (Is_Return_Store_Stmt (stmt))
      WN_EXTRACT_FromBlock (block, stmt);
    
    // Replace wn with call_wn.
    WN_INSERT_BlockAfter (block, wn, wn_if);
    WN_EXTRACT_FromBlock (block, wn);
    WN_Parentize (block, caller->Parent_Map(), caller->Map_Table());
  }
  return wn_if;
} // Convert_Icall

// return true if the edge has the dummy callsite
// corresponding to the ICALL node w
static bool Is_dummy_edge_for_node(IPA_EDGE *edge, WN *n)
{
   SUMMARY_CALLSITE* callsite = edge->Summary_Callsite();
   return WN_operator(n) == OPR_ICALL &&
          callsite->Is_dummy_callsite() &&
          callsite->Get_matching_map_id() == WN_map_id(n);
}

// ======================================================================
// Top-level function for icall-transformation.
// Called only for functions with icall-opt opportunity. Map_Callsites()
// must be called for other functions.
//
// Maps outgoing edges to call nodes. Adds call nodes when necessary,
// but still mainitaining the edge->call mapping.
// ======================================================================
void
IPO_Process_Icalls (IPA_NODE * node)
{
  bool do_trace =  Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL) ;
  FmtAssert (!node->Is_Visited(),
             ("Node is getting visited NOT for the first time"));

  node->Set_Visited();

  if (node->Total_Succ() == 0) return;

  // deque: constant time removal of elements from the front.
  std::deque<WN*> callsite_map;
  // Maintain elements in decreasing order of callsite id.
  std::priority_queue<IPA_EDGE *,
                      vector<IPA_EDGE *>,
                      order_by_callsite_id> edge_order;

  // Get the existing callsites ACTUALLY present in code.
  for (WN_ITER* wni = WN_WALK_TreeIter(node->Whirl_Tree(FALSE));
       wni != NULL;
       wni = WN_WALK_TreeNext(wni))
  {
    WN* wn = WN_ITER_wn (wni);

    switch (WN_operator(wn))
    {
      case OPR_CALL:
          if (WN_opcode(wn) == OPC_VCALL &&
              WN_Fake_Call_EH_Region (wn, Parent_Map))
          break;
          // fall through
      case OPR_ICALL:
      case OPR_INTRINSIC_CALL:
          callsite_map.push_back (wn);
          break;
    }
  }

  IPA_SUCC_ITER succ_iter (node);

  // Edges in decreasing order of callsite-id
  for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next())
  {

    IPA_EDGE *edge = succ_iter.Current_Edge();
    if (edge)
      edge_order.push (edge);
  }

  if (do_trace) {
    bool header_printed = false;
    std::priority_queue<IPA_EDGE *,
                      vector<IPA_EDGE *>,
                      order_by_callsite_id> edges = edge_order;
    while (!edges.empty()) {
       IPA_EDGE * edge = edges.top();
       IPA_NODE * caller = IPA_Call_Graph->Caller (edge);
       IPA_NODE * callee = IPA_Call_Graph->Callee (edge);

       edges.pop();
       if (!header_printed) {
          fprintf( TFile, "\n\nOrdered edges for Caller %s\n", caller->Name());
          header_printed = true;
       }
       fprintf( TFile, "\n[callsite %d, callee %s] ", edge->Callsite_Id(),
                callee->Name());
       edge->Print(TFile, IPA_Call_Graph);
       if (edge->Whirl_Node()) {
          fdump_tree(TFile, edge->Whirl_Node());
       }
    }

    fprintf( TFile, "\n\ncallsite_map \n");
    for (int i=0; i < callsite_map.size(); i++) {
       fprintf( TFile, "\n[callsite %d] \n", i);
       fdump_tree(TFile, callsite_map[i]);
    }
  }
  // The following counter helps in maintaining the mapping between
  // CALL/ICALL WN nodes and call graph edges.
  //
  // Detects cases where 
  // 1. there are WN nodes but no corresponding edges, like
  //      intrinsic calls, icalls not being converted.
  //        Ignore such WN nodes.
  // 2. there are edges but no WN nodes, like
  //      edges added for icall-opt. 
  //        In such a scenario, either add WN or delete edge from call graph.
  UINT32 callsite_idx = 0;
  while (!edge_order.empty())
  {
    // Edge with the lowest callsite id.
    IPA_EDGE * edge = edge_order.top();
    edge_order.pop();

    IPA_NODE * caller = IPA_Call_Graph->Caller (edge);
    IPA_NODE * callee = IPA_Call_Graph->Callee (edge);
    ST * st_callee = callee->Func_ST();
    TY_IDX ty_callee = ST_pu_type (st_callee);


    // Remove WN nodes for which we don't have an edge:
    // INTRINSIC_CALL and ICALL have no edge in the call graph,
    // but the ICALL may have dummy edges (corresponding to 
    // the dummy callsites of the ICALL node)
    // some CALL node may also have no edge (the callee may
    // be in library)
    WN * w = callsite_map.front();
    while (1) {
      if (Is_dummy_edge_for_node(edge, w) ||
           (WN_operator (w) == OPR_CALL && ST_st_idx(st_callee) == ST_st_idx(WN_st(w)))) {
        // some ICALL may have dummy call edges in call graph
        // don't skip them
        break; 
      } 
      if (do_trace) {
        fprintf( TFile, "\n[pop callsite %d from callsite_map]\n", callsite_idx);
        if (edge->Whirl_Node()) {
          fprintf( TFile, "Original edge->Whirl_Node:\n");
          fdump_tree(TFile, edge->Whirl_Node());
        }
        fprintf( TFile, "whirl node from callsite_map:\n");
        fdump_tree(TFile, callsite_map.front());
      }
      callsite_map.pop_front();
      callsite_idx++;
      FmtAssert( !callsite_map.empty(),
                 ("Error in sync edge and whirl node"));
      w = callsite_map.front();
    }

    // sync edge->Whirl_Node() with call node in callsite_map, which could 
    // could happen if the call is transformed from de-virtualization
    if (edge->Whirl_Node() && edge->Whirl_Node() != callsite_map.front()) {
      while (callsite_map.front() != edge->Whirl_Node() && !callsite_map.empty() ) {
        if (do_trace) {
          fprintf( TFile, "\n[pop callsite %d from callsite_map]\n", callsite_idx);
          if (edge->Whirl_Node()) {
            fprintf( TFile, "Original edge->Whirl_Node:\n");
            fdump_tree(TFile, edge->Whirl_Node());
          }
          fprintf( TFile, "whirl node from callsite_map:\n");
          fdump_tree(TFile, callsite_map.front());
        }
        callsite_map.pop_front();
        callsite_idx++;
      }
      FmtAssert (!callsite_map.empty(), 
                 ("Invalid edge, cannot find matching call node"));
    }

    if (WN_operator (w) == OPR_CALL) {
      if (do_trace) {
        fprintf( TFile, "\n[edge %d, callsite %d, callee %s]\n", edge->Edge_Index(), 
                 callsite_idx, callee->Name());
        if (edge->Whirl_Node()) {
          fprintf( TFile, "Original edge->Whirl_Node:\n");
          fdump_tree(TFile, edge->Whirl_Node());
        }
        fprintf( TFile, "Set to new whirl node:\n");
        fdump_tree(TFile, w);
      }
      FmtAssert ( ST_st_idx(st_callee) == ST_st_idx(WN_st(w)),
                  ("Error in sync edge and whirl node"));
      edge->Set_Whirl_Node (w);
    }
    else // do icall optimization
    {
      FmtAssert (Is_dummy_edge_for_node(edge, w),
                 ("IPO_Process_Icalls: Expected dummy edge for ICALL node"));
      IPA_EDGE * next_edge = (!edge_order.empty()) ?
                        edge_order.top() : NULL;

#ifdef USE_OLD_ICALL_PROMOTION
      if (edge->Summary_Callsite()->Is_func_ptr())
      { 
        // There is no dummy call site added for the ICALL at
        // summary phase, that means we decided not to do icall
        // conversion based on IPA data flow
        edge->Set_Whirl_Node (w);
        if (do_trace) {
          fprintf( TFile, "\n[edge %d, callsite %d, callee %s]\n", edge->Edge_Index(), 
                   callsite_idx, callee->Name());
          if (edge->Whirl_Node()) {
            fprintf( TFile, "Original edge->Whirl_Node:\n");
            fdump_tree(TFile, edge->Whirl_Node());
          }
          fprintf( TFile, "Set to new whirl node:\n");
          fdump_tree(TFile, w);
        }
      }
      else if (next_edge && next_edge->Summary_Callsite()->Is_func_ptr() &&
               (edge->Callsite_Id() + 1 == next_edge->Callsite_Id()))
      {
        if (do_trace) {
          fprintf( TFile, "\n[edge %d, callsite %d, callee %s]\n", edge->Edge_Index(), 
                   callsite_idx, callee->Name());
          if (edge->Whirl_Node()) {
            fprintf( TFile, "deleted edge->Whirl_Node:\n");
            fdump_tree(TFile, edge->Whirl_Node());
          }
          fprintf( TFile, "\n[use edge %d, callsite %d]\n", next_edge->Edge_Index(), callsite_idx+1);
          if (next_edge->Whirl_Node()) {
            fprintf( TFile, "Original edge->Whirl_Node:\n");
            fdump_tree(TFile, next_edge->Whirl_Node());
          }
          fprintf( TFile, "Set to new whirl node:\n");
          fdump_tree(TFile, w);
        }
        // Delete dummy edge because IPA data flow has resolved the
        // function pointer to the actual function, no need to guess
        // any more.
        IPA_Call_Graph->Graph()->Delete_Edge (edge->Edge_Index());
        callsite_idx++; // account for callsite in deleted edge

        // next_edge should be the edge created by IPA data flow
        next_edge->Set_Whirl_Node (w);
        edge_order.pop();
      }
      else
#endif
      {
        // Do the actual transformation, set WN node in edge.
        if (do_trace) {
          fprintf( TFile, "\n[convert icall for edge %d, callsite %d]\n", 
                edge->Edge_Index(), callsite_idx);
          if (edge->Whirl_Node()) {
            fprintf( TFile, "Original edge->Whirl_Node:\n");
            fdump_tree(TFile, edge->Whirl_Node());
          }
          fprintf( TFile, "whirl node before icall convert:\n");
          fdump_tree(TFile, w);
        }
        // find the number of dummy icalls site associated with icall
        // node and convert these dymmy icall sites in one shot
        vector<IPA_EDGE *> dummy_edge_list;
        while (edge && Is_dummy_edge_for_node(edge, w))
        {
          dummy_edge_list.push_back(edge);
          callsite_idx++;  // for the call added
          if (!edge_order.empty()) {
            edge =  edge_order.top();
            if (!Is_dummy_edge_for_node(edge, w))
               break;
            edge_order.pop();
#ifdef USE_OLD_ICALL_PROMOTION
            // the dummy callsites id for one icall node are all
            // continuous
            // there may be a case which two icalls in a row and
            // the callsites are dummy callsite but they have 
            // discontinous
            if (edge->Callsite_Id() > last_edge->Callsite_Id() + 1)
               break;
#endif
          }
          else
             break;
        } 

        FmtAssert (dummy_edge_list.size() <= ICALL_MAX_PROMOTE_PER_CALLSITE,
                  ("IPO_Process_Icalls: Invalid number of dummy callsites"));
        
        if (Cur_PU_Feedback && do_trace) {
          fprintf( TFile, "\nThe feedback info:\n");
          Cur_PU_Feedback->Print(TFile, w);
        }

        WN *promoted_tree = Convert_Icall (w, dummy_edge_list, 0);

        if (do_trace) {
          fprintf( TFile, "\nAfter convert, new edge->Whirl_Node:\n");
          fdump_tree(TFile, promoted_tree);
        }

      }
    }

    callsite_map.pop_front();
    callsite_idx++; // for the popped node
  }
} // IPO_Process_Icalls

void
IPO_Process_Virtual_Functions (IPA_NODE * node)
{
    //
    // Follow an approach similar to IPO_Process_Icalls to 
    // remap the callsites and call graph edges. This remapping
    // is necessary because there may now be a different number of
    // call whirl instructions because of virtual function 
    // transformation. This function and IPO_Process_Icalls 
    // are mutually exclusive.
    //

    if (node->Is_Visited())
       return;

    node->Set_Visited();

    if (node->Total_Succ() == 0) return;

    // deque: constant time removal of elements from the front.
    std::deque<WN*> callsite_map;
    // Maintain elements in decreasing order of callsite id.
    std::priority_queue<IPA_EDGE *,
                      vector<IPA_EDGE *>,
                      order_by_callsite_id> edge_order;

    // Get the existing callsites ACTUALLY present in code.
    for (WN_ITER* wni = WN_WALK_TreeIter(node->Whirl_Tree(FALSE));
       wni != NULL;
       wni = WN_WALK_TreeNext(wni))
    {
        WN* wn = WN_ITER_wn (wni);

        switch (WN_operator(wn))
        {
          case OPR_CALL:
            if (WN_opcode(wn) == OPC_VCALL &&
                WN_Fake_Call_EH_Region (wn, Parent_Map))
            break;
          // fall through
          case OPR_ICALL:
          case OPR_INTRINSIC_CALL:
              callsite_map.push_back (wn);
              break;
        }
    }

    IPA_SUCC_ITER succ_iter (node);
  
    // Edges in decreasing order of callsite-id
    for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next())
    {
      IPA_EDGE *edge = succ_iter.Current_Edge();
      if (edge)
        edge_order.push (edge);
    }
  

    UINT32 callsite_idx = 0;
    while (!edge_order.empty())
    {
      // Edge with the lowest callsite id.
      IPA_EDGE * edge = edge_order.top();
      edge_order.pop();
  
      // Remove WN nodes for which we don't have an edge
      for (; callsite_idx < edge->Callsite_Id(); callsite_idx++)
        callsite_map.pop_front();
  
      FmtAssert (callsite_idx == edge->Callsite_Id(),
                 ("IPO_Process_Virtual_Functions: Invalid callsite index"));
      IPA_EDGE * next_edge = (!edge_order.empty()) ?
                          edge_order.top() : NULL;
      WN * w = callsite_map.front();
      if(WN_operator(w) == OPR_CALL) {
          edge->Set_Whirl_Node (w);
      } else {
          FmtAssert (WN_operator (w) == OPR_ICALL,
                 ("IPO_Process_Virtual_Function: Expected ICALL"));
          if (edge->Summary_Callsite()->Is_func_ptr())
          { // We decided not to do virtual function call conversion, but IPA data flow
            // has the answer.
            edge->Set_Whirl_Node (w);
          } 
      }
        callsite_map.pop_front();
        callsite_idx++; // for the popped node
      
    }
}
