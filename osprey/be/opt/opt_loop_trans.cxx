//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_loop_trans.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_loop_trans.cxx,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Transformation 1:  loop butterfly
// 
//   CFG before transformation:
//   edges: (1,2) (2,3) (2,9) (3,4) (4,6) (4,5) (5,7) (6,7) (7,4)
//          (7,8) (8,9)
//
//   zone representing a loop butterfly transformation:
//   entry edges: (3,4)
//   clone edges: (4,6) (6,7) (7,4)
//   exit  edges: (4,5) (7,8)
//   
//   CFG after transformation:
//   edges: (1,2) (2,3) (2,9) (3,13) (4,6) (4,5) (5,7) (6,7) (7,4) 
//          (7,8) (8,9) (10,11) (10,5) (11,12) (12,10) (12,8) (13,10)
//  
//   Translation of old to new blocks:
//   translation: 10<-4 11<-6 12<-7
//  
//   introduced vertex 13 
//     -- the new preheader for inner-loop, and as the 
//     -- the header of the outer loop.
//
// NOTE: there are lots of limitation in loop butterfly because of
// the difficulty in estimate performance.   Trip-count-able loops
// are excluded from loop butterflying because of the potential
// performance degradations due to butterflying.
//
//
// Transformation 2:  loop unrolling (not implemented)
//   We will probably only implement full unrolling here.
//
// Transformation 3:  loop unswitching (not implemented)
//   Hoisting of invariant if/switch out of loop.
//
// ====================================================================
// ====================================================================


#define USE_STANDARD_TYPE
#include "opt_defs.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "config_wopt.h"
#include "opt_cfg.h"
#include "opt_fb.h"
#include "opt_cfg_trans.h"
#include "opt_transform.h"
#include "opt_main.h"

#include <iterator>
#include <stack>
#include <queue>

using std::insert_iterator;
using std::priority_queue;
using std::set;
using std::stack;

static bool check_hazardous_op(BB_NODE *bb)
{
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (OPCODE_is_call(stmt->Op()))
      return true;
    if (stmt->Op() == OPC_COMPGOTO)
      return true;
  }
  return false;
}

inline bool member_of(int elem, set<int>& set)
{
  return (set.find(elem) != set.end());
}

// Return percentage path coverage
static double path_exit_counts(successor_graph& g, set<vertex_id>& ends,
			       OPT_FEEDBACK *feedback, bool trace)
{
  FB_FREQ coverage = FB_FREQ_ZERO;
  for (set<vertex_id>::iterator si = ends.begin();
       si != ends.end();
       ++si) {
    vertex_id bb = *si;
    for (predecessor_graph::fast_iterator e = g[bb].begin(); 
	 e != g[bb].end(); 
	 ++e) {
      vertex_id to = (*e).second;
      if (! member_of(to, ends)) {
	if ( trace ) {
	  fprintf( TFile, "path_exit_counts: %d->%d, ", bb, to );
	  feedback->Get_edge_freq( bb, to ).Print( TFile );
	  fprintf( TFile, "\n" );
	}
	coverage += feedback->Get_edge_freq(bb, to);
      }
    }
  }
  if ( coverage.Known() )
    return coverage.Value();
  else
    return 0.0;  // WHAT IS APPROPRIATE HERE?
}


// Butterfly a loop using feedback information.  The butterfly loop must 
// have 'min_coverage' % of the original loop.  The algorithm is by
// repeatedly finding the best path connecting the loop tail and loop
// headers and included it into the butterfly loop.
//
static double
butterfly_loop_with_profile(successor_graph& g,
			    set<vertex_id> orig_loop,
			    predecessor_graph& loop, 
			    set<vertex_id>& starts, 
			    set<vertex_id>& ends,
			    OPT_FEEDBACK *feedback,
			    int min_coverage,
			    bool trace) 
{
  priority_queue<path_type*> primary_heap, secondary_heap;
  double orig_counts = path_exit_counts(g, orig_loop, feedback, trace);
  double total_counts =
    path_exit_counts(g, ends, feedback, trace) - orig_counts;
  if (!(total_counts > 1.0)) {
    if (trace)
      fprintf(TFile, "disable loop butterfly because of small counts.\n");
    return 0.0;
  }
  if (trace) 
    fprintf(TFile,
	    "<<<begin loop butterfly with initial counts %g\n", total_counts);
  double coverage = 0.0;
  double exit_counts = 0.0;
  for (set<int>::iterator si = starts.begin();
       si != starts.end();
       ++si) {
    path_type *p =
      CXX_NEW( path_type( *si, feedback->Get_node_freq_out(*si).Value() ),
	       &MEM_local_pool );
    primary_heap.push(p);
  }
  set<int> processed;
  while (!primary_heap.empty()) {
    path_type *p = primary_heap.top();
    primary_heap.pop();
    vertex_id bb = (*p).last_bb();
    if (member_of(bb, ends)) {
      for (int i = 0; i < (*p).bbs.size(); ++i)
	ends.insert((*p).bbs[i]);
      if (trace) {
	fprintf(TFile, "adding path with freq %g: ", (*p).wt);
	for (int i = 0; i < (*p).bbs.size(); ++i)
	  fprintf(TFile, "%d ",(*p).bbs[i]);
 	fprintf(TFile, "\n");
      }
    } else {
      if (member_of(bb, processed)) 
	secondary_heap.push(p);
      else {
	processed.insert(bb);
	for (predecessor_graph::fast_iterator e = loop[bb].begin(); 
	     e != loop[bb].end(); 
	     ++e) {
	  vertex_id pred = (*e).first;
	  path_type *p2 = CXX_NEW(path_type(*p), &MEM_local_pool);
	  (*p2).add_bb(pred);
	  (*p2).wt *= feedback->Get_pred_prob(pred, bb);
	  primary_heap.push(p2);
	}
      }
    }
    if (!secondary_heap.empty()) {
      path_type *p = secondary_heap.top();
      if (member_of((*p).last_bb(), ends)) {
	secondary_heap.pop();
	primary_heap.push(p);
      }
    }
    // enough paths selected?
    exit_counts = path_exit_counts(g, ends, feedback, false) - orig_counts;
    coverage = 1.0 - exit_counts/total_counts;
    if (coverage * 100 > min_coverage)
      goto butterfly_exit;
  }
  // heap is empty
  {
    exit_counts = path_exit_counts(g, ends, feedback, false) - orig_counts;
    coverage = 1.0 - exit_counts/total_counts;
  }

butterfly_exit:
  if (trace) {
    // call path_exit_counts again just for its trace
    path_exit_counts(g, ends, feedback, true);
    fprintf(TFile, ">>> end loop butterfly with coverage %g, exit_count %g\n",
	    coverage, exit_counts);
  }
  return coverage;
}


// convert a set of bb into a zone representation
void add_loop_to_zone(CFG *cfg, BB_NODE *header, 
		      set<vertex_id>& new_loop, zone_container& zones) 
{
  zones.push_back(zone(zones.size()));
  zone_container::iterator z = zones.end()-1;
  (*z).loop_butterfly = header->Id();
  (*z).profit = 10;
  
  BB_NODE *succ, *pred;
  BB_LIST_ITER bb_succ_iter;
  BB_LIST_ITER bb_pred_iter;
  FOR_ALL_ELEM(pred, bb_pred_iter, Init(header->Pred())) {
    int pred_id = pred->Id();
    (*z).entry.push_back(edge(pred_id, header->Id()));
  }
  for (set<vertex_id>::iterator p = new_loop.begin();
       p != new_loop.end(); ++p) {
    int bb_id = *p;
    BB_NODE *bb = cfg->Get_bb(bb_id);
    FOR_ALL_ELEM(succ, bb_succ_iter, Init(bb->Succ())) {
      int succ_id = succ->Id();
      if (new_loop.find(succ_id) != new_loop.end()) 
	(*z).clone.push_back(edge(bb_id, succ_id));
      else
	(*z).exit.push_back(edge(bb_id, succ_id));
    }
  }
  // use set_differene to exclude the back-edge from loop butterfly entry
  zone::edge_container tmp;
  insert_iterator<zone::edge_container> ins_tmp(tmp, tmp.begin());
  (*z).canonicalize(); // prerequisite to run set_difference
  set_difference((*z).entry.begin(), (*z).entry.end(),
		 (*z).clone.begin(), (*z).clone.end(), ins_tmp);
  (*z).entry.erase((*z).entry.begin(),(*z).entry.end());
  (*z).entry.insert((*z).entry.begin(),tmp.begin(),tmp.end());
}


static bool loop_should_be_butterflied(BB_LOOP *loop)
{
  // butterfly only innermost loop for now!
  //  TODO -- with feedback information, we should be able
  //    to improve performance by butterflying outer loop as well.
  //  -Raymond 9/21/98
  if (loop->Depth() != loop->Max_depth())  // innermost loop
    return false;

  // Do not butterfly loops with known trip count because 
  // butterflying disables do-loop unrolling and swp.
  // 
  if (loop->Trip_count_stmt() != NULL ||
      loop->Trip_count_expr() != NULL)
    return false;

  // circuit breaker: skip butterfly if the loop is too big
  int bb_count_limit = 2 * WOPT_Enable_CFG_Opt_Limit;
  if (loop->True_body_set()->Size() > bb_count_limit) 
    return false;

  return true;
}

// Generate loop butterfly zone.
//  If no feedback information is available, form butterfly loop
//  by excluding CALLs.  If feedback is available, use feedback
//  to select the "best" paths to butterfly.
//
void generate_loop_butterfly_zones(COMP_UNIT *cu,
				   successor_graph &g, 
				   vector<zone>& zones,
				   int min_coverage,
				   bool trace) 
{
  CFG *cfg = cu->Cfg();
  OPT_FEEDBACK *feedback = NULL; // cfg->Feedback();

  for (BB_NODE *header = cfg->First_bb();
       header != NULL; header = header->Next()) {
    BB_LOOP *loop = header->Loop();
    if (loop != NULL && 
	loop->Header() == header && 
	loop->Well_formed() &&
	loop_should_be_butterflied(loop)) {
      BB_NODE *bb;
      BB_NODE_SET_ITER bb_iter;
      set<vertex_id> new_loop;
      set<vertex_id> orig_loop;

      // skip hazardous basic blocks
      int inserted_count = 0;
      int deleted_count = 0;
      FOR_ALL_ELEM(bb, bb_iter, Init(loop->True_body_set())) {
	if (!check_hazardous_op(bb)) {
	  new_loop.insert(bb->Id());
	  inserted_count++;
	} else
	  deleted_count++;
	orig_loop.insert(bb->Id());
      }
      bool need_cfg_trans = inserted_count > 0 && deleted_count > 0;
      if (feedback && inserted_count >= 2) {
	predecessor_graph loop_pred_graph;
	FOR_ALL_ELEM(bb, bb_iter, Init(loop->True_body_set())) {
	  BB_NODE *pred;
	  BB_LIST_ITER bb_pred_iter;
	  if (member_of(bb->Id(), new_loop)) {
	    FOR_ALL_ELEM(pred, bb_pred_iter, Init(bb->Pred())) {
	      if (member_of(pred->Id(), new_loop))
		add_edge(loop_pred_graph, edge(pred->Id(), bb->Id()));
	    }
	  }
	}
	set<int> starts, ends;
	starts.insert(loop->Loopback()->Id());
	ends.insert(loop->Header()->Id());
	loop_pred_graph.extend(loop->Loopback()->Id());
	loop_pred_graph.extend(loop->Header()->Id());
	OPT_POOL_Push( &MEM_local_pool, -1);
	double coverage = 
	  butterfly_loop_with_profile(g, orig_loop, loop_pred_graph, starts,
				      ends, feedback, min_coverage, trace);
	OPT_POOL_Pop(&MEM_local_pool, -1);
	if (trace)
#ifdef KEY /* Mac port */
	  fprintf(TFile, "new_loop size=%ld, butterfly size=%ld\n",
		  (long) new_loop.size(), (long) ends.size());
#else /* KEY Mac port */
	  fprintf(TFile, "new_loop size=%d, butterfly size=%d\n",
		  (INT)new_loop.size(), (INT)ends.size());
#endif /* KEY Mac port */
	if (coverage * 100 >= min_coverage && new_loop.size() > ends.size()) {
	  if (trace) {
	    print_vertex_set(&new_loop, TFile);
	    print_vertex_set(&ends, TFile);
	  }
	  new_loop = ends;
	  need_cfg_trans = true;
	}
      }
      if (need_cfg_trans) 
	add_loop_to_zone(cfg, loop->Header(), new_loop, zones);
    }
  }
}



