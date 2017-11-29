/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


// ====================================================================
// ====================================================================
//
// Module: ipa_solver.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_solver.cxx,v $
//
// Revision history:
//  19-Sep-95 - Original Version
//
// Description:
//
// This module contains functions required for implementation of the
// template dataflow solver based on class DFBASE in ipa_solver.h.
//
// It also contains an instance of the solver which prints the
// callgraph for tracing purposes.  See the definition of the DF_PRINT
// class below for an example of how to instantiate DFBASE.
//
// TODO:  Connect_graph, Restore_graph, and Reverse_graph, with their
// underlying utility functions, should be moved to the ip_graph.[ch]
// module.
//
// ====================================================================
// ====================================================================

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_solver.cxx,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */

#include "assert.h"
#include "defs.h"
#include "dwarf_DST_mem.h"	/* Needed by ipc_file.h for DST_TYPE */
#include "erglob.h"
#include "mempool.h"
#include "tracing.h"

#include "mtypes.h"	/* Needed by const.h for TYPE_ID */
#include "symtab.h"
#include "const.h"	/* Needed by ipc_file.h for Const_Tab_Type */
#include "strtab.h"

#include "ip_graph.h"
#include "ipc_file.h"
#include "ipa_cg.h"
#include "ipa_solver.h"
#include "ipa_summary.h"
#include "ipa_option.h"

static BOOL Trace_CG = FALSE;

// ====================================================================
//
// DFBASE::DFBASE
//
// Constructor for the dataflow base class.  The graph iterator may be
// NULL, in which case the solver must provide it later.
//
// ====================================================================

DFBASE::DFBASE (
  IPA_CALL_GRAPH &cg,		// The underlying call graph to use
  DF_DIRECTION direction,	// The dataflow problem direction
  MEM_POOL *mpool,		// Where to put new junk
  DFN *iter )			// An iterator over the graph
{
  // Record the information provided:
  _m = mpool;
  _cg = &cg;
  _iter = iter;
  _entry = INVALID_NODE_INDEX;
  _exit  = INVALID_NODE_INDEX;
  _dir = direction;
  _saved_root = INVALID_NODE_INDEX;
  _reversed = FALSE;
  _changed = FALSE;

  // Make sure it's valid:
  assert ( _cg->Graph() != NULL );
  assert ( (_dir == BACKWARD) || (_dir == FORWARD) );

  // Connect the graph to unique entry and exit nodes:
  Connect_graph ( mpool );
}


// ====================================================================
//
// DFBASE::~DFBASE
//
// Destructor for the dataflow base class.  We must restore the
// underlying graph and free the iterator memory.
//
// ====================================================================

DFBASE::~DFBASE ()
{
  if ( Get_saved_root() != INVALID_NODE_INDEX ) {
    Restore_graph();
  }
  if ( Get_iter() != NULL ) {
    Free_DFN ( Get_iter(), Get_mempool() );
  }
}

// ====================================================================
//
// Forward_Visit / Backward_Visit
//
// Do a forward (backward) depth-first visit of the given graph from
// the given vertex, marking visited nodes in the given vector.  These
// routines should not be called from an already-visited node.
//
// See the Dragon book, page 662.
//
// ====================================================================

static void
Forward_Visit ( GRAPH *g, NODE_INDEX v, mBOOL *visit )
{
  // This node has been visited now:
  visit[v] = TRUE;

  // Create a vertex iterator:
  NODE_ITER v_iter ( g, v );

  for ( NODE_INDEX vtx = v_iter.First_Succ();
	vtx != INVALID_NODE_INDEX ;
	vtx = v_iter.Next_Succ() )
  {
    if ( ! visit[vtx] ) {
      // We haven't seen this one yet -- walk its subgraph:
      Forward_Visit ( g, vtx, visit ); 
    }
  }
}

// ====================================================================

static void
Backward_Visit ( GRAPH *g, NODE_INDEX v, mBOOL *visit )
{
  // This node has been visited now:
  visit[v] = TRUE;

  // Create a vertex iterator:
  NODE_ITER v_iter(g, v);

  for ( NODE_INDEX vtx = v_iter.First_Pred();
	vtx != INVALID_NODE_INDEX ;
	vtx = v_iter.Next_Pred() )
  {
    if ( ! visit[vtx] ) {
      // We haven't seen this one yet -- walk its subgraph:
      Backward_Visit ( g, vtx, visit ); 
    }
  }
}

// ====================================================================
//
// DFBASE::Connect_graph
//
// Ensure that the entire graph is connected, with a unique entry and a
// unique exit node.  This requires a 3-step algorithm, after creating
// the entry and exit nodes:
//
//  1)	Go find the nodes without predecessors/successors, and connect
//	them to entry/exit.  This is a simple enumeration of the nodes
//	where order is unimportant.
//
//  2)	Do a depth-first successor walk from the entry marking
//	reachable nodes.
//
//  3)	Search the nodes again for unreachable ones.  When one is
//	found, connect it to the entry and walk its successors as in
//	step (2) before continuing.
//
// Do the predecessor dual of (2/3) to connect nodes unreachable from
// the exit.
//
// This algorithm is not optimal in the sense that because step (3)
// connects unreachable nodes to the entry (exit) in the order it sees
// them in its final walk, it may connect more than necessary.
//
// ====================================================================

void
DFBASE::Connect_graph ( MEM_POOL *pool )
{
  GRAPH *g = this->Get_callgraph()->Graph();
  NODE_INDEX entry, exit, v;
  NODE_INDEX vertex_max;
  mBOOL *reach;

  // If it's already done, don't bother:
  if ( this->Get_entry() != INVALID_NODE_INDEX ) return;

  Trace_CG = Get_Trace ( TP_IPA, IPA_TRACE_CG );

  // Add entry and exit nodes:
  Set_saved_root ( GRAPH_root (g) );
  this -> Set_entry ( entry = g->Add_Node ( NULL ) );
  this -> Set_exit  ( exit  = g->Add_Node ( NULL ) );
  GRAPH_root(g) = entry;

  // We need a vector to note reachable nodes:
  vertex_max = GRAPH_vmax(g);
  reach = (mBOOL *) MEM_POOL_Alloc ( pool, sizeof(NODE_INDEX)*vertex_max );
  if ( reach == NULL ) {
    ErrMsg ( EC_No_Mem, "DFBASE::Connect_graph" );
  }

  // Trace:
  if ( Trace_CG ) {
    fprintf ( TFile,
	      "<ips> Connect_Graph: entry=%d, exit=%d, count=%d (max=%d)\n",
	      entry, exit, GRAPH_vcnt(g), vertex_max );
  }
 
  // Phase 1:  Iterate over all the vertices:
  FOR_EACH_NODE ( g, v ) {

    // If the vertex has no predecessors, create an edge from entry:
    if ( g->Num_Preds(v) == 0 ) {
      if ( v != entry ) {
	g->Add_Edge ( entry, v, NULL );
	if ( Trace_CG ) {
	  fprintf ( TFile, "<ips>\tAdding (1n) %d->%d\n", entry, v );
	}
      }
    }

    // If the vertex has no successors, create an edge to exit:
    if ( g->Num_Succs(v) == 0 ) {
      if ( v != exit ) {
	g->Add_Edge ( v, exit, NULL );
	if ( Trace_CG ) {
	  fprintf ( TFile, "<ips>\tAdding (1x) %d->%d\n", v, exit );
	}
      }
    }
  }

  // Phase 2:  Mark nodes reachable from the entry:
  bzero ( reach, sizeof(mBOOL) * vertex_max );
  Forward_Visit ( g, entry, reach );

  // Phase 3:  Find unconnected components and connect them:
  FOR_EACH_NODE ( g, v ) {
    if ( ! reach[v] && v != entry ) {
      g->Add_Edge ( entry, v, NULL );
      Forward_Visit ( g, v, reach );
      if ( Trace_CG ) {
	fprintf ( TFile, "<ips>\tAdding (3n) %d->%d\n", entry, v );
      }
    }
  }

  // Phase 4:  Mark nodes reachable from the exit:
  bzero ( reach, sizeof(mBOOL) * vertex_max );
  Backward_Visit ( g, exit, reach );

  // Phase 5:  Find unconnected components and connect them:
  FOR_EACH_NODE ( g, v ) {
    if ( ! reach[v] && v != exit ) {
      g->Add_Edge ( v, exit, NULL );
      Backward_Visit ( g, v, reach );
      if ( Trace_CG ) {
	fprintf ( TFile, "<ips>\tAdding (5x) %d->%d\n", v, exit );
      }
    }
  }

  // Free our junk memory:
  MEM_POOL_FREE ( pool, reach );
}

// ====================================================================
//
// Reverse_Graph  /  DFBASE::Reverse_graph
//
// Reverse the graph for backward data flow problems.  The first
// function does the underlying graph reversal (i.e. changes direction
// of all the edges and switches predecessor and successor lists), and
// the second also switches the entry/exit indicators in the dataflow
// control structure.
//
// ====================================================================

void
Reverse_Graph ( GRAPH *g )
{
  NODE_INDEX v;
  EDGE_INDEX e, e2;

  // Switch from and to lists of vertices: from->to, to->from:
  for ( v=0; v<GRAPH_vmax(g); v++ ) {
    if ( NODE_fcnt ( &GRAPH_v_i(g,v) ) != -1 ) {
      e = NODE_from ( &GRAPH_v_i(g,v) );
      NODE_from ( &GRAPH_v_i(g,v) ) = NODE_to ( &GRAPH_v_i(g,v) );
      NODE_to ( &GRAPH_v_i(g,v) ) = e;
    }
  }

  // Switch from and to vertices of edges:
  for ( e=0; e<GRAPH_emax(g); e++ ) {
    if ( EDGE_from ( &GRAPH_e_i(g,e) ) != INVALID_EDGE_INDEX ) {
      v = EDGE_from ( &GRAPH_e_i(g,e) ) ;
      EDGE_from ( &GRAPH_e_i(g,e) ) = EDGE_to ( &GRAPH_e_i(g,e) );
      EDGE_to ( &GRAPH_e_i(g,e) ) = v;
      e2 = EDGE_nfrom ( &GRAPH_e_i(g,e) );
      EDGE_nfrom ( &GRAPH_e_i(g,e) ) = EDGE_nto ( &GRAPH_e_i(g,e) );
      EDGE_nto ( &GRAPH_e_i(g,e) ) = e2;
   }
  }
}

// ====================================================================

void
DFBASE::Reverse_graph ()
{
  NODE_INDEX new_entry;

  // Reverse the underlying graph:
  Reverse_Graph ( Get_callgraph()->Graph() );

  // Exchange the entry/exit nodes:
  new_entry = this->Get_exit();
  Set_exit ( Get_entry() );
  Set_entry ( new_entry );

  GRAPH_root ( Get_callgraph()->Graph() ) = new_entry;

  _reversed = ! _reversed;
}

// ====================================================================
//
// DFBASE::Restore_graph
//
// Restore graph to its original form, removing the entry and exit
// nodes, and reversing it for backward dataflow problems.
//
// ====================================================================

void
DFBASE::Restore_graph ()
{
  GRAPH *g = Get_callgraph()->Graph();

  if ( Get_reversed() ) {
    Reverse_Graph ( g );
  }
 
  // Delete those unneccesary vertices: all edges originating to and
  // from the vertices will be eliminated by default:
  g->Delete_Node ( Get_entry() );
  Set_entry ( INVALID_NODE_INDEX );
  g->Delete_Node ( Get_exit() );
  Set_exit ( INVALID_NODE_INDEX );

  GRAPH_root(g) = Get_saved_root(); 
  Set_saved_root ( INVALID_NODE_INDEX );
}

// ====================================================================
// ====================================================================
//
// DF_PRINT -- an example instance of a dataflow solver
//
// This class is a derived from the dataflow solver class DFBASE using
// simple instantiations of the associated function templates.  Its
// purpose, aside from serving as an example, is to trace the call
// graph in the order implied by the solver's iterator.
//
// ====================================================================
// ====================================================================

class DF_PRINT : public DFBASE
{
 public:
  // These will in general be the types of the per-node problem data:
  // in this case, we don't need any, so we'll just store NULLs.
  typedef void IN_T;
  typedef void OUT_T;

 private:
  // All dataflow solver instances must have these:
  IN_T **_in;	// Input annotations -- not used
  OUT_T **_out;	// Output annotations -- not used

  // This field is specific to this instance:
  FILE *_f;	// Output file

 public:

  // Constructor -- DFBASE's parameters plus a file:
  DF_PRINT ( IPA_CALL_GRAPH &cg, DF_DIRECTION df,
	     MEM_POOL *m, DFN *iter, FILE *f );

  // Destructor -- free extra data:
  ~DF_PRINT ();

  // ====== Field access =====
  inline FILE *Get_file() const { return _f; };

  // The following accessors are required by the solver templates;
  // they can generally be identical to these given IN_T and OUT_T.
  inline IN_T  **Get_in()  const { return _in; };
  inline OUT_T **Get_out() const { return _out; };
  inline void Set_in  ( IN_T **in )   { _in = in; };
  inline void Set_out ( OUT_T **out ) { _out = out; };
  inline IN_T  *Get_in_elmt(NODE_INDEX i)  const { return _in[i]; };
  inline OUT_T *Get_out_elmt(NODE_INDEX i) const { return _out[i]; };
  inline void Set_in_elmt  ( NODE_INDEX i, IN_T *in )
	{ _in[i] = in; };
  inline void Set_out_elmt ( NODE_INDEX i, OUT_T *out )
	{ _out[i] = out; };

  // Initialize the annotation of a callgraph node -- we don't need
  // any initialization for this example, but the function is
  // required by the instantiation that follows.
  inline void Initialize_node ( void * ) { return; };

  // This function, to initialize the callgraph vertex and in/out
  // annotations, is required by the solver.  It can normally be just
  // an instantiation of the template, identical to this one, given
  // Initialize_node above.
  inline void Initialize_annotations ()
  {
    Initialize_Annotation ( this, this->_in, this->_out );
  };

  // The Meet function merges data from the input edges and the current
  // input annotation (given by the in parameter), producing a new
  // input annotation, which it returns.  It must be able to cope with
  // a NULL input annotation the first time.
  IN_T *Meet ( IN_T *in, void *vertex_user );

  // The Trans function transfers data from the input annotation (the
  // in parameter) using the node information (vertex_user) and the old
  // output annotation (out), to produce a new output annotation.  In
  // this example, we don't do anything interesting.
  OUT_T *Trans ( IN_T *in, OUT_T *out, void *vertex_user );

  // One or both of Meet and Trans must set _changed to TRUE if either
  // makes a change which may affect the solution for successor nodes.
  // In our example, neither will ever set it, so we will do a single
  // iteration over the call graph.

  // A single-iteration callgraph walk occurs in the solver core, which
  // should be an instantiation of Iterative_Solver_Core:
  inline void Solver_core()
  {
    Iterative_Solver_Core ( this );
  }

  // The master solver needs to call the solver core above, as well
  // as the various setup/cleanup routines.  As an interative solver,
  // it should be an instantiation of Iterative_Dataflow_Solver:
  inline void Solver()
  {
    Iterative_Dataflow_Solver ( this, Depth_First_Ordering );
  }

  // For output from the solver, there are two steps.  The first
  // initializes any required data structures.  Ours does nothing:
  inline void Init_IO() const { return; };

  // The second output step calls this function for each callgraph
  // node to extract any required information from the solution.
  // Generally, this routine should clean up the callgraph nodes,
  // although could be done in the destructor.  Ours does nothing:
  inline void Post_process_IO ( const void * ) const { return; };

  // Both output steps are called from this extraction routine, which
  // should generally be an instantiation of Extract_Solution as here:
  inline void Extract_solution () { Extract_Solution ( this ); };
};

// ====================================================================
//
// DF_PRINT::DF_PRINT
//
// Constructor for the dataflow printer class.  The graph iterator may
// be NULL, in which case the solver must provide it later.
//
// ====================================================================

DF_PRINT::DF_PRINT (
  IPA_CALL_GRAPH &cg,		// The underlying call graph to use
  DF_DIRECTION direction,	// The dataflow problem direction
  MEM_POOL *mpool,		// Where to put new junk
  DFN *iter,			// An iterator over the graph
  FILE *f )			// File to print to
: DFBASE ( cg, direction, mpool, iter )
{
  // Record the information provided:
  _f = f;
  _in = NULL;
  _out = NULL;
}


// ====================================================================
//
// DF_PRINT::~DF_PRINT
//
// Destructor for the dataflow printer class.  We must remove the
// input and output annotations; the DFBASE destructor will take care
// of restoring the graph and freeing the iterator memory.
//
// ====================================================================

DF_PRINT::~DF_PRINT ()
{
  if ( _in != NULL ) {
    MEM_POOL_FREE ( Get_mempool(), _in );
    _in = NULL;
  }
  if ( _out != NULL ) {
    MEM_POOL_FREE ( Get_mempool(), _out );
    _out = NULL;
  }
}

// ====================================================================
//
// DF_PRINT::Meet
//
// Meet operator for the printer class.  Simply print the vertex
// information and the input edge names.
//
// ====================================================================

DF_PRINT::IN_T *
DF_PRINT::Meet ( IN_T *, void *vertex_user )
{
  IPA_NODE *v = (IPA_NODE *)vertex_user;

  // Print the vertex:
  if ( v != NULL ) {
    fprintf ( _f, "Meet  vertex: " );
    v->Print ( _f );
  } else {
    fprintf ( _f, "Meet  NULL vertex\n" );
    return NULL;
  }

  IPA_SUCC_ITER edge_iter ( Get_callgraph(), v );

  for ( edge_iter.First();
	! edge_iter.Is_Empty();
        edge_iter.Next())
  {
    IPA_EDGE *e = edge_iter.Current_Edge();
    if ( e != NULL ) {
      fprintf ( _f, "\tedge: " );
      e->Trace ( Get_callgraph() );

    } else {
      fprintf ( _f, "\tNULL edge\n" );
    }
  }
  if (! v->Icall_List ().empty () ) {
    // We've got indirect calls -- print their information, too: */
    Ipl_Summary_Symbol = IPA_get_symbol_array (v);
    const IPA_ICALL_LIST& icall_list = v->Icall_List ();

    for (IPA_ICALL_LIST::const_iterator icall_iter = icall_list.begin ();
	 icall_iter != icall_list.end (); ++icall_iter) {

      SUMMARY_CALLSITE *c = (*icall_iter)->Callsite();
      if ( c != NULL ) {
	fprintf ( _f, "\tedge-i: " );
	c->Trace ();
      } else {
	fprintf ( _f, "\tNULL edge-i\n" );
      }
    }
  }
  
  // We're not using the in vector -- just return NULL:
  return NULL;
}

// ====================================================================
//
// DF_PRINT::Trans
//
// Transfer operator for the printer class.  Simply print a bar to
// separate nodes.
//
// ====================================================================

/*ARGSUSED*/
DF_PRINT::OUT_T *
DF_PRINT::Trans ( IN_T *in, OUT_T *out, void *vertex_user )
{
  fprintf ( _f, "%s", SBar );

  // We're not using the out vector -- just return NULL:
  return NULL;
}

// ====================================================================
//
// Trace_Callgraph
//
// Instantiate a dataflow solver as a very large hammer to print a
// trace of the given callgraph.
//
// NOTE:  We carefully push and pop the memory pool here, but it should
// not be necessary if we've handled everything right...
//
// ====================================================================

void
Trace_Callgraph ( IPA_CALL_GRAPH &cg, DF_DIRECTION dir, const char *msg )
{
  // Push the local memory pool:
  MEM_POOL_Push ( MEM_local_pool_ptr );

  // Print the heading:
  fprintf ( TFile, "%s%sDataflow %s Callgraph Trace -- %s\n%s%s\n",
	    DBar, DBar,
	    ( dir == FORWARD ) ? "Forward" : "Backward", msg,
	    DBar, DBar );

  {
    // Create a solver class instance:
    DF_PRINT solver ( cg, dir, MEM_local_pool_ptr, NULL, TFile );

    // Run the solver:
    solver.Solver ();

    // Clean up after ourselves:
    solver.Extract_solution ();

    // Destructor called here...
  }

  // Pop the local memory pool:
  MEM_POOL_Pop ( MEM_local_pool_ptr );
}
