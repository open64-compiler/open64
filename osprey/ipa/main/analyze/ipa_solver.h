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
// Module: ipa_solver.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_solver.h,v $
//
// Revision history:
//  19-Sep-95 - Initial version
//
// Description:
//
// This package provides a template interface for solving dataflow
// problems over the call graph.  It is derived from the base class
// implementation of ipa_df.cxx.
//
// The associated source file ipa_solver.cxx contains an instantiation
// of these facilities for the purpose of printing a callgraph.  See it
// (the routine Trace_Callgraph and the class DF_PRINT) for a complete
// description of how this is intended to be used.  The IPA aliasing
// and mod/ref analysis in ipaa.cxx contains a more extensive instance.
//
// ====================================================================
// ====================================================================

#ifdef _KEEP_RCS_ID
static char *ipa_solver_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_solver.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#ifndef cxx_ipa_solver_INCLUDED
#define cxx_ipa_solver_INCLUDED

// This header requires prior inclusion of erglob.h, ip_graph.h.


#ifndef cxx_ipa_df_INCLUDED
#include "ipa_df.h"                 // DF_DIRECTION
#endif

#ifndef ipa_option_INCLUDED
#include "ipa_option.h"
#endif


// ====================================================================
//
// Utility functions
//
// ====================================================================

// Reverse the graph for backward data flow problems:
extern void Reverse_Graph ( GRAPH *g );

// Trace a callgraph via the depth-first iterator in the given
// direction.  This routine serves as an example of using the dataflow
// solver facilities in this module.
extern void Trace_Callgraph (
  IPA_CALL_GRAPH &cg,		// Callgraph to trace
  DF_DIRECTION direction,	// Direction to use
  const char *msg		// Message to add to heading
);

// ====================================================================
// ====================================================================
//
// Define some infrastructure for manipulating the dataflow problem.
//
// ====================================================================
// ====================================================================


// ====================================================================
//
// DFBASE:  Dataflow solver base class.
//
// It is expected that every dataflow solver will be a class derived
// from this one, with additional member functions having
// implementations derived from the templates below.
//
// ====================================================================

class DFBASE
{
 protected:

  MEM_POOL *_m;		// Mempool to use for allocation
  IPA_CALL_GRAPH *_cg;	// Underlying callgraph
  DFN *_iter;		// Iterator over the callgraph
  NODE_INDEX _entry, _exit;	// Entry, exit nodes that connect the entire graph
  NODE_INDEX _saved_root;	// Original root node of the graph
  DF_DIRECTION _dir;	// Data flow direction
  mBOOL _reversed;	// Has the callgraph been reversed?
  mBOOL _changed;	// Has the solution changed yet this round?

 public:

  // Constructor:  This sets the data fields to the given values,
  // connects the callgraph to distinct entry and exit nodes.  If
  // a NULL iterator is provided, it is left to the solver to provide.
  DFBASE ( IPA_CALL_GRAPH &cg, DF_DIRECTION direction,
	   MEM_POOL *mpool, DFN *iter );

  // Destructor:  If necessary, this restores the underlying callgraph
  // and frees the iterator memory:
  ~DFBASE ();

  // ==== Field references =====
  // Don't change these fields after construction:
  MEM_POOL *Get_mempool () const	{ return _m; };
  IPA_CALL_GRAPH *Get_callgraph () const { return _cg; };

  // These fields may change after construction:
  DF_DIRECTION Get_direction () const	{ return _dir; };
  void Set_direction ( DF_DIRECTION d )	{ _dir = d; };
  DFN	*Get_iter () const		{ return _iter; };
  void	Set_iter ( DFN *d )		{ _iter = d; };
  NODE_INDEX Get_entry () const		{ return _entry; };
  void	Set_entry ( NODE_INDEX entry )	{ _entry = entry; };
  NODE_INDEX Get_exit () const		{ return _exit; };
  void	Set_exit ( NODE_INDEX exit )	{ _exit = exit; };
  NODE_INDEX Get_saved_root () const	{ return _saved_root; };
  void	Set_saved_root ( NODE_INDEX root )	{ _saved_root = root; };
  BOOL	Get_changed () const		{ return _changed; };
  void	Set_changed ()			{ _changed = TRUE; };
  void	Reset_changed ()		{ _changed = FALSE; };
  BOOL	Get_reversed () const		{ return _reversed; };

  // Ensure that the entire graph is connected, with a unique entry
  // and a unique exit node:
  void Connect_graph ( MEM_POOL *pool );

  // Reverse the graph for backward data flow problems.
  void Reverse_graph ();

  // Restore graph to its original form, removing the entry and exit
  // nodes, and reversing it for backward dataflow problems:
  void Restore_graph ();

  // Query the underlying callgraph.  Note that the meanings of the
  // edge ends is inverted if the graph has been reversed:
  IPA_NODE* Get_caller ( const IPA_EDGE *edge ) const
	{ return _reversed ? _cg->Callee(edge)
			   : _cg->Caller(edge); };
  IPA_NODE* Get_callee ( const IPA_EDGE *edge ) const
	{ return _reversed ? _cg->Caller(edge)
			   : _cg->Callee(edge); };

  // Clone a node in the underlying callgraph:
  IPA_NODE* Clone ( IPA_NODE *n)
	{ return ( _cg->Create_Clone(n) ); };
};

// ====================================================================
// ====================================================================
//
// Define the template functions to be used in setting up a dataflow
// solver.
//
// In each of the following templates, the formal class parameter
// DF_T is the type derived from DFBASE above.  IN_T is the type of its
// input annotations, and OUT_T is the type of its output annotations.
//
// ====================================================================
// ====================================================================

// ====================================================================
//
// Initialize_Annotation
//
// Initialize the in/out annotations on the callgraph nodes.
// The 'in' and 'out' formal function parameters are strictly for the
// purpose of establishing their types.
//
// This function is generally called by the solver template, because
// the initialization may depend on having the graph already
// reversed.  Therefore, a class with a multi-solver problem (e.g.
// alias analysis does a backward phase followed by a forward phase)
// needs to make sure it can deal with already-initialized data
// structures.
//
// REQUIRES -- the DF_T class must have members:
//	void Set_in ( IN_T ** );	-- input annotation array
//	IN_T ** Get_in ();
//	void Set_out ( OUT_T ** );	-- output annotation array
//	OUT_T** Get_out ();
//	void Initialize_node ( void * user_data );
//
// ====================================================================

template < class DF_T, class IN_T, class OUT_T >
void
Initialize_Annotation ( DF_T *dataflow, IN_T **, OUT_T ** )
{
  INT32 i;
  DFN *iter = dataflow->Get_iter();
  GRAPH *graph = dataflow->Get_callgraph()->Graph();
  MEM_POOL *mpool = dataflow->Get_mempool();
  IN_T **in = dataflow->Get_in();
  OUT_T **out = dataflow->Get_out();

  // Create the input annotations for all vertices:
  if ( in == NULL ) {
    INT32 size_in = sizeof(IN_T*) * GRAPH_vmax(graph);

    in = (IN_T**) MEM_POOL_Alloc ( mpool, size_in );
    if ( in == NULL ) {
      ErrMsg ( EC_No_Mem, "Initialize_Annotation: input annotations" );
    } else {
      bzero ( in, size_in );
      dataflow->Set_in ( in );
    }
  }

  // Create the output annotations for all vertices:
  if ( out == NULL ) {
    INT32 size_out = sizeof(OUT_T*) * GRAPH_vmax(graph);

    out = (OUT_T**) MEM_POOL_Alloc ( mpool, size_out );
    if ( out == NULL ) {
      ErrMsg ( EC_No_Mem, "Initialize_Annotation: output annotations" );
    } else {
      bzero ( out, size_out );
      dataflow->Set_out ( out );
    }
  }

  // Check the allocations and clear the resulting arrays:

  for ( i=DFN_first(iter); i< DFN_end(iter); ++i ) {
    NODE_INDEX vindex = DFN_v_list_i(iter,i);

    if ( vindex == dataflow->Get_entry() ) continue;
    dataflow->Initialize_node (
	NODE_user ( &GRAPH_v_i ( graph, vindex ) ) );
  }
}

// ====================================================================
//
// Iterative_Solver_Core
//
// Do one iteration over the dataflow graph, in the order given by its
// iterator.  For each of the callgraph nodes, call the Meet function
// for each of its incoming edges, producing its 'in' annotation, and
// then the Trans function to produce its 'out' annotation.
//
// The parameters passed to Meet are the input sets to the meet (in),
// the predecessor's current data flow set (pred_set), the annotation
// in the callgraph vertex obtained from the local phase, and the edge
// annotation obtained from the local phase (on the first iteration).
// For the subsequent iterations, it reflects the result of the Trans
// operation.
//
// For a call graph, local node annotation corresponds to summary
// information collected for a function or procedure and edge
// annotation refers to summary information collected for a call site.
// The parameters passed to trans are the result of the meet operation
// and the previous out annotation and the variable change which
// is used to determine whether the data flow problem has settled.
//
// REQUIRES -- the DF_T class must have members:
//	void Set_in_elmt ( NODE_INDEX i, IN_T * );
//	IN_T *Get_in_elmt ( NODE_INDEX i );
//	void Set_out_elmt ( NODE_INDEX i, OUT_T * );
//	OUT_T *Get_out_elmt ( NODE_INDEX i );
//	IN_T *Meet ( IN_T *, VUSER_T * );
//	OUT_T *Trans ( IN_T *, OUT_T *, VUSER_T * );
// where:
//	IN_T is the type of an input annotation.
//	OUT_T is the type of an output annotation.
//	VUSER_T is the type of a callgraph vertex user annotation.
//
// ====================================================================

template < class DF_T >
void
Iterative_Solver_Core ( DF_T *dataflow )
{
  DFN *iter = dataflow->Get_iter();
  IPA_CALL_GRAPH *callgraph = dataflow->Get_callgraph();
  GRAPH *graph = callgraph->Graph();
  NODE_INDEX i;

  // Make sure that iter is not empty:
  assert ( iter != NULL );

  if ( Get_Trace ( TP_IPA, IPA_TRACE_CG ) ) {
    Print_DFN ( TFile, iter );
  }

  // for all vertices in depth first ordering, perform meet and trans:
  for ( i=DFN_first(iter); i< DFN_end(iter); ++i ) {
    INT vindex = DFN_v_list_i(iter,i);

    if ( vindex != dataflow->Get_entry()
      && vindex != dataflow->Get_exit() )
    { // Avoid the special entry and exit nodes, since they are there
      // only to connect the entire graph.
      
      // Compute the meet of incoming edge and existing in.  For
      // the meet operation, pass the in set, the predecessor's out
      // annotation, the current node annotation and current edge
      // annotation.

      // NOTE: for a call graph and the forward dataflow problem,
      // the current node annotation refers to the callee and the
      // edge annotation refers to the callsite note, the meet
      // operation will delete the old in and return the new in.
		       
      dataflow->Set_in_elmt (
		    vindex,
		    dataflow->Meet (
			  dataflow->Get_in_elmt ( vindex ),
			  NODE_user ( &GRAPH_v_i (graph, vindex) ) )
		 );

      // Transfer: pass the in set, the out set, and the current
      // node annotation (for a call graph, the procedure node).  It
      // must set the _changed indicator in the class instance to
      // indicate whether the dataflow problem has settled.
      //
      // NOTE: the trans operation will delete the old out and return
      // the new out.

      dataflow->Set_out_elmt (
		    vindex,
		    dataflow->Trans (
			  dataflow->Get_in_elmt ( vindex ),
			  dataflow->Get_out_elmt ( vindex ),
			  NODE_user ( &GRAPH_v_i ( graph, vindex ) ) )
		  );
      
    }
  }
}

// ====================================================================
//
// Iterative_Dataflow_Solver
//
// Main driver for an iterative solver.  In addition to the DF_T class
// parameter, it requires a GRAPH_ITER parameter which is a function:
//	DFN *GRAPH_ITER ( GRAPH *g, MEM_POOL *m );
// returning a node iterator of type DFN *, e.g. Depth_First_Ordering.
//
// REQUIRES -- the DF_T class must have members:
//	void Initialize_annotations ();
//	void Solver_core ();
//	void Init_IO ();
//	void Post_process_IO ( void *vertex_user );
//
// ====================================================================

template < class DF_T, class GRAPH_ITER >
void
Iterative_Dataflow_Solver (
  DF_T *dataflow,	// The solver class object
  GRAPH_ITER get_iter )	// Function to create a callgraph iterator
{
  // We'll need a handle on the underlying callgraph:
  IPA_CALL_GRAPH *callgraph = dataflow->Get_callgraph();
  GRAPH *graph = callgraph->Graph();

  /* If it's a backward data flow solver, reverse the graph */
  if ( dataflow->Get_direction() == BACKWARD ) {
    dataflow->Reverse_graph ();
  }

  if ( dataflow->Get_iter() == NULL ) {
    dataflow->Set_iter ( get_iter ( graph,
				    dataflow->Get_mempool() ) );
  }

  // Initialize callgraph vertex, in, and out annotations:
  dataflow->Initialize_annotations ();

  dataflow->Set_changed ();

  // While the data flow problem has not settled do:
  while ( dataflow->Get_changed() ) {
    // Reset changed to FALSE.  Any step which changes information
    // during the iteration (i.e. Meet or Trans) must set it to TRUE.
    dataflow->Reset_changed();

    // Call the workhorse dataflow solver:
    dataflow->Solver_core ();
  }
}

// ====================================================================
//
// Extract_Solution
//
// Extract the solution data from the solved problem.
//
// REQUIRES -- the DF_T class must have members:
//	void Init_IO ();
//	void Post_process_IO ( void *vertex_user );
//
// ====================================================================

template < class DF_T >
void
Extract_Solution ( DF_T *dataflow )
{
  // We'll need a handle on the underlying callgraph:
  IPA_CALL_GRAPH *callgraph = dataflow->Get_callgraph();
  GRAPH *graph = callgraph->Graph();

  // Other temporaries:
  INT i;

  // Initialize any data structures that are needed by post processing:
  dataflow->Init_IO ();

  // Do any necessary post processing:
  // In the case of constant propagation, the tcons need to be reset.

  DFN* iter = dataflow->Get_iter();
  for ( i=DFN_first(iter); i< DFN_end(iter); ++i ) {
    NODE_INDEX vindex = DFN_v_list_i(iter,i);

    if ( vindex == dataflow->Get_entry()
      || vindex == dataflow->Get_exit() )
    {
      continue;
    }

    dataflow->Post_process_IO ( NODE_user(&GRAPH_v_i(graph,vindex)) );
  }
}

#endif /* cxx_ipa_solver_INCLUDED */

