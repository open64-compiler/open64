//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_dfs.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_dfs.h,v $
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
// Description:
//
// The template depth-first search package requires that you define a
// single class that describes the implementation of the search. The
// class must have the following members:
//
// Member typedefs:
//   node_type
//   adj_list_type
//   adj_list_iter_type
//   node_iterator
//
//   TODO: Document the typedefs.
//
// Member functions:
//   constructor(node_type *N)
//      Sets the current node in the search to be node *N; N can also
//      be NULL for a top-level search descriptor that carries only
//      type information for templatization. This hack would not be
//      necessary if not for a bug in our C++ front end (see below).
//
//   void Set_seen(node_type *N)
//      Has the side effect of noting that the search has visited the
//      node *N. Also incorporates any other side-effect actions that
//      need to be done in preorder (before the search recurses on the
//      neighbors of *N).
//
//   BOOL Seen(node_type *N)
//      Returns TRUE if Set_seen(N) has been called since the start of
//      the search, otherwise returns FALSE.
//
//   void Reach_from_to(node_type *M, INT tag, node_type *N)
//      Performs any side-effect actions that need to be done to any
//      neighbor *N of node *M when the search is visiting node
//      *M. Note that the search may or may not go on to recurse for
//      N. For many search implementations, Reach_from_to() will be an
//      empty function that does nothing.
//      TODO: Document the parameter list properly.
//
//   BOOL Start_from(node_type *N)
//      Returns TRUE if the search should begin from node *N, otherwise
//      returns FALSE. When Start_from(N) is called, it is guaranteed
//      that Seen(N) is FALSE.
//
//   BOOL Continue_from_to(node_type *M, INT tag, node_type *N)
//      Returns TRUE if the search should recurse from node *M to node
//      *N, where *N is a neighbor of *M. When
//      Continue_from_to(M, tag, N) is called,
//      Reach_from_to(M, tag, N) is guaranteed to have been called
//      already, Set_seen(M) has been called, and Seen(N) has just
//      returned FALSE.
//      TODO: Document the parameter list properly and provide a
//      better explanation.
//
//   void Postorder_processing(node_type *N)
//      Performs any side-effect actions that should be done after the
//      recursion on *N's neighbors is complete.
//
//   node_type *Current_node(void)
//      Returns a pointer to the current node being visited by the
//      search. This function exists because of a bug in our C++ front
//      end that keeps us from making the current node an argument to
//      Df_search with a parameterized type.
//
//   adj_list_type *Neighbors(node_type *N)
//      Returns a pointer to the list of neighbors of node *N in a form
//      acceptable to the function
//      adj_list_iter_type::Init(adj_list_type *).
//
//   BOOL Tracing(void)
//      Returns a flag that tells whether the depth-first search
//      template routines should trace their progress to TFile.
//
//   The following member is required only if Perform_dfs is used to
//   check each node in the graph for starting points of the
//   search. If the search is started only "manually" via direct calls
//   to the nodewise Df_search routine, this member isn't needed.
//   x Nodes(void)
//      Returns an entity of type x which, when passed to
//      node_iterator::Init(x), initializes *this to iterate over all
//      nodes in the universe of interest where the depth-first search
//      might originate. The specifics of type x are left up to the
//      designer of the search implementation descriptor class.
//
// ====================================================================
// ====================================================================


// In the following template function definition, we unfortunately
// have to specify EXP_OCCURS explicitly rather than use
// search_type::node_type. This seems to be a EDG front-end bug.

template <class search_type> void
Df_search(const search_type &srch)
{
  Is_Trace(srch.Tracing(), (TFile, "%s: searching ", srch.Search_name()));
  Is_Trace_cmd(srch.Tracing(), srch.Current_node()->Print(TFile));

  srch.Set_seen(srch.Current_node());

  typename search_type::adj_list_type      *neighbor;
  typename search_type::adj_list_iter_type  neighbor_iter;

  FOR_ALL_NODE(neighbor,
	       neighbor_iter,
	       Init(srch.Neighbors(srch.Current_node()))) {
    Is_Trace(srch.Tracing(), (TFile, "%s: reaching ", srch.Search_name()));
    Is_Trace_cmd(srch.Tracing(), neighbor->Node()->Print(TFile));

    srch.Reach_from_to(srch.Current_node(),
		       neighbor->Opnd_idx(),
		       neighbor->Node());
    if (!srch.Seen(neighbor->Node()) &&
	srch.Continue_from_to(srch.Current_node(),
			      neighbor->Opnd_idx(),
			      neighbor->Node())) {
      search_type new_srch(neighbor->Node());
      Df_search(new_srch);
    }
  }
  srch.Postorder_processing(srch.Current_node());
}

template <class search_type> void
Perform_dfs(const search_type &srch)
{
  typename search_type::node_iterator  nodes;
  typename search_type::node_type      *node;

  FOR_ALL_NODE(node, nodes, Init(srch.Nodes())) {
    if (!srch.Seen(node) && srch.Start_from(node)) {
      Is_Trace(srch.Tracing(), (TFile, "%s: Beginning from ", srch.Search_name()));
      Is_Trace_cmd(srch.Tracing(), node->Print(TFile));

      search_type srch(node);
      Df_search(srch);
    }
  }
}
