/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_fb.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_fb.cxx,v $
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
//  Implementation of a simple feedback data structure to support both
//  vertex and edge frequency.
//
// ====================================================================
// ====================================================================


#include "opt_fb.h"
#include <stack>
#include "opt_htable.h" // for STMTREP
#include "DaVinci.h"    // for DaVinci viewer

using std::map;

// ====================================================================

#define VALIDATE_NODEX( nx, s ) \
Is_True( nx != IDTYPE_NULL, ( "OPT_FEEDBACK::" s " invalid node") )

#define VALIDATE_EDGEX( ex, s ) \
Is_True( ex != IDTYPE_NULL, ( "OPT_FEEDBACK::" s " invalid edge") )

#define VALIDATE_EDGE( nx_src, nx_dst, s ) \
Is_True( nx_src != IDTYPE_NULL && nx_dst != IDTYPE_NULL, \
	( "OPT_FEEDBACK::" s " invalid edge ( %d --> %d)", nx_src, nx_dst) )


// ====================================================================
// Update feedback when cfg is modified
// ====================================================================

bool
OPT_FEEDBACK::Edge_has_freq( IDTYPE nx_src, IDTYPE nx_dst ) const
{
  VALIDATE_EDGE( nx_src, nx_dst, "Edge_has_freq" );

  const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    if ( _fb_opt_edges[ex].destination == nx_dst ) {
      return true;
    }
  }
  return false;
}


IDTYPE
OPT_FEEDBACK::Find_edge_by_type( IDTYPE nx, FB_EDGE_TYPE edge_type ) const
{
  VALIDATE_NODEX( nx, "Find_edge_by_type" );

  const OPT_FB_NODE& node = _fb_opt_nodes[nx];
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    if ( _fb_opt_edges[ex].edge_type == edge_type ) {
      return ex;
    }
  }
  return IDTYPE_NULL;
}


FB_FREQ
OPT_FEEDBACK::Get_edge_freq_by_type( IDTYPE nx, FB_EDGE_TYPE edge_type ) const
{
  VALIDATE_NODEX( nx, "Get_edge_freq_by_type" );

  const OPT_FB_NODE& node = _fb_opt_nodes[nx];
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    if ( _fb_opt_edges[ex].edge_type == edge_type ) {
      return _fb_opt_edges[ex].freq;
    }
  }
  Is_True( false, ( "OPT_FEEDBACK::Get_edge_freq_by_type: edge not found" ) );
  return FB_FREQ_UNINIT;
}


// Return sum of frequencies of all edges (nx_src ==> nx_dst)
FB_FREQ
OPT_FEEDBACK::Get_edge_freq( IDTYPE nx_src, IDTYPE nx_dst ) const
{
  VALIDATE_EDGE( nx_src, nx_dst, "Get_edge_freq" );
  Is_True( Edge_has_freq( nx_src, nx_dst ),
	   ( "OPT_FEEDBACK::Get_edge_freq: edge (%d --> %d) not found",
	     nx_src, nx_dst ) );

  const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
  FB_FREQ freq  = FB_FREQ_ZERO;
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    if (edge.destination == nx_dst)
      freq += edge.freq;
  }
  return freq;
}

// Get the type of the first edge between nx_src and nx_dst.
FB_EDGE_TYPE 
OPT_FEEDBACK::Get_edge_type(IDTYPE nx_src, IDTYPE nx_dst) const
{
  VALIDATE_EDGE( nx_src, nx_dst, "Get_edge_type" );
  Is_True( Edge_has_freq( nx_src, nx_dst ),
	   ( "OPT_FEEDBACK::Get_edge_freq: edge (%d --> %d) not found",
	     nx_src, nx_dst ) );

  const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
  FB_FREQ freq  = FB_FREQ_ZERO;
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    if (edge.destination == nx_dst)
      return edge.edge_type;
  }
  FmtAssert(FALSE, ( "OPT_FEEDBACK::Get_edge_freq edge not found"));
  return FB_EDGE_UNINIT;
}

// Set the type of the given edge.
void
OPT_FEEDBACK::Set_edge_type(IDTYPE ex, FB_EDGE_TYPE type)
{
  OPT_FB_EDGE& edge = _fb_opt_edges[ex];
  edge.edge_type = type;
}

// Get the edge between the given pair of nodes.
IDTYPE
OPT_FEEDBACK::Get_edge(IDTYPE nx_src, IDTYPE nx_dst) const
{
  VALIDATE_EDGE( nx_src, nx_dst, "Get_edge_type" );
  Is_True( Edge_has_freq( nx_src, nx_dst ),
	   ( "OPT_FEEDBACK::Get_edge_freq: edge (%d --> %d) not found",
	     nx_src, nx_dst ) );

  const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    if (edge.destination == nx_dst)
      return ex;
  }
  FmtAssert(FALSE, ( "OPT_FEEDBACK::Get_edge edge not found"));
  return IDTYPE_NULL;
}

// Returns the unique successor, or IDTYPE_NULL if none.
IDTYPE
OPT_FEEDBACK::Get_node_successor( IDTYPE nx ) const
{
  VALIDATE_NODEX( nx, "Get_node_successor" );

  IDTYPE nx_succ = IDTYPE_NULL;
  const OPT_FB_NODE& node = _fb_opt_nodes[nx];
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex     = node.outgoing_edges[t];
    IDTYPE nx_dst = _fb_opt_edges[ex].destination;
    Is_True( nx_succ == IDTYPE_NULL || nx_succ == nx_dst,
	     ( "OPT_FEEDBACK::Get_node_successor:"
	       " node %d has multiple successors", nx ) );
    nx_succ = nx_dst;
  }
  return nx_succ;
}


float
OPT_FEEDBACK::Get_pred_prob( IDTYPE nx_src, IDTYPE nx_dst ) const
{
  if ( ! Edge_has_freq( nx_src, nx_dst ) )
    return 0.0;
  else {
    const OPT_FB_NODE& node = _fb_opt_nodes[nx_dst];
    FB_FREQ freq = Get_edge_freq( nx_src, nx_dst );
    freq /= node.freq_total_in;
    if ( freq.Known() )
      return freq.Value();
    else
      return 1.0 / node.incoming_edges.size();
  }
}


float
OPT_FEEDBACK::Get_succ_prob( IDTYPE nx_src, IDTYPE nx_dst ) const
{
  if ( ! Edge_has_freq( nx_src, nx_dst ) )
    return 0.0;
  else {
    const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
    FB_FREQ freq = Get_edge_freq( nx_src, nx_dst );
    freq /= node.freq_total_out;
    if ( freq.Known() )
      return freq.Value();
    else
      return 1.0 / node.outgoing_edges.size();
  }
}


// ====================================================================
// Update feedback when cfg is modified (private methods)
//   May invalidate frequency balance!
// ====================================================================

// Add a new node with no incoming nor outgoing edges
void
OPT_FEEDBACK::Add_node( IDTYPE nx_new )
{
  if ( _trace )
    fprintf( TFile, "  OPT_FEEDBACK::Add_node(%d)\n", nx_new );
  VALIDATE_NODEX( nx_new, "Add_node" );

  // Make sure _fb_opt_nodes can hold the node
  if ( nx_new >= _fb_opt_nodes.size() )
    _fb_opt_nodes.insert( _fb_opt_nodes.end(),
			  nx_new - _fb_opt_nodes.size() + 1,
			  OPT_FB_NODE( _mem_pool ) );
}


// Add_edge adds a new edge to the feedback cfg, and updates frequency data
void
OPT_FEEDBACK::Add_edge( IDTYPE nx_src, IDTYPE nx_dst,
			FB_EDGE_TYPE edge_type, FB_FREQ freq )
{
  if ( _trace )
    fprintf( TFile, "  OPT_FEEDBACK::Add_edge(%d --> %d)\n", nx_src, nx_dst );
  VALIDATE_EDGE( nx_src, nx_dst, "Add_edge" );
  Is_True( Find_edge_by_type( nx_src, edge_type ) == IDTYPE_NULL,
	   ( "OPT_FEEDBACK::Add_edge: redundant edge" ) );

  OPT_FB_NODE& node_src = _fb_opt_nodes[nx_src];
  OPT_FB_NODE& node_dst = _fb_opt_nodes[nx_dst];

  // Add edge to feedback cfg
  OPT_FB_EDGE new_edge( nx_src, nx_dst, edge_type, freq );
  IDTYPE ex_new = _fb_opt_edges.size();
  _fb_opt_edges.push_back( new_edge );
  node_src.outgoing_edges.push_back( ex_new );
  node_dst.incoming_edges.push_back( ex_new );

  // Update node frequency data
  if ( ! freq.Exact() ) {
    node_src.unexact_out += 1;
    node_dst.unexact_in  += 1;

    if ( ! freq.Known() ) {
      node_src.unknown_out += 1;
      node_dst.unknown_in  += 1;
    }
  }
  node_src.freq_total_out += freq;
  node_dst.freq_total_in  += freq;
}


// quickly removes ex from vector ex_list; order of ex_list not maintained,
//   but iterating backwards through ex_list while removing edges is safe
inline void
remove_ex( vector<IDTYPE, mempool_allocator<IDTYPE> >& ex_list, IDTYPE ex )
{
  INT last = ex_list.size() - 1;
  for ( INT t = last; t >= 0; t-- ) {
    if ( ex_list[t] == ex ) {
      ex_list[t] = ex_list[last];
      ex_list.pop_back();
      return;
    }
  }
  Is_True( false, ( "remove_ex (in opt_fb.cxx): ex == %d not found", ex ) );
}


// quickly replaces ex_old by ex_new within vector ex_list
inline void
replace_ex( vector<IDTYPE, mempool_allocator<IDTYPE> >& ex_list,
	    IDTYPE ex_old, IDTYPE ex_new )
{
  for ( INT t = ex_list.size() - 1; t >= 0; t-- ) {
    if ( ex_list[t] == ex_old ) {
      ex_list[t] = ex_new;
      return;
    }
  }
  Is_True( false,
	   ( "replace_ex (in opt_fb.cxx): ex_old == %d not found", ex_old ) );
}


// Deletes an edge from the feedback cfg
//   Iterating backwards through incoming or outgoing edge list while
//   Deleting edges is safe
// The edge id number ex is immediately reused, by the current last edge,
// in order to conserve memory
void
OPT_FEEDBACK::Remove_edge( IDTYPE ex )
{
  if ( _trace )
    fprintf( TFile, "  OPT_FEEDBACK::Remove_edge(ex %d)\n", ex );
  VALIDATE_EDGEX( ex, "Remove_edge" );
  OPT_FB_EDGE& edge = _fb_opt_edges[ex];
  OPT_FB_NODE& node_src = _fb_opt_nodes[edge.source];
  OPT_FB_NODE& node_dst = _fb_opt_nodes[edge.destination];
  FB_FREQ freq_old = edge.freq;

  // Remove edge from source and destination nodes
  remove_ex( node_src.outgoing_edges, ex );
  remove_ex( node_dst.incoming_edges, ex );

  // If ex is not the last edge, rename/move the last edge to replace ex
  IDTYPE ex_last = _fb_opt_edges.size() - 1;
  if ( ex != ex_last ) {
    OPT_FB_EDGE &edge_last = _fb_opt_edges[ex_last];
    replace_ex( _fb_opt_nodes[edge_last.source].outgoing_edges, ex_last, ex );
    replace_ex( _fb_opt_nodes[edge_last.destination].incoming_edges,
		ex_last, ex );
    _fb_opt_edges[ex] = edge_last;
  }

  // Discard last edge
  _fb_opt_edges.pop_back();

  // Update frequency data
  if ( ! freq_old.Exact() ) {
    node_src.unexact_out -= 1;
    node_dst.unexact_in  -= 1;

    if ( ! freq_old.Known() ) {
      node_src.unknown_out -= 1;
      node_dst.unknown_in  -= 1;
    }
  }
  node_src.freq_total_out -= freq_old;
  node_dst.freq_total_in  -= freq_old;
}


// Changes the frequency of an edge in the feedback cfg
void
OPT_FEEDBACK::Change_edge_freq( IDTYPE ex, FB_FREQ freq_new )
{
  if ( _trace )
    fprintf( TFile, "  OPT_FEEDBACK::Change_edge_freq(ex %d)\n", ex );
  VALIDATE_EDGEX( ex, "Change_edge_freq" );

  OPT_FB_EDGE& edge = _fb_opt_edges[ex];
  OPT_FB_NODE& node_src = _fb_opt_nodes[edge.source];
  OPT_FB_NODE& node_dst = _fb_opt_nodes[edge.destination];

  // Update the edge frequency
  FB_FREQ freq_old = edge.freq;
  edge.freq = freq_new;

  // Update node frequency data with freq_old
  if ( ! freq_old.Exact() ) {
    node_src.unexact_out -= 1;
    node_dst.unexact_in  -= 1;

    if ( ! freq_old.Known() ) {
      node_src.unknown_out -= 1;
      node_dst.unknown_in  -= 1;
    }
  }
  node_src.freq_total_out -= freq_old;
  node_dst.freq_total_in  -= freq_old;

  // Update node frequency data with freq_new
  if ( ! freq_new.Exact() ) {
    node_src.unexact_out += 1;
    node_dst.unexact_in  += 1;

    if ( ! freq_new.Known() ) {
      node_src.unknown_out += 1;
      node_dst.unknown_in  += 1;
    }
  }
  node_src.freq_total_out += freq_new;
  node_dst.freq_total_in  += freq_new;
}


// Change the destination of a single edge in the feedback cfg
// Before:  nx_src --ex--> nx_dst_old        nx_dst_new
// After:   nx_src --ex--> nx_dst_new        nx_dst_old
void
OPT_FEEDBACK::Set_edge_dest( IDTYPE ex, IDTYPE nx_dst_new ) {
  if ( _trace )
    fprintf( TFile, "  OPT_FEEDBACK::Set_edge_dest(ex %d, nx_dst %d)\n",
	     ex, nx_dst_new );
  VALIDATE_EDGEX( ex, "Set_edge_dest" );
  VALIDATE_NODEX( nx_dst_new, "Set_edge_dest" );

  OPT_FB_EDGE& edge = _fb_opt_edges[ex];
  IDTYPE nx_dst_old = edge.destination;
  FB_FREQ freq_edge = edge.freq;

  OPT_FB_NODE& node_dst_old = _fb_opt_nodes[nx_dst_old];
  OPT_FB_NODE& node_dst_new = _fb_opt_nodes[nx_dst_new];

  // Update edge destination, and incoming edges for old and new destinations
  edge.destination = nx_dst_new;
  remove_ex( node_dst_old.incoming_edges, ex );
  node_dst_new.incoming_edges.push_back( ex );

  // Update frequencies
  if ( ! freq_edge.Known() ) {
    node_dst_old.unknown_in -= 1;
    node_dst_new.unknown_in += 1;
  }
  if ( ! freq_edge.Exact() ) {
    node_dst_old.unexact_in -= 1;
    node_dst_new.unexact_in += 1;
  }
  node_dst_old.freq_total_in -= freq_edge;
  node_dst_new.freq_total_in += freq_edge;
}


// ====================================================================
// Frequency propagation
// ====================================================================

void
OPT_FEEDBACK::Freq_propagate_edge_in ( OPT_FB_NODE& node,
				       IDTYPE edge_id, FB_FREQ freq )
{
  Change_edge_freq (edge_id, freq);
  node.freq_total_in = node.freq_total_out;
  const OPT_FB_EDGE& edge = _fb_opt_edges[edge_id];
  const OPT_FB_NODE& pred = _fb_opt_nodes[edge.source];
  if ( pred.unknown_out < 2 ) {
    Freq_propagate_node_out( edge.source );
  }
} // OPT_FEEDBACK::Freq_propagate_edge_in


void
OPT_FEEDBACK::Freq_propagate_edge_out (OPT_FB_NODE& node,
				       IDTYPE edge_id, FB_FREQ freq)
{
  Change_edge_freq (edge_id, freq);
  node.freq_total_out = node.freq_total_in;
  const OPT_FB_EDGE& edge = _fb_opt_edges[edge_id];
  const OPT_FB_NODE& succ = _fb_opt_nodes[edge.destination];
  if (succ.unknown_in < 2)
    Freq_propagate_node_in (edge.destination);

} // OPT_FEEDBACK::Freq_propagate_edge_out


// For the given node, compare the sum of the incoming edge frequencies
// with freq_total_in.  Propagate any frequency data that can be computed
// from known values.
void
OPT_FEEDBACK::Freq_propagate_node_in( IDTYPE nx )
{
  OPT_FB_NODE& node = _fb_opt_nodes[nx];

  if ( _trace_prop ) {
    fprintf( TFile, "OPT_FEEDBACK::Freq_propagate_node_in for:\n" );
    node.Print( nx, TFile );
  }

  Is_True( node.unknown_in < 2,
	   ( "OPT_FEEDBACK::Freq_propagate_node_in: unknown_in"
	     "[nx%d](uk%d) > 1", nx, node.unknown_in ) );

  if ( node.in_out_same &&
       node.freq_total_out.Known() && node.unknown_in > 0 ) {

    // Identify unique incoming edge with unknown frequency and
    // obtain total of known frequencies
    INT unknown_count = 0;
    IDTYPE  ex_unknown = IDTYPE_NULL;
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for (OPT_FB_NODE::EDGES_ITER iter (node.incoming_edges.begin ());
	 iter != node.incoming_edges.end (); ++iter) {
      FB_FREQ freq = _fb_opt_edges[*iter].freq;
      if ( freq.Known() ) {
	freq_total += freq;
      } else {
	++unknown_count;
	Is_True( unknown_count <= node.unknown_in,
		 ( "OPT_FEEDBACK::Freq_propagate_node_in[nx%d]"
		   " found multiple unknown incoming freqs", nx ) );
	ex_unknown = *iter;
      }
    }
    Is_True( unknown_count == node.unknown_in,
	     ( "OPT_FEEDBACK::Freq_propagate_node_in[nx%d]"
	       " found no unknown outgoing edge freqs", nx ) );

    if (unknown_count == 1) {
      // Exactly one in edge has unknown frequency; compute it and propagate
      FB_FREQ freq_found = node.freq_total_out - freq_total;
      if ( freq_found.Error() ) return;  // Abort
      Is_True( freq_found.Known(),
	       ( "OPT_FEEDBACK::Freq_propagate_node_in[nx%d]"
		 " subtract yields unknown freq", nx ) );
      Freq_propagate_edge_in (node, ex_unknown, freq_found);

    } else if (freq_total == node.freq_total_out) {
      // more than one unknown edges and they are all zero

      for (OPT_FB_NODE::EDGES_ITER iter (node.incoming_edges.begin ());
	   iter != node.incoming_edges.end (); ++iter) {
	
	if (! _fb_opt_edges[*iter].freq.Known ())
	  Freq_propagate_edge_in (node, *iter, FB_FREQ_ZERO);
      }
    }

  } else if ( node.in_out_same &&
	      node.freq_total_out.Exact() && node.unexact_in > 0 ) {

    // Identify incoming edge(s) with unexact frequency and
    // obtain total of exact frequencies
    INT unexact_count = 0;
    IDTYPE  ex_unexact = IDTYPE_NULL;
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for (OPT_FB_NODE::EDGES_ITER iter (node.incoming_edges.begin ());
	 iter != node.incoming_edges.end (); ++iter) {
      FB_FREQ freq = _fb_opt_edges[*iter].freq;
      if ( freq.Exact() ) {
	freq_total += freq;
      } else {
	++unexact_count;
	Is_True( unexact_count <= node.unexact_in,
		 ( "OPT_FEEDBACK::Freq_propagate_node_in[nx%d]"
		   " found multiple unexact incoming freqs", nx ) );
	ex_unexact = *iter;
      }
    }
    Is_True( unexact_count == node.unexact_in,
	     ( "OPT_FEEDBACK::Freq_propagate_node_in"
	       " found no unexact outgoing edge freqs" ) );

    if (unexact_count == 1) {
      // Exactly one in edge has unexact frequency; compute it and propagate
      FB_FREQ freq_found = node.freq_total_out - freq_total;
      if ( freq_found.Error() ) return;  // Abort
      Is_True( freq_found.Exact(),
	       ( "OPT_FEEDBACK::Freq_propagate_node_in[nx%d]"
		 " subtract yields unexact freq", nx ) );
      Freq_propagate_edge_in (node, ex_unexact, freq_found);
      
    } else if (freq_total == node.freq_total_out) {
      // more than one unexact edges and they are all zero

      for (OPT_FB_NODE::EDGES_ITER iter (node.incoming_edges.begin ());
	   iter != node.incoming_edges.end (); ++iter) {

	if (! _fb_opt_edges[*iter].freq.Exact ())
	  Freq_propagate_edge_in (node, *iter, FB_FREQ_ZERO);
      }
    }
  } else if ( ( node.unknown_in == 0 && ! node.freq_total_in.Known() ) ||
	      ( node.unexact_in == 0 && ! node.freq_total_in.Exact() ) ) {

    // Total incoming frequency
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for (OPT_FB_NODE::EDGES_ITER iter (node.incoming_edges.begin ());
	 iter != node.incoming_edges.end (); ++iter) {
      freq_total += _fb_opt_edges[*iter].freq;
    }
    Is_True( freq_total.Known(),
	     ( "OPT_FEEDBACK::Freq_propagate_node_in[nx%d]"
	       " found an unknown incoming edge freq", nx ) );
    Is_True( freq_total.Exact() || node.unexact_in > 0,
	     ( "OPT_FEEDBACK::Freq_propagate_node_in[nx%d]"
	       " found an unexact incoming edge freq", nx ) );
    node.freq_total_in = freq_total;

    // Try to copy freq_total_in to freq_total_out
    if ( node.in_out_same &&
	 ( node.unknown_out == 1 ||
	   ( freq_total.Exact() && node.unexact_out == 1 ) ) )
	  Freq_propagate_node_out( nx );
  }
}


// For the given node, compare the sum of the outgoing edge frequencies
// with freq_total_out.  Propagate any frequency data that can be computed
// from known values.
void
OPT_FEEDBACK::Freq_propagate_node_out( IDTYPE nx )
{
  OPT_FB_NODE& node = _fb_opt_nodes[nx];

  if ( _trace_prop ) {
    fprintf( TFile, "OPT_FEEDBACK::Freq_propagate_node_out for:\n" );
    node.Print( nx, TFile );
  }

  Is_True( node.unknown_out < 2,
	   ( "OPT_FEEDBACK::Freq_propagate_node_out: unknown_out"
	     "[nx%d](uk%d) > 1", nx, node.unknown_out ) );

  if ( node.in_out_same &&
       node.freq_total_in.Known() && node.unknown_out > 0) {

    // Identify unique outgoing edge with unknown frequency and
    // obtain total of known frequencies
    INT unknown_count = 0;
    IDTYPE  ex_unknown = IDTYPE_NULL;
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for (OPT_FB_NODE::EDGES_ITER iter (node.outgoing_edges.begin ());
	 iter != node.outgoing_edges.end (); ++iter) {
      FB_FREQ freq = _fb_opt_edges[*iter].freq;
      if ( freq.Known() ) {
	freq_total += freq;
      } else {
	++unknown_count;
	Is_True( unknown_count <= node.unknown_out,
		 ( "OPT_FEEDBACK::Freq_propagate_node_out[nx%d]"
		   " found multiple unknown outgoing freqs", nx ) );
	ex_unknown = *iter;
      }
    }
    Is_True( unknown_count == node.unknown_out,
	     ( "OPT_FEEDBACK::Freq_propagate_node_out[nx%d]"
	       " found no unknown incoming edge freqs", nx ) );

    if (unknown_count == 1) {
      // Exactly one out edge has unknown frequency; compute it and propagate
      FB_FREQ freq_found = node.freq_total_in - freq_total;
      if ( freq_found.Error() ) return;  // Abort
      Is_True( freq_found.Known(),
	       ( "OPT_FEEDBACK::Freq_propagate_node_out[nx%d]"
		 " subtract yields unknown freq", nx ) );
      Freq_propagate_edge_out (node, ex_unknown, freq_found);

    } else if (freq_total == node.freq_total_in) {
      // more than one unknown edges and they are all zero
      for (OPT_FB_NODE::EDGES_ITER iter (node.outgoing_edges.begin ());
	   iter != node.outgoing_edges.end (); ++iter) {
	
	if (! _fb_opt_edges[*iter].freq.Known ())
	  Freq_propagate_edge_out (node, *iter, FB_FREQ_ZERO);
      }
    }
  } else if ( node.in_out_same &&
	      node.freq_total_in.Exact() && node.unexact_out > 0 ) {

    // Identify unique outgoing edge with unexact frequency and
    // obtain total of exact frequencies
    INT unexact_count = 0;
    IDTYPE  ex_unexact = IDTYPE_NULL;
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for (OPT_FB_NODE::EDGES_ITER iter (node.outgoing_edges.begin ());
	 iter != node.outgoing_edges.end (); ++iter) {
      FB_FREQ freq = _fb_opt_edges[*iter].freq;
      if ( freq.Exact() ) {
	freq_total += freq;
      } else {
	++unexact_count;
	Is_True( unexact_count <= node.unexact_out,
		 ( "OPT_FEEDBACK::Freq_propagate_node_out[nx%d]"
		   " found multiple unexact outgoing freqs", nx ) );
	ex_unexact = *iter;
      }
    }
    Is_True( unexact_count == node.unexact_out,
	     ( "OPT_FEEDBACK::Freq_propagate_node_out"
	       " found no unexact incoming edge freqs" ) );

    if (unexact_count == 1) {
      // Exactly one out edge has unexact frequency; compute it and propagate
      FB_FREQ freq_found = node.freq_total_in - freq_total;
      if ( freq_found.Error() ) return;  // Abort
      Is_True( freq_found.Exact(),
	       ( "OPT_FEEDBACK::Freq_propagate_node_out[nx%d]"
		 " subtract yields unexact freq", nx ) );
      Freq_propagate_edge_out (node, ex_unexact, freq_found);

    } else if (freq_total == node.freq_total_in) {
      // more than one unknown edges and they are all zero
      for (OPT_FB_NODE::EDGES_ITER iter (node.outgoing_edges.begin ());
	   iter != node.outgoing_edges.end (); ++iter) {
	
	if (! _fb_opt_edges[*iter].freq.Exact ())
	  Freq_propagate_edge_out (node, *iter, FB_FREQ_ZERO);
      }
    }
  } else if ( ( node.unknown_out == 0 && ! node.freq_total_out.Known() ) ||
	      ( node.unexact_out == 0 && ! node.freq_total_out.Exact() ) ) {

    // Total outgoing frequency
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for ( INT t = 0; t < node.outgoing_edges.size(); t++ ) {
      IDTYPE  ex   = node.outgoing_edges[t];
      FB_FREQ freq = _fb_opt_edges[ex].freq;
      freq_total += freq;
    }
    Is_True( freq_total.Known(),
	     ( "OPT_FEEDBACK::Freq_propagate_node_out[nx%d]"
	       " found an unknown outgoing edge freq", nx ) );
    Is_True( freq_total.Exact() || node.unexact_out > 0,
	     ( "OPT_FEEDBACK::Freq_propagate_node_out[nx%d]"
	       " found an unexact outgoing edge freq", nx ) );
    node.freq_total_out = freq_total;
    
    // Try to copy freq_total_out to freq_total_in
    if ( node.in_out_same &&
	 ( node.unknown_in == 1 ||
	   ( freq_total.Exact() && node.unexact_in == 1 ) ) )
      Freq_propagate_node_in( nx );
  }
}

// Propagate edge frequency for the given node.
void
OPT_FEEDBACK::Freq_propagate(IDTYPE nx)
{
  OPT_FB_NODE& node = _fb_opt_nodes[nx];
  if ( node.unknown_in < 2 ) {
    Freq_propagate_node_in( nx );
  }
  if ( node.unknown_out < 2 ) {
    Freq_propagate_node_out( nx );
  }
}

void
OPT_FEEDBACK::Freq_propagate()
{
  for ( IDTYPE nx = _fb_opt_nodes.size() - 1; nx > 0; nx-- ) {
    Freq_propagate(nx);
  }
}

// ====================================================================
// Construction of OPT_FEEDBACK from CFG and Cur_PU_Feedback
// ====================================================================

OPT_FEEDBACK::OPT_FEEDBACK( CFG *cfg, MEM_POOL *pool )
  : _mem_pool( pool ),
    _trace(        Get_Trace(TP_FEEDBACK, TP_OPT_FEEDBACK       ) ),
    _trace_draw(   Get_Trace(TP_FEEDBACK, TP_OPT_FEEDBACK_DRAW  ) ),
    _trace_before( Get_Trace(TP_FEEDBACK, TP_OPT_FEEDBACK_BEFORE) ),
    _trace_prop(   Get_Trace(TP_FEEDBACK, TP_OPT_FEEDBACK_PROP  ) ),
    _fb_opt_nodes( mempool_allocator<OPT_FB_NODE>( pool ) ),
    _fb_opt_edges( mempool_allocator<OPT_FB_EDGE>( pool ) )
{
  // Allocate space for all bb nodes, and guess the number of edges
  _fb_opt_nodes.insert( _fb_opt_nodes.end(), cfg->Last_bb_id() + 1,
			OPT_FB_NODE( _mem_pool ) );
  _fb_opt_edges.reserve( 2 * cfg->Last_bb_id() );
  OPT_FB_EDGE dummy_edge( IDTYPE_NULL, IDTYPE_NULL );
  _fb_opt_edges.push_back( dummy_edge );  // Skip over edge 0
  // If the first Is_True below fails, insert the following code here:
  //   for ( BB_NODE *bb = cfg->First_bb(); bb != NULL; bb = bb->Next() ) {
  //     Add_node( bb->Id() );
  //   }

  for ( BB_NODE *bb = cfg->First_bb(); bb != NULL; bb = bb->Next() ) {

    // Skip fake entry and fake exit
    if ( ! cfg->Removable_bb( bb ) )
      continue;

    // Create an OPT_FB_NODE for bb
    Is_True( bb->Id() < _fb_opt_nodes.size(),
	     ( "OPT_FEEDBACK::OPT_FEEDBACK: Last_bb_id is not largest id" ) );
    OPT_FB_NODE& node = _fb_opt_nodes[bb->Id()];
    // Determine if in_out_same should be false.
    if ( bb->Kind() == BB_ENTRY || bb->Kind() == BB_EXIT )
      node.in_out_same = false;
    else {
      STMT_ITER stmt_iter;
      WN *wn;
      FOR_ALL_ELEM ( wn, stmt_iter, Init( bb->Firststmt(), bb->Laststmt() ) ) {
	if ( WN_operator( wn ) != OPR_IO &&   // IO is special case
	     ! Cur_PU_Feedback->Same_in_out( wn ) ) {
	  node.in_out_same = false;
	  break;
	}
#if defined (TARG_SL) && defined(TARG_SL2)
	else if (WN_operator( wn ) != OPR_IO &&
		 (WN_is_compgoto_for_minor ( wn ) || WN_is_compgoto_para ( wn ))) {
	  node.in_out_same = false;
	  break;
	}
#endif
      }
    }

    // Determine outbound frequency data, namely freq_total_out and
    //   outgoing edge frequencies.
    FB_FREQ freq = FB_FREQ_UNINIT;
    WN *wn_last;
    if ( bb->Kind() == BB_ENTRY )
      wn_last = bb->Entrywn();
    else
      wn_last = bb->Laststmt();

    if ( wn_last == NULL ) {
#if defined(TARG_SL)  && defined(TARG_SL2)
      if(bb->Kind()==BB_REGIONSTART) {
	RID* rid=bb->Regioninfo()->Rid();
	if(rid!=NULL) {
	  WN* rgn=bb->Regioninfo()->Orig_wn();
	  if( rgn!=NULL && WN_operator(rgn)==OPR_REGION) {
	    FB_FREQ in_freq = Cur_PU_Feedback->Query( rgn, FB_EDGE_CALL_INCOMING );
	    if ( bb->Succ() )
	      Add_edge( bb->Id(), bb->Nth_succ(0)->Id(), FB_EDGE_OUTGOING, in_freq);
	  }
	}
      }
      else if (bb->Kind()==BB_REGIONEXIT) {
	RID* rid=bb->Regioninfo()->Rid();
	if(rid!=NULL) {
	  WN* rgn=bb->Regioninfo()->Orig_wn();
	  if( rgn!=NULL && WN_operator(rgn)==OPR_REGION)   {
	    FB_FREQ out_freq = Cur_PU_Feedback->Query( rgn, FB_EDGE_CALL_OUTGOING);
	    if ( bb->Succ() )
	      Add_edge( bb->Id(), bb->Nth_succ(0)->Id(), FB_EDGE_OUTGOING, out_freq);
	  }
	}
      }
      else if (! cfg->Removable_bb( bb->Nth_succ(0) ) )
	Add_edge( bb->Id(), bb->Nth_succ(0)->Id(), FB_EDGE_OUTGOING, FB_FREQ_ZERO );
      else 
#endif
      // Raymond says it's a bug that bb->Succ() could be NULL, but we do 
      // see such a case?!?
      if ( bb->Succ() )
	Add_edge( bb->Id(), bb->Nth_succ(0)->Id(), FB_EDGE_OUTGOING,
		  FB_FREQ_UNINIT ); 
    } else {

      OPERATOR opr = WN_operator( wn_last );

#ifdef KEY
      node.orig_wn = NULL;

      if( opr == OPR_ICALL ){
	node.orig_wn = wn_last;

      } else if( !cfg->Calls_break() ){
	STMT_ITER stmt_iter;
	WN* wn = NULL;
	int num_icalls = 0;
	FOR_ALL_ELEM ( wn, stmt_iter, Init( bb->Firststmt(), bb->Laststmt() ) ) {
	  if( WN_operator( wn ) == OPR_ICALL ){
	    node.orig_wn = wn;
	    num_icalls++;
	  }
	}

      }
#endif

      switch ( opr ) {

      case OPR_PRAGMA:
	if ( WN_pragma( wn_last ) != WN_PRAGMA_PREAMBLE_END ) {
	  Add_edge( bb->Id(), bb->Nth_succ(0)->Id(),
		    FB_EDGE_OUTGOING, FB_FREQ_UNINIT );
	  break;
	}
	// else fall through

      case OPR_GOTO:
      case OPR_FUNC_ENTRY:
      case OPR_ALTENTRY:
	freq = Cur_PU_Feedback->Query( wn_last, FB_EDGE_OUTGOING );
	Add_edge( bb->Id(), bb->Nth_succ(0)->Id(), FB_EDGE_OUTGOING, freq );
	break;

      case OPR_RETURN:
      case OPR_RETURN_VAL:
#ifdef KEY
      case OPR_GOTO_OUTER_BLOCK:
#endif
	break;

      case OPR_TRUEBR:
      case OPR_FALSEBR:

	if ( ! cfg->Lower_fully() ) {  // PREOPT

	  if ( bb->Kind() == BB_REPEATEND ) {

	    // get WN for loop
	    WN *wn_loop = bb->Loop()->Orig_wn();

	    IDTYPE nx_bb    = bb->Id();
	    IDTYPE nx_merge = bb->Loop()->Merge()->Id();
	    IDTYPE nx_body  = bb->Loop()->Body()->Id();

	    FB_FREQ freq_out =
	      Cur_PU_Feedback->Query( wn_loop, FB_EDGE_LOOP_OUT );
	    FB_FREQ freq_back =
	      Cur_PU_Feedback->Query( wn_loop, FB_EDGE_LOOP_BACK );

	    Add_edge( nx_bb, nx_merge, FB_EDGE_LOOP_OUT,  freq_out  );
	    Add_edge( nx_bb, nx_body,  FB_EDGE_LOOP_BACK, freq_back );
	    break;
	  }

	  if ( bb->Kind() == BB_DOEND || bb->Kind() == BB_WHILEEND ) {

	    // get WN for loop
	    WN *wn_loop = bb->Loop()->Orig_wn();

	    FB_FREQ freq_zero =
	      Cur_PU_Feedback->Query( wn_loop, FB_EDGE_LOOP_ZERO );
	    FB_FREQ freq_positive =
	      Cur_PU_Feedback->Query( wn_loop, FB_EDGE_LOOP_POSITIVE );
	    FB_FREQ freq_out =
	      Cur_PU_Feedback->Query( wn_loop, FB_EDGE_LOOP_OUT );
	    FB_FREQ freq_back =
	      Cur_PU_Feedback->Query( wn_loop, FB_EDGE_LOOP_BACK );
	    FB_FREQ freq_exit =
	      Cur_PU_Feedback->Query( wn_loop, FB_EDGE_LOOP_EXIT );
	    FB_FREQ freq_iterate =
	      Cur_PU_Feedback->Query( wn_loop, FB_EDGE_LOOP_ITERATE );

	    IDTYPE nx_bb    = bb->Id();
	    IDTYPE nx_merge = bb->Loop()->Merge()->Id();
	    IDTYPE nx_body  = bb->Loop()->Body()->Id();

	    if ( freq_zero.Known() || freq_out.Known() ) {
	      Add_edge( nx_bb, nx_merge, FB_EDGE_LOOP_ZERO, freq_zero );
	      Add_edge( nx_bb, nx_merge, FB_EDGE_LOOP_OUT,  freq_out );
	    } else {
	      Add_edge( nx_bb, nx_merge, FB_EDGE_LOOP_EXIT, freq_exit );
	    }

	    if ( freq_positive.Known() || freq_back.Known() ) {
	      Add_edge( nx_bb, nx_body, FB_EDGE_LOOP_POSITIVE, freq_positive );
	      Add_edge( nx_bb, nx_body, FB_EDGE_LOOP_BACK,     freq_back );
	    } else {
	      Add_edge( nx_bb, nx_body, FB_EDGE_LOOP_ITERATE,  freq_iterate );
	    }

	    break;
	  }
	}

	{ // not a BB_DOEND, BB_WHILEEND, or BB_REPEATEND during PREOPT
	  
	  FB_FREQ freq_taken =
	    Cur_PU_Feedback->Query( wn_last, FB_EDGE_BRANCH_TAKEN );
	  FB_FREQ freq_not_taken =
	    Cur_PU_Feedback->Query( wn_last, FB_EDGE_BRANCH_NOT_TAKEN );
	  INT32    label_num = WN_label_number(wn_last);
	  BB_NODE *bb_branch = cfg->Get_bb_from_label(label_num);
	  Add_edge( bb->Id(), bb->Next()->Id(),
		    FB_EDGE_BRANCH_NOT_TAKEN, freq_not_taken );
	  Add_edge( bb->Id(), bb_branch->Id(),
		    FB_EDGE_BRANCH_TAKEN,     freq_taken );
	}
	break;

      case OPR_REGION:
#if defined (TARG_SL) && defined(TARG_SL2) 
	{
	  BB_LIST* succs = bb->Succ();
	  INT succ_count=0;
	  while(succs!=NULL) {
	    succ_count++;
	    BB_NODE* succbb=succs->Node();
	    if(succbb->Kind() == BB_EXIT)
	      //according to the cfg building process, the successor bb of BB_EXIT KIND
	      // after a region will be used to prevent SSAPRE to do something 
	      Add_edge ( bb->Id(), succbb->Id(), FB_EDGE_OUTGOING, FB_FREQ_ZERO);
	    else  {
	      FB_FREQ freq_out = Cur_PU_Feedback->Query( wn_last, FB_EDGE_CALL_OUTGOING );	
	      Add_edge ( bb->Id(), succbb->Id(), FB_EDGE_CALL_OUTGOING, FB_FREQ_UNINIT);
	    }
	    succs=succs->Next();
	  }
	}
	break;
      case OPR_REGION_EXIT:
	{
	  RID* rid=bb->Regioninfo()->Rid();
	  if(rid!=NULL) {	
	    WN* rgn=bb->Regioninfo()->Orig_wn();
	    if( rgn!=NULL && WN_operator(rgn)==OPR_REGION )   {
	      FB_FREQ freq_out = Cur_PU_Feedback->Query( rgn, FB_EDGE_CALL_OUTGOING );
	      if(bb->Succ()) {
		Add_edge( bb->Id(), bb->Nth_succ(0)->Id(), FB_EDGE_OUTGOING,freq_out);
	      }
	    }
	  }	  
      	}
	break;		
#endif
      case OPR_PICCALL:
      case OPR_CALL:
      case OPR_ICALL:
      case OPR_INTRINSIC_CALL:
	freq = Cur_PU_Feedback->Query( wn_last, FB_EDGE_CALL_OUTGOING );
	if ( bb->Succ() != NULL ) {   // STOP in f77 has Succ() == NULL
	  Add_edge( bb->Id(), bb->Nth_succ(0)->Id(), FB_EDGE_OUTGOING, freq );
	}
	break;

      case OPR_IO:
	{
	  freq = Cur_PU_Feedback->Query( wn_last, FB_EDGE_CALL_OUTGOING );

	  if ( bb->Kind() != BB_IO ) {

	    // IO does not affect control flow
	    Add_edge( bb->Id(), bb->Nth_succ(0)->Id(),
		      FB_EDGE_IO_OUTGOING, freq );

	  } else {  // IO affects control flow

	    // Add primary outgoing edge
	    BB_NODE *next = bb->Next();
	    Add_edge( bb->Id(), next->Id(), FB_EDGE_IO_OUTGOING, freq );

	    // May have from 0 to 3 escape edges
	    freq = FB_FREQ_UNKNOWN;
	    if ( Cur_PU_Feedback->Same_in_out( wn_last ) )
	      freq = FB_FREQ_ZERO;

	    BB_LIST_ITER bb_iter;
	    BB_NODE *succ;
	    INT32 branch = 0;
	    FOR_ALL_ELEM( succ, bb_iter, Init( bb->Succ() ) ) {
	      if ( succ == next ) continue;

	      Add_edge( bb->Id(), succ->Id(),
			FB_EDGE_IO_ESCAPE( branch ), freq );
	      ++branch;
	    }
	  }
	}
	break;

      case OPR_COMPGOTO:
      case OPR_XGOTO:
      case OPR_SWITCH:
	{
	  for ( INT32 branch = 0; branch < bb->Switchentries(); ++branch ) {
	    freq = Cur_PU_Feedback->Query( wn_last, FB_EDGE_SWITCH(branch) );
	    Add_edge( bb->Id(), bb->Switchcase(branch)->Id(),
		      FB_EDGE_SWITCH(branch), freq );
	  }
	  // Handle default branch
	  if ( bb->Switchdefault() != NULL ) {
	    freq = Cur_PU_Feedback->Query( wn_last, FB_EDGE_SWITCH_DEFAULT );
	    Add_edge( bb->Id(), bb->Switchdefault()->Id(),
		      FB_EDGE_SWITCH_DEFAULT, freq );
	  }
	}
	break;

      default:
#if defined (TARG_SL)	
	if ( bb->Succ() ) 
#endif	  	
	Add_edge( bb->Id(), bb->Nth_succ(0)->Id(),
		  FB_EDGE_OUTGOING, FB_FREQ_UNINIT );
	break;
      }
    }
  }

  // Display trace data
  if ( _trace_before && _trace ) {
    fprintf( TFile, "Before initial frequency propagation:\n" );
    Print( TFile );
  }

  // Propagate frequency values in cfg
  Freq_propagate();

  // Display trace data
  if ( _trace ) {
    fprintf( TFile, "After initial frequency propagation:\n" );
    Print( TFile );
  }
}

OPT_FEEDBACK::~OPT_FEEDBACK()
{
  if ( _trace ) Print( TFile );
}

// ====================================================================
// Retrieve feedback data from OPT_FEEDBACK for emitted whirl nodes
// ====================================================================

void
OPT_FEEDBACK::Emit_feedback( WN *wn, BB_NODE *bb ) const
{
  IDTYPE nx = bb->Id();
  const OPT_FB_NODE& node = _fb_opt_nodes[nx];
  OPERATOR opr = WN_operator( wn );

  if ( _trace ) {
    fprintf( TFile, "OPT_FEEDBACK::Emit_feedback bb->Id() = %3d, opr = %s:\n",
	     nx, OPERATOR_name( opr ) );
  }

  switch ( opr ) {

  case OPR_LABEL:
    Cur_PU_Feedback->Annot( wn, FB_EDGE_INCOMING, node.freq_total_in );
    break;

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) == WN_PRAGMA_PREAMBLE_END )
      Cur_PU_Feedback->Annot( wn, FB_EDGE_OUTGOING, node.freq_total_in );
    break;

  case OPR_GOTO:
    Cur_PU_Feedback->Annot( wn, FB_EDGE_OUTGOING, node.freq_total_out );
    break;

  case OPR_FUNC_ENTRY:
  case OPR_ALTENTRY:
    Cur_PU_Feedback->Annot( wn, FB_EDGE_OUTGOING, node.freq_total_out );
    break;

  case OPR_RETURN:
  case OPR_RETURN_VAL:
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
    {
      // Use node.freq_total_in as a guess
      FB_FREQ freq_guess = node.freq_total_in;
      if ( freq_guess.Exact() )
	freq_guess = FB_FREQ( freq_guess.Value(), false );
      Cur_PU_Feedback->Annot( wn, FB_EDGE_INCOMING, freq_guess );
    }
    break;

  case OPR_TRUEBR:
  case OPR_FALSEBR:
    {
      FB_FREQ freq_not = Get_edge_freq( nx, bb->Next()->Id() );
      FB_FREQ freq_tkn = _fb_opt_nodes[nx].freq_total_out - freq_not;

      // Old Code to do the same thing:
      //
      // IDTYPE ex_tkn    = Find_edge_by_type( nx, FB_EDGE_BRANCH_TAKEN     );
      // IDTYPE ex_not    = Find_edge_by_type( nx, FB_EDGE_BRANCH_NOT_TAKEN );
      // if ( ex_tkn == IDTYPE_NULL || ex_not == IDTYPE_NULL ) {
      //   IDTYPE ex_tkn    = Find_edge_by_type( nx, FB_EDGE_LOOP_BACK );
      //   IDTYPE ex_not    = Find_edge_by_type( nx, FB_EDGE_LOOP_OUT  );
      //   DevWarn( "OPT_FEEDBACK::Emit_feedback(TRUEBR/FALSEBR) edge missing" );
      // }
      // FB_FREQ freq_tkn = _fb_opt_edges[ex_tkn].freq;
      // FB_FREQ freq_not = _fb_opt_edges[ex_not].freq;

      Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_TAKEN,     freq_tkn );
      Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_NOT_TAKEN, freq_not );
    }
    break;

  case OPR_IF:
    {
      BB_NODE *bb_then = bb->If_then();
      BB_NODE *bb_else = bb->If_else();
      if ( bb_then == bb_else ) {
	DevWarn( "OPT_FEEDBACK::Emit_feedback(IF) identical targets" );
      }
      FB_FREQ freq_then = Get_edge_freq( nx, bb_then->Id() );
      FB_FREQ freq_else = Get_edge_freq( nx, bb_else->Id() );
      Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_TAKEN,     freq_then );
      Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_NOT_TAKEN, freq_else );
    }
    break;

  case OPR_CSELECT:
    Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_TAKEN,     FB_FREQ_UNKNOWN );
    Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_NOT_TAKEN, FB_FREQ_UNKNOWN );
    break;

  case OPR_DO_LOOP:
  case OPR_WHILE_DO:
    {
      // bb->Loopend()->Id() == nx
      IDTYPE ex_zero   = Find_edge_by_type( nx, FB_EDGE_LOOP_ZERO     );
      IDTYPE ex_pos    = Find_edge_by_type( nx, FB_EDGE_LOOP_POSITIVE );
      IDTYPE ex_out    = Find_edge_by_type( nx, FB_EDGE_LOOP_OUT      );
      IDTYPE ex_back   = Find_edge_by_type( nx, FB_EDGE_LOOP_BACK     );

      if ( ex_zero > IDTYPE_NULL && ex_pos  > IDTYPE_NULL &&
	   ex_out  > IDTYPE_NULL && ex_back > IDTYPE_NULL ) {

	FB_FREQ freq_zero = _fb_opt_edges[ex_zero].freq;
	FB_FREQ freq_pos  = _fb_opt_edges[ex_pos ].freq;
	FB_FREQ freq_out  = _fb_opt_edges[ex_out ].freq;
	FB_FREQ freq_back = _fb_opt_edges[ex_back].freq;
	Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_ZERO,     freq_zero );
	Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_POSITIVE, freq_pos  );
	Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_OUT,      freq_out  );
	Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_BACK,     freq_back );

      } else {

	BB_NODE *bb_body = bb->Loopbody();
	FB_FREQ freq_iterate = Get_edge_freq( nx, bb_body->Id() );
	FB_FREQ freq_exit    = _fb_opt_nodes[nx].freq_total_out - freq_iterate;
	Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_EXIT,     freq_exit    );
	Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_ITERATE,  freq_iterate );
      }
    }
    break;

  case OPR_DO_WHILE:
    {
      // bb->Loopend()->Id() == nx
      IDTYPE ex_back   = Find_edge_by_type( nx, FB_EDGE_LOOP_BACK );
      IDTYPE ex_out    = Find_edge_by_type( nx, FB_EDGE_LOOP_OUT  );
      if ( ex_back == IDTYPE_NULL || ex_out == IDTYPE_NULL ) {
	DevWarn("OPT_FEEDBACK::Emit_feedback(DO_WHILE) edge missing" );
      }
      FB_FREQ freq_back = _fb_opt_edges[ex_back].freq;
      FB_FREQ freq_out  = _fb_opt_edges[ex_out ].freq;
      Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_ZERO,     FB_FREQ_ZERO    );
      Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_POSITIVE, FB_FREQ_UNKNOWN );
      Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_OUT,      freq_out        );
      Cur_PU_Feedback->Annot( wn, FB_EDGE_LOOP_BACK,     freq_back       );
    }
    break;

  case OPR_CAND:
  case OPR_CIOR:
    Cur_PU_Feedback->Annot( wn, FB_EDGE_CIRCUIT_LEFT,    FB_FREQ_UNKNOWN );
    Cur_PU_Feedback->Annot( wn, FB_EDGE_CIRCUIT_RIGHT,   FB_FREQ_UNKNOWN );
    Cur_PU_Feedback->Annot( wn, FB_EDGE_CIRCUIT_NEITHER, FB_FREQ_UNKNOWN );
    break;

  case OPR_REGION:
  case OPR_PICCALL:
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
    if ( node.in_out_same )
      Cur_PU_Feedback->Annot( wn, FB_EDGE_CALL_INOUTSAME, node.freq_total_in );
    else {
      // Use node.freq_total_in and node.freq_total_out as guesses
      FB_FREQ freq_guess_in  = node.freq_total_in;
      FB_FREQ freq_guess_out = node.freq_total_out;
      if ( bb->Kind() == BB_EXIT )
	freq_guess_out = freq_guess_in;
      if ( freq_guess_in.Exact() )
	freq_guess_in  = FB_FREQ( freq_guess_in.Value(),  false );
      if ( freq_guess_out.Exact() )
	freq_guess_out = FB_FREQ( freq_guess_out.Value(), false );

      Cur_PU_Feedback->Annot( wn, FB_EDGE_CALL_INCOMING, freq_guess_in  );
      Cur_PU_Feedback->Annot( wn, FB_EDGE_CALL_OUTGOING, freq_guess_out );
    // Would like to do: (requires changes to optimizer cfg)
    // Cur_PU_Feedback->Annot(wn, FB_EDGE_CALL_OUTGOING, node.freq_total_in );
    // Cur_PU_Feedback->Annot(wn, FB_EDGE_CALL_INCOMING, node.freq_total_out);
    }
#ifdef KEY
    if( opr == OPR_ICALL &&
	node.orig_wn != NULL ){
      FB_Info_Icall fb_info_icall = Cur_PU_Feedback->Query_icall(node.orig_wn);
      Cur_PU_Feedback->Annot_icall( wn, fb_info_icall );

      if( !fb_info_icall.Is_uninit() ){
	FmtAssert( fb_info_icall.tnv._exec_counter >= fb_info_icall.tnv._counters[0],
		   ("icall execution counter is invalid") );

	/* We cannot do the following checking, because the representation of
	   _exec_counter is UINT64, and node.freq_total_in is float.
	   TODO:
	   Modify FB_TNV by using FB_FREQ!!! And perform this same checking
	   at Convert_Feedback_Info().

	   const UINT64 exec_counter = (UINT64)ceilf( node.freq_total_in.Value() );
	   FmtAssert( fb_info_icall.tnv._exec_counter == exec_counter,
	   ("icall counter is not updated") );
	*/
      }
    }
#endif // KEY
    break;

  case OPR_IO:
    {
      IDTYPE ex_outgoing = Find_edge_by_type( nx, FB_EDGE_IO_OUTGOING );
      if ( ex_outgoing == IDTYPE_NULL ) {

	// IO has no escape edges, but is not at end of block
	FB_FREQ freq_io = node.freq_total_in;
	if ( ! node.in_out_same && freq_io.Exact() )
	  freq_io = FB_FREQ( freq_io.Value(), false );
	Cur_PU_Feedback->Annot( wn, FB_EDGE_CALL_INOUTSAME, freq_io );

      } else {

	// IO frequencies are stored in the block's outgoing edges
	FB_FREQ freq_outgoing = _fb_opt_edges[ex_outgoing].freq;

	// Find the total frequency of the escape edges
	FB_FREQ freq_escape = FB_FREQ_ZERO;
	for ( INT32 br = 0; br < FB_IO_ESCAPE_EDGES_MAX; ++br ) {

	  IDTYPE ex_escape = Find_edge_by_type( nx, FB_EDGE_IO_ESCAPE(br) );
	  if ( ex_escape == IDTYPE_NULL )
	    break;
	  freq_escape += _fb_opt_edges[ex_escape].freq;
	}

	if ( freq_escape.Exact() && freq_escape.Zero() )
	  Cur_PU_Feedback->Annot( wn, FB_EDGE_CALL_INOUTSAME, freq_outgoing );
	else {
	  FB_FREQ freq_incoming = freq_outgoing + freq_escape;
	  Cur_PU_Feedback->Annot( wn, FB_EDGE_CALL_INCOMING, freq_outgoing );
	  Cur_PU_Feedback->Annot( wn, FB_EDGE_CALL_OUTGOING, freq_outgoing );
	}
      }
    }
    break;

  case OPR_COMPGOTO:
  case OPR_XGOTO:
  case OPR_SWITCH:
    {
      for ( INT t = 0; t < node.outgoing_edges.size(); t++ ) {
	IDTYPE ex = node.outgoing_edges[t];
	const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
	Cur_PU_Feedback->Annot( wn, edge.edge_type, edge.freq );
      }
    }
    break;

  default:
    break;
  }
}


// ====================================================================
// Update feedback when cfg is modified (public interface)
//   May invalidate frequency balance!
// ====================================================================


// Delete_edge deletes all edges (nx_src ===> nx_dst)
void
OPT_FEEDBACK::Delete_edge( IDTYPE nx_src, IDTYPE nx_dst ) {
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Delete_edge(%d --> %d)\n",
	     nx_src, nx_dst );
  VALIDATE_EDGE( nx_src, nx_dst, "Delete_edge" );

  bool match = false;
  const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    if ( _fb_opt_edges[ex].destination == nx_dst ) {
      match = true;
      Remove_edge( ex ); // ok because t is decreasing
    }
  }
  if (! match)
    DevWarn ("OPT_FEEDBACK::Delete_edge: edge (%d --> %d) not found",
	     nx_src, nx_dst);
}


// Before:  nx_src ===> nx_dst_old        nx_dst_new
// After:   nx_src ===> nx_dst_new        nx_dst_old
void
OPT_FEEDBACK::Move_edge_dest( IDTYPE nx_src,
			      IDTYPE nx_dst_old, IDTYPE nx_dst_new )
{
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Move_edge_dest(%d --> %d, %d)\n",
	     nx_src, nx_dst_old, nx_dst_new );
  VALIDATE_EDGE( nx_src, nx_dst_old, "Move_edge_dest" );
  VALIDATE_NODEX( nx_dst_new, "Move_edge_dest" );

  // Move destination of edges
  bool match = false;
  const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    if ( _fb_opt_edges[ex].destination == nx_dst_old ) {
      match = true;
     Set_edge_dest( ex, nx_dst_new );
    }
  }
  Is_True( match, ( "OPT_FEEDBACK::Move_edge_dest: edge (%d --> %d) not found",
		    nx_src, nx_dst_old ) );
}


void
OPT_FEEDBACK::Move_incoming_edges_dest( IDTYPE nx_dst_old, IDTYPE nx_dst_new )
{
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Move_incoming_edges_dest(%d, %d)\n",
	     nx_dst_old, nx_dst_new );
  VALIDATE_NODEX( nx_dst_old, "Move_incoming_edges_dest" );
  VALIDATE_NODEX( nx_dst_new, "Move_incoming_edges_dest" );

  // Move destination of all incoming edges
  const OPT_FB_NODE& node = _fb_opt_nodes[nx_dst_old];
  for ( INT t = node.incoming_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.incoming_edges[t];
    Set_edge_dest( ex, nx_dst_new );
  }
}


// Merge all outgoing edges:
//   nx_src ===> <various>  becomes  nx_src ---> nx_dst_new
void
OPT_FEEDBACK::Merge_outgoing_edges( IDTYPE nx_src, IDTYPE nx_dst_new )
{
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Merge_outgoing_paths(%d, %d)\n",
	     nx_src, nx_dst_new );
  VALIDATE_NODEX( nx_src,     "Merge_outgoing_edges" );
  VALIDATE_NODEX( nx_dst_new, "Merge_outgoing_edges" );

  // Save the current total outgoing frequency
  const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
  FB_FREQ freq_outgoing = node.freq_total_out;

  // Delete all outgoing edges
  for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node.outgoing_edges[t];
    Remove_edge( ex ); // ok because t is decreasing
  }

  // Add a single new outgoing edge
  Add_edge( nx_src, nx_dst_new, FB_EDGE_OUTGOING, freq_outgoing );
}


// Delete_unreached_node removes nx and all incoming and outgoing edges
void
OPT_FEEDBACK::Delete_node( IDTYPE nx ) {
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Delete_node(%d)\n", nx );
  VALIDATE_NODEX( nx, "Delete_node" );

  const OPT_FB_NODE& node = _fb_opt_nodes[nx];

  // Delete all incoming edges
  for ( INT t1 = node.incoming_edges.size() - 1; t1 >= 0; --t1 ) {
    IDTYPE ex = node.incoming_edges[t1];
    Remove_edge( ex ); // ok because t1 is decreasing
  }

  // Delete all outgoing edges
  for ( INT t2 = node.outgoing_edges.size() - 1; t2 >= 0; --t2 ) {
    IDTYPE ex = node.outgoing_edges[t2];
    Remove_edge( ex ); // ok because t2 is decreasing
  }
}


// Before:  nx_src ===> nx_dst
// After:   nx_src ===> nx_mid ---> nx_dst
void
OPT_FEEDBACK::Split_edge( IDTYPE nx_src, IDTYPE nx_mid, IDTYPE nx_dst )
{
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Split_edge(%d --> %d --> %d)\n",
	     nx_src, nx_mid, nx_dst );
  VALIDATE_EDGE( nx_src, nx_dst, "Split_edge" );
  VALIDATE_NODEX( nx_mid, "Split_edge" );

  // Create middle node
  Add_node( nx_mid );

  // Move destination of edges
  Move_edge_dest( nx_src, nx_dst, nx_mid );

  // Add edge nx_mid ---> nx_dst
  Add_edge( nx_mid, nx_dst, FB_EDGE_OUTGOING,
	    _fb_opt_nodes[nx_mid].freq_total_in );
}


// Before:  various ===>             nx_old ===> various
// After:   various ===> nx_new ---> nx_old ===> various
void
OPT_FEEDBACK::Split_node( IDTYPE nx_old, IDTYPE nx_new )
{
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Split_node(nx_old %d, nx_new %d)\n",
	     nx_old, nx_new );
  VALIDATE_NODEX( nx_old, "Split_node" );
  VALIDATE_NODEX( nx_new, "Split_node" );

  Add_node( nx_new );
  OPT_FB_NODE &node_old = _fb_opt_nodes[nx_old];
  OPT_FB_NODE &node_new = _fb_opt_nodes[nx_new];

  // Update incoming edge destinations
  for ( INT t = node_old.incoming_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node_old.incoming_edges[t];
    _fb_opt_edges[ex].destination = nx_new;
  }

  // Update both nodes
  node_new.incoming_edges.swap(node_old.incoming_edges);
  node_new.freq_total_in   = node_old.freq_total_in;
  node_old.freq_total_in   = FB_FREQ_ZERO;
  node_new.unknown_in = node_old.unknown_in;
  node_old.unknown_in = 0;
  node_new.unexact_in = node_old.unexact_in;
  node_old.unexact_in = 0;

  // Add edge:  nx_new ---> nx_old
  Add_edge( nx_new, nx_old, FB_EDGE_OUTGOING, node_new.freq_total_in );
}


// Clones all edges nx_src_old ===> nx_dst_old
void
OPT_FEEDBACK::Clone_edge( IDTYPE nx_src_old, IDTYPE nx_dst_old,
			  IDTYPE nx_src_new, IDTYPE nx_dst_new,
			  float scale )
{
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Clone_edge(%d --> %d, %d --> %d)\n",
	     nx_src_old, nx_dst_old, nx_src_new, nx_dst_new );
  VALIDATE_EDGE( nx_src_old, nx_dst_old, "Clone_edge" );
  VALIDATE_EDGE( nx_src_new, nx_dst_new, "Clone_edge" );
  Is_True( scale >= 0.0 && scale <= 1.0,
           ( "OPT_FEEDBACK::Clone_edge: scale == %f", scale ) );

  bool match = false;
  OPT_FB_NODE& node_src = _fb_opt_nodes[nx_src_old];
  OPT_FB_NODE& node_dst = _fb_opt_nodes[nx_dst_old];
  for ( INT t = node_src.outgoing_edges.size() - 1; t >= 0; t-- ) {
    IDTYPE ex = node_src.outgoing_edges[t];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    if ( edge.destination == nx_dst_old ) {
      match = true;
      OPT_FB_EDGE& edge = _fb_opt_edges[ex];

      // Modify original edge, and add the clone edge
      FB_FREQ freq_clone = edge.freq * scale;
      FB_FREQ freq_origl = edge.freq - freq_clone;
      Change_edge_freq( ex, freq_origl );
      Add_edge( nx_src_new, nx_dst_new, edge.edge_type, freq_clone );
    }
  }
  Is_True( match, ( "OPT_FEEDBACK::Clone_edge: edge (%d --> %d) not found",
		    nx_src_old, nx_dst_old ) );
}


//  Given a list of vertices sorted in topological order, sort the edges
//  based on the order.  The constructor builds a mapping from vertex to
//  its position in the topological sorted list.
//    
struct compare_edge_topological_order {
  map<vertex_id, int> topo_pos;  // positions of vertices in topological ordered vector
  bool operator()(const edge& x, const edge& y) {
    if ( topo_pos[x.first] < topo_pos[y.first] ) return true;
    if ( topo_pos[x.first] == topo_pos[y.first] &&
	 topo_pos[x.second] < topo_pos[y.second] ) return true;
    return false;
  }
  compare_edge_topological_order( compare_edge_topological_order &c ):
    topo_pos( c.topo_pos ) {}

  template <class Container>
  compare_edge_topological_order( Container& topo_order ) {
    int pos = 0;
    for ( typename Container::iterator iv = topo_order.begin();
	  iv != topo_order.end();
	  ++iv ) {
      vertex_id v = ( *iv );
      topo_pos[v] = pos++;
    }
  }
};


//  Adjust feedback for CFG after cloning zone z.
//    Clones all of the nodes and edges in zone z;
//    Leaves the entry and side_entry edges untouched.
//  This is not a general routine.  It will handle zones with certain
//  characteristics.
//    1. Acyclic zone:  zone that contains no cycles.
//    2. Butterfly zone:  zone produces butterfly transformation. After
//       removing the backedge in a butterfly zone, the zone becomes acyclic.
//    3. (not implemented yet) 
//       Replicated zone: zone might contain arbritary cycles, but the cloned
//       zone completely duplicates that in the original CFG.
void
OPT_FEEDBACK::Clone_zone( zone&                      z,
			  map<vertex_id, vertex_id>& nx_old_to_new )
{
  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Clone_zone\n" );

  // Sort the clone verticies in topological order
  vector<vertex_id> topo_order;
  edge e = *( z.entry.begin() );
  topological_sort( z.clone, e.second, topo_order );

  if ( _trace ) {
    fprintf( TFile, "  topo order for zone: " );
    for ( INT i = 0; i < topo_order.size(); ++i )
      fprintf( TFile, "%d ", topo_order[i] );
    fprintf( TFile, "\n" );
  }

  // topo-sort the edges
  compare_edge_topological_order cmp( topo_order );
  vector<edge> edge_in_topo_order = z.clone;
  sort( edge_in_topo_order.begin(), edge_in_topo_order.end(), cmp );

  // Find all edges to be cloned, sorted in topological order
  vector<IDTYPE> edges_clone;
  INT i;
  for ( i = 0; i < edge_in_topo_order.size(); ++i ) {

    // Find all edges edges_clone[i].first ===> edges_clone[i].second
    // This could be implemented more efficiently
    const OPT_FB_NODE& node = _fb_opt_nodes[ edge_in_topo_order[i].first ];
    vertex_id second = edge_in_topo_order[i].second;
    for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
      IDTYPE ex = node.outgoing_edges[t];
      if ( _fb_opt_edges[ex].destination == second )
	edges_clone.push_back( ex );
    }
  }

  // Find all exit edges
  vector<IDTYPE> edges_exit;
  for ( zone::iterator ie = z.exit.begin(); ie != z.exit.end(); ++ie ) {

    // Find all edges (*ie).first ===> (*ie).second
    // This could be implemented more efficiently
    const OPT_FB_NODE& node = _fb_opt_nodes[ (*ie).first ];
    vertex_id second = (*ie).second;
    for ( INT t = node.outgoing_edges.size() - 1; t >= 0; t-- ) {
      IDTYPE ex = node.outgoing_edges[t];
      const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
      if ( edge.destination == second )
	edges_exit.push_back( ex );
    }
  }


  // The domain of nx_old_to_new is the set of all nodes to be cloned;
  //   the image of each node is its new clone.
  // freq_transfer_node[nx] is the frequency to be transfered from the
  //   node nx to its clone.
  // freq_transfer_edge[ex] is the frequency to be transfered from the
  //   edge ex to its clone.

  map<IDTYPE, FB_FREQ> freq_transfer_node;
  map<IDTYPE, FB_FREQ> freq_transfer_edge;

  // Initialize freq_transfer_node[] to zero
  for ( i = 0; i < edges_clone.size(); ++i ) {
    IDTYPE ex = edges_clone[i];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    freq_transfer_node[ edge.source      ] = FB_FREQ_ZERO;
    freq_transfer_node[ edge.destination ] = FB_FREQ_ZERO;
  }
  for ( i = 0; i < edges_exit.size(); ++i ) {
    IDTYPE ex = edges_exit[i];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    freq_transfer_node[ edge.source      ] = FB_FREQ_ZERO;
  }

  // Initialize known node incoming transfer frequencies
  if ( z.loop_butterfly ) {
    vertex_id header = z.loop_butterfly;
    freq_transfer_node[header] = Get_node_freq_out( header );
  } else {
    for ( zone::iterator ie = z.entry.begin(); ie != z.entry.end(); ++ie ) {
      freq_transfer_node[(*ie).second] +=
	Get_edge_freq( (*ie).first, (*ie).second );
    }
  }

  // Iterate through all clone edges
  for ( i = 0; i < edges_clone.size(); ++i ) {
    IDTYPE ex = edges_clone[i];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    IDTYPE nx_src = edge.source;
    IDTYPE nx_dst = edge.destination;
    OPT_FB_NODE& node = _fb_opt_nodes[nx_src];

    // Determine the edge's transfer frequency
    FB_FREQ scale = freq_transfer_node[nx_src] / node.freq_total_in;
    FB_FREQ freq_transfer = edge.freq * scale;
    freq_transfer_edge[ex] = freq_transfer;

    // Update the destination node's incoming transfer frequency,
    // if this is a back edge to the loop head, we should not propagate the 
    // frequence because it has already been counted.
    if (! (z.loop_butterfly && z.loop_butterfly == nx_dst))
	freq_transfer_node[nx_dst] += freq_transfer;
  }

  // Iterate through all exit edges
  for ( i = 0; i < edges_exit.size(); ++i ) {
    IDTYPE ex = edges_exit[i];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    IDTYPE nx_src = edge.source;
    OPT_FB_NODE& node = _fb_opt_nodes[nx_src];

    // Determine the edge's transfer frequency
    FB_FREQ scale = freq_transfer_node[nx_src] / node.freq_total_in;
    FB_FREQ freq_transfer = edge.freq * scale;
    freq_transfer_edge[ex] = freq_transfer;
  }

  // Display transfer frequencies for nodes and edges
  if ( _trace ) {
    fprintf( TFile, "  freq_transfer_node:");
    for ( map<IDTYPE, FB_FREQ>::iterator iv = freq_transfer_node.begin();
	  iv != freq_transfer_node.end(); ++iv ) {
      fprintf( TFile, " %d_", (*iv).first );
      (*iv).second.Print( TFile );
    }
    fprintf( TFile, "\n  freq_transfer_edge:" );
    for ( map<IDTYPE, FB_FREQ>::iterator ie = freq_transfer_edge.begin();
	  ie != freq_transfer_edge.end(); ++ie ) {
      IDTYPE ex = (*ie).first;
      const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
      fprintf( TFile, " (%d,%d)_", edge.source, edge.destination );
      (*ie).second.Print( TFile );
    }
    fprintf( TFile, "\n" );
  }

  map<vertex_id, vertex_id>::const_iterator map_iter;

  // determine the largest of the new node ids (perhaps the last?)
  IDTYPE nx_largest = 0;
  if ( ! nx_old_to_new.empty() ) {
    map_iter = nx_old_to_new.end();
    map_iter--;
    nx_largest = map_iter->second;
    for ( map_iter  = nx_old_to_new.begin();
	  map_iter != nx_old_to_new.end();   map_iter++ ) {
      IDTYPE nx_new = map_iter->second;
      if ( nx_new > nx_largest )
	nx_largest = nx_new;
    }
  }

  // Add the cloned nodes to the feedback cfg, initially with no incoming
  //   or outgoing edges.
  if ( _trace )
    fprintf( TFile, " Clone the nodes\n" );
  Add_node(nx_largest);
  for ( map_iter  = nx_old_to_new.begin();
	map_iter != nx_old_to_new.end();   map_iter++ ) {
    OPT_FB_NODE& node_old = _fb_opt_nodes[map_iter->first];
    OPT_FB_NODE& node_new = _fb_opt_nodes[map_iter->second];
    node_new.update_count = node_old.update_count;
    node_new.in_out_same  = node_old.in_out_same;
  }

  // Clone all the zone.clone edges
  if ( _trace )
    fprintf( TFile, " Clone the zone.clone edges\n" );
  for ( i = 0; i < edges_clone.size(); ++i ) {
    IDTYPE ex = edges_clone[i];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];

    // Modify original edge, and add the clone edge
    FB_FREQ freq_clone = freq_transfer_edge[ex];
    FB_FREQ freq_origl = edge.freq - freq_clone;
    Change_edge_freq( ex, freq_origl );
    Add_edge( nx_old_to_new[edge.source], nx_old_to_new[edge.destination],
	      edge.edge_type, freq_clone );
  }

  // Clone all the zone.exit edges
  if ( _trace )
    fprintf( TFile, " Clone the zone.exit edges\n" );
  for ( i = 0; i < edges_exit.size(); ++i ) {
    IDTYPE ex = edges_exit[i];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];

    // Modify original edge, and add the clone edge
    FB_FREQ freq_clone = freq_transfer_edge[ex];
    FB_FREQ freq_origl = edge.freq - freq_clone;
    Change_edge_freq( ex, freq_origl );
    Add_edge( nx_old_to_new[edge.source], edge.destination,
	      edge.edge_type, freq_clone );
  }
}


// ====================================================================
//
// OPT_FEEDBACK::Print displays a text representation of all node and
// edge frequency values
//
// OPT_FEEDBACK::Verify returns:
//   FB_VERIFY_INVALID     if feedback not valid,
//   FB_VERIFY_UNBALANCED  if feedback is valid but not balanced, and
//   FB_VERIFY_CONSISTENT  if feedback is valid and balanced.
// Asserts if feedback data has been corrupted
//
// ====================================================================

void
OPT_FEEDBACK::Print( FILE *fp ) const
{
  fprintf( fp, "OPT_FEEDBACK annotation:\n" );

  // Display nodes
#ifdef KEY /* Mac port */
  fprintf( fp, "%ld nodes:\n", (long) (_fb_opt_nodes.size() - 1) );
#else /* KEY Mac port */
  fprintf( fp, "%d nodes:\n", (INT)(_fb_opt_nodes.size() - 1) );
#endif
  for ( IDTYPE nx = 1; nx < _fb_opt_nodes.size(); nx++ ) {
    _fb_opt_nodes[nx].Print( nx, fp );
  }

  // Display edges
#ifdef KEY /* Mac port */
  fprintf( fp, "%ld edges:\n", (long) (_fb_opt_edges.size() - 1) );
#else /* KEY Mac port */
  fprintf( fp, "%d edges:\n", (INT)(_fb_opt_edges.size() - 1) );
#endif
  for ( IDTYPE ex = 1; ex < _fb_opt_edges.size(); ex++ ) {
    _fb_opt_edges[ex].Print( ex, fp );
  }
}


FB_VERIFY_STATUS
OPT_FEEDBACK::Verify( CFG *cfg, const char *const phase )
{
  Freq_propagate();

  if ( _trace )
    fprintf( TFile, "OPT_FEEDBACK::Verify %s:\n", phase );

  bool valid = true, balanced = true;

  // Look for unknown, uninitialized, and error edge frequencies
  for ( IDTYPE ex = 1; ex < _fb_opt_edges.size(); ++ex ) {
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];

    if ( ! edge.freq.Known() ) {
      if ( ! edge.freq.Initialized() )
	valid = false;
      else
	balanced = false;
      if ( _trace ) {
	fprintf( TFile, "  Edge[%d] has incoming frequency == ", ex );
	edge.freq.Print( TFile );
	fprintf( TFile, "\n" );
      }
    }
  }

  for ( IDTYPE nx = 1; nx < _fb_opt_nodes.size(); ++nx ) {
    const OPT_FB_NODE& node = _fb_opt_nodes[nx];

    // Test incoming frequency data
    INT t, unknown_in = 0, unexact_in = 0;
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for ( t = 0; t < node.incoming_edges.size(); ++t ) {
      IDTYPE ex = node.incoming_edges[t];
      const OPT_FB_EDGE& edge = _fb_opt_edges[ex];

      Is_True( edge.destination == nx,
	       ( "OPT_FEEDBACK::Verify found edge[ex%d] destination mismatch"
		 " (nx%d != nx%d)", ex, edge.destination, nx ) );

      freq_total += edge.freq;
      if ( ! edge.freq.Known() )  unknown_in += 1;
      if ( ! edge.freq.Exact() )  unexact_in += 1;
    }

    Is_True( node.unknown_in == unknown_in,
	     ( "OPT_FEEDBACK::Verify found unknown_in[%d] miscount (%d != %d)",
	       nx, node.unknown_in, unknown_in ) );
    Is_True( node.unexact_in == unexact_in,
	     ( "OPT_FEEDBACK::Verify found unexact_in[%d] miscount (%d != %d)",
	       nx, node.unexact_in, unexact_in ) );

    if ( node.freq_total_in != freq_total ) {  // change to Is_True later
      balanced = false;
      DevWarn( "OPT_FEEDBACK::Verify found node[%d]"
	       " freq_total_in mismatch!\n", nx );
    }

    // Test outgoing frequency data
    INT unknown_out = 0, unexact_out = 0;
    freq_total = FB_FREQ_ZERO;
    for ( t = 0; t < node.outgoing_edges.size(); t++ ) {
      IDTYPE ex = node.outgoing_edges[t];
      const OPT_FB_EDGE& edge = _fb_opt_edges[ex];

      Is_True( edge.source == nx,
	       ( "OPT_FEEDBACK::Verify found edge[ex%d] source mismatch"
		 " (nx%d != nx%d)", ex, edge.source, nx ) );

      freq_total += edge.freq;
      if ( ! edge.freq.Known() )  unknown_out += 1;
      if ( ! edge.freq.Exact() )  unexact_out += 1;
    }

    Is_True( node.unknown_out == unknown_out,
	     ( "OPT_FEEDBACK::Verify found unknown_out[%d] miscount"
	       " (%d != %d)", nx, node.unknown_out, unknown_out ) );
    Is_True( node.unexact_out == unexact_out,
	     ( "OPT_FEEDBACK::Verify found unexact_out[%d] miscount"
	       " (%d != %d)", nx, node.unexact_out, unexact_out ) );

    if ( node.freq_total_out != freq_total ) {  // change to Is_True later
      balanced = false;
      DevWarn( "  OPT_FEEDBACK::Verify found node[%d]"
	       " freq_total_out mismatch\n", nx );
    }

    // Do incoming and outgoing frequencies agree?
    if ( node.in_out_same &&
	 node.freq_total_in != node.freq_total_out &&
	 node.freq_total_in.Known() &&
	 node.freq_total_out.Known() ) {

      balanced = false;
      if ( _trace ) {
	fprintf( TFile, "  Node[%d] is unbalanced: incoming == ", nx );
	node.freq_total_in.Print( TFile );
	fprintf( TFile, ", outgoing == " );
	node.freq_total_out.Print( TFile );
	fprintf( TFile, "\n" );
      }
    }
  }

  // Check consistency of cfg with OPT_FEEDBACK
  for ( BB_NODE *bb = cfg->First_bb(); bb != NULL; bb = bb->Next() ) {

    if ( ! cfg->Removable_bb( bb ) ) // Skip fake entry and fake exit
      continue;

    if ( bb->Id() >= _fb_opt_nodes.size() ) {
      valid = false;
      if ( _trace )
#ifdef KEY /* Mac port */
	fprintf( TFile, "  CFG bb%d missing feedback! (_fb_opt_nodes.size()"
		 " = %ld)\n", bb->Id(), (long) _fb_opt_nodes.size() );
#else /* KEY Mac port */
	fprintf( TFile, "  CFG bb%d missing feedback! (_fb_opt_nodes.size()"
		 " = %d)\n", bb->Id(), (INT)_fb_opt_nodes.size() );
#endif
    }
    const OPT_FB_NODE& node = _fb_opt_nodes[bb->Id()];

    BB_LIST_ITER bb_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM( succ, bb_iter, Init(bb->Succ()) ) {
      if ( ! Edge_has_freq( bb->Id(), succ->Id() )
	   && cfg->Removable_bb( succ ) ) {
	valid = false;
	if ( _trace )
	  fprintf( TFile, "  CFG edge (bb%d --> bb%d) missing feedback!\n",
		   bb->Id(), succ->Id() );
      }
    }
  }

  if ( ! valid )
    DevWarn ( "OPT_FEEDBACK failed validation!" );

  // Display conclusions
  if ( _trace ) {
    if ( valid )
      fprintf( TFile, "OPT_FEEDBACK valid %s\n", phase );
    else
      fprintf( TFile, "OPT_FEEDBACK invalid %s\n", phase );
    if ( balanced )
      fprintf( TFile, "OPT_FEEDBACK balanced %s\n", phase );
    else
      fprintf( TFile, "OPT_FEEDBACK unbalanced %s\n", phase );
  }

  // Return status of OPT_FEEDBACK
  if ( ! valid ) {
    DevWarn( "OPT Feedback invalid" );
    return FB_VERIFY_INVALID;
  } else if ( ! balanced ) {
    DevWarn( "OPT Feedback unbalanced" );
    return FB_VERIFY_UNBALANCED;
  } else
    return FB_VERIFY_CONSISTENT;
}


// ====================================================================
// DaVinci OPT_FEEDBACK cfg Viewer
// ====================================================================

static DaVinci *DV = NULL;
static MEM_POOL DV_fb_mempool;

void
OPT_FEEDBACK::Print_node( FILE *fp, IDTYPE nx ) const
{
  _fb_opt_nodes[nx].Print( nx, fp );
}

void
OPT_FEEDBACK::Print_edge( FILE *fp, IDTYPE nx_src, IDTYPE nx_dst ) const
{
  const OPT_FB_NODE& node = _fb_opt_nodes[nx_src];
  for ( INT t = 0; t < node.outgoing_edges.size(); t++ ) {
    IDTYPE ex = node.outgoing_edges[t];
    const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
    if ( edge.destination == nx_dst )
      _fb_opt_edges[ex].Print( ex, fp );
  }
}

class OPT_FB_Callback : public DaVinci_Callback {
private:
  const OPT_FEEDBACK& _cfg;
public:
  OPT_FB_Callback( const OPT_FEEDBACK& cfg ) : _cfg( cfg ) {}

  virtual void Node_Select( const INT n_ids, const NODE_ID id_array[] );
  virtual void Edge_Select( const EDGE_ID& edge_id );
};

void
OPT_FB_Callback::Node_Select( const INT n_ids, const NODE_ID id_array[] )
{
  for (INT i = 0; i < n_ids; ++i) {
    IDTYPE nx = IDTYPE(INTPS(id_array[i]));
    _cfg.Print_node( stderr, nx );
  }
}

void
OPT_FB_Callback::Edge_Select( const EDGE_ID& edge_id )
{
  IDTYPE nx_src = IDTYPE(INTPS(edge_id.src));
  IDTYPE nx_dst = IDTYPE(INTPS(edge_id.dst));
  _cfg.Print_edge( stderr, nx_src, nx_dst );
}

char *
OPT_FEEDBACK::Node_label( IDTYPE nx ) const
{
  static char  label[50];
  char        *cp = label;

  const OPT_FB_NODE& node = _fb_opt_nodes[nx];

  // display IDTYPE number and incoming and outgoing frequencies
  cp += sprintf( cp, "%d: ", nx );
  cp += sprintf( cp, "\\nin  = " );
  cp += node.freq_total_in.Sprintf( cp );
  cp += sprintf( cp, "\\nout = " );
  cp += node.freq_total_out.Sprintf( cp );

  return label;
}


void
OPT_FEEDBACK::Draw() const
{
  NODE_TYPE  nt_unbalanced, nt_exact, nt_guess, nt_unknown, nt_change;
  EDGE_TYPE  et_exact, et_guess, et_unknown, et_uninit;

  // default color is black
  nt_unbalanced.Color ("light sky blue");
  nt_guess.Color(   "pink" );
  nt_unknown.Color( "orange" );
  nt_change.Color(  "light green" );
  et_guess.Color(   "red" );
  et_unknown.Color( "orange" );
  et_uninit.Color(  "blue" );

  DV->Graph_Begin();

  for ( IDTYPE nx = 1; nx < _fb_opt_nodes.size(); nx++ ) {
    const OPT_FB_NODE& node = _fb_opt_nodes[nx];
    // unbalanced, in/out different, exact, guess, or unknown node?
    FB_FREQ freq = node.freq_total_in + node.freq_total_out;
    NODE_TYPE *nt = &nt_exact;
    if ( ! node.in_out_same )   nt = &nt_change;
    else if ( ! freq.Known() )  nt = &nt_unknown;
    else if ( node.freq_total_in != node.freq_total_out )
				nt = &nt_unbalanced;
    else if ( freq.Guess() )    nt = &nt_guess;

    // Add the node and its outgoing edges to the daVinci graph
    DV->Node_Begin( NODE_ID(INTPTR(nx)), Node_label(nx), *nt );
    for ( INT t = 0; t < node.outgoing_edges.size(); t++ ) {
      IDTYPE ex = node.outgoing_edges[t];
      const OPT_FB_EDGE& edge = _fb_opt_edges[ex];
      FB_FREQ freq = edge.freq;

      EDGE_TYPE *et = &et_uninit;
      if (      edge.freq.Exact() )        et = &et_exact;
      else if ( edge.freq.Guess() )        et = &et_guess;
      else if ( edge.freq.Initialized() )  et = &et_unknown;

      IDTYPE nx_dst = edge.destination;
      DV->Out_Edge( EDGE_ID(NODE_ID(INTPTR(nx)), NODE_ID(INTPTR(nx_dst))),
		    *et, NODE_ID(INTPTR(nx_dst)) );
    }
    DV->Node_End();

  }
  DV->Graph_End();
}


void dV_view_fb_opt_cfg( const OPT_FEEDBACK& cfg,
			 WN *wn_tree, const char *caller )
{
  const char *trace_fname = getenv( "DV_TRACE_FILE" );
  FILE       *trace_fp    = NULL;
  const char       *func_name   = "<unknown func>";
  char        title[100];

  if ( ! DaVinci::enabled( true ) ) return;

  if ( wn_tree && WN_operator( wn_tree ) == OPR_FUNC_ENTRY ) {
    func_name = ST_name( WN_entry_name( wn_tree ) );
  }
  sprintf( title, "OPT_FEEDBACK display: %s ", func_name );

  FmtAssert( DV == NULL, ( "dV_view_fb_cfg: DV is null" ) );
  MEM_POOL_Initialize( &DV_fb_mempool, "DV_fb_mempool", FALSE );
  MEM_POOL_Push( &DV_fb_mempool );

  DV = CXX_NEW( DaVinci( &DV_fb_mempool, trace_fp ), &DV_fb_mempool );

  DV->Title( title );
  if ( caller ) DV->Show_Status( caller );
  cfg.Draw();

  OPT_FB_Callback callback( cfg );
  DV->Event_Loop( &callback );

  CXX_DELETE( DV, &DV_fb_mempool );
  DV = NULL;

  MEM_POOL_Pop( &DV_fb_mempool );
  MEM_POOL_Delete( &DV_fb_mempool );

  if ( trace_fp ) (void) fclose( trace_fp );
}


void
OPT_FEEDBACK::Display( WN *wn_tree, const char *caller ) const
{
  static char title[80];

  if ( _trace_draw ) {
    sprintf( title, "OPT_FEEDBACK for %s", caller );
    dV_view_fb_opt_cfg( *this, wn_tree, title );
  }
}


// ====================================================================
// The following is Raymond's code to repair unbalanced frequency flows
// resulting from cloning followed by dead code elimination.
// KEEP THIS!  It will probably be useful later.
//
// For the moment, all frequencies of cloned edges and nodes are set to
// FB_FREQ_UNKNOWN, so the unbalance problem does not arise.  This is a
// temporarly patch that will probably need to be replaced when IPA is
// handled, since much larger portions of the CFG will then be cloned.
// ====================================================================

// void
// OPT_FEEDBACK::inc_edge_freq(IDTYPE nx_src, IDTYPE nx_dst, FB_FREQ delta) {
//   IDTYPE ex = Find_edge_by_dest(nx_src, nx_dst);
//   if (ex == IDTYPE_NULL) {
//     add_edge(nx_src, nx_dst, FB_EDGE_UNINIT, delta); // OR ASSERT?
//   } else {
//     _fb_opt_edges[ex].freq += delta;
//   }
// }

// void
// OPT_FEEDBACK::set_edge_freq(IDTYPE nx_src, IDTYPE nx_dst, FB_FREQ delta) {
//   IDTYPE ex = Find_edge_by_dest(nx_src, nx_dst);
//   if (ex == IDTYPE_NULL) {
//     add_edge(nx_src, nx_dst, FB_EDGE_UNINIT, delta); // OR ASSERT?
//   } else {
//     _fb_opt_edges[ex].freq = delta;
//   }
// }

// void
// OPT_FEEDBACK::inc_node_freq(IDTYPE nx, FB_FREQ delta) {
//   _fb_opt_nodes[nx].update_count++;
//   _fb_opt_nodes[nx].freq_total_in  += delta;
//   _fb_opt_nodes[nx].freq_total_out += delta;
// }

// void
// OPT_FEEDBACK::set_node_freq(IDTYPE nx, FB_FREQ delta) { // DELETE?
//   // TODO: use new feedback so that we don't need to estimate
//   //  the n_pred and n_succ.
//   _fb_opt_nodes[nx].update_count--;
//   _fb_opt_nodes[nx] = OPT_FB_NODE(nx, 1, 1, delta); 
// }

// ====================================================================

// // Returns the sum of the frequencies of the incoming edges
// FB_FREQ
// OPT_FEEDBACK::in_flow(IDTYPE nx)
// {
//   OPT_FB_NODE& node = _fb_opt_nodes[nx];
//   FB_FREQ freq_total = FB_FREQ_ZERO;
//   for (INT t = node.incoming_edges.size() - 1; t >= 0; t--) {
//     IDTYPE ex = node.incoming_edges[t];
//     freq_total = freq_total + _fb_opt_edges[ex].freq;
//   }
//   return freq_total;
// }

// // Returns the sum of the frequencies of the outgoing edges
// FB_FREQ
// OPT_FEEDBACK::out_flow(IDTYPE nx)
// {
//   OPT_FB_NODE& node = _fb_opt_nodes[nx];
//   FB_FREQ freq_total = FB_FREQ_ZERO;
//   for (INT t = node.outgoing_edges.size() - 1; t >= 0; t--) {
//     IDTYPE ex = node.outgoing_edges[t];
//     freq_total = freq_total + _fb_opt_edges[ex].freq;
//   }
//   return freq_total;
// }


// // return a priority from 0.0 to 1.0.
// //  1.0 being highest priority
// FB_FREQ
// OPT_FEEDBACK::edge_repair_priority(IDTYPE nx_src, IDTYPE nx_dst, FB_FREQ flow)
// {
//   OPT_FB_NODE& node_src = _fb_opt_nodes[nx_src];
//   OPT_FB_NODE& node_dst = _fb_opt_nodes[nx_dst];

//   FB_FREQ pri = 0.95;
//   if (flow < 0.0) { // backward flow
//     FB_FREQ max_flow = get_edge_freq(nx_src, nx_dst);
//     if (- flow > max_flow)
//       pri *= - max_flow / flow;
//   }
//   if (flow < 0.0) {
//     if (node_src.update_count > node_dst.update_count) {
//       FB_FREQ t = 1 << max(3, node_src.update_count - node_dst.update_count);
//       pri /= t;
//     }
//   } else {
//     if (node_dst.update_count > node_src.update_count) {
//       FB_FREQ t = 1 << max(3, node_dst.update_count - node_src.update_count);
//       pri /= t;
//     }
//   }
//   return pri;
// }

// // NOTE: Rewrite using outgoing_edges instead of cfg?
// static bool
// is_reverse_flow(CFG *cfg, IDTYPE nx1, IDTYPE nx2)
// {
//   BB_LIST_ITER bb_ps_iter;
//   BB_NODE *succ;
//   FOR_ALL_ELEM( succ, bb_ps_iter, Init(cfg->Get_bb(nx2)->Succ()) ) {
//     if (succ->Id() == nx1)
//       return false;
//   }
//   return true;
// }

// FB_FREQ
// OPT_FEEDBACK::fix_path(path_type& p, bool trace)
// {
//   if (trace)
//     print_path_type(&p, TFile);

//   FB_FREQ flow = get_node_total_freq_in(p.first_bb()) - in_flow(p.first_bb());
//   bool ends_at_entry = _cfg->Get_bb(p.last_bb())->Kind() == BB_ENTRY;
//   if (! ends_at_entry)
//     flow = min(flow,
// 	       get_node_total_freq_out(p.last_bb()) - out_flow(p.last_bb()));
  
//   for (int i = 0; i < p.bbs.size() - 1; ++i) {
//     IDTYPE nx1 = p.bbs[i];
//     IDTYPE nx2 = p.bbs[i + 1];
//     if (is_reverse_flow(_cfg, nx1, nx2))  // upper limit for reverse flow
//       flow = min(flow, get_edge_freq(nx1, nx2));
//   }

//   if (trace) 
//     fprintf(TFile, "increase path flow by %g\n", flow);

//   for (i = 0; i < p.bbs.size() - 1; ++i) {
//     IDTYPE nx1 = p.bbs[i];
//     IDTYPE nx2 = p.bbs[i + 1];
//     if (is_reverse_flow(_cfg, nx1, nx2)) {
//       inc_edge_freq(nx1, nx2, -flow);
//       inc_node_freq(nx1, -flow);
//     } else {
//       inc_edge_freq(nx2, nx1, flow);
//       if (nx2 != p.last_bb())
// 	inc_node_freq(nx2, flow);
//     }
//   }
//   if (ends_at_entry)
//     inc_node_freq(p.last_bb(), flow);
  
//   return flow;
// }

// static const FB_FREQ threshold = 0.5; // allow an error threshold

// FB_FREQ
// OPT_FEEDBACK::increase_in_flow(IDTYPE nx, FB_FREQ flow)
// {
//   priority_queue<path_type*> heap;
//   path_type *p = CXX_NEW(path_type(nx, 1.0), &MEM_local_pool);
//   heap.push(p);
//   set<IDTYPE> processed;

//   while (! heap.empty()) {
//     path_type *p = heap.top();
//     heap.pop();
//     IDTYPE bb = (*p).last_bb();
//     if (processed.find(bb) != processed.end())
//       continue;

//     FB_FREQ diff = get_node_total_freq_out(bb) - out_flow(bb);
//     if ((diff > threshold ||
// 	 _cfg->Get_bb(bb)->Kind() == BB_ENTRY) &&
// 	(*p).bbs.size() > 1) {
//       // found a path to fix.
//       FB_FREQ adjusted_flow = fix_path(*p, _trace);
//       return adjusted_flow;
//     }
//     processed.insert(bb);
//     BB_LIST_ITER bb_ps_iter;
//     BB_NODE *succ, *pred;
//     if (bb != nx) {
//       FOR_ALL_ELEM( succ, bb_ps_iter, Init(_cfg->Get_bb(bb)->Succ()) ) {
// 	FB_FREQ pri = edge_repair_priority(bb, succ->Id(), -flow);
// 	path_type *p2 = CXX_NEW(path_type(*p), &MEM_local_pool);
// 	(*p2).add_bb(succ->Id());
// 	(*p2).wt *= pri;
// 	heap.push(p2);
//       }
//     }
//     FOR_ALL_ELEM( pred, bb_ps_iter, Init(_cfg->Get_bb(bb)->Pred()) ) {
//       FB_FREQ pri = edge_repair_priority(pred->Id(), bb, flow);
//       path_type *p2 = CXX_NEW(path_type(*p), &MEM_local_pool);
//       (*p2).add_bb(pred->Id());
//       (*p2).wt *= pri;
//       heap.push(p2);
//     }
//   }
//   return 0.0;
// } 


// // Adjust feedback after DCE deletes some edges (paths)
// //   -- use _CFG
// //   -- find "best" path for adjustment
// // 
// void
// OPT_FEEDBACK::dead_path_adjustment()
// {
//   // gather all basic block whose in-flow is reduced
//   // (due to edges deleted by DCE).
//   for (BB_NODE *bb = _cfg->First_bb();
//        bb != NULL;
//        bb = bb->Next()) {
//     FB_FREQ adjusted, diff;
//     int max_iter = 10;
//     do {
//       diff = get_node_total_freq_in(bb->Id()) - in_flow(bb->Id());
//       if (diff > threshold) {
// 	OPT_POOL_Push( &MEM_local_pool, -1);
// 	adjusted = increase_in_flow(bb->Id(), diff);
// 	OPT_POOL_Pop(&MEM_local_pool, -1);
//       }
//     } while (diff - adjusted > threshold &&
// 	     adjusted > threshold &&
// 	     max_iter-- > 0);
//   }
// }

// void
// OPT_FEEDBACK::show_error(FILE *fp)
// {
//   BB_NODE *bb_next;
//   FB_FREQ cumulative_err = 0.0;
//   for (BB_NODE *bb = _cfg->First_bb();
//        bb != NULL;
//        bb = bb->Next()) {
//     FB_FREQ vfin  = get_node_total_freq_in(bb->Id());
//     FB_FREQ vfout = get_node_total_freq_out(bb->Id());
//     FB_FREQ inf   = in_flow(bb->Id());
//     FB_FREQ outf  = out_flow(bb->Id());
//     if (bb->Kind() != BB_ENTRY &&
// 	fabs(vfin - inf) > threshold) {
//       fprintf(fp, "BB%d freqin=%g freqout=%g in=%g out=%g\n",
// 	      bb->Id(), vfin, vfout, inf, outf);
//       cumulative_err += fabs(vfin - inf);
//     }
//     if (bb->Kind() != BB_EXIT &&
// 	fabs(vfout - outf) > threshold) {
//       fprintf(fp, "BB%d freqin=%g freqout=%g in=%g out=%g\n",
// 	      bb->Id(), vfin, vfout, inf, outf);
//       cumulative_err += fabs(vfout - outf);
//     }
//   }
//   fprintf(fp, "OPT_FEEDBACK: cumulative error = %g\n", cumulative_err);
// }

// static bool is_succ(BB_NODE *bb, BB_NODE *bb2)
// {
//   BB_LIST_ITER bb_succ_iter;
//   BB_NODE *succ;
//   FOR_ALL_ELEM(succ, bb_succ_iter, Init(bb->Succ())) {
//     if (succ == bb2) return true;
//   }
//   return false;
// }

// void
// OPT_FEEDBACK::make_coherent()
// {
//   if (available()) {
//     {
//       vector<edge> buffer;
//       for (edge_freq_type::cluster_iterator ci = edge_freq.cluster_begin();
// 	   ci != edge_freq.cluster_end();
// 	   ++ci) {
// 	for (edge_freq_type::fast_iterator fi = (*ci).begin();
// 	     fi != (*ci).end();
// 	     ++fi) {
// 	  IDTYPE nx_src = (*fi).first;
// 	  IDTYPE nx_dst = (*fi).second;
// 	  if (_cfg->Get_bb(nx_src) == NULL ||
// 	      _cfg->Get_bb(nx_dst) == NULL ||
// 	      !is_succ(_cfg->Get_bb(nx_src), _cfg->Get_bb(nx_dst))) {
// 	    buffer.push_back(edge(nx_src,nx_dst));
// 	  }
// 	}
//       }
//       for (int i = 0; i < buffer.size(); ++i)
// 	erase_edge(edge_freq, buffer[i].first, buffer[i].second);
//     }
//     {
//       vector<IDTYPE> buffer;
//       for (node_freq_type::iterator vi = node_freq.begin();
// 	   vi != node_freq.end();
// 	   ++vi) {
// 	IDTYPE id = (*vi).first;
// 	if (_cfg->Get_bb(id) == NULL)
// 	  buffer.push_back(id);
//       }
//       for (int i = 0; i < buffer.size(); ++i)
// 	node_freq.erase(buffer[i]);
//     }
//     dead_path_adjustment();
//     if (_trace) {
//       Print(TFile);
//       show_error(TFile);
//     }
//   }
// }
