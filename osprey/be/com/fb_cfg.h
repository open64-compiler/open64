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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: fb_cfg.h
// $Revision: 1.10 $
// $Date: 05/12/05 08:59:13-08:00 $
// $Author: bos@eng-24.pathscale.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.fb_cfg.h $
//
// Description:
//
// fb_cfg.h and fb_cfg.cxx implement a control flow graph for edge
// frequency data.  The CFG can be displayed (using daVinci), and can
// propagate known frequencies to compute unknown values.
// The CFG _MUST_ contain no critical edges.
//
// ====================================================================
// ====================================================================

#ifndef fb_cfg_INCLUDED
#define fb_cfg_INCLUDED

#include "mempool_allocator.h"
#include <vector>
#include <deque>
#include <ext/hash_map>
#include "fb_whirl.h"

// ====================================================================
// FB_NODEX     -- Nodes are numbered from 0 upwards
// ====================================================================

typedef INT32 FB_NODEX;
#define FB_NODEX_UNINIT   (-1)
#define FB_NODEX_VALID(nx) ((nx) >= 0)

// ====================================================================
// FB_CFG Nodes
// ====================================================================

// Since FB_CFG contains no critical edges, if pred preceeds succ, then
// either pred.one_edge_succs or succ.one_edge_preds (or both) are true

struct FB_NODE {

  vector<FB_NODEX> preds; // Predecessors
  vector<FB_NODEX> succs; // Successors

  bool one_edge_preds;    // true iff every pred has no other succs
  bool one_edge_succs;    // true iff every succ has no other preds

  INT undelayed_succs;    // Used by daVinci

  // Source of node frequency data
  FB_EDGE_TYPE        node_type;
  WN                 *source;

  // Frequency data.  If in_out_same is true, then we may assume that
  // freq_total_in and freq_total_out are equal.
  bool                in_out_same;
  FB_FREQ             freq_total_in;
  FB_FREQ             freq_total_out;

  INT                 unknown_in;
  INT                 unknown_out;
  INT                 unexact_in;
  INT                 unexact_out;

  FB_NODE() :
    one_edge_preds( true ),
    one_edge_succs( true ),
    undelayed_succs( 0),
    node_type( FB_EDGE_UNINIT ),
    source( NULL ),
    in_out_same( true ),
    freq_total_in( FB_FREQ_UNKNOWN ),
    freq_total_out( FB_FREQ_UNKNOWN ),
    unknown_in( 1 ),
    unknown_out( 1 ),
    unexact_in( 1 ),
    unexact_out( 1 ) {}

  FB_NODE( FB_EDGE_TYPE type, WN *src, bool same,
	   FB_FREQ freq_in, FB_FREQ freq_out ) :
    one_edge_preds( true ),
    one_edge_succs( true ),
    undelayed_succs( 0 ),
    node_type( type ),
    source( src ),
    in_out_same( same ),
    freq_total_in( freq_in ),
    freq_total_out( freq_out ),
    unknown_in(  freq_in.Known()  ? 0 : 1 ),
    unknown_out( freq_out.Known() ? 0 : 1 ),
    unexact_in(  freq_in.Exact()  ? 0 : 1 ),
    unexact_out( freq_out.Exact() ? 0 : 1 ) {}

  void Print( FILE *fp, FB_NODEX nx ) const {
    INT t;
    char buffer[FB_EDGE_TYPE_NAME_LENGTH];
    FB_EDGE_TYPE_sprintf( buffer, node_type );
    fprintf( fp, "node %d: node_type %s, source 0x%p, in_out_same %c,\n",
	     nx, buffer, source, ( in_out_same ? 'Y' : 'N' ) );
    fprintf( fp, "  one_edge_preds %c, one_edge_succs %c,"
	     " undelayed_succs %d,\n", ( one_edge_preds ? 'Y' : 'N' ),
	     ( one_edge_succs ? 'Y' : 'N' ), undelayed_succs );
    fprintf( fp, "  unknown_in  %d, unexact_in  %d, freq_total_in  ",
	     unknown_in,  unexact_in );
    freq_total_in.Print( fp );
    fprintf( fp, ", preds [" );
    for ( t = 0; t < preds.size(); t++ )
      fprintf( fp, " %d", preds[t] );
    fprintf( fp, " ],\n");
    fprintf( fp, "  unknown_out %d, unexact_out %d, freq_total_out ",
	     unknown_out, unexact_out );
    freq_total_out.Print( fp );
    fprintf( fp, ", succs [" );
    for ( t = 0; t < succs.size(); t++ )
      fprintf( fp, " %d", succs[t] );
    fprintf( fp, " ]\n");
  }
};

struct FB_EDGE_DELAYED {  // Destination is a label instead of a node
  FB_NODEX      source;
  LABEL_IDX     destination;

  FB_EDGE_DELAYED( FB_NODEX src, LABEL_IDX dst ) {
    source = src;
    destination = dst;
  }
};

// ====================================================================

class FB_CFG_MEM {
protected:
  MEM_POOL _m;

  FB_CFG_MEM() {
    MEM_POOL_Initialize( &_m, "FB_CFG_MEM", true );
    MEM_POOL_Push( &_m );
  }
  ~FB_CFG_MEM() {
    MEM_POOL_Pop( &_m );
    MEM_POOL_Delete(&_m );
  }
};

class FB_CFG : public FB_CFG_MEM {
private:

  bool _trace;        // Get_Trace(TP_FEEDBACK, TP_FEEDBACK_CFG)
  bool _trace_draw;   // Get_Trace(TP_FEEDBACK, TP_FEEDBACK_CFG_DRAW)
  bool _trace_before; // Get_Trace(TP_FEEDBACK, TP_FEEDBACK_CFG_BEFORE)
  bool _trace_prop;   // Get_Trace(TP_FEEDBACK, TP_FEEDBACK_CFG_PROP)

  // vectors containing all nodes, indexed by FB_NODEX
  vector< FB_NODE, mempool_allocator<FB_NODE> > _nodes;

  // The following data elements are needed only during the construction of
  // the feedback control flow graph, to maintain state as the WHIRL code is
  // processed.
  //   _lblx_to_nx    maps label indicies to node indicies; assumes label
  //                  numbers are unique within a program unit
  //   _delayed_edges lists edges that will need to be added to the graph
  //                  after all nodes are in place
  //   _curr_nx       is the node currently being visited; if no node is
  //                  current, then nx == FB_NODEX_UNINIT

  hash_map< LABEL_IDX, FB_NODEX, __gnu_cxx::hash<LABEL_IDX>, std::equal_to<LABEL_IDX>,
    mempool_allocator< pair<LABEL_IDX,FB_NODEX> > > _lblx_to_nx;

  std::deque< FB_EDGE_DELAYED, mempool_allocator<FB_EDGE_DELAYED> > _delayed_edges;

  FB_NODEX     _curr_nx;

private:

  // The following functions access the FB_CFG data representation

  FB_NODEX New_node();
  FB_NODEX New_node( FB_EDGE_TYPE node_type, WN *source, FB_FREQ freq_total_in,
		     FB_FREQ freq_total_out, bool in_out_same = false );
  FB_NODEX New_node( FB_EDGE_TYPE node_type, WN *source,
		     FB_FREQ freq_total_in ) {
    return New_node( node_type, source, freq_total_in, freq_total_in, true );
  }

  void Add_label( LABEL_IDX labelx, FB_NODEX nx ) { _lblx_to_nx[labelx] = nx; }

  void Add_edge(FB_NODEX nx_src, FB_NODEX nx_dst, bool delayed = false );
  void Adjust_edge( FB_NODEX nodex);
  void Add_delayed_edge( FB_NODEX nx_src, WN *wn );
  void Complete_delayed_edges();

  FB_NODEX Curr() const { return _curr_nx; }
  void     Set_curr( FB_NODEX nx ) { _curr_nx = nx; }
  FB_NODEX Get_curr(); // Creates a new _curr if there isn't one already

  // The following functions are invoked during the construction of the cfg

  void Walk_WN_expression( WN *wn );
  void Walk_WN_test_expression( WN *wn, FB_NODEX true_nx, FB_NODEX false_nx );
  void Walk_WN_statement( WN *wn );

  void Freq_propagate_node_in( FB_NODEX nx );
  void Freq_propagate_node_out( FB_NODEX nx );
  void Freq_propagate();

  char *Node_label( FB_NODEX nx ) const;

public:

  FB_CFG() :
    _trace(        Get_Trace( TP_FEEDBACK, TP_FEEDBACK_CFG ) ),
    _trace_draw(   Get_Trace( TP_FEEDBACK, TP_FEEDBACK_CFG_DRAW ) ),
    _trace_before( Get_Trace( TP_FEEDBACK, TP_FEEDBACK_CFG_BEFORE ) ),
    _trace_prop(   Get_Trace( TP_FEEDBACK, TP_FEEDBACK_CFG_PROP ) ),
    _nodes( mempool_allocator<FB_NODE>(&_m) ),
    _lblx_to_nx( 100, __gnu_cxx::hash<LABEL_IDX>(), std::equal_to<LABEL_IDX>(),
		 mempool_allocator< pair<LABEL_IDX,FB_NODEX> >(&_m) ),
    _delayed_edges( mempool_allocator<FB_EDGE_DELAYED>(&_m) ),
    _curr_nx( FB_NODEX_UNINIT ) {}

  ~FB_CFG() {}

  void Construct_from_whirl( WN *wn_root, const char *caller );
  void Guess_unknowns( WN *wn_root, const char *caller );
  FB_VERIFY_STATUS Verify_frequencies() const;
  void Patch_whirl_frequencies() const;

  void Print( FILE *fp ) const;
  void Print_node( FILE *fp, FB_NODEX nx ) const;
  void Print_edge( FILE *fp, FB_NODEX src_nx, FB_NODEX dst_nx ) const;
  void Draw() const;
};

void dV_view_fb_cfg(const FB_CFG& cfg, WN *root_wn, const char *caller);

#endif
