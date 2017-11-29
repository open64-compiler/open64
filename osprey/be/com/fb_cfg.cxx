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
// Module: fb_cfg.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/com/fb_cfg.cxx,v $
//
// Description:
//
// Code to generate CFG for edge frequency data.  Displays CFG using
// daVinci package.  Uses CFG to compute unknown frequencies from known
// values.
// The CFG _MUST_ contain no critical edges.
//
// ======================================================================
// ======================================================================

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#define USE_STANDARD_TYPES

#include <cmplrs/fb.h>
#include <stdlib.h>
#include "wn_util.h"
#include "wn_map.h"
#include "errors.h"		// for ErrMsg
#include "tracing.h"

#include "fb_cfg.h"
#include "fb_whirl.h"
#include "com_whirlview.h"

#include "cxx_graph.h"

#include "DaVinci.h"      // for DaVinci viewer (for FB CFG).
#include "wb_util.h"      // more: move this to another file (gwe).

// ====================================================================

FB_NODEX
FB_CFG::New_node()
{
  FB_NODE new_node;
  FB_NODEX new_nodex = _nodes.size();
  _nodes.push_back( new_node );
  return new_nodex;
}

FB_NODEX
FB_CFG::New_node( FB_EDGE_TYPE node_type, WN *source, FB_FREQ freq_total_in,
		  FB_FREQ freq_total_out, bool in_out_same )
{
  FB_NODE new_node( node_type, source, in_out_same,
		    freq_total_in, freq_total_out );
  FB_NODEX new_nodex = _nodes.size();
  _nodes.push_back(new_node);
  return new_nodex;
}

/*
 * Adjust_edge, it is used to prevent critical edge happen in fb cfg
 */
void
FB_CFG::Adjust_edge( FB_NODEX nodex)
{
    INT s;

    for ( INT t = _nodes[nodex].preds.size() - 1; t >= 0; --t ){
	FB_NODEX nx_pred = _nodes[nodex].preds[t];
	FB_NODE& pred = _nodes[nx_pred];
	if(!( _nodes[nodex].one_edge_preds || pred.one_edge_succs)) {

		_nodes[nodex].preds.erase(_nodes[nodex].preds.begin() + t);

		for ( s = pred.succs.size() - 1; s >= 0; --s ){
			FB_NODEX nx_succs = pred.succs[s];
			if (nx_succs == nodex) {
				FB_NODEX *del_one;
				del_one = &(pred.succs[s]);
				pred.succs.erase(pred.succs.begin()+s);
    				_nodes[nx_pred].undelayed_succs -= 1;

				break;
			}
		}
		
		if (s < 0)
    			DevWarn( "Some thing wrong with edge adjust" );

		FB_NODEX nx_tmp = New_node();

  		if ( ! _nodes[nodex].freq_total_in.Exact() ) {
    			_nodes[nx_pred].unexact_out -= 1;
    			if ( ! _nodes[nodex].freq_total_in.Known() )
      				_nodes[nx_pred].unknown_out -= 1;
  		}

  		if ( ! _nodes[nx_pred].freq_total_out.Exact() ) {
    			_nodes[nodex].unexact_in -= 1;
    			if ( ! _nodes[nx_pred].freq_total_out.Known() )
      				_nodes[nodex].unknown_in -= 1;
  		}
			
		Add_edge(nx_pred, nx_tmp);
		Add_edge(nx_tmp, nodex);
		
    		_nodes[nodex].one_edge_preds = TRUE;
    		for ( s = _nodes[nodex].preds.size() - 1; s >= 0; --s ){
			FB_NODEX nx_spred = _nodes[nodex].preds[s];
			FB_NODE& spred = _nodes[nx_spred];
			if (spred.succs.size() >=2 ){
    				_nodes[nodex].one_edge_preds = FALSE; 
				break;
			}
		}
    		_nodes[nx_pred].one_edge_succs = TRUE;
    		for ( s = _nodes[nx_pred].succs.size() - 1; s >= 0; --s ){
			FB_NODEX nx_succ = _nodes[nx_pred].succs[s];
			FB_NODE& succ= _nodes[nx_succ];
			if (succ.preds.size() >=2 ){
    				_nodes[nx_pred].one_edge_succs = FALSE;
				break;
			}
		}

	}
    }
}

void
FB_CFG::Add_edge( FB_NODEX nx_src, FB_NODEX nx_dst, bool delayed )
{
  Is_True( FB_NODEX_VALID( nx_src ),
	   ( "FB_CFG::Add_edge encountered invalid source node" ) );
  Is_True( FB_NODEX_VALID( nx_dst ),
	   ( "FB_CFG::Add_edge encountered invalid destination node" ) );

  // Add successor to predecessor
  _nodes[nx_src].succs.push_back( nx_dst );
  if ( ! _nodes[nx_dst].freq_total_in.Exact() ) {
    _nodes[nx_src].unexact_out += 1;
    if ( ! _nodes[nx_dst].freq_total_in.Known() )
      _nodes[nx_src].unknown_out += 1;
  }
  if ( _nodes[nx_src].succs.size() == 2 ) {
    _nodes[_nodes[nx_src].succs[0]].one_edge_preds = FALSE;
  }
  if ( _nodes[nx_src].succs.size() >= 2 ) {
    _nodes[nx_dst].one_edge_preds = FALSE;
  }

  // Add predecessor to successor
  _nodes[nx_dst].preds.push_back( nx_src );
  if ( ! _nodes[nx_src].freq_total_out.Exact() ) {
    _nodes[nx_dst].unexact_in += 1;
    if ( ! _nodes[nx_src].freq_total_out.Known() )
      _nodes[nx_dst].unknown_in += 1;
  }
  if ( _nodes[nx_dst].preds.size() == 2 ) {
    _nodes[_nodes[nx_dst].preds[0]].one_edge_succs = FALSE;
  }
  if ( _nodes[nx_dst].preds.size() >= 2 ) {
    _nodes[nx_src].one_edge_succs = FALSE;
  }
  if ( ! delayed ) {
    _nodes[nx_src].undelayed_succs += 1;
  }
}

void
FB_CFG::Add_delayed_edge( FB_NODEX nx_src, WN *wn )
{
#ifdef Is_True_On
  if ( ! FB_NODEX_VALID( nx_src ) ) {
    DevWarn( "FB_CFG::Add_delayed_edge encountered invalid source node" );
  }
#endif

  LABEL_IDX lx_dst = WN_label_number(wn);
  Is_True( lx_dst > LABEL_IDX_ZERO,
	   ( "FB_CFG::Add_delayed_edge: lx_dst == %d should be positive",
	     lx_dst ) );
  FB_EDGE_DELAYED new_edge( nx_src, lx_dst );
  _delayed_edges.push_back( new_edge );
}

void
FB_CFG::Complete_delayed_edges()
{
  while ( ! _delayed_edges.empty() ) {
    FB_EDGE_DELAYED new_edge = _delayed_edges.front();
    _delayed_edges.pop_front();
    LABEL_IDX lx_dst = new_edge.destination;
    FB_NODEX  nx_dst = _lblx_to_nx[lx_dst];
    Add_edge( new_edge.source, nx_dst, TRUE );
  }
}

// ====================================================================
// The following methods construct a cfg from the given whirl tree.
// Expressions containing CALLS, CAND and CIOR are lowered in order to
// fully represent the frequency data.  The resulting cfg contains one
// node with total incoming or outgoing frequency cooresponding to each
// frequency measurement for the whirl tree, and a few other cfg nodes.
//
// The resulting cfg MUST contain NO critical edges.
// - Any branch to a label MUST have an intermediary node inserted
// ====================================================================


FB_NODEX
FB_CFG::Get_curr()
{
  if ( ! FB_NODEX_VALID( Curr() ) ) {
    FB_NODEX nx_new = New_node( FB_EDGE_UNINIT, NULL, FB_FREQ_ZERO );
    Set_curr( nx_new );
  }
  return Curr();
}


void
FB_CFG::Walk_WN_expression( WN *wn )
{
  OPERATOR opr = WN_operator( wn );

  switch ( opr ) {

  case OPR_CAND:
  case OPR_CIOR:
    {
      FB_FREQ freq_left, freq_right, freq_neither;
      freq_left    = Cur_PU_Feedback->Query( wn, FB_EDGE_CIRCUIT_LEFT    );
      freq_right   = Cur_PU_Feedback->Query( wn, FB_EDGE_CIRCUIT_RIGHT   );
      freq_neither = Cur_PU_Feedback->Query( wn, FB_EDGE_CIRCUIT_NEITHER );

      // Walk through the left test expression
      FB_NODEX nx_left = New_node( FB_EDGE_CIRCUIT_LEFT, wn, freq_left );
      FB_NODEX nx_not_left = New_node();
      if (opr == OPR_CAND) {
	Walk_WN_test_expression( WN_kid0(wn), nx_not_left, nx_left );
      } else {
	Walk_WN_test_expression( WN_kid0(wn), nx_left, nx_not_left );
      }

      // Walk through the right test expression
      Set_curr( nx_not_left );
      FB_NODEX nx_right = New_node( FB_EDGE_CIRCUIT_RIGHT, wn, freq_right );
      FB_NODEX nx_neither = New_node( FB_EDGE_CIRCUIT_NEITHER, wn,
				      freq_neither );
      if (opr == OPR_CAND) {
	Walk_WN_test_expression( WN_kid1(wn), nx_neither, nx_right );
      } else {
	Walk_WN_test_expression( WN_kid1(wn), nx_right, nx_neither );
      }

      // Rejoin
      FB_NODEX nx_join = New_node();
      Add_edge( nx_left,      nx_join );
      Add_edge( nx_right,     nx_join );
      Add_edge( nx_neither,   nx_join );
      Set_curr( nx_join );
    }
    break;

  case OPR_COMMA:
    Walk_WN_statement(WN_kid0(wn));
    Walk_WN_expression(WN_kid1(wn));
    break;

  case OPR_RCOMMA:
    Walk_WN_expression(WN_kid0(wn));
    Walk_WN_statement(WN_kid1(wn));
    break;

  case OPR_CSELECT:
    {
      FB_FREQ freq_then, freq_else;
      freq_then = Cur_PU_Feedback->Query( wn, FB_EDGE_BRANCH_TAKEN     );
      freq_else = Cur_PU_Feedback->Query( wn, FB_EDGE_BRANCH_NOT_TAKEN );

      // Walk through the test expression
      FB_NODEX nx_then = New_node( FB_EDGE_BRANCH_TAKEN, wn, freq_then );
      FB_NODEX nx_else = New_node( FB_EDGE_BRANCH_NOT_TAKEN, wn, freq_else );
      Walk_WN_test_expression( WN_kid0(wn), nx_then, nx_else );

      // Walk through THEN branch
      Set_curr( nx_then );
      Walk_WN_expression( WN_kid1(wn) );
      nx_then = Curr();

      // Walk through ELSE branch
      Set_curr( nx_else );
      Walk_WN_expression( WN_kid2(wn) );
      nx_else = Curr();

      // Add the join node
      FB_NODEX nx_join = New_node();
      Add_edge( nx_then, nx_join );
      Add_edge( nx_else, nx_join );
      Set_curr( nx_join );
    }
    break;

  case OPR_CALL:
  case OPR_ICALL:
  case OPR_VFCALL:
  case OPR_PICCALL:
  case OPR_INTRINSIC_CALL:

    if ( Cur_PU_Feedback->Same_in_out( wn ) ) {

      // Walk through the parameters
      for ( INT t = 0; t < WN_kid_count(wn); ++t )
	Walk_WN_expression( WN_kid( wn, t ) );

      FB_FREQ freq = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_INOUTSAME );

      FB_NODEX nx_next = New_node( FB_EDGE_CALL_INOUTSAME, wn, freq );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Set_curr( nx_next );

    } else { // not Same_in_out

      FB_FREQ freq_entry = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_INCOMING );
      FB_NODEX nx_next = New_node( FB_EDGE_CALL_INCOMING, wn, freq_entry );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Set_curr( nx_next );

      // Walk through the parameters
      for ( INT t = 0; t < WN_kid_count(wn); ++t )
	Walk_WN_expression( WN_kid( wn, t ) );

      FB_FREQ freq_exit  = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_OUTGOING );
      nx_next = New_node( FB_EDGE_CALL_OUTGOING, wn,
			  FB_FREQ_UNKNOWN, freq_exit );
      Add_edge( Curr(), nx_next );
      Set_curr( nx_next );
    }
    break;

  default:
    {
    // For most operations, just walk the kids
    for ( INT t = 0; t < WN_kid_count(wn); ++t )
      Walk_WN_expression( WN_kid( wn, t ) );
    }
    break;
  }
}

void
FB_CFG::Walk_WN_test_expression( WN *wn, FB_NODEX nx_true, FB_NODEX nx_false )
{
  OPERATOR opr = WN_operator( wn );

  switch ( opr ) {

  case OPR_CAND:
  case OPR_CIOR:
    {
      FB_NODEX nx_branch, nx_default;
      if ( opr == OPR_CAND ) {
	nx_branch  = nx_false;
	nx_default = nx_true;
      } else {
	nx_branch  = nx_true;
	nx_default = nx_false;
      }

      FB_FREQ freq_left, freq_right, freq_neither;
      freq_left    = Cur_PU_Feedback->Query( wn, FB_EDGE_CIRCUIT_LEFT    );
      freq_right   = Cur_PU_Feedback->Query( wn, FB_EDGE_CIRCUIT_RIGHT   );
      freq_neither = Cur_PU_Feedback->Query( wn, FB_EDGE_CIRCUIT_NEITHER );

      // Walk through the left test expression
      FB_NODEX nx_left = New_node( FB_EDGE_CIRCUIT_LEFT, wn, freq_left );
      FB_NODEX nx_not_left = New_node();
      if ( opr == OPR_CAND ) {
	Walk_WN_test_expression( WN_kid0(wn), nx_not_left, nx_left );
      } else {
	Walk_WN_test_expression( WN_kid0(wn), nx_left, nx_not_left );
      }

      // Walk through the right test expression
      Set_curr( nx_not_left );
      FB_NODEX nx_right = New_node( FB_EDGE_CIRCUIT_RIGHT, wn, freq_right );
      FB_NODEX nx_neither = New_node( FB_EDGE_CIRCUIT_NEITHER, wn,
				      freq_neither );
      if (opr == OPR_CAND) {
	Walk_WN_test_expression( WN_kid1(wn), nx_neither, nx_right );
      } else {
	Walk_WN_test_expression( WN_kid1(wn), nx_right, nx_neither );
      }

      // Add join edges
      Add_edge( nx_left, nx_branch );
      Add_edge( nx_right, nx_branch );
      Add_edge( nx_neither, nx_default );
    }
    break;

  case OPR_CSELECT:
    {
      FB_FREQ freq_then, freq_else;
      freq_then = Cur_PU_Feedback->Query( wn, FB_EDGE_BRANCH_TAKEN     );
      freq_else = Cur_PU_Feedback->Query( wn, FB_EDGE_BRANCH_NOT_TAKEN );

      // Walk through the test expression
      FB_NODEX nx_then = New_node( FB_EDGE_BRANCH_TAKEN, wn, freq_then );
      FB_NODEX nx_else = New_node( FB_EDGE_BRANCH_NOT_TAKEN, wn, freq_else );
      Walk_WN_test_expression( WN_kid0(wn), nx_then, nx_else );

      // Walk through THEN branch
      Set_curr( nx_then );
      FB_NODEX nx_then_true = New_node();
      FB_NODEX nx_then_false = New_node();
      Walk_WN_test_expression( WN_kid1(wn), nx_then_true, nx_then_false );

      // Walk through ELSE branch
      Set_curr( nx_else );
      FB_NODEX nx_else_true = New_node();
      FB_NODEX nx_else_false = New_node();
      Walk_WN_test_expression( WN_kid2(wn), nx_else_true, nx_else_false );

      // Add join edges
      Add_edge( nx_then_true, nx_true );
      Add_edge( nx_else_true, nx_true );
      Add_edge( nx_then_false, nx_false );
      Add_edge( nx_else_false, nx_false );
    }
    break;

  case OPR_COMMA:
    Walk_WN_statement( WN_kid0(wn) );
    Walk_WN_test_expression( WN_kid1(wn), nx_true, nx_false );
    break;

  case OPR_RCOMMA: // FIX RCOMMA !!
    Walk_WN_test_expression( WN_kid0(wn), nx_true, nx_false );
    Walk_WN_statement( WN_kid1(wn) );
    break;

  default:
    Walk_WN_expression(wn);
    Add_edge(Get_curr(), nx_true);
    Add_edge(Curr(), nx_false);

    Adjust_edge(nx_true);
    Adjust_edge(nx_false);
    break;
  }
}

// Constructs a cfg from the whirl tree rooted at wn.  Invokes
//
// After Walk_WN_statement returns, the Curr() node has no successors

void
FB_CFG::Walk_WN_statement( WN *wn )
{
  OPERATOR  opr = WN_operator( wn );
  WN       *wn2;
  static bool _in_preamble = FALSE;


  // Skip preamble (not instrumented).
  if ( _in_preamble )
    if ( opr == OPR_PRAGMA && WN_pragma( wn ) == WN_PRAGMA_PREAMBLE_END )
      _in_preamble = FALSE;
    else return;

  switch ( opr ) {

  case OPR_FUNC_ENTRY:
    {
      // Node for FUNC_ENTRY and start of body
      FB_FREQ freq_default =
	Cur_PU_Feedback->Query( wn, FB_EDGE_ENTRY_OUTGOING );
      FB_NODEX nx_next = New_node( FB_EDGE_ENTRY_OUTGOING, wn,
				   FB_FREQ_ZERO, freq_default );
      Set_curr( nx_next );

      WN *body         = WN_func_body(wn);
      WN *preamble_end = WN_first(body);

      // Locate end of preamble
      while ( preamble_end
	      && ! ( WN_operator(preamble_end) == OPR_PRAGMA &&
		     WN_pragma(preamble_end) == WN_PRAGMA_PREAMBLE_END) ) {
	preamble_end = WN_next(preamble_end);
      }
      Is_True( preamble_end != NULL,
	       ( "FB_CFG::Walk_WN_statement found no PREAMBLE_END PRAGMA" ) );

      // Walk through body of function (skipping preamble, but not pragma)
      for ( wn2 = preamble_end; wn2; wn2 = WN_next(wn2) ) {
	Walk_WN_statement( wn2 );
      }
    }
    break;

  case OPR_ALTENTRY:
    {
      // Node for ALTENTRY and start of body
      FB_FREQ freq_default =
	Cur_PU_Feedback->Query( wn, FB_EDGE_ENTRY_OUTGOING );
      FB_NODEX nx_next = New_node( FB_EDGE_ENTRY_OUTGOING, wn,
				   FB_FREQ_ZERO, freq_default );
      Set_curr( nx_next );

      // Assume there is a preamble
      _in_preamble = TRUE;
    }
    break;

  case OPR_PRAGMA:
    if ( WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END ) {

      // Node for preamble end PRAGMA
      FB_FREQ freq_default =
	Cur_PU_Feedback->Query( wn, FB_EDGE_OUTGOING );
      FB_NODEX nx_next = New_node( FB_EDGE_INCOMING, wn, freq_default );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Set_curr( nx_next );
    }
    break;

  case OPR_XPRAGMA:
    // skip pragmas (more? verify no BB created?)
    break;

  case OPR_BLOCK:
    // Walk through children; block itself is a no-op
    for (wn2 = WN_first(wn); wn2; wn2 = WN_next(wn2)) {
      Walk_WN_statement( wn2 );
    }
    Is_True( ! _in_preamble,
	     ("FB_CFG::Walk_WN_statement: missing PREAMBLE_END PRAGMA") );
    break;

    // case OPR_REGION:

  case OPR_DO_LOOP:
    {
      FB_FREQ freq_exit, freq_iterate;
      freq_exit    = Cur_PU_Feedback->Query( wn, FB_EDGE_LOOP_EXIT );
      freq_iterate = Cur_PU_Feedback->Query( wn, FB_EDGE_LOOP_ITERATE );

      // Walk through the start of the loop
      Walk_WN_statement( WN_start(wn) );

      // Walk through the test expression
      FB_NODEX nx_test = New_node();
      FB_NODEX nx_body = New_node( FB_EDGE_LOOP_ITERATE, wn, freq_iterate );
      FB_NODEX nx_next = New_node( FB_EDGE_LOOP_EXIT,    wn, freq_exit    );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_test );
      Set_curr( nx_test );
      Walk_WN_test_expression( WN_end(wn), nx_body, nx_next );

      // Walk through body and step of DO_LOOP
      Set_curr( nx_body );
      Walk_WN_statement( WN_do_body(wn) );
      Walk_WN_statement( WN_step(wn) );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_test );
      Set_curr( nx_next );
    }
    break;

  case OPR_DO_WHILE:
    {
      FB_FREQ freq_zero, freq_positive, freq_out, freq_back;
      freq_zero     = Cur_PU_Feedback->Query( wn, FB_EDGE_LOOP_ZERO );
      freq_positive = Cur_PU_Feedback->Query( wn, FB_EDGE_LOOP_POSITIVE );
      freq_out      = Cur_PU_Feedback->Query( wn, FB_EDGE_LOOP_OUT );
      freq_back     = Cur_PU_Feedback->Query( wn, FB_EDGE_LOOP_BACK );
      Is_True( freq_zero.Zero() || ! freq_zero.Known(),
	       ("FB_CFG::Walk_WN_statement: DO_WHILE freq_zero == %f nonzero",
		freq_zero.Value()) );

      // Walk through body of DO_WHILE
      FB_NODEX nx_pos  = New_node( FB_EDGE_LOOP_POSITIVE, wn, freq_positive );
      FB_NODEX nx_body = New_node();
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_pos );
      Add_edge( nx_pos, nx_body );
      Set_curr( nx_body );
      Walk_WN_statement( WN_while_body(wn) );

      // Walk through the test expression
      FB_NODEX nx_back = New_node( FB_EDGE_LOOP_BACK, wn, freq_back );
      FB_NODEX nx_next = New_node( FB_EDGE_LOOP_OUT , wn, freq_out  );
      Walk_WN_test_expression( WN_while_test(wn), nx_back, nx_next );
      Add_edge( nx_back, nx_body );
      Set_curr( nx_next );
    }
    break;

  case OPR_WHILE_DO:
    {
      FB_FREQ freq_exit, freq_iterate;
      freq_exit    = Cur_PU_Feedback->Query( wn, FB_EDGE_LOOP_EXIT );
      freq_iterate = Cur_PU_Feedback->Query( wn, FB_EDGE_LOOP_ITERATE );

      // Walk through the test expression
      FB_NODEX nx_test = New_node();
      FB_NODEX nx_body = New_node( FB_EDGE_LOOP_ITERATE, wn, freq_iterate );
      FB_NODEX nx_next = New_node( FB_EDGE_LOOP_EXIT,    wn, freq_exit    );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_test );
      Set_curr( nx_test );
      Walk_WN_test_expression( WN_while_test(wn), nx_body, nx_next );

      // Walk through body of WHILE_DO
      Set_curr( nx_body );
      Walk_WN_statement( WN_while_body(wn) );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_test );
      Set_curr( nx_next );
    }
    break;

  case OPR_IF:
    {
      FB_FREQ freq_then, freq_else;
      freq_then = Cur_PU_Feedback->Query( wn, FB_EDGE_BRANCH_TAKEN     );
      freq_else = Cur_PU_Feedback->Query( wn, FB_EDGE_BRANCH_NOT_TAKEN );

      // Walk through the test expression
      FB_NODEX nx_then = New_node( FB_EDGE_BRANCH_TAKEN,     wn, freq_then );
      FB_NODEX nx_else = New_node( FB_EDGE_BRANCH_NOT_TAKEN, wn, freq_else );
      Walk_WN_test_expression( WN_if_test(wn), nx_then, nx_else );

      // Walk through THEN branch
      Set_curr( nx_then );
      Walk_WN_statement( WN_then(wn) );
      nx_then = Curr();

      // Walk through ELSE branch
      if (! WN_else_is_empty(wn) ) {
	Set_curr( nx_else );
	Walk_WN_statement( WN_else(wn) );
	nx_else = Curr();
      }

      // Add the join node
      FB_NODEX nx_join = New_node();
      if ( FB_NODEX_VALID( nx_then ) )
	Add_edge( nx_then, nx_join );
      if ( FB_NODEX_VALID( nx_else ) )
	Add_edge( nx_else, nx_join );
      Set_curr( nx_join );
    }
    break;

  case OPR_GOTO:
    {
      FB_FREQ freq_branch =
	Cur_PU_Feedback->Query( wn, FB_EDGE_OUTGOING );
      FB_NODEX nx_next = New_node( FB_EDGE_OUTGOING, wn, freq_branch );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Add_delayed_edge( nx_next, wn );
      Set_curr( FB_NODEX_UNINIT );  // no default successor (expect label next)
    }
    break;

  case OPR_SWITCH:
  case OPR_COMPGOTO:
  case OPR_XGOTO:
    // XGOTO:    no fall-thru, all targets via branches.
    // COMPGOTO: add'l branch for out of range (if kid2 present).
    // SWITCH:   like COMPGOTO, except uses CASEGOTO instead of GOTO.
    {
      INT32    n_targ   = WN_num_entries(wn);
      WN      *targ_blk = WN_kid1(wn);
      INT32    count    = 0;
      OPERATOR br_opr   = (opr == OPR_SWITCH ? OPR_CASEGOTO : OPR_GOTO);
      FB_NODEX nx_target;
      FB_FREQ freq_branch;

#if defined(TARG_SL) && defined(TARG_SL2)
      if(WN_is_compgoto_for_minor ( wn ) || WN_is_compgoto_para ( wn )) {
        FB_NODEX head = New_node ( FB_EDGE_SWITCH_DEFAULT, wn, FB_FREQ_UNINIT, FB_FREQ_UNINIT);
        Add_edge(Curr(), head);
        Set_curr ( head ); 
      }
#endif

      // Walk through expression
      Walk_WN_expression(WN_kid0(wn));

      // Add target nodes
      for (wn2 = WN_first(targ_blk); wn2; wn2 = WN_next(wn2)) {
	Is_True( WN_operator(wn2) == br_opr,
		 ( "XGOTO/COMPGOTO/SWITCH: expected %s, found %s",
		   OPERATOR_name(br_opr), OPERATOR_name( WN_operator(wn2)) ) );
	freq_branch = Cur_PU_Feedback->Query( wn, FB_EDGE_SWITCH( count ) );

	nx_target = New_node( FB_EDGE_SWITCH( count ), wn, freq_branch );
	if(FB_NODEX_VALID(Curr()))
	  Add_edge( Curr(), nx_target );
	Add_delayed_edge( nx_target, wn2 );
	count += 1;
      }

      Is_True( count == n_targ, ( "%s: goto_count=%d n_targ=%d",
				  "XGOTO/COMPGOTO", count, n_targ ) );

      // WN_kid2(wn) is default jump target
      if ( ( opr == OPR_COMPGOTO || opr == OPR_SWITCH )
	   && WN_kid_count(wn) == 3 ) {
	wn2 = WN_kid2(wn);
	Is_True( WN_operator(wn2) == OPR_GOTO,
		 ( "COMPGOTO/SWITCH blk, expected GOTO" ) );

	freq_branch = Cur_PU_Feedback->Query( wn, FB_EDGE_SWITCH_DEFAULT );
	nx_target = New_node( FB_EDGE_SWITCH_DEFAULT, wn, freq_branch );
	if(FB_NODEX_VALID(Curr()))
	  Add_edge( Curr(), nx_target );
	Add_delayed_edge( nx_target, wn2 );
      }
      Set_curr( FB_NODEX_UNINIT ); // no fall-though successor.
    }
    break;

    // case OPR_AGOTO:
    // case OPR_REGION_EXIT:

  case OPR_TRUEBR:
  case OPR_FALSEBR:
    {
      FB_FREQ freq_branch, freq_default;
      freq_branch  = Cur_PU_Feedback->Query( wn, FB_EDGE_BRANCH_TAKEN     );
      freq_default = Cur_PU_Feedback->Query( wn, FB_EDGE_BRANCH_NOT_TAKEN );

      // Walk through the test expression
      FB_NODEX nx_branch = New_node( FB_EDGE_BRANCH_TAKEN, wn, freq_branch );
      FB_NODEX nx_next   = New_node( FB_EDGE_BRANCH_NOT_TAKEN, wn,
				     freq_default );
      if (opr == OPR_TRUEBR) {
	Walk_WN_test_expression( WN_kid0(wn), nx_branch, nx_next );
      } else {
	Walk_WN_test_expression( WN_kid0(wn), nx_next, nx_branch );
      }

      Add_delayed_edge( nx_branch, wn );
      Set_curr( nx_next );
    }
    break;

  case OPR_RETURN:
  case OPR_RETURN_VAL:
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
    {
      FB_FREQ freq_default =
	Cur_PU_Feedback->Query( wn, FB_EDGE_INCOMING );
      FB_NODEX nx_next = New_node( FB_EDGE_INCOMING, wn,
				   freq_default, FB_FREQ_ZERO );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Set_curr( FB_NODEX_UNINIT );  // no default successor.
    }
    break;

  case OPR_LABEL:
    {
      FB_FREQ freq = Cur_PU_Feedback->Query( wn, FB_EDGE_INCOMING );
      FB_NODEX nx_next = New_node( FB_EDGE_INCOMING, wn, freq );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Set_curr( nx_next );
      Add_label( WN_label_number(wn), nx_next );
    }
    break;

  case OPR_CALL:
  case OPR_ICALL:
  case OPR_VFCALL:
  case OPR_PICCALL:
  case OPR_INTRINSIC_CALL:

    if ( Cur_PU_Feedback->Same_in_out( wn ) ) {

      // Walk through the parameters
      for ( INT t = 0; t < WN_kid_count(wn); ++t )
	Walk_WN_expression( WN_kid( wn, t ) );

      FB_FREQ freq = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_INOUTSAME );

      FB_NODEX nx_next = New_node( FB_EDGE_CALL_INOUTSAME, wn, freq );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Set_curr( nx_next );

    } else { // not Same_in_out

      FB_FREQ freq_entry = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_INCOMING );
      FB_NODEX nx_next = New_node( FB_EDGE_CALL_INCOMING, wn, freq_entry );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Set_curr( nx_next );

      // Walk through the parameters
      for ( INT t = 0; t < WN_kid_count(wn); ++t )
	Walk_WN_expression( WN_kid( wn, t ) );

      FB_FREQ freq_exit  = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_OUTGOING );
      nx_next = New_node( FB_EDGE_CALL_OUTGOING, wn,
			  FB_FREQ_UNKNOWN, freq_exit );
      Add_edge( Curr(), nx_next );
      Set_curr( nx_next );
    }
    break;

    // parameters of fortran IO are not instrumented; IO feedback is stored
    //   like a CALL, but FB_CFG represents IO like a branch
  case OPR_IO:
    {
      // Prepare the incoming node
      FB_FREQ freq_escape = FB_FREQ_UNINIT;
      if ( Cur_PU_Feedback->Same_in_out( wn ) ) {

	freq_escape = FB_FREQ_ZERO;
	if ( ! FB_NODEX_VALID( Curr() ) ) {
	  FB_NODEX nx_next = New_node();
	  Set_curr( nx_next );
	}

      } else {  // not Same_in_out

	FB_FREQ freq_entry = Cur_PU_Feedback->Query( wn,
						     FB_EDGE_CALL_INCOMING );
	FB_NODEX nx_next = New_node( FB_EDGE_CALL_INCOMING, wn, freq_entry );
	if ( FB_NODEX_VALID( Curr() ) )
	  Add_edge( Curr(), nx_next );
	Set_curr( nx_next );
      }

      // Sometimes there are branches out of IO
      for ( INT32 iolist = 2; iolist < WN_kid_count( wn ); iolist++ ) {

	WN *wn_tmp = WN_kid( wn, iolist );
	IOITEM ioitem_tmp = ( IOITEM ) WN_io_item( wn_tmp );

	if ( ioitem_tmp == IOC_END ||
	     ioitem_tmp == IOC_ERR ||
	     ioitem_tmp == IOC_EOR ) {

	  FB_NODEX nx_escape = New_node( FB_EDGE_UNINIT, NULL, freq_escape );
	  Add_edge( Curr(), nx_escape );
	  Add_delayed_edge( nx_escape, WN_kid0( wn_tmp ) );
	}
      }

      // Handle the outgoing node
      if ( Cur_PU_Feedback->Same_in_out( wn ) ) {

	FB_FREQ freq = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_INOUTSAME );

	FB_NODEX nx_next = New_node( FB_EDGE_CALL_INOUTSAME, wn, freq );
	Add_edge( Curr(), nx_next );
	Set_curr( nx_next );

      } else { // not Same_in_out

	FB_FREQ freq_exit = Cur_PU_Feedback->Query( wn,
						    FB_EDGE_CALL_OUTGOING );
	FB_NODEX nx_next = New_node( FB_EDGE_CALL_OUTGOING, wn, freq_exit );
	Add_edge( Curr(), nx_next );
	Set_curr( nx_next );
      }
    }
    break;

  case OPR_EVAL:
  case OPR_STID:
    Walk_WN_expression( WN_kid0(wn) );
    break;

  case OPR_ISTORE:
    {
      for ( INT t = 0; t < WN_kid_count(wn); ++t )
	Walk_WN_expression( WN_kid( wn, t ) );
    }
    break;

  case OPR_MSTORE:
    {
      // Walk through kids
      for ( INT t = 0; t < WN_kid_count(wn); ++t )
	Walk_WN_expression( WN_kid( wn, t ) );

      // Node for MSTORE
      FB_FREQ freq_default =
	Cur_PU_Feedback->Query( wn, FB_EDGE_OUTGOING );
      FB_NODEX nx_next = New_node( FB_EDGE_OUTGOING, wn, freq_default );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_next );
      Set_curr( nx_next );
    }
    break;

  case OPR_COMMENT:
    break;

#ifdef KEY
  case OPR_PREFETCH:
  case OPR_PREFETCHX:
    break;
  case OPR_REGION:
    {
#if defined (TARG_SL) && defined(TARG_SL2)
      FB_FREQ freq_entry = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_INCOMING );
      FB_NODEX nx_entry = New_node( FB_EDGE_CALL_INCOMING, wn, freq_entry );
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_entry );
      Set_curr( nx_entry );

      Walk_WN_statement( WN_region_body(wn) );

      FB_FREQ freq_exit  = Cur_PU_Feedback->Query( wn, FB_EDGE_CALL_OUTGOING );
      if(Curr() == nx_entry && freq_entry.Initialized() && freq_exit.Initialized()) {
	    //add a fake node when the region freqs have been known, but we must maintain the fb_cfg consistence
	  FB_NODEX fn;
	  if(Cur_PU_Feedback->Same_in_out(wn)) 
          fn = New_node ( FB_EDGE_CALL_OUTGOING, wn, freq_exit);
	  else
          fn = New_node ( FB_EDGE_CALL_OUTGOING, wn, freq_entry, freq_exit);
        Add_edge(Curr(), fn);
        Set_curr ( fn ); 
      }

      FB_NODEX nx_exit = New_node( FB_EDGE_CALL_OUTGOING, wn, freq_exit );	  
      Add_edge( Curr(), nx_exit );
      Set_curr( nx_exit );
#else
      FB_NODEX nx_body = New_node();
      if ( FB_NODEX_VALID( Curr() ) )
	Add_edge( Curr(), nx_body );
      Set_curr( nx_body );
      Walk_WN_statement( WN_region_body(wn) );
#endif	  
    }
    break;
#endif

    // case OPR_PREFETCH:
    // case OPR_PREFETCHX:
    // case OPR_COMMENT:
    // case TRAP:
    // case OPR_ASSERT:
    // case FORWARD_BARRIER:
    // case BACKWARD_BARRIER:

  default:
#ifdef Is_True_On
    DevWarn( "FB_CFG::Walk_WN_statement, unexpectd opr %s",
	     OPERATOR_name(opr) );
#endif
    break;
  }
}

// ====================================================================

void
FB_CFG::Patch_whirl_frequencies() const
{
  FB_NODEX  nx;
  FB_FREQ freq_old, freq_new;

  if ( _trace )
    fprintf( TFile, "FB_CFG::Patch_whirl_frequencies:\n" );

  for ( nx = _nodes.size() - 1; FB_NODEX_VALID(nx); --nx ) {

    WN *wn = _nodes[nx].source;
    FB_EDGE_TYPE node_type = _nodes[nx].node_type;
    if ( node_type == FB_EDGE_UNINIT ) continue;

    freq_old = Cur_PU_Feedback->Query( wn, node_type );

    if ( node_type == FB_EDGE_OUTGOING ||
	 node_type == FB_EDGE_ENTRY_OUTGOING ||
	 node_type == FB_EDGE_CALL_OUTGOING )
      freq_new = _nodes[nx].freq_total_out;
    else
      freq_new = _nodes[nx].freq_total_in;

    if ( freq_new.Better( freq_old ) )
      Cur_PU_Feedback->Annot( wn, node_type, freq_new );
  }
}

// ====================================================================
// Frequency propagation
// ====================================================================

// For node nx, compare the sum of the incoming edge frequencies with
// freq_total_in.  Propagate any frequency data that can be computed
// from known values.
void
FB_CFG::Freq_propagate_node_in( FB_NODEX nx )
{
  FB_NODE& node = _nodes[nx];
  if ( _trace_prop ) {
    fprintf( TFile, "Before FB_CFG::Freq_propagate_node_in for:\n" );
    node.Print( TFile, nx );
  }

  if ( node.one_edge_preds && node.unexact_in > 0 ) {

    if ( node.freq_total_in.Exact() ) {

      // Identify unique predecessor with unexact frequency and
      // obtain total of exact frequencies
      FB_NODEX nx_unexact = FB_NODEX_UNINIT;
      FB_FREQ  freq_total = FB_FREQ_ZERO;
      for ( INT t = node.preds.size() - 1; t >= 0; --t ) {
	FB_NODEX nx_pred = node.preds[t];
	FB_FREQ  freq    = _nodes[nx_pred].freq_total_out;
	if ( freq.Exact() ) {
	  freq_total += freq;
	} else {
	  Is_True( nx_unexact == FB_NODEX_UNINIT || node.unexact_in > 1,
		   ( "FB_CFG::Freq_propagate_node_in[nx%d]"
		     " found multiple unexact pred freqs", nx ) );
	  nx_unexact = nx_pred;
	}
      }
      Is_True( nx_unexact != FB_NODEX_UNINIT,
	       ( "FB_CFG::Freq_propagate_node_in found no unexact freqs"));

      // Compute unaccounted portion of freq_unexact
      FB_FREQ freq_unexact = node.freq_total_in - freq_total;
      if ( freq_unexact.Error() ) return;  // Abort

      if ( node.unexact_in == 1 ) {

	// Exactly one pred has unexact frequency; propagate it
	FB_NODE& pred = _nodes[nx_unexact];
	pred.freq_total_out = freq_unexact;
	Is_True( pred.unexact_out == 1,
		 ( "FB_CFG::Freq_propagate_node_in[nx%d] found"
		   " pred[%d] unexact_out(%d) != 1",
		   nx, nx_unexact, pred.unexact_out ) );
	pred.unexact_out = 0;
	pred.unknown_out = 0;
	node.unexact_in  = 0;
	node.unknown_in  = 0;
	Freq_propagate_node_out( nx_unexact );

      } else if ( freq_unexact.Zero() ) {

	// Set all unexact predecessors to exact zero and propagate
	for ( INT t = node.preds.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_pred = node.preds[t];
	  FB_NODE& pred    = _nodes[nx_pred];
	  if ( ! pred.freq_total_out.Exact() ) {
	    node.unexact_in -= 1;
	    if ( ! pred.freq_total_out.Known() )
	      node.unknown_in -= 1;
	    pred.freq_total_out = FB_FREQ_ZERO;
	    pred.unexact_out = 0;
	    pred.unknown_out = 0;
	    Freq_propagate_node_out( nx_pred );
	  }
	}
      }

    } else if ( node.unexact_in == 1 ) { // && ! node.freq_total_in.Exact()

      // Total frequencies and decrement unexact_out of predecessors
      FB_FREQ  freq_total = FB_FREQ_ZERO;
      for ( INT t = node.preds.size() - 1; t >= 0; --t ) {
	FB_NODEX nx_pred = node.preds[t];
	FB_NODE& pred    = _nodes[nx_pred];
	FB_FREQ  freq    = pred.freq_total_out;
	Is_True( freq.Exact(), ( "FB_CFG::Freq_propagate_node_in[nx%d]"
				 " found an unexact pred[nx%d] freq",
				 nx, nx_pred ) );
	freq_total += freq;
	Is_True( pred.unexact_out == 1,
		 ( "FB_CFG::Freq_propagate_node_in[nx%d] found"
		   " pred[nx%d] unexact_out(%d) != 1",
		   nx, nx_pred, pred.unexact_out ) );
	pred.unexact_out = 0;
	pred.unknown_out = 0;
      }
      node.freq_total_in = freq_total;
      node.unexact_in = 0;
      node.unknown_in = 0;
    }

    if ( node.unknown_in == 1 ) {

      if ( node.freq_total_in.Known() ) {

	// Identify unique predecessor with unknown frequency and
	// obtain total of known frequencies
	FB_NODEX nx_unknown = FB_NODEX_UNINIT;
	FB_FREQ  freq_total = FB_FREQ_ZERO;
	for ( INT t = node.preds.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_pred = node.preds[t];
	  FB_FREQ  freq    = _nodes[nx_pred].freq_total_out;
	  if ( freq.Known() ) {
	    freq_total += freq;
	  } else {
	    Is_True( nx_unknown == FB_NODEX_UNINIT,
		     ( "FB_CFG::Freq_propagate_node_in[nx%d]"
		       " found multiple unknown pred freqs", nx ) );
	    nx_unknown = nx_pred;
	  }
	}
	Is_True( nx_unknown != FB_NODEX_UNINIT,
		 ( "FB_CFG::Freq_propagate_node_in[nx%d]"
		   " found no unknown freqs", nx ) );

	// Exactly one pred has unknown frequency; compute it and propagate
	FB_FREQ freq_unknown = node.freq_total_in - freq_total;
	if ( freq_unknown.Error() ) return;  // Abort

	FB_NODE& pred = _nodes[nx_unknown];
	pred.freq_total_out = freq_unknown;
	Is_True( pred.unknown_out == 1,
		 ( "FB_CFG::Freq_propagate_node_in[nx%d] found"
		   " pred[%d] unknown_out(%d) != 1",
		   nx, nx_unknown, pred.unknown_out ) );
	pred.unknown_out = 0;
	node.unknown_in = 0;
	Freq_propagate_node_out( nx_unknown );

      } else {

	// Total frequencies and decrement unknown_out of predecessors
	FB_FREQ  freq_total = FB_FREQ_ZERO;
	for ( INT t = node.preds.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_pred = node.preds[t];
	  FB_NODE& pred    = _nodes[nx_pred];
	  FB_FREQ freq = pred.freq_total_out;
	  Is_True( freq.Known(),
		   ( "FB_CFG::Freq_propagate_node_in[nx%d] found"
		     " an unknown pred[nx%d] freq", nx, nx_pred ) );
	  freq_total += freq;
	  Is_True( pred.unknown_out == 1,
		   ( "FB_CFG::Freq_propagate_node_in[nx%d] found"
		     " pred[nx%d] unknown_out(%d) != 1",
		     nx, nx_pred, pred.unknown_out ) );
	  pred.unknown_out = 0;
	}
	node.freq_total_in = freq_total;
	node.unknown_in = 0;
      }
    }
  }

  // Try to copy freq_total_in to freq_total_out
  if ( node.in_out_same || ( node.freq_total_in.Zero() &&
			     node.freq_total_in.Exact() &&
			     node.node_type != FB_EDGE_ENTRY_OUTGOING ) ) {
    bool copy_exact =
      node.freq_total_in.Exact() && ! node.freq_total_out.Exact();
    bool copy_known =
      node.freq_total_in.Known() && ! node.freq_total_out.Known();

    if ( copy_exact || copy_known ) {

      Is_True( copy_known || node.unexact_out > 0,
	       ( "FB_CFG::Freq_propagate_node_in[nx%d]"
		 " found unexact_out == 0", nx ) );
      Is_True( copy_exact || node.unknown_out > 0,
	       ( "FB_CFG::Freq_propagate_node_in[nx%d]"
		 " found unknown_out == 0", nx ) );

      node.freq_total_out = node.freq_total_in;

      if ( copy_exact ) {
	node.unexact_out -= 1;
	for ( INT t = node.succs.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_succ = node.succs[t];
	  Is_True( _nodes[nx_succ].unexact_in > 0,
		   ( "FB_CFG::Freq_propagate_node_in[nx%d] found"
		     " succ[nx%d].unexact_in == 0", nx, nx_succ ) );
	  _nodes[nx_succ].unexact_in -= 1;
	}
      }

      if ( copy_known ) {
	node.unknown_out -= 1;
	for ( INT t = node.succs.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_succ = node.succs[t];
	  Is_True( _nodes[nx_succ].unknown_in > 0,
		  ( "FB_CFG::Freq_propagate_node_in[nx%d] found"
		    " succ[nx%d].unknown_in == 0", nx, nx_succ ) );
	  _nodes[nx_succ].unknown_in -= 1;
	}
      }

      if ( node.one_edge_succs ) {
	Freq_propagate_node_out( nx );
      } else if ( node.succs.size() > 0 ) {
	Freq_propagate_node_in( node.succs[0] );
      }
    }
  }

  // fprintf( stderr, "After Freq_propagate_node_in:\n" );
  // _nodes[nx].Print( stderr, nx );
}


// For node nx, compare the sum of the outgoing edge frequencies with
// freq_total_out.  Propagate any frequency data that can be computed
// from known values.
void
FB_CFG::Freq_propagate_node_out( FB_NODEX nx )
{
  FB_NODE& node = _nodes[nx];
  if ( _trace_prop ) {
    fprintf( TFile, "Before FB_CFG::Freq_propagate_node_out for:\n" );
    node.Print( TFile, nx );
  }

  if ( node.one_edge_succs && node.unexact_out > 0 ) {

    if ( node.freq_total_out.Exact() ) {

      // Identify unique successor with unexact frequency and
      // obtain total of exact frequencies
      FB_NODEX nx_unexact = FB_NODEX_UNINIT;
      FB_FREQ  freq_total = FB_FREQ_ZERO;
      for ( INT t = node.succs.size() - 1; t >= 0; --t ) {
	FB_NODEX nx_succ = node.succs[t];
	FB_FREQ  freq    = _nodes[nx_succ].freq_total_in;
	if ( freq.Exact() ) {
	  freq_total += freq;
	} else {
	  Is_True( nx_unexact == FB_NODEX_UNINIT || node.unexact_out > 1,
		   ( "FB_CFG::Freq_propagate_node_out[nx%d]"
		     " found multiple unexact succ freqs", nx ) );
	  nx_unexact = nx_succ;
	}
      }
      Is_True( nx_unexact != FB_NODEX_UNINIT,
	       ( "FB_CFG::Freq_propagate_node_out found no unexact freqs"));

      // Compute unaccounted portion of freq_unexact
      FB_FREQ freq_unexact = node.freq_total_out - freq_total;
      if ( freq_unexact.Error() ) return;  // Abort

      if ( node.unexact_out == 1 ) {

	// Exactly one succ has unexact frequency; propagate it
	FB_NODE& succ = _nodes[nx_unexact];
	succ.freq_total_in = freq_unexact;
	Is_True( succ.unexact_in == 1,
		 ( "FB_CFG::Freq_propagate_node_out[nx%d] found"
		   " succ[%d] unexact_in(%d) != 1",
		   nx, nx_unexact, succ.unexact_in ) );
	succ.unexact_in  = 0;
	succ.unknown_in  = 0;
	node.unexact_out = 0;
	node.unknown_out = 0;
	Freq_propagate_node_in( nx_unexact );

      } else if ( freq_unexact.Zero() ) {

	// Set all unexact successors to exact zero and propagate
	for ( INT t = node.succs.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_succ = node.succs[t];
	  FB_NODE& succ    = _nodes[nx_succ];
	  if ( ! succ.freq_total_in.Exact() ) {
	    node.unexact_out -= 1;
	    if ( ! succ.freq_total_in.Known() )
	      node.unknown_out -= 1;
	    succ.freq_total_in = FB_FREQ_ZERO;
	    succ.unexact_in = 0;
	    succ.unknown_in = 0;
	    Freq_propagate_node_in( nx_succ );
	  }
	}
      }

    } else if ( node.unexact_out == 1 ) { // && ! node.freq_total_out.Exact()

      // Total frequencies and decrement unexact_in of successors
      FB_FREQ  freq_total = FB_FREQ_ZERO;
      for ( INT t = node.succs.size() - 1; t >= 0; --t ) {
	FB_NODEX nx_succ = node.succs[t];
	FB_NODE& succ    = _nodes[nx_succ];
	FB_FREQ  freq    = succ.freq_total_in;
	Is_True( freq.Exact(), ( "FB_CFG::Freq_propagate_node_out[nx%d]"
				 " found an unexact succ[nx%d] freq",
				 nx, nx_succ ) );
	freq_total += freq;
	Is_True( succ.unexact_in == 1,
		 ( "FB_CFG::Freq_propagate_node_out[nx%d] found"
		   " succ[nx%d] unexact_in(%d) != 1",
		   nx, nx_succ, succ.unexact_in ) );
	succ.unexact_in = 0;
	succ.unknown_in = 0;
      }
      node.freq_total_out = freq_total;
      node.unexact_out = 0;
      node.unknown_out = 0;
    }

    if ( node.unknown_out == 1 ) {

      if ( node.freq_total_out.Known() ) {

	// Identify unique successor with unknown frequency and
	// obtain total of known frequencies
	FB_NODEX nx_unknown = FB_NODEX_UNINIT;
	FB_FREQ  freq_total = FB_FREQ_ZERO;
	for ( INT t = node.succs.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_succ = node.succs[t];
	  FB_FREQ  freq    = _nodes[nx_succ].freq_total_in;
	  if ( freq.Known() ) {
	    freq_total += freq;
	  } else {
	    Is_True( nx_unknown == FB_NODEX_UNINIT,
		     ( "FB_CFG::Freq_propagate_node_out[nx%d] found"
		       " multiple unknown succ freqs", nx ) );
	    nx_unknown = nx_succ;
	  }
	}
	Is_True( nx_unknown != FB_NODEX_UNINIT,
		 ( "FB_CFG::Freq_propagate_node_out[nx%d]"
		   " found no unknown freqs", nx ) );

	// Exactly one succ has unknown frequency; compute it and propagate
	FB_FREQ freq_unknown = node.freq_total_out - freq_total;
	if ( freq_unknown.Error() ) return;  // Abort

	FB_NODE& succ = _nodes[nx_unknown];
	succ.freq_total_in = freq_unknown;
	Is_True( succ.unknown_in == 1,
		 ( "FB_CFG::Freq_propagate_node_out[nx%d] found"
		   " succ[%d] unknown_in(%d) != 1",
		   nx, nx_unknown, succ.unknown_in ) );
	succ.unknown_in = 0;
	node.unknown_out = 0;
	Freq_propagate_node_in( nx_unknown );

      } else {

	// Total frequencies and decrement unknown_in of successors
	FB_FREQ  freq_total = FB_FREQ_ZERO;
	for ( INT t = node.succs.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_succ = node.succs[t];
	  FB_NODE& succ    = _nodes[nx_succ];
	  FB_FREQ freq     = succ.freq_total_in;
	  Is_True( freq.Known(),
		   ( "FB_CFG::Freq_propagate_node_out[nx%d] found"
		     " an unknown succ[nx%d] freq", nx, nx_succ ) );
	  freq_total += freq;
	  Is_True( succ.unknown_in == 1,
		   ( "FB_CFG::Freq_propagate_node_out[nx%d] found"
		     " succ[nx%d] unknown_in(%d) != 1",
		     nx, nx_succ, succ.unknown_in ) );
	  succ.unknown_in = 0;
	}
	node.freq_total_out = freq_total;
	node.unknown_out = 0;
      }
    }
  }

  // Try to copy freq_total_out to freq_total_in
  if ( node.in_out_same ) {
    bool copy_exact =
      node.freq_total_out.Exact() && ! node.freq_total_in.Exact();
    bool copy_known =
      node.freq_total_out.Known() && ! node.freq_total_in.Known();

    if ( copy_exact || copy_known ) {

      Is_True( copy_known || node.unexact_in > 0,
	       ( "FB_CFG::Freq_propagate_node_out[nx%d]"
		 " found unexact_in == 0", nx ) );
      Is_True( copy_exact || node.unknown_in > 0,
	       ( "FB_CFG::Freq_propagate_node_out[nx%d]"
		 " found unknown_in == 0", nx ) );

      node.freq_total_in = node.freq_total_out;

      if ( copy_exact ) {
	node.unexact_in -= 1;
	for ( INT t = node.preds.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_pred = node.preds[t];
	  Is_True( _nodes[nx_pred].unexact_out > 0,
		   ( "FB_CFG::Freq_propagate_node_out[nx%d] found"
		     " pred[nx%d].unexact_out == 0", nx, nx_pred ) );
	  _nodes[nx_pred].unexact_out -= 1;
	}
      }

      if ( copy_known ) {
	node.unknown_in -= 1;
	for ( INT t = node.preds.size() - 1; t >= 0; --t ) {
	  FB_NODEX nx_pred = node.preds[t];
	  Is_True( _nodes[nx_pred].unknown_out > 0,
		   ( "FB_CFG::Freq_propagate_node_out[nx%d] found"
		     " pred[nx%d].unknown_out == 0", nx, nx_pred ) );
	  _nodes[nx_pred].unknown_out -= 1;
	}
      }

      if ( node.one_edge_preds ) {
	Freq_propagate_node_in( nx );
      } else if ( node.preds.size() > 0 ) {
	Freq_propagate_node_out( node.preds[0] );
      }
    }
  }

  // fprintf( stderr, "After Freq_propagate_node_out:\n" );
  // _nodes[nx].Print( stderr, nx );
}


void
FB_CFG::Freq_propagate()
{
  FB_NODEX nx;
  if ( _trace )
    fprintf( TFile, "FB_CFG::Freq_propagate:\n" );

  // for ( nx = 0; nx < _nodes.size(); ++nx ) {
  //   _nodes[nx].Print( stderr, nx );
  // }

  for ( nx = 0; nx < _nodes.size(); ++nx ) {
    Freq_propagate_node_in(nx);
    Freq_propagate_node_out(nx);
  }
}


// ====================================================================
// Guess all unknown frequencies  (perform Freq_propagate first)
// ====================================================================

void
FB_CFG::Guess_unknowns( WN *wn_root, const char *caller )
{
  if ( _trace )
    fprintf( TFile, "FB_CFG::Guess_unknowns:\n" );

  vector<FB_NODEX> unfinished_nodes;

  // Identify all unfinished nodes
  for ( FB_NODEX nx = 0; nx < _nodes.size(); ++nx ) {
    const FB_NODE& node = _nodes[nx];
    if ( node.unknown_in > 0 || node.unknown_out > 0 )
      unfinished_nodes.push_back( nx );
  }

  if ( unfinished_nodes.empty() ) {
    if ( _trace )
      fprintf( TFile, "  FB_CFG::Guess_unknowns found no unknowns" );
    return;
  }

  bool change;

  do {

    do {

      change = false;

      // First, fill in guesses for predecessors

      for ( INT t = unfinished_nodes.size() - 1; t >= 0; --t ) {
	FB_NODEX nx = unfinished_nodes[t];
	FB_NODE& node = _nodes[nx];

	if ( node.one_edge_preds && node.freq_total_in.Known() ) {

	  // Reduce the number of unknown predecessors to 1 by guessing
	  while ( node.unknown_in > 1 ) {

	    // Identify one predecessor with unknown frequency and
	    // obtain total of known frequencies
	    FB_NODEX nx_unknown = FB_NODEX_UNINIT;
	    FB_FREQ  freq_total = FB_FREQ_ZERO;
	    for ( INT t1 = node.preds.size() - 1; t1 >= 0; --t1 ) {
	      FB_NODEX nx_pred = node.preds[t1];
	      FB_FREQ  freq    = _nodes[nx_pred].freq_total_out;
	      if ( freq.Known() )
		freq_total += freq;
	      else
		nx_unknown = nx_pred;
	    }
	    Is_True( nx_unknown != FB_NODEX_UNINIT,
		     ( "FB_CFG::Guess_unknowns[nx%d]"
		       " found no unknown incoming freqs", nx ) );

	    // Compute a guess for the predecessor frequency
	    FB_FREQ  freq_guess = node.freq_total_in - freq_total;
	    if ( freq_guess.Error() )
	      freq_guess = FB_FREQ( 0.0, false );
	    else
	      freq_guess /= node.unknown_in;

	    // Update one unknown predecessor frequency with the guess
	    FB_NODE& pred = _nodes[nx_unknown];
	    pred.freq_total_out = freq_guess;
	    pred.unknown_out = 0;
	    node.unknown_in -= 1;
	    if ( freq_guess.Exact() ) {
	      pred.unexact_out = 0;
	      node.unexact_in -= 1;
	    }
	    Freq_propagate_node_out( nx_unknown );
	  }

	  // At most one predecessor frequency is unknown
	  Freq_propagate_node_in( nx );
	}

	if ( node.unknown_in == 0 && node.unknown_out == 0 ) {
	  // delete unfinished node t
	  // This works because traversal is in reverse order
	  unfinished_nodes[t] = unfinished_nodes.back();
	  unfinished_nodes.pop_back();
	}
      }

    } while ( change );

    // Second, fill in guesses for successors
    for ( INT t = unfinished_nodes.size() - 1; t >= 0; --t ) {
      FB_NODEX nx = unfinished_nodes[t];
      FB_NODE& node = _nodes[nx];

      if ( node.one_edge_succs && node.freq_total_out.Known() ) {

	// Reduce the number of unknown successors to 1 by guessing
	while ( node.unknown_out > 1 ) {

	  // Identify one successor with unknown frequency and
	  // obtain total of known frequencies
	  FB_NODEX nx_unknown = FB_NODEX_UNINIT;
	  FB_FREQ  freq_total = FB_FREQ_ZERO;
	  for ( INT t1 = node.succs.size() - 1; t1 >= 0; --t1 ) {
	    FB_NODEX nx_succ = node.succs[t1];
	    FB_FREQ  freq    = _nodes[nx_succ].freq_total_in;
	    if ( freq.Known() )
	      freq_total += freq;
	    else
	      nx_unknown = nx_succ;
	  }
	  Is_True( nx_unknown != FB_NODEX_UNINIT,
		   ( "FB_CFG::Guess_unknowns[nx%d]"
		     " found no unknown outgoing freqs", nx ) );

	  // Compute a guess for the successor frequency
	  FB_FREQ  freq_guess = node.freq_total_out - freq_total;
	  if ( freq_guess.Error() )
	    freq_guess = FB_FREQ( 0.0, false );
	  else
	    freq_guess /= node.unknown_out;

	  // Update one unknown successor frequency with the guess
	  FB_NODE& succ = _nodes[nx_unknown];
	  succ.freq_total_in = freq_guess;
	  succ.unknown_in   = 0;
	  node.unknown_out -= 1;
	  if ( freq_guess.Exact() ) {
	    succ.unexact_in   = 0;
	    node.unexact_out -= 1;
	  }
	  Freq_propagate_node_in( nx_unknown );
	}

	// At most one successor frequency is unknown
	Freq_propagate_node_out( nx );
      }

      if ( node.unknown_in == 0 && node.unknown_out == 0 ) {
	// delete unfinished node t
	// This works because traversal is in reverse order
	unfinished_nodes[t] = unfinished_nodes.back();
	unfinished_nodes.pop_back();
      }
    }

  } while ( change );

  if ( ! unfinished_nodes.empty() ) {
    DevWarn( "FB_CFG::Guess_unknowns failed to guess all unknowns!" );
  }

  if ( _trace || _trace_draw ) {
    static char title[80];
    sprintf( title, "FB_CFG for %s after Guess_unknowns", caller );

    if ( _trace ) {
      fprintf( TFile, "------------ %s ------------\n", title );
      Print( TFile );
    }

    if ( _trace_draw )
      dV_view_fb_cfg( *this, wn_root, title );
  }
}


// ====================================================================
// Verify frequencies
// ====================================================================

FB_VERIFY_STATUS
FB_CFG::Verify_frequencies() const
{
  if ( _trace )
    fprintf( TFile, "FB_CFG::Verify_frequencies:\n" );

  bool valid = true, balanced = true;

  for ( FB_NODEX nx = 0; nx < _nodes.size(); ++nx ) {
    const FB_NODE& node = _nodes[nx];

    // Are incoming and outgoing frequencies initialized and known?
    if ( ! node.freq_total_in.Known() ) {
      if ( ! node.freq_total_in.Initialized() )
	valid = false;
      else
	balanced = false;
      if ( _trace ) {
	fprintf( TFile, "  Node[%d] has incoming frequency == ", nx );
	node.freq_total_in.Print( TFile );
	fprintf( TFile, "\n" );
      }
    }
    if ( ! node.freq_total_out.Known() ) {
      if ( ! node.freq_total_out.Initialized() )
	valid = false;
      else
	balanced = false;
      if ( _trace ) {
	fprintf( TFile, "  Node[%d] has outgoing frequency == ", nx );
	node.freq_total_out.Print( TFile );
	fprintf( TFile, "\n" );
      }
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

    // Test incoming frequency data
    {
      INT     unknown_in = ( node.freq_total_in.Known() ? 0 : 1 );
      INT     unexact_in = ( node.freq_total_in.Exact() ? 0 : 1 );
      FB_FREQ freq_total = FB_FREQ_ZERO;
      
      for ( INT t = node.preds.size() - 1; t >= 0; --t ){
	FB_NODEX nx_pred = node.preds[t];
	const FB_NODE& pred = _nodes[nx_pred];

	// Check for errors in fb_cfg.cxx code
	Is_True( find( pred.succs.begin(), pred.succs.end(), nx )
		 != pred.succs.end(),
		 ( "FEEDBACK::Verify found disconnected edge nx%d <-- nx%d",
		   nx_pred, nx ) );
	Is_True( node.one_edge_preds || pred.one_edge_succs,
		 ( "FEEDBACK::Verify found node[%d] and pred[%d] with"
		   " one_edge_* both false", nx, nx_pred ) );
	Is_True( ! node.one_edge_preds || pred.succs.size() == 1,
		 ( "FEEDBACK::Verify found pred[%d] of node[%d] with %d succs",
		   nx_pred, nx, pred.succs.size() ) );

	FB_FREQ freq_pred = pred.freq_total_out;
	freq_total += freq_pred;
	if ( ! freq_pred.Known() )  unknown_in += 1;
	if ( ! freq_pred.Exact() )  unexact_in += 1;
      }

      // Verify that unknown_in and unexact_in have correct values
      Is_True( node.unknown_in == unknown_in,
	       ( "FEEDBACK::Verify found unknown_in[%d] miscount (%d != %d)",
		 nx, node.unknown_in, unknown_in ) );
      Is_True( node.unexact_in == unexact_in,
	       ( "FEEDBACK::Verify found unexact_in[%d] miscount (%d != %d)",
		 nx, node.unexact_in, unexact_in ) );

      if ( node.one_edge_preds && node.freq_total_in != freq_total ) {
	balanced = false;
	if ( _trace ) {
	  fprintf( TFile, "  Node[%d] has incoming unbalance (", nx );
	  node.freq_total_in.Print( TFile );
	  fprintf( TFile, " != " );
	  freq_total.Print( TFile );
	  fprintf( TFile, "\n" );
	}
      }
    }

    // Test outgoing frequency data
    {
      INT     unknown_out = ( node.freq_total_out.Known() ? 0 : 1 );
      INT     unexact_out = ( node.freq_total_out.Exact() ? 0 : 1 );
      FB_FREQ freq_total = FB_FREQ_ZERO;
      
      for ( INT t = node.succs.size() - 1; t >= 0; --t ){
	FB_NODEX nx_succ = node.succs[t];
	const FB_NODE& succ = _nodes[nx_succ];

	// Check for errors in fb_cfg.cxx code
	Is_True( find( succ.preds.begin(), succ.preds.end(), nx )
		 != succ.preds.end(),
		 ( "FEEDBACK::Verify found disconnected edge nx%d --> nx%d",
		   nx, nx_succ ) );
	Is_True( node.one_edge_succs || succ.one_edge_preds,
		 ( "FEEDBACK::Verify found node[%d] and succ[%d] with"
		   " one_edge_* both false", nx, nx_succ ) );
	Is_True( ! node.one_edge_succs || succ.preds.size() == 1,
		 ( "FEEDBACK::Verify found succ[%d] of node[%d] with %d preds",
		   nx_succ, nx, succ.preds.size() ) );

	FB_FREQ freq_succ = succ.freq_total_in;
	freq_total += freq_succ;
	if ( ! freq_succ.Known() )  unknown_out += 1;
	if ( ! freq_succ.Exact() )  unexact_out += 1;
      }

      // Verify that unknown_out and unexact_out have correct values
      Is_True( node.unknown_out == unknown_out,
	       ( "FEEDBACK::Verify found unknown_out[%d] miscount (%d != %d)",
		 nx, node.unknown_out, unknown_out ) );
      Is_True( node.unexact_out == unexact_out,
	       ( "FEEDBACK::Verify found unexact_out[%d] miscount (%d != %d)",
		 nx, node.unexact_out, unexact_out ) );

      if ( node.one_edge_succs && node.freq_total_out != freq_total ) {
	balanced = false;
	if ( _trace ) {
	  fprintf( TFile, "  Node[%d] has outgoing unbalance (", nx );
	  node.freq_total_out.Print( TFile );
	  fprintf( TFile, " != " );
	  freq_total.Print( TFile );
	  fprintf( TFile, "\n" );
	}
      }
    }
  }

  // Display conclusions
  if ( _trace ) {
    if ( valid )
      fprintf( TFile, "FB_CFG valid!\n" );
    else
      fprintf( TFile, "FB_CFG invalid!\n" );
    if ( balanced )
      fprintf( TFile, "FB_CFG balanced!\n" );
    else
      fprintf( TFile, "FB_CFG unbalanced!\n" );
  }

  // Return status of FB_CFG
  if ( ! valid )
    return FB_VERIFY_INVALID;
  else if ( ! balanced )
    return FB_VERIFY_UNBALANCED;
  else
    return FB_VERIFY_CONSISTENT;
}


// ====================================================================
// FB_CFG::Construct_from_whirl(WN *wn_root)
// initialized (though possibly unknown) frequencies.
// ====================================================================

void
FB_CFG::Construct_from_whirl( WN *wn_root, const char *caller )
{
  static char title[80];

  if ( _trace )
    fprintf( TFile, "FB_CFG::Construct_from_whirl:\n" );

  // Construction of cfg proceeds as follows:

  // (1)  Make one pass over all the whirl code.  Create cfg nodes for
  // control flow constructs are they are encountered.  Record the whirl
  // constructs are encountered, create cfg node are created in the cfg
  // node in the cfg for
  Walk_WN_statement(wn_root);   // init. _lblx_to_nx, _branch_src, link succ.

  // (2)  Add all remaining edges to the cfg, replacing labels by cfg nodes.
  Complete_delayed_edges();

  if ( _trace_before && ( _trace || _trace_draw ) )
    sprintf( title, "FB_CFG for %s before propagation", caller );

  if ( _trace_before && _trace ) {
    fprintf( TFile, "------------ %s ------------\n", title );
    Print( TFile );
  }

  if ( _trace_draw && _trace_before )
    dV_view_fb_cfg( *this, wn_root, title );

  // (3) Compute unknown edge frequencies by propagating known values
  Freq_propagate();

  if ( _trace || _trace_draw )
    sprintf( title, "FB_CFG for %s after propagation", caller );

  if ( _trace ) {
    fprintf( TFile, "------------ %s ------------\n", title );
    Print( TFile );
  }

  if ( _trace_draw )
    dV_view_fb_cfg( *this, wn_root, title );
}

// ====================================================================
// DaVinci FB CFG Viewer
// ====================================================================

static DaVinci *DV = NULL;
static MEM_POOL DV_fb_mempool;

void
FB_CFG::Print_node( FILE *fp, FB_NODEX nx ) const
{
  _nodes[nx].Print( fp, nx );

  // OLD CODE:
  //   char buf[5000];
  //   WN *wn = _nodes[nx].source;

  //   INT end = WB_Dump_Whirl_Expr(wn, wn, buf, 0);
  //   buf[end] = '\0';
  //   fprintf( fp, "nx=%d, in=", nx );
  //   _nodes[nx].freq_total_in.Print( fp );
  //   fprintf( fp, ", out=" );
  //   _nodes[nx].freq_total_out.Print( fp );
  //   fprintf( fp, ", wn=%0#x: %s\n", wn, buf);
}

void
FB_CFG::Print_edge( FILE *fp, FB_NODEX src_nx, FB_NODEX dst_nx ) const
{
  fprintf( fp, "Edge from %d to %d\n", src_nx, dst_nx );
}

void
FB_CFG::Print( FILE *fp ) const
{
  for ( FB_NODEX nx = 0; nx < _nodes.size(); ++nx ) {
    _nodes[nx].Print( fp, nx );
  }
}

class FB_Callback : public DaVinci_Callback {
private:
  const FB_CFG& _cfg;
public:
  FB_Callback( const FB_CFG& cfg ) : _cfg( cfg ) {}

  virtual void Node_Select( const INT n_ids, const NODE_ID id_array[] );
  virtual void Edge_Select( const EDGE_ID& edge_id );
};

void
FB_Callback::Node_Select( const INT n_ids, const NODE_ID id_array[] )
{
  for (INT i = 0; i < n_ids; ++i) {
    FB_NODEX nx = FB_NODEX(INTPS(id_array[i]));
    _cfg.Print_node( stderr, nx );
  }
}

void
FB_Callback::Edge_Select( const EDGE_ID& edge_id )
{
  FB_NODEX src_nx = FB_NODEX(INTPS(edge_id.src));
  FB_NODEX dst_nx = FB_NODEX(INTPS(edge_id.dst));
  _cfg.Print_edge( stderr, src_nx, dst_nx );
}

char *
FB_CFG::Node_label( FB_NODEX nx ) const
{
  static char  label[50];
  char        *cp = label;

  // display EDGE_TYPE and FB_NODEX number
  cp += sprintf( cp, "%d: ", nx );
  cp += FB_EDGE_TYPE_sprintf( cp, _nodes[nx].node_type );

  // incoming and outgoing frequencies
  cp += sprintf( cp, "\\nin  = " );
  cp += _nodes[nx].freq_total_in.Sprintf( cp );
  cp += sprintf( cp, "\\nout = " );
  cp += _nodes[nx].freq_total_out.Sprintf( cp );

  return label;
}

static BOOL
Node_Unbalanced (const FB_NODE& node,
		 const vector<FB_NODE, mempool_allocator<FB_NODE> >& _nodes)
{
  typedef vector<FB_NODEX>::const_iterator NODEX_ITER;
  
  // Compare freq_total_in with total of predecessor frequencies
  if ( node.one_edge_preds && node.unknown_in == 0 ) {
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for (NODEX_ITER i (node.preds.begin ()); i != node.preds.end (); ++i) {
      freq_total += _nodes[*i].freq_total_out;
    }
    if ( freq_total.Known () && node.freq_total_in != freq_total )
      return TRUE;
  }

  // Compare freq_total_out with total of successor frequencies
  if ( node.one_edge_succs && node.unknown_out == 0 ) {
    FB_FREQ freq_total = FB_FREQ_ZERO;
    for (NODEX_ITER i (node.succs.begin ()); i != node.succs.end (); ++i)
      freq_total += _nodes[*i].freq_total_in;
    if ( freq_total.Known() && node.freq_total_out != freq_total )
      return TRUE;
  }
  return FALSE;
} // Node_Unbalanced


void
FB_CFG::Draw() const
{
  NODE_TYPE  nt_unbalanced, nt_exact, nt_guess, nt_unknown, nt_change;
  EDGE_TYPE  et_advancing, et_delayed;
  FB_NODEX   nx, dst_nx;
  INT        t;

  // default color is black
  nt_unbalanced.Color ("light sky blue");
  nt_guess.Color(   "pink" );
  nt_unknown.Color( "orange" );
  nt_change.Color(  "light green" );
  et_delayed.Color( "blue" );

  DV->Graph_Begin();

  for ( nx = 0; nx < _nodes.size(); ++nx ) {
    const FB_NODE& node = _nodes[nx];
    // unbalanced, in/out different, exact, guess, or unknown node?
    FB_FREQ freq = node.freq_total_in + node.freq_total_out;
    NODE_TYPE *nt = &nt_exact;
    if ( Node_Unbalanced ( node, _nodes ) )  nt = &nt_unbalanced;
    else if ( ! node.in_out_same )           nt = &nt_change;
    else if ( freq.Guess() )                 nt = &nt_guess;
    else if ( ! freq.Known() )               nt = &nt_unknown;

    // Add the node and its outgoing edges to the daVinci graph;
    DV->Node_Begin( NODE_ID(INTPTR(nx)), Node_label(nx), *nt );
    for ( t = 0; t < node.undelayed_succs; ++t ) {
      dst_nx = node.succs[t];
      DV->Out_Edge( EDGE_ID(NODE_ID(INTPTR(nx)), NODE_ID(INTPTR(dst_nx))),
		    et_advancing, NODE_ID(INTPTR(dst_nx)) );
    }
    for ( ; t < node.succs.size(); ++t ) {
      dst_nx = node.succs[t];
      DV->Out_Edge( EDGE_ID(NODE_ID(INTPTR(nx)), NODE_ID(INTPTR(dst_nx))),
		    et_delayed, NODE_ID(INTPTR(dst_nx)) );
    }
    DV->Node_End();

  }
  DV->Graph_End();
}

void dV_view_fb_cfg( const FB_CFG& cfg, WN *root_wn, const char *caller )
{
  const char *trace_fname = getenv( "DV_TRACE_FILE" );
  FILE       *trace_fp    = NULL;
  const char *func_name   = "<unknown func>";
  char        title[100];

  if ( ! DaVinci::enabled( true ) ) return;

  if ( root_wn && WN_operator( root_wn ) == OPR_FUNC_ENTRY ) {
    func_name = ST_name( WN_entry_name( root_wn ) );
  }
  sprintf( title, "fb_whirl FB-CFG display: %s ", func_name );

//   if ( trace_fname && (trace_fp = fopen(trace_fname, "w")) == NULL ) {
//     fprintf(stderr, "DV_TRACE_FILE not writeable\n");
//     perror( trace_fname );
//   }
  FmtAssert( DV == NULL, ( "dV_view_fb_cfg: DV is null" ) );
  MEM_POOL_Initialize( &DV_fb_mempool, "DV_fb_mempool", FALSE );
  MEM_POOL_Push( &DV_fb_mempool );

  DV = CXX_NEW( DaVinci( &DV_fb_mempool, trace_fp ), &DV_fb_mempool );

  DV->Title( title );
  if ( caller ) DV->Show_Status( caller );
  cfg.Draw();

  FB_Callback callback( cfg );
  DV->Event_Loop( &callback );

  CXX_DELETE( DV, &DV_fb_mempool );
  DV = NULL;

  MEM_POOL_Pop( &DV_fb_mempool );
  MEM_POOL_Delete( &DV_fb_mempool );

  if ( trace_fp ) (void)fclose( trace_fp );
}

// ====================================================================

// bool
// FEEDBACK::Verify(const char *caller, bool abort_if_error) const
// {
//   if ( this == NULL ) return TRUE;
  
//   if ( _trace ) {
//     fprintf(TFile, "\n===== FEEDBACK::Verify (%s)\n", caller);
//   }

//   FB_CFG   fb_cfg;
//   bool     is_ok = fb_cfg.Construct_from_whirl(_root_wn);
//   FB_NODEX nx;
//   FB_EDGEX ex;
  
//   for (nx = cfg.Get_Vertex(); nx; nx = cfg.Get_Next_Vertex(nx)) {
//     INT32   n_in    = 0;
//     INT32   n_out   = 0;
//     FB_FREQ cnt_in  = 0;
//     FB_FREQ cnt_out = 0;
//     FB_FREQ freq;

//     for (ex = cfg.Get_In_Edge(nx); ex; ex = cfg.Get_Next_In_Edge(ex)) {
//       freq = cfg.Edge(ex).freq;

//       if ( _trace ) {
// 	fprintf(TFile, " ex=%-9d - In_Edge freq: %lld ",
// 		ex, (INT64) freq);
// 	WN *wn = _nodes[nx].source;
// 	fdump_wn(TFile, wn);
//       }
      
//       if ( ! freq.Known() ) {
// 	is_ok = FALSE;
//       }
//       cnt_in += freq;
//       n_in   += 1;
//     }
//     for (ex = cfg.Get_Out_Edge(nx); ex; ex = cfg.Get_Next_Out_Edge(ex)) {
//       freq = cfg.Edge(ex).freq;

//       if ( _trace ) {
// 	fprintf(TFile, " ex=%-8d - Out_Edge freq: %lld  ",
// 		ex, (INT64) freq);
// 	fb_cfg.Print_node(nx, true);
//       }
      
//       if ( freq == FB_FREQ_UNINIT ) {
// 	is_ok = FALSE;
//       }
//       cnt_out += freq;
//       n_out   += 1;
//     }
//     if ( n_in > 0 && n_out > 0 && cnt_in != cnt_out ) {
// #ifdef WANT_FEEDBACK_WARNINGS
//       fprintf(stderr, "FEEDBACK::verify(root_wn=%0#x) ERROR\n", _root_wn);
//       fprintf(stderr, "   in/out=%d/%d; cnt=%lld/%lld\n",
// 	      n_in, n_out, cnt_in, cnt_out);
//       fb_cfg.Print_node(nx, true);
//       is_ok = FALSE;
// #endif
//     }
//   }
// #ifdef WANT_FEEDBACK_WARNINGS
//   if ( ! is_ok )
//     fb_cfg.dV_view_fb_cfg( _root_wn, caller );
// #endif
//   if ( _trace_draw )
//     fb_cfg.dV_view_fb_cfg( _root_wn, caller );
//   return is_ok;
// }
