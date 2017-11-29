/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
// Module: fb_whirl.cxx
// $Revision: 1.28 $
// $Date: 06/01/13 15:35:23-08:00 $
// $Author: gautam@jacinth.keyresearch $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.fb_whirl.cxx $
//
// Description:
//
// Code to read feedback file and annotate WHIRL tree with the
// frequency information found there.
//
// Revision history:
// 960321 Initial Revision (rkennedy)
// 960425 Major changes for new .cfb format with column
//	  numbers (rkennedy)
// 980228  if be -tt16:4 then call dV_show_whirl() after
//            feedback info is added (gwe).
// 9803    Initial hooks for new (edge) feedback.
// 9804    More work on new feedback; esp. annotation/verification.
// 9806    Through May and June restructure to include support for
//         FEEDBACK during CG.
// ======================================================================
// ======================================================================

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#define USE_STANDARD_TYPES

#include <stdint.h>
#include <cmplrs/fb.h>
#include <stdlib.h>
#include "wn_util.h"
#include "wn_map.h"
#include "errors.h"		// for ErrMsg
#include "ir_reader.h"
#include "tracing.h"
#include "wn_tree_util.h"	// for tree iterators

#include "fb_whirl.h"
#include "fb_cfg.h"

#include "com_whirlview.h"

#include "cxx_graph.h"

#include "DaVinci.h"      // for DaVinci viewer (for FB CFG).
#include "wb_util.h"      // more: move this to another file (gwe).

// ====================================================================

// Delete this.
void
FB_old_Annotate_whirl(WN *wn)
{
  return;
}

// ====================================================================
ADDRESS_NAME_MAP PU_Addr_Name_Map;
ADDRESS_PUSIZE_MAP PU_Addr_Pusize_Map;

#ifdef KEY
#define IS_VALID_DIV_VALUE(wn)                           \
  ( !WN_operator_is( WN_kid1(wn), OPR_INTCONST ) &&	 \
    MTYPE_is_integral( OPCODE_rtype(WN_opcode(wn) ) ) )

#define IS_VALID_MPY_VALUE(wn)                           \
  ( !WN_operator_is( WN_kid1(wn), OPR_CONST ) &&	 \
    !MTYPE_is_integral( OPCODE_rtype(WN_opcode(wn) ) ) )
#endif


// ====================================================================
void
FEEDBACK::FB_hoist_case( WN *wn_switch, vector<FB_FREQ>::size_type wcase)
{
   FB_Info_Switch info_switch = Query_switch( wn_switch );
   FB_FREQ freq_taken = info_switch[wcase];
   info_switch[wcase] = FB_FREQ_ZERO;
   Annot_switch( wn_switch, info_switch );
}


FEEDBACK *Cur_PU_Feedback = NULL;

FEEDBACK::FEEDBACK( WN *wn, MEM_POOL *m,
		    INT32 invoke_size,
		    INT32 branch_size,
		    INT32 loop_size,
		    INT32 circuit_size,
		    INT32 call_size,
		    INT32 icall_size,
		    INT32 switch_size, 
#ifdef KEY
		    INT32 value_size,
		    INT32 value_fp_bin_size,
		    UINT64 runtime_fun_address,
#endif
		    WN_MAP_TAB *maptab ) :
  _m( m ),
  _maptab( maptab ),
  _root_wn( wn ),
  _trace(      Get_Trace( TP_FEEDBACK, TP_FEEDBACK_WN ) ),
  _trace_draw( Get_Trace( TP_FEEDBACK, TP_FEEDBACK_WN_DRAW ) ),
  // note: skiping first entry so the 0 index yields FB_FREQ_UNINIT.
  _invokes ( 1, FB_Info_Invoke(),  m ),
  _branches( 1, FB_Info_Branch(),  m ),
  _loops   ( 1, FB_Info_Loop(),    m ),
  _circuits( 1, FB_Info_Circuit(), m ),
  _calls   ( 1, FB_Info_Call(),    m ),
  _icalls  ( 1, FB_Info_Icall(),   m ),
#ifdef KEY
  _values  ( 1, FB_Info_Value(),   m ),
  _values_fp_bin  ( 1, FB_Info_Value_FP_Bin(),   m ),
  _runtime_func_addr( runtime_fun_address ),
#endif
  _switches( 1, FB_Info_Switch(),  m ),
  _last_wn_icall( 0 )
{
  if ( _trace )
    fprintf( TFile, "==================================================\n"
	     "Constructing FEEDBACK for " );

  _invokes.reserve (invoke_size);
  _branches.reserve (branch_size);
  _loops.reserve (loop_size);
  _circuits.reserve (circuit_size);
  _calls.reserve (call_size);
  _icalls.reserve (icall_size);
  _switches.reserve (switch_size);
#ifdef KEY
  _values.reserve (value_size);
  _values_fp_bin.reserve (value_fp_bin_size);
#endif
  
  OPERATOR opr = WN_operator( wn );

  FmtAssert( opr == OPR_FUNC_ENTRY || opr == OPR_REGION,
	     ( "FEEDBACK constructor: unexpected opr %d", opr ) );

  if ( _trace ) {
    if ( opr == OPR_FUNC_ENTRY )
      fprintf( TFile, "FUNC_ENTRY %s", ST_name( WN_entry_name( wn ) ) );
    else
      fprintf( TFile, "REGION" );
    fprintf( TFile, "\n==================================================\n" );
  }

  if ( _trace_draw ) {
    dV_view_whirl( wn );
  }
}

// ====================================================================
// Get_index_* functions retrieve index into vectors of feedback data
// ====================================================================

INT32
FEEDBACK::Get_index_invoke( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_invoke expects non-NULL wn" ) );
  Is_True( FB_valid_opr_invoke( wn ),
	   ( "FEEDBACK::Get_index_invoke found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _invokes.size(),
	   ( "FEEDBACK::Get_index_invoke found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_branch( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_branch expects non-NULL wn" ) );
  Is_True( FB_valid_opr_branch( wn ),
	   ( "FEEDBACK::Get_index_branch found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _branches.size(),
	   ( "FEEDBACK::Get_index_branch found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_loop( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_loop expects non-NULL wn" ) );
  Is_True( FB_valid_opr_loop( wn ),
	   ( "FEEDBACK::Get_index_loop found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _loops.size(),
	  ( "FEEDBACK::Get_index_loop found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_circuit( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_circuit expects non-NULL wn" ) );
  Is_True( FB_valid_opr_circuit( wn ),
	   ( "FEEDBACK::Get_index_circuit found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _circuits.size(),
	  ( "FEEDBACK::Get_index_circuit found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_call( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_call expects non-NULL wn" ) );
  Is_True( FB_valid_opr_call( wn ),
	   ( "FEEDBACK::Get_index_call found unexpected operator" ) );

  //For those WN* <wn> whose Operator is OPR_ICALL,
  // to make IPA_WN_MAP32_Get and IPA_WN_MAP32_Set work,
  // we choose to map OPR_ICALL twice:
  //   first map to OPERATOR_MAPCAT_CALL, 
  //   then map to OPERATOR_MAPCAT_ICALL
  // To do this, we change the operator of wn to OPR_CALL in Annot_call temporarily,
  // we will immediately restore the correct operator(OPR_ICALL) after Add_index_call.
  
  WN * wn_notconst = ( WN * ) wn;
  OPERATOR old_opr = WN_operator(wn_notconst);
  if (old_opr == OPR_ICALL)
  {
    WN_set_operator(wn_notconst, OPR_CALL);
  }
   
  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn_notconst );
  Is_True( fb_index >= 0 && fb_index < _calls.size(),
	   ( "FEEDBACK::Get_index_call found out of range fb_index" ) );
 
  Is_True(WN_operator(wn_notconst) != OPR_ICALL,("WN operator must not be icall! icall has been changed to call."));
  if (old_opr == OPR_ICALL)
  {
    WN_set_operator(wn_notconst, old_opr);
  }

  return fb_index;
}

INT32
FEEDBACK::Get_index_icall( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_icall expects non-NULL wn" ) );
  Is_True( FB_valid_opr_call( wn ),
	   ( "FEEDBACK::Get_index_icall found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn ); 

#ifdef KEY
  Is_True( fb_index >= 0 && fb_index < _icalls.size(),
	   ( "FEEDBACK::Get_index_icall found out of range fb_index" ) );
#endif

  if (fb_index >= _icalls.size())
  {
	DevWarn("FEEDBACK::Get_index_icall found out of range fb_index, suspect it is because call index.");
	fb_index = 0;
  }
  Is_True( fb_index >= 0 && fb_index < _icalls.size(),
	   ( "FEEDBACK::Get_index_icall found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_switch( const WN *wn ) const
{
  Is_True( wn != NULL,
	  ( "FEEDBACK::Get_index_switch expects non-NULL wn" ) );
  Is_True( FB_valid_opr_switch( wn ),
	  ( "FEEDBACK::Get_index_switch found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _switches.size(),
	   ( "FEEDBACK::Get_index_switch found out of range fb_index" ) );
  return fb_index;
}

// ====================================================================
// Add_index_* functions retrieve index into vectors of feedback data,
//   adding a new slot for feedback data if necessary
// ====================================================================

INT32
FEEDBACK::Add_index_invoke( WN *wn )
{
  INT32 fb_index = Get_index_invoke( wn );
  Is_True( fb_index >= 0 && fb_index < _invokes.size(),
	   ( "FEEDBACK::Add_index_invoke found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _invokes.size();
    _invokes.push_back(FB_Info_Invoke());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_branch( WN *wn )
{
  INT32 fb_index = Get_index_branch( wn );
  Is_True( fb_index >= 0 && fb_index < _branches.size(),
	  ( "FEEDBACK::Add_index_branch found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _branches.size();
    _branches.push_back(FB_Info_Branch());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_loop( WN *wn )
{
  INT32 fb_index = Get_index_loop( wn );
  Is_True( fb_index >= 0 && fb_index < _loops.size(),
	  ( "FEEDBACK::Add_index_loop found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _loops.size();
    _loops.push_back(FB_Info_Loop());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_circuit( WN *wn )
{
  INT32 fb_index = Get_index_circuit( wn );
  Is_True( fb_index >= 0 && fb_index < _circuits.size(),
	  ( "FEEDBACK::Add_index_circuit found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _circuits.size();
    _circuits.push_back(FB_Info_Circuit());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_call( WN *wn )
{
  //For those WN* <wn> whose Operator is OPR_ICALL,
  // to make IPA_WN_MAP32_Get and IPA_WN_MAP32_Set work,
  // we choose to map OPR_ICALL twice:
  //   first map to OPERATOR_MAPCAT_CALL, 
  //   then map to OPERATOR_MAPCAT_ICALL
  // To do this, we change the operator of wn to OPR_CALL in Annot_call temporarily,
  // we will immediately restore the correct operator(OPR_ICALL) after Add_index_call.
  
  OPERATOR old_opr = WN_operator(wn);
  if (old_opr == OPR_ICALL)
  {
    WN_set_operator(wn, OPR_CALL);
  }

  INT32 fb_index = Get_index_call( wn );
  Is_True( fb_index >= 0 && fb_index < _calls.size(),
	   ( "FEEDBACK::Add_index_call found out of range fb_index" ) );

  if (fb_index == 0) {
    fb_index = _calls.size();
    _calls.push_back(FB_Info_Call());
#ifdef KEY
    _icalls.push_back(FB_Info_Icall());
#endif
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }

  Is_True(WN_operator(wn) != OPR_ICALL,("WN operator must not be icall! icall has been changed to call."));
  if (old_opr == OPR_ICALL)
  {
	WN_set_operator(wn, old_opr);
  }
   
  return fb_index;
}

INT32
FEEDBACK::Add_index_icall( WN *wn )
{
  INT32 fb_index = Get_index_icall( wn );
  Is_True( fb_index >= 0 && fb_index < _icalls.size(),
	   ( "FEEDBACK::Add_index_icall found out of range fb_index" ) );

  if (fb_index == 0) {
    fb_index = _icalls.size();
    _icalls.push_back(FB_Info_Icall());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_switch( WN *wn )
{
  INT32 fb_index = Get_index_switch( wn );
  Is_True( fb_index >= 0 && fb_index < _switches.size(),
	   ( "FEEDBACK::Add_index_switch found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _switches.size();
    _switches.push_back(FB_Info_Switch());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

// ====================================================================
// Are the total incoming and outgoing frequencies known to be equal?
// ====================================================================

bool
FEEDBACK::Same_in_out( const WN *wn ) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  case OPR_FUNC_ENTRY: case OPR_ALTENTRY:
  case OPR_RETURN: case OPR_RETURN_VAL:
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
    return false;

  fb_opr_cases_call:
    {
      INT32 fb_index = Get_index_call( wn );
      return _calls[fb_index].in_out_same;
    }

  default:
    break;
  }
  return true;
}


void
FEEDBACK::FB_set_in_out_same_node( WN *wn )
{
  // Set in_out_same true for all calls
  if ( FB_valid_opr_call( wn ) ) {

    if ( _trace )
      fprintf( TFile, "FEEDBACK::FB_set_in_out_same_node(0x%p):\n", wn );

    FB_Info_Call info_call = Query_call( wn );
    info_call.in_out_same = true;
    Annot_call( wn, info_call );
  }
}


void
FEEDBACK::FB_set_in_out_same( WN *wn )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_set_in_out_same(0x%p):\n", wn );

  for ( TREE_ITER iter( wn ); iter.Wn() != NULL; ++iter )
    FB_set_in_out_same_node( iter.Wn() );
}

#if defined(TARG_SL) && defined(TARG_SL2)
void
FEEDBACK::FB_reset_in_out_same_node( WN *wn )
{
  // Set in_out_same true for all calls
  if ( FB_valid_opr_call( wn ) ) {

    if ( _trace )
      fprintf( TFile, "FEEDBACK::FB_set_in_out_same_node(0x%p):\n", wn );

    FB_Info_Call info_call = Query_call( wn );
    info_call.in_out_same = false;
    Annot_call( wn, info_call );
  }
}
#endif

// ====================================================================
// Print function displays feedback info for tracing purposes
// ====================================================================

void
FEEDBACK::Print( FILE *fp, const WN *wn ) const
{
  if ( wn == NULL ) return;
  INT32 fb_index;
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    fb_index = Get_index_invoke( wn );
    _invokes[fb_index].Print( fp );
    break;

  fb_opr_cases_branch:
    fb_index = Get_index_branch( wn );
    _branches[fb_index].Print( fp );
    break;

  fb_opr_cases_loop:
    fb_index = Get_index_loop( wn );
    _loops[fb_index].Print( fp );
    break;

  fb_opr_cases_circuit:
    fb_index = Get_index_circuit( wn );
    _circuits[fb_index].Print( fp );
    break;

  fb_opr_cases_call:
    fb_index = Get_index_call( wn );
    _calls[fb_index].Print( fp );
#ifdef KEY
    if( WN_operator(wn) == OPR_ICALL ){
      fb_index = Get_index_icall( wn );
      _icalls[fb_index].Print( fp );
    }
#endif
    break;

  fb_opr_cases_switch:
    fb_index = Get_index_switch( wn );
    _switches[fb_index].Print( fp );
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn ) ){
      fb_index = Get_index_value( wn );
      _values[fb_index].Print( fp );
    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn ) ){
      fb_index = Get_index_value_fp_bin( wn );
      _values_fp_bin[fb_index].Print( fp );
    }
    break;
#endif

  default:
    break;
  }
}

void
FEEDBACK::Print_with_wn( FILE *fp, WN *wn ) const
{
  Print( fp, wn );
  fprintf( fp, "\nfor " );
  fdump_wn( TFile, wn );
}

// ====================================================================
// Query_* functions retrieve feedback data or FB_FREQ_UNINIT
// ====================================================================

const FB_Info_Invoke&
FEEDBACK::Query_invoke( const WN *wn ) const
{
  INT32 fb_index = Get_index_invoke( wn );
  return _invokes[fb_index];
}

const FB_Info_Branch&
FEEDBACK::Query_branch( const WN *wn ) const
{
  INT32 fb_index = Get_index_branch( wn );
  return _branches[fb_index];
}

const FB_Info_Loop&
FEEDBACK::Query_loop( const WN *wn ) const
{
  INT32 fb_index = Get_index_loop( wn );
  return _loops[fb_index];
}

const FB_Info_Circuit&
FEEDBACK::Query_circuit( const WN *wn ) const
{
  INT32 fb_index = Get_index_circuit( wn );
  return _circuits[fb_index];
}

const FB_Info_Call&
FEEDBACK::Query_call( const WN *wn ) const
{
  INT32 fb_index = Get_index_call( wn );
  return _calls[fb_index];
}

const FB_Info_Icall&
FEEDBACK::Query_icall( const WN *wn ) const
{
  INT32 fb_index = Get_index_icall( wn );
  return _icalls[fb_index];
}

const FB_Info_Switch&
FEEDBACK::Query_switch( const WN *wn ) const
{
  INT32 fb_index = Get_index_switch( wn );
  return _switches[fb_index];
}

#ifdef KEY
const FB_Info_Value&
FEEDBACK::Query_value( const WN *wn ) const
{
  if (! IS_VALID_DIV_VALUE(wn))
    return _values[0];
  INT32 fb_index = Get_index_value( wn );
  return _values[fb_index];
}

const FB_Info_Value_FP_Bin&
FEEDBACK::Query_value_fp_bin( const WN *wn ) const
{
  if (! IS_VALID_MPY_VALUE(wn))
    return _values_fp_bin[0];
  INT32 fb_index = Get_index_value_fp_bin( wn );
  return _values_fp_bin[fb_index];
}
#endif


FB_FREQ
FEEDBACK::Query( const WN *wn, const FB_EDGE_TYPE type ) const
{
  INT32 fb_index;
  OPERATOR opr = WN_operator( wn );
  FB_FREQ freq = FB_FREQ_UNINIT;

  switch( type ) {

  case FB_EDGE_UNINIT:
    Is_True( false, ( "FEEDBACK::Query found edge type FB_EDGE_UNINT" ) );
    break;

  case FB_EDGE_INCOMING:
  case FB_EDGE_OUTGOING:
  case FB_EDGE_ENTRY_OUTGOING:
    fb_index = Get_index_invoke( wn );
    freq     = _invokes[fb_index].freq_invoke;
    break;

  case FB_EDGE_BRANCH_TAKEN:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].freq_taken;
    break;

  case FB_EDGE_BRANCH_NOT_TAKEN:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].freq_not_taken;
    break;

  case FB_EDGE_LOOP_ZERO:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_zero;
    break;

  case FB_EDGE_LOOP_POSITIVE:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_positive;
    break;

  case FB_EDGE_LOOP_OUT:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_out;
    break;

  case FB_EDGE_LOOP_BACK:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_back;
    break;

  case FB_EDGE_LOOP_EXIT:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_exit;
    break;

  case FB_EDGE_LOOP_ITERATE:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_iterate;
    break;

  case FB_EDGE_CIRCUIT_LEFT:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_left;
    break;

  case FB_EDGE_CIRCUIT_RIGHT:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_right;
    break;

  case FB_EDGE_CIRCUIT_NEITHER:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_neither;
    break;

  case FB_EDGE_CALL_INCOMING:
    fb_index = Get_index_call( wn );
    freq     = _calls[fb_index].freq_entry;
    break;

  case FB_EDGE_CALL_OUTGOING:
  case FB_EDGE_IO_OUTGOING:
    fb_index = Get_index_call( wn );
    freq     = _calls[fb_index].freq_exit;
    break;

  case FB_EDGE_CALL_INOUTSAME:
    fb_index = Get_index_call( wn );
    Is_True( _calls[fb_index].in_out_same,
	     ( "FEEDBACK::Query found in_out_same discrepancy" ) );
    freq     = _calls[fb_index].freq_exit;
    break;

  case FB_EDGE_SWITCH_DEFAULT:
    fb_index = Get_index_switch( wn );
    freq     = _switches[fb_index][0];
    break;

  default:
    if ( type >= FB_EDGE_SWITCH_BASE ) {

      // case FB_EDGE_SWITCH_BASE:
      fb_index = Get_index_switch( wn );
      const INT32 index_target = FB_EDGE_SWITCH_INDEX( type );
      freq = _switches[fb_index][index_target];

    } else if ( type >= FB_EDGE_IO_ESCAPE_BASE &&
		type <  FB_EDGE_IO_ESCAPE_BASE + FB_IO_ESCAPE_EDGES_MAX ) {

      // case FB_EDGE_IO_ESCAPE_BASE:
      fb_index = Get_index_call( wn );
      freq = ( _calls[fb_index].in_out_same ? FB_FREQ_ZERO : FB_FREQ_UNKNOWN );

    } else
      Is_True( false, ( "FEEDBACK::Query encounted unexpected edge type" ) );

    break;
  }

  if ( _trace ) {
    char buffer[FB_EDGE_TYPE_NAME_LENGTH];
    FB_EDGE_TYPE_sprintf( buffer, type );

    fprintf( TFile, "FEEDBACK::Query(0x%p, %s) == ", wn, buffer );
    freq.Print( TFile );
    fprintf( TFile, "\n" );
  }
  return freq;
}


FB_FREQ
FEEDBACK::Query_prob( const WN *wn, const FB_EDGE_TYPE type ) const
{
  INT32 fb_index;
  OPERATOR opr = WN_operator( wn );
  FB_FREQ freq = FB_FREQ_UNINIT, total = FB_FREQ_UNINIT;

  switch( type ) {

  case FB_EDGE_UNINIT:
    Is_True( false,
	     ( "FEEDBACK::Query_prob found edge type FB_EDGE_UNINT" ) );
    break;

  case FB_EDGE_INCOMING:
  case FB_EDGE_OUTGOING:
  case FB_EDGE_ENTRY_OUTGOING:
    fb_index = Get_index_invoke( wn );
    freq     = _invokes[fb_index].freq_invoke;
    total    = freq;
    break;

  case FB_EDGE_BRANCH_TAKEN:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].freq_taken;
    total    = _branches[fb_index].Total();
    break;

  case FB_EDGE_BRANCH_NOT_TAKEN:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].freq_not_taken;
    total    = _branches[fb_index].Total();
    break;

  case FB_EDGE_LOOP_ZERO:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_zero;
    total    = _loops[fb_index].freq_exit;
    break;

  case FB_EDGE_LOOP_POSITIVE:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_positive;
    total    = _loops[fb_index].freq_iterate;
    break;

  case FB_EDGE_LOOP_OUT:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_out;
    total    = _loops[fb_index].freq_exit;
    break;

  case FB_EDGE_LOOP_BACK:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_back;
    total    = _loops[fb_index].freq_iterate;
    break;

  case FB_EDGE_LOOP_EXIT:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_exit;
    total    = _loops[fb_index].Total();
    break;

  case FB_EDGE_LOOP_ITERATE:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_iterate;
    total    = _loops[fb_index].Total();
    break;

  case FB_EDGE_CIRCUIT_LEFT:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_left;
    total    = _circuits[fb_index].Total();
    break;

  case FB_EDGE_CIRCUIT_RIGHT:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_right;
    total    = _circuits[fb_index].Total();
    break;

  case FB_EDGE_CIRCUIT_NEITHER:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_neither;
    total    = _circuits[fb_index].Total();
    break;

  case FB_EDGE_CALL_INCOMING:
  case FB_EDGE_CALL_OUTGOING:
  case FB_EDGE_CALL_INOUTSAME:
  case FB_EDGE_IO_OUTGOING:
    fb_index = Get_index_call( wn );
    freq     = _calls[fb_index].freq_exit;
    total    = _calls[fb_index].freq_entry;
    break;

  case FB_EDGE_SWITCH_DEFAULT:
    fb_index = Get_index_switch( wn );
    freq     = _switches[fb_index][0];
    total    = _switches[fb_index].Total();
    break;

  default:
    if ( type >= FB_EDGE_SWITCH_BASE ) {

      // case FB_EDGE_SWITCH_BASE:
      fb_index = Get_index_switch( wn );
      const INT32 index_target = FB_EDGE_SWITCH_INDEX( type );
      freq  = _switches[fb_index][index_target];
      total = _switches[fb_index].Total();

    } else if ( type >= FB_EDGE_IO_ESCAPE_BASE &&
		type <  FB_EDGE_IO_ESCAPE_BASE + FB_IO_ESCAPE_EDGES_MAX ) {

      // case FB_EDGE_IO_ESCAPE_BASE:
      fb_index = Get_index_call( wn );
      freq  = ( _calls[fb_index].in_out_same
		? FB_FREQ_ZERO : FB_FREQ_UNKNOWN );
      total = _calls[fb_index].freq_entry;

    } else
      Is_True( false, ( "FEEDBACK::Query encounted unexpected edge type" ) );

    break;
  }

  if ( _trace ) {
    char buffer[FB_EDGE_TYPE_NAME_LENGTH];
    FB_EDGE_TYPE_sprintf( buffer, type );

    fprintf( TFile, "FEEDBACK::Query_prob(0x%p, %s) == ", wn, buffer );
    freq.Print( TFile );
    fprintf( TFile, ", total = " );
    total.Print( TFile );
    fprintf(TFile, "\n" );
  }
  return freq / total;
}


FB_FREQ
FEEDBACK::Query_total_out( const WN *wn ) const
{
  Is_True( wn != NULL, ("FEEDBACK::Query_total_out expects non-NULL wn") );

  INT32 fb_index;
  OPERATOR opr = WN_operator( wn );
  FB_FREQ freq = FB_FREQ_UNINIT;

  switch( opr ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    if ( opr == OPR_RETURN || opr == OPR_RETURN_VAL
#ifdef KEY
  	 || opr == OPR_GOTO_OUTER_BLOCK
#endif
       )
      freq     = FB_FREQ_ZERO;
    else {
      fb_index = Get_index_invoke( wn );
      freq     = _invokes[fb_index].freq_invoke;
    }
    break;

  fb_opr_cases_branch:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].Total();
    break;

  fb_opr_cases_loop:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_exit;
    break;

  fb_opr_cases_circuit:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].Total();
    break;

  fb_opr_cases_call:
    fb_index = Get_index_call( wn );
    freq     = ( opr == OPR_IO
		 ? _calls[fb_index].freq_entry
		 : _calls[fb_index].freq_exit  );
    break;

  fb_opr_cases_switch:
    fb_index = Get_index_switch( wn );
    freq     = _switches[fb_index].Total();
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn ) ){
      fb_index = Get_index_value( wn );
      freq     = _values[fb_index].Total();
    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn ) ){
      fb_index = Get_index_value_fp_bin( wn );
      freq     = _values_fp_bin[fb_index].Total();
    }
    break;
#endif

  default:
    break;
  }

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Query_total_out(0x%p: %s) == ",
	     wn, OPERATOR_name(opr) );
    freq.Print( TFile );
    fprintf( TFile, "\n" );
  }
  return freq;
}


// ====================================================================
// Annot_* functions store feedback data
// ====================================================================

void
FEEDBACK::Annot_invoke( WN *wn, const FB_Info_Invoke& fb_info )
{
  INT32 fb_index = Add_index_invoke( wn );
  _invokes[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_invoke(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_branch( WN *wn, const FB_Info_Branch& fb_info )
{
  INT32 fb_index = Add_index_branch( wn );
  _branches[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_branch(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

#ifdef KEY
INT32 FEEDBACK::Get_index_value_fp_bin( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_invoke expects non-NULL wn" ) );
  Is_True( FB_valid_opr_value( wn ),
	   ( "FEEDBACK::Get_index_invoke found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _values_fp_bin.size(),
	   ( "FEEDBACK::Get_index_invoke found out of range fb_index" ) );
  return fb_index;
}

INT32 FEEDBACK::Add_index_value_fp_bin( WN *wn )
{
  INT32 fb_index = Get_index_value_fp_bin( wn );
  Is_True( fb_index >= 0 && fb_index < _values_fp_bin.size(),
	   ( "FEEDBACK::Add_index_value_fp_bin found out of range fb_index" ));

  if ( fb_index == 0 ) {
    fb_index = _values_fp_bin.size();
    _values_fp_bin.push_back(FB_Info_Value_FP_Bin());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

void FEEDBACK::Annot_value_fp_bin( WN *wn, 
				   const FB_Info_Value_FP_Bin& fb_info )
{
  INT32 fb_index = Add_index_value_fp_bin( wn );
  _values_fp_bin[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_value_fp_bin(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

INT32 FEEDBACK::Get_index_value( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_invoke expects non-NULL wn" ) );
  Is_True( FB_valid_opr_value( wn ),
	   ( "FEEDBACK::Get_index_invoke found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _values.size(),
	   ( "FEEDBACK::Get_index_invoke found out of range fb_index" ) );
  return fb_index;
}

INT32 FEEDBACK::Add_index_value( WN *wn )
{
  INT32 fb_index = Get_index_value( wn );
  Is_True( fb_index >= 0 && fb_index < _values.size(),
	   ( "FEEDBACK::Add_index_value found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _values.size();
    _values.push_back(FB_Info_Value());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

void FEEDBACK::Annot_value( WN *wn, const FB_Info_Value& fb_info )
{
  INT32 fb_index = Add_index_value( wn );
  _values[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_value(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}
#endif


void
FEEDBACK::Annot_loop( WN *wn, const FB_Info_Loop& fb_info )
{
  INT32 fb_index = Add_index_loop( wn );
  _loops[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_loop(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_circuit( WN *wn, const FB_Info_Circuit& fb_info )
{
  INT32 fb_index = Add_index_circuit( wn );
  _circuits[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_circuit(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_call( WN *wn, const FB_Info_Call& fb_info )
{
  INT32 fb_index = Add_index_call( wn );
  _calls[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_call(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_icall( WN *wn, const FB_Info_Icall& fb_info )
{
  INT32 fb_index = Add_index_icall( wn );
  _icalls[fb_index] = fb_info;


  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_icall(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_switch( WN *wn, const FB_Info_Switch& fb_info )
{
  INT32 fb_index = Add_index_switch( wn );
  _switches[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_switch(0x%p):\n", wn );
    Print_with_wn( TFile, wn );
  }
}


void
FEEDBACK::Annot( WN *wn, FB_EDGE_TYPE type, FB_FREQ freq )
{
  INT32 fb_index;

  switch(type) {

  case FB_EDGE_UNINIT:
    Is_True( false, ( "FEEDBACK::Annot found edge type FB_EDGE_UNINIT" ) );
    break;

  case FB_EDGE_INCOMING:
  case FB_EDGE_OUTGOING:
  case FB_EDGE_ENTRY_OUTGOING:
    fb_index = Add_index_invoke( wn );
    _invokes[fb_index].freq_invoke = freq;
    break;

  case FB_EDGE_BRANCH_TAKEN:
    fb_index = Add_index_branch( wn );
    _branches[fb_index].freq_taken = freq;
    break;

  case FB_EDGE_BRANCH_NOT_TAKEN:
    fb_index = Add_index_branch( wn );
    _branches[fb_index].freq_not_taken = freq;
    break;

  case FB_EDGE_LOOP_ZERO:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_zero = freq;
      if ( ! freq.Known() || loop.freq_out.Known() )
	loop.freq_exit = freq + loop.freq_out;
      else if ( loop.freq_exit.Known() )
	loop.freq_out = loop.freq_exit - freq;
    }
    break;

  case FB_EDGE_LOOP_POSITIVE:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_positive = freq;
      if ( ! freq.Known() || loop.freq_back.Known() )
	loop.freq_iterate = freq + loop.freq_back;
      else if ( loop.freq_iterate.Known() )
	loop.freq_back = loop.freq_iterate - freq;
    }
    break;

  case FB_EDGE_LOOP_OUT:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_out = freq;
      if ( ! freq.Known() || loop.freq_zero.Known() )
	loop.freq_exit = freq + loop.freq_zero;
      else if ( loop.freq_exit.Known() )
	loop.freq_zero = loop.freq_exit - freq;
    }
    break;

  case FB_EDGE_LOOP_BACK:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_back = freq;
      if ( ! freq.Known() || loop.freq_positive.Known() )
	loop.freq_iterate = freq + loop.freq_positive;
      else if ( loop.freq_iterate.Known() )
	loop.freq_positive = loop.freq_iterate - freq;
    }
    break;

  case FB_EDGE_LOOP_EXIT:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_exit = freq;
      if ( ! freq.Known() )
	loop.freq_zero = loop.freq_out = freq;
      else if ( loop.freq_zero.Known() )
	if ( ! loop.freq_out.Known() )
	  loop.freq_out = freq - loop.freq_zero;
	else if ( loop.freq_out.Known() )
	  loop.freq_zero = freq - loop.freq_out;
    }
    break;

  case FB_EDGE_LOOP_ITERATE:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_iterate = freq;
      if ( ! freq.Known() )
	loop.freq_positive = loop.freq_back = freq;
      else if ( loop.freq_positive.Known() )
	if ( ! loop.freq_back.Known() )
	  loop.freq_back = freq - loop.freq_positive;
	else if ( loop.freq_back.Known() )
	  loop.freq_positive = freq - loop.freq_back;
    }
    break;

  case FB_EDGE_CIRCUIT_LEFT:
    fb_index = Add_index_circuit( wn );
    _circuits[fb_index].freq_left = freq;
    break;

  case FB_EDGE_CIRCUIT_RIGHT:
    fb_index = Add_index_circuit( wn );
    _circuits[fb_index].freq_right = freq;
    break;

  case FB_EDGE_CIRCUIT_NEITHER:
    fb_index = Add_index_circuit( wn );
    _circuits[fb_index].freq_neither = freq;
    break;

  case FB_EDGE_CALL_INCOMING:
    {
      fb_index = Add_index_call( wn );
      FB_Info_Call& fb_info = _calls[fb_index];
      fb_info.freq_entry = freq;

      if ( ! fb_info.in_out_same &&
	   fb_info.freq_entry.Exact() &&
	   fb_info.freq_exit.Exact() &&
	   fb_info.freq_entry == fb_info.freq_exit )
	fb_info.in_out_same = true;
    }
    break;

  case FB_EDGE_CALL_OUTGOING:
  case FB_EDGE_IO_OUTGOING:
    {
      fb_index = Add_index_call( wn );
      FB_Info_Call& fb_info = _calls[fb_index];
      fb_info.freq_exit = freq;

      if ( ! fb_info.in_out_same &&
	   fb_info.freq_entry.Exact() &&
	   fb_info.freq_exit.Exact() &&
	   fb_info.freq_entry == fb_info.freq_exit )
	fb_info.in_out_same = true;
    }
    break;

  case FB_EDGE_CALL_INOUTSAME:
    fb_index = Add_index_call( wn );
    _calls[fb_index].in_out_same = true;
    _calls[fb_index].freq_entry = freq;
    _calls[fb_index].freq_exit  = freq;
    break;

  case FB_EDGE_SWITCH_DEFAULT:
    fb_index = Add_index_switch( wn );
    _switches[fb_index][0] = freq;
    break;

  default:
    if ( type >= FB_EDGE_SWITCH_BASE ) {

      // case FB_EDGE_SWITCH_BASE:
      fb_index = Add_index_switch( wn );
      const INT32 index_target = FB_EDGE_SWITCH_INDEX( type );
      _switches[fb_index][index_target] = freq;

    } else if ( type >= FB_EDGE_IO_ESCAPE_BASE &&
		type <  FB_EDGE_IO_ESCAPE_BASE + FB_IO_ESCAPE_EDGES_MAX ) {

      // case FB_EDGE_IO_ESCAPE_BASE:
      DevWarn( "Annot called with type FB_EDGE_IO_ESCAPE(br) -- not handled" );

    } else
      Is_True( false, ( "FEEDBACK::Query encounted unexpected edge type" ) );

    break;
  }

  if ( _trace ) {
    char buffer[FB_EDGE_TYPE_NAME_LENGTH];
    FB_EDGE_TYPE_sprintf( buffer, type );

    fprintf( TFile, "FEEDBACK::Annot(0x%p, %s)  = ", wn, buffer );
    freq.Print( TFile );
    fprintf( TFile, "\n" );
    Print_with_wn( TFile, wn );
  }
}

// When transforming:
// From:
// if (a && b) {
//   x = ...
// }
// else {
//   <empty>
// }
// To:
// if (a) {
//   if (b) {
//     x = ...
//   }
//   else {
//      <empty>
//   }
// }
// else {
//   <empty>
// }
//
// We adjust the frequency data as follows:
// From:
//                     if (a && b)
//                  TK /         \ NT
//                    /           \
// To:
// 
//                     if (a)
//               TKnew /     \ NTnew
//                    /       \
//                 if (b)
//          TKinner /   \ NTinner
//                 /     \
// Where:
// TOT = TK + NT
// TK% = TK / TOT
// TK%new = sqrt(TK%)
// TKnew = TOT * TK%new
// NTnew = TOT - TKnew
// TKinner = TKnew * TK%new
// NTinner = TKnew - TKinner
// 
void
FEEDBACK::FB_split_cand_if( WN *wn_outer_if, WN *wn_inner_if )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_split_cand(0x%p, 0x%p):\n",
	     wn_outer_if, wn_inner_if );

  Is_True( wn_outer_if != NULL && wn_inner_if != NULL,
	   ( "FEEDBACK::FB_cplit_can expects non-NULL wn_outer_if and wn_inner_if" ) );

  Is_True( WN_operator( wn_outer_if ) == OPR_IF &&
	   WN_operator( wn_inner_if ) == OPR_IF,
	   ( "FEEDBACK::FB_split_cand expects IF wn_outer_if and wn_inner_if" ) );

  const FB_Info_Branch& info_branch = Query_branch( wn_outer_if );
  FB_FREQ freq_tkn = info_branch.freq_taken;
  FB_FREQ freq_not = info_branch.freq_not_taken;
  FB_FREQ freq_total = freq_tkn + freq_not;
  FB_FREQ freq_tkn_pct = freq_tkn / freq_total;
  FB_FREQ freq_tkn_pct_new = freq_tkn_pct.sqrt();
  FB_FREQ freq_tkn_new_outer = freq_total * freq_tkn_pct_new;
  FB_FREQ freq_not_new_outer = freq_total - freq_tkn_new_outer;
  Annot_branch( wn_outer_if, FB_Info_Branch( freq_tkn_new_outer,
					     freq_not_new_outer,
					     OPR_IF ) );
  FB_FREQ freq_tkn_new_inner = freq_tkn_new_outer * freq_tkn_pct_new;
  FB_FREQ freq_not_new_inner = freq_tkn_new_outer - freq_tkn_new_inner;
  Annot_branch( wn_inner_if, FB_Info_Branch( freq_tkn_new_inner,
					     freq_not_new_inner,
					     OPR_IF ) );
}

// ====================================================================
// The following procedures update the feedback information for whirl
// nodes created or modified during lowering transformations.
// ====================================================================

// *IF                                *(FALSEBR <cond> <else_lbl>)
//    <cond>                           <then_clause>
//  THEN                               (GOTO <cont_lbl>)
//    <then_clause>        ===>        (LABEL <else_lbl>)
//  ELSE                               <else_clause>
//    <else_clause>                    (LABEL <cont_lbl>)
//  END_IF
//
//  IF                                *(FALSEBR <cond> <cont_lbl>)
//    <cond>               ===>        <then_clause>
//  THEN                               (LABEL <cont_lbl>)
//    <then_clause>
//  END_IF
//
//  IF                                *(TRUEBR <cond> <cont_lbl>)
//    <cond>               ===>        <else_clause>
//  ELSE                               (LABEL <cont_lbl>)
//    <else_clause>
//  END_IF
//
//  IF                     ===>        (EVAL <cond>)
//    <cond>
//  END_IF
//
// *CSELECT                            *IF
//   <test>                              <test>
//   <then_expr>                        THEN
//   <else_expr>            ===>          <then_expr>
//                                       STID <preg>
//                                      ELSE
//                                        <else_expr>
//                                       STID <preg>
//                                      END_IF
void
FEEDBACK::FB_lower_branch( WN *wn_br, WN *wn_branch )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_branch(0x%p, 0x%p):\n",
	     wn_br, wn_branch );

  Is_True( wn_br != NULL,
	   ( "FEEDBACK::FB_lower_branch expects non-NULL wn_branch" ) );

  // Retrieve IF or CSELECT frequency data
  OPERATOR opr = WN_operator( wn_br );
  Is_True( opr == OPR_IF || opr == OPR_CSELECT,
	   ( "FEEDBACK::FB_lower_branch expects IF or CSELECT wn_br" ) );

  if ( wn_branch != NULL ) {
    const FB_Info_Branch& info_branch = Query_branch( wn_br );
    Annot_branch( wn_branch, FB_Info_Branch( info_branch.freq_taken,
					     info_branch.freq_not_taken,
					     WN_operator( wn_branch ) ) );
  }

  Delete( wn_br );
}


//   <cond_1st>               *TRUEBR/FALSEBR <cond_left>  <label>
//   <cond_2nd>      ===>     *TRUEBR/FALSEBR <cond_right> <label>
// *CAND/CIOR                  GOTO <neither_label>
void
FEEDBACK::FB_lower_circuit( WN *wn_circuit, WN *wn_left_br, WN *wn_right_br )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_circuit(0x%p, 0x%p, 0x%p):\n",
	     wn_circuit, wn_left_br, wn_right_br );

  Is_True( wn_circuit != NULL,
	   ( "FEEDBACK::FB_lower_circuit expects non-NULL wn_circuit" ) );

  // Retrieve CAND/CIOR frequency data
  OPERATOR opr = WN_operator( wn_circuit );
  Is_True( opr == OPR_CAND || opr == OPR_CIOR,
	   ( "FEEDBACK::FB_lower_cand expects CAND wn_circuit" ) );
  const FB_Info_Circuit& info_circuit = Query_circuit( wn_circuit );

  // Annot frequency data for left branch
  FB_Info_Branch fb_info_branch;
  if ( wn_left_br != NULL ) {
    FB_FREQ freq_tkn = info_circuit.freq_left;
    FB_FREQ freq_not = info_circuit.freq_right + info_circuit.freq_neither;
    OPERATOR opr_br  = WN_operator( wn_left_br );
    if ( opr == OPR_CAND )
      fb_info_branch = FB_Info_Branch( freq_not, freq_tkn, opr_br );
    else
      fb_info_branch = FB_Info_Branch( freq_tkn, freq_not, opr_br );
    Annot_branch( wn_left_br, fb_info_branch );
  }

  // Annot frequency data for right branch
  if ( wn_right_br != NULL ) {
    FB_FREQ freq_tkn = info_circuit.freq_right;
    FB_FREQ freq_not = info_circuit.freq_neither;
    OPERATOR opr_br  = WN_operator( wn_right_br );
    if ( opr == OPR_CAND )
      fb_info_branch = FB_Info_Branch( freq_not, freq_tkn, opr_br );
    else
      fb_info_branch = FB_Info_Branch( freq_tkn, freq_not, opr_br );
    Annot_branch( wn_right_br, fb_info_branch );
  }

  Delete( wn_circuit );
}


//  ( e1 *CIOR e2 ) CAND ( e1 *CIOR e3 )    ===>    e1 *CIOR ( e2 *CAND e3 )
//  ( e1 *CAND e2 ) CIOR ( e1 *CAND e3 )    ===>    e1 *CAND ( e2 *CIOR e3 )
void
FEEDBACK::FB_factor_circuit( WN *wn_left, WN *wn_right,
			     WN *wn_outer, WN *wn_inner )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_factor_circuit(0x%p, 0x%p, 0x%p, 0x%p):\n",
	     wn_left, wn_right, wn_outer, wn_inner );

  // Retrieve middle, left, and right circuit frequency data
  const FB_Info_Circuit& info_left  = Query_circuit( wn_left  );
  const FB_Info_Circuit& info_right = Query_circuit( wn_right );

  // Annot frequency data for outer and inner circuits
  Annot_circuit( wn_outer, FB_Info_Circuit( info_left.freq_left,
					    info_right.freq_right,
					    info_left.freq_neither
					    + info_right.freq_neither ) );
  Annot_circuit( wn_inner, FB_Info_Circuit( info_left.freq_neither,
					    info_right.freq_neither,
					    info_right.freq_right ) );
  Delete( wn_left  );
  Delete( wn_right );
}


// ====================================================================
// The following procedures lower the feedback information for loop
// (DO_LOOP, DO_WHILE, WHILE_DO) lowering transformations.
// ====================================================================


// *DO_LOOP                            <init_stmt>
//    <index_var>                     *(FALSEBR <end_cond> <cont_lbl>)
//  INIT                               (LABEL <top_lbl> <loop_info>)
//    <init_stmt>          ===>        <body>
//  COMP                               <incr_stmt>
//    <end_cond>                      *(TRUEBR <end_cond> <top_lbl>)
//  INCR                               (LABEL <cont_lbl>)
//    <incr_stmt>
//  BODY
//    <body>
//
// *DO_WHILE                           (LABEL <top_lbl>)
//    <test>               ===>        <body>
//  BODY                              *(TRUEBR <test> <top_lbl>)
//    <body>
//
// *WHILE_DO                          *(FALSEBR <test> <cont_lbl>)
//    <test>               ===>        (LABEL <top_lbl>)
//  BODY                               <body>
//    <body>                          *(TRUEBR <end_cond> <top_lbl>)
//                                     (LABEL <cont_lbl>)
void
FEEDBACK::FB_lower_loop( WN *wn_loop, WN *wn_top_br, WN *wn_back_br )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_loop(0x%p, 0x%p, 0x%p):\n",
	     wn_loop, wn_top_br, wn_back_br );

  Is_True( wn_loop != NULL,
	   ( "FEEDBACK::FB_lower_loop expects non-NULL wn_loop" ) );

  // Retrieve loop frequency data
  Is_True( FB_valid_opr_loop( wn_loop ),
	   ( "FEEDBACK::FB_lower_loop expects loop WHIRL operator" ) );
  const FB_Info_Loop& info_loop = Query_loop( wn_loop );
  if ( WN_operator( wn_loop ) == OPR_DO_WHILE && ! info_loop.freq_zero.Zero() )
    DevWarn( "FEEDBACK::FB_lower_loop found freq_zero == %f",
	     info_loop.freq_zero.Value() );

  // Annote frequency data for top branch
  if ( wn_top_br != NULL )
    Annot_branch( wn_top_br, FB_Info_Branch( info_loop.freq_positive,
					     info_loop.freq_zero,
					     WN_operator( wn_top_br ) ) );

  // Annote frequency data for back branch
  if ( wn_back_br != NULL ) {
    if ( WN_operator( wn_back_br ) == OPR_GOTO )
      Annot_invoke( wn_back_br, FB_Info_Invoke( info_loop.freq_back ) );
    else
      Annot_branch( wn_back_br, FB_Info_Branch( info_loop.freq_back,
						info_loop.freq_out,
						WN_operator( wn_back_br ) ) );
  }

  // Don't deleted wn_loop data; PREOPT may need it!
}


//  DO_LOOP/WHILE_DO                   LABEL <top_label>
//    <test>               ===>        FALSEBR <test> <cont_label>
//  BODY                               <body>
//    <body>                           GOTO <top_label>
//                                     LABEL <cont_lbl>
void
FEEDBACK::FB_lower_loop_alt ( WN *wn_loop, WN *wn_top_br )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_loop_alt(0x%p, 0x%p):\n",
	     wn_loop, wn_top_br );

  Is_True( wn_loop != NULL,
	   ( "FEEDBACK::FB_lower_loop_alt expects non-NULL wn_loop" ) );

  // Retrieve loop frequency data
  OPERATOR opr = WN_operator( wn_loop );
  Is_True( opr == OPR_WHILE_DO || opr == OPR_DO_LOOP,
	   ( "FEEDBACK::FB_lower_loop_alt expects"
	     " WHILE_DO or DO_LOOP wn_loop" ) );
  const FB_Info_Loop& info_loop = Query_loop( wn_loop );

  // Annote frequency data for top branch
  if ( wn_top_br != NULL )
    Annot_branch( wn_top_br, FB_Info_Branch( info_loop.freq_iterate,
					     info_loop.freq_exit,
					     WN_operator( wn_top_br ) ) );

  // Don't deleted wn_loop data; PREOPT may need it!
}


//  WHILE_DO                           IF
//    <test>                             <test>
//  BODY                               THEN
//    <body>               ===>          DO_WHILE
//                                         <test>
//                                       BODY
//                                         <body>
//                                       END_DO_WHILE
//                                     END_IF
//
// wn_loop WN is not deleted, but modified ( WHILE_DO --> DO_WHILE )
void
FEEDBACK::FB_lower_while_do_to_do_while ( WN *wn_loop, WN *wn_top_br )
{
  // NOTE: the same WN wn_loop is both WHILE_DO and DO_WHILE
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_while_do(0x%p, 0x%p):\n",
	     wn_loop, wn_top_br );

  Is_True( wn_loop != NULL,
	  ( "FEEDBACK::FB_lower_while_do expects non-NULL wn_loop" ) );

  // Retrieve loop frequency data
  OPERATOR opr = WN_operator( wn_loop );
  Is_True( opr == OPR_WHILE_DO, ( "FEEDBACK::FB_lower_while_do_to_do_while"
				  " expects WHILE_DO wn_loop" ) );
  const FB_Info_Loop& info_loop = Query_loop( wn_loop );

  // Annote frequency data for top branch
  if ( wn_top_br != NULL )
    Annot_branch( wn_top_br, FB_Info_Branch( info_loop.freq_positive,
					     info_loop.freq_zero,
					     WN_operator( wn_top_br ) ) );

  // Update frequency data for DO_WHILE loop
  Annot( wn_loop, FB_EDGE_LOOP_ZERO, FB_FREQ_ZERO );
}


// ====================================================================
// The following procedure lowers the feedback information for COMPGOTO
// ====================================================================

void
FEEDBACK::FB_lower_compgoto ( WN *wn_compgoto, WN *wn_xgoto, WN *wn_branch )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_compgoto(0x%p, 0x%p, 0x%p):\n",
	     wn_compgoto, wn_xgoto, wn_branch );

  Is_True( wn_compgoto != NULL,
	   ( "FEEDBACK::FB_lower_compgoto expects non-NULL wn_compgoto" ) );

  // Retrieve loop frequency data
  OPERATOR opr = WN_operator( wn_compgoto );
  Is_True( opr == OPR_COMPGOTO, ( "FEEDBACK::FB_lower_compgoto"
				  " expects COMPGOTO compgoto" ) );
  FB_Info_Switch info_switch = Query_switch( wn_compgoto );

  // Strip off default frequency
  FB_FREQ freq_default = ( WN_kid_count( wn_compgoto ) == 3
			   ? info_switch[0] : FB_FREQ_UNINIT );
  info_switch[0] = FB_FREQ_ZERO;

  // Annote frequency data for top branch
  if ( wn_branch != NULL )
    Annot_branch( wn_branch, FB_Info_Branch( freq_default, info_switch.Total(),
					     WN_operator( wn_branch ) ) );

  // Annot frequency data for XGOTO loop
  Annot_switch( wn_xgoto, info_switch );

  Delete( wn_compgoto );
}


// ====================================================================
// The following procedures lower the feedback information for CALL
// and RETURN_VAL and MSTORE and MLOAD lowering transformations.
// ====================================================================

void
FEEDBACK::FB_lower_call( WN *wn_call, WN *wn_new_call )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_call(0x%p, 0x%p):\n",
	     wn_call, wn_new_call );

  Is_True( wn_call != NULL,
	   ( "FEEDBACK::FB_lower_call expects non-NULL wn_call" ) );

  Is_True( FB_valid_opr_call( wn_call ),
	   ( "FEEDBACK::FB_lower_call encounted unexpected operator" ) );
  FB_Info_Call info_call = Query_call( wn_call );

  if ( ! info_call.in_out_same )
    info_call.freq_entry = FB_FREQ_UNKNOWN;

  Is_True( FB_valid_opr_call( wn_new_call ),
	   ( "FEEDBACK::FB_lower_call encounted unexpected operator" ) );
  Annot_call( wn_new_call, info_call );

  if ( wn_call != wn_new_call )
    Delete( wn_call );
}

//  wn_icall  is transformed to 
//      if (*fptr == new_call1)    // <== new_if 1
//         new_call1(...);         // <== new_call [icall_no1]
//      else {
//         if (*fptr == new_call2) // <== new_if 2
//            new_call2(...);      // <== new_call [icall_no2]
//         else {
//            ...
//            fptr(...);           // <== new_icall
//      }
//  the wn_new_icall may be null if it is not for the last
//  if-else statement
void 
FEEDBACK::FB_lower_icall( WN *wn_icall, WN *wn_new_icall, WN * wn_new_call, WN * wn_new_if, int icall_no )
{
  if (wn_new_icall != 0) {
    // initialize the FB info from the icall node when wn_new_icall is not
    // empty (when it is first time called by Covert_Icall for the wn_icall ), 
    // the FB info will be overwrite by the copied new_icall node which shares
   //  the same FB info in the FB map
    _info_call = Query_call( wn_icall );
    _info_icall = Query_icall( wn_icall );
    _last_wn_icall = wn_icall;
  }

  Is_True( wn_icall == _last_wn_icall, ("invalid last_wn_icall %#p\n", wn_icall) );
  FB_lower_icall (_info_call, _info_icall, wn_new_icall, wn_new_call, wn_new_if, icall_no);
}

void
FEEDBACK::FB_lower_icall( FB_Info_Call &info_call, FB_Info_Icall &info_icall,
                          WN *wn_new_icall, WN * wn_new_call, WN * wn_new_if, 
                          int icall_no )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_icall(0x%p, ,0x%p, 0x%p, 0x%p, 0x%p, %d):\n",
	     &info_call, &info_icall, wn_new_icall, wn_new_call, wn_new_if, icall_no );

#if 0
  Is_True( wn_icall != NULL,
	   ( "FEEDBACK::FB_lower_icall expects non-NULL wn_icall" ) );

  Is_True( FB_valid_opr_call( wn_icall ),
	   ( "FEEDBACK::FB_lower_icall encounted unexpected operator" ) );
  FB_Info_Call info_call = Query_call( wn_icall );
  FB_Info_Icall info_icall = Query_icall( wn_icall );
#endif

  Is_True( info_call.freq_entry._value >= (float) info_icall.tnv._counters[icall_no], 
		  ("entry count of icall less than counter[%d]! impossible!", icall_no)  );

  FB_FREQ freq_taken((float)info_icall.tnv._counters[icall_no],true);
  // compute the no-taken frequency
  // summrize the previous calls taken frequency
  UINT64 prev_freq = 0;
  for (int i=0; i<=icall_no; i++)
     prev_freq += info_icall.tnv._counters[i];
  FB_FREQ freq_nottaken(info_call.freq_entry._value - (float)prev_freq, true);

  Is_True( FB_valid_opr_branch(wn_new_if), 
		  ("FEEDBACK::FB_lower_icall encounted unexpected operator") );
  Annot_branch(wn_new_if, FB_Info_Branch(freq_taken,freq_nottaken)); //...
  Is_True(wn_new_icall == NULL || FB_valid_opr_call( wn_new_icall ),
	   ( "FEEDBACK::FB_lower_icall encounted unexpected operator" ) );

  if ( ! info_call.in_out_same )
  {
    Annot_call( wn_new_call, FB_Info_Call(freq_taken,FB_FREQ_UNKNOWN,false) );
    if (wn_new_icall)
      Annot_call( wn_new_icall, FB_Info_Call(freq_nottaken,FB_FREQ_UNKNOWN,false) );
  }
  else
  {
    Annot_call( wn_new_call, FB_Info_Call(freq_taken) );
    if (wn_new_icall)
      Annot_call( wn_new_icall, FB_Info_Call(freq_nottaken) );
  }

  if (wn_new_icall) {
#ifdef KEY
    FB_Info_Icall info_new_icall = info_icall;
    const UINT64 taken = (UINT64)freq_taken.Value();

    info_new_icall.tnv._exec_counter -= taken;
    info_new_icall.tnv._counters[icall_no]  -= taken;
    // Don't convert it more than once.
    info_new_icall.tnv._flag = FB_TNV_FLAG_UNINIT;

    Annot_icall( wn_new_icall, info_new_icall );
#endif
  }
}


// RETURNVAL                            <expr>
//  <expr>                 ===>        STID <preg>
//                                     RETURN
void
FEEDBACK::FB_lower_return_val( WN *wn_return_val, WN *wn_return )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_return_val(0x%p, 0x%p):\n",
	     wn_return_val, wn_return );

  Is_True( wn_return_val != NULL,
	   ( "FEEDBACK::FB_lower_return_val expects non-NULL wn_cselect" ) );

  OPERATOR opr = WN_operator( wn_return_val );
  Is_True( opr == OPR_RETURN_VAL, ( "FEEDBACK::FB_lower_return_val"
				    " expects RETURN_VAL wn_return_val" ) );
  const FB_Info_Invoke& info_invoke = Query_invoke( wn_return_val );

  Annot_invoke( wn_return, info_invoke );

  Delete( wn_return_val );
}

// MSTORE                  ===>        DO_LOOP  (constant # of iterations)
// MSTORE                  ===>        WHILE_DO (variable # of iterations)

void
FEEDBACK::FB_lower_mstore_to_loop ( WN *wn_mstore, WN *wn_loop, INT64 nMoves )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::lower_mstore_to_loop(0x%p, 0x%p, %lld):\n",
	     wn_mstore, wn_loop, nMoves );

  Is_True( wn_mstore != NULL, ( "FEEDBACK::FB_lower_mstore_to_loop"
				" expects non-NULL wn_mstore" ) );

  Is_True( WN_operator( wn_mstore ) == OPR_MSTORE,
	   ( "FEEDBACK::FB_lower_mstore_to_loop expects MSTORE" ) );
  const FB_Info_Invoke& info_invoke = Query_invoke( wn_mstore );

  OPERATOR opr = WN_operator( wn_loop );
  FB_Info_Loop info_loop;
  if ( opr == OPR_DO_LOOP ) {

    // Constant number of iterations == nMoves
    Is_True( nMoves > 0, ( "FEEDBACK::lower_mstore_to_loop found"
			   " non-positive nMoves == %lld", nMoves ) );
    info_loop = FB_Info_Loop( FB_FREQ_ZERO,
			      info_invoke.freq_invoke,
			      info_invoke.freq_invoke,
			      FB_FREQ( nMoves - 1, true /*exact*/ )
			      * info_invoke.freq_invoke );
  } else {

    // Variable/unknown number of iterations
    Is_True( opr == OPR_WHILE_DO, ( "FEEDBACK::FB_lower_mstore_to_loop"
				    " expects DO_LOOP or WHILE_DO" ) );
    info_loop = FB_Info_Loop( info_invoke.freq_invoke, FB_FREQ_UNKNOWN );

  }

  Annot_loop( wn_loop, info_loop );

  Delete( wn_mstore );
}


// ====================================================================
// Converting GOTOs into IFs and DO_WHILEs
// ====================================================================

// IF                                       IF
//   <expr>                                   <expr>
// THEN/ELSE                                THEN/ELSE
//   <statements1>                            <statements1>
//   TRUEBR <cond> <label>                    <goto_preg> = <cond>
//   <statements2>              ===>          IF
// END_IF                                       <! goto_preg>
//                                            THEN
//                                              <statements2>
//                                            ENDIF
//                                          END_IF
//                                          TRUEBR <goto_preg> <label>
void
FEEDBACK::FB_move_goto_out( WN *wn_branch, WN *wn_inner_br, WN *wn_outer_br )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_move_goto_out(0x%p, 0x%p, 0x%p):\n",
	     wn_branch, wn_inner_br, wn_outer_br );

  Is_True( wn_branch != NULL,
	   ( "FEEDBACK::FB_move_goto_out expects non-NULL wn_branch" ) );

  // Retrieve goto (GOTO or TRUEBR or FALSEBR) frequency data
  FB_FREQ freq_taken, freq_not;
  OPERATOR opr = WN_operator( wn_branch );
  if ( opr == OPR_GOTO ) {
    const FB_Info_Invoke& info_invoke = Query_invoke( wn_branch );
    freq_taken = info_invoke.freq_invoke;
    freq_not   = FB_FREQ_ZERO;
  } else {
    Is_True( opr == OPR_TRUEBR || opr == OPR_FALSEBR,
	     ( "FEEDBACK::FB_move_goto_out expects TRUE/FALSEBR wn_branch" ) );
    const FB_Info_Branch& info_branch = Query_branch( wn_branch );
    freq_taken = info_branch.freq_taken;
    freq_not   = info_branch.freq_not_taken;
  }

  // Annote frequency data for inner IF
  if ( wn_inner_br != NULL ) {
    opr = WN_operator( wn_inner_br );
    Is_True( opr == OPR_IF,
	     ( "FEEDBACK::FB_move_goto_out expects NULL or IF wn_inner_br" ) );
    Annot_branch( wn_inner_br, FB_Info_Branch( freq_not, freq_taken ) );
  }

  // Update frequency data for outer TRUEBR
  opr = WN_operator( wn_outer_br );
  Is_True( opr == OPR_TRUEBR,
	   ( "FEEDBACK::FB_move_goto_out expects TRUEBR wn_outer_br" ) );
  Annot_branch( wn_outer_br, FB_Info_Branch( freq_taken, FB_FREQ_UNKNOWN ) );

  Delete( wn_branch );
}


// TRUEBR <cond> <label>                    IF
// <statements>                               <! cond>
// LABEL <label>                ===>        THEN
//                                            <statements>
//                                          END_IF
void
FEEDBACK::FB_convert_goto_to_if( WN *wn_branch, WN *wn_if )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_convert_goto_to_if(0x%p, 0x%p):\n",
	     wn_branch, wn_if );

  Is_True( wn_branch != NULL,
	   ( "FEEDBACK::FB_convert_goto_to_if expects non-NULL wn_branch" ) );

  // Retrieve goto (TRUEBR or FALSEBR) frequency data
  OPERATOR opr = WN_operator( wn_branch );
  Is_True( opr == OPR_TRUEBR || opr == OPR_FALSEBR,
	   ( "FEEDBACK::FB_convert_goto_to_if"
	     " expects TRUE/FALSEBR wn_branch" ) );
  const FB_Info_Branch& info_branch = Query_branch( wn_branch );

  // Annote frequency data for inner IF
  opr = WN_operator( wn_if );
  Is_True( opr == OPR_IF,
	   ( "FEEDBACK::FB_convert_goto_to_if expects IF wn_if" ) );
  Annot_branch( wn_if, FB_Info_Branch( info_branch.freq_not_taken,
				       info_branch.freq_taken ) );

  Delete( wn_branch );
}


// LABEL <label>                            DO_WHILE
// <statements>                               <cond>
// TRUEBR <cond> <label>        ===>        BODY
//                                            <statements>
//                                          END_DO_WHILE
void
FEEDBACK::FB_convert_goto_to_loop( WN *wn_branch, WN *wn_loop )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_convert_goto_to_loop(0x%p, 0x%p):\n",
	     wn_branch, wn_loop );

  Is_True( wn_branch != NULL, ( "FEEDBACK::FB_convert_goto_to_loop"
				" expects non-NULL wn_branch" ) );

  // Retrieve goto (TRUEBR or FALSEBR) frequency data
  OPERATOR opr = WN_operator( wn_branch );
  Is_True( opr == OPR_GOTO || opr == OPR_TRUEBR || opr == OPR_FALSEBR,
	   ( "FEEDBACK::FB_convert_goto_to_loop"
	     " expects GOTO, TRUEBR or FALSEBR wn_branch" ) );

  FB_FREQ freq_taken, freq_not_taken;
  if ( opr == OPR_GOTO ) {    // Unconditional branch
    const FB_Info_Invoke& info_invoke = Query_invoke( wn_branch );
    freq_taken     = info_invoke.freq_invoke;
    freq_not_taken = FB_FREQ_ZERO;
  } else {                  // Conditional branch
    const FB_Info_Branch& info_branch = Query_branch( wn_branch );
    freq_taken     = info_branch.freq_taken;
    freq_not_taken = info_branch.freq_not_taken;
  }

  // Annote frequency data for inner DO_WHILE loop
  opr = WN_operator( wn_loop );
  Is_True( opr == OPR_DO_WHILE,
	   ( "FEEDBACK::FB_convert_goto_to_loop expects DO_WHILE wn_loop" ) );
  Annot_loop( wn_loop, FB_Info_Loop( FB_FREQ_ZERO, FB_FREQ_UNKNOWN,
				     freq_not_taken, freq_taken ) );

  Delete( wn_branch );
}


// ====================================================================
// Convert a TRUEBR/FALSEBR branch with a constant condition into a GOTO
// ====================================================================

void
FEEDBACK::FB_simplify_branch_to_goto( WN *wn_branch )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_simplify_branch_to_goto(0x%p):\n",
	     wn_branch );

  Is_True( wn_branch != NULL, ( "FEEDBACK::FB_simplify_branch_to_goto"
				" expects non-NULL wn_branch" ) );
  OPERATOR opr = WN_operator( wn_branch );
  Is_True( opr == OPR_GOTO, ( "FEEDBACK::FB_simplify_branch_to_goto"
			      " expects GOTO wn_branch" ) );

  // Retrieve branch (TRUEBR or FALSEBR) frequency data
  WN_set_operator( wn_branch, OPR_TRUEBR );
  const FB_Info_Branch& info_branch = Query_branch( wn_branch );
  WN_set_operator( wn_branch, OPR_GOTO );

  Delete( wn_branch );

  if ( info_branch.freq_not_taken.Known()
       && ! info_branch.freq_not_taken.Zero() ) {
    DevWarn( "FEEDBACK::FB_simplify_branch_to_goto: nonzero freq_not_taken" );
  }

  // Annote frequency data for GOTO
  Annot_invoke( wn_branch, FB_Info_Invoke( info_branch.freq_taken ) );
}


// ====================================================================
// Scaling, Duplicating, Cloning, and Recombining whirl nodes and trees
// ====================================================================

void
FEEDBACK::FB_set_zero_node( WN *wn )
{
  switch( WN_operator( wn ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info( FB_FREQ_ZERO );
      Annot_invoke( wn, fb_info );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info( FB_FREQ_ZERO, FB_FREQ_ZERO );
      Annot_branch( wn, fb_info );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info( FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO,
			    FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO );
      Annot_loop( wn, fb_info );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info( FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO );
      Annot_circuit( wn, fb_info );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info( FB_FREQ_ZERO );
      Annot_call( wn, fb_info );
#ifdef KEY
      if( WN_operator(wn) == OPR_ICALL ){
	FB_Info_Icall fb_icall_info;   // ???
	Annot_icall( wn, fb_icall_info );
      }
#endif
    }
    break;

  fb_opr_cases_switch:
    {
      // Count number of cases
      INT32 number_cases = ( WN_kid_count(wn) == 3 ) ? 1 : 0;
      for ( WN *wn1 = WN_first( WN_kid1( wn ) ); wn1; wn1 = WN_next( wn1 ) )
	++number_cases;

      FB_Info_Switch fb_info( number_cases );
      for (INT32 t = number_cases - 1; t >= 0; --t ) {
	fb_info[t] = FB_FREQ_ZERO;
      }
      Annot_switch( wn, fb_info );
    }
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn ) ){
      FB_Info_Value fb_info( 0, 0, NULL, NULL );
      Annot_value( wn, fb_info );
    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn ) ){
      FB_Info_Value_FP_Bin fb_info( 0, 0, 0, 0, 0 );
      Annot_value_fp_bin( wn, fb_info );
      break;
    }
    break;
#endif

  default:
    break;
  }
}


void
FEEDBACK::FB_set_zero( WN *wn )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_set_zero(0x%p):\n", wn );

  for ( TREE_ITER iter( wn ); iter.Wn() != NULL; ++iter ) {

    // Zero out the node data
    FB_set_zero_node( iter.Wn() );
  }
}

void
FEEDBACK::FB_set_unknown_node( WN *wn )
{
  switch( WN_operator( wn ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info( FB_FREQ_UNKNOWN );
      Annot_invoke( wn, fb_info );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info( FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN );
      Annot_branch( wn, fb_info );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info( FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN,
			    FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN,
			    FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN );
      Annot_loop( wn, fb_info );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info( FB_FREQ_UNKNOWN,
			       FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN );
      Annot_circuit( wn, fb_info );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info( FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN );
      Annot_call( wn, fb_info );
#ifdef KEY
      if( WN_operator(wn) == OPR_ICALL ){
	FB_Info_Icall fb_icall_info;   // ???
	Annot_icall( wn, fb_icall_info );
      }
#endif
    }
    break;

  fb_opr_cases_switch:
    {
      // Count number of cases
      INT32 number_cases = ( WN_kid_count(wn) == 3 ) ? 1 : 0;;
      for ( WN *wn1 = WN_first( WN_kid1( wn ) ); wn1; wn1 = WN_next( wn1 ) )
	++number_cases;

      FB_Info_Switch fb_info( number_cases );
      for (INT32 t = number_cases - 1; t >= 0; --t ) {
	fb_info[t] = FB_FREQ_UNKNOWN;
      }
      Annot_switch( wn, fb_info );
    }
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn ) ){
      FB_Info_Value fb_info( 0, 0, NULL, NULL );
      Annot_value( wn, fb_info );
    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn ) ){
      FB_Info_Value_FP_Bin fb_info( 0, 0, 0, 0, 0 );
      Annot_value_fp_bin( wn, fb_info );
    }
    break;
#endif

  default:
    break;
  }
}


void
FEEDBACK::FB_set_unknown( WN *wn )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_set_unknown(0x%p):\n", wn );

  for ( TREE_ITER iter( wn ); iter.Wn() != NULL; ++iter ) {

    // Zero out the node data
    FB_set_unknown_node( iter.Wn() );
  }
}


void
FEEDBACK::FB_scale_node( WN *wn, FB_FREQ freq_scale )
{
/*
  Is_True( freq_scale.Known() &&
	   freq_scale.Value() >= 0.0 && freq_scale.Value() <= 1.0,
	   ( "FEEDBACK::FB_scale: freq_scale == %f", freq_scale.Value() ) );
*/

  switch( WN_operator( wn ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info = Query_invoke( wn );
      fb_info.freq_invoke *= freq_scale;
      Annot_invoke( wn, fb_info );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info = Query_branch( wn );
      fb_info.freq_taken     *= freq_scale;
      fb_info.freq_not_taken *= freq_scale;
      Annot_branch( wn, fb_info );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info = Query_loop( wn );
      fb_info.freq_zero     *= freq_scale;
      fb_info.freq_positive *= freq_scale;
      fb_info.freq_out      *= freq_scale;
      fb_info.freq_back     *= freq_scale;
      fb_info.freq_exit     *= freq_scale;
      fb_info.freq_iterate  *= freq_scale;
      Annot_loop( wn, fb_info );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info = Query_circuit( wn );
      fb_info.freq_left    *= freq_scale;
      fb_info.freq_right   *= freq_scale;
      fb_info.freq_neither *= freq_scale;
      Annot_circuit( wn, fb_info );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info = Query_call( wn );
      fb_info.freq_entry *= freq_scale;
      fb_info.freq_exit  *= freq_scale;
      Annot_call( wn, fb_info );
#ifdef KEY
      if( WN_operator(wn) == OPR_ICALL ){
	FB_Info_Icall fb_icall_info = Query_icall( wn );
	const float scale = freq_scale.Value();
	fb_icall_info.tnv._exec_counter =
	  (UINT64)(fb_icall_info.tnv._exec_counter * scale);

	for( int i = 0; i < FB_TNV_SIZE; i ++ ){
	  if( fb_icall_info.tnv._values[i] == 0 )
	    break;
	  fb_icall_info.tnv._counters[i] =
	    (UINT64)(fb_icall_info.tnv._counters[i] * scale);
	}
	
	Annot_icall( wn, fb_icall_info );
      }
#endif
    }
    break;

  fb_opr_cases_switch:
    {
      FB_Info_Switch fb_info = Query_switch( wn );
      for ( INT t = fb_info.size() - 1; t >= 0; t-- )
	fb_info[t] *= freq_scale;
      Annot_switch( wn, fb_info );
    }
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn ) ){
      FB_Info_Value fb_info = Query_value( wn );
      fb_info.exe_counter *= freq_scale;
      for( int i = 0; i < fb_info.num_values; i++ ){
	fb_info.freq[i] *= freq_scale;
      }
      Annot_value( wn, fb_info );
    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn ) ){
      FB_Info_Value_FP_Bin fb_info = Query_value_fp_bin( wn );
      fb_info.exe_counter *= freq_scale;
      fb_info.zopnd0 *= freq_scale;
      fb_info.zopnd1 *= freq_scale;
      fb_info.uopnd0 *= freq_scale;
      fb_info.uopnd1 *= freq_scale;
      Annot_value_fp_bin( wn, fb_info );
    }
    break;
#endif

  default:
    break;
  }
}


void
FEEDBACK::FB_scale( WN *wn, FB_FREQ freq_scale )
{
  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::FB_scale(0x%p, ", wn );
    freq_scale.Print( TFile );
    fprintf( TFile, "):\n" );
  }

  for ( TREE_ITER iter( wn ); iter.Wn() != NULL; ++iter ) {

    // Scale the node data
    FB_scale_node( iter.Wn(), freq_scale );
  }
}


void
FEEDBACK::FB_duplicate_node( WN *wn_origl, WN *wn_dupli )
{
  // Annotate the duplicate whirl node
  switch( WN_operator( wn_origl ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn_origl ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      const FB_Info_Invoke& fb_info = Query_invoke( wn_origl );
      Annot_invoke( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_branch:
    {
      const FB_Info_Branch& fb_info = Query_branch( wn_origl );
      Annot_branch( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_loop:
    {
      const FB_Info_Loop& fb_info = Query_loop( wn_origl );
      Annot_loop( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_circuit:
    {
      const FB_Info_Circuit& fb_info = Query_circuit( wn_origl );
      Annot_circuit( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_call:
    {
      const FB_Info_Call& fb_info = Query_call( wn_origl );
      Annot_call( wn_dupli, fb_info );
#ifdef KEY
      if( WN_operator(wn_origl) == OPR_ICALL ){
	FB_Info_Icall fb_icall_info = Query_icall( wn_origl );
	Annot_icall( wn_dupli, fb_icall_info );
      }
#endif
    }
    break;

  fb_opr_cases_switch:
    {
      const FB_Info_Switch& fb_info = Query_switch( wn_origl );
      Annot_switch( wn_dupli, fb_info );
    }
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn_origl ) ){
      const FB_Info_Value& fb_info = Query_value( wn_origl );
      Annot_value( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn_origl ) ){
      const FB_Info_Value_FP_Bin& fb_info = Query_value_fp_bin( wn_origl );
      Annot_value_fp_bin( wn_dupli, fb_info );
    }
    break;
#endif

  default:
    break;
  }
}


void
FEEDBACK::FB_duplicate( WN *wn_origl, WN *wn_dupli )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_duplicate(0x%p, 0x%p):\n",
	     wn_origl, wn_dupli );

  for ( TREE_ITER origl( wn_origl ), dupli( wn_dupli );
	origl.Wn() != NULL && dupli.Wn() != NULL;
	++origl, ++dupli ) {
    
    // Duplicate the node data
    FB_duplicate_node( origl.Wn(), dupli.Wn() );
  }
}


void
FEEDBACK::FB_recombine_node( WN *wn_origl, WN *wn_extra )
{
  switch( WN_operator( wn_origl ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn_origl ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info_origl = Query_invoke( wn_origl );
      const FB_Info_Invoke& fb_info_extra = Query_invoke( wn_extra );
      fb_info_origl.freq_invoke += fb_info_extra.freq_invoke;
      Annot_invoke( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info_origl = Query_branch( wn_origl );
      const FB_Info_Branch& fb_info_extra = Query_branch( wn_extra );
      fb_info_origl.freq_taken     += fb_info_extra.freq_taken    ;
      fb_info_origl.freq_not_taken += fb_info_extra.freq_not_taken;
      Annot_branch( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info_origl = Query_loop( wn_origl );
      const FB_Info_Loop& fb_info_extra = Query_loop( wn_extra );
      fb_info_origl.freq_zero     += fb_info_extra.freq_zero    ;
      fb_info_origl.freq_positive += fb_info_extra.freq_positive;
      fb_info_origl.freq_out      += fb_info_extra.freq_out     ;
      fb_info_origl.freq_back     += fb_info_extra.freq_back    ;
      fb_info_origl.freq_exit     += fb_info_extra.freq_exit    ;
      fb_info_origl.freq_iterate  += fb_info_extra.freq_iterate ;
      Annot_loop( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info_origl = Query_circuit( wn_origl );
      const FB_Info_Circuit& fb_info_extra = Query_circuit( wn_extra );
      fb_info_origl.freq_left    += fb_info_extra.freq_left   ;
      fb_info_origl.freq_right   += fb_info_extra.freq_right  ;
      fb_info_origl.freq_neither += fb_info_extra.freq_neither;
      Annot_circuit( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info_origl = Query_call( wn_origl );
      const FB_Info_Call& fb_info_extra = Query_call( wn_extra );
      fb_info_origl.freq_entry += fb_info_extra.freq_entry;
      fb_info_origl.freq_exit  += fb_info_extra.freq_exit;
      Annot_call( wn_origl, fb_info_origl );
#ifdef KEY
      if( WN_operator(wn_origl) == OPR_ICALL ){
	FB_Info_Icall fb_icall_info_origl = Query_icall( wn_origl );
	const FB_Info_Icall& fb_icall_info_extra = Query_icall( wn_extra );
	fb_icall_info_origl.tnv._exec_counter += fb_icall_info_extra.tnv._exec_counter;

	for( int i = 0; i < FB_TNV_SIZE; i ++ ){
	  if( fb_icall_info_origl.tnv._values[i] == 0 )
	    break;
	  fb_icall_info_origl.tnv._counters[i] += fb_icall_info_extra.tnv._counters[i];
	}
	
	Annot_icall( wn_origl, fb_icall_info_origl );
      }
#endif
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_switch:
    {
      FB_Info_Switch fb_info_origl = Query_switch( wn_origl );
      const FB_Info_Switch& fb_info_extra = Query_switch( wn_extra );
      for ( INT t = fb_info_origl.size() - 1; t >= 0; t-- )
	fb_info_origl[t] += fb_info_extra[t];
      Annot_switch( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn_origl ) ){
      FB_Info_Value fb_info_origl = Query_value( wn_origl );
      const FB_Info_Value& fb_info_extra = Query_value( wn_extra );

      fb_info_origl.exe_counter += fb_info_extra.exe_counter;
      for( int i = 0; i < fb_info_origl.num_values; i++ ){
	fb_info_origl.freq[i] += fb_info_extra.freq[i];
      }

      Annot_value( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn_origl ) ){
      FB_Info_Value_FP_Bin fb_info_origl = Query_value_fp_bin( wn_origl );
      const FB_Info_Value_FP_Bin& fb_info_extra = 
	Query_value_fp_bin( wn_extra );

      fb_info_origl.exe_counter += fb_info_extra.exe_counter;
      fb_info_origl.zopnd0 += fb_info_extra.zopnd0;
      fb_info_origl.zopnd1 += fb_info_extra.zopnd1;
      fb_info_origl.uopnd0 += fb_info_extra.uopnd0;
      fb_info_origl.uopnd1 += fb_info_extra.uopnd1;

      Annot_value_fp_bin( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;
#endif

  default:
    break;
  }
}


void
FEEDBACK::FB_recombine( WN *wn_origl, WN *wn_extra )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_recombine(0x%p, 0x%p):\n",
	     wn_origl, wn_extra );

  for ( TREE_ITER origl( wn_origl ), extra(wn_extra);
	origl.Wn() != NULL && extra.Wn() != NULL;
	++origl, ++extra ) {

    // Recombine the node data
    FB_recombine_node( origl.Wn(), extra.Wn() );
  }
}


void
FEEDBACK::FB_clone_node( WN *wn_origl, WN *wn_clone, FB_FREQ freq_scale )
{
/*
  Is_True( freq_scale.Known() &&
	   freq_scale.Value() >= 0.0 && freq_scale.Value() <= 1.0,
	   ( "FEEDBACK::FB_scale: freq_scale == %f", freq_scale.Value() ) );
*/

  switch( WN_operator( wn_origl ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn_origl ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info_origl = Query_invoke( wn_origl );
      FB_Info_Invoke fb_info_clone( fb_info_origl.freq_invoke * freq_scale );
      fb_info_origl.freq_invoke -= fb_info_clone.freq_invoke;
      Annot_invoke( wn_origl, fb_info_origl );
      Annot_invoke( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info_origl = Query_branch( wn_origl );
      FB_Info_Branch fb_info_clone( fb_info_origl.freq_taken     * freq_scale,
				    fb_info_origl.freq_not_taken * freq_scale );
      fb_info_origl.freq_taken     -= fb_info_clone.freq_taken    ;
      fb_info_origl.freq_not_taken -= fb_info_clone.freq_not_taken;
      Annot_branch( wn_origl, fb_info_origl );
      Annot_branch( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info_origl = Query_loop( wn_origl );
      FB_Info_Loop fb_info_clone( fb_info_origl.freq_zero     * freq_scale,
				  fb_info_origl.freq_positive * freq_scale,
				  fb_info_origl.freq_out      * freq_scale,
				  fb_info_origl.freq_back     * freq_scale,
				  fb_info_origl.freq_exit     * freq_scale,
				  fb_info_origl.freq_iterate  * freq_scale );
      fb_info_origl.freq_zero     -= fb_info_clone.freq_zero    ;
      fb_info_origl.freq_positive -= fb_info_clone.freq_positive;
      fb_info_origl.freq_out      -= fb_info_clone.freq_out     ;
      fb_info_origl.freq_back     -= fb_info_clone.freq_back    ;
      fb_info_origl.freq_exit     -= fb_info_clone.freq_exit    ;
      fb_info_origl.freq_iterate  -= fb_info_clone.freq_iterate ;
      Annot_loop( wn_origl, fb_info_origl );
      Annot_loop( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info_origl = Query_circuit( wn_origl );
      FB_Info_Circuit fb_info_clone( fb_info_origl.freq_left    * freq_scale,
				     fb_info_origl.freq_right   * freq_scale,
				     fb_info_origl.freq_neither * freq_scale );
      fb_info_origl.freq_left    -= fb_info_clone.freq_left   ;
      fb_info_origl.freq_right   -= fb_info_clone.freq_right  ;
      fb_info_origl.freq_neither -= fb_info_clone.freq_neither;
      Annot_circuit( wn_origl, fb_info_origl );
      Annot_circuit( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info_origl = Query_call( wn_origl );
      FB_Info_Call fb_info_clone( fb_info_origl.freq_entry * freq_scale,
				  fb_info_origl.freq_exit  * freq_scale,
				  fb_info_origl.in_out_same );
      fb_info_origl.freq_entry -= fb_info_clone.freq_entry;
      fb_info_origl.freq_exit  -= fb_info_clone.freq_exit;
      Annot_call( wn_origl, fb_info_origl );
      Annot_call( wn_clone, fb_info_clone );
#ifdef KEY
      if( WN_operator(wn_origl) == OPR_ICALL ){
	FB_Info_Icall fb_info_origl = Query_icall( wn_origl );
	const float scale = freq_scale.Value();
	FB_Info_Icall fb_info_clone = fb_info_origl;

	fb_info_clone.tnv._exec_counter =
	  (UINT64)(fb_info_origl.tnv._exec_counter * scale);
	fb_info_origl.tnv._exec_counter -= fb_info_clone.tnv._exec_counter;

	for( int i = 0; i < FB_TNV_SIZE; i ++ ){
	  if( fb_info_origl.tnv._values[i] == 0 )
	    break;
	  fb_info_clone.tnv._counters[i] =
	    (UINT64)(fb_info_origl.tnv._counters[i] * scale);
	  if( fb_info_origl.tnv._counters[i] > fb_info_clone.tnv._counters[i] )
	    fb_info_origl.tnv._counters[i] -= fb_info_clone.tnv._counters[i];
	  else
	    fb_info_origl.tnv._counters[i] = 0;
	}

	Annot_icall( wn_origl, fb_info_origl );
	Annot_icall( wn_clone, fb_info_clone );
      }
#endif
    }
    break;

  fb_opr_cases_switch:
    {
      FB_Info_Switch fb_info_origl = Query_switch( wn_origl );
      FB_Info_Switch fb_info_clone = fb_info_origl;
      for ( INT t = fb_info_origl.size() - 1; t >= 0; t-- ) {
	fb_info_clone[t] *= freq_scale;
	fb_info_origl[t] -= fb_info_clone[t];
      }
      Annot_switch( wn_origl, fb_info_origl );
      Annot_switch( wn_clone, fb_info_clone );
    }
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn_origl ) ){
      FB_Info_Value fb_info_origl = Query_value( wn_origl );
      FB_Info_Value fb_info_clone = fb_info_origl;

      fb_info_clone.exe_counter *= freq_scale;
      fb_info_origl.exe_counter -= fb_info_clone.exe_counter;
      for( int i = 0; i < fb_info_origl.num_values; i++ ){
	fb_info_clone.freq[i] *= freq_scale;
	fb_info_origl.freq[i] -= fb_info_clone.freq[i];
      }

      Annot_value( wn_origl, fb_info_origl );
      Annot_value( wn_clone, fb_info_clone );

    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn_origl ) ){
      FB_Info_Value_FP_Bin fb_info_origl = Query_value_fp_bin( wn_origl );
      FB_Info_Value_FP_Bin fb_info_clone = fb_info_origl;

      fb_info_clone.exe_counter *= freq_scale;
      fb_info_clone.zopnd0      *= freq_scale;
      fb_info_clone.zopnd1      *= freq_scale;
      fb_info_clone.uopnd0      *= freq_scale;
      fb_info_clone.uopnd1      *= freq_scale;
      fb_info_origl.exe_counter -= fb_info_clone.exe_counter;
      fb_info_origl.zopnd0      -= fb_info_clone.zopnd0;
      fb_info_origl.zopnd1      -= fb_info_clone.zopnd1;
      fb_info_origl.uopnd0      -= fb_info_clone.uopnd0;
      fb_info_origl.uopnd1      -= fb_info_clone.uopnd1;

      Annot_value_fp_bin( wn_origl, fb_info_origl );
      Annot_value_fp_bin( wn_clone, fb_info_clone );

    }
    break;
#endif

  default:
    break;
  }
}

void
FEEDBACK::FB_clone( WN *wn_origl, WN *wn_clone, FB_FREQ freq_scale )
{
  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::FB_clone(0x%p, 0x%p", wn_origl, wn_clone );
    freq_scale.Print( TFile );
    fprintf( TFile, "):\n" );
  }

  for ( TREE_ITER origl( wn_origl ), clone( wn_clone );
	origl.Wn() != NULL && clone.Wn() != NULL;
	++origl, ++clone ) {

    // Clone the node data
    FB_clone_node( origl.Wn(), clone.Wn(), freq_scale );
  }
}


void
FEEDBACK::FB_clone_test( WN *wn_origl, WN *wn_clone,
			 FB_FREQ freq_origl_taken, FB_FREQ freq_origl_not,
			 FB_FREQ freq_clone_taken, FB_FREQ freq_clone_not )
{
  if ( ! ( freq_origl_taken.Known() && freq_origl_not.Known() &&
	   freq_clone_taken.Known() && freq_clone_not.Known() )) {

    DevWarn( "FEEDBACK::FB_clone_test found unknown frequency" );

    // Guess a half-half split
    FB_clone( wn_origl, wn_clone, FB_FREQ( 0.5, false ) );
    return;
  }

  // First, annotate the original and duplicate whirl nodes
  OPERATOR opr = WN_operator( wn_origl );
  switch( opr ) {

  case OPR_LNOT:
    FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		   freq_origl_not, freq_origl_taken,
		   freq_clone_not, freq_clone_taken );
    break;

  case OPR_CAND:
    {
      FB_FREQ freq_scale =
	freq_clone_not / ( freq_origl_not + freq_clone_not );
      if ( ! freq_scale.Known() )
	freq_scale = FB_FREQ( 0.5, false );  // Guess 0.5

      // Update the CAND frequencies
      FB_Info_Circuit fb_info_origl = Query_circuit( wn_origl );
      FB_Info_Circuit fb_info_clone( fb_info_origl.freq_left  * freq_scale,
				     fb_info_origl.freq_right * freq_scale,
				     freq_clone_taken );
      fb_info_origl.freq_left    -= fb_info_clone.freq_left   ;
      fb_info_origl.freq_right   -= fb_info_clone.freq_right  ;
      fb_info_origl.freq_neither -= fb_info_clone.freq_neither;
      Annot_circuit( wn_origl, fb_info_origl );
      Annot_circuit( wn_clone, fb_info_clone );

      // Update the two kids
      FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		     fb_info_origl.freq_right + fb_info_origl.freq_neither,
		     fb_info_origl.freq_left,
		     fb_info_clone.freq_right + fb_info_clone.freq_neither,
		     fb_info_clone.freq_left );
      FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		     fb_info_origl.freq_neither, fb_info_origl.freq_right,
		     fb_info_clone.freq_neither, fb_info_clone.freq_right );
    }
    break;

  case OPR_CIOR:
    {
      FB_FREQ freq_scale =
	freq_clone_taken / ( freq_origl_taken + freq_clone_taken );
      if ( ! freq_scale.Known() )
	freq_scale = FB_FREQ( 0.5, false );  // Guess 0.5

      // Update the CIOR frequencies
      FB_Info_Circuit fb_info_origl = Query_circuit( wn_origl );
      FB_Info_Circuit fb_info_clone( fb_info_origl.freq_left  * freq_scale,
				     fb_info_origl.freq_right * freq_scale,
				     freq_clone_not );
      fb_info_origl.freq_left    -= fb_info_clone.freq_left   ;
      fb_info_origl.freq_right   -= fb_info_clone.freq_right  ;
      fb_info_origl.freq_neither -= fb_info_clone.freq_neither;
      Annot_circuit( wn_origl, fb_info_origl );
      Annot_circuit( wn_clone, fb_info_clone );

      // Update the two kids
      FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		     fb_info_origl.freq_left,
		     fb_info_origl.freq_right + fb_info_origl.freq_neither,
		     fb_info_clone.freq_left,
		     fb_info_clone.freq_right + fb_info_clone.freq_neither );
      FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		     fb_info_origl.freq_right, fb_info_origl.freq_neither,
		     fb_info_clone.freq_right, fb_info_clone.freq_neither );
    }
    break;

  case OPR_COMMA:
    FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		   freq_origl_taken, freq_origl_not,
		   freq_clone_taken, freq_clone_not );
    FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		   freq_origl_taken, freq_origl_not,
		   freq_clone_taken, freq_clone_not );
    break;

  case OPR_RCOMMA: // NOTE: This is not quite right
    FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		   freq_origl_taken, freq_origl_not,
		   freq_clone_taken, freq_clone_not );
    FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		   freq_origl_taken, freq_origl_not,
		   freq_clone_taken, freq_clone_not );
    break;

  case OPR_CSELECT:
    {
      // Compute the scale factor
      FB_FREQ freq_origl_total = freq_origl_taken + freq_origl_not;
      FB_FREQ freq_clone_total = freq_clone_taken + freq_clone_not;
      FB_FREQ freq_scale =
	freq_clone_total / ( freq_origl_total + freq_clone_total );
      if ( ! freq_scale.Known() )
	freq_scale = FB_FREQ( 0.5, false );  // Guess 0.5

      FB_Info_Branch fb_info_origl = Query_branch( wn_origl );

      // Calculate the scale for kid1
      FB_FREQ freq_scale_kid1 =
	fb_info_origl.freq_taken / fb_info_origl.Total();
      if ( ! freq_scale_kid1.Known() )
	freq_scale_kid1 = FB_FREQ( 0.5, false );  // Guess 0.5

      // Scale the CSELECT
      FB_Info_Branch
	fb_info_clone( fb_info_origl.freq_taken     * freq_scale,
		       fb_info_origl.freq_not_taken * freq_scale );
      fb_info_origl.freq_taken     -= fb_info_clone.freq_taken    ;
      fb_info_origl.freq_not_taken -= fb_info_clone.freq_not_taken;
      Annot_branch( wn_origl, fb_info_origl );
      Annot_branch( wn_clone, fb_info_clone );

      // Compute the scaled frequencies for kid1 and kid2
      FB_FREQ freq_kid1_origl_taken = freq_origl_taken * freq_scale_kid1;
      FB_FREQ freq_kid1_origl_not   = freq_origl_not   * freq_scale_kid1;
      FB_FREQ freq_kid1_clone_taken = freq_clone_taken * freq_scale_kid1;
      FB_FREQ freq_kid1_clone_not   = freq_clone_not   * freq_scale_kid1;
      freq_origl_taken -= freq_kid1_origl_taken;
      freq_origl_not   -= freq_kid1_origl_not;
      freq_clone_taken -= freq_kid1_clone_taken;
      freq_clone_not   -= freq_kid1_clone_not;

      // Scale the three kids
      FB_clone( WN_kid0( wn_origl ), WN_kid0( wn_clone ), freq_scale );
      FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		     freq_kid1_origl_taken, freq_kid1_origl_not,
		     freq_kid1_clone_taken, freq_kid1_clone_not );

      FB_clone_test( WN_kid2( wn_origl ), WN_kid2( wn_clone ),
		     freq_origl_taken, freq_origl_not,
		     freq_clone_taken, freq_clone_not );
    }
    break;

  default:
    {
      // Compute the scale factor
      FB_FREQ freq_origl_total = freq_origl_taken + freq_origl_not;
      FB_FREQ freq_clone_total = freq_clone_taken + freq_clone_not;
      FB_FREQ freq_scale =
	freq_clone_total / ( freq_origl_total + freq_clone_total );
      if ( ! freq_scale.Known() )
	freq_scale = FB_FREQ( 0.5, false );  // Guess 0.5

      FB_clone( wn_origl, wn_clone, freq_scale );
    }
    break;
  }
}


void
FEEDBACK::FB_clone_loop_test( WN *wn_origl, WN *wn_clone, WN *wn_loop )
{
  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::FB_clone_loop_test(0x%p, 0x%p, 0x%p):\n",
	     wn_origl, wn_clone, wn_loop );
    Print_with_wn( TFile, wn_origl );
    Print_with_wn( TFile, wn_loop );
  }
  const FB_Info_Loop& fb_info = Query_loop( wn_loop );

  if ( fb_info.freq_back.Known()     && fb_info.freq_out.Known() &&
       fb_info.freq_positive.Known() && fb_info.freq_zero.Known() ) {
    FB_clone_test( wn_origl, wn_clone,
		   fb_info.freq_back,     fb_info.freq_out,
		   fb_info.freq_positive, fb_info.freq_zero );
  } else {
    // Guess that the loop iterates an average of eight times
    FB_clone( wn_origl, wn_clone, FB_FREQ( 0.125, false ) );
  }
}


// ====================================================================
// IPA Cloning and Inlining
// ====================================================================
      
void
FB_IPA_Clone_node( FEEDBACK *feedback_origl, FEEDBACK *feedback_clone,
		   WN             *wn_origl, WN             *wn_clone,
		   FB_FREQ freq_scale )
{
/*
  Is_True( freq_scale.Known() &&
	   freq_scale.Value() >= 0.0 && freq_scale.Value() <= 1.0,
	   ( "FEEDBACK::FB_IPA_Clone_node: freq_scale == %f",
	     freq_scale.Value() ) );
*/

  Is_True( feedback_origl != NULL,
	   ( "FEEDBACK::FB_IPA_Clone_node: feedback_origl == NULL" ) );

  // First, annotate the original and clone whirl nodes
  switch( WN_operator( wn_origl ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn_origl ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info_origl = feedback_origl->Query_invoke( wn_origl );
      FB_Info_Invoke fb_info_clone( fb_info_origl.freq_invoke * freq_scale );
      fb_info_origl.freq_invoke -= fb_info_clone.freq_invoke;
      feedback_origl->Annot_invoke( wn_origl, fb_info_origl );
      feedback_clone->Annot_invoke( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info_origl = feedback_origl->Query_branch( wn_origl );
      FB_Info_Branch fb_info_clone( fb_info_origl.freq_taken     * freq_scale,
				    fb_info_origl.freq_not_taken * freq_scale );
      fb_info_origl.freq_taken     -= fb_info_clone.freq_taken    ;
      fb_info_origl.freq_not_taken -= fb_info_clone.freq_not_taken;
      feedback_origl->Annot_branch( wn_origl, fb_info_origl );
      feedback_clone->Annot_branch( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info_origl = feedback_origl->Query_loop( wn_origl );
      FB_Info_Loop fb_info_clone( fb_info_origl.freq_zero     * freq_scale,
				  fb_info_origl.freq_positive * freq_scale,
				  fb_info_origl.freq_out      * freq_scale,
				  fb_info_origl.freq_back     * freq_scale,
				  fb_info_origl.freq_exit     * freq_scale,
				  fb_info_origl.freq_iterate  * freq_scale );
      fb_info_origl.freq_zero     -= fb_info_clone.freq_zero    ;
      fb_info_origl.freq_positive -= fb_info_clone.freq_positive;
      fb_info_origl.freq_out      -= fb_info_clone.freq_out     ;
      fb_info_origl.freq_back     -= fb_info_clone.freq_back    ;
      fb_info_origl.freq_exit     -= fb_info_clone.freq_exit    ;
      fb_info_origl.freq_iterate  -= fb_info_clone.freq_iterate ;
      feedback_origl->Annot_loop( wn_origl, fb_info_origl );
      feedback_clone->Annot_loop( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit
	fb_info_origl = feedback_origl->Query_circuit( wn_origl );
      FB_Info_Circuit
	fb_info_clone( fb_info_origl.freq_left    * freq_scale,
		       fb_info_origl.freq_right   * freq_scale,
		       fb_info_origl.freq_neither * freq_scale );
      fb_info_origl.freq_left    -= fb_info_clone.freq_left   ;
      fb_info_origl.freq_right   -= fb_info_clone.freq_right  ;
      fb_info_origl.freq_neither -= fb_info_clone.freq_neither;
      feedback_origl->Annot_circuit( wn_origl, fb_info_origl );
      feedback_clone->Annot_circuit( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info_origl = feedback_origl->Query_call( wn_origl );
      FB_Info_Call fb_info_clone( fb_info_origl.freq_entry * freq_scale,
				  fb_info_origl.freq_exit  * freq_scale );
      fb_info_origl.freq_entry -= fb_info_clone.freq_entry;
      fb_info_origl.freq_exit  -= fb_info_clone.freq_exit;
      feedback_origl->Annot_call( wn_origl, fb_info_origl );
      feedback_clone->Annot_call( wn_clone, fb_info_clone );

#ifdef KEY
      if( WN_operator(wn_origl) == OPR_ICALL ){
	FB_Info_Icall fb_info_origl = feedback_origl->Query_icall( wn_origl );
	const float scale = freq_scale.Value();
	FB_Info_Icall fb_info_clone = fb_info_origl;

	fb_info_clone.tnv._exec_counter =
	  (UINT64)(fb_info_origl.tnv._exec_counter * scale);
	fb_info_origl.tnv._exec_counter -= fb_info_clone.tnv._exec_counter;

	for( int i = 0; i < FB_TNV_SIZE; i ++ ){
	  if( fb_info_origl.tnv._values[i] == 0 )
	    break;
	  fb_info_clone.tnv._counters[i] =
	    (UINT64)(fb_info_origl.tnv._counters[i] * scale);
	  if( fb_info_origl.tnv._counters[i] > fb_info_clone.tnv._counters[i] )
	    fb_info_origl.tnv._counters[i] -= fb_info_clone.tnv._counters[i];
	  else
	    fb_info_origl.tnv._counters[i] = 0;
	}

	feedback_origl->Annot_icall( wn_origl, fb_info_origl );
	feedback_clone->Annot_icall( wn_clone, fb_info_clone );
      }
#endif // KEY
    }
    break;

  fb_opr_cases_switch:
    {
      FB_Info_Switch fb_info_origl = feedback_origl->Query_switch( wn_origl );
      FB_Info_Switch fb_info_clone = fb_info_origl;
      for ( INT32 t = fb_info_origl.size() - 1; t >= 0; --t ) {
	fb_info_clone[t] *= freq_scale;
	fb_info_origl[t] -= fb_info_clone[t];
      }
      feedback_origl->Annot_switch( wn_origl, fb_info_origl );
      feedback_clone->Annot_switch( wn_clone, fb_info_clone );
    }
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn_origl ) ){
      FB_Info_Value fb_info_origl = feedback_origl->Query_value( wn_origl );
      FB_Info_Value fb_info_clone = fb_info_origl;

      fb_info_clone.exe_counter = fb_info_origl.exe_counter * freq_scale;
      fb_info_origl.exe_counter -= fb_info_clone.exe_counter;

      for( int i = 0; i < fb_info_origl.num_values; i++ ){
	fb_info_clone.freq[i] = fb_info_origl.freq[i] * freq_scale;
	fb_info_origl.freq[i] -= fb_info_clone.freq[i];
      }

      feedback_origl->Annot_value( wn_origl, fb_info_origl );
      feedback_clone->Annot_value( wn_clone, fb_info_clone );

    }
    break;

  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn_origl ) ){
      FB_Info_Value_FP_Bin fb_info_origl = 
	feedback_origl->Query_value_fp_bin( wn_origl );
      FB_Info_Value_FP_Bin fb_info_clone = fb_info_origl;

      fb_info_clone.exe_counter = fb_info_origl.exe_counter * freq_scale;
      fb_info_clone.zopnd0      = fb_info_clone.zopnd0      * freq_scale;
      fb_info_clone.zopnd1      = fb_info_clone.zopnd1      * freq_scale;
      fb_info_clone.uopnd0      = fb_info_clone.uopnd0      * freq_scale;
      fb_info_clone.uopnd1      = fb_info_clone.uopnd1      * freq_scale;
      fb_info_origl.exe_counter -= fb_info_clone.exe_counter;
      fb_info_origl.zopnd0      -= fb_info_clone.zopnd0;
      fb_info_origl.zopnd1      -= fb_info_clone.zopnd1;
      fb_info_origl.uopnd0      -= fb_info_clone.uopnd0;
      fb_info_origl.uopnd1      -= fb_info_clone.uopnd1;

      feedback_origl->Annot_value_fp_bin( wn_origl, fb_info_origl );
      feedback_clone->Annot_value_fp_bin( wn_clone, fb_info_clone );

    }
    break;
#endif // KEY

  default:
    break;
  }
}


void
FB_IPA_Clone( FEEDBACK *feedback_origl, FEEDBACK *feedback_clone,
	      WN             *wn_origl, WN             *wn_clone,
	      FB_FREQ freq_scale )
{
/*
  Is_True( freq_scale.Known() &&
	   freq_scale.Value() >= 0.0 && freq_scale.Value() <= 1.0,
	   ( "FEEDBACK::FB_IPA_Clone: freq_scale == %f",
	     freq_scale.Value() ) );
*/

  if ( feedback_origl == NULL ) {
    if ( freq_scale.Exact() && freq_scale.Zero() ){
#ifdef KEY
      if( feedback_clone != NULL )
#endif
	feedback_clone->FB_set_zero( wn_clone );
    }
    return;
  }

  for ( TREE_ITER origl( wn_origl ), clone( wn_clone );
	origl.Wn() != NULL && clone.Wn() != NULL;
	++origl, ++clone ) {

    // Clone the node data
    FB_IPA_Clone_node( feedback_origl, feedback_clone,
		       origl.Wn(),     clone.Wn(),     freq_scale );
  }
}


void
FB_IPA_Inline( FEEDBACK *feedback_origl, FEEDBACK *feedback_inlin,
	       WN             *wn_origl, WN             *wn_inlin,
	       FB_FREQ freq_scale )
{
  Is_True( WN_operator( wn_origl ) == OPR_FUNC_ENTRY,
	   ( "FEEDBACK::FB_IPA_Inline: wn_origl should be FUNC_ENTRY" ) );

  FB_FREQ freq_origl = FB_FREQ( 1, true ) - freq_scale;

  WN *func_body = WN_func_body( wn_origl );

  if ( feedback_origl ) {

    // Scale the FUNC_ENTRY
    FB_Info_Invoke fb_info = feedback_origl->Query_invoke( wn_origl );
    fb_info.freq_invoke *= freq_origl;
    feedback_origl->Annot_invoke( wn_origl, fb_info );

    // Scale kids other than function body
    for ( INT32 t = 0; t < WN_kid_count( wn_origl ); ++t )
      if ( WN_kid( wn_origl, t ) != func_body )
	feedback_origl->FB_scale( WN_kid( wn_origl, t ), freq_origl );
  }

  // Clone the function body
  FB_IPA_Clone( feedback_origl, feedback_inlin,
		func_body, wn_inlin, freq_scale );
}


// ====================================================================
// Transfer FB from one PU's FEEDBACK object to another, for MP lowerer
// ====================================================================
      
void
FB_Transfer_node(FEEDBACK *feedback_origl, FEEDBACK *feedback_new, WN *wn)
{
  switch( WN_operator( wn ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;  // process only WN_PRAGMA_PREAMBLE_END

  fb_opr_cases_invoke:
    feedback_new->Annot_invoke(wn, feedback_origl->Query_invoke(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_branch:
    feedback_new->Annot_branch(wn, feedback_origl->Query_branch(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_loop:
    feedback_new->Annot_loop(wn, feedback_origl->Query_loop(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_circuit:
    feedback_new->Annot_circuit(wn, feedback_origl->Query_circuit(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_call:
    feedback_new->Annot_call(wn, feedback_origl->Query_call(wn));
#ifdef KEY
    if( WN_operator(wn) == OPR_ICALL ){
      feedback_new->Annot_icall(wn, feedback_origl->Query_icall(wn));    
    }
#endif
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_switch:
    feedback_new->Annot_switch(wn, feedback_origl->Query_switch(wn));
    feedback_origl->Delete(wn);
    break;

#ifdef KEY
  fb_opr_cases_value:
    if( IS_VALID_DIV_VALUE( wn ) ){
      feedback_new->Annot_value( wn, feedback_origl->Query_value(wn) );
      feedback_origl->Delete(wn);
    }
    break;
      
  fb_opr_cases_value_fp_bin:
    if( IS_VALID_MPY_VALUE( wn ) ){
      feedback_new->Annot_value_fp_bin(wn, 
				       feedback_origl->Query_value_fp_bin(wn));
      feedback_origl->Delete(wn);
    }
    break;
#endif

  default:
    break;
  }
} // FB_Transfer_node


void
FB_Transfer(FEEDBACK *feedback_origl, FEEDBACK *feedback_new, WN *wn)
{
  Is_True(feedback_origl, ("NULL feedback_origl"));
  Is_True(feedback_new, ("NULL feedback_new"));
  Is_True(wn, ("NULL wn"));

  for ( TREE_ITER it (wn) ; it.Wn() != NULL; ++it ) {
    FB_Transfer_node(feedback_origl, feedback_new, it.Wn());
  }

} // FB_Transfer

// for interactive debugging, call this from dbx
void dump_fb ( const FEEDBACK *feedback, const WN *wn )
{
  feedback->Print( stdout, wn );
  fflush(stdout);
}


// ====================================================================
// Verifier and DaVinci FB CFG Viewer
// ====================================================================

FB_VERIFY_STATUS
FEEDBACK::Verify( const char *caller, bool abort_if_error ) const
{
  Is_True( this != NULL, ( "FEEDBACK::Verify encountered NULL FEEDBACK" ) );

  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK::Verify (%s)\n", caller );
    fdump_tree_with_freq( TFile, _root_wn, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
  }

  FB_CFG fb_cfg;
  fb_cfg.Construct_from_whirl( _root_wn, caller );
  FB_VERIFY_STATUS fb_status = fb_cfg.Verify_frequencies();
  fb_cfg.Patch_whirl_frequencies();

  switch( fb_status ) {
  case FB_VERIFY_CONSISTENT:
    break;
  case FB_VERIFY_UNBALANCED:
    DevWarn( "Feedback unbalanced %s", caller );
    break;
  case FB_VERIFY_INVALID:
    DevWarn( "Feedback invalid %s", caller );
    break;
  default:
    Is_True( false, ( "FEEDBACK::Verify found unexpected branch" ) );
    break;
  }

  return fb_status;
}


FB_VERIFY_STATUS
FEEDBACK::Verify_and_guess( const char *caller, bool abort_if_error ) const
{
  Is_True( this != NULL,
	   ( "FEEDBACK::Verify_and_guess encountered NULL FEEDBACK" ) );

  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK::Verify_and_guess (%s)\n", caller );
    fdump_tree_with_freq( TFile, _root_wn, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
  }

  FB_CFG fb_cfg;
  fb_cfg.Construct_from_whirl( _root_wn, caller );
  fb_cfg.Guess_unknowns( _root_wn, caller );
  FB_VERIFY_STATUS fb_status = fb_cfg.Verify_frequencies();
  fb_cfg.Patch_whirl_frequencies();

  return fb_status;
}


void
FEEDBACK::Display_FB_CFG_From_Whirl( const char *caller )
{
  if ( this == NULL || ! DaVinci::enabled( true ) ) {
    return;
  }
  FB_CFG fb_cfg;
  fb_cfg.Construct_from_whirl( _root_wn, caller );

  dV_view_fb_cfg( fb_cfg, _root_wn, caller );
}


// ====================================================================
// Convert the Feedback info from the internal form to the file format
// ====================================================================

INT
Convert_Feedback_Info (const FEEDBACK* fb, const WN* tree,
		       PU_Profile_Handle& pu_handle)
{
  Is_True (fb != NULL && tree != NULL,
	   ("Convert_Feedback_Info: invalid FEEDBACK* or WN* "));
  
  INT count = 0;
  for ( CONST_TREE_ITER iter (tree); iter.Wn () != NULL; ++iter ) {
    const WN* wn = iter.Wn ();

    switch ( WN_operator( wn ) ) {

    case OPR_PRAGMA:
      if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
	break;
      // else fall through

    fb_opr_cases_invoke:
      pu_handle.Get_Invoke_Table ().push_back (fb->Query_invoke (wn));
      ++count;
      break;

    fb_opr_cases_branch:
      pu_handle.Get_Branch_Table ().push_back (fb->Query_branch (wn));
      ++count;
      break;

    fb_opr_cases_loop:
      pu_handle.Get_Loop_Table ().push_back (fb->Query_loop (wn));
      ++count;
      break;

    fb_opr_cases_circuit:
      pu_handle.Get_Short_Circuit_Table ().push_back (fb->Query_circuit (wn));
      ++count;
      break;

    fb_opr_cases_call:
      pu_handle.Get_Call_Table ().push_back (fb->Query_call (wn));
#ifdef KEY
      if( WN_operator(wn) == OPR_ICALL ){
	pu_handle.Get_Icall_Table ().push_back (fb->Query_icall (wn));

	/* Verify the icall counters before passing this info to next phases.
	 */
	const FB_Info_Icall& info_icall = Cur_PU_Feedback->Query_icall(wn);

	if( !info_icall.Is_uninit() ){
	  const FB_Info_Call& info_call = Cur_PU_Feedback->Query_call(wn);
	  FmtAssert( info_icall.tnv._exec_counter >= info_icall.tnv._counters[0],
		     ("icall exec counters don't match") );
	}
      }
#endif // KEY
      ++count;
      break;

    fb_opr_cases_switch:
      pu_handle.Get_Switch_Table ().push_back (fb->Query_switch (wn));
      ++count;
      break;

#ifdef KEY
    fb_opr_cases_value:
      if( IS_VALID_DIV_VALUE( wn ) ){
	pu_handle.Get_Value_Table ().push_back (fb->Query_value (wn));
	++count;
	break;
      }

    fb_opr_cases_value_fp_bin:
      if( IS_VALID_MPY_VALUE( wn ) ){
	pu_handle.Get_Value_FP_Bin_Table().push_back(fb->Query_value_fp_bin (wn));
	++count;
	break;
      }
#endif

    default:
      break;
    }
  }
  return count;
} // Convert_Feedback_Info


// Convert the Feedback info from the file buffer to internal format
void
Read_Feedback_Info (FEEDBACK* fb, WN* tree, const Pu_Hdr& pu_hdr)
{
  Is_True (fb != NULL && tree != NULL,
	   ("Read_Feedback_Info: invalid FEEDBACK* or WN* "));

  const char * baseaddr = (const char *) (&pu_hdr);
  const FB_Info_Invoke* fb_invoke =
    (const FB_Info_Invoke*) (baseaddr + pu_hdr.pu_inv_offset);
  const FB_Info_Invoke* fb_invoke_last = fb_invoke + pu_hdr.pu_num_inv_entries;

  const FB_Info_Branch* fb_branch =
    (const FB_Info_Branch*) (baseaddr + pu_hdr.pu_br_offset);
  const FB_Info_Branch* fb_branch_last = fb_branch + pu_hdr.pu_num_br_entries;

  const FB_FREQ* fb_switch =
    (const FB_FREQ*) (baseaddr + pu_hdr.pu_switch_offset);
  const INT32* fb_switch_target =
    (const INT32*) (baseaddr + pu_hdr.pu_switch_target_offset);
  const INT32* fb_switch_target_last =
      fb_switch_target + pu_hdr.pu_num_switch_entries;

  const FB_Info_Loop* fb_loop =
    (const FB_Info_Loop*) (baseaddr + pu_hdr.pu_loop_offset);
  const FB_Info_Loop* fb_loop_last = fb_loop + pu_hdr.pu_num_loop_entries;

  const FB_Info_Circuit* fb_circuit =
    (const FB_Info_Circuit*) (baseaddr + pu_hdr.pu_scircuit_offset);
  const FB_Info_Circuit* fb_circuit_last =
    fb_circuit + pu_hdr.pu_num_scircuit_entries;

#ifdef KEY
  const FB_Info_Icall* fb_icall =
    (const FB_Info_Icall*) (baseaddr + pu_hdr.pu_icall_offset);
  const FB_Info_Icall* fb_icall_last = fb_icall + pu_hdr.pu_num_icall_entries;

  const FB_Info_Value* fb_value =
    (const FB_Info_Value*) (baseaddr + pu_hdr.pu_value_offset);
  const FB_Info_Value* fb_value_last = fb_value + pu_hdr.pu_num_value_entries;

  const FB_Info_Value_FP_Bin* fb_value_fp_bin =
    (const FB_Info_Value_FP_Bin*) (baseaddr + pu_hdr.pu_value_fp_bin_offset);
  const FB_Info_Value_FP_Bin* fb_value_fp_bin_last = fb_value_fp_bin + 
    pu_hdr.pu_num_value_fp_bin_entries;
#endif

  const FB_Info_Call* fb_call =
    (const FB_Info_Call*) (baseaddr + pu_hdr.pu_call_offset);
  const FB_Info_Call* fb_call_last = fb_call + pu_hdr.pu_num_call_entries;

  INT count = 0;
  
  for ( TREE_ITER iter (tree); iter.Wn () != NULL; ++iter ) {
    WN* wn = iter.Wn ();

    switch ( WN_operator( wn ) ) {

    case OPR_PRAGMA:
      if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
	break;
      // else fall through

    fb_opr_cases_invoke:
      fb->Annot_invoke( wn, *fb_invoke );
      ++fb_invoke;
      ++count;
      break;

    fb_opr_cases_branch:
      fb->Annot_branch( wn, *fb_branch );
      ++fb_branch;
      ++count;
      break;

    fb_opr_cases_loop:
      fb->Annot_loop( wn, *fb_loop );
      ++fb_loop;
      ++count;
      break;

    fb_opr_cases_circuit:
      fb->Annot_circuit( wn, *fb_circuit );
      ++fb_circuit;
      ++count;
      break;

    fb_opr_cases_call:
      fb->Annot_call( wn, *fb_call );
      ++fb_call;
#ifdef KEY
      if( WN_operator(wn) == OPR_ICALL ){
	fb->Annot_icall( wn, *fb_icall );
	++fb_icall;
      }
#endif // KEY
      ++count;
      break;

    fb_opr_cases_switch:
      {
	FB_Info_Switch info;
	info.freq_targets.insert( info.freq_targets.begin (),
				  fb_switch, fb_switch + *fb_switch_target );
	fb->Annot_switch( wn, info );
	fb_switch += *fb_switch_target;
	++fb_switch_target;
	++count;
      }
      break;

#ifdef KEY
    fb_opr_cases_value:
      if( IS_VALID_DIV_VALUE( wn ) ){
	fb->Annot_value( wn, *fb_value );
	++fb_value;
	++count;
	break;
      }

    fb_opr_cases_value_fp_bin:
      if( IS_VALID_MPY_VALUE( wn ) ){
	fb->Annot_value_fp_bin( wn, *fb_value_fp_bin );
	++fb_value_fp_bin;
	++count;
	break;
      }
#endif

    default:
      break;
    }
  }

#ifdef KEY
  FmtAssert( count == pu_hdr.pu_checksum &&
	     fb_invoke == fb_invoke_last &&
	     fb_branch == fb_branch_last &&
	     fb_value  == fb_value_last  &&
	     fb_value_fp_bin  == fb_value_fp_bin_last  &&
	     fb_switch_target == fb_switch_target_last &&
	     fb_loop == fb_loop_last &&
	     fb_circuit == fb_circuit_last &&
	     fb_icall == fb_icall_last &&
	     fb_call == fb_call_last,
	     ("Feedback data is stale. Please rebuild the feedback data.") );
#else
  Is_True( count == pu_hdr.pu_checksum &&
	   fb_invoke == fb_invoke_last &&
	   fb_branch == fb_branch_last &&
	   fb_switch_target == fb_switch_target_last &&
	   fb_loop == fb_loop_last &&
	   fb_circuit == fb_circuit_last &&
	   fb_icall == fb_icall_last &&
	   fb_call == fb_call_last, ( "Error in reading Feedback info" ) );
#endif // KEY

} // Read_Feedback_Info

#if ! defined(BUILD_OS_DARWIN) /* Temporarily remove whirl browser */
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#include <elf.h>
#include <ctype.h>
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include <stdio.h>
#include "opt_du.h"
#include "opt_alias_mgr.h"
#include "dep_graph.h"
#include "ir_reader.h"
#include "wb_util.h"
#include "wb_buffer.h"
#include "wb_carray.h"
#include "wb_browser.h"
#include <wb.h>

void
wb_gwe(WN *wn_root)
{
  WB_BROWSER   wb;

  WB_Set_Phase( WBP_LOWER );
  WB_Initialize( &wb, wn_root, &Get_Current_PU(), NULL, NULL );
  wb.Sdebug( "" );
  WB_Set_Phase( WBP_NONE ); 
  WB_Terminate( &wb ); 
}
#endif
