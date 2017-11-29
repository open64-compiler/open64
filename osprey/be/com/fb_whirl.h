/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/* -*- c++ -*-
 *
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
// Module: fb_whirl.h
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.fb_whirl.h $
//
// Description:
//
// During instrumentation, certain events associated with program
// control flow (such as the number of times a particular branch is
// taken, or not taken) are counted.  During subsequent compilations,
// this data may be retrieved and used to guide optimization decisions.
//
// fb_whirl.h and fb_whirl.cxx define data structures and functions to
// manipulate this feedback information.
//
// When initially retrieved from the file of instrumentation data,
// frequencies count the number of times that certain events occurred
// during execution.
//
// Some whirl transformations require frequency values to be scaled
// or estimated.  Consequently, these values may not always be exact
// or even compatible.
//
// ====================================================================
// ====================================================================

#ifndef fb_whirl_INCLUDED
#define fb_whirl_INCLUDED

#include "fb_info.h"
#include "mempool_allocator.h"
#ifndef wn_INCLUDED
#include "wn.h"
#endif
#include <vector>             // STL vector.

#ifndef instr_reader_INCLUDED
#include "instr_reader.h"
#endif


// ====================================================================
// Tracing masks for TP_FEEDBACK tracing phase
// ====================================================================

#define	TP_FEEDBACK_WN         0x0001
#define TP_FEEDBACK_WN_DRAW    0x0002
#define TP_FEEDBACK_CFG        0x0010
#define TP_FEEDBACK_CFG_DRAW   0x0020
#define TP_FEEDBACK_CFG_BEFORE 0x0040
#define TP_FEEDBACK_CFG_PROP   0x0080
#define TP_OPT_FEEDBACK        0x0100
#define TP_OPT_FEEDBACK_DRAW   0x0200
#define TP_OPT_FEEDBACK_BEFORE 0x0400
#define TP_OPT_FEEDBACK_PROP   0x0800
#define TP_CG_FEEDBACK         0x1000
#define TP_CG_FEEDBACK_DRAW    0x2000


// ==================================================================
// Map to record <pu_runtime_address, pu_name> pair.
// 

template <class _Key> struct fbhash { };
template <> struct fbhash<UINT64> {
  size_t operator()(const UINT64 x)const{return (size_t)x;}
};

typedef hash_map<UINT64, char*, fbhash<UINT64> > ADDRESS_NAME_MAP;
typedef hash_map<UINT64, INT32, fbhash<UINT64> > ADDRESS_PUSIZE_MAP;


// ====================================================================

// FEEDBACK::Verify(...) and OPT_FEEDBACK::Verify(...) return one of
// the following values.  "UNBALANCED" means that some of the frequency
// values are UNKNOWN or do not add up, and "INVALID" indicates the
// presence of UNINITialized or ERROR frequencies.

enum FB_VERIFY_STATUS {
  FB_VERIFY_CONSISTENT,
  FB_VERIFY_UNBALANCED,
  FB_VERIFY_INVALID
};

// ====================================================================

class FEEDBACK {
#ifdef KEY
  friend class ARA_LOOP_INFO;
#endif
private:

  MEM_POOL   *_m;
  WN_MAP_TAB *_maptab;
  WN         *_root_wn;
  bool        _trace;      // Get_Trace(TP_FEEDBACK, TP_FEEDBACK_WN)
  bool        _trace_draw; // Get_Trace(TP_FEEDBACK, TP_FEEDBACK_WN_DRAW)

#ifdef KEY
  UINT64     _runtime_func_addr;  // run time function address
#endif

  FB_Info_Call _info_call;
  FB_Info_Icall _info_icall;
  WN *_last_wn_icall;

  // For Whirl nodes, the map WN_MAP_FEEDBACK holds an index into
  // one of the following vectors:

  vector< FB_Info_Invoke,  mempool_allocator<FB_Info_Invoke>  >  _invokes;
  vector< FB_Info_Branch,  mempool_allocator<FB_Info_Branch>  >  _branches;
  vector< FB_Info_Loop,    mempool_allocator<FB_Info_Loop>    >  _loops;
  vector< FB_Info_Circuit, mempool_allocator<FB_Info_Circuit> >  _circuits;
  vector< FB_Info_Call,    mempool_allocator<FB_Info_Call>    >  _calls;
  vector< FB_Info_Icall,   mempool_allocator<FB_Info_Icall>   >  _icalls;
  vector< FB_Info_Switch,  mempool_allocator<FB_Info_Switch>  >  _switches;
#ifdef KEY
  vector< FB_Info_Value,   mempool_allocator<FB_Info_Value>    >  _values;
  vector< FB_Info_Value_FP_Bin,mempool_allocator<FB_Info_Value_FP_Bin> > _values_fp_bin;
#endif

  INT32 Get_index_invoke  ( const WN *wn ) const;
  INT32 Get_index_branch  ( const WN *wn ) const;
  INT32 Get_index_loop    ( const WN *wn ) const;
  INT32 Get_index_circuit ( const WN *wn ) const;
  INT32 Get_index_call    ( const WN *wn ) const;
  INT32 Get_index_icall   ( const WN *wn ) const;
  INT32 Get_index_switch  ( const WN *wn ) const;
#ifdef KEY
  INT32 Get_index_value   ( const WN *wn ) const;
  INT32 Get_index_value_fp_bin( const WN *wn ) const;
#endif

  INT32 Add_index_invoke  ( WN *wn );
  INT32 Add_index_branch  ( WN *wn );
  INT32 Add_index_loop    ( WN *wn );
  INT32 Add_index_circuit ( WN *wn );
  INT32 Add_index_call    ( WN *wn );
  INT32 Add_index_icall   ( WN *wn );
  INT32 Add_index_switch  ( WN *wn );
#ifdef KEY
  INT32 Add_index_value   ( WN *wn );
  INT32 Add_index_value_fp_bin( WN *wn );
#endif

public:

  FEEDBACK( WN *wn, MEM_POOL *m,
	    INT32 invoke_size = 1,
	    INT32 branch_size = 1,
	    INT32 loop_size = 1,
	    INT32 circuit_size = 1,
	    INT32 call_size = 1,
	    INT32 icall_size = 1,
	    INT32 switch_size = 1,
#ifdef KEY
	    INT32 value_size = 1,
	    INT32 value_fp_bin_size = 1,
	    UINT64 runtime_fun_address = 0x0,
#endif
	    WN_MAP_TAB *maptab = Current_Map_Tab );

  void  Reset_Root_WN( WN *root_wn ) { _root_wn = root_wn; }

#ifdef KEY
  void Set_Runtime_Func_Addr( UINT64 addr ) { _runtime_func_addr = addr; }
  UINT64 Get_Runtime_Func_Addr() { return _runtime_func_addr; }
#endif

  bool  Same_in_out( const WN *wn );
  void  FB_set_in_out_same_node( WN *wn );
  void  FB_set_in_out_same( WN *wn );

#if defined(TARG_SL) && defined(TARG_SL2)
  void FB_reset_in_out_same_node( WN *wn );
#endif

  void  Print         ( FILE *fp, const WN *wn ) const;
  void  Print_with_wn ( FILE *fp, WN *wn ) const;

  const FB_Info_Invoke&  Query_invoke  ( const WN *wn ) const;
  const FB_Info_Branch&  Query_branch  ( const WN *wn ) const;
  const FB_Info_Loop&    Query_loop    ( const WN *wn ) const;
  const FB_Info_Circuit& Query_circuit ( const WN *wn ) const;
  const FB_Info_Call&    Query_call    ( const WN *wn ) const;
  const FB_Info_Icall&   Query_icall   ( const WN *wn ) const;
  const FB_Info_Switch&  Query_switch  ( const WN *wn ) const;
#ifdef KEY
  const FB_Info_Value&    Query_value   ( const WN *wn ) const;
  const FB_Info_Value_FP_Bin& Query_value_fp_bin( const WN *wn ) const;
#endif

  FB_FREQ Query      ( const WN *wn, const FB_EDGE_TYPE type ) const;
  FB_FREQ Query_prob ( const WN *wn, const FB_EDGE_TYPE type ) const;
  FB_FREQ Query_total_out ( const WN *wn ) const;

  void Annot_invoke  ( WN *wn, const FB_Info_Invoke & fb_info );
  void Annot_branch  ( WN *wn, const FB_Info_Branch & fb_info );
  void Annot_loop    ( WN *wn, const FB_Info_Loop   & fb_info );
  void Annot_circuit ( WN *wn, const FB_Info_Circuit& fb_info );
  void Annot_call    ( WN *wn, const FB_Info_Call   & fb_info );
  void Annot_icall   ( WN *wn, const FB_Info_Icall  & fb_info );
  void Annot_switch  ( WN *wn, const FB_Info_Switch & fb_info );
#ifdef KEY
  void Annot_value   ( WN *wn, const FB_Info_Value  & fb_info );
  void Annot_value_fp_bin( WN *wn, const FB_Info_Value_FP_Bin  & fb_info );
#endif

  void Annot         ( WN *wn, const FB_EDGE_TYPE type, FB_FREQ freq );

  // Resets feedback info to FB_FREQ_UNINIT; use when wn is deleted
  void Delete(WN *wn) {
    IPA_WN_MAP32_Set ( _maptab, WN_MAP_FEEDBACK, wn, 0 );
  }

  FB_VERIFY_STATUS Verify( const char *caller         = NULL,
			   bool        abort_if_error = TRUE ) const;

  FB_VERIFY_STATUS Verify_and_guess( const char *caller         = NULL,
				     bool        abort_if_error = TRUE ) const;

  // Feedback info in CG is carried by code-gen internal structures.
  // Verification for CG feedback info done apart from FEEDBACK class.

  // Lower feedback info

  void FB_split_cand_if ( WN *wn_outer_if, WN *wn_inner_if );
  void FB_lower_branch  ( WN *wn_br,   WN *wn_branch );
  void FB_lower_circuit ( WN *wn_cand, WN *wn_left_br, WN *wn_right_br );
  void FB_factor_circuit( WN *wn_left, WN *wn_right,
			  WN *wn_outer, WN *wn_inner );

  void FB_lower_loop     ( WN *wn_loop, WN *wn_top_br, WN *wn_back_br );
  void FB_lower_loop_alt ( WN *wn_loop, WN *wn_top_br );
  void FB_lower_while_do_to_do_while ( WN *wn_loop, WN *wn_top_br );

  void FB_lower_compgoto ( WN *wn_compgoto, WN *wn_xgoto, WN *wn_branch );

  void FB_lower_call ( WN *wn_call, WN *wn_new_call );
  void FB_lower_icall( WN *wn_icall, WN *wn_new_icall, WN * wn_new_call, WN * wn_new_if, int icall_no = 0 );
  void FB_lower_icall( FB_Info_Call &info_call, FB_Info_Icall &info_icall,
                       WN *wn_new_icall, WN * wn_new_call, WN * wn_new_if, 
                       int icall_no );
  void FB_lower_return_val ( WN *wn_return_val, WN *wn_return );

  void FB_lower_mstore_to_loop ( WN *wn_mstore, WN *wn_loop, INT64 nMoves );
  void FB_hoist_case( WN *wn_switch, vector<FB_FREQ>::size_type wcase);

  // Goto conversion

  void FB_move_goto_out( WN *wn_branch, WN *wn_inner_br, WN *wn_outer_br );
  void FB_convert_goto_to_if(   WN *wn_branch, WN *wn_if );
  void FB_convert_goto_to_loop( WN *wn_branch, WN *wn_loop );

  void FB_simplify_branch_to_goto( WN *wn_branch );

  // Cloning (clone freq = original freq * scale; original reduced by same)
  void FB_set_zero_node(    WN *wn );
  void FB_set_zero(         WN *wn );
  void FB_set_unknown_node( WN *wn );
  void FB_set_unknown(      WN *wn );
  void FB_scale_node(       WN *wn, FB_FREQ freq_scale );
  void FB_scale(            WN *wn, FB_FREQ freq_scale );
  void FB_duplicate_node(   WN *wn_origl, WN *wn_clone );
  void FB_duplicate(        WN *wn_origl, WN *wn_clone );
  void FB_recombine_node(   WN *wn_origl, WN *wn_clone );
  void FB_recombine(        WN *wn_origl, WN *wn_clone );
  void FB_clone_node(       WN *wn_origl, WN *wn_clone, FB_FREQ freq_scale );
  void FB_clone(            WN *wn_origl, WN *wn_clone, FB_FREQ freq_scale );

private:
  void FB_clone_test( WN *wn_origl, WN *wn_clone,
		      FB_FREQ freq_origl_taken, FB_FREQ freq_origl_not,
		      FB_FREQ freq_clone_taken, FB_FREQ freq_clone_not );
public:
  void FB_clone_loop_test( WN *wn_origl, WN *wn_clone, WN *wn_loop );

  void Display_FB_CFG_From_Whirl(const char *caller = NULL);

  // IPA interface

  friend void FB_IPA_Clone_node(
			    FEEDBACK *feedback_origl, FEEDBACK *feedback_clone,
			    WN             *wn_origl, WN             *wn_clone,
			    FB_FREQ freq_scale );

  friend void FB_IPA_Clone( FEEDBACK *feedback_origl, FEEDBACK *feedback_clone,
			    WN             *wn_origl, WN             *wn_clone,
			    FB_FREQ freq_scale );

  friend void FB_IPA_Clone( FEEDBACK *feedback_origl, FEEDBACK *feedback_clone,
			    WN             *wn_origl, WN             *wn_clone,
			    float scale ) {
    FB_IPA_Clone( feedback_origl, feedback_clone, wn_origl, wn_clone,
		  FB_FREQ( scale, false ) );
  }

  friend void FB_IPA_Inline(FEEDBACK *feedback_origl, FEEDBACK *feedback_clone,
			    WN             *wn_origl, WN             *wn_clone,
			    FB_FREQ freq_scale );

  // Transfer FB from one PU to another, for MP lowerer

  friend void FB_Transfer_node(
                          FEEDBACK *feedback_origl, FEEDBACK *feedback_new,
                          WN                   *wn);

  friend void FB_Transfer(FEEDBACK *feedback_origl, FEEDBACK *feedback_new,
                          WN                   *wn);

};

extern "C" void dump_fb ( const FEEDBACK *feedback, const WN *wn );

extern FEEDBACK *Cur_PU_Feedback;
 
extern ADDRESS_NAME_MAP PU_Addr_Name_Map;
extern ADDRESS_PUSIZE_MAP PU_Addr_Pusize_Map;

extern INT
Convert_Feedback_Info (const FEEDBACK* fb, const WN* tree,
		       PU_Profile_Handle& pu_handle);

extern void
Read_Feedback_Info (FEEDBACK* fb, WN* tree, const Pu_Hdr& pu_hdr);

// scale is portion of freq transfered to clone; original gets (1 - scale)

#ifdef __cplusplus
extern "C" {
#endif

void FB_old_Annotate_whirl(WN *);  // remove when new scheme ready ..

#ifdef __cplusplus
}
#endif

#define FB_DEVWARN_LIMIT  5

#endif
