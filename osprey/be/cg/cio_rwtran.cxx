/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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
// =======================================================================
// =======================================================================
//
//  Module: cio_rwtran.cxx
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/cio_rwtran.cxx,v $
//
//  Description:
//  ============
//  This module uses omega notation to perform several cross iteration
//  loop optimizations (CIO):
//
//  --> CIO copy removal eliminates move OPs by propagating the operand
//  for the result throughout the loop body and epilog backpatches.
//  For example:
//      loop {
//        TN4 <-- TN3[omega 1]
//        TN3 <-- ...
//        ... <-- TN4
//      }
//  becomes:
//      loop {
//        TN3 <-- ...
//        ... <-- TN3[omega 1]
//      }
//  This transformation has two benefits:  (1) On architectures with
//  rotating registers, the copy OP is eliminated (on other architectures,
//  the compiler will later reinsert the OP when it removes the omega
//  notation, see CG_LOOP_Remove_Notations in cg_loop.cxx).  (2) More
//  opportunities for CIO common subexpression elimination are enabled.
//
//  --> CIO read elimination eliminates some LOAD OPs, when the value
//  read from memory is available as a load or store from a previous loop
//  iteration.  For example:
//      for (i = 1; i < 1000; i++) {
//        TN4 = A[i - 1]
//        TN5 = A[i + 2]
//        ... = TN4
//      }
//  becomes:
//      TN5[omega 1] <-- A[2]
//      TN5[omega 2] <-- A[1]
//      TN5[omega 3] <-- A[0]
//      for (i = 1; i < 1000; i++) {
//        TN5 = A[i + 2]
//        ... = TN5[omega 3]
//      }
//  A similar transformation is possible when the load "TN5 = A[i + 2]" is
//  replaced by the store"A[i + 2] = TN5".
//
//  --> CIO write elimination eliminates some STORE OPs, when one store
//  overwrites another store from a previous loop iteration.  For example:
//      for (i = 1; i < 1000; i++) {
//        A[i - 1] = TN4
//        A[i + 2] = TN5
//      }
//  becomes:
//      for (i = 1; i < 1000; i++) {
//        A[i - 1] = TN4
//      }
//      A[1002] = TN5[omega 0]
//      A[1001] = TN5[omega 1]
//      A[1000] = TN5[omega 2]
//
//  --> CIO common subexpression elimination is an extension of CIO read
//  elimination that detects expressions whose values have already been
//  calculated during a previous iteration.  For example:
//      for (i = 1; i < 1000; i++) {
//        TN4 = A[i - 1] - 3 * B[i + 4]
//        TN5 = A[i + 2] - 3 * B[i + 7]
//        ... = TN4
//      }
//  becomes:
//      TN5[omega 1] <-- A[2] - 3 * B[7]
//      TN5[omega 2] <-- A[1] - 3 * B[6]
//      TN5[omega 3] <-- A[0] - 3 * B[5]
//      for (i = 1; i < 1000; i++) {
//        TN5 = A[i + 2] - 3 * B[i + 7]
//        ... = TN5[omega 3]
//      }
//
//  Currently, predication is only partially supported.  If predicated OPs
//  are encountered, then some CIO opportunities will not be detected.
//  For example, predicated move OPs will not be copy propagated, even if
//  it would be a legal transformation.
//
//  The code below does can handle full omega notation already present in
//  the loop body.
//
//  See "cio_rwtran.h" for the interface description.
//
// =======================================================================
// =======================================================================

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
static const char rcs_id[] = "";
#endif /* _KEEP_RCS_ID */

#include <alloca.h>
#include "defs.h"
#include "resource.h"
#include "config.h"
#include "errors.h"
#include "mempool.h"
#include "cg.h"
#include "cgir.h"
#include "tracing.h"
#include "cgtarget.h"
#include "cgprep.h"
#include "op.h"
#include "op_list.h"
#include "op_map.h"
#include "bb.h"
#include "register.h"
#include "bbregs.h"
#include "cg_loop.h"
#include "cgexp.h"
#include "annotations.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cg_cflow.h"
#include "whirl2ops.h"
#include "cg_db_op.h"
#include "cio.h"
#include "cio_rwtran.h"
#include "gra_live.h"
#include "data_layout.h"
#include <vector>
#include <map>
#include "cxx_memory.h"
#include "pqs_cg.h"



// ======================================================================
//
// Cross Iteration Loop Optimization options.
//
// CIO_enable_copy_removal  --> activates CIO copy removal
// CIO_enable_read_removal  --> activates CIO read elimination
// CIO_enable_cse_removal   --> activates CIO cicse elimination
// CIO_enable_write_removal --> activates CIO write elimination
//
// If read elimination is off, then cicse elimination also has no effect.
// If CG_opt_level < 2, then all loop optimizations are off.
//
// CIO_rw_max_omega         --> during read/write removal, ignore MEMARCs
//                              with omega > CIO_rwtran_max_omega
//
// ======================================================================


BOOL  CIO_enable_copy_removal  = TRUE;
BOOL  CIO_enable_read_removal  = TRUE;
#if defined(TARG_PPC32) // seems we sometimes run into the function Predicate_Write and generate an FmtAssertion
BOOL  CIO_enable_write_removal = FALSE;
#else
BOOL  CIO_enable_write_removal = TRUE;
#endif
BOOL  CIO_enable_cse_removal   = TRUE;
INT32 CIO_rw_max_omega         = 8;

#define CICSE_MAX_TRACE_LEVEL 3

// ======================================================================
//
// Obsolete code retained to jog David's memory
//
// ======================================================================


// QUESTION:  What should be the value of omega_limit for
//              CIO_Read_Write_Removal?
//
// Comments from old code:
//
// We'll disallow the transformation if we've already decided to or if
// it uses a truly unreasonable number of registers.  I've picked a
// pretty arbitrary but high limit here -- the total number number or
// registers in the class minus 4.  It seems like there is no way it
// would be a good idea to allow a more aggressive transformation (and
// probably this one is too aggrssive.)
//
// Compute the maximum omega which we will allow. This is the minimum
// of a predefined upper limit and the estimated trip count - 1.
// If the omega is higher than the trip count than we end up doing
// unnecessary work.
//
//
//   max_omega = CIO_rw_max_omega;
//   annot = ANNOT_Get(BB_annotations(body), ANNOT_LOOPINFO);
//   if ( annot ) {
//     LOOPINFO *loopinfo = ANNOT_loopinfo(annot);
//     if ( loopinfo ) {
//       WN *wn = LOOPINFO_wn(loopinfo);
//       if ( wn ) {
// 	INT32 trip_est = WN_loop_trip_est(wn);
// 	if ( trip_est && trip_est <= max_omega) max_omega = trip_est - 1;
//       }
//     }
//   }
//
//   if ( omega > max_omega )
//     return FALSE;
//
//   return omega < REGISTER_SET_Size(
// 		     REGISTER_CLASS_allocatable(TN_register_class(tn))) - 4;


// _op_ordering[]: Write        Write         Read         Read        Write
//                   |            |            |            |            |
// _arc_ordering[]:  |---MEMOUT-->|---MEMIN--->|--MEMREAD-->|--MEMANTI-->|
//                   |            |            |            |            |


// can_copy_op_to_prolog
//
// Return TRUE iff copy_op_to_prolog should be able to copy <op>
// from loop <body> to its prolog.
//
// static BOOL 
// can_copy_op_to_prolog( BB *body, OP *op )


// static void
// remove_invariant_backpatches( BB *body )
// {
//   CG_LOOP_BACKPATCH *bp = CG_LOOP_Backpatch_First( CG_LOOP_prolog, NULL );
//   while ( bp ) {
//     CG_LOOP_BACKPATCH *next_bp = CG_LOOP_Backpatch_Next( bp );
//     TN *body_tn = CG_LOOP_BACKPATCH_body_tn( bp );

//     if ( ! CG_DEF_OP( body_tn, body, in_same_bb ) ) {
//       TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn( bp );
//       replace_tn_and_omega( body, body_tn, non_body_tn, 
// 			    CG_LOOP_BACKPATCH_omega( bp ), 0 );
//       if ( _trace_CG_RTran ) {
// 	#pragma mips_frequency_hint NEVER
// 	fprintf( TFile, "<cio> invariant prolog backpatch removed: " );
// 	CG_LOOP_BackpatchTrace( CG_LOOP_prolog, bp );
//       }
//       CG_LOOP_Backpatch_Delete( CG_LOOP_prolog, bp );
//     }

//     bp = next_bp;
//   }

//   bp = CG_LOOP_Backpatch_First( CG_LOOP_epilog, NULL );
//   while ( bp ) {
//     CG_LOOP_BACKPATCH *next_bp = CG_LOOP_Backpatch_Next( bp );
//     TN *body_tn = CG_LOOP_BACKPATCH_body_tn( bp );
//     if ( ! CG_DEF_OP( body_tn, body, in_same_bb ) ) {
//       if ( _trace_CG_RTran ) {
// 	#pragma mips_frequency_hint NEVER
// 	fprintf( TFile, "<cio> invariant epilog backpatch removed: ");
// 	CG_LOOP_Backpatch_Trace( CG_LOOP_epilog, bp );
//       }
//       CG_LOOP_Backpatch_Delete( CG_LOOP_epilog, bp );
//     }
//     bp = next_bp;
//   }
// }


// ======================================================================
//
// CIO_RWTRAN class declaration
//
// CIO_RWTRAN contains the implimentation of read/cicse/write elimination
// analysis and transformation.
//
// ======================================================================


typedef std::pair< OP *, UINT8 > OP_OMEGA;

struct OP_OMEGA_less_prolog  // omega increasing
{
  bool operator()( const OP_OMEGA& s1, const OP_OMEGA& s2 ) const {
    if ( s1.second < s2.second ) return true;
    if ( s1.second > s2.second ) return false;
    return OP_Precedes( s1.first, s2.first );
  }
};

struct OP_OMEGA_less_epilog  // omega decreasing
{
  bool operator()( const OP_OMEGA& s1, const OP_OMEGA& s2 ) const {
    if ( s1.second > s2.second ) return true;
    if ( s1.second < s2.second ) return false;
    return OP_Precedes( s1.first, s2.first );
  }
};


struct CICSE_entry;   // defined below
struct CICSE_change;  // defined below


class CIO_RWTRAN {
private:

  // The local memory pool is set up and initialized by the caller to
  // the CIO_RWTRAN constructor

  MEM_POOL *_loc_mem_pool;

  // Trace flags are initialized by CIO_RWTRAN constructor

  const BOOL _trace_CG_DupTN;
  const BOOL _trace_CG_RTran;
  const BOOL _trace_CG_Chkpnt;

  // _trip_count_tn contains either NULL, or a constant loop trip count,
  // or a register holding the trip count computed in the loop prolog at
  // the time RW elimination was invoked.

  TN *_trip_count_tn;

  // --------------------------------------------------------------------
  // CIO Copy Propagation
  // --------------------------------------------------------------------

  BOOL CIO_Copy_Remove( BB *body );

  // --------------------------------------------------------------------
  // Prolog and Epilog Insertions
  // --------------------------------------------------------------------

  // Before a read/CS/write can be eliminated from the loop body, it is
  // usually necessary to insert some reads/CSs/writes into the loop
  // prolog/prolog/epilog to perform the first/first/last few
  // reads/CSs/writes.

  // _op_prolog_map and _op_epilog_map are ordered maps:
  //     pair<OP *, omega> ---> OP *
  // The domain OP* is in OP within the loop body, and the omega counts
  // the iterations of the loop body from the start (1,2,3,...) or the
  // end (...,2,1,0).  The range OP* is the corresponding OPs that will
  // be inserted into the prolog or epilog.

  typedef std::pair< const OP_OMEGA, OP * >           op_copy_pair;
#if defined(BUILD_OS_DARWIN)
  /* temporary workaround for bugs in 4.0 g++ STL headers */
  typedef std::map< OP_OMEGA, OP *, OP_OMEGA_less_prolog
	       >    op_copy_map_prolog;
  typedef std::map< OP_OMEGA, OP *, OP_OMEGA_less_epilog
	       >    op_copy_map_epilog;
#else
  typedef std::map< OP_OMEGA, OP *, OP_OMEGA_less_prolog,
	       mempool_allocator< op_copy_pair > >    op_copy_map_prolog;
  typedef std::map< OP_OMEGA, OP *, OP_OMEGA_less_epilog,
	       mempool_allocator< op_copy_pair > >    op_copy_map_epilog;
#endif /* defined(BUILD_OS_DARWIN) */

  op_copy_map_prolog                                  _op_prolog_map;
  op_copy_map_epilog                                  _op_epilog_map;

  // Mark_Op_For_Prolog( op, omega ) inserts a new OP into _op_prolog_map
  // duplicating the action of op during the omega'th execution of the
  // loop body.  Operand and result TNs are renamed and recursively
  // duplicated as necessary.  Mark_Op_For_Prolog returns a pointer to
  // the new op copy, or NULL if the copy is unsuccessful.
  //
  // Backpatch_Op_In_Prolog( op, new_tns, start_omega, end_omega )
  // inserts new prolog backpatches to initialize values for the TNs in
  // new_tns.  Copies of op retrieving these initial values to these
  // passes are inserted into _op_prolog_map by invoking
  // Mark_Op_For_Prolog.  Backpatch_Op_In_Prolog returns TRUE unless
  // Mark_Op_For_Prolog returns NULL.
  //
  // Copy_Ops_To_Prolog transfers the OP copies from _op_prolog_map into
  // the loop prolog.  All invocations to Backpatch_Op_In_Prolog should
  // be completed before invoking Copy_Ops_To_Prolog.

  OP *  Mark_Op_For_Prolog( OP *op, const UINT8 omega );
  BOOL  Backpatch_Op_In_Prolog( OP *op, TN *new_tns[],
				const UINT8 start_omega,
				const UINT8 end_omega );
  void  Copy_Ops_To_Prolog();

  // Write elimination sometimes requires the insertion of predicated
  // stores into the loop epilog.  On architectures that don't support
  // predicated stores, this is accomplished through "black hole"
  // locations in memory where values can be stored and then ignored.
  //
  // cgbh_node_int and cgbh_node_float remember the black hole addresses.
  //
  // Generate_Black_Holes initializes the black hole memory locations.
  //
  // Safe_Offset verifies that a computed offset will not overflow the
  // offset field of a memory operand.

  void Generate_Black_Holes();
  BOOL Safe_Offset( INT64 offset, OP *op );

  // Predicate_Write converts an unconditional memory store OP into a
  // store conditional on a predicate tn, either by predicating the OP
  // or by using a black hole memory address.

  void Predicate_Write( OPS *ops, OP *op, TN *tn_predicate );

  // Mark_Op_For_Epilog( op, omega ) inserts a new OP into _op_epilog_map
  // duplicating the action of op during the last omega'th execution of
  // the loop body.  Epilog backpatches are inserted as necessary to
  // retrieve the operand TNs (and the result TNs if op is predicated).
  // Mark_Op_For_Epilog returns a pointer to the new op copy.
  //
  // Backpatch_Op_In_Epilog invokes Mark_Op_For_Epilog to insert into the
  // loop epilog copies of op corresponding to the last start_omega'th
  // passes through the loop body.
  //
  // Copy_Ops_To_Epilog transfers the OP copies from _op_epilog_map into
  // the loop epilog.  Depending on the value of _trip_count_tn, these
  // OPs may be predicated to ensure correctness.  All invocations to
  // Mark_Op_For_Epilog should be completed before invoking
  // Copy_Ops_To_Prolog.

  OP *  Mark_Op_For_Epilog( OP *op, const UINT8 omega );
  BOOL  Backpatch_Op_In_Epilog( OP *op, const UINT8 start_omega );
  void  Copy_Ops_To_Epilog();

  // --------------------------------------------------------------------
  // Read/CICSE/Write elimination ANALYSIS fields and methods
  // --------------------------------------------------------------------

  BOOL  Read_CICSE_Candidate_Op( OP *op );
  BOOL  Write_Candidate_Op( OP *op );
  BOOL  Read_Candidate_Arc( ARC *arc );
  BOOL  Write_Candidate_Arc( ARC *arc );

  // --------------------------------------------------------------------
  // Read/CICSE elimination TRANSFORMATION fields and methods
  // --------------------------------------------------------------------

  void  Trace_CICSE_Entries( CICSE_entry *table, INT count,
			     const char *message );
  void  Trace_CICSE_Changes( CICSE_change *table, INT count,
			     const char *message );
  INT   CICSE_Lookup_Op( CICSE_entry *cicse_table, INT op_count, OP *op );
  void  Replace_Tn( BB *body, TN *tn_old, TN *tn_new, UINT8 omega_change );
  OP *  Append_TN_Copy( TN *tn_dest, TN *tn_from, OP *point, UINT8 omega );
  BOOL  CICSE_Transform( BB *body );

  // --------------------------------------------------------------------
  // Write elimination fields and methods
  // --------------------------------------------------------------------

  // The arrays _op_ordering and _arc_ordering list OPs and ARCs that are
  // candidates for write elimination.  _ordering_count is the number of
  // initialized elements of _op_ordering and _arc_ordering (although
  // _op_ordering may have additional unused memory allocated to it).
  // These OPs and ARCs must always satisfy the restrictions:
  // (1)  op != NULL and Write_Candidate_Op(  op  ) returns TRUE;
  //      Either arc == NULL or Write_Candidate_Arc( arc ) returns TRUE.
  // (2)  If _arc_ordering[t] != NULL, then
  //        ARC_pred( _arc_ordering[t] ) == _op_ordering[t],    and
  //        ARC_succ( _arc_ordering[t] ) == _op_ordering[t + 1].
  // _ordering_count, _op_ordering and _arc_ordering are allocated and
  // initialized by Sort_Ops and Sort_Arcs.

  INT   _ordering_count;
  OP  **_op_ordering;
  ARC **_arc_ordering;

  // Print_Ops and Print_Arcs dump the contents of _op_ordering and
  // _arc_ordering, respectively, into the trace file.

  void  Print_Ops(  const char *Msg );
  void  Print_Arcs( const char *Msg );

  // Sort_Ops and Sort_Arcs, when invoked in that order, identify all
  // MEMOUT arcs and store OPs that are candidates for write elimination.

  void  Sort_Ops_Preorder( OP *currentOP, BOOL current_has_arc );
  void  Sort_Ops( BB *body );
  void  Sort_Arcs( void );

  // Transform_Arcs applied the write elimination transformation to each of
  // the ARCs in _arc_ordering.

  void  Transform_Arcs( BB *body );

  BOOL  Write_Removal( BB *body );

  // These constructors and operators should never be called.
  // They are here for error detection purposes.

  CIO_RWTRAN( void );
  CIO_RWTRAN( const CIO_RWTRAN & );
  CIO_RWTRAN & operator=( const CIO_RWTRAN & );

public:

  // CIO_RWTRAN constructor and destructor

  CIO_RWTRAN(  LOOP_DESCR *loop, MEM_POOL *mem_pool )
    : _loc_mem_pool(    mem_pool ),
      _trace_CG_DupTN(  Get_Trace( TP_CGLOOP, 0x100,
				   LOOP_DESCR_loophead( loop ) ) ),
      _trace_CG_RTran(  Get_Trace( TP_CGLOOP, 0x200,
				   LOOP_DESCR_loophead( loop ) ) ),
      _trace_CG_Chkpnt( Get_Trace( TP_CGLOOP, 0x400,
				   LOOP_DESCR_loophead( loop ) ) ),
      _trip_count_tn(   CG_LOOP_Trip_Count( loop ) ),
      _ordering_count(  0 ),
      _op_ordering(     NULL ),
      _arc_ordering(    NULL ) {}

  ~CIO_RWTRAN( void ) {}

  // Read_Write_Removal is the driver for read/cicse/write elimination
  // analysis and transformations.  Returns TRUE iff changes are made to
  // *loop.

  BOOL  Read_CICSE_Write_Removal( LOOP_DESCR *loop );
};


// ======================================================================
//
// CIO_Copy_Remove performs cross iteration copy propagation.
// Currently, the algorithm is not effective on predicated move OPs.
// Also, we currently only try to propagate move OPs with nonzero operand
// omega to avoid a number of bugs.
//
// CIO_copy is one element in a list maintained within CIO_Copy_Remove to
// map TNs to OPs.  For each result TN of an eligible copy OP, the list
// remembers the most recent OP that stores to that TN.
//
// ======================================================================


struct CIO_copy {
  TN *tn_result;
  OP *op;
};


BOOL
CIO_RWTRAN::CIO_Copy_Remove( BB *body )
{
  // Count and set flag1 for all OPs that we think we can remove.  If we
  // later change our mind, we will reset flag1.  Currently, we only try
  // to remove unpredicated copy OPs.  Also, we currently only try to
  // propagate move OPs with nonzero operand omega to avoid a number of
  // bugs.
  INT eligible_OP_count = 0;
  OP *op;
  FOR_ALL_BB_OPs_FWD( body, op ) {
    // Note: Could use CGTARG_Is_Copy and CGTARG_Copy_Operand instead
    // of OP_copy and OP_COPY_OPND ?
    if ( OP_copy( op ) &&
	 ( ! OP_has_predicate( op ) ||
	   TN_is_true_pred( OP_opnd( op, OP_PREDICATE_OPND ) ) ) &&
	 OP_omega( op, OP_COPY_OPND ) > 0 ) {
      Set_OP_flag1( op );
      eligible_OP_count++;

      // Some of the code below assumes that copy OPs have exactly one
      // result TN.
      Is_True( OP_results( op ) == 1,
	       ( "CIO_RWTRAN::CIO_Copy_Remove copy OP has %d != 1"
		 " result TNs", OP_results( op ) ) );
    } else {
      Reset_OP_flag1( op );
    }
  }

  // Abort if there are no copy removal opportunities
  if ( eligible_OP_count == 0 ) return FALSE;

  // If none of the OPs in the loop body are predicated, then our
  // algorithm can skip one step.
  BOOL predicated_OPs = FALSE;

  // For each result TN of an eligible copy OP, cio_copy_table
  // remembers the most recent OP that stores to that TN.
  // cio_copy_table_size is the number of entries in cio_copy_table.
  // Since each copy has one result TN, we know cio_copy_table_size
  // is at most eligible_OP_count.
  CIO_copy *cio_copy_table = (CIO_copy *) CXX_NEW_ARRAY( CIO_copy,
							 eligible_OP_count,
							 _loc_mem_pool );
  INT cio_copy_table_size = 0;

  // Initialize predicated_OPs and cio_copy_table: find the last
  // unpredicated OP into each result TN.
  FOR_ALL_BB_OPs_FWD( body, op ) {
    if ( OP_has_predicate( op ) &&
	 ! TN_is_true_pred( OP_opnd( op, OP_PREDICATE_OPND ) ) )
      predicated_OPs = TRUE;
    else {
      for ( INT res = OP_results( op ) - 1; res >= 0; --res ) {
	TN *tn = OP_result( op, res );
	INT index;
	for ( index = cio_copy_table_size - 1; index >= 0; --index )
	  if ( cio_copy_table[index].tn_result == tn ){
#ifdef KEY
	    /* cond_def OPs prevent the removal of earlier copy OPs
	       with the same result TNs.
	     */
	    if( OP_cond_def( op ) ){
	      Reset_OP_flag1( cio_copy_table[index].op );	      
	    }
#endif
	    break;
	  }
	if ( index < 0 && OP_flag1( op ) ) {
	  index = cio_copy_table_size;
	  cio_copy_table[index].tn_result = tn;
	  cio_copy_table_size++;
	}
	if ( index >= 0 )
	  cio_copy_table[index].op = op;
      }
    }
  }
  Is_True( cio_copy_table_size <= cio_copy_table_size,
	   ( "CIO_RWTRAN::CIO_Copy_Remove cio_copy_table_size(%d)"
	     " > eligible_OP_count(%d)",
	     cio_copy_table_size, eligible_OP_count ) );

  // Identify any copy OPs that cannot be propagated.  In particular,
  // predicated OPs may prevent the removal of earlier copy OPs with the
  // same result TNs.
  if ( predicated_OPs ) {
    FOR_ALL_BB_OPs_FWD( body, op ) {
      BOOL predicated
	= ( OP_has_predicate( op ) &&
	    ! TN_is_true_pred( OP_opnd( op, OP_PREDICATE_OPND ) ) );
      for ( INT res = OP_results( op ) - 1; res >= 0; --res ) {
	TN *tn = OP_result( op, res );
	for ( INT index = cio_copy_table_size - 1; index >= 0; --index )
	  if ( cio_copy_table[index].tn_result == tn ) {
	    if ( predicated ) {
	      // Cannot propagate this source OP
	      Reset_OP_flag1( cio_copy_table[index].op );
	    } else {
	      // Update cio_copy_table
	      cio_copy_table[index].op = op;
	    }
	    break;
	  }
      }
    }
  }

  // Also, if the operand TN and result TN of the copy OP have
  // conflicting prolog backpatches, then we cannot perform the copy
  // propagation.  (Possible future improvement: if we can determine
  // that the prolog backpatches initialize the two TNs to the same
  // value, then they don't really conflict.)  Happily, epilog
  // backpatches are never an issue, and we only need to check the
  // last copy OP for a given result TN.
  INT index;
  for ( index = cio_copy_table_size - 1; index >= 0; --index ) {
    OP *op = cio_copy_table[index].op;
    if ( ! OP_flag1( op ) ) continue;

    TN *tn_old = OP_result( op, 0 );
    TN *tn_new = OP_opnd( op, OP_COPY_OPND );
    UINT8 omega_change = OP_omega( op, OP_COPY_OPND );

    // Check the prolog backpatches for conflicts.
    CG_LOOP_BACKPATCH *bp;
    for ( bp = CG_LOOP_Backpatch_First( CG_LOOP_prolog, tn_old );
	  bp != NULL; bp = CG_LOOP_Backpatch_Next( bp ) ) {
      UINT8 omega = CG_LOOP_BACKPATCH_omega( bp ) + omega_change;
      if ( CG_LOOP_Backpatch_Find_Non_Body_TN( CG_LOOP_prolog,
					       tn_new, omega ) ) {
	// Detected prolog backpatch conflict
	Reset_OP_flag1( op );
	DevWarn( "CIO_RWTRAN::CIO_Copy_Remove prolog backpatch conflict"
		 " prevent copy removal of TN%d <-- TN%d[%d], when omega"
		 " == %d", TN_number( tn_old ), TN_number( tn_new ),
		 omega_change, omega );
	break;
      }
    }
  }

  // Copy propagation is performed in three steps.  First, we update all
  // operand TNs with omega == 0.  Second, we update all operand TNs with
  // omega > 0 (which must refer to the last definition of that TN in the
  // loop body.  Third, we remove the copy OPs and update the prolog and
  // epilog backpatch lists.

  FOR_ALL_BB_OPs_FWD( body, op ) {

    // Perform the propagation by substituting operand TNs if omega == 0
    for ( INT opnd = OP_opnds( op ) - 1; opnd >= 0; --opnd ) {
      TN *tn = OP_opnd( op, opnd );
      if ( TN_is_register( tn ) && OP_omega( op, opnd ) == 0 )
	for ( INT index = cio_copy_table_size - 1; index >= 0; --index )
	  if ( cio_copy_table[index].tn_result == tn ) {
	    OP *op_src = cio_copy_table[index].op;
	    if ( OP_flag1( op_src ) ) {
#ifdef KEY
	      // Don't propagate copy if the copy source is a dedicated
	      // register, since the omega code can't track dedicated registers
	      // correctly.  See explanation in bug 4426.
	      // Don't propagate copy if op has same_res properties.
	      TN *copy_src_opnd = OP_opnd( op_src, OP_COPY_OPND );
	      if (TN_is_dedicated(copy_src_opnd) 
	      || (OP_same_res(op) && OP_Defs_TN(op, tn))) {
		Reset_OP_flag1( op_src );
		break;
	      }
#endif
	      Set_OP_opnd( op, opnd, OP_opnd( op_src, OP_COPY_OPND ) );
	      Set_OP_omega( op, opnd, OP_omega( op, opnd )
			    + OP_omega( op_src, OP_COPY_OPND ) );
	    }
	    break;
	  }
    }

    // Update cio_copy_table
    if (! OP_has_predicate( op ) ||
	TN_is_true_pred( OP_opnd( op, OP_PREDICATE_OPND ) ) )
      for ( INT res = OP_results( op ) - 1; res >= 0; --res ) {
	TN *tn = OP_result( op, res );
	for ( INT index = cio_copy_table_size - 1; index >= 0; --index )
	  if ( cio_copy_table[index].tn_result == tn ) {
	    cio_copy_table[index].op = op;
	    break;
	  }
      }
  }

  // Substitute all operands within the select OPs; repeat as necessary,
  // since the new operand itself could be replaced.  We know these only
  // refer to the last definition of TN in the loop body since omega > 0.
  for ( index = cio_copy_table_size - 1; index >= 0; --index ) {
    OP *op = cio_copy_table[index].op;
    if ( ! OP_flag1( op ) ) continue;

    // Perform the propagation by substituting operand TNs if omega is > 0
    if ( OP_omega( op, OP_COPY_OPND ) > 0 ) { // TN_is_register( tn ) == TRUE 
      TN *tn = OP_opnd( op, OP_COPY_OPND );
      for ( INT index = cio_copy_table_size - 1; index >= 0; --index )
	if ( cio_copy_table[index].tn_result == tn ) {
	  OP *op_src = cio_copy_table[index].op;
	  if ( ! OP_flag1( op_src ) )
	    break;
#ifdef KEY
	  // Avoid infinite recursion.  Bugs 21, 8929.
	  if (OP_opnd(op_src, OP_COPY_OPND) == tn)
	    break;
#endif
	  tn = OP_opnd( op_src, OP_COPY_OPND );
	  Set_OP_opnd( op, OP_COPY_OPND, tn );
	  Set_OP_omega( op, OP_COPY_OPND, OP_omega( op, OP_COPY_OPND )
			+ OP_omega( op_src, OP_COPY_OPND ) );
	  // Restart loop, in case this operand is also being substituted
	  index = cio_copy_table_size;
	  Is_True( OP_omega( op, OP_COPY_OPND ) < MAX_OMEGA,
		   ( "CIO_RWTRAN::CIO_Copy_Remove overflowed MAX_OMEGA;"
		     " infinite loop?" ) );
	}
    }
  }

  // Substitute all remaining operands (with omega > 0).  We know these
  // only refer to the last definition of TN in the loop body.
  FOR_ALL_BB_OPs_FWD( body, op ) {

    // Perform the propagation by substituting operand TNs IF OMEGA IS > 0
    for ( INT opnd = OP_opnds( op ) - 1; opnd >= 0; --opnd )
      if ( OP_omega( op, opnd ) > 0 ) {  // so TN_is_register( tn ) == TRUE 
	TN *tn = OP_opnd( op, opnd );
	for ( INT index = cio_copy_table_size - 1; index >= 0; --index )
	  if ( cio_copy_table[index].tn_result == tn ) {
	    OP *op_src = cio_copy_table[index].op;
	    if ( OP_flag1( op_src )
#ifdef KEY
		 /* Bug125:
		    No need to propagate opnd again, which had happened at the
		    previous stage. The original algorithm does not expect to see
		    a copy operation that has the same opnd and result.
		  */
		 && ( OP_result(op_src,0) != OP_opnd(op_src,0) )
#endif
		 ) {
	      Set_OP_opnd( op, opnd, OP_opnd( op_src, OP_COPY_OPND ) );
	      Set_OP_omega( op, opnd, OP_omega( op, opnd )
			    + OP_omega( op_src, OP_COPY_OPND ) );
	    }
	    break;
	  }
      }
  }

  // Update the prolog and epilog backpatch lists.
  // We already filtered out any prolog backpatch conflicts.
  for ( index = cio_copy_table_size - 1; index >= 0; --index ) {
    OP *op = cio_copy_table[index].op;
    if ( ! OP_flag1( op ) ) continue;

    // Update any occurances in the prolog and epilog backpatch lists
    TN *tn_old = cio_copy_table[index].tn_result;
    TN *tn_new = OP_opnd( op, OP_COPY_OPND );

#ifdef KEY
    /* Don't update the prolog and epilog if source and result are
       the same TNs; otherwise the original omega value will be overwritten
       by CG_LOOP_Backpatch_Replace_Body_TN().  (bug#2484)

       Or should we use CG_LOOP_Backpatch_Add instead to resolve the
       conflicts ???
     */
    if( tn_old == tn_new ){
      continue;
    }
#endif // KEY

    UINT8 omega_change = OP_omega( op, OP_COPY_OPND );
    CG_LOOP_Backpatch_Replace_Body_TN( CG_LOOP_epilog,
				       tn_old, tn_new, omega_change );
    CG_LOOP_Backpatch_Replace_Body_TN( CG_LOOP_prolog,
				       tn_old, tn_new, omega_change );
  }

  // Finally, remove the copy OPs.
  BOOL change = FALSE;
  OP *next_op;
  for ( op = BB_first_op( body ); op != NULL; op = next_op ) {
    next_op = OP_next( op );
    if ( ! OP_flag1( op ) ) continue;

    // Remove the copy OP
    if ( _trace_CG_RTran ) {
      #pragma mips_frequency_hint NEVER
      fprintf( TFile, "CIO_RWTRAN::CIO_Copy_Remove removes:\n\t" );
      Print_OP_No_SrcLine( op );
    }
    BB_Remove_Op( body, op );
    change = TRUE;
  }

  return change;
}


// ======================================================================
//
// OP_lookup_opnd( op, tn ) returns i with OP_opnd( op, i ) == tn.
// If no such i, assert.
//
// OP_lookup_result( op, tn ) returns i with OP_result( op, i ) == tn.
// If no such i, assert.
//
// ======================================================================


static inline INT OP_lookup_opnd( const OP *op, const TN *tn )
{
  for ( INT opnd = OP_opnds( op ) - 1; opnd >= 0; --opnd )
    if ( OP_opnd( op, opnd ) == tn )
      return opnd;
  FmtAssert( FALSE, ( "OP_lookup_opnd failed to find operand" ) );
  /*NOTREACHED*/
}


static inline INT OP_lookup_result( const OP *op, const TN *tn )
{
  for ( INT res = OP_results( op ) - 1; res >= 0; --res )
    if ( OP_result( op, res ) == tn )
      return res;
  FmtAssert( FALSE, ( "OP_lookup_result failed to find result" ) );
  /*NOTREACHED*/
}


// ======================================================================
//
// Mark_Op_For_Prolog( op, omega ) creates a copy of op for the loop
// prolog, duplicating the execution of op during the omega'th pass
// through the loop body.  Definitions of operand TNs (and result TNs if
// op is predicated) are also duplicated recursively.  Also, TNs local to
// the loop body are replaced by newly generated TNs that are valid in
// the loop prolog.  All copied OPs are stored in the map _op_prolog_map.
// Mark_Op_For_Prolog returns a pointer to the new op copy, or NULL if
// the copy is unsuccessful.  (If the copy is unsuccessful, then some
// dead code could be inserted into _op_prolog_map, and later into the
// loop prolog.)
//
// Copy_Ops_To_Prolog() inserts all of the OP copies from
// _op_prolog_map into the loop prolog.  It should only be called after
// all invocations to Mark_Op_For_Prolog are complete, in order to
// guarantee the correct order of prolog code.
//
// ======================================================================


OP *
CIO_RWTRAN::Mark_Op_For_Prolog( OP *op, const UINT8 omega )
{
  // Step (1)  Check to see if the pair (op, omega) already has a copy
  op_copy_map_prolog::iterator
    op_find = _op_prolog_map.find( OP_OMEGA( op, omega ) );
  if ( op_find != _op_prolog_map.end() )
    return op_find->second;

  // Step (2)  Some OPs should not be copied to the prolog -- abort!
  if ( OP_has_implicit_interactions( op ) )
    return NULL;

#ifdef TARG_X8664
  // Arcs between rflags setters and rflags users are labeled as MISC.  When
  // copying an insn to the prologue, only REGIN preds are copied along with
  // the insn; MISC preds are not copied.  As a workaround, don't copy rflags
  // users into the prologue.  Bug 4991.
  if (OP_reads_rflags(op))
    return NULL;
#endif

  // Step (3)  Duplicate the OP, but don't insert it anywhere yet
  OP *op_prolog = Dup_OP( op );
#ifdef KEY
  CG_LOOP_Init_Op(op_prolog);
  Copy_WN_For_Memory_OP( op_prolog, op );
#endif
  if ( Is_DB_OP_Init( op ) )
    DB_Copy_Aux_OP( op_prolog, op );

  // Step (4)  Recursively copy definitions of all operands into prolog
  ARC_LIST *arcs = ARC_LIST_Find( OP_preds( op ), CG_DEP_REGIN, DONT_CARE );
  while ( arcs ) {
    ARC *arc = ARC_LIST_first( arcs );
    if ( omega > ARC_omega( arc ) ) {
      OP *op_new = Mark_Op_For_Prolog( ARC_pred( arc ),
				       omega - ARC_omega( arc ) );
      if ( op_new == NULL )
	return NULL;
      TN *tn_old = OP_opnd( op, ARC_opnd( arc ) );
      TN *tn_new = OP_result( op_new,
			      OP_lookup_result( ARC_pred( arc ), tn_old ) );
      Set_OP_opnd( op_prolog, ARC_opnd( arc ), tn_new );
    }
    arcs = ARC_LIST_Find( ARC_LIST_rest( arcs ), CG_DEP_REGIN, DONT_CARE );
  }

  if ( OP_cond_def( op ) ) {

    // Step (5a)  If op is predicated, then copy definitions of all results
    //            into prolog.  Also, generate new result TNs to avoid
    //            overwriting the previously live TN value.
    UINT8 res;
    TN **result = (TN **) alloca( OP_results( op ) * sizeof( TN * ) );
    for ( res = 0; res < OP_results( op ); ++res )
      result[res] = NULL;

    ARC_LIST *arcs = ARC_LIST_Find( OP_preds( op ), CG_DEP_REGOUT, DONT_CARE );
    while ( arcs ) {
      ARC *arc = ARC_LIST_first( arcs );
      if ( omega > ARC_omega( arc ) ) {
	OP *op_old = ARC_pred( arc );
	OP *op_new = Mark_Op_For_Prolog( ARC_pred( arc ),
					 omega - ARC_omega( arc ) );
	if ( op_new == NULL )
	  return NULL;

	// WARNING:  
	// If count > 1, then the following algorithm might fail.  This
	// could only occur for predicated operand with multiple results.
	// For example, the following code could fail:
	//       x,y = .....
	//       x   = .....
	//   (p) x,y = .....
	//
	// One good fix would be to introduce ARC_result, analogous to
	// ARC_opnd, to the dependence graph in cg_dep_graph.h/cxx.
	// Then code below should be replaced by the following, simpler
	// code:
	//
	// UINT8 which = ARC_result( arc );
	// TN *tn_new = OP_result( op_new,
	//		           OP_lookup_result( op_old, which ) );
	// Set_OP_result( op_prolog, which, tn_new );
	//
	// Alternatively, the the renaming of result TNs could be performed
	// during the execution of Copy_Ops_To_Prolog instead of
	// Mark_Op_For_Prolog.  See further below for details.

	UINT8 count = 0;
	for ( UINT8 i = 0; i < OP_results( op ); ++i )
	  for ( UINT8 j = 0; j < OP_results( op_old ); ++j )
	    if ( OP_result( op, i ) == OP_result( op_old, j ) ) {
	      ++count;
	      TN *tn_new = OP_result( op_new, j );
	      Is_True(j < OP_results(op), ("result index too large"));
	      result[j] = tn_new;
	    }
	if ( count != 1 ) {
	  Is_True( FALSE, ( "CIO_RWTRAN::Mark_Op_For_Prolog found"
			    " count %u != 1", count ) );
	  return NULL;  // Abort in no debug compiler
	}
      }
      arcs = ARC_LIST_Find( ARC_LIST_rest( arcs ), CG_DEP_REGOUT, DONT_CARE );
    }

    for ( res = 0; res < OP_results( op ); ++res ) {
      TN *tn_new = result[res];
      if ( tn_new == NULL ) {

	// Insert copy of old TN value into new TN, so live value is not lost
	TN *tn_old = OP_result( op, res );
	tn_new = CGPREP_Dup_TN( tn_old );
	CGPREP_Copy_TN_Into_BB( tn_new, tn_old, CG_LOOP_prolog,
				NULL, 0, FALSE );
      }
      Set_OP_result( op_prolog, res, tn_new );
    }

  } else {  // ! OP_cond_def( op )

    // Step (5b)  If op is NOT predicated, then rename all result TNs to avoid
    //            hiding live TN values
    for ( UINT8 i = 0; i < OP_results( op ); ++i ) {
      Set_OP_result( op_prolog, i, CGPREP_Dup_TN( OP_result( op, i ) ) );
    }
  }

  // Step (6)  Record (op, omega) --> op_prolog and return the new instruction
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile,
	     "<cio> Mark_Op_For_Prolog(op 0x%p, omega %u) ---> op 0x%p\n\t",
	     op, omega, op_prolog );
    Print_OP_No_SrcLine( op );
    fprintf( TFile, "\t" );
    Print_OP_No_SrcLine( op_prolog );
  }
  _op_prolog_map[ OP_OMEGA( op, omega ) ] = op_prolog;
  return op_prolog;
}


void
CIO_RWTRAN::Copy_Ops_To_Prolog()
{
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "\n<cio> Copy_Ops_To_Prolog copying:\n" );
  }

  // Iterate through all of the OP duplicates, ordered first by omega
  // and then by the order of the original OPs within the loop body.
  for ( op_copy_map_prolog::const_iterator op_iter = _op_prolog_map.begin();
	op_iter != _op_prolog_map.end();
	++op_iter ) {

    // Append the OP to the loop prolog
    BB_Insert_Op( CG_LOOP_prolog, NULL, op_iter->second, FALSE );
    if ( _trace_CG_RTran ) {
      #pragma mips_frequency_hint NEVER
      fprintf( TFile, "(op 0x%p, omega %u) ---> op 0x%p\n\t",
	       op_iter->first.first, op_iter->first.second, op_iter->second );
      Print_OP_No_SrcLine( op_iter->second );
    }
  }

  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "\n" );
  }
  _op_prolog_map.clear();
}


// ======================================================================
//
// The code in this section performs the same function as the code in
// the previous section, except that TNs are renamed during the
// execution of Copy_Ops_To_Prolog instead of Mark_Op_For_Prolog.
// This works around a potential bug in the previous section, but is
// less elegant, less efficient, and requires an extra map data
// structure ( TN, omega ) ---> TN.
//
// This code has not been tested or debugged.
//
//
// Mark_Op_For_Prolog( op, omega ) creates a copy of op for the
// loop prolog, duplicating the execution of op during pass omega
// through the loop body.  Definitions of operand TNs (and result TNs
// if op is predicated) are also duplicated recursively.
// Mark_Op_For_Prolog returns a pointer to the new op copy, or NULL if
// the copy is unsuccessful.
//
// Copy_Ops_To_Prolog() inserts all of the OP copies from
// _op_prolog_map into the loop prolog.  It should only be called after
// all invocations to Mark_Op_For_Prolog are complete, in order to
// guarantee the correct order of prolog code.  Also, TNs local to the
// loop body are replaced by newly generated TNs that are valid in the
// loop prolog.  All copied OPs are stored in the map _op_prolog_map.
//
// ======================================================================




// ======================================================================
//
// Backpatch_Op_In_Prolog( op_read, new_tns, start_omega, end_omega )
// inserts new prolog backpatches to initialize values for the TNs in
// new_tns.  These inital values are obtained from op_read during the
// first few passes through the loop body:
//
// tn_new[end_omega]     <--  op(pass = 1)
// tn_new[end_omega - 1] <--  op(pass = 2)
// tn_new[...]           <--  op(pass = ...)
// tn_new[start_omega]   <--  op(pass = end_omega - start_omega + 1)
//
// Copies of op corresponding to these passes are inserted into
// _op_prolog_map by invoking Mark_Op_For_Prolog.
//
// Backpatch_Op_In_Prolog returns TRUE unless Mark_Op_For_Prolog
// returns NULL.
//
// ======================================================================


BOOL
CIO_RWTRAN::Backpatch_Op_In_Prolog( OP *op, TN *new_tns[],
				    const UINT8 start_omega,
				    const UINT8 end_omega )
{
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "<cio> Backpatch_Op_In_Prolog(%u...%u) with results in",
	     start_omega, end_omega );
    for ( INT res = 0; res < OP_results( op ); ++res ) {
      fprintf( TFile, " " );
      Print_TN( new_tns[res], FALSE );
    }
    fprintf( TFile, "\n\t" );
    Print_OP_No_SrcLine( op );
  }

  for ( UINT8 omega = start_omega; omega <= end_omega; ++omega ) {

    OP *opr = Mark_Op_For_Prolog( op, end_omega - omega + 1 );
    if ( opr == NULL )
      return FALSE;
    for ( INT res = 0; res < OP_results( opr ); ++res ) {
      TN *tn = OP_result( opr, res );
      TN *tn_new = new_tns[res];
      CG_LOOP_Backpatch_Add( CG_LOOP_prolog, tn, tn_new, omega );
      if ( _trace_CG_RTran ) {
	#pragma mips_frequency_hint NEVER
	fprintf( TFile, "<cio> Backpatch_Op_In_Prolog backpatch:  " );
	Print_TN( tn_new, FALSE );
	fprintf( TFile, "[%d] <-- ", omega );
	Print_TN( tn, FALSE );
	fprintf( TFile, "\n" );
      }
    }
  }
  return TRUE;
}


// ====================================================================
//
// Generate_Black_Holes, Safe_Offset
//
// Write elimination sometimes requires the insertion of predicated
// stores into the loop epilog.  On architectures that don't support
// predicated stores, this is accomplished through "black hole"
// locations in memory where values can be stored and then ignored.
// A select OP is inserted to switch between the original write
// address and the black hole address, according to the value of the
// predicate TN.
//
// Generate_Black_Holes initializes the black hole memory locations.
//
// Safe_Offset examines the value of a literal offset operand of a memory
// OP which is being subtracted from the positive offset of a stack
// location.  Return TRUE if the result will not overflow the literal
// offset field of a memory operand.
//
// ====================================================================


static ST *cgbh_node_int   = NULL;
static ST *cgbh_node_float = NULL;
static ST *latest_pu       = NULL;


void
CIO_RWTRAN::Generate_Black_Holes()
{
  // Only need to reinitialize for each new PU.
  if ( latest_pu == Get_Current_PU_ST() )
    return;

  latest_pu = Get_Current_PU_ST();

  cgbh_node_int   = Gen_Temp_Symbol( Spill_Int_Type,   "cg_blackhole_int" );
  cgbh_node_float = Gen_Temp_Symbol( Spill_Float_Type, "cg_blackhole_float" );
  Allocate_Temp_To_Memory( cgbh_node_int );
  Allocate_Temp_To_Memory( cgbh_node_float );
}


#define MAX_BLACK_HOLE_OFFSET 0x800


BOOL
CIO_RWTRAN::Safe_Offset( INT64 offset, OP *op )
{
  return offset > - (0x8000 - MAX_BLACK_HOLE_OFFSET);
}


// ====================================================================
//
// Predicate_Write
//
// Converts an uncondition memory store OP into a store conditional on
// a predicate tn.  Depending on the architecture, this is accomplished
// either by predicating the OP, or by inserting into ops a sequence of
// OPs that manipulate the address of the memory store OP.
//
// ====================================================================


void
CIO_RWTRAN::Predicate_Write( OPS *ops, OP *op, TN *tn_predicate )
{
  Is_True( ops != NULL, ( "OPS are NULL" ) );
  Is_True( OP_memory( op ), ( "OP_memory(op) == FALSE" ) );
  Is_True( OP_store( op ), ( "OP_store(op) == FALSE" ) );
  Is_True( tn_predicate != NULL, ( "tn_predicate == NULL" ) );

  // Use predication, if available
  if ( OP_has_predicate( op ) ) {
    TN *tn_pred = tn_predicate;
    if ( ! TN_is_true_pred( OP_opnd( op, OP_PREDICATE_OPND ) ) ) {
      ; // CHANGE tn_pred !!!
    }
    Set_OP_opnd( op, OP_PREDICATE_OPND, tn_pred );
    return;
  }

  Generate_Black_Holes();

  // If the OP is an indexed memory OP, the offset is a symbol, or if
  // the literal TN to be computed below does not fit in the literal
  // operand of an add immediate OP, we combine the base and offset
  // into a new base, and (if necesssary) change the OP code on the
  // memory OP.

  UINT8 opnd_base   = OP_find_opnd_use( op, OU_base   );
  UINT8 opnd_offset = OP_find_opnd_use( op, OU_offset );
  TN *tn_offset = OP_opnd( op, opnd_offset );
  if ( ! TN_is_constant( tn_offset ) || ! TN_has_value( tn_offset ) ||
       Safe_Offset( TN_value( tn_offset ), op ) == FALSE ) {

    OPS ops_addr = OPS_EMPTY;
    TN *tn_zero = Gen_Literal_TN( 0, 4 );
    TN *tn_new_base = Build_TN_Like( OP_opnd( op, opnd_base ) );

    Exp_OP2( Pointer_Size == 4 ? OPC_I4ADD : OPC_I8ADD,
	     tn_new_base, OP_opnd( op, opnd_base ), tn_offset, &ops_addr );

    OPS_Insert_Ops_Before( ops, op, &ops_addr );

    // Update op

    TOP new_opcode = CGTARG_Equiv_Nonindex_Memory_Op( op );
    if ( new_opcode != TOP_UNDEFINED ) {
      OP_Change_Opcode( op, new_opcode );
    }
    Set_OP_opnd( op, opnd_base,   tn_new_base );
    Set_OP_opnd( op, opnd_offset, tn_zero );
    tn_offset = tn_zero;
  }

  // Generate OPS to compute the block hole address base.  Note that
  // the address base is offset by the negative of the offset of op.

  TN *tn_storeval = OP_opnd( op, OP_find_opnd_use( op, OU_storeval ) );
  ST *st_addr = TN_is_float( tn_storeval ) ? cgbh_node_float : cgbh_node_int;
  TN *tn_hole_base = Build_TN_Like( OP_opnd( op, opnd_base ) );
  OPS ops_hole = OPS_EMPTY;
  Exp_Lda( Pointer_type, tn_hole_base, st_addr,
	   - TN_value( tn_offset ), OPERATOR_UNKNOWN, &ops_hole );
  OPS_Insert_Ops_Before( ops, op, &ops_hole );

  // Generate a select OP, which uses the predicate TN to select either
  // the original address base or the blackhole address base

  TN *tn_base = OP_opnd( op, opnd_base );
  TN *tn_safe_base  = Build_TN_Like( tn_base );
#if defined KEY && defined TARG_MIPS
  Build_OP(TOP_or, tn_safe_base, tn_base, Zero_TN, ops);
  if (TN_register_class(tn_predicate) == ISA_REGISTER_CLASS_fcc)
    Build_OP(TOP_movt, tn_safe_base, tn_hole_base, tn_predicate, ops);
  else
    Build_OP(TOP_movn, tn_safe_base, tn_hole_base, tn_predicate, ops);
  Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);      
  Set_OP_opnd( op, opnd_base, tn_safe_base );
#else
  TOP code = CGTARG_Which_OP_Select( TN_size( tn_base ) * 8, FALSE, FALSE );
  OP *op_select = Mk_OP( code, tn_safe_base, Zero_TN, Zero_TN, Zero_TN );
  Set_OP_selcondopnd( op_select, tn_predicate );
  // I MAY HAVE THESE NEXT TWO BACKWARDS
  Set_OP_selndfopnd( op_select, tn_base );
  Set_OP_seldefopnd( op_select, tn_hole_base );
  Set_OP_result( op_select, 0 /*???*/, tn_safe_base );

  // Update ops and update the op with the correct base

  Set_OP_opnd( op, opnd_base, tn_safe_base );
  OPS_Insert_Op_Before( ops, op, op_select );
#endif
}


// ======================================================================
//
// Mark_Op_For_Epilog( op, omega ) inserts a new OP into _op_epilog_map
// duplicating the action of op during the last omega'th execution of the
// loop body.  Epilog backpatches are inserted as necessary to retrieve
// the operand TNs (and the result TNs if op is predicated).
// Mark_Op_For_Epilog returns a pointer to the new op copy.
//
// Copy_Ops_To_Epilog transfers the OP copies from _op_epilog_map into
// the loop epilog.  Depending on the value of _trip_count_tn, these
// OPs may be predicated to ensure correctness.  All invocations to
// Mark_Op_For_Epilog should be completed before invoking
// Copy_Ops_To_Prolog.
//
// ======================================================================


OP *
CIO_RWTRAN::Mark_Op_For_Epilog( OP *op, const UINT8 omega )
{
  Is_True( OP_store( op ),
	   ( "CIO_RWTRAN::Mark_Op_For_Epilog op is not store" ) );

  // If trip count is constant and less than omega, don't copy op to epilog
  if ( TN_has_value( _trip_count_tn ) && TN_value( _trip_count_tn ) < omega )
    return NULL;

  // Step (1)  Check to see if the pair (op, omega) already has a copy
  //           Probably will never happen
  op_copy_map_epilog::iterator
    op_find = _op_epilog_map.find( OP_OMEGA( op, omega ) );
  if ( op_find != _op_epilog_map.end() )
    return op_find->second;

  // Step (2)  Duplicate the OP, but don't insert it anywhere yet
  OP *op_epilog = Dup_OP( op );
  if ( Is_DB_OP_Init( op ) )
    DB_Copy_Aux_OP( op_epilog, op );

  // Step (3)  Determine state of each operand:
  //   0 = loop invariant
  //   1 = tn has same value in op as at end of loop body
  //   2 = tn has same value in op as at start but not end of loop body
  //   3 = none of the above

  //  Initialize states to 0
  UINT8 i;
  UINT8 *opnd_state = (UINT8 *) alloca( OP_opnds( op ) * sizeof( UINT8 ) );
  for ( i = 0; i < OP_opnds( op ); ++i )
    opnd_state[i] = 0;

  // Distinguish between states 0, 1 and 2 (classify 3 as 2)
  ARC_LIST *arcs = ARC_LIST_Find( OP_succs( op ), CG_DEP_REGANTI, DONT_CARE );
  while ( arcs ) {
    ARC *arc = ARC_LIST_first( arcs );
    arcs = ARC_LIST_Find( ARC_LIST_rest( arcs ), CG_DEP_REGANTI, DONT_CARE );

    UINT8 opnd = ARC_opnd( arc );
    if ( ARC_omega( arc ) == 0 )
      opnd_state[ opnd ] = 2;
    else if ( opnd_state[ opnd ] == 0 )
      opnd_state[ opnd ] = 1;
  }

  // Distinguish between states 2 and 3
  arcs = ARC_LIST_Find( OP_preds( op ), CG_DEP_REGIN, DONT_CARE );
  while ( arcs ) {
    ARC *arc = ARC_LIST_first( arcs );
    arcs = ARC_LIST_Find( ARC_LIST_rest( arcs ), CG_DEP_REGIN, DONT_CARE );
    UINT8 opnd = ARC_opnd( arc );
    if ( ARC_omega( arc ) == 0 ) {
      if ( opnd_state[ opnd ] == 2 )
	opnd_state[ opnd ] = 3;
      else
	opnd_state[ opnd ] = 1;
    } else if ( ARC_omega( arc ) == 1 ) {
      if ( opnd_state[ opnd ] < 3 )
	opnd_state[ opnd ] = 2;
    }
  }

  // Step (4)  Insert epilog backpatches to retrieve operands
  // NOTE: Be sure to test code below with state 3
  for ( UINT8 opnd = 0; opnd < OP_opnds( op ); ++opnd ) {
    UINT8 state = opnd_state[ opnd ];
    if ( state == 0 ) continue;

    TN *tn_old = OP_opnd( op, opnd );
    if ( state == 3 ) {
      // Insert copy of opnd into new TN
      TN *tn_new = CGPREP_Dup_TN( tn_old );
      CGPREP_Copy_TN( tn_new, tn_old, op, 0, FALSE );
      tn_old = tn_new;
    }

    // Insert epilog backpatch unless one already exists
    UINT8 omega_bp = ( state == 2 ? omega : omega - 1 );
    TN *tn_new = CG_LOOP_Backpatch_Find_Non_Body_TN( CG_LOOP_epilog,
						     tn_old, omega_bp );
    if ( tn_new == NULL ) {
      tn_new = CGPREP_Dup_TN( tn_old );
      CG_LOOP_Backpatch_Add( CG_LOOP_epilog, tn_new, tn_old, omega_bp );
    }
    if ( _trace_CG_RTran ) {
      #pragma mips_frequency_hint NEVER
      fprintf( TFile, "<cio> Mark_Op_For_Epilog backpatch:  "
	       "TN%d[%d] --> TN%d\n",
	       TN_number( tn_old ), omega_bp, TN_number( tn_new ) );
    }
    Set_OP_opnd( op_epilog, opnd, tn_new );
  }

  // A store should have no result TNs
  Is_True( OP_results( op ) == 0,
	   ( "CIO_RWTRAN::Mark_Op_For_Epilog expected zero results" ) );

  // Step (5)  Record (op, omega) --> op_epilog and return the new instruction
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile,
	     "<cio> Mark_Op_For_Epilog(op 0x%p, omega %u) ---> op 0x%p\n\t",
	     op, omega, op_epilog );
    Print_OP_No_SrcLine( op );
    fprintf( TFile, "\t" );
    Print_OP_No_SrcLine( op_epilog );
  }
  _op_epilog_map[ OP_OMEGA( op, omega ) ] = op_epilog;
  return op_epilog;
}


void
CIO_RWTRAN::Copy_Ops_To_Epilog()
{
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "\n<cio> CIO_RWTRAN::Copy_Ops_To_Epilog copying:\n" );
  }

  UINT8 omega = MAX_OMEGA;
  TN *tn_predicate = NULL;
  OPS ops = OPS_EMPTY;

  // Iterate through all of the OP duplicates, ordered first by omega
  // and then by the order of the original OPs within the loop body.
  for ( op_copy_map_epilog::const_iterator op_iter = _op_epilog_map.begin();
	op_iter != _op_epilog_map.end();
	++op_iter ) {

    // Has omega changed?
    if ( omega != op_iter->first.second ) {
      omega = op_iter->first.second;
      if ( ! TN_has_value( _trip_count_tn ) ) {

	// Generate new predicate to test ( _trip_count_tn >= omega )
	tn_predicate = Gen_Predicate_TN();

	// Generate OP:  tn_predicate <-- _trip_count_tn >= omega
	INT size = TN_size( _trip_count_tn );
	TN *tn_omega = Gen_Literal_TN( omega, size );
	if ( CGTARG_Can_Predicate() ) {
	  TN *tn_predicate_false = Gen_Predicate_TN();
	  VARIANT variant = ( size == 4 ? V_BR_I4GE : V_BR_I8GE );
	  Exp_Pred_Compare( tn_predicate, tn_predicate_false,
			    _trip_count_tn, tn_omega, variant, &ops );
	} else {
	  OPCODE opcode = ( size == 4 ? OPC_I4I4GE : OPC_I4I8GE );
	  Exp_OP2( opcode, tn_predicate, _trip_count_tn, tn_omega, &ops );
	}
	if ( _trace_CG_RTran ) {
	#pragma mips_frequency_hint NEVER
	fprintf( TFile,
		 "\tAdded: predicate TN%d :- trip_count TN%d >= omega %d\n",
		 TN_number( tn_predicate ), TN_number( _trip_count_tn ),
		 omega );
	}
      }
    }

    if ( ! TN_has_value( _trip_count_tn )
	 || TN_value( _trip_count_tn ) >= omega ) {

      // Append the OP duplicate to the loop epilog
      OPS_Append_Op( &ops, op_iter->second );
      if ( ! TN_has_value( _trip_count_tn ) ) {
	Predicate_Write( &ops, op_iter->second, tn_predicate );
      }
      if ( _trace_CG_RTran ) {
	#pragma mips_frequency_hint NEVER
	fprintf( TFile, "\t(op 0x%p, omega %u) ---> op 0x%p\n\t",
		 op_iter->first.first, omega, op_iter->second );
	Print_OP_No_SrcLine( op_iter->second );
      }
    }
  }

  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "<cio> Copy_Ops_To_Epilog prepends to epilog:\n" );
    Print_OPS( &ops );
    fprintf( TFile, "\n" );
  }

  BB_Prepend_Ops( CG_LOOP_epilog, &ops );

  _op_epilog_map.clear();
}


// ======================================================================
//
// Backpatch_Op_In_Epilog( op, start_omega )
// Invokes Mark_Op_For_Epilog to insert into the loop epilog copies of
// op corresponding to the last start_omega'th passes through the
// loop body.  Backpatch_Op_In_Epilog returns TRUE iff Mark_Op_For_Epilog
// is successful.
//
// ======================================================================


BOOL
CIO_RWTRAN::Backpatch_Op_In_Epilog( OP *op, const UINT8 start_omega )
{
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "<cio> Backpatch_Op_In_Epilog for [%u...1]\n\t",
	     start_omega );
    Print_OP_No_SrcLine( op );
  }

  for ( UINT8 omega = start_omega; omega > 0; --omega )
    if ( Mark_Op_For_Epilog( op, omega ) == NULL ) return FALSE;
  return TRUE;
}


// ======================================================================
//
// Read_CICSE_Candidate_Op returns TRUE iff op is a candidate for read
// elimination or cross iteration common subexpression elimination.
//
// Write_Candidate_Op returns TRUE iff op is a candidate for write
// elimination.
//
// Read_Candidate_Arc returns TRUE iff arc is a candidate for read
// elimination.
//
// Write_Candidate_Arc returns TRUE iff arc is a candidate for write
// elimination.
//
// If particular, if:
//   Read_Candidate_Arc( arc )
//   Read_CICSE_Candidate_Op( ARC_pred( arc ) )
//   Read_CICSE_Candidate_Op( ARC_succ( arc ) )
// all return TRUE, then read elimination should be a legal (though not
// necessarily wise) optimization for arc.
// Similarly, if:
//   Write_Candidate_Arc( arc )
//   Write_Candidate_Op( ARC_pred( arc ) )
//   Write_Candidate_Op( ARC_succ( arc ) )
// all return TRUE, then write elimination should be a legal (though not
// necessarily wise) optimization for arc.
//
// Read_CICSE_Candidate_Op and Write_CICSE_Candidate_Op are invoked just
// once for each OP.  Read_Candidate_Arc and Write_Candidate_Arc may be
// invoked multiple times.
//
// ======================================================================


inline BOOL
CIO_RWTRAN::Read_CICSE_Candidate_Op( OP *op )
{
#ifdef TARG_X8664
  /* Bug096: a cmp operations is not suitable for read elimination or
     cross iteration cse. */
  if( OP_icmp(op ) )
    return FALSE;

  if (OP_code(op) == TOP_lddqu || OP_code(op) == TOP_lddqa)
    return FALSE;
#endif /* TARG_X8664 */

#ifdef TARG_MIPS
  // Avoid backpatching OPs that define fcc.  Backpatches may require copies
  // and there is no MIPS instruction to copy between fcc registers.  Bug
  // 12428.
  if (OP_results(op) &&
      TN_register_class(OP_result(op, 0)) == ISA_REGISTER_CLASS_fcc) {
    return FALSE;
  }
#endif

  if ( OP_has_implicit_interactions( op ) ||
       OP_opnds( op ) > OP_MAX_FIXED_OPNDS )
    return FALSE;

  if ( ! OP_memory( op ) )
    return TRUE;

  if ( OP_prefetch( op )      ||
       OP_unalign_store( op ) ||
       OP_unalign_ld( op )    ||

       // Skip predicated writes
       ( OP_store( op ) && OP_cond_def( op ) ) ||

       // If we optimize small loads/stores then we need to
       // introduce a truncation or sign-extension operation.
       // Until we figure out how to do that, keep the problem
       // from happening (pv647031).

       CGTARG_Mem_Ref_Bytes( op ) < 4 )
    return FALSE;

  return TRUE;
}


inline BOOL
CIO_RWTRAN::Write_Candidate_Op( OP *op )
{
  return ( OP_store( op )
	   && ! ( OP_unalign_store( op ) ||
		  OP_has_implicit_interactions( op ) ||

		  // If we optimize small loads/stores then we need to
		  // introduce a truncation or sign-extension operation.
		  // Until we figure out how to do that, keep the problem
		  // from happening (pv647031).

		  CGTARG_Mem_Ref_Bytes( op ) < 4 ) );
}


inline BOOL
CIO_RWTRAN::Read_Candidate_Arc( ARC *arc )
{
  return ( ( ARC_kind( arc ) == CG_DEP_MEMIN ||
	     ARC_kind( arc ) == CG_DEP_MEMREAD )
	   && ARC_is_definite( arc )
	   && ARC_pred( arc ) != ARC_succ( arc )
	   && ARC_omega( arc ) <= CIO_rw_max_omega
	   && ARC_omega( arc ) < MAX_OMEGA );
}


inline BOOL
CIO_RWTRAN::Write_Candidate_Arc( ARC *arc )
{
  return ( ARC_kind( arc ) == CG_DEP_MEMOUT
	   && ARC_is_definite( arc )
	   && ARC_pred( arc ) != ARC_succ( arc )
	   && ARC_omega( arc ) <= CIO_rw_max_omega 
	   && ARC_omega( arc ) < MAX_OMEGA );
}


// ======================================================================
// During the analysis phase for cross-iteration read elimination and
// common sub-expression elimination, an array of CICSE_entrys is
// maintained with one CICSE_entry for each OP in the loop body, in that
// same order.
//
// -> op is that OP
// -> source is the index of an OP (usually just op) that produces the
//    same value as op during the current or an earlier iteration.
//    (Predicates are ignored).
// -> omega equals the number of iterations ago that source matches op
// -> basis == TRUE iff this OP is a memory read or write.  The source of
//    this OP can be propagated, but no sources should be propagated to
//    it from its operands.
// -> keep_pred, this entry is a source entry and op has predicate,
//    should it keep predicate when optimized with CICSE
// -> potential_match is the index of the closest CICSE_entry before this
//    one that could possibly produce the same value in a different
//    iteration.  potential_match == 0 if there is none, or if basis ==
//    TRUE.
// -> last_nonzero_opnd_source is the largest operand index with
//    opnd_source[last_nonzero_opnd_source] > 0, or -1 if all are zero.
// -> opnd_source[] gives the index of the source OP of the operand TNs.
//    (Its value is 0 if the operand TN is constant within the loop body.)
//    (Predicates are NOT ignored.)
// -> opnd_result[] indicates which result TN (usually 0) of the source
//    OP corresponds to the operand TN.  (If opnd_source[] == 0 or the
//    source OP is a STORE, then this value is meaningless.)
// -> opnd_omega[] gives the number of iterations ago that the operand's
//    source OP produced the operand TN value.
// -> result_unique[] is TRUE iff this is the only op with this result TN
//    
//
// If source == 0, then that OP is not a suitable candidate for
// optimization, and only the entry's op value is required to be valid.
// ======================================================================


struct CICSE_entry {
  OP    *op;
  INT    source;
  UINT8  omega;
  BOOL   basis;
  BOOL   keep_pred;
  INT    potential_match;
  INT    last_nonzero_opnd_source;
  INT    opnd_source[OP_MAX_FIXED_OPNDS];
  INT    opnd_result[OP_MAX_FIXED_OPNDS];
  UINT8  opnd_omega[OP_MAX_FIXED_OPNDS];
  BOOL   result_unique[OP_MAX_FIXED_RESULTS];
};


// ======================================================================
// During the transformation phase for cross-iteration read elimination
// and common sub-expression elimination, an array of CICSE_changes is
// maintained with one CICSE_change for each OP in the eligible for
// elimination.
//
// -> okay == TRUE iff the transformation is still viable
// -> Within each set of viable CICSE_changes that share the same source,
//    exactly one of them has duplicate_source == FALSE.
// -> op is the OP to be eliminated
// -> souce is the OP that will generate the values instead of op
// -> source index in cicse table, used for getting cicse entry info
//    this is only used when back trace if a op can be executed 
//    unconditionally.
// -> omega equals the number of iterations ago that source matches op
// -> new_tns is an array of TNs that store the results of the source OP.
//    Each new_tn either is the result TN of the source OP, or a newly
//    generated TN of the same kind.
// Often, op and source have the same opccode.  The one exception is
// that op can be a LOAD when source is a WRITE.  This case must be
// handled separately in the code implementation below.
// ======================================================================


struct CICSE_change {
  BOOL   okay;
  BOOL   duplicate_source;
  OP    *op;
  OP    *source;
  INT    source_idx;
  UINT8  omega;
  TN    *new_tns[OP_MAX_FIXED_RESULTS];
  BOOL   need_copy[OP_MAX_FIXED_RESULTS];
};


// ======================================================================
// Trace_CICSE_Entries  and  Trace_CICSE_Changes  print the contents of
// the an array of CICSE_entry or CICSE_change, respectively, into TFile.
// ======================================================================


void
CIO_RWTRAN::Trace_CICSE_Entries( CICSE_entry *table, INT count,
				 const char *message )
{
  Is_True( TFile, ( "CIO_RWTRAN::Trace_CICSE_Entries -- TFile is NULL\n" ) );
  fprintf( TFile, "<cio> CIO_RWTRAN::Trace_CICSE_Entries(count %d) -- %s",
	   count, message );

  // Assumes table[0] is not used
  for ( INT index = 1; index <= count; ++index ) {
    CICSE_entry& entry = table[index];
    fprintf( TFile, "\n%2d: ", index );
    Print_OP_No_SrcLine( entry.op );

    fprintf( TFile, "    op 0x%p, source %2d, omega %u, basis %d,"
	     " match %d, lnz_opnd %d\n    opnds:",
	     entry.op, entry.source, entry.omega, entry.basis,
	     entry.potential_match, entry.last_nonzero_opnd_source );
    if ( entry.source == 0 )
      fprintf( TFile, " OP_opnds(entry.op) == %d", OP_opnds( entry.op ) );
    else if ( OP_opnds( entry.op ) > OP_MAX_FIXED_OPNDS )
      fprintf( TFile, " OP_opnds(entry.op) %d > OP_MAX_FIXED_OPNDS %d",
	       OP_opnds( entry.op ), OP_MAX_FIXED_OPNDS );
    else
      for ( INT opnd = 0; opnd < OP_opnds( entry.op ); ++opnd )
	fprintf( TFile, "  opnd %d: src %2d, om %u;",
		 opnd, entry.opnd_source[opnd], entry.opnd_omega[opnd] );
    fprintf( TFile, "\n    results:" );
    for ( INT res = 0; res < OP_results( entry.op ); ++res )
      fprintf( TFile, "  res %d: unique %d;", res, entry.result_unique[res] );
  }
  fprintf( TFile, "\n\n" );
}


void
CIO_RWTRAN::Trace_CICSE_Changes( CICSE_change *table, INT count,
				 const char *message )
{
  Is_True( TFile, ( "CIO_RWTRAN::Trace_CICSE_Changes -- TFile is NULL" ) );
  fprintf( TFile, "<cio> CIO_RWTRAN::Trace_CICSE_Changes(count %d) -- %s",
	   count, message );
  for ( INT index = 0; index < count; ++index ) {
    CICSE_change& change = table[index];
    fprintf( TFile, "\n%2d: okay %d, duplicate %d, op 0x%p, source 0x%p,"
	     " omega %u\n  op:      ",
	     index, change.okay, change.duplicate_source,
	     change.op, change.source, change.omega );
    Print_OP_No_SrcLine( change.op );
    fprintf( TFile, "  source:  " );
    Print_OP_No_SrcLine( change.source );
    fprintf( TFile, "  results: " );
    if ( OP_results( change.op ) > 0 ) {
      Print_TN( change.new_tns[0], FALSE );
      for ( INT res = 1; res < OP_results( change.op ); ++res ) {
	fprintf( TFile, ", " );
	Print_TN( change.new_tns[res], FALSE );
	fprintf( TFile, " %d", change.need_copy[res] );
      }
    }
  }
  fprintf( TFile, "\n\n" );
}


// ======================================================================
// CICSE_Lookup_Op  returns an index with cicse_table[index].op == op.
// If none exists, returns 0.  Requires that the CICSE_entry.op OPs have
// the same ordering in cicse_table as in the loop body, so that
// OP_Precedes can be used in a binary search.
// ======================================================================


INT
CIO_RWTRAN::CICSE_Lookup_Op( CICSE_entry *cicse_table, INT count, OP *op )
{
  // Assumes table[0] is not used
  INT left  = 1;
  INT right = count;
  while ( left <= right ) {
    INT middle = ( left + right ) / 2;
    OP *middle_op = cicse_table[middle].op;
    if ( op == middle_op )
      return middle;
    if ( OP_Precedes( op, middle_op ) )
      right = middle - 1;
    else
      left = middle + 1;
  }
  return 0;
}


// ======================================================================
//
// Replace_Tn
//
// Replace_Tn substitutes tn_new (with omega_change omega offset) for
// tn_old throughout the body and the prolog and epilog backpatches of
// the current loop.
//
// This transformation assumes that any exposed use will have had the
// necessary setup added to the Prolog and Epilog backpatches.
// Therefore, we need only rename the setup occurrences, rather than
// creating new ones.  This implies that no global TN processed will
// have exposed uses.
//
// This assumption implies that this routine will not be used to
// rename a TN with only uses (no definitions), which would be global,
// and would have exposed uses with zero omega.
//
// ======================================================================


void
CIO_RWTRAN::Replace_Tn( BB *body, TN *tn_old, TN *tn_new, UINT8 omega_change )
{
  Is_True( TN_is_float( tn_old ) == TN_is_float( tn_new ),
	   ( "CIO_RWTRAN::Replace_Tn old TN%d and new TN%d have incompatible"
	     " types", TN_number( tn_old ), TN_number( tn_new ) ) );
  
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "CIO_RWTRAN::Replace_Tn changing " );
    Print_TN( tn_old, FALSE );
    fprintf( TFile, " to " );
    Print_TN( tn_new, FALSE );
    fprintf( TFile, "[%u] in BB:%d\n", omega_change, BB_id( body ) );
  }

  // Replace all occurrences in the body
  OP *op;
  FOR_ALL_BB_OPs_FWD( body, op ) {

    // Examine operands of op
    for ( INT opnd = OP_opnds( op ) - 1; opnd >= 0; --opnd ) {
      if ( OP_opnd( op, opnd ) == tn_old ) {

	// Replace operand TN and update omega
	Set_OP_opnd( op, opnd, tn_new );
	Set_OP_omega( op, opnd, OP_omega( op, opnd ) + omega_change );
      }
    }
  }

#ifdef KEY
  // Update any occurances in the prolog backpatch list.  Bug 11749.
  CG_LOOP_Backpatch_Replace_Body_TN( CG_LOOP_prolog,
				     tn_old, tn_new, omega_change );
#else
  // Delete all prolog backpatches for tn_old;
  // Calling procedure must guarantee that tn_new has appropriate backpatches
  CG_LOOP_BACKPATCH *bp, *bp_next = NULL;
  for ( bp = CG_LOOP_Backpatch_First( CG_LOOP_prolog, tn_old );
	bp != NULL; bp = bp_next ) {
    bp_next = CG_LOOP_Backpatch_Next( bp );
    CG_LOOP_Backpatch_Delete( CG_LOOP_prolog, bp );
  }
#endif  

  // Update any occurances in the epilog backpatch list
  CG_LOOP_Backpatch_Replace_Body_TN( CG_LOOP_epilog,
				     tn_old, tn_new, omega_change );
  
  // Is it live in the tail block?
  if ( GRA_LIVE_TN_Live_Outof_BB( tn_old, CG_LOOP_epilog ) )
    CG_LOOP_Backpatch_Add( CG_LOOP_epilog, tn_old, tn_new, omega_change );
}


// ======================================================================
//
// Append_TN_Copy inserts, after point, a single new OP that performs a
// TN copy tn_dest <-- tn_from.  It returns a pointer to the new OP.
//
// ======================================================================


OP *
CIO_RWTRAN::Append_TN_Copy( TN *tn_dest, TN *tn_from, OP *point, UINT8 omega )
{
  // Insert tn_dest <-- tn_from[omega] after point
  OP *old_op_next = OP_next( point );
  CGPREP_Copy_TN( tn_dest, tn_from, point, omega, FALSE );
  OP *op_copy = OP_next( point );
  // Make sure only one OP was inserted
  Is_True( OP_next( op_copy ) == old_op_next,
	   ( "CIO_RWTRAN::Append_TN_Copy appended more than one new OP" ) );
  OP_srcpos( op_copy ) = OP_srcpos( point );
  if ( Is_DB_OP_Init( point ) )
    DB_Copy_Aux_OP( op_copy, point );
  return op_copy;
}

// ======================================================================
//
// CICSE_Check_OP_Unconditionally
//   check if cicse_table[index] is can executed unconditionally in BB.
//   1. result TN is local TN not GTN and unique assign in BB.
//      this can guarantee unconditionaly assign to this reuslt will 
//      not affect other read/write
//   2. All TN's opnd_source's op is also "Unconditionally"
//      this guarantee when unconditionally exectute this op, no
// ======================================================================
BOOL CICSE_Check_OP_Unconditionally( CICSE_entry *cicse_table, INT index,
                                     UINT32 level )
{
  // this means no definitly define op in BB.
  // can't get any analysis result.
  if ( index == 0 ) {
    return FALSE;
  }

  CICSE_entry& entry = cicse_table[index];
  OP* op = entry.op;
  Is_True(op, ("checking NULL op\n"));

  // flag1 means this op being checked by CICSE_Check_OP_Unconditionally
  // this can avoid inifite deeper trace and lead check return false.
  if ( OP_flag1( op ) ) {
    return TRUE;
  }

  // if op has no predicate, return ture.
  if ( !OP_has_predicate( op )
    || OP_opnd( op, OP_PREDICATE_OPND ) == True_TN ) {
    return TRUE;
  }

  // exceed detect level limitation.
  // no more check
  if ( level >= CICSE_MAX_TRACE_LEVEL ) {
    return FALSE;
  }


  // 1. result tn is global or result is not unique, return false.
  for ( INT i = 0; i < OP_results(op); i++ ) {
    if ( TN_is_global_reg( OP_result( op, i ) ) ) {
      return FALSE;
    }
    else if ( !entry.result_unique[i] ) {
      return FALSE;
    }
  }

  if ( entry.source == 0 ) {
    // no source info
    for ( INT i = 0 ; i < OP_opnds( op ); i++ ) {
      TN* opnd = OP_opnd(op, i);
#ifdef TARG_IA64
      if ( TN_is_predicate(opnd) && i == OP_PREDICATE_OPND ) {
        continue;
      }
#endif
      // only allow TN be constant, GTN sp,bp, r0, f0, f1
      if ( TN_is_constant( opnd ) || ( opnd == FP_TN )
#ifdef TARG_IA64
          || ( opnd == Zero_TN ) || ( opnd == FOne_TN ) || ( opnd == FZero_TN )
#endif
         ) {
      }
      else {
        return FALSE;
      }
    }
  }
  else {
    // check all source tn
    for ( INT i = 0; i < OP_opnds( op ); i++ ) {
      TN* opnd = OP_opnd( op, i );
#ifdef TARG_IA64
      if ( TN_is_predicate( opnd ) && i == OP_PREDICATE_OPND ) {
        continue;
      }
#endif
      if ( TN_is_constant( opnd ) ) {
      }
      else if ( TN_is_register( opnd ) &&
               !TN_is_global_reg( opnd ) ) {
        Set_OP_flag1( op );
        INT opnd_src_index = entry.opnd_source[i];
        if ( !CICSE_Check_OP_Unconditionally( cicse_table, opnd_src_index,
            level+1 ) ) {
          Reset_OP_flag1( op );
          return FALSE;
        }
        Reset_OP_flag1( op );
      }
      else
        return FALSE;
    }
  }

  return TRUE;
}

// ======================================================================
//
// CICSE_Mark_OP_Unconditionally
//   recursivly mark op and its opnd_source's op
// ======================================================================
void CICSE_Mark_OP_Unconditionally( CICSE_entry *cicse_table, INT index,
                                    UINT32 level )
{
  if ( index == 0 ) {
    return;
  }
  CICSE_entry& entry = cicse_table[index];
  OP* op = entry.op;
  Is_True( op, ( "checking NULL op\n" ) );
  
  if ( OP_flag1( op ) ) {
    return;
  }
  if ( level >= CICSE_MAX_TRACE_LEVEL ) {
    return;
  }

  if ( OP_has_predicate( op ) &&
       OP_opnd( op, OP_PREDICATE_OPND ) != True_TN ) {
    // check this is op only write local TN and local TN is unique assigned.
    for ( INT i = 0; i < OP_results(op); i++ ) {
      Is_True ( !TN_is_global_reg( OP_result( op, i ) ) &&
        entry.result_unique[i], ( "assign TN must be local and unqiue assign." ) );
    }
    Set_OP_opnd( op, OP_PREDICATE_OPND, True_TN );

    for ( INT i = 0; i < OP_opnds ( op ); i++ ) {
      if ( i == OP_PREDICATE_OPND )
        continue;
      TN* opnd = OP_opnd(op, i);
      if ( TN_is_register( opnd ) && !TN_is_global_reg( opnd ) ) {
        INT opnd_src_idx = entry.opnd_source[i];
        Set_OP_flag1( op );
        CICSE_Mark_OP_Unconditionally( cicse_table, opnd_src_idx, level+1 );
        Reset_OP_flag1( op );
      }
      else {
        // TN can only be constant TN or unchanged GTN
        Is_True( TN_is_constant( opnd ) || opnd == FP_TN 
#ifdef TARG_IA64
          || ( opnd == Zero_TN ||
               opnd == FOne_TN || opnd == FZero_TN )
#endif
          ,("unexpected source in op when mark it unconditionally\n"));
      }
    }
  }
}

// ======================================================================
//

// CICSE_Check_Predicate_Subset
//   check if op's predicate is subset of source op's predicate
// ======================================================================
BOOL CICSE_Check_Predicate_Subset( CICSE_entry *cicse_table, INT op_index,
                                   INT source_index )
{
  OP* op = cicse_table[op_index].op;
  OP* src_op = cicse_table[source_index].op;
  Is_True( OP_has_predicate( op ), 
    ("op must have predicate, same with source op\n") );

  Is_True( ( cicse_table[op_index].source != 0 &&
             cicse_table[source_index].source != 0 ), 
           ("two op must have non-zero source\n") );

  TN* op_pred = OP_opnd( op, OP_PREDICATE_OPND );
  TN* src_op_pred = OP_opnd( src_op, OP_PREDICATE_OPND );
  

  if ( src_op_pred == True_TN )
    return TRUE;
  
  if ( op_pred == True_TN )
    return FALSE;

  // if omega is different, return false
  if ( cicse_table[op_index].opnd_omega[OP_PREDICATE_OPND] !=
       cicse_table[source_index].opnd_omega[OP_PREDICATE_OPND] )
    return FALSE;
  
  // if two predicate are same, return true.
  // 1. TN number same
  // 2. def is unqiue assign in BB.
  if ( TN_number( op_pred ) == TN_number( src_op_pred ) &&
       !TN_is_global_reg( op_pred ) ) {
    INT pred_src = cicse_table[op_index].opnd_source[OP_PREDICATE_OPND];
    Is_True( pred_src != 0, ("not found local predicate\n") );
    if ( cicse_table[pred_src].result_unique )
      return TRUE;
  }

  // check if one predicate is subset of another.
  // using pqs_manager
  if ( PQSCG_is_subset_of( op_pred, src_op_pred ) ) {
    return TRUE;
  }
  return FALSE;
}

// ======================================================================
//
// CICSE_Transform searches for opportunities to perform cross-iteration
// read elimination and cross-iteration common subexpression elimination.
// Next, it estimates the effects each transformation would have on the
// loop scheduling (using cg_sched_est.*), then performs beneficial
// transformations.  CICSE_Transform returns TRUE iff at least one
// transformation is performed on the loop code.
//
// ======================================================================


// CICSE_Transform uses a hash function to divide the loop body OPs into
// classes.  If two OPs could potentially be identified through cicse,
// they should land in the same class.

#define CICHT_SIZELOG2          6
#define CICHT_SIZE              (1 << CICHT_SIZELOG2)
#define CICHT_MASK              (CICHT_SIZE - 1)


BOOL
CIO_RWTRAN::CICSE_Transform( BB *body )
{
#ifdef TARG_X8664
  TN* rflags_tn = Rflags_TN();
#endif

  // Count the number of OPs in the loop body.
  OP *op;
  INT op_count = 0;
  FOR_ALL_BB_OPs_FWD( body, op ) {
    ++op_count;
  }

  // Allocate a table of CICSE_entry, with one entry for each loop OP.
  // cicse_table[0] is not used, because 0 is used to indicate constant
  // and global operand TNs
  CICSE_entry *cicse_table
    = (CICSE_entry *) CXX_NEW_ARRAY( CICSE_entry, op_count + 1,
                                     _loc_mem_pool );

  // The map tn_last_op remembers, for each TN, the index (in
  // cicse_table) of the most recent OP defining that TN
  hTN_MAP32 tn_last_op = hTN_MAP32_Create( _loc_mem_pool );

  // Initialize the cicse_table entries and the map tn_last_op.
  // Also, identify (set flag1 for) all OPs that are suitable candidates
  // for read/write elimination or cicse elimination.
  INT index;
  op = BB_first_op( body );
  for ( index = 1; index <= op_count; ++index ) {

    // Initialize the table entry for this OP
    CICSE_entry& entry = cicse_table[index];
    entry.op = op;
    entry.basis = FALSE;  // To be determined later
    entry.potential_match = 0;

    // Initialize the map tn_last_op
    for ( INT res = OP_results( op ) - 1; res >= 0; --res )
      hTN_MAP32_Set( tn_last_op, OP_result( op, res ), index );

    if ( Read_CICSE_Candidate_Op( op ) ) {
      Set_OP_flag1( op );
      entry.source = index;
    } else {
      Reset_OP_flag1( op );
      entry.source = 0;
    }

    op = OP_next( op );
  }

  // Initialize the operands of cicse_table entries.
  for ( index = 1; index <= op_count; ++index ) {
    CICSE_entry& entry = cicse_table[index];
    OP *op = entry.op;

    // Initialize the cicse_table entry for the operands of this OP op
    // to point to the operands' most recent definitions
    for ( INT opnd = OP_opnds( op ) - 1; opnd >= 0; --opnd ) {
      INT source = 0;
      TN *opnd_op = OP_opnd( op, opnd );
      if ( TN_is_register( opnd_op ) ) {
	source = hTN_MAP32_Get( tn_last_op, opnd_op );
#ifdef TARG_X8664
	// If OP uses rflag, don't eliminate the source OP that sets rflag.
	// Bug 4934.
	if (opnd_op == rflags_tn) {
	  CICSE_entry &source_entry = cicse_table[source];
	  Reset_OP_flag1(source_entry.op);
	  source_entry.source = 0;
	  source = 0;
	}
#endif
      }

#ifdef KEY
      // Don't eliminate OP if its result is a global TN.  See example in
      // bug 6255.
      for (INT res = OP_results(op) - 1; res >= 0; res--) {
	TN *tn = OP_result(op, res);
	if (TN_is_global_reg(tn)) {
	  CICSE_entry &source_entry = cicse_table[index];
	  Reset_OP_flag1(source_entry.op);
	  source_entry.source = 0;
	  source = 0;
	  break;
	}
      }
      // same as above:  stores have results also
      if (OP_results(op) == 0 && OP_store(op))
      {
        for (INT res = OP_opnds(op) - 1; res >= 0; res--) {
          TN *tn = OP_opnd(op, res);
          if (TN_is_global_reg(tn)) {
            CICSE_entry &source_entry = cicse_table[index];
            Reset_OP_flag1(source_entry.op);
            source_entry.source = 0;
            source = 0;
            break;
          }
        }
      }
#endif
      entry.opnd_source[opnd] = source;
      entry.opnd_omega[opnd]  = OP_omega( op, opnd );

      // Find the result matching the current operand
      INT res;
      if ( source > 0 ) {
	OP *op_src = cicse_table[source].op;
	for ( res = OP_results( op_src ) - 1; res >= 0; --res )
	  if ( OP_result( op_src, res ) == opnd_op )
	    break;
	Is_True( res >= 0, ( "CIO_RWTRAN::CICSE_Transform"
			     " unable to find result TN" ) );
      } else
        res = 0;
      entry.opnd_result[opnd] = res;
    }

    // Update the map tn_last_op
    for ( INT res = OP_results( op ) - 1; res >= 0; --res ) {
      INT32 old_index = hTN_MAP32_Get_And_Set( tn_last_op,
					       OP_result( op, res ), index );
      entry.result_unique[res] = ( old_index == index );
    }
#ifdef TARG_X8664
    // rflag is an implicit result.  Bug 4934.
    if (TOP_is_change_rflags(OP_code(op))) {
      hTN_MAP32_Set( tn_last_op, rflags_tn, index);
    }
#endif
  }

  // Search the CG dependence graph for definite MEMIN and MEMREAD
  // arcs, and record them in cicse_table.
  BOOL change = FALSE;
  for ( index = op_count; index > 0; --index ) {
    CICSE_entry& entry = cicse_table[index];
    if ( OP_load( entry.op ) && OP_flag1( entry.op ) ) {

      ARC_LIST *arcs = OP_preds( entry.op );
      while ( arcs ) {
	ARC *arc = ARC_LIST_first( arcs );
	arcs = ARC_LIST_rest( arcs );

	if ( Read_Candidate_Arc( arc ) && OP_flag1( ARC_pred( arc ) ) ) {

	  INT index_pred = CICSE_Lookup_Op( cicse_table,
					    op_count, ARC_pred( arc ) );
	  Is_True( index_pred > 0,
		   ( "CIO_RWTRAN::CICSE_Transform didn't find op_pred" ) );
	  entry.source = index_pred;
	  entry.omega = ARC_omega( arc );
	  entry.basis = TRUE;
	  cicse_table[index_pred].basis = TRUE;
	  change = TRUE;
	}
      }
    }
  }

  //   // Display cicse_table
  //   if ( _trace_CG_RTran ) {
  //     #pragma mips_frequency_hint NEVER
  //     Trace_CICSE_Entries( cicse_table, op_count, "Before CSE Analysis" );
  //   }

  // If there are no opportunities for read elimination, then there
  // will be no opportunities for CICSE elimination either.
  if ( ! change ) return FALSE;

  // Determine earliest sources of basis OPs;
  // Non-basis memory OPs can be ignored from now on.
  // First pass shrinks loops to rhos with single edge loops
  // (to avoid infinite looping -- see pv 787505)
  for ( index = op_count; index > 0; --index ) {
    CICSE_entry& entry = cicse_table[index];
    if ( entry.basis )
      while ( entry.source > 0 &&
	      cicse_table[entry.source].source < entry.source ) {
	entry.omega += cicse_table[entry.source].omega;
	entry.source = cicse_table[entry.source].source;
      }
    else
      if ( OP_memory( entry.op ) )
	entry.source = 0;
  }
  // Second pass finishes the job for non-loop nodes
  for ( index = op_count; index > 0; --index ) {
    CICSE_entry& entry = cicse_table[index];
    if ( entry.basis )
      while ( entry.source > 0 &&
	      cicse_table[entry.source].source != entry.source ) {
	entry.omega += cicse_table[entry.source].omega;
	entry.source = cicse_table[entry.source].source;
      }
  }

  // Perform CICSE analysis
  if ( CIO_enable_cse_removal ) {

    // Propagate non-candidate OPs (with source == 0) through operands
    do {
      change = FALSE;
      // (Forward iteration of index probably faster here)
      for ( INT index = 1; index <= op_count; ++index ) {
	CICSE_entry& entry = cicse_table[index];
	if ( entry.source > 0 && ! entry.basis )
	  for ( INT opnd = OP_opnds( entry.op ) - 1; opnd >= 0; --opnd ) {
	    INT source = entry.opnd_source[opnd];
	    if ( source > 0 && cicse_table[source].source == 0 ) {
	      entry.source = 0;
	      change = TRUE;
	    }
	  }
      }
    } while ( change );

    // Calculate last_nonzero_opnd_source for each relevant OP
    for ( index = op_count; index > 1; --index ) {
      CICSE_entry& entry = cicse_table[index];
      if ( entry.source == 0 || entry.basis ) continue;

      // Find the last operand of entry with nonzero opnd_source
      INT opnd = OP_opnds( entry.op ) - 1;
      while ( opnd >= 0 && entry.opnd_source[opnd] == 0 )
	--opnd;
      entry.last_nonzero_opnd_source = opnd;
    }

    // Identify potential matches (OPs with identical operands, number
    // of arguments, and last_nonzero_opnd_source).  The potential_match
    // fields form a linked list of potential matches, each indexing the
    // most recent previous entry that is a potential match.  All OPs
    // with source == 0 or basis == TRUE will have potential_match == 0.

    // First, use a hash function to separated the OPs into buckets;
    // Potential matches always land in the same class

    INT hash_buckets[CICHT_SIZE];
    INT bucket;
    for ( bucket = 0; bucket < CICHT_SIZE; ++bucket )
      hash_buckets[bucket] = 0;

    // Note: This loop must iterate _forward_ through the loop body OPs
    for ( index = 1; index <= op_count; ++index ) {
      CICSE_entry& entry = cicse_table[index];
      if ( entry.source == 0 || entry.basis ) continue;

      // Apply the hash function
      bucket = ( OP_code( entry.op ) + entry.last_nonzero_opnd_source )
	& CICHT_MASK;
      entry.potential_match = hash_buckets[bucket];
      hash_buckets[bucket] = index;
    }

    // Next, subdivide the OPs within each bucket into potential_matches,
    // according to opcode, last_nonzero_opnd_source, and matching locally
    // constant operands
    for ( bucket = 0; bucket < CICHT_SIZE; ++bucket ) {
      INT index2, next_index2;
      for ( index2 = hash_buckets[bucket]; index2 > 0; index2 = next_index2 ) {
	CICSE_entry& entry2 = cicse_table[index2];
	next_index2 = entry2.potential_match;

	INT index1, next_index1;
	for ( index1 = next_index2; index1 > 0; index1 = next_index1 ) {
	  CICSE_entry& entry1 = cicse_table[index1];
	  next_index1 = entry1.potential_match;

	  if ( OP_code( entry1.op ) != OP_code( entry2.op ) ||
	       entry1.last_nonzero_opnd_source
	       != entry2.last_nonzero_opnd_source ) continue;
	  Is_True( OP_opnds( entry1.op ) == OP_opnds( entry2.op ),
		   ( "CIO_RWTRAN::CICSE_Transform operand counts differ" ) );

	  // Do locally constant operands match?
	  INT opnd;
	  for ( opnd = OP_opnds( entry2.op ) - 1; opnd >= 0; --opnd )
	    if ( entry2.opnd_source[opnd] == 0 &&
		 OP_opnd( entry1.op, opnd ) != OP_opnd( entry2.op, opnd ) )
	      break;
	  // LATER: Add code here to handle ADD and other commutative OPs
	  if (opnd >= 0) continue;

	  // Found a potential match
	  break;
	}
	// Update entry2 with potential match index, or 0 if there is none
	entry2.potential_match = index1;
      }
    }

    // Find all matches among OPs whose operands are all locally constant;
    // index1 and index2 (with index1 < index2) loop through OPs with no
    // other sources and last_nonzero_opnd_source == -1;
    // index1 loops through potential_match linked list of indicies.

    for ( INT index2 = 2; index2 <= op_count; ++index2 ) {
      CICSE_entry& entry2 = cicse_table[index2];
      if ( entry2.last_nonzero_opnd_source >= 0 ) continue;

      INT index1 = entry2.potential_match;
      if ( index1 > 0 ) {

	// There is a match; Find the earliest source
	while ( cicse_table[index1].potential_match != 0 )
	  index1 = cicse_table[index1].potential_match;

	// Record the match!
	entry2.source = index1;
	entry2.omega = 0;
      }
    }

    // Now handle OPs with locally nonconstant operands.  Matches can enable
    // other matches.  Repeat the following loop body until no changes occur.
    do {

      // Determine earliest sources
      INT index;
      for ( index = op_count; index > 0; --index ) {
	CICSE_entry& entry = cicse_table[index];
	while ( entry.source > 0 &&
		cicse_table[entry.source].source != entry.source ) {
	  entry.omega += cicse_table[entry.source].omega;
	  entry.source = cicse_table[entry.source].source;
	}
      }

      // Update sources of operands
      for ( index = op_count; index > 0; --index ) {
	CICSE_entry& entry = cicse_table[index];
	if ( entry.source > 0 && ! entry.basis )
	  for ( INT opnd = OP_opnds( entry.op ) - 1; opnd >= 0; --opnd ) {
	    INT source = entry.opnd_source[opnd];
	    if ( source > 0 && cicse_table[source].source != source ) {
	      entry.opnd_source[opnd] = cicse_table[source].source;
	      entry.opnd_omega[opnd] += cicse_table[source].omega;
	      // entry.opnd_result[opnd] remains the same
	    }
	  }
      }

      // Look for new matches of source OPs;
      // index1 and index2 (with index1 < index2) loop through OPs with no
      // other sources; index2 is increasing for better performance;
      // index1 loops through potential_match linked list of indicies.

      change = FALSE;

      for ( INT index2 = 2; index2 <= op_count; ++index2 ) {
	CICSE_entry& entry2 = cicse_table[index2];
	if ( entry2.source != index2 ||
	     entry2.last_nonzero_opnd_source < 0 ) continue;

	// Update entry2.potential_match and initialize index1
	INT index1 = entry2.potential_match;
	if ( index1 > 0 && cicse_table[index1].source != index1 ) {
	  do {
	    index1 = cicse_table[index1].potential_match;
	  } while ( index1 > 0 && cicse_table[index1].source != index1 );
	  entry2.potential_match = index1;
	}

	// Loop index1 through linked list of potential_match indicies
	// For each, decide if index1 and index2 OPs match, in that one
	// could act as a source for the other
	while ( index1 > 0 ) {
	  CICSE_entry& entry1 = cicse_table[index1];

	  // Do opnd_sources and opnd_results match?  (Locally constant
	  // operands have already been checked.)
	  INT opnd;
	  for ( opnd = OP_opnds( entry2.op ) - 1; opnd >= 0; --opnd )
	    if ( entry1.opnd_source[opnd] != entry2.opnd_source[opnd] ||
#ifdef OSP_OPT
		 //Bug fix for OSP_239
		 //TODO: This bug fix could be refined
		 (entry1.opnd_source[opnd] > index1 && entry1.opnd_source[opnd] < index2) ||
#endif
		 entry1.opnd_result[opnd] != entry2.opnd_result[opnd] )
	      break;
	  // Add code to handle ADD and other commutative OPs
	  if (opnd < 0) {

	    // opnd_sources do match.  Do opnd_omegas differ consistently?
	    opnd = entry2.last_nonzero_opnd_source;
	    UINT8 omega1 = entry1.opnd_omega[opnd];
	    UINT8 omega2 = entry2.opnd_omega[opnd];
	    for ( --opnd; opnd >= 0; --opnd )
	      if ( entry1.opnd_source[opnd] > 0 && 
		   entry1.opnd_omega[opnd] + omega2
		   != entry2.opnd_omega[opnd] + omega1 )
		break;
	    if ( opnd < 0 ) {

	      // Found a match!
	      change = TRUE;
	      if ( omega1 > omega2 ) {
		entry1.source = index2;
		entry1.omega = omega1 - omega2;
		// Don't break out of do loop; just proceed to next index1
	      } else {
		entry2.source = index1;
		entry2.omega = omega2 - omega1;
		break; // Break out of do loop
	      }
	    }
	  }

	  // Update entry1.potential_match and get next index1
	  index1 = entry1.potential_match;
	  if ( index1 > 0 && cicse_table[index1].source != index1 ) {
	    do {
	      index1 = cicse_table[index1].potential_match;
	    } while ( index1 > 0 && cicse_table[index1].source != index1 );
	    entry1.potential_match = index1;
	  }
	}  // index1 while loop
      }  // index2 for loop
    } while ( change );

    // for non store op with predicate, whose predicate is not True_TN 
    // optimization will remove its predicate, so before this action
    // we should guarantee if this op's all opnd_src[] op can be executed
    // unconditionally.
    //
    // the conditions to check if a op can be executed unconditionally
    // 1. this op assign to a local Tn and this local TN is assgined once
    //    in BB
    // 2. all its opnd_src op can be assgined unconditionally.
    //
    // clear flag1, used as visited flag
    op = BB_first_op( body );
    for ( index = 1; index <= op_count; ++index ) {
      Reset_OP_flag1(op);
      op = OP_next( op );

      cicse_table[index].keep_pred = TRUE;
    }
    // if all find cicse opptr is not valid, abor early
    BOOL find_valid_opt = FALSE;
    for ( index = op_count; index > 0; --index ) {

      CICSE_entry& entry = cicse_table[index];

      if ( entry.source != 0 && entry.source != index ) {
        CICSE_entry& entry_src = cicse_table[entry.source];
        // if op and its source op all have predicate
        // check if op's predicate is subset of source op's predicate
        // suppose ops are
        // (p1)  source_op
        // ....
        // (p2)  op1
        // ....
        // (p3)  op2
        //
        // all entries keep predicate are initialized true
        //
        // if p2 is subset of p1,
        //   no change
        // if p2 is not subset of p1,
        //   if source_op can execute with true_TN
        //     record not keep predicate on source_op and op1
        //   else
        //     don't change, mark op1 can't be optimized
        //
        // when analysis op2
        // if source_op need keep predicate
        //   check if p3 is subset of p1
        //   ...
        // else this indicate source_op can execute unconditionally
        //   no further check. op2 can optimized withCICSE
        //   
        if( !OP_has_predicate( entry_src.op)
            || OP_opnd( entry_src.op, OP_PREDICATE_OPND ) == True_TN ) {
          find_valid_opt = TRUE;
          continue;
        }

        // source op can executed without predicate
        if (entry_src.keep_pred == FALSE) {
          find_valid_opt = TRUE;
          continue;
        }

        BOOL is_subset = CICSE_Check_Predicate_Subset( cicse_table, index, entry.source );
        if ( is_subset ) {
          find_valid_opt = TRUE;
          // do nothing
        }
        else if ( OP_load( entry.op ) ) {
          if ( !CICSE_Check_OP_Unconditionally( cicse_table, entry.source, 0 ) ) {
            entry.source = 0;
          }
          else {
            find_valid_opt = TRUE;
            entry_src.keep_pred = FALSE;
          }
        }
        else {
          find_valid_opt = TRUE;
          entry_src.keep_pred = FALSE;
        }
      }
    }

    // early abor to avoid following asssertion.
    if ( find_valid_opt == FALSE )
      return FALSE;
  }  // preform CICSE analysis

  // Display cicse_table
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    Trace_CICSE_Entries( cicse_table, op_count, "After CSE Analysis" );
  }

  // Count the number of optimization opportunities available
  INT oppor_count = 0;
  for ( index = op_count; index > 0; --index )
    if ( cicse_table[index].source != index && cicse_table[index].source > 0 )
      ++oppor_count;
  Is_True( oppor_count > 0,
	   ( "CIO_RWTRAN::CICSE_Transform didn't abort early" ) );

#ifdef KEY
  // A heuristic to avoid imposing too much pressure on register allocation.
  if( oppor_count >= ( REGISTER_MAX >> 1 ) ){
    return FALSE;
  }
#endif

  // Store the opportunites into CIO_RWTRAN.opportunities
  CICSE_change *opportunities
    = (CICSE_change *) CXX_NEW_ARRAY( CICSE_change, oppor_count,
				      _loc_mem_pool );

  INT oppor_index = 0;
  for ( index = 1; index <= op_count; ++index ) {
    CICSE_entry& entry = cicse_table[index];
    if ( entry.source == 0 || entry.source == index ) continue;

    CICSE_change& change = opportunities[oppor_index];
    ++oppor_index;

    change.okay             = TRUE;
    change.duplicate_source = FALSE;
    change.op               = entry.op;
    change.source           = cicse_table[entry.source].op;
    change.source_idx       = entry.source;
    change.omega            = entry.omega;

    // Have we seen this source OP before?
    INT index_dup;
    for ( index_dup = oppor_index - 2; index_dup >= 0; --index_dup )
      if ( opportunities[index_dup].source == change.source
	   && ! opportunities[index_dup].duplicate_source ) break;

    // Determine TNs to hold the results of the source OP
    if ( index_dup >= 0 ) {  // One is a duplicate
      CICSE_change& change_dup = opportunities[index_dup];

      // Mark the one with the smaller omega as the duplicate
      if ( change.omega > change_dup.omega )
	change_dup.duplicate_source = TRUE;
      else
	change.duplicate_source = TRUE;

      // Always use the same result TNs for the same source OP
      if ( OP_store( change.source ) )
	change.new_tns[0] = change_dup.new_tns[0];
      else
	for ( INT res = OP_results( change.source ) - 1; res >= 0; --res )
	  change.new_tns[res] = change_dup.new_tns[res];

    } else if ( OP_store( change.source ) ) {

      // Look up TN holding value to be stored
      INT opnd = OP_find_opnd_use( change.source, OU_storeval );
      Is_True( opnd >= 0, ( "CIO_RWTRAN::CICSE_Transform can't find"
			    " storeval operand for %s",
			    TOP_Name( OP_code( change.source ) ) ) );
      change.new_tns[0] = OP_opnd( change.source, opnd );

      // All result TNs of read must be initialized using write
      Is_True( OP_results( change.op ) == 1,
	       ( "CIO_RWTRAN:CICSE_Transform: Read has multiple results" ) );

      // For now, don't handle predicated write source

      Is_True( ! OP_has_predicate( change.source )
	       || OP_opnd( change.source, OP_PREDICATE_OPND ) == True_TN,
	       ( "Can't handle predicated write source" ) );

      // If source TN occurs later as a result, then new TN is required
      // NOT ALWAYS NECESSARY!  IMPROVE THIS!
      INT tn_index = hTN_MAP32_Get( tn_last_op, change.new_tns[0] );
#ifdef KEY
      /* fix bug#1989 where the store value is not put in the table. */
      if( tn_index == 0 )
	change.new_tns[0] = Build_TN_Like( change.new_tns[0] );      
      else
#endif // KEY
      if ( OP_Precedes( change.source, cicse_table[tn_index].op ) )
	change.new_tns[0] = Build_TN_Like( change.new_tns[0] );

    } else {

      // read removal with source read, or common subexpression elimination
      for ( INT res = OP_results( change.source ) - 1; res >= 0; --res )
	change.new_tns[res] = OP_result( change.source, res );

      // If source is predicated, new TNs and TN copies are required
      if ( OP_has_predicate( change.source )
	   && OP_opnd( change.source, OP_PREDICATE_OPND ) != True_TN )
	for ( INT res = OP_results( change.source ) - 1; res >= 0; --res )
	  change.new_tns[res] = Build_TN_Like( change.new_tns[res] );

      else
	// If source TN is not last occurance of that TN as a result,
	// then new TN is required
	for ( INT res = OP_results( change.source ) - 1; res >= 0; --res ) {
	  INT tn_index = hTN_MAP32_Get( tn_last_op, change.new_tns[res] );
	  if ( cicse_table[tn_index].op != change.source )
	    change.new_tns[res] = Build_TN_Like( change.new_tns[res] );
	}
    }

    // Determine whether removing op will require inserting TN copies
    // IMPROVE THIS LATER!
    if ( ! OP_has_predicate( change.op )
	 || OP_opnd( change.op, OP_PREDICATE_OPND ) == True_TN ){
      for ( INT res = OP_results( change.op ) - 1; res >= 0; --res ) {
	TN *tn_result = OP_result( change.op, res );
	change.need_copy[res] =
	  ( ! entry.result_unique[res] ||
	    GRA_LIVE_TN_Live_Outof_BB( tn_result, CG_LOOP_epilog ) ||
	    change.new_tns[res] == tn_result /* Fix for pv779607 */ );
#ifdef KEY
	/* A copy is needed if <tn_result> is exposed. */
	if( !change.need_copy[res] ){
	  for( index_dup = oppor_index - 2; index_dup >= 0; index_dup-- ){
	    if( tn_result == opportunities[index_dup].new_tns[res] ){
	      change.need_copy[res] = TRUE;
	      break;
	    }
	  }
	}
#endif
      }
    }

#ifdef KEY
    /* To avoid a body tn in the prolog being re-defined, we need to create a
       new tn, and a copy for it. (bug#358, bug#2912)
    */
    for( int start_omega = 1; start_omega <= change.omega; start_omega++ ){
      for( INT res = OP_results( change.op ) - 1; res >= 0; --res ){
	if( CG_LOOP_Backpatch_Find_Non_Body_TN(CG_LOOP_prolog,
					       change.new_tns[res],
					       start_omega) != NULL ){
	  change.new_tns[res] = Build_TN_Like( change.new_tns[res] );
	  change.need_copy[res] = true;
	}
      }
    }
#endif
  }

  // Display opportunities
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    Trace_CICSE_Changes( opportunities, oppor_count,
			 "During CIO_RWTRAN:CICSE_Transform" );
  }

  // Insert backpatches into loop prolog;
  // Abort if unable to complete backpatches
  for ( index = oppor_count - 1; index >= 0; --index ) {
    CICSE_change& change = opportunities[index];
    if ( change.duplicate_source ) continue;

    if ( ! Backpatch_Op_In_Prolog( change.op, change.new_tns,
				   1, change.omega ) ) {

      // Cancel all optimizations with this source
      for ( INT ch_index = oppor_count; ch_index >= 0; --ch_index )
	if ( opportunities[ch_index].source == change.source )
	  opportunities[ch_index].okay = FALSE;
    }
  }

  // Copy all OPs generated into the prolog
  Copy_Ops_To_Prolog();

  // Now, make modifications to loop body

  // Insert any necessary TN copies for sources
  for ( index = oppor_count - 1; index >= 0; --index ) {
    CICSE_change& change = opportunities[index];
    if ( change.duplicate_source || ! change.okay ) continue;

    if ( OP_store( change.source ) ) {

      // read removal with source write
      // All result TNs of read must be initialized using write
      Is_True( OP_results( change.op ) == 1,
	       ( "CIO_RWTRAN:CICSE_Transform: Read has multiple results" ) );

      // For now, don't handle predicated write source

      // If source TN is not last occurance of that TN as a result,
      // then new TN is required
      INT opnd = OP_find_opnd_use( change.source, OU_storeval );
      TN *tn_old = OP_opnd( change.source, opnd );
      TN *tn_new = change.new_tns[0];
      if ( tn_old != tn_new ) {

	// Insert tn_new <-- tn_old after source
	OP *op_copy = Append_TN_Copy( tn_new, tn_old, change.source,
				      OP_omega( change.source, opnd ) );
	if ( _trace_CG_RTran ) {
	  #pragma mips_frequency_hint NEVER
	  fprintf( TFile, "CIO_RWTRAN::Transform_Arcs inserts:\n\t" );
	  Print_OP_No_SrcLine( op_copy );
	}
      }

    } else {

      // If source is predicated, insert predicated TN copies, and remove
      // source predication
      // ALTERNATIVELY: REPEAT SOURCE OP?
      if ( OP_has_predicate( change.source )
	   && OP_opnd( change.source, OP_PREDICATE_OPND ) != True_TN ) {
	TN *tn_pred = OP_opnd( change.source, OP_PREDICATE_OPND );
	// change all its opnd_src's op be unconditionally
    // recursivly if load can execute unconditionally.
    if(cicse_table[change.source_idx].keep_pred) {
      // do nothing for original op
    }
    else if(OP_load(change.source)) {
      CICSE_Mark_OP_Unconditionally(cicse_table, change.source_idx, 0);

    }
    else {
      Set_OP_opnd( change.source, OP_PREDICATE_OPND, True_TN );
    }
	for ( INT res = OP_results( change.source ) - 1; res >= 0; --res ) {
	  TN *tn_old = OP_result( change.source, res );
	  TN *tn_new = change.new_tns[res];
	  Set_OP_result( change.source, res, tn_new );

	  // Insert (tn_pred) tn_old <-- tn_new after source
	  OP *op_copy = Append_TN_Copy( tn_old, tn_new, change.source, 0 );
	  Set_OP_opnd( op_copy, OP_PREDICATE_OPND, tn_pred );
	  if ( _trace_CG_RTran ) {
	    #pragma mips_frequency_hint NEVER
	    fprintf( TFile, "CIO_RWTRAN::Transform_Arcs inserts:\n\t" );
	    Print_OP_No_SrcLine( op_copy );
	  }
	}

      } else
	// If source TN is not last occurance of that TN as a result,
	// then new TN is required
	for ( INT res = OP_results( change.source ) - 1; res >= 0; --res ) {
	  TN *tn_old = OP_result( change.source, res );
	  TN *tn_new = change.new_tns[res];
	  if ( tn_old == tn_new ) continue;

	  // Insert tn_new <-- tn_old after source
	  OP *op_copy = Append_TN_Copy( tn_new, tn_old, change.source, 0 );
	  if ( _trace_CG_RTran ) {
	    #pragma mips_frequency_hint NEVER
	    fprintf( TFile, "CIO_RWTRAN::Transform_Arcs inserts:\n\t" );
	    Print_OP_No_SrcLine( op_copy );
	  }
	}
    }
  }

  // Eliminate read and common subexpression OPs
  for ( index = oppor_count - 1; index >= 0; --index ) {
    CICSE_change& change = opportunities[index];
    if ( ! change.okay ) continue;

    if ( _trace_CG_RTran ) {
      #pragma mips_frequency_hint NEVER
      fprintf( TFile, "CIO_RWTRAN::CICSE_Transform replaces:\n\t" );
      Print_OP_No_SrcLine( change.op );
      fprintf( TFile, "with:" );
    }

    // read/common subexpression elimination; insert TN copies if necessary
    if ( OP_has_predicate( change.op )
	 && OP_opnd( change.op, OP_PREDICATE_OPND ) != True_TN ) {
      TN *tn_pred = OP_opnd( change.op, OP_PREDICATE_OPND );
      for ( INT res = OP_results( change.op ) - 1; res >= 0; --res ) {
	TN *tn_old = OP_result( change.op, res );
	TN *tn_new = change.new_tns[res];

	// Insert a new copy  (tn_pred) tn_old <-- tn_new[omega]
	OP *op_copy = Append_TN_Copy( tn_old, tn_new,
				      change.op, change.omega );
	Set_OP_opnd( op_copy, OP_PREDICATE_OPND, tn_pred );
	if ( _trace_CG_RTran ) {
	  #pragma mips_frequency_hint NEVER
	  fprintf( TFile, "\t" );
	  Print_OP_No_SrcLine( op_copy );
	}
      }

    } else
      for ( INT res = OP_results( change.op ) - 1; res >= 0; --res ) {
	TN *tn_old = OP_result( change.op, res );
	TN *tn_new = change.new_tns[res];
	if ( change.need_copy[res] ) {
	  // Insert a new copy  tn_old <-- tn_new[omega]
	  OP *op_copy = Append_TN_Copy( tn_old, tn_new,
					change.op, change.omega );
	  if ( _trace_CG_RTran ) {
	    #pragma mips_frequency_hint NEVER
	    fprintf( TFile, "\t" );
	    Print_OP_No_SrcLine( op_copy );
	  }
	} else {
	  // Replace  tn_old  by  tn_new[omega]  in body
	  Replace_Tn( body, tn_old, tn_new, change.omega );
	}
      }
    BB_Remove_Op( body, change.op );
  }

  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "\n" );
  }

  return TRUE;
}


// ======================================================================
//
// Print_Ops  and  Print_Arcs  print the current contents of the
// vectors _op_ordering and _arc_ordering, respectively, into TFile.
//
// ======================================================================


void
CIO_RWTRAN::Print_Ops( const char *message )
{
  Is_True( TFile, ( "CIO_RWTRAN::Print_Ops -- TFile is NULL" ) );
  fprintf( TFile, "<cio> CIO_RWTRAN::Print_Ops -- %s\n", message );
  for ( INT t = 0; t < _ordering_count; ++t ) {
    OP *op = _op_ordering[t];
    if ( op )
      Print_OP_No_SrcLine( op );
    else
      fprintf( TFile, "NULL\n" );
  }
  fprintf( TFile, "\n" );
}


void
CIO_RWTRAN::Print_Arcs( const char *message )
{
  Is_True( TFile, ( "CIO_RWTRAN::Print_Arcs -- TFile is NULL" ) );
  fprintf( TFile, "<cio> CIO_RWTRAN::Print_Arcs -- %s\n", message );
  for ( INT t = 0; t < _ordering_count; ++t ) {
    ARC *arc = _arc_ordering[t];
    if ( arc )
      CG_DEP_Trace_Arc( arc, FALSE, TRUE /*verbose*/ );
    else
      fprintf( TFile, "NULL\n" );
  }
  fprintf( TFile, "\n" );
}


// ======================================================================
//
// Sort_Ops_Preorder, Sort_Ops, and Sort_Arcs
//
// These procedures identify all MEMOUT memory dependence arcs that
// provide potential optimizations through write elimination.  In
// addition, OPs are ordered according to the direction of the
// dependencies.
//
// Sort_Ops_Preorder performs a depth-first search to obtain a preorder
// ordering of the OPs.  Sort_Ops applies Sort_Ops_Preorder to all
// potentially optimizable OPs.  Sort_Arcs finds the ARCs connecting
// adjacent OPs after sorting.
//
// ======================================================================


void
CIO_RWTRAN::Sort_Ops_Preorder( OP *op_current, BOOL current_has_arc )
{
  // Mark OP as visited
  Reset_OP_flag1( op_current );

  // Visit all predecessor ARCs and OPs
  for ( ARC_LIST *arclist = OP_preds( op_current );
	arclist != NULL;
	arclist = ARC_LIST_rest( arclist ) ) {

    // Is this ARC potentially optimizable and its predecessor OP unvisited?
    ARC *arc = ARC_LIST_first( arclist );
    if ( ! Write_Candidate_Arc( arc ) ) continue;
    OP *op_pred = ARC_pred( arc );
    if ( ! OP_flag1( op_pred ) ) continue;

    // Found a potentially optimizable predecessor ARC and OP
    current_has_arc = TRUE;
    Sort_Ops_Preorder( op_pred, TRUE );
  }

  // Finished with predecessors; If op_current is known to be in a
  // potentially optimizable dependence arc, then append it to ordering.
  if ( current_has_arc )
    _op_ordering[_ordering_count++] = op_current;
  else
    // Mark OP as not visited, since it might have a successor
    Set_OP_flag1( op_current );
}


void
CIO_RWTRAN::Sort_Ops( BB *body )
{
  // Identify (set flag1 for) and count all potentially optimizable OPs
  OP *op;
  INT op_count = 0;
  FOR_ALL_BB_OPs_FWD( body, op ) {
    if ( Write_Candidate_Op( op ) ) {
      Set_OP_flag1( op );
      ++op_count;
    } else
      Reset_OP_flag1( op );
  }

  // Allocate _op_ordering and initialize _ordering_count;
  // Abort if no potentially optimizable OPs
  _ordering_count = 0;
  if ( op_count == 0 ) return;
  _op_ordering = (OP **) CXX_NEW_ARRAY( OP *, op_count, _loc_mem_pool );

  // Sort OPs in order of dependence arcs; Skip over ops with no preds;
  // Reset flag1 for OPs when visited; Visit OPs in reverse order to
  // better handle MEMARC loops
  FOR_ALL_BB_OPs_REV( body, op ) {
    if ( ! OP_flag1( op ) ) continue;
    BOOL current_has_arc = FALSE;
    OP *op_current = op;
    ARC_LIST *arclist;
    do {

      // Sort all (potentially optimizable) predecessors of op_current
      Sort_Ops_Preorder( op_current, current_has_arc );

      // Search for potentially optimizable successors of op_current
      for ( arclist = OP_succs( op_current );
	    arclist != NULL;
	    arclist = ARC_LIST_rest( arclist ) ) {

	// Is this arc and successor OP potentially optimizable?
	ARC *arc = ARC_LIST_first( arclist );
	if ( ! Write_Candidate_Arc( arc ) ) continue;
	OP *op_succ = ARC_succ( arc );
	if ( ! OP_flag1( op_succ ) ) continue;

	// Yes, it is; consider the successor OP instead
	op_current = op_succ;
	current_has_arc = TRUE;
	break;
      }
    } while ( arclist != NULL );
  }

  // Display sorted ops
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    Print_Ops( "After CIO_RWTRAN::Sort_Ops" );
  }
}


void
CIO_RWTRAN::Sort_Arcs( void )
{
  // Allocate and initialize _arc_ordering
  if ( _ordering_count == 0 ) return;
  _arc_ordering = (ARC **) CXX_NEW_ARRAY( ARC *, _ordering_count,
					  _loc_mem_pool );

  OP *op_succ = _op_ordering[0];
  INT t;
  for ( t = 1; t < _ordering_count; ++t ) {  // Skip first OP
    OP *op_pred = op_succ;
    op_succ = _op_ordering[t];

    // Look for the correct MEM arc
    _arc_ordering[t - 1] = NULL;
    for ( ARC_LIST *arcs = OP_preds( op_succ );
	  arcs != NULL; arcs = ARC_LIST_rest( arcs ) ) {
      ARC *arc = ARC_LIST_first( arcs );
      if ( ARC_pred( arc ) == op_pred && Write_Candidate_Arc( arc ) ) {
	_arc_ordering[t - 1] = arc;
	break;
      }
    }
  }
  _arc_ordering[_ordering_count - 1] = NULL;

  // The last OP in each connected sequence of ARCs must not be predicated
  BOOL within_chain = FALSE;
  for ( t = _ordering_count - 2; t >= 0; --t ) {
    if ( _arc_ordering[t] == NULL )
      within_chain = FALSE;
    else if ( ! within_chain ) {
      if ( OP_cond_def( _op_ordering[t + 1] ) )
	_arc_ordering[t] = NULL;
      else
	within_chain = TRUE;
    }
  }

  // Display sorted arcs
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    Print_Arcs( "After CIO_RWTRAN::Sort_Arcs" );
  }
}


// ======================================================================
//
// Transform_Arcs applied the write elimination transformation to each of
// the ARCs in _arc_ordering.
//
// ======================================================================


void
CIO_RWTRAN::Transform_Arcs( BB *body )
{
  // Display sorted arcs
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    Print_Arcs( "Before CIO_RWTRAN::Transform_Arcs" );
  }

  // Perform WW elimination
  for ( INT t = 0; t < _ordering_count; ++t ) {
    ARC *arc = _arc_ordering[t];
    if ( arc == NULL ) continue;
    Is_True( ARC_kind( arc ) == CG_DEP_MEMOUT,
	     ( "CIO_RWTRAN::Transform_Arcs found unexpected arc type" ) );
    OP *op_base = ARC_pred( arc );

    // Backpatch loop epilog -- Abort if unable to complete backpatch
    if ( ! Backpatch_Op_In_Epilog( op_base, ARC_omega( arc ) ) )
      continue;

    // Remove the old store  memory <-- tn_base
    if ( _trace_CG_RTran ) {
      #pragma mips_frequency_hint NEVER
      fprintf( TFile, "CIO_RWTRAN::Transform_Arcs removes:\n\t" );
      Print_OP_No_SrcLine( op_base );
    }
    BB_Remove_Op( body, op_base );
  }

  // Copy all OPs generated into the epilog
  Copy_Ops_To_Epilog();
}


// ======================================================================


BOOL
CIO_RWTRAN::Write_Removal( BB *body )
{
  // Until the alternate predication code is ready, only remove writes
  // if the trip count is a known constant value, or if predication is
  // available on this target.
#ifndef KEY
  if ( ! TN_has_value( _trip_count_tn ) && ! CGTARG_Can_Predicate() ) {
#else
  if ( 0 ) {
#endif
    DevWarn( "CIO_RWTRAN::Read_Write_Removal predication not activated" );
    return FALSE;
  }

  // Identify and sort potentially optimizable OPs and ARCs
  Sort_Ops( body );                            // Generate _op_ordering
  if ( _ordering_count == 0 ) return FALSE;    // If none, quit early
  Sort_Arcs();                                 // Generate _arc_ordering

  // Try to avoid "optimizations" that increase the number of reads and writes
  if ( TN_has_value( _trip_count_tn )
       && TN_value( _trip_count_tn ) < MAX_OMEGA ) {
    for ( INT t = _ordering_count - 1; t >= 0; --t ) {
      ARC *arc = _arc_ordering[t];
      if ( arc != NULL && ARC_omega( arc ) >= TN_value( _trip_count_tn ) )
	_arc_ordering[t] = NULL;
    }
  }

  // Are there any available optimizations?
  INT t;
  for ( t = _ordering_count - 1; t >= 0; --t )
    if ( _arc_ordering[t] != NULL ) break;
  if ( t < 0 ) return FALSE;

  // Preform the optimizations
  Transform_Arcs( body );

  // Memory pools make array deletion unnecessary
  // CXX_DELETE_ARRAY( _op_ordering, _loc_mem_pool );
  // CXX_DELETE_ARRAY( _arc_ordering, _loc_mem_pool );
  _op_ordering = NULL;
  _arc_ordering = NULL;
  return TRUE;
}


// ======================================================================
//
// Read_CICSE_Write_Removal is the driver for read/cicse/write elimination
// analysis and transformations.  Returns TRUE iff changes are made to
// *loop.
//
// ======================================================================


BOOL
CIO_RWTRAN::Read_CICSE_Write_Removal( LOOP_DESCR *loop )
{
  BB *body = LOOP_DESCR_loophead( loop );
  BB *head = LOOP_DESCR_loophead( loop );

  // TEMPORARY: Vector r/w removal can't deal with "asymmetrical" memory
  // dependence arcs that result from CG's non-cyclic address analysis.
  // Once it can deal with this, or we make CG's address analysis detect
  // cyclic dependences also, this (and the corresponding restore of
  // CG_DEP_Addr_Analysis below) can be removed.

  BOOL save_CG_DEP_Addr_Analysis = CG_DEP_Addr_Analysis;
  CG_DEP_Addr_Analysis = FALSE;

  // Note our activity:
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "\n<cio> %s<cio> Read/CICSE/Write Removal:\n<cio> %s\n",
	     SBar, SBar );
    CG_LOOP_Trace_Loop( loop, "<cio> Before CIO Copy Propagation" );
  }

  // Preform CIO copy propagation
  BOOL changed_loop_copy = FALSE;
  if ( CIO_enable_copy_removal )
    changed_loop_copy = CIO_Copy_Remove( body );

  // Derive dependence graph for loop body
  CG_DEP_Compute_Graph( head, NO_ASSIGNED_REG_DEPS, CYCLIC,
			INCLUDE_MEMREAD_ARCS, INCLUDE_MEMIN_ARCS,
			NO_CONTROL_ARCS, NULL );

  // Note our activity:
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    if ( changed_loop_copy ) {
      CG_LOOP_Trace_Loop( loop, "<cio> Between Copy Propagation"
			  " and Read/CICSE Removal" );
    } else {
      fprintf( TFile, "<cio> Between Copy Propagation"
	       " and Read/CICSE Removal: No Change\n\n" );
    }
    CG_DEP_Trace_Graph( body );
  }

  // Preform CIO read and CICSE removal
  BOOL changed_loop_read = FALSE;
  if ( CIO_enable_read_removal )
    changed_loop_read = CICSE_Transform( body );

  // Recompute dependence graph for loop body
  if ( changed_loop_read ) {
    CG_DEP_Delete_Graph( head );
    CG_DEP_Compute_Graph( head, NO_ASSIGNED_REG_DEPS, CYCLIC,
			  NO_MEMREAD_ARCS, INCLUDE_MEMIN_ARCS,
			  NO_CONTROL_ARCS, NULL );
  }

  // Note our activity:
  if ( _trace_CG_RTran ) {
    #pragma mips_frequency_hint NEVER
    if ( changed_loop_read ) {
      CG_LOOP_Trace_Loop( loop, "<cio> Between Read/CICSE and Write Removal" );
      CG_DEP_Trace_Graph( body );
    } else
      fprintf( TFile,
	       "<cio> Between Read/CICSE and Write Removal: No Change\n\n" );
  }

  // Perform Write Removal
  BOOL changed_loop_write = FALSE;
  if ( CIO_enable_write_removal )
    changed_loop_write = Write_Removal( body );

  // Note our activity:
  if ( _trace_CG_RTran || _trace_CG_Chkpnt ) {
    #pragma mips_frequency_hint NEVER
    if ( changed_loop_write )
      CG_LOOP_Trace_Loop( loop, "<cio> After Write Removal\n" );
    else
      fprintf( TFile, "<cio> After Write Removal: No Change\n" );
    fprintf( TFile, "\n" );
  }

  CG_DEP_Addr_Analysis = save_CG_DEP_Addr_Analysis;

  CG_DEP_Delete_Graph( head );

  return ( changed_loop_read || changed_loop_write || changed_loop_copy );
}


// ======================================================================
// ======================================================================
//
// Cross-iteration Read, CSE, and Write Elimination
//
// Perform_Read_Write_Removal  allocates a local memory pool and creates
// and invokes a CIO_RWTRAN class object to perform read/cicse/write
// elimination on loop.
//
// ======================================================================
// ======================================================================


BOOL Perform_Read_Write_Removal( LOOP_DESCR *loop )
{
  if ( ! CIO_enable_copy_removal &&
       ! CIO_enable_read_removal && ! CIO_enable_write_removal )
    return FALSE;

  // Initialize memory pool for CIO_RWTRAN
  MEM_POOL local_mem_pool;
  MEM_POOL_Initialize( &local_mem_pool, "CIO_RWTRAN local pool", FALSE );
  MEM_POOL_Push( &local_mem_pool );

  BOOL changed_loop = FALSE;
  {
    // Invoke Read/CSE/Write Removal
    CIO_RWTRAN cio_rwtran( loop, &local_mem_pool );
    changed_loop = cio_rwtran.Read_CICSE_Write_Removal( loop );
  }

  // Dispose memory pool for CIO_RWTRAN
  MEM_POOL_Pop( &local_mem_pool );
  MEM_POOL_Delete( &local_mem_pool );

  if ( changed_loop )
    CG_LOOP_Recompute_Liveness( loop );

  return changed_loop;
}


// ======================================================================
