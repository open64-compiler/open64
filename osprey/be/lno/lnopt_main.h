/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
//
// The main internal include file for LNO
//
//
// Exported types and functions:
//
//    EST_REGISTER_USAGE
//
//      BOOL Fits() const
//      BOOL Does_Not_Fit() const
//
//              Both are FALSE when we are not
//              sure either way, otherwise exactly one is true.
//
//      void Set_Fits(BOOL)
//      void Set_Does_Not_Fit(BOOL)
//
//              Only when there is no register estimate, but somehow you
//              know.  Sets Est_Regs() to -1, sets Fits() or Does_Not_Fit()
//              to the passed in value and sets the other to FALSE.
//
//      void Set_Est_Regs(INT fp_est, INT fp_regs_available)
//		          INT int_est, INT int_regs_available,
//			  INT tlb_est, INT tlb_available)
//      INT Est_Fp_Regs() const
//      INT Est_Int_Regs() const
//      INT Est_TLB() const
//
//              An estimate.  If all parameters are >= 0, then we set
//              one of the bits above to TRUE (fits iff est <= regs_available).
//              Est_Fp_Regs() is the most recent value of est, or -1 if unknown.
//
//       EST_REGISTER_USAGE(EST_REGISTER_USAGE)
//       void operator = (EST_REGISTER_USAGE)
//
//              What you'd expect.  The constructor sets to no info, Est_Regs()
//              to -1.
//
//    DO_LOOP_INFO
//
//		Hold information about DO loops
//
//	ACCESS_ARRAY *LB
//
//		The lower bound is the max of all the access vectors in LB
//
//	ACCESS_ARRAY *UB
//
//		The upper bound is the min of all the access vectors in UB
//
//	ACCESS_VECTOR *Step
//
//		The Step
//
//      mBOOL Is_Inner
//
//		Are there any DO loops inside this loop
//
//	mBOOL Has_Calls
//
//		Any calls inside this do loop
//
//      mBOOL Has_Nested_Calls
//              
//              Any calls to nested functions inside this do loop
//
//	mBOOL Has_Unsummarized_Calls
//
//		Any calls without IPA summary info inside this do loop
//
//	mBOOL Has_Unsummarized_Call_Cost
//
//		Any calls without IPA summary cost info inside this do loop
//
//	mBOOL Has_Threadprivate 
//
//		Has a THREADPRIVATE variable (these inhibit automatic 
//		parallelization)
//
//	mBOOL Has_Gotos
//
//		Any non-structured control flow, while loops or computed
//		gotos inside this do loop.
//
//	mBOOL Has_Conditional
//
//		Any Gotos jumps to a label inside the loop body.
//		If there is such goto, the loop cannot be unrolled,
//		because unrolling will introduce duplicated label. 
//
//	mBOOL Has_Exits
//
//		Any gotos/returns leaving the loop
//
//	mBOOL Has_EH_Regions
//
//		Has EH regions in the loop
//
//	mBOOL Has_Gotos_This_Level
//
//		Any gotos jump to a label in this DO loop but not
//		nested in a deeper DO loop (this is important because
//		if no and if no exits then any write at this level is a must write
//
//	mBOOL Has_Bad_Mem
//
//		Does this have any non-mapped loads or stores or calls.  
//		This may be set arbitrarily if Has_Calls or Has_Exits is set.
//
//  	mBOOL Has Barriers 
//
//		Has either a FOIRWARD_BARRIER or a BACKWARD_BARRIER.  Right
//		now this is equivalent to 'Has_Bad_Mem'.
// 
//	mBOOL Is_Ivdep
//
//		Was ivdep set for this loop.  Currently, ivdep is only
//		recognised for inner loops.
//
//      mBOOL Is_Concurrent_Call;
//
//		Is there a concurrent call directive on this loop
//		or any outermore loop.  If so, ignore dependences
//		caused by a call
//
//	mBOOL Concurrent_Directive
//
//		Is there a concurrent directive on this loop.
//		If true, ignore "non-obvious" dependences
//
//	mBOOL No_Fission
//
//		Fission is not allowed for this loop. Default is FALSE.
//
//	mBOOL No_Fusion
//
//		Fusion is not allowed for this loop. Default is FALSE.
//
//	mBOOL Aggressive_Inner_Fission
//
//		Fission inner loop as fine as possible. Default is FALSE.
//
//      mBOOL Cannot_Interchange
//
//              NO INTERCHANGE pragma seen for this loop.  Default is FALSE.
//
//      mBOOL Cannot_Block
//
//              NO BLOCKING pragma seen for this loop.  Default is FALSE.
//
//	mBOOL Pragma_Cannot_Concurrentize 
//
//		NO CONCURRENTIZE pragma seen for this loop. Default is FALSE
//
//	mBOOL Pragma_Prefer_Concurrentize 
//
//		PREFER CONCURRENTIZE pragma seen for this loop. 
//              Default is FALSE
//
//	mBOOL Auto_Parallelized 
//
//		Automatically parallelized this loop 
//
//	mBOOL Serial_Version_of_Concurrent_Loop
//
//		Loop is in serial version of concurrent loop 
//
//	mBOOL Is_Inner_Tile 
// 		
//		The block is an inner tile.  Default is FALSE.  (May not 
//              be marked on every inner tile, but will be for those created
//              during cache and scalar expansion tiling in the SNL phase
//              and scalar expansion tiling in other phases.) 
//
//	mBOOL Is_Outer_Tile 
//
// 		The block is an outer tile.  Default is FALSE. 
//
//      mBOOL Is_Backward 
// 
//    		TRUE if every use of the loop index has a proveably negative 
//		coefficent, FALSE otherwise. 
// 		 
//	mBOOL Is_Outer_Lego_Tile
//   
//		TRUE if this loop is an outer tile created in lego tiling. 
//
//	mBOOL Is_Inner_Lego_Tile
//   
//		TRUE if this loop is an inner tile created in lego tiling. 
//
//	mBOOL Is_Processor_Tile 
//
//		TRUE if this loop is a processor tile loop. 
//
//	mBOOL Is_Doacross;
//
//		TRUE if this loop is a true doacross loop (with sync)
//
//	mBOOL Suggested_Parallel
//
//		TRUE if we desire this loop to be a parallel loop 
//
//	mBOOL Parallelizable 
//
//		TRUE if the loop is parallelizable in some position within
//		the given SNL. 
//
//	mBOOL Last_Value_Peeled 
//
//		TRUE if the loop was created by peeling the last iteration 
//		of a parallel loop
//
//	mBOOL Not_Enough_Parallel_Work 
//
//		TRUE if loop was not parallelized because there was not 
//		enough estimated work to make it worthwhile to go parallel
//
//      mBOOL Inside_Critical_Section
//
//              TRUE if the loop is inside a critical section (and therefore
//              cannot be parallelized
//
//	double Work_Estimate
//
//		Work estimate of one iteration of the innermost loop 
//		(used to build if-test under -LNO:ap=1) 
//
//      mINT8 Required_Unroll
//
//              UNROLL pragma seen for this loop.  Default is 0.  Value, if
//              positive, is how many times we must outer unroll this loop.
//
//      mINT32 Required_Blocksize[MHD_MAX_LEVELS]
//
//              BLOCKING SIZE pragma seen for this loop.  Default is {-1,-1,...}
//              The blocksize for each level of the cache.  0 means put in
//              the tile but don't block.  It will not result in any illegal
//              transformations.
//
//	mINT32 Tile_Size 
//
//		If Is_Inner_Tile is set, this is the size at which the loop 
//		has been tiled.  
//
//      INT Blockable_Specification
//
//              0 if unspecified.  2 or more are interesting values.  This
//              and the Blockable_Specification-1 loops directly nested
//              inside can be blocked.  If a positive value is specified here,
//              the transformation routines *may* choose to consider the
//              nest blockable even if it thinks dependences prevent it.
//
//      INT* Permutation_Spec_Array
//
//              If non-NULL, indicates a permutation requested for this loop.
//              E.g. Permutation_Spec_Array[0] = 1,
//                   Permutation_Spec_Array[1] = 0
//              says to interchange this loop and the one next inside.
//              The compiler may do this.  It also may refuse to do it,
//              especially if that transformation is illegal.  Note that
//                   Permutation_Spec_Array[0] = x
//              says that the new outer loop (0) is the old x.
//
//	mINT8 Permutation_Spec_Count 
//
//		The number of elements in the array Permutation_Spec_Array.
//		i.e. Permutation_Spec_Array[0..Permutation_Spec_Count-1].
// 
//	INT64 Est_Num_Iterations
//
//		An estimate for how many iterations in the loop.
//		Currently, any loop with symbolic bounds or a non-const step
//		is estimated to have LNO_Num_Iters=100 iterations.
//		Initialized to -1.
//
//	INT64 Est_Max_Iterations_Index
//
//		From the sizes of each index (e.g. a(i) where a(1..63),
//              an estimate of the maximum iterations possible.
//		Initialized to -1.  This is set by Mark_Code(), and
//              *not* set by Set_Est_Num_Iterations.  This is for
//              efficiency.  Perhaps this should change.
//
//	mBOOL Num_Iterations_Symbolic
//
//		Did the estimate for Est_Num_Iterations rely on LNO_Num_Iters
//
//	void Set_Est_Num_Iterations(DOLOOP_STACK *do_stack)
//
//		Set the estimate given that the stack contains the 
//		DO_LOOP_INFOs with ACCESS_ARRAYs for the outer loops
//
//	void Set_Est_Num_Iterations(DOLOOP_STACK *do_stack)
//
//		Set the estimate given that the stack contains the 
//		DO_LOOP_INFOs with ACCESS_ARRAYs for the outer loops
//
//	mUINT8 Depth
//
//		What is the "depth" of this DO loop.  The outer DO loop
//		(good or bad) is numbered 0.  The next inner 1, etc.
//
//	WN *Guard
//
//		Points to the if statement that "guards" this loop.
//		By guard we mean guarantee that the do loop is not
//		zero trip count.  If this is NULL, then the loop
//		is non-zero trip count without any special guard.
//
//      LEGO_INFO* Lego_Info
//	
//		Pointer to information needed for lego transformations. 
//
//	MP_INFO* Mp_Info
//
//		Pointer to information needed for MP transformations. 
//
//  	INT Lego_Mp_Key_Lower
//
//		Lower bound of range of Lego_Mp_Key values for this loop  
//
//  	INT Lego_Mp_Key_Upper
//
//		Upper bound of range of Lego_Mp_Key values for this loop 
//
// 	INT Lego_Mp_Key_Depth 
//
//		Depth of this loop in the tile group with keys from 
//		Lego_Mp_Key_Lower to Lego_Mp_Key_Upper, inclusive. 
//
//	EST_REGISTER_USAGE Est_Register_Usage
//
//		An estimation of the register usage from the model.
//              This only makes sense for an inner loop.
//
//      DO_LOOP_INFO(MEM_POOL *pool, ACCESS_ARRAY *lb, ACCESS_ARRAY *ub,
//	  ACCESS_VECTOR *step, BOOL has_calls, BOOL has_unsummarized_calls,
//        BOOL has unsummarized_call_cost, BOOL has_gotos, BOOL has_exits, 
//        BOOL has_goto_this_level, BOOL is_inner) 
//
//
//      DO_LOOP_INFO(DO_LOOP_INFO *dli, MEM_POOL *pool)
//
//		Copy the info
//
//      MEM_POOL *Pool()
//
//		Which pool was used for this info
//
//	void Print(FILE *fp)
//
//		Print out the info
//
//	BOOL Is_Cache_Winddown() const
//	void Set_Cache_Winddown(BOOL b = TRUE)
//	BOOL Is_Register_Winddown() const
//	void Set_Register_Winddown(BOOL b = TRUE)
//	BOOL Is_In_Cache_Winddown() const
//	void Set_In_Cache_Winddown(BOOL b = TRUE)
//	BOOL Is_In_Register_Winddown() const
//	void Set_In_Register_Winddown(BOOL b = TRUE)
//	BOOL Is_Generally_Unimportant() const
//	void Set_Generally_Unimportant(BOOL b = TRUE)
//
//		Set/Get flags for DO_LOOP_INFO.  If the optional parameter
//		on the Set is FALSE, then reset the bit.  The first four
//		indicate whether the loop is the result of winding down
//		from cache blocking or register blocking.  The next four
//		are for loops inside cache windown or register winddown loops.
//		[Note that the setting if Is_Cache_Winddown does not imply
//		the setting if Is_In_Cache_Winddown, the latter saying that
//		a loop *further out* is a cache winddown; likewise for
//		registers].  The next four indicate whether (according to
//		our attempts to model the loop), the loop fits
//		in the registers, doesn't or (if neither is set)
//		not sure.  Both cannot be set.  Currently, only an inner
//		loop may have one of these set.
//
//	INT Doacross_Tile_Size
//
//		Size of the doacross tile size.
//
//	INT Sync_Distances[2]
//
//		Two synchronization vectors for doacross:
//			(Sync_Distances[0],-1), (Sync_Distances[1],1)
//
//	INT Doacross_Overhead
//
//		Overhead of the doacross. I.e., init delay and sync
//		overhead.
//
//  DO_LOOP_INFO* Get_Do_Loop_Info(WN* wn, BOOL ok_if_none = FALSE)
//
//		Get the annotation for this do loop.  Fails if ok_if_none
//		is FALSE and there is no annotation.
//
//  IF_INFO* Get_If_Info(WN* wn, BOOL ok_if_none = FALSE)
//
//		Get the annotation for this if.  Fails if ok_if_none
//		is FALSE and there is no annotation.
//
//  void Set_Do_Loop_Info(WN* wn, DO_LOOP_INFO* dli)
//
//		Set the loop to have this annotation.
//
//  BOOL Do_Loop_Is_Good (WN *wn)
//
//		Is this a good do loop?  Currently, a DO loop is not good 
//		if it has any unmapped loads/stores/calls
//		If a DO loop is not good.  All its surrounding DO loops must
//		be not good as well
//
//  BOOL Do_Loop_Has_Calls (WN *wn)
//
//		Does this do loop have calls
//
//  BOOL Do_Loop_Has_Gotos (WN *wn)
//
//		Does this do loop have gotos
//
//  BOOL Do_Loop_Has_Gotos_This_Level (WN *wn)
//
//		Does this do loop have gotos that jump to this level
//
//  BOOL Do_Loop_Has_Unsummarized_Calls (WN *wn)
//
//		Does this do loop have calls that are not summarized by IPA
//
//  BOOL Do_Loop_Has_Unsummarized_Call_Cost (WN *wn)
//
//		Does this do loop have calls whose cost is not summarized 
//		by IPA
//
//  BOOL Do_Loop_Is_Mp (WN *wn)
//
//		Is this an mp DO loop?
//
//  BOOL Do_Loop_Is_Inner (WN *wn)
//
//		Is this an inner DO loop?
//
//  BOOL Do_Loop_Is_Ivdep (WN *wn)
//
//		Is this an ivdep loop
//
//  INT Do_Loop_Depth(WN *wn)
//
//  BOOL Mark_Code(WN *func_nd, BOOL promote_pointers=FALSE,
//			BOOL strict_limit=TRUE)
//
//		Mark all the do loops and IFs in the function 
//		using the DO_LOOP_INFO data structure and the LNO_Info_Map map.
//	        if promote_pointers, try to promote pointers into arrays
//		  note that promote_pointers doesn't set up access_vectors
//		  for the generated arrays
//		if 'strict_limit', use a strict limit on the number of loops
//   		  allowed in a nest (i.e. LNO_MAX_DO_LOOP_DEPTH_STRICT). 
//		  Otherwise, use a more liberal limit (i.e. 
//		  LNO_MAX_DO_LOOP_DEPTH-1 = 32)
//		return TRUE if there exists a do loop in the code
//
//  BOOL Build_Array_Dependence_Graph (WN* func_nd)
//
//      Called with the WHIRL tree for the function to build the
//      array dependence graph. Return TRUE if graph was OK, FALSE
//      otherwise.
//
//  void Build_CG_Dependence_Graph (WN* func_nd)
//      
//      Build CG dependence graph from scratch.
//
//  void Build_CG_Dependence_Graph (ARRAY_DIRECTED_GRAPH16*)
//      
//      Build CG dependence graph using LNO array dependence graph.
//
// Exported Global Variables:
//
//    LNO_MAX_DO_LOOP_DEPTH=32
//
//		The maximum depth of a DO loop.  Used by SNL as a data
//		structure limiation.
//
//    LNO_MAX_DO_LOOP_DEPTH_STRICT: default=10
//		
//		A stricter limit that can be set on the command line.
//		Anything deeper will be dismantled by CAN
//
//    WN_MAP Parent_Map
//
//		Map each node to its parent
//
//
//    WN_MAP LNO_Info_Map
//
//		Map each DO_LOOP to a DO_LOOP_INFO *
//		Map each array statement to an ACCESS_ARRAY *
//
//    WN_MAP Array_Dependence_Map
//
//		Map each array statement to a vertex in the array dependence
//		graph.
//
//    MEM_POOL LNO_default_pool, LNO_local_pool
//
//		The phase and local pools used by LNO.
//		Neither of these pools is "zeroed"
//
//    FILE *STDOUT 
//		
//		Set to stdout.  Can be used by dbx.
//
//    class REGION_INFO 
//
// 	Summarizes important information about a parallel region. 
//
//	BOOL Auto_Parallelized()
//
//	  Was this region generated by auto-parallelization. 
//
//	REGION_INFO()
//
//	  Default constructor. 
//  
//	REGION_INFO(BOOL auto_parallelized)
//
//	  Constructor.  Pass 'auto_parallelized' == TRUE if region was 
//        constructed by auto-parallelization, FALSE otherwise.        
//
// 	~REGION_INFO()
//
//	  Destructor 
//
//	void Print(FILE* fp)
//
//	  Print human readable version of REGION_INFO contents to 
//	  FILE* 'fp'.  
//      
//      REGION_INFO* Get_Region_Info(WN* wn)
//
//	  Returns pointer to region info, if there is one, otherwise, 
//        returns NULL. 
//
// Tracing flags
//
//	All of lno's tracing flags are associated with TP_LNOPT = 31
//	The individual bits are used as follows
//
//  TT_LNO_DEP		        0x0001	base dependence analysis trace
//  TT_LNO_DEP2	        	0x0002	detailed dependence analysis trace
//  TT_LNO_VERBOSE	        0x0004  print useful LNO info to stdout
//  TT_LNO_PREFETCH_VERBOSE     0x0008  print "useful" prefetch info to stdout
//
//  TT_LNO_SKIP_FIZ_FUSE        0x0010	skip fiz_fuse phase
//  TT_LNO_SKIP_PH2     	0x0020	skip phase 2
//  TT_LNO_SKIP_INNER_FISSION  	0x0040	skip inner_fission phase
//  TT_LNO_SKIP_LNO	        0x0080	skip lno entirely, run only pre-opt
//
//  TT_LNO_SNL_DEBUG1	        0x0100	debugging information for SNL
//  TT_LNO_SNL_DEBUG2   	0x0200	extra debugging information for SNL
//  TT_LNO_SNL_INTERACT		0x0400  interactive transformation selection
//                                      (untested, certainly broken)
//  TT_LNO_SE_MALLOC            0x0800  use the stack for scalar expansion
//                                      SHOULD USE -LNO:use_malloc INSTEAD
//
//  TT_LNO_PREFETCH        	0x1000  Enable prefetching.
//  TT_LNO_PREFETCH_DEBUG	0x2000  Dump prefetch debug info to trace file
//  TT_LNO_MODEL		0x4000  debug the machine model
//  TT_LNO_CACHE_MODEL_DEBUG	0x8000  debug the cache model
//
//  TT_LNO_AEQUIV		0x10000 debug equivalence local arrays
//  TT_LNO_SCLRZE		0x20000 debug scalarize local arrays
//  TT_LNO_DEAD		        0x40000 debug dead code cf elimination
//  TT_LNO_GUARD		0x80000 turns off guarding of DO loops
//                                      and memory invariant removal
//
//  TT_LNO_MINVARIANT_DEBUG	0x100000 debug memory invariant removal
//  TT_LNO_NORENAME		0x200000 turns off scalar renaming
//  TT_LNO_SKIP_GS		0x400000 turns off gather/scatter
//
//  TT_LNO_SKIP_NONLIN		0x1000000 turn off NONLINEAR analysis
//  TT_LNO_DEBUG_DELIN		0x2000000 debug delinearization
//  TT_LNO_BIG_SCALAR_TILES	0x4000000 use arbitrarily large scalar 
//					  expansion tiles 
//  TT_GRAPH_CAPACITY		0x8000000 set the graph capacity to 
//					  given value 
//  TT_LNO_DEBUG_CSE		0x10000000  debug inter-iteration cses
//
// 
// The following tracing flags are associated with TP_LNOPT2,
// the next set of LNO options. This class TP_LNOPT2 is 
// tentatively being used for LEGO options.  
// (If you need more, there is yet another LNO class, TT_LNOPT = 33.)
//
//  TT_LEGO_VERBOSE             0x00000001 verbose mode
//  TT_LEGO_DEBUG               0x00000002 debugging dump mode
//  TT_LEGO_PRAGMAS_ONLY        0x00000004 stop lego processing after
//                                         processing the pragmas only
//  TT_LEGO_DISABLE_DIVMOD      0x00000008 disable div/mod optimizations
//  TT_LEGO_DISABLE_HOIST       0x00000010 disable hoisting
//  TT_LEGO_DISABLE_IMPLICIT_AFFINITY    0x00000020 disable implicit affinity
//  TT_LEGO_DISABLE_EXPLICIT_AFFINITY    0x00000040 disable explicit affinity
//  TT_LEGO_DISABLE_INTERCHANGE 0x00000080 disable lego interchange
//  TT_LNO_RUN_ARA              0x00000100 turns on ARA
//  TT_LNO_ARA_VERBOSE          0x00000200 turns on ARA verbose mode
//  TT_LNO_ARA_DEBUG            0x00000400 turns on ARA debug
//  TT_LNO_NO_AUTO_PARALLEL	0x00000800 turns on off autoparallelization 
//					     with interchange 
//
//  TT_LEGO_DISABLE_RR_MAPS     0x00001000 disable using maps for lowering
//                                         of reshaped arrays
//  TT_LEGO_DISABLE_FP_DIVMOD   0x00002000 disable using FP arithmetic for
//                                         INT32 div/mod/rem operations
//  TT_LNO_PARALLEL_DEBUG	0x00004000 debug parallelization
//  TT_LNO_NO_TRANSPOSE	        0x00008000 no transpose optimization  
//  [unused]                    0x00010000 (was TT_LNO_DEBUG_PROMPF)
//  TT_LNO_DISABLE_SEFIN	0x00020000 disable finalization of scalar 
//					     variables 
//  TT_LNO_NO_PAD 		0x00040000 inhibit automatic padding of
//                                         arrays that have power-of-2 sizes
//  TT_LNO_OMP_TRANSFORMS 	0x00080000 describe Whirl transformations
//					   performed by OMP_Prelower()
//  TT_HMB_FORCE_VERSIONS	0x00100000 use multiple-version algorithm
//					   for array bounds hoisting even
//					   for single-version case

/* ====================================================================
 * ====================================================================
 *
 * Module: lnopt_main.h
 * $Revision: 1.13 $
 * $Date: 05/06/20 21:34:05-07:00 $
 * $Author: fchow@fluorspar.internal.keyresearch.com $
 * $Source: be/lno/SCCS/s.lnopt_main.h $
 *
 * Revision history:
 *  27-NOV-94 - Original Version
 *
 * Description:
 *
 * The internal interface for the loop optimizer.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef lnopt_main_INCLUDED
#define lnopt_main_INCLUDED

#ifndef wn_INCLUDED
#include "wn.h"
#endif
#ifndef mempool_INCLUDED
#include "mempool.h"
#endif
#ifndef cxx_memory_INCLUDED
#include "cxx_memory.h"
#endif
#ifndef config_cache_INCLUDED
#include "config_cache.h"
#endif
#ifndef config_lno_INCLUDED
#include "config_lno.h"
#endif
#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif
#ifndef access_main_INCLUDED
#include "access_main.h"
#endif
#ifndef tracing_INCLUDED
#include "tracing.h"
#endif
#ifndef if_info_INCLUDED
#include "if_info.h"
#endif

#include "dep_graph.h"

#include "cxx_hash.h"

#ifdef KEY // bug 7422
#define LNO_MAX_DO_LOOP_DEPTH 64
#else
#define LNO_MAX_DO_LOOP_DEPTH 32
#endif
extern WN_MAP Parent_Map;  /* contains the mapping for the */
			   /* parent pointers for all nodes */
extern WN_MAP LNO_Info_Map;
extern WN_MAP Array_Dependence_Map;
extern WN_MAP LNO_Precom_Map;
// the map to keep track of deleted loop (because of unroll etc.)
extern HASH_TABLE<WN*, BOOL> *Deleted_Loop_Map;
extern MEM_POOL LNO_default_pool;
extern MEM_POOL LNO_local_pool;
extern INT snl_debug; 

extern BOOL Run_Snl;
extern BOOL Contains_MP;  // does the PU contain any MP constructs
extern BOOL Do_Aggressive_Fuse;

extern FILE *STDOUT;

extern BOOL LNO_Debug_Delinearization;
extern BOOL LNO_Allow_Nonlinear;
extern BOOL LNO_Allow_Delinearize; 
extern BOOL LNO_Tlog;
extern FILE* LNO_Analysis;

// The following is useful since LNO might be called to do lego
// processing without optimizations. This variable is TRUE by
// default, off if lno is disabled for some reason.
// (see lnopt_main.cxx for conditions when it gets set to FALSE).
extern BOOL LNO_enabled;

extern WN* Current_Func_Node; 

extern class DU_MANAGER *Du_Mgr;  // manage the def-use chains
extern class ALIAS_MANAGER *Alias_Mgr;  // manage alias info
extern class REDUCTION_MANAGER *red_manager; // manage redution information
extern class ARRAY_DIRECTED_GRAPH16 *Array_Dependence_Graph;  // LNO dep graph
typedef STACK<ST *> STACK_OF_ST;

// Permutation arrays
class PERMUTATION_DESCRIPTOR {
public:
  ST *_st;  // the array
  BOOL _is_good; // is it good in the region of interest (not written)
  PERMUTATION_DESCRIPTOR(ST *st) { _st = st; _is_good = FALSE; }
};

typedef STACK<PERMUTATION_DESCRIPTOR> PERMUTATION_ARRAYS;

extern PERMUTATION_ARRAYS *Permutation_Arrays;

class ACCESS_ARRAY;
class ACCESS_VECTOR;
class LEGO_INFO;
class ARA_LOOP_INFO;
class MP_INFO; 

class EST_REGISTER_USAGE {
  mINT16        _fp_est;
  mINT16        _int_est;
  mINT16        _tlb_est;
  mBOOL         _fits;
  mBOOL         _no_fit;
 public:
  EST_REGISTER_USAGE() : _fp_est(-1), _int_est(-1), _tlb_est(-1),
			 _fits(FALSE), _no_fit(FALSE) {}
  void          operator = (EST_REGISTER_USAGE r) {
    _fp_est = r._fp_est; _int_est=r._int_est; _tlb_est=r._tlb_est;
    _fits = r._fits; _no_fit = r._no_fit;
  }
  BOOL          Fits() const {return _fits;}
  BOOL          Does_Not_Fit() const {return _no_fit;}
  INT           Est_Fp_Regs() const {return _fp_est;}
  INT           Est_Int_Regs() const {return _int_est;}
  INT           Est_TLB() const {return _tlb_est;}
  void          Set_Est_Regs(INT fp_est, INT fp_regs_available,
  			     INT int_est, INT int_regs_available,
			     INT tlb_est, INT tlb_available);
  void          Set_Fits(BOOL f)
    {_fp_est = -1; _int_est = -1; _tlb_est = -1; _fits = f; _no_fit = FALSE;}
  void          Set_Does_Not_Fit(BOOL f)
    {_fp_est = -1; _int_est = -1; _tlb_est = -1; _fits = FALSE; _no_fit = f;}
  void          Print(FILE* f);
};

// Until 7.3, when we create a copy constructor for the ARA_LOOP_INFO,
// Use this mechanism to indicate that it's OK to copy a DO_LOOP_INFO
// with an ARA_LOOP_INFO, because we are going to discard the ARA in-
// formation soon.  Right now, this should only happen if we are doing 
// last value peeling. 

extern BOOL Last_Value_Peeling();
extern void Last_Value_Peeling_On(); 
extern void Last_Value_Peeling_Off(); 

class DO_LOOP_INFO {
private:
  MEM_POOL *_pool;
  IDTYPE  _id;  //  a unique ID for debugging purpose.
  mUINT8 _wind_down_flags;
  enum DLI_FLAG {CWD = 1, RWD = 2, ICWD = 4, IRWD = 8, UNIMPORTANT = 16};
  void Set_Flag(DLI_FLAG flag, BOOL set) {
    if (set)
      _wind_down_flags |= flag;
    else
      _wind_down_flags &= ~flag;
  }
  BOOL Get_Flag(DLI_FLAG flag) const {
    return (_wind_down_flags & flag) != 0;
  }

public:
  ACCESS_ARRAY *LB;
  ACCESS_ARRAY *UB;
  ACCESS_VECTOR *Step;

  INT64 Est_Num_Iterations;
  INT64 Est_Max_Iterations_Index;
  mBOOL Num_Iterations_Symbolic;
  mBOOL Num_Iterations_Profile;
  void Set_Est_Num_Iterations(DOLOOP_STACK *do_stack);

  mBOOL Has_Calls;
#ifdef KEY //bug 14284
  mBOOL Has_Nested_Calls;
#endif
  mBOOL Has_Unsummarized_Calls;
  mBOOL Has_Unsummarized_Call_Cost; 
  mBOOL Has_Threadprivate; 
  mBOOL Has_Gotos;
  mBOOL Has_Conditional;
  mBOOL Has_Gotos_This_Level;
  mBOOL Has_Exits;
  mBOOL Has_EH_Regions;
  mBOOL Is_Inner;
  mBOOL Has_Bad_Mem;
  mBOOL Is_Ivdep; 
  mBOOL Is_Concurrent_Call;
  mBOOL Concurrent_Directive;
  mBOOL No_Fission; 
  mBOOL No_Fusion; 
  mBOOL Aggressive_Inner_Fission; 
  mBOOL Cannot_Interchange;
  mBOOL Cannot_Block;
  mBOOL Pragma_Cannot_Concurrentize; 
  mBOOL Pragma_Prefer_Concurrentize; 
  mBOOL Serial_Version_of_Concurrent_Loop; 
  mBOOL Auto_Parallelized; 
  mBOOL Is_Inner_Tile; 
  mBOOL Is_Outer_Tile; 
  mBOOL Is_Backward; 
  mBOOL Is_Outer_Lego_Tile;
  mBOOL Is_Inner_Lego_Tile;
  mBOOL Is_Processor_Tile;
  mBOOL Is_Doacross;
  mBOOL Suggested_Parallel; 
  mBOOL Parallelizable; 
#ifdef KEY
  mBOOL Vectorizable;
  mBOOL Delay_Full_Unroll;
#endif
  mBOOL Last_Value_Peeled; 
  mBOOL Not_Enough_Parallel_Work; 
  mBOOL Inside_Critical_Section;
  mBOOL Has_Barriers; 
  mBOOL Multiversion_Alias;
  mBOOL Loop_Vectorized;  // attribute to mark loops which are vectorized
  mBOOL Loop_Align_Peeled;  // attribute to mark loops that are peeled for align
  mINT8 Required_Unroll;
  mINT8 Prefer_Fuse;
  mINT8 Has_Precom_Def;
  mINT8 Has_Precom_Use;
  mINT8 Is_Precom_Init;
  mINT8 Sclrze_Dse;
  mINT32 Tile_Size; 
  double Work_Estimate; 
  mINT32 Required_Blocksize[MHD_MAX_LEVELS];
  WN    *Guard;
  LEGO_INFO* Lego_Info;
  ARA_LOOP_INFO* ARA_Info;
  MP_INFO* Mp_Info; 
  INT Lego_Mp_Key_Lower;
  INT Lego_Mp_Key_Upper;
  INT Lego_Mp_Key_Depth; 
  SYMBOL* Lego_LB_Symbols;
  mUINT8 Depth;
  INT  Blockable_Specification;
  INT* Permutation_Spec_Array;
  mINT8 Permutation_Spec_Count;
  EST_REGISTER_USAGE Est_Register_Usage;
  INT Doacross_Tile_Size;
  INT Sync_Distances[2];
  INT Doacross_Overhead;

  MEM_POOL *Pool() { return _pool; };

  BOOL Is_Cache_Winddown() const {return Get_Flag(CWD);}
  void Set_Cache_Winddown(BOOL b = TRUE) {Set_Flag(CWD, b);}
  BOOL Is_Register_Winddown() const {return Get_Flag(RWD);}
  void Set_Register_Winddown(BOOL b = TRUE) {Set_Flag(RWD, b);}
  BOOL Is_In_Cache_Winddown() const {return Get_Flag(ICWD);}
  void Set_In_Cache_Winddown(BOOL b = TRUE) {Set_Flag(ICWD, b);}
  BOOL Is_In_Register_Winddown() const {return Get_Flag(IRWD);}
  void Set_In_Register_Winddown(BOOL b = TRUE) {Set_Flag(IRWD, b);}
  BOOL Is_Generally_Unimportant() const {return Get_Flag(UNIMPORTANT);}
  void Set_Generally_Unimportant(BOOL b = TRUE) {Set_Flag(UNIMPORTANT, b);}
  IDTYPE Get_Id() { return _id; }
  IDTYPE Set_Id(IDTYPE i ) { _id = i; }

  DO_LOOP_INFO(MEM_POOL *pool, ACCESS_ARRAY *lb, ACCESS_ARRAY *ub,
	ACCESS_VECTOR *step, BOOL has_calls, BOOL has_nested_calls, BOOL has_unsummarized_calls,
	BOOL has_unsummarized_call_cost, BOOL has_gotos, 
	BOOL has_gotos_this_level,BOOL has_exits, BOOL is_inner); 
  DO_LOOP_INFO(DO_LOOP_INFO *dli, MEM_POOL *pool);

  void Print(FILE *fp, INT = 0);
  ~DO_LOOP_INFO() {
    CXX_DELETE(LB,_pool);
    CXX_DELETE(UB,_pool);
    CXX_DELETE(Step,_pool);
    if (Permutation_Spec_Count > 0)
      CXX_DELETE(Permutation_Spec_Array, _pool);
  }
  private:
  DO_LOOP_INFO();   //intentionally undefined
};

inline DO_LOOP_INFO* Get_Do_Loop_Info(const WN* wn, BOOL ok_if_none = FALSE)
{
  Is_True(wn && WN_opcode(wn) == OPC_DO_LOOP, ("Get_Do_Loop_Info bug"));
  DO_LOOP_INFO* dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map, wn);
  if (!ok_if_none)
    FmtAssert(dli, ("Get_Do_Loop_Info(): Unmarked do loop 0x%lx", wn));
  return dli;
}

inline void Set_Do_Loop_Info(WN* wn, DO_LOOP_INFO* dli)
{
  Is_True(wn && WN_opcode(wn) == OPC_DO_LOOP, ("Set_Do_Loop_Info bug"));
  WN_MAP_Set(LNO_Info_Map, wn, dli);
}

inline BOOL Do_Loop_Is_Good (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && !dli->Has_Bad_Mem);
}

inline BOOL Do_Loop_Has_Calls (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_Calls);
}

inline BOOL Do_Loop_Has_Unsummarized_Calls (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_Unsummarized_Calls);
}

inline BOOL Do_Loop_Has_Unsummarized_Call_Cost (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_Unsummarized_Call_Cost);
}

inline BOOL Do_Loop_Has_Threadprivate (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_Threadprivate);
}

inline BOOL Do_Loop_Has_Gotos (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_Gotos);
}

inline BOOL Do_Loop_Has_Conditional( WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_Conditional);
}

inline BOOL Do_Loop_Has_Gotos_This_Level (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_Gotos_This_Level);
}

inline BOOL Do_Loop_Has_Exits (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_Exits);
}

inline BOOL Do_Loop_Has_EH_Regions (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli && dli->Has_EH_Regions);
}

inline BOOL Do_Loop_Is_Inner (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli->Is_Inner);
}

inline BOOL Do_Loop_Is_Ivdep(WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli->Is_Ivdep);
}

inline BOOL Do_Loop_Is_Concurrent_Call(WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli->Is_Concurrent_Call);
}

inline BOOL Do_Loop_Concurrent_Directive(WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli->Concurrent_Directive);
}

inline INT Do_Loop_Depth (WN *wn)
{
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  return(dli->Depth);
}

extern BOOL Mark_Code(WN *wn, BOOL promote_pointers = FALSE, 
  BOOL strict_limit = TRUE);

extern BOOL Build_Array_Dependence_Graph (WN* func_nd);
extern void Build_CG_Dependence_Graph (WN* func_nd);
extern void Build_CG_Dependence_Graph (ARRAY_DIRECTED_GRAPH16*
                                       Array_Dependence_Graph);
extern INT32 Current_PU_Count(void);

#ifdef TARG_X8664
extern void Mark_Auto_Vectorizable_Loops (WN* func_nd);
#endif

inline IF_INFO* Get_If_Info(WN* wn, BOOL ok_if_none = FALSE)
{
  Is_True(wn && WN_opcode(wn) == OPC_IF, ("Get_If_Info bug"));
  IF_INFO* ii = (IF_INFO *) WN_MAP_Get(LNO_Info_Map, wn);
  if (!ok_if_none)
    FmtAssert(ii, ("Get_If_Info(): Unmarked if"));
  return ii;
}

class REGION_INFO { 
private: 
  mBOOL _auto_parallelized; 
public: 
  BOOL Auto_Parallelized() {return _auto_parallelized;} 
  REGION_INFO() {_auto_parallelized = FALSE;} 
  REGION_INFO(BOOL auto_parallelized): _auto_parallelized(auto_parallelized) {} 
  ~REGION_INFO() {} 
  void Print(FILE* fp); 
}; 

inline REGION_INFO* Get_Region_Info(const WN* wn)
{
  FmtAssert(wn != NULL && WN_opcode(wn) == OPC_REGION, 
    ("Get_Region_Info: Passed null pointer to pointer to non-REGION node")); 
  REGION_INFO* rgi = (REGION_INFO *) WN_MAP_Get(LNO_Info_Map, wn);
  return rgi;
}

// TP_LNOPT = 31 options.

#define  TT_LNO_DEP			0x00000001
#define  TT_LNO_DEP2			0x00000002
#define  TT_LNO_VERBOSE			0x00000004
#define  TT_LNO_PREFETCH_VERBOSE	0x00000008

#define  TT_LNO_SKIP_FIZ_FUSE		0x00000010
#define  TT_LNO_SKIP_PH2		0x00000020
#define  TT_LNO_SKIP_INNER_FISSION	0x00000040
#define  TT_LNO_SKIP_LNO		0x00000080

#define  TT_LNO_SNL_DEBUG1		0x00000100
#define  TT_LNO_SNL_DEBUG2		0x00000200
#define  TT_LNO_SNL_INTERACT		0x00000400
#define  TT_LNO_SE_MALLOC               0x00000800

#define  TT_LNO_PREFETCH        	0x00001000
#define  TT_LNO_PREFETCH_DEBUG		0x00002000
#define  TT_LNO_MODEL			0x00004000 
#define  TT_LNO_CACHE_MODEL_DEBUG	0x00008000

#define  TT_LNO_AEQUIV			0x00010000
#define  TT_LNO_SCLRZE			0x00020000
#define  TT_LNO_DEAD			0x00040000
#define  TT_LNO_GUARD			0x00080000

#define  TT_LNO_MINVARIANT_DEBUG	0x00100000
#define  TT_LNO_NORENAME		0x00200000
#define  TT_LNO_SKIP_GS		        0x00400000

#define TT_LNO_SKIP_NONLIN		0x01000000 
#define TT_LNO_DEBUG_DELIN		0x02000000 
#define TT_LNO_BIG_SCALAR_TILES		0x04000000
#define TT_GRAPH_CAPACITY		0x08000000

#define TT_LNO_DEBUG_CSE		0x10000000
#ifdef KEY
#define TT_LNO_DEBUG_SIMD               0x20000000
#define TT_LNO_DEBUG_HOISTIF            0x40000000
#define TT_LNO_LOOP_UNSWITCH             0x80000000
#endif /* KEY */

// TP_LNOPT = 32 options

#define TT_LEGO_VERBOSE             0x00000001
#define TT_LEGO_DEBUG               0x00000002
#define TT_LEGO_PRAGMAS_ONLY        0x00000004
#define TT_LEGO_DISABLE_DIVMOD      0x00000008

#define TT_LEGO_DISABLE_HOIST       0x00000010
#define TT_LEGO_DISABLE_IMPLICIT_AFFINITY    0x00000020
#define TT_LEGO_DISABLE_EXPLICIT_AFFINITY    0x00000040
#define TT_LEGO_DISABLE_INTERCHANGE 0x00000080

#define TT_LNO_RUN_ARA              0x00000100
#define TT_LNO_ARA_VERBOSE          0x00000200
#define TT_LNO_ARA_DEBUG            0x00000400
#define TT_LNO_NO_AUTO_PARALLEL	    0x00000800

#define TT_LEGO_DISABLE_RR_MAPS     0x00001000
#define TT_LEGO_DISABLE_FP_DIVMOD   0x00002000
#define TT_LNO_PARALLEL_DEBUG	    0x00004000
#define TT_LNO_NO_TRANSPOSE	    0x00008000

// [unused]                         0x00010000 (was TT_LNO_DEBUG_PROMPF)
#define TT_LNO_DISABLE_SEFIN	    0x00020000
#define TT_LNO_NO_PAD 		    0x00040000
#define TT_LNO_OMP_TRANSFORMS 	    0x00080000
#define TT_HMB_FORCE_VERSIONS	    0x00100000
#define TT_SHACKLE_ONLY		    0x00200000
#define TT_TILE_ONLY		    0x00400000
#define TT_IPA_LNO_READ 	    0x00800000
#define TT_CALL_INFO		    0x01000000
#define TT_SHACKLE_DEBUG            0x02000000
#define TT_CROSS_LOOP               0x04000000
#define TT_STRUCT_ARRAY_COPY        0x08000000
#define TT_TRACE_STRUCT_SPLIT_TRANS 0x10000000
#define TT_STRUCT_SPLIT_DUMP_IR     0x20000000

#ifdef TARG_X8664
extern BOOL Minvariant_Removal_For_Simd;
#endif
#endif /* lnopt_main_INCLUDED */


