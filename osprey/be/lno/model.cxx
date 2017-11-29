/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//                     Machine Models
//                     --------------
//
// Description:
//
//	Model the amount of time it takes to execute one iteration of an 
//	inner loop.  This model is used by lno to select the inner loop
//	and to select blocking factors
//
//	It is impossible to be perfectly accurate.  LNO occurs too early
//	in the compilation process.  As our algorithm is enumerate and
//	evaluate, it's also possible to be very expensive if we're not
//	careful. For the first cut of the algorithm,
//	we use many simplifying assumptions.  As we gain experience on
//	real code, we will enhance the model accordingly.
//	Where possible, we attempt to err in such a way as to minimize
//	unrolling.  It's more likely that we'll do damage if we
//	unroll too much than if we unroll too little.
//
//	Algorithm:
//
//		We first describe the components.  Then we'll discuss
//	how to combine them.
//
//	Step 1. Get resource constraints
//
//	    We need to calculate five things here; OP_rcycles,
//	MEM_rcycles, OP_issue, LOOP_INIT_issue, MEM_issue.  XXX_rcycles is
//	the number of cycles (based on resources for XXX) needed to
//	execute the loop.  XXX_issue is the number of instruction issues
//	needed for XXX.  The number of resource cycles needed overall will be 
//
//	MAX(OP_rcycles,
//          MEM_rcycles,
//          (OP_issue+LOOP_INIT_issue+MEM_issue)/ISSUE_rate)
//
//	We separate issue from XXX_rcycles since issue is a shared resource.
//
//	    1a. Calculate the number of cycles needed based on adding up
//	all the resource requirements.  We walk
//	through the code and add up the resources requirements for all the
//	ops. We pre-compute resource requirements for unroll_prod=1 ..  16.
//	and add in the loop branch and loop var. increment.
//	We tried to calculate the number of integer cycles needed based on
//	integer resource requirements.  This is difficult
//	for three reasons.  First, forward substitution makes it seem like
//	there are a lot more integer ops than there really are.  Second,
//	we don't know how array addressing will expand out. Third, integer
//	multiplication will usually be strength reduced.
//      Then we compute the minimum cycles per orig. iteration
//      required based on the resource requirement.
//      We return -1.0 cycles if there is any op we can't handle.
//	For multiply adds, we consider any ADD/SUBTRACT 
//	above a MPY to be a multiply add.  This should underestimate
//	the number of MADDS since it won't catch things like a = b*c; d += a.
//	For complex operators, we treat add/sub as two floating-point adds,
//	we treat neg as two fp negates,
//	we treat multiply as two madds + 2 fp mults.
//	All other complex operators are unhandleable (division generates
//	complex code sequences so we'll never want to unroll for regs).
//	We don't currently handle quads.
//
//	    1b. Calculate the number of ops.  This gives an approximaton
//	to the number of issue resources needed by the ops.  
//	For efficiency this is calculated at the same time as 1a.
//
//	   1.c. Calculate the number of MEM_rcycles.  We assume that each
//	load/store takes 1 cycle.  We look only at array references.
//	Scalars will probably be loop invariant.  We don't count references
//	that are invariant in the inner loop.
//	Group the subexpressions, ie a[i] and a[i]  into equivalence classes.
//	For the inner loop, assume references within a small distance are in 
//	the same class; i.e. assume that a[i][j] and a[i][j+1] are in the same 
//	class. These will generate common-subexpressions when unrolled by the 
//	inner loop If an equivalence class is both loaded and stored and the
//	references are equivalent and the load is lexically before the store,
//	we add two for it.  Otherwise we add one for each equivalence class.  
//	For example given a[i] = a[i] ..., we add two for a[i] since we'll 
//	need to both load and store it.  Given a[i] = ...; ... = a[i] or
//	given a[i] = a[i-1]..., we only add one
//	since we'll keep the value being loaded in a register
//	We don't actually use dependences to find the 
//	cses.  First, we don't have read-read dependences.  Second,
//	it's not sufficient.  Two references can be the same even if they
//	have a star dependence.  Instead, we compare the access vectors.
//	We also ignore intervening dependences.  We figure that either
//	they prevent unrolling (an input to model) or they don't exist.
//
//	Dealing with register blocking.  We assume that 1b scales
//	with the product of the unrolling factors. For 1c, we unroll and count.
//
//	Step 2. Get latency constraints
//
//	    2a. Build a dependence graph for the loop.  Add a vertex for
//	each each fp load and each fp store.  Add an edge to every store from
//	every load in the same statement.  These edges will have a latency
//	equal to the sum of the latencies of the operations going from
//	the load up.  They will have a distance '0'.
//	The latency of the FP ops will be taken from targ_info.
//	We then add an edge for each flow dependence.
//	These edge contains a "distance vector" (we collapse the
//	DEPV array into a single DEPV) and a latency of 0.
//	(this is pretty much true for TFP and T5).  
//	The total latency will be the maxium over all cycles 
//	of (latency/distance).
//
//	We use the SWP algorithm to find this maximum.  
//	This algorithm runs faster with a
//	good lower bound.  We set the lower bound to FP_rcycle
//	We can never do better than this.
//
//	We ignore scalar flow dependences.  So given for example
//	x = a[i] + b[i]
//	c[i] = x + d[i]
//	The total latency will be one add rather than two.  This is a tradeoff.
//	In the above case, we do the wrong thing, but in the example below
//	x = x + a[i]
//	forward substitution/reduction elimination will get rid of the scalar
//	dependence, so we really don't want to count it.  We assume that
//	because of forward substitution, cases like the first example are
//	rarer than cases like the second.
//
//	    2b. Deal with register blocking
//      We assume that unrolling an outer loop does not increase the 
//	recurrence bound for the inner loop.  Carr proved this is true.
//	The one case it's not really true is when there is a reduction
//	and we unroll by changing associativity.  In that case, we assume
//	that the reduction can be broken (using partial sums).
//
//
//	Step 3. Floating point register pressure
//	
//	We want to estimate the number of registers
//	required for the loop.  Actually scheduling and allocating
//	registers is impossible.  So, we guesstimate using the following
//	formula.  We add together the following terms.
//
//	    3a.  Take the latency from load-fp_use-store of the target 
//	machine; usually 5 on tfp and on the r4k, usually 3 on t5. 
//	Add a fudge factor of 1.  Multiply this number by the number
//	of fp units on the machine.  This gives an estimate for the number
//	of registers needed to keep the pipeline of the machine humming;
//	12 on tfp, 6 on the r4k and 8 on t5.
//
//	    3b. Count the number of invariant locations being read/written.
//	Add one for each.  We assume each location is register allocated
//	for the whole loop.  For arrays, we count all the invariant ones.
//	For scalars we only count those not stored.  We assume the stored
//	ones are just temporaries which do not live across loop iterations.
//      If most of the stores in the loop are invariant, we don't need
//      registers to keep the pipeline full.  I.e. the registers needed
//      for the pipeline are there for the invariants.  So if > 80% of
//      the stores are invariant, we return 2/3 of the registers needed
//      by the pipeline.
//
//	    3c. Count the number of loop variant array common subexpressions;
//	ie a[i] and a[i].  Group the subexpressions into equivalence classes.
//	Assume that we need to store the value of the equivalence class
//	the whole iteration (in reality we might be storing it for less).
//	So count the number of equivalence classes.  For the inner loop, 
//	assume references within a small distance are in the same class;
//	i.e. assume that a[i][j] and a[i][j+1] are in the same class.
//	These will generate common-subexpressions when unrolled by the
//	inner loop.  For exact duplicates, only count as duplicates
//	things that are loaded multiple times.  Given a[i] = a[i],
//	we don't need to store a[i] in a register.
//	For small distance duplicates, keep track of the min/max offsets.
//	The number of registers needed is the span.  I.e., given
//	a[i][j] and a[i][j+3], add three registers.
//
//	    3d. Add a fuge factor of 6 for fp.
//	Looking at a few codes and the SWP, the above estimate for fp
//	seems to be a bit low.
//
//	    3e. Dealing with register blocking.  3a, 3d are automatic.
//	For 3b and 3c, we unroll the references and count.
//
//	Step 4. Integer register pressure
//
//	    4a. Calculate integer registers used for addressing.
//	We examine all array references after removing potential cse and
//      invariants. If the addresses difference of two references are
//	a known constant and is <2^16, they will use the same register
//	for addressing. For example,
//		for i
//		  for j
//			a[i][j],a[i+1][j],b[i][j]-- different base registers
//			a[i][j],a[i][j+1]		-- same registers
//
//	    4b. Dealing with register blocking. We
//	unroll the references and count.
//
//      Register allocation penalties (both fp and int) 
//
//	Register penalty.  We never unroll when that creates a 
//	schedule that uses more than the maximum number of registers.  We add 
//	a 10% penalty if we're in two of the maximum number of registers.
//	This prevents us from trying to use all the registers unless the
//	possible gain is significant.  When we exceed the number of registers
//	(not unrolling), we divide the total number of refs by the number
//	of registers required for loads/stores.  We take this refs/reg
//	ratio and multiply it by the excess number of registers required.
//	We add this to the number of memory references.  This is an attempt
//	to model not using registers.
//
//
//
//	COMBINING THE COMPONENTS
//
//	   The constructor computes all the components that do not
//	vary with choice of inner loop or with blocking factors:
//	OP_rcycles, OP_issue, LOOP_INIT_issue, issue_rate,
//	base_{fp,int}_regs (the number of registers needed for the pipeline)
//	and scalar_{fp,int}_regs (the number of registers needed for scalars).
//	It then creates a latency graph that will later be used to estimate
//	latencies.  This routine also creates an ARRAY_REF: a list of all the
//	array references in the routine.  These will later be unrolled and
//	evaluated.  Finally, the constructor loops over each possible inner 
//	loop, calling Try_Inner.
//
//	   Try_Inner computes a latency bound (using the latency graph 
//	computed above).  It then calls Try_Unroll, which recurses and
//	loops through the possible unrolling factors.
//
//	  Try_Unroll is a recursive routine with recursion variable "l".
//	If we are allowed to unroll "l", we loop through possible unrolling
//	factors u.  For each u, we unroll the list of array references by 'u'.
//	u starts at 1 and is incremented until we find a reason
//	to stop.  We stop incrementing if the product of the unrolling factors
//	gets too big (> 16), if we can't register allocate or if we get
//	a perfect schedule (bound by floating point resource requirements).
//	If we get a perfect schedule, we pass the information up the 
//	recursion chain so that we don't try any different unrolling
//	factors.  If u=1 and we can't register allocate, then we also
//	pass the information up.  So, if u(l) = 1 and u(l-1) = 2, we
//	won't try to unroll "l-1" any further.  If u!=1, we don't pass
//	the register information up the recursion chain.  If u(l) = 2 and
//	u(l-1) = 1, we WILL still try the case where u(l-1) = 2 and u(l) = 1.
//	If 'l' is greater than the number of loops, we've reached our base
//	case and call evaluate to evaluate a particular unrolling.
//
//	  Evaluate evaluates a particular choice of unrolling.  It counts
//	the number of register needed and array memory references as 
//	described above.  If we can't register allocate, it sets 
//	*can_reg_allocate = FALSE and return.  It computes the resource
//	and latency limits as described above.  If we've found a perfect
//	schedule (bound by resource limits), we set
//	*try_further_unroll = FALSE.  This will prevent us from trying 
//	further unrolling factors.  If we're within two registers of the
//	maximum, we add a 10% penalty to the schedule.  We compare this
//	schedule with the current best, if it's atleast 1% better we reset 
//	the best.  In case of a tie, we chose the schedule with the lowest 
//	unrolling product.  The 1% is to avoid unrolling 16 times in cases 
//	with diminishing returns.
//
//
// 	Max_Unroll_Prod=16
//
//		The maximum amount we allow the product of the unrolling
//		factors to be.
//
//	Max_Cse_Dist=6
//
//		Given 'i' as an inner loop and two references
//		a[i] and a[i+-c], we'll assume that the SWP will
//		cse the two references if 'c' <= 6
//

/* ====================================================================
 * ====================================================================
 *
 * Module: model.cxx
 * $Revision: 1.21 $
 * $Date: 05/05/04 09:52:29-07:00 $
 * $Author: gautam@eng-27.pathscale.com $
 * $Source: ../../be/lno/SCCS/s.model.cxx $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Model time taken by inner loops
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source: ../../be/lno/SCCS/s.model.cxx $ $Revision: 1.21 $";

#include <sys/types.h>
#include <alloca.h>
#include <math.h>

#include "model.h"
#include "cxx_memory.h"
#include "lnopt_main.h"
#include "stab.h"
#include "lnotarget.h"
#include "dep_graph.h"
#include "lwn_util.h"
#include "cache_model.h"
#include "reduc.h"
#include "config.h"
#include "config_targ_opt.h"
#include "config_cache.h"
#include "config_lno.h"
#include "config_opt.h"
#include "tlog.h"

typedef HASH_TABLE<WN*,INT> WN2INT;

#if defined(TARG_MIPS) || defined(TARG_LOONGSON)
#define Reserved_Int_Regs	9	// $0, $26-$29, loop ub, fudge (3)
#endif
#ifdef TARG_PPC32
#define Reserved_Int_Regs	9	// TODO:verify this number later
#endif
#ifdef TARG_IA64
#define Reserved_Int_Regs	10	// r0, r1, r12, r13, loop ub, fudge (5)
#endif
#ifdef TARG_IA32
#define Reserved_Int_Regs	3	// $sp, $bp, fudge(1)
#endif
#ifdef TARG_X8664
#define Reserved_Int_Regs	3	// $sp, $bp, fudge(1)
#endif

#define MTYPE_is_double(m)	(MTYPE_size_reg(m)==MTYPE_size_reg(MTYPE_I8))

static MEM_POOL Model_Local_Pool;
static MEM_POOL Model_Lat_Pool;
static BOOL model_mempool_initialized;

// how many good DO loops (counting possibly this one, surround wn)
static INT Num_Good(WN *wn) 
{
  if (!wn) return(0);
  if (WN_operator(wn) == OPR_DO_LOOP) {
    if (Do_Loop_Is_Good(wn)) {
      return(1+Num_Good(LWN_Get_Parent(wn)));
    } else {
      return(0);
    }
  } else {
    return(Num_Good(LWN_Get_Parent(wn)));
  }
}

// Find the step of loop i given that wn is the inner loop
// return 0 on error or if it's not constant
static INT Find_Step(WN *wn, INT i)
{
  if (!wn) return(0);
  if (WN_operator(wn) == OPR_DO_LOOP) {
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    if (dli->Depth < i) {
      return(0);
    } else if (dli->Depth > i) {
      return(Find_Step(LWN_Get_Parent(wn),i));
    } else {
      ACCESS_VECTOR *Step = dli->Step;
      if (Step->Is_Const()) {
	return(Step->Const_Offset);
      } else {
	return 0;
      }
    }
  } else {
    return(Find_Step(LWN_Get_Parent(wn),i));
  }
}


// Start off with routines for ARRAY_REF_LIST and ARRAY_REF
// We store a list of all array references to help us estimate how
// many loads and stores are in the loop
void ARRAY_REF_NODE::Print(FILE *fp) const
{
  fprintf(fp,"(size=%d) ", _element_size);
  fprintf(fp, "Wn = 0x%p ", Wn);
  Array->Print(fp);
}

INT Max_Unroll_Prod=16;
INT Max_Cse_Dist = 6;

INT LOOP_MODEL::_model_no = 0;

LOOP_MODEL::~LOOP_MODEL()
{
  CXX_DELETE_ARRAY(_block_number,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_block_number_inner,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_iloop,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_iloop_inner,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_stripsz,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_stripsz_inner,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_striplevel,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_striplevel_inner,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_new_order,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_new_order_inner,Malloc_Mem_Pool);
}

static BOOL debug_model;

static INT Inner_Invariant_Refs;
static double Invariant_Ref_Coeff;
static INT Num_Complex_Mpy;

INT64 *SNL_Line_Numbers;

extern INT Debug_Cache_Model; 


#ifndef TARG_X8664
void
LOOP_MODEL::Shift_Vectorizable_Innermost (void) {
}

#else
void
LOOP_MODEL::Shift_Vectorizable_Innermost (void) {
    
  if (!LNO_Interchange)
    return;

  Is_True (_wn && _can_be_inner, ("Internal inconsistency"));

  INT wndepth = Do_Loop_Depth (_wn);

  // step 1: anchor the vectorizable innermost loop in the innermost position
  //
  
  //bug 2456, bug 5724 and bug 9143
  //if an inner loop is vectorizable and it is beneficial to do so, then
  //we should keep this loop innermost (i.e. the innermost loop can not
  //be changed
  if (Is_Vectorizable_Inner_Loop(_wn) && 
      Is_Vectorization_Beneficial(WN_do_body(_wn))) {

    _required_permutation[_inner_loop] = _inner_loop; //set inner's position
    for (INT i = 0; i <= wndepth; i++) {
      //only inner can be inner - don't change out
      _can_be_inner[i] = (i == _inner_loop); 
    }
    return;
  }
    
  // step 2: shift vectrorizable outer loop into the innermost position
  //

  if (LNO_Loop_Model_Simd) {
    DOLOOP_STACK stack (&Model_Local_Pool);
    Build_Doloop_Stack (_wn, &stack);
    
    // Work from second innermost to outer, examing if the loop can be 
    // vectorized.
    //
    #define MAX_TRY 3
    for (INT i = 1; i < MIN (wndepth, MAX_TRY); i++) {
      WN* loop = stack.Top_nth (i);
      if (!_can_be_inner [wndepth - i])
        break;

      if (Is_Vectorizable_Outer_Loop(loop) && 
          Is_Vectorization_Beneficial(WN_do_body(_wn))) {

        const char* fmt = 
        "Vectorizable outer loop at line:%d is moved to innermost position\n";

        if (LNO_Simd_Verbose) {
            printf (fmt, Srcpos_To_Line (WN_Get_Linenum (loop)));
        } 
    
        if (Get_Trace(TP_LNOPT, TT_LNO_MODEL)) {
            fprintf (TFile, fmt, Srcpos_To_Line (WN_Get_Linenum (loop)));
        }

        for (INT j = 0; j <= wndepth; j++) {
          _can_be_inner[j] = (j == (wndepth - i)); 
        }
      }
    }
  }
}
#endif

void 
LOOP_MODEL::Model(WN* wn, 
                  BOOL* can_be_inner, 
                  BOOL* can_be_unrolled,
                  INT outermost_can_be_tiled, 
                  ARRAY_DIRECTED_GRAPH16* array_graph,
                  SX_INFO* pi,
                  INT SNL_Depth,
                  HASH_TABLE<WN*,BIT_VECTOR*>* invar_table)
{
  if (!model_mempool_initialized) {
    MEM_POOL_Initialize(&Model_Local_Pool,"Model_Local_Pool",FALSE);
    MEM_POOL_Initialize(&Model_Lat_Pool,"Model_Local_Pool",FALSE);
    model_mempool_initialized = TRUE;
  }

  MEM_POOL_Push(&Model_Local_Pool);
  
  INT wndepth = Do_Loop_Depth(wn);
  _num_loops = wndepth+1;
  
  Is_True(WN_operator(wn)==OPR_DO_LOOP, ("non DO loop passed to LOOP_MODEL"));
  Is_True(Do_Loop_Is_Inner(wn),("non inner loop passed to LOOP_MODEL"));
  _est_num_iterations=CXX_NEW_ARRAY(INT64, wndepth+1,&Model_Local_Pool);
  _required_unroll=CXX_NEW_ARRAY(INT, wndepth+1,&Model_Local_Pool);
  _required_blocksize=CXX_NEW_ARRAY(INT, 
			(wndepth+1)*MHD_MAX_LEVELS,&Model_Local_Pool);
  _required_permutation=CXX_NEW_ARRAY(INT, wndepth+1,&Model_Local_Pool);
  for (INT iii = 0; iii <= wndepth; iii++)
    _required_permutation[iii] = -1;
  _block_number = CXX_NEW_ARRAY(INT, Do_Loop_Depth(wn)+1,Malloc_Mem_Pool);
  _block_number_inner = CXX_NEW_ARRAY(INT, wndepth+1,Malloc_Mem_Pool);
  _iloop = CXX_NEW_ARRAY(INT, (wndepth+1)*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _iloop_inner = CXX_NEW_ARRAY(INT,(wndepth+1)*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _stripsz = CXX_NEW_ARRAY(INT,(wndepth+1)*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _stripsz_inner = CXX_NEW_ARRAY(INT,(wndepth+1)*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _striplevel = CXX_NEW_ARRAY(INT,(wndepth+1)*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _striplevel_inner = CXX_NEW_ARRAY(INT,(wndepth+1)*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _new_order = CXX_NEW_ARRAY(INT,wndepth+1,Malloc_Mem_Pool);
  _new_order_inner = CXX_NEW_ARRAY(INT,wndepth+1,Malloc_Mem_Pool);
  _num_cycles = -1.0;
  _wn = wn;
  _can_be_inner = can_be_inner;
  _num_evaluations = 0;
  _num_fp_regs = Target_FPRs + 1;
  _num_fp_refs = 0;
  _model_no++;
  _lat_graph = NULL;

  debug_model = Get_Trace(TP_LNOPT, TT_LNO_MODEL);
  if (debug_model) {
    fprintf(TFile,"Modeling an SNL\n");
  }

  // Initialize results to default case
  INT i;
  _inner_loop = wndepth;
  for (i = 0; i <= wndepth; i++) {
    _block_number[i] = 1;
    _new_order[i] = i;
  }
  _nstrips = 0;

  if (LNO_Analysis || LNO_Tlog) {
    SNL_Line_Numbers = CXX_NEW_ARRAY(INT64,1+wndepth,Malloc_Mem_Pool);
  }
  i = wndepth;
  WN *tmp = wn;
  _blocking_disabled = LNO_Blocking == 0;


#ifdef TARG_X8664
   // Bug 5880: if this loop contains a vectorizable intrinsic, and the user
   // want to vectorize it anyway (vintr==2), we disable blocking
   if(Is_Aggressive_Vintr_Loop(wn))
    _blocking_disabled = TRUE;
#endif

  if (LNO_Interchange == FALSE) {
    for (INT j = 0; j <= wndepth; j++)
      _required_permutation[j] = j;
  } else {
    Shift_Vectorizable_Innermost ();
  }

  INT loop_count = 0; 
  while (tmp) {
    if (WN_operator(tmp) == OPR_DO_LOOP) {
      loop_count++; 
      if (LNO_Analysis || LNO_Tlog) {
        SNL_Line_Numbers[i] = WN_Get_Linenum(tmp);
      }
      DO_LOOP_INFO *dli = Get_Do_Loop_Info(tmp);
      _required_unroll[i] = dli->Required_Unroll;
      for (INT ll = 0; ll < MHD_MAX_LEVELS; ll++) {
        Is_True(dli->Required_Blocksize[ll] >= -1 &&
	          dli->Required_Blocksize[ll] <= 10000,
		  ("Suspicious required blocksize %d for loopno=%d level=%d",
		   dli->Required_Blocksize[ll], i, ll));
        _required_blocksize[i*MHD_MAX_LEVELS+ll] =
	                   dli->Required_Blocksize[ll];
      }
      if (_blocking_disabled == FALSE && loop_count <= SNL_Depth)
        _blocking_disabled = dli->Cannot_Block;    // conservative but okay
      if (dli->Permutation_Spec_Count > 0) {
        for (INT j = 0; j < dli->Permutation_Spec_Count; j++) {
	  _required_permutation[i+j] = dli->Permutation_Spec_Array[j] + i;
	}
      }
      _est_num_iterations[i--] = dli->Est_Num_Iterations;
    }
    tmp = LWN_Get_Parent(tmp);
  }
  Is_True(i == -1, ("Bad loop depth"));
  
  BOOL error = FALSE;
  // one loop not mapped to several levels
  INT jj;
  for (jj = 0; !error && jj <= wndepth; jj++) {
    if (_required_permutation[jj] >= 0) {
      // one loop not mapped to several levels
      for (INT jjj = 0; jjj < jj; jjj++)
        if (_required_permutation[jj] == _required_permutation[jjj])
	  error = TRUE;
      // legal
      if ( ! LNO_Apply_Illegal_Transformation_Directives ) {
        if (jj < outermost_can_be_tiled ||
	    _required_permutation[jj] < outermost_can_be_tiled)
	  error = TRUE;
      }
    }
  }
  if (error) {
    // TODO: print a user level real error message
    for (INT jj = 0; jj <= wndepth; jj++)
      _required_permutation[jj] = -1;
  }
  for (jj = 0; jj <= wndepth; jj++)
    if (_required_permutation[jj] != -1)
      _can_be_inner[_required_permutation[jj]] = (jj == wndepth);

  // Resources
  _OP_issue = 0.0;
  INT num_good = Num_Good(wn);
  INT num_bad = Do_Loop_Depth(wn)+1-num_good;

  Num_Complex_Mpy = 0;

  _OP_resource_count = OP_Resources(WN_do_body(wn), &_OP_issue, invar_table);
  if (_OP_resource_count) {
    if (debug_model) {
      fprintf(TFile, "OP_Resource_Count:\n");
      TI_RES_COUNT_Print(TFile, _OP_resource_count);
      fprintf(TFile, "\n");
    }
    TI_RES_COUNT* resource_count = TI_RES_COUNT_Alloc(&Model_Local_Pool);
    _loop_rcycles_unroll_by = 
      CXX_NEW_ARRAY(double, Max_Unroll_Prod, &Model_Local_Pool);
    for (i = 0; i < Max_Unroll_Prod; i++) {
      TI_RES_COUNT* tmp_resource_count = TI_RES_COUNT_Alloc(&Model_Local_Pool);
      TI_RES_COUNT_Add(tmp_resource_count, resource_count, _OP_resource_count);
      LNOTARGET_Loop_Inc_Test_Res(tmp_resource_count);
      _loop_rcycles_unroll_by[i] =
        TI_RES_COUNT_Min_Cycles(tmp_resource_count)/(i+1);
      TI_RES_COUNT_Add(resource_count, resource_count, _OP_resource_count);
    }
  }

  _LOOP_INIT_issue = 2.0; 
  _base_int_regs = Reserved_Int_Regs;

  if (Is_Target_R8K()) {
    _issue_rate = 4.0;
    _base_fp_regs = 18;
    _num_mem_units = 2.0;
  } else if (Is_Target_R10K()) {
    _issue_rate = 4.0;
    _base_fp_regs = 14;
    _num_mem_units = 1.0;
  } else if (Is_Target_R4K()) {
    _issue_rate = 1.0;
    _base_fp_regs = 12;
    _num_mem_units = 1.0;
  } else if (Is_Target_R5K()) {
    _issue_rate = 2.0;
    _base_fp_regs = 14;
    _num_mem_units = 1.0;
#ifdef TARG_MIPS
  } else if (Is_Target_Sb1()) {
    _issue_rate = 4.0;
    _base_fp_regs = 18;
    _num_mem_units = 2.0;
#endif
#ifdef TARG_IA64
  } else if (Is_Target_Itanium()) {
    // Lmt_DevWarn(1, ("TODO: Tune LNO machine model parameters for IA-64"));
    _issue_rate = 6.0;  // 2 bundles with 3 instructions each
    _base_fp_regs = 32; // (8+1)*2+6
    _num_mem_units = 2.0;
#endif
#ifdef TARG_X8664
  } else if (Is_Target_x86_64()) {
    _issue_rate = 3.0;
    _base_fp_regs = 16;
    _num_mem_units = 2.0;
#endif
#ifdef TARG_PPC32
#define Is_Target_PPC() (1)
  } else if (Is_Target_PPC()) {
    //TODO: verify the parameters for PPC32
    _issue_rate = 4.0;
    _base_fp_regs = 18;
    _num_mem_units = 2.0;
#endif
#ifdef TARG_LOONGSON
  }else if (Is_Target_Loongson()) {
    _issue_rate = 4.0;
    _base_fp_regs = 14;
    _num_mem_units = 1.0;
#endif
  } else {
    Lmt_DevWarn(1, ("TODO: LNO machine model parameters are just wild guesses"));
    _issue_rate = 4.0;
    _base_fp_regs = 4;
    _num_mem_units = 2.0;
  }     

  _arl = CXX_NEW(ARRAY_REF(WN_do_body(wn),
                           SNL_Depth, 
                           &Model_Local_Pool,
                           invar_table),
                 &Model_Local_Pool);

  _num_tlb = _arl->Elements(); // how many base arrays

  if (_OP_resource_count) {
    // Latency
    MEM_POOL_Push(&Model_Lat_Pool);
    _lat_graph = CXX_NEW(LAT_DIRECTED_GRAPH16(50,200,num_good,num_bad,
                                              &Model_Lat_Pool,array_graph),
                         &Model_Lat_Pool);
    if (_lat_graph->Add_Vertices_Op_Edges(WN_do_body(wn),invar_table) == -1)
      _OP_resource_count = NULL;
    if (_lat_graph->Add_Flow_Edges() == -1)
      _OP_resource_count = NULL;

    // Registers
    _scalar_fp_regs = Unique_Unstored_Fp_Scalar_Refs(WN_do_body(wn),_arl,pi);
    _scalar_int_regs = Unique_Unstored_Int_Scalar_Refs(WN_do_body(wn),_arl,pi);
  }

  _num_fp_array_refs = _arl->Num_Fp_Refs();
  _num_int_array_refs = _arl->Num_Int_Refs();

  // This coefficient will be used in estimating potential benefit
  // of unrolling beyound the "ideal" schedule due to the hoisting
  // of a larger number of invariant memory references
  if (_num_fp_array_refs > 0) {
    Invariant_Ref_Coeff = 1 / (double)(_num_fp_array_refs * Max_Unroll_Prod);
  }
  else {
    Invariant_Ref_Coeff = 0.0;
  }
  if (debug_model) {
    fprintf(TFile, "Invariant_Ref_Coeff = 1 / (%d * %d) = %f\n",
            _num_fp_array_refs, Max_Unroll_Prod, Invariant_Ref_Coeff);
  }

  // Loop through possible inner loops.
  for (i=num_good+num_bad-1; i>=0; i--) {
    if (can_be_inner[i]) {
      Try_Inner(can_be_unrolled,outermost_can_be_tiled,
		  i,num_good+num_bad);
    }
  }

  if (_lat_graph) {
    CXX_DELETE(_lat_graph, &Model_Lat_Pool);
    MEM_POOL_Pop(&Model_Lat_Pool);
    _lat_graph = NULL;
  }

  CXX_DELETE_ARRAY(_est_num_iterations,&Model_Local_Pool);
  CXX_DELETE_ARRAY(_required_unroll,&Model_Local_Pool);
  CXX_DELETE_ARRAY(_required_blocksize,&Model_Local_Pool);
  CXX_DELETE_ARRAY(_required_permutation,&Model_Local_Pool);
  MEM_POOL_Pop(&Model_Local_Pool);
  if (debug_model) {
    fprintf(TFile,"Evaluated %d combinations\n",_num_evaluations);
    fprintf(TFile,"And the results are:\n");
    fprintf(TFile,"Transform<loop,reg> = (");
    for (INT b=0; b<num_good+num_bad; b++) 
      fprintf(TFile,"<%d,%d>", _new_order[b], _block_number[b]);
    if (_nstrips > 0) {
      fprintf(TFile," cblk[stripdepth=%d]=",_stripdepth);
      for (INT s=0; s<_nstrips; s++)
        fprintf(TFile,"(%d,%d)",_iloop[s],_stripsz[s]);
    }
    fprintf(TFile,")\n");
    if (_OP_resource_count==NULL)
      fprintf(TFile,"Couldn't accurately evaluate ops\n");
    else if (_num_fp_regs > Target_FPRs)
      fprintf(TFile,"Couldn't FP register allocate\n");
    else if (_num_int_regs > Target_INTRs)
      fprintf(TFile,"Couldn't INT register allocate (%d)\n", _num_int_regs);
    else {
      fprintf(TFile,"The number of cycles is %f \n", _num_cycles);
      fprintf(TFile,"The number of fp registers needed is %d\n", _num_fp_regs);
      fprintf(TFile,"The number of int registers needed is %d\n", _num_int_regs);
    }
  }
  if (LNO_Verbose) {
    printf("Evaluated %d combinations\n",_num_evaluations);
    if (_OP_resource_count==NULL)
      printf("Couldn't accurately evaluate ops\n");
    else if (_num_fp_regs > Target_FPRs)
      printf("Couldn't FP register allocate\n");
    else if (_num_int_regs > Target_INTRs)
      printf("Couldn't INT register allocate (%d)\n", _num_int_regs);
    else {
      printf("The number of cycles is %f \n", _num_cycles);
      printf("The number of fp registers needed is %d\n", _num_fp_regs);
      printf("The number of int registers needed is %d\n", _num_int_regs);
    }

  }
  if ( LNO_Tlog ) {
#define TLOG_MSG_LENGTH 640	// 30 chars * 16 loops + misc.

    char message[TLOG_MSG_LENGTH];
    char buffer[32];
    UINT16 char_count=0;
    message[0]='\0';

    sprintf(buffer,"comb=%d ",_num_evaluations);
    if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
      DevWarn("Tlog message buffer overflow");
      char_count=0;
      message[0]='\0';
    } else {
      strcpy(message+char_count, buffer);
      char_count+=strlen(buffer);
    }

    for (INT b=0; b<num_good+num_bad; b++) {
      sprintf(buffer,"<%d,%d>", _new_order[b], _block_number[b]);
      if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
        DevWarn("Tlog message buffer overflow");
        char_count=0;
        message[0]='\0';
      } else {
        strcpy(message+char_count, buffer);
        char_count+=strlen(buffer);
      }
    }
    if (_nstrips > 0) {
      sprintf(buffer," sd=%d ",_stripdepth);
      if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
        DevWarn("Tlog message buffer overflow");
        char_count=0;
        message[0]='\0';
      } else {
        strcpy(message+char_count, buffer);
        char_count+=strlen(buffer);
      }
      for (INT s=0; s<_nstrips; s++) {
        sprintf(buffer,"(%d,%d)",_iloop[s],_stripsz[s]);
        if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
          DevWarn("Tlog message buffer overflow");
          char_count=0;
          message[0]='\0';
        } else {
          strcpy(message+char_count, buffer);
          char_count+=strlen(buffer);
        }
      }
    }
    sprintf(buffer," cycles=%f fpreg=%d", _num_cycles, _num_fp_regs);
    if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
      DevWarn("Tlog message buffer overflow");
      char_count=0;
      message[0]='\0';
    } else {
      strcpy(message+char_count, buffer);
      char_count+=strlen(buffer);
    }
    sprintf(buffer,"INNER_LOOP %d",
      Srcpos_To_Line(SNL_Line_Numbers[_new_order[wndepth]]));

    SRCPOS srcpos=WN_Get_Linenum(wn);
    Generate_Tlog("LNO","snl", Srcpos_To_Line(srcpos),
                ST_name(WN_st(WN_index(wn))), message, buffer, "");

  }

  if (LNO_Analysis) {
    fprintf(LNO_Analysis,"    (INNER_LOOP %d)\n",
      Srcpos_To_Line(SNL_Line_Numbers[_new_order[wndepth]]));
  }
  if (LNO_Analysis) {
    CXX_DELETE_ARRAY(SNL_Line_Numbers, Malloc_Mem_Pool);
  }
  if (_num_tlb > Mhd.L[0].TLB_Entries) {
    _num_cycles += Mhd.L[0].TLB_Miss_Penalty * (Mhd.L[0].TLB_Entries-_num_tlb);
  }
} 


// ------------------------------------------------------------
// Count the number of references that are likely to be hoisted
// out of the inner loop by minvar algorithm. A reference that 
// has both a load and a store is counted double.
// ------------------------------------------------------------
static INT
Num_Invariant_Refs(ARRAY_REF_LIST* arl, INT loop)
{
  if (debug_model) {
    arl->Base_Array->Print(TFile); 
    fprintf(TFile, "\n");
  }
  
  INT num_invar_refs = 0;
  arl->Remove_Cse(loop, 0, 1);
  arl->Mark_Invariants(loop);
  ARRAY_REF_ITER iter(arl);
  for (ARRAY_REF_NODE* node = iter.First(); node; node = iter.Next()) {
    if (node->_is_invariant) {

      INT regs_per_ref = 1;
      if (arl->_is_scalar_expanded) {
        if (MTYPE_is_complex(arl->Base_Array->Type))
          regs_per_ref *= 2;
        if (MTYPE_is_quad(arl->Base_Array->Type))
          regs_per_ref *= 2;
      }
      else {
        WN* parent = LWN_Get_Parent(node->Wn);
        if (MTYPE_is_complex(WN_desc(parent)) ||
            MTYPE_is_complex(WN_rtype(parent)))
          regs_per_ref *= 2;
        if (MTYPE_is_quad(WN_desc(parent)) ||
            MTYPE_is_quad(WN_rtype(parent)))
          regs_per_ref *= 2;
      }

      if (node->_has_store) {
        num_invar_refs += regs_per_ref;
      }
      if (node->_has_load) {
        num_invar_refs += regs_per_ref;
      }
      if (debug_model) {
        fprintf(TFile, "  INV: ");
        if (node->_has_load) fprintf(TFile, "LOAD ");
        if (node->_has_store) fprintf(TFile, "STORE");
        node->Print(TFile);
      }
    }
    else {
      if (debug_model) {
        fprintf(TFile, "  MIXED INV/NOINV -> Assume INV = 0\n");
      }
      return 0;
    }
  }
  return num_invar_refs;
}
  
static INT
Num_Invariant_Refs(ARRAY_REF* ar, INT loop)
{
  INT num_invar_refs = 0;
  ARRAY_REF local_ar(ar, &Model_Local_Pool);
  for (INT i = 0; i < local_ar.Elements(); i++) {
    num_invar_refs += Num_Invariant_Refs(local_ar.Array_Ref_List(i), loop);
  }
  return num_invar_refs;
}


// model the nest assumming inner is the inner loop
void 
LOOP_MODEL::Try_Inner(BOOL* can_be_unrolled, 
                      INT outermost_can_be_tiled,
                      INT inner, 
                      INT num_loops)
{
  INT i, j;
  double machine_cycles;

  if (debug_model) {
    fprintf(TFile,"Trying loop %d for inner \n",inner);
  }

  _model_limit = MODEL_LIMIT_UNSET;

  MEM_POOL_Push(&Model_Local_Pool);
  
  INT* unroll_factors = CXX_NEW_ARRAY(INT, num_loops, &Model_Local_Pool);
  for (i = 0; i < num_loops; i++) {
    unroll_factors[i] = 1;
    _block_number_inner[i] = 1;
  }
  
  _num_int_regs_inner = Target_INTRs + 1;
  _num_int_refs_inner = 0;
  _num_fp_regs_inner = Target_FPRs + 1;
  _num_fp_refs_inner = 0;

  ARRAY_REF* arl_for_cache =
    CXX_NEW(ARRAY_REF(_arl, &Model_Local_Pool), &Model_Local_Pool);

  if (_OP_resource_count && 
      _base_fp_regs + _scalar_fp_regs <= Target_FPRs && 
      _base_int_regs + _scalar_int_regs <= Target_INTRs) {

    _latency_cycles = 
      _lat_graph->Max_Cycle(inner, _loop_rcycles_unroll_by[Max_Unroll_Prod-1]);
    
    if (debug_model) {
      fprintf(TFile, "Latency cycles for inner loop %d: %7.2f\n",
              inner, _latency_cycles);
    }

    BOOL can_reg_allocate;
    BOOL try_further_unroll = TRUE;

    _num_cycles_inner = -1.0;

    Inner_Invariant_Refs = Num_Invariant_Refs(_arl, inner);
    if (debug_model) {
      fprintf(TFile, "For inner loop %d there are %d invariant refs\n",
              inner, Inner_Invariant_Refs);
    }

    ARRAY_REF* new_arl =
      CXX_NEW(ARRAY_REF(_arl, &Model_Local_Pool), &Model_Local_Pool);

    Try_Unroll(can_be_unrolled, inner, num_loops, unroll_factors, 0, 1,
               &can_reg_allocate, &try_further_unroll, new_arl);

    for (i = 0; i < num_loops; i++)
      if (_block_number_inner[i] > 1)
        arl_for_cache->Unroll(i, _block_number_inner[i]);
  }
  else {
    // any greater than 0 value should be fine.
    _num_cycles_inner = 0.01;
    _latency_cycles = 0;
    machine_cycles = -1.0;
  }

  arl_for_cache->Remove_Cse(inner, 0, 1);

  double cycles_per_iter;
  double overhead_cycles;

  machine_cycles = _num_cycles_inner;

  FmtAssert(num_loops < 64, 
            ("Impossibly large number of loops %d", num_loops));

  INT	legal_inners[64];
  INT	legal_tiles[64];
  INT*	pinners = legal_inners;
  INT*	ptiles = legal_tiles;

  // backwards or cache model gets confused
  for (i = num_loops - 1; i >= 0; i--) {
    if (_can_be_inner[i])
      *pinners++ = i;
    else if (i >= outermost_can_be_tiled)
      *ptiles++ = i;
    _new_order_inner[i] = _required_permutation[i];
  }
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(_wn, &stack);
  Cache_Model(arl_for_cache, num_loops-1, inner,
              legal_inners, pinners - legal_inners,
              legal_tiles, ptiles - legal_tiles,
              _block_number_inner, &stack,
              _blocking_disabled, _required_blocksize,
              _num_cycles_inner, _num_fp_refs_inner+_num_int_refs_inner,
	      _new_order_inner,
              &_nstrips_inner, &_stripdepth_inner,
              _iloop_inner, _stripsz_inner, _striplevel_inner,
              &cycles_per_iter, &overhead_cycles);

  if (LNO_Verbose || Debug_Cache_Model)
    printf("inner = %d -> cycle est %g (before cache) ",
           inner, _num_cycles_inner);
  if (debug_model)
   fprintf(TFile,"inner = %d -> cycle est %g (before cache) ",
         inner, _num_cycles_inner);

  _num_cycles_inner += cycles_per_iter;

  double cache_cycles = _num_cycles_inner - machine_cycles - overhead_cycles;

  if (LNO_Verbose || Debug_Cache_Model)
    printf(" %g (after cache)\n", _num_cycles_inner);
  if (debug_model)
    fprintf(TFile," %g (after cache)\n", _num_cycles_inner);

  if (LNO_Verbose || Debug_Cache_Model) 
    printf("   %g cache cycles  %g overhead cycles\n", cache_cycles, 
      overhead_cycles); 
  if (debug_model)
    fprintf(TFile, "   %g cache cycles  %g overhead cycles\n", cache_cycles, 
      overhead_cycles); 

  if (_num_cycles == -1.0 || 
      _num_cycles_inner < _num_cycles || 
      (_num_cycles_inner == _num_cycles &&
       (_num_fp_regs_inner+_num_fp_regs_inner < _num_int_regs+_num_fp_regs ||
        _unroll_prod_inner < _unroll_prod))) {
    // a new best
    _num_cycles = _num_cycles_inner;
    _num_int_regs = _num_int_regs_inner;
    _num_int_refs = _num_int_refs_inner;
    _num_fp_regs = _num_fp_regs_inner;
    _num_fp_refs = _num_fp_refs_inner;
    _unroll_prod = _unroll_prod_inner;
    _nstrips = _nstrips_inner;
    _stripdepth = _stripdepth_inner;
    _inner_loop = _inner_loop_inner;

    for (i = 0; i < num_loops; i++) {
      _new_order[i] = _new_order_inner[i];
      _block_number[i] = _block_number_inner[i];
    }

    for (j = 0; j < _nstrips; j++) {
      _iloop[j] = _iloop_inner[j];
      _stripsz[j] = _stripsz_inner[j];
      _striplevel[j] = _striplevel_inner[j];
    }

    if (debug_model) {
      fprintf(TFile,"An overall best\n");
    }
  }

  if (LNO_Analysis) {
    Model_Results_Analysis(inner, num_loops, outermost_can_be_tiled,
			   machine_cycles, cache_cycles, overhead_cycles);
  }
  CXX_DELETE_ARRAY(unroll_factors, &Model_Local_Pool);
  MEM_POOL_Pop(&Model_Local_Pool);
}

// try different unrolling factors for loop 'l'
// unroll_factors gives the unrolling factors for the outermore loops
// unroll product gives the product of the unrolling factors for
// the outermore loops (we stop trying if the product is too large)
// on output set *can_reg_allocate to true if it was possible to
// register allocate given the outer unrolling factors
// on output set try_further_unroll to TRUE if there is a possible reason
// to try unrolling further (this gives us a short circuit)
// arl gives a list of all the array references unrolled by the outer 
// unrolling factors
void 
LOOP_MODEL::Try_Unroll(BOOL* can_be_unrolled, INT inner, INT num_loops,
                       INT* unroll_factors, INT l, INT unroll_product, 
                       BOOL* can_reg_allocate, BOOL* try_further_unroll,
                       ARRAY_REF* arl)
{
  if (l >= num_loops) { // base case of the recursion
    Evaluate(inner, num_loops, unroll_factors, unroll_product,
             can_reg_allocate, try_further_unroll, arl, can_be_unrolled);
  } 
  else if ((l != inner) && can_be_unrolled[l]) {
    INT known_unroll = 
      _required_unroll[l] ? _required_unroll[l] : LNO_Outer_Unroll;
    INT u = known_unroll ? known_unroll : 1;
    double prod = unroll_product;

    // Loop through all possible unrolling values as long as the product
    // stays small enough, we can still allocate registers and we haven't
    // reached a good enough schedule.
    // If we can't allocate with u=1, pass that up so that we won't further
    // unroll any outer loops
    BOOL can_allocate = TRUE;
    INT  max_unroll_prod = 999999;
    INT  max_unroll      = 999999;
    if (known_unroll) {
      max_unroll = known_unroll;
    }
    else {
      if (LNO_Outer_Unroll_Max)
        max_unroll = LNO_Outer_Unroll_Max;
      if (LNO_Outer_Unroll_Prod_Max)
        max_unroll_prod = LNO_Outer_Unroll_Prod_Max;
      if (_est_num_iterations[l] > 0)
        max_unroll = MIN(max_unroll,_est_num_iterations[l]);
    }

    while (can_allocate &&
           *try_further_unroll &&
	   (unroll_product * u) <= max_unroll_prod &&
           u <= max_unroll) {
      if (!known_unroll &&
          _est_num_iterations[l] > 0 &&
          u != max_unroll &&
          u*2 > _est_num_iterations[l]) {
        // If the loop has 6 iterations, silly to unroll by 4 or 5.
        // TODO: However, we don't particularly want to do this unless
        // est_num_iters is exact.  Likewise, the setting of max_unroll
        // above should probably be based not on _est_num_iters but
        // some sort of max.
        u = max_unroll;
        continue;
      }
      MEM_POOL_Push(&Model_Local_Pool);
      ARRAY_REF* new_arl;
      unroll_factors[l] = u;
      if (u > 1) {
        new_arl = CXX_NEW(ARRAY_REF(arl,&Model_Local_Pool),&Model_Local_Pool);
	new_arl->Unroll(l,u);
	prod = unroll_product*u;
      } 
      else {
	new_arl = arl;
      }
      new_arl->Remove_Cse(inner, Max_Cse_Dist, Find_Step(_wn,inner));
      new_arl->Mark_Invariants(inner);
      Try_Unroll(can_be_unrolled,inner,num_loops,unroll_factors,l+1,int(prod),
		&can_allocate, try_further_unroll, new_arl);
      if ((u == 1) && !can_allocate) {
	*can_reg_allocate = FALSE;  // don't let people above unroll further
      }

      u++;
      MEM_POOL_Pop(&Model_Local_Pool);
    }
    unroll_factors[l] = 1;
  } 
  else { // go to the next loop
    Try_Unroll(can_be_unrolled, inner, num_loops, unroll_factors, l+1,
               unroll_product, can_reg_allocate, try_further_unroll, arl);
  }
}

//  Given that we've unrolled the loop all we will (a factor of unroll
//  product), model this nest, setting inner_loop, num_cycles and
//  block_number if we've found a new best candidate.  Set *can_reg_allocate
//  to true if we don't use too many registers.  Set *try_further_unroll
//  to false if further unrolling won't help (we're doing good enough)
//  on input, arl gives all the array references after unrolling
void 
LOOP_MODEL::Evaluate(INT inner, INT num_loops, INT* unroll_factors,
                     INT unroll_product, BOOL* can_reg_allocate, 
                     BOOL* try_further_unroll, ARRAY_REF* arl,
                     BOOL* can_be_unrolled)
{
  _num_evaluations++;

  if (debug_model) {
    fprintf(TFile, "\nDoing an evaluation ");
    fprintf(TFile, "unroll_factors = (");
    for (INT b = 0; b < num_loops; b++) {
      fprintf(TFile, " %d ", unroll_factors[b]);
    }
    fprintf(TFile, ") \n");
  }

  BOOL did_unroll = FALSE;
  for (INT b = 0; b < num_loops && !did_unroll; b++) {
    if (unroll_factors[b] > 1 &&
        !(unroll_factors[b] == _required_unroll[b]) &&
        !(_required_unroll[b] <= 0 && unroll_factors[b] == LNO_Outer_Unroll))
      did_unroll = TRUE;
  }

  MODEL_LIMIT this_limit = MODEL_LIMIT_UNSET;

  // Count the number of registers 
  INT num_fp_regs, num_fp_refs, num_fp_variant_stores, num_fp_invariant_stores;
  INT num_int_regs, num_int_refs, num_int_variant_stores, num_int_invariant_stores;
  INT new_base_regs = _base_fp_regs;
  INT num_fp_spills = 0;
  INT num_int_spills = 0;
  arl->Calc_Regs_And_Refs(&num_fp_regs, &num_fp_refs, 
                          &num_fp_variant_stores, &num_fp_invariant_stores,
                          &num_int_regs, &num_int_refs, 
                          &num_int_variant_stores, &num_int_invariant_stores);

  // This coefficient is used to preference unrolling of multiple
  // loops by factors that are similar in size. I don't have a good
  // explanation for it, except for the empirical evidence that, e.g.,
  // unrolling 4x4 is usually better that unroling 2x8 or 8x2.
  double unequal_unroll_penalty = 1.0;
  if (num_loops >= 2) {
    INT un1, un2;
    if (unroll_factors[0] > unroll_factors[1]) {
      un1 = unroll_factors[0], un2 = unroll_factors[1];
    }
    else {
      un1 = unroll_factors[1], un2 = unroll_factors[0];
    }
    for (INT i = 2; i < num_loops; i++) {
      if (unroll_factors[i] > un1) {
        un2 = un1;
        un1 = unroll_factors[i];
      }
      else if (unroll_factors[i] > un2) {
        un2 = unroll_factors[i];
      }
    }
    unequal_unroll_penalty = pow(((double)un2)/un1, 0.3);
  }

  // Here's what goes into estimating minvar benefit:
  //
  // Inner_Invariant_Refs:
  //   number of hoistable memory references for the current inner loop
  // unroll_product
  //   because Inner_Invariant_Refs is computed prior to unrolling
  // unequal_unroll_penalty
  //   to favor "square/cube" unrolling factors
  // Invariant_Ref_Coeff
  //   based on the total number of array references and Max_Unroll_Prod
  // MINVAR_MAGIC_COEFF
  //   limit the potential benefit to 20% of the ideal cycle count
  
#ifndef KEY 
#define MINVAR_MAGIC_COEFF (0.20 * _loop_rcycles_unroll_by[Max_Unroll_Prod-1])
#else
  // Match octane cc MINVAR_MAGIC_COEFF
#define MINVAR_MAGIC_COEFF 0.20
#endif
  double minvar_benefit = MINVAR_MAGIC_COEFF
                        * Inner_Invariant_Refs 
                        * unroll_product
                        * unequal_unroll_penalty
                        * Invariant_Ref_Coeff;
    
  if (debug_model) {
    fprintf(TFile, 
            "Minvar benefit: %.3f * %d * %d * %.3f * %.3f = %.3f\n",
            MINVAR_MAGIC_COEFF,
            Inner_Invariant_Refs,
            unroll_product,
            unequal_unroll_penalty,
            Invariant_Ref_Coeff,
            minvar_benefit);
  }

  // don't count invariants both as invariants and as pipelines
  if (num_fp_invariant_stores > 4*num_fp_variant_stores) {
    new_base_regs /= 3;  
  }

  // Complex multiplies will use additional temporary FP registers 
  if (_est_num_iterations[inner] > LNO_Small_Trip_Count) {
    num_fp_regs += (Num_Complex_Mpy * unroll_product);
  }
  else {
    // Loops with small trip counts will not be pipelined
    new_base_regs = 0;
    num_fp_regs = 0;
  }
     
  *can_reg_allocate = TRUE;
  if (num_fp_regs + new_base_regs +_scalar_fp_regs > Target_FPRs) { 
    if (!did_unroll) { // penalty
      double fp_refs_per_reg = (_num_fp_array_refs + _num_fp_scalar_refs)
                             / (num_fp_regs + _scalar_fp_regs);
      num_fp_spills = 
	(INT)((num_fp_regs+new_base_regs+_scalar_fp_regs-Target_FPRs) 
	      * fp_refs_per_reg);
      num_fp_refs += num_fp_spills;
    }
    if (debug_model) {
      fprintf(TFile,"Can't FP register allocate \n");
    }
    *can_reg_allocate = FALSE;
  }
  if (num_int_regs + _base_int_regs + _scalar_int_regs > Target_INTRs) { 
    if (!did_unroll) { // penalty
      double int_refs_per_reg = (_num_int_array_refs + _num_int_scalar_refs)
                              / (num_int_regs + _scalar_int_regs);
      num_int_spills = 
        (INT)((num_int_regs + _base_int_regs + _scalar_int_regs - Target_INTRs)
	      * int_refs_per_reg);
      num_int_refs += num_int_spills;
    }
    if (debug_model) {
      fprintf(TFile, "Couldn't INT register allocate (%d)\n",
              num_int_regs + _base_int_regs + _scalar_int_regs);
    }
    *can_reg_allocate = FALSE;
  }

  // count memory references
  double MEM_issue = ((double) (num_fp_refs+num_int_refs)) / unroll_product;
  double MEM_rcycles = MEM_issue/_num_mem_units;
  if (debug_model) {
    fprintf(TFile, "MEM_rcycles: %.2f\n", MEM_rcycles);
    fprintf(TFile, "Issue: %.0f + %.0f + %.0f = %.0f\n", 
            _OP_issue * unroll_product, 
            _LOOP_INIT_issue, 
            MEM_issue * unroll_product,
            _OP_issue * unroll_product +
            _LOOP_INIT_issue +
            MEM_issue * unroll_product);
  }
  
  double MEM_issue_minus_spills = 
    ((double)(num_fp_refs - num_fp_spills + num_int_refs - num_int_spills)) 
    / unroll_product;
  double MEM_rcycles_minus_spills = MEM_issue_minus_spills/_num_mem_units;

  double issue_limit =
    (_OP_issue + _LOOP_INIT_issue/unroll_product + MEM_issue) / _issue_rate;
  double issue_limit_minus_spills = 
    (_OP_issue + _LOOP_INIT_issue/unroll_product + MEM_issue_minus_spills) 
    / _issue_rate;
  double ideal_resource_cycles = _loop_rcycles_unroll_by[Max_Unroll_Prod-1];
  double resource_cycles = 
    MAX(_loop_rcycles_unroll_by[unroll_product-1],
        MAX(MEM_rcycles, issue_limit)) - minvar_benefit;
  double resource_cycles_minus_spills =
    MAX(_loop_rcycles_unroll_by[unroll_product-1],
        MAX(MEM_rcycles_minus_spills, issue_limit_minus_spills));
#ifdef KEY
  // Match Octane CC and it seems logical to subtract minvar_benefit
  // from resource_cycles_minus_spills because it is going to be compared with 
  // resource_cycles below.
  resource_cycles_minus_spills -= minvar_benefit;
#endif

  // is memory or int still a problem, if not, we shouldn't unroll anymore
  if (!(*can_reg_allocate) ||
      (resource_cycles > ideal_resource_cycles) ||
      (minvar_benefit > 0.0) ||
      (_latency_cycles/unroll_product > resource_cycles)) {
    *try_further_unroll = TRUE;  // latency bound so unrolling can help
  } 
  else {
    *try_further_unroll = FALSE; // doing well enough
    this_limit = MODEL_LIMIT_IDEAL;
  }

#define SWP_INSTR_LIMIT 80

  // Unrolling should not create loops that are too big for SWP
  if (unroll_product  > 1 &&
      issue_limit * _issue_rate * unroll_product > SWP_INSTR_LIMIT) {
    if (debug_model) {
      fprintf(TFile, "Unrolled loop would be too large for SWP\n");
    }
    *try_further_unroll = FALSE;
    return;
  }

  double cycles = MAX(resource_cycles, _latency_cycles/unroll_product);
  double cycles_minus_spills = MAX(resource_cycles_minus_spills,
                                   _latency_cycles/unroll_product);

  if (!(*can_reg_allocate) && 
      !did_unroll && 
      cycles == cycles_minus_spills) { 
    // spilling is free so set the number of registers to Target_FPRs
    *can_reg_allocate = TRUE;
    num_fp_regs = Target_FPRs - new_base_regs - _scalar_fp_regs;
    num_int_regs = Target_INTRs - _base_int_regs - _scalar_int_regs;
  }

  INT fp_reg_usage = num_fp_regs + new_base_regs +_scalar_fp_regs;
  INT int_reg_usage = num_int_regs + _base_int_regs +_scalar_int_regs;

  if (!*can_reg_allocate) {
    if (did_unroll) {
     if (debug_model) {
       fprintf(TFile,"Can't register allocate\n");
     }
     return;
    }
  } 
  else if (fp_reg_usage > Target_FPRs-2) {
    cycles *= 1.1;  // penalty for being close to unallocatable
  } 
  else if (int_reg_usage > Target_INTRs-2){
    cycles *= 1.1;  // penalty for being close to unallocatable
  }

  // Penalty for unrolling something that's not
  // a factor of the estimated number of iterations
  // TODO: the right thing to do is to average in 
  // the percentage of time spent in cleanup
  double worst_mod = 0.0;
  for (INT l = 0; l < num_loops; l++) {
    if(unroll_factors[l] &&
       _est_num_iterations[l] > 0 &&
       _est_num_iterations[l] < LNO_Outer_Unroll_Max) {
      double this_mod = (_est_num_iterations[l] % unroll_factors[l]);
      if (this_mod > _est_num_iterations[l]*worst_mod) {
	worst_mod = this_mod/_est_num_iterations[l];
      }
    }
  }
  if (worst_mod > 0.1) {
    cycles *= 1.05;
  }

#ifdef TARG_IA64
  // For IA-64 we prefer an even number of FP ops,
  // but we don't penalize small loops that can be unrolled in CG
  INT num_fp_ops = (INT)(_OP_issue * unroll_product);
  if ((num_fp_ops & 1) &&
      issue_limit * _issue_rate * unroll_product > SWP_INSTR_LIMIT / 2) {
    cycles *= 1.05;
  }
#endif

  if (debug_model) {
    fprintf(TFile,"Ignoring cache effects ");
    fprintf(TFile,"this loop takes %f cycles\n",cycles);
    fprintf(TFile,"Uses %d int and %d fp regs\n", int_reg_usage, fp_reg_usage);
  }
  if (this_limit == MODEL_LIMIT_IDEAL) {
    if (debug_model) fprintf(TFile,"Ideal schedule \n");
  } 
  else if (resource_cycles > _latency_cycles/unroll_product) {
    this_limit = MODEL_LIMIT_RES;
    if (debug_model) fprintf(TFile,"Number of cycles limited by resources \n");
  } 
  else {
    this_limit = MODEL_LIMIT_LAT;
    if (debug_model) fprintf(TFile,"Number of cycles limited by latency \n");
  }

  if (_num_cycles_inner == -1.0 || 
      1.01*cycles < _num_cycles_inner || 
      (cycles <= _num_cycles_inner && 
       (unroll_product < _unroll_prod_inner ||
        fp_reg_usage+int_reg_usage<_num_fp_regs_inner+_num_int_regs_inner))) {
    // a new best
    _inner_loop_inner = inner;
    for (INT i=0; i<num_loops; i++) {
      _block_number_inner[i] = unroll_factors[i];
    }
    _num_cycles_inner = cycles;
    _num_fp_regs_inner = fp_reg_usage;
    _num_fp_refs_inner = num_fp_refs;
    _num_int_regs_inner = int_reg_usage;
    _num_int_refs_inner = num_int_refs;
    _unroll_prod_inner = unroll_product;
    _model_limit = this_limit;

    if (debug_model) {
      fprintf(TFile,
              "Found a new best for unrolling factors for this inner loop\n");
    }
  }
}

// Walk through the inner loop 
// return how many cycles the loop takes based on the resource requirements 
// of the floating point instructions
// also set num_instr to the number of floating point instructions
// return -1.0 on error (something unmodelable)
TI_RES_COUNT* 
LOOP_MODEL::OP_Resources(WN* wn,
                         double* num_instr,
                         HASH_TABLE<WN*,BIT_VECTOR*>* invar_table)
{
  TI_RES_COUNT* resource_count = TI_RES_COUNT_Alloc(&Model_Local_Pool);
  if (OP_Resources_R(wn, resource_count, num_instr, invar_table) == -1) {
    return(NULL);
  }
  return(resource_count);
}

// Similar to above but apply it to the statements in the REGISTER_MODEL
// Another difference is that the loop init instruction is already
// included because we are not going to unroll anymore
double 
LOOP_MODEL::OP_Cycles(REGISTER_MODEL* rmodel,
                      double* num_instr,
                      HASH_TABLE<WN*,BIT_VECTOR*>* invar_table,
                      MEM_POOL* pool)
{
  TI_RES_COUNT* resource_count = TI_RES_COUNT_Alloc(pool);
  for (INT i = 0; i < rmodel->Num_Statements(); i++) {
    if (OP_Resources_R(rmodel->Statement(i),
                       resource_count,
                       num_instr,
                       invar_table) == -1) {
      return -1.0;
    }
  }
  LNOTARGET_Loop_Inc_Test_Res(resource_count);
  return TI_RES_COUNT_Min_Cycles(resource_count);
}


// Assume that INTCONST * INDEX_VARIABLE will be strength reduced
static BOOL
Multiply_Will_Be_Strength_Reduced(WN* wn)
{
  Is_True(WN_operator(wn) == OPR_MPY, ("Expected an OPR_MPY node"));
  WN* ldid;
  if (WN_operator(WN_kid0(wn)) == OPR_INTCONST) {
    ldid = WN_kid1(wn);
  }
  else if (WN_operator(WN_kid1(wn)) == OPR_INTCONST) {
    ldid = WN_kid0(wn);
  }
  else {
    return FALSE;
  }
  if (WN_operator(ldid) != OPR_LDID) {
    return FALSE;
  }
#ifdef TARG_X8664
  // multiply is always strength reduced if on of the operands is 
  // a constant.
  return TRUE;
#endif /* TARG_X8664 */
  for (WN* parent = ldid; parent; parent = LWN_Get_Parent(parent)) {
    if (WN_operator(parent) == OPR_DO_LOOP) {
      WN* idx_var = WN_index(parent);
      if (WN_st_idx(idx_var) == WN_st_idx(ldid)) {
        return TRUE;
      }
    }
  }
  return FALSE;
}


// mips4+ and IA-64 ISAs have multiply-adds
static inline BOOL
Target_ISA_Has_Madd()
{
  return (Is_Target_ISA_M4Plus() || Is_Target_ISA_I1Plus());
}


// walk the code, update resource_count and num_instr 
// for every floating point opcode
// par_v is the vertex number of the parent of wn, if any
// return -1 on error
INT 
LOOP_MODEL::OP_Resources_R(WN* wn,
                           TI_RES_COUNT* resource_count, 
                           double* num_instr,
                           HASH_TABLE<WN*,BIT_VECTOR*>* invar_table)
{
  TOP top;
  OPERATOR oper = WN_operator(wn);
  TYPE_ID rtype = WN_rtype(wn);
  TYPE_ID  desc = WN_desc(wn);
  
  if (OPERATOR_is_leaf(oper)) {
    return (1);
  }

//bug 12184: Don't know yet how to model the resource for vector type.
//           Skip for now. Note those must be GNU vectors at this point. 
#ifdef TARG_X8664
  if(MTYPE_is_vector(rtype) || MTYPE_is_vector(desc))
   return -1;
#endif 

  if (oper == OPR_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      if (OP_Resources_R(kid,resource_count,num_instr,invar_table) == -1) {
	return -1;
      }
      kid = WN_next(kid);
    }
    return 1;
  } 

  if (invar_table) { // no cost to invariant expressions
    BIT_VECTOR* bv = invar_table->Find(wn);
    if (bv && bv->Pop_Count()) {
      return 1;
    }
  }

  if (oper == OPR_CVT   || 
      oper == OPR_RND   ||
      oper == OPR_CEIL  || 
      oper == OPR_TRUNC || 
#ifdef TARG_X8664
      // skip float to float floor
      oper == OPR_FLOOR &&
        !(desc == MTYPE_F4 && rtype == MTYPE_F4) &&
        !(desc == MTYPE_F8 && rtype == MTYPE_F8)
#else
      oper == OPR_FLOOR
#endif
      ) {
    if (OP_Cycles_Cvt(WN_opcode(wn), resource_count, num_instr) == -1) {
    	return -1;
    }
  } 
  else if (oper == OPR_INTRINSIC_OP) {
    if (FP_Cycles_Intrinsic(wn, resource_count, num_instr) == -1) {
      return -1;
    }
  } 
  else if (oper == OPR_REALPART || 
           oper == OPR_IMAGPART ||
           oper == OPR_PAREN    || 
           oper == OPR_PARM) {
    // no-ops
  } 
  else if (OPERATOR_is_expression(oper) && 
           !OPERATOR_is_load(oper) &&
           oper != OPR_CONST) {
    // an fp expression
    if (desc  == MTYPE_FQ || 
        desc  == MTYPE_CQ || 
        rtype == MTYPE_FQ ||
        rtype == MTYPE_CQ) {
      return -1;
    } 
    // regular floating point
    else if (desc  == MTYPE_F4 || 
             desc  == MTYPE_F8 ||
#if defined(TARG_IA64) || defined(TARG_X8664)
             desc  == MTYPE_F10 ||
             rtype  == MTYPE_F10 ||
#endif
             rtype == MTYPE_F4 ||
             rtype == MTYPE_F8) {
      // multiply-adds
      if (Target_ISA_Has_Madd() && 
          (oper == OPR_ADD || oper == OPR_SUB) && 
          (WN_operator(WN_kid0(wn)) == OPR_MPY || 
           WN_operator(WN_kid1(wn)) == OPR_MPY)) { 
        return FP_Cycles_Madd(wn, resource_count, num_instr, invar_table);
      } 
      else if (oper == OPR_MAX || oper == OPR_MIN) {
        *num_instr += LNOTARGET_FP_Min_Max_Res(resource_count, rtype);
      } 
      else if (oper == OPR_SQRT) {
        *num_instr += LNOTARGET_FP_Sqrt_Res(resource_count, rtype);
      } 
      else if ((top = LNOTARGET_Whirl_To_Top(wn)) != TOP_UNDEFINED) {
        *num_instr += 1.0;
	TI_RES_COUNT_Add_Op_Resources(resource_count, top);
      }
      else if (oper == OPR_DIV) {
        *num_instr += LNOTARGET_FP_Div_Res(resource_count, rtype);
      } 
      else if (oper == OPR_RECIP) {
        *num_instr += LNOTARGET_FP_Recip_Res(resource_count, rtype);
      } 
      else if (oper == OPR_RSQRT
#ifdef TARG_X8664
	       || oper == OPR_ATOMIC_RSQRT
#endif
	      ) {
        *num_instr += LNOTARGET_FP_Rsqrt_Res(resource_count, rtype);
      } 
#ifdef TARG_X8664
      else if (oper == OPR_FLOOR) {
        *num_instr += LNOTARGET_FP_Floor_Res(resource_count, rtype);
      } 
      else if (oper == OPR_SELECT) {
        *num_instr += LNOTARGET_Fp_Select_Res(resource_count, rtype);
      }
      else if (OPCODE_is_compare(WN_opcode(wn))) {
	*num_instr += LNOTARGET_Fp_Compare_Res(resource_count, rtype);
      }
#endif
      else {
        return -1;
      }
    } 
    else if (desc  == MTYPE_C4 || 
             desc  == MTYPE_C8 ||
#if defined(TARG_IA64)
             desc  == MTYPE_C10 ||
             rtype  == MTYPE_C10 ||
#endif
             rtype == MTYPE_C4 ||
             rtype == MTYPE_C8) {
      if (oper == OPR_ADD || oper== OPR_SUB) {
        *num_instr += LNOTARGET_Complex_Add_Res(resource_count, rtype);
      } 
      else if (oper == OPR_MPY)  {
        *num_instr += LNOTARGET_Complex_Mult_Res(resource_count, rtype);

        // Count the number of "true" complex multiplies (no zeros)
        // because they require additional FP temp registers
        WN* kid0 = WN_kid0(wn);
        WN* kid1 = WN_kid1(wn);
        if ((WN_operator(kid0) != OPR_COMPLEX 
             || ((WN_operator(WN_kid0(kid0)) != OPR_CONST 
                  || !Targ_Is_Zero(STC_val(WN_st(WN_kid0(kid0))))) 
                 && (WN_operator(WN_kid1(kid0)) != OPR_CONST 
                     || !Targ_Is_Zero(STC_val(WN_st(WN_kid1(kid0)))))))
            &&
            (WN_operator(kid1) != OPR_COMPLEX 
             || ((WN_operator(WN_kid0(kid1)) != OPR_CONST 
                  || !Targ_Is_Zero(STC_val(WN_st(WN_kid0(kid1))))) 
                 && (WN_operator(WN_kid1(kid1)) != OPR_CONST 
                     || !Targ_Is_Zero(STC_val(WN_st(WN_kid1(kid1))))))))
        {
          Num_Complex_Mpy++;
        }
      } 
      else if (oper == OPR_NEG) {
        *num_instr += LNOTARGET_Complex_Neg_Res(resource_count, rtype);
      }
      else if (oper == OPR_COMPLEX)  {
	; // not really a floating-point op
      } 
      else {
	return -1;
      }

    } 
    else if (desc  == MTYPE_B  || 
             desc  == MTYPE_I1 ||
             desc  == MTYPE_I2 ||
             desc  == MTYPE_I4 || 
             desc  == MTYPE_I8 || 
             desc  == MTYPE_U1 || 
             desc  == MTYPE_U2 || 
             desc  == MTYPE_U4 || 
             desc  == MTYPE_U8 ||
             rtype == MTYPE_B  || 
             rtype == MTYPE_I1 || 
             rtype == MTYPE_I2 ||
             rtype == MTYPE_I4 || 
             rtype == MTYPE_I8 || 
             rtype == MTYPE_U1 || 
             rtype == MTYPE_U2 || 
             rtype == MTYPE_U4 || 
             rtype == MTYPE_U8) {

      BOOL double_word = (MTYPE_is_double(desc) || MTYPE_is_double(rtype));

      switch (oper) {
        case OPR_ARRAY: 
          return 1;
        case OPR_INTRINSIC_OP: 
          return -1;
        case OPR_TAS: 
          (*num_instr)++; 
          break;
#ifdef TARG_X8664
        case OPR_SELECT:
          *num_instr += LNOTARGET_Int_Select_Res(resource_count, rtype);
          break;
#elif defined(TARG_LOONGSON)
        case OPR_SELECT:
          *num_instr += LNOTARGET_Int_Select_Res(resource_count, double_word);
	break;
#else
        case OPR_SELECT:
          *num_instr += LNOTARGET_Int_Select_Res(resource_count);
          break;
#endif /* TARG_X8664 */
        case OPR_CVTL:
#ifdef TARG_X8664
          *num_instr += LNOTARGET_Int_Cvtl_Res(resource_count, rtype, 
					       WN_cvtl_bits(wn));
#elif defined(TARG_LOONGSON)
          *num_instr += LNOTARGET_Int_Cvtl_Res(resource_count, desc, rtype);
#else
          *num_instr += LNOTARGET_Int_Cvtl_Res(resource_count);
#endif /* TARG_X8664 */
          break;
        case OPR_NEG: 
          *num_instr += LNOTARGET_Int_Neg_Res(resource_count, double_word);
          break;
        case OPR_ABS:
#ifdef TARG_LOONGSON
          *num_instr += LNOTARGET_Int_Abs_Res(resource_count, double_word, rtype);
#else
          *num_instr += LNOTARGET_Int_Abs_Res(resource_count, double_word);
#endif //TARG_LOONGSON
          break;
        case OPR_PAREN: 
          break;
#ifdef TARG_X8664
        case OPR_BNOT: 
          *num_instr += LNOTARGET_Int_Bnot_Res(resource_count, rtype);
          break;
        case OPR_LNOT: 
          *num_instr += LNOTARGET_Int_Lnot_Res(resource_count, rtype);
          break;
#else
        case OPR_BNOT: 
          *num_instr += LNOTARGET_Int_Bnot_Res(resource_count);
          break;
        case OPR_LNOT: 
          *num_instr += LNOTARGET_Int_Lnot_Res(resource_count);
          break;
#endif /* TARG_X8664 */
        case OPR_MPY: 
#ifdef TARG_X8664
	  // we always strength reduce if one of the operands is an integer constant.
	  if (WN_operator_is(WN_kid0(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Mult_Str_Red_Res(resource_count, 
							 rtype, 
							 WN_const_val(WN_kid0(wn)));
	  else if (WN_operator_is(WN_kid1(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Mult_Str_Red_Res(resource_count, 
							 rtype, 
							 WN_const_val(WN_kid1(wn)));
	  else
            *num_instr += LNOTARGET_Int_Mult_Res(resource_count, double_word);
	  break;
#else						 
          if (!Multiply_Will_Be_Strength_Reduced(wn)) {
            *num_instr += LNOTARGET_Int_Mult_Res(resource_count, double_word);
            break;
          }
#endif /* TARG_X8664 */
          // if multiply will be strength reduced
          // fall through to OPR_ADD
        case OPR_ADD: 
          *num_instr += LNOTARGET_Int_Add_Res(resource_count, double_word);
          break;
        case OPR_SUB:  
          *num_instr += LNOTARGET_Int_Sub_Res(resource_count, double_word);
          break;
#ifdef TARG_X8664
        case OPR_DIV: 
	  if (WN_operator_is(WN_kid1(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Div_Str_Red_Res(resource_count, 
							rtype, 
							WN_const_val(WN_kid1(wn)));
	  else
	    *num_instr += LNOTARGET_Int_Div_Res(resource_count, rtype);
          break;
        case OPR_MOD: 
	  if (WN_operator_is(WN_kid1(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Mod_Str_Red_Res(resource_count, 
							rtype, 
							WN_const_val(WN_kid1(wn)));
	  else
	    *num_instr += LNOTARGET_Int_Mod_Res(resource_count, rtype);
          break;
        case OPR_REM: 
	  if (WN_operator_is(WN_kid1(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Rem_Str_Red_Res(resource_count, 
							rtype, 
							WN_const_val(WN_kid1(wn)));
	  else
	    *num_instr += LNOTARGET_Int_Rem_Res(resource_count, rtype);
          break;
        case OPR_DIVREM: 
	  if (WN_operator_is(WN_kid1(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_DivRem_Str_Red_Res(resource_count, 
							   rtype, 
							   WN_const_val(WN_kid1(wn)));
	  else
	    *num_instr += LNOTARGET_Int_DivRem_Res(resource_count, rtype);
          break;
        case OPR_MAX:
          *num_instr += LNOTARGET_Int_Min_Res(resource_count, rtype);
	  break;
        case OPR_MIN:
          *num_instr += LNOTARGET_Int_Max_Res(resource_count, rtype);
	  break;
        case OPR_MINMAX:
          *num_instr += LNOTARGET_Int_Min_Max_Res(resource_count, rtype);
          break;
        case OPR_BAND: 
	  if (WN_operator_is(WN_kid0(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Band_Str_Red_Res(resource_count, 
							 rtype, 
							 WN_const_val(WN_kid0(wn)));
	  else if (WN_operator_is(WN_kid1(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Band_Str_Red_Res(resource_count, 
							 rtype, 
							 WN_const_val(WN_kid1(wn)));	  
	  else
	    *num_instr += LNOTARGET_Int_Band_Res(resource_count, rtype);
          break;
        case OPR_BIOR: 
	  if (WN_operator_is(WN_kid0(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Bior_Str_Red_Res(resource_count, 
							 rtype, 
							 WN_const_val(WN_kid0(wn)));
	  else if (WN_operator_is(WN_kid1(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Bior_Str_Red_Res(resource_count, 
							 rtype, 
							 WN_const_val(WN_kid1(wn)));	  
	  else
	    *num_instr += LNOTARGET_Int_Bior_Res(resource_count, rtype);
          break;
        case OPR_BNOR: 
          *num_instr += LNOTARGET_Int_Bnor_Res(resource_count, rtype);
          break;
        case OPR_BXOR: 
	  if (WN_operator_is(WN_kid0(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Bxor_Str_Red_Res(resource_count, 
							 rtype, 
							 WN_const_val(WN_kid0(wn)));
	  else if (WN_operator_is(WN_kid1(wn), OPR_INTCONST))
	    *num_instr += LNOTARGET_Int_Bxor_Str_Red_Res(resource_count, 
							 rtype, 
							 WN_const_val(WN_kid1(wn)));	  
	  else
	    *num_instr += LNOTARGET_Int_Bxor_Res(resource_count, rtype);
          break;
        case OPR_LAND: 
          *num_instr += LNOTARGET_Int_Land_Res(resource_count, rtype);
          break;
        case OPR_CAND: 
          *num_instr += LNOTARGET_Int_Cand_Res(resource_count, rtype);
          break;
        case OPR_LIOR: 
          *num_instr += LNOTARGET_Int_Lior_Res(resource_count, rtype);
          break;
        case OPR_CIOR: 
          *num_instr += LNOTARGET_Int_Cior_Res(resource_count, rtype);
          break;
#else
        case OPR_DIV: 
#if !(defined(TARG_MIPS) || defined(TARG_PPC32))
          *num_instr += LNOTARGET_Int_Div_Res(resource_count, double_word);
#else
          *num_instr += LNOTARGET_Int_Div_Res(resource_count, double_word, 
					      MTYPE_signed(WN_desc(wn)));
#endif
          break;
        case OPR_MOD: 
          *num_instr += LNOTARGET_Int_Mod_Res(resource_count, double_word);
          break;
        case OPR_REM: 
          *num_instr += LNOTARGET_Int_Rem_Res(resource_count, double_word);
          break;
        case OPR_DIVREM: 
          *num_instr += LNOTARGET_Int_DivRem_Res(resource_count, double_word);
          break;
        case OPR_MAX:
        case OPR_MIN:
        case OPR_MINMAX:
#ifdef TARG_LOONGSON
          *num_instr += LNOTARGET_Int_Min_Max_Res(resource_count,
                                                  oper == OPR_MINMAX, double_word);
#else
          *num_instr += LNOTARGET_Int_Min_Max_Res(resource_count,
                                                  oper == OPR_MINMAX);
#endif //TARG_LOONGSON
          break;
        case OPR_BAND: 
          *num_instr += LNOTARGET_Int_Band_Res(resource_count);
          break;
        case OPR_BIOR: 
          *num_instr += LNOTARGET_Int_Bior_Res(resource_count);
          break;
        case OPR_BNOR: 
          *num_instr += LNOTARGET_Int_Bnor_Res(resource_count);
          break;
        case OPR_BXOR: 
          *num_instr += LNOTARGET_Int_Bxor_Res(resource_count);
          break;
        case OPR_LAND: 
          *num_instr += LNOTARGET_Int_Land_Res(resource_count);
          break;
        case OPR_CAND: 
          *num_instr += LNOTARGET_Int_Cand_Res(resource_count);
          break;
        case OPR_LIOR: 
          *num_instr += LNOTARGET_Int_Lior_Res(resource_count);
          break;
        case OPR_CIOR: 
          *num_instr += LNOTARGET_Int_Cior_Res(resource_count);
          break;
#endif /* TARG_X8664 */
        case OPR_SHL: 
          *num_instr += LNOTARGET_Int_Shl_Res(resource_count, double_word); 
          break;
        case OPR_ASHR: 
          *num_instr += LNOTARGET_Int_Ashr_Res(resource_count, double_word); 
          break;
        case OPR_LSHR: 
          *num_instr += LNOTARGET_Int_Lshr_Res(resource_count, double_word); 
          break;
#ifdef TARG_X8664
        case OPR_EQ:  
          *num_instr += LNOTARGET_Int_Eq_Res(resource_count, desc); 
          break;
        case OPR_NE:  
          *num_instr += LNOTARGET_Int_Ne_Res(resource_count, desc); 
          break;
        case OPR_GT:  
          *num_instr += LNOTARGET_Int_Gt_Res(resource_count, desc); 
          break;
        case OPR_GE: 
          *num_instr += LNOTARGET_Int_Ge_Res(resource_count, desc); 
          break;
        case OPR_LT: 
          *num_instr += LNOTARGET_Int_Lt_Res(resource_count, desc); 
          break;
        case OPR_LE: 
          *num_instr += LNOTARGET_Int_Le_Res(resource_count, desc); 
          break;
#else
        case OPR_EQ:  
          *num_instr += LNOTARGET_Int_Eq_Res(resource_count); 
          break;
        case OPR_NE:  
          *num_instr += LNOTARGET_Int_Ne_Res(resource_count); 
          break;
        case OPR_GT:  
          *num_instr += LNOTARGET_Int_Gt_Res(resource_count); 
          break;
        case OPR_GE: 
          *num_instr += LNOTARGET_Int_Ge_Res(resource_count); 
          break;
        case OPR_LT: 
          *num_instr += LNOTARGET_Int_Lt_Res(resource_count); 
          break;
        case OPR_LE: 
          *num_instr += LNOTARGET_Int_Le_Res(resource_count); 
          break;
#endif /* TARG_X8664 */
        case OPR_LDA:
          *num_instr += LNOTARGET_Int_Lda_Res(resource_count); 
          break;
      case OPR_INTCONST: 
        break;
      default: 
        DevWarn("Unknown whirl in LOOP_MODEL::OP_Resources_R");
        return -1;
      }
    }
  }

  for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    WN* kid = WN_kid(wn,kidno);
    if (OP_Resources_R(kid, resource_count, num_instr, invar_table) == -1) {
      return -1;
    }
  }
  return 1;
}

// deal with a floating point add/subtract with a multiply kid
// we're assuming this will later be turned in to a madd
// on entry, wn points to the fp add or subtract
// return -1 on error
INT 
LOOP_MODEL::FP_Cycles_Madd(WN* wn, 
                           TI_RES_COUNT* resource_count, 
                           double* num_fp_instr,
                           HASH_TABLE<WN*,BIT_VECTOR*>* invar_table)
{
  // first add the resources of the madd
  *num_fp_instr += LNOTARGET_FP_Madd_Res(resource_count, WN_rtype(wn));

  WN *kid0 = WN_kid0(wn);
  WN *kid1 = WN_kid1(wn);

  WN *non_mult_kid;
  WN *mult_kid0;
  WN *mult_kid1;

  // now find the resources of the three kids (the non-multiply kid
  // of the add/subtract and the two kids of the multiply
  if (WN_operator(kid0) == OPR_MPY) {
    non_mult_kid = kid1;
    mult_kid0 = WN_kid0(kid0);
    mult_kid1 = WN_kid1(kid0);
  } 
  else {
    non_mult_kid = kid0;
    mult_kid0 = WN_kid0(kid1);
    mult_kid1 = WN_kid1(kid1);
  }

  if (OP_Resources_R(non_mult_kid,
                     resource_count,
                     num_fp_instr,
                     invar_table) == -1) {
    return -1;
  }

  if (OP_Resources_R(mult_kid0,
                     resource_count,
                     num_fp_instr,
                     invar_table) == -1) {
    return -1;
  }
  if (OP_Resources_R(mult_kid1,
                     resource_count,
                     num_fp_instr,
                     invar_table) == -1) {
    return -1;
  }
  return 1;
}

// Deal with a cvt
// return -1 on error (something we can't handle)
INT 
LOOP_MODEL::OP_Cycles_Cvt(OPCODE opcode, 
                          TI_RES_COUNT* resource_count, 
                          double* num_fp_instr)
{
  double instr = LNOTARGET_Cvt_Res(resource_count, opcode);
  if (instr < 0.1) {
    return -1;
  }
  *num_fp_instr += instr;
  return 1;
}

// Deal with an intrinsic
// Currently we only handle exponentiation by a small number
// return -1 on error (something we can't handle)
INT 
LOOP_MODEL::FP_Cycles_Intrinsic(WN* wn, 
                                TI_RES_COUNT* resource_count, 
                                double* num_fp_instr)
{
  if (WN_kid_count(wn) != 2) {
    return -1;
  }
  WN *const_kid = WN_kid1(wn);
  if (WN_operator(const_kid) == OPR_PARM) {
    const_kid = WN_kid0(const_kid);
  }
  if (WN_operator(const_kid) != OPR_INTCONST) {
    return -1;
  }
  INT num_multiplies = WN_const_val(const_kid) - 1;
  if (num_multiplies == 0) {
    return 1; // noop
  }
  if (num_multiplies < 0 || num_multiplies > 3) {
    return -1;
  }
  double instr = LNOTARGET_FP_Exp_Res(resource_count,
                                      (INTRINSIC) WN_intrinsic(wn),
                                      num_multiplies);
  if (instr < 0.1) {
    return -1;
  }
  *num_fp_instr += instr;
  return 1;
}


// Is this node lexically before node2
BOOL 
ARRAY_REF_NODE::Lexically_Before(ARRAY_REF_NODE *node2)
{
  INT num_loops = Array->Dim(0)->Nest_Depth();
  for (INT i=0; i<num_loops; i++) {
    if (_unroll_copy[i] < node2->_unroll_copy[i]) return TRUE;
  }
  return (_lex_number < node2->_lex_number);
}

void 
ARRAY_REF_LIST::Print(FILE *fp) const
{
  fprintf(fp,"The base array is \"");
  Base_Array->Print(fp);
  if (_is_scalar_expanded) fprintf(fp," (scalar expanded) ");
  fprintf(fp,"\" and the references are \n");
  ARRAY_REF_CONST_ITER iter(this);
  const ARRAY_REF_NODE *first = iter.First();
  for (const ARRAY_REF_NODE *n = first; !iter.Is_Empty(); n = iter.Next()) {
    fprintf(fp, "    ");
    n->Print(fp);
  }
}


// delete a list, including every node on it
// this assumes that all elements are from the same mempool
ARRAY_REF_LIST::~ARRAY_REF_LIST()
{
  MEM_POOL_Set_Default(_pool);
  while (!Is_Empty())
    CXX_DELETE(Remove_Headnode(),_pool);
}


ARRAY_REF_LIST::ARRAY_REF_LIST(ARRAY_REF_LIST *orig, MEM_POOL *pool)
{
  _pool = pool;
  _is_scalar_expanded = orig->_is_scalar_expanded;
  Base_Array = orig->Base_Array;
  ARRAY_REF_ITER iter(orig);
  for (ARRAY_REF_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
	Append(CXX_NEW(ARRAY_REF_NODE(node,pool),pool));
  }
}

void 
ARRAY_REF_LIST::Remove_Invariants(INT loopno)
{
  ARRAY_REF_ITER iter(this);
  ARRAY_REF_NODE *first = iter.First();
  ARRAY_REF_NODE *prev_node = NULL;
  ARRAY_REF_NODE *next_node = NULL;
  for (ARRAY_REF_NODE *node=first; node; node = next_node) {
    next_node = iter.Next();
    ACCESS_ARRAY *array = node->Array;
    BOOL is_invar = TRUE;
    for (INT i=0; i<array->Num_Vec(); i++) {
      ACCESS_VECTOR *av = array->Dim(i);
      if ((av->Non_Const_Loops() > loopno) || (av->Loop_Coeff(loopno) != 0)) {
	is_invar = FALSE;
      }
    }
    if (is_invar) {
      Remove(prev_node,node);
    } else {
      prev_node = node;
    }
  }
}

// are array and array2 cses or duplicates given that loop inner is inner and
// that we consider a cse anything within a distance max_dist
// step is the step size of the loop.  Zero implies its not constant
// if they are exact duplicates, set *is_dup to TRUE
// if we return TRUE, *min_inner_offset and *max_inner_offset are set
// to the min/max offset in dimensions that use the inner loop variable
// these are normalized by the step and inner loop mulitplier
// if we don't return TRUE, they are undefined
static BOOL 
Cse_Or_Dup(ACCESS_ARRAY* array, ACCESS_ARRAY* array2,
           INT inner, INT max_dist, INT step, 
           BOOL* is_dup, mINT16* max_inner_offset, mINT16* min_inner_offset) 
{
  *min_inner_offset = INT16_MAX;
  *max_inner_offset = INT16_MIN;
  if(!step) return((*array) == (*array2));

  if (array->Too_Messy || array2->Too_Messy) return(FALSE);
  if (array->Num_Vec() != array2->Num_Vec()) return(FALSE);
  BOOL seen_mult = FALSE;
  INT diff=0;
  for (INT i=0; i<array->Num_Vec(); i++) {
    ACCESS_VECTOR *av1 = array->Dim(i);
    ACCESS_VECTOR *av2 = array2->Dim(i);
    if (av1->Too_Messy || av2->Too_Messy) return(FALSE);
    if (av1->Nest_Depth() != av2->Nest_Depth()) return(FALSE);

    INT dist = av1->Const_Offset - av2->Const_Offset;

    // short circuit to get const reference case quickly, ie a[0] vrs a[1]
    if (!av1->Has_Loop_Coeff() || !av2->Has_Loop_Coeff()) {
      if (dist) return(FALSE);
    }

    // the symbols must be equal
    if (av1->Lin_Symb != NULL && !av1->Lin_Symb->Is_Empty()) { // av1 has a symb
      if (av2->Lin_Symb == NULL || av2->Lin_Symb->Is_Empty() ||
		  !(*av1->Lin_Symb == *av2->Lin_Symb)) {
	return(FALSE);
      }
    } else if (av2->Lin_Symb != NULL && !av2->Lin_Symb->Is_Empty()) {
      return(FALSE);
    }
    if (av1->Non_Lin_Symb != NULL && !av1->Non_Lin_Symb->Is_Empty()) {
      if (av2->Non_Lin_Symb == NULL || av2->Non_Lin_Symb->Is_Empty() ||
	!(*av1->Non_Lin_Symb == *av2->Non_Lin_Symb)) {
	return(FALSE);
      }
    } else if (av2->Non_Lin_Symb != NULL && !av2->Non_Lin_Symb->Is_Empty()) {
      return(FALSE);
    }

    // Now check the induction variables
    for (INT i=0; i<av1->Nest_Depth(); i++) {
      if (av1->Loop_Coeff(i) != av2->Loop_Coeff(i)) {
	return(FALSE);
      }
    }
    INT mult = av1->Loop_Coeff(inner);
    if (mult) {
      if (abs(dist) > abs(step*mult*max_dist)) {
	return(FALSE); // too far to be a cse
      }
      if ((dist % (step*mult)) != 0) {
	return(FALSE); // independent
      } 
      INT this_diff = (dist / (step*mult));
      if (seen_mult && (this_diff != diff)) {  // contradictory coupling
	return(FALSE);
      }
      seen_mult = TRUE;
      diff = this_diff;
      *max_inner_offset = MAX(*max_inner_offset,
		MAX(av1->Const_Offset,av2->Const_Offset)/(step*mult));
      *min_inner_offset = MIN(*min_inner_offset,
		MIN(av1->Const_Offset,av2->Const_Offset)/(step*mult));
    } else if (dist != 0) return(FALSE);
  }
  *is_dup = (diff == 0);
  return(TRUE);
}


// Remove the cses and duplicates 
// Mark the remaining copy as a cse or duplicate 
// Union _has_store  and _has_load
// _first_ref_store is set to the _first_ref_store of the earlier ref
void 
ARRAY_REF_LIST::Remove_Cse(INT inner, INT max_dist, INT step)
{
  mINT16 max_inner_offset, min_inner_offset;
  ARRAY_REF_ITER iter(this);
  for (ARRAY_REF_NODE *node=iter.First(); node; node = iter.Next()) {
    ACCESS_ARRAY *array = node->Array;

    // loop through all possible later nodes, removing duplicates
    ARRAY_REF_ITER iter2(node);
    ARRAY_REF_NODE *first = iter2.First();
    first = iter2.Next(); // first is one after node
    ARRAY_REF_NODE *prev_node = node;
    ARRAY_REF_NODE *next_node = NULL;
    for (ARRAY_REF_NODE *node2=first; node2; node2 = next_node) {
      next_node = iter2.Next();
      ACCESS_ARRAY *array2 = node2->Array;
      BOOL is_dup;
      if (Cse_Or_Dup(array,array2,inner,max_dist,step,&is_dup,
		&max_inner_offset,&min_inner_offset)) {
	if (is_dup) {
	  node->_is_dup = TRUE;
	  if (node2->_has_dup_loads || 
	      (node->_has_load && node2->_has_load)) {
	    node->_has_dup_loads = TRUE;
          }
	} else {
	  node->_is_cse = TRUE;
	  node->_max_inner_offset = 
		MAX(node->_max_inner_offset,max_inner_offset);
	  node->_min_inner_offset = 
		MIN(node->_min_inner_offset,min_inner_offset);
	}
	node->_has_store |= node2->_has_store;
	node->_has_load |= node2->_has_load;
	if (node->_first_ref_store != node2->_first_ref_store) {
	  if (node2->Lexically_Before(node)) {
	    node->_first_ref_store = node2->_first_ref_store;
	  }
	}
	Remove(prev_node,node2);
      } else {
        prev_node = node2;
      }
    }
  }
}

void 
ARRAY_REF_LIST::Unroll(INT loop_no, INT num_copies)
{
  ARRAY_REF_ITER iter(this);
  ARRAY_REF_NODE *next_node = NULL;
  for (ARRAY_REF_NODE *node=iter.First(); node; node = next_node) {
    next_node = node->Next();
    ACCESS_ARRAY *array = node->Array;

    // Does this reference vary with loop_no
    // Note that we ignore symbolics, if they vary in a weird way
    // we won't be able to unroll anyway
    BOOL varies = FALSE;
    if (array->Too_Messy) varies = TRUE;
    for (INT i=0; i<array->Num_Vec() && !varies; i++) {
      ACCESS_VECTOR *av = array->Dim(i);
      if (av->Too_Messy || (av->Loop_Coeff(loop_no) != 0)) {
	varies = TRUE;
      }
    }

    if (!varies) {
      node->_is_dup = TRUE;
      if (node->_has_load) node->_has_dup_loads = TRUE;
    } else {  // do the duplication
      INT orig_copy_num = node->_unroll_copy[loop_no];
      for (INT i=num_copies-1; i>=0; i--) {
	if (i != 0) { // create a copy
	  ARRAY_REF_NODE *new_node=CXX_NEW(ARRAY_REF_NODE(node,_pool),_pool);
	  if (orig_copy_num) {
	    new_node->_unroll_copy[loop_no] = orig_copy_num*num_copies+i;
          } else {
	    new_node->_unroll_copy[loop_no] = i;
          }
	  array = new_node->Array;
	  Prepend(new_node,node);
	} else {
	  array = node->Array;
	  if (orig_copy_num) {
	    node->_unroll_copy[loop_no] = orig_copy_num*num_copies+i;
          } else {
	    node->_unroll_copy[loop_no] = i;
          }
	}
	for (INT j=0; j<array->Num_Vec(); j++) {
	  ACCESS_VECTOR *av = array->Dim(j);
	  if (!av->Too_Messy) {
	    INT mult = av->Loop_Coeff(loop_no);
	    if (mult) {
	      av->Const_Offset += mult*i;
	      av->Set_Loop_Coeff(loop_no,num_copies*mult);
            }
          }
	}
      }
    }
  }
}


void 
ARRAY_REF_LIST::Mark_Invariants(INT loopno)
{
  ARRAY_REF_ITER iter(this);
  ARRAY_REF_NODE *first = iter.First();
  ARRAY_REF_NODE *next_node = NULL;
  for (ARRAY_REF_NODE *node=first; node; node = next_node) {
    next_node = iter.Next();
    ACCESS_ARRAY *array = node->Array;
    BOOL is_invar = TRUE;
    for (INT i=0; i<array->Num_Vec(); i++) {
      ACCESS_VECTOR *av = array->Dim(i);
      if ((av->Non_Const_Loops() > loopno) || (av->Loop_Coeff(loopno) != 0)) {
	is_invar = FALSE;
      }
    }
    node->_is_invariant = is_invar;
  }
}

// How many invariants are there
INT 
ARRAY_REF_LIST::Num_Invariants(INT loopno)
{
  INT result=0;
  Mark_Invariants(loopno);

  ARRAY_REF_ITER iter(this);
  for (ARRAY_REF_NODE *node=iter.First(); node; node = iter.Next()) {
    if (node->_is_invariant) result++;
  }
  return result;
}

// How many fp references are there
INT 
ARRAY_REF_LIST::Num_Fp_Refs() const
{
  INT result=0;

  if (_is_scalar_expanded) {
    if (MTYPE_float(Base_Array->Type))
      result += this->Len();
  } else {
    if (MTYPE_float(WN_desc(LWN_Get_Parent(Head()->Wn))) ||
        MTYPE_float(WN_rtype(LWN_Get_Parent(Head()->Wn))))
      result += this->Len();
  }
  return result;
}

// How many int references are there
INT 
ARRAY_REF_LIST::Num_Int_Refs() const
{
  INT result=0;

  if (_is_scalar_expanded) {
    if (!MTYPE_float(Base_Array->Type))
      result += this->Len();
  } else {
    if (!MTYPE_float(WN_desc(LWN_Get_Parent(Head()->Wn))) &&
        !MTYPE_float(WN_rtype(LWN_Get_Parent(Head()->Wn))))
      result += this->Len();
  }
  return result;
}

void 
ARRAY_REF_LIST::Calc_Regs_And_Refs(INT* num_fp_regs, 
                                   INT* num_fp_refs,
                                   INT* num_fp_variant_stores, 
                                   INT* num_fp_invariant_stores,
                                   INT* num_int_regs, 
                                   INT* num_int_refs,
                                   INT* num_int_variant_stores, 
                                   INT* num_int_invariant_stores)
{
  INT fp_regs = 0;
  INT fp_refs = 0;
  INT fp_variant_stores = 0;
  INT fp_invariant_stores = 0;
  INT int_regs = 0;
  INT int_refs = 0;
  INT int_variant_stores = 0;
  INT int_invariant_stores = 0;

  ARRAY_REF_ITER iter(this);
  for (ARRAY_REF_NODE* node=iter.First(); node; node = iter.Next()) {

    INT tmp_regs = 0;
    INT tmp_refs = 0;
    INT tmp_variant_stores = 0;
    INT tmp_invariant_stores = 0;
    INT tmp_base_regs = 0;

    if (!node->_is_invariant) { // assuming invariant will be moved out
      // compare this array ref against previous ones to see
      // if a new array base register is needed
      BOOL found = FALSE;
      ARRAY_REF_ITER iter1(this);
      for (ARRAY_REF_NODE* node1=iter1.First(); 
           node1 != node; 
           node1 = iter1.Next()) {
        ACCESS_ARRAY* ar  = node->Array;
        ACCESS_ARRAY* ar1 = node1->Array;
        if (ar->Too_Messy  || 
            ar1->Too_Messy || 
            ar->Num_Vec() != ar1->Num_Vec()) {
          continue;
        }
        BOOL need_diff_base = FALSE;
        for (INT i = 0; i < ar->Num_Vec(); i++) {
          ACCESS_VECTOR* av  = ar->Dim(i);
          ACCESS_VECTOR* av1 = ar1->Dim(i);
          if (av->Too_Messy || av1->Too_Messy) {
            need_diff_base=TRUE;
            break;
          }
          ACCESS_VECTOR* diff=Subtract(av, av1, _pool);
          if (!diff->Is_Const()) {
            need_diff_base=TRUE;
            break;
          } 
          else if (i < ar->Num_Vec()-1) {
            if (diff->Const_Offset != 0) {
              need_diff_base=TRUE;
              break;
            } 
            else {
              continue;
            }
          } 
          else if (diff->Const_Offset < 0x10000 &&
                 - diff->Const_Offset < 0x10000) {
            need_diff_base=FALSE;
            break;
          } 
          else {
            need_diff_base=TRUE;
            break;
          }
        }
        if (!need_diff_base) {
          found = TRUE;
          break;
        }
      }
      if (!found) {
        tmp_base_regs++;
      }
    }

    BOOL is_fp = FALSE;
    INT  regs_per_ref = 1;
    if (_is_scalar_expanded) {
      if (MTYPE_float(Base_Array->Type)) {
        is_fp = TRUE;
      }
      if (MTYPE_is_complex(Base_Array->Type)) {
        regs_per_ref *= 2;
      }
      if (MTYPE_is_quad(Base_Array->Type)) {
        regs_per_ref *= 2;
      }
    } 
    else {
      WN* parent = LWN_Get_Parent(node->Wn);
      if (MTYPE_float(WN_desc(parent)) ||
          MTYPE_float(WN_rtype(parent))) {
        is_fp = TRUE;
      }
      if (MTYPE_is_complex(WN_desc(parent)) ||
          MTYPE_is_complex(WN_rtype(parent))) {
        regs_per_ref *= 2;
      }
      if (MTYPE_is_quad(WN_desc(parent)) ||
          MTYPE_is_quad(WN_rtype(parent))) {
        regs_per_ref *= 2;
      } 
    }
    if (node->_is_invariant) {
      tmp_regs += regs_per_ref;
    } 
    else if (node->_is_cse) {  
      if (node->_max_inner_offset > node->_min_inner_offset) {
        tmp_regs += 
          MIN(Max_Cse_Dist,(node->_max_inner_offset-node->_min_inner_offset))
          * regs_per_ref;
      } 
      else {
	tmp_regs += regs_per_ref;
      }
      tmp_refs += regs_per_ref;
    } 
    else if (node->_is_dup) {  // a pure duplicate
      if (node->_has_dup_loads) {
        tmp_regs += regs_per_ref;
      }
      if (node->_has_store) {
	tmp_refs += regs_per_ref;
      }
      if (node->_has_load && !node->_first_ref_store) {
	tmp_refs += regs_per_ref;
      }
    } 
    else {
      tmp_refs += regs_per_ref;
    }
    if (node->_has_store) {
      if (node->_is_invariant) {
	tmp_invariant_stores += regs_per_ref;
      } 
      else {
	tmp_variant_stores += regs_per_ref;
      }
    }
    if (is_fp) {
      fp_regs += tmp_regs;
      fp_refs += tmp_refs;
      fp_invariant_stores += tmp_invariant_stores;
      fp_variant_stores += tmp_variant_stores;
    } 
    else {
      int_regs += tmp_regs;
      int_refs += tmp_refs;
      int_invariant_stores += tmp_invariant_stores;
      int_variant_stores += tmp_variant_stores;
    }
    int_regs += tmp_base_regs;
  }

  *num_fp_regs = fp_regs;
  *num_fp_refs = fp_refs;
  *num_fp_variant_stores = fp_variant_stores;
  *num_fp_invariant_stores = fp_invariant_stores;
  *num_int_regs = int_regs;
  *num_int_refs = int_refs;
  *num_int_variant_stores = int_variant_stores;
  *num_int_invariant_stores = int_invariant_stores;
}

// How many references of the maximal dimensionality (but
// at least 2) are invariant in some outer unrollable loop
INT 
ARRAY_REF_LIST::Conflict_Refs(INT max_dim, 
                              BOOL* can_be_unrolled, 
                              INT num_loops)
{
  INT result = 0;

  MEM_POOL_Push(&LNO_local_pool); 
  BOOL *invar = CXX_NEW_ARRAY(BOOL,num_loops,&LNO_local_pool);
  ARRAY_REF_ITER iter1(this);
  for (ARRAY_REF_NODE *node1=iter1.First(); node1; node1 = iter1.Next()) {
    if (!node1->_is_invariant) { // don't care about inner invariants
      ACCESS_ARRAY *array=node1->Array;
      UINT num_vec = array->Num_Vec();
      if (num_vec == max_dim) {
        INT i;
	for (i=0; i<num_loops; i++) { 
          invar[i] = can_be_unrolled[i];
        }
	BOOL too_messy = FALSE;
        for (i=0; i<num_vec; i++) {
          ACCESS_VECTOR *av = array->Dim(i);
	  if (!av->Too_Messy) {
            for (INT j=0; j<av->Nest_Depth(); j++) {
              if ((av->Non_Const_Loops() > j) || (av->Loop_Coeff(j) != 0)) {
	        invar[j] = FALSE;
              }
            }
          } else {
	    too_messy = TRUE;
          }
        }
	if (!too_messy) {
	  BOOL is_invar = FALSE;
          for (i=0; i<num_loops && !is_invar; i++) {
	    if (invar[i]) {
	      is_invar = TRUE;
            }
          }
	  if (is_invar) {
	    result++;
          }
        }
      }
    }
  }
  MEM_POOL_Pop(&LNO_local_pool); 
  return result;
}



ARRAY_REF::ARRAY_REF(ARRAY_REF *orig, MEM_POOL *pool) : _stack(pool)
{
  _pool = pool;
  _num_bad_fp = orig->_num_bad_fp;
  _num_bad_int = orig->_num_bad_int;
  for (INT i=0; i<orig->Elements(); i++) {
    Push(CXX_NEW(ARRAY_REF_LIST(orig->Array_Ref_List(i),pool),pool));
  }
}

void 
ARRAY_REF::Print(FILE *fp) const
{
  fprintf(fp,"The number of bad references is %d fp and %d int\n",
    Num_Fp_Bad(), Num_Int_Bad());
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Print(fp);
  }
}


INT 
ARRAY_REF::Num_Fp_Refs() const
{
  INT result = Num_Fp_Bad();
  for (INT i=0; i<Elements(); i++) {
    result += Array_Ref_List(i)->Num_Fp_Refs();
  }
  return result;
}

INT 
ARRAY_REF::Num_Int_Refs() const
{
  INT result = Num_Int_Bad();
  for (INT i=0; i<Elements(); i++) {
    result += Array_Ref_List(i)->Num_Int_Refs();
  }
  return result;
}

INT 
ARRAY_REF::Num_Invariants(INT loopno)
{
  INT result = 0;
  for (INT i=0; i<Elements(); i++) {
    result += Array_Ref_List(i)->Num_Invariants(loopno);
  }
  return result;
}

INT 
ARRAY_REF::Conflict_Refs(BOOL *can_be_unrolled, INT num_loops) 
{
  INT max_dim = 0;
  INT result = 0;
  for (INT i=0; i<Elements(); i++) {
    ARRAY_REF_ITER iter(Array_Ref_List(i));
    ARRAY_REF_NODE *node = iter.First();
    ACCESS_ARRAY *ar=node->Array;
    max_dim = MAX(max_dim,ar->Num_Vec());
  }
  if (max_dim >= 2) {
    for (INT i=0; i<Elements(); i++) {
      result += 
	Array_Ref_List(i)->Conflict_Refs(max_dim,can_be_unrolled,num_loops);
    }
  }
  return result;
}

void 
ARRAY_REF::Calc_Regs_And_Refs(INT* num_fp_regs, 
                              INT* num_fp_refs,
                              INT* num_fp_variant_stores, 
                              INT* num_fp_invariant_stores,
                              INT* num_int_regs, 
                              INT* num_int_refs,
                              INT* num_int_variant_stores, 
                              INT* num_int_invariant_stores)
{
  *num_fp_regs = 0;
  *num_fp_refs = Num_Fp_Bad();
  *num_fp_variant_stores = 0;
  *num_fp_invariant_stores = 0;
  *num_int_regs = 0;
  *num_int_refs = Num_Int_Bad();
  *num_int_variant_stores = 0;
  *num_int_invariant_stores = 0;
  for (INT i=0; i<Elements(); i++) {
    INT this_fp_refs = 0;
    INT this_fp_regs = 0;
    INT fp_variant_stores = 0;
    INT fp_invariant_stores = 0;
    INT this_int_refs = 0;
    INT this_int_regs = 0;
    INT int_variant_stores = 0;
    INT int_invariant_stores = 0;
    Array_Ref_List(i)->Calc_Regs_And_Refs(
	&this_fp_regs, &this_fp_refs, &fp_variant_stores, &fp_invariant_stores,
	&this_int_regs, &this_int_refs, &int_variant_stores,
		&int_invariant_stores);
    *num_fp_regs += this_fp_regs;
    *num_fp_refs += this_fp_refs;
    *num_fp_variant_stores += fp_variant_stores;
    *num_fp_invariant_stores += fp_invariant_stores;
    *num_int_regs += this_int_regs;
    *num_int_refs += this_int_refs;
    *num_int_variant_stores += int_variant_stores;
    *num_int_invariant_stores += int_invariant_stores;
  }
  // assuming each bad ref need one address reg
  *num_int_regs += (_num_bad_fp+_num_bad_int);
}


void 
ARRAY_REF::Remove_Invariants(INT loopno)
{
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Remove_Invariants(loopno);
  }
}

void 
ARRAY_REF::Remove_Cse(INT inner, INT max_dist, INT step)
{
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Remove_Cse(inner, max_dist, step);
  }
}

void 
ARRAY_REF::Mark_Invariants(INT loopno)
{
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Mark_Invariants(loopno);
  }
}

void 
ARRAY_REF::Unroll(INT loop_no, INT num_copies)
{
  _num_bad_fp *= num_copies;
  _num_bad_int *= num_copies;
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Unroll(loop_no,num_copies);
  }
}

static void 
Build_DLI_Stack(WN *wn, DLI_STACK *stack)
{
  if (wn) {
    Build_DLI_Stack(LWN_Get_Parent(wn), stack);
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
      stack->Push(dli);
    }
  }
}

// Is this access vector a weird triangular vector
// i.e. is there a term a[i] where i's bounds are triangular
// with a multiple > 5, ie.  do i = 6j,...
BOOL 
Weird_Triangular(ACCESS_VECTOR* av, DLI_STACK* dli_stack, INT SNL_Depth)
{
  for (INT i=0; i<av->Nest_Depth(); i++) {
    if (av->Loop_Coeff(i)) {
      ACCESS_ARRAY *array = dli_stack->Bottom_nth(i)->LB;
      INT j;
      for (j=0; j<array->Num_Vec(); j++) {
        ACCESS_VECTOR *bound = array->Dim(j);
        INT lb = av->Nest_Depth() - SNL_Depth; 
        for (INT k=lb; k<bound->Nest_Depth()-1; k++) {
	  if (abs(bound->Loop_Coeff(k)) > 5) {
	    return TRUE;
          }
        }
      }
      array = dli_stack->Bottom_nth(i)->UB;
      for (j=0; j<array->Num_Vec(); j++) {
        ACCESS_VECTOR *bound = array->Dim(j);
        INT lb = av->Nest_Depth() - SNL_Depth; 
        for (INT k=lb; k<bound->Nest_Depth()-1; k++) {
	  if (abs(bound->Loop_Coeff(k)) > 5) {
	    return TRUE;
          }
        }
      }
    }
  }
  return FALSE;
}

extern BOOL 
Is_Bad_Array(WN* wn_ref, INT nloops)
{
  OPCODE op = WN_opcode(wn_ref);
  OPERATOR opr = OPCODE_operator(op);
  if (!OPCODE_is_load(op) && !OPCODE_is_store(op))
    return FALSE;
  if (opr == OPR_LDID || opr == OPR_STID)
    return FALSE;
  WN* wn_array = OPCODE_is_load(op) ? WN_kid0(wn_ref) : WN_kid1(wn_ref);
  if (WN_operator(wn_array) != OPR_ARRAY)
    return FALSE;
  ACCESS_ARRAY* aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  WN* wn_base = WN_array_base(wn_array);
  if (WN_operator(wn_base) != OPR_LDA
      && WN_operator(wn_base) != OPR_LDID)
    return TRUE;
  if (aa == NULL || aa->Too_Messy
      || Do_Depth(wn_array) + 1 - aa->Non_Const_Loops() < nloops)
    return TRUE;
  DLI_STACK *dli_stack =
    CXX_NEW(DLI_STACK(&LNO_local_pool), &LNO_local_pool);   
  Build_DLI_Stack(wn_ref, dli_stack);
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR *av = aa->Dim(i);
    if (av->Too_Messy || av->Non_Lin_Symb
        || Weird_Triangular(av, dli_stack, nloops))
      return TRUE;
  }
  return FALSE;
}

void 
ARRAY_REF::Build(WN* wn, 
                 INT SNL_Depth,
                 HASH_TABLE<WN*,BIT_VECTOR*>* invar_table)
{
  DLI_STACK *dli_stack = CXX_NEW(DLI_STACK(_pool),_pool);
  Build_DLI_Stack(wn,dli_stack);
  Build_Rec(wn,dli_stack,SNL_Depth,invar_table);
  CXX_DELETE(dli_stack,_pool);
}

void 
ARRAY_REF::Build_Rec(WN* wn, 
                     DLI_STACK* dli_stack, 
                     INT SNL_Depth,
                     HASH_TABLE<WN*,BIT_VECTOR*>* invar_table)
{
  if (!wn) return;

  OPCODE opcode = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opcode);

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Build(kid, SNL_Depth,invar_table);
      kid = WN_next(kid);
    }
    return;
  } else if (opcode == OPC_DO_LOOP) {
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
    dli_stack->Push(dli);
  }

  if (invar_table && !OPCODE_is_load(opcode) && !OPCODE_is_store(opcode)) {
    BIT_VECTOR *bv = invar_table->Find(wn);
    if (bv && bv->Pop_Count()) {
      Enter_Scalar_Expand(bv,wn);
      return;
    }
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN *kid = WN_kid(wn,kidno);
    Build(kid, SNL_Depth,invar_table);
  }


  if (OPCODE_is_load(opcode) && opr != OPR_LDID) {
    if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
      Build_Array(WN_kid0(wn),FALSE,dli_stack,SNL_Depth);
    } else {
      if (MTYPE_is_float(WN_desc(wn)))
        _num_bad_fp++;
      else
        _num_bad_int++;
    }
  } else if (OPCODE_is_store(opcode) && opr != OPR_STID) {
    if (WN_operator(WN_kid1(wn)) == OPR_ARRAY) {
      Build_Array(WN_kid1(wn),TRUE,dli_stack,SNL_Depth);
    } else {
      if (MTYPE_is_float(WN_desc(wn)))
        _num_bad_fp++;
      else
        _num_bad_int++;
    }
  } else if (opcode == OPC_DO_LOOP) {
    dli_stack->Pop();
  }

}

void 
ARRAY_REF::Build_Array(WN* wn_array, 
                       BOOL is_store, 
                       DLI_STACK* dli_stack, 
                       INT SNL_Depth)
{
  TYPE_ID type = WN_desc(LWN_Get_Parent(wn_array));
  INT esz = MTYPE_size_min(type) >> 3;
#ifdef KEY
  // Bug 3072 - For BSISTORE (1-bit MTYPE_bit_size), adjust esz to 1-byte:
  // 1-byte is the minimum unit recognized by LNO model.
  if (esz == 0 && type == MTYPE_BS)
    esz = 1;
#endif

  ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,wn_array);
  if (!array || array->Too_Messy 
    || Do_Depth(wn_array) + 1 - array->Non_Const_Loops() < SNL_Depth) {
    if (MTYPE_is_float(type))
      _num_bad_fp++;
    else
      _num_bad_int++;
    return;
  }

  INT i;
  for (i=0; i<array->Num_Vec(); i++) {
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Too_Messy || av->Non_Lin_Symb 
	|| Weird_Triangular(av,dli_stack,SNL_Depth)) {
      if (MTYPE_is_float(type))
        _num_bad_fp++;
      else
        _num_bad_int++;
      return;
    }
  }

  // Find which element in the stack contains our base array
  WN *base = WN_array_base(wn_array);
  if ((WN_operator(base) != OPR_LDA) &&
      (WN_operator(base) != OPR_LDID)) {
    if (MTYPE_is_float(type))
      _num_bad_fp++;
    else
      _num_bad_int++;
    return;
  }
  SYMBOL symb(base);

  ARRAY_REF_NODE* arn = CXX_NEW(ARRAY_REF_NODE(array, wn_array, is_store, esz,
				_lex_number++), _pool);
  for (i=0; i<Elements(); i++) {
    if (symb == *Array_Ref_List(i)->Base_Array) {
      Array_Ref_List(i)->Append(arn);
      return;
    }
  }
  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(&symb),_pool);
  Push(CXX_NEW(ARRAY_REF_LIST(_pool,tmp_symb), _pool));
  Array_Ref_List(Elements()-1)->Append(arn);
}

// Enter a scalar expanded references
// If we have to expand if 'i' and 'j' are inner, then create an
// ACCESS_ARRAY [i][j]
void 
ARRAY_REF::Enter_Scalar_Expand(WN* wn,
                               SX_PNODE* pnode,
                               BOOL* can_be_inner, 
                               INT num_loops)
{
  BOOL is_store = OPCODE_is_store(WN_opcode(wn));
  TYPE_ID type = WN_desc(wn);
  INT esz = MTYPE_size_min(type) >> 3;

  INT count=0;
  INT i;
  for (i=0; i<num_loops; i++) {
    if (can_be_inner[i] && pnode->Transformable(i)==SX_PNODE::SE_REQD) {
      count++;
    }
  }

  ACCESS_ARRAY *a = CXX_NEW(ACCESS_ARRAY(MAX(1,count),num_loops,_pool),_pool);
  a->Too_Messy = FALSE;
  if (count == 0) {
    a->Dim(0)->Too_Messy = FALSE;
  } else {
    INT c=0;
    for (INT i=0; i<num_loops; i++) {
      if (can_be_inner[i]&&pnode->Transformable(i)==SX_PNODE::SE_REQD) {
	a->Dim(c)->Too_Messy = FALSE;
	a->Dim(c)->Set_Loop_Coeff(i,1);
        c++;
      }
    }
  }

  ARRAY_REF_NODE *arn = 
    CXX_NEW(ARRAY_REF_NODE(a,NULL,is_store,esz,_lex_number++),_pool);
  for (i=0; i<Elements(); i++) {
    if (pnode->Symbol() == *Array_Ref_List(i)->Base_Array) {
      Array_Ref_List(i)->Append(arn);
      Array_Ref_List(i)->_is_scalar_expanded = TRUE;
      return;
    }
  }
  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(pnode->Symbol()),_pool);
  Push(CXX_NEW(ARRAY_REF_LIST(_pool,tmp_symb), _pool));
  Array_Ref_List(Elements()-1)->Append(arn);
  Array_Ref_List(Elements()-1)->_is_scalar_expanded = TRUE;
}

// We have an invariant expression, treat it like a load
// that varies in all the dimensions that the expression varies
void 
ARRAY_REF::Enter_Scalar_Expand(BIT_VECTOR* bv, WN* wn)
{
  INT num_loops = bv->Size();
  INT num_invar = bv->Pop_Count();
  INT num_var = num_loops - num_invar;

  ACCESS_ARRAY* a=CXX_NEW(ACCESS_ARRAY(MAX(1,num_var),num_loops,_pool),_pool);
  a->Too_Messy = FALSE;
  if (num_var == 0) {
    a->Dim(0)->Too_Messy = FALSE;
  } else {
    INT count=0;
    for (INT i=0; i<num_loops; i++) {
      if (!bv->Test(i)) {
	a->Dim(count)->Too_Messy = FALSE;
	a->Dim(count)->Set_Loop_Coeff(i,1);
        count++;
      }
    }
  }

  TYPE_ID type = WN_rtype(wn);
  INT esz = MTYPE_size_min(type) >> 3;

  ARRAY_REF_NODE *arn = 
    CXX_NEW(ARRAY_REF_NODE(a,wn,FALSE,esz,_lex_number++),_pool);
  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(),_pool);
  tmp_symb->Type = type;
  Push(CXX_NEW(ARRAY_REF_LIST(_pool,tmp_symb), _pool));
  Array_Ref_List(Elements()-1)->Append(arn);
  Array_Ref_List(Elements()-1)->_is_scalar_expanded = TRUE;
}

// Enter a scalar expanded references
// If we have to expand if 'i' and 'j' are inner, then create an
// ACCESS_ARRAY [i][j]
void 
ARRAY_REF::Enter_Innermost_Scalar_Expand(WN* wn)
{
  BOOL is_store = OPCODE_is_store(WN_opcode(wn));
  TYPE_ID type = WN_desc(wn);
  INT esz = MTYPE_size_min(type) >> 3;
  SYMBOL sym(wn);

  ACCESS_ARRAY *a = CXX_NEW(ACCESS_ARRAY(1,1,_pool),_pool);
  a->Too_Messy = FALSE;
  a->Dim(0)->Too_Messy = FALSE;

  ARRAY_REF_NODE *arn = 
    CXX_NEW(ARRAY_REF_NODE(a,NULL,is_store,esz,_lex_number++),_pool);
  for (INT i=0; i<Elements(); i++) {
    if (sym == *Array_Ref_List(i)->Base_Array) {
      Array_Ref_List(i)->Append(arn);
      Array_Ref_List(i)->_is_scalar_expanded = TRUE;
      return;
    }
  }
  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(sym),_pool);
  Push(CXX_NEW(ARRAY_REF_LIST(_pool,tmp_symb), _pool));
  Array_Ref_List(Elements()-1)->Append(arn);
  Array_Ref_List(Elements()-1)->_is_scalar_expanded = TRUE;
}



//
//
//
//
//
// LATENCY ROUTINES
//
//
//
//
//

// Walk the code
// Add a vertex to the graph for each fp array store/load
// put into the hash table a mapping from the vertex in the array dependence 
// graph to the vertex in the latency graph
// Add an edge from each load to its parent store with the latency = sum
// of the latencies from the store down to the load
// Return -1 on failure
INT LAT_DIRECTED_GRAPH16::Add_Vertices_Op_Edges(WN *wn,HASH_TABLE<WN *,BIT_VECTOR *> *invar_table)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      if (Add_Vertices_Op_Edges(kid,invar_table) == -1) return(-1);
      kid = WN_next(kid);
    }
    return (1);
  }

  VINDEX16 v;
  if (OPCODE_is_store(opcode) &&  (v =_array_graph->Get_Vertex(wn))) {
    VINDEX16 this_v = Add_Vertex(wn);
    if (!this_v) return(-1);
    Map_Vertex(v,this_v);

    if (Add_Vertices_Op_Edges_Rec(this_v,WN_kid0(wn),0,invar_table) == -1) return -1;

  } else if (!OPCODE_is_stmt(opcode)) {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      if (Add_Vertices_Op_Edges(WN_kid(wn,kidno),invar_table) == -1) return(-1);
    }
  }
  return(1);
}


// Deal recursively with the things under the store
INT 
LAT_DIRECTED_GRAPH16::Add_Vertices_Op_Edges_Rec(VINDEX16 store,
                                                WN *wn,
                                                INT latency,
                                                HASH_TABLE<WN *,BIT_VECTOR *> 
                                                *invar_table)
{
  TOP top;
  OPERATOR oper = WN_operator(wn);
  TYPE_ID rtype = WN_rtype(wn);
  TYPE_ID  desc = WN_desc(wn);

  VINDEX16 v;
  if (OPERATOR_is_load(oper) && (v = _array_graph->Get_Vertex(wn))) {
    VINDEX16 this_v = Add_Vertex(wn);
    if (!this_v) {
      return -1;
    }
    Map_Vertex(v,this_v);
    EINDEX16 e = Add_Edge(this_v,store,0,latency,0);
    if (!e) {
      return -1;
    }
  } 

  if (invar_table) { // no cost to invariant expressions
    BIT_VECTOR *bv = invar_table->Find(wn);
    if (bv && bv->Pop_Count()) {
      return 1;
    }
  }

  INT op_latency = 0;

  if (oper == OPR_CVT   || 
      oper == OPR_RND   ||
      oper == OPR_CEIL  || 
      oper == OPR_TRUNC || 
#ifdef TARG_X8664
      // skip float to float floor
      oper == OPR_FLOOR &&
        !(desc == MTYPE_F4 && rtype == MTYPE_F4) &&
        !(desc == MTYPE_F8 && rtype == MTYPE_F8)
#else
      oper == OPR_FLOOR
#endif
      ) {
    op_latency = LNOTARGET_Cvt_Lat(WN_opcode(wn));
    if (op_latency == -1) {
      return -1;
    }
  } 
  else if (oper == OPR_INTRINSIC_OP) {
    op_latency = FP_Latency_Intrinsic(wn);
    if (op_latency == -1) {
      return -1;
    }
  } 
  else if (oper == OPR_REALPART || 
           oper == OPR_IMAGPART ||
           oper == OPR_PAREN    || 
           oper == OPR_PARM) {  // no-ops
    op_latency = 0;
  } 
  else if (OPERATOR_is_expression(oper) && 
           !OPERATOR_is_load(oper) &&
           oper != OPR_CONST) {
    // an fp expression
    if (desc  == MTYPE_FQ || 
        desc  == MTYPE_CQ || 
        rtype == MTYPE_FQ ||
        rtype == MTYPE_CQ) {
      return -1;
    } 
    // regular floating point
    else if (desc  == MTYPE_F4 || 
             desc  == MTYPE_F8 ||
#if defined(TARG_IA64) || defined(TARG_X8664)
             desc  == MTYPE_F10 ||
             rtype  == MTYPE_F10 ||
#endif
             rtype == MTYPE_F4 ||
             rtype == MTYPE_F8) {
      // multiply-adds
      if (Target_ISA_Has_Madd() && 
          (oper == OPR_ADD || oper == OPR_SUB) && 
          (WN_operator(WN_kid0(wn)) == OPR_MPY || 
           WN_operator(WN_kid1(wn)) == OPR_MPY)) { 
        return FP_Latency_Madd(store, wn, latency, invar_table);
      } 
      else if (oper == OPR_MAX || oper == OPR_MIN) {
        op_latency = LNOTARGET_FP_Min_Max_Lat(rtype);
      } 
      else if (oper == OPR_SQRT) {
        op_latency = LNOTARGET_FP_Sqrt_Lat(rtype);
      } 
      else if ((top = LNOTARGET_Whirl_To_Top(wn)) != TOP_UNDEFINED) {
	op_latency = LNOTARGET_Top_Latency(top);
      }
      else if (oper == OPR_DIV) {
        op_latency = LNOTARGET_FP_Div_Lat(rtype);
      } 
      else if (oper == OPR_RECIP) {
        op_latency = LNOTARGET_FP_Recip_Lat(rtype);
      } 
      else if (oper == OPR_RSQRT
#ifdef TARG_X8664
	       || oper == OPR_ATOMIC_RSQRT
#endif
	      ) {
        op_latency = LNOTARGET_FP_Rsqrt_Lat(rtype);
      } 
#ifdef TARG_X8664
      else if (oper == OPR_FLOOR) {
        op_latency = LNOTARGET_FP_Floor_Lat(rtype);	
      }       
      else if (oper == OPR_SELECT) {
        op_latency = LNOTARGET_FP_Select_Lat(rtype);	
      } 
      else if (OPCODE_is_compare(WN_opcode(wn))) {
        op_latency = LNOTARGET_FP_Compare_Lat(rtype);	
      }             
#endif
      else {
        return -1;
      }
    }
    else if (desc  == MTYPE_C4 || 
#if defined(TARG_IA64)
             desc  == MTYPE_C10 ||
             rtype  == MTYPE_C10 ||
#endif
             desc  == MTYPE_C8 ||
             rtype == MTYPE_C4 ||
             rtype == MTYPE_C8) {
      if (oper == OPR_ADD || oper == OPR_SUB) {
        op_latency = LNOTARGET_Complex_Add_Lat(rtype);
      } 
      else if (oper == OPR_MPY)  {
        op_latency = LNOTARGET_Complex_Mult_Lat(rtype);
      }
      else if (oper == OPR_NEG)  {
        op_latency = LNOTARGET_Complex_Neg_Lat(rtype);
      } 
    }
  } 

  for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    if (Add_Vertices_Op_Edges_Rec(store,
                                  WN_kid(wn,kidno),
                                  latency+op_latency,
                                  invar_table) == -1) {
      return -1;
    } 
  }
  return 1;
}

// deal with a madd
INT 
LAT_DIRECTED_GRAPH16::FP_Latency_Madd(VINDEX16 store,
                                      WN *wn, 
                                      INT latency,
                                      HASH_TABLE<WN *,BIT_VECTOR *> 
                                      *invar_table)
{
  TYPE_ID rtype = WN_rtype(wn);
  INT add_op_latency = LNOTARGET_FP_Madd_Add_Lat(rtype);
  INT mult_op_latency = LNOTARGET_FP_Madd_Mult_Lat(rtype);

  WN *kid0 = WN_kid0(wn);
  WN *kid1 = WN_kid1(wn);

  WN *non_mult_kid;
  WN *mult_kid0;
  WN *mult_kid1;

  // now do the appropriate kids
  if (WN_operator(kid0) == OPR_MPY) {
    non_mult_kid = kid1;
    mult_kid0 = WN_kid0(kid0);
    mult_kid1 = WN_kid1(kid0);
  } else {
    non_mult_kid = kid0;
    mult_kid0 = WN_kid0(kid1);
    mult_kid1 = WN_kid1(kid1);
  }

  if (Add_Vertices_Op_Edges_Rec(store,
                                non_mult_kid,
                                latency+add_op_latency,
                                invar_table) == -1) {
    return -1;
  }
  if (Add_Vertices_Op_Edges_Rec(store,
                                mult_kid0,
                                latency+mult_op_latency,
                                invar_table) == -1) {
    return -1;
  }
  if (Add_Vertices_Op_Edges_Rec(store,
                                mult_kid1,
                                latency+mult_op_latency,
                                invar_table) == -1) {
    return -1;
  }

  return 1;
}

// What is the latency of an intrinsic op (currently only handle 
// exponentiation by a small number)
// return -1 on error
INT LAT_DIRECTED_GRAPH16::FP_Latency_Intrinsic(WN *wn)
{
  if (WN_kid_count(wn) != 2) {
    return -1;
  }
  WN *const_kid = WN_kid1(wn);
  if (WN_operator(const_kid) == OPR_PARM) {
    const_kid = WN_kid0(const_kid);
  }
  if (WN_operator(const_kid) != OPR_INTCONST) {
    return -1;
  }
  INT num_multiplies = WN_const_val(const_kid)-1;
  if (num_multiplies == 0) {
    return 0;
  }
  if (num_multiplies < 0 || num_multiplies > 3) {
    return -1;
  }
  return LNOTARGET_FP_Exp_Lat((INTRINSIC) WN_intrinsic(wn), num_multiplies);
}



// use the array_graph to add into the latency graph all the edges from 
// stores to loads.  We assume that these all have latency 0  (We add
// them in because of the transitivity)
// return -1 on error
INT LAT_DIRECTED_GRAPH16::Add_Flow_Edges()
{
  for (VINDEX16 i = Get_Vertex(); i; i=Get_Next_Vertex(i)) {
    WN *wn = _v[i]._wn;
    if (OPCODE_is_store(WN_opcode(wn))) { // go through every store in LAT 
      REDUCTION_TYPE source_red = 
	 red_manager == NULL? RED_NONE : red_manager->Which_Reduction(wn);
      VINDEX16 array_source = _array_graph->Get_Vertex(wn);
      EINDEX16 e = _array_graph->Get_Out_Edge(array_source);
      while (e) { // go through every edge from this store in array graph
        REDUCTION_TYPE sink_red = 
	 red_manager == NULL? RED_NONE : red_manager->Which_Reduction(wn);
        if ((source_red != sink_red) || (source_red == RED_NONE)) {
	  VINDEX16 array_sink = _array_graph->Get_Sink(e);
	  // is the edge to a load in this loop (ie a load in the hash table)
          VINDEX16 sink =  _hash_table.Find(array_sink);
	  if (sink && OPCODE_is_load(WN_opcode(_v[sink]._wn))) { 
	    EINDEX16 newe=
	    Add_Edge(i,sink,_array_graph->Depv_Array(e)->Union(_pool),0,
		_array_graph->Depv_Array(e)->Num_Unused_Dim());
	    if (!newe) return(-1);
	  }
	}
	e = _array_graph->Get_Next_Out_Edge(e);
      }
    }
  }
  return(1);
}

// Find the latency bound assuming loop number "inner" is the inner loop
// Find the max over all cycles of sum(latencies)/sum(distances) for
// distances > 0
//
// We use the same algorithm as the SWP taken from Lam taken from Floyd.
// We really should just call the SWP code, but ...
double LAT_DIRECTED_GRAPH16::Max_Cycle(INT inner, double lower_bound)
{
  double result=0.0;

  // First build and find the SCC_GRAPH
  // We initialize it with v vertices and edges (the init size has no effect
  // on correctness) rather than v vertices and e edges because many of the
  // edges might not be valid for this choice of inner loop
  SCC_DIRECTED_GRAPH16 *scc_graph = CXX_NEW(SCC_DIRECTED_GRAPH16(
			Get_Vertex_Count(),Get_Vertex_Count()),_pool);
  Set_Scc_Graph(scc_graph,inner);
  INT num_scc = scc_graph->Get_Scc_Count();

  // how many elements of each scc 
  INT *scc_counts = CXX_NEW_ARRAY(INT,num_scc+1,_pool);

  // map each vertex to its ordering of the vertices in the same scc
  // i.e. if vertex 'v' is the x'th vertex in scc[i], set scc_pos[v] = i-1
  INT *scc_pos = CXX_NEW_ARRAY(INT,scc_graph->Get_Vertex_Count()+1,_pool);
  INT i;
  for (i=1; i<=num_scc; i++) {
    scc_counts[i] = 0;
    scc_pos[i] = 0;
  }
  for (i=1; i<=scc_graph->Get_Vertex_Count(); i++) {
    INT id = scc_graph->Get_Scc_Id(i);
    scc_pos[i] = scc_counts[id];
    scc_counts[id]++;
  }

  COST_TABLE *ct = NULL;
  // Find the maximum cycle of each scc
  for (i=1; i<=num_scc; i++) {
    if (scc_counts[i] > 1) { // because of what we put in the graph,
			     // the maximum can't be a self cycle
      // create a cost table
      if (!ct) {
        ct = CXX_NEW(COST_TABLE(scc_counts[i],_pool),_pool);
      } else {
	ct->Realloc(scc_counts[i]);
      }

      // initialize the cost table with the edges in the graph
      double upper_bound = ct->Init(inner,this,scc_graph,i,scc_pos);
      if (upper_bound > result) {
	// solve
	INT solve = (INT)(ct->Solve(lower_bound));
        result = MAX(result,solve);
      }
    }
  }
  CXX_DELETE(scc_graph,_pool);
  CXX_DELETE(ct,_pool);
  CXX_DELETE_ARRAY(scc_pos,_pool);
  CXX_DELETE_ARRAY(scc_counts,_pool);
  return result;
}

// Initialize the scc graph to have the same vertices as this.
// Add an edge for each valid dependence.  A dependence is
// valid if it has an equal direction in every loop except possibly inner.
// The scc graph cannot overflow since it's strictly smaller than this
void LAT_DIRECTED_GRAPH16::Set_Scc_Graph(SCC_DIRECTED_GRAPH16 *scc_graph,
								INT inner)
{
  Lat_scc_vertex_map= CXX_NEW_ARRAY(VINDEX16,_v.Lastidx()+1,_pool); 
  Scc_lat_vertex_map= CXX_NEW_ARRAY(VINDEX16,_v.Lastidx()+1,_pool); 
  for (INT i=Get_Vertex(); i; i = Get_Next_Vertex(i)) {
    VINDEX16 scc_vertex = scc_graph->Add_Vertex();
    Is_True(scc_vertex,("Impossible overflow in Set_Scc_Graph"));
    Lat_scc_vertex_map[i] = scc_vertex;
    Scc_lat_vertex_map[scc_vertex] = i;
  }
  EINDEX16 e = Get_Edge();
  while (e) {
    if (Is_Valid(inner,e)) {
      scc_graph->Add_Edge(Lat_scc_vertex_map[Get_Source(e)],
			Lat_scc_vertex_map[Get_Sink(e)]);
    } 
    e = Get_Next_Edge(e);
  }
  CXX_DELETE_ARRAY(Lat_scc_vertex_map,_pool);
}

COST_V::COST_V() {
  _alloc_length = 4;
  _length = 0;
  _costs = CXX_NEW_ARRAY(COST,(INT) _alloc_length,Default_Mem_Pool);
}


void COST_V::Push(UINT16 latency, UINT16 distance, MEM_POOL *pool) {
  if (_length == _alloc_length) {
    COST *tmp = CXX_NEW_ARRAY(COST,((INT) 2*_alloc_length),pool);
    bcopy(_costs,tmp,_length*sizeof(COST));
    CXX_DELETE_ARRAY(_costs,pool);
    _costs = tmp;
    _alloc_length *= 2;
  }
  _costs[_length].Distance = distance;
  _costs[_length++].Latency = latency;
}



COST_TABLE::COST_TABLE(UINT16 num_vertex, MEM_POOL *pool)
{
  _pool = pool;
  MEM_POOL_Set_Default(_pool);
  _data = CXX_NEW_ARRAY(COST_V,((INT) num_vertex*num_vertex),pool);
  _n = num_vertex;
  _maxn = _n;
}

// Reinit an array (use the same space if possible)
void COST_TABLE::Realloc(UINT16 num_vertex)
{
  if (num_vertex <= _maxn) {
    for (INT i=0; i<num_vertex; i++) {
      for (INT j=0; j<num_vertex; j++) {
	_data[num_vertex*i+j].Init();
      }
    }
    _n = num_vertex;
  } else {
    MEM_POOL_Set_Default(_pool);
    CXX_DELETE_ARRAY(_data,_pool);
    _data = CXX_NEW_ARRAY(COST_V,((INT) num_vertex*num_vertex),_pool);
    _n = _maxn = num_vertex;
  }
}

  
// Initialize the table with the edges from the scc graph (these are
// all the valid edges)
// Use the latency graph to get the latencies of these edges
// scc_pos maps from vertex num to position among vertices of the same
//  scc.  We use it for the vertex number in the Cost Table
// Return the sum of all the latencies.  This is an upper bound on
// the value of the cycle
double COST_TABLE::Init(INT inner, LAT_DIRECTED_GRAPH16 *graph, 
	SCC_DIRECTED_GRAPH16 *scc_graph, INT scc_id, INT *scc_pos)
{
  double result = 0.0;
  BOOL pos_distance = TRUE;
  EINDEX16 scc_e = scc_graph->Get_Edge();
  while (scc_e) {
    VINDEX16 source = scc_graph->Get_Source(scc_e);
    INT source_id = scc_graph->Get_Scc_Id(source);
    if (source_id == scc_id) {
      VINDEX16 sink = scc_graph->Get_Sink(scc_e);
      INT sink_id = scc_graph->Get_Scc_Id(sink);
      if (sink_id == scc_id) {
        EINDEX16 e = graph->Get_Edge(graph->Scc_lat_vertex_map[source],
				     graph->Scc_lat_vertex_map[sink]);
        UINT latency = graph->Latency(e);
	result = result + latency;
        UINT distance=0;
        DEPV *depv = graph->Depv(e);
        if (depv) {  // is this distance positive or zero (we don't care
		     // about lexicographically negative dependences)
	  DEP dep = DEPV_Dep(depv,inner-graph->Num_Unused_Dim(e));
	  if (DEP_IsDistance(dep)) {
	    if (DEP_Distance(dep) >= 0) {
	      distance = DEP_Distance(dep);
	    } else {
	      pos_distance = FALSE;
	    }
          } else {
	    DIRECTION dir = DEP_Direction(dep);
	    if ((dir == DIR_POS) || (dir == DIR_POSEQ) || (dir == DIR_POSNEG) 
	      ||(dir == DIR_STAR)) {
	      distance = 1;
	    } else if ((dir == DIR_NEGEQ) || (dir == DIR_EQ)) {
	      distance = 0;
	    } else {
	      pos_distance = FALSE;
	    }
	  }
        }
        if (pos_distance) {
	  Push(scc_pos[source],scc_pos[sink],latency,distance);
	}
      }
    }
    scc_e = scc_graph->Get_Next_Edge(scc_e);
  }
  return (double) result;
}


// Do the algorithm
double COST_TABLE::Solve(double init_min_ii)
{
  _min_ii = init_min_ii;

  for (INT k=0; k<_n; k++) {
    for (INT i=0; i<_n; i++) {
      for (INT j=0; j<_n; j++) {
        // Consider the existing costs of paths from i to j and the
        // costs of paths from i to j via k.  Filter out those that
        // are cannot be maximal (given a min II) and replace the
        // costs of paths from i to j with the result.
	Add_Maximal_Costs(Cost_V(i,j),Cost_V(i,k),Cost_V(k,j));

	// updatin _min_ii if need be
	Update_Min_II(Cost_V(i,j),Cost_V(j,i));
      }
    }
  }
  return(_min_ii);
}

//  Add_Maximal_Costs
//
//  'Cvij', 'cvik', 'cvkj' are existing entries in the cost table.
//  set cvij U (cvik + cvkj).  In other words, we want to consider
//  paths from i to j via k as well as all the paths from i to j we
//  have already considered.  Only those that we can be maximal (given
//  the min_ii) are included in the resulting 'cvij'.
void COST_TABLE::Add_Maximal_Costs(COST_V *cvij, COST_V *cvik, COST_V *cvkj)
{
  COST *cvik_costs = cvik->Costs();
  COST *cvkj_costs = cvkj->Costs();
  UINT16 cvik_length = cvik->Length();
  UINT16 cvkj_length = cvkj->Length();

  // First consider costs in the cross product of cjik and cvkj.
  // We'll add a cost if it is maximal relative to the current costs
  // in cvij.
  INT i;
  for ( i = 0; i < cvik_length; ++i ) {
    COST *cpik      = cvik_costs + i;
    INT   ikdist   = cpik->Distance;
    INT   iklatency = cpik->Latency;
    for (INT  j = 0; j < cvkj_length; ++j ) {
      COST *cpkj       =  cvkj_costs + j;
      INT   kjdist    = cpkj->Distance;
      INT   kjlatency  = cpkj->Latency;
      INT   ikjdist   = ikdist + kjdist;
      INT   ikjlatency = iklatency + kjlatency;
      if ( Is_Max_Cost(ikjdist,ikjlatency,cvij,0)) {
        cvij->Push(ikjlatency,ikjdist,_pool);
      }
    }
  }

  // each cost in cvij is maximal relative to the preceeding
  // costs, but not necessarily relative to the succeeding costs.
  // We'll compare each cost to the succeeding costs, and delete it if
  // it is not (possibly) maximal.  Deletion is accompilshed by
  // copying the last element of cvij into the element to be deleted
  // and decrementing the length of cvij.  We consider the elements in
  // reverse order so that will not disturb the part of the vector not
  // yet processed.
  //
  COST *cvij_costs  = cvij->Costs();    /* Possibly side-effected */
  UINT16 cvij_length = cvij->Length();  /* ..by previous loop.    */
  for ( i = cvij_length - 1; i >= 0; --i ) {
    COST *cpij = cvij_costs + i;
    INT   dist   = cpij->Distance;
    INT   latency = cpij->Latency;
    if (!Is_Max_Cost(dist,latency,cvij,i+1)) {
      // Delete by replacing with last element...
      if ( i != cvij_length - 1 ) {
        COST *ij_last = cvij_costs + (cvij_length - 1);
        *cpij = *ij_last;           /* Structure copy. */
      }
      --cvij_length;
    }
  }
  cvij->Set_Length(cvij_length);
}

//  Is_Max_Cost
//
//  Is the cost <'distance','latency'> maximal relative to the elements
//  in 'cv' given 'min_ii'?   Offset gives the index of the first element
//  to check
//
BOOL COST_TABLE::Is_Max_Cost(INT dist, INT latency, COST_V *cv, INT offset)
{
  INT   len = cv->Length();
  COST *cp  = cv->Costs();
  for (INT i = offset; i < len; ++i ) {
    INT cvdist   = cp[i].Distance;
    INT cvlatency = cp[i].Latency;
    // We can reject things with duplicates, since never check a cost
    // against itself.
    if ((dist == cvdist && latency <= cvlatency) || 
	dist > cvdist && (latency - cvlatency) <= ((dist-cvdist) * _min_ii)) {
      return FALSE;
    }
  }
  return TRUE;
}


// Update min_ii give that cv1 and cv2 are cost vectors from one vertex
// to another and back
void COST_TABLE::Update_Min_II(COST_V *cv1, COST_V *cv2)
{
  COST *cp1 = cv1->Costs();
  COST *cp2 = cv2->Costs();
  INT   len1 = cv1->Length();
  INT   len2 = cv2->Length();
  for (INT i = 0; i < len1; ++i ) {
    INT dist1   = cp1[i].Distance;
    INT latency1   = cp1[i].Latency;
    for (INT j = 0; j < len2; ++j) {
      INT dist2   = cp2[j].Distance;
      INT latency2   = cp2[j].Latency;
      if ((dist1 + dist2) != 0) {
        INT path_mii = (latency1+latency2)/(dist1+dist2);
        _min_ii = MAX(_min_ii,path_mii);
      }
    }
  }
}




void COST_TABLE::Print(FILE *fp)
{
  fprintf(fp,"Printing a table \n");
  for (INT i = 0; i<_n; i++) {
    for (INT j = 0; j<_n; j++) {
      COST_V cv = _data[_n*i+j];
      if (cv.Length()) {
	fprintf(fp,"Point[%d][%d]: ",i,j);
        for (INT l = 0; l<cv.Length(); l++) {
	  fprintf(fp," (L:%d, D:%d) ",cv.Costs()[l].Latency, 
		cv.Costs()[l].Distance);
	}
	fprintf(fp,"\n");
      }
    }
  }
}


// Is edge e valid given this inner loop
// an edge is valid if all the non-inner dependences have equal dependences
BOOL LAT_DIRECTED_GRAPH16::Is_Valid(INT inner,EINDEX16 e)
{
  DEPV *depv = _e[e].Depv;
  if (!depv) return TRUE;  // an all equals dependence
  for (INT i=0; i<_num_dim; i++) {
    if ((i!=(inner-_num_bad)) && 
	(DEP_Direction(DEPV_Dep(depv,i)) != DIR_EQ) &&
	(DEP_Direction(DEPV_Dep(depv,i)) != DIR_POSEQ) &&
	(DEP_Direction(DEPV_Dep(depv,i)) != DIR_NEGEQ) &&
	(DEP_Direction(DEPV_Dep(depv,i)) != DIR_STAR)) {  
      return(FALSE);
    }
  }
  return(TRUE);
}


void LAT_DIRECTED_GRAPH16::Print(FILE *fp)
{
  VINDEX16 i;
  EINDEX16 e;
  fprintf(fp,"Printing a LAT_DIRECTED_GRAPH16 \n");
  for (i=Get_Vertex(); i; i = Get_Next_Vertex(i)) {
    fprintf(fp,"Vertex %d for Wn = %s\n",i,OPCODE_name(WN_opcode(_v[i]._wn)));

    e = _v[i].Get_Out_Edge();
    while (e) {
      fprintf(fp,"Edge to vertex %d ",_e[e].Get_Sink());
      fprintf(fp," has latency = %d ",_e[e].Latency);
      if (_e[e].Depv) {
	fprintf(fp," and dependence ");
	DEPV_Print(_e[e].Depv,fp,_num_dim);
	fprintf(fp,"\n");
      } else {
	fprintf(fp," and an all equals dependence \n");
      } 
      e = _e[e].Get_Next_Out_Edge();
    }
  }

}


//
//
//
//
// Register pressure routines
//
//
//


// How many unique, never stored, scalar refs are in the loop
// Count complex and quad as two (it really takes two registers to load them)
// Count complex quad as four
// Count OPR_CONST as well.
//
// Don't count scalars that will be scalar expanded
// Instead pretend they are arrays and add them to ar
// Don't count things inside loads, since we won't need to
// store both the ILOAD and the address expression

INT LOOP_MODEL::Unique_Unstored_Fp_Scalar_Refs(WN *wn, ARRAY_REF *ar,
		SX_INFO *pi)
{
  MEM_POOL_Push(&LNO_local_pool);
  SYMBOL_TREE *symbol_tree = CXX_NEW(SYMBOL_TREE(
	/*is_floating_point*/TRUE,&LNO_local_pool), &LNO_local_pool);
  INT outer=0;
  _num_fp_scalar_refs = 0;
  while (!_can_be_inner[outer]) outer++;
  symbol_tree->Enter_Scalar_Refs(wn,ar,pi,_can_be_inner,_num_loops,outer,
				&_num_fp_scalar_refs);
  INT result = symbol_tree->Num_Fp_Unstored();
  CXX_DELETE(symbol_tree,&LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
  return result;
}

INT LOOP_MODEL::Unique_Unstored_Int_Scalar_Refs(WN *wn, ARRAY_REF *ar,
		SX_INFO *pi)
{
  MEM_POOL_Push(&LNO_local_pool);
  SYMBOL_TREE *symbol_tree = CXX_NEW(SYMBOL_TREE(
	/*is_floating_point*/FALSE,&LNO_local_pool), &LNO_local_pool);
  INT outer=0;
  _num_int_scalar_refs = 0;
  while (!_can_be_inner[outer]) outer++;
  symbol_tree->Initialize_Innermost_Loop_Var_Symbol(wn);
  symbol_tree->Enter_Scalar_Refs(wn,ar,pi,_can_be_inner,_num_loops,outer,
				&_num_int_scalar_refs);
  INT result = symbol_tree->Num_Int_Unstored();
  CXX_DELETE(symbol_tree,&LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
  return result;
}

void SYMBOL_TREE::Initialize_Innermost_Loop_Var_Symbol(WN* wn) {
  WN* parent=wn;
  while (WN_opcode(parent)!=OPC_DO_LOOP)
    parent=LWN_Get_Parent(parent);
  _innermost_loop_var_symb.Init(WN_index(parent));
}

BOOL SYMBOL_TREE::Integer_Ref_Needs_Reg(WN* wn) {

      SYMBOL symb(wn);
      WN* wn1=wn;
      WN* parent=LWN_Get_Parent(wn1);
      while (WN_operator(parent)!=OPR_ARRAY &&
             OPCODE_is_expression(WN_opcode(parent))) {
	wn1=LWN_Get_Parent(wn1);
	parent=LWN_Get_Parent(wn1);
      }

      if (WN_operator(parent)==OPR_ARRAY) {
        INT kid_id=0;
	INT num_dim=WN_num_dim(parent);
        while (WN_kid(parent,kid_id)!=wn1) kid_id++;

        // ignore scalar ref to array bases or bound of 1st dim

        if (1<kid_id && kid_id<=num_dim) { // appear in dim expr
	  ACCESS_ARRAY *ar=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,parent);
	  for (INT i=kid_id+1; i<=num_dim; i++) {
	    ACCESS_VECTOR *av=ar->Dim(i-1);
	    // see if innermost loop var appear in outer dim
            if (av->Loop_Coeff(av->Nest_Depth()-1)!=0) {
              return TRUE;
            }
          }
        } else if (num_dim<kid_id) { // appear in index expr
          if (symb!=_innermost_loop_var_symb) {
            return FALSE;
          }
	}
      } else if (symb!=_innermost_loop_var_symb) {
        return TRUE;
      }
     return FALSE;
}

void SYMBOL_TREE::Enter_Scalar_Refs(
WN *wn, INT *num_scalar_refs,
WN2INT *se_needed, ARRAY_REF *ar)
{
  OPCODE opcode = WN_opcode(wn);
  BOOL is_store = FALSE;

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Enter_Scalar_Refs(kid,num_scalar_refs,se_needed,ar);
      kid = WN_next(kid);
    }
    return;
  } 
  
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_LDID)  || (oper == OPR_CONST) ||
             (is_store=(OPCODE_operator(opcode) == OPR_STID))) {

    if (se_needed && se_needed->Find(wn)==1)
      ar->Enter_Innermost_Scalar_Expand(wn);
    else {

      TYPE_ID type;
      if (is_store) {
        type = OPCODE_desc(opcode);
      } else {
        type = OPCODE_rtype(opcode);
      }
      if (_is_floating_point && MTYPE_float(type)) {
        if ((type == MTYPE_F4) || (type == MTYPE_F8)) {
          SYMBOL symb(wn);
          (*num_scalar_refs)++;
          Enter(&symb, is_store, 1);
        } else if ((type == MTYPE_C4) || (type==MTYPE_C8) ||
#if defined(TARG_IA64) || defined(TARG_X8664)
		   (type == MTYPE_F10) ||
#endif
                   (type == MTYPE_FQ)) {
          SYMBOL symb(wn);
          (*num_scalar_refs)+=2;
          Enter(&symb, is_store, 2);
        } else if (type == MTYPE_CQ
#if defined(TARG_IA64) || defined(TARG_X8664)
	    || type == MTYPE_C10
#endif
	      ) {
          SYMBOL symb(wn);
          (*num_scalar_refs)+=4;
          Enter(&symb, is_store, 4);
        }
      } else if ( !_is_floating_point && MTYPE_float(type)==FALSE ) {
        SYMBOL symb(wn);

        if (Integer_Ref_Needs_Reg(wn)) {
          (*num_scalar_refs)++;
          Enter(&symb, is_store, 1);
        }
      }
    }
  } else if (OPCODE_is_store(opcode)) {
    Enter_Scalar_Refs(WN_kid0(wn),num_scalar_refs,se_needed,ar);
  } else if (!OPCODE_is_load(opcode)) {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *kid = WN_kid(wn,kidno);
      Enter_Scalar_Refs(kid,num_scalar_refs,se_needed,ar);
    }
  }
}



void SYMBOL_TREE::Enter_Scalar_Refs(WN *wn, ARRAY_REF *ar, 
	SX_INFO *pi, BOOL *can_be_inner, INT num_loops,
	INT outer, INT *num_scalar_refs)
{
  OPCODE opcode = WN_opcode(wn);
  BOOL is_store = FALSE;

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Enter_Scalar_Refs(kid,ar,pi,can_be_inner,num_loops,outer,
			num_scalar_refs);
      kid = WN_next(kid);
    }
    return;
  } 

  if (OPCODE_is_store(opcode)) {
    Enter_Scalar_Refs(WN_kid0(wn),ar,pi,can_be_inner,num_loops,outer,
		      num_scalar_refs);
  } else if (!OPCODE_is_load(opcode)) {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *kid = WN_kid(wn,kidno);
      Enter_Scalar_Refs(kid,ar,pi,can_be_inner,num_loops,outer,
		      num_scalar_refs);
    }
  }
  
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_LDID)  || (oper == OPR_CONST) ||
             (is_store=(OPCODE_operator(opcode) == OPR_STID))) { 
    TYPE_ID type;
    if (is_store) {
      type = OPCODE_desc(opcode);
    } else {
      type = OPCODE_rtype(opcode);
    }
    if (_is_floating_point != MTYPE_float(type)) return;
    SYMBOL symb(wn);
    SX_PITER   ii(&pi->Plist);
    BOOL found = FALSE;
    SX_PNODE *n,*found_n=NULL;
    for (n = ii.First(); n && !found; n = ii.Next()) {
      if (n->Symbol() == symb) {
	found = TRUE;
	SX_PNODE::STATUS status = n->Transformable(outer);
	if (status != SX_PNODE::SE_NOT_REQD) {
	  found_n = n;
	}
      }
    }
    
    if (found_n) {
      ar->Enter_Scalar_Expand(wn,found_n, can_be_inner, num_loops);
    } else if (_is_floating_point && MTYPE_float(type)) {
      if ((type == MTYPE_F4) || (type == MTYPE_F8)) {
        Enter(&symb, is_store, 1);
        (*num_scalar_refs)++;
      } else if ((type == MTYPE_C4) || (type==MTYPE_C8) || 
#if defined(TARG_IA64) || defined(TARG_X8664)
      	(type == MTYPE_F10) ||
#endif
      	(type == MTYPE_FQ)) {
        Enter(&symb, is_store, 2);
        (*num_scalar_refs)+=2;
      } else if (type == MTYPE_CQ
#if defined(TARG_IA64) || defined(TARG_X8664)
      	|| type == MTYPE_C10
#endif
      	) {
        Enter(&symb, is_store, 4);
        (*num_scalar_refs)+=4;
      }
    } else if (_is_floating_point==FALSE && MTYPE_float(type)==FALSE) {
      if (Integer_Ref_Needs_Reg(wn)) {
        (*num_scalar_refs)++;
        Enter(&symb, is_store, 1);
      }
    }
  }
}


// Enter a symbol into the binary true if it's a new symbol
// If is_store, set is_store in the tree
// If it's a new symbol set the weight
void SYMBOL_TREE_NODE::Enter(SYMBOL *symbol, MEM_POOL *pool, BOOL is_store,
				INT weight)
{
  INT result = Symbol_Compare(symbol);
  if (result == 0) {
    if (is_store) _is_store = TRUE;
  } else if (result < 0) {
    if (_left) {
      _left->Enter(symbol,pool,is_store,weight);
    } else {
      _left = CXX_NEW(SYMBOL_TREE_NODE(*symbol,is_store,weight),pool);
    }
  } else {
    if (_right) {
      _right->Enter(symbol,pool,is_store,weight);
    } else {
      _right = CXX_NEW(SYMBOL_TREE_NODE(*symbol,is_store,weight),pool);
    }
  }
}

// Return the sum of each unstored FP node times its weight 
INT SYMBOL_TREE_NODE::Num_Fp_Unstored() const
{
  INT result=0;
  if (!_is_store && MTYPE_float(_symbol.Type))
	result+=_weight;
  if (_left) result += _left->Num_Fp_Unstored();
  if (_right) result += _right->Num_Fp_Unstored();
  return result;
}

// Return the sum of each unstored INT node times its weight 
INT SYMBOL_TREE_NODE::Num_Int_Unstored() const
{
  INT result=0;
  if (!_is_store && !MTYPE_float(_symbol.Type))
	result+=_weight;
  if (_left) result += _left->Num_Int_Unstored();
  if (_right) result += _right->Num_Int_Unstored();
  return result;
}


// REGISTER_MODEL

void 
REGISTER_MODEL::Calculate_Register_Usage(WN* inner, 
                                         INT* fp_regs_used_out, 
                                         INT* int_regs_used_out, 
                                         INT* tlb_out)
{
  Evaluate(inner,NULL,NULL,NULL,fp_regs_used_out,int_regs_used_out,tlb_out);
}

void 
REGISTER_MODEL::Evaluate(WN* inner, 
                         WN2INT* se_needed,
                         HASH_TABLE<WN*,BIT_VECTOR*>* invar_table,
                         double* loop_cycles, 
                         INT* fp_regs_used_out, 
                         INT* int_regs_used_out,
                         INT* tlb_out)
{
  INT base_fp_regs;  
  INT32 fp_regs_used;
  INT32 int_regs_used;
  INT num_fp_scalar_refs = 0;
  INT num_int_scalar_refs = 0;
  INT issue_rate;
  INT num_mem_units;
  INT num_fp_regs, num_fp_array_refs;
  INT num_fp_variant_stores, num_fp_invariant_stores;
  INT num_int_regs, num_int_array_refs;
  INT num_int_variant_stores, num_int_invariant_stores;

  BOOL register_only = (se_needed == NULL && loop_cycles == NULL);

  MEM_POOL_Push(_pool);

  INT base_int_regs = Reserved_Int_Regs;

  // registers required for pipeline
  if (Is_Target_R8K()) {
    issue_rate = 4;
    base_fp_regs = 18;
    num_mem_units = 2;
  } else if (Is_Target_R10K()) {
    issue_rate = 4;
    base_fp_regs = 14;
    num_mem_units = 1;
  } else if (Is_Target_R4K()) {
    issue_rate = 1;
    base_fp_regs = 12;
    num_mem_units = 1;
  } else if (Is_Target_R5K()) {
    issue_rate = 2;
    base_fp_regs = 14;
    num_mem_units = 1;
#ifdef TARG_MIPS
  } else if (Is_Target_Sb1()) {
    issue_rate = 4;
    base_fp_regs = 18;
    num_mem_units = 2;
#endif
#ifdef TARG_X8664
  } else if (Is_Target_x86_64()) {
    issue_rate = 3;
    base_fp_regs = 16;
    num_mem_units = 2;
#endif
#ifdef TARG_IA64
  } else if (Is_Target_Itanium()) {
    Lmt_DevWarn(1, ("TODO: Tune LNO machine model parameters for IA-64"));
    issue_rate = 6;  // 2 bundles with 3 instructions each
    base_fp_regs = 32; // (8+1)*2+6
    num_mem_units = 2;
#endif
#ifdef TARG_PPC32
#define Is_Target_PPC() (1)
  } else if (Is_Target_PPC()) {
    //TODO: verify the parameters for PPC32
    issue_rate = 4;
    base_fp_regs = 18;
    num_mem_units = 2;
#endif
  } else {
    Lmt_DevWarn(1, ("TODO: LNO machine model parameters are just wild guesses"));
    issue_rate = 4;
    base_fp_regs = 4;
    num_mem_units = 2;
  }     

  // registers required for array refs
  ARRAY_REF *array_ref = CXX_NEW(ARRAY_REF(_pool),_pool);
  INT i;
  for (i=0; i<_statement_stack->Elements(); i++) {
    array_ref->Add_References(_statement_stack->Bottom_nth(i), 1,NULL);
  }

  *tlb_out = array_ref->Elements();

  // registers required for scalars
  SYMBOL_TREE *fp_symbol_tree = 
	CXX_NEW(SYMBOL_TREE(/*is_floating_point*/TRUE,_pool),_pool);
  SYMBOL_TREE *int_symbol_tree = 
	CXX_NEW(SYMBOL_TREE(/*is_floating_point*/FALSE,_pool),_pool);
  for (i=0; i<_statement_stack->Elements(); i++) {
    fp_symbol_tree->Enter_Scalar_Refs(_statement_stack->Bottom_nth(i),
	                               &num_fp_scalar_refs,
                                       se_needed,array_ref);
    int_symbol_tree->Enter_Scalar_Refs(_statement_stack->Bottom_nth(i),
	                               &num_int_scalar_refs,
                                       se_needed,array_ref);
  }
  INT scalar_fp_regs = fp_symbol_tree->Num_Fp_Unstored();
  INT scalar_int_regs = int_symbol_tree->Num_Int_Unstored();
  CXX_DELETE(fp_symbol_tree,_pool);
  CXX_DELETE(int_symbol_tree,_pool);

  INT inner_number = Do_Loop_Depth(inner);
  num_fp_array_refs = array_ref->Num_Fp_Refs();
  num_int_array_refs = array_ref->Num_Int_Refs();
  array_ref->Remove_Cse(inner_number, Max_Cse_Dist,
				Find_Step(inner,inner_number));
  array_ref->Mark_Invariants(inner_number);

  INT num_fp_refs;
  INT num_int_refs;
  INT num_fp_spills=0;
  INT num_int_spills=0;

  array_ref->Calc_Regs_And_Refs(
	&num_fp_regs,&num_fp_refs,&num_fp_variant_stores,
	&num_fp_invariant_stores,
	&num_int_regs,&num_int_refs,&num_int_variant_stores,
	&num_int_invariant_stores);

  if (num_fp_invariant_stores > 4*num_fp_variant_stores) {
     base_fp_regs /= 3;  // don't count invariants both as invariants and
                          // as pipelines
  }

  fp_regs_used = base_fp_regs + scalar_fp_regs + num_fp_regs;
  int_regs_used = base_int_regs + scalar_int_regs + num_int_regs;

  BOOL can_reg_allocate = TRUE;
  if (fp_regs_used > Target_FPRs) {
    double fp_refs_per_reg = (num_fp_array_refs + num_fp_scalar_refs)/
                            (num_fp_regs +scalar_fp_regs);
    num_fp_spills =
         (INT)((num_fp_regs+base_fp_regs+scalar_fp_regs - Target_FPRs) *
                        fp_refs_per_reg);
    num_fp_refs += num_fp_spills;
    can_reg_allocate = FALSE;
  }
  if (int_regs_used > Target_INTRs) {
  
    double int_refs_per_reg = (num_int_array_refs + num_int_scalar_refs)/
                            (num_int_regs +scalar_int_regs);
    num_int_spills =
         (INT)((num_int_regs+base_int_regs+scalar_int_regs - Target_INTRs)*
                        int_refs_per_reg);
    num_int_refs += num_int_spills;
    can_reg_allocate = FALSE;
  }

  if (register_only && can_reg_allocate) {
    CXX_DELETE(array_ref,_pool);
    MEM_POOL_Pop(_pool);
    *fp_regs_used_out=fp_regs_used;
    *int_regs_used_out=int_regs_used;
    return;
  }


  double OP_issue = 0.0, op_cycles;
  op_cycles = LOOP_MODEL::OP_Cycles(this, &OP_issue, invar_table, _pool);
  if (op_cycles == -1.0) {
    OP_issue = Count_Op();
  }

  double LOOP_INIT_issue = 2.0;

  // count memory references
  double MEM_issue = ((double) (num_fp_refs+num_int_refs));
  double MEM_rcycles = MEM_issue/num_mem_units;

  double MEM_issue_minus_spills = ((double) 
	(num_fp_refs-num_fp_spills + num_int_refs-num_int_spills));
  double MEM_rcycles_minus_spills = MEM_issue_minus_spills/num_mem_units;

  double issue_limit =
	(OP_issue+LOOP_INIT_issue+MEM_issue) / issue_rate;
  double issue_limit_minus_spills = 
	(OP_issue+LOOP_INIT_issue+MEM_issue_minus_spills)/ issue_rate;
  double resource_cycles = MAX(op_cycles,
        MAX(MEM_rcycles,issue_limit));
  double resource_cycles_minus_spills =
	MAX(op_cycles,
	MAX(MEM_rcycles_minus_spills,issue_limit_minus_spills));

  double cycles = resource_cycles;
  double cycles_minus_spills = resource_cycles_minus_spills;

  if (can_reg_allocate==FALSE &&
      (cycles == cycles_minus_spills)) { // spilling is free so set the 
					 // number of registers
			                 // to Target_FPRs
    can_reg_allocate = TRUE;
    num_fp_regs = Target_FPRs - scalar_fp_regs - base_fp_regs;
    num_int_regs = Target_INTRs - scalar_int_regs - base_int_regs;
  }

  if (can_reg_allocate) {
    if (num_fp_regs + base_fp_regs + scalar_fp_regs > Target_FPRs-2) {
      cycles *= 1.1;  // penalty for being close to unallocatable
    } else if (num_int_regs + base_int_regs + scalar_int_regs >
               Target_INTRs-2){
      cycles *= 1.1;  // penalty for being close to unallocatable
    }
  }

  fp_regs_used = base_fp_regs + scalar_fp_regs + num_fp_regs;
  int_regs_used = base_int_regs + scalar_int_regs + num_int_regs;

  CXX_DELETE(array_ref,_pool);
  MEM_POOL_Pop(_pool);
  *fp_regs_used_out=fp_regs_used;
  *int_regs_used_out=int_regs_used;

  if (*tlb_out > Mhd.L[0].TLB_Entries) {
    cycles += Mhd.L[0].TLB_Miss_Penalty * (Mhd.L[0].TLB_Entries-*tlb_out);
  }

  if (!register_only)
    *loop_cycles=cycles;

}

// How many fp/complex/quad ops in the system
// complex and quad count as two
// This is an approximation for FP_issue and INT_issue
// in the cases where we can't model
double 
REGISTER_MODEL::Count_Op()
{
  double result = 0.0;
  for (INT i = 0; i < _statement_stack->Elements(); i++) {
    result += Count_Op(_statement_stack->Bottom_nth(0));
  }
  return result;
}

double 
REGISTER_MODEL::Count_Op(WN* wn)
{
  double result = 0.0;
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      result += Count_Op(kid);
      kid = WN_next(kid);
    }
  } else if (!OPCODE_is_leaf(opcode)) {
    TYPE_ID ti = OPCODE_rtype(opcode);
    TYPE_ID ti2 = OPCODE_desc(opcode);
    if ((ti == MTYPE_C4) || (ti == MTYPE_C8) || (ti == MTYPE_CQ) ||
        (ti2 == MTYPE_C4) || (ti2 == MTYPE_C8) || (ti2 == MTYPE_CQ) ||
#if defined(TARG_IA64) || defined(TARG_X8664)
	ti == MTYPE_C10 || ti2 == MTYPE_C10 ||
        ti == MTYPE_F10 || ti2 == MTYPE_F10 ||
#endif
        (ti == MTYPE_FQ) || (ti2 == MTYPE_FQ)) {
        result = 2.0;
    } else if ((ti == MTYPE_F4) || (ti == MTYPE_F8) || 
        (ti2 == MTYPE_F4) || (ti2 == MTYPE_F8)) { 
        result = 1.0;
    } else if ((ti == MTYPE_B) || (ti == MTYPE_I1) || (ti == MTYPE_I2) ||
               (ti == MTYPE_I4) || (ti == MTYPE_I8) || (ti == MTYPE_U1) ||
	       (ti == MTYPE_U2) || (ti == MTYPE_U4) || (ti == MTYPE_U8) ||
               (ti2 == MTYPE_B) || (ti2 == MTYPE_I1) || (ti2 == MTYPE_I2) ||
               (ti2 == MTYPE_I4) || (ti2 == MTYPE_I8) || (ti2 == MTYPE_U1) ||
	       (ti2 == MTYPE_U2) || (ti2 == MTYPE_U4) || (ti2 == MTYPE_U8)) {
        result = 1.0;
    }
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *kid = WN_kid(wn,kidno);
      result += Count_Op(kid);
    }
 }
 return result;
}


// Output results to analysis file
void 
LOOP_MODEL::Model_Results_Analysis(INT inner, 
                                   INT num_loops, 
                                   INT outermost_can_be_tiled,
                                   double machine_cycles, 
                                   double cache_cycles, 
                                   double overhead_cycles)
{
  fprintf(LNO_Analysis,"    (IF_INNER %d \n",
      Srcpos_To_Line(SNL_Line_Numbers[inner]));
  fprintf(LNO_Analysis,"        (CYCLES %g \n",_num_cycles_inner);
  if (machine_cycles < 0.0) {
    if (_OP_resource_count==NULL) {
      fprintf(LNO_Analysis,"            (0 \"Can't model fp resources\")");
    } else {
      fprintf(LNO_Analysis,"            (0 \"Requires too many registers\")");
    }
  } else {
    switch (_model_limit)  {
      case MODEL_LIMIT_UNSET: 
        fprintf(LNO_Analysis,"            (%g \"\")\n",machine_cycles);
	break;
      case MODEL_LIMIT_IDEAL: 
        fprintf(LNO_Analysis,"            (%g \"Ideal Schedule\")\n",
							machine_cycles);
	break;
      case MODEL_LIMIT_RES: 
        fprintf(LNO_Analysis,"            ");
	fprintf(LNO_Analysis,"(%g \"Resource Limited Schedule\")\n",
							machine_cycles);
	break;
      case MODEL_LIMIT_LAT: 
        fprintf(LNO_Analysis,"            ");
	fprintf(LNO_Analysis,"(%g \"Latency Limited Schedule\")\n",
							machine_cycles);
	break;
    }
  }
  fprintf(LNO_Analysis,"            %g\n",cache_cycles);
  fprintf(LNO_Analysis,"            %g)\n",overhead_cycles);
  fprintf(LNO_Analysis,"        (FP_REGISTERS %d) \n",_num_fp_regs_inner);

  fprintf(LNO_Analysis,"        (TRANSFORMATIONS\n");

  fprintf(LNO_Analysis,"            (UNTILED_ORDER");
  INT i;
  for (i=outermost_can_be_tiled; i<num_loops; i++) {
    fprintf(LNO_Analysis," %d", 
	Srcpos_To_Line(SNL_Line_Numbers[_new_order_inner[i]]));
  }
  fprintf(LNO_Analysis,")");

  INT unroll_entries = 0;
  for (i=outermost_can_be_tiled; i<num_loops; i++) {
    if (_block_number_inner[i] > 1)
      unroll_entries++;
  }
  if (unroll_entries) {
    fprintf(LNO_Analysis,"\n            (UNROLL");
    for (i=outermost_can_be_tiled; i<num_loops; i++) {
      if (_block_number_inner[i] > 1)
        fprintf(LNO_Analysis," (%d %d)",
                Srcpos_To_Line(SNL_Line_Numbers[i]),_block_number_inner[i]);
    }
    fprintf(LNO_Analysis,")");
  }
  if (_nstrips_inner) {
    fprintf(LNO_Analysis,"\n            (BLOCKING");
    for (INT s = 0; s < _nstrips_inner; s++) {
      INT i = _iloop_inner[s];
      fprintf(LNO_Analysis," (%d %d L%d %d)", 
              Srcpos_To_Line(SNL_Line_Numbers[_new_order_inner[i]]),
              _stripsz_inner[s],
              _striplevel_inner[s],
              Srcpos_To_Line(SNL_Line_Numbers[_new_order_inner[_stripdepth_inner]]));
    }
    fprintf(LNO_Analysis,")");
  }
  fprintf(LNO_Analysis,"))\n");
}

