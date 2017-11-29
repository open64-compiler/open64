/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


// -*-C++-*-

/**
*** Description: 
***
***  Exported types and functions:
*** 
***    enum INTERCHANGE_TYPE 
***
***	Indicates how the bounds will need to change if the loops are 
***	permuted.  
***
***	INT_PERMUTABLE: loops are fully permutable without changing bounds
***	INT_INVARIANT: loops are not INT_PERMUTABLE but are permutable i
***	  without changing bounds for the given permutation
*** 	INT_GENERAL: loops are not INT_PERMUTABLE or INT_INVARIANT but do
***	  have non-messy access vectors 
***	INT_NONE: none of the above 
***
***    enum PAR_DIR_TYPE
***
***	Indicates which kind of relevant parallel directive is on a parallel 
***	loop, if any. 
***
***	PD_NONE: no parallel directive 
***	PD_NO_CONCURRENT: No CONCURRENT directive (don't choose this loop 
***	  for parallelization) 
***	PD_PREFER_CONCURRENT: PREFER CONCURRENT directive (choose this loop 
***	  before choosing loops without this directive) 
***	
***    WN* Minimal_Kernel(WN* wn_outer, 
***                       INT nloops)
***
***     For the SNL with outermost loop 'wn_outer' and containing 'nloops'
***     loops, check whether each do loop is good and attempt to standardize
***	and finalize the index variables of the loops of the SNL, going 
***     innermost out.  Returns the outermost loop for which this was 
***     successful.
***
***    BOOL General_Permutation(WN* wn_outer,
***                             INT permutation,
***                             INT nloops)
***
***     Returns TRUE if all loops in the SNL with outermost loop 'wn_outer'
***     containing 'nloops' loops can be permuted in the given 'permutation'
***     by using the access vectors.  Returns FALSE otherwise. 
***
***    BOOL Invariant_Permutation(WN* wn_outer,
***                               INT permutation[],
***                               INT nloops)
***
***	Returns TRUE if all loops in the SNL with outermost loop 'wn_outer'
***	containing 'nloops' loops which are permuted in the given 'permuta-
***	tion' are within the kernel of the SNL's inner invariant loops. 
***	Returns FALSE otherwise.
***
***    BOOL Fully_Permutable_Permutation(WN* wn_outer,
***					 INT nloops)
***
***	Returns TRUE if the loops within the SNL with outermost loop 'wn_outer'
***	consisting of 'nloops' loops can be arbitrarily permuted without 
***     rewriting the loop bounds.  Returns FALSE otherwise. 
***
***    SNL_DEP_MATRIX** Inv_Dep_Info(WN* wn_outer, INT nloops, 
***	BOOL check_privates = FALSE, BOOL definitely = FALSE)
***
***	For the SNL with outermost loop 'wn_outer' which contains 'nloops'
***	loops, return an array of SNL_DEP_MATRIXes which summarize the 
***	dependences in the body of the SNL which DO NOT depend on the 
***	permutation order. If 'check_privates', then exclude dependences 
***	from privatizable arrays. 
***
***    BOOL SNL_Legal_Perm_Deps(SNL_DEP_MATRIX* sdm_body, INT permutation[],
***	INT nloops)
***
***	Returns TRUE if the SNL whose dependences are summarized in 
***	'sdm_body' can be legally permuted with the given 'permutation of
***	length 'nloops'.  Returns FALSE otherwise. 
***
***    void Auto_Parallelization(PU_Info* current_pu, WN* func_nd); 
***
***	Perform automatic parallelization on the tree of loops rooted
***	at 'func_nd'.
***
***    BOOL Outermore_Parallel_Construct(WN* wn_loop, BOOL check_suggested)
***
***	Returns TRUE if some parallel construct encloses the loop
***	'wn_loop' or if 'check_suggested' 'wn_loop' has the 'Suggested_
***	Parallel' bit set in the DO_LOOP_INFO.
***
***    BOOL Innermore_Parallel_Loop(WN* wn_loop, BOOL check_suggested)
***
***	Return TRUE if 'wn_loop' or any loop enclosed by it is already
***	a parallel loop.  If 'check_suggested', include loops for which 
***	the 'Suggested_Parallel' bit is set in the DO_LOOP_INFO.
***
***    void Mark_Auto_Parallelizable_Loops(WN* func_nd)
***
***	Mark the 'Parallelizable' field for all loops parallelizable
***	in 'func_nd' in some legal permutation within their SNLs.
***
***    BOOL Is_Privatizable_With_Context(WN* loop,WN* wn,BOOL defnitely)
***
***	Check privatizability of 'wn' in 'loop' and make sure the sources
***	and sinks of this node are also privatizable.
***
***    double Compute_Work_Estimate(double machine, double cache)
***
***     Compute estimate of total cycles per iteration of a parallelized
***     loop.
***
***    BOOL Cannot_Concurrentize(WN* wn_loop)
***
***	Returns TRUE if 'wn_loop' cannot be concurrentized because of a 
***	pragma or because it is the serial version of a concurrent loop.
***
***    class PARALLEL_INFO 
***	
*** 	This class stores all information about an SNL to which we apply
***	a permutation and all of the enabling transformations to obtain 
***     that permutation and a parallel loop within it. 
***
***	Given a '_permutation' of length '_nloops', we first transform the
***	SNL to get the loops in that order.  If the SNL is perfectly nested
***	this is straightforward, but if it conatins sandwiched code, this 
***	is a it more difficult.  
***
***	We perform two types of distribution to get the loops in permuta-
***	tion order.  The first splits out unexpandable scalars in sandwiched
***	code.  By splitting, we mean a simple type of distribution which 
***	takes a particular loop in the SNL and distributes all of the code
***	above and below that loop out of the nest.  So, for example, in 
***	the nest: 
***	
***	  do i = 1, n 
*** 	    a(i) = s 
***	    s = b(i) + c(i) 
***	    do j = 1, n 
*** 	      do k = 1, n 
***	        d(i,j,k) = e(i,j,k) + a(i,i,i)
***	      end do 
***	    end do 
***	  end do 
***
***	The variable "s" is an unexpandable scalar which can be split out
***	of the nest to yield:   
***
***	  do i = 1, n 
*** 	    a(i) = s 
***	    s = b(i) + c(i) 
***	  end do 
***	  do i = 1, n 
***	    do j = 1, n 
*** 	      do k = 1, n 
***	        d(i,j,k) = e(i,j,k) + a(i,i,i)
***	      end do 
***	    end do 
***	  end do 
***
***	It is now possible to parallelize the second "i" loop.  In this 
***	case we say that we have split the SNL at depth (or level) 1. 
***	In parallel.cxx, this depth is stored in 'sd_split_depth', and 
***	passed around among the various functions there.  In this class, 
***	the '_sd_split_depth' holds the value for this permutation.  
***
***	Once all of the unexpandable scalars have been split out, we dis- 
***	tribute the nest to get the loops in permutation order.  How much 
***	distribution is done is will depend on the particular permutation 
***	and where the sandwiched code is located.  Consider the example SNL: 
***
***  	  do i = 1, n 
*** 	    a(i) = 2 * b(i) 
***	    do j = 1, n 
***	      c(i,j) = a(i) * d(i,j) 
***	      do k = 1, n 
*** 	        e(i,j,k) = c(i,j) - f(i,j,k) 
*** 	        do l = 1, n 
*** 		  g(i,j,k,l) = h(i,j,k,l) + e(i,j,k)
***	        end do 
*** 	      end do 
***	    end do 
***	  end do 
***
***	and suppose that we are applying the permutation [1 0 3 2].  To 
***	get the loops in this order, we must interchange the "i" and "j"
***	loops, and interchange the "k" and "l" loops.  When we are done, 
***	we will have the nest: 
***
***  	  do i = 1, n 
*** 	    a(i) = 2 * b(i) 
***	  end do 
***	  do j = 1, n 
***	    do i = 1, n 
***	      c(i,j) = a(i) * d(i,j) 
***	      do k = 1, n 
*** 	        e(i,j,k) = c(i,j) - f(i,j,k) 
***	      end do 
***	      do l = 1, n 
*** 	        do k = 1, n 
*** 		  g(i,j,k,l) = h(i,j,k,l) + e(i,j,k)
***	        end do 
*** 	      end do 
***	    end do 
***	  end do 
***
***	Note that the resulting loop nest is not always (and in this case 
***	is definitely not) an SNL.  Also note that we did not distribute 
***	out the "c(i,j) = a(i) * d(i,j)" between the "j" and the "k" loops,
***	since this was not needed to get the desired permutation. 
***
***	The SNL code contains two different permutation algorithms: a 
***	general algorithm and an invariant algorithm.  The PARALLEL_INFO 
***	stores a variable "_int_type" which tells us which of these al-
***	gorithms to perform.  
***
***	Having applied the permutation to the SNL, we now try to parallelize
***	the outermost parallel loop.  This may also require a distribution, 
***	either to remove unexapndable scalars or array references with loop 
***	carried dependences.  Consider the example: 
***
***	  do i = 2, n 
***	    a(i) = c(i) + 15 
***	    c(i-1) = a(i-1) + 20 
***	    do j = 1, n 
***	      b(i,j) = d(j,i) + 40 
***	    end do 
***	  end do 
***
***	In this case, we have loop-carried dependences within the "i" loop,
***     but we can apply distribution to get: 
***
***	  do i = 2, n 
***	    a(i) = c(i) + 15 
***	    c(i-1) = a(i-1) + 20 
***	  end do 
*** 	  do i = 2, n
***	    do j = 1, n 
***	      b(i,j) = d(j,i) + 40 
***	    end do 
***	  end do 
***
***	It is now possible to parallelize the second "i" loop.  We say that
***	we have split (or distributed) the SNL for parallelization at depth 
***	(or level) 1.  As in the case of splitting to remove unexpandable 
***	scalars to get the desired permutation, we allow only one level of 
***	splitting to achieve this purpose (although a more complex algorithm
***	is possible).  In parallel.cxx, this depth is called the 'split_depth'
***	and is a formal parameter to many of the functions there.  In the 
***	PARALLEL_INFO class, it is stored in '_split_depth'. 
***
***	Finally, we must also store the depth of the parallel loop in the 
***	transformed nest.  This is given in the PARALLEL_INFO variable 
***	'_parallel_depth'.  We also compute a '_cost' for each PARALLEL_INFO
***	choice.  The choice with the lowest cost is the one we select for 
***	transformation.
***
***	If doall parallelization at '_parallel_depth' failed, we will
***	try parallelizing with doacross transformation at the same level
***	(aka parallelization with synchronization). The requirements are
***	that the immediate outer loop has to be perfectly nested. We then
***	use the same parallelizable test as in doall, except that we
***	ignore the invariant array dependences which will be covered
***	by synchronization. If it is parallelizable, we compute the cost
***	for doacross parallelization. The cost model takes into
***	consideration of initial delay due to skewing, sync overhead.
***	Since the MP loop will be interchanged out of the immediate
***	outer loop, the parallelization overhead may be slightly lower.
***	Additionally, we set _is_doacross to TRUE, compute the tile
***	size for synchronization (which controls granularity of synch),
***	and the synchronization vectors, _sync_distances[2].
***
***	The code is transformed and we set the 'Suggested_Parallel' bit 
***	in the DO_LOOP_INFO of all loops we believe should go parallel. 
***	We then call 'Perform_ARA_And_Parallelization()' to actually 
***	parallelize the loops.  A DevWarn is printed if an unexpected loop
***	is parallelized. 
**/

#ifndef parallel_INCLUDED
#define parallel_INCLUDED "parallel.h"

#ifndef pu_info_INCLUDED 
#include <sys/types.h>
#endif
#ifndef pu_info_INCLUDED 
#include "pu_info.h"
#endif
#ifndef sxlist_INCLUDED
#include "sxlist.h"
#endif
#ifndef sdlist_INCLUDED
#include "sdlist.h"
#endif
#ifndef snl_INCLUDED
#include "snl.h"
#endif

enum INTERCHANGE_TYPE {INT_NONE, INT_PERMUTABLE, INT_INVARIANT, INT_GENERAL}; 

enum PAR_DIR_TYPE {PD_NONE, PD_PREFER_CONCURRENT, PD_NO_CONCURRENT}; 

class PARALLEL_INFO {
  double _cost;
  INT _work_estimate; 
  INT _nloops;
  INT _permutation[SNL_MAX_LOOPS];
  INTERCHANGE_TYPE _int_type; 
  INT _sd_split_depth; 
  INT _split_depth; 
  INT _parallel_depth;
  INT _is_doacross;
  INT _doacross_tile_size;
  INT _sync_distances[2];
  INT _doacross_overhead;
  double _reduction_cycles;
  double _loop_cycles;
  double _parallel_cycles;
  double _machine_cycles;
  double _cache_cycles_per_iter;
  double _cache_cycles;
  BOOL _preferred_concurrent;
  WN*  _wn_outer;

 public:
  PARALLEL_INFO(INT nloops); 
  PARALLEL_INFO(WN* wn_outer, INT permutation[], INT nloops, 
    INT parallel_depth, INTERCHANGE_TYPE int_type, SNL_DEP_MATRIX** sdm_array, 
    BOOL sdm_scl[], SX_INFO* sx_info, SD_INFO* sd_info, INT sd_split_depth,
    double machine_cycles, double work_estimate); 
 PARALLEL_INFO(WN* wn_outer, INT permutation[], INT nloops, 
    INT parallel_depth, INTERCHANGE_TYPE int_type, SNL_DEP_MATRIX** sdm_array, 
    BOOL sdm_scl[], SX_INFO* sx_info, SD_INFO* sd_info, INT sd_split_depth,
    double machine_cycles, double work_estimate, BOOL dummy); 
  ~PARALLEL_INFO() {}
  double Cost() {return _cost;} 
  INT Work_Estimate() {return _work_estimate;}
  INT Parallel_Depth() {return _parallel_depth;}
  INT Parallel_Loop() { return _permutation[_parallel_depth - Do_Loop_Depth(_wn_outer)]; }
  INT* Permutation() {return _permutation;} 
  INT Permutation(INT i) {return _permutation[i];} 
  INT Nloops() {return _nloops;}
  INTERCHANGE_TYPE Int_Type() {return _int_type;}
  INT Sd_Split_Depth() {return _sd_split_depth;}
  INT Split_Depth() {return _split_depth;}
  INT Is_Doacross() {return _is_doacross;}
  INT Doacross_Tile_Size() {return _doacross_tile_size;}
  INT* Sync_Distances() {return _sync_distances;}
  INT Doacross_Overhead() {return _doacross_overhead;}
  void Set_Preferred(void) { _preferred_concurrent = TRUE; }
  void Print(FILE *f);
  double Reduction_Cost(void) { return _reduction_cycles; }
  double Parallel_Overhead_Cost(void) { return _parallel_cycles; }
  double Loop_Cost(void) { return _loop_cycles; }
  double Machine_Cost(void) { return _machine_cycles; }
  double Cache_Cost(void) { return _cache_cycles; }
};

extern WN* Minimal_Kernel(WN* wn_outer, INT nloops);
extern BOOL General_Permutation(WN* wn_outer, INT permutation[], INT nloops);
extern BOOL Invariant_Permutation(WN* wn_outer, INT permutation[], INT nloops);
extern BOOL Fully_Permutable_Permutation(WN* wn_outer, INT nloops);
extern SNL_DEP_MATRIX** Inv_Dep_Info(WN* wn_outer, INT nloops, 
  BOOL check_privates = FALSE, BOOL definitely = FALSE);
extern BOOL SNL_Legal_Perm_Deps(SNL_DEP_MATRIX* sdm_body, INT permutation[],
  INT nloops); 
extern void Mark_Auto_Parallelizable_Loops(WN* func_nd); 
extern BOOL Outermore_Parallel_Construct(WN* wn_loop, BOOL check_suggested); 
extern BOOL Innermore_Parallel_Loop(WN* wn_loop, BOOL check_suggested); 
extern void Auto_Parallelization(PU_Info* current_pu, WN* func_nd);
extern BOOL Is_Privatizable_With_Context(WN* loop, WN* wn, BOOL defnitely);
extern double Compute_Work_Estimate(double machine, double cache);
extern BOOL Cannot_Concurrentize(WN* wn_loop); 
extern void Mark_Critical_Section_Loops(WN* func_nd);
extern void Mark_Threadprivate_Loops(WN* func_nd);
extern BOOL Outermore_Parallel_Construct_Or_Lego_Loop(WN* wn_loop);
extern BOOL Innermore_Parallel_Or_Lego_Loop(WN* wn_loop);
extern void IPA_LNO_Evaluate_Call_Infos(WN* func_nd);
extern void IPA_LNO_Unevaluate_Call_Infos(WN* func_nd);

#ifdef KEY
extern INT Last_Apo_Loop_Id;
#endif

#endif /* parallel_INCLUDED */ 
