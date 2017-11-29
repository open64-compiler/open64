/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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
***	This file contains a class PAR_STAT which gives the basic structure
***     of a parallel nest after transformations are applied to it.  We use   
*** 	it to model permuting the loops in an SNL and obtaining cost estimates
***	of each of the permutations.  
***
*** Exported types and functions: 
***
***	class PAR_STAT
***	
***	    Class for modelling statistics for a nest which we propose 
***	    transforming for parallelism.  Each item of type PAR_STAT is   
*** 	    node in a tree (called a PAR_STAT tree) which represents either 
***	    a DO LOOP or a STATEMENT.  A DO LOOP may have a set of children, 
***	    each of which is either a DO LOOP or a STATEMENT.  A STATEMENT 
***	    is always a leaf node. Consequently, we are only modelling SNLs
***	    not arbitrary loop nests. 
***
***	PRIVATE MEMBERS: 
***
***	  PAR_STAT* _next 
***	  PAR_STAT* _prev 
***	  PAR_STAT* _parent
***	
***	    Pointers to the next and previous siblings and the parent of 
***	    the current node. 
***	    
***	  PAR_STAT* _first
***	  PAR_STAT* _last
***
***	    Pointers to the first and last children of the current node.
***	    Should always be NULL for STATEMENT nodes.
***
***	  INT _depth 
***
***	    The depth of this node in the transformed nest.  For DO LOOPs
***	    This is the loop depth.  For STATEMENT nodes, this is one more
***	    than the loop depth of the innermost enclosing loop. 
***	   	
***	  BOOL _is_parallel
***
***	    This should only be set on DO LOOPs.  If set, it indicates that
***	    the loop will be a parallel loop. 
***	
***	  INT _count 
***	
***	    If this is a DO LOOP, the estimated number of iterations for 
***	    the loop.  If this is a STATEMENT, the number of cycles for 
***	    the statement.
***
***	  INT _id
***	
***	    An identifier for the node.  DO LOOPs which are cloned from 
***	    an original have the same identifier as the original. 
***
***	  WN* _wn
***
***	    The whirl node represented by this node.  If a STATEMENT, the 
***	    whole tree rooted at the statement is represented. 
***
***	  BOOL _is_cloned
***
***	    TRUE if the node was cloned from an original, FALSE otherwise. 
***
***	  PAR_STAT()
***
***	    Create a PAR_STAT node with default values for each of the 
***	    fields. 
***
***	  PAR_STAT(PAR_STAT* ps)
***
***	    Create a PAR_STAT node by cloning the PAR_STAT node 'ps'. 
***
***	  Has_Loop()
***
***	    Returns TRUE if the PAR_STAT tree contains a DO LOOP, FALSE 
***	    otherwise. 
***
***	  Is_Outer_Loop()
***
***	    Returns TRUE if the PAR_STAT node represents an outermost
***	    loop, FALSE otherwise.
***
***	  Is_Inner_Loop() 
***
***	    Returns TRUE if the PAR_STAT node represents an innermost loop, 
***	    FALSE otherwise. 
***
***	  Is_Parallel_Enclosed_Loop()
***
***	    Returns TRUE if the PAR_STAT tree is a parallel loop or is 
***         enclosed within a parallel loop, returns FALSE otherwise. 
***
***	  Remove()
***
***	    Remove the PAR_STAT node from its PAR_STAT tree. 
***  
***	  Make_Parent(PAR_STAT* ps_parent, BOOL first)
***
***	    Make 'ps_parent' the parent of this node.  If 'first', make 
***	    it 'ps_parent's first child, otherwise make it 'ps_parent's 
***	    last child. 
***
***	  Make_Sibling(PAR_STAT* ps_sibling, BOOL above)
***
***	    Make 'ps_sibling' the sibling of this node.  If 'above' make 
***	    it 'ps_sibling's immediate older sibling, otherwise make it 
***	    'ps_sibling's immediate younger sibling. 
***
***	  PAR_STAT* Find(WN* wn, BOOL uncloned)
***
***	    Find the node in the PAR_STAT tree which has a '_wn' field equal 
*** 	    to 'wn'.  If 'uncloned', look only for an uncloned node.  Other-
***	    wise, return the first node encountered on a depth first search 
***	    of the tree. 
***
***	  PAR_STAT* Innermost_Sandwiched_Code(PAR_STAT* ps_inner, BOOL above)
***
***	    Return a pointer to the sandwiched innermost code which will 
***	    be obtained when the sandwiched code in the PAR_STAT tree but 
***	    outside the nest represented by 'ps_inner' is distributed out.
***	    If 'above', consider the code above 'ps_inner', otherwise con-
***	    sider the code below 'ps_inner'. 
*** 
***	  PAR_STAT* Distribute(PAR_STAT* ps_inner, BOOL above)
***
***	    Transform the PAR_STAT tree by distributing sandwiched code 
***	    outside of 'ps_inner'.  If 'above' distribute out the code 
***	    above 'ps_inner', otherwise distribute out the code below 
***	    'ps_inner'. Return a pointer to the distributed part.  
***
***	  INT Sanity_Check_Node(FILE* fp)
*** 
***	    Perform sanity checks on the PAR_STAT node.  Return the number 
***	    of errors encountered.  Print the errors to the file 'fp'.
*** 
***	  INT Num_Refs()
*** 
***	    Returns the number of array references in the PAR_STAT tree
***	    which must represent an innermost loop. 
***  
***	  INT Num_Reductions()
***
***	    Returns the number of scalar reductions in the PAR_STAT tree
***	    which must represent an innermost loop.
***
***	  BOOL Invariant_Reduction(WN* wn_istore)
***
***	    Returns TRUE if the array reduction store 'wn_istore' is
***	    invariant to all of the loops inside the parallel nest enclosing
***	    the PAR_STAT, FALSE otherwise.
***
***	  double Reduction_Cost()
***
***	    Returns the number of cycles needed to compute the partial 
***	    sum loops for the reductions in any parallel loop in the 
***	    PAR_STAT tree. 
***
***	  double Loop_Overhead_Cost()
***	
***	    Returns the number of loop overhead cycles represented by
***	    the PAR_STAT tree.
***
***	  double Parallel_Overhead_Cost()
***	
***	    Returns the number of parallel overhead cycles represented by
***	    the PAR_STAT tree.
***
***	PUBLIC MEMBERS: 
***
***	  static INT id_count
***	
***	    The current number of identifiers that have been created for 
***	    the class PAR_STAT. 
***
***	  PAR_STAT(WN* wn_tree, INT nloops, MEM_POOL* pool, PAR_STAT* 
***	    ps_parent = NULL)
***
***	    Create a PAR_STAT tree to represent the SNL rooted at 'wn_tree'
***	    containing 'nloops' using memory from 'pool'.  If 'ps_parent 
***	    != NULL', make this PAR_STAT tree the last child of 'ps_parent'.  
***
***	  PAR_STAT* Distribute_For_Permutation(WN* wn_outer, WN* wn_inner,
***	    INT permutation[], INT nloops)
***
***	    Distribute the PAR_STAT tree as if SNL_Distribute_For_Permutation()
***	    were applied. (See description in snl_dist.cxx). 
***
***	  PAR_STAT* Distribute_For_Parallelism(WN* wn_outer, WN* wn_inner, 
***	    INT nloops, INT split_depth)
***
***	    Distribute the PAR_STAT tree as if SNL_Distribute_For_Parallelism()
***         were applied. (See description in snl_dist.cxx).
***
***	  void Permute_Loops(WN* wn_outer, WN* wn_inner, INT permutation[],
***	    INT nloops)
***
***	    Transform the PAR_STAT tree as if SNL_Permute_Loops() were applied. 
***	    (See description in snl_trans.cxx).  
***
***	  PAR_STAT* Parallel_Interchange(WN* wn_outer, INT permutation[], 
***	    INT nloops, INT parallel_depth, INT split_depth)
***
***	    Transform the PAR_STAT tree as if Parallel_Interchange() were 
***	    applied.  (See description in parallel.cxx). 
***
***	  double Cycle_Count(WN* wn_outer, INT permutation[], INT nloops,
***         INT parallel_depth, SX_PLIST* plist, INT split_depth, 
***	    double machine_cycles, BOOL is_doacross=FALSE)
***
***	    Returns the number of machine cycles represented by the PAR_STAT 
***	    tree.  This models an SNL with outermost loop 'wn_outer' contain-
***	    ing 'nloops' loops.  We apply a 'permutation' of length 'nloops' 
***	    and parallelize the loop which after transformation is at 'par-
***	    allel_depth'.  The list of expandable scalars is given by 'plist'. 
***	    We split the loop nest at 'split_depth' to rid ourselves of LCDs
***	    in imperfect code of the nest above the loop at 'split_depth'.  
***	    The number of machine cycles (which is independent of 'permuta-
***	    tion', and so computed elsewhere), is given by 'machine_cycles'. 
***	    Set 'is_doacross' to TRUE if this is to estimate overhead for
***	    a true doacross loop.
***
***	  INT Sanity_Check(FILE* fp)
***
***	    Perform sanity checks on the PAR_STAT tree.  Return the number 
***	    of errors encountered.  Print the errors to the file 'fp'. 
***
***	  void Print(FILE *fp, INT indent_count = 0) 
***	
***	    Print a user readable representation of the PAR_STAT tree to 
***	    'fp'.  Start printing with an indentation of 'ident_count'
***	    spaces. 
***
***	double SNL_Machine_Cost(WN* wn_outer, INT nloops, INT parallel_depth,
***	  SX_PLIST* plist, double* work_estimate, BOOL include_calls)
***
***	  Return the cost in machine cycles of executing the innermost
***	  loops of all of the inner SNLs within the SNL with outermost loop
***	  'wn_outer' containing 'nloops' loops. The 'plist' contains a list of
***	  scalar expanded variables. The 'parallel_depth' is the depth of the
***	  parallel loop in the transformed nest.  An estimate of one iteration
***	  of the SNL is returned in 'work_estimate'.  If 'include_calls' is 
***	  TRUE, we include an estimate of the cost of calls in the loop.
***
***	double SNL_Min_Parallel_Overhead_Cost(WN* wn_outer, INT nloops,
***	  INT parallel_depth)
***
***	  Returns a lower bound in parallel overhead cycles for executing
***	  the SNL with outermost loop 'wn_outer' of 'nloops' loops, where the
***	  loop at 'parallel_depth' is made parallel.
***
***	NON-MEMBER FUNCTIONS: 
***
***       double SNL_Cache_Cost(WN* wn_outer, INT permutation[],
***         INT nloops,  INT parallel_depth,  INT split_depth,
***         SX_PLIST* plist, double *est_num_iters)
***
***         Return the cache miss cost (in cycles) for an SNL.
**/	   

#ifndef parmodel_INCLUDED
#define parmodel_INCLUDED "parmodel.h"

#ifndef wnmp_INCLUDED
#include "wn_mp.h"        // for REDUCTION_LIST
#endif

class SX_PLIST;

extern double SNL_Machine_Cost(WN* wn_outer, INT nloops, INT parallel_depth,
  SX_PLIST* plist, double* work_estimate, BOOL include_calls); 

extern double SNL_Min_Parallel_Overhead_Cost(WN* wn_outer, INT nloops,
  INT parallel_depth);

extern double SNL_Cache_Cost(WN* wn_outer, INT permutation[], INT nloops,
  INT parallel_depth, INT split_depth, SX_PLIST* plist,
  double *est_num_iters);

#include "config_lno.h"
#define NOMINAL_PROCS                 (LNO_Num_Processors == 0 ? 8 : LNO_Num_Processors) 
#define UNBOUNDED_ITERS         12345678
#define LOOP_CYCLES_PER_ITER           4
#define HASH_SIZE                    100

class PAR_STAT {
  DECL_CXX_ALLOC_AS_FRIEND(PAR_STAT); 
private:
  PAR_STAT* _next; 
  PAR_STAT* _prev; 
  PAR_STAT* _parent; 
  PAR_STAT* _first; 
  PAR_STAT* _last; 
  INT _depth; 
  BOOL _is_parallel; 
  INT _count; 
  double _num_estimated_iters;
  INT _id;  
  WN* _wn; 
  BOOL _is_cloned; 
  PAR_STAT(); 
  PAR_STAT(PAR_STAT* ps); 
  BOOL Has_Loop(); 
  BOOL Is_Outer_Loop(); 
  BOOL Is_Inner_Loop(); 
  BOOL Is_Parallel_Enclosed_Loop(); 
  void Remove(); 
  void Make_Parent(PAR_STAT* ps_parent, BOOL first); 
  void Make_Sibling(PAR_STAT* ps_sibling, BOOL above); 
  PAR_STAT* Find(WN* wn, BOOL uncloned); 
  PAR_STAT* Innermost_Sandwiched_Code(PAR_STAT* ps_inner, BOOL above); 
  PAR_STAT* Distribute(PAR_STAT* ps_inner, BOOL above);
  INT Sanity_Check_Node(FILE* fp); 
  INT Num_Refs(); 
  BOOL Invariant_Reduction(WN* wn_istore);
  void Reduction_List(REDUCTION_LIST *rlist);
  INT Num_Reductions(); 

public: 
  static INT id_count; 
  PAR_STAT(WN* wn_tree, INT nloops, MEM_POOL* pool, PAR_STAT* ps_parent 
    = NULL); 
  PAR_STAT* Distribute_For_Permutation(WN* wn_outer, WN* wn_inner, 
    INT permutation[], INT nloops); 
  PAR_STAT* Distribute_By_Splitting(WN* wn_outer, WN* wn_inner, INT nloops, 
    INT split_depth); 
  void Permute_Loops(WN* wn_outer, WN* wn_inner, INT permutation[], 
    INT nloops); 
  PAR_STAT* Parallel_Interchange(WN* wn_outer, INT permutation[], INT nloops,
    INT parallel_depth, INT sd_split_depth, INT split_depth);
  double Cycle_Count(WN* wn_outer, INT permutation[], INT nloops, 
    INT parallel_depth, SX_PLIST* plist, INT split_depth, 
    double machine_cycles, double *cache_cycles_per_iter,
    BOOL is_doacross=FALSE);
  double Reduction_Cost();
  double Loop_Overhead_Cost(); 
  double Parallel_Overhead_Cost(); 
  INT Sanity_Check(FILE* fp); 
  void Print(FILE *fp, INT indent_count = 0); 
  double Num_Estimated_Iters(void) { return _num_estimated_iters; }
};

extern INT Parallel_Debug_Level; 

extern double SNL_Est_Innermost_Iters(WN* wn_outer, WN *wn_innermost);

#endif /* parmodel_INCLUDED */
