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
//  Module: cg_swp_allocator.h
//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:22-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_swp_allocator.h $
//
//  Revision comments:
//
//  01-May-1999 - Initial version
//
//  Description:
//  ------------
//
//    This implements register allocation for a modulo scheduled
//    software pipelined loop body.  The basic algorithm is taken
//    from the paper:
//
//       "Register Allocation for Software Pipelined Loops",
//       by B. R. Rau, M. Lee, P. P. Tirumalai, M. S. Schlansker,
//       in ACM SIGPLAN '92 PLDI-6/92/CA
//
//    The algorithm implemented here is "best-fit" allocation, with
//    "adjacency" and "start-time" as primary and secondary ordering
//    heuristics respectively.  We support register allocation for 
//    loop variants in kernel-only and modulo-variable-expanded code.
//
//  Interface:
//  ----------
//
//    The algorithm is invoked by the constructor to SWP_ALLOCATOR.
//
//    Given an unsorted sequence of lifetimes, the algorithm allocates
//    a physical register location to each lifetime using Rau's algorithm.
//    In the event that the available register-bank is not large enough 
//    to allocate all lifetimes, the status will become 
//    SWP_ALLOC::NEED_MORE_REGS and the locations for unallocated 
//    lifetimes will be set to SWP_LIFETIME::InvalidLoc.  Note that the 
//    translation from unbounded physical locations to logical rotating 
//    registers can be done with a simple modulo operation, after the
//    physical locations have been shifted up to make them all positive
//    (see SWP_REG_ASSIGNMENT::Get_Register_Offset() in cg_swp.h).
//    
//    The lifetimes created for the construction of SWP_ALLOCATOR have
//    and omega and alpha field, which do not directly correspond to 
//    the omega fields in TNs.  The relation is as follows for a given
//    TN "tn":
//
//       SWP_LIFETIME::omega == Max_in_prologue(TN::omega(tn))
//
//       SWP_LIFETIME::alpha == Max_in_epilogue(TN::omega(tn) + 1)
//
//    Both SWP_LIFETIME::omega and SWP_LIFETIME::alpha are zero ("0"),
//    when the TN holds a value that is neither live-in nor live-out.
//
//    The typical use of this interface is as follows:
//
//        1) Create a vector of lifetimes, one for each rotating register
//           bank.
//
//        2) Walk through the schedule entering and updating start, end,
//           omega, and alpha for each TN representing a loop variant.
//
//        3) For each rotating register bank, construct a SWP_ALLOCATOR
//           object, giving it the appropriate set of lifetimes.
//
//        4) Walk through the allocated lifetimes, and find the smallest
//           allocated location.  If it is negative, assign its absolute
//           value to "offset"; otherwise assign zero to "offset.
//
//        5) Again, walk through the allocated lifetimes, and map the tn to
//           the allocated location + "offset" in 
//           SWP_REG_ASSIGNMENT::reg_allocation.
//
//        6) Update the schedule to refer to the allocated registers in 
//           terms of rotating logical registers.  The number of logical
//           registers that need to be rotating can be determined by
//           accessing SWP_REG_ASSIGNMENT::num_allocated_regs().
//
//  Includes:
//  --------
//
//      Whatever file includes this, must also have the following includes:
//
//         #include <stdio.h>
//         #include <vector>
//         #include "defs.h"
//
//     and a definition of Is_True when !defined(STANDALONE_SWP_ALLOCATOR).
//
// ====================================================================
// ====================================================================

#ifndef cg_swp_allocator_INCLUDED
#define cg_swp_allocator_INCLUDED

#ifdef STANDALONE_SWP_ALLOCATOR

extern const char *Swp_Assert_File;
extern INT32       Swp_Assert_Line;
extern void        Swp_Assert(const char *msg, ...);

#define SWP_ASSERT_LOC(a_truth, diag_handler, diag_args) \
   ((a_truth) ? \
    (void) 0 :  \
    ((Swp_Assert_File=__FILE__, Swp_Assert_Line=__LINE__), \
     diag_handler ## diag_args))

#define Is_True(a_truth, args) SWP_ASSERT_LOC(a_truth, Swp_Assert, args)

#include "matrix.h"

struct TN;
struct MEM_POOL;
#define SWP_MEMALLOC(T) std::allocator<T>
#define SWP_USE_POOL(T,P) T::allocator_type()
#define Malloc_Mem_Pool NULL
#define SWP_POOL_Initialize(a, b, c) (a) = NULL
#define SWP_POOL_Delete(a) (void)(1)
#define SWP_POOL_Push(p) (void)(1)
#define SWP_POOL_Pop(p) (void)(1)

#else // !STANDALONE_SWP_ALLOCATOR

#include "mempool_allocator.h"
#include "matrix.h"

#define SWP_MEMALLOC(T) mempool_allocator<T>
#define SWP_USE_POOL(T,P) T::allocator_type(P)
#define SWP_POOL_Initialize(a, b, c) MEM_POOL_Initialize((a), (b), (c))
#define SWP_POOL_Delete(a)           MEM_POOL_Delete(a)
#define SWP_POOL_Push(p)             MEM_POOL_Push(p)
#define SWP_POOL_Pop(p)              MEM_POOL_Pop(p)

#endif // STANDALONE_SWP_ALLOCATOR


// ------------------ SWP_LIFETIME ------------------
// --------------------------------------------------

typedef INT32 SWP_TUNIT; // Time unit as measured in slots, cycles or whatever

class SWP_LIFETIME
{
private:

  SWP_TUNIT _start; // Start unit, inclusive, based at unit zero
  SWP_TUNIT _end;   // End unit, exclusive
  INT32     _omega; // Max # of iterations forward from a live-in def to a use
  INT32     _alpha; // Max # of iterations forward from def to live-out use
  INT32     _loc;   // Physical register location for first iteration of loop
  TN       *_tn;    // The TN for which this lifetime applies (1-to-1 mapping)
  
public:

  static const INT32 InvalidLoc = INT32_MIN;

  SWP_LIFETIME():
    _start(0), _end(0), _omega(0), _alpha(0), _loc(0), _tn(NULL)
  {}

  SWP_LIFETIME(SWP_TUNIT start, 
	       SWP_TUNIT end, 
	       INT32     omega, 
	       INT32     alpha, 
	       TN       *tn = NULL):
    _start(start), _end(end), _omega(omega), _alpha(alpha), _loc(0), _tn(tn)
  {}
  
  SWP_TUNIT start()    const {return _start;}
  SWP_TUNIT end()      const {return _end;}
  INT32     omega()    const {return _omega;}
  INT32     alpha()    const {return _alpha;}
  INT32     location() const {return _loc;}
  TN       *tn()       const {return _tn;}

  void      set_start(INT32 start) {_start = start;}
  void      set_end(INT32 end)     {_end = end;}
  void      set_omega(INT32 omega) {_omega = omega;}
  void      set_alpha(INT32 alpha) {_alpha = alpha;}
  void      set_location(INT32 loc) {_loc = loc;}
  
  void      print(FILE *outf);
  
}; // SWP_LIFETIME


// --------------- The register allocator ---------------
// ------------------------------------------------------

class SWP_ALLOCATOR
{
public:

  enum STATUS {UNALLOCATED, ALLOCATED, NEED_MORE_REGS};

  typedef SWP_MEMALLOC(SWP_LIFETIME)                   LIFETIME_MEMALLOC;
  typedef std::vector<SWP_LIFETIME, LIFETIME_MEMALLOC> LIFETIME_SET;

  typedef SWP_MEMALLOC(INT32)                          INT32_MEMALLOC;
  typedef std::vector<INT32, INT32_MEMALLOC>           INT32_VECTOR;
  
private:

  typedef std::pair<INT32,INT32> INT32_PAIR;

  typedef SWP_MEMALLOC(char)  CHAR_MEMALLOC;
  typedef SWP_MEMALLOC(bool)  BOOL_MEMALLOC;

  typedef std::vector<bool, BOOL_MEMALLOC> BOOL_VECTOR;

  typedef MATRIX<INT32, INT32_MEMALLOC> INT32_MATRIX;
  typedef MATRIX<char, CHAR_MEMALLOC>   CHAR_MATRIX;
  typedef MATRIX<bool, BOOL_MEMALLOC>   BOOL_MATRIX;
  

  LIFETIME_SET _lifetime;   // Lifetimes to be allocated
  SWP_TUNIT    _ii;         // # of time-units in the initiation interval
  INT32        _sc;         // # of ii stages in one complete loop iteration
  INT32        _no_of_regs; // # of physical regs available
  INT32        _first_log_reg; // Lowest # logical reg used in allocation
  INT32        _last_log_reg;  // Highest # logical reg used in allocation
  INT32        _num_allocated_lifetimes;  // May be less than _lifetime.size()
  STATUS       _status;     // Current status
  MEM_POOL    *_mpool;      // Pool to use for all dynamic memory allocation

  // Converting unbound physical register locations to unbound logical
  // rotating register locations.  Logical and physical regs are numbered
  // the same in stage 0 in the first iteration of the modulo scheduled loop.
  //
  INT32 _logical_reg(SWP_TUNIT at_unit, INT32 physical_reg) const 
  {
    return (physical_reg + at_unit/_ii);
  }

  INT32 _livein_logical_reg(INT32 omega, INT32 physical_reg) const 
  {
    return (physical_reg + omega); // Set before loop starts
  }

  INT32 _liveout_logical_reg(INT32 alpha, INT32 physical_reg) const 
  {
    return (physical_reg + (_sc - 1) + alpha); // Accessed after loop ends
  }

  // Wrapping register numbers around a given upper limit, assuming a lower
  // limit of zero.  I.e. the registers are wrapped around 0..num_regs-1
  // registers.
  //
  INT32 _wraparound(INT32 p, INT32 num_regs) const
  {
    INT32 r = p;
    if (r > num_regs)
      r = r % num_regs;
    else
      while (r < 0) r = num_regs + r;
    return r;
  }

  INT32 _wraparound(INT32 p) const
  {
    return _wraparound(p, _no_of_regs);
  }
  
  // Minimum physical register distance, and an upper/lower bound on the
  // legal location ranges for a given lifetime w.r.t. an already allocated
  // lifetime.
  //
  INT32 _regnum_dist(const SWP_LIFETIME &lt1, const SWP_LIFETIME &lt2) const;

  void  _calculate_dist(INT32_MATRIX &dist);

  INT32 _ub_loc(const INT32_MATRIX &dist,
		INT32               allocated_loc,
		INT32               allocated_lt,
		INT32               unallocated_lt) const
  {
    // The allocated_loc for allocated_lt restricts unallocated_lt to be in
    // locations smaller than or equal to the returned "upper bound" value.
    //
    return allocated_loc - dist(unallocated_lt, allocated_lt);
  }

  INT32 _lb_loc(const INT32_MATRIX &dist,
		INT32               allocated_loc,
		INT32               allocated_lt,
		INT32               unallocated_lt) const
  {
    // The allocated_loc for allocated_lt restricts unallocated_lt to be in
    // locations larger than or equal to the returned "lower bound" value.
    //
    return allocated_loc + dist(allocated_lt, unallocated_lt);
  }
  

  // Sorting algorithm
  //
  INT32 _adjacency_order_dist(const INT32_MATRIX &dist,
			      INT32               from_lt, 
			      INT32               to_lt) const;

  void _adjacency_order_sort(const INT32_MATRIX &dist,
			     INT32_VECTOR       &ordered);

  // Allocation algorithm.
  //
  INT32_PAIR _candidate_locs(const INT32_MATRIX          &dist,
			     INT32_VECTOR::const_iterator begin_alloced,
			     INT32_VECTOR::const_iterator end_alloced,
			     INT32                        candidate_lt) const;

  INT32_PAIR _logical_reg_seq(INT32 lt_idx, INT32 physical_reg) const;
   
  bool _has_conflicts(const BOOL_MATRIX &conflicts,
		      INT32              new_allocated_lt,
		      INT32              new_location) const
  {
    return conflicts(new_allocated_lt, _wraparound(new_location));
  }

  void _update_conflicts(BOOL_MATRIX                 &conflicts,
			 const INT32_MATRIX          &dist,
			 INT32_VECTOR::const_iterator begin_unalloced,
			 INT32_VECTOR::const_iterator end_unalloced,
			 INT32                        new_allocated_lt,
			 INT32                        new_location);

  INT32 _num_new_conflicts(const BOOL_MATRIX            &conflicts,
			   const INT32_MATRIX           &dist,
			   INT32_VECTOR::const_iterator begin_unalloced,
			   INT32_VECTOR::const_iterator end_unalloced,
			   INT32                        new_allocated_lt,
			   INT32                        new_location) const;
  
  STATUS _best_fit(const INT32_MATRIX &dist,
		   const INT32_VECTOR &ordered);
  
  STATUS _allocate(bool use_adjacency);

  // Printing and verifying results.
  //
  void _plot_line(CHAR_MATRIX &linebuf,
		  INT32        reg,
		  INT32        from_tunit,
		  INT32        to_tunit,
		  char         plotc);
  void _plot_and_verify(CHAR_MATRIX &linebuf, 
			INT32        num_used_regs,
			INT32        num_loop_iterations);
  void _print_ordered(FILE               *outf, 
		      const char         *msg,
		      const INT32_VECTOR &ordered);
  void _print_dist(FILE *outf, INT32_MATRIX &dist);
  void _print_status(FILE *outf);
  void _print_location_map(FILE *outf, 
			   INT32 num_loop_iterations, 
			   INT32 plot_cols);
  
public:

  typedef LIFETIME_SET::iterator LIFETIME_IT;

  template <class InputIterator>
  SWP_ALLOCATOR(SWP_TUNIT     ii,
		INT32         sc,
		INT32         no_of_available_regs, 
		InputIterator lifetimes_begin,
		InputIterator lifetimes_end,
		MEM_POOL     *mpool = Malloc_Mem_Pool,
		bool          use_adjacency_order = true):
    _lifetime(lifetimes_begin, 
	      lifetimes_end, 
	      SWP_USE_POOL(LIFETIME_SET, mpool)),
    _ii(ii),
    _sc(sc),
    _no_of_regs(no_of_available_regs),
    _first_log_reg(SWP_LIFETIME::InvalidLoc),
    _last_log_reg(SWP_LIFETIME::InvalidLoc),
    _num_allocated_lifetimes(0),
    _status(UNALLOCATED),
    _mpool(mpool)
  {
    _status = _allocate(use_adjacency_order);
  }

  STATUS      status() const {return _status;}
  LIFETIME_IT begin()        {return _lifetime.begin();}
  LIFETIME_IT end()          {return _lifetime.end();}

  INT32 num_allocated_regs() const 
  {
    return (_last_log_reg - _first_log_reg + 1);
  }

  INT32 num_allocated_lifetimes() const 
  {
    return _num_allocated_lifetimes;
  }
    
  void print(FILE *outf, INT32 num_iterations, INT32 plot_width);
  void dump() {print(stderr, _sc+1, 65);}
  
  bool has_conflicts(); // Expensive correctness verification algorithm
#ifdef TARG_IA64
  bool adjust_predicate(const SWP_OP_vector& op_state,INT32 &used_regs) {
     INT32 offset = INT32_MAX;
     
     for (INT32 i = 0; i < _lifetime.size(); ++i) {
       SWP_LIFETIME  *lt = &_lifetime[i];
       if (lt->location() < offset) offset = lt->location();
     }

     //Should get the control predicate's offset first.
     //op_state[i]/ii + Op_omega(op,j);
     
     INT32 control_pred_loc = 0;
     BOOL need_adjust = FALSE;
     for (INT32 i = 0; i < _lifetime.size(); ++i) {
       SWP_LIFETIME  *lt = &_lifetime[i];
       const INT32 loc = lt->location() - offset;

        if ((lt->tn() == NULL || lt->tn() == op_state.control_predicate_tn)) {
          
          control_pred_loc = loc;
        }  
	  }  
     
    INT32 adjust_value = 0;
    //if (need_adjust) {
    for (INT32 i = 0; i < _lifetime.size(); ++i) {
      SWP_LIFETIME  *lt = &_lifetime[i];
      INT32_PAIR     lr = _logical_reg_seq(i, lt->location());
      INT32 start = lr.first - offset;
      INT32 end   = lr.second - offset;
      if ((start < control_pred_loc) && (end >= control_pred_loc)) {
        INT32 temp_adjust = end - control_pred_loc +1;
        //if (temp_adjust == 0) temp_adjust = 1; 
        if (temp_adjust > adjust_value) adjust_value = temp_adjust;
        
      }  

      
     }

     /*if (adjust_value != 0) {
       return FALSE;
     } else {
       return TRUE;
     } */ 
    used_regs +=adjust_value;
    if (adjust_value != 0) {  
      for (INT32 i = 0; i < _lifetime.size(); ++i) {
        SWP_LIFETIME *lt = &_lifetime[i];
        if ((lt->location() - offset) < control_pred_loc ) {
          INT32 new_loc = lt->location() - adjust_value;
          lt->set_location(new_loc);
        }
      }  
    }  
  }  
  //}
#endif   // TARG_IA64  
}; // class SWP_ALLOCATOR

#endif // cg_swp_allocator_INCLUDED
