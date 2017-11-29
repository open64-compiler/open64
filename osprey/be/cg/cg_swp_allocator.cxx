/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
//  Module: cg_swp_allocator.cxx
//  $Revision: 1.7 $
//  $Date: 05/12/05 08:59:04-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_swp_allocator.cxx $
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
//    The comments in the algorithm implemented here often refers to
//    a time vs reg-num diagram.
//
// Memory Management Scheme
// ------------------------
//
//    When this is integrated into cg, we create a new mempool for internal
//    use (see SWP_REG_ASSIGNMENT::Allocate_Loop_Variants).  Datastructures
//    declared outside this package will have their own mempools, and as such
//    we can freely push and pop the internal mempool without being concerned
//    about growing datastructures declared using other mempools (e.g. the
//    data-structures defined in SWP_REG_ASSIGNMENT).
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#ifndef STANDALONE_SWP_ALLOCATOR
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <algorithm>
#include <utility>
#include "defs.h"
#include "glob.h"    // for Cur_PU_Name
#include "timing.h"
#include "tracing.h"
#include "cgprep.h"
#include "cg.h"
#include "cg_swp.h"
#include "cg_swp_allocator.h"
#include "cg_swp_options.h"
#include "cg_swp_target.h"
#include "cg_loop.h"

static BOOL Trace_SWP_Alloc = FALSE;
#define SA_TRACE(action) if (Trace_SWP_Alloc) {action;}

// Use STL min/max, not the one from common/com/wn_core.h
//
#ifdef max
#undef max
#endif
#ifdef min
#undef min
#endif

#endif // ifndef STANDALONE_SWP_ALLOCATOR


#ifdef STANDALONE_SWP_ALLOCATOR
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <algorithm>
#include "defs.h"
#include "cg_swp_allocator.h"

#define TFile stdout
#define SA_TRACE(action) action

const char *Swp_Assert_File = NULL;
INT32       Swp_Assert_Line = -1;

void Swp_Assert(const char *msg, ...)
{
  char    c[512];
  va_list arg_ptr;
  if (Swp_Assert_File != NULL)
    (void)sprintf(&c[0], "SWP_ALLOCATOR ASSERTION (%s:%d): %s\n", 
		  Swp_Assert_File, Swp_Assert_Line, msg);
  else
    (void)sprintf(&c[0], "SWP_ALLOCATOR ASSERTION: %s\n", msg);

  va_start(arg_ptr, msg);
  vfprintf(stderr, &c[0], arg_ptr);
  va_end(arg_ptr);
} // Swp_Assert

#endif // STANDALONE_SWP_ALLOCATOR

// Tracing sub-options (for phase TP_SRA = 49):
#define TP_SRA_SUMMARY		0x01    // summary traces
#define TP_SRA_LR_SUMMARY	0x02    // live range summary
#define TP_SRA_LR_DETAIL	0x04    // live range detail
#define TP_SRA_ADJACENCY	0x08    // adjacency matrix
#define TP_SRA_ALLOCATION	0x10    // final allocation
#define TP_SRA_ALLOC_DETAIL	0x20    // allocation detail

static const char ErrTunitChar = '*';
static const char AvailTunitChar = '.';
static const char LiveInChar = '>';
static const char LiveOutChar = '>';

static const char PlotChar[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8',
				'9',
				'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
				'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
				's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
				'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
				'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
				'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'};


//============== SWP_LIFETIME definitions ===============
//=======================================================

void 
SWP_LIFETIME::print(FILE *outf)
{
  if (location() == InvalidLoc)
    fprintf(outf, "[start,end,omega,alpha]=[%d %d %d %d] UNALLOCATED\n",
	    start(), end(), omega(), alpha());
  else
    fprintf(outf, "[start,end,omega,alpha]=[%d %d %d %d] pseudo-loc=%d\n",
	    start(), end(), omega(), alpha(), location());
} // SWP_LIFETIME::print


//============ Minimal distance calculation =============
//=======================================================

INT32
SWP_ALLOCATOR::_regnum_dist(const SWP_LIFETIME &lt1, 
			    const SWP_LIFETIME &lt2) const
{
  // Start out calculating how many stages lt2 must move forward in time to
  // fit in after lifetime lt1 ends in the first iteration (i.e. how much
  // the register identity for lt2 must increase above lt1's identity number
  // in a time versus reg_number diagram).  When lt1.end()-lt2.start() < 0, 
  // then lt2 already starts after lt1 ends, and we may be able to move lt2 
  // down to a lower register number to get a closer horizontal fit; hence
  // dist may be a negative register offset!
  //
  const SWP_TUNIT du = lt1.end() - lt2.start();// Distance in time units
  INT32           d  = du/_ii;                 // Distance in stages==registers

  Is_True(&lt1 != &lt2, 
	  ("Cannot calculate register number distance lifetime and itself"));

  if (du > _ii*d)
    d += 1;  // Ceiling of above divide

  // Next, if lt1 has any live-in values, then these will be carried
  // in through physical registers immediately above the one assigned
  // to lt1, and if lt2 also has live-in values, lt2 must be moved up such
  // that its live-in registers reside above lt1's live-in registers.
  // I.e. we have something like the following (ii==3):
  //
  //   r8 2222222222222222222              live in for lt2
  //   r7 2222222222222222222222           live in for lt2
  //   r6 111111111111    222222222        live in for lt1, first reg for lt2
  //   r5          111111    222222222     first reg for lt1, second for lt2
  //   r4             111111    222222222  second reg for lt1, third for lt2
  //   ... etc
  //
  if ( lt2.omega() > 0)
    d = std::max(d, lt1.omega());
      
  // Similarly for live-out values, since lt2 has been moved to the
  // right (or above) lt1 in the diagram, lt2 must be in a register that
  // is at least lt1.alpha() above lt1.
  //
  if ( lt1.alpha() > 0)
    d = std::max(d, lt1.alpha());

  return d;
} // SWP_ALLOCATOR::_regnum_dist


void
SWP_ALLOCATOR::_calculate_dist(INT32_MATRIX &dist)
{
  // Between each pair of lifetimes, i and j, calculate how many registers
  // apart they must be (dist(i,j)) such that the horizontal distance
  // (i.e. idleness of registers shared between the two lifetimes) is
  // minimized.
  //
  Is_True(dist.rows() == dist.cols(), 
          ("Dimensions for distance matrix must be equal"));

  for (INT i = 0; i < dist.rows(); i++)
    for (INT j = 0; j < i; j++)
    {
      dist(i,j) = _regnum_dist(_lifetime[i], _lifetime[j]);
      dist(j,i) = _regnum_dist(_lifetime[j], _lifetime[i]);
    }
} // SWP_ALLOCATOR::_calculate_dist


//============ Sorting heuristics =============
//=============================================

// A strict weak ordering on indices into a LIFETIME_SET, where we
// order indices primarily by increasing start time, and secondarily
// by how much time is spent in the last stage.  The less time is
// spent in the last stage (or if the last stage is completely filled)
// the better the chances are of fitting another lifetime into the 
// same logical register.
//
class START_TIME_CMP
{
private:

  const SWP_ALLOCATOR::LIFETIME_SET *_lt_set;
  SWP_TUNIT                          _ii;

  const SWP_LIFETIME &_lt(INT32 i) const {return (*_lt_set)[i];}
  
public:

  START_TIME_CMP(const SWP_ALLOCATOR::LIFETIME_SET &lt_set, SWP_TUNIT ii): 
    _lt_set(&lt_set), _ii(ii)
  {}
  
  bool operator ()(INT32 i, INT32 j)
  {
    return (_lt(i).start() == _lt(j).start()?
	    _lt(i).end()%_ii < _lt(j).end()%_ii :
	    _lt(i).start() < _lt(j).start());
  }
}; // START_TIME_CMP


INT32
SWP_ALLOCATOR::_adjacency_order_dist(const INT32_MATRIX &dist,
				     INT32               from_lt, 
				     INT32               to_lt) const
{
  
  // Horizontal distance in the time/register diagram is:
  //
  //    Distance_in_locations*ii + Distance_in_lifetime_cycles
  //
  // The dist(from_lt, to_lt) indicates how many stages "to_lt"
  // must be shift to the right (if positive) or to the left (if
  // negative) to fit as snugly horizontally after "from_lt" as is
  // possible.  The resultant snug horizontally distance in time
  // units is then the distance from the end of "from_lt" to the
  // start of "to_lt", adjusted by the minimum shift stages.
  //
  //
  return (dist(from_lt, to_lt) * _ii + 
	  (_lifetime[to_lt].start() - _lifetime[from_lt].end()));
} // SWP_ALLOCATOR::_adjacency_order_dist


void 
SWP_ALLOCATOR::_adjacency_order_sort(const INT32_MATRIX &dist,
				     INT32_VECTOR       &ordered)
{
  // This is not a regular sort, in the sense that it does not impose a
  // strict weak ordering on the elements.  Instead, it chooses elements
  // in the order they have already been sorted into (e.g. by increasing 
  // start times), and the next element in the order is the one that
  // most tightly fits on the rhs of the previous element chosen.  This
  // produces an order for register allocation, which aims at minimizing
  // the amount of time a register is idle.
  //
  INT32_VECTOR sorted(0, 0, SWP_USE_POOL(INT32_VECTOR, _mpool));
  BOOL_VECTOR  is_sorted(ordered.size(), false,
			 SWP_USE_POOL(BOOL_VECTOR, _mpool));
  INT32        last_lt = 0; // First lifetime in "ordered" set

  Is_True(ordered.size() > 0,
	  ("Attempt to sort empty set of lifetimes"));
  
  sorted.reserve(ordered.size());
  sorted.push_back(ordered[last_lt]);
  is_sorted[last_lt] = true;

  for (INT32 i = 1; i < ordered.size(); i++)
  {
    // Choose a lifetime to succeed current_lifetime in the new ordering,
    // such that the horizontal distance is minimized w.r.t. the up and
    // down movements imposed by the dist matrix.
    //
    INT32 min_dist = INT32_MAX;
    INT32 next_lt = INT32_MIN;
    
    for (INT32 j = 1; j < ordered.size(); j++)
    {
      if (!is_sorted[j])
      {
	// Select the unsorted element with the smallest horizontal distance
	// when placed to the right of the last sorted element.
	//
	const INT32 adj_dist = 
	  _adjacency_order_dist(dist, ordered[last_lt], ordered[j]);
	
	Is_True(adj_dist >= 0,
		("Unexpected negative adjacency order distance"));
	
	if (adj_dist < min_dist)
	{
	  min_dist = adj_dist;
	  next_lt = j;
	}
      } // if not already entered into sorted list
    } // compare last_lt with each lifetime not yet entered into "sorted"
    
    // We have chosen the next lifetime for the sorted list, so move it
    // over, and proceed.
    //
    sorted.push_back(ordered[next_lt]);
    last_lt = next_lt;
    is_sorted[last_lt] = true;
    
  } // for each element in ordered list of lifetimes

  // We are done, so copy the elements over from the sorted list and
  // back to the ordered list.
  //
  std::copy(sorted.begin(), sorted.end(), ordered.begin());
  
} // SWP_ALLOCATOR::_adjacency_order_sort


//============= Manipulation of conflict matrix ==============
//============================================================

void
SWP_ALLOCATOR::_update_conflicts(BOOL_MATRIX                 &conflicts,
				 const INT32_MATRIX          &dist,
				 INT32_VECTOR::const_iterator begin_unalloced,
				 INT32_VECTOR::const_iterator end_unalloced,
				 INT32                        new_allocated_lt,
				 INT32                        new_location)
{
  // Set the conflict matrix to false where the new allocation conflicts
  // with unallocated lifetimes.
  //
  for (INT32_VECTOR::const_iterator unallocated = begin_unalloced;
       unallocated != end_unalloced;
       ++unallocated)
  {
    const INT32 unallocated_lt = *unallocated;
    const INT32 start_loc = 
      _ub_loc(dist, new_location, new_allocated_lt, unallocated_lt) + 1;
    const INT32 end_loc =
      _lb_loc(dist, new_location, new_allocated_lt, unallocated_lt);
    
    for (INT32 loc = start_loc; loc < end_loc; loc++)
      conflicts(unallocated_lt, _wraparound(loc)) = true;
  }
} // SWP_ALLOCATOR::_update_conflicts


INT32
SWP_ALLOCATOR::_num_new_conflicts(
			      const BOOL_MATRIX            &conflicts,
			      const INT32_MATRIX           &dist,
			      INT32_VECTOR::const_iterator begin_unalloced,
			      INT32_VECTOR::const_iterator end_unalloced,
			      INT32                        new_allocated_lt,
			      INT32                        new_location) const
{
  // Count the number of new conflicts created by the given new allocation.
  //
  INT32 sum = 0;

  for (INT32_VECTOR::const_iterator unallocated = begin_unalloced;
       unallocated != end_unalloced;
       ++unallocated)
  {
    const INT32 unallocated_lt = *unallocated;
    const INT32 start_loc = 
      _ub_loc(dist, new_location, new_allocated_lt, unallocated_lt) + 1;
    const INT32 end_loc =
      _lb_loc(dist, new_location, new_allocated_lt, unallocated_lt);
    
    for (INT32 loc = start_loc; loc < end_loc; loc++)
      if (!conflicts(unallocated_lt, _wraparound(loc)))
	sum += 1;
  }
  return sum;
} // SWP_ALLOCATOR::_num_new_conflicts


//============= Allocation method ==============
//==============================================

SWP_ALLOCATOR::INT32_PAIR
SWP_ALLOCATOR::_candidate_locs(const INT32_MATRIX          &dist,
			       INT32_VECTOR::const_iterator begin_alloced,
			       INT32_VECTOR::const_iterator end_alloced,
			       INT32                        candidate_lt) const
{
  // Find a range of locations to be considered for the candidate_lt,
  // such that the range is as tight as possible (smallest to_loc,
  // largest from_loc) and the bound values are guaranteed legal
  // locations for the candidate lifetime.
  //
  INT32 from_loc = INT32_MAX;
  INT32 to_loc = INT32_MIN;
  
  for (INT32_VECTOR::const_iterator allocated = begin_alloced;
       allocated != end_alloced;
       ++allocated)
  {
    const INT32 allocated_lt  = *allocated;  // Lifetime idx
    const INT32 allocated_loc = _lifetime[allocated_lt].location();
    
    const INT32 ub = _ub_loc(dist, allocated_loc, allocated_lt, candidate_lt);
    const INT32 lb = _lb_loc(dist, allocated_loc, allocated_lt, candidate_lt);
    
    if (ub < from_loc)
      from_loc = ub;
    if (lb > to_loc)
      to_loc = lb;
  }
  return INT32_PAIR(from_loc, to_loc);
} // SWP_ALLOCATOR::_candidate_locs


SWP_ALLOCATOR::INT32_PAIR
SWP_ALLOCATOR::_logical_reg_seq(INT32 lt_idx, INT32 physical_reg) const
{
  // Return the smallest and largest logical register numbers required
  // by the given lifetime, given that it is given a real or tentative 
  // physical register location for its definition in the first iteration
  // of the loop.  This must take into consideration both live-in and
  // live-out values.  NOTE: this ties in with our general algorithms for
  // converting physical registers to logical registers.
  //
  
  INT32 first_logical_reg =
    _logical_reg(_lifetime[lt_idx].start(), physical_reg);
  INT32 last_logical_reg =
    _logical_reg(_lifetime[lt_idx].end() - 1, physical_reg);
  
  if (_lifetime[lt_idx].omega() > 0)
    first_logical_reg = 
      std::min(first_logical_reg, 
	       _livein_logical_reg(1, physical_reg));

  if (_lifetime[lt_idx].alpha() > 0)
    last_logical_reg = 
      std::max(last_logical_reg, 
	       _liveout_logical_reg(_lifetime[lt_idx].alpha(), physical_reg));

  Is_True(first_logical_reg >= physical_reg &&
	  last_logical_reg >= first_logical_reg,
	  ("Unexpected logical register range for a physical register"));
  
  return INT32_PAIR(first_logical_reg, last_logical_reg);
} // SWP_ALLOCATOR::_logical_reg_seq


SWP_ALLOCATOR::STATUS
SWP_ALLOCATOR::_best_fit(const INT32_MATRIX &dist,
			 const INT32_VECTOR &ordered)
{
  // Assigns physical register numbers to each member of the _lifetime
  // set, in the order prescribed by the given ordering ("ordered"). The
  // algorithm tries to minimize the logical register pressure in doing
  // the assignment.  We use a conflict matrix, which folds physical
  // registers around the _no_of_regs available when calculating the
  // conflicts.
  //
  INT32_VECTOR::const_iterator current = ordered.begin();
  BOOL_MATRIX                  conflicts(_lifetime.size(), _no_of_regs, false,
					 SWP_USE_POOL(BOOL_MATRIX, _mpool));

  STATUS      status = ALLOCATED;
  const INT32 first_loc = 0;
  INT32_PAIR  logical_regs = _logical_reg_seq(*current, first_loc);
  INT32       smallest_logical_reg = logical_regs.first; // For allocated lt's
  INT32       largest_logical_reg = logical_regs.second; // For allocated lt's

  // See if the first lifetime fits in our set of registers.
  //
  if ((largest_logical_reg - smallest_logical_reg + 1) > _no_of_regs)
  {
      status = NEED_MORE_REGS;
      while (current != ordered.end())
      {
	_lifetime[*current].set_location(SWP_LIFETIME::InvalidLoc);
	++current;
      }
  }
  else
  {
    _lifetime[*current].set_location(first_loc); // First allocation
    _update_conflicts(conflicts, dist, 
		      current+1, ordered.end(),
		      *current, first_loc);

    SA_TRACE(fprintf(TFile, "candidate locs for %d = (%d to %d): chose %d\n",
		      *current, first_loc, first_loc, first_loc));
  }
  
  for (++current; current != ordered.end() && status == ALLOCATED; ++current)
  {
    const INT32 candidate_lt = *current;  // This lifetime
    INT32       best_fit = INT32_MAX;       // No of logical regs
    INT32       best_conflicts = INT32_MAX; // No of new conflicts
    INT32       selected_loc = SWP_LIFETIME::InvalidLoc; // Chosen location
    INT32_PAIR  selected_logical_regs(0,0); // Chosen logic reg sequence

    const INT32_PAIR candidate_locations = 
      _candidate_locs(dist, ordered.begin(), current, candidate_lt);

    Is_True(candidate_locations.first <= candidate_locations.second,
	    ("Unexpected negative range of physical register locations"));

    for (INT32 loc = candidate_locations.second; 
	 loc >= candidate_locations.first;
	 --loc)
    {
      if (!_has_conflicts(conflicts, candidate_lt, loc))
      {
	// Determine how using this physical location impacts the range
	// of logical registers used by all allocated lifetimes.
	//
	logical_regs = _logical_reg_seq(candidate_lt, loc);

	const INT32 total_logical_regs = 
	  (std::max(logical_regs.second, largest_logical_reg) - 
	   std::min(logical_regs.first, smallest_logical_reg) + 1);
	
	// For an equally good fit as far as logical register requirements
	// are concerned, choose the physical location that minimizes 
	// additional conflicts with the thus far unallocated lifetimes.
	//
	INT32 num_conflicts = INT32_MAX; // Conflicts with unalloced
	if (total_logical_regs <= best_fit)
	  num_conflicts = _num_new_conflicts(conflicts, dist,
					     current + 1, 
					     ordered.end(),
					     candidate_lt, loc);
	
	// Choose this physical location if it minimizes the logical register
	// requirements (best fit thus far), or if it meets the logical 
	// register requirements (best_fit) and has a better figure of merit
	// (best_conflicts).
	//
	if (total_logical_regs < best_fit ||
	    (total_logical_regs == best_fit && 
	     num_conflicts < best_conflicts))
	{
	  selected_logical_regs = logical_regs;
	  selected_loc = loc;
	  best_fit = total_logical_regs;
	  best_conflicts = num_conflicts;
	}
      } // if legal allocation
    } // for each candidate location

    SA_TRACE(fprintf(TFile, "candidate locs for %d = (%d to %d): chose %d\n",
		      candidate_lt,
		      candidate_locations.first,
		      candidate_locations.second,
		      selected_loc));

    // Update logical register-sequence in use by currently allocated
    // lifetimes
    //
    if (smallest_logical_reg > selected_logical_regs.first)
      smallest_logical_reg = selected_logical_regs.first;
    if (largest_logical_reg < selected_logical_regs.second)
      largest_logical_reg = selected_logical_regs.second;
    
    // Note the selected allocation and update the conflict matrix for
    // the remaining unallocated lifetimes.
    //
    if (selected_loc == SWP_LIFETIME::InvalidLoc ||
	(largest_logical_reg - smallest_logical_reg + 1) > _no_of_regs)
    {
      status = NEED_MORE_REGS;
      while (current != ordered.end())
      {
	_lifetime[*current].set_location(SWP_LIFETIME::InvalidLoc);
	++current;
      }
    }
    else
    {
      _lifetime[candidate_lt].set_location(selected_loc);
      _update_conflicts(conflicts, dist, 
			current + 1, ordered.end(),
			candidate_lt, selected_loc);
    }
  } // for each lifetime, in ordered sequence

  _num_allocated_lifetimes = current - ordered.begin();
  _first_log_reg = smallest_logical_reg;
  _last_log_reg = largest_logical_reg;
  SA_TRACE(fprintf(TFile, "smallest_logical_reg = %d, largest_logical_reg = %d\n", 
		      smallest_logical_reg, largest_logical_reg));

  return status;
} // SWP_ALLOCATOR::_best_fit


//============= Printing methods ==============
//=============================================

void 
SWP_ALLOCATOR::_print_ordered(FILE               *outf, 
			      const char         *msg,
			      const INT32_VECTOR &ordered)
{
  fprintf(outf, "ORDERED SEQUENCE (%s):", msg);
  for (INT32 i = 0; i < ordered.size(); i++)
    fprintf(outf, " %d", ordered[i]);
  fputc('\n', outf);
} // SWP_ALLOCATOR::_print_ordered


void
SWP_ALLOCATOR::_print_dist(FILE *outf, INT32_MATRIX &dist)
{
  INT32 j; // column
  
  fprintf(outf, "DISTANCE_MATRIX:");
  fprintf(outf, "\n----------------\n");
  fprintf(outf, "   ||");
  for (j = 0; j < dist.cols(); j++)
    fprintf(outf, "%.3d|", j);
  fprintf(outf, "\n===++");
  for (j = 0; j < dist.cols(); j++)
    fprintf(outf, "===+");
  for (INT32 i = 0; i < dist.rows(); i++)
  {
    fprintf(outf, "\n%.3d||", i);
    for (j = 0; j < dist.cols(); j++)
      if (dist(i,j) >= 0)
	fprintf(outf, "%.3d|", dist(i,j));
      else
	fprintf(outf, "%.2d|", dist(i,j));

    fprintf(outf, "\n---++");
    for (j = 0; j < dist.cols(); j++)
      fprintf(outf, "---+");
  }
  fprintf(outf, "\n\n");
} // SWP_ALLOCATOR::_print_dist


void 
SWP_ALLOCATOR::_print_status(FILE *outf)
{
  fprintf(outf, "\nStatus = ");
  switch (_status)
  {
  case UNALLOCATED:
    fprintf(outf, "UNALLOCATED\n");
    break;

  case ALLOCATED:
    fprintf(outf, "ALLOCATED to %d registers\n", num_allocated_regs());
    break;

  case NEED_MORE_REGS:
    fprintf(outf, 
	    "Need more registers; "
#if defined(BUILD_OS_DARWIN)
	    "gave up after allocating %d (of %ld) lifetimes.\n"
	    "The last allocation ran the register count up to %d\n", 
	    _num_allocated_lifetimes,
	    (long) _lifetime.size(),
#else /* defined(BUILD_OS_DARWIN) */
	    "gave up after allocating %d (of %d) lifetimes.\n"
	    "The last allocation ran the register count up to %d\n", 
	    _num_allocated_lifetimes,
	    _lifetime.size(),
#endif /* defined(BUILD_OS_DARWIN) */
	    num_allocated_regs());
    break;

  default:
    fprintf(outf, "\nUNKNOWN STATUS!!\n");
    break;
  }
} // SWP_ALLOCATOR::_print_status


void
SWP_ALLOCATOR::_plot_line(CHAR_MATRIX &linebuf,
			  INT32        reg,
			  SWP_TUNIT    from_tunit,
			  SWP_TUNIT    to_tunit,
			  char         plotc)
{
  const INT32 rotating_reg = _wraparound(reg, linebuf.rows());
  
  Is_True(from_tunit >= 0 && from_tunit <= to_tunit,
	  ("Illegal time-line for reg %d (%d to %d)\n",
	   reg, from_tunit, to_tunit));
  
  for (SWP_TUNIT tunit = from_tunit; 
       tunit < linebuf.cols() && tunit < to_tunit;
       ++tunit)
  {
    if (linebuf(rotating_reg, tunit) == AvailTunitChar)
      linebuf(rotating_reg, tunit) = plotc;
    else
      linebuf(rotating_reg, tunit) = ErrTunitChar;
  }
} // SWP_ALLOCATOR::_plot_line


void
SWP_ALLOCATOR::_plot_and_verify(CHAR_MATRIX &linebuf, 
				INT32        num_used_regs,
				INT32        num_loop_iterations)
{
  // linebuf.rows() == number of rotating registers
  // linebuf.cols() == number of tunits to plot across
  // num_loop_iterations == number of instances of each lifetime to plot
  //
  for (INT32 i = 0; i < _lifetime.size(); ++i)
  {
    const SWP_LIFETIME *lt = &_lifetime[i];
    
    if (lt->location() != SWP_LIFETIME::InvalidLoc)
    {
      for (INT32 livein = lt->omega(); livein > 0; livein--)
      {
	const INT32 livein_end = lt->end() - _ii*livein;
	
	if (livein_end >= 0)
	{
	  INT32 reg = lt->location() + livein;
	  if (reg >= num_used_regs) reg -= num_used_regs;
	  _plot_line(linebuf, 
		     reg,  // Live-in above first defn
		     0,    // Live-in start at tunit 0
		     livein_end,
		     LiveInChar);
	}
      }
      for (INT32 iteration = 0; iteration < num_loop_iterations; ++iteration)
      {
	const char id = PlotChar[i%sizeof(PlotChar)];
	_plot_line(linebuf, 
		   lt->location() - iteration,  // Move one reg down per iter
		   lt->start() + _ii*iteration, // Move _ii to right per iter
		   lt->end() + _ii*iteration,   // Move _ii to right per iter
		   id);
      }
      for (INT32 liveout = lt->alpha(); liveout > 0; liveout--)
      {
	if (liveout <= num_loop_iterations)
	{
	  const INT32 end_iter_reg = lt->location() - num_loop_iterations;
	  const INT32 lt_end = lt->end() + _ii*(num_loop_iterations - liveout);
	  _plot_line(linebuf, 
		     end_iter_reg + liveout,       // Live-out above last defn
		     lt_end,                       // Live-out starts after end
		     std::max(linebuf.cols(), lt_end), // Live-out till the end
		     LiveOutChar);
	}
      }
    }
  }
} // SWP_ALLOCATOR::_plot_and_verify


void
SWP_ALLOCATOR::_print_location_map(FILE *outf,
				   INT32 num_loop_iterations, 
				   INT32 plot_cols)
{
  // Note that we do not take into account lifetimes that span across
  // more than _sc*_ii time units, in the sense that the last time unit
  // of the last iterations may be chopped off in the plot.
  //
  const INT32 tunits_to_print = (_sc + num_loop_iterations)*_ii;
  const INT32 num_used_regs = num_allocated_regs();
  CHAR_MATRIX linebuf(num_used_regs, tunits_to_print, AvailTunitChar,
		      SWP_USE_POOL(CHAR_MATRIX, _mpool));
  
  // Get the plot in the linebuf matrix.
  //
  _plot_and_verify(linebuf, num_used_regs, num_loop_iterations);

  // Print out registers in order larger to smaller.  We split the plot
  // such we plot at most 120 tunits per plot, by means of the outer
  // loop.
  //
  const INT32 num_plots = ((linebuf.cols() - 1)/plot_cols) + 1;
  
  for (INT32 plot = 1; plot <= num_plots; ++plot)
  {
    const INT32 start_tunit = (plot - 1) * plot_cols;
    const INT32 end_tunit = std::min(plot * plot_cols, linebuf.cols());
    
    fprintf(outf, "\nPlot for time units %d to %d\n",
	    start_tunit, end_tunit - 1);
    
    for (INT32 reg = linebuf.rows() - 1; reg >= 0; --reg)
    {
      fprintf(outf, "R%.2d  ", reg);
      for (INT32 tunit = start_tunit; tunit < end_tunit; ++tunit)
	fputc(linebuf(reg, tunit), outf);
      fputc('\n', outf);
    }
  }
} // SWP_ALLOCATOR::_print_location_map
  

void
SWP_ALLOCATOR::print(FILE *outf, INT32 num_iterations, INT32 plot_width)
{
  if (_status == ALLOCATED || _status == NEED_MORE_REGS)
  {
    SWP_POOL_Push(_mpool);
    _print_location_map(outf, num_iterations, plot_width);
    SWP_POOL_Pop(_mpool);
  }

  fprintf(outf, "\nLIFETIMES (ii=%d, sc=%d)\n", _ii, _sc);
  for (INT32 i = 0; i < _lifetime.size(); ++i)
  {
    const char           id = PlotChar[i%sizeof(PlotChar)];
    SWP_LIFETIME * const lt = &_lifetime[i];
    const INT32_PAIR     lr = _logical_reg_seq(i, lt->location());

    fprintf(outf, "Lifetime=%d, Plot=%c ", i, id);
    lt->print(outf);
    fprintf(outf, "   Logical regs %d..%d ", lr.first, lr.second);
#ifndef STANDALONE_SWP_ALLOCATOR
    if (lt->tn() == NULL)
      fprintf(outf, "For special NIL tn\n");
    else
      fPrint_TN(outf, "For %s\n", lt->tn());
#endif // STANDALONE_SWP_ALLOCATOR
  }
} // SWP_ALLOCATOR::print


//============== SWP_ALLOCATOR verification ===============
//=========================================================

bool SWP_ALLOCATOR::has_conflicts()
{
  bool conflict = false;

  SWP_POOL_Push(_mpool);
  {
    const INT32 num_loop_iterations = _sc + 1;
    const INT32 tunits_to_print = (_sc + num_loop_iterations)*_ii;
    const INT32 num_used_regs = num_allocated_regs();
    CHAR_MATRIX linebuf(num_used_regs, tunits_to_print, AvailTunitChar,
			SWP_USE_POOL(CHAR_MATRIX, _mpool));
    
    // Get the plot in the linebuf matrix, where any conflicts show up as
    // an ErrTunitChar.
    //
    _plot_and_verify(linebuf, num_used_regs, num_loop_iterations);
    
    for (INT32 reg = linebuf.rows() - 1; reg >= 0; --reg)
      for (INT32 tunit = linebuf.cols() - 1; tunit >= 0; --tunit)
	if (linebuf(reg, tunit) == ErrTunitChar)
	  conflict = true;
  }
  SWP_POOL_Pop(_mpool);
  
  return conflict;
} // SWP_ALLOCATOR::has_conflicts


//============== SWP_ALLOCATOR algorithm ==================
//=========================================================

SWP_ALLOCATOR::STATUS 
SWP_ALLOCATOR::_allocate(bool use_adjacency)
{
  STATUS status;
  
  SWP_POOL_Push(_mpool);
  {
    INT32_VECTOR lt_order(0, 0, SWP_USE_POOL(INT32_VECTOR, _mpool));
    INT32_MATRIX dist(_lifetime.size(), _lifetime.size(), -1, 
		      SWP_USE_POOL(INT32_MATRIX, _mpool));
  
    if ( Get_Trace ( TP_SRA, TP_SRA_ADJACENCY ) ) {
      SA_TRACE(fprintf(TFile,
		      "\n-------------SWP_ALLOCATOR-------------------\n\n"));
    } else {
      SA_TRACE(fprintf(TFile,
		      "\n-------------SWP_ALLOCATOR-------------------\n\n"));
    }

    if (_lifetime.size() == 0)
      return ALLOCATED;
  
    // Calculate the dist(i,j) matrix; i.e. for any two lifetimes, i and j,
    // compute the minimum distance in REGISTER numbers between the two 
    // lifetimes when j is allocated to a higher register number than i.
    // The diagonal is set to -1, while all other elements are >= 0.
    //
    _calculate_dist(dist);
    // _mve_calculate_dist(dist);
    SA_TRACE(_print_dist(TFile, dist));
    if ( Get_Trace ( TP_SRA, TP_SRA_ADJACENCY ) ) {
      _print_dist(TFile, dist);
    } else {
      SA_TRACE(_print_dist(TFile, dist));
    }

    // In the lt_order vector, create a sorted ordering of indices into
    // the lifetime set.  First put the indices in order of increasing start
    // time, resolving conflicts by ordering elements by increasing end time
    // modulo _ii.  This is our secondary sorting heuristics.
    //
    lt_order.reserve(_lifetime.size());
    for (INT32 i = 0; i < _lifetime.size(); i++) 
      lt_order.push_back(i); // Initial order is same as in _lifetime set.

    SWP_POOL_Push(_mpool);
    std::sort(lt_order.begin(), lt_order.end(), 
	      START_TIME_CMP(_lifetime, _ii));
    SWP_POOL_Pop(_mpool);
    if ( Get_Trace ( TP_SRA, TP_SRA_ADJACENCY ) ) {
      _print_ordered(TFile, "start-time", lt_order);
    } else {
      SA_TRACE(_print_ordered(TFile, "start-time", lt_order));
    }

    // Next, apply our primary sorting heuristics:  Starting with the 
    // first element in the order we already have, choose the next
    // lifetime such that the TUNITS distance from the the previous 
    // lifetime in the ordered sequence is minimized.  The secondary
    // order is maintained by making this a "stable" sort.
    //
    if (use_adjacency)
    {
      SWP_POOL_Push(_mpool);
      _adjacency_order_sort(dist, lt_order);
      // _mve_adjacency_order_sort(lt_order);
      SWP_POOL_Pop(_mpool);
      if ( Get_Trace ( TP_SRA, TP_SRA_ADJACENCY ) ) {
	_print_ordered(TFile, "adjacency", lt_order);
      } else {
	SA_TRACE(_print_ordered(TFile, "adjacency", lt_order));
      }
    }
  
    // Next, apply the best-fit algorithm.
    //
    status = _best_fit(dist, lt_order);
    // status = _mve_best_fit(lt_order);
  }
  SWP_POOL_Pop(_mpool);

  return status;
  
} // SWP_ALLOCATOR::_allocate


//========= SWP_REG_ASSIGNMENT::Allocate_Loop_Variants =========
//==============================================================

#ifndef STANDALONE_SWP_ALLOCATOR

typedef SWP_ALLOCATOR::LIFETIME_SET    LIFETIME_SET;
typedef std::map<TN*, INT32>           TN2INT32_MAP;
typedef SWP_REG_ASSIGNMENT::TN2REG_MAP TN2REG_MAP;
typedef SWP_ALLOCATOR::INT32_VECTOR    INT32_VECTOR;

inline bool
SWP_Is_Variant(TN_SET *invariants, TN *tn)
{
  return (TN_is_register(tn) && 
	  !TN_is_dedicated(tn) && 
	  !TN_SET_MemberP(invariants, tn));
}


static void
SWP_Update_Lifetime(TN2INT32_MAP &tn2lt_map,
		    LIFETIME_SET &lt, 
		    INT           ii_slots,
		    INT           at_slot,
		    INT           end_of_defn_slot,  // Zero for use
		    INT           omega,             // Zero for a defn
		    TN           *tn,
		    bool          defn)
{

  // A defn sets the start-slot of the lifetime, and it also constrains
  // the end-slot to be at least one higher than the start_slot.
  //
  // A use sets the end-slot of the lifetime.  The end-slot is one 
  // slot after the last use.  Until a defn is seen the start-slot
  // will remain INT32_MAX.
  //
  // We have separate mechanisms for finding the alpha and omega attributes,
  // based on traversal of prologue and epilogue code.
  //
  const INT32 start_slot = (defn? at_slot : INT32_MAX);
  const INT32 end_slot = (defn? 
			  end_of_defn_slot :
			  at_slot + ii_slots*omega + 1);
  
  Is_True((!defn || (omega==0 && start_slot < end_slot)),
	  ("SWP_REG_ASSIGNMENT: Encountered negative lifetime range!"));
  
  // Access the map entry for this TN.
  //
  pair<TN2INT32_MAP::iterator, bool> insert_status = 
    tn2lt_map.insert(TN2INT32_MAP::value_type(tn, -1));

  if (insert_status.second)  // A new map-entry was inserted for the tn
  {
    const SWP_LIFETIME new_lt(start_slot, end_slot,
			      0/*omega*/, 0/*alpha*/, tn);
    
    (insert_status.first)->second = lt.size(); // New lt will be appended
    lt.push_back(new_lt);
  }
  else  // A lifetime already exists for this tn
  {
    SWP_LIFETIME * const ltp = &lt[(insert_status.first)->second];

    if (start_slot < ltp->start())
      ltp->set_start(start_slot);
    if (end_slot > ltp->end())
      ltp->set_end(end_slot);
  }

  // Trace insertion:
  if ( Get_Trace ( TP_SRA, TP_SRA_LR_DETAIL ) ) {
    fprintf ( TFile, "[sra]\tTN%d\t%3d:%3d %s\t%s\n",
	      TN_number(tn),
	      start_slot, end_slot,
	      defn?"def":"use",
	      insert_status.second ? "" : "(new)" );
  };

} // SWP_Update_Lifetime

	   
static void
SWP_Gather_Lifetime_Extents(TN2INT32_MAP        &tn2lt_map,
			    LIFETIME_SET         lt[ISA_REGISTER_CLASS_MAX+1],
			    const SWP_OP_vector& op_state,
			    MEM_POOL            *apool)
{
  INT          i;
  INT32_VECTOR last_slot_for_mcycle(op_state.ii, -1024/*illegal value*/, 
				    SWP_USE_POOL(INT32_VECTOR, apool));

  // Calculate the last slot number for each mcycle.
  //
  for (i = 0; i < op_state.size(); i++)
    if (op_state[i].op != NULL &&
	op_state[i].slot > last_slot_for_mcycle[op_state[i].modulo_cycle])
      last_slot_for_mcycle[op_state[i].modulo_cycle] = op_state[i].slot;

  // Traverse all operations and create all lifetimes representing loop
  // variant values.  This will set the "start" and "end" tunit (i.e. slot
  // number) for each lifetime, but not omega and alpha.
  //
  for (i = 0; i < op_state.size(); i++)
  {
    OP *op = op_state[i].op;

    if (op != NULL)
    {
      // Both a defn and a use occurs at a slot within a cycle.
      //
      // A defn must be live up until the end of this cycle, i.e. up until
      // the end_of_defn_slot, such that we do not reuse the same register
      // for two defs within the same cycle.  Note that op_state[i].slot
      // denotes the slot-offset within one stage of the swp loop.  The
      // stage-number is given by cycle/ii.
      //
      const INT stage_first_slot =
	(op_state[i].cycle/op_state.ii) * op_state.ii_slots;
      const INT op_at_slot = stage_first_slot + op_state[i].slot;
      const INT end_of_defn_slot = 
	(stage_first_slot + 
	 last_slot_for_mcycle[op_state[i].modulo_cycle] + 1);

      // Trace OP:
      if ( Get_Trace ( TP_SRA, TP_SRA_LR_DETAIL ) ) {
	fprintf ( TFile, "[sra]\t" );
	Print_OP_No_SrcLine(op);
      }

      // Traverse all results (i.e. all defs).
      //
      for (INT k = 0; k < OP_results(op); k++)
      {
	TN *tn = OP_result(op, k);

	if (SWP_Is_Variant(op_state.tn_non_rotating, tn))
	  SWP_Update_Lifetime(tn2lt_map,
			      lt[TN_register_class(tn)], 
			      op_state.ii_slots, 
			      op_at_slot,
			      end_of_defn_slot,
			      0/*omega*/,
			      tn, 
			      TRUE/*defn*/);
      } // For each op result

      // Traverse all operands (i.e. all uses).
      //
      for (INT j = 0; j < OP_opnds(op); j++)
      {
	TN *tn = OP_opnd(op, j);

	if (SWP_Is_Variant(op_state.tn_non_rotating, tn))
	  SWP_Update_Lifetime(tn2lt_map,
			      lt[TN_register_class(tn)], 
			      op_state.ii_slots, 
			      op_at_slot,
			      0/*end_of_defn_slot*/,
			      OP_omega(op,j),
			      tn,
			      FALSE/*not defn*/);
      } // For each op opnd
    } // If valid op
  } // For each op
} // SWP_Gather_Lifetime_Extents


static INT
Get_Cycle_Of_Defining_SWP_Op (const SWP_OP_vector& op_state, TN *def_tn)
{
  INT cycle = -1;	// returns -1 if definition not found
  for (INT i = 0; i < op_state.size(); i++) {
    OP *op = op_state[i].op;
    if (op) {
      for (INT k = 0; k < OP_results(op); k++) {
	if (OP_result(op,k) == def_tn) {
	    Is_True(cycle < 0, ("multiple defs of tn %d in SWP body", TN_number(def_tn)));
	    cycle = op_state[i].cycle;
	}
      }
    }
  }
  Is_True(cycle >= 0, ("no def of tn %d in SWP body", TN_number(def_tn)));
  return cycle;
}
		   
static void
SWP_Gather_Lifetimes(LIFETIME_SET         lt[ISA_REGISTER_CLASS_MAX+1],
		     const SWP_OP_vector& op_state,
		     BB                  *head,
		     BB                  *tail,
		     MEM_POOL            *apool)
{
  CG_LOOP_BACKPATCH *bp;        // For travesring prologue/epilogue
  TN2INT32_MAP       tn2lt_map; // Maps each TN to an entry in a lifetime set

  // Create the lifetimes in the "lt" vector of lifetime sets, and map each
  // TN with a lifetime to its entry idx in the appropriate lifetime set.
  // After this each lifetime will have its start and end cycles set, but
  // omega (live-in recurrences) and alpha (liveout recurrences) will not yet
  // be set.
  //
  SWP_Gather_Lifetime_Extents(tn2lt_map, lt, op_state, apool);
  
  // Traverse the prologue to set the live-in attributes (i.e. omega) 
  // of lifetimes.
  //
  for (bp = CG_LOOP_Backpatch_First(head, NULL);
       bp != NULL;
       bp = CG_LOOP_Backpatch_Next(bp))
  {
    TN * const body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    if (SWP_Is_Variant(op_state.tn_non_rotating, body_tn)) {
      const INT              omega = CG_LOOP_BACKPATCH_omega(bp);
      TN2INT32_MAP::iterator map = tn2lt_map.find(body_tn);
    
      Is_True(map != tn2lt_map.end(),
	      ("SWP_REG_ASSIGNMENT: Cannot find lifetime for prologue tn %d",
	       TN_number(body_tn)));

      SWP_LIFETIME *this_lt = &(lt[TN_register_class(body_tn)][map->second]);
    
      if (this_lt->omega() < omega)
	this_lt->set_omega(omega);
    }
  }

  // Traverse the epilogue to determine live-out attributes (i.e. alpha)
  // of lifetimes.
  //
  for (bp = CG_LOOP_Backpatch_First(tail, NULL);
       bp != NULL;
       bp = CG_LOOP_Backpatch_Next(bp)) {
    TN * const             body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    if (SWP_Is_Variant(op_state.tn_non_rotating, body_tn)) {
      INT              alpha = CG_LOOP_BACKPATCH_omega(bp) + 1;
#ifdef TARG_IA64
      // For live-out predicate tns, if the def is in a different
      // stage than the branch, then the last iteration will clear
      // the predicate values (because a false qp on a cmp.unc
      // clears both predicate defs).  So in that case we need
      // to preserve the predicate values by allocating space for both
      // the predicate and its next value (p+1).  We can do this
      // by increasing the alpha, so that it preserves the value
      // from the iteration(s) before the final iteration.
      // The number of iterations to preserve is 
      // #stages - (stage_of_def_of_predicate)
      if (TN_register_class(body_tn) == ISA_REGISTER_CLASS_predicate) {
	INT def_cycle = Get_Cycle_Of_Defining_SWP_Op (op_state, body_tn);
	INT def_stage = (def_cycle/op_state.ii) + 1;
	DevWarn("liveout swp predicate, change alpha by %d", (op_state.sc-def_stage));
        // TEMPORARY:  have option to turn this off till are sure it is okay.
        if ( ! Get_Trace(TP_SRA,0x1000) ) {
	  alpha += (op_state.sc - def_stage);
        }
      }
#endif

      TN2INT32_MAP::iterator map = tn2lt_map.find(body_tn);
      Is_True(map != tn2lt_map.end(),
	      ("SWP_REG_ASSIGNMENT: Cannot find lifetime for epilogue tn %d",
	       TN_number(body_tn)));
      
      SWP_LIFETIME *this_lt = &(lt[TN_register_class(body_tn)][map->second]);
      
      if (this_lt->alpha() < alpha)
	this_lt->set_alpha(alpha);
    }
  }

  // Insert a special lifetime to account for the special control predicate 
  // used by the schedule (i.e. SWP_REG_ASSIGNMENT::control_predicate_loc).
  // It is live-in, and can be considered defined in slot 0 of stage 0 for
  // each iteration.  It's lifetime ends after the last slot of the iteration
  // (i.e. after sc stages).
  //
#ifdef TARG_IA64
  if (op_state.is_doloop) {
    const SWP_LIFETIME 
      cntrl_pred_lt(0                                /*start*/, 
		    op_state.sc * op_state.ii_slots  /*end*/,
		    0                                /*omega*/,
		    0                                /*alpha*/,
		    NULL/*tn*/);
  
    lt[ISA_REGISTER_CLASS_predicate].push_back(cntrl_pred_lt);
  } else {
    TN *ptn = op_state.control_predicate_tn;
    Is_True(ptn != NULL, ("couldn't locate control predicate"));
    TN2INT32_MAP::iterator map = tn2lt_map.find(ptn);
    Is_True(map != tn2lt_map.end(),
	    ("SWP_REG_ASSIGNMENT: Cannot find lifetime for control predicate tn %d",
	     TN_number(ptn)));

    SWP_LIFETIME *this_lt = &(lt[TN_register_class(ptn)][map->second]);
    this_lt->set_start(0);
  }
#endif
} // SWP_Gather_Lifetimes


static bool
SWP_Allocate_Rotating_Regs(bool                trace,
			   MEM_POOL           *apool,
			   const SWP_OP_vector& op_state,
			   ISA_REGISTER_CLASS  reg_class,
			   INT                 ii,       // initiation interval
			   INT                 sc,       // stage count
			   INT                 num_regs, // num available regs
			   const LIFETIME_SET &lt,       // set of lifetimes
			   INT                &ctr_pred_loc, // ctrl predicate
			   INT                &used_regs,  // # regs allocated
			   TN2REG_MAP         &allocation)
{
  // Determines any special location (e.g. for a special predicate lifetime),
  // and updates the "allocation" for the TNs with a physical psudo-register
  // location that is larger than zero.  Also sets the minimum numbers of
  // registers needed for this allocation.  Note that there can be at most
  // one special location (location without a TN) per allocation, and the
  // returned value will be SWP_LIFETIME::InvalidLoc when there is no
  // special location.
  //
  SWP_ALLOCATOR allocator(ii, sc, num_regs, lt.begin(), lt.end(), apool);
  bool          is_alloced = (allocator.status() == SWP_ALLOCATOR::ALLOCATED);
  INT max_alloced_reg = 0;

  // Only assert for conflicts when we do not wish to see them in a trace
  // file.
  //
  if (!trace)
    Is_True(!allocator.has_conflicts(), 
	    ("Conflicts in SWP_Allocate_Rotating_Regs!!"));

  // Record the minimum number of registers needed for this allocation.
  //
  used_regs = allocator.num_allocated_regs();
  ctr_pred_loc = SWP_LIFETIME::InvalidLoc;

#ifdef TARG_IA64
   //Added to adjust predicate register allocation.
  if (reg_class == ISA_REGISTER_CLASS_predicate) {
      #ifdef TARG_IA64
      BOOL success = allocator.adjust_predicate(op_state,used_regs);
      /*if (!success) {
        printf("Do not success because of SWP p63!\n"); 
        return FALSE;
      } */ 
      #endif
  }
#endif
  
  if (is_alloced)
  {
    LIFETIME_SET::const_iterator lt_itr;
    INT32                        offset = INT32_MAX;
    
    // First determine the offset we need to subtract from locations to 
    // make the smallest location equal to zero (i.e. the offset is the
    // smallest location).
    //
    for (lt_itr = allocator.begin(); lt_itr !=  allocator.end(); ++lt_itr)
      if (lt_itr->location() < offset)
	offset = lt_itr->location();

    // Next, update the "allocation" map to reflect our results, and
    // also determine the location of any register without a TN ptr.
    //
    for (lt_itr = allocator.begin(); lt_itr !=  allocator.end(); ++lt_itr)
    {
      const INT32 loc = lt_itr->location() - offset;

#ifdef TARG_IA64
      if (reg_class == ISA_REGISTER_CLASS_predicate && 
	  (lt_itr->tn() == NULL || lt_itr->tn() == op_state.control_predicate_tn)) {
	Is_True(ctr_pred_loc == SWP_LIFETIME::InvalidLoc,
		("SWP_REG_ASSIGNMENT: Allocation has more than one "
		 "control predicate location"));
	
	ctr_pred_loc = loc;
      }
      if (reg_class != ISA_REGISTER_CLASS_predicate  ||
	  lt_itr->tn() != NULL) {
	Is_True(allocation.find(lt_itr->tn()) == allocation.end(),
		("SWP_REG_ASSIGNMENT: Multiple locations assigned to a TN"));

	CLASS_REG_PAIR rp;
	// Replacing the following two lines with a procedure call to avoid
	// a g++ bug.
	// Set_CLASS_REG_PAIR_rclass(rp, reg_class);
	// Set_CLASS_REG_PAIR_reg(rp, loc);
	Set_CLASS_REG_PAIR(rp, reg_class, loc);
	allocation[lt_itr->tn()] = rp;

        if (reg_class == ISA_REGISTER_CLASS_integer
	    && loc > max_alloced_reg)
	{
		max_alloced_reg = loc;
	}
      }
#endif
    }
  } // if (is_alloced)

  if (reg_class == ISA_REGISTER_CLASS_integer) {
    // "used_regs" is used to determine how many stacked rotating regs
    // to use.  But in some cases (pv 793591) it is wrong because it
    // returns the last-first range but first != 0.  We still need
    // to reserve rotating regs starting at 0, so check for when
    // max_alloced_reg > used_regs and use the larger number.
    // Maybe we should always use max_alloced_reg, but I'm being
    // cautious since I don't fully understand the original code. 
    if ( Get_Trace(TP_SRA,TP_SRA_ALLOCATION) ) {
      fprintf ( TFile,
		"used_regs = %d, max_alloced_reg = %d\n",
		used_regs, max_alloced_reg );
    }
    if ((used_regs/8) < (max_alloced_reg/8)) {
      DevWarn ( "increasing # rotating regs from %d to %d",
		used_regs, max_alloced_reg );
    }
    if (used_regs < max_alloced_reg) {
      used_regs = max_alloced_reg;
    }
  }

  if ( Get_Trace(TP_SRA,TP_SRA_ALLOCATION)
    && allocator.begin() != allocator.end() )
  {
#ifdef TARG_IA64
    const ISA_REGISTER_CLASS_INFO *reg_info = 
      ISA_REGISTER_CLASS_Info(reg_class);

    if (ISA_REGISTER_CLASS_INFO_Name(reg_info) != NULL)
      fprintf(TFile, 
	      "\n=============== SWP Allocator (%s) ================\n",
	      ISA_REGISTER_CLASS_INFO_Name(reg_info));
    else
#endif
      fprintf(TFile, 
	      "\n========= SWP Allocator (Unknown reg class) =========\n");
      
    allocator.print(TFile, sc+3/*num_iterations*/, 70/*columns*/);
  }
  return is_alloced;
    
} // SWP_Allocate_Rotating_Regs


bool
SWP_REG_ASSIGNMENT::Allocate_Loop_Variants(const SWP_OP_vector& op_state,
					   BB                  *head,
					   BB                  *tail)
{
  // Allocate loop variants to pseudo physical register locations, using
  // SWP_ALLOCATOR and updating SWP_REG_ASSIGNMENT::rotating_reg_used and
  // SWP_REG_ASSIGNMENT::reg_allocation.  Prerequisites for this call is
  // that the following be initialized, in addition to the paremeter values:
  //
  //   SWP_REG_ASSIGNMENT::rotating_reg_avail
  //
  bool      allocated = true;
  MEM_POOL  allocator_pool;
  
  Trace_SWP_Alloc = Get_Trace(TP_SWPIPE,0x40);
  SWP_POOL_Initialize(&allocator_pool, "CG_SWP_ALLOCATOR POOL", FALSE);
  SWP_POOL_Push(&allocator_pool);
  {
    INT32                ctrl_pred_loc; // Location of control predicate reg.
    ISA_REGISTER_CLASS   reg_class;
    LIFETIME_SET         lt[ISA_REGISTER_CLASS_MAX+1];
    
    // Force "lt" to use the allocator_pool (size is zero, so copy is trivial).
    //
    FOR_ALL_ISA_REGISTER_CLASS(reg_class) {
      lt[reg_class] = 
	LIFETIME_SET(0, SWP_LIFETIME(),
		     SWP_USE_POOL(LIFETIME_SET, &allocator_pool));
    }

    // Walk through the kernel and epilogue, gathering up the lifetime data
    // needed to do allocation in each rotating register bank in "lt".  Each
    // lifetime will be expressed in terms of slots (not cycles).
    //
    SWP_Gather_Lifetimes(lt, op_state, head, tail, &allocator_pool);

    // For each register bank, do the allocation and write the results into 
    // this SWP_REG_ASSIGNMENT.
    //
    FOR_ALL_ISA_REGISTER_CLASS(reg_class) {
      SWP_POOL_Push(&allocator_pool);
      allocated = 
	(allocated &&
	 SWP_Allocate_Rotating_Regs(Trace(),
				    &allocator_pool,
				    op_state,
				    reg_class,
				    op_state.ii_slots,
				    op_state.sc,
				    rotating_reg_avail[reg_class],
				    lt[reg_class],
				    ctrl_pred_loc,
				    rotating_reg_used[reg_class],
				    reg_allocation));
      SWP_POOL_Pop(&allocator_pool);

#ifdef TARG_IA64
      if (reg_class == ISA_REGISTER_CLASS_predicate)
	control_predicate_loc = ctrl_pred_loc;
      else
      {
	Is_True(ctrl_pred_loc == SWP_LIFETIME::InvalidLoc,
	      ("SWP_REG_ASSIGNMENT: Encountered special location for wrong "
	       "register class"));
      }
#endif
    }
    if (Trace())
      fprintf(TFile, 
	      "\n========= Done with SWP Allocation =========\n");
  }

  if (Trace())
    Print_Allocation(TFile);

  SWP_POOL_Pop(&allocator_pool);
  SWP_POOL_Delete(&allocator_pool);

  return allocated;
} // SWP_REG_ASSIGNMENT::Allocate_Loop_Variants


bool SWP_REG_ASSIGNMENT::Trace()
{
  return Get_Trace(TP_SWPIPE, 0x80)
      || Get_Trace(TP_SRA, TP_SRA_LR_SUMMARY);
}


void SWP_REG_ASSIGNMENT::Print_Allocation(FILE *file)
{
  TN2REG_MAP::iterator p;
  for (p = reg_allocation.begin(); p != reg_allocation.end(); p++) {
    TN *tn = (*p).first;
    CLASS_REG_PAIR rp = (*p).second;
    fprintf(file, "Allocation: TN%d CLASS%d REG%d\n", TN_number(tn), 
	    CLASS_REG_PAIR_rclass(rp), CLASS_REG_PAIR_reg(rp));
  }
}

#endif // ifndef STANDALONE_SWP_ALLOCATOR
