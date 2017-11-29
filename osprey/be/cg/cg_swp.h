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
/* ====================================================================
 * ====================================================================
 *
 * Module: cg_swp.h
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef cg_swp_INCLUDED
#define cg_swp_INCLUDED "cg_swp.h"

#ifndef USE_STANDARD_TYPES
#define USE_STANDARD_TYPES
#endif

#include <stdio.h>
#include "defs.h"
#include <vector>
#include <map>
#include "mempool.h"
#include "tn.h"
#include "tn_set.h"
#include "op.h"
#include "bb.h"

// #define for SWP INTERNAL USE only
//
#define SWP_DEBUG 0         // do not enable expensive sanity checks
#define SWP_USE_STL 0       // do not use STL instead of hard-coded vector
#define SWP_OPS_OVERHEAD 2  // SWP has 2 overhead OPs (the fake start and stop OPs)
#define SWP_OPS_LIMIT 130   // max # of OPs if STL is not used


// Forward references
class LOOP_DESCR;
class CG_LOOP;
class ROTATING_KEREL_INFO;


//************************************************************************
// Data structure to perform register assignment
//
//  - Initial register allocation for loop variants assigns to each TN*
//    a physical pseudo-register location, where the location numbers 
//    start at zero and are initially unbounded.  There is however an
//    upper bound on the number of rotating logical registers that can
//    be used from each register bank, and if we exceed that number, 
//    register allocation for a modulo scheduled loop fails.
//
//  - ISA registers are registers numbered according to the ISA target
//
//  - reg_allocation is a mapping from a TN to a physical pseudo-register.
//    In terms of rotating "logical" registers, the physical location is
//    the same as the logical register at cycle 0 of the 1st iteration.  
//    After cycle zero the logical register number for a TN increases by
//    one for each successive stage.  The rotating logical register number
//    is a function of:
//
//      i)   the physical pseudo-register location
//      ii)  the sequence of register numbers that can be made to rotate
//      iii) where the OP belongs: the prolog, epilog, body
//      iv)  the stage count of the schedule
//      v)   omega 
//
//    The functions: 
// 
//       INT Get_Register_Offset(INT cycle, INT ii, INT omega),
//       INT Get_Livein_Register_Offset(INT omega), and
//       INT Get_Liveout_Register_Offset(INT sc, INT omega)
//
//    computes the register number difference between the physical 
//    pseudo-register location and the corresponding logical
//    register location at the given cycle (or time-unit), without
//    taking into regard any upper bound on register numbers (i.e. without
//    wrapping the logical registers around a bounded sequence of
//    register numbers).  The reg_allocation is always >= 0, and since
//    the offsets also are >= 0, so are the rotating logical registers.
//
//  - reg2tn_map is a mapping from a ISA register into its represented TN.
//
// ****************************************
// Algorithm to perform register assignment
//
//  - For each loop we assign a physical pseudo-register location to each
//    loop variable TN, by means of Allocate_Loop_Variants().  This
//    register assignment is denoted by "reg_allocation", while the minimum
//    number of rotating registers required is denoted by "rotating_reg_used".
//
//  - For predicate registers we also assign an extra physical pseudo-
//    register location for loop control purposes, which should be live
//    throughout the sc stages of the schedule.  We denote this location 
//    as the "control_predicate_loc".
//
//  - Then, later during SWP_Emit(), we convert the "reg_allocation" and
//    "control_predicate_loc" into unbounded rotating logical register 
//    numbers (by means of SWP_Rename_TNs()) and assign these numbers to
//    TN refs and defs in the schedule.
//
//  - After all loops have been processed, we have a lower bound on the
//    number of rotating registers required by each loop, and at this
//    point we can further convert the unbounded rotating logical register
//    numbers into bounded rotating logical register numbers by means of
//    SWP_Fixup() during Perform_Loop_Optimizations().  At this point we
//    can also rotate the predicate register assignment across the
//    boundaries to force the special control predicate 
//    (control_predicate_loc) to be assigned the lower "sc" rotating 
//    register numbers.
//
//************************************************************************

// Define an ordering for CLASS_REG_PAIR.
//   - required by STL map.
//
inline bool operator<(CLASS_REG_PAIR x, CLASS_REG_PAIR y) {
  return CLASS_REG_PAIR_class_n_reg(x) <  CLASS_REG_PAIR_class_n_reg(y); 
}

inline bool operator==(CLASS_REG_PAIR x, CLASS_REG_PAIR y) {
  return memcmp(&x, &y, sizeof(CLASS_REG_PAIR)) == 0;
}


class SWP_OP_vector; // Defined below.


struct SWP_REG_ASSIGNMENT {
  typedef std::map<CLASS_REG_PAIR, TN*> REG2TN_MAP;
  typedef std::map<TN*, CLASS_REG_PAIR> TN2REG_MAP;

 // 1st rotating registers (
  INT rotating_reg_base[ISA_REGISTER_CLASS_MAX+1];

  // Num of available rotating registers (an upper limit on what we can use)
  INT rotating_reg_avail[ISA_REGISTER_CLASS_MAX+1];

  // Num of rotating registers used by this allocation (<= rotating_reg_avail)
  INT rotating_reg_used[ISA_REGISTER_CLASS_MAX+1];

  // The allocated physical pseudo-register location for the control predicate.
  // This may be any register number, and once we know the size of the 
  // rotating register bank we intend to use, we can add in an appropriate
  // offset to force the control_predicate_loc to be a certain value.
  //
  INT control_predicate_loc;

  // Sets of all non-rotating registers
  REGISTER_SET non_rotating_reg[ISA_REGISTER_CLASS_MAX+1];

  //  reg_allocation is a mapping from a TN into a logical register 
  //  as if it is used at cycle 0
  TN2REG_MAP reg_allocation;

  //  a mapping from a ISA register into the assigned TN.
  REG2TN_MAP reg2tn_map;

  // Calculate the offset in register numbering, when converting 
  // from a physical pseudo-register location for a ref with
  // the given omega at the given cycle to the corresponding 
  // (unbounded) rotating register number.
  INT Get_Register_Offset(INT cycle, INT ii, INT omega) {
    return cycle/ii + omega;
  }

  // Calculate the offset in register numbering, when converting 
  // from a physical pseudo-register location for a live-in def to
  // the corresponding (unbounded) rotating register number.
  INT Get_Livein_Register_Offset(INT omega) {
    return omega;
  }

  // Calculate the offset in register numbering, when converting 
  // from a physical pseudo-register location for a live-out ref to
  // the corresponding (unbounded) rotating register number.
  INT Get_Liveout_Register_Offset(INT sc, INT omega) {
    return sc + omega;
  }

  // Determine the rotating register assigned for 'tn', assuming unbounded
  // rotating register banks.
  // The 'adjustment' is the offset in register numbering, when converting 
  // from a physical pseudo-register location of the current reference to
  // the corresponding (unbounded) rotating register number.
  TN *Get_Register_TN(TN *tn, INT adjustment);
  
  TN *Get_Non_Rotating_Register_TN(TN *tn);

  TN *Get_Control_Predicate(INT stage) const;

  bool Enough_Non_Rotating_Registers(TN_SET *) const;

  // Allocate loop variants to physical pseudo register locations
  // (see cg_swp_allocator.cxx for the definition of this function).
  bool Allocate_Loop_Variants(const SWP_OP_vector& op_state,
			      BB                  *head,
			      BB                  *tail);

  void Print_Allocation(FILE *file);
  
  // Update SWP BB annotation 
  //  - the livein and kill REGSET
  void Update_Annotation(ROTATING_KERNEL_INFO *);

  SWP_REG_ASSIGNMENT();

  // True when we are to trace register allocation for swp
  static bool Trace();

};



//************************************************************************
//   SWP_OP
//************************************************************************

enum SWP_RETURN_CODE {
  SWP_OK,                            // no constraints to disable swp
  SWP_PREP_ONLY,                     // disabled swp due to -SWP:prep_only
  SWP_ASM,                           // disabled swp due to asm statement
  SWP_WORKAROUND,                    // disabled swp due to hardware workaround
  SWP_DEDICATED_ROT_REG,             // disabled swp due to dedicated rot register
  SWP_LOOP_EMPTY,                    // disabled swp due to empty loop
  SWP_LOOP_LIMIT,                    // disabled swp due to loop too large
  REG_ALLOC_FAILED,                  // swp failed: failure to allocate register
  NON_ROT_REG_ALLOC_FAILED,          // swp failed: failure to allocate non-rot reg
  MOD_SCHED_FAILED,                  // swp failed: failure to schedule
  REG_ALLOC_SUCCEEDED,               // reg alloc succeeded
  MOD_SCHED_SUCCEEDED,               // mod sched succeeded
  SWP_LOW_TRIP_COUNT,		     // disable swp due to low trip count
};

enum SCHED_DIRECTION {
  SWP_UNKOWN,
  SWP_TOP_DOWN,
  SWP_BOTTOM_UP,
};


// Use mapping from OP_map_idx to Unique Number 
extern INT *swp_map_tbl;

inline INT SWP_index(OP *op) { return swp_map_tbl[OP_map_idx(op)]; }

// inline INT SWP_index(OP *op) { return OP_map_idx(op); }

struct SWP_OP {
  SCHED_DIRECTION direction;  // direction
  OP     *op;
  bool    placed;
  bool    is_noop;
  double  scale;
  INT     cycle;     // placed cycle
  INT     trials;    // statistics -- number of trials
  INT     modulo_cycle;
  INT     slot;
  void Print(FILE *fp) const;
  INT Index() const { return SWP_index(op); }
  SWP_OP():op(NULL),is_noop(false),placed(false),cycle(0) {}
};

class SWP_OP_vector {

#if SWP_USE_STL
  vector<SWP_OP>  v;
#else
  INT v_size;
  SWP_OP v[SWP_OPS_LIMIT * 4];
#endif

public:
  INT start;
  INT stop;
  INT branch;
  INT num_mops;   // number of memory ops
  INT num_flops;  // number of floating point ops
  
  TN *control_predicate_tn;  // for while-loops
  bool is_doloop;
  bool loop_one_more_time; // the loop will execute at one more time because of entry cond
  INT ii;        // initiation interval in cycles
  INT ii_slots;  // initiation interval in slots after bundling
  TN_SET *tn_invariants;  // set of invariant TNs
  TN_SET *tn_non_rotating; // set of TN that must be allocated to non-rotating reg
  bool succeeded;

  // Statistics
  INT min_ii;     // min II = max(res_mii, recur_mii)
  INT res_min_ii; // resource MII
  INT rec_min_ii; // recurrence MII
  INT min_sl;     // minimium schedule length
  INT sl;         // found schedule length
  INT sc;         // found stage count
  INT previous_trials;
  double prep_time;
  double sched_time;
  double reg_alloc_time;
  double code_gen_time;

#if SWP_USE_STL
  INT size() const { return v.size(); }
  void push_back(const SWP_OP &op) {v.push_back(op);}
#else
  INT size() const { return v_size; }
  void push_back(const SWP_OP &op) { v[v_size++] = op; }
#endif

  SWP_OP& operator[](INT i) { return v[i]; }
  const SWP_OP& operator[](INT i) const { return v[i]; }
  void Verify() const;
  void Print(FILE *fp) const;
  SWP_OP_vector(BB *body, BOOL doloop, MEM_POOL *pool);
};


//************************************************************************
//   MinDist calculation
//************************************************************************

class MinDist {
  static const INT NEG_INF = -999;
#if SWP_USE_STL
  vector< vector<INT> >  mindist;
  INT size() const { mindist.size(); }
#else
  INT mindist[SWP_OPS_LIMIT][SWP_OPS_LIMIT];
  INT mindist_size;
  INT size() const { return mindist_size; }
#endif
  INT found_ii;
  INT Compute(const SWP_OP_vector& v, INT start, INT stop, INT branch, INT ii);
public:
  INT operator()(INT i, INT j) const { return mindist[i][j]; }
  INT Found_ii() { return found_ii; }
  void Print(FILE *fp) const;
  MinDist(const SWP_OP_vector& v, INT start, INT stop, INT branch, INT ii);
};



extern SWP_RETURN_CODE
Modulo_Schedule(SWP_OP_vector& v, INT min_ii, INT max_ii,
		double incr_alpha, double incr_beta,
		INT budget, bool trace, bool trace_details);

extern void SWP_Emit(SWP_OP_vector& op_state, 
		     SWP_REG_ASSIGNMENT& reg_assign,
		     TN *trip_count_tn,
		     BB *head, BB *body, BB *tail,
		     bool is_doloop, bool trace);

extern void Emit_SWP_Note(BB *bb, FILE *file);

#ifdef TARG_IA64
SWP_RETURN_CODE Detect_SWP_Constraints(CG_LOOP &cl, bool trace);
#endif

// dep_graph_manager class makes sure that CG_DEP_Delete is invoked
//   before Perform_SWP exits
class DEP_GRAPH_MANAGER {
private:
  BB *_body;
public:
  DEP_GRAPH_MANAGER( BB *body, TN_SET *non_rotating, MEM_POOL *pool );
  ~DEP_GRAPH_MANAGER();
};

#endif


