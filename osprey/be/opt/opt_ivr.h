/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_ivr.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_ivr.h,v $
//
// Revision history:
//  27-JAN-95 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
//   The Induction Variable Recognition in a LOOP structure
//
//   With SSA form of the program, the induction variable can be
//   characterized by:
//   
//   Follow the U-D chain in the global value numbering table
//   within the loop, we can find the strong connected regions.
//
//   a. If the SCR has only one phi-function.
//   b. There is only addition and subtraction operation in the SCR.
//   c. The right hand side of the addition and subtraction is loop
//      invariant.
//   d. All load and store in this SCR has know base address at
//      compile time.
//   f. If the SCR has more than one phi-function, the phi-function
//      within the loop body must have identical operands.
//   
//   Base on these rules, here is the algorithm for mongoose IVR:
//
//   For each loop, get to the loop end block.
//     For all phi-function for LDID in the block do
//       Identify the operand that is defined in the loop, the other
//       operand is the init value if it turns out to be IV
//       If the global copy propagation and dead code elimination
//       have already been performed.  The value of the definition
//       inside the loop must be an fully propagated expression, which
//       is the SCR.  For simplicity, we will expand the expression.
//       If the SCR satisfy the first 3 rules, it is an IV.  Because
//       we don't look into any phi-function for IVAR, so rule 4 is
//       satified.  Because the global copy propagation does not
//       propagate into phi-function, the rule 5 is not implemented.
//
//   Once all IVs are identified, we can then merge them by replacing
//   those secondary IVs with the primary IV.  Since this is done
//   befor the global copy propagation, we can place a initialization
//   at the beginning of the loop body with a specific equation.  We
//   use the triple (var, var_init, var_step ) to represent an
//   induction variable.  If variable i and j are recognized as IV,
//   (i, init_i, step_i) and (j, init_j, step_j).  We can convert j
//   to be function of i.  The function is:
//   j = init_j + (step_j/step_i)// (i - init_i)
//   The major factor is really (step_j/step_i).  If it is 1, j
//   becomes i + init_j - init_i.  If (init_j - init_i) is less than
//   32K, there is not any register requirement.  If (step_j/step_i)
//   is not a constant, we then need a register for its value.  We
//   probably end up using another register for
//   (step_j/step_i)*init_i.  With all these register consideration,
//   CG would definition want to have some control over the IV
//   conversion.
//
// Note: IV recognition is using copy propagation to do the real code
//       replacement.  Similarly, the Loop Normalization use the same
//       approach.  It is important to coordinate these two
//       transformation.  Currently, we do the IV recognition first.
//
// ====================================================================
// ====================================================================


#ifndef opt_ivr_INCLUDED
#define opt_ivr_INCLUDED	"opt_ivr.h"
#ifdef _KEEP_RCS_ID
static char *opt_ivrrcs_id = 	opt_ivr_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#define USE_STANDARD_TYPES

#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif
#ifndef opt_ssa_INCLUDED
#include "opt_ssa.h"
#endif
#ifndef opt_htable_INCLUDED
#include "opt_htable.h"
#endif

#include "opt_main.h"

class BB_NODE;


class IV_CAND : public SLIST_NODE {
private:
  PHI_NODE *_phi;
  CODEREP  *_var;   // phi result
  CODEREP  *_init_var;  
  CODEREP  *_incr_var;  
  CODEREP  *_init_value;
  CODEREP  *_step_value;  // the value that the var incremented every time
  MTYPE     _dtype : 8;
  BOOL      _is_primary;

  IV_CAND(const IV_CAND&);
  IV_CAND& operator = (const IV_CAND&);
  void      Init(PHI_NODE *phi, CODEREP *init, CODEREP *incr, CODEREP *step, MTYPE   dtype) {
    _phi = phi;
    _init_var = init;
    _incr_var = incr;
    _init_value = init;
    _step_value = step;
    _dtype = dtype;
    _is_primary = FALSE;
    _var = phi->RESULT(); }

  DECLARE_SLIST_NODE_CLASS(IV_CAND)

public:
  IV_CAND(void);
  IV_CAND(PHI_NODE *phi,
	  CODEREP  *init,
	  CODEREP  *incr,
	  CODEREP  *step,
	  MTYPE    dtype)              { Init(phi, init, incr, step, dtype); }
  ~IV_CAND(void)                       { }

  void      Print (FILE *fp=stderr)
const;             // print the list

  PHI_NODE *Phi(void) const            { return _phi; }
  CODEREP  *Var(void) const            { return _var; }
  CODEREP  *Init_var(void) const       { return _init_var; }
  CODEREP  *Incr_var(void) const       { return _incr_var; }
  CODEREP  *Init_value(void) const     { return _init_value; }
  CODEREP  *Step_value(void) const     { return _step_value; }
  MTYPE     Dtype(void) const          { return _dtype; }
  BOOL      Is_primary(void) const     { return _is_primary; }
  void      Set_is_primary(void)       { _is_primary = TRUE; }
  void      Set_init_value(CODEREP *new_init){ _init_value = new_init; }
  inline BOOL *Satisfy_primary_IV(void);
};


class IVR {
private:
  IV_CAND   *_iv_cands;
  COMP_UNIT *_cu;
  CODEMAP   *_htable;
  CFG       *_cfg;
  OPT_STAB  *_opt_stab;
  MEM_POOL  _mem_pool;
  BOOL      _rebuild_loops;
  BOOL      _trace;
  INT32     _loop_counter;       // # of loop processed
  INT32     _slt_counter;        // # of slt optimization
  INT32     _ivr_counter;        // # of replacing secondary by primary
  INT32     _exit_value_counter; // # of exit value computed
  INT32     _trip_counter;       // # of trip count generated


  // data structure used per loop
  BB_LOOP   *_loop;
  CODEREP   *_trip_count;
  CODEREP   *_guarded_trip_count;
  CODEREP   *_entry_test;  // a predicate that is TRUE inside the loop
  CODEREP   *_trip_count_primary_IV;
  vector<IV_CAND*> iv_cand_container;
  BOOL       ivr_generated_primary;

  BB_LOOP   *Loop() const       { return _loop; }
  CODEREP   *Trip_count() const { return _trip_count; }
  CODEREP   *Guarded_trip_count() const { return _guarded_trip_count; }
  CODEREP   *Trip_count_primary_IV() const { return _trip_count_primary_IV; }
  CODEREP   *Entry_test() const { return _entry_test; }

  void       Set_trip_count(CODEREP *tc) { _trip_count = tc; }
  void       Set_guarded_trip_count(CODEREP *tc) { _guarded_trip_count = tc; }
  void       Set_trip_count_primary_IV(CODEREP *tc) { _trip_count_primary_IV = tc; }
  void       Set_entry_test(CODEREP *et) { _entry_test = et; }

  void       Init_loop(BB_LOOP *loop) {
    _loop = loop;
    Set_trip_count(NULL);
    Set_guarded_trip_count(NULL);
    Set_trip_count_primary_IV(NULL);
    Set_entry_test(NULL);
    ivr_generated_primary = FALSE;
    iv_cand_container.erase(iv_cand_container.begin(), iv_cand_container.end());
  }

  IVR(const IVR&);
  IVR& operator = (const IVR&);

  // Generate a step by computing the difference of new_iv and old_iv.
  CODEREP *Generate_step(CODEREP *new_iv, CODEREP *old_iv) const;
  
  // Check if IV is suitable to be selected as the primary IV
  BOOL Satisfy_primary_IV(const IV_CAND *, BOOL) const;

  // Choose the best IV cand to be the primary
  IV_CAND *Choose_primary_IV(const BB_LOOP *loop);

  // Replace occurence of based_iv with replacement.
  CODEREP *Replace_IV_with_invar(CODEREP *iv_expr, CODEREP *based_iv, CODEREP *replacement);

  // Compute the trip count expression
  CODEREP *Compute_trip_count
  (const OPCODE opc, BB_LOOP *loop,
   CODEREP *bound, CODEREP *var, CODEREP *init,
   CODEREP *step, CODEREP *iv_expr, CODEREP *based_iv, INT64 new_step,
   BOOL testatentry, STMTREP *cmp_stmt, BOOL swapped, IV_CAND *primary_cand);
      
  // Choose the primary iv from
  IV_CAND *Choose_primary_IV(const BB_LOOP*, IV_CAND *); 

  // Determine the trip count for the loop
  void Determine_trip_IV_and_exit_count(BB_LOOP *loop, IV_CAND **trip_iv_found, IV_CAND *primary);

  // Replace an secondary IV by a function of the primary IV
  void Replace_secondary_IV(const IV_CAND *primary, const IV_CAND *secondary, BB_NODE *startbb, BB_LOOP *loop);
  
  // Update the value of the secondary IV at the merge block if the trip-count is determined.
  void Update_exit_stmt(const IV_CAND *secondary, BB_NODE *bb_merge, BB_LOOP *loop);

  // to be moved to opt_loop.cxx
  BB_LOOP *Ident_loop  (BB_NODE *first,// Identify the do loop for
			BB_NODE *last, // the region between first
			INT32 loopnest,// and last BB.  It returns the
			BB_LOOP *loop);// first loop in the region

  void Print_all_iv_cand(FILE *);

  // Incr optimization counters
  void Inc_loop_counter(void)       { _loop_counter++; }
  void Inc_slt_counter(void)        { _slt_counter++; }
  void Inc_ivr_counter(void)        { _ivr_counter++; }
  void Inc_exit_value_counter(void) { _exit_value_counter++; }
  void Inc_trip_counter(void)       { _trip_counter++; }

  INT32 Loop_counter(void)          { return _loop_counter; }
  INT32 Slt_counter(void)           { return _slt_counter; }
  INT32 Ivr_counter(void)           { return _ivr_counter; }
  INT32 Exit_value_counter(void)    { return _exit_value_counter; }
  INT32 Trip_counter(void)          { return _trip_counter; }

  void Init_counters(void) {
    _loop_counter = 0;
    _slt_counter  = 0;
    _ivr_counter  = 0;
    _exit_value_counter = 0;
    _trip_counter = 0;
  }

  BB_NODE *Get_my_regionstart(BB_NODE *bb) const;

  STMTREP* Find_parallel_pragma_stmt(BB_NODE *bb);

  STMTREP* Find_associated_parallel_pragma(BB_LOOP *loop,BB_NODE **contain_bb);

  BOOL     Is_mp_with_same_mp_pragma(BB_LOOP *child, BB_LOOP *parent);

  STMTREP *Preprocess_mp_pragma_list(BB_LOOP *loop, BB_NODE **bb);

  void     Update_mp_pragma_list(BB_LOOP *loop, BB_NODE *bb, STMTREP *stmt);

  void     Reset_dont_prop(CODEREP *cr, const BB_LOOP *loop);

  void     Set_rebuild_loops(void) { _rebuild_loops = TRUE; }

  void     Canon_loop_end_br (BB_LOOP*, IV_CAND*);

public:
  IVR(COMP_UNIT *cu, BOOL trace);
  ~IVR(void);

  CODEMAP  *Htable(void) const         { return _htable; }
  OPT_STAB *Opt_stab(void) const       { return _opt_stab; }
  CFG      *Cfg(void) const            { return _cfg; }
  MEM_POOL *Mem_pool(void)             { return &_mem_pool; }
  BOOL     Trace(void) const           { return _trace; }
  OPT_PHASE Phase(void) const          { return _cu->Phase(); }
  BOOL     Rebuild_loops(void) const { return _rebuild_loops; }

  // Convert all secondary IV into primary for this loop
  void     Convert_all_ivs(BB_LOOP *);
  
  // Perform IVR for one loop
  BOOL     Process_one_loop(BB_LOOP *);

  // Identify all IV candidates
  void Ident_all_iv_cands(const BB_LOOP *loop, const BB_NODE *bb);
  vector<IV_CAND*> &Get_iv_candidates(void) { return iv_cand_container; }
};

#endif  // Opt_ivr_INCLUDED
