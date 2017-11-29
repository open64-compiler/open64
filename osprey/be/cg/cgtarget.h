/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 *  Target Specific Miscellany
 *
 *  Here are various miscellaneous functions to provide machine dependent
 *  information.
 *
 *  Exported functions:
 *
 *    Misc:
 *
 *	void CGTARG_Initialize(void)
 *	  Perform one-time initialization.
 *
 *    Target capabilities and peculiarities:
 *
 *	const INT INST_BYTES
 *	  The number of bytes per instruction word.
 *
 *      const INT DEFAULT_LONG_BRANCH_LIMIT
 *	  The default limit for branch displacements (in bytes) after
 *	  which long-branch fixups occur.
 *
 *      const INT DEFAULT_BRP_BRANCH_LIMIT
 *	  The default limit for branch-predict displacements (in bytes).
 *        This currently applies only for IA-64, all other targets have 
 *        this set to INT32_MAX. This is the legal displacement limit
 *        of the branch-predict instructions, beyond which they will be
 *        converted to NOPs by the cgemit phase. 
 *
 *      const INT MIN_BRANCH_DISP
 *	  The displacement (in bytes) for the shortest PC-relative branch.
 *
 *	BOOL CGTARG_Have_Indexed_Mem_Insts(void)
 *	  Returns a boolean that indicates if the target supports
 *	  indexed memory reference instructions.
 *
 *	TOP CGTARG_Inter_RegClass_Copy(ISA_REGISTER_CLASS dst,
 *				       ISA_REGISTER_CLASS src,
 *				       BOOL is_double)
 *	  Returns the copy instruction for moving data from register
 *	  class <src> to <dst> in single or double precision according
 *	  to <is_double>. Returns TOP_UNDEFINED if there is no
 *	  such instruction.
 *
 *	INT CGTARG_Text_Alignment(void)
 *	  Return the best instruction alignment for the processor.
 *
 *      INT32 CGTARG_Branch_Taken_Penalty(void)
 *        Return the number of extra cycles (often 0) consumed by a
 *        branch that is taken (even if correctly predicted).
 *
 *	BOOL CGTARG_Use_Brlikely(float branch_taken_probability)
 *	  Given a conditional branch with a <branch_taken_probability>
 *	  return TRUE if it would be beneficial to convert it to a brlikely.
 *	  This handles targets that have a penalty for brlikely 
 *        instructions that are not taken (e.g. T5).
 *
 *	BOOL CGTARG_Can_Change_To_Brlikely(OP *xfer_op, TOP *new_opcode)
 *        Checks to see if <xfer_op> can be converted to its branch-likely 
 *	  form and returns the new_opcode if result TRUE.
 *
 *	BOOL CGTARG_Can_Predicate_Calls()
 *	  Returns true if the target supports a predicated call instruction.
 *
 *	BOOL CGTARG_Can_Predicate_Returns()
 *	  Returns true if the target supports a predicated call instruction.
 *
 *	BOOL CGTARG_Can_Predicate_Branches()
 *	  Returns true if the target supports predicated branch instructions.
 *
 *	BOOL CGTARG_Unconditional_Compare(OP* op, TOP* uncond_ver)
 *	  Returns true if a compare sets predicates unconditionally.  It
 *	  returns the unconditional version of the compare if one exists.
 *
 *      TOP CGTARG_Noop_Top(void)
 *        TOP (opcode) to use for noops on current target.
 *        
 *      TOP CGTARG_Simulated_Top(TOP, ISA_EXEC_UNIT_PROPERTY)
 *        Returns the right opcode for simulated TOP which matches 
 *	  ISA_EXEC_UNIT_PROPERTY.
 *        
 *      TOP CGTARG_Noop(ISA_EXEC_UNIT_PROPERTY)
 *        TOP (opcode) to use for noops on current target, with given unit.
 *        
 *      BOOL CGTARG_Can_daddu_Be_Folded(OP *op1, OP *op2)
 *        Returns TRUE if <op1> and <op2> is a daddu/addiu sequence
 *	  that can be folded, i.e.  <op1> is a addiu op which is feeding 
 *	  into daddu op <op2>.
 *
 *      TOP CGTARG_Copy_Op(UINT8 size, BOOL is_float)
 *        TOP (opcode) to use for copying a register <size> bytes in
 *	  length (FP if <is_float>).
 *
 *	TOP CGTARG_Parallel_Compare(TOP cmp_top, COMPARE_TYPE ctype)
 *	  Return a parallel version of cmp_top if any such exists.
 *        
 *	BOOL CGTARG_Dependence_Required(OP *pred_op, OP *succ_op)
 *	  Check for any target-specific dependences (other than the usual
 *        repertoire) that may be required between <pred_op> and <succ_op>. 
 *        
 *	void CGTARG_Adjust_Latency(OP *pred_op, OP *succ_op, 
 *                                 CG_DEP_KIND kind, UINT8 opnd, INT *latency)
 *	  Makes any target-specific latency adjustments that may be
 *	  required between <pred_op> and <succ_op>.
 *        
 *	void CGTARG_Peak_Rate( PEAK_RATE_CLASS prc, INT ratio[2] )
 *	  What is the peak rate (in ops per cycle) for the given
 *	  class of instruction(s). A rate of 0 means the class of
 *	  instruction isn't supported on the target processor.
 *	  The rate is expressed as a fraction: insts/cycle and stored
 *	  in the <ratio> array (ratio[0]==insts, ratio[1]==cycles).
 *
 *	void CGTARG_Compute_PRC_INFO (BB *bb, PRC_INFO *info)
 *	  For the given basic block <bb> compute some basic information
 *        about the distribution of the different instruction types.
 *	  Return this information in <info>.
 *
 *	void CGTARG_Print_PRC_INFO (FILE *file, PRC_INFO *info, INT ii,
 *				    const char *prefix, const char *suffix)
 *	  Print the PRC_INFO <info> to <file>. The iteration interval to
 *	  be used for printing the statistics is <ii>. Use the string
 *	  <prefix> before printing each line of output. Use the string
 *	  <suffix> after printing each line of output.
 *
 *	INT CGTARG_ARC_Sched_Latency( ARC *arc )
 *	  Wrapper function for ARC_latency to let us fix up some cases 
 *	  where it returns a result that just doesn't make sence.  In
 *	  particular a latency of -1 for the pre-branch latency makes no
 *	  scheduling sense for CPUs which have same-cycle branch shadows.
 *	  Should be 0.
 *
 *      void CGTARG_Handle_Errata_Hazard (OP *op, INT erratnum, 
 *                                        INT ops_to_check)
 *        Handle all the Errata hazards. These are typically workarounds
 *        for bugs in particular versions of various target processors.
 *
 *      void CGTARG_Handle_Bundle_Hazard(OP *op, TI_BUNDLE *bundle, 
 *					 VECTOR *bundle_vector, 
 *					 BOOL can_fill, INT slot_pos, 
 *					 INT max_pos,
 *					 ISA_EXEC_UNIT_PROPERTY prop)
 *	  Handle all bundle hazards in this routine. It inserts any extra
 *        nops required to form  a legal bundle. (1) <op> is the current
 *        OP being processed. (2) <bundle> is the current bundle. (3)
 *        <bundle_vector> is an internal vect to determine relations with
 * 	  any prior filled OPs in the <bundle>. (4) <can_fill> tells that
 *        a <slot_pos> has already been chosen for the <op> in <bundle>.
 *        (5) <max_pos> identifies OPs which occupy more than one slot position.
 *        (6) <prop> is the ISA_EXEC_UNIT_PROPERTY type that is being used.
 *
 *      BOOL CGTARG_Bundle_Slot_Available(TI_BUNDLE *bundle, OP *op, INT slot,
 *					  ISA_EXEC_UNIT_PROPERTY *prop,
 *					  BOOL stop_bit_reqd,
 *                                        const CG_GROUPING *grouping)
 *	  Checks to see if <slot> position is available for <op> using the
 *	  property type <prop> in the <bundle>. The <stop_bit_reqd> flag
 *        checks if stop bit is also required due to dependencies.  The
 *        grouping parameter, if not NULL, is used to choose between property
 *        types where there is a choice.
 *
 * 	BOOL CGTARG_Bundle_Stop_Bit_Available(TI_BUNDLE *bundle, INT slot);
 *        Checks to see if <stop> bit is available at <slot> position in the
 *	  bundle.
 *
 *	BOOL CGTARG_Preg_Register_And_Class(WN_OFFSET preg,
 *					    ISA_REGISTER_CLASS *rclass,
 *					    REGISTER *reg)
 *	  Return the register and class (by reference through 'rclass'
 *	  and 'reg') corresponding to 'preg'. The return value indicates
 *	  if there was a preg -> register/class mapping.
 *
 *
 *	void CGTARG_Compute_Branch_Parameters(INT32 *mispredict,
 *					      INT32 *fixed,
 *					      INT32 *brtaken,
 *					      double factor)
 *	  Various machine dependent values use for non loop if conversion.
 *	  mispredict:
 *		Branch misprediction penalty. Given the condition ready, how 
 *		many cycles are wasted in resolving the new branch and 
 *		starting the pipline?
 *	  fixed:
 *		Number of cycles in executing the branch and it's trailer.
 *	  brtaken:
 *		For some machines, the taken pc requires ALU resources.
 *	  factor
 *		The misprediction rate may not correlate to the branch 
 *		probability.
 *		A factor (0..1.0) will scale the misprediction penalty.
 *
 *	INT32 CGTARG_Special_Min_II(BB* loop_body, BOOL trace)
 *	  Check for target specific (tail stepping, and other?) special
 *	  cases that might force a higher Min II. If a case applies, the
 *	  target specific MII is returned, otherwise 0 is returned.
 *
 *    OP/TOP specific:
 *
 *      BOOL CGTARG_Is_OP_Barrier(OP* op)
 *        Returns TRUE if <op> is a barrier node.
 *        
 *      BOOL CGTARG_Is_OP_Intrinsic(OP *op)
 *        Returns TRUE if <op> is an intrinsic node.
 *
 *      BOOL CGTARG_Is_OP_Speculative(OP *op)
 *        Returns TRUE if <op> is a safe speculative OP.
 *
 *      BOOL CGTARG_Is_OP_Addr_Incr(OP* op)
 *        Returns TRUE if <op> is an address increment op
 *	  (eg. {addiu, daddiu} rd, rd, offset) with the destination and
 *         source1 being the same registers.
 *
 *      void CGTARG_Perform_THR_Code_Generation(OP *op, THR_TYPE type)
 *        Perform THR (and target-specific) code generation tasks per
 *        THR_TYPE and OP. Depending on <type>, <op> can now be 
 *        substituted (or replaced with) new ops.
 *
 *	INT CGTARG_Copy_Operand(OP *op)
 *	  If 'op' performs a copy operation, return the index of
 *	  the source operand; otherwise return -1.
 *
 *	TN *CGTARG_Copy_Operand_TN(OP *op)
 *	  If 'op' performs a copy operation, return the TN of
 *	  the source operand; otherwise return NULL.
 *
 *	BOOL CGTARG_Is_Copy(OP *op)
 *	  Return a boolean indicating if 'op' performs a copy operation.
 *
 *	BOOL CGTARG_Is_Preference_Copy(OP *op)
 *	  Return a boolean indicating if 'op' performs a copy operation
 *	  that is a candidate for preferencing.  Does extra consistency
 *	  checks.  This is the preferred method for testing for a copy
 *	  before invoking CGTARG_Copy_Operand() and friends.
 *
 *	BOOL CGTARG_Is_OP_daddu(OP *op)
 *	  Return a boolean indicating if 'op' is a mips daddu inst.
 *
 *	void CGTARG_Load_From_Memory(TN *tn, ST *mem_loc, OPS *ops)
 *	void CGTARG_Store_To_Memory(TN *tn, ST *mem_loc, OPS *ops)
 *	  Load/store any TN from/to memory, even if it takes a sequence
 *	  of instructions to do so. Note that the expander can only
 *	  store TNs in register classes that have load and store
 *	  instructions.
 *
 *	TOP CGTARG_Immed_To_Reg(TOP opr)
 *	  <opr> is an immediate format instruction, return its equivalent
 *	  register form if there is one, return TOP_UNDEFINED if
 *	  there isn't.
 *
 *	void CGTARG_Predicate_OP(BB* bb, OP* op, TN* pred_tn)
 *	    Replace <op> with its predicated form, inserting new ops as needed.
 *
 *	BOOL CGTARG_Branches_On_True(OP* br_op, OP* cmp_op)
 *	    Returns TRUE if branch executed on TRUE test of cmp_op.  Note
 *	    that it doesn't really verify that br_op is conditional, so it
 *	    may return odd results if given an unconditional branch.
 *
 *      BOOL CGTARG_Is_Long_Latency(TOP op)
 *	  Return a boolean indicating if <op> is a long latency operation.
 *
 *      BOOL CGTARG_Is_Bad_Shift_Op (OP *op)
 *        Return TRUE if <op> is a shift instruction that has the
 *        ERRATA01 or the ERRATA02 hazard.
 *
 *	TOP CGTARG_Invert(TOP opr)
 *	  Return the inverse of <opr>. TOP_UNDEFINED is returned
 *	  if there is no inverse. Note that the inverse of an
 *	  instruction that takes an immediate does not also necessarily
 *	  take an immediate. 
 *
 *      void CGTARG_Branch_Info ( const OP* op, INT* tfirst, INT* tcount )
 *        Given a branch OP, which operand is the first branch target and how
 *        many branch targets are there?  Returns index of first branch target
 *        by reference in <tfirst> and count of branch targets in <tcount>.
 *
 *	UINT32 CGTARG_Mem_Ref_Bytes(const OP *memop)
 *	  Requires: OP_load(memop) || OP_store(memop)
 *	  Return the number of bytes in memory referenced by <memop>.
 *  
 *      BOOL CGTARG_Can_Be_Speculative( OP* op )
 *        Determine whether the given OP may be scheduled eagerly.  This
 *        involves a target-specific mapping of its exception classification
 *        to an exception level, and determination of whether that level of
 *        speculation is enabled.
 *  
 *        There is a major can of worms lurking under this seemingly innocent
 *        function.  Different targets have vastly different styles of
 *        speculation.  For example, TFP was designed with the expectation
 *        that software would be responsible for speculation and thus fully
 *        supports dismissing floating point exceptions in hardware.  By
 *        contrast, T5 does not support this.  Triton, which does nothing in
 *        hardware to speculate also cannot dismiss may floating point traps.
 *
 *      BOOL CGTARG_Is_OP_Speculative_Load( OP* memop )
 *      BOOL CGTARG_Is_OP_Advanced_Load( OP* memop )
 *      BOOL CGTARG_Is_OP_Check_Load( OP* memop )
 *        These routines check to see if <memop> is a speculative,
 *        advanced or check load. Assumes that <memop> is a load operation.
 *  
 *	INT CGTARG_Analyze_Compare(OP *br,
 *				       TN **tn1, 
 *				       TN **tn2, 
 *				       OP **compare_op)
 *	  Analyze a branch to determine the condition of the branch and
 *	  TNs being compared. Where possible and appropriate if the branch
 *	  is based on the result of an slt instruction, take that in to
 *	  consideration in determining the condition and arguments.
 * 
 *	  The variant of the branch (see variant.h) is returned through 
 *	  the function return value.
 *
 *	  <br> is a pointer to a branch instruction to be analyzed. The
 *	  arguments of the comparison are returned through the out parameters
 *	  <tn1> and <tn2>. The comparison OP is returned through out
 *	  parameter <compare_op>. 
 *
 *	  NOTES: Currently restricted to integer branches; also see
 *	  CGTARG_Analyze_Branch
 *
 *      VARIANT CGTARG_Analyze_Branch(OP *br, TN **tn1, TN **tn2)
 *	  Analyze a branch to determine the condition of the branch and
 *	  the operand TNs.
 * 
 *	  The variant of the branch (see variant.h) is returned through 
 *	  the function return value.
 *
 *	  <br> is a pointer to a branch instruction to be analyzed. The
 *	  arguments of the branch are returned through the out parameters
 *	  <tn1> and <tn2>. 
 *
 *	  NOTES: also see CGTARG_Analyze_Compare
 *
 *    Horrible implicit register definitions and uses:
 *
 *      BOOL CGTARG_OP_Defs_TN( OP* op, TN* tn )
 *      BOOL CGTARG_OP_Refs_TN( OP* op, TN* tn )
 *        There are (only very few) cases where a TN is implictly defined or
 *        referenced by certain OPs.  These functions allow us to find these
 *        cases. (Currently used only for the floating status register.)
 *
 *    Associated base operations:
 *
 *	void CGTARG_Init_Assoc_Base(void)
 *	  A number of tables are maintained for performing base
 *	  association related inqueries. The accessors to the
 *	  tables are declared below. See cg_loop_recur:Find_Assoc_Base_Opr
 *	  and cgprep:assoc_base_opr for additional details.
 *
 *	OPCODE CGTARG_Assoc_Base_Opr(TOP topcode)
 *	  Return the correpsonding associated base whirl opcode for <topcode>.
 *
 *	TOP CGTARG_Assoc_Base_Top(TOP topcode)
 *	  Return the correpsonding associated base topcode for <topcode>.
 *
 *	CGTARG_ASSOC_BASE_FNC CGTARG_Assoc_Base_Fnc(TOP topcode)
 *	  When the associated base operation is not a simple mapping
 *	  based soley on <topcode>, this function returns an
 *	  enumeration of the function that needs to be performed to
 *	  determine the corresponding base operation, if any.
 *	  The following are the possible enumerations:
 *
 *	  ASSOC_BASE_null
 *	    This is a simple mapping, no additional function need be performed.
 *
 *	  ASSOC_BASE_inverse
 *	    Indicates this is an inverse operation.
 *
 *	  ASSOC_BASE_minmax
 *	    <topcode> is a min/max operation comprised of multiple operations.
 *	    The additional operations need to be checked.
 *
 *	  ASSOC_BASE_imul
 *	    <topcode> is an integer multiply. It is necessary to determine
 *	    if the multiply is within a recurrence.
 *
 *  Target specific live range interference:
 *
 *    For TFP we implement two register allocation restrictions.  Violation of
 *    these rules results in stalls.  Essentially this is because the hardware
 *    does not have register renaming and a specific register can contain at
 *    most one particular value in any given cycle.
 *
 *      1. No two operations can target the same register with the
 *         same register write cycle, and
 *
 *      2. No operand may be read in the register write cycle of
 *         another value.  This particularly disallows flds to overwrite the
 *         operand of a previous flop in the same cycle.
 *    
 *    A third restriction exists but is not implemented:
 *
 *      3. V0 may not redifine the register of v1 while v1 is in
 *         flight.  A live range is in flight from its definition point
 *         through the cycle before it is available for bypassing.  This
 *         final rule is not currently implemented.  TODO: implement it if
 *         it becomes a problem.
 *
 *    Theese restrictions are convied through the following functions.  They
 *    are intended to be called for the operation in a set of contigious
 *    blocks before register allocation is performend but after secheduling.
 *    The client supplies live ranges and information about which OP define
 *    and use them when in the schedules.  A client supplied function is
 *    called when two live ranges may not have the same register (interfere).
 *    
 *    It is important to call these functions in a particular order:
 *
 *      1. CGTARG_Interface_Initialize
 *      2. All calls to CGTARG_Result_Live_Range
 *      3. All calls to CGTARG_Operand_Live_Range
 *      4. CGTARG_Interface_Finalize
 *
 *    Exported Function for target specific live range interference:
 *
 *      BOOL CGTARG_Interference_Required(void)
 *        Does the current target require target specific live range
 *        interference at all?
 *        
 *      void CGTARG_Interference_Initialize( INT32 cycle_count,
 *                                           BOOL is_loop,
 *                                           void (*inter_fn)(void*,void*) )
 *        Prepare to make the target specific interferences.  <cycle_count> is
 *        the number of total cycles in the contigious set of blocks under
 *        consideration.  <is_loop> is true if the blocks constitute a loop
 *        body (and thus we need to worry about OPs in the final few cycles
 *        writing in the first few cycles of the loop.)  <inter_fn> ia a
 *        function pointer to be called when for pairs of live ranges that
 *        must not be allocated to the same register.  It is assumed that
 *        interference is a reflexive relation and thus that it is not
 *        necessary to call the function for both x,y and y,x.  (So the client
 *        must make sure that both interference graph edges are drawn.)
 *        
 *      void CGTARG_Result_Live_Range( void* lrange, OP* op, INT32 offset )
 *        Present the <lrange> written by <op>.  <offset> gives a cycle count
 *        to be added to OP_scycle as the start time of <op> relative to the
 *        first cycle of the blocks under consideration.
 *        
 *      void CGTARG_Operand_Live_Range( void * lrange, INT opnd, OP* op,
 *                                      INT32  offset )
 *        Present the <lrange> used by <op> and it's <opnd> operand.  <offset>
 *        gives a cycle count to be added to OP_scycle as the start time of
 *        <op> relative tot he first cycle of the blocks under consideration.
 *        
 *      void CGTARG_Interference_Finalize(void)
 *        All done presenting this set of OPs.
 *
 *      TOP CGTARG_Which_OP_Select (UINT16 bit_size, BOOL is_float,
 *                                  BOOL is_fcc)
 *        Determine which opcode to use to copy TNs based upon whether or
 *        not the value being copied is floating point, its size in bits,
 *        and whether the condition to be used is a floating CC or integer
 *        register.
 *
 *      TOP CGTARG_Equiv_Nonindex_Memory_Op(OP *op)
 *        Return the nonindexed OP code corresponding to a indexed memory
 *        OP or TOP_UNDEFINED if code is not an indexed memory OP.
 *
 *      void CGTARG_Generate_Remainder_Branch(TN *trip_count, TN *label_tn
 *                                            OPS *prolog_ops, OPS *body_ops)
 *        Generate the branch instruction and its initialization into the
 *        body_ops and prolog_ops respectively for the unrolling remainder loop.
 *
 *      void CGTARG_Generate_Branch_Cloop(OP *op, TN *unrolled_trip_count, TN *trip_count, INT32 ntimes,
 *                                        TN *label_tn, OPS *prolog_ops. OPS *body_ops);
 *        If the target architecture supports a counted loop instruction,
 *        generate the initialization and the cloop instruction and append
 *        them to the prolog_ops and body_ops respectively.
 *
 *      BOOL CGTARG_OP_is_counted_loop(OP *op)
 *        Returns TRUE if op is the counted loop branch.
 *        
 *	TOP CGTARG_Invert_Branch(BB* bb)
 *	  Invert sense of the branch terminating bb.  Returns new branch
 *	  op if successful.
 *
 *      void CGTARG_Init_OP_cond_def_kind(OP *op)
 *        Initialize target dependent OP_cond_def_kind.
 *
 *      void CGTARG_Use_Load_Latency(OP *, TN *)
 *        The TN is the load-result of the OP, so use of TN has the
 *        latency of a load
 *
 * ====================================================================
 * ====================================================================
 */

/* 
 * $Revision: 1.15 $
 * $Date: 05/12/05 08:59:06-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cgtarget.h $
 */

#ifndef CGTARGET_INCLUDED
#define CGTARGET_INCLUDED

#include "defs.h"
#include "cgir.h"
#include "variants.h"
#include "cg_flags.h"
#include "cg_dep_graph.h"
#include "cg_vector.h"
#include "config_targ.h"
#include "targ_isa_bundle.h"
#include "ti_bundle.h"
#include "cg_thr.h"

#ifdef TARG_IA64
#include "targ_bypass_mck.h"
#endif

extern UINT32 CGTARG_branch_taken_penalty;
extern BOOL CGTARG_branch_taken_penalty_overridden;

#include "cgtarget_arch.h"

class CG_GROUPING; // Defined only for isa where it is used (e.g. IA-64).


inline TOP
CGTARG_Invert(TOP opr)
{
  extern TOP CGTARG_Invert_Table[TOP_count+1];

  return CGTARG_Invert_Table[(INT)opr];
}

inline TOP
CGTARG_Immed_To_Reg(TOP opr)
{
  extern TOP CGTARG_Immed_To_Reg_Table[TOP_count+1];

  return CGTARG_Immed_To_Reg_Table[(INT)opr];
}

extern void CGTARG_Perform_THR_Code_Generation(OP *load_op, OP *check_load,
					       THR_TYPE type);
extern INT  CGTARG_ARC_Sched_Latency( ARC *arc );
extern void CGTARG_Handle_Errata_Hazard (OP *op, INT erratnum, 
					 INT ops_to_check);
extern void CGTARG_Handle_Bundle_Hazard(OP                     *op, 
					TI_BUNDLE              *bundle, 
					VECTOR                 *bundle_vector, 
					BOOL                   can_fill, 
					INT                    slot_pos, 
					INT                    max_pos,
					BOOL                   stop_bit_reqd,
					ISA_EXEC_UNIT_PROPERTY prop);

extern BOOL CGTARG_Bundle_Slot_Available(TI_BUNDLE              *bundle, 
					 OP                     *op, 
					 INT                    slot,
					 ISA_EXEC_UNIT_PROPERTY *prop,
					 BOOL                   stop_bit_reqd,
                                         const CG_GROUPING      *grouping);

extern BOOL CGTARG_Bundle_Stop_Bit_Available(TI_BUNDLE *bundle, INT slot);

extern void CGTARG_Branch_Info ( const OP* op, INT* tfirst, INT* tcount );
extern BOOL CGTARG_Can_Be_Speculative( OP* op );
extern BOOL CGTARG_Is_OP_Speculative(OP *op);
extern BOOL CGTARG_Is_OP_Speculative_Load( OP* memop );
extern BOOL CGTARG_Is_OP_Advanced_Load( OP* memop );
#ifdef TARG_IA64
extern BOOL CGTARG_Is_Form_For_Advanced_Load (ISA_ENUM_CLASS_VALUE ldform); 
#endif
extern BOOL CGTARG_Is_OP_Check_Load( OP* memop );

extern BOOL CGTARG_OP_Defs_TN( OP* op, TN* tn );
extern BOOL CGTARG_OP_Refs_TN( OP* op, TN* tn );
extern BOOL CGTARG_Interference_Required(void);
extern TOP  CGTARG_Which_OP_Select (UINT16 bit_size, BOOL is_float,
				    BOOL is_fcc);
extern TOP CGTARG_Equiv_Nonindex_Memory_Op ( OP *op );
extern void CGTARG_Interference_Initialize( INT32 cycle_count,
                                            BOOL is_loop,
                                            void (*inter_fn)(void*,void*) );
extern void CGTARG_Result_Live_Range( void* lrange, OP* op, INT32 offset );
extern void CGTARG_Operand_Live_Range( void * lrange, INT opnd, OP* op,
                                       INT32  offset );
extern void CGTARG_Interference_Finalize(void);
extern TOP CGTARG_Invert_Branch(BB* bb);

typedef enum {
  PRC_INST,
  PRC_MEMREF,
  PRC_FLOP,
  PRC_FLOP_S,
  PRC_MADD,
  PRC_MADD_S,
  PRC_FADD,
  PRC_FADD_S,
  PRC_FMUL,
  PRC_FMUL_S,
  PRC_IOP,
  PRC_LAST
} PEAK_RATE_CLASS;

typedef struct {
#ifdef KEY
  mUINT16 refs[PRC_LAST];
#else
  mINT16 refs[PRC_LAST];
#endif
} PRC_INFO;

extern void CGTARG_Peak_Rate( PEAK_RATE_CLASS prc, PRC_INFO *info, INT ratio[2] );
extern void CGTARG_Print_PRC_INFO (FILE *file, PRC_INFO *info, INT ii, 
				   const char *prefix, const char *suffix);
extern void CGTARG_Compute_PRC_INFO (BB *bb, PRC_INFO *info);
extern UINT32 CGTARG_Mem_Ref_Bytes(const OP *memop);
extern BOOL CGTARG_Preg_Register_And_Class(WN_OFFSET preg,
					   ISA_REGISTER_CLASS *rclass,
					   REGISTER *reg);

extern void CGTARG_Compute_Branch_Parameters(INT32 *mispredict,
						    INT32 *fixed,
						    INT32 *brtaken,
						    double *factor);

extern BOOL CGTARG_Can_Change_To_Brlikely(OP *xfer_op, TOP *new_opcode);
extern TOP CGTARG_Negate_Branch(TOP op);
extern BOOL CGTARG_Is_Long_Latency(TOP op);
extern INT32 CGTARG_Latency(TOP op);

extern VARIANT CGTARG_Analyze_Branch(OP *br, TN **tn1, TN **tn2);
extern VARIANT CGTARG_Analyze_Compare(OP *br, TN **tn1, TN **tn2,
	 			      OP **compare_op);

extern INT32 CGTARG_Special_Min_II(BB* loop_body, BOOL trace);

/* placeholder for all hardware workarounds */
extern void Hardware_Workarounds (void);
extern void Insert_Stop_Bits(BB *bb);

extern void CGTARG_Initialize(void);

extern void CGTARG_Load_From_Memory(TN *tn, ST *mem_loc, OPS *ops);
extern void CGTARG_Store_To_Memory(TN *tn, ST *mem_loc, OPS *ops);

typedef enum {
  ASSOC_BASE_null,
  ASSOC_BASE_inverse,
  ASSOC_BASE_minmax,
  ASSOC_BASE_imul,
  ASSOC_BASE_COUNT
} CGTARG_ASSOC_BASE_FNC;

extern void CGTARG_Init_Assoc_Base(void);

inline OPCODE CGTARG_Assoc_Base_Opr(TOP topcode)
{
  extern OPCODE CGTARG_Assoc_Base_Opr_Table[TOP_count];

  return CGTARG_Assoc_Base_Opr_Table[(INT)topcode];
}

inline TOP CGTARG_Assoc_Base_Top(TOP topcode)
{
  extern mTOP CGTARG_Assoc_Base_Top_Table[TOP_count];

  return (TOP)CGTARG_Assoc_Base_Top_Table[(INT)topcode];
}

inline CGTARG_ASSOC_BASE_FNC CGTARG_Assoc_Base_Fnc(TOP topcode)
{
  extern mTOP CGTARG_Assoc_Base_Fnc_Table[TOP_count];

  return (CGTARG_ASSOC_BASE_FNC)CGTARG_Assoc_Base_Fnc_Table[(INT)topcode];
}

extern INT CGTARG_Copy_Operand(OP *op);

inline BOOL CGTARG_Is_Copy(OP *op)
{
  return CGTARG_Copy_Operand(op) >= 0;
}

inline BOOL CGTARG_Is_Preference_Copy(OP* op) 
{
  if (OP_copy(op)) {
    if (CGTARG_Is_Copy(op)) {
      return TRUE;
    } else {
      //
      // Ops that are to be deleted sometimes have their opcode temporarily
      // set to nop prior to deletion (principally in lra).
      //
      Is_True(OP_code(op) == CGTARG_Noop_Top(),
	      ("Op_copy set on non-copy op"));
      return FALSE;
    }
  }
  return FALSE;
}

inline TN *CGTARG_Copy_Operand_TN(OP *op)
{
  INT iopnd = CGTARG_Copy_Operand(op);
  return (iopnd < 0) ? NULL : OP_opnd(op,iopnd);
}

inline TOP CGTARG_Inter_RegClass_Copy(ISA_REGISTER_CLASS dst,
				      ISA_REGISTER_CLASS src,
				      BOOL is_double)
{
  extern mTOP CGTARG_Inter_RegClass_Copy_Table
    [ISA_REGISTER_CLASS_MAX+1][ISA_REGISTER_CLASS_MAX+1][2];

  return (TOP)CGTARG_Inter_RegClass_Copy_Table[src][dst][is_double];
}


extern BOOL CGTARG_Can_Fit_Immediate_In_Add_Instruction (INT64 immed);
extern BOOL CGTARG_Can_Load_Immediate_In_Single_Instruction (INT64 immed);
#ifdef TARG_MIPS
extern BOOL CGTARG_Can_Fit_Displacement_In_Branch_Instruction (INT64 disp);
#endif

extern void CGTARG_Save_Pfs (TN *saved_pfs, OPS *ops);
extern void CGTARG_Restore_Pfs (TN *saved_pfs, OPS *ops);

/* target-specific adjustments to entry ops */
extern void CGTARG_Fixup_Entry_Code (BB *bb);

typedef enum {
  COMPARE_TYPE_unc,
  COMPARE_TYPE_or,
  COMPARE_TYPE_orcm,
  COMPARE_TYPE_and,
  COMPARE_TYPE_andcm,
  COMPARE_TYPE_or_andcm,
  COMPARE_TYPE_and_orcm,
  COMPARE_TYPE_normal
} COMPARE_TYPE;

extern TOP CGTARG_Parallel_Compare(OP* cmp_op, COMPARE_TYPE ctype);

extern BOOL CGTARG_Dependence_Required(OP *pred_op, OP *succ_op);

extern void CGTARG_Adjust_Latency(OP *pred_op, OP *succ_op, CG_DEP_KIND kind, UINT8 opnd, INT *latency);

extern BOOL CGTARG_Unconditional_Compare(OP* op, TOP* uncond_ver);

extern BOOL CGTARG_Branches_On_True(OP* br_op, OP* cmp_op);

extern void CGTARG_Predicate_OP(BB* bb, OP* op, TN* pred_tn);

extern void CGTARG_Generate_Remainder_Branch(TN *trip_count, TN *label_tn,
					     OPS *prolog_ops, OPS *body_ops);

extern BOOL CGTARG_OP_is_counted_loop(OP *op);

extern void CGTARG_Generate_Branch_Cloop(OP *op, TN *unrolled_trip_count, TN *trip_count,
					 INT32 ntimes, TN *label_tn, OPS *prolog_ops, OPS *body_ops);
#ifdef TARG_X8664
extern void CGTARG_Generate_Countdown_Loop(TN *trip_count_tn, BB *tail, 
					   OPS *prolog_ops, OPS *body_ops, 
					   BOOL single_bb, LOOP_DESCR *loop);
extern void CGTARG_LOOP_Optimize( LOOP_DESCR* loop );
extern TN* CGTARG_Process_Asm_m_constraint( WN*, void**, OPS* );
#endif

/* call init routine once per asm stmt */
extern void CGTARG_Init_Asm_Constraints (void);

/* Given a constraint for an ASM parameter, and the load of the matching
 * argument passed to ASM (possibly NULL), choose an appropriate TN for it
 */
#if defined(TARG_IA64) || defined(TARG_PPC32) || defined(TARG_LOONGSON)
extern TN* CGTARG_TN_For_Asm_Operand(const char* constraint, 
                                     const WN* load,
                                     TN* pref_tn,
                                     ISA_REGISTER_SUBCLASS* subclass,
                                     const WN* asm_wn);
#else
extern TN* CGTARG_TN_For_Asm_Operand(const char* constraint, 
                                     const WN* load,
                                     TN* pref_tn,
                                     ISA_REGISTER_SUBCLASS* subclass,
                                     const WN* asm_wn, 
				     TYPE_ID type);
#endif

/* given asm constraint and mtype, 
 * pick appropriate dedicated tn and string name */
extern void CGTARG_TN_And_Name_For_Asm_Constraint (char *constraint, 
                                                   TYPE_ID rtype, 
                                                   TYPE_ID desc,
                                                   TN **tn, 
                                                   const char **name);

/* may have to clean up the asm string */
extern void CGTARG_Postprocess_Asm_String (char* asm_string);

/* target specific modifiers for printing different versions
 * of register names when they appear as AM operands 
 */
extern char CGTARG_Asm_Opnd_Modifiers[];
extern INT  CGTARG_Num_Asm_Opnd_Modifiers;

extern const char* CGTARG_Modified_Asm_Opnd_Name(char modifier, TN* tn, char *tn_name);


extern void CGTARG_Init_OP_cond_def_kind(OP *);


//  Returns TRUE if the dependence between the latency
//  between pred_op and succ_op is the load_latency
//
inline BOOL CGTARG_Use_Load_Latency(OP *pred_op, TN *tn) 
{
#ifdef TARG_IA64
  return OP_load(pred_op) && OP_result(pred_op,0) == tn;
#else
  return TRUE;
#endif
}

#if defined(TARG_IA64) || defined(TARG_X8664)
/* return TRUE iff op is load with UNAT bit (IA64)*/
extern BOOL CGTARG_Load_with_UNAT (OP* op); 

/* return TRUE iff op is store with UNAT bit (IA64) */
extern BOOL CGTARG_Store_With_UNAT (OP* op);

#endif

/* Returns TRUE if OP is a suitable candidate for HBF. */
extern BOOL CGTARG_Check_OP_For_HB_Suitability(OP *op);

#ifdef TARG_IA64
/* Return TRUE if OP is def use stack register; */
extern BOOL OP_def_use_stack_regs(OP* op);

/* return the max number of hidden operands given <op> may have */
extern INT32 CGTARG_Max_Number_of_Hidden_Opnd (mTOP top);

/* Go through all OP in the current PU and and hidden their hidden operands */
extern void CGTARG_Add_Implict_Operands (void);
#endif
extern TN* CGTARG_Gen_Dedicated_Subclass_TN( OP* op, int idx, BOOL is_result );

#ifdef TARG_IA64
typedef mempool_allocator<SCHED_INFO_CLASS>  SIC_MEM_ALLOC;
typedef	std::vector<SCHED_INFO_CLASS, SIC_MEM_ALLOC>  TOP_SET;
void Fix_MM_Latency ( BB *bb, TOP_SET *src_op_class, TOP_SET *tgt_op_class, UINT8 cycles_apart);
void Fix_Cache_Conflict_latency( BB *bb);
#endif

#ifdef KEY
// Return TRUE if OP accesses thread-local memory.
extern BOOL CGTARG_Is_Thread_Local_Memory_OP(OP *op);
#endif

#ifdef TARG_LOONGSON
// Emit code to turn on flush-to-zero mode when doing floating point calculation
void CGTARG_enable_FTZ(OPS& ops);
// Count how many registers a branch OP need of register class <cl>
INT CGTARG_branch_op_need_register_numbers(OP* , ISA_REGISTER_CLASS);
#endif

#ifdef TARG_SL
extern void CGTARG_Mem_AR_Dep(OP *pred_op, OP *succ_op, CG_DEP_KIND kind);
#endif
#endif /* CGTARGET_INCLUDED */
