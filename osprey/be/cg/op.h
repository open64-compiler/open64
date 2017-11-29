/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
 * Module: op.h
 * $Revision: 1.21 $
 * $Date: 06/03/14 14:38:58-08:00 $
 * $Author: tkong@hyalite.keyresearch $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.op.h $
 *
 * CGIR operation structure (OP) and utility routines.
 *
 * Exported types:
 *
 *   OP
 *     A machine-level operation.  The following accessors are provided.
 *     These are lvalues:
 *       SRCPOS OP_srcpos(OP *)	source position
 *
 *     These are rvalues with standard Set functions:
 *       UINT8 OP_unrolling(OP *op)
 *	 void Set_OP_unrolling(OP *op, UINT8 unrolling)
 *         Whether or not a loop body is unrolled, each OP in the loop
 *         body has an associated "unrolling" number.  This number is
 *         zero for the original loop body OPs, and incremented by one
 *         for each unrolled copy created.
 *
 *	 TODO: document rest of these
 *
 *     These are rvalues.  They are set by the BB/OPS Insert/Remove routines
 *     (see utility routines below and in "bb.h").
 *	 TOP OP_code(OP *)	opcode
 *	 BB *OP_bb(OP *)	BB (if any) to which this OP belongs
 *	 OP *OP_next(OP *)	next OP (if any) in OPS/BB
 *	 OP *OP_prev(OP *)	previous OP (if any) in OPS/BB
 *	 UINT16 OP_map_idx(OP*)	unique/compact/stable per-BB index
 *				(won't change unless OP is moved to diff BB)
 *				(undefined if OP_bb(op) == NULL)
 *     Constructors:
 *	 OP *Dup_OP(OP *op)
 *	    Return a copy of <op>, detached from any BB or OPS.
 *	 OP *Mk_OP(TOP opr, ...)
 *	    Create, and return, an OP with opcode <opr>. The variable
 *	    argument list contains the result TNs, followed immediately
 *	    by the operand TNs. The topcode determines the number of
 *	    result and operand parameters. This routine can only be
 *	    used with topcodes with a fixed number of operands/results.
 *	 OP *Mk_VarOP(TOP opr, INT results, INT opnds,
 *		      struct tn **res_tn, 
 *		      struct tn **opnd_tn)
 *	    Create, and return, an OP with opcode <opr>. The parameters
 *	    <results> and <operands> specify the number of results and
 *	    operands. The results and operands are contained in arrays,
 *	    pointed to by the parameters <res_tn> and <opnd_tn>.
 *	    Mk_VarOP is used for topcodes with a variable number of
 *	    operands/results, but may also be used with fixed topcodes.
 *
 *     Constants:
 *       OP_MAX_FIXED_OPNDS
 *       OP_MAX_FIXED_RESULTS
 *	   The maximum number of fixed operands and results an OP can
 *	   have. Note that OPs with variable numbers of operands, e.g. asm,
 *	   will have at least this number, but can have more.
 *
 *     Accessors:
 *	 TN *OP_result(OP *op, UINT8 i)
 *	 void Set_OP_result(OP *op, UINT8 i, TN *tn)
 *	 TN *OP_opnd(OP *op, UINT8 i)
 *	 void Set_OP_opnd(OP *op, UINT8 i, TN *tn)
 *
 *   OPS
 *     Internal doubly-linked list of OPS.  The following accessors are
 *     provided.  They are all rvalues.  See utility functions below for
 *     OPS_Insert/Remove functions to modify OPS.
 *       OP *OPS_first(OPS *)	first OP (if any) in OPS
 *	 OP *OPS_last(OPS *)	last OP (if any) in OPS
 *     Constructors:
 *	 OPS *OPS_Create(void)
 *	   Return a new empty OPS allocated in the PU pool.
 *     Initializers:
 *	 OPS *OPS_Init(OPS *ops)
 *	   Make <ops> empty and return it.
 *     Constants:
 *	 OPS OPS_EMPTY
 *	   Empty OPS used to initialize automatically allocated OPS.
 *
 *
 * Utility functions:
 *
 *   void OP_Change_Opcode(OP *op, TOP opc)
 *	Change the opcode of an OP. The new opcode must have the
 *	same number of fixed results and operands as the old
 *	opcode, and they must both have the same var_opnds property.
 *	If the client can't guarantee these conditions, then a
 *	new OP should be created.
 *
 *   void OP_Change_To_Noop(OP *op)
 *	Change the specified OP into a generic noop (TOP_noop).
 *
 *   INT64 OP_Ordering(OP *op1, OP *op2)
 *   BOOL OP_Precedes(OP *op1, OP *op2)
 *   BOOL OP_Follows(OP *op1, OP *op2)
 *   INT32 OP_Order(OP *op1, OP *op2)
 *     Requires: OP_bb(op1) == OP_bb(op2) && OP_bb(op1) != NULL
 *     OP_Ordering returns an integer indicating the relative order
 *     of <op1> and <op2> in the BB.  The value returned is:
 *	 > 0  iff  <op1> follows <op2>
 *	 = 0  iff  <op1> and <op2> are the same
 *	 < 0  iff  <op2> follows <op1>
 *     The values returned by OP_Ordering may be very sparsely distributed,
 *     and the actual value returned may differ for the same pair of OPs
 *     if any other OP is added anywhere in the BB between the calls to
 *     OP_Ordering.  OP_Follows(op1,op2) is (OP_Ordering(op1,op2) > 0).
 *     OP_Precedes(op1,op2) is (OP_Ordering(op1,op2) < 0).  Provided no OPs
 *     are added to the BB between calls to OP_Ordering, the following
 *     implications hold:
 *	(OP_Ordering(op1,op2) > OP_Ordering(op1,op3)) ==> OP_Follows(op2,op3)
 *	(OP_Ordering(op1,op2) < OP_Ordering(op1,op3)) ==> OP_Precedes(op2,op3)
 *     OP_Order is similar to OP_Ordering, except that a) the return
 *     value is 32 bits, and b) the return value is -1,0,1 (so there
 *     is no significance in the magnatude of the values).
 *
 *   void OPS_Insert_Op(OPS *ops, OP *point, OP *op, BOOL before)
 *     Requires: <point> is in <ops>.  <op> isn't in another OPS.
 *     Insert <op> in <ops>.  If <point> isn't NULL, insert before
 *     <point> if <before> or after it otherwise.  If <point> is NULL
 *     insert at start of <ops> if <before>, or end of <ops> otherwise.
 *
 *   void OPS_Insert_Ops(OPS *ops, OP *point, OPS *insert_ops, BOOL before)
 *     Requires: <point> is in <ops>.  <insert_ops> isn't in another OPS.
 *     Insert <insert_ops> in <ops>.  If <point> isn't NULL, insert before
 *     <point> if <before> or after it otherwise.  If <point> is NULL
 *     insert at start of <ops> if <before>, or end of <ops> otherwise.
 *
 *   void OPS_Insert_Op_Before(OPS *ops, OP *point, OP *op)
 *   void OPS_Insert_Op_After(OPS *ops, OP *point, OP *op)
 *   void OPS_Append_Op(OPS *ops, OP *op)
 *   void OPS_Prepend_Op(OPS *ops, OP *op)
 *   void OPS_Insert_Ops_Before(OPS *ops, OP *point, OPS *insert_ops)
 *   void OPS_Insert_Ops_After(OPS *ops, OP *point, OPS *insert_ops)
 *   void OPS_Append_Ops(OPS *ops, OPS *insert_ops)
 *   void OPS_Prepend_Ops(OPS *ops, OPS *insert_ops)
 *     More-specific names for the general Insert routines.  These do
 *     what you think they should do.
 *
 *   void OPS_Remove_Op(OPS *ops, OP *op)
 *     Requires: <op> is in <ops>.
 *     Remove <op> from <ops>.
 *
 *   void OPS_Remove_Ops(OPS *ops, OP *remove_ops)
 *     Requires: <remove_ops> is in <ops>.
 *     Remove <remove_ops> from <ops>.
 *
 *   void OPS_Remove_All(OPS *ops)
 *     Make <ops> empty.
 *
 *   UINT32 OPS_length(OPS *ops)
 *     Returns the number of OPs in <ops>.
 *
 *   void Print_OP(OP *op)
 *   void Print_OPs(OP *first)
 *   void Print_OPS(OPS *ops)
 *     Print OP(s) to the trace file.  Print_OP prints only <op>;
 *     Print_OPs prints the list of OPs starting with <first>;
 *     Print_OPS prints the OPs in <ops>.
 *
 *   void Print_OP_No_SrcLine(OP *op)
 *   void Print_OPs_No_SrcLines(OP *first)
 *   void Print_OPS_No_SrcLines(OPS *ops)
 *     Same as above, but don't print lines from source code with the OP(s).
 *
 *   BOOL OP_Is_Float_Mem(const OP *op)
 *     Return a boolean that indicates whether <op> is a floating point
 *     memory operation, i.e. a floating point load or store.
 *
 *   BOOL OP_same_res(OP *op)
 *     OPER_same_res(OP_code(op)) indicates that an OP *may* have an
 *     operand that needs to be the same register as the result. This
 *     function checks a specific instance.  (E.g., the initial unaligned
 *     load instruction in an unaligned load pair does NOT need to have
 *     its last operand be the same as the result.)
 *     OP_same_res is target-specific; it's defined in TARGET/op_targ.h
 *
 *   BOOL OP_Defs_TN(const OP *op, const struct tn *res)
 *   BOOL OP_Refs_TN(const OP *op, const struct tn *opnd)
 *     Determine if the OP defines/references the given TN.
 *
 *   BOOL OP_Defs_Reg(const OP *op, ISA_REGISTER_CLASS rclass, REGISTER reg)
 *   BOOL OP_Refs_Reg(const OP *op, ISA_REGISTER_CLASS rclass, REGISTER reg)
 *     Determine if the OP defines/references the given register.
 *
 *   void OP_Base_Offset_TNs(OP *memop, TN **base_tn, TN **offset_tn)
 *     A generic utility routine which returns the <base_tn> and the
 *     <offset_tn> for the given <memory_op>.
 *
 *   const INT OP_COPY_OPND
 *	For OPs flagged with OP_copy, OP_COPY_OPND gives the operand
 *	number the source operand.
 *
 *   const INT OP_PREDICATE_OPND
 *	For OPs flagged with OP_has_predicate, OP_PREDICATE_OPND gives the
 *	operand number of the predicate operand.
 *
 *   BOOL OP_cond_def(OP *)
 *     OP_cond_def is an attribute of the OP including its operands.
 * 
 *     OP_cond_def can be solely derived from the def OP and its uses.
 *     For example, OP_cond_def is position independent.  It does not change
 *     if the OP is moved into the delay slot of a branch-likely on MIPS.
 *     The higher level routines are responsible to model nullify-able delay 
 *     slots.
 *
 *     OP_cond_def is not set for OPi defining TNj for 0 <= j < OP_opnds(OPi)
 *     if there is no OPk reachable from OPi referencing a previous defined 
 *     value of any TNj.  It is set otherwise.
 *
 *   BOOL OP_has_implicit_interactions
 *     A generic placeholder routine where all OPs with implicit interactions
 *     with other OPs in an non-obvious way are placed.
 *
 *   BOOL TN_Pair_In_OP (OP* op, TN *tn_res, TN* tn_opnd)
 *      Returns TRUE if <op> contains <tn_res> as a result
 *      and <tn_opnd> as an operand.
 *
 *   INT TN_Resnum_In_OP (OP* op, TN *tn)
 *      This routine assumes that the given <tn> is a result in <op>.
 *      It returns an INT that is the value of the result-number for
 *      this <tn> in this <op>.
 *
 *   INT TN_Opernum_In_OP (OP* op, TN *tn)
 *      This routine assumes that the given <tn> is an operand in <op>.
 *      It returns an INT that is the value of the operand-number for
 *      this <tn> in this <op>.
 *
 * TODO: Complete this interface description.
 *
 *
 * ====================================================================
 * ==================================================================== */


#ifndef op_INCLUDED
#define op_INCLUDED

#ifdef TARG_IA64
/* Include the values for the variant field: */
#include "variants.h"
#include <vector>
using std::vector;
#endif

/* to get the definition of SRCPOS. */
#include "srcpos.h"

#include "targ_isa_operands.h"
#include "targ_isa_properties.h"
#include "targ_isa_hazards.h"
#include "targ_isa_pack.h"

/* to get Is_True */
#include "errors.h"

/* to get definition of REGISTER */
#include "register.h"

/* to get TYPE_MEM_POOL_ALLOC */
#include "mempool.h"

/* Declare some structures from elsewhere: */
struct tn;
struct bb;

/* ---------------------------------------------------------------------
 *			       OP stuff
 * ---------------------------------------------------------------------
 */

/* Define the maximum number of fixed operands and results an OP can
 * have. Note that OPs with variable numbers of operands, e.g. asm,
 * may have more operands and/or results.
 */
#define OP_MAX_FIXED_OPNDS	ISA_OPERAND_max_operands
#define OP_MAX_FIXED_RESULTS	ISA_OPERAND_max_results

/* HD - To use TN_LIST, and tn.h is not included yet  */
struct TN_LIST;

/* Define the OP structure: */
typedef struct op OP;
typedef struct op {
  SRCPOS	srcpos;		/* source position for the OP */
  OP		*next;		/* Next OP in BB list */
  OP		*prev;		/* Preceding OP in BB list */
  struct bb	*bb;		/* BB in which this OP lives */
  struct bb	*unroll_bb;	/* BB just after unrolling */
  mUINT32	flags;		/* attributes associated with OP */
  mUINT16  	order;		/* relative order in BB */
  mINT16        variant;        /* Opcode variant */
  mUINT16	map_idx;	/* index used by OP_MAPs; unique in BB */
  mUINT16	orig_idx;	/* index of orig op before unrolling */
#ifdef TARG_IA64
  mUINT16	orig_bb_id;	/* id of orig bb before speculation */
#endif
  mINT16	scycle;		/* Start cycle */
#ifdef TARG_X8664
  union {			/* share some data - keep OP struct size down */
    struct {
      mINT16	d_group;	/* Dispatch group */
      mUINT8    p_size;         /* padding size, if any */
    } ds_info; 
    struct {
      mUINT16	aux_opr;	/* Tokenized top for special funcionality */
      mUINT8	aux_idx;	/* Token for index of opnd */
    } aux_data;
  } aux_info;
#endif
  mTOP		opr;		/* Opcode. topcode.h */
  mUINT8	unrolling;	/* which unrolled replication (if any) */
#if defined(TARG_SL)
  TN_LIST       *extra_result;  /* HD - For sl2, I need an extra list 
                                 * of resutl/operand TNs */
  TN_LIST       *extra_operand;
#endif
  mUINT8	results;	/* Number of results */
  mUINT8	opnds;		/* Number of operands */
#ifdef TARG_IA64
  mUINT8       hidden_opnds;   /* Number of hidden operands, <opnds> include
				* hidden_opnds*/
  mUINT8        flag_value_profile;       /* flag to identify value_profile */
  mUINT32       value_profile_id;       /* unique ID to indicate value profiled No. */
  mUINT8        flag_stride_profile;      /* flag to identify stride_profile */
  mUINT32       stride_profile_id;        /*  unique ID to indicate stride profile No. */
  mUINT64        exec_count;          /* Number of executed count */
  mUINT8        flag_stride_prefetch;     /*flag to identify stride_prefetch */
#endif
  struct tn     *res_opnd[10];	/* result/operand array (see OP_sizeof for info)
				 * !!! THIS FIELD MUST BE LAST !!!
				 */
} OP;

/* Return the 'sizeof' the OP struct needed for the specified number
 * of operands and results. For the sake of simplicity, the result of 
 * the calculation is not rounded up to the alignment of the struct. 
 * Note that the sizeof the res_opnd field as declared in the struct 
 * is arbitrary and is set to something other than one only for convenience 
 * in printing structs in the debugger (OP_sizeof's result is independent 
 * of the dimension).
 */
#define OP_sizeof(nresults, nopnds) \
	((size_t)(offsetof(OP, res_opnd[0]) + ((nresults) + (nopnds)) * sizeof(TN *)))

/* Both the operand and result TNs are stored in _res_opnds.
 * OP_opnd_offset and OP_result_offset give the offset into the
 * array to the start of the operands and results. 
 * NOTE: the offset is NOT constant for all OPs!!!
 * NOTE: Add_Hidden_Operands() and Remove_Hidden_Operands() also 
 *   assume that results are placed right after source operands.  
 *   If the layout is changed, these two functions need to be changed
 *   too.
 */

#define OP_opnd_offset(o)	(0)
#define OP_result_offset(o)	OP_opnds(o)

/* Define the access functions: */
#define OP_srcpos(o)	((o)->srcpos)
#ifdef KEY
#define OP_variant(o)	((o)->variant)
#endif
#define OP_scycle(o)	((o)->scycle)
#ifdef TARG_X8664
#define OP_dgroup(o)	((o)->aux_info.ds_info.d_group)
#define OP_dpadd(o)	((o)->aux_info.ds_info.p_size)
#define OP_auxcode(o)	((o)->aux_info.aux_data.aux_opr)
#define OP_auxidx(o)	((o)->aux_info.aux_data.aux_idx)
#endif
#define OP_flags(o)	((o)->flags)
#ifdef TARG_IA64
#define OP_flags_val_prof(o)    ((o)->flag_value_profile)
#define OP_val_prof_id(o)        ((o)->value_profile_id)
#define OP_flags_srd_prof(o)    ((o)->flag_stride_profile)
#define OP_srd_prof_id(o)        ((o)->stride_profile_id)
#define OP_exec_count(o)        ((o)->exec_count)
#endif
/* These are rvalues */
#define OP_next(o)	((o)->next+0)
#define OP_prev(o)	((o)->prev+0)
#define OP_map_idx(o)	((o)->map_idx+0)
#define OP_orig_idx(o)	((o)->orig_idx+0)
#ifdef TARG_IA64
#define OP_orig_bb_id(o) ((o)->orig_bb_id+0)
#endif
#define OP_unroll_bb(o)	((o)->unroll_bb+0)
#define OP_unrolling(o)	((o)->unrolling+0)
#define OP_bb(o)	((o)->bb+0)
#define OP_results(o)	((o)->results+0)
#define OP_opnds(o)	((o)->opnds+0)
#ifdef TARG_IA64
#define OP_hidden_opnds(o) ((o)->hidden_opnds+0)
#endif
#define OP_code(o)	((TOP)(o)->opr)
#define OP_result(o,n)	((struct tn *)(o)->res_opnd[(n)+OP_result_offset(o)])
#define OP_opnd(o,n)	((struct tn *)(o)->res_opnd[(n)+OP_opnd_offset(o)])

#if defined(TARG_SL)
#define OP_extra_rslt(o)  ((TN_LIST*)(o)->extra_result)
#define OP_extra_opnd(o) ((TN_LIST*)(o)->extra_operand)
#endif 

/* Mutators: */
#define Set_OP_orig_idx(op,idx) ((op)->orig_idx = (idx))
#ifdef TARG_IA64
#define Set_OP_orig_bb_id(op,id) ((op)->orig_bb_id = (id))
#endif
#define Set_OP_unroll_bb(op,bb) ((op)->unroll_bb = (bb))
#define Set_OP_unrolling(op,u)	((op)->unrolling = (u))
#define Set_OP_result(o,result,tn) \
	((o)->res_opnd[(result) + OP_result_offset(o)] = (tn))
#define Set_OP_opnd(o,opnd,tn) \
	((o)->res_opnd[(opnd) + OP_opnd_offset(o)] = (tn))

#ifdef TARG_LOONGSON
#define Set_OP_bb(o,b)      ((o)->bb = (b))
#define Set_OP_prev(o,p)    ((o)->prev = (p))
#define Set_OP_next(o,n)    ((o)->next = (n))
#endif

/*
 * Define the OP cond def mask.
 *
 * OP_ALWAYS_UNC_DEF:
 *   Results are always unconditionally defined, independent of
 *   the value of the control predicate.  No upward exposed use!
 *   All unconditional compare falls into this category.
 *   All local TN def falls into this category.
 *
 * OP_ALWAYS_COND_DEF:
 *   Results are conditionally defined.  Whether the result is modified
 *   cannot not be solely determined by the OP's control predicate.
 *   All parallel compare falls into this category.
 *
 * PREDICATED_DEF:
 *   Results are defined if the control predicate evaluates to TRUE.
 *
 * Please notice that the OP_cond_def_type is sticky.  It does not
 * need to updated by optimizations (such as HB and EBO).  It is
 * initialized when the OP is first created.   [OP created through
 * Mk_OP will have this initialized automatically.  Other means
 * of OP creation have to worry about the initialization]
 *
 * It can be promoted (i.e, from PREDICATED_DEF to ALWAYS_UNC_DEF)
 * when any TN is discovered to be local.  Promotion is optional
 * in the sense that it is required by optimization, but not
 * by correctness.
 *
 */
enum OP_COND_DEF_KIND {
  OP_UNKNOWN_DEF = 0,
  OP_ALWAYS_UNC_DEF = 0x800,      
  OP_ALWAYS_COND_DEF = 0x1000,
  OP_PREDICATED_DEF = 0x1800,
  OP_MASK_COND_DEF = 0x1800,   
};

/* Define the flag masks. */

#define OP_MASK_GLUE	      0x00000001 /* Is OP region "glue"? */
#define OP_MASK_NO_ALIAS      0x00000002 /* Is OP Memop that can't be aliased */
#define OP_MASK_COPY	      0x00000004 /* Is OP a COPY? */
#define OP_MASK_FLAG1	      0x00000008 /* temporary flag for local use */
#define OP_MASK_VOLATILE      0x00000010 /* Is OP a volatile Memop? */
#define OP_MASK_HOISTED       0x00000020 /* Is there a WN map attatched to a hoisted OP */
#define OP_MASK_END_GROUP     0x00000040 /* Is OP end of an instruction group */
#define OP_MASK_M_UNIT	      0x00000080 /* Is OP assigned to M unit */
#define OP_MASK_TAIL_CALL     0x00000100 /* Is OP a tail call? */
#define OP_MASK_BUNDLED	      0x00000200 /* Is OP bundled? */
#define OP_MASK_SPECULATIVE   0x00000400 /* Is OP a speculative live-range op? */
/* OP_MAKS_COND_DEF  0x00001800  -- See enum defined above  */
#define OP_MASK_NO_CI_ALIAS   0x00002000 /* no cross-iteration alias */
#define OP_MASK_NO_MOVE_BEFORE_GRA \
                              0x00004000 /* do not move this op before GRA */
#define OP_MASK_TAG 	      0x00008000 /* OP has tag */
#define OP_MASK_SPADJ_PLUS    0x00010000 /* Is OP de-alloca spadjust (plus)? */
#define OP_MASK_SPADJ_MINUS   0x00020000 /* Is OP alloca spadjust (minus)? */
#ifdef TARG_IA64
#define OP_MASK_START_BD      0x00040000 /* Is OP start of a bundle */
#define OP_MASK_SAFE_LOAD     0x00080000 /* Is OP safe load */
#define OP_MASK_SCHEDULED     0x00100000 /* Has OP been scheduled */
#define OP_MASK_CNTL_SPEC     0x00200000 /* Is OP control speculated? */ 
#define OP_MASK_DATA_SPEC     0x00400000 /* Is OP data speculated? */
#define OP_MASK_PREFETCHED    0x00800000 /* Has OP been prefetched? */
#define OP_MASK_IF_CONVERTED  0x01000000 /* Is OP if-converted? */
#define OP_MASK_RENAMED	      0x02000000 /* Is OP renamed by GLOS */
#define OP_MASK_SPILL_RESTORE 0x04000000 /* Is OP a spill or restore  */
#define OP_MASK_LD_GOT_ENT    0x08000000 /* load GOT entry */ 
#endif
#define OP_MASK_FIRST_OP_AFTER_PREAMBLE_END  \
                              0x10000000 /* first OP following PRAGMA_PREAMBLE_END? */
#define OP_MASK_PREFETCH_DELETED \
                              0x20000000 /* prefetch deleted by scheduler */

#ifdef TARG_X8664
#define OP_MASK_MEMORY_HI     0x00040000 /* Is OP load/store the high 32-bit? */
#define OP_RES_NORENAME       0x00080000 /* lock result tn as non renamable */
#define OP_MASK_COMPUTES_GOT  0x00100000 /* Does OP compute GOT ? */
#define OP_MASK_PREFIX_LOCK   0x01000000
#endif
#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS)
# define OP_renamed(op)		(OP_flags(op) & OP_MASK_RENAMED)
# define Set_OP_renamed(o)	(OP_flags(o) |= OP_MASK_RENAMED)
# define Reset_OP_renamed(o)	(OP_flags(o) &= ~OP_MASK_RENAMED)
 
#if defined (TARG_MIPS) && !defined(TARG_SL)
#define OP_MASK_RENAMED       0x00100000  /* Is OP renamed during global sched */
#endif
#if defined(TARG_SL) 
#define OP_MASK_RENAMED       0x00100000  /* Is OP renamed during global sched */
#define OP_MASK_BC2_OP        0x00200000  /* Is OP renamed during global sched */
#define OP_MASK_C2_BR_OP      0x00400000
#define OP_MASK_16bit_OP      0x00400000  /* Is OP rechanged as 16-bit OP*/
#define OP_MASK_PAIRED_OP     0x00800000  /* Is OP paired: 2 continus 16-bit OP */ 
#define OP_MASK_ARdep_OP      0x01000000  /* Is OP dep with normal memory OP */
#define OP_bc2_op(op)		(OP_flags(op) & OP_MASK_BC2_OP)
#define Set_OP_bc2_op(o)	(OP_flags(o) |= OP_MASK_BC2_OP)
#define Reset_OP_bc2_op(o)	(OP_flags(o) &= ~OP_MASK_BC2_OP)
#define OP_16bit_op(op)        (OP_flags(op) & OP_MASK_16bit_OP)
#define Set_OP_16bit_op(o)     (OP_flags(o) |= OP_MASK_16bit_OP)
#define Reset_OP_16bit_op(o)   (OP_flags(o) &= ~OP_MASK_16bit_OP)
#define OP_Paired_op(op)       (OP_flags(op) & OP_MASK_PAIRED_OP)
#define Set_OP_Paired_op(o)    (OP_flags(o) |= OP_MASK_PAIRED_OP)
#define Reset_OP_Paired_op(o)  (OP_flags(o) &= ~OP_MASK_PAIRED_OP)
#define OP_c2_br_op(op)		(OP_flags(op) & OP_MASK_C2_BR_OP)
#define Set_OP_c2_br_op(o)	(OP_flags(o) |= OP_MASK_C2_BR_OP)
#define Reset_OP_c2_br_op(o)	(OP_flags(o) &= ~OP_MASK_C2_BR_OP)
#define OP_ARdep(o)           (OP_flags(o) & OP_MASK_ARdep_OP)
#define Set_OP_ARdep(o)       (OP_flags(o) |= OP_MASK_ARdep_OP)
#define Reset_OP_ARdep(o)     (OP_flags(o) &= ~OP_MASK_ARdep_OP)
#endif //TARG_SL
#endif 

#if defined(TARG_PPC32) 
#define OP_MASK_BC2_OP        0x00200000  /* Is OP renamed during global sched */
#define OP_MASK_C2_BR_OP 0x00400000
#define OP_MASK_16bit_OP 0x00400000  /* Is OP rechanged as 16-bit OP*/
#define OP_MASK_PAIRED_OP 0x00800000  /* Is OP paired: 2 continus 16-bit OP */ 

#define OP_MASK_LR_SPILL_OP 0x04000000
#define OP_LR_spill(o)          (OP_flags(o) &  OP_MASK_LR_SPILL_OP)
#define Set_OP_LR_spill(o)      (OP_flags(o) |= OP_MASK_LR_SPILL_OP)
#define Reset_OP_LR_spill(o)	  (OP_flags(o) &= ~OP_MASK_LR_SPILL_OP)

#define OP_bc2_op(op)		(OP_flags(op) & OP_MASK_BC2_OP)
#define Set_OP_bc2_op(o)	(OP_flags(o) |= OP_MASK_BC2_OP)
#define Reset_OP_bc2_op(o)	(OP_flags(o) &= ~OP_MASK_BC2_OP)
#define OP_16bit_op(op)        (OP_flags(op) & OP_MASK_16bit_OP)
#define Set_OP_16bit_op(o)     (OP_flags(o) |= OP_MASK_16bit_OP)
#define Reset_OP_16bit_op(o)   (OP_flags(o) &= ~OP_MASK_16bit_OP)
#define OP_Paired_op(op)       (OP_flags(op) & OP_MASK_PAIRED_OP)
#define Set_OP_Paired_op(o)    (OP_flags(o) |= OP_MASK_PAIRED_OP)
#define Reset_OP_Paired_op(o)  (OP_flags(o) &= ~OP_MASK_PAIRED_OP)
#endif //TARG_PPC32

# define OP_glue(o)		(OP_flags(o) & OP_MASK_GLUE)
# define Set_OP_glue(o)		(OP_flags(o) |= OP_MASK_GLUE)
# define Reset_OP_glue(o)	(OP_flags(o) &= ~OP_MASK_GLUE)
# define OP_no_alias(o)		(OP_flags(o) & OP_MASK_NO_ALIAS)
# define Set_OP_no_alias(o)	(OP_flags(o) |= OP_MASK_NO_ALIAS)
# define Reset_OP_no_alias(o)	(OP_flags(o) &= ~OP_MASK_NO_ALIAS)
# define OP_copy(o)		(OP_flags(o) & OP_MASK_COPY)
# define Set_OP_copy(o)		(OP_flags(o) |= OP_MASK_COPY)
# define Reset_OP_copy(o)	(OP_flags(o) &= ~OP_MASK_COPY)
# define OP_flag1(o)		(OP_flags(o) & OP_MASK_FLAG1)
# define Set_OP_flag1(o)	(OP_flags(o) |= OP_MASK_FLAG1)
# define Reset_OP_flag1(o)	(OP_flags(o) &= ~OP_MASK_FLAG1)
# define OP_volatile(o)		(OP_flags(o) & OP_MASK_VOLATILE)
# define Set_OP_volatile(o)	(OP_flags(o) |= OP_MASK_VOLATILE)
# define Reset_OP_volatile(o)	(OP_flags(o) &= ~OP_MASK_VOLATILE)
# define OP_hoisted(o)		(OP_flags(o) & OP_MASK_HOISTED)
# define Set_OP_hoisted(o)	(OP_flags(o) |= OP_MASK_HOISTED)
# define Reset_OP_hoisted(o)	(OP_flags(o) &= ~OP_MASK_HOISTED)
# define OP_end_group(o)	(OP_flags(o) & OP_MASK_END_GROUP)
# define Set_OP_end_group(o)	(OP_flags(o) |= OP_MASK_END_GROUP)
# define Reset_OP_end_group(o)	(OP_flags(o) &= ~OP_MASK_END_GROUP)
# define OP_m_unit(o)		(OP_flags(o) & OP_MASK_M_UNIT)
# define Set_OP_m_unit(o)	(OP_flags(o) |= OP_MASK_M_UNIT)
# define Reset_OP_m_unit(o)	(OP_flags(o) &= ~OP_MASK_M_UNIT)
# define OP_tail_call(o)	(OP_flags(o) & OP_MASK_TAIL_CALL)
# define Set_OP_tail_call(o)	(OP_flags(o) |= OP_MASK_TAIL_CALL)
# define Reset_OP_tail_call(o)	(OP_flags(o) &= ~OP_MASK_TAIL_CALL)
# define OP_bundled(o)		(OP_flags(o) & OP_MASK_BUNDLED)
# define Set_OP_bundled(o)	(OP_flags(o) |= OP_MASK_BUNDLED)
# define Reset_OP_bundled(o)	(OP_flags(o) &= ~OP_MASK_BUNDLED)
# define OP_speculative(o)	(OP_flags(o) & OP_MASK_SPECULATIVE)
# define Set_OP_speculative(o)	(OP_flags(o) |= OP_MASK_SPECULATIVE)
# define Reset_OP_speculative(o)(OP_flags(o) &= ~OP_MASK_SPECULATIVE)
# define OP_cond_def_kind(o)	(OP_flags(o) & OP_MASK_COND_DEF)
# define Set_OP_cond_def_kind(o,k) (OP_flags(o) = (OP_flags(o) & ~OP_MASK_COND_DEF) | k)
# define OP_no_ci_alias(o)	(OP_flags(o) & OP_MASK_NO_CI_ALIAS)
# define Set_OP_no_ci_alias(o)	(OP_flags(o) |= OP_MASK_NO_CI_ALIAS)
# define Reset_OP_no_ci_alias(o) (OP_flags(o) &= ~OP_MASK_NO_CI_ALIAS)
# define OP_no_move_before_gra(o) (OP_flags(o) & OP_MASK_NO_MOVE_BEFORE_GRA)
# define Set_OP_no_move_before_gra(o) (OP_flags(o) |= OP_MASK_NO_MOVE_BEFORE_GRA)
# define OP_has_tag(o)		(OP_flags(o) & OP_MASK_TAG)
# define Set_OP_has_tag(o)	(OP_flags(o) |= OP_MASK_TAG)
# define Reset_OP_has_tag(o) 	(OP_flags(o) &= ~OP_MASK_TAG)
# define OP_spadjust_plus(o)	(OP_flags(o) & OP_MASK_SPADJ_PLUS)
# define Set_OP_spadjust_plus(o)(OP_flags(o) |= OP_MASK_SPADJ_PLUS)
# define Reset_OP_spadjust_plus(o) (OP_flags(o) &= ~OP_MASK_SPADJ_PLUS)
# define OP_spadjust_minus(o)	(OP_flags(o) & OP_MASK_SPADJ_MINUS)
# define Set_OP_spadjust_minus(o) (OP_flags(o) |= OP_MASK_SPADJ_MINUS)
# define Reset_OP_spadjust_minus(o) (OP_flags(o) &= ~OP_MASK_SPADJ_MINUS)
#ifdef TARG_IA64
# define OP_start_bundle(o)	(OP_flags(o) & OP_MASK_START_BD)
# define Set_OP_start_bundle(o)	(OP_flags(o) |= OP_MASK_START_BD)
# define Reset_OP_start_bundle(o)	(OP_flags(o) &= ~OP_MASK_START_BD)
# define OP_safe_load(o) (OP_flags(o) & OP_MASK_SAFE_LOAD)
# define Set_OP_safe_load(o)  (OP_flags(o) |= OP_MASK_SAFE_LOAD)
# define Reset_OP_safe_load(o) (OP_flags(o) &= ~OP_MASK_SAFE_LOAD)

# define OP_Scheduled(o)       (OP_flags(o) & OP_MASK_SCHEDULED)
# define Set_OP_Scheduled(o)   (OP_flags(o) |= OP_MASK_SCHEDULED)
# define Reset_OP_Scheduled(o) (OP_flags(o) &= ~OP_MASK_SCHEDULED)

# define OP_Prefetched(o)       (OP_flags(o) & OP_MASK_PREFETCHED)
# define Set_OP_Prefetched(o)   (OP_flags(o) |= OP_MASK_PREFETCHED)
# define Reset_OP_Prefetched(o) (OP_flags(o) &= ~OP_MASK_PREFETCHED)

//# define OP_catch_nat(o)  (OP_flags(o) & OP_MASK_CATCH_NAT)
//# define Set_OP_catch_nat(o)  (OP_flags(o) |= OP_MASK_CATCH_NAT)
//# define Reset_OP_catch_nat(o)  (OP_flags(o) &= ~OP_MASK_CATCH_NAT)
# define OP_cntl_spec(o)           (OP_flags(o) &  OP_MASK_CNTL_SPEC)
# define Set_OP_cntl_spec(o)       (OP_flags(o) |= OP_MASK_CNTL_SPEC)
# define Reset_OP_cntl_spec(o)     (OP_flags(o) &= ~OP_MASK_CNTL_SPEC)
# define OP_data_spec(o)           (OP_flags(o) &  OP_MASK_DATA_SPEC)
# define Set_OP_data_spec(o)       (OP_flags(o) |= OP_MASK_DATA_SPEC)
# define Reset_OP_data_spec(o)     (OP_flags(o) &= ~OP_MASK_DATA_SPEC)
# define OP_if_converted(o)			(OP_flags(o) & OP_MASK_IF_CONVERTED)
# define Set_OP_if_converted(o)		(OP_flags(o) |= OP_MASK_IF_CONVERTED)
# define Reset_OP_if_converted(o)	(OP_flags(o) &= ~OP_MASK_IF_CONVERTED)
# define OP_spill_restore(op)			(OP_flags(op) & OP_MASK_SPILL_RESTORE)
# define Set_OP_spill_restore(o)		(OP_flags(o) |= OP_MASK_SPILL_RESTORE)
# define Reset_OP_spill_restore(o)	(OP_flags(o) &= ~OP_MASK_SPILL_RESTORE)
# define OP_load_GOT_entry(o)           (OP_flags(o) & OP_MASK_LD_GOT_ENT)
# define Set_OP_load_GOT_entry(o)       (OP_flags(o) |= OP_MASK_LD_GOT_ENT)
# define Reset_OP_load_GOT_entry(o)     (OP_flags(o) &= ~OP_MASK_LD_GOT_ENT)
#endif

#ifdef TARG_X8664
# define OP_res_norename(o)	(OP_flags(o) & OP_RES_NORENAME)
# define Set_OP_res_norename(o)	(OP_flags(o) |= OP_RES_NORENAME)
# define Reset_OP_res_norename(o)	(OP_flags(o) &= ~OP_RES_NORENAME)
# define OP_memory_hi(o)	(OP_flags(o) & OP_MASK_MEMORY_HI)
# define Set_OP_memory_hi(o)	(OP_flags(o) |= OP_MASK_MEMORY_HI)
# define Reset_OP_memory_hi(o)	(OP_flags(o) &= ~OP_MASK_MEMORY_HI)
# define OP_computes_got(o)     (OP_flags(o) & OP_MASK_COMPUTES_GOT)
# define Set_OP_computes_got(o) (OP_flags(o) |= OP_MASK_COMPUTES_GOT)
# define Set_OP_prefix_lock(o)  (OP_flags(o) |= OP_MASK_PREFIX_LOCK)
# define OP_prefix_lock(o)      (OP_flags(o) & OP_MASK_PREFIX_LOCK)
#endif // TARG_X8664

# define OP_first_after_preamble_end(o) (OP_flags(o) & OP_MASK_FIRST_OP_AFTER_PREAMBLE_END)
# define Set_OP_first_after_preamble_end(o) (OP_flags(o) |= OP_MASK_FIRST_OP_AFTER_PREAMBLE_END)
# define Reset_OP_first_after_preamble_end(o) (OP_flags(o) &= ~OP_MASK_FIRST_OP_AFTER_PREAMBLE_END)

# define OP_prefetch_deleted(o) (OP_flags(o) & OP_MASK_PREFETCH_DELETED)
# define Set_OP_prefetch_deleted(o) (OP_flags(o) |= OP_MASK_PREFETCH_DELETED)
# define Reset_OP_prefetch_deleted(o) (OP_flags(o) &= ~OP_MASK_PREFETCH_DELETED)

extern BOOL OP_cond_def(const OP*);
extern BOOL OP_has_implicit_interactions(OP*);
#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS)
extern BOOL OP_def_return_value (OP*);
#endif
#ifdef TARG_IA64
extern BOOL OP_xfer(OP*);           // After RBG, chk should be taken as xfer.

extern BOOL OP_restore_b0(OP*);
extern BOOL OP_restore_ar_pfs(OP*);
extern BOOL OP_def_ar_lc(OP*);
extern BOOL OP_use_return_value(OP*); 
#endif

/* Convenience access macros for properties of the OP */
/* TODO: define all the macros for OP properties. */
#define OP_noop(o)		(TOP_is_noop(OP_code(o)))
#define OP_load(o)		(TOP_is_load(OP_code(o)))
#define OP_store(o)		(TOP_is_store(OP_code(o)))
#define OP_prefetch(o)	(TOP_is_prefetch(OP_code(o)))
#if defined(TARG_SL)        
#define OP_alu(o)              (TOP_is_alu_instr(OP_code(o)))
#endif
#ifdef TARG_X8664
#define OP_load_exe(o)		(TOP_is_load_exe(OP_code(o)))
#define OP_load_exe_store(o)	(TOP_is_load_exe_store(OP_code(o)))
#define OP_memory(o)		(OP_load(o) | OP_store(o) | OP_prefetch(o))
#define OP_mcode(o)             (TOP_is_mcode(OP_code(o)))
#define OP_is4(o)               (TOP_is_is4_reg(OP_code(o)))
#define OP_vec_lo_ldst(o)       (TOP_is_vector_lo_loadstore(OP_code(o)))
#define OP_vec_hi_ldst(o)       (TOP_is_vector_high_loadstore(OP_code(o)))
#else
#define OP_memory(o)		(OP_load(o) | OP_store(o) | OP_prefetch(o))
#endif
#if defined(TARG_X8664)
#define OP_mem_fill_type(o)     (0)
#define OP_likely(o)		(0)
#else
#define OP_mem_fill_type(o)     (TOP_is_mem_fill_type(OP_code(o)))
#define OP_likely(o)		(TOP_is_likely(OP_code(o)))
#endif
#define OP_call(o)		(TOP_is_call(OP_code(o)))
#if defined(TARG_X8664) || defined(TARG_SL) || defined(TARG_NVISA) || defined(TARG_MIPS) || defined(TARG_PPC32) || defined(TARG_LOONGSON)
#define OP_xfer(o)		(TOP_is_xfer(OP_code(o)))
#endif
#define OP_cond(o)		(TOP_is_cond(OP_code(o)))
#define OP_dummy(o)		(TOP_is_dummy(OP_code(o)))
#define OP_flop(o)		(TOP_is_flop(OP_code(o)))
#define OP_sse5(o)		(TOP_is_non_destructive(OP_code(o)))
#define OP_fadd(o)		(TOP_is_fadd(OP_code(o)))
#define OP_fdiv(o)		(TOP_is_fdiv(OP_code(o)))
#define OP_fmul(o)		(TOP_is_fmul(OP_code(o)))
#define OP_fmisc(o)		(TOP_is_fmisc(OP_code(o)))
#define OP_fsub(o)		(TOP_is_fsub(OP_code(o)))
#define OP_iadd(o)		(TOP_is_iadd(OP_code(o)))
#define OP_ior(o)		(TOP_is_ior(OP_code(o)))
#define OP_ixor(o)		(TOP_is_ixor(OP_code(o)))
#define OP_iand(o)		(TOP_is_iand(OP_code(o)))
#define OP_icmp(o)		(TOP_is_icmp(OP_code(o)))
#define OP_idiv(o)		(TOP_is_idiv(OP_code(o)))
#define OP_imul(o)		(TOP_is_imul(OP_code(o)))
#define OP_isub(o)		(TOP_is_isub(OP_code(o)))
#define OP_madd(o)		(TOP_is_madd(OP_code(o)))
#define OP_sqrt(o)		(TOP_is_sqrt(OP_code(o)))
#define OP_mmmul(o)		(TOP_is_mmmul(OP_code(o)))
#define OP_mmshf(o)		(TOP_is_mmshf(OP_code(o)))
#define OP_mmalu(o)		(TOP_is_mmalu(OP_code(o)))
#define OP_select(o)		(TOP_is_select(OP_code(o)))
#define OP_cond_move(o)		(TOP_is_cond_move(OP_code(o)))
#if defined(TARG_X8664)
#define OP_uniq_res(o)		(0)
#else
#define OP_uniq_res(o)		(TOP_is_uniq_res(OP_code(o)))
#endif
#define OP_unalign_ld(o)	(TOP_is_unalign_ld(OP_code(o)))
#define OP_unalign_store(o)	(TOP_is_unalign_store(OP_code(o)))
#define OP_unalign_mem(o)	(OP_unalign_ld(o) | OP_unalign_store(o))
#define OP_defs_fcc(o)		(TOP_is_defs_fcc(OP_code(o)))
#define OP_defs_fcr(o)		(TOP_is_defs_fcr(OP_code(o)))
#define OP_defs_fpu_int(o)	(TOP_is_defs_fpu_int(OP_code(o)))
#define OP_defs_fp(o)		(TOP_is_defs_fp(OP_code(o)))
#define OP_refs_fcr(o)		(TOP_is_refs_fcr(OP_code(o)))
#define OP_unsafe(o)            (TOP_is_unsafe(OP_code(o)))
#define OP_jump(o)		(TOP_is_jump(OP_code(o)))
#define OP_ijump(o)		(TOP_is_ijump(OP_code(o)))
#define OP_f_group(o)           (TOP_is_f_group(OP_code(o)))
#define OP_l_group(o)           (TOP_is_l_group(OP_code(o)))
#define OP_privileged(o)        (TOP_is_privileged(OP_code(o)))
#define OP_simulated(o)		(TOP_is_simulated(OP_code(o)))
#if defined(TARG_X8664)
#define OP_has_predicate(o)	(0)
#define OP_access_reg_bank(o)	(0)
#define OP_branch_predict(o)	(0)
#else
#define OP_has_predicate(o)	(TOP_is_predicated(OP_code(o)))
#define OP_access_reg_bank(o)	(TOP_is_access_reg_bank(OP_code(o)))
#define OP_branch_predict(o)	(TOP_is_branch_predict(OP_code(o)))
#endif
#define OP_side_effects(o)	(TOP_is_side_effects(OP_code(o)))
#define OP_var_opnds(o)		(TOP_is_var_opnds(OP_code(o)))
#define OP_uncond(o)            (OP_xfer(o) && !OP_cond(o))
#ifdef TARG_X8664
#define OP_x86_style(o)	        (TOP_is_x86_style(OP_code(o)))
#define OP_reads_rflags(o)      (TOP_is_read_rflags(OP_code(o)))
#define OP_x87(o)		(TOP_is_x87(OP_code(o)))
#define OP_mmx(o)		(TOP_is_mmx(OP_code(o)))
#endif

#if defined(TARG_SL) //fork_joint
#define OP_fork(o)              (TOP_is_fork_instr(OP_code(o)))
#define OP_vbuf_load(o)         (TOP_is_c2_v_load(OP_code(o)))
#define OP_sbuf_load(o)         (TOP_is_c2_s_load(OP_code(o)))
#define OP_vbuf_store(o)        (TOP_is_c2_v_store(OP_code(o)))
#define OP_sbuf_store(o)        (TOP_is_c2_s_store(OP_code(o)))
#define OP_has_bypass(o)        (TOP_is_c2_has_bypass(OP_code(o)))
#define OP_c2_load(o)           (TOP_is_c2_load(OP_code(o)))
#define OP_c2_store(o)          (TOP_is_c2_store(OP_code(o)))

#define OP_c3_load(o)	        (TOP_is_c3_load(OP_code(o)))
#define OP_c3_store(o)          (TOP_is_c3_store(OP_code(o)))

#define OP_sl_special_ldst(o) (OP_c3_load(o) | OP_c3_store(o))
#endif 

#define OP_operand_info(o)	(ISA_OPERAND_Info(OP_code(o)))
#define OP_has_hazard(o)	(ISA_HAZARD_TOP_Has_Hazard(OP_code(o)))
#define OP_immediate_opnd(o)	(TOP_Immediate_Operand(OP_code(o),NULL))
#define OP_has_immediate(o)	(OP_immediate_opnd(o) >= 0)
#define OP_inst_words(o)	(ISA_PACK_Inst_Words(OP_code(o)))
#define OP_find_opnd_use(o,u)	(TOP_Find_Operand_Use(OP_code(o),(u)))

#ifdef TARG_SL
#define OP_memtrap(o)           (TOP_is_memtrap(OP_code(o))) /* memory operation*/
#define OP_no_peephole(o)       (TOP_is_npeep(OP_code(o)))
#endif

#ifdef TARG_LOONGSON
#define OP_unsigned(o)          (TOP_is_unsigned_ext(OP_code(o)))
#endif

#ifdef TARG_IA64
// bug fix for OSP_87 and OSP_88
#define OP_asm(o)              (OP_code(o)==TOP_asm)

inline BOOL OP_float_chk(OP *op)
{
    mTOP opcode = OP_code(op);
    switch (opcode) {
    case TOP_chk_f_a:
    case TOP_chk_f_s:
      return TRUE;
    default:
      return FALSE;
    }
}

inline BOOL OP_chk_a(OP *op)
{
    mTOP opcode = OP_code(op);
    switch (opcode) {
    case TOP_chk_a:
    case TOP_chk_f_a:
      return TRUE;
    default:
      return FALSE;
    }	
}
   
inline BOOL OP_chk_s(OP *op)
{
    mTOP opcode = OP_code(op);
    switch (opcode) {
    case TOP_chk_s:
    case TOP_chk_s_i:
    case TOP_chk_s_m:
    case TOP_chk_f_s:
      return TRUE;
    default:
      return FALSE;
    }	
}

inline BOOL OP_chk(OP *op)
{
    return ( OP_chk_s(op) ||
	     OP_chk_a(op) );
}

inline BOOL OP_cmp_unc(OP *op)
{
	mTOP opcode = OP_code(op);
	switch (opcode) {
	  case TOP_cmp_eq_unc:
	  case TOP_cmp_lt_unc:
	  case TOP_cmp_ltu_unc:
	  case TOP_cmp_i_eq_unc:
	  case TOP_cmp_i_lt_unc:
	  case TOP_cmp_i_ltu_unc:
	  case TOP_cmp4_eq_unc:
	  case TOP_cmp4_lt_unc:
	  case TOP_cmp4_ltu_unc:
	  case TOP_cmp4_i_eq_unc:
	  case TOP_cmp4_i_lt_unc:
	  case TOP_cmp4_i_ltu_unc:
	  case TOP_fclass_m_unc:
	  case TOP_fcmp_eq_unc:
	  case TOP_fcmp_lt_unc:
	  case TOP_fcmp_le_unc:
	  case TOP_fcmp_unord_unc:
	  case TOP_tbit_z_unc:
	  case TOP_tnat_z_unc:
	  case TOP_cmp_ne_unc:
	  case TOP_cmp_le_unc:
	  case TOP_cmp_gt_unc:
	  case TOP_cmp_ge_unc:
	  case TOP_cmp_leu_unc:
	  case TOP_cmp_gtu_unc:
	  case TOP_cmp_geu_unc:
	  case TOP_cmp_i_ne_unc:
	  case TOP_cmp_i_le_unc:
	  case TOP_cmp_i_gt_unc:
	  case TOP_cmp_i_ge_unc:
	  case TOP_cmp_i_leu_unc:
	  case TOP_cmp_i_gtu_unc:
	  case TOP_cmp_i_geu_unc:
	  case TOP_cmp4_ne_unc:
	  case TOP_cmp4_le_unc:
	  case TOP_cmp4_gt_unc:
	  case TOP_cmp4_ge_unc:
	  case TOP_cmp4_leu_unc:
	  case TOP_cmp4_gtu_unc:
	  case TOP_cmp4_geu_unc:
	  case TOP_cmp4_i_ne_unc:
	  case TOP_cmp4_i_le_unc:
	  case TOP_cmp4_i_gt_unc:
	  case TOP_cmp4_i_ge_unc:
	  case TOP_cmp4_i_leu_unc:
	  case TOP_cmp4_i_gtu_unc:
	  case TOP_cmp4_i_geu_unc:
	  case TOP_fclass_nm_unc:
	  case TOP_fcmp_gt_unc:
	  case TOP_fcmp_ge_unc:
	  case TOP_fcmp_neq_unc:
	  case TOP_fcmp_nlt_unc:
	  case TOP_fcmp_nle_unc:
	  case TOP_fcmp_ngt_unc:
	  case TOP_fcmp_nge_unc:
	  case TOP_fcmp_ord_unc:
	  case TOP_tbit_nz_unc:
	  case TOP_tnat_nz_unc:
	  	return TRUE;
	  default:
	  	return FALSE;
	}
}
#endif

inline INT OP_result_size(OP *op, INT result)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Result(oinfo, result);
  return ISA_OPERAND_VALTYP_Size(otype);
}

inline INT OP_opnd_size(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Size(otype);
}

inline BOOL OP_result_is_reg(OP *op, INT result)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Result(oinfo, result);
  return ISA_OPERAND_VALTYP_Is_Register(otype);
}

inline BOOL OP_opnd_is_reg(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Is_Register(otype);
}

inline BOOL OP_opnd_is_literal(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Is_Literal(otype);
}

inline BOOL OP_opnd_is_enum(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Is_Enum(otype);
}

inline BOOL OP_result_is_signed(OP *op, INT result)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Result(oinfo, result);
  return ISA_OPERAND_VALTYP_Is_Signed(otype);
}

inline BOOL OP_opnd_is_signed(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Is_Signed(otype);
}

inline BOOL OP_result_is_fpu_int(OP *op, INT result)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Result(oinfo, result);
  return ISA_OPERAND_VALTYP_Is_FPU_Int(otype);
}

inline BOOL OP_opnd_is_fpu_int(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Is_FPU_Int(otype);
}

inline BOOL OP_opnd_is_pcrel(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Is_PCRel(otype);
}

inline ISA_LIT_CLASS OP_opnd_lit_class(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Literal_Class(otype);
}

inline ISA_REGISTER_CLASS OP_opnd_reg_class(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Register_Class(otype);
}

inline ISA_REGISTER_CLASS OP_result_reg_class(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Result(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Register_Class(otype);
}

inline ISA_REGISTER_SUBCLASS OP_opnd_reg_subclass(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Register_Subclass(otype);
}

inline ISA_REGISTER_SUBCLASS OP_result_reg_subclass(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Result(oinfo, opnd);
  return ISA_OPERAND_VALTYP_Register_Subclass(otype);
}

inline ISA_OPERAND_USE OP_opnd_use(OP *op, INT opnd)
{
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));
  return ISA_OPERAND_INFO_Use(oinfo, opnd);
}

#define OP_has_result(o) (OP_results(o) != 0)

/* Is OP a branch ? */
#define  OP_br(o) (OP_xfer(o) && !OP_call(o))

/* _fixed_results and _fixed_opnds return how many fixed
 * results/operands an instruction has (OP_result/OP_opnds includes
 * any variable operands in the count).
 */
#define TOP_fixed_results(o)	(ISA_OPERAND_INFO_Results(ISA_OPERAND_Info(o)))
#define TOP_fixed_opnds(o)	(ISA_OPERAND_INFO_Operands(ISA_OPERAND_Info(o)))
#define OP_fixed_results(o)	(TOP_fixed_results(OP_code(o)))
#define OP_fixed_opnds(o)	(TOP_fixed_opnds(OP_code(o)))

/* ---------------------------------------------------------------------
 *			       OPS stuff
 * ---------------------------------------------------------------------
 */

typedef struct ops {
  struct op *first;
  struct op *last;
  UINT16 length;
} OPS;

#define OPS_first(ops) ((ops)->first+0)
#define OPS_last(ops) ((ops)->last+0)
#define OPS_length(ops) ((ops)->length+0)
#define OPS_EMPTY { NULL, NULL, 0 }

inline OPS *OPS_Create(void)
{
  OPS *result = TYPE_MEM_POOL_ALLOC(OPS, &MEM_pu_pool);
  result->first = result->last = NULL;
  result->length = 0;
  return result;
}

inline void OPS_Remove_All(OPS *ops)
{
  ops->first = ops->last = NULL;
  ops->length = 0;
}
  
inline OPS *OPS_Init(OPS *ops)
{
  ops->first = ops->last = NULL;
  ops->length = 0;
  return ops;
}

inline void OPS_Remove_Op(OPS *ops, OP *op)
{
  OP **prevp = OP_next(op) ? &OP_next(op)->prev : &ops->last;
  OP **nextp = OP_prev(op) ? &OP_prev(op)->next : &ops->first;

#if defined(TARG_NVISA)
  FmtAssert(OP_prev(op) || OP_next(op) || ops->first == op, 
	("OPS_Remove_Op: op not in ops"));
#endif
  
  *prevp = OP_prev(op);
  *nextp = OP_next(op);

  op->prev = op->next = NULL;
  ops->length--;
}

inline void OPS_Remove_Ops(OPS *ops, OPS *remove_ops)
{
  OP *first = OPS_first(remove_ops);
  OP *last = OPS_last(remove_ops);
  OP **prevp, **nextp;

  if (first == NULL) return;

  FmtAssert(last != NULL,
	    ("remove_ops is malformed (last is NULL but first isn't)"));

  prevp = OP_next(last) ? &OP_next(last)->prev : &ops->last;
  nextp = OP_prev(first) ? &OP_prev(first)->next : &ops->first;
  
  *prevp = OP_prev(first);
  *nextp = OP_next(last);

  first->prev = last->next = NULL;
  ops->length -= OPS_length(remove_ops);
}

#ifdef TARG_X8664
void Init_LegacySSE_To_Vex_Group(void);
#endif

void OPS_Insert_Op_Before(OPS *ops, OP *point, OP *op);
void OPS_Insert_Op_After(OPS *ops, OP *point, OP *op);
void OPS_Append_Op(OPS *ops, OP *op);
void OPS_Prepend_Op(OPS *ops, OP *op);
void OPS_Insert_Ops_Before(OPS *ops, OP *point, OPS *insert_ops);
void OPS_Insert_Ops_After(OPS *ops, OP *point, OPS *insert_ops);
void OPS_Append_Ops(OPS *ops, OPS *insert_ops);
void OPS_Prepend_Ops(OPS *ops, OPS *insert_ops);
void OPS_Insert_Op(OPS *ops, OP *point, OP *op, BOOL before);
void OPS_Insert_Ops(OPS *ops, OP *point, OPS *insert_ops, BOOL before);

/* ====================================================================
 *
 * Utility routines which manipulate OPs.
 *
 * ====================================================================
 */

/* Basic OP generators: */
extern OP *Dup_OP ( OP *op );

extern void Free_OP_List ( OP *op );	/* Free OP list's space */

/* OP generation utilities: */
extern OP *Mk_OP (
  TOP opr,		/* operator */
  ...			/* the results and operands */
);

extern OP *Mk_VarOP (
  TOP opr,		/* operator */
  INT results,		/* number of results */
  INT opnds,		/* number of operands */
  struct tn **res_tn,	/* result array */
  struct tn **opnd_tn	/* operand array */
);

/* Build OP and append to ops list: */

inline void Build_OP(TOP opc, OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc));
}

inline void Build_OP(TOP opc, struct tn *t1, OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3, 
			      OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3, 
			      struct tn *t4, OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3, 
			      struct tn *t4, struct tn *t5, OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3, 
			      struct tn *t4, struct tn *t5, struct tn *t6, 
			      OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5, t6));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3, 
			      struct tn *t4, struct tn *t5, struct tn *t6, 
			      struct tn *t7, OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5, t6, t7));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3, 
			      struct tn *t4, struct tn *t5, struct tn *t6, 
			      struct tn *t7, struct tn *t8, OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5, t6, t7, t8));
}
inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3, 
			      struct tn *t4, struct tn *t5, struct tn *t6, 
			      struct tn *t7, struct tn *t8, struct tn *t9, 
			      OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5, t6, t7, t8, t9));
}
#if defined(TARG_SL)
inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3, 
			      struct tn *t4, struct tn *t5, struct tn *t6, 
			      struct tn *t7, struct tn *t8, struct tn *t9, struct tn *t10,
			      OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5, t6, t7, t8, t9,t10));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3,
                              struct tn *t4, struct tn *t5, struct tn *t6,
                              struct tn *t7, struct tn *t8, struct tn *t9, 
		              struct tn *t10, struct tn *t11,
                              OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3,
                              struct tn *t4, struct tn *t5, struct tn *t6,
                              struct tn *t7, struct tn *t8, struct tn *t9,
                              struct tn *t10, struct tn *t11, struct tn *t12,
                              OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5, t6, t7, t8, t9,t10,t11, t12));
}

inline void Build_OP(TOP opc, struct tn *t1, struct tn *t2, struct tn *t3,
                              struct tn *t4, struct tn *t5, struct tn *t6,
                              struct tn *t7, struct tn *t8, struct tn *t9,
                              struct tn *t10, struct tn *t11, struct tn *t12,
                              struct tn *t13,
                              OPS *ops)
{
  OPS_Append_Op(ops, Mk_OP(opc, t1, t2, t3, t4, t5, t6, t7, t8, t9,t10,t11, t12, t13));
}
#endif
/* Determine if the op defines/references the given TN result/operand. */
extern BOOL OP_Defs_TN(const OP *op, const struct tn *res);
extern BOOL OP_Refs_TN(const OP *op, const struct tn *opnd);

/* Determine if the op defines/references the given register. */
extern BOOL OP_Defs_Reg(const OP *op, ISA_REGISTER_CLASS rclass, REGISTER reg);
extern BOOL OP_Refs_Reg(const OP *op, ISA_REGISTER_CLASS rclass, REGISTER reg);

#ifdef TARG_X8664
inline void OP_Change_Aux_Opcode(OP *op, mUINT16 aux_opc, mUINT8 idx)
{
  op->aux_info.aux_data.aux_opr = aux_opc;
  op->aux_info.aux_data.aux_idx = idx;
}
#endif

inline void OP_Change_Opcode(OP *op, TOP opc)
{
  Is_True(   OP_fixed_results(op) == TOP_fixed_results(opc)
	  && OP_fixed_opnds(op) == TOP_fixed_opnds(opc),
	  ("different number of results/operands for %s and %s",
	   TOP_Name(OP_code(op)), TOP_Name(opc)));
  Is_True(OP_var_opnds(op) == TOP_is_var_opnds(opc),
	  ("var_opnds property not same for %s and %s",
	   TOP_Name(OP_code(op)), TOP_Name(opc)));

  op->opr = (mTOP)opc;
}

inline void OP_Change_To_Noop(OP *op)
{
  op->opr = (mTOP)TOP_noop;
  op->opnds = 0;
  op->results = 0;
  OP_flags(op) = 0;
}

inline BOOL OP_Precedes(OP *op1, OP *op2)
{
  return op1->order < op2->order;
}

inline BOOL OP_Follows(OP *op1, OP *op2)
{
  return op1->order > op2->order;
}

inline INT64 OP_Ordering(OP *op1, OP *op2)
{
  return (INT64)op1->order - (INT64)op2->order;
}

inline INT32 OP_Order(OP *op1, OP *op2)
{
  return -(op1->order < op2->order) | (op2->order < op1->order);
}


/* Count the number of real ops/instruction-words emitted by the op.
 */
extern INT16 OP_Real_Ops( const OP *op );
extern INT OP_Real_Inst_Words( const OP *op );


/* ======================================================================
 * Macros to traverse the OPs within a basic block. Use of these macros
 * allows us to change the underlying representation of the list OPs
 * without affecting most users.
 * ======================================================================*/

#define FOR_ALL_BB_OPs_FWD(bb,op) \
    for (op = BB_first_op(bb); op != NULL; op = OP_next(op))

#define FOR_ALL_BB_OPs_REV(bb,op) \
    for (op = BB_last_op(bb); op != NULL; op = OP_prev(op))

#define FOR_ALL_BB_OPs 	FOR_ALL_BB_OPs_FWD

#define FOR_ALL_OPS_OPs_FWD(ops,op) \
    for (op = OPS_first(ops); op && op != OP_next(OPS_last(ops)); \
	 op = OP_next(op))

#define FOR_ALL_OPS_OPs_REV(ops,op) \
    for (op = OPS_last(ops); op && op != OP_prev(OPS_first(ops)); \
	 op = OP_prev(op))

#define FOR_ALL_OPS_OPs FOR_ALL_OPS_OPs_FWD

void Print_OP ( const OP *op );
#pragma mips_frequency_hint NEVER Print_OP
void Print_OPs( const OP *op );
#pragma mips_frequency_hint NEVER Print_OPs
void Print_OPS( const OPS *ops);
#pragma mips_frequency_hint NEVER Print_OPS
void Print_OP_No_SrcLine( const OP *op);
#pragma mips_frequency_hint NEVER Print_OP_No_SrcLine
void Print_OPs_No_SrcLines( const OP *op );
#pragma mips_frequency_hint NEVER Print_OPs_No_SrcLines
void Print_OPS_No_SrcLines( const OPS *ops);
#pragma mips_frequency_hint NEVER Print_OPS_No_SrcLines

BOOL OP_Is_Float_Mem(const OP *op);
BOOL OP_Alloca_Barrier(OP *op);
BOOL Is_Delay_Slot_Op (OP *xfer_op, OP *op);

extern void OP_Base_Offset_TNs(OP *memop, struct tn **base_tn, struct tn **offset_tn);

#ifdef KEY
extern BOOL TN_Pair_In_OP(OP* op, struct tn *tn_res, struct tn *tn_opnd);
#else
/***********************************************************************
 *
 *      Return a boolean to indicate if <tn> is both an operand and a
 *      result in the given <op>, i.e. is same_res.
 *
 ***********************************************************************/

inline BOOL 
TN_Pair_In_OP(OP* op, struct tn *tn_res, struct tn *tn_opnd) 
{
  INT i;
  for (i = 0; i < OP_results(op); i++) {
    if (tn_res == OP_result(op,i)) {
      break; 
    }
  }
  if (i == OP_results(op)) {
    return FALSE;
  }
  for (i = 0; i < OP_opnds(op); i++) {
    if (tn_opnd == OP_opnd(op,i)) {
      return TRUE; 
    }
  }
  return FALSE;
}
#endif	// KEY

#ifdef KEY
INT TN_Resnum_In_OP (OP* op, struct tn *tn, BOOL match_assigned_reg = FALSE); 
#else
/***********************************************************************
 *
 *      This routine assumes that the given <tn> is a result in <op>.
 *      It returns an INT that is the value of the result-number for
 *      this <tn> in this <op>.
 *
 ***********************************************************************/

inline INT 
TN_Resnum_In_OP (OP* op, struct tn *tn) 
{
  for (INT i = 0; i < OP_results(op); i++) {
    if (tn == OP_result(op,i)) {
      return i;
    }
  }
  FmtAssert (FALSE,
             ("TN_resnum_in_OP: Could not find <tn> in results list\n"));
  return -1;
}
#endif	// KEY

/***********************************************************************
 *
 *      This routine assumes that the given <tn> is an operand in <op>.
 *      It returns an INT that is the value of the operand-number for
 *      this <tn> in this <op>.
 *
 ***********************************************************************/

inline INT 
TN_Opernum_In_OP (OP* op, struct tn *tn) 
{
  for (INT i = 0; i < OP_opnds(op); i++) {
    if (tn == OP_opnd(op, i)) {
      return i;
    }
  }
  FmtAssert (FALSE,
             ("TN_Opernum_in_OP: Could not find <tn> in operands list\n"));
  return -1;
}

#ifdef TARG_IA64
inline void
Remove_Hidden_Operands (OP* op) {
  if (op->hidden_opnds == 0) return;

  // remove the results to upward so that there is 
  // no gap between operands and resutls 
  INT from_idx, to_idx;
  from_idx = op->opnds;

  op->opnds -= op->hidden_opnds;
  to_idx = op->opnds;

  for (INT i = 0; i < op->results; i++) {
    op->res_opnd[to_idx++] = op->res_opnd[from_idx++];
  }

  op->hidden_opnds = 0;
}

void Add_Hidden_Operands (OP* op, const vector<struct tn*> & opnds);

inline BOOL Is_Hidden_Opnd (OP* op, UINT8 opnd) {
  return OP_hidden_opnds(op) != 0 && opnd < OP_opnds(op) &&
    opnd >= (OP_opnds(op) - OP_hidden_opnds(op));
}
#endif

/* Is <op> a copy from a callee-saves register into its save-TN?  */
BOOL OP_Is_Copy_To_Save_TN (const OP*);

/*  Is <op> a copy to a callee-saves register from its save-TN?  */
BOOL OP_Is_Copy_From_Save_TN (const OP* );



#include "op_targ.h"

#endif /* op_INCLUDED */

