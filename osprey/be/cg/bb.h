/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: bb.h
 * $Revision: 1.7 $
 * $Date: 05/12/05 08:59:02-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.bb.h $
 *
 * Description:
 *
 * Definitions for the principal CGIR basic block (BB) structure, with
 * related global variable declarations	and utility routine prototypes.
 *
 * Constants:
 *
 * Utilities:
 *
 *   BB *Gen_BB(void)
 *     Return a new empty BB, not attached to anything else.
 *
 *   BB *Gen_BB_N(INT n)
 *     Create vector of <n> new empty BBs, not attached to anything else.
 *     Return pointer to first element in vector.
 *
 *   BB *Gen_BB_Like (BB *model)
 *     Return a new empty BB, with rid like the model bb. If <model>
 *     is NULL, then REGION_First_BB is used at a model. If that is NULL,
 *     then the behavior is exactly like Gen_BB.
 *
 *   BB *Gen_And_Append_BB(BB *prev_bb)
 *     Requires: BB_next(prev_bb) == NULL
 *     Create a new empty BB, append it after <prev_bb>, and return it.
 *     The RID is determined by using <prev_bb> as a model for Gen_BB_Like.
 *
 *   BB *Gen_And_Insert_BB_After(BB *point)
 *   BB *Gen_And_Insert_BB_Before(BB *point)
 *     Create a new empty BB, append it beofore or after <point>. 
 *     The RID is determined by using <point> as a model for Gen_BB_Like.
 *
 *   void Chain_BBs(BB* bb1, BB* bb2)
 *     Chain <bb1> and <bb2> together through their _next and _prev
 *     fields.
 *
 *   void Target_Simple_Fall_Through_BB(BB* bb, BB* target_bb)
 *     Make <bb> be a simple fall-through to <target_bb>.  <bb> must
 *     a GOTO BB.
 *
 *   void Target_Logif_BB(BB* bb, BB* br_targ_bb, float br_targ_prob,
 *				  BB* fall_through_bb)
 *     <bb> is a LOGIF BB.  Make it branch to <br_targ_bb> with probability
 *     <br_targ_prob> and fall through to <fall_through_bb>.  Handles the 
 *     _succs, _preds fields of the 3 blocks and makes the fall through 
 *     directly follow <bb> (by adjusting the _next,_prev fields.)  It is 
 *     the responsibility of the caller to make sure that the _succ, _preds 
 *     fields do not contain any (no invalid) vestiges of previous flow.  
 *     This means that the _succs of <bb> should be initialized but empty 
 *     and that the _preds should only contain the other BBs that branch 
 *     to them.
 *
 *   void Target_Cond_Branch(BB* bb, BB* br_targ_bb, float br_targ_prob)
 *	Same as above, but does nothing with fall thru, nor does it set
 *	probabilities.
 *
 *   void Negate_Logif_BB(BB *bb)
 *	Negate the sense of the final branch in the given LOGIF <bb>.
 *
 *   void Add_Goto(BB *bb, BB *target_bb)
 *     Make <bb> goto <target_bb>.  <bb> may not already have a branch
 *     instruction. Properly maintains _succs of <bb> and _preds of
 *     <target_bb>.
 *
 *   BB* Create_Dummy_BB( BB *dest_bb )
 *     Allocate, initialize, and return a new empty fall through BB that
 *     falls through to <dest_bb>.
 *
 *   UINT16 BB_New_Op_Map_Idx(BB *bb)
 *     Return an index that is distinct from the map_idx fields of
 *     all other OPs in <bb>.
 *
 *   UINT16 BB_length(BB *bb)
 *     Return the number of OPs in <bb>.
 *
 *   BB *BB_loop_head_bb(BB *bb)
 *     Returns nil if <bb> is not part of any loop.  Otherwise, returns
 *     the head of the closest containing natural loop.
 *
 *   void Set_BB_loop_head_bb(BB *bb, BB *head)
 *     Used by the loop finder (see "findloops.h") to indicate <bb> is
 *     part of the natural loop headed by <head>.
 *
 *   UINT16 BB_unrollings(BB *bb)
 *     Returns the number of times <bb> has been unrolled.
 *
 *   BOOL BB_unrolled_fully(BB *bb)
 *     Returns TRUE iff <bb> is part of a fully-unrolled loop.
 *
 *   void Set_BB_unrollings(BB *bb, UINT16 u)
 *     Used by loop unroller (see "cg_loop.c") to indicate <bb> has
 *     been unrolled <u> times.
 *
 *   void Set_BB_unrolled_fully(BB *bb)
 *   void Reset_BB_unrolled_fully(BB *bb)
 *     Used by loop unroller (see "cg_loop.c") to indicate <bb> is
 *     part of a fully unrolled loop.  (Reset provided in case it's
 *     needed someday.)
 *
 *   void BB_Insert_Op(BB *bb, OP *point, OP *op, BOOL before)
 *     Requires: OP_bb(point) == bb.
 *     Insert <op> in <bb>.  If <point> isn't NULL, insert before
 *     <point> if <before> or after it otherwise.  If <point> is NULL
 *     insert at start of <bb> if <before>, or end of <bb> otherwise.
 *     Sets OP_bb(op) to <bb> and OP_map_idx(op) to a value unique in
 *     <bb>.  Implemented in "oputil.c".
 *
 *   void BB_Insert_Ops(BB *bb, OP *point, OPS *ops, BOOL before)
 *     Requires: OP_bb(point) == bb.
 *     Insert <ops> in <bb>.  If <point> isn't NULL, insert before
 *     <point> if <before> or after it otherwise.  If <point> is NULL
 *     insert at start of <bb> if <before>, or end of <bb> otherwise.
 *     For each OP <op> in <ops>, sets OP_bb(op) to <bb> and
 *     OP_map_idx(op) to a value unique in <bb>.  Implemented in
 *     "oputil.c".
 *
 *   void BB_Insert_Op_Before(BB *bb, OP *point, OP *op)
 *   void BB_Insert_Op_After(BB *bb, OP *point, OP *op)
 *   void BB_Append_Op(BB *bb, OP *op)
 *   void BB_Prepend_Op(BB *bb, OP *op)
 *   void BB_Insert_Ops_Before(BB *bb, OP *point, OPS *insert_ops)
 *   void BB_Insert_Ops_After(BB *bb, OP *point, OPS *insert_ops)
 *   void BB_Append_Ops(BB *bb, OPS *insert_ops)
 *   void BB_Prepend_Ops(BB *bb, OPS *insert_ops)
 *     More-specific names for the general Insert routines.  These do
 *     what you think they should do.  Implemented in "oputil.c".
 *
 *   void BB_Insert_Noops(OP *op, INT num, BOOL before)
 *     Insert <num> number of noops either before or after the <op>.
 *
 *   OP *BB_Remove_Branch(BB *bb)
 *     If <bb> has a branch, remove it (and delay slot nop if present).
 *     Return the branch OP or NULL (if no branch).
 *
 *   void BB_Remove_Op(BB *bb, OP *op)
 *     Requires: OP_bb(op) == bb.
 *     Remove <op> from <bb> and set OP_bb(op) to NULL.
 *     Implemented in "oputil.c".
 *
 *   void BB_Remove_Ops(BB *bb, OPS *ops)
 *     Requires: <ops> are in <bb>
 *     Remove <ops> from <bb> and set each OP_bb to NULL.
 *     Implemented in "oputil.c".
 *
 *   void BB_Remove_All(BB *bb)
 *     Remove all OPs from <bb> and set each OP_bb to NULL.
 *     Implemented in "oputil.c".
 *
 *   void BB_Append_All(BB *to_bb, BB *from_bb)
 *     Append all OPs from <from_bb> to <to_bb>.
 *     Implemented in "oputil.c".
 *
 *   void BB_Prepend_All(BB *to_bb, BB *from_bb)
 *     Prepend all OPs from <from_bb> to <to_bb>.
 *     Implemented in "oputil.c".
 *
 *   void BB_Move_Op(BB *to_bb, OP *point, BB *from_bb, OP *op, BOOL before)
 *   void BB_Move_Op_Before(BB *to_bb, OP *point, BB *from_bb, OP *op)
 *   void BB_Move_Op_After(BB *to_bb, OP *point, BB *from_bb, OP *op)
 *   void BB_Move_Op_To_Start(BB *to_bb, BB *from_bb, OP *op)
 *   void BB_Move_Op_To_End(BB *to_bb, BB *from_bb, OP *op)
 *     Requires: OP_bb(point) == to_bb && OP_bb(op) == from_bb
 *     Move <op> from <from_bb> to <to_bb>.  If <before> is TRUE, <op>
 *     is placed before <point> (or at start of <to_bb> if <point> is
 *     NULL); otherwise <op> is placed after <point> (or at end of
 *     <to_bb> if <point> is NULL).  The _Before, _After, _To_Start,
 *     and _To_End versions are simply abbreviations for the general
 *     form (but should be preferred when applicable since they're
 *     a little faster).  If <to_bb> and <from_bb> are the same, any
 *     local OP_MAP values are retained.  Implemented in "oputil.c".
 *
 *   void BB_Sink_Op_Before(BB *bb, OP *op, OP *point);
 *     Sink an OP before point.   The movement is within a BB,
 *     therefore data structure such as CG_DEP_GRAPH, and CG_LOOP
 *     are not destroyed.
 *
 *   BBLIST *BBlist_Find_BB(BBLIST *lst, BB *bb);
 *     Returns the BBLIST node in <lst> whose BBLIST_item is <bb>, or NULL
 *     if there is none.
 *
 *   BBLIST *BBlist_Fall_Thru_Succ(BB *bb);
 *     Returns a pointer to the BBLIST <node> in BB_succs(list) such that
 *     BBLIST_item(node) is the fall through control flow successor of <bb>,
 *     or NULL if there is none.		
 *
 *   BBLIST *BBlist_Fall_Thru_Pred(BB *bb);
 *     Returns a pointer to the BBLIST <node> in BB_preds(list) such that
 *     BBLIST_item(node) is the fall through control flow predecessor of <bb>,
 *     or NULL if there is none.		
 *
 *   BBLIST *BB_Find_Succ(BB *bb, BB *succ)
 *   BBLIST *BB_Find_Pred(BB *bb, BB *pred)
 *     Returns the BBLIST node in the successor/predecessor list of <bb>
 *	whose BBLIST_item is <succ>/<pred>, or NULL if there is none.
 *
 *   BOOL BB_in_succ(BB *bb, BB *b)
 *     Returns true is b is in BB succ list
 *
 *   BOOL BB_in_pred(BB *bb, BB *b)
 *     Returns true is b is in BB pred list
 *
 *   INT32 BB_succs_len(BB *bb)
 *   INT32 BB_preds_len(BB *bb)
 *     return the number of elements in the succs/preds list
 *
 *   BB *BB_Other_Successor(BB *bb, BB *succ)
 *   BB *BB_Other_Predecessor(BB *bb, BB *pred)
 *	<bb> has two different successors/predecessors. Return the 
 *	successor/predecessor that is not <succ>/<pred>.
 * 
 *   BB *BB_Unique_Successor( BB *bb );
 *   BB *BB_Unique_Predecessor( BB *bb );
 *   BB *BB_Unique_Source( BB *bb );
 *     If <bb> has a unique successor/predecessor/source, and it IS NOT <bb>,
 *     return it.  Otherwise return NULL.
 *
 *   BB *BB_Fall_Thru_Successor( BB *bb );
 *   BB *BB_Fall_Thru_Predecessor( BB *bb );
 *     Return the fall through control flow successor/predecessor of <bb>, 
 *     or NULL if there is none.
 *
 *   BOOL BB_Retarget_Branch(BB *bb, BB *from, BB *to)
 *     If <bb> branches to <from>, make it branch to <to> instead, updating
 *     pred/succ lists, frequency info and returning TRUE. Otherwise, return
 *     FALSE;
 *
 *   BOOL BB_Can_Retarget_Branch(BB *bb, BB *from)
 *     Predicate if BB_Retarget_Branch would succeed.
 *
 *   BOOL BB_Is_Unique_Successor( BB *bb, BB *succ );
 *   BOOL BB_Is_Unique_Predecessor( BB *bb, BB *pred );
 *     If succ/pred is the unique successor/predecessor, return TRUE
 *
 *   INT32 BBlist_Len(BBLIST *bblist)
 *     return the number of elements in the list
 *
 *   BOOL BBlist_Has_One_Element(BBLIST *bblist)
 *     Return a boolean that indicates iff 'bblist' contains one
 *     element. Less overhead than BBlist_Len.
 *
 *   BOOL BB_Has_One_Succ(BB *bb)
 *   BOOL BB_Has_One_Pred(BB *bb)
 *     Return a boolean that indicates if 'bb' has one succ/pred.
 *     Less overhead than BB_{succs,preds}_len.
 *
 *   BOOL BB_Move_Delay_Slot_Op (BB *bb)
 *     Check if the <bb> is terminated by an OP in the delay slot of an
 *     'xfer_op'. If yes, move the delay slot OP above the xfer_op. If
 *     the delay slot OP defines one of the operands of the xfer_op, copy
 *     that value into a temp before the delay slot OP and rename the 
 *     operand in the xfer_op.
 *
 *   void BB_Delete_Successors(BB *bb)
 *     Delete all of the successor from 'bb's _succ list, leaving an
 *     initialized empty list of successors.
 *
 *   void BB_Delete_Predecessors(BB *bb)
 *     Delete all of the predecessor from 'bb's _pred list, leaving an
 *     initialized empty list of predecessors
 *
 *   void BB_Mark_Unreachable_Blocks (void)
 *     Set the BB_unreachable(bb) attribute for all basic blocks that
 *     are unreachable in the current region. If some unreachable blocks
 *     are added later, this routine must be invoked again.
 *
 *   BOOL BB_Add_Ancestors(BB_SET **set, BB *bb, BB *start_bb, MEM_POOL *pool)
 *     Requires: <*set> is allocated from <pool>
 *     Def'n: The control-flow ancestors of <bb> are <bb> itself and all
 *	      BBs that precede <bb> on some path from an entry to <bb>.
 *     Find the control-flow ancestors of <bb> that aren't ancestors of
 *     BBs already in <*set> and add them to <*set>.  Returns TRUE if
 *     <start_bb> is visited.
 *
 *   LABEL_IDX Gen_Label_For_BB(BB *bb)
 *     Create a label for <bb>. If one already exists, don't create another.
 *
 *   BOOL Is_Label_For_BB(ST *label, BB *bb)
 *     Returns TRUE iff <label> is a label for <bb>.
 *
 *   BOOL BB_Has_Exc_Label(BB *bb)
 *     Returns TRUE iff <bb> has a label used to mark the beginning or
 *     end of an exception-handling region or exception handler.
 *
 *   BOOL BB_Has_Addr_Taken_Label(BB *bb)
 *     Returns TRUE if <bb> has a label whose address is taken, i.e. is
 *     flagged with LABEL_addr_saved().
 *
 *   BOOL BB_Has_Outer_Block_Label(BB *bb)
 *     Returns TRUE if <bb> has a label that is the target of a
 *     goto-outer-block, i.e. is flagged with LABEL_target_of_goto_outer_block()
 *
 *   BOOL Change_Switchtable_Entries(ST *tbl, BB *old_tgt, BB *new_tgt)
 *     Changes all references to <old_tgt> in switch table <tbl> to
 *     refer to <new_tgt> instead.  Returns TRUE iff any changes made.
 *
 *   BBKIND BB_kind(BB *bb)
 *     Return a BB's kind.
 *
 *   BOOL VALID_BBKIND(BBKIND k)
 *     Check a BBKIND for validity.
 *
 *   const char *BBKIND_Name(BBKIND k)
 *     Return a BBKIND as a string for printing.
 *
 *   void Print_LOOPINFO(LOOPINFO *info)
 *     Print out information associated with LOOPINFO annotations.
 *
 *   BB_MAP BB_Depth_First_Map(BB_SET *region, BB *entry)
 *     Requires: If (region != NULL), <entry> dominates all BBs in <region>
 *		 and there exists a subset of <region> such that all control
 *		 flow paths from <entry> to members of this subset include
 *		 exactly the BBs in <region>
 *     Return a BB_MAP representing a depth-first partial ordering
 *     of the CFG for the BBs in <region> starting with <entry>.
 *     <map> will contain zero for BBs unreachable from <entry>.
 *     Otherwise, <map> will contain contiguous numbers starting with
 *     1 (for <entry>).  If <region> is NULL, the current region or PU
 *     is used.  If <entry> is NULL, the current region entry (or PU
 *     entries) is used.
 *
 *   BB_MAP BB_Topological_Map(BB_SET *region, BB *entry)
 *     Requires: If (region != NULL), <entry> dominates all BBs in <region>
 *		 and there exists a subset of <region> such that all control
 *		 flow paths from <entry> to members of this subset include
 *		 exactly the BBs in <region>
 *     Return a BB_MAP representing a topological partial ordering
 *     of the CFG for the BBs in <region> starting with <entry>.
 *     <map> will contain zero for BBs whose predecessors can't be
 *     visited before the BB itself is visited.  Otherwise, <map> will
 *     contain contiguous numbers starting with 1 (for <entry>).  If
 *     <region> is NULL, the current region or PU is used.  If <entry>
 *     is NULL, the current region entry (or PU entries) is used.
 *
 *   SRCPOS BB_Loop_Srcpos(BB *bb);
 *   INT32 BB_Loop_Lineno(BB *bb);
 *     <bb> is the head of a loop; return the starting SRCPOS/line number
 *     for the loop if possible; return 0 otherwise.
 *
 *   void Change_Succ(BB *pred, BB *old_succ, BB *new_succ)
 *     Requires: <old_succ> in BB_succs(pred) && <pred> in BB_preds(old_succ)
 *     Change the pred/succ lists and frequency info (if present) to
 *     indicate that <pred> is now succeeded by <new_succ> instead of
 *     <old_succ>.
 *
 *   void Change_Succ_Prob(BB *pred, BB *succ, float prob)
 *     Requires: <succ> in BB_succs(pred) && <pred> in BB_preds(succ)
 *     Change the successor probability of the edge from <pred> to <succ>
 *     to <prob>, then update BB_freq(succ) accordingly.
 *
 *   void BB_Add_Annotation(BB *bb, ANNOTATION_KIND kind, void *info)
 *	Add an annotation of the given type 'kind' to the 'bb.
 *	'info' points to type-specific data for the annotation.
 *
 *   INT BB_Copy_Annotations(BB *to_bb, BB *from_bb, ANNOTATION_KIND kind)
 *	Copy the annotations of type 'kind' of 'from_bb' to 'to_bb'.
 *	NOTE: this only copies the pointer to the type-specific data
 *	(ANNOT_info); not its contents. The return value is the
 *	count of the annotations copied.
 *
 *   BOOL BB_Is_Cold(BB *bb)
 *	Return a boolean to indicate if a BB is part of a cold region
 *	or not.
 *
 *   ST *Gen_ST_For_BB(BB *bb)
 *	Generate a symbol (ST) to mark the start of the BB. Used in the
 *	rare cases when a label won't suffice (e.g. you need to generate
 * 	code to take it's address).
 *
 *   ST *BB_st(BB *bb)
 *	Return the symbol associated with the start of the BB, returning
 *	NULL if there is none.
 *
 * TODO: Complete this interface description.
 *
 * ====================================================================
 * ====================================================================
 */


#ifndef	bb_INCLUDED
#define	bb_INCLUDED

#include <vector>
#include "mempool_allocator.h"  /* to get mempool allocator */

#include "region_util.h" 	/* to get the definition of RID. */
#include "symtab.h" 		/* to get ST */

#include "srcpos.h" 		/* to get the definition of SRCPOS. */
#include "annotations.h" 	/* to get definition of ANNOTATION. */
#include "op.h" 		/* to get definition of OPS */
#include "bbregs.h" 		/* to get the definition of BBREGS */

/* Dummy definition to keep ANSI prototypes quiet: */
struct tn;
struct bblist;
class  WN;

/* ====================================================================
 *
 * The Basic Block (BB)
 *
 * ====================================================================
 */

/* Define the BB number type: */
typedef INT32 BB_NUM;	/* Individual objects */
typedef mINT32 mBB_NUM;	/* Table components */
#define BB_NUM_MAX	INT32_MAX

typedef	struct bb {
  struct bb	*next;		/* next	(sequential) bb		  */
  struct bb 	*prev;		/* previous (sequential) bb	  */
  struct bblist *preds;		/* list	of predecessor BBs	  */
  struct bblist *succs;		/* list	of successor BBs	  */
  OPS		ops;		/* list of OPs			  */
  mBB_NUM	id;		/* trace identification	number	  */
#if defined(TARG_IA64)
  UINT64	flags;	        /* flags			  */
#else
  UINT32	flags;	        /* flags			  */
#endif
  UINT16	nest_level;	/* loop	nesting	level		  */
  mUINT16	next_op_map_idx; /* next OP_map_idx to be assigned in BB  */
  mUINT16	unrollings;	/* (cached) number of unrollings */
  float  	freq;		/* profile or estimated	frequency */
  struct bb    *loop_head_bb;	/* BB of enclosing loop head (if known) */
  RID          *rid;            /* nearest enclosing region_id    */
  WN           *branch_wn;	/* terminating branch whirl node  */
  struct bbregs *bbregs;	/* auxiliary register info (bbregs.h) */
  struct annotation *annotations; /* annotations attached to bb   */
#ifdef KEY
  struct bb     *aux;
#endif 
#if defined(TARG_X8664)
  /* array of all target register classes used to supply pressure info */
  INT64		 offset;
  INT32          has_regpressure[ISA_REGISTER_CLASS_MAX+1]; 
#endif
#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS) || defined(TARG_LOONGSON)
  INT		bb_cycle; 
#if !defined(TARG_SL)
  mBB_NUM       id_before_profile;  /* old trace number before any profile process*/
#endif
#endif
} BB;

#ifndef	CAN_USE_BB
#define	CAN_USE_BB(a) (a)
#endif

/* Define the field access macros: b is	a *BB, y is an integer */

#define	BB_next(b)	(CAN_USE_BB(b)->next)
#define	BB_prev(x)	 (CAN_USE_BB(x)->prev)
#define	BB_preds(x)	 (CAN_USE_BB(x)->preds)
#define	BB_succs(x)	 (CAN_USE_BB(x)->succs)
#define BB_cycle(x)	 (CAN_USE_BB(x)->bb_cycle)
#define	BB_flag(b)	(CAN_USE_BB(b)->flags)
#define	BB_nest_level(b) (CAN_USE_BB(b)->nest_level)
#define	BB_rid(b)	(CAN_USE_BB(b)->rid)
#define BB_next_op_map_idx(b) (CAN_USE_BB(b)->next_op_map_idx)
#define	BB_freq(x)	 (CAN_USE_BB(x)->freq)
#define BB_branch_wn(x)	 (CAN_USE_BB(x)->branch_wn)
#define	BB_bbregs(x)	 (CAN_USE_BB(x)->bbregs)
#define BB_annotations(x) (CAN_USE_BB(x)->annotations)

/* rvalue field accessors */
#define	BB_id(b)	(CAN_USE_BB(b)->id+0)
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
#define BB_id_before_profile(b) (CAN_USE_BB(b)->id_before_profile+0)
#endif
#define	BB_first_op(b)	(CAN_USE_BB(b)->ops.first+0)
#define	BB_last_op(b)	(CAN_USE_BB(b)->ops.last+0)
#define BB_unrollings(b) (CAN_USE_BB(b)->unrollings+0)
#if defined(TARG_X8664)
#define BB_regpressure(b,rc) (CAN_USE_BB(b)->has_regpressure[rc])
#endif
#define BB_loop_head_bb(b) (CAN_USE_BB(b)->loop_head_bb+0)
#define BB_loophead(bb) (BB_loop_head_bb(bb) == (bb))
#ifdef KEY
#define BB_aux(b)       (CAN_USE_BB(b)->aux)
#define BB_offset(b)    (CAN_USE_BB(b)->offset)
#endif
/* mutators */
inline void Set_BB_unrollings(BB *bb, UINT16 u) {
  bb->unrollings = u;
}

#if defined(TARG_X8664)
inline void Set_BB_regpressure(BB *bb, INT32 x, ISA_REGISTER_CLASS cl) {
  bb->has_regpressure[cl] = x;
}
#endif

inline void Set_BB_loop_head_bb(BB *bb, BB *head) {
  bb->loop_head_bb = head;
}

#define	BBM_ENTRY	0x0001 /* BB is subprogram entry point */
#define BBM_HANDLER	0x0002 /* BB is handler entry point */
#define	BBM_EXIT	0x0004 /* BBKIND_RETURN, or call STOP (unmaintained) */
#define	BBM_CALL	0x0008 /* BB terminates with a call */
#define BBM_LABEL	0x0010 /* BB has labels */
#define BBM_PRAGMA	0x0020 /* BB has pragmas */
#define BBM_NOTE	0x0040 /* BB has one or more notes. */
#define	BBM_UNREACHABLE	0x0080 /* BB is unreachable (valid only right after a 
			   	     call to BB_Mark_Unreachable_Blocks) */
#define BBM_UNROLLED_FULLY 0x0100 /* BB is fully unrolled */
#define BBM_INNERMOST	0x0200 /* in an innermost loop */
#define	BBM_SCHEDULED	0x0400 /* BB has been scheduled by SWP/CFLOW/LOCS */
#define BBM_REG_ALLOC   0x0800 /* BB has been register allocated by SWP/LRA */
#define BBM_LOCAL_FLAG1	0x1000 /* local use flag */
#define BBM_FREQ_FB	0x2000 /* BB_freq is based on feedback */
#define BBM_GRA_SPILL	0x4000 /* BB used to aid GRA spill placement */
#define	BBM_SCHEDULED_HBS 0x8000 /* BB (or HB) has been scheduled by HBS */
#define BBM_ROTATING_KERNEL 0x10000 /* BB uses rotating registers */
#define BBM_MOD_ROTATING_REGISTERS 0x20000 /* BB modifies all rotating registers */
#define BBM_MOD_PRED_ROTATING_REGISTERS 0x40000 /* BB modifies predicate rotating registers */
#define BBM_ASM			0x00080000 /* BB has asm */
#define BBM_PREDICATE_PROMOTE	0x00100000 /* Predicate promotion phase has been
					    invoked for the BB. */
#define BBM_POST_LABEL          0x00200000 /* BB has a post-label, i.e. a 
					    label at the end of the BB, rather
					    than at the beginning */
#ifdef KEY
#define BBM_NON_LOCAL_LABEL     0x00400000 /* BB has a non-local label */
#endif

#if defined(TARG_X8664)
#define BBM_AFTER_PIC_ENTRY     0x00800000 /* BB is the original entry after PIC entry on IA-32 with -fPIC */
#define BBM_DISPATCH            0x01000000 /* BB has been dispatch scheduled */
#endif

#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS) || defined(TARG_LOONGSON)
#define BBM_EDGE_SPLITTING      0x00800000 /* BB is used for edge splitting */

#if defined(TARG_IA64) || defined(TARG_LOONGSON)
#define BBM_RECOVERY            0x01000000 /* BB is a recovery block */
#define BBM_CHK_SPLIT           0x02000000 /* BB splitted from another because of chk insertion */
#define BBM_EMITTED             0x04000000 /* BB has been emitted */
#define BBM_PROFILE_SPLITTED    0x08000000 /* BB is bb splitted from old bb by profile */
#define BBM_PROFILE_CHANGED     0x10000000 /* BB is changed by profile*/
#define BBM_PROFILE_ADDED       0x20000000 /* BB is new bb added by profile*/
#define BBM_CHK_SPLIT_HEAD      0x40000000 /* BB splitted from another because of chk insertion */
#define BBM_PARTIAL_BUNDLE      0x80000000 /* BB partial bundle for across boundary*/
#define BBM_CHK_SPLIT_TAIL     0x200000000 /* BB is splited tail *///bug fix for OSP_212
#elif defined(TARG_SL)
#define BBM_ZDL_PROLOG          0x01000000 
#define BBM_ZDL_BODY            0x02000000        
#define BBM_HAS_TAG          0x04000000        
#define BBM_SCHEDULED_SIZE      0x08000000          
#define BBM_FREQ_UNBALANCED	0x10000000
#endif // TARG_SL

#define BB_edge_splitting(x)  	      (BB_flag(x) & BBM_EDGE_SPLITTING)
#define Set_BB_edge_splitting(x)      (BB_flag(x) |= BBM_EDGE_SPLITTING)
#define Reset_BB_edge_splitting(x)    (BB_flag(x) &= ~BBM_EDGE_SPLITTING)
#endif // TARG_IA64 || TARG_SL

#if defined (TARG_SL)
/* BB is the prolog of  zero-delay-loop, so it  only contains 
 * add.i/mvtc/loop three instructios, LIS may add nops.
 */
#define BB_zdl_prolog(x)        (BB_flag(x) & BBM_ZDL_PROLOG)
#define Set_BB_zdl_prolog(x)    (BB_flag(x) |= BBM_ZDL_PROLOG)
#define Reset_BB_zdl_prolog(x)  (BB_flag(x) &= ~BBM_ZDL_PROLOG)

/* BB is the body of zero-delay-loop, so if it has branch inside,
 * it should be deleted ( i delay this to keep cfg not complain)
 */
#define BB_zdl_body(x)          (BB_flag(x) & BBM_ZDL_BODY)
#define Set_BB_zdl_body(x)      (BB_flag(x) |= BBM_ZDL_BODY)
#define Reset_BB_zdl_body(x)    (BB_flag(x) &= ~BBM_ZDL_BODY)

#define BB_has_tag(x)           (BB_flag(x) & BBM_HAS_TAG)
#define Set_BB_has_tag(x)       (BB_flag(x) |= BBM_HAS_TAG)
#define Reset_BB_has_tag(x)     (BB_flag(x) &=~ BBM_HAS_TAG)

#define BB_SCHED_SIZE(x)        (BB_flag(x) & BBM_SCHEDULED_SIZE)
#define Set_BB_sched_size(x)    (BB_flag(x) |= BBM_SCHEDULED_SIZE)
#define Reset_BB_sched_size(x)  (BB_flag(x) &= ~BBM_SCHEDULED_SIZE)

#define BB_freq_unbalanced(x)			(BB_flag(x) & BBM_FREQ_UNBALANCED)
#define Set_BB_freq_unbalanced(x)		(BB_flag(x) |= BBM_FREQ_UNBALANCED)
#define Reset_BB_freq_unbalanced(x)		(BB_flag(x) &= ~BBM_FREQ_UNBALANCED)
#endif // TARG_SL

#define	BB_entry(x)		(BB_flag(x) & BBM_ENTRY)
#define BB_handler(bb)		(BB_flag(bb) & BBM_HANDLER)
#define	BB_exit(x)		(BB_flag(x) & BBM_EXIT)
#define	BB_call(x)		(BB_flag(x) & BBM_CALL)
#define	BB_has_label(x)		(BB_flag(x) & BBM_LABEL)
#define	BB_has_pragma(x)	(BB_flag(x) & BBM_PRAGMA)
#define	BB_has_note(x)		(BB_flag(x) & BBM_NOTE)
#define	BB_unreachable(x)	(BB_flag(x) & BBM_UNREACHABLE)
#define	BB_unrolled_fully(x)	(BB_flag(x) & BBM_UNROLLED_FULLY)
#define BB_innermost(x)		(BB_flag(x) & BBM_INNERMOST)
#define BB_scheduled(bb)	(BB_flag(bb) & BBM_SCHEDULED)
#define BB_reg_alloc(bb)	(BB_flag(bb) & BBM_REG_ALLOC)
#define BB_local_flag1(bb)	(BB_flag(bb) & BBM_LOCAL_FLAG1)
#define BB_freq_fb_based(bb)	(BB_flag(bb) & BBM_FREQ_FB)
#define BB_gra_spill(bb)	(BB_flag(bb) & BBM_GRA_SPILL)
#define BB_scheduled_hbs(bb)	(BB_flag(bb) & BBM_SCHEDULED_HBS)
#define BB_rotating_kernel(bb)  (BB_flag(bb) & BBM_ROTATING_KERNEL)
#define BB_mod_rotating_registers(bb) (BB_flag(bb) & BBM_MOD_ROTATING_REGISTERS)
#define BB_mod_pred_rotating_registers(bb) (BB_flag(bb) & BBM_MOD_PRED_ROTATING_REGISTERS)
#define BB_asm(bb) 		(BB_flag(bb) & BBM_ASM)
#define BB_predicate_promote(bb) (BB_flag(bb) & BBM_PREDICATE_PROMOTE)
#define	BB_has_post_label(x)		(BB_flag(x) & BBM_POST_LABEL)
#ifdef KEY
#define	BB_has_non_local_label(x)	(BB_flag(x) & BBM_NON_LOCAL_LABEL)
#endif
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
#define BB_recovery(x)          (BB_flag(x) & BBM_RECOVERY)
#define BB_chk_split(x)         (BB_flag(x) & BBM_CHK_SPLIT)
#define BB_chk_split_head(x)    (BB_flag(x) & BBM_CHK_SPLIT_HEAD)
#define BB_emitted(x)           (BB_flag(x) & BBM_EMITTED)
#define BB_profile_splitted(x)    (BB_flag(x) & BBM_PROFILE_SPLITTED)
#define BB_profile_changed(x)    (BB_flag(x) & BBM_PROFILE_CHANGED)
#define BB_profile_added(x)    (BB_flag(x) & BBM_PROFILE_ADDED)
#define BB_partial_bundle(x)	(BB_flag(x) & BBM_PARTIAL_BUNDLE)
#define BB_chk_split_tail(x)    (BB_flag(x) & BBM_CHK_SPLIT_TAIL)//bug fix for OSP_212
#endif
#if defined(TARG_X8664)
#define BB_after_pic_entry(x)   (BB_flag(x) & BBM_AFTER_PIC_ENTRY)
#define BB_dispatch(x)          (BB_flag(x) & BBM_DISPATCH)
#endif

#define	Set_BB_entry(x)		(BB_flag(x) |= BBM_ENTRY)
#define Set_BB_handler(bb)	(BB_flag(bb) |= BBM_HANDLER)
#define	Set_BB_exit(x)		(BB_flag(x) |= BBM_EXIT)
#define	Set_BB_call(x)		(BB_flag(x) |= BBM_CALL)
#define	Set_BB_has_label(x)	(BB_flag(x) |= BBM_LABEL)
#define	Set_BB_has_pragma(x)	(BB_flag(x) |= BBM_PRAGMA)
#define	Set_BB_has_note(x)	(BB_flag(x) |= BBM_NOTE)
#define	Set_BB_unreachable(x)	(BB_flag(x) |= BBM_UNREACHABLE)
#define	Set_BB_unrolled_fully(x)(BB_flag(x) |= BBM_UNROLLED_FULLY)
#define	Set_BB_innermost(x)	(BB_flag(x) |= BBM_INNERMOST)
#define Set_BB_scheduled(bb)	(BB_flag(bb) |= BBM_SCHEDULED)
#define Set_BB_reg_alloc(bb)	(BB_flag(bb) |= BBM_REG_ALLOC)
#define Set_BB_local_flag1(bb)	(BB_flag(bb) |= BBM_LOCAL_FLAG1)
#define Set_BB_freq_fb_based(bb)(BB_flag(bb) |= BBM_FREQ_FB)
#define Set_BB_gra_spill(bb)	(BB_flag(bb) |= BBM_GRA_SPILL)
#define Set_BB_scheduled_hbs(bb)(BB_flag(bb) |= BBM_SCHEDULED_HBS)
#define Set_BB_rotating_kernel(bb) (BB_flag(bb) |= BBM_ROTATING_KERNEL)
#define Set_BB_mod_rotating_registers(bb) (BB_flag(bb) |= BBM_MOD_ROTATING_REGISTERS)
#define Set_BB_mod_pred_rotating_registers(bb) (BB_flag(bb) |= BBM_MOD_PRED_ROTATING_REGISTERS)
#define Set_BB_asm(bb) 		(BB_flag(bb) |= BBM_ASM)
#define Set_BB_predicate_promote(bb) 	(BB_flag(bb) |= BBM_PREDICATE_PROMOTE)
#define	Set_BB_has_post_label(x)	(BB_flag(x) |= BBM_POST_LABEL)
#ifdef KEY
#define	Set_BB_has_non_local_label(x)	(BB_flag(x) |= BBM_NON_LOCAL_LABEL)
#endif

#if defined(TARG_IA64) || defined(TARG_LOONGSON)
#define Set_BB_recovery(x)          (BB_flag(x) |= BBM_RECOVERY)
#define Set_BB_chk_split(x)         (BB_flag(x) |= BBM_CHK_SPLIT)
#define Set_BB_chk_split_head(x)    (BB_flag(x) |= BBM_CHK_SPLIT_HEAD)
#define Set_BB_emitted(x)           (BB_flag(x) |= BBM_EMITTED)
#define Set_BB_profile_splitted(x)    (BB_flag(x) |= BBM_PROFILE_SPLITTED)
#define Set_BB_profile_changed(x)    (BB_flag(x) |= BBM_PROFILE_CHANGED)
#define Set_BB_profile_added(x)    (BB_flag(x) |= BBM_PROFILE_ADDED)
#define Set_BB_partial_bundle(x)    (BB_flag(x) |= BBM_PARTIAL_BUNDLE)
#define Set_BB_chk_split_tail(x)    (BB_flag(x) |= BBM_CHK_SPLIT_TAIL)//bug fix for OSP_212
#endif
#if defined(TARG_X8664)
#define Set_BB_after_pic_entry(x)   (BB_flag(x) |= BBM_AFTER_PIC_ENTRY)
#define Set_BB_dispatch(x)          (BB_flag(x) |= BBM_DISPATCH)
#endif

#define	Reset_BB_entry(x)	(BB_flag(x) &= ~BBM_ENTRY)
#define Reset_BB_handler(bb) 	(BB_flag(bb) &= ~BBM_HANDLER)
#define	Reset_BB_exit(x)	(BB_flag(x) &= ~BBM_EXIT)
#define	Reset_BB_call(x)	(BB_flag(x) &= ~BBM_CALL)
#define	Reset_BB_has_label(x)	(BB_flag(x) &= ~BBM_LABEL)
#define	Reset_BB_has_pragma(x)	(BB_flag(x) &= ~BBM_PRAGMA)
#define	Reset_BB_has_note(x)	(BB_flag(x) &= ~BBM_NOTE)
#define	Reset_BB_unreachable(x) (BB_flag(x) &=	~BBM_UNREACHABLE)
#define	Reset_BB_unrolled_fully(x) (BB_flag(x) &= ~BBM_UNROLLED_FULLY)
#define	Reset_BB_innermost(x)	(BB_flag(x) &= ~BBM_INNERMOST)
#define Reset_BB_scheduled(bb)	(BB_flag(bb) &= ~BBM_SCHEDULED)
#define Reset_BB_reg_alloc(bb) 	(BB_flag(bb) &= ~BBM_REG_ALLOC)
#define Reset_BB_local_flag1(bb) (BB_flag(bb) &= ~BBM_LOCAL_FLAG1)
#define Reset_BB_freq_fb_based(bb) (BB_flag(bb) &= ~BBM_FREQ_FB)
#define Reset_BB_gra_spill(bb)	(BB_flag(bb) &= ~BBM_GRA_SPILL)
#define Reset_BB_scheduled_hbs(bb) (BB_flag(bb) &= ~BBM_SCHEDULED_HBS)
#define Reset_BB_rotating_kernel(bb) (BB_flag(bb) &= ~BBM_ROTATING_KERNEL)
#define Reset_BB_mod_rotating_registers(bb) (BB_flag(bb) &= ~BBM_MOD_ROTATING_REGISTERS)
#define Reset_BB_mod_pred_rotating_registers(bb) (BB_flag(bb) &= ~BBM_MOD_PRED_ROTATING_REGISTERS)
#define Reset_BB_asm(bb) 	(BB_flag(bb) &= ~BBM_ASM)
#define Reset_BB_predicate_promote(bb) 	(BB_flag(bb) &= ~BBM_PREDICATE_PROMOTE)
#define	Reset_BB_has_post_label(x)	(BB_flag(x) &= ~BBM_POST_LABEL)

#if defined(TARG_IA64) || defined(TARG_LOONGSON)
#define Reset_BB_recovery(x)          (BB_flag(x) &= ~BBM_RECOVERY)
#define Reset_BB_chk_split(x)         (BB_flag(x) &= ~BBM_CHK_SPLIT)
#define Reset_BB_chk_split_head(x)    (BB_flag(x) &= ~BBM_CHK_SPLIT_HEAD)
#define Reset_BB_emitted(x)           (BB_flag(x) &= ~BBM_EMITTED)
#define Reset_BB_profile_splitted(x)    (BB_flag(x) &= ~BBM_PROFILE_SPLITTED)
#define Reset_BB_profile_changed(x)    (BB_flag(x) &= ~BBM_PROFILE_CHANGED)
#define Resset_BB_profile_added(x)    (BB_flag(x) &= ~BBM_PROFILE_ADDED)
#define Reset_BB_partial_bundle(x)    (BB_flag(x) &= ~BBM_PARTIAL_BUNDLE)
#define Reset_BB_chk_split_tail(x)    (BB_flag(x) &= ~BBM_CHK_SPLIT_TAIL)//bug fix for OSP_212
#endif
#if defined(TARG_X8664)
#define Reset_BB_after_pic_entry(x)  (BB_flag(x) &= ~BBM_AFTER_PIC_ENTRY)
#define Reset_BB_dispatch(x)          (BB_flag(x) &= ~BBM_DISPATCH)
#endif

#ifdef KEY
#define	Reset_BB_has_non_local_label(x)	(BB_flag(x) &= ~BBM_NON_LOCAL_LABEL)
#endif
#define BB_tail_call(bb)	(   (BB_flag(bb) & (BBM_CALL | BBM_EXIT)) \
				 == (BBM_CALL | BBM_EXIT))

/* ====================================================================
 *
 * BBKIND -- Basic Block kinds.
 *
 * ====================================================================
 */

/* WARNING: the order of this type must match array in bbutil.c. */
typedef	enum {
  BBKIND_UNKNOWN,	/* Unknown BB kind */
  BBKIND_GOTO,		/* GOTO block -- single target BB */
  BBKIND_LOGIF,		/* Logical IF -- two target BBs */
  BBKIND_VARGOTO,	/* Variable GOTO -- targets explicit, i.e. switch */
  BBKIND_INDGOTO,	/* Indirect GOTO -- targets NOT explicit */
  BBKIND_RETURN,	/* Return from subprogram */
  BBKIND_CALL,		/* Function call */
  BBKIND_REGION_EXIT,	/* Region exit */
  BBKIND_TAIL_CALL,	/* Tail call */
#ifdef TARG_IA64
  BBKIND_CHK,       /* end with check */
#endif
#if defined(TARG_SL)
  BBKIND_ZDL_BODY, 
  BBKIND_FORK,
#endif
  BBKIND_LAST		/* > last legal value */
} BBKIND;

#define VALID_BBKIND(k)	(k>=BBKIND_UNKNOWN && k<BBKIND_LAST)

extern const char *BBKIND_Name (BBKIND k);
extern BBKIND BB_kind (BB *bb);

/* ====================================================================
 *
 * Basic Block lists.
 *
 * For a generic list of basic blocks, BB_LIST should be used. 
 *
 * The successor and predecessor basic block lists need an additional
 * attribute, the probability of the edge. For these lists, BBLIST is 
 * used. It should not be used for any other purpose.
 *
 * NOTE: Currently the predecessor edge probability is not calculated 
 * and is set to NaN.
 *
 * ====================================================================
 */

/* to get definition of BB_LIST */
#include "bb_list.h"

typedef	struct bblist {
    BB		  *item;	/* The BB list element       */
    struct bblist *next;	/* The next list component   */
    float	   prob;	/* probability for this edge */
#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS) || defined(TARG_LOONGSON)
    float	   freq;	/* frequency for this edge */    
#endif
    mUINT16        flags;       /* flags                     */
} BBLIST;

#define	BBLIST_item(b)	((b)->item)
#define	BBLIST_next(b)	((b)->next)
#define BBLIST_prob(b)	((b)->prob) /*** Only valid for succ edges ***/
#define BBLIST_flags(b) ((b)->flags)
#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS) || defined(TARG_LOONGSON)
#define BBLIST_freq(b)	((b)->freq) /*** Only valid for succ edges ***/
#endif

#define BLM_PROB_FB     0x0001 /* bblist::prob based on Feedback. */
#define BLM_ON_TREE     0x0002 /* bblist::edge on the spanning tree */
#define BLM_PROB_HINT   0x0004 /* bblist::prob based on user hint. */

#define BBLIST_prob_hint_based(b)       (BBLIST_flags(b) & BLM_PROB_HINT)
#define Set_BBLIST_prob_hint_based(b)   (BBLIST_flags(b) |= BLM_PROB_HINT)
#define Reset_BBLIST_prob_hint_based(b) (BBLIST_flags(b) &= ~BLM_PROB_HINT)

#define BBLIST_prob_fb_based(b)       (BBLIST_flags(b) & BLM_PROB_FB)
#define Set_BBLIST_prob_fb_based(b)   (BBLIST_flags(b) |= BLM_PROB_FB)
#define Reset_BBLIST_prob_fb_based(b) (BBLIST_flags(b) &= ~BLM_PROB_FB)

#define BBLIST_on_tree(b)       (BBLIST_flags(b) & BLM_ON_TREE)
#define Set_BBLIST_on_tree(b)   (BBLIST_flags(b) |= BLM_ON_TREE)
#define Reset_BBLIST_on_tree(b) (BBLIST_flags(b) &= ~BLM_ON_TREE)

/* Macros for stepping through BBlists. */
#define FOR_ALL_BBLIST_ITEMS(list,item) \
	for (item = list; item != NULL; item = BBLIST_next(item))

/* Macros for stepping through BB STL lists. */
#define FOR_ALL_BB_STLLIST_ITEMS_FWD(list,item) \
      	for (item = list.begin(); item != list.end(); item++)

/* Macros for stepping through BB STL lists. */
#define FOR_ALL_BB_STLLIST_ITEMS_BKWD(list,item) \
      	for (item = list.rbegin(); item != list.rend(); item++)

#define FOR_ALL_BB_SUCCS(bb,succ) FOR_ALL_BBLIST_ITEMS(BB_succs(bb),succ)
#define FOR_ALL_BB_PREDS(bb,pred) FOR_ALL_BBLIST_ITEMS(BB_preds(bb),pred)
#define BB_First_Succ(bb) \
    ((BB_succs(bb) != NULL) ? BBLIST_item(BB_succs(bb)) : NULL)
#define BB_First_Pred(bb) \
    ((BB_preds(bb) != NULL) ? BBLIST_item(BB_preds(bb)) : NULL)

extern void BBlist_Free (BBLIST **lst);
extern INT  BBlist_Len (BBLIST *lst);
extern BBLIST *BBlist_Find_BB(BBLIST *lst, BB *bb);
extern BBLIST* BBlist_Fall_Thru_Succ(BB *bb);
extern BBLIST* BBlist_Fall_Thru_Pred(BB *bb);

inline BBLIST *BB_Find_Succ(BB *bb, BB *succ)
{
  return BBlist_Find_BB(BB_succs(bb), succ);
}

inline BBLIST *BB_Find_Pred(BB *bb, BB *pred)
{
  return BBlist_Find_BB(BB_preds(bb), pred);
}

inline BOOL BB_in_succs(BB *bb, BB *b)
{
  return BBlist_Find_BB(BB_succs(bb), b) != NULL;
}

inline BOOL BB_in_preds(BB *bb, BB *b)
{
  return BBlist_Find_BB(BB_preds(bb), b) != NULL;
}

inline INT32 BB_succs_len(BB *bb)
{
  return BBlist_Len(BB_succs(bb));
}

inline INT32 BB_preds_len(BB *bb)
{
  return BBlist_Len(BB_preds(bb));
}

inline BOOL BBlist_Has_One_Element(BBLIST *bblist)
{
  return bblist && BBLIST_next(bblist) == NULL;
}

inline BOOL BB_Has_One_Succ(BB *bb)
{
  return BBlist_Has_One_Element(BB_succs(bb));
}

inline BOOL BB_Has_One_Pred(BB *bb)
{
  return BBlist_Has_One_Element(BB_preds(bb));
}

/* ====================================================================
 *
 * Global variables.
 *
 * ====================================================================
 */

/* The following global	variables describe BBs for the current PU: */

extern BB *REGION_First_BB;	/* First BB in current REGION (or PU) */
				/* Note that each region has own list of BB's,
				 * and these lists are merged when a nested
				 * region is processed; so may not see all
				 * BB's until come to PU level. */
extern BB_NUM PU_BB_Count;	     /*	Number of BBs in the current
				      *	PU, indexed 1..PU_BB_Count in
				      *	BB_idx.
				      */

extern BB_LIST *Entry_BB_Head;		/* chain of entry point	BBs  */
extern BB_LIST *Exit_BB_Head;		/* chain of exit BBs */

extern BB **BB_Vec;		/* mapping from bb idx to BB in each PU */
#define BBvec(i) BB_Vec[i] 

/* to get the definition of BB_MAP, must come after the definition of BB */
#include "bb_map.h"

#include "bb_set.h"  /* to get BB_SET, must come after definition of BB_vec */


/* ====================================================================
 *
 *  BB_REGION represents a region of BBs in the CFG, analogous to
 *  other BB data structure such as BBLIST and BB_SET.
 *
 *  Any region of the CFG can be uniquely identified by
 *   1) a set of entry blocks and;
 *   2) a set of exit blocks.
 *
 *  The region is defined to contain all basic blocks in any path 
 *  starting from an entry block, and not containing any exit block.
 *
 *  The has_omega field specifies whether the OPs in the region has
 *  CG_LOOP_INFO.  Currently either all OPs in the region have omega
 *  or none of them have omega.
 *
 *  INVARIANTS (or properties of a valid region):
 *    entries must contain at least one basic block.
 *    exits might bbe empty, e.g. the REGION represents an infinite loop.
 *    exit blocks are not considered part of the REGION.
 *    no basic block can be in both entries and exits of the same region.
 *    a basic block can be appear more than once in entries or in exits.
 *    If has_omega is TRUE, all OPs in the region must have CG_LOOP_INFO.
 *
 *  template <class Container>
 *  void BB_REGION_to_Container(Container& c, const BB_REGION& r)
 *    Convert a BB_REGION representation into a Container.
 *
 *  BB_SET *BB_REGION_to_BB_SET(BB_SET *bbs, const BB_REGION& r, MEM_POOL *pool);
 *    Convert a BB_REGION representation into a BB_SET representation.
 *    The usage convention follows BB_SET_UnionD() where the pool is the
 *    MEM_POOL of bbs.
 *
 * ====================================================================
 */

struct BB_REGION {
  typedef mempool_allocator<BB*> allocator_type;
  typedef vector<BB*, allocator_type> bb_vector;
  allocator_type data_allocator;
  bb_vector entries;   
  bb_vector exits;  
  bool      has_omega;

  // Assert if any the invariants becomes false.
  void Verify() const; 

  bool Has_omega() const { return has_omega; }
  void Set_has_omega() { has_omega = true; }
  
  //  Construct an empty BB_REGION.
  //
  BB_REGION(MEM_POOL *pool)
    :data_allocator(pool),
     entries(0, (BB*)NULL, data_allocator),
     exits(0, (BB*)NULL, data_allocator),
     has_omega(false)
  {}
  
  //  Construct a BB_REGION from a BB_SET.
  //
  BB_REGION(BB_SET *bbs, MEM_POOL *pool);
  void Print(void) const;
};

extern BB_SET *BB_REGION_to_BB_SET(BB_SET *bbs, const BB_REGION& r,
				   MEM_POOL *pool);
extern void BB_REGION_to_Vector (vector<BB*>& c, const BB_REGION& r);

/* =======================================================================
 *
 *  BB_VISITED_COUNTER
 *    Use a version counter to keep track of visited BBs, so that the
 *    same data structure can be used several times without the need
 *    of resetting every element.
 *
 *    A BB is visited if its counter equals the current version counter.
 *   
 *
 *    BB_VISITED_COUNTER(MEM_POOL *m)
 *      -- constructor
 *
 *    Visited(BB *bb)
 *      -- returns TRUE if BB is visited
 *
 *    Set_visited(BB *bb)
 *      -- mark BB visited
 *
 *    Init()
 *      -- initialization before use
 *
 *  This struct can be generalized using templates
 *  for any index-able data structure.
 * 
 * =======================================================================
 */
class BB_VISITED_COUNTER {
  MEM_POOL *pool;
  INT  current_version;
  INT  size;
  INT  *counter;

public:
  //  Set BB visited
  void Set_visited(BB *bb) const { counter[BB_id(bb)] = current_version; }

  //  Test if BB has been visited
  BOOL Visited(BB *bb) const { return counter[BB_id(bb)] == current_version; }
  
  //  Initialize:  
  //   by incrementing the version counter, all BB visited are reset
  //
  void Init() {
    INT required_size = PU_BB_Count + 2;      // use the magic +2
    if (required_size > size) {
#pragma mips_frequency_hint NEVER
      if (size == 0) 
	counter = (INT *) MEM_POOL_Alloc(pool, required_size * sizeof(INT));
      else
	counter = (INT *) MEM_POOL_Realloc(pool, counter, size * sizeof(INT), required_size * sizeof(INT));
      for (INT i = size; i < required_size; i++)
	counter[i] = 0;
      size = required_size;
    }
    if (++current_version == 0) {  // overflowed ==> reset all counters
#pragma mips_frequency_hint NEVER
      for (INT i = 0; i < size; i++)
	counter[i] = 0;
    }
  }

  BB_VISITED_COUNTER(MEM_POOL *m)
    :current_version(0),counter(NULL), size(0), pool(m) 
  {}
};



/* ====================================================================
 *
 * BB manipulation routines (implemented in bbutil.c).
 *
 * ====================================================================
 */

/* =====  Creation and Initialization  ====== */

/* Described in comment at top */
extern BB *Gen_BB(void);
extern BB *Gen_BB_N(INT n);
extern BB *Gen_BB_Like (BB *model);
extern BB *Gen_And_Append_BB(BB *prev_bb);
extern BB *Gen_And_Insert_BB_Before(BB *point);
extern BB *Gen_And_Insert_BB_After(BB *point);
extern void Chain_BBs(BB* bb1, BB* bb2);
extern void Target_Simple_Fall_Through_BB(BB* bb, BB* target_bb);
extern void Target_Logif_BB(BB* bb, BB* br_targ_bb, float br_targ_prob,
				    BB* fall_through);
extern void Target_Cond_Branch(BB* bb, BB* br_targ_bb, float br_targ_prob);
extern void Negate_Logif_BB(BB *bb);
extern void Add_Goto_Op(BB *bb, BB *target_bb);
extern void Add_Goto(BB *bb, BB *target_bb);
extern BB* Create_Dummy_BB( BB *dest_bb );
extern LABEL_IDX Gen_Label_For_BB (BB *bb);
extern BOOL Is_Label_For_BB(LABEL_IDX label, BB *bb);
extern BOOL Change_Switchtable_Entries(ST *tbl, BB *old_tgt, BB *new_tgt);
extern void Change_Succ(BB *pred, BB *old_succ, BB *new_succ);
extern void Change_Succ_Prob(BB *pred, BB *succ, float prob);

/* Add/copy an annotation to a BB */
extern void BB_Add_Annotation(BB *bb, ANNOTATION_KIND kind, void *info);
extern INT BB_Copy_Annotations(BB *to_bb, BB *from_bb, ANNOTATION_KIND kind);
extern INT BB_Copy_All_Annotations(BB *to_bb, BB *from_bb);

/* Free the memory associated with the live BBs and clear pointers: */
extern	void  Free_BB_Memory ( void );

/* =====  Predecessor/Successor	Handling  ====== */

/* Link up the pred and succ basic blocks. */
extern void Link_Pred_Succ (BB *pred, BB *succ);
#if defined (KEY)
extern void Link_Pred_Succ_with_Prob(BB *pred, BB *succ, float prob, 
				     BOOL via_feedback = FALSE,
				     BOOL set_prob = FALSE,
				     BOOL via_hint =FALSE, 
				     BOOL incr_prob=TRUE);
#else
extern void Link_Pred_Succ_with_Prob(BB *pred, BB *succ, float prob, 
				     BOOL via_feedback = FALSE,
				     BOOL set_prob = FALSE);
#endif
extern BBLIST *BBlist_Add_BB(BBLIST **lst, BB *bb);
extern void BBlist_Delete_BB(BBLIST **lst, BB *bb);
#if defined(KEY)
extern BBLIST *BBlist_Add_BB_with_Prob(BBLIST **lst, BB *bb, float prob,
				       BOOL via_feedback = FALSE,
				       BOOL set_prob     = FALSE,
				       BOOL via_hint = FALSE,
				       BOOL incr_prob = TRUE);

#else
extern BBLIST *BBlist_Add_BB_with_Prob(BBLIST **lst, BB *bb, float prob,
				       BOOL via_feedback = FALSE,
				       BOOL set_prob     = FALSE,
				       BOOL incr_prob = TRUE);
#endif

/* Unlink the pred and succ basic blocks. */
extern void Unlink_Pred_Succ (BB *pred, BB *succ);


/*  If bb has a unique successor not included in the set described by */
/*  the BB_MAP32 map, return it.  Otherwise return NULL. */
extern BB *BB_Unique_Successor_Not_In_Set( BB *bb, BB_MAP map );

/*
 *	If bb has a unique successor/predecessor, return it.  
 *	Otherwise return NULL. 
 */
extern BB *BB_Unique_Successor( BB *bb );
extern BB *BB_Unique_Predecessor( BB *bb );
extern BB *BB_Unique_Source( BB *bb );
extern void Remove_Explicit_Branch (BB *bb);
extern BB *BB_Fall_Thru_Successor( BB *bb );
extern BB *BB_Fall_Thru_Predecessor( BB *bb );
extern BOOL BB_Retarget_Branch(BB *bb, BB *from, BB *to);
extern BOOL BB_Can_Retarget_Branch(BB *bb, BB *from);

extern BB *BB_Other_Successor(BB *bb, BB *succ);
extern BB *BB_Other_Predecessor(BB *bb, BB *pred);

inline BOOL BB_Is_Unique_Successor( BB *bb, BB *succ )
{
  return BB_Unique_Successor(bb) == succ;
}

inline BOOL BB_Is_Unique_Predecessor( BB *bb, BB *pred)
{
  return BB_Unique_Predecessor(bb) == pred;
}

#define BB_Delete_Successors(bb) BBlist_Free(&BB_succs(bb))
#define BB_Delete_Predecessors(bb) BBlist_Free(&BB_preds(bb))


/* =====  Miscellaneous	 ======	*/

/* Initialize for a new PU */
extern void BB_PU_Initialize(void);
/* Initialize for a new REGION */
extern void BB_REGION_Initialize(void);

/* Return the op for the terminating branch of a given BB */
extern struct op *BB_branch_op (BB *);

#ifdef TARG_IA64
/* Return the last non nop op of a branch bb */
extern OP* Last_Non_Nop_op (BB *); 
extern OP* BB_Last_chk_op(BB *);
#endif 

/* Return the terminating xfer OP in a given BB */
extern struct op* BB_xfer_op( BB *bb );

/* Return the last OP which isn't a copy/xfer OP in a given BB */
extern struct op* BB_copy_xfer_op( BB *bb );

/* Return the call op in a given BB */
extern struct op* BB_call_op(BB* bb);

/* Return the op that that does the entry/exit SP adjustment in BB */
extern struct op *BB_entry_sp_adj_op (BB *bb);
extern struct op *BB_exit_sp_adj_op (BB *bb);

/* Set the op that does the entry/exit SP adjustment in BB */
extern void Set_BB_entry_sp_adj_op (BB *bb, struct op *);
extern void Set_BB_exit_sp_adj_op (BB *bb, struct op *);

/* Remove a BB from the list: */
extern void  Remove_BB (BB *);

/* Insert a BB into the list after a given BB (NULL for beginning): */
extern void  Insert_BB ( BB *bb, BB *after );

/* Move a BB from its current position to be after a given BB
 * (NULL for beginning):
 */
extern void  Move_BB ( BB *bb, BB *after );

/* Append the BBs from a previously compiled REGION to the
 * list of BBs for the current REGION.
 */
extern BB *Append_Region_BBs( BB *prev, RID *rid );

/* set BB_unreachable(bb) flag for all unreachable bbs in current region */
extern void BB_Mark_Unreachable_Blocks (void);

/* transfer annotations for entry/exit blocks for PU */
extern void BB_Transfer_Exitinfo(BB* from, BB* to);
extern void BB_Transfer_Entryinfo(BB* from, BB* to);
extern void BB_Transfer_Callinfo(BB* from, BB* to);
extern void BB_Transfer_Asminfo (BB *from, BB *to);

/* Print just the Procedure name from the BB text */
#pragma mips_frequency_hint NEVER Get_Procedure_Name
extern char *Get_Procedure_Name ( void );

/* Print the given BB or BBLIST: */
extern void Print_BB_Header ( BB *bp,
			      BOOL flow_info_only, BOOL print_tn_info );
#pragma mips_frequency_hint NEVER Print_BB_Header
extern void Trace_BB ( BB *bp, char *msg );
#pragma mips_frequency_hint NEVER Trace_BB
extern void Print_BB ( BB *bp );
#pragma mips_frequency_hint NEVER Print_BB
extern void Print_BB_No_Srclines ( BB *bp );
#pragma mips_frequency_hint NEVER Print_BB_No_Srclines
extern void Print_BB_Pragmas( BB *bp );
#pragma mips_frequency_hint NEVER Print_BB_Pragmas
extern void Print_All_BBs ( void );
#pragma mips_frequency_hint NEVER Print_All_BBs
extern void Print_All_BB_Headers ( void );
#pragma mips_frequency_hint NEVER Print_All_BB_Headers
extern void Print_Entry_Chain ( char *banner );
#pragma mips_frequency_hint NEVER Print_Entry_Chain
extern void Print_Flow_Graph  ( char *banner, BOOL verbose );
#pragma mips_frequency_hint NEVER Print_Flow_Graph
extern void Print_LOOPINFO( LOOPINFO *info );
#pragma mips_frequency_hint NEVER Print_LOOPINFO

#define BB_length(bb) OPS_length(&(bb)->ops)


UINT16 BB_New_Op_Map_Idx(BB *bb);

void BB_Insert_Op_Before(BB *bb, OP *point, OP *op);
void BB_Insert_Op_After(BB *bb, OP *point, OP *op);
void BB_Append_Op(BB *bb, OP *op);
void BB_Prepend_Op(BB *bb, OP *op);
void BB_Insert_Ops_Before(BB *bb, OP *point, OPS *ops);
void BB_Insert_Ops_After(BB *bb, OP *point, OPS *ops);
void BB_Append_Ops(BB *bb, OPS *ops);
void BB_Prepend_Ops(BB *bb, OPS *ops);
void BB_Insert_Op(BB *bb, OP *point, OP *op, BOOL before);
void BB_Insert_Ops(BB *bb, OP *point, OPS *ops, BOOL before);

void BB_Insert_Noops(OP *op, INT num, BOOL before);

void BB_Move_Op(BB *to_bb, OP *point, BB *from_bb, OP *op, BOOL before);
void BB_Move_Op_Before(BB *to_bb, OP *point, BB *from_bb, OP *op);
void BB_Move_Op_After(BB *to_bb, OP *point, BB *from_bb, OP *op);
void BB_Move_Op_To_Start(BB *to_bb, BB *from_bb, OP *op);
void BB_Move_Op_To_End(BB *to_bb, BB *from_bb, OP *op);

void BB_Sink_Op_Before(BB *bb, OP *op, OP *point);
void BB_Update_OP_Order(BB *bb);
void BB_Verify_OP_Order(BB *bb);

BOOL BB_Move_Delay_Slot_Op (BB *bb);

void BB_Append_All(BB *to_bb, BB *from_bb);
void BB_Prepend_All(BB *to_bb, BB *from_bb);

OP *BB_Remove_Branch(BB *bb);
void BB_Remove_Ops(BB *bb, OPS *ops);
void BB_Remove_Op(BB *bb, OP *op);
void BB_Remove_All(BB *bb);

extern SRCPOS BB_Loop_Srcpos(BB *bb);
inline INT32 BB_Loop_Lineno(BB *bb)
{
  return Srcpos_To_Line(BB_Loop_Srcpos(bb));
}

BOOL BB_Add_Ancestors(struct bs **set, BB *bb, BB *start_bb, MEM_POOL *pool);

BOOL BB_Has_Exc_Label(BB *bb);
BOOL BB_Has_Addr_Taken_Label(BB *bb);
BOOL BB_Has_Outer_Block_Label(BB *bb);

struct bb_map *BB_Depth_First_Map(struct bs *region, BB *entry);
struct bb_map *BB_Topological_Map(struct bs *region, BB *entry);

BOOL BB_Is_Cold(BB *bb);
#if defined(TARG_SL)
BOOL BB_Is_Hot(BB* bb);
#endif
ST *Gen_ST_For_BB(BB *bb);
ST *BB_st(BB *bb);

void Split_BBs();

extern BB_SET* Find_BB_Parents(BB* bb);

#ifdef Is_True_On
extern void Verify_BB(BB *);
#else
#define Verify_BB(bb)
#endif

/*
**	return true  if BB needs to be processed (compile)
**	return false if BB is already compiled
*/
inline BOOL BB_compile(BB *bb)
{
  return (BB_rid(bb) == NULL || RID_level(BB_rid(bb)) < RL_CGSCHED);
}

/* Routine for displaying the flow graph using DaVinci */
void draw_flow_graph(void);
void verify_flow_graph(void);

#endif /* bb_INCLUDED */
