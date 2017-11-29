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
 * Module: oputil.c
 * $Revision: 1.28 $
 * $Date: 06/03/14 14:38:58-08:00 $
 * $Author: tkong@hyalite.keyresearch $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.oputil.cxx $
 *
 * Revision history:
 *  12-Oct-89 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *  12-Jun-91 - Removed INS/INSCH stuff to insutil.c
 *  12-Jun-91 - Added OP insertion/deletion stuff from bbutil.c
 *
 * Description:
 *
 * Utility routines for manipulating the CGIR OP and OPS data
 * structures.  Also implements a few routines that manipulate BBs as
 * well since the BB implementation is intrinsically intertwined with
 * the OP implementation.  See "op.h" and "bb.h" for interfaces.
 *
 * TODO: Combine "op.h" and "bb.h" into "cgir.h", and "oputil.cxx" and
 *       "bbutil.cxx" into "cgir.cxx".
 *
 * ====================================================================
 * ==================================================================== */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <stdarg.h>

#include "defs.h"
#include "config.h"
#include "tracing.h"
#include "erglob.h"
#include "printsrc.h"

#include "opt_alias_interface.h"        /* for Print_alias_info */
#include "cg.h"                         /* for Alias_Manager */

#include "cgir.h"
#include "cg.h"
#include "register.h"
#include "cg_dep_graph.h"
#include "cgprep.h"
#include "cg_loop.h"
#include "cgtarget.h"
#include "cg_spill.h"
#ifdef TARG_IA64
#include "targ_sim.h"
#endif
#include "wn.h"
#include "whirl2ops.h"
#include "cgexp.h"
#include "xstats.h"
#include "tag.h"

/* Allocate OPs for the duration of the PU. */
#define OP_Alloc(size)  ((OP *)Pu_Alloc(size))

/* OP mutators that are NOT to be made public */
#define Set_OP_code(o,opc)	((o)->opr = (mTOP)(opc))
#define Set_OP_opnds(o,n)	((o)->opnds = (n))
#define Set_OP_results(o,n)	((o)->results = (n))

#ifdef TARG_IA64
BOOL
OP_xfer(OP *op) 
{
  if(TOP_is_xfer(OP_code(op))) 
    return TRUE;

  if(OP_chk(op)){	  
    BB* home_bb = OP_bb(op);
    if(!home_bb) 
        return FALSE;
    if(BB_succs_len(home_bb) != 2) 
        return FALSE;
    if(op != BB_last_op(home_bb)) 
        return FALSE;    
    return TRUE;
  }
  return FALSE;
}


/* -----------------------------------------------------------------------
 * check if an operation that restores b0 register
 * -----------------------------------------------------------------------
 */
BOOL
OP_restore_b0(OP *op)
{
  if(OP_results(op) != 1 || OP_call(op)) return FALSE;
  TN* res_tn = OP_result(op, 0);
  if(TN_is_constant(res_tn)) return FALSE;
  return (TN_register_class(res_tn) == ISA_REGISTER_CLASS_branch && TN_register(res_tn) == REGISTER_MIN+0);
}

/* -----------------------------------------------------------------------
 * check if an operation that restores ar.pfs register
 * -----------------------------------------------------------------------
 */
BOOL
OP_restore_ar_pfs(OP *op)
{
  if(OP_results(op) != 1) return FALSE;
  TN* res_tn = OP_result(op, 0);
  if(TN_is_constant(res_tn)) return FALSE;
  return TN_is_pfs_reg(res_tn);
}

/* -----------------------------------------------------------------------
 * check if an operation that restores ar.lc register
 * -----------------------------------------------------------------------
 */
BOOL
OP_def_ar_lc(OP *op)
{
  if(OP_results(op) != 1) return FALSE;
  TN* res_tn = OP_result(op, 0);
  if(TN_is_constant(res_tn)) return FALSE;
  return TN_is_lc_reg(res_tn);
}

static inline void
Copy_GOT_Sym_Info (OP* new_op, OP* op) {
  if (OP_load_GOT_entry(op)){
     OP_MAP_Set (OP_Ld_GOT_2_Sym_Map, 
                new_op, 
		OP_MAP_Get (OP_Ld_GOT_2_Sym_Map, op));
  }
}

#endif // TARG_IA64

// ----------------------------------------
// Copy ASM_OP_ANNOT when duplicating an OP
// ----------------------------------------
static inline void
Copy_Asm_OP_Annot(OP* new_op, OP* op) 
{
  if (OP_code(op) == TOP_asm) {
    OP_MAP_Set(OP_Asm_Map, new_op, OP_MAP_Get(OP_Asm_Map, op));
  }
}


/* ====================================================================
 *
 * New_OP
 *
 * Create and clear a new OP structure.
 *
 * ====================================================================
 */

static OP *
#ifdef TARG_IA64
New_OP ( INT results, INT opnds, INT hidden_opnds)
#else
New_OP ( INT results, INT opnds )
#endif
{
  OP *op = OP_Alloc ( OP_sizeof(results, opnds
#if defined(TARG_IA64)
				+hidden_opnds
#endif
				) );
  PU_OP_Cnt++;
  Set_OP_opnds(op, opnds);
  Set_OP_results(op, results);
  return op;
}


/* ====================================================================
 *
 * Dup_OP
 *
 * Create a new OP structure as a duplicate of another, with zero ID.
 *
 * ====================================================================
 */

OP *
Dup_OP ( OP *op )
{
  INT results = OP_results(op);
  INT opnds = OP_opnds(op);
#ifdef TARG_IA64
  INT hidden_opnds = CGTARG_Max_Number_of_Hidden_Opnd (OP_code(op)); 
  OP *new_op = New_OP (results, opnds, hidden_opnds);
#else
  OP *new_op = New_OP ( results, opnds );
#endif

  memcpy(new_op, op, OP_sizeof(results, opnds));
  new_op->next = new_op->prev = NULL;
  new_op->bb = NULL;

  Copy_Asm_OP_Annot ( new_op, op );
#ifdef TARG_IA64
  if (OP_load_GOT_entry(op)) {
    Copy_GOT_Sym_Info (new_op, op);
  }
#endif
  if (OP_has_tag(op)) {
	Set_OP_Tag (new_op, Gen_Tag());
  }
  
#ifdef TARG_X8664
  if ( TOP_is_vector_high_loadstore ( OP_code ( new_op ) ) )
    Set_OP_cond_def_kind(new_op, OP_ALWAYS_COND_DEF);
#endif
#if defined(KEY) && !defined(TARG_NVISA)
  // If OP is a restore, increment the spill location's restore count.
  if (OP_load(op)) {
    ST *spill_loc = CGSPILL_OP_Spill_Location(op);
    if (spill_loc != (ST *)0)		// It's a spill OP.
      CGSPILL_Inc_Restore_Count(spill_loc);
  }
#endif

  return new_op;
}

/* =====================================================================
 *			      OPS stuff
 *		(see "op.h" for interface description)
 * =====================================================================
 */

/* -----------------------------------------------------------------------
 *
 * void insert_ops_before(OPS *ops, OP *point, OP *first, OP *last)
 * void insert_ops_after(OPS *ops, OP *point, OP *first, OP *last)
 * void append_ops(OPS *ops, OP *first, OP *last)
 * void prepend_ops(OPS *ops, OP *first, OP *last)
 * void insert_ops(OPS *ops, OP *point, OP *first, OP *last, BOOL before)
 *
 * Requires: <last> is a (not necessarily direct) successor of <first>.
 *
 * Insert the OPs from <first> to <last> inclusive in the place implied
 * by the function names and/or <point> and/or <before> arguments when
 * applicable.
 *
 * Basically these are the workhorses for the OPS/BB Insert routines,
 * but they avoid setting any OP attributes other than the next and
 * previous pointers.
 *
 * -----------------------------------------------------------------------
 */

inline void prepend_ops(OPS *ops, OP *first, OP *last)
{
  OP **pprev = OPS_first(ops) ? &OPS_first(ops)->prev : &ops->last;
  first->prev = NULL;
  last->next = OPS_first(ops);
  ops->first = first;
  *pprev = last;
}


inline void append_ops(OPS *ops, OP *first, OP *last)
{
  OP **pnext = OPS_last(ops) ? &OPS_last(ops)->next : &ops->first;
  last->next = NULL;
  first->prev = OPS_last(ops);
  ops->last = last;
  *pnext = first;
}

inline void insert_ops_before(OPS *ops, OP *point, OP *first, OP *last)
{
  OP **prevp = OP_prev(point) ? &OP_prev(point)->next : &ops->first;
  *prevp = first;
  last->next = point;
  first->prev = OP_prev(point);
  point->prev = last;
}

inline void insert_ops_after(OPS *ops, OP *point, OP *first, OP *last)
{
  OP **nextp = OP_next(point) ? &OP_next(point)->prev : &ops->last;
  *nextp = last;
  first->prev = point;
  last->next = OP_next(point);
  point->next = first;
}

inline void insert_ops(OPS *ops, OP *point, OP *first, OP *last, BOOL before)
{
  if (point == NULL) {
    if (before)
      prepend_ops(ops, first, last);
    else
      append_ops(ops, first, last);
  } else {
    if (before)
      insert_ops_before(ops, point, first, last);
    else
      insert_ops_after(ops, point, first, last);
  }
}


/* -----------------------------------------------------------------------
 *   
 *  Sink the OP before point
 *
 * -----------------------------------------------------------------------
 */
void BB_Sink_Op_Before(BB *bb, OP *op, OP *point)
{
  if (OP_next(op) == point) return;

  Is_True(OP_bb(op) == bb && OP_bb(point) == bb,
	  ("Sink_Op_Before: must sink inside the bb."));

  // Disconnect "op" from body
  OP *t1 = OP_prev(op);
  OP *t2 = OP_next(op);
  if (t1) t1->next = t2;
  if (t2) t2->prev = t1;

  // Reconnect "op" to "succ"
  OP *prev = OP_prev(point);
  op->prev = prev;
  op->next = point;
  prev->next = op;
  point->prev = op;

  if (op == BB_first_op(OP_bb(op)))
    OP_bb(op)->ops.first = t2;

}


/* -----------------------------------------------------------------------
 *
 *  void setup_ops(BB *bb, OP *first, OP *last, UINT32 len)
 *
 *  Setup various fields (bb/map_idx/order) on OPs between <first> and
 *  <last>, inclusive, newly inserted into <bb>.  The bb and map_idx
 *  fields are fairly straightforward.  The order field is used to
 *  indicate relative order within <bb> (unless it is NULL).  May also
 *  change the order fields of other OPs in the BB.  Should almost
 *  always execute in time linear in the length of the chain from
 *  <first> to <last>.  Worst case time is linearly dependent on the
 *  size of the BB, but should be tuned to make this case extremely
 *  rare.
 *
 * ----------------------------------------------------------------------- */

/* Assume op->order is some kind of unsigned integer type.
 */
#define ORDER_TYPE UINT16
#define mORDER_TYPE mUINT16
#define mMAP_IDX_TYPE mUINT16
#define ORDER_BITS (sizeof(mORDER_TYPE) * 8)
#define MIN_INITIAL_SPACING \
  ((mORDER_TYPE)1 << (ORDER_BITS-(sizeof(mMAP_IDX_TYPE)*8)))
#define INITIAL_SPACING ((ORDER_TYPE)(MIN_INITIAL_SPACING * 8))
#define MAX_ORDER ((ORDER_TYPE)-1)


static void setup_ops(BB *bb, OP *first, OP *last, UINT32 len)
{
  OP *op;
  ORDER_TYPE incr;
  ORDER_TYPE order_before;
  ORDER_TYPE order_after;
  ORDER_TYPE order;

  /* Empty lists are easy.
   */
  if (len == 0) return;

  /* Get the 'order' number of the OPs immediately before and after
   * the OPs were inserting.
   */
  order_before = OP_prev(first) ? OP_prev(first)->order : 0;
  order_after = OP_next(last) ? OP_next(last)->order : MAX_ORDER;

  /* Compute the increment to use when assigning 'order' numbers
   * so they will fit between the OPs were inserting at. If there
   * isn't enough room, i.e. 'incr' is 0, we'll detect that later.
   */
  incr = (order_after - order_before - 1) / (len + 1);
  if (incr > INITIAL_SPACING) incr = INITIAL_SPACING;

  /* Loop over the OPs being inserted and initialize the necessary
   * fields. Our attempt at generating 'order' numbers is an
   * educated guess, but in many cases we'll guess right.
   */
  order = order_before;
  op = first;
  do {
    FmtAssert(op, ("input ops not connected properly"));
    op->bb = bb;
    op->map_idx = BB_New_Op_Map_Idx(bb);
    order += incr;
    op->order = order;
    REGISTER_CLASS_OP_Update_Mapping (op);
    op = OP_next(op);
  } while (op != OP_next(last));

  /* All done if we were able to squeeze in the 'order' numbers.
   */
  if (incr != 0) goto done;

  /* It was not possible to assign 'order' numbers to the new OPs --
   * we'll have to re-order some of the OPs on the list we're inserting
   * into. Include OPs from before and/or after the inserted OPs until
   * we make a big enough hole that it is possible to re-order.
   *
   * NOTE: We include all 'after' OPs before including any of the 
   * 'before' OPs. This tends to keep the beginning of the list
   * "nicely" ordered. Another approach would be to include from
   * the direction that adds the most to the delta between 
   * 'order_after' and 'order_before'.
   */
  do {
    if (OP_next(last)) {
      last = OP_next(last);
      order_after = OP_next(last) ? OP_next(last)->order : MAX_ORDER;
    } else if (OP_prev(first)) {
      first = OP_prev(first);
      order_before = OP_prev(first) ? OP_prev(first)->order : 0;
    } else {
      FmtAssert(FALSE, ("unable to reorder"));
    }
    len++;
    incr = (order_after - order_before - 1) / (len + 1);
  } while (incr == 0);
  if (incr > INITIAL_SPACING) incr = INITIAL_SPACING;

  /* Re-order the OPs.
   */
  op = first;
  order = order_before;
  do {
    order += incr;
    op->order = order;
    op = OP_next(op);
  } while (op != OP_next(last));

done:
  /* C insists on requiring at least one statement after a label.
   * There won't be any when VERIFY_OPS is not defined, so here ya go...
   */
  ;

#ifdef VERIFY_OPS
  {
    UINT16 len = 1;
    op = first;
    while (OP_prev(op)) op = OP_prev(op);
    while (OP_next(op)) {
      FmtAssert(op->order < OP_next(op)->order, ("OP order set wrong"));
      FmtAssert(op->bb == bb, ("OP bb set wrong"));
      op = OP_next(op);
      len++;
    }
    FmtAssert(len == BB_length(bb), ("BB_length set wrong"));
  }
#endif
}


void OPS_Insert_Op(OPS *ops, OP *point, OP *op, BOOL before)
{
  insert_ops(ops, point, op, op, before);
  ops->length++;
}


void OPS_Insert_Op_Before(OPS *ops, OP *point, OP *op)
{
  insert_ops_before(ops, point, op, op);
  ops->length++;
}


void OPS_Insert_Op_After(OPS *ops, OP *point, OP *op)
{
  insert_ops_after(ops, point, op, op);
  ops->length++;
}


void OPS_Append_Op(OPS *ops, OP *op)
{
  append_ops(ops, op, op);
  ops->length++;
}


void OPS_Prepend_Op(OPS *ops, OP *op)
{
  prepend_ops(ops, op, op);
  ops->length++;
}


void OPS_Insert_Ops(OPS *ops, OP *point, OPS *new_ops, BOOL before)
{
  if (OPS_first(new_ops) == NULL) return;
  insert_ops(ops, point, OPS_first(new_ops), OPS_last(new_ops), before);
  ops->length += OPS_length(new_ops);
}


void OPS_Insert_Ops_Before(OPS *ops, OP *point, OPS *new_ops)
{
  if (OPS_first(new_ops) == NULL) return;
  insert_ops_before(ops, point, OPS_first(new_ops), OPS_last(new_ops));
  ops->length += OPS_length(new_ops);
}


void OPS_Insert_Ops_After(OPS *ops, OP *point, OPS *new_ops)
{
  if (OPS_first(new_ops) == NULL) return;
  insert_ops_after(ops, point, OPS_first(new_ops), OPS_last(new_ops));
  ops->length += OPS_length(new_ops);
}


void OPS_Append_Ops(OPS *ops, OPS *new_ops)
{
  if (OPS_first(new_ops) == NULL) return;
  append_ops(ops, OPS_first(new_ops), OPS_last(new_ops));
  ops->length += OPS_length(new_ops);
}


void OPS_Prepend_Ops(OPS *ops, OPS *new_ops)
{
  if (OPS_first(new_ops) == NULL) return;
  prepend_ops(ops, OPS_first(new_ops), OPS_last(new_ops));
  ops->length += OPS_length(new_ops);
}

// Update OP order
//
void BB_Update_OP_Order(BB *bb)
{
  INT order = 0;
  INT incr = INITIAL_SPACING;
  for (OP *op = BB_first_op(bb); op; op = OP_next(op)) {
    order += incr;
    op->order = order;
  } 
}

// Verify OP order
//
void BB_Verify_OP_Order(BB *bb)
{
  INT prev_order = -1;
  for (OP *op = BB_first_op(bb); op; op = OP_next(op)) {
    FmtAssert(prev_order < op->order,
	      ("BB_Verify_OP_Order: OP_order() is not correct."));
    prev_order = op->order;
  }
}


/* =====================================================================
 *			   (Some) BB stuff
 *		(see "bb.h" for interface description)
 * =====================================================================
 */

void BB_Insert_Op(BB *bb, OP *point, OP *op, BOOL before)
{
  Is_True(bb, ("can't insert in NULL BB"));
  insert_ops(&bb->ops, point, op, op, before);
  bb->ops.length++;
  setup_ops(bb, op, op, 1);
}


void BB_Insert_Op_Before(BB *bb, OP *point, OP *op)
{
  Is_True(bb, ("can't insert in NULL BB"));
  insert_ops_before(&bb->ops, point, op, op);
  bb->ops.length++;
  setup_ops(bb, op, op, 1);
}


void BB_Insert_Op_After(BB *bb, OP *point, OP *op)
{
  Is_True(bb, ("can't insert in NULL BB"));
  insert_ops_after(&bb->ops, point, op, op);
  bb->ops.length++;
  setup_ops(bb, op, op, 1);
}


void BB_Prepend_Op(BB *bb, OP *op)
{
  Is_True(bb, ("can't insert in NULL BB"));
  prepend_ops(&bb->ops, op, op);
  bb->ops.length++;
  setup_ops(bb, op, op, 1);
}


void BB_Append_Op(BB *bb, OP *op)
{
  Is_True(bb, ("can't insert in NULL BB"));
  append_ops(&bb->ops, op, op);
  bb->ops.length++;
  setup_ops(bb, op, op, 1);
}


void BB_Insert_Ops(BB *bb, OP *point, OPS *ops, BOOL before)
{
  if (OPS_first(ops) == NULL) return;
  insert_ops(&bb->ops, point, OPS_first(ops), OPS_last(ops), before);
  bb->ops.length += OPS_length(ops);
  setup_ops(bb, OPS_first(ops), OPS_last(ops), OPS_length(ops));
}


void BB_Insert_Ops_Before(BB *bb, OP *point, OPS *ops)
{
  if (OPS_first(ops) == NULL) return;
  insert_ops_before(&bb->ops, point, OPS_first(ops), OPS_last(ops));
  bb->ops.length += OPS_length(ops);
  setup_ops(bb, OPS_first(ops), OPS_last(ops), OPS_length(ops));
}


void BB_Insert_Ops_After(BB *bb, OP *point, OPS *ops)
{
  if (OPS_first(ops) == NULL) return;
  insert_ops_after(&bb->ops, point, OPS_first(ops), OPS_last(ops));
  bb->ops.length += OPS_length(ops);
  setup_ops(bb, OPS_first(ops), OPS_last(ops), OPS_length(ops));
}


void  BB_Insert_Noops(OP *op, INT num, BOOL before)
{
  OPS new_ops = OPS_EMPTY;
  INT i;

  for (i = 0; i < num; i++) {
    Exp_Noop (&new_ops);
  }
  BB_Insert_Ops(OP_bb(op), op, &new_ops, before);
}


void BB_Prepend_Ops(BB *bb, OPS *ops)
{
  if (OPS_first(ops) == NULL) return;
  prepend_ops(&bb->ops, OPS_first(ops), OPS_last(ops));
  bb->ops.length += OPS_length(ops);
  setup_ops(bb, OPS_first(ops), OPS_last(ops), OPS_length(ops));
}


void BB_Append_Ops(BB *bb, OPS *ops)
{
  if (OPS_first(ops) == NULL) return;
  append_ops(&bb->ops, OPS_first(ops), OPS_last(ops));
  bb->ops.length += OPS_length(ops);
  setup_ops(bb, OPS_first(ops), OPS_last(ops), OPS_length(ops));
}


void BB_Move_Op(BB *to_bb, OP *point, BB *from_bb, OP *op, BOOL before)
{
  Is_True(OP_bb(op) == from_bb, ("op not in from_bb"));
  Is_True(OP_bb(point) == to_bb, ("point not in to_bb"));
  OPS_Remove_Op(&from_bb->ops, op);
  insert_ops(&to_bb->ops, point, op, op, before);
  to_bb->ops.length++;
  setup_ops(to_bb, op, op, 1);
}


void BB_Move_Op_Before(BB *to_bb, OP *point, BB *from_bb, OP *op)
{
  Is_True(OP_bb(op) == from_bb, ("op not in from_bb"));
  Is_True(OP_bb(point) == to_bb, ("point not in to_bb"));
  OPS_Remove_Op(&from_bb->ops, op);
  insert_ops_before(&to_bb->ops, point, op, op);
  to_bb->ops.length++;
  setup_ops(to_bb, op, op, 1);
}

void BB_Move_Op_After(BB *to_bb, OP *point, BB *from_bb, OP *op)
{
  Is_True(OP_bb(op) == from_bb, ("op not in from_bb"));
  Is_True(OP_bb(point) == to_bb, ("point not in to_bb"));
  OPS_Remove_Op(&from_bb->ops, op);
  insert_ops_after(&to_bb->ops, point, op, op);
  to_bb->ops.length++;
  setup_ops(to_bb, op, op, 1);
}


void BB_Move_Op_To_Start(BB *to_bb, BB *from_bb, OP *op)
{
  Is_True(OP_bb(op) == from_bb, ("op not in from_bb"));
  OPS_Remove_Op(&from_bb->ops, op);
  prepend_ops(&to_bb->ops, op, op);
  to_bb->ops.length++;
  setup_ops(to_bb, op, op, 1);
}


void BB_Move_Op_To_End(BB *to_bb, BB *from_bb, OP *op)
{
  Is_True(OP_bb(op) == from_bb, ("op not in from_bb"));
  OPS_Remove_Op(&from_bb->ops, op);
  append_ops(&to_bb->ops, op, op);
  to_bb->ops.length++;
  setup_ops(to_bb, op, op, 1);
}


void BB_Append_All(BB *to_bb, BB *from_bb)
{
  OPS the_ops;

  if (BB_length(from_bb) == 0) return;

  the_ops = from_bb->ops;
  BB_Remove_All(from_bb);
  BB_Append_Ops(to_bb, &the_ops);
}


void BB_Prepend_All (BB *to_bb, BB *from_bb)
{
  OPS the_ops;

  if (BB_length(from_bb) == 0) return;

  the_ops = from_bb->ops;
  BB_Remove_All (from_bb);
  BB_Prepend_Ops (to_bb, &the_ops);
}


OP *BB_Remove_Branch(BB *bb)
{
  OP *last_op;
  OP *br = BB_branch_op(bb);

  if (br) {
    last_op = BB_last_op(bb);
    if (OP_noop(last_op)) BB_Remove_Op(bb, last_op);
    BB_Remove_Op(bb, br);
  }

  return br;
}


void BB_Remove_Op(BB *bb, OP *op)
{
  OP *orig_last_op = BB_last_op(bb);

  OPS_Remove_Op(&bb->ops, op);
  op->bb = NULL;

#ifdef TARG_SL
  if (BB_has_tag(bb) && BB_length(bb)==0) {
    OP *nop = Mk_OP( TOP_nop );
    BB_Append_Op(bb, nop);
  }
#endif
}


void BB_Remove_Ops(BB *bb, OPS *ops)
{
  OP *op;

  if (OPS_first(ops) == NULL) return;

  OPS_Remove_Ops(&bb->ops, ops);

  FOR_ALL_OPS_OPs(ops, op) op->bb = NULL;
}

TOP Remap_topcode(OP *op, TOP opr);

void BB_Remove_All(BB *bb)
{
  BB_Remove_Ops(bb, &bb->ops);
  BB_next_op_map_idx(bb) = 0;
}

/* ====================================================================
 *
 * Mk_OP / Mk_VarOP
 *
 * Make new OP records.
 *
 * ====================================================================
 */
OP *
Mk_OP(TOP opr, ...)
{
  va_list ap;
  INT i;
  INT results;
  INT opnds;
  OP *op;

#ifdef TARG_X8664
  opr = Remap_topcode(op, opr);
#endif

  results = TOP_fixed_results(opr);
  opnds = TOP_fixed_opnds(opr);

#ifdef TARG_IA64
  op = New_OP(results, opnds, CGTARG_Max_Number_of_Hidden_Opnd(opr));
#else
  op = New_OP(results, opnds);
#endif

  FmtAssert(!TOP_is_var_opnds(opr), ("Mk_OP not allowed with variable operands"));
  FmtAssert(opr != TOP_UNDEFINED,   ("Mk_OP not allowed with TOP_UNDEFINED"));

  Set_OP_code(op, opr);

  va_start(ap, opr);

  for (i = 0; i < results; ++i) {
    TN *result = va_arg(ap, TN *);
    Set_OP_result(op, i, result);
  }
  if (TOP_is_defs_fpu_int(opr)) Set_TN_is_fpu_int(OP_result(op, 0));

  for (i = 0; i < opnds; ++i) {
    TN *opnd = va_arg(ap, TN *);
    Set_OP_opnd(op, i, opnd);
  }

  va_end(ap);

  CGTARG_Init_OP_cond_def_kind(op);

#if Is_True_On
#ifdef TARG_X8664
  // Make sure no 64-bit int operations for n32 will be generated.
  if( Is_Target_32bit() &&
      !OP_dummy( op )   &&
      !OP_simulated(op) &&
      !OP_cond_move(op) &&
      OP_code(op) != TOP_leave ){

    for( int i = 0; i < OP_results(op); i++ ){
      TN* tn = OP_result( op, i );
      if( tn != NULL && 
	  OP_result_size( op, i ) > 32 &&
	  TN_register_class(tn) == ISA_REGISTER_CLASS_integer ){
	FmtAssert( FALSE, ("i386 does not support 64-bit operation -- %s",
			   TOP_Name(opr) ) );
      }
    }

    const int base_idx = OP_find_opnd_use( op, OU_base );
    const int index_idx = OP_find_opnd_use( op, OU_index );
    const int target_idx = OP_find_opnd_use( op, OU_target );

    for( int i = 0; i < OP_opnds(op); i++ ){
      TN* tn = OP_opnd( op, i );
      if( tn != NULL     &&
	  i != base_idx  &&
	  i != index_idx &&
	  i != target_idx&&
	  OP_opnd_size( op, i ) > 32 &&
	  TN_register_class(tn) == ISA_REGISTER_CLASS_integer ){
	FmtAssert( FALSE, ("i386 does not support 64-bit operation -- %s",
			   TOP_Name(opr) ) );
      }
    }
  }
#endif // TARG_X8664
#endif // Is_True_On

#ifdef TARG_X8664
  if ( TOP_is_vector_high_loadstore ( OP_code ( op ) ) )
    Set_OP_cond_def_kind(op, OP_ALWAYS_COND_DEF);
#endif
  return op;
}

OP *
Mk_VarOP(TOP opr, INT results, INT opnds, TN **res_tn, TN **opnd_tn)
{
  if (results != TOP_fixed_results(opr)) {
    FmtAssert(TOP_is_var_opnds(opr) && results > TOP_fixed_results(opr),
	      ("%d is not enough results for %s", results, TOP_Name(opr)));
  }
  if (opnds != TOP_fixed_opnds(opr)) {
    FmtAssert(TOP_is_var_opnds(opr) && opnds > TOP_fixed_opnds(opr),
	      ("%d is not enough operands for %s", opnds, TOP_Name(opr)));
  }

  INT i;
#ifdef TARG_IA64
  OP *op = New_OP(results, opnds, CGTARG_Max_Number_of_Hidden_Opnd(opr));
#else
  OP *op = New_OP(results, opnds);
#endif

  FmtAssert(opr != TOP_UNDEFINED,   ("Mk_VarOP not allowed with TOP_UNDEFINED"));
  Set_OP_code(op, opr);

  for (i = 0; i < results; ++i) Set_OP_result(op, i, res_tn[i]);
  if (TOP_is_defs_fpu_int(opr)) Set_TN_is_fpu_int(res_tn[0]);

  for (i = 0; i < opnds; ++i) Set_OP_opnd(op, i, opnd_tn[i]);

  CGTARG_Init_OP_cond_def_kind(op);

  return op;
}

/* ====================================================================
 *
 * Print_OP / Print_OP_No_SrcLine / Print_OPs / Print_OPS
 *
 * Print an OP (or OP list) to the trace file.  These shouldn't be
 * inlined since they're useful for debugging and don't affect user
 * compile-time performance.
 *
 * ====================================================================
 */

void Print_OP_No_SrcLine(const OP *op)
{
  INT16 i;
  WN *wn;
  BOOL cg_loop_op = Is_CG_LOOP_Op(op);
#ifdef TARG_IA64
  if (OP_start_bundle(op)) fprintf( TFile, " }\n{\n");
  fprintf (TFile, "[%3d] ", OP_map_idx(op));
  fprintf (TFile, "[%4d] ", Srcpos_To_Line(OP_srcpos(op)));
#elif defined(TARG_X8664) 
  fprintf (TFile, "[%4d,%2d] ", Srcpos_To_Line(OP_srcpos(op)), OP_scycle(op) );
#else
  fprintf (TFile, "[%4d] ", Srcpos_To_Line(OP_srcpos(op)));
#endif
  if (OP_has_tag(op)) {
	LABEL_IDX tag = Get_OP_Tag(op);
	fprintf (TFile, "<tag %s>: ", LABEL_name(tag));
  }
  for (i = 0; i < OP_results(op); i++) {
    Print_TN(OP_result(op,i),FALSE);
    fprintf(TFile, " ");
  }
  fprintf(TFile, ":- ");
  fprintf(TFile, "%s ", TOP_Name(OP_code(op)));
#ifdef TARG_IA64
  if ( OP_variant(op) != 0 ) {
    fprintf ( TFile, "(%x) ", OP_variant(op));
  }
#endif
  for (i=0; i<OP_opnds(op); i++) {
    TN *tn = OP_opnd(op,i);
    Print_TN(tn,FALSE);
    if ( cg_loop_op ) {
      INT omega = TN_is_symbol(tn) ? OP_restore_omega(op) : OP_omega(op,i);
      if (omega) fprintf(TFile, "[%d]", omega);
    }
    if (OP_Defs_TN(op, tn)) fprintf(TFile, "<defopnd>");
    fprintf(TFile, " ");
  }

#ifdef TARG_SL
  /* print out the extra operands, due to LUT */
  TN_LIST *extra_opnds = op->extra_operand;
  if( extra_opnds )
    fprintf( TFile, " ExtraOpndList: ");
  while( extra_opnds ){
    TN* opnd_tn = TN_LIST_first( extra_opnds );
    Print_TN( opnd_tn, FALSE );
    extra_opnds = TN_LIST_rest( extra_opnds );
  }

  TN_LIST *extra_results = op->extra_result;
  if( extra_results )
    fprintf( TFile, " ExtraResultList: ");
  while( extra_results ){
    TN* res_tn = TN_LIST_first( extra_results );
    Print_TN( res_tn, FALSE );
    extra_results = TN_LIST_rest( extra_results );
  }
#endif

  fprintf(TFile, ";");

  /* print flags */
  // fprintf(TFile," flags 0x%08x ",OP_flags(op));
  if (OP_glue(op)) fprintf (TFile, " glue");
  if (OP_no_alias(op)) fprintf (TFile, " noalias");
  if (OP_copy(op)) fprintf (TFile, " copy");
  if (OP_volatile(op)) fprintf (TFile, " volatile");
  if (OP_side_effects(op)) fprintf (TFile, " side_effects");
  if (OP_hoisted(op)) fprintf (TFile, " hoisted");
  if (OP_cond_def(op)) fprintf (TFile, " cond_def");
  if (OP_end_group(op)) fprintf (TFile, " end_group");
  if (OP_tail_call(op)) fprintf (TFile, " tail_call");
  if (OP_no_move_before_gra(op)) fprintf (TFile, " no_move");
  if (OP_spadjust_plus(op)) fprintf (TFile, " spadjust_plus");
  if (OP_spadjust_minus(op)) fprintf (TFile, " spadjust_minus");
#ifdef TARG_IA64
  if (OP_Scheduled(op)) fprintf (TFile, " scheduled");
  if (OP_start_bundle(op)) fprintf (TFile, " start_bundle");
  if (OP_safe_load(op)) fprintf (TFile, " safe_load");
#endif

  if (wn = Get_WN_From_Memory_OP(op)) {
    char buf[500];
    buf[0] = '\0';
    if (Alias_Manager) Print_alias_info (buf, Alias_Manager, wn);
#ifdef TARG_X8664
    fprintf(TFile, " WN %s", buf);
#else
    fprintf(TFile, " WN=0x%p %s", wn, buf);
#endif
  }
  if (OP_unrolling(op)) {
    UINT16 unr = OP_unrolling(op);
    fprintf(TFile, " %d%s unrolling", unr,
	    unr == 1 ? "st" : unr == 2 ? "nd" : unr == 3 ? "rd" : "th");
  }
  fprintf(TFile, "\n");
}

void Print_OP( const OP *op )
{
  Print_Src_Line (OP_srcpos(op), TFile);
  Print_OP_No_SrcLine(op);
}

void Print_OPs( const OP *op )
{
  for ( ; op; op = OP_next(op))
    Print_OP(op);
}

void Print_OPS( const OPS *ops )
{
  OP *op;
  FOR_ALL_OPS_OPs_FWD(ops, op)
    Print_OP(op);
}

void Print_OPs_No_SrcLines( const OP *op )
{
  for ( ; op; op = OP_next(op))
    Print_OP_No_SrcLine(op);
}

void Print_OPS_No_SrcLines( const OPS *ops )
{
  OP *op;
  FOR_ALL_OPS_OPs_FWD(ops, op)
    Print_OP_No_SrcLine(op);
}



/* ====================================================================
 *
 * OP_Defs_Reg
 *
 * See interface description.
 *
 * ====================================================================
 */

BOOL
OP_Defs_Reg(const OP *op, ISA_REGISTER_CLASS cl, REGISTER reg)
{
  register INT num;

  for ( num = 0; num < OP_results(op); num++ ) {
    TN *res_tn = OP_result(op,num);
    if (TN_is_register(res_tn)) {
      if (TN_register_class(res_tn) == cl && TN_register(res_tn) == reg ) {
	return TRUE;
      }
    }
  }

  /* if we made it here, we must not have found it */
  return FALSE;
}

/* ====================================================================
 *
 * OP_Refs_Reg
 *
 * See interface description.
 *
 * ====================================================================
 */

BOOL
OP_Refs_Reg(const OP *op, ISA_REGISTER_CLASS cl, REGISTER reg)
{
  register INT num;

  for ( num = 0; num < OP_opnds(op); num++ ) {
    TN *opnd_tn = OP_opnd(op,num);
    if (TN_is_register(opnd_tn)) {
      if (TN_register_class(opnd_tn) == cl && TN_register(opnd_tn) == reg ) {
	return TRUE;
      }
    }
  }

#ifdef KEY
  if( OP_cond_def( op ) ){
    for ( num = 0; num < OP_results(op); num++ ) {
      TN* result_tn = OP_result( op, num );
      if (TN_is_register(result_tn)          &&
	  TN_register_class(result_tn) == cl &&
	  TN_register(result_tn) == reg ) {
	return TRUE;
      }      
    }
  }
#endif

  /* if we made it here, we must not have found it */
  return FALSE;
}


/* ====================================================================
 *
 * OP_Defs_TN
 *
 * See interface description.
 *
 * ====================================================================
 */

BOOL
OP_Defs_TN(const OP *op, const struct tn *res)
{
  register INT num;

  for ( num = 0; num < OP_results(op); num++ ) {
    if ( OP_result(op,num) == res ) {
      return( TRUE );
    }
  }

  /* if we made it here, we must not have found it */
  return( FALSE );
}


/* ====================================================================
 *
 * OP_Refs_TN
 *
 * See interface description.
 *
 * ====================================================================
 */

BOOL
OP_Refs_TN( const OP *op, const struct tn *opnd )
{
  register INT16 num;

  for ( num = 0; num < OP_opnds(op); num++ ) {
    if ( OP_opnd(op,num) == opnd ) {
      return( TRUE );
    }
  }

#ifdef KEY
  if( OP_cond_def( op ) ){
    for ( num = 0; num < OP_results(op); num++ ) {
      if( OP_result( op, num ) == opnd )
	return TRUE;
    }
  }
#endif

  /* if we made it here, we must not have found it */
  return( FALSE );
}


/* ====================================================================
 *
 * OP_Real_Ops - How many ops does this op really represent, i.e. will
 * be emitted.
 *
 * ====================================================================
 */

INT16
OP_Real_Ops( const OP *op )
{
  if ( op == NULL || OP_dummy(op) ) {
    return 0;
  }
  else if ( OP_simulated(op) ) {
    return Simulated_Op_Real_Ops (op);
  }
  return 1;
}


/* ====================================================================
 *
 * OP_Real_Inst_Words - How many instruction words does this op really 
 * represent, i.e. will be emitted.
 *
 * ====================================================================
 */

INT
OP_Real_Inst_Words( const OP *op )
{
  if ( op == NULL || OP_dummy(op) ) {
    return 0;
  }
  else if ( OP_simulated(op) ) {
    return Simulated_Op_Real_Inst_Words (op);
  }

  return OP_inst_words(op);
}


/* ====================================================================
 *
 * OP_Is_Float_Mem - Is OP a floating point memory operation?
 *
 * ====================================================================
 */

BOOL
OP_Is_Float_Mem( const OP *op )
{
  return (OP_load(op) && TN_is_float(OP_result(op, 0))) ||
	 (OP_store(op) && TN_is_float(OP_opnd(op, 0)));
}

/* ====================================================================
 *
 * OP_Alloca_Barrier - Is OP a alloca barrier node with alias info?
 *
 * ====================================================================
 */

BOOL
OP_Alloca_Barrier(OP *op )
{
  return (OP_code(op) == TOP_spadjust && Get_WN_From_Memory_OP(op));
}

// =======================================================================
// Is_Delay_Slot_Op
// Return TRUE if the <op> is the right type to put into a delay slot of
// <xfer_op>.
// =======================================================================
BOOL 
Is_Delay_Slot_Op (OP *xfer_op, OP *op)
{
  if (op == NULL || OP_xfer(op) || OP_Real_Ops(op) != 1) return FALSE;

  // R10k chip bug workaround: Avoid placing integer mult/div in delay 
  // slots of unconditional branches. (see pv516598) for more details.
  if (xfer_op && OP_uncond(xfer_op) &&
      (OP_imul(op) || OP_idiv(op))) return FALSE;
  
  // TODO: do we need the following restriction ?
  if (OP_has_hazard(op) || OP_has_implicit_interactions(op))
    return FALSE;
  return TRUE;
}



// Debugging routine
void dump_op (const OP *op)
{
   FILE *f;
   f = TFile;
   Set_Trace_File_internal(stdout);
   Print_OP_No_SrcLine(op);
   Set_Trace_File_internal(f);
}

void dump_ops (const OPS *ops)
{
   FILE *f;
   f = TFile;
   Set_Trace_File_internal(stdout);
   Print_OPS(ops);
   Set_Trace_File_internal(f);
}

/* ====================================================================
 *
 * OP_cond_def
 *
 * Return TRUE if the OP conditionally modifies some of the results.
 *
 * ====================================================================
 */

BOOL OP_cond_def(const OP *op) 
{
  return OP_cond_def_kind(op) == OP_ALWAYS_COND_DEF ||
    ((OP_cond_def_kind(op) == OP_PREDICATED_DEF) &&
     !TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND)));
}

/* ====================================================================
 *
 * OP_has_implicit_interactions
 *
 * Return TRUE if the OP has some implicit interaction properties with
 * other OPs in a non-obvious way.
 *
 * ====================================================================
 */

BOOL OP_has_implicit_interactions(OP *op) 
{
  if (OP_volatile(op) || OP_side_effects(op))
    return TRUE;

  INT i;
  for (i = 0; i < OP_opnds(op); i++) {
    TN *opnd_tn = OP_opnd(op, i);
    if (TN_is_tag(opnd_tn)) return TRUE;
  }

  return FALSE;
}

/* ====================================================================
 *
 * OP_Base_Offset_TNs
 *
 * Return the base and offset TNs for the given memory OP.
 *
 * ====================================================================
 */
void OP_Base_Offset_TNs(OP *memop, TN **base_tn, TN **offset_tn)
{
#ifdef TARG_X8664
  Is_True(OP_load(memop) || OP_load_exe(memop) || OP_store(memop), ("not a load or store"));
#else
  Is_True(OP_load(memop) || OP_store(memop), ("not a load or store"));
#endif

  INT offset_num = OP_find_opnd_use (memop, OU_offset);
  INT base_num   = OP_find_opnd_use (memop, OU_base);

  *offset_tn = NULL;

  *base_tn = base_num >= 0 ? OP_opnd(memop, base_num) : NULL;

  // <offset> TNs are not part of <memop>. Find the definining OP_iadd
  // instruction which sets the offset and matches the base_tn.

  if (offset_num < 0) {
    if( *base_tn ){
      DEF_KIND kind;
      OP *defop = TN_Reaching_Value_At_Op(*base_tn, memop, &kind, TRUE);
      if (defop && OP_iadd(defop) && kind == VAL_KNOWN) {
        TN *defop_offset_tn = OP_opnd(defop, 1);
        TN *defop_base_tn = OP_opnd(defop, 2);
        if (defop_base_tn == *base_tn && TN_has_value(defop_base_tn)) {
          *offset_tn = defop_offset_tn;
        }
      }
    }
  } else {
    *offset_tn = OP_opnd(memop, offset_num);
  }
}


/* Is <op> a copy from a callee-saves register into its save-TN?
 */
BOOL
OP_Is_Copy_To_Save_TN(const OP* op)
{
  INT i;

  for ( i = OP_results(op) - 1; i >= 0; --i ) {
    TN* tn = OP_result(op,i);
    if ( TN_is_save_reg(tn)) return TRUE;
  }

  return FALSE;
}

/*  Is <op> a copy to a callee-saves register from its save-TN?
 */
BOOL
OP_Is_Copy_From_Save_TN( const OP* op )
{
  INT i;

  // You'd think there'd be a better way than groveling through the operands,
  // but short of marking these when we make them, this seems to be the most
  // bullet-proof

  for ( i = OP_results(op) - 1; i >= 0; --i ) {
    if ( TN_is_dedicated(OP_result(op,i)) ) break;
  }
  if ( i < 0 ) return FALSE;

  for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
    TN* tn = OP_opnd(op,i);
    if ( TN_Is_Allocatable(tn) && TN_is_save_reg(tn))
      return TRUE;
  }

  return FALSE;
}


#ifdef TARG_IA64
/* ====================================================================
 * OP_ld_st_unat
 *
 * return TRUE if op load/store a unat bit 
 *
 * ====================================================================
 */
BOOL OP_ld_st_unat(OP *op)
{
    mTOP opcode = OP_code(op);
    if(opcode == TOP_mov_f_ar || opcode == TOP_mov_t_ar_r  ||
       opcode == TOP_mov_f_ar_m || opcode == TOP_mov_t_ar_r_m)
    {
        for(INT i=0; i<OP_results(op); i++)
        {
            if(OP_result(op,i) == 
               Build_Dedicated_TN(ISA_REGISTER_CLASS_application,(REGISTER)(REGISTER_MIN + 36),0))
                 return TRUE;
        }  
        for(INT i=0;i<OP_opnds(op); i++)
        {
            if(OP_opnd(op,i) == 
               Build_Dedicated_TN(ISA_REGISTER_CLASS_application,(REGISTER)(REGISTER_MIN + 36),0))
                return TRUE;
        }
    }
    return FALSE;
}

BOOL OP_def_return_value(OP* op)
{
    for (INT i = OP_results(op) - 1 ; i >= 0 ; i--) {
        mTN_NUM n = TN_number(OP_result(op,i));
        if ((n >= First_Int_Preg_Return_Offset && 
             n <= Last_Int_Preg_Return_Offset)       ||
            (n >= First_Float_Preg_Return_Offset &&
             n <= Last_Float_Preg_Return_Offset)) {
            return TRUE;    
        }
    }         
    return FALSE;
}        

BOOL OP_use_return_value (OP* op) {
    for (INT i = 0; i < OP_opnds (op); i++) {
        TN* opnd = OP_opnd(op, i);
        if (TN_is_constant(opnd)) { continue; }

        mTN_NUM n = TN_number(opnd);
        if ((n >= First_Int_Preg_Return_Offset && 
             n <= Last_Int_Preg_Return_Offset)       ||
            (n >= First_Float_Preg_Return_Offset &&
             n <= Last_Float_Preg_Return_Offset)) {
            return TRUE;    
        }
    }         
    return FALSE;
}

/* Add hidden operands to given op. All hidden operands should be added at one time.
 */
void
Add_Hidden_Operands (OP* op, const vector<TN*>& hopnds) {
  if (hopnds.size () == 0) return;

  INT t = CGTARG_Max_Number_of_Hidden_Opnd (OP_code(op));
  Is_True (t > 0,  ("Op does not have hidden openrands"));
  Is_True (hopnds.size() <= t, ("Expected at most %d hidden operands"));
  Is_True (OP_hidden_opnds(op) == 0, ("Hidden operands are added once"));

  // leave room for hidden operands   
  if (OP_results(op) != 0) {
    INT32 from_idx = op->opnds+op->results - 1;
    INT32 to_idx = from_idx + hopnds.size();
    for (INT32 count = OP_results(op); count > 0; count--) {
      op->res_opnd[to_idx--] = op->res_opnd[from_idx--];
    }
  }

  // now interpose the hidden operands between operands and results.
  for (INT32 i = 0; i < hopnds.size (); ++i) {
    op->res_opnd[op->opnds+i] = hopnds[i];
  }

  op->hidden_opnds = hopnds.size();
  op->opnds += hopnds.size ();
}

#endif // TARG_IA64


#ifdef KEY
/* ====================================================================
 *
 * TN_Pair_In_OP
 *
 * See interface description.
 *
 * ====================================================================
 */

BOOL 
TN_Pair_In_OP(OP* op, struct tn *tn_res, struct tn *tn_opnd) 
{
  INT i;
  for (i = 0; i < OP_results(op); i++) {
    if (tn_res == OP_result(op,i)) {
      break; 
    }
  }
  if (i == OP_results(op)) {
    TN_size(tn_res);	// TK debug
    // If tn_res has an assigned register, check if it matches a result.  (This
    // changes the semantics of TN_Pair_In_OP, but that's ok since the only
    // user of TN_Pair_In_OP is LRA, which wants this check.)  Bug 9489.
    BOOL result_match = FALSE;
    if (TN_register(tn_res) != REGISTER_UNDEFINED) {
      for (int j = 0; j < OP_results(op); j++) {
        TN *res = OP_result(op, j);
	if (TN_register_and_class(res) == TN_register_and_class(tn_res)) {
	  result_match = TRUE;
	}
      }
    }
    if (!result_match)
      return FALSE;
  }
  for (i = 0; i < OP_opnds(op); i++) {
    if (tn_opnd == OP_opnd(op,i)) {
      return TRUE; 
    }
  }
  return FALSE;
}

/* ====================================================================
 *
 * TN_Resnum_In_OP
 *
 * See interface description.
 *
 * ====================================================================
 */

INT 
TN_Resnum_In_OP (OP* op, struct tn *tn, BOOL match_assigned_reg) 
{
  for (INT i = 0; i < OP_results(op); i++) {
    TN *res = OP_result(op, i);
    if (tn == res) {
      return i;
    }

    if (match_assigned_reg &&
	TN_register(res) != REGISTER_UNDEFINED &&
	TN_register_and_class(res) == TN_register_and_class(tn)) {
      return i;
    }
  }
  FmtAssert (FALSE,
             ("TN_resnum_in_OP: Could not find <tn> in results list\n"));
  return -1;
}
#endif

#if defined(TARG_SL) || defined(TARG_MIPS)
#include "targ_sim.h"
BOOL OP_def_return_value(OP* op)
{
    for (INT i = OP_results(op) - 1 ; i >= 0 ; i--) {
        mTN_NUM n = TN_number(OP_result(op,i));
        if ((n >= First_Int_Preg_Return_Offset && 
             n <= Last_Int_Preg_Return_Offset)       ||
            (n >= First_Float_Preg_Return_Offset &&
             n <= Last_Float_Preg_Return_Offset)) {
            return TRUE;    
        }
    }         
    return FALSE;
}        

#endif

#ifdef TARG_X8664
typedef struct {
  TOP legacy_mode;
  TOP vex_mode;
} Top_Trans_Group;

static Top_Trans_Group Top_Leg_To_Vex_Mode_Group[TOP_count+1];

static Top_Trans_Group Top_SSE_To_Vex_Mode_Group_Table[] = {
    // LEGACY MODE          VEX MODE
    {TOP_faddsub128v32,     TOP_vfaddsub128v32},
    {TOP_fhadd128v32,       TOP_vfhadd128v32},
    {TOP_fhsub128v32,       TOP_vfhsub128v32},
    {TOP_faddsub128v64,     TOP_vfaddsub128v64},
    {TOP_fhadd128v64,       TOP_vfhadd128v64},
    {TOP_fhsub128v64,       TOP_vfhsub128v64},
    {TOP_faddsubx128v32,    TOP_vfaddsubx128v32},
    {TOP_fhaddx128v32,      TOP_vfhaddx128v32},
    {TOP_fhsubx128v32,      TOP_vfhsubx128v32},
    {TOP_faddsubx128v64,    TOP_vfaddsubx128v64},
    {TOP_fhaddx128v64,      TOP_vfhaddx128v64},
    {TOP_fhsubx128v64,      TOP_vfhsubx128v64},
    {TOP_faddsubxx128v32,   TOP_vfaddsubxx128v32},
    {TOP_fhaddxx128v32,     TOP_vfhaddxx128v32},
    {TOP_fhsubxx128v32,     TOP_vfhsubxx128v32},
    {TOP_faddsubxx128v64,   TOP_vfaddsubxx128v64},
    {TOP_fhaddxx128v64,     TOP_vfhaddxx128v64},
    {TOP_fhsubxx128v64,     TOP_vfhsubxx128v64},
    {TOP_faddsubxxx128v32,  TOP_vfaddsubxxx128v32},
    {TOP_fhaddxxx128v32,    TOP_vfhaddxxx128v32},
    {TOP_fhsubxxx128v32,    TOP_vfhsubxxx128v32},
    {TOP_faddsubxxx128v64,  TOP_vfaddsubxxx128v64},
    {TOP_fhaddxxx128v64,    TOP_vfhaddxxx128v64},
    {TOP_fhsubxxx128v64,    TOP_vfhsubxxx128v64},
    {TOP_addx128v8,         TOP_vaddx128v8},
    {TOP_addx128v16,        TOP_vaddx128v16},
    {TOP_addx128v32,        TOP_vaddx128v32},
    {TOP_addx128v64,        TOP_vaddx128v64},
    {TOP_faddx128v32,       TOP_vfaddx128v32},
    {TOP_faddx128v64,       TOP_vfaddx128v64},
    {TOP_andx128v8,         TOP_vandx128v8},
    {TOP_andx128v16,        TOP_vandx128v16},
    {TOP_andx128v32,        TOP_vandx128v32},
    {TOP_andx128v64,        TOP_vandx128v64},
    {TOP_fandx128v32,       TOP_vfandx128v32},
    {TOP_fandx128v64,       TOP_vfandx128v64},
    {TOP_orx128v8,          TOP_vorx128v8},
    {TOP_orx128v16,         TOP_vorx128v16},
    {TOP_orx128v32,         TOP_vorx128v32},
    {TOP_orx128v64,         TOP_vorx128v64},
    {TOP_forx128v32,        TOP_vforx128v32},
    {TOP_forx128v64,        TOP_vforx128v64},
    {TOP_xorx128v8,         TOP_vxorx128v8},
    {TOP_xorx128v16,        TOP_vxorx128v16},
    {TOP_xorx128v32,        TOP_vxorx128v32},
    {TOP_xorx128v64,        TOP_vxorx128v64},
    {TOP_fxorx128v32,       TOP_vfxorx128v32},
    {TOP_fxorx128v64,       TOP_vfxorx128v64},
    {TOP_fmaxx128v32,       TOP_vfmaxx128v32},
    {TOP_fmaxx128v64,       TOP_vfmaxx128v64},
    {TOP_fminx128v32,       TOP_vfminx128v32},
    {TOP_fminx128v64,       TOP_vfminx128v64},
    {TOP_fdivx128v32,       TOP_vfdivx128v32},
    {TOP_fdivx128v64,       TOP_vfdivx128v64},
    {TOP_fmulx128v32,       TOP_vfmulx128v32},
    {TOP_fmulx128v64,       TOP_vfmulx128v64},
    {TOP_cmpgtx128v8,       TOP_vcmpgtx128v8},
    {TOP_cmpgtx128v16,      TOP_vcmpgtx128v16},
    {TOP_cmpgtx128v32,      TOP_vcmpgtx128v32},
    {TOP_cmpeqx128v8,       TOP_vcmpeqx128v8},
    {TOP_cmpeqx128v16,      TOP_vcmpeqx128v16},
    {TOP_cmpeqx128v32,      TOP_vcmpeqx128v32},
    {TOP_subx128v8,         TOP_vsubx128v8},
    {TOP_subx128v16,        TOP_vsubx128v16},
    {TOP_subx128v32,        TOP_vsubx128v32},
    {TOP_subx128v64,        TOP_vsubx128v64},
    {TOP_fsubx128v32,       TOP_vfsubx128v32},
    {TOP_fsubx128v64,       TOP_vfsubx128v64},
    {TOP_addxx128v8,        TOP_vaddxx128v8},
    {TOP_addxx128v16,       TOP_vaddxx128v16},
    {TOP_addxx128v32,       TOP_vaddxx128v32},
    {TOP_addxx128v64,       TOP_vaddxx128v64},
    {TOP_faddxx128v32,      TOP_vfaddxx128v32},
    {TOP_faddxx128v64,      TOP_vfaddxx128v64},
    {TOP_andxx128v8,        TOP_vandxx128v8},
    {TOP_andxx128v16,       TOP_vandxx128v16},
    {TOP_andxx128v32,       TOP_vandxx128v32},
    {TOP_andxx128v64,       TOP_vandxx128v64},
    {TOP_fandxx128v32,      TOP_vfandxx128v32},
    {TOP_fandxx128v64,      TOP_vfandxx128v64},
    {TOP_orxx128v8,         TOP_vorxx128v8},
    {TOP_orxx128v16,        TOP_vorxx128v16},
    {TOP_orxx128v32,        TOP_vorxx128v32},
    {TOP_orxx128v64,        TOP_vorxx128v64},
    {TOP_forxx128v32,       TOP_vforxx128v32},
    {TOP_forxx128v64,       TOP_vforxx128v64},
    {TOP_xorxx128v8,        TOP_vxorxx128v8},
    {TOP_xorxx128v16,       TOP_vxorxx128v16},
    {TOP_xorxx128v32,       TOP_vxorxx128v32},
    {TOP_xorxx128v64,       TOP_vxorxx128v64},
    {TOP_fxorxx128v32,      TOP_vfxorxx128v32},
    {TOP_fxorxx128v64,      TOP_vfxorxx128v64},
    {TOP_fmaxxx128v32,      TOP_vfmaxxx128v32},
    {TOP_fmaxxx128v64,      TOP_vfmaxxx128v64},
    {TOP_fminxx128v32,      TOP_vfminxx128v32},
    {TOP_fminxx128v64,      TOP_vfminxx128v64},
    {TOP_fdivxx128v32,      TOP_vfdivxx128v32},
    {TOP_fdivxx128v64,      TOP_vfdivxx128v64},
    {TOP_fmulxx128v32,      TOP_vfmulxx128v32},
    {TOP_fmulxx128v64,      TOP_vfmulxx128v64},
    {TOP_cmpgtxx128v8,      TOP_vcmpgtxx128v8},
    {TOP_cmpgtxx128v16,     TOP_vcmpgtxx128v16},
    {TOP_cmpgtxx128v32,     TOP_vcmpgtxx128v32},
    {TOP_cmpeqxx128v8,      TOP_vcmpeqxx128v8},
    {TOP_cmpeqxx128v16,     TOP_vcmpeqxx128v16},
    {TOP_cmpeqxx128v32,     TOP_vcmpeqxx128v32},
    {TOP_subxx128v8,        TOP_vsubxx128v8},
    {TOP_subxx128v16,       TOP_vsubxx128v16},
    {TOP_subxx128v32,       TOP_vsubxx128v32},
    {TOP_subxx128v64,       TOP_vsubxx128v64},
    {TOP_fsubxx128v32,      TOP_vfsubxx128v32},
    {TOP_fsubxx128v64,      TOP_vfsubxx128v64},
    {TOP_addxxx128v8,       TOP_vaddxxx128v8},
    {TOP_addxxx128v16,      TOP_vaddxxx128v16},
    {TOP_addxxx128v32,      TOP_vaddxxx128v32},
    {TOP_addxxx128v64,      TOP_vaddxxx128v64},
    {TOP_faddxxx128v32,     TOP_vfaddxxx128v32},
    {TOP_faddxxx128v64,     TOP_vfaddxxx128v64},
    {TOP_andxxx128v8,       TOP_vandxxx128v8},
    {TOP_andxxx128v16,      TOP_vandxxx128v16},
    {TOP_andxxx128v32,      TOP_vandxxx128v32},
    {TOP_andxxx128v64,      TOP_vandxxx128v64},
    {TOP_fandxxx128v32,     TOP_vfandxxx128v32},
    {TOP_fandxxx128v64,     TOP_vfandxxx128v64},
    {TOP_orxxx128v8,        TOP_vorxxx128v8},
    {TOP_orxxx128v16,       TOP_vorxxx128v16},
    {TOP_orxxx128v32,       TOP_vorxxx128v32},
    {TOP_orxxx128v64,       TOP_vorxxx128v64},
    {TOP_forxxx128v32,      TOP_vforxxx128v32},
    {TOP_forxxx128v64,      TOP_vforxxx128v64},
    {TOP_xorxxx128v8,       TOP_vxorxxx128v8},
    {TOP_xorxxx128v16,      TOP_vxorxxx128v16},
    {TOP_xorxxx128v32,      TOP_vxorxxx128v32},
    {TOP_xorxxx128v64,      TOP_vxorxxx128v64},
    {TOP_fxorxxx128v32,     TOP_vfxorxxx128v32},
    {TOP_fxorxxx128v64,     TOP_vfxorxxx128v64},
    {TOP_fmaxxxx128v32,     TOP_vfmaxxxx128v32},
    {TOP_fmaxxxx128v64,     TOP_vfmaxxxx128v64},
    {TOP_fminxxx128v32,     TOP_vfminxxx128v32},
    {TOP_fminxxx128v64,     TOP_vfminxxx128v64},
    {TOP_fdivxxx128v32,     TOP_vfdivxxx128v32},
    {TOP_fdivxxx128v64,     TOP_vfdivxxx128v64},
    {TOP_fmulxxx128v32,     TOP_vfmulxxx128v32},
    {TOP_fmulxxx128v64,     TOP_vfmulxxx128v64},
    {TOP_cmpgtxxx128v8,     TOP_vcmpgtxxx128v8},
    {TOP_cmpgtxxx128v16,    TOP_vcmpgtxxx128v16},
    {TOP_cmpgtxxx128v32,    TOP_vcmpgtxxx128v32},
    {TOP_cmpeqxxx128v8,     TOP_vcmpeqxxx128v8},
    {TOP_cmpeqxxx128v16,    TOP_vcmpeqxxx128v16},
    {TOP_cmpeqxxx128v32,    TOP_vcmpeqxxx128v32},
    {TOP_subxxx128v8,       TOP_vsubxxx128v8},
    {TOP_subxxx128v16,      TOP_vsubxxx128v16},
    {TOP_subxxx128v32,      TOP_vsubxxx128v32},
    {TOP_subxxx128v64,      TOP_vsubxxx128v64},
    {TOP_fsubxxx128v32,     TOP_vfsubxxx128v32},
    {TOP_fsubxxx128v64,     TOP_vfsubxxx128v64},
    {TOP_mul128v16,         TOP_vmul128v16},
    {TOP_add128v8,          TOP_vadd128v8},
    {TOP_add128v16,         TOP_vadd128v16},
    {TOP_add128v32,         TOP_vadd128v32},
    {TOP_add128v64,         TOP_vadd128v64},
    {TOP_fadd128v32,        TOP_vfadd128v32},
    {TOP_fadd128v64,        TOP_vfadd128v64},
    {TOP_and128v8,          TOP_vand128v8},
    {TOP_and128v16,         TOP_vand128v16},
    {TOP_and128v32,         TOP_vand128v32},
    {TOP_and128v64,         TOP_vand128v64},
    {TOP_fand128v32,        TOP_vfand128v32},
    {TOP_fand128v64,        TOP_vfand128v64},
    {TOP_or128v8,           TOP_vor128v8},
    {TOP_or128v16,          TOP_vor128v16},
    {TOP_or128v32,          TOP_vor128v32},
    {TOP_or128v64,          TOP_vor128v64},
    {TOP_for128v32,         TOP_vfor128v32},
    {TOP_for128v64,         TOP_vfor128v64},
    {TOP_xor128v8,          TOP_vxor128v8},
    {TOP_xor128v16,         TOP_vxor128v16},
    {TOP_xor128v32,         TOP_vxor128v32},
    {TOP_xor128v64,         TOP_vxor128v64},
    {TOP_pxor,              TOP_vxor128v8},
    {TOP_fxor128v32,        TOP_vfxor128v32},
    {TOP_fxor128v64,        TOP_vfxor128v64},
    {TOP_andps,             TOP_vandps},
    {TOP_andpd,             TOP_vandpd},
    {TOP_xorps,             TOP_vxorps},
    {TOP_xorpd,             TOP_vxorpd},
    {TOP_orps,              TOP_vorps},
    {TOP_orpd,              TOP_vorpd},
    {TOP_fmax128v32,        TOP_vfmax128v32},
    {TOP_fmax128v64,        TOP_vfmax128v64},
    {TOP_fmin128v32,        TOP_vfmin128v32},
    {TOP_fmin128v64,        TOP_vfmin128v64},
    {TOP_fdiv128v32,        TOP_vfdiv128v32},
    {TOP_fdiv128v64,        TOP_vfdiv128v64},
    {TOP_fmul128v32,        TOP_vfmul128v32},
    {TOP_fmul128v64,        TOP_vfmul128v64},
    {TOP_cmpgt128v8,        TOP_vcmpgt128v8},
    {TOP_cmpgt128v16,       TOP_vcmpgt128v16},
    {TOP_cmpgt128v32,       TOP_vcmpgt128v32},
    {TOP_cmpeq128v8,        TOP_vcmpeq128v8},
    {TOP_cmpeq128v16,       TOP_vcmpeq128v16},
    {TOP_cmpeq128v32,       TOP_vcmpeq128v32},
    {TOP_frcp128v32,        TOP_vfrcp128v32},
    {TOP_fsqrt128v32,       TOP_vfsqrt128v32},
    {TOP_frsqrt128v32,      TOP_vfrsqrt128v32},
    {TOP_fsqrt128v64,       TOP_vfsqrt128v64},
    {TOP_addsd,             TOP_vfaddsd},
    {TOP_addss,             TOP_vfaddss},
    {TOP_addxsd,            TOP_vfaddxsd},
    {TOP_addxss,            TOP_vfaddxss},
    {TOP_addxxsd,           TOP_vfaddxxsd},
    {TOP_addxxss,           TOP_vfaddxxss},
    {TOP_addxxxsd,          TOP_vfaddxxxsd},
    {TOP_addxxxss,          TOP_vfaddxxxss},
    {TOP_comisd,            TOP_vcomisd},
    {TOP_comixsd,           TOP_vcomixsd},
    {TOP_comixxsd,          TOP_vcomixxsd},
    {TOP_comixxxsd,         TOP_vcomixxxsd},
    {TOP_comiss,            TOP_vcomiss},
    {TOP_comixss,           TOP_vcomixss},
    {TOP_comixxss,          TOP_vcomixxss},
    {TOP_comixxxss,         TOP_vcomixxxss},
    {TOP_cvtdq2pd,          TOP_vcvtdq2pd},
    {TOP_cvtdq2ps,          TOP_vcvtdq2ps},
    {TOP_cvtps2pd,          TOP_vcvtps2pd},
    {TOP_cvtpd2ps,          TOP_vcvtpd2ps},
    {TOP_cvtss2si,          TOP_vcvtss2si},
    {TOP_cvtsd2si,          TOP_vcvtsd2si},
    {TOP_cvtss2siq,         TOP_vcvtss2siq},
    {TOP_cvtsd2siq,         TOP_vcvtsd2siq},
    {TOP_cvttss2si,         TOP_vcvttss2si},
    {TOP_cvttsd2si,         TOP_vcvttsd2si},
    {TOP_cvttss2siq,        TOP_vcvttss2siq},
    {TOP_cvttsd2siq,        TOP_vcvttsd2siq},
    {TOP_cvtps2dq,          TOP_vcvtps2dq},
    {TOP_cvttps2dq,         TOP_vcvttps2dq},
    {TOP_cvtpd2dq,          TOP_vcvtpd2dq},
    {TOP_cvttpd2dq,         TOP_vcvttpd2dq},
    {TOP_cvtsi2sd,          TOP_vcvtsi2sd},
    {TOP_cvtsi2ss,          TOP_vcvtsi2ss},
    {TOP_cvtsi2sdq,         TOP_vcvtsi2sdq},
    {TOP_cvtsi2ssq,         TOP_vcvtsi2ssq},
    {TOP_cvtss2sd,          TOP_vcvtss2sd},
    {TOP_cvtsd2ss,          TOP_vcvtsd2ss},
    {TOP_cvtsd2ss_x,        TOP_vcvtsd2ssx},
    {TOP_cvtsd2ss_xx,       TOP_vcvtsd2ssxx},
    {TOP_cvtsd2ss_xxx,      TOP_vcvtsd2ssxxx},
    {TOP_cvtsi2sd_x,        TOP_vcvtsi2sdx},
    {TOP_cvtsi2sd_xx,       TOP_vcvtsi2sdxx},
    {TOP_cvtsi2sd_xxx,      TOP_vcvtsi2sdxxx},
    {TOP_cvtsi2ss_x,        TOP_vcvtsi2ssx},
    {TOP_cvtsi2ss_xx,       TOP_vcvtsi2ssxx},
    {TOP_cvtsi2ss_xxx,      TOP_vcvtsi2ssxxx},
    {TOP_cvtsi2sdq_x,       TOP_vcvtsi2sdqx},
    {TOP_cvtsi2sdq_xx,      TOP_vcvtsi2sdqxx},
    {TOP_cvtsi2sdq_xxx,     TOP_vcvtsi2sdqxxx},
    {TOP_cvtsi2ssq_x,       TOP_vcvtsi2ssqx},
    {TOP_cvtsi2ssq_xx,      TOP_vcvtsi2ssqxx},
    {TOP_cvtsi2ssq_xxx,     TOP_vcvtsi2ssqxxx},
    {TOP_cvtdq2pd_x,        TOP_vcvtdq2pdx},
    {TOP_cvtdq2ps_x,        TOP_vcvtdq2psx},
    {TOP_cvtps2pd_x,        TOP_vcvtps2pdx},
    {TOP_cvtpd2ps_x,        TOP_vcvtpd2psx},
    {TOP_cvtps2dq_x,        TOP_vcvtps2dqx},
    {TOP_cvttps2dq_x,       TOP_vcvttps2dqx},
    {TOP_cvttpd2dq_x,       TOP_vcvttpd2dqx},
    {TOP_cvtdq2pd_xx,       TOP_vcvtdq2pdxx},
    {TOP_cvtdq2ps_xx,       TOP_vcvtdq2psxx},
    {TOP_cvtps2pd_xx,       TOP_vcvtps2pdxx},
    {TOP_cvtpd2ps_xx,       TOP_vcvtpd2psxx},
    {TOP_cvtps2dq_xx,       TOP_vcvtps2dqxx},
    {TOP_cvttps2dq_xx,      TOP_vcvttps2dqxx},
    {TOP_cvttpd2dq_xx,      TOP_vcvttpd2dqxx},
    {TOP_cvtdq2pd_xxx,      TOP_vcvtdq2pdxxx},
    {TOP_cvtdq2ps_xxx,      TOP_vcvtdq2psxxx},
    {TOP_cvtps2pd_xxx,      TOP_vcvtps2pdxxx},
    {TOP_cvtpd2ps_xxx,      TOP_vcvtpd2psxxx},
    {TOP_cvtps2dq_xxx,      TOP_vcvtps2dqxxx},
    {TOP_cvttps2dq_xxx,     TOP_vcvttps2dqxxx},
    {TOP_cvttpd2dq_xxx,     TOP_vcvttpd2dqxxx},
    {TOP_divsd,             TOP_vdivsd},
    {TOP_divxsd,            TOP_vdivxsd},
    {TOP_divxxsd,           TOP_vdivxxsd},
    {TOP_divxxxsd,          TOP_vdivxxxsd},
    {TOP_divss,             TOP_vdivss},
    {TOP_divxss,            TOP_vdivxss},
    {TOP_divxxss,           TOP_vdivxxss},
    {TOP_divxxxss,          TOP_vdivxxxss},
    {TOP_sub128v8,          TOP_vsub128v8},
    {TOP_sub128v16,         TOP_vsub128v16},
    {TOP_sub128v32,         TOP_vsub128v32},
    {TOP_sub128v64,         TOP_vsub128v64},
    {TOP_fsub128v32,        TOP_vfsub128v32},
    {TOP_fsub128v64,        TOP_vfsub128v64},
    {TOP_subsd,             TOP_vsubsd},
    {TOP_subss,             TOP_vsubss},
    {TOP_subxsd,            TOP_vsubxsd},
    {TOP_subxss,            TOP_vsubxss},
    {TOP_subxxsd,           TOP_vsubxxsd},
    {TOP_subxxss,           TOP_vsubxxss},
    {TOP_subxxxsd,          TOP_vsubxxxsd},
    {TOP_subxxxss,          TOP_vsubxxxss},
    {TOP_mulsd,             TOP_vmulsd},
    {TOP_mulss,             TOP_vmulss},
    {TOP_mulxsd,            TOP_vmulxsd},
    {TOP_mulxss,            TOP_vmulxss},
    {TOP_mulxxsd,           TOP_vmulxxsd},
    {TOP_mulxxss,           TOP_vmulxxss},
    {TOP_mulxxxsd,          TOP_vmulxxxsd},
    {TOP_mulxxxss,          TOP_vmulxxxss},
    {TOP_movsd,             TOP_vmovsd},
    {TOP_movss,             TOP_vmovss},
    {TOP_movdq,             TOP_vmovdqa},
    {TOP_movapd,            TOP_vmovaps},
    {TOP_movaps,            TOP_vmovaps},
    {TOP_movg2x64,          TOP_vmovg2x64},
    {TOP_movg2x,            TOP_vmovg2x},
    {TOP_movx2g64,          TOP_vmovx2g64},
    {TOP_movx2g,            TOP_vmovx2g},
    {TOP_ldsd,              TOP_vldsd},
    {TOP_ldsdx,             TOP_vldsdx},
    {TOP_ldsdxx,            TOP_vldsdxx},
    {TOP_ldsd_n32,          TOP_vldsd_n32},
    {TOP_ldss,              TOP_vldss},
    {TOP_ldssx,             TOP_vldssx},
    {TOP_ldssxx,            TOP_vldssxx},
    {TOP_ldss_n32,          TOP_vldss_n32},
    {TOP_lddqa,             TOP_vlddqa},
    {TOP_lddqa_n32,         TOP_vlddqa_n32},
    {TOP_stdqa,             TOP_vstdqa},
    {TOP_stdqa_n32,         TOP_vstdqa_n32},
    {TOP_stntpd,            TOP_vstntpd},
    {TOP_stntps,            TOP_vstntps},
    {TOP_lddqu,             TOP_vlddqu},
    {TOP_lddqu_n32,         TOP_vlddqu_n32},
    {TOP_ldlps,             TOP_vldlps},
    {TOP_ldlps_n32,         TOP_vldlps_n32},
    {TOP_ldhps,             TOP_vldhps},
    {TOP_ldhps_n32,         TOP_vldhps_n32},
    {TOP_ldlpd,             TOP_vldlpd},
    {TOP_ldlpd_n32,         TOP_vldlpd_n32},
    {TOP_ldhpd,             TOP_vldhpd},
    {TOP_ldhpd_n32,         TOP_vldhpd_n32},
    {TOP_ldapd,             TOP_vldapd},
    {TOP_ldapd_n32,         TOP_vldapd_n32},
    {TOP_ldaps,             TOP_vldaps},
    {TOP_ldaps_n32,         TOP_vldaps_n32},
    {TOP_ldupd,             TOP_vldupd},
    {TOP_ldupdx,            TOP_vldupdx},
    {TOP_ldupdxx,           TOP_vldupdxx},
    {TOP_ldupd_n32,         TOP_vldupd_n32},
    {TOP_ldups,             TOP_vldups},
    {TOP_ldupsx,             TOP_vldupsx},
    {TOP_ldupsxx,             TOP_vldupsxx},
    {TOP_ldups_n32,         TOP_vldups_n32},
    {TOP_stdqu,             TOP_vstdqu},
    {TOP_stdqu_n32,         TOP_vstdqu_n32},
    {TOP_stlps,             TOP_vstlps},
    {TOP_sthps,             TOP_vsthps},
    {TOP_stlpd,             TOP_vstlpd},
    {TOP_sthpd,             TOP_vsthpd},
    {TOP_sthpdx,             TOP_vsthpdx},
    {TOP_sthpdxx,             TOP_vsthpdxx},
    {TOP_stlps_n32,         TOP_vstlps_n32},
    {TOP_sthps_n32,         TOP_vsthps_n32},
    {TOP_stlpd_n32,         TOP_vstlpd_n32},
    {TOP_sthpd_n32,         TOP_vsthpd_n32},
    {TOP_lddqax,            TOP_vlddqax},
    {TOP_stdqax,            TOP_vstdqax},
    {TOP_stntpdx,           TOP_vstntpdx},
    {TOP_stntpsx,           TOP_vstntpsx},
    {TOP_lddqux,            TOP_vlddqux},
    {TOP_ldlpsx,            TOP_vldlpsx},
    {TOP_ldhpsx,            TOP_vldhpsx},
    {TOP_ldlpdx,            TOP_vldlpdx},
    {TOP_ldhpdx,            TOP_vldhpdx},
    {TOP_ldapdx,            TOP_vldapdx},
    {TOP_ldapsx,            TOP_vldapsx},
    {TOP_stdqux,            TOP_vstdqux},
    {TOP_stlpdx,            TOP_vstlpdx},
    {TOP_sthpdx,            TOP_vsthpdx},
    {TOP_stlpsx,            TOP_vstlpsx},
    {TOP_sthpsx,            TOP_vsthpsx},
    {TOP_lddqaxx,           TOP_vlddqaxx},
    {TOP_stdqaxx,           TOP_vstdqaxx},
    {TOP_stntpdxx,          TOP_vstntpdxx},
    {TOP_stntpsxx,          TOP_vstntpsxx},
    {TOP_lddquxx,           TOP_vlddquxx},
    {TOP_ldlpsxx,           TOP_vldlpsxx},
    {TOP_ldhpsxx,           TOP_vldhpsxx},
    {TOP_ldlpdxx,           TOP_vldlpdxx},
    {TOP_ldhpdxx,           TOP_vldhpdxx},
    {TOP_ldapdxx,           TOP_vldapdxx},
    {TOP_ldapsxx,           TOP_vldapsxx},
    {TOP_stdquxx,           TOP_vstdquxx},
    {TOP_stlpdxx,           TOP_vstlpdxx},
    {TOP_sthpdxx,           TOP_vsthpdxx},
    {TOP_stlpsxx,           TOP_vstlpsxx},
    {TOP_sthpsxx,           TOP_vsthpsxx},
    {TOP_staps,             TOP_vstaps},
    {TOP_staps_n32,         TOP_vstaps_n32},
    {TOP_stups,             TOP_vstups},
    {TOP_stupsx,            TOP_vstupsx},
    {TOP_stupsxx,           TOP_vstupsxx},
    {TOP_stups_n32,         TOP_vstups_n32},
    {TOP_stupd,             TOP_vstupd},
    {TOP_stupdx,            TOP_vstupdx},
    {TOP_stupdxx,           TOP_vstupdxx},
    {TOP_stupd_n32,         TOP_vstupd_n32},
    {TOP_stapd,             TOP_vstapd},
    {TOP_stapd_n32,         TOP_vstapd_n32},
    {TOP_stapsx,            TOP_vstapsx},
    {TOP_stapdx,            TOP_vstapdx},
    {TOP_stapsxx,           TOP_vstapsxx},
    {TOP_stapdxx,           TOP_vstapdxx},
    {TOP_stss,              TOP_vstss},
    {TOP_stss_n32,          TOP_vstss_n32},
    {TOP_stssx,             TOP_vstssx},
    {TOP_stssxx,            TOP_vstssxx},
    {TOP_stsd,              TOP_vstsd},
    {TOP_stsd_n32,          TOP_vstsd_n32},
    {TOP_stsdx,             TOP_vstsdx},
    {TOP_stsdxx,            TOP_vstsdxx},
    {TOP_maxss,             TOP_vfmaxss},
    {TOP_maxsd,             TOP_vfmaxsd},
    {TOP_minss,             TOP_vfminss},
    {TOP_minsd,             TOP_vfminsd},
    {TOP_rcpss,             TOP_vfrcpss},
    {TOP_rsqrtss,           TOP_vfrsqrtss},
    {TOP_sqrtss,            TOP_vfsqrtss},
    {TOP_sqrtsd,            TOP_vfsqrtsd},
    {TOP_andnps,            TOP_vfandn128v32},
    {TOP_andnpd,            TOP_vfandn128v64},
    {TOP_cmpss,             TOP_vcmpss},
    {TOP_cmpsd,             TOP_vcmpsd},
    {TOP_cmpps,             TOP_vcmpps},
    {TOP_cmppd,             TOP_vcmppd},
    {TOP_cmpeqps,           TOP_vcmpeqps}, 
    {TOP_cmpltps,           TOP_vcmpltps},
    {TOP_cmpleps,           TOP_vcmpleps},
    {TOP_cmpunordps,        TOP_vcmpunordps},
    {TOP_cmpneqps,          TOP_vcmpneqps},
    {TOP_cmpnltps,          TOP_vcmpnltps},
    {TOP_cmpnleps,          TOP_vcmpnleps},
    {TOP_cmpordps,          TOP_vcmpordps},
    {TOP_cmpeqss,           TOP_vcmpeqss},
    {TOP_cmpltss,           TOP_vcmpltss},
    {TOP_cmpless,           TOP_vcmpless},
    {TOP_cmpunordss,        TOP_vcmpunordss},
    {TOP_cmpneqss,          TOP_vcmpneqss},
    {TOP_cmpnltss,          TOP_vcmpnltss },
    {TOP_cmpnless,          TOP_vcmpnless},
    {TOP_cmpordss,          TOP_vcmpordss},
    {TOP_cmpeqsd,           TOP_vcmpeqsd},
    {TOP_cmpltsd,           TOP_vcmpltsd},
    {TOP_cmplesd,           TOP_vcmplesd},
    {TOP_cmpunordsd,        TOP_vcmpunordsd},
    {TOP_cmpneqsd,          TOP_vcmpneqsd},
    {TOP_cmpnltsd,          TOP_vcmpnltsd },
    {TOP_cmpnlesd,          TOP_vcmpnlesd},
    {TOP_cmpordsd,          TOP_vcmpordsd},
    {TOP_unpckhpd,          TOP_vunpckh128v64},
    {TOP_unpckhps,          TOP_vunpckh128v32},
    {TOP_unpcklpd,          TOP_vunpckl128v64},
    {TOP_unpcklps,          TOP_vunpckl128v32},
    {TOP_punpcklbw128,      TOP_vpunpckl64v8},
    {TOP_punpcklwd128,      TOP_vpunpckl64v16},
    {TOP_punpckldq128,      TOP_vpunpckl64v32},
    {TOP_punpckhbw128,      TOP_vpunpckh64v8},
    {TOP_punpckhwd128,      TOP_vpunpckh64v16},
    {TOP_punpckhdq128,      TOP_vpunpckh64v32},
    {TOP_packsswb128,       TOP_vpacksswb},
    {TOP_packssdw128,       TOP_vpackssdw},
    {TOP_packuswb128,       TOP_vpackuswb},
    {TOP_pshufd,            TOP_vpshuf128v32},
    {TOP_pshufw,            TOP_vpshufw64v16},
    {TOP_pshuflw,           TOP_vpshuflw},
    {TOP_pshufhw,           TOP_vpshufhw},
    // TBD - need mem opnd forms of TOP_shufx128v{32|64}
    {TOP_shufpd,            TOP_vfshuf128v64},
    {TOP_shufps,            TOP_vfshuf128v32},
    {TOP_movhlps,           TOP_vmovhlps},
    {TOP_movlhps,           TOP_vmovlhps},
    {TOP_psrldq,            TOP_vpsrldq},
    {TOP_psrlq128v64,       TOP_vpsrlqi},
    {TOP_pslldq,            TOP_vpslldq},
    {TOP_psllw,             TOP_vpsllw},
    {TOP_pslld,             TOP_vpslld},
    {TOP_psllq,             TOP_vpsllq},
    {TOP_psrlw,             TOP_vpsrlw},
    {TOP_psrld,             TOP_vpsrld},
    {TOP_psrlq,             TOP_vpsrlq},
    {TOP_psraw,             TOP_vpsraw},
    {TOP_psrad,             TOP_vpsrad},
    {TOP_xzero32,           TOP_vxzero32},
    {TOP_xzero64,           TOP_vxzero64},
    {TOP_xzero128v32,       TOP_vxzero128v32},
    {TOP_xzero128v64,       TOP_vxzero128v64},
    {TOP_subus128v16,       TOP_vsubus128v16},
    {TOP_pavgb128,          TOP_vpavgb},
    {TOP_pavgw128,          TOP_vpavgw},
    {TOP_psadbw128,         TOP_vpsadbw},
    {TOP_storenti128,       TOP_vstorenti128},
    {TOP_storelpd,          TOP_vstorelpd},
    {TOP_pmovmskb128,       TOP_vpmovmskb128},
    // SSE 4.1
    {TOP_mpsadbw,            TOP_vmpsadbw},
    {TOP_mpsadbwx,           TOP_vmpsadbwx},
    {TOP_mpsadbwxx,          TOP_vmpsadbwxx},
    {TOP_mpsadbwxxx,         TOP_vmpsadbwxxx},
    {TOP_muldq,              TOP_vmuldq},
    {TOP_muldqx,             TOP_vmuldqx},
    {TOP_muldqxx,            TOP_vmuldqxx},
    {TOP_muldqxxx,           TOP_vmuldqxxx},
    {TOP_mul128v32,          TOP_vmul128v32},
    {TOP_mulx128v32,         TOP_vmulx128v32},
    {TOP_mulxx128v32,        TOP_vmulxx128v32},
    {TOP_mulxxx128v32,       TOP_vmulxxx128v32},
    {TOP_fdp128v32,          TOP_vfdp128v32},
    {TOP_fdpx128v32,         TOP_vfdpx128v32},
    {TOP_fdpxx128v32,        TOP_vfdpxx128v32},
    {TOP_fdpxxx128v32,       TOP_vfdpxxx128v32},
    {TOP_fdp128v64,          TOP_vfdp128v64},
    {TOP_fdpx128v64,         TOP_vfdpx128v64},
    {TOP_fdpxx128v64,        TOP_vfdpxx128v64},
    {TOP_fdpxxx128v64,       TOP_vfdpxxx128v64},
    {TOP_fblend128v32,       TOP_vfblend128v32},
    {TOP_fblendx128v32,      TOP_vfblendx128v32},
    {TOP_fblendxx128v32,     TOP_vfblendxx128v32},
    {TOP_fblendxxx128v32,    TOP_vfblendxxx128v32},
    {TOP_fblend128v64,       TOP_vfblend128v64},
    {TOP_fblendx128v64,      TOP_vfblendx128v64},
    {TOP_fblendxx128v64,     TOP_vfblendxx128v64},
    {TOP_fblendxxx128v64,    TOP_vfblendxxx128v64},
    {TOP_fblendv128v32,      TOP_vfblendv128v32},
    {TOP_fblendvx128v32,     TOP_vfblendvx128v32},
    {TOP_fblendvxx128v32,    TOP_vfblendvxx128v32},
    {TOP_fblendvxxx128v32,   TOP_vfblendvxxx128v32},
    {TOP_fblendv128v64,      TOP_vfblendv128v64},
    {TOP_fblendvx128v64,     TOP_vfblendvx128v64},
    {TOP_fblendvxx128v64,    TOP_vfblendvxx128v64},
    {TOP_fblendvxxx128v64,   TOP_vfblendvxxx128v64},
    {TOP_blendv128v8,        TOP_vblendv128v8},
    {TOP_blendvx128v8,       TOP_vblendvx128v8},
    {TOP_blendvxx128v8,      TOP_vblendvxx128v8},
    {TOP_blendvxxx128v8,     TOP_vblendvxxx128v8},
    {TOP_blend128v16,        TOP_vblend128v16},
    {TOP_blendx128v16,       TOP_vblendx128v16},
    {TOP_blendxx128v16,      TOP_vblendxx128v16},
    {TOP_blendxxx128v16,     TOP_vblendxxx128v16},
    {TOP_minu128v8,          TOP_vminu128v8},
    {TOP_minux128v8,         TOP_vminux128v8},
    {TOP_minuxx128v8,        TOP_vminuxx128v8},
    {TOP_minuxxx128v8,       TOP_vminuxxx128v8},
    {TOP_mins128v8,          TOP_vmins128v8},
    {TOP_minsx128v8,         TOP_vminsx128v8},
    {TOP_minsxx128v8,        TOP_vminsxx128v8},
    {TOP_minsxxx128v8,       TOP_vminsxxx128v8},
    {TOP_minu128v16,         TOP_vminu128v16},
    {TOP_minux128v16,        TOP_vminux128v16},
    {TOP_minuxx128v16,       TOP_vminuxx128v16},
    {TOP_minuxxx128v16,      TOP_vminuxxx128v16},
    {TOP_mins128v16,         TOP_vmins128v16},
    {TOP_minsx128v16,        TOP_vminsx128v16},
    {TOP_minsxx128v16,       TOP_vminsxx128v16},
    {TOP_minsxxx128v16,      TOP_vminsxxx128v16},
    {TOP_minu128v32,         TOP_vminu128v32},
    {TOP_minux128v32,        TOP_vminux128v32},
    {TOP_minuxx128v32,       TOP_vminuxx128v32},
    {TOP_minuxxx128v32,      TOP_vminuxxx128v32},
    {TOP_mins128v32,         TOP_vmins128v32},
    {TOP_minsx128v32,        TOP_vminsx128v32},
    {TOP_minsxx128v32,       TOP_vminsxx128v32},
    {TOP_minsxxx128v32,      TOP_vminsxxx128v32},
    {TOP_maxu128v8,          TOP_vmaxu128v8},
    {TOP_maxux128v8,         TOP_vmaxux128v8},
    {TOP_maxuxx128v8,        TOP_vmaxuxx128v8},
    {TOP_maxuxxx128v8,       TOP_vmaxuxxx128v8},
    {TOP_maxs128v8,          TOP_vmaxs128v8},
    {TOP_maxsx128v8,         TOP_vmaxsx128v8},
    {TOP_maxsxx128v8,        TOP_vmaxsxx128v8},
    {TOP_maxsxxx128v8,       TOP_vmaxsxxx128v8},
    {TOP_maxu128v16,         TOP_vmaxu128v16},
    {TOP_maxux128v16,        TOP_vmaxux128v16},
    {TOP_maxuxx128v16,       TOP_vmaxuxx128v16},
    {TOP_maxuxxx128v16,      TOP_vmaxuxxx128v16},
    {TOP_maxs128v16,         TOP_vmaxs128v16},
    {TOP_maxsx128v16,        TOP_vmaxsx128v16},
    {TOP_maxsxx128v16,       TOP_vmaxsxx128v16},
    {TOP_maxsxxx128v16,      TOP_vmaxsxxx128v16},
    {TOP_maxu128v32,         TOP_vmaxu128v32},
    {TOP_maxux128v32,        TOP_vmaxux128v32},
    {TOP_maxuxx128v32,       TOP_vmaxuxx128v32},
    {TOP_maxuxxx128v32,      TOP_vmaxuxxx128v32},
    {TOP_maxs128v32,         TOP_vmaxs128v32},
    {TOP_maxsx128v32,        TOP_vmaxsx128v32},
    {TOP_maxsxx128v32,       TOP_vmaxsxx128v32},
    {TOP_maxsxxx128v32,      TOP_vmaxsxxx128v32},
    {TOP_round128v32,        TOP_vround128v32},
    {TOP_roundx128v32,       TOP_vroundx128v32},
    {TOP_roundxx128v32,      TOP_vroundxx128v32},
    {TOP_roundxxx128v32,     TOP_vroundxxx128v32},
    {TOP_roundss,            TOP_vroundss},
    {TOP_roundxss,           TOP_vroundxss},
    {TOP_roundxxss,          TOP_vroundxxss},
    {TOP_roundxxxss,         TOP_vroundxxxss},
    {TOP_round128v64,        TOP_vround128v64},
    {TOP_roundx128v64,       TOP_vroundx128v64},
    {TOP_roundxx128v64,      TOP_vroundxx128v64},
    {TOP_roundxxx128v64,     TOP_vroundxxx128v64},
    {TOP_roundsd,            TOP_vroundsd},
    {TOP_roundxsd,           TOP_vroundxsd},
    {TOP_roundxxsd,          TOP_vroundxxsd},
    {TOP_roundxxxsd,         TOP_vroundxxxsd},
    {TOP_finsr128v32,        TOP_vfinsr128v32},
    {TOP_finsrx128v32,       TOP_vfinsrx128v32},
    {TOP_finsrxx128v32,      TOP_vfinsrxx128v32},
    {TOP_finsrxxx128v32,     TOP_vfinsrxxx128v32},
    {TOP_insr128v8,          TOP_vinsr128v8},
    {TOP_insrx128v8,         TOP_vinsrx128v8},
    {TOP_insrxx128v8,        TOP_vinsrxx128v8},
    {TOP_insrxxx128v8,       TOP_vinsrxxx128v8},
    {TOP_insr128v16,         TOP_vinsr128v16},
    {TOP_insrx128v16,        TOP_vinsrx128v16},
    {TOP_insrxx128v16,       TOP_vinsrxx128v16},
    {TOP_insrxxx128v16,      TOP_vinsrxxx128v16},
    {TOP_insr128v32,         TOP_vinsr128v32},
    {TOP_insrx128v32,        TOP_vinsrx128v32},
    {TOP_insrxx128v32,       TOP_vinsrxx128v32},
    {TOP_insrxxx128v32,      TOP_vinsrxxx128v32},
    {TOP_insr128v64,         TOP_vinsr128v64},
    {TOP_insrx128v64,        TOP_vinsrx128v64},
    {TOP_insrxx128v64,       TOP_vinsrxx128v64},
    {TOP_insrxxx128v64,      TOP_vinsrxxx128v64},
    {TOP_fextr128v32,        TOP_vfextr128v32},
    {TOP_fextrx128v32,       TOP_vfextrx128v32},
    {TOP_fextrxx128v32,      TOP_vfextrxx128v32},
    {TOP_fextrxxx128v32,     TOP_vfextrxxx128v32},
    {TOP_extr128v8,          TOP_vextr128v8},
    {TOP_extrx128v8,         TOP_vextrx128v8},
    {TOP_extrxx128v8,        TOP_vextrxx128v8},
    {TOP_extrxxx128v8,       TOP_vextrxxx128v8},
    {TOP_extr128v16,         TOP_vextr128v16},
    {TOP_extrx128v16,        TOP_vextrx128v16},
    {TOP_extrxx128v16,       TOP_vextrxx128v16},
    {TOP_extrxxx128v16,      TOP_vextrxxx128v16},
    {TOP_extr128v32,         TOP_vextr128v32},
    {TOP_extrx128v32,        TOP_vextrx128v32},
    {TOP_extrxx128v32,       TOP_vextrxx128v32},
    {TOP_extrxxx128v32,      TOP_vextrxxx128v32},
    {TOP_extr128v64,         TOP_vextr128v64},
    {TOP_extrx128v64,        TOP_vextrx128v64},
    {TOP_extrxx128v64,       TOP_vextrxx128v64},
    {TOP_extrxxx128v64,      TOP_vextrxxx128v64},
    {TOP_pmovsxbw,           TOP_vpmovsxbw},
    {TOP_pmovsxbwx,          TOP_vpmovsxbwx},
    {TOP_pmovsxbwxx,         TOP_vpmovsxbwxx},
    {TOP_pmovsxbwxxx,        TOP_vpmovsxbwxxx},
    {TOP_pmovzxbw,           TOP_vpmovzxbw},
    {TOP_pmovzxbwx,          TOP_vpmovzxbwx},
    {TOP_pmovzxbwxx,         TOP_vpmovzxbwxx},
    {TOP_pmovzxbwxxx,        TOP_vpmovzxbwxxx},
    {TOP_pmovsxbd,           TOP_vpmovsxbd},
    {TOP_pmovsxbdx,          TOP_vpmovsxbdx},
    {TOP_pmovsxbdxx,         TOP_vpmovsxbdxx},
    {TOP_pmovsxbdxxx,        TOP_vpmovsxbdxxx},
    {TOP_pmovzxbd,           TOP_vpmovzxbd},
    {TOP_pmovzxbdx,          TOP_vpmovzxbdx},
    {TOP_pmovzxbdxx,         TOP_vpmovzxbdxx},
    {TOP_pmovzxbdxxx,        TOP_vpmovzxbdxxx},
    {TOP_pmovsxbq,           TOP_vpmovsxbq},
    {TOP_pmovsxbqx,          TOP_vpmovsxbqx},
    {TOP_pmovsxbqxx,         TOP_vpmovsxbqxx},
    {TOP_pmovsxbqxxx,        TOP_vpmovsxbqxxx},
    {TOP_pmovzxbq,           TOP_vpmovzxbq},
    {TOP_pmovzxbqx,          TOP_vpmovzxbqx},
    {TOP_pmovzxbqxx,         TOP_vpmovzxbqxx},
    {TOP_pmovzxbqxxx,        TOP_vpmovzxbqxxx},
    {TOP_pmovsxwd,           TOP_vpmovsxwd},
    {TOP_pmovsxwdx,          TOP_vpmovsxwdx},
    {TOP_pmovsxwdxx,         TOP_vpmovsxwdxx},
    {TOP_pmovsxwdxxx,        TOP_vpmovsxwdxxx},
    {TOP_pmovzxwd,           TOP_vpmovzxwd},
    {TOP_pmovzxwdx,          TOP_vpmovzxwdx},
    {TOP_pmovzxwdxx,         TOP_vpmovzxwdxx},
    {TOP_pmovzxwdxxx,        TOP_vpmovzxwdxxx},
    {TOP_pmovsxwq,           TOP_vpmovsxwq},
    {TOP_pmovsxwqx,          TOP_vpmovsxwqx},
    {TOP_pmovsxwqxx,         TOP_vpmovsxwqxx},
    {TOP_pmovsxwqxxx,        TOP_vpmovsxwqxxx},
    {TOP_pmovzxwq,           TOP_vpmovzxwq},
    {TOP_pmovzxwqx,          TOP_vpmovzxwqx},
    {TOP_pmovzxwqxx,         TOP_vpmovzxwqxx},
    {TOP_pmovzxwqxxx,        TOP_vpmovzxwqxxx},
    {TOP_pmovsxdq,           TOP_vpmovsxdq},
    {TOP_pmovsxdqx,          TOP_vpmovsxdqx},
    {TOP_pmovsxdqxx,         TOP_vpmovsxdqxx},
    {TOP_pmovsxdqxxx,        TOP_vpmovsxdqxxx},
    {TOP_pmovzxdq,           TOP_vpmovzxdq},
    {TOP_pmovzxdqx,          TOP_vpmovzxdqx},
    {TOP_pmovzxdqxx,         TOP_vpmovzxdqxx},
    {TOP_pmovzxdqxxx,        TOP_vpmovzxdqxxx},
    {TOP_ptest128,           TOP_vptest128},
    {TOP_ptestx128,          TOP_vptestx128},
    {TOP_ptestxx128,         TOP_vptestxx128},
    {TOP_ptestxxx128,        TOP_vptestxxx128},
    {TOP_cmpeq128v64,        TOP_vcmpeq128v64},
    {TOP_cmpeqx128v64,       TOP_vcmpeqx128v64},
    {TOP_cmpeqxx128v64,      TOP_vcmpeqxx128v64},
    {TOP_cmpeqxxx128v64,     TOP_vcmpeqxxx128v64},
    {TOP_packusdw,           TOP_vpackusdw},
    {TOP_packusdwx,          TOP_vpackusdwx},
    {TOP_packusdwxx,         TOP_vpackusdwxx},
    {TOP_packusdwxxx,        TOP_vpackusdwxxx},
    {TOP_ldntdqa,            TOP_vldntdqa},
    {TOP_ldntdqax,           TOP_vldntdqax},
    {TOP_ldntdqaxx,          TOP_vldntdqaxx},
    {TOP_stntdq,             TOP_vstntdq},
    {TOP_stntdqx,            TOP_vstntdqx},
    {TOP_stntdqxx,           TOP_vstntdqxx},
    // SSE4.2
    {TOP_cmpestri,           TOP_vcmpestri},
    {TOP_cmpestrix,          TOP_vcmpestrix},
    {TOP_cmpestrixx,         TOP_vcmpestrixx},
    {TOP_cmpestrixxx,        TOP_vcmpestrixxx},
    {TOP_cmpestrm,           TOP_vcmpestrm},
    {TOP_cmpestrmx,          TOP_vcmpestrmx},
    {TOP_cmpestrmxx,         TOP_vcmpestrmxx},
    {TOP_cmpestrmxxx,        TOP_vcmpestrmxxx},
    {TOP_cmpistri,           TOP_vcmpistri},
    {TOP_cmpistrix,          TOP_vcmpistrix},
    {TOP_cmpistrixx,         TOP_vcmpistrixx},
    {TOP_cmpistrixxx,        TOP_vcmpistrixxx},
    {TOP_cmpistrm,           TOP_vcmpistrm},
    {TOP_cmpistrmx,          TOP_vcmpistrmx},
    {TOP_cmpistrmxx,         TOP_vcmpistrmxx},
    {TOP_cmpistrmxxx,        TOP_vcmpistrmxxx},
    {TOP_cmpgt128v64,        TOP_vcmpgt128v64},
    {TOP_cmpgtx128v64,       TOP_vcmpgtx128v64},
    {TOP_cmpgtxx128v64,      TOP_vcmpgtxx128v64},
    {TOP_cmpgtxxx128v64,     TOP_vcmpgtxxx128v64},
    // SSSE3
    {TOP_psign128v8,         TOP_vpsign128v8},
    {TOP_psignx128v8,        TOP_vpsignx128v8},
    {TOP_psignxx128v8,       TOP_vpsignxx128v8},
    {TOP_psignxxx128v8,      TOP_vpsignxxx128v8},
    {TOP_psign128v16,        TOP_vpsign128v16},
    {TOP_psignx128v16,       TOP_vpsignx128v16},
    {TOP_psignxx128v16,      TOP_vpsignxx128v16},
    {TOP_psignxxx128v16,     TOP_vpsignxxx128v16},
    {TOP_psign128v32,        TOP_vpsign128v32},
    {TOP_psignx128v32,       TOP_vpsignx128v32},
    {TOP_psignxx128v32,      TOP_vpsignxx128v32},
    {TOP_psignxxx128v32,     TOP_vpsignxxx128v32},
    {TOP_pabs128v8,          TOP_vabs128v8},
    {TOP_pabsx128v8,         TOP_vabsx128v8},
    {TOP_pabsxx128v8,        TOP_vabsxx128v8},
    {TOP_pabsxxx128v8,       TOP_vabsxxx128v8},
    {TOP_pabs128v16,         TOP_vabs128v16},
    {TOP_pabsx128v16,        TOP_vabsx128v16},
    {TOP_pabsxx128v16,       TOP_vabsxx128v16},
    {TOP_pabsxxx128v16,      TOP_vabsxxx128v16},
    {TOP_pabs128v32,         TOP_vabs128v32},
    {TOP_pabsx128v32,        TOP_vabsx128v32},
    {TOP_pabsxx128v32,       TOP_vabsxx128v32},
    {TOP_pabsxxx128v32,      TOP_vabsxxx128v32},
    {TOP_palignr128,         TOP_vpalignr128},
    {TOP_palignrx128,        TOP_vpalignrx128},
    {TOP_palignrxx128,       TOP_vpalignrxx128},
    {TOP_palignrxxx128,      TOP_vpalignrxxx128},
    {TOP_pshuf128v8,         TOP_vpshuf128v8},
    {TOP_pshufx128v8,        TOP_vpshufx128v8},
    {TOP_pshufxx128v8,       TOP_vpshufxx128v8},
    {TOP_pshufxxx128v8,      TOP_vpshufxxx128v8},
    {TOP_pmulhrsw128,        TOP_vmulhrsw},
    {TOP_pmulhrswx128,       TOP_vmulhrswx},
    {TOP_pmulhrswxx128,      TOP_vmulhrswxx},
    {TOP_pmulhrswxxx128,     TOP_vmulhrswxxx},
    {TOP_pmaddubsw128,       TOP_vpmaddubsw128},
    {TOP_pmaddubswx128,      TOP_vpmaddubswx128},
    {TOP_pmaddubswxx128,     TOP_vpmaddubswxx128},
    {TOP_pmaddubswxxx128,    TOP_vpmaddubswxxx128},
    {TOP_phsub128v16,        TOP_vphsub128v16},
    {TOP_phsubx128v16,       TOP_vphsubx128v16},
    {TOP_phsubxx128v16,      TOP_vphsubxx128v16},
    {TOP_phsubxxx128v16,     TOP_vphsubxxx128v16},
    {TOP_phsub128v32,        TOP_vphsub128v32},
    {TOP_phsubx128v32,       TOP_vphsubx128v32},
    {TOP_phsubxx128v32,      TOP_vphsubxx128v32},
    {TOP_phsubxxx128v32,     TOP_vphsubxxx128v32},
    {TOP_phsubs128v16,       TOP_vphsubs128v16},
    {TOP_phsubsx128v16,      TOP_vphsubsx128v16},
    {TOP_phsubsxx128v16,     TOP_vphsubsxx128v16},
    {TOP_phsubsxxx128v16,    TOP_vphsubsxxx128v16},
    {TOP_phadd128v16,        TOP_vphadd128v16},
    {TOP_phaddx128v16,       TOP_vphaddx128v16},
    {TOP_phaddxx128v16,      TOP_vphaddxx128v16},
    {TOP_phaddxxx128v16,     TOP_vphaddxxx128v16},
    {TOP_phadd128v32,        TOP_vphadd128v32},
    {TOP_phaddx128v32,       TOP_vphaddx128v32},
    {TOP_phaddxx128v32,      TOP_vphaddxx128v32},
    {TOP_phaddxxx128v32,     TOP_vphaddxxx128v32},
    {TOP_phadds128v16,       TOP_vphadds128v16},
    {TOP_phaddsx128v16,      TOP_vphaddsx128v16},
    {TOP_phaddsxx128v16,     TOP_vphaddsxx128v16},
    {TOP_phaddsxxx128v16,    TOP_vphaddsxxx128v16},
    {TOP_fmovddup,           TOP_vmovddup},
    {TOP_fmovddupx,          TOP_vmovddupx},
    {TOP_fmovddupxx,         TOP_vmovddupxx},
    {TOP_fmovddupxxx,        TOP_vmovddupxxx},
    {TOP_fmovshdup,          TOP_vmovshdup},
    {TOP_fmovshdupx,         TOP_vmovshdupx},
    {TOP_fmovshdupxx,        TOP_vmovshdupxx},
    {TOP_fmovshdupxxx,       TOP_vmovshdupxxx},
    {TOP_fmovsldup,          TOP_vmovsldup},
    {TOP_fmovsldupx,         TOP_vmovsldupx},
    {TOP_fmovsldupxx,        TOP_vmovsldupxx},
    {TOP_fmovsldupxxx,       TOP_vmovsldupxxx},
};

void Init_LegacySSE_To_Vex_Group(void)
{
  int i, j;
 
  for (i = 0; i <= TOP_count; i++) {
    Top_Leg_To_Vex_Mode_Group[i].legacy_mode = (TOP)i;
    Top_Leg_To_Vex_Mode_Group[i].vex_mode = TOP_UNDEFINED;
  }

  UINT table_size = sizeof(Top_SSE_To_Vex_Mode_Group_Table) / 
                    sizeof(Top_Trans_Group);

  // now populate the vex_mode translations
  for (i = 0; i <= TOP_count; i++) {
    for (j = 0; j < table_size; j++) {
      if (Top_SSE_To_Vex_Mode_Group_Table[j].legacy_mode == i) {
        Top_Leg_To_Vex_Mode_Group[i].vex_mode =
          Top_SSE_To_Vex_Mode_Group_Table[j].vex_mode;
      }
    }
  }
}

TOP Remap_topcode(OP *op, TOP opr)
{
  if (Is_Target_Orochi() && Is_Target_AVX()) {
    if (Top_Leg_To_Vex_Mode_Group[opr].vex_mode == TOP_UNDEFINED)
      return opr;

    opr = Top_Leg_To_Vex_Mode_Group[opr].vex_mode;

    // TODO: add operand size check for 256-bit
    if (PU_has_avx128 == FALSE)
      PU_has_avx128 = TRUE;
  }
  return opr;
}

#endif
