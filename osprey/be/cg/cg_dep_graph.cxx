/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


//-*-c++-*-
/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_dep_graph.cxx
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/cg_dep_graph.cxx,v $
 *
 *  Description:
 *  ============
 *
 *  CG dependence graph implementation.  See cg_dep_graph.h for interface.
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <stdint.h>

#define USE_STANDARD_TYPES
#include <sys/types.h>
#include <list>
#include <vector>
#include <map>
#include "defs.h"
#include "mempool.h"
#include "errors.h"
#include "ercg.h"
#include "wn.h"
#include "dvector.h"				/* for LNO dep graph */
#include "dep_graph.h"				/* for LNO dep graph */
#include "opt_alias_interface.h"		/* for WOPT alias mgr */
#include "opt_points_to.h"

#include "cg.h"
#include "cgir.h"
#include "tn_set.h"
#include "tn_map.h"
#include "tn_list.h"
#include "ti_latency.h"
#include "register.h"
#include "bitset.h"
#include "tracing.h"
#include "cgtarget.h"
#include "cgprep.h"
#include "op_list.h"
#include "whirl2ops.h"
#include "cg_loop.h"
#include "pf_cg.h"
#include "wn_map.h"
#include "cg_db_op.h"
#include "cg_cflow.h"
#include "cg_loop_scc.h"
#include "cg_flags.h"
#include "cg_spill.h"
#include "cg_swp_target.h"
#include "dominate.h"
#include "bb_set.h"
#include "freq.h"
#include "gra_live.h"
#include "reg_live.h"
#include "targ_proc_properties.h"
#include "pqs_cg.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "gcm.h"

#include "cg_dep_graph.h"
#include "cg_dep_graph_util.h"

#ifdef TARG_IA64
#include "speculation.h"
#include "recovery.h"
#endif

#include "data_layout.h"

#if defined(TARG_SL) || defined(TARG_MIPS) // Build_LUT_Insn
#include <iostream>
#include <vector>
#include <stdlib.h>
#include <strings.h>
#include <map>
#include <fstream>
#include <string>
#include <ctype.h>
#include "glob.h"
#include "lib_phase_dir.h"
#include "dag.h"
#endif 

#ifdef TARG_IA64
/* for ORC's dag constructor */
#include <set>
#include <ext/hash_map>
#include "ipfec_defs.h"
#include "region_bb_util.h"
#include "dag.h"
#include "prdb.h"
#include "op_targ.h"
#include "dag.h"

/* for cache information */
#include "targ_cache_info.h"
#include "cache_analysis.h"

// #include "w2op.h"
#endif

/* Without this, C++ inlines even with -g */
#ifdef DONT_INLINE
#define inline static
#endif

#ifdef TARG_X8664
#define OP_Load(o)   ( OP_load(o) || OP_load_exe(o) )
#else
#define OP_Load(o)   OP_load(o)
#endif

#define Set_OP_opnds(o,n)	((o)->opnds = (n))
#define Set_OP_results(o,n)	((o)->results = (n))

//
// =====================================================================
//			     Dependency Info
// =====================================================================
//

/* Define the cycles referred to by ARCs:
 */
typedef enum {
  CYC_UNKNOWN,  /* Undefined kind */
  CYC_ISSUE,    /* First operation issue cycle */
  CYC_ISSUED,   /* Last operation issue cycle */
  CYC_COMMIT,   /* Operation commit cycle */
  CYC_READ,     /* Register operand access cycle */
  CYC_WRITE,    /* Register operand write cycle */
  CYC_LOAD,     /* Memory operand load cycle */
  CYC_STORE,    /* Memory operand store cycle */
  CYC_NUM_KIND	/* Number (count) of CYC_KINDs */
} CYC_KIND;

// Define the default cycles and adjustments associated with the
// various dependency kinds:
//
struct dep_info {
  CG_DEP_KIND  kind;	/* The dependency kind being described */
  char         name[8];	/* Name of the dependency kind (for tracing) */
  mINT16       tail;	/* The tail cycle of arcs of this kind */
  mINT16       head;	/* The head cycle of arcs of this kind */
  mINT16       adjust;	/* The default adjustment of arcs of this kind */
};

// This table associates with (potentially) each dependency kind a
// tail and head cycle kind, and a cycle count adjustment to use in
// calculating latencies.  By default, if not specified here for a
// given dependency kind, the issue cycle is always used, with an
// adjustment of zero.
//
// Note that the standard latency calculation will allow operations
// A->B with delay 0 (same cycles) to be scheduled in the same cycle,
// resources allowing.  Therefore, for cases like register input
// dependencies, where the hardware does not allow use in the same
// cycle that a new result is written, either this table must provide
// a positive adjust value, or the dependency arcs must be created with
// a positive delay.
//
static const struct dep_info *dep_info[CG_DEP_NUM_KIND];

// Accessors for dep_info table:

#define DEP_INFO_name(k)	(dep_info[k]->name)
#define DEP_INFO_tail(k)	(dep_info[k]->tail)
#define DEP_INFO_head(k)	(dep_info[k]->head)
#define DEP_INFO_adjust(k)	(dep_info[k]->adjust)

//
// Initialization data for dep_info table. Unfortunately we can't
// initialize the table directly without assuming the ordering of
// the values in the CG_DEP_KIND enum, so we have to set the pointers
// at run-time.
//
static const struct dep_info dep_info_data[] = {
  { CG_DEP_REGIN,   "REGIN",   CYC_WRITE,   CYC_READ,    0 },
  { CG_DEP_REGOUT,  "REGOUT",  CYC_WRITE,   CYC_WRITE,   1 },
  { CG_DEP_REGANTI, "REGANTI", CYC_READ,    CYC_WRITE,   1 },
  { CG_DEP_MEMIN,   "MEMIN",   CYC_STORE,   CYC_LOAD,    1 },
  { CG_DEP_MEMOUT,  "MEMOUT",  CYC_STORE,   CYC_STORE,   1 },
  { CG_DEP_MEMANTI, "MEMANTI", CYC_LOAD,    CYC_STORE,   1 },
  { CG_DEP_MEMVOL,  "MEMVOL",  CYC_LOAD,    CYC_LOAD,    1 },
  { CG_DEP_MEMREAD, "MEMREAD", CYC_LOAD,    CYC_LOAD,    1 },
  { CG_DEP_SPILLIN, "SPILLIN", CYC_STORE,   CYC_LOAD,    1 },
  { CG_DEP_PREFIN,  "PREFIN",  CYC_LOAD,    CYC_LOAD,    1 },
  { CG_DEP_PREFOUT, "PREFOUT", CYC_STORE,   CYC_STORE,   1 },
  { CG_DEP_PREBR,   "PREBR",   CYC_ISSUED,  CYC_COMMIT,  1 },
  { CG_DEP_POSTBR,  "POSTBR",  CYC_ISSUED,  CYC_COMMIT,  1 },
  { CG_DEP_SCC,     "SCC",     CYC_UNKNOWN, CYC_UNKNOWN, 0 },
#ifdef TARG_IA64
  { CG_DEP_PRECHK,  "PRECHK",  CYC_ISSUED,  CYC_COMMIT,  1 },
  { CG_DEP_POSTCHK, "POSTCHK", CYC_ISSUED,  CYC_COMMIT,  1 },
  { CG_DEP_CTLSPEC, "CTLSPEC", CYC_WRITE,   CYC_READ,   0 },
#endif
  { CG_DEP_MISC,    "MISC",    CYC_ISSUE,   CYC_ISSUE,   0 },
};

//
// =====================================================================
//			     Module State
// =====================================================================
//

BB_MAP _cg_dep_op_info;			/* used in exported inline functions */
enum { PRUNE_NONE, PRUNE_NON_CYCLIC, PRUNE_NON_CYCLIC_WITH_REG,
       PRUNE_CYCLIC_0, PRUNE_CYCLIC_1 };

BOOL CG_DEP_Ignore_LNO = FALSE;			/* exported */
BOOL CG_DEP_Ignore_WOPT = FALSE;		/* exported */
BOOL CG_DEP_Addr_Analysis = TRUE;		/* exported */
BOOL CG_DEP_Verify_Mem_Deps = FALSE;		/* exported */
#ifdef TARG_IA64
BOOL CG_DEP_Add_Alloca_Arcs = FALSE;		/* exported */
BOOL CG_DEP_Relax_Xfer_Dependence = TRUE;       /* exported */
BOOL CG_DEP_Prune_Dependence = TRUE;            /* exported */
#else
BOOL CG_DEP_Add_Alloca_Arcs = TRUE;		/* exported */
BOOL CG_DEP_Relax_Xfer_Dependence = FALSE;      /* exported */
BOOL CG_DEP_Prune_Dependence = FALSE;           /* exported */
#endif
BOOL CG_DEP_Adjust_OOO_Latency = TRUE;          /* exported */
INT32 CG_DEP_Mem_Arc_Pruning = PRUNE_NONE;	/* exported */

BB * _cg_dep_bb; // exported to cg_dep_graph_update.h so it can 
		 // be used in an inline function there.

static std::list<BB*> _cg_dep_bbs;
static MEM_POOL dep_map_nz_pool;
       MEM_POOL dep_nz_pool;
static BOOL include_assigned_registers;
static BOOL cyclic;
static BOOL include_memread_arcs;
static BOOL include_memin_arcs;
static BOOL include_control_arcs;
static BOOL tracing;


//
// =====================================================================
//		      Barrier/Intrinsic Support
// =====================================================================
//

// All that's necessary is to treat the barrier and intrinsic OPs
// like stores when constructing the graph.  WOPT alias analysis
// (or our conservative assumptions when no Alias_Manager given)
// will do the right thing.

inline BOOL OP_like_barrier(OP *op)
{
  return (CGTARG_Is_OP_Barrier(op) || OP_Alloca_Barrier(op));
}

BOOL OP_like_store(OP *op)
{
  BOOL like_store = (OP_store(op) || CGTARG_Is_OP_Intrinsic(op) || 
#if defined(TARG_SL)
		     OP_code(op) == TOP_c2_joint ||
#endif
		     CGTARG_Is_OP_Barrier(op) || OP_code(op) == TOP_asm);

#ifdef TARG_X8664
  like_store |= OP_load_exe_store(op);
#endif

  like_store |= OP_like_barrier(op);

  return like_store;
}

//
// -----------------------------------------------------------------------
// TRUE if the control dependence between <op> and <xfer_op> 
// must be preserved for all practical purposes.
// -----------------------------------------------------------------------
//
BOOL
is_xfer_depndnce_reqd(const void *op, const void *xfer_op)

{
  // If <option> disabled, always return TRUE.
  if (!CG_DEP_Relax_Xfer_Dependence) return TRUE;

  // can't rearrange xfer_ops, return TRUE.
  if (OP_xfer((OP *) op)) return TRUE;

  // if <xfer_op> is a call, check if <op> can move across the call.
  if (OP_call((OP*) xfer_op) && 
      CG_DEP_Can_OP_Move_Across_Call((OP*) op, (OP*) xfer_op, TRUE, TRUE))
    goto safe_dependence;

  // if <xfer_op> is a branch op, check if <op> is not live-out.
  if (OP_cond((OP *) xfer_op)) {
    // Check if the <op> is not safe to speculate.
    if (!CGTARG_Can_Be_Speculative((OP *) op)) return TRUE;

    BBLIST *succ_list;
    BOOL live_out = FALSE;
    for (INT i = 0; i < OP_results((OP *) op); ++i) {
      TN *result_tn = OP_result((OP *) op, i);
      FOR_ALL_BB_SUCCS(OP_bb((OP *) xfer_op), succ_list) {

	// Check to see if TN is not live-into <succ_bb>
	BB *succ_bb = BBLIST_item(succ_list);
	if (succ_bb == OP_bb((OP *) op)) continue;
	live_out |= GRA_LIVE_TN_Live_Into_BB(result_tn, succ_bb);

	// Check to see if TN has an associated register and the register
	// is live-into <succ_bb>.
	if (TN_is_register(result_tn)) {
	  ISA_REGISTER_CLASS result_cl = TN_register_class (result_tn);
	  REGISTER result_reg = TN_register (result_tn);
	  live_out |= REG_LIVE_Into_BB (result_cl, result_reg, succ_bb);
	}
      }
    }
    if (!live_out) goto safe_dependence;
  }

  // If we reached this point, we couldn't convince ourselves that the 
  // control dependence between <op> and <xfer_op> can be relaxed.
  return TRUE;

  // At this point, we can safely conclude that the control dependence can
  // be relaxed.
safe_dependence:
  return FALSE;
}


//
// =====================================================================
//			Internal ARC accessors
//
// Various ARC accessors for use only in this module.  Since these are
// internal, they're allowed to access structure members directly.
// =====================================================================
//

#define ARC_pred_idx(arc) (OP_map_idx((arc)->pred+0))

#define ARC_succ_idx(arc) (OP_map_idx((arc)->succ+0))

#define ARC_rest_preds(arc) ((arc)->next[0])

#define ARC_rest_succs(arc) ((arc)->next[1])

#define Set_ARC_pred(arc, predop) ((arc)->pred = (predop))

#define Set_ARC_succ(arc, succop) ((arc)->succ = (succop))

#define Set_ARC_omega(arc, val) ((arc)->omega = (val))

#define Set_ARC_kind(arc, val) {				\
  ARC *_arc = arc;						\
  _arc->kind_def_opnd &= ~0xff;					\
  _arc->kind_def_opnd |= val;					\
}
 
#define Set_ARC_is_definite(arc, val) {				\
  ARC *_arc = arc;						\
  _arc->kind_def_opnd &= ~0x0100;				\
  _arc->kind_def_opnd |= (val != 0) << 8;			\
}

#define Set_ARC_opnd(arc, val) {				\
  ARC *_arc = arc;						\
  _arc->kind_def_opnd &= ~0xf000;				\
  _arc->kind_def_opnd |= val << 12;				\
}

#define Set_ARC_latency(arc, val) ((arc)->latency = (val))

#define Set_ARC_rest_preds(arc, val) ((arc)->next[0] = (val))

#define Set_ARC_rest_succs(arc, val) ((arc)->next[1] = (val))

#define ARC_LIST_is_succ_list(list) ((INTPTR)list & 1)
#define ARC_LIST_is_pred_list(list) (!((INTPTR)list & 1))

ARC_LIST*
ARC_LIST_Find(ARC_LIST *list, CG_DEP_KIND kind, INT16 opnd)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  for (; list; list = ARC_LIST_rest(list)) {
    ARC *arc = ARC_LIST_first(list);
    if (ARC_kind(arc) == kind &&
	(!ARC_has_opnd(arc) || opnd == DONT_CARE || ARC_opnd(arc) == opnd))
      return list;
  }
  return NULL;
}


/* -----------------------------------------------------------------------
 * Use the already-built arcs to find the input OP to a given OP.
 * Note that this assume we are building the arcs by processing a block
 * in a forwrad direction.
 * -----------------------------------------------------------------------
 */
OP *
ARC_LIST_Find_Defining_Op(OP *op, INT16 rslt, CG_DEP_KIND kind, INT16 opnd)
/* -----------------------------------------------------------------------
 *
 * -----------------------------------------------------------------------
 */
{
  INT32 iteration_count = 0;
  ARC_LIST *list = OP_preds(op);
  OP *first_match = NULL;
  for (; list; list = ARC_LIST_rest(list)) {
    ARC *arc = ARC_LIST_first(list);
    if (ARC_kind(arc) == kind &&
        (!ARC_has_opnd(arc) || opnd == DONT_CARE || ARC_opnd(arc) == opnd)) {
      OP *new_op = ARC_pred(arc);
      if (op == new_op) continue;
      if (rslt == -1) {
        return new_op;
      }
      if (!(Is_CG_LOOP_Op(op) && Is_CG_LOOP_Op(new_op)) ||
          (OP_omega(op,rslt) < OP_omega(new_op,opnd)) ) {
        if (first_match != NULL) return NULL;
        first_match = new_op;
        continue;
      }
      if (++iteration_count > 10000) {
        DevWarn("cg_dep_graph: ARC count exceeded. Entering fail-safe node.\n");
        return NULL;
      }
    }
  }
  return first_match;
}


//
// =====================================================================
//
//		  _CG_DEP_OP_INFO Memory Management
//
// Provides an interface that allows us to efficiently allocate and reuse
// _CG_DEP_OP_INFO structures in a transparent manner:
//
//   void init_op_info(void)
//     Initialization routine - should be called from compute_whole_graph.
//
//   _CG_DEP_OP_INFO *new_op_info(void)
//     Returns a pointer to a new, blank _CG_DEP_OP_INFO structure.
//
//   void delete_op_info(OP *op)
//     Frees the storage pointed to by _CG_DEP_op_info(op) for later
//     reallocation.  
//
// =====================================================================
//
 
// Implemented as a simple internally-linked free list.  We use the
// first word of the OP_INFO field as the pointer to the next free
// element.

#define next_free_op_info(op_info) (*(_CG_DEP_OP_INFO **)op_info)
static _CG_DEP_OP_INFO *free_op_info = NULL;

/* See above for specification */
#define init_op_info() (free_op_info = NULL)

#if !defined(TARG_IA64) && !defined(TARG_SL) && !defined(TARG_MIPS)
static
#endif
_CG_DEP_OP_INFO *new_op_info(void)
/* See above for specification */
{
  _CG_DEP_OP_INFO *result = free_op_info;

  if (result) {
    /* Use first element from free list. */
    free_op_info = next_free_op_info(result);
  } else {
    /* Free list is empty.  Allocate from dep_nz_pool. */
    result = TYPE_MEM_POOL_ALLOC(_CG_DEP_OP_INFO, &dep_nz_pool);
  }

  /* Zero the new structure.  Note that this is faster than using a
   * zeroed MEM_POOL since the (small) size is known at compile time.
   */
  result->succs = result->preds = NULL;

  return result;
}

inline void delete_op_info(OP *op)
/* See above for specification */
{
  BB_OP_MAP omap = (BB_OP_MAP)BB_MAP_Get(_cg_dep_op_info, OP_bb(op));
  _CG_DEP_OP_INFO *info = (_CG_DEP_OP_INFO *)BB_OP_MAP_Get(omap, op);
  next_free_op_info(info) = free_op_info;
  free_op_info = info;
}

//
// =====================================================================
//
//		  PREDICATE QUERY (PQS) FUNCTIONS
//
//
// Utility to determine if an OP might interfere with another OP
// It returns TRUE if 
// 1) There is not interference because the qualifying predicates are disjoint
// or
// 2) There is no interference because the OP value2 is a subset of the 
//    predicate in value1, so an assignment is killed. 
//
//
// =====================================================================
//

BOOL
OP_has_subset_predicate(const void *value1, const void *value2)
{
#ifdef TARG_IA64
//use IPFEC aurora PRDB in first priority!
  if(PRDB_Valid()){
  	PRDB_GEN* prdb = Get_PRDB();
  	if(Home_Region(OP_bb((OP*)value1)) != Home_Region(OP_bb((OP*)value2))) return FALSE;
  	if(Home_Region(OP_bb((OP*)value1))->Region_Type() == IMPROPER ||
  		Home_Region(OP_bb((OP*)value1))->Is_No_Further_Opt()) return FALSE;
  	if(!OP_has_predicate((OP*)value1) || !OP_has_predicate((OP*)value2)) return FALSE;
  	return prdb->Partition_Graph(Home_Region(OP_bb((OP*)value1)))->Is_Subset(
          TN_OP_PAIR(OP_opnd((OP*)value2, OP_PREDICATE_OPND),(OP*)value2),
          TN_OP_PAIR(OP_opnd((OP*)value1, OP_PREDICATE_OPND),(OP*)value1));
  }
#endif

  BOOL v1P = FALSE; // value1 has a qualifying predicate.
  BOOL v2P = FALSE; // value2 has a qualifying predicate.

  // Check if OPs have associated predicates and don't execute under same
  // conditions.

  TN *p1, *p2;
  if (OP_has_predicate((OP *) value1) && OP_has_predicate((OP *) value2)) {

    p1 = OP_opnd((OP*) value1, OP_PREDICATE_OPND);
    p2 = OP_opnd((OP*) value2, OP_PREDICATE_OPND);
    v1P = v2P = TRUE;

  } else if (OP_has_predicate((OP *) value1) && 
	     !OP_has_predicate((OP *) value2)) {

    p1 = OP_opnd((OP*) value1, OP_PREDICATE_OPND);
    p2 = True_TN;          // default case
    v1P = TRUE;
    v2P = FALSE;

  } else if (!OP_has_predicate((OP *) value1) && 
	     OP_has_predicate((OP *) value2)) {

    p1 = True_TN;          // default case
    p2 = OP_opnd((OP*) value2, OP_PREDICATE_OPND);
    v1P = FALSE;
    v2P = TRUE;

  }

  if (v1P || v2P) {
    
    // First, check the trivial case
    if (p1 == p2) return TRUE;

    // Second, return conservative if no PQS information is available.
    if (!PQSCG_pqs_valid()) return FALSE;

    // Third, invoke PQS interface to determine if p2 is not a subset of p1.
    return (PQSCG_is_subset_of(p2, p1));
  } 

  return TRUE;
}

BOOL
OP_has_disjoint_predicate(const OP *value1, const OP *value2)
{
#ifdef TARG_IA64
//use IPFEC aurora PRDB in first priority!
  if(PRDB_Valid()){
  	PRDB_GEN* prdb = Get_PRDB();
  	if(Home_Region(OP_bb(value1)) != Home_Region(OP_bb(value2))) return FALSE;
  	if(Home_Region(OP_bb(value1))->Region_Type() == IMPROPER ||
  		Home_Region(OP_bb(value1))->Is_No_Further_Opt()) return FALSE;
  	if(!OP_has_predicate(value1) || !OP_has_predicate(value2)) return FALSE;
  	return prdb->Partition_Graph(Home_Region(OP_bb(value1)))->Is_Disjoint(
          TN_OP_PAIR(OP_opnd(value1, OP_PREDICATE_OPND),value1),
          TN_OP_PAIR(OP_opnd(value2, OP_PREDICATE_OPND),value2));
  }
#endif

  // Check if OPs have associated predicates and don't execute under same
  // conditions.

  if (PQSCG_pqs_valid() && 
      OP_has_predicate(value1) && 
      OP_has_predicate(value2)) {

    TN *p1 = OP_opnd(value1, OP_PREDICATE_OPND);
    TN *p2 = OP_opnd(value2, OP_PREDICATE_OPND);

    // Invoke PQS interface to determine if p1 and p2 are exclusive.
    if (PQSCG_is_disjoint(p1, p2)) return TRUE;
  }

  return FALSE;
}


BOOL
OP_has_subset_predicate_cyclic(OP *op1, OP *op2)
{
  if (!OP_cond_def(op1)) return TRUE;

  TN *p1 = OP_has_predicate(op1) ? OP_opnd(op1, OP_PREDICATE_OPND) : True_TN;
  TN *p2 = OP_has_predicate(op2) ? OP_opnd(op2, OP_PREDICATE_OPND) : True_TN;

  if (!PQSCG_pqs_valid()) return FALSE;
  // Invoke PQS interface to determine if p2 is not a subset of p1.
  return (PQSCG_is_subset_of(p2, p1));
}


BOOL
OP_has_disjoint_predicate_cyclic(OP *op1, OP *op2)
{
  if (PQSCG_pqs_valid() && 
      OP_has_predicate(op1) && 
      OP_has_predicate(op2)) {

    TN *p1 = OP_opnd(op1, OP_PREDICATE_OPND);
    TN *p2 = OP_opnd(op2, OP_PREDICATE_OPND);
    
    // Invoke PQS interface to determine if p1 and p2 are exclusive.
    if (PQSCG_is_disjoint(p1, p2)) return TRUE;
  }

  return FALSE;
}

static BOOL maintain_prebr;
static void maintain_prebr_arc(OP *op);

/* =====================================================================
 *		     Register Assignment Tracking
 *
 * Groups of TNs with the same register assignment are tracked with:
 *
 *   void init_reg_assignments(void)
 *     Create new, empty register assignment groups.
 *
 *   void add_reg_assignment(TN *tn)
 *     Requires: has_assigned_reg(tn)
 *     Notify the tracker of <tn's> register assignment.
 *
 * =====================================================================
 */

// -----------------------------------------------------------------------
// Returns TRUE iff there is a register assignment for <tn> which should
// not be ignored by the dep graph builder.  (We ignore non-dedicated
// assignments when include_assigned_registers is FALSE.)
// -----------------------------------------------------------------------
inline BOOL has_assigned_reg(TN *tn)

{
  return TN_is_register(tn) &&
    (TN_is_dedicated(tn) ||
     include_assigned_registers && ((TN_register(tn) != REGISTER_UNDEFINED) ||
				    !TN_is_true_pred(tn)));
}

TN_LIST *same_reg[REGISTER_MAX+1][ISA_REGISTER_CLASS_MAX+1];

#define init_reg_assignments() bzero(same_reg, sizeof(same_reg))

// See above for interface.
inline void add_reg_assignment(TN *tn)
{
  REGISTER rnum = TN_register(tn);
  ISA_REGISTER_CLASS rclass = TN_register_class(tn);
  TN_LIST *tns;
  Is_True(has_assigned_reg(tn), ("no register (or ignored)"));
  for (tns = same_reg[rnum][rclass]; tns; tns = TN_LIST_rest(tns))
    if (TN_LIST_first(tns) == tn)
      return;
  same_reg[rnum][rclass] = TN_LIST_Push(tn, same_reg[rnum][rclass],
					&dep_nz_pool);
}


/* =====================================================================
 *
 *			    ARC Management
 *
 * Provides an interface that allows us to efficiently allocate, reuse,
 * attach, and detach ARCs in a transparent manner:
 *
 *   void init_arcs(void)
 *     Initialization routine - should be called from compute_whole_graph.
 *
 *   ARC *create_arc(void)
 *     Return a new, unitialized (not necessarily zeroed!) ARC.
 *
 *   void detach_arc_from_succ(ARC *arc)
 *   void detach_arc_from_pred(ARC *arc)
 *   void detach_arc(ARC *arc)
 *     Remove <arc> from OP_preds(ARC_succ(arc)) (from_succ) and/or
 *     OP_succs(ARC_pred(arc)) (from_pred).  detach_arc does both.
 *
 *   void attach_arc_to_succ(ARC *arc)
 *   void attach_arc_to_pred(ARC *arc)
 *   void attach_arc(ARC *arc)
 *     Push <arc> onto OP_preds(ARC_succ(arc)) (to_succ) and/or
 *     OP_succs(ARC_pred(arc)) (to_pred).  attach_arc does both.
 *
 *   ARC *new_arc(CG_DEP_KIND kind, OP *pred, OP *succ, UINT8 omega,
 *		  UINT8 opnd, BOOL is_definite)
 *   ARC *new_arc_with_latency(CG_DEP_KIND kind, OP *pred, OP *succ,
 *			       INT16 latency, UINT8 omega,
 *			       UINT8 opnd, BOOL is_definite)
 *     Returns a pointer to a new ARC with the given attributes.
 *     Also adds the new ARC to the start of <pred's> successor
 *     list and <succ's> predecessor list.  new_arc fills in the
 *     arc's latency via a call to CG_DEP_Latency.  MEMIN arcs are
 *     converted into SPILLIN arcs if <pred> and <succ> are spills.
 *     <latency>, <omega> values are updated accordingly. <is_definite>
 *     flag determines if the dependence is a definite dependence type,
 *     i.e always exists, and applies only in the context of memory
 *     edges.
 *
 *   void delete_arc(ARC *arc)
 *     Frees <arc> for later reallocation.
 *
 * ===================================================================== */

/* Implemented as a simple internally-linked free list.  We use the
 * first word of the ARC field as the pointer to the next free
 * element.
 */
#define next_free_arc(arc) (*(ARC **)arc)
static ARC *free_arcs = NULL;


// See above for specification.
#define init_arcs() (free_arcs = NULL)


// See above for specification.
static ARC *create_arc(void)
{
  ARC *arc = free_arcs;
  if (arc) {
    /* Use first element from free list. */
    Is_True(arc->kind_def_opnd == 0xff, ("non-deleted arc on free list"));
#ifdef Is_True_On
    arc->kind_def_opnd = 0;
#endif
    free_arcs = next_free_arc(arc);
  } else {
    /* Free list is empty.  Allocate from dep_nz_pool. */
    arc = TYPE_MEM_POOL_ALLOC(ARC, &dep_nz_pool);
  }
  return arc;
}

// See above for interface.
inline void detach_arc_from_succ(ARC *arc)
{
  ARC_LIST **prevp = &_CG_DEP_op_info(ARC_succ(arc))->preds;
  while (ARC_LIST_first(*prevp) != arc)
    prevp = &ARC_rest_preds(ARC_LIST_first(*prevp));
  *prevp = ARC_rest_preds(*prevp);
#ifdef Is_True_On
  /* See Is_True in ARC_LIST_rest */
  Set_ARC_rest_preds(arc, (ARC *)~0);
#endif
}

// See above for interface.
inline void detach_arc_from_pred(ARC *arc)
{
  OP *pred = ARC_pred(arc);
  ARC_LIST **prevp = &_CG_DEP_op_info(pred)->succs;
  while (ARC_LIST_first(*prevp) != arc)
    prevp = &ARC_rest_succs(ARC_LIST_first(*prevp));
  *prevp = ARC_rest_succs(arc);
#ifdef Is_True_On
  /* See Is_True in ARC_LIST_rest */
  Set_ARC_rest_succs(arc, (ARC *)~0);
#endif
  if (maintain_prebr && ARC_kind(arc) != CG_DEP_PREBR && ARC_omega(arc) == 0 &&
      ARC_latency(arc) >= 0)
    maintain_prebr_arc(pred);
}

// See above for interface.
inline void detach_arc(ARC *arc)
{
  detach_arc_from_succ(arc);
  detach_arc_from_pred(arc);
}

void CG_DEP_Detach_Arc(ARC *arc) 
{
  detach_arc(arc);
}

// See above for interface.
inline void attach_arc_to_succ(ARC *arc)
{
  OP *succ = ARC_succ(arc);
  Set_ARC_rest_preds(arc, OP_preds(succ));
  _CG_DEP_op_info(succ)->preds = arc;
}

// See above for interface.
inline void attach_arc_to_pred(ARC *arc)
{
  OP *pred = ARC_pred(arc);
  Set_ARC_rest_succs(arc, OP_succs(pred));
  _CG_DEP_op_info(pred)->succs = (ARC_LIST *)((INTPTR)arc | 1);
  if (maintain_prebr && ARC_kind(arc) != CG_DEP_PREBR) maintain_prebr_arc(pred);
}

// See above for interface.
inline void attach_arc(ARC *arc)
{
  attach_arc_to_succ(arc);
  attach_arc_to_pred(arc);
}

inline BOOL dir_has_eq(DIRECTION dir)
/* -----------------------------------------------------------------------
 * Returns TRUE if <dir> includes the "equal" direction component
 * (see "be/com/dvector.h").
 * -----------------------------------------------------------------------
 */
{
  return dir == DIR_EQ || dir == DIR_NEGEQ || dir == DIR_POSEQ ||
    dir == DIR_STAR;
}


inline BOOL ALIAS_RESULT_positive(ALIAS_RESULT result)
{
  return result == POSSIBLY_ALIASED || result == SAME_LOCATION;
}

// Creates a new arc of type <kind> from node <pred> to node <succ> with
// <latency> and <omega> set. <opnd> is the operand number and <is_definite>
// tells if the dependence is a definite type.
#if !(defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS))
static
#endif
ARC *new_arc_with_latency(CG_DEP_KIND kind, OP *pred, OP *succ,
				 INT16 latency, UINT8 omega,
				 UINT8 opnd, BOOL is_definite)
{
  ARC *arc;
  BB_OP_MAP pmap = (BB_OP_MAP)BB_MAP_Get(_cg_dep_op_info, OP_bb(pred));
  BB_OP_MAP smap = (BB_OP_MAP)BB_MAP_Get(_cg_dep_op_info, OP_bb(succ));
  
  if (BB_OP_MAP_Get(pmap, pred) == NULL)
    BB_OP_MAP_Set(pmap, pred, new_op_info());
  if (BB_OP_MAP_Get(smap, succ) == NULL)
    BB_OP_MAP_Set(smap, succ, new_op_info());

  arc = create_arc();

  // Correct <kind> for volatile memory dependences.
  if ((kind == CG_DEP_MEMIN || kind == CG_DEP_MEMOUT ||
       kind == CG_DEP_MEMANTI || kind == CG_DEP_MEMREAD) &&
      OP_volatile(pred) && OP_volatile(succ))
    kind = CG_DEP_MEMVOL;

  // Correct <kind> and recompute <latency> for SPILLIN dependences.
  if (kind == CG_DEP_MEMIN && CGSPILL_Is_Spill_Op(pred) &&
      CGSPILL_Is_Spill_Op(succ) &&
      OP_store(pred) && OP_load(succ)) {
    kind = CG_DEP_SPILLIN;
    latency = CG_DEP_Latency(pred, succ, kind, opnd);
  }

  // Correct <omega> for SPILLIN dependences.
  if (kind == CG_DEP_SPILLIN && cyclic)
    omega = OP_restore_omega(succ);

  Set_ARC_kind(arc, kind);
  Set_ARC_opnd(arc, opnd);
  Set_ARC_is_definite(arc, is_definite);
  Set_ARC_pred(arc, pred);
  Set_ARC_succ(arc, succ);
  Set_ARC_omega(arc, omega);
  Set_ARC_latency(arc, latency);
  Set_ARC_is_dotted(arc, FALSE);

  attach_arc(arc);
  return arc;
}

// See above for specification.
ARC *new_arc(CG_DEP_KIND kind, OP *pred, OP *succ, UINT8 omega,
		    UINT8 opnd, BOOL is_definite)
{
  INT16 latency = (CG_DEP_Adjust_OOO_Latency && PROC_is_out_of_order() &&
		  (kind == CG_DEP_REGOUT || kind == CG_DEP_REGANTI))
		   ? 0 : CG_DEP_Latency(pred, succ, kind, opnd);
  ARC *arc = new_arc_with_latency(kind, pred, succ, latency, omega, opnd,
				  is_definite);
  return arc;
}


// See above for specification.
inline void delete_arc(ARC *arc)
{
  Is_True(((INTPTR)arc & 1) == 0, ("delete_arc passed ARC_LIST"));
  Is_True(arc->kind_def_opnd != 0xff, ("deleting already-deleted arc"));
#ifdef Is_True_On
  /* For assertion check above - also see create_arc */
  arc->kind_def_opnd = 0xff;
  /* See Is_True in ARC_LIST_rest */
  Set_ARC_rest_succs(arc, (ARC *)~0);
  Set_ARC_rest_preds(arc, (ARC *)~0);
#endif
  next_free_arc(arc) = free_arcs;
  free_arcs = arc;
}



/* =====================================================================
 *
 *		    Global TN REGIN Arcs Tracking
 *
 * ARC_LISTs of partial REGIN arcs to uses of TNs not defined locally
 * are kept so we can quickly make incremental updates to the graph.
 * This interface manages these lists at a low level.  Once a local
 * def for a TN is added to the BB, each partial REGIN arc can be
 * turned into a normal REGIN arc by doing:
 *	OP *succ = ARC_succ(arc);
 *	UINT8 opnd = ARC_opnd(arc);
 *	detach_gtn_use_arc(arc);
 *	Set_ARC_pred(arc, def)
 *	Set_ARC_latency(arc, CG_DEP_Latency(def, succ, CG_DEP_REGIN, opnd));
 *	if (cyclic) Set_ARC_omega(OP_omega(succ, opnd));
 *	attach_arc(arc);
 * Note that detach_gtn_use_arc invalidates the ARC_LIST_next pointer.
 *
 *   void init_gtn_use_arcs(void)
 *     Per-compilation-unit initialization routine.  Should be called
 *     from CG_DEP_Init.
 *
 *   void add_gtn_use_arc(OP *op, UINT8 opnd)
 *     Create a partial REGIN arc to OP_opnd(op,opnd) and add it to
 *     the list for that TN.
 *
 *   ARC_LIST *CG_DEP_GTN_Use_Arcs(TN *tn) (exported)
 *     Return list of partial REGIN arcs to uses of <tn>.
 *
 *   void detach_gtn_use_arc(ARC *arc)
 *     Detach the global TN use arc for OP_opnd(op,opnd) from the
 *     list for that TN, invalidating the ARC_LIST_next pointer.
 *
 *   void delete_gtn_use_arc(OP *op, UINT8 opnd)
 *     Remove the global TN use arc for OP_opnd(op,opnd) from the
 *     list for that TN and recycle the arc, if it exists.
 *
 *   void delete_gtn_use_arcs()
 *     Forget about all global TN use arcs.
 *
 * ===================================================================== */
 
/* <gtn_use_map> maps each TN to a list of partial REGIN arcs.
 * Note that ARC_rest_preds(arc) is the "next" pointer in this list,
 * and ARC_rest_succs(arc) is the "previous" pointer (doubly linked
 * for quick detaching).
 */
#define ARC_LIST_prev ARC_rest_succs
#define Set_ARC_LIST_prev Set_ARC_rest_succs
#define Set_ARC_LIST_rest Set_ARC_rest_preds

TN_MAP gtn_use_map;

// See above for interface.
#define init_gtn_use_arcs() (gtn_use_map = TN_MAP_Create())
#define delete_gtn_use_arcs() { \
  TN_MAP_Delete(gtn_use_map); \
  gtn_use_map = NULL; \
}


ARC_LIST *CG_DEP_GTN_Use_Arcs(TN *tn)
{
  ARC_LIST *result = (ARC_LIST *)TN_MAP_Get(gtn_use_map, tn);
  return result;
}

// See above for interface.
void add_gtn_use_arc(OP *op, UINT8 opnd)
{
  TN *tn = OP_opnd(op, opnd);
  ARC_LIST *arcs = CG_DEP_GTN_Use_Arcs(tn);
  ARC *arc = create_arc();

  Is_True(TN_is_register(tn), ("add_gtn_use_arc called w/const TN"));

  Set_ARC_kind(arc, CG_DEP_REGIN);
  Set_ARC_opnd(arc, opnd);
  Set_ARC_is_definite(arc, FALSE);
  Set_ARC_succ(arc, op);
  Set_ARC_omega(arc, 0);

  if (arcs) {
    /* Insert second in list to avoid TN_MAP_Set */
    ARC_LIST *rest = ARC_LIST_rest(arcs);
    /* Set "next" pointers */
    Set_ARC_LIST_rest((ARC_LIST *)arc, rest);
    Set_ARC_LIST_rest(arcs, (ARC_LIST *)arc);
    /* And "previous" pointers */
    Set_ARC_LIST_prev((ARC_LIST *)arc, arcs);
    if (rest) Set_ARC_LIST_prev(rest, (ARC_LIST *)arc);
  } else {
    /* Set "next" pointer */
    Set_ARC_LIST_rest((ARC_LIST *)arc, NULL);
    /* And "previous" pointer */
    Set_ARC_LIST_prev((ARC_LIST *)arc, NULL);
    TN_MAP_Set(gtn_use_map, tn, (ARC_LIST *)arc);
  }
}

// See above for specification.
static void detach_gtn_use_arc(ARC *arc)
{
  TN *tn = OP_opnd(ARC_succ(arc), ARC_opnd(arc));
  if (TN_is_register(tn)) {
    ARC_LIST *before = ARC_LIST_prev(arc);
    ARC_LIST *after = ARC_LIST_rest(arc);
    if (after) Set_ARC_LIST_prev(after, before);
    if (before) Set_ARC_LIST_rest(before, after);
    else TN_MAP_Set(gtn_use_map, tn, after);
  }
}

// See above for specification.
static void delete_gtn_use_arc(OP *op, UINT8 opnd)
{
  TN *tn = OP_opnd(op, opnd);
  if (TN_is_register(tn)) {
    ARC_LIST *arcs = CG_DEP_GTN_Use_Arcs(tn);
#ifdef Is_True_On
    BOOL found = FALSE;
#endif
    while (arcs) {
      ARC *arc = ARC_LIST_first(arcs);
      UINT8 aopnd = ARC_opnd(arc);
      INT16 asidx = ARC_succ_idx(arc);
      arcs = ARC_LIST_rest(arcs);
      if (asidx == OP_map_idx(op) && aopnd == opnd) {
	detach_gtn_use_arc(arc);
	delete_arc(arc);
#ifdef Is_True_On
      if (found)
	DevWarn("more than one gtn use arc for opnd %d of OP%d; "
		"removing",
		aopnd, asidx);
	found = TRUE;
#else
	return;
#endif
      }
    }
  }
}

#undef ARC_LIST_prev       // ARC_rest_succs
#undef Set_ARC_LIST_prev   // Set_ARC_rest_succs
#undef Set_ARC_LIST_rest   // Set_ARC_rest_preds


/* =====================================================================
 *
 *			 Latency Calculation
 *	     (mostly stolen from Ragnarok implementation)
 *
 * The main function here is CG_DEP_Latency.  All others support it.
 *
 * =====================================================================
 */

//
// ---------------------------------------------------------------------
// Given an operation, the (symbolic) cycle required, and the operand
// number (when relevant), return the cycle number referenced.
// ---------------------------------------------------------------------
//
inline INT16 get_cycle(TOP opcode, INT16 ckind, UINT8 opnd)
{
  switch ( ckind ) {
  case CYC_LOAD:
    return TI_LATENCY_Load_Cycle(opcode);
  case CYC_STORE:
    return TI_LATENCY_Store_Cycle(opcode);
  case CYC_ISSUE:
    return 0;
  case CYC_ISSUED:
    return TI_LATENCY_Last_Issue_Cycle(opcode);
  case CYC_COMMIT:
    return TI_LATENCY_Commit_Cycle(opcode);
  case CYC_READ:
    return TI_LATENCY_Operand_Access_Cycle(opcode, opnd);
  case CYC_WRITE:
    return TI_LATENCY_Result_Available_Cycle(opcode, 0 /*???*/);
  }

  ErrMsg(EC_Ill_Cycle, ckind, "get_cycle");
  return 0;
}

#ifdef TARG_IA64
// -----------------------------------------------------------------------
// See "cg_dep_graph.h" for interface description.
// -----------------------------------------------------------------------
//
 
INT16
CG_DEP_Oper_cycle(TOP oper, CG_DEP_KIND kind)
{
  // Initialize the dep_info table.
  INT i;
  for (i = 0; i < sizeof(dep_info_data) / sizeof(dep_info_data[0]); i++) {
    CG_DEP_KIND kind = dep_info_data[i].kind;
    dep_info[kind] = dep_info_data + i;
  }
 
  FmtAssert(DEP_INFO_tail(kind) == CYC_WRITE, ("Failed option to get the cycle of the last op "));
  return get_cycle(oper, DEP_INFO_tail(kind), 0);
}   
#endif

// -----------------------------------------------------------------------
// See "cg_dep_graph.h" for interface description.
// -----------------------------------------------------------------------
//
INT16 
CG_DEP_Oper_Latency(TOP pred_oper, TOP succ_oper, CG_DEP_KIND kind, UINT8 opnd)
{
 
  // Initialize the dep_info table.
  INT i;
  for (i = 0; i < sizeof(dep_info_data) / sizeof(dep_info_data[0]); i++) {
    CG_DEP_KIND kind = dep_info_data[i].kind;
    dep_info[kind] = dep_info_data + i;
  }

  /* The operator latency is the sum of the following parts:
   *
   *  1)  The difference in the referenced cycles (succ minus pred).
   *  2)  The kind-specific adjustment in the TDT dependency info table.
   *
   * The referenced cycles are identified as follows:
   *
   *  1)  The TDT dependency info table specifies a cycle kind relevant
   *      to each node (pred and succ).
   *
   *  2)  The TDT operator descriptor table specifies the cycle number
   *	  associated with each relevant cycle kind.
   */

  INT16 cyc_pred, cyc_succ, latency;

  /* Get the referenced pred cycle: */
  cyc_pred = get_cycle(pred_oper, DEP_INFO_tail(kind), opnd);

  /* Get the referenced succ cycle: */
  cyc_succ = get_cycle(succ_oper, DEP_INFO_head(kind), opnd);

  latency = (cyc_pred - cyc_succ) + DEP_INFO_adjust(kind);

  /* register latencies must be non-negative */
  if (latency < 0 &&
      (kind == CG_DEP_REGIN || kind == CG_DEP_REGOUT ||
       kind == CG_DEP_REGANTI || kind == CG_DEP_MEMIN ||
       kind == CG_DEP_SPILLIN || kind == CG_DEP_MEMOUT ||
       kind == CG_DEP_MEMANTI || kind == CG_DEP_MEMVOL))
    latency = 0;

  return latency;
}

//
// -----------------------------------------------------------------------
// See "cg_dep_graph.h" for interface description.
// -----------------------------------------------------------------------
//
INT16 
CG_DEP_Latency(OP *pred, OP *succ, CG_DEP_KIND kind, UINT8 opnd)
{
  TOP popcode = OP_code(pred);
  TOP sopcode = OP_code(succ);
  INT16 latency = CG_DEP_Oper_Latency(popcode, sopcode, kind, opnd);

  if (OP_load(pred) && kind == CG_DEP_REGIN) {
    INT32 ld_latency_adjust = 0;
    WN *wn, *pf_wn;
    PF_POINTER *pf_ptr;
    UINT32 confidence;

    if (CGTARG_Use_Load_Latency(pred, OP_opnd(succ, opnd))) {
      
      if (    ( wn = Get_WN_From_Memory_OP( pred ) )
	      && ( pf_ptr = (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,wn) ) ) {
	
	if (    ( pf_wn = PF_PTR_wn_pref_1L(pf_ptr) )
		&& ( (confidence = WN_pf_confidence( pf_wn )) != 1 )
		&& (  ! Prefetch_Kind_Enabled( pf_wn ) )   ) {
	
	  if ( confidence )
	    ld_latency_adjust = MAX(ld_latency_adjust, CG_L1_ld_latency);
	  else
	    ld_latency_adjust = MAX(ld_latency_adjust, CG_z_conf_L1_ld_latency);
	}

	if (pf_wn = PF_PTR_wn_pref_2L(pf_ptr)) {

	  // if we already prefetch for L2 cache, no need to increase
	  // the load latency for L1
	
	  ld_latency_adjust = 0;

	  if ( (confidence = WN_pf_confidence( pf_wn )) != 1 
	       && (  ! Prefetch_Kind_Enabled( pf_wn ))) {

	  if ( confidence )
	    ld_latency_adjust = MAX(ld_latency_adjust, CG_L2_ld_latency);
	  else
	    ld_latency_adjust = MAX(ld_latency_adjust, CG_z_conf_L2_ld_latency);
	  }
	}
      }
#ifdef TARG_IA64
      // we need update the latency by using L2 cycle;
      if (Cache_L2_Has_Data(pred)) {
         ld_latency_adjust = Cache_Read_Cycle(CACHE_L2) - Cache_Read_Cycle(CACHE_L1D); 
      }
#endif
      ld_latency_adjust = MAX(ld_latency_adjust, CG_ld_latency);

      latency += ld_latency_adjust;
      
    }
  }

  // Make any target-specific latency adjustments that cannot be presented
  // in targ_info.
  CGTARG_Adjust_Latency(pred, succ, kind, opnd, &latency);

  return latency;
}

/* =====================================================================
 *			  Tracing Functions
 * =====================================================================
 */

void 
CG_DEP_Trace_Arc(ARC *arc, BOOL is_succ, BOOL verbose)
{
  UINT16 pred_id = ARC_pred_idx(arc);
  UINT16 succ_id = ARC_succ_idx(arc);
  CG_DEP_KIND kind = ARC_kind(arc);

  OP *pred_op = ARC_pred(arc);
  OP *succ_op = ARC_succ(arc);
  if (verbose) {
    /* Trace the predecessor */
    fprintf (TFile, "<arc>%4d >>> ", pred_id);
    Print_OP_No_SrcLine(pred_op);
  }

  fprintf(TFile, "<arc>   %c %-10s%4d", verbose ? ' ' : is_succ ? 's' : 'p',
	  DEP_INFO_name(kind), pred_id);

  if (kind == CG_DEP_REGIN || kind == CG_DEP_REGOUT) 
    fprintf(TFile, "(res)  (BB:%d) ", BB_id(OP_bb(pred_op)));
  else if (kind == CG_DEP_REGANTI) 
    fprintf(TFile, "(opd%d) (BB:%d) ", ARC_opnd(arc), BB_id(OP_bb(pred_op)));
  else fprintf(TFile, "      (BB:%d) ", BB_id(OP_bb(pred_op)));
  fprintf(TFile, " ->%4d", succ_id);

  if (kind == CG_DEP_REGANTI || kind == CG_DEP_REGOUT) 
    fprintf(TFile, "(res) (BB:%d) ", BB_id(OP_bb(succ_op)));
  else if (kind == CG_DEP_REGIN) 
    fprintf(TFile, "(opd%d) (BB:%d) ", ARC_opnd(arc), BB_id(OP_bb(succ_op)));
  else fprintf(TFile, "      (BB:%d) ", BB_id(OP_bb(succ_op)));

  fprintf(TFile, "  latency%3d  omega%3d",
	  ARC_latency(arc), ARC_omega(arc));
  if (ARC_is_mem(arc) && ARC_is_definite(arc))
    fprintf(TFile, "  definite");
#ifdef TARG_IA64
  if (ARC_is_dotted(arc))
    fprintf(TFile, "  dotted");
#endif
  fprintf(TFile, "\n");

  if (verbose) {
    /* Trace the successor */
    fprintf(TFile, "<arc>%4d >>> ", succ_id);
    Print_OP_No_SrcLine(succ_op);
  }
}

void 
CG_DEP_Trace_Op_SCC_Arcs(OP *op)
{
  ARC_LIST *arcs;
  if (!Is_CG_LOOP_Op(op)) {
    fprintf(TFile, "<arc>   No SCC arcs - not a loop OP\n");
  } else {
    if (_CG_DEP_op_info(op) == NULL) {
      fprintf(TFile, "<arc>   CG_DEP INFO is NULL\n");
    } else {
      for (arcs = OP_scc_ancestors(op); arcs; arcs = ARC_LIST_rest(arcs))
	CG_DEP_Trace_Arc(ARC_LIST_first(arcs), FALSE, FALSE);
    }
  }
  fprintf(TFile, "<arc> %3d >>> ", OP_map_idx(op));
  Print_OP_No_SrcLine(op);
  if (Is_CG_LOOP_Op(op) && _CG_DEP_op_info(op)) {
    for (arcs = OP_scc_descendents(op); arcs; arcs = ARC_LIST_rest(arcs))
      CG_DEP_Trace_Arc(ARC_LIST_first(arcs), TRUE, FALSE);
  }
}

void 
CG_DEP_Trace_Op_Arcs(OP *op)
{
  ARC_LIST *arcs;
  if (_CG_DEP_op_info(op) == NULL) {
    fprintf(TFile, "<arc>   CG_DEP INFO is NULL\n");
  } else {
    for (arcs = OP_preds(op); arcs; arcs = ARC_LIST_rest(arcs))
      CG_DEP_Trace_Arc(ARC_LIST_first(arcs), FALSE, FALSE);
  }
  fprintf(TFile, "<arc> %3d >>> ", OP_map_idx(op));
  Print_OP_No_SrcLine(op);
  if (_CG_DEP_op_info(op)) {
    for (arcs = OP_succs(op); arcs; arcs = ARC_LIST_rest(arcs))
      CG_DEP_Trace_Arc(ARC_LIST_first(arcs), TRUE, FALSE);
  }
}

void 
CG_DEP_Trace_Graph(BB *bb)
{
  OP *op;

  if (bb == NULL) {
    fprintf(TFile, "CG_DEP_Trace_Graph: no dep graph instantiated\n");
    return;
  }

  op = BB_first_op(bb);
  fprintf(TFile,
	  "%sCG %s dependence graph for BB:%d (line %d)\n"
	  "  current phase: %s\n"
	  "  %s dependences due to register assignments\n%s",
	  DBar,
	  cyclic ? "cyclic" : "non-cyclic",
	  BB_id(bb),
	  op ? Srcpos_To_Line(OP_srcpos(op)) : 0,
	  Get_Error_Phase(),
	  include_assigned_registers ? "includes" : "does not include",
	  DBar);

  if (Get_Trace(TP_CG, 2)) {
    Print_OPs_No_SrcLines(op);
  }

  FOR_ALL_BB_OPs(bb, op) {
    fprintf (TFile, "\n");
    CG_DEP_Trace_Op_Arcs(op);
  }
  fprintf(TFile, "%s\n", DBar);
}

void 
CG_DEP_Trace_HB_Graph(std::list<BB*> bblist)
{

  if (bblist.empty()) {
    fprintf(TFile, "CG_DEP_Trace_HB_Graph: no dep graph instantiated\n");
    return;
  }

  std::list<BB*>::iterator bbi;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    CG_DEP_Trace_Graph(*bbi);
  }

  fprintf(TFile, "%s\n", DBar);
}



/* =====================================================================
 *			    DefOp tracker
 *
 * Keep track of defining OPs, indexed by TN and register assignment.
 *
 * TODO: This overlaps some functionality (now) provided by the cg_def_op
 *	 module.  Need to combine these two ...
 *
 * void defop_init(void)
 *   Clear the defop data structures in preparation for use.
 *
 * void defop_set(OP *op)
 *   Register <op> so that it gets returned by subsequent lookups (see
 *   below).
 *
 * OP_LIST *defop_for_tn(TN *tn)
 *   Return last OP registered with defop_set that defines <tn> (if any).
 *
 * OP_LIST *defop_for_op(OP *op, UINT8 opnd, BOOL is_result)
 *   Return last OP registered with defop_set (if any) that defines the
 *   operand or result specified.  If the TN is dedicated or <include_
 *   assigned_registers> is true and the TN has a register assignment,
 *   we compare the register assignments; otherwise we compare the TNs.
 *   <is_result> is a generic variable which is set to TRUE for result.
 *
 * void defop_finish(void)
 *   Deallocate defop data structures.
 *
 * =====================================================================
 */

static TN_MAP defop_by_tn;
OP_LIST *defop_by_reg[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1];

// See above for specification.
inline void defop_init(void)
{
  defop_by_tn = TN_MAP_Create();
  bzero(defop_by_reg, sizeof(defop_by_reg));
}

//
// -----------------------------------------------------------------------
// Return register assignment (or REGISTER_UNDEFINED if none) for the <tn>.
// -----------------------------------------------------------------------
//
inline REGISTER defop_get_reg_for_tn(TN *tn)
{
  return TN_is_dedicated(tn) || include_assigned_registers ?
    TN_register(tn) : REGISTER_UNDEFINED;
}
  
// See above for specification.
inline void defop_set(OP *op)
{
  INT i;
  for (i = 0; i < OP_results(op); ++i) {
    TN *result_tn = OP_result(op,i);
    REGISTER reg = defop_get_reg_for_tn(result_tn);
    if (reg != REGISTER_UNDEFINED) {
      defop_by_reg[TN_register_class(result_tn)][reg] = 
	OP_LIST_Push(op, defop_by_reg[TN_register_class(result_tn)][reg],
		     &MEM_pu_pool);
    }
    CG_DEP_Add_Def(op, i, defop_by_tn, &MEM_pu_pool);
  }
#if defined(TARG_SL)
  TN_LIST * extra_results = op->extra_result;
  while( extra_results ) {
    TN *result_tn = TN_LIST_first( extra_results );
    REGISTER reg = defop_get_reg_for_tn(result_tn);
    if (reg != REGISTER_UNDEFINED) {
      defop_by_reg[TN_register_class(result_tn)][reg] = 
	OP_LIST_Push(op, defop_by_reg[TN_register_class(result_tn)][reg],
		     &MEM_pu_pool);
    }
    CG_DEP_Add_Def_By_TN(op, result_tn, defop_by_tn, &MEM_pu_pool);
    extra_results = TN_LIST_rest( extra_results );
  }
#endif
}

// See above for specification.
inline OP_LIST* defop_for_tn(TN *tn)
{
  return TN_is_register(tn) ? (OP_LIST *)CG_DEP_Get_Defs(tn, defop_by_tn) : NULL;
}

// See above for specification. 
inline OP_LIST* defop_for_op(OP *op, UINT8 res, BOOL is_result)
{
  TN *tn = is_result ? OP_result(op, res) : OP_opnd(op, res);
  if (is_result || TN_is_register(tn)) {
    if (TN_is_true_pred(tn)) {
      return NULL;
    } else {
      REGISTER reg = defop_get_reg_for_tn (tn);
      return (reg != REGISTER_UNDEFINED) ?
	defop_by_reg[TN_register_class(tn)][reg] :
	(OP_LIST *)CG_DEP_Get_Defs(tn, defop_by_tn);
    }
  } else {
    return NULL;
  }
}
  
// See above for specification.
inline void defop_finish(void)
{
  TN_MAP_Delete(defop_by_tn);
  defop_by_tn = NULL;
}



/* =====================================================================
 *		   MEMORY DEPENDENCE GRAPH BUILDING
 * =====================================================================
 */

/* Communicated results of analysis */
typedef enum { DISTINCT, IDENTICAL, OVERLAPPING, DONT_KNOW } SAME_ADDR_RESULT;

/* Section variables */
static UINT16 num_mem_ops;		/* memory ops in the block	     */
static OP **mem_ops;			/* vector of these mem ops	     */

static OP **xfer_ops;                   /* vector of the <xfer_op> ops       */
static INT32 **mem_op_lat_0;		/* latencies to 0-omega descendents  */
#define NO_DEP INT32_MIN		/* NO_DEP indicates the target is    */
					/*  not a descendent; note that      */
					/*  NO_DEP must be less than all     */
					/*  possible latencies (INT16)	     */

static void make_prefetch_arcs(OP *op, BB *bb)
/* --------------------------------------------------
 * Generate prefetch arcs for a give load store <op>.
 * --------------------------------------------------
 */
{
  if ( !CG_enable_prefetch ) return;
  if ( !cyclic) return;
  if ( !OP_store(op) && !OP_load(op)) return;

  WN *memwn = Get_WN_From_Memory_OP(op);
  PF_POINTER *pf_ptr = memwn ? (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,memwn) : NULL;
  if ( !pf_ptr) return;
  
  OP *pref_op;
  FOR_ALL_BB_OPs(bb, pref_op) {

    if (OP_prefetch(pref_op)) {
      WN *prefwn = Get_WN_From_Memory_OP(pref_op);
      PF_POINTER *pf_ptr2 = prefwn ? (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,prefwn) : NULL;

      if (pf_ptr2 == pf_ptr) {
	ARC *pref_arc;
	CG_DEP_KIND kind = OP_store(op) ? CG_DEP_PREFOUT : CG_DEP_PREFIN;
	pref_arc = new_arc(kind, pref_op, op, 0, 0, TRUE);
	
	INT pf_lat = (WN_pf_stride_2L(prefwn) != 0) ?  CG_L2_pf_latency : CG_L1_pf_latency;
	Set_ARC_latency(pref_arc, pf_lat);
      }
    }
  }
}

inline UINT8 addr_omega(OP *memop, UINT8 n)
/* -----------------------------------------------------------------------
 * Requires: OP_load(memop) || OP_store(memop)
 * Return the omega of the <n>th address operand in <memop>, or 0 if
 * <memop> isn't a loop OP.
 * -----------------------------------------------------------------------
 */
{
  Is_True(OP_Load(memop) || OP_store(memop), ("not a load or store"));
  return Is_CG_LOOP_Op(memop) ? OP_omega(memop, n) : 0;
}

inline BOOL addr_invariant_in_loop(OP *memop)
/* -----------------------------------------------------------------------
 * Requires: Incoming register arcs for <memop> have been built.
 *	     OP_load(memop) || OP_store(memop)
 *	     Is_CG_LOOP_Op(memop)
 * Return TRUE iff any of the address TNs for memory OP <memop> are
 * defined in the loop.  (Used to test for loop invariance of the
 * address.)
 * -----------------------------------------------------------------------
 */
{
  INT opnd_base   = OP_find_opnd_use( memop, OU_base   );
#ifdef TARG_X8664
  INT opnd_offset = OP_find_opnd_use( memop, OU_index );
#else
  INT opnd_offset = OP_find_opnd_use( memop, OU_offset );
#endif
  ARC_LIST *arcs = ARC_LIST_Find( OP_preds( memop ), CG_DEP_REGIN, DONT_CARE );
  while ( arcs != NULL ) {
    INT opnd = ARC_opnd( ARC_LIST_first( arcs ) );
    if ( opnd == opnd_base || opnd == opnd_offset )
      return FALSE;
    arcs = ARC_LIST_Find( ARC_LIST_rest( arcs ), CG_DEP_REGIN, DONT_CARE );
  }
  return TRUE;
}

/* --------------------------------------------------
 * Look through the OPS that perform a computation
 * to identify a constant offset that is added to a
 * base index variable of a load or store.
 * --------------------------------------------------
 */
static OP *addr_base_offset(OP *op, ST **initial_sym, ST **sym, TN **base_tn, INT64 *offset)
{
  TN *defop_base_tn = NULL;
  OP *defop;
  BB *bb = OP_bb(op);

  Is_True(OP_Load(op) || OP_store(op), ("not a load or store"));

  INT offset_num = OP_find_opnd_use (op, OU_offset);
  INT base_num   = OP_find_opnd_use (op, OU_base);
  INT result_num = -1;

  *initial_sym = NULL;
  *sym = NULL;
  *base_tn = OP_opnd(op, base_num);
  *offset = (offset_num < 0) ? 0 : TN_value(OP_opnd(op, offset_num));
  defop_base_tn = *base_tn;
  defop = op;

#ifdef TARG_X8664
  TN* ofst_tn = OP_opnd( op, offset_num );
  if( TN_is_symbol( ofst_tn ) ){
    *initial_sym = *sym = TN_var( ofst_tn );

    ST* root_sym = NULL;
    INT64 root_offset = 0;
    Base_Symbol_And_Offset( *sym, &root_sym, &root_offset);
    if (*sym != root_sym) {
      *sym = root_sym;
      *offset += root_offset;
    }
  }
#endif

  while (defop && defop_base_tn) {
    TN *defop_offset_tn = NULL;
    defop_base_tn = NULL;

    OP *new_defop = ARC_LIST_Find_Defining_Op(defop, base_num, CG_DEP_REGIN, base_num);
#ifdef TARG_X8664
    if( new_defop == NULL ){
      break;
    }
#endif
    if (new_defop == defop) {
      defop = NULL;
    } else defop = new_defop;
    if (defop &&
        (OP_bb(defop) == bb)) {

      if (OP_iadd(defop)) {

        result_num = 0;
        defop_offset_tn = OP_opnd(defop, 1);
#ifdef TARG_IA64    // in pathscale-3.0 is #ifdef KEY
        defop_base_tn = OP_opnd(defop, 2);
#elif TARG_LOONGSON
        defop_offset_tn = OP_opnd(defop, 2);
        defop_base_tn = OP_opnd(defop, 1);
#else
        defop_base_tn = OP_opnd(defop, 0);
#endif
        if (TN_is_constant(defop_offset_tn)) {
          *base_tn = defop_base_tn;
#ifdef TARG_IA64
	  base_num = 2;
#elif TARG_LOONGSON
          base_num = 1;
#else
          base_num = 0;
#endif
        } else if (TN_is_constant(defop_base_tn)) {
          *base_tn = defop_offset_tn;
          defop_offset_tn = defop_base_tn;
          defop_base_tn = *base_tn;
          base_num = 1;
        } else {
          defop_base_tn = NULL;
        }
      } else if (OP_memory(defop)) {
#if !defined(TARG_MIPS) && !defined(TARG_X8664) && !defined(TARG_PPC32)
        INT postinc_num = OP_find_opnd_use(defop, OU_postincr);
        base_num   = OP_find_opnd_use (defop, OU_base);
        if ((postinc_num >= 0) &&
            TNs_Are_Equivalent(*base_tn, OP_opnd(defop, base_num))) {
          result_num = 1;
          defop_offset_tn = OP_opnd(defop, postinc_num);
          defop_base_tn = *base_tn;
        } else {
          defop_base_tn = NULL;
        }
#endif
      } else if (OP_copy(defop)) {
        result_num = 0;
        defop_base_tn = OP_opnd(defop, OP_COPY_OPND);
        *base_tn = defop_base_tn;
#ifdef TARG_X8664
      } else if( OP_code(defop) == TOP_ldc64 ){
	base_num = 0;
	*base_tn = defop_base_tn = OP_opnd(defop,base_num);
	if( TN_is_symbol( defop_base_tn ) ){
	  *initial_sym = *sym = TN_var( defop_base_tn );
	  *offset += TN_offset( defop_base_tn );

	  ST* root_sym = NULL;
	  INT64 root_offset = 0;
	  Base_Symbol_And_Offset( *sym, &root_sym, &root_offset );
	  if( *sym != root_sym ){
	    *sym = root_sym;
	    *offset += root_offset;
	  }

	} else if( TN_has_value( defop_base_tn ) ){
	  *offset += TN_value( defop_base_tn );

	} else {
	  FmtAssert( false, ("NYI") );
	}
#endif
      } else {
        defop_base_tn = NULL;
#ifdef TARG_X8664
	int base = TOP_Find_Operand_Use( OP_code(defop), OU_base );
	if( base >= 0 &&
	    TOP_Find_Operand_Use( OP_code(defop), OU_index ) < 0 ){
	  base_num = base;
	  *base_tn = defop_base_tn = OP_opnd( defop, base );
	}
#endif
      }
    } else {
      defop_base_tn = NULL;
    }

    if (defop_offset_tn != NULL) {
      if (TN_is_symbol(defop_offset_tn)) {
        if (*sym != NULL) return defop;
        *offset += TN_offset(defop_offset_tn);
        *sym = TN_var(defop_offset_tn);
        *initial_sym = *sym;
#ifndef TARG_X8664
        defop_base_tn = NULL;
#endif
        ST *root_sym;
        INT64 root_offset;
        Base_Symbol_And_Offset( *sym, &root_sym, &root_offset);
        if (*sym != root_sym) {
          *sym = root_sym;
          *offset += root_offset;
        }
      } else if (TN_has_value(defop_offset_tn)) {
        *offset += TN_value(defop_offset_tn);
      }
    }
  }

  return defop;
}

/* --------------------------------------------------
 * Compare the constant offsets and variable indexs
 * of two memory operations to determine if there
 * is an overlap of the memory locations that each
 * sepcifies.
 * --------------------------------------------------
 */
static BOOL symbolic_addr_subtract(OP *pred_op, OP *succ_op, SAME_ADDR_RESULT *res)
{
    ST *pred_initial_sym;
    ST *succ_initial_sym;
    ST *pred_sym;
    ST *succ_sym;
    TN *pred_base;
    TN *succ_base;
    INT64 pred_offset;
    INT64 succ_offset;

    OP *pred_root = addr_base_offset(pred_op, &pred_initial_sym, &pred_sym, &pred_base, &pred_offset);
    OP *succ_root = addr_base_offset(succ_op, &succ_initial_sym, &succ_sym, &succ_base, &succ_offset);

    if ((pred_root != NULL) && (pred_base != NULL) &&
        (succ_root != NULL) && (succ_base != NULL)) {
      if ((pred_sym != NULL) && (succ_sym != NULL) &&
          (ST_sclass(pred_initial_sym) != SCLASS_UNKNOWN) && (ST_sclass(
succ_initial_sym) != SCLASS_UNKNOWN)) {
        if ((pred_sym != succ_sym) ||
            (ST_sclass(pred_initial_sym) != ST_sclass(succ_initial_sym)
#ifdef KEY   // Bug 14319.
	     && ST_sclass(pred_initial_sym) != SCLASS_EXTERN
	     && ST_sclass(succ_initial_sym) != SCLASS_EXTERN
#endif
	    )) {
         /* Different base symbols imply different locations. */
          *res = DISTINCT;
          return TRUE;  
        } else {
	  /* The base symbols are the same so we can use offsets to determine conflicts. */
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
	  /* Given the same symbols, we need to check base registers, too. */
	  if( pred_base != succ_base )
	    return FALSE;
#endif
        }
      } else if ((pred_root == succ_root) && (pred_base == succ_base)) {
       /* The index computations have a common origin so we can use offsets 
to determine conflicts. */
      } else {
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
	if( pred_base != succ_base ||
	    pred_root != pred_op   ||
	    succ_root != succ_op ){
	  return FALSE;
	}
#else
       /* We can't tell so give up. */
        return FALSE;
#endif
      }

     /* Use offsets and sizes to determine conflicts. */
      INT32 size1 = CGTARG_Mem_Ref_Bytes(pred_op);
      INT32 size2 = CGTARG_Mem_Ref_Bytes(succ_op);
      if (pred_offset == succ_offset) {
        *res = (size1 == size2) ? IDENTICAL : OVERLAPPING;
      } else if (pred_offset < succ_offset)  {
        *res = ((pred_offset + size1) <= succ_offset) ? DISTINCT : OVERLAPPING;
      } else {
        *res = ((succ_offset + size2) <= pred_offset) ? DISTINCT : OVERLAPPING;
      }
      return TRUE;
    }

  return FALSE;
}

static BOOL addr_subtract(OP *pred_op, OP *succ_op, TN *pred_tn, 
			  TN *succ_tn, INT64 *diff)
/* -----------------------------------------------------------------------
 * Requires: Incoming register arcs for <pred_op> and <succ_op> are built.
 *	     OP_load(pred_op) || OP_store(pred_op)
 *	     OP_load(succ_op) || OP_store(succ_op)
 *
 * Subtract <pred_tn> value with <succ_tn> value if they are constant
 * TNs. If the difference between them is always
 * constant, return TRUE, and set <*diff> to the difference (in bytes).
 * Otherwise return FALSE.
 * -----------------------------------------------------------------------
 */
{
  if (pred_tn && succ_tn) {
    if (TN_is_constant(pred_tn) && TN_is_constant(succ_tn)) {
      if (TN_has_value(pred_tn) && TN_has_value(succ_tn)) {
	*diff = TN_value(pred_tn) - TN_value(succ_tn);
	return TRUE;
      } else if (TN_is_symbol(pred_tn) && TN_is_symbol(succ_tn) &&
		 TN_var(pred_tn) == TN_var(succ_tn)) {
	*diff = TN_offset(pred_tn) - TN_offset(succ_tn);
	return TRUE;
#ifdef TARG_X8664
      } else if (TN_is_symbol(pred_tn) && TN_is_symbol(succ_tn) ){
	ST* pred_var = TN_var(pred_tn);
	ST* succ_var = TN_var(succ_tn);
	ST* base_st = NULL;
	ST* base_st1 = NULL;
	INT64 val = 0, val1 = 0;

	Base_Symbol_And_Offset( pred_var, &base_st, &val );
	Base_Symbol_And_Offset( succ_var, &base_st1, &val1 );

	if( ( base_st == SP_Sym || base_st == FP_Sym ) &&
	    base_st == base_st1 ){
	  val += TN_offset(pred_tn);
	  val1 += TN_offset(succ_tn);
	  *diff = val - val1;
	  return TRUE;
	}
#endif
      } else if (TN_is_label(pred_tn) && TN_is_label(succ_tn) &&
		 TN_label(pred_tn) == TN_label(succ_tn)) {
	*diff = TN_offset(pred_tn) - TN_offset(succ_tn);
	return TRUE;
      }
    } 
    else if (pred_tn == succ_tn) {
      INT pi = TN_Opernum_In_OP (pred_op, pred_tn);
      INT si = TN_Opernum_In_OP (succ_op, succ_tn);

      if (addr_omega(pred_op, pi) == addr_omega(succ_op, si)) {
	ARC *parc = ARC_LIST_Find_First(OP_preds(pred_op), CG_DEP_REGIN, pi);
	ARC *sarc = ARC_LIST_Find_First(OP_preds(succ_op), CG_DEP_REGIN, si);
	if ((!parc && !sarc) || 
	    (parc && sarc && ARC_pred(parc) == ARC_pred(sarc))) {
	  *diff = 0;
	  return TRUE;
	}
      }
    }
  }
  return FALSE;
}

static const char *same_addr_result_name(SAME_ADDR_RESULT res)
{
  switch (res) {
  case DISTINCT:
    return "DISTINCT";
  case IDENTICAL:
    return "IDENTICAL";
  case OVERLAPPING:
    return "OVERLAPPING";
  default:
    return "DONT_KNOW";
  }
}
		 
inline SAME_ADDR_RESULT analyze_overlap(OP *memop1, OP *memop2, INT64 diff)
/* -----------------------------------------------------------------------
 * Requires: <memop1> and <memop2> always access memory locations starting
 *	     <diff> bytes apart.
 * Returns the kind of overlap (IDENTICAL, DISTINCT, OVERLAPPING) for the
 * two memory OPs, given their offset from one another.
 * -----------------------------------------------------------------------
 */
{
  INT32 size1 = CGTARG_Mem_Ref_Bytes(memop1);
  INT32 size2 = CGTARG_Mem_Ref_Bytes(memop2);
  if (diff == 0) {
    return size1 == size2 ? IDENTICAL : OVERLAPPING;
  } else if (diff > 0 && size2 > diff || diff < 0 && size1 > -diff) {
    return OVERLAPPING;
  } else {
    return DISTINCT;
  }
}

BOOL 
CG_DEP_Mem_Ops_Offsets_Overlap(OP *memop1, OP *memop2, BOOL *identical)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  Is_True(OP_memory(memop1) && OP_memory(memop2), ("not a load or store"));

  TN *base1, *offset1;
  (void) OP_Base_Offset_TNs (memop1, &base1, &offset1);

  TN *base2, *offset2;
  (void) OP_Base_Offset_TNs (memop2, &base2, &offset2);

  Is_True(TNs_Are_Equivalent(base1, base2), 
	  ("Assumes that the base TNs are equivalent"));

  if (offset1 && offset2 && TN_has_value(offset1) && TN_has_value(offset2)) {
    INT64 diff = TN_value(offset1) - TN_value(offset2);
    switch (analyze_overlap(memop1, memop2, diff)) {
    case IDENTICAL:
      if (*identical) *identical = TRUE;
      return TRUE;
    case OVERLAPPING:
      if (*identical) *identical = FALSE;
      return TRUE;
    case DISTINCT:
      return FALSE;
    }
  }
  /* Fallback: may overlap */
  if (identical) *identical = FALSE;
  return TRUE;
}

#if defined(TARG_SL)
/*      Check whether the instruction load/store from Vbuf or Sbuf
 *      If two instruction access different buffer, then they are 
 *      certainly distinct, or else, we need more computation.
 */
static inline bool C2_LDST_Sbuf(OP *op)
{
  switch (OP_code(op)) {
  case TOP_c2_ldi_s_h_u:
  case TOP_c2_ldi_s_h:
  case TOP_c2_ldi_s_w:
  case TOP_c2_ldi_c:

  case TOP_c2_sti_c:
  case TOP_c2_sti_s_h:
  case TOP_c2_sti_s_w:
    return true;
    break;
  default:
    return false;
    break;
  }
  return false;
}

static inline bool C2_LDST_Vbuf(OP *op)
{
  switch (OP_code(op)) {
  case TOP_c2_ldi_v2g_b_u:
  case TOP_c2_ldi_v2g_b:
  case TOP_c2_ldi_v2g_h_u:
  case TOP_c2_ldi_v2g_h:
  case TOP_c2_ldi_v2g_w:
  case TOP_c2_ldi_v_b_u:
  case TOP_c2_ldi_v_b:
  case TOP_c2_ldi_v_h:
  case TOP_c2_ldi_v_w:
  case TOP_c2_ldi_v_m_b_u:
  case TOP_c2_ldi_v_m_b:
  case TOP_c2_ldi_v_m_h:
  case TOP_c2_ldi_v_m_w:

  case TOP_c2_sti_v_b:  
  case TOP_c2_sti_v_h:  
  case TOP_c2_sti_v_w:  
  case TOP_c2_sti_v_m_b:  
  case TOP_c2_sti_v_m_h:  
  case TOP_c2_sti_v_m_w:  
  case TOP_c2_sti_g2v_b:
  case TOP_c2_sti_g2v_h:
  case TOP_c2_sti_g2v_w:
    return true;
    break;
  default:
    return false;
    break;
  }
  return false;
}

#endif

static SAME_ADDR_RESULT CG_DEP_Address_Analyze(OP *pred_op, OP *succ_op)
/* -----------------------------------------------------------------------
 * Requires: Incoming register arcs for <pred_op> and <succ_op> are built.
 *	     OP_load(pred_op) || OP_store(pred_op)
 *	     OP_load(succ_op) || OP_store(succ_op)
 *
 * Perform simple alias analysis on <pred_op> and <succ_op>, returning
 * one of:
 *   DISTINCT:    references guaranteed to never overlap
 *   IDENTICAL:   references guaranteed to always be the same (addr and size)
 *   OVERLAPPING: references guaranteed to overlap, but aren't identical
 *   DONT_KNOW:   can't tell how/if the references are related?
 *
 * IMPORTANT: Does NOT deal with loop-carried dependences.  If used in
 *	      a loop context, NOT_ALIASED means the OPs aren't aliased
 *	      within a single iteration.
 *
 * TODO: Deal with loop-carried dependences.  
 *
 * -----------------------------------------------------------------------
 */
{
  SAME_ADDR_RESULT res = DONT_KNOW;
  INT64 diff0, diff1;

#if defined (TARG_SL)
  if (CG_ignore_mem_alias)
    return DONT_KNOW;
  if(OP_sl_special_ldst(pred_op) || OP_sl_special_ldst(succ_op))
    return DONT_KNOW;
#endif
  
#ifndef TARG_X8664
  /* Unaligned mem ops can be tricky and aren't very common.
   * Rely on WOPT/LNO analysis of these (in their original form,
   * before they were split into two unaligned ops).
   */
  if (OP_unalign_mem(pred_op) || OP_unalign_mem(succ_op))
    return DONT_KNOW;
#endif // !TARG_X8664

#ifdef TARG_X8664
  /* Stay away from floating-point mpy until it is well-understood.
   */
  if( ( OP_code(pred_op) == OP_code(succ_op) ) &&
      OP_load_exe(pred_op) &&
      OP_fmul(pred_op) ){
    return DONT_KNOW;
  }

  /* Check the index register and scale value first.
   */
  const TOP pred_top = OP_code(pred_op);
  const TOP succ_top = OP_code(succ_op);
  const int pred_index = TOP_Find_Operand_Use( pred_top, OU_index );
  const int succ_index = TOP_Find_Operand_Use( succ_top, OU_index );

  if( ( pred_index >= 0 && succ_index < 0 ) ||
      ( pred_index < 0  && succ_index >= 0 ) )
    return DONT_KNOW;

  if( pred_index >= 0 ){
    TN* pred_index_tn = OP_opnd( pred_op, pred_index );
    TN* succ_index_tn = OP_opnd( succ_op, succ_index );
    if( pred_index_tn != succ_index_tn )
      return DONT_KNOW;

    TN* pred_scale = OP_opnd( pred_op, TOP_Find_Operand_Use( pred_top, OU_index ) );
    TN* succ_scale = OP_opnd( succ_op, TOP_Find_Operand_Use( succ_top, OU_index ) );
    if( TN_value( pred_scale ) != TN_value( succ_scale ) )
      return DONT_KNOW;
  }
#endif // TARG_X8664

  if (symbolic_addr_subtract(pred_op, succ_op, &res)) {
    return res;
  }

  TN *pred_base, *pred_offset;
  (void) OP_Base_Offset_TNs (pred_op, &pred_base, &pred_offset);

  TN *succ_base, *succ_offset;
  (void) OP_Base_Offset_TNs (succ_op, &succ_base, &succ_offset);

  diff0 = 0;
	 /* for constant address */ 	
  if( pred_base && succ_base ) {
    if (addr_subtract(pred_op, succ_op, pred_base, succ_base, &diff0) &&
	addr_subtract(pred_op, succ_op, pred_offset, succ_offset, &diff1)) {
      return analyze_overlap(pred_op, succ_op, diff0 + diff1);
#ifdef TARG_IA64
    } else {

  	  /* for variable address */
      if (pred_base == succ_base && pred_offset == succ_offset) {
	if (CGTARG_Mem_Ref_Bytes (pred_op) == CGTARG_Mem_Ref_Bytes (succ_op)) {
	  return  IDENTICAL;
	} else  {
	  return  OVERLAPPING ;
	}
      }
#endif
    }
  }

  return DONT_KNOW;
}

inline BOOL under_same_cond_tn(OP *pred_op, OP *succ_op, UINT8 omega)
/* ---------------------------------------------------------------------
 * Return TRUE iff <pred_op> and <succ_op> are evaluated under the
 * same condition TN (possibly introduced by if-conversion).  We
 * assume <succ_op> is evaluated <omega> iterations later than <pred_op>.
 *
 * TODO: I think we're currently a little too conservative about some
 * cross-iteration dependences here, allowing them to stay definite
 * only when both ops are executed unconditionally.  We should also
 * allow them to stay definite when the condition (adjusted for the
 * different iteration) is the same.  But remember omega can be just
 * a minimum!
 * ---------------------------------------------------------------------
 */
{
#if defined(TARG_X8664) || defined(TARG_LOONGSON) // merged from pathscale-3.0
#ifdef KEY
  // CIO can not do WW elimination because MIPS is not predicated 
  // architecture
  if (omega >= 1 && OP_store(pred_op) && OP_store(succ_op))
    return FALSE;
  
  // At Key, we do not have a predicated architecture. We need to analyze
  // the BB to see, for a store, whether or not the input base/offset
  // operands are results of a conditional operation in the same iteration.
  // If they are, then we can not conclusively say that pred_op and succ_op
  // have same cond tn. So, we return FALSE.
  // Also, the caller uses LNO dependence graph info (which is old) 
  // and assumes that (after If-Conver) pred_op and succ_op have the 
  // predicates tied within.
  // only MEMIN & cross-iteration
  if (omega >= 1 && OP_store(pred_op) && OP_load(succ_op)) {
    BOOL not_predicated = TRUE;
    ARC_LIST *arcs = OP_preds(pred_op);
    while (arcs) {
      ARC *arc = ARC_LIST_first(arcs);
      arcs = ARC_LIST_rest(arcs);
      if ( ARC_kind(arc) == CG_DEP_REGIN &&
	   OP_cond_def(ARC_pred(arc))) {
	not_predicated = FALSE;
	break;
      }
    }
    if (!not_predicated)
      return FALSE;
  }      
#endif
#endif // TARG_X8664

  TN *pred_guard, *succ_guard;
  UINT8 pred_guard_omega, succ_guard_omega;
  BOOL pred_invguard, succ_invguard;

  Get_Memory_OP_Predicate_Info(pred_op, &pred_guard, &pred_guard_omega,
			       &pred_invguard);
  Get_Memory_OP_Predicate_Info(succ_op, &succ_guard, &succ_guard_omega,
			       &succ_invguard);

  return omega ? !pred_guard && !succ_guard :
    pred_guard == succ_guard && pred_invguard == succ_invguard &&
      pred_guard_omega == succ_guard_omega;
}

static void report_bad_mem_dep(BOOL result, BOOL definite,
			       OP *pred_op, OP *succ_op,
			       SAME_ADDR_RESULT same_addr_res,
			       const char *result_src,
			       BOOL new_result, BOOL new_definite)
/* -----------------------------------------------------------------------
 * Report bad memory dependence described by the arguments, but try to
 * avoid repeating the same warning.  If <pred_op> and <succ_op> are NULL,
 * flush the repeated-warnings buffer.
 * -----------------------------------------------------------------------
 */
{
  static BOOL last_result;
  static BOOL last_definite;
  static SAME_ADDR_RESULT last_same_addr_res;
  static const char *last_result_src;
  static BOOL last_new_result;
  static BOOL last_new_definite;
  static UINT64 repeat_count;

  WN *pred_wn;
  WN *succ_wn;
  char pbuf[512], sbuf[512];
  
  if (pred_op == NULL && succ_op == NULL) {
    /* Flush repeated-warnings buffer and return */
    if (repeat_count > 2)
      DevWarn("verify_mem: last warning reoccurred %lld times "
	      "(possibly for different OPs/BBs)", repeat_count);
    repeat_count = 0;
    return;
  }

  if (repeat_count > 0 &&
      result == last_result && same_addr_res == last_same_addr_res &&
      definite == last_definite && last_result_src == result_src &&
      new_result == last_new_result && new_definite == last_new_definite) {
    ++repeat_count;
    return;
  } else {
    if (repeat_count > 2)
      DevWarn("verify_mem: last warning reoccurred %lld times", repeat_count);
    last_result = result;
    last_same_addr_res = same_addr_res;
    last_result_src = result_src;
    last_definite = definite;
    last_new_result = new_result;
    last_new_definite = new_definite;
    repeat_count = 1;
  }

  pred_wn = OP_hoisted(pred_op) ? NULL : Get_WN_From_Memory_OP(pred_op);
  succ_wn = OP_hoisted(succ_op) ? NULL : Get_WN_From_Memory_OP(succ_op);	      
  pbuf[0] = sbuf[0] = '\0';
#ifdef Is_True_On
  if (pred_wn && Alias_Manager) Print_alias_info(pbuf, Alias_Manager, pred_wn);
  if (succ_wn && Alias_Manager) Print_alias_info(sbuf, Alias_Manager, succ_wn);
#endif
  DevWarn("verify_mem: CG addr analysis says %s but %s says %s ALIASED "
	  "for OPs %d (WN=0x%p%s%s) and %d (WN=0x%p%s%s) "
	  "in BB:%d (assuming %s ALIASED)",
	  same_addr_result_name(same_addr_res), result_src,
	  result ? (definite ? "DEFINITELY" : "POSSIBLY") : "NOT",
	  OP_map_idx(pred_op), pred_wn, pbuf[0] ? " " : "", pbuf,
	  OP_map_idx(succ_op), succ_wn, sbuf[0] ? " " : "", sbuf,
	  BB_id(OP_bb(pred_op)),
	  new_result ? (new_definite ? "DEFINITELY" : "POSSIBLY") : "NOT");
  if (tracing) {
    fprintf(TFile,"    ");Print_OP_No_SrcLine(pred_op);
    fprintf(TFile,"    ");Print_OP_No_SrcLine(succ_op);
  }

}

static BOOL verify_mem(BOOL              result, 
		       BOOL              *definite, 
		       UINT8             *omega,
		       OP                *pred_op, 
		       OP                *succ_op,
		       SAME_ADDR_RESULT  same_addr_res,
		       const char        *result_src)
/* -----------------------------------------------------------------------
 * For two memory ops <pred_op> and <succ_op>, compare the result from
 * CG's address analysis <same_addr_res> to the result from another alias
 * analysis source <result_src>, given by <result> and <*definite>.  If
 * they're consistent, return <result> and leave <*definite> alone.
 * Otherwise issue a warning and return a new result based on
 * <same_addr_res>, correcting <*definite> if necessary.
 *
 * We trust CG's addr analysis more since it's analyzing the OPs
 * themselves, while the other sources use older data whose validity is
 * more likely to be compromised by some code transformation along the
 * way.
 *
 * Possible dependences don't conflict with anything - only definite
 * dependence (and lack of dependence) conflict.
 *
 * NOTE: Disabled by -CG:verify_mem_deps=no or -CG:addr_analysis=no.
 * ----------------------------------------------------------------------- */
{
  /* Don't bother with non-definite MEMREAD arcs */
  if (!*definite && OP_load(pred_op) && OP_load(succ_op)) return FALSE;

#ifdef TARG_X8664
  /* Stay away from floating-point mpy until it is well-understood.
   */
  if( !*definite && OP_load_exe(pred_op) && OP_load_exe(succ_op) ){
    if( !OP_fmul(pred_op) ||
	!OP_fmul(succ_op) )
      return FALSE;
  }
#endif

  if (!CG_DEP_Verify_Mem_Deps || !CG_DEP_Addr_Analysis) return result;

  if (!result &&
      (same_addr_res == IDENTICAL &&
       under_same_cond_tn(pred_op, succ_op, omega ? *omega : 0) ||
       same_addr_res == OVERLAPPING)) {
    *definite = same_addr_res == IDENTICAL;
    /* Don't warn about missing read-read dependences.  The dependence
     * might be invalidated by an intervening write that we haven't yet
     * discovered (adjust_for_rw_elim hasn't been called yet). 
     */
    if (!OP_load(pred_op) || !OP_load(succ_op))
      report_bad_mem_dep(result, FALSE, pred_op, succ_op, same_addr_res,
			 result_src, TRUE, *definite);
    return TRUE;
  }

  if (result) {
    if (*definite) {
      if (same_addr_res == DISTINCT) {
	report_bad_mem_dep(result, *definite, pred_op, succ_op, same_addr_res,
			   result_src, FALSE, FALSE);
	return FALSE;
      } else if (same_addr_res == OVERLAPPING) {
	report_bad_mem_dep(result, *definite, pred_op, succ_op, same_addr_res,
			   result_src, TRUE, FALSE);
	*definite = FALSE;
	return TRUE;
      }
    } else {
      /*
       * Non-definite dependences don't conflict with anything, so don't
       * report an error, but return the result from our addr analysis
       * (if any).
       */
      switch (same_addr_res) {
      case DISTINCT:
	return FALSE;
      case IDENTICAL:
	*definite = TRUE;
	return TRUE;
      case OVERLAPPING:
	return TRUE;
      }
    }
  }
  
  return result;
}


#if defined(TARG_SL)
//====================================================================
// Real_Memory_WN
//
// For some intrinsics, the real memory operations are their parameter
// WNs.  It returns how many real memory WNs are put in real_mems
//
// This function is a specific function, and analyze WN case by case
// I need a data structure, which records which parameters of a INTRINSIC
// OP/CALL is memory operation. I hard code here.
//====================================================================
static INT32 Real_Memory_WN( OP *op, WN *input, WN **real_mems )
{
  real_mems[0] = NULL;
  real_mems[1] = NULL;

  // There are some memory access like gra spill, it has no WN map,
  // say input, so I set real_mems[] to NULL and return.
  if( !input )
    return 0;

  TOP opcode = OP_code(op);
  if(!(OP_c2_load(op) || OP_c2_store(op) ||
      OP_c3_load(op) || OP_c3_store(op))) {
    real_mems[0] = input;
    real_mems[1] = NULL;
    return 1;
  } 

  INT32 kids = WN_kid_count(input);
  INT32 id = 0;
  INT32 num = 0;
  for( ; id < kids; id++ ){
    WN *kid = WN_kid(input,id);
    OPERATOR kid_opr = WN_operator(kid);
    if( kid_opr == OPR_PARM ){
      if( WN_Parm_Dereference(kid) )
        real_mems[num++] = kid;
    }
  }

  Is_True( (num<=2), ("more than two load addr in an intrinsic") ); 
  
  if( num==0 ){
    real_mems[0] = input;
    real_mems[1] = NULL;
    return 1;
  }
 
  return num;
}


//====================================================================
// Aliased_By_WOPT 
//
// This function determines alias using alias manager in WOPT, between
// two WNs
// (1) verify_mem_res is TRUE if they alias, else it's set FALSE
//====================================================================
static void Aliased_By_WOPT( WN *pred_wn, WN *succ_wn, 
                             OP *pred_op, OP *succ_op,
                             SAME_ADDR_RESULT cg_result,
                             UINT8 *omega,
                             const char *info_src,
                             BOOL *verify_mem_res ,
                             BOOL *definite )
{
  // We now insert barrier (fwd_bar, bwd_bar) nodes while lowering
  // ALLOCA/DEALLOCA nodes. These barrier nodes carry aliasing info.
  // Check for those instances here and query the Aliased_with_region.
  if(succ_wn && OP_Alloca_Barrier(succ_op)){
  
    switch (Aliased_with_region(Alias_Manager, pred_wn, succ_wn, 
  			      READ_AND_WRITE)) {
    case SAME_LOCATION:
    case POSSIBLY_ALIASED:
      *definite = TRUE;
      *verify_mem_res = verify_mem(TRUE, definite, omega, pred_op, succ_op, 
  		                  cg_result, info_src);
      break;
    case NOT_ALIASED:
      *definite = FALSE;
      *verify_mem_res = verify_mem(FALSE, definite, omega, pred_op, succ_op, 
  		                  cg_result, info_src);
      break;
    default:
      Is_True(FALSE, ("bad return value from Aliased_with_region"));
    }
  }else{
    switch (Aliased(Alias_Manager, pred_wn, succ_wn, !cyclic)) {
    case SAME_LOCATION:
      *definite = TRUE;
      *verify_mem_res = verify_mem(TRUE, definite, omega, pred_op, succ_op, 
  		                  cg_result, info_src);
      break;
    case POSSIBLY_ALIASED:
      /* Definite iff addr analysis says definite */
      *definite = cg_result == IDENTICAL;
      *verify_mem_res = verify_mem(TRUE, definite, omega, pred_op, succ_op, 
  		                  cg_result, info_src);
      break;
    case NOT_ALIASED:
      *verify_mem_res = verify_mem(FALSE, definite, omega, pred_op, succ_op, 
  		                  cg_result, info_src);
      break;
    default:
      Is_True(FALSE, ("bad return value from Aliased"));
    }
  }
  return;
}
#endif // TARG_SL


#if !(defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS))
static 
#endif
BOOL get_mem_dep(OP *pred_op, OP *succ_op, BOOL *definite, UINT8 *omega)
/* -----------------------------------------------------------------------
 * Check whether <succ_op> can access the same location as <pred_op>
 * after <pred_op> is issued.  If <omega> is NULL, ignore loop-carried
 * dependences.  If a dependence exists, return TRUE, and set <*definite>
 * to TRUE iff <succ_op> definitely accesses the same location.  If
 * <omega> is NULL, ignore cyclic dependences (return FALSE);
 * otherwise set <*omega> to the dependence distance (which is a
 * minimum distance if the dependence isn't definite).  Otherwise
 * return FALSE.
 * -----------------------------------------------------------------------
 */
{
  WN *pred_wn, *succ_wn;
  UINT8 pred_unrollings = 0, succ_unrollings = 0;
#ifdef TARG_IA64
  BOOL lex_neg = (!OP_Precedes(pred_op, succ_op)) &&
      (OP_bb(pred_op) == OP_bb(succ_op)) ;
#else
  BOOL lex_neg = !OP_Precedes(pred_op, succ_op);
#endif
  SAME_ADDR_RESULT cg_result = DONT_KNOW;
  const char *info_src = "";
  UINT8 min_omega = 0;
  BOOL memread = OP_Load(pred_op) && OP_Load(succ_op);

  *definite = FALSE;

  /* Don't bother checking for lexicographically negative deps
   * when we're not looking for loop-carried deps.
   */
  if (omega == NULL && lex_neg)
    return FALSE;

  /* Prefetches don't alias anything (but see make_prefetch_arcs) */
  if (OP_prefetch(pred_op) || OP_prefetch(succ_op)) 
    return FALSE;

  /* no_alias ops don't alias anything by definition */
  if (OP_no_alias(pred_op) || OP_no_alias(succ_op)) 
    return FALSE;

#ifdef TARG_IA64
  /* Advanced loads do alias with succ stores.
   * Since we do not want the respective store of a ld.a
   * being moved forward across the ld.a. */
  if ((OP_load(pred_op) && CGTARG_Is_OP_Advanced_Load(pred_op)) &&
      (OP_store(succ_op)))
    return TRUE;
#else

  /* Advanced loads don't alias with anything. */
  if ((OP_load(pred_op) && CGTARG_Is_OP_Advanced_Load(pred_op)) ||
      (OP_load(succ_op) && CGTARG_Is_OP_Advanced_Load(succ_op)))
    return FALSE;
#endif

  /* Volatile ops are dependent on all other volatile OPs (but dependence
   * is marked as not definite to prevent removal by r/w elimination).
   */
  if ((OP_volatile(pred_op) && OP_volatile(succ_op))
#ifdef KEY	// bug 4850
      || (CGTARG_Is_OP_Barrier(pred_op) || CGTARG_Is_OP_Barrier(succ_op))
#endif
     ) {
    *definite = FALSE;
    if (omega) *omega = lex_neg;
    return TRUE;
  }

  /* Don't check for MEMREAD (load-load) dependence when:
   *  (a) we're not including MEMREAD arcs in the graph, or
   *  (b) either load is restoring a spill
   *  (c) don;t access same cache line
   */
  if (memread &&
      (!include_memread_arcs ||
       CGSPILL_Is_Spill_Op(pred_op) || CGSPILL_Is_Spill_Op(succ_op)))
    return FALSE;
      
  /* Spills are renamed in the cyclic world (by SWP), so don't check
   * for MEMANTI or MEMOUT dependences involving them.
   */
  if (cyclic && OP_store(succ_op) && CGSPILL_Is_Spill_Op(succ_op))
    return FALSE;

  /* If a memop has no cross-iteration dependence, then its
   * unrolled instance do not alias 
   */
  if (OP_no_ci_alias(pred_op) && OP_no_ci_alias(succ_op)) {
    if (OP_orig_idx(pred_op) == OP_orig_idx(succ_op) &&
	OP_unrolling(pred_op) != OP_unrolling(succ_op))
      return FALSE;
  }

#if defined(TARG_SL)
  BOOL pred_acc_vbuf=FALSE;
  BOOL succ_acc_vbuf=FALSE;
  if(OP_vbuf_load(pred_op)|| OP_vbuf_store(pred_op)) 
    pred_acc_vbuf=TRUE;
  if(OP_vbuf_load(succ_op)|| OP_vbuf_store(succ_op)) 
    succ_acc_vbuf=TRUE;

  if((pred_acc_vbuf && (!succ_acc_vbuf)) || (succ_acc_vbuf && (!pred_acc_vbuf)))
    return FALSE;

#endif

  /* Try to analyze the address TNs ourselves unless disabled.
   */
  if (CG_DEP_Addr_Analysis && !lex_neg &&
      (OP_Load(pred_op) || OP_store(pred_op)) &&
      (OP_Load(succ_op) || OP_store(succ_op))) {

    switch (cg_result = CG_DEP_Address_Analyze(pred_op, succ_op)) {
    case IDENTICAL:
      if (omega) *omega = lex_neg;
      *definite = under_same_cond_tn(pred_op, succ_op, omega ? *omega : 0);
      /* Don't include non-definite MEMREAD arcs */
      if (!*definite && memread) return FALSE;
      if (!CG_DEP_Verify_Mem_Deps) return TRUE;
      break;
    case OVERLAPPING:
#ifdef TARG_IA64
      *definite = TRUE;
#else
      *definite = FALSE;
#endif
      /* Don't include non-definite MEMREAD arcs */
      if (memread) return FALSE;
      if (omega) *omega = lex_neg;
      if (!CG_DEP_Verify_Mem_Deps) return TRUE;
      break;

    case DISTINCT:

      if (omega == NULL) {
	if (!CG_DEP_Verify_Mem_Deps) return FALSE;
      } else {
	/*
	 * same_addr doesn't detect loop-carried dependences, so
	 * all we know is there's no dependence within this iteration,
	 * so the dependence distance is at least one.
	 */
	cg_result = DONT_KNOW;
	min_omega = 1;
      }
      break;
    }
  }

  /* Our address analysis was disabled, didn't produce a definitive answer,
   * or we're verifying memory dependence info, so now resort to auxiliary
   * information to determine dependence relation.
   */
#if !defined(TARG_SL)
  pred_wn = OP_hoisted(pred_op) ? NULL : Get_WN_From_Memory_OP(pred_op);
  succ_wn = OP_hoisted(succ_op) ? NULL : Get_WN_From_Memory_OP(succ_op);
#else
  pred_wn = Get_WN_From_Memory_OP(pred_op);
  succ_wn = Get_WN_From_Memory_OP(succ_op);

  // for some intrinsics, the real memory operations are their parameter
  // WNs, like LDID
  WN *pred_real_mem_wns[2];
  WN *succ_real_mem_wns[2];
  INT pred_real_mem_num = 0;
  INT succ_real_mem_num = 0;
  pred_real_mem_num = Real_Memory_WN(pred_op, pred_wn, pred_real_mem_wns);
  succ_real_mem_num = Real_Memory_WN(succ_op, succ_wn, succ_real_mem_wns);
  // To make life easier, I only check the second real mem wn when using
  // wopt alias manager
  pred_wn = pred_real_mem_wns[0];
  succ_wn = succ_real_mem_wns[0];
#endif
	      
  if (OP_unroll_bb(pred_op))
    pred_unrollings = BB_unrollings(OP_unroll_bb(pred_op));
  if (OP_unroll_bb(succ_op))
    succ_unrollings = BB_unrollings(OP_unroll_bb(succ_op));

  if (pred_wn == NULL || succ_wn == NULL) {
    ST *pred_spill_st = CGSPILL_OP_Spill_Location(pred_op);
    ST *succ_spill_st = CGSPILL_OP_Spill_Location(succ_op);
    info_src = "CG (spill info)";
    if (pred_spill_st && succ_spill_st) {
      if (succ_spill_st == pred_spill_st) {
	/*
	 * They're in the same spill set, so there's a definite dependence.
	 */
        if ((cg_result == DISTINCT) &&
            (OP_load(pred_op) && OP_store(succ_op)) &&
            (OP_results(pred_op) == 1) &&
            TNs_Are_Equivalent(OP_result(pred_op,0),
                               OP_opnd(succ_op, TOP_Find_Operand_Use(OP_code(succ_op), OU_storeval)))) {
          /* A value is loaded from one location and stored to a spill location.
            This is not a memory dependency, but should show up as a REGIN dependency
            later on. */
          return verify_mem(FALSE, definite, omega, pred_op, succ_op, cg_result, info_src);
        }
	*definite = TRUE;
      } else {
	/*
	 * They're in different spill sets, so there's no dependence.
	 */
	return verify_mem(FALSE, definite, omega, pred_op, succ_op, cg_result,
			  info_src);
      }
    } else if (pred_spill_st || succ_spill_st) {

      /* One's a spill, and the other's not, so they're independent.  */
      return verify_mem(FALSE, definite, omega, pred_op, succ_op, cg_result,
			info_src);

    } else {

      /* Fallback: Treat as possibly aliased unless addr analysis says
       * they're definitely aliased.
       */
      *definite &= cg_result == IDENTICAL;

    }

  } else {

    /* Filter out a strange case */
    if (pred_wn == succ_wn && succ_op != pred_op &&
	(pred_unrollings < 2 && succ_unrollings < 2 ||
	 OP_orig_idx(pred_op) != OP_orig_idx(succ_op))) {
      info_src = "CG (shared-wn analysis)";
      /*
       * OPs are different parts of a multi-OP WN (like an unaligned
       * memory op), or one is an (non-unrolling) copy of the other.
       * We'll call this "maybe dependent".
       */
      *definite = FALSE;
      if (omega) *omega = MAX(lex_neg, min_omega);
      return verify_mem(TRUE, definite, omega, pred_op, succ_op, cg_result,
			info_src);
    }

    /* First try the LNO dependence graph */
    if (!CG_DEP_Ignore_LNO && Current_Dep_Graph != NULL &&
#ifdef TARG_X8664
	/* bug#1964
	 */
	!TOP_is_vector_op(OP_code(pred_op)) &&
	!TOP_is_vector_op(OP_code(succ_op)) &&
#endif
	OP_unroll_bb(pred_op) == OP_unroll_bb(succ_op)) {
      VINDEX16 v1 = Current_Dep_Graph->Get_Vertex(pred_wn);
      VINDEX16 v2 = Current_Dep_Graph->Get_Vertex(succ_wn);
      info_src = "LNO";
      if (v1 != 0 && v2 != 0) {
	EINDEX16 edge = Current_Dep_Graph->Get_Edge(v1, v2);
	BOOL is_must, is_distance;
	DIRECTION dir;
	INT32 dist;
#ifdef TARG_X8664
	if( Is_Target_32bit() &&
	    edge != 0         &&
	    OP_memory_hi( pred_op ) != OP_memory_hi( succ_op ) ){
	  edge = 0;
	}
#endif
	if (edge) {
	  DEP dep = Current_Dep_Graph->Dep(edge);
	  is_distance = DEP_IsDistance(dep);
	  dir = DEP_Direction(dep);
	  is_must = Current_Dep_Graph->Is_Must(edge);
	  dist = is_distance ? DEP_Distance(dep) : DEP_DistanceBound(dep);
	}
	if (!lex_neg && (edge == 0 || dist > 0)) {
	  /*
	   * LNO ignores lexical ordering so that WOPT doesn't need
	   * to worry about reversing edges when performing intra-
	   * iteration transformations.  This means we must check
	   * for intra-iteration dependences from v2 to v1 when possible
	   * and there's either no edge or only a cross-iteration edge
	   * from v1 to v2.
	   */
	  EINDEX16 inv_edge = Current_Dep_Graph->Get_Edge(v2, v1);
	  if (inv_edge) {
	    DEP inv_dep = Current_Dep_Graph->Dep(inv_edge);
	    INT32 inv_dist = DEP_IsDistance(inv_dep) ?
	      DEP_Distance(inv_dep) : DEP_DistanceBound(inv_dep);
	    if (inv_dist == 0) {
	      /*
	       * Found an intra-iteration edge from v2 to v1.  This can
	       * only be due to the LNO feature described above, so use
	       * this "inverted" edge instead.
	       */
	      BOOL inv_must = Current_Dep_Graph->Is_Must(inv_edge);
	      if (edge) {
		DEP udep = DEP_UnionDirection(inv_dep, dir);
		dir = DEP_Direction(udep);
		is_distance &= DEP_IsDistance(inv_dep) && inv_dist == dist;
		is_must &= inv_must && inv_dist == dist;
		dist = MIN(dist, inv_dist);
	      } else {
		dir = DEP_Direction(inv_dep);
		is_distance = DEP_IsDistance(inv_dep);
		is_must = inv_must;
		dist = inv_dist;
	      }
	      edge = inv_edge;
	    }
	  }
	}
	if (edge == 0) {
	  /* Independent */
	  return verify_mem(FALSE, definite, omega, pred_op, succ_op, cg_result,
			    info_src);
	} else {
	  if (dist < 0) {
	    DevWarn("LNO edge %d has dist of %d; ignoring", edge, dist);
	    return verify_mem(FALSE, definite, omega, pred_op, succ_op,
			      cg_result, info_src);
	  }
	  if (dir == DIR_POS && dist == 0) {
	    DevWarn("LNO POS(+) edge %d has dist of 0; assuming 1", edge);
	    dist = 1;
	  }
	  if (pred_unrollings > 1) {
	    INT32 adjust = dist + OP_unrolling(pred_op)-OP_unrolling(succ_op);
	    info_src = "LNO (+ CG unrolling info)";
	    if (is_distance && adjust % (INT32)pred_unrollings != 0) {
	      return verify_mem(FALSE, definite, omega, pred_op, succ_op,
				cg_result, info_src);
	    } else {
	      dist = adjust / (INT32)pred_unrollings;
	    }
	  }
	  *definite = cg_result == IDENTICAL && *definite ||
	    is_must && dist < MAX_OMEGA;
	  if (lex_neg && dist == 0) {
	    /* LNO can't exclude the zero-omega arcs, so we do. */
	    if (is_distance)
	      return verify_mem(FALSE, definite, omega, pred_op, succ_op,
				cg_result, info_src);
	    dist = 1;
	  }
	  if (omega == NULL && dist > 0)
	    return verify_mem(FALSE, definite, omega, pred_op, succ_op,
			      cg_result, info_src);
	  if (omega)
	    *omega = MIN(MAX(dist, min_omega), MAX_OMEGA);
	  if (*definite)
	    *definite = under_same_cond_tn(pred_op, succ_op,
					   omega ? *omega : 0);
	  return verify_mem(TRUE, definite, omega, pred_op, succ_op, cg_result,
			    info_src);
	}
      }
    }

    if (pred_wn == succ_wn) {

      /* Can't ask WOPT about deps crossing (even pre-unrolling) iteration
       * boundaries.  If the address is loop invariant, there's a definite
       * (+) dependence.  Otherwise, we'll have to assume a non-definite
       * dependence exists.
       */
      info_src = "CG (same-WN analysis, part II)";
      *definite = cg_result == IDENTICAL && *definite ||
	Is_CG_LOOP_Op(pred_op) && addr_invariant_in_loop(pred_op);

    } else {

      /* Now try the WOPT alias manager */
      info_src = "WOPT";
      if (Alias_Manager != NULL && !CG_DEP_Ignore_WOPT) {
#if !defined(TARG_SL)
	// We now insert barrier (fwd_bar, bwd_bar) nodes while lowering
	// ALLOCA/DEALLOCA nodes. These barrier nodes carry aliasing info.
	// Check for those instances here and query the Aliased_with_region.
	if (succ_wn && OP_Alloca_Barrier(succ_op)) {
	  switch (Aliased_with_region(Alias_Manager, pred_wn, succ_wn, 
				      READ_AND_WRITE)) {
	  case SAME_LOCATION:
	  case POSSIBLY_ALIASED:
	    *definite = TRUE;
	    break;
	  case NOT_ALIASED:
	    *definite = FALSE;
	    break;
	  default:
	    Is_True(FALSE, ("bad return value from Aliased_with_region"));
	  }
	}
	else {
	  BOOL ignore_loop_carried = 
	      (omega == NULL && OP_unrolling(pred_op)==OP_unrolling(succ_op));
	  switch (Aliased(Alias_Manager, pred_wn, succ_wn, ignore_loop_carried)) {
	  case SAME_LOCATION:
	    *definite = TRUE;
	    break;
	  case POSSIBLY_ALIASED:
	    /* Definite iff addr analysis says definite */
	    *definite = cg_result == IDENTICAL;
	    break;
	  case NOT_ALIASED:
	    return verify_mem(FALSE, definite, omega, pred_op, succ_op, 
			      cg_result, info_src);
	  default:
	    Is_True(FALSE, ("bad return value from Aliased"));
	  }

	  if (omega && 
	      OP_unrolling(pred_op)==OP_unrolling(succ_op) &&
	      min_omega <= 0 &&
	      Aliased(Alias_Manager, pred_wn, succ_wn, TRUE) == NOT_ALIASED) {
	       /* there is no loop-independent dependence between them. If there
	        * is loop carried dependence between them, the distance should 
	        * be at least one.
	        */
	    min_omega = 1;
	  }
	}
#else
        BOOL definite_1 = FALSE, definite_2 = FALSE, 
             definite_3 = FALSE, definite_4 = FALSE;
        BOOL ret_1 = FALSE, ret_2 = FALSE, ret_3 = FALSE, ret_4 = FALSE;
        BOOL res_1 = FALSE, res_2 = FALSE, res_3 = FALSE, res_4 = FALSE;
        Aliased_By_WOPT( pred_real_mem_wns[0], 
                                   succ_real_mem_wns[0], pred_op, succ_op,
                                   cg_result, omega, info_src, 
                                   &res_1, &definite_1 );
        if( pred_real_mem_wns[1] )
          Aliased_By_WOPT( pred_real_mem_wns[1], 
                                   succ_real_mem_wns[0], pred_op, succ_op,
                                   cg_result, omega, info_src, 
                                   &res_2, &definite_2 );
        if( succ_real_mem_wns[1] )
          Aliased_By_WOPT( pred_real_mem_wns[0], 
                                   succ_real_mem_wns[1], pred_op, succ_op,
                                   cg_result, omega, info_src, 
                                   &res_3, &definite_3 );
        if( pred_real_mem_wns[1] && succ_real_mem_wns[1] )
          Aliased_By_WOPT( pred_real_mem_wns[1], 
                                   succ_real_mem_wns[1], pred_op, succ_op,
                                   cg_result, omega, info_src, 
                                   &res_4, &definite_4 );
        *definite = definite_1 || definite_2 || definite_3 || definite_4;
        BOOL res = (res_1 || res_2 || res_3 || res_4);
        if (omega) 
	  *omega = MAX(lex_neg, min_omega);
        return res;
#endif // !TARG_SL
      } else {

	  /* Fallback: Treat as possibly aliased unless addr analysis says
	   * they're definitely aliased.
	   */
	  *definite &= cg_result == IDENTICAL;
	  
      }
    }
  }

  /* If we reach this point, we've determined that a dependence with a
   * minimum distance of 0 may exist.  Now adjust <*definite> for
   * unrolling if necessary.
   */
  if (*definite &&
      (OP_unroll_bb(pred_op) != OP_unroll_bb(succ_op) ||
       pred_unrollings > 1 &&
       OP_unrolling(pred_op) != OP_unrolling(succ_op)) &&
      (!Is_CG_LOOP_Op(pred_op) || !addr_invariant_in_loop(pred_op) ||
       !addr_invariant_in_loop(succ_op))) {
    /*
     * Since this may cross a pre-unrolling iteration boundary, and
     * the address TNs aren't all invariant, turn this into a
     * possible (not definite) dependence unless addr analysis says
     * otherwise.
     */
    *definite &= cg_result == IDENTICAL;
  }

  /* Set <*omega> according to OP ordering. */
  if (omega) *omega = MAX(lex_neg, min_omega);

  /*
   * Adjust definite deps for if-conversion: dependences are
   * definite only if they're executed under the same guard
   * condition.
   */
  if (*definite)
    *definite = under_same_cond_tn(pred_op, succ_op, omega ? *omega : 0);

  return verify_mem(TRUE, definite, omega, pred_op, succ_op, cg_result,
		    info_src);
}

BOOL 
CG_DEP_Call_Aliases(OP *call_op, OP *op, BOOL read, BOOL write)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface.
 * -----------------------------------------------------------------------
 */
{

  /* If -CG:ignore_wopt=on, be conservative, return TRUE. */
  if (CG_DEP_Ignore_WOPT) return TRUE;

  WN *call_wn = Get_WN_From_Memory_OP(call_op);
  WN *wn = Get_WN_From_Memory_OP(op);
  UINT8 i;
  static READ_WRITE rw[2][2] = {{NO_READ_NO_WRITE, WRITE},
				{READ, READ_AND_WRITE}};

  /* First check with Alias_Manager if possible */
  if (Alias_Manager && wn && call_wn) {
    BOOL alias_result = 
      ALIAS_RESULT_positive(Aliased_with_region(Alias_Manager, wn, call_wn,
						rw[read][write]));

    /* Check for uses/defs of dedicated TNs. */
    for (i = 0; i < OP_results(op); i++) {
      TN *res = OP_result(op,i);
      if (TN_is_dedicated(res)) {
	/* TODO: Construct dedicated PREG WN corresponding to <res>
	 *       and call Aliased_with_region.  For now, return 
	 *       conservative (TRUE) answer.  (Aliased_with_region
	 *       is doing the same now.)
	 */
	return TRUE;
      }
    }
    for (i = 0; i < OP_opnds(op); i++) {
      TN *opnd = OP_opnd(op, i);
      if (TN_is_dedicated(opnd)) {
	/* TODO: Construct dedicated PREG WN corresponding to <opnd>
	 *       and call Aliased_with_region.  For now, return 
	 *       conservative (TRUE) answer.  (Aliased_with_region
	 *       is doing the same now.)
	 */
	return TRUE;
      }
    }

    // If alias_result is negative, and we have guaranteed that there is
    // no reference to dedicated TNs, return NO ALIAS with CALL.
    if (!alias_result) return FALSE;
  }
  
  return TRUE;
}

BOOL 
CG_DEP_Mem_Ops_Alias(OP *memop1, OP *memop2, BOOL *identical)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  WN *wn1, *wn2;
  BB *bb1 = OP_bb(memop1), *bb2 = OP_bb(memop2);
  BOOL in_same_loop = BB_loop_head_bb(bb1) &&
    (bb1 == bb2 ||
     BB_loop_head_bb(bb1) == BB_loop_head_bb(bb2) &&
     !BB_unrolled_fully(bb1) && !BB_unrolled_fully(bb2));
  UINT8 unrollings1 = in_same_loop && OP_unroll_bb(memop1) ?
    BB_unrollings(bb1) : 0;
  UINT8 unrollings2 = in_same_loop && OP_unroll_bb(memop2) ?
    BB_unrollings(bb2) : 0;

  /* Handle easy special case (easy since we don't worry about cyclic
   * dependences ...).
   */
  if (memop1 == memop2) {
    if (identical) *identical = TRUE;
    return TRUE;
  }

  /* no_alias ops don't alias anything by definition */
  if (OP_no_alias(memop1) || OP_no_alias(memop2))
    return FALSE;

  wn1 = OP_hoisted(memop1) ? NULL : Get_WN_From_Memory_OP(memop1);
  wn2 = OP_hoisted(memop2) ? NULL : Get_WN_From_Memory_OP(memop2);	      

  if (wn1 == NULL || wn2 == NULL) {
    ST *spill_st1 = CGSPILL_OP_Spill_Location(memop1);
    ST *spill_st2 = CGSPILL_OP_Spill_Location(memop2);
    if (spill_st1 && spill_st2) {
      if (spill_st2 == spill_st1) {
	/*
	 * They're in the same spill set, so there's a definite dependence.
	 */
	if (identical) *identical = TRUE;
	return TRUE;
      } else {
	/*
	 * They're in different spill sets, so there's no dependence.
	 */
	return FALSE;
      }
    } else if (spill_st1 || spill_st2) {
      /* One's a spill, and the other's not, so they're independent.  */
#ifdef KEY
      // they may still be the same spill location because EBO's folding can
      // cause CGSPILL_OP_Spill_Location to fail to recognize a spill op 
      if (OP_opnd(memop1, TOP_Find_Operand_Use(OP_code(memop1), OU_offset)) ==
          OP_opnd(memop2, TOP_Find_Operand_Use(OP_code(memop2), OU_offset))) {
	if (identical) *identical = TRUE;
	return TRUE;
      }
      else
#endif
      return FALSE;
    }

  } else {

    /* Filter out a strange case */
    if (wn1 == wn2 && memop2 != memop1 &&
	(unrollings1 < 2 && unrollings2 < 2 ||
	 OP_orig_idx(memop1) != OP_orig_idx(memop2))) {
      /*
       * OPs are different parts of a multi-OP WN (like an unaligned
       * memory op), or one is an (non-unrolling) copy of the other.
       * We'll call this "maybe dependent".
       */
      if (identical) *identical = FALSE;
      return TRUE;
    }

    /* TODO: Consult LNO dep graph for ops in same loop.  Must deal with
     *       distance component for unrolled ops ...
     */

    if (wn1 == wn2) {

      /* Can't ask WOPT about deps crossing (even pre-unrolling) iteration
       * boundaries.  We'll have to assume aliasing is possible.
       */
      if (identical) *identical = FALSE;
      return TRUE;

    } else {

      /* Now try the WOPT alias manager */
      if (Alias_Manager != NULL && !CG_DEP_Ignore_WOPT) {
	switch (Aliased(Alias_Manager, wn1, wn2)) {
	case SAME_LOCATION:
	  if (identical)
	    *identical = (OP_unroll_bb(memop1) == OP_unroll_bb(memop2) &&
			  unrollings1 < 2 ||
			  OP_unrolling(memop1) == OP_unrolling(memop2)) &&
			 under_same_cond_tn(memop1, memop2, 0);
	  return TRUE;
	case POSSIBLY_ALIASED:
	  if (identical) *identical = FALSE;
	  return TRUE;
	case NOT_ALIASED:
	  return FALSE;
	default:
	  Is_True(FALSE, ("bad return value from Aliased"));
	}
      }
    }
  }

  /* Fallback: Treat as possibly aliased.
   */
  if (identical) *identical = FALSE;
  return TRUE;
}


// ======================================================================
// Can_OP_Move_Across_Call
// returns TRUE if <cur_op> can be moved across <call_op>. It first 
// eliminates instructions which are hazardous to move across calls. It
// then calls CG_DEP_Call_Aliases routine (cg_dep_graph.cxx) which inturn
// uses WOPT alias_manager to detect the legality of movement. uses <forw>
// and OP_Follows to reduce unnecessary comparisons. <Ignore_TN_Dep> tells
// whether to ignore any TN dependences (i.e only include register ones.)
// ======================================================================
BOOL
CG_DEP_Can_OP_Move_Across_Call(OP *cur_op, OP *call_op, BOOL forw, 
			       BOOL Ignore_TN_Dep) 
{
  if (call_op) {
    BOOL in_same_bb = 	OP_bb(cur_op) == OP_bb(call_op);
    // check to identify cases where both the <call_op> and <cur_op> are
    // in the same bb and <cur_op> needs to move past it.
    BOOL mov_across_call = in_same_bb && 
                           ((forw && OP_Follows(cur_op, call_op)) ||
                            (!forw && OP_Follows(call_op, cur_op)));
    if ((mov_across_call || (!mov_across_call && !in_same_bb)) && 
	!GCM_Motion_Across_Calls) return FALSE;
  }

  //TODO: this may be very conservative. need to verify.
  if (call_op) {

    // Prune out the obvious cases.
    if (OP_defs_fcr(cur_op) || OP_defs_fcc(cur_op) || OP_refs_fcr(cur_op))
      return FALSE;

    INT i;
    for (i = 0; i < OP_results(cur_op); i++) {
      TN *result = OP_result(cur_op,i);
      if (Ignore_TN_Dep) {
	REGISTER reg = TN_register(result);
#ifdef TARG_IA64
	if (reg == REGISTER_UNDEFINED) continue;
	// Is_True(reg != REGISTER_UNDEFINED, ("reg should not be REGISTER_UNDEFINED"));
#endif
#ifdef TARG_LOONGSON
     	if (!reg) return FALSE;
#endif
	ISA_REGISTER_CLASS rclass = TN_register_class (result);
	 
	// prune out regs which have implicit meaning.
	if(REGISTER_SET_MemberP(REGISTER_CLASS_function_value(rclass), reg) ||
	   REGISTER_SET_MemberP(REGISTER_CLASS_function_argument(rclass),
				reg) ||
	   REGISTER_SET_MemberP(REGISTER_CLASS_caller_saves(rclass), reg))
	  return FALSE;

	// #802534: The output register portion of the allocated frame
	// doesn't get preserved across calls. Check for this condition.

#ifdef HAS_STACKED_REGISTERS
	if (REGISTER_Is_Stacked_Output(rclass, reg))
	  return FALSE;
#endif
      } else {
	// TODO: start with disallowing dedicated TN's and refine it
	// further.
	if (TN_is_dedicated(result)) 
	  return FALSE;
      }
    }

    for (i = 0; i < OP_opnds(cur_op); i++) {
      TN *opnd_tn = OP_opnd(cur_op, i);
      if (TN_is_constant(opnd_tn)) continue;
      if (Ignore_TN_Dep) {
	REGISTER opnd_reg = TN_register(opnd_tn);
#ifdef TARG_IA64
	if (opnd_reg == REGISTER_UNDEFINED) continue;
	// Is_True(opnd_reg != REGISTER_UNDEFINED, ("reg should not be REGISTER_UNDEFINED"));
#endif
	ISA_REGISTER_CLASS opnd_cl = TN_register_class (opnd_tn);

	// prune out regs which have implicit meaning.
	if(REGISTER_SET_MemberP(REGISTER_CLASS_function_value(opnd_cl), 
				opnd_reg) ||
	   REGISTER_SET_MemberP(REGISTER_CLASS_function_argument(opnd_cl),
				opnd_reg) ||
	   REGISTER_SET_MemberP(REGISTER_CLASS_caller_saves(opnd_cl),
				opnd_reg))
	  return FALSE;
      } else {
	// TODO: start with disallowing dedicated TN's and refine it
	// further.
	if (TN_is_dedicated(opnd_tn)) 
	  return FALSE;
      }
    }
  }

  // now call the WOPT Alias Manager
  if (call_op && CG_DEP_Call_Aliases(call_op, cur_op, TRUE, TRUE))
    return FALSE;
  
  return TRUE;
}

inline ARC_LIST *first_mem_arc(ARC_LIST *arcs)
/* -----------------------------------------------------------------------
 * Searches for first memory arc in <arcs>.  If found, the ARC_LIST with
 * the memory arc in the first position is returned; otherwise NULL is
 * returned.
 * -----------------------------------------------------------------------
 */
{
  while (arcs) {
    ARC *arc = ARC_LIST_first(arcs);
    if (ARC_is_mem(arc))
      return arcs;
    arcs = ARC_LIST_rest(arcs);
  }
  return NULL;
}

inline ARC_LIST *first_definite_mem_arc(ARC_LIST *arcs)
/* -----------------------------------------------------------------------
 * Searches for first definite memory arc in <arcs>.  If found, the
 * ARC_LIST with the memory arc in the first position is returned;
 * otherwise NULL is returned.
 * -----------------------------------------------------------------------
 */
{
  while (arcs) {
    ARC *arc = ARC_LIST_first(arcs);
    if (ARC_is_mem(arc) && ARC_is_definite(arc))
      return arcs;
    arcs = ARC_LIST_rest(arcs);
  }
  return NULL;
}

inline INT16 get_bb_idx(BB *bb, std::list<BB*> bb_list)
{
  std::list<BB*>::iterator bb_iter;
  INT idx = -1;
  
  // Assumes <bb> is present in <bb_list>.
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bb_list, bb_iter) {
    ++idx;
    if (*bb_iter == bb) break;
  }

  return idx;
}

//
// -----------------------------------------------------------------------
// This routine adds POSTBR arcs between a branch and succeeding OPs in
// an extended block.
// -----------------------------------------------------------------------
//
void 
Add_BRANCH_Arcs(BB* bb, std::list<BB*> bb_list, BOOL include_latency)
{

  INT16 pred_idx;
  INT16 bb_idx = get_bb_idx(bb, bb_list);
  OP *op, *cur_xfer_op;
  
  FOR_ALL_BB_OPs(bb, op) {

    // Insert PREBR dependences.
    for (pred_idx = bb_idx; pred_idx < bb_list.size(); pred_idx++) {
      cur_xfer_op = xfer_ops[pred_idx];
      if (cur_xfer_op &&  cur_xfer_op != op) {
	
	// Always build an edge to the last control point.
	if ((pred_idx == bb_list.size() - 1) ||
	    is_xfer_depndnce_reqd(op, cur_xfer_op)) {
	  if (include_latency) {
	    new_arc_with_latency(CG_DEP_PREBR, op, cur_xfer_op, 0,0,0, FALSE);
	  } else {
	    new_arc(CG_DEP_PREBR, op, cur_xfer_op, 0, 0, FALSE);
	  }
	  break;
	}

	// Build any cross-bb MISC dependences here. 
	if (CGTARG_Dependence_Required(op, cur_xfer_op)) {
	  if (include_latency) {
	    new_arc_with_latency(CG_DEP_MISC, op, cur_xfer_op, 0, 0, 0, FALSE);
	  } else {
	    new_arc(CG_DEP_MISC, op, cur_xfer_op, 0, 0, FALSE);
	  }
	  break;
	}
      }
    } // PREBR dependences

    // Insert POSTBR dependences.
    for (pred_idx = bb_idx - 1; pred_idx >= 0 ; pred_idx--) {
      cur_xfer_op = xfer_ops[pred_idx];
      if (cur_xfer_op) {

	if (is_xfer_depndnce_reqd(op, cur_xfer_op)) {
	  if (include_latency) {
	    new_arc_with_latency(CG_DEP_POSTBR, cur_xfer_op, op, 0,0,0,FALSE);
	  } else {
	    new_arc(CG_DEP_POSTBR, cur_xfer_op, op, 0, 0, FALSE);
	  }
	  break;
	}

	// Build any cross-bb MISC dependences here.
	if (CGTARG_Dependence_Required(cur_xfer_op, op)) {
	  if (include_latency) {
	    new_arc_with_latency(CG_DEP_MISC, cur_xfer_op, op, 0, 0, 0,FALSE);
	  } else {
	    new_arc(CG_DEP_MISC, cur_xfer_op, op, 0, 0, FALSE);
	  }
	  break;
	}
      }
    } // POSTBR dependences

  } // FOR_ALL_BB_OPs loop
}

//
// -----------------------------------------------------------------------
// This routine adds any target-dependent MISC arcs between any two
// concerned OPs that may be required.
// -----------------------------------------------------------------------
//
void 
Add_MISC_Arcs(BB* bb)
{

  OP *op;
  OP *prev_op, *next_op;

  FOR_ALL_BB_OPs(bb, op) {

    // If <cyclic> graph, need to build back-edges as well.
    if (cyclic) {
      for (prev_op = BB_last_op(bb); prev_op && prev_op != op; 
	   prev_op = OP_prev(prev_op)) {

	// Since, we don;t know the exact omega values, since these are
	// not real register dependences, set the omega value conservative
	// to 1.

	if (CGTARG_Dependence_Required(prev_op, op)) {
	  new_arc_with_latency(CG_DEP_MISC, prev_op, op, 0, 1, 0, FALSE);
	}
      }
    }

    // Build the acyclic dependence edge here.

    for (next_op = OP_next(op); next_op; next_op = OP_next(next_op)) {
      if (CGTARG_Dependence_Required(op, next_op)) {
#ifdef TARG_IA64
          new_arc(CG_DEP_MISC, op, next_op, 0, 0, FALSE);	
#else
	  new_arc_with_latency(CG_DEP_MISC, op, next_op, 0, 0, 0, FALSE);
#endif
      }
    }
  } // FOR_ALL_BB_OPs loop
}

inline BOOL is_closer_loop_succ(OP *pred,
				OP *succ1, UINT8 omega1,
				OP *succ2, UINT8 omega2)
/* -----------------------------------------------------------------------
 * Returns TRUE iff the path from <pred> to <succ1> crossing the loop 
 * back-edge <omega1> times is shorter than the path from <pred> to
 * <succ2> crossing the loop back-edge <omega2> times.
 * -----------------------------------------------------------------------
 */
{
  BOOL closer = omega1 < omega2 ||
    omega1 == omega2 && OP_Ordering(succ1, pred) < OP_Ordering(succ2, pred);

  /* Check for meaningful omegas */
  Is_True(OP_Follows(succ1, pred) || omega1 > 0, ("omega1 can't be 0"));
  Is_True(OP_Follows(succ2, pred) || omega2 > 0, ("omega2 can't be 0"));

  return closer;
}

inline BOOL is_closer_succ(OP *pred, OP *succ1, OP *succ2)
/* -----------------------------------------------------------------------
 * Returns TRUE if the path from <pred> to <succ1> is shorter than the
 * path <pred> to <succ2>.  Each path is assumed to cross the loop back-
 * edge exactly once if the successor does not follow <pred>.
 * -----------------------------------------------------------------------
 */
{
  BOOL closer = is_closer_loop_succ(pred,
				    succ1, !OP_Follows(succ1, pred),
				    succ2, !OP_Follows(succ2, pred));
  return closer;
}

inline OP *closer_succ(OP *pred, OP *succ1, OP *succ2)
/* -----------------------------------------------------------------------
 * Returns either <succ1> or <succ2>, whichever is closer to <pred>.
 * See is_closer_succ, above, for definition of "closer".
 * -----------------------------------------------------------------------
 */
{
  OP *closer = is_closer_succ(pred, succ1, succ2) ? succ1 : succ2;
  return closer;
}

inline BOOL succ_arc_shorter(ARC *arc1, ARC *arc2)
/* -----------------------------------------------------------------------
 * Requires: ARC_pred(arc1) == ARC_pred(arc2)
 * Returns TRUE iff <arc1> is shorter than <arc2> where arc length is
 * measured by number of intervening instructions (least significant)
 * and number of iterations crossed (most significant).
 * -----------------------------------------------------------------------
 */
{
  BOOL closer = is_closer_loop_succ(ARC_pred(arc1),
				    ARC_succ(arc1), ARC_omega(arc1),
				    ARC_succ(arc2), ARC_omega(arc2));
  Is_True(ARC_pred(arc1) == ARC_pred(arc2), ("arc preds not the same"));
  return closer;
}

ARC *shorter_succ_arc(ARC *arc1, ARC *arc2)
/* -----------------------------------------------------------------------
 * Requires: ARC_pred(arc1) == ARC_pred(arc2)
 * Returns the shorter of <arc1> and <arc2>.  See succ_arc_shorter, above.
 * -----------------------------------------------------------------------
 */
{
  ARC *shorter = succ_arc_shorter(arc1, arc2) ? arc1 : arc2;
  return shorter;
}

inline BOOL is_closer_loop_pred(OP *succ,
				OP *pred1, UINT8 omega1,
				OP *pred2, UINT8 omega2)
/* -----------------------------------------------------------------------
 * Returns TRUE iff the path from <pred1> to <succ> crossing the loop 
 * back-edge <omega1> times is shorter than the path from <pred2> to
 * <succ> crossing the loop back-edge <omega2> times.
 * -----------------------------------------------------------------------
 */
{
  BOOL closer = omega1 < omega2 ||
    omega1 == omega2 && OP_Ordering(succ, pred1) < OP_Ordering(succ, pred2);

  /* Check for meaningful omegas */
  Is_True(OP_Follows(succ, pred1) || omega1 > 0, ("omega1 can't be 0"));
  Is_True(OP_Follows(succ, pred2) || omega2 > 0, ("omega2 can't be 0"));

  return closer;
}

inline BOOL pred_arc_shorter(ARC *arc1, ARC *arc2)
/* -----------------------------------------------------------------------
 * Requires: ARC_succ(arc1) == ARC_succ(arc2)
 * Returns TRUE iff <arc1> is shorter than <arc2> where arc length is
 * measured by number of intervening instructions (least significant)
 * and number of iterations crossed (most significant).
 * -----------------------------------------------------------------------
 */
{
  BOOL closer = is_closer_loop_pred(ARC_succ(arc1),
				    ARC_pred(arc1), ARC_omega(arc1),
				    ARC_pred(arc2), ARC_omega(arc2));
  Is_True(ARC_succ(arc1) == ARC_succ(arc2), ("arc succs not the same"));
  return closer;
}

inline ARC *shorter_pred_arc(ARC *arc1, ARC *arc2)
/* -----------------------------------------------------------------------
 * Requires: ARC_succ(arc1) == ARC_succ(arc2)
 * Returns the shorter of <arc1> and <arc2>.  See pred_arc_shorter, above.
 * -----------------------------------------------------------------------
 */
{
  ARC *shorter = pred_arc_shorter(arc1, arc2) ? arc1 : arc2;
  return shorter;
}

static void adjust_arc_for_rw_elim(ARC *arc, BOOL is_succ, ARC *shortest,
				   ARC *shortest_to_from_store)
/* -----------------------------------------------------------------------
 * See adjust_for_rw_elim below.  This does the work for a single arc.
 * -----------------------------------------------------------------------
 */
{
  OP *pred = ARC_pred(arc);
  OP *succ = ARC_succ(arc);
  CG_DEP_KIND kind = ARC_kind(arc);

  /* TEMPORARY WORKAROUND for PV 706612.  Don't allow definite
   * MEMIN/MEMREAD arcs if the TNs being referenced have different
   * types (FP/int) or sizes.  Also don't allow definite MEMOUT arcs
   * if the predecessor stores more bytes than the successor.
   */
  if (kind == CG_DEP_MEMIN &&
      ((
#ifdef KEY
	// Bug088
	( OP_results(succ) > 0 ) &&
#endif	
	(TN_is_float(OP_opnd(pred, 0)) ^ TN_is_float(OP_result(succ,0 /*???*/)))) ||
       CGTARG_Mem_Ref_Bytes(pred) != CGTARG_Mem_Ref_Bytes(succ))) {
    /* invalidate for r/w elimination */
    Set_ARC_is_definite(arc, FALSE);
  } else if (kind == CG_DEP_MEMREAD &&
#ifdef KEY
// Bug #517
	     OP_results(succ) > 0 &&
	     OP_results(pred) > 0 &&
#endif
	     ((TN_is_float(OP_result(pred,0 /*???*/)) ^ TN_is_float(OP_result(succ,0 /*???*/))) ||
	      CGTARG_Mem_Ref_Bytes(pred) != CGTARG_Mem_Ref_Bytes(succ))) {
    /* non-definite MEMREAD arcs aren't useful */
    detach_arc(arc);
    delete_arc(arc);
  } else if (kind == CG_DEP_MEMOUT &&
	     CGTARG_Mem_Ref_Bytes(pred) > CGTARG_Mem_Ref_Bytes(succ)) {
    /* invalidate for w/w elimination */
    Set_ARC_is_definite(arc, FALSE);
  } else

    /* END OF TEMPORARY WORKAROUND for PV 706612 */

  if (OP_unalign_mem(pred) || OP_unalign_mem(succ) ||
      kind == CG_DEP_MEMOUT && arc != shortest ||
      shortest_to_from_store &&
      (is_succ && succ_arc_shorter(shortest_to_from_store, arc) ||
       !is_succ && pred_arc_shorter(shortest_to_from_store, arc))) {
    if (kind == CG_DEP_MEMREAD) {
      /* non-definite MEMREAD arcs aren't useful */
      detach_arc(arc);
      delete_arc(arc);
    } else {
      /* invalidate for r/w elimination */
      Set_ARC_is_definite(arc, FALSE);
    }
  }
}

void adjust_for_rw_elim(ARC_LIST *arcs, UINT32 num_definite_arcs,
			       ARC *shortest, ARC *shortest_to_from_store)
/* -----------------------------------------------------------------------
 * Some arcs are marked "definite" for use by r/w elimination.  Find
 * the ones in <arcs> that can't be used for this because of an
 * intervening dependent memory operation and mark them "not
 * definite" (or remove MEMREAD arcs - non-definite ones aren't useful).
 * Write/write elimination (enabled by definite MEMOUT arcs) is
 * invalidated by any intervening memory dependence (read or
 * write) since that stops us from removing the earlier store.
 * Read/write or read/read elimination is invalidated only by an
 * intervening store.
 *
 * The following parameters must be specified (these can be found by
 * walking <arcs>, but since our clients have just done that, we'll
 * have them pass these in to minimize compile speed):
 *   <num_definite_arcs> is the number of definite arcs in <arcs>
 *   <shortest> is the shortest arc in <arcs>, as defined by
 *	{succ,pred}_arc_shorter
 *   <shortest> is the shortest arc in <arcs>, as defined by
 *	{succ,pred}_arc_shorter, going {to,from} a store
 *
 * TODO: We invalidate arcs for R/W elim by marking them "not
 * definite", but this isn't accurate.  There's still a definite
 * dependence between the two memory references; it's just that we
 * can't guarantee they're accessing the same value.  For now, we're
 * only using the definite attribute for R/W elimination, so there's
 * no need to make a distinction.  But it would be cleaner and more
 * accurate to introduce a separate attribute (same_val?) for this
 * purpose.  Can get a spare bit from the ARC_omega field.
 * -----------------------------------------------------------------------
 */
{
  BOOL are_succs = ARC_LIST_is_succ_list(arcs);
  while (num_definite_arcs-- > 0) {
    ARC *arc;
    arcs = first_definite_mem_arc(arcs);
    Is_True(arcs, ("num_definite_arcs is too high"));
    arc = ARC_LIST_first(arcs);
    arcs = ARC_LIST_rest(arcs);
    adjust_arc_for_rw_elim(arc, are_succs, shortest, shortest_to_from_store);
  }
  Is_True(first_definite_mem_arc(arcs) == NULL,
	  ("num_definite_arcs is too low"));
}

void add_mem_arcs_from(UINT16 op_idx)
/* -----------------------------------------------------------------------
 * Requires: mem_ops[], num_mem_ops setup correctly
 *	     mem_op_lat_0[] setup correctly if CG_DEP_Mem_Arc_Pruning > 0.
 *
 * Find the memory successors of mem_ops[op_idx] and create memory
 * dependence arcs to them.  Pruning controlled by CG_DEP_Mem_Arc_Pruning.
 * -----------------------------------------------------------------------
 */
{
  OP *op = mem_ops[op_idx];
  UINT16 succ_idx, s, num_definite_arcs = 0;
  ARC *shortest = NULL, *shortest_to_store = NULL;
  /* Index of first possible memory successor of <op>. */
  UINT16 first_poss_succ_idx = cyclic ? 0 : op_idx+1;
  /* Max number of 0-omega successors of <op>. */
  UINT16 num_poss_0_succs = num_mem_ops - op_idx - 1;
  BOOL found_definite_memread_succ = FALSE;

  /* Note: <mem_op_lat_0> is NULL when not pruning. */
  if (mem_op_lat_0) {
    if (mem_op_lat_0[op_idx]) {
      /* Already visited this <op_idx>, so return. */
      return;
    } else {
      /* Since 0-omega successors always follow <op>, allocate <mem_op_lat_0>
       * as the upper-right half (triangle) of a <num_mem_ops> by <num_mem_ops>
       * matrix.  This uses half the memory, though it means that matrix
       * entry [x][y] must be accessed by mem_op_lat_0[x][y-x-1].
       */
      mem_op_lat_0[op_idx] = TYPE_L_ALLOC_N(INT32, num_poss_0_succs);
      for (s = 0; s < num_poss_0_succs; s++) mem_op_lat_0[op_idx][s] = NO_DEP;
    }
  }

  /* Search through possible memory successors.
   */
  for (succ_idx = first_poss_succ_idx; succ_idx < num_mem_ops; succ_idx++) {
    BOOL definite, have_latency = FALSE;
    UINT8 omega = 0;
    OP *succ = mem_ops[succ_idx];
    ARC *arc;
    INT16 latency;
    CG_DEP_KIND kind = OP_Load(op) ?
      (OP_Load(succ) ? CG_DEP_MEMREAD : CG_DEP_MEMANTI) :
      (OP_Load(succ) ? CG_DEP_MEMIN : CG_DEP_MEMOUT);

    if (OP_volatile(succ) && OP_volatile(op)) kind = CG_DEP_MEMVOL;

    if (kind == CG_DEP_MEMREAD && !include_memread_arcs) 
      continue;

    if (!cyclic && CG_DEP_Mem_Arc_Pruning >= PRUNE_NON_CYCLIC ||
	cyclic && omega == 0 && CG_DEP_Mem_Arc_Pruning >= PRUNE_CYCLIC_0) {
      if (kind == CG_DEP_MEMREAD) {
	/* No need to look for MEMREAD succs if we've found a definite one */
	if (found_definite_memread_succ) continue;
	/* MEMREAD arcs aren't for scheduling - have no latency */
	latency = 0;
      } else {
	latency = CG_DEP_Latency(op, succ, kind, 0);
      }
      have_latency = TRUE;
      /* If a more restrictive arc exists, no need to check for this one */
      if (latency <= mem_op_lat_0[op_idx][succ_idx-op_idx-1]) continue;
    }

    if (get_mem_dep(op, succ, &definite, cyclic ? &omega : NULL)) {
#ifdef TARG_SL
    // insert nop when c3 mem op points to same location of normal mem op in same BB
    if (!cyclic && (Is_Target_Sl1_pcore() || Is_Target_Sl1_dsp()) 
      && (OP_bb(op) == OP_bb(succ))) {
      CGTARG_Mem_AR_Dep(op, succ, kind);
    }
#endif
      // For OOO machine (eg. T5), non-definite memory dependences can be 
      // relaxed to edges with zero latency. The belief is that this can 
      // help avoid creating false dependences with biased critical info. 
    
      if (!have_latency) latency =
        (CG_DEP_Adjust_OOO_Latency && PROC_is_out_of_order() && !definite) ? 
	0 : CG_DEP_Latency(op, succ, kind, 0);
#ifdef TARG_IA64      
      if (omega == 0) Cache_Adjust_Latency(op,succ,kind,&latency);
#endif
      /* Build a mem dep arc from <op> to <succ> */
      arc = new_arc_with_latency(kind, op, succ, latency, omega, 0, definite);

#ifdef TARG_IA64
      /* Set the dependence is violable on some circumstance */
      if (kind == CG_DEP_MEMIN && !definite &&
          !CGTARG_Is_OP_Check_Load(succ) &&
          !OP_volatile (op) && !OP_volatile(succ) &&
          !OP_asm(op) && !OP_asm(succ)) {
        Set_ARC_is_dotted (arc, TRUE);
      }
#else
      /* if MEMIN dependence is not a definite dependence and
        !include_memin_arcs is SET, not already a check-load, then
        set the ARC as a dotted edge. */
      if (!CGTARG_Is_OP_Check_Load(succ) &&
         kind == CG_DEP_MEMIN && !definite && !include_memin_arcs)
       Set_ARC_is_dotted(arc, TRUE);
#endif
      found_definite_memread_succ |= (kind == CG_DEP_MEMREAD && definite);

      /* Find latency to 0-omega store descendents of <succ>, then use this
       * to determine latencies from <op> to the 0-omega memory descendents
       * of <succ>.
       */
      if (omega == 0 && mem_op_lat_0 && kind != CG_DEP_MEMREAD) {

	/* Find mem dep successors of <succ>.  Recursion is guaranteed
	 * to terminate because <succ_idx> > <op_idx> (since 0-omega arcs
	 * go lexically forward), so <op_idx> in each call is strictly
	 * increasing, and add_mem_arcs_from(num_mem_ops-1) cannot build
	 * any 0-omega arcs.
	 */
	Is_True(succ_idx > op_idx, ("0-omega arc not lexically forward"));
	add_mem_arcs_from(succ_idx);
      
	/* Update <mem_op_lat_0> such that mem_op_lat_0[op][desc]
	 * contains the latency of the longest 0-omega path between
	 * <op> and store descendent <desc>, or NO_DEP if there is no
	 * 0-omega path.
	 */

	/* Fill in the latency for the new direct path <op> -> <succ>. */
	mem_op_lat_0[op_idx][succ_idx-op_idx-1] = latency;

	/* Update 0-omega path latencies from <op> to memory descendents
	 * of <succ>.
	 */
	for (s = succ_idx+1; s < num_mem_ops; s++) {
	  if (mem_op_lat_0[succ_idx] &&
	      mem_op_lat_0[succ_idx][s-succ_idx-1] != NO_DEP) {
	    INT16 new_latency = latency + mem_op_lat_0[succ_idx][s-succ_idx-1];
	    INT16 old_latency = mem_op_lat_0[op_idx][s-op_idx-1];
	    /* MAX works because NO_DEP < all possible latencies */
	    mem_op_lat_0[op_idx][s-op_idx-1] = MAX(old_latency, new_latency);
	  }
	}
      }
	
      /* Collect info needed by adjust_for_rw_elim */
      num_definite_arcs += definite;
      shortest = shortest ? shorter_succ_arc(shortest, arc) : arc;
      if (OP_like_store(succ))
	shortest_to_store = shortest_to_store ?
	  shorter_succ_arc(shortest_to_store, arc) : arc;
    }
  }

  if (num_definite_arcs)
    adjust_for_rw_elim(OP_succs(op), num_definite_arcs, shortest,
		       shortest_to_store);
}

#ifdef KEY
// OP has a homeable TN.  Add memory arcs between OP and memory OPs accessing
// the home location.  PREV_MEM_OPS_SEEN is the number of memory OPs preceeding
// OP in the BB.
void
add_home_mem_arcs_for_op (OP *op, TN *home_tn, int prev_mem_ops_seen)
{
  int i;

  for (i = 0; i < num_mem_ops; i++) {
    OP *mem_op = mem_ops[i];
    if (mem_op == op)
      continue;
    WN* wn = Get_WN_From_Memory_OP(mem_op);
    if (wn != NULL &&
	Aliased(Alias_Manager, TN_home(home_tn), wn) == SAME_LOCATION) {
      if (i < prev_mem_ops_seen) {
	// MEM_OP preceeds OP.
	new_arc(CG_DEP_MISC, mem_op, op, 0, 0, FALSE);
      } else {
	// OP preceeds MEM_OP.
	new_arc(CG_DEP_MISC, op, mem_op, 0, 0, FALSE);
      }
    }
  }
}

void
add_home_mem_arcs (BB *bb)
{
  OP *op;
  int i, mem_ops_seen;

  mem_ops_seen = 0;
  FOR_ALL_BB_OPs(bb, op) {
    if (OP_Load(op) || OP_like_store(op))
      mem_ops_seen++;
    // Analyze the operands and results' home locations.
    for (i = 0; i < OP_opnds(op); i++) {
      TN *opnd = OP_opnd(op, i);
      if (TN_is_register(opnd) &&
	  TN_is_gra_homeable(opnd)) {
	add_home_mem_arcs_for_op(op, opnd, mem_ops_seen);
      }
    }
    for (i = 0; i < OP_results(op); i++) {
      TN *result = OP_result(op, i);
      if (TN_is_register(result) &&
	  TN_is_gra_homeable(result)) {
	add_home_mem_arcs_for_op(op, result, mem_ops_seen);
      }
    }
  }
}
#endif

inline BOOL op_defines_sp(OP *op)
{
  INT i;
  for (i = 0; i < OP_results(op); ++i) {
    TN *result = OP_result(op,i);
    if (TN_register_and_class(result) == CLASS_AND_REG_sp) return TRUE;
  }
  return FALSE;
}

typedef enum {
  STACKREF_NO,
  STACKREF_YES,
  STACKREF_MAYBE
} STACKREF_KIND;

//
// Does <op> reference the stack?
//
static STACKREF_KIND Memory_OP_References_Stack(OP *op)
{

  /* Non-memory references are by definition do not reference the stack.
   */
  if (!OP_memory(op)) return STACKREF_NO;

  /* For loads of TNs which can be rematerialized, check the home location.
   */
  if (   OP_load(op)
      && OP_results(op) == 1 
      && TN_is_rematerializable(OP_result(op,0)))
  {
    TN *result = OP_result(op,0);
    WN *home = TN_home(result);
#ifdef TARG_MIPS
    /* To use a 64-bit constant in the program, we place the data 
       in .rodata section, and then load it back.
       The loads of 64-bit constants do not have a symbol
       so we don't want to call WN_st, and we know they don't access
       the stack, */
    if (WN_operator(home) == OPR_INTCONST)
      return STACKREF_NO;
#endif

    ST *st = WN_st(home);
    return (ST_sclass(st) == SCLASS_AUTO) ? STACKREF_YES : STACKREF_NO;
  }

  /* If we can get the whirl node the memop was generated from then 
   * perhaps we can make use of it.
   */
  WN *wn = Get_WN_From_Memory_OP(op);
  if (wn) {

    /* Attempt to use the wopt alias manager to determine if the
     * memop can point to the stack, i.e. a local variable.
     */
    if (Alias_Manager && Valid_alias(Alias_Manager, wn)) {
      BOOL on_stack = (Points_to(Alias_Manager, wn)->Local() != 0);
      return on_stack ? STACKREF_YES : STACKREF_NO;
    }

    /* Try to make a determination based on the whirl itself.
     * We only handle the simple (and common) cases of a direct
     * load/store and an indirect load/store with an LDA for an address.
     * For the above cases, get the symbol for the variable being accessed.
     */
    /*
     for LDA case, there're still a case that the ST is CLASS_BLOCK,which maybe
     bss section symbol generated by data layout,for example.
    */
    ST *st = NULL;
    if (WN_has_sym(wn)) {
      st = WN_st(wn);
#ifdef KEY
      Is_True(ST_class(st) == CLASS_VAR || ST_class(st) == CLASS_CONST ||
              ST_class(st) == CLASS_FUNC || ST_class(st) == CLASS_BLOCK, 
      	      ("expected CLASS_VAR/CONST/FUNC/BLOCK symbol"));
#else
      Is_True(ST_class(st) == CLASS_VAR, ("expected CLASS_VAR symbol"));
#endif
    } else {
      WN *lda = NULL;
      switch (WN_operator(wn)) {
      case OPR_ILOAD:
      case OPR_ILDBITS:
      case OPR_ILOADX:
	lda = WN_kid0(wn);
	break;
      case OPR_ISTORE:
      case OPR_ISTBITS:
      case OPR_ISTOREX:
	lda = WN_kid1(wn);
	break;
#ifdef KEY
      case OPR_CVT:
	return STACKREF_MAYBE;
      default:
	return STACKREF_MAYBE;
#endif
      }
      if (WN_operator_is(lda, OPR_LDA)) st = WN_st(lda);
    }

    /* If we found a symbol, then give a definitive answer based on
     * the storage class.
     */
    if (st) return (ST_sclass(st) == SCLASS_AUTO) ? STACKREF_YES : STACKREF_NO;
  }

  /* We didn't have enough info to make a definitive answer so we have
   * no choice but to assume it might reference the stack.
   */
  return STACKREF_MAYBE;
}

//
// Add an arc from <mem_op> to the <exit_sp_adj_op> under the given conditions.
//
void maybe_add_exit_sp_adj_arc (OP *mem_op, OP *exit_sp_adj_op) 
{
  if (   Memory_OP_References_Stack(mem_op) != STACKREF_NO
      && OP_Precedes(mem_op, exit_sp_adj_op))
  {
    new_arc_with_latency(CG_DEP_MISC, mem_op, exit_sp_adj_op, 0, 0, 0, FALSE);
  }
}

BOOL 
CG_DEP_Alloca_Aliases(OP *mem_op)
{
  WN * mem_wn = Get_WN_From_Memory_OP(mem_op);
  
  if (Alias_Manager == NULL)
    // Be very conservative if there's no alias mgr to query.
    return TRUE;

  if (mem_wn && May_refer_to_alloca_mem(Alias_Manager, mem_wn))
    return TRUE;

  return FALSE;
}

static void Add_MEM_Arcs(BB *bb)
/* -----------------------------------------------------------------------
 * Add memory arcs to the current dep graph.
 * ----------------------------------------------------------------------- */
{
  OP *op;
  UINT16 i;
  UINT32 sp_defs = 0;

  /* Count the memory OPs */
  num_mem_ops = 0;
  FOR_ALL_BB_OPs(bb, op) {
    if (OP_Load(op) || OP_like_store(op))
      num_mem_ops++;

    if (CG_DEP_Add_Alloca_Arcs && op_defines_sp(op))
      ++sp_defs;
  }

  /* Return if there's nothing to do */
  if (num_mem_ops < 1) return;


  /* For an exit block, add an arc from every stack memory op to the
   * SP adjustment op.
   */
  if (BB_exit(bb)) {
    OP *exit_sp_adj = BB_exit_sp_adj_op(bb);
#ifdef KEY
    /* <exit_sp_adj> could reside in a different bb, say _epilog_bb
       for bug#3241.
     */
    if (exit_sp_adj &&
	OP_bb(exit_sp_adj) == bb)
#endif // KEY
      {
	for (op = exit_sp_adj; op != NULL; op = OP_prev(op)) {
	  maybe_add_exit_sp_adj_arc (op, exit_sp_adj);
	}
      }
  }

#ifdef KEY
  // To fix the position of asm ops w.r.t. other operations, create 
  // dependency with all other ops. 
  // TODO: Need to find out if the better way is to create a new BB for 
  // every asm. 
  {
    FOR_ALL_BB_OPs(bb, op) {
      if (OP_code(op) == TOP_asm) {
	OP *op_tmp;
	BOOL tail = FALSE;

	FOR_ALL_BB_OPs(bb, op_tmp) {
	  if (op_tmp == op) {
	    tail = TRUE;
	    continue;
	  }
	  if (!tail) 
	    new_arc_with_latency(CG_DEP_MEMOUT, op_tmp, op, 1, 0, 0,FALSE);
	  else
	    new_arc_with_latency(CG_DEP_MEMOUT, op, op_tmp, 1, 0, 0,FALSE);
	}
      }
    }
  }
#endif
  if (!cyclic && num_mem_ops == 1) return;

  /* Initialize data structures used by add_mem_arcs_from */
  MEM_POOL_Push(&MEM_local_pool);
  mem_ops = TYPE_L_ALLOC_N(OP *, num_mem_ops);
  i = 0;
  FOR_ALL_BB_OPs(bb, op) {
    if (OP_Load(op) || OP_like_store(op)){
      mem_ops[i++] = op;
    }
  }
  if (CG_DEP_Mem_Arc_Pruning >= PRUNE_CYCLIC_0 ||
      !cyclic && CG_DEP_Mem_Arc_Pruning >= PRUNE_NON_CYCLIC)
    mem_op_lat_0 = TYPE_L_ALLOC_N(INT32 *, num_mem_ops);

  /* Call add_mem_arcs_from, which does the real work */
  for (i = 0; i < num_mem_ops; i++)
    add_mem_arcs_from(i);

  /* Workaround for PV 707179.  Also see code above dependent on
   * CG_DEP_Add_Alloca_Arcs.
   */
  if (CG_DEP_Add_Alloca_Arcs) {
    for (op = BB_first_op(bb); op && sp_defs > 0; op = OP_next(op)) {
      if (op_defines_sp(op)) {
	--sp_defs;
	for (i = 0; i < num_mem_ops; i++) {
#ifdef KEY
	  if (op == mem_ops[i])
	    continue;
#endif
	  if (CG_DEP_Alloca_Aliases(mem_ops[i])) {
	    if (OP_Precedes(op, mem_ops[i]))
	      new_arc(CG_DEP_MISC, op, mem_ops[i], 0, 0, FALSE);
	    else
	      new_arc(CG_DEP_MISC, mem_ops[i], op, 0, 0, FALSE);
	  }
	}
      }
    }
  }

#ifdef KEY
  // Add memory arcs between OPs whose TNs have home locations and the
  // load/stores of those home locations.  This is so that when GRA inserts
  // spill code around the OPs, the spill code will read/write memory in the
  // correct order relative to the load/stores.  Bug 7847.
  add_home_mem_arcs(bb);
#endif

  MEM_POOL_Pop(&MEM_local_pool);

  /* This acts as a flag, so be sure to reset it. */
  mem_op_lat_0 = NULL;

  /* Make add_mem_arcs_from/to crash immediately when <mem_ops> not right */
  mem_ops = NULL;
}

#if defined(TARG_SL)
static void Add_Vbuf_MEM_Arcs(BB *bb)
/* -----------------------------------------------------------------------
 * Add memory arcs to the current dep graph.
 * ----------------------------------------------------------------------- */
{
  OP *op;
  UINT16 i;
  UINT32 sp_defs = 0;

  /* Count the memory OPs */
  num_mem_ops = 0;
  FOR_ALL_BB_OPs(bb, op) {
    if (OP_vbuf_load(op) || OP_vbuf_store(op))
      num_mem_ops++;
  }

  /* Return if there's nothing to do */
  if (num_mem_ops < 1) return;


  /* For an exit block, add an arc from every stack memory op to the
   * SP adjustment op.
   */
  if (BB_exit(bb)) {
    OP *exit_sp_adj = BB_exit_sp_adj_op(bb);
#ifdef KEY
    /* <exit_sp_adj> could reside in a different bb, say _epilog_bb
       for bug#3241.
     */
    if (exit_sp_adj &&
	OP_bb(exit_sp_adj) == bb)
#endif // KEY
      {
	for (op = exit_sp_adj; op != NULL; op = OP_prev(op)) {
	  maybe_add_exit_sp_adj_arc (op, exit_sp_adj);
	}
      }
  }

#ifdef KEY
  // To fix the position of asm ops w.r.t. other operations, create 
  // dependency with all other ops. 
  // TODO: Need to find out if the better way is to create a new BB for 
  // every asm. 
  {
    FOR_ALL_BB_OPs(bb, op) {
      if (OP_code(op) == TOP_asm) {
	OP *op_tmp;
	BOOL tail = FALSE;

	FOR_ALL_BB_OPs(bb, op_tmp) {
	  if (op_tmp == op) {
	    tail = TRUE;
	    continue;
	  }
	  if (!tail) 
	    new_arc_with_latency(CG_DEP_MEMOUT, op_tmp, op, 1, 0, 0,FALSE);
	  else
	    new_arc_with_latency(CG_DEP_MEMOUT, op, op_tmp, 1, 0, 0,FALSE);
	}
      }
    }
  }
#endif
  if (!cyclic && num_mem_ops == 1) return;

  /* Initialize data structures used by add_mem_arcs_from */
  MEM_POOL_Push(&MEM_local_pool);
  mem_ops = TYPE_L_ALLOC_N(OP *, num_mem_ops);
  i = 0;
  FOR_ALL_BB_OPs(bb, op) {
    if (OP_vbuf_load(op) || OP_vbuf_store(op)){
      mem_ops[i++] = op;
    }
  }
  if (CG_DEP_Mem_Arc_Pruning >= PRUNE_CYCLIC_0 ||
      !cyclic && CG_DEP_Mem_Arc_Pruning >= PRUNE_NON_CYCLIC)
    mem_op_lat_0 = TYPE_L_ALLOC_N(INT32 *, num_mem_ops);

  /* Call add_mem_arcs_from, which does the real work */
  for (i = 0; i < num_mem_ops; i++)
    add_mem_arcs_from(i);

  /* Workaround for PV 707179.  Also see code above dependent on
   * CG_DEP_Add_Alloca_Arcs.
   */
  if (CG_DEP_Add_Alloca_Arcs) {
    for (op = BB_first_op(bb); op && sp_defs > 0; op = OP_next(op)) {
      if (op_defines_sp(op)) {
	--sp_defs;
	for (i = 0; i < num_mem_ops; i++) {
	  if (CG_DEP_Alloca_Aliases(mem_ops[i])) {
	    if (OP_Precedes(op, mem_ops[i]))
	      new_arc(CG_DEP_MISC, op, mem_ops[i], 0, 0, FALSE);
	    else
	      new_arc(CG_DEP_MISC, mem_ops[i], op, 0, 0, FALSE);
	  }
	}
      }
    }
  }

  MEM_POOL_Pop(&MEM_local_pool);

  /* This acts as a flag, so be sure to reset it. */
  mem_op_lat_0 = NULL;

  /* Make add_mem_arcs_from/to crash immediately when <mem_ops> not right */
  mem_ops = NULL;
}
#endif 

static void make_virtual_anti_or_output_arc(CG_DEP_KIND kind, OP *pred, 
					    OP *succ, UINT8 opnd)
/* -----------------------------------------------------------------------
 * Requires: kind == CG_DEP_REGOUT || kind == CG_DEP_REGANTI
 * Create a <kind> arc (for <pred> operand <opnd> if <kind> is CG_DEP_REGANTI)
 * from <pred> to <succ>, but prune OUT/ANTI arcs so that each OP has at
 * most one REGOUT successor, and at most one REGANTI successor for a given
 * operand, going to the closest definition.
 * -----------------------------------------------------------------------
 */
{
  INT16 search_opnd = (kind == CG_DEP_REGOUT) ? DONT_CARE : (INT16)opnd;
  ARC *arc = _CG_DEP_op_info(pred) ?
    ARC_LIST_Find_First(OP_succs(pred), kind, search_opnd) : NULL;

  Is_True(kind == CG_DEP_REGOUT || kind == CG_DEP_REGANTI,
	    ("kind not REGOUT or REGANTI"));
  Is_True(cyclic || OP_Precedes(pred, succ),
	    ("cannot make non-cyclic backwards arc"));

  if (arc) {
    OP *other_succ = ARC_succ(arc);

    /* Found existing arc to <other_succ>.  If <succ> is closer to
     * <pred> than <other_succ>, point the arc to <succ>.  There
     * should be a chain of REGOUT arcs connecting the closer of
     * <succ> and <other_succ> to the more distant one, so no other
     * arcs are necessary.
     */

    if (is_closer_succ(pred, succ, other_succ)) {
      detach_arc_from_succ(arc);
      Set_ARC_succ(arc, succ);
      Set_ARC_kind(arc, kind);
      Set_ARC_omega(arc, !OP_Follows(succ, pred));
      attach_arc_to_succ(arc);
      if (maintain_prebr) maintain_prebr_arc(pred);
    }

  } else {

    /* Make sure <succ> is the closest def to <pred>.  We can do this
     * quickly because all the defs are connected with REGOUT arcs.
     */
    if (_CG_DEP_op_info(succ)) {
      do {
	arc = ARC_LIST_Find_First(OP_preds(succ), CG_DEP_REGOUT, DONT_CARE);
	Is_True(!arc || ARC_pred(arc) != succ, ("found REGOUT arc to self"));
	if (arc)
	  succ = closer_succ(pred, succ, ARC_pred(arc));
      } while (arc && ARC_pred(arc) == succ);
    }

    /* Build new arc to <succ>.
     */
    new_arc(kind, pred, succ, !OP_Follows(succ, pred), opnd, FALSE);
  }
}

// -----------------------------------------------------------------------
// OP_Shadowed_By_Prev_OPs
// Placeholder routine to exclude unnecessary dependencies.
// -----------------------------------------------------------------------
//
static BOOL
OP_Shadowed_By_Prev_OPs(OP                    *defop, 
			OP_LIST               *prev_list, 
			COMPARE_FUNCTION      comp_func)
{
  OP *op;
  while (prev_list) {
    op = OP_LIST_first(prev_list);
    if (comp_func(op, defop)) return TRUE;
    prev_list = OP_LIST_rest(prev_list);
  }
  return FALSE;
}

/* Get word size for c2.load/store instructions in multi-mode*/ 
#if defined(TARG_SL)
static 
INT Get_Word_Size_For_Multi_Mode(TOP opcode) 
{ 
   switch(opcode) 
   {
     case TOP_c2_ldi_v_m_b_u:
     case TOP_c2_ldi_v_m_b:
     case TOP_c2_ld_v_m_b:
     case TOP_c2_ld_v_m_b_u:
     case TOP_c2_sti_v_m_b:
     case TOP_c2_st_v_m_b:
      return 1;
     break;
     case TOP_c2_ldi_v_m_h:
     case TOP_c2_ld_v_m_h:
     case TOP_c2_sti_v_m_h:
     case TOP_c2_st_v_m_h:
      return 2;
     break;
     case TOP_c2_ldi_v_m_w:
     case TOP_c2_ld_v_m_w:
     case TOP_c2_sti_v_m_w:
     case TOP_c2_st_v_m_w:
      return 4;
     break;
     default:
      FmtAssert(FALSE, ("invalid opcode for multi-mode c2.ld/st"));
   }
}
#endif 

//
// -----------------------------------------------------------------------
// Add_Forw_REG_Arcs
// Compute register dependences that can be computed in a single forward
// pass. They include REGIN, REGOUT, Prefetch dependences.
// -----------------------------------------------------------------------
//
static void
Add_Forw_REG_Arcs(BB *bb)
{
  OP *op;
  
  FOR_ALL_BB_OPs(bb, op) {
    INT32 i;

    if (OP_store(op) || OP_load(op)) {
      /* Generate any prefetch arcs */
      make_prefetch_arcs(op, bb);
    }

    for (i = 0; i < OP_opnds(op); i++) {
      OP_LIST *defop_list = defop_for_op(op, i, FALSE);
      if (has_assigned_reg(OP_opnd(op,i)))
	add_reg_assignment(OP_opnd(op,i));

      OP_LIST *prev_list = NULL;
      while (defop_list) {
	/*
	 * Build REGIN arc from operand's def.
	 * Don't prune REGIN arcs!
	 */
	OP *defop = OP_LIST_first(defop_list);
	if (!OP_Shadowed_By_Prev_OPs(defop, prev_list, 
				     OP_has_subset_predicate)) {

	  // #795487: PQS doesn't work with register allocated code. It
	  // uses TN as a handle to query predicate relations and relies
	  // on single-reaching definitions. This can no longer be TRUE
	  // after register allocation, since a same register can be
	  // allocated to multiple TNs. This is a deficiency of the 
	  // current PQS implementation.

#ifdef TARG_IA64
	  if ((!PRDB_Valid() && include_assigned_registers) ||
#else
	  if (include_assigned_registers ||
#endif
	      !OP_has_disjoint_predicate(defop,op)) {
#ifdef TARG_SL2 	      
           if(TOP_is_c2_multi_mode_load(OP_code(defop))) {
             Is_True( defop->extra_result, 
                      (" multi mode defop should have already been created with extra opnds/results") );
             TN_LIST * more_tns = defop->extra_operand;
             INT32 row_count = 0;
             while( more_tns ){
               row_count++;
               more_tns = TN_LIST_rest( more_tns );
             } 
             Is_True( (row_count >= 0), 
                      ("Invalid row_count in multi_mode c2.ld/st"));
             INT word_size = Get_Word_Size_For_Multi_Mode(OP_code(defop)); 
             new_arc_with_latency(CG_DEP_REGIN, defop, op, row_count*word_size, 0, i,  FALSE);
           }
           else {
             new_arc(CG_DEP_REGIN, defop, op, 0, i, FALSE);
           }
#else 
	    new_arc(CG_DEP_REGIN, defop, op, 0, i, FALSE);
#endif
	  }
	}
	prev_list = OP_LIST_Push(defop, prev_list, &dep_nz_pool);
	defop_list = OP_LIST_rest(defop_list);
      }
    }

#if defined(TARG_SL)
    TN_LIST * extra_opnds = op->extra_operand;
    while( extra_opnds ){
      /* Strictly, I should get the defop using defop_by_reg,
       * But since each SL2 vector registers have one single TN,
       * and all the places use the same TN, so I use defop_by_tn
       */
      TN* opnd_tn = TN_LIST_first( extra_opnds );
      OP_LIST *defop_list = defop_for_tn( opnd_tn );
      if( has_assigned_reg(opnd_tn) )
	add_reg_assignment(opnd_tn);

      OP_LIST *prev_list = NULL;
      while (defop_list) {
	OP *defop = OP_LIST_first(defop_list);
	if (!OP_Shadowed_By_Prev_OPs(defop, prev_list, 
				     OP_has_subset_predicate)) {
	  if (include_assigned_registers ||
	      !OP_has_disjoint_predicate(defop,op)) {
            /* the position of operands in 'op' is treated as 0,
             * since all the extra operands begin with 0th opnd.
             */
	    if(TOP_is_c2_multi_mode_load(OP_code(defop))) {
	      Is_True( defop->extra_result, 
		       (" multi mode defop should have already been created with extra opnds/results") );
	      TN_LIST * more_tns = defop->extra_operand;
	      INT32 row_count = 0;
	      while( more_tns ){
		row_count++;
		more_tns = TN_LIST_rest( more_tns );
	      } 
	      Is_True( (row_count >= 0), 
		       ("Invalid row_count in multi_mode c2.ld/st") );
	      INT word_size = Get_Word_Size_For_Multi_Mode(OP_code(defop)); 
	      new_arc_with_latency(CG_DEP_REGIN, defop, op, row_count*word_size, 0, 0,  FALSE);
	    }
	    else {
	      new_arc(CG_DEP_REGIN, defop, op, 0, 0, FALSE);
	    }
	  }
	}
	prev_list = OP_LIST_Push(defop, prev_list, &dep_nz_pool);
	defop_list = OP_LIST_rest(defop_list);
      }
      extra_opnds = TN_LIST_rest( extra_opnds );
    }
#endif

    for (i = 0; i < OP_results(op); i++) {
      OP_LIST *prev_defop_list = defop_for_op(op, i, TRUE);
      if (has_assigned_reg(OP_result(op,i)))
	add_reg_assignment(OP_result(op,i));

      OP_LIST *prev_list = NULL;
      while (prev_defop_list) {
	/*
	 * Build REGOUT arc from previous def of same result.
	 *
	 * TODO (??): If <prev_defop> has a REGIN successor and the
	 * successor has a REGANTI arc to <op>, then this REGOUT
	 * isn't necessary for scheduling, so don't add it.
	 */
	OP *prev_defop = OP_LIST_first(prev_defop_list);

	if (!OP_Shadowed_By_Prev_OPs(prev_defop, prev_list, OP_has_subset_predicate)) {

	  // #795487: PQS doesn't work with register allocated code. It
	  // uses TN as a handle to query predicate relations and relies
	  // on single-reaching definitions. This can no longer be TRUE
	  // after register allocation, since a same register can be
	  // allocated to multiple TNs. This is a deficiency of the 
	  // current PQS implementation.

#ifdef TARG_IA64
	  if ((!PRDB_Valid() && include_assigned_registers) ||
#else
	  if (include_assigned_registers ||
#endif
	      !OP_has_disjoint_predicate(prev_defop,op)) {
	    new_arc(CG_DEP_REGOUT, prev_defop, op, 0, 0, FALSE);
	  }
	}

	prev_list = OP_LIST_Push(prev_defop, prev_list, &dep_nz_pool);
	prev_defop_list = OP_LIST_rest(prev_defop_list);
      }
    }
    
#if defined(TARG_SL) 
    /* for multi mode sl2 load, it has extra results. */
    TN_LIST * extra_results = op->extra_result;
    while( extra_results ) {
      TN* tn = TN_LIST_first( extra_results );
      OP_LIST *prev_defop_list = defop_for_tn(tn);
      if( has_assigned_reg(tn) )
	add_reg_assignment(tn);

      OP_LIST *prev_list = NULL;
      while (prev_defop_list) {
	OP *prev_defop = OP_LIST_first(prev_defop_list);

	if (!OP_Shadowed_By_Prev_OPs(prev_defop, prev_list, OP_has_subset_predicate)) {

	  if (include_assigned_registers ||
	      !OP_has_disjoint_predicate(prev_defop,op)) {
	    new_arc(CG_DEP_REGOUT, prev_defop, op, 0, 0, FALSE);
	  }
	}

	prev_list = OP_LIST_Push(prev_defop, prev_list, &dep_nz_pool);
	prev_defop_list = OP_LIST_rest(prev_defop_list);
      }
      extra_results = TN_LIST_rest( extra_results );
    }
#endif
    defop_set(op);  // Push this op's definitions
  }
}

//
// -----------------------------------------------------------------------
// Add_Bkwd_REG_Arcs
// Compute register dependence arcs that can be determined in one single
// backward pass. They include Cyclic REGOUT/IN/ANTI and Non-cyclic 
// REGANTI dependences.
// -----------------------------------------------------------------------
//
static void
Add_Bkwd_REG_Arcs(BB *bb, TN_SET *need_anti_out_dep)
{
  OP *op;
  FOR_ALL_BB_OPs_REV(bb, op) {
    INT32 i;
    defop_set(op);			/* Remember this def */

    for (i = 0; i < OP_opnds(op); i++) {
      TN *opnd = OP_opnd(op,i);
      if (TN_is_register(opnd) && !TN_is_const_reg(opnd)) {
	BOOL tn_def_found = FALSE;
	OP_LIST *defop_list = defop_for_op(op, i, FALSE);
	OP_LIST *prev_list = NULL;
	while (defop_list) {
	  OP *defop = OP_LIST_first(defop_list);
	  if (defop != op &&
	      !OP_Shadowed_By_Prev_OPs(defop, prev_list, OP_has_subset_predicate)) {

	  // #795487: PQS doesn't work with register allocated code. It
	  // uses TN as a handle to query predicate relations and relies
	  // on single-reaching definitions. This can no longer be TRUE
	  // after register allocation, since a same register can be
	  // allocated to multiple TNs. This is a deficiency of the 
	  // current PQS implementation.

#ifdef TARG_IA64
	    if ((!PRDB_Valid() && include_assigned_registers) ||
	         !OP_has_disjoint_predicate(defop,op) ||
      	         TN_is_predicate(OP_opnd(op,i))) {  
#else
	      if (include_assigned_registers ||
		  !OP_has_disjoint_predicate(defop,op)) {
#endif
	      tn_def_found = TRUE;
	      /*
	       * Build non-cyclic REGANTI arc to next def
	       */
	      new_arc(CG_DEP_REGANTI, op, defop, 0, i, FALSE);
	    }
	  }
	  prev_list = OP_LIST_Push(defop, prev_list, &dep_nz_pool);
	  defop_list = OP_LIST_rest(defop_list);
	}
	if (!tn_def_found && !ARC_LIST_Find(OP_preds(op), CG_DEP_REGIN, i))
	  add_gtn_use_arc(op, i);
      }
    }
#if defined(TARG_SL)
    TN_LIST * extra_operands = op->extra_operand;
    while( extra_operands ) {
      TN *opnd = TN_LIST_first( extra_operands );
      if (TN_is_register(opnd) && !TN_is_const_reg(opnd)) {
	BOOL tn_def_found = FALSE;
	OP_LIST *defop_list = defop_for_tn(opnd);
	OP_LIST *prev_list = NULL;
	while (defop_list) {
	  OP *defop = OP_LIST_first(defop_list);
	  if (defop != op &&
	      !OP_Shadowed_By_Prev_OPs(defop, prev_list, OP_has_subset_predicate)) {

	    if (include_assigned_registers ||
	      !OP_has_disjoint_predicate(defop,op)) {
	      tn_def_found = TRUE;
	      new_arc(CG_DEP_REGANTI, op, defop, 0, 0, FALSE);
	    }
	  }
	  prev_list = OP_LIST_Push(defop, prev_list, &dep_nz_pool);
	  defop_list = OP_LIST_rest(defop_list);
	}
        /* change i->0, since we treat all the extra operands to 
         * the first operand
         */
	if(!tn_def_found && !ARC_LIST_Find(OP_preds(op), CG_DEP_REGIN, 0))
	  add_gtn_use_arc(op, 0);
      }
      extra_operands = TN_LIST_rest( extra_operands );
    }
#endif // TARG_SL
  }
}


// Construct a TN to TN_DU mapping.
//  - used by Build_Cyclic_Arcs
//
struct TN_2_DEFS_VECTOR_MAP {
  typedef std::vector<int> DEFS_VECTOR_TYPE;
  typedef std::map<TN*, DEFS_VECTOR_TYPE> TN_2_DEFS_VECTOR_MAP_TYPE;
  typedef TN_2_DEFS_VECTOR_MAP_TYPE::iterator iterator;

private:
  TN_2_DEFS_VECTOR_MAP_TYPE tn_2_defs_vector_map;

public:

  iterator begin() { return tn_2_defs_vector_map.begin(); }
  iterator end() { return tn_2_defs_vector_map.end(); }
  iterator find(TN *tn) { return tn_2_defs_vector_map.find(tn); }

  DEFS_VECTOR_TYPE& operator[](TN *tn) {
    return tn_2_defs_vector_map[tn];
  }

  TN_2_DEFS_VECTOR_MAP(OP_VECTOR& op_vec, bool trace) {
#ifdef TARG_X8664
    static TN* rflags = Rflags_TN();
#endif

    for (INT op_num = 0; op_num < op_vec.size(); op_num++) {
      OP *op = op_vec[op_num];
      for (INT i = 0; i < OP_results(op); i++) {
	TN *tn = OP_result(op,i);
	if (TN_is_register(tn) && 
#ifdef TARG_X8664
	    tn != rflags       &&
#endif
	    !TN_is_const_reg(tn)) {
	  if (tn_2_defs_vector_map.find(tn) == tn_2_defs_vector_map.end()) 
	    tn_2_defs_vector_map[tn] = DEFS_VECTOR_TYPE();
	  tn_2_defs_vector_map[tn].push_back(op_num);
	}
      }
    }
  }
};


//  Bulid the cyclic arcs for the CYCLIC DEP GRPAH
//
void Build_Cyclic_Arcs(BB *bb)
{
  OP_VECTOR op_vec(bb);
  TN_2_DEFS_VECTOR_MAP tn_map(op_vec, false);

  for (INT use_num = 0; use_num < op_vec.size(); use_num++) {
    OP *op = op_vec[use_num];
    for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
      TN *tn = OP_opnd(op, opnd);

      // If TN is a not register, or is a constant register
      // no dependence arcs are necessary!
      if (!TN_is_register(tn) || TN_is_const_reg(tn)) 
	continue;

      if (tn_map.find(tn) == tn_map.end())  // loop invariant
	continue;

      TN_2_DEFS_VECTOR_MAP::DEFS_VECTOR_TYPE& tn_defs = tn_map[tn];
      INT omega = OP_omega(op, opnd);

      // local live range
      bool local_lr = !TN_is_global_reg(tn) || !GTN_SET_MemberP(BB_live_in(bb), tn);
      bool definite_dep = !local_lr || !TN_is_dedicated(tn);
      bool single_def = tn_defs.size() == 1;

      INT i;
      for (i = 0; i < tn_defs.size(); i++) {
	INT def_num = tn_defs[i];
	OP *op = op_vec[def_num];
	if (Base_update_tn(op) == tn)
	  definite_dep = false;
      }

      Is_True(omega <= 1 || definite_dep,
	      ("Build_Cyclic_Arcs:  cannot have omega=%d for non-definite dependence.",
	       omega));

      // Build REGIN arcs
      if (single_def) {
	
	INT def_num = tn_defs[0];
	new_arc(CG_DEP_REGIN, op_vec[def_num], op, omega, opnd, FALSE);
	
      } else {                          // multiple definitions
	
	INT i;
	// found REGIN within same iteration
	for (i = tn_defs.size()-1; i >= 0; i--) {
	  INT def_num = tn_defs[i];
	  if (def_num >= use_num) continue;

	  if (!OP_has_disjoint_predicate_cyclic(op_vec[def_num], op))
	    new_arc(CG_DEP_REGIN, op_vec[def_num], op, omega, opnd, FALSE);
	  
	  if (OP_has_subset_predicate_cyclic(op_vec[def_num], op)) 
	    break;
	}

	// found REGIN across iterations
	if (!local_lr) {
	  if (!definite_dep ||
	      (definite_dep && omega > 0)) {
	    for (i = tn_defs.size()-1; i >= 0; i--) {
	      INT def_num = tn_defs[i];
	      if (def_num < use_num) break;
	      new_arc(CG_DEP_REGIN, op_vec[def_num], op, definite_dep ? omega : 1, opnd, FALSE);
	    }
	  }
	}
      }

      // Build REGANTI arcs
      if (single_def) {

	if (!definite_dep) {
	  INT def_num = tn_defs[0];
	  INT omega = (def_num <= use_num) ? 1 : 0;
	  if (def_num != use_num) // skip self ANTI
	    new_arc(CG_DEP_REGANTI, op, op_vec[def_num], omega, opnd, FALSE);
	}

      } else {
	
	// found REGANTI within same iteration
	INT i;
	for (i = 0; i < tn_defs.size(); i++) {
	  INT def_num = tn_defs[i];
	  if (def_num <= use_num) continue;  // skip self ANTI

	  if (!OP_has_disjoint_predicate_cyclic(op_vec[def_num], op))
	    new_arc(CG_DEP_REGANTI, op, op_vec[def_num], 0, opnd, FALSE);
	  
	  if (OP_has_subset_predicate_cyclic(op_vec[def_num], op))
	    break;
	}

	// found REGANTI across iterations
	if (!definite_dep) {
	  for (i = 0; i < tn_defs.size(); i++) {
	    INT def_num = tn_defs[i];
	    if (def_num >= use_num) break;
	    
	    // do not use PQS across iterations
	    new_arc(CG_DEP_REGANTI, op, op_vec[def_num], 1, opnd, FALSE);
	  }
	}
      }
    }
  }

  for (TN_2_DEFS_VECTOR_MAP::iterator it = tn_map.begin(); it != tn_map.end(); it++) {
    TN *tn = (*it).first;
    TN_2_DEFS_VECTOR_MAP::DEFS_VECTOR_TYPE& tn_defs = (*it).second;

    // local live range
    bool local_lr = !TN_is_global_reg(tn) || !GTN_SET_MemberP(BB_live_in(bb), tn);
    bool definite_dep = !local_lr && !TN_is_dedicated(tn);
    bool single_def = tn_defs.size() == 1;

    INT i;
    for (i = 0; i < tn_defs.size(); i++) {
      INT def_num = tn_defs[i];
      OP *op = op_vec[def_num];
      if (Base_update_tn(op) == tn)
	definite_dep = false;
    }

    // Build REGOUT arcs
    if (!single_def) {
      INT last_i = tn_defs.size()-2;
      for (INT i = 0; i <= last_i; i++) {
	INT d1 = tn_defs[i];
	INT d2 = tn_defs[i+1];
	OP *op1 = op_vec[d1];
	OP *op2 = op_vec[d2];

	// Ignore PQS for the first and last OP
	if (i == 0 ||  // first op
	    i == last_i ||  // last op
	    !OP_has_disjoint_predicate_cyclic(op1, op2))
	  new_arc(CG_DEP_REGOUT, op1, op2, 0, 0, FALSE);
      }

      // Ignore PQS for the cross iteration REGOUT arc
      if (!definite_dep) {
	INT d1 = tn_defs[tn_defs.size()-1];
	INT d2 = tn_defs[0];
	new_arc(CG_DEP_REGOUT, op_vec[d1], op_vec[d2], 1, 0, FALSE);
      }
    }
  }

#ifdef Is_True_On
  if (Get_Trace(TP_CG, 0x02))
  {
    for (INT i = 0; i < op_vec.size(); i++) {
      OP *op = op_vec[i];
      for (ARC_LIST *arcs = OP_succs(op); arcs; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	if (ARC_omega(arc) == 0) {
	  OP *succ = ARC_succ(arc);
	  if (!OP_Precedes(op, succ)) {
	    CG_DEP_Trace_Graph(bb);
	    fprintf(TFile, "OP:\t");
	    Print_OP_No_SrcLine(op);
	    fprintf(TFile, "Succ:\t");
	    Print_OP_No_SrcLine(succ);
	    Is_True(FALSE, 
		    ("Build_Cyclic_Graphs: omega == 0 but pred OP does not precedes succ OP."));
	  }
	}
      }
    }
  }
#endif
}


#ifdef TARG_IA64
//=============================================================================
//
//Function:  Add_CHK_Arcs
//Input:
//    - bb 
//Output:
//    - No explicit output.
//Description:
//    - Since chk is inserted into basic block, we should build corresponding
//      arcs during DAG construction.
//=============================================================================
void 
Add_CHK_Arcs(BB *bb)
{
    BOOL is_recovery  = BB_recovery(bb);

    if(BB_length(bb) < 2) 
      return;
    
    OP* barrier = NULL;
    if(is_recovery) {
      barrier = BB_first_op(bb);
    }
    
    for(OP* op = BB_first_op(bb); op; op = OP_next(op)){
        if(barrier == op) continue;
        if(barrier){
            if(OP_chk(barrier)){
                new_arc_with_latency (CG_DEP_POSTCHK , barrier, op, 0, 0, 0, FALSE);
            } else if(OP_xfer(barrier) || OP_call(barrier)){
                new_arc_with_latency(CG_DEP_POSTBR, barrier, op, 0, 0, 0, FALSE);
            }else{
                new_arc_with_latency(CG_DEP_MISC, barrier, op, 0, 0, 0, FALSE);
            }
        }
        if(OP_chk(op) || OP_xfer(op) || OP_call(op))
            barrier = op;
    }
    
    barrier = NULL;
    for(OP* op = BB_last_op(bb); op; op = OP_prev(op)){
        if(barrier && !OP_xfer(op) && !OP_call(op) && !OP_chk(op)){
            if(OP_chk(barrier)){
                if(OP_store(op) && OP_chk_a(barrier)){
                    new_arc_with_latency(CG_DEP_PRECHK, op, barrier, 1, 0, 0, FALSE);
                }else{
                    new_arc_with_latency(CG_DEP_PRECHK, op, barrier, 0, 0, 0, FALSE);
                }
            }else{
                new_arc_with_latency(CG_DEP_PREBR, op, barrier, 0, 0, 0, FALSE);
            }
        }        
        if(OP_chk(op) || OP_xfer(op) || OP_call(op))
            barrier = op;
    }
}
#endif

/* -----------------------------------------------------------------------
 * Compute the whole dependence graph for <bb> anew.
 *
 * See add_mem_arcs for description of pruning of memory arcs (if enabled).
 *
 * Note that certain other non-memory arcs (REGANTI/REGOUT) not
 * necessary for scheduling (because the constraints are transitive)
 * are also pruned.
 *
 * REGIN arcs are not pruned.  They are used to maintain an OP DAG
 * traversed by various CGPREP algorithms.
 * -----------------------------------------------------------------------
 */
static void 
Compute_BB_Graph(BB *bb, TN_SET *need_anti_out_dep)
{
  OP *op;
  BB_OP_MAP omap = BB_OP_MAP_Create(bb, &dep_map_nz_pool);

  BB_MAP_Set(_cg_dep_op_info, bb, omap);
  OP *xfer_op = BB_xfer_op(bb);
  xfer_ops = TYPE_L_ALLOC_N(OP *, 1); // Assumes 1 branch OP per BB.


  INT i = 0;
  // Instantiate empty OP_preds/succs.
  FOR_ALL_BB_OPs(bb, op) {
    BB_OP_MAP_Set(omap, op, new_op_info());
  }

  if (xfer_op) { xfer_ops[i++] = xfer_op; }
  else { xfer_ops[i++] = NULL; }

  // Build arcs that can be built in one forward pass:
  //   REGIN arcs (except those to exposed uses)
  //   REGOUT arcs
  //   Prefetch arcs

  if (cyclic) {

    Build_Cyclic_Arcs(bb);

  } else {

    defop_init();
    Add_Forw_REG_Arcs(bb);
    defop_finish();

    defop_init();
    Add_Bkwd_REG_Arcs(bb, need_anti_out_dep);
    defop_finish();
  }
  
  // Build memory arcs
  Add_MEM_Arcs(bb);

  std::list<BB*> bb_list;
  bb_list.push_back(bb);
  // Build control arcs
  if (include_control_arcs) {
    Add_BRANCH_Arcs(bb, bb_list, TRUE);
  }

  // Build target-dependent (if any) MISC arcs .
  Add_MISC_Arcs(bb);
#ifdef TARG_IA64
  // Build pre-chk and post-chk  arcs.
  Add_CHK_Arcs(bb);
#endif
}


//  Manage the CYCLIC_DEP_GRAPH data structure
//
CYCLIC_DEP_GRAPH::CYCLIC_DEP_GRAPH( BB *body, MEM_POOL *pool )
  : _body( body ) 
{
  CG_DEP_Compute_Graph( _body, 
			NO_ASSIGNED_REG_DEPS, 
			CYCLIC,
			NO_MEMREAD_ARCS,
			INCLUDE_MEMIN_ARCS,
			NO_CONTROL_ARCS, 
			NULL);
}

CYCLIC_DEP_GRAPH::~CYCLIC_DEP_GRAPH()
{
  CG_DEP_Delete_Graph( _body );
}

/* -----------------------------------------------------------------------
 * Computes the dependence graph for a region containing a list of <BBs>.
 *
 * Assumes that list of <BBs> forms a single entry multiple exit region.
 *
 * In addition to REG* and MEM* arcs, PREBR and POSBR dependence arcs
 * are also inserted to preserve dependences across branch instructions.
 * -----------------------------------------------------------------------
 */
static void
Compute_Region_Graph(std::list<BB*> bb_list)
{

  std::list<BB*>::iterator bb_iter;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bb_list, bb_iter) {
    BB_OP_MAP omap = BB_OP_MAP_Create(*bb_iter, &dep_map_nz_pool);
    BB_MAP_Set(_cg_dep_op_info, *bb_iter, omap);

    OP *op;
    FOR_ALL_BB_OPs(*bb_iter, op) {
      BB_OP_MAP_Set(omap, op, new_op_info());
    }
  }

  defop_init();
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bb_list, bb_iter) {

    // Build arcs that can be built in one forward pass:
    //   REGIN arcs (except those to exposed uses)
    //   REGOUT arcs
    //   Prefetch arcs
    
    Add_Forw_REG_Arcs(*bb_iter);
  }
  defop_finish();

  defop_init();
  std::list<BB*>::reverse_iterator bb_riter;
  FOR_ALL_BB_STLLIST_ITEMS_BKWD(bb_list, bb_riter) {

    // Build other arcs in a backwards pass:
    // Non-cyclic REGANTI arcs
    
    Add_Bkwd_REG_Arcs(*bb_riter, NULL);
  }
  defop_finish();

  FOR_ALL_BB_STLLIST_ITEMS_FWD(bb_list, bb_iter) {
    // Build MEM arcs
    Add_MEM_Arcs(*bb_iter);

    // Build target-dependent (if any) MISC arcs .
    Add_MISC_Arcs(*bb_iter);
  }

  // If <include_control_arcs>, generate all PREBR and POSTBR dependence arcs.
  if (include_control_arcs) {
    // Accumulate all the <xfer_ops> in the extended block.
    xfer_ops = TYPE_L_ALLOC_N(OP *, bb_list.size());
    INT i = 0;
    FOR_ALL_BB_STLLIST_ITEMS_FWD(bb_list, bb_iter) {
      // Assumes that each BB has only one <xfer_op>.
      OP *xfer_op;
      
      // if <xfer_op> is present, set the corresponding block index <i> to
      // <xfer_op>, else set it to NULL.

      if (xfer_op = BB_xfer_op(*bb_iter)) {
	xfer_ops[i++] = xfer_op;
      } else {
	xfer_ops[i++] = NULL;
      }
    }

    FOR_ALL_BB_STLLIST_ITEMS_FWD(bb_list, bb_iter) {
      // Build BRANCH (PREBR and POSTBR) branch arcs
      Add_BRANCH_Arcs(*bb_iter, bb_list, TRUE);
    }
  }
}



/* =====================================================================
 *			     PREBR Arcs
 * =====================================================================
 */

static BOOL 
has_no_0_omega_non_neg_latency_succ(const void *op, const void *br_op)
/* -----------------------------------------------------------------------
 * TRUE iff <op> has no 0-omega non-negative latency successors other
 * than <br_op>.
 * -----------------------------------------------------------------------
 */
{
  ARC_LIST *arcs;

  for (arcs = OP_succs((OP*) op); arcs; arcs = ARC_LIST_rest(arcs)) {
    ARC *arc = ARC_LIST_first(arcs);
    if (ARC_succ(arc) != (OP*) br_op && 
	ARC_omega(arc) == 0 && ARC_latency(arc) >= 0)
      return FALSE;
  }
  return TRUE;
}

static void maintain_prebr_arc(OP *op)
/* -----------------------------------------------------------------------
 * Either add or remove PREBR arc from <op> to the loopback branch
 * if necessary.
 * -----------------------------------------------------------------------
 */
{
  OP *br_op = BB_branch_op(OP_bb(op));
  if (br_op != op) {
    BOOL needs_prebr_arc = 
      has_no_0_omega_non_neg_latency_succ((void*) op, (void *) br_op);
    ARC_LIST *arcs = ARC_LIST_Find(OP_succs(op), CG_DEP_PREBR, DONT_CARE);
    if (needs_prebr_arc && arcs == NULL) {
      new_arc(CG_DEP_PREBR, op, br_op, 0, 0, FALSE);
    } else if (!needs_prebr_arc && arcs) {
      ARC *prebr = ARC_LIST_first(arcs);
      detach_arc(prebr);
      delete_arc(prebr);
    }
  }
}

/* =====================================================================
 *			   Same-Res OP Arcs
 * =====================================================================
 */

void 
CG_DEP_Add_Op_Same_Res_Arcs(OP *op)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  BB *bb = OP_bb(op);
  INT16 which = CGPREP_Same_Res_Opnd(op);
  TN *opnd = OP_opnd(op, which);
  ARC_LIST *arcs = ARC_LIST_Find(OP_preds(op), CG_DEP_REGIN, which);
  
  Is_True(CG_DEP_Has_Graph(bb),
	    ("no current CG dep graph for BB:%d", BB_id(bb)));

  FmtAssert(which >= 0, ("neither operand of select can be default"));

  if (arcs) {
    ARC *def_arc = ARC_LIST_first(arcs);
    OP *def_op = ARC_pred(def_arc);
    Is_True(!ARC_LIST_Find(ARC_LIST_rest(arcs), CG_DEP_REGIN, which),
	      ("in BB:%d multiple REGIN arcs to opnd %d of OP%d",
	       BB_id(OP_bb(op)), which, OP_map_idx(op)));
    arcs = ARC_LIST_Find(OP_succs(def_op), CG_DEP_REGIN, DONT_CARE);

    if (op != def_op) {
      /* Add REGOUT arcs between <op> and <def_op> */
      make_virtual_anti_or_output_arc(CG_DEP_REGOUT, op, def_op, 0);
      make_virtual_anti_or_output_arc(CG_DEP_REGOUT, def_op, op, 0);
    }

  } else {
    arcs = CG_DEP_GTN_Use_Arcs(opnd);
  }

  while (arcs) {
    ARC *arc = ARC_LIST_first(arcs);
    OP *use_op = ARC_succ(arc);
    UINT8 which = ARC_opnd(arc);
    arcs = ARC_LIST_Find(ARC_LIST_rest(arcs), CG_DEP_REGIN, DONT_CARE);
    if (use_op != op && (cyclic || OP_Precedes(use_op, op)))
      make_virtual_anti_or_output_arc(CG_DEP_REGANTI, use_op, op, which);
  }

  if (tracing) {
    fprintf(TFile, "\n<arc> CG %sdependence graph for BB:%d updated "
	    "with same-res arcs for:\n", cyclic ? "cyclic " : "",
	    BB_id(bb));
    CG_DEP_Trace_Op_Arcs(op);
  }
}

BOOL 
CG_DEP_Add_Same_Res_Arcs()
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * Currently works only for single-BB dep graph.
 * -----------------------------------------------------------------------
 */
{
  Is_True(_cg_dep_bb == NULL,
	  ("Need to Invoke CG_DEP_Compute_Graph first"));

  OP *op;
  BOOL any = FALSE;
  FOR_ALL_BB_OPs(_cg_dep_bb, op) {
    if (OP_same_res(op)) {
      any = TRUE;
      CG_DEP_Add_Op_Same_Res_Arcs(op);
    }
  }
  return any;
}

void remove_unnecessary_anti_or_output_arcs(ARC_LIST *arcs)
/* -----------------------------------------------------------------------
 * Remove any REGANTI or REGOUT arcs from <arcs> that are unnecessary
 * because the relevant TNs don't have the same register assignment.
 * (These were presumably created by CG_DEP_Add_Same_Res_Arcs.)
 * -----------------------------------------------------------------------
 */
{
  while (arcs) {
    ARC *arc = ARC_LIST_first(arcs);
    CG_DEP_KIND kind = ARC_kind(arc);
    arcs = ARC_LIST_rest(arcs);
    if (kind == CG_DEP_REGANTI || kind == CG_DEP_REGOUT) {
      TN *succ_tn = OP_result(ARC_succ(arc),0 /*???*/);
      OP *pred = ARC_pred(arc);
      TN *pred_tn = (kind == CG_DEP_REGOUT) ?
	OP_result(pred,0 /*???*/) : OP_opnd(pred, ARC_opnd(arc));
      if (succ_tn != pred_tn &&
	  (!has_assigned_reg(succ_tn) || !has_assigned_reg(pred_tn) ||
	   TN_register(succ_tn) != TN_register(pred_tn) ||
	   TN_register_class(succ_tn) != TN_register_class(pred_tn))) {
	detach_arc(arc);
	delete_arc(arc);
      }
    }
  }
}

void 
CG_DEP_Remove_Op_Same_Res_Arcs(OP *op)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  Is_True(OP_same_res(op), ("<op> not a same-res OP"));
  remove_unnecessary_anti_or_output_arcs(OP_preds(op));
  remove_unnecessary_anti_or_output_arcs(OP_succs(op));
  if (tracing) {
    fprintf(TFile, "\n<arc> CG %sdependence graph for BB:%d updated "
	    "by removing same-res arcs for:\n", cyclic ? "cyclic " : "",
	    BB_id(OP_bb(op)));
    CG_DEP_Trace_Op_Arcs(op);
  }
}

void 
CG_DEP_Remove_Same_Res_Arcs()
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * Currently works only for single-BB dep graph.
 * -----------------------------------------------------------------------
 */
{
  Is_True(_cg_dep_bb == NULL,
	  ("Need to Invoke CG_DEP_Compute_Graph first"));

  OP *op;
  FOR_ALL_BB_OPs(_cg_dep_bb, op) {
    if (OP_same_res(op)) CG_DEP_Remove_Op_Same_Res_Arcs(op);
  }
}

/* =====================================================================
 *			       SCC Arcs
 * =====================================================================
 */

void 
CG_DEP_Set_SCC_ARC_omega(ARC *arc, UINT8 omega)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  Set_ARC_omega(arc, omega);
}

void 
CG_DEP_Add_SCC_Arc(OP *pred, OP *succ, UINT8 omega, INT16 latency,
		   ARC_LIST **scc_ancestor_list,
		   ARC_LIST **scc_descendent_list)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  ARC *arc = create_arc();

  Set_ARC_kind(arc, CG_DEP_SCC);
  Set_ARC_pred(arc, pred);
  Set_ARC_succ(arc, succ);
  Set_ARC_omega(arc, omega);
  Set_ARC_latency(arc, latency);
  Set_ARC_rest_preds(arc, *scc_ancestor_list);
  Set_ARC_rest_succs(arc, *scc_descendent_list);

  *scc_ancestor_list = arc;
  *scc_descendent_list = (ARC_LIST *)((INTPTR)arc | 1);
}

void 
CG_DEP_Delete_SCC_Arcs(ARC_LIST *arcs)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  while (arcs) {
    ARC *arc = ARC_LIST_first(arcs);
    arcs = ARC_LIST_rest(arcs);
    delete_arc(arc);
  }
}

BOOL 
CG_DEP_Graph_Is_Cyclic(BB *bb)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * Currently works only for single-BB dep graph.
 * -----------------------------------------------------------------------
 */
{
  return bb == _cg_dep_bb && cyclic;
}

void 
CG_DEP_Delete_Graph(void *item)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  Is_True(item != NULL,
	  ("NULL value passed to CG_DEP_Delete_Graph routine"));

  Is_True( _cg_dep_bb != NULL || ! _cg_dep_bbs.empty(),
	   ( "CG_DEP_Delete_Graph: no dep graph currently exists" ) );

  delete_gtn_use_arcs();
  BB_MAP_Delete(_cg_dep_op_info);

  MEM_POOL_Pop(&dep_map_nz_pool);
  MEM_POOL_Pop(&dep_nz_pool);
  MEM_POOL_Delete(&dep_map_nz_pool);
  MEM_POOL_Delete(&dep_nz_pool);

  _cg_dep_bb = NULL;
  _cg_dep_bbs.clear();
  
}

#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS)
void 
CG_DEP_Delete_DAG(void)
/* -----------------------------------------------------------------------
 * See "cg_dep_graph.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  delete_gtn_use_arcs();
  BB_MAP_Delete(_cg_dep_op_info);

  MEM_POOL_Pop(&dep_map_nz_pool);
  MEM_POOL_Pop(&dep_nz_pool);
  MEM_POOL_Delete(&dep_map_nz_pool);
  MEM_POOL_Delete(&dep_nz_pool);
}
#endif

// -----------------------------------------------------------------------
// Computes the whole graph for the block.
// -----------------------------------------------------------------------
//
void 
Invoke_Init_Routines()
{
  // Initialize the pools.
  MEM_POOL_Initialize(&dep_nz_pool, "CG_Dep_Graph", FALSE);
  MEM_POOL_Initialize(&dep_map_nz_pool, "CG_Dep_Graph_BB_OP_MAP", FALSE);
  MEM_POOL_Push(&dep_nz_pool);
  MEM_POOL_Push(&dep_map_nz_pool);

  
  // Initiliaze preparatory routines.
  init_op_info();
  init_reg_assignments();
  init_arcs();
  init_gtn_use_arcs();
  maintain_prebr = FALSE;

  // Initialize the dep_info table.
  INT i;
  for (i = 0; i < sizeof(dep_info_data) / sizeof(dep_info_data[0]); i++) {
    CG_DEP_KIND kind = dep_info_data[i].kind;
    dep_info[kind] = dep_info_data + i;
  }

  // MAP to store OP info. 
  _cg_dep_op_info = BB_MAP_Create();
}

static char *multiple_inst = const_cast<char*>("multiple_instance");

// -----------------------------------------------------------------------
// Update_Entry_For_TN
// Updates the value_map <vmap>, register ops <register_ops> and the OP map 
// <omap>, based on specific properties associated with value_tn <vtn>. 
// <is_result> is a BOOL flag which determines if <vtn> is a result_tn of 
// <cur_op>.
// -----------------------------------------------------------------------
//
static void
Update_Entry_For_TN(
    TN              *vtn, 
    OP              *cur_op, 
    TN_MAP          vmap, 
    void            *register_ops[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1], 
    OP_MAP          omap, 
    BOOL            is_result)
{

  // record entries for TN usages.
  OP *get_op = (OP*) TN_MAP_Get(vmap, vtn);

  if (!get_op) {
    TN_MAP_Set(vmap, vtn, cur_op);
  } else if (get_op != cur_op) {

    // If checking for multiple_instances of results and the two OPs don't
    // have qualifying predicates (i.e unconditional def), or,
    // the qualifying predicates are different, return <multiple_inst>.

    if (is_result || ((void*) get_op == (void*) multiple_inst) ||
	!OP_has_subset_predicate(cur_op, get_op)) {
      TN_MAP_Set(vmap, vtn, multiple_inst);
      OP_MAP_Set(omap, cur_op, multiple_inst);
    }
  }

  // record entries for register usages.
  if (TN_register(vtn) != REGISTER_UNDEFINED) {
    REGISTER reg = TN_register(vtn);
    ISA_REGISTER_CLASS rc = TN_register_class(vtn);
    OP *get_op = (OP*) register_ops[rc][reg];

    if (get_op == NULL) {
      register_ops[rc][reg] = (void*) get_op;
    } else if (get_op != cur_op) {

      // If checking for multiple_instances of results and the two OPs don't
      // have qualifying predicates (i.e unconditional def), or,
      // the qualifying predicates are different, return <multiple_inst>.

      if (is_result || ((void*) get_op == (void*) multiple_inst) ||
	!OP_has_subset_predicate(cur_op, get_op)) {
	register_ops[rc][reg] = (void*) multiple_inst;
	OP_MAP_Set(omap, cur_op, multiple_inst);
      }
    }
  }
}

// -----------------------------------------------------------------------
// See "cg_dep_graph.h" for interface.
// -----------------------------------------------------------------------
//
void 
CG_DEP_Prune_Dependence_Arcs(std::list<BB*>    bblist,
			     BOOL         prune_predicate_arcs,
			     BOOL         trace)
{
  std::list<BB*>::iterator bbi;
  TN_MAP tn_usage_map = TN_MAP_Create();
  void *reg_ops[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1];
  OP_MAP omap = OP_MAP_Create();
  bzero(reg_ops, sizeof(reg_ops));

  // Record multiple-definitions of the same TN. Set to <TRUE> if a TN is
  // multiply-defined. Set to <FALSE> if TN is not multiply-defined.

  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    OP *cur_op;
    FOR_ALL_BB_OPs(*bbi, cur_op) {
      INT i;
      for (i = 0; i < OP_opnds(cur_op); ++i) {
	TN *otn = OP_opnd(cur_op, i);
	if (TN_is_constant(otn)) continue;
	Update_Entry_For_TN(otn, cur_op, tn_usage_map, reg_ops, omap, FALSE);
      }

      for (i = 0; i < OP_results(cur_op); ++i) {
	TN *rtn = OP_result(cur_op, i);
	Update_Entry_For_TN(rtn, cur_op, tn_usage_map, reg_ops, omap, TRUE);
      }
    }
  }

  if (prune_predicate_arcs) {
    FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
      OP *cur_op;

      FOR_ALL_BB_OPs(*bbi, cur_op) {

	// Ignore OPs which comprise multiple instance (or defined) TNs.
	if (OP_MAP_Get(omap, cur_op) == multiple_inst) continue;

	// Ignore memory and xfer ops.
	if (OP_memory(cur_op) || OP_xfer(cur_op)) continue;

	BOOL cond_use = TRUE;
	if (OP_has_predicate(cur_op)) {
	  if (!TN_is_true_pred(OP_opnd(cur_op, OP_PREDICATE_OPND)) &&
	      CGTARG_Can_Be_Speculative(cur_op)) {

	    INT i;
	    cond_use = FALSE;

	    for (i = 0; i < OP_results(cur_op); ++i) {
	      TN *result_tn = OP_result(cur_op, i);

	      BOOL live_out = FALSE;
	      BOOL live_in = FALSE;

	      // We don;t promote instructions which compute predicate
	      // results. we only promote instructions which are consumer
	      // of predicate results. promoting instructions which are
	      // producers of predicate results is unsafe most of the times
	      // in addition to more complex analysis.

	      if (Is_Predicate_REGISTER_CLASS( TN_register_class(result_tn))) 
		{ cond_use = TRUE; break; }

	      // If <result_tn> is a global TN and is live_out, return
	      // conservative result.
	      if (TN_is_global_reg(result_tn)) {
		live_out |= GRA_LIVE_TN_Live_Outof_BB(result_tn, *bbi);
	      }
	      
	      // If <result_tn> is a global TN and is live_in, return
	      // conservative result.
	      if (TN_is_global_reg(result_tn)) {
		live_in |= GRA_LIVE_TN_Live_Into_BB(result_tn, *bbi);
	      }
	      
	      // If <result_tn> has a register assigned, check REG_LIVE.
	      if (TN_register(result_tn) != REGISTER_UNDEFINED) {
		ISA_REGISTER_CLASS result_cl = TN_register_class (result_tn);
		REGISTER result_reg = TN_register (result_tn);
		live_out |= REG_LIVE_Outof_BB (result_cl, result_reg, *bbi);

		if (reg_ops[result_cl][result_reg] == multiple_inst)
		  { cond_use = TRUE; break; }
								  
	      }
	      
	      // (1) if live-out, be conservative. don't know.
	      // (2) the <result_tn> is used as an operand in multiple
	      //     instances with conflicting predicates, be conservative.
	      if (live_in ||
		  live_out || 
		  (TN_MAP_Get(tn_usage_map, result_tn) == multiple_inst)) 
		{ cond_use = TRUE; break;}

	    }

	    // Need to check if there doesn't exist any conditional reaching
	    // definitions.

	    for (i = 0; i < OP_opnds(cur_op); ++i) {
	      TN *opnd_tn = OP_opnd(cur_op, i);
	      if (TN_is_constant(opnd_tn)) continue;
	      OP *defop = (OP *) TN_MAP_Get(tn_usage_map, opnd_tn);

	      // Need to check if <defop> is not <multiple_inst> before
	      // quering for its operands.
	      if (defop && ((void *) defop != (void *) multiple_inst) &&
		  OP_has_predicate(defop)) {
		if (!TN_is_true_pred(OP_opnd(defop, OP_PREDICATE_OPND)))
		  { cond_use = TRUE; break; }
	      }
	    }
	  }
	    
	  // if no conditional definitions reaching <cur_op> operands,
	  // it's now safe to unconditionally promote the qualifying 
	  // predicate.

	  if (!cond_use) {
#ifdef Is_True_On
	    // detect the case if the predicate TN is modified
	    {
	      TN *pred = OP_opnd(cur_op, OP_PREDICATE_OPND);
	      BOOL pred_modified = FALSE;
	      for (OP *op = OP_next(cur_op); op != NULL; op = OP_next(op)) {
		if (pred_modified) {
		  for (INT i = 0; i < OP_results(cur_op); ++i) {
		    TN *result_tn = OP_result(cur_op, i);
		    Is_True(!OP_Refs_TN(op, result_tn),
			    ("CG_DEP_Prune_Dependence_Arcs: predicate TN modified"));
		  }
		} else {
		  if (!TN_is_true_pred(pred) && OP_Defs_TN(op, pred))
		    pred_modified = TRUE;
		}
	      }
	    }
#endif
	    Set_OP_opnd(cur_op, OP_PREDICATE_OPND, True_TN);
	    Set_OP_cond_def_kind(cur_op, OP_ALWAYS_UNC_DEF);
	    if (trace) {
	      fprintf(TFile, "<pred promotion> ");
	      Print_OP_No_SrcLine(cur_op);
	    }
	  }
	} /* OP_has_predicate */
      } /* FOR_ALL_BB_OPs */
    } /* FOR_ALL_BB_STLLIST */
  } /* prune_predicate_arcs */

  TN_MAP_Delete(tn_usage_map);
  OP_MAP_Delete(omap);
}

// -----------------------------------------------------------------------
// See "cg_dep_graph.h" for interface.
// -----------------------------------------------------------------------
//
void 
CG_DEP_Compute_Graph(BB      *bb, 
		     BOOL    assigned_regs,
		     BOOL    compute_cyclic, 
		     BOOL    memread_arcs,
		     BOOL    memin_arcs,
		     BOOL    control_arcs,
		     TN_SET *need_anti_out_dep)
{
  Is_True(BB_rid(bb) == NULL || RID_level(BB_rid(bb)) < RL_CGSCHED
#if defined(TARG_SL)
	  || RID_TYPE_major(BB_rid(bb)) || RID_TYPE_minor(BB_rid(bb))
#endif
	  ,
	  ("cannot compute dep graph for SWP replication BB:%d", BB_id(bb)));

  tracing = Get_Trace(TP_CG, 1);

  // Set the module state. TODO: need to remove all state instances..
  Is_True( _cg_dep_bb == NULL && _cg_dep_bbs.empty(),
	   ( "CG_DEP_Compute_Graph: another dep graph currently exists" ) );
  _cg_dep_bb = bb;

  include_assigned_registers = assigned_regs;
  cyclic = compute_cyclic;
  include_memread_arcs = memread_arcs;
  include_memin_arcs = memin_arcs;
  include_control_arcs = control_arcs;

  Invoke_Init_Routines();

  Compute_BB_Graph(_cg_dep_bb, need_anti_out_dep);

  if (tracing) CG_DEP_Trace_Graph(_cg_dep_bb);
}

// -----------------------------------------------------------------------
// See "cg_dep_graph.h" for interface.
// -----------------------------------------------------------------------
//
void 
CG_DEP_Compute_Region_Graph(std::list<BB*>    bb_region, 
			    BOOL         assigned_regs,
			    BOOL         memread_arcs,
			    BOOL         control_arcs)
{
  tracing = Get_Trace(TP_CG, 1);

  Is_True( _cg_dep_bb == NULL && _cg_dep_bbs.empty(),
	   ( "CG_DEP_Compute_Region_Graph:"
	     " another dep graph currently exists" ) );
  _cg_dep_bbs = bb_region;

  include_assigned_registers = assigned_regs;
  cyclic = FALSE;
  include_memread_arcs = memread_arcs;
  include_control_arcs = control_arcs;

  Invoke_Init_Routines();

  Compute_Region_Graph(_cg_dep_bbs);
  if (tracing) {
    CG_DEP_Trace_HB_Graph(_cg_dep_bbs);
  }
}

#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS)
OP* get_def_op(OP* op , CG_DEP_KIND kind, UINT8 opnd)
// get the define op for <op> accroding to CG_DEP_REGIN arc .
// ARC_LIST_rest only look for ops inside one BB.
// Note : must ensure one of the two opnd of <op> is const.
{
  BB* bb=OP_bb(op);
  OP *def_op=NULL;

  ARC_LIST *list = OP_preds(op);
  mUINT16 max_op_order=0;
  OP* nearest_op=NULL;
  // Traverse all pred op , and find the nearest def op 
  for (; list; list = ARC_LIST_rest(list)) {
    ARC *arc = ARC_LIST_first(list);
    if ( ARC_kind(arc) == kind && ARC_opnd(arc)==opnd ) {
      def_op = ARC_pred(arc);
      if (def_op->order > max_op_order ) {
      	max_op_order = def_op->order;
      	nearest_op = def_op;
      }
    }
  }//for

  if (nearest_op==NULL || nearest_op==op || OP_bb(nearest_op) != bb) return NULL;
  else {
    // If nearest_op is mov , then continue to find the previous def op
#if defined(TARG_IA64)
    if (OP_code(nearest_op)==TOP_mov) {
      def_op = nearest_op;
      // Continue to find the pred nearest def of of def_op
      list = OP_preds(def_op);
      max_op_order=0;  nearest_op=NULL;
      for (; list; list = ARC_LIST_rest(list)) {
      	ARC *arc = ARC_LIST_first(list);
      	if ( ARC_kind(arc) == CG_DEP_REGIN && ARC_opnd(arc)==1 ) {//opnd 1 are used tn of mov 
          def_op = ARC_pred(arc);
          if (def_op->order > max_op_order ) {
      	    max_op_order = def_op->order;
      	    nearest_op = def_op;
          }
        }
      }//for
      if (nearest_op==NULL || nearest_op==op || OP_bb(nearest_op) != bb) return NULL;
      else return nearest_op ;
      
    }
    else return nearest_op; // Not mov , return def op
#else 
    return nearest_op;
#endif // TARG_IA64
  }//else
  
  return NULL;
}

BOOL similar_ops(OP * op1 , OP* op2)
// Return TRUE iff op1 and op2 are the same kinds, and they both in the
// [add, sub , mul, div ] set
{
  if ( ( OP_iadd(op1) ||OP_isub(op1)||OP_imul(op1)||OP_idiv(op1) ) && \
      OP_code(op1)==OP_code(op2)
    ) return TRUE;
  else
    return FALSE;
}

TN* get_const_tn(OP* op)
{
  TN* tn1 = OP_opnd(op, 1);
  TN* tn2 = OP_opnd(op, 2);
  if (TN_is_constant(tn1)) {
    return tn1;
  }
  else if (TN_is_constant(tn2)) {
        return tn2;
      }
      else {
        return NULL;
      }
}

UINT get_var_tn_idx(OP* op)
{
  TN* tn1 = OP_opnd(op, 1);
  TN* tn2 = OP_opnd(op, 2);
  if (TN_is_constant(tn1)) {
    return 2;
  }
  else if (TN_is_constant(tn2)) {
        return 1;
      }
      else {
        return 0; // both are var tn
      }
}

BOOL get_definite_alias_info(OP *pred_op, OP *succ_op)
// return TRUE : definite alias
// return FALSE: cannot judge (possibly aliased)
{
#if defined(TARG_SL)
  if (OP_code(pred_op) == TOP_asm || OP_code(succ_op) == TOP_asm)
    return TRUE;

  if(OP_code(pred_op) == TOP_c2_joint || OP_code(succ_op) == TOP_c2_joint)
    return TRUE;
#endif

  Is_True((OP_load(pred_op) || OP_like_store(pred_op)) &&
     (OP_load(succ_op) || OP_like_store(succ_op)) ,
     ("not a load or store"));

  if (OP_like_store (pred_op) && !OP_store (pred_op) ||
      OP_like_store (succ_op) && !OP_store (succ_op)) {
     return FALSE;
  }

  BB* pred_bb = OP_bb(pred_op); // for debug
  BB* succ_bb = OP_bb(succ_op); // for debug
  
  // Get the TN in the ld or st , which is used in the indirect mem addressing
  INT pred_op_base_num   = OP_find_opnd_use (pred_op, OU_base);
  INT succ_op_base_num   = OP_find_opnd_use (succ_op, OU_base);

  TN* pred_op_base_tn = OP_opnd(pred_op, pred_op_base_num); // TN980
  TN* succ_op_base_tn = OP_opnd(succ_op, succ_op_base_num); // TN988


  // Analyse the possible definite alias between pred_op_base_tn and
  // succ_op_base_tn , if OP_load() find CG_DEP_REGIN arc ,
  // if OP_strore() find CG_DEP_REGOUT
  // stops when encountered pred ld(st)


  OP *def_of_pred_op = get_def_op(pred_op, CG_DEP_REGIN, pred_op_base_num);
  OP *def_of_succ_op = get_def_op(succ_op, CG_DEP_REGIN, succ_op_base_num);


  UINT pred_op_opnd_idx = pred_op_base_num;
  UINT succ_op_opnd_idx = succ_op_base_num;
  
  for ( ; (def_of_pred_op != NULL) && (def_of_succ_op != NULL) ;
        def_of_pred_op = get_def_op(def_of_pred_op,CG_DEP_REGIN,pred_op_opnd_idx) ,
        def_of_succ_op = get_def_op(def_of_succ_op,CG_DEP_REGIN,succ_op_opnd_idx) ) {

    // not the same kind return FALSE
    if ( OP_code(def_of_pred_op) != OP_code(def_of_succ_op) )return FALSE ;

    // both are load , needs to compare their base TNs
    if ( OP_load(def_of_pred_op) && OP_load(def_of_succ_op) ) {
      if ( OP_opnd(def_of_pred_op, OP_find_opnd_use (def_of_pred_op, OU_base)) == \
          OP_opnd(def_of_succ_op, OP_find_opnd_use (def_of_succ_op, OU_base))
        ){
        return TRUE; // load from the same address
      }
      else 
      	return FALSE;
    }

    // both are ALU operations , compare their constant TN and decide whether to continue
    if ( similar_ops(def_of_pred_op,def_of_succ_op) ) {
      TN* pred_const_tn = get_const_tn(def_of_pred_op);
      TN* succ_const_tn = get_const_tn(def_of_succ_op);
      if ( (pred_const_tn != NULL ) && (succ_const_tn!=NULL) && \
          ( TN_value(pred_const_tn)==TN_value(succ_const_tn) )
        ) {
        
        pred_op_opnd_idx = get_var_tn_idx(def_of_pred_op);
        succ_op_opnd_idx = get_var_tn_idx(def_of_succ_op);
        continue;
        
      }
      else
        return FALSE; //const are different , stop to find backwards

    }

  }// end for
  
  return FALSE;
}

void
DAG_BUILDER::Build_Mem_Arcs(OP *op)
{
  UINT16 s, num_definite_arcs = 0;
  ARC *shortest = NULL, *shortest_to_store = NULL;
  BOOL found_definite_memread_succ = FALSE;

  // Search through possible memory predcessors.
  for (OPs::iterator ops_iter = _bb_ops_map[OP_bb(op)].begin();
       ops_iter != _bb_ops_map[OP_bb(op)].end();
       ops_iter++) {
    if (!(OP_load(*ops_iter) || OP_like_store(*ops_iter))) {
      continue;
    }
    
    BOOL definite, have_latency = FALSE;
    UINT8 omega = 0;
    OP *pred = *ops_iter;
    ARC *arc;
    INT16 latency;
    CG_DEP_KIND kind = OP_load(pred) ?
      (OP_load(op) ? CG_DEP_MEMREAD : CG_DEP_MEMANTI) :
      (OP_load(op) ? CG_DEP_MEMIN : CG_DEP_MEMOUT);

    if (kind == CG_DEP_MEMREAD &&
        OP_volatile(pred) && OP_volatile(op)) kind = CG_DEP_MEMVOL;

#if defined(TARG_SL)
    if (kind == CG_DEP_MEMOUT && OP_code(op) == TOP_asm) kind = CG_DEP_MEMVOL;
#endif

    if (kind == CG_DEP_MEMREAD && !_include_memread_arcs
#if defined(TARG_IA64)
	&& !Cache_Has_Conflict(pred,op,kind)
#endif
	)
      continue;

    if (!_cyclic && CG_DEP_Mem_Arc_Pruning >= PRUNE_NON_CYCLIC ||
        _cyclic && omega == 0 && CG_DEP_Mem_Arc_Pruning >= PRUNE_CYCLIC_0) {
      if (kind == CG_DEP_MEMREAD) {
        /* No need to look for MEMREAD succs if we've found a definite one */
        if (found_definite_memread_succ) continue;
        /* MEMREAD arcs aren't for scheduling - have no latency */
        latency = 0;
      } else {
        latency = CG_DEP_Latency(pred, op, kind, 0);
      }
      have_latency = TRUE;
    }

    if (get_mem_dep(pred, op, &definite, _cyclic ? &omega : NULL) ||
        (kind == CG_DEP_MEMREAD
#if defined(TARG_IA64)
	 && Cache_Has_Conflict(pred,op,kind)
#endif
	)) {

      // For OOO machine (eg. T5), non-definite memory dependences can be 
      // relaxed to edges with zero latency. The belief is that this can 
      // help avoid creating false dependences with biased critical info. 
    
      if (!have_latency) latency =
        (CG_DEP_Adjust_OOO_Latency && PROC_is_out_of_order() && !definite) ? 
        0 : CG_DEP_Latency(pred, op, kind, 0);
      
#if defined(TARG_IA64)
      if (omega == 0)
          Cache_Adjust_Latency(pred,op,kind, &latency);
#endif
      if(!definite) {
        definite = get_definite_alias_info(pred, op);
      }

      /* Build a mem dep arc from <op> to <succ> */
      arc = new_arc_with_latency(kind, pred, op, latency, omega, 0, definite);

      /* if MEMIN dependence is not a definite dependence and 
         !_include_memin_arcs is SET, not already a check-load, then
         set the ARC as a dotted edge. */

      if (!CGTARG_Is_OP_Check_Load(op) &&
          kind == CG_DEP_MEMIN && !definite && !_include_memin_arcs) {
        /* volatile op dep on all other valatile ops no matter the dependence 
         * between between them is definite or not, get_mem_dep always set
         * the dependence is indefinite to prevent removal by r/w elimination.
         * So, we need futher checking. 
         */
	// bug fix for OSP_88
	if ((!OP_volatile (pred) || !OP_volatile(op))
#if !defined(TARG_SL) && !defined(TARG_MIPS)
	    && !OP_asm(pred)
#endif
	    ) {
	  Set_ARC_is_dotted(arc, TRUE);
	  _num_data_spec_arcs++;
        }
      }

      found_definite_memread_succ |= (kind == CG_DEP_MEMREAD && definite);
    }
  }
}

void
DAG_BUILDER::Build_Branch_Arcs(OP* op, BOOL include_latency)
{
  ARC *arc;
  // Insert POSTBR dependences.
  for (OPs::iterator ops_iter = _bb_ops_map[OP_bb(op)].begin();
       ops_iter != _bb_ops_map[OP_bb(op)].end();
       ops_iter++) {
      OP* xfer_op = *ops_iter;
      
    if (OP_xfer(xfer_op) &&
        OP_bb(xfer_op) != OP_bb(op) &&
        is_xfer_depndnce_reqd(op, xfer_op)) {
      if (include_latency) {
        arc = new_arc_with_latency(CG_DEP_POSTBR, xfer_op, op, 0,0,0, FALSE);
      } else {
        arc = new_arc(CG_DEP_POSTBR, xfer_op, op, 0, 0, FALSE);
      }

      if (Is_Control_Speculative(xfer_op, op)) {
        Set_ARC_is_dotted(arc, TRUE);
        _num_cntl_spec_arcs++;
      }
    }
  } // PREBR dependences

  // Insert PREBR dependences.
  OP* xfer_op = BB_xfer_op(OP_bb(op));
  
  if (xfer_op && op != xfer_op/* && is_xfer_depndnce_reqd(op, xfer_op)*/) {
    if (include_latency) {
      arc = new_arc_with_latency(CG_DEP_PREBR, op, xfer_op, 0,0,0, FALSE);
    } else {
      arc = new_arc(CG_DEP_PREBR, op, xfer_op, 0, 0, FALSE);
    }
  }
}

void
DAG_BUILDER::Build_Misc_Arcs(OP* op)
{
  for (OPs::iterator ops_iter = _bb_ops_map[OP_bb(op)].begin();
       ops_iter != _bb_ops_map[OP_bb(op)].end();
       ops_iter++) {
    if (CGTARG_Dependence_Required(*ops_iter, op)) {
        new_arc(CG_DEP_MISC, *ops_iter, op, 0, 0, FALSE);
    }

    /* Workaround for PV 707179.  Also see code above dependent on
     * CG_DEP_Add_Alloca_Arcs.
     */
    if (CG_DEP_Add_Alloca_Arcs &&
        op_defines_sp(op) &&
        CG_DEP_Alloca_Aliases(*ops_iter)) {
          new_arc(CG_DEP_MISC, *ops_iter, op, 0, 0, FALSE);
    }
  }
}

void
DAG_BUILDER::Build_Reg_Arcs(OP* op)
{
  INT32 i;

  TN * tn_ptr = OP_opnd (op, OP_PREDICATE_OPND);

  // Start building REGIN Arcs .
  // The switch DAG_BITSET_SWITCH_ON  is used to switch between the old and 
  // new version.
  
  for (i = 0; i < OP_opnds(op); i++) {

#ifndef  DAG_BITSET_SWITCH_ON
    
    OPs& def_ops = Get_Def_Use_OPs(op, i, CG_DEP_REGIN);
    // Build REGIN arc from operand's def.
    for (OPs::iterator ops_iter = def_ops.begin();
         ops_iter != def_ops.end();
         ops_iter++) {
            
#else
    // get the relative def ops into vect : _Define_OPs[]
    Get_Define_OPs(op, i, CG_DEP_REGIN); 
    for(DEFINE_OPS_ITER ops_iter=_Define_OPs.begin();
                ops_iter!=_Define_OPs.end();
                ops_iter++  ){
          
#endif

      // #795487: PQS doesn't work with register allocated code. It
      // uses TN as a handle to query predicate relations and relies
      // on single-reaching definitions. This can no longer be TRUE
      // after register allocation, since a same register can be
      // allocated to multiple TNs. This is a deficiency of the 
      // current PQS implementation.
      
      if (!OP_has_disjoint_predicate(*ops_iter, op)) {
#if defined(TARG_IA64)
            if(i == OP_PREDICATE_OPND) {
                ARC *arc_ptr = new_arc(CG_DEP_CTLSPEC, *ops_iter, op, 0, i, FALSE);
                Set_ARC_is_dotted (arc_ptr, TRUE);
            } else 
#endif
                ARC *arc_ptr = new_arc(CG_DEP_REGIN, *ops_iter, op, 0, i, FALSE);
      }
    }// for
  }
  

  // Start building REGOUT Arcs .
  // The switch DAG_BITSET_SWITCH_ON  is used to switch between the old and
  // new version.
  
  for (i = 0; i < OP_results(op); i++) {

#ifndef  DAG_BITSET_SWITCH_ON
    
    OPs& prev_def_ops = Get_Def_Use_OPs(op, i, CG_DEP_REGOUT);
    // Build REGOUT arc from previous def of same result.
    for (OPs::iterator ops_iter = prev_def_ops.begin();
         ops_iter != prev_def_ops.end();
         ops_iter++) {
    
#else
    // get the relative def ops into vect : _Define_OPs[]
    Get_Define_OPs(op, i, CG_DEP_REGOUT);
    for(DEFINE_OPS_ITER ops_iter=_Define_OPs.begin();
                ops_iter!=_Define_OPs.end();
                ops_iter++  ){
                        
#endif

      // #795487: See above.
      // if (_include_assigned_registers ||
      //    !OP_has_disjoint_predicate(*ops_iter,op)) {
      if (!OP_has_disjoint_predicate(*ops_iter, op)) {
        new_arc(CG_DEP_REGOUT, *ops_iter, op, 0, 0, FALSE);
      }
    }// for
  }// for(i=0; i<OP_results(op)..)


  // Start building REGANTI Arcs .
  // The switch DAG_BITSET_SWITCH_ON  is used to switch between the old and 
  // new version.
  
#ifndef  DAG_BITSET_SWITCH_ON
  // Build Non-cyclic REGANTI arcs
  for(i = 0; i < OP_results(op); i++){
    OPs& use_ops = Get_Def_Use_OPs(op, i, CG_DEP_REGANTI);
    for (OPs::iterator ops_iter = use_ops.begin();
         ops_iter != use_ops.end();
         ops_iter++) {

      BOOL tn_def_found = FALSE;
    
      // #795487: See above.
      // if (_include_assigned_registers ||
      //    !OP_has_disjoint_predicate(*ops_iter,op)) {
      TN * tn = OP_result(op,i) ;
      if (TN_is_register(tn) && TN_register_class(tn) == 
          ISA_REGISTER_CLASS_predicate) {
        tn_def_found = TRUE;
        INT16 opnd_idx = get_opnd_idx (*ops_iter,tn);
        Is_True (opnd_idx >= 0, ("fail to find opnd!"));
        new_arc(CG_DEP_REGANTI, *ops_iter, op, 0, (UINT8)opnd_idx, FALSE);        
      }
      else{

      if (!OP_has_disjoint_predicate(*ops_iter, op)) {
        tn_def_found = TRUE;

          INT16 opnd_idx = get_opnd_idx (*ops_iter,tn);
          Is_True (opnd_idx >= 0, ("fail to find opnd!"));
          ARC * arc = new_arc(CG_DEP_REGANTI, *ops_iter, op, 0,
                              (UINT8)opnd_idx, FALSE);
          adjust_reganti_latency (arc) ;
        }
      }// else
    }
  }
  
#else // defined DAG_BITSET_SWITCH_ON  
  
  for(i = 0; i < OP_opnds(op); i++){
    // get the relative def ops into vect : _Define_OPs[]
    Get_Define_OPs(op, i, CG_DEP_REGANTI);
    for(DEFINE_OPS_ITER ops_iter=_Define_OPs.begin();
                ops_iter!=_Define_OPs.end();
                ops_iter++  ){

      //if(op==*ops_iter) continue;  // omit the case: i=i+1 

        BOOL OUT = FALSE;
      for(INT32 j = 0; (j < OP_results(*ops_iter)) && (OUT==FALSE); j++){

        BOOL tn_def_found = FALSE;
        
#if defined(TARG_IA64)          
        // the following has exchanged *ops_iter and op to each other
        TN * tn = OP_result(*ops_iter,j) ;
        if (TN_is_register(tn) && TN_register_class(tn) == 
            ISA_REGISTER_CLASS_predicate) {
          tn_def_found = TRUE;

          new_arc(CG_DEP_REGANTI, op, *ops_iter, 0, (UINT8)i, FALSE);
          OUT = TRUE;

        }
        else{

          if (!_prdb ||!OP_has_predicate(op) ||!OP_has_predicate(*ops_iter)||
                (_prdb && !_prdb->Partition_Graph(Home_Region(OP_bb(op)))->Is_Disjoint(
                TN_OP_PAIR(OP_opnd(op, OP_PREDICATE_OPND),op),
                TN_OP_PAIR(OP_opnd(*ops_iter, OP_PREDICATE_OPND),*ops_iter)))) {
            tn_def_found = TRUE;

            ARC * arc = new_arc(CG_DEP_REGANTI, op, *ops_iter, 0, (UINT8)i, FALSE);
            OUT = TRUE;
            adjust_reganti_latency (arc) ;
          }
        }// else
#else
        ARC * arc = new_arc(CG_DEP_REGANTI, op, *ops_iter, 0, (UINT8)i, FALSE);
        OUT = TRUE;
        adjust_reganti_latency (arc) ;
#endif
      }// for(j = 0; j < OP_results(op); j++){
    }
  }
  
#endif // end of #ifndef  DAG_BITSET_SWITCH_ON


}
#endif
