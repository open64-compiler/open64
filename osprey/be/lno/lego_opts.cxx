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
// ====================================================================
// ====================================================================
//
// Module: lego_opts.cxx
//
// Revision history:
//  dd-mmm-95 - Original Version
//
// Description:
// 
//    Data structures and routines for lego-specific code transformations.
//
// ====================================================================
//

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
static char **rcs_dummy[] = { &rcs_id, (char **)&rcs_dummy,
			      (char **)&source_file };
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "stdlib.h" 
#include "lnopt_main.h"
#include "config_targ.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "lego_opts.h"
#include "lego_pragma.h"
#include "lego_util.h"
#include "lego.h"
#include "lego_gen.h"
#include "wn_simp.h"
#include "wn_core.h"
#include "tile.h"
#include "targ_sim.h"

/* ========================================================================
   Public Function Declarations
   ======================================================================== */

extern WN *Generate_Bounds(const WN *doloop, 
		           SYMBOL **new_lb, SYMBOL **new_ub, 
			   SYMBOL **new_step, INT bound);
extern BOOL Loop_Bounds_Simple(const WN *doloop);

/* ========================================================================
   Private Function Declarations
   ======================================================================== */

static WN *Generate_Block_Bounds(DISTR_INFO *dinfo, 
				 LEGO_INFO *lego_info,
			         const WN *doloop, SYMBOL **new_lb, 
				 SYMBOL **new_ub);
static WN *Generate_Block_Bounds_Negative(DISTR_INFO *dinfo, 
				 LEGO_INFO *lego_info,
			         const WN *doloop, SYMBOL **new_lb, 
				 SYMBOL **new_ub);
static WN *Generate_Cyclic_Bounds(DISTR_INFO *dinfo, 
				  LEGO_INFO *lego_info,
			          const WN *doloop, SYMBOL **new_lb,
			          SYMBOL **new_ub,SYMBOL **new_step);
static WN *Generate_Blkcyc_Bounds(DISTR_INFO *dinfo, 
			          LEGO_INFO *lego_info,
			          const WN *doloop, SYMBOL **new_lb,
			          SYMBOL **new_ub,SYMBOL **new_step, 
			          INT bound);
static WN *Generate_Runtime_Cyclic_Bounds(DISTR_INFO *dinfo, 
			           LEGO_INFO *lego_info, 
			           const WN *doloop, SYMBOL **new_lb, 
			           SYMBOL **new_ub, SYMBOL **new_step);
static WN* Generate_Simple_Bounds(const WN* wn_loop, SYMBOL** new_lb, 
				  SYMBOL** new_ub, DU_MANAGER* du, 
				  ARRAY_DIRECTED_GRAPH16* dg); 
static WN* Generate_Interleaved_Bounds(const WN* wn_loop, 
			               SYMBOL** new_lb, 
				       SYMBOL** new_ub, 
			               SYMBOL** new_step,
				       DU_MANAGER* du, 
  				       INT bound);
static WN* Generate_Dynamic_Bounds(LEGO_INFO *lego_info,
			           const WN *doloop, 
			           SYMBOL **new_lb, 
			           SYMBOL **new_ub,
			           SYMBOL **new_step,
			           INT bound);
static INT64 Get_Step_Multiplier(WN *doloop, SYMBOL *var);
static BOOL Symbols_Equiv(WN *wn_ldid, SYMBOL *var);

/* ========================================================================
   Public Variables Declarations
   ======================================================================== */
/* ========================================================================
   Private Variables Declarations
   ======================================================================== */
static char Str_Buf[256];

/* ========================================================================
   Public Function Implementations
   ======================================================================== */

WN *Generate_Bounds(const WN *doloop, 
		    SYMBOL **new_lb, 
		    SYMBOL **new_ub,  
		    SYMBOL **new_step, 
		    INT bound)
{
  WN *bounds_code = NULL;
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(doloop);
  LEGO_INFO* lego_info = dli->Lego_Info; 
  MP_INFO* mp_info = dli->Mp_Info; 
  *new_lb = NULL;
  *new_ub = NULL;
  *new_step = NULL;

  if (!doloop) 
    return NULL;

  if (lego_info == NULL) {
  
    Is_True(Upper_Bound_Standardize(WN_end(doloop), TRUE), 
      ("Don't lower untransformable do_across loops.")); 
    FmtAssert(mp_info != NULL, ("No lego info, must be do_across")); 
    switch(mp_info->Sched_Type()) {
    case MP_SCHED_SIMPLE: 
      bounds_code = Generate_Simple_Bounds(doloop, new_lb, new_ub, 
        Du_Mgr, Array_Dependence_Graph); 
      break;
    case MP_SCHED_INTERLEAVE:
      bounds_code = Generate_Interleaved_Bounds(doloop, new_lb, new_ub, 
        new_step, Du_Mgr, bound);
      break;
    default: 
      FmtAssert(TRUE, ("Generate_Bounds does not support this sched type."));
    }  

  } else {

    Is_True(Loop_Bounds_Simple(doloop), 
      ("Generate_Bounds cannot generate code for loop %s with complex bounds",
       ST_name(WN_st(WN_index(doloop)))));

    Is_True(lego_info, ("Generate_Bounds passed empty LEGO_INFO"));
    SYMBOL *array_sym = lego_info->Array();
    if (!array_sym) return NULL;

    if (lego_info->Dynamic_Affinity())
      return Generate_Dynamic_Bounds(lego_info, doloop, new_lb, new_ub, 
        new_step, bound);

    DISTR_ARRAY *dact = Lookup_DACT(array_sym->St());
    Is_True(dact, ("Generate_Bounds: No DACT for array %s in LEGO_INFO",
                   ST_name(array_sym->St())));

    DISTR_INFO *dinfo = dact->Dinfo();
    INT num_dim = dinfo->Num_Dim();
    INT curr_dim = lego_info->Dim_Num();
    Is_True((curr_dim >= 0) && (curr_dim < num_dim), 
      ("Generate_Bounds: Bad dimension (%d) in LEGO_INFO, 0..%d expected",
       curr_dim, num_dim-1));
    FmtAssert(lego_info->Stride() > 0 || dact->Get_Dim(curr_dim)->Distr_Type()
      == DISTRIBUTE_BLOCK, ("Only can handle negative strides with BLOCK"));
    switch (dact->Get_Dim(curr_dim)->Distr_Type()) {
      case DISTRIBUTE_BLOCK:  
	//
	// BLOCK Distribution
	//
        if (lego_info->Stride() < 0) {
	  bounds_code = Generate_Block_Bounds_Negative(dinfo, lego_info, 
            doloop, new_lb, new_ub);
        } else {
	  bounds_code = Generate_Block_Bounds(dinfo, lego_info, doloop,
	    new_lb, new_ub);
        }
	break;
      case DISTRIBUTE_STAR: 
	//
	// * Distribution
	//
	return NULL;  

      case DISTRIBUTE_CYCLIC_CONST: {
	if (dact->Get_Dim(curr_dim)->Chunk_Const_Val() == 1) {
	  //
	  // CYCLIC(1) Distribution
	  //
	  bounds_code = Generate_Cyclic_Bounds(dinfo, lego_info, doloop,
					       new_lb, new_ub, new_step);
	} else {
	  //
	  // CYCLIC(k) Distribution, k is constant
	  //
	  bounds_code = Generate_Blkcyc_Bounds(dinfo, lego_info, doloop, 
					       new_lb, new_ub, new_step, 
					       bound);
	}
	break;
      }

      // CYCLIC(b) Distribution, b is a variable
      //
      case DISTRIBUTE_CYCLIC_EXPR: {
	bounds_code = Generate_Blkcyc_Bounds(dinfo, lego_info, doloop, 
					     new_lb, new_ub, new_step, bound);
      }
    }

  }

  return bounds_code;
} /* Generate_Bounds */


/*
 * Checks if the original loop bounds and step are in a form such that 
 * we can generate scheduling code.  This means:
 * (1) Step is 1
 * (2) Upper bound is of the form i <= bound  
 */
BOOL 
Loop_Bounds_Simple(const WN *doloop)
{
  INT64 step = Step_Size((WN *)doloop);
  if (step != 1) return FALSE;

  BOOL ub_ok = Upper_Bound_Standardize(WN_end(doloop), TRUE);
  return (ub_ok);
} /* Loop_Bounds_Simple */



/* ========================================================================
   Private Functions Implementation
   ======================================================================== */

//-----------------------------------------------------------------------
// NAME: Highest_Unique_Definition 
// FUNCTION: Follows DU chains through simple copies to get the uppermost 
//   unique definition of 'wn_ldid' and returns it.  
//-----------------------------------------------------------------------

static WN* Highest_Unique_Definition(WN* wn_ldid, 
			             DU_MANAGER* du)
{
  if (WN_operator(wn_ldid) != OPR_LDID)
    return wn_ldid; 
  DEF_LIST* def_list = du->Ud_Get_Def(wn_ldid); 
  if (def_list == NULL || def_list->Incomplete())
    return wn_ldid; 
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  INT count = 0;
  WN* wn_def = NULL;   
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    wn_def = node->Wn();
    if (WN_operator(wn_def) != OPR_STID
      || SYMBOL(wn_def) != SYMBOL(wn_ldid) || ++count > 1)
      return wn_ldid; 
  }
  return Highest_Unique_Definition(WN_kid0(wn_def), du);
} /* Highest_Unique_Definition */

 
//-----------------------------------------------------------------------
// NAME: LWN_Copy_Tree_With_High_Defs 
// FUNCTION: Returns a copy of 'wn_tree' and its DU information, and
//   performs copy propagation on the LDIDs in the copy.  
//-----------------------------------------------------------------------

static WN* LWN_Copy_Tree_With_High_Defs(WN* wn_tree, 
				        DU_MANAGER* du)
{
  WN* wn_tree_copy = LWN_Copy_Tree(wn_tree); 
  LWN_Copy_Def_Use(wn_tree, wn_tree_copy, du); 
  LWN_ITER* itr_next = NULL; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_tree_copy); 
  for (; itr != NULL; itr = itr_next) {  
    WN* wn = itr->wn; 
    itr_next = LWN_WALK_TreeNext(itr); 
    if (WN_operator(wn) != OPR_LDID)
      continue;  
    WN* new_def = Highest_Unique_Definition(wn, du);
    if (new_def == wn)
      continue;  
    WN* wn_new = Replace_Wnexp_With_Exp_Copy(wn, new_def, du);  
    if (wn == wn_tree_copy)
      wn_tree_copy = wn_new; 
  }
  return wn_tree_copy;
} /* LWN_Copy_Tree_With_High_Defs */


//-----------------------------------------------------------------------
// NAME: Affinity_Array_Lower_Bound 
// FUNCTION: Returns the lower bound of the array over which 'wn_loop' 
//   has been mapped with the affinity specified in its LEGO_TILE. 
//-----------------------------------------------------------------------

static WN* Affinity_Array_Lower_Bound(const WN* wn_loop)
{
  TYPE_ID index_type = WN_desc(WN_start(wn_loop));
  return LWN_Make_Icon(index_type, 0);
} /* Affinity_Array_Lower_Bound */

 
 
//-----------------------------------------------------------------------
// NAME: Affinity_Array_Upper_Bound 
// FUNCTION: Returns the upper bound of the array over which 'wn_loop' 
//   has been mapped with the affinity specified in its LEGO_TILE. 
//-----------------------------------------------------------------------

static WN* Affinity_Array_Upper_Bound(const WN* wn_loop)
{
  TYPE_ID index_type = WN_desc(WN_start(wn_loop));
  WN* wn_one = LWN_Make_Icon(index_type, 1);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  SYMBOL* sym = dli->Lego_Info->Array();
  DISTR_ARRAY* dact = Lookup_DACT(sym->St());
  WN* wn_size = dact->Array_Size_WN(dli->Lego_Info->Dim_Num());
  WN* wn_upper_bound = AWN_Sub(index_type, wn_size, wn_one);
  return wn_upper_bound;  
} /* Affinity_Array_Upper_Bound */

//-----------------------------------------------------------------------
// NAME: Lower_Bound_In_Affinity_Range 
// FUNCTION: Returns TRUE if:  
//     affinity_function(lower_bound(wn_loop)) 
//       >= lower_bound(affinity_array(wn_loop))  
//   FALSE otherwise. 
// For negative strides, the test becomes:
//     affinity_function(lower_bound(wn_loop)) 
//       <= upper_bound(affinity_array(wn_loop))  
// NOTE: TRUE means that the lower bound of the loop is in the range of the 
//   lower bound of the array used in the affinity mapping for the loop. 
//-----------------------------------------------------------------------

static BOOL Lower_Bound_In_Affinity_Range(const WN* wn_loop,
					  BOOL negative_stride, 
                                          DU_MANAGER* du)
{
  BOOL result = FALSE; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  LEGO_INFO* lego_info = dli->Lego_Info;
  FmtAssert(lego_info != NULL,
    ("Lower_Bound_In_Affinity_Range() called on loop w/o LEGO_INFO\n"));
  WN* wn_bound_array = negative_stride ? Affinity_Array_Upper_Bound(wn_loop)
    : Affinity_Array_Lower_Bound(wn_loop);
  if (wn_bound_array == NULL)
    return FALSE;
  TYPE_ID index_type = WN_desc(WN_start(wn_loop));
  WN* wn_stride = LWN_Make_Icon(index_type, lego_info->Stride());
  WN* wn_offset = LWN_Make_Icon(index_type, lego_info->Offset());
  WN* wn_lb_loop = LWN_Copy_Tree_With_High_Defs(WN_kid0(WN_start(wn_loop)), du);
  WN* wn_mul = AWN_Mpy(index_type, wn_stride, wn_lb_loop);
  WN* wn_affinity = AWN_Add(index_type, wn_mul, wn_offset);
  WN* wn_cmpzero = AWN_Sub(index_type, wn_affinity, wn_bound_array);
  wn_cmpzero = WN_Simplify_Tree(wn_cmpzero);
  if (negative_stride) {
    result = (WN_operator(wn_cmpzero) == OPR_INTCONST
      && WN_const_val(wn_cmpzero) <= 0);
  } else {
    result = (WN_operator(wn_cmpzero) == OPR_INTCONST
      && WN_const_val(wn_cmpzero) >= 0);
  }
  LWN_Delete_Tree(wn_cmpzero); 
  return result; 
} /* Lower_Bound_In_Affinity_Range */


//-----------------------------------------------------------------------
// NAME: Upper_Bound_In_Affinity_Range 
// FUNCTION: Returns TRUE if:  
//     affinity_function(upper_bound(wn_loop))
//       <= upper_bound(affinity_array(wn_loop))
//   FALSE otherwise. 
// For negative strides, the test becomes:
//     affinity_function(upper_bound(wn_loop))
//       >= lower_bound(affinity_array(wn_loop))
// NOTE: TRUE means that the upper bound of the loop is in the range of the 
//   upper bound of the array used in the affinity mapping for the loop. 
//-----------------------------------------------------------------------

static BOOL Upper_Bound_In_Affinity_Range(const WN* wn_loop,
					  BOOL negative_stride, 
                                          DU_MANAGER* du)

{
  BOOL result = FALSE; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  LEGO_INFO* lego_info = dli->Lego_Info;
  FmtAssert(lego_info != NULL,
    ("Lower_Bound_In_Affinity_Range() called on loop w/o LEGO_INFO\n"));
  WN* wn_bound_array_orig = negative_stride ? 
    Affinity_Array_Lower_Bound(wn_loop) : Affinity_Array_Upper_Bound(wn_loop);
  if (wn_bound_array_orig == NULL)
    return FALSE;
  if (!Upper_Bound_Standardize(WN_end(wn_loop), TRUE))
    return FALSE;
  WN* wn_ub_loop = LWN_Copy_Tree_With_High_Defs(UBexp(WN_end(wn_loop)), du);
  WN* wn_bound_array = LWN_Copy_Tree_With_High_Defs(wn_bound_array_orig, du);
  LWN_Delete_Tree(wn_bound_array_orig); 
  TYPE_ID index_type = WN_desc(WN_start(wn_loop));
  WN* wn_stride = LWN_Make_Icon(index_type, lego_info->Stride());
  WN* wn_offset = LWN_Make_Icon(index_type, lego_info->Offset());
  WN* wn_mul = AWN_Mpy(index_type, wn_stride, wn_ub_loop);
  WN* wn_affinity = AWN_Add(index_type, wn_mul, wn_offset);
  WN* wn_cmpzero = AWN_Sub(index_type, wn_affinity, wn_bound_array);
  wn_cmpzero = WN_Simplify_Tree(wn_cmpzero);
  if (negative_stride) {
    result = (WN_operator(wn_cmpzero) == OPR_INTCONST
      && WN_const_val(wn_cmpzero) >= 0);
  } else {
    result = (WN_operator(wn_cmpzero) == OPR_INTCONST
      && WN_const_val(wn_cmpzero) <= 0);
  }
  LWN_Delete_Tree(wn_cmpzero); 
  return result; 
} /* Upper_Bound_In_Affinity_Range */


//-----------------------------------------------------------------------
// NAME: Extended_Lower_Bound 
// FUNCTION: Extend the formula for the lower bound of the lego tiled loop 
//  'wn_loop' from 'wn_gen' to the minimum of this and the original lower
//  bound of 'wn_loop'.  Make variables of size 'type'.  The symbol of 
//  'wn_gen' is 'new_lb'.     
//-----------------------------------------------------------------------

static WN* Extended_Lower_Bound(WN* wn_gen,
                                const WN* wn_loop,
                                TYPE_ID type,
				SYMBOL* new_lb,
  			        BOOL negative_stride, 
                                DU_MANAGER* du)
{
  WN* wn_gen_rhs = WN_kid0(wn_gen); 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  SYMBOL* pid = dli->Lego_Info->Pid_Sym0();
  WN* wn_cond = NULL;
  if (negative_stride) {
    WN* wn_num_threads = Get_Numthreads_Ldid(dli->Lego_Info);
    TYPE_ID desc = WN_desc(wn_num_threads);
    TYPE_ID index_type = Max_Wtype(type, desc);
    WN* wn_one = LWN_Make_Icon(index_type, 1);
    OPCODE subop = OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
    WN* wn_proc_ub = LWN_CreateExp2(subop, wn_num_threads, wn_one);
    OPCODE eq_op = OPCODE_make_op(OPR_EQ, Boolean_type, index_type);
    wn_cond = LWN_CreateExp2(eq_op, AWN_LdidSym(pid), wn_proc_ub);
  } else {
    WN* wn_proc_lb = LWN_Make_Icon(type, 0);
    OPCODE eq_op =
      OPCODE_make_op(OPR_EQ, Boolean_type, type);
    wn_cond = LWN_CreateExp2(eq_op, AWN_LdidSym(pid), wn_proc_lb);
  }
  WN* wn_orig = LWN_Copy_Tree(WN_kid0(WN_start(wn_loop)));
  LWN_Copy_Def_Use(WN_kid0(WN_start(wn_loop)), wn_orig, du);
  WN* wn_else = wn_gen;
  WN* wn_gen_copy = LWN_Copy_Tree(wn_gen_rhs); 
  LWN_Copy_Def_Use(wn_gen_rhs, wn_gen_copy, du); 
  WN* wn_then = AWN_Min(type, wn_orig, wn_gen_copy);
  wn_then = AWN_StidIntoSym(new_lb, wn_then);
  WN* wn_if = LWN_CreateIf(wn_cond, WN_CreateBlock(), WN_CreateBlock());
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_then);
  LWN_Insert_Block_After(WN_else(wn_if), NULL, wn_else);
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_loop));
  IF_INFO *ii =
      CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, FALSE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(LEGO_pool), LEGO_pool);
  Build_Doloop_Stack(wn_if, stack);
  LNO_Build_If_Access(wn_if, stack);
  CXX_DELETE(stack, LEGO_pool);
  return wn_if;
} /* Extended_Lower_Bound */


//-----------------------------------------------------------------------
// NAME: Extended_Upper_Bound 
// FUNCTION: Extend the formula for the upper bound of the lego tiled loop 
//  'wn_loop' from 'wn_gen' to the minimum of this and the original upper
//  bound of 'wn_loop'.  Make variables of size 'type'.  The symbol of 
//  'wn_gen' is 'new_lb'.     
//-----------------------------------------------------------------------

static WN* Extended_Upper_Bound(WN* wn_gen,
                                const WN* wn_loop,
                                TYPE_ID type,
				SYMBOL* new_ub,
				BOOL negative_stride, 
                                DU_MANAGER* du)
{
  WN* wn_gen_rhs = WN_kid0(wn_gen); 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  SYMBOL* pid = dli->Lego_Info->Pid_Sym0();
  FmtAssert(!dli->Lego_Info->Dynamic_Affinity(), 
    ("Should not call this lego tiling with dynamic affinity")); 
  WN* wn_cond = NULL;
  if (negative_stride) {
    WN* wn_proc_lb = LWN_Make_Icon(type, 0);
    OPCODE eq_op =
      OPCODE_make_op(OPR_EQ, Boolean_type, type);
    wn_cond = LWN_CreateExp2(eq_op, AWN_LdidSym(pid), wn_proc_lb);
  } else {
    WN* wn_num_threads = Get_Numthreads_Ldid(dli->Lego_Info);
    TYPE_ID desc = WN_desc(wn_num_threads);
    TYPE_ID index_type = Max_Wtype(type, desc);
    WN* wn_one = LWN_Make_Icon(index_type, 1);
    OPCODE subop = OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
    WN* wn_proc_ub = LWN_CreateExp2(subop, wn_num_threads, wn_one);
    OPCODE eq_op = OPCODE_make_op(OPR_EQ, Boolean_type, index_type);
    wn_cond = LWN_CreateExp2(eq_op, AWN_LdidSym(pid), wn_proc_ub);
  } 
  WN* wn_orig = LWN_Copy_Tree(UBexp(WN_end(wn_loop)));
  LWN_Copy_Def_Use(UBexp(WN_end(wn_loop)), wn_orig, du);
  WN* wn_else = wn_gen;
  WN* wn_gen_copy = LWN_Copy_Tree(wn_gen_rhs); 
  LWN_Copy_Def_Use(wn_gen_rhs, wn_gen_copy, du);
  WN* wn_then = AWN_Max(type, wn_orig, wn_gen_copy);
  wn_then = AWN_StidIntoSym(new_ub, wn_then);
  WN* wn_if = LWN_CreateIf(wn_cond, WN_CreateBlock(), WN_CreateBlock());
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_then);
  LWN_Insert_Block_After(WN_else(wn_if), NULL, wn_else);
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_loop));
  IF_INFO *ii =
      CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, FALSE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(LEGO_pool), LEGO_pool);
  Build_Doloop_Stack(wn_if, stack);
  LNO_Build_If_Access(wn_if, stack);
  CXX_DELETE(stack, LEGO_pool);
  return wn_if;
} /* Extended_Upper_Bound */


/*
 * Generate bounds for block distributions:
 *
 * new_lb = max(lb, divceil((p*b)-c,s))
 * new_ub = min(ub, divfloor(((p+1)*b)-c-1,s)
 *
 * step is unchanged
 * 
 * where 
 *   lb = original lower bound
 *   ub = original upper bound
 *   p  = processor id
 *   b  = blocksize 
 *   c  = offset
 *   s  = stride, s > 0
 * 
 */
WN *
Generate_Block_Bounds(DISTR_INFO *dinfo, 
		      LEGO_INFO *lego_info,
		      const WN *doloop, 
		      SYMBOL **new_lb, 
		      SYMBOL **new_ub)
{
  INT64 linenum = WN_Get_Linenum(doloop);
  WN *bounds_code = WN_CreateBlock();
  WN_Set_Linenum(bounds_code, linenum);

  WN *curr_lb_wn = WN_kid0(WN_start(doloop));
  WN *curr_ub_wn = UBexp(WN_end(doloop));
  INT curr_dim = lego_info->Dim_Num();

  SYMBOL *my_pid = lego_info->Pid_Sym0();
  SYMBOL *dim_size = dinfo->Get_Dimsize(curr_dim);

  Is_True(my_pid && dim_size, 
	  ("Generate_Block_Bounds: Bad pid and/or dim_size\n"));

  FmtAssert((lego_info->Stride() > 0), 
	    ("Generate_Block_Bounds called with stride <= 0"));

  //
  // Lower bound
  //
  TYPE_ID type = Promote_Type(Do_Wtype((WN *) doloop)); 
  WN *new_lb_wn = AWN_Mpy(type,AWN_LdidSym(my_pid),dinfo->Dimsize(curr_dim));
  new_lb_wn = AWN_Sub(type, new_lb_wn, 
    LWN_Make_Icon(type, lego_info->Offset()));
  if (lego_info->Stride() != 1) {
    new_lb_wn = LWN_CreateDivceil(type, new_lb_wn, 
				  LWN_Make_Icon(type, lego_info->Stride()));
  }

  WN *curr_lb_copy = LWN_Copy_Tree(curr_lb_wn);
  LWN_Copy_Def_Use(curr_lb_wn, curr_lb_copy, Du_Mgr);
  new_lb_wn = AWN_Max(type, new_lb_wn, curr_lb_copy);
  sprintf(Str_Buf, "$dsm_block_lb%d", WN_map_id(doloop));
  *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
  new_lb_wn = AWN_StidIntoSym(*new_lb, new_lb_wn);
  if (!Lower_Bound_In_Affinity_Range(doloop, FALSE, Du_Mgr))
    new_lb_wn = Extended_Lower_Bound(new_lb_wn, doloop, type, *new_lb, 
      FALSE, Du_Mgr);
  WN_Set_Linenum(new_lb_wn, linenum);
  LWN_Insert_Block_After(bounds_code, NULL, new_lb_wn);
      
  //
  // Upper bound 
  // 
  WN *new_ub_wn = AWN_Add(type, AWN_LdidSym(my_pid), LWN_Make_Icon(type, 1));
  new_ub_wn = AWN_Mpy(type, new_ub_wn, dinfo->Dimsize(curr_dim));
  new_ub_wn = AWN_Sub(type, new_ub_wn, LWN_Make_Icon(type, lego_info->Offset() + 1));

  if (lego_info->Stride() != 1) {
    new_ub_wn = LWN_CreateDivfloor(type, new_ub_wn,
				   LWN_Make_Icon(type, lego_info->Stride()));
  }

  WN *curr_ub_copy = LWN_Copy_Tree(curr_ub_wn);
  LWN_Copy_Def_Use(curr_ub_wn, curr_ub_copy, Du_Mgr);
  new_ub_wn = AWN_Min(type, new_ub_wn, curr_ub_copy);
  sprintf(Str_Buf, "$dsm_block_ub%d", WN_map_id(doloop));
  *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
  new_ub_wn = AWN_StidIntoSym(*new_ub, new_ub_wn);
  if (!Upper_Bound_In_Affinity_Range(doloop, FALSE, Du_Mgr))
    new_ub_wn = Extended_Upper_Bound(new_ub_wn, doloop, type, *new_ub, 
      FALSE, Du_Mgr);
  WN_Set_Linenum(new_ub_wn, linenum);
  LWN_Insert_Block_After(bounds_code, NULL, new_ub_wn);

  //
  // Last local (for doacross only) 
  //
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(doloop);
  if (dli->Mp_Info != NULL) {
    FmtAssert(WN_operator(WN_end(doloop)) == OPR_LE, 
      ("Loop is not in standard form")); 
    WN* wn_ub_loop = LWN_Copy_Tree(WN_kid1(WN_end(doloop)));
    LWN_Copy_Def_Use(WN_kid1(WN_end(doloop)), wn_ub_loop, Du_Mgr); 
    WN* wn_stride = LWN_Make_Icon(type, lego_info->Stride());
    WN* wn_offset = LWN_Make_Icon(type, lego_info->Offset()); 
    WN* wn_mul = AWN_Mpy(type, wn_stride, wn_ub_loop);
    WN* wn_index = AWN_Add(type, wn_mul, wn_offset);
    WN* wn_div = AWN_Div_Safe(type, wn_index, dinfo->Dimsize(curr_dim)); 
    FmtAssert(!dli->Lego_Info->Dynamic_Affinity(), 
      ("Should not call this lego tiling with dynamic affinity")); 
    WN* wn_last_proc = AWN_Min(type, wn_div, Get_Numthreads_Ldid(lego_info));
    OPCODE op_eq = OPCODE_make_op(OPR_EQ, Boolean_type, type);  
    WN* wn_cond = LWN_CreateExp2(op_eq, AWN_LdidSym(my_pid), wn_last_proc);   
    sprintf(Str_Buf, "$da_last_iter%d",  WN_map_id(doloop));
    SYMBOL* sym_last_iter = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)),
      &LNO_default_pool);
    WN* wn_last_iter = AWN_StidIntoSym(sym_last_iter, wn_cond);
    WN_Set_Linenum(wn_last_iter, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, wn_last_iter); 
    if (dli->Mp_Info->Nest_Total() == 1) 
        Du_Mgr->Add_Def_Use(wn_last_iter, Return_Node(Current_Func_Node));
    WN* wn_region = NULL;
    for (wn_region = (WN*) doloop; wn_region != NULL;
      wn_region = LWN_Get_Parent(wn_region))
      if (WN_opcode(wn_region) == OPC_REGION)
	break;
    FmtAssert(wn_region != NULL, ("Could not find region."));
    WN* wn_pragma = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                    ST_IDX_ZERO,
                                    (INT32) WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED,
                                    0);
    LWN_Insert_Block_Before(WN_region_pragmas(wn_region), NULL, wn_pragma); 
    Add_Pragma_To_MP_Region(doloop, sym_last_iter->St(), 
      sym_last_iter->WN_Offset(), WN_PRAGMA_LASTTHREAD);
  }

  return bounds_code;
} /* Generate_Block_Bounds */

/*
 * Generate bounds for block distributions:
 *
 * nub = min(ub, divfloor(c-p*b, abs(s))); 
 * nlb = divceil(c+1-(p+1)*b, abs(s)); 
 * if (step > 1) {
 *   tmp = (nlb-lb) % step;
 *   if (tmp) tmp = step - tmp;
 *   nlb = nlb + tmp;
 *  } 
 *  nlb = max(lb, nlb);
 *
 * step is unchanged
 * 
 * where 
 *   lb = original lower bound
 *   ub = original upper bound
 *   p  = processor id
 *   b  = blocksize 
 *   c  = offset
 *   s  = stride, s < 0
 * 
 * NOTE: We aen't implementing the case for step > 1 at this time 
 *   because we expect preopt to normalize all loops so that they 
 *   have a step of 1. 
 */
static WN* Generate_Block_Bounds_Negative(DISTR_INFO *dinfo, 
		      		          LEGO_INFO *lego_info,
		      			  const WN *doloop, 
		      			  SYMBOL **new_lb, 
		      			  SYMBOL **new_ub)
{
  INT64 linenum = WN_Get_Linenum(doloop);
  WN *bounds_code = WN_CreateBlock();
  WN_Set_Linenum(bounds_code, linenum);

  WN* curr_lb_wn = WN_kid0(WN_start(doloop));
  WN* curr_ub_wn = UBexp(WN_end(doloop));
  WN* curr_step_wn = Loop_Step((WN*) doloop);  
  INT curr_dim = lego_info->Dim_Num();

  SYMBOL *my_pid = lego_info->Pid_Sym0();
  SYMBOL *dim_size = dinfo->Get_Dimsize(curr_dim);
  SYMBOL* new_tmp = NULL;

  Is_True(my_pid && dim_size, 
	  ("Generate_Block_Bounds: Bad pid and/or dim_size\n"));
  //
  // Lower bound: divceil(c+1-(p+1)*b, abs(s))
  //
  TYPE_ID type = Promote_Type(Do_Wtype((WN *) doloop)); 
  WN* wn_add = AWN_Add(type, AWN_LdidSym(my_pid), LWN_Make_Icon(type, 1));
  WN* wn_prod = AWN_Mpy(type, dinfo->Dimsize(curr_dim), wn_add);
  WN* wn_const = LWN_Make_Icon(type, lego_info->Offset() + 1);
  WN* wn_sub = AWN_Sub(type, wn_const, wn_prod);
  WN* wn_abs_stride = LWN_Make_Icon(type, -lego_info->Stride()); 
  WN* wn_divceil = LWN_CreateDivceil(type, wn_sub, wn_abs_stride);
  WN* new_lb_wn = NULL; 
  WN* wn_lb_copy = LWN_Copy_Tree(curr_lb_wn);
  LWN_Copy_Def_Use(curr_lb_wn, wn_lb_copy, Du_Mgr);
  WN* wn_max = AWN_Max(type, wn_lb_copy, wn_divceil);
  sprintf(Str_Buf, "$dsm_block_lb%d", WN_map_id(doloop));
  *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
  new_lb_wn = AWN_StidIntoSym(*new_lb, wn_max);
  if (!Lower_Bound_In_Affinity_Range(doloop, TRUE, Du_Mgr))
    new_lb_wn = Extended_Lower_Bound(new_lb_wn, doloop, type, *new_lb, 
      TRUE, Du_Mgr);
  WN_Set_Linenum(new_lb_wn, linenum);
  LWN_Insert_Block_After(bounds_code, NULL, new_lb_wn);

  //
  // Upper bound: nub = min(ub, divfloor(c-p*b, abs(s)))
  // 
  wn_prod = AWN_Mpy(type, AWN_LdidSym(my_pid), dinfo->Dimsize(curr_dim));
  wn_sub = AWN_Sub(type, LWN_Make_Icon(type, lego_info->Offset()), wn_prod);
  wn_abs_stride = LWN_Make_Icon(type, -lego_info->Stride()); 
  WN* wn_divfloor = LWN_CreateDivfloor(type, wn_sub, wn_abs_stride);
  WN* wn_ub_copy = LWN_Copy_Tree(curr_ub_wn);
  LWN_Copy_Def_Use(curr_ub_wn, wn_ub_copy, Du_Mgr);
  WN* wn_min = AWN_Min(type, wn_ub_copy, wn_divfloor); 
  sprintf(Str_Buf, "$dsm_block_ub%d", WN_map_id(doloop));
  *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
  WN* new_ub_wn = AWN_StidIntoSym(*new_ub, wn_min);
  if (!Upper_Bound_In_Affinity_Range(doloop, TRUE, Du_Mgr))
    new_ub_wn = Extended_Upper_Bound(new_ub_wn, doloop, type, *new_ub, 
      TRUE, Du_Mgr);
  WN_Set_Linenum(new_ub_wn, linenum);
  LWN_Insert_Block_After(bounds_code, NULL, new_ub_wn);

  //
  // Last local (for doacross only) 
  //
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(doloop);
  if (dli->Mp_Info != NULL) {
    FmtAssert(WN_operator(WN_end(doloop)) == OPR_LE, 
      ("Loop is not in standard form")); 
    WN* wn_ub_loop = LWN_Copy_Tree(WN_kid1(WN_end(doloop)));
    LWN_Copy_Def_Use(WN_kid1(WN_end(doloop)), wn_ub_loop, Du_Mgr); 
    WN* wn_stride = LWN_Make_Icon(type, lego_info->Stride());
    WN* wn_offset = LWN_Make_Icon(type, lego_info->Offset()); 
    WN* wn_mul = AWN_Mpy(type, wn_stride, wn_ub_loop);
    WN* wn_index = AWN_Add(type, wn_mul, wn_offset);
    WN* wn_div = AWN_Div_Safe(type, wn_index, dinfo->Dimsize(curr_dim)); 
    FmtAssert(!dli->Lego_Info->Dynamic_Affinity(), 
      ("Should not call this lego tiling with dynamic affinity")); 
    WN* wn_last_proc = AWN_Max(type, wn_div, LWN_Make_Icon(type, 0));
    OPCODE op_eq = OPCODE_make_op(OPR_EQ, Boolean_type, type);  
    WN* wn_cond = LWN_CreateExp2(op_eq, AWN_LdidSym(my_pid), wn_last_proc);   
    sprintf(Str_Buf, "$da_last_iter%d",  WN_map_id(doloop));
    SYMBOL* sym_last_iter = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)),
      &LNO_default_pool);
    WN* wn_last_iter = AWN_StidIntoSym(sym_last_iter, wn_cond);
    WN_Set_Linenum(wn_last_iter, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, wn_last_iter); 
    if (dli->Mp_Info->Nest_Total() == 1) 
        Du_Mgr->Add_Def_Use(wn_last_iter, Return_Node(Current_Func_Node));
    WN* wn_region = NULL;
    for (wn_region = (WN*) doloop; wn_region != NULL;
      wn_region = LWN_Get_Parent(wn_region))
      if (WN_opcode(wn_region) == OPC_REGION)
	break;
    FmtAssert(wn_region != NULL, ("Could not find region."));
    WN* wn_pragma = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                    ST_IDX_ZERO,
                                    (INT32) WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED,
                                    0);
    LWN_Insert_Block_Before(WN_region_pragmas(wn_region), NULL, wn_pragma); 
    Add_Pragma_To_MP_Region(doloop, sym_last_iter->St(), 
      sym_last_iter->WN_Offset(), WN_PRAGMA_LASTTHREAD);
  }

  return bounds_code;
} /* Generate_Block_Bounds_Negative */


/*
 * Generate bounds for cyclic distributions: 
 *
 * new_lb = ((p - lb - c) mod P) + lb
 * new_step = P
 * 
 * ub is unchanged
 *
 * where 
 *   lb = original lower bound
 *   p  = processor id
 *   P  = number of processors
 *   c  = offset
 *   s  = stride, s == 1
 *
 * Note: In the calculation of new_lb above, the mod operation is a true mod -- 
 *       rem cannot be used instead.  The expression (p - lb - c) can be negative,
 *       and mod and rem have different semantics when the operands are negative.
 */
WN *
Generate_Cyclic_Bounds(DISTR_INFO *dinfo, 
		       LEGO_INFO *lego_info,
		       const WN *doloop, 
		       SYMBOL **new_lb, 
		       SYMBOL **new_ub, 
		       SYMBOL **new_step)
{
  if (lego_info->Stride() > 1) {
    return Generate_Runtime_Cyclic_Bounds(dinfo, lego_info, doloop, 
				   new_lb, new_ub, new_step);
  }

  INT64 linenum = WN_Get_Linenum(doloop);
  WN *bounds_code = WN_CreateBlock();
  WN_Set_Linenum(bounds_code, linenum);

  WN *curr_lb_wn = WN_kid0(WN_start(doloop));
  WN *curr_ub_wn = UBexp(WN_end(doloop));

  INT curr_dim = lego_info->Dim_Num();

  SYMBOL *my_pid = lego_info->Pid_Sym0();
  SYMBOL *num_procs = dinfo->Get_Numprocs(curr_dim);

  Is_True(my_pid && num_procs,
	  ("Generate_Cyclic_Bounds: Bad pid and/or num_procs\n"));

  FmtAssert((lego_info->Stride() > 0), 
	    ("Generate_Cyclic_Bounds called with stride <= 0"));

  //
  // Lower bound
  //
  TYPE_ID type = Promote_Type(Do_Wtype((WN *) doloop)); 
  WN *curr_lb_copy = LWN_Copy_Tree(curr_lb_wn);
  LWN_Copy_Def_Use(curr_lb_wn, curr_lb_copy, Du_Mgr);
  WN *new_lb_wn = AWN_Sub(type, AWN_LdidSym(my_pid), curr_lb_copy);
  new_lb_wn = AWN_Sub(type, new_lb_wn, 
    LWN_Make_Icon(type, lego_info->Offset()));
  new_lb_wn = AWN_Mod_Safe(type, new_lb_wn, dinfo->Numprocs(curr_dim));

  curr_lb_copy = LWN_Copy_Tree(curr_lb_wn);
  LWN_Copy_Def_Use(curr_lb_wn, curr_lb_copy, Du_Mgr);
  new_lb_wn = AWN_Add(type, new_lb_wn, curr_lb_copy);

  sprintf(Str_Buf, "$dsm_cyclic_lb%d", WN_map_id(doloop));
  *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
  new_lb_wn = AWN_StidIntoSym(*new_lb, new_lb_wn);
  WN_Set_Linenum(new_lb_wn, linenum);
  LWN_Insert_Block_After(bounds_code, NULL, new_lb_wn);

  // 
  // Upper bound
  //
  WN *curr_ub_copy = LWN_Copy_Tree(curr_ub_wn);
  LWN_Copy_Def_Use(curr_ub_wn, curr_ub_copy, Du_Mgr);

  sprintf(Str_Buf, "$dsm_cyclic_ub%d", WN_map_id(doloop));
  *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
  WN *new_ub_wn = AWN_StidIntoSym(*new_ub, curr_ub_copy);
  WN_Set_Linenum(new_ub_wn, linenum);
  LWN_Insert_Block_After(bounds_code, NULL, new_ub_wn);

  //
  // Step 
  // 
  sprintf(Str_Buf, "$dsm_cyclic_step%d", WN_map_id(doloop));
  *new_step = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
  WN *new_step_wn = AWN_StidIntoSym(*new_step, dinfo->Numprocs(curr_dim));
  WN_Set_Linenum(new_step_wn, linenum);
  LWN_Insert_Block_After(bounds_code, NULL, new_step_wn);

  // 
  // Last local (for doacross only) 
  //
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(doloop);
  if (dli->Mp_Info != NULL) {
    FmtAssert(WN_operator(WN_end(doloop)) == OPR_LE,
      ("Loop is not in standard form"));
    WN* wn_ub_loop = LWN_Copy_Tree(WN_kid1(WN_end(doloop)));
    LWN_Copy_Def_Use(WN_kid1(WN_end(doloop)), wn_ub_loop, Du_Mgr);
    WN* wn_stride = LWN_Make_Icon(type, lego_info->Stride());
    WN* wn_offset = LWN_Make_Icon(type, lego_info->Offset());
    WN* wn_mul = AWN_Mpy(type, wn_stride, wn_ub_loop);
    WN* wn_index = AWN_Add(type, wn_mul, wn_offset);
    FmtAssert(!dli->Lego_Info->Dynamic_Affinity(), 
      ("Should not call this lego tiling with dynamic affinity")); 
    WN* wn_last_proc = AWN_Mod_Safe(type, wn_index, 
      Get_Numthreads_Ldid(lego_info));
    OPCODE op_eq = OPCODE_make_op(OPR_EQ, Boolean_type, type);
    WN* wn_cond = LWN_CreateExp2(op_eq, AWN_LdidSym(my_pid), wn_last_proc);
    sprintf(Str_Buf, "$da_last_iter%d",  WN_map_id(doloop));
    SYMBOL* sym_last_iter = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)),
      &LNO_default_pool);
    WN* wn_last_iter = AWN_StidIntoSym(sym_last_iter, wn_cond);
    WN_Set_Linenum(wn_last_iter, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, wn_last_iter);
    if (dli->Mp_Info->Nest_Total() == 1)
      Du_Mgr->Add_Def_Use(wn_last_iter, Return_Node(Current_Func_Node));
    WN* wn_region = NULL;
    for (wn_region = (WN*) doloop; wn_region != NULL;
      wn_region = LWN_Get_Parent(wn_region))
      if (WN_opcode(wn_region) == OPC_REGION)
        break;
    FmtAssert(wn_region != NULL, ("Could not find region."));
    WN* wn_pragma = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                    ST_IDX_ZERO,
                                    (INT32) WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED,
                                    0);
    LWN_Insert_Block_Before(WN_region_pragmas(wn_region), NULL, wn_pragma);
    Add_Pragma_To_MP_Region(doloop, sym_last_iter->St(),
      sym_last_iter->WN_Offset(), WN_PRAGMA_LASTTHREAD);
  }

  return bounds_code;
} /* Generate_Cyclic_Bounds */


/*
 * Generate bounds for block-cyclic distributions: 
 *
 * bound == 0: (middle loop, iterates over cycles)
 *
 *   new_lb = divceil(lb*stride + offset + 1, b*P) - 1
 *   new_ub = divceil(ub*stride + offset + 1, b*P) - 1
 *   new_step = 1;
 *
 * bound == 1: (inner loop, iterates over elements in each block)
 * 
 *   new_lb = max(lb, divceil(p*b-offset+(b*P*q), stride))
 *   new_ub = min(ub, divfloor((p+1)*b-offset-1+(b*P*q), stride))
 * 
 *   step is unchanged
 *
 * where 
 *   lb = original lower bound
 *   p  = processor id
 *   q  = cycle start (e.g. index variable of middle loop) 
 *   P  = number of processors
 *   b  = blocksize
 *   c  = offset
 *   s  = stride, s > 0
 *
 */
WN *
Generate_Blkcyc_Bounds(DISTR_INFO *dinfo, 
		       LEGO_INFO *lego_info,
		       const WN *doloop, 
		       SYMBOL **new_lb, 
		       SYMBOL **new_ub, 
		       SYMBOL **new_step,
		       INT bound)
{
  INT64 linenum = WN_Get_Linenum(doloop);
  WN *bounds_code = WN_CreateBlock();
  WN_Set_Linenum(bounds_code, linenum);

  WN *curr_lb_wn = WN_kid0(WN_start(doloop));
  WN *curr_ub_wn = UBexp(WN_end(doloop));

  INT curr_dim = lego_info->Dim_Num();

  SYMBOL *my_pid0 = lego_info->Pid_Sym0();
  SYMBOL *my_pid1 = lego_info->Pid_Sym1();
  SYMBOL *num_procs = dinfo->Get_Numprocs(curr_dim);

  Is_True(my_pid0 && my_pid1 && num_procs,
          ("Generate_Blkcyc_Bounds: Bad pid and/or num_procs\n"));

  FmtAssert((lego_info->Stride() > 0), 
	    ("Generate_BlkCyc_Bounds called with stride <= 0"));

  TYPE_ID type = Promote_Type(Do_Wtype((WN*) doloop)); 
  if (bound == 0) { 

    //
    // Lower bound 
    //
    WN *num_cycles_wn = AWN_Mpy(type, dinfo->Chunksize(curr_dim), 
				dinfo->Numprocs(curr_dim));    // b*P

    WN *curr_lb_copy = LWN_Copy_Tree(curr_lb_wn);
    LWN_Copy_Def_Use(curr_lb_wn, curr_lb_copy, Du_Mgr);
    WN *new_lb_wn = AWN_Mpy(type, curr_lb_copy,
			    LWN_Make_Icon(type,lego_info->Stride()));
    new_lb_wn = AWN_Add(type, new_lb_wn, 
      LWN_Make_Icon(type, lego_info->Offset() + 1));

    WN *num_cycles_copy = LWN_Copy_Tree(num_cycles_wn);
    LWN_Copy_Def_Use(num_cycles_wn, num_cycles_copy, Du_Mgr);
    new_lb_wn = LWN_CreateDivceil(type, new_lb_wn, num_cycles_copy);
    new_lb_wn = AWN_Sub(type, new_lb_wn, LWN_Make_Icon(type, 1));

    sprintf(Str_Buf, "$dsm_blkcyc_outer_lb%d", WN_map_id(doloop));
    *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
    new_lb_wn = AWN_StidIntoSym(*new_lb, new_lb_wn);
    WN_Set_Linenum(new_lb_wn, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, new_lb_wn);

    // 
    // Upper bound
    //
    WN *curr_ub_copy = LWN_Copy_Tree(curr_ub_wn);
    LWN_Copy_Def_Use(curr_ub_wn, curr_ub_copy, Du_Mgr);
    WN *new_ub_wn = AWN_Mpy(type, curr_ub_copy,
			    LWN_Make_Icon(type,lego_info->Stride()));
    new_ub_wn = AWN_Add(type, new_ub_wn, 
      LWN_Make_Icon(type, lego_info->Offset() + 1));

    num_cycles_copy = LWN_Copy_Tree(num_cycles_wn);
    LWN_Copy_Def_Use(num_cycles_wn, num_cycles_copy, Du_Mgr);
    new_ub_wn = LWN_CreateDivceil(type, new_ub_wn, num_cycles_copy);
    new_ub_wn = AWN_Sub(type, new_ub_wn, LWN_Make_Icon(type, 1));

    sprintf(Str_Buf, "$dsm_blkcyc_outer_ub%d", WN_map_id(doloop));
    *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
    new_ub_wn = AWN_StidIntoSym(*new_ub, new_ub_wn);
    WN_Set_Linenum(new_ub_wn, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, new_ub_wn);

    //
    // Step 
    // 
    sprintf(Str_Buf, "$dsm_blkcyc_outer_step%d", WN_map_id(doloop));
    *new_step = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
    WN *new_step_wn = AWN_StidIntoSym(*new_step, LWN_Make_Icon(type, 1));
    WN_Set_Linenum(new_step_wn, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, new_step_wn);

    LWN_Delete_Tree(num_cycles_wn);

    // 
    // Last local (for doacross only) 
    //
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(doloop);
    if (dli->Mp_Info != NULL) {
      FmtAssert(WN_operator(WN_end(doloop)) == OPR_LE,
	("Loop is not in standard form"));
      WN* wn_ub_loop = LWN_Copy_Tree(WN_kid1(WN_end(doloop)));
      LWN_Copy_Def_Use(WN_kid1(WN_end(doloop)), wn_ub_loop, Du_Mgr);
      WN* wn_stride = LWN_Make_Icon(type, lego_info->Stride());
      WN* wn_offset = LWN_Make_Icon(type, lego_info->Offset());
      WN* wn_mul = AWN_Mpy(type, wn_stride, wn_ub_loop);
      WN* wn_index = AWN_Add(type, wn_mul, wn_offset);
      WN* wn_div = AWN_Div_Safe(type, wn_index, dinfo->Chunksize(curr_dim));  
      FmtAssert(!dli->Lego_Info->Dynamic_Affinity(), 
	("Should not call this lego tiling with dynamic affinity")); 
      WN* wn_last_proc = AWN_Mod_Safe(type, wn_div, 
        Get_Numthreads_Ldid(lego_info));
      OPCODE op_eq = OPCODE_make_op(OPR_EQ, Boolean_type, type);
      WN* wn_cond = LWN_CreateExp2(op_eq, AWN_LdidSym(my_pid0), wn_last_proc);
      sprintf(Str_Buf, "$da_last_iter%d",  WN_map_id(doloop));
      SYMBOL* sym_last_iter = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)),
	&LNO_default_pool);
      WN* wn_last_iter = AWN_StidIntoSym(sym_last_iter, wn_cond);
      WN_Set_Linenum(wn_last_iter, linenum);
      LWN_Insert_Block_After(bounds_code, NULL, wn_last_iter);
      if (dli->Mp_Info->Nest_Total() == 1)
	Du_Mgr->Add_Def_Use(wn_last_iter, Return_Node(Current_Func_Node));
      WN* wn_region = NULL;
      for (wn_region = (WN*) doloop; wn_region != NULL;
	wn_region = LWN_Get_Parent(wn_region))
	if (WN_opcode(wn_region) == OPC_REGION)
	  break;
      FmtAssert(wn_region != NULL, ("Could not find region."));
      WN* wn_pragma = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                      ST_IDX_ZERO,
                                      (INT32)WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED,
                                      0);
      LWN_Insert_Block_Before(WN_region_pragmas(wn_region), NULL, wn_pragma);
      Add_Pragma_To_MP_Region(doloop, sym_last_iter->St(),
	sym_last_iter->WN_Offset(), WN_PRAGMA_LASTTHREAD);
    }

  } else {

    FmtAssert(bound == 1, 
      ("Generate_Blkcyc_Bounds: Only bound = {0,1} is valid."));

    //
    // Lower bound 
    //
    WN *curr_cycle_wn = AWN_Mpy(type, dinfo->Chunksize(curr_dim), 
				dinfo->Numprocs(curr_dim));    // b*P
    curr_cycle_wn = AWN_Mpy(type, curr_cycle_wn, AWN_LdidSym(my_pid1));// b*P*q
    WN *new_lb_wn = AWN_Mpy(type, dinfo->Chunksize(curr_dim), 
      AWN_LdidSym(my_pid0));
    new_lb_wn = AWN_Sub(type, new_lb_wn, LWN_Make_Icon(type, 
      lego_info->Offset()));

    WN *curr_cycle_copy = LWN_Copy_Tree(curr_cycle_wn); 
    LWN_Copy_Def_Use(curr_cycle_wn, curr_cycle_copy, Du_Mgr);
    new_lb_wn = AWN_Add(type, new_lb_wn, curr_cycle_copy);

    if (lego_info->Stride() != 1) {
      new_lb_wn = LWN_CreateDivceil(type, new_lb_wn, 
				    LWN_Make_Icon(type, lego_info->Stride()));
    }
    
    WN *curr_lb_copy = LWN_Copy_Tree(curr_lb_wn); 
    LWN_Copy_Def_Use(curr_lb_wn, curr_lb_copy, Du_Mgr);
    new_lb_wn = AWN_Max(type, curr_lb_copy, new_lb_wn); 

    sprintf(Str_Buf, "$dsm_blkcyc_inner_lb%d", WN_map_id(doloop));
    *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
    new_lb_wn = AWN_StidIntoSym(*new_lb, new_lb_wn);
    WN_Set_Linenum(new_lb_wn, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, new_lb_wn);

    //
    // Upper bound 
    // 
    WN *new_ub_wn = AWN_Add(type, AWN_LdidSym(my_pid0), LWN_Make_Icon(type, 1));
    new_ub_wn = AWN_Mpy(type, new_ub_wn, dinfo->Chunksize(curr_dim));
    new_ub_wn = AWN_Sub(type, new_ub_wn, 
      LWN_Make_Icon(type, lego_info->Offset() + 1));

    curr_cycle_copy = LWN_Copy_Tree(curr_cycle_wn); 
    LWN_Copy_Def_Use(curr_cycle_wn, curr_cycle_copy, Du_Mgr);
    new_ub_wn = AWN_Add(type, new_ub_wn, curr_cycle_copy);

    if (lego_info->Stride() != 1) {
      new_ub_wn = LWN_CreateDivfloor(type, new_ub_wn, 
				     LWN_Make_Icon(type, lego_info->Stride()));
    }
    
    WN *curr_ub_copy = LWN_Copy_Tree(curr_ub_wn); 
    LWN_Copy_Def_Use(curr_ub_wn, curr_ub_copy, Du_Mgr);
    new_ub_wn = AWN_Min(type, curr_ub_copy, new_ub_wn); 

    sprintf(Str_Buf, "$dsm_blkcyc_inner_ub%d", WN_map_id(doloop));
    *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);
    new_ub_wn = AWN_StidIntoSym(*new_ub, new_ub_wn);
    WN_Set_Linenum(new_ub_wn, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, new_ub_wn);

    LWN_Delete_Tree(curr_cycle_wn);
  }

  return bounds_code; 
} /* Generate_Blkcyc_Bounds */


/*
 * Generate call to the following runtime library routine to get the bounds:
 * 
 *    void  __dsm_Cyclic_Bounds(DISTR_ARRAY_RT *dart, INT64 curr_dim, 
 * 			         INT64 my_pid,
 *                               INT64 stride, INT64 offset,
 *                               INT64 lb, INT64 ub, INT64 step, 
 *                               INT64 *new_lb, INT64 *new_ub, INT64 *new_step) 
 *
 */
WN* Generate_Runtime_Cyclic_Bounds(DISTR_INFO *dinfo,
			    LEGO_INFO *lego_info,
			    const WN *doloop, 
			    SYMBOL **new_lb, 
			    SYMBOL **new_ub,
			    SYMBOL **new_step)
{
  INT64 linenum = WN_Get_Linenum(doloop);
  WN *bounds_code = WN_CreateBlock();
  WN_Set_Linenum(bounds_code, linenum);
  TYPE_ID ty_out = Promote_Type(Do_Wtype((WN*) doloop)); 
 
  WN *curr_lb_wn = WN_kid0(WN_start(doloop));
  WN *curr_ub_wn = UBexp(WN_end(doloop));

  INT64 step = Step_Size((WN *)doloop);
  Is_True(step == 1, ("Generate_Runtime_Cyclic_Bounds: Got step != 1"));
  FmtAssert(step != 0, ("Got a non-constant or zero step")); 

  INT curr_dim = lego_info->Dim_Num();

  SYMBOL *my_pid = lego_info->Pid_Sym0();
  Is_True(my_pid, ("Generate_Runtime_Cyclic_Bounds: Bad pid\n"));

  // Create the call
  OPCODE call_op = OPCODE_make_op(OPR_CALL, MTYPE_V, MTYPE_V);
  WN *call_wn = WN_Create(call_op, 12);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Cyclic_Bounds]);
  Set_Runtime_Call_Side_Effects (call_wn);
  WN_Set_Linenum(call_wn, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, call_wn);
  Array_Dependence_Graph->Add_Vertex(call_wn); 

  //
  // Set the parameters of the call
  //

  // Kid 0: DISTR_ARRAY_RT *dart
  WN *kid = dinfo->DART_Ldid();
  WN *parm_wn = WN_CreateParm(Pointer_type, kid, Be_Type_Tbl(Pointer_type), 
    WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 0) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 1: INT64 curr_dim
  kid = LWN_Make_Icon(MTYPE_I8, curr_dim);
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 1) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 2: INT64 my_pid
  kid = AWN_LdidSym(my_pid);
  if (WN_rtype(kid) != MTYPE_I8)  
    kid = LWN_Integer_Cast(kid, MTYPE_I8, WN_rtype(kid));  
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 2) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 3: INT64 stride
  kid = LWN_Make_Icon(MTYPE_I8, lego_info->Stride());
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 3) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 4: INT64 offset
  kid = LWN_Make_Icon(MTYPE_I8, lego_info->Offset());
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 4) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 5: INT64 lb
  kid = LWN_Copy_Tree(curr_lb_wn);
  LWN_Copy_Def_Use(curr_lb_wn, kid, Du_Mgr);
  if (WN_rtype(curr_lb_wn) != MTYPE_I8)
    kid = LWN_Integer_Cast(kid, MTYPE_I8, WN_rtype(curr_lb_wn)); 
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 5) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 6: INT64 ub
  kid = LWN_Copy_Tree(curr_ub_wn);
  LWN_Copy_Def_Use(curr_ub_wn, kid, Du_Mgr);
  if (WN_rtype(curr_ub_wn) != MTYPE_I8)
    kid = LWN_Integer_Cast(kid, MTYPE_I8, WN_rtype(curr_ub_wn)); 
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 6) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 7: INT64 step
  kid = LWN_Make_Icon(MTYPE_I8, step);
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 7) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 8: INT64 *new_lb
  sprintf(Str_Buf, "$dsm_temp_lb%d", WN_map_id(doloop));
  SYMBOL *temp_lb = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I8)),
    LEGO_pool);
  Add_Pragma_To_MP_Region(doloop, temp_lb->St(), temp_lb->WN_Offset(),
    WN_PRAGMA_LOCAL);

  OPCODE lda_op = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  TY_IDX i8_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
  kid = WN_CreateLda(lda_op,temp_lb->WN_Offset(),i8_ptr_ty, temp_lb->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_lb->St());
#else
  Set_ST_addr_taken_passed(temp_lb->St());
#endif
  parm_wn = WN_CreateParm(Pointer_type, kid, i8_ptr_ty, WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 8) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  sprintf(Str_Buf, "$dsm_rt_lb%d", WN_map_id(doloop));
  *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, ty_out)), LEGO_pool);
  WN *new_lb_wn = AWN_LdidSym(temp_lb);
  Du_Mgr->Add_Def_Use(parm_wn, new_lb_wn);
  Create_local_alias(Alias_Mgr, parm_wn); 
  Copy_alias_info(Alias_Mgr, parm_wn, new_lb_wn); 
  lego_info->Set_Runtime_Lb_Sym(*new_lb);
  if (ty_out != MTYPE_I8) 
    new_lb_wn = LWN_Integer_Cast(new_lb_wn, ty_out, MTYPE_I8); 
  new_lb_wn = AWN_StidIntoSym(*new_lb, new_lb_wn);
  WN_Set_Linenum(new_lb_wn, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, new_lb_wn);

  // Kid 9: INT64 *new_ub
  sprintf(Str_Buf, "$dsm_temp_ub%d", WN_map_id(doloop));
  SYMBOL *temp_ub = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I8)), 
    LEGO_pool);
  Add_Pragma_To_MP_Region(doloop, temp_ub->St(), temp_ub->WN_Offset(),
    WN_PRAGMA_LOCAL);

  kid = WN_CreateLda(lda_op, temp_ub->WN_Offset(),i8_ptr_ty,temp_ub->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_ub->St());
#else
  Set_ST_addr_taken_passed(temp_ub->St());
#endif
  parm_wn = WN_CreateParm(Pointer_type, kid, i8_ptr_ty, WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 9) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  sprintf(Str_Buf, "$dsm_rt_ub%d", WN_map_id(doloop));
  *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, ty_out)), LEGO_pool);
  WN *new_ub_wn = AWN_LdidSym(temp_ub);
  Du_Mgr->Add_Def_Use(parm_wn, new_ub_wn);
  Create_local_alias(Alias_Mgr, parm_wn); 
  Copy_alias_info(Alias_Mgr, parm_wn, new_ub_wn);
  lego_info->Set_Runtime_Ub_Sym(*new_ub);
  if (ty_out != MTYPE_I8) 
    new_ub_wn = LWN_Integer_Cast(new_ub_wn, ty_out, MTYPE_I8); 
  new_ub_wn = AWN_StidIntoSym(*new_ub, new_ub_wn);
  WN_Set_Linenum(new_ub_wn, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, new_ub_wn);

  // Kid 10: INT64 *new_step
  sprintf(Str_Buf, "$dsm_temp_step%d", WN_map_id(doloop));
  SYMBOL *temp_step = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I8)),
    LEGO_pool);
  Add_Pragma_To_MP_Region(doloop, temp_step->St(), temp_step->WN_Offset(),
    WN_PRAGMA_LOCAL);

  kid = WN_CreateLda(lda_op, temp_step->WN_Offset(), i8_ptr_ty, 
    temp_step->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_step->St());
#else
  Set_ST_addr_taken_passed(temp_step->St());
#endif
  parm_wn = WN_CreateParm(Pointer_type, kid, i8_ptr_ty, WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 10) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  sprintf(Str_Buf, "$dsm_rt_step%d", WN_map_id(doloop));
  *new_step = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, ty_out)), 
    LEGO_pool);
  WN *new_step_wn = AWN_LdidSym(temp_step);
  Du_Mgr->Add_Def_Use(parm_wn, new_step_wn);
  Create_local_alias(Alias_Mgr, parm_wn); 
  Copy_alias_info(Alias_Mgr, parm_wn, new_step_wn);
  lego_info->Set_Runtime_Step_Sym(*new_step);
  if (ty_out != MTYPE_I8) 
    new_step_wn = LWN_Integer_Cast(new_step_wn, ty_out, MTYPE_I8); 
  new_step_wn = AWN_StidIntoSym(*new_step, new_step_wn);
  WN_Set_Linenum(new_step_wn, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, new_step_wn);

  // Kid 11: INT32 *lastlocal
  sprintf(Str_Buf, "$dsm_temp_lastlocal%d", WN_map_id(doloop));
  SYMBOL *temp_last = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf,MTYPE_I4)),
    LEGO_pool);
  Add_Pragma_To_MP_Region(doloop, temp_last->St(), temp_last->WN_Offset(),
    WN_PRAGMA_LOCAL);
  TY_IDX i4_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4));
  kid = WN_CreateLda(lda_op, temp_last->WN_Offset(), i4_ptr_ty, 
    temp_last->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_last->St());
#else
  Set_ST_addr_taken_passed(temp_last->St());
#endif
  parm_wn = WN_CreateParm(Pointer_type, kid, i4_ptr_ty, WN_PARM_BY_REFERENCE);
  Create_local_alias(Alias_Mgr, parm_wn); 
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 11) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(doloop);
  if (dli->Mp_Info != NULL) {
    WN* wn_region = NULL;
    for (wn_region = (WN*) doloop; wn_region != NULL;
      wn_region = LWN_Get_Parent(wn_region))
      if (WN_opcode(wn_region) == OPC_REGION)
	break;
    FmtAssert(wn_region != NULL, ("Could not find region."));
    WN* wn_pragma = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                    ST_IDX_ZERO,
                                    (INT32) WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED,
                                    0);
    LWN_Insert_Block_Before(WN_region_pragmas(wn_region), NULL, wn_pragma); 
    Add_Pragma_To_MP_Region(doloop, temp_last->St(), temp_last->WN_Offset(),
      WN_PRAGMA_LASTTHREAD);
  }

  return bounds_code;
} /* Generate_Runtime_Cyclic_Bounds */

static WN* Generate_Dynamic_Bounds(LEGO_INFO *lego_info,
			           const WN *doloop, 
			           SYMBOL **new_lb, 
			           SYMBOL **new_ub,
			           SYMBOL **new_step,
			           INT bound)
{
  INT64 linenum = WN_Get_Linenum(doloop);
  WN *bounds_code = WN_CreateBlock();
  WN_Set_Linenum(bounds_code, linenum);
 
  WN *curr_lb_wn = WN_kid0(WN_start(doloop));
  WN *curr_ub_wn = UBexp(WN_end(doloop));
  WN *curr_step_wn = Loop_Step((WN*) doloop);

  INT curr_dim = lego_info->Dim_Num();

  TYPE_ID ty_out = Promote_Type(Do_Wtype((WN*) doloop)); 

  // Create the call
  OPCODE call_op = OPCODE_make_op(OPR_CALL, MTYPE_V, MTYPE_V);
  WN *call_wn = WN_Create(call_op, 14);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Dynamic_Affinity_Bounds]);
  WN_Set_Call_Parm_Mod(call_wn); 
  WN_Set_Call_Parm_Ref(call_wn); 
  WN_Set_Linenum(call_wn, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, call_wn);
  Array_Dependence_Graph->Add_Vertex(call_wn); 

  //
  // Set the parameters of the call
  //

  // Kid 0: Array  
  WN* kid = Load_Distr_Array(lego_info->Array()->St()); 
  WN* parm_wn = WN_CreateParm(Pointer_type, kid,
    Be_Type_Tbl(Pointer_type), WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 0) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 1: INT64 curr_dim
  kid = LWN_Make_Icon(MTYPE_I8, curr_dim);
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 1) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 2: INT64 outer loop index
  SYMBOL* outer_index = lego_info->Pid_Sym0(); 
  kid = AWN_LdidSym(outer_index);
  if (WN_rtype(kid) != MTYPE_I8) 
    kid = LWN_Integer_Cast(kid, MTYPE_I8, outer_index->Type); 
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 2) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 3: INT64 inner loop index    
  kid = bound == 1 ? AWN_LdidSym(lego_info->Pid_Sym1()) 
    : LWN_Make_Icon(MTYPE_I8, 0);
  TYPE_ID my_type = bound == 1 ? lego_info->Pid_Sym1()->Type : MTYPE_I8; 
  if (WN_rtype(kid) != MTYPE_I8)
    kid = LWN_Integer_Cast(kid, MTYPE_I8, WN_rtype(kid)); 
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 3) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 4: Loop number 
  kid = LWN_Make_Icon(MTYPE_I8, bound + 1);
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8),
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 4) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 5: Stride  
  kid = LWN_Make_Icon(MTYPE_I8, lego_info->Stride());
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 5) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 6: Offset 
  kid = LWN_Make_Icon(MTYPE_I8, lego_info->Offset());
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 6) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 7: INT64 lb
  kid = LWN_Copy_Tree(curr_lb_wn);
  LWN_Copy_Def_Use(curr_lb_wn, kid, Du_Mgr);
  if (WN_rtype(kid) != MTYPE_I8)
    kid = LWN_Integer_Cast(kid, MTYPE_I8, WN_rtype(kid)); 
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 7) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 8: INT64 ub
  kid = LWN_Copy_Tree(curr_ub_wn);
  LWN_Copy_Def_Use(curr_ub_wn, kid, Du_Mgr);
  if (WN_rtype(kid) != MTYPE_I8)
    kid = LWN_Integer_Cast(kid, MTYPE_I8, WN_rtype(kid)); 
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 8) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 9: INT64 step
  kid = LWN_Copy_Tree(curr_step_wn);
  LWN_Copy_Def_Use(curr_step_wn, kid, Du_Mgr);
  if (WN_rtype(kid) != MTYPE_I8)
    kid = LWN_Integer_Cast(kid, MTYPE_I8, WN_rtype(kid)); 
  parm_wn = WN_CreateParm(MTYPE_I8, kid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 9) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  // Kid 10: INT64 *new_lb
  sprintf(Str_Buf, "$dsm_temp_lb%d", WN_map_id(doloop));
  SYMBOL *temp_lb = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I8)),
    LEGO_pool);
  Add_Pragma_To_MP_Region(doloop, temp_lb->St(), temp_lb->WN_Offset(),
    WN_PRAGMA_LOCAL);

  OPCODE lda_op = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  TY_IDX i8_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
  kid = WN_CreateLda(lda_op,temp_lb->WN_Offset(),i8_ptr_ty, temp_lb->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_lb->St());
#else
  Set_ST_addr_taken_passed(temp_lb->St());
#endif
  parm_wn = WN_CreateParm(Pointer_type, kid, i8_ptr_ty, WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 10) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  sprintf(Str_Buf, "$dsm_rt_lb%d", WN_map_id(doloop));
  *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, ty_out)), LEGO_pool);
  WN *new_lb_wn = AWN_LdidSym(temp_lb);
  Du_Mgr->Add_Def_Use(parm_wn, new_lb_wn);
  Create_local_alias(Alias_Mgr, parm_wn); 
  Copy_alias_info(Alias_Mgr, parm_wn, new_lb_wn);
  lego_info->Set_Runtime_Lb_Sym(*new_lb);
  if (ty_out != MTYPE_I8)
    new_lb_wn = LWN_Integer_Cast(new_lb_wn, ty_out, MTYPE_I8); 

  new_lb_wn = AWN_StidIntoSym(*new_lb, new_lb_wn);
  WN_Set_Linenum(new_lb_wn, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, new_lb_wn);

  // Kid 11: INT64 *new_ub
  sprintf(Str_Buf, "$dsm_temp_ub%d", WN_map_id(doloop));
  SYMBOL *temp_ub = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I8)), 
    LEGO_pool);
  Add_Pragma_To_MP_Region(doloop, temp_ub->St(), temp_ub->WN_Offset(),
    WN_PRAGMA_LOCAL);

  kid = WN_CreateLda(lda_op,temp_ub->WN_Offset(),i8_ptr_ty, temp_ub->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_ub->St());
#else
  Set_ST_addr_taken_passed(temp_ub->St());
#endif
  parm_wn = WN_CreateParm(Pointer_type, kid, i8_ptr_ty, WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 11) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  sprintf(Str_Buf, "$dsm_rt_ub%d", WN_map_id(doloop));
  *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, ty_out)), LEGO_pool);
  WN *new_ub_wn = AWN_LdidSym(temp_ub);
  Du_Mgr->Add_Def_Use(parm_wn, new_ub_wn);
  Create_local_alias(Alias_Mgr, parm_wn); 
  Copy_alias_info(Alias_Mgr, parm_wn, new_ub_wn);
  lego_info->Set_Runtime_Ub_Sym(*new_ub);
  if (ty_out != MTYPE_I8)
    new_ub_wn = LWN_Integer_Cast(new_ub_wn, ty_out, MTYPE_I8); 

  new_ub_wn = AWN_StidIntoSym(*new_ub, new_ub_wn);
  WN_Set_Linenum(new_ub_wn, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, new_ub_wn);

  // Kid 12: INT64 *new_step
  sprintf(Str_Buf, "$dsm_temp_step%d", WN_map_id(doloop));
  SYMBOL *temp_step = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I8)),
    LEGO_pool);
  Add_Pragma_To_MP_Region(doloop, temp_step->St(), temp_step->WN_Offset(),
    WN_PRAGMA_LOCAL);

  kid = WN_CreateLda(lda_op, temp_step->WN_Offset(), i8_ptr_ty, 
    temp_step->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_step->St());
#else
  Set_ST_addr_taken_passed(temp_step->St());
#endif
  parm_wn = WN_CreateParm(Pointer_type, kid, i8_ptr_ty, WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(kid, parm_wn);
  WN_kid(call_wn, 12) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  sprintf(Str_Buf, "$dsm_rt_step%d", WN_map_id(doloop));
  *new_step = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, ty_out)), 
    LEGO_pool);
  WN *new_step_wn = AWN_LdidSym(temp_step);
  Du_Mgr->Add_Def_Use(parm_wn, new_step_wn);
  Create_local_alias(Alias_Mgr, parm_wn); 
  Copy_alias_info(Alias_Mgr, parm_wn, new_step_wn);
  lego_info->Set_Runtime_Step_Sym(*new_step);
  if (ty_out != MTYPE_I8)
    new_step_wn = LWN_Integer_Cast(new_step_wn, ty_out, MTYPE_I8); 

  new_step_wn = AWN_StidIntoSym(*new_step, new_step_wn);
  WN_Set_Linenum(new_step_wn, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, new_step_wn);

  // Kid 13: address of "am I last iteration" bit
  sprintf(Str_Buf, "$da_is_last%d", WN_map_id(doloop));
  SYMBOL* is_last = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I4)),
    &LNO_default_pool);
  TY_IDX i4_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4));
  OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  kid = WN_CreateLda(op_lda, is_last->WN_Offset(), i4_ptr_ty,
                     is_last->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(is_last->St());
#else
  Set_ST_addr_taken_passed(is_last->St());
#endif
  parm_wn = WN_CreateParm(Pointer_type, kid, i4_ptr_ty, 
    WN_PARM_BY_REFERENCE);
  Create_local_alias(Alias_Mgr, parm_wn); 
  WN_kid(call_wn, 13) = parm_wn;
  LWN_Set_Parent(kid, parm_wn);
  LWN_Set_Parent(parm_wn, call_wn);
  Add_Pragma_To_MP_Region(doloop, is_last->St(), is_last->WN_Offset(),
    WN_PRAGMA_LOCAL);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(doloop); 
  if (dli->Mp_Info != NULL) 
    Add_Pragma_To_MP_Region(doloop, is_last->St(), is_last->WN_Offset(),
      WN_PRAGMA_LASTTHREAD);

  return bounds_code;
} /* Generate_Dynamic_Bounds */

//-----------------------------------------------------------------------
// NAME: Generate_Simple_Bounds 
// FUNCTION: Generate bounds for the do_across loop 'wn_loop' given 
//   that it is scheduled with simple scheduling. Returns a WN* to the 
//   generated bounds code. 
//-----------------------------------------------------------------------

static WN* Generate_Simple_Bounds(const WN* wn_loop, 
				  SYMBOL** new_lb, 
                                  SYMBOL** new_ub, 
				  DU_MANAGER* du, 
				  ARRAY_DIRECTED_GRAPH16* dg)
{
  // Some generally needed items. 
  INT64 linenum = WN_Get_Linenum(wn_loop);
  WN *bounds_code = WN_CreateBlock();
  WN_Set_Linenum(bounds_code, linenum);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  WN* wn_region = NULL; 
  for (wn_region = (WN*) wn_loop; wn_region != NULL; 
    wn_region = LWN_Get_Parent(wn_region))
    if (WN_opcode(wn_region) == OPC_REGION)
      break;
  FmtAssert(wn_region != NULL, ("Could not find region."));
  TYPE_ID ty_out = Promote_Type(Do_Wtype((WN*) wn_loop)); 

  // Create the call. 
  OPCODE op_call = OPCODE_make_op(OPR_CALL, MTYPE_V, MTYPE_V);
  WN* wn_call = WN_Create(op_call, 8); 
  WN_st_idx(wn_call) = ST_st_idx(distr_st_entries[Simple_Bounds]);
  WN_Set_Call_Parm_Mod(wn_call); 
  WN_Set_Call_Parm_Ref(wn_call); 
  WN_Set_Linenum(wn_call, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, wn_call);
  dg->Add_Vertex(wn_call); 
  // Set the parameters of the call. 

  // Kid 0: original start value 
  WN* wn_orig_lb = WN_kid0(WN_start(wn_loop)); 
  WN* wn_cp_orig_lb = LWN_Copy_Tree(wn_orig_lb); 
  LWN_Copy_Def_Use(wn_orig_lb, wn_cp_orig_lb, du); 
  TYPE_ID type = WN_rtype(wn_cp_orig_lb); 
  if (type != MTYPE_I8)
    wn_cp_orig_lb = LWN_Integer_Cast(wn_cp_orig_lb, MTYPE_I8, type); 
  WN* wn_parm = WN_CreateParm(MTYPE_I8, wn_cp_orig_lb, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE); 
  LWN_Set_Parent(wn_cp_orig_lb, wn_parm); 
  WN_kid(wn_call, 0) = wn_parm; 
  LWN_Set_Parent(wn_parm, wn_call); 

  // Kid 1: original stop value
  WN* wn_orig_ub = UBexp(WN_end(wn_loop));  
  WN* wn_cp_orig_ub = LWN_Copy_Tree(wn_orig_ub);  
  LWN_Copy_Def_Use(wn_orig_ub, wn_cp_orig_ub, du);
  type = WN_rtype(wn_cp_orig_ub); 
  if (type != MTYPE_I8)
    wn_cp_orig_ub = LWN_Integer_Cast(wn_cp_orig_ub, MTYPE_I8, type); 
  wn_parm = WN_CreateParm(MTYPE_I8, wn_cp_orig_ub, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(wn_cp_orig_ub, wn_parm); 
  WN_kid(wn_call, 1) = wn_parm; 
  LWN_Set_Parent(wn_parm, wn_call);

  // Kid 2: original step value 
  WN* wn_orig_step = Loop_Step((WN*) wn_loop); 
  WN* wn_cp_orig_step = LWN_Copy_Tree(wn_orig_step);  
  LWN_Copy_Def_Use(wn_orig_step, wn_cp_orig_step, du); 
  type = WN_rtype(wn_cp_orig_step); 
  if (type != MTYPE_I8)
    wn_cp_orig_step = LWN_Integer_Cast(wn_cp_orig_step, MTYPE_I8, type); 
  wn_parm = WN_CreateParm(MTYPE_I8, wn_cp_orig_step, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(wn_cp_orig_step, wn_parm); 
  WN_kid(wn_call, 2) = wn_parm; 
  LWN_Set_Parent(wn_parm, wn_call);

  // Kid 3: number of threads 
  WN* wn_num_threads = NULL; 
  if (dli->Mp_Info->Nest_Total() == 1) { 
    wn_num_threads = dli->Mp_Info->Is_Pdo() 
      ? (Is_Versioned_Mp_Loop((WN*) wn_loop)
      ? Get_Runtime_Cur_Numthreads_Ldid() 
      : Get_Runtime_Cur_Numthreads_Func((WN*) wn_loop))
      : Get_Frozen_Numthreads_Ldid((WN*) wn_loop);
  } else {
    wn_num_threads = AWN_LdidSym(dli->Mp_Info->Nest_Layout()); 
    WN* wn_outer_loop = (WN*) wn_loop; 
    for (WN* wn = (WN*) wn_loop; wn != NULL; wn = LWN_Get_Parent(wn))
      if (WN_opcode(wn) == OPC_DO_LOOP)
	wn_outer_loop = wn; 
    WN* wn_stid = Find_Node(dli->Mp_Info->Nest_Layout(),  
      LWN_Get_Parent(wn_outer_loop)); 
    du->Add_Def_Use(wn_stid, wn_num_threads); 
  }
  type = WN_rtype(wn_num_threads);
  if (type != MTYPE_I8)
    wn_num_threads = LWN_Integer_Cast(wn_num_threads, MTYPE_I8, type);
  wn_parm = WN_CreateParm(MTYPE_I8, wn_num_threads, Be_Type_Tbl(MTYPE_I8),
    WN_PARM_BY_VALUE);
  LWN_Set_Parent(wn_num_threads, wn_parm);
  WN_kid(wn_call, 3) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);
   
  // Kid 4: processor id
  SYMBOL* my_pid = dli->Mp_Info->Pid_Sym0();  
  WN* wn_pid = AWN_LdidSym(my_pid); 
  if (WN_rtype(wn_pid) != MTYPE_I8)
    wn_pid = LWN_Integer_Cast(wn_pid, MTYPE_I8, WN_rtype(wn_pid)); 
  if (dli->Is_Backward && dli->Auto_Parallelized) {
    WN* wn_num_threads1=LWN_Copy_Tree(wn_num_threads);
    LWN_Copy_Def_Use(wn_num_threads,wn_num_threads1,du);
    wn_pid=LWN_CreateExp2(OPCODE_make_op(OPR_SUB, MTYPE_I8, MTYPE_V),
                       wn_num_threads1, wn_pid);
    wn_pid=LWN_CreateExp2(OPCODE_make_op(OPR_SUB, MTYPE_I8, MTYPE_V),
                       wn_pid, LWN_Make_Icon(MTYPE_I8,1));
    LWN_Parentize(wn_pid); 
  }
  wn_parm = WN_CreateParm(MTYPE_I8, wn_pid, Be_Type_Tbl(MTYPE_I8), 
    WN_PARM_BY_VALUE); 
  LWN_Set_Parent(wn_pid, wn_parm); 
  WN_kid(wn_call, 4) = wn_parm; 
  LWN_Set_Parent(wn_parm, wn_call);
  
  // Kid 5: address of local start value 
  sprintf(Str_Buf, "$da_temp_lb%d", WN_map_id(wn_loop)); 
  SYMBOL* temp_lb = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I8)), 
    &LNO_default_pool);
  OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  TY_IDX i8_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
  WN* wn_kid = WN_CreateLda(op_lda, temp_lb->WN_Offset(), i8_ptr_ty, 
    temp_lb->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_lb->St());
#else
  Set_ST_addr_taken_passed(temp_lb->St());
#endif
  wn_parm=WN_CreateParm(Pointer_type, wn_kid, i8_ptr_ty, WN_PARM_BY_REFERENCE);
  WN_kid(wn_call, 5) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);
  LWN_Set_Parent(wn_kid, wn_parm);
  sprintf(Str_Buf, "$da_lb%d", WN_map_id(wn_loop));
  *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, ty_out)), 
    &LNO_default_pool);
  WN *wn_new_lb = AWN_LdidSym(temp_lb);
  Du_Mgr->Add_Def_Use(wn_parm, wn_new_lb);
  Create_local_alias(Alias_Mgr, wn_parm); 
  Copy_alias_info(Alias_Mgr, wn_parm, wn_new_lb); 
  if (ty_out != MTYPE_I8)
    wn_new_lb = LWN_Integer_Cast(wn_new_lb, ty_out, MTYPE_I8); 
  wn_new_lb = AWN_StidIntoSym(*new_lb, wn_new_lb);
  WN_Set_Linenum(wn_new_lb, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, wn_new_lb);
  Add_Pragma_To_MP_Region(wn_loop, temp_lb->St(), temp_lb->WN_Offset(),
                          WN_PRAGMA_LOCAL); 

  // Kid 6: address of local stop value 
  sprintf(Str_Buf, "$da_temp_ub%d", WN_map_id(wn_loop)); 
  SYMBOL* temp_ub = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I8)), 
    &LNO_default_pool);
  wn_kid = WN_CreateLda(op_lda, temp_ub->WN_Offset(), i8_ptr_ty, 
    temp_ub->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(temp_ub->St());
#else
  Set_ST_addr_taken_passed(temp_ub->St());
#endif
  wn_parm=WN_CreateParm(Pointer_type, wn_kid, i8_ptr_ty, WN_PARM_BY_REFERENCE);
  WN_kid(wn_call, 6) = wn_parm;
  LWN_Set_Parent(wn_parm, wn_call);
  LWN_Set_Parent(wn_kid, wn_parm);
  sprintf(Str_Buf, "$da_ub%d", WN_map_id(wn_loop));
  *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, ty_out)), 
    &LNO_default_pool);
  WN *wn_new_ub = AWN_LdidSym(temp_ub);
  Du_Mgr->Add_Def_Use(wn_parm, wn_new_ub);
  Create_local_alias(Alias_Mgr, wn_parm); 
  Copy_alias_info(Alias_Mgr, wn_parm, wn_new_ub); 
  if (ty_out != MTYPE_I8)
    wn_new_ub = LWN_Integer_Cast(wn_new_ub, ty_out, MTYPE_I8); 
  wn_new_ub = AWN_StidIntoSym(*new_ub, wn_new_ub);
  WN_Set_Linenum(wn_new_ub, linenum);
  LWN_Insert_Block_Before(bounds_code, NULL, wn_new_ub);
  Add_Pragma_To_MP_Region(wn_loop, temp_ub->St(), temp_ub->WN_Offset(),
                          WN_PRAGMA_LOCAL); 
 
  // Kid 7: address of "am I last iteration" bit 
  sprintf(Str_Buf, "$da_is_last%d", WN_map_id(wn_loop));
  SYMBOL* is_last = CXX_NEW(SYMBOL(Create_Stack_Symbol(Str_Buf, MTYPE_I4)),
    &LNO_default_pool);
  TY_IDX i4_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4));
  wn_kid = WN_CreateLda(op_lda, is_last->WN_Offset(), i4_ptr_ty,
    is_last->St());
#ifdef _NEW_SYMTAB
  Clear_ST_addr_not_passed(is_last->St());
#else
  Set_ST_addr_taken_passed(is_last->St());
#endif
  wn_parm=WN_CreateParm(Pointer_type, wn_kid, i4_ptr_ty, WN_PARM_BY_REFERENCE);
  Create_local_alias(Alias_Mgr, wn_parm); 
  WN_kid(wn_call, 7) = wn_parm;
  LWN_Set_Parent(wn_kid, wn_parm);
  LWN_Set_Parent(wn_parm, wn_call);
  Add_Pragma_To_MP_Region(wn_loop, is_last->St(), is_last->WN_Offset(),
                          WN_PRAGMA_LOCAL); 

  // Replace the SCHEDTYPE_SIMPLE with SCHEDTYPE_PSEUDOLOWERED 
  if (dli->Mp_Info->Nest_Index() == 0) {
    WN* wn_first = WN_first(WN_region_pragmas(wn_region));
    WN* wn = NULL; 
    for (wn = wn_first; wn != NULL; wn = WN_next(wn)) {
      if (WN_opcode(wn) == OPC_PRAGMA) {
	if (WN_pragma(wn) == WN_PRAGMA_MPSCHEDTYPE
	    && WN_pragma_arg1(wn) == WN_PRAGMA_SCHEDTYPE_SIMPLE) {
	  WN_pragma_arg1(wn) = WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED; 
	  break;
	}
      }
    }
    if (wn == NULL) {
      WN* wn_pragma = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                      ST_IDX_ZERO,
                                      (INT32)WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED,
                                      0);
      LWN_Insert_Block_Before(WN_region_pragmas(wn_region), NULL, wn_pragma);
    }
  }
  Add_Pragma_To_MP_Region(wn_loop, is_last->St(), is_last->WN_Offset(),
			  WN_PRAGMA_LASTTHREAD);
  return bounds_code; 
}

//-----------------------------------------------------------------------
// NAME: Interleaved_Pragma_Chunksize
// FUNCTION: Returns a WN* to the chunksize with which the 'wn_loop' is 
//   to be interleave scheduled.  Asserts if 'wn_loop' is not an 
//   interleave scheduled do_across. 
//-----------------------------------------------------------------------

extern WN* Interleaved_Pragma_Chunksize(const WN* wn_loop, 
					DU_MANAGER* du)   
{  
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  MP_INFO* mp_info = dli->Mp_Info; 
  WN *wn;

  FmtAssert(mp_info != NULL, ("Expected a doacross loop")); 
  FmtAssert(mp_info->Sched_Type() == MP_SCHED_INTERLEAVE, 
    ("Expected INTERLEAVED sched type"));  
  for (wn = (WN*) wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_REGION) 
      break;
  WN* wn_region = wn;  
  FmtAssert(wn_region != NULL, ("Could not find doacross region"));  
  WN* wn_first = WN_first(WN_region_pragmas(wn_region)); 
  WN* wn_chunksize = NULL; 
  for (wn = wn_first; wn != NULL; wn = WN_next(wn)) {
    if (WN_opcode(wn) == OPC_XPRAGMA 
      && (WN_pragma(wn) == WN_PRAGMA_CHUNKSIZE)) {
      wn_chunksize = WN_kid0(wn); 
      break;
    } 
  }
  WN* wn_cp_chunksize = NULL; 
  if (wn_chunksize == NULL) {
    wn_cp_chunksize = LWN_Make_Icon(Do_Wtype((WN*) wn_loop), 1); 
  } else {
    wn_cp_chunksize = LWN_Copy_Tree(wn_chunksize); 
    LWN_Copy_Def_Use(wn_chunksize, wn_cp_chunksize, du); 
  }
  return wn_cp_chunksize; 
}

//-----------------------------------------------------------------------
// NAME: Generate_Interleaved_Bounds 
// FUNCTION: Generate bounds for the do_across loop 'wn_loop' given
//   that it is scheduled with interleaved scheduling.  Returns a WN* 
//   to the generated bounds code.  
//-----------------------------------------------------------------------

static WN* Generate_Interleaved_Bounds(const WN* wn_loop, 
                                       SYMBOL** new_lb, 
				       SYMBOL** new_ub,
                                       SYMBOL** new_step, 
				       DU_MANAGER* du, 
				       INT bound)
{ 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  INT64 linenum = WN_Get_Linenum(wn_loop);
  WN* wn_stride = Loop_Step((WN*) wn_loop); 
  TYPE_ID type = Promote_Type(Do_Wtype((WN *) wn_loop));  
  WN* bounds_code = WN_CreateBlock();
  WN *wn;

  for (wn = (WN*) wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_REGION)
      break; 
  WN* wn_region = wn;  
  FmtAssert(wn_region != NULL, ("Could not find doacross region"));
  if (bound == 0) {

    // Calculate Lower Bound. 
    WN* wn_cp_stride = LWN_Copy_Tree(wn_stride); 
    LWN_Copy_Def_Use(wn_stride, wn_cp_stride, du); 
    WN* wn_cp_chunksize = Interleaved_Pragma_Chunksize(wn_loop, du); 
    WN* wn_stride_chunk = AWN_Mpy(type, wn_cp_stride, wn_cp_chunksize);  
    WN* wn_stride_chunk2 = LWN_Copy_Tree(wn_stride_chunk); 
    LWN_Copy_Def_Use(wn_stride_chunk, wn_stride_chunk2, du); 
    WN* wn_lb = LWN_Copy_Tree(WN_kid0(WN_start(wn_loop))); 
    LWN_Copy_Def_Use(WN_kid0(WN_start(wn_loop)), wn_lb, du); 
    WN* wn_pid0 = AWN_LdidSym(dli->Mp_Info->Pid_Sym0()); 
    WN* wn_base_increment = AWN_Mpy(type, wn_pid0, wn_stride_chunk); 
    WN* wn_base = AWN_Add(type, wn_lb, wn_base_increment); 
    sprintf(Str_Buf, "$da_lb%d",  WN_map_id(wn_loop));   
    *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), 
      &LNO_default_pool);
    WN* wn_new_lb = AWN_StidIntoSym(*new_lb, wn_base);
    WN_Set_Linenum(wn_new_lb, linenum); 
    LWN_Insert_Block_After(bounds_code, NULL, wn_new_lb); 
    Add_Pragma_To_MP_Region(wn_loop, (*new_lb)->St(), (*new_lb)->WN_Offset(),
                            WN_PRAGMA_LOCAL); 

    // Calculate Upper Bound. 
    WN* wn_ub = LWN_Copy_Tree(UBexp(WN_end(wn_loop))); 
    LWN_Copy_Def_Use(UBexp(WN_end(wn_loop)), wn_ub, du); 
    sprintf(Str_Buf, "$da_ub%d",  WN_map_id(wn_loop));   
    *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), 
      &LNO_default_pool);
    WN* wn_new_ub = AWN_StidIntoSym(*new_ub, wn_ub);
    WN_Set_Linenum(wn_new_ub, linenum); 
    LWN_Insert_Block_After(bounds_code, NULL, wn_new_ub); 
    Add_Pragma_To_MP_Region(wn_loop, (*new_ub)->St(), (*new_ub)->WN_Offset(),
                            WN_PRAGMA_LOCAL); 

    // Calculate Step.
    WN* wn_num_threads = NULL;
    if (dli->Mp_Info->Nest_Total() == 1) {
      wn_num_threads = dli->Mp_Info->Is_Pdo()
        ? (Is_Versioned_Mp_Loop((WN*) wn_loop)
        ? Get_Runtime_Cur_Numthreads_Ldid()
        : Get_Runtime_Cur_Numthreads_Func((WN*) wn_loop))
        : Get_Frozen_Numthreads_Ldid((WN*) wn_loop);
    } else { 
      wn_num_threads = AWN_LdidSym(dli->Mp_Info->Nest_Layout());
      WN* wn_outer_loop = (WN*) wn_loop;
      for (WN* wn = (WN*) wn_loop; wn != NULL; wn = LWN_Get_Parent(wn))
	if (WN_opcode(wn) == OPC_DO_LOOP)
	  wn_outer_loop = wn;
      WN* wn_stid = Find_Node(dli->Mp_Info->Nest_Layout(),
	LWN_Get_Parent(wn_outer_loop));
      du->Add_Def_Use(wn_stid, wn_num_threads);
    }
    WN* wn_step = AWN_Mpy(type, wn_num_threads, wn_stride_chunk2);
    sprintf(Str_Buf, "$da_step%d",  WN_map_id(wn_loop));   
    *new_step = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), 
      &LNO_default_pool);
    WN* wn_new_step = AWN_StidIntoSym(*new_step, wn_step);
    WN_Set_Linenum(wn_new_step, linenum); 
    LWN_Insert_Block_After(bounds_code, NULL, wn_new_step); 
    Add_Pragma_To_MP_Region(wn_loop, (*new_step)->St(),
                            (*new_step)->WN_Offset(), WN_PRAGMA_LOCAL); 
 
    // Calculate Last Iteration Flag
    wn_ub = LWN_Copy_Tree(UBexp(WN_end(wn_loop))); 
    LWN_Copy_Def_Use(UBexp(WN_end(wn_loop)), wn_ub, du);   
    wn_lb = LWN_Copy_Tree(WN_kid0(WN_start(wn_loop))); 
    LWN_Copy_Def_Use(WN_kid0(WN_start(wn_loop)), wn_lb, du); 
    WN* wn_diff = AWN_Sub(type, wn_ub, wn_lb); 
    wn_cp_stride = LWN_Copy_Tree(wn_stride);
    LWN_Copy_Def_Use(wn_stride, wn_cp_stride, du);
    wn_cp_chunksize = Interleaved_Pragma_Chunksize(wn_loop, du);
    wn_stride_chunk = AWN_Mpy(type, wn_cp_stride, wn_cp_chunksize);
    wn_stride_chunk2 = LWN_Copy_Tree(wn_stride_chunk);
    LWN_Copy_Def_Use(wn_stride_chunk, wn_stride_chunk2, du);
    if (dli->Mp_Info->Nest_Total() == 1) {
      wn_num_threads = dli->Mp_Info->Is_Pdo() 
        ? (Is_Versioned_Mp_Loop((WN*) wn_loop)
        ? Get_Runtime_Cur_Numthreads_Ldid() 
        : Get_Runtime_Cur_Numthreads_Func((WN*) wn_loop))
        : Get_Frozen_Numthreads_Ldid((WN*) wn_loop);
    } else { 
      wn_num_threads = AWN_LdidSym(dli->Mp_Info->Nest_Layout());
      WN* wn_outer_loop = (WN*) wn_loop;
      for (WN* wn = (WN*) wn_loop; wn != NULL; wn = LWN_Get_Parent(wn))
        if (WN_opcode(wn) == OPC_DO_LOOP)
          wn_outer_loop = wn;
      WN* wn_stid = Find_Node(dli->Mp_Info->Nest_Layout(),
        LWN_Get_Parent(wn_outer_loop));
      du->Add_Def_Use(wn_stid, wn_num_threads);
    }
    wn_step = AWN_Mpy(type, wn_num_threads, wn_stride_chunk2);
    WN* wn_num = AWN_Mod_Safe(type, wn_diff, wn_step); 
    WN* wn_last_proc = AWN_Div_Safe(type, wn_num, wn_stride_chunk); 
    wn_pid0 = AWN_LdidSym(dli->Mp_Info->Pid_Sym0()); 
    OPCODE eq_op =  
      OPCODE_make_op(OPR_EQ, Boolean_type, type);
    WN* wn_cond = LWN_CreateExp2(eq_op, wn_pid0, wn_last_proc);
    sprintf(Str_Buf, "$da_last_iter%d",  WN_map_id(wn_loop));   
    SYMBOL* sym_last_iter = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), 
      &LNO_default_pool);    
    WN* wn_last_iter = AWN_StidIntoSym(sym_last_iter, wn_cond);   
    WN_Set_Linenum(wn_last_iter, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, wn_last_iter);
    Add_Pragma_To_MP_Region(wn_loop, sym_last_iter->St(),
                            sym_last_iter->WN_Offset(), WN_PRAGMA_LOCAL); 
    if (dli->Mp_Info->Nest_Total() == 1) 
      du->Add_Def_Use(wn_last_iter, Return_Node(Current_Func_Node)); 

    // Replace the SCHEDTYPE_INTERLEAVE with SCHEDTYPE_PSEUDOLOWERED 
    WN* wn_first = WN_first(WN_region_pragmas(wn_region)); 
    for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) {
      if (WN_opcode(wn) == OPC_PRAGMA) {
	if (WN_pragma(wn) == WN_PRAGMA_MPSCHEDTYPE
	  && WN_pragma_arg1(wn) == WN_PRAGMA_SCHEDTYPE_INTERLEAVE) {
	  WN_pragma_arg1(wn) = WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED; 
	  break;
	}
      }
    }
    Add_Pragma_To_MP_Region(wn_loop,
                            sym_last_iter->St(), sym_last_iter->WN_Offset(),
                            WN_PRAGMA_LASTTHREAD);

  } else {

    // Calculate lower bound.  
    WN* wn_pid1 = AWN_LdidSym(dli->Mp_Info->Pid_Sym1());
    sprintf(Str_Buf, "$da_lb%d",  WN_map_id(wn_loop));   
    *new_lb = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), 
      &LNO_default_pool);
    WN* wn_new_lb = AWN_StidIntoSym(*new_lb, wn_pid1);
    WN_Set_Linenum(wn_new_lb, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, wn_new_lb);
    Add_Pragma_To_MP_Region(wn_loop, (*new_lb)->St(), (*new_lb)->WN_Offset(),
                            WN_PRAGMA_LOCAL); 

    // Calculate upper bound. 
    WN* wn_cp_stride = LWN_Copy_Tree(wn_stride);
    LWN_Copy_Def_Use(wn_stride, wn_cp_stride, du);
    WN* wn_cp_chunksize = Interleaved_Pragma_Chunksize(wn_loop, du);
    WN* wn_stride_chunk = AWN_Mpy(type, wn_cp_stride, wn_cp_chunksize);
    wn_cp_stride = LWN_Copy_Tree(wn_stride);
    LWN_Copy_Def_Use(wn_stride, wn_cp_stride, du);
    WN* wn_base_increment = AWN_Sub(type, wn_stride_chunk, wn_cp_stride);  
    wn_pid1 = AWN_LdidSym(dli->Mp_Info->Pid_Sym1());
    WN* wn_base = AWN_Add(type, wn_pid1, wn_base_increment); 
    WN* wn_ub = LWN_Copy_Tree(UBexp(WN_end(wn_loop)));
    LWN_Copy_Def_Use(UBexp(WN_end(wn_loop)), wn_ub, du);
    wn_ub = AWN_Min(type, wn_base, wn_ub); 
    sprintf(Str_Buf, "$da_ub%d",  WN_map_id(wn_loop));
    *new_ub = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), 
      &LNO_default_pool);
    WN* wn_new_ub = AWN_StidIntoSym(*new_ub, wn_ub);
    WN_Set_Linenum(wn_new_ub, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, wn_new_ub);
    Add_Pragma_To_MP_Region(wn_loop, (*new_ub)->St(), (*new_ub)->WN_Offset(),
                            WN_PRAGMA_LOCAL); 
 
    // Calculate step. 
    wn_cp_stride = LWN_Copy_Tree(wn_stride);
    sprintf(Str_Buf, "$da_step%d",  WN_map_id(wn_loop));
    *new_step = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), 
      &LNO_default_pool);
    WN* wn_new_step = AWN_StidIntoSym(*new_step, wn_cp_stride);
    WN_Set_Linenum(wn_new_step, linenum);
    LWN_Insert_Block_After(bounds_code, NULL, wn_new_step);
    Add_Pragma_To_MP_Region(wn_loop, (*new_step)->St(),
                            (*new_step)->WN_Offset(), WN_PRAGMA_LOCAL); 

    // Remove the chunksize pragma. 
    WN* wn_first = WN_first(WN_region_pragmas(wn_region));
    for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) {
      if (WN_opcode(wn) == OPC_XPRAGMA
	&& (WN_pragma(wn) == WN_PRAGMA_CHUNKSIZE)) {
	LWN_Extract_From_Block(LWN_Get_Parent(wn), wn); 
	LWN_Delete_Tree(wn);  
	break;
      }
    }
  }

  return bounds_code; 
}


/* 
 * If step is of the form:
 *     index = index + x*var 
 *
 * and x is constant
 *
 * return x, otherwise return 0.
 */
INT64 
Get_Step_Multiplier(WN *doloop, SYMBOL *var)
{
  if ((var == NULL) || (doloop == NULL) || (WN_opcode(doloop) != OPC_DO_LOOP)) return 0;

  WN *step_wn = WN_step(doloop);
  WN *index_wn = WN_index(doloop);

  if ((WN_st(step_wn) != WN_st(index_wn)) || 
      (WN_offset(step_wn) != WN_offset(index_wn))) {

    DevWarn("Get_Step_Multiplier: index %s/%d but assignment to %s/%d in step",
	    ST_name(WN_st(step_wn)), WN_offset(step_wn),
	    ST_name(WN_st(index_wn)), WN_offset(index_wn));
    return 0;
  }

  if (WN_operator(step_wn) != OPR_STID) {
    DevWarn("Get_Step_Multiplier: step expression not STID, got opcode=%d", 
	    WN_operator(step_wn));
    return 0;
  }

  WN *kid = WN_kid0(step_wn);
  if (WN_operator(kid) != OPR_ADD) return 0;

  WN *exprkid = WN_kid1(kid);

  // index = index + var
  // 
  if ((WN_operator(exprkid) == OPR_LDID) && 
      (Symbols_Equiv(exprkid, var))) {

    return 1;
  }

  if (WN_operator(exprkid) != OPR_MPY) return 0;

  // index = index + x*var
  // 
  WN *constkid = NULL;
  WN *varkid = NULL;
  if ((WN_operator(WN_kid0(exprkid)) == OPR_INTCONST) &&
      (WN_operator(WN_kid1(exprkid)) == OPR_LDID)) {

    constkid = WN_kid0(exprkid);
    varkid = WN_kid1(exprkid);
    if (!Symbols_Equiv(varkid, var)) return 0;

  } else if ((WN_operator(WN_kid1(exprkid)) == OPR_INTCONST) &&
	     (WN_operator(WN_kid0(exprkid)) == OPR_LDID)) {

    varkid = WN_kid0(exprkid);
    constkid = WN_kid1(exprkid);
    if (WN_st(varkid) != var->St()) return 0;
  } else {
    return 0;
  }

  return (WN_const_val(constkid));
} /* Get_Step_Multiplier */


/*
 * Return TRUE if the wn_ldid's symbol is equivalent to var.  Follows DU chains
 * through simple copies looking for matching symbols.
 */
BOOL
Symbols_Equiv(WN *wn_ldid,
	      SYMBOL *var)
{
  if (WN_operator(wn_ldid) != OPR_LDID) return FALSE;

  DEF_LIST *defs = Du_Mgr->Ud_Get_Def(wn_ldid); 
  if (defs && defs->Len() == 1) {
    WN *def_wn = defs->Head()->Wn();

    if ((WN_operator(def_wn) != OPR_STID) ||
        (SYMBOL(def_wn) != SYMBOL(wn_ldid)))  {

      return FALSE;
    } 
    WN* rhs_wn = WN_kid0(def_wn);
    if (WN_operator(rhs_wn) == OPR_CVTL ||
        WN_operator(rhs_wn) == OPR_CVT)
      rhs_wn = WN_kid0(rhs_wn);

    if (!OPCODE_is_load(WN_opcode(rhs_wn))) return FALSE;

    if (SYMBOL(rhs_wn) == *var) {
      return TRUE;
    }

    return (Symbols_Equiv(WN_kid0(def_wn), var));
  }

  return FALSE;
} /* Symbols_Equiv */


/* ========================================================================
   Class Implementations
   ======================================================================== */

LEGO_INFO::LEGO_INFO(SYMBOL *array) :
  _local_index_wn(LEGO_pool),
  _dynamic_affinity(FALSE)
{
  Init(array, 0, 1, 0, 0, 0);
}

LEGO_INFO::LEGO_INFO(SYMBOL *array, 
		     INT32 dim, 
		     INT32 stride, 
		     INT32 offset) :
  _local_index_wn(LEGO_pool),
  _dynamic_affinity(FALSE)
{
  Init(array, dim, stride, offset, 0, 0);
}

LEGO_INFO::LEGO_INFO(SYMBOL *array, 
		     INT32 dim, 
		     INT32 stride, 
		     INT32 offset,
		     INT32 front_peel, 
		     INT32 back_peel) :
  _local_index_wn(LEGO_pool),
  _dynamic_affinity(FALSE)
{
  Init(array, dim, stride, offset, front_peel, back_peel);
}

LEGO_INFO::~LEGO_INFO()
{
  CXX_DELETE(_array, LEGO_pool);
  FmtAssert(_pid_sym0 == NULL, ("_pid_sym0 should be NULL"));
  FmtAssert(_pid_sym1 == NULL, ("_pid_sym1 should be NULL"));
  FmtAssert(_local_index_sym == NULL, ("_local_index_sym should be NULL"));
  FmtAssert(_runtime_lb_sym == NULL, ("_runtime_lb_sym should be NULL"));
  FmtAssert(_runtime_ub_sym == NULL, ("_runtime_ub_sym should be NULL"));
  FmtAssert(_runtime_step_sym == NULL, ("_runtime_step_sym should be NULL"));
}

void LEGO_INFO::Init(SYMBOL *array, 
		     INT32 dim, 
		     INT32 stride, 
		     INT32 offset,
		     INT32 front_peel, 
		     INT32 back_peel) 
{
  _dynamic_affinity = FALSE;
  _array = array;
  _dim_num = dim;
  _stride = stride;
  _offset = offset;
  _front_peel = front_peel;
  _back_peel = back_peel;
  _min_offset = 0;
  _max_offset = 0;
  _too_messy = FALSE;
  _pid_sym0 = NULL;
  _pid_sym1 = NULL;
  _local_index_sym = NULL;
  _runtime_lb_sym = NULL;
  _runtime_ub_sym = NULL;
  _runtime_step_sym = NULL;
}

LEGO_INFO::LEGO_INFO(LEGO_INFO* lego_info, 
	             MEM_POOL *pool) :
  _local_index_wn(LEGO_pool),
  _dynamic_affinity(FALSE)
{
  _array = CXX_NEW(SYMBOL(lego_info->_array), pool); 
  _dim_num = lego_info->_dim_num; 
  _stride = lego_info->_stride; 
  _offset = lego_info->_offset;  
  _front_peel = lego_info->_front_peel;  
  _back_peel = lego_info->_back_peel;  
  _min_offset = lego_info->_min_offset;  
  _max_offset = lego_info->_max_offset;  
  _too_messy = lego_info->_too_messy;  
  _pid_sym0 = lego_info->_pid_sym0 != NULL ? 
    CXX_NEW(SYMBOL(lego_info->_pid_sym0), pool) : NULL; 
  _pid_sym1 = lego_info->_pid_sym1 != NULL ? 
    CXX_NEW(SYMBOL(lego_info->_pid_sym1), pool) : NULL; 
  _local_index_sym = NULL;  

  _runtime_lb_sym = lego_info->_runtime_lb_sym != NULL ? 
    CXX_NEW(SYMBOL(lego_info->_runtime_lb_sym), pool) : NULL; 
  _runtime_ub_sym = lego_info->_runtime_ub_sym != NULL ? 
    CXX_NEW(SYMBOL(lego_info->_runtime_ub_sym), pool) : NULL; 
  _runtime_step_sym = lego_info->_runtime_step_sym != NULL ? 
    CXX_NEW(SYMBOL(lego_info->_runtime_step_sym), pool) : NULL; 
}

WN* LEGO_INFO::Pid0(WN* curr_wn)
{
  Is_True (Pid_Sym0(), ("Pid0: pid_sym0 is NULL\n"));

  WN *ldid_wn = NULL;
  TYPE_ID type = Pid_Sym0()->Type;

  if (ST_class(Pid_Sym0()->St()) == CLASS_PREG) {
    ldid_wn = AWN_LdidSym (Pid_Sym0());
  } else {
    OPCODE op = OPCODE_make_op(OPR_LDID, type, type);
    ldid_wn = WN_CreateLdid(op, Pid_Sym0()->WN_Offset(), Pid_Sym0()->St(), 
			    Be_Type_Tbl(type));
  }

  // Update the DU chains.
  // Find the loop with Pid_Sym0 as it's index variable -- the start and
  // step are the defs
  WN *pid_loop = curr_wn;
  while (pid_loop) {
    if ((WN_opcode(pid_loop) == OPC_DO_LOOP) && 
	(SYMBOL(WN_index(pid_loop)) == *(Pid_Sym0()))) { 
      break;
    } else {
      pid_loop = LWN_Get_Parent(pid_loop);
    }
  }

  if (pid_loop != NULL) {
    Du_Mgr->Add_Def_Use(WN_start(pid_loop), ldid_wn);
    Du_Mgr->Add_Def_Use(WN_step(pid_loop), ldid_wn);
    Du_Mgr->Ud_Get_Def(ldid_wn)->Set_loop_stmt(pid_loop);
    Copy_alias_info(Alias_Mgr, WN_start(pid_loop), ldid_wn);
    return ldid_wn;
  }

  WN* wn_stid = NULL; 
  for (WN* wn = curr_wn; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP)
      continue;  
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    if (!dli->Is_Outer_Lego_Tile)
      continue;
    if (!dli->Is_Processor_Tile) 
      continue; 
    pid_loop = wn; 
    break;
  }
  FmtAssert(pid_loop != NULL, ("Could not find pid loop for pid0")); 
  WN* wn_first = WN_first(WN_do_body(pid_loop)); 
  for (WN* wnn = wn_first; wnn != NULL; wnn = WN_next(wnn)) {
    if (WN_operator(wnn) == OPR_STID   
      && SYMBOL(wnn) == *(Pid_Sym0())) {
      wn_stid = wnn; 
      break; 
    }
  } 
  FmtAssert(wn_stid != NULL, ("Could not find pid0")); 
  Du_Mgr->Add_Def_Use(wn_stid, ldid_wn);
  Du_Mgr->Ud_Get_Def(ldid_wn)->Set_loop_stmt(NULL);
  Copy_alias_info(Alias_Mgr, wn_stid, ldid_wn);
  return ldid_wn; 
}

WN* LEGO_INFO::Pid1(WN* curr_wn)
{
  Is_True (Pid_Sym1(), ("Pid: pid_sym1 is NULL\n"));

  WN *ldid_wn = NULL;
  TYPE_ID type = Pid_Sym1()->Type;

  if (ST_class(Pid_Sym1()->St()) == CLASS_PREG) {
    ldid_wn = AWN_LdidSym (Pid_Sym1());
  } else {
    OPCODE op = OPCODE_make_op(OPR_LDID, type, type);
    ldid_wn = WN_CreateLdid(op, Pid_Sym1()->WN_Offset(), Pid_Sym1()->St(), 
			    Be_Type_Tbl(type));
  }

  // Update the DU chains.
  // Find the loop with Pid_Sym1 as it's index variable -- the start and
  // step are the defs
  // 
  WN *pid_loop = curr_wn;
  while (pid_loop) {
    if ((WN_opcode(pid_loop) == OPC_DO_LOOP) && 
	(SYMBOL(WN_index(pid_loop)) == *(Pid_Sym1()))) { 
      break;
    } else {
      pid_loop = LWN_Get_Parent(pid_loop);
    }
  }

  if (pid_loop != NULL) {
    Du_Mgr->Add_Def_Use(WN_start(pid_loop), ldid_wn);
    Du_Mgr->Add_Def_Use(WN_step(pid_loop), ldid_wn);
    Du_Mgr->Ud_Get_Def(ldid_wn)->Set_loop_stmt(pid_loop);
    Copy_alias_info(Alias_Mgr, WN_start(pid_loop), ldid_wn);
    return ldid_wn;
  }

  WN* wn_stid = NULL; 
  for (WN* wn = curr_wn; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP)
      continue;  
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    if (!dli->Is_Outer_Lego_Tile)
      continue;
    if (!dli->Is_Processor_Tile) 
      continue; 
    pid_loop = wn; 
    break;
  }
  FmtAssert(pid_loop != NULL, ("Could not find pid loop for pid1")); 
  WN* wn_first = WN_first(WN_do_body(pid_loop)); 
  for (WN* wnn = wn_first; wnn != NULL; wnn = WN_next(wnn)) {
    if (WN_operator(wnn) == OPR_STID   
      && SYMBOL(wnn) == *(Pid_Sym1())) {
      wn_stid = wnn; 
      break; 
    }
  } 
  FmtAssert(wn_stid != NULL, ("Could not find pid1")); 
  Du_Mgr->Add_Def_Use(wn_stid, ldid_wn);
  Du_Mgr->Ud_Get_Def(ldid_wn)->Set_loop_stmt(NULL);
  Copy_alias_info(Alias_Mgr, wn_stid, ldid_wn);
  return ldid_wn; 
}


WN *LEGO_INFO::Local_Index()
{
  Is_True(Local_Index_Sym() && (_local_index_wn.Elements() > 0), 
	  ("Local_Index: local_index and/or local_index_wn are empty\n"));

  Is_True(ST_class(Local_Index_Sym()->St()) == CLASS_PREG, 
          ("Local_Index: local_index is not a PREG, got class %d\n",
	   ST_class(Local_Index_Sym()->St())));

  WN *ldid_wn = NULL;
  ldid_wn = AWN_LdidSym(Local_Index_Sym());
  WN *def_wn = NULL;

  for (INT i = 0; i < _local_index_wn.Elements(); i++) {
    def_wn = _local_index_wn[i];

    Is_True(OPCODE_is_store(WN_opcode(def_wn)),
	    ("Local_Index: def_wn is not a store, got opcode=%d\n", 
	     WN_operator(def_wn)));

    Du_Mgr->Add_Def_Use(def_wn, ldid_wn);
  }

  Copy_alias_info(Alias_Mgr, def_wn, ldid_wn);

  return ldid_wn;
}


void LEGO_INFO::Set_Pid0(SYMBOL *pid_sym)
{
  _pid_sym0 = pid_sym;
}

void LEGO_INFO::Set_Pid1(SYMBOL *pid_sym)
{
  _pid_sym1 = pid_sym;
}


void LEGO_INFO::Create_Local_Index(WN *doloop)
{
  if (Has_Local_Index()) return;

  // Get the Dact for this lego_info
  //
  SYMBOL *array_sym = Array();
  DISTR_ARRAY *dact = Lookup_DACT(array_sym->St());
  Is_True(dact, ("Create_Local_Index: No DACT for array %s in LEGO_INFO",
		 ST_name(array_sym->St())));

  // Create the preg for the local index
  //
  WN *curr_lb_wn = WN_kid0(WN_start(doloop));
  TYPE_ID type = WN_rtype(curr_lb_wn);

  sprintf(Str_Buf, "$local_index%d", WN_map_id(doloop));
  _local_index_sym = 
    CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, type)), LEGO_pool);

  // Initialize preg outside of loop:
  //
  WN *curr_lb_copy = LWN_Copy_Tree(curr_lb_wn);
  LWN_Copy_Def_Use(curr_lb_wn, curr_lb_copy, Du_Mgr);

  WN *init_wn = AWN_Mpy(type, curr_lb_copy, LWN_Make_Icon(type, Stride()));
  init_wn = AWN_Add(type, init_wn, LWN_Make_Icon(type, Offset()));

  // The initialization of the local index depends on the type of distribution:
  //
  // BLOCK:      local_index = (stride*lb + offset) % blocksize
  // CYCLIC(1):  local_index = (stride*lb + offset) / numprocs
  // CYCLIC(b):  local_index = (stride*lb + offset) % b
  //
  switch (dact->Dims()[Dim_Num()].Distr_Type()) {

    // BLOCK distribution
    //
    case DISTRIBUTE_BLOCK: {
      WN *dimsize = dact->Dinfo()->Dimsize(Dim_Num());
      init_wn = AWN_Rem_Safe(type, init_wn, dimsize);
      break;
    }

    // * Distribution
    //
    case DISTRIBUTE_STAR: {
      FmtAssert(FALSE, ("Create_Local_Index: Got DISTRIBUTE_STAR\n"));
      break;
    }

    case DISTRIBUTE_CYCLIC_CONST: {
      if (dact->Dims()[Dim_Num()].Chunk_Const_Val() == 1) {
	//
	// CYCLIC(1) Distribution
	//
	WN *numprocs = dact->Dinfo()->Numprocs(Dim_Num());
	init_wn = AWN_Div_Safe(type, init_wn, numprocs);

      } else {
	//
	// CYCLIC(k) Distribution, k is constant
	//
	WN *chunk_nd = LWN_Make_Icon(type, dact->Dims()[Dim_Num()].Chunk_Const_Val());
	init_wn = AWN_Rem_Safe(type, init_wn, chunk_nd);
      }
      break;
    }

    // CYCLIC(b) Distribution, b is a variable
    //
    case DISTRIBUTE_CYCLIC_EXPR: {
      WN *chunk_nd = dact->Chunksize(Dim_Num());
      init_wn = AWN_Rem_Safe(type, init_wn, chunk_nd);
      break;
    }
  }

  init_wn = AWN_StidIntoSym(_local_index_sym, init_wn);
  WN_Set_Linenum(init_wn, WN_Get_Linenum(doloop));

  LWN_Insert_Block_Before(LWN_Get_Parent(doloop), doloop, init_wn);
  _local_index_wn[_local_index_wn.Newidx()] = init_wn;

  // Increment preg at end of loop body: 
  // local_index += stride*step
  //
  WN *ldid_wn = AWN_LdidSym (_local_index_sym);
  Du_Mgr->Add_Def_Use(init_wn, ldid_wn);

  INT64 step = Get_Local_Step(doloop);

  FmtAssert(step != 0, ("Create_Local_Index: Bad step size in loop %s\n", 
			ST_name(WN_st(WN_index(doloop)))));

  WN *incr_wn = AWN_Add(type, ldid_wn, LWN_Make_Icon(type, step*Stride()));
  incr_wn = AWN_StidIntoSym(_local_index_sym, incr_wn);
  WN_Set_Linenum(incr_wn, WN_Get_Linenum(doloop));

  Copy_alias_info(Alias_Mgr, init_wn, incr_wn);
  Du_Mgr->Add_Def_Use (incr_wn, ldid_wn);
  

  LWN_Insert_Block_Before(WN_do_body(doloop), NULL, incr_wn);
  _local_index_wn[_local_index_wn.Newidx()] = incr_wn;

  if (LNO_enabled && !Get_Trace(TP_LNOPT2, TT_LEGO_DISABLE_HOIST)) {
    // let's hoist the init since it might contain MODs, 
    // forcing wopt to be conservative
    extern void Try_Hoist_Expression (WN* expr_wn);

    Try_Hoist_Expression (WN_kid0(init_wn));
  }
}


INT64 LEGO_INFO::Get_Local_Step(WN *doloop)
{
  // Get the Dact for this lego_info
  //
  SYMBOL *array_sym = Array();
  DISTR_ARRAY *dact = Lookup_DACT(array_sym->St());
  Is_True(dact, ("Get_Local_Step: No DACT for array %s in LEGO_INFO",
		 ST_name(array_sym->St())));

  INT64 step = 0;

  switch (dact->Dims()[Dim_Num()].Distr_Type()) {

    // BLOCK distribution
    //
    case DISTRIBUTE_BLOCK: {
      // for inline bounds generation
      step = Step_Size(doloop); 
      // for runtime bounds generation
      if (step == 0)  step = Get_Step_Multiplier(doloop, Runtime_Step_Sym());
      break;
    }

    // * Distribution
    //
    case DISTRIBUTE_STAR: {
      step = Step_Size(doloop);
      break;
    }

    case DISTRIBUTE_CYCLIC_CONST: {
      if (dact->Dims()[Dim_Num()].Chunk_Const_Val() == 1) {
	//
	// CYCLIC(1) Distribution
	//

	// for inline bounds generation
	step = Get_Step_Multiplier(doloop, dact->Dinfo()->Get_Numprocs(Dim_Num()));
	// for runtime bounds generation
	if (step == 0)  step = Get_Step_Multiplier(doloop, Runtime_Step_Sym());

      } else {
	//
	// CYCLIC(k) Distribution, k is constant
	//

	// for inline bounds generation
	step = Step_Size(doloop);
	// for runtime bounds generation
	if (step == 0)  step = Get_Step_Multiplier(doloop, Runtime_Step_Sym());
      }
      break;
    }

    // CYCLIC(b) Distribution, b is a variable
    //
    case DISTRIBUTE_CYCLIC_EXPR: {
      // for inline bounds generation
      step = Step_Size(doloop);
      // for runtime bounds generation
      if (step == 0)  step = Get_Step_Multiplier(doloop, Runtime_Step_Sym());
      break;
    }
  }

  return step;
}


void LEGO_INFO::Print(FILE* fp)  
{
  if (Is_Too_Messy()) {
    fprintf(fp, "Lego loop info too messy\n");
    return;
  }
  if (Array()) {
    fprintf(fp, "Lego loop info for %s: dim(%d) = %dx+%d\n", 
      ST_name(Array()->St()), Dim_Num(), Stride(), Offset());
    if (Pid_Sym0() != NULL) {
      const char* name = ST_class(Pid_Sym0()->St()) != CLASS_PREG 
	? ST_name(Pid_Sym0()->St()) 
	: Pid_Sym0()->WN_Offset() > Last_Dedicated_Preg_Offset
	? Preg_Name(Pid_Sym0()->WN_Offset()) : "DEDICATED PREG"; 
      fprintf(fp, " Pid_Sym0 = %s\n", name); 
    }
    if (Pid_Sym1() != NULL) {
      const char* name = ST_class(Pid_Sym1()->St()) != CLASS_PREG 
	? ST_name(Pid_Sym1()->St()) 
	: Pid_Sym1()->WN_Offset() > Last_Dedicated_Preg_Offset
	? Preg_Name(Pid_Sym1()->WN_Offset()) : "DEDICATED PREG"; 
      fprintf(fp, " Pid_Sym1 = %s\n", name); 
    }
    if (Dynamic_Affinity())
      fprintf(fp, "  Dynamic"); 
  }
  fprintf(fp, "\n  Front Peel = %d\n", Front_Peel());        
  fprintf(fp, "  Back Peel = %d\n", Back_Peel());        
  fprintf(fp, "  Min Offset = %d\n", Min_Offset());
  fprintf(fp, "  Max Offset = %d\n", Max_Offset());
  if (Has_Local_Index())
    fprintf(fp, "Has Local Index\n");  
}


RR_INFO::RR_INFO (INT ndims) {
  _ndims = ndims;
  _rrdim = CXX_NEW_ARRAY (RR_DIM, ndims, LEGO_pool);
  for (INT i=0; i<ndims; i++) _rrdim[i].Init (0, NULL, -1);
}

RR_INFO::RR_INFO (RR_INFO* rri) {
  _ndims = rri->_ndims;
  _rrdim = CXX_NEW_ARRAY (RR_DIM, _ndims, LEGO_pool);
  for (INT i=0; i<_ndims; i++) _rrdim[i].Init (rri->Dim(i));
}

RR_INFO::~RR_INFO () {
  CXX_DELETE_ARRAY (_rrdim, LEGO_pool);
}

extern RR_INFO* Get_RR_Map (WN* wn) {
  return (RR_INFO*) WN_MAP_Get (RR_Map, wn);
}

extern void Set_RR_Map (WN* wn, RR_INFO* rri) {
  WN_MAP_Set (RR_Map, wn, (void*) rri);
}

/***********************************************************************
 *
 * Given an inner lego-tile loop, find the processor tile loop.
 *
 ***********************************************************************/
static WN* Find_Proc_Tile_Loop (WN* do_wn) {
  Is_True (do_wn && WN_operator(do_wn) == OPR_DO_LOOP,
           ("Find_Proc_Tile_Loop must be called with a do-loop"));

  DO_LOOP_INFO* dli = Get_Do_Loop_Info (do_wn);
  LEGO_INFO* li = dli->Lego_Info;

  Is_True (dli->Is_Inner_Lego_Tile,
           ("Find_Proc_Tile_Loop must be called with lego inner-tile loop"));
  
  Is_True (dli->Lego_Mp_Key_Lower == dli->Lego_Mp_Key_Upper &&
           dli->Lego_Mp_Key_Depth > 0,
           ("Find_Proc_Tile_Loop: inconsistent lower/upper key values"));

  WN* proc_wn = LWN_Get_Parent (do_wn);
  while (proc_wn) {
    if (WN_operator(proc_wn) == OPR_DO_LOOP) {
      DO_LOOP_INFO* pdli = Get_Do_Loop_Info(proc_wn);
      if (pdli->Is_Processor_Tile &&
          pdli->Lego_Mp_Key_Depth == 0 &&
          dli->Lego_Mp_Key_Lower >= pdli->Lego_Mp_Key_Lower &&
          dli->Lego_Mp_Key_Lower <= pdli->Lego_Mp_Key_Upper) {
        // this is the loop
        return proc_wn;
      }
    }
    proc_wn = LWN_Get_Parent(proc_wn);
  }
  Is_True (FALSE, ("Find_Proc_Tile_Loop: could not find proc-tile loop"));
  return NULL;
}

/***********************************************************************
 *
 * Create RR_Maps on reshaped array references.
 * do_wn is the inner lego-tile loop.
 *
 ***********************************************************************/
static void RR_Map_Refs (WN* do_wn, WN* ref_wn) {
  if (!ref_wn) return;

  OPERATOR opr = WN_operator(ref_wn);
  if (opr == OPR_ARRAY) {
    WN* base_wn = WN_array_base(ref_wn);
    if (WN_operator(base_wn) != OPR_LDID &&
        WN_operator(base_wn) != OPR_LDA)
      goto recurse;

    DISTR_INFO* dinfo = da_hash->Find(WN_st(base_wn));
    if (!dinfo || !dinfo->IsReshaped()) 
      goto recurse;

    // is a reshaped array reference
    DISTR_ARRAY*    my_dact = dinfo->Get_Dact(0);
    ACCESS_ARRAY*   aa      = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, ref_wn);
    DO_LOOP_INFO*   dli     = Get_Do_Loop_Info (do_wn);
    LEGO_INFO*      li      = dli->Lego_Info;
    DISTR_ARRAY*    aff_dact= Lookup_DACT(li->Array()->St());
    INT             dim_num = li->Dim_Num();
    
    for (INT i=0; i<aa->Num_Vec(); i++) {
      ACCESS_VECTOR* av = aa->Dim(i);
      INT64 stride, offset;
      mINT32 depth;

      if (!Single_Loop_Coeff (av, &stride, &offset, &depth)) continue;
      if (dli->Depth != depth) continue;
      if (li->Stride() != stride) continue;

      // so this reference uses do_wn index in dimension "i"
      if (!my_dact->DACT_Equiv(aff_dact, i, dim_num)) continue;

      RR_INFO* rri = Get_RR_Map (ref_wn);
      if (!rri) {
        rri = CXX_NEW (RR_INFO(aa->Num_Vec()), LEGO_pool);
        Set_RR_Map (ref_wn, rri);
      }
      RR_DIM* rrdim = rri->Dim(i);
      Is_True (rrdim->Do_Loop() == NULL,
               ("RR_Map: map already has a proc-tile loop"));

      // find the processor tile loop
      WN* proc_wn = Find_Proc_Tile_Loop (do_wn);
      DO_LOOP_INFO* pdli = Get_Do_Loop_Info (proc_wn);
      
      rrdim->Init (offset-li->Offset(), proc_wn,
                   (dli->Lego_Mp_Key_Lower-pdli->Lego_Mp_Key_Lower));
    }
  }

  recurse:
  if (opr == OPR_BLOCK) {
    WN* kid = WN_first(ref_wn);
    while (kid) {
      RR_Map_Refs(do_wn, kid);
      kid = WN_next(kid);
    }
    return;
  }

  for (INT i=0; i<WN_kid_count(ref_wn); i++) {
    RR_Map_Refs(do_wn, WN_kid(ref_wn,i));
  }
}

/***********************************************************************
 *
 * Given a inner-lego tile do-loop, return the distribution that it
 * was tiled for.
 *
 ***********************************************************************/
DISTRIBUTE_TYPE Get_Lego_Affinity_Distr_Type (WN* do_wn) {

  Is_True (do_wn && WN_operator(do_wn) == OPR_DO_LOOP,
           ("Get_Lego_Affinity_Distr_Type not called with a do-loop"));

  DO_LOOP_INFO* dli = Get_Do_Loop_Info (do_wn);
  Is_True (dli->Is_Inner_Lego_Tile,
           ("Get_Lego_Affinity_Distr_Type not called with inner-lego-tile-loop"));
  LEGO_INFO* li = dli->Lego_Info;

  DISTR_ARRAY* dact = Lookup_DACT(li->Array()->St());
  DISTR_INFO* dinfo = dact->Dinfo();

  // handle only block distribution for now
  return (dact->Get_Dim(li->Dim_Num())->Distr_Type());
}

/***********************************************************************
 *
 * Given the inner lego-tile do loop after lego-tiling,
 * create reshape-ref-maps for conforming references within
 * the loop.
 *
 ***********************************************************************/
extern void RR_Map_Setup_InnerLoop (WN* do_wn) {

  // handle only block distribution for now
  if (Get_Lego_Affinity_Distr_Type (do_wn) != DISTRIBUTE_BLOCK)
    return;


  // Continue whether affinity array is reshaped or not.
  // We can optimize reshaped references if they conform to affinity
  // array.

  // get the body of the original loop
  WN* body_wn = WN_do_body(do_wn);
  RR_Map_Refs (do_wn, body_wn);
}


/***********************************************************************
 *
 * Find all inner-lego-tile loops and call RR_Map_Setup_InnerLoop
 * for each of those loops to setup reshaped-reference maps.
 * In the outer processor-tile loop, initialize all the lower-bound
 * symbols for subsequent lowering.
 *
 ***********************************************************************/
static void RR_Map_Setup_Traverse (WN* wn) {
  if (!wn) return;
  if (!OPCODE_is_scf(WN_opcode(wn))) return;

  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_DO_LOOP) {
    // is it an inner-lego tile loop?
    DO_LOOP_INFO* dli = Get_Do_Loop_Info (wn);
    LEGO_INFO* li = dli->Lego_Info;
    if (dli->Is_Inner_Lego_Tile && li && !li->Dynamic_Affinity()) {
      WN* proc_wn = Find_Proc_Tile_Loop (wn);

      // initialize symbols to LBs for proc-wn
      DO_LOOP_INFO* pdli = Get_Do_Loop_Info (proc_wn);
      if (pdli->Lego_LB_Symbols == NULL) {
        INT nloops = pdli->Lego_Mp_Key_Upper - pdli->Lego_Mp_Key_Lower + 1;
        pdli->Lego_LB_Symbols = CXX_NEW_ARRAY (SYMBOL, nloops, LEGO_pool);
      }
      
      // determine index of this do-loop
      Is_True (dli->Lego_Mp_Key_Lower == dli->Lego_Mp_Key_Upper,
               ("Unequal key values in inner lego-tile loop"));

      INT myidx = dli->Lego_Mp_Key_Lower - pdli->Lego_Mp_Key_Lower;
      SYMBOL* sym = pdli->Lego_LB_Symbols;
      char name[64];
      sprintf (name, "$store_lb_%d", myidx);
      sym[myidx] =
        Create_Preg_Symbol (name, TY_mtype(ST_type(WN_st(WN_index(wn)))));

      // copy the lower bound of the do-loop, assign it into this new sym
      WN* lb_wn = LWN_Copy_Tree(WN_kid0(WN_start(wn)), TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use (WN_kid0(WN_start(wn)), lb_wn, Du_Mgr);
      WN* stid_wn = AWN_StidIntoSym (&(sym[myidx]), lb_wn);
      Du_Mgr->Add_Def_Use (stid_wn, Return_Node (Current_Func_Node));
      LWN_Insert_Block_Before (NULL, wn, stid_wn);

      // now setup the maps
      RR_Map_Setup_InnerLoop (wn);
    }
  }
  
  if (opr == OPR_BLOCK) {
    WN* kid = WN_first(wn);
    while (kid) {
      RR_Map_Setup_Traverse(kid);
      kid = WN_next(kid);
    }
  }
  else {
    for (INT i=0; i<WN_kid_count(wn); i++)
      RR_Map_Setup_Traverse(WN_kid(wn, i));
  }
} /* RR_Map_Setup_Traverse () */

/***********************************************************************
 *
 * Given the func_wn, setup maps based on lego-affinity tiling.
 *
 ***********************************************************************/
extern void RR_Map_Setup (WN* func_wn) {
  if (disable_rr_maps) 
    return;

  RR_Map_Setup_Traverse(func_wn);
}

/***********************************************************************
 *
 * Given 
 *  do_wn   = the original inner-lego-tile do-loop
 *  old_wn  = reference in original do-loop
 *  new_wn  = corresponding reference in peeled portion
 *
 * Create a map for the new reference based on the old-reference and the
 * peeling.
 *
 ***********************************************************************/
static void Pre_Peel_Map_Refs (WN* do_wn, WN* old_wn, WN* new_wn) {
  if (!old_wn) return;

  Is_True (WN_operator(old_wn) ==
           WN_operator(new_wn),
           ("Pre_Peel: expected identical old/new nodes"));

  OPERATOR opr = WN_operator(old_wn);
  if (opr == OPR_ARRAY) {
    WN* base_wn = WN_array_base(old_wn);
    if (WN_operator(base_wn) != OPR_LDID &&
        WN_operator(base_wn) != OPR_LDA)
      goto recurse;

    DISTR_INFO* dinfo = da_hash->Find(WN_st(base_wn));
    if (!dinfo || !dinfo->IsReshaped()) 
      goto recurse;

    // is a reshaped array reference
    DISTR_ARRAY*    my_dact = dinfo->Get_Dact(0);
    ACCESS_ARRAY*   aa      = (ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,old_wn);
    DO_LOOP_INFO*   dli     = Get_Do_Loop_Info (do_wn);
    LEGO_INFO*      li      = dli->Lego_Info;
    DISTR_ARRAY*    aff_dact= Lookup_DACT(li->Array()->St());
    INT             dim_num = li->Dim_Num();
    
    for (INT i=0; i<aa->Num_Vec(); i++) {
      ACCESS_VECTOR* av = aa->Dim(i);
      INT64 stride, offset;
      mINT32 depth;

      if (!Single_Loop_Coeff (av, &stride, &offset, &depth)) continue;
      if (dli->Depth != depth) continue;
      if (li->Stride() != stride) continue;

      // so this reference uses do_wn index in dimension "i"
      if (!my_dact->DACT_Equiv(aff_dact, i, dim_num)) continue;

      RR_INFO* old_rri = Get_RR_Map (old_wn);
      Is_True (old_rri, ("Pre_Peel: missing RR-Map"));

      // there should already exist a map from the call to LWN_Copy_Tree.
      // delete it.
      RR_INFO* gomi_rri = Get_RR_Map(new_wn);
      FmtAssert (gomi_rri, ("Pre-Peel: expected a map"));
      CXX_DELETE (gomi_rri, LEGO_pool);

      // copy the map to the new reference
      RR_INFO* new_rri = CXX_NEW (RR_INFO(old_rri), LEGO_pool);
      Set_RR_Map (new_wn, new_rri);

      // update based on peeling
      RR_DIM* old_rrdim = old_rri->Dim(i);
      RR_DIM* new_rrdim = new_rri->Dim(i);
      INT dimoff = offset-li->Offset();
      if (dimoff == 0) {
        // was local, stays local
      } else if (dimoff < 0) {
        // remote in some (or all) pre-peels. be conservative for now
        new_rrdim->Remotize ();
      }
      else {
        // (dimoff > 0)
        // this is likely local, but we can't be sure.
        // it's a function of magnitude of offset, and peel-number
        new_rrdim->Remotize ();
      }
    }
  }

  recurse:
  if (opr == OPR_BLOCK) {
    WN* kid_old = WN_first(old_wn);
    WN* kid_new = WN_first(new_wn);
    while (kid_old) {
      Pre_Peel_Map_Refs(do_wn, kid_old, kid_new);
      kid_old = WN_next(kid_old);
      kid_new = WN_next(kid_new);
    }
    return;
  }

  for (INT i=0; i<WN_kid_count(old_wn); i++) {
    Pre_Peel_Map_Refs(do_wn, WN_kid(old_wn,i), WN_kid(new_wn, i));
  }
} /* Pre_Peel_Map_Refs() */

/***********************************************************************
 *
 * Called with 
 *  do_wn   == inner-lego tile loop
 *  prev_wn == the node before the do-loop *before* the peeling was done 
 *             (might be NULL)
 *  create_loop == TRUE if pre-peel created a loop, FALSE otherwise.
 *
 ***********************************************************************/
extern void Pre_Peel_RR_Map_Update(WN* do_wn, WN* prev_wn, BOOL create_loop)
{
  if (disable_rr_maps)
    return;

  if (Get_Lego_Affinity_Distr_Type (do_wn) != DISTRIBUTE_BLOCK)
    return;

  WN* peel_wn;
  if (prev_wn) peel_wn = WN_next(prev_wn);
  else peel_wn = WN_first(LWN_Get_Parent(do_wn));

  if (create_loop) {
    Is_True (WN_operator(peel_wn) == OPR_IF,
             ("Pre-peel: expected an IF"));
    WN* peel_body_wn = WN_first(WN_then(peel_wn));
    Is_True (WN_operator(peel_body_wn) == OPR_DO_LOOP,
             ("Pre-peel: expected a do-loop"));
    Pre_Peel_Map_Refs (do_wn, WN_do_body(do_wn), WN_do_body(peel_body_wn));
  }
  else {
    LEGO_INFO* li = Get_Do_Loop_Info(do_wn)->Lego_Info;
    Is_True (li->Front_Peel() > 0,
             ("Pre_Peel: front_peel is %d", li->Front_Peel()));
    for (INT i=0; i<li->Front_Peel(); i++) {
      WN* body_wn = WN_first(WN_do_body(do_wn));
      Is_True (WN_operator(peel_wn) == OPR_IF,
               ("Pre-peel: expected an IF"));
      WN* peel_body_wn = WN_first(WN_then(peel_wn));
      while (body_wn) {
        Pre_Peel_Map_Refs (do_wn, body_wn, peel_body_wn);
        peel_body_wn = WN_next(peel_body_wn);
        body_wn = WN_next(body_wn);
      }
      peel_wn = WN_next(peel_wn);
    }
    Is_True (peel_wn == do_wn,
             ("peel should have reached do loop by now"));
  }
} /* Pre_Peel_RR_Map_Update() */

/***********************************************************************
 *
 * Given 
 *  do_wn   = the original inner-lego-tile do-loop
 *  old_wn  = reference in original do-loop
 *  new_wn  = corresponding reference in peeled portion
 *
 * Create a map for the new reference based on the old-reference and the
 * peeling.
 *
 ***********************************************************************/
static void Post_Peel_Map_Refs (WN* do_wn, WN* old_wn, WN* new_wn) {
  if (!old_wn) return;

  Is_True (WN_operator(old_wn) ==
           WN_operator(new_wn),
           ("Post_Peel: expected identical old/new nodes"));

  OPERATOR opr = WN_operator(old_wn);
  if (opr == OPR_ARRAY) {
    WN* base_wn = WN_array_base(old_wn);
    if (WN_operator(base_wn) != OPR_LDID &&
        WN_operator(base_wn) != OPR_LDA)
      goto recurse;

    DISTR_INFO* dinfo = da_hash->Find(WN_st(base_wn));
    if (!dinfo || !dinfo->IsReshaped()) 
      goto recurse;

    // is a reshaped array reference
    DISTR_ARRAY*    my_dact = dinfo->Get_Dact(0);
    ACCESS_ARRAY*   aa      = (ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,old_wn);
    DO_LOOP_INFO*   dli     = Get_Do_Loop_Info (do_wn);
    LEGO_INFO*      li      = dli->Lego_Info;
    DISTR_ARRAY*    aff_dact= Lookup_DACT(li->Array()->St());
    INT             dim_num = li->Dim_Num();
    
    for (INT i=0; i<aa->Num_Vec(); i++) {
      ACCESS_VECTOR* av = aa->Dim(i);
      INT64 stride, offset;
      mINT32 depth;

      if (!Single_Loop_Coeff (av, &stride, &offset, &depth)) continue;
      if (dli->Depth != depth) continue;
      if (li->Stride() != stride) continue;

      // so this reference uses do_wn index in dimension "i"
      if (!my_dact->DACT_Equiv(aff_dact, i, dim_num)) continue;

      RR_INFO* old_rri = Get_RR_Map (old_wn);
      Is_True (old_rri, ("Post_Peel: missing RR-Map"));

      // there should already exist a map from the call to LWN_Copy_Tree.
      // delete it.
      RR_INFO* gomi_rri = Get_RR_Map(new_wn);
      FmtAssert (gomi_rri, ("Post-Peel: expected a map"));
      CXX_DELETE (gomi_rri, LEGO_pool);

      // copy the map to the new reference
      RR_INFO* new_rri = CXX_NEW (RR_INFO(old_rri), LEGO_pool);
      Set_RR_Map (new_wn, new_rri);

      // update based on peeling
      RR_DIM* old_rrdim = old_rri->Dim(i);
      RR_DIM* new_rrdim = new_rri->Dim(i);
      INT dimoff = offset-li->Offset();
      if (dimoff == 0) {
        // was local, stays local
      } else if (dimoff < 0) {
        // remote in some (or all) post-peels. be conservative for now
        new_rrdim->Remotize ();
      }
      else {
        // (dimoff > 0)
        // this is likely local, but we can't be sure.
        // it's a function of magnitude of offset, and peel-number
        new_rrdim->Remotize ();
      }
    }
  }

  recurse:
  if (opr == OPR_BLOCK) {
    WN* kid_old = WN_first(old_wn);
    WN* kid_new = WN_first(new_wn);
    while (kid_old) {
      Post_Peel_Map_Refs(do_wn, kid_old, kid_new);
      kid_old = WN_next(kid_old);
      kid_new = WN_next(kid_new);
    }
    return;
  }

  for (INT i=0; i<WN_kid_count(old_wn); i++) {
    Post_Peel_Map_Refs(do_wn, WN_kid(old_wn,i), WN_kid(new_wn, i));
  }
} /* Post_Peel_Map_Refs () */

/***********************************************************************
 *
 * Called with 
 *  do_wn   == inner-lego tile loop
 *  next_wn == the node after the do-loop *before* the peeling was done 
 *             (might be NULL)
 *  create_loop == TRUE if post-peel created a loop, FALSE otherwise.
 *
 ***********************************************************************/
extern void Post_Peel_RR_Map_Update(WN* do_wn, WN* next_wn, BOOL create_loop)
{

  if (disable_rr_maps)
    return;

  if (Get_Lego_Affinity_Distr_Type (do_wn) != DISTRIBUTE_BLOCK)
    return;
  
  WN* peel_wn;
  if (next_wn) peel_wn = WN_prev(next_wn);
  else peel_wn = WN_last(LWN_Get_Parent(do_wn));

  if (create_loop) {
    Is_True (WN_operator(peel_wn) == OPR_IF,
             ("Pre-peel: expected an IF"));
    WN* peel_body_wn = WN_first(WN_then(peel_wn));
    Is_True (WN_operator(peel_body_wn) == OPR_DO_LOOP,
             ("Pre-peel: expected a do-loop"));
    Post_Peel_Map_Refs (do_wn, WN_do_body(do_wn), WN_do_body(peel_body_wn));
  }
  else {
    LEGO_INFO* li = Get_Do_Loop_Info(do_wn)->Lego_Info;
    Is_True (li->Back_Peel() > 0,
             ("Post_Peel: back_peel is %d", li->Back_Peel()));
    for (INT i=0; i<li->Back_Peel(); i++) {
      WN* body_wn = WN_first(WN_do_body(do_wn));
      Is_True (WN_operator(peel_wn) == OPR_IF,
               ("Pre-peel: expected an IF"));
      WN* peel_body_wn = WN_first(WN_then(peel_wn));
      while (body_wn) {
        Post_Peel_Map_Refs (do_wn, body_wn, peel_body_wn);
        peel_body_wn = WN_next(peel_body_wn);
        body_wn = WN_next(body_wn);
      }
      peel_wn = WN_prev(peel_wn);
    }
    Is_True (peel_wn == do_wn,
             ("peel should have reached do loop by now"));
  }
} /* Post_Peel_RR_Map_Update() */
